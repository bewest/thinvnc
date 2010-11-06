//
// TTTTTTTTTTTT  HH                            VV          VV  NN     NN  CCCCCCCCCC
//      TT       HH           II                VV        VV   NNN    NN  CC
//      TT       HH                              VV      VV    NNNN   NN  CC
//      TT       HHHHHHHHHHH  II   NNNNNNNNN      VV    VV     NN NN  NN  CC
//      TT       HH       HH  II  NN       NN      VV  VV      NN  NN NN  CC
//      TT       HH       HH  II  NN       NN       VVVV       NN   NNNN  CC
//      TT       HH       HH  II  NN       NN        VV        NN    NNN  CCCCCCCCCC
//
// Copyright 2010 Cybele Software, Inc.
//
//
//
// This file is part of ThinVNC.
//
// ThinVNC is free software: you can redistribute it and/or modify
//     it under the terms of the GNU General Public License as published by
//     the Free Software Foundation, either version 3 of the License, or
//     (at your option) any later version.
//
//     ThinVNC is distributed in the hope that it will be useful,
//     but WITHOUT ANY WARRANTY; without even the implied warranty of
//     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//     GNU General Public License for more details.
//
//     You should have received a copy of the GNU General Public License
//     along with ThinVNC. If not, see <http://www.gnu.org/licenses/>
//
// For additional information, please refer to our Licensing FAQ or contact us via e-mail.
//
// See also:
// http://en.wikipedia.org/wiki/GPL
//

unit ThinVnc.Capture;
{$I ThinVnc.DelphiVersion.inc}
{$I ThinVnc.inc}
interface

uses Windows,Classes,SysUtils,SyncObjs,Messages,Simpletimer,
  ThinVnc.Log,ThinVnc.Utils,
  ThinVnc.MirrorWindow,ThinVnc.ClientSession;

type
  TCaptureThread = class(TThread)
  private
    FScraper : TAbstractScraper;
    FReset : Boolean;
    FSessions : TClientSessionList;
    FCs : TCriticalSection;
    FEventCapture : TEvent;
    FEndWaitEvent: TEvent;
    FOnSessionAdded: TNotifyEvent;
    FOnSessionRemoved: TNotifyEvent;
    FMethod:TThreadMethod;
    procedure LockList;
    procedure UnlockList;
    procedure Capture;
    procedure ForceCapture(Sender:TObject);
    function ProcessCapture:boolean;
    procedure NotifySessionAdded;
    procedure NotifySessionRemoved;
    procedure SetScraper(const Value: TAbstractScraper);
    procedure SyncTimer(Sender: TObject);
  protected
    procedure Synchronize(Method: TThreadMethod);overload;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Resume;reintroduce;
    procedure DisconnectAllUsers;
    procedure DisconnectUser(aUser: string);
    function  GetSession(Id: Integer): TClientSession;
    procedure AddSession(ASession:TClientSession);
    procedure RemoveSession(ASession:TClientSession;AFree:Boolean=false);
    procedure GetSessionUsers(aUsers:TStrings);
    function  SessionCount:Integer;
    procedure FreeSessions;
    procedure Execute;override;
    procedure Terminate; reintroduce; virtual;
    function  IsUserActive(AUser:string):Boolean;
    property  Scraper:TAbstractScraper read FScraper write SetScraper;
    property  OnSessionAdded : TNotifyEvent read FOnSessionAdded write FOnSessionAdded;
    property  OnSessionRemoved : TNotifyEvent read FOnSessionRemoved write FOnSessionRemoved;
  end;

function CaptureThread:TCaptureThread;

implementation

var
  gCaptureThread : TCaptureThread;

function CaptureThread:TCaptureThread;
begin
  if not Assigned(gCaptureThread) then
    gCaptureThread := TCaptureThread.Create;
  result:=gCaptureThread;
end;

Type
  TSyncHelper = Class
  Private
    wnd: HWND;
    Procedure MsgProc( Var msg: TMessage );
    Procedure Wakeup( sender: TObject );
  Public
    Constructor Create;
    Destructor Destroy; override;
  End;

Var
  helper: TSyncHelper = nil;

{ TSyncHelper }

Constructor TSyncHelper.Create;
Begin
  inherited;
  wnd:= AllocateHWnd( msgproc );
  WakeMainThread := nil;
  PostMessage( wnd, WM_USER, 0, 0 );
End;

Destructor TSyncHelper.Destroy;
Begin
  WakeMainThread := nil;
  DeallocateHWnd( wnd );
  inherited;
End;

Procedure TSyncHelper.MsgProc(Var msg: TMessage);
Begin
  If msg.Msg = WM_USER Then begin
    WakeMainThread := Wakeup;
    CheckSynchronize;
  end else
    msg.result := DefWindowProc( wnd, msg.msg, msg.WParam, msg.LParam );
End;

Procedure TSyncHelper.Wakeup(sender: TObject);
Begin
  PostMessage( wnd, WM_USER, 0, 0 );
End;

{ TCaptureThread }

constructor TCaptureThread.Create;
begin
  inherited Create(true);
  if Assigned(gScraperClass) then
    FScraper:=gScraperClass.Create(nil);
//  FreeOnTerminate:=true;
  FSessions := TClientSessionList.Create;
  FCs:=TCriticalSection.Create;
  FEventCapture := TEvent.Create(nil,false,false,'');
  FEndWaitEvent := TEvent.Create(nil, True, False, '');
end;

destructor TCaptureThread.Destroy;
begin
  FreeSessions;
  FreeAndNil(FSessions);
  FreeAndNil(FEventCapture);
  FreeAndNil(FEndWaitEvent);
  FreeAndNil(FCs);
  FreeAndNil(FScraper);
  inherited;
end;

function TCaptureThread.GetSession(Id:Integer):TClientSession;
begin
  result:=nil;
  LockList;
  try
    if FSessions.IndexOf(TClientSession(Id))>=0 then
      if not TClientSession(Id).IsGarbage then
      begin
        result := TClientSession(Id);
      end;
      exit;
  finally
    UnlockList;
  end;
end;

procedure TCaptureThread.GetSessionUsers(aUsers: TStrings);
var n : Integer;
begin
  LockList;
  try
    for n := 0 to FSessions.Count - 1 do
      if FSessions[n].User<>'' then
        aUsers.Add(FSessions[n].User);
  finally
    UnlockList;
  end;
end;

procedure TCaptureThread.DisconnectUser(aUser:string);
var n : Integer;
begin
  LockList;
  try
    for n := 0 to FSessions.Count - 1 do
      if FSessions[n].User=AUser then begin
        FSessions[n].ForceDisconnect:=True;
        exit;
      end;
  finally
    UnlockList;
  end;
end;

procedure TCaptureThread.DisconnectAllUsers;
var n : Integer;
begin
  LockList;
  try
    for n := FSessions.Count - 1 downto 0 do
       if (FSessions[n].Ticket<>'') then FSessions[n].ForceDisconnect:=true;
  finally
    UnlockList;
  end;
end;

function TCaptureThread.IsUserActive(AUser: string): Boolean;
var n : Integer;
begin
  result:=False;
  LockList;
  try
    for n := 0 to FSessions.Count - 1 do
      if SameText(FSessions[n].User,AUser) then
        result:=FSessions[n].Active;
  finally
    UnlockList;
  end;
end;

procedure TCaptureThread.Resume;
begin
  FEndWaitEvent.SetEvent;
  inherited resume;
end;

function TCaptureThread.SessionCount: Integer;
begin
  LockList;
  try
    result:=FSessions.Count;
  finally
    UnlockList;
  end;
end;

procedure TCaptureThread.SetScraper(const Value: TAbstractScraper);
var
  n : Integer;
begin
  FScraper := Value;
  LockList;
  try
    for n := 0 to FSessions.Count - 1 do
      TClientSession(FSessions[n]).Scraper:=FScraper;
  finally
    UnlockList;
  end;

end;

procedure TCaptureThread.SyncTimer(Sender:TObject);
begin
  With (Sender as TSimpletimer) do begin
    Enabled:=False;
    FMethod;
    free;
  end;
end;

procedure TCaptureThread.Synchronize(Method:TThreadMethod);
var
  Log : ILogger;
begin
  Log:=TLogger.Create(Self,'Synchronize');
  if IsLibrary and not Assigned(WakeMainThread) then Method
  else begin
{    with TSimpleTimer.Create do begin
      FMethod:=Method;
      Interval:=1;
      OnTimer:=Synctimer;
      Enabled:=true;
    end;
}    inherited Synchronize(Method);
  end;
end;

procedure TCaptureThread.AddSession(ASession:TClientSession);
var
  Log : ILogger;
begin
  Log:=TLogger.Create(Self,'AddSession');
  LockList;
  try
    Log.LogInfo('AfterLockList');
    FSessions.Remove(ASession);
    FSessions.Add(ASession);
    ASession.OnForceCapture:=ForceCapture;
    FReset:=FSessions.Count=1;
    resume;
  finally
    UnlockList;
  end;
  Synchronize(NotifySessionAdded);
end;

procedure TCaptureThread.RemoveSession(ASession:TClientSession;AFree:Boolean=false);
var
  Log : ILogger;
begin
  Log:=TLogger.Create(Self,'RemoveSession');
  if not assigned(FSessions) then exit;

  LockList;
  try
    if FSessions.IndexOf(ASession)=-1 then exit;

    if (FSessions.Remove(ASession)>=0) and AFree then
      ASession.Free;
  finally
    UnlockList;
  end;
end;

procedure TCaptureThread.NotifySessionAdded;
begin
  if Assigned(FOnSessionAdded) then
    FOnSessionAdded(Self);
  if assigned(FScraper) then
    FScraper.SessionAdded(self);
end;

procedure TCaptureThread.NotifySessionRemoved;
begin
  if Assigned(FOnSessionRemoved) then
    FOnSessionRemoved(Self);
  if assigned(FScraper) then
    FScraper.SessionRemoved(self);
end;

procedure TCaptureThread.Terminate;
begin
  inherited;
  FEndWaitEvent.SetEvent;
  Resume;
end;

procedure TCaptureThread.Capture;
//var
//  Log : ILogger;
begin
//  Log:=TLogger.Create(Self,'Capture');
  if FScraper <> nil then
    FScraper.Capture;
end;

procedure TCaptureThread.ForceCapture(Sender:TObject);
begin
  FEventCapture.SetEvent;
end;

procedure TCaptureThread.FreeSessions;
var I: Integer;
  Log : ILogger;
begin
  Log:=TLogger.Create(Self,'FreeSessions');
  LockList;
  try
    if not assigned(FSessions) then exit;

    for I := FSessions.Count - 1 downto 0 do
      RemoveSession(FSessions[I],true);
    FSessions.Clear;
  finally
    UnlockList;
  end;
end;

procedure TCaptureThread.Execute;
  function WaitforSessionsEnabled:boolean;
  var
    n, m,SessionCount : Integer;
    events : array of THandle;
    SessionRemoved : Boolean;
  begin
    SessionRemoved := False;
    LockList;
    try
      FEndWaitEvent.ResetEvent;
      SetLength(events,FSessions.Count + 1);
      events[0] := FEndWaitEvent.Handle;
      m := 1;

      if Assigned(FScraper) then
        FScraper.FlushKeyboard;
      for n := FSessions.Count - 1 downto 0 do
        if FSessions[n].IsGarbage then begin
          RemoveSession(FSessions[n],true);
          SessionRemoved:=true;
        end else begin
          if FSessions[n].Active then begin
            events[m]:= FSessions[n].EnableEvent.Handle;
            Inc(m);
          end;
        end;
      result:=m>1;
      SessionCount:=FSessions.Count;
    finally
      UnlockList
    end;

    if SessionRemoved then
      Synchronize(NotifySessionRemoved);

    if (SessionCount=0) or not result then begin
      LogInfo('Suspending 1');
      Suspend;
    end;

    if result then
      result:=WaitForMultipleObjects(m,@events[0],false,1000)<>WAIT_TIMEOUT;
  end;
  
var
  delta : Integer;
  StartTime : TDatetime;
begin
  while not Terminated do begin
    StartTime:=now;
    if not WaitforSessionsEnabled then continue;
    if Terminated then Break;

    if assigned(FScraper) then begin
      delta:=FScraper.FrameDelay-DateTimeToTimeStamp(Now-StartTime).Time;
      if delta>0 then FEventCapture.WaitFor(delta);
    end;

    ProcessCapture;
  end;
end;

function TCaptureThread.ProcessCapture:Boolean;
var
  n : Integer;
  rc : boolean;
begin
  Result := False;
  if FScraper=nil then exit;

  if FScraper.SynchronizeCapture then
    Synchronize(Capture)
  else Capture;
  LockList;
  try
    if FSessions.Count>0 then begin
      for n := FSessions.Count - 1 downto 0 do begin
        try
          rc:=FSessions[n].ProcessDifferences;
          result:=result or rc;
        except
        end;
      end;
    end;
  finally
    UnlockList;
  end;
end;

procedure TCaptureThread.LockList;
begin
  if assigned(FCs) then
    FCs.Enter;
end;

procedure TCaptureThread.UnlockList;
begin
  if assigned(FCs) then
    FCs.Leave;
end;

Initialization
  if IsLibrary then begin
    Assert(MainThreadId=GetCurrentThreadId);
    helper:= TSyncHelper.Create;
  end;
Finalization
  if Assigned(gCaptureThread) then begin
    gCaptureThread.Terminate;
    gCaptureThread.free;
  end;
  FreeAndNil(helper);
end.

