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

uses Windows,Classes,SysUtils,SyncObjs,
  ThinVnc.Log,
  ThinVnc.MirrorWindow,ThinVnc.ClientSession;

type
  TCaptureThread = class(TThread)
  private
    FWmgr : TMirrorManager;
    FReset : Boolean;
    FSessions : TClientSessionList;
    FCs : TCriticalSection;
    FEventCapture : TEvent;
    FEndWaitEvent: TEvent;
    procedure LockList;
    procedure UnlockList;
    procedure Capture;
    procedure ForceCapture(Sender:TObject);
    function ProcessCapture:boolean;
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
    property  MirrorManager:TMirrorManager read FWmgr;
  end;

var
  gCaptureThread : TCaptureThread;

implementation

{ TCaptureThread }

constructor TCaptureThread.Create;
begin
  inherited Create(true);
  FWmgr:=TMirrorManager.Create;
//  FreeOnTerminate:=true;
  FSessions := TClientSessionList.Create;
  FCs:=TCriticalSection.Create;
  FEventCapture := TEvent.Create(nil,false,false,'');
  FEndWaitEvent := TEvent.Create(nil, True, False, '');
end;

destructor TCaptureThread.Destroy;
begin
  if gCaptureThread = Self then
    gCaptureThread := nil;
  FreeSessions;
  FreeAndNil(FSessions);
  FreeAndNil(FEventCapture);
  FreeAndNil(FEndWaitEvent);
  FreeAndNil(FCs);
  FreeAndNil(FWmgr);
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
        result.Update;
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
       FSessions[n].ForceDisconnect:=true;
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

procedure TCaptureThread.AddSession(ASession:TClientSession);
begin
  LockList;
  try
    FSessions.Remove(ASession);
    FSessions.Add(ASession);
    ASession.OnForceCapture:=ForceCapture;
    FReset:=FSessions.Count=1;
  finally
    UnlockList;
  end;
end;

procedure TCaptureThread.RemoveSession(ASession:TClientSession;AFree:Boolean=false);
begin
  if not assigned(FSessions) then exit;

  LockList;
  try
    if (FSessions.Remove(ASession)>=0) and AFree then
      ASession.Free;
  finally
    UnlockList;
  end;
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
  FWmgr.Capture;
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
  begin
    LockList;
    try
      FEndWaitEvent.ResetEvent;
      SetLength(events,FSessions.Count + 1);
      events[0] := FEndWaitEvent.Handle;
      m := 1;
      for n := FSessions.Count - 1 downto 0 do
        if FSessions[n].IsGarbage then begin
          RemoveSession(FSessions[n],true);
        end else begin
          FSessions[n].FlushKeyboard;
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

    if (SessionCount=0) or not result then begin
      LogInfo('Suspending 1');
      Suspend;
    end;

    if result then begin
      if WaitForMultipleObjects(m,@events[0],false,10000)=WAIT_TIMEOUT then
      begin
        LogError('Timeout detected');
        // Fuerzo la rehabilitacion para que no se cuelgue
        for n := FSessions.Count - 1 downto 0 do
          FSessions[n].Enable;
      end;
    end;
  end;
var
  delta : Integer;
begin
  while not Terminated do begin
    WaitforSessionsEnabled;
    if Terminated then Break;

    ProcessCapture;
    delta:=10;//100;//50;//100-DateTimeToTimeStamp(Now-CaptureTime).Time
    if delta>0 then
      FEventCapture.WaitFor(delta);
  end;
end;

function TCaptureThread.ProcessCapture:Boolean;
var
  n : Integer;
  rc : boolean;
begin
  Result := False;
  Capture;
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

initialization
  gCaptureThread := TCaptureThread.Create;
finalization
  if Assigned(gCaptureThread) then begin
    gCaptureThread.Terminate;
    FreeAndNil(gCaptureThread);
  end;
end.

