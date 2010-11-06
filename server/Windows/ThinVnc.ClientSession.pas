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

unit ThinVnc.ClientSession;
{$I ThinVnc.DelphiVersion.inc}
{$I ThinVnc.inc}

interface
uses ActiveX,Windows,Messages,Classes,Sysutils,Graphics,Forms,jpeg, Contnrs,
  SyncObjs,StrUtils,Clipbrd,Multimon,
  ThinVnc.TcpCommon,
  ThinVnc.Log,
  ThinVnc.Utils,
  ThinVNC.LkJSON,
  ThinVnc.SessionWindow,
  ThinVnc.Cursor,
  ThinVnc.AdvHttp,
  ThinVnc.InputQueue,
  ThinVNC.DigestAuth,
  ThinVnc.MirrorWindow;

type
  TCallbackSendEvent = procedure (Text:AnsiString;APacketNumber:Cardinal) of object;
  TCallbackTextEvent = procedure (Text:AnsiString) of object;
  TClientSession = class;
  TClientSessionClass = class of TClientSession;
  TConnectStatus = (csNone,csLocal,csRemote);
  TAuthStatus = (asAuthNotRequired,asAuthenticated,asAuthRequired);
  TAuthGetPasswordEvent = procedure (ASession:TClientSession;
                                      AUser:string;var Password : String) of object;
  TAuthGetTypeEvent = procedure(ASession:TClientSession;
                              var AuthType:TAuthenticationType;var AuthRealm:string) of object;

  TGatewayConnectEvent =  function (Session:TClientSession):Boolean of object;
  TProcessAuthenticationEvent =   procedure (Sender:TObject;var Username:string;var AuthStatus:TAuthStatus;var Error:string) of object;

  TClientType = (ctAjax,ctWebSockets,ctBinaryClient,ctPresentationViewer);
  TClientSession = class
  private
    FMouseControl : Boolean;
    FKbdControl   : Boolean;
    FProcessIds: array of Integer;
    FEmbeddedImage: Boolean;
    FSeamless : Boolean;
    FUseCanvas: Boolean;
    FJpgQuality: Integer;
    FJpgPixelFormat: TJPEGPixelFormat;
    FJpgGrayscale: boolean;
    FActive: Boolean;
    FOnSendCmd: TCallbackSendEvent;
    FOnSendBuf: TCallbackSendEvent;
    FLastUsed : TDateTime;
    FPolled : Boolean;
    FEnableEvent : TEvent;
    FAuthStatus : TAuthStatus;
    FSendThread : TSendThread;
    FOnForceCapture: TNotifyevent;
    FBinaryImages: Boolean;
    FRemotePointer : Boolean;
    FResetRemotePointer : Boolean;
    FOldSignature : string;
    FInputQueue  : TThinBufferQueueThread;
    FActiveMonitor : Integer;
    FCursorWindow : TCursorWindow;
    FDestAddr: string;
    FOnSendScreen: TCallbackSendEvent;
    FIsLocal: Boolean;
    FConnectStatus: TConnectStatus;
    FConnectError: string;
    FGarbage : Boolean;
    FAuthenticationData: string;
    FauthByPass : string;
    FNeedRemoteAuthentication: Boolean;
    FClientType : TClientType;
    FId : string;
    FUser: string;
    FTicket: string;
    FForceDisconnect: boolean;
    FJsonEvent: TEvent;
    FJsonText: AnsiString;
    FAuthDigestServer : TAuthDigestServer;
    FAuthorization : string;
    FOnAuthGetPassword: TAuthGetPasswordEvent;
    FOnAuthGetType: TAuthGetTypeEvent;
    FOnGatewayConnect: TGatewayConnectEvent;
    FChangedRgn : HRGN;
    FAuthenticationPassed: Boolean;
    FOnProcessAuthentication: TProcessAuthenticationEvent;
    FImageMethod: TImageMethod;
    FLock : TCriticalsection;
    procedure SetJpgGrayscale(const Value: boolean);
    procedure SetJpgPixelFormat(const Value: TJPEGPixelFormat);
    procedure SetJpgQuality(const Value: Integer);
    function  IsProcessInList(APid: Integer): Boolean;
    function  RefreshWindows:boolean;
    procedure Refresh;
    procedure OnEmptyQueue(Sender: TObject);
    procedure AckReceived(APacketNumber: Cardinal);
    procedure SetActive(const Value: Boolean);
    procedure SetDestAddr(const Value: string);
    function GetActiveMonitor: Integer;
    procedure SetScraper(const Value: TAbstractScraper);
    procedure   AccumChangedRgn;
    procedure   ClearChangedRgn;
    procedure DumpMonitorInfo;
    function RealMonitorIndex(Index:Integer): Integer;
    function RealMonitorCount: Integer;
  protected
    FScraper : TAbstractScraper;
    FForceRefresh : Boolean;
    FLastCaptureIndex : Integer;
    FSessionWindowList : TSessionWindowList;
    procedure   QueuedProcessPacket(Sender: TObject; Item: TThinQueueItem);virtual;
    property    JsonText: AnsiString read FJsonText write FJsonText;
    procedure   ProcessKeyboardInput(key,ch: word; Down: boolean);overload;virtual;
    procedure   ProcessKeyboardInput(key: word; Down: boolean);overload;virtual;
    procedure   ProcessMouseInput(X,Y,delta,Btn:Integer;action:string);virtual;
    procedure   GetCursor(var cur: string; var curX, curY: Integer);virtual;
    function    ExecCommandLineOrder(jsObj: TlkJSONobject): AnsiString;
  public
    constructor Create(APolled:Boolean=false); virtual;
    destructor  Destroy;override;
    class function IsValid(AClientSession:TClientSession):Boolean;
    function    InputQueue:TThinBufferQueueThread;
    procedure   Update;
    function    IsGarbage:Boolean;
    procedure   Enable;
    function    TryLock:Boolean;
    procedure   Lock;
    procedure   Unlock;
    procedure   SetGarbage;
    function    GetDifferences:boolean;
    function    ProcessDifferences:boolean;
    function    GetJson(path:string=''):string;
    function    ProcessPacket(jsObj: TlkJSONobject):AnsiString; overload;virtual;
    function    ProcessPacket(JsonText: Ansistring):AnsiString;  overload;
    function    GetSessionStatus:AnsiString;
    function    GetConnectStatus: AnsiString;
    procedure   SendImages;
    procedure   SendBuffer(AStream: TStream);
    procedure   SendCmd(AText:AnsiString);
    procedure   SendScreen(AText: AnsiString);
    function    WaitForJson(out output: AnsiString): Boolean; virtual;
    property    Polled : Boolean read FPolled write FPolled;
    property    DestAddr : string read FDestAddr write SetDestAddr;
    property    IsLocal: Boolean read FIsLocal;
    property    Active: Boolean read FActive write SetActive;
    property    OnSendScreen : TCallbackSendEvent read FOnSendScreen write FOnSendScreen;
    property    OnSendCmd : TCallbackSendEvent read FOnSendCmd write FOnSendCmd;
    property    OnSendBuf : TCallbackSendEvent read FOnSendBuf write FOnSendBuf;
    property    OnAuthGetPassword: TAuthGetPasswordEvent read FOnAuthGetPassword write FOnAuthGetPassword;
    property    OnAuthGetType: TAuthGetTypeEvent read FOnAuthGetType write FOnAuthGetType;
    property    EmbeddedImage:Boolean read FEmbeddedImage write FEmbeddedImage;
    property    BinaryImages:Boolean read FBinaryImages write FBinaryImages;
    property    UseCanvas:Boolean read FUseCanvas write FUseCanvas;
    property    JpgGrayscale:boolean read FJpgGrayscale write SetJpgGrayscale;
    property    JpgQuality:Integer read FJpgQuality write SetJpgQuality;
    property    JpgPixelFormat:TJPEGPixelFormat read FJpgPixelFormat write SetJpgPixelFormat;
    property    EnableEvent: TEvent read FEnableEvent;
    property    JsonEvent: TEvent read FJsonEvent;
    property    OnForceCapture:TNotifyevent read FOnForceCapture write FOnForceCapture;
    property    ConnectStatus: TConnectStatus read FConnectStatus write FConnectStatus;
    property    ConnectError: string read FConnectError;
    property    OnProcessAuthentication:TProcessAuthenticationEvent read FOnProcessAuthentication write FOnProcessAuthentication;
    property    OnGatewayConnect:TGatewayConnectEvent read FOnGatewayConnect write FOnGatewayConnect;
    property    NeedRemoteAuthentication: Boolean read FNeedRemoteAuthentication;
    property    AuthenticationData: string read FAuthenticationData;
    property    ActiveMonitor:Integer read GetActiveMonitor;
    property    Ticket:string read FTicket write FTicket;    
    property    User:string read FUser write FUser;
    property    Id:string read FId write FId;
    property    ForceDisconnect:boolean read FForceDisconnect write FForceDisconnect;
    property    Scraper: TAbstractScraper read FScraper write SetScraper;
    property    AuthenticationStatus : TAuthStatus read FAuthStatus;
    property    ImageMethod: TImageMethod read FImageMethod write FImageMethod;
  end;

  TClientSessionList = class(TList)
  private
    function GetItems(Index: Integer): TClientSession;
  public
    function Add(Item: TClientSession): Integer;
    property Items[Index: Integer]: TClientSession read GetItems; default;
  end;

var
  ClientSessionClass: TClientSessionClass = TClientSession;

function FindHeader(List: TStrings; Header: string; var Value: string): Boolean;
function GetClientSession(id:string):TClientSession;

procedure Cleanup;

implementation

uses
  ThinVnc.Capture;

var
  gSessionList : TList;

function GetClientSession(id:string):TClientSession;
var
  n : Integer;
begin
  result:=nil;
  for n := 0 to gSessionList.Count - 1 do
    if not TClientSession(gSessionList[n]).IsGarbage and (TClientSession(gSessionList[n]).FId=Id) then begin
      result := TClientSession(gSessionList[n]);
      break;
    end;
end;

function FindHeader(List: TStrings; Header: string; var Value: string): Boolean;
var
  HdrName, N: string;
  HdrSize, I, J: Integer;
begin
  Result := False;
  Value := '';
  HdrName := Header + ':';
  HdrSize := Length(HdrName);
  for I := 0 to List.Count - 1 do
  begin
    N := List[I];
    J := Pos(':', N);
    if J >= 0 then
      if SameText(HdrName, Copy(N, 1, HdrSize)) then
      begin
        Result := True;
        //Value := Trim(Copy(N, HdrSize + 1, MaxInt));
        Value := N;
        Break;
      end;
  end;
end;

procedure Cleanup;
begin
end;

{ TClientSession }

constructor TClientSession.Create(APolled:Boolean);
var
  Log : ILogger;
  n : Integer;
  Guid : TGuid;
begin
  Log := TLogger.Create(self,'Create');
  FSessionWindowList := TSessionWindowList.Create;
  SetScraper(CaptureThread.Scraper);
  FEmbeddedImage := true;
  FBinaryImages := false;
  FJpgQuality:=85;
  FPolled:=APolled;
  FEnableEvent := TEvent.Create(nil, true,false,'');
  FJsonEvent := TEvent.Create(nil, true,false,'');
  FCursorWindow := TCursorWindow.Create;
  FIsLocal := True;

  FActiveMonitor := 0;
  for N := 0 to MyScreen.MonitorCount - 1 do
    if MyScreen.Monitors[n].Primary then begin
      FActiveMonitor:=N;
      break;
    end;

  Update;
  FAuthDigestServer := TAuthDigestServer.Create;
  FLastCaptureIndex:=-1;
  gSessionList.Add(self);
  CoCreateGuid(Guid);
  FId:=GuidToString(Guid);
  FLock := TCriticalsection.Create;
end;

destructor TClientSession.Destroy;
var
  Log : ILogger;
begin
  Log:=TLogger.Create(Self,Format('Destroy: %d',[Integer(self)]));
  gSessionList.Remove(self);
  CaptureThread.RemoveSession(Self);
  //FlushKeyboard(true);
  Active := False;
  if FJsonEvent <> nil then
  begin
    FJsonEvent.SetEvent;
    Sleep(20); // Makes sure WaitForJson ends
    FreeAndNil(FJsonEvent);
  end;
  OnSendScreen := nil;
  OnSendCmd := nil;
  OnSendBuf := nil;
  if assigned(FInputQueue) then
    FInputQueue.Terminate;
  FreeAndNil(FCursorWindow);
  if Assigned(FSendThread) then begin
    FSendThread.Terminate;
    FSendThread.Continue;
    FSendThread := nil;
  end;
  FreeAndNil(FAuthDigestServer);
  FreeAndNil(FSessionWindowList);
  FreeAndNil(FEnableEvent);
  FreeAndNil(FLock);
  inherited;
end;


function TClientSession.IsGarbage: Boolean;
begin
  result:=FGarbage or FPolled and FActive and (DateTimeToTimeStamp(Now-FLastUsed).Time>60000);
end;

function TClientSession.IsProcessInList(APid: Integer):Boolean;
var n : Integer;
begin
  result:=Length(FProcessIds)=0;
  for n := 0 to Length(FProcessIds) - 1 do begin
    result:=(FProcessIds[n]=APid) or (FProcessIds[n]=0);
    if result then exit;
  end;
end;

class function TClientSession.IsValid(AClientSession: TClientSession): Boolean;
begin
  result:=gSessionList.IndexOf(AClientSession)>-1
end;

function TClientSession.RefreshWindows:boolean;
 function GetSignature(List:TSessionWindowList):string;
  var
    I: Integer;
    sw : TSessionWindow;
  begin
    result:='';
    for I := 0 to List.Count - 1 do
    begin
      sw := List[I];
      result:=result+Format('h:%d-%d-%d-%d-%d-%s;',[sw.Handle,sw.BoundsRect.Left,sw.BoundsRect.Right,
        sw.BoundsRect.Right,sw.BoundsRect.Bottom,BoolToStr(sw.IsMasked,true)]);
    end;
  end;

var
  wm : TMirrorWindow;
  sw : TSessionWindow;
  n,m: Integer;
  found : boolean;
begin
  if FForceRefresh then FSessionWindowList.Clear;
  FForceRefresh:=false;
  Result := False;
  if FScraper = nil then Exit;

  for n := FSessionWindowList.Count - 1 downto 0 do begin
    found:=false;
    for m := 0 to FScraper.MirrorList.Count - 1 do begin
      wm := FScraper.MirrorList[m];
      if wm.Handle=FSessionWindowList[n].Handle then begin
        Found:=true;
        break;
      end;
    end;
    if not found then FSessionWindowList.Delete(n)
    else FSessionWindowList[n].UpdateBitmap;
  end;

  for n := 0 to FScraper.MirrorList.Count - 1 do begin
    wm := FScraper.MirrorList[n];
    found:=false;
    for m := 0 to FSessionWindowList.Count - 1 do begin
      sw:=FSessionWindowList[m];
      if wm.Handle=sw.Handle then begin
        Found:=true;
        break;
      end;
    end;
    if not found then begin
      sw:=TSessionWindow.Create(wm);
      FSessionWindowList.Add(sw);
    end;
  end;

  result:=FOldSignature<>GetSignature(FSessionWindowList);
  if result then
    LogInfo('SessionWindowList changed.');
  FOldSignature:=GetSignature(FSessionWindowList);
end;

function TClientSession.ProcessDifferences:Boolean;
begin
  result:=False;
  AccumChangedRgn;
  if FEnableEvent.Waitfor(0)=wrSignaled then begin
    if ForceDisconnect then begin
      SendScreen('{"status":9,"windows": []}');
      FActive:=False;
      FGarbage:=True;
      ForceDisconnect:=False;
      result:=true;
      exit;
    end;

    result:=GetDifferences;
    if result then begin
      FEnableEvent.ResetEvent;
      //if FJsonEvent.WaitFor(0) = wrSignaled then Exit;
      //if not Assigned(OnSendScreen) then exit;
      SendScreen(GetJson);
      if BinaryImages then SendImages;
    end;
    ClearChangedRgn;
  end else LogInfo('ProcessDifferences FEnableEvent not enabled');
end;

function TClientSession.GetDifferences:Boolean;
var
  sw : TSessionWindow;
  m : Integer;
  rc,forcerefresh : Boolean;
begin
  result:=false;
  if FScraper = nil then Exit;
  forcerefresh:=FForceRefresh;
  result:=RefreshWindows;

  if (FLastCaptureIndex<>FScraper.CaptureIndex) then
    for m := FSessionWindowList.Count - 1 downto 0 do begin
      sw:=TSessionWindow(FSessionWindowList[m]);
      if not sw.Active then continue;
      if (Length(FProcessIds)>0) and not IsProcessInList(sw.ProcessId) then continue;
      rc:=sw.CaptureDifferences(FChangedRgn,FActiveMonitor,forcerefresh);
      result:=result or rc;
    end;

  if (FScraper <> nil) and FScraper.SendCursor then begin
    rc:=FCursorWindow.Capture(FResetRemotePointer);
    FResetRemotePointer:=false;
    if FRemotePointer or FCursorWindow.ShapeChanged then begin
      result:=result or rc or FCursorWindow.ShapeChanged;
    end;
  end;
  LogInfo(Format('GetDifferences result: %s',[BoolToStr(result,true)]));
end;

procedure TClientSession.GetCursor(var cur:string;var curX,curY:Integer);
begin
  if (FScraper <> nil) and FScraper.SendCursor then
    FCursorWindow.GetCursor(cur,curX,curY);
end;

function TClientSession.GetJson(path: string): string;
var n,curX,curY : Integer;
  json,cur : string;
  Log : Ilogger;
begin
  Log := TLogger.Create(Self,'GetJson');
  json:='';
  FLastCaptureIndex:=FScraper.CaptureIndex;
  for n := 0 to FSessionWindowList.Count - 1 do begin
    if FSeamless and FSessionWindowList[n].IsShellWindow then continue;
    if not IsProcessInList(FSessionWindowList[n].ProcessId) then continue;
    if json<>'' then json:=json+','+#13#10;
    json:=json+#9+FSessionWindowList[n].getJson(path,EmbeddedImage,FBinaryImages,FImageMethod,JpgQuality,JpgPixelFormat,JpgGrayScale);
  end;

  cur:='Default';
  curX:=0;
  curY:=0;

  if (FScraper <> nil) and FScraper.SendCursor then begin
    if FRemotePointer then
      json:=json+','+#13#10+FCursorWindow.getJson;
  end;

  GetCursor(cur,curX,curY);
  result:=Format('{"status":1,"desktopWidth":%d,"desktopHeight":%d,"cursor":"%s","cursorX":%d,"cursorY":%d,"windows": ['+#13#10+json+#13#10+']}',
        [Scraper.DVWidth(ActiveMonitor),Scraper.DVHeight(ActiveMonitor),cur,curX,curY]);
end;

function TClientSession.GetActiveMonitor: Integer;
begin
  result:=FActiveMonitor;
end;

function TClientSession.GetConnectStatus: AnsiString;
var
  jsObj: TlkJSONobject;
  AuthType : TAuthenticationType;
  AuthRealm : string;
begin
//  if not FPolled then FAuthStatus:=asAuthNotRequired;
  jsObj :=TlkJSONobject.Create();
  try
    jsObj.Add('cmd', 'connectStatus');
    jsObj.Add('id', FId);
    jsObj.Add('status', Integer(FConnectStatus));
    jsObj.Add('authStatus', Integer(FAuthStatus));
    if FConnectError <> '' then
      jsObj.Add('errormsg', FConnectError);
    jsObj.Add('dateTime',FormatDateTime('yyyy-mm-dd hh:mm:ss:zzz',Now));
    result:=TlkJSON.GenerateText(jsObj);
  finally
    jsObj.Free;
  end;
end;

procedure TClientSession.SendImages;
var n,m : Integer;
  bmpp : TBmpPart;
  ms : TStringStream;
  Log : Ilogger;
begin
  Log := TLogger.Create(Self,'SendImages');
  for n := 0 to FSessionWindowList.Count - 1 do begin
    if FSeamless and FSessionWindowList[n].IsShellWindow then continue;
    if not IsProcessInList(FSessionWindowList[n].ProcessId) then continue;
    for m:=0 to FSessionWindowList[n].DiffBmpList.Count-1 do begin
      bmpp:=FSessionWindowList[n].DiffBmpList[m];
      bmpp.GetStream(FImageMethod,JpgQuality,JpgPixelFormat,JpgGrayScale);
      bmpp.Stream.Seek(0,0);
      ms:=TStringStream.Create(bmpp.id +':');
      try
        ms.Seek(ms.Size,0);
        ms.CopyFrom(bmpp.Stream,bmpp.Stream.size);
        ms.Seek(0,0);
        SendBuffer(ms);
        LogInfo(Format('SendImage id=%s, size:%d',[bmpp.id,bmpp.Stream.size]));
      finally
        ms.Free;
      end;
    end;
    FSessionWindowList[n].ApplyDifferences;
  end;
end;

procedure TClientSession.Refresh;
begin
  FForceRefresh:=true;
  FLastCaptureIndex:=-1;
  FResetRemotePointer:=true;
end;

procedure TClientSession.AccumChangedRgn;
var
  n : Integer;
  Rgn : HRGN;
begin
  if FScraper = nil then Exit;
  if FScraper.ChangedRgn=0 then exit;
  
  if FChangedRgn=0 then
    FChangedRgn:=CreateRectRgn(0,0,0,0);

  CombineRgn(FChangedRgn, FScraper.ChangedRgn,FChangedRgn, RGN_OR);
end;

procedure TClientSession.ClearChangedRgn;
var
  Log : ILogger;
begin
  if FChangedRgn<>0 then begin
    DeleteObject(FChangedRgn);
    FChangedRgn:=0;
  end;
end;

procedure TClientSession.AckReceived(APacketNumber:Cardinal);
var
  Log:ILogger;
begin
  Log:=TLogger.Create(self,Format('AckReceived: %d',[APacketNumber]));
  If Assigned(FSendThread) and (FSendThread.LastPacketNumber=APacketNumber) then
    FEnableEvent.SetEvent;
end;

procedure TClientSession.Enable;
var
  Log:ILogger;
begin
  Log:=TLogger.Create(self,'Enable');
  FEnableEvent.SetEvent;
end;

function TClientSession.ExecCommandLineOrder(jsObj: TlkJSONobject): AnsiString;
    function GetVk(C:Char):word;
    begin
      result:=VkKeyScanEx(C,0);
    end;
var
  jsResp: TlkJSONobject;
  Ok: Boolean;
  cliType, cliAction, cliText, cliLineMode: AnsiString;
begin
  Ok := False;
  jsResp := TlkJSONobject.Create;
  try
    jsResp.add('cmd', 'cli');
    try
      if jsObj.Field['type'] <> nil then
      begin
        cliType := jsObj.Field['type'].Value;
        jsResp.add('type', cliType);
        if SameText(cliType, 'clipboard') then
        begin
          cliAction := jsObj.Field['action'].Value;
          jsResp.add('action', cliAction);
          if SameText(cliAction, 'copy') then
          begin
            Clipboard.Open;
            try
              if Clipboard.HasFormat(CF_TEXT) then
                cliText := Clipboard.AsText else
                cliText := '';
            finally
              Clipboard.Close;
            end;
          end else begin
            cliText := jsObj.Field['textdata'].Value;
            cliLineMode := jsObj.Field['linemode'].Value;
            if cliLineMode = 'CR' then
              cliText := StringReplace(cliText, #10, #13#10, [rfReplaceAll]);
            Clipboard.Open;
            try
              Clipboard.AsText := cliText;
            finally
              Clipboard.Close;
            end;
            ProcessKeyboardInput(VK_CONTROL, True);
            ProcessKeyboardInput(Ord('V'), True);
            Sleep(10);
            ProcessKeyboardInput(VK_CONTROL, False);
            ProcessKeyboardInput(Ord('V'), False);
            cliText := '';
          end;
          Ok := True;
          jsResp.add('text', cliText);
        end;
      end;
      if not Ok then Abort;
    except
      on E: Exception do
      begin
        jsResp.Add('error', True);
      end;
    end;
    Result := TlkJSON.GenerateText(jsResp);
  finally
    jsResp.Free;
  end;
end;

procedure TClientSession.SendCmd(AText: AnsiString);
var
  Log : ILogger;
begin
  Log:=TLogger.Create(Self,'SendCmd');
  Log.LogInfo(AText);
  try
    if FPolled then begin
      if Assigned(OnSendCmd) then
        try
          OnSendCmd(AText,1);
        except
          OnSendCmd:=nil;
        end;
    end else begin
      if not assigned(FSendThread) then begin
        FSendThread:=TSendThread.Create;
        FSendThread.OnEmptyQueue:=OnEmptyQueue;
      end;
      FSendThread.SendCmd(OnSendCmd,AText);
    end;
  except
    On E:Exception do
      LogException (E.Message);
  end;
end;

procedure TClientSession.SendScreen(AText: AnsiString);
var
  Log : ILogger;
begin
  Log:=TLogger.Create(Self,'SendScreen');
  try
    if FPolled then begin
      if Assigned(OnSendScreen) then
      begin
        try
          OnSendScreen(AText,1);
        except
          OnSendScreen:=nil;
        end;
      end else begin
        FJsonText := AText;
        FJsonEvent.SetEvent;
      end;
    end else begin
      if not assigned(FSendThread) then begin
        FSendThread:=TSendThread.Create;
        FSendThread.OnEmptyQueue:=OnEmptyQueue;
      end;
      Log.LogInfo(Format('Sending Bytes:%d',[Length(AText)]));
      FSendThread.SendCmd(OnSendScreen,AText);
    end;
  except
    on E:Exception do
      LogException (E.Message);
  end;
end;

procedure TClientSession.SendBuffer(AStream: TStream);
var
  Text : Ansistring;
begin
  try
    AStream.Seek(0,0);
    SetLength(Text,AStream.Size);
    AStream.Read(Text[1],AStream.Size);
    if not assigned(FSendThread) then begin
      FSendThread:=TSendThread.Create;
      FSendThread.OnEmptyQueue:=OnEmptyQueue;
    end;
    FSendThread.SendCmd(OnSendBuf,Text);
  except
    On E:Exception do
      LogException (E.Message);
  end;
end;

procedure TClientSession.OnEmptyQueue(Sender:TObject);
begin
//  Enable;
end;

procedure TClientSession.SetActive(const Value: Boolean);
var
  Log : ILogger;
begin
  Log:=TLogger.Create(Self,'SetActive');
  if Value<>FActive then begin
    FActive := Value;
    if Value then
      CaptureThread.AddSession(Self)
    else CaptureThread.RemoveSession(Self);
  end;
end;

procedure TClientSession.SetDestAddr(const Value: string);
var I, P: Integer; A: string;
begin
  FConnectError:='';
  FDestAddr := Trim(Value);
  if FDestAddr = '' then FIsLocal := True else
  begin
    I := Pos(':', FDestAddr);
    if I = 0 then
    begin
      A := FDestAddr;
      P := 8080;
    end else begin
      A := Copy(FDestAddr, 1, I - 1);
      P := StrToIntDef(Copy(FDestAddr, I + 1, MaxInt), 8080);
      if (P <= 0) or (P > 65535) then P := 8080;
    end;
    FIsLocal := IsThisComputer(A);
    FDestAddr := Format('%s:%d', [A, P]);
  end;
end;

procedure TClientSession.SetGarbage;
begin
  FGarbage:=true;
end;

procedure TClientSession.SetJpgGrayscale(const Value: boolean);
begin
  if FJpgGrayscale <> Value then begin
    FJpgGrayscale := Value;
    Refresh;
  end;
end;

procedure TClientSession.SetJpgPixelFormat(const Value: TJPEGPixelFormat);
begin
  if FJpgPixelFormat <> Value then begin
    FJpgPixelFormat := Value;
    Refresh;
  end;
end;

procedure TClientSession.SetJpgQuality(const Value: Integer);
begin
  if FJpgQuality <> Value then begin
    FJpgQuality := Value;
    Refresh;
  end;
end;

procedure TClientSession.SetScraper(const Value: TAbstractScraper);
begin
  FScraper := Value;
  if FScraper<>nil then begin
    FImageMethod := FScraper.ImageMethod;
    FJpgPixelFormat:=FScraper.JpgPixelFormat;
    FRemotePointer:=FScraper.RemotePointer;
    FResetRemotePointer:=FRemotePointer;
  end;
end;

function TClientSession.TryLock: Boolean;
begin
  result:=FLock.TryEnter;
end;

procedure TClientSession.Lock;
begin
  FLock.Enter;
end;

procedure TClientSession.Unlock;
begin
  FLock.Release;
end;

procedure TClientSession.Update;
begin
  FLastUsed:=Now;
end;

function TClientSession.WaitForJson(out output: AnsiString): Boolean;
var
  Log : ILogger;
begin
  Log := TLogger.Create(self, 'WaitForJson : ' + IntToStr(Integer(self)));
  FJsonText:='';
  FJsonEvent.ResetEvent;
  Enable;
  LogInfo('WaitForEvent(FJsonEvent): '+IntToStr(Integer(FJsonEvent)));
  result:=FJsonEvent.WaitFor(30000)=wrSignaled;
//  result:=WaitForEvent(FJsonEvent,10000)=wrSignaled;
  output:=FJsonText;
end;

procedure TClientSession.ProcessKeyboardInput(key,ch: word; Down:boolean);
begin
  if not FKbdControl then exit;
  FScraper.ProcessKeyboardInput(key,ch, Down);
end;

procedure TClientSession.ProcessKeyboardInput(key: word; Down:boolean);
begin
  if not FKbdControl then exit;
  FScraper.ProcessKeyboardInput(key,0, Down);
end;

procedure TClientSession.ProcessMouseInput(X, Y,delta, Btn: Integer; action: string);
begin
  if not FMouseControl then exit;
  if FScraper = nil then Exit;
  FScraper.ProcessMouseInput(X, Y,delta, Btn,action);
end;

function TClientSession.ProcessPacket(JsonText: Ansistring):AnsiString;
var
  jsObj:TlkJSONobject;
  Log : ILogger;
begin
  Log := TLogger.Create(Self,'ProcessPacket');
  result:='';
  jsObj:=TlkJSON.ParseText(JsonText) as TlkJSONobject;
  if not Assigned(jsObj) then exit;
  try
    result:=ProcessPacket(jsObj);
    if result<>'' then
      SendCmd(result);
  finally
    FreeAndNil(jsObj);
  end;
end;

function TClientSession.ProcessPacket(jsObj: TlkJSONobject):AnsiString;
  procedure SetParams(jsObj:TlkJSONobject);
  var
    Log : ILogger;
  begin
    Log := TLogger.Create(Self,'SetParams');
    if (jsObj.IndexOfName('mouseControl')<>-1) and (FTicket='') then
      FMouseControl:=jsObj.Field['mouseControl'].Value;
    if (jsObj.IndexOfName('kbdControl')<>-1) and (FTicket='') then
      FKbdControl:=jsObj.Field['kbdControl'].Value;
    if jsObj.IndexOfName('grayscale')<>-1 then
      JpgGrayscale:=jsObj.Field['grayscale'].Value;
    if jsObj.IndexOfName('quality')<>-1 then
      JpgQuality:=jsObj.Field['quality'].Value;
    if jsObj.IndexOfName('pixelFormat')<>-1 then
      JpgPixelFormat:=jsObj.Field['pixelFormat'].Value;
    if jsObj.IndexOfName('imageMethod')<>-1 then
      ImageMethod:=TImageMethod(jsObj.Field['imageMethod'].Value);
    if jsObj.IndexOfName('embeddedImage')<>-1 then
      FEmbeddedImage:=jsObj.Field['embeddedImage'].Value;
    if jsObj.IndexOfName('binaryImages')<>-1 then
      FBinaryImages:=jsObj.Field['binaryImages'].Value;
    if jsObj.IndexOfName('authorization')<>-1 then
      FAuthorization:=jsObj.Field['authorization'].Value;
    if jsObj.IndexOfName('remotePointer')<>-1 then begin
      FRemotePointer:=jsObj.Field['remotePointer'].Value;
      FResetRemotePointer:=FRemotePointer;
    end;
    if (jsObj.IndexOfName('id')<>-1) and (Length(jsObj.Field['id'].Value)>1) then
      FId:=jsObj.Field['id'].Value;
    if jsObj.IndexOfName('monitor')<>-1 then begin
      FActiveMonitor:=RealMonitorIndex(jsObj.Field['monitor'].Value);
      Refresh;
    end;
    if jsObj.IndexOfName('destAddr')<>-1 then
      SetDestAddr(jsObj.Field['destAddr'].Value);
    if jsObj.IndexOfName('clientType')<>-1 then begin
      FClientType:=TClientType(jsObj.Field['clientType'].Value);
        end;
        if jsObj.IndexOfName('authByPass')<>-1 then begin
      FauthByPass:=jsObj.Field['authByPass'].Value;
    end;
  end;
var
  cmd : string;
  Headers: TStringList;
  Code: Integer;
  Body,lUser: string;
  Log : ILogger;
begin
  Log := TLogger.Create(Self,'ProcessPacket 2');
  result:='{}';
  if jsObj.IndexOfName('cmd') < 0 then exit;
  cmd:=jsObj.Field['cmd'].Value;
  LogInfo('Command: '+cmd);
  if Sametext(cmd,'start') then begin
    SetParams(jsObj);
    Refresh;
    Active:=true;
    result:=GetSessionStatus;
  end else if Sametext(cmd,'stop') then begin
    Active:=false;
    //FlushKeyboard(true);
    result:=GetSessionStatus;
  end else if Sametext(cmd,'disconnect') then begin
    FActive:=False;
    FGarbage:=True;
    result:=GetSessionStatus;
  end else if Sametext(cmd,'refresh') then begin
    Refresh;
    Enable;
  end else if Sametext(cmd,'queryStatus') then
    result:=GetSessionStatus
  else if Sametext(cmd,'ack') then begin
    AckReceived(jsObj.Field['number'].Value);
    result:='';
    exit;
  end else if Sametext(cmd,'ready') then
    enable
  else if Sametext(cmd,'showWindow') then begin
    ShowWindow(jsObj.Field['wnd'].Value,SW_RESTORE);
  end else if Sametext(cmd,'switchSeamless') then begin
    FSeamLess:=not FSeamLess;
    result:=GetSessionStatus;
    Refresh;
  end else if Sametext(cmd,'authenticate') then begin
    FAuthorization:=jsObj.Field['authorization'].Value;
    result:=GetConnectStatus;
  end else if Sametext(cmd,'params') then begin
    SetParams(jsObj);
    result:=GetSessionStatus;
  end else if Sametext(cmd,'mouse') then begin
    InputQueue.AddBuffer(TlkJSON.GenerateText(jsObj));
    if not FPolled then result:='';
  end else if Sametext(cmd,'keyb') then begin
    InputQueue.AddBuffer(TlkJSON.GenerateText(jsObj));
    if not FPolled then result:='';
  end else if Sametext(cmd,'fkey') then begin
    if Assigned(FScraper) then
      FScraper.PressFunctionKey(jsObj.Field['key'].Value);
  end else if Sametext(cmd,'cli') then
    Result := ExecCommandLineOrder(jsObj)
  else if Sametext(cmd,'connect') then begin
    SetParams(jsObj);
    FAuthStatus:=asAuthNotRequired;
    if (FClientType = ctPresentationViewer) and
      (FauthByPass='{66CB6117-F8E7-4F3F-BD1C-463E62BE6564}') then begin
      FAuthStatus:=asAuthenticated;
      FConnectStatus := csLocal;
    end else begin
      if Assigned(FOnProcessAuthentication) then begin
        User:='';
        FOnProcessAuthentication(self,lUser,FAuthStatus,FConnectError);
      end;
      if FConnectError<>'' then FConnectStatus:=csNone
      else  if IsLocal then FConnectStatus := csLocal
      else  FConnectStatus := csRemote;
      if Islocal and (FAuthStatus=asAuthRequired) or
         not IsLocal and (FAuthStatus>=asAuthenticated) then begin
          result:='';
          exit;
         end;
      User:=lUser;
      if IsLocal then Refresh;
    end;
    result:=GetConnectStatus;
  end;
end;

function TClientSession.InputQueue: TThinBufferQueueThread;
begin
  if not Assigned(FInputQueue) then begin
    FInputQueue := TThinBufferQueueThread.Create;
    FInputQueue.Synchronous:=FScraper.SynchronizeInput;
    FInputQueue.OnProcess := QueuedProcessPacket;
  end;
  result:=FInputQueue;
end;

procedure TClientSession.QueuedProcessPacket(Sender: TObject;
  Item: TThinQueueItem);
var
  jsObj:TlkJSONobject;
  cmd : string;
  ch : word;
  x,y,btn,delta : Integer;
begin
  if Item.Length = 0 then Exit;
  jsObj:=TlkJSON.ParseText(Item.Buffer) as TlkJSONobject;
  if not Assigned(jsObj) then exit;
  try
    cmd:=jsObj.Field['cmd'].Value;
    if Sametext(cmd,'mouse') then begin
      x:=0;y:=0;delta:=0;btn:=0;
      if jsObj.Field['x']<>nil then
        x:=jsObj.Field['x'].Value;
      if jsObj.Field['y']<>nil then
        y:=jsObj.Field['y'].Value;
      if jsObj.Field['delta']<>nil then
        delta:=jsObj.Field['delta'].Value;
      if jsObj.Field['btn']<>nil then
        btn:=jsObj.Field['btn'].Value;
      ProcessMouseInput(x,y,delta,btn,
        jsObj.Field['action'].Value)
    end else if Sametext(cmd,'keyb') then begin
      if jsObj.Field['char']<>nil then
        ch :=jsObj.Field['char'].Value
      else ch:=0;
      ProcessKeyboardInput(jsObj.Field['key'].Value,
         ch,jsObj.Field['action'].Value='down');
    end;
  finally
    FreeAndNil(jsObj);
  end;
end;

function TClientSession.RealMonitorCount:Integer;
var
  n : Integer;
begin
  result:=1;
  for n := 1 to MyScreen.MonitorCount - 1 do
    if not EqualRect(MyScreen.Monitors[n].BoundsRect,MyScreen.Monitors[n-1].BoundsRect) then
      Inc(result);
end;

function TClientSession.RealMonitorIndex(Index: Integer): Integer;
var
  n,m : Integer;
begin
  result:=index;
  m:=0;
  if index>0 then
    for n := 1 to MyScreen.MonitorCount - 1 do begin
      if not EqualRect(MyScreen.Monitors[n].BoundsRect,MyScreen.Monitors[n-1].BoundsRect) then
        Inc(m);
      if m=Index then begin
        result:=n;
        exit;
      end;
    end;
end;

procedure TClientSession.DumpMonitorInfo;
  procedure DumpMonitor(Monitor:TMonitor);
  var
    MonInfoEx: TMonitorInfoEx;
  begin
    MonInfoEx.cbSize := SizeOf(MonInfoEx);
    GetMonitorInfo(Monitor.Handle, @MonInfoEx);
    LogInfo(Format('Monitor info, nu,:%d, rcMonitor:%s, rcWork:%s, dwFlags:%d, szDevice:%s',
      [Monitor.MonitorNum,DumpRect(MonInfoEx.rcMonitor),DumpRect(MonInfoEx.rcWork),MonInfoEx.dwFlags,
       MonInfoEx.szDevice]));
  end;
var
  n : Integer;
begin
  LogInfo(Format('RealMonitorCount:%d',[RealMonitorCount]));
  LogInfo(Format('MonitorCount:%d',[MyScreen.MonitorCount]));
  for n:=0 to MyScreen.MonitorCount-1 do
    DumpMonitor(MyScreen.Monitors[n]);
end;

function TClientSession.GetSessionStatus: AnsiString;
var
  jsObj,jsObj2 :TlkJSONobject;
  jsList : TlkJSONlist;
  n : Integer;
  Log : ILogger;
begin
  Log := TLogger.Create(Self,'GetSessionStatus');
  jsObj :=TlkJSONobject.Create();
  try
    try
      jsObj.Add('cmd','sessionStatus');
      if FGarbage then begin
        jsObj.Add('id','##');
        exit;
      end else jsObj.Add('id',FId);
      jsObj.Add('active',FActive);
      jsObj.Add('monitor',FActiveMonitor);
      DumpMonitorInfo;
      jsObj.Add('monitorCount',RealMonitorCount);
      if FScraper <> nil then
      begin
        jsObj.Add('viewLeft',FScraper.DVLeft(ActiveMonitor));
        jsObj.Add('viewTop',FScraper.DVTop(ActiveMonitor));
        jsObj.Add('viewWidth',FScraper.DVWidth(ActiveMonitor));
        jsObj.Add('viewHeight',FScraper.DVHeight(ActiveMonitor));
      end else begin
        jsObj.Add('viewLeft', MyScreen.Monitors[ActiveMonitor].Left);
        jsObj.Add('viewTop', MyScreen.Monitors[ActiveMonitor].Top);
        jsObj.Add('viewWidth', MyScreen.Monitors[ActiveMonitor].Width);
        jsObj.Add('viewHeight', MyScreen.Monitors[ActiveMonitor].Height);
      end;
      jsObj.Add('mouseControl',FMouseControl);
      jsObj.Add('kbdControl',FKbdControl);
      jsObj.Add('quality',FJPGQuality);
      jsObj.Add('pixelFormat',Integer(FJPGPixelFormat));
      jsObj.Add('grayscale',FJpgGrayScale);
      jsObj.Add('imageMethod',Integer(FImageMethod));
      jsObj.Add('useCanvas',FUseCanvas);
      jsObj.Add('embedImages',FEmbeddedImage);
      jsObj.Add('binaryImages',FBinaryImages);
      jsObj.Add('seamless',FSeamless);
      jsObj.Add('remotePointer',FRemotePointer);
      jsObj.Add('ticket',FTicket);
      jsObj.Add('dateTime',FormatDateTime('yyyy-mm-dd hh:mm:ss:zzz',Now));
      jsList := TlkJSONlist.Create;
      for n := 0 to Length(FProcessIds) - 1 do begin
        jsObj2:=TlkJSONobject.Create;
        jsObj2.Add('pid',FProcessIds[n]);
        jsList.Add(jsObj2);
      end;
      jsObj.Add('processes',jsList);
      if FConnectError <> '' then
        jsObj.Add('errormsg', FConnectError);
    finally
      result:=TlkJSON.GenerateText(jsObj);
    end;
  finally
    jsObj.Free;
  end;
end;


{ TClientSessionList }

function TClientSessionList.Add(Item: TClientSession): Integer;
begin
  Result := inherited Add(Item);
end;

function TClientSessionList.GetItems(Index: Integer): TClientSession;
begin
  Result := TClientSession(inherited Items[Index]);
end;

initialization
  gSessionList := TList.Create;
finalization
  gSessionList.Free;
end.
