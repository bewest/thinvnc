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
uses Windows,Classes,Sysutils,Graphics,Forms,jpeg,uLkJSON, Contnrs,
  SyncObjs,uAdvHttp,StrUtils,
  ThinVnc.Log,
  ThinVnc.Utils,
  ThinVnc.Cache,
  ThinVnc.SessionWindow,
  ThinVnc.Cursor,
  ThinVnc.InputQueue,
  ThinVnc.MirrorWindow;

type
  TCallbackSendEvent = procedure (Text:AnsiString;APacketNumber:Cardinal) of object;
  TCallbackTextEvent = procedure (Text:AnsiString) of object;
  TClientSession = class;
  TConnectStatus = (csNone,csLocal,csRemote);

  TClientSession = class
  private
    FMouseControl : Boolean;
    FKbdControl   : Boolean;
    FProcessIds: array of Integer;
    FUseJpeg: Boolean;
    FEmbeddedImage: Boolean;
    FSeamless : Boolean;
    FUseCanvas: Boolean;
    FJpgQuality: Integer;
    FJpgPixelFormat: TJPEGPixelFormat;
    FJpgGrayscale: boolean;
    FForceRefresh : Boolean;
    FMirrorManager : TMirrorManager;
    FSessionWindowList : TSessionWindowList;
    FActive: Boolean;
    FOnSendCmd: TCallbackSendEvent;
    FOnSendBuf: TCallbackSendEvent;
    FLastUsed : TDateTime;
    FPolled : Boolean;
    FEnableEvent : TEvent;
    FOnForceCapture: TNotifyevent;
    FBinaryImages: Boolean;
    FRemotePointer : Boolean;
    FResetRemotePointer : Boolean;
    FOldSignature : string;
    FKeyDownList : TList;
    FLastKeytime : TDatetime;
    FKeyCs : TCriticalSection;
    FInputQueue  : TThinBufferQueueThread;
    FActiveMonitor : Integer;

    FCursorWindow : TCursorWindow;
    FDestAddr: string;
    FOnSendScreen: TCallbackSendEvent;
    FIsLocal: Boolean;
    FConnectStatus: TConnectStatus;
    FConnectError: string;
    FGarbage : Boolean;
    FAuthenticationData: AnsiString;
    FNeedRemoteAuthentication: Boolean;
    FUser: string;
    FTicket: string;
    FForceDisconnect: boolean;
    procedure SetJpgGrayscale(const Value: boolean);
    procedure SetJpgPixelFormat(const Value: TJPEGPixelFormat);
    procedure SetJpgQuality(const Value: Integer);
    function  IsProcessInList(APid: Integer): Boolean;
    function  RefreshWindows:boolean;
    procedure Refresh;
    procedure OnEmptyQueue(Sender: TObject);
    procedure ProcessKeyboardInput(key: word; Down: boolean);
    procedure SetActive(const Value: Boolean);
    function KeysDown(vks: array of integer): boolean;
    function KeysUp(vks: array of integer): boolean;
    function PressFunctionKey(Func: string): Boolean;
    procedure AddKeyDownToList(key:word);
    procedure QueuedProcessPacket(Sender: TObject; Item: TThinQueueItem);
    procedure SetDestAddr(const Value: string);
    function GetActiveMonitor: Integer;
  public
    constructor Create(AMirrorManager:TMirrorManager;APolled:Boolean=false);
    destructor  Destroy;override;
    function    InputQueue:TThinBufferQueueThread;
    procedure   Update;
    function    IsGarbage:Boolean;
    procedure   Enable;
    procedure   FlushKeyboard(force: Boolean=false);
    procedure   ProcessMouseInput(X,Y,Btn:Integer;action:string);
    function    GetDifferences:boolean;
    function    ProcessDifferences:boolean;
    function    GetJson(path:string=''):string;
    function    ProcessPacket(JsonText: Ansistring):AnsiString;
    function    GetSessionStatus:AnsiString;
    function    GetConnectStatus: AnsiString;
    procedure   SendScreen(AText: AnsiString);
    property    DestAddr : string read FDestAddr write SetDestAddr;
    property    IsLocal: Boolean read FIsLocal;
    property    Active: Boolean read FActive write SetActive;
    property    OnSendScreen : TCallbackSendEvent read FOnSendScreen write FOnSendScreen;
    property    OnSendCmd : TCallbackSendEvent read FOnSendCmd write FOnSendCmd;
    property    OnSendBuf : TCallbackSendEvent read FOnSendBuf write FOnSendBuf;
    property    UseJpeg:Boolean read FUseJpeg write FUseJpeg;
    property    EmbeddedImage:Boolean read FEmbeddedImage write FEmbeddedImage;
    property    BinaryImages:Boolean read FBinaryImages write FBinaryImages;
    property    UseCanvas:Boolean read FUseCanvas write FUseCanvas;
    property    JpgGrayscale:boolean read FJpgGrayscale write SetJpgGrayscale;
    property    JpgQuality:Integer read FJpgQuality write SetJpgQuality;
    property    JpgPixelFormat:TJPEGPixelFormat read FJpgPixelFormat write SetJpgPixelFormat;
    property    EnableEvent: TEvent read FEnableEvent;
    property    OnForceCapture:TNotifyevent read FOnForceCapture write FOnForceCapture;
    property    ConnectStatus: TConnectStatus read FConnectStatus write FConnectStatus;
    property    ConnectError: string read FConnectError;
    property    NeedRemoteAuthentication: Boolean read FNeedRemoteAuthentication;
    property    AuthenticationData: AnsiString read FAuthenticationData;
    property    ActiveMonitor:Integer read GetActiveMonitor;
    property    Ticket:string read FTicket write FTicket;    
    property    User:string read FUser write FUser;
    property    ForceDisconnect:boolean read FForceDisconnect write FForceDisconnect;
  end;

  TClientSessionList = class(TList)
  private
    function GetItems(Index: Integer): TClientSession;
  public
    function Add(Item: TClientSession): Integer;
    property Items[Index: Integer]: TClientSession read GetItems; default;
  end;

function FindHeader(List: TStrings; Header: string; var Value: string): Boolean;
  
implementation

uses
  ThinVnc.Capture;

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

{ TClientSession }

constructor TClientSession.Create(AMirrorManager: TMirrorManager;APolled:Boolean);
var
  Log : ILogger;
  n : Integer;
begin
  Log := TLogger.Create(self,'Create');
  FMirrorManager := AMirrorManager;
  FSessionWindowList := TSessionWindowList.Create;
  FUseJpeg := true;
  FEmbeddedImage := true;
  FBinaryImages := false;
  FJpgQuality:=85;
  FJpgPixelFormat:=jf24Bit;
  FPolled:=APolled;
  FEnableEvent := TEvent.Create(nil, true,false,'');
  FCursorWindow := TCursorWindow.Create;
  FKeyDownList := TList.Create;
  FKeyCs := TCriticalSection.Create;
  FIsLocal := True;

  FActiveMonitor := 0;
  for N := 0 to Screen.MonitorCount - 1 do
    if Screen.Monitors[n].Primary then begin
      FActiveMonitor:=N;
      break;
    end;

  Update;
  gCaptureThread.AddSession(Self);
end;

destructor TClientSession.Destroy;
var
  Log : ILogger;
begin
  Log:=TLogger.Create(Self,Format('Destroy: %d',[Integer(self)]));
  if assigned(gCaptureThread) then
    gCaptureThread.RemoveSession(Self);
  FlushKeyboard(true);
  Active := False;
  OnSendScreen := nil;
  OnSendCmd := nil;
  OnSendBuf := nil;
  if assigned(FInputQueue) then
    FInputQueue.Terminate;
  FreeAndNil(FCursorWindow);
  FreeAndNil(FKeyDownList);
  FreeAndNil(FKeyCs);
  FreeAndNil(FSessionWindowList);
  FreeAndNil(FEnableEvent);
  inherited;
end;

procedure TClientSession.FlushKeyboard(force:Boolean);
begin
  FKeyCs.Enter;
  try
    if (FKeyDownList.Count>0) and (Force or (DatetimeToTimestamp(Now-FLastKeytime).time>2000))  then
      while FKeyDownList.Count>0 do
        ProcessKeyboardInput(Word(FKeyDownList[FKeyDownList.Count-1]),false);
  finally
    FKeyCs.Leave;
  end;
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
      result:=result+Format('h:%d-%d-%d-%d-%d;',[sw.Handle,sw.BoundsRect.Left,sw.BoundsRect.Right,
        sw.BoundsRect.Right,sw.BoundsRect.Bottom]);
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

  for n := FSessionWindowList.Count - 1 downto 0 do begin
    found:=false;
    for m := 0 to FMirrorManager.MirrorList.Count - 1 do begin
      wm := FMirrorManager.MirrorList[m];
      if wm.Handle=FSessionWindowList[n].Handle then begin
        Found:=true;
        break;
      end;
    end;
    if not found then FSessionWindowList.Delete(n)
    else FSessionWindowList[n].UpdateBitmap;
  end;

  for n := 0 to FMirrorManager.MirrorList.Count - 1 do begin
    wm := FMirrorManager.MirrorList[n];
    found:=false;
    for m := 0 to FSessionWindowList.Count - 1 do begin
      if wm.Handle=FSessionWindowList[m].Handle then begin
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
var
  Log:ILogger;
begin
  Log:=TLogger.Create(self,'ProcessDifferences',true);
  result:=False;
  if FEnableEvent.Waitfor(0)=wrSignaled then begin
    result:=GetDifferences;
    if result then begin
      FEnableEvent.ResetEvent;
      if not Assigned(OnSendScreen) then exit;
      SendScreen(GetJson);
    end;
  end else LogInfo('ProcessDifferences FEnableEvent not enabled');
end;

function TClientSession.GetDifferences:Boolean;
var
  sw : TSessionWindow;
  m : Integer;
  rc : Boolean;
  Log:ILogger;
begin
  Log:=TLogger.Create(self,'GetDifferences',true);
  result:=RefreshWindows;

  for m := FSessionWindowList.Count - 1 downto 0 do begin
    sw:=TSessionWindow(FSessionWindowList[m]);
    if not sw.Active then continue;
    if (Length(FProcessIds)>0) and not IsProcessInList(sw.ProcessId) then continue;
    rc:=sw.CaptureDifferences(FActiveMonitor);
    result:=result or rc;
  end;

  rc:=FCursorWindow.Capture(FResetRemotePointer);
  FResetRemotePointer:=false;
  if FRemotePointer or FCursorWindow.ShapeChanged then begin
    result:=result or rc or FCursorWindow.ShapeChanged;
  end;
end;

function TClientSession.GetJson(path: string): string;
var n,curX,curY : Integer;
  json,cur : string;
  Log : Ilogger;
begin
  Log := TLogger.Create(Self,'GetJson');
  json:='';
  for n := 0 to FSessionWindowList.Count - 1 do begin
    if FSeamless and FSessionWindowList[n].IsShellWindow then continue;
    if not IsProcessInList(FSessionWindowList[n].ProcessId) then continue;
    if json<>'' then json:=json+','+#13#10;
    json:=json+#9+FSessionWindowList[n].getJson(path,EmbeddedImage,FBinaryImages,UseJpeg,JpgQuality,JpgPixelFormat,JpgGrayScale);
  end;
  if FRemotePointer then
    json:=json+','+#13#10+FCursorWindow.getJson;
  FCursorWindow.GetCursor(cur,curX,curY);
  result:=Format('{"windows": ['+#13#10+json+#13#10+'],"status:":1,"cursor":"%s","cursorX":%d,"cursorY":%d}',
        [cur,curX,curY]);
end;

function TClientSession.GetActiveMonitor: Integer;
begin
  result:=FActiveMonitor;
end;

function TClientSession.GetConnectStatus: AnsiString;
var
  jsObj: TlkJSONobject;
begin
  jsObj :=TlkJSONobject.Create();
  try
    jsObj.Add('cmd', 'connectStatus');
    jsObj.Add('id', Integer(self));
    jsObj.Add('status', FConnectStatus<>csNone);
    if FConnectError <> '' then
      jsObj.Add('errormsg', FConnectError);
    jsObj.Add('dateTime',FormatDateTime('yyyy-mm-dd hh:mm:ss:zzz',Now));
    result:=TlkJSON.GenerateText(jsObj);
  finally
    jsObj.Free;
  end;
end;


procedure TClientSession.Refresh;
begin
  FForceRefresh:=true;
end;


procedure TClientSession.Enable;
var
  Log:ILogger;
begin
  Log:=TLogger.Create(self,'Enable');
  FEnableEvent.SetEvent;
end;


procedure TClientSession.SendScreen(AText: AnsiString);
var
  Log : ILogger;
begin
  Log:=TLogger.Create(Self,'SendScreen');
  try
      Log.LogInfo('Assigned(OnSendScreen)='+BoolToStr(Assigned(OnSendScreen),true));
      if Assigned(OnSendScreen) then
      try
        OnSendScreen(AText,1);
      except
        OnSendScreen:=nil;
      end;
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
begin
  FActive := Value;
  if FActive then
    gCaptureThread.Resume;
end;

procedure TClientSession.SetDestAddr(const Value: string);
begin
  FDestAddr := '';
  FIsLocal := true;
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

procedure TClientSession.Update;
begin
  FLastUsed:=Now;
end;

function TClientSession.PressFunctionKey(Func:string):Boolean;
var Log :ILogger;
begin
  if func='CtrlAltDel' then begin
    FlushKeyboard(true);
      KeysDown([VK_CONTROL,VK_SHIFT,VK_ESCAPE]);
      Sleep(50);
      KeysUp([VK_ESCAPE,VK_SHIFT,VK_CONTROL]);
  end else if func='AltTab' then begin
    KeysDown([VK_MENU,VK_TAB]);
    KeysUp([VK_TAB]);
  end else if func='AltShiftTab' then begin
    KeysDown([VK_MENU,VK_SHIFT,VK_TAB]);
    KeysUp([VK_SHIFT,VK_TAB]);
  end else if func='ShiftCtrlEsc' then begin
    FlushKeyboard(true);
    KeysDown([VK_CONTROL,VK_SHIFT,VK_ESCAPE]);
    KeysUp([VK_ESCAPE]);
  end else if func='CtrlEsc' then begin
    FlushKeyboard(true);
    KeysDown([VK_CONTROL,VK_ESCAPE]);
    KeysUp([VK_ESCAPE,VK_CONTROL]);
  end else if func='AltEsc' then begin
    FlushKeyboard(true);
    KeysDown([VK_MENU,VK_ESCAPE]);
    KeysUp([VK_ESCAPE]);
  end else if func='LeftWin' then begin
    FlushKeyboard(true);
    KeysDown([VK_LWIN]);
    KeysUp([VK_LWIN]);
  end else if func='RightWin' then begin
    FlushKeyboard(true);
    KeysDown([VK_RWIN]);
    KeysUp([VK_RWIN]);
  end else if func='PrtScr' then begin
    FlushKeyboard(true);
    KeysDown([VK_SNAPSHOT]);
    KeysUp([VK_SNAPSHOT]);
  end else if func='AltPrtScr' then begin
    FlushKeyboard(true);
    KeysDown([VK_MENU]);
    KeysDown([VK_SNAPSHOT]);
    KeysUp([VK_SNAPSHOT,VK_MENU]);
  end;
  result:=true;
end;

function TClientSession.KeysDown(vks:array of integer):boolean;
var
  n,sc : integer;
begin
  for n := 0 to Length(vks) - 1 do begin
    sc:=MapVirtualKey(vks[n],0);
    keybd_event(vks[n],sc, 0,0);
    AddKeyDownToList(vks[n]);
  end;
  result:=true;
end;

function TClientSession.KeysUp(vks:array of integer):boolean;
var
  n,sc : integer;
begin
  for n := 0 to Length(vks) - 1 do begin
    sc:=MapVirtualKey(vks[n],0);
    keybd_event(vks[n],sc, KEYEVENTF_KEYUP,0);
    FKeyDownList.Remove(TObject(vks[n]));
  end;
  result:=true;
end;

procedure TClientSession.ProcessKeyboardInput(key: word; Down:boolean);
  procedure DumpKeys;
  var n : Integer;
    S : string;
  begin
    S:='';
    for n := 0 to FKeyDownList.Count - 1 do begin
      if S<>'' then S:=S+',';
      S:=S+IntToStr(Integer(FKeyDownList[n]));
    end;
    LogInfo(Format('Keys Pressed: [%s]',[S]));
  end;
  
  function AreVkInList(vks:array of Integer):Boolean;
  var n,m : Integer;
    found : Boolean;
  begin
    result:=false;
    if length(vks)>FKeyDownList.Count then exit;
    
    found:=false;
    for n := 0 to FKeyDownList.Count - 1 do begin
      found:=false;
      for m := 0 to Length(vks) - 1 do
        if vks[m]=Integer(FKeyDownList[n]) then begin
          found:=true;
          break;
        end;
      if not found then break;
    end;
    result:=found;
  end;

  procedure RemoveKeys(vks:array of Integer);
  var n,m : Integer;
  begin
    for n :=FKeyDownList.Count - 1 downto 0 do begin
      for m := 0 to Length(vks) - 1 do
        if Integer(FKeyDownList[n]) = vks[m] then begin
          FKeyDownList.delete(n);
          break;
        end;
    end;
  end;
  
  function ValidCombination:Boolean;
    function GetVk(C:Char):word;
    begin
      result:=VkKeyScanEx(C,0);
    end;
  begin
    result:=false;
    DumpKeys;
    if AreVkInList([VK_MENU,VK_PRIOR]) then begin
      RemoveKeys([VK_PRIOR]);
      result:=PressFunctionKey('AltTab');
    end else if AreVkInList([VK_MENU,VK_NEXT]) then begin
      RemoveKeys([VK_NEXT]);
      result:=PressFunctionKey('AltShiftTab');
    end else if AreVkInList([VK_CONTROL,VK_MENU,VK_INSERT]) then begin
      RemoveKeys([VK_CONTROL,VK_MENU,VK_INSERT]);
      result:=PressFunctionKey('AltEsc');
    end else if AreVkInList([VK_CONTROL,VK_MENU,VK_HOME]) then begin
      RemoveKeys([VK_CONTROL,VK_MENU,VK_HOME]);
      result:=PressFunctionKey('CtrlEsc');
    end else if AreVkInList([VK_CONTROL,VK_MENU,VK_END]) then begin
      RemoveKeys([VK_CONTROL,VK_MENU,VK_END]);
      result:=PressFunctionKey('CtrlAltDel');
    end else if AreVkInList([VK_CONTROL,GetVk('C')]) then
    else if AreVkInList([VK_CONTROL,GetVk('X')]) then
    else if AreVkInList([VK_CONTROL,GetVk('V')]) then
    else if AreVkInList([VK_CONTROL,GetVk('Z')]) then;
  end;
  function IsExtendedVK(vk: Integer): Boolean;
  begin
    Result := (vk = VK_LEFT)
           or (vk = VK_RIGHT)
           or (vk = VK_UP)
           or (vk = VK_DOWN)
           or (vk = VK_SCROLL)
           or (vk = VK_CAPITAL)
           or (vk = VK_NUMLOCK)
           or (vk = VK_HOME)
           or (vk = VK_END)
           or (vk = VK_PRIOR)
           or (vk = VK_NEXT)
           or (vk = VK_INSERT)
           or (vk = VK_DELETE)
  end;
var
  sc, flags: Integer;
begin
  SwitchToActiveDesktop;
  FKeyCs.Enter;
  try
    FLastKeytime := Now;
    LogInfo(Format('ProcessKeyboardInput key:%d down:%s',[key,BoolToStr(down,true)]));
    if down then begin
      AddKeyDownToList(key);
      if ValidCombination then exit;
    end else FKeyDownList.Remove(TObject(key));
  finally
    FKeyCs.Leave;
  end;
  sc:=MapVirtualKey(key,0);
  if down then flags := 0
  else flags := KEYEVENTF_KEYUP;
  if IsExtendedVK(key) then flags := flags or KEYEVENTF_EXTENDEDKEY;
  keybd_event(key, sc, flags, 0);
end;

procedure TClientSession.AddKeyDownToList(key:word);
begin
  if FKeyDownList.IndexOf(TObject(key))=-1 then
    FKeyDownList.Add(TObject(key));
end;

procedure TClientSession.ProcessMouseInput(X, Y, Btn: Integer; action: string);
var
  flags : DWORD;
  Log : ILogger;
begin
  if not FMouseControl then exit;
  SwitchToActiveDesktop;

  if action<>'move' then
    Log := TLogger.Create(Self,'ProcessMouseInput action='+action);

  X:=round(X/(Screen.width)*65535);
  Y:=round(Y/(Screen.Height)*65535);
  flags:=0;
  if action='move' then begin
    mouse_event(MOUSEEVENTF_MOVE or MOUSEEVENTF_ABSOLUTE,X,Y,0,0);
    exit;
  end;

  if action='up' then begin
    case btn of
      0 : flags:=MOUSEEVENTF_LEFTUP;
      1 : flags:=MOUSEEVENTF_MIDDLEUP;
      2 : flags:=MOUSEEVENTF_RIGHTUP;
    end;
  end else if action='down' then begin
    case btn of
      0 : flags:=MOUSEEVENTF_LEFTDOWN;
      1 : flags:=MOUSEEVENTF_MIDDLEDOWN;
      2 : flags:=MOUSEEVENTF_RIGHTDOWN;
    end;
  end;
  flags:=flags or MOUSEEVENTF_MOVE or MOUSEEVENTF_ABSOLUTE;
  mouse_event(flags,X,Y,0,0);
end;

function TClientSession.ProcessPacket(JsonText: Ansistring):AnsiString;
  procedure SetParams(jsObj:TlkJSONobject);
  begin
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
    if jsObj.IndexOfName('useJpeg')<>-1 then
      FUseJpeg:=jsObj.Field['useJpeg'].Value;
    if jsObj.IndexOfName('embeddedImage')<>-1 then
      FEmbeddedImage:=jsObj.Field['embeddedImage'].Value;
    if jsObj.IndexOfName('remotePointer')<>-1 then begin
      FRemotePointer:=jsObj.Field['remotePointer'].Value;
      FResetRemotePointer:=FRemotePointer;
    end;
    if jsObj.IndexOfName('monitor')<>-1 then begin
      FActiveMonitor:=jsObj.Field['monitor'].Value;
      Refresh;
    end;
    if jsObj.IndexOfName('destAddr')<>-1 then
      SetDestAddr(jsObj.Field['destAddr'].Value);
    if jsObj.IndexOfName('ticket')<>-1 then begin
      FTicket:=jsObj.Field['ticket'].Value;
      if (FTicket<>'') then FRemotePointer:=true;
    end;
  end;
var
  jsObj:TlkJSONobject;
  cmd : string;
  Log : ILogger;
  Headers: TStringList;
  Code: Integer;
  Body: AnsiString;
begin
  Log := TLogger.Create(Self,'ProcessPacket');
  result:='';
  jsObj:=TlkJSON.ParseText(JsonText) as TlkJSONobject;
  if not Assigned(jsObj) then exit;
  try
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
      FlushKeyboard(true);
      result:=GetSessionStatus;
    end else if Sametext(cmd,'disconnect') then begin
      FActive:=False;
      FGarbage:=True;
      result:=GetSessionStatus;
    end else if Sametext(cmd,'refresh') then
      Refresh
    else if Sametext(cmd,'queryStatus') then
      result:=GetSessionStatus
    else if Sametext(cmd,'params') then begin
      SetParams(jsObj);
      result:=GetSessionStatus;
    end else if Sametext(cmd,'mouse') then
      InputQueue.AddBuffer(JsonText)
    else if Sametext(cmd,'keyb') then
      InputQueue.AddBuffer(JsonText)
    else if Sametext(cmd,'connect') then begin
      SetParams(jsObj);
        FConnectStatus := csLocal;
        FConnectError := '';
        Refresh;
        Active:=true;
      result:=GetConnectStatus;
    end;
  finally
    FreeAndNil(jsObj);
  end;
end;

function TClientSession.InputQueue: TThinBufferQueueThread;
begin
  if not Assigned(FInputQueue) then begin
    FInputQueue := TThinBufferQueueThread.Create;
    FInputQueue.OnProcess := QueuedProcessPacket;
  end;
  result:=FInputQueue;
end;

procedure TClientSession.QueuedProcessPacket(Sender: TObject;
  Item: TThinQueueItem);
var
  jsObj:TlkJSONobject;
  cmd : string;
begin
  if Item.Length = 0 then Exit;
  jsObj:=TlkJSON.ParseText(Item.Buffer) as TlkJSONobject;
  if not Assigned(jsObj) then exit;
  try
    cmd:=jsObj.Field['cmd'].Value;
    if Sametext(cmd,'mouse') then
      ProcessMouseInput(jsObj.Field['x'].Value,
        jsObj.Field['y'].Value,
        jsObj.Field['btn'].Value,
        jsObj.Field['action'].Value)
    else if Sametext(cmd,'keyb') then
      ProcessKeyboardInput(jsObj.Field['key'].Value,
         jsObj.Field['action'].Value='down');
  finally
    FreeAndNil(jsObj);
  end;
end;

function TClientSession.GetSessionStatus: AnsiString;
var
  jsObj,jsObj2 :TlkJSONobject;
  jsList : TlkJSONlist;
  n : Integer;
begin
  jsObj :=TlkJSONobject.Create();
  try
    jsObj.Add('cmd','sessionStatus');
    if FGarbage then
      jsObj.Add('id',-1)
    else jsObj.Add('id',Integer(self));
    jsObj.Add('active',FActive);
    jsObj.Add('monitor',FActiveMonitor);
    jsObj.Add('monitorCount',Screen.MonitorCount);
    jsObj.Add('viewLeft',DVLeft(ActiveMonitor));
    jsObj.Add('viewTop',DVTop(ActiveMonitor));
    jsObj.Add('viewWidth',DVWidth(ActiveMonitor));
    jsObj.Add('viewHeight',DVHeight(ActiveMonitor));
    jsObj.Add('mouseControl',FMouseControl);
    jsObj.Add('kbdControl',FKbdControl);
    jsObj.Add('quality',FJPGQuality);
    jsObj.Add('pixelFormat',Integer(FJPGPixelFormat));
    jsObj.Add('grayscale',FJpgGrayScale);
    jsObj.Add('useJpeg',FUseJpeg);
    jsObj.Add('useCanvas',FUseCanvas);
    jsObj.Add('embedImages',FEmbeddedImage);
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
    result:=TlkJSON.GenerateText(jsObj);
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

end.
