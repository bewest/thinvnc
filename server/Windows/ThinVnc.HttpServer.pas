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

unit ThinVnc.HttpServer;
{$I ThinVnc.DelphiVersion.inc}
{$I ThinVnc.inc}
interface

uses
  SysUtils, Classes, Forms,StrUtils,Inifiles,jpeg,SyncObjs,
  OverbyteIcsWinSock,  OverbyteIcsWSocket, OverbyteIcsWndControl,
  OverbyteIcsHttpSrv, OverbyteIcsUtils,OverbyteIcsWSocketS,
  uLkJSON,
  ThinVnc.Log,
  ThinVnc.Utils,ThinVnc.Cache,
  ThinVnc.MirrorWindow,
  ThinVnc.ClientSession,ThinVNC.capture;

const
  NO_CACHE  = 'Cache-Control: no-cache' + #13#10;


type
  TMyHttpConnection = class(THttpConnection)
  private
    FSession  : TClientSession;
    FEvent    : TEvent;
    FJsonText : AnsiString;
    FCs       : TCriticalSection;
    function RequiresAuthentication: Boolean;
    function QueryStringToJson: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AnswerPage(var   Flags    : THttpGetFlag;
                               const Status   : String;
                               const Header   : String;
                               const HtmlFile : String;
                               UserData       : TObject;
                               Tags           : array of const); override;
    function    ResolveSession(CreateIfDoesntExist:boolean=True):Boolean;
    procedure   SendScreen(Text:AnsiString;APacketNumber:Cardinal);
    function    WaitForJson(out output:AnsiString):boolean;
    function    IsConnectStatus:Boolean;
    function    ProcessPacket(JsonText:AnsiString):AnsiString;
  end;


  THttpServer = class
  private
    FIcsHttpServer: OverbyteIcsHttpSrv.THttpServer;
    FPassword: string;
    FUser: string;
    FOnStopped: TNotifyEvent;
    FOnStarted: TNotifyEvent;
    FAuthType: TAuthenticationType;
    FHttpsEnabled: Boolean;
    FHttpEnabled: Boolean;
    FOnAuthGetPassword: TAuthGetPasswordEvent;

    procedure ServerStarted(Sender: TObject);
    procedure ServerStopped(Sender: TObject);
    procedure AuthGetType(Sender, Client: TObject{;
      var AuthenticationType: TAuthenticationType});
    procedure AuthGetPassword(Sender, Client: TObject;
      var Password: String);
    procedure GetDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure AuthNtlmBeforeValidate(Sender, Client: TObject;
      var Allow: Boolean);
    procedure CreateVirtualDocument_Json(Sender: TObject;
      ClientCnx: TMyHttpConnection; var Flags: THttpGetFlag);
    procedure CreateVirtualDocument_Root(Sender: TObject;
      ClientCnx: TMyHttpConnection; LoginPage: Boolean; var Flags: THttpGetFlag);
    procedure ProcessCmd(Sender: TObject; ClientCnx: TMyHttpConnection;
      IsLocalCmd: Boolean; var Flags: THttpGetFlag);
    procedure ProcessImages(Sender: TObject; ClientCnx: TMyHttpConnection;
      var Flags: THttpGetFlag);
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetLastError: Integer;
  public
    constructor Create;
    destructor  Destroy;override;
    function    Start:boolean;
    procedure   Stop;
    property    LastError:Integer read GetLastError;
    property    User:string read FUser write FUser;
    property    Password:string read FPassword write FPassword;
    property    AuthType:TAuthenticationType read FAuthType write FAuthType;
    property    Port:Integer read GetPort write SetPort;
    property    HttpEnabled:Boolean read FHttpEnabled write FHttpEnabled;
    property    HttpsEnabled:Boolean read FHttpsEnabled write FHttpsEnabled;
    property    OnStarted:TNotifyEvent read FOnStarted write FOnStarted;
    property    OnStopped:TNotifyEvent read FOnStopped write FOnStopped;
    property    OnAuthGetPassword:TAuthGetPasswordEvent read FOnAuthGetPassword write FOnAuthGetPassword; 
  end;

implementation

{ THttpServer }

constructor THttpServer.Create;
begin
  FIcsHttpServer:=OverbyteIcsHttpSrv.THttpServer.Create(nil);
  with FIcsHttpServer do begin
    OnServerStarted:=ServerStarted;
    OnServerStopped:=ServerStopped;
    OnGetDocument  :=GetDocument;
    OnAuthGetPassword:=AuthGetPassword;
    OnAuthGetType:=AuthGetType;
    OnAuthNtlmBeforeValidate:=AuthNtlmBeforeValidate;
    Options:=[hoContentEncoding];
    SizeCompressMin:=100;
    KeepAliveTimeSec:=60;
    ClientClass := TMyHttpConnection;
    DocDir:=GetModulePath+'web';
    TemplateDir:=GetModulePath+'web';
  end;

end;

destructor THttpServer.Destroy;
begin
  FIcsHttpServer.Free;
  inherited;
end;

function THttpServer.GetPort: Integer;
begin
  result:=StrToIntDef(FIcsHttpServer.Port,80);
end;

procedure THttpServer.SetPort(const Value: Integer);
begin
  FIcsHttpServer.Port:=IntToStr(Value);
end;

function THttpServer.Start:Boolean;
begin
  result:=false;
  if FHttpEnabled then
  begin
    LogInfo('Starting HTTP Service...');
    FIcsHttpServer.Start;
  end;
  result:=FHttpEnabled;
end;

procedure THttpServer.Stop;
begin
  if FHttpEnabled then
  begin
    LogInfo('Stopping HTTP Service...');
    FIcsHttpServer.Stop;
  end;
  gCaptureThread.FreeSessions;
end;


procedure THttpServer.GetDocument(Sender, Client: TObject;
  var Flags: THttpGetFlag);
var
  ClientCnx  : TMyHttpConnection;
  Log : ILogger;
begin
  Log := TLogger.Create(self,'GetDocument');
  if Flags = hg401 then
      Exit;

  ClientCnx := TMyHttpConnection(Client);

  Log.LogInfo('Path = '+ClientCnx.Path);
  if CompareText(ClientCnx.Path, '/') = 0 then begin
    CreateVirtualDocument_Root(Sender, ClientCnx, not ClientCnx.IsConnectStatus, Flags)
  end else if CompareText(ClientCnx.Path, '/json') = 0 then
    CreateVirtualDocument_Json(Sender, ClientCnx, Flags)
  else if CompareText(ClientCnx.Path, '/cmd') = 0 then
    ProcessCmd(Sender, ClientCnx, False, Flags)
  else if CompareText(ClientCnx.Path, '/lcmd') = 0 then
    ProcessCmd(Sender, ClientCnx, True, Flags)
  else if CompareText(ClientCnx.Path, '/img') = 0 then
    ProcessImages(Sender, ClientCnx, Flags)
  else if CompareText(ClientCnx.Path, '/rauth') = 0 then
    ClientCnx.AnswerString(Flags, '', '','','');
end;

function THttpServer.GetLastError: Integer;
begin
  result:=FIcsHttpServer.WSocketServer.LastError;
end;

procedure THttpServer.CreateVirtualDocument_Root(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    LoginPage : Boolean;
    var Flags : THttpGetFlag);
var
  QueryFields : TStringList;
  Page : string;
begin
  QueryFields := TStringList.Create;
  try
    QueryFields.Delimiter:='&';
    QueryFields.DelimitedText:=UrlDecode(ClientCnx.Params);
  finally
    QueryFields.Free;
  end;

  Page:='client.htm';
  if LoginPage then Page:='connect.htm';

  ClientCnx.AnswerPage(Flags,'',NO_CACHE,Page,nil,[]);
end;

procedure THttpServer.CreateVirtualDocument_Json(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
var
  id,dest,output : string;
  Log : ILogger;
begin
  Log := TLogger.Create(self,'CreateVirtualDocument_Json');
  Log.LogInfo(Format('Query: %s, Cookie: %s',[UrlDecode(ClientCnx.Params),ClientCnx.RequestCookies]));

  id:=IntToStr(Integer(ClientCnx.FSession));
  if ClientCnx.ResolveSession(false) then begin
    if ClientCnx.FSession.ForceDisconnect then
      output:='{"status":9}'
    else if not ClientCnx.WaitForJson(output) then
      output:='{"status":2}';
  end else begin
    id := '';
    output:='{"windows":[],"status":3}';
  end;

  Log.LogInfo(Copy(output,1,2048));
  if output<>'' then
    ClientCnx.AnswerString(Flags, '', 'application/x-javascript',
        NO_CACHE+MakeCookie('SID',id,0,'/'), output);
end;

procedure THttpServer.ProcessCmd(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    IsLocalCmd: Boolean;
    var Flags : THttpGetFlag);
var
  n : Integer;
  QueryFields : TStringList;
  output, SendStatus, SendHeader,Cmd: AnsiString;
  id : string;
  Log : ILogger;
  Handled: Boolean;
begin
  Log := TLogger.Create(self,'ProcessCmd');
  Log.LogInfo(Format('Query: %s, Cookie: %s',[UrlDecode(ClientCnx.Params),ClientCnx.RequestCookies]));

  ExtractURLEncodedValue(UrlDecode(ClientCnx.Params),'Cmd',Cmd);
  if not ClientCnx.ResolveSession(SameText(Cmd,'start') or SameText(Cmd,'connect')) then
    exit;


  output:=ClientCnx.ProcessPacket(ClientCnx.QueryStringToJson);

  if (ClientCnx.FSession.ConnectStatus=csNone) and ClientCnx.FSession.NeedRemoteAuthentication then
  begin
    Flags := hg401;
    SendStatus := '401 Access Denied';
    SendHeader := NO_CACHE + MakeCookie('SID',IntToStr(Integer(ClientCnx.FSession)),0,'/') + ClientCnx.FSession.AuthenticationData + #13#10;
    ClientCnx.AnswerString(Flags, SendStatus, 'application/x-javascript', SendHeader, ClientCnx.FSession.GetConnectStatus);
    Exit;
  end;

  if output<>'' then begin
    SendStatus := '';
    SendHeader := NO_CACHE + MakeCookie('SID',IntToStr(Integer(ClientCnx.FSession)),0,'/');
    ClientCnx.AnswerString(Flags, SendStatus,'application/x-javascript', SendHeader, output);
  end else ClientCnx.AnswerString(Flags,'','application/x-javascript','','{}');
end;


procedure THttpServer.ProcessImages(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
var
  id : string;
  ci : TCacheItem;
begin
  ExtractURLEncodedValue(UrlDecode(ClientCnx.Params),'id',id);

  if assigned(ClientCnx.DocStream) then
    ClientCnx.DocStream.Free;

  ci:=gCaptureThread.MirrorManager.Cache.extract(id);

  if assigned(ci) then
  try
    ClientCnx.DocStream:=TMemoryStream.Create;
    ClientCnx.DocStream.CopyFrom(ci.GetStream,ci.GetStream.Size);
    ClientCnx.AnswerStream(Flags,'','image/'+ci.ImageType,'');
  finally
    ci.Free;
  end;
end;

procedure THttpServer.AuthGetType(Sender, Client: TObject);
var
  ClientCnx  : TMyHttpConnection;
  Ticket : string;
begin
  ClientCnx := TMyHttpConnection(Client);
  if ClientCnx.RequiresAuthentication then begin
    ClientCnx.AuthTypes  := [AuthType];
    ClientCnx.AuthRealm := 'thinvnc';
  end;
end;

procedure THttpServer.AuthGetPassword(
    Sender       : TObject;
    Client       : TObject;
    var Password : String);
var
  ClientCnx  : TMyHttpConnection;
  Ticket : string;
begin
  ClientCnx := TMyHttpConnection(Client);
  if (ClientCnx.AuthTypes     = [atDigest]) and
     (ClientCnx.AuthUserName = FUser) then
      Password := FPassword
  else if (ClientCnx.AuthTypes     = [atBasic]) and
          (ClientCnx.AuthUserName = FUser) then
      Password := FPassword
  else if (ClientCnx.AuthTypes = [atNtlm]) then ;
  ClientCnx.FSession.User:=ClientCnx.AuthUserName;
end;

procedure THttpServer.AuthNtlmBeforeValidate(Sender, Client: TObject;
  var Allow: Boolean);
var
  ClientCnx  : TMyHttpConnection;
begin
  ClientCnx := TMyHttpConnection(Client);
  Allow := (ClientCnx.AuthNtlmSession.Username <> '') and
           (ClientCnx.AuthNtlmSession.Domain <> 'SomeDomain');
end;

procedure THttpServer.ServerStarted(Sender: TObject);
begin
  if Sender = FIcsHttpServer then
    LogInfo('HTTP Service was started') else
    LogInfo('HTTPS Service was started');
  if assigned(FOnStarted) then
    FOnStarted(Sender);
end;

procedure THttpServer.ServerStopped(Sender: TObject);
begin
  if Sender = FIcsHttpServer then
    LogInfo('HTTP Service was stopped') else
    LogInfo('HTTPS Service was stopped');
  if assigned(FOnStopped) then
    FOnStopped(Sender);
end;

{ TMyHttpConnection }

constructor TMyHttpConnection.Create(AOwner: TComponent);
begin
  inherited;
  ComponentOptions := [wsoNoReceiveLoop,wsoTcpNoDelay];
  KeepAliveTimeSec := 60;
  FCs := TCriticalSection.Create;
  FEvent := TEvent.Create(nil,true,false,'');
end;

destructor TMyHttpConnection.Destroy;
var
  Log : ILogger;
begin
  if (Self = nil) or (FCs = nil) then
  begin
    LogError('Object is already destroyed: ' + IntToStr(Integer(Self)));
    Exit;
  end;
  Log := TLogger.Create(self,'Destroy : '+IntToStr(Integer(self)));
  if Assigned(FEvent) then begin
    FEvent.SetEvent;
    Sleep(20); // Makes sure WaitForJson ends
  end;
  FreeAndNil(FCs);
  FreeAndNil(FEvent);
  inherited;
end;

procedure TMyHttpConnection.AnswerPage(var Flags: THttpGetFlag; const Status,
  Header, HtmlFile: String; UserData: TObject; Tags: array of const);
var
  st : TStringList;
begin
  st := TStringList.Create;
  try
    try
      st.LoadFromFile(TemplateDir + '\' +HtmlFile);
    except
      on E: Exception do begin
        st.Add('<HTML><BODY>');
        st.Add('Unable to open ''' + HtmlFile + '''<BR>');
        st.Add(E.ClassName + ': ' + E.Message);
        st.Add('</BODY></HTML>');
      end;
    end;
    AnswerString(Flags,Status,'text/html',Header,st.Text);
  finally
    st.free;
  end;
end;


function TMyHttpConnection.RequiresAuthentication: Boolean;
begin
  result:=SameText(Path,'/rauth') or SameText(Path,'/') and
          IsConnectStatus and (FSession.ConnectStatus=csLocal);
end;

function TMyHttpConnection.IsConnectStatus: Boolean;
begin
  ResolveSession(false);
  result:=(FSession <>nil) and (FSession.ConnectStatus<>csNone);
end;

function TMyHttpConnection.ResolveSession(CreateIfDoesntExist:boolean=True):Boolean;
var
  Log : ILogger;
  Id:Integer;
  dest,sid : string;
begin
  Log := TLogger.Create(self,'ResolveSession');
  result:=False;
  Id:=0;
  if GetCookieValue(RequestCookies,'SID',sid) then
    Id:=StrToIntDef(sid,0);
  if Id=0 then begin
    ExtractURLEncodedValue(Params,'id',sid);
    id:=StrToIntDef(sid,0);
  end;

  FSession:=gCaptureThread.GetSession(id);
  if FSession=nil then begin
    if not CreateIfDoesntExist then exit;
    FSession := TClientSession.Create(gCaptureThread.MirrorManager,true);
    FSession.EmbeddedImage:=true;
  end;
  result:=true;
end;

function TMyHttpConnection.ProcessPacket(JsonText: AnsiString):AnsiString;
begin
  result:=FSession.ProcessPacket(JsonText);
end;


function TMyHttpConnection.QueryStringToJson:string;
var
  QueryFields : TStringList;
  json : string;
  n : Integer;
begin
  QueryFields := TStringList.Create;
  try
    QueryFields.Delimiter:='&';
    QueryFields.DelimitedText:=UrlDecode(Params);
    json:='';
    for n := 0 to QueryFields.Count - 1 do begin
      if json<>'' then json:=json+', ';
      json:=json+Format('"%s":"%s"',[QueryFields.Names[n],QueryFields.ValueFromIndex[n]]);
    end;
    result:='{'+json+'}';
  finally
    QueryFields.Free;
  end;
end;

procedure TMyHttpConnection.SendScreen(Text: AnsiString;APacketNumber:Cardinal);
var
  Log : ILogger;
begin
  if not assigned(FCs) then Abort;
  Log := TLogger.Create(self,'SendScreen: '+IntToStr(Integer(self)));
  FCs.Enter;
  try
    FJsonText:=Text;
    if assigned(FEvent) then
      FEvent.SetEvent;
    LogInfo('SetEvent(FEvent): '+IntToStr(Integer(FEvent)));
  finally
    FCs.Leave;
  end;
end;

function TMyHttpConnection.WaitForJson(out output:AnsiString):boolean;
var
  Log : ILogger;
begin
  Log := TLogger.Create(self,'WaitForJson : '+IntToStr(Integer(self)));
  if assigned(FEvent) then begin
    FCs.Enter;
    try
      FJsonText:='';
      FSession.OnSendScreen:=SendScreen;
      FSession.Enable;
      FEvent.ResetEvent;
    finally
      FCs.Leave;
    end;
    LogInfo('WaitForEvent(FEvent): '+IntToStr(Integer(FEvent)));
    result:=WaitForEvent(FEvent,10000)=wrSignaled;
    FSession.OnSendScreen:=nil;
    output:=FJsonText;
  end;
end;

end.
