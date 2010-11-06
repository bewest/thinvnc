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
  SysUtils, Classes, Forms,StrUtils,Inifiles,jpeg,SyncObjs,IdWinSock2,
  ThinVnc.WebSockets,
  ThinVnc.LkJSON,
  ThinVnc.Log,
  ThinVnc.Utils,
  ThinVnc.DigestAuth,
  ThinVnc.MirrorWindow,
  ThinVnc.ClientSession,ThinVNC.capture;

const
  NO_CACHE  = 'Cache-Control: no-cache' + #13#10;

type
  TOnAuthGetPassword = procedure (AConnection:TWebConnection;Username:string;var Password : String) of object;
  TOnAuthGetType = procedure (AConnection:TWebConnection;var AAuthType:TAuthenticationType;var AAuthRealm:string) of object;
  TProcessScriptEvent =  procedure (AConnection:TWebConnection) of object;
  
  TThinWebServer = class(TComponent)
  private
    FHttpServer: TWebSocketServer;
    FOnStopped: TNotifyEvent;
    FOnStarted: TNotifyEvent;
    FHttpEnabled: Boolean;
    FLastError : Integer;
    FOnAuthGetPassword: TOnAuthGetPassword;
    FOnAuthGetType: TOnAuthGetType;
    FHttpActive : boolean;
    FOnProcessScript: TProcessScriptEvent;
    FPassword: string;
    FUsername: string;

    function  GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function  GetLastError: Integer;
    procedure ServerStarted(Sender: TObject);
    procedure ServerStopped(Sender: TObject);
    procedure ProcessJson(AConnection:TWebConnection);
    procedure ProcessCmd(AConnection:TWebConnection);
    function  QueryStringToJson(Request: TWebRequest):string;
    function  ResolveSession(AConnection:TWebConnection;CreateIfDoesntExist:boolean):TClientSession;
    function  MakeCookie(const Name, Value : String; const Path: String) : String;
    function  GetHttpActive: Boolean;
    procedure SetHttpActive(const Value: Boolean);
    procedure StopHttp;
    function GetRootPath: string;
    procedure SetRootPath(const Value: string);
    function GetDefaultPage: string;
    procedure SetDefaultPage(const Value: string);
    function GetAuthenticationType: TAuthenticationType;
    procedure SetAuthenticationType(const Value: TAuthenticationType);
    function GetWebSocketsEnabled: Boolean;
    procedure SetWebSocketsEnabled(const Value: Boolean);
  protected
    procedure Loaded;override;
    procedure ProcessScript(AConnection:TWebConnection);virtual;
    procedure AuthGetType(AConnection:TWebConnection;var AAuthType:TAuthenticationType;var AAuthRealm:string);virtual;
    procedure AuthGetPassword(AConnection:TWebConnection;var Password: String);virtual;
  public
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy;override;
    function    Start:boolean;
    procedure   Stop;
    property    LastError:Integer read GetLastError;
  published
    property    WebSocketsEnabled:Boolean read GetWebSocketsEnabled write SetWebSocketsEnabled default true;
    property    HttpActive:Boolean read GetHttpActive write SetHttpActive default true;
    property    HttpPort:Integer read GetPort write SetPort default 80;
    property    RootPath:string read GetRootPath write SetRootPath;
    property    DefaultPage:string read GetDefaultPage write SetDefaultPage;
    property    AuthenticationType : TAuthenticationType read GetAuthenticationType write SetAuthenticationType;
    property    Username : string read FUsername write FUsername;
    property    Password : string read FPassword write FPassword;
    property    OnStarted:TNotifyEvent read FOnStarted write FOnStarted;
    property    OnStopped:TNotifyEvent read FOnStopped write FOnStopped;
    property    OnProcessScript:TProcessScriptEvent read FOnProcessScript write FOnProcessScript;
    property    OnAuthGetPassword: TOnAuthGetPassword read FOnAuthGetPassword write FOnAuthGetPassword;
    property    OnAuthGetType: TOnAuthGetType read FOnAuthGetType write FOnAuthGetType;
  end;

  TThinWebServerEx = class(TThinWebServer)
  private
    FPassword: string;
    FUser: string;
    FAuthType: TAuthenticationType;
  protected
    procedure   ProcessScript(AConnection:TWebConnection);override;
    procedure   AuthGetType(AConnection:TWebConnection;var AAuthType:TAuthenticationType;var AAuthRealm:string);override;
    procedure   AuthGetPassword(AConnection:TWebConnection;var Password: String);override;
  public
    constructor Create(AOwner: TComponent);override;
    property    AuthType:TAuthenticationType read FAuthType write FAuthType;
    property    User:string read FUser write FUser;
    property    Password:string read FPassword write FPassword;
  end;

implementation

{ TThinWebServer }

constructor TThinWebServer.Create(AOwner:TComponent);
begin
  inherited;
  FHttpEnabled:=true;
  FHttpServer:=TWebSocketServer.Create;
  with FHttpServer do begin
    OnServerStarted:=ServerStarted;
    OnServerStopped:=ServerStopped;
    OnProcessScript:=ProcessScript;
    OnAuthGetPassword:=Self.AuthGetPassword;
    OnAuthGetType:=Self.AuthGetType;
    RootPath:=GetModulePath+'web\';
  end;

end;

destructor TThinWebServer.Destroy;
begin
  FHttpServer.Free;
  inherited;
end;

function TThinWebServer.GetRootPath: string;
begin
  result:=FHttpServer.RootPath;
end;

function TThinWebServer.GetWebSocketsEnabled: Boolean;
begin
  result:=FHttpServer.WebSocketsEnabled;
end;

procedure TThinWebServer.SetRootPath(const Value: string);
begin
  FHttpServer.RootPath:=Value;
end;

procedure TThinWebServer.SetWebSocketsEnabled(const Value: Boolean);
begin
  FHttpServer.WebSocketsEnabled:=Value;
end;

procedure TThinWebServer.Loaded;
begin
  inherited;
  try
    HttpActive:=FHttpActive;
  except
  end;
end;

function TThinWebServer.GetPort: Integer;
begin
  result:=FHttpServer.Port;
end;

procedure TThinWebServer.SetPort(const Value: Integer);
begin
  FHttpServer.Port:=Value;
end;

function TThinWebServer.Start:Boolean;
begin
  result:=false;
  if FHttpEnabled then
  begin
    LogInfo('Starting HTTP Service...');
    FHttpServer.Active:=True;
    if not FHttpServer.Active then begin
      FLastError:=WSAEADDRINUSE;
      abort;
    end;
  end;
  result:=FHttpServer.Active;
end;

procedure TThinWebServer.Stop;
begin
  StopHttp;
end;

procedure TThinWebServer.StopHttp;
begin
  if FHttpEnabled then
  begin
    LogInfo('Stopping HTTP Service...');
    FHttpServer.Active:=False;
  end;
  CaptureThread.FreeSessions;
end;

procedure TThinWebServer.SetDefaultPage(const Value: string);
begin
  FHttpServer.DefaultPage:=Value;
end;

procedure TThinWebServer.SetHttpActive(const Value: Boolean);
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    FHttpActive:=Value
  else begin
    if Value then begin
      Start;
    end else begin
      Stop;
    end;
  end;
end;

function TThinWebServer.GetDefaultPage: string;
begin
  result:=FHttpServer.DefaultPage;
end;

function TThinWebServer.GetHttpActive: Boolean;
begin
  if csDesigning in ComponentState then
    result:=FHttpActive
  else result:=FHttpServer.active;
end;

procedure TThinWebServer.SetAuthenticationType(
  const Value: TAuthenticationType);
begin
  FHttpServer.AuthenticationType:=Value;
end;

function TThinWebServer.GetAuthenticationType: TAuthenticationType;
begin
  result:=FHttpServer.AuthenticationType;
end;



function TThinWebServer.GetLastError: Integer;
begin
  result:=FLastError;
end;


procedure TThinWebServer.ProcessScript(AConnection:TWebConnection);
var
  Log : ILogger;
begin
  Log := TLogger.Create(self,'ProcessScript');

  if CompareText(AConnection.Request.Path, 'json') = 0 then
    ProcessJson(AConnection)
  else if CompareText(AConnection.Request.Path, 'cmd') = 0 then
    ProcessCmd(AConnection)
  else if Assigned(FOnProcessScript) then
    FOnProcessScript(AConnection)
  else AConnection.Answer404;
end;

procedure TThinWebServer.ProcessJson(AConnection:TWebConnection);
var
  Session : TClientSession;
  output : Ansistring;
begin
  Session:=ResolveSession(AConnection,false);
  if assigned(Session) then begin
     if Session.ForceDisconnect then
      output:='{"status":9,"windows": []}'
    else if not Session.WaitForJson(output) then
      output:='{"status":2,"windows": []}'
  end else output:='{"status":3,"windows": []}';

  AConnection.AnswerString('','application/x-javascript',NO_CACHE,Output);
end;

procedure TThinWebServer.ProcessCmd(AConnection:TWebConnection);
var
  Cmd,Output : AnsiString;
  Session : TClientSession;
  SendHeader: AnsiString;
  Log : ILogger;
begin
  Log := TLogger.Create(self,'ProcessCmd');
  Cmd:=AConnection.Request.QueryFields.Values['Cmd'];
  Session:=ResolveSession(AConnection,SameText(Cmd,'start') or SameText(Cmd,'connect'));
  if not assigned(Session) then Exit;
  if SameText(Cmd,'connect') then
  begin
    Log.LogDebug(Format('Session $%s. ID:%s - Cmd:%s - QueryString:%s',
      [IntToHex(Integer(Session), 8), Session.Id, Cmd, AConnection.Request.Query]));
  end;
  if not SameText(Cmd,'connect') or Session.TryLock then
  try
  
    output:=Session.ProcessPacket(QueryStringToJson(AConnection.Request));

    if (Session.ConnectStatus=csNone) and Session.NeedRemoteAuthentication then
    begin
      SendHeader := NO_CACHE + MakeCookie('SID',Session.Id,'/') + Session.AuthenticationData + #13#10;
      AConnection.AnswerString('401 Access Denied', 'application/x-javascript', SendHeader, Session.GetConnectStatus);
      Exit;
    end;

    if output<>'' then begin
      SendHeader := NO_CACHE + MakeCookie('SID',Session.Id,'/');
      AConnection.AnswerString('','application/x-javascript', SendHeader, output);
    end;
  finally
    if SameText(Cmd,'connect') then Session.Unlock;
  end else LogError('Multiple connect request');
end;

procedure TThinWebServer.AuthGetType(AConnection:TWebConnection;var AAuthType:TAuthenticationType;var AAuthRealm:string);
begin
  if Assigned(OnAuthGetType) then
    OnAuthGetType(AConnection,AAuthType,AAuthRealm);
end;

procedure TThinWebServer.AuthGetPassword(AConnection:TWebConnection;var Password : String);
begin
  if Assigned(FOnAuthGetPassword) then
    FOnAuthGetPassword(AConnection,AConnection.AuthUserName,Password)
  else if SameText(AConnection.AuthUserName,FUsername) then
    Password:=FPassword;
end;

procedure TThinWebServer.ServerStarted(Sender: TObject);
begin
  if Sender = FHttpServer then
    LogInfo('HTTP Service was started') else
    LogInfo('HTTPS Service was started');
  if assigned(FOnStarted) then
    FOnStarted(Sender);
end;

procedure TThinWebServer.ServerStopped(Sender: TObject);
begin
  if Sender = FHttpServer then
    LogInfo('HTTP Service was stopped') else
    LogInfo('HTTPS Service was stopped');
  if assigned(FOnStopped) then
    FOnStopped(Sender);
end;


function TThinWebServer.MakeCookie(const Name, Value : String; const Path: String) : String;
begin
    Result := 'Set-Cookie: ' + Name + '=' + Value;
    Result := Result + '; PATH=' + Path+#13#10;
end;

function TThinWebServer.ResolveSession(AConnection:TWebConnection;CreateIfDoesntExist:boolean):TClientSession;
var
  Log : ILogger;
  sid : string;
begin
  Log := TLogger.Create(self,'ResolveSession');

  try
    sid:=AConnection.Request.QueryFields.Values['id'];
    result:=GetClientSession(sid);
    if result=nil then begin
      if not CreateIfDoesntExist then exit;
      result := AConnection.CreateSession(true);
      result.EmbeddedImage:=true;
    end else AConnection.AssignSession(result);
  finally
    if result<>nil then result.Update; // Lastused
  end;
end;

function TThinWebServer.QueryStringToJson(Request: TWebRequest):string;
  function StringForJson(Value: string): string;
  var I: Integer;
  begin
    Result := '';
    for I := 1 to Length(Value) do
    begin
      case Value[I] of
        '/', '\', '"': Result := Result + '\' + Value[I];
         #8: Result := Result + '\b';
         #9: Result := Result + '\t';
        #10: Result := Result + '\n';
        #13: Result := Result + '\r';
        #12: Result := Result + '\f';
        #00..#07,#11,#14..#19: Result := Result + '\u' + IntToHex(Ord(Value[I]), 4);
        else Result := Result + Value[I];
      end;
    end;
  end;
var
  json : string;
  n : Integer;
  ParamName, ParamValue: string;
begin
  json:='';
  for n := 0 to Request.QueryFields.Count - 1 do begin
    ParamName := StringForJson(Request.QueryFields.Names[n]);
    ParamValue := StringForJson(Request.QueryFields.ValueFromIndex[n]);
    if json<>'' then json:=json+', ';
    json:=json+Format('"%s":"%s"',[ParamName, ParamValue]);
  end;
  if Request.WebMethod = wmPost then
  begin
    if Request.IsBinaryPost then
    begin
      ParamName := 'textdata';
      ParamValue := StringForJson(Request.BinaryData);
      if json<>'' then json:=json+', ';
      json:=json+Format('"%s":"%s"',[ParamName, ParamValue]);

      ParamName := 'linemode';
      ParamValue := Request.GetHeaderValue('User-Agent');
      if Pos('windows', LowerCase(ParamValue)) > 0 then
        ParamValue := 'CRLF' else
        ParamValue := 'LF';
      if json<>'' then json:=json+', ';
      json:=json+Format('"%s":"%s"',[ParamName, ParamValue]);
    end else begin
      for n := 0 to Request.PostFields.Count - 1 do begin
        ParamName := StringForJson(Request.PostFields.Names[n]);
        ParamValue := StringForJson(Request.PostFields.ValueFromIndex[n]);
        if json<>'' then json:=json+', ';
        json:=json+Format('"%s":"%s"',[ParamName, ParamValue]);
      end;
    end;
  end;
  result:='{'+json+'}';
end;



{ TThinWebServerEx }

constructor TThinWebServerEx.Create(AOwner: TComponent);
begin
  inherited;
  FHttpServer.ProtectedPages.Add(DefaultPage);
end;

procedure TThinWebServerEx.AuthGetPassword(AConnection: TWebConnection;
  var Password: String);
var
  Ticket : string;
  Session : TClientSession;
begin
  if (AConnection.AuthUserName = FUser) then
    Password := FPassword;
end;

procedure TThinWebServerEx.AuthGetType(AConnection: TWebConnection;
  var AAuthType: TAuthenticationType; var AAuthRealm: string);
var
  Ticket : string;
  Session : TClientSession;
begin
  AAuthRealm := 'ThinVNC';
  AAuthType  := FAuthType;
end;

procedure TThinWebServerEx.ProcessScript(AConnection: TWebConnection);
var
  Log : ILogger;
begin
  Log := TLogger.Create(self,'ProcessScript');

  inherited;
end;


end.
