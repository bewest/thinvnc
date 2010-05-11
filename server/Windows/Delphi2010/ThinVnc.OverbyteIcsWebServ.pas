unit ThinVnc.OverbyteIcsWebServ;

(*:@author Gustavo Ricardi
   @desc <pre>

This software is distributed under the GPL license.

Copyright (c) 2010, Gustavo Ricardi
All rights reserved.
*)

{$I OverbyteIcsDefs.inc}
{$B-}                 { Enable partial boolean evaluation   }
{$T-}                 { Untyped pointers                    }
{$X+}                 { Enable extended syntax              }
{$I+}                 { Turn IO exceptions to on            }
{$H+}                 { Use long strings                    }
{$J+}                 { Allow typed constant to be modified }
{$IFDEF COMPILER12_UP}
    { These are usefull for debugging !}
    {$WARN IMPLICIT_STRING_CAST       OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS  OFF}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Controls, Forms,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls, StrUtils,Inifiles,jpeg,
  OverbyteIcsWinSock,  OverbyteIcsWSocket, OverbyteIcsWndControl,
  OverbyteIcsHttpSrv, OverbyteIcsUtils,
  ThinVnc.Windows,
  ThinVnc.Utils,
  ThinVnc.Input,
  ComCtrls, ActnList, Menus;

const
  NO_CACHE  = 'Pragma: no-cache' + #13#10 + 'Expires: -1' + #13#10;


type
  { This is the main form for our application. Any data here is global for  }
  { all clients. Put private data in THttpConnection class (see above).   }
  TWebServForm = class(TForm)
    ToolsPanel: TPanel;
    HttpServer1: THttpServer;
    StartButton: TButton;
    StopButton: TButton;
    Label3: TLabel;
    edPort: TEdit;
    TrayIcon1: TTrayIcon;
    StatusBar1: TStatusBar;
    GroupBox1: TGroupBox;
    rbAuthNTLM: TRadioButton;
    rbAuthDigest: TRadioButton;
    rbAuthNone: TRadioButton;
    lblPassword: TLabel;
    edPassword: TEdit;
    edUser: TEdit;
    lblUser: TLabel;
    PopupMenu1: TPopupMenu;
    ActionList1: TActionList;
    actStart: TAction;
    actStop: TAction;
    Start1: TMenuItem;
    Stop1: TMenuItem;
    actExit: TAction;
    Exit1: TMenuItem;
    N1: TMenuItem;
    chkAutoStart: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure actStartExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure HttpServer1ServerStarted(Sender: TObject);
    procedure HttpServer1ServerStopped(Sender: TObject);
    procedure HttpServer1AuthGetType(Sender, Client: TObject{;
      var AuthenticationType: TAuthenticationType});
    procedure HttpServer1AuthGetPassword(Sender, Client: TObject;
      var Password: String);
    procedure HttpServer1AuthResult(Sender, Client: TObject;
      Success: Boolean);
    procedure HttpServer1GetDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure FormDestroy(Sender: TObject);
    procedure HttpServer1AuthNtlmBeforeValidate(Sender, Client: TObject;
      var Allow: Boolean);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FIniFileName   : String;
    FWmgr          : TMirrorManager;
    FForceClose    : Boolean;
    procedure CreateVirtualDocument_Json(Sender: TObject;
      ClientCnx: THttpConnection; var Flags: THttpGetFlag);
    procedure CreateVirtualDocument_Root(Sender: TObject;
      ClientCnx: THttpConnection; var Flags: THttpGetFlag);
    procedure ProcessMouse(Sender: TObject; ClientCnx: THttpConnection;
      var Flags: THttpGetFlag);
    procedure ProcessKey(Sender: TObject; ClientCnx: THttpConnection;
      var Flags: THttpGetFlag);
    procedure InitTray(ToTray:Boolean);
    procedure Display(msg: string);
    procedure ProcessImages(Sender: TObject; ClientCnx: THttpConnection;
      var Flags: THttpGetFlag);
    procedure SaveConfig(CatchException: Boolean);
  end;

var
  WebServForm       : TWebServForm;
  DisplayLock       : TRTLCriticalSection;

implementation

{$R *.DFM}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.FormCreate(Sender: TObject);
var
  IniFile : TIniFile;
begin
  TrayIcon1.Icon.Assign(Application.Icon);

  FWmgr:=TMirrorManager.Create;
  FWmgr.UseJpeg := true;
  FWmgr.UseCanvas := true;
  FWmgr.EmbeddedImage := true;

  HttpServer1.DocDir:=GetModulePath;
  HttpServer1.TemplateDir:=GetModulePath;

  { Create IniFileName based on EXE file name; }
  FIniFileName := ChangeFileExt(ParamStr(0),'.ini');
  IniFile      := TIniFile.Create(FIniFileName);
  try
    edUser.Text       := IniFile.ReadString('Data', 'User','test');
    edPassword.Text   := IniFile.ReadString('Data', 'Password','test');
    edPort.Text       := IniFile.ReadString('Data', 'Port','80');
    rbAuthNTLM.Checked:= IniFile.ReadString('Data', 'Authentication','')='NTLM';
    rbAuthDigest.Checked:= IniFile.ReadString('Data', 'Authentication','')='Digest';
    chkAutoStart.Checked:=IniFile.ReadBool('Data', 'AutoStart',false)
  finally
    IniFile.Free;
  end;

  InitTray(FileExists(FIniFileName));

  if chkAutoStart.Checked then
    actStart.execute;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.FormDestroy(Sender: TObject);
begin
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveConfig(True);
end;


procedure TWebServForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:=FForceClose;
  if not CanClose then
  begin
    InitTray(true);
    hide;
  end;
end;

procedure TWebServForm.InitTray(ToTray:Boolean);
begin
  TrayIcon1.Visible:=true;
  Application.ShowMainForm:=not ToTray;
end;

procedure TWebServForm.actExitExecute(Sender: TObject);
begin
  FForceClose:=true;
  Close;
end;

procedure TWebServForm.actStartExecute(Sender: TObject);
begin
  HttpServer1.Port                 := Trim(edPort.Text);
  HttpServer1.ClientClass          := THttpConnection;
  try
    HttpServer1.Start;
  except
    on E: Exception do begin
      if HttpServer1.WSocketServer.LastError = WSAEADDRINUSE then begin
        Display('**** Port ' + HttpServer1.Port +
            ' already used by another application ****');
        Exit;
      end;
      Display('**** ' + E.ClassName + ': ' + E.Message + ' ****');
    end;
  end;
  SaveConfig(False);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when user clicks on stop button. We just  }
{ stop the server. We will get OnServerStopped event triggered.             }
procedure TWebServForm.actStopExecute(Sender: TObject);
begin
  HttpServer1.Stop;
end;


procedure TWebServForm.TrayIcon1DblClick(Sender: TObject);
begin
  Show;
end;

procedure TWebServForm.Display(msg:string);
begin
  EnterCriticalSection(DisplayLock);
  try
    Statusbar1.Panels[0].Text:=msg;
  finally
    LeaveCriticalSection(DisplayLock);
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when HTTP server is started, that is when }
{ server socket has started listening.                                      }
procedure TWebServForm.HttpServer1ServerStarted(Sender: TObject);
begin
  edPort.Enabled                  := FALSE;
  actStart.Enabled               := FALSE;
  actStop.Enabled                := TRUE;
  Display('Listening on port ' + HttpServer1.Port);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when server has been stopped, that is     }
{ when server socket stop listening.                                        }
procedure TWebServForm.HttpServer1ServerStopped(Sender: TObject);
begin
  edPort.Enabled                  := TRUE;
  actStart.Enabled               := TRUE;
  actStop.Enabled                := FALSE;
  Display('Server stopped');
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.HttpServer1GetDocument(Sender, Client: TObject;
  var Flags: THttpGetFlag);
var
  ClientCnx  : THttpConnection;
begin
  if Flags = hg401 then
      Exit;

  ClientCnx := THttpConnection(Client);

  if CompareText(ClientCnx.Path, '/') = 0 then
    CreateVirtualDocument_Root(Sender, ClientCnx, Flags)
  else if CompareText(ClientCnx.Path, '/json') = 0 then
    CreateVirtualDocument_Json(Sender, ClientCnx, Flags)
  else if CompareText(ClientCnx.Path, '/mouse') = 0 then
    ProcessMouse(Sender, ClientCnx, Flags)
  else if CompareText(ClientCnx.Path, '/key') = 0 then
    ProcessKey(Sender, ClientCnx, Flags)
  else if CompareText(ClientCnx.Path, '/img') = 0 then
    ProcessImages(Sender, ClientCnx, Flags);
end;

procedure TWebServForm.CreateVirtualDocument_Root(
    Sender    : TObject;
    ClientCnx : THttpConnection;
    var Flags : THttpGetFlag);
var
  QueryFields : TStringList;
begin
  QueryFields := TStringList.Create;
  try
    QueryFields.Delimiter:='&';
    QueryFields.DelimitedText:=ClientCnx.Params;
    if Pos('MSIE',ClientCnx.RequestUserAgent)>0 then begin
      FWmgr.UseCanvas:=SameText(QueryFields.Values['canvas'],'true');
      FWmgr.EmbeddedImage:=SameText(QueryFields.Values['embedimg'],'true')
    end else begin
      FWmgr.UseCanvas:=not SameText(QueryFields.Values['canvas'],'false');
      FWmgr.EmbeddedImage:=not SameText(QueryFields.Values['embedimg'],'false');
    end;
    FWmgr.UseJpeg:=not SameText(QueryFields.Values['png'],'true');
    FWmgr.JpgQuality:=StrToIntDef(QueryFields.Values['quality'],FWmgr.JpgQuality);
    FWmgr.JpgPixelFormat:=TJPEGPixelFormat(StrToIntDef(QueryFields.Values['pixelformat'],Integer(FWmgr.JpgPixelFormat)));
    FWmgr.JpgGrayScale:=SameText(QueryFields.Values['grayscale'],'true');

  finally
    QueryFields.Free;
  end;
  if FWmgr.UseCanvas then
    ClientCnx.AnswerPage(Flags,'',NO_CACHE,'client-canvas.htm',nil,[])
  else
    ClientCnx.AnswerPage(Flags,'',NO_CACHE,'client-images.htm',nil,[]);
end;

procedure TWebServForm.ProcessImages(
    Sender    : TObject;
    ClientCnx : THttpConnection;
    var Flags : THttpGetFlag);
var
  id : string;
  ci:TCacheItem;
begin
  ExtractURLEncodedValue(ClientCnx.Params,'id',id);

  ci:=FWmgr.ExtractImageFromCache(id);

  if assigned(ClientCnx.DocStream) then
    ClientCnx.DocStream.Free;

  if assigned(ci) then begin
    ClientCnx.DocStream:=ci.GetStream;
    ClientCnx.AnswerStream(Flags,'','image/'+ci.ImageType,'');
    ci.Free;
  end;
end;

procedure TWebServForm.CreateVirtualDocument_Json(
    Sender    : TObject;
    ClientCnx : THttpConnection;
    var Flags : THttpGetFlag);
var
  rc : Boolean;
  reset : Boolean;
  n : Integer;
begin
  reset:=ClientCnx.Params='reset';
  repeat
    rc:=FWmgr.Capture(reset);
    if not rc then begin
      for n := 0 to 10 - 1 do begin
        Application.ProcessMessages;
        sleep(10);
      end;
    end;
  until rc=true;
  ClientCnx.AnswerString(Flags,
      '',           { Default Status '200 OK'         }
      'application/x-javascript',           { Default Content-Type: text/html }
      NO_CACHE,           { Default header                  }
      FWmgr.getJson(''));
end;

procedure TWebServForm.ProcessMouse(
    Sender    : TObject;
    ClientCnx : THttpConnection;
    var Flags : THttpGetFlag);
var
  x,y : Integer;
  btn : Integer;
  QueryFields : TStringList;
begin
  QueryFields := TStringList.Create;
  try
    QueryFields.Delimiter:='&';
    QueryFields.DelimitedText:=ClientCnx.Params;
    X:=StrToIntDef(QueryFields.Values['x'],0);
    Y:=StrToIntDef(QueryFields.Values['y'],0);
    btn:=StrToIntDef(QueryFields.Values['btn'],0)+1;
    ProcessMouseInput(X,Y,btn,QueryFields.Values['action']);
  finally
    QueryFields.Free;
  end;
  ClientCnx.AnswerString(Flags,
      '',           { Default Status '200 OK'         }
      '',           { Default Content-Type: text/html }
      NO_CACHE,           { Default header                  }
      '');
end;

procedure TWebServForm.SaveConfig(CatchException: Boolean);
var
  IniFile : TIniFile;
  Auth : string;
begin
  { Save persistent data to INI file }
  try
    if rbAuthNTLM.Checked then auth:='NTLM'
    else if rbAuthDigest.Checked then Auth:='Digest'
    else Auth:='None';

    IniFile := TIniFile.Create(FIniFileName);
    try
      IniFile.WriteString('Data',    'User',      edUser.Text);
      IniFile.WriteString('Data',    'Password',  edPassword.Text);
      IniFile.WriteString('Data',    'Authentication', Auth);
      IniFile.WriteString('Data',    'Port',        HttpServer1.Port);
      IniFile.WriteBool('Data',    'AutoStart',        chkAutoStart.Checked);
    finally
      IniFile.Free;
    end;
  except
    if not CatchException then raise;
  end;
end;

procedure TWebServForm.ProcessKey(
    Sender    : TObject;
    ClientCnx : THttpConnection;
    var Flags : THttpGetFlag);
begin
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.HttpServer1AuthGetType(
    Sender, Client: TObject);
var
  ClientCnx  : THttpConnection;
begin
  { It's easyer to do the cast one time. Could use with clause... }
  ClientCnx := THttpConnection(Client);
  if rbAuthDigest.Checked then begin
      ClientCnx.AuthTypes  := [atDigest];
      ClientCnx.AuthRealm := 'DigestAuth';
  end else if rbAuthNTLM.Checked then begin
      ClientCnx.AuthTypes  := [atNtlm];
      ClientCnx.AuthRealm := 'NtlmAuth';
  end{ else if CompareText(ClientCnx.Path, '/DemoAuthAll.html') = 0 then begin
      ClientCnx.AuthTypes  := [atBasic, atDigest, atNtlm];
      ClientCnx.AuthRealm := 'DemoAuthAll';
  end};
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.HttpServer1AuthGetPassword(
    Sender       : TObject;
    Client       : TObject;
    var Password : String);
var
  ClientCnx  : THttpConnection;
begin
  { It's easyer to do the cast one time. Could use with clause... }
  ClientCnx := THttpConnection(Client);
  if (ClientCnx.AuthTypes     = [atDigest]) and
     (ClientCnx.AuthUserName = edUser.Text) then
      Password := edPassword.Text
  else if (ClientCnx.AuthTypes     = [atBasic]) and
          (ClientCnx.AuthUserName = edUser.Text) then
      Password := edPassword.Text
  else if (ClientCnx.AuthTypes = [atNtlm]) then ;
      //  nothing to do windows will validate credentials
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.HttpServer1AuthResult(
    Sender, Client : TObject;
    Success        : Boolean);
var
  ClientCnx  : THttpConnection;
const
  SuccessStr : array [Boolean] of String = ('failed', 'OK');
begin
  { It's easier to do the cast one time. Could use with clause...         }
  ClientCnx := THttpConnection(Client);

  { If we always want to pop up client browser's login dialog with digest }
  { authentication when the nonce is stale we may set FAuthDigestStale    }
  { back to FALSE.  Note: Do not set this value to TRUE.                  }
  { A nonce is considered stale after AuthDigestNonceLifeTimeMin expired. }
  { Uncomment next three lines to see what changes.                       }
  {if (not Success) and (ClientCnx.AuthTypes = [atDigest]) and
     ClientCnx.FAuthDigestStale then
      ClientCnx.FAuthDigestStale := FALSE;}

  if (not Success) and (ClientCnx.AuthTypes = [atNtlm]) and
     (ClientCnx.AuthNtlmSession <> nil) then
      Display(ClientCnx.AuthNtlmSession.AuthErrorDesc);  // just for debugging!
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.HttpServer1AuthNtlmBeforeValidate(Sender, Client: TObject;
  var Allow: Boolean);
var
  ClientCnx  : THttpConnection;
begin
  ClientCnx := THttpConnection(Client);
  Allow := (ClientCnx.AuthNtlmSession.Username <> '') and
           (ClientCnx.AuthNtlmSession.Domain <> 'SomeDomain');
end;


initialization
  InitializeCriticalSection(DisplayLock);
finalization
  DeleteCriticalSection(DisplayLock);
end.

