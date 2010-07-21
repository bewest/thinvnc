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

unit ThinVnc.Manager;
{$I ThinVnc.DelphiVersion.inc}
{$I ThinVnc.inc}
interface

uses
  Windows,SysUtils, Classes, Controls, Forms,Messages,
  StdCtrls, ExtCtrls, StrUtils,Inifiles,jpeg,SyncObjs,
  ComCtrls, ActnList, Menus,OverbyteIcsHttpsrv,OverbyteIcsWinSock,
  pngextra, pngimage,Shellapi,Graphics,
  ThinVnc.Log,
  ThinVnc.Utils,
  ThinVnc.HttpServer,
  ThinVnc.Paths,
  ThinVNC.capture, ToolWin;

type
  TWebServForm = class(TForm)
    ToolsPanel: TPanel;
    TrayIcon1: TTrayIcon;
    StatusBar1: TStatusBar;
    PopupMenu1: TPopupMenu;
    ActionList1: TActionList;
    actStart: TAction;
    actStop: TAction;
    actExit: TAction;
    Exit1: TMenuItem;
    N1: TMenuItem;
    PageControl1: TPageControl;
    tabStatus: TTabSheet;
    tabHttp: TTabSheet;
    tabTCP: TTabSheet;
    chkAutoStart: TCheckBox;
    StartButton: TButton;
    StopButton: TButton;
    edTcpPort: TEdit;
    Label1: TLabel;
    Image1: TPNGImage;
    gbAuth: TGroupBox;
    lblPassword: TLabel;
    lblUser: TLabel;
    rbAuthDigest: TRadioButton;
    rbAuthNone: TRadioButton;
    edPassword: TEdit;
    edUser: TEdit;
    gbPort: TGroupBox;
    Label3: TLabel;
    edHttpPort: TEdit;
    lbHttpsPort: TLabel;
    edHttpsPort: TEdit;
    chkHttpEnabled: TCheckBox;
    chkHttpsEnabled: TCheckBox;
    TimerTray: TTimer;
    actShow: TAction;
    actSettings: TAction;
    Settings1: TMenuItem;
    Show1: TMenuItem;
    N2: TMenuItem;
    bManageCertificate: TButton;
    gbPresentation: TGroupBox;
    Label4: TLabel;
    edExtUrl: TEdit;
    mTicketText: TMemo;
    Label2: TLabel;
    actInstallWindowsService: TAction;
    actUninstallWindowsService: TAction;
    actClose: TAction;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    PopupMenu2: TPopupMenu;
    Start2: TMenuItem;
    Stop2: TMenuItem;
    N5: TMenuItem;
    InstallasWindowsService1: TMenuItem;
    UninstallWindowsService1: TMenuItem;
    N6: TMenuItem;
    Close2: TMenuItem;
    N3: TMenuItem;
    Help1: TMenuItem;
    actHelp: TAction;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure actStartExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure rbAuthDigestClick(Sender: TObject);
    procedure NumericOnlyKeyPress(Sender: TObject; var Key: Char);
    procedure TimerTrayTimer(Sender: TObject);
    procedure actShowExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure bManageCertificateClick(Sender: TObject);
    procedure actInstallWindowsServiceExecute(Sender: TObject);
    procedure actUninstallWindowsServiceExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure MainMenu1Change(Sender: TObject; Source: TMenuItem;
      Rebuild: Boolean);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure TrayIcon1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure actHelpExecute(Sender: TObject);
  private
    FIniFileName   : String;
    FForceClose    : Boolean;
    FHttpServer    : THttpServer;
    procedure InitTray(ToTray:Boolean);
    procedure Display(msg: string);
    procedure SaveConfig(CatchException: Boolean);
    procedure HttpServerStarted(Sender: TObject);
    procedure HttpServerStopped(Sender: TObject);
    procedure EnableControls(AControl: TControl; AValue: Boolean);
  end;


var
  WebServForm       : TWebServForm;
  
implementation
uses uLkJSON;

{$R *.DFM}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.FormCreate(Sender: TObject);
  function Decrypt(Value: string; Default: string): string;
  begin
    result:=Value;
    if Result = '' then Result := Default;
  end;
var
  IniFile : TIniFile;
begin
  TrayIcon1.Icon.Assign(Application.Icon);

  FHttpServer := THttpServer.Create;
  FHttpServer.OnStarted:=HttpServerStarted;
  FHttpServer.OnStopped:=HttpServerStopped;

  FIniFileName := ThinVncSettingsPath + 'ThinVnc.ini';
  IniFile      := TIniFile.Create(FIniFileName);
  try
    edUser.Text       := Decrypt(IniFile.ReadString('Authentication', 'User',''), 'admin');
    edPassword.Text   := Decrypt(IniFile.ReadString('Authentication', 'Password', ''), 'admin');
    edHttpPort.Text   := IniFile.ReadString('Http', 'Port','80');
    edHttpsPort.Text  := IniFile.ReadString('Https', 'Port','443');
    chkHttpEnabled.Checked:=IniFile.ReadBool('Http', 'Enabled',true);
    chkHttpsEnabled.Checked:=IniFile.ReadBool('Https', 'Enabled',true);
    edExtUrl.Text     := IniFile.ReadString('Presentation', 'ExtUrl','');
    mTicketText.Text  := StringReplace(IniFile.ReadString('Presentation', 'Invitation',StringReplace(mTicketText.Text,#13#10,'^',[rfReplaceall])),'^',#13#10,[rfReplaceall]);
    rbAuthDigest.Checked:= IniFile.ReadString('Authentication', 'Type','Digest')='Digest';
    if not rbAuthDigest.Checked then rbAuthNone.Checked := True;
    chkAutoStart.Checked:=IniFile.ReadBool('General', 'AutoStart',false)
  finally
    IniFile.Free;
  end;

  InitTray(FileExists(FIniFileName));

  rbAuthDigestClick(nil);
  PageControl1.ActivePageIndex:=0;
  actStart.visible:= true;
  actStop.visible:= false;
  actShow.Enabled:=false;
  if chkAutoStart.Checked then actStart.execute;
end;

procedure TWebServForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveConfig(True);
  actStopExecute(nil);
  FreeAndNil(FHttpServer);
end;

procedure TWebServForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:=FForceClose;
  if not CanClose then begin
    InitTray(true);
    hide;
  end;
end;

procedure TWebServForm.InitTray(ToTray:Boolean);
begin
  if not FindCmdLineSwitch('notray') then
    TrayIcon1.Visible:=true
  else FForceClose:=true;
  Application.ShowMainForm:=not ToTray;
end;

procedure TWebServForm.MainMenu1Change(Sender: TObject; Source: TMenuItem;
  Rebuild: Boolean);
begin
end;

procedure TWebServForm.NumericOnlyKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key >= #$20) and not (Key in ['0'..'9']) then Key := #$00;
end;

procedure TWebServForm.PopupMenu2Popup(Sender: TObject);
begin
end;

procedure TWebServForm.rbAuthDigestClick(Sender: TObject);
begin
  edUser.Enabled:=rbAuthDigest.Checked;
  edPassword.Enabled:=rbAuthDigest.Checked;
  lblUser.Enabled:=rbAuthDigest.Checked;
  lblPassword.Enabled:=rbAuthDigest.Checked;
end;

procedure TWebServForm.actCloseExecute(Sender: TObject);
begin
  Hide;
end;

procedure TWebServForm.actExitExecute(Sender: TObject);
begin
  FForceClose:=true;
  Close;
end;

procedure TWebServForm.actHelpExecute(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://www.supportsmith.com/thinvnc/help.aspx', nil, nil, 0);
end;

procedure TWebServForm.actSettingsExecute(Sender: TObject);
begin
  Show;
  SetForegroundWindow(Handle);
end;

procedure TWebServForm.actShowExecute(Sender: TObject);
begin
  ShellExecute(0,'open',PChar(ParamStr(0)),'-p',nil,0);
end;

procedure TWebServForm.actStartExecute(Sender: TObject);
var
  DisplayText,DisplayText2 : string;
  Started : Boolean;
  IniFile : TIniFile;
begin

  with FHttpServer do begin
    HttpEnabled:=chkHttpEnabled.Checked;
    HttpsEnabled:=chkHttpsEnabled.Checked;
    Port := StrToIntDef(edHttpPort.Text, 80);
    User := edUser.Text;
    Password := edPassword.Text;
    if rbAuthDigest.Checked then
      AuthType:=atDigest
    else AuthType:=atNone;

  end;

  Started := False;
  DisplayText:='';
  try
    if FHttpServer.Start then begin
      DisplayText:='http on port '+IntToStr(FHttpServer.Port);
      Started:=true;
    end;
  except
    on E: Exception do begin
      if FHttpServer.LastError = WSAEADDRINUSE then begin
        DisplayText:='http port ' + IntToStr(FHttpServer.Port) + ' in use';
      end else DisplayText:=E.Message;
    end;
  end;

  DisplayText2:='';
  if (DisplayText<>'') and (DisplayText2<>'') then
    DisplayText:=DisplayText+ ', '+DisplayText2
  else if (DisplayText2<>'') then DisplayText:=DisplayText2;
  if Started then DisplayText:='Server started. Listening '+DisplayText + '.';

  Display(DisplayText);
  
  SaveConfig(False);
end;

procedure TWebServForm.actStopExecute(Sender: TObject);
begin
  if Assigned(FHttpServer) then
    FHttpServer.Stop;
end;

procedure TWebServForm.bManageCertificateClick(Sender: TObject);
begin
end;


procedure TWebServForm.actUninstallWindowsServiceExecute(Sender: TObject);
begin
end;

procedure TWebServForm.actInstallWindowsServiceExecute(Sender: TObject);
begin
end;

procedure TWebServForm.TimerTrayTimer(Sender: TObject);
begin
end;

procedure TWebServForm.TrayIcon1DblClick(Sender: TObject);
begin
  Show;
  SetForegroundWindow(Handle);
end;

procedure TWebServForm.TrayIcon1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  CursorPos: TPoint;
begin
  if Button=mbLeft then begin
    if GetCursorPos(CursorPos) then begin
      TrayIcon1.PopupMenu.Popup(CursorPos.X, CursorPos.Y);
    end;
  end;
end;

procedure TWebServForm.Display(msg:string);
begin
  Statusbar1.Panels[0].Text:=msg;
end;

procedure TWebServForm.EnableControls(AControl:TControl;AValue:Boolean);
var
  n : Integer;
begin
  AControl.Enabled:=AValue;
  if AControl is TWinControl then
    for n:= 0 to (AControl as TWinControl).ControlCount - 1 do
      EnableControls((AControl as TWinControl).Controls[n],AValue);
end;

procedure TWebServForm.HttpServerStarted(Sender: TObject);
begin
  EnableControls(gbPort,False);
  EnableControls(gbAuth,False);
  EnableControls(gbPresentation,False);
  edTcpPort.Enabled := False;
  actStart.visible:= False;
  actStop.visible:= true;
  actShow.Enabled:=true;

  Display(Format('Listening on port %d',[FHttpServer.Port]));
end;


procedure TWebServForm.HttpServerStopped(Sender: TObject);
begin
  EnableControls(gbPort,true);
  EnableControls(gbAuth,true);
  EnableControls(gbPresentation,true);
  rbAuthDigestClick(nil);
  edTcpPort.Enabled:= true;
  actStart.visible := true;
  actStop.visible := false;
  actShow.Enabled := false;
  Display('Server stopped');
end;

procedure TWebServForm.SaveConfig(CatchException: Boolean);
  function  EncryptString(Value:string):string;
  begin
    result:=Value;
    end;
var
  IniFile : TIniFile;
  Auth : string;
begin
  { Save persistent data to INI file }
  try
    if rbAuthDigest.Checked then Auth:='Digest'
    else Auth:='None';

    IniFile := TIniFile.Create(FIniFileName);
    try
      IniFile.WriteString('Authentication','User', EncryptString(edUser.Text));
      IniFile.WriteString('Authentication','Password', EncryptString(edPassword.Text));
      IniFile.WriteString('Authentication','Type',Auth);
      IniFile.WriteString('Http','Port',edHttpPort.Text);
      IniFile.WriteBool('Http','Enabled',chkHttpEnabled.Checked);

      IniFile.WriteString('Presentation','ExtUrl',edExtUrl.Text);
      IniFile.WriteString('Presentation', 'Invitation',StringReplace(mTicketText.Text,#13#10,'^',[rfReplaceall]));


      IniFile.WriteString('Tcp','Port',edTcpPort.Text);
      IniFile.WriteBool('General','AutoStart',chkAutoStart.Checked);
    finally
      IniFile.Free;
    end;
  except
    if not CatchException then raise;
  end;
end;


end.

