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

unit ThinVNC.Log;
{$I ThinVnc.DelphiVersion.inc}
{$I ThinVnc.inc}
interface

uses
{$IFDEF SMARTINSPECT}
  SiAuto,SmartInspect,
{$ENDIF}
  Windows, SysUtils, Classes, IniFiles, Forms, ActiveX, SyncObjs,Registry,Graphics;

const
  PRODOWNER_KEY = 'Cybele Software';
var
  Product : string = 'ThinVnc';

{.$D-,L-,Y-,H+}
type
  TLogLevel = (llError, llWarning, llDebugInfo, llDebugProc);

type
  ILogger = interface
    ['{FBD1702D-C61A-40A7-AC91-83FDE77BDB6C}']
    procedure   LogDebug(Text: string);
    procedure   LogError(Text: string);
    procedure   LogException(Text: string);
    procedure   LogInfo(Text: string);
    procedure   LogMsg(Level: TLogLevel; Text: string);
    procedure   ODS(S:string);
    procedure   Dump(const Title: string; Ptr: Pointer; Len: Integer);
  end;

  TLogger = class(TInterfacedObject,ILogger)
  private
    FModule: string;
    FMethod: string;
    FThread : Boolean;
    function GetHeader(Code:string):string;
    procedure EnterMethod;
    procedure LeaveMethod;
  public
    constructor Create(Sender:TObject);overload;
    constructor create(Sender:TObject;AMethod: string;IsParentObject:Boolean=False);overload;
    constructor create(AModule:string;AMethod:string);overload;
    destructor  Destroy;override;
    procedure   LogDebug(Text: string);
    procedure   LogError(Text: string);
    procedure   LogException(Text: string);
    procedure   LogInfo(Text: string);
    procedure   LogMsg(Level: TLogLevel; Text: string);
    procedure   ODS(S:string);
    procedure   Dump(const Title: string; Ptr: Pointer; Len: Integer);
  end;

procedure LogError(Text: string);
procedure LogDebug(Text: string);
procedure LogInfo(Text: string);
procedure LogBitmap(Text: string;const Bitmap:Graphics.TBitmap);
procedure LogException(Text: string);
procedure LogDump(const Title: string; Ptr: Pointer; Len: Integer);
procedure CreateLog(AEnabled:Boolean=false); overload;

var
  gLogDir: string;
  gLogEnabled: Boolean;
  gConnections : string;

implementation

function GetDebugPath: string; forward;

{$IFDEF FILE_MONITOR}
type
  TLogMonitor = class(TObject)
  private
    FManagedFile: TManagedFile;
    procedure FileChanged(Sender: TObject);
  public
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

{ TLogMonitor }

destructor TLogMonitor.Destroy;
begin
  Stop;
  inherited;
end;

procedure TLogMonitor.FileChanged(Sender: TObject);
begin
  CreateLog;
end;

procedure TLogMonitor.Start;
begin
  FManagedFile := FilesMonitor.AddManagedFile(0, GetDebugPath + Product+'.ini', FileChanged);
  FilesMonitor.Start;
  FileChanged(nil);
end;

procedure TLogMonitor.Stop;
begin
  if Assigned(FManagedFile) then
  begin
    FManagedFile.Suscriptors.Remove(FileChanged);
    FManagedFile := nil;
  end;
end;

var
  gLogMon: TLogMonitor;

{$ENDIF}

var
  gLock : TCriticalSection;
  gIndentLevent: Integer;

threadvar
  IndentLevel : Integer;
  ParentObjectIndentLevel : Integer;
  ParentObject : TObject;

procedure Indent;
begin
  if not Assigned(gLock) then Exit;
  gLock.Enter;
  try
    Inc(gIndentLevent);
    IndentLevel:=gIndentLevent;
  finally
    gLock.Leave;
  end;
end;

procedure Unindent;
begin
  if not Assigned(gLock) then Exit;
  gLock.Enter;
  try
    Dec(gIndentLevent);
    IndentLevel:=gIndentLevent;
    if IndentLevel = ParentObjectIndentLevel then
      ParentObject:=nil;
  finally
    gLock.Leave;
  end;
end;

function CompleteText(AText:string):String;
begin
  if ParentObject<>nil then
    result:=Format('[%.4x] %s',[Integer(ParentObject),AText])
  else result:=AText;
end;

procedure LogError(Text: string);
begin
{$IFDEF SMARTINSPECT}
  SiMain.LogError(CompleteText(Text));
{$ENDIF}
end;

procedure LogDump(const Title: string; Ptr: Pointer; Len: Integer);
begin
{$IFDEF SMARTINSPECT}
  SiMain.LogMemory(Title,Ptr,Len);
{$ENDIF}
end;

procedure LogException(Text: string);
begin
{$IFDEF SMARTINSPECT}
  SiMain.LogException(CompleteText(Text));
{$ENDIF}
end;

procedure LogDebug(Text: string);
begin
{$IFDEF SMARTINSPECT}
  SiMain.LogDebug(CompleteText(Text));
{$ENDIF}
end;

procedure LogBitmap(Text: string;const Bitmap:Graphics.TBitmap);
begin
{$IFDEF SMARTINSPECT}
  SiMain.LogBitmap(CompleteText(Text),Bitmap);
{$ENDIF}
end;

procedure LogInfo(Text: string);
begin
{$IFDEF SMARTINSPECT}
  SiMain.LogMessage(CompleteText(Text));
{$ENDIF}
end;

procedure CreateLog(LogLevel:Integer;Dir,Prefix,Ext:string); overload;
begin
{$IFDEF SMARTINSPECT}
  if LogLevel>0 then begin
    if (Pos('file',gConnections)>0) and (Pos('%s',gConnections)>0) then
      Si.Connections:=Format(gConnections,[IncludeTrailingBackSlash(Dir)+Prefix+FormatDateTime('yymmdd_hhmmss', Now)])
    else Si.Connections:=gConnections;
    Si.Enabled:=True;
    if not Islibrary  then
      SiMain.EnterProcess(ParamStr(0));
  end;
{$ENDIF}
end;

function GetModuleFilename:string;
begin
  SetLength(result,255);
  SetLength(result,Windows.GetModuleFileName(hInstance,@result[1],255));
end;

function GetDebugPath:string;
var
  reg : TRegistry;
begin
  reg:=TRegistry.Create(KEY_QUERY_VALUE);
  try
    reg.RootKey:=HKEY_LOCAL_MACHINE;
    if reg.OpenkeyReadOnly('\Software\' + PRODOWNER_KEY) and Reg.ValueExists('DebugPath') then
      result:=IncludeTrailingBackSlash(Reg.ReadString('DebugPath'))
    else result:=ExtractFilePath(GetModuleFilename);
  finally
    reg.free;
  end;
end;

procedure CreateLog(AEnabled:Boolean);
{$IFDEF SMARTINSPECT}

    procedure FlushMem(Filename:string);
    var
      LStream : TFileStream;
      Delete: Boolean;
    begin
      try
        LStream := TFileStream.Create(Filename, fmCreate);
        try
          Si.Dispatch('mem', 0, LStream);
          Delete := LStream.Size = 0;
        finally
          LStream.Free;
        end;
        if Delete then DeleteFile(Filename);
      except end;
    end;

    function EnableLog(Section: string; var LogDir: string): Boolean;
    var
      Ini: TIniFile;
      S: string;
    begin
      Ini := TIniFile.Create(GetDebugPath + Product+'.ini');
      try
        Result := StrToBoolDef(Ini.ReadString('DEBUG', 'Enabled', 'False'), False) or
                  StrToBoolDef(Ini.ReadString(Section, 'Enabled', 'False'), False);
        S := Trim(Ini.ReadString('DEBUG', 'LogDir', ''));
        if S <> '' then LogDir := S;
        S := Trim(Ini.ReadString(Section, 'LogDir', ''));
        if S <> '' then LogDir := S;
        if Result then begin
          gConnections:=Ini.ReadString('DEBUG', 'Connections', 'file(filename="%s.sil")');
          gConnections:=Ini.ReadString(Section, 'Connections', gConnections);
          Si.Level:=TSiLevel(Ini.ReadInteger('DEBUG', 'Level', Integer(lvDebug)));
          Si.Level:=TSiLevel(Ini.ReadInteger(Section, 'Level', Integer(Si.Level)));
          Si.DefaultLevel:=TSiLevel(Ini.ReadInteger('DEBUG', 'DefaultLevel', Integer(lvDebug)));
          Si.DefaultLevel:=TSiLevel(Ini.ReadInteger(Section, 'DefaultLevel', Integer(Si.DefaultLevel)));
          if StrToBoolDef(Ini.ReadString('DEBUG','Flush','False'),False) or
            StrToBoolDef(Ini.ReadString(Section,'Flush','False'),False) then begin
            S := ExtractFileExt(Section);
            if not SameText(S, '.exe') then
              Section := Section + '_' + ExtractFileName(Application.ExeName);
            FlushMem(IncludeTrailingBackSlash(LogDir)+Section+FormatDateTime('_yymmdd_hhmmss', Now)+'.sil');
          end;
        end;
      finally
        Ini.Free;
      end;
    end;
{$ENDIF}
var
  Prefix, LogDir, S: string;
  B: Boolean;
begin
{$IFDEF SMARTINSPECT}
{$IFDEF FILE_MONITOR}
  if not Assigned(gLogMon) then
  begin
    gLogMon := TLogMonitor.Create;
    gLogMon.Start;
    Exit;
  end;
{$ENDIF}
  LogDir := GetDebugPath;
  Prefix := ExtractFileName(GetModuleFilename);
  B := AEnabled or EnableLog(Prefix, LogDir);
  if B then
  begin
    S := ExtractFileExt(Prefix);
    SetLength(Prefix, Length(Prefix) - Length(S));
    if not SameText(S, '.exe') then
    begin
      Prefix := Prefix + '_' + ExtractFileName(Application.ExeName);
//    end else begin
//    if SameText(Prefix, 'SsSgLite') then
//      Prefix := Prefix + '_PID_' + IntToStr(GetCurrentProcessId mod 10000) + '_';
    end;
    CreateLog(9, LogDir, Prefix + '_', 'log');
    gLogEnabled := True;
  end else begin
    gLogEnabled := False;
    CreateLog(0, LogDir, Prefix + '_', 'log');
  end;
  gLogDir := LogDir;
{$ENDIF}
end;

{ TLogger }

procedure TLogger.EnterMethod;
begin
{$IFDEF SMARTINSPECT}
  SiMain.EnterMethod(FModule+'.'+FMethod);
{$ENDIF}
end;

procedure TLogger.LeaveMethod;
begin
{$IFDEF SMARTINSPECT}
  SiMain.LeaveMethod(FModule+'.'+FMethod);
{$ENDIF}
end;

constructor TLogger.create(Sender:TObject;AMethod: string;IsParentObject:Boolean=False);
begin
  inherited create;
  if IsParentObject then begin
    ParentObject:=Sender;
    ParentObjectIndentLevel:=IndentLevel;
  end;

  FModule:=CompleteText(Sender.ClassName);
  FMethod:=AMethod;
  EnterMethod;
  Indent;
end;

constructor TLogger.Create(Sender:TObject);
begin
  inherited create;
  ParentObject:=Sender;
  ParentObjectIndentLevel:=IndentLevel;
  FModule:=Sender.ClassName;
  FMethod:=CompleteText('Execute');
  FThread:=Sender is TThread;
{$IFDEF SMARTINSPECT}
  if FThread then
    SiMain.EnterThread(FModule)
  else EnterMethod;
{$ENDIF}
  Indent;
end;

constructor TLogger.create(AModule,AMethod: string);
begin
  inherited create;
  FModule:=CompleteText(AModule);
  FMethod:=AMethod;
  EnterMethod;
  Indent;
end;

destructor TLogger.Destroy;
var E: Pointer; A: Pointer; Buffer: string;
begin
  try
    {$IFDEF ACQUIRE_EXCEPTION}
    E := AcquireExceptionObject;
    A := ExceptAddr;
    ReleaseExceptionObject;
    {$ELSE}
    E := ExceptObject;
    A := ExceptAddr;
    {$ENDIF}
    if E <> nil then
    begin
      try
        {$IFDEF CLR}
        Buffer:=ExceptionErrorMessage(E);
        {$ELSE}
        gLock.Enter;
        try
          Buffer:=StringOfChar(#0,2048);
          if A = nil then Buffer:='Access violation at 0000' else
          begin
            ExceptionErrorMessage(E, A, PChar(Buffer),2048);
            Buffer := Trim(Buffer);
          end;
        finally
          gLock.Leave;
        end;
          {$ENDIF}
        {$IFDEF SMARTINSPECT}
        SiMain.LogException(CompleteText(Format('Message: %s',[Buffer])));
        {$ENDIF}
      except
      end;
    end;

    Unindent;
    if FThread then
    {$IFDEF SMARTINSPECT}
      SiMain.LeaveThread(FModule)
    {$ENDIF}
    else LeaveMethod;
  except

  end;
  inherited;
end;

procedure TLogger.Dump(const Title: string; Ptr: Pointer; Len: Integer);
begin
  {$IFDEF SMARTINSPECT}
  SiMain.LogMemory(Title,Ptr,Len);
  {$ENDIF}
end;

function TLogger.GetHeader(Code:string): string;
begin
  result:=(Format('%s Module: %s Method: %s',[Code,FModule,FMethod]));
end;

procedure TLogger.LogDebug(Text: string);
begin
  LogMsg(llDebugProc,CompleteText(Text));
end;

procedure TLogger.LogInfo(Text: string);
begin
  LogMsg(llDebugInfo,CompleteText(Text));
end;

procedure TLogger.LogError(Text: string);
begin
  LogMsg(llError,CompleteText(Text));
end;

procedure TLogger.LogException(Text: string);
begin
  {$IFDEF SMARTINSPECT}
  SiMain.LogException(CompleteText(Text));
  {$ENDIF}
end;

procedure TLogger.LogMsg(Level: TLogLevel; Text: string);
var S : string;
begin
  {$IFDEF SMARTINSPECT}
  S:=StringOfChar(' ',(IndentLevel)*4)+Text;
  case Level of
    llError: SiMain.LogError(Text);
    llWarning: SiMain.LogWarning(Text);
    llDebugProc : SiMain.LogDebug(Text);
    else SiMain.LogMessage(Text);
  end;
  {$ENDIF}
end;

procedure TLogger.ODS(S: string);
begin
  LogMsg(llDebugInfo,S);
end;

initialization
  gLock := TCriticalSection.Create;
  gLogDir := GetDebugPath;
finalization
   FreeAndNil(gLock);
{$IFDEF FILE_MONITOR}
  FreeAndNil(gLogMon);
{$ENDIF}
end.
