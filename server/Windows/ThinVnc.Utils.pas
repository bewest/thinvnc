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

unit ThinVnc.Utils;
{$I ThinVnc.DelphiVersion.inc}
{$I ThinVnc.inc}
interface

uses Windows,Messages,Classes,pngimage,Graphics,SysUtils,jpeg,TLHelp32,
  SyncObjs,WinSock,Forms,ShlObj,
  ThinVnc.Log;

const
  EncodeTable: array[0..63] of AnsiChar =
    AnsiString('ABCDEFGHIJKLMNOPQRSTUVWXYZ') +
    AnsiString('abcdefghijklmnopqrstuvwxyz') +
    AnsiString('0123456789+/');
  CAPTUREBLT = $40000000;

type
  TCallback = procedure of object;
  TCancelCallback = procedure (var Cancel:Boolean) of object;
  TWaitCallback = procedure (pi:TProcessInformation) of object;
  TRectArray = array of TRect;
  TMyJPEGImage = class(TJPEGImage);

  TLangAndCodePage = record
    wLanguage: WORD;
    wCodePage: WORD;
  end;
  PLangAndCodePage = ^TLangAndCodePage;

  TFixedFileInfo = record
    dwSignature: DWORD;
    dwStrucVersion: DWORD;
    dwFileVersionMS: DWORD;
    dwFileVersionLS: DWORD;
    dwProductVersionMS: DWORD;
    dwProductVersionLS: DWORD;
    dwFileFlagsMask: DWORD;
    dwFileFlags: DWORD;
    dwFileOS: DWORD;
    dwFileType: DWORD;
    dwFileSubtype: DWORD;
    dwFileDateMS: DWORD;
    dwFileDateLS: DWORD;
  end;
  PFixedFileInfo = ^TFixedFileInfo;

  TVersionInfo = packed record
    wLength: WORD;
    wValueLength: WORD;
    wType: WORD;
    szKey: array [0..13] of Char;
    Value: TFixedFileInfo;
  end;
  PVersionInfo = ^TVersionInfo;

  TVersionInformation = class(TObject)
  private
    FFileName: string;
    FVersionInfoSize: Integer;
    FVersionInfo: Pointer;
    FUseSysDir: Boolean;
    procedure SetFileName(const Value: string);
    function GetMajorVersion: Integer;
    function GetMinorVersion: Integer;
    function GetVersionItem(Index: Integer): string;
  protected
    procedure Clear;
    property VerionInfo: Pointer read FVersionInfo;
  public
    destructor Destroy; override;
    property UseSysDir: Boolean read FUseSysDir write FUseSysDir;
    property MajorVersion: Integer read GetMajorVersion;
    property MinorVersion: Integer read GetMinorVersion;
    property FileName: string read FFileName write SetFileName;
    property CompanyName: string index 0 read GetVersionItem;
    property FileDescription: string index 1 read GetVersionItem;
    property FileVersion: string index 2 read GetVersionItem;
    property InternalName: string index 3 read GetVersionItem;
    property LegalCopyright: string index 4 read GetVersionItem;
    property OriginalFilename: string index 5 read GetVersionItem;
    property ProductName: string index 6 read GetVersionItem;
    property ProductVersion: string index 7 read GetVersionItem;
  end;

function Height(ARect:TRect):integer;
function Width(ARect:TRect):integer;
function GetTempPath:string;
function GetModuleFilename:string;
function GetModulePath:string;
function GetWindowClassName(wnd:THandle):string;
function EncodeBase64(const Input: TStream): AnsiString; overload;
function EncodeBase64(const Input: AnsiString): AnsiString; overload;
procedure CopyBmpToPng(var Png : TPNGObject; Bmpo,Bmpm: TBitmap;R:TRect);
procedure CopyBmpToJpg(var Jpg : TJpegImage; Bmp: TBitmap;R:TRect;Pf : TPixelFormat);
function CopyBmpToBmp(Bmp: TBitmap;R:TRect;Pf : TPixelFormat):TBitmap;
function CompareBitmaps(Bitmap1,Bitmap2:Graphics.TBitmap;var R:TRect):Boolean;
function GetDiffRects(Bitmap1,Bitmap2:Graphics.TBitmap;rbmp : TRect):TRectArray;
function EqualBitmaps(Bitmap1,Bitmap2:Graphics.TBitmap):Boolean;
function IsWin98: Boolean;
function IsWinNT: Boolean;
function IsWinXP: Boolean;
function IsVistaOrGreater: Boolean;
function IsVista: Boolean;
function SwitchToActiveDesktop:string;

function WaitForEvent(AEvent:TEvent;ATimeout:Cardinal;CancelCallback:TCancelCallback=nil):TWaitResult;
procedure ProcessMessages;
function ComputerName: string;

function DVRect(Index:Integer=-1):TRect;
function DVWidth(Index:Integer=-1):Integer;
function DVHeight(Index:Integer=-1):Integer;
function DVLeft(Index:Integer=-1):Integer;
function DVTop(Index:Integer=-1):Integer;

implementation

var
  BlockSize : Integer = 0;
  Delta : Integer = 0;

type
  PPacket = ^TPacket;
  TPacket = packed record
    case Integer of
      0: (b0, b1, b2, b3: Byte);
      1: (i: Integer);
      2: (a: array[0..3] of Byte);
      3: (c: array[0..3] of AnsiChar);
  end;

procedure EncodePacket(const Packet: TPacket; NumChars: Integer; OutBuf: PAnsiChar);
begin
  OutBuf[0] := EnCodeTable[Packet.a[0] shr 2];
  OutBuf[1] := EnCodeTable[((Packet.a[0] shl 4) or (Packet.a[1] shr 4)) and $0000003f];
  if NumChars < 2 then
    OutBuf[2] := '='
  else OutBuf[2] := EnCodeTable[((Packet.a[1] shl 2) or (Packet.a[2] shr 6)) and $0000003f];
  if NumChars < 3 then
    OutBuf[3] := '='
  else OutBuf[3] := EnCodeTable[Packet.a[2] and $0000003f];
end;

procedure EncodeStream(Input, Output: TStream);
type
  PInteger = ^Integer;
var
  InBuf: array[0..509] of Byte;
  OutBuf: array[0..1023] of AnsiChar;
  BufPtr: PAnsiChar;
  I, J, K, BytesRead: Integer;
  Packet: TPacket;
begin
  K := 0;
  repeat
    BytesRead := Input.Read(InBuf, SizeOf(InBuf));
    I := 0;
    BufPtr := OutBuf;
    while I < BytesRead do
    begin
      if BytesRead - I < 3 then
        J := BytesRead - I
      else J := 3;
      Packet.i := 0;
      Packet.b0 := InBuf[I];
      if J > 1 then
        Packet.b1 := InBuf[I + 1];
      if J > 2 then
        Packet.b2 := InBuf[I + 2];
      EncodePacket(Packet, J, BufPtr);
      Inc(I, 3);
      Inc(BufPtr, 4);
      Inc(K, 4);
    end;
    Output.Write(Outbuf, BufPtr - PChar(@OutBuf));
  until BytesRead = 0;
end;

function EncodeBase64(const Input: TStream): AnsiString;
var
  OutStr: TMemoryStream;
begin
  OutStr := TMemoryStream.Create;
  try
    Input.Seek(0,0);
    EncodeStream(Input, OutStr);
    SetString(Result, PAnsiChar(OutStr.Memory), OutStr.Size);
  finally
    OutStr.Free;
  end;
end;

function EncodeBase64(const Input: AnsiString): AnsiString; overload;
var Stm: TStringStream;
begin
  Stm := TStringStream.Create(Input);
  try
    Result := EncodeBase64(Stm);
  finally
    Stm.Free;
  end;
end;

function CreateBitmap(w,H:Integer;pf:TPixelFormat=pf32bit): TBitmap;
begin
  result := TBitmap.Create;
  result.PixelFormat:=pf;
  result.Width:=w;
  result.Height:=h;
end;

function GetTempPath:string;
begin
  SetLength(result,255);
  SetLength(result,Windows.GetTempPath(255,@result[1]));
end;

function GetModuleFilename:string;
begin
  SetLength(result,255);
  SetLength(result,Windows.GetModuleFileName(hInstance,@result[1],255));
end;

function GetModulePath:string;
begin
  result:=ExtractFilePath(GetModuleFilename);
end;

function Height(ARect:TRect):integer;
begin
  result:=ARect.Bottom-ARect.Top;
end;

function Width(ARect:TRect):integer;
begin
  result:=ARect.Right-ARect.Left;
end;

type
  TTrible = packed record
    Blue:Byte;
    Green:Byte;
    Red: Byte;
  end;
  TAjQuad = packed record
    Blue:Byte;
    Green:Byte;
    Red:Byte;
    Alpha:Byte;
  end;
  PTTrible = ^TTrible;
  PTAjQuad = ^TAjQuad;

procedure CopyBmpToPng(var Png : TPNGObject; Bmpo,Bmpm: TBitmap;R:TRect);
var H,W,y,x: Integer;
  Trb : PTTrible;
  Alph : PByte;
  QBmp1 : PTAjQuad;
  QBmpx : PTAjQuad;
  QBmp2 : PTAjQuad;
  bmp1,bmp2 : TBitmap;
begin
  H := Height(R);
  W := Width(R);
  bmp2:=CreateBitmap(Width(R),Height(R));
  try
    Windows.BitBlt(bmp2.canvas.handle, 0, 0, Width(R), Height(R), BmpM.canvas.handle,
         R.Left, R.Top, SRCCOPY);
      Png.CompressionLevel:=9;
    Png.Assign(Bmp2);
    {$IFDEF PNG_TRANSPARENT}
    bmp1:=CreateBitmap(Width(R),Height(R));
    try
      Windows.BitBlt(bmp1.canvas.handle, 0, 0, Width(R), Height(R), BmpO.canvas.handle,
            R.Left, R.Top, SRCCOPY);

      Png.CreateAlpha();
      for y := 0 to H-1 do begin
        Trb := Png.Scanline[y];
        Alph := PByte(Png.AlphaScanline[y]);
        QBmp1 := Bmp1.ScanLine[y];
        QBmp2 := Bmp2.ScanLine[y];
        for x := 0 to W-1 do begin
          if (QBmp1.Red=QBmp2.Red) and
             (QBmp1.Green=QBmp2.Green) and
             (QBmp1.Blue=QBmp2.Blue) and
             (QBmp1.Alpha=QBmp2.Alpha) then begin
              Trb^.Red := 0;
              Trb^.Green := 0;
              Trb^.Blue := 0;
              Alph^ := 0;
          end else begin
            Trb^.Red := QBmp2.Red;
            Trb^.Green := QBmp2.Green;
            Trb^.Blue := QBmp2.Blue;
            Alph^ := 255;//QBmp.Alpha;
          end;
          Trb:=PTTrible(PAnsiChar(Trb)+SizeOf(TTrible));
          Alph:=PByte(PAnsiChar(Alph)+SizeOf(Byte));
          QBmp1:=PTAjQuad(PAnsiChar(QBmp1)+SizeOf(TAjQuad));
          QBmp2:=PTAjQuad(PAnsiChar(QBmp2)+SizeOf(TAjQuad));
        end;
      end;
    finally
      bmp1.Free;
    end;
    {$ENDIF}
  finally
    bmp2.Free;
  end;
end;

function CopyBmpToBmp(Bmp: TBitmap;R:TRect;Pf : TPixelFormat):TBitmap;
begin
  result:=CreateBitmap(Width(R),Height(R),Pf);
  Windows.BitBlt(result.canvas.handle, 0, 0, Width(R), Height(R), Bmp.canvas.handle,
        R.Left, R.Top, SRCCOPY);
end;

procedure CopyBmpToJpg(var Jpg : TJpegImage; Bmp: TBitmap;R:TRect;Pf : TPixelFormat);
var
  bmp1 : TBitmap;
begin
  bmp1:=CopyBmpToBmp(Bmp,R,pf);
  try
    Jpg.Assign(bmp1);
  finally
    bmp1.Free;
  end;
end;

function CompareBitmaps(Bitmap1,Bitmap2:Graphics.TBitmap;var R:TRect):Boolean;
var
  i : INTEGER;
  j,k : INTEGER;
  deep : Integer;
  aux : Integer;
  pitch : Integer;
  addr1,addr2 : pansichar;
  bmpwidth : integer;
begin
  result:=(Bitmap1.Height<>Bitmap2.Height) or
           (Bitmap1.Width<>Bitmap2.Width) or
           (Bitmap1.PixelFormat<>Bitmap2.PixelFormat) or
           (Bitmap1.Height<=1);
  if result then begin
    R:=Rect(0,0,Bitmap1.Width,Bitmap1.Height);
    exit;
  end;

  case Bitmap1.PixelFormat of
    pf1bit: deep:=1;
    pf4bit: deep:=1;
    pf8bit: deep:=1;
    pf15bit: deep:=2;
    pf16bit: deep:=2;
    pf24bit: deep:=3;
    pf32bit: deep:=4;
  end;

  pitch:=Integer(Bitmap1.Scanline[0])-Integer(Bitmap1.Scanline[1]);
  addr1:=pansichar(Bitmap1.Scanline[Bitmap1.Height-1]);
  addr2:=pansichar(Bitmap2.Scanline[Bitmap2.Height-1]);
  bmpwidth:=Bitmap1.Width*deep;

  r:=Rect(Bitmap1.Width+1,Bitmap1.Height+1,0,0);
  For j := Bitmap1.Height-1 downto 0 do begin
    if not CompareMem(addr1,addr2,bmpwidth) then begin
      if r.Bottom<j then r.Bottom:=j;
      r.Top:=j;
      for I := Bitmap1.Width - 1 downto 0 do
        if not CompareMem(addr1+I*deep,
                      addr2+I*deep, deep) then begin
          if r.Right<I then r.Right:=I;
          if r.Left>I then r.Left:=I;
          for k := 0 to r.Left-1 do begin
            if not CompareMem(addr1+k*deep,
                          addr2+k*deep, deep) then begin
               if r.Left>k then r.Left:=k;
               break;
            end;
          end;
          Break;
        end;
    end;
    inc(addr1,pitch);
    inc(addr2,pitch);
  end;
  result:=r.Top<=Bitmap1.Height;
  if result then begin
    Inc(r.Right,1);
    Inc(r.Bottom,1);
    if r.Right>Bitmap1.Width then r.Right:=Bitmap1.Width;
    if r.Bottom>Bitmap1.Height then r.Bottom:=Bitmap1.Height;
  end;
end;

function GetDiffRects(Bitmap1,Bitmap2:Graphics.TBitmap;rbmp : TRect):TRectArray;
var
  i : INTEGER;
  j,k,n : INTEGER;
  deep : Integer;
  aux : Integer;
  pitch : Integer;
  addr1,addr2 : pansichar;
  bmpwidth : integer;
  r : PRect;
  b,newrect : Boolean;
begin
  try
    SetLength(result,0);
    b:=(Bitmap1.Height<>Bitmap2.Height) or
             (Bitmap1.Width<>Bitmap2.Width) or
             (Bitmap1.PixelFormat<>Bitmap2.PixelFormat) or
             (Bitmap1.Width=1) and (Bitmap1.Height=1);
    if b then begin
      SetLength(result,1);
      Result[0]:=Rect(0,0,Bitmap1.Width,Bitmap1.Height);
      exit;
    end;

    case Bitmap1.PixelFormat of
      pf1bit: deep:=1;
      pf4bit: deep:=1;
      pf8bit: deep:=1;
      pf15bit: deep:=2;
      pf16bit: deep:=2;
      pf24bit: deep:=3;
      pf32bit: deep:=4;
    end;

    pitch:=Integer(Bitmap1.Scanline[0])-Integer(Bitmap1.Scanline[1]);
    addr1:=pansichar(Bitmap1.Scanline[rbmp.bottom-1]);
    addr2:=pansichar(Bitmap2.Scanline[rbmp.bottom-1]);
    bmpwidth:=Width(rbmp)*deep;

    r:=nil;
    NewRect:=True;
    For j := rbmp.Bottom-1 downto rbmp.top do begin
      if not CompareMem(addr1+rbmp.Left*deep,addr2+rbmp.Left*deep,bmpwidth) then begin
        if newrect then begin
          SetLength(result,Length(result)+1);
          r:=@Result[Length(result)-1];
          r^:=Rect(rbmp.right+1,rbmp.bottom+1,rbmp.left,rbmp.top);
          NewRect:=false;
        end;
        if r.Bottom<j then r.Bottom:=j;
        r.Top:=j;
        for I := rbmp.Right-1 downto rbmp.left do
          if not CompareMem(addr1+I*deep,
                        addr2+I*deep, deep) then begin
            if r.Right<I then r.Right:=I;
            if r.Left>I then r.Left:=I;
            for k := rbmp.left to r.Left-1 do begin
              if not CompareMem(addr1+k*deep,
                            addr2+k*deep, deep) then begin
                 if r.Left>k then r.Left:=k;
                 break;
              end;
            end;
            Break;
          end;
      end else newrect:=true;
      inc(addr1,pitch);
      inc(addr2,pitch);
    end;

    if Length(result)=0 then exit;

    for i:=0 to Length(result)-1 do
      InflateRect(Result[i],1,1);
  except
    On E:Exception do begin
      LogException('GetDiffRects');
      LogInfo(Format('GetDiffRects Rbmp: (%d,%d,%d,%d)',[rbmp.Left,rbmp.Top,rbmp.Right,rbmp.Bottom]));
      LogInfo(Format('GetDiffRects Bitmap1 Width:%d Height:%d)',[Bitmap1.Width,Bitmap1.Height]));
      SetLength(result,1);
      Result[0]:=Rect(0,0,Bitmap1.Width,Bitmap1.Height);
    end;
  end;
end;

function EqualBitmaps(Bitmap1,Bitmap2:Graphics.TBitmap):Boolean;
var
  i : INTEGER;
  j,k : INTEGER;
  deep : Integer;
  aux : Integer;
  pitch : Integer;
  addr1,addr2 : PAnsiChar;
  bmpwidth : integer;
begin
  result:=(Bitmap1.Height<>Bitmap2.Height) or
           (Bitmap1.Width<>Bitmap2.Width) or
           (Bitmap1.PixelFormat<>Bitmap2.PixelFormat);
  if result then exit;

  case Bitmap1.PixelFormat of
    pf1bit: deep:=1;
    pf4bit: deep:=1;
    pf8bit: deep:=1;
    pf15bit: deep:=2;
    pf16bit: deep:=2;
    pf24bit: deep:=3;
    pf32bit: deep:=4;
  end;

  pitch:=Integer(Bitmap1.Scanline[1])-Integer(Bitmap1.Scanline[0]);
  addr1:=PAnsiChar(Bitmap1.Scanline[Bitmap1.Height-1])+pitch;
  addr2:=PAnsiChar(Bitmap2.Scanline[Bitmap2.Height-1])+pitch;
  bmpwidth:=Bitmap1.Width*deep;

  For j := Bitmap1.Height-1 downto 0 do begin
    dec(addr1,pitch);
    dec(addr2,pitch);
    if not CompareMem(addr1,addr2,bmpwidth) then begin
      result:=true;
      exit;
    end;
  end;
end;

function GetWindowClassName(wnd:THandle):string;
begin
  result:=StringOfChar(#0,255);
  SetLength(result,windows.GetClassName(wnd,@result[1],255));
end;

function IsWin98: Boolean;
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_WINDOWS;
end;

function IsWinNT: Boolean;
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and
    ((Win32MajorVersion < 5) or
     (Win32MajorVersion = 5) and (Win32MinorVersion = 0));
end;

function IsWinXP: Boolean;
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and
    ((Win32MajorVersion = 5) and (Win32MinorVersion > 0) or
     (Win32MajorVersion > 5));
end;

function IsVista: Boolean;
begin
  result:=Win32MajorVersion=6;
end;

function IsVistaOrGreater: Boolean;
begin
  result:=Win32MajorVersion>=6;
end;

{ TVersionInformation }

const
  VER_STRINGCOUNT = 8;
  VersionNames: array [0..VER_STRINGCOUNT - 1] of string =
    ('CompanyName',
     'FileDescription',
     'FileVersion',
     'InternalName',
     'LegalCopyright',
     'OriginalFilename',
     'ProductName',
     'ProductVersion');

destructor TVersionInformation.Destroy;
begin
  Clear;
end;

procedure TVersionInformation.Clear;
begin
  if Assigned(FVersionInfo) then
  begin
    FreeMem(FVersionInfo);
    FVersionInfo := nil;
    FVersionInfoSize := 0;
  end;
end;

procedure TVersionInformation.SetFileName(const Value: string);
var
  Handle: THandle;
  SysDir: PChar;
begin
  Clear;
  SysDir := StrAlloc(MAX_PATH);
  GetSystemDirectory(SysDir, MAX_PATH - 1);
  if FUseSysDir then
    FFileName := SysDir + '\' + Value
  else
    FFileName := Value;
  FVersionInfoSize := GetFileVersionInfoSize(PChar(FFileName), Handle);
  if FVersionInfoSize <> 0 then
  begin
    GetMem(FVersionInfo, FVersionInfoSize);
    GetFileVersionInfo(PChar(FFileName), Handle, FVersionInfoSize, FVersionInfo);
  end;
  StrDispose(SysDir);
end;

function TVersionInformation.GetMajorVersion: Integer;
begin
  Result := 0;
  if (FVersionInfo <> nil) and (PVersionInfo(FVersionInfo).wValueLength <> 0) then
    Result := LongRec(PVersionInfo(FVersionInfo).Value.dwFileVersionMS).Hi;
end;

function TVersionInformation.GetMinorVersion: Integer;
begin
  Result := 0;
  if (FVersionInfo <> nil) and (PVersionInfo(FVersionInfo).wValueLength <> 0) then
    Result := LongRec(PVersionInfo(FVersionInfo).Value.dwFileVersionMS).Lo;
end;

function TVersionInformation.GetVersionItem(Index: Integer): string;
var
  Buffer: Pointer;
  BufferSize: Cardinal;
  CodePage: PLangAndCodePage absolute Buffer;
  S: string;
begin
  Result := '';
  if (FVersionInfo <> nil) and (VerQueryValue(FVersionInfo,
    '\VarFileInfo\Translation', Buffer, BufferSize)) then
  begin
    S := Format('\StringFileInfo\%.4x%.4x\%s', [CodePage.wLanguage,
      CodePage.wCodePage, VersionNames[Index]]);
    if VerQueryValue(FVersionInfo, PChar(S), Buffer, BufferSize) then
      Result := PChar(Buffer);
  end;
end;

const
  DESKTOP_ALL = DESKTOP_CREATEMENU
             or DESKTOP_CREATEWINDOW
             or DESKTOP_ENUMERATE
             or DESKTOP_HOOKCONTROL
             or DESKTOP_WRITEOBJECTS
             or DESKTOP_READOBJECTS
             or DESKTOP_SWITCHDESKTOP
             or GENERIC_WRITE;
var
  home_window_station:HWINSTA;
  CS : TCriticalSection;

function WinStationEnumProc(name:LPSTR; param:LPARAM):BOOL; stdcall;
var
  station:HWINSTA;
  oldstation:HWINSTA;
  flags:USEROBJECTFLAGS;
  tmp:Cardinal;
begin
  try
    station := OpenWindowStationA(name, FALSE, GENERIC_ALL);
    oldstation := GetProcessWindowStation;
    tmp:=0;
    if not GetUserObjectInformationA(station, UOI_FLAGS, @flags, sizeof(flags), tmp) then
      Result:=True
    else begin
      if (flags.dwFlags and WSF_VISIBLE)<>0 then begin
        if (SetProcessWindowStation(station)) then begin
          if (oldstation <> home_window_station) then
            CloseWindowStation(oldstation);
          Result:=False; // success !!!
        end else begin
          CloseWindowStation(station);
          Result:=True;
        end;
      end else Result:=True;
    end;
  except
    Result:=True;
  end;
end;

procedure SelectInputWinStation;
var
  flags:USEROBJECTFLAGS;
  tmp:Cardinal;
begin
  home_window_station := 0;
  try
    tmp:=0;
    home_window_station := GetProcessWindowStation;
    if not GetUserObjectInformationA(home_window_station, UOI_FLAGS, @flags, sizeof(flags), tmp) or
       ((flags.dwFlags and WSF_VISIBLE)=0) then begin
      if EnumWindowStations(@WinStationEnumProc, 0) then
        home_window_station := 0;
    end;
  except
    home_window_station := 0;
  end;
end;

procedure SelectHomeWinStation;
var
  station:HWINSTA;
begin
  if home_window_station<>0 then begin
    station:=GetProcessWindowStation();
    SetProcessWindowStation(home_window_station);
    CloseWindowStation(station);
  end;
end;

function SwitchToActiveDesktop:string;
var
  LogonDesktop,CurDesktop:HDESK;
  lpnLengthNeeded: DWORD;
begin
  CS.Enter;
  try

    SelectInputWinStation;

    LogonDesktop := OpenInputDesktop(DF_ALLOWOTHERACCOUNTHOOK, False, DESKTOP_ALL);

    CurDesktop := GetThreadDesktop(GetCurrentThreadID);

    SetLength(result,255);
    if GetUserObjectInformation(LogonDesktop,UOI_NAME,@result[1],255,lpnLengthNeeded) then 
      result:=StrPas(PChar(result));

    if (LogonDesktop<>0) and (LogonDesktop<>CurDesktop) then begin
//      LogDebug('Switching to Desktop: '+result);
      SetThreadDesktop(LogonDesktop);
      CloseDesktop(CurDesktop);
    end;

  finally
    CS.Leave;
  end;
end;


procedure ProcessMessages;
var Msg:TMsg;
begin
  try
    if PeekMessage(Msg,0,0,0,0) then begin
      GetMessage(Msg,0,0,0);
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
  except

  end;
end;

function WaitForEvent(AEvent:TEvent;ATimeout:Cardinal;CancelCallback:TCancelCallback):TWaitResult;
var InitialTime : TDateTime;
  Elapsed : Integer;
  Cancel : Boolean;
begin
  InitialTime:=Now;
  Elapsed:=0;
  result:=wrTimeOut;
  if Assigned(CancelCallback) then CancelCallback(Cancel);
  While(ATimeout>Cardinal(Elapsed)) do begin
    result:=AEvent.WaitFor(10);
    case result of
      wrTimeOut: begin
          if Assigned(CancelCallback) then begin
            Cancel:=False;
            CancelCallback(Cancel);
            if Cancel then break;
          end;
          ProcessMessages;
          Elapsed:=DateTimeToTimeStamp(Now-InitialTime).Time;
        end;
      wrSignaled : break;
      wrError : break;
    end;
  end;
end;

function ComputerName: string;
var
  Buffer: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  Size: Cardinal;
begin
  Size := SizeOf(Buffer);
  FillChar(Buffer, Size, 0);
  Windows.GetComputerName(Buffer, Size);
  Result := StrPas(Buffer);
end;

function IsThisComputer(DestAddr: string): Boolean;
    function IsLocalName: Boolean;
  var
    Addr: Integer;
    Host: PHostEnt;
  begin
    Result := False;
    Addr := inet_addr(PChar(DestAddr));
    if (Addr = INADDR_NONE) then
    begin
      Host := gethostbyname(PChar(DestAddr));
      if Host <> nil then
        Addr := PInAddr(Host.h_addr_list^)^.S_addr;
    end;
      end;
begin
  Result := (DestAddr='') or
            SameText(DestAddr, 'localhost') or
            SameText(DestAddr, ComputerName) or
            SameText(DestAddr, '127.0.0.1') or
            IsLocalName;
end;

function DVRect(Index:Integer=-1):TRect;
begin
  result:=Rect(DVLeft(Index),DVTop(Index),DVWidth(Index)+DVLeft(Index),DVHeight(Index)+DVTop(Index));
end;

function DVWidth(Index:Integer=-1):Integer;
begin
  if Index=-1 then
    result:=Screen.DesktopWidth
  else result:=Screen.Monitors[Index].Width;
end;

function DVHeight(Index:Integer=-1):Integer;
begin
  if Index=-1 then
    result:=Screen.DesktopHeight
  else result:=Screen.Monitors[Index].Height;
end;

function DVLeft(Index:Integer=-1):Integer;
begin
  if Index=-1 then
    result:=Screen.DesktopLeft
  else result:=Screen.Monitors[Index].Left;
end;

function DVTop(Index:Integer=-1):Integer;
begin
  if Index=-1 then
    result:=Screen.DesktopTop
  else result:=Screen.Monitors[Index].Top;
end;

function GetSpecialFolderPath(CSIDL:integer):string;
var path:string;
begin
  path:=stringofChar(#0,1024);
  ShlObj.SHGetSpecialFolderPath(0,PChar(path),CSIDL,False);
  result:=Trim(path);
end;


initialization
  CS:=TCriticalSection.Create;
finalization
  Cs.Free;
end.
