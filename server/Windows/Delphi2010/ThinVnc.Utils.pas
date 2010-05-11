unit ThinVnc.Utils;

(*:@author Gustavo Ricardi
   @desc <pre>

This software is distributed under the GPL license.

Copyright (c) 2010, Gustavo Ricardi
All rights reserved.
*)

interface
uses Windows,Classes,pngimage,Graphics,SysUtils,jpeg;

const
  EncodeTable: array[0..63] of AnsiChar =
    AnsiString('ABCDEFGHIJKLMNOPQRSTUVWXYZ') +
    AnsiString('abcdefghijklmnopqrstuvwxyz') +
    AnsiString('0123456789+/');
  CAPTUREBLT = $40000000;

type
  TRectArray = array of TRect;
  TMyJPEGImage = class(TJPEGImage);

function Height(ARect:TRect):integer;
function Width(ARect:TRect):integer;
function GetModulePath:string;
function GetWindowClassName(wnd:THandle):string;
function EncodeBase64(const Input: TStream): AnsiString;
procedure CopyBmpToPng(var Png : TPngImage; Bmpo,Bmpm: TBitmap;R:TRect);
procedure CopyBmpToJpg(var Jpg : TJpegImage; Bmp: TBitmap;R:TRect);
function CopyBmpToBmp(Bmp: TBitmap;R:TRect;Pf : TPixelFormat):TBitmap;
function CompareBitmaps(Bitmap1,Bitmap2:Graphics.TBitmap;var R:TRect):Boolean;
function GetDiffRects(Bitmap1,Bitmap2:Graphics.TBitmap;rbmp : TRect):TRectArray;
function EqualBitmaps(Bitmap1,Bitmap2:Graphics.TBitmap):Boolean;

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
  OutStr: TBytesStream;
begin
  OutStr := TBytesStream.Create;
  try
    Input.Seek(0,0);
    EncodeStream(Input, OutStr);
    SetString(Result, PAnsiChar(OutStr.Memory), OutStr.Size);
  finally
    OutStr.Free;
  end;
end;

function CreateBitmap(w,H:Integer;pf:TPixelFormat=pf32bit): TBitmap;
begin
  result := TBitmap.Create;
  result.PixelFormat:=pf;
  result.Width:=w;
  result.Height:=h;
end;

function GetModulePath:string;
begin
  SetLength(result,255);
  SetLength(result,Windows.GetModuleFileName(hInstance,@result[1],255));
  result:=ExtractFilePath(result);
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

procedure CopyBmpToPng(var Png : TPngImage; Bmpo,Bmpm: TBitmap;R:TRect);
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

procedure CopyBmpToJpg(var Jpg : TJpegImage; Bmp: TBitmap;R:TRect);
var
  bmp1 : TBitmap;
  pf : TPixelFormat;
begin
  pf:=pf24bit;
  if Jpg.PixelFormat = jf8Bit then pf:=pf8bit;

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
  SetLength(result,0);
  b:=(Bitmap1.Height<>Bitmap2.Height) or
           (Bitmap1.Width<>Bitmap2.Width) or
           (Bitmap1.PixelFormat<>Bitmap2.PixelFormat);
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

end.
