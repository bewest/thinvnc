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

unit ThinVnc.Cursor;
{$I ThinVnc.DelphiVersion.inc}
{$I ThinVnc.inc}
interface

uses Windows,Classes,Sysutils,Graphics,PngImage,ThinVnc.Utils,math,Forms,Controls;

type
  TRGBLine = array[Word] of TRGBTriple;
  PRGBLine = ^TRGBLine;
  TRGBALine = array[Word] of TRGBQuad;
  PRGBALine = ^TRGBALine;

  TCursorWindow = class
  private
    FImage : TIcon;
    FPos,FHotspot : TPoint;
    FMoved : Boolean;
    FLastHwnd : Integer;
    FShapeChanged : Boolean;
    FHandleCursor : THandle;
  public
    constructor Create;
    destructor Destroy;Override;
    function getJson: string;
    procedure GetCursor(var cur:string;var cX,cY:Integer);
    function Capture(reset: Boolean): Boolean;
    property ShapeChanged:Boolean read FShapeChanged;
  end;

procedure ConvertToPNG(Source: TGraphic; out Dest: TPNGObject);
function GetBitmapFromCursor(CursorHandle:THandle):TBitmap;
implementation

function GetBitmapFromCursor(CursorHandle:THandle):TBitmap;
// efg, www.efg2.com/Lab, February 2001
var
  Temp    :  TBitmap;
begin
  result := TBitmap.Create;

  result.Width := 32;
  result.Height := 32;
  result.PixelFormat := pf24bit;  // avoid working with palettes

  // Draw cursor on canvas of CursorBitmap
  DrawIconEx(result.Canvas.Handle, 0,0, CursorHandle, 32,32, 0,0, DI_NORMAL);

  // Flood fill cursor from the "outside" so a white cursor will
  // appear white.  Assume right-top position will work for floodfilling
  // the whole area.  Assume RGB(250,250,250) will work as transparency color.
  result.Canvas.Brush.Color := RGB(250,250,250);
  result.Canvas.FloodFill(31,0, clWhite, fsSurface);

  // Kludge fix for 3DMove cursor since upper left corner is "blocked"
  // during flood fill.
  result.Canvas.FloodFill(0,0, clWhite, fsSurface);

  result.TransparentMode := tmFixed;
  result.TransparentColor := RGB(250,250,250);
  result.Transparent := TRUE;
end;

procedure ConvertToPNG(Source: TGraphic; out Dest: TPNGObject);
var
  MaskLines: array of pngimage.PByteArray;

  function CompareColors(const Color1: TRGBTriple; const Color2: TColor): Boolean;
  begin
    Result := (Color1.rgbtBlue = Color2 shr 16 and $FF) and
      (Color1.rgbtGreen = Color2 shr 8 and $FF) and
      (Color1.rgbtRed = Color2 and $FF);
  end;

  function ColorToTriple(const Color: TColor): TRGBTriple;
  begin
    Result.rgbtBlue := Color shr 16 and $FF;
    Result.rgbtGreen := Color shr 8 and $FF;
    Result.rgbtRed := Color and $FF;
  end;

  procedure GetAlphaMask(SourceColor: TBitmap);
  type
    TBitmapInfo = packed record
      bmiHeader: TBitmapV4Header;
      //Otherwise I may not get per-pixel alpha values.
      bmiColors: array[0..0] of TRGBQuad;
    end;
  var
    Bits: PRGBALine;
    BitmapInfo: TBitmapInfo;
    I, X, Y: Integer;
    HasAlpha: Boolean;
    BitsSize: Integer;
  begin
    BitsSize := 4 * SourceColor.Width * SourceColor.Height;
    Bits := AllocMem(BitsSize);
    try
      ZeroMemory(Bits, BitsSize);
      ZeroMemory(@BitmapInfo, SizeOf(BitmapInfo));
      BitmapInfo.bmiHeader.bV4Size := SizeOf(BitmapInfo.bmiHeader);
      BitmapInfo.bmiHeader.bV4Width := SourceColor.Width;
      BitmapInfo.bmiHeader.bV4Height := -SourceColor.Height;
      //Otherwise the image is upside down.
      BitmapInfo.bmiHeader.bV4Planes := 1;
      BitmapInfo.bmiHeader.bV4BitCount := 32;
      BitmapInfo.bmiHeader.bV4V4Compression := BI_BITFIELDS;
      BitmapInfo.bmiHeader.bV4SizeImage := BitsSize;

      if GetDIBits(SourceColor.Canvas.Handle, SourceColor.Handle, 0,
        SourceColor.Height, Bits, Windows.PBitmapInfo(@BitmapInfo)^,
        DIB_RGB_COLORS) > 0 then begin
        //Because Win32 API is a piece of crap when it comes to icons, I have to check
        //whether an has an alpha-channel the hard way.
        HasAlpha := False;
        for I := 0 to (SourceColor.Height * SourceColor.Width) - 1 do begin
          if Bits[I].rgbReserved <> 0 then begin
            HasAlpha := True;
            Break;
          end;
        end;
        if HasAlpha then begin
          //OK, so not all alpha-values are 0, which indicates the existence of an
          //alpha-channel.
          I := 0;
          for Y := 0 to SourceColor.Height - 1 do
            for X := 0 to SourceColor.Width - 1 do begin
              MaskLines[Y][X] := Bits[I].rgbReserved;
              Inc(I);
            end;
        end;
      end;
    finally
      FreeMem(Bits, BitsSize);
    end;
  end;

  function WinXPOrHigher: Boolean;
  var
    Info: TOSVersionInfo;
  begin
    Info.dwOSVersionInfoSize := SizeOf(Info);
    GetVersionEx(Info);
    Result := (Info.dwPlatformId = VER_PLATFORM_WIN32_NT) and
      ((Info.dwMajorVersion > 5) or
      ((Info.dwMajorVersion = 5) and (Info.dwMinorVersion >= 1)));
  end;

var
  Temp, SourceColor, SourceMask: TBitmap;
  X,X2, Y: Integer;
  Line: PRGBLine;
  MaskLine, AlphaLine: pngimage.PByteArray;
  TransparentColor, CurrentColor: TColor;
  IconInfo: TIconInfo;
  AlphaNeeded: Boolean;
begin
  //A PNG does not have to be converted
  if Source is TPNGObject then begin
    Dest := TPNGObject.Create;
    Dest.Assign(Source);
    Exit;
  end;

  AlphaNeeded := False;
  Temp := TBitmap.Create;
  SetLength(MaskLines, Source.Height);
  for Y := 0 to Source.Height - 1 do begin
    MaskLines[Y] := AllocMem(Source.Width);
    FillMemory(MaskLines[Y], Source.Width, 255);
  end;
  try
    //Initialize intermediate color bitmap
    Temp.Width := Source.Width;
    Temp.Height := Source.Height;
    Temp.PixelFormat := pf24bit;

    //Now figure out the transparency
    if Source is TBitmap then begin
      if Source.Transparent then begin
        //TBitmap is just about comparing the drawn colors against the TransparentColor
        if TBitmap(Source).TransparentMode = tmFixed then
          TransparentColor := TBitmap(Source).TransparentColor
        else
          TransparentColor := TBitmap(Source).Canvas.Pixels[0, Source.Height - 1];

        for Y := 0 to Temp.Height - 1 do begin
          Line := Temp.ScanLine[Y];
          MaskLine := MaskLines[Y];
          for X := 0 to Temp.Width - 1 do begin
            CurrentColor := GetPixel(TBitmap(Source).Canvas.Handle, X, Y);
            if CurrentColor = TransparentColor then begin
              MaskLine^[X] := 0;
              AlphaNeeded := True;
            end;
            Line[X] := ColorToTriple(CurrentColor);
          end;
        end;
      end
      else begin
        Temp.Canvas.Draw(0, 0, Source);
      end;
    end
    else if Source is TIcon then begin
      //TIcon is more complicated, because there are bitmasked (classic) icons and
      //alphablended (modern) icons. Not to forget about the "inverse" color.
      GetIconInfo(TIcon(Source).Handle, IconInfo);
      SourceColor := TBitmap.Create;
      SourceMask := TBitmap.Create;
      try
        SourceColor.Handle := IconInfo.hbmColor;
        SourceMask.Handle := IconInfo.hbmMask;
        if SourceColor.Handle>0 then begin
          Temp.Canvas.Draw(0, 0, SourceColor);
          for Y := 0 to Temp.Height - 1 do begin
            MaskLine := MaskLines[Y];
            for X := 0 to Temp.Width - 1 do begin
              if GetPixel(SourceMask.Canvas.Handle, X, Y) <> 0 then begin
                MaskLine^[X] := 0;
                AlphaNeeded := True;
              end;
            end;
          end;
          if (GetDeviceCaps(SourceColor.Canvas.Handle, BITSPIXEL) = 32) and WinXPOrHigher then begin
            //This doesn't neccesarily mean we actually have 32bpp in the icon, because the
            //bpp of an icon is always the same as the display settings, regardless of the
            //actual color depth of the icon :(
            AlphaNeeded := True;
            GetAlphaMask(SourceColor);
          end;
        end else DrawIconEx(Temp.Canvas.Handle, 0,0, TIcon(Source).Handle, 32,32, 0,0, DI_NORMAL);
        //This still doesn't work for alphablended icons...
      finally
        SourceColor.Free;
        SourceMask.Free
      end;
    end;

    //And finally, create the destination PNG image
    Dest := TPNGObject.Create;
    Dest.Assign(Temp);
    if AlphaNeeded then begin
      Dest.CreateAlpha;
      for Y := 0 to Dest.Height - 1 do begin
        AlphaLine := Dest.AlphaScanline[Y];
        CopyMemory(AlphaLine, MaskLines[Y], Temp.Width);
      end;
    end;

  finally
    for Y := 0 to Source.Height - 1 do
      FreeMem(MaskLines[Y], Source.Width);
    Temp.Free;
  end;
end;

{ TCursorWindow }

constructor TCursorWindow.Create;
begin
end;

destructor TCursorWindow.Destroy;
begin
  FreeAndNil(FImage);
  inherited;
end;

procedure TCursorWindow.GetCursor(var cur: string; var cX, cY: Integer);
var
  hCursor : smallint;
  n : Integer;
  ci : TCursorInfo;
begin
  cX:=CI.ptScreenPos.X;
  cY:=CI.ptScreenPos.Y;
  cur:='default';
  CI.cbSize := SizeOf(CI);
  GetCursorInfo(CI);

  for n := crDefault downto crSizeAll do begin
    if Screen.Cursors[n]=CI.hCursor then begin
      case n of
        crDefault,
        crNone,
        crArrow       : cur:='default';
        crCross       : cur:='crosshair';
        crIBeam       : cur:='text';
        crSizeNESW    : cur:='ne-resize';
        crSizeNS      : cur:='s-resize';
        crSizeNWSE    : cur:='se-resize';
        crSizeWE      : cur:='e-resize';
        crUpArrow     : cur:='n-resize';
        crHourGlass   : cur:='wait';
        crSQLWait     : cur:='progress';
        crAppStart    : cur:='progress';
        crHelp        : cur:='help';
        crHandPoint   : cur:='pointer';
        crSizeAll     : cur:='move';
        crNo          : cur:='not-allowed';
        crNoDrop      : cur:='no-drop';
        crHSplit      : cur:='row-resize';
        crVSplit      : cur:='col-resize';
        else cur:='default';
      end;
      break;
    end;
  end;
end;

function TCursorWindow.Capture(reset:Boolean):Boolean;
var
  pt : TPoint;
  ci : TCursorInfo;
begin
  FMoved:=false;
  FShapeChanged:=false;
  ci.cbSize := SizeOf(ci);
  if GetCursorInfo(ci) then begin
    if reset or (FPos.X<>ci.ptScreenPos.X) or
       (FPos.Y<>ci.ptScreenPos.Y) then begin
      FMoved:=true;
      FPos:=ci.ptScreenPos;
    end;
    FShapeChanged:=reset or (ci.hCursor<>FHandleCursor);
    FHandleCursor:=ci.hCursor;
  end;
  result:=FMoved or FShapeChanged;
end;

function TCursorWindow.getJson: string;
var
  img,imgs : string;
  ms : TMemoryStream;
  Size : Integer;
  FileIconMem : Pointer;
  CI: TCursorInfo;
  IconInfo : TIconInfo;
  png : TPNGObject;
  bmp : TBitmap;
begin
  if FShapeChanged then
    FLastHwnd:=(FLastHwnd mod 10)+1;

  result:=Format('{ "hwnd":"%d","zidx":%d,"left":%d,"top":%d,"width":%d,"height":%d',
                    [FLastHwnd,255,FPos.X-FHotspot.X,FPos.Y-FHotspot.Y,32,32]);

  if FShapeChanged then
  try
    CI.cbSize := SizeOf(CI);
    if GetCursorInfo(CI) and (CI.Flags = CURSOR_SHOWING) then begin
      ms:=TMemoryStream.Create;
      try
        GetIconInfo(CI.hCursor, IconInfo);
        FHotspot.X:=IconInfo.xHotspot;
        FHotspot.Y:=IconInfo.yHotspot;
        bmp:=GetBitmapFromCursor(CI.hCursor);
        try
          bmp.TransparentMode:=tmAuto;
          ConvertToPNG(bmp,png);
          try
            png.SaveToStream(ms);
          finally
            png.Free;
          end;
        finally
          bmp.free;
        end;
        img:=Format('data:image/png;base64,%s',[EncodeBase64(ms)]);
      finally
        ms.Free;
      end;
      imgs:=Format('{ "x":%d,"y":%d,"w":%d,"h":%d,"img": "%s" }',[0,0,32,32,img]);
      result:=result+Format(',"imgs": [%s]}',[imgs])
    end else result:=result+'}';
  except
  end else result:=result+'}';
end;


end.
