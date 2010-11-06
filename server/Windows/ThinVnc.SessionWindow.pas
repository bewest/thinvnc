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

unit ThinVnc.SessionWindow;
{$I ThinVnc.DelphiVersion.inc}
{$I ThinVnc.inc}
interface
uses Windows,Classes,SysUtils,Graphics,DateUtils,
  jpeg,math,strutils, Contnrs,
  pngimage,
{.$DEFINE DEBUG_BMP}
  ThinVnc.Log,
  ThinVnc.Utils,ThinVnc.MirrorWindow;

{$DEFINE CAPTURE_CHANGEDRGN}

type
  TSessionWindow = class;
  TBmpPart = class
  private
    FSessionWindow:TSessionWindow;
    FRect : TRect;
    FID : string;
    FBitmap : TBitmap;
    FStream : TMemoryStream;
    FImageType : string;
  public
    constructor Create(ASessionWindow:TSessionWindow;ARect:TRect;Bmp:TBitmap);overload;
    destructor Destroy;override;
    function GetStream(ImageMethod:TImageMethod;JpgQuality:Integer;
        JpgPixelFormat:TJpegPixelFormat;JpgGrayScale:Boolean):TStream;
    property Stream :TMemoryStream read FStream;
    property ID:string read FId;
  end;

  TBmpPartList = class(TObjectList)
  private
    function GetItems(Index: Integer): TBmpPart;
  public
    function Add(Item: TBmpPart): Integer;
    property Items[Index: Integer]: TBmpPart read GetItems; default;
  end;

  TSessionWindow = class
  private
    FMirrorWindow  : TMirrorWindow;
    FDiffBmpList   : TBmpPartList;
    FImageType     : string;
    FMoved         : TDatetime;
    FImage         : TBitmap;
    function  GetActive: Boolean;
    function GetBoundsRect: TRect;
    function GetHandle: THandle;
    function GetzIndex: Integer;
    function GetScraper: TAbstractScraper;
    function GetProcessId: Integer;
    function GetClassname: string;
    procedure ReCreateImage;
    function CreateBlackImage: TBitmap;
  public
    constructor Create(AMirrorWindow:TMirrorWindow);
    destructor  Destroy;override;
    procedure   ApplyDifferences;
    procedure   UpdateBitmap;
    function    IsMasked: Boolean;
    function    getJson(path:string;EmbeddedImage,BinaryImages:Boolean; ImageMethod: TImageMethod;
                        JpgQuality:Integer;JpgPixelFormat:TJpegPixelFormat;
                        JpgGrayScale:Boolean): string;
    function    CaptureDifferences(AChangedRgn:HRGN;ActiveMonitor:Integer;reset:boolean=false): Boolean;
    function    IsShellWindow:Boolean;
    property    Scraper:TAbstractScraper read GetScraper;
    property    BoundsRect:TRect read GetBoundsRect;
    property    Active:Boolean read GetActive;
    property    zIndex:Integer read GetzIndex;
    property    Handle:THandle read GetHandle;
    property    DiffBmpList : TBmpPartList read FDiffBmpList;
    property    ProcessId:Integer read GetProcessId;
    property    Classname:string read GetClassname;
  end;

  TSessionWindowList = class(TObjectList)
  private
    function GetItems(Index: Integer): TSessionWindow;
  public
    function  Add(Item: TSessionWindow): Integer;
    property  Items[Index: Integer]: TSessionWindow read GetItems; default;
  end;

implementation

{ TBmpPart }

constructor TBmpPart.Create(ASessionWindow:TSessionWindow;ARect: TRect; Bmp: TBitmap);
begin
  FSessionWindow := ASessionWindow;
  FRect:=ARect;
  FBitmap:=Bmp;
  FImageType := 'jpeg';
end;

destructor TBmpPart.Destroy;
begin
  FreeAndNil(FStream);
  FreeAndNil(FBitmap);
  inherited;
end;

{function TBmpPart.MaxPngSize(W,H : Integer):Integer;
begin
//
end;
}
function TBmpPart.GetStream(ImageMethod:TImageMethod;JpgQuality:Integer;
        JpgPixelFormat:TJpegPixelFormat;JpgGrayScale:Boolean): TStream;
  
  function BytesPerPixel(pf: TPixelFormat): integer;
  begin
    case pf of
      pf1bit,
      pf4bit,
      pf8bit:  Result := 1;
      pf15bit,
      pf16bit: Result := 2;
      pf24bit: Result := 3;
      else Result := 4;
    end;
  end;

  procedure DumpPerformance(type_:string;start:TDatetime;size:Integer);
  var
    ratio : Integer;
  begin
    ratio:=100-size *100 div (FBitmap.Width*FBitmap.Height*BytesPerPixel(FBitmap.PixelFormat));
    LogInfo(Format('%s Size:%d Osize:%d Ratio:%d time:%.3d',[type_,size,(FBitmap.Width*FBitmap.Height*BytesPerPixel(FBitmap.PixelFormat)),ratio,DateTimeToTimeStamp(Now-Start).Time]));
  end;

  function SaveToPng:Integer;
  var
    Png : TPNGObject;
    Start : TDatetime;
  begin
    FStream.Clear;
    Start:=Now;
    png:=TPNGObject.Create;
    try
      if JpgPixelFormat=jf8Bit then
        CopyBmpToPng(png,FBitmap,Rect(0,0,FBitmap.Width,FBitmap.Height),pf8bit)
      else png.Assign(FBitmap);
      png.SaveToStream(FStream);
      result:=FStream.Size;
      FImageType:='png';
    finally
      png.Free;
    end;
    DumpPerformance('PNG',Start,result);
  end;

  function SaveToJpeg:Integer;
  var
    Jpg : jpeg.TJpegImage;
    Start : TDatetime;
  begin
    FStream.Clear;
    Start:=Now;
  
    Jpg := jpeg.TJpegImage.Create;
    try
      Jpg.CompressionQuality:=JpgQuality;
      Jpg.PixelFormat:=jf24Bit;
      Jpg.GrayScale:=JpgGrayScale;
      if JpgPixelFormat=jf8Bit then begin
        CopyBmpToJpg(Jpg,FBitmap,Rect(0,0,FBitmap.Width,FBitmap.Height),pf8bit);
      end else Jpg.Assign(FBitmap);
      FImageType:='jpeg';
      Jpg.SaveToStream(FStream);
    finally
      Jpg.Free;
    end;
    result:=FStream.Size;
    DumpPerformance('JPG',Start,result);
  end;

  function BitmapDataPtr(const Image: TBitmap): pointer;
  begin
    With Image do
      Result := ScanLine[Height-1];
  end;


var
  s : AnsiString;
  Count : Integer;
begin
  if not Assigned(FStream) then
    FStream:=TMemoryStream.Create;

  Count:=FBitmap.Width*FBitmap.Height;
  case ImageMethod of
    imJpeg: try
              SaveToJpeg;
            except
              SaveToPng;
            end;
    imPng: SaveToPng;
  end;

  result:=FStream;
end;

{ TSessionWindow }

constructor TSessionWindow.Create(AMirrorWindow: TMirrorWindow);
begin
  FMirrorWindow :=AMirrorWindow;
  ReCreateImage;
  FDiffBmpList := TBmpPartList.Create;
end;

destructor TSessionWindow.Destroy;
begin
  FreeAndNil(FDiffBmpList);
  FreeAndNil(FImage);
  inherited;
end;

function TSessionWindow.GetBoundsRect:TRect;
begin
  result:=FMirrorWindow.BoundsRect;
end;

function TSessionWindow.GetClassname: string;
begin
  result:=FMirrorWindow.Classname;
end;

function TSessionWindow.GetHandle: THandle;
begin
  result:=FMirrorWindow.Handle;
end;

function TSessionWindow.GetActive: Boolean;
begin
  result:=FMirrorWindow.Active;
end;

function TSessionWindow.GetzIndex: Integer;
begin
  result:=FMirrorWindow.zIndex;
end;

function TSessionWindow.IsShellWindow: Boolean;
begin
  result:=(FMirrorWindow.Classname='Progman') or
      (FMirrorWindow.Classname='Button') or
      (FMirrorWindow.Classname='Shell_TrayWnd');
end;

function TSessionWindow.GetScraper: TAbstractScraper;
begin
  result:=FMirrorWindow.Scraper;
end;

function TSessionWindow.GetProcessId: Integer;
begin
  result:=FMirrorWindow.ProcessId;
end;

procedure TSessionWindow.UpdateBitmap;
var
  NewImage : TBitmap;
  h,w : Integer;
  rc:boolean;
begin
  if (Width(BoundsRect)<=0) or (Height(BoundSRect)<=0) or not assigned(FImage) then exit;

  if ((Width(BoundsRect)<>FImage.Width) or (Height(BoundSRect)<>FImage.Height)) then begin
    NewImage := FMirrorWindow.CreateBitmap;
    NewImage.Canvas.Lock;
    try
      FImage.Canvas.Lock;
      try
        Bitblt(NewImage.Canvas.Handle,
           0,0,Min(FImage.Width,FImage.Width),Min(FImage.Height,NewImage.Height),
            FImage.Canvas.handle, 0,0,SRCCOPY);
      finally
        FImage.Canvas.Unlock;
      end;
    finally
      NewImage.Canvas.Unlock;
    end;
    FImage.Free;
    FImage:=NewImage;
  end;
end;

procedure TSessionWindow.ApplyDifferences;
var
  I,n: Integer;
  BmpPart : TBmpPart;
  brush : TBrush;
begin
  if (FDiffBmpList.Count=0) and (FMirrorWindow.BitmapCache=nil) and Assigned(FImage) then begin
    FreeAndNil(FImage);
    FDiffBmpList.Clear;
    exit;
  end;

  if FDiffBmpList.Count=0 then exit;

  if not Assigned(FImage) then FImage:=CreateBlackImage;
  
  for I := 0 to FDiffBmpList.Count -1 do
  begin
     BmpPart := FDiffBmpList[I];
     with BmpPart do begin
       FImage.Canvas.Lock;
       try
         FBitmap.Canvas.Lock;
         try
           for n:=0 to 10 do begin
            if Bitblt(FImage.Canvas.Handle,
              FRect.Left,FRect.Top,Width(FRect),Height(FRect),
              FBitmap.Canvas.handle, 0,0,SRCCOPY) then break;
            Sleep(10);
           end;
         finally
           FBitmap.Canvas.Unlock;
         end;
       finally
         FImage.Canvas.Unlock;
       end;
{$IFDEF DEBUG_BMP}
       LogBitmap(Format('ImagePart Handle:%d R(%d,%d,%d,%d)',[FMirrorWindow.Handle,FRect.Left,FRect.Top,Width(FRect),Height(FRect)]),FBitmap);
//       LogBitmap(Format('Image Handle:%d',[FMirrorWindow.Handle]),FImage);
{$ENDIF}
     end;
  end;
  FDiffBmpList.Clear;
end;

function TSessionWindow.CreateBlackImage:TBitmap;
begin
  result:=FMirrorWindow.CreateBitmap;
  result.Canvas.Lock;
  try
    FillRect(result.Canvas.Handle,Rect(0,0,result.Width,result.Height),GetStockObject(BLACK_BRUSH));
  finally
    result.Canvas.Unlock;
  end;
end;

procedure TSessionWindow.ReCreateImage;
begin
  FreeAndNil(FImage);
  FImage:=CreateBlackImage;
end;

function TSessionWindow.CaptureDifferences(AChangedRgn:HRGN;ActiveMonitor:Integer;reset: boolean): Boolean;
var
  TmpImage : TBitmap;
  Jpg : TJpegImage;
  ra,ra2 : TRectArray;
  n,m : Integer;
  tmpbmp : TBitmap;
  rbmp : TRect;
  R : TRect;
  Rgn : HRGN;
  aux : string;
  BmpPart : TBmpPart;
begin
  result:=False;
  FDiffBmpList.Clear;

  if Scraper.UseChangedRgn and (AChangedRgn=0) and not reset then exit;
//  IntersectRect(R,BoundsRect,Scraper.DVRect(ActiveMonitor));
  R:=BoundsRect;
  if (Width(R)*Height(R)=0) or IsIconic(Handle) then exit;

  TmpImage:=FMirrorWindow.BitmapCache;
  if not Assigned(TmpImage) then exit;

  if reset then ReCreateImage;

  OffsetRect(R,-BoundsRect.Left,-BoundsRect.Top);

{$IFDEF CAPTURE_CHANGEDRGN}
  if not reset and (AChangedRgn<>0) then begin
    Rgn:=CreateRectRgnIndirect(BoundsRect);
    try
      CombineRgn(Rgn,Rgn,AChangedRgn,RGN_AND);
      OffsetRgn(Rgn,-BoundsRect.Left,-BoundsRect.Top);
      CombineRgn(Rgn,Rgn,FMirrorWindow.ClipRgn,RGN_AND);
      ra:=ExtractClippingRegions(Rgn,R);
    finally
      DeleteObject(Rgn);
    end;
  end else
{$ENDIF}
    ra:=ExtractClippingRegions(FMirrorWindow.ClipRgn,R);

  result:=(Length(ra)>0);
  for n := 0 to Length(ra) - 1 do begin
    ra2:=GetDiffRects(FImage,TmpImage,ra[n]);
    result:=result or (Length(ra2)>0);
    for m := 0 to Length(ra2) - 1 do begin
      rbmp := ra2[m];
      IntersectRect(rbmp,rbmp,R);
      if IsRectEmpty(rbmp) then continue;

      // problems with jpeg encoding
      if Height(rbmp)=1 then begin
        if rbmp.Top>0 then Dec(rbmp.Top,1)
        else Inc(rbmp.bottom,1);
      end;

      BmpPart:=TBmpPart.Create(Self,rbmp,CopyBmpToBmp(TmpImage,rbmp,TmpImage.PixelFormat));
      FDiffBmpList.Add(BmpPart);
    end;
  end;

  result:=FDiffBmpList.Count>0;
end;

function TSessionWindow.IsMasked:Boolean;
begin
  result:=(FMirrorWindow.Masked);
end;

function TSessionWindow.getJson(path: string; EmbeddedImage,BinaryImages:Boolean; ImageMethod: TImageMethod;
  JpgQuality: Integer; JpgPixelFormat: TJpegPixelFormat;
  JpgGrayScale: Boolean): string;
var
  n : Integer;
  img,imgs : string;
  bmpp : TBmpPart;
begin
  result:=Format('{ "hwnd":"%d","zidx":%d,"left":%d,"top":%d,"width":%d,"height":%d,"mask":%s',
                    [Handle,zIndex,BoundsRect.Left,BoundsRect.Top,
                              Width(BoundsRect),Height(BoundsRect),Lowercase(BoolToStr(IsMasked,true))]);

  imgs:='';
  for n:=0 to FDiffBmpList.Count-1 do begin
    bmpp:=FDiffBmpList[n];
    bmpp.GetStream(ImageMethod,JpgQuality,JpgPixelFormat,JpgGrayScale);
 //     if (FMirrorWindow.Classname='Progman') then
 //       LogBitmap(FMirrorWindow.Classname,bmpp.FBitmap);

    if EmbeddedImage and not BinaryImages then
      img:=Format('data:image/%s;base64,%s',[bmpp.FImageType,EncodeBase64(bmpp.FStream)])
    else begin
      with bmpp.FRect do begin
        bmpp.FID:=Format('%.3d-%d-%d-%d-%d-%d-%d.%s',[Scraper.CaptureIndex,
              Handle,Left,Top,Right,Bottom,Integer(bmpp),bmpp.FImageType]);
        img:=Format('%s/img?id=%s',[path,bmpp.FID]);
      end;
    end;

    imgs:=Format('%s %s{ "x":%d,"y":%d,"w":%d,"h":%d,"img": "%s" }',[imgs,ifthen(n>0,',',''),
          bmpp.FRect.Left,bmpp.FRect.Top,width(bmpp.FRect),Height(bmpp.FRect),img]);
  end;

  if FDiffBmpList.Count>0 then
    result:=result+Format(',"imgs": [%s]}',[imgs])
  else result:=result+'}';
  if not BinaryImages then ApplyDifferences;
end;

{ TBmpPartList }

function TBmpPartList.Add(Item: TBmpPart): Integer;
begin
  Result := inherited Add(Item);
end;

function TBmpPartList.GetItems(Index: Integer): TBmpPart;
begin
  Result := TBmpPart(inherited Items[Index]);
end;

{ TSessionWindowList }

function TSessionWindowList.Add(Item: TSessionWindow): Integer;
begin
  Result := inherited Add(Item);
end;

function TSessionWindowList.GetItems(Index: Integer): TSessionWindow;
begin
  Result := TSessionWindow(inherited Items[Index]);
end;

end.
