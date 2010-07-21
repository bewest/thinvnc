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
  jpeg,pngimage,math,strutils, Contnrs, GIFImg,
{.$DEFINE DEBUG_BMP}
  ThinVnc.Log,
  ThinVnc.Utils,ThinVnc.MirrorWindow;

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
    function GetStream(usejpeg:Boolean;JpgQuality:Integer;
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
    function GetMirrorManager: TMirrorManager;
    function GetProcessId: Integer;
    function GetClassname: string;
    procedure ReCreateImage;
    function CreateBlackImage: TBitmap;
  public
    constructor Create(AMirrorWindow:TMirrorWindow);
    destructor  Destroy;override;
    procedure   ApplyDifferences;
    procedure   UpdateBitmap;
    function    getJson(path:string;EmbeddedImage,BinaryImages,UseJpeg:Boolean;
                        JpgQuality:Integer;JpgPixelFormat:TJpegPixelFormat;
                        JpgGrayScale:Boolean): string;
    function    CaptureDifferences(ActiveMonitor:Integer;reset:boolean=false): Boolean;
    function    IsShellWindow:Boolean;
    property    MirrorManager:TMirrorManager read GetMirrorManager;
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
    function Add(Item: TSessionWindow): Integer;
    property Items[Index: Integer]: TSessionWindow read GetItems; default;
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


function TBmpPart.GetStream(usejpeg: Boolean;JpgQuality:Integer;
        JpgPixelFormat:TJpegPixelFormat;JpgGrayScale:Boolean): TStream;
  var
  Jpg : jpeg.TJpegImage;
  Png : TPNGObject;
//  Log : Ilogger;
  Start1,Start2 : TDatetime;
  s : AnsiString;
  h : Integer;
begin
//  Log := TLogger.Create(Self,'GetStream');
  if not Assigned(FStream) then begin
    FStream:=TMemoryStream.Create;
    if usejpeg then begin
      Start1:=Now;
      Jpg := jpeg.TJpegImage.Create;
      try
        Jpg.CompressionQuality:=JpgQuality;
        Jpg.PixelFormat:=jf24Bit;
        Jpg.GrayScale:=JpgGrayScale;
        if JpgPixelFormat=jf8Bit then begin
          CopyBmpToJpg(Jpg,FBitmap,Rect(0,0,FBitmap.Width,FBitmap.Height),pf8bit);
        end else Jpg.Assign(FBitmap);
        FImageType:='jpeg';
        try
          Jpg.SaveToStream(FStream);
        except
          On E:Exception do begin
            FBitmap.SaveToStream(FStream);
            FImageType:='bmp';
          end;
        end;
      finally
        Jpg.Free;
      end;
{$IFDEF DEBUG_BMP}
      LogBitmap(Format('Image',[Integer(Self)]),FBitmap);
{$ENDIF}
    end else begin
      png:=TPNGObject.Create;
      try
        png.Assign(FBitmap);
        png.SaveToStream(FStream);
      finally
        png.Free;
      end;
    end;
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

function TSessionWindow.GetMirrorManager: TMirrorManager;
begin
  result:=FMirrorWindow.MirrorManager;
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
  if ((Width(BoundsRect)<>FImage.Width) or (Height(BoundSRect)<>FImage.Height)) then begin
    NewImage := FMirrorWindow.CreateBitmap;
    Bitblt(NewImage.Canvas.Handle,
         0,0,Min(FImage.Width,FImage.Width),Min(FImage.Height,NewImage.Height),
        FImage.Canvas.handle, 0,0,SRCCOPY);
    FImage.Free;
    FImage:=NewImage;
  end;
end;

procedure TSessionWindow.ApplyDifferences;
var
  I: Integer;
  BmpPart : TBmpPart;
begin
  if FDiffBmpList.Count=0 then exit;

  for I := 0 to FDiffBmpList.Count -1 do
  begin
     BmpPart := FDiffBmpList[I];
     with BmpPart do begin
       Bitblt(FImage.Canvas.Handle,
            FRect.Left,FRect.Top,Width(FRect),Height(FRect),
            FBitmap.Canvas.handle, 0,0,SRCCOPY);
     end;
  end;
  FDiffBmpList.Clear;
end;

function TSessionWindow.CreateBlackImage:TBitmap;
var
  brush : TBrush;
begin
  result:=FMirrorWindow.CreateBitmap;
  brush:=TBrush.Create;
  try
    brush.Color:=clBlack;
    brush.Style:=bsSolid;
    FillRect(result.Canvas.Handle,Rect(0,0,result.Width,result.Height),brush.Handle);
  finally
    brush.free;
  end;
end;

procedure TSessionWindow.ReCreateImage;
begin
  FreeAndNil(FImage);
  FImage:=CreateBlackImage;
end;

function TSessionWindow.CaptureDifferences(ActiveMonitor:Integer;reset: boolean): Boolean;
var
  ObjIndex: Integer;
  TmpImage : TBitmap;
//  png : TPNGObject;
  Jpg : TJpegImage;
  ra,ra2 : TRectArray;
  n,m : Integer;
  tmpbmp : TBitmap;
  rbmp : TRect;
  R : TRect;
  FreeTmpImage : Boolean;
begin
  result:=False;
  FDiffBmpList.Clear;

  IntersectRect(R,BoundsRect,DVRect(ActiveMonitor));
  if Width(R)*Height(R)=0 then exit;
  if not IsWindowVisible(Handle) or IsIconic(Handle) then exit;

  FreeTmpImage:=False;
  ObjIndex := MirrorManager.LastCapture.IndexOf(IntToStr(Handle));
  if ObjIndex < 0 then begin
//  if MirrorManager.IsProcessExcluded(FMirrorWindow.ProcessId) then begin
    TmpImage:=CreateBlackImage;
    FreeTmpImage:=True;
  end else TmpImage:=MirrorManager.LastCapture.Objects[ObjIndex] as TBitmap;

  try
    if reset then begin
      ReCreateImage;
      SetLength(ra,1);
    end;

    OffsetRect(R,-BoundsRect.Left,-BoundsRect.Top);

    ra:=FMirrorWindow.ExtractClippingRegions(R);
    result:=(Length(ra)>0);
    for n := 0 to Length(ra) - 1 do begin
      rbmp := ra[n];
      ra2:=GetDiffRects(FImage,TmpImage,ra[n]);
      result:=result or (Length(ra2)>0);
      for m := 0 to Length(ra2) - 1 do begin
        rbmp := ra2[m];
        IntersectRect(rbmp,rbmp,R);
        if IsRectEmpty(rbmp) then continue;

        FDiffBmpList.Add(TBmpPart.Create(Self,rbmp,CopyBmpToBmp(TmpImage,rbmp,TmpImage.PixelFormat)));
      end;
    end;
    result:=FDiffBmpList.Count>0;
  finally
    if FreeTmpImage then TmpImage.Free;    
  end;
end;

function TSessionWindow.getJson(path: string; EmbeddedImage,BinaryImages, UseJpeg: Boolean;
  JpgQuality: Integer; JpgPixelFormat: TJpegPixelFormat;
  JpgGrayScale: Boolean): string;
var
  n : Integer;
  img,imgs : string;
  bmpp : TBmpPart;
begin
  result:=Format('{ "hwnd":"%d","zidx":%d,"desktop":"%s","left":%d,"top":%d,"width":%d,"height":%d',
                    [Handle,zIndex,FMirrorWindow.DesktopName,BoundsRect.Left,BoundsRect.Top,
                              Width(BoundsRect),Height(BoundsRect)]);

  imgs:='';
  for n:=0 to FDiffBmpList.Count-1 do begin
    MirrorManager.ConfirmUpdate;
    bmpp:=FDiffBmpList[n];
    bmpp.GetStream(UseJpeg,JpgQuality,JpgPixelFormat,JpgGrayScale);

    if EmbeddedImage and not BinaryImages then
      img:=Format('data:image/%s;base64,%s',[bmpp.FImageType,EncodeBase64(bmpp.FStream)])
    else begin
      with bmpp.FRect do begin
        bmpp.FID:=Format('%.3d-%d-%d-%d-%d-%d-%d.%s',[MirrorManager.CaptureIndex,
              Handle,Left,Top,Right,Bottom,Integer(bmpp),bmpp.FImageType]);
        img:=Format('%s/img?id=%s',[path,bmpp.FID]);
        if not BinaryImages then
          MirrorManager.Cache.Add(bmpp.FID,bmpp.FImageType,bmpp.FStream);
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
