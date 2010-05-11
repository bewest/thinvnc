unit ThinVnc.Windows;

(*:@author Gustavo Ricardi
   @desc <pre>

This software is distributed under the GPL license.

Copyright (c) 2010, Gustavo Ricardi
All rights reserved.
*)
interface
uses Activex,Windows,Classes,SysUtils,Forms,Contnrs,Graphics,SyncObjs,strutils,
  Generics.Collections,Generics.Defaults,math,pngimage,jpeg, ThinVnc.Utils;

const
  MOVE_TIME = 0;

type

  TWin = class(TObject)
  private
    Wnd : Hwnd;
    Rect : TRect;
    Pid : Cardinal;
  public
    constructor Create(AWnd:HWND;ARect:TRect;APid:Cardinal);
  end;

  TMirrorManager = class;

  TImagePart = class
  private
    FStream : TMemoryStream;
    FRect : TRect;
    FImageType : string;
  public
    constructor Create(ARect:TRect;AImageType:string);
    destructor Destroy;override;
  end;

  TWindowMirror = class
  private
    FIndex : Integer;
    FMirrorManager : TMirrorManager;
    FRgn : HRGN;
    FHandle : THandle;
    FBoundsRect : TRect;
    FProcessId : Integer;
    FImage : TBitmap;
    FDiffStreamList : TList<TImagePart>;
    FImageType : string;
    FMoved : TDatetime;
    function GetId: string;
    function GetActive: Boolean;
    procedure SetBoundsRect(ARect:TRect);
    function CreateBitmap: TBitmap;
    function getJson(path:string): string;
    function ExtractClippingRegions(R: TRect): TRectArray;
  public
    constructor Create(AMirrorManager:TMirrorManager;AHandle:THandle;ARect:TRect;APid:Integer);
    destructor  Destroy;override;
    function Capture(ANewImage: TBitmap): Boolean;
    function CaptureDifferences(reset:boolean=false): Boolean;
    procedure GenRegions(wl: TList<TWin>;AIndex:Integer);
    property zIndex:Integer read FIndex;
    property Id:string read GetId;
    property Active:Boolean read GetActive;
  end;

  TCacheItem = class
  private
    FImageStream : TMemoryStream;
    FImageType : string;
    FAdded : TDateTime;
    FID : string;
    function GetFilename(path:string): string;
  public
    constructor Create(AID:string;AImageStream:TStream;AImageType:string);
    destructor Destroy;override;
    property ImageType : string read FImageType;
    function GetStream:TStream;
    function ID:string;
  end;

  TMirrorManager = class(TInterfacedObject,IComparer<TWindowMirror>)
  private
    FMirrorList : TList<TWindowMirror>;
    FProcessIds: array of Integer;
    FImage : TBitmap;
    FImageCacheCs : TCriticalSection;
    FImageCache : TDictionary<string,TCacheItem>;
    FJsonIdx : Integer;
    FUseJpeg: Boolean;
    FEmbeddedImage: Boolean;
    FUseCanvas: Boolean;
    FLastFullPoll : TDateTime;
    FJpgQuality: Integer;
    FJpgPixelFormat: TJPEGPixelFormat;
    FJpgGrayscale: boolean;
    function Compare(const Left, Right: TWindowMirror): Integer;
    procedure Cleanup;
    function GetWindowMirror(AWinList:TList<TWindowMirror>;AHandle: THandle): TWindowMirror;
    function IsProcessInList(APid: Integer): Boolean;
    procedure ClearList(List: TList<TWindowMirror>); overload;
    procedure ClearList(List: TList<TWin>); overload;
    function AddImageToCache(AID:string;AStream: TStream; AImageType: string): string;
    procedure SaveToFile(json: string);
  protected
    procedure RefreshMirrorList(out OneMoved:Boolean);
  public
    constructor Create;virtual;
    destructor  Destroy;override;
    procedure   ClearCache;
    function    ExtractImageFromCache(AID:string): TCacheItem;
    function    getJson(path:string):string;
    function    Capture(reset:boolean=false):boolean;
    procedure   AddProcessId(APid:Integer);
    procedure   DelProcessId(APid:Integer);
    property    WinList : TList<TWindowMirror> read FMirrorList;
    property    UseJpeg:Boolean read FUseJpeg write FUseJpeg;
    property    EmbeddedImage:Boolean read FEmbeddedImage write FEmbeddedImage;
    property    UseCanvas:Boolean read FUseCanvas write FUseCanvas;
    property    JpgGrayscale:boolean read FJpgGrayscale write FJpgGrayscale;
    property    JpgQuality:Integer read FJpgQuality write FJpgQuality;
    property    JpgPixelFormat:TJPEGPixelFormat read FJpgPixelFormat write FJpgPixelFormat;
  end;

implementation

uses
  IOUtils,Types;

function IsInIgnoreClassList(Wnd:HWnd):Boolean;
begin
  result:=Pos(GetWindowClassName(wnd)+';','SysShadow;DimmedWindowClass;')>0;
end;

function EnumWindowsProc(Wnd: HWnd; const obj:TList<TWin>): Bool; export; stdcall;
var ProcessId : Cardinal;
  R,R1 : TRect;
  Win : TWin;
begin
  Result:=True;
  GetWindowThreadProcessId(Wnd,ProcessId);
  if IsWindowVisible(Wnd) and not IsIconic(wnd)then begin
    GetWindowRect(Wnd,R);
    IntersectRect(R1,R,Screen.DesktopRect);
    if not IsRectEmpty(R1) then begin

      // Ignore some windows
      if IsInIgnoreClassList(wnd) then exit;

      win := TWin.Create(Wnd,R,ProcessId);
      obj.Add(win);
    end;
  end;
end;

{ TWindowMirror }

constructor TWindowMirror.Create(AMirrorManager:TMirrorManager;AHandle:THandle;ARect:TRect;APid:Integer);
begin
  inherited Create;
  FMirrorManager:=AMirrorManager;
  FHandle := AHandle;
  SetBoundsRect(ARect);
  FProcessId := APid;
  FImage := CreateBitmap;
  FDiffStreamList := TList<TImagePart>.Create;
end;

destructor TWindowMirror.Destroy;
begin
  if FRgn<>0 then
    DeleteObject(FRgn);
  FreeAndNil(FDiffStreamList);
  FreeAndNil(FImage);
  inherited;
end;

function TWindowMirror.CreateBitmap:TBitmap;
begin
  result := TBitmap.Create;
  result.PixelFormat:=pf32bit;
  result.Width:=Width(FBoundsRect);
  result.Height:=Height(FBoundsRect);
end;

function TWindowMirror.GetActive: Boolean;
begin
  result:=IsWindow(FHandle) and IsWindowVisible(FHandle) and not IsIconic(FHandle);
end;

function TWindowMirror.GetId: string;
begin
  result:=IntToStr(FHandle);
end;

function TWindowMirror.ExtractClippingRegions(R: TRect): TRectArray;
var n : Integer;
  Count: DWORD;
  RgnData: PRgnData;
  P :PAnsiChar;
  PR : PRect;
  Rgn : HRgn;
begin
  SetLength(result,0);
  Rgn:=CreateRectRgn(R.Left,R.Top,R.Right,R.Bottom);
  try
    CombineRgn(Rgn, FRgn,Rgn, RGN_AND);

    RgnData:=AllocMem(GetRegionData(Rgn,2048,nil));
    try
      Count:=GetRegionData(Rgn,2048,RgnData);
      if Count=0 then abort;
      p:=@RgnData.Buffer[0];

      if RgnData^.rdh.nCount>0 then
      for n := 0 to RgnData^.rdh.nCount - 1 do begin
        PR:=PRect(p);
        SetLength(result,Length(result)+1);
        result[Length(result)-1]:=PR^;
        Inc(p,SizeOf(TRect));
      end;
    finally
      Freemem(RgnData);
    end;
  finally
    DeleteObject(Rgn);
  end;
end;

procedure TWindowMirror.SetBoundsRect(ARect: TRect);
var
  TmpImage : TBitmap;
begin
  if not EqualRect(FBoundsRect,ARect) then begin
    if (FBoundsRect.Left<>ARect.Left) or (FBoundSRect.Top<>ARect.Top) then
      FMoved:=Now;
    FBoundsRect:=ARect;
    if assigned(FImage) then begin
      TmpImage := TBitmap.Create;
      try
        TmpImage.Assign(FImage);
        FImage.Width:=Width(FBoundsRect);
        FImage.Height:=Height(FBoundsRect);
        Bitblt(FImage.Canvas.Handle,
            0,0,Min(TmpImage.Width,FImage.Width),Min(TmpImage.Height,FImage.Height),
            TmpImage.Canvas.handle, 0,0,SRCCOPY);
      finally
        TmpImage.Free;
      end;
    end;
  end;
end;

procedure TWindowMirror.GenRegions(wl:TList<TWin>;AIndex:Integer);
var
  n : Integer;
  Rgn2 : HRGN;
  R : TRect;
begin
  if FRgn<>0 then
    DeleteObject(FRgn);
  with wl[AIndex].Rect do
    FRgn:=CreateRectRgn(0,0,Right-Left,Bottom-Top);
  for n := 0 to AIndex - 1 do begin
    IntersectRect(R,wl[n].Rect,wl[AIndex].Rect);
    if IsRectEmpty(R) then Continue;
    OffsetRect(R,-wl[AIndex].Rect.Left,-wl[AIndex].Rect.Top);
    with R do
      Rgn2:=CreateRectRgn(Left,Top,Right,Bottom);
    CombineRgn(FRgn, FRgn,Rgn2, RGN_DIFF);
    DeleteObject(Rgn2);
  end;
end;

function TWindowMirror.CaptureDifferences(reset:boolean=false): Boolean;
var TmpImage : TBitmap;
  png : TPngImage;
  Jpg : TJpegImage;
  ra,ra2 : TRectArray;
  n,m : Integer;
  tmpbmp : TBitmap;
  rbmp : TRect;
begin
  result:=False;
  FDiffStreamList.Clear;
  if Width(FBoundsRect)*Height(FBoundsRect)=0 then exit;
  if not IsWindowVisible(FHandle) or IsIconic(FHandle) then exit;

  TmpImage:=CreateBitmap;
  try
    result:=Capture(TmpImage);
    if result then begin
      if reset then begin
        FreeAndNil(FImage);
        FImage:=CreateBitmap;
        SetLength(ra,1);
      end;

      ra:=ExtractClippingRegions(Rect(0,0,TmpImage.Width,TmpImage.Height));
      result:=(Length(ra)>0);
      for n := 0 to Length(ra) - 1 do begin
        rbmp := ra[n];
        ra2:=GetDiffRects(FImage,TmpImage,ra[n]);
        result:=result or (Length(ra2)>0);
        for m := 0 to Length(ra2) - 1 do begin
          rbmp := ra2[m];
          IntersectRect(rbmp,rbmp,Rect(0,0,TmpImage.Width,TmpImage.Height));
          if IsRectEmpty(rbmp) then continue;

          if FMirrorManager.UseJpeg then begin
            Jpg := TJpegImage.Create;
            try
              Jpg.CompressionQuality:=FMirrorManager.JpgQuality;
              Jpg.PixelFormat:=FMirrorManager.JpgPixelFormat;
              Jpg.GrayScale:=FMirrorManager.JpgGrayScale;
              CopyBmpToJpg(Jpg,TmpImage,rbmp);

              FDiffStreamList.Add(TImagePart.Create(rbmp,'jpeg'));
              try
                Jpg.SaveToStream(FDiffStreamList[FDiffStreamList.Count-1].FStream);
              except
                On E:Exception do begin
                  tmpbmp:=CopyBmpToBmp(TmpImage,rbmp,pf32bit);
                  try
                    FDiffStreamList[FDiffStreamList.Count-1].FImageType:='bmp';
                    tmpbmp.SaveToStream(FDiffStreamList[FDiffStreamList.Count-1].FStream);
                  finally
                    tmpbmp.Free;
                  end;
                end;
              end;
            finally
              Jpg.Free;
            end;
            FImageType:='jpeg';
          end else begin
            png:=TPngImage.Create;
            try
              CopyBmpToPng(png,FImage,TmpImage,rbmp);
              FDiffStreamList.Add(TImagePart.Create(rbmp,'png'));
              png.SaveToStream(FDiffStreamList[FDiffStreamList.Count-1].FStream);
            finally
              png.Free;
            end;
          end;
          Bitblt(FImage.Canvas.Handle,
            rbmp.Left,rbmp.Top,Width(rbmp),Height(rbmp),
            TmpImage.Canvas.handle, rbmp.Left,rbmp.Top,SRCCOPY);
        end;
      end;
    end
  finally
    TmpImage.Free;
  end;
end;

function TWindowMirror.Capture(ANewImage:TBitmap): Boolean;
  function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC;
      XSrc, YSrc: Integer; Rop: DWORD): BOOL;
  begin
    // Capture only visible regions
    SelectClipRgn(DestDC,FRgn);
    result:=Windows.BitBlt(DestDC, X, Y, Width, Height, SrcDC,
        XSrc, YSrc, Rop);
    SelectClipRgn(DestDC,0);
  end;
var
  DC : HDC;
  RasterOp,ExStyle: DWORD;
begin
  RasterOp := SRCCOPY;
  ExStyle:=GetWindowLong(FHandle, GWL_EXSTYLE);
  if (ExStyle and WS_EX_LAYERED) = WS_EX_LAYERED then
    RasterOp := SRCCOPY or CAPTUREBLT;

  DC := GetDCEx(FHandle,0,DCX_WINDOW or DCX_NORESETATTRS or DCX_CACHE);
  try
    Result:=BitBlt(ANewImage.Canvas.Handle,0,0,
                   Width(FBoundsRect),Height(FBoundsRect),
                   DC,0,0, RasterOp)
  finally
    ReleaseDC(FHandle,DC);
  end;
end;

function TWindowMirror.getJson(path:string): string;
var n : Integer;
  img,imgs : string;
  imgp : TImagePart;
begin
  result:=Format('{ "hwnd":"%d","zidx":%d,"left":%d,"top":%d,"width":%d,"height":%d',
                    [FHandle,FIndex,FBoundsRect.Left,FBoundsRect.Top,
                              Width(FBoundsRect),Height(FBoundsRect)]);
  imgs:='';
  for n:=0 to FDiffStreamList.Count-1 do begin
    imgp:=FDiffStreamList[n];
    if FMirrorManager.EmbeddedImage then
      img:=Format('data:image/%s;base64,%s',[FDiffStreamList[n].FImageType,EncodeBase64(FDiffStreamList[n].FStream)])
    else img:=Format('%s/img?id=%s',
          [path,FMirrorManager.AddImageToCache(Format('%.3d-%d-%d-%d-%d-%d',
          [FMirrorManager.FJsonIdx,FHandle,imgp.FRect.Left,imgp.FRect.Top,imgp.FRect.Right,imgp.FRect.Bottom]),
          FDiffStreamList[n].FStream,FImageType)]);
    imgs:=Format('%s %s{ "x":%d,"y":%d,"w":%d,"h":%d,"img": "%s" }',[imgs,ifthen(n>0,',',''),
          imgp.FRect.Left,imgp.FRect.Top,width(imgp.FRect),Height(imgp.FRect),img]);
  end;
  if FDiffStreamList.Count>0 then
    result:=result+Format(',"imgs": [%s]}',[imgs])
  else result:=result+'}';
  FDiffStreamList.Clear;
end;
{ TMirrorManager }

constructor TMirrorManager.Create;
var
  FileList : TStringDynArray;
  sFile : string;
begin
  FMirrorList := TList<TWindowMirror>.Create(Self);
  FImageCacheCs := TCriticalSection.Create;
  FImageCache := TDictionary<string,TCacheItem>.Create;
  FUseJpeg := True;
  FEmbeddedImage := False;
  if DirectoryExists(GetModulePath+'Debug') then begin
    FileList:=TDirectory.GetFiles(GetModulePath+'Debug\','*.*');
    for sfile in FileList do
      DeleteFile(sFile);
  end;
  FLastFullPoll:=Now;
  FJpgQuality:=80;
  FJpgPixelFormat:=jf24Bit;
end;

destructor TMirrorManager.Destroy;
begin
  FreeAndNil(FImage);
  ClearList(FMirrorList);
  FreeAndNil(FMirrorList);
  FreeAndnil(FImageCache);
  FreeAndNil(FImageCacheCs);
  inherited;
end;

function TMirrorManager.AddImageToCache(AID:string;AStream:TStream;AImageType:string):string;
var
  ci : TCacheItem;
begin
  FImageCacheCs.Enter;
  try
    ci:=TCacheItem.Create(AID,AStream,AImageType);
    FImageCache.Add(AID,ci);
    result:=AID;
  finally
    FImageCacheCs.Leave;
  end;
end;

procedure TMirrorManager.ClearCache;
begin
  FImageCacheCs.Enter;
  try
    FImageCache.Clear;
  finally
    FImageCacheCs.Leave;
  end;
end;

function TMirrorManager.ExtractImageFromCache(AID:string): TCacheItem;
begin
  FImageCacheCs.Enter;
  try
    result:=FImageCache[AID];
    FImageCache.ExtractPair(AID);
  finally
    FImageCacheCs.Leave;
  end;
end;

procedure TMirrorManager.SaveToFile(json:string);
var
  sstream : TStringStream;
  path : string;
  pair : TPair<string,TCacheItem>;
begin
  path:=GetModulePath+'Debug\';
  if not DirectoryExists(path) then exit;

  sstream := TStringStream.Create(json);
  try
    sstream.SaveToFile(Format('%s%.3d_json.js',[path,FJsonIdx]));
    if not EmbeddedImage then begin
      FImageCacheCs.Enter;
      try
        for pair in FImageCache do
          Pair.Value.FImageStream.SaveToFile(Pair.Value.GetFilename(path));
      finally
        FImageCacheCs.Leave;
      end;
    end;
  finally
    sstream.Free;
  end;
end;

procedure TMirrorManager.Cleanup;
var n : Integer;
begin
  for n:=FMirrorList.Count-1 downto 0 do begin
    if not TWindowMirror(FMirrorList[n]).Active then begin
      FMirrorList[n].Free;
      FMirrorList.Delete(n);
    end;
  end;
end;

procedure TMirrorManager.ClearList(List: TList<TWin>);
var I: Integer;
begin
  for I := List.Count - 1 downto 0 do List[I].Free;
  List.Clear;
end;

function TMirrorManager.Compare(const Left, Right: TWindowMirror): Integer;
begin
  if Left.FIndex>Right.FIndex then result:=1
  else if Left.FIndex<Right.FIndex then result:=-1
  else result:=0;
end;

procedure TMirrorManager.ClearList(List: TList<TWindowMirror>);
var I: Integer;
begin
  for I := 0 to List.Count - 1 do List[I].Free;
  List.Clear;
end;

function  TMirrorManager.GetWindowMirror(AWinList:TList<TWindowMirror>;AHandle:THandle):TWindowMirror;
var n : Integer;
begin
  result:=nil;
  for n:=AWinList.Count-1 downto 0 do
    if TWindowMirror(AWinList[n]).FHandle=AHandle then begin
      result:= TWindowMirror(AWinList[n]);
      break;
    end;
end;

procedure TMirrorManager.RefreshMirrorList(out OneMoved:Boolean);
  procedure GetProcessWindowList(WinList:TList<TWin>);
  begin
    WinList.Clear;
    EnumWindows(@EnumWindowsProc, Longint(WinList));
  end;

var
  wl : TList<TWin>;
  n : Integer;
  wm : TWindowMirror;
begin
  OneMoved:=False;

  wl := TList<TWin>.Create;
  try
    // Enumerates top windows
    GetProcessWindowList(wl);
    try
      for n := wl.Count - 1 downto 0 do begin
        // Looks for a cached window
        wm:=GetWindowMirror(FMirrorList,wl[n].Wnd);
        if assigned(wm) then begin
          if IsIconic(wl[n].Wnd) then
            wm.SetBoundsRect(Rect(0,0,0,0))
          else wm.SetBoundsRect(wl[n].Rect);

          // Returns true when at least one window moved
          OneMoved:=OneMoved or (DateTimeToTimeStamp(Now-wm.FMoved).time<MOVE_TIME);
        end else begin
          // Do not create a TWindowMirror for invisible windows
          if IsIconic(wl[n].Wnd) then Continue;

          wm:=TWindowMirror.Create(Self,wl[n].Wnd,wl[n].Rect, wl[n].pid);
          FMirrorList.Add(wm);
        end;
        // Saves the zIndex
        wm.FIndex:=wl.Count-n;
        // Generates clipping regions
        wm.GenRegions(wl,n);
      end;
    finally
      ClearList(wl);
    end;
    // Sorts the mirror list by zIndex
    FMirrorList.Sort;
  finally
    wl.free;
  end;
end;

function TMirrorManager.IsProcessInList(APid: Integer):Boolean;
var n : Integer;
begin
  result:=Length(FProcessIds)=0;
  for n := 0 to Length(FProcessIds) - 1 do begin
    result:=(FProcessIds[n]=APid) or (FProcessIds[n]=0);
    if result then exit;
  end;
end;

procedure TMirrorManager.AddProcessId(APid: Integer);
begin
  if (Length(FProcessIds)>0) and IsProcessInList(APid) then exit;
  SetLength(FProcessIds,Length(FProcessIds)+1);
  FProcessIds[Length(FProcessIds)-1]:=APid;
end;

procedure TMirrorManager.DelProcessId(APid: Integer);
var n,m : Integer;
begin
  for n := 0 to Length(FProcessIds) - 1 do
    if FProcessIds[n]=APid then begin
      for m := n+1 to Length(FProcessIds) - 1 do
        FProcessIds[m-1]:= FProcessIds[m];
      SetLength(FProcessIds,Length(FProcessIds)-1);
      break;
    end;
end;

function TMirrorManager.getJson(path:string): string;
var n : Integer;
  json : string;
begin
  Cleanup;

  Inc(FJsonIdx);

  json:='';
  for n := 0 to FMirrorList.Count - 1 do begin
    if not IsProcessInList(FMirrorList[n].FProcessId) then continue;
    if json<>'' then json:=json+','+#13#10;
    json:=json+#9+FMirrorList[n].getJson(path);
  end;
  result:='{"windows": ['+#13#10+json+']}';

  SaveToFile(result);
end;

function TMirrorManager.Capture(reset:boolean=false):boolean;
var
  wm : TWindowMirror;
  m : Integer;
  forewnd : HWND;
begin
  RefreshMirrorList(result);
  if result then exit;

  forewnd:=GetForegroundWindow;
  for m := FMirrorList.Count - 1 downto 0 do begin
    wm:=TWindowMirror(FMirrorList[m]);
    if not wm.Active then continue;
    if (Length(FProcessIds)>0) and not IsProcessInList(wm.FProcessId) then continue;
    wm.CaptureDifferences(reset);
    result:=result or (wm.FDiffStreamList.Count>0);
    if result and not reset and (wm.FHandle=forewnd) and
      (DateTimeToTimeStamp(FLastFullPoll-Now).Time<1000) then break;
    if m = FMirrorList.Count - 1 then FLastFullPoll:=Now;
  end;
end;

{ TCacheItem }

constructor TCacheItem.Create(AID:string;AImageStream: TStream;AImageType: string);
begin
  FImageStream:=TMemoryStream.Create;
  AImageStream.Seek(0,0);
  FImageStream.CopyFrom(AImageStream,AImageStream.Size);
  FAdded:=Now;
  FImageType:=AImageType;
  FID:=AID;
end;

destructor TCacheItem.destroy;
begin
  FreeAndNil(FImageStream);
  inherited;
end;

function TCacheItem.GetFilename(path:string):string;
begin
  result:=Format('%s%s.%s',[path,FID,FImageType]);
end;

function TCacheItem.GetStream: TStream;
begin
  FImageStream.Seek(0,0);
  result:=FImageStream;
  FImageStream:=nil;
end;

function TCacheItem.ID: string;
begin
  result:=FID;
end;

{ TImagePart }

constructor TImagePart.Create(ARect:TRect;AImageType:string);
begin
  FStream := TMemoryStream.Create;
  FRect:=ARect;
  FImageType:=AImageType;
end;

destructor TImagePart.Destroy;
begin
  FStream.Free;
  inherited;
end;

{ TWin }

constructor TWin.Create(AWnd: HWND; ARect: TRect; APid: Cardinal);
begin
  Wnd:=AWnd;
  Rect:=ARect;
  Pid:=APid;
end;

end.
