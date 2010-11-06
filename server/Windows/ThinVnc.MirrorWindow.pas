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

unit ThinVnc.MirrorWindow;
{$I ThinVnc.DelphiVersion.inc}
{$I ThinVnc.inc}
interface

{.$UNDEF VIDEO_DRIVER}
{.$UNDEF CLIPPING_WINDOWS}

uses Activex,Windows,Classes,SysUtils,Forms,Contnrs,Graphics,SyncObjs, StrUtils,
  math,jpeg, IniFiles,
  ThinVnc.Log,
{$IFDEF ENABLE_DESKTOP_UTILS}
  ThinVnc.DesktopUtils,
{$ENDIF}
  ThinVnc.Utils;

{$DEFINE CAPTURE_CHANGEDRGN}
const
  CREATE_TIME = 0;
  MOVE_TIME = 100;
  STRETCH_TIME = 250;
  MIN_TIME = 250;
  MAX_TIME = 250;
  MAX_POLLTIME = 0;
  INITIAL_POLL_FREQUENCY = 25;

type
  TImageMethod = (imJpeg,imPng,imMixed);

  TAbstractScraper = class;
  TScraperClass = class of TAbstractScraper;

  TWinData = class(TObject)
  private
    Wnd : Hwnd;
    Rect : TRect;
    Pid : Cardinal;
    ClassName : string;
  public
    constructor Create(AWnd:HWND;ARect:TRect;APid:Cardinal;AClassname:string);
  end;

  TWinDataList = class(TObjectList)
  private
    function GetItems(Index: Integer): TWinData;
  public
    function Add(Item: TWinData): Integer;
    property Items[Index: Integer]: TWinData read GetItems; default;
  end;

  TMirrorWindow = class;

  TMirrorWindow = class
  private
    FIndex : Integer;
    FScraper : TAbstractScraper;
    FClipRgn : HRGN;
    FUpdateRgn : HRGN;
    FHandle : THandle;
    FProcessId : Integer;
    FMoved : TDatetime;
    FMinMax : TDateTime;
    FClassname : string;
    FDesktopName : string;
    FLastpoll : TDatetime;
    FPollFrequency : Integer;
    FBitmapCache : TBitmap;
    FClientRect: TRect;
    FChangedRgn : HRGN;
    FBypass : Boolean;
    FMasked: Boolean;
    function GetClientRect: TRect;
    function GetBoundsRect: TRect;
    procedure AccumChangedParentRgn(Rgn: HRGN);
    procedure CreateUpdateRgn(Rgn: HRGN);
    procedure AccumChangedRgn(R: TRect);
    procedure ClearBitmapCache;
    function IsSpecialWindow:boolean;
  protected
    FBoundsRect : TRect;
    procedure SetBoundsRect(ARect:TRect);
    function GetActive: Boolean;virtual;
  public
    constructor Create(AScraper:TAbstractScraper;AHandle:THandle;ARect:TRect;APid:Integer;AClassname,ADesktopName:string);
    destructor  Destroy;override;
    procedure RefreshBitmapCache;
    function CreateBitmap: TBitmap;
    function Capture(ANewImage: TBitmap=nil): Boolean;
    function MustPoll(Count:Integer):Boolean;
    procedure GenRegions(wl: TWinDataList;AIndex:Integer);
    procedure ClearChangedRgn;
    property ClipRgn:HRgn read FClipRgn;
    property ChangedRgn : HRGN read FChangedRgn;
    property zIndex:Integer read FIndex;
    property Active:Boolean read GetActive;
    property Handle:THandle read FHandle;
    property ProcessId:Integer read FProcessId;
    property Scraper:TAbstractScraper read FScraper;
    property BoundsRect:TRect read GetBoundsRect;
    property Classname:string read FClassname;
    property DesktopName:string read FDesktopName;
    property BitmapCache:TBitmap read FBitmapCache;
    property ClientRect:TRect read GetClientRect write FClientRect;
    property Masked:Boolean read FMasked;
  end;

  TMirrorWindowList = class(TObjectList)
  private
    function GetItems(Index: Integer): TMirrorWindow;
    function GetItemsByHandle(Index: Integer): TMirrorWindow;
  public
    function Add(Item: TMirrorWindow): Integer;
    property Items[Index: Integer]: TMirrorWindow read GetItems; default;
    property ItemsByHandle[Index:Integer]:TMirrorWindow read GetItemsByHandle;
  end;

  TIntArray = array of integer;
  TAbstractScraper = class(TComponent)
  private
    FMirrorListCs : TCriticalSection;
    FSendCursor: Boolean;
    FJpgPixelFormat: TJpegPixelFormat;
    FCapturePixelFormat: TPixelFormat;
    FKeyDownList : TList;
    FLastKeytime : TDatetime;
    FKeyCs : TCriticalSection;
    FSynchronizeInput: Boolean;
    FSynchronizeCapture: Boolean;
    FFrameDelay: Integer;
    FUseChangedRgn: Boolean;
    FChangedRgn : HRGN;
    FMirrorList : TMirrorWindowList;
    FOnSessionAdded : TNotifyEvent;
    FOnSessionRemoved : TNotifyEvent;
    FImageMethod: TImageMethod;
    FRemotePointer: Boolean;
    procedure AddKeyDownToList(key: word);
    procedure AccumChangedRgn(Rgn:HRgn);overload;
    procedure AccumChangedRgn(R:TRect);overload;
    procedure AccumChangedChildRgn(mw: TMirrorWindow);
  protected
    FCaptureIndex : Integer;
    FCurrentDesktopName: string;
  public
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
    procedure   ClearChangedRgn;
    function    IsInputAllowed(AHandle:THandle):boolean;virtual;
    function    KeysDown(vks: array of integer): boolean;
    function    KeysUp(vks: array of integer): boolean;
    procedure   FlushKeyboard(force: Boolean=false);
    procedure   ProcessMouseInput(X, Y,delta, Btn: Integer; action: string);virtual;
    function    PressFunctionKey(Func: string): Boolean;virtual;
    procedure   ProcessKeyboardInput(key,ch: word; Down: boolean);virtual;
    function    DVRect(Index:Integer=-1):TRect;virtual;abstract;
    function    DVWidth(Index:Integer=-1):Integer;virtual;abstract;
    function    DVHeight(Index:Integer=-1):Integer;virtual;abstract;
    function    DVLeft(Index:Integer=-1):Integer;virtual;abstract;
    function    DVTop(Index:Integer=-1):Integer;virtual;abstract;
    function    Capture:boolean;virtual;
    procedure   Lock;
    procedure   UnLock;
    procedure   ClearWindows;
    procedure   SessionAdded(Sender:TObject);virtual;
    procedure   SessionRemoved(Sender:TObject);virtual;
    property    MirrorList : TMirrorWindowList read FMirrorList;
    property    ChangedRgn : HRGN read FChangedRgn;
    property    UseChangedRgn : Boolean read FUseChangedRgn;
    property    CapturePixelFormat : TPixelFormat read FCapturePixelFormat write FCapturePixelFormat;
    property    CaptureIndex:Integer read FCaptureIndex;
    property    JpgPixelFormat:TJpegPixelFormat read FJpgPixelFormat write FJpgPixelFormat;
    property    SynchronizeInput:Boolean read FSynchronizeInput write FSynchronizeInput;
    property    SynchronizeCapture:Boolean read FSynchronizeCapture write FSynchronizeCapture;
    property    FrameDelay:Integer read FFrameDelay write FFrameDelay;
    property    SendCursor:Boolean read FSendCursor write FSendCursor;
    property    OnSessionAdded : TNotifyEvent read FOnSessionAdded write FOnSessionAdded;
    property    OnSessionRemoved : TNotifyEvent read FOnSessionRemoved write FOnSessionRemoved;
    property    ImageMethod: TImageMethod read FImageMethod write FImageMethod;
    property    RemotePointer:Boolean read FRemotePointer write FRemotePointer;
  end;

  TThinScreenScraper = class(TAbstractScraper)
  private
    FLastFullPoll : TDateTime;
    FVideoCs   : TCriticalSection;
    FMirrorListChanged : Boolean;
    FDisableAero: Boolean;
    FDisableWallpaper: Boolean;
    FUseVideoDriver: Boolean;
    function  GetMirrorWindow(AWinList:TMirrorWindowList;AHandle: THandle): TMirrorWindow;
    function  RefreshMirrorList:boolean;
    function GetDevicePixelFormat: TPixelFormat;
    procedure SetUseVideoDriver(const Value: Boolean);
    procedure InitVideoDriver;
  public
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
    procedure   SessionAdded(Sender:TObject);override;
    procedure   SessionRemoved(Sender:TObject);override;
    function    DVRect(Index:Integer=-1):TRect;override;
    function    DVWidth(Index:Integer=-1):Integer;override;
    function    DVHeight(Index:Integer=-1):Integer;override;
    function    DVLeft(Index:Integer=-1):Integer;override;
    function    DVTop(Index:Integer=-1):Integer;override;
    function    VideoDriverInstalled: Boolean;
    function    Capture:boolean;override;
    property    MirrorListChanged : Boolean read FMirrorListChanged;
    property    CurrentDesktopName:string read FCurrentDesktopName;
    property    DisableAero:Boolean read FDisableAero write FDisableAero;
    property    DisableWallpaper:Boolean read FDisableWallpaper write FDisableWallpaper;
    property    UseVideoDriver:Boolean read FUseVideoDriver write SetUseVideoDriver;
  end;

function ScreenScraper : TThinScreenScraper;

var
  gScraperClass : TScraperClass = TThinScreenScraper;
  MyScreen : TScreen;

implementation

uses
  ThinVnc.Capture;


{$IFNDEF DELPHI2010}
const
  {$EXTERNALSYM KEYEVENTF_UNICODE}
  KEYEVENTF_UNICODE     = 4;
{$ENDIF}

function ScreenScraper : TThinScreenScraper;
begin
  result:=(CaptureThread.Scraper as TThinScreenScraper);
end;

function IsInIgnoreClassList(Classname:string;Wnd:HWnd):Boolean;
begin
  result:=Pos(Classname+';','TThinVncAppBorder;TSgAppBorder;SysShadow;DimmedWindowClass;TGlassForm;')>0;
end;

function IsInHiddenClassList(Classname:string):Boolean;
begin
  result:=Pos(Classname+';','TThinVNCPM;')>0;
end;

function EnumWindowsProc(Wnd: HWnd; const obj:TWinDataList): Bool; export; stdcall;
var ProcessId : Cardinal;
  R,R1 : TRect;
  Win : TWinData;
  ExStyle,Style : DWORD;
  Classname : string;
begin
  Result:=True;
  GetWindowThreadProcessId(Wnd,ProcessId);
  if IsWindowVisible(Wnd){ and not IsIconic(wnd)}then begin
    GetWindowRect(Wnd,R);
    IntersectRect(R1,R,MyScreen.DesktopRect);
    if not IsRectEmpty(R1) or IsIconic(wnd) then begin

      // Ignore some windows
      Classname:=GetWindowClassName(wnd);

      if IsInIgnoreClassList(Classname,wnd) then exit;
      ExStyle:=GetWindowLong(Wnd, GWL_EXSTYLE);
{      Style:=GetWindowLong(Wnd, GWL_STYLE);
      LogInfo(Format('Window class:%s style:%.8x exstyle:%.8x',[Classname,Style,ExStyle]));
}      if ((ExStyle and WS_EX_LAYERED) = WS_EX_LAYERED) and (Classname='#32768') then
        exit;
      win := TWinData.Create(Wnd,R,ProcessId,Classname);
      obj.Add(win);
    end;
  end;
end;

function CompareTMirrorWindow(Left, Right: Pointer): Integer;
begin
  if TMirrorWindow(Left).FIndex>TMirrorWindow(Right).FIndex then result:=1
  else if TMirrorWindow(Left).FIndex<TMirrorWindow(Right).FIndex then result:=-1
  else result:=0;
end;

{ TMirrorWindow }

constructor TMirrorWindow.Create(AScraper:TAbstractScraper;AHandle:THandle;ARect:TRect;APid:Integer;AClassname,ADesktopName:string);
begin
  inherited Create;
  FScraper:=AScraper;
  FHandle := AHandle;
  SetBoundsRect(ARect);
  FProcessId := APid;
  FClassname := AClassname;
  FDesktopName := ADesktopName;
  FPollFrequency := INITIAL_POLL_FREQUENCY;
  FLastpoll := Now;
end;

destructor TMirrorWindow.Destroy;
begin
  ClearChangedRgn;
  FreeAndNil(FBitmapCache);
  if (FClipRgn<>0) then
    DeleteObject(FClipRgn);
  if (FUpdateRgn<>0) then
    DeleteObject(FUpdateRgn);
  inherited;
end;

function TMirrorWindow.CreateBitmap:TBitmap;
begin
  result := TBitmap.Create;
  result.PixelFormat:=FScraper.CapturePixelFormat;
  result.Width:=Width(BoundsRect);
  result.Height:=Height(BoundsRect);
end;

function TMirrorWindow.GetActive: Boolean;
begin
  result:=IsWindow(FHandle) and IsWindowVisible(FHandle){ and not IsIconic(FHandle)};
end;

function TMirrorWindow.GetBoundsRect: TRect;
var R : TRect;
begin
  result:=FBoundsRect;
  if not IsRectEmpty(FClientRect) then begin
    R:=ClientRect;
    OffsetRect(R,FBoundsRect.Left,FBoundsRect.Top);
    IntersectRect(result,FBoundsRect,R);
  end;
end;

function TMirrorWindow.MustPoll(Count:Integer): Boolean;
begin
  result:=(FScraper.FCurrentDesktopName=FDesktopName) and
    (DateTimeToTimeStamp(Now-FLastPoll).time>=Min(FPollFrequency*((Count-zIndex)),MAX_POLLTIME));
  if result then FLastPoll:=Now;
end;

procedure TMirrorWindow.RefreshBitmapCache;
begin
  FreeAndNil(FBitmapCache);
  FBitmapCache:=CreateBitmap;
end;

procedure TMirrorWindow.ClearBitmapCache;
begin
  FreeAndNil(FBitmapCache);
end;

procedure TMirrorWindow.SetBoundsRect(ARect: TRect);
begin
  if not EqualRect(FBoundsRect,ARect) then begin
    
    if ((Width(FBoundsRect)<>Width(ARect)) or
      (Height(FBoundsRect)<>Height(ARect))) and Assigned(FBitmapCache) then begin
        // Avoid loosing cache
        if (Width(ARect)<>0) and (Height(ARect)<>0) then begin
          FBitmapCache.Canvas.Lock;
          try
            FBitmapCache.Height:=Height(ARect);
            FBitmapCache.Width:=Width(ARect);
          finally
            FBitmapCache.Canvas.Unlock;
          end;
        end;
      end;

    if (FBoundsRect.Left<>ARect.Left) or (FBoundSRect.Top<>ARect.Top) then begin
      FMoved:=Now;
      if IsRectEmpty(ARect) or IsRectEmpty(FBoundsRect) then
        FMinMax:=Now;
    end;
    FBoundsRect:=ARect;
  end;
end;

procedure TMirrorWindow.GenRegions(wl:TWinDataList;AIndex:Integer);
var
  n : Integer;
  Rgn2 : HRGN;
  R : TRect;
begin
  if FClipRgn<>0 then
    DeleteObject(FClipRgn);
  with wl[AIndex].Rect do
    FClipRgn:=CreateRectRgn(0,0,Right-Left,Bottom-Top);
  for n := 0 to AIndex - 1 do begin
    IntersectRect(R,wl[n].Rect,wl[AIndex].Rect);
    if IsRectEmpty(R) then Continue;
    OffsetRect(R,-wl[AIndex].Rect.Left,-wl[AIndex].Rect.Top);
    with R do
      Rgn2:=CreateRectRgn(Left,Top,Right,Bottom);
    CombineRgn(FClipRgn, FClipRgn,Rgn2, RGN_DIFF);
    DeleteObject(Rgn2);
  end;
end;

function TMirrorWindow.GetClientRect:TRect;
begin
  result:=Rect(0,0,Width(FBoundsRect),Height(FBoundsRect));
  if not IsRectEmpty(FClientRect) then
    IntersectRect(result,result,FClientRect);
end;

function TMirrorWindow.IsSpecialWindow: boolean;
var
  Style,ExStyle : DWORD;
begin
  result:=(Classname=#32768);
  if not result then begin
    ExStyle:=GetWindowLong(Handle, GWL_EXSTYLE);
    Style:=GetWindowLong(Handle, GWL_STYLE);
    result:=((ExStyle and WS_EX_TOPMOST)=WS_EX_TOPMOST) and
       (Style and WS_POPUP = WS_POPUP) and
       (Style and WS_BORDER = 0);
    LogInfo(Format('Window class:%s style:%.8x exstyle:%.8x',[Classname,Style,ExStyle]));
  end;
end;

procedure TMirrorWindow.AccumChangedParentRgn(Rgn:HRGN);
var
  lRgn : HRGN;
begin
  if FChangedRgn=0 then
    FChangedRgn:=CreateRectRgn(0,0,0,0);

  lRgn:=CreateRectRgnIndirect(BoundsRect);
  try
    CombineRgn(lRgn, lRgn,Rgn, RGN_AND);
    OffsetRgn(lRgn,-BoundsRect.Left,-BoundsRect.Top);
    CombineRgn(lRgn, lRgn,FClipRgn, RGN_AND);
    CombineRgn(FChangedRgn, FChangedRgn,lRgn, RGN_OR);
  finally
    DeleteObject(lRgn);
  end;
end;

procedure TMirrorWindow.CreateUpdateRgn(Rgn: HRGN);
begin
  if FUpdateRgn<>0 then
    DeleteObject(FUpdateRgn);
  FUpdateRgn:=CreateRectRgn(0,0,0,0);
  FBypass:=CombineRgn(FUpdateRgn, Rgn,FClipRgn, RGN_AND)=NULLREGION;
end;

procedure TMirrorWindow.AccumChangedRgn(R:TRect);
var
  Rgn : HRGN;
begin
  if FChangedRgn=0 then
    FChangedRgn:=CreateRectRgnIndirect(R)
  else begin
    Rgn:=CreateRectRgnIndirect(R);
    try
      CombineRgn(Rgn, Rgn,FClipRgn, RGN_AND);
      CombineRgn(FChangedRgn, FChangedRgn,Rgn, RGN_OR);
    finally
      DeleteObject(Rgn);
    end;
  end;
end;

procedure TMirrorWindow.ClearChangedRgn;
begin
  if FChangedRgn<>0 then begin
    DeleteObject(FChangedRgn);
    FChangedRgn:=0;
  end;
end;

function TMirrorWindow.Capture(ANewImage:TBitmap=nil): Boolean;

  function CaptureFromDC:boolean;
  var
    DC : HDC;
    RasterOp,ExStyle: DWORD;
    Rgn : HRGN;
    R : TRect;
    ra : TRectArray;
    n : Integer;
  begin
    RasterOp := SRCCOPY;
    ExStyle:=GetWindowLong(FHandle, GWL_EXSTYLE);
    if ((ExStyle and WS_EX_LAYERED) = WS_EX_LAYERED) then
      RasterOp := SRCCOPY or CAPTUREBLT;

    R:=GetClientRect;
    DC := GetDCEx(FHandle,0,DCX_WINDOW);
    try
      ra:=ExtractClippingRegions(FUpdateRgn,R);
      for n := 0 to Length(ra)-1 do begin
        Result:=BitBlt(ANewImage.Canvas.Handle,ra[n].left,ra[n].top,
                       Width(ra[n]),Height(ra[n]),
                       DC,ra[n].Left,ra[n].Top, RasterOp);
      end;
    finally
      ReleaseDC(FHandle,DC);
    end;
  end;

begin
  result:=false;

  if FByPass or (Width(BoundsRect)*Height(BoundsRect)=0) then exit;
  if not IsWindowVisible(FHandle) or IsIconic(FHandle) then exit;

  if ANewImage=nil then ANewImage:=FBitmapCache;

  ANewImage.Canvas.Lock;
  try
        result:=CaptureFromDC;
  finally
    ANewImage.Canvas.UnLock;
  end;
//  LogBitmap(Format('ANewImage Handle:%d Region:%s',[FHandle,DumpRegion(FUpdateRgn)]),ANewImage);
end;


{ TAbstractScraper }

constructor TAbstractScraper.Create(AOwner:TComponent);
begin
  inherited;
  FSendCursor:=true;
  FRemotePointer:=true;
  FMirrorList := TMirrorWindowList.Create;
  JpgPixelFormat:=jf24Bit;
  FCapturePixelFormat:=pf16bit;
  FFrameDelay := 50;
  FKeyDownList := TList.Create;
  FKeyCs := TCriticalSection.Create;
  FMirrorListCs := TCriticalSection.Create;

  FImageMethod := imJpeg;
end;

destructor TAbstractScraper.Destroy;
begin
  Lock;
  try
    if CaptureThread.Scraper=Self then begin
      CaptureThread.Suspend;
      CaptureThread.Scraper:=nil;
      CaptureThread.Resume;
    end;
  finally
    Unlock;
  end;
  FreeAndNil(FMirrorList);
  FreeAndNil(FKeyDownList);
  FreeAndNil(FKeyCs);
  FreeAndNil(FMirrorListCs);
  inherited;
end;

procedure TAbstractScraper.SessionAdded(Sender:TObject);
var Log: ILogger;
begin
  Log := TLogger.Create(Self, 'SessionAdded');
  if Assigned(FOnSessionAdded) then
    FOnSessionAdded(Sender);
end;

procedure TAbstractScraper.SessionRemoved(Sender:TObject);
var Log: ILogger;
begin
  Log := TLogger.Create(Self, 'SessionRemoved');
  if Assigned(FOnSessionRemoved) then
    FOnSessionRemoved(Sender);
end;

function TAbstractScraper.Capture:boolean;
begin
  result:=false;
  ClearChangedRgn;
  if result then Inc(FCaptureIndex);
end;

procedure TAbstractScraper.AccumChangedRgn(Rgn:HRgn);
begin
  if FChangedRgn=0 then
    FChangedRgn:=CreateRectRgn(0,0,0,0);
  CombineRgn(FChangedRgn, FChangedRgn,Rgn, RGN_OR);
end;

procedure TAbstractScraper.AccumChangedRgn(R: TRect);
var
  n : Integer;
  Rgn : HRGN;
begin
  FUseChangedRgn:=true;

  if FChangedRgn=0 then
    FChangedRgn:=CreateRectRgnIndirect(R)
  else begin
    Rgn:=CreateRectRgnIndirect(R);
    try
      CombineRgn(FChangedRgn, FChangedRgn,Rgn, RGN_OR);
    finally
      DeleteObject(Rgn);
    end;
  end;
end;

procedure TAbstractScraper.AccumChangedChildRgn(mw : TMirrorWindow);
var
  lRgn : HRGN;
begin
  if mw.ChangedRgn=0 then exit;
  
  if FChangedRgn=0 then
    FChangedRgn:=CreateRectRgn(0,0,0,0);

  OffsetRgn(mw.FChangedRgn,mw.BoundsRect.Left,mw.BoundsRect.Top);
  CombineRgn(FChangedRgn, FChangedRgn,mw.FChangedRgn, RGN_OR);
  OffsetRgn(mw.FChangedRgn,-mw.BoundsRect.Left,-mw.BoundsRect.Top);
end;

procedure TAbstractScraper.ClearChangedRgn;
var
  n : Integer;
begin
  if FChangedRgn<>0 then begin
    DeleteObject(FChangedRgn);
    FChangedRgn:=0;
  end;
  if assigned(FMirrorList) then
    for n := 0 to FMirrorList.Count - 1 do
      FMirrorList[n].ClearChangedRgn;
  FUseChangedRgn:=false;
end;

procedure TAbstractScraper.ProcessMouseInput(X, Y,delta, Btn: Integer; action: string);
var
  flags : DWORD;
  X1,Y1 : Integer;
begin
  SwitchToActiveDesktop;

  X1:=round(X/(MyScreen.width)*65535);
  Y1:=round(Y/(MyScreen.Height)*65535);

  flags:=0;
  if action='wheel' then begin
    mouse_event(MOUSEEVENTF_WHEEL,0,0,delta,0);
    exit;
  end;

  if action='move' then begin
    mouse_event(MOUSEEVENTF_MOVE or MOUSEEVENTF_ABSOLUTE,X1,Y1,0,0);
    exit;
  end;

  if action='up' then begin
    case btn of
      0 : flags:=MOUSEEVENTF_LEFTUP;
      1 : flags:=MOUSEEVENTF_MIDDLEUP;
      2 : flags:=MOUSEEVENTF_RIGHTUP;
    end;
  end else if action='down' then begin
    if not IsInputAllowed(WindowFromPoint(Point(X,Y))) then exit;
    case btn of
      0 : flags:=MOUSEEVENTF_LEFTDOWN;
      1 : flags:=MOUSEEVENTF_MIDDLEDOWN;
      2 : flags:=MOUSEEVENTF_RIGHTDOWN;
    end;
  end;
  flags:=flags or MOUSEEVENTF_MOVE or MOUSEEVENTF_ABSOLUTE;
  mouse_event(flags,X1,Y1,0,0);
end;

procedure TAbstractScraper.FlushKeyboard(force:Boolean);
begin
  FKeyCs.Enter;
  try
    if (FKeyDownList.Count>0) and (Force or (DatetimeToTimestamp(Now-FLastKeytime).time>10000))  then
      while FKeyDownList.Count>0 do
        ProcessKeyboardInput(Word(FKeyDownList[FKeyDownList.Count-1]),0,false);
  finally
    FKeyCs.Leave;
  end;
end;

function TAbstractScraper.IsInputAllowed(AHandle: THandle): boolean;
begin
  result:=true;
end;

function TAbstractScraper.PressFunctionKey(Func:string):Boolean;
var Log :ILogger;
begin
  Log:=TLogger.Create(Self,'PressFunctionKey');
  Log.LogInfo('Func: '+Func);
  if func='CtrlAltDel' then begin
    FlushKeyboard(true);
      KeysUp([VK_MENU]);
      KeysDown([VK_CONTROL,VK_SHIFT,VK_ESCAPE]);
      Sleep(50);
      KeysUp([VK_SHIFT,VK_CONTROL,VK_ESCAPE]);
  end else if func='AltTab' then begin
    KeysDown([VK_MENU,VK_TAB]);
    KeysUp([VK_TAB]);
  end else if func='AltShiftTab' then begin
    KeysDown([VK_MENU,VK_SHIFT,VK_TAB]);
    KeysUp([VK_SHIFT,VK_TAB]);
  end else if func='ShiftCtrlEsc' then begin
    FlushKeyboard(true);
    KeysDown([VK_CONTROL,VK_SHIFT,VK_ESCAPE]);
    Sleep(50);
    KeysUp([VK_SHIFT,VK_CONTROL,VK_ESCAPE]);
  end else if func='CtrlEsc' then begin
    FlushKeyboard(true);
    KeysDown([VK_CONTROL,VK_ESCAPE]);
    Sleep(50);
    KeysUp([VK_ESCAPE,VK_CONTROL]);
  end else if func='AltEsc' then begin
    FlushKeyboard(true);
    KeysDown([VK_MENU,VK_ESCAPE]);
    KeysUp([VK_ESCAPE]);
  end else if func='AltDel' then begin
    FlushKeyboard(true);
    KeysDown([VK_MENU,VK_SPACE]);
    KeysUp([VK_SPACE]);
  end else if func='LeftWin' then begin
    FlushKeyboard(true);
    KeysDown([VK_LWIN]);
    KeysUp([VK_LWIN]);
  end else if func='RightWin' then begin
    FlushKeyboard(true);
    KeysDown([VK_RWIN]);
    KeysUp([VK_RWIN]);
  end else if func='PrtScr' then begin
    FlushKeyboard(true);
    KeysDown([VK_SNAPSHOT]);
    Sleep(50);
    KeysUp([VK_SNAPSHOT]);
  end else if func='AltPrtScr' then begin
    FlushKeyboard(true);
    KeysDown([VK_MENU,VK_SNAPSHOT]);
    Sleep(50);
    KeysUp([VK_MENU, VK_SNAPSHOT]);
  end;
  result:=true;
end;

function TAbstractScraper.KeysDown(vks:array of integer):boolean;
var
  n,sc : integer;
begin
  for n := 0 to Length(vks) - 1 do begin
    sc:=MapVirtualKey(vks[n],0);
    keybd_event(vks[n],sc, 0,0);
    AddKeyDownToList(vks[n]);
  end;
  result:=true;
end;

function TAbstractScraper.KeysUp(vks:array of integer):boolean;
var
  n,sc : integer;
begin
  for n := 0 to Length(vks) - 1 do begin
    sc:=MapVirtualKey(vks[n],0);
    keybd_event(vks[n],sc, KEYEVENTF_KEYUP,0);
    FKeyDownList.Remove(TObject(vks[n]));
  end;
  result:=true;
end;

procedure TAbstractScraper.Lock;
begin
  FMirrorListCs.Enter;
end;

procedure TAbstractScraper.UnLock;
begin
  FMirrorListCs.Leave;
end;

procedure TAbstractScraper.ClearWindows;
begin
  Lock;
  try
    MirrorList.Clear;
  finally
    Unlock;
  end;
end;

procedure TAbstractScraper.ProcessKeyboardInput(key,ch: word; Down:boolean);
  procedure DumpKeys;
  var n : Integer;
    S : string;
  begin
    S:='';
    for n := 0 to FKeyDownList.Count - 1 do begin
      if S<>'' then S:=S+',';
      S:=S+IntToStr(Integer(FKeyDownList[n]));
    end;
    LogInfo(Format('Keys Pressed: [%s]',[S]));
  end;

  function AreVkInList(vks:array of Integer):Boolean;
  var n,m : Integer;
    found : Boolean;
  begin
    result:=false;
    if length(vks)>FKeyDownList.Count then exit;

    found:=false;
    for n := 0 to FKeyDownList.Count - 1 do begin
      found:=false;
      for m := 0 to Length(vks) - 1 do
        if vks[m]=Integer(FKeyDownList[n]) then begin
          found:=true;
          break;
        end;
      if not found then break;
    end;
    result:=found;
  end;

  procedure RemoveKeys(vks:array of Integer);
  var n,m : Integer;
  begin
    for n :=FKeyDownList.Count - 1 downto 0 do begin
      for m := 0 to Length(vks) - 1 do
        if Integer(FKeyDownList[n]) = vks[m] then begin
          FKeyDownList.delete(n);
          break;
        end;
    end;
  end;

  function ValidCombination:Boolean;
    function GetVk(C:Char):word;
    begin
      result:=VkKeyScanEx(C,0);
    end;
  begin
    result:=false;
    DumpKeys;
    if AreVkInList([VK_MENU,VK_PRIOR]) then begin
      RemoveKeys([VK_PRIOR]);
      result:=PressFunctionKey('AltTab');
    end else if AreVkInList([VK_MENU,VK_NEXT]) then begin
      RemoveKeys([VK_NEXT]);
      result:=PressFunctionKey('AltShiftTab');
    end else if AreVkInList([VK_MENU,VK_INSERT]) then begin
      RemoveKeys([VK_MENU,VK_INSERT]);
      KeysUp([VK_MENU,VK_INSERT]);
      result:=PressFunctionKey('AltEsc');
    end else if AreVkInList([VK_MENU,VK_HOME]) then begin
      RemoveKeys([VK_MENU,VK_HOME]);
      KeysUp([VK_MENU,VK_HOME]);
      result:=PressFunctionKey('CtrlEsc');
    end else if AreVkInList([VK_MENU,VK_DELETE]) then begin
      RemoveKeys([VK_MENU,VK_DELETE]);
      KeysUp([VK_MENU,VK_DELETE]);
      result:=PressFunctionKey('AltDel');
    end else if AreVkInList([VK_CONTROL,VK_MENU,VK_END]) then begin
      RemoveKeys([VK_CONTROL,VK_MENU,VK_END]);
      KeysUp([VK_CONTROL,VK_MENU,VK_END]);
      result:=PressFunctionKey('CtrlAltDel');
    end else if AreVkInList([VK_CONTROL,VK_MENU,VK_ADD]) then begin
      RemoveKeys([VK_CONTROL,VK_MENU,VK_ADD]);
      KeysUp([VK_CONTROL,VK_MENU,VK_ADD]);
      result:=PressFunctionKey('AltPrtScr');
    end else if AreVkInList([VK_CONTROL,VK_MENU,VK_SUBTRACT]) then begin
      RemoveKeys([VK_CONTROL,VK_MENU,VK_SUBTRACT]);
      KeysUp([VK_CONTROL,VK_MENU,VK_SUBTRACT]);
      result:=PressFunctionKey('PrtScr');
    end else if AreVkInList([VK_CONTROL,GetVk('C')]) then
    else if AreVkInList([VK_CONTROL,GetVk('X')]) then
    else if AreVkInList([VK_CONTROL,GetVk('V')]) then
    else if AreVkInList([VK_CONTROL,GetVk('Z')]) then;
  end;
  function IsExtendedVK(vk: Integer): Boolean;
  begin
    Result := (vk = VK_LEFT)
           or (vk = VK_RIGHT)
           or (vk = VK_UP)
           or (vk = VK_DOWN)
           or (vk = VK_SCROLL)
           or (vk = VK_CAPITAL)
           or (vk = VK_NUMLOCK)
           or (vk = VK_HOME)
           or (vk = VK_END)
           or (vk = VK_PRIOR)
           or (vk = VK_NEXT)
           or (vk = VK_INSERT)
           or (vk = VK_DELETE)
  end;
var
  sc, flags: Integer;
  Input : TInput;
begin
  SwitchToActiveDesktop;
  FKeyCs.Enter;
  try
    if down and not IsInputAllowed(GetforegroundWindow) then exit;

    // generates keydown when is lost
    if not down and (FKeyDownList.IndexOf(TObject(key))=-1) and
      AreVkInList([VK_MENU,VK_CONTROL]) then
      ProcessKeyboardInput(key,ch,true);

    FLastKeytime := Now;
    LogInfo(Format('ProcessKeyboardInput key:%d char:%d down:%s',[key,ch,BoolToStr(down,true)]));
    if down then begin
      AddKeyDownToList(key);
      if ValidCombination then exit;
    end else FKeyDownList.Remove(TObject(key));
  finally
    FKeyCs.Leave;
  end;
  sc:=MapVirtualKey(key,0);
  if down then flags := 0
  else flags := KEYEVENTF_KEYUP;
  if IsExtendedVK(key) then flags := flags or KEYEVENTF_EXTENDEDKEY;

  if (ch>0) then begin
    Input.Itype:=INPUT_KEYBOARD;
    with Input do begin
      ki.wScan:=sc;
      ki.wVk:=key;
      if (ch>0) then begin
        ki.wVk:=0;
        ki.wScan:=ch;
        flags:= KEYEVENTF_UNICODE;
        if not down then flags:=flags or KEYEVENTF_KEYUP;
      end;
      ki.dwFlags:=flags;
      ki.time:=0;
      ki.dwExtraInfo:=0;
    end;
    SendInput(1,Input,sizeof(TInput));
  end else
   keybd_event(key, sc, flags, 0);
end;

procedure TAbstractScraper.AddKeyDownToList(key:word);
begin
  if FKeyDownList.IndexOf(TObject(key))=-1 then
    FKeyDownList.Add(TObject(key));
end;

{ TThinScreenScraper }

constructor TThinScreenScraper.Create(AOwner:TComponent);
begin
  inherited;
  FDisableAero:=true;
  FDisableWallPaper:=false;
  FVideoCs:=TCriticalSection.Create;
  FUseVideoDriver:=true;
end;

destructor TThinScreenScraper.Destroy;
begin
  FreeAndnil(FVideoCs);
  MyScreen:=Screen;
  inherited;
end;

function TThinScreenScraper.GetDevicePixelFormat: TPixelFormat;
var
  DC : HDC;
  bpp: Integer;
begin
  DC:=GetDC(0);
  try
    bpp := GetDeviceCaps(DC, BITSPIXEL);
    case bpp of
      1..4  : result:=pf4bit;
      5..8  : result:=pf8bit;
      9..16 : result:=pf16bit;
      17..24: result:=pf24bit;
      32:     result:=pf32bit;
      else    LogError('Unexpected BITSPIXEL value: ' + IntToStr(bpp));
              result:=pf16bit;
    end;
  finally
    ReleaseDC(0,DC);
  end;
end;

function TThinScreenScraper.DVHeight(Index: Integer): Integer;
begin
  if Index=-1 then
    result:=MyScreen.DesktopHeight
  else result:=MyScreen.Monitors[Index].Height;
end;

function TThinScreenScraper.DVLeft(Index: Integer): Integer;
begin
  if Index=-1 then
    result:=MyScreen.DesktopLeft
  else result:=MyScreen.Monitors[Index].Left;
end;

function TThinScreenScraper.DVRect(Index: Integer): TRect;
begin
  result:=Rect(DVLeft(Index),DVTop(Index),DVWidth(Index)+DVLeft(Index),DVHeight(Index)+DVTop(Index));
end;

function TThinScreenScraper.DVTop(Index: Integer): Integer;
begin
  if Index=-1 then
    result:=MyScreen.DesktopTop
  else result:=MyScreen.Monitors[Index].Top;
end;

function TThinScreenScraper.DVWidth(Index: Integer): Integer;
begin
  if Index=-1 then
    result:=MyScreen.DesktopWidth
  else result:=MyScreen.Monitors[Index].Width;
end;

function  TThinScreenScraper.GetMirrorWindow(AWinList:TMirrorWindowList;AHandle:THandle):TMirrorWindow;
var n : Integer;
begin
  result:=nil;
  for n:=AWinList.Count-1 downto 0 do
    if TMirrorWindow(AWinList[n]).FHandle=AHandle then begin
      result:= TMirrorWindow(AWinList[n]);
      break;
    end;
end;

function TThinScreenScraper.RefreshMirrorList:boolean;
 function GetSignature(List:TMirrorWindowList):string;
  var
    I: Integer;
    wm : TMirrorWindow;
  begin
    result:='';
    for I := 0 to List.Count - 1 do
    begin
      wm := List[I];
      result:=result+Format('h:%d-%d-%d-%d-%d;',[wm.Handle,wm.BoundsRect.Left,wm.BoundsRect.Right,
        wm.BoundsRect.Right,wm.BoundsRect.Bottom]);
    end;
  end;

  procedure GetProcessWindowList(WinList:TWinDataList);
  begin
    WinList.Clear;
    EnumDesktopWindows(GetThreadDesktop(GetCurrentThreadId),@EnumWindowsProc, Longint(WinList));
  end;

var
  wl : TWinDataList;
  n,Delay,Count,size: Integer;
  wm : TMirrorWindow;
  forewnd : THandle;
  UpdateRgn,Rgn : HRGN;
  R : TRect;
  RgnData: PRgnData;
  OldSignature,aux : string;
  P :PAnsiChar;
  PR : PRect;
//  Log : ILogger;
begin
//  Log := TLogger.Create(self,'RefreshMirrorList');

  OldSignature:=GetSignature(FMirrorList);

  for n:=FMirrorList.Count-1 downto 0 do begin
    wm:=FMirrorList[n];

    if not IsWindow(wm.Handle) then begin
      FMirrorList.Delete(n);
    end else if not IsWindowVisible(wm.Handle) and
        not IsRectEmpty(wm.BoundsRect) then begin
      wm.SetBoundsRect(Rect(0,0,0,0));
    end;
  end;

  UpdateRgn:=CreateRectRgn(0,0,0,0);
  if FUseChangedRgn then
    CombineRgn(UpdateRgn,UpdateRgn,FChangedRgn,RGN_OR);

  forewnd := GetForegroundWindow;
  wl := TWinDataList.Create;
  try
    // Enumerates top windows
    GetProcessWindowList(wl);
    try
      for n := wl.Count - 1 downto 0 do begin
        // Looks for a cached window
        wm:=GetMirrorWindow(FMirrorList,wl[n].Wnd);
        if assigned(wm) then begin
          R:=wm.BoundsRect;

          if IsIconic(wl[n].Wnd) or not IsWindowVisible(wl[n].Wnd) or
            (wm.DesktopName<>FCurrentDesktopName) then begin
            wm.SetBoundsRect(Rect(0,0,0,0));
            if (wm.DesktopName<>FCurrentDesktopName) then
              R:=wm.BoundsRect;
          end else wm.SetBoundsRect(wl[n].Rect);

          if FUseChangedRgn and not EqualRect(R,wm.BoundsRect) and IsRectEmpty(R) then
            AccumChangedRgn(wm.BoundsRect);

                    IntersectRect(R,MyScreen.DesktopRect,wm.BoundsRect);
          Rgn:=CreateRectRgnIndirect(R);
          CombineRgn(UpdateRgn, UpdateRgn,Rgn, RGN_OR);
          DeleteObject(Rgn);
        end else begin
          // Do not create a TMirrorWindow for invisible windows
          if IsIconic(wl[n].Wnd) then Continue;

          wm:=TMirrorWindow.Create(Self,wl[n].Wnd,wl[n].Rect, wl[n].pid,wl[n].Classname,FCurrentDesktopName);
          FMirrorList.Add(wm);

          if FUseChangedRgn then begin
            Rgn:=CreateRectRgnIndirect(wm.BoundsRect);
            CombineRgn(UpdateRgn, UpdateRgn,Rgn, RGN_OR);
            DeleteObject(Rgn);
          end;

                  end;

        // Saves the zIndex
        wm.FIndex:=wl.Count-n;
        // Generates clipping regions
        wm.GenRegions(wl,n);
      end;


    finally
      wl.Clear;
    end;

    
    result:=GetRegionCount(UpdateRgn)>0;

    for n := 0 to FMirrorList.Count - 1 do begin
      OffsetRgn(UpdateRgn,-FMirrorList[n].BoundsRect.Left,-FMirrorList[n].BoundsRect.Top);
      FMirrorList[n].CreateUpdateRgn(UpdateRgn);
      OffsetRgn(UpdateRgn,FMirrorList[n].BoundsRect.Left,FMirrorList[n].BoundsRect.Top);
      if FUseChangedRgn then
        FMirrorList[n].AccumChangedParentRgn(FChangedRgn);
    end;

    if UpdateRgn<>0 then
      DeleteObject(UpdateRgn);

    // Sorts the mirror list by zIndex
    FMirrorList.Sort(CompareTMirrorWindow);

    FMirrorListChanged:=OldSignature<>GetSignature(FMirrorList);
  finally
    wl.free;
  end;
end;


function TThinScreenScraper.VideoDriverInstalled:Boolean;
begin

  result:=False;
end;

procedure TThinScreenScraper.SessionAdded(Sender:TObject);
begin
  inherited;
  if CaptureThread.SessionCount>1 then exit;

  InitVideoDriver;

  
    {$IFDEF ENABLE_DESKTOP_UTILS}
      
  DesktopUtils.EnableAero(not FDisableAero);
        DesktopUtils.EnableWallpaper(not FDisableWallpaper);
    {$ENDIF}
    if Screen<>MyScreen then FreeAndNil(MyScreen);
  MyScreen := TScreen.Create(nil);
end;

procedure TThinScreenScraper.SessionRemoved(Sender:TObject);
begin
  inherited;
  if CaptureThread.SessionCount>0 then exit;
    try
    {$IFDEF ENABLE_DESKTOP_UTILS}
    DesktopUtils.Rollback;
    {$ENDIF}
  finally
      end;
  if Screen<>MyScreen then FreeAndNil(MyScreen);
  MyScreen:=Screen;
end;

procedure TThinScreenScraper.InitVideoDriver;
var
  pf : TPixelFormat;
  fd : Integer;
begin
  end;

procedure TThinScreenScraper.SetUseVideoDriver(const Value: Boolean);
begin
  if FUseVideoDriver <> Value then begin
    FUseVideoDriver := Value;
    FVideoCs.Enter;
    try
      {$IFDEF ENABLE_DESKTOP_UTILS}
      DesktopUtils.CheckVideoDriver := FUseVideoDriver;
      {$ENDIF}
      if not FUseVideoDriver then begin
        ClearChangedRgn;
            end else if CaptureThread.SessionCount>=1 then
        InitVideoDriver;
    finally
      FVideoCs.Leave;
    end;
  end;
end;


function TThinScreenScraper.Capture:boolean;
  var
  wm : TMirrorWindow;
  forewnd : THandle;
  n,idx : Integer;
  mskd : boolean;
  rc : Boolean;
//  Log : ILogger;
begin
//  Log := Tlogger.Create(Self,'Capture');
  FVideoCs.Enter;
  try
    Lock;
    try
          FCurrentDesktopName:=SwitchToActiveDesktop;

    
      result:=false;
    
      if not RefreshMirrorList and not result then begin
        result:=FMirrorListChanged;
        exit;
      end;
      result:=FMirrorListChanged;
    
      forewnd:=GetForegroundWindow;
      for n:=FMirrorList.Count-1 downto 0 do begin
        wm:=FMirrorList[n];
            if not assigned(wm.FBitmapCache) and not wm.FByPass then begin
                  wm.FBitmapCache:=wm.CreateBitmap;
        end;
        rc:=wm.Capture;
        result:=result or rc;
      end;

      if result then Inc(FCaptureIndex);
    finally
      Unlock;
    end;
  finally
    FVideoCs.Leave;
  end;
end;


{ TWinData }

constructor TWinData.Create(AWnd: HWND; ARect: TRect; APid: Cardinal;AClassname:string);
begin
  Wnd:=AWnd;
  Rect:=ARect;
  Pid:=APid;
  Classname:=AClassname;
end;

{ TWinDataList }

function TWinDataList.Add(Item: TWinData): Integer;
begin
  Result := inherited Add(Item);
end;

function TWinDataList.GetItems(Index: Integer): TWinData;
begin
  Result := TWinData(inherited Items[Index]);
end;

{ TMirrorWindowList }

function TMirrorWindowList.Add(Item: TMirrorWindow): Integer;
begin
  Result := inherited Add(Item);
end;

function TMirrorWindowList.GetItems(Index: Integer): TMirrorWindow;
begin
  Result := TMirrorWindow(inherited Items[Index]);
end;

function TMirrorWindowList.GetItemsByHandle(Index: Integer): TMirrorWindow;
var
  n : Integer;
begin
  result:=nil;
  for n := 0 to Count - 1 do
    if GetItems(n).Handle=index then begin
      result:=GetItems(n);
      exit;
    end;
end;


initialization
  MyScreen := Screen;
end.
