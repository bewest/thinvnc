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

uses Activex,Windows,Classes,SysUtils,Forms,Contnrs,Graphics,SyncObjs, StrUtils,
  math,pngimage,jpeg, IniFiles,
  ThinVnc.Log,
  ThinVnc.Cache,
  ThinVnc.Utils;

const
  CREATE_TIME = 50;
  MOVE_TIME = 150;
  MIN_TIME = 100;
  MAX_TIME = 150;
  MAX_POLLTIME = 0;
  INITIAL_POLL_FREQUENCY = 25;

type
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

  
  TMirrorManager = class;
  TMirrorWindow = class
  private
    FIndex : Integer;
    FMirrorManager : TMirrorManager;
    FRgn : HRGN;
    FHandle : THandle;
    FBoundsRect : TRect;
    FProcessId : Integer;
    FMoved : TDatetime;
    FMinMax : TDateTime;
    FClassname : string;
    FDesktopName : string;
    FLastpoll : TDatetime;
    FPollFrequency : Integer;
    function GetActive: Boolean;
    procedure SetBoundsRect(ARect:TRect);
  public
    constructor Create(AMirrorManager:TMirrorManager;AHandle:THandle;ARect:TRect;APid:Integer;AClassname,ADesktopName:string);
    destructor  Destroy;override;
    function ExtractClippingRegions(R: TRect): TRectArray;
    function CreateBitmap: TBitmap;
    function Capture(ANewImage: TBitmap): Boolean;
    function MustPoll(Count:Integer):Boolean;
    procedure GenRegions(wl: TWinDataList;AIndex:Integer);
    property zIndex:Integer read FIndex;
    property Active:Boolean read GetActive;
    property Handle:THandle read FHandle;
    property ProcessId:Integer read FProcessId;
    property MirrorManager:TMirrorManager read FMirrorManager;
    property BoundsRect:TRect read FBoundsRect;
    property Classname:string read FClassname;
    property DesktopName:string read FDesktopName;
  end;

  TMirrorWindowList = class(TObjectList)
  private
    function GetItems(Index: Integer): TMirrorWindow;
  public
    function Add(Item: TMirrorWindow): Integer;
    property Items[Index: Integer]: TMirrorWindow read GetItems; default;
  end;

  TIntArray = array of integer;
  TMirrorManager = class
  private
    FMirrorList : TMirrorWindowList;
    FLastFullPoll : TDateTime;
    FLastCapture : THashedStringList;
    FCaptureIndex,FCaptureIndexSent : Integer;
    FOneMoved: Boolean;
    FCache : TCache;
    FMirrorListChanged : Boolean;
    FCurrentDesktopName: string;
    function    GetMirrorWindow(AWinList:TMirrorWindowList;AHandle: THandle): TMirrorWindow;
    procedure   SaveToFile(json: string);
    function   RefreshMirrorList:boolean;
   function GetRegionCount(UpdateRgn: HRGN): Integer;
  public
    constructor Create;virtual;
    destructor  Destroy;override;
    procedure   ConfirmUpdate;
    function    Capture:boolean;
    property    MirrorList : TMirrorWindowList read FMirrorList;
    property    OneMoved:Boolean read FOneMoved;
    property    CaptureIndex:Integer read FCaptureIndex;
    property    LastCapture : THashedStringList read FLastCapture;
    property    Cache : TCache read FCache;
    property    MirrorListChanged : Boolean read FMirrorListChanged;
    property    CurrentDesktopName:string read FCurrentDesktopName;
  end;

implementation

function IsInIgnoreClassList(Classname:string;Wnd:HWnd):Boolean;
begin
  result:=Pos(Classname+';','TThinVNCPM;TThinVncAppBorder;TSgAppBorder;SysShadow;DimmedWindowClass;TGlassForm;')>0;
end;

function EnumWindowsProc(Wnd: HWnd; const obj:TWinDataList): Bool; export; stdcall;
var ProcessId : Cardinal;
  R,R1 : TRect;
  Win : TWinData;
  ExStyle : DWORD;
  Classname : string;
begin
  Result:=True;
  GetWindowThreadProcessId(Wnd,ProcessId);
  if IsWindowVisible(Wnd){ and not IsIconic(wnd)}then begin
    GetWindowRect(Wnd,R);
    IntersectRect(R1,R,Screen.DesktopRect);
    if not IsRectEmpty(R1) or IsIconic(wnd) then begin

      // Ignore some windows
      Classname:=GetWindowClassName(wnd);
      if IsInIgnoreClassList(Classname,wnd) then exit;
      ExStyle:=GetWindowLong(Wnd, GWL_EXSTYLE);
      if ((ExStyle and WS_EX_LAYERED) = WS_EX_LAYERED) and (Classname='#32768') then
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

constructor TMirrorWindow.Create(AMirrorManager:TMirrorManager;AHandle:THandle;ARect:TRect;APid:Integer;AClassname,ADesktopName:string);
begin
  inherited Create;
  FMirrorManager:=AMirrorManager;
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
  if (FRgn<>0) then
    DeleteObject(FRgn);
  inherited;
end;

function TMirrorWindow.CreateBitmap:TBitmap;
begin
  result := TBitmap.Create;
  result.PixelFormat:=pf16bit;
  result.Width:=Width(FBoundsRect);
  result.Height:=Height(FBoundsRect);
end;

function TMirrorWindow.GetActive: Boolean;
begin
  result:=IsWindow(FHandle) and IsWindowVisible(FHandle){ and not IsIconic(FHandle)};
end;

function TMirrorWindow.MustPoll(Count:Integer): Boolean;
begin
  result:=(FMirrorManager.FCurrentDesktopName=FDesktopName) and
    (DateTimeToTimeStamp(Now-FLastPoll).time>=Min(FPollFrequency*((Count-zIndex)),MAX_POLLTIME));
  if result then FLastPoll:=Now;
end;

function TMirrorWindow.ExtractClippingRegions(R: TRect): TRectArray;
var n,size : Integer;
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

    size:=GetRegionData(Rgn,0,nil);
    if size>0 then begin
      RgnData:=AllocMem(size);
      try
        Count:=GetRegionData(Rgn,size,RgnData);
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
    end;
  finally
    DeleteObject(Rgn);
  end;
end;

procedure TMirrorWindow.SetBoundsRect(ARect: TRect);
var
  TmpImage : TBitmap;
begin
  if not EqualRect(FBoundsRect,ARect) then begin
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

function TMirrorWindow.Capture(ANewImage:TBitmap): Boolean;
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
  result:=false;

  if Width(FBoundsRect)*Height(FBoundsRect)=0 then exit;
  if not IsWindowVisible(FHandle) or IsIconic(FHandle) then exit;

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

{ TMirrorManager }

constructor TMirrorManager.Create;
begin
  FLastCapture := THashedStringList.Create;
  FMirrorList := TMirrorWindowList.Create;

  FCache := TCache.Create;
end;

destructor TMirrorManager.Destroy;
begin
  FreeAndNil(FLastCapture);
  FreeAndNil(FMirrorList);
  FreeAndNil(FCache);
  inherited;
end;

procedure TMirrorManager.SaveToFile(json:string);
var
  sstream : TMemoryStream;
  path : string;
begin
  path:=GetModulePath+'Debug\';
  if not DirectoryExists(path) then exit;

  sstream := TMemoryStream.Create;
  try
    if json <> '' then
      sstream.Write(json[1], Length(json));
    sstream.SaveToFile(Format('%s%.3d_json.js',[path,FCaptureIndex]));
  finally
    sstream.Free;
  end;
end;

procedure TMirrorManager.ConfirmUpdate;
begin
  FCaptureIndexSent:=FCaptureIndex;
end;

function  TMirrorManager.GetMirrorWindow(AWinList:TMirrorWindowList;AHandle:THandle):TMirrorWindow;
var n : Integer;
begin
  result:=nil;
  for n:=AWinList.Count-1 downto 0 do
    if TMirrorWindow(AWinList[n]).FHandle=AHandle then begin
      result:= TMirrorWindow(AWinList[n]);
      break;
    end;
end;

function TMirrorManager.GetRegionCount(UpdateRgn:HRGN):Integer;
var
  Size : Integer;
  RgnData: PRgnData;
  P :PAnsiChar;
  PR : PRect;
  n : Integer;
begin
  size:=GetRegionData(UpdateRgn,0,nil);
  if size>0 then begin
    RgnData:=AllocMem(size);
    try
      GetRegionData(UpdateRgn,size,RgnData);
//      LogInfo(Format('GetRegionCount Count:%d',[RgnData^.rdh.nCount]));
      p:=@RgnData.Buffer[0];
      result:=RgnData^.rdh.nCount;
      for n := 0 to result - 1 do begin
        PR:=PRect(p);
//        LogInfo(Format('Region Rect:%d,%d,%d,%d',
//            [PR^.Left,PR^.Top,PR^.Right,PR^.Bottom]));
        Inc(p,SizeOf(TRect));
      end;
    finally
      Freemem(RgnData);
    end;
  end else begin
//    LogInfo(Format('GetRegionCount Count:%d',[0]));
    result:=0;
  end;
end;

function TMirrorManager.RefreshMirrorList:boolean;
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
  n,Delay,Count,size,LastzIndex : Integer;
  wm : TMirrorWindow;
  forewnd : THandle;
  UpdateRgn,Rgn : HRGN;
  R : TRect;
  RgnData: PRgnData;
  OldSignature : string;
  P :PAnsiChar;
  PR : PRect;
//  Log : ILogger;
begin
//  Log := TLogger.Create(self,'RefreshMirrorList');

  OldSignature:=GetSignature(FMirrorList);

  for n:=FMirrorList.Count-1 downto 0 do begin

    if not IsWindow(FMirrorList[n].Handle) then begin
      FMirrorList.Delete(n);
    end else if not IsWindowVisible(FMirrorList[n].Handle) then
      FMirrorList[n].SetBoundsRect(Rect(0,0,0,0))
  end;

  UpdateRgn:=CreateRectRgn(0,0,0,0);

  LastzIndex:=0;
  // Look for last zindex for windows belonging to invisible desktops
  for n:=0 to FMirrorList.Count-1 do begin
    if (FMirrorList[n].DesktopName<>FCurrentDesktopName) and (LastzIndex<FMirrorList[n].FIndex) then
       LastzIndex:=FMirrorList[n].FIndex;
  end;

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

          if IsIconic(wl[n].Wnd) then
            wm.SetBoundsRect(Rect(0,0,0,0))
          else wm.SetBoundsRect(wl[n].Rect);

                    if wm.MustPoll(FMirrorList.Count) or (wm.Handle=forewnd) then begin
            IntersectRect(R,Screen.DesktopRect,wm.BoundsRect);
            Rgn:=CreateRectRgnIndirect(R);
            CombineRgn(UpdateRgn, UpdateRgn,Rgn, RGN_OR);
            DeleteObject(Rgn);
//            LogInfo(Format('CanCapture Handle:%d, Classname:%s',[wm.Handle,wm.FClassname]));
          end;
        end else begin
          // Do not create a TMirrorWindow for invisible windows
          if IsIconic(wl[n].Wnd) then Continue;

          wm:=TMirrorWindow.Create(Self,wl[n].Wnd,wl[n].Rect, wl[n].pid,wl[n].Classname,FCurrentDesktopName);
          FMirrorList.Add(wm);

                  end;

        // Saves the zIndex
        wm.FIndex:=wl.Count-n+LastzIndex;
        // Generates clipping regions
        wm.GenRegions(wl,n);
      end;


    finally
      wl.Clear;
    end;

    
    result:=GetRegionCount(UpdateRgn)>0;

    for n := 0 to FMirrorList.Count - 1 do begin
      OffsetRgn(UpdateRgn,-FMirrorList[n].BoundsRect.Left,-FMirrorList[n].BoundsRect.Top);
      CombineRgn(FMirrorList[n].FRgn,FMirrorList[n].FRgn,UpdateRgn, RGN_AND);
      OffsetRgn(UpdateRgn,FMirrorList[n].BoundsRect.Left,FMirrorList[n].BoundsRect.Top);
    end;

    if UpdateRgn<>0 then
      DeleteObject(UpdateRgn);

    // Sorts the mirror list by zIndex
    FMirrorList.Sort(CompareTMirrorWindow);

    FMirrorListChanged:=OldSignature<>GetSignature(FMirrorList);
{    if FMirrorListChanged then
      LogInfo('FMirrorList changed.');
}  finally
    wl.free;
  end;
end;


function TMirrorManager.Capture:boolean;
var
  wm : TMirrorWindow;
  bmp : TBitmap;
  forewnd : THandle;
  n : Integer;
  rc : Boolean;
//  Log : ILogger;
begin
//  Log := Tlogger.Create(Self,'Capture');

  FCurrentDesktopName:=SwitchToActiveDesktop;

  result:=false;
  if not RefreshMirrorList then begin
    result:=FMirrorListChanged;
    exit;
  end;
  result:=FMirrorListChanged;

  for n := FLastCapture.Count - 1 downto 0 do
    FLastCapture.Objects[n].Free;
  FLastCapture.Clear;

  forewnd:=GetForegroundWindow;
  for n:=FMirrorList.Count-1 downto 0 do begin
    wm:=FMirrorList[n];
    bmp:=wm.CreateBitmap;
    rc:=wm.Capture(bmp);
    if rc then
      FLastCapture.AddObject(IntToStr(wm.FHandle), bmp)
    else bmp.Free;
    result:=result or rc;
  end;

  if FCaptureIndexSent=FCaptureIndex then
    Inc(FCaptureIndex);
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

end.
