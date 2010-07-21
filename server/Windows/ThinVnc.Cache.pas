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

unit ThinVnc.Cache;
{$I ThinVnc.DelphiVersion.inc}
{$I ThinVnc.inc}
interface

uses Classes,Sysutils,SyncObjs;

type
  TCacheItem = class
  private
    FImageStream : TStream;
    FImageType : string;
    FAdded : TDateTime;
    FID : string;
  public
    constructor Create(AID,AImageType:string;AImageStream:TStream);
    destructor Destroy;override;
    property ImageType : string read FImageType;
    function GetStream:TStream;
    function ID:string;
  end;

  TCache = class
  private
    FItems : TStringList;
    FCs : TCriticalSection;
    procedure ClearOld;
    procedure ClearAll;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Add(AID,AImageType:string;AImageStream: TStream);
    function  Extract(AID:string):TCacheItem;
  end;

implementation

{ TCacheItem }

constructor TCacheItem.Create(AID,AImageType:string;AImageStream: TStream);
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

function TCacheItem.GetStream: TStream;
begin
  FImageStream.Seek(0,0);
  result:=FImageStream;
end;

function TCacheItem.ID: string;
begin
  result:=FID;
end;

{ TCache }

constructor TCache.Create;
begin
  FItems := TStringList.Create;
  FCs := TCriticalSection.Create;
end;

destructor TCache.Destroy;
begin
  ClearAll;
  FreeAndNil(FItems);
  FCs.Free;
  inherited;
end;

procedure TCache.Add(AID, AImageType: string; AImageStream: TStream);
begin
  ClearOld;
  FItems.AddObject(AID,TCacheItem.Create(AID,AImageType,AImageStream));
end;

function TCache.Extract(AID: string): TCacheItem;
var
  idx : Integer;
begin
  FCs.Enter;
  try
    result:=nil;
    idx:=FItems.IndexOf(AID);
    if idx>-1 then begin
      result:=FItems.Objects[idx] as TCacheItem;
      FItems.Delete(idx);
    end;
  finally
    FCs.Leave;
  end;
end;

procedure TCache.ClearOld;
var
  n : Integer;
  ci : TCacheItem;
begin
  FCs.Enter;
  try
    for n := FItems.Count-1 downto 0 do begin
      ci := FItems.Objects[n] as TCacheItem;
      if (DatetimeToTimestamp(now-ci.FAdded).Time>30000) then begin
        FItems.Delete(n);
        ci.Free;
      end;
    end;
  finally
    FCs.Leave;
  end;
end;

procedure TCache.ClearAll;
var
  n : Integer;
  ci : TCacheItem;
begin
  FCs.Enter;
  try
    for n := FItems.Count-1 downto 0 do begin
      ci := FItems.Objects[n] as TCacheItem;
      FItems.Delete(n);
      ci.Free;
    end;
  finally
    FCs.Leave;
  end;
end;

end.
