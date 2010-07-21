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

unit ThinVnc.InputQueue;
{$I ThinVnc.DelphiVersion.inc}
{$I ThinVnc.inc}
interface

uses Windows,Messages, Classes, SysUtils, Contnrs, SyncObjs,
  ThinVnc.Log;

type
  TThinQueueItem = class
  private
    FLength: Integer;
    FBuffer: AnsiString;
    FTimestamp: TDateTime;
  public
    constructor Create(ABuffer: AnsiString);
    property Buffer: AnsiString read FBuffer;
    property Length: Integer read FLength;
    property Timestamp: TDateTime read FTimestamp;
  end;

  TThinProcessQueueItemEvent = procedure (Sender: TObject; Item: TThinQueueItem) of object;
  TThinBufferQueueThread = class(TThread)
  private
    FQueue: TQueue;
    FItem: TThinQueueItem;
    FLocker: TCriticalSection;
    FEvent: TEvent;
    FOnProcess: TThinProcessQueueItemEvent;
    FSynchronous: Boolean;
    function GetCount: Integer;
    procedure SyncProcessItem;
  protected
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Terminate; reintroduce; virtual;

    procedure AddBuffer(Buffer: AnsiString);
    procedure Clear;
    property Count: Integer read GetCount;
    property Synchronous:Boolean read FSynchronous write FSynchronous;
    property OnProcess: TThinProcessQueueItemEvent read FOnProcess write FOnProcess;
  end;


implementation

{ TThinQueueItem }

constructor TThinQueueItem.Create(ABuffer: AnsiString);
begin
  inherited Create;
  FBuffer := ABuffer;
  FLength := System.Length(FBuffer);
  FTimestamp := Now;
end;

{ TThinBufferQueueThread }

procedure TThinBufferQueueThread.AddBuffer(Buffer: AnsiString);
begin
  FLocker.Enter;
  try
    FQueue.Push(TThinQueueItem.Create(Buffer));
    LogInfo('Queue.Addbuffer: '+Buffer);
    FEvent.SetEvent;
  finally
    FLocker.Leave;
  end;
end;

procedure TThinBufferQueueThread.Clear;
var
  I: Integer;
begin
  FLocker.Enter;
  try
    while FQueue.Count > 0 do
      TThinQueueItem(FQueue.Pop).Free;
  finally
    FLocker.Leave;
  end;
end;

constructor TThinBufferQueueThread.Create;
begin
  inherited Create(True);
  FQueue := TQueue.Create;
  FLocker := TCriticalSection.Create;
  FEvent := TEvent.Create;
  FreeOnTerminate := True;
  Suspended := False;
end;

destructor TThinBufferQueueThread.Destroy;
begin
  Clear;
  FreeAndNil(FQueue);
  FreeAndNil(FLocker);
  FreeAndNil(FEvent);
  inherited;
end;

procedure TThinBufferQueueThread.Execute;
var
  Item: TThinQueueItem;
  msg : TMsg;
begin
//  PeekMessage(msg, 0, WM_USER, WM_USER, PM_NOREMOVE); { Create message queue }
  while not Terminated do
  begin
{    if PeekMessage(msg, 0, 0, 0, PM_REMOVE) then
      DispatchMessage(msg);
}    FEvent.WaitFor(INFINITE);
    if Terminated then Break;
    FLocker.Enter;
    try
      if FQueue.Count = 0 then
      begin
        FEvent.ResetEvent;
        Item := nil;
      end else begin
        Item := FQueue.Pop;
      end;
    finally
      FLocker.Leave;
    end;
    if Item <> nil then
      try
        try
          LogInfo('Queue.Execute: '+Item.FBuffer);
          if Synchronous then begin
            FItem:=Item;
            Synchronize(SyncProcessItem);
          end else if Assigned(FOnProcess) then
            FOnProcess(Self, Item);
        finally
          Item.Free;
        end;
      except end;
  end;
end;

procedure TThinBufferQueueThread.SyncProcessItem;
begin
  if Assigned(FOnProcess) then
    FOnProcess(Self, FItem);
end;

function TThinBufferQueueThread.GetCount: Integer;
begin
  FLocker.Enter;
  try
    Result := FQueue.Count;
  finally
    FLocker.Leave;
  end;
end;

procedure TThinBufferQueueThread.Terminate;
begin
  inherited Terminate;
  FEvent.SetEvent;
end;

end.
