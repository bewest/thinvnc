unit ThinVnc.Input;

(*:@author Gustavo Ricardi
   @desc <pre>

This software is distributed under the GPL license.

Copyright (c) 2010, Gustavo Ricardi
All rights reserved.
*)

interface
uses Windows,Classes,Forms;

procedure ProcessMouseInput(X,Y,Btn:Integer;action:string);

implementation

procedure ProcessMouseInput(X,Y,Btn:Integer;action:string);
var
  flags : DWORD;
  idealMouseInfo : array[0..2] of ULONG;
  MouseInfo : array[0..2] of ULONG;
  oldSpeed, newSpeed : ULONG;
begin
  X:=round(X/(Screen.DesktopWidth)*65535);
  Y:=round(Y/(Screen.DesktopHeight)*65535);
  if X<0 then X:=0 else if X>65535 then X:=65535;
  if Y<0 then Y:=0 else if Y>65535 then Y:=65535;


  flags:=0;
  if action='move' then begin
    idealMouseInfo[0]:=10;

 		SystemParametersInfo(SPI_GETMOUSE, 0, @mouseInfo, 0);
		SystemParametersInfo(SPI_GETMOUSESPEED, 0, @oldSpeed, 0);
		SystemParametersInfo(SPI_SETMOUSESPEED, 0, @newSpeed, 0);
		SystemParametersInfo(SPI_SETMOUSE, 0, @idealMouseInfo, 0);

    mouse_event(MOUSEEVENTF_MOVE or MOUSEEVENTF_ABSOLUTE,X,Y,0,0);

		SystemParametersInfo(SPI_SETMOUSE, 0, @mouseInfo, 0);
		SystemParametersInfo(SPI_SETMOUSESPEED, 0, @oldSpeed, 0);
    exit;
  end;

  if action='up' then begin
    case btn of
      1 : flags:=MOUSEEVENTF_LEFTUP;
      2 : flags:=MOUSEEVENTF_MIDDLEUP;
      3 : flags:=MOUSEEVENTF_RIGHTUP;
    end;
  end else if action='down' then begin
    case btn of
      1 : flags:=MOUSEEVENTF_LEFTDOWN;
      2 : flags:=MOUSEEVENTF_MIDDLEDOWN;
      3 : flags:=MOUSEEVENTF_RIGHTDOWN;
    end;
  end;
  flags:=flags or MOUSEEVENTF_MOVE or MOUSEEVENTF_ABSOLUTE;
  mouse_event(flags,X,Y,0,0);
end;

end.
