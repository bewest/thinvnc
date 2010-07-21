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

unit ThinVnc.Paths;
{$I ThinVnc.DelphiVersion.inc}
{$I ThinVnc.inc}
interface

uses
  Windows, SysUtils, Classes, Registry;

function ThinVncSettingsPath: string;

implementation

uses ThinVnc.Utils;

const

  THINVNC_KEY = '\SOFTWARE\ThinVnc';
  THINVNC_VALUE_CONFIG_PATH = 'ConfigPath';

function ThinVncSettingsPath: string;
var
  Reg: TRegistry;
  S: string;
begin
  Result := GetModulePath;
  Reg := TRegistry.Create(KEY_QUERY_VALUE or KEY_ENUMERATE_SUB_KEYS);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly(THINVNC_KEY) then
      if Reg.ValueExists(THINVNC_VALUE_CONFIG_PATH) then
      begin
        S := Reg.ReadString(THINVNC_VALUE_CONFIG_PATH);
        if S <> '' then
        begin
          ForceDirectories(S);
          Result := S;
        end;
      end;
  finally
    Reg.Free;
  end;
  Result := IncludeTrailingPathDelimiter(Result);
end;

end.
