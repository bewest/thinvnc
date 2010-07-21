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

unit ThinVnc.OneInstance;
{$I ThinVnc.DelphiVersion.inc}
{$I ThinVnc.inc}
interface

uses
  Windows, SysUtils, Classes, Messages, AccCtrl, ThinVnc.Utils;

function RegisterInstance(CheckParams: Boolean = True; Global: Boolean = False): Boolean; overload;

implementation

var
  gSessionMutex: Cardinal = 0;
  gGlobalMutex: Cardinal = 0;
  
function RegisterInstance(MutexName: string; Global: Boolean): Boolean; overload;
var
  SecurityAttrPtr: PSecurityAttributes;
  SecurityAttr: TSecurityAttributes;
  SecurityDescriptor: TSecurityDescriptor;

    procedure CreateSecurityDescriptor;

        function NTSetPrivilege(sPrivilege: string; bEnabled: Boolean): Boolean;
        var
          hToken: THandle;
          TokenPriv: TOKEN_PRIVILEGES;
          PrevTokenPriv: TOKEN_PRIVILEGES;
          ReturnLength: Cardinal;
        begin
          Result := True;
          // Only for Windows NT/2000/XP and later.
          if not (Win32Platform = VER_PLATFORM_WIN32_NT) then Exit;
          // obtain the processes token
          if OpenProcessToken(GetCurrentProcess(),
            TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then
          begin
            try
              // Get the locally unique identifier (LUID) .
              if LookupPrivilegeValue(nil, PChar(sPrivilege),
                TokenPriv.Privileges[0].Luid) then
              begin
                TokenPriv.PrivilegeCount := 1; // one privilege to set

                case bEnabled of
                  True: TokenPriv.Privileges[0].Attributes  := SE_PRIVILEGE_ENABLED;
                  False: TokenPriv.Privileges[0].Attributes := 0;
                end;

                ReturnLength := 0; // replaces a var parameter
                PrevTokenPriv := TokenPriv;

                // enable or disable the privilege

                Windows.AdjustTokenPrivileges(hToken, False, TokenPriv, SizeOf(PrevTokenPriv),
                  PrevTokenPriv, ReturnLength);
              end;
            finally
              CloseHandle(hToken);
            end;
          end;
          // test the return value of AdjustTokenPrivileges.
          Result := GetLastError = ERROR_SUCCESS;
        //  if not Result then
        //    raise Exception.Create(SysErrorMessage(GetLastError));
        end;

    type
      SetEntriesInAclProc = function(cCountOfExplicitEntries: ULONG; pListOfExplicitEntries: PEXPLICIT_ACCESS_;
             OldAcl: PACL; var NewAcl: ACL): DWORD; stdcall;
    var
      ExplicitAccess: array[0..2] of EXPLICIT_ACCESS;
      Dacl: ACL;
      P: Pointer;
      LibH: Cardinal;
    begin
      SecurityAttrPtr := @SecurityAttr;
      SecurityAttr.nLength:= SizeOf(SecurityAttr);
      SecurityAttr.bInheritHandle:= True;
      SecurityAttr.lpSecurityDescriptor:= @SecurityDescriptor;
      InitializeSecurityDescriptor(@SecurityDescriptor, SECURITY_DESCRIPTOR_REVISION);

      SetSecurityDescriptorControl(SecurityAttr.lpSecurityDescriptor, SE_DACL_AUTO_INHERIT_REQ or SE_SACL_AUTO_INHERIT_REQ, SE_DACL_AUTO_INHERIT_REQ or SE_SACL_AUTO_INHERIT_REQ);
      InitializeAcl(Dacl, SizeOf(Dacl), 1{ACL_REVISION});

      ExplicitAccess[0].grfAccessPermissions:= EVENT_ALL_ACCESS;
      ExplicitAccess[0].grfAccessMode:= GRANT_ACCESS;
      ExplicitAccess[0].grfInheritance:= NO_INHERITANCE;
      ExplicitAccess[0].Trustee.MultipleTrusteeOperation:= NO_MULTIPLE_TRUSTEE;
      // change these informations to grant access to a group or other user
      ExplicitAccess[0].Trustee.TrusteeForm:= TRUSTEE_IS_NAME;
      ExplicitAccess[0].Trustee.TrusteeType:= TRUSTEE_IS_USER;
      ExplicitAccess[0].Trustee.ptstrName:= 'CURRENT_USER'; //'CURRENT_USER';  // 'CREATOR OWNER'

      ExplicitAccess[1].grfAccessPermissions:= EVENT_ALL_ACCESS;
      ExplicitAccess[1].grfAccessMode:= GRANT_ACCESS;
      ExplicitAccess[1].grfInheritance:= NO_INHERITANCE;
      ExplicitAccess[1].Trustee.MultipleTrusteeOperation:= NO_MULTIPLE_TRUSTEE;
      // change these informations to grant access to a group or other user
      ExplicitAccess[1].Trustee.TrusteeForm:= TRUSTEE_IS_NAME;
      ExplicitAccess[1].Trustee.TrusteeType:= TRUSTEE_IS_USER;
      ExplicitAccess[1].Trustee.ptstrName:= 'CURRENT_USER'; //'CURRENT_USER';  // 'CREATOR OWNER'

      ExplicitAccess[2].grfAccessPermissions:= SECTION_ALL_ACCESS;
      ExplicitAccess[2].grfAccessMode:= GRANT_ACCESS;
      ExplicitAccess[2].grfInheritance:= NO_INHERITANCE;
      ExplicitAccess[2].Trustee.MultipleTrusteeOperation:= NO_MULTIPLE_TRUSTEE;
      // change these informations to grant access to a group or other user
      ExplicitAccess[2].Trustee.TrusteeForm:= TRUSTEE_IS_NAME;
      ExplicitAccess[2].Trustee.TrusteeType:= TRUSTEE_IS_USER;
      ExplicitAccess[2].Trustee.ptstrName:= 'CURRENT_USER'; //'CURRENT_USER';  // 'CREATOR OWNER'

      LibH:= LoadLibrary('ADVAPI32.DLL');
      if LibH <> 0 then
      try
        P:= GetProcAddress(LibH, 'SetEntriesInAclA');
        if P <> nil then
          SetEntriesInAclProc(P)(3, @ExplicitAccess, nil, Dacl);
      finally
        FreeLibrary(LibH);
      end;

      SetSecurityDescriptorDacl(SecurityAttr.lpSecurityDescriptor, True, nil {@Dacl {@Dacl{ nil = all access }, False);

      NTSetPrivilege('SeCreateGlobalPrivilege', True);
      NTSetPrivilege('SeCreatePagefilePrivilege', True);
    end;
begin
  Result := True;
  gSessionMutex := CreateMutex(nil, True, PAnsiChar(MutexName));
  if GetLastError = ERROR_ALREADY_EXISTS then Result := False else
  if gSessionMutex = 0 then Result := False else
  if Global then
  begin
    CreateSecurityDescriptor;
    gGlobalMutex := CreateMutex(nil, True, PAnsiChar(MutexName));
    if GetLastError = ERROR_ALREADY_EXISTS then Result := False else
    if gGlobalMutex = 0 then Result := False;
  end;
end;

function RegisterInstance(CheckParams: Boolean; Global: Boolean): Boolean;
var
  S: string;
begin
  if CheckParams then
  begin
    S := StrPas(GetCommandLine);
    if S = '' then S := ExtractFileName(GetModuleName(hInstance));
  end else begin
    S := ExtractFileName(GetModuleName(hInstance));
  end;
  S := StringReplace(S, ' ', '', [rfReplaceAll]);
  S := EncodeBase64(S);
  S := StringReplace(S, '/', '_', [rfReplaceAll]);
  S := StringReplace(S, '+', '_', [rfReplaceAll]);
  Result := RegisterInstance(S, Global);
end;

procedure CloseMutex(var Mutex: Cardinal);
begin
  if (Mutex <> 0) and (Mutex <> INVALID_HANDLE_VALUE) then
  begin
    ReleaseMutex(Mutex);
    CloseHandle(Mutex);
  end;
  Mutex := 0;
end;

initialization
finalization
  CloseMutex(gSessionMutex);
  CloseMutex(gGlobalMutex);
end.
