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

{$IFNDEF VER80}
 {$IFNDEF VER90}
  {$IFNDEF VER93}
    {$DEFINE DELPHI3} { Delphi 3.0 or higher }
    {$UNDEF DELPHI4}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VER120} { Delphi 4.0 }
  {$DEFINE DELPHI4}
{$ENDIF}

{$IFDEF VER130} { Delphi 5.0 }
  {$DEFINE DELPHI4}
{$ENDIF}

{$IFDEF VER140} { Delphi 5.0 }
  {$DEFINE DELPHI6}
{$ENDIF}

{$IFDEF VER150} { Delphi 6.0 }
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI7}
{$ENDIF}

{$IFDEF VER160} { Delphi 8.0 }
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI7}
{$ENDIF}

{$IFDEF VER170} { Delphi 9.0 }
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI7}
  {$DEFINE DELPHI9}
{$ENDIF}

{$IFDEF VER180} { Delphi 10.0 }
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI7}
  {$DEFINE DELPHI9}
  {$DEFINE DELPHI10}
{$ENDIF}

{$IFDEF VER190} { Delphi 11.0 }
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI7}
  {$DEFINE DELPHI9}
  {$DEFINE DELPHI10}
  {$DEFINE DELPHI11}
  {$AUTOBOX ON}
{$ENDIF}

{$IFDEF VER200} { Delphi 12.0 - D2009 }
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI7}
  {$DEFINE DELPHI9}
  {$DEFINE DELPHI10}
  {$DEFINE DELPHI11}
  {$DEFINE DELPHI12}
  {$AUTOBOX ON}
{$ENDIF}

{$IFDEF VER210} { Delphi 12.0 - D2010 }
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI7}
  {$DEFINE DELPHI9}
  {$DEFINE DELPHI10}
  {$DEFINE DELPHI11}
  {$DEFINE DELPHI12}
  {$DEFINE DELPHI2010}
  {$AUTOBOX ON}
{$ENDIF}


