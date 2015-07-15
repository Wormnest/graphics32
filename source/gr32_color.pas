unit GR32_Color;
(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Michael Hansen <dyster_tid@hotmail.com>
 *   Andre Beckedorf <Andre@metaException.de>
 *   Mattias Andersson <mattias@centaurix.com>
 *   J. Tulach <tulach@position.cz>
 *
 * ***** END LICENSE BLOCK ***** *)

// jb split of from GR32 to help fpc compilation

interface

{$I GR32.inc}

uses
  Classes, SysUtils, Graphics, Windows;

{ 32-bit Color }

type
  PColor32 = ^TColor32;
  TColor32 = type Cardinal;

  PColor32Array = ^TColor32Array;
  TColor32Array = array [0..0] of TColor32;
  TArrayOfColor32 = array of TColor32;

  PPalette32 = ^TPalette32;
  TPalette32 = array [Byte] of TColor32;

const
  // Some predefined color constants
  clBlack32               = TColor32($FF000000);
  clDimGray32             = TColor32($FF3F3F3F);
  clGray32                = TColor32($FF7F7F7F);
  clLightGray32           = TColor32($FFBFBFBF);
  clWhite32               = TColor32($FFFFFFFF);
  clMaroon32              = TColor32($FF7F0000);
  clGreen32               = TColor32($FF007F00);
  clOlive32               = TColor32($FF7F7F00);
  clNavy32                = TColor32($FF00007F);
  clPurple32              = TColor32($FF7F007F);
  clTeal32                = TColor32($FF007F7F);
  clRed32                 = TColor32($FFFF0000);
  clLime32                = TColor32($FF00FF00);
  clYellow32              = TColor32($FFFFFF00);
  clBlue32                = TColor32($FF0000FF);
  clFuchsia32             = TColor32($FFFF00FF);
  clAqua32                = TColor32($FF00FFFF);

  // Some semi-transparent color constants
  clTrWhite32             = TColor32($7FFFFFFF);
  clTrBlack32             = TColor32($7F000000);
  clTrRed32               = TColor32($7FFF0000);
  clTrGreen32             = TColor32($7F00FF00);
  clTrBlue32              = TColor32($7F0000FF);


// Color construction and conversion functions
function Color32(WinColor: TColor): TColor32; overload;
function Color32(R, G, B: Byte; A: Byte = $FF): TColor32; overload;
function Color32(Index: Byte; var Palette: TPalette32): TColor32; overload;
function Gray32(Intensity: Byte; Alpha: Byte = $FF): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}
function WinColor(Color32: TColor32): TColor;
function ArrayOfColor32(Colors: array of TColor32): TArrayOfColor32;

// Color component access
procedure Color32ToRGB(Color32: TColor32; var R, G, B: Byte);
procedure Color32ToRGBA(Color32: TColor32; var R, G, B, A: Byte);
function RedComponent(Color32: TColor32): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}
function GreenComponent(Color32: TColor32): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}
function BlueComponent(Color32: TColor32): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}
function AlphaComponent(Color32: TColor32): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}
function Intensity(Color32: TColor32): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}
function SetAlpha(Color32: TColor32; NewAlpha: Integer): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}

// Color space conversion
function HSLtoRGB(H, S, L: Single): TColor32;
procedure RGBtoHSL(RGB: TColor32; out H, S, L : Single);

{$IFNDEF CLX}
// Palette conversion functions
function WinPalette(const P: TPalette32): HPALETTE;
{$ENDIF}

{ Gamma bias for line/pixel antialiasing }

var
  GAMMA_TABLE: array [Byte] of Byte;
  Interpolator: function(WX_256, WY_256: Cardinal; C11, C21: PColor32): TColor32;

procedure SetGamma(Gamma: Single = 0.7);


// ----- jb Functions moved here from LowLevel -----

{ Clamp function restricts Value to [0..255] range }
function Clamp(const Value: Integer): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}

{ ColorSwap exchanges ARGB <-> ABGR and fill A with $FF }
function ColorSwap(WinColor: TColor): TColor32;


implementation

uses Math,
  GR32_LowLevel;

{ Color construction and conversion functions }

function Color32(WinColor: TColor): TColor32; overload;
{$IFDEF WIN_COLOR_FIX}
var
  I: Longword;
{$ENDIF}
begin
{$IFDEF CLX}
  WinColor := ColorToRGB(WinColor);
{$ELSE}
  if WinColor < 0 then WinColor := GetSysColor(WinColor and $000000FF);
{$ENDIF}

{$IFDEF WIN_COLOR_FIX}
  Result := $FF000000;
  I := (WinColor and $00FF0000) shr 16;
  if I <> 0 then Result := Result or TColor32(Integer(I) - 1);
  I := WinColor and $0000FF00;
  if I <> 0 then Result := Result or TColor32(Integer(I) - $00000100);
  I := WinColor and $000000FF;
  if I <> 0 then Result := Result or TColor32(Integer(I) - 1) shl 16;
{$ELSE}
  asm
        MOV    EAX,WinColor
        BSWAP  EAX
        MOV    AL,$FF
        ROR    EAX,8
        MOV    Result,EAX
  end;
{$ENDIF}
end;

function Color32(R, G, B: Byte; A: Byte = $FF): TColor32; overload;
asm
        MOV  AH,A
        SHL  EAX,16
        MOV  AH,DL
        MOV  AL,CL
end;

function Color32(Index: Byte; var Palette: TPalette32): TColor32; overload;
begin
  Result := Palette[Index];
end;

function Gray32(Intensity: Byte; Alpha: Byte = $FF): TColor32;
begin
  Result := TColor32(Alpha) shl 24 + TColor32(Intensity) shl 16 +
    TColor32(Intensity) shl 8 + TColor32(Intensity);
end;

function WinColor(Color32: TColor32): TColor;
asm
  // the alpha channel byte is set to zero!
        ROL    EAX,8  // ABGR  ->  BGRA
        XOR    AL,AL  // BGRA  ->  BGR0
        BSWAP  EAX    // BGR0  ->  0RGB
end;

function ArrayOfColor32(Colors: array of TColor32): TArrayOfColor32;
var
  L: Integer;
begin
  // build a dynamic color array from specified colors
  L := High(Colors) + 1;
  SetLength(Result, L);
  MoveLongword(Colors[0], Result[0], L);
end;

procedure Color32ToRGB(Color32: TColor32; var R, G, B: Byte);
begin
  R := (Color32 and $00FF0000) shr 16;
  G := (Color32 and $0000FF00) shr 8;
  B := Color32 and $000000FF;
end;

procedure Color32ToRGBA(Color32: TColor32; var R, G, B, A: Byte);
begin
  A := Color32 shr 24;
  R := (Color32 and $00FF0000) shr 16;
  G := (Color32 and $0000FF00) shr 8;
  B := Color32 and $000000FF;
end;

function RedComponent(Color32: TColor32): Integer;
begin
  Result := (Color32 and $00FF0000) shr 16;
end;

function GreenComponent(Color32: TColor32): Integer;
begin
  Result := (Color32 and $0000FF00) shr 8;
end;

function BlueComponent(Color32: TColor32): Integer;
begin
  Result := Color32 and $000000FF;
end;

function AlphaComponent(Color32: TColor32): Integer;
begin
  Result := Color32 shr 24;
end;

function Intensity(Color32: TColor32): Integer;
begin
// (R * 61 + G * 174 + B * 21) / 256
  Result := (
    (Color32 and $00FF0000) shr 16 * 61 +
    (Color32 and $0000FF00) shr 8 * 174 +
    (Color32 and $000000FF) * 21
    ) shr 8;
end;

function SetAlpha(Color32: TColor32; NewAlpha: Integer): TColor32;
begin
  if NewAlpha < 0 then NewAlpha := 0
  else if NewAlpha > 255 then NewAlpha := 255;
  Result := (Color32 and $00FFFFFF) or (TColor32(NewAlpha) shl 24);
end;

{ Color space conversions }

function HSLtoRGB(H, S, L: Single): TColor32;
const
  OneOverThree = 1 / 3;
var
  M1, M2: Single;
  R, G, B: Byte;

  function HueToColor(Hue: Single): Byte;
  var
    V: Double;
  begin
    Hue := Hue - Floor(Hue);
    if 6 * Hue < 1 then V := M1 + (M2 - M1) * Hue * 6
    else if 2 * Hue < 1 then V := M2
    else if 3 * Hue < 2 then V := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else V := M1;
    Result := Round(255 * V);
  end;

begin
  if S = 0 then
  begin
    R := Round(255 * L);
    G := R;
    B := R;
  end
  else
  begin
    if L <= 0.5 then M2 := L * (1 + S)
    else M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColor(H + OneOverThree);
    G := HueToColor(H);
    B := HueToColor(H - OneOverThree)
  end;
  Result := Color32(R, G, B, 255);
end;

procedure RGBtoHSL(RGB: TColor32; out H, S, L : Single);
var
  R, G, B, D, Cmax, Cmin: Single;
begin
  R := RedComponent(RGB) / 255;
  G := GreenComponent(RGB) / 255;
  B := BlueComponent(RGB) / 255;
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));
  L := (Cmax + Cmin) / 2;

  if Cmax = Cmin then
  begin
    H := 0;
    S := 0
  end
  else
  begin
    D := Cmax - Cmin;
    if L < 0.5 then S := D / (Cmax + Cmin)
    else S := D / (2 - Cmax - Cmin);
    if R = Cmax then H := (G - B) / D
    else
      if G = Cmax then H  := 2 + (B - R) / D
      else H := 4 + (R - G) / D;
    H := H / 6;
    if H < 0 then H := H + 1
  end;
end;

{ Palette conversion }

{$IFNDEF CLX}
function WinPalette(const P: TPalette32): HPALETTE;
var
  L: TMaxLogPalette;
  L0: LOGPALETTE absolute L;
  I: Cardinal;
  Cl: TColor32;
begin
  L.palVersion := $300;
  L.palNumEntries := 256;
  for I := 0 to 255 do
  begin
    Cl := P[I];
    with L.palPalEntry[I] do
    begin
      peFlags := 0;
      peRed := RedComponent(Cl);
      peGreen := GreenComponent(Cl);
      peBlue := BlueComponent(Cl);
    end;
  end;
  Result := CreatePalette(l0);
end;
{$ENDIF}


// ----- jb Functions moved here from LowLevel -----

{$R-}{$Q-}  // switch off overflow and range checking

function Clamp(const Value: Integer): TColor32;
begin
  if Value < 0 then Result := 0
  else if Value > 255 then Result := 255
  else Result := Value;
end;

{ Colorswap exchanges ARGB <-> ABGR and fill A with $FF }
function ColorSwap(WinColor: TColor): TColor32;
asm
// EAX = WinColor
// this function swaps R and B bytes in ABGR
// and writes $FF into A component
        BSWAP   EAX
        MOV     AL, $FF
        ROR     EAX,8
end;


{ Gamma / Pixel Shape Correction table }

procedure SetGamma(Gamma: Single);
var
  i: Integer;
begin
  for i := 0 to 255 do
    GAMMA_TABLE[i] := Round(255 * Power(i / 255, Gamma));
end;

end.

