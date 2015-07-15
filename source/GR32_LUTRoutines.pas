unit GR32_LUTRoutines;

{*********************************************}
{  This unit contains some routines converted }
{  from FastLib and other sources.            }
{                                             }
{  Convertion to GR32 done by                 }
{  Andre Beckedorf                            }
{  metaException OHG                          }
{  email: Andre@metaException.de              }
{*********************************************}

interface

uses GR32, GR32_Filters, Math;

procedure ApplyLUTRGB(Dst, Src: TBitmap32; const LUT_R, LUT_G, LUT_B: TLUT8);
procedure FillLut(var Lut: TLUT8; x1, y1, x2, y2: Byte);
function ContrastLut(Amount: Integer): TLUT8;
function LightnessLut(Amount: Integer): TLUT8;
function AdditionLut(Amount: Integer): TLUT8;
function GammaLut(Amount: Extended): TLUT8;
function MergeLuts(Luts: array of TLUT8): TLUT8;

implementation

procedure ApplyLUTRGB(Dst, Src: TBitmap32; const LUT_R, LUT_G, LUT_B: TLUT8);
var
  I: Integer;
  D, S: PColor32;
  r, g, b: TColor32;
  C: TColor32;
begin
  CheckParams(Src, Dst);
  D := @Dst.Bits[0];
  S := @Src.Bits[0];

  for I := 0 to Src.Width * Src.Height - 1 do
  begin
    C := S^;
    r := C and $00FF0000;
    g := C and $0000FF00;
    r := r shr 16;
    b := C and $000000FF;
    g := g shr 8;
    r := LUT_R[r];
    g := LUT_G[g];
    b := LUT_B[b];
    D^ := $FF000000 or r shl 16 or g shl 8 or b;
    Inc(S);
    Inc(D);
  end;
  Dst.Changed;
end;

procedure FillLut(var Lut: TLUT8; x1, y1, x2, y2: Byte);
var
  x, n, i, ii: Integer;
begin
  n := x2 - x1;
  if n <> 0 then
  begin
    i := y1 shl 16;
    ii := ((y2 - y1 + 1) shl 16) div n;
    for x := x1 to x2 - 1 do
    begin
      Lut[x] := i shr 16;
        // this may create overflow of byte when i shr 16 evaluates to > 255...
      Inc(i, ii);
    end;
    // .. so we are going to force set the last cell:
    Lut[x2] := y2;
  end;
end;

function ContrastLut(Amount: Integer): TLUT8;
begin
  if Amount < 0 then
    FillLut(Result, 0, -Amount, 255, 255 + Amount)
  else
  begin
    if Amount > 255 then
      Amount := 255;
    FillChar(Result, Amount, 0);
    FillLut(Result, Amount, 0, 255 - Amount, 255);
    FillChar(Result[256 - Amount], Amount, 255);
  end;
end;

function LightnessLut(Amount: Integer): TLUT8;
begin
  if Amount < 0 then
    FillLut(Result, 0, 0, 255, 255 + Amount)
  else
  begin
    if Amount > 255 then
      Amount := 255;
    FillLut(Result, 0, Amount, 255, 255);
  end;
end;

function AdditionLut(Amount: Integer): TLUT8;
var
  i, x: Integer;
begin
  if Amount < 0 then
  begin
    if Amount < -255 then
      Amount := -255;
    FillChar(Result, -Amount, 0);
    x := 0;
    for i := -Amount to 255 do
    begin
      Result[i] := x;
      Inc(x);
    end;
  end
  else if Amount > 0 then
  begin
    if Amount > 255 then
      Amount := 255;
    x := Amount;
    for i := 0 to 255 - Amount do
    begin
      Result[i] := x;
      Inc(x);
    end;
    FillChar(Result[256 - Amount], Amount, 255);
  end
  else
    for i := 0 to 255 do
      Result[i] := i;
end;

function GammaLut(Amount: Extended): TLUT8;
var
  i, z: Integer;
  y: Extended;
begin
  if Amount > 0 then
  begin
    Result[0] := 0;
    y := 1 / Amount;
    for i := 1 to 255 do
    begin
      z := Round(255 * (Power(i / 256, y)));
      if z > 255 then
        z := 255
      else if z < 0 then
        z := 0;
      Result[i] := z;
    end;
  end;
end;

function MergeLuts(Luts: array of TLUT8): TLUT8;
var
  x, y, z: Integer;
begin
  x := Low(Luts);
  for y := 0 to 255 do
    Result[y] := Luts[x, y];
  for z := x + 1 to High(Luts) do
    for y := 0 to 255 do
      Result[y] := Luts[z, Result[y]];
end;

end.
