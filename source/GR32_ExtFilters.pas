unit GR32_ExtFilters;

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

uses GR32, GR32_Filters, GR32_LUTRoutines, Math, Windows;

type
  TColorBGRA = record // color splitting type
    b: Byte;
    g: Byte;
    r: Byte;
    a: Byte;
  end;
  PColorBGRA = ^TColorBGRA;

  PSaturationLUT = ^TSaturationLUT;
  TSaturationLUT = record
    Grays: array[0..767] of Integer;
    Alpha: array[Byte] of Word;
  end;

procedure RemoveNoise(Bitmap: TBitmap32; d: Byte);
procedure AddNoise(Bitmap: TBitmap32; Amount: Integer; Mono: Boolean);
procedure SplitBlur(Bitmap: TBitmap32; Split: Integer);
procedure RenderLightmapA(Bitmap: TBitmap32; Size: Integer);
procedure RenderLightmap(Bitmap: TBitmap32; Size: Integer);
procedure Mosaic(Bitmap: TBitmap32; xAmount, yAmount: Integer);
procedure AutoContrast(Bitmap: TBitmap32);
procedure AutoContrastB(Bitmap: TBitmap32);
function SaturationLUT(Amount: Integer): TSaturationLUT;
procedure ApplySaturationLut(Bitmap: TBitmap32; LUT: TSaturationLUT);
procedure Saturation(Bitmap: TBitmap32; Amount: Integer);

implementation

procedure ApplyLutB(Bitmap: TBitmap32; Lut: TLUT8);
var
  pb: PByte;
  x, y, w, h: Integer;
begin
  w := Bitmap.Width;
  h := Bitmap.Height;

  pb := Pointer(@Bitmap.Bits[0]);
  for y := 0 to h - 1 do
  begin
    for x := 0 to (w - 1) * 4 do
    begin
      pb^ := Lut[pb^];
      Inc(pb);
    end;
  end;
end;

procedure AutoContrastB(Bitmap: TBitmap32);
var
  Lut: TLUT8;
  pb: PByte;
  hi, lo: Byte;
  x, y, w, h: Integer;
begin
  w := Bitmap.Width;
  h := Bitmap.Height;

  hi := 0;
  lo := 255;
  pb := Pointer(@Bitmap.Bits[0]);
  for y := 0 to h - 1 do
  begin
    for x := 0 to (w - 1) * 4 do
    begin
      if pb^ > hi then
        hi := pb^;
      if pb^ < lo then
        lo := pb^;
      Inc(pb);
    end;
  end;
  if (lo <> 0) or (hi <> 255) then
  begin
    FillLut(Lut, lo, 0, hi, 255);
    ApplyLutB(Bitmap, Lut);
  end;
end;

procedure RemoveNoise(Bitmap: TBitmap32; d: Byte);
var
  pc: PColorBGRA;
  lst: TColorBGRA;
  x, y, w, h: Integer;
  rd, gd, bd: Byte;
begin
  w := Bitmap.Width;
  h := Bitmap.Height;

  pc := PColorBGRA(@Bitmap.Bits[0]);
  for y := 0 to h - 1 do
  begin
    lst := pc^;
    for x := 0 to w - 1 do
    begin
      bd := lst.b - pc.b;
      if bd < 0 then
        Inc(bd, bd + bd);
      gd := lst.g - pc.g;
      if gd < 0 then
        Inc(gd, gd + gd);
      rd := lst.r - pc.r;
      if rd < 0 then
        Inc(rd, rd + rd);
      if (bd > d) or (gd > d) or (rd > d) then
      begin
        lst.b := ((lst.b * (255 xor bd)) + (pc.b * bd)) shr 8;
        lst.g := ((lst.g * (255 xor gd)) + (pc.g * gd)) shr 8;
        lst.r := ((lst.r * (255 xor rd)) + (pc.r * rd)) shr 8;
      end;
      pc^ := lst;
      Inc(pc);
    end;
  end;

  for x := 0 to w - 1 do
  begin
    pc := PColorBGRA(Bitmap.PixelPtr[x, 0]);
    lst := pc^;
    for y := 0 to h - 1 do
    begin
      bd := lst.b - pc.b;
      if bd < 0 then
        Inc(bd, bd + bd);
      gd := lst.g - pc.g;
      if gd < 0 then
        Inc(gd, gd + gd);
      rd := lst.r - pc.r;
      if rd < 0 then
        Inc(rd, rd + rd);
      if (bd > d) or (gd > d) or (rd > d) then
      begin
        lst.b := ((lst.b * (255 xor bd)) + (pc.b * bd)) shr 8;
        lst.g := ((lst.g * (255 xor gd)) + (pc.g * gd)) shr 8;
        lst.r := ((lst.r * (255 xor rd)) + (pc.r * rd)) shr 8;
      end;
      pc^ := lst;
    end;
  end;
end;

procedure SplitBlur(Bitmap: TBitmap32; Split: Integer);
var
  n, s, e, d, x, y, w, h: Integer;
  Lin1, Lin2: PColor32Array;
  pc: PColorBGRA;
begin
  w := Bitmap.Width;
  h := Bitmap.Height;

  pc := PColorBGRA(@Bitmap.Bits[0]);
  for y := 0 to h - 1 do
  begin
    n := y + Split;
    if n > h - 1 then
      n := h - 1;
    s := y - Split;
    if s < 0 then
      s := 0;
    Lin1 := Bitmap.Scanline[s];
    Lin2 := Bitmap.Scanline[n];
    for x := 0 to w - 1 do
    begin
      e := x + Split;
      if e > w - 1 then
        e := w - 1;
      d := x - Split;
      if w < 0 then
        w := 0;
      pc.b := (TColorBGRA(Lin1[d]).b +
        TColorBGRA(Lin1[e]).b +
        TColorBGRA(Lin2[d]).b +
        TColorBGRA(Lin2[e]).b) shr 2;
      pc.g := (TColorBGRA(Lin1[d]).g +
        TColorBGRA(Lin1[e]).g +
        TColorBGRA(Lin2[d]).g +
        TColorBGRA(Lin2[e]).g) shr 2;
      pc.r := (TColorBGRA(Lin1[d]).r +
        TColorBGRA(Lin1[e]).r +
        TColorBGRA(Lin2[d]).r +
        TColorBGRA(Lin2[e]).r) shr 2;
      Inc(pc);
    end;
  end;
end;

procedure AddNoise(Bitmap: TBitmap32; Amount: Integer; Mono: Boolean);
var
  s, a, z, x, y, w, h: Integer;
  pa: PColorBGRA;
begin
  w := Bitmap.Width;
  h := Bitmap.Height;

  s := Amount shr 1;
  pa := PColorBGRA(@Bitmap.Bits[0]);
  if Mono then
  begin
    for y := 0 to h - 1 do
    begin
      for x := 0 to w - 1 do
      begin
        a := Random(Amount) - s;
        z := pa.b + a;
        if z > 255 then
          pa.b := 255
        else if z < 0 then
          pa.b := 0
        else
          pa.b := z;
        z := pa.g + a;
        if z > 255 then
          pa.g := 255
        else if z < 0 then
          pa.g := 0
        else
          pa.g := z;
        z := pa.r + a;
        if z > 255 then
          pa.r := 255
        else if z < 0 then
          pa.r := 0
        else
          pa.r := z;
        Inc(pa);
      end;
    end;
  end
  else
  begin
    for y := 0 to h - 1 do
    begin
      for x := 0 to w - 1 do
      begin
        z := pa.b + Random(Amount) - s;
        if z > 255 then
          pa.b := 255
        else if z < 0 then
          pa.b := 0
        else
          pa.b := z;
        z := pa.g + Random(Amount) - s;
        if z > 255 then
          pa.g := 255
        else if z < 0 then
          pa.g := 0
        else
          pa.g := z;
        z := pa.r + Random(Amount) - s;
        if z > 255 then
          pa.r := 255
        else if z < 0 then
          pa.r := 0
        else
          pa.r := z;
        Inc(pa);
      end;
    end;
  end;
end;

procedure RenderLightmapA(Bitmap: TBitmap32; Size: Integer);
var
  x, y, yy, f, r, i, w, h: Integer;
  pc: PColorBGRA;
begin
  w := Bitmap.Width;
  h := Bitmap.Height;

  r := Size shr 1;
  f := Round(65536 / (Size / ((256 / (Size / 2)) * 2)));

  pc := PColorBGRA(@Bitmap.Bits[0]);
  for y := 0 to h - 1 do
  begin
    yy := (y - r) * (y - r);
    for x := 0 to w - 1 do
    begin
      i := ((x - r) * (x - r) + yy) * f shr 16;
      if i > 255 then
        i := 255;
      i := i xor -1;
      pc.a := i;
      Inc(pc);
    end;
  end;
end;

procedure RenderLightmap(Bitmap: TBitmap32; Size: Integer);
var
  x, y, yy, f, r, i, w, h: Integer;
  pc: PColorBGRA;
begin
  w := Bitmap.Width;
  h := Bitmap.Height;

  r := Size shr 1;
  f := Round(65536 / (Size / ((256 / (Size / 2)) * 2)));

  pc := PColorBGRA(@Bitmap.Bits[0]);
  for y := 0 to h - 1 do
  begin
    yy := (y - r) * (y - r);
    for x := 0 to w - 1 do
    begin
      i := ((x - r) * (x - r) + yy) * f shr 16;
      if i > 255 then
        i := 255;
      i := i xor -1;
      pc.b := i;
      pc.g := i;
      pc.r := i;
      Inc(pc);
    end;
  end;
end;

procedure Mosaic(Bitmap: TBitmap32; xAmount, yAmount: Integer);
var
  Delta, tx, ty, ix, iy, cx, cy, x, y, w, h, wt4: Integer;
  Line: PColor32Array;
  pc: PColor32;
  tc: TColor32;
begin
  w := Bitmap.Width;
  wt4 := w * 4;
  h := Bitmap.Height;

  xAmount := Abs(xAmount);
  yAmount := Abs(yAmount);

  if (xAmount = 0) or (yAmount = 0) then
    Exit;

  ix := (xAmount shr 1) + (xAmount and 1);
  iy := (yAmount shr 1) + (yAmount and 1);
  y := 0;
  while y < h do
  begin
    x := 0;
    cy := y + iy;

    if cy >= h then
      Line := Bitmap.Scanline[h - 1]
    else
      Line := Bitmap.Scanline[cy];

    if y + yAmount - 1 > h - 1 then
      ty := h - y
    else
      ty := yAmount;

    while x < w do
    begin
      cx := x + ix;

      if cx >= w then
        tc := Line[w - 1]
      else
        tc := Line[cx];

      if x + xAmount - 1 > w - 1 then
        tx := w - x
      else
        tx := xAmount;

      Delta := Integer(wt4) - tx * 4;
      pc := Bitmap.PixelPtr[x, y];
      for cy := 1 to ty do
      begin
        for cx := 1 to tx do
        begin
          pc^ := tc;
          Inc(pc);
        end;
        pc := Ptr(Integer(pc) + Delta);
      end;
      Inc(x, xAmount);
    end;
    Inc(y, yAmount);
  end;
end;

procedure AutoContrast(Bitmap: TBitmap32);
var
  hr, hg, hb, lr, lg, lb: Byte;
  r, g, b: TLUT8;
  pc: PColorBGRA;
  x, y, w, h: Integer;
begin
  hr := 0;
  hg := 0;
  hb := 0;
  lr := 255;
  lg := 255;
  lb := 255;
  pc := PColorBGRA(@Bitmap.Bits[0]);
  w := Bitmap.Width;
  h := Bitmap.Height;

  for y := 0 to h - 1 do
  begin
    for x := 0 to w - 1 do
    begin
      if pc.b > hb then
        hb := pc.b;
      if pc.b < lb then
        lb := pc.b;
      if pc.g > hg then
        hg := pc.g;
      if pc.g < lg then
        lg := pc.g;
      if pc.r > hr then
        hr := pc.r;
      if pc.r < lr then
        lr := pc.r;
      Inc(pc);
    end;
  end;
  if ((lr or lg or lb) <> 0) or ((hr and hg and hb) <> 255) then
  begin
    FillLut(r, lr, 0, hr, 255);
    FillLut(g, lg, 0, hg, 255);
    FillLut(b, lb, 0, hb, 255);
    ApplyLutRGB(Bitmap, Bitmap, r, g, b);
  end;
end;

function SaturationLUT(Amount: Integer): TSaturationLUT;
var
  x, y, i: Integer;
begin
  x := 0;
  for i := 1 to 256 do
    Result.Alpha[i - 1] := (i * Amount) shr 8;
  for i := 1 to 256 do
  begin
    y := i - Result.Alpha[i - 1];
    Result.Grays[x] := y;
    Inc(x);
    Result.Grays[x] := y;
    Inc(x);
    Result.Grays[x] := y;
    Inc(x);
  end;
end;

procedure ApplySaturationLUT(Bitmap: TBitmap32; LUT: TSaturationLUT);
var
  g, x, y, z, w, h: Integer;
  pc: PColorBGRA;
begin
  pc := PColorBGRA(@Bitmap.Bits[0]);
  w := Bitmap.Width;
  h := Bitmap.Height;

  for y := 0 to h - 1 do
  begin
    for x := 0 to w - 1 do
    begin
      g := Lut.Grays[pc.b + pc.g + pc.r];
      z := Lut.Alpha[pc.b] + g;
      if z > 255 then
        z := 255
      else if z < 0 then
        z := 0;
      pc.b := z;
      z := Lut.Alpha[pc.g] + g;
      if z > 255 then
        z := 255
      else if z < 0 then
        z := 0;
      pc.g := z;
      z := Lut.Alpha[pc.r] + g;
      if z > 255 then
        z := 255
      else if z < 0 then
        z := 0;
      pc.r := z;
      Inc(pc);
    end;
  end;
end;

procedure Saturation(Bitmap: TBitmap32; Amount: Integer);
begin
  ApplySaturationLUT(Bitmap, SaturationLut(Amount));
end;

end.
