unit GR32_Types;

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

interface

{$I GR32.inc}

uses
  Classes, SysUtils, Windows, Graphics,
  GR32_Color, GR32_Blend;

// JB 2015-06-23 Moved base types from GR32 here because of problems in fpc
// with circular references between GR32_Bland and GR32.

{ TBitmap32 draw mode // jb moved to GR32_Blend }
//type
//  TDrawMode = (dmOpaque, dmBlend, dmCustom);
//  TCombineMode = (cmBlend, cmMerge);

type
{ Stretch filters }
  TStretchFilter = (sfNearest, sfDraft, sfLinear, sfCosine, sfSpline,
    sfLanczos, sfMitchell);


// jb moved from GR32_Transforms to GR32_Types
type
  ETransformError = class(Exception);

// jgb moved from GR32
// jgb 2012-04-14 I need to be able to distinguish gr32 exceptions from other
// general exceptions to be able to handle some of them gracefully
type
  ECantAllocateDIBHandleException = class(Exception);
  ECantCreateCompatibleDCException = class(Exception);
  ECantSelectObjectIntoDCException = class(Exception);

  // jb moved to GR32_Types
{ A fixed-point type }

type
  // this type has data bits arrangement compatible with Windows.TFixed
  TFixed = type Integer;
  PFixed = ^TFixed;

  // a little bit of fixed point math
  function Fixed(S: Single): TFixed; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
  function Fixed(I: Integer): TFixed; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
  function FixedFloor(A: TFixed): Integer;
  function FixedCeil(A: TFixed): Integer;
  function FixedMul(A, B: TFixed): TFixed;
  function FixedDiv(A, B: TFixed): TFixed;
  function FixedRound(A: TFixed): Integer;

{ Fixedmath related constants }

const
  FixedOne = $10000;
  FixedPI  = Round( PI * FixedOne );
  FixedToFloat = 1/FixedOne;

{ Points }

type
{$IFNDEF BCB}
  TPoint = {$IFDEF CLX}Types{$ELSE}Windows{$ENDIF}.TPoint;
{$ENDIF}
  PPoint = ^TPoint;
  TFloatPoint = record
    X, Y: Single;
  end;
  PFloatPoint = ^TFloatPoint;
  TFixedPoint = record
    X, Y: TFixed;
  end;
  PFixedPoint = ^TFixedPoint;
  TArrayOfPoint = array of TPoint;
  TArrayOfArrayOfPoint = array of TArrayOfPoint;
  TArrayOfFloatPoint = array of TFloatPoint;
  TArrayOfArrayOfFloatPoint = array of TArrayOfFloatPoint;
  TArrayOfFixedPoint = array of TFixedPoint;
  TArrayOfArrayOfFixedPoint = array of TArrayOfFixedPoint;

// jb moved to GR32_Types
{ Rectangles }

type
  TFloatRect = packed record
    case Integer of
      0: (Left, Top, Right, Bottom: Single);
      1: (TopLeft, BottomRight: TFloatPoint);
  end;
  TFixedRect = packed record
    case Integer of
      0: (Left, Top, Right, Bottom: TFixed);
      1: (TopLeft, BottomRight: TFixedPoint);
  end;
  TRectRounding = (rrClosest, rrOutside, rrInside);

  // Rectangle construction/conversion functions
  function MakeRect(const L, T, R, B: Integer): TRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
  function MakeRect(const FR: TFloatRect; Rounding: TRectRounding = rrClosest): TRect; overload;
  function MakeRect(const FXR: TFixedRect; Rounding: TRectRounding = rrClosest): TRect; overload;
  function FixedRect(const L, T, R, B: TFixed): TFixedRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
  function FixedRect(const ARect: TRect): TFixedRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
  function FixedRect(const FR: TFloatRect): TFixedRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
  function FloatRect(const L, T, R, B: Single): TFloatRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
  function FloatRect(const ARect: TRect): TFloatRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
  function FloatRect(const FXR: TFixedRect): TFloatRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

  // Some basic operations over rectangles
  function IntersectRect(out Dst: TRect; const R1, R2: TRect): Boolean;
  function IntersectRectF(out Dst: TFloatRect; const FR1, FR2: TFloatRect): Boolean;
  function EqualRect(const R1, R2: TRect): Boolean; {$IFDEF USEINLINING} inline; {$ENDIF}
  procedure InflateRect(var R: TRect; Dx, Dy: Integer); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
  procedure InflateRectF(var FR: TFloatRect; Dx, Dy: Single); {$IFDEF USEINLINING} inline; {$ENDIF}
  procedure OffsetRect(var R: TRect; Dx, Dy: Integer); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
  procedure OffsetRectF(var FR: TFloatRect; Dx, Dy: Single); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
  function IsRectEmpty(const R: TRect): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
  function IsRectEmptyF(const FR: TFloatRect): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
  function PtInRect(const R: TRect; const P: TPoint): Boolean; {$IFDEF USEINLINING} inline; {$ENDIF}

{ Other dynamic arrays }
type
  TArrayOfByte = array of Byte;
  TArrayOfInteger = array of Integer;
  TArrayOfArrayOfInteger = array of TArrayOfInteger;
  TArrayOfSingle = array of Single;


type
  // jb moved TThreadPersistent and TCustomMap from GR32_Types 2015-06-23
  { TThreadPersistent }
  { TThreadPersistent is an ancestor for TBitmap32 object. In addition to
    TPersistent methods, it provides thread-safe locking and change notification }
  TThreadPersistent = class(TPersistent)
  private
    FLock: TRTLCriticalSection;
    FLockCount: Integer;
    FUpdateCount: Integer;
    FOnChange: TNotifyEvent;
  protected
    property LockCount: Integer read FLockCount;
    property UpdateCount: Integer read FUpdateCount;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Changed; virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Lock;
    procedure Unlock;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TCustomMap }
  { An ancestor for bitmaps and similar 2D distributions wich have width and
    height properties }
  TCustomMap = class(TThreadPersistent)
  private
    FHeight: Integer;
    FWidth: Integer;
    FOnResize: TNotifyEvent;
    procedure SetHeight(NewHeight: Integer);
    procedure SetWidth(NewWidth: Integer);
  protected
    procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer); virtual;
  public
    procedure Delete; virtual;
    function  Empty: Boolean; virtual;
    procedure Resized; virtual;
    function SetSizeFrom(Source: TPersistent): Boolean;
    function SetSize(NewWidth, NewHeight: Integer): Boolean; virtual;
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
  end;

  TPixelCombineEvent = procedure(F: TColor32; var B: TColor32; M: TColor32) of object;

  // jb added TCustomBitmap32
  TCustomBitmap32 = class(TCustomMap)
  private
    FBits: PColor32Array;
    FCanvas: TCanvas;
    FClipRect: TRect;
    FFixedClipRect: TFixedRect;
    F256ClipRect: TRect;
    FClipping: Boolean;
    FDrawMode: TDrawMode;
    FFont: TFont;
    FHandle: HBITMAP;
    FHDC: HDC;
    FBitmapInfo: TBitmapInfo;
    FMasterAlpha: Cardinal;
    FOuterColor: TColor32;
    FStippleCounter: Single;
    FStipplePattern: TArrayOfColor32;
    FStippleStep: Single;
    FStretchFilter: TStretchFilter;
    FOnHandleChanged: TNotifyEvent;
    FOnPixelCombine: TPixelCombineEvent;
    FCombineMode: TCombineMode;
    procedure FontChanged(Sender: TObject);
    procedure CanvasChanged(Sender: TObject);
    function  GetCanvas: TCanvas;
    function  GetPixel(X, Y: Integer): TColor32;
    function  GetPixelS(X, Y: Integer): TColor32;
    function  GetPixelPtr(X, Y: Integer): PColor32;
    function  GetScanLine(Y: Integer): PColor32Array;
    procedure SetCombineMode(const Value: TCombineMode);
    procedure SetDrawMode(Value: TDrawMode);
    procedure SetFont(Value: TFont);
    procedure SetMasterAlpha(Value: Cardinal);
    procedure SetPixel(X, Y: Integer; Value: TColor32);
    procedure SetPixelS(X, Y: Integer; Value: TColor32);
    procedure SetStretchFilter(Value: TStretchFilter);
    procedure UpdateClipRects;
    procedure SetClipRect(const Value: TRect);
  protected
    FontHandle: HFont;
    procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer); override;
    procedure HandleChanged; virtual;
    procedure SET_T256(X, Y: Integer; C: TColor32);
    procedure SET_TS256(X, Y: Integer; C: TColor32);
    function  GET_T256(X, Y: Integer): TColor32;
    function  GET_TS256(X, Y: Integer): TColor32;
    procedure ReadData(Stream: TStream); virtual;
    procedure WriteData(Stream: TStream); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function  BoundsRect: TRect;
    function  Empty: Boolean; override;
    procedure Clear; overload;
    procedure Clear(FillColor: TColor32); overload;
    procedure ResetAlpha; overload;
    procedure ResetAlpha(const AlphaValue: Byte); overload;
    procedure SetPixelT(X, Y: Integer; Value: TColor32); overload;
    procedure SetPixelT(var Ptr: PColor32; Value: TColor32); overload;
    procedure SetPixelTS(X, Y: Integer; Value: TColor32);

    procedure SetStipple(NewStipple: TArrayOfColor32); overload;
    procedure SetStipple(NewStipple: array of TColor32); overload;
    procedure AdvanceStippleCounter(LengthPixels: Single);
    function  GetStippleColor: TColor32;
    procedure HorzLine(X1, Y, X2: Integer; Value: TColor32);
    procedure HorzLineS(X1, Y, X2: Integer; Value: TColor32);
    procedure HorzLineT(X1, Y, X2: Integer; Value: TColor32);
    procedure HorzLineTS(X1, Y, X2: Integer; Value: TColor32);
    procedure HorzLineTSP(X1, Y, X2: Integer);

    procedure VertLine(X, Y1, Y2: Integer; Value: TColor32);
    procedure VertLineS(X, Y1, Y2: Integer; Value: TColor32);
    procedure VertLineT(X, Y1, Y2: Integer; Value: TColor32);
    procedure VertLineTS(X, Y1, Y2: Integer; Value: TColor32);
    procedure VertLineTSP(X, Y1, Y2: Integer);

    procedure Line(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure LineS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure LineT(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure LineTS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure LineA(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure LineAS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure LineX(X1, Y1, X2, Y2: TFixed; Value: TColor32; L: Boolean = False); overload;
    procedure LineF(X1, Y1, X2, Y2: Single; Value: TColor32; L: Boolean = False); overload;
    procedure LineXS(X1, Y1, X2, Y2: TFixed; Value: TColor32; L: Boolean = False); overload;
    procedure LineFS(X1, Y1, X2, Y2: Single; Value: TColor32; L: Boolean = False); overload;
    procedure LineXP(X1, Y1, X2, Y2: TFixed; L: Boolean = False); overload;
    procedure LineFP(X1, Y1, X2, Y2: Single; L: Boolean = False); overload;
    procedure LineXSP(X1, Y1, X2, Y2: TFixed; L: Boolean = False); overload;
    procedure LineFSP(X1, Y1, X2, Y2: Single; L: Boolean = False); overload;

    procedure FillRect(X1, Y1, X2, Y2: Integer; Value: TColor32);
    procedure FillRectS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
    procedure FillRectT(X1, Y1, X2, Y2: Integer; Value: TColor32);
    procedure FillRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
    procedure FillRectS(const ARect: TRect; Value: TColor32); overload;
    procedure FillRectTS(const ARect: TRect; Value: TColor32); overload;

    procedure FrameRectS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
    procedure FrameRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
    procedure FrameRectTSP(X1, Y1, X2, Y2: Integer);
    procedure FrameRectS(const ARect: TRect; Value: TColor32); overload;
    procedure FrameRectTS(const ARect: TRect; Value: TColor32); overload;

    procedure RaiseRectTS(X1, Y1, X2, Y2: Integer; Contrast: Integer); overload;
    procedure RaiseRectTS(const ARect: TRect; Contrast: Integer); overload;

    procedure ResetClipRect;
    property Canvas: TCanvas read GetCanvas;
    function CanvasAllocated: Boolean;
    procedure DeleteCanvas;
    property  Pixel[X, Y: Integer]: TColor32 read GetPixel write SetPixel; default;
    property  PixelS[X, Y: Integer]: TColor32 read GetPixelS write SetPixelS;
    property BitmapHandle: HBITMAP read FHandle;
    property BitmapInfo: TBitmapInfo read FBitmapInfo;
    property Bits: PColor32Array read FBits;
    property Handle: HDC read FHDC;
    property ClipRect: TRect read FClipRect write SetClipRect;
    property Clipping: Boolean read FClipping;
    property Font: TFont read FFont write SetFont;
    property PixelPtr[X, Y: Integer]: PColor32 read GetPixelPtr;
    property ScanLine[Y: Integer]: PColor32Array read GetScanLine;
    property StippleCounter: Single read FStippleCounter write FStippleCounter;
    property StippleStep: Single read FStippleStep write FStippleStep;

    // To be published in TBitmap32
    property DrawMode: TDrawMode read FDrawMode write SetDrawMode default dmOpaque;
    property CombineMode: TCombineMode read FCombineMode write SetCombineMode default cmBlend;
    property MasterAlpha: Cardinal read FMasterAlpha write SetMasterAlpha default $FF;
    property OuterColor: TColor32 read FOuterColor write FOuterColor default 0;
    property StretchFilter: TStretchFilter read FStretchFilter write SetStretchFilter default sfNearest;
    property OnHandleChanged: TNotifyEvent read FOnHandleChanged write FOnHandleChanged;
    property OnPixelCombine: TPixelCombineEvent read FOnPixelCombine write FOnPixelCombine;
  end;


// jgb Fpc Can't initialize TBitmap before WidgetSet is initialized?
// For now add an init function that will be called in Create of lowest level class
// if StockBitmap = nil
procedure InitStockBitmap;

var // moved to interface
{$IFDEF CLX}
  StockFont: TFont;
{$ELSE}
  StockFont: HFONT;
{$ENDIF}
  StockBitmap: TBitmap;

implementation

uses Controls, Clipbrd, Math,
  GR32_LowLevel, GR32_DrawingEx;

var
  CounterLock: TRTLCriticalSection;


const
  ZERO_RECT: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

type
  TGraphicAccess = class(TGraphic);

{ TThreadPersistent }

constructor TThreadPersistent.Create;
begin
  {$IFDEF FPC}
  // jgb added InitStockBitmap. It will initialize StockBitmap once if it's
  // still nil.
  InitStockBitmap;
  {$ENDIF}
  InitializeCriticalSection(FLock);
end;

destructor TThreadPersistent.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TThreadPersistent.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TThreadPersistent.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then FOnChange(Self);
end;

procedure TThreadPersistent.EndUpdate;
begin
  Assert(FUpdateCount > 0, 'Unpaired TThreadPersistent.EndUpdate');
  Dec(FUpdateCount);
end;

procedure TThreadPersistent.Lock;
begin
  EnterCriticalSection(CounterLock);
  Inc(FLockCount);
  LeaveCriticalSection(CounterLock);
  EnterCriticalSection(FLock);
end;

procedure TThreadPersistent.Unlock;
begin
  LeaveCriticalSection(FLock);
  EnterCriticalSection(CounterLock);
  Dec(FLockCount);
  LeaveCriticalSection(CounterLock);
end;


{ TCustomMap }

procedure TCustomMap.ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer);
begin
  Width := NewWidth;
  Height := NewHeight;
end;

procedure TCustomMap.Delete;
begin
  SetSize(0, 0);
end;

function TCustomMap.Empty: Boolean;
begin
  Result := (Width = 0) or (Height = 0);
end;

procedure TCustomBitmap32.Clear;
begin
  Clear(clBlack32);
end;

procedure TCustomBitmap32.Clear(FillColor: TColor32);
begin
  if Empty then Exit;
  FillLongword(Bits[0], Width * Height, FillColor);
  Changed;
end;

procedure TCustomMap.Resized;
begin
  if Assigned(FOnResize) then FOnResize(Self);
end;

procedure TCustomMap.SetHeight(NewHeight: Integer);
begin
  SetSize(Width, NewHeight);
end;

function TCustomMap.SetSize(NewWidth, NewHeight: Integer): Boolean;
begin
  if NewWidth < 0 then NewWidth := 0;
  if NewHeight < 0 then NewHeight := 0;
  Result := (NewWidth <> FWidth) or (NewHeight <> FHeight);
  if Result then
  begin
    ChangeSize(FWidth, FHeight, NewWidth, NewHeight);
    Changed;
    Resized;
  end;
end;

function TCustomMap.SetSizeFrom(Source: TPersistent): Boolean;
begin
  if Source is TCustomMap then
    Result := SetSize(TCustomMap(Source).Width, TCustomMap(Source).Height)
  else if Source is TGraphic then
    Result := SetSize(TGraphic(Source).Width, TGraphic(Source).Height)
  else if Source is TControl then
    Result := SetSize(TControl(Source).Width, TControl(Source).Height)
  else if Source = nil then
    Result := SetSize(0, 0)
  else
    raise Exception.Create('Can''t set size from ''' + Source.ClassName + '''');
end;

procedure TCustomMap.SetWidth(NewWidth: Integer);
begin
  SetSize(NewWidth, Height);
end;

procedure TCustomBitmap32.DeleteCanvas;
begin
  if FCanvas <> nil then
  begin
{$IFDEF CLX}
    FCanvas.Handle := nil;
{$ELSE}
    FCanvas.Handle := 0;
{$ENDIF}
    FCanvas.Free;
    FCanvas := nil;
  end;
end;

procedure TCustomBitmap32.SetPixel(X, Y: Integer; Value: TColor32);
begin
  Bits[X + Y * Width] := Value;
end;

procedure TCustomBitmap32.SetPixelS(X, Y: Integer; Value: TColor32);
begin
  if (X >= FClipRect.Left) and (X < FClipRect.Right) and
     (Y >= FClipRect.Top) and (Y < FClipRect.Bottom) then
    Bits[X + Y * Width] := Value;
end;

procedure TCustomBitmap32.SetStretchFilter(Value: TStretchFilter);
begin
  if FStretchFilter <> Value then
  begin
    FStretchFilter := Value;
    Changed;
  end;
end;

function TCustomBitmap32.GetScanLine(Y: Integer): PColor32Array;
begin
  Result := @Bits[Y * Width];
end;

function TCustomBitmap32.GetCanvas: TCanvas;
begin
  if FCanvas = nil then
  begin
{$IFDEF CLX}
    FCanvas := TBitmap32Canvas.Create(Self);
{$ELSE}
    FCanvas := TCanvas.Create;
{$ENDIF}
    FCanvas.Handle := Handle;
    FCanvas.OnChange := CanvasChanged;
  end;
  Result := FCanvas;
end;

procedure TCustomBitmap32.CanvasChanged(Sender: TObject);
begin
  Changed;
end;

function TCustomBitmap32.CanvasAllocated: Boolean;
begin
  Result := FCanvas <> nil;
end;

function TCustomBitmap32.GetPixel(X, Y: Integer): TColor32;
begin
  Result := Bits[X + Y * Width];
end;

function TCustomBitmap32.GetPixelS(X, Y: Integer): TColor32;
begin
  if (X >= FClipRect.Left) and (X < FClipRect.Right) and
     (Y >= FClipRect.Top) and (Y < FClipRect.Bottom) then
    Result := Bits[X + Y * Width]
  else
    Result := OuterColor;
end;

procedure TCustomBitmap32.SetPixelT(X, Y: Integer; Value: TColor32);
begin
  BLEND_MEM[CombineMode](Value, Bits[X + Y * Width]);
  if MMX_ACTIVE then
  asm
    db $0F,$77               /// EMMS
  end;
end;

procedure TCustomBitmap32.SetPixelT(var Ptr: PColor32; Value: TColor32);
begin
  BLEND_MEM[CombineMode](Value, Ptr^);
  Inc(Ptr);
  if MMX_ACTIVE then
  asm
    db $0F,$77               /// EMMS
  end;
end;

procedure TCustomBitmap32.SetPixelTS(X, Y: Integer; Value: TColor32);
begin
  if (X >= FClipRect.Left) and (X < FClipRect.Right) and
     (Y >= FClipRect.Top) and (Y < FClipRect.Bottom) then
  begin
    BLEND_MEM[FCombineMode](Value, Bits[X + Y * Width]);
    if MMX_ACTIVE then
    asm
      db $0F,$77               /// EMMS
    end;
  end;
end;

procedure TCustomBitmap32.SET_T256(X, Y: Integer; C: TColor32);
var
  flrx, flry, celx, cely: Longword;
  P: PColor32;
  A: TColor32;
  CombineMem: TCombineMem;
begin
  { Warning: EMMS should be called after using this method }
  A := C shr 24;  // opacity

  flrx := X and $FF;
  flry := Y and $FF;

  asm
    SAR X, 8
    SAR Y, 8
  end;

  celx := A * GAMMA_TABLE[flrx xor 255];
  cely := GAMMA_TABLE[flry xor 255];
  P := @Bits[X + Y * Width];
  flrx := A * GAMMA_TABLE[flrx];
  flry := GAMMA_TABLE[flry];

  CombineMem := COMBINE_MEM[FCombineMode];
  CombineMem(C, P^, celx * cely shr 16); Inc(P);
  CombineMem(C, P^, flrx * cely shr 16); Inc(P, Width);
  CombineMem(C, P^, flrx * flry shr 16); Dec(P);
  CombineMem(C, P^, celx * flry shr 16);
end;

procedure TCustomBitmap32.SET_TS256(X, Y: Integer; C: TColor32);
var
  flrx, flry, celx, cely: Longword;
  P: PColor32;
  A: TColor32;
  CombineMem: TCombineMem;
begin
  { Warning: EMMS should be called after using this method }

  // we're checking against Left - 1 and Top - 1 due to antialiased values...
  if (X < F256ClipRect.Left - 256) or (X >= F256ClipRect.Right) or
     (Y < F256ClipRect.Top - 256) or (Y >= F256ClipRect.Bottom) then Exit;

  flrx := X and $FF;
  flry := Y and $FF;

  asm
    SAR X, 8
    SAR Y, 8
  end;

  A := C shr 24;  // opacity

  celx := A * GAMMA_TABLE[flrx xor 255];
  cely := GAMMA_TABLE[flry xor 255];
  P := @FBits[X + Y * Width];
  flrx := A * GAMMA_TABLE[flrx];
  flry := GAMMA_TABLE[flry];

  CombineMem := COMBINE_MEM[FCombineMode];

  if (X >= FClipRect.Left) and (Y >= FClipRect.Top) and
     (X < FClipRect.Right - 1) and (Y < FClipRect.Bottom - 1) then
  begin
    CombineMem(C, P^, celx * cely shr 16); Inc(P);
    CombineMem(C, P^, flrx * cely shr 16); Inc(P, Width);
    CombineMem(C, P^, flrx * flry shr 16); Dec(P);
    CombineMem(C, P^, celx * flry shr 16);
  end
  else // "pixel" lies on the edge of the bitmap
  with FClipRect do
  begin
    if (X >= Left) and (Y >= Top) then CombineMem(C, P^, celx * cely shr 16); Inc(P);
    if (X < Right - 1) and (Y >= Top) then CombineMem(C, P^, flrx * cely shr 16); Inc(P, Width);
    if (X < Right - 1) and (Y < Bottom - 1) then CombineMem(C, P^, flrx * flry shr 16); Dec(P);
    if (X >= Left) and (Y < Bottom - 1) then CombineMem(C, P^, celx * flry shr 16);
  end;
end;

function TCustomBitmap32.GetPixelPtr(X, Y: Integer): PColor32;
begin
  Result := @Bits[X + Y * Width];
end;

function TCustomBitmap32.GET_T256(X, Y: Integer): TColor32;
// When using this, remember that it interpolates towards next x and y!
var
  Pos: Integer;
begin
  Pos := (X shr 8) + (Y shr 8) * Width;
  Result := Interpolator(GAMMA_TABLE[X and $FF xor 255],
                         GAMMA_TABLE[Y and $FF xor 255],
                         @FBits[Pos], @FBits[Pos + Width]);
end;

function TCustomBitmap32.GET_TS256(X, Y: Integer): TColor32;
begin
  if (X > 0) and (Y > 0) and (X < (Width - 1) shl 8) and (Y < (Height - 1) shl 8) then
    Result := GET_T256(X,Y)
  else
    Result := FOuterColor;
end;

procedure TCustomBitmap32.ReadData(Stream: TStream);
var
  w, h: Integer;
begin
  try
    Stream.ReadBuffer(w, 4);
    Stream.ReadBuffer(h, 4);
    SetSize(w, h);
    Stream.ReadBuffer(FBits[0], Width * Height * 4);
  finally
    Changed;
  end;
end;

procedure TCustomBitmap32.WriteData(Stream: TStream);
begin
  Stream.WriteBuffer(Width, 4);
  Stream.WriteBuffer(Height, 4);
  Stream.WriteBuffer(FBits[0], Width * Height * 4);
end;

procedure TCustomBitmap32.SetStipple(NewStipple: TArrayOfColor32);
begin
  FStippleCounter := 0;
  FStipplePattern := Copy(NewStipple, 0, Length(NewStipple));
end;

procedure TCustomBitmap32.SetStipple(NewStipple: array of TColor32);
var
  L: Integer;
begin
  FStippleCounter := 0;
  L := High(NewStipple) + 1;
  SetLength(FStipplePattern, L);
  Move(NewStipple[0], FStipplePattern[0], L shl 2);
end;

procedure TCustomBitmap32.AdvanceStippleCounter(LengthPixels: Single);
var
  L: Integer;
  Delta: Single;
begin
  L := Length(FStipplePattern);
  Delta := LengthPixels * FStippleStep;
  if (L = 0) or (Delta = 0) then Exit;
  FStippleCounter := FStippleCounter + Delta;
  FStippleCounter := FStippleCounter - Floor(FStippleCounter / L) * L;
end;

function TCustomBitmap32.GetStippleColor: TColor32;
var
  L: Integer;
  NextIndex, PrevIndex: Integer;
  PrevWeight: Integer;
begin
  L := Length(FStipplePattern);
  if L = 0 then
  begin
    // no pattern defined, just return something and exit
    Result := clBlack32;
    Exit;
  end;
  while FStippleCounter >= L do FStippleCounter := FStippleCounter - L;
  while FStippleCounter < 0 do FStippleCounter := FStippleCounter + L;
  PrevIndex := Round(FStippleCounter - 0.5);
  PrevWeight := 255 - Round(255 * (FStippleCounter - PrevIndex));
  if PrevIndex < 0 then FStippleCounter := L - 1;
  NextIndex := PrevIndex + 1;
  if NextIndex >= L then NextIndex := 0;
  if PrevWeight = 255 then Result := FStipplePattern[PrevIndex]
  else
  begin
    Result := COMBINE_REG[CombineMode](
      FStipplePattern[PrevIndex],
      FStipplePattern[NextIndex],
      PrevWeight);
    EMMS;
  end;
  FStippleCounter := FStippleCounter + FStippleStep;
end;

procedure TCustomBitmap32.HorzLine(X1, Y, X2: Integer; Value: TColor32);
begin
  FillLongword(Bits[X1 + Y * Width], X2 - X1 + 1, Value);
end;

procedure TCustomBitmap32.HorzLineS(X1, Y, X2: Integer; Value: TColor32);
begin
  if (Y >= FClipRect.Top) and (Y < FClipRect.Bottom) and
     TestClip(X1, X2, FClipRect.Left, FClipRect.Right) then
    HorzLine(X1, Y, X2, Value);
end;

procedure TCustomBitmap32.HorzLineT(X1, Y, X2: Integer; Value: TColor32);
var
  i: Integer;
  P: PColor32;
  BlendMem: TBlendMem;
begin
  if X2 < X1 then Exit;
  P := PixelPtr[X1, Y];
  BlendMem := BLEND_MEM[CombineMode];
  for i := X1 to X2 do
  begin
    BlendMem(Value, P^);
    Inc(P);
  end;
  EMMS;
end;

procedure TCustomBitmap32.HorzLineTS(X1, Y, X2: Integer; Value: TColor32);
begin
  if (Y >= FClipRect.Top) and (Y < FClipRect.Bottom) and
     TestClip(X1, X2, FClipRect.Left, FClipRect.Right) then
    HorzLineT(X1, Y, X2, Value);
end;

procedure TCustomBitmap32.HorzLineTSP(X1, Y, X2: Integer);
var
  I, N: Integer;
begin
  if Empty then Exit;
  if (Y >= FClipRect.Top) and (Y < FClipRect.Bottom) then
  begin
    if ((X1 < FClipRect.Left) and (X2 < FClipRect.Left)) or
       ((X1 >= FClipRect.Right) and (X2 >= FClipRect.Right)) then
    begin
      AdvanceStippleCounter(Abs(X2 - X1) + 1);
      Exit;
    end;
    if X1 < FClipRect.Left then
    begin
      AdvanceStippleCounter(FClipRect.Left - X1);
      X1 := FClipRect.Left;
    end
    else if X1 >= FClipRect.Right then
    begin
      AdvanceStippleCounter(X1 - (FClipRect.Right - 1));
      X1 := FClipRect.Right - 1;
    end;
    N := 0;
    if X2 < FClipRect.Left then
    begin
      N := FClipRect.Left - X2;
      X2 := FClipRect.Left;
    end
    else if X2 >= FClipRect.Right then
    begin
      N := X2 - (FClipRect.Right - 1);
      X2 := FClipRect.Right - 1;
    end;

    if X2 >= X1 then
      for I := X1 to X2 do SetPixelT(I, Y, GetStippleColor)
    else
      for I := X1 downto X2 do SetPixelT(I, Y, GetStippleColor);

    if N > 0 then AdvanceStippleCounter(N);
  end
  else
    AdvanceStippleCounter(Abs(X2 - X1) + 1);
end;

procedure TCustomBitmap32.VertLine(X, Y1, Y2: Integer; Value: TColor32);
var
  I, NH, NL: Integer;
  P: PColor32;
begin
  if Y2 < Y1 then Exit;
  P := PixelPtr[X, Y1];
  I := Y2 - Y1 + 1;
  NH := I shr 2;
  NL := I and $03;
  for I := 0 to NH - 1 do
  begin
    P^ := Value; Inc(P, Width);
    P^ := Value; Inc(P, Width);
    P^ := Value; Inc(P, Width);
    P^ := Value; Inc(P, Width);
  end;
  for I := 0 to NL - 1 do
  begin
    P^ := Value; Inc(P, Width);
  end;
end;

procedure TCustomBitmap32.VertLineS(X, Y1, Y2: Integer; Value: TColor32);
begin
  if (X >= FClipRect.Left) and (X < FClipRect.Right) and
     TestClip(Y1, Y2, FClipRect.Top, FClipRect.Bottom) then
    VertLine(X, Y1, Y2, Value);
end;

procedure TCustomBitmap32.VertLineT(X, Y1, Y2: Integer; Value: TColor32);
var
  i: Integer;
  P: PColor32;
  BlendMem: TBlendMem;
begin
  P := PixelPtr[X, Y1];
  BlendMem := BLEND_MEM[FCombineMode];
  for i := Y1 to Y2 do
  begin
    BlendMem(Value, P^);
    Inc(P, Width);
  end;
  EMMS;
end;

procedure TCustomBitmap32.VertLineTS(X, Y1, Y2: Integer; Value: TColor32);
begin
  if (X >= FClipRect.Left) and (X < FClipRect.Right) and
     TestClip(Y1, Y2, FClipRect.Top, FClipRect.Bottom) then
    VertLineT(X, Y1, Y2, Value);
end;

procedure TCustomBitmap32.VertLineTSP(X, Y1, Y2: Integer);
var
  I, N: Integer;
begin
  if Empty then Exit;
  if (X >= FClipRect.Left) and (X < FClipRect.Right) then
  begin
    if ((Y1 < FClipRect.Top) and (Y2 < FClipRect.Top)) or
       ((Y1 >= FClipRect.Bottom) and (Y2 >= FClipRect.Bottom)) then
    begin
      AdvanceStippleCounter(Abs(Y2 - Y1) + 1);
      Exit;
    end;
    if Y1 < FClipRect.Top then
    begin
      AdvanceStippleCounter(FClipRect.Top - Y1);
      Y1 := FClipRect.Top;
    end
    else if Y1 >= FClipRect.Bottom then
    begin
      AdvanceStippleCounter(Y1 - (FClipRect.Bottom - 1));
      Y1 := FClipRect.Bottom - 1;
    end;
    N := 0;
    if Y2 < FClipRect.Top then
    begin
      N := FClipRect.Top - Y2;
      Y2 := FClipRect.Top;
    end
    else if Y2 >= FClipRect.Bottom then
    begin
      N := Y2 - (FClipRect.Bottom - 1);
      Y2 := FClipRect.Bottom - 1;
    end;

    if Y2 >= Y1 then
      for I := Y1 to Y2 do SetPixelT(X, I, GetStippleColor)
    else
      for I := Y1 downto Y2 do SetPixelT(X, I, GetStippleColor);

    if N > 0 then AdvanceStippleCounter(N);
  end
  else
    AdvanceStippleCounter(Abs(Y2 - Y1) + 1);
end;

procedure TCustomBitmap32.Line(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Dy, Dx, Sy, Sx, I, Delta: Integer;
  P: PColor32;
begin
  try
    Dx := X2 - X1;
    Dy := Y2 - Y1;

    if Dx > 0 then Sx := 1
    else if Dx < 0 then
    begin
      Dx := -Dx;
      Sx := -1;
    end
    else // Dx = 0
    begin
      if Dy > 0 then VertLine(X1, Y1, Y2 - 1, Value)
      else if Dy < 0 then VertLine(X1, Y2 + 1, Y1, Value);
      if L then Pixel[X2, Y2] := Value;
      Exit;
    end;

    if Dy > 0 then Sy := 1
    else if Dy < 0 then
    begin
      Dy := -Dy;
      Sy := -1;
    end
    else // Dy = 0
    begin
      if X2 > X1 then HorzLine(X1, Y1, X2 - 1, Value)
      else HorzLine(X2 + 1, Y1, X1, Value);
      if L then Pixel[X2, Y2] := Value;
      Exit;
    end;

    P := PixelPtr[X1, Y1];
    Sy := Sy * Width;

    if Dx > Dy then
    begin
      Delta := Dx shr 1;
      for I := 0 to Dx - 1 do
      begin
        P^ := Value;
        Inc(P, Sx);
        Inc(Delta, Dy);
        if Delta > Dx then
        begin
          Inc(P, Sy);
          Dec(Delta, Dx);
        end;
      end;
    end
    else // Dx < Dy
    begin
      Delta := Dy shr 1;
      for I := 0 to Dy - 1 do
      begin
        P^ := Value;
        Inc(P, Sy);
        Inc(Delta, Dx);
        if Delta > Dy then
        begin
          Inc(P, Sx);
          Dec(Delta, Dy);
        end;
      end;
    end;
    if L then P^ := Value;
  finally
    Changed;
  end;
end;

procedure TCustomBitmap32.LineS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Cx1, Cx2, Cy1, Cy2, PI, Sx, Sy, Dx, Dy, xd, yd, Dx2, Dy2, rem, term, tmp, e: Integer;
  Swapped, CheckAux: Boolean;
  P: PColor32;
begin
  Dx := X2 - X1; Dy := Y2 - Y1;

  // check for trivial cases...
  if Dx = 0 then // vertical line?
  begin
    if Dy > 0 then VertLineS(X1, Y1, Y2 - 1, Value)
    else if Dy < 0 then VertLineS(X1, Y2 + 1, Y1, Value);
    if L then PixelS[X2, Y2] := Value;
    Changed;
    Exit;
  end
  else if Dy = 0 then // horizontal line?
  begin
    if Dx > 0 then HorzLineS(X1, Y1, X2 - 1, Value)
    else if Dx < 0 then HorzLineS(X2 + 1, Y1, X1, Value);
    if L then PixelS[X2, Y2] := Value;
    Changed;
    Exit;
  end;

  Cx1 := FClipRect.Left; Cx2 := FClipRect.Right - 1;
  Cy1 := FClipRect.Top;  Cy2 := FClipRect.Bottom - 1;

  if Dx > 0 then
  begin
    If (X1 > Cx2) or (X2 < Cx1) then Exit; // segment not visible
    Sx := 1;
  end
  else
  begin
    If (X2 > Cx2) or (X1 < Cx1) then Exit; // segment not visible
    Sx := -1;
    X1 := -X1;   X2 := -X2;   Dx := -Dx;
    Cx1 := -Cx1; Cx2 := -Cx2;
    Swap(Cx1, Cx2);
  end;

  if Dy > 0 then
  begin
    If (Y1 > Cy2) or (Y2 < Cy1) then Exit; // segment not visible
    Sy := 1;
  end
  else
  begin
    If (Y2 > Cy2) or (Y1 < Cy1) then Exit; // segment not visible
    Sy := -1;
    Y1 := -Y1;   Y2 := -Y2;   Dy := -Dy;
    Cy1 := -Cy1; Cy2 := -Cy2;
    Swap(Cy1, Cy2);
  end;

  if Dx < Dy then
  begin
    Swapped := True;
    Swap(X1, Y1); Swap(X2, Y2); Swap(Dx, Dy);
    Swap(Cx1, Cy1); Swap(Cx2, Cy2); Swap(Sx, Sy);
  end
  else
    Swapped := False;

  // Bresenham's set up:
  Dx2 := Dx shl 1; Dy2 := Dy shl 1;
  xd := X1; yd := Y1; e := Dy2 - Dx; term := X2;
  CheckAux := True;

  // clipping rect horizontal entry
  if Y1 < Cy1 then
  begin
    tmp := Dx2 * (Cy1 - Y1) - Dx;
    Inc(xd, tmp div Dy2);
    rem := tmp mod Dy2;
    if xd > Cx2 then Exit;
    if xd + 1 >= Cx1 then
    begin
      yd := Cy1;
      Dec(e, rem + Dx);
      if rem > 0 then
      begin
        Inc(xd);
        Inc(e, Dy2);
      end;
      CheckAux := False; // to avoid ugly labels we set this to omit the next check
    end;
  end;

  // clipping rect vertical entry
  if CheckAux and (X1 < Cx1) then
  begin
    tmp := Dy2 * (Cx1 - X1);
    Inc(yd, tmp div Dx2);
    rem := tmp mod Dx2;
    if (yd > Cy2) or (yd = Cy2) and (rem >= Dx) then Exit;
    xd := Cx1;
    Inc(e, rem);
    if (rem >= Dx) then
    begin
      Inc(yd);
      Dec(e, Dx2);
    end;
  end;

  // set auxiliary var to indicate that temp is not clipped, since
  // temp still has the unclipped value assigned at setup.
  CheckAux := False;

  // is the segment exiting the clipping rect?
  if Y2 > Cy2 then
  begin
    tmp := Dx2 * (Cy2 - Y1) + Dx;
    term := X1 + tmp div Dy2;
    rem := tmp mod Dy2;
    if rem = 0 then Dec(term);
    CheckAux := True; // set auxiliary var to indicate that temp is clipped
  end;

  if term > Cx2 then
  begin
    term := Cx2;
    CheckAux := True; // set auxiliary var to indicate that temp is clipped
  end;

  Inc(term);

  if Sy = -1 then
    yd := -yd;

  if Sx = -1 then
  begin
    xd := -xd;
    term := -term;
  end;

  Dec(Dx2, Dy2);

  if Swapped then
  begin
    PI := Sx * Width;
    P := @Bits[yd + xd * Width];
  end
  else
  begin
    PI := Sx;
    Sy := Sy * Width;
    P := @Bits[xd + yd * Width];
  end;

  // do we need to draw the last pixel of the line and is temp not clipped?
  if not(L or CheckAux) then
  begin
    if xd < term then
      Dec(term)
    else
      Inc(term);
  end;

  while xd <> term do
  begin
    Inc(xd, Sx);

    P^ := Value;
    Inc(P, PI);
    if e >= 0 then
    begin
      Inc(P, Sy);
      Dec(e, Dx2);
    end
    else
      Inc(e, Dy2);
  end;

  Changed;
end;

procedure TCustomBitmap32.LineT(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Dy, Dx, Sy, Sx, I, Delta: Integer;
  P: PColor32;
  BlendMem: TBlendMem;
begin
  try
    Dx := X2 - X1;
    Dy := Y2 - Y1;

    if Dx > 0 then Sx := 1
    else if Dx < 0 then
    begin
      Dx := -Dx;
      Sx := -1;
    end
    else // Dx = 0
    begin
      if Dy > 0 then VertLineT(X1, Y1, Y2 - 1, Value)
      else if Dy < 0 then VertLineT(X1, Y2 + 1, Y1, Value);
      if L then SetPixelT(X2, Y2, Value);
      Exit;
    end;

    if Dy > 0 then Sy := 1
    else if Dy < 0 then
    begin
      Dy := -Dy;
      Sy := -1;
    end
    else // Dy = 0
    begin
      if X2 > X1 then HorzLineT(X1, Y1, X2 - 1, Value)
      else HorzLineT(X2 + 1, Y1, X1, Value);
      if L then SetPixelT(X2, Y2, Value);
      Exit;
    end;

    P := PixelPtr[X1, Y1];
    Sy := Sy * Width;

    try
      BlendMem := BLEND_MEM[FCombineMode];
      if Dx > Dy then
      begin
        Delta := Dx shr 1;
        for I := 0 to Dx - 1 do
        begin
          BlendMem(Value, P^);
          Inc(P, Sx);
          Inc(Delta, Dy);
          if Delta > Dx then
          begin
            Inc(P, Sy);
            Dec(Delta, Dx);
          end;
        end;
      end
      else // Dx < Dy
      begin
        Delta := Dy shr 1;
        for I := 0 to Dy - 1 do
        begin
          BlendMem(Value, P^);
          Inc(P, Sy);
          Inc(Delta, Dx);
          if Delta > Dy then
          begin
            Inc(P, Sx);
            Dec(Delta, Dy);
          end;
        end;
      end;
      if L then BlendMem(Value, P^);
    finally
      EMMS;
    end;
  finally
    Changed;
  end;
end;

procedure TCustomBitmap32.LineTS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Cx1, Cx2, Cy1, Cy2, PI, Sx, Sy, Dx, Dy, xd, yd, Dx2, Dy2, rem, term, tmp, e: Integer;
  Swapped, CheckAux: Boolean;
  P: PColor32;
  BlendMem: TBlendMem;
begin
  Dx := X2 - X1; Dy := Y2 - Y1;

  // check for trivial cases...
  if Dx = 0 then // vertical line?
  begin
    if Dy > 0 then VertLineTS(X1, Y1, Y2 - 1, Value)
    else if Dy < 0 then VertLineTS(X1, Y2 + 1, Y1, Value);
    if L then SetPixelTS(X2, Y2, Value);
    Exit;
  end
  else if Dy = 0 then // horizontal line?
  begin
    if Dx > 0 then HorzLineTS(X1, Y1, X2 - 1, Value)
    else if Dx < 0 then HorzLineTS(X2 + 1, Y1, X1, Value);
    if L then SetPixelTS(X2, Y2, Value);
    Exit;
  end;

  Cx1 := FClipRect.Left; Cx2 := FClipRect.Right - 1;
  Cy1 := FClipRect.Top;  Cy2 := FClipRect.Bottom - 1;

  if Dx > 0 then
  begin
    If (X1 > Cx2) or (X2 < Cx1) then Exit; // segment not visible
    Sx := 1;
  end
  else
  begin
    If (X2 > Cx2) or (X1 < Cx1) then Exit; // segment not visible
    Sx := -1;
    X1 := -X1;   X2 := -X2;   Dx := -Dx;
    Cx1 := -Cx1; Cx2 := -Cx2;
    Swap(Cx1, Cx2);
  end;

  if Dy > 0 then
  begin
    If (Y1 > Cy2) or (Y2 < Cy1) then Exit; // segment not visible
    Sy := 1;
  end
  else
  begin
    If (Y2 > Cy2) or (Y1 < Cy1) then Exit; // segment not visible
    Sy := -1;
    Y1 := -Y1;   Y2 := -Y2;   Dy := -Dy;
    Cy1 := -Cy1; Cy2 := -Cy2;
    Swap(Cy1, Cy2);
  end;

  if Dx < Dy then
  begin
    Swapped := True;
    Swap(X1, Y1); Swap(X2, Y2); Swap(Dx, Dy);
    Swap(Cx1, Cy1); Swap(Cx2, Cy2); Swap(Sx, Sy);
  end
  else
    Swapped := False;

  // Bresenham's set up:
  Dx2 := Dx shl 1; Dy2 := Dy shl 1;
  xd := X1; yd := Y1; e := Dy2 - Dx; term := X2;
  CheckAux := True;

  // clipping rect horizontal entry
  if Y1 < Cy1 then
  begin
    tmp := Dx2 * (Cy1 - Y1) - Dx;
    Inc(xd, tmp div Dy2);
    rem := tmp mod Dy2;
    if xd > Cx2 then Exit;
    if xd + 1 >= Cx1 then
    begin
      yd := Cy1;
      Dec(e, rem + Dx);
      if rem > 0 then
      begin
        Inc(xd);
        Inc(e, Dy2);
      end;
      CheckAux := False; // to avoid ugly labels we set this to omit the next check
    end;
  end;

  // clipping rect vertical entry
  if CheckAux and (X1 < Cx1) then
  begin
    tmp := Dy2 * (Cx1 - X1);
    Inc(yd, tmp div Dx2);
    rem := tmp mod Dx2;
    if (yd > Cy2) or (yd = Cy2) and (rem >= Dx) then Exit;
    xd := Cx1;
    Inc(e, rem);
    if (rem >= Dx) then
    begin
      Inc(yd);
      Dec(e, Dx2);
    end;
  end;

  // set auxiliary var to indicate that temp is not clipped, since
  // temp still has the unclipped value assigned at setup.
  CheckAux := False;

  // is the segment exiting the clipping rect?
  if Y2 > Cy2 then
  begin
    tmp := Dx2 * (Cy2 - Y1) + Dx;
    term := X1 + tmp div Dy2;
    rem := tmp mod Dy2;
    if rem = 0 then Dec(term);
    CheckAux := True; // set auxiliary var to indicate that temp is clipped
  end;

  if term > Cx2 then
  begin
    term := Cx2;
    CheckAux := True; // set auxiliary var to indicate that temp is clipped
  end;

  Inc(term);

  if Sy = -1 then
    yd := -yd;

  if Sx = -1 then
  begin
    xd := -xd;
    term := -term;
  end;

  Dec(Dx2, Dy2);

  if Swapped then
  begin
    PI := Sx * Width;
    P := @Bits[yd + xd * Width];
  end
  else
  begin
    PI := Sx;
    Sy := Sy * Width;
    P := @Bits[xd + yd * Width];
  end;

  // do we need to draw the last pixel of the line and is temp not clipped?
  if not(L or CheckAux) then
  begin
    if xd < term then
      Dec(term)
    else
      Inc(term);
  end;

  try
    BlendMem := BLEND_MEM[FCombineMode];
    while xd <> term do
    begin
      Inc(xd, Sx);

      BlendMem(Value, P^);
      Inc(P, PI);
      if e >= 0 then
      begin
        Inc(P, Sy);
        Dec(e, Dx2);
      end
      else
        Inc(e, Dy2);
    end;
  finally
    EMMS;
    Changed;
  end;
end;

procedure TCustomBitmap32.LineX(X1, Y1, X2, Y2: TFixed; Value: TColor32; L: Boolean);
var
  n, i: Integer;
  nx, ny, hyp: Integer;
  A: TColor32;
  h: Single;
begin
  try
    nx := X2 - X1; ny := Y2 - Y1;
    Inc(X1, 127); Inc(Y1, 127); Inc(X2, 127); Inc(Y2, 127);
    hyp := Round(Hypot(nx, ny));
    if L then Inc(hyp, 65536);
    if hyp < 256 then Exit;
    n := hyp shr 16;
    if n > 0 then
    begin
      h := 65536 / hyp;
      nx := Round(nx * h); ny := Round(ny * h);
      for i := 0 to n - 1 do
      begin
        SET_T256(X1 shr 8, Y1 shr 8, Value);
        Inc(X1, nx);
        Inc(Y1, ny);
      end;
    end;
    A := Value shr 24;
    hyp := hyp - n shl 16;
    A := A * Cardinal(hyp) shl 8 and $FF000000;
    SET_T256((X1 + X2 - nx) shr 9, (Y1 + Y2 - ny) shr 9, Value and $00FFFFFF + A);
  finally
    EMMS;
    Changed;
  end;
end;

procedure TCustomBitmap32.LineF(X1, Y1, X2, Y2: Single; Value: TColor32; L: Boolean);
begin
  LineX(Fixed(X1), Fixed(Y1), Fixed(X2), Fixed(Y2), Value, L);
end;

procedure TCustomBitmap32.LineXS(X1, Y1, X2, Y2: TFixed; Value: TColor32; L: Boolean);
var
  n, i: Integer;
  ex, ey, nx, ny, hyp: Integer;
  A: TColor32;
  h: Single;
begin
  ex := X2; ey := Y2;

  // Check for visibility and clip the coordinates
  if not ClipLine(Integer(X1), Integer(Y1), Integer(X2), Integer(Y2),
    FFixedClipRect.Left - $10000, FFixedClipRect.Top - $10000,
    FFixedClipRect.Right, FFixedClipRect.Bottom) then Exit;

  { TODO : Handle L on clipping here... }

  if (ex <> X2) or (ey <> Y2) then L := True;

  // Check if it lies entirely in the bitmap area. Even after clipping
  // some pixels may lie outside the bitmap due to antialiasing
  if (X1 > FFixedClipRect.Left) and (X1 < FFixedClipRect.Right - $20000) and
     (Y1 > FFixedClipRect.Top) and (Y1 < FFixedClipRect.Bottom - $20000) and
     (X2 > FFixedClipRect.Left) and (X2 < FFixedClipRect.Right - $20000) and
     (Y2 > FFixedClipRect.Top) and (Y2 < FFixedClipRect.Bottom - $20000) then
  begin
    LineX(X1, Y1, X2, Y2, Value);
    Exit;
  end;

  // If we are still here, it means that the line touches one or several bitmap
  // boundaries. Use the safe version of antialiased pixel routine
  try
    nx := X2 - X1; ny := Y2 - Y1;
    Inc(X1, 127); Inc(Y1, 127); Inc(X2, 127); Inc(Y2, 127);
    hyp := Round(Hypot(nx, ny));
    if L then Inc(Hyp, 65536);
    if hyp < 256 then Exit;
    n := hyp shr 16;
    if n > 0 then
    begin
      h := 65536 / hyp;
      nx := Round(nx * h); ny := Round(ny * h);
      for i := 0 to n - 1 do
      begin
        SET_TS256(SAR_8(X1), SAR_8(Y1), Value);
        X1 := X1 + nx;
        Y1 := Y1 + ny;
      end;
    end;
    A := Value shr 24;
    hyp := hyp - n shl 16;
    A := A * Longword(hyp) shl 8 and $FF000000;
    SET_TS256(SAR_9(X1 + X2 - nx), SAR_9(Y1 + Y2 - ny), Value and $00FFFFFF + A);
  finally
    EMMS;
    Changed;
  end;
end;

procedure TCustomBitmap32.LineFS(X1, Y1, X2, Y2: Single; Value: TColor32; L: Boolean);
begin
  LineXS(Fixed(X1), Fixed(Y1), Fixed(X2), Fixed(Y2), Value, L);
end;

procedure TCustomBitmap32.LineXP(X1, Y1, X2, Y2: TFixed; L: Boolean);
var
  n, i: Integer;
  nx, ny, hyp: Integer;
  A, C: TColor32;
begin
  try
    nx := X2 - X1; ny := Y2 - Y1;
    Inc(X1, 127); Inc(Y1, 127); Inc(X2, 127); Inc(Y2, 127);
    hyp := Round(Hypot(nx, ny));
    if L then Inc(hyp, 65536);
    if hyp < 256 then Exit;
    n := hyp shr 16;
    if n > 0 then
    begin
      nx := Round(nx / hyp * 65536);
      ny := Round(ny / hyp * 65536);
      for i := 0 to n - 1 do
      begin
        C := GetStippleColor;
        SET_T256(X1 shr 8, Y1 shr 8, C);
        EMMS;
        X1 := X1 + nx;
        Y1 := Y1 + ny;
      end;
    end;
    C := GetStippleColor;
    A := C shr 24;
    hyp := hyp - n shl 16;
    A := A * Longword(hyp) shl 8 and $FF000000;
    SET_T256((X1 + X2 - nx) shr 9, (Y1 + Y2 - ny) shr 9, C and $00FFFFFF + A);
    EMMS;
  finally
    Changed;
  end;
end;

procedure TCustomBitmap32.LineFP(X1, Y1, X2, Y2: Single; L: Boolean);
begin
  LineXP(Fixed(X1), Fixed(Y1), Fixed(X2), Fixed(Y2), L);
end;

procedure TCustomBitmap32.LineXSP(X1, Y1, X2, Y2: TFixed; L: Boolean);
const
  StippleInc: array [Boolean] of Single = (0, 1);
var
  n, i: Integer;
  sx, sy, ex, ey, nx, ny, hyp: Integer;
  A, C: TColor32;
begin
  sx := X1; sy := Y1; ex := X2; ey := Y2;

  // Check for visibility and clip the coordinates
  if not ClipLine(Integer(X1), Integer(Y1), Integer(X2), Integer(Y2),
    FFixedClipRect.Left - $10000, FFixedClipRect.Top - $10000,
    FFixedClipRect.Right, FFixedClipRect.Bottom) then
  begin
    AdvanceStippleCounter(Hypot((X2 - X1) / 65536, (Y2 - Y1) / 65536) - StippleInc[L]);
    Exit;
  end;

  if (ex <> X2) or (ey <> Y2) then L := True;

  // Check if it lies entirely in the bitmap area. Even after clipping
  // some pixels may lie outside the bitmap due to antialiasing
  if (X1 > FFixedClipRect.Left) and (X1 < FFixedClipRect.Right - $20000) and
     (Y1 > FFixedClipRect.Top) and (Y1 < FFixedClipRect.Bottom - $20000) and
     (X2 > FFixedClipRect.Left) and (X2 < FFixedClipRect.Right - $20000) and
     (Y2 > FFixedClipRect.Top) and (Y2 < FFixedClipRect.Bottom - $20000) then
  begin
    LineXP(X1, Y1, X2, Y2);
    Exit;
  end;

  if (sx <> X1) or (sy <> Y1) then
    AdvanceStippleCounter(Hypot((X1 - sx) / 65536, (Y1 - sy) / 65536));

  // If we are still here, it means that the line touches one or several bitmap
  // boundaries. Use the safe version of antialiased pixel routine
  try
    nx := X2 - X1; ny := Y2 - Y1;
    Inc(X1, 127); Inc(Y1, 127); Inc(X2, 127); Inc(Y2, 127);
    hyp := Round(Hypot(nx, ny));
    if L then Inc(hyp, 65536);
    if hyp < 256 then Exit;
    n := hyp shr 16;
    if n > 0 then
    begin
      nx := Round(nx / hyp * 65536); ny := Round(ny / hyp * 65536);
      for i := 0 to n - 1 do
      begin
        C := GetStippleColor;
        SET_TS256(SAR_8(X1), SAR_8(Y1), C);
        EMMS;
        X1 := X1 + nx;
        Y1 := Y1 + ny;
      end;
    end;
    C := GetStippleColor;
    A := C shr 24;
    hyp := hyp - n shl 16;
    A := A * Longword(hyp) shl 8 and $FF000000;
    SET_TS256(SAR_9(X1 + X2 - nx), SAR_9(Y1 + Y2 - ny), C and $00FFFFFF + A);
    EMMS;

  if (ex <> X2) or (ey <> Y2) then
    AdvanceStippleCounter(Hypot((X2 - ex) / 65536, (Y2 - ey) / 65536) - StippleInc[L]);

  finally
    Changed;
  end;
end;

procedure TCustomBitmap32.LineFSP(X1, Y1, X2, Y2: Single; L: Boolean);
begin
  LineXSP(Fixed(X1), Fixed(Y1), Fixed(X2), Fixed(Y2), L);
end;

procedure TCustomBitmap32.LineA(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Dx, Dy, Sx, Sy, D: Integer;
  EC, EA: Word;
  CI: Byte;
  P: PColor32;
  BlendMemEx: TBlendMemEx;
begin
  if (X1 = X2) or (Y1 = Y2) then
  begin
    LineT(X1, Y1, X2, Y2, Value, L);
    Exit;
  end;

  Dx := X2 - X1;
  Dy := Y2 - Y1;

  if Dx > 0 then Sx := 1
  else
  begin
    Sx := -1;
    Dx := -Dx;
  end;

  if Dy > 0 then Sy := 1
  else
  begin
    Sy := -1;
    Dy := -Dy;
  end;

  try
    EC := 0;
    BLEND_MEM[FCombineMode](Value, Bits[X1 + Y1 * Width]);
    BlendMemEx := BLEND_MEM_EX[FCombineMode];

    if Dy > Dx then
    begin
      EA := Dx shl 16 div Dy;
      if not L then Dec(Dy);
      while Dy > 0 do
      begin
        Dec(Dy);
        D := EC;
        Inc(EC, EA);
        if EC <= D then Inc(X1, Sx);
        Inc(Y1, Sy);
        CI := EC shr 8;
        P := @Bits[X1 + Y1 * Width];
        BlendMemEx(Value, P^, GAMMA_TABLE[CI xor 255]);
        Inc(P, Sx);
        BlendMemEx(Value, P^, GAMMA_TABLE[CI]);
      end;
    end
    else // DY <= DX
    begin
      EA := Dy shl 16 div Dx;
      if not L then Dec(Dx);
      while Dx > 0 do
      begin
        Dec(Dx);
        D := EC;
        Inc(EC, EA);
        if EC <= D then Inc(Y1, Sy);
        Inc(X1, Sx);
        CI := EC shr 8;
        P := @Bits[X1 + Y1 * Width];
        BlendMemEx(Value, P^, GAMMA_TABLE[CI xor 255]);
        if Sy = 1 then Inc(P, Width) else Dec(P, Width);
        BlendMemEx(Value, P^, GAMMA_TABLE[CI]);
      end;
    end;
  finally
    EMMS;
    Changed;
  end;
end;

procedure TCustomBitmap32.LineAS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Cx1, Cx2, Cy1, Cy2, PI, Sx, Sy, Dx, Dy, xd, yd, rem, term, tmp: Integer;
  CheckVert, CornerAA, TempClipped: Boolean;
  D1, D2: PInteger;
  EC, EA, ED, D: Word;
  CI: Byte;
  P: PColor32;
  BlendMemEx: TBlendMemEx;
begin
  If (FClipRect.Right - FClipRect.Left = 0) or
     (FClipRect.Bottom - FClipRect.Top = 0) then Exit;

  Dx := X2 - X1; Dy := Y2 - Y1;

  // check for trivial cases...
  if Abs(Dx) = Abs(Dy) then // diagonal line?
  begin
    LineTS(X1, Y1, X2, Y2, Value, L);
    Exit;
  end
  else if Dx = 0 then // vertical line?
  begin
    if Dy > 0 then VertLineTS(X1, Y1, Y2 - 1, Value)
    else if Dy < 0 then VertLineTS(X1, Y2 + 1, Y1, Value);
    if L then SetPixelTS(X2, Y2, Value);
    Exit;
  end
  else if Dy = 0 then // horizontal line?
  begin
    if Dx > 0 then HorzLineTS(X1, Y1, X2 - 1, Value)
    else if Dx < 0 then HorzLineTS(X2 + 1, Y1, X1, Value);
    if L then SetPixelTS(X2, Y2, Value);
    Exit;
  end;

  Cx1 := FClipRect.Left; Cx2 := FClipRect.Right - 1;
  Cy1 := FClipRect.Top;  Cy2 := FClipRect.Bottom - 1;

  if Dx > 0 then
  begin
    if (X1 > Cx2) or (X2 < Cx1) then Exit; // segment not visible
    Sx := 1;
  end
  else
  begin
    if (X2 > Cx2) or (X1 < Cx1) then Exit; // segment not visible
    Sx := -1;
    X1 := -X1;   X2 := -X2;   Dx := -Dx;
    Cx1 := -Cx1; Cx2 := -Cx2;
    Swap(Cx1, Cx2);
  end;

  if Dy > 0 then
  begin
    if (Y1 > Cy2) or (Y2 < Cy1) then Exit; // segment not visible
    Sy := 1;
  end
  else
  begin
    if (Y2 > Cy2) or (Y1 < Cy1) then Exit; // segment not visible
    Sy := -1;
    Y1 := -Y1;   Y2 := -Y2;   Dy := -Dy;
    Cy1 := -Cy1; Cy2 := -Cy2;
    Swap(Cy1, Cy2);
  end;

  if Dx < Dy then
  begin
    Swap(X1, Y1); Swap(X2, Y2); Swap(Dx, Dy);
    Swap(Cx1, Cy1); Swap(Cx2, Cy2); Swap(Sx, Sy);
    D1 := @yd; D2 := @xd;
    PI := Sy;
  end
  else
  begin
    D1 := @xd; D2 := @yd;
    PI := Sy * Width;
  end;

  rem := 0;
  EA := Dy shl 16 div Dx;
  EC := 0;
  xd := X1; yd := Y1;
  CheckVert := True;
  CornerAA := False;
  BlendMemEx := BLEND_MEM_EX[FCombineMode];

  // clipping rect horizontal entry
  if Y1 < Cy1 then
  begin
    tmp := (Cy1 - Y1) * 65536;
    rem := tmp - 65536; // rem := (Cy1 - Y1 - 1) * 65536;
    if tmp mod EA > 0 then
      tmp := tmp div EA + 1
    else
      tmp := tmp div EA;

    xd := Min(xd + tmp, X2 + 1);
    EC := tmp * EA;

    if rem mod EA > 0 then
      rem := rem div EA + 1
    else
      rem := rem div EA;

    tmp := tmp - rem;

    // check whether the line is partly visible
    if xd > Cx2 then
      // do we need to draw an antialiased part on the corner of the clip rect?
      If xd <= Cx2 + tmp then
        CornerAA := True
      else
        Exit;

    if (xd {+ 1} >= Cx1) or CornerAA then
    begin
      yd := Cy1;
      rem := xd; // save old xd

      ED := EC - EA;
      term := SwapConstrain(xd - tmp, Cx1, Cx2);

      If CornerAA then
      begin
        Dec(ED, (xd - Cx2 - 1) * EA);
        xd := Cx2 + 1;
      end;

      // do we need to negate the vars?
      if Sy = -1 then yd := -yd;
      if Sx = -1 then
      begin
        xd := -xd;
        term := -term;
      end;

      // draw special case horizontal line entry (draw only last half of entering segment)
      try
        while xd <> term do
        begin
          Inc(xd, -Sx);
          BlendMemEx(Value, Bits[D1^ + D2^ * Width], GAMMA_TABLE[ED shr 8]);
          Dec(ED, EA);
        end;
      finally
        EMMS;
      end;

      If CornerAA then
      begin
        // we only needed to draw the visible antialiased part of the line,
        // everything else is outside of our cliprect, so exit now since
        // there is nothing more to paint...
        { TODO : Handle Changed here... }
        Changed;
        Exit;
      end;

      if Sy = -1 then yd := -yd;  // negate back
      xd := rem;  // restore old xd
      CheckVert := False; // to avoid ugly labels we set this to omit the next check
    end;
  end;

  // clipping rect vertical entry
  if CheckVert and (X1 < Cx1) then
  begin
    tmp := (Cx1 - X1) * EA;
    Inc(yd, tmp div 65536);
    EC := tmp;
    xd := Cx1;
    if (yd > Cy2) then
      Exit
    else if (yd = Cy2) then
      CornerAA := True;
  end;

  term := X2;
  TempClipped := False;
  CheckVert := False;

  // horizontal exit?
  if Y2 > Cy2 then
  begin
    tmp := (Cy2 - Y1) * 65536;
    term := X1 + tmp div EA;
    if not(tmp mod EA > 0) then
      Dec(Term);

    if term < Cx2 then
    begin
      rem := tmp + 65536; // was: rem := (Cy2 - Y1 + 1) * 65536;
      if rem mod EA > 0 then
        rem := X1 + rem div EA + 1
      else
        rem := X1 + rem div EA;

      if rem > Cx2 then rem := Cx2;
      CheckVert := True;
    end;

    TempClipped := True;
  end;

  if term > Cx2 then
  begin
    term := Cx2;
    TempClipped := True;
  end;

  Inc(term);

  if Sy = -1 then yd := -yd;
  if Sx = -1 then
  begin
    xd := -xd;
    term := -term;
    rem := -rem;
  end;

  // draw line
  if not CornerAA then
  try
    // do we need to draw the last pixel of the line and is temp not clipped?
    if not(L or TempClipped) and not CheckVert then
    begin
      if xd < term then
        Dec(term)
      else if xd > term then
        Inc(term);
    end;

    while xd <> term do
    begin
      CI := EC shr 8;
      P := @Bits[D1^ + D2^ * Width];
      BlendMemEx(Value, P^, GAMMA_TABLE[CI xor 255]);
      Inc(P, PI);
      BlendMemEx(Value, P^, GAMMA_TABLE[CI]);
      // check for overflow and jump to next line...
      D := EC;
      Inc(EC, EA);
      if EC <= D then
        Inc(yd, Sy);

      Inc(xd, Sx);
    end;
  finally
    EMMS;
  end;

  // draw special case horizontal line exit (draw only first half of exiting segment)
  If CheckVert then
  try
    while xd <> rem do
    begin
      BlendMemEx(Value, Bits[D1^ + D2^ * Width], GAMMA_TABLE[EC shr 8 xor 255]);
      Inc(EC, EA);
      Inc(xd, Sx);
    end;
  finally
    EMMS;
  end;

  Changed;
end;

procedure TCustomBitmap32.FillRect(X1, Y1, X2, Y2: Integer; Value: TColor32);
var
  j: Integer;
  P: PColor32Array;
begin
  for j := Y1 to Y2 - 1 do
  begin
    P := Pointer(GetScanLine(j));
    FillLongword(P[X1], X2 - X1, Value);
  end;
  Changed;
end;

procedure TCustomBitmap32.FillRectS(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
  if (X2 > X1) and (Y2 > Y1) and
    (X1 < FClipRect.Right) and (Y1 < FClipRect.Bottom) and
    (X2 > FClipRect.Left) and (Y2 > FClipRect.Top) then
  begin
    if X1 < FClipRect.Left then X1 := FClipRect.Left;
    if Y1 < FClipRect.Top then Y1 := FClipRect.Top;
    if X2 > FClipRect.Right then X2 := FClipRect.Right;
    if Y2 > FClipRect.Bottom then Y2 := FClipRect.Bottom;
    FillRect(X1, Y1, X2, Y2, Value);
  end;
end;

procedure TCustomBitmap32.FillRectT(X1, Y1, X2, Y2: Integer; Value: TColor32);
var
  i, j: Integer;
  P: PColor32;
  A: Integer;
  CombineMem: TCombineMem;
begin
  A := Value shr 24;
  if A = $FF then FillRect(X1, Y1, X2, Y2, Value)
  else
  try
    Dec(Y2);
    Dec(X2);
    CombineMem := COMBINE_MEM[FCombineMode];
    for j := Y1 to Y2 do
    begin
      P := GetPixelPtr(X1, j);
      for i := X1 to X2 do
      begin
        CombineMem(Value, P^, A);
        Inc(P);
      end;
    end;
  finally
    EMMS;
    Changed;
  end;
end;

procedure TCustomBitmap32.FillRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
  if (X2 > X1) and (Y2 > Y1) and
    (X1 < FClipRect.Right) and (Y1 < FClipRect.Bottom) and
    (X2 > FClipRect.Left) and (Y2 > FClipRect.Top) then
  begin
    if X1 < FClipRect.Left then X1 := FClipRect.Left;
    if Y1 < FClipRect.Top then Y1 := FClipRect.Top;
    if X2 > FClipRect.Right then X2 := FClipRect.Right;
    if Y2 > FClipRect.Bottom then Y2 := FClipRect.Bottom;
    FillRectT(X1, Y1, X2, Y2, Value);
  end;
end;

procedure TCustomBitmap32.FillRectS(const ARect: TRect; Value: TColor32);
begin
  with ARect do FillRectS(Left, Top, Right, Bottom, Value);
end;

procedure TCustomBitmap32.FillRectTS(const ARect: TRect; Value: TColor32);
begin
  with ARect do FillRectTS(Left, Top, Right, Bottom, Value);
end;

procedure TCustomBitmap32.FrameRectS(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
  if (X2 > X1) and (Y2 > Y1) and
    (X1 < FClipRect.Right) and (Y1 < FClipRect.Bottom) and
    (X2 > FClipRect.Left) and (Y2 > FClipRect.Top) then
  begin
    Dec(Y2);
    Dec(X2);
    HorzLineS(X1, Y1, X2, Value);
    if Y2 > Y1 then HorzLineS(X1, Y2, X2, Value);
    if Y2 > Y1 + 1 then
    begin
      VertLineS(X1, Y1 + 1, Y2 - 1, Value);
      if X2 > X1 then VertLineS(X2, Y1 + 1, Y2 - 1, Value);
    end;
    Changed;
  end;
end;

procedure TCustomBitmap32.FrameRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
  if (X2 > X1) and (Y2 > Y1) and
    (X1 < FClipRect.Right) and (Y1 < FClipRect.Bottom) and
    (X2 > FClipRect.Left) and (Y2 > FClipRect.Top) then
  begin
    Dec(Y2);
    Dec(X2);
    HorzLineTS(X1, Y1, X2, Value);
    if Y2 > Y1 then HorzLineTS(X1, Y2, X2, Value);
    if Y2 > Y1 + 1 then
    begin
      VertLineTS(X1, Y1 + 1, Y2 - 1, Value);
      if X2 > X1 then VertLineTS(X2, Y1 + 1, Y2 - 1, Value);
    end;
    Changed;
  end;
end;

procedure TCustomBitmap32.FrameRectTSP(X1, Y1, X2, Y2: Integer);
begin
  if (X2 > X1) and (Y2 > Y1) and
     (X1 < Width) and (Y1 < Height) and  // don't check against ClipRect here
     (X2 > 0) and (Y2 > 0) then          // due to StippleCounter
  begin
    Dec(X2);
    Dec(Y2);
    if X1 = X2 then
      if Y1 = Y2 then SetPixelT(X1, Y1, GetStippleColor)
      else VertLineTSP(X1, Y1, Y2)
    else
      if Y1 = Y2 then HorzLineTSP(X1, Y1, X2)
      else
      begin
        HorzLineTSP(X1, Y1, X2 - 1);
        VertLineTSP(X2, Y1, Y2 - 1);
        HorzLineTSP(X2, Y2, X1 + 1);
        VertLineTSP(X1, Y2, Y1 + 1);
      end;
    Changed;
  end;
end;

procedure TCustomBitmap32.FrameRectS(const ARect: TRect; Value: TColor32);
begin
  with ARect do FrameRectS(Left, Top, Right, Bottom, Value);
end;

procedure TCustomBitmap32.FrameRectTS(const ARect: TRect; Value: TColor32);
begin
  with ARect do FrameRectTS(Left, Top, Right, Bottom, Value);
end;

procedure TCustomBitmap32.RaiseRectTS(X1, Y1, X2, Y2: Integer; Contrast: Integer);
var
  C1, C2: TColor32;
begin
  if (X2 > X1) and (Y2 > Y1) and
    (X1 < FClipRect.Right) and (Y1 < FClipRect.Bottom) and
    (X2 > FClipRect.Left) and (Y2 > FClipRect.Top) then
  try
    if Contrast > 0 then
    begin
      C1 := SetAlpha(clWhite32, Clamp(Contrast * 512 div 100));
      C2 := SetAlpha(clBlack32, Clamp(Contrast * 255 div 100));
    end
    else if Contrast < 0 then
    begin
      Contrast := -Contrast;
      C1 := SetAlpha(clBlack32, Clamp(Contrast * 255 div 100));
      C2 := SetAlpha(clWhite32, Clamp(Contrast * 512 div 100));
    end
    else Exit;

    Dec(X2);
    Dec(Y2);
    HorzLineTS(X1, Y1, X2, C1);
    HorzLineTS(X1, Y2, X2, C2);
    Inc(Y1);
    Dec(Y2);
    VertLineTS(X1, Y1, Y2, C1);
    VertLineTS(X2, Y1, Y2, C2);
  finally
    Changed;
  end;
end;

procedure TCustomBitmap32.RaiseRectTS(const ARect: TRect; Contrast: Integer);
begin
  with ARect do RaiseRectTS(Left, Top, Right, Bottom, Contrast);
end;

procedure TCustomBitmap32.ResetAlpha;
begin
  ResetAlpha($FF);
end;

procedure TCustomBitmap32.ResetAlpha(const AlphaValue: Byte);
var
  I: Integer;
  P: PByte;
  NH, NL: Integer;
begin
  P := Pointer(FBits);
  Inc(P, 3); // shift the pointer to 'alpha' component of the first pixel

  { Enroll the loop 4 times }
  I := Width * Height;
  NH := I shr 2;
  NL := I and $3;
  for I := 0 to NH - 1 do
  begin
    P^ := AlphaValue; Inc(P, 4);
    P^ := AlphaValue; Inc(P, 4);
    P^ := AlphaValue; Inc(P, 4);
    P^ := AlphaValue; Inc(P, 4);
  end;
  for I := 0 to NL - 1 do
  begin
    P^ := AlphaValue; Inc(P, 4);
  end;
  Changed;
end;

procedure TCustomBitmap32.SetCombineMode(const Value: TCombineMode);
begin
  FCombineMode := Value;
  Changed;
end;

procedure TCustomBitmap32.SetDrawMode(Value: TDrawMode);
begin
  if FDrawMode <> Value then
  begin
    FDrawMode := Value;
    Changed;
  end;
end;

procedure TCustomBitmap32.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  FontChanged(Self);
end;

procedure TCustomBitmap32.FontChanged(Sender: TObject);
begin
{$IFDEF CLX}
  if Assigned(FontHandle) then FontHandle := nil;
{$ELSE}
  if FontHandle <> 0 then
  begin
    if Handle <> 0 then SelectObject(Handle, StockFont);
    FontHandle := 0;
  end;
{$ENDIF}
end;

procedure TCustomBitmap32.SetMasterAlpha(Value: Cardinal);
begin
  if FMasterAlpha <> Value then
  begin
    FMasterAlpha := Value;
    Changed;
  end;
end;

procedure TCustomBitmap32.HandleChanged;
begin
  if FCanvas <> nil then FCanvas.Handle := Self.Handle;
  if Assigned(FOnHandleChanged) then FOnHandleChanged(Self);
end;

procedure TCustomBitmap32.ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer);
begin
  try
     FontChanged(Self);
    DeleteCanvas; // Patch by Thomas Bauer.....

{$IFDEF CLX}
    if Assigned(FHDC) then QPainter_destroy(FHDC);
    FHDC := nil;
    if Assigned(FHandle) then QImage_destroy(FHandle);
    FHandle := nil;
{$ELSE}
    if FHDC <> 0 then DeleteDC(FHDC);
    FHDC := 0;
    if FHandle <> 0 then DeleteObject(FHandle);
    FHandle := 0;
{$ENDIF}

    FBits := nil;
    Width := 0;
    Height := 0;
    if (NewWidth > 0) and (NewHeight > 0) then
    begin
{$IFDEF CLX}
      FHandle := QImage_create(NewWidth, NewHeight, 32, 1, QImageEndian_IgnoreEndian);
      if FHandle <> nil then
      begin
        FBits := Pointer(QImage_bits(FHandle));
        // clear it since QT doesn't initialize the image data:
        FillLongword(FBits[0], NewWidth * NewHeight, clBlack32);
      end;
{$ELSE}
      with FBitmapInfo.bmiHeader do
      begin
        biWidth := NewWidth;
        biHeight := -NewHeight;
      end;
      FHandle := CreateDIBSection(0, FBitmapInfo, DIB_RGB_COLORS, Pointer(FBits), 0, 0);
{$ENDIF}

      // 9-1-2012 jgb: As the following error in most cases means there isn't
      // enough free memory I changed the message to make that clearer
      //if FBits = nil then raise Exception.Create('Can''t allocate the DIB handle');
      if FBits = nil then
        raise ECantAllocateDIBHandleException.Create('Can''t allocate the DIB handle: in most cases this means that you don''t have enough free memory to load the image');

{$IFDEF CLX}
      FHDC := QPainter_create;
      if FHDC = nil then
      begin
        QImage_destroy(FHandle);
        FBits := nil;
        raise ECantCreateCompatibleDCException.Create('Can''t create compatible DC');
      end;

      FPixmap := QPixmap_create;
      FPixmapActive := False;
{$ELSE}
      FHDC := CreateCompatibleDC(0);
      if FHDC = 0 then
      begin
        DeleteObject(FHandle);
        FHandle := 0;
        FBits := nil;
        raise ECantCreateCompatibleDCException.Create('Can''t create compatible DC');
      end;

      if SelectObject(FHDC, FHandle) = 0 then
      begin
        DeleteDC(FHDC);
        DeleteObject(FHandle);
        FHDC := 0;
        FHandle := 0;
        FBits := nil;
        raise ECantSelectObjectIntoDCException.Create('Can''t select an object into DC');
      end;
{$ENDIF}
    end;

    Width := NewWidth;
    Height := NewHeight;

    ResetClipRect;
  finally
    HandleChanged;
  end;
end;

function TCustomBitmap32.Empty: Boolean;
begin
{$IFDEF CLX}
  Result := not(Assigned(FHandle) or Assigned(FPixmap)) or inherited Empty;
{$ELSE}
  Result := (FHandle = 0) or inherited Empty;
{$ENDIF}
end;

constructor TCustomBitmap32.Create;
begin
  inherited;
  FillChar(FBitmapInfo, SizeOf(TBitmapInfo), 0);
  with FBitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
  end;
  FOuterColor := $00000000;  // by default as full transparency black
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  {$IFDEF BORLAND}
  FFont.OwnerCriticalSection := @FLock;
  {$ENDIF}
  FMasterAlpha := $FF;
  FStippleStep := 1;
  CombineMode := cmBlend;
end;

destructor TCustomBitmap32.Destroy;
begin
  BeginUpdate;
  Lock;
  try
    DeleteCanvas;
    SetSize(0, 0);
    FFont.Free;
  finally
    Unlock;
  end;
  inherited;
end;

procedure TCustomBitmap32.Assign(Source: TPersistent);
var
  Canvas: TCanvas;
  Picture: TPicture;
  TempBitmap: TCustomBitmap32;
  I: integer;
  DstP, SrcP: PColor32;
  DstColor: TColor32;

  procedure AssignFromBitmap(SrcBmp: TBitmap);
  var
    TransparentColor: TColor32;
    I: integer;
  begin
    SetSize(SrcBmp.Width, SrcBmp.Height);
    if Empty then Exit;
{$IFDEF CLX}
    if not QPainter_isActive(Handle) then
      if not QPainter_begin(Handle, Pixmap) then
        raise EInvalidGraphicOperation.CreateRes(@SInvalidCanvasState);
    QPainter_drawPixmap(Handle, 0, 0, SrcBmp.Handle, 0, 0, Width, Height);
    QPainter_end(Handle);
    PixmapChanged := True;
{$ELSE}
    BitBlt(Handle, 0, 0, Width, Height, SrcBmp.Canvas.Handle, 0, 0, SRCCOPY);
{$ENDIF}
    if SrcBmp.PixelFormat <> pf32bit then ResetAlpha;
    if SrcBmp.Transparent then
    begin
      TransparentColor := Color32(SrcBmp.TransparentColor) and $00FFFFFF;
      DstP := @Bits[0];
      for I := 0 to Width * Height - 1 do
      begin
        DstColor := DstP^ and $00FFFFFF;
        if DstColor = TransparentColor then
          DstP^ := DstColor;
        inc(DstP);
      end;
    end;
  end;

begin
  BeginUpdate;
  try
    if Source = nil then
    begin
      SetSize(0, 0);
      Exit;
    end
    else if Source is TCustomBitmap32 then
    begin
      SetSize(TCustomBitmap32(Source).Width, TCustomBitmap32(Source).Height);
      if Empty then Exit;
{$IFDEF CLX}
      Move(TBitmap32(Source).Bits[0], Bits[0], Width * Height * 4);
{$ELSE}
      BitBlt(Handle, 0, 0, Width, Height, TCustomBitmap32(Source).Handle, 0, 0, SRCCOPY);
      //Move(TBitmap32(Source).Bits[0], Bits[0], Width * Height * 4);
      // Move is up to 2x faster with FastMove by the FastCode Project
{$ENDIF}
      FDrawMode := TCustomBitmap32(Source).FDrawMode;
      FMasterAlpha := TCustomBitmap32(Source).FMasterAlpha;
      FOuterColor := TCustomBitmap32(Source).FOuterColor;
      FStretchFilter := TCustomBitmap32(Source).FStretchFilter;
      Exit;
    end
    else if Source is TBitmap then
    begin
      AssignFromBitmap(TBitmap(Source));
      Exit;
    end
    else if Source is TGraphic then
    begin
      SetSize(TGraphic(Source).Width, TGraphic(Source).Height);
      if Empty then Exit;
      Canvas := TCanvas.Create;
      try
        Canvas.Handle := Self.Handle;
        TGraphicAccess(Source).Draw(Canvas, MakeRect(0, 0, Width, Height));
        ResetAlpha;
      finally
        Canvas.Free;
      end;
    end
    else if Source is TPicture then
    begin
      //with TPicture(Source) do
      begin
        if TPicture(Source).Graphic is TBitmap then
          AssignFromBitmap(TBitmap(TPicture(Source).Graphic))
        else if (TPicture(Source).Graphic is TIcon) {$IFDEF BORLAND}{$IFNDEF CLX}or
                (TPicture(Source).Graphic is TMetaFile) {$ENDIF}{$ENDIF} then
        begin
          // icons, metafiles etc...
          SetSize(TPicture(Source).Graphic.Width, TPicture(Source).Graphic.Height);
          if Empty then Exit;

          TempBitmap := TCustomBitmap32.Create;
          Canvas := TCanvas.Create;
          try
            Self.Clear(clWhite32);  // mask on white;
            Canvas.Handle := Self.Handle;
            TGraphicAccess(TPicture(Source).Graphic).Draw(Canvas, MakeRect(0, 0, Width, Height));

            TempBitmap.SetSize(TPicture(Source).Graphic.Width, TPicture(Source).Graphic.Height);
            TempBitmap.Clear(clRed32); // mask on red;
            Canvas.Handle := TempBitmap.Handle;
            TGraphicAccess(TPicture(Source).Graphic).Draw(Canvas, MakeRect(0, 0, Width, Height));

            DstP := @Bits[0];
            SrcP := @TempBitmap.Bits[0];
            for I := 0 to Width * Height - 1 do
            begin
              DstColor := DstP^ and $00FFFFFF;
              // this checks for transparency by comparing the pixel-color of the
              // temporary bitmap (red masked) with the pixel of our
              // bitmap (white masked). If they match, make that pixel opaque
              if DstColor = (SrcP^ and $00FFFFFF) then
                DstP^ := DstColor or $FF000000
              else
              // if the colors don't match (that is the case if there is a
              // match "is clRed32 = clBlue32 ?"), just make that pixel
              // transparent:
                DstP^ := DstColor;

               inc(SrcP); inc(DstP);
            end;
          finally
            TempBitmap.Free;
            Canvas.Free;
          end;
        end
        else
        begin
          // anything else...
          SetSize(TPicture(Source).Graphic.Width, TPicture(Source).Graphic.Height);
          if Empty then Exit;
          Canvas := TCanvas.Create;
          try
            Canvas.Handle := Self.Handle;
            TGraphicAccess(TPicture(Source).Graphic).Draw(Canvas, MakeRect(0, 0, Width, Height));
            ResetAlpha;
          finally
            Canvas.Free;
          end;
        end;
      end;
      Exit;
    end
    else if Source is TClipboard then
    begin
      Picture := TPicture.Create;
      try
        Picture.Assign(TClipboard(Source));
        SetSize(Picture.Width, Picture.Height);
        if Empty then Exit;
        Canvas := TCanvas.Create;
        try
          Canvas.Handle := Self.Handle;
          TGraphicAccess(Picture.Graphic).Draw(Canvas, MakeRect(0, 0, Width, Height));
          ResetAlpha;
        finally
          Canvas.Free;
        end;
      finally
        Picture.Free;
      end;
      Exit;
    end
    else
      inherited; // default handler
  finally;
    EndUpdate;
    Changed;
  end;
end;

function TCustomBitmap32.BoundsRect: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Width;
  Result.Bottom := Height;
end;

procedure TCustomBitmap32.UpdateClipRects;
begin
  // calculate clip rects in other units, so we can speed things up a bit.
  FFixedClipRect := FixedRect(FClipRect);

  F256ClipRect.Left := FClipRect.Left shl 8;
  F256ClipRect.Top := FClipRect.Top shl 8;
  F256ClipRect.Right := FClipRect.Right shl 8;
  F256ClipRect.Bottom := FClipRect.Bottom shl 8;

  FClipping := not EqualRect(FClipRect, BoundsRect);
end;

procedure TCustomBitmap32.ResetClipRect;
begin
  FClipRect.Left := 0;
  FClipRect.Top := 0;
  FClipRect.Right := Width;
  FClipRect.Bottom := Height;

  UpdateClipRects;
end;

procedure TCustomBitmap32.SetClipRect(const Value: TRect);
begin
  FClipRect.Right := Constrain(Value.Right, 0, Width);
  FClipRect.Bottom := Constrain(Value.Bottom, 0, Height);
  FClipRect.Left := Constrain(Value.Left, 0, FClipRect.Right);
  FClipRect.Top := Constrain(Value.Top, 0, FClipRect.Bottom);

  UpdateClipRects;
end;


{ Rectangles }

function MakeRect(const L, T, R, B: Integer): TRect;
begin
  Result.Left := L;
  Result.Top := T;
  Result.Right := R;
  Result.Bottom := B;
end;

function MakeRect(const FR: TFloatRect; Rounding: TRectRounding): TRect;
begin
  case Rounding of
    rrClosest:
      begin
        Result.Left := Round(FR.Left);
        Result.Top := Round(FR.Top);
        Result.Right := Round(FR.Right);
        Result.Bottom := Round(FR.Bottom);
      end;

    rrInside:
      begin
        Result.Left := Ceil(FR.Left);
        Result.Top := Ceil(FR.Top);
        Result.Right := Ceil(FR.Right);
        Result.Bottom := Ceil(FR.Bottom);
        if Result.Right < Result.Left then Result.Right := Result.Left;
        if Result.Bottom < Result.Top then Result.Bottom := Result.Top;
      end;

    rrOutside:
      begin
        Result.Left := Floor(FR.Left);
        Result.Top := Floor(FR.Top);
        Result.Right := Ceil(FR.Right);
        Result.Bottom := Ceil(FR.Bottom);
      end;
  end;
end;

function MakeRect(const FXR: TFixedRect; Rounding: TRectRounding): TRect;
begin
  case Rounding of
    rrClosest:
      begin
        Result.Left := FixedRound(FXR.Left);
        Result.Top := FixedRound(FXR.Top);
        Result.Right := FixedRound(FXR.Right);
        Result.Bottom := FixedRound(FXR.Bottom);
      end;

    rrInside:
      begin
        Result.Left := FixedCeil(FXR.Left);
        Result.Top := FixedCeil(FXR.Top);
        Result.Right := FixedFloor(FXR.Right);
        Result.Bottom := FixedFloor(FXR.Bottom);
        if Result.Right < Result.Left then Result.Right := Result.Left;
        if Result.Bottom < Result.Top then Result.Bottom := Result.Top;
      end;

    rrOutside:
      begin
        Result.Left := FixedFloor(FXR.Left);
        Result.Top := FixedFloor(FXR.Top);
        Result.Right := FixedCeil(FXR.Right);
        Result.Bottom := FixedCeil(FXR.Bottom);
      end;
  end;
end;

function FixedRect(const L, T, R, B: TFixed): TFixedRect;
begin
  Result.Left := L;
  Result.Top := T;
  Result.Right := R;
  Result.Bottom := B;
end;

function FixedRect(const ARect: TRect): TFixedRect;
begin
  Result.Left := ARect.Left shl 16;
  Result.Top := ARect.Top shl 16;
  Result.Right := ARect.Right shl 16;
  Result.Bottom := ARect.Bottom shl 16;
end;

function FixedRect(const FR: TFloatRect): TFixedRect;
begin
  Result.Left := Round(FR.Left * 65536);
  Result.Top := Round(FR.Top * 65536);
  Result.Right := Round(FR.Right * 65536);
  Result.Bottom := Round(FR.Bottom * 65536);
end;

function FloatRect(const L, T, R, B: Single): TFloatRect;
begin
  Result.Left := L;
  Result.Top := T;
  Result.Right := R;
  Result.Bottom := B;
end;

function FloatRect(const ARect: TRect): TFloatRect;
begin
  Result.Left := ARect.Left;
  Result.Top := ARect.Top;
  Result.Right := ARect.Right;
  Result.Bottom := ARect.Bottom;
end;

function FloatRect(const FXR: TFixedRect): TFloatRect;
begin
  Result.Left := FXR.Left * FixedToFloat;
  Result.Top := FXR.Top * FixedToFloat;
  Result.Right := FXR.Right * FixedToFloat;
  Result.Bottom := FXR.Bottom * FixedToFloat;
end;

function IntersectRect(out Dst: TRect; const R1, R2: TRect): Boolean;
begin
  if R1.Left >= R2.Left then Dst.Left := R1.Left else Dst.Left := R2.Left;
  if R1.Right <= R2.Right then Dst.Right := R1.Right else Dst.Right := R2.Right;
  if R1.Top >= R2.Top then Dst.Top := R1.Top else Dst.Top := R2.Top;
  if R1.Bottom <= R2.Bottom then Dst.Bottom := R1.Bottom else Dst.Bottom := R2.Bottom;
  Result := (Dst.Right >= Dst.Left) and (Dst.Bottom >= Dst.Top);
  if not Result then Dst := ZERO_RECT;
end;

function IntersectRectF(out Dst: TFloatRect; const FR1, FR2: TFloatRect): Boolean;
begin
  Dst.Left   := Max(FR1.Left,   FR2.Left);
  Dst.Right  := Min(FR1.Right,  FR2.Right);
  Dst.Top    := Max(FR1.Top,    FR2.Top);
  Dst.Bottom := Min(FR1.Bottom, FR2.Bottom);
  Result := (Dst.Right >= Dst.Left) and (Dst.Bottom >= Dst.Top);
  if not Result then FillLongword(Dst, 4, 0);
end;

function EqualRect(const R1, R2: TRect): Boolean;
begin
  Result := CompareMem(@R1, @R2, SizeOf(TRect));
end;

procedure InflateRect(var R: TRect; Dx, Dy: Integer);
begin
  Dec(R.Left, Dx); Dec(R.Top, Dy);
  Inc(R.Right, Dx); Inc(R.Bottom, Dy);
end;

procedure InflateRectF(var FR: TFloatRect; Dx, Dy: Single);
begin
  with FR do
  begin
    Left := Left - Dx; Top := Top - Dy;
    Right := Right + Dx; Bottom := Bottom + Dy;
  end;
end;

procedure OffsetRect(var R: TRect; Dx, Dy: Integer);
begin
  Inc(R.Left, Dx); Inc(R.Top, Dy);
  Inc(R.Right, Dx); Inc(R.Bottom, Dy);
end;

procedure OffsetRectF(var FR: TFloatRect; Dx, Dy: Single);
begin
  with FR do
  begin
    Left := Left + Dx; Top := Top + Dy;
    Right := Right + Dx; Bottom := Bottom + Dy;
  end;
end;

function IsRectEmpty(const R: TRect): Boolean;
begin
  Result := (R.Right <= R.Left) or (R.Bottom <= R.Top);
end;

function IsRectEmptyF(const FR: TFloatRect): Boolean;
begin
  Result := (FR.Right <= FR.Left) or (FR.Bottom <= FR.Top);
end;

function PtInRect(const R: TRect; const P: TPoint): Boolean;
begin
  Result := (P.X >= R.Left) and (P.X < R.Right) and
    (P.Y >= R.Top) and (P.Y < R.Bottom);
end;

{ Fixed-point math }

function Fixed(S: Single): TFixed;
begin
  Result := Round(S * 65536);
end;

function Fixed(I: Integer): TFixed;
begin
  Result := I * $10000{I shl 16};
end;

function FixedFloor(A: TFixed): Integer;
asm
        SAR     EAX, 16;
end;

function FixedCeil(A: TFixed): Integer;
asm
        ADD     EAX, $0000FFFF
        SAR     EAX, 16;
end;

function FixedRound(A: TFixed): Integer;
asm
        ADD     EAX, $00007FFF
        SAR     EAX, 16
end;

function FixedMul(A, B: TFixed): TFixed;
asm
        IMUL    EDX
        SHRD    EAX, EDX, 16
end;

function FixedDiv(A, B: TFixed): TFixed;
asm
        MOV     ECX, B
        CDQ
        SHLD    EDX, EAX, 16
        SHL     EAX, 16
        IDIV    ECX
end;

// jgb added for fpc 2015-05-03
procedure InitStockBitmap;
begin
  if StockBitmap = nil then begin
    StockBitmap := TBitmap.Create;
    StockBitmap.Width := 8;
    StockBitmap.Height := 8;
  end;
end;

initialization
  InitializeCriticalSection(CounterLock);
finalization
  DeleteCriticalSection(CounterLock);
end.

