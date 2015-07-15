unit MainUnit;

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
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Michael Hansen <dyster_tid@hotmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GR32, GR32_Image, GR32_Transforms, GR32_Resamplers, GR32_Layers, ExtCtrls, StdCtrls, Buttons,
  ComCtrls, Grids, GR32_RangeBars, ExtDlgs, GR32_TransformsWarp,GR32_TransformsEx,
  GR32_Rasterizers,GR32_RasterizerEx;

type
  TOpType = (opNone, opTranslate, opScale, opRotate, opSkew);
  TOpRec = record
    OpType: TOpType;
    Dx, Dy: Extended;        // shifts for opTranslate mode
    Sx, Sy: Extended;        // scale factors for opScale mode
    Cx, Cy, Alpha: Extended; // rotation center and angle (deg) for opRotate mode
    Fx, Fy: Extended;        // skew factors for opSkew mode
  end;
  TOpRecs = array[0..7] of TOpRec;

const
  OpTypes: array [0..4] of TOpType = (opNone, opTranslate, opScale, opRotate,
    opSkew);

type
  TTransformMode = (tmAffine, tmProjective, tmBilinear, tmFishEye, tmDisturbance, tmTwirl, tmBloat,tmWarp,tmSphere);

  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Panel2: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    StringGrid: TStringGrid;
    ListBox: TListBox;
    Button1: TButton;
    Label9: TLabel;
    CodeString: TEdit;
    Panel1: TPanel;
    Label1: TLabel;
    ComboBox: TComboBox;
    Notebook: TNotebook;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    eDx: TEdit;
    eDy: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    eSy: TEdit;
    eSx: TEdit;
    Label11: TLabel;
    Label13: TLabel;
    Label16: TLabel;
    Label15: TLabel;
    eCx: TEdit;
    eAlpha: TEdit;
    eCy: TEdit;
    Label12: TLabel;
    Label14: TLabel;
    Label17: TLabel;
    eFx: TEdit;
    eFy: TEdit;
    Label10: TLabel;
    Panel3: TPanel;
    TabSheet2: TTabSheet;
    Label18: TLabel;
    OpacityBar: TGaugeBar;
    sbDx: TGaugeBar;
    sbDy: TGaugeBar;
    sbSx: TGaugeBar;
    sbSy: TGaugeBar;
    sbAlpha: TGaugeBar;
    sbFx: TGaugeBar;
    sbFy: TGaugeBar;
    ResamplerLabel: TLabel;
    ResamplerClassNamesList: TComboBox;
    KernelLabel: TLabel;
    KernelClassNamesList: TComboBox;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    Label19: TLabel;
    EditDisturbance: TEdit;
    Label20: TLabel;
    EditTwirl: TEdit;
    EditBloat: TEdit;
    Label21: TLabel;
    ButtonRepeatTransform: TButton;
    ButtonBloatDown: TButton;
    ButtonBloatUp: TButton;
    ButtonTwirlDown: TButton;
    ButtonTwirlUp: TButton;
    ButtonDisturbanceUp: TButton;
    ButtonDisturbanceDown: TButton;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    ButtonLoadImage: TButton;
    LabelTime: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel4: TPanel;
    Src: TImage32;
    Dst: TImage32;
    Splitter1: TSplitter;
    TabWarps: TTabControl;
    PanelParam1: TPanel;
    UpDown2: TUpDown;
    UpDown1: TUpDown;
    EditParam1: TEdit;
    Label22: TLabel;
    PanelParam2: TPanel;
    Label23: TLabel;
    UpDown3: TUpDown;
    UpDown4: TUpDown;
    EditParam2: TEdit;
    PanelParam3: TPanel;
    Label24: TLabel;
    UpDown5: TUpDown;
    UpDown6: TUpDown;
    EditParam3: TEdit;
    UpDown7: TUpDown;
    UpDown8: TUpDown;
    Edit1: TEdit;
    Label25: TLabel;
    Edit2: TEdit;
    UpDown9: TUpDown;
    UpDown10: TUpDown;
    Label26: TLabel;
    Edit3: TEdit;
    UpDown11: TUpDown;
    UpDown12: TUpDown;
    Label27: TLabel;
    Edit4: TEdit;
    UpDown13: TUpDown;
    UpDown14: TUpDown;
    Label28: TLabel;
    Edit5: TEdit;
    UpDown15: TUpDown;
    UpDown16: TUpDown;
    Label29: TLabel;
    LabelConvex: TLabel;
    Button2: TButton;
    Label30: TLabel;
    Label31: TLabel;
    CheckBox1: TCheckBox;
    Label32: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure TranslationChanged(Sender: TObject);
    procedure ScaleChanged(Sender: TObject);
    procedure TranslationScrolled(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ScaleScrolled(Sender: TObject);
    procedure RotationChanged(Sender: TObject);
    procedure RotationScrolled(Sender: TObject);
    procedure SkewChanged(Sender: TObject);
    procedure SkewScrolled(Sender: TObject);
    procedure OpacityChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);

    procedure SrcRBResizingEvent(Sender: TObject; const OldLocation: TFloatRect;
      var NewLocation: TFloatRect; DragState: TDragState; Shift: TShiftState);

    procedure RubberLayerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure RubberLayerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure RubberLayerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure AppEventsIdle(Sender: TObject; var Done: Boolean);
    procedure ResamplerClassNamesListClick(Sender: TObject);
    procedure ResamplerClassNamesListChange(Sender: TObject);
    procedure KernelClassNamesListChange(Sender: TObject);
    procedure DstPaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure EditDisturbanceChange(Sender: TObject);
    procedure EditTwirlChange(Sender: TObject);
    procedure EditBloatChange(Sender: TObject);
    procedure ButtonRepeatTransformClick(Sender: TObject);
    procedure ButtonBloatDownClick(Sender: TObject);
    procedure ButtonBloatUpClick(Sender: TObject);
    procedure ButtonTwirlDownClick(Sender: TObject);
    procedure ButtonTwirlUpClick(Sender: TObject);
    procedure ButtonDisturbanceDownClick(Sender: TObject);
    procedure ButtonDisturbanceUpClick(Sender: TObject);
    procedure ButtonLoadImageClick(Sender: TObject);
    procedure DstResize(Sender: TObject);
    procedure TabWarpsChange(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown2Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown4Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown3Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown6Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown5Click(Sender: TObject; Button: TUDBtnType);
    procedure EditParam1Change(Sender: TObject);
    procedure EditParam2Change(Sender: TObject);
    procedure EditParam3Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure UpDown8Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown10Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown11Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown13Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown14Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown15Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown16Click(Sender: TObject; Button: TUDBtnType);
    procedure Button2Click(Sender: TObject);
    procedure SrcMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
  protected
    LoadingValues: Boolean;
    DraggedVertex: Integer;
    LastMousePos: TPoint;
    isMouseDown: boolean;
    StippleStart: Single;
    procedure PaintHandles(Sender: TObject; BackBuffer: TBitmap32);
    procedure ChangeParam(i:integer;value:single);
  public
    Rasterizer:TRasterizer;
    RasteriserRegular:TRegularRasterizer;
    RasterizerBoxed:TBoxRasterizer;
    SrcRubberBandLayer: TRubberBandLayer;
    Operation: TOpRecs;
    Current: ^TOpRec;
    AT: TAffineTransformation;
    PT: TProjectiveTransformation;
    TT: TTransformation;
    TWT: TTwirlTransformation;
    BT: TBloatTransformation;
    DT: TDisturbanceTransformation;
    FET: TFishEyeTransformation;
    ST:TSphereTransformation;
    Vertices: array[0..3] of TPoint;
    Mode: TTransformMode;
    procedure ClearTransformations;
    procedure DoTransform;
    procedure GenTransform;
    procedure PrepareSource;
    procedure ShowSettings(OperationNum: Integer);
    procedure InitVertices; // for projective mapping
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  JPEG,
  GR32_MathAddons;


function GetVal(Src: string; var Dst: Extended): Boolean;
var
  Code: Integer;
begin
  Val(Src, Dst, Code);
  Result := Code = 0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Src.Bitmap.LoadFromFile('image.jpg');
  isMouseDown := false;

  //Setup custom paintstages ("checkerboard" and border)
  with Dst do
  begin
    with PaintStages[0]^ do //Set up custom paintstage to draw checkerboard
    begin
      Stage := PST_CUSTOM;
      Parameter := 1; // use parameter to tag the stage, we inspect this in OnPaintStage
    end;
    with PaintStages.Add^ do  //Insert new paintstage on top of everything else, we use this to draw border
    begin
      Stage := PST_CUSTOM;
      Parameter := 2;
    end;
  end;

  with Src do
  begin
    with PaintStages[0]^ do
    begin
      Stage := PST_CUSTOM;
      Parameter := 1;
    end;
    with PaintStages.Add^ do
    begin
      Stage := PST_CUSTOM;
      Parameter := 2;
    end;
  end;

  ResamplerList.GetClassNames(ResamplerClassNamesList.Items);
  KernelList.GetClassNames(KernelClassNamesList.Items);
  ResamplerClassNamesList.ItemIndex := 0;
  KernelClassNamesList.ItemIndex := 0;

  SrcRubberBandLayer := TRubberBandLayer.Create(Src.Layers);
  SrcRubberBandLayer.OnResizing := SrcRBResizingEvent;
  SrcRubberBandLayer.Location := FloatRect(0, 0, Src.Bitmap.Width - 1, Src.Bitmap.Height - 1);
  with TCustomLayer.Create(Dst.Layers) do
  begin
    OnPaint := PaintHandles;
  end;

  RasteriserRegular := TRegularRasterizer.create;
  RasterizerBoxed := TBoxRasterizer.create;
  Rasterizer := RasteriserRegular;

  DraggedVertex := -1;
  Dst.SetupBitmap; // set the destination bitmap size to match the image size
  PrepareSource;
  ClearTransformations;
  ShowSettings(0);
  AT := TAffineTransformation.Create;
  PT := TProjectiveTransformation.Create;
  TT := AT;
  TWT := TTwirlTransformation.Create;
  BT := TBloatTransformation.Create;
  DT := TDisturbanceTransformation.Create;
  FET := TFishEyeTransformation.Create;
  ST := TSphereTransformation.Create;
  ST.Ray := 100;
  ST.CenterX := 100;
  ST.CenterY := 100;
  PageControl1.ActivePage := TabSheet1;
  DoTransform;

  Application.OnIdle := AppEventsIdle;

  TabWarps.Tabs.AddObject('Square Root Radial',TSqrRadialWarpTransformation.Create);
  TabWarps.Tabs.AddObject('ArcSin Radial',TInvSinRadialWarpTransformation.Create);
  TabWarps.Tabs.AddObject('Sin Radial',TSinRadialWarpTransformation.Create);
  TabWarps.Tabs.AddObject('Radial to Power',TPowerWarpTransformation.Create);
  TabWarps.Tabs.AddObject('Sin Cartesian',TSinWarpTransformation.Create);
  TabWarps.Tabs.AddObject('Suqare cartesian',TSquareWarpTransformation.Create);
  TabWarps.Tabs.AddObject('ArcSin Cartesian',TInvSinCartesianWarpTransformation.Create);
  TabWarps.Tabs.AddObject('1-ar² Cartesian',TRadialSquareWarpTransformation.Create);
  TabWarps.Tabs.AddObject('Modest lens',TModestLensWarpTransformation.Create);
  TabWarps.Tabs.AddObject('Logarithmic',TLogWarpTransformation.Create);
  TabWarps.Tabs.AddObject('Polynomial 3',TPoly3WarpTransformation.Create);
  TabWarps.Tabs.AddObject('Wide Lens',TWideLensWarpTransformation.Create);
  TabWarps.Tabs.AddObject('CylindricalH',TCylindricalHWarpTransformation.Create);
  TabWarps.Tabs.AddObject('CylindricalV',TCylindricalVWarpTransformation.Create);
  TabWarps.TabIndex := 0;
  TabWarpsChange(Sender);
  Edit1.Text := FloatToStr(ST.CenterX);
  Edit2.Text := FloatToStr(ST.CenterY);
  Edit3.Text := FloatToStr(ST.Ray);
  Edit4.Text := FloatToStr(ST.Lattitude);
  Edit5.Text := FloatToStr(ST.Longitude);
end;

procedure TForm1.ClearTransformations;
var
  I: Integer;
begin
  FillChar(Operation[0], SizeOf(TOpRecs), 0);
  for I := 0 to 7 do
  begin
    Operation[I].Sx := 1;
    Operation[I].Sy := 1;
    Operation[I].Cx := Src.Bitmap.Width / 2;
    Operation[I].Cy := Src.Bitmap.Height / 2;
  end;
end;

procedure TForm1.PrepareSource;
begin
  // make the border pixels transparent while keeping their RGB components
  SetBorderTransparent(Src.Bitmap, Src.Bitmap.BoundsRect);
end;

procedure TForm1.DoTransform;
var
  i, j: Integer;
  t:Cardinal;
begin
  GenTransform;
  Dst.BeginUpdate;

  Dst.Bitmap.Clear($00000000);
  t := GetTickCount;
  Transform(Dst.Bitmap, Src.Bitmap, TT,Rasterizer);
  t := GetTickCount - t;
  Dst.EndUpdate;
  Dst.Invalidate;
  LabelTime.Caption := format('Transformed in %.4f sec.',[t/1000]);
  if Mode = tmAffine then
  begin
    // fill the string grid
    for j := 0 to 2 do
      for i := 0 to 2 do
        StringGrid.Cells[i, j] := Format('%.3g', [AT.Matrix[i, j]]);
    StringGrid.Col := 3; // hide grid cursor
  end;
end;

procedure TForm1.GenTransform;
var
  I: Integer;
  Rec: TOpRec;
  S: string;
begin
  case Mode of
  tmProjective:
    begin
      PT.X0 := Vertices[0].X;
      PT.Y0 := Vertices[0].Y;
      PT.X1 := Vertices[1].X;
      PT.Y1 := Vertices[1].Y;
      PT.X2 := Vertices[2].X;
      PT.Y2 := Vertices[2].Y;
      PT.X3 := Vertices[3].X;
      PT.Y3 := Vertices[3].Y;
    end;
  tmAffine: // affine mode
    begin
      AT.Clear;
      for I := 0 to 7 do
      begin
        Rec := Operation[I];
        case Rec.OpType of
          opTranslate:  AT.Translate(Rec.Dx, Rec.Dy);
          opScale:      AT.Scale(Rec.Sx, Rec.Sy);
          opRotate:     AT.Rotate(Rec.Cx, Rec.Cy, Rec.Alpha);
          opSkew:       AT.Skew(Rec.Fx, Rec.Fy);
        end;
        case Rec.OpType of
          opTranslate:  s := s + Format('Translate(%.3g, %.3g); ', [Rec.Dx, Rec.Dy]);
          opScale:      s := s + Format('Scale(%.3g, %.3g); ', [Rec.Sx, Rec.Sy]);
          opRotate:     s := s + Format('Rotate(%.3g, %.3g, %3g); ', [Rec.Cx, Rec.Cy, Rec.Alpha]);
          opSkew:       s := s + Format('Skew(%.3g, %.3g); ', [Rec.Fx, Rec.Fy]);
        end;
      end;
      if Length(s) = 0 then s := 'Clear;';
      CodeString.Text := s;
    end;
  end;
  TT.SrcRect := SrcRubberBandLayer.Location;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  I:Integer;
begin
  AT.Free;
  PT.Free;
  TWT.Free;
  BT.Free;
  DT.Free;
  FET.Free;
  for i := 0to TabWarps.Tabs.Count - 1 do
    TabWarps.Tabs.Objects[i].Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ClearTransformations;
  ShowSettings(Listbox.ItemIndex);
  DoTransform;
end;

procedure TForm1.ListBoxClick(Sender: TObject);
begin
  ShowSettings(ListBox.ItemIndex);
end;

procedure TForm1.ShowSettings(OperationNum: Integer);
begin
  LoadingValues := True;
  ListBox.ItemIndex := OperationNum;
  Current := @Operation[OperationNum];
  Combobox.ItemIndex := Ord(Current.OpType);
  NoteBook.PageIndex := Ord(Current.OpType);
  eDx.Text := Format('%.4g', [Current.Dx]);
  eDy.Text := Format('%.4g', [Current.Dy]);
  sbDx.Position := Round(Current.Dx * 10);
  sbDy.Position := Round(Current.Dy * 10);
  eSx.Text := Format('%.4g', [Current.Sx]);
  eSy.Text := Format('%.4g', [Current.Sy]);
  sbSx.Position := Round(Current.Sx * 100);
  sbSy.Position := Round(Current.Sy * 100);
  eCx.Text := Format('%.4g', [Current.Cx]);
  eCy.Text := Format('%.4g', [Current.Cy]);
  eAlpha.Text := Format('%.4g', [Current.Alpha]);
  sbAlpha.Position := Round(Current.Alpha * 2);
  eFx.Text := Format('%.4g', [Current.Fx]);
  eFy.Text := Format('%.4g', [Current.Fy]);
  sbFx.Position := Round(Current.Fx * 100);
  sbFy.Position := Round(Current.Fy * 100);
  LoadingValues := False;
end;

procedure TForm1.ComboBoxChange(Sender: TObject);
begin
  Current.OpType := OpTypes[ComboBox.ItemIndex];
  ShowSettings(ListBox.ItemIndex);
  DoTransform;
end;

procedure TForm1.TranslationChanged(Sender: TObject);
var
  Tx, Ty: Extended;
begin
  if LoadingValues then Exit;
  if GetVal(eDx.Text, Tx) and GetVal(eDy.Text, Ty) then
  begin
    Current.Dx := Tx;
    Current.Dy := Ty;
    DoTransform;
    LoadingValues := True;
    sbDx.Position := Round(Current.Dx * 10);
    sbDy.Position := Round(Current.Dy * 10);
    LoadingValues := False;
  end;
end;

procedure TForm1.TranslationScrolled(Sender: TObject);
begin
  if LoadingValues then Exit;
  Current.Dx := sbDx.Position / 10;
  Current.Dy := sbDy.Position / 10;
  DoTransform;
  LoadingValues := True;
  eDx.Text := FloatToStr(Current.Dx);
  eDy.Text := FloatToStr(Current.Dy);
  LoadingValues := False;
end;

procedure TForm1.ScaleChanged(Sender: TObject);
var
  Sx, Sy: Extended;
begin
  if LoadingValues then Exit;
  if GetVal(eSx.Text, Sx) and GetVal(eSy.Text, Sy) then
  begin
    Current.Sx := Sx;
    Current.Sy := Sy;
    DoTransform;
    LoadingValues := True;
    sbSx.Position := Round(Current.Sx * 100);
    sbSy.Position := Round(Current.Sy * 100);
    LoadingValues := False;
  end;
end;

procedure TForm1.ScaleScrolled(Sender: TObject);
begin
  if LoadingValues then Exit;
  Current.Sx := sbSx.Position / 100;
  Current.Sy := sbSy.Position / 100;
  DoTransform;
  LoadingValues := True;
  eSx.Text := FloatToStr(Current.Sx);
  eSy.Text := FloatToStr(Current.Sy);
  LoadingValues := False;
end;

procedure TForm1.RotationChanged(Sender: TObject);
var
  Cx, Cy, Alpha: Extended;
begin
  if LoadingValues then Exit;
  if GetVal(eCx.Text, Cx) and GetVal(eCy.Text, Cy) and
    GetVal(eAlpha.Text, Alpha) then
  begin
    Current.Cx := Cx;
    Current.Cy := Cy;
    Current.Alpha := Alpha;
    DoTransform;
    LoadingValues := True;
    sbAlpha.Position := Round(Alpha * 2);
    LoadingValues := False;
  end;
end;

procedure TForm1.RotationScrolled(Sender: TObject);
begin
  if LoadingValues then Exit;
  Current.Alpha := sbAlpha.Position / 2;
  DoTransform;
  LoadingValues := True;
  eAlpha.Text := FloatToStr(Current.Alpha / 2);
  LoadingValues := False;
end;

procedure TForm1.SkewChanged(Sender: TObject);
var
  Fx, Fy: Extended;
begin
  if LoadingValues then Exit;
  if GetVal(eFx.Text, Fx) and GetVal(eFy.Text, Fy) then
  begin
    Current.Fx := Fx;
    Current.Fy := Fy;
    DoTransform;
    LoadingValues := True;
    sbFx.Position := Round(Current.Fx * 10);
    sbFy.Position := Round(Current.Fy * 10);
    LoadingValues := False;
  end;
end;

procedure TForm1.SkewScrolled(Sender: TObject);
begin
  if LoadingValues then Exit;
  Current.Fx := sbFx.Position / 10;
  Current.Fy := sbFy.Position / 10;
  DoTransform;
  LoadingValues := True;
  eFx.Text := FloatToStr(Current.Fx);
  eFy.Text := FloatToStr(Current.Fy);
  LoadingValues := False;
end;

procedure TForm1.OpacityChange(Sender: TObject);
begin
  OpacityBar.Update;
  Src.Bitmap.MasterAlpha := OpacityBar.Position;
  DoTransform;
end;

procedure TForm1.InitVertices;
begin
  Vertices[0].X := 0;
  Vertices[0].Y := 0;
  Vertices[1].X := Src.Bitmap.Width - 1;
  Vertices[1].Y := 0;
  Vertices[2].X := Src.Bitmap.Width - 1;
  Vertices[2].Y := Src.Bitmap.Height - 1;
  Vertices[3].X := 0;
  Vertices[3].Y := Src.Bitmap.Height - 1;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage = TabSheet1 then
  begin
    Mode := tmAffine;
    TT := AT;
  end
  else if PageControl1.ActivePage = TabSheet2 then
  begin
    // set current transformation as projective
    Mode := tmProjective;
    TT := PT;
    InitVertices;
  end
  else if PageControl1.ActivePage = TabSheet3 then
  begin
    Mode := tmFishEye;
    TT := FET;
  end
  else if PageControl1.ActivePage = TabSheet4 then
  begin
    Mode := tmDisturbance;
    TT := DT;
  end
  else if PageControl1.ActivePage = TabSheet5 then
  begin
    Mode := tmTwirl;
    TT := TWT;
  end
  else if PageControl1.ActivePage = TabSheet6 then
  begin
    Mode := tmBloat;
    TT := BT;
  end
  else if PageControl1.ActivePage = TabSheet7 then
  begin
    Mode := tmWarp;
    TT := TTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]);
  end
  else if PageControl1.ActivePage = TabSheet8 then
  begin
    Mode := tmSphere;
    TT := ST;
  end;
  ResamplerClassNamesList.Parent := PageControl1.ActivePage;
  ResamplerLabel.Parent := PageControl1.ActivePage;
  KernelClassNamesList.Parent := PageControl1.ActivePage;
  KernelLabel.Parent := PageControl1.ActivePage;
  DoTransform;
end;

procedure TForm1.RubberLayerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  I: Integer;
begin
  // store current mouse position
  LastMousePos := Point(X, Y);
  isMouseDown := true;
  if Mode = tmProjective then
  begin
    DraggedVertex := -1;
    // find the vertex to drag
    for I := 0 to 3 do
      if (Abs(Vertices[I].X - X) < 3) and (Abs(Vertices[I].Y - Y) < 3) then
      begin
        DraggedVertex := I;
        Break;
      end;
    // or drag all of them, (DragVertex = 4)
    if DraggedVertex = -1 then
      DraggedVertex := 4;
  end
  else if (Mode = tmSphere) and CheckBox1.Checked then
    Rasterizer := RasterizerBoxed;
end;

procedure TForm1.RubberLayerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer; Layer: TCustomLayer);
var
  Dx, Dy, I: Integer;
begin
  Label30.Caption := intToStr(AlphaComponent(Dst.Bitmap.PixelS[X,Y]));
  if not isMouseDown then
    Exit;
  Dx := X - LastMousePos.X;
  Dy := Y - LastMousePos.Y;
  LastMousePos := Point(X, Y);
  if Mode = tmProjective then
  begin
    if DraggedVertex = -1 then Exit; // mouse is not pressed
    // update coords
    if DraggedVertex = 4 then
    begin
      for I := 0 to 3 do
      begin
        Inc(Vertices[I].X, Dx);
        Inc(Vertices[I].Y, Dy);
      end;
    end
    else
    begin
      Inc(Vertices[DraggedVertex].X, Dx);
      Inc(Vertices[DraggedVertex].Y, Dy);
      LabelConvex.Visible :=  not Convex(PointArrayToFloatArray(Vertices));
    end;

    DoTransform;
  end
  else if Mode = tmSphere then
  begin
    if ssShift in Shift then
    begin // drag center...
      ST.CenterX := ST.CenterX + Dx;
      ST.CenterY := ST.CenterY + Dy;
      Edit1.Text := FloatToStr(ST.CenterX);
      Edit2.Text := FloatToStr(ST.CenterY);
    end
    else
    begin
      ST.Longitude := ST.Longitude - (DX / ST.Ray) * HalfPIS;
      ST.Lattitude := ST.Lattitude - (DY / ST.Ray) * HalfPIS;
      Edit4.Text := FloatToStr(ST.Lattitude);
      Edit5.Text := FloatToStr(ST.Longitude);
    end;
    DoTransform;
  end;
end;

procedure TForm1.RubberLayerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  DraggedVertex := -1;
  isMouseDown := false;
  Rasterizer := RasteriserRegular;
  if (Mode = tmSphere) and CheckBox1.Checked then
    DoTransform;
end;

procedure TForm1.AppEventsIdle(Sender: TObject; var Done: Boolean);
begin
  if DraggedVertex >= 0 then Exit;
  StippleStart := StippleStart - 0.05;
  Dst.Invalidate;
end;

procedure TForm1.PaintHandles(Sender: TObject; BackBuffer: TBitmap32);
var
  I, X0, Y0, X1, Y1: Integer;

  procedure PaintVertex(X, Y: Integer);
  begin
    BackBuffer.FillRectS(X - 2, Y - 2, X + 2, Y + 2, clWhite32);
    BackBuffer.FrameRectS(X - 3, Y - 3, X + 3, Y + 3, clBlack32);
  end;

begin
  if Mode <> tmProjective then Exit;

  BackBuffer.SetStipple([clBlack32, clBlack32, clWhite32, clWhite32]);
  BackBuffer.StippleStep := 0.5;
  BackBuffer.StippleCounter := StippleStart;

  X0 := Vertices[3].X;
  Y0 := Vertices[3].Y;
  for I := 0 to 3 do
  begin
    X1 := Vertices[I].X;
    Y1 := Vertices[I].Y;
    BackBuffer.LineFSP(X0, Y0, X1, Y1);
    X0 := X1;
    Y0 := Y1;
  end;
  for I := 0 to 3 do PaintVertex(Vertices[I].X, Vertices[I].Y);
end;

procedure TForm1.ResamplerClassNamesListClick(Sender: TObject);
begin
  with ResamplerClassNamesList do
    if ItemIndex >= 0 then
      Src.Bitmap.ResamplerClassName:= Items[ ItemIndex ];
  DoTransform;
end;

procedure TForm1.SrcRBResizingEvent(Sender: TObject;
  const OldLocation: TFloatRect; var NewLocation: TFloatRect;
  DragState: TDragState; Shift: TShiftState);
begin
  Src.Invalidate;
  DoTransform;
end;

procedure TForm1.ResamplerClassNamesListChange(Sender: TObject);
var
  R: TBitmap32Resampler;
begin
  with ResamplerClassNamesList do
    if ItemIndex >= 0 then
    begin
      Src.Bitmap.BeginUpdate;
      R := TBitmap32ResamplerClass(ResamplerList[ItemIndex]).Create(Src.Bitmap);
      KernelClassNamesListChange(nil);
      Src.Bitmap.EndUpdate;
      Src.Bitmap.Changed;

      KernelClassNamesList.Visible := R is TKernelResampler;
      KernelLabel.Visible := KernelClassNamesList.Visible;
    end;
end;

procedure TForm1.KernelClassNamesListChange(Sender: TObject);
var
  Index: Integer;
begin
  Index := KernelClassNamesList.ItemIndex;
  if Src.Bitmap.Resampler is TKernelResampler then
  begin
    TKernelResampler(Src.Bitmap.Resampler).Kernel := TCustomKernelClass(KernelList[Index]).Create;
  end;
  DoTransform;
end;

procedure TForm1.DstPaintStage(Sender: TObject; Buffer: TBitmap32;
  StageNum: Cardinal);
const            //0..1
  Colors: array [Boolean] of TColor32 = ($FFFFFFFF, $FFB0B0B0); 
var 
  R: TRect;
  I, J: Integer; 
  OddY: Integer; 
  TilesHorz, TilesVert: Integer; 
  TileX, TileY: Integer;
  TileHeight, TileWidth: Integer; 
begin
  if Sender is TCustomImage32 then with TCustomImage32(Sender) do
  begin
    BeginUpdate;
    R := GetViewportRect;
    case PaintStages[StageNum].Parameter of
      1: begin //Draw Checkerboard
           TileHeight := 8;
           TileWidth := 8;

           TilesHorz := (R.Right - R.Left) div TileWidth;
           TilesVert := (R.Bottom - R.Top) div TileHeight;
           TileY := 0;

           for J := 0 to TilesVert do
           begin
             TileX := 0;
             OddY := J and $1;
             for I := 0 to TilesHorz do
             begin
               Buffer.FillRectS(TileX, TileY, TileX + TileWidth, TileY + TileHeight,Colors[I and $1 = OddY]);
               Inc(TileX, TileWidth);
             end;
             Inc(TileY, TileHeight);
           end
         end;
      2: Buffer.FrameRectS(R , $FF000000); //Draw Frame
    end;
    EndUpdate;
  end;
end;

procedure TForm1.EditDisturbanceChange(Sender: TObject);
begin
  if EditDisturbance.Text <> '' then
  begin
    DT.Disturbance := StrToFloatDef(EditDisturbance.Text,DT.Disturbance);
    DoTransform;
  end;
end;

procedure TForm1.EditTwirlChange(Sender: TObject);
begin
  if EditTwirl.Text <> '' then
  begin
    TWT.Twirl := StrToFloatDef(EditTwirl.Text,TWT.Twirl);
    DoTransform;
  end;
end;

procedure TForm1.EditBloatChange(Sender: TObject);
begin
  if EditBloat.Text <> '' then
  begin
    BT.BloatPower := StrToFloatDef(EditBloat.Text,BT.BloatPower);
    DoTransform;
  end;
end;

procedure TForm1.ButtonRepeatTransformClick(Sender: TObject);
begin
  DoTransform;
end;

procedure TForm1.ButtonBloatDownClick(Sender: TObject);
begin
  BT.BloatPower := BT.BloatPower - 0.05;
  EditBloat.Text := FloatToStr(BT.BloatPower);
  DoTransform;
end;

procedure TForm1.ButtonBloatUpClick(Sender: TObject);
begin
  BT.BloatPower := BT.BloatPower + 0.05;
  EditBloat.Text := FloatToStr(BT.BloatPower);
  DoTransform;
end;

procedure TForm1.ButtonTwirlDownClick(Sender: TObject);
begin
  TWT.Twirl := TWT.Twirl - 0.005;
  EditTwirl.Text := FloatToStr(TWT.Twirl);
  DoTransform;
end;

procedure TForm1.ButtonTwirlUpClick(Sender: TObject);
begin
  TWT.Twirl := TWT.Twirl + 0.005;
  EditTwirl.Text := FloatToStr(TWT.Twirl);
  DoTransform;
end;

procedure TForm1.ButtonDisturbanceDownClick(Sender: TObject);
begin
  DT.Disturbance := DT.Disturbance - 0.05;
  EditDisturbance.Text := FloatToStr(DT.Disturbance);
  DoTransform;
end;

procedure TForm1.ButtonDisturbanceUpClick(Sender: TObject);
begin
  DT.Disturbance := DT.Disturbance + 0.05;
  EditDisturbance.Text := FloatToStr(DT.Disturbance);
  DoTransform;
end;

procedure TForm1.ButtonLoadImageClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    Src.Bitmap.LoadFromFile(OpenPictureDialog1.FileName);
    SrcRubberBandLayer.Location := FloatRect(0, 0, Src.Bitmap.Width - 1, Src.Bitmap.Height - 1);
    DoTransform;
  end;
end;

procedure TForm1.DstResize(Sender: TObject);
begin
  Dst.SetupBitmap; // set the destination bitmap size to match the image size
  DoTransform;
end;

procedure TForm1.TabWarpsChange(Sender: TObject);
begin
  PanelParam1.Visible := false;
  PanelParam2.Visible := false;
  PanelParam3.Visible := false;
  if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TPowerWarpTransformation then
    with TPowerWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
    begin
      PanelParam1.Visible := true;
      EditParam1.Text := FloatToStr(PowerCoef);
    end;
  if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TCylindricalHWarpTransformation then
    with TCylindricalHWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
    begin
      PanelParam1.Visible := true;
      EditParam1.Text := FloatToStr(Focal);
    end;
  if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TCylindricalVWarpTransformation then
    with TCylindricalVWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
    begin
      PanelParam1.Visible := true;
      EditParam1.Text := FloatToStr(Focal);
    end;
  if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TRadialSquareWarpTransformation then
    with TRadialSquareWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
    begin
      PanelParam1.Visible := true;
      EditParam1.Text := FloatToStr(Param);
    end;
  if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TModestLensWarpTransformation then
    with TModestLensWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
    begin
      PanelParam1.Visible := true;
      EditParam1.Text := FloatToStr(Param);
    end;
  if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TLogWarpTransformation then
    with TLogWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
    begin
      PanelParam1.Visible := true;
      EditParam1.Text := FloatToStr(Param);
      PanelParam2.Visible := true;
      EditParam2.Text := FloatToStr(PowerCoef);
    end;
  if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TPoly3WarpTransformation then
    with TPoly3WarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
    begin
      PanelParam1.Visible := true;
      EditParam1.Text := FloatToStr(Coef1Order);
      PanelParam2.Visible := true;
      EditParam2.Text := FloatToStr(Coef2Order);
      PanelParam3.Visible := true;
      EditParam3.Text := FloatToStr(Coef3Order);
    end;
  if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TWideLensWarpTransformation then
    with TWideLensWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
    begin
      PanelParam1.Visible := true;
      EditParam1.Text := FloatToStr(Param);
    end;
  if (PageControl1.ActivePage = TabSheet7) then
  begin
    TT := TTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]);
    DoTransform;
  end;
end;

procedure TForm1.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  if (Button = btNext) then
    ChangeParam(1,0.05)
  else
    ChangeParam(1,-0.05);
end;

procedure TForm1.UpDown2Click(Sender: TObject; Button: TUDBtnType);
begin
  if (Button = btNext) then
    ChangeParam(1,0.5)
  else
    ChangeParam(1,-0.5);
end;

procedure TForm1.UpDown4Click(Sender: TObject; Button: TUDBtnType);
begin
  if (Button = btNext) then
    ChangeParam(2,0.05)
  else
    ChangeParam(2,-0.05);
end;

procedure TForm1.UpDown3Click(Sender: TObject; Button: TUDBtnType);
begin
  if (Button = btNext) then
    ChangeParam(2,0.5)
  else
    ChangeParam(2,-0.5);
end;

procedure TForm1.UpDown6Click(Sender: TObject; Button: TUDBtnType);
begin
  if (Button = btNext) then
    ChangeParam(3,0.05)
  else
    ChangeParam(3,-0.05);
end;

procedure TForm1.UpDown5Click(Sender: TObject; Button: TUDBtnType);
begin
  if (Button = btNext) then
    ChangeParam(3,0.5)
  else
    ChangeParam(3,-0.5);
end;

procedure TForm1.ChangeParam(i:integer; value: single);
begin
  if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TCylindricalHWarpTransformation then
    with TCylindricalHWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
    begin
      if i = 1 then
      begin
        Focal := Focal + value;
        EditParam1.Text := FloatToStr(Focal);
      end;
    end;
  if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TCylindricalVWarpTransformation then
    with TCylindricalVWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
    begin
      if i = 1 then
      begin
        Focal := Focal + value;
        EditParam1.Text := FloatToStr(Focal);
      end;
    end;
  if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TPowerWarpTransformation then
    with TPowerWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
    begin
      if i = 1 then
      begin
        PowerCoef := PowerCoef + value;
        EditParam1.Text := FloatToStr(PowerCoef);
      end;
    end;
  if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TRadialSquareWarpTransformation then
    with TRadialSquareWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
    begin
      if i = 1 then
      begin
        Param := Param + value;
        EditParam1.Text := FloatToStr(Param);
      end;
    end;
  if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TModestLensWarpTransformation then
    with TModestLensWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
    begin
      if i = 1 then
      begin
        Param := Param + value;
        EditParam1.Text := FloatToStr(Param);
      end;
    end;
  if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TLogWarpTransformation then
    with TLogWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
      case i of
      1:begin
          Param := Param + value;
          EditParam1.Text := FloatToStr(Param);
        end;
      2:begin
          PowerCoef := PowerCoef + value;
          EditParam2.Text := FloatToStr(PowerCoef);
        end;
      end;
  if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TPoly3WarpTransformation then
    with TPoly3WarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
      case i of
      1:begin
          Coef1Order := Coef1Order + value;
          EditParam1.Text := FloatToStr(Coef1Order);
        end;
      2:begin
          Coef2Order := Coef2Order + value;
          EditParam2.Text := FloatToStr(Coef2Order);
        end;
      3:begin
          Coef3Order := Coef3Order + value;
          EditParam3.Text := FloatToStr(Coef3Order);
        end;
      end;
  if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TWideLensWarpTransformation then
    with TWideLensWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
    begin
      if i = 1 then
      begin
        Param := Param + value;
        EditParam1.Text := FloatToStr(Param);
      end;
    end;
  DoTransform;
end;

procedure TForm1.EditParam1Change(Sender: TObject);
var
  z : single;
begin
  try
    z := StrToFloat(EditParam1.Text);
    if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TPowerWarpTransformation then
      with TPowerWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
      begin
        PowerCoef := z;
        EditParam1.Text := FloatToStr(PowerCoef);
      end;
    if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TRadialSquareWarpTransformation then
      with TRadialSquareWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
      begin
        Param := z;
        EditParam1.Text := FloatToStr(Param);
      end;
    if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TModestLensWarpTransformation then
      with TModestLensWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
      begin
        Param := Z;
        EditParam1.Text := FloatToStr(Param);
      end;
    if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TLogWarpTransformation then
      with TLogWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
      begin
        Param := z;
        EditParam1.Text := FloatToStr(Param);
      end;
    if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TPoly3WarpTransformation then
      with TPoly3WarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
      begin
        Coef1Order := z;
        EditParam1.Text := FloatToStr(Coef1Order);
      end;
    if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TWideLensWarpTransformation then
      with TWideLensWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
      begin
        Param := z;
        EditParam1.Text := FloatToStr(Param);
      end;
    if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TCylindricalHWarpTransformation then
      with TCylindricalHWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
      begin
        Focal := z;
        EditParam1.Text := FloatToStr(Focal);
      end;
    if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TCylindricalVWarpTransformation then
      with TCylindricalVWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
      begin
        Focal := z;
        EditParam1.Text := FloatToStr(Focal);
      end;
    DoTransform;
  except on EConvertError do Exit;
  end;
end;

procedure TForm1.EditParam2Change(Sender: TObject);
var
  z : single;
begin
  try
    z := StrToFloat(EditParam2.Text);
    if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TLogWarpTransformation then
      with TLogWarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
      begin
        PowerCoef := z;
        EditParam2.Text := FloatToStr(PowerCoef);
      end;
    if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TPoly3WarpTransformation then
      with TPoly3WarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
      begin
        Coef2Order := z;
        EditParam2.Text := FloatToStr(Coef2Order);
      end;
  except on EConvertError do Exit;
  end;
end;

procedure TForm1.EditParam3Change(Sender: TObject);
var
  z : single;
begin
  try
    z := StrToFloat(EditParam3.Text);
    if TabWarps.Tabs.Objects[TabWarps.TabIndex] is TPoly3WarpTransformation then
      with TPoly3WarpTransformation(TabWarps.Tabs.Objects[TabWarps.TabIndex]) do
      begin
        Coef3Order := z;
        EditParam3.Text := FloatToStr(Coef3Order);
      end;
  except on EConvertError do Exit;
  end;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  ST.CenterX := StrToFloatDef(Edit1.Text,ST.CenterX);
  DoTransform;
end;

procedure TForm1.Edit2Change(Sender: TObject);
begin
  ST.CenterY := StrToFloatDef(Edit2.Text,ST.CenterY);
  DoTransform;
end;

procedure TForm1.Edit3Change(Sender: TObject);
begin
  ST.Ray := StrToFloatDef(Edit3.Text,ST.Ray);
  DoTransform;
end;

procedure TForm1.Edit4Change(Sender: TObject);
begin
  ST.Lattitude := StrToFloatDef(Edit4.Text,ST.Lattitude);
  DoTransform;
end;

procedure TForm1.Edit5Change(Sender: TObject);
begin
  ST.Longitude := StrToFloatDef(Edit5.Text,ST.Longitude);
  DoTransform;
end;

procedure TForm1.UpDown8Click(Sender: TObject; Button: TUDBtnType);
begin
  if (Button = btNext) then
    ST.CenterX := ST.CenterX + TupDown(Sender).Tag
  else
    ST.CenterX := ST.CenterX - TupDown(Sender).Tag;
  edit1.Text := FloatToStr(ST.CenterX);
  DoTransform;
end;

procedure TForm1.UpDown10Click(Sender: TObject; Button: TUDBtnType);
begin
  if (Button = btNext) then
    ST.CenterY := ST.CenterY + TupDown(Sender).Tag
  else
    ST.CenterY := ST.CenterY - TupDown(Sender).Tag;
  edit2.Text := FloatToStr(ST.CenterY);
  DoTransform;
end;

procedure TForm1.UpDown11Click(Sender: TObject; Button: TUDBtnType);
begin
  if (Button = btNext) then
    ST.Ray := ST.Ray + TupDown(Sender).Tag
  else
    ST.Ray := ST.Ray - TupDown(Sender).Tag;
  edit3.Text := FloatToStr(ST.Ray);
  DoTransform;
end;

procedure TForm1.UpDown13Click(Sender: TObject; Button: TUDBtnType);
begin
  if (Button = btNext) then
    ST.Lattitude := ST.Lattitude + PI/10
  else
    ST.Lattitude := ST.Lattitude - PI/10;
  edit4.Text := FloatToStr(ST.Lattitude);
  DoTransform;
end;

procedure TForm1.UpDown14Click(Sender: TObject; Button: TUDBtnType);
begin
  if (Button = btNext) then
    ST.Lattitude := ST.Lattitude + PI/2
  else
    ST.Lattitude := ST.Lattitude - PI/2;
  edit4.Text := FloatToStr(ST.Lattitude);
  DoTransform;
end;

procedure TForm1.UpDown15Click(Sender: TObject; Button: TUDBtnType);
begin
  if (Button = btNext) then
    ST.Longitude := ST.Longitude + PI/10
  else
    ST.Longitude := ST.Longitude - PI/10;
  edit5.Text := FloatToStr(ST.Longitude);
  DoTransform;
end;

procedure TForm1.UpDown16Click(Sender: TObject; Button: TUDBtnType);
begin
  if (Button = btNext) then
    ST.Longitude := ST.Longitude + PI/2
  else
    ST.Longitude := ST.Longitude - PI/2;
  edit5.Text := FloatToStr(ST.Longitude);
  DoTransform;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SrcRubberBandLayer.Location := FloatRect(0,0,Src.Bitmap.width,src.Bitmap.Height);
  DoTransform;
end;

procedure TForm1.SrcMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer; Layer: TCustomLayer);
begin
  Label30.Caption := intToStr(AlphaComponent(Src.Bitmap.PixelS[X,Y]));
end;

end.
