object Form1: TForm1
  Left = 208
  Top = 110
  Caption = 'Form1'
  ClientHeight = 444
  ClientWidth = 324
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    324
    444)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 253
    Width = 76
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'RotLayer.Angle:'
    ExplicitTop = 292
  end
  object Label2: TLabel
    Left = 12
    Top = 285
    Width = 96
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'RotLayer.Position.X:'
    ExplicitTop = 324
  end
  object Label3: TLabel
    Left = 12
    Top = 313
    Width = 96
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'RotLayer.Position.Y:'
    ExplicitTop = 352
  end
  object Label4: TLabel
    Left = 12
    Top = 381
    Width = 85
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'ImgView32.Scale:'
    ExplicitTop = 420
  end
  object ImgView: TImgView32
    Left = 4
    Top = 8
    Width = 305
    Height = 234
    Anchors = [akLeft, akTop, akRight, akBottom]
    Scale = 1.000000000000000000
    ScrollBars.Color = clBtnShadow
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    SizeGrip = sgAuto
    OverSize = 0
    TabOrder = 0
  end
  object GaugeBar1: TGaugeBar
    Left = 120
    Top = 253
    Width = 153
    Height = 16
    Anchors = [akLeft, akBottom]
    Backgnd = bgPattern
    Max = 180
    Min = -180
    ShowHandleGrip = True
    Position = 0
    OnChange = GaugeBar1Change
  end
  object GaugeBar2: TGaugeBar
    Left = 120
    Top = 285
    Width = 153
    Height = 16
    Anchors = [akLeft, akBottom]
    Backgnd = bgPattern
    Max = 200
    ShowHandleGrip = True
    Position = 100
    OnChange = GaugeBar2Change
  end
  object GaugeBar3: TGaugeBar
    Left = 120
    Top = 313
    Width = 153
    Height = 16
    Anchors = [akLeft, akBottom]
    Backgnd = bgPattern
    Max = 200
    ShowHandleGrip = True
    Position = 100
    OnChange = GaugeBar2Change
  end
  object GaugeBar4: TGaugeBar
    Left = 120
    Top = 381
    Width = 153
    Height = 16
    Anchors = [akLeft, akBottom]
    Backgnd = bgPattern
    Min = -100
    ShowHandleGrip = True
    Position = 0
    OnChange = GaugeBar4Change
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 345
    Width = 125
    Height = 17
    Alignment = taLeftJustify
    Anchors = [akLeft, akBottom]
    Caption = 'RotLayer.Scaled:'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CheckBox1Click
  end
end
