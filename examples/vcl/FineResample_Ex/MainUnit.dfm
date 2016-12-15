object Form1: TForm1
  Left = 192
  Top = 114
  Caption = 'FineResample Ex'
  ClientHeight = 189
  ClientWidth = 278
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    278
    189)
  PixelsPerInch = 96
  TextHeight = 13
  object Image32: TImage32
    Left = 12
    Top = 12
    Width = 145
    Height = 131
    Anchors = [akLeft, akTop, akRight, akBottom]
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smStretch
    TabOrder = 0
  end
  object RadioGroup1: TRadioGroup
    Left = 169
    Top = 12
    Width = 93
    Height = 157
    Anchors = [akTop, akRight]
    Caption = 'StretchFilter'
    ItemIndex = 0
    Items.Strings = (
      'sfNearest'
      'sfDraft'
      'sfLinear '
      'sfCosine'
      'sfSpline'
      'sfLanczos'
      'sfMitchell')
    TabOrder = 1
    OnClick = RadioGroup1Click
  end
  object CheckBox1: TCheckBox
    Left = 169
    Top = 172
    Width = 93
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'FullEdge'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = CheckBox1Click
  end
end
