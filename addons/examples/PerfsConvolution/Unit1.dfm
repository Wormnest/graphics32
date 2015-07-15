object Form1: TForm1
  Left = 198
  Top = 114
  Width = 800
  Height = 600
  Caption = 'The Morpheus Convolution Benchmark...'
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 800
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 201
    Height = 566
    Align = alLeft
    TabOrder = 0
    DesignSize = (
      201
      566)
    object Label1: TLabel
      Left = 8
      Top = 210
      Width = 185
      Height = 33
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Test 100 times the convolution function "find Edge".'
      WordWrap = True
    end
    object Label2: TLabel
      Left = 8
      Top = 328
      Width = 185
      Height = 33
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Test 100 times the convolution function "Blur".'
      WordWrap = True
    end
    object Label3: TLabel
      Left = 8
      Top = 508
      Width = 185
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Test 100 times the 5x5 functions.'
      WordWrap = True
    end
    object Label4: TLabel
      Left = 8
      Top = 424
      Width = 185
      Height = 33
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Test 100 times the Sharpness function.'
      WordWrap = True
    end
    object LabelWait: TLabel
      Left = 8
      Top = 196
      Width = 185
      Height = 13
      AutoSize = False
      Caption = 'Please wait, Test is runing...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
    end
    object Memo1: TMemo
      Left = 1
      Top = 1
      Width = 199
      Height = 192
      Align = alTop
      TabOrder = 0
    end
    object Button1: TButton
      Left = 8
      Top = 240
      Width = 185
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'MMX'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 8
      Top = 260
      Width = 185
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Scanline'
      TabOrder = 2
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 8
      Top = 280
      Width = 185
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'PixelPtr'
      TabOrder = 3
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 8
      Top = 376
      Width = 185
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'new MMX/PixelPtr'
      TabOrder = 4
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 8
      Top = 356
      Width = 185
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'PixelPtr (original)'
      TabOrder = 5
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 8
      Top = 396
      Width = 185
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Gaussian Blur'
      TabOrder = 6
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 8
      Top = 300
      Width = 185
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'new MMX/PixelPtr'
      TabOrder = 7
      OnClick = Button7Click
    end
    object Button8: TButton
      Left = 8
      Top = 524
      Width = 185
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Gaussian Blur'
      TabOrder = 8
      OnClick = Button8Click
    end
    object Button9: TButton
      Left = 8
      Top = 544
      Width = 185
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Edge Laplace'
      TabOrder = 9
      OnClick = Button9Click
    end
    object Button10: TButton
      Left = 8
      Top = 440
      Width = 185
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'PixelPtr (original)'
      TabOrder = 10
      OnClick = Button10Click
    end
    object Button11: TButton
      Left = 8
      Top = 460
      Width = 185
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'new MMX/PixelPtr'
      TabOrder = 11
      OnClick = Button11Click
    end
    object Button12: TButton
      Left = 8
      Top = 480
      Width = 185
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Sharpen'
      TabOrder = 12
      OnClick = Button12Click
    end
  end
  object Dest: TImage32
    Left = 201
    Top = 0
    Width = 591
    Height = 566
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 1
  end
  object Src: TBitmap32List
    Bitmaps = <
      item
        Bitmap.ResamplerClassName = 'TNearestResampler'
      end>
    Left = 8
    Top = 8
  end
end
