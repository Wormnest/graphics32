object RGBALoaderForm: TRGBALoaderForm
  Left = 200
  Top = 138
  BorderStyle = bsDialog
  Caption = 'New Bitmap Layer with Alpha Channel'
  ClientHeight = 367
  ClientWidth = 412
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 96
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 412
    Height = 2
    Align = alTop
    Shape = bsTopLine
  end
  object Label2: TLabel
    Left = 24
    Top = 83
    Width = 57
    Height = 13
    Caption = 'RGB Image:'
  end
  object Label3: TLabel
    Left = 216
    Top = 83
    Width = 64
    Height = 13
    Caption = 'Alpha Image:'
  end
  object Label4: TLabel
    Left = 24
    Top = 294
    Width = 323
    Height = 13
    Caption = 'If the images have different sizes, the alpha image will be rescaled.'
  end
  object SpeedButton1: TSpeedButton
    Left = 148
    Top = 80
    Width = 21
    Height = 21
    Caption = '+'
    Flat = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Layout = blGlyphBottom
    ParentFont = False
    OnClick = SpeedButton1Click
  end
  object SpeedButton2: TSpeedButton
    Left = 170
    Top = 80
    Width = 21
    Height = 21
    Caption = '-'
    Flat = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Layout = blGlyphBottom
    ParentFont = False
    OnClick = SpeedButton2Click
  end
  object SpeedButton3: TSpeedButton
    Left = 340
    Top = 80
    Width = 21
    Height = 21
    Caption = '+'
    Flat = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Layout = blGlyphBottom
    ParentFont = False
    OnClick = SpeedButton3Click
  end
  object SpeedButton4: TSpeedButton
    Left = 362
    Top = 80
    Width = 21
    Height = 21
    Caption = '-'
    Flat = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Layout = blGlyphBottom
    ParentFont = False
    OnClick = SpeedButton4Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 2
    Width = 412
    Height = 55
    Align = alTop
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 6
      Width = 382
      Height = 39
      Caption = 'Load two images, one of them will be used to fill RGB components '#13#10'of the layer, another will be converted to a grayscale image and '#13#10'used as alpha channel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
    end
  end
  object ImgRGB: TImgView32
    Left = 24
    Top = 112
    Width = 169
    Height = 169
    Color = clAppWorkSpace
    ParentColor = False
    Scale = 1
    ScrollBars.Color = clBtnShadow
    ScrollBars.ShowHandleGrip = True
    SizeGrip = sgAuto
    TabOrder = 1
  end
  object Button1: TButton
    Left = 96
    Top = 80
    Width = 49
    Height = 21
    Caption = 'Load...'
    TabOrder = 2
    OnClick = Button1Click
  end
  object ImgAlpha: TImgView32
    Left = 216
    Top = 112
    Width = 169
    Height = 169
    Color = clAppWorkSpace
    ParentColor = False
    Scale = 1
    ScrollBars.Color = clBtnShadow
    ScrollBars.ShowHandleGrip = True
    SizeGrip = sgAuto
    TabOrder = 3
  end
  object Button2: TButton
    Left = 288
    Top = 80
    Width = 49
    Height = 21
    Caption = 'Load...'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 232
    Top = 328
    Width = 75
    Height = 21
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object Button4: TButton
    Left = 312
    Top = 328
    Width = 75
    Height = 21
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object Button5: TButton
    Left = 24
    Top = 328
    Width = 75
    Height = 21
    Caption = 'Reset Scales'
    TabOrder = 7
    OnClick = Button5Click
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 192
    Top = 56
  end
end
