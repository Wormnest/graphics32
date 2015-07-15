unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, GR32_Image, StdCtrls, Controls,
  Forms, ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    Button1: TButton;
    Dest: TImage32;
    Src: TBitmap32List;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Label3: TLabel;
    Button8: TButton;
    Button9: TButton;
    Label4: TLabel;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    LabelWait: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses JPeg,GR32,G32_WConvolution,G32_Convolution,pqConvolution,GR32_Convolution;

procedure TForm1.Button1Click(Sender: TObject);
var
  i:integer;
  m,t:Cardinal;
begin
  m := 0;
  LabelWait.Visible := true;
  LabelWait.Refresh;
  for i := 1 to 100 do
  begin
    t := GetTickCount;
    ApplyConv3x3(Src.Bitmap[0],Dest.Bitmap,'');
    m := m + (GetTickCount-t);
  end;
  LabelWait.Visible := false;
  Memo1.Lines.Add('MMX='+IntToStr(m div 100)+'ms');
end;

procedure TForm1.Button2Click(Sender: TObject);
const
  kernel:array[0..8] of Integer = (-1, -1, -1, -1,  9, -1, -1, -1, -1);
var
  i:integer;
  m,t:Cardinal;
begin
  m := 0;
  LabelWait.Visible := true;
  LabelWait.Refresh;
  for i := 1 to 100 do
  begin
    Dest.Bitmap.Assign(Src.Bitmap[0]);
    t := GetTickCount;
    ConvolveI(kernel,1,Dest.Bitmap);
    m := m + (GetTickCount-t);
  end;
  LabelWait.Visible := false;
  Memo1.Lines.Add('Scanline='+IntToStr(m div 100)+'ms');
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i:integer;
  m,t:Cardinal;
begin
  m := 0;
  LabelWait.Visible := true;
  LabelWait.Refresh;
  for i := 1 to 100 do
  begin
    Dest.Bitmap.Assign(Src.Bitmap[0]);
    t := GetTickCount;
    ApplyConvolution(Dest.Bitmap, Src.Bitmap[0],KERNEL_ARRAY[6]);
    m := m + (GetTickCount-t);
  end;
  LabelWait.Visible := false;
  Memo1.Lines.Add('PixelPtr='+IntToStr(m div 100)+'ms');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Src.Bitmap[0].LoadFromFile(ExtractFilePath(Application.ExeName)+'image.jpg');
  Memo1.Lines.Add('Image('+IntToStr(Src.Bitmap[0].Width)+'x'+IntToStr(Src.Bitmap[0].Height)+')');
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  i:integer;
  m,t:Cardinal;
begin
  m := 0;
  LabelWait.Visible := true;
  LabelWait.Refresh;
  for i := 1 to 100 do
  begin
    Dest.Bitmap.Assign(Src.Bitmap[0]);
    t := GetTickCount;
    ApplyConvolution(Dest.Bitmap, Src.Bitmap[0],KERNEL_ARRAY[5]);
    m := m + (GetTickCount-t);
  end;
  LabelWait.Visible := false;
  Memo1.Lines.Add('Blur PixelPtr='+IntToStr(m div 100)+'ms');
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  i:integer;
  m,t:Cardinal;
begin
  m := 0;
  LabelWait.Visible := true;
  LabelWait.Refresh;
  for i := 1 to 100 do
  begin
    Dest.Bitmap.Assign(Src.Bitmap[0]);
    t := GetTickCount;
    ApplyConvolution3x3(Dest.Bitmap, Src.Bitmap[0],KERNEL_ARRAY[5]);
    m := m + (GetTickCount-t);
  end;
  LabelWait.Visible := false;
  Memo1.Lines.Add('Blur MMX/PPtr='+IntToStr(m div 100)+'ms');
end;


procedure TForm1.Button6Click(Sender: TObject);
var
  i:integer;
  m,t:Cardinal;
begin
  m := 0;
  LabelWait.Visible := true;
  LabelWait.Refresh;
  for i := 1 to 100 do
  begin
    Dest.Bitmap.Assign(Src.Bitmap[0]);
    t := GetTickCount;
    GaussianBlur(Dest.Bitmap, Src.Bitmap[0]);
    m := m + (GetTickCount-t);
  end;
  LabelWait.Visible := false;
  Memo1.Lines.Add('Gaussian Blur='+IntToStr(m div 100)+'ms');
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  i:integer;
  m,t:Cardinal;
begin
  m := 0;
  LabelWait.Visible := true;
  LabelWait.Refresh;
  for i := 1 to 100 do
  begin
    Dest.Bitmap.Assign(Src.Bitmap[0]);
    t := GetTickCount;
    ApplyConvolution3x3(Dest.Bitmap, Src.Bitmap[0],KERNEL_ARRAY[6]);
    m := m + (GetTickCount-t);
  end;
  LabelWait.Visible := false;
  Memo1.Lines.Add('new MMX/PiwelPtr='+IntToStr(m div 100)+'ms');
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  i:integer;
  m,t:Cardinal;
begin
  m := 0;
  LabelWait.Visible := true;
  LabelWait.Refresh;
  for i := 1 to 100 do
  begin
    Dest.Bitmap.Assign(Src.Bitmap[0]);
    t := GetTickCount;
    ApplyConvolution5x5(Dest.Bitmap, Src.Bitmap[0],cf5GaussianBlur);
    m := m + (GetTickCount-t);
  end;
  LabelWait.Visible := false;
  Memo1.Lines.Add('5x5 Blur='+IntToStr(m div 100)+'ms');
end;

procedure TForm1.Button9Click(Sender: TObject);
var
  i:integer;
  m,t:Cardinal;
begin
  m := 0;
  LabelWait.Visible := true;
  LabelWait.Refresh;
  for i := 1 to 100 do
  begin
    Dest.Bitmap.Assign(Src.Bitmap[0]);
    t := GetTickCount;
    ApplyConvolution5x5(Dest.Bitmap, Src.Bitmap[0],cf5Laplace);
    m := m + (GetTickCount-t);
  end;
  LabelWait.Visible := false;
  Memo1.Lines.Add('5x5 Laplace='+IntToStr(m div 100)+'ms');
end;

procedure TForm1.Button10Click(Sender: TObject);
var
  i:integer;
  m,t:Cardinal;
begin
  m := 0;
  LabelWait.Visible := true;
  LabelWait.Refresh;
  for i := 1 to 100 do
  begin
    Dest.Bitmap.Assign(Src.Bitmap[0]);
    t := GetTickCount;
    ApplyConvolution(Dest.Bitmap, Src.Bitmap[0],KERNEL_ARRAY[7]);
    m := m + (GetTickCount-t);
  end;
  LabelWait.Visible := false;
  Memo1.Lines.Add('Sharp PixelPtr='+IntToStr(m div 100)+'ms');
end;

procedure TForm1.Button11Click(Sender: TObject);
var
  i:integer;
  m,t:Cardinal;
begin
  m := 0;
  LabelWait.Visible := true;
  LabelWait.Refresh;
  for i := 1 to 100 do
  begin
    Dest.Bitmap.Assign(Src.Bitmap[0]);
    t := GetTickCount;
    ApplyConvolution3x3(Dest.Bitmap, Src.Bitmap[0],KERNEL_ARRAY[7]);
    m := m + (GetTickCount-t);
  end;
  LabelWait.Visible := false;
  Memo1.Lines.Add('Sharp MMX/PPtr='+IntToStr(m div 100)+'ms');
end;

procedure TForm1.Button12Click(Sender: TObject);
var
  i:integer;
  m,t:Cardinal;
begin
  m := 0;
  LabelWait.Visible := true;
  LabelWait.Refresh;
  for i := 1 to 100 do
  begin
    Dest.Bitmap.Assign(Src.Bitmap[0]);
    t := GetTickCount;
    Sharpness(Dest.Bitmap, Src.Bitmap[0]);
    m := m + (GetTickCount-t);
  end;
  LabelWait.Visible := false;
  Memo1.Lines.Add('Sharpness='+IntToStr(m div 100)+'ms');
end;

end.
