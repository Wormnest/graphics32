program Image32_Ex;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  Forms, {$IFDEF FPC}Interfaces,{$ENDIF}
  MainUnit in 'MainUnit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
