{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit graphics32_design;

interface

uses
  GR32_Reg, GR32_Dsgn_Bitmap, GR32_Dsgn_Color, GR32_Reg_ProgressBar, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GR32_Reg', @GR32_Reg.Register);
  RegisterUnit('GR32_Reg_ProgressBar', @GR32_Reg_ProgressBar.Register);
end;

initialization
  RegisterPackage('graphics32_design', @Register);
end.
