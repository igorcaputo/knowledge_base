{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LabeledControls;

interface

uses
  LabeledCtrls, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LabeledCtrls', @LabeledCtrls.Register);
end;

initialization
  RegisterPackage('LabeledControls', @Register);
end.
