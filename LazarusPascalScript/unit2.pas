unit Unit2; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TForm2 }

  TForm2 = class(TForm)
    Label1: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form2: TForm2;



implementation




initialization
  {$I unit2.lrs}
  RegisterClass(TForm2);

finalization
  UnRegisterClass(TForm2);


end.

