unit Unit3;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs;

type
  TForm3 = class(TForm)
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form3: TForm3;

implementation

initialization
  {$I unit3.lrs}
  RegisterClass(TForm3);

finalization
  UnRegisterClass(TForm3);

end.

