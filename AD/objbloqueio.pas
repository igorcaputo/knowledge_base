unit objBloqueio;

{$mode objfpc}{$H+}

interface

type

  { TBloqueio }

  TBloqueio = class(TObject)
  private
    FId: Int64;
    FName: string;
  published
    property Id: Int64 read FId write FId;
    property Name: string read FName write FName;
  end;

implementation

end.


