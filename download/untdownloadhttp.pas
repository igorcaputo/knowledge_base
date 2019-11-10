unit untDownloadHTTP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, httpsend;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
function DownloadHTTP(URL: string; FileName: string): boolean;
var
  httpClient: THTTPSend;
  noErrors: boolean = True;
  Path: string;
begin
  httpClient := THTTPSend.Create;
//  trzvForm.escr(nDepuAlto, 'Va a descargar xml %s', [URL]);
  if httpClient.HTTPMethod('GET', URL) then
  begin
    // for windows
    FileName := StringReplace(FileName, '/', '\',
      [rfReplaceAll, rfIgnoreCase]);
    // Gets the current path
    Path := IncludeTrailingPathDelimiter(GetCurrentDir) + FileName;
    // Creates folders if they don't exit
    if not DirectoryExists(ExtractFilePath(Path)) then
    begin
      ForceDirectories(ExtractFilePath(Path));
    end;
    // saves file
    httpClient.Document.SaveToFile(FileName);
    if httpClient.ResultCode = 200 then
    begin
      //it's been downloaded
      //noErrors := False;
    end
    else
    begin
      //httpClient.ResultCode is not 200
      noErrors := False;
      DeleteFile(ExtractFilePath(Path));
    end;
  end
  else
  begin
//    trzvForm.escr(nAlarma, 'Error while downloading %s', [URL]);
    noErrors := False;
  end;
  Result := noErrors;
  httpClient.Free;
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  DownloadHTTP('http://pvista.proevento.com.br/_soap/get/busca_tpcnseg_pedidos.asp?var_ambiente=cnseg&var_codevento=133','aa1.txt');
end;

end.
