unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, DBGrids,
  DBCtrls, DBExtCtrls, Process, dUtils, dClasses, dSqlBuilder, dOpf,
  dSQLdbBroker, sqlite3conn, db;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    DataSource1: TDataSource;
    edtRetorno: TDBDateEdit;
    edtMatricula: TDBEdit;
    DBGrid1: TDBGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses objBloqueio;
{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  AProcess: TProcess;
  AStringList: TStringList;
begin
  AProcess := TProcess.Create(nil);
  AStringList := TStringList.Create;

  AProcess.CommandLine :=
    'powershell -command "import-module ActiveDirectory; Set-ADuser jansen.pereira -description '
    + QuotedStr('S010101000 - FEDERAÇÃO NACIONAL DE SAÚDE SUPLEMENTAR') + '"';
  AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
  AProcess.Execute;

  AStringList.LoadFromStream(AProcess.Output);
  AStringList.SaveToFile('output.txt');

  AStringList.Free;
  AProcess.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  AProcess: TProcess;
  AStringList: TStringList;
  CounterVar: integer;
begin
  AProcess := TProcess.Create(nil);
  AStringList := TStringList.Create;

  AStringList.LoadFromFile('input.txt');

  for CounterVar := 0 to Pred(AStringList.Count) do
  begin

    AProcess.CommandLine := AStringList.Strings[CounterVar];
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    AProcess.Execute;

  end;

  AStringList.SaveToFile('output.txt');

  AStringList.Free;
  AProcess.Free;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i: Integer;
  b, e: TDateTime;
  con: TdSQLdbConnector;
  qry: TdSQLdbQuery;
  Bloqueio:TBloqueio;
  a:string;
begin
  con := TdSQLdbConnector.Create(nil);
  qry := TdSQLdbQuery.Create(con);
  Bloqueio := TBloqueio.Create;

  try
    con.Logger.Active := False;
    con.Logger.FileName := 'OUTPUT.LOG';
    con.Driver := 'sqlite3';
    con.Database := 'controlebloqueio.db';
    con.Connect;

    qry.SQL.Text := 'select AD_MAT as MATRICULA,AD_NOME as NOME, AD_DTINICIO AS INICIO, AD_DTFIM AS FIM, date('+quotedstr('dd/mm/yyyy')+',AD_DTRETORNO) AS AD_DTRETORNO from BLOQUEIO_AD';
     a := qry.SQL.text;
//    qry.SQL.Text := qry.SQL.Text + ' WHERE AD_DTRETORNO=' + QuotedStr(FormatDateTime('dd/MM/yyyy',now));
    DataSource1.DataSet := qry.DataSet;
    edtMatricula.DataSource := DataSource1;
    edtRetorno.DataSource := DataSource1;
    qry.Open;

    qry.First;

  finally
  end;

end;

procedure TForm1.Button4Click(Sender: TObject);
begin

end;

procedure TForm1.DataSource1DataChange(Sender: TObject; Field: TField);
begin

end;

end.
