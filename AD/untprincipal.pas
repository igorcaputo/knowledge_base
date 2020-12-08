unit untPrincipal;

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
    Edit1: TEdit;
    edtRetorno: TDBDateEdit;
    DBGrid1: TDBGrid;
    edtInicio: TDBDateEdit;
    lbDtInicio: TLabel;
    lbMat: TLabel;
    lbDtRetorno: TLabel;
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
  con: TdSQLdbConnector;
  qry: TdSQLdbQuery;
  Bloqueio:TBloqueio;
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

    qry.SQL.Text := 'SELECT AD_LOGIN as LOGIN,COALESCE(AD_DTINICIO, ''1/1/1900 00:00:00'') AS INICIO, AD_DTFIM AS FIM, COALESCE(AD_DTRETORNO, ''1/1/1900 00:00:00'') as RETORNO, COALESCE(AD_DTCADASTRO, ''1/1/1900 00:00:00'') as CADASTRO  from BLOQUEIO_AD';
    qry.SQL.Text := qry.SQL.Text + ' WHERE AD_DTINICIO = :INICIO';
    qry.Param('INICIO').AsString := '07/12/2020';
    DataSource1.DataSet := qry.DataSet;
    edtInicio.DataSource := DataSource1;

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
