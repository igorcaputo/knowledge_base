program demo1;

{$mode objfpc}{$H+}

uses
{$IFDEF DEBUG}
  heaptrc,
{$ENDIF}
  dUtils, dClasses, dSqlBuilder, dOpf, dSQLdbBroker, person, sysutils,
  oracleconnection;

var
  i: Integer;
  b, e: TDateTime;
  con: TdSQLdbConnector;
  qry: TdSQLdbQuery;
  per: TPerson;
begin
  con := TdSQLdbConnector.Create(nil);
  qry := TdSQLdbQuery.Create(con);
  per := TPerson.Create;
  try
    con.Logger.Active := True;
    con.Logger.FileName := 'OUTPUT.LOG';
    con.Driver := 'oracle';
    con.Database := 'RAC_MXM_WEB';
//    con.Host := 'MXM_PRD';
    con.User:='MXM_PRD';
    con.Password:='MXM_PRD#$01';
    con.Connect;
   (*
    qry.SQL.Text := 'delete from person';
    qry.Execute;
    qry.Apply;

    qry.SQL.Text := 'insert into person (id, name) values (:id, :name)';
    per.Id := 1;
    per.Name := 'Silvio';
    dUtils.dSetParams(per, qry.Params);
    qry.Execute;
    per.Id := 2;
    per.Name := 'Waldir';
    dUtils.dSetParams(per, qry.Params);
    qry.Execute;
    qry.Apply;   *)

    qry.SQL.Text := 'SELECT FAT_CDCLIFOR AS NAME FROM FATURAS_FAT';
    qry.Open;
   (* qry.First;
    while not qry.EOF do
    begin
      dUtils.dGetFields(per, qry.Fields);
      WriteLn('Record: ', per.id, ', ', per.Name);
      qry.Next;
    end;
     *)
    qry.First;
    b := Now;
    for i := 1 to 1000000 do
    begin
//      per.Id := 0;
//      per.Name := '';
      dUtils.dGetFields(per, qry.Fields);
      WriteLn(per.Name);
 //     WriteLn(qry.Field('FAT_CDFATURA').AsString);
    end;
    e := Now;
    WriteLn('Performance: ', FormatDateTime('hh:nn:ss.zzz', e - b));

    qry.Apply;

    ReadLn;
  finally
    per.Free;
    con.Free;
  end;
{$IFDEF DEBUG}
  DeleteFile('HEAP.TXT');
  SetHeapTraceOutput('HEAP.TXT');
  Sleep(1000);
{$ENDIF}
end.

