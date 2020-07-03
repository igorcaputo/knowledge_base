(*
  PressObjects, InterBase / Firebird database Broker
  Copyright (C) 2007 Laserpress Ltda.

  http://www.pressobjects.org

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
unit PressSqliteBroker;

{$I Press.inc}

interface

uses
  PressOPFClasses,
  PressOPFSQLBuilder,
  PressOPF, PressSession;

type
  TPressSqliteDDLBuilder = class(TPressOPFDDLBuilder)
  protected
    function InternalFieldTypeStr(AFieldType: TPressOPFFieldType): string; override;
    function InternalImplicitIndexCreation: Boolean; override;
    function InternalMaxIdentLength: Integer; override;
  public
    function CreateGeneratorStatement(const AName: string): string; override;
    function SelectGeneratorStatement: string; override;
  end;

implementation

uses
  SysUtils;

{ TPressSqliteDDLBuilder }

function TPressSqliteDDLBuilder.CreateGeneratorStatement(const AName: string): string;
begin
  Result := Format('create generator %s', [AName]);
end;

function TPressSqliteDDLBuilder.InternalFieldTypeStr(
  AFieldType: TPressOPFFieldType): string;
const
  CFieldTypeStr: array[TPressOPFFieldType] of string = (
   '',                  //  oftUnknown
   'varchar',           //  oftPlainString
   'varchar',           //  oftAnsiString
   'smallint',          //  oftInt16
   'integer',           //  oftInt32
   'bigint',            //  oftInt64
   'double precision',  //  oftDouble
   'decimal(14,4)',     //  oftCurrency
   'smallint',          //  oftBoolean
   'date',              //  oftDate
   'time',              //  oftTime
   'timestamp',         //  oftDateTime
   'blob sub_type 1',   //  oftMemo
   'blob');             //  oftBinary
begin
  Result := CFieldTypeStr[AFieldType];
end;

function TPressSqliteDDLBuilder.InternalImplicitIndexCreation: Boolean;
begin
  Result := True;
end;

function TPressSqliteDDLBuilder.InternalMaxIdentLength: Integer;
begin
  Result := 31;
end;

function TPressSqliteDDLBuilder.SelectGeneratorStatement: string;
begin
//  PressOPFService.StartTransaction;
//  PressOPFService.ExecuteStatement('update gen_opf set id=id+1', nil);
//  PressOPFService.Commit;
  Result := 'select rowid from gen_opf';
end;

end.

