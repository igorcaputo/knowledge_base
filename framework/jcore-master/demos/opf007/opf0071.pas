program opf0071;

{$mode objfpc}{$H+}

uses
  heaptrc,
  sysutils,
//  mssqlconn,
  sqlite3conn,
  JCoreDIC,
  JCoreLogger,
  JCoreEntity,
  JCoreOPFConfig,
  JCoreOPFSession,
  JCoreOPFMappingSQL,
  JCoreOPFOID,
  JCoreOPFDriverSQLdb;

type

  { TPerson }

  TPerson1 = class(TJCoreEntity)
  private
    FName: string;
  published
    property Name: string read FName write FName;
  end;

var
  VConfig: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VPerson: TPerson1;

{
  create table person (
    id integer identity,
    name varchar(255)
  );
  alter table person
    add constraint pk_person primary key (id);
}

begin
  TJCoreDIC.LazyRegister(IJCoreLogFactory, TJCoreConsoleLogFactory, jdsApplication);
  VConfig := TJCoreOPFConfiguration.Create;
  VConfig.Params.Values['connection'] := 'sqlite3';
  VConfig.Params.Values['database'] := 'C:\Desenvolvimento\Projetos\SisExtrator_SQlite\banco.db';
  VConfig.DriverClass := TJCoreOPFDriverSQLdb;
  VConfig.AddMappingClass([TJCoreOPFSQLMapping]);
  VConfig.Model.AcquireMetadata(TPerson1).TableName:='Person';

  VConfig.Model.OIDGenerator := TJCoreOPFOIDGeneratorAutoinc.Create;
  VConfig.Model.OIDClass := TJCoreOPFOIDInt64;
  VSession := VConfig.CreateSession;
  VPerson := TPerson1.Create;
  try
    VPerson.Name := 'Igor Caputo';
    VSession.Store(VPerson);
    VPerson := VSession.Retrieve(TPerson1, 'Igor Caputo') as TPerson1;

  finally
    FreeAndNil(VPerson);
  end;
end.

