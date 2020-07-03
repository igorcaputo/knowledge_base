unit TestOPFSessionTests;

{$mode objfpc}{$H+}

interface

uses
  TestOPFConfig;

type

  { TTestOPFSessionTests }

  TTestOPFSessionTests = class(TTestOPFInvoiceAutoMappingTestCase)
  published
    procedure DriverNotFound;
    procedure MappingNotFound;
    procedure WithoutModelWithAggregation;
    procedure WithoutModelWithoutInheritance;
    procedure WithoutModelWithInheritance;
  end;

implementation

uses
  sysutils,
  testregistry,
  JCoreClasses,
  JCoreOPFOID,
  JCoreOPFMetadata,
  JCoreOPFADM,
  JCoreOPFConfig,
  JCoreOPFSession,
  JCoreOPFMappingSQL,
  TestOPFModelContact,
  TestOPFModelInvoice;

{ TTestOPFSessionTests }

procedure TTestOPFSessionTests.DriverNotFound;
var
  VConfiguration: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
begin
  VConfiguration := TJCoreOPFConfiguration.Create;
  VConfiguration.AddMappingClass([TTestEmptyMapping]);
  try
    VSession := VConfiguration.CreateSession;
    Fail('EJCoreOPF(2102) expected');
  except
    on E: EJCoreOPF do
      if E.Code <> 2102 then
        raise;
  end;
  VConfiguration.AddDriverClass([TTestEmptyDriver]);
  try
    VConfiguration.DriverName := TTestEmptyDriver.DriverName + ' invalid';
    Fail('EJCoreOPF(2101) expected');
  except
    on E: EJCoreOPF do
      if E.Code <> 2101 then
        raise;
  end;
  VConfiguration.DriverName := TTestEmptyDriver.DriverName;
  VSession := VConfiguration.CreateSession;
  AssertNotNull(VSession);
end;

procedure TTestOPFSessionTests.MappingNotFound;
var
  VConfiguration: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VProduct: TProduct;
begin
  VConfiguration := TJCoreOPFConfiguration.Create(TJCoreOPFModel.Create);
  VConfiguration.DriverClass := TTestEmptyDriver;
  VConfiguration.Model.AddADMClass([TJCoreOPFADMAnsiStringNativeCtl]);
  VConfiguration.Model.OIDGenerator := TJCoreOPFOIDGeneratorGUID.Create;
  VSession := VConfiguration.CreateSession;
  AssertNotNull(VSession);
  VProduct := TProduct.Create;
  try
    AssertExceptionStore(VSession, VProduct, EJCoreOPF, 2116);
    VConfiguration.AddMappingClass([TTestEmptyMapping]);
    VSession.Store(VProduct);
  finally
    FreeAndNil(VProduct);
  end;
end;

procedure TTestOPFSessionTests.WithoutModelWithAggregation;
var
  VConfiguration: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VPerson: TTestProxyPerson;
  VInvoice: TInvoice;
begin
  VConfiguration := TJCoreOPFConfiguration.Create;
  VConfiguration.DriverClass := TTestSQLDriver;
  VConfiguration.AddMappingClass([TTestEmptyMapping]);
  VSession := VConfiguration.CreateSession;
  VPerson := TTestProxyPerson.Create;
  try
    try
      VSession.Store(VPerson);
      Fail('EJCoreOPF(2121) expected');
    except
      on E: EJCoreOPF do
        if E.Code <> 2121 then
          raise;
    end;
    VConfiguration.Model.AddClass([TTestProxyPhone, TTestProxyCity]);
    VConfiguration.Model.AddGenerics(TTestProxyPhoneList, TTestProxyPhone);
    VSession.Store(VPerson);
  finally
    FreeAndNil(VPerson);
  end;
  VInvoice := TInvoice.Create;
  try
    try
      VSession.Store(VInvoice);
      Fail('EJCoreOPF(2121) expected');
    except
      on E: EJCoreOPF do
        if E.Code <> 2121 then
          raise;
    end;
    VConfiguration.Model.AddClass([TAddress, TClient, TInvoiceItem]);
    VConfiguration.Model.AddGenerics(TInvoiceItemList, TInvoiceItem);
    VSession.Store(VInvoice);
  finally
    FreeAndNil(VInvoice);
  end;
end;

procedure TTestOPFSessionTests.WithoutModelWithoutInheritance;
var
  VConfiguration: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VProduct: TProduct;
begin
  VConfiguration := TJCoreOPFConfiguration.Create;
  VConfiguration.DriverClass := TTestSQLDriver;
  VConfiguration.AddMappingClass([TJCoreOPFSQLMapping]);
  VSession := VConfiguration.CreateSession;
  VProduct := TProduct.Create;
  try
    VProduct.Name := 'prod';
    VSession.Store(VProduct);
    AssertSQLDriverCommands([
     'WriteString ' + VProduct._proxy.OID.AsString,
     'WriteString prod',
     'ExecSQL INSERT INTO PRODUCT (ID,NAME) VALUES (?,?)']);
  finally
    FreeAndNil(VProduct);
  end;
end;

procedure TTestOPFSessionTests.WithoutModelWithInheritance;
var
  VConfiguration: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VCompany: TCompany;
begin
  VConfiguration := TJCoreOPFConfiguration.Create;
  VConfiguration.DriverClass := TTestSQLDriver;
  VConfiguration.AddMappingClass([TJCoreOPFSQLMapping]);
  VConfiguration.Model.AddClass([TAddress]);
  VSession := VConfiguration.CreateSession;
  VCompany := TCompany.Create;
  try
    VCompany.Name := 'comp';
    VCompany.ContactName := 'contact';
    VSession.Store(VCompany);
    AssertSQLDriverCommands([
     'WriteString ' + VCompany._proxy.OID.AsString,
     'WriteString comp',
     'WriteNull',
     'ExecSQL INSERT INTO CLIENT (ID,NAME,ADDRESS) VALUES (?,?,?)',
     'WriteString ' + VCompany._proxy.OID.AsString,
     'WriteString contact',
     'ExecSQL INSERT INTO COMPANY (ID,CONTACTNAME) VALUES (?,?)']);
  finally
    FreeAndNil(VCompany);
  end;
end;

initialization
  RegisterTest('jcore.opf.session', TTestOPFSessionTests);

end.

