unit TestOPFMetadataTests;

{$mode objfpc}{$H+}

interface

uses
  TestOPFConfig;

type

  { TTestOPFMetadataTest }

  TTestOPFMetadataTest = class(TTestOPFInvoiceManualMappingTestCase)
  published
    procedure NoPIDEntity;
    procedure CreatePIDInheritance;
    procedure CircularReference;
    procedure OwnOwnedMetadata;
    procedure OwnInvalidMetadata;
    procedure AddGenericNonEntity;
    procedure PropertyCompositionType;
    procedure PropertySize;
  end;

  { TTestOPFIPIDMetadataTests }

  TTestOPFIPIDMetadataTests = class(TTestOPFIPIDContactTestCase)
  published
    procedure CreateIPID;
    procedure AttributeList;
    procedure InheritedAttributeList;
    procedure NonPidAttributeList;
  end;

  { TTestOPFProxyMetadataTests }

  TTestOPFProxyMetadataTests = class(TTestOPFProxyContactTestCase)
  published
    procedure CreateProxy;
    procedure LazyEntity;
    procedure LazyCollection;
  end;

  { TTestOPFIdMetadataTests }

  TTestOPFIdMetadataTests = class(TTestOPFIdContactTestCase)
  published
    procedure CreateId;
    procedure CreateAltId;
    procedure CreateOverridenId;
    procedure UpdateId;
    procedure DisposeId;
  end;

implementation

uses
  sysutils,
  Classes,
  fgl,
  testregistry,
  JCoreClasses,
  JCoreEntity,
  JCoreMetadata,
  JCoreOPFConfig,
  JCoreOPFMetadata,
  TestOPFModelContact,
  TestOPFModelInvoice,
  TestOPFModelCircular;

type
  TTestPIDFriend = class(TJCoreOPFPID);

{ TTestOPFMetadataTest }

type

  { TTestPersonNoPID }

  TTestPersonNoPID = class(TPersistent)
  private
    FName: string;
  published
    property Name: string read FName write FName;
  end;

procedure TTestOPFMetadataTest.NoPIDEntity;
var
  VPerson: TTestPersonNoPID;
begin
  CreateConfigIPIDContactAuto;
  VPerson := TTestPersonNoPID.Create;
  try
    try
      Session.Store(VPerson);
      Fail('EJCoreOPF(2108) expected');
    except
      on E: EJCoreOPF do
        if E.Code <> 2108 then
          raise;
    end;
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFMetadataTest.CreatePIDInheritance;
var
  VCompany: TCompany;
  VPID: TJCoreOPFPID;
  VADMMap: TJCoreOPFADMMap;
begin
  VCompany := TCompany.Create;
  try
    Session.Store(VCompany);
    AssertNotNull('company proxy', VCompany._proxy);
    VPID := VCompany._proxy.PID as TJCoreOPFPID;
    AssertNotNull('company pid', VPID);
    VADMMap := TTestPIDFriend(VPID).ADMMap;
    AssertEquals('pid cnt adm', 3, VADMMap.Count);
    AssertEquals('pid.adm0', 'Name', VADMMap.Data[0].Metadata.Name);
    AssertEquals('pid.adm1', 'Address', VADMMap.Data[1].Metadata.Name);
    AssertEquals('pid.adm2', 'ContactName', VADMMap.Data[2].Metadata.Name);
  finally
    FreeAndNil(VCompany);
  end;
end;

procedure TTestOPFMetadataTest.CircularReference;
var
  VPerson: TCircularPerson;
begin
  CreateConfigProxyCircularAuto;
  VPerson := TCircularPerson.Create;
  try
    Session.Store(VPerson);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFMetadataTest.OwnOwnedMetadata;
var
  VConfig: IJCoreOPFConfiguration;
  VInvoiceMetadata: TJCoreOPFClassMetadata;
  VInvoiceItemMetadata: TJCoreOPFClassMetadata;
  VInvoiceClientMetadata: TJCoreOPFAttrMetadata;
begin
  VConfig := TJCoreOPFConfiguration.Create;
  VConfig.Model.AddClass([TClient, TPerson, TAddress, TInvoice, TInvoiceItem]);
  VConfig.Model.AddGenerics(TInvoiceItemList, TInvoiceItem);
  VInvoiceMetadata := VConfig.Model.AcquireMetadata(TInvoice);
  VInvoiceItemMetadata := VConfig.Model.AcquireMetadata(TInvoiceItem);
  AssertEquals('invoice item owned by invoice',
   VInvoiceMetadata.TheClass, VInvoiceItemMetadata.OwnerClass.TheClass);
  VInvoiceClientMetadata := VConfig.Model.AcquireMetadata(TInvoice).AttributeByName('Client');
  try
    VInvoiceItemMetadata.OwnerAttr := VInvoiceClientMetadata;
    Fail('EJCoreMetadata(0502) expected');
  except
    on E: EJCoreMetadata do
      if E.Code <> 502 then
        raise;
  end;
end;

procedure TTestOPFMetadataTest.OwnInvalidMetadata;
var
  VConfig: IJCoreOPFConfiguration;
  VInvoiceItemMetadata: TJCoreOPFClassMetadata;
  VInvoiceDateMetadata: TJCoreOPFAttrMetadata;
begin
  VConfig := TJCoreOPFConfiguration.Create;
  VConfig.Model.AddClass([TClient, TAddress, TInvoice, TInvoiceItem]);
  VConfig.Model.AddGenerics(TInvoiceItemList, TInvoiceItem);
  VInvoiceItemMetadata := VConfig.Model.AcquireMetadata(TInvoiceItem);
  VInvoiceDateMetadata := VConfig.Model.AcquireMetadata(TInvoice).AttributeByName('Date');
  VInvoiceItemMetadata.OwnerAttr := nil;
  try
    VInvoiceItemMetadata.OwnerAttr := VInvoiceDateMetadata;
    Fail('EJCoreMetadata(0503) expected');
  except
    on E: EJCoreMetadata do
      if E.Code <> 503 then
        raise;
  end;
end;

type
  TPhoneTestNonEntity = class(TObject)
  private
    FNumber: string;
  published
    property Number: string read FNumber write FNumber;
  end;
  TPhoneTestNonEntityList = specialize TFPGObjectList<TPhoneTestNonEntity>;
  TPersonTestNonEntity = class(TObject)
  private
    FName: string;
    FPhones: TPhoneTestNonEntityList;
  published
    property Name: string read FName write FName;
    property Phones: TPhoneTestNonEntityList read FPhones write FPhones;
  end;

procedure TTestOPFMetadataTest.AddGenericNonEntity;
var
  VConfig: IJCoreOPFConfiguration;
  VMetadata: TJCoreOPFClassMetadata;
begin
  VConfig := TJCoreOPFConfiguration.Create;
  VConfig.Model.AddClass([TPersonTestNonEntity]);
  VConfig.Model.AddGenerics(TPhoneTestNonEntityList, TPhoneTestNonEntity);
  VMetadata := VConfig.Model.AcquireMetadata(TPhoneTestNonEntity);
  AssertEquals(VMetadata.TheClass.ClassName, TPhoneTestNonEntity.ClassName);
end;

type

  { TTestPropertyCompositionType }

  TTestPropertyCompositionType = class(TObject)
  private
    FP01: string;
    FP02: string;
    FP03: string;
    FP04: TTestPropertyCompositionType;
    FP05: TTestPropertyCompositionType;
    FP06: TTestPropertyCompositionType;
  published
    property P01: string read FP01 write FP01;
    property P02: string read FP02 write FP02 stored False;
    property P03: string read FP03 write FP03 stored True;
    property P04: TTestPropertyCompositionType read FP04 write FP04;
    property P05: TTestPropertyCompositionType read FP05 write FP05 stored False;
    property P06: TTestPropertyCompositionType read FP06 write FP06 stored True;
  end;

procedure TTestOPFMetadataTest.PropertyCompositionType;
var
  VConfig: IJCoreOPFConfiguration;
  VP01, VP02, VP03, VP04, VP05, VP06: TJCoreOPFAttrMetadata;
begin
  VConfig := TJCoreOPFConfiguration.Create;
  VConfig.Model.AddClass([TTestPropertyCompositionType]);
  VP01 := VConfig.Model.AcquireAttrMetadata(TTestPropertyCompositionType, 'P01');
  VP02 := VConfig.Model.AcquireAttrMetadata(TTestPropertyCompositionType, 'P02');
  VP03 := VConfig.Model.AcquireAttrMetadata(TTestPropertyCompositionType, 'P03');
  VP04 := VConfig.Model.AcquireAttrMetadata(TTestPropertyCompositionType, 'P04');
  VP05 := VConfig.Model.AcquireAttrMetadata(TTestPropertyCompositionType, 'P05');
  VP06 := VConfig.Model.AcquireAttrMetadata(TTestPropertyCompositionType, 'P06');
  AssertEquals('P01 composition type', Ord(jctNone), Ord(VP01.CompositionType));
  AssertEquals('P02 composition type', Ord(jctNone), Ord(VP02.CompositionType));
  AssertEquals('P03 composition type', Ord(jctNone), Ord(VP03.CompositionType));
  AssertEquals('P04 composition type', Ord(jctComposition), Ord(VP04.CompositionType));
  AssertEquals('P05 composition type', Ord(jctAggregation), Ord(VP05.CompositionType));
  AssertEquals('P06 composition type', Ord(jctComposition), Ord(VP06.CompositionType));
end;

type

  { TTestPropertySize }

  TTestPropertySize = class(TObject)
  private
    FP01: Integer;
    FP02: Integer;
    FP03: Integer;
    FP04: string;
    FP05: string;
    FP06: string;
  published
    property P01: Integer read FP01 write FP01;
    property P02: Integer index 25 read FP02 write FP02;
    property P03: Integer index 100 read FP03 write FP03;
    property P04: string read FP04 write FP04;
    property P05: string index 25 read FP05 write FP05;
    property P06: string index 100 read FP06 write FP06;
  end;

procedure TTestOPFMetadataTest.PropertySize;
var
  VConfig: IJCoreOPFConfiguration;
  VP01, VP02, VP03, VP04, VP05, VP06: TJCoreOPFAttrMetadata;
begin
  VConfig := TJCoreOPFConfiguration.Create;
  VConfig.Model.AddClass([TTestPropertySize]);
  VP01 := VConfig.Model.AcquireAttrMetadata(TTestPropertySize, 'P01');
  VP02 := VConfig.Model.AcquireAttrMetadata(TTestPropertySize, 'P02');
  VP03 := VConfig.Model.AcquireAttrMetadata(TTestPropertySize, 'P03');
  VP04 := VConfig.Model.AcquireAttrMetadata(TTestPropertySize, 'P04');
  VP05 := VConfig.Model.AcquireAttrMetadata(TTestPropertySize, 'P05');
  VP06 := VConfig.Model.AcquireAttrMetadata(TTestPropertySize, 'P06');
  AssertEquals('P01 composition type', 0, VP01.Size);
  AssertEquals('P02 composition type', 0, VP02.Size);
  AssertEquals('P03 composition type', 0, VP03.Size);
  AssertEquals('P04 composition type', 0, VP04.Size);
  AssertEquals('P05 composition type', 25, VP05.Size);
  AssertEquals('P06 composition type', 100, VP06.Size);
end;

{ TTestOPFIPIDMetadataTests }

procedure TTestOPFIPIDMetadataTests.CreateIPID;
var
  VPerson: TTestIPIDPerson;
  VPID: IJCorePID;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    AssertNull(VPerson._PID);
    Session.Store(VPerson);
    VPID := VPerson._PID;
    AssertNotNull(VPID);
    AssertSame(VPID.Entity, VPerson);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFIPIDMetadataTests.AttributeList;
var
  VMetadata: TJCoreOPFClassMetadata;
begin
  VMetadata := Session.AcquireMetadata(TTestIPIDPerson);
  AssertEquals('meta.cnt', 6, VMetadata.AttributeCount);
  AssertEquals('meta0.name', 'Name', VMetadata[0].Name);
  AssertEquals('meta1.name', 'Age', VMetadata[1].Name);
  AssertEquals('meta2.name', 'Phones', VMetadata[2].Name);
  AssertEquals('meta3.name', 'Address', VMetadata[3].Name);
  AssertEquals('meta4.name', 'City', VMetadata[4].Name);
  AssertEquals('meta5.name', 'Languages', VMetadata[5].Name);
end;

procedure TTestOPFIPIDMetadataTests.InheritedAttributeList;
var
  VMetadata: TJCoreOPFClassMetadata;
begin
  VMetadata := Session.AcquireMetadata(TTestIPIDEmployee);
  AssertEquals('meta.cnt', 1, VMetadata.AttributeCount);
  AssertEquals('meta0.name', 'Salary', VMetadata[0].Name);
end;

procedure TTestOPFIPIDMetadataTests.NonPidAttributeList;
var
  VMetadata: TJCoreOPFClassMetadata;
begin
  VMetadata := Session.AcquireMetadata(TTestIPIDSimple);
  AssertEquals('meta.cnt', 1, VMetadata.AttributeCount);
  AssertEquals('meta0.name', 'Field1', VMetadata[0].Name);
end;

{ TTestOPFProxyMetadataTests }

procedure TTestOPFProxyMetadataTests.CreateProxy;
var
  VCity: TTestProxyCity;
begin
  VCity := TTestProxyCity.Create;
  try
    AssertNull('city.proxy', VCity._Proxy);
    AssertTrue('city.isdirty', VCity._Proxy.IsDirty);
    AssertFalse('city.ispersistent', VCity._Proxy.IsPersistent);
    AssertNull('city.oid null', VCity._Proxy.OID);
    AssertNull('city.owner null', VCity._Proxy.Owner);
    AssertNull('city.pid null', VCity._Proxy.PID);
    Session.Store(VCity);
    AssertNotNull('city.pid', VCity._Proxy.PID);
    AssertSame('city.entity', VCity._Proxy.PID.Entity, VCity);
  finally
    FreeAndNil(VCity);
  end;
end;

procedure TTestOPFProxyMetadataTests.LazyEntity;
var
  VPerson: TTestProxyPerson;
  VCity: TTestProxyCity;
begin
  TTestSQLDriver.Data.Add('2');
  TTestSQLDriver.Data.Add('somename');
  TTestSQLDriver.Data.Add('20');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('4');
  TTestSQLDriver.ExpectedResultsets.Add(1);
  VPerson := Session.Retrieve(TTestProxyPerson, '2') as TTestProxyPerson;
  try
    AssertEquals('person cmd', 0, TTestSQLDriver.Data.Count);
    AssertEquals('person exprs', 0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertNotNull('person nil', VPerson);
    AssertNotNull('person oid nil', VPerson._Proxy.OID);
    AssertEquals('person oid', '2', VPerson._Proxy.OID.AsString);
    AssertEquals('person name', 'somename', VPerson.Name);
    AssertSQLDriverCommands([
     'WriteInt64 2',
     'ExecSQL SELECT ID,NAME,AGE,ADDRESS,CITY FROM PERSON WHERE ID=?']);

    TTestSQLDriver.Data.Add('4');
    TTestSQLDriver.Data.Add('acity');
    TTestSQLDriver.ExpectedResultsets.Add(1);
    TTestSQLDriver.Commands.Clear;
    VCity := VPerson.City;
    AssertEquals('city cmd', 0, TTestSQLDriver.Data.Count);
    AssertEquals('city exprs', 0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertNotNull('city nil', VCity);
    AssertNotNull('city oid nil', VCity._Proxy.OID);
    AssertEquals('city oid', '4', VCity._Proxy.OID.AsString);
    AssertEquals('city name', 'acity', VCity.Name);
    AssertSQLDriverCommands([
     'WriteInt64 4',
     'ExecSQL SELECT ID,NAME FROM CITY WHERE ID=?']);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFProxyMetadataTests.LazyCollection;
var
  VPerson: TTestProxyPerson;
  VPhones: TTestProxyPhoneList;
begin
  TTestSQLDriver.Data.Add('21');
  TTestSQLDriver.Data.Add('name');
  TTestSQLDriver.Data.Add('38');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.ExpectedResultsets.Add(1);
  VPerson := Session.Retrieve(TTestProxyPerson, '21') as TTestProxyPerson;
  try
    AssertEquals('person cmd', 0, TTestSQLDriver.Data.Count);
    AssertEquals('person exprs', 0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertNotNull('person nil', VPerson);
    AssertNotNull('person oid nil', VPerson._Proxy.OID);
    AssertEquals('person oid', '21', VPerson._Proxy.OID.AsString);
    AssertEquals('person name', 'name', VPerson.Name);
    AssertSQLDriverCommands([
     'WriteInt64 21',
     'ExecSQL SELECT ID,NAME,AGE,ADDRESS,CITY FROM PERSON WHERE ID=?']);

    TTestSQLDriver.Data.Add('18');
    TTestSQLDriver.Data.Add('1213-9876');
    TTestSQLDriver.Data.Add('19');
    TTestSQLDriver.Data.Add('2231-6621');
    TTestSQLDriver.Data.Add('20');
    TTestSQLDriver.Data.Add('9989-3399');
    TTestSQLDriver.ExpectedResultsets.Add(3);
    TTestSQLDriver.Commands.Clear;
    VPhones := VPerson.Phones;
    AssertEquals('phones cmd', 0, TTestSQLDriver.Data.Count);
    AssertEquals('phones exprs', 0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertNotNull('phones nil', VPhones);
    AssertEquals('phones count', 3, VPhones.Count);
    AssertNotNull('phone0 oid nil', VPhones[0]._Proxy.OID);
    AssertEquals('phone0 oid', '18', VPhones[0]._Proxy.OID.AsString);
    AssertEquals('phone0 number', '1213-9876', VPhones[0].Number);
    AssertNotNull('phone1 oid nil', VPhones[1]._Proxy.OID);
    AssertEquals('phone1 oid', '19', VPhones[1]._Proxy.OID.AsString);
    AssertEquals('phone1 number', '2231-6621', VPhones[1].Number);
    AssertNotNull('phone2 oid nil', VPhones[2]._Proxy.OID);
    AssertEquals('phone2 oid', '20', VPhones[2]._Proxy.OID.AsString);
    AssertEquals('phone2 number', '9989-3399', VPhones[2].Number);
    AssertSQLDriverCommands([
     'WriteInt64 21',
     'ExecSQL SELECT ID,NUMBER FROM PHONE WHERE PERSON=?']);
  finally
    FreeAndNil(VPerson);
  end;
end;

{ TTestOPFIdMetadataTests }

procedure TTestOPFIdMetadataTests.CreateId;
var
  VPerson: TTestIdPerson;
begin
  VPerson := TTestIdPerson.Create;
  try
    VPerson.Name := 'jane';
    Session.Store(VPerson);
    AssertEquals('person id', 1, VPerson._ID);
    AssertSQLDriverCommands([
     'WriteInt32 1',
     'WriteString jane',
     'ExecSQL INSERT INTO TESTIDPERSON (ID,NAME) VALUES (?,?)']);
  finally
    FreeAndNil(VPerson);
  end;
end;

type

  { TTestOIDPerson }

  TTestOIDPerson = class(TPersistent)
  private
    FName: string;
    FOID: string;
  published
    property _OID: string read FOID write FOID;
    property Name: string read FName write FName;
  end;

procedure TTestOPFIdMetadataTests.CreateAltId;
var
  VPerson: TTestOIDPerson;
begin
  VPerson := TTestOIDPerson.Create;
  try
    VPerson.Name := 'jack';
    Session.Store(VPerson);
    AssertEquals('person oid', '1', VPerson._OID);
    AssertSQLDriverCommands([
     'WriteString 1',
     'WriteString jack',
     'ExecSQL INSERT INTO TESTOIDPERSON (OID,NAME) VALUES (?,?)']);
  finally
    FreeAndNil(VPerson);
  end;
end;

type

  { TTestBaseOID }

  TTestBaseOID = class(TPersistent)
  private
    FOID: Integer;
  published
    property _OID: Integer read FOID write FOID;
  end;

  { TTestOverrideOID }

  TTestOverrideOID = class(TTestBaseOID)
  private
    FID: Integer;
    FName: string;
  published
    property _ID: Integer read FID write FID;
    property Name: string read FName write FName;
  end;


procedure TTestOPFIdMetadataTests.CreateOverridenId;
begin
  try
    Config.Model.AcquireMetadata(TTestOverrideOID);
    Fail('EJCoreOPF(2135) expected');
  except
    on E: EJCoreOPF do
      if E.Code <> 2135 then
        raise;
  end;
end;

procedure TTestOPFIdMetadataTests.UpdateId;
var
  VPerson: TTestIdPerson;
begin
  VPerson := TTestIdPerson.Create;
  try
    VPerson.Name := 'james';
    Session.Store(VPerson);
    AssertEquals('person id', 1, VPerson._ID);
    VPerson.Name := 'joe';
    TTestSQLDriver.Commands.Clear;
    Session.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteString joe',
     'WriteInt32 1',
     'ExecSQL UPDATE TESTIDPERSON SET NAME=? WHERE ID=?']);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFIdMetadataTests.DisposeId;
var
  VPerson: TTestIdPerson;
begin
  VPerson := TTestIdPerson.Create;
  try
    Session.Store(VPerson);
    AssertEquals('person id', 1, VPerson._ID);
    TTestSQLDriver.Commands.Clear;
    Session.Dispose(VPerson);
    AssertEquals('person id', 0, VPerson._ID);
    AssertSQLDriverCommands([
     'WriteInt32 1',
     'ExecSQL DELETE FROM TESTIDPERSON WHERE ID=?']);
  finally
    FreeAndNil(VPerson);
  end;
end;

initialization
  RegisterTest('jcore.opf.metadata', TTestOPFMetadataTest);
  RegisterTest('jcore.opf.metadata', TTestOPFIPIDMetadataTests);
  RegisterTest('jcore.opf.metadata', TTestOPFProxyMetadataTests);
  RegisterTest('jcore.opf.metadata', TTestOPFIdMetadataTests);

end.

