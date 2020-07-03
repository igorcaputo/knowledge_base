unit TestOPFModelContact;

{$mode objfpc}{$H+}

interface

uses
  fgl,
  JCoreClasses,
  JCoreEntity;

type

  { TTestIdBase }

  TTestIdBase = class(TObject)
  private
    FId: Integer;
  public
    destructor Destroy; override;
  published
    property _ID: Integer read FId write FId;
  end;

  { TTestIdPerson }

  TTestIdPerson = class(TTestIdBase)
  private
    FName: string;
  published
    property Name: string read FName write FName;
  end;

  { TTestIPIDBase }

  TTestIPIDBase = class(TJCoreManagedObject)
  private
    FPID: IJCorePID;
  published
    property _PID: IJCorePID read FPID write FPID;
  end;

  { TTestIPIDSimple }

  TTestIPIDSimple = class(TObject)
  private
    FPID: IJCorePID;
    FField1: Integer;
  published
    property _PID: IJCorePID read FPID write FPID;
    property Field1: Integer read FField1 write FField1;
  end;

  { TTestIPIDAddress }

  TTestIPIDAddress = class(TTestIPIDBase)
  private
    FStreet: string;
    FZipCode: string;
  published
    property Street: string read FStreet write FStreet;
    property ZipCode: string read FZipCode write FZipCode;
  end;

  { TTestIPIDCity }

  TTestIPIDCity = class(TTestIPIDBase)
  private
    FName: string;
  published
    property Name: string read FName write FName;
  end;

  { TTestIPIDPhone }

  TTestIPIDPhone = class(TTestIPIDBase)
  private
    FNumber: string;
  published
    property Number: string read FNumber write FNumber;
  end;

  TTestIPIDPhoneList = specialize TFPGObjectList<TTestIPIDPhone>;

  { TTestIPIDLanguage }

  TTestIPIDLanguage = class(TTestIPIDBase)
  private
    FName: string;
  public
    constructor Create(const AName: string);
  published
    property Name: string read FName write FName;
  end;

  TTestIPIDLanguageList = specialize TFPGObjectList<TTestIPIDLanguage>;

  { TTestIPIDPerson }

  TTestIPIDPerson = class(TTestIPIDBase)
  private
    FName: string;
    FAge: Integer;
    FPhones: TTestIPIDPhoneList;
    FAddress: TTestIPIDAddress;
    FCity: TTestIPIDCity;
    FLanguages: TTestIPIDLanguageList;
    function GetLanguages: TTestIPIDLanguageList;
    function GetPhones: TTestIPIDPhoneList;
    procedure SetAddress(AValue: TTestIPIDAddress);
    procedure SetCity(AValue: TTestIPIDCity);
    procedure SetLanguages(AValue: TTestIPIDLanguageList);
    procedure SetPhones(AValue: TTestIPIDPhoneList);
  protected
    procedure Finit; override;
  published
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
    property Phones: TTestIPIDPhoneList read GetPhones write SetPhones;
    property Address: TTestIPIDAddress read FAddress write SetAddress;
    property City: TTestIPIDCity read FCity write SetCity stored False;
    property Languages: TTestIPIDLanguageList read GetLanguages write SetLanguages stored False;
  end;

  { TTestIPIDEmployee }

  TTestIPIDEmployee = class(TTestIPIDPerson)
  private
    FSalary: Currency;
  published
    property Salary: Currency read FSalary;
  end;

  { TTestProxyBase }

  TTestProxyBase = class(TJCoreManagedObject)
    _Proxy: TJCoreEntityProxy;
  protected
    procedure Finit; override;
  end;

  { TTestProxyCity }

  TTestProxyCity = class(TTestProxyBase)
  private
    FName: string;
  published
    property Name: string read FName write FName;
  end;

  { TTestProxyPhone }

  TTestProxyPhone = class(TTestProxyBase)
  private
    FNumber: string;
  published
    property Number: string read FNumber write FNumber;
  end;

  TTestProxyPhoneList = specialize TFPGObjectList<TTestProxyPhone>;

  { TTestProxyPerson }

  TTestProxyPerson = class(TTestProxyBase)
  private
    FName: string;
    FAge: Integer;
    FPhones: TTestProxyPhoneList;
    FCity: TTestProxyCity;
    function GetCity: TTestProxyCity;
    function GetPhones: TTestProxyPhoneList;
    procedure SetCity(AValue: TTestProxyCity);
    procedure SetPhones(AValue: TTestProxyPhoneList);
  protected
    procedure Finit; override;
  published
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
    property Phones: TTestProxyPhoneList read GetPhones write SetPhones;
    property City: TTestProxyCity read GetCity write SetCity;
  end;

var
  TestContactModel: IJCoreModel;

implementation

uses
  sysutils;

{ TTestIdBase }

destructor TTestIdBase.Destroy;
begin
  TestContactModel.ReleaseEntity(Self);
  inherited Destroy;
end;

{ TTestIPIDLanguage }

constructor TTestIPIDLanguage.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

{ TTestIPIDPerson }

function TTestIPIDPerson.GetLanguages: TTestIPIDLanguageList;
begin
  if not Assigned(FLanguages) then
    FLanguages := TTestIPIDLanguageList.Create;
  Result := FLanguages;
end;

function TTestIPIDPerson.GetPhones: TTestIPIDPhoneList;
begin
  if not Assigned(FPhones) then
    FPhones := TTestIPIDPhoneList.Create;
  Result := FPhones;
end;

procedure TTestIPIDPerson.SetAddress(AValue: TTestIPIDAddress);
begin
  FreeAndNil(FAddress);
  FAddress := AValue;
end;

procedure TTestIPIDPerson.SetCity(AValue: TTestIPIDCity);
begin
  if FCity <> AValue then
  begin
    FreeAndNil(FCity);
    FCity := AValue;
  end;
end;

procedure TTestIPIDPerson.SetLanguages(AValue: TTestIPIDLanguageList);
begin
  FreeAndNil(FLanguages);
  FLanguages := AValue;
end;

procedure TTestIPIDPerson.SetPhones(AValue: TTestIPIDPhoneList);
begin
  FreeAndNil(FPhones);
  FPhones := AValue;
end;

procedure TTestIPIDPerson.Finit;
begin
  FreeAndNil(FPhones);
  FreeAndNil(FAddress);
  FreeAndNil(FCity);
  FreeAndNil(FLanguages);
  inherited Finit;
end;

{ TTestProxyBase }

procedure TTestProxyBase.Finit;
begin
  FreeAndNil(_Proxy);
  inherited Finit;
end;

{ TTestProxyPerson }

function TTestProxyPerson.GetCity: TTestProxyCity;
begin
  _Proxy.Lazyload(@FCity);
  Result := FCity;
end;

function TTestProxyPerson.GetPhones: TTestProxyPhoneList;
begin
  _Proxy.Lazyload(@FPhones);
  Result := FPhones;
end;

procedure TTestProxyPerson.SetCity(AValue: TTestProxyCity);
begin
  if FCity <> AValue then
  begin
    FreeAndNil(FCity);
    FCity := AValue;
  end;
end;

procedure TTestProxyPerson.SetPhones(AValue: TTestProxyPhoneList);
begin
  if FPhones <> AValue then
  begin
    FreeAndNil(FPhones);
    FPhones := AValue;
  end;
end;

procedure TTestProxyPerson.Finit;
begin
  FreeAndNil(FPhones);
  FreeAndNil(FCity);
  inherited Finit;
end;

end.

