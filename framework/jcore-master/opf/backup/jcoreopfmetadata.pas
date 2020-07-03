(*
  JCore, OPF Mediator and Metadata Classes
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFMetadata;

{$I jcore.inc}
{$WARN 5024 OFF} // hint 'parameter not used'

interface

uses
  typinfo,
  fgl,
  JCoreClasses,
  JCoreEntity,
  JCoreMetadata,
  JCoreOPFDriver,
  JCoreOPFOID;

type

  TJCoreOPFADMEntity = class;
  TJCoreOPFADMCollection = class;

  TJCoreOPFPID = class;
  TJCoreOPFPIDClass = class of TJCoreOPFPID;
  TJCoreOPFPIDList = specialize TFPGObjectList<TJCoreOPFPID>;
  TJCoreOPFPIDMap = specialize TFPGMap<Pointer, TJCoreOPFPID>;
  TJCoreOPFPIDArray = array of TJCoreOPFPID;

  IJCoreOPFPIDManager = interface
    function AcquirePID(const AEntity: TObject): TJCoreOPFPID;
    function CreatePIDArray(const AItems: TJCoreObjectArray): TJCoreOPFPIDArray;
    procedure LoadEntity(const APID: TJCoreOPFPID; const AADM: TJCoreOPFADMEntity);
    procedure LoadCollection(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection);
    procedure StorePID(const APID: TJCoreOPFPID);
  end;

  TJCoreOPFAttributeType = (jatSimple, jatEntity, jatCollection);
  TJCoreOPFAttributeTypeSet = set of TJCoreOPFAttributeType;

  TJCoreOPFAttrMetadata = class;

  { TJCoreOPFADM }

  TJCoreOPFADM = class(TJCoreManagedObject, IJCoreADM)
  private
    FCacheUpdated: Boolean;
    FLoaded: Boolean;
    FMetadata: TJCoreOPFAttrMetadata;
    FPID: TJCoreOPFPID;
    class function IsGUIDEqual(const AGuid1, AGuid2: TGuid): Boolean;
  protected
    class function Implements(const ATypeInfo: PTypeInfo; const AClass: TClass): Boolean;
    class function Implements(const ATypeInfo: PTypeInfo; const AGUID: TGuid): Boolean;
    procedure InternalCommit; virtual;
    function InternalIsDirty: Boolean; virtual; abstract;
    procedure InternalLoad; virtual;
    procedure InternalUpdateCache; virtual; abstract;
  public
    constructor Create(const APID: TJCoreOPFPID; const AMetadata: TJCoreOPFAttrMetadata); virtual;
    class function Apply(const AModel: TJCoreModel; const AAttrTypeInfo: PTypeInfo): Boolean; virtual; abstract;
    class function AttributeType: TJCoreOPFAttributeType; virtual; abstract;
    procedure Commit;
    function IsDirty: Boolean;
    procedure Load;
    procedure ReadFromResultSet(const AResultSet: IJCoreOPFResultSet); virtual; abstract;
    procedure WriteToParams(const AParams: IJCoreOPFParams); virtual; abstract;
    property Metadata: TJCoreOPFAttrMetadata read FMetadata;
    property PID: TJCoreOPFPID read FPID;
  end;

  TJCoreOPFADMClass = class of TJCoreOPFADM;
  TJCoreOPFADMClassList = specialize TFPGList<TJCoreOPFADMClass>;
  TJCoreOPFADMArray = array of TJCoreOPFADM;
  TJCoreOPFADMMap = class(specialize TFPGMap<string, TJCoreOPFADM>);

  { TJCoreOPFADMValueType }

  TJCoreOPFADMValueType = class(TJCoreOPFADM)
  public
    class function Apply(const AModel: TJCoreModel; const AAttrTypeInfo: PTypeInfo): Boolean; override;
  end;

  { TJCoreOPFADMController }

  TJCoreOPFADMController = class(TJCoreOPFADM)
  private
    FAttrAddr: Pointer;
    FAttrPropInfo: PPropInfo;
  protected
    procedure InternalGetter; virtual; abstract;
    property AttrAddr: Pointer read FAttrAddr;
    property AttrPropInfo: PPropInfo read FAttrPropInfo;
  public
    constructor Create(const APID: TJCoreOPFPID; const AMetadata: TJCoreOPFAttrMetadata); override;
    procedure UpdateAttrAddr(const AAttrAddrRef: PPPointer);
  end;

  { TJCoreOPFADMObject }

  TJCoreOPFADMObject = class(TJCoreOPFADMController)
  private
    function GetValue: TObject;
    procedure SetValue(const AValue: TObject);
    function UseGetter: TObject;
    procedure UseSetter(const AValue: TObject);
  protected
    procedure InternalGetter; override;
  public
    property Value: TObject read GetValue write SetValue;
  end;

  { TJCoreOPFADMEntity }

  TJCoreOPFADMEntity = class(TJCoreOPFADMObject)
  private
    FCompositionOID: IJCoreOPFOID;
    FOIDCache: IJCoreOPFOID;
  protected
    function AcquirePID: TJCoreOPFPID;
    function InternalIsDirty: Boolean; override;
    procedure InternalLoad; override;
    procedure InternalUpdateCache; override;
    property OIDCache: IJCoreOPFOID read FOIDCache write FOIDCache;
  public
    class function Apply(const AModel: TJCoreModel; const AAttrTypeInfo: PTypeInfo): Boolean; override;
    procedure AssignComposition(const AComposite: TObject);
    class function AttributeType: TJCoreOPFAttributeType; override;
    procedure ReadFromResultSet(const AResultSet: IJCoreOPFResultSet); override;
    procedure WriteToParams(const AParams: IJCoreOPFParams); override;
    property CompositionOID: IJCoreOPFOID read FCompositionOID write FCompositionOID;
  end;

  { TJCoreOPFADMCollection }

  TJCoreOPFADMCollection = class(TJCoreOPFADMObject)
  private
    { TODO : Thread safety between arrays initialization and finish the transaction }
    FChangesUpdated: Boolean;
    FItemsArray: TJCoreObjectArray;
    FItemsArrayUpdated: Boolean;
    FOIDCache: TJCoreOPFOIDArray;
    FOIDRemoved: TJCoreOPFOIDArray;
    FPIDAdded: TJCoreOPFPIDArray;
    FPIDArray: TJCoreOPFPIDArray;
    FPIDArrayUpdated: Boolean;
    FPIDReorder: TJCoreOPFPIDArray;
    function ArrayContentIsDirty: Boolean;
    function ArrayLinkIsDirty: Boolean;
    function ArrayOrderIsDirty: Boolean;
    function ArraySizeIsDirty: Boolean;
    function GetItemsArray: TJCoreObjectArray;
    function GetOIDRemoved: TJCoreOPFOIDArray;
    function GetPIDAdded: TJCoreOPFPIDArray;
    function GetPIDArray: TJCoreOPFPIDArray;
    function GetPIDReorder: TJCoreOPFPIDArray;
    function HasOIDInCache(const AOID: IJCoreOPFOID): Boolean;
    function HasOIDInCollection(const APIDArray: TJCoreOPFPIDArray; const AOID: IJCoreOPFOID): Boolean;
    procedure UpdateChanges;
  protected
    procedure InternalAssignArray(const AArray: TJCoreObjectArray); virtual; abstract;
    procedure InternalCommit; override;
    function InternalCreateItemsArray: TJCoreObjectArray; virtual; abstract;
    function InternalIsDirty: Boolean; override;
    procedure InternalLoad; override;
    procedure InternalUpdateCache; override;
    property ItemsArray: TJCoreObjectArray read GetItemsArray;
    property OIDCache: TJCoreOPFOIDArray read FOIDCache write FOIDCache;
  public
    procedure AssignArray(const AArray: TJCoreObjectArray);
    class function AttributeType: TJCoreOPFAttributeType; override;
    function OrderIsDirty: Boolean;
    procedure ReadFromResultSet(const AResultSet: IJCoreOPFResultSet); override;
    procedure WriteToParams(const AParams: IJCoreOPFParams); override;
    property OIDRemoved: TJCoreOPFOIDArray read GetOIDRemoved;
    property PIDAdded: TJCoreOPFPIDArray read GetPIDAdded;
    property PIDArray: TJCoreOPFPIDArray read GetPIDArray;
    property PIDReorder: TJCoreOPFPIDArray read GetPIDReorder;
  end;

  { TJCoreOPFADMMapping }

  TJCoreOPFADMMapping = class(TJCoreOPFADMMap)
  // ADMs of a single class metadata, without parents.
  // Based on TJCoreOPFMaps and synchronized with TJCoreOPFMappingClassFactory's mappings.
  private
    FADMAttributeChanged: TJCoreOPFADMArray;
    FADMCollectionChanged: TJCoreOPFADMArray;
    FPID: TJCoreOPFPID;
    function BuildChangedADMArray(const AAttributeType: TJCoreOPFAttributeTypeSet): TJCoreOPFADMArray;
    function GetADM(const AIndex: Integer): TJCoreOPFADM;
    function GetADMAttributeChanged: TJCoreOPFADMArray;
    function GetADMByName(const AAttributeName: string): TJCoreOPFADM;
    function GetADMCollectionChanged: TJCoreOPFADMArray;
  protected
    function InternalIsDirty(const AIncludeCollections: Boolean): Boolean;
  public
    constructor Create(const APID: TJCoreOPFPID);
    function AcquireADM(const AAttributeName: string): TJCoreOPFADM;
    procedure Commit;
    function IsAttributesDirty: Boolean;
    function IsDirty: Boolean;
    property ADM[const AIndex: Integer]: TJCoreOPFADM read GetADM;
    property ADMByName[const AAttributeName: string]: TJCoreOPFADM read GetADMByName; default;
    property ADMAttributeChanged: TJCoreOPFADMArray read GetADMAttributeChanged;
    property ADMCollectionChanged: TJCoreOPFADMArray read GetADMCollectionChanged;
    property PID: TJCoreOPFPID read FPID;
  end;

  TJCoreOPFADMMappingMap = specialize TFPGMap<Pointer, TJCoreOPFADMMapping>;

  TJCoreOPFMap = class;
  TJCoreOPFMaps = specialize TFPGObjectList<TJCoreOPFMap>;
  TJCoreOPFMapMap = specialize TFPGMap<string, TJCoreOPFMap>;

  TJCoreOPFClassMetadata = class;

  { TJCoreOPFPID }

  { TODO :
    Sessions from different configurations need different PIDs,
    iow, the same entity may be persistent to a configuration
    and nonpersistent to another one }

  TJCoreOPFPID = class(TInterfacedObject, IJCorePID)
  private
    // Stores and owns all ADMs
    FADMMap: TJCoreOPFADMMap;
    // Stores maps of ADMs, synchronized with TJCoreOPFMaps
    FADMMappingMap: TJCoreOPFADMMappingMap;
    FAttrAddrRef: PPointer;
    FEntity: TObject;
    FIsPersistent: Boolean;
    FPIDManager: IJCoreOPFPIDManager;
    FMetadata: TJCoreOPFClassMetadata;
    FOID: IJCoreOPFOID;
    FOwner: TJCoreOPFPID;
    FOwnerADM: TJCoreOPFADMCollection;
    FSequence: Integer;
    FSequenceCache: Integer;
    function CreateADM(const AAttrMetadata: TJCoreOPFAttrMetadata): TJCoreOPFADM;
    procedure CreateADMs(const AMaps: TJCoreOPFMaps);
    function GetADMMapping(const AIndex: Integer): TJCoreOPFADMMapping;
    function GetPIDManager: IJCoreOPFPIDManager;
    function IGetEntity: TObject;
    function IGetIsPersistent: Boolean;
    function IGetOID: IJCoreOID;
    function IGetOwner: IJCorePID;
    function IJCorePID.Entity = IGetEntity;
    function IJCorePID.IsPersistent = IGetIsPersistent;
    function IJCorePID.OID = IGetOID;
    function IJCorePID.Owner = IGetOwner;
    procedure SetOID(const AValue: IJCoreOPFOID);
  protected
    function AcquireADMByAttrAddr(const AAttrAddr: Pointer): TJCoreOPFADM;
    property ADMMap: TJCoreOPFADMMap read FADMMap;
    property ADMMappingMap: TJCoreOPFADMMappingMap read FADMMappingMap;
    property PIDManager: IJCoreOPFPIDManager read GetPIDManager;
  public
    constructor Create(const AEntity: TObject; const AMetadata: TJCoreOPFClassMetadata);
    destructor Destroy; override;
    function AcquireADM(const AAttributeName: string): TJCoreOPFADM;
    function ADMByName(const AAttributeName: string): IJCoreADM;
    function ADMMappingCount: Integer;
    procedure AssignOwner(const AOwner: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection);
    procedure AttachManager(const APIDManager: IJCoreOPFPIDManager);
    procedure Commit;
    function IsDirty: Boolean;
    function Lazyload(const AAttrAddr: Pointer): Boolean;
    procedure ReadSequenceField(const AResultSet: IJCoreOPFResultSet);
    procedure WriteSequenceField(const AParams: IJCoreOPFParams);
    property ADMMapping[const AIndex: Integer]: TJCoreOPFADMMapping read GetADMMapping; default;
    property IsPersistent: Boolean read FIsPersistent;
    property Entity: TObject read FEntity;
    property Metadata: TJCoreOPFClassMetadata read FMetadata;
    property OID: IJCoreOPFOID read FOID write SetOID;
    property Owner: TJCoreOPFPID read FOwner;
    property Sequence: Integer read FSequence write FSequence;
    property SequenceCache: Integer read FSequenceCache;
  end;

  TJCoreOPFModel = class;

  { TJCoreOPFAttrMetadata }

  TJCoreOPFAttrMetadata = class(TJCoreAttrMetadata)
  private
    FADMClass: TJCoreOPFADMClass;
    FAttributeType: TJCoreOPFAttributeType;
    FExternalLinkLeftFieldName: string;
    FExternalLinkRightFieldName: string;
    FExternalLinkTableName: string;
    FHasLazyload: Boolean;
    FIsPersistent: Boolean;
    FOrderFieldName: string;
    FPersistentFieldName: string;
    function GetCompositionMetadata: TJCoreOPFClassMetadata;
    function GetHasExternalLink: Boolean;
    function GetModel: TJCoreOPFModel;
    function ReadComposition(const ATypeInfo: PTypeInfo): TClass;
  protected
    property Model: TJCoreOPFModel read GetModel;
  public
    constructor Create(const AModel: TJCoreModel; const AOwner: TJCoreClassMetadata; const APropInfo: PPropInfo); override;
    function CreateADM(const APID: TJCoreOPFPID): TJCoreOPFADM;
    function IsCollection: Boolean; override;
    procedure NoLazyload;
    property ADMClass: TJCoreOPFADMClass read FADMClass;
    property AttributeType: TJCoreOPFAttributeType read FAttributeType;
    property CompositionMetadata: TJCoreOPFClassMetadata read GetCompositionMetadata;
    property ExternalLinkLeftFieldName: string read FExternalLinkLeftFieldName write FExternalLinkLeftFieldName;
    property ExternalLinkRightFieldName: string read FExternalLinkRightFieldName write FExternalLinkRightFieldName;
    property ExternalLinkTableName: string read FExternalLinkTableName write FExternalLinkTableName;
    property HasExternalLink: Boolean read GetHasExternalLink;
    property HasLazyload: Boolean read FHasLazyload;
    property IsPersistent: Boolean read FIsPersistent write FIsPersistent;
    property OrderFieldName: string read FOrderFieldName write FOrderFieldName;
    property PersistentFieldName: string read FPersistentFieldName;
  end;

  TJCoreOPFAttrMetadataList = class(specialize TFPGObjectList<TJCoreOPFAttrMetadata>);
  TJCoreOPFAttrMetadataArray = array of TJCoreOPFAttrMetadata;

  { TJCoreOPFMap }

  TJCoreOPFMap = class(TJCoreOPFAttrMetadataList)
  // Map is a list of attributes that share the same persistence structure, eg a
  // table on a RDBMS)
  //
  // In the current version these attributes must be of the same class or a
  // parent class.
  private
    FHasInsertOIDFields: Boolean;
    FMetadata: TJCoreOPFClassMetadata;
    FOIDClass: TJCoreOPFOIDClass;
    FOIDGenerator: IJCoreOPFOIDGenerator;
    FOIDName: TJCoreStringArray;
    FOrderFieldName: string;
    FOwnerOIDName: TJCoreStringArray;
    FSubClasses: TJCoreClassArray;
    FSubMaps: TJCoreOPFMaps;
    FTableName: string;
  public
    constructor Create(const AMetadata: TJCoreOPFClassMetadata; const AIsRootMap: Boolean);
    function HasOrderField: Boolean;
    function HasOwnerOID: Boolean;
    property HasInsertOIDFields: Boolean read FHasInsertOIDFields;
    property Metadata: TJCoreOPFClassMetadata read FMetadata;
    property OIDClass: TJCoreOPFOIDClass read FOIDClass;
    property OIDGenerator: IJCoreOPFOIDGenerator read FOIDGenerator;
    property OIDName: TJCoreStringArray read FOIDName;
    property OrderFieldName: string read FOrderFieldName;
    property OwnerOIDName: TJCoreStringArray read FOwnerOIDName;
    property SubClasses: TJCoreClassArray read FSubClasses;
    property SubMaps: TJCoreOPFMaps read FSubMaps;
    property TableName: string read FTableName;
  end;

  { TJCoreOPFClassMetadata }

  TJCoreOPFClassMetadata = class(TJCoreClassMetadata)
  private
    FMaps: TJCoreOPFMaps;
    FOIDClass: TJCoreOPFOIDClass;
    FOIDGenerator: IJCoreOPFOIDGenerator;
    FOIDName: TJCoreStringArray;
    FOIDProp: TJCorePropInfoArray;
    FSubMaps: TJCoreOPFMaps;
    FTableName: string;
    procedure BuildOIDProperties;
    function GetAttributes(const AIndex: Integer): TJCoreOPFAttrMetadata;
    function GetMaps: TJCoreOPFMaps;
    function GetModel: TJCoreOPFModel;
    function GetParent: TJCoreOPFClassMetadata;
    function GetSubMaps: TJCoreOPFMaps;
    procedure SetOIDClass(const AValue: TJCoreOPFOIDClass);
  protected
    procedure AddAttribute(const APropInfo: PPropInfo); override;
    function IsPersistent: Boolean;
    property Model: TJCoreOPFModel read GetModel;
  public
    { TODO : Generics? }
    constructor Create(const AModel: TJCoreModel; const AClass: TClass; const AParent: TJCoreClassMetadata); override;
    destructor Destroy; override;
    function AttributeByName(const AAttributeName: string): TJCoreOPFAttrMetadata;
    function FindAttribute(const AAttributeName: string): TJCoreOPFAttrMetadata;
    property Attributes[const AIndex: Integer]: TJCoreOPFAttrMetadata read GetAttributes; default;
    property Maps: TJCoreOPFMaps read GetMaps;
    property OIDClass: TJCoreOPFOIDClass read FOIDClass write SetOIDClass;
    property OIDGenerator: IJCoreOPFOIDGenerator read FOIDGenerator write FOIDGenerator;
    property OIDName: TJCoreStringArray read FOIDName;
    property OIDProp: TJCorePropInfoArray read FOIDProp;
    property Parent: TJCoreOPFClassMetadata read GetParent;
    property SubMaps: TJCoreOPFMaps read GetSubMaps;
    property TableName: string read FTableName write FTableName;
  end;

  { TJCoreOPFModel }

  TJCoreOPFModel = class(TJCoreModel, IJCoreModel)
  { TODO : Model, map and metadata threadsafe }
  private
    FADMClassList: TJCoreOPFADMClassList;
    FMapMap: TJCoreOPFMapMap;
    FOIDClass: TJCoreOPFOIDClass;
    FOIDClassList: TJCoreOPFOIDClassList;
    FOIDGenerator: IJCoreOPFOIDGenerator;
    FOrderFieldName: string;
    FPIDCache: TJCoreOPFPIDMap;
    procedure AcquireMaps(const AMetadata: TJCoreOPFClassMetadata; const AMaps: TJCoreOPFMaps);
    function AcquirePIDFromCache(const AEntity: TObject): TJCoreOPFPID;
    function AcquirePIDFromIntfProp(const AEntity: TObject): TJCoreOPFPID;
    function AcquirePIDFromProxyField(const AEntity: TObject): TJCoreOPFPID;
    procedure AcquireSubMaps(const AMetadata: TJCoreOPFClassMetadata; const AMaps: TJCoreOPFMaps);
  protected
    function AcquireMap(const AMetadata: TJCoreOPFClassMetadata): TJCoreOPFMap;
    function AttributeMetadataClass: TJCoreAttrMetadataClass; override;
    function ClassMetadataClass: TJCoreClassMetadataClass; override;
    function CreateProxy(const APID: TJCoreOPFPID): TJCoreEntityProxy; virtual;
    procedure Finit; override;
    function PIDClass: TJCoreOPFPIDClass; virtual;
    property ADMClassList: TJCoreOPFADMClassList read FADMClassList;
    property MapMap: TJCoreOPFMapMap read FMapMap;
    property OIDClassList: TJCoreOPFOIDClassList read FOIDClassList;
    property PIDCache: TJCoreOPFPIDMap read FPIDCache;
  public
    constructor Create; override;
    function AcquireADMClass(const AAttrTypeInfo: PTypeInfo): TJCoreOPFADMClass;
    function AcquireAttrMetadata(const AClass: TClass; const AAttributeName: string): TJCoreOPFAttrMetadata;
    function AcquireMetadata(const AClass: TClass): TJCoreOPFClassMetadata;
    function AcquirePID(const AEntity: TObject): TJCoreOPFPID;
    procedure AddADMClass(const AADMClassArray: array of TJCoreOPFADMClass);
    procedure AddOIDClass(const AOIDClassArray: array of TJCoreOPFOIDClass);
    function CreateMaps(const AMetadata: TJCoreOPFClassMetadata): TJCoreOPFMaps;
    function CreateSubMaps(const AMetadata: TJCoreOPFClassMetadata): TJCoreOPFMaps;
    function FindOIDClass(const AOIDProp: TJCorePropInfoArray): TJCoreOPFOIDClass;
    procedure InitEntity(const AEntity: TObject);
    procedure ReleaseEntity(const AEntity: TObject);
    property OIDClass: TJCoreOPFOIDClass read FOIDClass write FOIDClass;
    property OIDGenerator: IJCoreOPFOIDGenerator read FOIDGenerator write FOIDGenerator;
    property OrderFieldName: string read FOrderFieldName write FOrderFieldName;
  end;

implementation

uses
  sysutils,
  JCoreConsts,
  JCoreUtils;

{ TJCoreOPFADM }

class function TJCoreOPFADM.IsGUIDEqual(const AGuid1, AGuid2: TGuid): Boolean;
begin
  Result := (AGuid1.D1 = AGuid2.D1) and
   (PDWORD(@AGuid1.D2)^ = PDWORD(@AGuid2.D2)^) and
   (PDWORD(@AGuid1.D4[0])^ = PDWORD(@AGuid2.D4[0])^) and
   (PDWORD(@AGuid1.D4[4])^ = PDWORD(@AGuid2.D4[4])^);
end;

class function TJCoreOPFADM.Implements(const ATypeInfo: PTypeInfo; const AClass: TClass): Boolean;
begin
  Result := (ATypeInfo^.Kind = tkClass) and (GetTypeData(ATypeInfo)^.ClassType.InheritsFrom(AClass));
end;

class function TJCoreOPFADM.Implements(const ATypeInfo: PTypeInfo; const AGUID: TGuid): Boolean;
begin
  Result := (ATypeInfo^.Kind = tkInterface) and IsGUIDEqual(GetTypeData(ATypeInfo)^.GUID, AGUID);
end;

procedure TJCoreOPFADM.InternalCommit;
begin
end;

procedure TJCoreOPFADM.InternalLoad;
begin
  raise EJCoreOPF.Create(2122, S2122_UnsupportedLoadOperation, [Metadata.PropInfo^.PropType^.Name]);
end;

constructor TJCoreOPFADM.Create(const APID: TJCoreOPFPID; const AMetadata: TJCoreOPFAttrMetadata);
begin
  inherited Create;
  FPID := APID;
  FCacheUpdated := False;
  FMetadata := AMetadata;
  FLoaded := False;
end;

procedure TJCoreOPFADM.Commit;
begin
  InternalUpdateCache;
  FCacheUpdated := True;
  InternalCommit;
end;

function TJCoreOPFADM.IsDirty: Boolean;
begin
  { TODO : Implement IsDirty cache while transaction is active }
  Result := not FCacheUpdated or InternalIsDirty;
end;

procedure TJCoreOPFADM.Load;
begin
  if not FLoaded then
  begin
    InternalLoad;
    FLoaded := True;
  end;
end;

{ TJCoreOPFADMValueType }

class function TJCoreOPFADMValueType.Apply(const AModel: TJCoreModel;
  const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := Implements(AAttrTypeInfo, Self);
end;

{ TJCoreOPFADMController }

constructor TJCoreOPFADMController.Create(const APID: TJCoreOPFPID; const AMetadata: TJCoreOPFAttrMetadata);
begin
  inherited Create(APID, AMetadata);
  FAttrPropInfo := AMetadata.PropInfo;
end;

procedure TJCoreOPFADMController.UpdateAttrAddr(const AAttrAddrRef: PPPointer);
begin
  // AAttrAddrRef^, which references ADM.AttrAddr address, will be updated
  // with the entity's attribute address by the Lazyload method.
  AAttrAddrRef^ := @FAttrAddr;
  InternalGetter;
  if not Assigned(FAttrAddr) then
    Metadata.NoLazyload;
end;

{ TJCoreOPFADMObject }

function TJCoreOPFADMObject.GetValue: TObject;
begin
  if Assigned(AttrAddr) then
    Result := TObject(AttrAddr^)
  else
    Result := UseGetter;
end;

procedure TJCoreOPFADMObject.SetValue(const AValue: TObject);
begin
  if Assigned(AttrAddr) then
    TObject(AttrAddr^) := AValue
  else
    UseSetter(AValue);
end;

function TJCoreOPFADMObject.UseGetter: TObject;
begin
  Result := GetObjectProp(PID.Entity, AttrPropInfo, nil);
end;

procedure TJCoreOPFADMObject.UseSetter(const AValue: TObject);
begin
  SetObjectProp(PID.Entity, AttrPropInfo, AValue);
end;

procedure TJCoreOPFADMObject.InternalGetter;
begin
  UseGetter;
end;

{ TJCoreOPFADMEntity }

function TJCoreOPFADMEntity.AcquirePID: TJCoreOPFPID;
var
  VObject: TObject;
begin
  VObject := Value;
  if Assigned(VObject) then
    Result := PID.PIDManager.AcquirePID(VObject)
  else
    Result := nil;
end;

function TJCoreOPFADMEntity.InternalIsDirty: Boolean;
var
  VPID: TJCoreOPFPID;
  VOID: IJCoreOPFOID;
begin
  VPID := AcquirePID;
  if Assigned(VPID) then
    VOID := VPID.OID
  else
    VOID := nil;
  Result := OIDCache <> VOID;
  if not Result and Assigned(VPID) and (Metadata.CompositionType = jctComposition) then
    Result := VPID.IsDirty;
end;

procedure TJCoreOPFADMEntity.InternalLoad;
begin
  // No OID means nil reference or non persistent entity. So no loading.
  if Assigned(CompositionOID) then
    PID.PIDManager.LoadEntity(PID, Self);
end;

procedure TJCoreOPFADMEntity.InternalUpdateCache;
var
  VPID: TJCoreOPFPID;
begin
  VPID := AcquirePID;
  if Assigned(VPID) then
    OIDCache := VPID.OID
  else
    OIDCache := nil;
end;

class function TJCoreOPFADMEntity.Apply(const AModel: TJCoreModel;
  const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := (AAttrTypeInfo^.Kind = tkClass) and AModel.IsEntityClass(GetTypeData(AAttrTypeInfo)^.ClassType);
end;

procedure TJCoreOPFADMEntity.AssignComposition(const AComposite: TObject);
begin
  SetObjectProp(PID.Entity, Metadata.PropInfo, AComposite);
end;

class function TJCoreOPFADMEntity.AttributeType: TJCoreOPFAttributeType;
begin
  Result := jatEntity;
end;

procedure TJCoreOPFADMEntity.ReadFromResultSet(const AResultSet: IJCoreOPFResultSet);
begin
  if not AResultSet.ReadNull then
    CompositionOID := Metadata.CompositionMetadata.OIDClass.CreateFromResultSet(AResultSet)
  else
    CompositionOID := nil;
end;

procedure TJCoreOPFADMEntity.WriteToParams(const AParams: IJCoreOPFParams);
var
  VPID: TJCoreOPFPID;
begin
  VPID := AcquirePID;
  if Assigned(VPID) then
  begin
    if not Assigned(VPID.OID) then
      VPID.PIDManager.StorePID(VPID);
    VPID.OID.WriteToParams(AParams);
  end else
    Metadata.CompositionMetadata.OIDClass.WriteNull(AParams);
end;

{ TJCoreOPFADMCollection }

function TJCoreOPFADMCollection.ArrayContentIsDirty: Boolean;
var
  VPIDArray: TJCoreOPFPIDArray;
  VPID: TJCoreOPFPID;
begin
  if Metadata.CompositionType = jctComposition then
  begin
    Result := True;
    VPIDArray := PIDArray;
    for VPID in VPIDArray do
      if VPID.IsDirty then
        Exit;
  end;
  Result := False;
end;

function TJCoreOPFADMCollection.ArrayLinkIsDirty: Boolean;
begin
  Result := (Length(PIDAdded) > 0) or (Length(OIDRemoved) > 0);
end;

function TJCoreOPFADMCollection.ArrayOrderIsDirty: Boolean;
begin
  Result := Length(PIDReorder) > 0;
end;

function TJCoreOPFADMCollection.ArraySizeIsDirty: Boolean;
begin
  Result := Length(OIDCache) <> Length(ItemsArray);
end;

function TJCoreOPFADMCollection.GetItemsArray: TJCoreObjectArray;
begin
  if not FItemsArrayUpdated then
  begin
    FItemsArray := InternalCreateItemsArray;
    { TODO : Update FItemsArrayUpdated flag within a complete (start/commit/rollback) transaction control }
    //FItemsArrayUpdated := True;
  end;
  Result := FItemsArray;
end;

function TJCoreOPFADMCollection.GetOIDRemoved: TJCoreOPFOIDArray;
begin
  UpdateChanges;
  Result := FOIDRemoved;
end;

function TJCoreOPFADMCollection.GetPIDAdded: TJCoreOPFPIDArray;
begin
  UpdateChanges;
  Result := FPIDAdded;
end;

function TJCoreOPFADMCollection.GetPIDArray: TJCoreOPFPIDArray;
begin
  if not FPIDArrayUpdated then
  begin
    FPIDArray := PID.PIDManager.CreatePIDArray(ItemsArray);
    { TODO : Update FPIDArrayUpdated flag within a complete (start/commit/rollback) transaction control }
    //FPIDArrayUpdated := True;
  end;
  Result := FPIDArray;
end;

function TJCoreOPFADMCollection.GetPIDReorder: TJCoreOPFPIDArray;
begin
  UpdateChanges;
  Result := FPIDReorder;
end;

function TJCoreOPFADMCollection.HasOIDInCache(const AOID: IJCoreOPFOID): Boolean;
var
  VOID: IJCoreOPFOID;
begin
  Result := True;
  for VOID in OIDCache do
    if AOID = VOID then
      Exit;
  Result := False;
end;

function TJCoreOPFADMCollection.HasOIDInCollection(
  const APIDArray: TJCoreOPFPIDArray; const AOID: IJCoreOPFOID): Boolean;
var
  VPID: TJCoreOPFPID;
begin
  Result := True;
  for VPID in APIDArray do
    if VPID.OID = AOID then
      Exit;
  Result := False;
end;

procedure TJCoreOPFADMCollection.UpdateChanges;
var
  VPIDArray: TJCoreOPFPIDArray;
  VPIDAddedArray: TJCoreOPFPIDArray;
  VOIDRemovedArray: TJCoreOPFOIDArray;
  VPID: TJCoreOPFPID;
  I, VMinSize, VMaxSize, VTmpSize, VPIDSize, VOIDSize: Integer;
  VAddedCount, VRemovedCount, VSequenceCount, VMaxSequence: Integer;
begin
  { TODO : Reduce npath }
  if FChangesUpdated then
    Exit;

  // Initializing vars
  VPIDArray := PIDArray;
  VPIDSize := Length(VPIDArray);
  VOIDSize := Length(OIDCache);
  VMinSize := VPIDSize;
  if VOIDSize < VMinSize then
  begin
    VMinSize := VOIDSize;
    VMaxSize := VPIDSize;
  end else
    VMaxSize := VOIDSize;

  // populating temp arrays
  SetLength(VPIDAddedArray, VMaxSize);
  SetLength(VOIDRemovedArray, VMaxSize);
  VTmpSize := 0;
  for I := 0 to Pred(VMinSize) do
  begin
    if VPIDArray[I].OID <> OIDCache[I] then
    begin
      VPIDAddedArray[VTmpSize] := VPIDArray[I];
      VOIDRemovedArray[VTmpSize] := OIDCache[I];
      Inc(VTmpSize);
    end;
  end;
  for I := VMinSize to Pred(VPIDSize) do
  begin
    VPIDAddedArray[VTmpSize] := VPIDArray[I];
    VOIDRemovedArray[VTmpSize] := nil;
    Inc(VTmpSize);
  end;
  for I := VMinSize to Pred(VOIDSize) do
  begin
    VPIDAddedArray[VTmpSize] := nil;
    VOIDRemovedArray[VTmpSize] := OIDCache[I];
    Inc(VTmpSize);
  end;

  // populating added/removed arrays
  SetLength(FPIDAdded, VTmpSize);
  SetLength(FOIDRemoved, VTmpSize);
  VAddedCount := 0;
  VRemovedCount := 0;
  for I := 0 to Pred(VTmpSize) do
  begin
    if Assigned(VPIDAddedArray[I]) and not HasOIDInCache(VPIDAddedArray[I].OID) then
    begin
      FPIDAdded[VAddedCount] := VPIDAddedArray[I];
      Inc(VAddedCount);
    end;
    if Assigned(VOIDRemovedArray[I]) and not HasOIDInCollection(VPIDArray, VOIDRemovedArray[I]) then
    begin
      FOIDRemoved[VRemovedCount] := VOIDRemovedArray[I];
      Inc(VRemovedCount);
    end;
  end;
  SetLength(FPIDAdded, VAddedCount);
  SetLength(FOIDRemoved, VRemovedCount);

  // populating array of PIDs out of order
  if Metadata.OrderFieldName <> '' then
  begin
    SetLength(FPIDReorder, VPIDSize);
    VMaxSequence := 0;
    VSequenceCount := 0;
    for I := Low(VPIDArray) to High(VPIDArray) do
    begin
      VPID := VPIDArray[I];
      if VPID.SequenceCache <= VMaxSequence then
      begin
        { TODO : Implement subtract former when possible }
        Inc(VMaxSequence);
        VPID.Sequence := VMaxSequence;
        if HasOIDInCache(VPID.OID) then
        begin
          FPIDReorder[VSequenceCount] := VPID;
          Inc(VSequenceCount);
        end;
      end else
        VMaxSequence := VPID.Sequence;
    end;
    SetLength(FPIDReorder, VSequenceCount);
  end else
    SetLength(FPIDReorder, 0);

  FChangesUpdated := True;
end;

procedure TJCoreOPFADMCollection.InternalCommit;
var
  VPIDArray: TJCoreOPFPIDArray;
  VPID: TJCoreOPFPID;
begin
  inherited;
  VPIDArray := PIDArray;
  // Current approach of the sequence storage is assign it to the PID
  // Advantage: *very* simpler interface when managing it's values as well as identifying PIDs
  //   out of order.
  // Disadvantages: 1. should avoid share the same PID with more than one collection (aggregation
  //   collections) on very concurrent environments because of the shared Sequence(Cache) field; and
  //   2. the scope violation below
  { TODO : proper storage of the collection sequence }
  for VPID in VPIDArray do
    VPID.FSequenceCache := VPID.FSequence; // friend class
  FChangesUpdated := False;
  FItemsArrayUpdated := False;
  FPIDArrayUpdated := False;
end;

function TJCoreOPFADMCollection.InternalIsDirty: Boolean;
begin
  Result := ArraySizeIsDirty or ArrayLinkIsDirty or ArrayOrderIsDirty or ArrayContentIsDirty;
  // Need to control FChangesUpdated flag on every single public boundary
  // because the ADM doesn't control changes made in the list.
  // This assignment is just a workaround.
  { TODO : Update FChangesUpdated flag within a complete (start/commit/rollback) transaction control }
  FChangesUpdated := False;
end;

procedure TJCoreOPFADMCollection.InternalLoad;
begin
  PID.PIDManager.LoadCollection(PID, Self);
end;

procedure TJCoreOPFADMCollection.InternalUpdateCache;
var
  VPIDArray: TJCoreOPFPIDArray;
  VOIDCache: TJCoreOPFOIDArray;
  VOID: IJCoreOPFOID;
  I, J: Integer;
begin
  VPIDArray := PIDArray;
  SetLength(VOIDCache, Length(VPIDArray));
  J := 0;
  for I := Low(VPIDArray) to High(VPIDArray) do
  begin
    VOID := VPIDArray[I].OID;
    if Assigned(VOID) then
    begin
      VOIDCache[J] := VOID;
      Inc(J);
    end;
  end;
  SetLength(VOIDCache, J);
  OIDCache := VOIDCache;
end;

procedure TJCoreOPFADMCollection.AssignArray(const AArray: TJCoreObjectArray);
begin
  InternalAssignArray(AArray);
end;

class function TJCoreOPFADMCollection.AttributeType: TJCoreOPFAttributeType;
begin
  Result := jatCollection;
end;

function TJCoreOPFADMCollection.OrderIsDirty: Boolean;
begin
  Result := ArrayOrderIsDirty;
end;

procedure TJCoreOPFADMCollection.ReadFromResultSet(const AResultSet: IJCoreOPFResultSet);
begin
  if not Metadata.HasLazyload then
    Load;
end;

procedure TJCoreOPFADMCollection.WriteToParams(const AParams: IJCoreOPFParams);
begin
  // Mapping does the job using OIDRemoved and PIDAdded properties
end;

{ TJCoreOPFADMMapping }

function TJCoreOPFADMMapping.BuildChangedADMArray(
  const AAttributeType: TJCoreOPFAttributeTypeSet): TJCoreOPFADMArray;
var
  VADM: TJCoreOPFADM;
  VCount, I: Integer;
begin
  SetLength(Result, Count);
  VCount := 0;
  for I := 0 to Pred(Count) do
  begin
    VADM := Data[I];
    if (VADM.AttributeType in AAttributeType) and VADM.IsDirty then
    begin
      Result[VCount] := VADM;
      Inc(VCount);
    end;
  end;
  SetLength(Result, VCount);
end;

function TJCoreOPFADMMapping.GetADM(const AIndex: Integer): TJCoreOPFADM;
begin
  Result := Data[AIndex];
end;

function TJCoreOPFADMMapping.GetADMAttributeChanged: TJCoreOPFADMArray;
begin
  if not Assigned(FADMAttributeChanged) then
    FADMAttributeChanged := BuildChangedADMArray([jatSimple, jatEntity]);
  Result := FADMAttributeChanged;
end;

function TJCoreOPFADMMapping.GetADMByName(const AAttributeName: string): TJCoreOPFADM;
var
  VIndex: Integer;
begin
  VIndex := IndexOf(AAttributeName);
  if VIndex = -1 then
    raise EJCoreMetadata.Create(501, S0501_AttributeNotFound, [PID.Entity.ClassName, AAttributeName]);
  Result := Data[VIndex];
end;

function TJCoreOPFADMMapping.GetADMCollectionChanged: TJCoreOPFADMArray;
begin
  if not Assigned(FADMCollectionChanged) then
    FADMCollectionChanged := BuildChangedADMArray([jatCollection]);
  Result := FADMCollectionChanged;
end;

function TJCoreOPFADMMapping.InternalIsDirty(const AIncludeCollections: Boolean): Boolean;
var
  VADM: TJCoreOPFADM;
  I: Integer;
begin
  Result := True;
  for I := 0 to Pred(Count) do
  begin
    VADM := Data[I];
    if VADM.Metadata.AttributeType = jatCollection then
    begin
      if AIncludeCollections and VADM.IsDirty then
        Exit;
    end else if VADM.IsDirty then
      Exit;
  end;
  Result := False;
end;

constructor TJCoreOPFADMMapping.Create(const APID: TJCoreOPFPID);
begin
  inherited Create;
  // do not own ADMs; PID.ADMMap will do.
  FPID := APID;
end;

function TJCoreOPFADMMapping.AcquireADM(const AAttributeName: string): TJCoreOPFADM;
begin
  Result := ADMByName[AAttributeName];
end;

procedure TJCoreOPFADMMapping.Commit;
begin
  SetLength(FADMAttributeChanged, 0);
  SetLength(FADMCollectionChanged, 0);
end;

function TJCoreOPFADMMapping.IsAttributesDirty: Boolean;
begin
  Result := InternalIsDirty(False);
end;

function TJCoreOPFADMMapping.IsDirty: Boolean;
begin
  Result := InternalIsDirty(True);
end;

{ TJCoreOPFPID }

function TJCoreOPFPID.CreateADM(const AAttrMetadata: TJCoreOPFAttrMetadata): TJCoreOPFADM;
var
  VADM: TJCoreOPFADM;
  VTypeInfo: PTypeInfo;
  VTypeData: PTypeData;
  VIntf: IInterface;
begin
  VADM := AAttrMetadata.CreateADM(Self);
  ADMMap.Add(AAttrMetadata.Name, VADM);
  if AAttrMetadata.HasLazyload and (VADM is TJCoreOPFADMController) then
  begin
    try
      { TODO : Abstract }
      TJCoreOPFADMController(VADM).UpdateAttrAddr(@FAttrAddrRef);
    finally
      FAttrAddrRef := nil;
    end;
  end;
  // Assign ValueType reference
  if VADM is TJCoreOPFADMValueType then
  begin
    VTypeInfo := AAttrMetadata.PropInfo^.PropType;
    VTypeData := GetTypeData(VTypeInfo);
    case VTypeInfo^.Kind of
      tkClass:
        if VTypeData^.ClassType.InheritsFrom(TJCoreOPFADM) then
          SetObjectProp(Entity, AAttrMetadata.PropInfo, VADM);
      tkInterface:
      begin
        VIntf := nil;
        VADM.QueryInterface(VTypeData^.GUID, VIntf);
        if Assigned(VIntf) then
          SetInterfaceProp(Entity, AAttrMetadata.PropInfo, VIntf);
      end;
    end;
  end;
  Result := VADM;
end;

procedure TJCoreOPFPID.CreateADMs(const AMaps: TJCoreOPFMaps);
var
  VAttrMetadata: TJCoreOPFAttrMetadata;
  VADMMapping: TJCoreOPFADMMapping;
  VMap: TJCoreOPFMap;
begin
  for VMap in AMaps do
  begin
    VADMMapping := TJCoreOPFADMMapping.Create(Self);
    ADMMappingMap.Add(VMap, VADMMapping);
    for VAttrMetadata in VMap do
      VADMMapping.Add(VAttrMetadata.Name, CreateADM(VAttrMetadata));
  end;
end;

function TJCoreOPFPID.GetADMMapping(const AIndex: Integer): TJCoreOPFADMMapping;
begin
  Result := ADMMappingMap.Data[AIndex];
end;

function TJCoreOPFPID.GetPIDManager: IJCoreOPFPIDManager;
begin
  { TODO : Fail if nil }
  Result := FPIDManager;
end;

function TJCoreOPFPID.IGetEntity: TObject;
begin
  Result := FEntity;
end;

function TJCoreOPFPID.IGetIsPersistent: Boolean;
begin
  Result := FIsPersistent;
end;

function TJCoreOPFPID.IGetOID: IJCoreOID;
begin
  Result := FOID;
end;

function TJCoreOPFPID.IGetOwner: IJCorePID;
begin
  Result := FOwner;
end;

procedure TJCoreOPFPID.SetOID(const AValue: IJCoreOPFOID);
var
  VOIDProp: TJCorePropInfoArray;
begin
  if IsPersistent and Assigned(AValue) then
    raise EJCoreOPF.Create(2119, S2119_CannotAssignOIDPersistent, []);
  FOID := AValue;
  VOIDProp := Metadata.OIDProp;
  if Length(VOIDProp) > 0 then
  begin
    if Assigned(FOID) then
      FOID.WriteToOIDProp(Entity, VOIDProp)
    else
      Metadata.OIDClass.ClearProp(Entity, VOIDProp);
  end;
end;

function TJCoreOPFPID.AcquireADMByAttrAddr(const AAttrAddr: Pointer): TJCoreOPFADM;
var
  I: Integer;
begin
  for I := 0 to Pred(ADMMap.Count) do
  begin
    Result := ADMMap.Data[I];
    { TODO : Abstract }
    if Result is TJCoreOPFADMController and (TJCoreOPFADMController(Result).AttrAddr = AAttrAddr) then
      Exit;
  end;
  raise EJCoreMetadata.Create(501, S0501_AttributeNotFound, [Entity.ClassName, HexStr(AAttrAddr)]);
end;

constructor TJCoreOPFPID.Create(const AEntity: TObject; const AMetadata: TJCoreOPFClassMetadata);
begin
  if not Assigned(AEntity) then
    raise EJCoreNilPointer.Create;
  inherited Create;
  FEntity := AEntity;
  FMetadata := AMetadata;
  FIsPersistent := False;
  FADMMap := TJCoreOPFADMMap.Create;
  FADMMappingMap := TJCoreOPFADMMappingMap.Create;
  CreateADMs(AMetadata.Maps);
end;

destructor TJCoreOPFPID.Destroy;
var
  I: Integer;
begin
  for I := 0 to Pred(FADMMap.Count) do
    FADMMap.Data[I].Free;
  for I := 0 to Pred(FADMMappingMap.Count) do
    FADMMappingMap.Data[I].Free;
  FreeAndNil(FADMMappingMap);
  FreeAndNil(FADMMap);
  inherited Destroy;
end;

function TJCoreOPFPID.AcquireADM(const AAttributeName: string): TJCoreOPFADM;
var
  VIndex: Integer;
begin
  VIndex := ADMMap.IndexOf(AAttributeName);
  if VIndex = -1 then
    raise EJCoreMetadata.Create(501, S0501_AttributeNotFound, [Entity.ClassName, AAttributeName]);
  Result := ADMMap.Data[VIndex];
end;

function TJCoreOPFPID.ADMByName(const AAttributeName: string): IJCoreADM;
begin
  Result := AcquireADM(AAttributeName) as IJCoreADM;
end;

function TJCoreOPFPID.ADMMappingCount: Integer;
begin
  Result := ADMMappingMap.Count;
end;

procedure TJCoreOPFPID.AssignOwner(const AOwner: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMCollection);
begin
  if AOwnerADM.Metadata.CompositionType = jctComposition then
  begin
    if not Assigned(AOwner) then
    begin
      FOwner := nil;
      FOwnerADM := nil;
    end else if not Assigned(FOwner) then
    begin
      { TODO : Check circular reference }
      FOwner := AOwner;
      FOwnerADM := AOwnerADM;
    end else if (AOwner <> FOwner) or (AOwnerADM <> FOwnerADM) then
      { TODO : Check duplication in the same admcollection }
      raise EJCoreOPF.Create(2120, S2120_ObjectAlreadyOwned, [FOwner.Entity.ClassName, Entity.ClassName]);
  end;
end;

procedure TJCoreOPFPID.AttachManager(const APIDManager: IJCoreOPFPIDManager);
begin
  if not Assigned(FPIDManager) then
    FPIDManager := APIDManager
  else if FPIDManager <> APIDManager then
    { TODO : Fail }
end;

procedure TJCoreOPFPID.Commit;
var
  I: Integer;
begin
  FIsPersistent := Assigned(FOID);
  for I := 0 to Pred(ADMMap.Count) do
    ADMMap.Data[I].Commit;
  for I := 0 to Pred(ADMMappingMap.Count) do
    ADMMappingMap.Data[I].Commit;
end;

function TJCoreOPFPID.IsDirty: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Pred(ADMMappingMap.Count) do
    if ADMMappingMap.Data[I].IsDirty then
      Exit;
  Result := False;
end;

function TJCoreOPFPID.Lazyload(const AAttrAddr: Pointer): Boolean;
begin
  // Return False only if an attribute *should* but *couldn't* be loaded
  // This will also return True if a composition references nil. The entity
  // should use this result and decide if an instance should be created
  if not Assigned(FAttrAddrRef) then
  begin
    // Normal execution
    Result := Assigned(AAttrAddr) and Assigned(TObject(AAttrAddr^));
    if not Result then
    begin
      AcquireADMByAttrAddr(AAttrAddr).Load;
      Result := Assigned(TObject(AAttrAddr^));
    end;
  end else
  begin
    // PID initialization, saving attribute address
    FAttrAddrRef^ := AAttrAddr;
    Result := True;
  end;
end;

procedure TJCoreOPFPID.ReadSequenceField(const AResultSet: IJCoreOPFResultSet);
begin
  FSequence := AResultSet.ReadInt32;
  FSequenceCache := FSequence;
end;

procedure TJCoreOPFPID.WriteSequenceField(const AParams: IJCoreOPFParams);
begin
  AParams.WriteInt32(FSequence);
end;

{ TJCoreOPFAttrMetadata }

function TJCoreOPFAttrMetadata.GetCompositionMetadata: TJCoreOPFClassMetadata;
begin
  Result := inherited CompositionMetadata as TJCoreOPFClassMetadata;
end;

function TJCoreOPFAttrMetadata.GetHasExternalLink: Boolean;
begin
  Result := (CompositionType = jctAggregation) and (AttributeType = jatCollection);
end;

function TJCoreOPFAttrMetadata.GetModel: TJCoreOPFModel;
begin
  Result := inherited Model as TJCoreOPFModel;
end;

function TJCoreOPFAttrMetadata.ReadComposition(const ATypeInfo: PTypeInfo): TClass;
var
  VClass: TClass;
begin
  VClass := GetTypeData(ATypeInfo)^.ClassType;
  if Model.IsEntityClass(VClass) then
    Result := VClass
  else
    Result := Model.FindSpecializedClass(VClass);
  if not Assigned(Result) then
    raise EJCoreOPF.Create(2121, S2121_UnsupportedAttributeType, [ATypeInfo^.Name]);
end;

constructor TJCoreOPFAttrMetadata.Create(const AModel: TJCoreModel; const AOwner: TJCoreClassMetadata;
  const APropInfo: PPropInfo);
var
  VOwner: TJCoreOPFClassMetadata;
begin
  inherited Create(AModel, AOwner, APropInfo);
  FADMClass := Model.AcquireADMClass(PropInfo^.PropType);
  FAttributeType := ADMClass.AttributeType;
  FIsPersistent := True;
  FPersistentFieldName := UpperCase(Name);
  if AttributeType in [jatEntity, jatCollection] then
  begin
    CompositionClass := ReadComposition(APropInfo^.PropType);
    VOwner := Owner as TJCoreOPFClassMetadata;
    if HasExternalLink then
      FExternalLinkTableName := VOwner.TableName + '_' + PersistentFieldName
    else
      FExternalLinkTableName := CompositionMetadata.TableName;
    FExternalLinkLeftFieldName := VOwner.TableName;
    if VOwner <> CompositionMetadata then
      FExternalLinkRightFieldName := CompositionMetadata.TableName
    else
      FExternalLinkRightFieldName := ExternalLinkTableName;
    FOrderFieldName := Model.OrderFieldName;
  end else
    CompositionClass := nil;
  FHasLazyload := True; // which also means "I dont know yet, try it"
  Changed;
end;

function TJCoreOPFAttrMetadata.CreateADM(const APID: TJCoreOPFPID): TJCoreOPFADM;
begin
  Result := ADMClass.Create(APID, Self);
end;

function TJCoreOPFAttrMetadata.IsCollection: Boolean;
begin
  Result := AttributeType = jatCollection;
end;

procedure TJCoreOPFAttrMetadata.NoLazyload;
begin
  FHasLazyload := False;
end;

{ TJCoreOPFMap }

constructor TJCoreOPFMap.Create(const AMetadata: TJCoreOPFClassMetadata; const AIsRootMap: Boolean);
var
  VOwnerAttr: TJCoreOPFAttrMetadata;
  VOwnerClass: TJCoreOPFClassMetadata;
  VTableName: string;
  VOIDName: TJCoreStringArray;
  I: Integer;
begin
  { TODO : Proper metadata -> map conversion }
  inherited Create(False);
  FMetadata := AMetadata;
  FHasInsertOIDFields := not AIsRootMap or not Metadata.OIDGenerator.IsPostInsertGenerator;
  FOIDClass := Metadata.OIDClass;
  FOIDGenerator := Metadata.OIDGenerator;
  FOIDName := Metadata.OIDName;
  FTableName := Metadata.TableName;
  VOwnerAttr := Metadata.OwnerAttr as TJCoreOPFAttrMetadata;
  VOwnerClass := Metadata.OwnerClass as TJCoreOPFClassMetadata;
  if Assigned(VOwnerAttr) and Assigned(VOwnerClass) then
  begin
    VTableName := VOwnerClass.TableName;
    VOIDName := VOwnerClass.OIDName;
    SetLength(FOwnerOIDName, Length(VOIDName));
    if Length(VOIDName) = 1 then
      FOwnerOIDName[0] := VTableName
    else
      for I := 0 to Pred(Length(VOIDName)) do
        FOwnerOIDName[I] := VTableName + '_' + VOIDName[I];
    FOrderFieldName := VOwnerAttr.OrderFieldName;
  end else
    SetLength(FOwnerOIDName, 0);
  FSubMaps := Metadata.SubMaps;
  SetLength(FSubClasses, FSubMaps.Count);
  for I := Low(FSubClasses) to High(FSubClasses) do
    FSubClasses[I] := FSubMaps[I].Metadata.TheClass;
end;

function TJCoreOPFMap.HasOrderField: Boolean;
begin
  Result := FOrderFieldName <> '';
end;

function TJCoreOPFMap.HasOwnerOID: Boolean;
begin
  Result := Length(FOwnerOIDName) > 0;
end;

{ TJCoreOPFClassMetadata }

procedure TJCoreOPFClassMetadata.BuildOIDProperties;
var
  VOIDProp: TJCorePropInfoArray;
  VOIDClass: TJCoreOPFOIDClass;
  VFound: Boolean;
  I: Integer;
begin
  if Assigned(Parent) and (Length(Parent.OIDName) > 0) then
  begin
    // Parent has valid OID
    if Length(FOIDName) > 0 then
      raise EJCoreOPF.Create(
       2135, S2135_CannotOverrideOID, [Parent.TheClass.ClassName, TheClass.ClassName]);
    FOIDName := Parent.OIDName
  end else if (Length(FOIDName) = 0) and IsPersistent then
  begin
    // No OID here neither on parent and this class is
    // persistent. Creating a default OID
    SetLength(FOIDName, 1);
    FOIDName[0] := SJCoreID;
  end;
  SetLength(VOIDProp, Length(FOIDName));
  VFound := Length(VOIDProp) > 0;
  for I := Low(VOIDProp) to High(VOIDProp) do
  begin
    { TODO : proper OID name <-> property convertion, see AddAttribute() }
    VOIDProp[I] := GetPropInfo(TheClass.ClassInfo, '_' + FOIDName[I]);
    if not Assigned(VOIDProp[I]) then
      VFound := False;
  end;
  if VFound then
  begin
    if Assigned(FOIDClass) and FOIDClass.Apply(VOIDProp) then
      VOIDClass := FOIDClass
    else
      VOIDClass := Model.FindOIDClass(VOIDProp);
    if not Assigned(VOIDClass) then
      raise EJCoreOPF.Create(2133, S2133_UnsupportedOID, [TheClass.ClassName]);
  end else
  begin
    VOIDClass := Model.OIDClass;
    SetLength(VOIDProp, 0);
  end;
  FOIDProp := VOIDProp;
  FOIDClass := VOIDClass;
  FOIDGenerator := Model.OIDGenerator;
end;

function TJCoreOPFClassMetadata.GetAttributes(const AIndex: Integer): TJCoreOPFAttrMetadata;
begin
  Result := inherited Attributes[AIndex] as TJCoreOPFAttrMetadata;
end;

function TJCoreOPFClassMetadata.GetMaps: TJCoreOPFMaps;
begin
  if not Assigned(FMaps) then
    FMaps := Model.CreateMaps(Self);
  Result := FMaps;
end;

function TJCoreOPFClassMetadata.GetModel: TJCoreOPFModel;
begin
  Result := inherited Model as TJCoreOPFModel;
end;

function TJCoreOPFClassMetadata.GetParent: TJCoreOPFClassMetadata;
begin
  Result := inherited Parent as TJCoreOPFClassMetadata;
end;

function TJCoreOPFClassMetadata.GetSubMaps: TJCoreOPFMaps;
begin
  if not Assigned(FSubMaps) then
    FSubMaps := Model.CreateSubMaps(Self);
  Result := FSubMaps;
end;

procedure TJCoreOPFClassMetadata.SetOIDClass(const AValue: TJCoreOPFOIDClass);
begin
  if Assigned(AValue) and (Length(OIDProp) > 0) and not AValue.Apply(OIDProp) then
    raise EJCoreOPF.Create(2132, S2132_InvalidOIDClass, [TheClass.ClassName, AValue.ClassName]);
  FOIDClass := AValue;
end;

procedure TJCoreOPFClassMetadata.AddAttribute(const APropInfo: PPropInfo);
var
  VAttrName: ShortString;
begin
  VAttrName := APropInfo^.Name;
  if not SameText(SJCorePID, VAttrName) then
  begin
    if VAttrName[1] = '_' then
    begin
      SetLength(FOIDName, Length(FOIDName) + 1);
      FOIDName[High(FOIDName)] := Copy(VAttrName, 2, MaxInt);
    end else
      inherited AddAttribute(APropInfo);
  end;
end;

function TJCoreOPFClassMetadata.IsPersistent: Boolean;
begin
  { TODO : proper is persistent check - note that this method
    is being called inside the class metadata constructor }
  Result := AttributeCount > 0;
end;

constructor TJCoreOPFClassMetadata.Create(const AModel: TJCoreModel; const AClass: TClass;
  const AParent: TJCoreClassMetadata);
begin
  FTableName := UpperCase(Copy(AClass.ClassName, 2, MaxInt));
  inherited Create(AModel, AClass, AParent);
  BuildOIDProperties;
end;

destructor TJCoreOPFClassMetadata.Destroy;
begin
  FreeAndNil(FMaps);
  FreeAndNil(FSubMaps);
  inherited Destroy;
end;

function TJCoreOPFClassMetadata.AttributeByName(
  const AAttributeName: string): TJCoreOPFAttrMetadata;
begin
  Result := inherited AttributeByName(AAttributeName) as TJCoreOPFAttrMetadata;
end;

function TJCoreOPFClassMetadata.FindAttribute(const AAttributeName: string): TJCoreOPFAttrMetadata;
begin
  Result := inherited FindAttribute(AAttributeName) as TJCoreOPFAttrMetadata;
end;

{ TJCoreOPFModel }

procedure TJCoreOPFModel.AcquireMaps(const AMetadata: TJCoreOPFClassMetadata; const AMaps: TJCoreOPFMaps);
begin
  if Assigned(AMetadata) then
  begin
    AcquireMaps(AMetadata.Parent, AMaps);
    if AMetadata.AttributeCount > 0 then
      AMaps.Add(AcquireMap(AMetadata));
  end;
end;

function TJCoreOPFModel.AcquirePIDFromCache(const AEntity: TObject): TJCoreOPFPID;
var
  VMetadata: TJCoreOPFClassMetadata;
  VIndex: Integer;
begin
  VIndex := PIDCache.IndexOf(AEntity);
  if VIndex >= 0 then
  begin
    Result := PIDCache.Data[VIndex];
  end else
  begin
    VMetadata := AcquireMetadata(AEntity.ClassType);
    if Length(VMetadata.OIDProp) > 0 then
    begin
      Result := PIDClass.Create(AEntity, VMetadata);
      PIDCache.Add(AEntity, Result);
    end;
  end;
end;

function TJCoreOPFModel.AcquirePIDFromIntfProp(const AEntity: TObject): TJCoreOPFPID;
var
  VPropInfo: PPropInfo;
begin
  VPropInfo := GetPropInfo(AEntity, SJCorePID);
  if Assigned(VPropInfo) and (VPropInfo^.PropType^.Kind = tkInterface) then
  begin
    Result := GetInterfaceProp(AEntity, VPropInfo) as TJCoreOPFPID;
    if not Assigned(Result) then
    begin
      Result := PIDClass.Create(AEntity, AcquireMetadata(AEntity.ClassType));
      SetInterfaceProp(AEntity, VPropInfo, Result as IJCorePID);
    end;
  end else
    Result := nil;
end;

function TJCoreOPFModel.AcquirePIDFromProxyField(const AEntity: TObject): TJCoreOPFPID;
var
  VFieldAddr: Pointer;
  VProxy: TJCoreEntityProxy;
  VMetadata: TJCoreOPFClassMetadata;
begin
  VFieldAddr := AEntity.FieldAddress(SJCoreProxy);
  if Assigned(VFieldAddr) then
  begin
    VProxy := TObject(VFieldAddr^) as TJCoreEntityProxy;
    if not Assigned(VProxy) then
    begin
      VMetadata := AcquireMetadata(AEntity.ClassType);
      Result := TJCoreOPFPID(PIDClass.NewInstance);
      VProxy := CreateProxy(Result);
      TJCoreEntityProxy(VFieldAddr^) := VProxy;
      Result.Create(AEntity, VMetadata);
    end else
      Result := VProxy.PID as TJCoreOPFPID;
  end else
    Result := nil;
end;

procedure TJCoreOPFModel.AcquireSubMaps(const AMetadata: TJCoreOPFClassMetadata; const AMaps: TJCoreOPFMaps);
var
  VClass: TClass;
  VOPFMetadata: TJCoreOPFClassMetadata;
  I: Integer;
begin
  for I := 0 to Pred(ClassMap.Count) do
  begin
    VClass := ClassMap.Data[I];
    if VClass.ClassParent = AMetadata.TheClass then
    begin
      VOPFMetadata := AcquireMetadata(VClass);
      AMaps.Add(AcquireMap(VOPFMetadata));
      AcquireSubMaps(VOPFMetadata, AMaps);
    end;
  end;
end;

function TJCoreOPFModel.AcquireMap(const AMetadata: TJCoreOPFClassMetadata): TJCoreOPFMap;
var
  VName: string;
  VMap: TJCoreOPFMap;
  VIndex: Integer;
  I: Integer;
begin
  VName := AMetadata.TheClass.ClassName;
  VIndex := MapMap.IndexOf(VName);
  if VIndex = -1 then
  begin
    VMap := TJCoreOPFMap.Create(AMetadata, MapMap.Count = 0);
    VIndex := MapMap.Add(VName, VMap);
    for I := 0 to Pred(AMetadata.AttributeCount) do
      if AMetadata[I].IsPersistent then
        VMap.Add(AMetadata[I]);
  end;
  Result := MapMap.Data[VIndex];
end;

function TJCoreOPFModel.AttributeMetadataClass: TJCoreAttrMetadataClass;
begin
  Result := TJCoreOPFAttrMetadata;
end;

function TJCoreOPFModel.ClassMetadataClass: TJCoreClassMetadataClass;
begin
  Result := TJCoreOPFClassMetadata;
end;

function TJCoreOPFModel.CreateProxy(const APID: TJCoreOPFPID): TJCoreEntityProxy;
begin
  Result := TJCoreEntityProxy.Create(APID);
end;

procedure TJCoreOPFModel.Finit;
var
  I: Integer;
begin
  FreeAndNil(FADMClassList);
  FreeAndNil(FOIDClassList);
  for I := 0 to Pred(FMapMap.Count) do
    FMapMap.Data[I].Free;
  FreeAndNil(FMapMap);
  for I := 0 to Pred(FPIDCache.Count) do
  begin
    { TODO : Move leaking analyzer to session }
    LOG.Warn(JCoreFormatMessage(
     2801, Format(S2801_PIDLeaking, [FPIDCache.Data[I].Metadata.TheClass.ClassName])));
    FPIDCache.Data[I].Free;
  end;
  FreeAndNil(FPIDCache);
  inherited Finit;
end;

function TJCoreOPFModel.PIDClass: TJCoreOPFPIDClass;
begin
  Result := TJCoreOPFPID;
end;

constructor TJCoreOPFModel.Create;
begin
  FADMClassList := TJCoreOPFADMClassList.Create;
  FOIDClassList := TJCoreOPFOIDClassList.Create;
  inherited Create;
  FMapMap := TJCoreOPFMapMap.Create;
  FPIDCache := TJCoreOPFPIDMap.Create;
  FOIDClass := TJCoreOPFOIDString;
  FOIDGenerator := TJCoreOPFOIDGeneratorGUID.Create;
end;

function TJCoreOPFModel.AcquireADMClass(const AAttrTypeInfo: PTypeInfo): TJCoreOPFADMClass;
begin
  for Result in ADMClassList do
    if Result.Apply(Self, AAttrTypeInfo) then
      Exit;
  raise EJCoreOPF.Create(2121, S2121_UnsupportedAttributeType, [AAttrTypeInfo^.Name]);
end;

function TJCoreOPFModel.AcquireAttrMetadata(const AClass: TClass;
  const AAttributeName: string): TJCoreOPFAttrMetadata;
begin
  Result := AcquireMetadata(AClass).AttributeByName(AAttributeName);
end;

function TJCoreOPFModel.AcquireMetadata(const AClass: TClass): TJCoreOPFClassMetadata;
begin
  Result := inherited AcquireMetadata(AClass) as TJCoreOPFClassMetadata;
end;

function TJCoreOPFModel.AcquirePID(const AEntity: TObject): TJCoreOPFPID;
begin
  if not Assigned(AEntity) then
    raise EJCoreNilPointer.Create;
    Result := AcquirePIDFromIntfProp(AEntity);
  if not Assigned(Result) then
    Result := AcquirePIDFromProxyField(AEntity);
  if not Assigned(Result) then
    Result := AcquirePIDFromCache(AEntity);
  if not Assigned(Result) then
    raise EJCoreOPF.Create(2108, S2108_PersistentIDNotFound, [AEntity.ClassName]);
end;

procedure TJCoreOPFModel.AddADMClass(const AADMClassArray: array of TJCoreOPFADMClass);
var
  VADMClass: TJCoreOPFADMClass;
begin
  for VADMClass in AADMClassArray do
    ADMClassList.Add(VADMClass);
end;

procedure TJCoreOPFModel.AddOIDClass(const AOIDClassArray: array of TJCoreOPFOIDClass);
var
  VOIDClass: TJCoreOPFOIDClass;
begin
  for VOIDClass in AOIDClassArray do
    OIDClassList.Add(VOIDClass);
end;

function TJCoreOPFModel.CreateMaps(const AMetadata: TJCoreOPFClassMetadata): TJCoreOPFMaps;
begin
  Result := TJCoreOPFMaps.Create(False);
  try
    AcquireMaps(AMetadata, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TJCoreOPFModel.CreateSubMaps(const AMetadata: TJCoreOPFClassMetadata): TJCoreOPFMaps;
begin
  Result := TJCoreOPFMaps.Create(False);
  try
    AcquireSubMaps(AMetadata, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TJCoreOPFModel.FindOIDClass(const AOIDProp: TJCorePropInfoArray): TJCoreOPFOIDClass;
var
  I: Integer;
begin
  for I := 0 to Pred(FOIDClassList.Count) do
  begin
    { TODO : use a proper factory }
    Result := FOIDClassList[I];
    if Result.Apply(AOIDProp) then
      Exit;
  end;
  Result := nil;
end;

procedure TJCoreOPFModel.InitEntity(const AEntity: TObject);
begin
  AcquirePID(AEntity);
end;

procedure TJCoreOPFModel.ReleaseEntity(const AEntity: TObject);
var
  VIndex: Integer;
begin
  VIndex := PIDCache.IndexOf(AEntity);
  if VIndex >= 0 then
  begin
    PIDCache.Data[VIndex].Free;
    PIDCache.Delete(VIndex);
  end;
end;

end.

