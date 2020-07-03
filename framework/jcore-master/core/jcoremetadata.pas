(*
  JCore, Metadata Classes
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreMetadata;

{$I jcore.inc}
{$WARN 5024 OFF} // hint 'parameter not used'

interface

uses
  typinfo,
  fgl,
  JCoreClasses,
  JCoreLogger;

type

  TJCoreMetadataCompositionType = (jctNone, jctComposition, jctAggregation);

  TJCoreClassMetadata = class;
  TJCoreClassMetadataClass = class of TJCoreClassMetadata;

  TJCoreModel = class;

  { TJCoreAttrMetadata }

  TJCoreAttrMetadata = class(TObject)
  private
    FCompositionClass: TClass;
    FCompositionMetadata: TJCoreClassMetadata;
    FCompositionType: TJCoreMetadataCompositionType;
    FModel: TJCoreModel;
    FName: string;
    FOwner: TJCoreClassMetadata;
    FPropInfo: PPropInfo;
    FSize: Integer;
    function GetCompositionMetadata: TJCoreClassMetadata;
    procedure ReadPropertyInfo;
    procedure SetCompositionClass(AValue: TClass);
    procedure SetCompositionType(AValue: TJCoreMetadataCompositionType);
  protected
    procedure Changed;
    function IsClass: Boolean;
    function IsString: Boolean;
    property Model: TJCoreModel read FModel;
  public
    constructor Create(const AModel: TJCoreModel; const AOwner: TJCoreClassMetadata; const APropInfo: PPropInfo); virtual;
    function IsCollection: Boolean; virtual; abstract;
    property CompositionClass: TClass read FCompositionClass write SetCompositionClass;
    property CompositionMetadata: TJCoreClassMetadata read GetCompositionMetadata;
    property CompositionType: TJCoreMetadataCompositionType read FCompositionType write SetCompositionType;
    property Name: string read FName;
    property Owner: TJCoreClassMetadata read FOwner;
    property PropInfo: PPropInfo read FPropInfo;
    property Size: Integer read FSize write FSize;
  end;

  TJCoreAttrMetadataClass = class of TJCoreAttrMetadata;
  TJCoreAttrMetadataList = specialize TFPGObjectList<TJCoreAttrMetadata>;

  { TJCoreClassMetadata }

  TJCoreClassMetadata = class(TObject)
  private
    FAttrList: TJCoreAttrMetadataList;
    FClass: TClass;
    FModel: TJCoreModel;
    FOwnerAttr: TJCoreAttrMetadata;
    FOwnerClass: TJCoreClassMetadata;
    FParent: TJCoreClassMetadata;
    function GetAttributes(const AIndex: Integer): TJCoreAttrMetadata;
    procedure SetOwnerAttr(AValue: TJCoreAttrMetadata);
  protected
    procedure AddAttribute(const APropInfo: PPropInfo); virtual;
    procedure BuildClassMetadata;
    property AttrList: TJCoreAttrMetadataList read FAttrList;
    property Model: TJCoreModel read FModel;
  public
    constructor Create(const AModel: TJCoreModel; const AClass: TClass; const AParent: TJCoreClassMetadata); virtual;
    destructor Destroy; override;
    function AttributeByName(const AAttributeName: string): TJCoreAttrMetadata;
    function AttributeCount: Integer;
    function FindAttribute(const AAttributeName: string): TJCoreAttrMetadata;
    property Attributes[const AIndex: Integer]: TJCoreAttrMetadata read GetAttributes; default;
    property OwnerAttr: TJCoreAttrMetadata read FOwnerAttr write SetOwnerAttr;
    property OwnerClass: TJCoreClassMetadata read FOwnerClass;
    property Parent: TJCoreClassMetadata read FParent;
    property TheClass: TClass read FClass;
  end;

  TJCoreClassMetadataMap = specialize TFPGMap<Pointer, TJCoreClassMetadata>;
  TJCoreGenericsMap = specialize TFPGMap<Pointer, TClass>;

  { TJCoreModel }

  TJCoreModel = class(TJCoreManagedObject)
  private
    FClassMap: TJCoreClassMap;
    FGenericsMap: TJCoreGenericsMap;
    FMetadataMap: TJCoreClassMetadataMap;
    class var FLOG: IJCoreLogger;
  protected
    function AttributeMetadataClass: TJCoreAttrMetadataClass; virtual;
    function ClassMetadataClass: TJCoreClassMetadataClass; virtual;
    procedure Finit; override;
    procedure InitRegistry; virtual;
    property ClassMap: TJCoreClassMap read FClassMap;
    property GenericsMap: TJCoreGenericsMap read FGenericsMap;
    property MetadataMap: TJCoreClassMetadataMap read FMetadataMap;
    class property LOG: IJCoreLogger read FLOG;
  public
    constructor Create; virtual;
    function AcquireMetadata(const AClass: TClass): TJCoreClassMetadata;
    procedure AddClass(const AClassArray: array of TClass);
    procedure AddGenerics(const ASpecializedClass, ASpecializationClass: TClass);
    function FindClass(const AClassName: string): TClass;
    function FindSpecializedClass(const ASpecializationClass: TClass): TClass;
    function IsEntityClass(const AClass: TClass): Boolean;
  end;

implementation

uses
  sysutils,
  JCoreConsts;

{ TJCoreAttrMetadata }

function TJCoreAttrMetadata.GetCompositionMetadata: TJCoreClassMetadata;
begin
  if not Assigned(FCompositionMetadata) and Assigned(CompositionClass) then
    FCompositionMetadata := Model.AcquireMetadata(CompositionClass);
  Result := FCompositionMetadata;
end;

procedure TJCoreAttrMetadata.ReadPropertyInfo;
const
  CCompositionType : array[Boolean] of TJCoreMetadataCompositionType = (jctAggregation, jctComposition);
var
  VTypeData: PTypeData;
  VInstance: TObject;
begin
  if IsString then
  begin
    FSize := PropInfo^.Index;
  end else if IsClass then
  begin
    VTypeData := GetTypeData(PropInfo^.PropType);
    VInstance := VTypeData^.ClassType.NewInstance;
    try
      FCompositionType := CCompositionType[IsStoredProp(VInstance, PropInfo)];
    finally
      VInstance.FreeInstance;
    end;
  end;
end;

procedure TJCoreAttrMetadata.SetCompositionClass(AValue: TClass);
begin
  if FCompositionClass <> AValue then
  begin
    FCompositionClass := AValue;
    FCompositionMetadata := nil;
  end;
end;

procedure TJCoreAttrMetadata.SetCompositionType(AValue: TJCoreMetadataCompositionType);
begin
  if FCompositionType <> AValue then
  begin
    FCompositionType := AValue;
    Changed;
  end;
end;

procedure TJCoreAttrMetadata.Changed;
var
  VCompositionMetadata: TJCoreClassMetadata;
begin
  VCompositionMetadata := CompositionMetadata;
  if Assigned(VCompositionMetadata) and IsCollection then
  begin
    if CompositionType = jctComposition then
      VCompositionMetadata.OwnerAttr := Self
    else // jctAggregation
      VCompositionMetadata.OwnerAttr := nil;
  end;
end;

function TJCoreAttrMetadata.IsClass: Boolean;
begin
  Result := PropInfo^.PropType^.Kind = tkClass;
end;

function TJCoreAttrMetadata.IsString: Boolean;
begin
  Result := PropInfo^.PropType^.Kind in [tkSString, tkAString, tkLString, tkWString, tkUString];
end;

constructor TJCoreAttrMetadata.Create(const AModel: TJCoreModel; const AOwner: TJCoreClassMetadata;
  const APropInfo: PPropInfo);
begin
  inherited Create;
  FModel := AModel;
  FOwner := AOwner;
  FPropInfo := APropInfo;
  FName := APropInfo^.Name;
  FCompositionType := jctNone;
  ReadPropertyInfo;
end;

{ TJCoreClassMetadata }

function TJCoreClassMetadata.GetAttributes(
  const AIndex: Integer): TJCoreAttrMetadata;
begin
  Result := AttrList[AIndex];
end;

procedure TJCoreClassMetadata.SetOwnerAttr(AValue: TJCoreAttrMetadata);
begin
  if not Assigned(FOwnerAttr) or not Assigned(AValue) then
  begin
    FOwnerAttr := AValue;
    if Assigned(FOwnerAttr) then
    begin
      if FOwnerAttr.CompositionType <> jctComposition then
        raise EJCoreMetadata.Create(503, S0503_InvalidOwnerAttr, [FOwnerAttr.Name]);
      FOwnerClass := FOwnerAttr.Owner;
    end else
      FOwnerClass := nil;
  end else if FOwnerAttr <> AValue then
    raise EJCoreMetadata.Create(502, S0502_MetadataAlreadyOwned, []);
end;

procedure TJCoreClassMetadata.AddAttribute(const APropInfo: PPropInfo);
begin
  AttrList.Add(Model.AttributeMetadataClass.Create(Model, Self, APropInfo));
end;

procedure TJCoreClassMetadata.BuildClassMetadata;
var
  VPropList: PPropList;
  VPropListCount: Integer;
  VParentPropCount: Integer;
  I: Integer;
begin
  if TheClass = TObject then
    Exit;
  VPropListCount := GetPropList(TheClass, VPropList);
  if Assigned(VPropList) then
  begin
    try
      VParentPropCount := GetTypeData(PTypeInfo(TheClass.ClassParent.ClassInfo))^.PropCount;
      for I := VParentPropCount to Pred(VPropListCount) do
        AddAttribute(VPropList^[I]);
    finally
      Freemem(VPropList);
    end;
  end;
end;

constructor TJCoreClassMetadata.Create(const AModel: TJCoreModel; const AClass: TClass;
  const AParent: TJCoreClassMetadata);
begin
  inherited Create;
  FModel := AModel;
  FClass := AClass;
  FParent := AParent;
  FAttrList := TJCoreAttrMetadataList.Create(True);
  BuildClassMetadata;
end;

destructor TJCoreClassMetadata.Destroy;
begin
  FreeAndNil(FAttrList);
  inherited Destroy;
end;

function TJCoreClassMetadata.AttributeByName(
  const AAttributeName: string): TJCoreAttrMetadata;
begin
  Result := FindAttribute(AAttributeName);
  if not Assigned(Result) then
    raise EJCoreMetadata.Create(501, S0501_AttributeNotFound, [TheClass.ClassName, AAttributeName]);
end;

function TJCoreClassMetadata.AttributeCount: Integer;
begin
  Result := AttrList.Count;
end;

function TJCoreClassMetadata.FindAttribute(const AAttributeName: string): TJCoreAttrMetadata;
begin
  for Result in AttrList do
    if SameText(AAttributeName, Result.Name) then
      Exit;
  Result := nil;
end;

{ TJCoreModel }

function TJCoreModel.AttributeMetadataClass: TJCoreAttrMetadataClass;
begin
  Result := TJCoreAttrMetadata;
end;

function TJCoreModel.ClassMetadataClass: TJCoreClassMetadataClass;
begin
  Result := TJCoreClassMetadata;
end;

procedure TJCoreModel.Finit;
var
  I: Integer;
begin
  FreeAndNil(FClassMap);
  FreeAndNil(FGenericsMap);
  { TODO : Fix AV on all map.free if an exception raises freeing an item.
           Need to assign nil or remove the item from the map }
  for I := 0 to Pred(FMetadataMap.Count) do
    FMetadataMap.Data[I].Free;
  FreeAndNil(FMetadataMap);
  inherited Finit;
end;

procedure TJCoreModel.InitRegistry;
begin
end;

constructor TJCoreModel.Create;
begin
  inherited Create;
  if not Assigned(FLOG) then
    FLOG := TJCoreLogger.GetLogger('jcore.model');
  FClassMap := TJCoreClassMap.Create;
  FGenericsMap := TJCoreGenericsMap.Create;
  FMetadataMap := TJCoreClassMetadataMap.Create;
  InitRegistry;
end;

function TJCoreModel.AcquireMetadata(const AClass: TClass): TJCoreClassMetadata;
var
  VParent: TClass;
  VParentMetadata: TJCoreClassMetadata;
  VIndex: Integer;
begin
  { TODO : Thread safe }
  VIndex := MetadataMap.IndexOf(AClass);
  if VIndex = -1 then
  begin
    VParent := AClass.ClassParent;
    if VParent <> TObject then
      VParentMetadata := AcquireMetadata(VParent)
    else
      VParentMetadata := nil;
    Result := TJCoreClassMetadata(ClassMetadataClass.NewInstance);
    VIndex := MetadataMap.Add(AClass, Result);
    try
      // Calling constructor after adding metadata because
      // building attribute metadata sometimes search it's
      // owner inside the class metadata constructor.
      Result.Create(Self, AClass, VParentMetadata);
    except
      MetadataMap.Delete(VIndex);
      raise;
    end;
  end else
    Result := MetadataMap.Data[VIndex];
end;

procedure TJCoreModel.AddClass(const AClassArray: array of TClass);
var
  VClass: TClass;
begin
  for VClass in AClassArray do
    ClassMap.Add(VClass.ClassName, VClass);
end;

procedure TJCoreModel.AddGenerics(const ASpecializedClass, ASpecializationClass: TClass);
begin
  GenericsMap.Add(ASpecializedClass, ASpecializationClass);
end;

function TJCoreModel.FindClass(const AClassName: string): TClass;
var
  VIndex: Integer;
begin
  VIndex := ClassMap.IndexOf(AClassName);
  if VIndex >= 0 then
    Result := ClassMap.Data[VIndex]
  else
    Result := nil;
end;

function TJCoreModel.FindSpecializedClass(const ASpecializationClass: TClass): TClass;
var
  VIndex: Integer;
begin
  VIndex := GenericsMap.IndexOf(ASpecializationClass);
  if VIndex >= 0 then
    Result := GenericsMap.Data[VIndex]
  else
    Result := nil;
end;

function TJCoreModel.IsEntityClass(const AClass: TClass): Boolean;
begin
  Result := ClassMap.IndexOfData(AClass) >= 0;
end;

end.

