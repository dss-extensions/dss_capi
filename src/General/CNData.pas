unit CNData;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
interface

uses
    Classes,
    Command,
    DSSClass,
    DSSObject,
    ConductorData,
    CableData;

type
{$SCOPEDENUMS ON}
    TCNDataProp = (
        INVALID = 0,
        k = 1,
        DiaStrand = 2,
        GmrStrand = 3,
        Rstrand = 4
    );
{$SCOPEDENUMS OFF}

    TCNData = class(TCableData)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TCNDataObj = class(TCableDataObj)
    PUBLIC
        FkStrand: Integer;
        FDiaStrand: Double;
        FGmrStrand: Double;
        FRStrand: Double;

        constructor Create(ParClass: TDSSClass; const CNDataName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        property NStrand: Integer READ FkStrand;
        property DiaStrand: Double READ FDiaStrand;
        property GmrStrand: Double READ FGmrStrand;
        property RStrand: Double READ FRStrand;
    end;

implementation

uses
    DSSGlobals,
    DSSClassDefs,
    Sysutils,
    UComplex, DSSUcomplex,
    Arraydef,
    LineUnits,
    Utilities,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TCNDataObj;
    TProp = TCNDataProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer;    

constructor TCNData.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, DSS_OBJECT, 'CNData');
end;

destructor TCNData.Destroy;
begin
    inherited Destroy;
end;

procedure TCNData.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    NumProperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // integer properties
    PropertyType[ActiveProperty + ord(TProp.k)] := TPropertyType.IntegerProperty;
    PropertyOffset[ActiveProperty + ord(TProp.k)] := ptruint(@obj.FkStrand);

    // double properties (default type)
    PropertyOffset[ActiveProperty + ord(TProp.DiaStrand)] := ptruint(@obj.FDiaStrand);
    PropertyOffset[ActiveProperty + ord(TProp.GmrStrand)] := ptruint(@obj.FGmrStrand);
    PropertyOffset[ActiveProperty + ord(TProp.Rstrand)] := ptruint(@obj.FRStrand);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TCNData.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        DSS.ActiveDSSObject := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TCNDataObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    // Set defaults
    case Idx of
        2:
            if FGmrStrand <= 0.0 then
                FGmrStrand := 0.7788 * 0.5 * FDiaStrand;
    end;
    // Check for critical errors
    case Idx of
        1:
            if (FkStrand < 2) then
                DoSimpleMsg('Error: Must have at least 2 concentric neutral strands for CNData %s', [Name], 999);
        2:
            if (FDiaStrand <= 0.0) then
                DoSimpleMsg('Error: Neutral strand diameter must be positive for CNData %s', [Name], 999);
        3:
            if (FGmrStrand <= 0.0) then
                DoSimpleMsg('Error: Neutral strand GMR must be positive for CNData %s', [Name], 999);
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TCNDataObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    FkStrand := Other.FkStrand;
    FDiaStrand := Other.FDiaStrand;
    FGmrStrand := Other.FGmrStrand;
    FRStrand := Other.FRStrand;
end;

constructor TCNDataObj.Create(ParClass: TDSSClass; const CNDataName: String);
begin
    inherited Create(ParClass, CNDataName);
    Name := LowerCase(CNDataName);
    DSSObjType := ParClass.DSSClassType;
    FkStrand := 2;
    FDiaStrand := -1.0;
    FGmrStrand := -1.0;
    FRStrand := -1.0;
end;

destructor TCNDataObj.Destroy;
begin
    inherited destroy;
end;

initialization
    PropInfo := NIL;
end.
