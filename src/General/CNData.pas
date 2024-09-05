unit CNData;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------
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
    TCNDataPropLegacy = (
        INVALID = 0,
        k = 1,
        DiaStrand = 2,
        GmrStrand = 3,
        Rstrand = 4
    );
    TCNDataProp = (
        INVALID = 0,
        k = 1,
        DiaStrand = 2,
        GMRStrand = 3,
        RStrand = 4
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
        kStrand: Integer;
        diaStrand: Double;
        gmrStrand: Double;
        rStrand: Double;

        constructor Create(ParClass: TDSSClass; const CNDataName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags); override;
        procedure MakeLike(OtherPtr: Pointer); override;
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
    TPropLegacy = TCNDataPropLegacy;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;    

constructor TCNData.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
    end;

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
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, PropInfoLegacy);

    // integer properties
    PropertyType[ActiveProperty + ord(TProp.k)] := TPropertyType.IntegerProperty;
    PropertyOffset[ActiveProperty + ord(TProp.k)] := ptruint(@obj.kStrand);
    // PropertyMinimum[ActiveProperty + ord(TProp.k)] := 2; //TODO: add support for minimum value

    // double properties (default type)
    PropertyOffset[ActiveProperty + ord(TProp.DiaStrand)] := ptruint(@obj.diaStrand);
    PropertyFlags[ActiveProperty + ord(TProp.DiaStrand)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero, TPropertyFlag.NoDefault];

    PropertyOffset[ActiveProperty + ord(TProp.GmrStrand)] := ptruint(@obj.gmrStrand);
    PropertyFlags[ActiveProperty + ord(TProp.GmrStrand)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero, TPropertyFlag.DynamicDefault];

    PropertyOffset[ActiveProperty + ord(TProp.Rstrand)] := ptruint(@obj.rStrand);
    PropertyFlags[ActiveProperty + ord(TProp.Rstrand)] := [TPropertyFlag.NoDefault, TPropertyFlag.Units_ohm_per_length];//, TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

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

procedure TCNDataObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags);
begin
    // Set defaults
    case Idx of
        ord(TProp.DiaStrand):
            if gmrStrand <= 0.0 then
                gmrStrand := 0.7788 * 0.5 * diaStrand;
    end;
    // Check for critical errors
    case Idx of
        ord(TProp.k):
            if (kStrand < 2) then
                DoSimpleMsg('Error: Must have at least 2 concentric neutral strands for CNData %s', [Name], 999);
        ord(TProp.DiaStrand):
            if (diaStrand <= 0.0) then
                DoSimpleMsg('Error: Neutral strand diameter must be positive for CNData %s', [Name], 999);
        ord(TProp.GmrStrand):
            if (gmrStrand <= 0.0) then
                DoSimpleMsg('Error: Neutral strand GMR must be positive for CNData %s', [Name], 999);
    end;
    inherited PropertySideEffects(Idx, previousIntVal, setterFlags);
end;

procedure TCNDataObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    kStrand := Other.kStrand;
    diaStrand := Other.diaStrand;
    gmrStrand := Other.gmrStrand;
    rStrand := Other.rStrand;
end;

constructor TCNDataObj.Create(ParClass: TDSSClass; const CNDataName: String);
begin
    inherited Create(ParClass, CNDataName);
    Name := AnsiLowerCase(CNDataName);
    DSSObjType := ParClass.DSSClassType;
    kStrand := 2;
    diaStrand := -1.0;
    gmrStrand := -1.0;
    rStrand := -1.0;
end;

destructor TCNDataObj.Destroy;
begin
    inherited destroy;
end;

end.
