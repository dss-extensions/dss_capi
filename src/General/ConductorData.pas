unit ConductorData;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2020, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

// The ConductorData object is a general DSS object used by all circuits
// as a reference for obtaining line impedances.
//
// The values are set by the normal New and Edit procedures for any DSS object.
//
// The values are retrieved by setting the Code Property in the ConductorData Class.
// This sets the active ConductorData object to be the one referenced by the Code Property;
//
// Then the values of that code can be retrieved via the public variables.

uses
    Classes,
    Command,
    DSSClass,
    DSSObject,
    ArrayDef;

type
{$SCOPEDENUMS ON}
    TConductorDataProp = (
        INVALID = 0,
        Rdc = 1,
        Rac = 2,
        Runits = 3,
        GMRac = 4,
        GMRunits = 5,
        radius = 6,
        radunits = 7,
        normamps = 8,
        emergamps = 9,
        diam = 10,
        Seasons = 11,
        Ratings = 12,
        Capradius = 13
    );
{$SCOPEDENUMS OFF}

    ConductorChoice = (Overhead, ConcentricNeutral, TapeShield, Unknown);

    ConductorChoiceArray = array[1..100] of ConductorChoice;
    pConductorChoiceArray = ^ConductorChoiceArray;

    TConductorData = class(TDSSClass)
    PROTECTED
        PropertyOffset_ConductorData: Integer;

        procedure CountPropertiesAndAllocate; override;
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
        destructor Destroy; OVERRIDE;
    end;

    TConductorDataObj = class(TDSSObject)
    PUBLIC
        FRDC: Double;
        FR60: Double;
        FGMR60: Double;
        Fcapradius60: Double;  // in case it is different than radius for cap calcs
        Fradius: Double;
        FGMRUnits: Integer;
        FResistanceUnits: Integer;
        FRadiusUnits: Integer;
        NormAmps: Double;
        EmergAmps: Double;
        NumAmpRatings: Integer;
        AmpRatings: array of Double;

        constructor Create(ParClass: TDSSClass; const ConductorDataName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherObj: Pointer); override;

        property Rdc: Double READ FRDC;
        property Rac: Double READ FR60;
        property GMR: Double READ FGMR60;
        Property CapRadius: Double Read Fcapradius60;
        property Radius: Double READ FRadius;
        property ResUnits: Integer READ FresistanceUnits;
        property RadiusUnits: Integer READ FradiusUnits;
        property GMRUnits: Integer READ FGMRUnits;
    end;

    TConductorDataArray = array[1..100] of TConductorDataObj;
    pConductorDataArray = ^TConductorDataArray;

implementation

uses
    DSSGlobals,
    DSSClassDefs,
    Sysutils,
    UComplex, DSSUcomplex,
    LineUNits,
    Utilities,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TConductorDataObj;
    TProp = TConductorDataProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    

constructor TConductorData.Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, DSSClsType or DSS_OBJECT, DSSClsName);
    ClassParents.Add('ConductorData');
end;

destructor TConductorData.Destroy;
begin
    inherited Destroy;
end;

procedure TConductorData.CountPropertiesAndAllocate;
begin
    NumProperties := NumProperties + NumPropsThisClass;
    inherited CountPropertiesAndAllocate;
end;

procedure TConductorData.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    PopulatePropertyNames(ActiveProperty, NumPropsThisClass, PropInfo);

    PropertyOffset_ConductorData := ActiveProperty;
    // enums
    PropertyType[ActiveProperty + ord(TProp.Runits)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ActiveProperty + ord(TProp.Runits)] := ptruint(@obj.FresistanceUnits);
    PropertyOffset2[ActiveProperty + ord(TProp.Runits)] := PtrInt(DSS.UnitsEnum);

    PropertyType[ActiveProperty + ord(TProp.GMRunits)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ActiveProperty + ord(TProp.GMRunits)] := ptruint(@obj.FGMRUnits);
    PropertyOffset2[ActiveProperty + ord(TProp.GMRunits)] := PtrInt(DSS.UnitsEnum);

    PropertyType[ActiveProperty + ord(TProp.radunits)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ActiveProperty + ord(TProp.radunits)] := ptruint(@obj.FRadiusUnits);
    PropertyOffset2[ActiveProperty + ord(TProp.radunits)] := PtrInt(DSS.UnitsEnum);

    // double arrays
    PropertyType[ActiveProperty + ord(TProp.Ratings)] := TPropertyType.DoubleDArrayProperty;
    PropertyOffset[ActiveProperty + ord(TProp.Ratings)] := ptruint(@obj.AmpRatings);
    PropertyOffset2[ActiveProperty + ord(TProp.Ratings)] := ptruint(@obj.NumAmpRatings);

    // double properties (default type)
    PropertyOffset[ActiveProperty + ord(TProp.Rdc)] := ptruint(@obj.FRDC);
    PropertyOffset[ActiveProperty + ord(TProp.Rac)] := ptruint(@obj.FR60);
    PropertyOffset[ActiveProperty + ord(TProp.normamps)] := ptruint(@obj.NormAmps);
    PropertyOffset[ActiveProperty + ord(TProp.emergamps)] := ptruint(@obj.EmergAmps); 

    PropertyOffset[ActiveProperty + ord(TProp.GMRac)] := ptruint(@obj.FGMR60);
    PropertyOffset[ActiveProperty + ord(TProp.radius)] := ptruint(@obj.Fradius);
    PropertyOffset[ActiveProperty + ord(TProp.Capradius)] := ptruint(@obj.Fcapradius60);

    PropertyFlags[ActiveProperty + ord(TProp.GMRac)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];
    PropertyFlags[ActiveProperty + ord(TProp.radius)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];
    PropertyFlags[ActiveProperty + ord(TProp.Capradius)] := [TPropertyFlag.NonZero];

    // scaled double
    PropertyOffset[ActiveProperty + ord(TProp.diam)] := ptruint(@obj.Fradius);
    PropertyScale[ActiveProperty + ord(TProp.diam)] := 1.0 / 2.0;
    PropertyFlags[ActiveProperty + ord(TProp.diam)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero, TPropertyFlag.Redundant];


    // integer properties
    PropertyType[ActiveProperty + ord(TProp.Seasons)] := TPropertyType.IntegerProperty;
    PropertyOffset[ActiveProperty + ord(TProp.Seasons)] := ptruint(@obj.NumAmpRatings);

    ActiveProperty := ActiveProperty + NumPropsThisClass;
    inherited DefineProperties;
end;

procedure TConductorDataObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    Idx2: Integer;
begin
    Idx2 := Idx - (ParentClass as TConductorData).PropertyOffset_ConductorData;
    case Idx2 of
        ord(TProp.Rdc):
            if FR60 < 0.0 then
                FR60 := 1.02 * FRDC;
        ord(TProp.Rac):
            if FRDC < 0.0 then
                FRDC := FR60 / 1.02;
        ord(TProp.GMRac):
        begin
            if Fradius < 0.0 then
                Fradius := FGMR60 / 0.7788;
            if (Fradius = 0.0) then
                DoSimpleMsg('Error: Radius is specified as zero for %s', [FullName], 999);
        end;
        ord(TProp.GMRunits):
            if FradiusUnits = 0 then
                FradiusUnits := FGMRunits;
        ord(TProp.radius), ord(TProp.diam):
        begin
            if FGMR60 < 0.0 then
                FGMR60 := 0.7788 * FRadius;
            if Fcapradius60 < 0.0 then
                Fcapradius60 := Fradius;    // default to radius
        end;
        ord(TProp.radunits):
            if FGMRUnits = 0 then
                FGMRunits := FradiusUnits;
        ord(TProp.normamps):
            if EmergAmps < 0.0 then
                EmergAmps := 1.5 * NormAmps;
        ord(TProp.emergamps):
            if NormAmps < 0.0 then
                NormAmps := EmergAmps / 1.5;
        ord(TProp.Seasons):
            setlength(AmpRatings, NumAmpRatings);
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TConductorDataObj.MakeLike(OtherObj: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherObj);
    Other := TObj(OtherObj);
    FRDC := Other.FRDC;
    FR60 := Other.FR60;
    FResistanceUnits := Other.FResistanceUnits;
    FGMR60 := Other.FGMR60;
    Fcapradius60 := Other.Fcapradius60;
    FGMRUnits := Other.FGMRUnits;
    FRadius := Other.FRadius;
    FRadiusUnits := Other.FRadiusUnits;
    NormAmps := Other.NormAmps;
    EmergAmps := Other.EmergAmps;
end;

constructor TConductorDataObj.Create(ParClass: TDSSClass; const ConductorDataName: String);
begin
    inherited Create(ParClass);
    Name := LowerCase(ConductorDataName);
    DSSObjType := ParClass.DSSClassType;

    FRDC := -1.0;
    FR60 := -1.0;
    FGMR60 := -1.0;
    Fradius := -1.0;
    Fcapradius60 := -1.0;   // init to not defined
    FGMRUnits := 0;
    FResistanceUnits := 0;
    FRadiusUnits := 0;
    Normamps := -1.0;
    EmergAmps := -1.0;
    NumAmpRatings := 1;
    setlength(AmpRatings, NumAmpRatings);
    AmpRatings[0] := NormAmps;
end;

destructor TConductorDataObj.Destroy;
begin
    inherited destroy;
end;

end.