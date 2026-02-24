unit ConductorData;

// ----------------------------------------------------------
// Copyright (c) 2008-2020, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

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
    CAPI_Types;

type
{$SCOPEDENUMS ON}
    TConductorDataPropLegacy = (
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
    TConductorDataProp = (
        INVALID = 0,
        RDC = 1,
        RAC = 2,
        RUnits = 3,
        GMRAC = 4,
        GMRUnits = 5,
        Radius = 6,
        RadUnits = 7,
        NormAmps = 8,
        EmergAmps = 9,
        Diam = 10,
        Seasons = 11,
        Ratings = 12,
        CapRadius = 13
    );
{$SCOPEDENUMS OFF}

    ConductorChoice = (Overhead, ConcentricNeutral, TapeShield, Unknown);

    ConductorChoiceArray = array[1..100] of ConductorChoice;
    pConductorChoiceArray = ^ConductorChoiceArray;

    TConductorData = class(TDSSClass)
    PROTECTED
        PropertyOffset_ConductorData: Integer;

        procedure CountPropertiesAndAllocate(); override;
        procedure DefineProperties(); override;
    PUBLIC
        constructor Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
        destructor Destroy; OVERRIDE;
    end;

    TConductorDataObj = class(TDSSObject)
    PUBLIC
        RDC: Double;
        RAC: Double;
        GMRAC: Double;
        capRadius: Double;  // in case it is different than radius for cap calcs
        radius: Double;
        GMRUnits: Integer;
        resistanceUnits: Integer;
        radiusUnits: Integer;
        NormAmps: Double;
        EmergAmps: Double;
        NumAmpRatings: Integer;
        AmpRatings: array of Double;

        constructor Create(ParClass: TDSSClass; const ConductorDataName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags); override;
        procedure MakeLike(OtherObj: Pointer); override;
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
    TPropLegacy = TConductorDataPropLegacy;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;    

constructor TConductorData.Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
    end;

    inherited Create(dssContext, DSSClsType or DSS_OBJECT, DSSClsName);
    ClassParents.Add('ConductorData');
end;

destructor TConductorData.Destroy;
begin
    inherited Destroy;
end;

procedure TConductorData.CountPropertiesAndAllocate();
begin
    NumProperties := NumProperties + NumPropsThisClass;
    inherited CountPropertiesAndAllocate;
end;

procedure TConductorData.DefineProperties();
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    PopulatePropertyNames(ActiveProperty, NumPropsThisClass, PropInfo, PropInfoLegacy, False, 'ConductorData');

    PropertyOffset_ConductorData := ActiveProperty;
    // enums
    PropertyType[ActiveProperty + ord(TProp.Runits)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ActiveProperty + ord(TProp.Runits)] := PtrInt(@obj.resistanceUnits);
    PropertyOffset2[ActiveProperty + ord(TProp.Runits)] := PtrInt(DSS.UnitsEnum);

    PropertyType[ActiveProperty + ord(TProp.GMRunits)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ActiveProperty + ord(TProp.GMRunits)] := PtrInt(@obj.GMRUnits);
    PropertyOffset2[ActiveProperty + ord(TProp.GMRunits)] := PtrInt(DSS.UnitsEnum);

    PropertyType[ActiveProperty + ord(TProp.radunits)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ActiveProperty + ord(TProp.radunits)] := PtrInt(@obj.radiusUnits);
    PropertyOffset2[ActiveProperty + ord(TProp.radunits)] := PtrInt(DSS.UnitsEnum);

    // double arrays
    PropertyType[ActiveProperty + ord(TProp.Ratings)] := TPropertyType.DoubleDArrayProperty;
    PropertyOffset[ActiveProperty + ord(TProp.Ratings)] := PtrInt(@obj.AmpRatings);
    PropertyOffset2[ActiveProperty + ord(TProp.Ratings)] := PtrInt(@obj.NumAmpRatings);

    // double properties (default type)
    PropertyOffset[ActiveProperty + ord(TProp.Rdc)] := PtrInt(@obj.RDC);
    PropertyFlags[ActiveProperty + ord(TProp.Rdc)] := [TPropertyFlag.DynamicDefault, TPropertyFlag.Units_ohm_per_length];
    
    PropertyOffset[ActiveProperty + ord(TProp.Rac)] := PtrInt(@obj.RAC);
    PropertyFlags[ActiveProperty + ord(TProp.Rac)] := [TPropertyFlag.DynamicDefault];

    PropertyOffset[ActiveProperty + ord(TProp.normamps)] := PtrInt(@obj.NormAmps);
    PropertyFlags[ActiveProperty + ord(TProp.normamps)] := [TPropertyFlag.DynamicDefault];

    PropertyOffset[ActiveProperty + ord(TProp.emergamps)] := PtrInt(@obj.EmergAmps); 
    PropertyFlags[ActiveProperty + ord(TProp.emergamps)] := [TPropertyFlag.DynamicDefault];

    PropertyOffset[ActiveProperty + ord(TProp.GMRac)] := PtrInt(@obj.GMRAC);
    PropertyFlags[ActiveProperty + ord(TProp.GMRac)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero, TPropertyFlag.DynamicDefault];
    
    PropertyOffset[ActiveProperty + ord(TProp.radius)] := PtrInt(@obj.radius);
    PropertyFlags[ActiveProperty + ord(TProp.radius)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero, TPropertyFlag.DynamicDefault];
    
    PropertyOffset[ActiveProperty + ord(TProp.Capradius)] := PtrInt(@obj.capRadius);
    PropertyFlags[ActiveProperty + ord(TProp.Capradius)] := [TPropertyFlag.NonZero, TPropertyFlag.DynamicDefault];

    // scaled double
    PropertyOffset[ActiveProperty + ord(TProp.diam)] := PtrInt(@obj.radius);
    PropertyScale[ActiveProperty + ord(TProp.diam)] := 1.0 / 2.0;
    PropertyFlags[ActiveProperty + ord(TProp.diam)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero, TPropertyFlag.Redundant];
    PropertyRedundantWith[ActiveProperty + ord(TProp.diam)] := ActiveProperty + ord(TProp.radius);

    // integer properties
    PropertyType[ActiveProperty + ord(TProp.Seasons)] := TPropertyType.IntegerProperty;
    PropertyOffset[ActiveProperty + ord(TProp.Seasons)] := PtrInt(@obj.NumAmpRatings);
    PropertyFlags[ActiveProperty + ord(TProp.Seasons)] := [TPropertyFlag.SuppressJSON]; // can be derived trivially from length(Ratings)

    ActiveProperty := ActiveProperty + NumPropsThisClass;
    inherited DefineProperties();
end;

procedure TConductorDataObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags);
var
    Idx2: Integer;
begin
    Idx2 := Idx - (ParentClass as TConductorData).PropertyOffset_ConductorData;
    case Idx2 of
        ord(TProp.Rdc):
            if RAC < 0.0 then
                RAC := 1.02 * RDC;
        ord(TProp.Rac):
            if RDC < 0.0 then
                RDC := RAC / 1.02;
        ord(TProp.GMRac):
        begin
            if radius < 0.0 then
                radius := GMRAC / 0.7788;
            if (radius = 0.0) then
                DoSimpleMsg('Error: Radius is specified as zero for %s', [FullName()], 999);
        end;
        ord(TProp.GMRunits):
            if radiusUnits = 0 then
                radiusUnits := GMRUnits;
        ord(TProp.radius), ord(TProp.diam):
        begin
            if GMRAC < 0.0 then
                GMRAC := 0.7788 * radius;
            if capRadius < 0.0 then
                capRadius := radius;    // default to radius
        end;
        ord(TProp.radunits):
            if GMRUnits = 0 then
                GMRUnits := radiusUnits;
        ord(TProp.normamps):
            if EmergAmps < 0.0 then
                EmergAmps := 1.5 * NormAmps;
        ord(TProp.emergamps):
            if NormAmps < 0.0 then
                NormAmps := EmergAmps / 1.5;
        ord(TProp.Seasons):
            setlength(AmpRatings, NumAmpRatings);
    end;
    inherited PropertySideEffects(Idx, previousIntVal, setterFlags);
end;

procedure TConductorDataObj.MakeLike(OtherObj: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherObj);
    Other := TObj(OtherObj);
    RDC := Other.RDC;
    RAC := Other.RAC;
    resistanceUnits := Other.resistanceUnits;
    GMRAC := Other.GMRAC;
    capRadius := Other.capRadius;
    GMRUnits := Other.GMRUnits;
    radius := Other.radius;
    radiusUnits := Other.radiusUnits;
    NormAmps := Other.NormAmps;
    EmergAmps := Other.EmergAmps;
end;

constructor TConductorDataObj.Create(ParClass: TDSSClass; const ConductorDataName: String);
begin
    inherited Create(ParClass, ConductorDataName);
    DSSObjType := ParClass.DSSClassType;

    RDC := -1.0;
    RAC := -1.0;
    GMRAC := -1.0;
    radius := -1.0;
    capRadius := -1.0;   // init to not defined
    GMRUnits := 0;
    resistanceUnits := 0;
    radiusUnits := 0;
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
