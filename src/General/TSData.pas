unit TSData;

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
    CableData,
    ConductorData;

type
{$SCOPEDENUMS ON}
    TTSDataProp = (
        INVALID = 0,
        DiaShield = 1,
        TapeLayer = 2,
        TapeLap = 3
    );
    TTSDataPropLegacy = TTSDataProp;
{$SCOPEDENUMS OFF}
    TTSData = class(TCableData)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TTSDataObj = class(TCableDataObj)
    PUBLIC
        DiaShield: Double;
        TapeLayer: Double;
        TapeLap: Double;

        constructor Create(ParClass: TDSSClass; const TSDataName: String);
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
    TObj = TTSDataObj;
    TProp = TTSDataProp;
    TPropLegacy = TTSDataPropLegacy;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;    

constructor TTSData.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
    end;

    inherited Create(dssContext, DSS_OBJECT, 'TSData');
end;

destructor TTSData.Destroy;
begin
    inherited Destroy;
end;

procedure TTSData.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    NumProperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, PropInfoLegacy);

    // double properties (default type)
    PropertyOffset[ActiveProperty + ord(TProp.DiaShield)] := ptruint(@obj.DiaShield);
    PropertyFlags[ActiveProperty + ord(TProp.DiaShield)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero, TPropertyFlag.NoDefault];
    
    PropertyOffset[ActiveProperty + ord(TProp.TapeLayer)] := ptruint(@obj.TapeLayer);
    PropertyFlags[ActiveProperty + ord(TProp.TapeLayer)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero, TPropertyFlag.NoDefault];
    
    PropertyOffset[ActiveProperty + ord(TProp.TapeLap)] := ptruint(@obj.TapeLap);
    PropertyFlags[ActiveProperty + ord(TProp.TapeLap)] := [TPropertyFlag.NonNegative, TPropertyFlag.NoDefault];

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TTSData.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        DSS.ActiveDSSObject := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TTSDataObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags);
begin
    // Check for critical errors
    case Idx of
        ord(TProp.DiaShield):
            if (DiaShield <= 0.0) then
                DoSimpleMsg('Error: Diameter over shield must be positive for TapeShieldData %s', [Name], 999);
        ord(TProp.TapeLayer):
            if (TapeLayer <= 0.0) then
                DoSimpleMsg('Error: Tape shield thickness must be positive for TapeShieldData %s', [Name], 999);
        ord(TProp.TapeLap):
            if ((TapeLap < 0.0) or (TapeLap > 100.0)) then
                DoSimpleMsg('Error: Tap lap must range from 0 to 100 for TapeShieldData %s', [Name], 999);
    end;
    inherited PropertySideEffects(Idx, previousIntVal, setterFlags);
end;

procedure TTSDataObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    DiaShield := Other.DiaShield;
    TapeLayer := Other.TapeLayer;
    TapeLap := Other.TapeLap;
end;

constructor TTSDataObj.Create(ParClass: TDSSClass; const TSDataName: String);
begin
    inherited Create(ParClass, TSDataName);
    Name := AnsiLowerCase(TSDataName);
    DSSObjType := ParClass.DSSClassType;
    DiaShield := -1.0;
    TapeLayer := -1.0;
    TapeLap := 20.0;
end;

destructor TTSDataObj.Destroy;
begin
    inherited destroy;
end;

end.
