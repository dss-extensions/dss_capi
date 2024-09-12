unit LineSpacing;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

uses
    Classes,
    Sysutils,
    Arraydef,
    Command,
    DSSClass,
    DSSObject;

type
{$SCOPEDENUMS ON}
    TLineSpacingPropLegacy = (
        INVALID = 0,
        nconds = 1,
        nphases = 2,
        x = 3,
        h = 4,
        units = 5
    );
    TLineSpacingProp = (
        INVALID = 0,
        NConds = 1,
        NPhases = 2,
        X = 3,
        H = 4,
        Units = 5
    );
{$SCOPEDENUMS OFF}

    SpcParmChoice = (X, H);

    TLineSpacing = class(TDSSClass)
    PROTECTED
        procedure DefineProperties(); override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TLineSpacingObj = class(TDSSObject)
    PUBLIC
        FX: pDoubleArray;
        FY: pDoubleArray;
        NConds: Integer;
        NPhases: Integer;
        Units: Integer;

        // CIM Accessors
        function GetXCoord(i: Integer): Double;
        function GetYCoord(i: Integer): Double;
    PUBLIC
        DataChanged: Boolean;
        constructor Create(ParClass: TDSSClass; const LineSpacingName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags); override;
        procedure MakeLike(OtherPtr: Pointer); override;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    UComplex, DSSUcomplex,
    Utilities,
    LineUnits,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TLineSpacingObj;
    TProp = TLineSpacingProp;
    TPropLegacy = TLineSpacingPropLegacy;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;    

constructor TLineSpacing.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
    end;

    inherited Create(dssContext, DSS_OBJECT, 'LineSpacing');
end;

destructor TLineSpacing.Destroy;
begin
    inherited Destroy;
end;

procedure TLineSpacing.DefineProperties();
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, PropInfoLegacy);
    
    // enums
    PropertyType[ord(TProp.units)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.units)] := ptruint(@obj.Units);
    PropertyOffset2[ord(TProp.units)] := PtrInt(DSS.UnitsEnum);

    // integers
    PropertyType[ord(TProp.nphases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.nphases)] := ptruint(@obj.Nphases);

    PropertyType[ord(TProp.nconds)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.nconds)] := ptruint(@obj.NConds);
    PropertyFlags[ord(TProp.nconds)] := [TPropertyFlag.SuppressJSON];

    // arrays
    PropertyType[ord(TProp.X)] := TPropertyType.DoubleVArrayProperty;
    PropertyOffset[ord(TProp.X)] := ptruint(@obj.FX);
    PropertyOffset2[ord(TProp.X)] := ptruint(@obj.NConds);

    PropertyType[ord(TProp.H)] := TPropertyType.DoubleVArrayProperty;
    PropertyOffset[ord(TProp.H)] := ptruint(@obj.FY);
    PropertyOffset2[ord(TProp.H)] := ptruint(@obj.NConds);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties();
end;

function TLineSpacing.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    obj: TObj;
begin
    obj := TObj.Create(Self, ObjName);
    if Activate then 
        DSS.ActiveDSSObject := obj;
    obj.ClassIndex := AddObjectToList(obj, Activate);
    Result := obj;
end;

procedure TLineSpacingObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags);
begin
    case Idx of
        ord(TProp.nconds):
        begin
            ReAllocmem(FX, Sizeof(FX[1]) * NConds);
            ReAllocmem(FY, Sizeof(FY[1]) * NConds);
            Units := UNITS_FT;
            DataChanged := TRUE;
        end;
        2..5:
            DataChanged := TRUE;
    end;
    inherited PropertySideEffects(Idx, previousIntVal, setterFlags);
end;

procedure TLineSpacingObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    NConds := Other.NConds;
    PropertySideEffects(ord(TProp.NConds), 0, []);
    NPhases := Other.NPhases;
    for i := 1 to NConds do
        FX[i] := Other.FX[i];
    for i := 1 to NConds do
        FY[i] := Other.FY[i];
    Units := Other.Units;
    DataChanged := TRUE;
end;

constructor TLineSpacingObj.Create(ParClass: TDSSClass; const LineSpacingName: String);
var
    i: Integer;
begin
    inherited Create(ParClass, LineSpacingName);
    DSSObjType := ParClass.DSSClassType;

    DataChanged := TRUE;
    FX := NIL;
    FY := NIL;
    units := UNITS_FT;
    NConds := 3;
    PropertySideEffects(ord(TProp.NConds), 0, []);
    // TODO: consider using NaN to indicate that the user left invalid data
    for i := 1 to NConds do
    begin
        FX[i] := 0;
        FY[i] := 0;
    end;
    NPhases := 3;
end;

destructor TLineSpacingObj.Destroy;
begin
    Reallocmem(FY, 0);
    Reallocmem(FX, 0);
    inherited destroy;
end;

function ArrayString(pF: pDoubleArray; N: Integer): String;
var
    i: Integer;
    r: String;
begin
    r := '[';
    if N > 0 then
        r := r + Format('%-g', [pF[1]]);
    for i := 2 to N do
        r := r + Format(',%-g', [pF[i]]);
    Result := r + ']';
end;

function TLineSpacingObj.GetXCoord(i: Integer): Double;
begin
    if i <= NConds then
        Result := FX[i]
    else
        Result := 0.0;
end;

function TLineSpacingObj.GetYCoord(i: Integer): Double;
begin
    if i <= NConds then
        Result := FY[i]
    else
        Result := 0.0;
end;

end.
