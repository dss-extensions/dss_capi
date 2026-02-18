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
        units = 5,
        detailed = 6,
        EqDistPhPh = 7,
        EqDistPhN = 8,
        AvgPhaseHeight = 9,
        AvgNeutralHeight = 10
    );
    TLineSpacingProp = (
        INVALID = 0,
        NConds = 1,
        NPhases = 2,
        X = 3,
        H = 4,
        Units = 5,
        Detailed = 6,
        EqDistPhPh = 7,
        EqDistPhN = 8,
        AvgPhaseHeight = 9,
        AvgNeutralHeight = 10
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

        detailed: LongBool;
        eqDistPhPh,
        eqDistPhN,
        avgPhaseHeight,
        avgNeutralHeight: Double;

        // CIM Accessors
        function GetXCoord(i: Integer): Double;
        function GetYCoord(i: Integer): Double;
    PUBLIC
        constructor Create(ParClass: TDSSClass; const LineSpacingName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags); override;
        procedure MakeLike(OtherPtr: Pointer); override;
        function EquivalentSpacing(): Boolean;
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
    PropertyOffset[ord(TProp.units)] := PtrInt(@obj.Units);
    PropertyOffset2[ord(TProp.units)] := PtrInt(DSS.UnitsEnum);

    // boolean
    PropertyType[ord(TProp.Detailed)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.Detailed)] := PtrInt(@obj.detailed);

    // integers
    PropertyType[ord(TProp.nphases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.nphases)] := PtrInt(@obj.Nphases);

    PropertyType[ord(TProp.nconds)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.nconds)] := PtrInt(@obj.NConds);
    PropertyFlags[ord(TProp.nconds)] := [TPropertyFlag.SuppressJSON];

    // arrays
    PropertyType[ord(TProp.X)] := TPropertyType.DoubleVArrayProperty;
    PropertyOffset[ord(TProp.X)] := PtrInt(@obj.FX);
    PropertyOffset2[ord(TProp.X)] := PtrInt(@obj.NConds);

    PropertyType[ord(TProp.H)] := TPropertyType.DoubleVArrayProperty;
    PropertyOffset[ord(TProp.H)] := PtrInt(@obj.FY);
    PropertyOffset2[ord(TProp.H)] := PtrInt(@obj.NConds);

    // doubles (default type)
    PropertyOffset[ord(TProp.EqDistPhPh)] := PtrInt(@obj.eqDistPhPh);
    PropertyOffset[ord(TProp.EqDistPhN)] := PtrInt(@obj.eqDistPhN);
    PropertyOffset[ord(TProp.AvgPhaseHeight)] := PtrInt(@obj.avgPhaseHeight);
    PropertyOffset[ord(TProp.AvgNeutralHeight)] := PtrInt(@obj.avgNeutralHeight);

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
        ord(TProp.NConds):
        begin
            ReAllocmem(FX, Sizeof(Double) * NConds);
            ReAllocmem(FY, Sizeof(Double) * NConds);
            Units := UNITS_FT;
        end;
        ord(TProp.Detailed):
        begin
            // use Detailed to clear the unused properties
            if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.NoPropertyTracking)) = 0 then
            begin
                if detailed then
                begin
                    // Using detailed distances, clear the equivalent data in EqDistPhPh, EqDistPhN, AvgPhaseHeight, AvgNeutralHeight
                    PrpSequence[ord(TProp.EqDistPhPh)] := 0;
                    PrpSequence[ord(TProp.EqDistPhN)] := 0;
                    PrpSequence[ord(TProp.AvgPhaseHeight)] := 0;
                    PrpSequence[ord(TProp.AvgNeutralHeight)] := 0;
                end
                else
                begin
                    // Using equivalent distances, clear X and H
                    PrpSequence[ord(TProp.X)] := 0;
                    PrpSequence[ord(TProp.H)] := 0;
                end;
            end;
        end;
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
end;

constructor TLineSpacingObj.Create(ParClass: TDSSClass; const LineSpacingName: String);
var
    i: Integer;
begin
    inherited Create(ParClass, LineSpacingName);
    DSSObjType := ParClass.DSSClassType;

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
    eqDistPhPh := 0.0;
    eqDistPhN := 0.0;
    avgPhaseHeight := 0.0;
    avgNeutralHeight := 0.0;
    detailed := true;
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

function TLineSpacingObj.EquivalentSpacing(): Boolean;
begin
    Result := not detailed;
end;

end.
