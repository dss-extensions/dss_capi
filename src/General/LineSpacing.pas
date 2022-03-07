unit LineSpacing;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

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
    TLineSpacingProp = (
        INVALID = 0,
        nconds = 1,
        nphases = 2,
        x = 3,
        h = 4,
        units = 5
    );
{$SCOPEDENUMS OFF}

    SpcParmChoice = (X, H);

    TLineSpacing = class(TDSSClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TLineSpacingObj = class(TDSSObject)
    PUBLIC
        FX: pDoubleArray;
        FY: pDoubleArray;
        FNConds: Integer;
        NPhases: Integer;
        Units: Integer;

        // CIM Accessors
        function Get_FX(i: Integer): Double;
        function Get_FY(i: Integer): Double;
        procedure Set_FX(i: Integer; Value: Double);
        procedure Set_FY(i: Integer; Value: Double);
    PUBLIC
        DataChanged: Boolean;
        constructor Create(ParClass: TDSSClass; const LineSpacingName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        // CIM XML accessors
        // TODO: remove
        property Xcoord[i: Integer]: Double READ Get_FX WRITE Set_FX;
        property Ycoord[i: Integer]: Double READ Get_FY WRITE Set_FY;
        property NWires: Integer READ FNConds;
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
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    

constructor TLineSpacing.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, DSS_OBJECT, 'LineSpacing');
end;

destructor TLineSpacing.Destroy;
begin
    inherited Destroy;
end;

procedure TLineSpacing.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);
    
    // enums
    PropertyType[ord(TProp.units)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.units)] := ptruint(@obj.Units);
    PropertyOffset2[ord(TProp.units)] := PtrInt(DSS.UnitsEnum);

    // integers
    PropertyType[ord(TProp.nphases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.nphases)] := ptruint(@obj.Nphases);
    PropertyType[ord(TProp.nconds)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.nconds)] := ptruint(@obj.FNconds);

    // arrays
    PropertyType[ord(TProp.X)] := TPropertyType.DoubleVArrayProperty;
    PropertyOffset[ord(TProp.X)] := ptruint(@obj.FX);
    PropertyOffset2[ord(TProp.X)] := ptruint(@obj.FNconds);

    PropertyType[ord(TProp.H)] := TPropertyType.DoubleVArrayProperty;
    PropertyOffset[ord(TProp.H)] := ptruint(@obj.FY);
    PropertyOffset2[ord(TProp.H)] := ptruint(@obj.FNconds);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TLineSpacing.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        DSS.ActiveDSSObject := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TLineSpacingObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    case Idx of
        ord(TProp.nconds):
        begin
            FX := Allocmem(Sizeof(FX^[1]) * FNconds);
            FY := Allocmem(Sizeof(FY^[1]) * FNconds);
            Units := UNITS_FT;
            DataChanged := TRUE;
        end;
        2..5:
            DataChanged := TRUE;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TLineSpacingObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    FNConds := Other.FNConds;
    PropertySideEffects(ord(TProp.NConds), 0);
    NPhases := Other.NPhases;
    for i := 1 to FNConds do
        FX^[i] := Other.FX^[i];
    for i := 1 to FNConds do
        FY^[i] := Other.FY^[i];
    Units := Other.Units;
    DataChanged := TRUE;
end;

constructor TLineSpacingObj.Create(ParClass: TDSSClass; const LineSpacingName: String);
begin
    inherited Create(ParClass);
    Name := AnsiLowerCase(LineSpacingName);
    DSSObjType := ParClass.DSSClassType;

    DataChanged := TRUE;
    FX := NIL;
    FY := NIL;
    units := UNITS_FT;
    FNConds := 3;
    PropertySideEffects(ord(TProp.NConds), 0);
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
        r := r + Format('%-g', [pF^[1]]);
    for i := 2 to N do
        r := r + Format(',%-g', [pF^[i]]);
    Result := r + ']';
end;

function TLineSpacingObj.Get_FX(i: Integer): Double;
begin
    if i <= FNConds then
        Result := FX^[i]
    else
        Result := 0.0;
end;

function TLineSpacingObj.Get_FY(i: Integer): Double;
begin
    if i <= FNConds then
        Result := FY^[i]
    else
        Result := 0.0;
end;

procedure TLineSpacingObj.Set_FX(i: Integer; Value: Double);
begin
    if (i > 0) and (i <= FNConds) then
        FX^[i] := Value;
end;

procedure TLineSpacingObj.Set_FY(i: Integer; Value: Double);
begin
    if (i > 0) and (i <= FNConds) then
        FY^[i] := Value;
end;

end.