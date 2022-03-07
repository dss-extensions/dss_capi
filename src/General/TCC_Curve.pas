unit TCC_Curve;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

// Nominally, a time-current curve, but also used for volt-time curves.
//
// Collections of time points.  Return values can be interpolated either
// Log-Log as traditional TCC or as over- or under-voltage definite time.

interface

uses
    Classes,
    Command,
    DSSClass,
    DSSObject,
    UcMatrix,
    Arraydef;

type
{$SCOPEDENUMS ON}
    TTCC_CurveProp = (
        INVALID = 0,
        npts = 1, // Number of points to expect
        C_array = 2, // vector of multiplier values
        T_array = 3 // vector of time values , Sec
    );
{$SCOPEDENUMS OFF}

    TTCC_Curve = class(TDSSClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TTCC_CurveObj = class(TDSSObject)
    PRIVATE
        LastValueAccessed,
        Npts: Integer;  // Number of points in curve

        Logt, LogC,        // Logarithms of t_values and c_values
        t_values,          // Time values (hr) if Interval > 0.0  Else nil
        c_values: pDoubleArray;

    PUBLIC
        constructor Create(ParClass: TDSSClass; const TCC_CurveName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        function GetTCCTime(const C_Value: Double): Double;  // Return operating time for a particular time value
        function GetUVTime(const V_Value: Double): Double;  // Return operating time for undervoltage relay
        function GetOVTime(const V_Value: Double): Double;  // Return operating time for overvoltage relay
        function Value(i: Integer): Double;  // get C_Value by index
        function Time(i: Integer): Double;  // get time value (sec) corresponding to point index

        property NumPoints: Integer READ Npts;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    UComplex, DSSUcomplex,
    MathUtil,
    Utilities,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TTCC_CurveObj;
    TProp = TTCC_CurveProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    

constructor TTCC_Curve.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, DSS_OBJECT, 'TCC_Curve');
end;

destructor TTCC_Curve.Destroy;
begin
    inherited Destroy;
end;

procedure TTCC_Curve.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // double arrays
    PropertyType[ord(TProp.C_array)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.C_array)] := ptruint(@obj.C_Values);
    PropertyOffset2[ord(TProp.C_array)] := ptruint(@obj.Npts);

    PropertyType[ord(TProp.T_array)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.T_array)] := ptruint(@obj.T_values);
    PropertyOffset2[ord(TProp.T_array)] := ptruint(@obj.Npts);

    // integer
    PropertyType[ord(TProp.npts)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.npts)] := ptruint(@obj.Npts);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TTCC_Curve.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        DSS.ActiveDSSObject := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure CalcLogPoints(const X, LogX: PDoubleArray; N: Integer);
var
    i: Integer;
begin
    for i := 1 to N do
        if X^[i] > 0.0 then
            LogX^[i] := Ln(X^[i])
        else
            LogX^[i] := Ln(0.001);
end;

procedure TTCC_CurveObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    case Idx of
        1:
        begin    // Reallocate arrays to corresponde to Npts
            ReAllocmem(C_Values, Sizeof(C_Values^[1]) * Npts);
            ReAllocmem(LogC, Sizeof(LogC^[1]) * Npts);
            ReAllocmem(T_values, Sizeof(T_values^[1]) * Npts);
            ReAllocmem(LogT, Sizeof(LogT^[1]) * Npts);
        end;
        2:
            CalcLogPoints(C_Values, LogC, Npts);
        3:
            CalcLogPoints(T_Values, LogT, Npts);
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TTCC_CurveObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    Npts := Other.Npts;
    ReAllocmem(C_Values, Sizeof(C_Values^[1]) * Npts);
    ReAllocmem(LogC, Sizeof(LogC^[1]) * Npts);
    ReAllocmem(T_values, Sizeof(T_values^[1]) * Npts);
    ReAllocmem(LogT, Sizeof(LogT^[1]) * Npts);
    for i := 1 to Npts do
        C_Values^[i] := Other.C_Values^[i];
    for i := 1 to Npts do
        T_values^[i] := Other.T_values^[i];
    for i := 1 to Npts do
        LogC^[i] := Other.LogC^[i];
    for i := 1 to Npts do
        LogT^[i] := Other.LogT^[i];
end;

constructor TTCC_CurveObj.Create(ParClass: TDSSClass; const TCC_CurveName: String);
begin
    inherited Create(ParClass);
    Name := AnsiLowerCase(TCC_CurveName);
    DSSObjType := ParClass.DSSClassType;

    LastValueAccessed := 1;
    Npts := 0;
    C_Values := NIL;
    T_Values := NIL;
    LogC := NIL;
    LogT := NIL;
end;

destructor TTCC_CurveObj.Destroy;
begin
    ReallocMem(T_Values, 0);
    ReallocMem(C_Values, 0);
    ReallocMem(LogC, 0);
    ReallocMem(LogT, 0);
    inherited destroy;
end;

function TTCC_CurveObj.GetTCCtime(const C_Value: Double): Double;
// This function returns the operation time for the value given.
// If the value is less than the first entry, return = -1 for No operation.
// Log-Log  interpolation is used.
var
    i: Integer;
    Logtest: Double;
begin
    Result := -1.0;    // default return value

    // If current is less than first point, just leave
    if C_Value < C_Values^[1] then
        Exit;


    if NPts > 0 then         // Handle Exceptional cases
        if NPts = 1 then
            Result := T_Values^[1]
        else
        begin
            // Start with previous value accessed under the assumption that most
            // of the time, this function will be called sequentially

            if C_Values^[LastValueAccessed] > C_Value then
                LastValueAccessed := 1;  // Start over from beginning
            for i := LastValueAccessed + 1 to Npts do
            begin
                if C_Values^[i] = C_Value then
                begin
                    Result := T_Values^[i];        // direct hit!
                    LastValueAccessed := i;
                    Exit;
                end

                else
                if C_Values^[i] > C_Value then
                begin   // Log-Log interpolation
                    LastValueAccessed := i - 1;
                    if C_value > 0.0 then
                        LogTest := Ln(C_Value)
                    else
                        LogTest := Ln(0.001);
                    Result := exp(LogT^[LastValueAccessed] +
                        (LogTest - LogC^[LastValueAccessed]) / (LogC^[i] - LogC^[LastValueAccessed]) *
                        (LogT^[i] - LogT^[LastValueAccessed]));
                    Exit;
                end;
            end;

            // If we fall through the loop, just use last value
            LastValueAccessed := Npts - 1;
            Result := T_Values^[Npts];
        end;
end;

function TTCC_CurveObj.GetOVTime(const V_Value: Double): Double;
// Over-voltage, definite time relay
var
    i: Integer;
begin
    result := -1.0;  // No op return

    if V_Value > C_Values^[1] then
    begin
        if Npts = 1 then
            Result := T_Values^[1]
        else
        begin
            i := 1;
            while C_Values^[i] < V_Value do
            begin
                inc(i);
                if i > Npts then
                    Break;
            end;
            Result := T_Values^[i - 1];
        end;
    end;
end;

function TTCC_CurveObj.GetUVTime(const V_Value: Double): Double;
// Under-voltage, definite time relay
var
    i: Integer;
begin
    result := -1.0;  // No op return

    if V_Value < C_Values^[Npts] then
    begin
        if Npts = 1 then
            Result := T_Values^[1]
        else
        begin
            i := Npts;
            while C_Values^[i] > V_Value do
            begin
                dec(i);
                if i = 0 then
                    Break;
            end;
            Result := T_Values^[i + 1];
        end;
    end;
end;

function TTCC_CurveObj.Value(i: Integer): Double;
begin
    if (i <= Npts) and (i > 0) then
    begin
        Result := C_Values^[i];
        LastValueAccessed := i;
    end
    else
        Result := 0.0;
end;

function TTCC_CurveObj.Time(i: Integer): Double;
begin
    if (i <= Npts) and (i > 0) then
    begin
        Result := T_Values^[i];
        LastValueAccessed := i;
    end
    else
        Result := 0.0;
end;

end.