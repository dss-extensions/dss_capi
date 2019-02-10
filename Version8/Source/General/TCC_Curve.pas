unit TCC_Curve;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{ Created 8-25-00 }

{
 Nominally, a time-current curve, but also used for volt-time curves.

 Collections of time points.  Return values can be interpolated either
 Log-Log as traditional TCC or as over- or under-voltage definite time.
}

interface

uses
    Command,
    DSSClass,
    DSSObject,
    UcMatrix,
    Arraydef;

type

    TTCC_Curve = class(TDSSClass)
    PRIVATE

        function Get_Code: String;  // Returns active TCC_Curve string
        procedure Set_Code(const Value: String);  // sets the  active TCC_Curve

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const ShapeName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;     // uses global parser
        function Init(Handle: Integer; ActorID: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;


       // Set this property to point ActiveTCC_CurveObj to the right value
        property Code: String READ Get_Code WRITE Set_Code;


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

        function GetTCCTime(const C_Value: Double): Double;  // Return operating time for a particular time value
        function GetUVTime(const V_Value: Double): Double;  // Return operating time for undervoltage relay
        function GetOVTime(const V_Value: Double): Double;  // Return operating time for overvoltage relay
        function Value(i: Integer): Double;  // get C_Value by index
        function Time(i: Integer): Double;  // get time value (sec) corresponding to point index

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

        property NumPoints: Integer READ Npts;

    end;

var
    ActiveTCC_CurveObj: TTCC_CurveObj;

implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    Ucomplex,
    MathUtil,
    Utilities;

const
    NumPropsThisClass = 3;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TTCC_Curve.Create;  // Creates superstructure for all Line objects
begin
    inherited Create;
    Class_Name := 'TCC_Curve';
    DSSClassType := DSS_OBJECT;

    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TTCC_Curve.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;
end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTCC_Curve.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


     // Define Property names
    PropertyName[1] := 'npts';     // Number of points to expect
    PropertyName[2] := 'C_array';     // vector of multiplier values
    PropertyName[3] := 'T_array';     // vextor of time values , Sec

     // define Property help values

    PropertyHelp[1] := 'Number of points to expect in time-current arrays.';     // Number of points to expect
    PropertyHelp[2] := 'Array of current (or voltage) values corresponding to time values (see help on T_Array).';     // vector of multiplier values
    PropertyHelp[3] := 'Array of time values in sec. Typical array syntax: ' + CRLF +
        't_array = (1, 2, 3, 4, ...)' + CRLF + CRLF +
        'Can also substitute a file designation: ' + CRLF +
        't_array =  (file=filename)' + CRLF + CRLF +
        'The specified file has one value per line.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TTCC_Curve.NewObject(const ObjName: String): Integer;
begin
   // create a new object of this class and add to list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveDSSObject[ActiveActor] := TTCC_CurveObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TTCC_Curve.Edit(ActorID: Integer): Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

begin
    Result := 0;
  // continue parsing with contents of Parser
    ActiveTCC_CurveObj := ElementList.Active;
    ActiveDSSObject[ActorID] := ActiveTCC_CurveObj;

    with ActiveTCC_CurveObj do
    begin

        ParamPointer := 0;
        ParamName := Parser[ActorID].NextParam;
        Param := Parser[ActorID].StrValue;
        while Length(Param) > 0 do
        begin
            if Length(ParamName) = 0 then
                Inc(ParamPointer)
            else
                ParamPointer := CommandList.GetCommand(ParamName);

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[ParamPointer] := Param;

            case ParamPointer of
                0:
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 420);
                1:
                    Npts := Parser[ActorID].Intvalue;
                2:
                    InterpretDblArray(Param, Npts, C_Values);   // Parser.ParseAsVector(Npts, Multipliers);
                3:
                    InterpretDblArray(Param, Npts, T_values);   // Parser.ParseAsVector(Npts, Hours);
            else
           // Inherited parameters
                ClassEdit(ActiveTCC_CurveObj, ParamPointer - NumPropsThisClass)
            end;

            case ParamPointer of
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

            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end; {WHILE}
    end; {WITH}
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TTCC_Curve.MakeLike(const ShapeName: String): Integer;
var
    OtherTCC_Curve: TTCC_CurveObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this line code in the present collection}
    OtherTCC_Curve := Find(ShapeName);
    if OtherTCC_Curve <> NIL then
        with ActiveTCC_CurveObj do
        begin
            Npts := OtherTCC_Curve.Npts;
            ReAllocmem(C_Values, Sizeof(C_Values^[1]) * Npts);
            ReAllocmem(LogC, Sizeof(LogC^[1]) * Npts);
            ReAllocmem(T_values, Sizeof(T_values^[1]) * Npts);
            ReAllocmem(LogT, Sizeof(LogT^[1]) * Npts);
            for i := 1 to Npts do
                C_Values^[i] := OtherTCC_Curve.C_Values^[i];
            for i := 1 to Npts do
                T_values^[i] := OtherTCC_Curve.T_values^[i];
            for i := 1 to Npts do
                LogC^[i] := OtherTCC_Curve.LogC^[i];
            for i := 1 to Npts do
                LogT^[i] := OtherTCC_Curve.LogT^[i];

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherTCC_Curve.PropertyValue[i];
        end
    else
        DoSimpleMsg('Error in TCC_Curve MakeLike: "' + ShapeName + '" Not Found.', 421);


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TTCC_Curve.Init(Handle: Integer; ActorID: Integer): Integer;

begin
    DoSimpleMsg('Need to implement TTCC_Curve.Init', -1);
    REsult := 0;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TTCC_Curve.Get_Code: String;  // Returns active line code string
var
    TCC_CurveObj: TTCC_CurveObj;

begin

    TCC_CurveObj := ElementList.Active;
    Result := TCC_CurveObj.Name;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTCC_Curve.Set_Code(const Value: String);  // sets the  active TCC_Curve

var
    TCC_CurveObj: TTCC_CurveObj;

begin

    ActiveTCC_CurveObj := NIL;
    TCC_CurveObj := ElementList.First;
    while TCC_CurveObj <> NIL do
    begin

        if CompareText(TCC_CurveObj.Name, Value) = 0 then
        begin
            ActiveTCC_CurveObj := TCC_CurveObj;
            Exit;
        end;

        TCC_CurveObj := ElementList.Next;
    end;

    DoSimpleMsg('TCC_Curve: "' + Value + '" not Found.', 422);

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TTCC_Curve Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TTCC_CurveObj.Create(ParClass: TDSSClass; const TCC_CurveName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(TCC_CurveName);
    DSSObjType := ParClass.DSSClassType;

    LastValueAccessed := 1;
    Npts := 0;
    C_Values := NIL;
    T_Values := NIL;
    LogC := NIL;
    LogT := NIL;

    InitPropertyValues(0);

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TTCC_CurveObj.Destroy;
begin

    ReallocMem(T_Values, 0);
    ReallocMem(C_Values, 0);
    ReallocMem(LogC, 0);
    ReallocMem(LogT, 0);
    inherited destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TTCC_CurveObj.GetTCCtime(const C_Value: Double): Double;

// This function returns the operation time for the value given.
// If the value is less than the first entry, return = -1 for No operation.
// Log-Log  interpolation is used.

var
    i: Integer;
    Logtest: Double;

begin

    Result := -1.0;    // default return value

  {If current is less than first point, just leave}
    if C_Value < C_Values^[1] then
        Exit;


    if NPts > 0 then         // Handle Exceptional cases
        if NPts = 1 then
            Result := T_Values^[1]
        else
        begin

      { Start with previous value accessed under the assumption that most
        of the time, this function will be called sequentially}

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


procedure TTCC_CurveObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
        end;
end;

function TTCC_CurveObj.GetPropertyValue(Index: Integer): String;
begin
    Result := '';

    case Index of
        2:
            Result := GetDSSArray_Real(Npts, C_Values);
        3:
            Result := GetDSSArray_Real(Npts, T_Values);
    else
        Result := inherited GetPropertyValue(index);
    end;
end;

procedure TTCC_CurveObj.InitPropertyValues(ArrayOffset: Integer);
begin
    PropertyValue[1] := '0';     // Number of points to expect
    PropertyValue[2] := '';     // vector of multiplier values
    PropertyValue[3] := '';     // vextor of sec values

    inherited InitPropertyValues(NumPropsThisClass);

end;

end.
