unit Capacitor;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{   4-17-00  Made IsShunt Public
    12-7-04 Added Reactance in series with each capacitor

}
{Basic  capacitor

  Implemented as a two-terminal constant impedance (Power Delivery Element)

  Bus2 connection defaults to 0 node of Bus1 (if Bus2 has the default bus connection
  at the time Bus1 is defined.  Therefore, if only Bus1 is specified, a shunt capacitor results.
  If delta connected, Bus2 is set to node zero of Bus1 and nothing is returned in the lower
  half of YPrim - all zeroes.

  If an ungrounded wye is desired, explicitly set Bus2= and set all nodes the same,
    e.g. Bus1.4.4.4   (uses 4th node of Bus1 as neutral point)
        or BusNew.1.1.1  (makes a new bus for the neutral point)
  You must specify the nodes or you will get a series capacitor!

  A series capacitor is specified simply by setting bus2 and declaring the connection
  to be Wye.  If the connection is specified as delta, nothing will be connected to Bus2.
  In fact the number of terminals is set to 1.

  Capacitance may be specified as:

     1.  kvar and kv ratings at base frequency.  impedance.  Specify kvar as total for
         all phases (all cans assumed equal). For 1-phase, kV = capacitor can kV rating.
         For 2 or 3-phase, kV is line-line three phase. For more than 3 phases, specify
         kV as actual can voltage.
     2.  Capacitance in uF to be used in each phase.  If specified in this manner,
         the given value is always used whether wye or delta.
     3.  A nodal C matrix (like a nodal admittance matrix).
         If conn=wye then 2-terminal through device
         If conn=delta then 1-terminal.
         Microfarads.

}
interface

uses
    Command,
    DSSClass,
    PDClass,
    PDElement,
    UcMatrix,
    ArrayDef;

type

    TCapacitor = class(TPDClass)
    PRIVATE
        procedure DoCmatrix(ActorID: Integer);

        procedure InterpretConnection(const S: String);
        procedure CapSetBus1(const s: String);
    PROTECTED
        function MakeLike(const CapacitorName: String): Integer; OVERRIDE;
        procedure DefineProperties;  // Add Properties of this class to propName
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;     // uses global parser
        function Init(Handle: Integer; ActorID: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;
    end;

    TCapacitorObj = class(TPDElement)
{$IFDEF DSS_CAPI}
    PUBLIC
{$ELSE}
    PRIVATE
{$ENDIF}
        FC,
        FXL,
        Fkvarrating,
        FR,
        FHarm: pDoubleArray;  // single C per phase (line rating) if Cmatrix not specified
        FStates: pIntegerArray;

        Ftotalkvar,
        kvrating: Double;
        FNumSteps,
        FLastStepInService: Integer;
        Cmatrix: pDoubleArray;  // If not nil then overrides C

        DoHarmonicRecalc: Boolean;
        Bus2Defined: Boolean;

        SpecType: Integer;
        NumTerm: Integer;   // Flag used to indicate The number of terminals

        function get_States(Idx: Integer; ActorID: Integer): Integer;
        procedure set_States(Idx: Integer; ActorID: Integer; const Value: Integer);
        procedure set_LastStepInService(const Value: Integer);

        procedure ProcessHarmonicSpec(const Param: String);
        procedure ProcessStatesSpec(const Param: String);
        procedure MakeYprimWork(YprimWork: TcMatrix; iStep: Integer; ActorID: Integer);

        procedure set_NumSteps(const Value: Integer); // 1=kvar, 2=Cuf, 3=Cmatrix


    PUBLIC

        Connection: Integer;   // 0 or 1 for wye (default) or delta, respectively
        constructor Create(ParClass: TDSSClass; const CapacitorName: String);
        destructor Destroy; OVERRIDE;

        procedure RecalcElementData(ActorID: Integer); OVERRIDE;
        procedure CalcYPrim(ActorID: Integer); OVERRIDE;

        procedure MakePosSequence(ActorID: Integer); OVERRIDE;  // Make a positive Sequence Model

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
        function GetPropertyValue(Index: Integer): String; OVERRIDE;

        function AddStep(ActorID: Integer): Boolean;
        function SubtractStep(ActorID: Integer): Boolean;
        function AvailableSteps: Integer;
        procedure FindLastStepInService;
        property NumSteps: Integer READ FNumSteps WRITE set_NumSteps;
        property States[Idx: Integer;ActorID: Integer]: Integer READ get_States WRITE set_States;
        property Totalkvar: Double READ FTotalkvar;
        property NomKV: Double READ kvrating;
        property LastStepInService: Integer READ FLastStepInService WRITE set_LastStepInService;

        property NumTerminals: Integer READ NumTerm;   // Property to know if the capacitor has 2 terminals

    end;

var
    ActiveCapacitorObj: TCapacitorObj;

implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    Ucomplex,
    Utilities;

const
    NumPropsThisClass = 13;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TCapacitor.Create;  // Creates superstructure for all Capacitor objects
begin
    inherited Create;
    Class_Name := 'Capacitor';
    DSSClassType := DSSClassType + CAP_ELEMENT;

    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TCapacitor.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TCapacitor.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


     // Define Property names
    PropertyName^[1] := 'bus1';
    PropertyName^[2] := 'bus2';
    PropertyName^[3] := 'phases';
    PropertyName^[4] := 'kvar';
    PropertyName^[5] := 'kv';
    PropertyName^[6] := 'conn';
    PropertyName^[7] := 'cmatrix';
    PropertyName^[8] := 'cuf';
    PropertyName^[9] := 'R';
    PropertyName^[10] := 'XL';
    PropertyName^[11] := 'Harm';
    PropertyName^[12] := 'Numsteps';
    PropertyName^[13] := 'states';

     // define Property help values

    PropertyHelp^[1] := 'Name of first bus of 2-terminal capacitor. Examples:' + CRLF +
        'bus1=busname' + CRLF + 'bus1=busname.1.2.3' + CRLF + CRLF +
        'If only one bus specified, Bus2 will default to this bus, Node 0, ' +
        'and the capacitor will be a Yg shunt bank.';
    PropertyHelp^[2] := 'Name of 2nd bus. Defaults to all phases connected ' +
        'to first bus, node 0, (Shunt Wye Connection) ' +
        'except when Bus2 explicitly specified. ' + CRLF + CRLF +
        'Not necessary to specify for delta (LL) connection.';
    PropertyHelp^[3] := 'Number of phases.';
    PropertyHelp^[4] := 'Total kvar, if one step, or ARRAY of kvar ratings for each step.  Evenly divided among phases. See rules for NUMSTEPS.';
    PropertyHelp^[5] := 'For 2, 3-phase, kV phase-phase. Otherwise specify actual can rating.';
    PropertyHelp^[6] := '={wye | delta |LN |LL}  Default is wye, which is equivalent to LN';
    PropertyHelp^[7] := 'Nodal cap. matrix, lower triangle, microfarads, of the following form:' + CRLF + CRLF +
        'cmatrix="c11 | -c21 c22 | -c31 -c32 c33"' + CRLF + CRLF +
        'All steps are assumed the same if this property is used.';
    PropertyHelp^[8] := 'ARRAY of Capacitance, each phase, for each step, microfarads.' + CRLF +
        'See Rules for NumSteps.';
    PropertyHelp^[9] := 'ARRAY of series resistance in each phase (line), ohms. Default is 0.0';
    PropertyHelp^[10] := 'ARRAY of series inductive reactance(s) in each phase (line) for filter, ohms at base frequency. Use this OR "h" property to define filter. Default is 0.0.';
    PropertyHelp^[11] := 'ARRAY of harmonics to which each step is tuned. Zero is interpreted as meaning zero reactance (no filter). Default is zero.';
    PropertyHelp^[12] := 'Number of steps in this capacitor bank. Default = 1. Forces reallocation of the capacitance, reactor, and states array.  Rules: ' +
        'If this property was previously =1, the value in the kvar property is divided equally among the steps. The kvar property ' +
        'does not need to be reset if that is accurate.  If the Cuf or Cmatrix property was used previously, all steps are set to the value of the first step. ' +
        'The states property is set to all steps on. All filter steps are set to the same harmonic. ' +
        'If this property was previously >1, the arrays are reallocated, but no values are altered. You must SUBSEQUENTLY assign all array properties.';
    PropertyHelp^[13] := 'ARRAY of integers {1|0} states representing the state of each step (on|off). Defaults to 1 when reallocated (on). ' +
        'Capcontrol will modify this array as it turns steps on or off.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TCapacitor.NewObject(const ObjName: String): Integer;
begin
   // create a new object of this class and add to list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveCktElement := TCapacitorObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TCapacitor.DoCmatrix(ActorID: Integer);
var
    OrderFound, j: Integer;
    MatBuffer: pDoubleArray;

begin
    with ActiveCapacitorObj do
    begin
        MatBuffer := Allocmem(Sizeof(Double) * Fnphases * Fnphases);
        OrderFound := Parser[ActorID].ParseAsSymMatrix(Fnphases, MatBuffer);

        if OrderFound > 0 then    // Parse was successful
        begin    {C}
            Reallocmem(Cmatrix, Sizeof(Cmatrix^[1]) * Fnphases * Fnphases);
            for j := 1 to Fnphases * Fnphases do
                Cmatrix^[j] := 1.0e-6 * MatBuffer^[j];
        end;

        Freemem(MatBuffer, Sizeof(Double) * Fnphases * Fnphases);
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TCapacitor.InterpretConnection(const S: String);

// Accepts
//    delta or LL           (Case insensitive)
//    Y, wye, or LN
var
    TestS: String;

begin
    with ActiveCapacitorObj do
    begin
        TestS := lowercase(S);
        case TestS[1] of
            'y', 'w':
                Connection := 0;  {Wye}
            'd':
                Connection := 1;  {Delta or line-Line}
            'l':
                case Tests[2] of
                    'n':
                        Connection := 0;
                    'l':
                        Connection := 1;
                end;

        end;
        case Connection of
            1:
                Nterms := 1;  // Force reallocation of terminals
            0:
                if Fnterms <> 2 then
                    Nterms := 2;
        end;
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TCapacitor.CapSetBus1(const s: String);

var
    s2: String;
    i, dotpos: Integer;

   // Special handling for Bus 1
   // Set Bus2 = Bus1.0.0.0

begin
    with ActiveCapacitorObj do
    begin
        SetBus(1, S);

     // Default Bus2 to zero node of Bus1 unless it is previously defined. (Grounded-Y connection)

        if not Bus2Defined and (Fnterms = 2) then   // only for WYE connection
        begin
       // Strip node designations from S
            dotpos := Pos('.', S);
            if dotpos > 0 then
                S2 := Copy(S, 1, dotpos - 1)
            else
                S2 := Copy(S, 1, Length(S));  // copy up to Dot
            for i := 1 to Fnphases do
                S2 := S2 + '.0';   // append series of ".0"'s

            SetBus(2, S2);    // default setting for Bus2
            IsShunt := TRUE;
        end;
    end;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TCapacitor.Edit(ActorID: Integer): Integer;

var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    i: Integer;

begin
    Result := 0;
  // continue parsing with contents of Parser
    ActiveCapacitorObj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActiveCapacitorObj;  // use property to set this value


    with ActiveCapacitorObj do
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "Capacitor.' + Name + '"', 450);
                1:
                    CapSetbus1(param);
                2:
                begin
                    Setbus(2, param);
                    NumTerm := 2;    // Specifies that the capacitor is not connected to ground
                end;
                3:
{ Numphases := Parser.IntValue};  // see below
                4:
                    FNumSteps := InterpretDblArray(Param, FNumSteps, FkvarRating);
                5:
                    kvRating := Parser[ActorID].Dblvalue;
                6:
                    InterpretConnection(Param);
                7:
                    DoCMatrix(ActorID);
                8:
                    FNumSteps := InterpretDblArray(Param, FNumSteps, FC);
                9:
                    FNumSteps := InterpretDblArray(Param, FNumSteps, FR);
                10:
                    FNumSteps := InterpretDblArray(Param, FNumSteps, FXL);
                11:
                    ProcessHarmonicSpec(Param);
                12:
                    NumSteps := Parser[ActorID].IntValue;
                13:
                    ProcessStatesSpec(Param);
            else
            // Inherited Property Edits
                ClassEdit(ActiveCapacitorObj, ParamPointer - NumPropsThisClass)
            end;

         // Some specials ...
            case ParamPointer of
                1:
                begin
                    PropertyValue[2] := GetBus(2);   // this gets modified
                    PrpSequence^[2] := 0; // Reset this for save function
                end;
                2:
                    if CompareText(StripExtension(GetBus(1)), StripExtension(GetBus(2))) <> 0 then
                    begin
                        IsShunt := FALSE;
                        Bus2Defined := TRUE;
                    end;
                3:
                    if Fnphases <> Parser[ActorID].IntValue then
                    begin
                        Nphases := Parser[ActorID].IntValue;
                        NConds := Fnphases;  // Force Reallocation of terminal info
                        Yorder := Fnterms * Fnconds;
                    end;
                4:
                    SpecType := 1;
                7:
                    SpecType := 3;
                8:
                begin
                    SpecType := 2;
                    for i := 1 to Fnumsteps do
                        FC^[i] := FC^[i] * 1.0e-6;
                end;
                10:
                begin
                    for i := 1 to Fnumsteps do
                        if FXL^[i] <> 0.0 then
                            if FR^[i] = 0.0 then
                                FR^[i] := Abs(FXL^[i]) / 1000.0;  // put in something so it doesn't fail
                    DoHarmonicRecalc := FALSE;  // XL is specified
                end;
            else
            end;

         //YPrim invalidation on anything that changes impedance values
            case ParamPointer of
                3..8:
                    YprimInvalid[ActorID] := TRUE;
                12, 13:
                    YprimInvalid[ActorID] := TRUE;
            else
            end;


            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;

        RecalcElementData(ActorID);
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TCapacitor.MakeLike(const CapacitorName: String): Integer;
var
    OtherCapacitor: TCapacitorObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this Capacitor name in the present collection}
    OtherCapacitor := Find(CapacitorName);
    if OtherCapacitor <> NIL then
        with ActiveCapacitorObj do
        begin

            if Fnphases <> OtherCapacitor.Fnphases then
            begin
                NPhases := OtherCapacitor.Fnphases;
                NConds := Fnphases; // force reallocation of terminals and conductors

                Yorder := Fnconds * Fnterms;
                YprimInvalid[ActiveActor] := TRUE;

            end;

            NumSteps := OtherCapacitor.NumSteps;

            for i := 1 to FNumSteps do
            begin
                FC^[i] := OtherCapacitor.FC^[i];
                Fkvarrating^[i] := OtherCapacitor.Fkvarrating^[i];
                FR^[i] := OtherCapacitor.FR^[i];
                FXL^[i] := OtherCapacitor.FXL^[i];
                FXL^[i] := OtherCapacitor.FXL^[i];
                FHarm^[i] := OtherCapacitor.FHarm^[i];
                Fstates^[i] := OtherCapacitor.Fstates^[i];
            end;

            kvrating := OtherCapacitor.kvrating;
            Connection := OtherCapacitor.Connection;
            SpecType := OtherCapacitor.SpecType;

            if OtherCapacitor.Cmatrix = NIL then
                Reallocmem(Cmatrix, 0)
            else
            begin
                Reallocmem(Cmatrix, SizeOf(Cmatrix^[1]) * Fnphases * Fnphases);
                for i := 1 to Fnphases * Fnphases do
                    Cmatrix^[i] := OtherCapacitor.Cmatrix^[i];
            end;

            ClassMakeLike(OtherCapacitor);  // Take care of inherited class properties

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherCapacitor.PropertyValue[i];
            Result := 1;
        end
    else
        DoSimpleMsg('Error in Capacitor MakeLike: "' + CapacitorName + '" Not Found.', 451);


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TCapacitor.Init(Handle: Integer; ActorID: Integer): Integer;

begin
    DoSimpleMsg('Need to implement TCapacitor.Init', 452);
    Result := 0;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TCapacitor Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCapacitorObj.Create(ParClass: TDSSClass; const CapacitorName: String);


begin
    inherited Create(ParClass);
    Name := LowerCase(CapacitorName);
    DSSObjType := ParClass.DSSClassType;

    NPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 2;  // Force allocation of terminals and conductors

    Setbus(2, (GetBus(1) + '.0.0.0'));  // Default to grounded wye

    IsShunt := TRUE;  // defaults to shunt capacitor

    Cmatrix := NIL;

     {Initialize these pointers to Nil so reallocmem will work reliably}
    FC := NIL;
    FXL := NIL;
    Fkvarrating := NIL;
    FR := NIL;
    FHarm := NIL;
    FStates := NIL;

    NumSteps := 1;  // Initial Allocation for the Arrays, too
    LastStepInService := FNumSteps;

    InitDblArray(FNumSteps, FR, 0.0);
    InitDblArray(FNumSteps, FXL, 0.0);
    InitDblArray(FNumSteps, FHarm, 0.0);
    InitDblArray(FNumSteps, Fkvarrating, 1200.0);

    Fstates^[1] := 1;

    kvrating := 12.47;
    InitDblArray(FNumSteps, FC, 1.0 / (TwoPi * BaseFrequency * SQR(kvrating) * 1000.0 / Fkvarrating^[1]));

    Connection := 0;   // 0 or 1 for wye (default) or delta, respectively
    SpecType := 1; // 1=kvar, 2=Cuf, 3=Cmatrix

    NormAmps := FkvarRating^[1] * SQRT3 / kvrating * 1.35;   // 135%
    EmergAmps := NormAmps * 1.8 / 1.35;   //180%
    FaultRate := 0.0005;
    PctPerm := 100.0;
    HrsToRepair := 3.0;
    Yorder := Fnterms * Fnconds;

    DoHarmonicRecalc := FALSE;
    Bus2Defined := FALSE;

    RecalcElementData(ActiveActor);
    NumTerm := 1;

    InitPropertyValues(0);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TCapacitorObj.Destroy;
begin
    ReallocMem(Cmatrix, 0);

    Reallocmem(FC, 0);
    Reallocmem(FXL, 0);
    Reallocmem(Fkvarrating, 0);
    Reallocmem(FR, 0);
    Reallocmem(FHarm, 0);
    Reallocmem(FStates, 0);

    inherited destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TCapacitorObj.RecalcElementData(ActorID: Integer);
var
    KvarPerPhase, PhasekV, w: Double;
    i: Integer;

begin
    Ftotalkvar := 0.0;
    PhasekV := 1.0;
    w := TwoPi * BaseFrequency;
    case SpecType of

        1:
        begin // kvar

            case Connection of
                1:
                begin  // Line-to-Line
                    PhasekV := kVRating;
                end;
            else
            begin  //  line-to-neutral
                case Fnphases of
                    2, 3:
                        PhasekV := kVRating / SQRT3;  // Assume three phase system
                else
                    PhasekV := kVRating;
                end;
            end;
            end;

            for i := 1 to FNumSteps do
                FC^[i] := 1.0 / (w * SQR(PhasekV) * 1000.0 / (FkvarRating^[1] / Fnphases));
            for i := 1 to FNumSteps do
                Ftotalkvar := Ftotalkvar + FkvarRating^[i];
        end;
        2:
        begin // Cuf
            case Connection of
                1:
                begin  // Line-to-Line
                    PhasekV := kVRating;
                end;
            else
            begin  //  line-to-neutral
                case Fnphases of
                    2, 3:
                        PhasekV := kVRating / SQRT3;  // Assume three phase system
                else
                    PhasekV := kVRating;
                end;
            end;
            end;
            for i := 1 to FNumSteps do
                Ftotalkvar := Ftotalkvar + w * FC^[i] * SQR(PhasekV) / 1000.0;
        end;
        3:
        begin // Cmatrix
           // Nothing to do

        end;
    end;

    if DoHarmonicRecalc then  // If harmonic specified, compute filter reactance
        for i := 1 to FNumsteps do
        begin
            if FHarm^[i] <> 0.0 then
                FXL^[i] := (1.0 / (w * FC^[i])) / SQR(FHarm^[i])
            else
                FXL^[i] := 0.0;   // Assume 0 harmonic means no filter
            if FR^[i] = 0.0 then
                FR^[i] := FXL^[i] / 1000.0;
        end;


    kvarPerPhase := Ftotalkvar / Fnphases;
    NormAmps := kvarPerPhase / PhasekV * 1.35;
    EmergAmps := NormAmps * 1.8 / 1.35;


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TCapacitorObj.CalcYPrim(ActorID: Integer);

var
    i: Integer;
    YPrimTemp, YPrimWork: TCMatrix;

begin

// Normally build only Yprim Shunt, but if there are 2 terminals and
// Bus1 <> Bus 2


    if YprimInvalid[ActorID] then
    begin    // Reallocate YPrim if something has invalidated old allocation
        if YPrim_Shunt <> NIL then
            YPrim_Shunt.Free;
        YPrim_Shunt := TcMatrix.CreateMatrix(Yorder);
        if Yprim_Series <> NIL then
            Yprim_Series.Free;
        Yprim_Series := TcMatrix.CreateMatrix(Yorder);
        if YPrim <> NIL then
            YPrim.Free;
        YPrim := TcMatrix.CreateMatrix(Yorder);
    end
    else
    begin
        YPrim_Series.Clear; // zero out YPrim
        YPrim_Shunt.Clear; // zero out YPrim
        Yprim.Clear;
    end;

    if IsShunt then
        YPrimTemp := YPrim_Shunt
    else
        YPrimTemp := Yprim_Series;

    YPrimWork := TcMatrix.CreateMatrix(Yorder);

    for i := 1 to FNumSteps do
        if FStates^[i] = 1 then
        begin
            MakeYprimWork(YprimWork, i, ActorID);
            YprimTemp.AddFrom(YprimWork);
        end;

    YPrimWork.Free;

   // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages doesn't fail
    if IsShunt then
        for i := 1 to Yorder do
            Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));


    Yprim.Copyfrom(YPrimTemp);

    {Don't Free YPrimTemp - It's just a pointer to an existing complex matrix}

    inherited CalcYPrim(ActorID);

    YprimInvalid[ActorID] := FALSE;
end;

procedure TCapacitorObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i, j: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
    begin
        Writeln(F, '~ ', PropertyName^[1], '=', firstbus);
        Writeln(F, '~ ', PropertyName^[2], '=', nextbus);

        Writeln(F, '~ ', PropertyName^[3], '=', Fnphases: 0);
        Writeln(F, '~ ', PropertyName^[4], '=', GetPropertyValue(4));

        Writeln(F, '~ ', PropertyName^[5], '=', kVRating: 0: 3);
        case Connection of
            0:
                Writeln(F, '~ ', PropertyName^[6], '=wye');
            1:
                Writeln(F, '~ ', PropertyName^[6], '=delta');
        end;
        if Cmatrix <> NIL then
        begin
            Write(F, PropertyName^[7], '= (');
            for i := 1 to Fnphases do
            begin
                for j := 1 to i do
                    Write(F, (CMatrix^[(i - 1) * Fnphases + j] * 1.0e6): 0: 3, ' ');
                if i <> Fnphases then
                    Write(F, '|');
            end;
            Writeln(F, ')');
        end;

        Writeln(F, '~ ', PropertyName^[8], '=', GetPropertyValue(8));
        Writeln(F, '~ ', PropertyName^[9], '=', GetPropertyValue(9));
        Writeln(F, '~ ', PropertyName^[10], '=', GetPropertyValue(10));
        Writeln(F, '~ ', PropertyName^[11], '=', GetPropertyValue(11));
        Writeln(F, '~ ', PropertyName^[12], '=', FNumSteps);
        Writeln(F, '~ ', PropertyName^[13], '=', GetPropertyValue(13));

        for i := NumPropsthisClass + 1 to NumProperties do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
        end;

        if Complete then
        begin
            Writeln(F, 'SpecType=', SpecType: 0);
        end;
    end;

end;


procedure TCapacitorObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := GetBus(1);
    PropertyValue[2] := GetBus(2);
    PropertyValue[3] := '3';
    PropertyValue[4] := '1200';
    PropertyValue[5] := '12.47';
    PropertyValue[6] := 'wye';
    PropertyValue[7] := '';
    PropertyValue[8] := '';
    PropertyValue[9] := '0';
    PropertyValue[10] := '0';
    PropertyValue[11] := '0';
    PropertyValue[12] := '1';
    PropertyValue[13] := '1'; // states


    inherited  InitPropertyValues(NumPropsThisClass);

       // Override Inherited properties
       //  Override Inherited properties
    PropertyValue[NumPropsThisClass + 1] := Format('%g', [Normamps]);
    PropertyValue[NumPropsThisClass + 2] := Format('%g', [Emergamps]);
    PropertyValue[NumPropsThisClass + 3] := Str_Real(FaultRate, 0);
    PropertyValue[NumPropsThisClass + 4] := Str_Real(PctPerm, 0);
    PropertyValue[NumPropsThisClass + 5] := Str_Real(HrsToRepair, 0);
    ClearPropSeqArray;
end;

procedure TCapacitorObj.MakePosSequence(ActorID: Integer);
var
    S: String;
    kvarperphase, phasekV, Cs, Cm: Double;
    i, j: Integer;

begin
    {If FnPhases>1 Then -- do same for 1-phase, too}
    begin

        S := ' ';
        case SpecType of

            1:
            begin // kvar

                if (FnPhases > 1) or (Connection <> 0) then
                    PhasekV := kVRating / SQRT3
                else
                    PhasekV := kVRating;

                S := 'Phases=1 ' + Format(' kV=%-.5g kvar=(', [PhasekV]);

              // 1-6-16  do caps Like load ...
                for i := 1 to FNumSteps do
                begin
                    kvarPerPhase := FkvarRating^[i] / 3.0;  // divide the total kvar equally among3 phases.../Fnphases;
                    S := S + Format(' %-.5g', [kvarPerPhase]);
                end;

                S := S + ')';

              {Leave R as specified}

            end;
            2:
            begin //
                S := 'Phases=1 ';
            end;
            3:
                if FnPhases > 1 then
                begin //  C Matrix
                    S := 'Phases=1 ';
              // R1
                    Cs := 0.0;   // Avg Self
                    for i := 1 to FnPhases do
                        Cs := Cs + Cmatrix^[(i - 1) * Fnphases + i];
                    Cs := Cs / FnPhases;

                    Cm := 0.0;     //Avg mutual
                    for i := 2 to FnPhases do
                        for j := i to FnPhases do
                            Cm := Cm + Cmatrix^[(i - 1) * Fnphases + j];
                    Cm := Cm / (FnPhases * (Fnphases - 1.0) / 2.0);

                    S := S + Format(' Cuf=%-.5g', [(Cs - Cm)]);

                end;
        end;

        Parser[ActorID].CmdString := S;
        Edit(ActorID);

    end;

    inherited;

end;


function TCapacitorObj.get_States(Idx: Integer; ActorID: Integer): Integer;
begin
    Result := FStates^[Idx];
end;

procedure TCapacitorObj.set_States(Idx: Integer; ActorID: Integer; const Value: Integer);
begin
    if FStates^[Idx] <> Value then
    begin
        FStates^[Idx] := Value;
        YprimInvalid[ActorID] := TRUE;
    end;
end;

procedure TCapacitorObj.set_NumSteps(const Value: Integer);

{
 Special case for changing from 1 to more ..  Automatically make a new bank
}

var
    StepSize, RStep, XLstep: Double;
    i: Integer;
begin
  {reallocate all arrays associated with steps }

    if (FNumSteps <> Value) and (Value > 0) then
    begin
        Rstep := 0.0;
        XLstep := 0.0;
        if FNumSteps = 1 then
        begin
          {Save total values to be divided up}
            FTotalkvar := Fkvarrating^[1];
            Rstep := FR^[1] * Value;
            XLstep := FXL^[1] * Value;
        end;

      // Reallocate arrays  (Must be initialized to nil for first call)
        Reallocmem(FC, Sizeof(FC^[1]) * Value);
        Reallocmem(FXL, Sizeof(FXL^[1]) * Value);
        Reallocmem(Fkvarrating, Sizeof(Fkvarrating^[1]) * Value);
        Reallocmem(FR, Sizeof(FR^[1]) * Value);
        Reallocmem(FHarm, Sizeof(FHarm^[1]) * Value);
        Reallocmem(FStates, Sizeof(FStates^[1]) * Value);

      // Special case for FNumSteps=1

        if FNumSteps = 1 then
        begin
            case SpecType of

                1:
                begin  // kvar        {We'll make a multi-step bank of same net size as at present}
                    StepSize := FTotalkvar / Value;
                    for i := 1 to Value do
                        FkvarRating^[i] := StepSize;
                end;

                2:
                begin  // Cuf           {We'll make a multi-step bank with all the same as first}
                    for i := 2 to Value do
                        FC^[i] := FC^[1];  // Make same as first step
                end;

                3:
                begin  // Cmatrix  {We'll make a multi-step bank with all the same as first}
                 // Nothing to do since all will be the same
                end;

            end;

            case SpecType of

                1:
                begin
                    for i := 1 to Value do
                        FR^[i] := Rstep;
                    for i := 1 to Value do
                        FXL^[i] := XLstep;
                end;

                2, 3:
                begin   // Make R and XL same as first step
                    for i := 2 to Value do
                        FR^[i] := FR^[1];
                    for i := 2 to Value do
                        FXL^[i] := FXL^[1];
                end;

            end;

            for i := 1 to Value do
                Fstates^[i] := 1;   // turn 'em all ON
            LastStepInService := Value;
            for i := 2 to Value do
                FHarm^[i] := FHarm^[1];  // tune 'em all the same as first

        end;

    end;

    FNumSteps := Value;
end;

procedure TCapacitorObj.ProcessHarmonicSpec(const Param: String);
begin
    FNumsteps := InterpretDblArray(Param, FNumsteps, FHarm);

    DoHarmonicRecalc := TRUE;
end;

procedure TCapacitorObj.FindLastStepInService;
// Find the last step energized
var
    i: Integer;
begin
    FLastStepInService := 0;

    for i := FNumsteps downto 1 do
    begin
        if Fstates^[i] = 1 then
        begin
            FLastStepInService := i;
            Break;
        end;
    end;
end;

procedure TCapacitorObj.set_LastStepInService(const Value: Integer);
// force the last step in service to be a certain value
var
    i: Integer;
begin

    for i := 1 to Value do
        FStates^[i] := 1;
     // set remainder steps, if any, to 0
    for i := Value + 1 to FNumSteps do
        FStates^[i] := 0;

     // Force rebuild of YPrims if necessary.
    if Value <> FLastStepInService then
        YprimInvalid[ActiveActor] := TRUE;

    FLastStepInService := Value;
end;

procedure TCapacitorObj.ProcessStatesSpec(const Param: String);

begin
    FNumsteps := InterpretIntArray(Param, FNumsteps, FStates);
    FindLastStepInService;
end;

procedure TCapacitorObj.MakeYprimWork(YprimWork: TcMatrix; iStep: Integer; ActorID: Integer);

{ call this routine only if step is energized}

var
    Value, Value2,
    ZL: Complex;
    i, j, ioffset: Integer;
    w, FreqMultiple: Double;
    HasZL: Boolean;

begin

    with YprimWork do
    begin

        FYprimFreq := ActiveCircuit[ActorID].Solution.Frequency;
        FreqMultiple := FYprimFreq / BaseFrequency;
        w := TwoPi * FYprimFreq;

        if (FR^[iStep] + Abs(FXL^[iSTep])) > 0.0 then
            HasZL := TRUE
        else
            HasZL := FALSE;

        if HasZL then
        begin
            ZL := Cmplx(FR^[iSTep], FXL^[iSTep] * FreqMultiple);
        end;

    { Now, Put C into in Yprim matrix }

        case SpecType of

            1, 2:
            begin

                Value := Cmplx(0.0, FC^[iSTep] * w);
                case Connection of
                    1:
                    begin   // Line-Line
                        Value2 := CmulReal(Value, 2.0);
                        Value := cnegate(Value);
                        for i := 1 to Fnphases do
                        begin
                            SetElement(i, i, Value2);
                            for j := 1 to i - 1 do
                                SetElemSym(i, j, Value);
                        end;
                // Remainder of the matrix is all zero
                    end;
                else
                begin // Wye
                    if HasZL then
                        Value := Cinv(Cadd(ZL, Cinv(Value))); // add in ZL
                    Value2 := cnegate(Value);
                    for i := 1 to Fnphases do
                    begin
                        SetElement(i, i, Value);     // Elements are only on the diagonals
                        SetElement(i + Fnphases, i + Fnphases, Value);
                        SetElemSym(i, i + Fnphases, Value2);
                    end;
                end;
                end;
            end;
            3:
            begin    // C matrix specified
                for i := 1 to Fnphases do
                begin
                    ioffset := (i - 1) * Fnphases;
                    for j := 1 to Fnphases do
                    begin
                        Value := Cmplx(0.0, Cmatrix^[(iOffset + j)] * w);
                        SetElement(i, j, Value);
                        SetElement(i + Fnphases, j + Fnphases, Value);
                        Value := cnegate(Value);
                        SetElemSym(i, j + Fnphases, Value);
                    end;
                end;
            end;
        end;

      {Add line reactance for filter reactor, if any}
        if HasZL then
            case SpecType of

                1, 2:
                    case Connection of
                        1: {Line-Line}
                        begin
                         {Add a little bit to each phase so it will invert}
                            for i := 1 to Fnphases do
                            begin
                                SetElement(i, i, CmulReal(GetElement(i, i), 1.000001));
                            end;
                            Invert;
                            for i := 1 to Fnphases do
                            begin
                                Value := Cadd(ZL, GetElement(i, i));
                                SetElement(i, i, Value);
                            end;
                            Invert;
                        end;
                    else {WYE - just put ZL in series}
                      {DO Nothing; Already in - see above}
                    end;

                3:
                begin
                    Invert;
                    for i := 1 to Fnphases do
                    begin
                        Value := Cadd(ZL, GetElement(i, i));
                        SetElement(i, i, Value);
                    end;
                    Invert;
                end;
            end;

    end; {With YPRIM}


end;

function TCapacitorObj.GetPropertyValue(Index: Integer): String;

var
    i: Integer;
    FTemp: pDoubleArray;
begin

    Result := '';
    case Index of  // Special cases
        1:
            Result := GetBus(1);
        2:
            Result := GetBus(2);
        4:
            Result := GetDSSArray_Real(FNumSteps, Fkvarrating);
        8:
        begin
            FTemp := Allocmem(SizeOf(Double) * FNumSteps);
            for i := 1 to FNumSteps do
                FTemp^[i] := FC^[i] * 1.0e6;  // To microfarads
            Result := GetDSSArray_Real(FNumSteps, FTemp);
            Reallocmem(FTemp, 0); // throw away temp storage
        end;
        9:
            Result := GetDSSArray_Real(FNumSteps, FR);
        10:
            Result := GetDSSArray_Real(FNumSteps, FXL);
        11:
            Result := GetDSSArray_Real(FNumSteps, Fharm);
        13:
            Result := GetDSSArray_Integer(FNumSteps, FStates);
        14:
            Result := Format('%g', [Normamps]);
        15:
            Result := Format('%g', [Emergamps]);
    else
        Result := inherited GetPropertyValue(index);
    end;

end;

function TCapacitorObj.AddStep(ActorID: Integer): Boolean;
begin
     // Start with last step in service and see if we can add more.  If not return FALSE

    if LastStepInService = FNumSteps then
        Result := FALSE
    else
    begin
        Inc(FLastStepInService);
        States[FLastStepInService, ActorID] := 1;
        Result := TRUE;
    end;
end;

function TCapacitorObj.SubtractStep(ActorID: Integer): Boolean;
begin
    if LastStepInService = 0 then
        Result := FALSE
    else
    begin
        States[FLastStepInService, ActorID] := 0;
        Dec(FLastStepInService);
        if LastStepInService = 0 then
            Result := FALSE
        else
            Result := TRUE;   // signify bank OPEN
    end;

end;

function TCapacitorObj.AvailableSteps: Integer;
begin
    Result := FNumsteps - LastStepInService;
end;

end.
