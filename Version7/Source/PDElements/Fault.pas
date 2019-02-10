unit Fault;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   3-1-00 Restored old Dump
          Removed 1.e6 multiplier (where did this come from???)
   9-??-00 Added Temporary fault logic       
   9-22-00 Revised Is_ON logic
   7-2-01 Corrected default bus2 phase designation
          Force rebuilding of bus lists if num phases changed
}

{
 Fault object:

   One or more faults can be placed across any two buses in the circuit.
   Like the capacitor, the second bus defaults to the ground node of the
   same bus that bus1 is connected to.

   The fault is basically an uncoupled, multiphase resistance branch.  however,
   you may also specify it as NODAL CONDUCTANCE (G) matrix, which will give you
   complete control of a complex fault situation.

   To eliminate a fault from the system after it has been defined, disable it.

   In Monte Carlo Fault mode, the fault resistance is varied by the % std dev specified
   If %Stddev is specified as zero (default), the resistance is varied uniformly.

   Fault may have its "ON" time specified (defaults to 0). When Time (t) exceeds this value, the
   fault will be enabled.  Else it is disabled.

   Fault may be designated as Temporary.  That is, after it is enabled, it will disable itself
   if the fault current drops below the MinAmps value.
}

interface

uses
    Command,
    DSSClass,
    PDClass,
    Circuit,
    PDElement,
    UcMatrix,
    ArrayDef;

type

    TFault = class(TPDClass)
    PRIVATE
        procedure DoGmatrix;

        procedure FltSetBus1(const s: String);
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const FaultName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function Init(Handle: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

    end;

    TFaultObj = class(TPDElement)
    PRIVATE
        MinAmps: Double;
        IsTemporary,
        Cleared,
        Is_ON: Boolean;
        Bus2Defined: Boolean;
        On_Time: Double;
        RandomMult: Double;
        function FaultStillGoing: Boolean;
    PROTECTED
        G: Double;         // single G per phase (line rating) if Gmatrix not specified
        Gmatrix: pDoubleArray;  // If not nil then overrides G

        Stddev: Double;  // per unit stddev
        SpecType: Integer;

    PUBLIC
        constructor Create(ParClass: TDSSClass; const FaultName: String);
        destructor Destroy; OVERRIDE;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        procedure Randomize;
        procedure CheckStatus(ControlMode: Integer);
        procedure Reset;

        procedure MakePosSequence; OVERRIDE;  // Make a positive Sequence Model

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

    end;

var
    ActiveFaultObj: TFaultObj;

implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    dynamics,
    Sysutils,
    Ucomplex,
    MathUtil,
    Utilities;

const
    NumPropsthisclass = 9;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TFault.Create;  // Creates superstructure for all Fault objects
begin
    inherited Create;
    Class_Name := 'Fault';
    DSSClassType := FAULTOBJECT + NON_PCPD_ELEM;  // Only in Fault object class

    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TFault.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TFault.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


     // Define Property names

    PropertyName^[1] := 'bus1';
    PropertyName^[2] := 'bus2';
    PropertyName^[3] := 'phases';
    PropertyName^[4] := 'r';
    PropertyName^[5] := '%stddev';
    PropertyName^[6] := 'Gmatrix';
    PropertyName^[7] := 'ONtime';
    PropertyName^[8] := 'temporary';
    PropertyName^[9] := 'MinAmps';

     // define Property help values
    PropertyHelp[1] := 'Name of first bus. Examples:' + CRLF + CRLF +
        'bus1=busname' + CRLF +
        'bus1=busname.1.2.3' + CRLF + CRLF +
        'Bus2 automatically defaults to busname.0,0,0 unless it was previously defined. ';
    PropertyHelp[2] := 'Name of 2nd bus of the 2-terminal Fault object. Defaults to all phases connected ' +
        'to first bus, node 0, if not specified. (Shunt Wye Connection to ground reference)' + CRLF + CRLF +
        'That is, the Fault defaults to a ground fault unless otherwise specified.';
    PropertyHelp[3] := 'Number of Phases. Default is 1.';
    PropertyHelp[4] := 'Resistance, each phase, ohms. Default is 0.0001. Assumed to be Mean value if gaussian random mode.' +
        'Max value if uniform mode.  A Fault is actually a series resistance ' +
        'that defaults to a wye connection to ground on the second terminal.  You ' +
        'may reconnect the 2nd terminal to achieve whatever connection.  Use ' +
        'the Gmatrix property to specify an arbitrary conductance matrix.';
    PropertyHelp[5] := 'Percent standard deviation in resistance to assume for Monte Carlo fault (MF) solution mode for GAUSSIAN distribution. Default is 0 (no variation from mean).';
    PropertyHelp[6] := 'Use this to specify a nodal conductance (G) matrix to represent some arbitrary resistance network. ' +
        'Specify in lower triangle form as usual for DSS matrices.';
    PropertyHelp[7] := 'Time (sec) at which the fault is established for time varying simulations. Default is 0.0 ' +
        '(on at the beginning of the simulation)';
    PropertyHelp[8] := '{Yes | No} Default is No.  Designate whether the fault is temporary.  For Time-varying simulations, ' +
        'the fault will be removed if the current through the fault drops below the MINAMPS criteria.';
    PropertyHelp[9] := 'Minimum amps that can sustain a temporary fault. Default is 5.';


    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TFault.NewObject(const ObjName: String): Integer;
begin
   // create a new object of this class and add to list
    with ActiveCircuit do
    begin
        ActiveCktElement := TFaultObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TFault.DoGmatrix;
var
    OrderFound, j: Integer;
    MatBuffer: pDoubleArray;

begin
    with ActiveFaultObj do
    begin
        MatBuffer := Allocmem(Sizeof(Double) * Fnphases * Fnphases);
        OrderFound := Parser.ParseAsSymMatrix(Fnphases, MatBuffer);

        if OrderFound > 0 then    // Parse was successful
        begin    {X}
            Reallocmem(Gmatrix, Sizeof(Gmatrix^[1]) * Fnphases * Fnphases);
            for j := 1 to Fnphases * Fnphases do
                Gmatrix^[j] := MatBuffer^[j];
        end;

        Freemem(MatBuffer, Sizeof(Double) * Fnphases * Fnphases);
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TFault.FltSetBus1(const s: String);

var
    s2: String;
    dotpos: Integer;

   // Special handling for Bus 1
   // Set Bus2 = Bus1.0.0.0

begin
    with ActiveFaultObj do
    begin

        SetBus(1, S);

     // Default Bus2 to zero node of Bus1 unless previously defined explicitly. (Wye Grounded connection)

        if not Bus2Defined then
        begin
         // Strip node designations from S
            dotpos := Pos('.', S);
            if dotpos > 0 then
                S2 := Copy(S, 1, dotpos - 1)  // copy up to Dot
            else
                S2 := Copy(S, 1, Length(S));

            S2 := S2 + '.0.0.0';     // Set Default for up to 3 phases

            SetBus(2, S2);
            IsShunt := TRUE;
        end;
    end;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TFault.Edit: Integer;

var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    PhasesTemp: Integer;

begin
    Result := 0;
  // continue parsing with contents of Parser
    ActiveFaultObj := ElementList.Active;
    ActiveCircuit.ActiveCktElement := ActiveFaultObj;  // use property to set this value

    with ActiveFaultObj do
    begin

        ParamPointer := 0;
        ParamName := Parser.NextParam;
        Param := Parser.StrValue;
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 350);
                1:
                    FltSetbus1(param);
                2:
                    Setbus(2, param);
                3: ;{Numphases := Parser.IntValue;}  // see below
                4:
                begin
                    G := Parser.Dblvalue;
                    if G <> 0.0 then
                        G := 1.0 / G
                    else
                        G := 10000.0;  // Default to a low resistance
                end;
                5:
                    StdDev := Parser.Dblvalue * 0.01;
                6:
                    DoGmatrix;
                7:
                    ON_Time := Parser.Dblvalue;
                8:
                    IsTemporary := InterpretYesNo(Param);
                9:
                    MinAmps := Parser.DblValue;
            else
           // Inherited
                ClassEdit(ActiveFaultObj, ParamPointer - NumPropsThisClass)
            end;

         // Some specials ...
            case ParamPointer of
                1:
                    PropertyValue[2] := GetBus(2);  // Bus2 gets modified if bus1 is
                2:
                    if CompareText(StripExtension(GetBus(1)), StripExtension(GetBus(2))) <> 0 then
                    begin
                        IsShunt := FALSE;
                        Bus2Defined := TRUE;
                    end;
                3:
                begin
                    PhasesTemp := Parser.IntValue;
                    if Fnphases <> PhasesTemp then
                    begin
                        Nphases := PhasesTemp;
                        NConds := Fnphases;  // Force Reallocation of terminal info
                        ActiveCircuit.BusNameRedefined := TRUE;  // Set Global Flag to signal circuit to rebuild busdefs
                    end;
                end;
                4:
                    SpecType := 1;
                6:
                    SpecType := 2;
                7:
                    if ON_Time > 0.0 then
                        Is_ON := FALSE;   // Assume fault will be on later
            else
            end;

         //YPrim invalidation on anything that changes impedance values
            case ParamPointer of
                3, 4, 6:
                    YprimInvalid := TRUE;
            else
            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

        RecalcElementData;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TFault.MakeLike(const FaultName: String): Integer;
var
    OtherFault: TFaultObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this Fault name in the present collection}
    OtherFault := Find(FaultName);
    if OtherFault <> NIL then
        with ActiveFaultObj do
        begin

            if Fnphases <> OtherFault.Fnphases then
            begin
                Fnphases := OtherFault.Fnphases;
                NConds := Fnphases; // force reallocation of terminals and conductors

                Yorder := Fnconds * Fnterms;
                YPrimInvalid := TRUE;

            end;

            BaseFrequency := OtherFault.BaseFrequency;
            G := OtherFault.G;
            SpecType := OtherFault.SpecType;

            MinAmps := OtherFault.MinAmps;
            IsTemporary := OtherFault.IsTemporary;
            Cleared := OtherFault.Cleared;
            Is_ON := OtherFault.Is_ON;
            On_Time := OtherFault.On_Time;


            if OtherFault.Gmatrix = NIL then
                Reallocmem(Gmatrix, 0)
            else
            begin
                Reallocmem(Gmatrix, SizeOf(Gmatrix^[1]) * Fnphases * Fnphases);
                for i := 1 to Fnphases * Fnphases do
                    Gmatrix^[i] := OtherFault.Gmatrix^[i];
            end;


            ClassMakeLike(OtherFault);

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherFault.PropertyValue[i];
            Result := 1;
        end
    else
        DoSimpleMsg('Error in Fault MakeLike: "' + FaultName + '" Not Found.', 351);


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TFault.Init(Handle: Integer): Integer;

begin
    DoSimpleMsg('Need to implement TFault.Init', -1);
    Result := 0;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TFault Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TFaultObj.Create(ParClass: TDSSClass; const FaultName: String);

begin
    inherited Create(ParClass);
    DSSObjType := ParClass.DSSClassType; //FAULTOBJECT + NON_PCPD_ELEM;  // Only in Fault object class
    Name := LowerCase(FaultName);

     // Default to SLG fault
    NPhases := 1;  // Directly set conds and phases
    Fnconds := 1;
    Nterms := 2;  // Force allocation of terminals and conductors

    Setbus(2, (GetBus(1) + '.0'));  // Default to grounded
    IsShunt := TRUE;

    Gmatrix := NIL;
    G := 10000.0;
    SpecType := 1; // G  2=Gmatrix

    MinAmps := 5.0;
    IsTemporary := FALSE;
    Cleared := FALSE;
    Bus2Defined := FALSE;
    Is_ON := TRUE;
    On_Time := 0.0;  // Always enabled at the start of a solution.


    RandomMult := 1;

    NormAmps := 0.0;
    EmergAmps := 0.0;
    FaultRate := 0.0;
    PctPerm := 100.0;
    HrsToRepair := 0.0;

    InitPropertyValues(0);


    Yorder := Fnterms * Fnconds;
    RecalcElementData;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TFaultObj.Destroy;
begin
    ReallocMem(Gmatrix, 0);
    inherited destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TFaultObj.RecalcElementData;

begin

// Nothing to do

end;

function Cube(const X: Double): Double;

begin
    Result := X * X * X;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TFaultObj.Randomize;

// called from solveMontefault Procedure

begin
    with activeCircuit.Solution do
    begin
        case RandomType of
            GAUSSIAN:
                RandomMult := Gauss(1.0, StdDev);
            UNIFORM:
                RandomMult := Random;
            LOGNORMAL:
                RandomMult := QuasiLogNormal(1.0);
        else
            RandomMult := 1.0;
        end;
    end;

     // Give the multiplier some skew to approximate more uniform/Gaussian current distributions
     //  RandomMult :=  Cube(RandomMult);   removed 12/7/04

    YPrimInvalid := TRUE;    // force rebuilding of matrix
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TFaultObj.CalcYPrim;

var
    Value, Value2: Complex;
    i,
    j,
    ioffset: Integer;

    YPrimTemp: TCMatrix;

begin

    if YPrimInvalid then
    begin    // Reallocate YPrim if something has invalidated old allocation
        if YPrim_Series <> NIL then
            YPrim_Series.Free;
        YPrim_Series := TCmatrix.CreateMatrix(Yorder);
        if YPrim_Shunt <> NIL then
            YPrim_Shunt.Free;
        YPrim_Shunt := TCmatrix.CreateMatrix(Yorder);
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

  // make sure randommult is 1.0 if not solution mode MonteFault

    if ActiveCircuit.Solution.Mode <> MONTEFAULT then
        RandomMult := 1.0;

    if RandomMult = 0.0 then
        RandomMult := 0.000001;

    with YPrimTemp do
    begin

    { Now, Put in Yprim matrix }

    {If the fault is not ON, the set zero conductance}

        case SpecType of

            1:
            begin

                if Is_ON then
                    Value := Cmplx(G / RandomMult, 0.0)
                else
                    Value := CZERO;
                Value2 := cnegate(Value);
                for i := 1 to Fnphases do
                begin
                    SetElement(i, i, Value);     // Elements are only on the diagonals
                    SetElement(i + Fnphases, i + Fnphases, Value);
                    SetElemSym(i, i + Fnphases, Value2);
                end;
            end;
            2:
            begin    // G matrix specified
                for i := 1 to Fnphases do
                begin
                    ioffset := (i - 1) * Fnphases;
                    for j := 1 to Fnphases do
                    begin
                        if Is_ON then
                            Value := Cmplx(Gmatrix^[(iOffset + j)] / RandomMult, 0.0)
                        else
                            Value := CZERO;
                        SetElement(i, j, Value);
                        SetElement(i + Fnphases, j + Fnphases, Value);
                        Value := cnegate(Value);
                        SetElemSym(i, j + Fnphases, Value);
                    end;
                end;
            end;
        end;

    end; {With YPRIM}

    YPrim.CopyFrom(YPrimTemp);

    inherited CalcYPrim;
    YprimInvalid := FALSE;
end;

procedure TFaultObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i, j: Integer;

begin
    inherited DumpProperties(F, complete);


    with ParentClass do
    begin
        Writeln(F, '~ ', PropertyName^[1], '=', firstbus);
        Writeln(F, '~ ', PropertyName^[2], '=', nextbus);

        Writeln(F, '~ ', PropertyName^[3], '=', Fnphases: 0);
        Writeln(F, '~ ', PropertyName^[4], '=', (1.0 / G): 0: 2);
        Writeln(F, '~ ', PropertyName^[5], '=', (StdDev * 100.0): 0: 1);
        if Gmatrix <> NIL then
        begin
            Write(F, '~ ', PropertyName^[6], '= (');
            for i := 1 to Fnphases do
            begin
                for j := 1 to i do
                    Write(F, (Gmatrix^[(i - 1) * Fnphases + j]): 0: 3, ' ');
                if i <> Fnphases then
                    Write(F, '|');
            end;
            Writeln(F, ')');
        end;
        Writeln(F, '~ ', PropertyName^[7], '=', ON_Time: 0: 3);
        if IsTemporary then
            Writeln(F, '~ ', PropertyName^[8], '= Yes')
        else
            Writeln(F, '~ ', PropertyName^[8], '= No');
        Writeln(F, '~ ', PropertyName^[9], '=', Minamps: 0: 1);


        for i := NumPropsthisClass to NumProperties do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
        end;

        if Complete then
        begin
            Writeln(F, '// SpecType=', SpecType: 0);
        end;
    end;

end;


procedure TFaultObj.CheckStatus(ControlMode: Integer);
begin

    case ControlMode of

        CTRLSTATIC:   {Leave it however it is defined by other processes}
        begin
        end;
        EVENTDRIVEN,
        MULTIRATE,
        TIMEDRIVEN:
        begin
            if not Is_ON then
            begin   {Turn it on unless it has been previously cleared}
                if (PresentTimeInSec > On_Time) and not Cleared then
                begin
                    Is_ON := TRUE;
                    YPrimInvalid := TRUE;
                    AppendtoEventLog('Fault.' + Name, '**APPLIED**');
                end;
            end
            else
            begin
                if IsTemporary then
                    if not FaultStillGoing then
                    begin
                        Is_ON := FALSE;
                        Cleared := TRUE;
                        YPrimInvalid := TRUE;
                        AppendtoEventLog('Fault.' + Name, '**CLEARED**');
                    end;
            end;
        end;
    end;

end;

function TFaultObj.FaultStillGoing: Boolean;
var
    i: Integer;
begin

    ComputeIterminal;
    Result := FALSE;
    for i := 1 to FNphases do
    begin
        if Cabs(Iterminal^[i]) > MinAmps then
        begin
            Result := TRUE;
            Exit;
        end;
    end;

end;

procedure TFaultObj.Reset;
begin
    Cleared := FALSE;
end;

procedure TFaultObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := getbus(1);
    PropertyValue[2] := getbus(2);
    PropertyValue[3] := '1';
    PropertyValue[4] := '0.0001';
    PropertyValue[5] := '0';
    PropertyValue[6] := '';
    PropertyValue[7] := '0.0';
    PropertyValue[8] := 'no';
    PropertyValue[9] := '5.0';

    inherited  InitPropertyValues(NumPropsThisClass);

     // Override Inherited Properties
    PropertyValue[NumPropsThisClass + 1] := '0';  //Normamps
    PropertyValue[NumPropsThisClass + 2] := '0';  //emergamps
    PropertyValue[NumPropsThisClass + 3] := '0';  //Fault rate
    PropertyValue[NumPropsThisClass + 4] := '0';   // Pct Perm
    PropertyValue[NumPropsThisClass + 5] := '0';    // Hrs to repair


end;

function TFaultObj.GetPropertyValue(Index: Integer): String;
var
    i, j: Integer;
begin
    case Index of

        6:
        begin
            Result := '(';
            if Assigned(Gmatrix) then
                for i := 1 to Fnphases do      // G matrix
                begin
                    for j := 1 to i do
                    begin
                        Result := Result + Format('%-g', [Gmatrix^[(i - 1) * Fnphases + j]]) + ' ';
                    end;
                    if i < Fnphases then
                        Result := Result + '|';
                end;

            Result := Result + ')';
        end;
    else
        Result := inherited GetPropertyValue(Index);
    end;
end;

procedure TFaultObj.MakePosSequence;


begin
    if FnPhases <> 1 then
    begin
        Parser.CmdString := 'Phases=1';
        Edit;
    end;
    inherited;

end;

end.
