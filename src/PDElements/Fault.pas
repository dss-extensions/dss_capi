unit Fault;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

// Fault object:
//
//   One or more faults can be placed across any two buses in the circuit.
//   Like the capacitor, the second bus defaults to the ground node of the
//   same bus that bus1 is connected to.
//
//   The fault is basically an uncoupled, multiphase resistance branch.  however,
//   you may also specify it as NODAL CONDUCTANCE (G) matrix, which will give you
//   complete control of a complex fault situation.
//
//   To eliminate a fault from the system after it has been defined, disable it.
//
//   In Monte Carlo Fault mode, the fault resistance is varied by the % std dev specified
//   If %Stddev is specified as zero (default), the resistance is varied uniformly.
//
//   Fault may have its "ON" time specified (defaults to 0). When Time (t) exceeds this value, the
//   fault will be enabled.  Else it is disabled.
//
//   Fault may be designated as Temporary.  That is, after it is enabled, it will disable itself
//   if the fault current drops below the MinAmps value.

interface

uses
    Classes,
    Command,
    DSSClass,
    PDClass,
    Circuit,
    PDElement,
    UcMatrix,
    ArrayDef;

type
{$SCOPEDENUMS ON}
    TFaultProp = (
        INVALID = 0,
        bus1 = 1,
        bus2 = 2,
        phases = 3,
        r = 4,
        pctstddev = 5,
        Gmatrix = 6,
        ONtime = 7,
        temporary = 8,
        MinAmps = 9
    );
{$SCOPEDENUMS OFF}

    TFault = class(TPDClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function BeginEdit(ptr: Pointer; SetActive: Boolean=True): Pointer; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TFaultObj = class(TPDElement)
    PRIVATE
        MinAmps: Double;
        IsTemporary: LongBool;
        Cleared,
        Is_ON: Boolean;
        Bus2Defined: Boolean;
        On_Time: Double;
        RandomMult: Double;
        function FaultStillGoing: Boolean;
    PROTECTED
        G: Double; // single G per phase (line rating) if Gmatrix not specified
        Gmatrix: pDoubleArray;  // If not nil then overrides G

        Stddev: Double;  // per unit stddev
        SpecType: Integer;

    PUBLIC
        constructor Create(ParClass: TDSSClass; const FaultName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        procedure Randomize;
        procedure CheckStatus(ControlMode: Integer);
        procedure Reset;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model

        procedure DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    dynamics,
    Sysutils,
    UComplex, DSSUcomplex,
    MathUtil,
    Utilities,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TFaultObj;
    TProp = TFaultProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    

constructor TFault.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, FAULTOBJECT or NON_PCPD_ELEM, 'Fault');
end;

destructor TFault.Destroy;
begin
    inherited Destroy;
end;

procedure TFault.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // real matrix
    PropertyType[ord(TProp.Gmatrix)] := TPropertyType.DoubleSymMatrixProperty;
    PropertyOffset[ord(TProp.Gmatrix)] := ptruint(@obj.Gmatrix);
    PropertyOffset2[ord(TProp.Gmatrix)] := ptruint(@obj.Fnphases);

    // integer properties
    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    // bus properties
    PropertyType[ord(TProp.bus1)] := TPropertyType.BusProperty;
    PropertyType[ord(TProp.bus2)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus1)] := 1;
    PropertyOffset[ord(TProp.bus2)] := 2;

    // boolean properties
    PropertyType[ord(TProp.temporary)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.temporary)] := ptruint(@obj.IsTemporary);

    // percent properties
    PropertyScale[ord(TProp.pctstddev)] := 0.01;
    PropertyOffset[ord(TProp.pctstddev)] := ptruint(@obj.StdDev);

    // double properties (default type)
    PropertyOffset[ord(TProp.ONtime)] := ptruint(@obj.ON_Time);
    PropertyOffset[ord(TProp.MinAmps)] := ptruint(@obj.MinAmps);

    PropertyOffset[ord(TProp.r)] := ptruint(@obj.G);
    PropertyInverse[ord(TProp.r)] := True;

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TFault.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TFaultObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    dotpos: Integer;
    S, S2: string;
begin
    case Idx of
        ord(TProp.phases):
            if Fnphases <> previousIntVal then
            begin
                NConds := Fnphases;  // Force Reallocation of terminal info
                ActiveCircuit.BusNameRedefined := TRUE;  // Set Global Flag to signal circuit to rebuild busdefs
            end;
        ord(TProp.bus1):
            if not Bus2Defined then
            begin
                // Default Bus2 to zero node of Bus1 unless previously defined explicitly. (Wye Grounded connection)

                // Strip node designations from S
                S := GetBus(1);
                dotpos := Pos('.', S);
                if dotpos > 0 then
                    S2 := Copy(S, 1, dotpos - 1)  // copy up to Dot
                else
                    S2 := Copy(S, 1, Length(S));

                S2 := S2 + '.0.0.0';     // Set Default for up to 3 phases

                SetBus(2, S2);
                IsShunt := TRUE;
                SetAsNextSeq(ord(TProp.bus2));
            end;
        ord(TProp.bus2):
            if AnsiCompareText(StripExtension(GetBus(1)), StripExtension(GetBus(2))) <> 0 then
            begin
                IsShunt := FALSE;
                Bus2Defined := TRUE;
            end;
        ord(TProp.r):
        begin
            SpecType := 1;
            if G = 0 then
                G := 10000.0;  // Default to a low resistance
        end;
        ord(TProp.Gmatrix):
            SpecType := 2;
        ord(TProp.ONtime):
            if ON_Time > 0.0 then
                Is_ON := FALSE;   // Assume fault will be on later
    end;

        //YPrim invalidation on anything that changes impedance values
    case Idx of
        3, 4, 6:
            YprimInvalid := TRUE;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TFault.BeginEdit(ptr: Pointer; SetActive: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj(inherited BeginEdit(ptr, SetActive));
    if SetActive then
        DSS.ActiveFaultObj := Obj;
    Result := Obj;
end;

procedure TFaultObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    if Fnphases <> Other.Fnphases then
    begin
        Fnphases := Other.Fnphases;
        NConds := Fnphases; // force reallocation of terminals and conductors

        Yorder := Fnconds * Fnterms;
        YPrimInvalid := TRUE;
    end;
    BaseFrequency := Other.BaseFrequency;
    G := Other.G;
    SpecType := Other.SpecType;

    MinAmps := Other.MinAmps;
    IsTemporary := Other.IsTemporary;
    Cleared := Other.Cleared;
    Is_ON := Other.Is_ON;
    On_Time := Other.On_Time;

    if Other.Gmatrix = NIL then
        Reallocmem(Gmatrix, 0)
    else
    begin
        Reallocmem(Gmatrix, SizeOf(Gmatrix^[1]) * Fnphases * Fnphases);
        for i := 1 to Fnphases * Fnphases do
            Gmatrix^[i] := Other.Gmatrix^[i];
    end;
end;

constructor TFaultObj.Create(ParClass: TDSSClass; const FaultName: String);
begin
    inherited Create(ParClass);
    DSSObjType := ParClass.DSSClassType; //FAULTOBJECT + NON_PCPD_ELEM;  // Only in Fault object class
    Name := AnsiLowerCase(FaultName);

     // Default to SLG fault
    FNPhases := 1;  // Directly set conds and phases
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

    Yorder := Fnterms * Fnconds;
    RecalcElementData;
end;

destructor TFaultObj.Destroy;
begin
    ReallocMem(Gmatrix, 0);
    inherited destroy;
end;

procedure TFaultObj.RecalcElementData;
begin
// Nothing to do
end;

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

    if ActiveCircuit.Solution.Mode <> TSolveMode.MONTEFAULT then
        RandomMult := 1.0;

    if RandomMult = 0.0 then
        RandomMult := 0.000001;

    with YPrimTemp do
    begin
        // Now, Put in Yprim matrix
        // If the fault is not ON, the set zero conductance
        case SpecType of

            1:
            begin
                if Is_ON then
                    Value := Cmplx(G / RandomMult, 0.0)
                else
                    Value := CZERO;
                Value2 := -Value;
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
                        Value := -Value;
                        SetElemSym(i, j + Fnphases, Value);
                    end;
                end;
            end;
        end;

    end;

    YPrim.CopyFrom(YPrimTemp);

    inherited CalcYPrim;
    YprimInvalid := FALSE;
end;

procedure TFaultObj.DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean);
var
    i, j: Integer;
begin
    inherited DumpProperties(F, complete);

    with ParentClass do
    begin
        FSWriteln(F, '~ ' + PropertyName^[1] + '=' + firstbus);
        FSWriteln(F, '~ ' + PropertyName^[2] + '=' + nextbus);

        FSWriteln(F, Format('~ %s=%d', [PropertyName^[3], Fnphases]));
        FSWriteln(F, Format('~ %s=%.2f', [PropertyName^[4], (1.0 / G)]));
        FSWriteln(F, Format('~ %s=%.1f', [PropertyName^[5], (StdDev * 100.0)]));
        if Gmatrix <> NIL then
        begin
            FSWrite(F, '~ ' + PropertyName^[6] + '= (');
            for i := 1 to Fnphases do
            begin
                for j := 1 to i do
                    FSWrite(F, Format('%.3f ', [(Gmatrix^[(i - 1) * Fnphases + j])]));
                if i <> Fnphases then
                    FSWrite(F, '|');
            end;
            FSWriteln(F, ')');
        end;
        FSWriteln(F, Format('~ %s=%.3f', [PropertyName^[7], ON_Time]));
        if IsTemporary then
            FSWriteln(F, '~ ' + PropertyName^[8] + '= Yes')
        else
            FSWriteln(F, '~ ' + PropertyName^[8] + '= No');
        FSWriteln(F, Format('~ %s=%.1f', [PropertyName^[9], Minamps]));

        for i := NumPropsthisClass to NumProperties do
        begin
            FSWriteln(F, '~ ' + PropertyName^[i] + '=' + PropertyValue[i]);
        end;

        if Complete then
        begin
            FSWriteln(F, Format('// SpecType=%d', [SpecType]));
        end;
    end;
end;

function PresentTimeInSec(DSS: TDSSContext): Double;
begin
    with DSS.ActiveCircuit.Solution do
        Result := Dynavars.t + DynaVars.intHour * 3600.0;
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
                if (PresentTimeInSec(DSS) > On_Time) and not Cleared then
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

procedure TFaultObj.MakePosSequence();
begin
    if Fnphases > 1 then
        SetInteger(ord(TProp.Phases), 1);
    inherited;
end;

end.