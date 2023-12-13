unit Fault;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

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
    TFaultPropLegacy = (
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
    TFaultProp = (
        INVALID = 0,
        Bus1 = 1,
        Bus2 = 2,
        Phases = 3,
        R = 4,
        pctStdDev = 5,
        GMatrix = 6,
        OnTime = 7,
        Temporary = 8,
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
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        procedure Randomize;
        procedure CheckStatus(ControlMode: Integer);
        procedure Reset;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model

        procedure DumpProperties(F: TStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;
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
    TPropLegacy = TFaultPropLegacy;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;    

constructor TFault.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
    end;

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
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, PropInfoLegacy);

    SpecSetNames := ArrayOfString.Create(
        'r',
        'Gmatrix'
    );
    SpecSets := TSpecSets.Create(
        TSpecSet.Create(ord(TProp.r)),
        TSpecSet.Create(ord(TProp.Gmatrix))
    );

    // real matrix
    PropertyType[ord(TProp.Gmatrix)] := TPropertyType.DoubleSymMatrixProperty;
    PropertyOffset[ord(TProp.Gmatrix)] := ptruint(@obj.Gmatrix);
    PropertyOffset3[ord(TProp.Gmatrix)] := ptruint(@obj.Fnphases);
    PropertyFlags[ord(TProp.Gmatrix)] := [TPropertyFlag.RequiredInSpecSet];

    // integer properties
    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    // bus properties
    PropertyType[ord(TProp.bus1)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus1)] := 1;
    PropertyFlags[ord(TProp.bus1)] := [TPropertyFlag.Required];

    PropertyType[ord(TProp.bus2)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus2)] := 2;

    // boolean properties
    PropertyType[ord(TProp.temporary)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.temporary)] := ptruint(@obj.IsTemporary);

    // percent properties
    PropertyScale[ord(TProp.pctstddev)] := 0.01;
    PropertyOffset[ord(TProp.pctstddev)] := ptruint(@obj.StdDev);

    // double properties (default type)
    PropertyOffset[ord(TProp.ONtime)] := ptruint(@obj.ON_Time);
    PropertyFlags[ord(TProp.ONtime)] := [TPropertyFlag.Units_s];
    
    PropertyOffset[ord(TProp.MinAmps)] := ptruint(@obj.MinAmps);
    PropertyFlags[ord(TProp.MinAmps)] := [TPropertyFlag.Units_A];

    PropertyOffset[ord(TProp.r)] := ptruint(@obj.G);
    PropertyFlags[ord(TProp.r)] := [TPropertyFlag.InverseValue, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.Units_ohm];

    ActiveProperty := NumPropsThisClass;

    inherited DefineProperties;

    //TODO: fully remove some inherited properties like normamps/emergamps?
    // Currently, this just suppresses them from the JSON output/schema
    PropertyFlags[PropertyOffset_PDClass + ord(TPDElementProp.normamps)] := [TPropertyFlag.SuppressJSON];
    PropertyFlags[PropertyOffset_PDClass + ord(TPDElementProp.emergamps)] := [TPropertyFlag.SuppressJSON];
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

procedure TFaultObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags);
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
            if (DSS_EXTENSIONS_COMPAT and ord(TDSSCompatFlags.NoPropertyTracking)) = 0 then
            begin
                PrpSequence[ord(TProp.Gmatrix)] := 0;
            end;
        end;
        ord(TProp.Gmatrix):
        begin
            SpecType := 2;
            if (DSS_EXTENSIONS_COMPAT and ord(TDSSCompatFlags.NoPropertyTracking)) = 0 then
            begin
                PrpSequence[ord(TProp.r)] := 0;
            end;
        end;
        ord(TProp.ONtime):
            if ON_Time > 0.0 then
                Is_ON := FALSE;   // Assume fault will be on later
    end;

        //YPrim invalidation on anything that changes impedance values
    case Idx of
        3, 4, 6:
            YprimInvalid := TRUE;
    end;
    inherited PropertySideEffects(Idx, previousIntVal, setterFlags);
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
        Reallocmem(Gmatrix, SizeOf(Gmatrix[1]) * Fnphases * Fnphases);
        for i := 1 to Fnphases * Fnphases do
            Gmatrix[i] := Other.Gmatrix[i];
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
    SpecType := 1; // G (r property)  2=Gmatrix
    if (DSS_EXTENSIONS_COMPAT and ord(TDSSCompatFlags.NoPropertyTracking)) = 0 then
    begin
        SetAsNextSeq(ord(TProp.r)); // ensure value is marked as filled
    end;

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
    case ActiveCircuit.Solution.RandomType of
        GAUSSIAN:
            RandomMult := Gauss(1.0, StdDev);
        UNIFORM:
            RandomMult := Random;
        LOGNORMAL:
            RandomMult := QuasiLogNormal(1.0);
    else
        RandomMult := 1.0;
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

    // Now, Put in Yprim matrix
    // If the fault is not ON, the set zero conductance
    case SpecType of
        1:
        begin
            if Is_ON then
                Value := G / RandomMult
            else
                Value := 0;
            Value2 := -Value;
            for i := 1 to Fnphases do
            begin
                YPrimTemp[i, i] := Value;     // Elements are only on the diagonals
                YPrimTemp[i + Fnphases, i + Fnphases] := Value;
                YPrimTemp[i, i + Fnphases] := Value2;
                YPrimTemp[i + Fnphases, i] := Value2;
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
                        Value := Cmplx(Gmatrix[(iOffset + j)] / RandomMult, 0.0)
                    else
                        Value := 0;
                    YPrimTemp[i, j] := Value;
                    YPrimTemp[i + Fnphases, j + Fnphases] := Value;
                    Value := -Value;
                    YPrimTemp[i, j + Fnphases] := Value;
                    YPrimTemp[j + Fnphases, i] := Value;
                end;
            end;
        end;
    end;


    YPrim.CopyFrom(YPrimTemp);

    inherited CalcYPrim;
    YprimInvalid := FALSE;
end;

procedure TFaultObj.DumpProperties(F: TStream; Complete: Boolean; Leaf: Boolean);
var
    i, j: Integer;
begin
    inherited DumpProperties(F, complete);

    FSWriteln(F, '~ ' + ParentClass.PropertyName[ord(TProp.Bus1)] + '=' + firstbus);
    FSWriteln(F, '~ ' + ParentClass.PropertyName[ord(TProp.Bus2)] + '=' + nextbus);

    FSWriteln(F, Format('~ %s=%d', [ParentClass.PropertyName[ord(TProp.Phases)], Fnphases]));
    FSWriteln(F, Format('~ %s=%.2f', [ParentClass.PropertyName[ord(TProp.R)], (1.0 / G)]));
    FSWriteln(F, Format('~ %s=%.1f', [ParentClass.PropertyName[ord(TProp.pctStdDev)], (StdDev * 100.0)]));
    if Gmatrix <> NIL then
    begin
        FSWrite(F, '~ ' + ParentClass.PropertyName[ord(TProp.GMatrix)] + '= (');
        for i := 1 to Fnphases do
        begin
            for j := 1 to i do
                FSWrite(F, Format('%.3f ', [(Gmatrix[(i - 1) * Fnphases + j])]));
            if i <> Fnphases then
                FSWrite(F, '|');
        end;
        FSWriteln(F, ')');
    end;
    FSWriteln(F, Format('~ %s=%.3f', [ParentClass.PropertyName[ord(TProp.OnTime)], ON_Time]));
    if IsTemporary then
        FSWriteln(F, '~ ' + ParentClass.PropertyName[ord(TProp.Temporary)] + '= Yes')
    else
        FSWriteln(F, '~ ' + ParentClass.PropertyName[ord(TProp.Temporary)] + '= No');
    FSWriteln(F, Format('~ %s=%.1f', [ParentClass.PropertyName[ord(TProp.MinAmps)], Minamps]));

    for i := NumPropsthisClass to ParentClass.NumProperties do
    begin
        FSWriteln(F, '~ ' + ParentClass.PropertyName[i] + '=' + PropertyValue[i]);
    end;

    if Complete then
    begin
        FSWriteln(F, Format('// SpecType=%d', [SpecType]));
    end;
end;

function PresentTimeInSec(DSS: TDSSContext): Double;
begin
    Result := DSS.ActiveCircuit.Solution.Dynavars.t + DSS.ActiveCircuit.Solution.DynaVars.intHour * 3600.0;
end;

procedure TFaultObj.CheckStatus(ControlMode: Integer);
begin
    case ControlMode of
        CTRLSTATIC:   // Leave it however it is defined by other processes
        begin
        end;
        EVENTDRIVEN,
        MULTIRATE,
        TIMEDRIVEN:
        begin
            if not Is_ON then
            begin   // Turn it on unless it has been previously cleared
                if (PresentTimeInSec(DSS) > On_Time) and not Cleared then
                begin
                    Is_ON := TRUE;
                    YPrimInvalid := TRUE;
                    AppendtoEventLog(FullName, '**APPLIED**');
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
                        AppendtoEventLog(FullName, '**CLEARED**');
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
        if Cabs(Iterminal[i]) > MinAmps then
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
        SetInteger(ord(TProp.Phases), 1, []);
    inherited;
end;

end.
