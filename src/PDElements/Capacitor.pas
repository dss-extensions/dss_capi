unit Capacitor;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

//  Basic  capacitor
//
//  Implemented as a two-terminal constant impedance (Power Delivery Element)
//
//  Bus2 connection defaults to 0 node of Bus1 (if Bus2 has the default bus connection
//  at the time Bus1 is defined.  Therefore, if only Bus1 is specified, a shunt capacitor results.
//  If delta connected, Bus2 is set to node zero of Bus1 and nothing is returned in the lower
//  half of YPrim - all zeroes.
//
//  If an ungrounded wye is desired, explicitly set Bus2= and set all nodes the same,
//    e.g. Bus1.4.4.4   (uses 4th node of Bus1 as neutral point)
//        or BusNew.1.1.1  (makes a new bus for the neutral point)
//  You must specify the nodes or you will get a series capacitor!
//
//  A series capacitor is specified simply by setting bus2 and declaring the connection
//  to be Wye.  If the connection is specified as delta, nothing will be connected to Bus2.
//  In fact the number of terminals is set to 1.
//
//  Capacitance may be specified as:
//
//     1.  kvar and kv ratings at base frequency.  impedance.  Specify kvar as total for
//         all phases (all cans assumed equal). For 1-phase, kV = capacitor can kV rating.
//         For 2 or 3-phase, kV is line-line three phase. For more than 3 phases, specify
//         kV as actual can voltage.
//     2.  Capacitance in uF to be used in each phase.  If specified in this manner,
//         the given value is always used whether wye or delta.
//     3.  A nodal C matrix (like a nodal admittance matrix).
//         If conn=wye then 2-terminal through device
//         If conn=delta then 1-terminal.
//         Microfarads.

interface

uses
    Classes,
    Command,
    DSSClass,
    PDClass,
    PDElement,
    UcMatrix,
    ArrayDef;

type
{$SCOPEDENUMS ON}
    TCapacitorProp = (
        INVALID = 0,
        bus1 = 1,
        bus2 = 2,
        phases = 3,
        kvar = 4,
        kv = 5,
        conn = 6,
        cmatrix = 7,
        cuf = 8,
        R = 9,
        XL = 10,
        Harm = 11,
        Numsteps = 12,
        states = 13
    );
    
    TCapacitorConnection = TGeneralConnection;
{$SCOPEDENUMS OFF}

    TCapacitor = class(TPDClass)
    PROTECTED
        procedure DefineProperties; override;  // Add Properties of this class to propName
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TCapacitorObj = class(TPDElement)
    PUBLIC
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

        function get_States(Idx: Integer): Integer;
        procedure set_States(Idx: Integer; const Value: Integer);
        procedure set_LastStepInService(const Value: Integer);

        procedure MakeYprimWork(YprimWork: TcMatrix; iStep: Integer);

        procedure set_NumSteps(const Value: Integer); // 1=kvar, 2=Cuf, 3=Cmatrix

{$IFDEF DSS_CAPI_INCREMENTAL_Y}
        procedure Set_ConductorClosed(Index: Integer; Value: Boolean); OVERRIDE; 
//        procedure Set_Enabled(Value: Boolean); OVERRIDE;
{$ENDIF}

    PUBLIC
        Connection: TCapacitorConnection;

        constructor Create(ParClass: TDSSClass; const CapacitorName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model

        procedure DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;

        function AddStep: Boolean;
        function SubtractStep: Boolean;
        function AvailableSteps: Integer;
        procedure FindLastStepInService;
        property NumSteps: Integer READ FNumSteps WRITE set_NumSteps;
        property States[Idx: Integer]: Integer READ get_States WRITE set_States;
        property Totalkvar: Double READ FTotalkvar;
        property NomKV: Double READ kvrating;
        property LastStepInService: Integer READ FLastStepInService WRITE set_LastStepInService;

        property NumTerminals: Integer READ NumTerm;   // Property to know if the capacitor has 2 terminals

    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    UComplex, DSSUcomplex,
    DSSObjectHelper,
    Utilities,
    Solution,
    DSSHelper,
    TypInfo;

type
    TObj = TCapacitorObj;
    TProp = TCapacitorProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    

constructor TCapacitor.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, CAP_ELEMENT, 'Capacitor');
end;

destructor TCapacitor.Destroy;
begin
    inherited Destroy;
end;

procedure TCapacitor.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // real matrix
    PropertyType[ord(TProp.cmatrix)] := TPropertyType.DoubleSymMatrixProperty;
    PropertyOffset[ord(TProp.cmatrix)] := ptruint(@obj.Cmatrix);
    PropertyOffset2[ord(TProp.cmatrix)] := ptruint(@obj.Fnphases);
    PropertyScale[ord(TProp.cmatrix)] := 1.0e-6;

    // array of doubles
    PropertyType[ord(TProp.kvar)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.kvar)] := ptruint(@obj.FkvarRating);
    PropertyOffset2[ord(TProp.kvar)] := ptruint(@obj.FNumSteps);

    PropertyType[ord(TProp.R)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.R)] := ptruint(@obj.FR);
    PropertyOffset2[ord(TProp.R)] := ptruint(@obj.FNumSteps);

    PropertyType[ord(TProp.XL)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.XL)] := ptruint(@obj.FXL);
    PropertyOffset2[ord(TProp.XL)] := ptruint(@obj.FNumSteps);

    PropertyType[ord(TProp.cuf)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.cuf)] := ptruint(@obj.FC);
    PropertyOffset2[ord(TProp.cuf)] := ptruint(@obj.FNumSteps);
    PropertyScale[ord(TProp.cuf)] := 1.0e-6;

    PropertyType[ord(TProp.Harm)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.Harm)] := ptruint(@obj.FHarm);
    PropertyOffset2[ord(TProp.Harm)] := ptruint(@obj.FNumSteps);

    // array of ints
    PropertyType[ord(TProp.states)] := TPropertyType.IntegerArrayProperty;
    PropertyOffset[ord(TProp.states)] := ptruint(@obj.FStates);
    PropertyOffset2[ord(TProp.states)] := ptruint(@obj.FNumSteps);

    // enum properties
    PropertyType[ord(TProp.conn)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.conn)] := ptruint(@obj.Connection);
    PropertyOffset2[ord(TProp.conn)] := PtrInt(DSS.ConnectionEnum);

    // bus properties
    PropertyType[ord(TProp.bus1)] := TPropertyType.BusProperty;
    PropertyType[ord(TProp.bus2)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus1)] := 1;
    PropertyOffset[ord(TProp.bus2)] := 2;

    // double properties (default type)
    PropertyOffset[ord(TProp.kv)] := ptruint(@obj.kvrating);

    // integer properties
    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    PropertyType[ord(TProp.NumSteps)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.NumSteps)] := ptruint(@obj.FNumSteps);
    PropertyFlags[ord(TProp.NumSteps)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TCapacitor.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TCapacitorObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    S, S2: String;
    StepSize, RStep, XLstep: Double;
    i, dotpos: Integer;
begin
    case Idx of
        ord(TProp.bus1):
            // Special handling for Bus 1
            // Set Bus2 = Bus1.0.0.0
            // Default Bus2 to zero node of Bus1 unless it is previously defined. (Grounded-Y connection)
            if not Bus2Defined and (Fnterms = 2) then   // only for WYE connection
            begin
                // Strip node designations from S
                S := GetBus(1);
                dotpos := Pos('.', S);
                if dotpos > 0 then
                    S2 := Copy(S, 1, dotpos - 1)
                else
                    S2 := Copy(S, 1, Length(S));  // copy up to Dot
                for i := 1 to Fnphases do
                    S2 := S2 + '.0';   // append series of ".0"'s

                SetBus(2, S2);    // default setting for Bus2
                IsShunt := TRUE;
                PrpSequence^[2] := 0; // Reset this for save function
            end;
        ord(TProp.conn):
            case Connection of
                TCapacitorConnection.Delta:
                    Nterms := 1;  // Force reallocation of terminals
                TCapacitorConnection.Wye:
                    if Fnterms <> 2 then
                        Nterms := 2;
            end;
        2:
        begin
            NumTerm := 2;    // Specifies that the capacitor is not connected to ground
            if AnsiCompareText(StripExtension(GetBus(1)), StripExtension(GetBus(2))) <> 0 then
            begin
                IsShunt := FALSE;
                Bus2Defined := TRUE;
            end;
        end;
        ord(TProp.phases):
            if Fnphases <> previousIntVal then
            begin
                NConds := Fnphases;  // Force Reallocation of terminal info
                Yorder := Fnterms * Fnconds;
            end;
        4:
            SpecType := 1;
        7:
            SpecType := 3;
        8:
            SpecType := 2;
        ord(TProp.Numsteps):
        begin
            // reallocate all arrays associated with steps 
            Rstep := 0.0;
            XLstep := 0.0;
            if previousIntVal = 1 then
            begin
                // Save total values to be divided up
                FTotalkvar := Fkvarrating^[1];
                Rstep := FR^[1] * FNumSteps;
                XLstep := FXL^[1] * FNumSteps;
            end;

            // Reallocate arrays  (Must be initialized to nil for first call)
            Reallocmem(FC, Sizeof(FC^[1]) * FNumSteps);
            Reallocmem(FXL, Sizeof(FXL^[1]) * FNumSteps);
            Reallocmem(Fkvarrating, Sizeof(Fkvarrating^[1]) * FNumSteps);
            Reallocmem(FR, Sizeof(FR^[1]) * FNumSteps);
            Reallocmem(FHarm, Sizeof(FHarm^[1]) * FNumSteps);
            Reallocmem(FStates, Sizeof(FStates^[1]) * FNumSteps);

            // Special case for FNumSteps=1
            if previousIntVal = 1 then
            begin
                case SpecType of

                    1:
                    begin  // kvar        // We'll make a multi-step bank of same net size as at present
                        StepSize := FTotalkvar / FNumSteps;
                        for i := 1 to FNumSteps do
                            FkvarRating^[i] := StepSize;
                    end;


                    2:
                    begin  // Cuf           // We'll make a multi-step bank with all the same as first
                        for i := 2 to FNumSteps do
                            FC^[i] := FC^[1];  // Make same as first step
                    end;

                    3:
                    begin  // Cmatrix  // We'll make a multi-step bank with all the same as first
                        // Nothing to do since all will be the same
                    end;

                end;

                case SpecType of

                    1:
                    begin
                        for i := 1 to FNumSteps do
                            FR^[i] := Rstep;
                        for i := 1 to FNumSteps do
                            FXL^[i] := XLstep;
                    end;

                    2, 3:
                    begin   // Make R and XL same as first step
                        for i := 2 to FNumSteps do
                            FR^[i] := FR^[1];
                        for i := 2 to FNumSteps do
                            FXL^[i] := FXL^[1];
                    end;

                end;

                for i := 1 to FNumSteps do
                    Fstates^[i] := 1;   // turn 'em all ON
                LastStepInService := FNumSteps;
                for i := 2 to FNumSteps do
                    FHarm^[i] := FHarm^[1];  // tune 'em all the same as first
            end;
        end;

        ord(TProp.XL):
        begin
            for i := 1 to Fnumsteps do
                if FXL^[i] <> 0.0 then
                    if FR^[i] = 0.0 then
                        FR^[i] := Abs(FXL^[i]) / 1000.0;  // put in something so it doesn't fail
            DoHarmonicRecalc := FALSE;  // XL is specified
        end;
        11:
            DoHarmonicRecalc := TRUE;
        13:
            FindLastStepInService;
    end;

    //YPrim invalidation on anything that changes impedance values
    case Idx of
        3..8:
            YprimInvalid := TRUE;
        12, 13:
        // Numsteps, states:
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
            // For changes in NumSteps and States, try to handle it incrementally
            if ((ActiveCircuit.Solution.SolverOptions and $FFFFFFFF) <> ord(TSolverOptions.ReuseNothing)) and 
                (not ActiveCircuit.Solution.SystemYChanged) and 
                (YPrim <> NIL) and 
                (not YPrimInvalid)
            then
                ActiveCircuit.IncrCktElements.Add(self)
            else
{$ENDIF}
                YprimInvalid := TRUE;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TCapacitorObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);  // Take care of inherited class properties
    Other := TObj(OtherPtr);
    if Fnphases <> Other.Fnphases then
    begin
        FNPhases := Other.Fnphases;
        NConds := Fnphases; // force reallocation of terminals and conductors

        Yorder := Fnconds * Fnterms;
        YPrimInvalid := TRUE;
    end;

    NumSteps := Other.NumSteps;

    for i := 1 to FNumSteps do
    begin
        FC^[i] := Other.FC^[i];
        Fkvarrating^[i] := Other.Fkvarrating^[i];
        FR^[i] := Other.FR^[i];
        FXL^[i] := Other.FXL^[i];
        FXL^[i] := Other.FXL^[i];
        FHarm^[i] := Other.FHarm^[i];
        Fstates^[i] := Other.Fstates^[i];
    end;

    kvrating := Other.kvrating;
    Connection := Other.Connection;
    SpecType := Other.SpecType;

    if Other.Cmatrix = NIL then
        Reallocmem(Cmatrix, 0)
    else
    begin
        Reallocmem(Cmatrix, SizeOf(Cmatrix^[1]) * Fnphases * Fnphases);
        for i := 1 to Fnphases * Fnphases do
            Cmatrix^[i] := Other.Cmatrix^[i];
    end;
end;

constructor TCapacitorObj.Create(ParClass: TDSSClass; const CapacitorName: String);
begin
    inherited Create(ParClass);
    Name := AnsiLowerCase(CapacitorName);
    DSSObjType := ParClass.DSSClassType;

    FNPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 2;  // Force allocation of terminals and conductors

    Setbus(2, (GetBus(1) + '.0.0.0'));  // Default to grounded wye

    IsShunt := TRUE;  // defaults to shunt capacitor

    Cmatrix := NIL;

    // Initialize these pointers to Nil so reallocmem will work reliably
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

    Connection := TCapacitorConnection.Wye;
    SpecType := 1; // 1=kvar, 2=Cuf, 3=Cmatrix

    NormAmps := FkvarRating^[1] * SQRT3 / kvrating * 1.35;   // 135%
    EmergAmps := NormAmps * 1.8 / 1.35;   //180%
    FaultRate := 0.0005;
    PctPerm := 100.0;
    HrsToRepair := 3.0;
    Yorder := Fnterms * Fnconds;

    DoHarmonicRecalc := FALSE;
    Bus2Defined := FALSE;

    RecalcElementData;
    NumTerm := 1;
end;

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

procedure TCapacitorObj.RecalcElementData;
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
                TCapacitorConnection.Delta:
                    PhasekV := kVRating;
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
                TCapacitorConnection.Delta:
                begin
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

procedure TCapacitorObj.CalcYPrim;
var
    i: Integer;
    YPrimTemp, YPrimWork: TCMatrix;
begin
    // Normally build only Yprim Shunt, but if there are 2 terminals and
    // Bus1 <> Bus 2
    if (Yprim = NIL) OR (Yprim.order <> Yorder) OR (Yprim_Shunt = NIL) OR (Yprim_Series = NIL) {YPrimInvalid} then
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
            MakeYprimWork(YprimWork, i);
            YprimTemp.AddFrom(YprimWork);
        end;

    YPrimWork.Free;

   // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages doesn't fail
    if IsShunt then
        for i := 1 to Yorder do
            Yprim_Series.SetElement(i, i, Yprim_Shunt.Getelement(i, i) * 1.0e-10);


    Yprim.Copyfrom(YPrimTemp);

    // Don't Free YPrimTemp - It's just a pointer to an existing complex matrix

    inherited CalcYPrim;

    YprimInvalid := FALSE;
end;

procedure TCapacitorObj.DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean);
var
    i: Integer;
begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            FSWriteln(F, '~ ' + PropertyName^[i] + '=' + PropertyValue[i]);
        end;

    if Complete then
    begin
        FSWriteln(F, 'SpecType=', IntToStr(SpecType));
    end;
end;

procedure TCapacitorObj.MakePosSequence();
var
    kvarperphase, phasekV, Cs, Cm: Double;
    i, j: Integer;
    newkvars: Array of Double;
begin
    // If FnPhases>1 Then -- do same for 1-phase, too
    case SpecType of
        1: // kvar
        begin 
            if (FnPhases > 1) or (Connection <> TCapacitorConnection.Wye) then
                PhasekV := kVRating / SQRT3
            else
                PhasekV := kVRating;

            // 1-6-16  do caps Like load ...
            SetLength(newkvars, FNumSteps);
            for i := 1 to FNumSteps do
            begin
                kvarPerPhase := FkvarRating^[i] / 3.0;  // divide the total kvar equally among3 phases.../Fnphases;
                newkvars[i - 1] := kvarPerPhase;
            end;
            BeginEdit(True);
            SetInteger(ord(TProp.Phases), 1);
            SetDouble(ord(TProp.kV), PhasekV);
            SetDoubles(ord(TProp.kvar), newkvars);
            EndEdit(3);
            // Leave R as specified
        end;
        2: //
            SetInteger(ord(TProp.Phases), 1);
        3:
            if FnPhases > 1 then
            begin //  C Matrix
                Cs := 0.0;   // Avg Self
                for i := 1 to FnPhases do
                    Cs := Cs + Cmatrix^[(i - 1) * Fnphases + i];
                Cs := Cs / FnPhases;

                Cm := 0.0;     //Avg mutual
                for i := 2 to FnPhases do
                    for j := i to FnPhases do
                        Cm := Cm + Cmatrix^[(i - 1) * Fnphases + j];
                Cm := Cm / (FnPhases * (Fnphases - 1.0) / 2.0);
                BeginEdit(True);
                SetInteger(ord(TProp.Phases), 1);
                SetDouble(ord(TProp.Cuf), (Cs - Cm));
                EndEdit(2);
            end;
    end;
    inherited;
end;


function TCapacitorObj.get_States(Idx: Integer): Integer;
begin
    Result := FStates^[Idx];
end;

procedure TCapacitorObj.set_States(Idx: Integer; const Value: Integer);
begin
    if FStates^[Idx] <> Value then
    begin
        FStates^[Idx] := Value;

{$IFDEF DSS_CAPI_INCREMENTAL_Y}
        if ((ActiveCircuit.Solution.SolverOptions and $FFFFFFFF) <> ord(TSolverOptions.ReuseNothing)) and 
           (not ActiveCircuit.Solution.SystemYChanged) and 
           (YPrim <> NIL) and 
           (not YPrimInvalid)
        then
            // Mark this to incrementally update the matrix.
            // If the matrix is already being rebuilt, there is 
            // no point in doing this, just rebuild it as usual.
           ActiveCircuit.IncrCktElements.Add(Self)
        else
{$ENDIF}        
        YprimInvalid := TRUE;
    end;
end;

procedure TCapacitorObj.set_NumSteps(const Value: Integer);
// Special case for changing from 1 to more ..  Automatically make a new bank
var
    prev: Integer;
begin
    if (Value <= 0) or (FNumSteps = Value) then Exit;

    prev := FNumSteps;
    FNumSteps := value;
    PropertySideEffects(ord(TProp.numsteps), prev);
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
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
        if ((ActiveCircuit.Solution.SolverOptions and $FFFFFFFF) <> ord(TSolverOptions.ReuseNothing)) and 
           (not ActiveCircuit.Solution.SystemYChanged) and 
           (YPrim <> NIL) and 
           (not YPrimInvalid)
        then
           ActiveCircuit.IncrCktElements.Add(Self)
{$ENDIF}
        else
            YprimInvalid := TRUE;

    FLastStepInService := Value;
end;

procedure TCapacitorObj.MakeYprimWork(YprimWork: TcMatrix; iStep: Integer);
// call this routine only if step is energized
var
    Value, Value2,
    ZL: Complex;
    i, j, ioffset: Integer;
    w, FreqMultiple: Double;
    HasZL: Boolean;
begin
    with YprimWork do
    begin
        FYprimFreq := ActiveCircuit.Solution.Frequency;
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

        // Now, Put C into in Yprim matrix

        case SpecType of

            1, 2:
            begin
                Value := Cmplx(0.0, FC^[iSTep] * w);
                case Connection of
                    TCapacitorConnection.Delta:
                    begin   // Line-Line
                        Value2 := Value * 2.0;
                        Value := -Value;
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
                        Value := Cinv(ZL + Cinv(Value)); // add in ZL
                    Value2 := -Value;
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
                        Value := -Value;
                        SetElemSym(i, j + Fnphases, Value);
                    end;
                end;
            end;
        end;

        // Add line reactance for filter reactor, if any
        if HasZL then
            case SpecType of

                1, 2:
                    case Connection of
                        TCapacitorConnection.Delta: // Line-Line
                        begin
                            // Add a little bit to each phase so it will invert
                            for i := 1 to Fnphases do
                            begin
                                SetElement(i, i, GetElement(i, i) * 1.000001);
                            end;
                            Invert;
                            for i := 1 to Fnphases do
                            begin
                                Value := ZL + GetElement(i, i);
                                SetElement(i, i, Value);
                            end;
                            Invert;
                        end;
                    else // WYE - just put ZL in series
                        // DO Nothing; Already in - see above
                    end;

                3:
                begin
                    Invert;
                    for i := 1 to Fnphases do
                    begin
                        Value := ZL + GetElement(i, i);
                        SetElement(i, i, Value);
                    end;
                    Invert;
                end;
            end;

    end;
end;

function TCapacitorObj.AddStep: Boolean;
begin
    // Start with last step in service and see if we can add more.  If not return FALSE
    if LastStepInService = FNumSteps then
        Result := FALSE
    else
    begin
        Inc(FLastStepInService);
        States[FLastStepInService] := 1;
        Result := TRUE;
    end;
end;

function TCapacitorObj.SubtractStep: Boolean;
begin
    if LastStepInService = 0 then
        Result := FALSE
    else
    begin
        States[FLastStepInService] := 0;
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

{$IFDEF DSS_CAPI_INCREMENTAL_Y}
procedure TCapacitorObj.Set_ConductorClosed(Index: Integer; Value: Boolean);
var
    i: Integer;
begin
    if (Index = 0) then
    begin  // Do all conductors
        for i := 1 to Fnphases do
            Terminals[ActiveTerminalIdx - 1].ConductorsClosed[i - 1] := Value; //TODO: why not use the ActiveTerminal directly?
        
        if ((ActiveCircuit.Solution.SolverOptions and $FFFFFFFF) <> ord(TSolverOptions.ReuseNothing)) and 
           (not ActiveCircuit.Solution.SystemYChanged) and 
           (YPrim <> NIL) and 
           (not YPrimInvalid)
        then
            // Mark this to incrementally update the matrix.
            // If the matrix is already being rebuilt, there is 
            // no point in doing this, just rebuild it as usual.
           ActiveCircuit.IncrCktElements.Add(Self)
        else
            YPrimInvalid := TRUE; // this also sets the global SystemYChanged flag
    end
    else
    if (Index > 0) and (Index <= Fnconds) then
    begin
        Terminals[ActiveTerminalIdx - 1].ConductorsClosed[index - 1] := Value;
            
        if ((ActiveCircuit.Solution.SolverOptions and $FFFFFFFF) <> ord(TSolverOptions.ReuseNothing)) and 
           (not ActiveCircuit.Solution.SystemYChanged) and 
           (YPrim <> NIL) and 
           (not YPrimInvalid)
        then
           ActiveCircuit.IncrCktElements.Add(Self)
        else
            YPrimInvalid := TRUE;
    end;
end;

// procedure TCapacitorObj.Set_Enabled(Value: Boolean);
// begin
//     if (DSS_CAPI_ALLOW_INCREMENTAL_Y) and 
//        (not ActiveCircuit.Solution.SystemYChanged) and 
//        (YPrim <> NIL) and 
//        (not YPrimInvalid) and
//        (NumTerm = 1) // Assumes disabling shunt capacitors have no ill-effects
//     begin
//        ActiveCircuit.IncrCktElements.Add(Self);
//         FEnabled := Value;
//         Exit;
//     end;
// 
//     // Fallback to the inherited CktElement procedure
//     Inherited Set_Enabled(Value);
// end;

{$ENDIF}

end.