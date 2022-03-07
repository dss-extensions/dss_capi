unit IndMach012;

// Symmetrical component Induction Machine model

//    ************  DRAFT Version 2 ******************************

{
  ----------------------------------------------------------
  Copyright (c) 2008-2017, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

//   Change Log
//
//   November 10, 2016
//
//   Created by
//     Andres Ovalle
//     Celso Rocha
//

interface

uses
    Classes,
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    UComplex, DSSUcomplex,
    ArrayDef,
    LoadShape,
    GrowthShape,
    Spectrum,
    Dynamics,
    GeneratorVars;

type
{$SCOPEDENUMS ON}    
    TIndMach012Prop = (
        INVALID = 0,
        phases = 1,
        bus1 = 2,
        kv = 3,
        kW = 4,
        pf = 5, 
        conn = 6, 
        kVA = 7,
        H = 8,
        D = 9,
        puRs = 10,
        puXs = 11,
        puRr = 12,
        puXr = 13,
        puXm = 14,
        Slip = 15,
        MaxSlip = 16,
        SlipOption = 17,
        Yearly = 18,
        Daily = 19,
        Duty = 20,
        Debugtrace = 21
    );
{$SCOPEDENUMS OFF}

    TIndMach012 = class(TPCClass)
    PROTECTED
        cBuffer: TCBuffer24; // Temp buffer for complex math calcs; allows up to 24-phase models.

        procedure DefineProperties; override;    // Define the property names and help strings
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE; // This function is called by the DSS New command
    end;

    TSymCompArray = array[0..2] of Complex;

    TIndMach012Obj = class(TPCElement)
    PRIVATE
        {Private variables of this class}
        Connection: Integer;  {0 = line-neutral; 1=Delta}
        Yeq: Complex;   // Y at nominal voltage

        puRs, puXs, puRr, puXr, puXm,
        S1,        // Pos seq slip
        S2,
        MaxSlip,  // limit for slip to prevent solution blowing up
        dSdP,  // for power flow

        {Dynamics variables}
        Xopen,
        Xp,
        T0p // Rotor time constant
        : Double;

        InDynamics: Boolean;

        Zs, Zm, Zr,
        Is1, Ir1, V1,    // Keep the last computed voltages and currents
        Is2, Ir2, V2: Complex;

        {Complex variables for dynamics}
        E1, E1n, dE1dt, dE1dtn,
        E2, E2n, dE2dt, dE2dtn,
        Zsp: Complex;

        FirstIteration: Boolean;
        FixedSlip: LongBool;

        RandomMult: Double;
        IndMach012SwitchOpen: Boolean;

        // Debugging
        TraceFile: TFileStream;
        DebugTrace: LongBool;

        MachineData: TGeneratorVars;    // Use generator variable structure

        MachineON: Boolean;
        ShapeFactor: Complex;
        ShapeIsActual: Boolean;

        VBase: Double;
        kWBase: Double;

        procedure set_Localslip(const Value: Double);

        procedure Get_PFlowModelCurrent(const V: Complex; const S: Double; var Istator, Irotor: Complex);
        procedure Get_DynamicModelCurrent;
        
        function GetRotorLosses: Double;
        function GetStatorLosses: Double;
        function Compute_dSdP: Double;
        procedure Randomize(Opt: Integer);
        procedure InitModel(V012, I012: TSymCompArray);

        procedure CalcYPrimMatrix(Ymatrix: TcMatrix);
        procedure CalcIndMach012ModelContribution;

        procedure DoIndMach012Model;

        procedure CalcModel(V, I: pComplexArray);

        procedure CalcDailyMult(Hr: Double);
        procedure CalcYearlyMult(Hr: Double);
        procedure CalcDutyMult(Hr: Double);

        procedure InitTraceFile;
        procedure WriteTraceRecord;
        procedure SetPowerkW(const PkW: Double);

    PROTECTED
        procedure GetTerminalCurrents(Curr: pComplexArray); OVERRIDE;
        procedure DoDynamicMode;
        procedure DoHarmonicMode;
    PUBLIC
        DailyDispShapeObj: TLoadShapeObj;  // Daily Generator Shape for this load
        DutyShapeObj: TLoadShapeObj;  // Shape for this generator
        YearlyShapeObj: TLoadShapeObj;  // Shape for this Generator

        constructor Create(ParClass: TDSSClass; const IndMach012ObjName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure Set_ConductorClosed(Index: Integer; Value: Boolean); OVERRIDE;
        procedure RecalcElementData; OVERRIDE;   // Generally called after Edit is complete to recompute variables
        procedure CalcYPrim; OVERRIDE;   // Calculate Primitive Y matrix
        procedure Integrate;
        procedure CalcDynamic(var V012, I012: TSymCompArray);
        procedure CalcPFlow(var V012, I012: TSymCompArray);
        procedure SetNominalPower;

        function InjCurrents: Integer; OVERRIDE;
        function NumVariables: Integer; OVERRIDE;
        procedure GetAllVariables(States: pDoubleArray); OVERRIDE;
        function Get_Variable(i: Integer): Double; OVERRIDE;
        procedure Set_Variable(i: Integer; Value: Double); OVERRIDE;
        function VariableName(i: Integer): String; OVERRIDE;

        // Support for Dynamics Mode
        procedure InitStateVars; OVERRIDE;
        procedure IntegrateStates; OVERRIDE;
        // Support for Harmonics Mode
        procedure InitHarmonics; OVERRIDE;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model, if possible
    end;

implementation

uses
    BufStream,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    Command,
    Sysutils,
    Math,
    MathUtil,
    Utilities,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TIndMach012Obj;
    TProp = TIndMach012Prop;
const
    NumPropsThisClass = Ord(High(TProp));
    NumIndMach012Variables = 22;
var  
    PropInfo: Pointer = NIL;    
    SlipOptionEnum : TDSSEnum;

constructor TIndMach012.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        SlipOptionEnum := TDSSEnum.Create('IndMach012: Slip Option', True, 1, 1, 
            ['VariableSlip', 'FixedSlip'], [0, Integer(True)]);
        SlipOptionEnum.DefaultValue := 0;
    end;

    inherited Create(dssContext, INDMACH012_ELEMENT, 'IndMach012');
end;

destructor TIndMach012.Destroy;
begin
    inherited Destroy;
end;

function PowerFactorProperty(obj: TObj): Double;
begin
    Result := PowerFactor(obj.Power[1]);
end;

procedure SetLocalSlip(Obj: TObj; Value: Double);
begin
    obj.set_Localslip(Value);
end;

procedure TIndMach012.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // integer
    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    // special function properties
    PropertyFlags[ord(TProp.pf)] := [TPropertyFlag.SilentReadOnly, TPropertyFlag.ReadByFunction];
    PropertyReadFunction[ord(TProp.pf)] := @PowerFactorProperty;

    // enum properties
    PropertyType[ord(TProp.conn)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.conn)] := ptruint(@obj.Connection);
    PropertyOffset2[ord(TProp.conn)] := PtrInt(DSS.ConnectionEnum);

    PropertyType[ord(TProp.SlipOption)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.SlipOption)] := ptruint(@obj.Fixedslip); // LongBool as Integer
    PropertyOffset2[ord(TProp.SlipOption)] := PtrInt(SlipOptionEnum);

    // bus properties
    PropertyType[ord(TProp.bus1)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus1)] := 1;

    // boolean properties
    PropertyType[ord(TProp.Debugtrace)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.Debugtrace)] := ptruint(@obj.DebugTrace);

    // object properties
    PropertyType[ord(TProp.yearly)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.daily)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.duty)] := TPropertyType.DSSObjectReferenceProperty;
    
    PropertyOffset[ord(TProp.yearly)] := ptruint(@obj.YearlyShapeObj);
    PropertyOffset[ord(TProp.daily)] := ptruint(@obj.DailyDispShapeObj);
    PropertyOffset[ord(TProp.duty)] := ptruint(@obj.DutyShapeObj);

    PropertyOffset2[ord(TProp.yearly)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.daily)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.duty)] := ptruint(DSS.LoadShapeClass);

    // double properties (default type)
    PropertyOffset[ord(TProp.kW)] := ptruint(@obj.kWBase);
    PropertyOffset[ord(TProp.puRs)] := ptruint(@obj.puRs);
    PropertyOffset[ord(TProp.puXs)] := ptruint(@obj.puXs);
    PropertyOffset[ord(TProp.puRr)] := ptruint(@obj.puRr);
    PropertyOffset[ord(TProp.puXr)] := ptruint(@obj.puXr);
    PropertyOffset[ord(TProp.puXm)] := ptruint(@obj.puXm);
    PropertyOffset[ord(TProp.MaxSlip)] := ptruint(@obj.MaxSlip);
    PropertyOffset[ord(TProp.H)] := ptruint(@obj.MachineData.Hmass);
    PropertyOffset[ord(TProp.D)] := ptruint(@obj.MachineData.D);
    PropertyOffset[ord(TProp.kVA)] := ptruint(@obj.MachineData.kVArating);
    PropertyOffset[ord(TProp.kV)] := ptruint(@obj.MachineData.kVGeneratorBase);

    // advanced double
    PropertyOffset[ord(TProp.slip)] := ptruint(@obj.S1);
    PropertyWriteFunction[ord(TProp.slip)] := @SetLocalSlip;
    PropertyFlags[ord(TProp.slip)] := [TPropertyFlag.WriteByFunction];

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TIndMach012.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure SetNcondsForConnection(Obj: TObj);
begin
    with Obj do
        case Connection of
            0:
                NConds := Fnphases;  // Neutral is not connected for induction machine
            1:
                case Fnphases of        // Delta connection
                    1, 2:
                        NConds := Fnphases + 1; // L-L and Open-delta
                else
                    NConds := Fnphases;    // no neutral for this connection
                end;
        end;
end;

procedure TIndMach012Obj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    case Idx of
        ord(TProp.phases):
            SetNCondsForConnection(self);  // Force Reallocation of terminal info
        ord(TProp.kV):
            with MachineData do
                case FNphases of
                    2, 3:
                        VBase := kVGeneratorBase * InvSQRT3x1000;
                else
                    VBase := kVGeneratorBase * 1000.0;
                end;
        ord(TProp.slip):
            MachineData.Speed := MachineData.w0 * (-S1); // make motor speed agree
        18:
            if Assigned(YearlyShapeObj) then
                with YearlyShapeObj do
                    if UseActual then
                        SetPowerkW(MaxP);
        19:
            if Assigned(DailyDispShapeObj) then
                with DailyDispShapeObj do
                    if UseActual then
                        SetPowerkW(MaxP);
        20:
            if Assigned(DutyShapeObj) then
                with DutyShapeObj do
                    if UseActual then
                        SetPowerkW(MaxP);
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TIndMach012.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
begin
    with TObj(ptr) do
    begin
        RecalcElementData;
        YPrimInvalid := TRUE;
        Exclude(Flags, Flg.EditionActive);
    end;
    Result := True;
end;

procedure TIndMach012Obj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr);

    Other := TObj(OtherPtr);
    if (Fnphases <> Other.Fnphases) then
    begin
        FNphases := Other.Fnphases;
        NConds := Fnphases;  // Forces reallocation of terminal stuff

        Yorder := Fnconds * Fnterms;
        YPrimInvalid := TRUE;
    end;

    MachineData := Other.MachineData; // record, copy everything at once
    VBase := Other.VBase;
    kWBase := Other.kWBase;

    puRs := Other.puRs;
    puRr := Other.puRr;
    puXr := Other.puXr;
    puXm := Other.puXm;
    puXs := Other.puXs;
    MaxSlip := Other.MaxSlip;
end;

constructor TIndMach012Obj.Create(ParClass: TDSSClass; const IndMach012ObjName: String);
begin
    inherited create(ParClass);
    Name := AnsiLowerCase(IndMach012ObjName);
    DSSObjType := ParClass.DSSClassType; // Same as Parent Class

    TraceFile := nil;
    
    FNphases := 3;
    Fnconds := 3;
    Yorder := 0;
    Nterms := 1;
    kWBase := 1000.0;

    YearlyShapeObj := NIL;  // if YearlyShapeobj = nil then the load alway stays nominal * global multipliers
    DailyDispShapeObj := NIL;  // if DaillyShapeobj = nil then the load alway stays nominal * global multipliers
    DutyShapeObj := NIL;  // if DutyShapeobj = nil then the load alway stays nominal * global multipliers

    Debugtrace := FALSE;

    Yorder := Fnterms * Fnconds;
    ShapeIsActual := FALSE;
    IndMach012SwitchOpen := FALSE;

    Connection := 1;  // Delta Default

    MachineData.kVGeneratorBase := 12.47;

    MachineData.kVArating := kWBase * 1.2;
    with MachineData do
    begin
        Hmass := 1.0;       //  W-sec/VA rating
        Theta := 0.0;
        w0 := TwoPi * Basefrequency;
        Speed := 0.0;  // relative speed
        dSpeed := 0.0;
        D := 1.0;
        XRdp := 20.0;   // not used for indmach

           // newly added
        Conn := connection;
        NumPhases := Fnphases;
        NumConductors := Fnconds;
    end;

    {Typical machine impedance data}
    puRs := 0.0053;
    puXs := 0.106;
    puRr := 0.007;
    puXr := 0.12;
    puXm := 4.0;

    // Set slip local and make generator model agree
    MaxSlip := 0.1;  // 10% slip limit     - set this before setting slip
    set_LocalSlip(0.007);   // About 1 pu power
    PropertySideEffects(ord(TProp.slip));

    FixedSlip := FALSE;  // Allow Slip to float to match specified power

    InDynamics := FALSE;

    RecalcElementData;
end;

destructor TIndMach012Obj.Destroy;
// Free everything here that needs to be freed
// If you allocated anything, dispose of it here
begin
    FreeAndNil(TraceFile);

    inherited Destroy;   // This will take care of most common circuit element arrays, etc.
end;

procedure TIndMach012Obj.RecalcElementData;
var
    Rs, Xs,
    Rr, Xr,
    Xm, ZBase: Double;
begin
    with MachineData do
    begin
        ZBase := Sqr(kVGeneratorBase) / kVArating * 1000.0;
        Conn := connection;
        NumPhases := Fnphases;
        NumConductors := Fnconds;
    end;

    Rs := puRs * ZBase;
    Xs := puXs * ZBase;
    Rr := puRr * ZBase;
    Xr := puXr * ZBase;
    Xm := puXm * ZBase;
    Zs := Cmplx(Rs, Xs);
    Zm := Cmplx(0.0, Xm);
    Zr := Cmplx(Rr, Xr);

    Xopen := Xs + Xm;
    Xp := Xs + (Xr * Xm) / (Xr + Xm);
    Zsp := Cmplx(Rs, Xp);
    //Yeq := Cinv(Zsp);   // for Yprim  for dynamics
    //Yeq := Cmplx(1.0/ZBase, -0.5/Zbase);   // vars are half the watts
    Yeq := Cmplx(0.0, -1.0 / ZBase);   // vars only for power flow
    T0p := (Xr + Xm) / (MachineData.w0 * Rr);

    dSdP := Compute_dSdP;

    Is1 := CZERO;
    V1 := CZERO;
    Is2 := CZERO;
    V2 := CZERO;

    FirstIteration := TRUE;

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

    SetNominalPower;

    if DebugTrace then
        InitTraceFile
    else
        FreeAndNil(TraceFile);
end;

procedure TIndMach012Obj.SetPowerkW(const PkW: Double);
begin
    kWBase := PkW;
end;

procedure TIndMach012Obj.Integrate;
var
    h2: Double;
begin
    with  ActiveCircuit.Solution.Dynavars do
    begin
        if IterationFlag = 0 then
        begin  // on predictor step
            E1n := E1;            // update old values
            dE1dtn := dE1dt;
            E2n := E2;
            dE2dtn := dE2dt;
        end;

        // Derivative of E
        // dEdt = -jw0SE' - (E' - j(X-X')I')/T0'
        dE1dt := cmplx(0.0, -MachineData.w0 * S1) * E1 - (E1 - cmplx(0.0, Xopen - Xp) * Is1) / T0p;
        dE2dt := cmplx(0.0, -MachineData.w0 * S2) * E2 - (E2 - cmplx(0.0, Xopen - Xp) * Is2) / T0p;

        // Trapezoidal Integration
        h2 := h * 0.5;
        E1 := E1n + (dE1dt + dE1dtn) * h2;
        E2 := E2n + (dE2dt + dE2dtn) * h2;
    end;
end;

procedure TIndMach012Obj.CalcDynamic(var V012, I012: TSymCompArray);
begin
      {In dynamics mode, slip is allowed to vary}
    InDynamics := TRUE;
    V1 := V012[1];   // Save for variable calcs
    V2 := V012[2];
    
    {Gets slip from shaft speed}
    with MachineData do
        set_LocalSlip((-Speed) / w0);
    Get_DynamicModelCurrent;

     //  Get_ModelCurrent(V2, S2, Is2, Ir2);
    I012[1] := Is1;    // Save for variable calcs
    I012[2] := Is2;
    I012[0] := cmplx(0.0, 0.0);
end;

procedure TIndMach012Obj.CalcPFlow(var V012, I012: TSymCompArray);
var
    P_Error: Double;
begin
    V1 := V012[1];   // Save for variable calcs
    V2 := V012[2];

    InDynamics := FALSE;

    if FirstIteration then
    begin
        Get_PFlowModelCurrent(V1, S1, Is1, Ir1);  // initialize Is1
        FirstIteration := FALSE;
    end;

      {If Fixed slip option set, then use the value set by the user}
    if not FixedSlip then
    begin
        P_Error := MachineData.PnominalperPhase - (V1 * cong(Is1)).re;
        set_LocalSlip(S1 + dSdP * P_Error);   // make new guess at slip
    end;

    Get_PFlowModelCurrent(V1, S1, Is1, Ir1);
    Get_PFlowModelCurrent(V2, S2, Is2, Ir2);

    I012[1] := Is1;    // Save for variable calcs
    I012[2] := Is2;
    I012[0] := cmplx(0.0, 0.0);
end;

procedure TIndMach012Obj.Randomize(Opt: Integer);
// typical proc for handling randomization in DSS fashion
begin
    case Opt of
        0:
            RandomMult := 1.0;
        //   GAUSSIAN:  RandomMult := Gauss(YearlyShapeObj.Mean, YearlyShapeObj.StdDev);
        UNIFORM:
            RandomMult := Random;  // number between 0 and 1.0
        //  LOGNORMAL: RandomMult := QuasiLognormal(YearlyShapeObj.Mean);
    end;
end;

procedure TIndMach012Obj.InitModel(V012, I012: TSymCompArray);
// Init for Dynamics mode
begin
    // Compute Voltage behind transient reactance and set derivatives to zero
    // *** already done *** E1 := V012[1] - I012[1] * Zsp;
    dE1dt := czero;
    E1n := E1;
    dE1dtn := dE1dt;
    E2 := V012[2] - I012[2] * Zsp;
    dE2dt := czero;
    E2n := E2;
    dE2dtn := dE2dt;
end;

procedure TIndMach012Obj.InitStateVars;
var
    i: Integer;
    V012,
    I012: TSymCompArray;
    Vabc: array[1..3] of Complex;
begin
    YPrimInvalid := TRUE;  // Force rebuild of YPrims

    with MachineData do
    begin
     {Compute nominal Positive sequence voltage behind transient reactance}

        if MachineON then
            with ActiveCircuit.Solution do
            begin
                Yeq := Cinv(Zsp);

                ComputeIterminal;

                case Fnphases of

                    1:
                    begin
                        E1 := NodeV^[NodeRef^[1]] - NodeV^[NodeRef^[2]] - ITerminal^[1] * Zsp;
                    end;

                    3:
                    begin
                        // Calculate E1 based on Pos Seq only
                        Phase2SymComp(ITerminal, pComplexArray(@I012));   // terminal currents

                        // Voltage behind Zsp  (transient reactance), volts
                        for i := 1 to FNphases do
                            Vabc[i] := NodeV^[NodeRef^[i]];   // Wye Voltage
                        Phase2SymComp(pComplexArray(@Vabc), pComplexArray(@V012));
                        E1 := V012[1] - I012[1] * Zsp;    // Pos sequence
                    end;
                else
                    DoSimpleMsg('Dynamics mode is implemented only for 1- or 3-phase Motors. %s has %d phases.', [FullName, Fnphases], 5672);
                    DSS.SolutionAbort := TRUE;
                end;

                InitModel(V012, I012); // E2, etc

                // Shaft variables
                Theta := Cang(E1);
                dTheta := 0.0;
                w0 := Twopi * ActiveCircuit.Solution.Frequency;
                // recalc Mmass and D in case the frequency has changed
                with MachineData do
                begin
                    Mmass := 2.0 * Hmass * kVArating * 1000.0 / (w0);   // M = W-sec
                    D := Dpu * kVArating * 1000.0 / (w0);
                end;
                Pshaft := Power[1].re; // Initialize Pshaft to present power consumption of motor

                Speed := -S1 * w0;    // relative to synch speed
                dSpeed := 0.0;

                if DebugTrace then     // Put in a separator record
                begin
                    FSWriteln(TraceFile);
                    FSWriteln(TraceFile, '*************** Entering Dynamics Mode ***********************');
                    FSWriteln(TraceFile);
                    FSFlush(Tracefile);
                end;

            end
        else
        begin
            Theta := 0.0;
            dTheta := 0.0;
            w0 := 0;
            Speed := 0.0;
            dSpeed := 0.0;
        end;
    end;
end;

procedure TIndMach012Obj.CalcYPrimMatrix(Ymatrix: TcMatrix);
// A typical helper function for PC elements to assist in the computation of Yprim
var
    Y, Yij, Yadder: Complex;
    i, j: Integer;
    FreqMultiplier: Double;
begin
    FYprimFreq := ActiveCircuit.Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;  // ratio to adjust reactances for present solution frequency

    with ActiveCircuit.solution do
        if IsDynamicModel or IsHarmonicModel then
        // for Dynamics and Harmonics modes use constant equivalent Y
        begin
            if MachineON then
                Y := Yeq   // L-N value computed in initialization routines
            else
                Y := Cmplx(EPSILON, 0.0);

            if Connection = 1 then
                Y := Y / 3.0; // Convert to delta impedance
            Y.im := Y.im / FreqMultiplier;  // adjust for frequency
            Yij := -Y;
            for i := 1 to Fnphases do
            begin
                case Connection of
                    0:
                    begin
                        Ymatrix.SetElement(i, i, Y);  // sets the element
                        // Ymatrix.AddElement(Fnconds, Fnconds, Y);  // sums the element
                        // Ymatrix.SetElemsym(i, Fnconds, Yij);
                    end;
                    1:
                    begin   {Delta connection}
                        Yadder := Y * 1.000001;  // to prevent floating delta
                        Ymatrix.SetElement(i, i, Y + Yadder);   // add a little bit to diagonal
                        Ymatrix.AddElement(i, i, Y);  // put it in again
                        for j := 1 to i - 1 do
                            Ymatrix.SetElemsym(i, j, Yij);
                    end;
                end;
            end;
        end
        else
        begin
    //  Typical code for a regular power flow  model
    //  Borrowed from Generator object

       {Yeq is typically expected as the equivalent line-neutral admittance}

            Y := Yeq;  //     Yeq is L-N quantity

       // ****** Need to modify the base admittance for real harmonics calcs
            Y.im := Y.im / FreqMultiplier;

            case Connection of

                0:
                    with YMatrix do
                    begin // WYE
                        for i := 1 to Fnphases do
                        begin
                            SetElement(i, i, Y);
                            {
                            AddElement(Fnconds, Fnconds, Y);
                            SetElemsym(i, Fnconds, Yij);
                            }
                        end;
                    end;

                1:
                    with YMatrix do
                    begin  // Delta  or L-L
                        Y := Y / 3.0; // Convert to delta impedance
                        Yij := -Y;
                        for i := 1 to Fnphases do
                        begin
                            j := i + 1;
                            if j > Fnconds then
                                j := 1;  // wrap around for closed connections
                            AddElement(i, i, Y);
                            AddElement(j, j, Y);
                            AddElemSym(i, j, Yij);
                        end;
                    end;
            end;
        end;  {ELSE IF Solution.mode}
end;

{--- Notes Andres: Added according to IndMach012.dll model }
function TIndMach012Obj.Compute_dSdP: Double;
begin
// dSdP based on rated slip and rated voltage
    V1 := Cmplx(MachineData.kvGeneratorBase * 1000.0 / 1.732, 0.0);
    if S1 <> 0.0 then
        Get_PFlowModelCurrent(V1, S1, Is1, Ir1);
    Result := S1 / (V1 * cong(Is1)).Re;
end;

procedure TIndMach012Obj.CalcYPrim;

// Required routine to calculate the primitive Y matrix for this element

// This example uses a helper function (CalcYPrimMatrix) to keep the code
// here clean

var
    i: Integer;

begin
{
  There are three Yprim matrices that could be computed:

     YPrim_Series:  Used for zero-load solutions to initialize the first guess
     YPrim_Shunt:   Equivalent Y in shunt with power system
                    For PC elements, this is typically the main YPrim
     YPrim:         Generally the sum of the other two; the total YPrim
}

     // Typical PC Elements build only shunt Yprim
     // Also, build a dummy Yprim Series so that CalcVoltagebases does not fail

     // First clear present value; redefine if necessary
     // Note: Complex matrix (TcMatrix -- see uCmatrix.pas) is used for this
    if (Yprim = NIL) OR (Yprim.order <> Yorder) OR (Yprim_Shunt = NIL) OR (Yprim_Series = NIL) {YPrimInvalid} then
    begin
        if YPrim_Shunt <> NIL then
            YPrim_Shunt.Free;
        YPrim_Shunt := TcMatrix.CreateMatrix(Yorder);
        if YPrim_Series <> NIL then
            Yprim_Series.Free;
        YPrim_Series := TcMatrix.CreateMatrix(Yorder);
        if YPrim <> NIL then
            YPrim.Free;
        YPrim := TcMatrix.CreateMatrix(Yorder);
    end

    else
    begin
        YPrim_Shunt.Clear;
        YPrim_Series.Clear;
        YPrim.Clear;
    end;


     // call helper routine to compute YPrim_Shunt
    CalcYPrimMatrix(YPrim_Shunt);

     // Set YPrim_Series based on a small fraction of the diagonals of YPrim_shunt
     // so that CalcVoltages doesn't fail
     // This is just one of a number of possible strategies but seems to work most of the time
    for i := 1 to Yorder do
        Yprim_Series.SetElement(i, i, Yprim_Shunt.Getelement(i, i) * 1.0e-10);

     // copy YPrim_shunt into YPrim; That's all that is needed for most PC Elements
    YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors -- done in base class
    inherited CalcYPrim;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

procedure TIndMach012Obj.DoIndMach012Model;
{Compute total terminal Current }
var
    i: Integer;
begin
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array

    CalcModel(Vterminal, Iterminal);

    IterminalUpdated := TRUE;

    for i := 1 to FNphases do
        InjCurrent^[i] -= Iterminal^[i];
    if (DebugTrace) then
        WriteTraceRecord;
end;

procedure TIndMach012Obj.CalcModel(V, I: pComplexArray); // given voltages returns currents
var
    V012, I012: TSymCompArray;
begin
    // Convert abc voltages to 012
    Phase2SymComp(V, pComplexArray(@V012));

    // compute I012

    case ActiveCircuit.Solution.DynaVars.SolutionMode of
        TSolveMode.DYNAMICMODE:
        begin
            CalcDynamic(V012, I012);
        end;
    else  {All other modes are power flow modes}
    begin
        CalcPflow(V012, I012);
    end;
    end;

    SymComp2Phase(I, pComplexArray(@I012));       // convert back to I abc

end;

procedure TIndMach012Obj.DoDynamicMode;
{ This is an example taken from Generator illustrating how a PC element might
  handle Dynamics mode with a Thevenin equivalent

  Also illustrates the computation of symmetrical component values
}
{Compute Total Current and add into InjTemp}
var
    i: Integer;
begin
   // Start off by getting the current in the admittance branch of the model
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array

   {Inj = -Itotal (in) - Yprim*Vtemp}

    CalcModel(Vterminal, Iterminal);

    IterminalUpdated := TRUE;
    for i := 1 to FNphases do
        InjCurrent^[i] -= ITerminal^[i];
end;

procedure TIndMach012Obj.DoHarmonicMode;
{
  Example taken from Generator illustrating how a PC element might handle
  current calcs for Harmonics mode

  Note: Generator objects assume a Thevenin model (voltage behind and impedance)
        while Load objects assume the Spectrum applies to a Norton model injection current
}

{Compute Injection Current Only when in harmonics mode}

{Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built}
{Vd is the fundamental frequency voltage behind Xd" for phase 1}
var
    i: Integer;
    E: Complex;
    GenHarmonic: Double;
    pBuffer: PCBuffer24;
begin
    pBuffer := @TIndMach012(ParentClass).cBuffer;
   // Set the VTerminal array
    ComputeVterminal;

    with ActiveCircuit.Solution do
    begin
        GenHarmonic := Frequency / BaseFrequency; // harmonic based on the fundamental for this object
        // get the spectrum multiplier and multiply by the V thev (or Norton current for load objects)
      // ???  E := SpectrumObj.GetMult(GenHarmonic) * VThevHarm; // Get base harmonic magnitude
      // ???  RotatePhasorRad(E, GenHarmonic, ThetaHarm);  // Time shift by fundamental frequency phase shift

        // Put the values in a temp complex buffer
        for i := 1 to Fnphases do
        begin
            pBuffer[i] := E;
            if i < Fnphases then
                RotatePhasorDeg(E, GenHarmonic, -120.0);  // Assume 3-phase IndMach012
        end;
    end;

   {Handle Wye Connection}
    if Connection = 0 then
        pBuffer[Fnconds] := Vterminal^[Fnconds];  // assume no neutral injection voltage

   // In this case the injection currents are simply Yprim(frequency) times the voltage buffer
   // Refer to Load.Pas for load-type objects
   {Inj currents = Yprim (E) }
    YPrim.MVMult(InjCurrent, pComplexArray(pBuffer));
end;

procedure TIndMach012Obj.CalcIndMach012ModelContribution;
// Main dispatcher for computing PC Element currnts

// Calculates IndMach012 current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)
begin
    IterminalUpdated := FALSE;
    with  ActiveCircuit, ActiveCircuit.Solution do
    begin
        if IsDynamicModel then
            DoDynamicMode
        else
        if IsHarmonicModel and (Frequency <> Fundamental) then
            DoHarmonicMode
        else
            DoIndMach012Model;

    end; {WITH}
   {When this is done, ITerminal is up to date}
end;

procedure TIndMach012Obj.GetTerminalCurrents(Curr: pComplexArray);
// This function controls the calculation of the total terminal currents

// Note that it only does something if the solution count has changed.
// Otherwise, Iterminal array already contains the currents
begin
    with ActiveCircuit.Solution do
    begin
        if IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount then
        begin     // recalc the contribution
          // You will likely want some logic like this
            if not IndMach012SwitchOpen then
                CalcIndMach012ModelContribution;  // Adds totals in Iterminal as a side effect
        end;
        inherited GetTerminalCurrents(Curr); // add in inherited contribution
    end;
end;

function TIndMach012Obj.InjCurrents: Integer;
// Required function for managing computing of InjCurrents
begin
    with ActiveCircuit.Solution do
    begin
      // Generators and Loads use logic like this:
        if LoadsNeedUpdating then
            SetNominalPower; // Set the nominal kW, etc for the type of solution being done

        // call the main function for doing calculation
        // Difference between currents in YPrim and total terminal current
        if IndMach012SwitchOpen then
            // If the element is open, just zero the array and return
            ZeroInjCurrent
        else
            // otherwise, go to a routine that manages the calculation        
            CalcIndMach012ModelContribution;

      // If (DebugTrace) Then WriteTraceRecord;

       // Add into System Injection Current Array
        Result := inherited InjCurrents;
    end;
end;

procedure TIndMach012Obj.SetNominalPower;
// Set shaft power
var
    Factor: Double;
    MachineOn_Saved: Boolean;
begin
    MachineOn_Saved := MachineON;
    ShapeFactor := CDOUBLEONE;
    // Check to make sure the generation is ON
    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        if not (IsDynamicModel or IsHarmonicModel) then     // Leave machine in whatever state it was prior to entering Dynamic mode
        begin
            MachineON := TRUE;   // Init to on then check if it should be off
        end;

        if not MachineON then
        begin
         // If Machine is OFF enter as tiny resistive load (.0001 pu) so we don't get divide by zero in matrix
            MachineData.Pnominalperphase := -0.1 * kWBase / Fnphases;
          // Pnominalperphase   := 0.0;
            MachineData.Qnominalperphase := 0.0;   // This really doesn't matter
        end
        else
        begin    // Generator is on, compute it's nominal watts and vars
            with Solution do

                case Mode of
                    TSolveMode.SNAPSHOT:
                        Factor := 1.0;
                    TSolveMode.DAILYMODE:
                    begin
                        Factor := 1.0;
                        CalcDailyMult(DynaVars.dblHour) // Daily dispatch curve
                    end;
                    TSolveMode.YEARLYMODE:
                    begin
                        Factor := 1.0;
                        CalcYearlyMult(DynaVars.dblHour);
                    end;
                    TSolveMode.DUTYCYCLE:
                    begin
                        Factor := 1.0;
                        CalcDutyMult(DynaVars.dblHour);
                    end;
                    TSolveMode.GENERALTIME,   // General sequential time simulation
                    TSolveMode.DYNAMICMODE:
                    begin
                        Factor := 1.0;
                                   // This mode allows use of one class of load shape
                        case ActiveCircuit.ActiveLoadShapeClass of
                            USEDAILY:
                                CalcDailyMult(DynaVars.dblHour);
                            USEYEARLY:
                                CalcYearlyMult(DynaVars.dblHour);
                            USEDUTY:
                                CalcDutyMult(DynaVars.dblHour);
                        else
                            ShapeFactor := CDOUBLEONE     // default to 1 + j1 if not known
                        end;
                    end;
                    TSolveMode.MONTECARLO1,
                    TSolveMode.MONTEFAULT,
                    TSolveMode.FAULTSTUDY:
                        Factor := 1.0;
                    TSolveMode.MONTECARLO2,
                    TSolveMode.MONTECARLO3,
                    TSolveMode.LOADDURATION1,
                    TSolveMode.LOADDURATION2:
                    begin
                        Factor := 1.0;
                        CalcDailyMult(DynaVars.dblHour);
                    end;
                    TSolveMode.PEAKDAY:
                    begin
                        Factor := 1.0;
                        CalcDailyMult(DynaVars.dblHour);
                    end;
                    TSolveMode.AUTOADDFLAG:
                        Factor := 1.0;
                else
                    Factor := 1.0
                end;

            if not (IsDynamicModel or IsHarmonicModel) then         //******
            begin
                if ShapeIsActual then
                    MachineData.Pnominalperphase := 1000.0 * ShapeFactor.re / Fnphases
                else
                    MachineData.Pnominalperphase := 1000.0 * kWBase * Factor * ShapeFactor.re / Fnphases;

                // cannot dispatch vars in induction machine
                // you get what you get

            end;
        end; {ELSE GenON}

    end;  {With ActiveCircuit}

   // If machine state changes, force re-calc of Y matrix
    if MachineON <> MachineOn_Saved then
        YPrimInvalid := TRUE;
end;

procedure TIndMach012Obj.CalcDailyMult(Hr: Double);
begin
    if (DailyDispShapeObj <> NIL) then
    begin
        ShapeFactor := DailyDispShapeObj.GetMultAtHour(Hr);
        ShapeIsActual := DailyDispShapeObj.UseActual;
    end
    else
        ShapeFactor := CDOUBLEONE;  // Default to no daily variation
end;

procedure TIndMach012Obj.CalcDutyMult(Hr: Double);
begin
    if DutyShapeObj <> NIL then
    begin
        ShapeFactor := DutyShapeObj.GetMultAtHour(Hr);
        ShapeIsActual := DutyShapeObj.UseActual;
    end
    else
        CalcDailyMult(Hr);  // Default to Daily Mult if no duty curve specified
end;

procedure TIndMach012Obj.CalcYearlyMult(Hr: Double);
begin
{Yearly curve is assumed to be hourly only}
    if YearlyShapeObj <> NIL then
    begin
        ShapeFactor := YearlyShapeObj.GetMultAtHour(Hr);
        ShapeIsActual := YearlyShapeObj.UseActual;
    end
    else
        ShapeFactor := CDOUBLEONE;  // Defaults to no variation

end;

procedure TIndMach012Obj.InitHarmonics;
{Procedure to initialize for Harmonics solution}
begin
    YPrimInvalid := TRUE;  // Force rebuild of YPrims
end;

procedure TIndMach012Obj.IntegrateStates;
{
  This is a virtual function. You do not need to write this routine
  if you are not integrating state variables in dynamics mode.
}

// Integrate state variables for Dynamics analysis
// Example from Generator

// Illustrates use of debug tracing

// Present technique is a predictor-corrector trapezoidal rule

var
    TracePower: Complex;


begin
   // Compute Derivatives and then integrate

    ComputeIterminal;

    with ActiveCircuit.Solution, MachineData do
    begin
        with DynaVars do
            if (IterationFlag = 0) then
            begin {First iteration of new time step}
                ThetaHistory := Theta + 0.5 * h * dTheta;
                SpeedHistory := Speed + 0.5 * h * dSpeed;
            end;

      // Compute shaft dynamics
        TracePower := TerminalPowerIn(Vterminal, Iterminal, FnPhases); // in watts
        dSpeed := (TracePower.re - Pshaft - abs(D * Speed)) / Mmass;
        dTheta := Speed;

     // Trapezoidal method
        with DynaVars do
        begin
            Speed := SpeedHistory + 0.5 * h * dSpeed;
            Theta := ThetaHistory + 0.5 * h * dTheta;
        end;

        if DebugTrace then
            WriteTraceRecord;

        Integrate;

    end;
end;

procedure TIndMach012Obj.Get_DynamicModelCurrent;
begin
    Is1 := (V1 - E1) / Zsp; // I = (V-E')/Z'
    Is2 := (V2 - E2) / Zsp; // I = (V-E')/Z'

    // rotor current  Ir1= Is1-Vm/jXm
    Ir1 := Is1 - (V1 - Is1 * Zsp) / Zm;
    Ir2 := Is2 - (V2 - Is2 * Zsp) / Zm;
end;

procedure TIndMach012Obj.Get_PFlowModelCurrent(const V: Complex; const S: Double; var Istator, Irotor: Complex);
var
    RL: Double;
    ZRotor, Numerator, Zmotor: Complex;
begin
    if s <> 0.0 then
        RL := Zr.re * (1.0 - s) / s
    else
        RL := Zr.re * 1.0e6;

    ZRotor := RL + Zr;
    Numerator := Zm * Zrotor;
    Zmotor := Zs + Numerator / (ZRotor + Zm);
    Istator := V / Zmotor;
    Irotor := Istator - (V - (Zs * Istator)) / Zm;
end;

function TIndMach012Obj.NumVariables: Integer;
{
  Return the number of state variables

  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
  Note: it is not necessary to define any state variables
}
begin
    Result := NumIndMach012Variables;
end;


function TIndMach012Obj.VariableName(i: Integer): String;
{
  Returns the i-th state variable in a string

  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
}
begin
    if i < 1 then
        Exit;  // This means Someone goofed
    case i of
        1:
            Result := 'Frequency';
        2:
            Result := 'Theta (deg)';
        3:
            Result := 'E1';
        4:
            Result := 'Pshaft';
        5:
            Result := 'dSpeed (deg/sec)';
        6:
            Result := 'dTheta (deg)';
        7:
            Result := 'Slip';
        8:
            Result := 'puRs';
        9:
            Result := 'puXs';
        10:
            Result := 'puRr';
        11:
            Result := 'puXr';
        12:
            Result := 'puXm';
        13:
            Result := 'Maxslip';
        14:
            Result := 'Is1';
        15:
            Result := 'Is2';
        16:
            Result := 'Ir1';
        17:
            Result := 'Ir2';
        18:
            Result := 'Stator Losses';
        19:
            Result := 'Rotor Losses';
        20:
            Result := 'Shaft Power (hp)';
        21:
            Result := 'Power Factor';
        22:
            Result := 'Efficiency (%)';
    end;
end;

function TIndMach012Obj.Get_Variable(i: Integer): Double;
begin
    Result := -9999.99;   // Error Value

    with MachineData do
        case i of
            1:
                Result := (w0 + Speed) / TwoPi;  // Frequency, Hz
            2:
                Result := (Theta) * RadiansToDegrees;  // Report in Deg
            3:
                Result := Cabs(E1) / vbase;      // Report in pu
            4:
                Result := Pshaft;
            5:
                Result := dSpeed * RadiansToDegrees; // Report in Deg      57.29577951
            6:
                Result := dTheta;
            7:
                Result := S1;
            8:
                Result := puRs;
            9:
                Result := puXs;
            10:
                Result := puRr;
            11:
                Result := puXr;
            12:
                Result := puXm;
            13:
                Result := MaxSlip;
            14:
                Result := Cabs(Is1);
            15:
                Result := Cabs(Is2);
            16:
                Result := Cabs(Ir1);
            17:
                Result := Cabs(Ir2);
            18:
                Result := GetStatorLosses;
            19:
                Result := GetRotorLosses;
            20:
            begin  // Shaft Power  (hp)
                Result := 3.0 / 746.0 * (Sqr(Cabs(Ir1)) * (1.0 - S1) / S1 + Sqr(Cabs(Ir2)) * (1.0 - S2) / S2) * Zr.re;
            end;
            21:
                Result := PowerFactor(Power[1]);
            22:
                Result := (1.0 - (GetStatorLosses + GetRotorLosses) / power[1].re) * 100.0;    // Efficiency
        end;
end;

procedure TIndMach012Obj.Set_Variable(i: Integer; Value: Double); // TODO: remove -- this is completely redundant with the properties but doesn't call RecalcElementData
begin
    case i of
        7:
        begin
            set_LocalSlip(Value);
            PropertySideEffects(ord(TProp.slip));
        end;
        8:
            puRs := Value;
        9:
            puXs := Value;
        10:
            puRr := Value;
        11:
            puXr := Value;
        12:
            puXm := Value;
    end;
    // Do Nothing for other variables: they are read only
end;

procedure TIndMach012Obj.GetAllVariables(States: pDoubleArray);
{
  Return all state variables in double array (allocated by calling function)

  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
}
var
    i: Integer;
begin
    for i := 1 to NumIndMach012Variables do
        States^[i] := Variable[i];
end;

function TIndMach012Obj.GetRotorLosses: Double;
begin
    Result := 3.0 * (Sqr(Ir1.re) + Sqr(Ir1.im) + Sqr(Ir2.re) + Sqr(Ir2.im)) * Zr.re;
end;

function TIndMach012Obj.GetStatorLosses: Double;
begin
    Result := 3.0 * (Sqr(Is1.re) + Sqr(Is1.im) + Sqr(Is2.re) + Sqr(Is2.im)) * Zs.re;
end;

procedure TIndMach012Obj.MakePosSequence();
begin
end;

procedure TIndMach012Obj.Set_ConductorClosed(Index: Integer; Value: Boolean);
// Routine for handling Open/Close procedures
begin
    inherited;

    if Value then
        IndMach012SwitchOpen := FALSE
    else
        IndMach012SwitchOpen := TRUE;
end;

procedure TIndMach012Obj.set_Localslip(const Value: Double);
begin
    S1 := Value;
    if not InDynamics then
        if Abs(S1) > MaxSlip then
        begin
            // Put limits on the slip  unless dynamics
            if S1 < 0 then
                S1 := -MaxSlip   
            else
                S1 := MaxSlip;
        end;
    S2 := 2.0 - S1;
end;

procedure TIndMach012Obj.InitTraceFile;
begin
    FreeAndNil(TraceFile);
    TraceFile := TBufferedFileStream.Create(DSS.OutputDirectory + Format('%s_IndMach012_Trace.csv', [Name]), fmCreate);

    FSWrite(TraceFile, 'Time, Iteration, S1, |IS1|, |IS2|, |E1|, |dE1dt|, |E2|, |dE2dt|, |V1|, |V2|, Pshaft, Pin, Speed, dSpeed');
    FSWriteln(TraceFile);

    FSFlush(TraceFile);
end;

procedure TIndMach012Obj.WriteTraceRecord;
begin
    with ActiveCircuit.Solution do
        FSWrite(TraceFile, Format('%-.6g, %d, %-.6g, ', [Dynavars.dblHour * 3600.0, Iteration, S1]));

    FSWrite(TraceFile, Format('%-.6g, %-.6g, ', [Cabs(Is1), Cabs(Is2)]));
    FSWrite(TraceFile, Format('%-.6g, %-.6g, %-.6g, %-.6g, ', [Cabs(E1), Cabs(dE1dt), Cabs(E2), Cabs(dE2dt)]));
    FSWrite(TraceFile, Format('%-.6g, %-.6g, ', [Cabs(V1), Cabs(V2)]));
    FSWrite(TraceFile, Format('%-.6g, %-.6g, ', [MachineData.Pshaft, power[1].re]));
    FSWrite(TraceFile, Format('%-.6g, %-.6g, ', [MachineData.speed, MachineData.dSpeed]));

    FSWriteln(TraceFile);

    FSFlush(TraceFile);
end;

finalization    SlipOptionEnum.Free;
end.
