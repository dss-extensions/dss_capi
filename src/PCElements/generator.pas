unit generator;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

//    Change Log
//    8-28-13 Forced re-initializing solution if Model 3 generator added.
//    7-??-18 Corrected Generator Model 7 1-phase Model

//  The generator is essentially a negative load that can be dispatched.
//
//  If the dispatch value (DispValue) is 0, the generator always follows the
//  appropriate dispatch curve, which are simply load curves. If DispValue>0 then
//  the generator only comes on when the global circuit load multiplier exceeds
//  DispValue.  When the generator is on, it always follows the dispatch curve
//  appropriate for the type of solution being performed.
//
//  If you want to model a generator that is fully on whenever it is dispatched on,
//  simply designate "Status=Fixed".  The default is "Status=Variable" (i.e., it follows
//  a dispatch curve.  You could also define a dispatch curve that is always 1.0.
//
//  Generators have their own energy meters that record:
//  1. Total kwh
//  2. Total kvarh
//  3. Max kW
//  4. Max kVA
//  5. Hours in operation
//  6. Price * kwH
//
//  Generator meters reset with the circuit energy meters and take a sample with
//  the circuit energy meters as well. The Energy meters also used trapezoidal integration
//  so that they are compatible with Load-Duration simulations.
//
//  Generator models are:
//  1. Constant P, Q  (* dispatch curve, if appropriate).
//  2. Constant Z  (For simple solution)
//  3. Constant P, |V|  like a standard power flow
//  4. Constant P, Fixed Q  (vars)
//  5. Constant P, Fixed Q  (reactance)
//  6. User model
//  7. Approximate Inverter model
//
//  Most of the time you will use #1 for planning studies.


//  The Generator is assumed balanced over the no. of phases defined

// If you do not specify load shapes defaults are:
//    Yearly:  Defaults to No variation (i.e. multiplier = 1.0 always)
//    Daily:   Defaults to No variation
//    Dutycycle: Defaults to Daily shape

interface

uses
    Classes,
    GenUserModel,
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    UComplex, DSSUcomplex,
    LoadShape,
    // GrowthShape,
    Spectrum,
    ArrayDef,
    Dynamics;

const
    NumGenRegisters = 6;    // Number of energy meter registers
    NumGenVariables = 6;

type
{$SCOPEDENUMS ON}
    TGeneratorProp = (
        INVALID = 0,
        phases,
        bus1,
        kv,
        kW,
        pf,
        kvar, // 13
        model, // 6
        Vminpu, // 23
        Vmaxpu, // 24
        yearly, // 7
        daily, // 8
        duty, // 9
        dispmode, // 10
        dispvalue, // 11
        conn, // 12
        status, // 16
        cls, // 17
        Vpu, // 18
        maxkvar, // 19
        minkvar, // 20
        pvfactor, // 21
        forceon, // 25
        kVA, // 26
        MVA, // 27
        Xd, // 28
        Xdp, // 29
        Xdpp, // 30
        H, // 31
        D, // 32
        UserModel, // 33
        UserData, // 34
        ShaftModel, // 35
        ShaftData, // 36
        DutyStart, // 37
        debugtrace, // 22
        Balanced,
        XRdp,
        UseFuel,
        FuelkWh,
        pctFuel,
        pctReserve,
        Refuel
    );
{$SCOPEDENUMS OFF}
    // Generator public data/state variable structure
    TGeneratorVars = {$IFNDEF DSS_CAPI_NO_PACKED_RECORDS}packed{$ENDIF} record

        Theta,      {Direct-Axis voltage magnitude & angle}
        Pshaft,
        Speed,
        w0,         {present Shaft Power and relative Speed, rad/sec, difference from Synchronous speed, w0}
                    {actual speed = Speed + w0}
        Hmass,      {Per unit mass constant}
        Mmass,      {Mass constant actual values (Joule-sec/rad}
        D, Dpu,     {Actual and per unit damping factors}
        kVArating,
        kVGeneratorBase,
        Xd, Xdp, Xdpp,   {machine Reactances, ohms}
        puXd, puXdp, puXdpp,   {machine Reactances, per unit}
        dTheta,
        dSpeed,     {Derivatives of Theta and Speed}
        ThetaHistory,
        SpeedHistory,   {history variables for integration}
        Pnominalperphase,
        Qnominalperphase  {Target P and Q for power flow solution, watts, vars}: Double;    { All Doubles }

        {32-bit integers}
        NumPhases,       {Number of phases}
        NumConductors,   {Total Number of conductors (wye-connected will have 4)}
        Conn: Integer;   // 0 = wye; 1 = Delta

        { Revisons (additions) to structure ...
          Later additions are appended to end of the structure so that
          previously compiled DLLs do not break
          }

        VthevMag: Double;    {Thevinen equivalent voltage for dynamic model}
        VThevHarm: Double;    {Thevinen equivalent voltage mag reference for Harmonic model}
        ThetaHarm: Double;    {Thevinen equivalent voltage angle reference for Harmonic model}
        VTarget: Double;   // Target voltage for generator with voltage control
        Zthev: Complex;
        XRdp: Double;  // Assumed X/R for Xd'
    end;

    TGenerator = class(TPCClass)
    PROTECTED
        cBuffer: TCBuffer24;  // Temp buffer for calcs  24-phase generator?

        procedure DefineProperties; override;
    PUBLIC
        RegisterNames: array[1..NumGenregisters] of String;

        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;

        procedure ResetRegistersAll;
        procedure SampleAll;
    end;

    TGeneratorObj = class(TPCElement)
    PRIVATE
        // Moved to GeneratorVars        Zthev           :Complex;
        Yeq: Complex;   // at nominal
        Yeq95: Complex;   // at 95%
        Yeq105: Complex;   // at 105%

        Edp: Complex;
        PhaseCurrentLimit: Complex;
        Model7MaxPhaseCurr: Double;
        Model7LastAngle: Double;
        DebugTrace: LongBool;
        DeltaQMax: Double;  // Max allowable var change on Model=3 per iteration
        DispatchMode: Integer;
        DispatchValue: Double;
        DQDV: Double;
        DQDVSaved: Double;
        FirstSampleAfterReset: Boolean;
        IsFixed: LongBool;   // if Fixed, always at base value
        GeneratorSolutionCount: Integer;
        GenFundamental: Double;  {Thevinen equivalent voltage mag and angle reference for Harmonic model}
        GenON: Boolean;           {Indicates whether generator is currently on}
        GenSwitchOpen: Boolean;
        kVANotSet: Boolean;
        LastGrowthFactor: Double;
        LastYear: Integer;   // added for speedup so we don't have to search for growth factor a lot
        OpenGeneratorSolutionCount: Integer;
        PVFactor: Double;  // deceleration Factor for computing vars for PV generators
        RandomMult: Double;
        Reg_Hours: Integer;
        Reg_kvarh: Integer;
        Reg_kWh: Integer;
        Reg_MaxkVA: Integer;
        Reg_MaxkW: Integer;
        Reg_Price: Integer;
        ShapeFactor: Complex;
// moved to GeneratorVars        Thetaharm       :Double;  {Thevinen equivalent voltage angle reference for Harmonic model}
        TraceFile: TFileStream;
        UserModel, ShaftModel: TGenUserModel;   {User-Written Models}
        UserModelNameStr, UserModelEditStr, ShaftModelNameStr, ShaftModelEditStr: String;
        V_Avg: Double;
        V_Remembered: Double;
        var_Remembered: Double;
        varBase: Double; // Base vars per phase
        varMax: Double;
        varMin: Double;
        VBase: Double;  // Base volts suitable for computing currents
        VBase105: Double;
        VBase95: Double;
        Vthev: Complex;  {Thevinen equivalent voltage (complex) for dynamic model}
// moved to GeneratorVars        Vthevharm       :Double;  {Thevinen equivalent voltage mag reference for Harmonic model}
// moved to GeneratorVars        VthevMag        :Double;    {Thevinen equivalent voltage for dynamic model}
        YPrimOpenCond: TCmatrix;  // To handle cases where one conductor of load is open ; We revert to admittance for inj currents
        YQFixed: Double;  // Fixed value of y for type 7 load
        ShapeIsActual: Boolean;
        ForceBalanced: LongBool;

        procedure CalcDailyMult(Hr: Double);
        procedure CalcDutyMult(Hr: Double);  // now incorporates DutyStart offset
        procedure CalcGenModelContribution;
        procedure CalcInjCurrentArray;
        procedure CalcVterminal;
        procedure CalcVTerminalPhase;
        procedure CalcVthev_Dyn;      // 3-phase Voltage behind transient reactance
        procedure CalcVthev_Dyn_Mod7(const V: Complex);
        procedure CalcYearlyMult(Hr: Double);
        procedure CalcYPrimMatrix(Ymatrix: TcMatrix);

        procedure DoConstantPQGen;
        procedure DoConstantZGen;
        procedure DoCurrentLimitedPQ;
        procedure DoDynamicMode;
        procedure DoFixedQGen;
        procedure DoFixedQZGen;
        procedure DoHarmonicMode;
        procedure DoPVTypeGen;
        procedure DoUserModel;
        function CheckOnFuel(const Deriv: Double; Const Interval: Double): Boolean;

        procedure Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);
        procedure SetDragHandRegister(Reg: Integer; const Value: Double);
        procedure StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);

        procedure WriteTraceRecord(const s: String);

        procedure SyncUpPowerQuantities;


        function Get_PresentkW: Double;
        function Get_Presentkvar: Double;
        function Get_PresentkV: Double;
        procedure Set_PresentkV(const Value: Double);
        procedure Set_PresentkW(const Value: Double);
        procedure Set_PowerFactor(const Value: Double);

        procedure SetkWkvar(const PkW, Qkvar: Double);

    PROTECTED
        procedure GetTerminalCurrents(Curr: pComplexArray); OVERRIDE;

    PUBLIC
        Connection: Integer;  {0 = line-neutral; 1=Delta}
        DailyDispShapeObj: TLoadShapeObj;  // Daily Generator Shape for this load
        DutyShapeObj: TLoadShapeObj;  // Shape for this generator
        YearlyShapeObj: TLoadShapeObj;  // Shape for this Generator
        DutyStart: Double; // starting time offset into the DutyShape [hrs] for this generator
        GenClass: Integer;
        GenModel: Integer;   // Variation with voltage
        GenVars: TGeneratorVars; {State Variables}
        kvarBase: Double;
        kvarMax: Double;
        kvarMin: Double;
        kWBase: Double;
        PFNominal: Double;
        Vpu: Double;   // per unit Target voltage for generator with voltage control
        Vmaxpu: Double;
        Vminpu: Double;
        
        ForcedON: LongBool;

        // Fuel related variables
        GenActive: Boolean;
        UseFuel: LongBool;
        FuelkWh,
        pctFuel,
        pctReserve: Double;

        Registers, Derivatives: array[1..NumGenregisters] of Double;

        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        procedure Set_ConductorClosed(Index: Integer; Value: Boolean); OVERRIDE;
        function InjCurrents: Integer; OVERRIDE;
        function NumVariables: Integer; OVERRIDE;
        procedure GetAllVariables(States: pDoubleArray); OVERRIDE;
        function Get_Variable(i: Integer): Double; OVERRIDE;
        procedure Set_Variable(i: Integer; Value: Double); OVERRIDE;
        function VariableName(i: Integer): String; OVERRIDE;

        procedure SetNominalGeneration;
        procedure Randomize(Opt: Integer);   // 0 = reset to 1.0; 1 = Gaussian around mean and std Dev  ;  // 2 = uniform

        procedure ResetRegisters;
        procedure TakeSample;

        // Procedures for setting the DQDV used by the Solution Object
        procedure InitDQDVCalc;
        procedure BumpUpQ;
        procedure RememberQV;
        procedure CalcDQDV;
        procedure ResetStartPoint;

        // Support for Dynamics Mode
        procedure InitStateVars; OVERRIDE;
        procedure IntegrateStates; OVERRIDE;

        // Support for Harmonics Mode
        procedure InitHarmonics; OVERRIDE;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model

        property PresentkW: Double READ Get_PresentkW WRITE Set_PresentkW;
        property PowerFactor: Double READ PFNominal WRITE Set_PowerFactor;

        //TODO: remove?
        property PresentkV: Double READ Get_PresentkV WRITE Set_PresentkV;
        property Presentkvar: Double READ Get_Presentkvar;

    end;

implementation

uses
    BufStream,
    Circuit,
    Sysutils,
    // Command,
    Math,
    MathUtil,
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TGeneratorObj;
    TProp = TGeneratorProp;
const
    NumPropsThisClass = Ord(High(TProp));
    // Dispatch modes
    // DEFAULT = 0;
    LOADMODE = 1;
    PRICEMODE = 2;
var
    PropInfo: Pointer = NIL;
    GenStatusEnum, GenDispModeEnum: TDSSEnum;

constructor TGenerator.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        GenDispModeEnum := TDSSEnum.Create('Generator: Dispatch Mode', True, 1, 1, 
            ['Default', 'LoadLevel', 'Price'], [0, 1, 2]
        );
        GenDispModeEnum.DefaultValue := 0;
        GenStatusEnum := TDSSEnum.Create('Generator: Status', True, 1, 1, 
            ['Variable', 'Fixed'], [0, Integer(True)]);
        GenStatusEnum.DefaultValue := 0;
    end;

    inherited Create(dssContext, GEN_ELEMENT, 'Generator');
    
    // Set Register names
    RegisterNames[1] := 'kWh';
    RegisterNames[2] := 'kvarh';
    RegisterNames[3] := 'Max kW';
    RegisterNames[4] := 'Max kVA';
    RegisterNames[5] := 'Hours';
    RegisterNames[6] := '$';
end;

destructor TGenerator.Destroy;
begin
    inherited Destroy;
end;

procedure DoRefuel(Obj: TObj);
begin
    Obj.pctFuel := 100.0;
    Obj.GenActive := True;
end;

procedure TGenerator.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // enum properties
    PropertyType[ord(TProp.conn)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.conn)] := ptruint(@obj.Connection);
    PropertyOffset2[ord(TProp.conn)] := PtrInt(DSS.ConnectionEnum);

    PropertyType[ord(TProp.dispmode)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.dispmode)] := ptruint(@obj.DispatchMode);
    PropertyOffset2[ord(TProp.dispmode)] := PtrInt(GenDispModeEnum);

    PropertyType[ord(TProp.status)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.status)] := ptruint(@obj.IsFixed); // LongBool as Integer
    PropertyOffset2[ord(TProp.status)] := PtrInt(GenStatusEnum);

    // string properties
    PropertyType[ord(TProp.UserModel)] := TPropertyType.StringProperty;
    PropertyType[ord(TProp.UserData)] := TPropertyType.StringProperty;
    PropertyType[ord(TProp.ShaftModel)] := TPropertyType.StringProperty;
    PropertyType[ord(TProp.ShaftData)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.UserModel)] := ptruint(@obj.UserModelNameStr);
    PropertyOffset[ord(TProp.UserData)] := ptruint(@obj.UserModelEditStr);
    PropertyOffset[ord(TProp.ShaftModel)] := ptruint(@obj.ShaftModelNameStr);
    PropertyOffset[ord(TProp.ShaftData)] := ptruint(@obj.ShaftModelEditStr);

    // bus properties
    PropertyType[ord(TProp.bus1)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus1)] := 1;

    // boolean properties
    PropertyType[ord(TProp.debugtrace)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.forceon)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.Balanced)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.UseFuel)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.debugtrace)] := ptruint(@obj.DebugTrace);
    PropertyOffset[ord(TProp.forceon)] := ptruint(@obj.ForcedON);
    PropertyOffset[ord(TProp.Balanced)] := ptruint(@obj.ForceBalanced);
    PropertyOffset[ord(TProp.UseFuel)] := ptruint(@obj.UseFuel);

    // integer properties
    PropertyType[ord(TProp.cls)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.model)] := TPropertyType.IntegerProperty; //TODO: enum
    PropertyOffset[ord(TProp.cls)] := ptruint(@obj.GenClass);
    PropertyOffset[ord(TProp.model)] := ptruint(@obj.GenModel);

    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

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
    PropertyOffset[ord(TProp.pf)] := ptruint(@obj.PFNominal);
    PropertyOffset[ord(TProp.Vpu)] := ptruint(@obj.Vpu);
    PropertyOffset[ord(TProp.maxkvar)] := ptruint(@obj.kvarMax);
    PropertyOffset[ord(TProp.minkvar)] := ptruint(@obj.kvarMin);
    PropertyOffset[ord(TProp.pvfactor)] := ptruint(@obj.PVFactor);
    PropertyOffset[ord(TProp.dispvalue)] := ptruint(@obj.DispatchValue);
    PropertyOffset[ord(TProp.Vminpu)] := ptruint(@obj.VMinPu);
    PropertyOffset[ord(TProp.Vmaxpu)] := ptruint(@obj.VMaxPu);
    PropertyOffset[ord(TProp.DutyStart)] := ptruint(@obj.DutyStart);
    PropertyOffset[ord(TProp.FuelkWh)] := ptruint(@obj.FuelkWh);
    PropertyOffset[ord(TProp.pctFuel)] := ptruint(@obj.pctFuel);
    PropertyOffset[ord(TProp.pctReserve)] := ptruint(@obj.pctReserve);

    PropertyOffset[ord(TProp.kVA)] := ptruint(@obj.GenVars.kVArating);
    PropertyOffset[ord(TProp.Xd)] := ptruint(@obj.GenVars.puXd);
    PropertyOffset[ord(TProp.Xdp)] := ptruint(@obj.GenVars.puXdp);
    PropertyOffset[ord(TProp.Xdpp)] := ptruint(@obj.GenVars.puXdpp);
    PropertyOffset[ord(TProp.H)] := ptruint(@obj.GenVars.Hmass);
    PropertyOffset[ord(TProp.D)] := ptruint(@obj.GenVars.Dpu);
    PropertyOffset[ord(TProp.XRdp)] := ptruint(@obj.Genvars.XRdp);// X/R for dynamics model

    PropertyOffset[ord(TProp.kv)] := ptruint(@obj.Genvars.kVGeneratorBase);
    PropertyOffset[ord(TProp.kvar)] := ptruint(@obj.kvarBase);

    // adv doubles
    PropertyOffset[ord(TProp.MVA)] := ptruint(@obj.GenVars.kVArating);
    PropertyScale[ord(TProp.MVA)] := 1000.0;
    PropertyFlags[ord(TProp.MVA)] := [TPropertyFlag.Redundant];
    PropertyRedundantWith[ord(TProp.MVA)] := ord(TProp.kVA);

    // boolean action
    PropertyType[ord(TProp.Refuel)] := TPropertyType.BooleanActionProperty;
    PropertyOffset[ord(TProp.Refuel)] := ptruint(@DoRefuel);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TGenerator.NewObject(const ObjName: String; Activate: Boolean): Pointer;
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
                NConds := Fnphases + 1;
            1:
                case Fnphases of
                    1, 2:
                        NConds := Fnphases + 1; // L-L and Open-delta
                else
                    NConds := Fnphases;
                end;
        end;
end;

procedure TGeneratorObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    i: Integer;
    kVA_Gen: Double;
begin
    if (Idx > 0) and (Idx <= NumPropsThisClass) then
        case TProp(Idx) of
            TProp.Conn:
            begin
                SetNCondsForConnection(self);
                // VBase is always L-N voltage unless 1-phase device or more than 3 phases
                with GenVars do 
                    // CASE Connection OF
                    // 1: VBase := kVGeneratorBase * 1000.0 ;
                    // Else
                    case Fnphases of
                        2, 3:
                            VBase := kVGeneratorBase * InvSQRT3x1000;    // L-N Volts
                    else
                        VBase := kVGeneratorBase * 1000.0;   // Just use what is supplied
                    end;
                VBase95 := Vminpu * VBase;
                VBase105 := Vmaxpu * VBase;

                Yorder := Fnconds * Fnterms;
                YPrimInvalid := TRUE;
            end;
            TProp.kV:
                with Genvars do
                begin
                    case FNphases of
                        2, 3:
                            VBase := kVGeneratorBase * InvSQRT3x1000;
                    else
                        VBase := kVGeneratorBase * 1000.0;
                    end;
                end;

            TProp.kvar:
            begin
                Genvars.Qnominalperphase := 1000.0 * kvarBase / Fnphases; // init to something reasonable
                kVA_Gen := Sqrt(Sqr(kWBase) + Sqr(kvarBase));
                if kVA_Gen <> 0.0 then
                    PFNominal := kWBase / kVA_Gen
                else
                    PFNominal := 1.0;
                if (kWBase * kvarBase) < 0.0 then
                    PFNominal := -PFNominal;

                kvarMax := 2.0 * kvarBase;
                kvarMin := -kvarMax;
            end;

            TProp.phases:
                SetNCondsForConnection(self);  // Force Reallocation of terminal info

            // keep kvar nominal up to date with kW and PF
            TProp.kW, TProp.pf:
                SyncUpPowerQuantities;

            TProp.UserModel:
                UserModel.Name := UserModelNameStr;  // Connect to user written models
            TProp.UserData:
                if UserModel.Exists then
                    UserModel.Edit := UserModelEditStr;  // Send edit string to user model
            TProp.ShaftModel:
                ShaftModel.Name := ShaftModelNameStr;
            TProp.ShaftData:
                if ShaftModel.Exists then
                    ShaftModel.Edit := ShaftModelEditStr;

            // if a model 3 generator added, force calc of dQdV
            TProp.model:
                if GenModel = 3 then
                    ActiveCircuit.Solution.SolutionInitialized := FALSE;

            // Set shape objects;  returns nil if not valid
            // Sets the kW and kvar properties to match the peak kW demand from the Loadshape
            TProp.yearly:
                if Assigned(YearlyShapeObj) then
                    with YearlyShapeObj do
                        if UseActual then
                            SetkWkvar(MaxP, MaxQ);
            TProp.daily:
                if Assigned(DailyDispShapeObj) then
                    with DailyDispShapeObj do
                        if UseActual then
                            SetkWkvar(MaxP, MaxQ);
            TProp.duty:
                if Assigned(DutyShapeObj) then
                    with DutyShapeObj do
                        if UseActual then
                            SetkWkvar(MaxP, MaxQ);

            TProp.debugtrace:
                if DebugTrace then
                begin
                    FreeAndNil(TraceFile);
                    TraceFile := TBufferedFileStream.Create(DSS.OutputDirectory + 'GEN_' + Name + '.csv', fmCreate);
                    FSWrite(TraceFile, 't, Iteration, LoadMultiplier, Mode, LoadModel, GenModel, dQdV, Avg_Vpu, Vdiff, MQnominalperphase, MPnominalperphase, CurrentType');
                    for i := 1 to fnphases do
                        FSWrite(Tracefile, ', |Iinj' + IntToStr(i) + '|');
                    for i := 1 to fnphases do
                        FSWrite(Tracefile, ', |Iterm' + IntToStr(i) + '|');
                    for i := 1 to fnphases do
                        FSWrite(Tracefile, ', |Vterm' + IntToStr(i) + '|');
                    FSWrite(TraceFile, ',Vthev, Theta');
                    FSWriteln(TraceFile);
                    FSFlush(Tracefile);
                end
                else
                begin
                    FreeAndNil(TraceFile);
                end;
                
            TProp.kVA, TProp.MVA:
                kVANotSet := FALSE;
        end;

    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TGenerator.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
begin
    with TObj(ptr) do
    begin
        RecalcElementData;
        YPrimInvalid := TRUE;
        Exclude(Flags, Flg.EditionActive);
    end;
    Result := True;
end;

procedure TGeneratorObj.MakeLike(OtherPtr: Pointer);
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

    Vbase := Other.Vbase;
    Vminpu := Other.Vminpu;
    Vmaxpu := Other.Vmaxpu;
    Vbase95 := Other.Vbase95;
    Vbase105 := Other.Vbase105;
    kWBase := Other.kWBase;
    kvarBase := Other.kvarBase;

    GenVars := Other.GenVars; // record, copy everything at once
    PFNominal := Other.PFNominal;
    varMin := Other.varMin;
    varMax := Other.varMax;
    Connection := Other.Connection;
    YearlyShapeObj := Other.YearlyShapeObj;
    DailyDispShapeObj := Other.DailyDispShapeObj;
    DutyShapeObj := Other.DutyShapeObj;
    DutyStart := Other.DutyStart;
    DispatchMode := Other.DispatchMode;
    DispatchValue := Other.DispatchValue;
    GenClass := Other.GenClass;
    GenModel := Other.GenModel;
    IsFixed := Other.IsFixed;

    Vpu := Other.Vpu;
    kvarMax := Other.kvarMax;
    kvarMin := Other.kvarMin;
    ForcedON := Other.ForcedON;
    kVANotSet := Other.kVANotSet;
    UseFuel := Other.UseFuel;
    FuelkWh := Other.FuelkWh;
    pctFuel := Other.pctFuel;
    pctReserve := Other.pctReserve;

    UserModel.Name := Other.UserModel.Name;  // Connect to user written models
    ShaftModel.Name := Other.ShaftModel.Name;
    UserModelNameStr := Other.UserModelNameStr;
    ShaftModelNameStr := Other.ShaftModelNameStr;
end;

procedure TGenerator.ResetRegistersAll;  // Force all EnergyMeters in the circuit to reset
var
    pGen: TGeneratorObj;
begin
    pGen := ActiveCircuit.Generators.First;
    while (pGen <> NIL) do
    begin
        pGen.ResetRegisters;
        pGen := ActiveCircuit.Generators.Next;
    end;
end;

procedure TGenerator.SampleAll;  // Force all EnergyMeters in the circuit to take a sample
var
    pGen: TGeneratorObj;
begin
    pGen := ActiveCircuit.Generators.First;
    while pGen <> NIL do
    begin
        if pGen.enabled then
            pGen.TakeSample;
        pGen := ActiveCircuit.Generators.Next;
    end;
end;

constructor TGeneratorObj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := AnsiLowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType; // + GEN_ELEMENT;  // In both PCelement and Genelement list

    TraceFile := nil;

    FNphases := 3;
    Fnconds := 4;  // defaults to wye
    Yorder := 0;  // To trigger an initial allocation
    Nterms := 1;  // forces allocations
    kWBase := 1000.0;
    kvarBase := 60.0;

    kvarMax := kvarBase * 2.0;
    kvarMin := -kvarmax;
    PFNominal := 0.88;
    YearlyShapeObj := NIL;  // if YearlyShapeobj = nil then the load alway stays nominal * global multipliers
    DailyDispShapeObj := NIL;  // if DaillyShapeobj = nil then the load alway stays nominal * global multipliers
    DutyShapeObj := NIL;  // if DutyShapeobj = nil then the load alway stays nominal * global multipliers
    DutyStart := 0.0;
    Connection := 0;    // Wye (star)
    GenModel := 1;  {Typical fixed kW negative load}
    GenClass := 1;
    LastYear := 0;
    LastGrowthFactor := 1.0;

    DQDVSaved := 0.0;  // Initialize this here.  Allows generators to be turned off and on

    GeneratorSolutionCount := -1;  // For keep track of the present solution in Injcurrent calcs
    OpenGeneratorSolutionCount := -1;
    YPrimOpenCond := NIL;

    GenVars.kVGeneratorBase := 12.47;
    Vpu := 1.0;
    GenVars.VTarget := 1000.0 * Vpu * GenVars.kVGeneratorBase / SQRT3;  {Line-to-Neutral target}
    VBase := 7200.0;
    Vminpu := 0.90;
    Vmaxpu := 1.10;
    VBase95 := Vminpu * Vbase;
    VBase105 := Vmaxpu * Vbase;
    Yorder := Fnterms * Fnconds;
    RandomMult := 1.0;
    IsFixed := FALSE;

    // Machine rating stuff
    GenVars.kVArating := kWBase * 1.2;
    kVANotSet := TRUE;  // Flag for default value for kVA

    //GenVars.Vd         := 7200.0;
    with GenVars do
    begin
        puXd := 1.0;
        puXdp := 0.28;
        puXdpp := 0.20;
        Xd := puXd * SQR(kVGeneratorBase) * 1000.0 / kVARating;
        Xdp := puXdp * SQR(kVGeneratorBase) * 1000.0 / kVARating;
        Xdpp := puXdpp * SQR(kVGeneratorBase) * 1000.0 / kVARating;
        Hmass := 1.0;       //  W-sec/VA rating
        Theta := 0.0;
        w0 := TwoPi * Basefrequency;
        Speed := 0.0;
        dSpeed := 0.0;
        D := 1.0;
        XRdp := 20.0;
    end;

    // Advertise Genvars struct as public

    PublicDataStruct := pointer(@Genvars);
    PublicDataSize := SizeOf(TGeneratorVars);

    UserModel := TGenUserModel.Create(DSS, @Genvars);
    ShaftModel := TGenUserModel.Create(DSS, @Genvars);
    UserModelNameStr := '';
    UserModelEditStr := '';
    ShaftModelNameStr := '';
    ShaftModelEditStr := '';

    DispatchValue := 0.0;   // Follow curves

    Reg_kWh := 1;
    Reg_kvarh := 2;
    Reg_MaxkW := 3;
    Reg_MaxkVA := 4;
    Reg_Hours := 5;
    Reg_Price := 6;

    PVFactor := 0.1;
    DebugTrace := FALSE;
    ForcedON := FALSE;
    GenSwitchOpen := FALSE;
    ShapeIsActual := FALSE;
    ForceBalanced := FALSE;

    SpectrumObj := DSS.SpectrumClass.DefaultGen;  // override base class

    UseFuel := False;
    GenActive := True;
    FuelkWh := 0.0;
    pctFuel := 100.0;
    pctReserve := 20.0;

    RecalcElementData;
end;

destructor TGeneratorObj.Destroy;
begin
    FreeAndNil(TraceFile);
    YPrimOpenCond.Free;
    UserModel.Free;
    ShaftModel.Free;
    inherited Destroy;
end;

procedure TGeneratorObj.Randomize(Opt: Integer);
begin
    case Opt of
        0:
            RandomMult := 1.0;
        GAUSSIAN:
            RandomMult := Gauss(YearlyShapeObj.Mean, YearlyShapeObj.StdDev);
        UNIfORM:
            RandomMult := Random;  // number between 0 and 1.0
        LOGNORMAL:
            RandomMult := QuasiLognormal(YearlyShapeObj.Mean);
    end;
end;

procedure TGeneratorObj.CalcDailyMult(Hr: Double);
begin
    if (DailyDispShapeObj <> NIL) then
    begin
        ShapeFactor := DailyDispShapeObj.GetMultAtHour(Hr);
        ShapeIsActual := DailyDispShapeObj.UseActual;
    end
    else
        ShapeFactor := CDOUBLEONE;  // Default to no daily variation
end;

procedure TGeneratorObj.CalcDutyMult(Hr: Double);
begin
    if DutyShapeObj <> NIL then
    begin
        ShapeFactor := DutyShapeObj.GetMultAtHour(Hr + DutyStart);
        ShapeIsActual := DutyShapeObj.UseActual;
    end
    else
        CalcDailyMult(Hr);  // Default to Daily Mult if no duty curve specified
end;

procedure TGeneratorObj.CalcYearlyMult(Hr: Double);
begin
    // Yearly curve is assumed to be hourly only
    if YearlyShapeObj <> NIL then
    begin
        ShapeFactor := YearlyShapeObj.GetMultAtHour(Hr);
        ShapeIsActual := YearlyShapeObj.UseActual;
    end
    else
        ShapeFactor := CDOUBLEONE;  // Defaults to no variation
end;

procedure TGeneratorObj.SetNominalGeneration;
var
    Factor: Double;
    GenOn_Saved: Boolean;
begin
    GenOn_Saved := GenON;
    ShapeFactor := CDOUBLEONE;
    // Check to make sure the generation is ON
    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        if not (IsDynamicModel or IsHarmonicModel) then     // Leave generator in whatever state it was prior to entering Dynamic mode
        begin
            GenON := TRUE;   // Init to on then check if it should be off
            if not ForcedON then
                case DispatchMode of
                    LOADMODE:
                        if (DispatchValue > 0.0) and (GeneratorDispatchReference < DispatchValue) then
                            GenON := FALSE;
                    PRICEMODE:
                        if (DispatchValue > 0.0) and (PriceSignal < DispatchValue) then
                            GenON := FALSE;
                end;
        end;

        if not GenON then
        begin
            // If Generator is OFF enter as tiny resistive load (.0001 pu) so we don't get divide by zero in matrix
            Genvars.Pnominalperphase := -0.1 * kWBase / Fnphases;
            // Pnominalperphase   := 0.0;
            Genvars.Qnominalperphase := 0.0;
        end
        else
        begin    // Generator is on, compute it's nominal watts and vars
            with Solution do
                if IsFixed then
                begin
                    Factor := 1.0;   // for fixed generators, set constant
                end
                else
                begin
                    case Mode of
                        TSolveMode.SNAPSHOT:
                            Factor := ActiveCircuit.GenMultiplier * 1.0;
                        TSolveMode.DAILYMODE:
                        begin
                            Factor := ActiveCircuit.GenMultiplier;
                            CalcDailyMult(DynaVars.dblHour) // Daily dispatch curve
                        end;
                        TSolveMode.YEARLYMODE:
                        begin
                            Factor := ActiveCircuit.GenMultiplier;
                            CalcYearlyMult(DynaVars.dblHour);
                        end;
                        TSolveMode.DUTYCYCLE:
                        begin
                            Factor := ActiveCircuit.GenMultiplier;
                            CalcDutyMult(DynaVars.dblHour);
                        end;
                        TSolveMode.GENERALTIME,   // General sequential time simulation
                        TSolveMode.DYNAMICMODE:
                        begin
                            Factor := ActiveCircuit.GenMultiplier;
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
                            Factor := ActiveCircuit.GenMultiplier * 1.0;
                        TSolveMode.MONTECARLO2,
                        TSolveMode.MONTECARLO3,
                        TSolveMode.LOADDURATION1,
                        TSolveMode.LOADDURATION2:
                        begin
                            Factor := ActiveCircuit.GenMultiplier;
                            CalcDailyMult(DynaVars.dblHour);
                        end;
                        TSolveMode.PEAKDAY:
                        begin
                            Factor := ActiveCircuit.GenMultiplier;
                            CalcDailyMult(DynaVars.dblHour);
                        end;
                        TSolveMode.AUTOADDFLAG:
                            Factor := 1.0;
                    else
                        Factor := 1.0
                    end;
                end;

            if not (IsDynamicModel or IsHarmonicModel) then
            begin
                if ShapeIsActual then
                    Genvars.Pnominalperphase := 1000.0 * ShapeFactor.re / Fnphases
                else
                    Genvars.Pnominalperphase := 1000.0 * kWBase * Factor * ShapeFactor.re / Fnphases;

                with Genvars do
                    if GenModel = 3 then
                    begin   // Just make sure present value is reasonable
                        if Qnominalperphase > varMax then
                            Qnominalperphase := varMax
                        else
                        if Qnominalperphase < varMin then
                            Qnominalperphase := varMin;
                    end
                    else
                    begin
                        // for other generator models
                        if ShapeIsActual then
                            Qnominalperphase := 1000.0 * ShapeFactor.im / Fnphases
                        else
                            Qnominalperphase := 1000.0 * kvarBase * Factor * ShapeFactor.im / Fnphases;
                    end;
            end;
        end; // ELSE GenON

        if not (IsDynamicModel or IsHarmonicModel) then
        begin
            case GenModel of
                6:
                    Yeq := Cinv(cmplx(0.0, -Genvars.Xd));  // Gets negated in CalcYPrim
            else
                with Genvars do
                    Yeq := Cmplx(Pnominalperphase, -Qnominalperphase) / Sqr(Vbase);   // Vbase must be L-N for 3-phase
                if (Vminpu <> 0.0) then
                    Yeq95 := Yeq / sqr(Vminpu)  // at 95% voltage
                else
                    Yeq95 := Yeq; // Always a constant Z model

                if (Vmaxpu <> 0.0) then
                    Yeq105 := Yeq / Sqr(Vmaxpu)   // at 105% voltage
                else
                    Yeq105 := Yeq;
            end;

            // When we leave here, all the Yeq's are in L-N values
            if GenModel = 7 then
                with Genvars do
                begin
                    PhaseCurrentLimit := Cmplx(Pnominalperphase, -Qnominalperphase) / VBase95;
                    Model7MaxPhaseCurr := Cabs(PhaseCurrentLimit);
                end;
        end;
    end; // With ActiveCircuit

    // If generator state changes, force re-calc of Y matrix
    if GenON <> GenON_Saved then
        YPrimInvalid := TRUE;
end;

procedure TGeneratorObj.RecalcElementData;
begin
    VBase95 := VMinPu * VBase;
    VBase105 := VMaxPu * VBase;

    varBase := 1000.0 * kvarBase / Fnphases;
    varMin := 1000.0 * kvarMin / Fnphases;
    varMax := 1000.0 * kvarMax / Fnphases;

    // Populate data structures used for interchange with user-written models.
    with GenVars do
    begin
        Xd := puXd * 1000.0 * SQR(kVGeneratorBase) / kVARating;
        Xdp := puXdp * 1000.0 * SQR(kVGeneratorBase) / kVArating;
        Xdpp := puXdpp * 1000.0 * SQR(kVGeneratorBase) / kVArating;
        Conn := connection;
        NumPhases := Fnphases;
        NumConductors := Fnconds;
    end;

    SetNominalGeneration;

    YQFixed := -varBase / Sqr(VBase);   //10-17-02  Fixed negative sign
    GenVars.Vtarget := Vpu * 1000.0 * GenVars.kVGeneratorBase;

    if Fnphases > 1 then
        GenVars.VTarget := GenVars.VTarget / SQRT3;

    // Initialize to Zero - defaults to PQ generator
    // Solution object will reset after circuit modifications
    DQDV := DQDVSaved;         // for Model = 3
    DeltaQMax := (varMax - varMin) * 0.10;  // Limit to 10% of range

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

    // Update any user-written models
    if Usermodel.Exists then
        UserModel.FUpdateModel;
    if Shaftmodel.Exists then
        Shaftmodel.FUpdateModel;
end;

procedure TGeneratorObj.CalcYPrimMatrix(Ymatrix: TcMatrix);
var
    Y, Yij: Complex;
    i, j: Integer;
    FreqMultiplier: Double;
begin
    FYprimFreq := ActiveCircuit.Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;

    with  ActiveCircuit.solution do
        if IsDynamicModel or IsHarmonicModel then
        begin
            if GenON then
                Y := Yeq   // L-N value computed in initialization routines
            else
                Y := Cmplx(EPSILON, 0.0);

            if Connection = 1 then
                Y := Y / 3.0; // Convert to delta impedance
            Y.im := Y.im / FreqMultiplier;
            Yij := -Y;
            for i := 1 to Fnphases do
            begin
                case Connection of
                    0:
                    begin
                        Ymatrix.SetElement(i, i, Y);
                        Ymatrix.AddElement(Fnconds, Fnconds, Y);
                        Ymatrix.SetElemsym(i, Fnconds, Yij);
                    end;
                    1:
                    begin   {Delta connection}
                        Ymatrix.SetElement(i, i, Y);
                        Ymatrix.AddElement(i, i, Y);  // put it in again
                        for j := 1 to i - 1 do
                            Ymatrix.SetElemsym(i, j, Yij);
                    end;
                end;
            end;

//       Removed Neutral / Neutral may float
//
//       IF Connection = 0 Then   With Ymatrix Do  // Take care of neutral issues
//         Begin
//           AddElement(Fnconds, Fnconds, YNeut);  // Add in user specified Neutral Z, if any
//           // Bump up neutral-ground in case neutral ends up floating
//           SetElement(Fnconds, Fnconds, GetElement(Fnconds, Fnconds) * 1.000001);
//         End;
//
//      
        end
        else
        begin  //  Regular power flow generator model
            // Yeq is always expected as the equivalent line-neutral admittance
            Y := -Yeq;  // negate for generation    Yeq is L-N quantity
            // ****** Need to modify the base admittance for real harmonics calcs
            Y.im := Y.im / FreqMultiplier;

            case Connection of
                0:
                    with YMatrix do
                    begin // WYE
                        Yij := -Y;
                        for i := 1 to Fnphases do
                        begin
                            SetElement(i, i, Y);
                            AddElement(Fnconds, Fnconds, Y);
                            SetElemsym(i, Fnconds, Yij);
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
        end; // ELSE IF Solution.mode
end;

procedure TGeneratorObj.CalcYPrim;
var
    i: Integer;
begin
    // Build only shunt Yprim
    // Build a dummy Yprim Series so that CalcV does not fail
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

    if ActiveCircuit.Solution.LoadModel = POWERFLOW then
    begin
        // 12-7-99 we'll start with Yeq in system matrix
        SetNominalGeneration;
        CalcYPrimMatrix(YPrim_Shunt);
    end
    else
    begin
        // ADMITTANCE model wanted
        SetNominalGeneration;
        CalcYPrimMatrix(YPrim_Shunt);
    end;

    // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages doesn't fail
    for i := 1 to Yorder do
        Yprim_Series.SetElement(i, i, Yprim_Shunt.Getelement(i, i) * 1.0e-10);

    YPrim.CopyFrom(YPrim_Shunt);

    // Account for Open Conductors
    inherited CalcYPrim;
end;

procedure TGeneratorObj.StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);
// Add the current into the proper location according to connection

// Reverse of similar routine in load  (Cnegates are switched)
var
    j: Integer;

begin
    case Connection of

        0:
        begin  //Wye
            TermArray^[i] += Curr;
            TermArray^[Fnconds] -= Curr; // Neutral
        end;

        1:
        begin //DELTA
            TermArray^[i] += Curr;
            j := i + 1;
            if j > Fnconds then
                j := 1;
            TermArray^[j] -= Curr;
        end;
    end;
end;

procedure TGeneratorObj.WriteTraceRecord(const s: String);
var
    i: Integer;
    sout: String;
begin
    try
        if (not DSS.InShowResults) then
        begin
            WriteStr(sout, Format('%-.g, %d, %-.g, ',
                [ActiveCircuit.Solution.DynaVars.t + ActiveCircuit.Solution.Dynavars.IntHour * 3600.0,
                ActiveCircuit.Solution.Iteration,
                ActiveCircuit.LoadMultiplier]),
                DSS.SolveModeEnum.OrdinalToString(ord(DSS.ActiveCircuit.Solution.mode)), ', ',
                DSS.DefaultLoadModelEnum.OrdinalToString(DSS.ActiveCircuit.Solution.LoadModel), ', ',
                GenModel: 0, ', ',
                DQDV: 8: 0, ', ',
                (V_Avg * 0.001732 / GenVars.kVgeneratorbase): 8: 3, ', ',
                (GenVars.Vtarget - V_Avg): 9: 1, ', ',
                (Genvars.Qnominalperphase * 3.0 / 1.0e6): 8: 2, ', ',
                (Genvars.Pnominalperphase * 3.0 / 1.0e6): 8: 2, ', ',
                s, ', '
            );
            FSWrite(TraceFile, sout);
            for i := 1 to fnphases do
            begin
                WriteStr(sout, (Cabs(InjCurrent^[i])): 8: 1, ', ');
                FSWrite(TraceFile, sout);
            end;
            for i := 1 to fnphases do
            begin
                WriteStr(sout, (Cabs(ITerminal^[i])): 8: 1, ', ');
                FSWrite(TraceFile, sout);
            end;
            for i := 1 to fnphases do
            begin
                WriteStr(sout, (Cabs(Vterminal^[i])): 8: 1, ', ');
                FSWrite(TraceFile, sout);
            end;
            WriteStr(sout, GenVars.VThevMag: 8: 1, ', ', Genvars.Theta * 180.0 / PI);
            FSWrite(TraceFile, sout);
            FSWriteln(Tracefile);
            FSFlush(TraceFile);
        end;
    except
        On E: Exception do
        begin
        end;

    end;
end;

function TGeneratorObj.CheckOnFuel(const Deriv: Double; Const Interval: Double): Boolean;
Begin
    Result := True;
    pctFuel := ((((pctFuel / 100) * FuelkWh) - Interval * Deriv) / FuelkWh) * 100;
    
    if pctFuel <= pctReserve then
    begin
        Result  :=  False;
        pctFuel :=  pctReserve;
        // GenON   :=  False;
    end;
end;

procedure TGeneratorObj.DoConstantPQGen;
// Compute total terminal current for Constant PQ
var
    i: Integer;
    Curr, V: Complex;
    Vmag: Double;
//   V012,I012 :Array[0..2] of Complex;
//   Iabc :Array[1..3] of Complex;
begin
    //Treat this just like the Load model

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    ZeroITerminal;

    CalcVTerminalPhase; // get actual voltage across each phase of the load
    for i := 1 to Fnphases do
    begin
        V := Vterminal^[i];
        VMag := Cabs(V);

        case Connection of
            0:
            begin  {Wye}
                if VMag <= VBase95 then
                    Curr := Yeq95 * V  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Yeq105 * V  // above 105% use an impedance model
                else
                    with Genvars do
                        Curr := cong(Cmplx(Pnominalperphase, Qnominalperphase) / V);  // Between 95% -105%, constant PQ
            end;
            1:
            begin  {Delta}
                case Fnphases of
                    2, 3:
                        VMag := VMag / SQRT3;  // L-N magnitude
                else
                    // leave Vmag as is
                end;

                if VMag <= VBase95 then
                    Curr := (Yeq95 / 3.0) * V  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := (Yeq105 / 3.0) * V  // above 105% use an impedance model
                else
                    with Genvars do
                        Curr := cong(Cmplx(Pnominalperphase, Qnominalperphase) / V);  // Between 95% -105%, constant PQ
            end;
        end;

        // Checks the output in case of using Fuel
        if UseFuel and (not GenActive) then
            Curr := cmplx(0, 0);

        StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;

procedure TGeneratorObj.DoConstantZGen;
var
    i: Integer;
    Curr,
    Yeq2: Complex;
begin
    // Assume Yeq is kept up to date
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;
    if Connection = 0 then
        Yeq2 := Yeq
    else
        Yeq2 := Yeq / 3.0;

    for i := 1 to Fnphases do
    begin
        Curr := Yeq2 * Vterminal^[i];   // Yeq is always line to neutral

        // Checks the output in case of using Fuel
        if UseFuel and (not GenActive) then
            Curr := cmplx(0, 0);

        StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;

procedure TGeneratorObj.DoPVTypeGen;
// Compute total terminal current for Constant P,|V|

// Constant P, constant |V|
var
    i: Integer;
    DQ: Double;
    Curr: Complex;
begin
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the generator
    ZeroITerminal;

    // Guess at a new var output value
    V_Avg := 0.0;
    for i := 1 to Fnphases do
        V_Avg := V_Avg + Cabs(Vterminal^[i]);

    if Connection = 1 then
        V_Avg := V_Avg / (SQRT3 * Fnphases)
    else
        V_Avg := V_Avg / Fnphases;

    DQ := PVFactor * DQDV * (GenVars.Vtarget - V_Avg);   // Vtarget is L-N
    if (Abs(DQ) > DeltaQMax) then
        if (DQ < 0.0) then
            DQ := -DeltaQMax
        else
            DQ := DeltaQMax;
    with Genvars do
        Qnominalperphase := Qnominalperphase + DQ;

    // Test Limits
    with Genvars do
    begin
        if (Qnominalperphase > varMax) then
            Qnominalperphase := varMax
        else
        if (Qnominalperphase < varMin) then
            Qnominalperphase := varMin;

        // Compute injection currents using W and var values
        // Do not use comstant Z models outside normal range
        // Presumably the var source will take care of the voltage problems
        for i := 1 to Fnphases do
        begin
            Curr := cong(Cmplx(Pnominalperphase, Qnominalperphase) / Vterminal^[i]);
            
            // Checks the output in case of using Fuel
            if UseFuel and (not GenActive) then
                Curr := cmplx(0, 0);
            
            StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
            IterminalUpdated := TRUE;
            StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
        end;
    end; {With}
end;

procedure TGeneratorObj.DoFixedQGen;
// Compute total terminal current for Fixed Q
// Constant P, Fixed Q  Q is always kvarBase
var
    i: Integer;
    Curr,
    V: Complex;
    Vmag: Double;

begin
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        V := Vterminal^[i];
        VMag := Cabs(V);

        case Connection of
            0:
            begin
                if VMag <= VBase95 then
                    Curr := Cmplx(Yeq95.re, YQfixed) * V  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmplx(Yeq105.re, YQfixed) * V  // above 105% use an impedance model
                else
                    Curr := cong(Cmplx(Genvars.Pnominalperphase, varBase) / V);
            end;
            1:
            begin
                case Fnphases of
                    2, 3:
                        VMag := VMag / SQRT3;  // L-N magnitude
                else
                    {leave Vmag as is}
                end;
                if VMag <= VBase95 then
                    Curr := Cmplx(Yeq95.re / 3.0, YQfixed / 3.0) * V  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmplx(Yeq105.re / 3.0, YQfixed / 3.0) * V  // above 105% use an impedance model
                else
                    Curr := cong(Cmplx(Genvars.Pnominalperphase, varBase) / V);
            end;
        end;
        
        // Checks the output in case of using Fuel
        if UseFuel and (not GenActive) then
            Curr := cmplx(0, 0);
        
        StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;

procedure TGeneratorObj.DoFixedQZGen;
// Compute total terminal current for
// Constant P, Fixed Q  Q is always a fixed Z derived from kvarBase
var
    i: Integer;
    Curr,
    V: Complex;
    Vmag: Double;

begin
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        V := Vterminal^[i];
        Vmag := Cabs(V);

        case Connection of
            0:
            begin
                if Vmag <= VBase95 then
                    Curr := Cmplx(Yeq95.re, YQfixed) * V  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmplx(Yeq105.re, YQfixed) * V
                else
                begin
                    Curr := cong(Cmplx(Genvars.Pnominalperphase, 0.0) / V); // P component of current
                    Curr += Cmplx(0.0, YQFixed) * V;  // add in Q component of current
                end;
            end;
            1:
            begin
                case Fnphases of
                    2, 3:
                        VMag := VMag / SQRT3;  // L-N magnitude
                else
                    // leave Vmag as is
                end;
                if Vmag <= VBase95 then
                    Curr := Cmplx(Yeq95.re / 3.0, YQfixed / 3.0) * V  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmplx(Yeq105.re / 3.0, YQfixed / 3.0) * V
                else
                begin
                    Curr := cong(Cmplx(Genvars.Pnominalperphase, 0.0) / V); // P component of current
                    Curr += Cmplx(0.0, YQFixed / 3.0) * V;  // add in Q component of current
                end;
            end;
        end;

        // Checks the output in case of using Fuel
        if UseFuel and (not GenActive) then
            Curr := cmplx(0, 0);

        StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;

procedure TGeneratorObj.DoUserModel;
// Compute total terminal Current from User-written model
var
    i: Integer;
begin
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array

    if UserModel.Exists then    // Check automatically selects the usermodel if true
    begin
         //AppendToEventLog('Wnominal=', Format('%-.5g',[Pnominalperphase]));
        UserModel.FCalc(Vterminal, Iterminal);
        IterminalUpdated := TRUE;
        with ActiveCircuit.Solution do
        begin          // Negate currents from user model for power flow generator model
            for i := 1 to FnConds do
                InjCurrent^[i] -= Iterminal^[i];
        end;
    end
    else
    begin
        DoSimpleMsg('%s model designated to use user-written model, but user-written model is not defined.', [FullName], 567);
    end;
end;

procedure TGeneratorObj.DoCurrentLimitedPQ;
// Compute total terminal current for Constant PQ, but limit to max current below Vminpu
var
    i: Integer;
    PhaseCurr, DeltaCurr, VLN, VLL: Complex;
    VMagLN, VMagLL: Double;
    V012: array[0..2] of Complex;  // Sequence voltages

begin
    //Treat this just like the Load model

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load

    if ForceBalanced and (Fnphases = 3) then
    begin    // convert to pos-seq only
        Phase2SymComp(Vterminal, pComplexArray(@V012));
        V012[0] := CZERO; // Force zero-sequence voltage to zero
        V012[2] := CZERO; // Force negative-sequence voltage to zero
        SymComp2Phase(Vterminal, pComplexArray(@V012));  // Reconstitute Vterminal as balanced
    end;

    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        case Connection of
            0:
            begin
                VLN := Vterminal^[i];   // VTerminal is LN for this connection
                VMagLN := Cabs(VLN);
                with Genvars do
                    PhaseCurr := cong(Cmplx(Pnominalperphase, Qnominalperphase) / VLN);
                if Cabs(PhaseCurr) > Model7MaxPhaseCurr then
                    PhaseCurr := cong(PhaseCurrentLimit / (VLN / VMagLN));

                StickCurrInTerminalArray(ITerminal, -PhaseCurr, i);  // Put into Terminal array taking into account connection
                ITerminalUpdated := TRUE;
                StickCurrInTerminalArray(InjCurrent, PhaseCurr, i);  // Put into Terminal array taking into account connection
            end;
            1:
            begin
                VLL := Vterminal^[i];     // VTerminal is LL for this connection
                VMagLL := Cabs(VLL);
                case Fnphases of
                    2, 3:   // 2 or 3 phase generator model 7
                    begin
                        with Genvars do
                            DeltaCurr := cong(Cmplx(Pnominalperphase, Qnominalperphase) / VLL);
                        if Cabs(DeltaCurr) * SQRT3 > Model7MaxPhaseCurr then
                            DeltaCurr := cong(PhaseCurrentLimit / (VLL / (VMagLL / SQRT3)));
                    end
                else  // 1-phase generator model 7
                    with Genvars do
                        DeltaCurr := cong(Cmplx(Pnominalperphase, Qnominalperphase) / VLL);
                    if Cabs(DeltaCurr) > Model7MaxPhaseCurr then
                        DeltaCurr := cong(PhaseCurrentLimit / (VLL / VMagLL));
                end;

                // Checks the output in case of using Fuel
                if UseFuel and (not GenActive) then
                    DeltaCurr := cmplx(0, 0);

                StickCurrInTerminalArray(ITerminal, -DeltaCurr, i);  // Put into Terminal array taking into account connection
                ITerminalUpdated := TRUE;
                StickCurrInTerminalArray(InjCurrent, DeltaCurr, i);  // Put into Terminal array taking into account connection
            end;
        end;
    end;
end;

procedure TGeneratorObj.DoDynamicMode;
// Compute Total Current and add into InjTemp
var
    i: Integer;
    V012,
    I012: array[0..2] of Complex;
begin
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array  and computes VTerminal L-N

    // Inj = -Itotal (in) - Yprim*Vtemp

    case GenModel of

        6:
            if UserModel.Exists then       // auto selects model
            begin   {We have total currents in Iterminal}
                UserModel.FCalc(Vterminal, Iterminal);  // returns terminal currents in Iterminal
            end
            else
            begin
                DoSimpleMsg('Dynamics model missing for %s ', [FullName], 5671);
                DSS.SolutionAbort := TRUE;
            end;
    else

        case Fnphases of  {No user model, use default Thevinen equivalent for standard Generator model}
            1:
                with Genvars do
                begin
                   // 1-phase generators have 2 conductors
                    case Genmodel of
                        7:
                        begin  // simple inverter model
                            // Assume inverter stays in phase with terminal voltage
                            CalcVthev_Dyn_Mod7(VTerminal^[1] - VTerminal^[2]);
                        end;
                    else
                        CalcVthev_Dyn;  // Update for latest phase angle
                    end;


                    ITerminal^[1] := (VTerminal^[1] - Vthev - VTerminal^[2]) / Zthev;  // ZThev is based on Xd'
                    if Genmodel = 7 then
                    begin
                        if Cabs(Iterminal^[1]) > Model7MaxPhaseCurr then   // Limit the current but keep phase angle
                            ITerminal^[1] := ptocomplex(topolar(Model7MaxPhaseCurr, cang(Iterminal^[1])));
                    end;

                    ITerminal^[2] := -ITerminal^[1];
                end;

            3:
                with Genvars do
                begin
                    Phase2SymComp(Vterminal, pComplexArray(@V012));

                    case GenModel of
                        7:
                        begin  // simple inverter model
                            // Positive Sequence Contribution to Iterminal
                            // Assume inverter stays in phase with pos seq voltage
                            // and pos seq current is limited
                            CalcVthev_Dyn_Mod7(V012[1]);

                            // Positive Sequence Contribution to Iterminal
                            // Ref Frame here is all L-N

                            I012[1] := (V012[1] - Vthev) / Zthev; // ZThev is based on Xd'
                            if Cabs(I012[1]) > Model7MaxPhaseCurr  // Limit the current but keep phase angle
                            then
                                I012[1] := ptocomplex(topolar(Model7MaxPhaseCurr, cang(I012[1])));
                            if ForceBalanced  // set the negative sequence current
                            then
                                I012[2] := CZERO
                            else
                                I012[2] := V012[2] / Zthev;  // for inverter ZThev is  (Xd' + j0)

                        end
                    else
                        // Positive Sequence Contribution to Iterminal
                        CalcVthev_Dyn;  // Update for latest phase angle

                        // Positive Sequence Contribution to Iterminal
                        I012[1] := (V012[1] - Vthev) / Zthev;  // ZThev is based on Xd'
                        I012[2] := V012[2] / Cmplx(0.0, Xdpp);  // machine use Xd"
                    end;

                    // Adjust for generator connection
                    if (Connection = 1) or ForceBalanced then
                        I012[0] := CZERO
                    else
                        I012[0] := V012[0] / Cmplx(0.0, Xdpp);

                    SymComp2Phase(ITerminal, pComplexArray(@I012));  // Convert back to phase components

                    // Neutral current
                    if Connection = 0 then
                        ITerminal^[FnConds] := -I012[0] * 3;
                end;
        else
            DoSimpleMsg('Dynamics mode is implemented only for 1- or 3-phase Generators. %s has %d phases.', [FullName, Fnphases], 5671);
            DSS.SolutionAbort := TRUE;
        end;

    end;

    IterminalUpdated := TRUE;

    // Add it into inj current array
    for i := 1 to FnConds do
        InjCurrent^[i] -= Iterminal^[i];

    // Take Care of any shaft model calcs
    if (GenModel = 6) and ShaftModel.Exists then      // auto selects model
    begin           // Compute Mech Power to shaft
        ShaftModel.FCalc(Vterminal, Iterminal);     // Returns pshaft at least
    end;
end;

procedure TGeneratorObj.DoHarmonicMode;
// Compute Injection Current Only when in harmonics mode

// Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built
// Vd is the fundamental frequency voltage behind Xd" for phase 1
var
    i: Integer;
    E: Complex;
    GenHarmonic: Double;
    pBuffer: PCBuffer24;
begin
    pBuffer := @TGenerator(ParentClass).cBuffer;
    ComputeVterminal;

    with ActiveCircuit.Solution do
    begin
        GenHarmonic := Frequency / GenFundamental;
        E := SpectrumObj.GetMult(GenHarmonic) * GenVars.VThevHarm; // Get base harmonic magnitude
        RotatePhasorRad(E, GenHarmonic, GenVars.ThetaHarm);  // Time shift by fundamental frequency phase shift
        for i := 1 to Fnphases do
        begin
            pBuffer[i] := E;
            if i < Fnphases then
                RotatePhasorDeg(E, GenHarmonic, -120.0);  // Assume 3-phase generator
        end;
    end;

    // Handle Wye Connection
    if Connection = 0 then
        pBuffer[Fnconds] := Vterminal^[Fnconds];  // assume no neutral injection voltage

    // Inj currents = Yprim (E)
    YPrim.MVMult(InjCurrent, pComplexArray(pBuffer));
end;

procedure TGeneratorObj.CalcVTerminalPhase;
var
    i, j: Integer;
begin
    // Establish phase voltages and stick in Vterminal
    case Connection of

        0:
        begin
            with ActiveCircuit.Solution do
                for i := 1 to Fnphases do
                    Vterminal^[i] := VDiff(NodeRef^[i], NodeRef^[Fnconds]);
        end;

        1:
        begin
            with ActiveCircuit.Solution do
                for i := 1 to Fnphases do
                begin
                    j := i + 1;
                    if j > Fnconds then
                        j := 1;
                    Vterminal^[i] := VDiff(NodeRef^[i], NodeRef^[j]);
                end;
        end;
    end;
    GeneratorSolutionCount := ActiveCircuit.Solution.SolutionCount;
end;

procedure TGeneratorObj.CalcVTerminal;
// Put terminal voltages in an array
begin
    ComputeVTerminal;
    GeneratorSolutionCount := ActiveCircuit.Solution.SolutionCount;
end;

procedure TGeneratorObj.CalcGenModelContribution;
// Calculates generator current and adds it properly into the injcurrent array
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
        begin
           //  compute currents and put into InjTemp array;
            case GenModel of
                1:
                    DoConstantPQGen;
                2:
                    DoConstantZGen;
                3:
                    DoPVTypeGen;  // Constant P, |V|
                4:
                    DoFixedQGen;
                5:
                    DoFixedQZGen;
                6:
                    DoUserModel;
                7:
                    DoCurrentLimitedPQ;
            else
                DoConstantPQGen;  // for now, until we implement the other models.
            end;
        end;
    end;
   // When this is done, ITerminal is up to date
end;

procedure TGeneratorObj.CalcInjCurrentArray;
// Difference between currents in YPrim and total current
begin
    // Now Get Injection Currents
    if GenSwitchOpen then
        ZeroInjCurrent
    else
        CalcGenModelContribution;

    // We're not going to mess with this logic here -- too complicated: Use an open line in series
    // to look at open phase conditions. 
end;

procedure TGeneratorObj.GetTerminalCurrents(Curr: pComplexArray);
// Compute total Currents
begin
    with ActiveCircuit.Solution do
    begin
        if IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount then
        begin     // recalc the contribution
            if not GenSwitchOpen then
                CalcGenModelContribution;  // Adds totals in Iterminal as a side effect
        end;
        inherited GetTerminalCurrents(Curr);
    end;

    if (DebugTrace) then
        WriteTraceRecord('TotalCurrent');
end;

function TGeneratorObj.InjCurrents: Integer;
begin
    with ActiveCircuit.Solution do
    begin
        if LoadsNeedUpdating then
            SetNominalGeneration; // Set the nominal kW, etc for the type of solution being done

        CalcInjCurrentArray;          // Difference between currents in YPrim and total terminal current
        if (DebugTrace) then
            WriteTraceRecord('Injection');

        // Add into System Injection Current Array
        Result := inherited InjCurrents;
    end;
end;

procedure TGeneratorObj.ResetRegisters;
var
    i: Integer;
begin
    for i := 1 to NumGenregisters do
        Registers[i] := 0.0;
    for i := 1 to NumGenregisters do
        Derivatives[i] := 0.0;
    FirstSampleAfterReset := TRUE;  // initialize for trapezoidal integration
end;

procedure TGeneratorObj.Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);
begin
    if ActiveCircuit.TrapezoidalIntegration then
    begin
        // Trapezoidal Rule Integration
        if not FirstSampleAfterReset then
            Registers[Reg] := Registers[Reg] + 0.5 * Interval * (Deriv + Derivatives[Reg]);
    end
    else   
        // Plain Euler integration
        Registers[Reg] := Registers[Reg] + Interval * Deriv;

    Derivatives[Reg] := Deriv;
end;

procedure TGeneratorObj.TakeSample;
// Update Energy from metered zone
var
    S: Complex;
    Smag: Double;
    HourValue: Double;
begin
    // Compute energy in Generator branch
    if Enabled then
    begin
        if GenON then
        begin
            S := cmplx(Get_PresentkW, Get_Presentkvar);
            Smag := Cabs(S);
            HourValue := 1.0;
        end
        else
        begin
            S := CZERO;
            Smag := 0.0;
            HourValue := 0.0;
        end;

        if GenON or ActiveCircuit.TrapezoidalIntegration then
            // Make sure we always integrate for Trapezoidal case
            // Don't need to for Gen Off and normal integration
            with ActiveCircuit.Solution do
            begin
                if ActiveCircuit.PositiveSequence then
                begin
                    S := S * 3;
                    Smag := 3.0 * Smag;
                end;
                Integrate(Reg_kWh, S.re, IntervalHrs);   // Accumulate the power
                Integrate(Reg_kvarh, S.im, IntervalHrs);
                SetDragHandRegister(Reg_MaxkW, abs(S.re));
                SetDragHandRegister(Reg_MaxkVA, Smag);
                Integrate(Reg_Hours, HourValue, IntervalHrs);  // Accumulate Hours in operation
                Integrate(Reg_Price, S.re * ActiveCircuit.PriceSignal * 0.001, IntervalHrs);  // Accumulate Hours in operation
                FirstSampleAfterReset := FALSE;
                if UseFuel then 
                    GenActive := CheckonFuel(S.re, IntervalHrs);
            end;
    end;
end;

function TGeneratorObj.Get_PresentkW: Double;
begin
    Result := Genvars.Pnominalperphase * 0.001 * Fnphases;
end;

function TGeneratorObj.Get_PresentkV: Double;
begin
    Result := Genvars.kVGeneratorBase;
end;

function TGeneratorObj.Get_Presentkvar: Double;
begin
    Result := Genvars.Qnominalperphase * 0.001 * Fnphases;
end;

procedure TGeneratorObj.InitDQDVCalc;
begin
    DQDV := 0.0;
    Genvars.Qnominalperphase := 0.5 * (varmax + varmin);   // avg of the limits
end;

procedure TGeneratorObj.BumpUpQ;
// Bump up vars by 10% of range for next calc
begin
    with Genvars do
        Qnominalperphase := Qnominalperphase + 0.1 * (varmax - varmin);
end;

procedure TGeneratorObj.RememberQV;
var
    i: Integer;

begin
    var_Remembered := Genvars.Qnominalperphase;
    CalcVTerminal;
    V_Avg := 0.0;
    for i := 1 to Fnphases do
        V_Avg := V_Avg + Cabs(Vterminal^[i]);
    V_Avg := V_Avg / Fnphases;
    V_Remembered := V_Avg;
end;

procedure TGeneratorObj.CalcDQDV;
var
    Vdiff: Double;
    i: Integer;
begin
    CalcVTerminal;
    V_Avg := 0.0;
    for i := 1 to Fnphases do
        V_Avg := V_Avg + Cabs(Vterminal^[i]);
    V_Avg := V_Avg / Fnphases;

    Vdiff := V_Avg - V_Remembered;
    if (Vdiff <> 0.0) then
        DQDV := (Genvars.Qnominalperphase - var_Remembered) / Vdiff
    else
        DQDV := 0.0;  // Something strange has occured
                       // this will force a de facto P,Q model
    DQDVSaved := DQDV;  //Save for next time  Allows generator to be enabled/disabled during simulation
end;

procedure TGeneratorObj.ResetStartPoint;
begin
    Genvars.Qnominalperphase := 1000.0 * kvarBase / Fnphases;
end;

procedure TGeneratorObj.InitHarmonics;
var
    E, Va: complex;
begin
    YPrimInvalid := TRUE;  // Force rebuild of YPrims
    GenFundamental := ActiveCircuit.Solution.Frequency;  // Whatever the frequency is when we enter here.

    with GenVars do
    begin
        Yeq := Cinv(Cmplx(0.0, Xdpp));      // used for current calcs  Always L-N

        // Compute reference Thevinen voltage from phase 1 current
        if GenON then
        begin
            ComputeIterminal;  // Get present value of current

            with ActiveCircuit.solution do
                case Connection of
                    0:{wye - neutral is explicit}
                        Va := NodeV^[NodeRef^[1]] - NodeV^[NodeRef^[Fnconds]];
                    1:{delta -- assume neutral is at zero}
                        Va := NodeV^[NodeRef^[1]];
                end;

            E := Va - Iterminal^[1] * cmplx(0.0, Xdpp);
            Vthevharm := Cabs(E);   // establish base mag and angle
            ThetaHarm := Cang(E);
        end
        else
        begin
            Vthevharm := 0.0;
            ThetaHarm := 0.0;
        end;
    end;
end;

procedure TGeneratorObj.InitStateVars;
var
    // VNeut,
    i: Integer;
    V012,
    I012: array[0..2] of Complex;
    Vabc: array[1..3] of Complex;
begin
    YPrimInvalid := TRUE;  // Force rebuild of YPrims

    with GenVars do
    begin
        case Genmodel of
            7:
                Zthev := Cmplx(Xdp, 0.0); // use Xd' as an equivalent R for the inverter
        else
            Zthev := Cmplx(Xdp / XRdp, Xdp);
        end;

        Yeq := Cinv(Zthev);

        // Compute nominal Positive sequence voltage behind transient reactance
        if GenON then
            with ActiveCircuit.Solution do
            begin
                ComputeIterminal;

                case Fnphases of

                    1:
                    begin
                        Edp := NodeV^[NodeRef^[1]] - NodeV^[NodeRef^[2]] - ITerminal^[1] * Zthev;
                        VThevMag := Cabs(Edp);
                    end;

                    3:
                    begin
                 // Calculate Edp based on Pos Seq only
                        Phase2SymComp(ITerminal, pComplexArray(@I012));
                     // Voltage behind Xdp  (transient reactance), volts

                        for i := 1 to FNphases do
                            Vabc[i] := NodeV^[NodeRef^[i]];   // Wye Voltage
                        Phase2SymComp(pComplexArray(@Vabc), pComplexArray(@V012));
                        Edp := V012[1] - I012[1] * Zthev;    // Pos sequence
                        VThevMag := Cabs(Edp);
                    end;
                else
                    DoSimpleMsg('Dynamics mode is implemented only for 1- or 3-phase Generators. %s has %d phases.', [FullName, Fnphases], 5672);
                    DSS.SolutionAbort := TRUE;
                end;
                // Shaft variables
                // Theta is angle on Vthev[1] relative to system reference
                //Theta  := Cang(Vthev^[1]);   // Assume source at 0
                Theta := Cang(Edp);
                if GenModel = 7 then
                    Model7LastAngle := Theta;

                dTheta := 0.0;
                w0 := Twopi * ActiveCircuit.Solution.Frequency;
                // recalc Mmass and D in case the frequency has changed
                with GenVars do
                begin
                    GenVars.Mmass := 2.0 * GenVars.Hmass * GenVars.kVArating * 1000.0 / (w0);   // M = W-sec
                    D := Dpu * kVArating * 1000.0 / (w0);
                end;
                Pshaft := -Power[1].re; // Initialize Pshaft to present power Output

                Speed := 0.0;    // relative to synch speed
                dSpeed := 0.0;

                // Init User-written models
                //Ncond:Integer; V, I:pComplexArray; const X,Pshaft,Theta,Speed,dt,time:Double
                with ActiveCircuit.Solution do
                    if GenModel = 6 then
                    begin
                        if UserModel.Exists then
                            UserModel.FInit(Vterminal, Iterminal);
                        if ShaftModel.Exists then
                            ShaftModel.Finit(Vterminal, Iterminal);
                    end;

            end
        else
        begin
            Vthev := cZERO;
            Theta := 0.0;
            dTheta := 0.0;
            w0 := 0;
            Speed := 0.0;
            dSpeed := 0.0;
        end;
    end;
end;

procedure TGeneratorObj.IntegrateStates;
var
    TracePower: Complex;
begin
   // Compute Derivatives and then integrate

    ComputeIterminal;

    // Check for user-written exciter model.
    //Function(V, I:pComplexArray; const Pshaft,Theta,Speed,dt,time:Double)

    with ActiveCircuit.Solution, GenVars do
    begin
        with DynaVars do
            if (IterationFlag = 0) then
            begin {First iteration of new time step}
                ThetaHistory := Theta + 0.5 * h * dTheta;
                SpeedHistory := Speed + 0.5 * h * dSpeed;
            end;

        // Compute shaft dynamics
        TracePower := TerminalPowerIn(Vterminal, Iterminal, FnPhases);
        dSpeed := (Pshaft + TracePower.re - D * Speed) / Mmass;
//      dSpeed := (Torque + TerminalPowerIn(Vtemp,Itemp,FnPhases).re/Speed) / (Mmass);
        dTheta := Speed;

        // Trapezoidal method
        with DynaVars do
        begin
            Speed := SpeedHistory + 0.5 * h * dSpeed;
            Theta := ThetaHistory + 0.5 * h * dTheta;
        end;

        // Write Dynamics Trace Record
        if DebugTrace then
        begin
            FSWrite(TraceFile, Format('t=%-.5g ', [Dynavars.t]));
            FSWrite(TraceFile, Format(' Flag=%d ', [Dynavars.Iterationflag]));
            FSWrite(TraceFile, Format(' Speed=%-.5g ', [Speed]));
            FSWrite(TraceFile, Format(' dSpeed=%-.5g ', [dSpeed]));
            FSWrite(TraceFile, Format(' Pshaft=%-.5g ', [PShaft]));
            FSWrite(TraceFile, Format(' P=%-.5g Q= %-.5g', [TracePower.Re, TracePower.im]));
            FSWrite(TraceFile, Format(' M=%-.5g ', [Mmass]));
            FSWriteln(TraceFile);
            FSFlush(TraceFile);
        end;

        if GenModel = 6 then
        begin
            if UserModel.Exists then
                UserModel.Integrate;
            if ShaftModel.Exists then
                ShaftModel.Integrate;
        end;
    end;
end;

function TGeneratorObj.Get_Variable(i: Integer): Double;
// Return variables one at a time
var
    N, k: Integer;
begin
    N := 0;
    Result := -9999.99;  // error return value
    if i < 1 then
        Exit;  // Someone goofed

    with GenVars do
        case i of
            1:
                Result := (w0 + Speed) / TwoPi;  // Frequency, Hz
            2:
                Result := (Theta) * RadiansToDegrees;  // Report in Deg
            3:
                Result := Cabs(Vthev) / vbase;      // Report in pu
            4:
                Result := Pshaft;
            5:
                Result := dSpeed * RadiansToDegrees; // Report in Deg      57.29577951
            6:
                Result := dTheta;
        else
            begin
                if UserModel.Exists then
                begin
                    N := UserModel.FNumVars;
                    k := (i - NumGenVariables);
                    if k <= N then
                    begin
                        Result := UserModel.FGetVariable(k);
                        Exit;
                    end;
                end;

                // If we get here, must be in the Shaft Model if anywhere
                if ShaftModel.Exists then
                begin
                    k := i - (NumGenVariables + N);
                    if k > 0 then
                        Result := ShaftModel.FGetVariable(k);
                end;
            end;
        end;
end;

procedure TGeneratorObj.Set_Variable(i: Integer; Value: Double);
var
    N, k: Integer;
begin
    N := 0;
    if i < 1 then
        Exit;  // Someone goofed
    with GenVars do
        case i of
            1:
                Speed := (Value - w0) * TwoPi;
            2:
                Theta := Value / RadiansToDegrees; // deg to rad
            3: ;// meaningless to set Vd := Value * vbase; // pu to volts
            4:
                Pshaft := Value;
            5:
                dSpeed := Value / RadiansToDegrees;
            6:
                dTheta := Value;
        else
        begin
            if UserModel.Exists then
            begin
                N := UserModel.FNumVars;
                k := (i - NumGenVariables);
                if k <= N then
                begin
                    UserModel.FSetVariable(k, Value);
                    Exit;
                end;
            end;
         // If we get here, must be in the shaft model
            if ShaftModel.Exists then
            begin
                k := (i - (NumGenVariables + N));
                if k > 0 then
                    ShaftModel.FSetVariable(k, Value);
            end;
        end;
        end;
end;

procedure TGeneratorObj.GetAllVariables(States: pDoubleArray);

var
    i, N: Integer;
begin
    N := 0;
    for i := 1 to NumGenVariables do
        States^[i] := Variable[i];

    if UserModel.Exists then
    begin
        N := UserModel.FNumVars;
        UserModel.FGetAllVars(pDoubleArray(@States^[NumGenVariables + 1]));
    end;

    if ShaftModel.Exists then
    begin
        ShaftModel.FGetAllVars(pDoubleArray(@States^[NumGenVariables + 1 + N]));
    end;
end;

function TGeneratorObj.NumVariables: Integer;
begin
    Result := NumGenVariables;
    if UserModel.Exists then
        Result := Result + UserModel.FNumVars;
    if ShaftModel.Exists then
        Result := Result + ShaftModel.FNumVars;
end;

function TGeneratorObj.VariableName(i: Integer): String;
const
    BuffSize = 255;
var
    n,
    i2: Integer;
    Buff: array[0..BuffSize] of AnsiChar;
    pName: pAnsichar;

begin
    Result := 'ERROR';
    n := 0;
    if i < 1 then
        Exit;  // Someone goofed
    case i of
        1:
            Result := 'Frequency';
        2:
            Result := 'Theta (Deg)';
        3:
            Result := 'Vd';
        4:
            Result := 'PShaft';
        5:
            Result := 'dSpeed (Deg/sec)';
        6:
            Result := 'dTheta (Deg)';
    else
        begin
            if UserModel.Exists then  // Checks for existence and Selects
            begin
                pName := PAnsiChar(@Buff);
                n := UserModel.FNumVars;
                i2 := i - NumGenVariables;
                if i2 <= n then
                begin
                    // DLL functions require AnsiString type
                    UserModel.FGetVarName(i2, pName, BuffSize);
                    Result := String(pName);
                    Exit;
                end;
            end;

            if ShaftModel.Exists then
            begin
                pName := PAnsiChar(Buff);
                i2 := i - NumGenVariables - n;
                if i2 > 0 then
                    UserModel.FGetVarName(i2, pName, BuffSize);
                Result := String(pName);
            end;
        end;
    end;
end;

procedure TGeneratorObj.MakePosSequence();
var
    V: Double;
    had_kVA, had_MVA, had_kvars: Boolean;
    kW_new, PF_new, new_kVA, new_MVA, new_minkvar, new_maxkvar: Double;
    oldPhases, changes: Integer;
begin
    // Make sure voltage is line-neutral
    if (Fnphases > 1) or (connection <> 0) then
        V := GenVars.kVGeneratorBase / SQRT3
    else
        V := GenVars.kVGeneratorBase;

    // Divide the load by no. phases
    changes := 3;
    oldPhases := Fnphases;
    if Fnphases > 1 then
    begin
        had_kVA := PrpSequence[26] > 0;
        had_MVA := PrpSequence[27] > 0;
        had_kvars := (PrpSequence[19] <> 0) or (PrpSequence[20] <> 0);
        kW_new := kWbase / Fnphases;
        PF_new := PFNominal;
        if had_kvars then
        begin
            new_minkvar := kvarmin / Fnphases;
            new_maxkvar := kvarmax / Fnphases;
            Inc(changes);
        end;
        if had_kVA then
        begin
            new_kVA := genvars.kvarating / Fnphases;
            Inc(changes);
        end;
        if had_MVA then
        begin
            new_MVA := genvars.kvarating / 1000.0 / Fnphases;
            Inc(changes);
        end;
    end;

    BeginEdit(True);
    SetInteger(ord(TProp.Phases), 1);
    SetInteger(ord(TProp.conn), 0);
    SetDouble(ord(TProp.kV), V);
    if oldPhases > 1 then
    begin
        SetDouble(ord(TProp.kW), kW_new);
        SetDouble(ord(TProp.PF), PF_new);
        if had_kvars then
        begin
            SetDouble(ord(TProp.minkvar), new_minkvar);
            SetDouble(ord(TProp.maxkvar), new_maxkvar);
        end;
        if had_kVA then
            SetDouble(ord(TProp.kVA), new_kVA);
        if had_MVA then
            SetDouble(ord(TProp.MVA), new_MVA);
    end;
    EndEdit(changes);

    inherited;
end;

procedure TGeneratorObj.Set_ConductorClosed(Index: Integer; Value: Boolean);
begin
    inherited;

    // Just turn generator on or off;

    if Value then
        GenSwitchOpen := FALSE
    else
        GenSwitchOpen := TRUE;
end;

procedure TGeneratorObj.Set_PowerFactor(const Value: Double);
begin
    PFNominal := Value;
    SyncUpPowerQuantities;
end;

procedure TGeneratorObj.Set_PresentkV(const Value: Double);
begin
    Genvars.kVGeneratorBase := Value;
    PropertySideEffects(ord(TProp.kV));
end;

procedure TGeneratorObj.Set_PresentkW(const Value: Double);
begin
    kWBase := Value;
    SyncUpPowerQuantities;
end;

procedure TGeneratorObj.SyncUpPowerQuantities;
begin
    // keep kvar nominal up to date with kW and PF
    if (PFNominal <> 0.0) then
    begin
        kvarBase := kWBase * sqrt(1.0 / Sqr(PFNominal) - 1.0);
        Genvars.Pnominalperphase := 1000.0 * kWBase / Fnphases;
        Genvars.Qnominalperphase := 1000.0 * kvarBase / Fnphases;
        kvarMax := 2.0 * kvarBase;
        kvarMin := -kvarMax;
        if PFNominal < 0.0 then
            kvarBase := -kvarBase;

        if kVANotSet then
            GenVars.kVARating := kWBase * 1.2;
    end;
end;

procedure TGeneratorObj.SetDragHandRegister(Reg: Integer;
    const Value: Double);
begin
    if Value > Registers[reg] then
        Registers[Reg] := Value;
end;

procedure TGeneratorObj.SetkWkvar(const PkW, Qkvar: Double);
begin
    kWBase := PkW;
    kvarBase := Qkvar;
    PropertySideEffects(ord(TProp.kvar));
end;

procedure TGeneratorObj.CalcVthev_Dyn;
begin
    if GenSwitchOpen then
        GenVars.VThevMag := 0.0;
    Vthev := pclx(GenVars.VthevMag, Genvars.Theta);
end;

procedure TGeneratorObj.CalcVthev_Dyn_Mod7(const V: Complex);
// Adjust VThev to be in phase with V, if possible

// If the voltage magnitude drops below 15% or so, the accuracy of determining the
// phase angle gets flaky. This algorithm approximates the action of a PLL that will
// hold the last phase angle until the voltage recovers.
var
    Model7angle: Double;
begin
    if GenSwitchOpen then
        GenVars.VThevMag := 0.0;
    // For Phases=1, Vbase is voltage across the terminals.
    // Else it is LN voltage.
    if Cabs(V) > 0.2 * Vbase then
        Model7angle := Cang(V)
    else
        Model7Angle := Model7LastAngle;

    Vthev := pclx(GenVars.VthevMag, Model7angle);
    Model7Lastangle := Model7angle;
end;

finalization    GenStatusEnum.Free;
    GenDispModeEnum.Free;
end.
