unit PVsystem;

// ----------------------------------------------------------
// Copyright (c) 2018-2023, Paulo Meira, DSS Extensions contributors
// Copyright (c) 2011-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

uses
    Classes,
    PVsystemUserModel,
    DSSClass,
    InvBasedPCE,
    ucmatrix,
    UComplex, DSSUcomplex,
    LoadShape,
    TempShape,
    XYCurve,
    Spectrum,
    ArrayDef,
    Dynamics,
    InvDynamics,
    MathUtil;

const
    NumPVSystemRegisters = 6;    // Number of energy meter registers
    NumBasePVSystemVariables = 13;
    NumPVSystemVariables = NumBasePVSystemVariables + NumInvDynVars;    // No state variables that need integrating.
    VARMODEPF = 0;
    VARMODEKVAR = 1;

type
{$SCOPEDENUMS ON}
    TPVSystemProp = (
        INVALID = 0,
        phases = 1, 
        bus1 = 2,
        kv = 3, // propKV
        irradiance = 4, // propIrradiance
        Pmpp = 5, // propPmpp
        pctPmpp = 6, // proppctPmpp
        Temperature = 7, // propTemp
        pf = 8, // propPF
        conn = 9, // propCONNECTION
        kvar = 10, // propKVAR
        kVA = 11, // propKVA
        pctCutin = 12, // propCutin
        pctCutout = 13, // propCutout
        EffCurve = 14, // propInvEffCurve
        P__TCurve = 15, // propP_T_Curve
        pctR = 16, // propPCTR
        pctX = 17, // propPCTX
        model = 18, // propMODEL
        Vminpu = 19, // propVMINPU
        Vmaxpu = 20, // propVMAXPU
        Balanced = 21, // propBalanced
        LimitCurrent = 22, // propLimited
        yearly = 23, // propYEARLY
        daily = 24, // propDAILY
        duty = 25, // propDUTY
        Tyearly = 26, // propTYEARLY
        Tdaily = 27, // propTDAILY
        Tduty = 28, // propTDUTY
        cls = 29, // propCLASS
        UserModel = 30, // propUSERMODEL
        UserData = 31, // propUSERDATA
        debugtrace = 32, // propDEBUGTRACE
        VarFollowInverter = 33, // propVarFollowInverter
        DutyStart = 34, // propDutyStart 
        WattPriority = 35, // propPpriority
        PFPriority = 36, // propPFpriority
        pctPminNoVars = 37, // propPminNoVars
        pctPminkvarMax = 38, // propPminkvarLimit
        kvarMax = 39, // propkvarLimit
        kvarMaxAbs = 40, // propkvarLimitneg

        kVDC, // propkVDC,
        Kp, // propKp
        PITol, // propCtrlTol
        SafeVoltage, // propSMT
        SafeMode, // propSM
        DynamicEq, // propDynEq
        DynOut, // propDynOut
        ControlMode // propGFM
    );
{$SCOPEDENUMS OFF}

    // Struct to pass basic data to user-written DLLs
    TPVSystemVars = {$IFNDEF DSS_CAPI_NO_PACKED_RECORDS}packed{$ENDIF} record

        FkVArating: Double;
        kVPVSystemBase: Double;
        RThev: Double;
        XThev: Double;
        Vthevharm: Double;  // Thevinen equivalent voltage mag  for Harmonic model
        VthevmagDyn: Double;  // Thevinen equivalent voltage mag  reference for Dynamics model
        Thetaharm: Double;  // Thevinen equivalent  angle reference for Harmonic model
        ThetaDyn: Double;  // Thevinen equivalent  angle reference for Dynamics model
        InitialVAngle: Double;  // initial terminal voltage angle when entering dynamics mode
        EffFactor: Double;
        TempFactor: Double;
        PanelkW: Double; //computed
        FTemperature: Double;
        FPmpp: Double;
        FpuPmpp: Double;
        FIrradiance: Double;
        MaxDynPhaseCurrent: Double;
        Fkvarlimit: Double; //maximum kvar output of the PVSystem (unsigned)
        Fkvarlimitneg: Double;

    // Variables set from InvControl. They are results of monitor in mode 3
        Vreg: Double; // will be set from InvControl or ExpControl
        Vavg: Double;
        VVOperation: Double;
        VWOperation: Double;
        DRCOperation: Double;
        VVDRCOperation: Double;
        WPOperation: Double;
        WVOperation: Double;
    //        kW_out_desired   :Double;

        // 32-bit integers
        NumPhases: Integer;   // Number of phases
        NumConductors: Integer; // Total Number of conductors (wye-connected will have 4)
        Conn: Integer;   // 0 = wye; 1 = Delta
        P_Priority: LongBool;  // default False // added 10/30/2018
        PF_Priority: LongBool;  // default False // added 1/29/2019
    end;

    TPVSystem = class(TInvBasedPCEClass)
    PROTECTED
        cBuffer: TCBuffer24;  // Temp buffer for calcs  24-phase PVSystem element?

        procedure DefineProperties; override;
    PUBLIC
        RegisterNames: array[1..NumPVSystemRegisters] of String;

        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;

        procedure ResetRegistersAll;
        procedure SampleAll();
        procedure UpdateAll;
    end;

    TPVsystemObj = class(TInvBasedPCE)
    PRIVATE
        YEQ: Complex;   // at nominal
        YEQ_Min: Complex;   // at Vmin
        YEQ_Max: Complex;   // at VMax
        PhaseCurrentLimit: Complex;
        Zthev: Complex;

        LastThevAngle: Double;

        DebugTrace: Boolean;
        PVSystemSolutionCount: Integer;
        PVSystemFundamental: Double;  // Thevinen equivalent voltage mag and angle reference for Harmonic model
        PVsystemObjSwitchOpen: Boolean;
        FirstSampleAfterReset: Boolean;

      //PFSpecified             :Boolean;
      //kvarSpecified           :Boolean;

        ForceBalanced: Boolean;
        CurrentLimited: Boolean;

        kvar_out: Double;
        kW_out: Double;
        kvarRequested: Double;
        Fpf_wp_nominal: Double;
        kWRequested: Double;

        CutInkW: Double;
        CutOutkW: Double;
        FpctPminNoVars: Double;
        FpctPminkvarLimit: Double;
        PminNoVars: Double;
        PminkvarLimit: Double;

        pctR: Double;
        pctX: Double;

        OpenPVSystemSolutionCount: Integer;

        Pnominalperphase: Double;
        Qnominalperphase: Double;
        RandomMult: Double;

        Reg_Hours: Integer;
        Reg_kvarh: Integer;
        Reg_kWh: Integer;
        Reg_MaxkVA: Integer;
        Reg_MaxkW: Integer;
        Reg_Price: Integer;
        ShapeFactor: Complex;
        TShapeValue: Double;

        TraceFile: TFileStream;
        UserModel: TPVsystemUserModel;   // User-Written Models

        UserModelNameStr, UserModelEditStr: String;

        varBase: Double; // Base vars per phase
        VBaseMax: Double;
        VBaseMin: Double;
        YPrimOpenCond: TCmatrix;

        procedure CalcDailyMult(Hr: Double);  // now incorporates DutyStart offset
        procedure CalcDutyMult(Hr: Double);
        procedure CalcYearlyMult(Hr: Double);  // now incorporates DutyStart offset

        procedure CalcDailyTemperature(Hr: Double);
        procedure CalcDutyTemperature(Hr: Double);
        procedure CalcYearlyTemperature(Hr: Double);

        procedure ComputePanelPower;
        procedure ComputeInverterPower;
        procedure ComputekWkvar;

        procedure CalcPVSystemModelContribution();   // This is where the power gets computed
        procedure CalcInjCurrentArray();
        procedure CalcVTerminalPhase();

        procedure CalcYPrimMatrix(Ymatrix: TcMatrix);

        procedure DoConstantPQPVsystemObj();
        procedure DoConstantZPVsystemObj();
        procedure DoDynamicMode();
        procedure DoHarmonicMode();
        procedure DoUserModel();
        procedure DoGFM_Mode();

        procedure Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);
        procedure SetDragHandRegister(Reg: Integer; const Value: Double);
        procedure StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);

        procedure WriteTraceRecord(const s: String);

        procedure UpdatePVSystem;    // Update PVSystem elements based on present kW and IntervalHrs variable

        function Get_PresentkW: Double;
        function Get_Presentkvar: Double;
        function Get_PresentIrradiance: Double;

        procedure Set_PowerFactor(const Value: Double);
        procedure Set_pf_wp_nominal(const Value: Double);

        procedure Set_kVARating(const Value: Double);
        procedure Set_Pmpp(const Value: Double);

        procedure kWOut_Calc;

    PROTECTED
        procedure GetTerminalCurrents(Curr: pComplexArray); OVERRIDE;

    PUBLIC
        PVSystemVars: TPVSystemVars;

        VBase: Double;  // Base volts suitable for computing currents
        Vmaxpu: Double;
        Vminpu: Double;

        CurrentkvarLimit: Double;
        CurrentkvarLimitNeg: Double;
        FpctCutIn: Double;
        FpctCutOut: Double;

        Connection: Integer;  // 0 = line-neutral; 1=Delta
        DailyShapeObj: TLoadShapeObj;  // Daily PVSystem element irradianceShape for this load
        DutyShapeObj: TLoadShapeObj;  // irradiance Shape for this PVSystem element
        DutyStart: Double; // starting time offset into the DutyShape [hrs] for this PVsystem
        YearlyShapeObj: TLoadShapeObj;  // Yearly irradiance Shape for this PVSystem element

        DailyTShapeObj: TTShapeObj;
        DutyTShapeObj: TTShapeObj;
        YearlyTShapeObj: TTShapeObj;

        InverterCurveObj: TXYCurveObj;
        Power_TempCurveObj: TXYCurveObj;

        kvarLimitSet: Boolean;
        kvarLimitNegSet: Boolean;

        FClass: Integer;
        VoltageModel: Integer;   // Variation with voltage
        PFnominal: Double;

        Registers: array[1..NumPVSystemRegisters] of Double;
        Derivatives: array[1..NumPVSystemRegisters] of Double;
        PICtrl: array of TPICtrl;

        VarFollowInverter: LongBool;

        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure Set_ConductorClosed(Index: Integer; Value: Boolean); OVERRIDE;
        procedure RecalcElementData(); OVERRIDE;
        procedure CalcYPrim(); OVERRIDE;
        procedure GetCurrents(Curr: pComplexArray); OVERRIDE;

        function InjCurrents(): Integer; OVERRIDE;
        function NumVariables(): Integer; OVERRIDE;
        procedure GetAllVariables(var States: ArrayOfDouble); OVERRIDE;
        function Get_Variable(i: Integer): Double; OVERRIDE;
        procedure Set_Variable(i: Integer; Value: Double); OVERRIDE;
        function VariableName(i: Integer): String; OVERRIDE;

        procedure Set_Maxkvar(const Value: Double);
        procedure Set_Maxkvarneg(const Value: Double);

        procedure SetNominalPVSystemOuput();
        procedure Randomize(Opt: Integer);   // 0 = reset to 1.0; 1 = Gaussian around mean and std Dev  ;  // 2 = uniform


        procedure ResetRegisters;
        procedure TakeSample();

      // Support for Dynamics Mode
        procedure InitStateVars(); OVERRIDE;
        procedure IntegrateStates(); OVERRIDE;

        // Support for Harmonics Mode
        procedure InitHarmonics(); OVERRIDE;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model

        function IsPVSystem(): Boolean; OVERRIDE;
        function GetPFPriority(): Boolean; OVERRIDE;
        procedure SetPFPriority(value: Boolean); OVERRIDE;
        function CheckOLInverter(): Boolean; OVERRIDE;

        property PresentIrradiance: Double READ Get_PresentIrradiance WRITE PVSystemVars.FIrradiance;
        property PresentkW: Double READ Get_PresentkW WRITE kWRequested;
        property Presentkvar: Double READ Get_Presentkvar WRITE kvarRequested;
        property PresentkV: Double READ PVSystemVars.kVPVSystemBase;
        property PowerFactor: Double READ PFnominal WRITE Set_PowerFactor;
        property kVARating: Double READ PVSystemVars.FkVARating WRITE Set_kVARating;
        property Pmpp: Double READ PVSystemVars.FPmpp WRITE Set_pmpp;
        property puPmpp: Double READ PVSystemVars.FpuPmpp WRITE PVSystemVars.FpuPmpp;
        property kvarLimit: Double READ PVSystemVars.Fkvarlimit WRITE Set_Maxkvar;
        property kvarLimitneg: Double READ PVSystemVars.Fkvarlimitneg WRITE Set_Maxkvarneg;
        property MinModelVoltagePU: Double READ VminPu;
        property pf_wp_nominal: Double WRITE Set_pf_wp_nominal;
        property IrradianceNow: Double READ ShapeFactor.re;
    end;

implementation

uses
    BufStream,
    Circuit,
    Sysutils,
    Command,
    Math,
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    DSSHelper,
    DSSObjectHelper;

type
    TObj = TPVsystemObj;
    TProp = TPVSystemProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    

constructor TPVsystem.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, PVSYSTEM_ELEMENT, 'PVSystem');

    // Set Register names
    RegisterNames[1] := 'kWh';
    RegisterNames[2] := 'kvarh';
    RegisterNames[3] := 'Max kW';
    RegisterNames[4] := 'Max kVA';
    RegisterNames[5] := 'Hours';
    RegisterNames[6] := 'Price($)';
end;

destructor TPVsystem.Destroy;
begin
    inherited Destroy;
end;

function Getkvar(obj: TObj): Double;
begin
    Result := obj.kvar_out;
end;

procedure ObjSetDynOutput(obj: TObj; variable: String);
begin
    obj.SetDynOutput(variable);
end;

function ObjGetDynOutputStr(obj: TObj): String;
begin
    Result := obj.GetDynOutputStr();
end;

procedure TPVsystem.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // strings
    PropertyType[ord(TProp.UserModel)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.UserModel)] := ptruint(@obj.UserModelNameStr);
    PropertyType[ord(TProp.UserData)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.UserData)] := ptruint(@obj.UserModelEditStr);

    PropertyType[ord(TProp.DynOut)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.DynOut)] := 1; // dummy
    PropertyWriteFunction[ord(TProp.DynOut)] := @ObjSetDynOutput;
    PropertyReadFunction[ord(TProp.DynOut)] := @ObjGetDynOutputStr;
    PropertyFlags[ord(TProp.DynOut)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.ReadByFunction];

    // enum properties
    PropertyType[ord(TProp.conn)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.conn)] := ptruint(@obj.Connection);
    PropertyOffset2[ord(TProp.conn)] := PtrInt(DSS.ConnectionEnum);

    PropertyType[ord(TProp.ControlMode)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.ControlMode)] := ptruint(@obj.GFM_Mode);
    PropertyOffset2[ord(TProp.ControlMode)] := PtrInt(DSS.InvControlModeEnum);

    // bus properties
    PropertyType[ord(TProp.bus1)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus1)] := 1;

    // object properties
    PropertyType[ord(TProp.yearly)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.daily)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.duty)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.Tyearly)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.Tdaily)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.Tduty)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.EffCurve)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.P__TCurve)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.DynamicEq)] := TPropertyType.DSSObjectReferenceProperty;

    PropertyOffset[ord(TProp.yearly)] := ptruint(@obj.YearlyShapeObj);
    PropertyOffset[ord(TProp.daily)] := ptruint(@obj.DailyShapeObj);
    PropertyOffset[ord(TProp.duty)] := ptruint(@obj.DutyShapeObj);
    PropertyOffset[ord(TProp.Tyearly)] := ptruint(@obj.YearlyTShapeObj);
    PropertyOffset[ord(TProp.Tdaily)] := ptruint(@obj.DailyTShapeObj);
    PropertyOffset[ord(TProp.Tduty)] := ptruint(@obj.DutyTShapeObj);
    PropertyOffset[ord(TProp.EffCurve)] := ptruint(@obj.InverterCurveObj);
    PropertyOffset[ord(TProp.P__TCurve)] := ptruint(@obj.Power_TempCurveObj);
    PropertyOffset[ord(TProp.DynamicEq)] := ptruint(@obj.DynamicEqObj);

    PropertyOffset2[ord(TProp.yearly)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.daily)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.duty)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.Tyearly)] := ptruint(DSS.TShapeClass);
    PropertyOffset2[ord(TProp.Tdaily)] := ptruint(DSS.TShapeClass);
    PropertyOffset2[ord(TProp.Tduty)] := ptruint(DSS.TShapeClass);
    PropertyOffset2[ord(TProp.EffCurve)] := ptruint(DSS.XYCurveClass);
    PropertyOffset2[ord(TProp.P__TCurve)] := ptruint(DSS.XYCurveClass);
    PropertyOffset2[ord(TProp.DynamicEq)] := ptruint(DSS.DynamicExpClass);

    // boolean properties
    PropertyType[ord(TProp.debugtrace)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.Balanced)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.LimitCurrent)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.VarFollowInverter)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.WattPriority)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.PFPriority)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.debugtrace)] := ptruint(@obj.DebugTrace);
    PropertyOffset[ord(TProp.Balanced)] := ptruint(@obj.ForceBalanced);
    PropertyOffset[ord(TProp.LimitCurrent)] := ptruint(@obj.CurrentLimited);
    PropertyOffset[ord(TProp.VarFollowInverter)] := ptruint(@obj.VarFollowInverter);
    PropertyOffset[ord(TProp.WattPriority)] := ptruint(@obj.PVSystemVars.P_priority);
    PropertyOffset[ord(TProp.PFPriority)] := ptruint(@obj.PVSystemVars.PF_priority);

    PropertyType[ord(TProp.SafeMode)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.SafeMode)] := ptruint(@obj.dynVars.SafeMode);
    PropertyFlags[ord(TProp.SafeMode)] := [TPropertyFlag.SilentReadOnly];

    // integer properties
    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    PropertyType[ord(TProp.cls)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.cls)] := ptruint(@obj.FClass);
    PropertyType[ord(TProp.model)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.model)] := ptruint(@obj.VoltageModel); // TODO: enum?

    // double properties
    PropertyOffset[ord(TProp.irradiance)] := ptruint(@obj.PVSystemVars.FIrradiance);
    PropertyOffset[ord(TProp.pf)] := ptruint(@obj.PFnominal);
    PropertyOffset[ord(TProp.pctR)] := ptruint(@obj.pctR);
    PropertyOffset[ord(TProp.pctX)] := ptruint(@obj.pctX);
    PropertyOffset[ord(TProp.Temperature)] := ptruint(@obj.PVSystemVars.FTemperature);
    PropertyOffset[ord(TProp.Pmpp)] := ptruint(@obj.PVSystemVars.FPmpp);
    PropertyOffset[ord(TProp.pctCutin)] := ptruint(@obj.FpctCutIn);
    PropertyOffset[ord(TProp.pctCutout)] := ptruint(@obj.FpctCutOut);
    PropertyOffset[ord(TProp.Vminpu)] := ptruint(@obj.VMinPu);
    PropertyOffset[ord(TProp.Vmaxpu)] := ptruint(@obj.VMaxPu);
    PropertyOffset[ord(TProp.kVA)] := ptruint(@obj.PVSystemVars.FkVArating);
    PropertyOffset[ord(TProp.DutyStart)] := ptruint(@obj.DutyStart);
    PropertyOffset[ord(TProp.kv)] := ptruint(@obj.PVSystemVars.kVPVSystemBase);
    
    PropertyOffset[ord(TProp.kvar)] := ptruint(@obj.kvarRequested);
    PropertyReadFunction[ord(TProp.kvar)] := @Getkvar;
    PropertyFlags[ord(TProp.kvar)] := [TPropertyFlag.ReadByFunction];

    // double percent
    PropertyScale[ord(TProp.pctPmpp)] := 0.01;
    PropertyOffset[ord(TProp.pctPmpp)] := ptruint(@obj.PVSystemVars.FpuPmpp);

    // advanced doubles
    PropertyOffset[ord(TProp.pctPminNoVars)] := ptruint(@obj.FpctPminNoVars);
    PropertyFlags[ord(TProp.pctPminNoVars)] := [TPropertyFlag.Transform_Abs];

    PropertyOffset[ord(TProp.pctPminkvarMax)] := ptruint(@obj.FpctPminkvarLimit);
    PropertyFlags[ord(TProp.pctPminkvarMax)] := [TPropertyFlag.Transform_Abs];

    PropertyOffset[ord(TProp.kvarMax)] := ptruint(@obj.PVSystemVars.Fkvarlimit);
    PropertyFlags[ord(TProp.kvarMax)] := [TPropertyFlag.Transform_Abs];

    PropertyOffset[ord(TProp.kvarMaxAbs)] := ptruint(@obj.PVSystemVars.Fkvarlimitneg);
    PropertyFlags[ord(TProp.kvarMaxAbs)] := [TPropertyFlag.Transform_Abs];

    PropertyOffset[ord(TProp.kVDC)] := ptruint(@obj.dynVars.RatedVDC);
    PropertyScale[ord(TProp.kVDC)] := 1000;

    PropertyOffset[ord(TProp.kP)] := ptruint(@obj.dynVars.kP);
    PropertyScale[ord(TProp.kP)] := 1.0 / 1000.0;

    PropertyOffset[ord(TProp.PITol)] := ptruint(@obj.dynVars.CtrlTol);
    PropertyScale[ord(TProp.PITol)] := 1.0 / 100.0;

    PropertyOffset[ord(TProp.SafeVoltage)] := ptruint(@obj.dynVars.SMThreshold);


    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TPVsystem.NewObject(const ObjName: String; Activate: Boolean): Pointer;
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

procedure TPVsystem.UpdateAll;
var
    i: Integer;
begin
    for i := 1 to ElementList.Count do
        with TPVsystemObj(ElementList.Get(i)) do
            if Enabled then
                UpdatePVSystem;
end;

procedure TPVsystemObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    i: Integer;
begin
    case Idx of
        ord(TProp.UserModel):
            UserModel.Name := UserModelNameStr;  // Connect to user written models
        ord(TProp.UserData):
            if UserModel.Exists then
                UserModel.Edit := UserModelEditStr;  // Send edit string to user model

        ord(TProp.phases):
        begin
            SetNcondsForConnection(self); // Force Reallocation of terminal info
            PropertySideEffects(ord(TProp.kv), 0); // In case phases have been defined after
        end;
        ord(TProp.conn):
        begin
            SetNCondsForConnection(self);

            // VBase is always L-N voltage unless 1-phase device or more than 3 phases

            with PVSystemVars do
                case Fnphases of
                    2, 3:
                        VBase := kVPVSystemBase * InvSQRT3x1000; // L-N Volts
                else
                    VBase := kVPVSystemBase * 1000.0; // Just use what is supplied
                end;

            VBaseMin := Vminpu * VBase;
            VBaseMax := Vmaxpu * VBase;

            Yorder := Fnconds * Fnterms;
            YprimInvalid := TRUE;
        end;

        ord(TProp.pf):
            varMode := VARMODEPF;
        ord(TProp.kvar):
            varMode := VARMODEKVAR;

        ord(TProp.kv):
        begin
            with PVSystemVars do
                case FNphases of
                    2, 3:
                        VBase := kVPVSystemBase * InvSQRT3x1000;
                else
                    VBase := kVPVSystemBase * 1000.0;
                end;
        end;

        ord(TProp.kVA):
            with PVSystemVars do
            begin
                if not kvarLimitSet then
                    PVSystemVars.Fkvarlimit := FkVArating;
                if not kvarLimitSet and not kvarLimitNegSet then
                    PVSystemVars.Fkvarlimitneg := FkVArating;
            end;

        ord(TProp.kvarMaxAbs):
            kvarLimitNegSet := TRUE;

        ord(TProp.kvarMax):
            begin
                kvarLimitSet := TRUE;
                if not kvarLimitNegSet then
                    PVSystemVars.Fkvarlimitneg := Abs(PVSystemVars.Fkvarlimit);
            end;
        ord(TProp.debugtrace):
            if DebugTrace then
            begin   // Init trace file
                FreeAndNil(TraceFile);
                TraceFile := TBufferedFileStream.Create(DSS.OutputDirectory + 'STOR_' + Name + '.csv', fmCreate);
                FSWrite(TraceFile, 't, Iteration, LoadMultiplier, Mode, LoadModel, PVSystemModel,  Qnominalperphase, Pnominalperphase, CurrentType');
                for i := 1 to nphases do
                    FSWrite(Tracefile, ', |Iinj' + IntToStr(i) + '|');
                for i := 1 to nphases do
                    FSWrite(Tracefile, ', |Iterm' + IntToStr(i) + '|');
                for i := 1 to nphases do
                    FSWrite(Tracefile, ', |Vterm' + IntToStr(i) + '|');
                FSWrite(TraceFile, ',Vthev, Theta');
                FSWriteln(TraceFile);
                FSFlush(Tracefile);
            end
            else
            begin
                FreeAndNil(TraceFile);
            end;

        ord(TProp.DynamicEq):
            if DynamicEqObj <> NIL then
                SetLength(DynamicEqVals, DynamicEqObj.NVariables);

        ord(TProp.ControlMode):
        begin
            if GFM_mode then
            begin
                // Enables GFM mode for this IBR
                dynVars.ResetIBR := FALSE;
                if Length(dynVars.Vgrid) < NPhases then //TODO: check why this is not done in Storage
                    SetLength(dynVars.Vgrid, NPhases); // Used to store the voltage per phase
            end;
            YprimInvalid := TRUE;
        end;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TPVsystem.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
begin
    with TObj(ptr) do
    begin
        RecalcElementData;
        YPrimInvalid := TRUE;
        Exclude(Flags, Flg.EditionActive);
    end;
    Result := True;
end;

procedure TPVsystemObj.MakeLike(OtherPtr: Pointer);
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
        YprimInvalid := TRUE;
    end;

    PVSystemVars.kVPVSystemBase := Other.PVSystemVars.kVPVSystemBase;
    Vbase := Other.Vbase;
    Vminpu := Other.Vminpu;
    Vmaxpu := Other.Vmaxpu;
    VBaseMin := Other.VBaseMin;
    VBaseMax := Other.VBaseMax;
    kW_out := Other.kW_out;
    kvar_out := Other.kvar_out;
    Pnominalperphase := Other.Pnominalperphase;
    PFnominal := Other.PFnominal;
    Qnominalperphase := Other.Qnominalperphase;
    Connection := Other.Connection;
    YearlyShapeObj := Other.YearlyShapeObj;
    DailyShapeObj := Other.DailyShapeObj;
    DutyShapeObj := Other.DutyShapeObj;
    DutyStart := Other.DutyStart;
    YearlyTShapeObj := Other.YearlyTShapeObj;
    DailyTShapeObj := Other.DailyTShapeObj;
    DutyTShapeObj := Other.DutyTShapeObj;
    InverterCurveObj := Other.InverterCurveObj;
    Power_TempCurveObj := Other.Power_TempCurveObj;
    FClass := Other.FClass;
    VoltageModel := Other.VoltageModel;

    PVSystemVars.FTemperature := Other.PVSystemVars.FTemperature;
    PVSystemVars.FPmpp := Other.PVSystemVars.FPmpp;
    FpctCutin := Other.FpctCutin;
    FpctCutout := Other.FpctCutout;
    VarFollowInverter := Other.VarFollowInverter;
    PVSystemVars.Fkvarlimit := Other.PVSystemVars.Fkvarlimit;
    PVSystemVars.Fkvarlimitneg := Other.PVSystemVars.Fkvarlimitneg;
    FpctPminNoVars := Other.FpctPminNoVars;
    FpctPminkvarLimit := Other.FpctPminkvarLimit;
    kvarLimitSet := Other.kvarLimitSet;
    kvarLimitNegSet := Other.kvarLimitNegSet;

    PVSystemVars.FIrradiance := Other.PVSystemVars.FIrradiance;

    PVSystemVars.FkVArating := Other.PVSystemVars.FkVArating;

    pctR := Other.pctR;
    pctX := Other.pctX;

    RandomMult := Other.RandomMult;
    VWMode := Other.VWMode;
    WPMode := Other.WPMode;
    WVMode := Other.WVMode;
    DRCMode := Other.DRCMode;
    AVRMode := Other.AVRMode;
    UserModel.Name := Other.UserModel.Name;  // Connect to user written models
    UserModelNameStr := Other.UserModelNameStr;
    //UserModelEditStr := Other.UserModelEditStr;

    ForceBalanced := Other.ForceBalanced;
    CurrentLimited := Other.CurrentLimited;
end;

procedure TPVsystem.ResetRegistersAll;  // Force all EnergyMeters in the circuit to reset
var
    idx: Integer;
begin
    idx := First;
    while (idx > 0) do
    begin
        TPVsystemObj(GetActiveObj).ResetRegisters;
        idx := Next;
    end;
end;

procedure TPVsystem.SampleAll();  // Force all active PV System energy meters  to take a sample
var
    i: Integer;
begin
    for i := 1 to ElementList.Count do
        with TPVsystemObj(ElementList.Get(i)) do
            if Enabled then
                TakeSample();
end;

constructor TPVsystemObj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := AnsiLowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType; // + PVSystem_ELEMENT;  // In both PCelement and PVSystemelement list
    TraceFile := nil;

    FNphases := 3;
    Fnconds := 4;  // defaults to wye
    Yorder := 0;  // To trigger an initial allocation
    Nterms := 1;  // forces allocations

    YearlyShapeObj := NIL;  // If YearlyShapeobj = nil Then the Irradiance alway stays nominal
    DailyShapeObj := NIL;  // If DaillyShapeobj = nil Then the Irradiance alway stays nominal
    DutyShapeObj := NIL;  // If DutyShapeobj = nil Then the Irradiance alway stays nominal
    DutyStart := 0.0;

    YearlyTShapeObj := NIL;  // If YearlyShapeobj = nil Then the Temperature always stays nominal
    DailyTShapeObj := NIL;  // If DaillyShapeobj = nil Then the Temperature always stays nominal
    DutyTShapeObj := NIL;  // If DutyShapeobj = nil Then the Temperature always stays nominal

    InverterCurveObj := NIL;
    Power_TempCurveObj := NIL;

    Connection := 0;    // Wye (star, L-N)
    VoltageModel := 1;  // Typical fixed kW negative load
    FClass := 1;

    PVSystemSolutionCount := -1;  // For keep track of the present solution in Injcurrent calcs
    OpenPVSystemSolutionCount := -1;
    YPrimOpenCond := NIL;

    PVSystemVars.kVPVSystemBase := 12.47;
    VBase := 7200.0;
    Vminpu := 0.90;
    Vmaxpu := 1.10;
    VBaseMin := Vminpu * Vbase;
    VBaseMax := Vmaxpu * Vbase;
    Yorder := Fnterms * Fnconds;
    RandomMult := 1.0;

    varMode := VARMODEPF;
    InverterON := TRUE;
    VarFollowInverter := FALSE;
    ForceBalanced := FALSE;
    CurrentLimited := FALSE;

    with PVSystemVars do
    begin
        FTemperature := 25.0;
        FIrradiance := 1.0;  // kW/sq-m
        FkVArating := 500.0;
        FPmpp := 500.0;
        FpuPmpp := 1.0;    // full on
        Vreg := 9999;
        Vavg := 9999;
        VVOperation := 9999;
        VWOperation := 9999;
        DRCOperation := 9999;
        VVDRCOperation := 9999;
        WPOperation := 9999;
        WVOperation := 9999;
        //         kW_out_desired  :=9999;
        Fkvarlimit := FkVArating;
        Fkvarlimitneg := FkVArating;
        P_Priority := FALSE;    // This is a change from older versions
        PF_Priority := FALSE;
    end;
    with dynVars do
    begin
        RatedVDC := 8000;
        SMThreshold := 80;
        SafeMode := FALSE;
        kP := 0.00001;
    end;

    FpctCutIn := 20.0;
    FpctCutOut := 20.0;

    FpctPminNoVars := -1.0;
    FpctPminkvarLimit := -1.0;

    Fpf_wp_nominal := 1.0;

    // Output rating stuff
    kW_out := 500.0;
    kvar_out := 0.0;
    PFnominal := 1.0;

    pctR := 50.0;
    pctX := 0.0;

    PublicDataStruct := @PVSystemVars;
    PublicDataSize := SizeOf(TPVSystemVars);

    kvarLimitSet := FALSE;
    kvarLimitNegSet := FALSE;


    UserModel := TPVsystemUserModel.Create(DSS);
    UserModelNameStr := '';
    UserModelEditStr := '';

    Reg_kWh := 1;
    Reg_kvarh := 2;
    Reg_MaxkW := 3;
    Reg_MaxkVA := 4;
    Reg_Hours := 5;
    Reg_Price := 6;

    DebugTrace := FALSE;
    PVsystemObjSwitchOpen := FALSE;
    SpectrumObj := NIL; // override base class
    VWMode := FALSE;
    VVMode := FALSE;
    WVMode := FALSE;
    WPMode := FALSE;
    DRCMode := FALSE;
    AVRMode := FALSE;
    RecalcElementData();
end;

destructor TPVsystemObj.Destroy;
begin
    YPrimOpenCond.Free;
    UserModel.Free;
    FreeAndNil(TraceFile);
    inherited Destroy;
end;

procedure TPVsystemObj.Randomize(Opt: Integer);
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

procedure TPVsystemObj.CalcDailyMult(Hr: Double);
begin
    if (DailyShapeObj <> NIL) then
    begin
        ShapeFactor := DailyShapeObj.GetMultAtHour(Hr);
    end
    else
        ShapeFactor := CDOUBLEONE;  // Default to no  variation
end;

procedure TPVsystemObj.CalcDailyTemperature(Hr: Double);
begin
    if (DailyTShapeObj <> NIL) then
    begin
        TShapeValue := DailyTShapeObj.GetTemperature(Hr);
    end
    else
        TShapeValue := PVSystemVars.FTemperature;
    ;  // Default to no  variation
end;

procedure TPVsystemObj.CalcDutyMult(Hr: Double);
begin
    if DutyShapeObj <> NIL then
    begin
        ShapeFactor := DutyShapeObj.GetMultAtHour(Hr + DutyStart);
    end
    else
        CalcDailyMult(Hr);  // Default to Daily Mult If no duty curve specified
end;

procedure TPVsystemObj.CalcDutyTemperature(Hr: Double);
begin
    if DutyTShapeObj <> NIL then
    begin
        TShapeValue := DutyTShapeObj.GetTemperature(Hr);
    end
    else
        CalcDailyTemperature(Hr);  // Default to Daily Mult If no duty curve specified
end;

procedure TPVsystemObj.CalcYearlyMult(Hr: Double);
begin
    if YearlyShapeObj <> NIL then
    begin
        ShapeFactor := YearlyShapeObj.GetMultAtHour(Hr + DutyStart);
    end
    else
        CalcDailyMult(Hr);  // Defaults to Daily curve
end;

procedure TPVsystemObj.CalcYearlyTemperature(Hr: Double);
begin
    if YearlyTShapeObj <> NIL then
    begin
        TShapeValue := YearlyTShapeObj.GetTemperature(Hr);
    end
    else
        CalcDailyTemperature(Hr);  // Defaults to Daily curve
end;

procedure TPVsystemObj.GetCurrents(Curr: pComplexArray);
// Required for operation in GFM mode
var
    i: Integer;
begin
    if not GFM_Mode then
    begin
        inherited GetCurrents(Curr);
        Exit;
    end;

    // if GFM_Mode then
    with ActiveCircuit.Solution do
    begin
        try
            for i := 1 to Yorder do
                Vterminal[i] := NodeV[NodeRef[i]];

            YPrim.MVMult(Curr, Vterminal);  // Current from Elements in System Y
            CalcInjCurrentArray();  // Get present value of inj currents
            // Add Together with yprim currents
            for i := 1 to Yorder do
                Curr[i] -=  InjCurrent[i];
        except
            On E: Exception do
                DoErrorMsg(Format(_('GetCurrents for Element: %s.'), [Name]), E.Message, _('Inadequate storage allotted for circuit element.'), 327);
        end;
    end;
end;

function TPVsystemObj.CheckOLInverter(): Boolean;
// Returns True if any of the inverter phases is overloaded
var
    MaxAmps: Double;
    PhaseAmps: Double;
    i: Integer;
begin
  // Check if reaching saturation point in GFM
    Result := FALSE;
    if not GFM_Mode then
        Exit;

    ComputePanelPower();
    MaxAmps := ((PVSystemvars.PanelkW * 1000) / NPhases) / VBase;
    ComputeIterminal();
    for i := 1 to NPhases do
    begin
        PhaseAmps := cabs(Iterminal[i]);
        if PhaseAmps > MaxAmps then
        begin
            Result := TRUE;
            Exit;
        end;
    end;
end;

procedure TPVsystemObj.DoGFM_Mode();
// Implements the grid forming inverter control routine for the PVSystem device
var
    i: Integer;
begin
    dynVars.BaseV := VBase;
    dynVars.Discharging := TRUE;

    with ActiveCircuit.Solution, dynVars do
    begin
        // Initialization just in case
        if length(Vgrid) < NPhases then
            SetLength(Vgrid, NPhases);

        for i := 1 to NPhases do
            Vgrid[i - 1] := ctopolar(NodeV[NodeRef[i]]);
        
        CalcGFMVoltage(NPhases, Vterminal);
        YPrim.MVMult(InjCurrent, Vterminal);
        set_ITerminalUpdated(FALSE);
    end;
end;

procedure TPVsystemObj.RecalcElementData();
begin
    VBaseMin := VMinPu * VBase;
    VBaseMax := VMaxPu * VBase;

    varBase := 1000.0 * kvar_out / Fnphases;

    with PVSystemVars do
    begin
        // values in ohms for thevenin equivalents
        RThev := pctR * 0.01 * SQR(PresentkV) / FkVArating * 1000.0;
        XThev := pctX * 0.01 * SQR(PresentkV) / FkVArating * 1000.0;

        CutInkW := FpctCutin * FkVArating / 100.0;
        CutOutkW := FpctCutOut * FkVArating / 100.0;

        if FpctPminNoVars <= 0 then
            PminNoVars := -1
        else
            PminNoVars := FpctPminNoVars * FPmpp / 100.0;

        if FpctPminkvarLimit <= 0 then
            PminkvarLimit := -1
        else
            PminkvarLimit := FpctPminkvarLimit * FPmpp / 100.0;

    end;

    SetNominalPVSystemOuput();

    // Initialize to Zero - defaults to PQ PVSystem element
    // Solution object will reset after circuit modifications

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

    // Update any user-written models
    if Usermodel.Exists then
        UserModel.FUpdateModel;
end;

procedure TPVsystemObj.SetNominalPVSystemOuput();
begin
    ShapeFactor := CDOUBLEONE;  // init here; changed by curve routine
    TShapeValue := PVSystemVars.FTemperature; // init here; changed by curve routine

    // Check to make sure the PVSystem element is ON
    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        if not (IsDynamicModel or IsHarmonicModel) then     // Leave PVSystem element in whatever state it was prior to entering Dynamic mode
        begin
            // Check dispatch to see what state the PVSystem element should be in
            with Solution do
                case Mode of
                    TSolveMode.SNAPSHOT: ; // Just solve for the present kW, kvar  // Don't check for state change
                    TSolveMode.DAILYMODE:
                    begin
                        CalcDailyMult(DynaVars.dblHour);
                        CalcDailyTemperature(DynaVars.dblHour);
                    end;
                    TSolveMode.YEARLYMODE:
                    begin
                        CalcYearlyMult(DynaVars.dblHour);
                        CalcYearlyTemperature(DynaVars.dblHour);
                    end;
                    // TSolveMode.MONTECARLO1,
                    // TSolveMode.MONTEFAULT,
                    // TSolveMode.FAULTSTUDY,
                    // TSolveMode.DYNAMICMODE:   ; // // do nothing yet
                    TSolveMode.GENERALTIME:
                    begin
                         // This mode allows use of one class of load shape
                        case ActiveCircuit.ActiveLoadShapeClass of
                            USEDAILY:
                            begin
                                CalcDailyMult(DynaVars.dblHour);
                                CalcDailyTemperature(DynaVars.dblHour);
                            end;
                            USEYEARLY:
                            begin
                                CalcYearlyMult(DynaVars.dblHour);
                                CalcYearlyTemperature(DynaVars.dblHour);
                            end;
                            USEDUTY:
                            begin
                                CalcDutyMult(DynaVars.dblHour);
                                CalcDutyTemperature(DynaVars.dblHour);
                            end;
                        else
                            ShapeFactor := CDOUBLEONE     // default to 1 + j1 if not known
                        end;
                    end;

                // Assume Daily curve, If any, for the following
                    TSolveMode.MONTECARLO2,
                    TSolveMode.MONTECARLO3,
                    TSolveMode.LOADDURATION1,
                    TSolveMode.LOADDURATION2:
                    begin
                        CalcDailyMult(DynaVars.dblHour);
                        CalcDailyTemperature(DynaVars.dblHour);
                    end;
                    TSolveMode.PEAKDAY:
                    begin
                        CalcDailyMult(DynaVars.dblHour);
                        CalcDailyTemperature(DynaVars.dblHour);
                    end;

                    TSolveMode.DUTYCYCLE:
                    begin
                        CalcDutyMult(DynaVars.dblHour);
                        CalcDutyTemperature(DynaVars.dblHour);
                    end;
                // AUTOADDFLAG:  ; 
                end;

            ComputekWkvar;
            Pnominalperphase := 1000.0 * kW_out / Fnphases;
            Qnominalperphase := 1000.0 * kvar_out / Fnphases;

            case VoltageModel of

              //****  Fix this when user model gets connected in
                3: // YEQ := Cinv(cmplx(0.0, -StoreVARs.Xd))  ;  // Gets negated in CalcYPrim

            else

                YEQ := Cmplx(Pnominalperphase, -Qnominalperphase) / Sqr(Vbase);   // Vbase must be L-N for 3-phase

                if (Vminpu <> 0.0) then
                    YEQ_Min := YEQ / SQR(Vminpu)  // at 95% voltage
                else
                    YEQ_Min := YEQ; // Always a constant Z model

                if (Vmaxpu <> 0.0) then
                    YEQ_Max := YEQ / SQR(Vmaxpu)   // at 105% voltage
                else
                    YEQ_Max := YEQ;

            { Like Model 7 generator, max current is based on amount of current to get out requested power at min voltage
            }
                with PVSystemvars do
                begin
                    PhaseCurrentLimit := Cmplx(Pnominalperphase, Qnominalperphase) / VBaseMin;
                    MaxDynPhaseCurrent := Cabs(PhaseCurrentLimit);
                end;


            end;
           // When we leave here, all the YEQ's are in L-N values

        end;  // If  NOT (IsDynamicModel or IsHarmonicModel)
    end;  // With ActiveCircuit
end;

procedure TPVsystemObj.CalcYPrimMatrix(Ymatrix: TcMatrix);
var
    Y, Yij: Complex;
    i, j: Integer;
    FreqMultiplier: Double;
begin
    FYprimFreq := ActiveCircuit.Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;

    with ActiveCircuit.solution do
    begin
        if IsHarmonicModel then
        begin
          // YEQ is computed from %R and %X -- inverse of Rthev + j Xthev
            Y := YEQ;   // L-N value computed in initialization routines

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
                    begin   // Delta connection
                        Ymatrix.SetElement(i, i, Y);
                        Ymatrix.AddElement(i, i, Y);  // put it in again
                        for j := 1 to i - 1 do
                            Ymatrix.SetElemsym(i, j, Yij);
                    end;
                end;
            end;
            Exit;
        end;

        if GFM_Mode then
        begin
            // The inverter is in GFM control modem calculation changes
            with dynVars do
            begin
                RatedkVLL := PresentkV;
                mKVARating := PVSystemVars.FkVArating;
                CalcGFMYprim(NPhases, @YMatrix);
            end;
            Exit;
        end;

        
        //  Regular power flow PVSystem element model
        
        // YEQ is always expected as the equivalent line-neutral admittance
        Y := -YEQ;   // negate for generation    YEQ is L-N quantity

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
    end;
end;

procedure TPVsystemObj.ComputeInverterPower;
var
    kVA_Gen: Double;
    Qramp_limit: Double = 0.0;
    TempPF: Double = 0.0;
    CutOutkWAC: Double;
    // CutInkWAC: Double;
begin
    // Reset CurrentkvarLimit to kvarLimit
    CurrentkvarLimit := PVSystemVars.Fkvarlimit;
    CurrentkvarLimitNeg := PVSystemVars.Fkvarlimitneg;

    with PVSystemVars do
    begin
        EffFactor := 1.0;
        kW_Out := 0.0;

        if Assigned(InverterCurveObj) then
        begin
            CutOutkWAC := CutOutkW * InverterCurveObj.GetYValue(abs(CutOutkW) / FkVArating);
            // CutInkWAC := CutInkW * InverterCurveObj.GetYValue(abs(CutInkW) / FkVArating);
        end
        else  // Assume Ideal Inverter
        begin
            CutOutkWAC := CutOutkW;
            // CutInkWAC := CutInkW;
        end;


        // Determine state of the inverter
        if InverterON then
        begin
            if Panelkw < CutOutkW then
            begin
                InverterON := FALSE;
            end;
        end
        else
        begin
            if Panelkw >= CutInkW then
            begin
                InverterON := TRUE;
            end;
        end;

        // set inverter output. Defaults to 100% of the panelkW if no efficiency curve spec'd
        if InverterON then
        begin
            if Assigned(InverterCurveObj) then
                EffFactor := InverterCurveObj.GetYValue(PanelkW / FkVArating);  // pu eff vs pu power
            kWOut_Calc;
        end
        else
        begin
            kW_Out := 0.0;
        end;

        if (abs(kW_Out) < PminNoVars) then
        begin
            kvar_out := 0.0;  // Check minimum P for Q gen/absorption. if PminNoVars is disabled (-1), this will always be false

            CurrentkvarLimit := 0;
            CurrentkvarLimitNeg := 0.0;  // Set current limit to be used by InvControl's Check_Qlimits procedure.
        end
        else
        if varMode = VARMODEPF then
        begin
            if PFnominal = 1.0 then
                kvar_out := 0.0
            else
            begin
                kvar_out := kW_out * sqrt(1.0 / SQR(PFnominal) - 1.0) * sign(PFnominal);

                // Check limits
                if abs(kW_out) < PminkvarLimit then // straight line limit check. if PminkvarLimit is disabled (-1), this will always be false.
                begin
                    // straight line starts at max(PminNoVars, CutOutkWAC)
                    // if CutOut differs from CutIn, take cutout since it is assumed that CutOut <= CutIn always.
                    if abs(kW_out) >= max(PminNoVars, CutOutkWAC) then
                    begin
                        if (kvar_Out > 0.0) then
                        begin
                            Qramp_limit := Fkvarlimit / PminkvarLimit * abs(kW_out);   // generation limit
                            CurrentkvarLimit := Qramp_limit;  // For use in InvControl
                        end
                        else
                        if (kvar_Out < 0.0) then
                        begin
                            Qramp_limit := Fkvarlimitneg / PminkvarLimit * abs(kW_out);   // absorption limit
                            CurrentkvarLimitNeg := Qramp_limit;  // For use in InvControl
                        end;

                        if abs(kvar_Out) > Qramp_limit then
                            kvar_out := Qramp_limit * sign(kW_out) * sign(PFnominal);
                    end;
                end
                else
                if (abs(kvar_Out) > Fkvarlimit) or (abs(kvar_Out) > Fkvarlimitneg) then  // Other cases, check normal kvarLimit and kvarLimitNeg
                begin
                    if (kvar_Out > 0.0) then
                        kvar_out := Fkvarlimit * sign(kW_out) * sign(PFnominal)
                    else
                        kvar_out := Fkvarlimitneg * sign(kW_out) * sign(PFnominal);

                    if PF_Priority then // Forces constant power factor when kvar limit is exceeded and PF Priority is true.
                    begin
                        kW_out := kvar_out * sqrt(1.0 / (1.0 - Sqr(PFnominal)) - 1.0) * sign(PFnominal);
                    end;
                end;
            end;
        end
        else     // kvar is specified
        begin
            // Check limits
            if abs(kW_out) < PminkvarLimit then // straight line limit check. if PminkvarLimit is disabled (-1), this will always be false.
            begin
                  // straight line starts at max(PminNoVars, CutOutkWAC)
                  // if CutOut differs from CutIn, take cutout since it is assumed that CutOut <= CutIn always.
                if abs(kW_out) >= max(PminNoVars, CutOutkWAC) then
                begin
                    if (kvarRequested > 0.0) then
                    begin
                        Qramp_limit := Fkvarlimit / PminkvarLimit * abs(kW_out);   // generation limit
                        CurrentkvarLimit := Qramp_limit;   // For use in InvControl
                    end
                    else
                    if (kvarRequested < 0.0) then
                    begin
                        Qramp_limit := Fkvarlimitneg / PminkvarLimit * abs(kW_out);   // absorption limit
                        CurrentkvarLimitNeg := Qramp_limit;   // For use in InvControl
                    end;

                    if abs(kvarRequested) > Qramp_limit then
                        kvar_out := Qramp_limit * sign(kvarRequested)
                    else
                        kvar_out := kvarRequested;
                end;
            end
            else
            if ((kvarRequested > 0.0) and (abs(kvarRequested) >= Fkvarlimit)) or ((kvarRequested < 0.0) and (abs(kvarRequested) >= Fkvarlimitneg)) then
            begin
                if (kvarRequested > 0.0) then
                    kvar_Out := Fkvarlimit * sign(kvarRequested)
                else
                    kvar_Out := Fkvarlimitneg * sign(kvarRequested);

                if (varMode = VARMODEKVAR) and PF_Priority and WPMode then
                begin
                    kW_out := abs(kvar_out) * sqrt(1.0 / (1.0 - Sqr(Fpf_wp_nominal)) - 1.0) * sign(kW_out);
                end
                // Forces constant power factor when kvar limit is exceeded and PF Priority is true. Temp PF is calculated based on kvarRequested
                else
                if PF_Priority and (not VVMode or not DRCMode or not WVmode or not AVRMode) then
                begin
                    if abs(kvarRequested) > 0.0 then
                    begin
                        TempPF := cos(arctan(abs(kvarRequested / kW_out)));
                        kW_out := abs(kvar_out) * sqrt(1.0 / (1.0 - Sqr(TempPF)) - 1.0) * sign(kW_out);
                    end;
                end;
            end
            else
                kvar_Out := kvarRequested;
        end;

        if (InverterON = FALSE) and (VarFollowInverter = TRUE) then
            kvar_out := 0.0;

        // Limit kvar and kW so that kVA of inverter is not exceeded
        kVA_Gen := Sqrt(Sqr(kW_out) + Sqr(kvar_out));

        if kVA_Gen > FkVArating then
        begin
            if (varMode = VARMODEPF) and PF_Priority then
              // Operates under constant power factor when kVA rating is exceeded. PF must be specified and PFPriority must be TRUE
            begin
                kW_out := FkVArating * abs(PFnominal);
                kvar_out := FkVArating * sqrt(1 - Sqr(PFnominal)) * sign(PFnominal);
            end
            else
            if (varMode = VARMODEKVAR) and PF_Priority and WPMode then
            begin
                kW_out := FkVArating * abs(Fpf_wp_nominal) * sign(kW_out);
                kvar_out := FkVArating * abs(sin(ArcCos(Fpf_wp_nominal))) * sign(kvarRequested)
            end
            else
            if (varMode = VARMODEKVAR) and PF_Priority and (not VVMode or not DRCMode or not WVmode or not AVRMode) then
              // Operates under constant power factor (PF implicitly calculated based on kW and kvar)
            begin
                if abs(kvar_out) = Fkvarlimit then
                begin   // for handling cases when kvar limit and inverter's kVA limit are exceeded
                    kW_out := FkVArating * abs(TempPF) * sign(kW_out);
                end
                else
                begin
                    kW_out := FkVArating * abs(cos(ArcTan(kvarRequested / kW_out))) * sign(kW_out);
                end;
                kvar_out := FkVArating * abs(sin(ArcCos(kW_out / FkVArating))) * sign(kvarRequested);
            end
            else
            begin
                if P_Priority then
                begin  // back off the kvar
                    if kW_out > FkVArating then
                    begin
                        kW_out := FkVArating;
                        kvar_out := 0.0;
                    end
                    else
                        kvar_Out := Sqrt(SQR(FkVArating) - SQR(kW_Out)) * sign(kvar_Out);
                end
                else
                    kW_Out := Sqrt(SQR(FkVArating) - SQR(kvar_Out)) * sign(kW_Out);
            end;
        end;
        if (InverterON = FALSE) and (VarFollowInverter = TRUE) then
            kvar_out := 0.0;
    end;  // With PVSystemvars
end;


procedure TPVsystemObj.ComputekWkvar;
begin
    ComputePanelPower;   // apply irradiance
    ComputeInverterPower; // apply inverter eff after checking for cutin/cutout
end;

procedure TPVsystemObj.ComputePanelPower;
begin
    with PVSystemVars do
    begin
        TempFactor := 1.0;
        if Assigned(Power_TempCurveObj) then
        begin
            TempFactor := Power_TempCurveObj.GetYValue(TshapeValue);  // pu Pmpp vs T (actual)
        end;

        PanelkW := FIrradiance * ShapeFactor.re * FPmpp * TempFactor;
    end;
end;

procedure TPVsystemObj.CalcYPrim();

var
    i: Integer;

begin
    // Build only shunt Yprim
    // Build a dummy Yprim Series so that CalcV Does not fail
    if YprimInvalid then
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

    SetNominalPVSystemOuput();
    CalcYPrimMatrix(YPrim_Shunt);

    // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages Doesn't fail
    for i := 1 to Yorder do
        Yprim_Series.SetElement(i, i, Yprim_Shunt.Getelement(i, i) * 1.0e-10);

    YPrim.CopyFrom(YPrim_Shunt);

    // Account for Open Conductors
    inherited CalcYPrim();
end;

procedure TPVsystemObj.StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);
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

procedure TPVsystemObj.WriteTraceRecord(const s: String);

var
    i: Integer;
    sout: String;
begin
    try
        if (not DSS.InshowResults) then
        begin
            WriteStr(sout, Format('%-.g, %d, %-.g, ',
                [ActiveCircuit.Solution.DynaVARs.t,
                ActiveCircuit.Solution.Iteration,
                ActiveCircuit.LoadMultiplier]),
                DSS.SolveModeEnum.OrdinalToString(ord(DSS.ActiveCircuit.Solution.mode)), ', ',
                DSS.DefaultLoadModelEnum.OrdinalToString(DSS.ActiveCircuit.Solution.LoadModel), ', ',
                VoltageModel: 0, ', ',
                (Qnominalperphase * 3.0 / 1.0e6): 8: 2, ', ',
                (Pnominalperphase * 3.0 / 1.0e6): 8: 2, ', ',
                s, ', ');
            FSWrite(TraceFile, sout);
            for i := 1 to nphases do
            begin
                WriteStr(sout, (Cabs(InjCurrent^[i])): 8: 1, ', ');
                FSWrite(TraceFile, sout);
            end;
            for i := 1 to nphases do
            begin
                WriteStr(sout, (Cabs(ITerminal^[i])): 8: 1, ', ');
                FSWrite(TraceFile, sout);
            end;
            for i := 1 to nphases do
            begin
                WriteStr(sout, (Cabs(Vterminal^[i])): 8: 1, ', ');
                FSWrite(TraceFile, sout);
            end;

            FSWriteln(Tracefile);
            FSFlush(TraceFile);
        end;
    except
        On E: Exception do
        begin
        end;

    end;
end;


procedure TPVsystemObj.DoConstantPQPVsystemObj();

// Compute total terminal current for Constant PQ

var
    i: Integer;
    PhaseCurr,
    DeltaCurr,
    VLN, VLL: Complex;
    VmagLN,
    VmagLL: Double;
    V012: array[0..2] of Complex;  // Sequence voltages

begin
    //Treat this just like the Load model

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    ZeroITerminal;

    CalcVTerminalPhase(); // get actual voltage across each phase of the load

    if ForceBalanced and (Fnphases = 3) then
    begin  // convert to pos-seq only
        Phase2SymComp(Vterminal, pComplexArray(@V012));
        V012[0] := CZERO; // Force zero-sequence voltage to zero
        V012[2] := CZERO; // Force negative-sequence voltage to zero
        SymComp2Phase(Vterminal, pComplexArray(@V012));  // Reconstitute Vterminal as balanced
    end;

    for i := 1 to Fnphases do
    begin
        case Connection of

            0:
            begin  // Wye
                VLN := Vterminal^[i];
                VMagLN := Cabs(VLN);

                if CurrentLimited then
                begin
                    // Current-Limited Model
                    PhaseCurr := cong(Cmplx(Pnominalperphase, Qnominalperphase) / VLN);
                    if Cabs(PhaseCurr) > PVSystemVars.MaxDynPhaseCurrent then
                        PhaseCurr := cong(PhaseCurrentLimit / (VLN / VMagLN));
                end
                else
                begin
                    // The usual model
                    if (VMagLN <= VBaseMin) then
                        PhaseCurr := YEQ_Min * VLN  // Below Vminpu use an impedance model
                    else
                    if (VMagLN > VBaseMax) then
                        PhaseCurr := YEQ_Max * VLN  // above Vmaxpu use an impedance model
                    else
                        PhaseCurr := cong(Cmplx(Pnominalperphase, Qnominalperphase) / VLN);  // Between Vminpu and Vmaxpu, constant PQ
                end;

                StickCurrInTerminalArray(ITerminal, -PhaseCurr, i);  // Put into Terminal array taking into account connection
                set_ITerminalUpdated(TRUE);
                StickCurrInTerminalArray(InjCurrent, PhaseCurr, i);  // Put into Terminal array taking into account connection
            end;

            1:
            begin  // Delta
                VLL := Vterminal^[i];
                VMagLL := Cabs(VLL);

                if CurrentLimited then
                begin
                    // Current-Limited Model
                    DeltaCurr := cong(Cmplx(Pnominalperphase, Qnominalperphase) / VLL);
                    if Cabs(DeltaCurr) * SQRT3 > PVSystemVars.MaxDynPhaseCurrent then
                        DeltaCurr := cong(PhaseCurrentLimit / (VLL / (VMagLL / SQRT3)));
                end
                else
                begin
                   // The usual model
                    case Fnphases of
                        2, 3:
                            VMagLN := VmagLL / SQRT3;
                    else
                        VMagLN := VmagLL;
                    end;

                    if VMagLN <= VBaseMin then
                        DeltaCurr := (YEQ_Min / 3.0) * VLL  // Below 95% use an impedance model
                    else
                    if VMagLN > VBaseMax then
                        DeltaCurr := (YEQ_Max / 3.0) * VLL  // above 105% use an impedance model
                    else
                        DeltaCurr := cong(Cmplx(Pnominalperphase, Qnominalperphase) / VLL);  // Between 95% -105%, constant PQ
                end;

                StickCurrInTerminalArray(ITerminal, -DeltaCurr, i);  // Put into Terminal array taking into account connection
                set_ITerminalUpdated(TRUE);
                StickCurrInTerminalArray(InjCurrent, DeltaCurr, i);  // Put into Terminal array taking into account connection
            end;

        end;

    end;
end;

procedure TPVsystemObj.DoConstantZPVsystemObj;
// constant Z model
var
    i: Integer;
    Curr,
    YEQ2: Complex;
    V012: array[0..2] of Complex;  // Sequence voltages

begin
    // Assume YEQ is kept up to date
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase(); // get actual voltage across each phase of the load

    if ForceBalanced and (Fnphases = 3) then
    begin  // convert to pos-seq only
        Phase2SymComp(Vterminal, pComplexArray(@V012));
        V012[0] := CZERO; // Force zero-sequence voltage to zero
        V012[2] := CZERO; // Force negative-sequence voltage to zero
        SymComp2Phase(Vterminal, pComplexArray(@V012));  // Reconstitute Vterminal as balanced
    end;

    ZeroITerminal;

    if (Connection = 0) then
        YEQ2 := YEQ        // YEQ is always line to neutral
    else
        YEQ2 := YEQ / 3.0;          // YEQ for delta connection

    for i := 1 to Fnphases do
    begin
        Curr := YEQ2 * Vterminal^[i];
        StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(TRUE);
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;

procedure TPVsystemObj.DoUserModel;
// Compute total terminal Current from User-written model
var
    i: Integer;

begin
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array

    if UserModel.Exists then     // Check automatically selects the usermodel If true
    begin
        UserModel.FCalc(Vterminal, Iterminal);
        set_ITerminalUpdated(TRUE);
        with ActiveCircuit.Solution do
        begin          // Negate currents from user model for power flow PVSystem element model
            for i := 1 to FnConds do
                InjCurrent^[i] -= Iterminal^[i];
        end;
    end
    else
        DoSimpleMsg('%s model designated to use user-written model, but user-written model is not defined.', [FullName], 567);
end;

procedure TPVsystemObj.DoDynamicMode;
// Compute Total Current and add into InjTemp
var
    PolarN: Polar;
    i: Integer;
    // V012, I012: array[0..2] of Complex;
    NeutAmps: Complex;
    Vthev: Complex;
    iActual: Double;
    Theta: Double; // phase angle of thevinen source

    // -------------- Internal Proc -----------------------
    procedure CalcVthev_Dyn(const V: Complex);
    // If the voltage magnitude drops below 15% or so, the accuracy of determining the
    // phase angle gets flaky. This algorithm approximates the action of a PLL that will
    // hold the last phase angle until the voltage recovers.
    begin
        // Try to keep in phase with terminal voltage

        with PVSystemVars do
        begin
            if Cabs(V) > 0.20 * Vbase then
                Theta := ThetaDyn + (Cang(V) - InitialVangle)
            else
                Theta := LastThevAngle;

            Vthev := pclx(VthevMagDyn, Theta); //TODO: check why it's unused
            LastThevAngle := Theta;     // remember this for angle persistence
        end;
    end;

begin
    if GFM_Mode then
    begin
        dynVars.BaseV := dynVars.BasekV * 1000 * (dynVars.it[0] / dynVars.IMaxPPhase);  // Uses dynamics model as reference
        dynVars.CalcGFMVoltage(NPhases, Vterminal);
        YPrim.MVMult(InjCurrent, Vterminal);
        Exit;
    end;

    CalcYPrimContribution(InjCurrent); // Init InjCurrent Array  and computes VTerminal
    // Inj = -Itotal (in) - Yprim*Vtemp
    case VoltageModel of
        3:
            if UserModel.Exists then // auto selects model (User model)
            begin
                // {We have total currents in Iterminal
                UserModel.FCalc(Vterminal, Iterminal);  // returns terminal currents in Iterminal
            end
            else
            begin
                DoSimpleMsg(Format('Dynamics model missing for PVSystem.%s ', [Name]), 5671);
                DSS.SolutionAbort := TRUE;
            end;
    else  
        //All other models
        
        // This model has no limitation in the nmber of phases and is ideally unbalanced (no dq-dv, but is implementable as well)

        // First, get the phase angles for the currents
        NeutAmps := 0;
        for i := 1 to FNphases do
        begin
            with dynVars do
            begin
                // determine if the PV panel is ON
                if (it[i - 1] <= iMaxPPhase) or GFM_Mode then
                    iActual := it[i - 1]
                else
                    iActual := iMaxPPhase;
        
                PolarN := topolar(iActual, Vgrid[i - 1].ang); // Output Current estimated for active power
                Iterminal[i] := -ptocomplex(PolarN);
                NeutAmps := NeutAmps - Iterminal[i];
            end;
        end;
        if FnConds > FNphases then
            Iterminal[FnConds] := NeutAmps;
    end;
    // Add it into inj current array
    for i := 1 to FnConds do
        InjCurrent[i] -= Iterminal[i];

    set_ITerminalUpdated(TRUE);
end;

procedure TPVsystemObj.DoHarmonicMode();
// Compute Injection Current Only when in harmonics mode

// Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built
// Vd is the fundamental frequency voltage behind Xd" for phase 1
var
    i: Integer;
    E: Complex;
    PVSystemHarmonic: Double;
    pBuffer: PCBuffer24;
begin
    pBuffer := @TPVsystem(ParentClass).cBuffer; // TODO: not thread-safe

    ComputeVterminal();

    with ActiveCircuit.Solution, PVSystemVars do
    begin
        PVSystemHarmonic := Frequency / PVSystemFundamental;
        if SpectrumObj <> NIL then
            E := SpectrumObj.GetMult(PVSystemHarmonic) * VThevHarm // Get base harmonic magnitude
        else
            E := CZERO;

        RotatePhasorRad(E, PVSystemHarmonic, ThetaHarm);  // Time shift by fundamental frequency phase shift
        for i := 1 to Fnphases do
        begin
            pBuffer[i] := E;
            if i < Fnphases then
                RotatePhasorDeg(E, PVSystemHarmonic, -120.0);  // Assume 3-phase PVSystem element
        end;
    end;

    // Handle Wye Connection
    if Connection = 0 then
        pBuffer[Fnconds] := Vterminal^[Fnconds];  // assume no neutral injection voltage

    // Inj currents = Yprim (E) 
    YPrim.MVMult(InjCurrent, pComplexArray(pBuffer));
end;

procedure TPVsystemObj.CalcVTerminalPhase();

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

    PVSystemSolutionCount := ActiveCircuit.Solution.SolutionCount;
end;

procedure TPVsystemObj.CalcPVSystemModelContribution();
// Calculates PVSystem element current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)
begin
    set_ITerminalUpdated(FALSE);
    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        if IsDynamicModel then
        begin
            DoDynamicMode();
            Exit;
        end;
        if IsHarmonicModel and (Frequency <> Fundamental) then
        begin
            DoHarmonicMode();
            Exit;
        end;
        if GFM_Mode then
        begin
            DoGFM_Mode();
            Exit;
        end;

        // Compute currents and put into InjTemp array;
        case VoltageModel of
            1:
                DoConstantPQPVsystemObj();
            2:
                DoConstantZPVsystemObj();
            3:
                DoUserModel();
        else
            DoConstantPQPVsystemObj();  // for now, until we implement the other models.
        end;
    end;
    // When this is Done, ITerminal is up to date
end;

procedure TPVsystemObj.CalcInjCurrentArray();
// Difference between currents in YPrim and total current
begin
    // Now Get Injection Currents
    if PVsystemObjSwitchOpen then
        ZeroInjCurrent
    else
        CalcPVSystemModelContribution();
end;

procedure TPVsystemObj.GetTerminalCurrents(Curr: pComplexArray);
// Compute total Currents
begin
    with ActiveCircuit.Solution do
    begin
        if IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount then
        begin     // recalc the contribution
            if not PVsystemObjSwitchOpen then
                CalcPVSystemModelContribution();  // Adds totals in Iterminal as a side effect
        end;
        inherited GetTerminalCurrents(Curr);
    end;

    if (DebugTrace) then
        WriteTraceRecord('TotalCurrent');
end;

function TPVsystemObj.InjCurrents(): Integer;
begin
    with ActiveCircuit.Solution do
    begin
        if LoadsNeedUpdating then
            SetNominalPVSystemOuput(); // Set the nominal kW, etc for the type of solution being Done

        CalcInjCurrentArray();          // Difference between currents in YPrim and total terminal current

        if (DebugTrace) then
            WriteTraceRecord('Injection');

        // Add into System Injection Current Array

        Result := inherited InjCurrents();
    end;
end;

procedure TPVsystemObj.ResetRegisters;
var
    i: Integer;
begin
    for i := 1 to NumPVSystemRegisters do
        Registers[i] := 0.0;
    for i := 1 to NumPVSystemRegisters do
        Derivatives[i] := 0.0;
    FirstSampleAfterReset := TRUE;  // initialize for trapezoidal integration
end;

procedure TPVsystemObj.Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);
begin
    if ActiveCircuit.TrapezoidalIntegration then
    begin
        // Trapezoidal Rule Integration
        if not FirstSampleAfterReset then
            Registers[Reg] := Registers[Reg] + 0.5 * Interval * (Deriv + Derivatives[Reg]);
    end
    else   // Plain Euler integration
        Registers[Reg] := Registers[Reg] + Interval * Deriv;
    Derivatives[Reg] := Deriv;
end;

procedure TPVsystemObj.TakeSample();
// Update Energy from metered zone
var
    S: Complex;
    Smag: Double;
    HourValue: Double;
begin
    // Compute energy in PVSystem element branch
    if Enabled then
    begin
        S := cmplx(Get_PresentkW, Get_Presentkvar);
        Smag := Cabs(S);
        HourValue := 1.0;

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
            Integrate(Reg_Price, S.re * ActiveCircuit.PriceSignal * 0.001, IntervalHrs);  //
            FirstSampleAfterReset := FALSE;
        end;
    end;
end;

procedure TPVsystemObj.UpdatePVSystem;
// Update PVSystem levels
begin
    // Do Nothing
end;

function TPVsystemObj.Get_PresentkW: Double;
begin
    Result := Pnominalperphase * 0.001 * Fnphases;
end;

function TPVsystemObj.Get_PresentIrradiance: Double;
begin
    Result := PVSystemVars.FIrradiance * ShapeFactor.re;
end;

function TPVsystemObj.Get_Presentkvar: Double;
begin
    Result := Qnominalperphase * 0.001 * Fnphases;
end;

procedure TPVsystemObj.InitHarmonics();
// This routine makes a thevenin equivalent behis the reactance spec'd in %R and %X
var
    E, Va: complex;
begin
    YprimInvalid := TRUE;  // Force rebuild of YPrims
    PVSystemFundamental := ActiveCircuit.Solution.Frequency;  // Whatever the frequency is when we enter here.

    // Compute reference Thevinen voltage from phase 1 current

    ComputeIterminal();  // Get present value of current

    with ActiveCircuit.solution do
    begin
        case Connection of
            0:
            begin // wye - neutral is explicit
                Va := NodeV^[NodeRef^[1]] - NodeV^[NodeRef^[Fnconds]];
            end;

            1:
            begin  // delta -- assume neutral is at zero
                Va := NodeV^[NodeRef^[1]];
            end;
        end;
    end;

    with PVSystemVars do
    begin
        YEQ := Cinv(Cmplx(RThev, XThev));           // used for current calcs  Always L-N
        E := Va - Iterminal^[1] * cmplx(Rthev, Xthev);
        Vthevharm := Cabs(E);   // establish base mag and angle
        ThetaHarm := Cang(E);
    end;
end;

procedure TPVsystemObj.InitStateVars();
// for going into dynamics mode
var
    i: Integer;
    BaseZt: Double;
begin
    YprimInvalid := TRUE; // Force rebuild of YPrims

    with PVSystemVars, dynVars do
    begin
        if (Length(PICtrl) = 0) or (Length(PICtrl) < Fnphases) then
        begin
            setlength(PICtrl, Fnphases);
            for i := 0 to (Fnphases - 1) do
            begin
                PICtrl[i] := TPICtrl.Create;
                PICtrl[i].Kp := dynVars.kP;
                PICtrl[i].kNum := 0.9502;
                PICtrl[i].kDen := 0.04979;
            end;
        end;
        SafeMode := FALSE;
        with ActiveCircuit.Solution do
        begin
            case ActiveCircuit.ActiveLoadShapeClass of
                USEDAILY:
                begin
                    CalcDailyMult(DynaVars.dblHour);
                    CalcDailyTemperature(DynaVars.dblHour);
                end;
                USEYEARLY:
                begin
                    CalcYearlyMult(DynaVars.dblHour);
                    CalcYearlyTemperature(DynaVars.dblHour);
                end;
                USEDUTY:
                begin
                    CalcDutyMult(DynaVars.dblHour);
                    CalcDutyTemperature(DynaVars.dblHour);
                end;
            else
                ShapeFactor := CDOUBLEONE // default to 1 + j1 if not known
            end;
        end;

        ComputePanelPower();
        NumPhases := Fnphases; // set Publicdata vars
        NumConductors := Fnconds;
        Conn := Connection;
        // Sets the length of State vars to cover the num of phases
        InitDynArrays(NumPhases);

        if NumPhases > 1 then
            BasekV := PresentkV / sqrt(3)
        else
            BasekV := PresentkV;

        BaseZt := 0.01 * (SQR(PresentkV) / FkVArating) * 1000;
        MaxVS := (2 - (SMThreshold / 100)) * BasekV * 1000;
        MinVS := (SMThreshold / 100) * BasekV * 1000;
        MinAmps := (FpctCutOut / 100) * ((FkVArating / BasekV) / NumPhases);
        ResetIBR := FALSE;
        iMaxPPhase := (FkVArating / BasekV) / NumPhases;
        if pctX = 0 then
            pctX := 50; // forces the value to 50% in dynamics mode if not given

        XThev := pctX * BaseZt;
        RS := pctR * BaseZt;
        Zthev := Cmplx(RS, XThev);
        YEQ := 1 / Zthev; // used for current calcs  Always L-N

        ComputeIterminal();
        with ActiveCircuit.Solution do
        begin
            LS := XThev / (2 * PI * DSS.DefaultBaseFreq);

            for i := 0 to (NPhases - 1) do
            begin
                dit[i] := 0;
                Vgrid[i] := ctopolar(NodeV^[NodeRef^[i + 1]]);
                if GFM_Mode then
                    it[i] := 0
                else
                    it[i] := ((PanelkW * 1000) / Vgrid[i].mag) / NumPhases;
                
                m[i] := ((RS * it[i]) + Vgrid[i].mag) / RatedVDC; // Duty factor in terms of actual voltage

                if m[i] > 1 then
                    m[i] := 1;
                ISPDelta[i] := 0;
                AngDelta[i] := 0;
            end;
            if DynamicEqObj <> NIL then
                for i := 0 to High(DynamicEqVals) do
                    DynamicEqVals[i][1] := 0.0; // Initializes the memory values for the dynamic equation
        end;
    end;
end;

procedure TPVsystemObj.IntegrateStates();
// dynamics mode integration routine
var
    NumData, j, i: Integer;
begin
    // Compute Derivatives and Then integrate
    ComputeIterminal();
    if Usermodel.Exists then
    begin
        Usermodel.Integrate(); // Checks for existence and Selects
        Exit;
    end;

    // Compute actual power output for the PVPanel
    with ActiveCircuit.Solution do
    begin
        case ActiveCircuit.ActiveLoadShapeClass of
            USEDAILY:
            begin
                CalcDailyMult(DynaVars.dblHour);
                CalcDailyTemperature(DynaVars.dblHour);
            end;
            USEYEARLY:
            begin
                CalcYearlyMult(DynaVars.dblHour);
                CalcYearlyTemperature(DynaVars.dblHour);
            end;
            USEDUTY:
            begin
                CalcDutyMult(DynaVars.dblHour);
                CalcDutyTemperature(DynaVars.dblHour);
            end;
        else
            ShapeFactor := CDOUBLEONE // default to 1 + j1 if not known
        end;
    end;
    
    ComputePanelPower();
    
    with ActiveCircuit.Solution, PVSystemVars, dynVars do
    begin
        IMaxPPhase := (PanelkW / BasekV) / NumPhases;
        for i := 0 to (NumPhases - 1) do // multiphase approach
        begin
            with DynaVars do
                if (IterationFlag = 0) then
                begin // First iteration of new time step
                    itHistory[i] := it[i] + 0.5 * h * dit[i];
                end;
            
            Vgrid[i] := ctopolar(NodeV^[NodeRef[i + 1]]); // Voltage at the Inv terminals
            // Compute the actual target (Amps)

            if not GFM_Mode then
            begin
                ISP := ((PanelkW * 1000) / Vgrid[i].mag) / NumPhases;
                if ISP > IMaxPPhase then
                    ISP := IMaxPPhase;
                if (Vgrid[i].mag < MinVS) then
                    ISP := 0.01; // turn off the inverter
            end
            else
            begin
                if ResetIBR then
                    VDelta[i] := (0.001 - (Vgrid[i].mag / 1000)) / BasekV
                else
                    VDelta[i] := (BasekV - (Vgrid[i].mag / 1000)) / BasekV;
                if abs(VDelta[i]) > CtrlTol then
                begin
                    ISPDelta[i] += (IMaxPPhase * VDelta[i]) * kP * 100;
                    if ISPDelta[i] > IMaxPPhase then
                        ISPDelta[i] := IMaxPPhase
                    else
                    if ISPDelta[i] < 0 then
                        ISPDelta[i] := 0.01;
                end;
                ISP := ISPDelta[i];
                FixPhaseAngle(i);
            end;
            if DynamicEqObj <> NIL then // Loads values into dynamic expression if any
            begin
                NumData := (length(DynamicEqPair) div 2) - 1;
                DynamicEqVals[DynOut[0]][0] := it[i]; // brings back the current values/phase
                DynamicEqVals[DynOut[0]][1] := dit[i];

                for j := 0 to NumData do
                begin
                    if not DynamicEqObj.IsInitVal(DynamicEqPair[(j * 2) + 1]) then // it's not intialization
                    begin
                        case DynamicEqPair[(j * 2) + 1] of
                            2:
                                DynamicEqVals[DynamicEqPair[j * 2]][0] := Vgrid[i].mag; // volt per phase
                            4: 
                                ; // Nothing for this object (current)
                            10:
                                DynamicEqVals[DynamicEqPair[j * 2]][0] := RatedVDC;
                            11:
                            begin
                                SolveModulation(ActiveCircuit, i, @PICtrl[i]);
                                DynamicEqVals[DynamicEqPair[j * 2]][0] := m[i]
                            end
                        else
                            DynamicEqVals[DynamicEqPair[j * 2]][0] := PCEValue[1, DynamicEqPair[(j * 2) + 1]];
                        end;
                    end;
                end;
                DynamicEqObj.SolveEq(DynamicEqVals); // solves the differential equation using the given dynamic expression
            end
            else
                SolveDynamicStep(ActiveCircuit, i, @PICtrl[i]); // Solves dynamic step for inverter (no dynamic expression)

            // Trapezoidal method
            with DynaVars do
            begin
                if DynamicEqObj <> NIL then
                    dit[i] := DynamicEqVals[DynOut[0]][1];

                it[i] := itHistory[i] + 0.5 * h * dit[i];
            end;
        end;
    end;
end;

function TPVsystemObj.Get_Variable(i: Integer): Double;
// Return variables one at a time
var
    N, k: Integer;
begin
    Result := -9999.99;  // error return value; no state fars

    if i < 1 then
    begin
        DoSimpleMsg('%s: invalid variable index %d.', [FullName, i], 565);
        Exit;
    end;
    if DynamicEqObj <> NIL then
    begin
        if i <= DynamicEqObj.NVariables * Length(DynamicEqVals[0]) then
            Result := DynamicEqObj.Get_DynamicEqVal(i - 1, DynamicEqVals)
        else
            DoSimpleMsg('%s: invalid variable index %d.', [FullName, i], 565);
        Exit;
    end;

    with PVSystemVars do
        case i of
            1:
                Result := PresentIrradiance;
            2:
                Result := PanelkW;
            3:
                Result := TempFactor;
            4:
                Result := EffFactor;
            5:
                Result := Vreg;
            6:
                Result := Vavg;
            7:
                Result := VVOperation;
            8:
                Result := VWOperation;
            9:
                Result := DRCOperation;
            10:
                Result := VVDRCOperation;
            11:
                Result := WPOperation;
            12:
                Result := WVOperation;
            13:
                Result := PanelkW * EffFactor;
            (NumBasePVSystemVariables + 1)..NumPVSystemVariables:
                Result := dynVars.Get_InvDynValue(i - NumBasePVSystemVariables - 1, NumPhases);
        else
            if UserModel.Exists then
            begin
                N := UserModel.FNumVars;
                k := (i - NumPVSystemVariables);
                if k <= N then
                begin
                    Result := UserModel.FGetVariable(k);
                    Exit;
                end;
            end;
        end;
end;

procedure TPVsystemObj.kWOut_Calc;
var
    Pac: Double;
    PpctLimit: Double;
begin
    with PVSystemVars do
    begin
        Pac := PanelkW * EffFactor;

        if VWmode or WVmode then
        begin
            if (Pac > kwrequested) then
                kW_Out := kwrequested
            else
                kW_Out := Pac
        end
        else
        begin
            PpctLimit := FPmpp * FpuPmpp;
            if (Pac > PpctLimit) then
                kW_Out := PpctLimit
            else
                kW_Out := Pac;
        end;
    end;
end;

procedure TPVsystemObj.Set_Variable(i: Integer; Value: Double);
var
    N, k: Integer;
begin
    if i < 1 then
    begin
        DoSimpleMsg('%s: invalid variable index %d.', [FullName, i], 565);
        Exit;  // No variables to set
    end;
    if DynamicEqObj <> NIL then
    begin
        DoSimpleMsg('%s: cannot set state variable when using DynamicEq.', [FullName], 566);
        Exit;
    end;

    with PVSystemVars do
        case i of
            1:
                FIrradiance := Value;
            2, 3, 4: // Setting this has no effect Read only
                DoSimpleMsg('%s: variable index %d is read-only.', [FullName, i], 564);
            5:
                Vreg := Value; // the InvControl or ExpControl will do this
            6:
                Vavg := Value;
            7:
                VVOperation := Value;
            8:
                VWOperation := Value;
            9:
                DRCOperation := Value;
            10:
                VVDRCOperation := Value;
            11:
                WPOperation := Value;
            12:
                WVOperation := Value;
            13: ; //ReadOnly //kW_out_desired := Value;
            (NumBasePVSystemVariables + 1)..NumPVSystemVariables:
                dynVars.Set_InvDynValue(i - NumBasePVSystemVariables - 1, Value);
        else
            if UserModel.Exists then
            begin
                N := UserModel.FNumVars;
                k := (i - NumPVSystemVariables);
                if k <= N then
                begin
                    UserModel.FSetVariable(k, Value);
                    Exit;
                end;
            end;
        end;
end;

procedure TPVsystemObj.GetAllVariables(var States: ArrayOfDouble);
var
    i: Integer;
begin
    if DynamicEqObj <> NIL then
    begin
        for i := 1 to DynamicEqObj.NVariables * Length(DynamicEqVals[0]) do
            States[i - 1] := DynamicEqObj.Get_DynamicEqVal(i - 1, DynamicEqVals);

        Exit;
    end;

    for i := 1 to NumPVSystemVariables do
        States[i - 1] := Variable[i];

    if UserModel.Exists then
        UserModel.FGetAllVars(pDoubleArray(@States[NumPVSystemVariables]));
end;

function TPVsystemObj.NumVariables(): Integer;
begin
    // Try DynamicExp first
    Result := inherited NumVariables();
    if Result <> 0 then 
        Exit;

    // Fallback to the classic
    Result := NumPVSystemVariables;
    if UserModel.Exists then
        Result := Result + UserModel.FNumVars;
end;

function TPVsystemObj.VariableName(i: Integer): String;
const
    BuffSize = 255;
var
    n,
    i2: Integer;
    Buff: array[0..BuffSize] of AnsiChar;
    pName: pAnsichar;
begin
    if i < 1 then
        Exit;  // Someone goofed

    // Try DynamicExp first
    Result := inherited VariableName(i);
    if Length(Result) <> 0 then
        Exit;

    // Fallback to the classic
    case i of
        1:
            Result := 'Irradiance';
        2:
            Result := 'PanelkW';
        3:
            Result := 'P_TFactor';
        4:
            Result := 'Efficiency';
        5:
            Result := 'Vreg';
        6:
            Result := 'Vavg (DRC)';
        7:
            Result := 'volt-var';
        8:
            Result := 'volt-watt';
        9:
            Result := 'DRC';
        10:
            Result := 'VV_DRC';
        11:
            Result := 'watt-pf';
        12:
            Result := 'watt-var';
        13:
            Result := 'kW_out_desired';
        (NumBasePVSystemVariables + 1)..NumPVSystemVariables:
            Result := dynVars.Get_InvDynName(i - NumBasePVSystemVariables - 1);
    else
        if UserModel.Exists then
        begin
            pName := PAnsiChar(@Buff);
            n := UserModel.FNumVars;
            i2 := i - NumPVSystemVariables;
            if (i2 <= n) then
            begin
                UserModel.FGetVarName(i2, pName, BuffSize);
                Result := String(pName);
                Exit;
            end;
        end;
    end;
end;

procedure TPVsystemObj.MakePosSequence();
var
    newkVA, newPF, V: Double;
    oldPhases, changes: Integer;
begin
    with PVSystemVars do
    begin
        BeginEdit(True);
        // Make sure voltage is line-neutral
        if (Fnphases > 1) or (connection <> 0) then
            V := kVPVSystemBase / SQRT3
        else
            V := kVPVSystemBase;

        oldPhases := FnPhases;
        changes := 3;
        if oldPhases > 1 then
        begin
            newkVA := kVArating / Fnphases;
            newPF := PFNominal;
            changes := changes + 2;
        end;
        SetInteger(ord(TProp.Phases), 1);
        SetInteger(ord(TProp.conn), 0);
        SetDouble(ord(TProp.kV), V);
        if oldPhases > 1 then
        begin
            SetDouble(ord(TProp.kVA), newkVA);
            SetDouble(ord(TProp.PF), newPF);
        end;
        EndEdit(changes);
    end;

    inherited;   // write out other properties
end;

procedure TPVsystemObj.Set_ConductorClosed(Index: Integer; Value: Boolean);
begin
    inherited;

    // Just turn PVSystem element on or off;
    PVsystemObjSwitchOpen := not Value;
end;

procedure TPVsystemObj.Set_Maxkvar(const Value: Double);
begin
    PVSystemVars.Fkvarlimit := Value;
    SetAsNextSeq(ord(TProp.kvarMax));
end;

procedure TPVsystemObj.Set_Maxkvarneg(const Value: Double);
begin
    PVSystemVars.Fkvarlimitneg := Value;
    SetAsNextSeq(ord(TProp.kvarMaxAbs));
end;

procedure TPVsystemObj.Set_kVARating(const Value: Double);
begin
    PVSystemVars.FkVARating := Value;
    SetAsNextSeq(ord(TProp.kVA));
end;

procedure TPVsystemObj.Set_Pmpp(const Value: Double);
begin
    PVSystemVars.FPmpp := Value;
    SetAsNextSeq(ord(TProp.Pmpp));
end;

procedure TPVsystemObj.Set_PowerFactor(const Value: Double);
begin
    PFnominal := Value;
    varMode := VARMODEPF;
end;

procedure TPVsystemObj.Set_pf_wp_nominal(const Value: Double);
begin
    Fpf_wp_nominal := Value;
end;

procedure TPVsystemObj.SetDragHandRegister(Reg: Integer; const Value: Double);
begin
    if (Value > Registers[reg]) then
        Registers[Reg] := Value;
end;

function TPVSystemObj.IsPVSystem(): Boolean;
begin
    Result := True;
end;

function TPVSystemObj.GetPFPriority(): Boolean;
begin
    Result := PVSystemVars.PF_Priority;
end;

procedure TPVSystemObj.SetPFPriority(value: Boolean);
begin
    PVSystemVars.PF_Priority := value;
end;


end.