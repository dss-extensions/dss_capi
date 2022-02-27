unit PVsystem;

{
  ----------------------------------------------------------
  Copyright (c) 2011-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

//  To Do:
//    Make connection to User model
//    Yprim for various modes
//    Define state vars and dynamics mode behavior
//    Complete Harmonics mode algorithm (generator mode is implemented)

//  The PVsystem element is essentially a generator that consists of a PV panel and an inverter.
//
//  The PVsystem element can also produce or absorb vars within the kVA rating of the inverter.
//  // WGS: Updated 9/24/2015 to allow for simultaneous modes and additional functionality in the InvControl.

//  The PVsystem element is assumed balanced over the no. of phases defined


interface

uses
    Classes,
    PVsystemUserModel,
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    UComplex, DSSUcomplex,
    LoadShape,
    TempShape,
    XYCurve,
    Spectrum,
    ArrayDef,
    Dynamics;

const
    NumPVSystemRegisters = 6;    // Number of energy meter registers
    NumPVSystemVariables = 5;    // No state variables that need integrating.
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
        kvarLimit = 34, // propkvarLimit
        DutyStart = 35, // propDutyStart
        WattPriority = 36 // propPpriority
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
        Vreg: Double; // will be set from InvControl or ExpControl

        // 32-bit integers
        NumPhases: Integer;   // Number of phases
        NumConductors: Integer;// Total Number of conductors (wye-connected will have 4)
        Conn: Integer;   // 0 = wye; 1 = Delta
        P_Priority: Boolean;  // default False // added 10/30/2018

    end;

    TPVSystem = class(TPCClass)
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
        procedure SampleAll;
        procedure UpdateAll;

    end;

    TPVsystemObj = class(TPCElement)
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

        PFSpecified: Boolean;
        kvarSpecified: Boolean;

        ForceBalanced: Boolean;
        CurrentLimited: Boolean;

        kvar_out: Double;
        kW_out: Double;
        kvarRequested: Double;
        kWRequested: Double;

        FpctCutIn: Double;
        FpctCutOut: Double;
        FVarFollowInverter: Boolean;
        CutInkW: Double;
        CutOutkW: Double;
        FInverterON: Boolean;

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
        VBase: Double;  // Base volts suitable for computing currents
        VBaseMax: Double;
        VBaseMin: Double;
        Vmaxpu: Double;
        Vminpu: Double;
        YPrimOpenCond: TCmatrix;

        FVWMode: Boolean; //boolean indicating if under volt-watt control mode from InvControl (not ExpControl)
        FVWYAxis: Integer;  // integer value indicating that whether y-axis of watts is in %Pmpp or %PAvailable
                                  // 1 = %Pmpp, 0=%PAvailable.  Default is 1 such that pctPmpp user-settable
                                  // property will correctly operate on Pmpp (NOT PAvailable)
        procedure CalcDailyMult(Hr: Double);  // now incorporates DutyStart offset
        procedure CalcDutyMult(Hr: Double);
        procedure CalcYearlyMult(Hr: Double);  // now incorporates DutyStart offset

        procedure CalcDailyTemperature(Hr: Double);
        procedure CalcDutyTemperature(Hr: Double);
        procedure CalcYearlyTemperature(Hr: Double);

        procedure ComputePanelPower;
        procedure ComputeInverterPower;

        procedure ComputekWkvar;
        procedure CalcPVSystemModelContribution;   // This is where the power gets computed
        procedure CalcInjCurrentArray;
        procedure CalcVTerminalPhase;

        procedure CalcYPrimMatrix(Ymatrix: TcMatrix);

        procedure DoConstantPQPVsystemObj;
        procedure DoConstantZPVsystemObj;
        procedure DoDynamicMode;
        procedure DoHarmonicMode;
        procedure DoUserModel;

        procedure Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);
        procedure SetDragHandRegister(Reg: Integer; const Value: Double);
        procedure StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);

        procedure WriteTraceRecord(const s: String);

        procedure UpdatePVSystem;    // Update PVSystem elements based on present kW and IntervalHrs variable

        function Get_PresentkW: Double;
        function Get_Presentkvar: Double;
        function Get_PresentkV: Double;
        function Get_PresentIrradiance: Double;

        procedure Set_PresentkV(const Value: Double);
        procedure Set_Presentkvar(const Value: Double);
        procedure Set_PresentkW(const Value: Double);
        procedure Set_PowerFactor(const Value: Double);
        procedure Set_PresentIrradiance(const Value: Double);

        procedure Set_kVARating(const Value: Double);
        procedure Set_Pmpp(const Value: Double);
        procedure Set_puPmpp(const Value: Double);
        function Get_Varmode: Integer;

        procedure Set_Varmode(const Value: Integer);
        function Get_VWmode: Boolean;

        procedure Set_VWmode(const Value: Boolean);
        function Get_VWYAxis: Integer;

        procedure Set_VWYAxis(const Value: Integer);

        procedure kWOut_Calc;

    PROTECTED
        procedure GetTerminalCurrents(Curr: pComplexArray); OVERRIDE;

    PUBLIC

        PVSystemVars: TPVSystemVars;

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

        FClass: Integer;
        VoltageModel: Integer;   // Variation with voltage
        PFnominal: Double;

        Registers, Derivatives: array[1..NumPVSystemRegisters] of Double;

        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure Set_ConductorClosed(Index: Integer; Value: Boolean); OVERRIDE;
        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        function InjCurrents: Integer; OVERRIDE;
        function NumVariables: Integer; OVERRIDE;
        procedure GetAllVariables(States: pDoubleArray); OVERRIDE;
        function Get_Variable(i: Integer): Double; OVERRIDE;
        procedure Set_Variable(i: Integer; Value: Double); OVERRIDE;
        function VariableName(i: Integer): String; OVERRIDE;

        function Get_InverterON: Boolean;
        procedure Set_InverterON(const Value: Boolean);
        function Get_VarFollowInverter: Boolean;
        procedure Set_VarFollowInverter(const Value: Boolean);
        procedure Set_Maxkvar(const Value: Double);
        procedure SetNominalPVSystemOuput;
        procedure Randomize(Opt: Integer);   // 0 = reset to 1.0; 1 = Gaussian around mean and std Dev  ;  // 2 = uniform


        procedure ResetRegisters;
        procedure TakeSample;

        // Support for Dynamics Mode
        procedure InitStateVars; OVERRIDE;
        procedure IntegrateStates; OVERRIDE;

        // Support for Harmonics Mode
        procedure InitHarmonics; OVERRIDE;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model

        property PresentIrradiance: Double READ Get_PresentIrradiance WRITE Set_PresentIrradiance;
        property PresentkW: Double READ Get_PresentkW WRITE Set_PresentkW;
        property Presentkvar: Double READ Get_Presentkvar WRITE Set_Presentkvar;
        property PresentkV: Double READ Get_PresentkV WRITE Set_PresentkV;
        property PowerFactor: Double READ PFnominal WRITE Set_PowerFactor;
        property kVARating: Double READ PVSystemVars.FkVARating WRITE Set_kVARating;
        property Pmpp: Double READ PVSystemVars.FPmpp WRITE Set_pmpp;
        property puPmpp: Double READ PVSystemVars.FpuPmpp WRITE Set_puPmpp;
        property Varmode: Integer READ Get_Varmode WRITE Set_Varmode;  // 0=constant PF; 1=kvar specified
        property VWmode: Boolean READ Get_VWmode WRITE Set_VWmode;
        property VWYAxis: Integer READ Get_VWYAxis WRITE Set_VWYAxis;
        property InverterON: Boolean READ Get_InverterON WRITE Set_InverterON;
        property VarFollowInverter: Boolean READ Get_VarFollowInverter WRITE Set_VarFollowInverter;
        property kvarLimit: Double READ PVSystemVars.Fkvarlimit WRITE Set_Maxkvar;
        property MinModelVoltagePU: Double READ VminPu;
        property IrradianceNow :Double READ ShapeFactor.re;
    end;

implementation

uses
    Circuit,
    Sysutils,
    Command,
    Math,
    MathUtil,
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TPVsystemObj;
    TProp = TPVSystemProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer;    

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

function Getkvar(Obj: TObj): Double;
begin
    Result := Obj.kvar_out;
end;

procedure TPVsystem.DefineProperties;
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

    // bus properties
    PropertyType[ord(TProp.bus1)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus1)] := 1;

    // strings
    PropertyType[ord(TProp.UserModel)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.UserModel)] := ptruint(@obj.UserModelNameStr);
    PropertyType[ord(TProp.UserData)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.UserData)] := ptruint(@obj.UserModelEditStr);

    // integer properties
    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    PropertyType[ord(TProp.cls)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.cls)] := ptruint(@obj.FClass);
    PropertyType[ord(TProp.model)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.model)] := ptruint(@obj.VoltageModel); // TODO: enum?

    // object properties
    PropertyType[ord(TProp.yearly)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.daily)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.duty)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.Tyearly)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.Tdaily)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.Tduty)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.EffCurve)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.P__TCurve)] := TPropertyType.DSSObjectReferenceProperty;
    
    PropertyOffset[ord(TProp.yearly)] := ptruint(@obj.YearlyShapeObj);
    PropertyOffset[ord(TProp.daily)] := ptruint(@obj.DailyShapeObj);
    PropertyOffset[ord(TProp.duty)] := ptruint(@obj.DutyShapeObj);
    PropertyOffset[ord(TProp.Tyearly)] := ptruint(@obj.YearlyTShapeObj);
    PropertyOffset[ord(TProp.Tdaily)] := ptruint(@obj.DailyTShapeObj);
    PropertyOffset[ord(TProp.Tduty)] := ptruint(@obj.DutyTShapeObj);
    PropertyOffset[ord(TProp.EffCurve)] := ptruint(@obj.InverterCurveObj);
    PropertyOffset[ord(TProp.P__TCurve)] := ptruint(@obj.Power_TempCurveObj);

    PropertyOffset2[ord(TProp.yearly)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.daily)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.duty)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.Tyearly)] := ptruint(DSS.TShapeClass);
    PropertyOffset2[ord(TProp.Tdaily)] := ptruint(DSS.TShapeClass);
    PropertyOffset2[ord(TProp.Tduty)] := ptruint(DSS.TShapeClass);
    PropertyOffset2[ord(TProp.EffCurve)] := ptruint(DSS.XYCurveClass);
    PropertyOffset2[ord(TProp.P__TCurve)] := ptruint(DSS.XYCurveClass);

    // booleans
    PropertyType[ord(TProp.LimitCurrent)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.LimitCurrent)] := ptruint(@obj.CurrentLimited);
    PropertyType[ord(TProp.Balanced)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.Balanced)] := ptruint(@obj.ForceBalanced);
    PropertyType[ord(TProp.VarFollowInverter)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.VarFollowInverter)] := ptruint(@obj.FVarFollowInverter);
    PropertyType[ord(TProp.debugtrace)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.debugtrace)] := ptruint(@obj.DebugTrace);
    PropertyType[ord(TProp.WattPriority)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.WattPriority)] := ptruint(@obj.PVSystemVars.P_priority);

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
    PropertyOffset[ord(TProp.kvarLimit)] := ptruint(@obj.PVSystemVars.Fkvarlimit);
    PropertyFlags[ord(TProp.kvarLimit)] := [TPropertyFlag.Transform_Abs];


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

        ord(TProp.conn):
        begin
            SetNCondsForConnection(self);

            // VBase is always L-N voltage unless 1-phase device or more than 3 phases
            with PVSystemVars do
                case Fnphases of
                    2, 3:
                        VBase := kVPVSystemBase * InvSQRT3x1000;    // L-N Volts
                else
                    VBase := kVPVSystemBase * 1000.0;   // Just use what is supplied
                end;

            VBaseMin := Vminpu * VBase;
            VBaseMax := Vmaxpu * VBase;

            Yorder := Fnconds * Fnterms;
            YPrimInvalid := TRUE;
        end;

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

        ord(TProp.kvar):
        begin
            kvarSpecified := TRUE;
            PFSpecified := FALSE;
        end;

        ord(TProp.pf):
        begin
            PFSpecified := TRUE;
            kvarSpecified := FALSE;
        end;

        ord(TProp.kVA):
            with PVSystemVars do
                Fkvarlimit := FkVArating;   // Reset kvar limit to kVA rating

        ord(TProp.phases):
            SetNCondsForConnection(self);  // Force Reallocation of terminal info

        ord(TProp.debugtrace):
            if DebugTrace then
            begin   // Init trace file
                FreeAndNil(TraceFile);
                TraceFile := TFileStream.Create(DSS.OutputDirectory + 'STOR_' + Name + '.csv', fmCreate);
                FSWrite(TraceFile, 't, Iteration, LoadMultiplier, Mode, LoadModel, PVSystemModel,  Qnominalperphase, Pnominalperphase, CurrentType');
                for i := 1 to nphases do
                    FSWrite(Tracefile, ', |Iinj' + IntToStr(i) + '|');
                for i := 1 to nphases do
                    FsWrite(Tracefile, ', |Iterm' + IntToStr(i) + '|');
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
        YPrimInvalid := TRUE;
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
    FVarFollowInverter := Other.FVarFollowInverter;
    PVSystemVars.Fkvarlimit := Other.PVSystemVars.Fkvarlimit;

    PVSystemVars.FIrradiance := Other.PVSystemVars.FIrradiance;

    PVSystemVars.FkVArating := Other.PVSystemVars.FkVArating;

    pctR := Other.pctR;
    pctX := Other.pctX;

    RandomMult := Other.RandomMult;
    FVWMode := Other.FVWMode;
    FVWYAxis := Other.FVWYAxis;
    UserModel.Name := Other.UserModel.Name;  // Connect to user written models
    UserModelNameStr := Other.UserModelNameStr;
    //UserModelEditStr := Other.UserModelEditStr; -- TODO: not copied?

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

procedure TPVsystem.SampleAll;  // Force all active PV System energy meters  to take a sample
var
    i: Integer;
begin
    for i := 1 to ElementList.Count do
        with TPVsystemObj(ElementList.Get(i)) do
            if Enabled then
                TakeSample;
end;

constructor TPVsystemObj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := LowerCase(SourceName);
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

    PFSpecified := TRUE;
    kvarSpecified := FALSE;
    FInverterON := TRUE; // start with inverterON
    FVarFollowInverter := FALSE;
    ForceBalanced := FALSE;
    CurrentLimited := FALSE;

    with PVSystemVars do
    begin
        FTemperature := 25.0;
        FIrradiance := 1.0;  // kW/sq-m
        FkVArating := 500.0;
        FPmpp := 500.0;
        FpuPmpp := 1.0;    // full on
        Vreg := 1.0;
        Fkvarlimit := FkVArating;
        P_Priority := FALSE;    // This is a change from older versions
    end;

    FpctCutIn := 20.0;
    FpctCutOut := 20.0;

    // Output rating stuff
    kW_out := 500.0;
    kvar_out := 0.0;
    PFnominal := 1.0;

    pctR := 50.0;
    pctX := 0.0;

    PublicDataStruct := @PVSystemVars;
    PublicDataSize := SizeOf(TPVSystemVars);

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
    FVWMode := FALSE;
    FVWYAxis := 1;
    RecalcElementData;
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

procedure TPVsystemObj.RecalcElementData;
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

    end;

    SetNominalPVSystemOuput;

    // Initialize to Zero - defaults to PQ PVSystem element
    // Solution object will reset after circuit modifications

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

    // Update any user-written models
    if Usermodel.Exists then
        UserModel.FUpdateModel;
end;

procedure TPVsystemObj.SetNominalPVSystemOuput;
begin
    ShapeFactor := CDOUBLEONE;  // init here; changed by curve routine
    TShapeValue := PVSystemVars.FTemperature; // init here; changed by curve routine

    // Check to make sure the PVSystem element is ON
    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        if not (IsDynamicModel or IsHarmonicModel)      // Leave PVSystem element in whatever state it was prior to entering Dynamic mode
        then
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
                    // TSolveMode.DYNAMICMODE:   ; // do nothing yet
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
        if IsDynamicModel or IsHarmonicModel then
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
        end
        else
        begin  //  Regular power flow PVSystem element model

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
        end;  // ELSE IF Solution.mode
end;

procedure TPVsystemObj.ComputeInverterPower;
var
    kVA_Gen: Double;
begin
    with PVSystemVars do
    begin
        EffFactor := 1.0;
        kW_Out := 0.0;

        // Determine state of the inverter
        if FInverterON then
        begin
            if Panelkw < CutOutkW then
            begin
                FInverterON := FALSE;
            end;
        end
        else
        begin
            if Panelkw >= CutInkW then
            begin
                FInverterON := TRUE;
            end;
        end;

        // set inverter output. Defaults to 100% of the panelkW if no efficiency curve spec'd
        if FInverterON then
        begin
            if Assigned(InverterCurveObj) then
                EffFactor := InverterCurveObj.GetYValue(PanelkW / FkVArating);  // pu eff vs pu power
            kWOut_Calc; // if VOLTWATT control mode is enabled then smaller of:
                      //    (1) panelKW* EffFactor
                      //    (2) puPmpp, Pmpp (% of full-scale kW)
                      // if VOLTWATT control mode is not enabled then go with
                      // panelKW* EffFactor*puPmpp (puPmpp can be set locally in the
                      // PVSystem object, but defaults to 100% or 1.0 per-unit.
        end
        else
        begin
            kW_Out := 0.0;
        end;

      // kvar value
        if PFSpecified then
        begin
            if PFnominal = 1.0 then
                kvar_out := 0.0
            else
            begin
                kvar_out := kW_out * sqrt(1.0 / SQR(PFnominal) - 1.0) * sign(PFnominal);
                if abs(kvar_out) > Fkvarlimit then
                begin
                    kvar_out := Fkvarlimit * sign(PFnominal);
                    PFnominal := sign(PFnominal) * kW_out / sqrt((kvar_out * kvar_out) + (kW_out * kW_out));
                end;


            end;
           // if pf is negative, make sure kvar has opposite sign of kW
           // kW will always be positive
        end
        else     // kvar is specified
        begin
            if abs(kvarRequested) > Fkvarlimit then
                kvar_Out := Fkvarlimit * sign(kvarRequested)
            else
                kvar_Out := kvarRequested;
        end;
        if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then
            kvar_out := 0.0;


      // Limit kvar and kW so that kVA of inverter is not exceeded
        kVA_Gen := Sqrt(Sqr(kW_out) + Sqr(kvar_out));
        if kVA_Gen > FkVArating then
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
            begin  // Q Priority   (Default) back off the kW
                if abs(kvar_out) > Fkvarlimit then
                begin
            // first, back off the kvar to the kvar limit if necessary
                    kvar_out := Fkvarlimit * sign(kvar_out);
                end;

            // Back the kvar down to the kVA rating if necessary
                if abs(kvar_out) > FkVArating then
                begin
                    kvar_Out := FkVArating * sign(kvar_out);
                    kW_out := 0.0;
                end
              // Now, set the kW so kVA output is within rating
                else
                    kW_Out := Sqrt(SQR(FkVArating) - SQR(kvar_Out)) * sign(kW_Out);

            end;
        end;
        if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then
            kvar_out := 0.0;

    end;  // With PVSystemVars
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

procedure TPVsystemObj.CalcYPrim;
var
    i: Integer;
begin
    // Build only shunt Yprim
    // Build a dummy Yprim Series so that CalcV Does not fail
    if YPrimInvalid then
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

    SetNominalPVSystemOuput;
    CalcYPrimMatrix(YPrim_Shunt);

     // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages Doesn't fail
    for i := 1 to Yorder do
        Yprim_Series.SetElement(i, i, Yprim_Shunt.Getelement(i, i) * 1.0e-10);

    YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
    inherited CalcYPrim;
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
        if (not DSS.InShowResults) then
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

            FSWriteln(TRacefile);
            FSFlush(TraceFile);
        end;
    except
        On E: Exception do
        begin
        end;

    end;
end;

procedure TPVsystemObj.DoConstantPQPVsystemObj;
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

    CalcVTerminalPhase; // get actual voltage across each phase of the load

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
                    if Cabs(PhaseCurr) > PVSystemvars.MaxDynPhaseCurrent then
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
                IterminalUpdated := TRUE;
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
                    if Cabs(DeltaCurr) * SQRT3 > PVSystemvars.MaxDynPhaseCurrent then
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
                IterminalUpdated := TRUE;
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
    CalcVTerminalPhase; // get actual voltage across each phase of the load

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
        YEQ2 := YEQ / 3.0; // YEQ for delta connection

    for i := 1 to Fnphases do
    begin
        Curr := YEQ2 * Vterminal^[i];
        StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection

    end;
end;

procedure TPVsystemObj.DoUserModel;
// Compute total terminal Current from User-written model
var
    i: Integer;
begin
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array

    if UserModel.Exists     // Check automatically selects the usermodel If true
    then
    begin
        UserModel.FCalc(Vterminal, Iterminal);
        IterminalUpdated := TRUE;
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
    i: Integer;
    V012,
    I012: array[0..2] of Complex;
    Vthev: Complex;
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

            Vthev := pclx(VthevMagDyn, Theta);
            LastThevAngle := Theta;     // remember this for angle persistence
        end;
    end;

begin
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array  and computes VTerminal

    // Inj = -Itotal (in) - Yprim*Vtemp

    case VoltageModel of

        3:
            if UserModel.Exists then       // auto selects model
            begin   // We have total currents in Iterminal
                UserModel.FCalc(Vterminal, Iterminal);  // returns terminal currents in Iterminal
            end
            else
            begin
                DoSimpleMsg('Dynamics model missing for %s ', [FullName], 5671);
                DSS.SolutionAbort := TRUE;
            end;
    else  // All other models -- current-limited like Generator Model 7

        // This is a simple model that is basically a thevinen equivalent without inertia
        case Fnphases of  // No user model, use default Thevinen equivalent for standard Generator model
            1:
                with PVSystemVars do
                begin
                   // 1-phase generators have 2 conductors
                      // Assume inverter stays in phase with terminal voltage
                    CalcVthev_Dyn(VTerminal^[1] - VTerminal^[2]);  // see internal proc above


                    ITerminal^[1] := (VTerminal^[1] - Vthev - VTerminal^[2]) / Zthev;

                    if CurrentLimited then
                        if Cabs(Iterminal^[1]) > MaxDynPhaseCurrent then   // Limit the current but keep phase angle
                            ITerminal^[1] := ptocomplex(topolar(MaxDynPhaseCurrent, cang(Iterminal^[1])));

                    ITerminal^[2] := -ITerminal^[1];
                end;

            3:
                with PVSystemVars do
                begin
                    Phase2SymComp(Vterminal, pComplexArray(@V012));  // convert Vabc to V012

                    begin  // simple inverter model
                        // Positive Sequence Contribution to Iterminal
                        // Assume inverter stays in phase with pos seq voltage
                        CalcVthev_Dyn(V012[1]);

                        // Positive Sequence Contribution to Iterminal
                        I012[1] := (V012[1] - Vthev) / Zthev;

                        if CurrentLimited and (Cabs(I012[1]) > MaxDynPhaseCurrent) then   // Limit the pos seq current but keep phase angle
                            I012[1] := ptocomplex(topolar(MaxDynPhaseCurrent, cang(I012[1])));

                        if ForceBalanced then
                        begin
                            I012[2] := CZERO;
                        end
                        else
                            I012[2] := V012[2] / Zthev;  // for inverter

                    end;

                    // Adjust for generator connection
                    if (Connection = 1) or ForceBalanced then
                        I012[0] := CZERO
                    else
                        I012[0] := V012[0] / Zthev;

                    SymComp2Phase(ITerminal, pComplexArray(@I012));  // Convert back to phase components

                      // Neutral current
                    if Connection = 0 then
                        ITerminal^[FnConds] := -I012[0] * 3.0;
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
end;

procedure TPVsystemObj.DoHarmonicMode;
// Compute Injection Current Only when in harmonics mode

// Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built
// Vd is the fundamental frequency voltage behind Xd" for phase 1
var
    i: Integer;
    E: Complex;
    PVSystemHarmonic: Double;
    pBuffer: PCBuffer24;
begin
    pBuffer := @TPVSystem(ParentClass).cBuffer;

    ComputeVterminal;

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

procedure TPVsystemObj.CalcVTerminalPhase;
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

procedure TPVsystemObj.CalcPVSystemModelContribution;
// Calculates PVSystem element current and adds it properly into the injcurrent array
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
            case VoltageModel of
                1:
                    DoConstantPQPVsystemObj;
                2:
                    DoConstantZPVsystemObj;
                3:
                    DoUserModel;
            else
                DoConstantPQPVsystemObj;  // for now, until we implement the other models.
            end;
        end;
    end;
    // When this is Done, ITerminal is up to date
end;

procedure TPVsystemObj.CalcInjCurrentArray;
// Difference between currents in YPrim and total current
begin
      // Now Get Injection Currents
    if PVsystemObjSwitchOpen then
        ZeroInjCurrent
    else
        CalcPVSystemModelContribution;
end;

procedure TPVsystemObj.GetTerminalCurrents(Curr: pComplexArray);
// Compute total Currents
begin
    with ActiveCircuit.Solution do
    begin
        if IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount then
        begin     // recalc the contribution
            if not PVsystemObjSwitchOpen then
                CalcPVSystemModelContribution;  // Adds totals in Iterminal as a side effect
        end;
        inherited GetTerminalCurrents(Curr);
    end;

    if (DebugTrace) then
        WriteTraceRecord('TotalCurrent');
end;

function TPVsystemObj.InjCurrents: Integer;
begin
    with ActiveCircuit.Solution do
    begin
        if LoadsNeedUpdating then
            SetNominalPVSystemOuput; // Set the nominal kW, etc for the type of solution being Done

        CalcInjCurrentArray;          // Difference between currents in YPrim and total terminal current

        if (DebugTrace) then
            WriteTraceRecord('Injection');

         // Add into System Injection Current Array

        Result := inherited InjCurrents;
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

procedure TPVsystemObj.TakeSample;
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

function TPVsystemObj.Get_PresentkV: Double;
begin
    Result := PVSystemVars.kVPVSystemBase;
end;

function TPVsystemObj.Get_Presentkvar: Double;
begin
    Result := Qnominalperphase * 0.001 * Fnphases;
end;

function TPVsystemObj.Get_VarFollowInverter: Boolean;
begin
    if FVarFollowInverter then
        Result := TRUE
    else
        Result := FALSE;
end;

procedure TPVsystemObj.InitHarmonics;
// This routine makes a thevenin equivalent behis the reactance spec'd in %R and %X
var
    E, Va: complex;
begin
    YPrimInvalid := TRUE;  // Force rebuild of YPrims
    PVSystemFundamental := ActiveCircuit.Solution.Frequency;  // Whatever the frequency is when we enter here.

    // Compute reference Thevinen voltage from phase 1 current

    ComputeIterminal;  // Get present value of current

    with ActiveCircuit.solution do
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

    with PVSystemVars do
    begin
        YEQ := Cinv(Cmplx(RThev, XThev));           // used for current calcs  Always L-N
        E := Va - Iterminal^[1] * cmplx(Rthev, Xthev);
        Vthevharm := Cabs(E);   // establish base mag and angle
        ThetaHarm := Cang(E);
    end;
end;

procedure TPVsystemObj.InitStateVars;
// for going into dynamics mode
var
//    VNeut,
    Edp: Complex;
    V12: Complex;
    i: Integer;
    V012,
    I012: array[0..2] of Complex;
    Vabc: array[1..3] of Complex;

begin
    YPrimInvalid := TRUE;  // Force rebuild of YPrims

    with PVSystemVars do
    begin
        NumPhases := Fnphases;     // set Publicdata vars
        NumConductors := Fnconds;
        Conn := Connection;

        Zthev := Cmplx(RThev, XThev);
        YEQ := Cinv(Zthev);      // used for current calcs  Always L-N

        ComputeIterminal;

        with ActiveCircuit.Solution do
            case Fnphases of

                1:
                begin
                    V12 := NodeV^[NodeRef^[1]] - NodeV^[NodeRef^[2]];
                    InitialVAngle := Cang(V12);
                    Edp := V12 - ITerminal^[1] * Zthev;
                    VthevmagDyn := Cabs(Edp);
                    ThetaDyn := Cang(Edp); // initial thev equivalent phase angle
                end;

                3:
                begin
                 // Calculate Edp based on Pos Seq only
                    Phase2SymComp(ITerminal, pComplexArray(@I012));
                     // Voltage behind Xdp  (transient reactance), volts

                    for i := 1 to FNphases do
                        Vabc[i] := NodeV^[NodeRef^[i]];   // Wye Voltage
                    Phase2SymComp(pComplexArray(@Vabc), pComplexArray(@V012));
                    InitialVAngle := Cang(V012[1]);
                    Edp := V012[1] - I012[1] * Zthev;    // Pos sequence
                    VthevmagDyn := Cabs(Edp);
                    ThetaDyn := Cang(Edp); // initial thev equivalent phase angle
                end;
            else
                DoSimpleMsg('Dynamics mode is implemented only for 1- or 3-phase Generators. %s has %d phases.', [FullName, Fnphases], 5673);
                DSS.SolutionAbort := TRUE;
            end;

        LastThevAngle := ThetaDyn;

    end;
end;

procedure TPVsystemObj.IntegrateStates;
// dynamics mode integration routine
begin
   // Compute Derivatives and Then integrate

    ComputeIterminal;

    if Usermodel.Exists then   // Checks for existence and Selects

        Usermodel.Integrate

    else
        with ActiveCircuit.Solution do
        begin
        end;
end;


function TPVsystemObj.Get_Variable(i: Integer): Double;
// Return variables one at a time
var
    N, k: Integer;
begin
    Result := -9999.99;  // error return value; no state fars
    if i < 1 then
        Exit;
    // for now, report kWhstored and mode
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
        else
        begin
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
end;

function TPVsystemObj.Get_InverterON: Boolean;
begin
    if FInverterON then
        Result := TRUE
    else
        Result := FALSE;
end;

function TPVsystemObj.Get_Varmode: Integer;
begin
    if PFSpecified then
        Result := 0
    else
        Result := 1;    // 1 for kvar specified
end;

function TPVsystemObj.Get_VWmode: Boolean;

begin
    if FVWmode then
        Result := TRUE
    else
        Result := FALSE;    // TRUE if volt-watt mode
    //  engaged from InvControl (not ExpControl)
end;

function TPVsystemObj.Get_VWYAxis: Integer;
begin
    Result := FVWYAxis;
    //  engaged from InvControl (not ExpControl)
end;

procedure TPVsystemObj.kWOut_Calc;
var
    Peff, Pmpp, PTemp: Double;

    // --------Local Proc-----------------------
    procedure Calc_kWOut;
    begin
        with PVSystemVars do
        begin
            Peff := PanelkW * EffFactor;
            Pmpp := FPmpp * FpuPmpp;

            if (Peff > Pmpp) then
                kW_Out := Pmpp
            else
                kW_Out := Peff;
        end;
    end;
begin
    if VWmode then
        case FVWYAxis of
            0:
                with PVSystemVars do
                    kW_Out := PanelkW * EffFactor * FpuPmpp;
            1:
                Calc_kWOut;   // call local procedure
            2:
                with PVSystemVars do
                begin
                    PTemp := kWRequested;
                    Peff := PanelkW * EffFactor;
                    if (Peff > PTemp) then
                        kW_Out := PTemp
                    else
                        kW_Out := Peff;
                end;
        end
    else
        Calc_kWOut;
end;

procedure TPVsystemObj.Set_Variable(i: Integer; Value: Double);
var
    N, k: Integer;
begin
    if i < 1 then
        Exit;  // No variables to set
    with PVSystemVars do
        case i of
            1:
                FIrradiance := Value;
            2: ; // Setting this has no effect Read only
            3: ; // Setting this has no effect Read only
            4: ; // Setting this has no effect Read only
            5:
                Vreg := Value; // the InvControl or ExpControl will do this
        else
        begin
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
end;

procedure TPVsystemObj.Set_Varmode(const Value: Integer);
begin
    case Value of
        VARMODEKVAR:
            PFSpecified := FALSE;
    else
        PFSpecified := TRUE
    end;

    kvarSpecified := not PFSpecified;
end;

procedure TPVsystemObj.Set_VWmode(const Value: Boolean);
begin
    FVWmode := Value;
end;

procedure TPVsystemObj.Set_VWYAxis(const Value: Integer);
begin
    FVWYAxis := Value;
end;

procedure TPVsystemObj.GetAllVariables(States: pDoubleArray);
var
    i: Integer;
begin
    for i := 1 to NumPVSystemVariables do
        States^[i] := Variable[i];

    if UserModel.Exists then
        UserModel.FGetAllVars(pDoubleArray(@States^[NumPVSystemVariables + 1]));
end;

function TPVsystemObj.NumVariables: Integer;
begin
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
    else
    begin
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
            SetDouble(ord(TProp.KVA), newkVA);
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

    if Value then
        PVsystemObjSwitchOpen := FALSE
    else
        PVsystemObjSwitchOpen := TRUE;
end;

procedure TPVsystemObj.Set_Maxkvar(const Value: Double);
begin
    PVSystemVars.Fkvarlimit := Value;
    SetAsNextSeq(ord(TProp.kvarLimit));
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
    PFSpecified := TRUE;
end;

procedure TPVsystemObj.Set_PresentIrradiance(const Value: Double);
begin
    PVSystemVars.FIrradiance := Value;
    SetAsNextSeq(ord(TProp.kVA));
end;

procedure TPVsystemObj.Set_PresentkV(const Value: Double);
begin
    with PVSystemVars do
    begin
        kVPVSystemBase := Value;
        case FNphases of
            2, 3:
                VBase := kVPVSystemBase * InvSQRT3x1000;
        else
            VBase := kVPVSystemBase * 1000.0;
        end;
    end;
end;

procedure TPVsystemObj.Set_VarFollowInverter(const Value: Boolean);
begin
    FVarFollowInverter := Value;
end;

procedure TPVsystemObj.Set_InverterON(const Value: Boolean);
begin
    FInverterON := Value;
end;
procedure TPVsystemObj.Set_PresentkW(const Value: Double);
begin
    kWRequested := Value;
end;

procedure TPVsystemObj.Set_Presentkvar(const Value: Double);
begin
    kvarRequested := Value;
end;

procedure TPVsystemObj.Set_puPmpp(const Value: Double);
begin
    PVSystemVars.FpuPmpp := Value;
end;

procedure TPVsystemObj.SetDragHandRegister(Reg: Integer; const Value: Double);
begin
    if (Value > Registers[reg]) then
        Registers[Reg] := Value;
end;

initialization
    PropInfo := NIL;
end.
