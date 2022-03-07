unit Storage2;

{
  ----------------------------------------------------------
  Copyright (c) 2009-2016, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

// To Do:
//    Make connection to User model
//    Yprim for various modes
//    Define state vars and dynamics mode behavior
//    Complete Harmonics mode algorithm (generator mode is implemented)

//  The Storage element is essentially a generator that can be dispatched
//  to either produce power or consume power commensurate with rating and
//  amount of stored energy.
//
//  The Storage element can also produce or absorb vars within the kVA rating of the inverter.
//  That is, a StorageController object requests kvar and the Storage element provides them if
//  it has any capacity left. The Storage element can produce/absorb kvar while idling.

//  The Storage element is assumed balanced over the no. of phases defined

interface

uses
    Classes,
    Storage2Vars,
    StoreUserModel,
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    UComplex, DSSUcomplex,
    LoadShape,
    Spectrum,
    ArrayDef,
    Dynamics,
    XYCurve;

const
    NumStorage2Registers = 6;    // Number of energy meter registers
    NumStorage2Variables = 25;    // No state variables
    VARMODEPF = 0;
    VARMODEKVAR = 1;
//= = = = = = = = = = = = = = DEFINE STATES = = = = = = = = = = = = = = = = = = = = = = = = =

    STORE_CHARGING = -1;
    STORE_IDLING = 0;
    STORE_DISCHARGING = 1;
//= = = = = = = = = = = = = = DEFINE DISPATCH MODES = = = = = = = = = = = = = = = = = = = = = = = = =

    STORE_DEFAULT = 0;
    STORE_LOADMODE = 1;
    STORE_PRICEMODE = 2;
    STORE_EXTERNALMODE = 3;
    STORE_FOLLOW = 4;

type
{$SCOPEDENUMS ON}
    TStorage2Prop = (
        INVALID = 0,
        phases = 1,
        bus1 = 2,
        kv = 3, // propKV
        conn = 4, // propCONNECTION
        kW = 5, // propKW
        kvar = 6, // propKVAR
        pf = 7, // propPF
        kVA = 8, // propKVA
        pctCutin = 9, // propCutin
        pctCutout = 10, // propCutout
        EffCurve = 11, // propInvEffCurve
        VarFollowInverter = 12, // propVarFollowInverter
        kvarMax = 13, // propkvarLimit
        kvarMaxAbs = 14, // propkvarLimitneg
        WattPriority = 15, // propPpriority
        PFPriority = 16, // propPFPriority
        pctPminNoVars = 17, // propPminNoVars
        pctPminkvarMax = 18, // propPminkvarLimit
        kWrated = 19, // propKWRATED
        pctkWrated = 20, // proppctkWrated
        kWhrated = 21, // propKWHRATED
        kWhstored = 22, // propKWHSTORED
        pctstored = 23, // propPCTSTORED
        pctreserve = 24, // propPCTRESERVE
        State = 25, // propSTATE
        pctDischarge = 26, // propPCTKWOUT
        pctCharge = 27, // propPCTKWIN
        pctEffCharge = 28, // propCHARGEEFF
        pctEffDischarge = 29, // propDISCHARGEEFF
        pctIdlingkW = 30, // propIDLEKW
        
        // pctIdlingkvar = 31, // propIDLEKVAR --- was deprecated, removed
        
        pctR, // propPCTR
        pctX, // propPCTX
        model, // propMODEL
        Vminpu, // propVMINPU
        Vmaxpu, // propVMAXPU
        Balanced, // propBalanced
        LimitCurrent, // propLimited
        yearly, // propYEARLY
        daily, // propDAILY
        duty, // propDUTY
        DispMode, // propDISPMODE
        DischargeTrigger, // propDISPOUTTRIG
        ChargeTrigger, // propDISPINTRIG
        TimeChargeTrig, // propCHARGETIME
        cls, // propCLASS
        DynaDLL, // propDynaDLL
        DynaData, // propDynaData
        UserModel, // propUSERMODEL
        UserData, // propUSERDATA
        debugtrace = 50 // propDEBUGTRACE
    );
{$SCOPEDENUMS OFF}

    TStorage2 = class(TPCClass)
    PROTECTED
        cBuffer: TCBuffer24;  // Temp buffer for calcs  24-phase Storage element?

        procedure DefineProperties; override;
    PUBLIC
        RegisterNames: array[1..NumStorage2Registers] of String;

        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;

        procedure ResetRegistersAll;
        procedure SampleAll();
        procedure UpdateAll();

    end;

    TStorage2Obj = class(TPCElement)
    PRIVATE
        Yeq: Complex;   // at nominal
        Yeq95: Complex;   // at 95%
        Yeq105: Complex;   // at 105%
        PIdling: Double;
        YeqDischarge: Complex;   // equiv at rated power of Storage element only
        PhaseCurrentLimit: Complex;
        MaxDynPhaseCurrent: Double;

        DebugTrace: LongBool;
        FState: Integer;
        FStateChanged: Boolean;
        FirstSampleAfterReset: Boolean;
        StorageSolutionCount: Integer;
        StorageFundamental: Double; // Thevenin equivalent voltage mag and angle reference for Harmonic model
        Storage2ObjSwitchOpen: Boolean;


        ForceBalanced: LongBool;
        CurrentLimited: LongBool;

        kvar_out: Double;
        kW_out: Double;
        FDCkW: Double;
        pf_wp_nominal: Double;

        // Variables for Inverter functionalities
        FpctCutIn: Double;
        FpctCutOut: Double;
        CutInkW: Double;
        CutOutkW: Double;

        FpctPminNoVars: Double;
        FpctPminkvarLimit: Double;
        PminNoVars: Double;
        PminkvarLimit: Double;
        kVA_exceeded: Boolean;


        kvarLimitSet: Boolean;
        kvarLimitNegSet: Boolean;
        kVASet: Boolean;

        pctR: Double;
        pctX: Double;

        OpenStorageSolutionCount: Integer;
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

        Tracefile: TFileStream;
        IsUserModel: Boolean;
        UserModel: TStoreUserModel;   // User-Written Models
        DynaModel: TStoreDynaModel;

        UserModelNameStr, UserModelEditStr: String;
        DynaModelNameStr, DynaModelEditStr: String;

//        VBase           :Double;  // Base volts suitable for computing currents  made public
        VBase105: Double;
        VBase95: Double;
        YPrimOpenCond: TCmatrix;

        procedure CalcDailyMult(Hr: Double);
        procedure CalcDutyMult(Hr: Double);
        procedure CalcYearlyMult(Hr: Double);

        procedure ComputePresentkW; // Included
        procedure ComputeInverterPower; // Included

        procedure ComputekWkvar;        // Included
        procedure ComputeDCkW; // For Storage Update
        procedure CalcStorageModelContribution();
        procedure CalcInjCurrentArray();
        // PROCEDURE CalcVterminal;
        procedure CalcVTerminalPhase();

        procedure CalcYPrimMatrix(Ymatrix: TcMatrix);

        procedure DoConstantPQStorage2Obj();
        procedure DoConstantZStorage2Obj();
        procedure DoDynamicMode();
        procedure DoHarmonicMode();
        procedure DoUserModel();
        procedure DoDynaModel();

        procedure Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);
        procedure SetDragHandRegister(Reg: Integer; const Value: Double);
        procedure StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);

        procedure WriteTraceRecord(const s: String);

        procedure CheckStateTriggerLevel(Level: Double);
        procedure UpdateStorage();    // Update Storage elements based on present kW and IntervalHrs variable
        function NormalizeToTOD(h: Integer; sec: Double): Double;

        function Get_PresentkW: Double;
        function Get_Presentkvar: Double;
        function Get_PresentkV: Double;

        procedure Set_kW(const Value: Double);
        function Get_kW: Double;

        procedure Set_PowerFactor(const Value: Double);

        procedure Set_StorageState(const Value: Integer);

        function Get_DCkW: Double;
        function Get_kWTotalLosses: Double;
        function Get_InverterLosses: Double;
        function Get_kWIdlingLosses: Double;
        function Get_kWChDchLosses: Double;
        procedure Update_EfficiencyFactor;

        function Get_kWDesired: Double;

        // Procedures and functions for inverter functionalities
        procedure Set_kVARating(const Value: Double);

        procedure kWOut_Calc;

    PROTECTED
        procedure GetTerminalCurrents(Curr: pComplexArray); OVERRIDE;

    PUBLIC
        StateDesired: Integer;  // Stores desired state (before any change due to kWh limits or %CutIn/%CutOut

        StorageVars: TStorage2Vars;

        VBase: Double;  // Base volts suitable for computing currents
        Vmaxpu: Double;
        Vminpu: Double;

        Connection: Integer;  // 0 = line-neutral; 1=Delta
        DailyShapeObj: TLoadShapeObj;  // Daily Storage element Shape for this load
        DutyShapeObj: TLoadShapeObj;  // Shape for this Storage element
        YearlyShapeObj: TLoadShapeObj;  // Shape for this Storage element

        pctkWOut: Double;   // percent of kW rated output currently dispatched
        pctkWIn: Double;

        pctReserve: Double;
        DispatchMode: Integer;
        pctIdlekW: Double;
        kvarRequested: Double;
        kWRequested: Double;

        kWOutIdling: Double;

        pctIdlekvar: Double;
        pctChargeEff: Double;
        pctDischargeEff: Double;
        DischargeTrigger: Double;
        ChargeTrigger: Double;
        ChargeTime: Double;
        kWhBeforeUpdate: Double;
        CurrentkvarLimit: Double;
        CurrentkvarLimitNeg: Double;

        CutOutkWAC: Double;  // CutInkW  reflected to the AC side of the inverter
        CutInkWAC: Double;   // CutOutkW reflected to the AC side of the inverter

        // Inverter efficiency curve
        InverterCurveObj: TXYCurveObj;

        FVWStateRequested: Boolean;   // TEST Flag indicating if VW function has requested a specific state in last control iteration

        StorageClass: Integer;
        VoltageModel: Integer;   // Variation with voltage
        PFNominal: Double;

        Registers, Derivatives: array[1..NumStorage2Registers] of Double;

        // Variables for InvControl's Volt-Watt function
        VWMode: Boolean; //boolean indicating if under volt-watt control mode from InvControl (not ExpControl)
        VVMode: Boolean; //boolean indicating if under volt-var mode from InvControl
        DRCMode: Boolean; //boolean indicating if under DRC mode from InvControl
        WPMode: Boolean; //boolean indicating if under watt-pf mode from InvControl
        WVMode: Boolean; //boolean indicating if under watt-var mode from InvControl

        InverterON: Boolean;
        varMode: Integer;
        VarFollowInverter: LongBool;

        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure Set_ConductorClosed(Index: Integer; Value: Boolean); OVERRIDE;
        procedure RecalcElementData(); OVERRIDE;
        procedure CalcYPrim(); OVERRIDE;

        function InjCurrents(): Integer; OVERRIDE;
        function NumVariables: Integer; OVERRIDE;
        procedure GetAllVariables(States: pDoubleArray); OVERRIDE;
        function Get_Variable(i: Integer): Double; OVERRIDE;
        procedure Set_Variable(i: Integer; Value: Double); OVERRIDE;
        function VariableName(i: Integer): String; OVERRIDE;

        procedure Set_Maxkvar(const Value: Double);
        procedure Set_Maxkvarneg(const Value: Double);

        procedure SetNominalStorageOutput();
        procedure Randomize(Opt: Integer);   // 0 = reset to 1.0; 1 = Gaussian around mean and std Dev  ;  // 2 = uniform

        procedure ResetRegisters;
        procedure TakeSample();

        // Support for Dynamics Mode
        procedure InitStateVars(); OVERRIDE;
        procedure IntegrateStates(); OVERRIDE;

        // Support for Harmonics Mode
        procedure InitHarmonics(); OVERRIDE;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model

        property kW: Double READ Get_kW WRITE Set_kW;
        property kWDesired: Double READ Get_kWDesired;

        property PresentkW: Double READ Get_PresentkW;             // Present kW   at inverter output
        property Presentkvar: Double READ Get_Presentkvar;           // Present kvar at inverter output

        property PresentkV: Double READ Get_PresentkV;
        property PowerFactor: Double READ PFNominal WRITE Set_PowerFactor;
        property kVARating: Double READ StorageVars.FkVARating WRITE Set_kVARating;
       
        property kvarLimit: Double READ StorageVars.Fkvarlimit WRITE Set_Maxkvar;
        property kvarLimitneg: Double READ StorageVars.Fkvarlimitneg WRITE Set_Maxkvarneg;

        property StorageState: Integer READ FState WRITE Set_StorageState;

        property kWTotalLosses: Double READ Get_kWTotalLosses;
        property kWIdlingLosses: Double READ Get_kWIdlingLosses;
        property kWInverterLosses: Double READ Get_InverterLosses;
        property kWChDchLosses: Double READ Get_kWChDchLosses;
        property DCkW: Double READ Get_DCkW;

        property MinModelVoltagePU: Double READ VminPu;
    end;

implementation

uses
    BufStream,
    Circuit,
    Sysutils,
    Command,
    Math,
    MathUtil,
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    DSSHelper,
    DSSObjectHelper;

type
    TObj = TStorage2Obj;
    TProp = TStorage2Prop;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    
    StateEnum, DispatchModeEnum: TDSSEnum;

constructor TStorage2.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        StateEnum := TDSSEnum.Create('Storage: State', True, 1, 1, 
            ['Charging', 'Idling', 'Discharging'], 
            [-1, 0, 1]);
        StateEnum.DefaultValue := 0;
        DispatchModeEnum := TDSSEnum.Create('Storage: Dispatch Mode', True, 1, 1, 
            ['Default', 'LoadLevel', 'Price', 'External', 'Follow'], 
            [0, 1, 2, 3, 4]);
        DispatchModeEnum.DefaultValue := 0;
    end;

    inherited Create(dssContext, Storage_ELEMENT, 'Storage');

     // Set Register names
    RegisterNames[1] := 'kWh';
    RegisterNames[2] := 'kvarh';
    RegisterNames[3] := 'Max kW';
    RegisterNames[4] := 'Max kVA';
    RegisterNames[5] := 'Hours';
    RegisterNames[6] := 'Price($)';
end;

destructor TStorage2.Destroy;
begin
    inherited Destroy;
end;

procedure SetkW(obj: TObj; Value: Double);
begin
    obj.kW := Value;
end;

function Getkvar(obj: TObj): Double;
begin
    Result := obj.kvar_out;
end;

procedure SetPctStored(obj: TObj; Value: Double);
begin
    obj.StorageVars.kWhStored := Value * 0.01 * obj.StorageVars.kWhRating;
end;

function GetPctStored(obj: TObj): Double;
begin
    Result := obj.StorageVars.kWhStored / obj.StorageVars.kWhRating * 100.0;
end;

procedure TStorage2.DefineProperties;
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

    PropertyType[ord(TProp.DynaDLL)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.DynaDLL)] := ptruint(@obj.DynaModelNameStr);
    PropertyType[ord(TProp.DynaData)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.DynaData)] := ptruint(@obj.DynaModelEditStr);

    // enum properties
    PropertyType[ord(TProp.conn)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.conn)] := ptruint(@obj.Connection);
    PropertyOffset2[ord(TProp.conn)] := PtrInt(DSS.ConnectionEnum);

    // enum properties
    PropertyType[ord(TProp.State)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.State)] := ptruint(@obj.FState);
    PropertyOffset2[ord(TProp.State)] := PtrInt(StateEnum);

    PropertyType[ord(TProp.DispMode)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.DispMode)] := ptruint(@obj.DispatchMode);
    PropertyOffset2[ord(TProp.DispMode)] := PtrInt(DispatchModeEnum);

    // bus properties
    PropertyType[ord(TProp.bus1)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus1)] := 1;

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
    PropertyOffset[ord(TProp.WattPriority)] := ptruint(@obj.StorageVars.P_priority);
    PropertyOffset[ord(TProp.PFPriority)] := ptruint(@obj.StorageVars.PF_priority);

    // integer properties
    PropertyOffset[ord(TProp.cls)] := ptruint(@obj.StorageClass);
    PropertyOffset[ord(TProp.model)] := ptruint(@obj.VoltageModel);
    PropertyType[ord(TProp.cls)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.model)] := TPropertyType.IntegerProperty;

    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    // object properties
    PropertyType[ord(TProp.yearly)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.daily)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.duty)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.yearly)] := ptruint(@obj.YearlyShapeObj);
    PropertyOffset[ord(TProp.daily)] := ptruint(@obj.DailyShapeObj);
    PropertyOffset[ord(TProp.duty)] := ptruint(@obj.DutyShapeObj);
    PropertyOffset2[ord(TProp.yearly)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.daily)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.duty)] := ptruint(DSS.LoadShapeClass);

    PropertyType[ord(TProp.EffCurve)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.EffCurve)] := ptruint(@obj.InverterCurveObj);
    PropertyOffset2[ord(TProp.EffCurve)] := ptruint(DSS.XYCurveClass);

    PropertyScale[ord(TProp.pctkWrated)] := 0.01;
    PropertyOffset[ord(TProp.pctkWrated)] := ptruint(@obj.StorageVars.pctkWrated);

    // adv doubles
    PropertyOffset[ord(TProp.kvarMax)] := ptruint(@obj.StorageVars.Fkvarlimit);
    PropertyOffset[ord(TProp.kvarMaxAbs)] := ptruint(@obj.StorageVars.Fkvarlimitneg);
    PropertyFlags[ord(TProp.kvarMax)] := [TPropertyFlag.Transform_Abs];
    PropertyFlags[ord(TProp.kvarMaxAbs)] := [TPropertyFlag.Transform_Abs];

    // double properties (default type)
    PropertyOffset[ord(TProp.pctR)] := ptruint(@obj.pctR);
    PropertyOffset[ord(TProp.pctX)] := ptruint(@obj.pctX);
    PropertyOffset[ord(TProp.pctIdlingkW)] := ptruint(@obj.pctIdlekW);
    PropertyOffset[ord(TProp.DischargeTrigger)] := ptruint(@obj.DischargeTrigger);
    PropertyOffset[ord(TProp.ChargeTrigger)] := ptruint(@obj.ChargeTrigger);
    PropertyOffset[ord(TProp.pctEffCharge)] := ptruint(@obj.pctChargeEff);
    PropertyOffset[ord(TProp.pctEffDischarge)] := ptruint(@obj.pctDischargeEff);
    PropertyOffset[ord(TProp.pctDischarge)] := ptruint(@obj.pctkWout);
    PropertyOffset[ord(TProp.pctCharge)] := ptruint(@obj.pctkWIn);
    PropertyOffset[ord(TProp.pctCutin)] := ptruint(@obj.FpctCutIn);
    PropertyOffset[ord(TProp.pctCutout)] := ptruint(@obj.FpctCutOut);
    PropertyOffset[ord(TProp.Vminpu)] := ptruint(@obj.VMinPu);
    PropertyOffset[ord(TProp.Vmaxpu)] := ptruint(@obj.VMaxPu);
    PropertyOffset[ord(TProp.kWrated)] := ptruint(@obj.StorageVars.kWrating);
    PropertyOffset[ord(TProp.kWhrated)] := ptruint(@obj.StorageVars.kWhrating);
    PropertyOffset[ord(TProp.kWhstored)] := ptruint(@obj.StorageVars.kWhstored);
    PropertyOffset[ord(TProp.pctreserve)] := ptruint(@obj.pctReserve);
    PropertyOffset[ord(TProp.pctPminNoVars)] := ptruint(@obj.FpctPminNoVars);
    PropertyOffset[ord(TProp.pctPminkvarMax)] := ptruint(@obj.FpctPminkvarLimit);
    PropertyOffset[ord(TProp.TimeChargeTrig)] := ptruint(@obj.ChargeTime);
    PropertyOffset[ord(TProp.pf)] := ptruint(@obj.PFnominal);
    PropertyOffset[ord(TProp.kVA)] := ptruint(@obj.StorageVars.FkVArating);
    PropertyOffset[ord(TProp.kV)] := ptruint(@obj.StorageVars.kVStorageBase);

    PropertyOffset[ord(TProp.kvar)] := ptruint(@obj.kvarRequested);
    PropertyReadFunction[ord(TProp.kvar)] := @Getkvar;
    PropertyFlags[ord(TProp.kvar)] := [TPropertyFlag.ReadByFunction];

    PropertyType[ord(TProp.pctstored)] := TPropertyType.DoubleProperty;
    PropertyOffset[ord(TProp.pctstored)] := 1;
    PropertyWriteFunction[ord(TProp.pctstored)] := @SetPctStored;
    PropertyReadFunction[ord(TProp.pctstored)] := @GetPctStored;
    PropertyFlags[ord(TProp.pctstored)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.ReadByFunction];

    PropertyType[ord(TProp.kW)] := TPropertyType.DoubleProperty;
    PropertyOffset[ord(TProp.kW)] := ptruint(@obj.kW_out);
    PropertyWriteFunction[ord(TProp.kW)] := @SetkW;
    PropertyFlags[ord(TProp.kW)] := [TPropertyFlag.WriteByFunction];

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TStorage2.NewObject(const ObjName: String; Activate: Boolean): Pointer;
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
    begin
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
end;

procedure TStorage2.UpdateAll();
var
    i: Integer;
begin
    for i := 1 to ElementList.Count do
        with TStorage2Obj(ElementList.Get(i)) do
            if Enabled then
                UpdateStorage();
end;

procedure TStorage2Obj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    i: Integer;
begin
    case Idx of
        ord(TProp.conn):
            begin
                SetNCondsForConnection(self);

                // VBase is always L-N voltage unless 1-phase device or more than 3 phases

                case Fnphases of
                    2, 3:
                        VBase := StorageVars.kVStorageBase * InvSQRT3x1000;    // L-N Volts
                else
                    VBase := StorageVars.kVStorageBase * 1000.0;   // Just use what is supplied
                end;

                VBase95 := Vminpu * VBase;
                VBase105 := Vmaxpu * VBase;

                Yorder := Fnconds * Fnterms;
                YprimInvalid := TRUE;
            end;

        ord(TProp.kv):
            case FNphases of
                2, 3:
                    VBase := StorageVars.kVStorageBase * InvSQRT3x1000;
            else
                VBase := StorageVars.kVStorageBase * 1000.0;
            end;

        ord(TProp.kVA):
            with StorageVars do
            begin
                kVASet := TRUE;
                if not kvarLimitSet then
                    StorageVars.Fkvarlimit := FkVArating;
                if not kvarLimitSet and not kvarLimitNegSet then
                    StorageVars.Fkvarlimitneg := FkVArating;
            end;

        ord(TProp.pf):
            varMode := VARMODEPF;

        ord(TProp.kvar):
            varMode := VARMODEKVAR;

        ord(TProp.kvarMax):
        begin
            kvarLimitSet := TRUE;
            if not kvarLimitNegSet then
                StorageVars.Fkvarlimitneg := Abs(StorageVars.Fkvarlimit);
        end;
        ord(TProp.kvarMaxAbs):
            kvarLimitNegSet := TRUE;

        ord(TProp.phases):
            SetNCondsForConnection(self);  // Force Reallocation of terminal info

        ord(TProp.kWrated):
            if not kVASet then
                StorageVars.FkVArating := StorageVars.kWrating;

        ord(TProp.kWhrated):
        begin
            StorageVars.kWhStored := StorageVars.kWhRating; // Assume fully charged
            kWhBeforeUpdate := StorageVars.kWhStored;
            StorageVars.kWhReserve := StorageVars.kWhRating * pctReserve * 0.01;
        end;

        ord(TProp.pctreserve):
            StorageVars.kWhReserve := StorageVars.kWhRating * pctReserve * 0.01;

        ord(TProp.UserModel):
        begin
            UserModel.Name := UserModelNameStr;
            IsUserModel := UserModel.Exists;
        end;
        ord(TProp.UserData):
            if UserModel.Exists then
                UserModel.Edit := UserModelEditStr;
        ord(TProp.DynaDLL):
        begin
            DynaModel.Name := DynaModelNameStr; 
            IsUserModel := DynaModel.Exists;
        end;
        ord(TProp.DynaData):
            if DynaModel.Exists then
                DynaModel.Edit := DynaModelEditStr;

        ord(TProp.debugtrace):
            if DebugTrace then
            begin   // Init trace file
                FreeAndNil(TraceFile);
                TraceFile := TBufferedFileStream.Create(DSS.OutputDirectory + 'STOR_' + Name + '.csv', fmCreate);
                FSWrite(TraceFile, 't, Iteration, LoadMultiplier, Mode, LoadModel, StorageModel,  Qnominalperphase, Pnominalperphase, CurrentType');
                for i := 1 to nphases do
                    FSWrite(Tracefile, ', |Iinj' + IntToStr(i) + '|');
                for i := 1 to nphases do
                    FSWrite(Tracefile, ', |Iterm' + IntToStr(i) + '|');
                for i := 1 to nphases do
                    FSWrite(Tracefile, ', |Vterm' + IntToStr(i) + '|');
                for i := 1 to NumVariables do
                    FSWrite(Tracefile, ', ' + VariableName(i));

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

function TStorage2.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
begin
    with TObj(ptr) do
    begin
        RecalcElementData;
        YPrimInvalid := TRUE;
        Exclude(Flags, Flg.EditionActive);
    end;
    Result := True;
end;

procedure TStorage2Obj.MakeLike(OtherPtr: Pointer);
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

    StorageVars.kVStorageBase := Other.StorageVars.kVStorageBase;
    Vbase := Other.Vbase;
    Vminpu := Other.Vminpu;
    Vmaxpu := Other.Vmaxpu;
    Vbase95 := Other.Vbase95;
    Vbase105 := Other.Vbase105;
    kW_out := Other.kW_out;
    kvar_out := Other.kvar_out;
    Pnominalperphase := Other.Pnominalperphase;
    PFNominal := Other.PFNominal;
    Qnominalperphase := Other.Qnominalperphase;
    Connection := Other.Connection;
    YearlyShapeObj := Other.YearlyShapeObj;
    DailyShapeObj := Other.DailyShapeObj;
    DutyShapeObj := Other.DutyShapeObj;
    DispatchMode := Other.DispatchMode;
    InverterCurveObj := Other.InverterCurveObj;
    StorageClass := Other.StorageClass;
    VoltageModel := Other.VoltageModel;

    Fstate := Other.Fstate;
    FstateChanged := Other.FstateChanged;
    kvarLimitSet := Other.kvarLimitSet;
    kvarLimitNegSet := Other.kvarLimitNegSet;

    FpctCutin := Other.FpctCutin;
    FpctCutout := Other.FpctCutout;
    VarFollowInverter := Other.VarFollowInverter;
    StorageVars.Fkvarlimit := Other.StorageVars.Fkvarlimit;
    StorageVars.Fkvarlimitneg := Other.StorageVars.Fkvarlimitneg;
    StorageVars.FkVArating := Other.StorageVars.FkVArating;

    FpctPminNoVars := Other.FpctPminNoVars;
    FpctPminkvarLimit := Other.FpctPminkvarLimit;

    kWOutIdling := Other.kWOutIdling;

    StorageVars.kWRating := Other.StorageVars.kWRating;
    StorageVars.kWhRating := Other.StorageVars.kWhRating;
    StorageVars.kWhStored := Other.StorageVars.kWhStored;
    StorageVars.kWhReserve := Other.StorageVars.kWhReserve;
    kWhBeforeUpdate := Other.kWhBeforeUpdate;
    pctReserve := Other.pctReserve;
    DischargeTrigger := Other.DischargeTrigger;
    ChargeTrigger := Other.ChargeTrigger;
    pctChargeEff := Other.pctChargeEff;
    pctDischargeEff := Other.pctDischargeEff;
    pctkWout := Other.pctkWout;
    pctkWin := Other.pctkWin;
    pctIdlekW := Other.pctIdlekW;
    pctIdlekvar := Other.pctIdlekvar;
    ChargeTime := Other.ChargeTime;

    pctR := Other.pctR;
    pctX := Other.pctX;

    RandomMult := Other.RandomMult;
    VWMode := Other.VWMode;
    VVMode := Other.VVMode;
    DRCMode := Other.DRCMode;
    WPMode := Other.WPMode;
    WVMode := Other.WVMode;

    UserModel.Name := Other.UserModel.Name;
    DynaModel.Name := Other.DynaModel.Name;
    UserModelNameStr := Other.UserModelNameStr;
    DynaModelNameStr := Other.DynaModelNameStr;
    
    //TODO: this doesn't copy the parameters of the user models

    IsUserModel := Other.IsUserModel;
    ForceBalanced := Other.ForceBalanced;
    CurrentLimited := Other.CurrentLimited;
end;

procedure TStorage2.ResetRegistersAll;  // Force all EnergyMeters in the circuit to reset
var
    idx: Integer;
begin
    idx := First;
    while idx > 0 do
    begin
        TStorage2Obj(GetActiveObj).ResetRegisters;
        idx := Next;
    end;
end;

procedure TStorage2.SampleAll();  // Force all Storage elements in the circuit to take a sample
var
    i: Integer;
begin
    for i := 1 to ElementList.Count do
        with TStorage2Obj(ElementList.Get(i)) do
            if Enabled then
                TakeSample();
end;

constructor TStorage2Obj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := AnsiLowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType; // + Storage_ELEMENT;  // In both PCelement and Storageelement list
    TraceFile := nil;

    FNphases := 3;
    Fnconds := 4;  // defaults to wye
    Yorder := 0;  // To trigger an initial allocation
    Nterms := 1;  // forces allocations

    YearlyShapeObj := NIL;
    DailyShapeObj := NIL;
    DutyShapeObj := NIL;

    InverterCurveObj := NIL;

    Connection := 0;    // Wye (star)
    VoltageModel := 1;  // Typical fixed kW negative load
    StorageClass := 1;

    StorageSolutionCount := -1;  // For keep track of the present solution in Injcurrent calcs
    OpenStorageSolutionCount := -1;
    YPrimOpenCond := NIL;

    StorageVars.kVStorageBase := 12.47;
    VBase := 7200.0;
    Vminpu := 0.90;
    Vmaxpu := 1.10;
    VBase95 := Vminpu * Vbase;
    VBase105 := Vmaxpu * Vbase;
    Yorder := Fnterms * Fnconds;
    RandomMult := 1.0;

    varMode := VARMODEPF;
    InverterON := TRUE; // start with inverterON
    kVA_exceeded := FALSE;
    VarFollowInverter := FALSE;

    ForceBalanced := FALSE;
    CurrentLimited := FALSE;

    with StorageVars do
    begin
        kWRating := 25.0;
        FkVArating := kWRating;
        kWhRating := 50;
        kWhStored := kWhRating;
        kWhBeforeUpdate := kWhRating;
        kWhReserve := kWhRating * pctReserve / 100.0;
        Fkvarlimit := FkVArating;
        Fkvarlimitneg := FkVArating;
        pctkWrated := 1.0;
        P_Priority := FALSE;
        PF_Priority := FALSE;

        EffFactor := 1.0;

        Vreg := 9999;
        Vavg := 9999;
        VVOperation := 9999;
        VWOperation := 9999;
        DRCOperation := 9999;
        VVDRCOperation := 9999;
        WPOperation := 9999;
        WVOperation := 9999;

    end;

    FDCkW := 25.0;

    FpctCutIn := 0.0;
    FpctCutOut := 0.0;

    FpctPminNoVars := -1.0; // Deactivated by default
    FpctPminkvarLimit := -1.0; // Deactivated by default

    pf_wp_nominal := 1.0;

    // Output rating stuff
    kvar_out := 0.0;
     // removed kvarBase     := kvar_out;     // initialize
    PFNominal := 1.0;

    pctR := 0.0;
    pctX := 50.0;

    // Make the StorageVars struct as public
    PublicDataStruct := @StorageVars;
    PublicDataSize := SizeOf(TStorage2Vars);

    IsUserModel := FALSE;
    UserModel := TStoreUserModel.Create(DSS);
    DynaModel := TStoreDynaModel.Create(DSS);
    UserModelNameStr := '';
    UserModelEditStr := '';
    DynaModelNameStr := '';
    DynaModelEditStr := '';

    FState := STORE_IDLING;  // Idling and fully charged
    FStateChanged := TRUE;  // Force building of YPrim
    pctReserve := 20.0;  // per cent of kWhRating
    pctIdlekW := 1.0;
    pctIdlekvar := 0.0;

    DischargeTrigger := 0.0;
    ChargeTrigger := 0.0;
    pctChargeEff := 90.0;
    pctDischargeEff := 90.0;
    pctkWOut := 100.0;
    pctkWIn := 100.0;

    ChargeTime := 2.0;   // 2 AM

    kVASet := FALSE;
    kvarLimitSet := FALSE;
    kvarLimitNegSet := FALSE;

    Reg_kWh := 1;
    Reg_kvarh := 2;
    Reg_MaxkW := 3;
    Reg_MaxkVA := 4;
    Reg_Hours := 5;
    Reg_Price := 6;

    DebugTrace := FALSE;
    Storage2ObjSwitchOpen := FALSE;
    SpectrumObj := NIL; // override base class
    VWMode := FALSE;
    VVMode := FALSE;
    DRCMode := FALSE;
    WPMode := FALSE;
    WVMode := FALSE;

    RecalcElementData();
end;

procedure TStorage2Obj.Randomize(Opt: Integer);
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

destructor TStorage2Obj.Destroy;
begin
    YPrimOpenCond.Free;
    UserModel.Free;
    DynaModel.Free;
    FreeAndNil(TraceFile);
    inherited Destroy;
end;

procedure TStorage2Obj.CalcDailyMult(Hr: Double);
begin
    if (DailyShapeObj <> NIL) then
    begin
        ShapeFactor := DailyShapeObj.GetMultAtHour(Hr);
    end
    else
        ShapeFactor := CDOUBLEONE;  // Default to no  variation

    CheckStateTriggerLevel(ShapeFactor.re);   // last recourse
end;

procedure TStorage2Obj.CalcDutyMult(Hr: Double);
begin
    if DutyShapeObj <> NIL then
    begin
        ShapeFactor := DutyShapeObj.GetMultAtHour(Hr);
        CheckStateTriggerLevel(ShapeFactor.re);
    end
    else
        CalcDailyMult(Hr);  // Default to Daily Mult If no duty curve specified
end;

procedure TStorage2Obj.CalcYearlyMult(Hr: Double);
begin
    if YearlyShapeObj <> NIL then
    begin
        ShapeFactor := YearlyShapeObj.GetMultAtHour(Hr);
        CheckStateTriggerLevel(ShapeFactor.re);
    end
    else
        CalcDailyMult(Hr);  // Defaults to Daily curve
end;

procedure TStorage2Obj.RecalcElementData();
begin
    VBase95 := VMinPu * VBase;
    VBase105 := VMaxPu * VBase;

    with StorageVars do
    begin
        YeqDischarge := Cmplx((kWrating * 1000.0 / SQR(vbase) / FNPhases), 0.0);

        // values in ohms for thevenin equivalents
        RThev := pctR * 0.01 * SQR(PresentkV) / FkVARating * 1000.0;      // Changed
        XThev := pctX * 0.01 * SQR(PresentkV) / FkVARating * 1000.0;      // Changed

        CutInkW := FpctCutin * FkVArating / 100.0;
        CutOutkW := FpctCutOut * FkVArating / 100.0;

        if FpctPminNoVars <= 0 then
            PminNoVars := -1.0
        else
            PminNoVars := FpctPminNoVars * kWrating / 100.0;

        if FpctPminkvarLimit <= 0 then
            PminkvarLimit := -1.0
        else
            PminkvarLimit := FpctPminkvarLimit * kWrating / 100.0;

        // efficiencies
        ChargeEff := pctChargeEff * 0.01;
        DisChargeEff := pctDisChargeEff * 0.01;

        PIdling := pctIdlekW * kWrating / 100.0;

        if Assigned(InverterCurveObj) then
        begin
            kWOutIdling := PIdling / (InverterCurveObj.GetYValue(Pidling / (FkVArating)));
        end
        else
            kWOutIdling := PIdling;

    end;

    SetNominalStorageOutput();

    // Initialize to Zero - defaults to PQ Storage element
    // Solution object will reset after circuit modifications

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

    // Update any user-written models
    if Usermodel.Exists then
        UserModel.FUpdateModel;  // Checks for existence and Selects
    if Dynamodel.Exists then
        Dynamodel.FUpdateModel;  // Checks for existence and Selects
end;

procedure TStorage2Obj.SetNominalStorageOutput();
begin
    ShapeFactor := CDOUBLEONE;  // init here; changed by curve routine
    // Check to make sure the Storage element is ON
    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        if not (IsDynamicModel or IsHarmonicModel) then     // Leave Storage element in whatever state it was prior to entering Dynamic mode
        begin
            // Check dispatch to see what state the Storage element should be in
            case DispatchMode of

                STORE_EXTERNALMODE: ;  // Do nothing
                STORE_LOADMODE:
                    CheckStateTriggerLevel(GeneratorDispatchReference);
                STORE_PRICEMODE:
                    CheckStateTriggerLevel(PriceSignal);

            else // dispatch off element's loadshapes, If any
                with Solution do
                    case Mode of
                        TSolveMode.SNAPSHOT: ; // Just solve for the present kW, kvar  // Don't check for state change
                        TSolveMode.DAILYMODE:
                            CalcDailyMult(DynaVars.dblHour); // Daily dispatch curve
                        TSolveMode.YEARLYMODE:
                            CalcYearlyMult(DynaVars.dblHour);
                        // MONTECARLO1,
                        // MONTEFAULT,
                        // FAULTSTUDY,
                        // DYNAMICMODE:   ; // do nothing for these modes
                        TSolveMode.GENERALTIME:
                        begin
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
                            // Assume Daily curve, If any, for the following
                        TSolveMode.MONTECARLO2,
                        TSolveMode.MONTECARLO3,
                        TSolveMode.LOADDURATION1,
                        TSolveMode.LOADDURATION2:
                            CalcDailyMult(DynaVars.dblHour);
                        TSolveMode.PEAKDAY:
                            CalcDailyMult(DynaVars.dblHour);

                        TSolveMode.DUTYCYCLE:
                            CalcDutyMult(DynaVars.dblHour);
                // AUTOADDFLAG:  ; 
                    end;

            end;

            ComputekWkvar;

            // Pnominalperphase is net at the terminal.  If supplying idling losses, when discharging,
            // the Storage supplies the idling losses. When charging, the idling losses are subtracting from the amount
            // entering the Storage element.

            with StorageVars do
            begin
                Pnominalperphase := 1000.0 * kW_out / Fnphases;
                Qnominalperphase := 1000.0 * kvar_out / Fnphases;
            end;


            case VoltageModel of
            //****  Fix this when user model gets connected in
                3: // Yeq := Cinv(cmplx(0.0, -StoreVARs.Xd))  ;  // Gets negated in CalcYPrim
            else
                // Yeq no longer used for anything other than this calculation of Yeq95, Yeq105 and
                // constant Z power flow model
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
            // Like Model 7 generator, max current is based on amount of current to get out requested power at min voltage
            with StorageVars do
            begin
                PhaseCurrentLimit := Cmplx(Pnominalperphase, Qnominalperphase) / VBase95;
                MaxDynPhaseCurrent := Cabs(PhaseCurrentLimit);
            end;

            // When we leave here, all the Yeq's are in L-N values

        end;  // If  NOT (IsDynamicModel or IsHarmonicModel)
    end; // With ActiveCircuit

   // If Storage element state changes, force re-calc of Y matrix
    if FStateChanged then
    begin
        YprimInvalid := TRUE;
        FStateChanged := FALSE;  // reset the flag
    end;
end;

procedure TStorage2Obj.ComputekWkvar;
begin
    ComputePresentkW;
    ComputeInverterPower; // apply inverter eff after checking for cutin/cutout
end;

procedure TStorage2Obj.ComputePresentkW;
var
    OldState: Integer;
begin
    OldState := Fstate;
    StateDesired := OldState;
    with StorageVars do
        case FState of
            STORE_CHARGING:
            begin
                if kWhStored < kWhRating then
                    case DispatchMode of
                        STORE_FOLLOW:
                        begin
                            kW_out := kWRating * ShapeFactor.re;
                            pctkWIn := abs(ShapeFactor.re) * 100.0;  // keep %charge updated
                        end
                    else
                        kW_out := -kWRating * pctkWin / 100.0;
                    end
                else
                    Fstate := STORE_IDLING;   // all charged up
            end;
            STORE_DISCHARGING:
            begin
                if kWhStored > kWhReserve then
                    case DispatchMode of
                        STORE_FOLLOW:
                        begin
                            kW_out := kWRating * ShapeFactor.re;
                            pctkWOut := abs(ShapeFactor.re) * 100.0;  // keep %discharge updated
                        end
                    else
                        kW_out := kWRating * pctkWout / 100.0;
                    end
                else
                    Fstate := STORE_IDLING;  // not enough Storage to discharge
            end;
        end;

    // If idling output is only losses
    if Fstate = STORE_IDLING then
    begin
        kW_out   := -kWOutIdling;
    end;

    if OldState <> Fstate then
        FstateChanged := TRUE;
end;

procedure TStorage2Obj.ComputeInverterPower;
var
    kVA_Gen: Double;
    OldState: Integer;
    TempPF: Double = 0.0; // temporary power factor
    Qramp_limit: Double = 0.0;
begin
    // Reset CurrentkvarLimit to kvarLimit
    CurrentkvarLimit := StorageVars.Fkvarlimit;
    CurrentkvarLimitNeg := StorageVars.Fkvarlimitneg;

    with StorageVars do
    begin
        if Assigned(InverterCurveObj) then
        begin
            if Fstate = STORE_DISCHARGING then
            begin
                CutOutkWAC := CutOutkW * InverterCurveObj.GetYValue(abs(CutOutkW) / FkVArating);
                CutInkWAC := CutInkW * InverterCurveObj.GetYValue(abs(CutInkW) / FkVArating);
            end
            else  // Charging or Idling
            begin
                CutOutkWAC := CutOutkW / InverterCurveObj.GetYValue(abs(CutOutkW) / FkVArating);
                CutInkWAC := CutInkW / InverterCurveObj.GetYValue(abs(CutInkW) / FkVArating);
            end;
        end
        else // Assume Ideal Inverter
        begin
            CutOutkWAC := CutOutkW;
            CutInkWAC := CutInkW;
        end;

        OldState := Fstate;

      // CutIn/CutOut checking performed on the AC side.
        if InverterON then
        begin
            if abs(kW_Out) < CutOutkWAC then
            begin
                InverterON := FALSE;
                Fstate := STORE_IDLING;
            end;
        end
        else
        begin
            if abs(kW_Out) >= CutInkWAC then
            begin
                InverterON := TRUE;
            end
            else
            begin
                Fstate := STORE_IDLING;
            end;
        end;

        if OldState <> Fstate then
            FstateChanged := TRUE;

        // Set inverter output
        if InverterON then
        begin
            kWOut_Calc;
        end
        else
        begin
            // Idling
            kW_Out := -kWOutIdling; // In case it has just turned off due to %CutIn/%CutOut. Necessary to make sure SOC will be kept constant (higher priority than the %CutIn/%CutOut operation)
        end;

        // Calculate kvar value based on operation mode (PF or kvar)
        if FState = STORE_IDLING then      // Should watt-pf with watt=0 be applied here?
        // If in Idling state, check for kvarlimit only
        begin
            if varMode = VARMODEPF then
            begin
                // kvar_out := 0.0; //kW = 0 leads to kvar = 0 in constant PF Mode
                kvar_out := kW_out * sqrt(1.0 / SQR(PFnominal) - 1.0) * sign(PFnominal);

                if (kvar_out > 0.0) and (abs(kvar_out) > Fkvarlimit) then
                    kvar_Out := Fkvarlimit
                else
                if (kvar_out < 0.0) and (abs(kvar_out) > Fkvarlimitneg) then
                    kvar_Out := Fkvarlimitneg * sign(kvarRequested)

            end
            else  // kvarRequested might have been set either internally or by an InvControl
            begin
                if (kvarRequested > 0.0) and (abs(kvarRequested) > Fkvarlimit) then
                    kvar_Out := Fkvarlimit
                else
                if (kvarRequested < 0.0) and (abs(kvarRequested) > Fkvarlimitneg) then
                    kvar_Out := Fkvarlimitneg * sign(kvarRequested)
                else
                    kvar_Out := kvarRequested;
            end;
        end
        else
        // If in either Charging or Discharging states
        begin
            if (abs(kW_Out) < PminNoVars) then
            begin
                kvar_out := 0.0;  // Check minimum P for Q gen/absorption. if PminNoVars is disabled (-1), this will always be false

                CurrentkvarLimit := 0;
                CurrentkvarLimitNeg := 0.0;  // InvControl uses this.
            end
            else
            if varMode = VARMODEPF then
            begin
                if PFnominal = 1.0 then
                    kvar_out := 0.0
                else
                begin
                    kvar_out := kW_out * sqrt(1.0 / SQR(PFnominal) - 1.0) * sign(PFnominal); //kvar_out desired by constant PF

                    // Check Limits
                    if abs(kW_out) < PminkvarLimit then // straight line limit check. if PminkvarLimit is disabled (-1), this will always be false.
                    begin
                        // straight line starts at max(PminNoVars, CutOutkWAC)
                        // if CutOut differs from CutIn, take cutout since it is assumed that CutOut <= CutIn always.
                        if abs(kW_out) >= max(PminNoVars, CutOutkWAC) then
                        begin
                            if (kvar_Out > 0.0) then
                            begin
                                Qramp_limit := Fkvarlimit / PminkvarLimit * abs(kW_out);   // generation limit
                            end
                            else
                            if (kvar_Out < 0.0) then
                            begin
                                Qramp_limit := Fkvarlimitneg / PminkvarLimit * abs(kW_out);   // absorption limit
                            end;

                            if abs(kvar_Out) > Qramp_limit then
                            begin
                                kvar_out := Qramp_limit * sign(kW_out) * sign(PFnominal);

                                if kvar_out > 0 then
                                    CurrentkvarLimit := Qramp_limit;  // For use in InvControl
                                if kvar_out < 0 then
                                    CurrentkvarLimitNeg := Qramp_limit;  // For use in InvControl
                            end;
                        end
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
            else  // VARMODE kvar
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
                            CurrentkvarLimit := Qramp_limit;    // For use in InvControl
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
                if ((kvarRequested > 0.0) and (abs(kvarRequested) > Fkvarlimit)) or ((kvarRequested < 0.0) and (abs(kvarRequested) > Fkvarlimitneg)) then
                begin
                    if (kvarRequested > 0.0) then
                        kvar_Out := Fkvarlimit * sign(kvarRequested)
                    else
                        kvar_Out := Fkvarlimitneg * sign(kvarRequested);

                    if (varMode = VARMODEKVAR) and PF_Priority and WPMode then
                    begin
                        kW_out := abs(kvar_out) * sqrt(1.0 / (1.0 - Sqr(pf_wp_nominal)) - 1.0) * sign(kW_out);
                    end

                    // Forces constant power factor when kvar limit is exceeded and PF Priority is true. Temp PF is calculated based on kvarRequested
                    // PF Priority is not valid if controlled by an InvControl operating in at least one amongst VV and DRC modes
                    else
                    if PF_Priority and (not VVMode or not DRCMode or not WVmode) then
                    begin
                        if abs(kvarRequested) > 0.0 then
                        begin
                            TempPF := cos(arctan(abs(kvarRequested / kW_out)));
                            kW_out := abs(kvar_out) * sqrt(1.0 / (1.0 - Sqr(TempPF)) - 1.0) * sign(kW_out);
                        end
                    end
                end
                else
                    kvar_Out := kvarRequested;
            end;
        end;

        if (InverterON = FALSE) and (VarFollowInverter = TRUE) then
            kvar_out := 0.0;

        // Limit kvar and kW so that kVA of inverter is not exceeded
        kVA_Gen := Sqrt(Sqr(kW_out) + Sqr(kvar_out));

        if kVA_Gen > FkVArating then
        begin
            kVA_exceeded := TRUE;

            // Expectional case: When kVA is exceeded and in idling state, we force P priority always
            if (FState = STORE_IDLING) then
            begin
                kvar_Out := Sqrt(SQR(FkVArating) - SQR(kW_Out)) * sign(kvar_Out);
            end

            // Regular Cases
            else
            if (varMode = VARMODEPF) and PF_Priority then
            // Operates under constant power factor when kVA rating is exceeded. PF must be specified and PFPriority must be TRUE
            begin
                kW_out := FkVArating * abs(PFnominal) * sign(kW_out);

                kvar_out := FkVArating * sqrt(1 - Sqr(PFnominal)) * sign(kW_out) * sign(PFnominal);
            end
            else
            if (varMode = VARMODEKVAR) and PF_Priority and WPMode then
            begin
                kW_out := FkVArating * abs(pf_wp_nominal) * sign(kW_out);
                kvar_out := FkVArating * abs(sin(ArcCos(pf_wp_nominal))) * sign(kvarRequested)
            end
            else
            if (varMode = VARMODEKVAR) and PF_Priority and (not VVMode or not DRCMode or not WVmode) then
            // Operates under constant power factor (PF implicitly calculated based on kw and kvar)
            begin
                if abs(kvar_out) = Fkvarlimit then
                begin   // for handling cases when kvar limit and inverter's kVA limit are exceeded
                    kW_out := FkVArating * abs(TempPF) * sign(kW_out);  // Temp PF has already been calculated at this point
                end
                else
                begin
                    kW_out := FkVArating * abs(cos(ArcTan(kvarRequested / kW_out))) * sign(kW_out);
                end;

                kvar_out := FkVArating * abs(sin(ArcCos(kW_out / FkVArating))) * sign(kvarRequested)
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
                    kW_Out := Sqrt(SQR(FkVArating) - SQR(kvar_Out)) * sign(kW_Out); // Q Priority   (Default) back off the kW

            end;
        end  // With StorageVars
        else
        if abs(kVA_Gen - FkVArating) / FkVArating < 0.0005 then
            kVA_exceeded := TRUE
        else
            kVA_exceeded := FALSE;
    end;
end;

procedure TStorage2Obj.CalcYPrimMatrix(Ymatrix: TcMatrix);
var
    Y, Yij: Complex;
    i, j: Integer;
    FreqMultiplier: Double;
begin
    FYprimFreq := ActiveCircuit.Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;
    
    with ActiveCircuit.Solution do
        if IsHarmonicModel then // IsDynamicModel or
        begin
            // Yeq is computed from %R and %X -- inverse of Rthev + j Xthev
            Y := Yeq;

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
        begin  //  Regular power flow Storage element model
            // Yeq is always expected as the equivalent line-neutral admittance
            case Fstate of
                STORE_CHARGING:
                    Y := YeqDischarge;
                STORE_IDLING:
                    Y := cmplx(0.0, 0.0);
                STORE_DISCHARGING:
                    Y := -YeqDischarge;
            end;
            //---DEBUG--- WriteDLLDebugFile(Format('t=%.8g, Change To State=%s, Y=%.8g +j %.8g',[ActiveCircuit.Solution.dblHour, StateToStr, Y.re, Y.im]));
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

function TStorage2Obj.NormalizeToTOD(h: Integer; sec: Double): Double;
// Normalize time to a floating point number representing time of day If Hour > 24
// time should be 0 to 24.
var
    HourOfDay: Integer;
begin
    if h > 23 then
        HourOfDay := (h - (h div 24) * 24)
    else
        HourOfDay := h;

    Result := HourOfDay + sec / 3600.0;

    if Result > 24.0 then
        Result := Result - 24.0;   // Wrap around
end;

procedure TStorage2Obj.CheckStateTriggerLevel(Level: Double);
// This is where we set the state of the Storage element
var
    OldState: Integer;
begin
    FStateChanged := FALSE;

    OldState := Fstate;

    with StorageVars do
        if DispatchMode = STORE_FOLLOW then
        begin
            // set charge and discharge modes based on sign of loadshape
            if (Level > 0.0) and (kWhStored > kWhReserve) then
                StorageState := STORE_DISCHARGING
            else
            if (Level < 0.0) and (kWhStored < kWhRating) then
                StorageState := STORE_CHARGING
            else
                StorageState := STORE_IDLING;
        end
        else
        begin   // All other dispatch modes  Just compare to trigger value
            if (ChargeTrigger = 0.0) and (DischargeTrigger = 0.0) then
                Exit;

            // First see If we want to turn off Charging or Discharging State
            case Fstate of
                STORE_CHARGING:
                    if (ChargeTrigger <> 0.0) then
                        if (ChargeTrigger < Level) or (kWhStored >= kWHRating) then
                            Fstate := STORE_IDLING;
                STORE_DISCHARGING:
                    if (DischargeTrigger <> 0.0) then
                        if (DischargeTrigger > Level) or (kWhStored <= kWHReserve) then
                            Fstate := STORE_IDLING;
            end;

            // Now check to see If we want to turn on the opposite state
            case Fstate of
                STORE_IDLING:
                begin
                    if (DischargeTrigger <> 0.0) and (DischargeTrigger < Level) and (kWhStored > kWHReserve) then
                        FState := STORE_DISCHARGING
                    else
                    if (ChargeTrigger <> 0.0) and (ChargeTrigger > Level) and (kWhStored < kWHRating) then
                        Fstate := STORE_CHARGING;

                               // Check to see If it is time to turn the charge cycle on If it is not already on.
                    if not (Fstate = STORE_CHARGING) then
                        if ChargeTime > 0.0 then
                            with ActiveCircuit.Solution do
                            begin
                                if abs(NormalizeToTOD(DynaVars.intHour, DynaVARs.t) - ChargeTime) < DynaVARs.h / 3600.0 then
                                    Fstate := STORE_CHARGING;
                            end;
                end;
            end;
        end;

    if OldState <> Fstate then
    begin
        FstateChanged := TRUE;
        YprimInvalid := TRUE;
    end;
end;

procedure TStorage2Obj.CalcYPrim();
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

    SetNominalStorageOutput();
    CalcYPrimMatrix(YPrim_Shunt);

     // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages Doesn't fail
    for i := 1 to Yorder do
        Yprim_Series.SetElement(i, i, Yprim_Shunt.Getelement(i, i) * 1.0e-10);

    YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
    inherited CalcYPrim();
end;

procedure TStorage2Obj.StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);
// Add the current into the proper location according to connection
// Reverse of similar routine in load  (Cnegates are switched)
var
    j: Integer;
begin
    case Connection of
        0:
        begin  //Wye
            TermArray^[i] += Curr;
            TermArray^[Fnconds] += -Curr; // Neutral
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

procedure TStorage2Obj.WriteTraceRecord(const s: String);
var
    i: Integer;
    sout: String;
begin
    try
        if (not DSS.InshowResults) then
        begin
            WriteStr(sout, Format('%-.g, %d, %-.g, ',
                [ActiveCircuit.Solution.DynaVars.dblHour,
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
            for i := 1 to NumVariables do
                FSWrite(TraceFile, Format('%-.g, ', [Variable[i]]));

            FSWriteln(TRacefile);
            FSFlush(TraceFile);
        end;
    except
        On E: Exception do
        begin
        end;

    end;
end;

procedure TStorage2Obj.DoConstantPQStorage2Obj();
// Compute total terminal current for Constant PQ
var
    i: Integer;
    Curr,
//   CurrIdlingZ,
    VLN, VLL: Complex;
   //---DEBUG--- S:Complex;
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
                if VMagLN <= VBase95 then
                    Curr := Yeq95 * VLN  // Below 95% use an impedance model
                else
                if VMagLN > VBase105 then
                    Curr := Yeq105 * VLN  // above 105% use an impedance model
                else
                    Curr := cong(Cmplx(Pnominalperphase, Qnominalperphase) / VLN);  // Between 95% -105%, constant PQ

                if CurrentLimited then
                    if Cabs(Curr) > MaxDynPhaseCurrent then
                        Curr := cong(PhaseCurrentLimit / (VLN / VMagLN));
            end;

            1:
            begin  // Delta
                VLL := Vterminal^[i];
                VMagLL := Cabs(VLL);
                if Fnphases > 1 then
                    VMagLN := VMagLL / SQRT3
                else
                    VMagLN := VmagLL;  // L-N magnitude
                if VMagLN <= VBase95 then
                    Curr := (Yeq95 / 3.0) * VLL  // Below 95% use an impedance model
                else
                if VMagLN > VBase105 then
                    Curr := (Yeq105 / 3.0) * VLL  // above 105% use an impedance model
                else
                    Curr := cong(Cmplx(Pnominalperphase, Qnominalperphase) / VLL);  // Between 95% -105%, constant PQ

                if CurrentLimited then
                    if Cabs(Curr) * SQRT3 > MaxDynPhaseCurrent then
                        Curr := cong(PhaseCurrentLimit / (VLL / VMagLN)); // Note VmagLN has sqrt3 factor in it
            end;

        end;

        //---DEBUG--- WriteDLLDebugFile(Format('        Phase=%d, Pnom=%.8g +j %.8g', [i, Pnominalperphase, Qnominalperphase ]));

        StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(TRUE);
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
    //---DEBUG--- WriteDLLDebugFile(Format('        Icomp=%s ', [CmplxArrayToString(InjCurrent, Yprim.Order) ]));
end;

procedure TStorage2Obj.DoConstantZStorage2Obj();
// constant Z model
var
    i: Integer;
    Curr,
    Yeq2: Complex;
    V012: array[0..2] of Complex;  // Sequence voltages
begin
    // Assume Yeq is kept up to date
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase(); // get actual voltage across each phase of the load
    ZeroITerminal;
    if Connection = 0 then
        Yeq2 := Yeq
    else
        Yeq2 := Yeq / 3.0;

    if ForceBalanced and (Fnphases = 3) then
    begin  // convert to pos-seq only
        Phase2SymComp(Vterminal, pComplexArray(@V012));
        V012[0] := CZERO; // Force zero-sequence voltage to zero
        V012[2] := CZERO; // Force negative-sequence voltage to zero
        SymComp2Phase(Vterminal, pComplexArray(@V012));  // Reconstitute Vterminal as balanced
    end;

    for i := 1 to Fnphases do
    begin
        Curr := Yeq2 * Vterminal^[i];   // Yeq is always line to neutral
        StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(TRUE);
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;

procedure TStorage2Obj.DoUserModel();
// Compute total terminal Current from User-written model
var
    i: Integer;
begin
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array

    if UserModel.Exists then    // Check automatically selects the usermodel If true
    begin
        UserModel.FCalc(Vterminal, Iterminal);
        set_ITerminalUpdated(TRUE);
        with ActiveCircuit.Solution do
        begin          // Negate currents from user model for power flow Storage element model
            for i := 1 to FnConds do
                InjCurrent^[i] -= Iterminal^[i];
        end;
    end
    else
    begin
        DoSimpleMsg('%s model designated to use user-written model, but user-written model is not defined.', [FullName], 567);
    end;
end;

procedure TStorage2Obj.DoDynamicMode;
// Compute Total Current and add into InjTemp

//   For now, just assume the Storage element Thevenin voltage is constant
//   for the duration of the dynamic simulation.
var
    i: Integer;
    V012,
    I012: array[0..2] of Complex;

    procedure CalcVthev_Dyn;
    begin
        with StorageVars do
            Vthev := pclx(VthevMag, Theta);   // keeps theta constant
    end;
begin
   // Test using DESS model
   // Compute Vterminal

    if DynaModel.Exists then
        DoDynaModel()   // do user-written model
    else
    begin
        CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
        ZeroITerminal;

       // Simple Thevenin equivalent
       // compute terminal current (Iterminal) and take out the Yprim contribution

        with StorageVars do
            case Fnphases of
                1:
                begin
                    CalcVthev_Dyn;  // Update for latest phase angle
                    ITerminal^[1] := (VTerminal^[1] - Vthev - VTerminal^[2]) / Zthev;
                    if CurrentLimited then
                        if Cabs(Iterminal^[1]) > MaxDynPhaseCurrent then   // Limit the current but keep phase angle
                            ITerminal^[1] := ptocomplex(topolar(MaxDynPhaseCurrent, cang(Iterminal^[1])));
                    ITerminal^[2] := -ITerminal^[1];
                end;
                3:
                begin
                    Phase2SymComp(Vterminal, pComplexArray(@V012));

                  // Positive Sequence Contribution to Iterminal
                    CalcVthev_Dyn;  // Update for latest phase angle

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

                    I012[0] := CZERO;

                    SymComp2Phase(ITerminal, pComplexArray(@I012));  // Convert back to phase components

                end;
            else
                DoSimpleMsg('Dynamics mode is implemented only for 1- or 3-phase Storage Element. %s has %d phases.', [FullName, Fnphases], 5671);
                DSS.SolutionAbort := TRUE;
            end;

        // Add it into inj current array
        for i := 1 to FnConds do
            InjCurrent^[i] -= Iterminal^[i];
    end;
end;

procedure TStorage2Obj.DoDynaModel();
var
    DESSCurr: array[1..6] of Complex;  // Temporary biffer
    i: Integer;
begin
    // do user written dynamics model
    with ActiveCircuit.Solution do
    begin  // Just pass node voltages to ground and let dynamic model take care of it
        for i := 1 to FNconds do
            VTerminal^[i] := NodeV^[NodeRef^[i]];
        StorageVars.w_grid := TwoPi * Frequency;
    end;

    DynaModel.FCalc(Vterminal, pComplexArray(@DESSCurr));

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        StickCurrInTerminalArray(ITerminal, -DESSCurr[i], i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(TRUE);
        StickCurrInTerminalArray(InjCurrent, DESSCurr[i], i);  // Put into Terminal array taking into account connection
    end;
end;

procedure TStorage2Obj.DoHarmonicMode();
// Compute Injection Current Only when in harmonics mode

// Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built
// Vd is the fundamental frequency voltage behind Xd" for phase 1
var
    i: Integer;
    E: Complex;
    StorageHarmonic: Double;
    pBuffer: PCBuffer24;
begin
    pBuffer := @TStorage2(ParentClass).cBuffer;
    ComputeVterminal();

    with ActiveCircuit.Solution do
    begin
        StorageHarmonic := Frequency / StorageFundamental;
        if SpectrumObj <> NIL then
            E := SpectrumObj.GetMult(StorageHarmonic) * StorageVars.VThevHarm // Get base harmonic magnitude
        else
            E := CZERO;

        RotatePhasorRad(E, StorageHarmonic, StorageVars.ThetaHarm);  // Time shift by fundamental frequency phase shift
        for i := 1 to Fnphases do
        begin
            pBuffer[i] := E;
            if i < Fnphases then
                RotatePhasorDeg(E, StorageHarmonic, -120.0);  // Assume 3-phase Storage element
        end;
    end;

    // Handle Wye Connection
    if Connection = 0 then
        pBuffer[Fnconds] := Vterminal^[Fnconds];  // assume no neutral injection voltage

    // Inj currents = Yprim (E)
    YPrim.MVMult(InjCurrent, pComplexArray(pBuffer));
end;

procedure TStorage2Obj.CalcVTerminalPhase();
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

    StorageSolutionCount := ActiveCircuit.Solution.SolutionCount;
end;

procedure TStorage2Obj.CalcStorageModelContribution();
// Calculates Storage element current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)
begin
    set_ITerminalUpdated(FALSE);
    with  ActiveCircuit, ActiveCircuit.Solution do
    begin
        if IsDynamicModel then
            DoDynamicMode()
        else
        if IsHarmonicModel and (Frequency <> Fundamental) then
            DoHarmonicMode()
        else
        begin
               //  compute currents and put into InjTemp array;
            case VoltageModel of
                1:
                    DoConstantPQStorage2Obj();
                2:
                    DoConstantZStorage2Obj();
                3:
                    DoUserModel();
            else
                DoConstantPQStorage2Obj();  // for now, until we implement the other models.
            end;
        end; 
    end;
    // When this is Done, ITerminal is up to date
end;

procedure TStorage2Obj.CalcInjCurrentArray();
// Difference between currents in YPrim and total current
begin
    // Now Get Injection Currents
    if Storage2ObjSwitchOpen then
        ZeroInjCurrent
    else
        CalcStorageModelContribution();
end;

procedure TStorage2Obj.GetTerminalCurrents(Curr: pComplexArray);
// Compute total Currents
begin
    with ActiveCircuit.Solution do
    begin
        if IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount then
        begin     // recalc the contribution
            if not Storage2ObjSwitchOpen then
                CalcStorageModelContribution();  // Adds totals in Iterminal as a side effect
        end;
        inherited GetTerminalCurrents(Curr);
    end;

    if (DebugTrace) then
        WriteTraceRecord('TotalCurrent');
end;

function TStorage2Obj.InjCurrents(): Integer;
begin
    with ActiveCircuit.Solution do
    begin
        if LoadsNeedUpdating then
            SetNominalStorageOutput(); // Set the nominal kW, etc for the type of solution being Done

        CalcInjCurrentArray(); // Difference between currents in YPrim and total terminal current

        if (DebugTrace) then
            WriteTraceRecord('Injection');

         // Add into System Injection Current Array
        Result := inherited InjCurrents();
    end;
end;

procedure TStorage2Obj.ResetRegisters;
var
    i: Integer;
begin
    for i := 1 to NumStorage2Registers do
        Registers[i] := 0.0;
    for i := 1 to NumStorage2Registers do
        Derivatives[i] := 0.0;
    FirstSampleAfterReset := TRUE;  // initialize for trapezoidal integration
end;

procedure TStorage2Obj.Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);
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

procedure TStorage2Obj.TakeSample();
// Update Energy from metered zone
var
    S: Complex;
    Smag: Double;
    HourValue: Double;
begin
    // Compute energy in Storage element branch
    if Enabled then
    begin
        // Only tabulate discharge hours
        if FSTate = STORE_DISCHARGING then
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

        if (FState = STORE_DISCHARGING) or ActiveCircuit.TrapezoidalIntegration then
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
            end;
    end;
end;

procedure TStorage2Obj.UpdateStorage();
// Update Storage levels
begin
    with StorageVars do
    begin
        kWhBeforeUpdate := kWhStored;   // keep this for reporting change in Storage as a variable

        // Assume User model will take care of updating Storage in dynamics mode
        if ActiveCircuit.solution.IsDynamicModel and IsUserModel then
            Exit;

        with ActiveCircuit.Solution do
            case FState of
                STORE_DISCHARGING:
                begin
                    kWhStored := kWhStored - (DCkW + kWIdlingLosses) / DischargeEff * IntervalHrs;
                    if kWhStored < kWhReserve then
                    begin
                        kWhStored := kWhReserve;
                        Fstate := STORE_IDLING;  // It's empty Turn it off
                        FstateChanged := TRUE;
                    end;
                end;

                STORE_CHARGING:
                begin
                    if (abs(DCkW) - kWIdlingLosses) >= 0 then // 99.9 % of the cases will fall here
                    begin
                        kWhStored := kWhStored + (abs(DCkW) - kWIdlingLosses) * ChargeEff * IntervalHrs;
                        if kWhStored > kWhRating then
                        begin
                            kWhStored := kWhRating;
                            Fstate := STORE_IDLING;  // It's full Turn it off
                            FstateChanged := TRUE;
                        end;
                    end
                    else   // Exceptional cases when the idling losses are higher than the DCkW such that the net effect is that the
                                 // the ideal Storage will discharge
                    begin
                        kWhStored := kWhStored + (abs(DCkW) - kWIdlingLosses) / DischargeEff * IntervalHrs;
                        if kWhStored < kWhReserve then
                        begin
                            kWhStored := kWhReserve;
                            Fstate := STORE_IDLING;  // It's empty Turn it off
                            FstateChanged := TRUE;
                        end;
                    end;

                end;

                STORE_IDLING: ;
            end;

    end;

    // the update is done at the end of a time step so have to force
    // a recalc of the Yprim for the next time step.  Else it will stay the same.
    if FstateChanged then
        YprimInvalid := TRUE;
end;

procedure TStorage2Obj.ComputeDCkW;
// Computes actual DCkW to Update Storage SOC
var
    coefGuess: TCoeff;
    coef: TCoeff;
    N_tentatives: Integer;
begin
    coefGuess[1] := 0.0;
    coefGuess[2] := 0.0;

    coef[1] := 1.0;
    coef[2] := 1.0;  // just a guess

    FDCkW := Power[1].re * 0.001;  // Assume ideal inverter

    if not Assigned(InverterCurveObj) then
    begin
    // make sure sign is correct
        if (FState = STORE_IDLING) then
            FDCkW := abs(FDCkW) * -1
        else
            FDCkW := abs(FDCkW) * FState;
        Exit;
    end;

    N_tentatives := 0;
    while (coef[1] <> coefGuess[1]) and (coef[2] <> coefGuess[2]) or (N_tentatives > 9) do
    begin
        N_tentatives := N_tentatives + 1;
        coefGuess := InverterCurveObj.GetCoefficients(abs(FDCkW) / StorageVars.FkVArating);


        case FState of

            STORE_DISCHARGING:
                FDCkW := QuadSolver(coefGuess[1] / StorageVars.FkVArating, coefGuess[2], -1.0 * abs(Power[1].re * 0.001));
            STORE_CHARGING,
            STORE_IDLING:
                FDCkW := abs(FDCkW) * coefGuess[2] / (1.0 - (coefGuess[1] * abs(FDCkW) / StorageVars.FkVArating));
        end;

        // Final coefficients
        coef := InverterCurveObj.GetCoefficients(abs(FDCkW) / StorageVars.FkVArating);
    end;

    // make sure sign is correct
    if (FState = STORE_IDLING) then
        FDCkW := abs(FDCkW) * -1
    else
        FDCkW := abs(FDCkW) * FState;
end;

function TStorage2Obj.Get_PresentkW: Double;
begin
    Result := Pnominalperphase * 0.001 * Fnphases;
end;

function TStorage2Obj.Get_DCkW: Double;
begin
    ComputeDCkW;
    Result := FDCkW;
end;

function TStorage2Obj.Get_kWDesired: Double;
begin
    case StateDesired of
        STORE_CHARGING:
            Result := -pctkWIn * StorageVars.kWRating / 100.0;
        STORE_DISCHARGING:
            Result := pctkWOut * StorageVars.kWRating / 100.0;
        STORE_IDLING:
            Result := 0.0;
    else
        Result := 0.0;
    end;
end;

function TStorage2Obj.Get_kWTotalLosses: Double;
begin
    Result := kWIdlingLosses + kWInverterLosses + kWChDchLosses;
end;

function TStorage2Obj.Get_InverterLosses: Double;
begin
    Result := 0.0;

    with StorageVars do
    begin
        case StorageState of

            STORE_IDLING:
                Result := abs(Power[1].re * 0.001) - abs(DCkW);
            STORE_CHARGING:
                Result := abs(Power[1].re * 0.001) - abs(DCkW);
            STORE_DISCHARGING:
                Result := DCkW - abs(Power[1].re * 0.001);
        end;
    end;
end;

function TStorage2Obj.Get_kWIdlingLosses: Double;
begin
    if (FState = STORE_IDLING) then
    begin
        Result := abs(DCkW); // For consistency keeping with voltage variations
    end
    else
        Result := Pidling;
end;

function TStorage2Obj.Get_kWChDchLosses: Double;
begin
    Result := 0.0;

    with StorageVars do
    begin
        case StorageState of

            STORE_IDLING:
                Result := 0.0;

            STORE_CHARGING:
                if (abs(DCkW) - Pidling > 0) then
                    Result := (abs(DCkW) - Pidling) * (1.0 - 0.01 * pctChargeEff) // most cases will fall here
                else
                    Result := -1 * (abs(DCkW) - Pidling) * (1.0 / (0.01 * pctDischargeEff) - 1.0);             // exceptional cases when Pidling is higher than DCkW (net effect is that the ideal Storage will be discharged)

            STORE_DISCHARGING:
                Result := (DCkW + Pidling) * (1.0 / (0.01 * pctDischargeEff) - 1.0);
        end;
    end;
end;

procedure TStorage2Obj.Update_EfficiencyFactor;
begin
    with StorageVars do
    begin
        if not Assigned(InverterCurveObj) then
            EffFactor := 1.0
        else
            EffFactor := InverterCurveObj.GetYValue(abs(DCkW) / FkVArating);
    end;
end;

function TStorage2Obj.Get_PresentkV: Double;
begin
    Result := StorageVars.kVStorageBase;
end;

function TStorage2Obj.Get_Presentkvar: Double;
begin
    Result := Qnominalperphase * 0.001 * Fnphases;
end;

procedure TStorage2Obj.InitHarmonics();
// This routine makes a thevenin equivalent behis the reactance spec'd in %R and %X
var
    E, Va: complex;
begin
    YprimInvalid := TRUE;  // Force rebuild of YPrims
    StorageFundamental := ActiveCircuit.Solution.Frequency;  // Whatever the frequency is when we enter here.

    Yeq := Cinv(Cmplx(StorageVars.RThev, StorageVars.XThev));      // used for current calcs  Always L-N

    // Compute reference Thevinen voltage from phase 1 current

    ComputeIterminal();  // Get present value of current

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

    E := Va - Iterminal^[1] * cmplx(StorageVars.Rthev, StorageVars.Xthev);
    StorageVars.Vthevharm := Cabs(E);   // establish base mag and angle
    StorageVars.ThetaHarm := Cang(E);
end;

procedure TStorage2Obj.InitStateVars();
// for going into dynamics mode
var
//    VNeut: Complex;
    VThevPolar: Polar;
    i: Integer;
    V012,
    I012: array[0..2] of Complex;
    Vabc: array[1..3] of Complex;
begin
    YprimInvalid := TRUE;  // Force rebuild of YPrims

    with StorageVars do
    begin
        ZThev := Cmplx(RThev, XThev);
        Yeq := Cinv(ZThev);  // used to init state vars
    end;

    if DynaModel.Exists then   // Checks existence and selects
    begin
        ComputeIterminal();
        ComputeVterminal();
        with StorageVars do
        begin
            NumPhases := Fnphases;
            NumConductors := Fnconds;
            w_grid := twopi * ActiveCircuit.Solution.Frequency;
        end;
        DynaModel.FInit(Vterminal, Iterminal);
    end
    else
    begin
        // Compute nominal Positive sequence voltage behind equivalent filter impedance
        if FState = STORE_DISCHARGING then
            with ActiveCircuit.Solution do
            begin
                ComputeIterminal();

                if FnPhases = 3 then
                begin
                    Phase2SymComp(ITerminal, pComplexArray(@I012));

                    for i := 1 to FNphases do
                        Vabc[i] := NodeV^[NodeRef^[i]];   // Wye Voltage

                    Phase2SymComp(pComplexArray(@Vabc), pComplexArray(@V012));
                    with StorageVars do
                    begin
                        Vthev := V012[1] - I012[1] * ZThev;    // Pos sequence
                        VThevPolar := cToPolar(VThev);
                        VThevMag := VThevPolar.mag;
                        Theta := VThevPolar.ang;  // Initial phase angle
                    end;
                end
                else
                begin   // Single-phase Element
                    for i := 1 to Fnconds do
                        Vabc[i] := NodeV^[NodeRef^[i]];
                    with StorageVars do
                    begin
                        Vthev := VDiff(NodeRef^[1], NodeRef^[2]) - ITerminal^[1] * ZThev;    // Pos sequence
                        VThevPolar := cToPolar(VThev);
                        VThevMag := VThevPolar.mag;
                        Theta := VThevPolar.ang;  // Initial phase angle
                    end;

                end;
            end;
    end;
end;

procedure TStorage2Obj.IntegrateStates();
// dynamics mode integration routine
begin
    // Compute Derivatives and Then integrate
    ComputeIterminal();

    if Dynamodel.Exists then   // Checks for existence and Selects
        DynaModel.Integrate
    else
        with ActiveCircuit.Solution, StorageVars do
        begin
            // Write Dynamics Trace Record
            if DebugTrace then
            begin
                FSWrite(TraceFile, Format('t=%-.5g ', [Dynavars.t]));
                FSWrite(TraceFile, Format(' Flag=%d ', [Dynavars.Iterationflag]));
                FSWriteln(TraceFile);
                FSFlush(TraceFile);
            end;
        end;
end;
function TStorage2Obj.Get_Variable(i: Integer): Double;
// Return variables one at a time
var
    N, k: Integer;
begin
    Result := -9999.99;  // error return value; no state vars
    if i < 1 then
        Exit;
    // for now, report kWhstored and mode
    with StorageVars do
        case i of
            1:
                Result := kWhStored;
            2:
                Result := FState;
            3:
                if not (FState = STORE_DISCHARGING) then
                    Result := 0.0
                else
                    Result := abs(Power[1].re * 0.001);
            4:
                if (FState = STORE_CHARGING) or (FState = STORE_IDLING) then
                    Result := abs(Power[1].re * 0.001)
                else
                    Result := 0;
            5:
                Result := -1 * Power[1].im * 0.001;
            6:
                Result := DCkW;
            7:
                Result := kWTotalLosses; // Present kW charge or discharge loss incl idle losses
            8:
                Result := kWInverterLosses; // Inverter Losses
            9:
                Result := kWIdlingLosses; // Present kW Idling Loss
            10:
                Result := kWChDchLosses;  // Charge/Discharge Losses
            11:
                Result := kWhStored - kWhBeforeUpdate;
            12:
            begin
                Update_EfficiencyFactor;
                Result := EffFactor;  //Old: Result    := Get_EfficiencyFactor;
            end;
            13:
                if (InverterON) then
                    Result := 1.0
                else
                    Result := 0.0;
            14:
                Result := Vreg;
            15:
                Result := Vavg;
            16:
                Result := VVOperation;
            17:
                Result := VWOperation;
            18:
                Result := DRCOperation;
            19:
                Result := VVDRCOperation;
            20:
                Result := WPOperation;
            21:
                Result := WVOperation;
            22:
                Result := Get_kWDesired;
            23:
                if not (VWMode) then
                    Result := 9999
                else
                    Result := kWRequested;
            24:
                Result := pctkWrated * kWrating;
            25:
                if (kVA_exceeded) then
                    Result := 1.0
                else
                    Result := 0.0;
        else
        begin
            if UserModel.Exists then   // Checks for existence and Selects
            begin
                N := UserModel.FNumVars;
                k := (i - NumStorage2Variables);
                if k <= N then
                begin
                    Result := UserModel.FGetVariable(k);
                    Exit;
                end;
            end;
            if DynaModel.Exists then  // Checks for existence and Selects
            begin
                N := DynaModel.FNumVars;
                k := (i - NumStorage2Variables);
                if k <= N then
                begin
                    Result := DynaModel.FGetVariable(k);
                    Exit;
                end;
            end;
        end;
        end;
end;

procedure TStorage2Obj.Set_Variable(i: Integer; Value: Double);
var
    N, k: Integer;
begin
    if i < 1 then
        Exit;  // No variables to set

    with StorageVars do
        case i of
            1:
                kWhStored := Value;
            2:
                Fstate := Trunc(Value);
            3..13: ; // Do Nothing; read only
            14:
                Vreg := Value;
            15:
                Vavg := Value;
            16:
                VVOperation := Value;
            17:
                VWOperation := Value;
            18:
                DRCOperation := Value;
            19:
                VVDRCOperation := Value;
            20:
                WPOperation := Value;
            21:
                WVOperation := Value;
            22..25: ; // Do Nothing; read only

        else
            begin
                if UserModel.Exists then    // Checks for existence and Selects
                begin
                    N := UserModel.FNumVars;
                    k := (i - NumStorage2Variables);
                    if k <= N then
                    begin
                        UserModel.FSetVariable(k, Value);
                        Exit;
                    end;
                end;
                if DynaModel.Exists then     // Checks for existence and Selects
                begin
                    N := DynaModel.FNumVars;
                    k := (i - NumStorage2Variables);
                    if k <= N then
                    begin
                        DynaModel.FSetVariable(k, Value);
                        Exit;
                    end;
                end;
            end;
        end;
end;

procedure TStorage2Obj.GetAllVariables(States: pDoubleArray);
var
    i: Integer;
begin
    for i := 1 to NumStorage2Variables do
        States^[i] := Variable[i];

    if UserModel.Exists then
    begin    // Checks for existence and Selects
        // N := UserModel.FNumVars;
        UserModel.FGetAllVars(pDoubleArray(@States^[NumStorage2Variables + 1]));
    end;
    if DynaModel.Exists then
    begin    // Checks for existence and Selects
        // N := UserModel.FNumVars;
        DynaModel.FGetAllVars(pDoubleArray(@States^[NumStorage2Variables + 1]));
    end;
end;

function TStorage2Obj.NumVariables: Integer;
begin
    Result := NumStorage2Variables;

     // Exists does a check and then does a Select
    if UserModel.Exists then
        Result := Result + UserModel.FNumVars;
    if DynaModel.Exists then
        Result := Result + DynaModel.FNumVars;
end;

function TStorage2Obj.VariableName(i: Integer): String;
const
    BuffSize = 255;
var
    n,
    i2: Integer;
    Buff: array[0..BuffSize] of AnsiChar;
    pName: pAnsichar;

begin
    Result := '';

    if i < 1 then
        Exit;  // Someone goofed

    case i of
        1:
            Result := 'kWh';
        2:
            Result := 'State';
        3:
            Result := 'kWOut';
        4:
            Result := 'kWIn';
        5:
            Result := 'kvarOut';
        6:
            Result := 'DCkW';
        7:
            Result := 'kWTotalLosses';
        8:
            Result := 'kWInvLosses';
        9:
            Result := 'kWIdlingLosses';
        10:
            Result := 'kWChDchLosses';
        11:
            Result := 'kWh Chng';
        12:
            Result := 'InvEff';
        13:
            Result := 'InverterON';
        14:
            Result := 'Vref';
        15:
            Result := 'Vavg (DRC)';
        16:
            Result := 'VV Oper';
        17:
            Result := 'VW Oper';
        18:
            Result := 'DRC Oper';
        19:
            Result := 'VV_DRC Oper';
        20:
            Result := 'WP Oper';
        21:
            Result := 'WV Oper';
        22:
            Result := 'kWDesired';
        23:
            Result := 'kW VW Limit';
        24:
            Result := 'Limit kWOut Function';
        25:
            Result := 'kVA Exceeded';
    else
        begin
            if UserModel.Exists then    // Checks for existence and Selects
            begin
                pName := PAnsiChar(@Buff);
                n := UserModel.FNumVars;
                i2 := i - NumStorage2Variables;
                if i2 <= n then
                begin
                    UserModel.FGetVarName(i2, pName, BuffSize);
                    Result := String(pName);
                    Exit;
                end;
            end;
            if DynaModel.Exists then   // Checks for existence and Selects
            begin
                pName := PAnsiChar(@Buff);
                n := DynaModel.FNumVars;
                i2 := i - NumStorage2Variables; // Relative index
                if i2 <= n then
                begin
                    DynaModel.FGetVarName(i2, pName, BuffSize);
                    Result := String(pName);
                    Exit;
                end;
            end;
        end;
    end;
end;

procedure TStorage2Obj.MakePosSequence();
var
    newkW, newPF, V: Double;
    oldPhases, changes: Integer;
begin
    // Make sure voltage is line-neutral
    if (Fnphases > 1) or (connection <> 0) then
        V := StorageVars.kVStorageBase / SQRT3
    else
        V := StorageVars.kVStorageBase;

    oldPhases := FnPhases;
    changes := 3;
    if oldPhases > 1 then
    begin
        newkW := StorageVars.kWrating / Fnphases;
        newPF := PFNominal;
        changes := changes + 2;
    end;
    SetInteger(ord(TProp.Phases), 1);
    SetInteger(ord(TProp.conn), 0);
    SetDouble(ord(TProp.kV), V);
    if oldPhases > 1 then
    begin
        SetDouble(ord(TProp.kWrated), newkW);
        SetDouble(ord(TProp.PF), newPF);
    end;
    EndEdit(changes);

    inherited;   // write out other properties
end;

procedure TStorage2Obj.Set_ConductorClosed(Index: Integer; Value: Boolean);
begin
    inherited;

    // Just turn Storage element on or off;
    if Value then
        Storage2ObjSwitchOpen := FALSE
    else
        Storage2ObjSwitchOpen := TRUE;
end;

procedure TStorage2Obj.kWOut_Calc;
var
    limitkWpct: Double;
begin
    with StorageVars do
    begin
        FVWStateRequested := FALSE;

        if FState = STORE_DISCHARGING then
            limitkWpct := kWrating * pctkWrated
        else
            limitkWpct := kWrating * pctkWrated * -1;

//          if VWmode and (FState = STORE_DISCHARGING) then if (abs(kwRequested) < abs(limitkWpct)) then limitkWpct :=  kwRequested * sign(kW_Out);
          // VW works only if element is not in idling state.
          // When the VW is working in the 'limiting' region, kWRequested will be positive.
          // When in 'requesting' region, it will be negative.
        if VWmode and not (FState = STORE_IDLING) then
        begin
            if (kWRequested >= 0.0) and (abs(kwRequested) < abs(limitkWpct)) then  // Apply VW limit
            begin
                if FState = STORE_DISCHARGING then
                    limitkWpct := kwRequested
                else
                    limitkWpct := -1 * kwRequested;
            end
            else
            if kWRequested < 0.0 then // IEEE 1547 Requesting Region (not fully implemented)
            begin
                if FState = STORE_DISCHARGING then
                begin
                    if (kWhStored < kWhRating) then
                    begin  // let it charge only if enough not fully charged
                        FState := STORE_CHARGING;
                        kW_out := kWRequested;
                    end
                    else
                    begin
                        FState := STORE_IDLING;
                        kW_out := -kWOutIdling;
                    end;
                end
                else  // Charging
                begin
                    if (kWhStored > kWhReserve) then
                    begin  // let it charge only if enough not fully charged
                        Fstate := STORE_DISCHARGING;
                        kW_out := -1 * kWRequested;
                    end
                    else
                    begin
                        FState := STORE_IDLING;
                        kW_out := -kWOutIdling;
                    end;


                end;
                FStateChanged := TRUE;
                FVWStateRequested := TRUE;

                // Update limitkWpct because state might have been changed
                if FState = STORE_DISCHARGING then
                    limitkWpct := kWrating * pctkWrated
                else
                    limitkWpct := kWrating * pctkWrated * -1;
            end;
        end;

        if (limitkWpct > 0) and (kW_Out > limitkWpct) then
            kW_Out := limitkWpct
        else
        if (limitkWpct < 0) and (kW_Out < limitkWpct) then
            kW_Out := limitkWpct;
    end;
end;

procedure TStorage2Obj.Set_kW(const Value: Double);
begin
    if Value > 0 then
    begin
        FState := STORE_DISCHARGING;
        pctkWOut := Value / StorageVars.kWRating * 100.0;
    end
    else
    if Value < 0 then
    begin
        FState := STORE_CHARGING;
        pctkWIn := abs(Value) / StorageVars.kWRating * 100.0;
    end
    else
    begin
        FState := STORE_IDLING;
    end;
end;

procedure TStorage2Obj.Set_Maxkvar(const Value: Double);
begin
    StorageVars.Fkvarlimit := Value;
    SetAsNextSeq(ord(TProp.kvarMax));
end;

procedure TStorage2Obj.Set_Maxkvarneg(const Value: Double);
begin
    StorageVars.Fkvarlimitneg := Value;
    SetAsNextSeq(ord(TProp.kvarMaxAbs));
end;

procedure TStorage2Obj.Set_kVARating(const Value: Double);
begin
    StorageVars.FkVARating := Value;
    SetAsNextSeq(ord(TProp.kVA));
end;

procedure TStorage2Obj.Set_PowerFactor(const Value: Double);
begin
    PFNominal := Value;
    varMode := VARMODEPF;
end;

function TStorage2Obj.Get_kW: Double;
begin
    case Fstate of
        STORE_CHARGING:
            Result := -pctkWIn * StorageVars.kWRating / 100.0;
        STORE_DISCHARGING:
            Result := pctkWOut * StorageVars.kWRating / 100.0;
        STORE_IDLING:
            Result := -kWOutIdling;
    else
        Result := 0.0;
    end;
end;

procedure TStorage2Obj.Set_StorageState(const Value: Integer);
var
    SavedState: Integer;
begin
    SavedState := Fstate;

     // Decline if Storage is at its limits ; set to idling instead

    with StorageVars do
        case Value of

            STORE_CHARGING:
            begin
                if kWhStored < kWhRating then
                    Fstate := Value
                else
                    Fstate := STORE_IDLING;   // all charged up
            end;

            STORE_DISCHARGING:
            begin
                if kWhStored > kWhReserve then
                    Fstate := Value
                else
                    Fstate := STORE_IDLING;  // not enough Storage to discharge
            end;
        else
            Fstate := STORE_IDLING;
        end;

    if SavedState <> Fstate then
        FStateChanged := TRUE;

     //---DEBUG--- WriteDLLDebugFile(Format('t=%.8g, ---State Set To %s', [ActiveCircuit.Solution.dblHour, StateToStr ]));
end;

procedure TStorage2Obj.SetDragHandRegister(Reg: Integer; const Value: Double);
begin
    if Value > Registers[reg] then
        Registers[Reg] := Value;
end;

finalization    StateEnum.Free;
    DispatchModeEnum.Free;
end.
