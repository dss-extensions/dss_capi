unit Storage;

// ----------------------------------------------------------
// Copyright (c) 2018-2023, Paulo Meira, DSS-Extensions contributors
// Copyright (c) 2009-2016, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

uses
    Classes,
    StoreUserModel,
    DSSClass,
    InvBasedPCE,
    ucmatrix,
    UComplex, DSSUcomplex,
    LoadShape,
    Spectrum,
    ArrayDef,
    Dynamics,
    XYCurve,
    InvDynamics,
    MathUtil;

const
    NumStorageRegisters = 6; // Number of energy meter registers
    NumBaseStorageVariables = 25;
    NumStorageVariables = NumBaseStorageVariables + NumInvDynVars; // Number of state variables
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
    TStorageProp = (
        INVALID = 0,
        Phases,
        Bus1,
        kV, // propKV
        Conn, // propCONNECTION
        kW, // propKW
        kvar, // propKVAR
        PF, // propPF
        kVA, // propKVA
        pctCutIn, // propCutin
        pctCutOut, // propCutout
        EffCurve, // propInvEffCurve
        VarFollowInverter, // propVarFollowInverter
        kvarMax, // propkvarLimit
        kvarMaxAbs, // propkvarLimitneg
        WattPriority, // propPpriority
        PFPriority, // propPFPriority
        pctPMinNoVars, // propPminNoVars
        pctPMinkvarMax, // propPminkvarLimit
        kWRated, // propKWRATED
        pctkWRated, // proppctkWrated
        kWhRated, // propKWHRATED
        kWhStored, // propKWHSTORED
        pctStored, // propPCTSTORED
        pctReserve, // propPCTRESERVE
        State, // propSTATE
        pctDischarge, // propPCTKWOUT
        pctCharge, // propPCTKWIN
        pctEffCharge, // propCHARGEEFF
        pctEffDischarge, // propDISCHARGEEFF
        pctIdlingkW, // propIDLEKW
        
        pctIdlingkvar, //= 31, // propIDLEKVAR was deprecated, reintroduced for v0.12.2; TODO: TO BE REMOVED AGAIN LATER
        
        pctR, // propPCTR
        pctX, // propPCTX
        Model, // propMODEL
        VMinpu, // propVMINPU
        VMaxpu, // propVMAXPU
        Balanced, // propBalanced
        LimitCurrent, // propLimited
        Yearly, // propYEARLY
        Daily, // propDAILY
        Duty, // propDUTY
        DispMode, // propDISPMODE
        DischargeTrigger, // propDISPOUTTRIG
        ChargeTrigger, // propDISPINTRIG
        TimeChargeTrig, // propCHARGETIME
        cls, // propCLASS
        DynaDLL, // propDynaDLL
        DynaData, // propDynaData
        UserModel, // propUSERMODEL
        UserData, // propUSERDATA
        DebugTrace, // propDEBUGTRACE

        kVDC, // propkVDC
        Kp, // propkp
        PITol, // propCtrlTol
        SafeVoltage, // propSMT
        SafeMode, // propSM
        
        DynamicEq, // propDynEq
        DynOut, // propDynOut

        ControlMode, // propGFM
        AmpLimit,
        AmpLimitGain
    );
{$SCOPEDENUMS OFF}

    // Struct to pass basic data to user-written DLLs
    TStorageVars = {$IFNDEF DSS_CAPI_NO_PACKED_RECORDS}packed{$ENDIF} record

        kWrating: Double;
        kWhRating: Double;
        kWhStored: Double;
        kWhReserve: Double;

        ChargeEff: Double;
        DisChargeEff: Double;
        kVStorageBase: Double;
        RThev: Double;
        XThev: Double;

        // Inverter Related Properties
        FkVArating: Double;
        Fkvarlimit: Double;
        Fkvarlimitneg: Double;
        P_Priority: LongBool;
        PF_Priority: LongBool;
        pctkWrated: Double;
        EffFactor: Double;


        // Interaction with InvControl
        Vreg: Double;
        Vavg: Double;
        VVOperation: Double;
        VWOperation: Double;
        DRCOperation: Double;
        VVDRCOperation: Double;
        WPOperation: Double;
        WVOperation: Double;
//        kW_out_desired  :Double;


        // Dynamics variables
        Vthev: Complex;  // Thevenin equivalent voltage (complex) for dynamic model
        ZThev: Complex;
        Vthevharm: Double; // Thevenin equivalent voltage mag and angle reference for Harmonic model
        Thetaharm: Double; // Thevenin equivalent voltage mag and angle reference for Harmonic model
        VthevMag: Double; // Thevenin equivalent voltage for dynamic model
        Theta: Double; // Power angle between voltage and current
        w_grid: Double; // Grid frequency
        TotalLosses: Double;
        IdlingLosses: Double;

        // 32-bit integers
        NumPhases, // Number of phases
        NumConductors, // Total Number of conductors (wye-connected will have 4)
        Conn: Integer;   // 0 = wye; 1 = Delta
    end;

    TStorage = class(TInvBasedPCEClass)
    PROTECTED
        cBuffer: TCBuffer24;  // Temp buffer for calcs  24-phase Storage element?

        procedure DefineProperties; override;
    PUBLIC
        RegisterNames: array[1..NumStorageRegisters] of String;

        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;

        procedure ResetRegistersAll;
        procedure SampleAll();
        procedure UpdateAll();

    end;

    TStorageObj = class(TInvBasedPCE)
    PRIVATE
        PIdling: Double;
        YeqDischarge: Complex;   // equiv at rated power of Storage element only
        MaxDynPhaseCurrent: Double;

        FState: Integer;
        StorageSolutionCount: Integer;
        StorageFundamental: Double; // Thevenin equivalent voltage mag and angle reference for Harmonic model
        StorageObjSwitchOpen: Boolean;

        FDCkW: Double;

        kVA_exceeded: Boolean;

        kVASet: Boolean;

        IsUserModel: Boolean;
        UserModel: TStoreUserModel;   // User-Written Models
        DynaModel: TStoreDynaModel;

        DynaModelNameStr, DynaModelEditStr: String;

//        VBase           :Double;  // Base volts suitable for computing currents  made public

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

        procedure DoConstantPQStorageObj();
        procedure DoConstantZStorageObj();
        procedure DoDynamicMode();
        procedure DoHarmonicMode();
        procedure DoUserModel();
        procedure DoDynaModel();
        procedure DoGFM_Mode();

        procedure Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);
        procedure SetDragHandRegister(Reg: Integer; const Value: Double);

        procedure WriteTraceRecord(const s: String);

        procedure CheckStateTriggerLevel(Level: Double);
        function CheckIfDelivering(): Boolean;
        procedure UpdateStorage();    // Update Storage elements based on present kW and IntervalHrs variable
        function NormalizeToTOD(h: Integer; sec: Double): Double;

        function Get_PresentkW: Double;
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
        StateChanged: Boolean;
        StorageVars: TStorageVars;

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

        CutOutkWAC: Double;  // CutInkW  reflected to the AC side of the inverter
        CutInkWAC: Double;   // CutOutkW reflected to the AC side of the inverter


        FVWStateRequested: Boolean;   // TEST Flag indicating if VW function has requested a specific state in last control iteration

        StorageClass: Integer;

        Registers, Derivatives: array[1..NumStorageRegisters] of Double;

        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure Set_ConductorClosed(Index: Integer; Value: Boolean); OVERRIDE;
        procedure RecalcElementData(); OVERRIDE;
        procedure CalcYPrim(); OVERRIDE;

        function InjCurrents(): Integer; OVERRIDE;
        function NumVariables(): Integer; OVERRIDE;
        procedure GetAllVariables(var States: ArrayOfDouble); OVERRIDE;
        function Get_Variable(i: Integer): Double; OVERRIDE;
        procedure Set_Variable(i: Integer; Value: Double); OVERRIDE;
        function VariableName(i: Integer): String; OVERRIDE;

        procedure Set_Maxkvar(const Value: Double);
        procedure Set_Maxkvarneg(const Value: Double);

        procedure SetNominalDEROutput(); OVERRIDE;

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

        function IsStorage(): Boolean; OVERRIDE;
        function GetPFPriority(): Boolean; OVERRIDE;
        procedure SetPFPriority(value: Boolean); OVERRIDE;
        function CheckOLInverter(): Boolean; OVERRIDE;
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
    TObj = TStorageObj;
    TProp = TStorageProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    
    StateEnum, DispatchModeEnum: TDSSEnum;

constructor TStorage.Create(dssContext: TDSSContext);
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

destructor TStorage.Destroy;
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

procedure ObjSetDynOutput(obj: TObj; variable: String);
begin
    obj.SetDynOutput(variable);
end;

function ObjGetDynOutputStr(obj: TObj): String;
begin
    Result := obj.GetDynOutputStr();
end;

procedure TStorage.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    SpecSetNames := ArrayOfString.Create(
        'kW, PF',
        'kW, kvar'
    );
    SpecSets := TSpecSets.Create(
        TSpecSet.Create(ord(TProp.kW), ord(TProp.PF)),
        TSpecSet.Create(ord(TProp.kW), ord(TProp.kvar))
    );

    // strings
    PropertyType[ord(TProp.UserModel)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.UserModel)] := ptruint(@obj.UserModelNameStr);
    PropertyFlags[ord(TProp.UserModel)] := [TPropertyFlag.IsFilename];

    PropertyType[ord(TProp.UserData)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.UserData)] := ptruint(@obj.UserModelEditStr);

    PropertyType[ord(TProp.DynaDLL)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.DynaDLL)] := ptruint(@obj.DynaModelNameStr);
    PropertyFlags[ord(TProp.DynaDLL)] := [TPropertyFlag.IsFilename];

    PropertyType[ord(TProp.DynaData)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.DynaData)] := ptruint(@obj.DynaModelEditStr);

    PropertyType[ord(TProp.DynOut)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.DynOut)] := 1; // dummy
    PropertyWriteFunction[ord(TProp.DynOut)] := @ObjSetDynOutput;
    PropertyReadFunction[ord(TProp.DynOut)] := @ObjGetDynOutputStr;
    PropertyFlags[ord(TProp.DynOut)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.ReadByFunction];

    // enum properties
    PropertyType[ord(TProp.conn)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.conn)] := ptruint(@obj.Connection);
    PropertyOffset2[ord(TProp.conn)] := PtrInt(DSS.ConnectionEnum);

    PropertyType[ord(TProp.State)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.State)] := ptruint(@obj.FState);
    PropertyOffset2[ord(TProp.State)] := PtrInt(StateEnum);
    PropertyFlags[ord(TProp.State)] := [TPropertyFlag.NoDefault];

    PropertyType[ord(TProp.DispMode)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.DispMode)] := ptruint(@obj.DispatchMode);
    PropertyOffset2[ord(TProp.DispMode)] := PtrInt(DispatchModeEnum);

    PropertyType[ord(TProp.ControlMode)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.ControlMode)] := ptruint(@obj.GFM_Mode);
    PropertyOffset2[ord(TProp.ControlMode)] := PtrInt(DSS.InvControlModeEnum);

    // bus properties
    PropertyType[ord(TProp.bus1)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus1)] := 1;
    PropertyFlags[ord(TProp.bus1)] := [TPropertyFlag.Required];

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

    PropertyType[ord(TProp.SafeMode)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.SafeMode)] := ptruint(@obj.dynVars.SafeMode);
    PropertyFlags[ord(TProp.SafeMode)] := [TPropertyFlag.SilentReadOnly];

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
    PropertyType[ord(TProp.DynamicEq)] := TPropertyType.DSSObjectReferenceProperty;

    PropertyOffset[ord(TProp.yearly)] := ptruint(@obj.YearlyShapeObj);
    PropertyOffset[ord(TProp.daily)] := ptruint(@obj.DailyShapeObj);
    PropertyOffset[ord(TProp.duty)] := ptruint(@obj.DutyShapeObj);
    PropertyOffset[ord(TProp.DynamicEq)] := ptruint(@obj.DynamicEqObj);

    PropertyOffset2[ord(TProp.yearly)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.daily)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.duty)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.DynamicEq)] := ptruint(DSS.DynamicExpClass);

    PropertyType[ord(TProp.EffCurve)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.EffCurve)] := ptruint(@obj.InverterCurveObj);
    PropertyOffset2[ord(TProp.EffCurve)] := ptruint(DSS.XYCurveClass);

    PropertyScale[ord(TProp.pctkWrated)] := 0.01;
    PropertyOffset[ord(TProp.pctkWrated)] := ptruint(@obj.StorageVars.pctkWrated);

    // adv doubles
    PropertyOffset[ord(TProp.kvarMax)] := ptruint(@obj.StorageVars.Fkvarlimit);
    PropertyOffset[ord(TProp.kvarMaxAbs)] := ptruint(@obj.StorageVars.Fkvarlimitneg);
    PropertyFlags[ord(TProp.kvarMax)] := [TPropertyFlag.Transform_Abs, TPropertyFlag.DynamicDefault];
    PropertyFlags[ord(TProp.kvarMaxAbs)] := [TPropertyFlag.Transform_Abs, TPropertyFlag.DynamicDefault];

    PropertyType[ord(TProp.pctIdlingkvar)] := TPropertyType.DeprecatedAndRemoved; //TODO: fully remove
    PropertyDeprecatedMessage[ord(TProp.pctIdlingkvar)] := '"%Idlingkvar" was deprecated in 2020. It does nothing since then; please update your scripts.';

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
    PropertyFlags[ord(TProp.kWrated)] := [TPropertyFlag.Units_kW];
    
    PropertyOffset[ord(TProp.kWhrated)] := ptruint(@obj.StorageVars.kWhrating);
    PropertyFlags[ord(TProp.kWhrated)] := [TPropertyFlag.Units_kWh];
    
    PropertyOffset[ord(TProp.kWhstored)] := ptruint(@obj.StorageVars.kWhstored);
    PropertyFlags[ord(TProp.kWhstored)] := [TPropertyFlag.DynamicDefault, TPropertyFlag.NonNegative, TPropertyFlag.Units_kWh];

    PropertyOffset[ord(TProp.pctreserve)] := ptruint(@obj.pctReserve);
    PropertyOffset[ord(TProp.pctPminNoVars)] := ptruint(@obj.FpctPminNoVars);
    PropertyOffset[ord(TProp.pctPminkvarMax)] := ptruint(@obj.FpctPminkvarLimit);
    
    PropertyOffset[ord(TProp.TimeChargeTrig)] := ptruint(@obj.ChargeTime);
    PropertyFlags[ord(TProp.TimeChargeTrig)] := [TPropertyFlag.Units_ToD_hour];
    
    PropertyOffset[ord(TProp.pf)] := ptruint(@obj.PFnominal);
    PropertyFlags[ord(TProp.pf)] := [TPropertyFlag.RequiredInSpecSet];

    PropertyOffset[ord(TProp.kVA)] := ptruint(@obj.StorageVars.FkVArating);
    PropertyFlags[ord(TProp.kVA)] := [TPropertyFlag.Units_kVA];
    
    PropertyOffset[ord(TProp.kV)] := ptruint(@obj.StorageVars.kVStorageBase);
    PropertyFlags[ord(TProp.kV)] := [TPropertyFlag.Required, TPropertyFlag.Units_kV, TPropertyFlag.NonNegative];

    PropertyOffset[ord(TProp.kvar)] := ptruint(@obj.kvarRequested);
    PropertyReadFunction[ord(TProp.kvar)] := @Getkvar;
    PropertyFlags[ord(TProp.kvar)] := [TPropertyFlag.ReadByFunction, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.Units_kvar];

    PropertyType[ord(TProp.pctstored)] := TPropertyType.DoubleProperty;
    PropertyOffset[ord(TProp.pctstored)] := 1;
    PropertyWriteFunction[ord(TProp.pctstored)] := @SetPctStored;
    PropertyReadFunction[ord(TProp.pctstored)] := @GetPctStored;
    PropertyFlags[ord(TProp.pctstored)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.ReadByFunction];

    PropertyType[ord(TProp.kW)] := TPropertyType.DoubleProperty;
    PropertyOffset[ord(TProp.kW)] := ptruint(@obj.kW_out);
    PropertyWriteFunction[ord(TProp.kW)] := @SetkW;
    PropertyFlags[ord(TProp.kW)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.Units_kW];

    PropertyOffset[ord(TProp.kVDC)] := ptruint(@obj.dynVars.RatedVDC);
    PropertyScale[ord(TProp.kVDC)] := 1000;
    PropertyFlags[ord(TProp.kVDC)] := [TPropertyFlag.Units_kV];

    PropertyOffset[ord(TProp.kP)] := ptruint(@obj.dynVars.kP);
    PropertyScale[ord(TProp.kP)] := 1.0 / 1000.0;

    PropertyOffset[ord(TProp.PITol)] := ptruint(@obj.dynVars.CtrlTol);
    PropertyScale[ord(TProp.PITol)] := 1.0 / 100.0;

    PropertyOffset[ord(TProp.SafeVoltage)] := ptruint(@obj.dynVars.SMThreshold);
    
    PropertyOffset[ord(TProp.AmpLimit)] := ptruint(@obj.dynVars.ILimit);
    PropertyFlags[ord(TProp.AmpLimit)] := [TPropertyFlag.NoDefault];

    PropertyOffset[ord(TProp.AmpLimitGain)] := ptruint(@obj.dynVars.VError);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TStorage.NewObject(const ObjName: String; Activate: Boolean): Pointer;
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

procedure TStorage.UpdateAll();
var
    i: Integer;
begin
    for i := 1 to ElementList.Count do
        with TStorageObj(ElementList.Get(i)) do
            if Enabled then
                UpdateStorage();
end;

procedure TStorageObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
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

                VBaseMin := Vminpu * VBase;
                VBaseMax := Vmaxpu * VBase;

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
                UserModel.Edit(UserModelEditStr);
        ord(TProp.DynaDLL):
        begin
            DynaModel.Name := DynaModelNameStr; 
            IsUserModel := DynaModel.Exists;
        end;
        ord(TProp.DynaData):
            if DynaModel.Exists then
                DynaModel.Edit(DynaModelEditStr);

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
                for i := 1 to NumVariables() do
                    FSWrite(Tracefile, ', ' + VariableName(i));

                FSWrite(TraceFile, ',Vthev, Theta');
                FSWriteln(TraceFile);
                FSFlush(Tracefile);
            end
            else
            begin
                FreeAndNil(TraceFile);
            end;

        ord(TProp.ControlMode):
        begin
            if GFM_mode then
                dynVars.ResetIBR := FALSE;
            YprimInvalid := TRUE;
        end;
        ord(TProp.DynamicEq):
            if DynamicEqObj <> NIL then
                SetLength(DynamicEqVals, DynamicEqObj.NVariables);            
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TStorage.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
begin
    with TObj(ptr) do
    begin
        RecalcElementData;
        YPrimInvalid := TRUE;
        Exclude(Flags, Flg.EditionActive);
    end;
    Result := True;
end;

procedure TStorageObj.MakeLike(OtherPtr: Pointer);
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
    VBaseMin := Other.VBaseMin;
    VBaseMax := Other.VBaseMax;
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
    stateChanged := Other.stateChanged;
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
    AVRMode := Other.AVRMode;

    UserModel.Name := Other.UserModel.Name;
    DynaModel.Name := Other.DynaModel.Name;
    UserModelNameStr := Other.UserModelNameStr;
    DynaModelNameStr := Other.DynaModelNameStr;
    
    //TODO: missing from upstream
    dynVars.RatedVDC := Other.dynVars.RatedVDC;
    dynVars.SMThreshold := Other.dynVars.SMThreshold;
    dynVars.SafeMode := Other.dynVars.SafeMode;
    dynVars.kP := Other.dynVars.kP;
    dynVars.ResetIBR := Other.dynVars.ResetIBR;
    GFM_Mode := Other.GFM_Mode;
    //TODO: copy PICtrl?

    //TODO: this doesn't copy the parameters of the user models

    IsUserModel := Other.IsUserModel;
    ForceBalanced := Other.ForceBalanced;
    CurrentLimited := Other.CurrentLimited;
end;

procedure TStorage.ResetRegistersAll;  // Force all EnergyMeters in the circuit to reset
var
    idx: Integer;
begin
    idx := First;
    while idx > 0 do
    begin
        TStorageObj(GetActiveObj).ResetRegisters;
        idx := Next;
    end;
end;

procedure TStorage.SampleAll();  // Force all Storage elements in the circuit to take a sample
var
    i: Integer;
begin
    for i := 1 to ElementList.Count do
        with TStorageObj(ElementList.Get(i)) do
            if Enabled then
                TakeSample();
end;

constructor TStorageObj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := AnsiLowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType; // + Storage_ELEMENT;  // In both PCelement and Storageelement list
    TraceFile := nil;

    FNphases := 3;
    Fnconds := 4;  // defaults to wye
    Yorder := 0;  // To trigger an initial allocation
    Nterms := 1;  // forces allocations

    Connection := 0;    // Wye (star)
    VoltageModel := 1;  // Typical fixed kW negative load
    StorageClass := 1;

    StorageSolutionCount := -1;  // For keep track of the present solution in Injcurrent calcs
    YPrimOpenCond := NIL;

    StorageVars.kVStorageBase := 12.47;
    VBase := 7200.0;
    Vminpu := 0.90;
    Vmaxpu := 1.10;
    VBaseMin := Vminpu * Vbase;
    VBaseMax := Vmaxpu * Vbase;
    Yorder := Fnterms * Fnconds;

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
    with dynVars do
    begin
        RatedVDC := 8000;
        SMThreshold := 80;
        SafeMode := FALSE;
        kP := 0.00001;
        ResetIBR := FALSE;
    end;

    FDCkW := 25.0;

    FpctCutIn := 0.0;
    FpctCutOut := 0.0;

    FpctPminNoVars := 0.0; // Deactivated by default
    FpctPminkvarLimit := 0.0; // Deactivated by default

    pf_wp_nominal := 1.0;

    // Output rating stuff
    kvar_out := 0.0;
     // removed kvarBase     := kvar_out;     // initialize
    PFNominal := 1.0;

    pctR := 0.0;
    pctX := 50.0;

    // Make the StorageVars struct as public
    PublicDataStruct := @StorageVars;
    PublicDataSize := SizeOf(TStorageVars);

    IsUserModel := FALSE;
    UserModel := TStoreUserModel.Create(DSS);
    DynaModel := TStoreDynaModel.Create(DSS);
    UserModelNameStr := '';
    UserModelEditStr := '';
    DynaModelNameStr := '';
    DynaModelEditStr := '';

    FState := STORE_IDLING;  // Idling and fully charged
    StateChanged := TRUE;  // Force building of YPrim
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
    StorageObjSwitchOpen := FALSE;
    SpectrumObj := NIL; // override base class
    VWMode := FALSE;
    VVMode := FALSE;
    DRCMode := FALSE;
    WPMode := FALSE;
    WVMode := FALSE;
    AVRMode := FALSE;

    RecalcElementData();
end;

destructor TStorageObj.Destroy;
begin
    YPrimOpenCond.Free;
    UserModel.Free;
    DynaModel.Free;
    FreeAndNil(TraceFile);
    inherited Destroy;
end;

procedure TStorageObj.CalcDailyMult(Hr: Double);
begin
    if (DailyShapeObj <> NIL) then
    begin
        ShapeFactor := DailyShapeObj.GetMultAtHour(Hr);
    end
    else
        ShapeFactor := CDOUBLEONE;  // Default to no  variation

    CheckStateTriggerLevel(ShapeFactor.re);   // last recourse
end;

procedure TStorageObj.CalcDutyMult(Hr: Double);
begin
    if DutyShapeObj <> NIL then
    begin
        ShapeFactor := DutyShapeObj.GetMultAtHour(Hr);
        CheckStateTriggerLevel(ShapeFactor.re);
    end
    else
        CalcDailyMult(Hr);  // Default to Daily Mult If no duty curve specified
end;

procedure TStorageObj.CalcYearlyMult(Hr: Double);
begin
    if YearlyShapeObj <> NIL then
    begin
        ShapeFactor := YearlyShapeObj.GetMultAtHour(Hr);
        CheckStateTriggerLevel(ShapeFactor.re);
    end
    else
        CalcDailyMult(Hr);  // Defaults to Daily curve
end;

procedure TStorageObj.RecalcElementData();
begin
    VBaseMin := VMinPu * VBase;
    VBaseMax := VMaxPu * VBase;

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

    SetNominalDEROutput();

    // Initialize to Zero - defaults to PQ Storage element
    // Solution object will reset after circuit modifications

    Reallocmem(InjCurrent, SizeOf(InjCurrent[1]) * Yorder);

    // Update any user-written models
    if Usermodel.Exists then
        UserModel.FUpdateModel;  // Checks for existence and Selects
    if Dynamodel.Exists then
        Dynamodel.FUpdateModel;  // Checks for existence and Selects
end;

procedure TStorageObj.SetNominalDEROutput();
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
                // Yeq no longer used for anything other than this calculation of YEQ_Min, YEQ_Max and
                // constant Z power flow model
                Yeq := Cmplx(Pnominalperphase, -Qnominalperphase) / Sqr(Vbase);   // Vbase must be L-N for 3-phase
                if (Vminpu <> 0.0) then
                    YEQ_Min := Yeq / sqr(Vminpu)  // at 95% voltage
                else
                    YEQ_Min := Yeq; // Always a constant Z model

                if (Vmaxpu <> 0.0) then
                    YEQ_Max := Yeq / Sqr(Vmaxpu)   // at 105% voltage
                else
                    YEQ_Max := Yeq;
            end;
            // Like Model 7 generator, max current is based on amount of current to get out requested power at min voltage
            with StorageVars do
            begin
                PhaseCurrentLimit := Cmplx(Pnominalperphase, Qnominalperphase) / VBaseMin;
                MaxDynPhaseCurrent := Cabs(PhaseCurrentLimit);
            end;

            // When we leave here, all the Yeq's are in L-N values

        end;  // If  NOT (IsDynamicModel or IsHarmonicModel)
    end; // With ActiveCircuit

   // If Storage element state changes, force re-calc of Y matrix
    if StateChanged then
    begin
        YprimInvalid := TRUE;
        StateChanged := FALSE;  // reset the flag
    end;
end;

procedure TStorageObj.ComputekWkvar;
begin
    ComputePresentkW;
    ComputeInverterPower; // apply inverter eff after checking for cutin/cutout
end;

procedure TStorageObj.ComputePresentkW;
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
        kW_out := -kWOutIdling;
    end;

    if OldState <> Fstate then
        stateChanged := TRUE;
end;

procedure TStorageObj.ComputeInverterPower;
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
        if InverterCurveObj <> NIL then
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
            stateChanged := TRUE;

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

procedure TStorageObj.CalcYPrimMatrix(Ymatrix: TcMatrix);
var
    Y, Yij: Complex;
    i, j: Integer;
    FreqMultiplier: Double;
begin
    FYprimFreq := ActiveCircuit.Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;
    
    with ActiveCircuit.Solution do
    begin
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
                        Ymatrix[i, i] := Y;
                        Ymatrix.AddElement(Fnconds, Fnconds, Y);
                        Ymatrix[i, Fnconds] := Yij;
                        Ymatrix[Fnconds, i] := Yij;
                    end;
                    1:
                    begin   // Delta connection
                        Ymatrix[i, i] := Y;
                        Ymatrix.AddElement(i, i, Y);  // put it in again
                        for j := 1 to i - 1 do
                        begin
                            Ymatrix[i, j] := Yij;
                            Ymatrix[j, i] := Yij;
                        end;
                    end;
                end;
            end;
            Exit;
        end;

        //  Regular power flow Storage element model
        // Yeq is always expected as the equivalent line-neutral admittance
        case Fstate of
            STORE_CHARGING:
                Y := YeqDischarge;
            STORE_IDLING:
                Y := 0.0;
            STORE_DISCHARGING:
                if not GFM_mode then
                    Y := -YeqDischarge
                else
                with dynVars, StorageVars do
                begin
                    RatedkVLL := PresentkV;
                    Discharging := (StorageState = STORE_DISCHARGING);
                    mKVARating := FkVArating;
                    CalcGFMYprim(NPhases, @YMatrix);
                end;
        end;
        //---DEBUG--- WriteDLLDebugFile(Format('t=%.8g, Change To State=%s, Y=%.8g +j %.8g',[ActiveCircuit.Solution.dblHour, StateToStr, Y.re, Y.im]));
        // ****** Need to modify the base admittance for real harmonics calcs
        Y.im := Y.im / FreqMultiplier;
        if GFM_mode then
            Exit;

        case Connection of
            0:
                begin // WYE
                    Yij := -Y;
                    for i := 1 to Fnphases do
                    begin
                        YMatrix[i, i] := Y;
                        YMatrix.AddElement(Fnconds, Fnconds, Y);
                        YMatrix[i, Fnconds] := Yij;
                        YMatrix[Fnconds, i] := Yij;
                    end;
                end;
            1:
                begin  // Delta  or L-L
                    Y := Y / 3.0; // Convert to delta impedance
                    Yij := -Y;
                    for i := 1 to Fnphases do
                    begin
                        j := i + 1;
                        if j > Fnconds then
                            j := 1;  // wrap around for closed connections
                        YMatrix.AddElement(i, i, Y);
                        YMatrix.AddElement(j, j, Y);
                        YMatrix.AddElemSym(i, j, Yij);
                    end;
                end;
        end;
    end;
end;

function TStorageObj.NormalizeToTOD(h: Integer; sec: Double): Double;
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

procedure TStorageObj.CheckStateTriggerLevel(Level: Double);
// This is where we set the state of the Storage element
var
    OldState: Integer;
begin
    StateChanged := FALSE;

    OldState := Fstate;

    with StorageVars do
        if DispatchMode = STORE_FOLLOW then
        begin
            // set charge and discharge modes based on sign of loadshape
            if (Level > 0.0) and ((kWhStored - kWhReserve) > EPSILON) then
                StorageState := STORE_DISCHARGING
            else
            if (Level < 0.0) and ((kWhStored - kWhRating) < -EPSILON) then
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
        StateChanged := TRUE;
        YprimInvalid := TRUE;
    end;
end;

procedure TStorageObj.CalcYPrim();
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

    SetNominalDEROutput();
    CalcYPrimMatrix(YPrim_Shunt);

     // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages Doesn't fail
    for i := 1 to Yorder do
        Yprim_Series[i, i] := Yprim_Shunt[i, i] * 1.0e-10;

    YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
    inherited CalcYPrim();
end;

procedure TStorageObj.WriteTraceRecord(const s: String);
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
                WriteStr(sout, (Cabs(InjCurrent[i])): 8: 1, ', ');
                FSWrite(TraceFile, sout);
            end;
            for i := 1 to nphases do
            begin
                WriteStr(sout, (Cabs(ITerminal[i])): 8: 1, ', ');
                FSWrite(TraceFile, sout);
            end;
            for i := 1 to nphases do
            begin
                WriteStr(sout, (Cabs(Vterminal[i])): 8: 1, ', ');
                FSWrite(TraceFile, sout);
            end;
            for i := 1 to NumVariables() do
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

procedure TStorageObj.DoConstantPQStorageObj();
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
        V012[0] := 0; // Force zero-sequence voltage to zero
        V012[2] := 0; // Force negative-sequence voltage to zero
        SymComp2Phase(Vterminal, pComplexArray(@V012));  // Reconstitute Vterminal as balanced
    end;

    for i := 1 to Fnphases do
    begin
        case Connection of
            0:
            begin  // Wye
                VLN := Vterminal[i];
                VMagLN := Cabs(VLN);
                if VMagLN <= VBaseMin then
                    Curr := YEQ_Min * VLN  // Below 95% use an impedance model
                else
                if VMagLN > VBaseMax then
                    Curr := YEQ_Max * VLN  // above 105% use an impedance model
                else
                    Curr := cong(Cmplx(Pnominalperphase, Qnominalperphase) / VLN);  // Between 95% -105%, constant PQ

                if CurrentLimited then
                    if Cabs(Curr) > MaxDynPhaseCurrent then
                        Curr := cong(PhaseCurrentLimit / (VLN / VMagLN));
            end;

            1:
            begin  // Delta
                VLL := Vterminal[i];
                VMagLL := Cabs(VLL);
                if Fnphases > 1 then
                    VMagLN := VMagLL / SQRT3
                else
                    VMagLN := VmagLL;  // L-N magnitude
                if VMagLN <= VBaseMin then
                    Curr := (YEQ_Min / 3.0) * VLL  // Below 95% use an impedance model
                else
                if VMagLN > VBaseMax then
                    Curr := (YEQ_Max / 3.0) * VLL  // above 105% use an impedance model
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

procedure TStorageObj.DoConstantZStorageObj();
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
        V012[0] := 0; // Force zero-sequence voltage to zero
        V012[2] := 0; // Force negative-sequence voltage to zero
        SymComp2Phase(Vterminal, pComplexArray(@V012));  // Reconstitute Vterminal as balanced
    end;

    for i := 1 to Fnphases do
    begin
        Curr := Yeq2 * Vterminal[i];   // Yeq is always line to neutral
        StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(TRUE);
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;

procedure TStorageObj.DoUserModel();
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
                InjCurrent[i] -= Iterminal[i];
        end;
    end
    else
    begin
        DoSimpleMsg('%s model designated to use user-written model, but user-written model is not defined.', [FullName], 567);
    end;
end;

procedure TStorageObj.DoDynamicMode;
var
    i: Integer;
    PolarN: Polar;
    Curr, NeutAmps: Complex;
    // V012, I012: array[0..2] of Complex;
    AngCmp, // Required in this context to force the idling losses
    iActual: Double;

    procedure CalcVthev_Dyn;
    begin
        with StorageVars do
            Vthev := pclx(VthevMag, Theta);   // keeps theta constant
    end;

begin
    if DynaModel.Exists then
    begin
        DoDynaModel(); // do user-written model
        Exit;
    end;
    if GFM_Mode then
    begin
        dynVars.BaseV := dynVars.BasekV * 1000 * (dynVars.it[0] / dynVars.iMaxPPhase);
        dynVars.CalcGFMVoltage(NPhases, Vterminal);
        YPrim.MVMult(InjCurrent, Vterminal);
        Exit;
    end;

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    ZeroITerminal;
    // This model has no limitation in the number of phases and is ideally unbalanced (no dq-dv, but is implementable as well)
    // First, get the phase angles for the currents
    NeutAmps := 0;
    for i := 1 to FNphases do
        with dynVars do
        begin
            AngCmp := 0;
            // determine if the PV panel is ON
            if it[i - 1] <= iMaxPPhase then
                iActual := it[i - 1]
            else
                iActual := iMaxPPhase;

            if iActual < MinAmps then
                iActual := 0;                 // To mach with the %CutOut property

            if FState <> STORE_DISCHARGING then
                iActual := (PIdling / Vgrid[i - 1].mag) / NPhases;

            PolarN := topolar(iActual, Vgrid[i - 1].ang + AngCmp);     // Output Current estimated for active power
            Curr := -ptocomplex(PolarN);
            NeutAmps -= Curr;
            Iterminal[i] := Curr;
        end;

    if FnConds > FNphases then
        Iterminal[FnConds] := NeutAmps;

    // Add it into inj current array
    for i := 1 to FnConds do
        InjCurrent[i] -= Iterminal[i];

    set_ITerminalUpdated(TRUE);
end;

procedure TStorageObj.DoGFM_Mode();
// Implements the grid forming inverter control routine for the storage device
var
    i: Integer;
    W: Double;
    ZSys: Double;
begin
    with ActiveCircuit.Solution do
    begin
        dynVars.BaseV := VBase;
        dynVars.Discharging := (StorageState = STORE_DISCHARGING);
        if dynVars.IComp > 0 then
        begin
            ZSys := (2 * (Vbase * dynVars.ILimit)) - dynVars.IComp;
            dynVars.BaseV := (ZSys / dynVars.ILimit) * dynVars.VError;
        end;
        dynVars.CalcGFMVoltage(NPhases, Vterminal);
        YPrim.MVMult(InjCurrent, Vterminal);
        set_ITerminalUpdated(FALSE);
    end;
end;

procedure TStorageObj.DoDynaModel();
var
    DESSCurr: array[1..6] of Complex;  // Temporary biffer
    i: Integer;
begin
    // do user written dynamics model
    with ActiveCircuit.Solution do
    begin  // Just pass node voltages to ground and let dynamic model take care of it
        for i := 1 to FNconds do
            VTerminal^[i] := NodeV[NodeRef[i]];
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

procedure TStorageObj.DoHarmonicMode();
// Compute Injection Current Only when in harmonics mode

// Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built
// Vd is the fundamental frequency voltage behind Xd" for phase 1
var
    i: Integer;
    E: Complex;
    StorageHarmonic: Double;
    pBuffer: PCBuffer24;
begin
    pBuffer := @TStorage(ParentClass).cBuffer;
    ComputeVterminal();

    with ActiveCircuit.Solution do
    begin
        StorageHarmonic := Frequency / StorageFundamental;
        if SpectrumObj <> NIL then
            E := SpectrumObj.GetMult(StorageHarmonic) * StorageVars.VThevHarm // Get base harmonic magnitude
        else
            E := 0;

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
        pBuffer[Fnconds] := Vterminal[Fnconds];  // assume no neutral injection voltage

    // Inj currents = Yprim (E)
    YPrim.MVMult(InjCurrent, pComplexArray(pBuffer));
end;

procedure TStorageObj.CalcVTerminalPhase();
var
    i, j: Integer;
begin
    // Establish phase voltages and stick in Vterminal
    case Connection of
        0:
        begin
            with ActiveCircuit.Solution do
                for i := 1 to Fnphases do
                    Vterminal[i] := VDiff(NodeRef[i], NodeRef[Fnconds]);
        end;
        1:
        begin
            with ActiveCircuit.Solution do
                for i := 1 to Fnphases do
                begin
                    j := i + 1;
                    if j > Fnconds then
                        j := 1;
                    Vterminal[i] := VDiff(NodeRef[i], NodeRef[j]);
                end;
        end;
    end;

    StorageSolutionCount := ActiveCircuit.Solution.SolutionCount;
end;

procedure TStorageObj.CalcStorageModelContribution();
// Calculates Storage element current and adds it properly into the injcurrent array
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

        //  compute currents and put into InjTemp array;
        case VoltageModel of
            1:
                DoConstantPQStorageObj();
            2:
                DoConstantZStorageObj();
            3:
                DoUserModel();
        else
            DoConstantPQStorageObj(); // for now, until we implement the other models.
        end;
    end;
    // When this is Done, ITerminal is up to date
end;

procedure TStorageObj.CalcInjCurrentArray();
// Difference between currents in YPrim and total current
begin
    // Now Get Injection Currents
    if StorageObjSwitchOpen then
        ZeroInjCurrent
    else
        CalcStorageModelContribution();
end;

procedure TStorageObj.GetTerminalCurrents(Curr: pComplexArray);
// Compute total Currents
begin
    with ActiveCircuit.Solution do
    begin
        if IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount then
        begin     // recalc the contribution
            if not StorageObjSwitchOpen then
                CalcStorageModelContribution();  // Adds totals in Iterminal as a side effect
        end;
        inherited GetTerminalCurrents(Curr);
    end;

    if (DebugTrace) then
        WriteTraceRecord('TotalCurrent');
end;

function TStorageObj.InjCurrents(): Integer;
begin
    with ActiveCircuit.Solution do
    begin
        if LoadsNeedUpdating then
            SetNominalDEROutput(); // Set the nominal kW, etc for the type of solution being Done

        CalcInjCurrentArray(); // Difference between currents in YPrim and total terminal current

        if (DebugTrace) then
            WriteTraceRecord('Injection');

         // Add into System Injection Current Array
        Result := inherited InjCurrents();
    end;
end;

function TStorageObj.CheckOLInverter(): Boolean;
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

    MaxAmps := ((StorageVars.FkVArating * 1000) / NPhases) / VBase;
    GetCurrents(Iterminal);
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

procedure TStorageObj.ResetRegisters;
var
    i: Integer;
begin
    for i := 1 to NumStorageRegisters do
        Registers[i] := 0.0;
    for i := 1 to NumStorageRegisters do
        Derivatives[i] := 0.0;
    FirstSampleAfterReset := TRUE;  // initialize for trapezoidal integration
end;

procedure TStorageObj.Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);
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

procedure TStorageObj.TakeSample();
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
            S := 0;
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

function TStorageObj.CheckIfDelivering(): Boolean;
// Checks if delivering or absorbing power in GFM control mode
var
    i: Integer;
    P: double;
begin
    // If in GFM mode, check if we are actually delivering power
    Result := False;
    ComputeIterminal();
    Result := FALSE;  // Start assuming we are not delivering power
    for i := 1 to NPhases do
    begin
        P := (ActiveCircuit.Solution.NodeV[NodeRef[i]] * cong(Iterminal[i])).re;
        if (P < 0) then // If at least 1 phase is delivering, then returns True
        begin
            Result := True;
            Exit;
        end;
    end;
end;

procedure TStorageObj.UpdateStorage();
// Update Storage levels
var
    UpdateSt: Boolean;
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
                    UpdateSt := TRUE;
                    if GFM_Mode then
                        UpdateSt := CheckIfDelivering();

                    if UpdateSt then
                        kWhStored := kWhStored - (DCkW + kWIdlingLosses) / DischargeEff * IntervalHrs
                    else
                    begin
                        // We are absorbing power, let's recharge if needed
                        kWhStored := kWhStored + (DCkW + kWIdlingLosses) / DischargeEff * IntervalHrs;
                        if kWhStored > kWhRating then
                            kWhStored := kWhRating;
                    end;
                    // check if we have enough energy to deliver
                    if kWhStored < kWhReserve then
                    begin
                        kWhStored := kWhReserve;
                        Fstate := STORE_IDLING;  // It's empty Turn it off
                        StateChanged := TRUE;
                        GFM_Mode := FALSE;
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
                            StateChanged := TRUE;
                            GFM_Mode := FALSE;
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
                            StateChanged := TRUE;
                        end;
                    end;

                end;

                STORE_IDLING: ;
            end;

    end;

    // the update is done at the end of a time step so have to force
    // a recalc of the Yprim for the next time step.  Else it will stay the same.
    if StateChanged then
        YprimInvalid := TRUE;
end;

procedure TStorageObj.ComputeDCkW;
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

function TStorageObj.Get_PresentkW: Double;
begin
    Result := Pnominalperphase * 0.001 * Fnphases;
end;

function TStorageObj.Get_DCkW: Double;
begin
    ComputeDCkW;
    Result := FDCkW;
end;

function TStorageObj.Get_kWDesired: Double;
begin
    case StateDesired of
        STORE_CHARGING:
            Result := -pctkWIn * StorageVars.kWRating / 100.0;
        STORE_DISCHARGING:
            Result := pctkWOut * StorageVars.kWRating / 100.0;
        // STORE_IDLING:
        //     Result := 0.0;
    else
        Result := 0.0;
    end;
end;

function TStorageObj.Get_kWTotalLosses: Double;
begin
    Result := kWIdlingLosses + kWInverterLosses + kWChDchLosses;
end;

function TStorageObj.Get_InverterLosses: Double;
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

function TStorageObj.Get_kWIdlingLosses: Double;
begin
    if (FState = STORE_IDLING) then
    begin
        Result := abs(DCkW); // For consistency keeping with voltage variations
    end
    else
        Result := Pidling;
end;

function TStorageObj.Get_kWChDchLosses: Double;
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

procedure TStorageObj.Update_EfficiencyFactor;
begin
    with StorageVars do
    begin
        if not Assigned(InverterCurveObj) then
            EffFactor := 1.0
        else
            EffFactor := InverterCurveObj.GetYValue(abs(DCkW) / FkVArating);
    end;
end;

function TStorageObj.Get_PresentkV: Double;
begin
    Result := StorageVars.kVStorageBase;
end;

procedure TStorageObj.InitHarmonics();
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
                Va := NodeV[NodeRef[1]] - NodeV[NodeRef[Fnconds]];
            end;
            1:
            begin  // delta -- assume neutral is at zero
                Va := NodeV[NodeRef[1]];
            end;
        end;

    E := Va - Iterminal[1] * cmplx(StorageVars.Rthev, StorageVars.Xthev);
    StorageVars.Vthevharm := Cabs(E);   // establish base mag and angle
    StorageVars.ThetaHarm := Cang(E);
end;

procedure TStorageObj.InitStateVars();
// for going into dynamics mode
var
//    VNeut: Complex;
//    VThevPolar: Polar;
    i: Integer;
//    V012, I012: array[0..2] of Complex;
//    Vabc: array[1..3] of Complex;
    BaseZt: Double;
begin
    YprimInvalid := TRUE; // Force rebuild of YPrims

    if (Length(PICtrl) = 0) or (Length(PICtrl) < Fnphases) then
    begin
        SetLength(PICtrl, Fnphases);
        for i := 0 to (Fnphases - 1) do
        begin
            PICtrl[i] := TPICtrl.Create();
            PICtrl[i].Kp := dynVars.kP;
            PICtrl[i].kNum := 0.9502;
            PICtrl[i].kDen := 0.04979;
        end;
    end;

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
        Exit;
    end;

    if FState <> STORE_DISCHARGING then
        Exit;

    // Compute nominal Positive sequence voltage behind equivalent filter impedance
    with ActiveCircuit.Solution, StorageVars, dynVars do
    begin
        NumPhases := Fnphases;     // set Publicdata vars
        NumConductors := Fnconds;
        Conn := Connection;

        // Sets the length of State vars to cover the num of phases
        InitDynArrays(NumPhases);
        
        if NumPhases > 1 then
            BasekV := PresentkV / sqrt(3)
        else
            BasekV := PresentkV;

        BaseZt := 0.01 * ((PresentkV * PresentkV) / FkVArating) * 1000;
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
        ComputePresentkW();

        LS := ZThev.im / (2 * PI * DSS.DefaultBaseFreq);
        for i := 0 to (NPhases - 1) do
        begin
            Vgrid[i] := ctopolar(NodeV[NodeRef[i + 1]]);
            dit[i] := 0;
            it[i] := 0;
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

procedure TStorageObj.IntegrateStates();
// dynamics mode integration routine
var
    GFMUpdate: Boolean; // To avoid updating the IBR if current limit reached
    NumData, j, i: Integer;
    IMaxPhase, OFFVal: Double;
    IPresent: Double; // present amps per phase
    curr: Array of Complex; // For storing the present currents when using current limiter
begin
    // Compute Derivatives and Then integrate
    ComputeIterminal();

    if Dynamodel.Exists then   // Checks for existence and Selects
    begin
        DynaModel.Integrate();
        Exit;
    end;
    
    with ActiveCircuit.Solution, StorageVars, dynVars do
    begin
        ComputePresentkW();
        IMaxPhase := (kW_out / BasekV) / NumPhases;
        for i := 0 to (NumPhases - 1) do
        begin
            if FState = STORE_DISCHARGING then
            begin
                with DynaVars do
                    if (IterationFlag = 0) then //First iteration of new time step
                        itHistory[i] := it[i] + 0.5 * h * dit[i];
                Vgrid[i] := ctopolar(NodeV[NodeRef[i + 1]]); // Voltage at the Inv terminals
                if not GFM_Mode then
                begin
                    if (Vgrid[i].mag < MinVS) or (Vgrid[i].mag > MaxVS) then
                    begin
                        ISP := 0.01; // turn off the inverter
                        FState := STORE_IDLING;
                        if (Vgrid[i].mag > MaxVS) then
                            Vgrid[i].mag := MaxVs;

                    end
                    else
                        ISP := ((kW_out * 1000) / Vgrid[i].mag) / NumPhases;
                    if ISP > IMaxPPhase then
                        ISP := IMaxPPhase;
                end
                else
                begin
                    if ResetIBR then
                        VDelta[i] := (0.001 - (Vgrid[i].mag / 1000)) / BasekV
                    else
                        VDelta[i] := (BasekV - (Vgrid[i].mag / 1000)) / BasekV;

                    GFMUpdate := TRUE;
                    
                    // Checks if there is current limit set
                    if ILimit > 0 then
                    begin
                        SetLength(curr, NPhases + 1);
                        GetCurrents(pComplexArray(@curr[0]));
                        for j := 0 to (Nphases - 1) do
                        begin
                            IPresent := cabs(curr[j]);
                            GFMUpdate := GFMUpdate and (IPresent < (ILimit * VError));
                        end;
                    end;

                    if (abs(VDelta[i]) > CtrlTol) and GFMUpdate then
                    begin
                        ISPDelta[i] := ISPDelta[i] + (IMaxPhase * VDelta[i]) * kP * 100;
                        if ISPDelta[i] > IMaxPhase then
                            ISPDelta[i] := IMaxPhase
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
                                    ;  // Nothing for this object (current)
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
                    SolveDynamicStep(ActiveCircuit, i, @PICtrl[i]); // Solves dynamic step for inverter

                // Trapezoidal method
                with DynaVars do
                begin
                    if DynamicEqObj <> NIL then
                        dit[i] := DynamicEqVals[DynOut[0]][1];
                    it[i] := itHistory[i] + 0.5 * h * dit[i];
                end;
            end
            else
            begin
                if (Vgrid[i].mag >= MinVS) or ResetIBR then
                    OFFVal := PIdling / Vgrid[i].mag; // To match with idling losses
                //else
                //    OFFVal :=  0;
                it[i] := OFFVal; // To match with idling losses
            end;
        end;
    end;

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

function TStorageObj.Get_Variable(i: Integer): Double;
// Return variables one at a time
var
    N, k: Integer;
    A, B: Boolean;
begin
    Result := -9999.99;  // error return value; no state vars
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

    // for now, report kWhstored and mode
    with StorageVars do
        case i of
            1:
                Result := kWhStored;
            2:
            begin
                if not GFM_Mode then            
                begin
                    Result := FState;
                    Exit
                end;
                    if CheckIfDelivering() then
                        Result := STORE_DISCHARGING
                    else
                    if kWhStored = kWhRating then
                        Result := STORE_IDLING
                    else
                        Result := STORE_CHARGING;                
            end;
            3, 4:
            begin
                A := GFM_mode and CheckIFDelivering();
                B := (FState = STORE_DISCHARGING) and (not GFM_mode);
                A := A or B;
                if i = 4 then
                    A := not A;
                if A then
                    Result := abs(Power[1].re * 0.001)
                else
                    Result := 0.0;
            end;
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
            (NumBaseStorageVariables + 1)..NumStorageVariables:
                Result := dynVars.Get_InvDynValue(i - NumBaseStorageVariables - 1, NPhases);
        else
            if UserModel.Exists then   // Checks for existence and Selects
            begin
                N := UserModel.FNumVars;
                k := (i - NumStorageVariables);
                if k <= N then
                begin
                    Result := UserModel.FGetVariable(k);
                    Exit;
                end;
            end;
            if DynaModel.Exists then  // Checks for existence and Selects
            begin
                N := DynaModel.FNumVars;
                k := (i - NumStorageVariables);
                if k <= N then
                begin
                    Result := DynaModel.FGetVariable(k);
                    Exit;
                end;
            end;
        end;
end;

procedure TStorageObj.Set_Variable(i: Integer; Value: Double);
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

    with StorageVars do
        case i of
            1:
                kWhStored := Value;
            2:
                Fstate := Trunc(Value);
            3..13,
            22..25: 
                DoSimpleMsg('%s: variable index %d is read-only.', [FullName, i], 564);
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
            (NumBaseStorageVariables + 1)..NumStorageVariables:
                dynVars.Set_InvDynValue(i - NumBaseStorageVariables - 1, Value);
        else
            if UserModel.Exists then    // Checks for existence and Selects
            begin
                N := UserModel.FNumVars;
                k := (i - NumStorageVariables);
                if k <= N then
                begin
                    UserModel.FSetVariable(k, Value);
                    Exit;
                end;
            end;
            if DynaModel.Exists then     // Checks for existence and Selects
            begin
                N := DynaModel.FNumVars;
                k := (i - NumStorageVariables);
                if k <= N then
                begin
                    DynaModel.FSetVariable(k, Value);
                    Exit;
                end;
            end;
        end;
end;

procedure TStorageObj.GetAllVariables(var States: ArrayOfDouble);
var
    i: Integer;
begin
    if DynamicEqObj <> NIL then
    begin
        for i := 1 to DynamicEqObj.NVariables * Length(DynamicEqVals[0]) do
            States[i - 1] := DynamicEqObj.Get_DynamicEqVal(i - 1, DynamicEqVals);

        Exit;
    end;

    for i := 1 to NumStorageVariables do
        States[i - 1] := Variable[i];

    if UserModel.Exists then
    begin    // Checks for existence and Selects
        // N := UserModel.FNumVars;
        UserModel.FGetAllVars(pDoubleArray(@States[NumStorageVariables]));
    end;
    if DynaModel.Exists then
    begin    // Checks for existence and Selects
        // N := UserModel.FNumVars;
        DynaModel.FGetAllVars(pDoubleArray(@States[NumStorageVariables]));
    end;
end;

function TStorageObj.NumVariables(): Integer;
begin
    // Try DynamicExp first
    Result := inherited NumVariables();
    if Result <> 0 then 
        Exit;

    // Fallback to the classic
    Result := NumStorageVariables;

     // Exists does a check and then does a Select
    if UserModel.Exists then
        Result := Result + UserModel.FNumVars;
    if DynaModel.Exists then
        Result := Result + DynaModel.FNumVars;
end;

function TStorageObj.VariableName(i: Integer): String;
const
    BuffSize = 255;
var
    n, i2: Integer;
    Buff: array[0..BuffSize] of AnsiChar;
    pName: pAnsichar;
begin
    Result := 'ERROR';
    if i < 1 then
        Exit;  // Someone goofed

    // Try DynamicExp first
    Result := inherited VariableName(i);
    if Length(Result) <> 0 then
        Exit;

    // Fallback to the classic
    n := 0;
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
        (NumBaseStorageVariables + 1)..NumStorageVariables:
            Result := dynVars.Get_InvDynName(i - NumBaseStorageVariables - 1);
    else
        if UserModel.Exists then    // Checks for existence and Selects
        begin
            pName := PAnsiChar(@Buff);
            n := UserModel.FNumVars;
            i2 := i - NumStorageVariables;
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
            i2 := i - NumStorageVariables; // Relative index
            if i2 <= n then
            begin
                DynaModel.FGetVarName(i2, pName, BuffSize);
                Result := String(pName);
                Exit;
            end;
        end;
    end;
end;

procedure TStorageObj.MakePosSequence();
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

procedure TStorageObj.Set_ConductorClosed(Index: Integer; Value: Boolean);
begin
    inherited;

    // Just turn Storage element on or off;
    if Value then
        StorageObjSwitchOpen := FALSE
    else
        StorageObjSwitchOpen := TRUE;
end;

procedure TStorageObj.kWOut_Calc;
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

//          if VWmode and (FState = STORE_DISCHARGING) then if (abs(kWRequested) < abs(limitkWpct)) then limitkWpct :=  kWRequested * sign(kW_Out);
          // VW works only if element is not in idling state.
          // When the VW is working in the 'limiting' region, kWRequested will be positive.
          // When in 'requesting' region, it will be negative.
        if VWmode and not (FState = STORE_IDLING) then
        begin
            if (kWRequested >= 0.0) and (abs(kWRequested) < abs(limitkWpct)) then  // Apply VW limit
            begin
                if FState = STORE_DISCHARGING then
                    limitkWpct := kWRequested
                else
                    limitkWpct := -1 * kWRequested;
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
                StateChanged := TRUE;
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

procedure TStorageObj.Set_kW(const Value: Double);
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

procedure TStorageObj.Set_Maxkvar(const Value: Double);
begin
    StorageVars.Fkvarlimit := Value;
    SetAsNextSeq(ord(TProp.kvarMax));
end;

procedure TStorageObj.Set_Maxkvarneg(const Value: Double);
begin
    StorageVars.Fkvarlimitneg := Value;
    SetAsNextSeq(ord(TProp.kvarMaxAbs));
end;

procedure TStorageObj.Set_kVARating(const Value: Double);
begin
    StorageVars.FkVARating := Value;
    SetAsNextSeq(ord(TProp.kVA));
end;

procedure TStorageObj.Set_PowerFactor(const Value: Double);
begin
    PFNominal := Value;
    varMode := VARMODEPF;
end;

function TStorageObj.Get_kW: Double;
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

procedure TStorageObj.Set_StorageState(const Value: Integer);
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
        StateChanged := TRUE;

     //---DEBUG--- WriteDLLDebugFile(Format('t=%.8g, ---State Set To %s', [ActiveCircuit.Solution.dblHour, StateToStr ]));
end;

procedure TStorageObj.SetDragHandRegister(Reg: Integer; const Value: Double);
begin
    if Value > Registers[reg] then
        Registers[Reg] := Value;
end;

function TStorageObj.IsStorage(): Boolean;
begin
    Result := True;
end;

function TStorageObj.GetPFPriority(): Boolean;
begin
    Result := StorageVars.PF_Priority;
end;

procedure TStorageObj.SetPFPriority(value: Boolean);
begin
    StorageVars.PF_Priority := value;
end;

finalization
    StateEnum.Free;
    DispatchModeEnum.Free;
end.
