unit InvControl;

// ----------------------------------------------------------
// Copyright (c) 2018-2023, Paulo Meira, DSS-Extensions contributors
// Copyright (c) 2008-2022,  Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

uses
    RollAvgWindow,
    Command,
    ControlClass,
    ControlElem,
    InvBasedPCE,
    DSSClass,
    bus,
    PCElement,
    PVSystem,
    Storage,
    Arraydef,
    UComplex, DSSUcomplex,
    utilities,
    XYcurve,
    Dynamics,
    DSSPointerList,
    Classes,
    StrUtils,
    MathUtil;

type
{$SCOPEDENUMS ON}
    TInvControlPropLegacy = (
        INVALID = 0,

        DERList,
        Mode,
        CombiMode,
        vvc_curve1,
        hysteresis_offset,
        voltage_curvex_ref,
        avgwindowlen,

        voltwatt_curve,

        DbVMin,
        DbVMax,
        ArGraLowV,
        ArGraHiV,
        DynReacavgwindowlen,
        deltaQ_Factor,
        VoltageChangeTolerance,
        VarChangeTolerance,
        VoltwattYAxis,
        RateofChangeMode,
        LPFTau, // weird double: in the original version, value was not set
        RiseFallLimit, // weird double: in the original version, value was not set
        deltaP_Factor,
        EventLog,
        RefReactivePower,
        ActivePChangeTolerance,
        monVoltageCalc,
        monBus,
        MonBusesVbase,
        voltwattCH_curve,
        wattpf_curve,
        wattvar_curve,
        VV_RefReactivePower, // was deprecated, reintroduced for v0.12.2; TODO: TO BE REMOVED AGAIN LATER
        PVSystemList, // was 32 -- TODO: TO BE MARKED AS REMOVED
        Vsetpoint, // was 33
        ControlModel
    );
    TInvControlProp = (
        INVALID = 0,

        DERList,
        Mode,
        CombiMode,
        VVC_Curve1,
        Hysteresis_Offset,
        Voltage_CurveX_Ref,
        AvgWindowLen,

        VoltWatt_Curve,

        DbVMin,
        DbVMax,
        ArGraLowV,
        ArGraHiV,
        DynReacAvgWindowLen,
        DeltaQ_Factor,
        VoltageChangeTolerance,
        VarChangeTolerance,
        VoltWattYAxis,
        RateOfChangeMode,
        LPFTau,
        RiseFallLimit,
        DeltaP_Factor,
        EventLog,
        RefReactivePower,
        ActivePChangeTolerance,
        MonVoltageCalc,
        MonBus,
        MonBusesVBase,
        VoltWattCH_Curve,
        WattPF_Curve,
        WattVar_Curve,
        VV_RefReactivePower, // was deprecated, reintroduced for v0.12.2; TODO: TO BE REMOVED AGAIN LATER
        PVSystemList, // was 32 -- TODO: TO BE MARKED AS REMOVED
        VSetPoint, // was 33
        ControlModel
    );
{$SCOPEDENUMS OFF}

    // Modes
{$PUSH}
{$Z4} // keep enums as int32 values
    TInvControlControlMode = (
        NONE_MODE = 0,
        VOLTVAR = 1,
        VOLTWATT = 2,
        DRC = 3,
        WATTPF = 4,
        WATTVAR = 5,
        AVR = 6,
        GFM = 7
    );

    // Combi Modes
    TInvControlCombiMode = (
        NONE_COMBMODE = 0,
        VV_VW = 1,
        VV_DRC = 2
    );

    TInvControlModel = (
        Linear = 0,
        Exponential = 1
    );
{$POP}

    ERateofChangeMode = (
        INACTIVE,
        LPF,
        RISEFALL
    );

    TInvControl = class(TControlClass)
    PRIVATE
        XY_CurveClass: TDSSClass;

    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function NewObject(const ObjName: Ansistring; Activate: Boolean = True): Pointer; OVERRIDE;

        procedure UpdateAll();
    end;

    TInvVars = record
        CondOffset: Integer; // Offset for monitored terminal
        cBuffer: Array of Complex;    // Complex array buffer
        FAvgpVpuPrior: Double;
        FAvgpDRCVpuPrior: Double;
        FPresentVpu: Double;
        FPresentDRCVpu: Double;
        NPhasesDER: Integer;
        NCondsDER: Integer;
        FPendingChange: Integer;
        QDesiredVV: Double; // volt-var new set-point
        QDesiredWP: Double; // watt-pf new set-point
        QDesiredWV: Double; // watt-var new set-point
        QDesiredAVR: Double;
        QOld: Double;
        QOldVV: Double;
        QOldAVR: Double;
        QOldDRC: Double;
        QOldVVDRC: Double;
        QDesiredDRC: Double; //dynamic reactive power new set-point
        QDesiredVVDRC: Double;
        QHeadRoom: Double;
        QHeadRoomNeg: Double;
        PBase: Double;
        Qoutputpu: Double;
        QoutputVVpu: Double;
        QoutputDRCpu: Double;
        QoutputVVDRCpu: Double;
        QoutputAVRpu: Double;
        QDesireEndpu: Double;  // Q value used in the convergency algorithm
        QDesireVVpu: Double; // Q desired caculated in volt-var curve
        QDesireWPpu: Double; // Q desired caculated in watt-pf curve
        QDesireWVpu: Double; // Q desired caculated in watt-var curve
        QDesireDRCpu: Double;  // Q desired from the DRC equation
        QDesireAVRpu: Double;
        QDesireLimitedpu: Double; // Calculates possible Q considering kVA (watt priority) and kvarlimit limits
        QDesireOptionpu: Double; // Calculates Q Limit considering LPF and RF
        PLimitEndpu: Double;
        PLimitVWpu: Double;
        PLimitLimitedpu: Double;
        PLimitOptionpu: Double;
        deltaVDynReac: Double;
        PLimitVW: Double;
        POldVWpu: Double;
        FdeltaQFactor: Double;
        FdeltaPFactor: Double;
        DeltaV_old: Double;
        FVpuSolution: array of Double;
        FRollAvgWindow: TRollAvgWindow;
        FDRCRollAvgWindowpu: Double;
        FDRCRollAvgWindow: TRollAvgWindow;
        priorRollAvgWindow: Double;
        priorDRCRollAvgWindow: Double;
        FlagChangeCurve: Boolean;
        FActiveVVCurve: Integer;
        FPriorWattspu: Double;
        FPriorwatts: Double;
        FPriorPLimitOptionpu: Double;
        FPriorQDesireOptionpu: Double;
        kW_out_desiredpu: Double;
        kW_out_desired: Double;
        // Variables of functions that CONTROL reactive power
        FPriorvarspu: Double;
        FPriorvars: Double;
        // Active power
        FFlagVWOperates: Boolean;  // Flag enabled when volt-watt Pdesired is less than 1. So volt-watt algorithm starts to work
        // Flags used to record function states. They are interval variables of DER
        FVVOperation: Double;
        FVWOperation: Double;
        FDRCOperation: Double;
        FVVDRCOperation: Double;
        FWPOperation: Double;
        FWVOperation: Double;
        FAVROperation: Double;
        // Variables of functions that LIMIT active power
        // Variables of DER element
        FVBase: Double;
        FVarFollowInverter: Boolean;
        FInverterON: Boolean;
        FpresentkW: Double;
        FkVARating: Double;
        Fpresentkvar: Double;
        FkvarLimit: Double;
        FkvarLimitNeg: Double;
        FCurrentkvarLimit: Double;
        FCurrentkvarLimitNeg: Double;
        FDCkWRated: Double;  // Pmpp for PVSystem, kWRated for Storage
        FpctDCkWRated: Double;  // pctPmpp for PVSystem, pctkWRated for Storage
        FEffFactor: Double;
        FDCkW: Double;  // PanelkW for PVSystem, DCkW for Storage
        FPPriority: Boolean;
        // Active voltage regulation (AVR)
        DQDV: Double;
        Fv_setpointLimited: Double;
        FAvgpAVRVpuPrior: Double;
        PICtrl: TPICtrl;
    end;

    TInvControlObj = class(TControlElem)
    PRIVATE
        ControlActionHandle: Integer;
        ControlledElement: array of TInvBasedPCE;
        MonitoredElement: TInvBasedPCE;  // First DER element for now (the first element from ControlledElement TDSSPointerList)

        // Variables for voltages
        FVreg: Double;
        FVpuSolutionIdx: Integer;

        // Variables for convergence process
        FdeltaQ_factor: Double;
        FdeltaP_factor: Double;

        FVoltageChangeTolerance: Double;
        FVarChangeTolerance: Double;
        FActivePChangeTolerance: Double;

        // Variables of DER element
        FDERPointerList: TDSSPointerList;
        FListSize: Integer;

        // Variables for monitored Bus/buses
        FMonBusesPhase: Integer;
        FUsingMonBuses: Boolean;
        FMonBuses: array of Ansistring;
        FMonBusesIndex: Integer;
        FMonBusesVbase: pDoubleArray;
        FMonBusesNodes: array of array of Integer;

        // Variables for LPF and RF options
        RateofChangeMode: ERateofChangeMode;
        FRiseFallLimit: Double;

        // Variables of the smart inverter functions
        FVoltage_CurveX_ref: Integer;  // valid values are 0: = Vref (rated), 1:= avg
        FReacPower_ref: Integer;
        FVoltwattYAxis: Integer; // 1 = %Pmpp, 0 = %Available power

        // volt-var
        Fvvc_curveOffset: Double;
        FRollAvgWindowLength: Integer;//FVAvgWindowLengthSec // rolling average window length in seconds

        // watt-pf
        pf_wp_nominal: Double;

        // DRC
        FDbVMin: Double;
        FDbVMax: Double;


        // Active voltage regulation (AVR)
        Fv_setpoint: Double;

        // Others
        CtrlModel: TInvControlModel;
        CtrlVars: Array of TInvVars;

        procedure Set_PendingChange(Value: Integer; DevIndex: Integer);
        function Get_PendingChange(DevIndex: Integer): Integer;
        procedure UpdateInvControl(i: Integer);
        procedure UpdateDERParameters(i: Integer);
        procedure CalcVoltWatt_watts(j: Integer);
        procedure CalcQVVcurve_desiredpu(j: Integer);
        procedure CalcQWPcurve_desiredpu(j: Integer);
        procedure CalcQWVcurve_desiredpu(j: Integer);
        procedure CalcQDRC_desiredpu(j: Integer);
        procedure CalcQAVR_desiredpu(j: Integer);
        procedure Check_Qlimits(j: Integer; Q: Double);
        procedure Check_Qlimits_WV(j: Integer; Q: Double);
        procedure Calc_PQ_WV(j: Integer);
        procedure Calc_QHeadRoom(j: Integer);
        procedure CalcVoltVar_vars(j: Integer);
        procedure CalcAVR_vars(j: Integer);
        procedure CalcWATTPF_vars(j: Integer);
        procedure CalcWATTVAR_vars(j: Integer);
        procedure CalcDRC_vars(j: Integer);
        procedure CalcVVDRC_vars(j: Integer);
        procedure CalcLPF(m: Integer; powertype: Ansistring; LPF_desiredpu: Double);
        procedure CalcRF(m: Integer; powertype: Ansistring; RF_desiredpu: Double);
        procedure Calc_PBase(j: Integer);
        procedure Check_Plimits(j: Integer; P: Double);
        procedure CalcPVWcurve_limitpu(j: Integer);
        procedure GetMonVoltage(var Vpresent: Double; i: Integer; BasekV: Double);
        procedure Change_deltaQ_factor(j: Integer);
        procedure Change_deltaP_factor(j: Integer);


    PUBLIC
        DERNameList: TStringList;
        MonBusesNameList: TStringList;
        LPFTau: Double; // Variable for LPF and RF options

        // DRC
        FDRCRollAvgWindowLength: Integer; //FDRCVAvgWindowLengthSec // rolling average window length in seconds
        FArGraLowV: Double;
        FArGraHiV: Double;

        Fvvc_curve: TXYcurveObj; // volt-var
        Fwattpf_curve: TXYcurveObj;
        Fwattvar_curve: TXYcurveObj;
        Fvoltwatt_curve: TXYcurveObj; // volt-watt
        FvoltwattCH_curve: TXYcurveObj; // volt-watt (charging)

        ControlMode: TInvControlControlMode;
        CombiMode: TInvControlCombiMode;

        constructor Create(ParClass: TDSSClass; const InvControlName: Ansistring);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure MakePosSequence(); OVERRIDE; // Make a positive Sequence Model
        procedure RecalcElementData(); OVERRIDE;
        procedure Sample(); OVERRIDE; // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE; // do the action that is pending from last sample
        procedure Reset; OVERRIDE; // Reset to initial defined state
        function MakeDERList: Boolean;
        property PendingChange[DevIndex: Integer]: Integer READ Get_PendingChange WRITE Set_PendingChange;
    end;


implementation

uses
    Sysutils,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    uCmatrix,
    Math,
    DSSHelper,
    DSSObjectHelper;

type
    TObj = TInvControlObj;
    TProp = TInvControlProp;
    TPropLegacy = TInvControlPropLegacy;

const
    NumPropsThisClass = Ord(High(TProp));

    ReacPower_VARAVAL = 0;
    ReacPower_VARMAX = 1;

    NONE = 0;
    CHANGEVARLEVEL = 1;
    CHANGEWATTLEVEL = 2;
    CHANGEWATTVARLEVEL = 3;
    CHANGEDRCVVARLEVEL = 4;

    AVGPHASES = -1;
    MAXPHASE = -2;
    MINPHASE = -3;

    FLAGDELTAQ = -1.0;
    FLAGDELTAP = -1.0;
    DELTAQDEFAULT = 0.5;
    DELTAPDEFAULT = 0.5;

var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;
    ModeEnum, CombiModeEnum, VoltageCurveXRefEnum, VoltWattYAxisEnum, RoCEnum, RefQEnum, ControlModelEnum: TDSSEnum;

constructor TInvControl.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
        ModeEnum := TDSSEnum.Create('InvControl: Control Mode', True, 1, 5,
            ['Voltvar', 'VoltWatt', 'DynamicReaccurr', 'WattPF', 'Wattvar', 'AVR', 'GFM'],
            [ord(VOLTVAR), ord(VOLTWATT), ord(DRC), ord(WATTPF), ord(WATTVAR), ord(AVR), ord(GFM)]);
        CombiModeEnum := TDSSEnum.Create('InvControl: Combi Mode', True, 4, 4,
            ['VV_VW', 'VV_DRC'], [ord(VV_VW), ord(VV_DRC)]);
        VoltageCurveXRefEnum := TDSSEnum.Create('InvControl: Voltage Curve X Ref', True, 1, 2,
            ['Rated', 'Avg', 'RAvg'], [0, 1, 2]);
        VoltWattYAxisEnum := TDSSEnum.Create('InvControl: Volt-Watt Y-Axis', True, 1, 2,
            ['PAvailablePU', 'PMPPPU', 'PctPMPPPU', 'KVARatingPU'], [0, 1, 2, 3]);
        RoCEnum := TDSSEnum.Create('InvControl: Rate-of-change Mode', True, 3, 3,
            ['Inactive', 'LPF', 'RiseFall'], [ord(INACTIVE), ord(LPF), ord(RISEFALL)]);
        RoCEnum.JSONName := 'InvControlRateOfChangeMode';
        RefQEnum := TDSSEnum.Create('InvControl: Reactive Power Reference', True, 4, 4,
            ['VARAVAL', 'VARMAX'], [0, 1]);
        ControlModelEnum := TDSSEnum.Create('InvControl: Control Model', True, 1, 1,
            ['Linear', 'Exponential'], [0, 1]);
        ControlModelEnum.JSONUseNumbers := true;
        RefQEnum.AllowLonger := True;
    end;

    XY_CurveClass := GetDSSClassPtr(dssContext, 'XYCurve');

    inherited Create(dssContext, INV_CONTROL, 'InvControl');
end;

destructor TInvControl.Destroy;
begin
    inherited Destroy;
end;

function GetMonBusesCount(Obj: TObj): Integer;
begin
    Result := Obj.MonBusesNameList.Count;
end;

procedure TInvControl.DefineProperties;
var
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, PropInfoLegacy);

    // object references
    PropertyType[ord(TProp.vvc_curve1)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.vvc_curve1)] := ptruint(@obj.Fvvc_curve);
    PropertyOffset2[ord(TProp.vvc_curve1)] := ptruint(DSS.XYCurveClass);

    PropertyType[ord(TProp.voltwatt_curve)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.voltwatt_curve)] := ptruint(@obj.Fvoltwatt_curve);
    PropertyOffset2[ord(TProp.voltwatt_curve)] := ptruint(DSS.XYCurveClass);

    PropertyType[ord(TProp.voltwattCH_curve)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.voltwattCH_curve)] := ptruint(@obj.FvoltwattCH_curve);
    PropertyOffset2[ord(TProp.voltwattCH_curve)] := ptruint(DSS.XYCurveClass);

    PropertyType[ord(TProp.wattpf_curve)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.wattpf_curve)] := ptruint(@obj.Fwattpf_curve);
    PropertyOffset2[ord(TProp.wattpf_curve)] := ptruint(DSS.XYCurveClass);

    PropertyType[ord(TProp.wattvar_curve)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.wattvar_curve)] := ptruint(@obj.Fwattvar_curve);
    PropertyOffset2[ord(TProp.wattvar_curve)] := ptruint(DSS.XYCurveClass);

    // enum properties
    PropertyType[ord(TProp.Mode)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.Mode)] := ptruint(@obj.ControlMode);
    PropertyOffset2[ord(TProp.Mode)] := PtrInt(ModeEnum);
    PropertyFlags[ord(TProp.Mode)] := [TPropertyFlag.NoDefault];

    PropertyType[ord(TProp.CombiMode)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.CombiMode)] := ptruint(@obj.CombiMode);
    PropertyOffset2[ord(TProp.CombiMode)] := PtrInt(CombiModeEnum);
    PropertyFlags[ord(TProp.CombiMode)] := [TPropertyFlag.NoDefault];

    PropertyType[ord(TProp.voltage_curvex_ref)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.voltage_curvex_ref)] := ptruint(@obj.FVoltage_CurveX_ref);
    PropertyOffset2[ord(TProp.voltage_curvex_ref)] := PtrInt(VoltageCurveXRefEnum);

    PropertyType[ord(TProp.VoltwattYAxis)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.VoltwattYAxis)] := ptruint(@obj.FVoltwattYAxis);
    PropertyOffset2[ord(TProp.VoltwattYAxis)] := PtrInt(VoltWattYAxisEnum);

    PropertyType[ord(TProp.RateofChangeMode)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.RateofChangeMode)] := ptruint(@obj.RateofChangeMode);
    PropertyOffset2[ord(TProp.RateofChangeMode)] := PtrInt(RoCEnum);

    PropertyType[ord(TProp.RefReactivePower)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.RefReactivePower)] := ptruint(@obj.FReacPower_ref);
    PropertyOffset2[ord(TProp.RefReactivePower)] := PtrInt(RefQEnum);

    PropertyOffset[ord(TProp.VV_RefReactivePower)] := 0;
    PropertyType[ord(TProp.VV_RefReactivePower)] := TPropertyType.DeprecatedAndRemoved; //TODO: fully remove
    PropertyDeprecatedMessage[ord(TProp.VV_RefReactivePower)] := '"VV_RefReactivePower" was deprecated in 2020. Use "RefReactivePower" instead.';
    PropertyFlags[ord(TProp.VV_RefReactivePower)] := [TPropertyFlag.Deprecated];

    PropertyType[ord(TProp.monVoltageCalc)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.monVoltageCalc)] := ptruint(@obj.FMonBusesPhase);
    PropertyOffset2[ord(TProp.monVoltageCalc)] := PtrInt(DSS.MonPhaseEnum);

    PropertyType[ord(TProp.ControlModel)] := TPropertyType.MappedIntEnumProperty;
    PropertyOffset[ord(TProp.ControlModel)] := ptruint(@obj.CtrlModel);
    PropertyOffset2[ord(TProp.ControlModel)] := PtrInt(ControlModelEnum);

    // boolean properties
    PropertyType[ord(TProp.EventLog)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.EventLog)] := ptruint(@obj.ShowEventLog);

    // string lists
    PropertyType[ord(TProp.DERList)] := TPropertyType.StringListProperty;
    PropertyType[ord(TProp.monBus)] := TPropertyType.StringListProperty;
    PropertyType[ord(TProp.PVSystemList)] := TPropertyType.StringListProperty;
    PropertyOffset[ord(TProp.DERList)] := ptruint(@obj.DERNameList);
    PropertyOffset[ord(TProp.monBus)] := ptruint(@obj.MonBusesNameList);
    PropertyOffset[ord(TProp.PVSystemList)] := ptruint(@obj.DERNameList);
    PropertyFlags[ord(TProp.PVSystemList)] := [TPropertyFlag.Redundant, TPropertyFlag.Deprecated]; // TODO: mark as removed
    PropertyRedundantWith[ord(TProp.PVSystemList)] := ord(TProp.DERList);
    PropertyDeprecatedMessage[ord(TProp.PVSystemList)] := '"PVSystemList" was deprecated in 2020. Use "DERList" instead.';

    // array of doubles
    PropertyType[ord(TProp.MonBusesVbase)] := TPropertyType.DoubleVArrayProperty;
    PropertyOffset[ord(TProp.MonBusesVbase)] := ptruint(@obj.FMonBusesVbase);
    PropertyOffset3[ord(TProp.MonBusesVbase)] := ptruint(@GetMonBusesCount);
    PropertyFlags[ord(TProp.MonBusesVbase)] := [TPropertyFlag.SizeIsFunction]; // MonBusesNameList.Count

    // double properties (default type)
    PropertyOffset[ord(TProp.DbVMin)] := ptruint(@obj.FDbVMin);
    PropertyOffset[ord(TProp.DbVMax)] := ptruint(@obj.FDbVMax);
    PropertyOffset[ord(TProp.ArGraLowV)] := ptruint(@obj.FArGraLowV);
    PropertyOffset[ord(TProp.ArGraHiV)] := ptruint(@obj.FArGraHiV);
    PropertyOffset[ord(TProp.deltaQ_Factor)] := ptruint(@obj.FdeltaQ_factor);
    PropertyOffset[ord(TProp.VoltageChangeTolerance)] := ptruint(@obj.FVoltageChangeTolerance);
    PropertyOffset[ord(TProp.VarChangeTolerance)] := ptruint(@obj.FVarChangeTolerance);
    PropertyOffset[ord(TProp.deltaP_Factor)] := ptruint(@obj.FdeltaP_factor);
    PropertyOffset[ord(TProp.ActivePChangeTolerance)] := ptruint(@obj.FActivePChangeTolerance);
    PropertyOffset[ord(TProp.Vsetpoint)] := ptruint(@obj.Fv_setpoint);

    PropertyOffset[ord(TProp.LPFTau)] := ptruint(@obj.LPFTau);
    PropertyFlags[ord(TProp.LPFTau)] := [TPropertyFlag.Units_s];

    PropertyOffset[ord(TProp.RiseFallLimit)] := ptruint(@obj.FRiseFallLimit);

    // advanced doubles
    PropertyOffset[ord(TProp.hysteresis_offset)] := ptruint(@obj.Fvvc_curveOffset);
    PropertyFlags[ord(TProp.hysteresis_offset)] := [TPropertyFlag.NonPositive];

    // integer
    PropertyType[ord(TProp.DynReacavgwindowlen)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.DynReacavgwindowlen)] := ptruint(@obj.FDRCRollAvgWindowLength);
    PropertyFlags[ord(TProp.DynReacavgwindowlen)] := [TPropertyFlag.IntervalUnits];

    PropertyType[ord(TProp.avgwindowlen)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.avgwindowlen)] := ptruint(@obj.FRollAvgWindowLength);
    PropertyFlags[ord(TProp.avgwindowlen)] := [TPropertyFlag.IntervalUnits];


    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TInvControl.NewObject(const ObjName: Ansistring; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure ValidateXYCurve(dss: TDSSContext; var curve: TXYcurveObj; InvControlMode: TInvControlControlMode);
var
    i: Integer;
begin
    if curve = NIL then
        Exit;

    // if VOLTWATT control mode then check for any negative watt values (pu)
    // and values greater than 1.0 per-unit (=100 percent output)
    if InvControlMode = VOLTWATT then
    begin
        for i := 1 to curve.NumPoints do
        begin
            if (curve.YValue_pt[i] < 0.0) or (curve.YValue_pt[i] > 1.0) then
            begin
                DoSimpleMsg(dss, 'XY Curve object: "%s" has active power value(s) greater than 1.0 per-unit or less than -1.0 per-unit.  Not allowed for VOLTWATT control mode for PVSystem/Storages', [Curve.Name], 381);
                curve := NIL;
                Break;
            end;
        end;
    end;

    // if WATTPF control mode then check for any negative pf values
    // and values greater than 1.0
    if InvControlMode = WATTPF then
    begin
        for i := 1 to curve.NumPoints do
        begin
            if (curve.YValue_pt[i] < -1.0) or (curve.YValue_pt[i] > 1.0) then
            begin
                DoSimpleMsg(dss, 'XY Curve object: "%s" has power factor value(s) greater than 1.0 or less than -1.0.  Not allowed for WATTPF control mode for PVSystem/Storages', [Curve.Name], 381);
                curve := NIL;
                Break;
            end;
        end;
    end;

    // if WATTVAR control mode then check for any negative pf values
    // and values greater than 1.0
    if InvControlMode = WATTVAR then
    begin
        for i := 1 to curve.NumPoints do
        begin
            if (curve.YValue_pt[i] < -1.0) or (curve.YValue_pt[i] > 1.0) then
            begin
                DoSimpleMsg(dss, 'XY Curve object: "%s" has reactive power value(s) greater than 1.0 per-unit or less than -1.0 per-unit.  Not allowed for WATTVAR control mode for PVSystem/Storages', [Curve.Name], 381);
                curve := NIL;
                Break;
            end;
        end;
    end;
end;

procedure TInvControlObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags);
var
    NodeBuffer: array[1..10] of Integer;
    i,
    j,
    NNode: Integer;
begin
    case Idx of
        ord(TProp.DERList):
        begin // re-alloc based on
            FDERPointerList.Clear;
            FListSize := DERNameList.count;
        end;
        ord(TProp.Mode):
            CombiMode := NONE_COMBMODE;
        ord(TProp.DbVMin):
            if (FDbVMax > 0.0) and (FDbVmin > FDbVMax) then
            begin
                DoSimpleMsg('Minimum dead-band voltage value should be less than the maximum dead-band voltage value.  Value set to 0.0 "%s" for object "%s"', [ParentClass.PropertyName[Idx], FullName], 1365);
                FDbvMin := 0.0;
            end;
        ord(TProp.DbVMax):
            if (FDbVMin > 0.0) and (FDbVMax < FDbVmin) then
            begin
                DoSimpleMsg('Maximum dead-band voltage value should be greater than the minimum dead-band voltage value.  Value set to 0.0 "%s" for Object "%s"', [ParentClass.PropertyName[Idx], FullName], 1366);
                FDbvMax := 0.0;
            end;
        ord(TProp.LPFTau):
            if LPFTau <= 0  then
                RateofChangeMode := INACTIVE;
        ord(TProp.RiseFallLimit):
            if FRiseFallLimit <= 0 then
                RateofChangeMode := INACTIVE;
        ord(TProp.monBus):
        begin //FMonBuses := Param;
            SetLength(FMonBuses, MonBusesNameList.Count);
            SetLength(FMonBusesNodes, MonBusesNameList.Count);
            for i := 0 to MonBusesNameList.Count - 1 do
            begin
                FMonBuses[i] := DSS.AuxParser.ParseAsBusName(MonBusesNameList.Strings[i], NNode, pIntegerArray(@NodeBuffer));
                SetLength(FMonBusesNodes[i], NNode);
                for j := 0 to NNode - 1 do
                    FMonBusesNodes[i, j] := NodeBuffer[j + 1];
            end;
        end;
        ord(TProp.PVSystemList):
        begin
            // If using this property from the previous version of InvControl, we assume that the list includes only
            // PVSystems. The list is updated to prepend the names with "PVSystem.".
            for i := 0 to (DERNameList.Count - 1) do
                DERNameList[i] := 'PVSystem.' + DERNameList[i];

            PropertySideEffects(ord(TProp.DERList), previousIntVal, setterFlags);
        end;
        ord(TProp.vvc_curve1):
            ValidateXYCurve(DSS, Fvvc_curve, VOLTVAR);
        ord(TProp.voltwatt_curve):
            ValidateXYCurve(DSS, Fvoltwatt_curve, VOLTWATT);
        ord(TProp.voltwattCH_curve):
            ValidateXYCurve(DSS, FvoltwattCH_curve, VOLTWATT);
        ord(TProp.wattpf_curve):
            ValidateXYCurve(DSS, Fwattpf_curve, WATTPF);
        ord(TProp.wattvar_curve):
            ValidateXYCurve(DSS, Fwattvar_curve, WATTVAR);
    end;
    inherited PropertySideEffects(Idx, previousIntVal, setterFlags);
end;

procedure TInvControlObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i, j: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    FNPhases := Other.Fnphases;
    NConds := Other.Fnconds; // Force Reallocation of terminal stuff

    ControlledElement := Copy(Other.ControlledElement, Length(Other.ControlledElement));
    for i := 1 to FDERPointerList.Count do
    begin
        with CtrlVars[i] do
        begin
            CondOffset := Other.CtrlVars[i].CondOffset;

            FVBase := Other.CtrlVars[i].FVBase;
            FVarFollowInverter := Other.CtrlVars[i].FVarFollowInverter;
            FInverterON := Other.CtrlVars[i].FInverterON;
            FpresentkW := Other.CtrlVars[i].FpresentkW;
            FkVARating := Other.CtrlVars[i].FkVARating;
            Fpresentkvar := Other.CtrlVars[i].Fpresentkvar;
            FkvarLimit := Other.CtrlVars[i].FkvarLimit;
            FkvarLimitNeg := Other.CtrlVars[i].FkvarLimitNeg;
            FCurrentkvarLimit := Other.CtrlVars[i].FCurrentkvarLimit;
            FCurrentkvarLimitNeg := Other.CtrlVars[i].FCurrentkvarLimitNeg;
            FDCkWRated := Other.CtrlVars[i].FDCkWRated;
            FpctDCkWRated := Other.CtrlVars[i].FpctDCkWRated;
            FEffFactor := Other.CtrlVars[i].FEffFactor;
            FDCkW := Other.CtrlVars[i].FDCkW;
            FPPriority := Other.CtrlVars[i].FPPriority;
            FActiveVVCurve := Other.CtrlVars[i].FActiveVVCurve;
        end;
    end;

    ControlMode := Other.ControlMode;
    CombiMode := Other.CombiMode;
    FListSize := Other.FListSize;
    Fvvc_curve := Other.Fvvc_curve;
    Fvvc_curveOffset := Other.Fvvc_curveOffset;
    FVoltage_CurveX_ref := Other.FVoltage_CurveX_ref;
    Fvoltwatt_curve := Other.Fvoltwatt_curve;
    FvoltwattCH_curve := Other.FvoltwattCH_curve;
    Fwattpf_curve := Other.Fwattpf_curve;
    Fwattvar_curve := Other.Fwattvar_curve;
    FDbVMin := Other.FDbVMin;
    pf_wp_nominal := Other.pf_wp_nominal;
    FDbVMax := Other.FDbVMax;
    FArGraLowV := Other.FArGraLowV;
    FArGraHiV := Other.FArGraHiV;
    FRollAvgWindowLength := Other.FRollAvgWindowLength;
    FDRCRollAvgWindowLength := Other.FDRCRollAvgWindowLength;
    FActivePChangeTolerance := Other.FActivePChangeTolerance;
    FdeltaQ_factor := Other.FdeltaQ_factor;
    FdeltaP_factor := Other.FdeltaP_factor;
    FVoltageChangeTolerance := Other.FVoltageChangeTolerance;
    FVarChangeTolerance := Other.FVarChangeTolerance;
    FVoltwattYAxis := Other.FVoltwattYAxis;
    RateofChangeMode := Other.RateofChangeMode;
    LPFTau := Other.LPFTau;
    FRiseFallLimit := Other.FRiseFallLimit;
    FMonBusesPhase := Other.FMonBusesPhase;
    FMonBuses := Other.FMonBuses;
    FMonBusesNodes := Other.FMonBusesNodes;

    ReallocMem(FMonBusesVbase, SizeOf(Double) * MonBusesNameList.Count);
    for j := 1 to MonBusesNameList.Count do
        FMonBusesVbase[j] := Other.FMonBusesVbase[j];

    TimeDelay := Other.TimeDelay;
end;

constructor TInvControlObj.Create(ParClass: TDSSClass; const InvControlName: Ansistring);
begin
    inherited Create(ParClass);
    Name := AnsiLowerCase(InvControlName);
    DSSObjType := ParClass.DSSClassType;

    // Control elements are zero current sources that attach to a terminal of a
    // power-carrying device, but do not alter voltage or current flow.
    // Define a default number of phases and conductors here and update in
    // RecalcElementData routine if necessary. This allocates arrays for voltages
    // and currents and gives more direct access to the values,if needed
    FNPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors in base class
    ControlMode := NONE_MODE; // TODO: The docs say the default is "VoltVar"
    CombiMode := NONE_COMBMODE;
    ControlledElement := NIL;

    FVpuSolutionIdx := 0;

    // Variables for convergence process
    FdeltaQ_factor := FLAGDELTAQ;
    FdeltaP_factor := FLAGDELTAP;

    FVoltageChangeTolerance := 0.0001;
    FVarChangeTolerance := 0.025;
    FActivePChangeTolerance := 0.01;

    // Variables of DER element
    FDERPointerList := TDSSPointerList.Create(20);  // Default size and increment
    DERNameList := TSTringList.Create;

    // Variables for monitored Bus/buses
    MonBusesNameList := TStringList.Create;
    FMonBusesPhase := AVGPHASES;
    FMonBuses := NIL;
    FMonBusesVbase := NIL;
    FMonBusesNodes := NIL;

    // Variables for LPF and RF options
    RateofChangeMode := INACTIVE;
    LPFTau := 0.001; //TODO: documentation lists this as 0
    FRiseFallLimit := 0.001; //TODO: documentation lists this as -1 (disabled)

    // Variables of the smart inverter functions
    FVoltage_CurveX_ref := 0;
    FReacPower_ref := ReacPower_VARAVAL;
    FVoltwattYAxis := 1;

    // volt-var
    Fvvc_curve := NIL;
    Fvvc_curveOffset := 0.0;
    FRollAvgWindowLength := 1; //TODO: documentation lists this as 0

    // watt-pf
    Fwattpf_curve := NIL;
    pf_wp_nominal := 0.0;

    // watt-var
    Fwattvar_curve := NIL;

    // DRC
    FDbVMin := 0.95;
    FDbVMax := 1.05;
    FArGraLowV := 0.1;
    FArGraHiV := 0.1;
    FDRCRollAvgWindowLength := 1;

    // volt-watt
    Fvoltwatt_curve := NIL;
    FvoltwattCH_curve := NIL;

    // AVR
    Fv_setpoint := 1.0;

    CtrlModel := TInvControlModel.Linear;

    ShowEventLog := FALSE; // match SVN r3458
end;

destructor TInvControlObj.Destroy;
begin
    Finalize(FMonBuses);
    Finalize(FMonBusesNodes);
    Finalize(Fv_setpoint);
    if Assigned(FMonBusesVbase) then
        ReallocMem(FMonBusesVbase, 0);

    FreeAndNil(DERNameList);
    FreeAndNil(MonBusesNameList);

    inherited Destroy;
end;

procedure TInvControlObj.RecalcElementData();
var
    i: Integer;
begin
    if FDERPointerList.Count = 0 then
        MakeDERList;

    if FDERPointerList.Count > 0 then
    // Setting the terminal of the InvControl device to same as the 1st PVSystem/Storage element
    // This sets it to a realistic value to avoid crashes later
    begin
        MonitoredElement := TInvBasedPCE(FDERPointerList.Get(1));   // Set MonitoredElement to 1st elemnent in list
        Setbus(1, MonitoredElement.Firstbus);
    end;

    for i := 1 to FDERPointerList.Count do
    begin
        // User ControlledElement[] as the pointer to the PVSystem/Storage elements
        ControlledElement[i] := TInvBasedPCE(FDERPointerList.Get(i));  // pointer to i-th PVSystem/Storage element
        ControlledElement[i].ActiveTerminalIdx := 1; // Make the 1 st terminal active

        with CtrlVars[i] do
        begin
            SetLength(cBuffer, SizeOf(Complex) * ControlledElement[i].Yorder);
            FNphases := ControlledElement[i].NPhases;
            Nconds := Nphases; //TODO: check
            FRollAvgWindow.SetLength(FRollAvgWindowLength);
            FDRCRollAvgWindow.SetLength(FDRCRollAvgWindowLength);

            // for all modes other than VW and WATTPF, PF priority is not allowed
            if ((ControlMode <> VOLTWATT) and (ControlMode <> WATTPF)) then
                ControlledElement[i].SetPFPriority(FALSE);

            if Length(FMonBuses) = 0 then
                FUsingMonBuses := FALSE
            else
                FUsingMonBuses := TRUE;

            if (ControlledElement[i] <> NIL) then
                UpdateDERParameters(i)
            else
            begin
                // ControlledElement[i] := NIL;
                DoErrorMsg(Format(_('InvControl: "%s"'), [Self.Name]),
                    Format(_('Controlled Element "%s" not found.'), [DERNameList.Strings[i - 1]]),
                    _('PVSystem or Storage object must be defined previously.'), 361);
            end;
        end;
    end;
end;

procedure TInvControlObj.MakePosSequence();
// ***  This assumes the PVSystem/Storage devices have already been converted to pos seq
begin
    if FDERPointerList.Count = 0 then
        RecalcElementData();
    FNphases := 3;
    Nconds := 3;
    Setbus(1, MonitoredElement.GetBus(ElementTerminal));

    if FDERPointerList.Count > 0 then
    // Setting the terminal of the InvControl device to same as the 1st PVSystem/Storage element
    //  This sets it to a realistic value to avoid crashes later 
    begin
        MonitoredElement := TInvBasedPCE(FDERPointerList.Get(1));   // Set MonitoredElement to 1st PVSystem/Storage in list
        Setbus(1, MonitoredElement.Firstbus);
        FNphases := MonitoredElement.NPhases;
        Nconds := Nphases;
    end;
    inherited;
end;

procedure TInvControlObj.DoPendingAction(const Code, ProxyHdl: Integer);
var
    k: Integer;
    DERElem: TInvBasedPCE;
    DER_OL: Boolean;
begin
    for k := 1 to FDERPointerList.Count do
    begin
        DERElem := ControlledElement[k];
        with CtrlVars[k] do
        begin
            // Calculates QHeadRoom
            Calc_QHeadRoom(k);
            if QHeadRoom <> 0.0 then
                FPriorvarspu := FPriorvars / QHeadRoom;

            // Calculates PBase
            Calc_PBase(k);
            FPriorWattspu := FPriorWatts / PBase;

            // Calculates kW_out_desiredpu. Used for VW and VV_VW
            kW_out_desiredpu := kW_out_desired / PBase;

            // -------------------Smart Inverter Functions------------------------//
            // Smart Inverter volt-var function
            if (ControlMode = VOLTVAR) and (CombiMode = NONE_COMBMODE) and (FPendingChange = CHANGEVARLEVEL) then
            begin
                // Set var mode to VARMODEKVAR to indicate we might change kvar
                DERElem.VWmode := FALSE;
                DERElem.Varmode := VARMODEKVAR;
                DERElem.VVmode := TRUE;

                //--------------------------------------------- Main process ---------------------------------------------//

                CalcQVVcurve_desiredpu(k);

                // LPF or RF activated
                if (RateofChangeMode = LPF) then
                begin
                    CalcLPF(k, 'VARS', QDesireVVpu);
                    // Checks kVA (watt priority) and kvarlimit limits
                    Check_Qlimits(k, QDesireOptionpu);
                    QDesireEndpu := Min(abs(QDesireLimitedpu), abs(QDesireOptionpu)) * sign(QDesireOptionpu);
                end
                else
                if (RateofChangeMode = RISEFALL) then
                begin
                    CalcRF(k, 'VARS', QDesireVVpu);
                    // Checks kVA (watt priority) and kvarlimit limits
                    Check_Qlimits(k, QDesireOptionpu);
                    QDesireEndpu := Min(abs(QDesireLimitedpu), abs(QDesireOptionpu)) * sign(QDesireOptionpu);
                end
                else
                begin
                    // Checks kVA (watt priority) and kvarlimit limits
                    Check_Qlimits(k, QDesireVVpu);
                    QDesireEndpu := Min(abs(QDesireVVpu), abs(QDesireLimitedpu)) * sign(QDesireVVpu);
                end;

                // Calculates QDesiredVV through the convergence algorithm
                CalcVoltVar_vars(k);

                //--------------------------------------------- end Main process ---------------------------------------------//

                // Sets PVSystem/Storage's kvar_out
                if DERElem.IsPVSystem() then
                    TPVSystemObj(DERElem).Presentkvar := QDesiredVV
                else
                    TStorageObj(DERElem).kvarRequested := QDesiredVV;

                // Updates PresentkW and Presentkvar considering watt and var priorities
                DERElem.SetNominalDEROutput();
                if QDesiredVV >= 0.0 then
                    Qoutputpu := DERElem.Get_Presentkvar / QHeadroom
                else
                    Qoutputpu := DERElem.Get_Presentkvar / QHeadroomNeg;

                // Values used in convergence
                QoutputVVpu := Qoutputpu;
                FAvgpVpuPrior := FPresentVpu;

                // Values used in CalcQVVcurve_desiredpu
                QOld := DERElem.Get_Presentkvar;
                QOldVV := DERElem.Get_Presentkvar;
                if ShowEventLog then
                    AppendtoEventLog(Self.FullName + ', ' + DERElem.FullName,
                        Format('VOLTVAR mode requested DER output var level to **, kvar = %.5g. Actual output set to kvar= %.5g.',
                        [QDesiredVV, DERElem.Get_Presentkvar()]));
            end
            // Smart Inverter active voltage regulation function
            else
            if (ControlMode = AVR) and (CombiMode = NONE_COMBMODE) and (FPendingChange = CHANGEVARLEVEL) then
            begin
                // Set var mode to VARMODEKVAR to indicate we might change kvar
                DERElem.VWmode := FALSE;
                DERElem.Varmode := VARMODEKVAR;
                DERElem.AVRmode := TRUE;

                //--------------------------------------------- Main process ---------------------------------------------//
                if ActiveCircuit.Solution.ControlIteration = 1 then
                begin
                    FAvgpVpuPrior := FPresentVpu;
                    FAvgpAVRVpuPrior := FPresentVpu;

                    // Sets PVSystem/Storage's kvar_out
                    if DERElem.IsPVSystem() then
                        TPVSystemObj(DERElem).Presentkvar := QHeadRoom / 2
                    else
                        TStorageObj(DERElem).kvarRequested := QHeadRoom / 2;
                end
                else
                if ActiveCircuit.Solution.ControlIteration = 2 then
                begin
                    // Sets PVSystem/Storage's kvar_out
                    if DERElem.IsPVSystem() then
                        DQDV := abs(TPVSystemObj(DERElem).Presentkvar / QHeadRoom / (FPresentVpu - FAvgpVpuPrior))
                    else
                        DQDV := abs(TStorageObj(DERElem).kvarRequested / QHeadRoom / (FPresentVpu - FAvgpVpuPrior));
                end
                else
                begin
                    // Calculates QDesireAVRpu
                    CalcQAVR_desiredpu(k);

                    // Checks kVA (watt priority) and kvarlimit limits
                    Check_Qlimits(k, QDesireAVRpu);
                    QDesireEndpu := Min(abs(QDesireAVRpu), abs(QDesireLimitedpu)) * sign(QDesireAVRpu);

                    if abs(QDesireEndpu - QDesireLimitedpu) < 0.05 then
                        Fv_setpointLimited := FPresentVpu
                    else
                        Fv_setpointLimited := Fv_setpoint;

                    // Calculates QDesiredVV through the convergence algorithm
                    CalcAVR_vars(k);

                    //--------------------------------------------- end Main process ---------------------------------------------//

                    // Sets PVSystem/Storage's kvar_out
                    if DERElem.IsPVSystem() then
                        TPVSystemObj(DERElem).Presentkvar := QDesiredAVR
                    else
                        TStorageObj(DERElem).kvarRequested := QDesiredAVR;

                    // Uptates PresentkW and Presentkvar considering watt and var priorities
                    DERElem.SetNominalDEROutput();                    
                    if QDesiredAVR >= 0.0 then
                        Qoutputpu := DERElem.Get_Presentkvar / QHeadroom
                    else
                        Qoutputpu := DERElem.Get_Presentkvar / QHeadroomNeg;

                    // Values used in convergence
                    QoutputAVRpu := Qoutputpu;
                    FAvgpVpuPrior := FPresentVpu;

                    // Values used in CalcQVVcurve_desiredpu
                    QOld := DERElem.Get_Presentkvar;
                    QOldAVR := DERElem.Get_Presentkvar;

                    if ShowEventLog then
                        AppendtoEventLog(Self.FullName + ', ' + DERElem.FullName,
                            Format('VOLTVAR mode requested DER output var level to **, kvar = %.5g. Actual output set to kvar= %.5g.',
                            [QDesiredAVR, DERElem.Get_Presentkvar()]));
                end;
            end
            // Smart Inverter watt-pf function
            else
            if (ControlMode = WATTPF) and (CombiMode = NONE_COMBMODE) and (FPendingChange = CHANGEVARLEVEL) then
            begin
                // Set var mode to VARMODEKVAR to indicate we might change kvar
                DERElem.VWmode := FALSE;
                DERElem.Varmode := VARMODEKVAR;
                DERElem.WPmode := TRUE;

                //--------------------------------------------- Main process ---------------------------------------------//

                // Calculates QDesireWPpu
                CalcQWPcurve_desiredpu(k);

                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireWPpu);
                QDesireEndpu := Min(abs(QDesireWPpu), abs(QDesireLimitedpu)) * sign(QDesireWPpu);

                // Calculates QDesiredWP through the convergence algorithm
                CalcWATTPF_vars(k);

                //--------------------------------------------- end Main process ---------------------------------------------//
                // Sets PVSystem/Storage's pf_wp_nominal
                if DERElem.IsPVSystem() then
                    TPVSystemObj(DERElem).pf_wp_nominal := pf_wp_nominal
                else
                    TStorageObj(DERElem).kvarRequested := QDesiredWP;

                // Sets PVSystem/Storage's kvar_out
                if DERElem.IsPVSystem() then
                    TPVSystemObj(DERElem).Presentkvar := QDesiredWP
                else
                    TStorageObj(DERElem).kvarRequested := QDesiredWP;

                // Updates PresentkW and Presentkvar considering watt and var priorities
                DERElem.SetNominalDEROutput();
                if QDesiredWP >= 0.0 then
                    Qoutputpu := DERElem.Get_Presentkvar / QHeadroom
                else
                    Qoutputpu := DERElem.Get_Presentkvar / QHeadroomNeg;

                // Values used in convergence
                QoutputVVpu := Qoutputpu;
                FAvgpVpuPrior := FPresentVpu;

                // Values used in CalcQVVcurve_desiredpu
                QOld := DERElem.Get_Presentkvar;
                QOldVV := DERElem.Get_Presentkvar;

                if ShowEventLog then
                    AppendtoEventLog(Self.FullName + ', ' + DERElem.FullName,
                        Format('WATTPF mode requested DER output var level to **, kvar = %.5g. Actual output set to kvar= %.5g.',
                        [QDesiredWP, DERElem.Get_Presentkvar()]));
            end
            // Smart Inverter watt-var function
            else
            if (ControlMode = WATTVAR) and (CombiMode = NONE_COMBMODE) and (FPendingChange = CHANGEVARLEVEL) then
            begin
                // Set var mode to VARMODEKVAR to indicate we might change kvar
                DERElem.VWmode := FALSE;
                DERElem.Varmode := VARMODEKVAR;
                DERElem.WVmode := TRUE;

                //--------------------------------------------- Main process ---------------------------------------------//
                // Calculates QDesireWVpu
                CalcQWVcurve_desiredpu(k);

                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits_WV(k, QDesireWVpu);
                QDesireEndpu := Min(abs(QDesireWVpu), abs(QDesireLimitedpu)) * sign(QDesireWVpu);

                // It checks kVA or Q limits and makes sure the final P and Q stay in the watt-var curve (PauloRadatz - 2/16/2021)
                Calc_PQ_WV(k);

                //--------------------------------------------- end Main process ---------------------------------------------//

                // Sets PVSystem/Storage's kvar_out
                if DERElem.IsPVSystem() then
                begin
                    TPVSystemObj(DERElem).Presentkvar := QDesiredWV;
                    TPVSystemObj(DERElem).PresentkW := PLimitEndpu * Min(FkVARating, FDCkWRated);
                end
                else
                    TStorageObj(DERElem).kvarRequested := QDesiredWV;

                // Uptates PresentkW and Presentkvar considering watt and var priorities
                DERElem.SetNominalDEROutput();
                if QDesiredWV >= 0.0 then
                    Qoutputpu := DERElem.Get_Presentkvar / QHeadroom
                else
                    Qoutputpu := DERElem.Get_Presentkvar / QHeadroomNeg;

                // Values used in convergence
                QoutputVVpu := Qoutputpu;
                FAvgpVpuPrior := FPresentVpu;

                // Values used in CalcQVVcurve_desiredpu
                QOld := DERElem.Get_Presentkvar;
                QOldVV := DERElem.Get_Presentkvar;

                if ShowEventLog then
                    AppendtoEventLog(Self.FullName + ', ' + DERElem.FullName,
                        Format('WATTVAR mode requested DER output var level to **, kvar = %.5g. Actual output set to kvar= %.5g.',
                        [QDesiredWV, DERElem.Get_Presentkvar()]));
            end
            // Smart Inverter DRC function
            else
            if (ControlMode = DRC) and (CombiMode = NONE_COMBMODE) and (FPendingChange = CHANGEVARLEVEL) then
            begin
                // Set var mode to VARMODEKVAR to indicate we might change kvar
                DERElem.VWmode := FALSE;
                DERElem.Varmode := VARMODEKVAR;
                DERElem.DRCmode := TRUE;

                //--------------------------------------------- Main process ---------------------------------------------//

                // Calculates QDesireDRCpu
                CalcQDRC_desiredpu(k);

                // LPF or RF activated
                if (RateofChangeMode = LPF) then
                begin
                    CalcLPF(k, 'VARS', QDesireDRCpu);
                    // Checks kVA (watt priority) and kvarlimit limits
                    Check_Qlimits(k, QDesireOptionpu);
                    QDesireEndpu := Min(abs(QDesireLimitedpu), abs(QDesireOptionpu)) * sign(QDesireOptionpu);
                end
                else
                if (RateofChangeMode = RISEFALL) then
                begin
                    CalcRF(k, 'VARS', QDesireDRCpu);
                    // Checks kVA (watt priority) and kvarlimit limits
                    Check_Qlimits(k, QDesireOptionpu);
                    QDesireEndpu := Min(abs(QDesireLimitedpu), abs(QDesireOptionpu)) * sign(QDesireOptionpu);
                end
                else
                begin
                    // Checks kVA (watt priority) and kvarlimit limits
                    Check_Qlimits(k, QDesireDRCpu);
                    QDesireEndpu := Min(abs(QDesireDRCpu), abs(QDesireLimitedpu)) * sign(QDesireDRCpu);
                end;

                // Calculates QDesiredDRC
                CalcDRC_vars(k);

                //--------------------------------------------- end main process ---------------------------------------------//

                // Sets DER kvar_out
                if DERElem.IsPVSystem() then
                    TPVSystemObj(DERElem).Presentkvar := QDesiredDRC
                else
                    TStorageObj(DERElem).kvarRequested := QDesiredDRC;

                // Uptates PresentkW and Presentkvar considering watt and var priorities
                DERElem.SetNominalDEROutput();
                if QDesiredDRC >= 0.0 then
                    Qoutputpu := DERElem.Get_Presentkvar / QHeadroom
                else
                    Qoutputpu := DERElem.Get_Presentkvar / QHeadroomNeg;

                // Values used in convergence
                QoutputDRCpu := Qoutputpu;
                FAvgpDRCVpuPrior := FPresentDRCVpu;

                // Values used in CalcDRC_vars
                QOld := DERElem.Get_Presentkvar;
                QOldDRC := DERElem.Get_Presentkvar;

                if ShowEventLog then
                    AppendtoEventLog(Self.FullName + ', ' + DERElem.FullName,
                        Format('DRC mode requested DER output var level to **, kvar = %.5g. Actual output set to kvar = %.5g.',
                        [QDesiredDRC, DERElem.Get_Presentkvar()]));
            end
            // Smart Inverter VV_DRC function
            else
            if (ControlMode = NONE_MODE) and (CombiMode = VV_DRC) and (FPendingChange = CHANGEDRCVVARLEVEL) then
            begin
                // Set var mode to VARMODEKVAR to indicate we might change kvar
                DERElem.VWmode := FALSE;
                DERElem.Varmode := VARMODEKVAR;
                DERElem.VVmode := TRUE;
                DERElem.DRCmode := TRUE;

                //--------------------------------------------- Main process ---------------------------------------------//

                // Calculates QDesireVVpu and  QDesireDRCpu
                CalcQVVcurve_desiredpu(k);
                CalcQDRC_desiredpu(k);

                // LPF or RF activated
                if (RateofChangeMode = LPF) then
                begin
                    CalcLPF(k, 'VARS', QDesireVVpu + QDesireDRCpu);
                    // Checks kVA (watt priority) and kvarlimit limits
                    Check_Qlimits(k, QDesireOptionpu);
                    QDesireEndpu := Min(abs(QDesireLimitedpu), abs(QDesireOptionpu)) * sign(QDesireOptionpu);
                end
                else
                if (RateofChangeMode = RISEFALL) then
                begin
                    CalcRF(k, 'VARS', QDesireVVpu + QDesireDRCpu);
                    // Checks kVA (watt priority) and kvarlimit limits
                    Check_Qlimits(k, QDesireOptionpu);
                    QDesireEndpu := Min(abs(QDesireLimitedpu), abs(QDesireOptionpu)) * sign(QDesireOptionpu);
                end
                else
                begin
                    // Checks kVA (watt priority) and kvarlimit limits
                    Check_Qlimits(k, QDesireVVpu + QDesireDRCpu);
                    QDesireEndpu := Min(abs(QDesireVVpu + QDesireDRCpu), abs(QDesireLimitedpu)) * sign(QDesireVVpu + QDesireDRCpu);
                end;

                // Calculates QDesiredVVDRC
                CalcVVDRC_vars(k);

                //--------------------------------------------- end main process ---------------------------------------------//

                // Sets DER kvar_out
                if DERElem.IsPVSystem() then
                    TPVSystemObj(DERElem).Presentkvar := QDesiredVVDRC
                else
                    TStorageObj(DERElem).kvarRequested := QDesiredVVDRC;

                // Updates PresentkW and Presentkvar considering watt and var priorities
                DERElem.SetNominalDEROutput();

                if QDesiredVVDRC >= 0.0 then
                    Qoutputpu := DERElem.Get_Presentkvar / QHeadroom
                else
                    Qoutputpu := DERElem.Get_Presentkvar / QHeadroomNeg;

                // Values used in convergence
                QoutputVVDRCpu := Qoutputpu;
                FAvgpVpuPrior := FPresentVpu;
                FAvgpDRCVpuPrior := FPresentDRCVpu;

                // Values used in CalcQVVcurve_desiredpu and CalcVVDRC_vars
                QOld := DERElem.Get_Presentkvar;
                QOldVVDRC := DERElem.Get_Presentkvar;

                if ShowEventLog then
                    AppendtoEventLog(Self.FullName + ', ' + DERElem.FullName,
                        Format('**VV_DRC mode requested DER output var level to **, kvar = %.5g. Actual output set to kvar = %.5g.',
                        [QDesiredVVDRC, DERElem.Get_Presentkvar()]));
            end
            // Smart Inverter volt-watt function
            else
            if (ControlMode = VOLTWATT) and (CombiMode = NONE_COMBMODE) and (FPendingChange = CHANGEWATTLEVEL) then
            begin
                DERElem.VWmode := TRUE;

                //--------------------------------------------- Main process ---------------------------------------------//
                // Calculates QVWcurve_limitpu
                CalcPVWcurve_limitpu(k);

                // LPF or RF activated
                if (RateofChangeMode = LPF) then
                begin
                    CalcLPF(k, 'WATTS', PLimitVWpu);
                    // Checks kVA (var priority) and pctPmpp limits
                    Check_Plimits(k, PLimitOptionpu);
                    PLimitEndpu := Min(PLimitLimitedpu, PLimitOptionpu);
                end
                else
                if (RateofChangeMode = RISEFALL) then
                begin
                    CalcRF(k, 'WATTS', PLimitVWpu);
                    // Checks kVA (var priority) and pctPmpp limits
                    Check_Plimits(k, PLimitOptionpu);
                    PLimitEndpu := Min(PLimitLimitedpu, PLimitOptionpu);
                end
                else
                begin
                    // Checks kVA (var priority) and pctPmpp limits
                    Check_Plimits(k, PLimitVWpu);
                    PLimitEndpu := Min(abs(PLimitLimitedpu), abs(PLimitVWpu)) * sign(PLimitVWpu);
                end;

                // Calculates PLimitVW through the convergence algorithm
                CalcVoltWatt_watts(k);

                //--------------------------------------------- end main process ---------------------------------------------//
                // Sets DER kW_out
                if DERElem.IsPVSystem() then
                begin
                    TPVSystemObj(DERElem).PresentkW := PLimitVW;

                    // Updates PresentkW and Presentkvar considering watt and var priorities
                    TPVSystemObj(DERElem).SetNominalDEROutput();

                end
                else
                begin
                    TStorageObj(DERElem).kWRequested := PLimitVW;

                    // Updates PresentkW and Presentkvar considering watt and var priorities
                    TStorageObj(DERElem).SetNominalDEROutput();
                end;

                // Values used in convergence
                FAvgpVpuPrior := FPresentVpu;
                POldVWpu := PLimitVW / PBase;

                // Flag has to do set to 0 when kW_out is lower than Ptemp (max power allowed from volt-watt function)
                if DERElem.IsPVSystem() then
                begin
                    if ((abs(PLimitVW) > 0.0) and (abs(TPVSystemObj(DERElem).presentkW - PLimitVW) / PLimitVW > 0.0001)) then
                        FVWOperation := 0; // 0.01% is the value chosen at the moment

                    if ShowEventLog then
                        AppendtoEventLog(Self.FullName + ', ' + TPVSystemObj(DERElem).FullName,
                            Format('**VOLTWATT mode set PVSystem kW output limit to **, kW= %.5g. Actual output is kW= %.5g.',
                            [PLimitVW, TPVSystemObj(DERElem).presentkW]));
                end
                else
                begin
                    if abs(abs(TStorageObj(DERElem).presentkW) - PLimitVW) / PLimitVW > 0.0001 then
                        FVWOperation := 0; // 0.01% is the value chosen at the moment

                    if ShowEventLog then
                        AppendtoEventLog(Self.FullName + ', ' + TStorageObj(DERElem).FullName,
                            Format('**VOLTWATT mode set Storage kW output limit to ** kW= %.5g. Actual output is kW= %.5g.',
                            [PLimitVW, TStorageObj(DERElem).presentkW]));

                end;
            end
            else
            if (ControlMode = NONE_MODE) and (CombiMode = VV_VW) and (FPendingChange = CHANGEWATTVARLEVEL) then
            begin
                DERElem.VWmode := TRUE;
                DERElem.Varmode := VARMODEKVAR;
                DERElem.VVmode := TRUE;

                //--------------------------------------------- Main process ---------------------------------------------//
                // Calculates QDesireVVpu and QVWcurve_limitpu
                CalcPVWcurve_limitpu(k);
                CalcQVVcurve_desiredpu(k);

                // LPF or RF activated
                if (RateofChangeMode = LPF) then
                begin
                    CalcLPF(k, 'VARS', QDesireVVpu);
                    // Checks kVA (watt priority) and kvarlimit limits
                    Check_Qlimits(k, QDesireOptionpu);
                    QDesireEndpu := Min(abs(QDesireLimitedpu), abs(QDesireOptionpu)) * sign(QDesireOptionpu);

                    CalcLPF(k, 'WATTS', PLimitVWpu);
                    // Checks kVA (var priority) and pctPmpp limits
                    Check_Plimits(k, PLimitOptionpu);
                    PLimitEndpu := Min(PLimitLimitedpu, PLimitOptionpu);
                end
                else
                if (RateofChangeMode = RISEFALL) then
                begin
                    CalcRF(k, 'VARS', QDesireVVpu);
                    // Checks kVA (watt priority) and kvarlimit limits
                    Check_Qlimits(k, QDesireOptionpu);
                    QDesireEndpu := Min(abs(QDesireLimitedpu), abs(QDesireOptionpu)) * sign(QDesireOptionpu);

                    CalcRF(k, 'WATTS', PLimitVWpu);
                    // Checks kVA (var priority) and pctPmpp limits
                    Check_Plimits(k, PLimitOptionpu);
                    PLimitEndpu := Min(PLimitLimitedpu, PLimitOptionpu);
                end
                else
                begin
                    // Checks kVA (watt priority) and kvarlimit limits
                    Check_Qlimits(k, QDesireVVpu);
                    QDesireEndpu := Min(abs(QDesireVVpu), abs(QDesireLimitedpu)) * sign(QDesireVVpu);

                    // Checks kVA (var priority) and pctPmpp limits
                    Check_Plimits(k, PLimitVWpu);
                    PLimitEndpu := Min(abs(PLimitLimitedpu), abs(PLimitVWpu)) * sign(PLimitVWpu);
                end;

                // Calculates PLimitVW and QDesiredVV through the convergence algorithm
                CalcVoltWatt_watts(k);
                CalcVoltVar_vars(k);

                //--------------------------------------------- end main process ---------------------------------------------//

                // Sets DER kvar_out and kW_out
                if DERElem.IsPVSystem() then
                begin
                    TPVSystemObj(DERElem).Presentkvar := QDesiredVV;
                    TPVSystemObj(DERElem).presentkW := PLimitVW;
                end
                else
                begin
                    TStorageObj(DERElem).kvarRequested := QDesiredVV;
                    TStorageObj(DERElem).kWRequested := PLimitVW;
                end;

                // Updates PresentkW and Presentkvar considering watt and var priorities
                DERElem.SetNominalDEROutput();

                if QDesiredVV >= 0.0 then
                    Qoutputpu := DERElem.Get_Presentkvar / QHeadroom
                else
                    Qoutputpu := DERElem.Get_Presentkvar / QHeadroomNeg;

                // Values used in convergence
                QoutputVVpu := Qoutputpu;
                FAvgpVpuPrior := FPresentVpu;
                POldVWpu := PLimitVW / PBase;

                // Values used in CalcQVVcurve_desiredpu
                QOld := DERElem.Get_Presentkvar;
                QOldVV := DERElem.Get_Presentkvar;

                if ShowEventLog then
                    AppendtoEventLog(Self.FullName + ', ' + DERElem.FullName,
                        Format('**VV_VW mode requested DER output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.',
                        [QDesiredVV, DERElem.Get_Presentkvar()]));

                // Flag has to do set to 0 when kW_out is lower than Ptemp (max power allowed from volt-watt function)
                if DERElem.IsPVSystem() then
                begin
                    if abs(TPVSystemObj(DERElem).presentkW - PLimitVW) / PLimitVW > 0.0001 then
                        FVWOperation := 0; // 0.01% is the value chosen at the moment

                    if ShowEventLog then
                        AppendtoEventLog(Self.FullName + ', ' + TPVSystemObj(DERElem).FullName,
                            Format('**VV_VW mode set PVSystem kW output limit to **, kW= %.5g. Actual output is kW= %.5g.',
                            [PLimitVW, TPVSystemObj(DERElem).presentkW]));
                end
                else
                begin
                    if abs(abs(TStorageObj(DERElem).presentkW) - PLimitVW) / PLimitVW > 0.0001 then
                        FVWOperation := 0; // 0.01% is the value chosen at the moment

                    if ShowEventLog then
                        AppendtoEventLog(Self.FullName + ', ' + TStorageObj(DERElem).FullName,
                            Format('**VV_VW mode set Storage kW output limit to** kW= %.5g. Actual output is kW= %.5g.',
                            [PLimitVW, TStorageObj(DERElem).presentkW]));
                end;
            end
            // Grid forming inverter
            else
            if (ControlMode = GFM) then
            begin
                DER_OL := False;
                if DERElem.GFM_Mode then
                begin
                    if DERElem.IsStorage() then
                    begin
                        // If there is no Amps limit, check OL
                        if (DERElem.dynVars.ILimit <= 0) and DERElem.CheckOLInverter() then
                        begin
                            if not ActiveCircuit.Solution.IsDynamicModel then
                            begin
                                DER_OL := True;
                                TStorageObj(DERElem).StorageState := 0;  // It's burning, Turn it off
                                TStorageObj(DERElem).StateChanged := TRUE;
                            end
                            else
                                DERElem.dynVars.ResetIBR := True; // The dynamic alg will take it to safety
                        end;
                    end
                    else
                    begin
                        if not ActiveCircuit.Solution.IsDynamicModel then
                            DER_OL := DERElem.CheckOLInverter()
                        else
                        if DERElem.CheckOLInverter() then
                            DERElem.dynVars.ResetIBR := True;
                    end;

                    if DER_OL then
                    begin
                        DERElem.GFM_Mode := False;
                        DERElem.YprimInvalid := TRUE;
                    end;
                end;
            end;
        end;
        ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
        Set_PendingChange(NONE, k);
        DERElem := NIL;
    end;
end;

procedure TInvControlObj.GetMonVoltage(var Vpresent: Double; i: Integer; BasekV: Double);
var
    j: Integer;
    rBus: TDSSBus;
    numNodes: Integer;
    vi: Complex;
    vj: Complex;
    DERElem: TInvBasedPCE;
begin
    DERElem := ControlledElement[i];
    with CtrlVars[i] do
        if FUsingMonBuses then
        begin
            for j := 0 to Length(FMonBuses) - 1 do
            begin
                FMonBusesIndex := ActiveCircuit.BusList.Find(FMonBuses[j]);
                rBus := ActiveCircuit.Buses[FMonBusesIndex];

                if (length(FMonBusesNodes[j]) = 2) then
                begin
                    vi := (ActiveCircuit.Solution.NodeV[rBus.GetRef(FMonBusesNodes[j][0])]);
                    vj := (ActiveCircuit.Solution.NodeV[rBus.GetRef(FMonBusesNodes[j][1])]);
                    cBuffer[j] := (vi - vj) * (BasekV * 1000.0 / FMonBusesVbase[j + 1]); // TODO: NIL check?
                end
                else
                begin
                    cBuffer[j] := ActiveCircuit.Solution.NodeV[rBus.GetRef(FMonBusesNodes[j][0])] * (BasekV * 1000.0 / FMonBusesVbase[j + 1]);
                end;
            end;

            case FMonBusesPhase of
                AVGPHASES:
                begin
                    Vpresent := 0.0;
                    for j := 0 to Length(FMonBuses) - 1 do
                        Vpresent := Vpresent + Cabs(cBuffer[j]);
                    Vpresent := Vpresent / Length(FMonBuses);
                end;
                MAXPHASE:
                begin
                    Vpresent := 0.0;
                    for j := 0 to Length(FMonBuses) - 1 do
                        Vpresent := Max(Vpresent, Cabs(cBuffer[j]));
                end;
                MINPHASE:
                begin
                    Vpresent := 1.0E50;
                    for j := 0 to Length(FMonBuses) - 1 do
                        Vpresent := Min(Vpresent, Cabs(cBuffer[j]));
                end;
            else
                Vpresent := Cabs(cBuffer[FMonBusesPhase]);
            end;
        end
        else
        begin
            DERElem.ComputeVTerminal();

            numNodes := DERElem.NPhases;

            for j := 1 to numNodes do
                cBuffer[j] := DERElem.Vterminal[j];


            case FMonBusesPhase of
                AVGPHASES:
                begin
                    Vpresent := 0.0;
                    for j := 1 to numNodes do
                        Vpresent := Vpresent + Cabs(cBuffer[j]);
                    Vpresent := Vpresent / numNodes;
                end;
                MAXPHASE:
                begin
                    Vpresent := 0.0;
                    for j := 1 to numNodes do
                        Vpresent := Max(Vpresent, Cabs(cBuffer[j]));
                end;
                MINPHASE:
                begin
                    Vpresent := 1.0E50;
                    for j := 1 to numNodes do
                        Vpresent := Min(Vpresent, Cabs(cBuffer[j]));
                end;
            else
                Vpresent := Cabs(cBuffer[FMonBusesPhase]);
            end;
        end;
end;

procedure TInvControlObj.UpdateDERParameters(i: Integer);
var
    DERElem: TInvBasedPCE;
begin
    DERElem := ControlledElement[i];
    with CtrlVars[i], DERElem do
        if DERElem.IsPVSystem() then
        begin
            with TPVSystemObj(DERElem) do
            begin
                CondOffset := (NTerms - 1) * NCondsDER; // for speedy sampling

                FVBase := Vbase;
                FVarFollowInverter := VarFollowInverter;
                FInverterON := InverterON;
                FpresentkW := PresentkW;
                FkVARating := kVARating;
                Fpresentkvar := Presentkvar;
                FkvarLimit := kvarLimit;
                FkvarLimitNeg := kvarLimitNeg;
                FCurrentkvarLimit := CurrentkvarLimit;
                FCurrentkvarLimitNeg := CurrentkvarLimitNeg;
                FDCkWRated := Pmpp;
                FpctDCkWRated := puPmpp;
                FEffFactor := PVSystemVars.EffFactor;
                FDCkW := PVSystemVars.PanelkW;
                FPPriority := PVSystemVars.P_Priority;

            end;
        end
        else
        if DERElem.IsStorage() then
        begin
            with TStorageObj(DERElem) do
            begin
                FVBase := Vbase;
                FVarFollowInverter := VarFollowInverter;
                FInverterON := InverterON;
                FpresentkW := PresentkW;
                FkVARating := kVARating;
                Fpresentkvar := Presentkvar;
                FkvarLimit := kvarLimit;
                FkvarLimitNeg := kvarLimitNeg;
                FCurrentkvarLimit := CurrentkvarLimit;
                FCurrentkvarLimitNeg := CurrentkvarLimitNeg;
                FDCkWRated := StorageVars.kWrating;
                FpctDCkWRated := StorageVars.pctkWrated;
                FEffFactor := Storagevars.EffFactor;
                FDCkW := 0.0; // not using it (using TStorageObj.DCkW directly)
                FPPriority := StorageVars.P_priority;
            end
        end;
end;

procedure TInvControlObj.Sample();
var
    valid: Boolean;
    i: Integer;
    basekV: Double;
    Vpresent: Double;
    PVSys: TPVSystemObj = NIL;
    Storage: TStorageObj = NIL;
    DERElem: TInvBasedPCE;
begin
    // if list is not defined, go make one from all PVSystem/Storage in circuit
    if FDERPointerList.Count = 0 then
        RecalcElementData();

    if (FListSize <= 0) then
        Exit;

    // if an InvControl controls more than one PVSystem/Storage, control each one
    // separately based on the PVSystem/Storage's terminal voltages, etc.
    for i := 1 to FDERPointerList.Count do
    begin
        with CtrlVars[i] do
        begin
            UpdateDERParameters(i);
            DERElem := ControlledElement[i];
            if DERElem.IsPVSystem() then
                PVSys := DERElem as TPVSystemObj
            else
                Storage := DERElem as TStorageObj;

            BasekV := FVBase / 1000.0; // It's a line-to-ground voltage

            GetMonVoltage(Vpresent, i, BasekV);

            // for reporting Vpriorpu correctly in EventLog (this update is normally perform at DoPendingAction)
            if ActiveCircuit.Solution.ControlIteration = 1 then
            begin
                FAvgpVpuPrior := FPresentVpu;
                FAvgpDRCVpuPrior := FPresentDRCVpu;
            end;

            kW_out_desired := FpresentkW; // necessary to update kW_out_desired at every control iteration for Storage with SC

            // Help says that it must be used just for vv and vw
            // convert to per-unit on bus' kVBase, or
            // if using averaging window values, then set prior voltage to averaging window
            if (FVoltage_CurveX_ref = 1) and (FRollAvgWindow.AvgVal <> 0.0) then
                FPresentVpu := Vpresent / (FRollAvgWindow.AvgVal)
            else
            if (FVoltage_CurveX_ref = 2) and (FRollAvgWindow.AvgVal <> 0.0) then
                FPresentVpu := (FRollAvgWindow.AvgVal) / (basekV * 1000.0)
            else
                FPresentVpu := Vpresent / (BasekV * 1000.0);

            FPresentDRCVpu := Vpresent / (BasekV * 1000.0);

            // Sets internal variables of controlled element.
            // FVreg is the pu voltage used in the volt-var and volt-watt curves
            FVreg := FPresentVpu;

            // First, determine what control mode are we
            if CombiMode <> NONE_COMBMODE then
                case CombiMode of
                    VV_DRC:
                    begin
                        // Sets internal variables of controlled element.
                        // FVVDRCOperation is a flag which indicates if VVDRC function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)
                        if DERElem.IsPVSystem() then
                        begin
                            PVSys.Set_Variable(5, FVreg);
                            PVSys.Set_Variable(6, FDRCRollAvgWindow.AvgVal / (basekV * 1000.0)); // save rolling average voltage in monitor
                            PVSys.Set_Variable(10, FVVDRCOperation);
                        end
                        else
                        begin
                            Storage.Set_Variable(14, FVreg);
                            Storage.Set_Variable(15, FDRCRollAvgWindow.AvgVal / (basekV * 1000.0)); // save rolling average voltage in monitor
                            Storage.Set_Variable(19, FVVDRCOperation);
                        end;

                        // if inverter is off then exit
                        if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then
                            continue;

                        // if the volt-var curve does not exist, exit
                        if Fvvc_curve = NIL then
                        begin
                            DoSimpleMsg(_('XY Curve object representing vvc1_curve does not exist or is not tied to InvControl.'), 382);
                            exit
                        end;

                        DERElem.VVmode := TRUE;
                        DERElem.DRCmode := TRUE;

                        //DRC triggers
                        if (priorDRCRollAvgWindow = 0.0) then
                        begin
                            if (Abs(FPresentDRCVpu - FAvgpDRCVpuPrior) > FVoltageChangeTolerance) or
                                (Abs(FPresentVpu - FAvgpVpuPrior) > FVoltageChangeTolerance) then
                            begin
                                // Resets DER state variable only if it has not converged yet
                                FVVDRCOperation := 0.0;

                                Set_PendingChange(CHANGEDRCVVARLEVEL, i);

                                ControlActionHandle := ActiveCircuit.ControlQueue.Push(TimeDelay, PendingChange[i], 0, Self);

                                if ShowEventLog then
                                    AppendtoEventLog(Self.FullName + ', ' + DERElem.FullName,
                                        Format(_('**Ready to change var output due to DRC trigger in VV_DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g'),
                                        [FPresentDRCVpu, FAvgpDRCVpuPrior]));
                            end;

                        end;

                        //Trigger from volt-var mode
                        if (((Abs(FPresentVpu - FAvgpVpuPrior) > FVoltageChangeTolerance) or
                            (Abs(FPresentDRCVpu - FAvgpDRCVpuPrior) > FVoltageChangeTolerance) or
                            ((Abs(Abs(QoutputVVDRCpu) - Abs(QDesireEndpu)) > FVarChangeTolerance))) or
                            (ActiveCircuit.Solution.ControlIteration = 1)) then
                        begin
                            // Resets DER state variable only if it has not converged yet
                            FVVDRCOperation := 0.0;

                            Set_PendingChange(CHANGEDRCVVARLEVEL, i);
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push(TimeDelay, PendingChange[i], 0, Self);

                            if ShowEventLog then
                                AppendtoEventLog(Self.FullName + ', ' + DERElem.FullName,
                                    Format(_('**Ready to change VV_DRC output due to volt-var trigger in VV_DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g'),
                                    [FPresentVpu, FAvgpVpuPrior]));

                        end;
                    end;
                    VV_VW:
                    begin
                        // Sets internal variables of controlled element.
                        // FVVOperation is a flag which indicates if volt-var function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)
                        // FVWOperation is a flag which indicates if volt-watt function operates or not
                        // Combined modes operation is shown through TWO flags. It allows us to verify which of the individual function operates or not

                        if DERElem.IsPVSystem() then
                        begin
                            PVSys.Set_Variable(5, FVreg);
                            PVSys.Set_Variable(7, FVVOperation);
                            PVSys.Set_Variable(8, FVWOperation);
                        end
                        else
                        begin
                            Storage.Set_Variable(14, FVreg);
                            Storage.Set_Variable(16, FVVOperation);
                            Storage.Set_Variable(17, FVWOperation);
                        end;

                        // if inverter is off then exit
                        if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then
                            continue;

                        // if volt-watt curve does not exist, exit
                        if DERElem.IsPVSystem() then
                        begin
                            if Fvoltwatt_curve = NIL then
                            begin
                                DoSimpleMsg(_('XY Curve object representing voltwatt_curve does not exist or is not tied to InvControl.'), 381);
                                exit
                            end;
                        end
                        else
                        begin
                            if (Fvoltwatt_curve = NIL) and (FvoltwattCH_curve = NIL) then
                            begin
                                DoSimpleMsg(_('XY Curve object representing voltwatt_curve does not exist or is not tied to InvControl.'), 381);
                                exit
                            end;
                        end;

                        // if the volt-var curve does not exist, exit
                        if Fvvc_curve = NIL then
                        begin
                            DoSimpleMsg(_('XY Curve object representing vvc1_curve does not exist or is not tied to InvControl.'), 382);
                            exit
                        end;

                        DERElem.VVmode := TRUE;
                        DERElem.VWmode := TRUE;

                        // Trigger from volt-watt mode
                        if ((Abs(FPresentVpu - FAvgpVpuPrior) > FVoltageChangeTolerance) or (Abs(PLimitEndpu - POldVWpu) > FActivePChangeTolerance) or
                            (ActiveCircuit.Solution.ControlIteration = 1)) then

                        begin
                            // Resets DER state variable only if it has not converged yet
                            FVWOperation := 0;

                            Set_PendingChange(CHANGEWATTVARLEVEL, i);

                            ControlActionHandle := ActiveCircuit.ControlQueue.Push(TimeDelay, PendingChange[i], 0, Self);

                            if ShowEventLog then
                                AppendtoEventLog(Self.FullName + ', ' + DERElem.FullName,
                                    Format('**Ready to change VV_VW output due to volt-watt trigger**, Vavgpu= %.5g, VPriorpu=%.5g',
                                    [FPresentVpu, FAvgpVpuPrior]));
                            ;
                        end;

                        //Trigger from volt-var mode
                        if (((Abs(FPresentVpu - FAvgpVpuPrior) > FVoltageChangeTolerance) or
                            ((Abs(Abs(Qoutputpu) - Abs(QDesireEndpu)) > FVarChangeTolerance))) or
                            (ActiveCircuit.Solution.ControlIteration = 1)) then

                        begin
                            // Resets DER state variable only if it has not converged yet
                            FVVOperation := 0;
                            Set_PendingChange(CHANGEWATTVARLEVEL, i);
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push(TimeDelay, PendingChange[i], 0, Self);

                            if ShowEventLog then
                                AppendtoEventLog(Self.FullName + ', ' + DERElem.FullName,
                                    Format('**Ready to change VV_VW output due to volt-var trigger**, Vavgpu= %.5g, VPriorpu=%.5g',
                                    [FPresentVpu, FAvgpVpuPrior]));
                        end;
                    end;
                else
                    // Do nothing
                end
            else
            case ControlMode of
                NONE_MODE:
                begin
                    // Do nothing...
                end;
                VOLTWATT:  // volt-watt control mode
                begin
                    // Sets internal variables of controlled element.
                    // FVWOperation is a flag which indicates if volt-watt function operates or not

                    if DERElem.IsPVSystem() then
                    begin
                        PVSys.Set_Variable(5, FVreg);
                        PVSys.Set_Variable(8, FVWOperation);
                    end
                    else
                    begin
                        Storage.Set_Variable(14, FVreg);
                        Storage.Set_Variable(17, FVWOperation);
                    end;

                    if (FInverterON = FALSE) then
                        continue;

                    if DERElem.IsPVSystem() then
                    begin
                        if Fvoltwatt_curve = NIL then
                        begin
                            DoSimpleMsg(_('XY Curve object representing voltwatt_curve does not exist or is not tied to InvControl.'), 381);
                            exit
                        end;
                    end
                    else
                    begin
                        if (Fvoltwatt_curve = NIL) and (FvoltwattCH_curve = NIL) then
                        begin
                            DoSimpleMsg(_('XY Curve object representing voltwatt_curve does not exist or is not tied to InvControl.'), 381);
                            exit
                        end;

                    end;

                    DERElem.VWmode := TRUE;

                    if ((Abs(FPresentVpu - FAvgpVpuPrior) > FVoltageChangeTolerance) or (Abs(PLimitEndpu - POldVWpu) > FActivePChangeTolerance) or
                        (ActiveCircuit.Solution.ControlIteration = 1)) then
                    begin
                        // Resets DER state variable only if it has not converged yet
                        FVWOperation := 0;

                        Set_PendingChange(CHANGEWATTLEVEL, i);

                        ControlActionHandle := ActiveCircuit.ControlQueue.Push(TimeDelay, PendingChange[i], 0, Self);
                        if ShowEventLog then
                            AppendtoEventLog(Self.FullName + ', ' + DERElem.FullName,
                                Format('**Ready to limit watt output due to VOLTWATT mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                                [FPresentVpu, FAvgpVpuPrior]));
                    end;
                end;
                AVR: // Active voltage regulation control mode
                begin
                    // Sets internal variables of PVSystem/Storage.
                    // FAVROperation is a flag which indicates if volt-var function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)


                    // if inverter is off then exit
                    if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then
                        continue;


                    if (DERElem.IsPVSystem()) then
                        PVSys.AVRmode := TRUE
                    else
                        Storage.VVmode := TRUE;

                    //Trigger from AVR mode

                    if (((Abs(FPresentVpu - FAvgpVpuPrior) > FVoltageChangeTolerance) or
                        ((Abs(Abs(QoutputAVRpu) - Abs(QDesireEndpu)) > FVarChangeTolerance)) or
                        (Abs(FPresentVpu - Fv_setpointLimited) > FVoltageChangeTolerance)) or
                        (ActiveCircuit.Solution.ControlIteration = 1)) then

                    begin
                        // Resets DER state variable only if it has not converged yet
                        FAVROperation := 0;

                        Set_PendingChange(CHANGEVARLEVEL, i);

                        ControlActionHandle := ActiveCircuit.ControlQueue.Push(TimeDelay, PendingChange[i], 0, Self);

                        if ShowEventLog then
                            AppendtoEventLog(Self.FullName + ', ' + DERElem.FullName,
                                Format('**Ready to change var output due to AVR trigger in AVR mode**, Vavgpu= %.5g, VPriorpu=%.5g, Vsetpoint=%.5g, VsetpointLimited=%.5g',
                                [FPresentVpu, FAvgpVpuPrior, Fv_setpoint, Fv_setpointLimited]));
                    end;
                end;
                VOLTVAR: // volt-var control mode
                begin
                    // Sets internal variables of PVSystem/Storage.
                    // FVVOperation is a flag which indicates if volt-var function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)

                    if DERElem.IsPVSystem() then
                    begin
                        PVSys.Set_Variable(5, FVreg);
                        PVSys.Set_Variable(7, FVVOperation);
                    end
                    else
                    begin
                        Storage.Set_Variable(14, FVreg);
                        Storage.Set_Variable(16, FVVOperation);
                    end;

                    // if inverter is off then exit
                    if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then
                        continue;

                    if Fvvc_curve = NIL then
                    begin
                        DoSimpleMsg(_('XY Curve object representing vvc1_curve does not exist or is not tied to InvControl.'), 382);
                        exit
                    end;

                    DERElem.VVmode := TRUE;

                    //Trigger from volt-var mode
                    if (((Abs(FPresentVpu - FAvgpVpuPrior) > FVoltageChangeTolerance) or
                        ((Abs(Abs(QoutputVVpu) - Abs(QDesireEndpu)) > FVarChangeTolerance))) or
                        (ActiveCircuit.Solution.ControlIteration = 1)) then

                    begin
                        // Resets DER state variable only if it has not converged yet
                        FVVOperation := 0;

                        Set_PendingChange(CHANGEVARLEVEL, i);

                        ControlActionHandle := ActiveCircuit.ControlQueue.Push(TimeDelay, PendingChange[i], 0, Self);

                        if ShowEventLog then
                            AppendtoEventLog(Self.FullName + ', ' + DERElem.FullName,
                                Format('**Ready to change var output due to volt-var trigger in volt-var mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                                [FPresentVpu, FAvgpVpuPrior]));
                    end;
                end;
                WATTPF: // watt-pf control mode
                begin
                    // Sets internal variables of PVSystem/Storage.
                    // FWPOperation is a flag which indicates if watt-pf function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)

                    if DERElem.IsPVSystem() then
                    begin
                        PVSys.Set_Variable(5, FVreg);
                        PVSys.Set_Variable(11, FWPOperation);
                    end
                    else
                    begin
                        Storage.Set_Variable(14, FVreg);
                        Storage.Set_Variable(16, FWPOperation);
                    end;

                    // if inverter is off then exit
                    if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then
                        continue;

                    if Fwattpf_curve = NIL then
                    begin
                        DoSimpleMsg(_('XY Curve object representing wattpf_curve does not exist or is not tied to InvControl.'), 382);
                        exit
                    end;

                    DERElem.WPmode := TRUE;

                    //Trigger from volt-var mode
                    if (((Abs(FPresentVpu - FAvgpVpuPrior) > FVoltageChangeTolerance) or
                        ((Abs(Abs(QoutputVVpu) - Abs(QDesireEndpu)) > FVarChangeTolerance))) or
                        (ActiveCircuit.Solution.ControlIteration = 1)) then

                    begin
                        // Resets DER state variable only if it has not converged yet
                        FWPOperation := 0;

                        Set_PendingChange(CHANGEVARLEVEL, i);

                        ControlActionHandle := ActiveCircuit.ControlQueue.Push(TimeDelay, PendingChange[i], 0, Self);

                        if ShowEventLog then
                            AppendtoEventLog(Self.FullName + ', ' + DERElem.FullName,
                                Format('**Ready to change var output due to watt-pf trigger in watt-pf mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                                [FPresentVpu, FAvgpVpuPrior]));
                    end;
                end;
                WATTVAR: // watt-var control mode
                begin
                    // Sets internal variables of PVSystem/Storage.
                    // FWVOperation is a flag which indicates if watt-var function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)

                    if DERElem.IsPVSystem() then
                    begin
                        PVSys.Set_Variable(5, FVreg);
                        PVSys.Set_Variable(12, FWVOperation);        //CHANGE HERE
                    end
                    else
                    begin
                        Storage.Set_Variable(14, FVreg);
                        Storage.Set_Variable(16, FWVOperation);
                    end;

                    // if inverter is off then exit
                    if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then
                        continue;

                    if Fwattvar_curve = NIL then
                    begin
                        DoSimpleMsg(_('XY Curve object representing wattvar_curve does not exist or is not tied to InvControl.'), 382);
                        exit
                    end;

                    DERElem.WVmode := TRUE;

                    //Trigger from volt-var mode
                    if (((Abs(FPresentVpu - FAvgpVpuPrior) > FVoltageChangeTolerance) or
                        ((Abs(Abs(QoutputVVpu) - Abs(QDesireEndpu)) > FVarChangeTolerance))) or
                        (ActiveCircuit.Solution.ControlIteration = 1)) then

                    begin
                        // Resets DER state variable only if it has not converged yet
                        FWVOperation := 0;

                        Set_PendingChange(CHANGEVARLEVEL, i);

                        ControlActionHandle := ActiveCircuit.ControlQueue.Push(TimeDelay, PendingChange[i], 0, Self);

                        if ShowEventLog then
                            AppendtoEventLog(Self.FullName + ', ' + DERElem.FullName,
                                Format('**Ready to change var output due to watt-var trigger in watt-var mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                                [FPresentVpu, FAvgpVpuPrior]));
                    end;
                end;
                DRC: // dynamic reactive current control mode
                begin
                    // Sets internal variables of PVSystem/Storage.
                    // FDRCOperation is a flag which indicates if DRC function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)

                    if DERElem.IsPVSystem() then
                    begin
                        PVSys.Set_Variable(5, FVreg);
                        PVSys.Set_Variable(6, FDRCRollAvgWindow.AvgVal / (basekV * 1000.0)); // save rolling average voltage in monitor
                        PVSys.Set_Variable(9, FDRCOperation);
                    end
                    else
                    begin
                        Storage.Set_Variable(14, FVreg);
                        Storage.Set_Variable(15, FDRCRollAvgWindow.AvgVal / (basekV * 1000.0)); // save rolling average voltage in monitor
                        Storage.Set_Variable(18, FDRCOperation);
                    end;

                    // if inverter is off then exit
                    if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then
                        continue;

                    //DRC triggers
                    if (priorDRCRollAvgWindow = 0.0) then
                    begin
                        if ((Abs(FPresentDRCVpu - FAvgpDRCVpuPrior) > FVoltageChangeTolerance)) then
                        begin
                            // Resets DER state variable only if it has not converged yet
                            FDRCOperation := 0;


                            Set_PendingChange(CHANGEVARLEVEL, i);

                            ControlActionHandle := ActiveCircuit.ControlQueue.Push(TimeDelay, PendingChange[i], 0, Self);

                            if ShowEventLog then
                                AppendtoEventLog(Self.FullName + ', ' + DERElem.FullName,
                                    Format('**Ready to change var output due to DRC trigger in DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                                    [FPresentDRCVpu, FAvgpDRCVpuPrior]));
                        end;
                    end;

                    DERElem.DRCmode := TRUE;

                    if ((Abs(FPresentDRCVpu - FAvgpDRCVpuPrior) > FVoltageChangeTolerance) or
                        (Abs(Abs(QoutputDRCpu) - Abs(QDesireEndpu)) > FVarChangeTolerance) or // TEMc; also tried checking against QDesireEndpu
                        (ActiveCircuit.Solution.ControlIteration = 1)) then
                    begin
                        Set_PendingChange(CHANGEVARLEVEL, i);
                        ControlActionHandle := ActiveCircuit.ControlQueue.Push(TimeDelay, PendingChange[i], 0, Self);

                        if ShowEventLog then
                            AppendtoEventLog(Self.FullName + ', ' + DERElem.FullName,
                                Format('**Ready to change var output due to DRC trigger in DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g, QoutPU=%.3g, QDesiredEndpu=%.3g',
                                [FPresentDRCVpu, FAvgpDRCVpuPrior, QoutputDRCpu, QDesireEndpu]));

                    end;
                end;
                GFM:
                begin
                    if DERElem.GFM_Mode then
                    begin
                        // Check if it's in GFM mode
                        if (not DERElem.IsStorage()) or (DERElem.IsStorage() and (TStorageObj(DERElem).StorageState = 1)) then // storage case
                        begin
                            if DERElem.dynVars.ILimit > 0 then
                                Valid := DERElem.CheckAmpsLimit() // Checks if reached the Amps limit
                            else
                                Valid := DERElem.CheckOLInverter(); // Checks if Inv OL
                        end
                        else
                            Valid := True; /// <<<---

                        Valid := Valid and not DERElem.dynVars.ResetIBR; // Check if we are not resetting
                        if Valid then
                        begin
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push(TimeDelay, PendingChange[i], 0, self);
                        end;
                    end;
                end;
            else
                // TODO: raise exception?
            end;
        end;
    end; // for i := 1 to FDERPointerList.Count do
end;

function TInvControlObj.MakeDERList: Boolean;
var
    PVSysClass: TDSSClass;
    StorageClass: TDSSClass;
    PVSys: TPVSystemObj = NIL;
    Storage: TStorageObj = NIL;
    DERElem: TPCElement;
    i, j: Integer;
begin
    Result := FALSE;
    PVSysClass := GetDSSClassPtr(DSS, 'PVSystem');
    StorageClass := GetDSSClassPtr(DSS, 'Storage');

    if FListSize > 0 then
    begin    // Name list is defined - Use it
        SetLength(ControlledElement, FListSize + 1);  // Use this as the main pointer to PVSystem and Storage Elements
        SetLength(CtrlVars, FListSize + 1);

        for i := 1 to FListSize do
        begin
            with CtrlVars[i] do
            begin
                SetLength(FVpuSolution, 3);
                SetLength(cBuffer, 7);

                if StripExtension(AnsiLowerCase(DERNameList.Strings[i - 1])) = 'pvsystem' then
                begin
                    PVSys := PVSysClass.Find(StripClassName(DERNameList.Strings[i - 1]));

                    if Assigned(PVSys) then
                    begin
                        if PVSys.Enabled then
                            FDERPointerList.Add(PVSys)
                    end
                    else
                    begin
                        DoSimpleMsg('Error: PVSystem Element "%s" not found.', [DERNameList.Strings[i - 1]], 14403);
                        Exit;
                    end;

                end
                else
                if StripExtension(AnsiLowerCase(DERNameList.Strings[i - 1])) = 'storage' then
                begin
                    Storage := StorageClass.Find(StripClassName(DERNameList.Strings[i - 1]));

                    if Assigned(Storage) then
                    begin
                        if Storage.Enabled then
                            FDERPointerList.Add(Storage)
                    end
                    else
                    begin
                        DoSimpleMsg('Error: Storage Element "%s" not found.', [DERNameList.Strings[i - 1]], 14403);
                        Exit;
                    end;
                end
            end;
        end;
    end
    else
    begin
        // Search through the entire circuit for enabled PVSystem and Storage objects and add them to the list

        // Adding PVSystem elements
        for i := 1 to PVSysClass.ElementCount() do
        begin
            PVSys := PVSysClass.ElementList.Get(i);
            if PVSys.Enabled then
                FDERPointerList.Add(PVSys);
            DERNameList.Add(PVSys.FullName);
        end;
        // Adding Storage elements
        for i := 1 to StorageClass.ElementCount() do
        begin
            Storage := StorageClass.ElementList.Get(i);
            if Storage.Enabled then
                FDERPointerList.Add(Storage);
            DERNameList.Add(Storage.FullName);
        end;

        FListSize := FDERPointerList.Count;

        SetLength(ControlledElement, FListSize + 1);
        SetLength(CtrlVars, FListSize + 1);
    end;

    // Initialize arrays
    for i := 1 to FlistSize do
    begin
        DERElem := FDERPointerList.Get(i);
        with CtrlVars[i] do
        begin
            // Sets the constants for the PI controller
            PICtrl := TPICtrl.Create();
            PICtrl.Kp := 1; // Uses deltaQ-factor as sample time for tunning the controller

            SetLength(FVpuSolution, 3);
            SetLength(cBuffer, 7);
            for j := 1 to 6 do
                cBuffer[j] := 0;

            Set_NTerms(DERElem.NTerms);

            CondOffset := 0;
            NPhasesDER := DERElem.NPhases;
            NCondsDER := DERElem.NConds;
            FAvgpVpuPrior := 0.0;
            FAvgpDRCVpuPrior := 0.0;
            FPresentVpu := 0.0;
            FPresentDRCVpu := 0.0;
            QDesiredVV := 0.0;
            QDesiredWP := 0.0;
            QDesiredWV := 0.0;
            QOld := -1.0;
            QOldVV := -1.0;
            if PVSys = nil then
                QOldAVR := 0.0
            else
                QOldAVR := -PVSys.kvarLimitNeg / 2.0;
            QOldDRC := -1.0;
            QOldVVDRC := -1.0;
            QDesiredDRC := 0.0;
            QDesiredVVDRC := 0.0;
            PLimitVW := 0.0;
            POldVWpu := 0.0;
            PBase := 0.0;
            QHeadroom := 0.0;
            QHeadroomNeg := 0.0;
            Qoutputpu := 0.0;
            QoutputVVpu := 0.0;
            QoutputAVRpu := 0.0;
            QoutputDRCpu := 0.0;
            QoutputVVDRCpu := 0.0;
            QDesireEndpu := 0.0;
            QDesireVVpu := 0.0;
            QDesireWPpu := 0.0;
            QDesireWVpu := 0.0;
            QDesireAVRpu := 0.0;
            QDesireLimitedpu := 0.0;
            QDesireOptionpu := 0.0;
            PLimitVWpu := 0.0;
            PLimitLimitedpu := 0.0;
            PLimitEndpu := 0.0;
            PLimitOptionpu := 0.0;
            QDesireDRCpu := 0.0;
            FRollAvgWindow := TRollAvgWindow.Create();
            FDRCRollAvgWindow := TRollAvgWindow.Create();

            FdeltaQFactor := DELTAQDEFAULT;
            FdeltaPFactor := DELTAPDEFAULT;
            DeltaV_old := -1.0;

            deltaVDynReac := 0.0;
            FlagChangeCurve := FALSE;
            FActiveVVCurve := 1;
            priorRollAvgWindow := 0.0;
            priorDRCRollAvgWindow := 0.0;
            FPriorWattspu := 0.0;
            FPriorWatts := 0.0;
            FPriorPLimitOptionpu := 0.0;
            FPriorQDesireOptionpu := 0.0;
            kW_out_desiredpu := 0.0;
            kW_out_desired := 0.0;
            FPriorvarspu := 0.0;
            FPriorvars := 0.0;

            FFlagVWOperates := FALSE;

            FVVOperation := 0.0;
            FVWOperation := 0.0;
            FDRCOperation := 0.0;
            FVVDRCOperation := 0.0;
            FWPOperation := 0.0;
            FWVOperation := 0.0;
            FAVROperation := 0.0;

            for j := 1 to 2 do
                FVpuSolution[j] := 0.0;

            FPendingChange := NONE;

            FVbase := 0.0;
            FVarFollowInverter := FALSE;
            FInverterON := TRUE;
            FpresentkW := 0.0;
            FkVARating := 0.0;
            Fpresentkvar := 0.0;
            FkvarLimit := 0.0;
            FkvarLimitNeg := 0.0;
            FCurrentkvarLimit := 0.0;
            FCurrentkvarLimitNeg := 0.0;
            FDCkWRated := 0.0;
            FpctDCkWRated := 0.0;
            FEffFactor := 0.0;
            FDCkW := 0.0;
            FPPriority := FALSE;
            DQDV := 0.0;
            Fv_setpointLimited := 0.0;
            FAvgpAVRVpuPrior := 0.0;
        end;
    end;

    // RecalcElementData(); -- MakeDERList is only called FROM RecalcElementData, no need to call it again.
    if FDERPointerList.Count > 0 then
        Result := TRUE;
end;

procedure TInvControlObj.Reset;
begin
    // inherited;
end;

procedure TInvControlObj.Set_PendingChange(Value: Integer; DevIndex: Integer);
begin
    CtrlVars[DevIndex].FPendingChange := Value;
    DblTraceParameter := Value;
end;

procedure TInvControlObj.UpdateInvControl(i: Integer);
var
    j, k: Integer;
    solnvoltage: Double;
    tempVbuffer: pComplexArray;
    BasekV: Double;
    DERElem: TInvBasedPCE;
begin
    tempVbuffer := NIL;   // Initialize for Reallocmem

    for j := 1 to FDERPointerList.Count do
    begin
          // only update solution idx one time through this routine
        if (j = 1) and (i = 1) then
        begin
              //update solution voltage in per-unit for hysteresis
            if FVpuSolutionIdx = 2 then
                FVpuSolutionIdx := 1
            else
                FVpuSolutionIdx := FVpuSolutionIdx + 1;
        end;

        DERElem := ControlledElement[j];
        with CtrlVars[j] do
        begin
            BasekV := CtrlVars[i].FVBase / 1000.0; //TODO: check (i, j)

            FPriorPLimitOptionpu := PLimitOptionpu;
            FPriorQDesireOptionpu := QDesireOptionpu;

            // Used to update the VW resquested kW
            DERElem.VWmode := FALSE;
            DERElem.VVMode := FALSE;
            DERElem.DRCMode := FALSE;

            FFlagVWOperates := FALSE;

            // Reset DQDV - We might not need it
            DQDV := 0.0;

            // Reset the operation flags for the new time step
            FVVOperation := 0;
            FVWOperation := 0;
            FDRCOperation := 0;
            FVVDRCOperation := 0;
            FWPOperation := 0;
            FWVOperation := 0;
            FAVROperation := 0;

            // Reinitialize convergence arrays.
            //FdeltaQFactor := DELTAQDEFAULT;
            FdeltaPFactor := DELTAPDEFAULT;

            // allocated enough memory to buffer to hold voltages and initialize to 0
            Reallocmem(tempVbuffer, Sizeof(Complex) * DERElem.NConds);
            for k := 1 to DERElem.NConds do
                tempVbuffer[k] := 0;

            priorRollAvgWindow := FRollAvgWindow.AvgVal;
            priorDRCRollAvgWindow := FDRCRollAvgWindow.AvgVal;

            // compute the present terminal voltage
            DERElem.ComputeVterminal();
            //PVSys.Set_Variable(5,FDRCRollAvgWindow.AvgVal); // save rolling average voltage in monitor

            solnvoltage := 0.0;

            GetMonVoltage(solnvoltage, j, BasekV);

            // add present power flow solution voltage to the rolling average window
            FRollAvgWindow.Add(solnvoltage, ActiveCircuit.Solution.DynaVars.h, FRollAvgWindowLength);
            FDRCRollAvgWindow.Add(solnvoltage, ActiveCircuit.Solution.DynaVars.h, FDRCRollAvgWindowLength);

            FVpuSolution[FVpuSolutionIdx] := solnvoltage / ((ActiveCircuit.Buses[DERElem.terminals[0].busRef].kVBase) * 1000.0);

            Reallocmem(tempVbuffer, 0);   // Clean up memory
        end;
    end;
end;

function TInvControlObj.Get_PendingChange(DevIndex: Integer): Integer;
begin
    Result := CtrlVars[DevIndex].FPendingChange;
end;

procedure TInvControlObj.CalcVoltWatt_watts(j: Integer);
var
    DeltaPpu: Double;
  // PLimitEndpu <= abs(kW_out_desiredpu will always be true when we are in 'resquest' region of VW
  // That's what we want. In this region, VW will work similarly to VV. So we need to move slowly towards the VW curve point.
begin
    with CtrlVars[j] do
    begin
        if ((PLimitEndpu < 1.0) and (PLimitEndpu <= abs(kW_out_desiredpu))) or (FFlagVWOperates) then
        begin
            if (ActiveCircuit.Solution.ControlIteration = 1) then
                POldVWpu := abs(kW_out_desiredpu); // take abs(kW_out_desiredpu) because might be in charging mode.
            FFlagVWOperates := TRUE;

            // PLimitEndpu might be negative here in 'requesting' region. Do we need to give POldVW a sign in this case?
            // Yes, it will naturally evolve to a negative value with the process. It will always positive only in the 1st control iteration.
            DeltaPpu := PLimitEndpu - POldVWpu;

            if FdeltaP_factor = FLAGDELTAP then
                Change_deltaP_factor(j)
            else
                FdeltaPFactor := FdeltaP_factor;

            PLimitVW := (POldVWpu + DeltaPpu * FdeltaPFactor) * PBase;
        end
        else
        begin
            PLimitVW := PLimitEndpu * PBase;
        end;
    end;
end;

procedure TInvControlObj.Check_Plimits(j: Integer; P: Double);
var
    P_Ppriority: Double;
    pctDCkWRatedlimit: Double;
begin
    with CtrlVars[j] do
    begin
        PLimitLimitedpu := 1.0; // Not limited

        // volt-watt states
        if P < 1.0 then
            FVWOperation := 1.0;

        pctDCkWRatedlimit := FpctDCkWRated * FDCkWRated;

        // PLimitEndpu should be less than the P avaliable under var priority   (works for VV_VW)
        if FPPriority = FALSE then
        begin
            P_Ppriority := Sqrt(SQR(FkVARating) - SQR(Fpresentkvar));
            if P_Ppriority < (abs(P) * PBase) then   // P might be negative in requesting region for storage
            begin
                PLimitLimitedpu := P_Ppriority / PBase * sign(P);
                FVWOperation := 0.0; // kVA exceeded under watt priority
            end;
        end;

        // PLimitEndpu should be less than pctPmpp
        if (abs(P) * PBase) > pctDCkWRatedlimit then
        begin
            FVWOperation := 0.0; // pctPmpp exceeded under watt priority
            PLimitLimitedpu := pctDCkWRatedlimit / PBase * sign(P);
        end;
    end;
end;

procedure TInvControlObj.CalcVoltVar_vars(j: Integer);
var
    DeltaQ: Double;
begin
    with CtrlVars[j] do
    begin
        if (FlagChangeCurve = False) then
        begin
            if QDesireEndpu >= 0.0 then
                DeltaQ := QDesireEndpu * QHeadRoom
            else
                DeltaQ := QDesireEndpu * QHeadRoomNeg;

            if CtrlModel = TInvControlModel.Linear then
            begin
                DeltaQ := DeltaQ - QOldVV;
                if FdeltaQ_factor = FLAGDELTAQ then
                    Change_deltaQ_factor(j)
{$IFDEF DSS_CAPI_NOCOMPATFLAGS}
                    ;
{$ELSE}
                else
                if (DSS_EXTENSIONS_COMPAT and ord(TDSSCompatFlags.InvControl9611)) = 0 then
                    FdeltaQFactor := FdeltaQ_factor;
{$ENDIF}
                QDesiredVV := QOldVV + DeltaQ * FdeltaQFactor;
            end
            else
            begin
                // recalculates the constants in case they've changed on the go
                PICtrl.kDen := exp(-1 * abs(FdeltaQ_factor));
                PICtrl.kNum := 1 - PICtrl.kDen;
                QDesiredVV := PICtrl.SolvePI(DeltaQ);
            end;
        end
        // else, stay at present var output level
        else
        begin
            QDesiredVV := Fpresentkvar;
        end;
    end;
end;

procedure TInvControlObj.CalcAVR_vars(j: Integer);
var
    DeltaQ: Double;
begin
    with CtrlVars[j] do
    begin
        if QDesireEndpu >= 0.0 then
            DeltaQ := QDesireEndpu * QHeadRoom
        else
            DeltaQ := QDesireEndpu * QHeadRoomNeg;

        if CtrlModel = TInvControlModel.Linear then
        begin
            DeltaQ -= QOldAVR;
            if FdeltaQ_factor = FLAGDELTAQ then
                Change_deltaQ_factor(j)
{$IFDEF DSS_CAPI_NOCOMPATFLAGS}
                ;
{$ELSE}
            else
            if (DSS_EXTENSIONS_COMPAT and ord(TDSSCompatFlags.InvControl9611)) = 0 then
                FdeltaQFactor := FdeltaQ_factor;
{$ENDIF}

            QDesiredAVR := QOldAVR + 0.2 * DeltaQ;
            // QDesiredAVR := QDesireEndpu * QHeadRoomNeg
        end
        else
        begin
            // recalculates the constants in case they've changed on the go
            PICtrl.kDen := exp(-1 * abs(FdeltaQ_factor));
            PICtrl.kNum := 1 - PICtrl.kDen;
            QDesiredAVR := PICtrl.SolvePI(DeltaQ);
        end;
    end;
end;

procedure TInvControlObj.CalcWATTPF_vars(j: Integer);
begin
    with CtrlVars[j] do
    begin
        if QDesireEndpu >= 0.0 then
            QDesiredWP := QDesireEndpu * QHeadRoom
        else
            QDesiredWP := QDesireEndpu * QHeadRoomNeg;
    end;
end;

procedure TInvControlObj.CalcWATTVAR_vars(j: Integer);
begin
    with CtrlVars[j] do
    begin
        if QDesireEndpu >= 0.0 then
            QDesiredWV := QDesireEndpu * QHeadRoom
        else
            QDesiredWV := QDesireEndpu * QHeadRoomNeg;
    end;
end;

procedure TInvControlObj.CalcDRC_vars(j: Integer);
var
    DeltaQ: Double;
begin
    with CtrlVars[j] do
    begin
        if QDesireEndpu >= 0.0 then
            DeltaQ := QDesireEndpu * QHeadRoom
        else
            DeltaQ := QDesireEndpu * QHeadRoomNeg;

        if CtrlModel = TInvControlModel.Linear then
        begin
            DeltaQ := DeltaQ - QOldDRC;
            if FdeltaQ_factor = FLAGDELTAQ then
                Change_deltaQ_factor(j)
{$IFDEF DSS_CAPI_NOCOMPATFLAGS}
                ;
{$ELSE}
            else
            if (DSS_EXTENSIONS_COMPAT and ord(TDSSCompatFlags.InvControl9611)) = 0 then
                FdeltaQFactor := FdeltaQ_factor;
{$ENDIF}

            QDesiredDRC := QOldDRC + DeltaQ * FdeltaQFactor;
        end
        else
        begin
            // recalculates the constants in case they've changed on the go
            PICtrl.kDen := exp(-1 * abs(FdeltaQ_factor));
            PICtrl.kNum := 1 - PICtrl.kDen;
            QDesiredDRC := PICtrl.SolvePI(DeltaQ);
        end;
    end;
end;

procedure TInvControlObj.CalcVVDRC_vars(j: Integer);
var
    DeltaQ: Double;
begin
    with CtrlVars[j] do
    begin
        if QDesireEndpu >= 0.0 then
            DeltaQ := QDesireEndpu * QHeadRoom
        else
            DeltaQ := QDesireEndpu * QHeadRoomNeg;

        if CtrlModel = TInvControlModel.Linear then
        begin
            DeltaQ -= QOldVVDRC;
            if FdeltaQ_factor = FLAGDELTAQ then
                Change_deltaQ_factor(j)
{$IFDEF DSS_CAPI_NOCOMPATFLAGS}
                ;
{$ELSE}
            else
            if (DSS_EXTENSIONS_COMPAT and ord(TDSSCompatFlags.InvControl9611)) = 0 then
                FdeltaQFactor := FdeltaQ_factor;
{$ENDIF}

            QDesiredVVDRC := QOldVVDRC + DeltaQ * FdeltaQFactor;
        end
        else
        begin
            // recalculates the constants in case they've changed on the go
            PICtrl.kDen := exp(-1 * abs(FdeltaQ_factor));
            PICtrl.kNum := 1 - PICtrl.kDen;
            QDesiredVVDRC := PICtrl.SolvePI(DeltaQ);
        End;
    end;
end;

procedure TInvControlObj.Calc_PBase(j: Integer);
var
    DERElem: TInvBasedPCE;
begin
    DERElem := ControlledElement[j];
    with CtrlVars[j] do
    begin
        if DERElem.IsPVSystem() then
        begin
            if (FVoltwattYaxis = 0) then
                PBase := FDCkW * FEffFactor

            else
            if (FVoltwattYaxis = 1) then
                PBase := FDCkWRated

            else
            if (FVoltwattYaxis = 2) then
                PBase := FDCkWRated * FpctDCkWRated

            else
            if (FVoltwattYaxis = 3) then
                PBase := FkVARating;
        end
        else
        begin
            if (FVoltwattYaxis = 0) then
                PBase := TStorageObj(DERElem).DCkW * FEffFactor

            else
            if (FVoltwattYaxis = 1) then
                PBase := FDCkWRated

            else
            if (FVoltwattYaxis = 2) then
                PBase := FDCkWRated * FpctDCkWRated

            else
            if (FVoltwattYaxis = 3) then
                PBase := FkVARating;
        end;
    end;
end;

procedure TInvControlObj.CalcLPF(m: Integer; powertype: Ansistring; LPF_desiredpu: Double);
var
    alpha: Double;

    // Applies the LPF:
    //  Return value is in kvar for VARS
    //  Return value is in puPmpp for WATTS

begin
    with CtrlVars[m] do
    begin
        // Qoutput(t) = Qdesired(t) x {1- exp[-(t-t0)/tau]} + Qoutput(t-t0) x exp[-(t-t0)/tau]
        // calculate the alpha constant: alpha = exp[-(t-t0)/tau]
        alpha := exp(-1.0 * ActiveCircuit.Solution.DynaVars.h / LPFTau);

        if powertype = 'VARS' then
            QDesireOptionpu := LPF_desiredpu * (1 - alpha) + FPriorQDesireOptionpu * alpha;

        if powertype = 'WATTS' then
            PLimitOptionpu := LPF_desiredpu * (1 - alpha) + FPriorPLimitOptionpu * alpha
    end;
end;

procedure TInvControlObj.CalcRF(m: Integer; powertype: Ansistring; RF_desiredpu: Double);

begin
    // Applies the Rise/Fall limiting function:
    with CtrlVars[m] do
    begin
        if powertype = 'VARS' then
        begin
            // rate of change rise/fall limit
            if (RF_desiredpu - FPriorQDesireOptionpu) > (FRiseFallLimit * ActiveCircuit.Solution.DynaVars.h) then
                QDesireOptionpu := FPriorQDesireOptionpu + FRiseFallLimit * ActiveCircuit.Solution.DynaVars.h
            else
            if (RF_desiredpu - FPriorQDesireOptionpu) < (-1 * FRiseFallLimit * ActiveCircuit.Solution.DynaVars.h) then
                QDesireOptionpu := FPriorQDesireOptionpu - FRiseFallLimit * ActiveCircuit.Solution.DynaVars.h
            else
                QDesireOptionpu := RF_desiredpu;
        end;

        if powertype = 'WATTS' then
        begin
            // rate of change rise/fall limit
            if (RF_desiredpu - FPriorPLimitOptionpu) > (FRiseFallLimit * ActiveCircuit.Solution.DynaVars.h) then
                PLimitOptionpu := FPriorPLimitOptionpu + (FRiseFallLimit * ActiveCircuit.Solution.DynaVars.h)
            else
            if (RF_desiredpu - FPriorPLimitOptionpu) < (-1 * FRiseFallLimit * ActiveCircuit.Solution.DynaVars.h) then
                PLimitOptionpu := FPriorPLimitOptionpu - (FRiseFallLimit * ActiveCircuit.Solution.DynaVars.h)
            else
                PLimitOptionpu := RF_desiredpu;
        end;
    end;
end;


procedure TInvControlObj.CalcPVWcurve_limitpu(j: Integer);
begin
    with CtrlVars[j] do
    begin
        if ControlledElement[j].IsPVSystem() then
            PLimitVWpu := Fvoltwatt_curve.GetYValue(FPresentVpu)
        else
        begin
            if TStorageObj(ControlledElement[j]).StorageState = STORE_DISCHARGING then
            begin
                if TStorageObj(ControlledElement[j]).FVWStateRequested then
                    PLimitVWpu := FvoltwattCH_curve.GetYValue(FPresentVpu)
                else
                    PLimitVWpu := Fvoltwatt_curve.GetYValue(FPresentVpu);

            end
            else
            if (TStorageObj(ControlledElement[j]).StorageState = STORE_CHARGING) and (FvoltwattCH_curve <> NIL) then
            begin
                if TStorageObj(ControlledElement[j]).FVWStateRequested then
                    PLimitVWpu := Fvoltwatt_curve.GetYValue(FPresentVpu)
                else
                    PLimitVWpu := FvoltwattCH_curve.GetYValue(FPresentVpu) // try with positive PlimitVWpu
            end

            else
                PLimitVWpu := 1.0; // don't limit if in idling state
        end;
    end;
end;


procedure TInvControlObj.CalcQVVcurve_desiredpu(j: Integer);
var
    voltagechangesolution: Double;
    QPresentpu: Double;
    VpuFromCurve: Double;
begin
    with CtrlVars[j] do
    begin
        QDesireVVpu := 0.0;

        if Fpresentkvar >= 0.0 then
            QPresentpu := Fpresentkvar / QHeadRoom
        else
            QPresentpu := Fpresentkvar / QHeadRoomNeg;

        voltagechangesolution := 0.0;

        // for first two seconds, keep voltagechangesolution equal to zero
        // we don't have solutions from the time-series power flow, yet
        if ((ActiveCircuit.Solution.DynaVars.dblHour * 3600.0 / ActiveCircuit.Solution.DynaVars.h) < 3.0) then
            voltagechangesolution := 0.0
        else
        if (FVpuSolutionIdx = 1) then
            voltagechangesolution := FVpuSolution[1] - FVpuSolution[2]
        else
        if (FVpuSolutionIdx = 2) then
            voltagechangesolution := FVpuSolution[2] - FVpuSolution[1];

        // if no hysteresis (Fvvc_curveOffset == 0), then just look up the value
        // from the volt-var curve
        if Fvvc_curveOffset = 0.0 then
        begin  // no hysteresis
            QDesireVVpu := Fvvc_curve.GetYValue(FPresentVpu)
        end // end of logic for the no-hysteresis case

        // else if we're going in the positive direction and on curve 1, stay
        // with curve 1
        else
        if (voltagechangesolution > 0) and (FActiveVVCurve = 1) then
        begin
            if (FlagChangeCurve = TRUE) then
            begin
                VpuFromCurve := Fvvc_curve.GetXValue(QPresentpu);
                if (Abs(FPresentVpu - VpuFromCurve) < FVoltageChangeTolerance / 2.0) then
                begin
                    QDesireVVpu := Fvvc_curve.GetYValue(FPresentVpu);      //Y value = in per-unit of headroom
                    FlagChangeCurve := FALSE;
                end
                else
                begin
                    QDesireVVpu := QPresentpu;            // (PR) look at here
                    FlagChangeCurve := FALSE;
                end;
            end
            else
            begin
                QDesireVVpu := Fvvc_curve.GetYValue(FPresentVpu);      //Y value = in per-unit of headroom
            end;
        end

        // with hysteresis if we're going in the positive direction on voltages
        // from last two power flow solutions, and we're using curve 2, keep vars
        // the same, and change to curve1 active
        else
        if (voltagechangesolution > 0) and (FActiveVVCurve = 2) then
        begin
            QDesireVVpu := QPresentpu;
            FActiveVVCurve := 1;
            FlagChangeCurve := TRUE;
        end

        // with hysteresis if we're going in the negative direction on voltages
        // from last two power flow solutions, and we're using curve 2, either
        // lookup the vars for the voltage we're at (with offset on curve1),
        // or if we've not just changed curves, stay at the current p.u.
        // var output
        else
        if (voltagechangesolution < 0) and (FActiveVVCurve = 2) then
        begin
            if (FlagChangeCurve = TRUE) then
            begin
                VpuFromCurve := Fvvc_curve.GetXValue(QPresentpu);
                VpuFromCurve := VpuFromCurve - Fvvc_curveOffset;
                if (Abs(FPresentVpu - VpuFromCurve) < FVoltageChangeTolerance / 2.0) then
                begin
                    QDesireVVpu := Fvvc_curve.GetYValue(FPresentVpu - Fvvc_curveOffset);      //Y value = in per-unit of headroom
                    FlagChangeCurve := FALSE;
                end
                else
                begin
                    QDesireVVpu := QPresentpu;
                    FlagChangeCurve := FALSE;
                end;
            end
            else
            begin
                QDesireVVpu := Fvvc_curve.GetYValue(FPresentVpu - Fvvc_curveOffset);      //Y value = in per-unit of headroom
            end;
        end

        // with hysteresis if we're going in the negative direction on voltages
        // from last two power flow solutions, and we're using curve 1, then
        // stay wjth present output vars and make curve2 active, set curve change
        // flag
        else
        if (voltagechangesolution < 0) and (FActiveVVCurve = 1) then
        begin
            QDesireVVpu := QPresentpu;
            FActiveVVCurve := 2;
            FlagChangeCurve := TRUE;
        end


        // if no change in voltage from one powerflow to the next, then
        // do one of the following
        else
        if (voltagechangesolution = 0) and (FActiveVVCurve = 1) and (FlagChangeCurve = FALSE) then
        begin
            QDesireVVpu := Fvvc_curve.GetYValue(FPresentVpu);
        end
        else
        if (voltagechangesolution = 0) and (FlagChangeCurve = TRUE) then
        begin
            QDesireVVpu := QPresentpu;
        end

        else
        if (voltagechangesolution = 0) and (FActiveVVCurve = 2) and (FlagChangeCurve = FALSE) then
        begin
            QDesireVVpu := Fvvc_curve.GetYValue(FPresentVpu - Fvvc_curveOffset);
        end;
    end;
end;

procedure TInvControlObj.CalcQWVcurve_desiredpu(j: Integer);
var
    Pbase: Double;
begin
    with CtrlVars[j] do
    begin
        QDesireWVpu := 0.0;

        // for first two seconds, keep voltagechangesolution equal to zero
        // we don't have solutions from the time-series power flow, yet
        // if ((ActiveCircuit.Solution.DynaVars.dblHour*3600.0 / ActiveCircuit.Solution.DynaVars.h)<3.0) then voltagechangesolution := 0.0
        // else if(FVpuSolutionIdx = 1) then voltagechangesolution := FVpuSolution[j,1] - FVpuSolution[j,2]
        // else if(FVpuSolutionIdx = 2) then voltagechangesolution := FVpuSolution[j,2] - FVpuSolution[j,1];

        Pbase := Min(FkVARating, FDCkWRated); // Should include DC-to-AC and kW-to-KVA ratios to avoid to quick fix like this

        QDesireWVpu := Fwattvar_curve.GetYValue(FDCkW * FEffFactor * FpctDCkWRated / Pbase);
    end;
end;

procedure TInvControlObj.CalcQAVR_desiredpu(j: Integer);
var
    DQ: Double;
    QPresentpu: Double;
    DQmax: Double;
    DeltaV: Double;
    v: Double;
begin
    with CtrlVars[j] do
    begin
        DQmax := 0.1 * Fkvarlimit / QHeadRoomNeg;

        QDesireAVRpu := 0.0;

        if Fpresentkvar >= 0.0 then
            QPresentpu := Fpresentkvar / QHeadRoom
        else
            QPresentpu := Fpresentkvar / QHeadRoomNeg;

        if (ActiveCircuit.Solution.ControlIteration = 3) then
        begin
            v := FAvgpAVRVpuPrior;
            QPresentpu := 0.0;
            QOldAVR := 0.0;
        end
        else
            v := FPresentVpu;

        // for first two seconds, keep voltagechangesolution equal to zero
        // we don't have solutions from the time-series power flow, yet
        // if ((ActiveCircuit.Solution.DynaVars.dblHour*3600.0 / ActiveCircuit.Solution.DynaVars.h)<3.0) then voltagechangesolution := 0.0
        // else if(FVpuSolutionIdx = 1) then voltagechangesolution := FVpuSolution[j,1] - FVpuSolution[j,2]
        // else if(FVpuSolutionIdx = 2) then voltagechangesolution := FVpuSolution[j,2] - FVpuSolution[j,1];

        DeltaV := Abs(Fv_setpoint - FAvgpVpuPrior);

        if (abs(DeltaV) < 0.005) and (FdeltaQFactor > 0.2) then
            FdeltaQFactor := FdeltaQFactor + 0.1
        else
        if (abs(DeltaV) < 0.02) and (FdeltaQFactor > 0.2) then
            FdeltaQFactor := FdeltaQFactor + 0.05
        else
        if (abs(DeltaV) > 0.02) and (FdeltaQFactor < 0.9) then
            FdeltaQFactor := FdeltaQFactor - 0.05
        else
        if (abs(DeltaV) < 0.05) and (FdeltaQFactor < 0.9) then
            FdeltaQFactor := FdeltaQFactor - 0.1;

        FdeltaQFactor := 0.2;

        DeltaV_old := Abs(FPresentVpu - FAvgpVpuPrior);

        if (FPresentVpu - FAvgpVpuPrior = 0) then
            DQ := 0
        else
            DQ := FdeltaQFactor * DQDV * (Fv_setpoint - v);
        if (Abs(DQ) > DQmax) then
            if (DQ < 0.0) then
                DQ := -DQmax
            else
                DQ := DQmax;

        QDesireAVRpu := QPresentpu + DQ;
    end;
end;

procedure TInvControlObj.CalcQWPcurve_desiredpu(j: Integer);
var
    p: Double;
    pf_priority: Boolean = False;
    QDesiredWP: Double;
    DERElem: TInvBasedPCE;
begin
    DERElem := ControlledElement[j];
    with CtrlVars[j] do
    begin
        QDesireWPpu := 0.0;

        PF_Priority := False;

        // for first two seconds, keep voltagechangesolution equal to zero
        // we don't have solutions from the time-series power flow, yet
        // if ((ActiveCircuit.Solution.DynaVars.dblHour*3600.0 / ActiveCircuit.Solution.DynaVars.h)<3.0) then voltagechangesolution := 0.0
        // else if(FVpuSolutionIdx = 1) then voltagechangesolution := FVpuSolution[j,1] - FVpuSolution[j,2]
        // else if(FVpuSolutionIdx = 2) then voltagechangesolution := FVpuSolution[j,2] - FVpuSolution[j,1];

        pf_wp_nominal := Fwattpf_curve.GetYValue(FDCkW * FEffFactor * FpctDCkWRated / FDCkWRated);
        pf_priority := DERElem.GetPFPriority();
        if (FPPriority = FALSE) and (pf_priority = FALSE) then
            p := FDCkW * FEffFactor * FpctDCkWRated
        else
            p := kW_out_desired;

        QDesiredWP := p * sqrt(1 / (pf_wp_nominal * pf_wp_nominal) - 1) * sign(pf_wp_nominal);
        if QDesiredWP >= 0.0 then
            QDesireWPpu := QDesiredWP / QHeadRoom
        else
            QDesireWPpu := QDesiredWP / QHeadRoomNeg;
    end;
end;

procedure TInvControlObj.CalcQDRC_desiredpu(j: Integer);
var
    basekV: Double;
begin
    with CtrlVars[j] do
    begin
        QDesireDRCpu := 0.0;

        basekV := FVBase / 1000.0; // It's a line-to-ground voltage

        // calculate deltaV quantity in per-unit from subtracting the rolling average
        // value (in p.u.) from the present p.u. terminal voltage (average of line-ground)
        // if more than one phase
        if (FDRCRollAvgWindow.AvgVal / (basekV * 1000.0)) = 0.0 then
            deltaVDynReac := 0
        else
            deltaVDynReac := FPresentDRCVpu - FDRCRollAvgWindow.AvgVal / (basekV * 1000.0);

        // if below the lower deadband and deltaV quantity is non-zero then
        // calculate desired pu var output. In per-unit of kva rating (also
        // ampere rating), per report specifications.
        if (deltaVDynReac <> 0) and (FPresentDRCVpu < FDbVMin) then
            QDesireDRCpu := -deltaVDynReac * FArGraLowV

        // if above the upper deadband and deltaV quantity is non-zero then
        // calculate desired pu var output. In per-unit of kva rating (also
        // ampere rating), per report specifications.

        else
        if (deltaVDynReac <> 0) and (FPresentDRCVpu > FDbVMax) then
            QDesireDRCpu := -deltaVDynReac * FArGraHiV

        else
        if deltaVDynReac = 0.0 then
            QDesireDRCpu := 0.0;

        if (ActiveCircuit.Solution.Dynavars.t = 1) then
            QDesireDRCpu := 0.0;
    end;
end;


procedure TInvControlObj.Check_Qlimits_WV(j: Integer; Q: Double);
var
    // Q_Ppriority                              :Double;
    currentkvarlimitpu: Double;
    currentkvarlimitnegpu: Double;
    FOperation: Double;
    error: Double;
begin
    with CtrlVars[j] do
    begin
        // Will organize this part into functions later

        // states
        error := 0;
        if (ControlMode = WATTVAR) then
            error := 0.005;

        if Q < -error then
            FOperation := -1.0
        else
        if Q > error then
            FOperation := 1.0
        else
            FOperation := 0.0;


        QDesireLimitedpu := 1.0; // Not limited

        currentkvarlimitpu := FCurrentkvarLimit / QHeadRoom;
        currentkvarlimitnegpu := FCurrentkvarLimitNeg / QHeadRoomNeg;

        if currentkvarlimitpu > QDesireLimitedpu then
            currentkvarlimitpu := QDesireLimitedpu;
        if currentkvarlimitnegpu > QDesireLimitedpu then
            currentkvarlimitnegpu := QDesireLimitedpu;

        // Q curve desiredpu should be less than currentkvarlimit(neg)
        if (Q > 0.0) and (abs(Q) >= abs(currentkvarlimitpu)) then
        begin
            FOperation := 0.2 * sign(Q); // When kvarlimit is exceeded
            QDesireLimitedpu := currentkvarlimitpu * sign(Q);
        end
        else
        if (Q < 0.0) and (abs(Q) >= abs(currentkvarlimitnegpu)) then
        begin
            FOperation := 0.2 * sign(Q); // When kvarlimitneg is exceeded
            QDesireLimitedpu := currentkvarlimitnegpu * sign(Q);
        end;

        // States Flags
        if (ControlMode = WATTVAR) then
            FWVOperation := FOperation;
    end;
end;

procedure TInvControlObj.Calc_PQ_WV(j: Integer);
var
    coeff: TCoeff;
    var_limit_operation_value,
    Pbase,
    Qbase,
    A,
    B,
    C,
    a_line,
    b_line: Double;
begin
    with CtrlVars[j] do
    begin
        Pbase := Min(FkVARating, FDCkWRated);

        if QDesiredWV >= 0.0 then
        begin
            Qbase := QHeadroom;
        // Qbasesign := 1.0;
        end
        else
        begin
            Qbase := QHeadroomNeg;
        // Qbasesign := -1.0;
        end;

        var_limit_operation_value := 0.2;
        if (abs(FWVOperation) = var_limit_operation_value) then
            PLimitEndpu := Fwattvar_curve.GetXValue(QDesireEndpu)
        else
            PLimitEndpu := 1.0;

        CalcWATTVAR_vars(j);

    // Qdesiredpu should be less than the Q avaliable under watt priority  (works just for varmax)
        if (Sqrt(Sqr(FDCkW * FEffFactor * FpctDCkWRated * PLimitEndpu) + Sqr(QDesiredWV)) > FkVARating) then
        begin
            coeff := Fwattvar_curve.GetCoefficients(FDCkW * FEffFactor * FpctDCkWRated / Pbase);

            a_line := coeff[1] * Qbase / Pbase;
            b_line := coeff[2] * Qbase;

            A := 1 + Sqr(a_line);
            B := 2 * a_line * b_line;
            C := Sqr(b_line) - Sqr(FkVARating);


            PLimitEndpu := (-B + Sqrt(sqr(B) - 4 * A * C)) / (2 * A * Pbase);
            QDesireEndpu := Fwattvar_curve.GetYValue(PLimitEndpu);
        end;

        CalcWATTVAR_vars(j);
    end;
end;

procedure TInvControlObj.Check_Qlimits(j: Integer; Q: Double);
var
    Q_Ppriority: Double;
    currentkvarlimitpu: Double;
    currentkvarlimitnegpu: Double;
    FOperation: Double;
    error: Double;

begin
    with CtrlVars[j] do
    begin
        // states
        error := 0;
        if (ControlMode = VOLTVAR) then
            error := 0.005;
        if (ControlMode = WATTPF) then
            error := 0.005;
        if (ControlMode = WATTVAR) then
            error := 0.005;
        if (ControlMode = DRC) then
            error := 0.0005;
        if (ControlMode = AVR) then
            error := 0.005;
        if (CombiMode = VV_DRC) then
            error := 0.005;
        if (CombiMode = VV_VW) then
            error := 0.005;

        if Q < -error then
            FOperation := -1.0
        else
        if Q > error then
            FOperation := 1.0
        else
            FOperation := 0.0;


        QDesireLimitedpu := 1.0; // Not limited

        currentkvarlimitpu := FCurrentkvarLimit / QHeadRoom;
        currentkvarlimitnegpu := FCurrentkvarLimitNeg / QHeadRoomNeg;

        if currentkvarlimitpu > QDesireLimitedpu then
            currentkvarlimitpu := QDesireLimitedpu;
        if currentkvarlimitnegpu > QDesireLimitedpu then
            currentkvarlimitnegpu := QDesireLimitedpu;

        // Q curve desiredpu should be less than currentkvarlimit(neg)
        if (Q > 0.0) and (abs(Q) >= abs(currentkvarlimitpu)) then
        begin
            FOperation := 0.2 * sign(Q); // When kvarlimit is exceeded
            QDesireLimitedpu := currentkvarlimitpu * sign(Q);
        end
        else
        if (Q < 0.0) and (abs(Q) >= abs(currentkvarlimitnegpu)) then
        begin
            FOperation := 0.2 * sign(Q); // When kvarlimitneg is exceeded
            QDesireLimitedpu := currentkvarlimitnegpu * sign(Q);
        end;

        // Qdesiredpu should be less than the Q avaliable under watt priority  (works just for varmax)
        if FPPriority and ((FReacPower_ref = ReacPower_VARMAX) or (ControlMode = WATTPF)) then
        begin
            if Q >= 0.0 then
                Q_Ppriority := Sqrt(SQR(FkVARating) - SQR(FpresentkW)) / QHeadRoom
            else
                Q_Ppriority := Sqrt(SQR(FkVARating) - SQR(FpresentkW)) / QHeadRoomNeg;

            if (abs(Q_Ppriority) < abs(QDesireLimitedpu)) and (abs(Q_Ppriority) < abs(Q)) then
            begin
                FOperation := 0.6 * sign(Q); // kVA exceeded under watt priority is considered above
                if (abs(Q) < (0.01 / 100)) or (abs(Q_Ppriority) < epsilon) then
                    FOperation := 0.0;
                QDesireLimitedpu := Q_Ppriority * sign(Q);
            end;
        end;


        // States Flags
        if (ControlMode = VOLTVAR) then
            FVVOperation := FOperation;
        if (ControlMode = WATTPF) then
            FWPOperation := FOperation;
        if (ControlMode = WATTVAR) then
            FWVOperation := FOperation;
        if (ControlMode = DRC) then
            FDRCOperation := FOperation;
        if (ControlMode = AVR) then
            FAVROperation := FOperation;
        if (CombiMode = VV_DRC) then
            FVVDRCOperation := FOperation;
        if (CombiMode = VV_VW) then
            FVVOperation := FOperation;
    end;
end;

procedure TInvControlObj.Calc_QHeadRoom(j: Integer);
begin
    with CtrlVars[j] do
    begin
        if FReacPower_ref = ReacPower_VARAVAL then
        begin
            if (abs(FpresentkW) < FkVARating) then
                QHeadRoom := SQRT(Sqr(FkVARating) - Sqr(FpresentkW))
            else
                QHeadRoom := 0.0;

            QHeadRoomNeg := QHeadRoom;
        end;

        if (FReacPower_ref = ReacPower_VARMAX) or (ControlMode = WATTPF) then
        begin
            QHeadRoom := FkvarLimit;
            QHeadRoomNeg := FkvarLimitNeg;
        end;

        if (QHeadRoom = 0.0) then
            QHeadRoom := FkvarLimit;
        if (QHeadRoomNeg = 0.0) then
            QHeadRoomNeg := FkvarLimitNeg;
    end;
end;

procedure TInvControlObj.Change_deltaQ_factor(j: Integer);
var
    DeltaV: Double;
begin
    with CtrlVars[j] do
    begin
        DeltaV := Abs(FPresentVpu - FAvgpVpuPrior);

        if (DeltaV_old >= 0.0) then
        begin
            if (abs(DeltaV) > 0.8 * DeltaV_old) and (FdeltaQFactor > 0.2) then
                FdeltaQFactor := FdeltaQFactor - 0.1
            else
            if (abs(DeltaV) > 0.6 * DeltaV_old) and (FdeltaQFactor > 0.2) then
                FdeltaQFactor := FdeltaQFactor - 0.05
            else
            if (abs(DeltaV) < 0.2 * DeltaV_old) and (FdeltaQFactor < 0.9) then
                FdeltaQFactor := FdeltaQFactor + 0.1
            else
            if (abs(DeltaV) < 0.4 * DeltaV_old) and (FdeltaQFactor < 0.9) then
                FdeltaQFactor := FdeltaQFactor + 0.05;
        end;

        DeltaV_old := Abs(FPresentVpu - FAvgpVpuPrior);
    end;
end;

procedure TInvControlObj.Change_deltaP_factor(j: Integer);
var
    DeltaV: Double;
begin
    with CtrlVars[j] do
    begin
        DeltaV := Abs(FPresentVpu - FAvgpVpuPrior);

        if DeltaV_old >= 0.0 then
        begin
            if (abs(DeltaV) > 0.9 * DeltaV_old) and (FdeltaPFactor > 0.2) then
                FdeltaPFactor := FdeltaPFactor - 0.1
            else
            if (abs(DeltaV) > 0.8 * DeltaV_old) and (FdeltaPFactor > 0.1) then
                FdeltaPFactor := FdeltaPFactor - 0.05
            else
            if (abs(DeltaV) < 0.2 * DeltaV_old) and (FdeltaPFactor < 0.9) then
                FdeltaPFactor := FdeltaPFactor + 0.05
            else
            if (abs(DeltaV) < 0.1 * DeltaV_old) and (FdeltaPFactor < 0.9) then
                FdeltaPFactor := FdeltaPFactor + 0.1;
        end;

        DeltaV_old := Abs(FPresentVpu - FAvgpVpuPrior);
    end;
end;

//Called at end of main power flow solution loop
procedure TInvControl.UpdateAll();
var
    i: Integer;
begin
    for i := 1 to ElementList.Count do
        with TInvControlObj(ElementList.Get(i)) do
            if Enabled then
                UpdateInvControl(i);
end;

finalization
    ModeEnum.Free;
    CombiModeEnum.Free;
    VoltageCurveXRefEnum.Free;
    VoltWattYAxisEnum.Free;
    RoCEnum.Free;
    RefQEnum.Free;
    ControlModelEnum.Free;
end.
