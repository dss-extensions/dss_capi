unit InvControl2;

// ----------------------------------------------------------
// Copyright (c) 2018-2022, Paulo Meira, DSS Extensions contributors
// Copyright (c) 2008-2020,  Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

uses
    RollAvgWindow,
    Command,
    ControlClass,
    ControlElem,
    CktElement,
    DSSClass,
    bus,
    PCElement,
    PVSystem2,
    Storage2,
    Arraydef,
    UComplex, DSSUcomplex,
    utilities,
    XYcurve,
    Dynamics,
    DSSPointerList,
    Classes,
    StrUtils;

type
{$SCOPEDENUMS ON}
    TInvControl2Prop = (
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
        Vsetpoint // was 33
    );
{$SCOPEDENUMS OFF}

    // Modes
{$PUSH}
{$Z4} // keep enums as int32 values
    TInvControl2ControlMode = (
        NONE_MODE = 0,
        VOLTVAR = 1,
        VOLTWATT = 2,
        DRC = 3,
        WATTPF = 4,
        WATTVAR = 5,
        AVR = 6
    );

    // Combi Modes
    TInvControl2CombiMode = (
        NONE_COMBMODE = 0,
        VV_VW = 1,
        VV_DRC = 2
    );
{$POP}

    ERateofChangeMode = (
        INACTIVE,
        LPF,
        RISEFALL
    );

    TInvControl2 = class(TControlClass)

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

    TInvControl2Obj = class(TControlElem)
    PRIVATE
        ControlActionHandle: Integer;
        ControlledElement: array of TPCElement;
        MonitoredElement: TDSSCktElement;  // First DER element for now (the first element from ControlledElement TDSSPointerList)

        // Variables for voltages
        FVreg: Double;
        FAvgpVpuPrior: array of Double;
        FAvgpDRCVpuPrior: array of Double;
        FPresentVpu: array of Double;
        FPresentDRCVpu: array of Double;
        FVpuSolution: array of array of Double;
        FVpuSolutionIdx: Integer;

        // Variables for convergence process
        FdeltaQ_factor: Double;
        FdeltaP_factor: Double;

        FdeltaQFactor: array of Double;
        FdeltaPFactor: array of Double;
        DeltaV_old: array of Double;

        FVoltageChangeTolerance: Double;
        FVarChangeTolerance: Double;
        FActivePChangeTolerance: Double;

        // Active power
        PLimitVW: array of Double;
        POldVWpu: array of Double;
        FFlagVWOperates: array of Boolean;  // Flag enabled when volt-watt Pdesired is less than 1. So volt-watt algorithm starts to work
        PLimitVWpu: array of Double;
        PLimitLimitedpu: array of Double;
        PLimitEndpu: array of Double;
        PLimitOptionpu: array of Double;
        kW_out_desiredpu: array of Double;
        kW_out_desired: array of Double;

        // Reactive power
        QDesireEndpu: array of Double;  // Q value used in the convergency algorithm
        QDesireVVpu: array of Double; // Q desired caculated in volt-var curve
        QDesireWPpu: array of Double; // Q desired caculated in watt-pf curve
        QDesireWVpu: array of Double; // Q desired caculated in watt-var curve
        QDesireDRCpu: array of Double;  // Q desired from the DRC equation
        QDesireAVRpu: array of Double;
        QDesireLimitedpu: array of Double; // Calculates possible Q considering kVA (watt priority) and kvarlimit limits
        QDesireOptionpu: array of Double; // Calculates Q Limit considering LPF and RF
        QDesiredVV: array of Double; // volt-var new set-point
        QDesiredWP: array of Double; // watt-pf new set-point
        QDesiredWV: array of Double; // watt-var new set-point
        QDesiredAVR: array of Double;
        QOld: array of Double;
        QOldVV: array of Double;
        QOldAVR: array of Double;
        QOldDRC: array of Double;
        QOldVVDRC: array of Double;
        QDesiredDRC: array of Double; //dynamic reactive power new set-point
        QDesiredVVDRC: array of Double;

        // Variables of functions that CONTROL reactive power
        QHeadRoom: array of Double;
        QHeadRoomNeg: array of Double;
        Qoutputpu: array of Double;
        QoutputVVpu: array of Double;
        QoutputDRCpu: array of Double;
        QoutputVVDRCpu: array of Double;
        QoutputAVRpu: array of Double;

        FPriorvarspu: array of Double;
        FPriorvars: array of Double;

        // Variables of functions that LIMIT active power
        PBase: array of Double;

        FPriorWattspu: array of Double;
        FPriorwatts: array of Double;

        // Variables of DER element
        FDERPointerList: TDSSPointerList;
        FListSize: Integer;
        FVBase: array of Double;
        FVarFollowInverter: array of Boolean;
        FInverterON: array of Boolean;
        FpresentkW: array of Double;
        FkVARating: array of Double;
        Fpresentkvar: array of Double;
        FkvarLimit: array of Double;
        FkvarLimitNeg: array of Double;
        FCurrentkvarLimit: array of Double;
        FCurrentkvarLimitNeg: array of Double;
        FDCkWRated: array of Double;  // Pmpp for PVSystem, kWRated for Storage
        FpctDCkWRated: array of Double;  // pctPmpp for PVSystem, pctkWRated for Storage
        FEffFactor: array of Double;
        FDCkW: array of Double;  // PanelkW for PVSystem, DCkW for Storage
        FPPriority: array of Boolean;
        NPhasesDER: array of Integer;
        NCondsDER: array of Integer;

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
        FPriorPLimitOptionpu: array of Double;
        FPriorQDesireOptionpu: array of Double;

        // Variables of the smart inverter functions
        FVoltage_CurveX_ref: Integer;  // valid values are 0: = Vref (rated), 1:= avg
        FReacPower_ref: Integer;
        FVoltwattYAxis: Integer; // 1 = %Pmpp, 0 = %Available power

      // volt-var
        Fvvc_curveOffset: Double;
        FlagChangeCurve: array of Boolean;
        FActiveVVCurve: array of Integer;
        FRollAvgWindowLength: Integer;//FVAvgWindowLengthSec // rolling average window length in seconds
        FRollAvgWindow: array of TRollAvgWindow;
        
        priorRollAvgWindow: array of Double;

      // watt-pf
        pf_wp_nominal: Double;

      // DRC
        FDbVMin: Double;
        FDbVMax: Double;
        deltaVDynReac: array of Double;
        FDRCRollAvgWindowpu: array of Double;
        FDRCRollAvgWindow: array of TRollAvgWindow;
        priorDRCRollAvgWindow: array of Double;


      // Active voltage regulation (AVR)
        Fv_setpoint: Double;
        DQDV: array of Double;
        Fv_setpointLimited: array of Double;
        FAvgpAVRVpuPrior: array of Double;

        // Flags used to record function states. They are interval variables of DER
        FVVOperation: array of Double;
        FVWOperation: array of Double;
        FDRCOperation: array of Double;
        FVVDRCOperation: array of Double;
        FWPOperation: array of Double;
        FWVOperation: array of Double;
        FAVROperation: array of Double;

        // Others
        cBuffer: array of array of Complex;    // Complex array buffer
        CondOffset: array of Integer; // Offset for monitored terminal
        FPendingChange: array of Integer;

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
        procedure GetmonVoltage(var Vpresent: Double; i: Integer; BasekV: Double);
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

        ControlMode: TInvControl2ControlMode;
        CombiMode: TInvControl2CombiMode;

        constructor Create(ParClass: TDSSClass; const InvControl2Name: Ansistring);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
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
    MathUtil,
    Math,
    DSSHelper,
    DSSObjectHelper;

type
    TObj = TInvControl2Obj;
    TProp = TInvControl2Prop;

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

type
    TPVSystemObj = TPVSystem2Obj;
    TStorageObj = TStorage2Obj;
var
    PropInfo: Pointer = NIL;    
    ModeEnum, CombiModeEnum, VoltageCurveXRefEnum, VoltWattYAxisEnum, RoCEnum, RefQEnum: TDSSEnum;

constructor TInvControl2.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        ModeEnum := TDSSEnum.Create('InvControl: Control Mode', True, 1, 5, 
            ['Voltvar', 'VoltWatt', 'DynamicReaccurr', 'WattPF', 'Wattvar', 'AVR'],
            [ord(VOLTVAR), ord(VOLTWATT), ord(DRC), ord(WATTPF), ord(WATTVAR), ord(AVR)]);
        CombiModeEnum := TDSSEnum.Create('InvControl: Combi Mode', True, 4, 4, 
            ['VV_VW', 'VV_DRC'], [ord(VV_VW), ord(VV_DRC)]);
        VoltageCurveXRefEnum := TDSSEnum.Create('InvControl: Voltage Curve X Ref', True, 1, 2, 
            ['Rated', 'Avg', 'RAvg'], [0, 1, 2]);
        VoltWattYAxisEnum := TDSSEnum.Create('InvControl: Volt-watt Y-Axis', True, 1, 2, 
            ['PAvailablePU', 'PMPPPU', 'PctPMPPPU', 'KVARatingPU'], [0, 1, 2, 3]);
        RoCEnum := TDSSEnum.Create('InvControl: Rate-of-change Mode', True, 3, 3, 
            ['Inactive', 'LPF', 'RiseFall'], [ord(INACTIVE), ord(LPF), ord(RISEFALL)]);
        RefQEnum := TDSSEnum.Create('InvControl: Reactive Power Reference', True, 4, 4, 
            ['VARAVAL', 'VARMAX'], [0, 1]);
        RefQEnum.AllowLonger := True;
    end;

    XY_CurveClass := GetDSSClassPtr(dssContext, 'XYCurve');

    inherited Create(dssContext, INV_CONTROL, 'InvControl');
end;

destructor TInvControl2.Destroy;
begin
    inherited Destroy;
end;

function GetMonBusesCount(Obj: TObj): Integer;
begin
    Result := Obj.MonBusesNameList.Count;
end;

procedure TInvControl2.DefineProperties;
var
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

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

    PropertyType[ord(TProp.CombiMode)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.CombiMode)] := ptruint(@obj.CombiMode);
    PropertyOffset2[ord(TProp.CombiMode)] := PtrInt(CombiModeEnum);

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

function TInvControl2.NewObject(const ObjName: Ansistring; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure ValidateXYCurve(dss: TDSSContext; var curve: TXYcurveObj; InvControl2Mode: TInvControl2ControlMode);
var
    i: Integer;
begin
    if curve = NIL then
        Exit;

    // if VOLTWATT control mode then check for any negative watt values (pu)
    // and values greater than 1.0 per-unit (=100 percent output)
    if InvControl2Mode = VOLTWATT then
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
    if InvControl2Mode = WATTPF then
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
    if InvControl2Mode = WATTVAR then
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

procedure TInvControl2Obj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    CharPos: Integer;
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
        begin //FMonBuses     := Param;
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
            // Because is using this command from the previous version of InvControl, we assume that the list includes only
            // PVSystems, so the list is updated
            for CharPos := 0 to (DERNameList.Count - 1) do
                DERNameList[CharPos] := 'PVSystem.' + DERNameList[CharPos];
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
    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TInvControl2Obj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i, j: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    FNPhases := Other.Fnphases;
    NConds := Other.Fnconds; // Force Reallocation of terminal stuff

    for i := 1 to FDERPointerList.Count do
    begin
        ControlledElement[i] := Other.ControlledElement[i];
        CondOffset[i] := Other.CondOffset[i];

        FVBase[i] := Other.FVBase[i];
        FVarFollowInverter[i] := Other.FVarFollowInverter[i];
        FInverterON[i] := Other.FInverterON[i];
        FpresentkW[i] := Other.FpresentkW[i];
        FkVARating[i] := Other.FkVARating[i];
        Fpresentkvar[i] := Other.Fpresentkvar[i];
        FkvarLimit[i] := Other.FkvarLimit[i];
        FkvarLimitNeg[i] := Other.FkvarLimitNeg[i];
        FCurrentkvarLimit[i] := Other.FCurrentkvarLimit[i];
        FCurrentkvarLimitNeg[i] := Other.FCurrentkvarLimitNeg[i];
        FDCkWRated[i] := Other.FDCkWRated[i];
        FpctDCkWRated[i] := Other.FpctDCkWRated[i];
        FEffFactor[i] := Other.FEffFactor[i];
        FDCkW[i] := Other.FDCkW[i];
        FPPriority[i] := Other.FPPriority[i];
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
    FActiveVVCurve := Other.FActiveVVCurve;
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

    ReallocMem(FMonBusesVbase, SizeOf(FMonBusesVbase^[1]) * MonBusesNameList.Count);
    for j := 1 to MonBusesNameList.Count do
        FMonBusesVbase^[j] := Other.FMonBusesVbase^[j];

    TimeDelay := Other.TimeDelay;
end;

constructor TInvControl2Obj.Create(ParClass: TDSSClass; const InvControl2Name: Ansistring);
begin
    inherited Create(ParClass);
    Name := AnsiLowerCase(InvControl2Name);
    DSSObjType := ParClass.DSSClassType;

    // Control elements are zero current sources that attach to a terminal of a
    // power-carrying device, but do not alter voltage or current flow.
    // Define a default number of phases and conductors here and update in
    // RecalcElementData routine if necessary. This allocates arrays for voltages
    // and currents and gives more direct access to the values,if needed
    FNPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors in base class
    ControlMode := NONE_MODE;
    CombiMode := NONE_COMBMODE;
    ControlledElement := NIL;

    // Variables for voltages
    FAvgpVpuPrior := NIL;
    FAvgpDRCVpuPrior := NIL;
    FPresentVpu := NIL;
    FPresentDRCVpu := NIL;
    FVpuSolution := NIL;
    FVpuSolutionIdx := 0;

    // Variables for convergence process
    FdeltaQ_factor := FLAGDELTAQ;
    FdeltaP_factor := FLAGDELTAP;

    FdeltaQFactor := NIL;
    FdeltaPFactor := NIL;
    DeltaV_old := NIL;

    FVoltageChangeTolerance := 0.0001;
    FVarChangeTolerance := 0.025;
    FActivePChangeTolerance := 0.01;

    // Active power
    PLimitVW := NIL;
    POldVWpu := NIL;
    FFlagVWOperates := NIL;
    PLimitVWpu := NIL;
    PLimitLimitedpu := NIL;
    PLimitEndpu := NIL;
    PLimitOptionpu := NIL;
    kW_out_desiredpu := NIL;
    kW_out_desired := NIL;

    // Reactive power
    QDesireEndpu := NIL;
    QDesireVVpu := NIL;
    QDesireWPpu := NIL;
    QDesireWVpu := NIL;
    QDesireDRCpu := NIL;
    QDesireAVRpu := NIL;
    QDesireLimitedpu := NIL;
    QDesireOptionpu := NIL;
    QDesiredVV := NIL;
    QDesiredWP := NIL;
    QDesiredWV := NIL;
    QOld := NIL;
    QOldVV := NIL;
    QOldAVR := NIL;
    QOldDRC := NIL;
    QOldVVDRC := NIL;
    QDesiredDRC := NIL;
    QDesiredVVDRC := NIL;
    QDesiredAVR := NIL;

    // Variables of functions that CONTROL reactive power
    QHeadRoom := NIL;
    QHeadRoomNeg := NIL;
    Qoutputpu := NIL;
    QoutputVVpu := NIL;
    QoutputDRCpu := NIL;
    QoutputVVDRCpu := NIL;
    QoutputAVRpu := NIL;

    FPriorvarspu := NIL;
    FPriorvars := NIL;

    // Variables of functions that LIMIT active power
    PBase := NIL;

    FPriorWattspu := NIL;
    FPriorWatts := NIL;

    // Variables of DER element
    DERNameList := NIL;
    FDERPointerList := NIL;
    FDERPointerList := TDSSPointerList.Create(20);  // Default size and increment
    DERNameList := TSTringList.Create;
    FVBase := NIL;
    FVarFollowInverter := NIL;
    FInverterON := NIL;
    FpresentkW := NIL;
    FkVARating := NIL;
    Fpresentkvar := NIL;
    FkvarLimit := NIL;
    FkvarLimitNeg := NIL;
    FCurrentkvarLimit := NIL;
    FCurrentkvarLimitNeg := NIL;
    FDCkWRated := NIL;
    FpctDCkWRated := NIL;
    FEffFactor := NIL;
    FDCkW := NIL;
    FPPriority := NIL;
    NPhasesDER := NIL;
    NCondsDER := NIL;

    // Variables for monitored Bus/buses
    MonBusesNameList := TStringList.Create;
    FMonBusesPhase := AVGPHASES;
    FMonBuses := NIL;
    FMonBusesVbase := NIL;
    FMonBusesNodes := NIL;

    // Variables for LPF and RF options
    RateofChangeMode := INACTIVE;
    LPFTau := 0.001;
    FRiseFallLimit := 0.001;
    FPriorPLimitOptionpu := NIL;
    FPriorQDesireOptionpu := NIL;

    // Variables of the smart inverter functions
    FVoltage_CurveX_ref := 0;
    FReacPower_ref := ReacPower_VARAVAL;
    FVoltwattYAxis := 1;

    // volt-var
    Fvvc_curve := NIL;
    Fvvc_curveOffset := 0.0;
    FActiveVVCurve := NIL;
    FlagChangeCurve := NIL;
    FRollAvgWindow := NIL;
    FRollAvgWindowLength := 1;
    priorRollAvgWindow := NIL;

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
    FDRCRollAvgWindow := NIL;
    FDRCRollAvgWindowLength := 1;
    priorDRCRollAvgWindow := NIL;
    deltaVDynReac := NIL;

    // volt-watt
    Fvoltwatt_curve := NIL;
    FvoltwattCH_curve := NIL;

    // AVR
    Fv_setpoint := 1.0;
    DQDV := NIL;
    Fv_setpointLimited := NIL;
    FAvgpAVRVpuPrior := NIL;

    // Flags used to record function states. They are interval variables of DER
    FVVOperation := NIL;
    FVWOperation := NIL;
    FDRCOperation := NIL;
    FVVDRCOperation := NIL;
    FWPOperation := NIL;
    FWVOperation := NIL;
    FAVROperation := NIL;

    // Others
    FPendingChange := NIL;
    cBuffer := NIL;
    CondOffset := NIL;

    ShowEventLog := FALSE; // match SVN r3458
end;

destructor TInvControl2Obj.Destroy;
begin
    Finalize(ControlledElement);
    Finalize(NPhasesDER);
    Finalize(NCondsDER);
    Finalize(cBuffer);
    Finalize(CondOffset);
    Finalize(FlagChangeCurve);
    Finalize(FActiveVVCurve);
    Finalize(FFlagVWOperates);
    Finalize(FMonBuses);
    Finalize(FMonBusesNodes);
    Finalize(FVarFollowInverter);
    Finalize(Fv_setpoint);

    if Assigned(FMonBusesVbase) then
        ReallocMem(FMonBusesVbase, 0);

    inherited Destroy;
end;

procedure TInvControl2Obj.RecalcElementData();
var
    i: Integer;
begin
    if FDERPointerList.Count = 0 then
        MakeDERList;

    if FDERPointerList.Count > 0 then
    {Setting the terminal of the InvControl device to same as the 1st PVSystem/Storage element}
    { This sets it to a realistic value to avoid crashes later }
    begin
        MonitoredElement := TDSSCktElement(FDERPointerList.Get(1));   // Set MonitoredElement to 1st elemnent in list
        Setbus(1, MonitoredElement.Firstbus);
    end;

    for i := 1 to FDERPointerList.Count do
    begin
        // User ControlledElement[] as the pointer to the PVSystem/Storage elements
        ControlledElement[i] := TPCElement(FDERPointerList.Get(i));  // pointer to i-th PVSystem/Storage element
        SetLength(cBuffer[i], SizeOF(Complex) * ControlledElement[i].Yorder);


        ControlledElement[i].ActiveTerminalIdx := 1; // Make the 1 st terminal active
        FNphases := ControlledElement[i].NPhases;
        Nconds := Nphases;
        FRollAvgWindow[i].SetLength(FRollAvgWindowLength);
        FDRCRollAvgWindow[i].SetLength(FDRCRollAvgWindowLength);

        // for all modes other than VW and WATTPF, PF priority is not allowed
        if ((ControlMode <> VOLTWATT) and (ControlMode <> WATTPF)) then
        begin
            if ControlledElement[i].DSSClassName = 'PVSystem' then
                TPVSystemObj(ControlledElement[i]).PVSystemVars.PF_Priority := FALSE
            else
            if ControlledElement[i].DSSClassName = 'Storage' then
                TStorageObj(ControlledElement[i]).StorageVars.PF_Priority := FALSE;
        end;

        //FdeltaQFactor[i]                := FdeltaQ_factor;
        //FdeltaPFactor[i]                := FdeltaP_factor;

        if Length(FMonBuses) = 0 then
            FUsingMonBuses := FALSE
        else
            FUsingMonBuses := TRUE;

        if (ControlledElement[i] <> NIL) then
            UpdateDERParameters(i)
        else
        begin
            ControlledElement[i] := NIL;
            DoErrorMsg(Format(_('InvControl: "%s"'), [Self.Name]),
                Format(_('Controlled Element "%s" not found.'), [DERNameList.Strings[i - 1]]),
                _('PVSystem or Storage object must be defined previously.'), 361);
        end;
    end;
end;

procedure TInvControl2Obj.MakePosSequence();
// ***  This assumes the PVSystem/Storage devices have already been converted to pos seq
begin
    if FDERPointerList.Count = 0 then
        RecalcElementData();
    FNphases := 3;
    Nconds := 3;
    Setbus(1, MonitoredElement.GetBus(ElementTerminal));

    if FDERPointerList.Count > 0 then
    {Setting the terminal of the InvControl device to same as the 1st PVSystem/Storage element}
    { This sets it to a realistic value to avoid crashes later }
    begin
        MonitoredElement := TDSSCktElement(FDERPointerList.Get(1));   // Set MonitoredElement to 1st PVSystem/Storage in list
        Setbus(1, MonitoredElement.Firstbus);
        FNphases := MonitoredElement.NPhases;
        Nconds := Nphases;
    end;
    inherited;
end;

procedure TInvControl2Obj.DoPendingAction(const Code, ProxyHdl: Integer);
var
    k: Integer;
    DERelem: TPCElement;
begin
    for k := 1 to FDERPointerList.Count do
    begin
        DERelem := ControlledElement[k];

        // Calculates QHeadRoom
        Calc_QHeadRoom(k);
        if QHeadRoom[k] <> 0.0 then
            FPriorvarspu[k] := FPriorvars[k] / QHeadRoom[k];

        // Calculates PBase
        Calc_PBase(k);
        FPriorWattspu[k] := FPriorWatts[k] / PBase[k];

        // Calculates kW_out_desiredpu. Used for VW and VV_VW
        kW_out_desiredpu[k] := kW_out_desired[k] / PBase[k];

        // -------------------Smart Inverter Functions------------------------//
        // Smart Inverter volt-var function
        if (ControlMode = VOLTVAR) and (CombiMode = NONE_COMBMODE) and (PendingChange[k] = CHANGEVARLEVEL) then
        begin
            // Set var mode to VARMODEKVAR to indicate we might change kvar
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                TPVSystemObj(DERelem).VWmode := FALSE;
                TPVSystemObj(DERelem).Varmode := VARMODEKVAR;
                TPVSystemObj(DERelem).VVmode := TRUE;
            end
            else
            begin
                TStorageObj(DERelem).VWmode := FALSE;
                TStorageObj(DERelem).Varmode := VARMODEKVAR;
                TStorageObj(DERelem).VVmode := TRUE;
            end;

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireVVpu[k]
            CalcQVVcurve_desiredpu(k);

            // LPF or RF activated
            if (RateofChangeMode = LPF) then
            begin
                CalcLPF(k, 'VARS', QDesireVVpu[k]);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu[k]);
                QDesireEndpu[k] := Min(abs(QDesireLimitedpu[k]), abs(QDesireOptionpu[k])) * sign(QDesireOptionpu[k]);
            end
            else
            if (RateofChangeMode = RISEFALL) then
            begin
                CalcRF(k, 'VARS', QDesireVVpu[k]);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu[k]);
                QDesireEndpu[k] := Min(abs(QDesireLimitedpu[k]), abs(QDesireOptionpu[k])) * sign(QDesireOptionpu[k]);
            end
            else
            begin
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireVVpu[k]);
                QDesireEndpu[k] := Min(abs(QDesireVVpu[k]), abs(QDesireLimitedpu[k])) * sign(QDesireVVpu[k]);
            end;

            // Calculates QDesiredVV[k] through the convergence algorithm
            CalcVoltVar_vars(k);

            //--------------------------------------------- end Main process ---------------------------------------------//

            // Sets PVSystem/Storage's kvar_out
            if ControlledElement[k].DSSClassName = 'PVSystem' then
                TPVSystemObj(DERelem).Presentkvar := QDesiredVV[k]
            else
                TStorageObj(DERelem).kvarRequested := QDesiredVV[k];

            // Uptates PresentkW and Presentkvar considering watt and var priorities
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                TPVSystemObj(DERelem).SetNominalPVSystemOuput();

                if QDesiredVV[k] >= 0.0 then
                    Qoutputpu[k] := TPVSystemObj(DERelem).Presentkvar / QHeadroom[k]
                else
                    Qoutputpu[k] := TPVSystemObj(DERelem).Presentkvar / QHeadroomNeg[k];
            end
            else
            begin
                TStorageObj(DERelem).SetNominalStorageOutput();

                if QDesiredVV[k] >= 0.0 then
                    Qoutputpu[k] := TStorageObj(DERelem).Presentkvar / QHeadroom[k]
                else
                    Qoutputpu[k] := TStorageObj(DERelem).Presentkvar / QHeadroomNeg[k];
            end;

            // Values used in convergence
            QoutputVVpu[k] := Qoutputpu[k];
            FAvgpVpuPrior[k] := FPresentVpu[k];

            // Values used in CalcQVVcurve_desiredpu
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                QOld[k] := TPVSystemObj(DERelem).Presentkvar;
                QOldVV[k] := TPVSystemObj(DERelem).Presentkvar;

                if ShowEventLog then
                    AppendtoEventLog('InvControl.' + Self.Name + ', ' + TPVSystemObj(DERelem).FullName,
                        Format('VOLTVAR mode requested PVSystem output var level to**, kvar= %.5g. Actual output set to kvar= %.5g.',
                        [QDesiredVV[k], TPVSystemObj(DERelem).Presentkvar]));
            end
            else
            begin
                QOld[k] := TStorageObj(DERelem).Presentkvar;
                QOldVV[k] := TStorageObj(DERelem).Presentkvar;

                if ShowEventLog then
                    AppendtoEventLog('InvControl.' + Self.Name + ', ' + TStorageObj(DERelem).FullName,
                        Format('VOLTVAR mode requested Storage output var level to **, kvar = %.5g. Actual output set to kvar= %.5g.',
                        [QDesiredVV[k], TStorageObj(DERelem).Presentkvar]));

            end;
        end

        {Smart Inverter active voltage regulation function}
        else
        if (ControlMode = AVR) and (CombiMode = NONE_COMBMODE) and (PendingChange[k] = CHANGEVARLEVEL) then
        begin
            // Set var mode to VARMODEKVAR to indicate we might change kvar
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                TPVSystemObj(DERelem).VWmode := FALSE;
                TPVSystemObj(DERelem).Varmode := VARMODEKVAR;
                TPVSystemObj(DERelem).AVRmode := TRUE;
            end
            else
            begin
                TStorageObj(DERelem).VWmode := FALSE;
                TStorageObj(DERelem).Varmode := VARMODEKVAR;
//                TStorageObj(DERelem).VVmode     := TRUE;
            end;

            //--------------------------------------------- Main process ---------------------------------------------//

            if ActiveCircuit.Solution.ControlIteration = 1 then
            begin
                FAvgpVpuPrior[k] := FPresentVpu[k];
                FAvgpAVRVpuPrior[k] := FPresentVpu[k];

                 // Sets PVSystem/Storage's kvar_out
                if ControlledElement[k].DSSClassName = 'PVSystem' then
                    TPVSystemObj(DERelem).Presentkvar := QHeadRoom[k] / 2
                else
                    TStorageObj(DERelem).kvarRequested := QHeadRoom[k] / 2;
            end

            else
            if ActiveCircuit.Solution.ControlIteration = 2 then
            begin
                // Sets PVSystem/Storage's kvar_out
                if ControlledElement[k].DSSClassName = 'PVSystem' then
                    DQDV[k] := abs(TPVSystemObj(DERelem).Presentkvar / QHeadRoom[k] / (FPresentVpu[k] - FAvgpVpuPrior[k]))
                else
                    DQDV[k] := abs(TStorageObj(DERelem).kvarRequested / QHeadRoom[k] / (FPresentVpu[k] - FAvgpVpuPrior[k]));
            end

            else
            begin
                // Calculates QDesireAVRpu[k]
                CalcQAVR_desiredpu(k);


                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireAVRpu[k]);
                QDesireEndpu[k] := Min(abs(QDesireAVRpu[k]), abs(QDesireLimitedpu[k])) * sign(QDesireAVRpu[k]);

                if abs(QDesireEndpu[k] - QDesireLimitedpu[k]) < 0.05 then
                    Fv_setpointLimited[k] := FPresentVpu[k]
                else
                    Fv_setpointLimited[k] := Fv_setpoint;

                // Calculates QDesiredVV[k] through the convergence algorithm
                CalcAVR_vars(k);

                //--------------------------------------------- end Main process ---------------------------------------------//

                // Sets PVSystem/Storage's kvar_out
                if ControlledElement[k].DSSClassName = 'PVSystem' then
                    TPVSystemObj(DERelem).Presentkvar := QDesiredAVR[k]
                else
                    TStorageObj(DERelem).kvarRequested := QDesiredAVR[k];

                // Uptates PresentkW and Presentkvar considering watt and var priorities
                if ControlledElement[k].DSSClassName = 'PVSystem' then
                begin
                    TPVSystemObj(DERelem).SetNominalPVSystemOuput();

                    if QDesiredAVR[k] >= 0.0 then
                        Qoutputpu[k] := TPVSystemObj(DERelem).Presentkvar / QHeadroom[k]
                    else
                        Qoutputpu[k] := TPVSystemObj(DERelem).Presentkvar / QHeadroomNeg[k];
                end
                else
                begin
                    TStorageObj(DERelem).SetNominalStorageOutput();

                    if QDesiredAVR[k] >= 0.0 then
                        Qoutputpu[k] := TStorageObj(DERelem).Presentkvar / QHeadroom[k]
                    else
                        Qoutputpu[k] := TStorageObj(DERelem).Presentkvar / QHeadroomNeg[k];
                end;

              // Values used in convergence
                QoutputAVRpu[k] := Qoutputpu[k];
                FAvgpVpuPrior[k] := FPresentVpu[k];

              // Values used in CalcQVVcurve_desiredpu
                if ControlledElement[k].DSSClassName = 'PVSystem' then
                begin
                    QOld[k] := TPVSystemObj(DERelem).Presentkvar;
                    QOldAVR[k] := TPVSystemObj(DERelem).Presentkvar;

                    if ShowEventLog then
                        AppendtoEventLog('InvControl.' + Self.Name + ', ' + TPVSystemObj(DERelem).FullName,
                            Format('VOLTVAR mode requested PVSystem output var level to**, kvar= %.5g. Actual output set to kvar= %.5g.',
                            [QDesiredAVR[k], TPVSystemObj(DERelem).Presentkvar]));
                end
                else
                begin
                    QOld[k] := TStorageObj(DERelem).Presentkvar;
                    QOldAVR[k] := TStorageObj(DERelem).Presentkvar;

                    if ShowEventLog then
                        AppendtoEventLog('InvControl.' + Self.Name + ', ' + TStorageObj(DERelem).FullName,
                            Format('VOLTVAR mode requested Storage output var level to **, kvar = %.5g. Actual output set to kvar= %.5g.',
                            [QDesiredAVR[k], TStorageObj(DERelem).Presentkvar]));

                end;
            end;
        end

        {Smart Inverter watt-pf function}
        else
        if (ControlMode = WATTPF) and (CombiMode = NONE_COMBMODE) and (PendingChange[k] = CHANGEVARLEVEL) then
        begin
            // Set var mode to VARMODEKVAR to indicate we might change kvar
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                TPVSystemObj(DERelem).VWmode := FALSE;
                TPVSystemObj(DERelem).Varmode := VARMODEKVAR;
                TPVSystemObj(DERelem).WPmode := TRUE;
            end
            else
            begin
                TStorageObj(DERelem).VWmode := FALSE;
                TStorageObj(DERelem).Varmode := VARMODEKVAR;
                TStorageObj(DERelem).WPmode := TRUE;
            end;

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireWPpu[k]
            CalcQWPcurve_desiredpu(k);

            // Checks kVA (watt priority) and kvarlimit limits
            Check_Qlimits(k, QDesireWPpu[k]);
            QDesireEndpu[k] := Min(abs(QDesireWPpu[k]), abs(QDesireLimitedpu[k])) * sign(QDesireWPpu[k]);

            // Calculates QDesiredWP[k] through the convergence algorithm
            CalcWATTPF_vars(k);

            //--------------------------------------------- end Main process ---------------------------------------------//
            // Sets PVSystem/Storage's pf_wp_nominal
            if ControlledElement[k].DSSClassName = 'PVSystem' then
                TPVSystemObj(DERelem).pf_wp_nominal := pf_wp_nominal
            else
                TStorageObj(DERelem).kvarRequested := QDesiredWP[k];

            // Sets PVSystem/Storage's kvar_out
            if ControlledElement[k].DSSClassName = 'PVSystem' then
                TPVSystemObj(DERelem).Presentkvar := QDesiredWP[k]
            else
                TStorageObj(DERelem).kvarRequested := QDesiredWP[k];

            // Uptates PresentkW and Presentkvar considering watt and var priorities
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                TPVSystemObj(DERelem).SetNominalPVSystemOuput();

                if QDesiredWP[k] >= 0.0 then
                    Qoutputpu[k] := TPVSystemObj(DERelem).Presentkvar / QHeadroom[k]
                else
                    Qoutputpu[k] := TPVSystemObj(DERelem).Presentkvar / QHeadroomNeg[k];
            end
            else
            begin
                TStorageObj(DERelem).SetNominalStorageOutput();

                if QDesiredWP[k] >= 0.0 then
                    Qoutputpu[k] := TStorageObj(DERelem).Presentkvar / QHeadroom[k]
                else
                    Qoutputpu[k] := TStorageObj(DERelem).Presentkvar / QHeadroomNeg[k];
            end;

            // Values used in convergence
            QoutputVVpu[k] := Qoutputpu[k];
            FAvgpVpuPrior[k] := FPresentVpu[k];

            // Values used in CalcQVVcurve_desiredpu
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                QOld[k] := TPVSystemObj(DERelem).Presentkvar;
                QOldVV[k] := TPVSystemObj(DERelem).Presentkvar;

                if ShowEventLog then
                    AppendtoEventLog('InvControl.' + Self.Name + ', ' + TPVSystemObj(DERelem).FullName,
                        Format('WATTPF mode requested PVSystem output var level to**, kvar= %.5g. Actual output set to kvar= %.5g.',
                        [QDesiredWP[k], TPVSystemObj(DERelem).Presentkvar]));
            end
            else
            begin
                QOld[k] := TStorageObj(DERelem).Presentkvar;
                QOldVV[k] := TStorageObj(DERelem).Presentkvar;

                if ShowEventLog then
                    AppendtoEventLog('InvControl.' + Self.Name + ', ' + TStorageObj(DERelem).FullName,
                        Format('WATTPF mode requested Storage output var level to **, kvar = %.5g. Actual output set to kvar= %.5g.',
                        [QDesiredWP[k], TStorageObj(DERelem).Presentkvar]));

            end;
        end

        {Smart Inverter watt-var function}
        else
        if (ControlMode = WATTVAR) and (CombiMode = NONE_COMBMODE) and (PendingChange[k] = CHANGEVARLEVEL) then
        begin
            // Set var mode to VARMODEKVAR to indicate we might change kvar
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                TPVSystemObj(DERelem).VWmode := FALSE;
                TPVSystemObj(DERelem).Varmode := VARMODEKVAR;
                TPVSystemObj(DERelem).WVmode := TRUE;
            end
            else
            begin
                TStorageObj(DERelem).VWmode := FALSE;
                TStorageObj(DERelem).Varmode := VARMODEKVAR;
                TStorageObj(DERelem).WVmode := TRUE;
            end;

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireWVpu[k]
            CalcQWVcurve_desiredpu(k);

            // Checks kVA (watt priority) and kvarlimit limits
            Check_Qlimits_WV(k, QDesireWVpu[k]);
            QDesireEndpu[k] := Min(abs(QDesireWVpu[k]), abs(QDesireLimitedpu[k])) * sign(QDesireWVpu[k]);

            // It checks kVA or Q limits and makes sure the final P and Q stay in the watt-var curve (PauloRadatz - 2/16/2021)
            Calc_PQ_WV(k);

            //--------------------------------------------- end Main process ---------------------------------------------//

            // Sets PVSystem/Storage's kvar_out
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                TPVSystemObj(DERelem).Presentkvar := QDesiredWV[k];
                TPVSystemObj(DERelem).PresentkW := PLimitEndpu[k] * Min(FkVARating[k], FDCkWRated[k]);
            end
            else
                TStorageObj(DERelem).kvarRequested := QDesiredWV[k];

            // Uptates PresentkW and Presentkvar considering watt and var priorities
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                TPVSystemObj(DERelem).SetNominalPVSystemOuput();

                if QDesiredWV[k] >= 0.0 then
                    Qoutputpu[k] := TPVSystemObj(DERelem).Presentkvar / QHeadroom[k]
                else
                    Qoutputpu[k] := TPVSystemObj(DERelem).Presentkvar / QHeadroomNeg[k];
            end
            else
            begin
                TStorageObj(DERelem).SetNominalStorageOutput();

                if QDesiredWV[k] >= 0.0 then
                    Qoutputpu[k] := TStorageObj(DERelem).Presentkvar / QHeadroom[k]
                else
                    Qoutputpu[k] := TStorageObj(DERelem).Presentkvar / QHeadroomNeg[k];
            end;

            // Values used in convergence
            QoutputVVpu[k] := Qoutputpu[k];
            FAvgpVpuPrior[k] := FPresentVpu[k];

            // Values used in CalcQVVcurve_desiredpu
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                QOld[k] := TPVSystemObj(DERelem).Presentkvar;
                QOldVV[k] := TPVSystemObj(DERelem).Presentkvar;

                if ShowEventLog then
                    AppendtoEventLog('InvControl.' + Self.Name + ', ' + TPVSystemObj(DERelem).FullName,
                        Format('WATTVAR mode requested PVSystem output var level to**, kvar= %.5g. Actual output set to kvar= %.5g.',
                        [QDesiredWV[k], TPVSystemObj(DERelem).Presentkvar]));
            end
            else
            begin
                QOld[k] := TStorageObj(DERelem).Presentkvar;
                QOldVV[k] := TStorageObj(DERelem).Presentkvar;

                if ShowEventLog then
                    AppendtoEventLog('InvControl.' + Self.Name + ', ' + TStorageObj(DERelem).FullName,
                        Format('WATTVAR mode requested Storage output var level to **, kvar = %.5g. Actual output set to kvar= %.5g.',
                        [QDesiredWV[k], TStorageObj(DERelem).Presentkvar]));

            end;
        end

        {Smart Inverter DRC function}
        else
        if (ControlMode = DRC) and (CombiMode = NONE_COMBMODE) and (PendingChange[k] = CHANGEVARLEVEL) then
        begin
            // Set var mode to VARMODEKVAR to indicate we might change kvar
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                TPVSystemObj(DERelem).VWmode := FALSE;
                TPVSystemObj(DERelem).Varmode := VARMODEKVAR;
                TPVSystemObj(DERelem).DRCmode := TRUE;
            end
            else
            begin
                TStorageObj(DERelem).VWmode := FALSE;
                TStorageObj(DERelem).Varmode := VARMODEKVAR;
                TStorageObj(DERelem).DRCmode := TRUE;
            end;

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireDRCpu[k]
            CalcQDRC_desiredpu(k);

            // LPF or RF activated
            if (RateofChangeMode = LPF) then
            begin
                CalcLPF(k, 'VARS', QDesireDRCpu[k]);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu[k]);
                QDesireEndpu[k] := Min(abs(QDesireLimitedpu[k]), abs(QDesireOptionpu[k])) * sign(QDesireOptionpu[k]);
            end
            else
            if (RateofChangeMode = RISEFALL) then
            begin
                CalcRF(k, 'VARS', QDesireDRCpu[k]);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu[k]);
                QDesireEndpu[k] := Min(abs(QDesireLimitedpu[k]), abs(QDesireOptionpu[k])) * sign(QDesireOptionpu[k]);
            end
            else
            begin
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireDRCpu[k]);
                QDesireEndpu[k] := Min(abs(QDesireDRCpu[k]), abs(QDesireLimitedpu[k])) * sign(QDesireDRCpu[k]);
            end;

            // Calculates QDesiredDRC[k]
            CalcDRC_vars(k);

            //--------------------------------------------- end main process ---------------------------------------------//

            // Sets DER kvar_out
            if ControlledElement[k].DSSClassName = 'PVSystem' then
                TPVSystemObj(DERelem).Presentkvar := QDesiredDRC[k]
            else
                TStorageObj(DERelem).kvarRequested := QDesiredDRC[k];

            // Uptates PresentkW and Presentkvar considering watt and var priorities
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                TPVSystemObj(DERelem).SetNominalPVSystemOuput();

                if QDesiredDRC[k] >= 0.0 then
                    Qoutputpu[k] := TPVSystemObj(DERelem).Presentkvar / QHeadroom[k]
                else
                    Qoutputpu[k] := TPVSystemObj(DERelem).Presentkvar / QHeadroomNeg[k];
            end
            else
            begin
                TStorageObj(DERelem).SetNominalStorageOutput();

                if QDesiredDRC[k] >= 0.0 then
                    Qoutputpu[k] := TStorageObj(DERelem).Presentkvar / QHeadroom[k]
                else
                    Qoutputpu[k] := TStorageObj(DERelem).Presentkvar / QHeadroomNeg[k];
            end;

            // Values used in convergence
            QoutputDRCpu[k] := Qoutputpu[k];
            FAvgpDRCVpuPrior[k] := FPresentDRCVpu[k];

            // Values used in CalcDRC_vars
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                QOld[k] := TPVSystemObj(DERelem).Presentkvar;
                QOldDRC[k] := TPVSystemObj(DERelem).Presentkvar;

                if ShowEventLog then
                    AppendtoEventLog('InvControl.' + Self.Name + ', ' + TPVSystemObj(DERelem).FullName,
                        Format('DRC mode requested PVSystem output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.',
                        [QDesiredDRC[k], TPVSystemObj(DERelem).Presentkvar]));
            end
            else
            begin
                QOld[k] := TStorageObj(DERelem).Presentkvar;
                QOldDRC[k] := TStorageObj(DERelem).Presentkvar;

                if ShowEventLog then
                    AppendtoEventLog('InvControl.' + Self.Name + ', ' + TStorageObj(DERelem).FullName,
                        Format('DRC mode requested Storage output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.',
                        [QDesiredDRC[k], TStorageObj(DERelem).Presentkvar]));

            end;
        end

        // Smart Inverter VV_DRC function
        else
        if (ControlMode = NONE_MODE) and (CombiMode = VV_DRC) and (PendingChange[k] = CHANGEDRCVVARLEVEL) then
        begin
            // Set var mode to VARMODEKVAR to indicate we might change kvar
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                TPVSystemObj(DERelem).VWmode := FALSE;
                TPVSystemObj(DERelem).Varmode := VARMODEKVAR;
                TPVSystemObj(DERelem).VVmode := TRUE;
                TPVSystemObj(DERelem).DRCmode := TRUE;
            end
            else
            begin
                TStorageObj(DERelem).VWmode := FALSE;
                TStorageObj(DERelem).Varmode := VARMODEKVAR;
                TStorageObj(DERelem).VVmode := TRUE;
                TStorageObj(DERelem).DRCmode := TRUE;
            end;

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireVVpu[k] and  QDesireDRCpu[k]
            CalcQVVcurve_desiredpu(k);
            CalcQDRC_desiredpu(k);

            // LPF or RF activated
            if (RateofChangeMode = LPF) then
            begin
                CalcLPF(k, 'VARS', QDesireVVpu[k] + QDesireDRCpu[k]);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu[k]);
                QDesireEndpu[k] := Min(abs(QDesireLimitedpu[k]), abs(QDesireOptionpu[k])) * sign(QDesireOptionpu[k]);
            end
            else
            if (RateofChangeMode = RISEFALL) then
            begin
                CalcRF(k, 'VARS', QDesireVVpu[k] + QDesireDRCpu[k]);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu[k]);
                QDesireEndpu[k] := Min(abs(QDesireLimitedpu[k]), abs(QDesireOptionpu[k])) * sign(QDesireOptionpu[k]);
            end
            else
            begin
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireVVpu[k] + QDesireDRCpu[k]);
                QDesireEndpu[k] := Min(abs(QDesireVVpu[k] + QDesireDRCpu[k]), abs(QDesireLimitedpu[k])) * sign(QDesireVVpu[k] + QDesireDRCpu[k]);
            end;

            // Calculates QDesiredVVDRC[k]
            CalcVVDRC_vars(k);

            //--------------------------------------------- end main process ---------------------------------------------//

            // Sets DER kvar_out
            if ControlledElement[k].DSSClassName = 'PVSystem' then
                TPVSystemObj(DERelem).Presentkvar := QDesiredVVDRC[k]
            else
                TStorageObj(DERelem).kvarRequested := QDesiredVVDRC[k];

            // Uptates PresentkW and Presentkvar considering watt and var priorities
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                TPVSystemObj(DERelem).SetNominalPVSystemOuput();

                if QDesiredVVDRC[k] >= 0.0 then
                    Qoutputpu[k] := TPVSystemObj(DERelem).Presentkvar / QHeadroom[k]
                else
                    Qoutputpu[k] := TPVSystemObj(DERelem).Presentkvar / QHeadroomNeg[k];
            end
            else
            begin
                TStorageObj(DERelem).SetNominalStorageOutput();

                if QDesiredVVDRC[k] >= 0.0 then
                    Qoutputpu[k] := TStorageObj(DERelem).Presentkvar / QHeadroom[k]
                else
                    Qoutputpu[k] := TStorageObj(DERelem).Presentkvar / QHeadroomNeg[k];
            end;

            // Values used in convergence
            QoutputVVDRCpu[k] := Qoutputpu[k];
            FAvgpVpuPrior[k] := FPresentVpu[k];
            FAvgpDRCVpuPrior[k] := FPresentDRCVpu[k];

            // Values used in CalcQVVcurve_desiredpu and CalcVVDRC_vars
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                QOld[k] := TPVSystemObj(DERelem).Presentkvar;
                QOldVVDRC[k] := TPVSystemObj(DERelem).Presentkvar;

                if ShowEventLog then
                    AppendtoEventLog('InvControl.' + Self.Name + ', ' + TPVSystemObj(DERelem).FullName,
                        Format('**VV_DRC mode requested PVSystem output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.',
                        [QDesiredVVDRC[k], TPVSystemObj(DERelem).Presentkvar]));
            end
            else
            begin
                QOld[k] := TStorageObj(DERelem).Presentkvar;
                QOldVVDRC[k] := TStorageObj(DERelem).Presentkvar;

                if ShowEventLog then
                    AppendtoEventLog('InvControl.' + Self.Name + ', ' + TStorageObj(DERelem).FullName,
                        Format('**VV_DRC mode requested Storage output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.',
                        [QDesiredVVDRC[k], TStorageObj(DERelem).Presentkvar]));
            end;
        end

        {Smart Inverter volt-watt function}
        else
        if (ControlMode = VOLTWATT) and (CombiMode = NONE_COMBMODE) and (PendingChange[k] = CHANGEWATTLEVEL) then
        begin
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                TPVSystemObj(DERelem).VWmode := TRUE;
            end
            else
            begin
                TStorageObj(DERelem).VWmode := TRUE;
            end;

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QVWcurve_limitpu[k]
            CalcPVWcurve_limitpu(k);

            // LPF or RF activated
            if (RateofChangeMode = LPF) then
            begin
                CalcLPF(k, 'WATTS', PLimitVWpu[k]);
                // Checks kVA (var priority) and pctPmpp limits
                Check_Plimits(k, PLimitOptionpu[k]);
                PLimitEndpu[k] := Min(PLimitLimitedpu[k], PLimitOptionpu[k]);
            end
            else
            if (RateofChangeMode = RISEFALL) then
            begin
                CalcRF(k, 'WATTS', PLimitVWpu[k]);
                // Checks kVA (var priority) and pctPmpp limits
                Check_Plimits(k, PLimitOptionpu[k]);
                PLimitEndpu[k] := Min(PLimitLimitedpu[k], PLimitOptionpu[k]);
            end
            else
            begin
                // Checks kVA (var priority) and pctPmpp limits
                Check_Plimits(k, PLimitVWpu[k]);
                PLimitEndpu[k] := Min(abs(PLimitLimitedpu[k]), abs(PLimitVWpu[k])) * sign(PLimitVWpu[k]);
            end;

            // Calculates PLimitVW[k] through the convergence algorithm
            CalcVoltWatt_watts(k);

            //--------------------------------------------- end main process ---------------------------------------------//

            // Sets DER kW_out
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                TPVSystemObj(DERelem).PresentkW := PLimitVW[k];

                // Uptates PresentkW and Presentkvar considering watt and var priorities
                TPVSystemObj(DERelem).SetNominalPVSystemOuput();

            end
            else
            begin
                TStorageObj(DERelem).kWRequested := PLimitVW[k];

                // Uptates PresentkW and Presentkvar considering watt and var priorities
                TStorageObj(DERelem).SetNominalStorageOutput();
            end;


            // Values used in convergence
            FAvgpVpuPrior[k] := FPresentVpu[k];
            POldVWpu[k] := PLimitVW[k] / PBase[k];

            // Flag has to do set to 0 when kW_out is lower than Ptemp (max power allowed from volt-watt function)
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                if ((abs(PLimitVW[k]) > 0.0) and (abs(TPVSystemObj(DERelem).presentkW - PLimitVW[k]) / PLimitVW[k] > 0.0001)) then
                    FVWOperation[k] := 0; // 0.01% is the value chosen at the moment

                if ShowEventLog then
                    AppendtoEventLog('InvControl.' + Self.Name + ', ' + TPVSystemObj(DERelem).FullName,
                        Format('**VOLTWATT mode set PVSystem kw output limit to **, kw= %.5g. Actual output is kw= %.5g.',
                        [PLimitVW[k], TPVSystemObj(DERelem).presentkW]));
            end
            else
            begin
                if abs(abs(TStorageObj(DERelem).presentkW) - PLimitVW[k]) / PLimitVW[k] > 0.0001 then
                    FVWOperation[k] := 0; // 0.01% is the value chosen at the moment

                if ShowEventLog then
                    AppendtoEventLog('InvControl.' + Self.Name + ', ' + TStorageObj(DERelem).FullName,
                        Format('**VOLTWATT mode set Storage kw output limit to ** kw= %.5g. Actual output is kw= %.5g.',
                        [PLimitVW[k], TStorageObj(DERelem).presentkW]));

            end;
        end

        else
        if (ControlMode = NONE_MODE) and (CombiMode = VV_VW) and (PendingChange[k] = CHANGEWATTVARLEVEL) then
        begin
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                TPVSystemObj(DERelem).VWmode := TRUE;
                TPVSystemObj(DERelem).Varmode := VARMODEKVAR;
                TPVSystemObj(DERelem).VVmode := TRUE;
            end
            else
            begin
                TStorageObj(DERelem).VWmode := TRUE;
                TStorageObj(DERelem).Varmode := VARMODEKVAR;
                TStorageObj(DERelem).VVmode := TRUE;
            end;

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireVVpu[k] and QVWcurve_limitpu[k]
            CalcPVWcurve_limitpu(k);
            CalcQVVcurve_desiredpu(k);

            // LPF or RF activated
            if (RateofChangeMode = LPF) then
            begin
                CalcLPF(k, 'VARS', QDesireVVpu[k]);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu[k]);
                QDesireEndpu[k] := Min(abs(QDesireLimitedpu[k]), abs(QDesireOptionpu[k])) * sign(QDesireOptionpu[k]);

                CalcLPF(k, 'WATTS', PLimitVWpu[k]);
                // Checks kVA (var priority) and pctPmpp limits
                Check_Plimits(k, PLimitOptionpu[k]);
                PLimitEndpu[k] := Min(PLimitLimitedpu[k], PLimitOptionpu[k]);
            end
            else
            if (RateofChangeMode = RISEFALL) then
            begin
                CalcRF(k, 'VARS', QDesireVVpu[k]);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu[k]);
                QDesireEndpu[k] := Min(abs(QDesireLimitedpu[k]), abs(QDesireOptionpu[k])) * sign(QDesireOptionpu[k]);

                CalcRF(k, 'WATTS', PLimitVWpu[k]);
                // Checks kVA (var priority) and pctPmpp limits
                Check_Plimits(k, PLimitOptionpu[k]);
                PLimitEndpu[k] := Min(PLimitLimitedpu[k], PLimitOptionpu[k]);
            end
            else
            begin
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireVVpu[k]);
                QDesireEndpu[k] := Min(abs(QDesireVVpu[k]), abs(QDesireLimitedpu[k])) * sign(QDesireVVpu[k]);

                // Checks kVA (var priority) and pctPmpp limits
                Check_Plimits(k, PLimitVWpu[k]);
                PLimitEndpu[k] := Min(abs(PLimitLimitedpu[k]), abs(PLimitVWpu[k])) * sign(PLimitVWpu[k]);
            end;

            // Calculates PLimitVW[k] and QDesiredVV[k] through the convergence algorithm
            CalcVoltWatt_watts(k);
            CalcVoltVar_vars(k);

            //--------------------------------------------- end main process ---------------------------------------------//

            // Sets DER kvar_out and kW_out
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                TPVSystemObj(DERelem).Presentkvar := QDesiredVV[k];
                TPVSystemObj(DERelem).presentkW := PLimitVW[k];
            end
            else
            begin
                TStorageObj(DERelem).kvarRequested := QDesiredVV[k];
                TStorageObj(DERelem).kWRequested := PLimitVW[k];
            end;

            // Uptates PresentkW and Presentkvar considering watt and var priorities
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                TPVSystemObj(DERelem).SetNominalPVSystemOuput();

                if QDesiredVV[k] >= 0.0 then
                    Qoutputpu[k] := TPVSystemObj(DERelem).Presentkvar / QHeadroom[k]
                else
                    Qoutputpu[k] := TPVSystemObj(DERelem).Presentkvar / QHeadroomNeg[k];
            end
            else
            begin
                TStorageObj(DERelem).SetNominalStorageOutput();

                if QDesiredVV[k] >= 0.0 then
                    Qoutputpu[k] := TStorageObj(DERelem).Presentkvar / QHeadroom[k]
                else
                    Qoutputpu[k] := TStorageObj(DERelem).Presentkvar / QHeadroomNeg[k];
            end;

            // Values used in convergence
            QoutputVVpu[k] := Qoutputpu[k];
            FAvgpVpuPrior[k] := FPresentVpu[k];
            POldVWpu[k] := PLimitVW[k] / PBase[k];

            // Values used in CalcQVVcurve_desiredpu
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                QOld[k] := TPVSystemObj(DERelem).Presentkvar;
                QOldVV[k] := TPVSystemObj(DERelem).Presentkvar;

                if ShowEventLog then
                    AppendtoEventLog('InvControl.' + Self.Name + ', ' + TPVSystemObj(DERelem).FullName,
                        Format('**VV_VW mode requested PVSystem output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.',
                        [QDesiredVV[k], TPVSystemObj(DERelem).presentkvar]));
            end
            else
            begin
                QOld[k] := TStorageObj(DERelem).Presentkvar;
                QOldVV[k] := TStorageObj(DERelem).Presentkvar;

                if ShowEventLog then
                    AppendtoEventLog('InvControl.' + Self.Name + ', ' + TStorageObj(DERelem).FullName,
                        Format('**VV_VW mode requested Storage output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.',
                        [QDesiredVV[k], TStorageObj(DERelem).presentkvar]));
            end;

            // Flag has to do set to 0 when kW_out is lower than Ptemp (max power allowed from volt-watt function)
            if ControlledElement[k].DSSClassName = 'PVSystem' then
            begin
                if abs(TPVSystemObj(DERelem).presentkW - PLimitVW[k]) / PLimitVW[k] > 0.0001 then
                    FVWOperation[k] := 0; // 0.01% is the value chosen at the moment

                if ShowEventLog then
                    AppendtoEventLog('InvControl.' + Self.Name + ', ' + TPVSystemObj(DERelem).FullName,
                        Format('**VV_VW mode set PVSystem kw output limit to **, kw= %.5g. Actual output is kw= %.5g.',
                        [PLimitVW[k], TPVSystemObj(DERelem).presentkW]));
            end
            else
            begin
                if abs(abs(TStorageObj(DERelem).presentkW) - PLimitVW[k]) / PLimitVW[k] > 0.0001 then
                    FVWOperation[k] := 0; // 0.01% is the value chosen at the moment

                if ShowEventLog then
                    AppendtoEventLog('InvControl.' + Self.Name + ', ' + TStorageObj(DERelem).FullName,
                        Format('**VV_VW mode set Storage kw output limit to** kw= %.5g. Actual output is kw= %.5g.',
                        [PLimitVW[k], TStorageObj(DERelem).presentkW]));
            end;

        end;

        ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
        Set_PendingChange(NONE, k);
        DERelem := NIL;
    end;
end;

procedure TInvControl2Obj.GetmonVoltage(var Vpresent: Double; i: Integer; BasekV: Double);
var
    j: Integer;
    rBus: TDSSBus;
    numNodes: Integer;
    vi: Complex;
    vj: Complex;
begin
    if FUsingMonBuses then
    begin
        for j := 0 to Length(FMonBuses) - 1 do
        begin
            FMonBusesIndex := ActiveCircuit.BusList.Find(FMonBuses[j]);
            rBus := ActiveCircuit.Buses^[FMonBusesIndex];

            if (length(FMonBusesNodes[j]) = 2) then
            begin
                vi := (ActiveCircuit.Solution.NodeV^[rBus.GetRef(FMonBusesNodes[j][0])]);
                vj := (ActiveCircuit.Solution.NodeV^[rBus.GetRef(FMonBusesNodes[j][1])]);
                cBuffer[i, j] := (vi - vj) * (BasekV * 1000.0 / FMonBusesVbase[j + 1]); // TODO: NIL check?
            end
            else
            begin
                cBuffer[i, j] := ActiveCircuit.Solution.NodeV^[rBus.GetRef(FMonBusesNodes[j][0])] * (BasekV * 1000.0 / FMonBusesVbase[j + 1]);
            end;
        end;

        case FMonBusesPhase of
            AVGPHASES:
            begin
                Vpresent := 0.0;
                for j := 0 to Length(FMonBuses) - 1 do
                    Vpresent := Vpresent + Cabs(cBuffer[i, j]);
                Vpresent := Vpresent / Length(FMonBuses);
            end;
            MAXPHASE:
            begin
                Vpresent := 0.0;
                for j := 0 to Length(FMonBuses) - 1 do
                    Vpresent := Max(Vpresent, Cabs(cBuffer[i, j]));
            end;
            MINPHASE:
            begin
                Vpresent := 1.0E50;
                for j := 0 to Length(FMonBuses) - 1 do
                    Vpresent := Min(Vpresent, Cabs(cBuffer[i, j]));
            end;
        else
            Vpresent := Cabs(cBuffer[i, FMonBusesPhase]);
        end;
    end
    else
    begin
        ControlledElement[i].ComputeVTerminal();

        numNodes := ControlledElement[i].NPhases;

        for j := 1 to numNodes do
            cBuffer[i, j] := ControlledElement[i].Vterminal^[j];


        case FMonBusesPhase of
            AVGPHASES:
            begin
                Vpresent := 0.0;
                for j := 1 to numNodes do
                    Vpresent := Vpresent + Cabs(cBuffer[i, j]);
                Vpresent := Vpresent / numNodes;
            end;
            MAXPHASE:
            begin
                Vpresent := 0.0;
                for j := 1 to numNodes do
                    Vpresent := Max(Vpresent, Cabs(cBuffer[i, j]));
            end;
            MINPHASE:
            begin
                Vpresent := 1.0E50;
                for j := 1 to numNodes do
                    Vpresent := Min(Vpresent, Cabs(cBuffer[i, j]));
            end;
        else
            Vpresent := Cabs(cBuffer[i, FMonBusesPhase]);
        end;
    end;
end;

procedure TInvControl2Obj.UpdateDERParameters(i: Integer);
begin
    with ControlledElement[i] do
        if ControlledElement[i].DSSClassName = 'PVSystem' then
        begin
            with TPVSystemObj(ControlledElement[i]) do
            begin
                CondOffset[i] := (NTerms - 1) * NCondsDER[i]; // for speedy sampling

                FVBase[i] := Vbase;
                FVarFollowInverter[i] := VarFollowInverter;
                FInverterON[i] := InverterON;
                FpresentkW[i] := PresentkW;
                FkVARating[i] := kVARating;
                Fpresentkvar[i] := Presentkvar;
                FkvarLimit[i] := kvarLimit;
                FkvarLimitNeg[i] := kvarLimitNeg;
                FCurrentkvarLimit[i] := CurrentkvarLimit;
                FCurrentkvarLimitNeg[i] := CurrentkvarLimitNeg;
                FDCkWRated[i] := Pmpp;
                FpctDCkWRated[i] := puPmpp;
                FEffFactor[i] := PVSystemVars.EffFactor;
                FDCkW[i] := PVSystemVars.PanelkW;
                FPPriority[i] := PVSystemVars.P_Priority;

            end;
        end
        else
        if ControlledElement[i].DSSClassName = 'Storage' then
        begin
            with TStorageObj(ControlledElement[i]) do
            begin
                FVBase[i] := Vbase;
                FVarFollowInverter[i] := VarFollowInverter;
                FInverterON[i] := InverterON;
                FpresentkW[i] := PresentkW;
                FkVARating[i] := kVARating;
                Fpresentkvar[i] := Presentkvar;
                FkvarLimit[i] := kvarLimit;
                FkvarLimitNeg[i] := kvarLimitNeg;
                FCurrentkvarLimit[i] := CurrentkvarLimit;
                FCurrentkvarLimitNeg[i] := CurrentkvarLimitNeg;
                FDCkWRated[i] := StorageVars.kWrating;
                FpctDCkWRated[i] := StorageVars.pctkWrated;
                FEffFactor[i] := Storagevars.EffFactor;
                FDCkW[i] := 0.0; // not using it (using TStorageObj.DCkW directly)
                FPPriority[i] := StorageVars.P_priority;
            end
        end;
end;

procedure TInvControl2Obj.Sample();
var
    i: Integer;
    basekV: Double;
    Vpresent: Double;
    PVSys: TPVSystemObj = NIL;
    Storage: TStorageObj = NIL;
begin
    // if list is not defined, go make one from all PVSystem/Storage in circuit
    if FDERPointerList.Count = 0 then
        RecalcElementData();

    if (FListSize > 0) then
    begin
        // if an InvControl2 controls more than one PVSystem/Storage, control each one
        // separately based on the PVSystem/Storage's terminal voltages, etc.
        for i := 1 to FDERPointerList.Count do
        begin
            UpdateDERParameters(i);

            if ControlledElement[i].DSSClassName = 'PVSystem' then
                PVSys := ControlledElement[i] as TPVSystemObj
            else
                Storage := ControlledElement[i] as TStorageObj;

            BasekV := FVBase[i] / 1000.0; // It's a line-to-ground voltage

            GetmonVoltage(Vpresent, i, BasekV);

            // for reporting Vpriorpu correctly in EventLog (this update is normally perform at DoPendingAction)
            if ActiveCircuit.Solution.ControlIteration = 1 then
            begin
                FAvgpVpuPrior[i] := FPresentVpu[i];
                FAvgpDRCVpuPrior[i] := FPresentDRCVpu[i];
            end;

            kW_out_desired[i] := FpresentkW[i]; // necessary to update kW_out_desired at every control iteration for Storage with SC

            // Help says that it must be used just for vv and vw
            // convert to per-unit on bus' kvbase, or
            // if using averaging window values, then set prior voltage to averaging window
            if (FVoltage_CurveX_ref = 1) and (FRollAvgWindow[i].AvgVal <> 0.0) then
                FPresentVpu[i] := Vpresent / (FRollAvgWindow[i].AvgVal)
            else
            if (FVoltage_CurveX_ref = 2) and (FRollAvgWindow[i].AvgVal <> 0.0) then
                FPresentVpu[i] := (FRollAvgWindow[i].AvgVal) / (basekV * 1000.0)
            else
                FPresentVpu[i] := Vpresent / (BasekV * 1000.0);

            FPresentDRCVpu[i] := Vpresent / (BasekV * 1000.0);

            // Sets internal variables of controlled element.
            // FVreg is the pu voltage used in the volt-var and volt-watt curves
            FVreg := FPresentVpu[i];

            if CombiMode = VV_DRC then
            begin
                  // Sets internal variables of controlled element.
                  // FVVDRCOperation is a flag which indicates if VVDRC function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)
                if ControlledElement[i].DSSClassName = 'PVSystem' then
                begin
                    PVSys.Set_Variable(5, FVreg);
                    PVSys.Set_Variable(6, FDRCRollAvgWindow[i].AvgVal / (basekV * 1000.0)); // save rolling average voltage in monitor
                    PVSys.Set_Variable(10, FVVDRCOperation[i]);
                end
                else
                begin
                    Storage.Set_Variable(14, FVreg);
                    Storage.Set_Variable(15, FDRCRollAvgWindow[i].AvgVal / (basekV * 1000.0)); // save rolling average voltage in monitor
                    Storage.Set_Variable(19, FVVDRCOperation[i]);
                end;

                  // if inverter is off then exit
                if (FInverterON[i] = FALSE) and (FVarFollowInverter[i] = TRUE) then
                    continue;

                  // if the volt-var curve does not exist, exit
                if Fvvc_curve = NIL then
                begin
                    DoSimpleMsg(_('XY Curve object representing vvc1_curve does not exist or is not tied to InvControl.'), 382);
                    exit
                end;

                if (ControlledElement[i].DSSClassName = 'PVSystem') then
                begin
                    PVSys.VVmode := TRUE;
                    PVSys.DRCmode := TRUE;
                end
                else
                begin
                    Storage.VVmode := TRUE;
                    Storage.DRCmode := TRUE;
                end;

                  //DRC triggers
                if (priorDRCRollAvgWindow[i] = 0.0) then
                begin
                    if (Abs(FPresentDRCVpu[i] - FAvgpDRCVpuPrior[i]) > FVoltageChangeTolerance) or
                        (Abs(FPresentVpu[i] - FAvgpVpuPrior[i]) > FVoltageChangeTolerance) then
                    begin
                          // Resets DER state variable only if it has not converged yet
                        FVVDRCOperation[i] := 0.0;

                        Set_PendingChange(CHANGEDRCVVARLEVEL, i);

                        with ActiveCircuit.Solution.DynaVars do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                                (intHour, t + TimeDelay, PendingChange[i], 0, Self);

                        if ShowEventLog then
                            AppendtoEventLog('InvControl.' + Self.Name + ', ' + ControlledElement[i].FullName,
                                Format(_('**Ready to change var output due to DRC trigger in VV_DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g'),
                                [FPresentDRCVpu[i], FAvgpDRCVpuPrior[i]]));
                    end;

                end;

                    //Trigger from volt-var mode
                if (((Abs(FPresentVpu[i] - FAvgpVpuPrior[i]) > FVoltageChangeTolerance) or
                    (Abs(FPresentDRCVpu[i] - FAvgpDRCVpuPrior[i]) > FVoltageChangeTolerance) or
                    ((Abs(Abs(QoutputVVDRCpu[i]) - Abs(QDesireEndpu[i])) > FVarChangeTolerance))) or
                    (ActiveCircuit.Solution.ControlIteration = 1)) then
                begin
                      // Resets DER state variable only if it has not converged yet
                    FVVDRCOperation[i] := 0.0;

                    Set_PendingChange(CHANGEDRCVVARLEVEL, i);
                    with  ActiveCircuit.Solution.DynaVars do
                        ControlActionHandle := ActiveCircuit.ControlQueue.Push
                            (intHour, t + TimeDelay, PendingChange[i], 0, Self);

                    if ShowEventLog then
                        AppendtoEventLog('InvControl.' + Self.Name + ', ' + ControlledElement[i].FullName,
                            Format(_('**Ready to change VV_DRC output due to volt-var trigger in VV_DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g'),
                            [FPresentVpu[i], FAvgpVpuPrior[i]]));

                end;
            end

            else
            if CombiMode = VV_VW then
            begin
                // Sets internal variables of controlled element.
                // FVVOperation is a flag which indicates if volt-var function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)
                // FVWOperation is a flag which indicates if volt-watt function operates or not
                // Combined modes operation is shown through TWO flags. It allows us to verify which of the individual function operates or not

                if ControlledElement[i].DSSClassName = 'PVSystem' then
                begin
                    PVSys.Set_Variable(5, FVreg);
                    PVSys.Set_Variable(7, FVVOperation[i]);
                    PVSys.Set_Variable(8, FVWOperation[i]);
                end
                else
                begin
                    Storage.Set_Variable(14, FVreg);
                    Storage.Set_Variable(16, FVVOperation[i]);
                    Storage.Set_Variable(17, FVWOperation[i]);
                end;

                // if inverter is off then exit
                if (FInverterON[i] = FALSE) and (FVarFollowInverter[i] = TRUE) then
                    continue;

                // if volt-watt curve does not exist, exit
                if ControlledElement[i].DSSClassName = 'PVSystem' then
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

                if (ControlledElement[i].DSSClassName = 'PVSystem') then
                begin
                    PVSys.VVmode := TRUE;
                    PVSys.VWmode := TRUE
                end
                else
                begin
                    Storage.VVmode := TRUE;
                    Storage.VWmode := TRUE;
                end;

                // Trigger from volt-watt mode
                if ((Abs(FPresentVpu[i] - FAvgpVpuPrior[i]) > FVoltageChangeTolerance) or (Abs(PLimitEndpu[i] - POldVWpu[i]) > FActivePChangeTolerance) or
                    (ActiveCircuit.Solution.ControlIteration = 1)) then

                begin
                    // Resets DER state variable only if it has not converged yet
                    FVWOperation[i] := 0;

                    Set_PendingChange(CHANGEWATTVARLEVEL, i);

                    with  ActiveCircuit.Solution.DynaVars do
                        ControlActionHandle := ActiveCircuit.ControlQueue.Push
                            (intHour, t + TimeDelay, PendingChange[i], 0, Self);

                    if ShowEventLog then
                        AppendtoEventLog('InvControl.' + Self.Name + ', ' + ControlledElement[i].FullName,
                            Format('**Ready to change VV_VW output due to volt-watt trigger**, Vavgpu= %.5g, VPriorpu=%.5g',
                            [FPresentVpu[i], FAvgpVpuPrior[i]]));
                    ;
                end;

                  //Trigger from volt-var mode
                if (((Abs(FPresentVpu[i] - FAvgpVpuPrior[i]) > FVoltageChangeTolerance) or
                    ((Abs(Abs(Qoutputpu[i]) - Abs(QDesireEndpu[i])) > FVarChangeTolerance))) or
                    (ActiveCircuit.Solution.ControlIteration = 1)) then

                begin
                    // Resets DER state variable only if it has not converged yet
                    FVVOperation[i] := 0;
                    Set_PendingChange(CHANGEWATTVARLEVEL, i);
                    with  ActiveCircuit.Solution.DynaVars do
                        ControlActionHandle := ActiveCircuit.ControlQueue.Push
                            (intHour, t + TimeDelay, PendingChange[i], 0, Self);

                    if ShowEventLog then
                        AppendtoEventLog('InvControl.' + Self.Name + ', ' + ControlledElement[i].FullName,
                            Format('**Ready to change VV_VW output due to volt-var trigger**, Vavgpu= %.5g, VPriorpu=%.5g',
                            [FPresentVpu[i], FAvgpVpuPrior[i]]));
                end;
            end

            else
            if ControlMode = VOLTWATT then  // volt-watt control mode
            begin
                // Sets internal variables of controlled element.
                // FVWOperation is a flag which indicates if volt-watt function operates or not

                if ControlledElement[i].DSSClassName = 'PVSystem' then
                begin
                    PVSys.Set_Variable(5, FVreg);
                    PVSys.Set_Variable(8, FVWOperation[i]);
                end
                else
                begin
                    Storage.Set_Variable(14, FVreg);
                    Storage.Set_Variable(17, FVWOperation[i]);
                end;

                if (FInverterON[i] = FALSE) then
                    continue;

                if ControlledElement[i].DSSClassName = 'PVSystem' then
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

                if (ControlledElement[i].DSSClassName = 'PVSystem') then
                    PVSys.VWmode := TRUE
                else
                    Storage.VWmode := TRUE;

                if ((Abs(FPresentVpu[i] - FAvgpVpuPrior[i]) > FVoltageChangeTolerance) or (Abs(PLimitEndpu[i] - POldVWpu[i]) > FActivePChangeTolerance) or
                    (ActiveCircuit.Solution.ControlIteration = 1)) then
                begin
                    // Resets DER state variable only if it has not converged yet
                    FVWOperation[i] := 0;

                    Set_PendingChange(CHANGEWATTLEVEL, i);

                    with  ActiveCircuit.Solution.DynaVars do
                        ControlActionHandle := ActiveCircuit.ControlQueue.Push
                            (intHour, t + TimeDelay, PendingChange[i], 0, Self);
                    if ShowEventLog then
                        AppendtoEventLog('InvControl.' + Self.Name + ', ' + ControlledElement[i].FullName,
                            Format('**Ready to limit watt output due to VOLTWATT mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                            [FPresentVpu[i], FAvgpVpuPrior[i]]));
                end;
            end

            else
            if ControlMode = AVR then // Active voltage regulation control mode
            begin
                // Sets internal variables of PVSystem/Storage.
                // FAVROperation is a flag which indicates if volt-var function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)


                // if inverter is off then exit
                if (FInverterON[i] = FALSE) and (FVarFollowInverter[i] = TRUE) then
                    continue;


                if (ControlledElement[i].DSSClassName = 'PVSystem') then
                    PVSys.AVRmode := TRUE
                else
                    Storage.VVmode := TRUE;

                  //Trigger from AVR mode

                if (((Abs(FPresentVpu[i] - FAvgpVpuPrior[i]) > FVoltageChangeTolerance) or
                    ((Abs(Abs(QoutputAVRpu[i]) - Abs(QDesireEndpu[i])) > FVarChangeTolerance)) or
                    (Abs(FPresentVpu[i] - Fv_setpointLimited[i]) > FVoltageChangeTolerance)) or
                    (ActiveCircuit.Solution.ControlIteration = 1)) then

                begin
                    // Resets DER state variable only if it has not converged yet
                    FAVROperation[i] := 0;

                    Set_PendingChange(CHANGEVARLEVEL, i);

                    with  ActiveCircuit.Solution.DynaVars do
                        ControlActionHandle := ActiveCircuit.ControlQueue.Push(intHour, t + TimeDelay, PendingChange[i], 0, Self);

                    if ShowEventLog then
                        AppendtoEventLog('InvControl.' + Self.Name + ', ' + ControlledElement[i].FullName,
                            Format('**Ready to change var output due to AVR trigger in AVR mode**, Vavgpu= %.5g, VPriorpu=%.5g, Vsetpoint=%.5g, VsetpointLimited=%.5g',
                            [FPresentVpu[i], FAvgpVpuPrior[i], Fv_setpoint, Fv_setpointLimited[i]]));
                end;
            end

            else
            if ControlMode = VOLTVAR then // volt-var control mode
            begin
                // Sets internal variables of PVSystem/Storage.
                // FVVOperation is a flag which indicates if volt-var function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)

                if ControlledElement[i].DSSClassName = 'PVSystem' then
                begin
                    PVSys.Set_Variable(5, FVreg);
                    PVSys.Set_Variable(7, FVVOperation[i]);
                end
                else
                begin
                    Storage.Set_Variable(14, FVreg);
                    Storage.Set_Variable(16, FVVOperation[i]);
                end;

                // if inverter is off then exit
                if (FInverterON[i] = FALSE) and (FVarFollowInverter[i] = TRUE) then
                    continue;

                if Fvvc_curve = NIL then
                begin
                    DoSimpleMsg(_('XY Curve object representing vvc1_curve does not exist or is not tied to InvControl.'), 382);
                    exit
                end;

                if (ControlledElement[i].DSSClassName = 'PVSystem') then
                    PVSys.VVmode := TRUE
                else
                    Storage.VVmode := TRUE;

                  //Trigger from volt-var mode
                if (((Abs(FPresentVpu[i] - FAvgpVpuPrior[i]) > FVoltageChangeTolerance) or
                    ((Abs(Abs(QoutputVVpu[i]) - Abs(QDesireEndpu[i])) > FVarChangeTolerance))) or
                    (ActiveCircuit.Solution.ControlIteration = 1)) then

                begin
                    // Resets DER state variable only if it has not converged yet
                    FVVOperation[i] := 0;

                    Set_PendingChange(CHANGEVARLEVEL, i);

                    with  ActiveCircuit.Solution.DynaVars do
                        ControlActionHandle := ActiveCircuit.ControlQueue.Push(intHour, t + TimeDelay, PendingChange[i], 0, Self);

                    if ShowEventLog then
                        AppendtoEventLog('InvControl.' + Self.Name + ', ' + ControlledElement[i].FullName,
                            Format('**Ready to change var output due to volt-var trigger in volt-var mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                            [FPresentVpu[i], FAvgpVpuPrior[i]]));
                end;
            end

            else
            if ControlMode = WATTPF then // watt-pf control mode
            begin
                // Sets internal variables of PVSystem/Storage.
                // FWPOperation is a flag which indicates if watt-pf function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)

                if ControlledElement[i].DSSClassName = 'PVSystem' then
                begin
                    PVSys.Set_Variable(5, FVreg);
                    PVSys.Set_Variable(11, FWPOperation[i]);
                end
                else
                begin
                    Storage.Set_Variable(14, FVreg);
                    Storage.Set_Variable(16, FWPOperation[i]);
                end;

                // if inverter is off then exit
                if (FInverterON[i] = FALSE) and (FVarFollowInverter[i] = TRUE) then
                    continue;

                if Fwattpf_curve = NIL then
                begin
                    DoSimpleMsg(_('XY Curve object representing wattpf_curve does not exist or is not tied to InvControl.'), 382);
                    exit
                end;

                if (ControlledElement[i].DSSClassName = 'PVSystem') then
                    PVSys.WPmode := TRUE
                else
                    Storage.WPmode := TRUE;

                  //Trigger from volt-var mode
                if (((Abs(FPresentVpu[i] - FAvgpVpuPrior[i]) > FVoltageChangeTolerance) or
                    ((Abs(Abs(QoutputVVpu[i]) - Abs(QDesireEndpu[i])) > FVarChangeTolerance))) or
                    (ActiveCircuit.Solution.ControlIteration = 1)) then

                begin
                    // Resets DER state variable only if it has not converged yet
                    FWPOperation[i] := 0;

                    Set_PendingChange(CHANGEVARLEVEL, i);

                    with  ActiveCircuit.Solution.DynaVars do
                        ControlActionHandle := ActiveCircuit.ControlQueue.Push(intHour, t + TimeDelay, PendingChange[i], 0, Self);

                    if ShowEventLog then
                        AppendtoEventLog('InvControl.' + Self.Name + ', ' + ControlledElement[i].FullName,
                            Format('**Ready to change var output due to watt-pf trigger in watt-pf mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                            [FPresentVpu[i], FAvgpVpuPrior[i]]));
                end;
            end

            else
            if ControlMode = WATTVAR then // watt-var control mode
            begin
                // Sets internal variables of PVSystem/Storage.
                // FWVOperation is a flag which indicates if watt-var function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)

                if ControlledElement[i].DSSClassName = 'PVSystem' then
                begin
                    PVSys.Set_Variable(5, FVreg);
                    PVSys.Set_Variable(12, FWVOperation[i]);        //CHANGE HERE
                end
                else
                begin
                    Storage.Set_Variable(14, FVreg);
                    Storage.Set_Variable(16, FWVOperation[i]);
                end;

                // if inverter is off then exit
                if (FInverterON[i] = FALSE) and (FVarFollowInverter[i] = TRUE) then
                    continue;

                if Fwattvar_curve = NIL then
                begin
                    DoSimpleMsg(_('XY Curve object representing wattvar_curve does not exist or is not tied to InvControl.'), 382);
                    exit
                end;

                if (ControlledElement[i].DSSClassName = 'PVSystem') then
                    PVSys.WVmode := TRUE
                else
                    Storage.WVmode := TRUE;

                  //Trigger from volt-var mode
                if (((Abs(FPresentVpu[i] - FAvgpVpuPrior[i]) > FVoltageChangeTolerance) or
                    ((Abs(Abs(QoutputVVpu[i]) - Abs(QDesireEndpu[i])) > FVarChangeTolerance))) or
                    (ActiveCircuit.Solution.ControlIteration = 1)) then

                begin
                    // Resets DER state variable only if it has not converged yet
                    FWVOperation[i] := 0;

                    Set_PendingChange(CHANGEVARLEVEL, i);

                    with  ActiveCircuit.Solution.DynaVars do
                        ControlActionHandle := ActiveCircuit.ControlQueue.Push(intHour, t + TimeDelay, PendingChange[i], 0, Self);

                    if ShowEventLog then
                        AppendtoEventLog('InvControl.' + Self.Name + ', ' + ControlledElement[i].FullName,
                            Format('**Ready to change var output due to watt-var trigger in watt-var mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                            [FPresentVpu[i], FAvgpVpuPrior[i]]));
                end;
            end

            else
            if ControlMode = DRC then // dynamic reactive current control mode
            begin
                // Sets internal variables of PVSystem/Storage.
                // FDRCOperation is a flag which indicates if DRC function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)

                if ControlledElement[i].DSSClassName = 'PVSystem' then
                begin
                    PVSys.Set_Variable(5, FVreg);
                    PVSys.Set_Variable(6, FDRCRollAvgWindow[i].AvgVal / (basekV * 1000.0)); // save rolling average voltage in monitor
                    PVSys.Set_Variable(9, FDRCOperation[i]);
                end
                else
                begin
                    Storage.Set_Variable(14, FVreg);
                    Storage.Set_Variable(15, FDRCRollAvgWindow[i].AvgVal / (basekV * 1000.0)); // save rolling average voltage in monitor
                    Storage.Set_Variable(18, FDRCOperation[i]);
                end;

                // if inverter is off then exit
                if (FInverterON[i] = FALSE) and (FVarFollowInverter[i] = TRUE) then
                    continue;

                //DRC triggers
                if (priorDRCRollAvgWindow[i] = 0.0) then
                begin
                    if ((Abs(FPresentDRCVpu[i] - FAvgpDRCVpuPrior[i]) > FVoltageChangeTolerance)) then
                    begin
                        // Resets DER state variable only if it has not converged yet
                        FDRCOperation[i] := 0;


                        Set_PendingChange(CHANGEVARLEVEL, i);

                        with ActiveCircuit.Solution.DynaVars do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                                (intHour, t + TimeDelay, PendingChange[i], 0, Self);

                        if ShowEventLog then
                            AppendtoEventLog('InvControl.' + Self.Name + ', ' + ControlledElement[i].FullName,
                                Format('**Ready to change var output due to DRC trigger in DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                                [FPresentDRCVpu[i], FAvgpDRCVpuPrior[i]]));
                    end;
                end;

                if (ControlledElement[i].DSSClassName = 'PVSystem') then
                    PVSys.DRCmode := TRUE
                else
                    Storage.DRCmode := TRUE;

                if ((Abs(FPresentDRCVpu[i] - FAvgpDRCVpuPrior[i]) > FVoltageChangeTolerance) or
                    (Abs(Abs(QoutputDRCpu[i]) - Abs(QDesireEndpu[i])) > FVarChangeTolerance) or // TEMc; also tried checking against QDesireEndpu
                    (ActiveCircuit.Solution.ControlIteration = 1)) then
                begin
                    Set_PendingChange(CHANGEVARLEVEL, i);
                    with  ActiveCircuit.Solution.DynaVars do
                        ControlActionHandle := ActiveCircuit.ControlQueue.Push
                            (intHour, t + TimeDelay, PendingChange[i], 0, Self);

                    if ShowEventLog then
                        AppendtoEventLog('InvControl.' + Self.Name + ', ' + ControlledElement[i].FullName,
                            Format('**Ready to change var output due to DRC trigger in DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g, QoutPU=%.3g, QDesiredEndpu=%.3g',
                            [FPresentDRCVpu[i], FAvgpDRCVpuPrior[i], QoutputDRCpu[i], QDesireEndpu[i]]));

                end;
            end;
        end;
    end;
end;

function TInvControl2Obj.MakeDERList: Boolean;
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

        SetLength(CondOffset, FListSize + 1);
        SetLength(cBuffer, FListSize + 1, 7);  // assuming no more than 6 conductors
        SetLength(ControlledElement, FListSize + 1);  // Use this as the main pointer to PVSystem and Storage Elements
        SetLength(FAvgpVpuPrior, FListSize + 1);
        SetLength(FAvgpDRCVpuPrior, FListSize + 1);
        SetLength(FPresentVpu, FListSize + 1);
        SetLength(FPresentDRCVpu, FListSize + 1);
        SetLength(NPhasesDER, FListSize + 1);
        SetLength(NCondsDER, FListSize + 1);
        SetLength(FPendingChange, FListSize + 1);
        SetLength(QDesiredVV, FListSize + 1);
        SetLength(QDesiredWP, FListSize + 1);
        SetLength(QDesiredWV, FListSize + 1);
        SetLength(QDesiredAVR, FListSize + 1);
        SetLength(QOld, FListSize + 1);
        SetLength(QOldVV, FListSize + 1);
        SetLength(QOldAVR, FListSize + 1);
        SetLength(QOldDRC, FListSize + 1);
        SetLength(QOldVVDRC, FListSize + 1);
        SetLength(QDesiredDRC, FListSize + 1);
        SetLength(QDesiredVVDRC, FListSize + 1);
        SetLength(QHeadroom, FListSize + 1);
        SetLength(QHeadroomNeg, FListSize + 1);
        SetLength(PBase, FListSize + 1);
        SetLength(Qoutputpu, FListSize + 1);
        SetLength(QoutputVVpu, FListSize + 1);
        SetLength(QoutputAVRpu, FListSize + 1);
        SetLength(QoutputDRCpu, FListSize + 1);
        SetLength(QoutputVVDRCpu, FListSize + 1);
        SetLength(QDesireEndpu, FListSize + 1);
        SetLength(QDesireVVpu, FListSize + 1);
        SetLength(QDesireWPpu, FListSize + 1);
        SetLength(QDesireWVpu, FListSize + 1);
        SetLength(QDesireAVRpu, FListSize + 1);
        SetLength(QDesireLimitedpu, FListSize + 1);
        SetLength(QDesireOptionpu, FListSize + 1);
        SetLength(PLimitEndpu, FListSize + 1);
        SetLength(PLimitVWpu, FListSize + 1);
        SetLength(PLimitLimitedpu, FListSize + 1);
        SetLength(PLimitOptionpu, FListSize + 1);
        SetLength(QDesireDRCpu, FListSize + 1);
        SetLength(deltaVDynReac, FListSize + 1);
        SetLength(PLimitVW, FListSize + 1);
        SetLength(POldVWpu, FListSize + 1);
        SetLength(FdeltaQFactor, FListSize + 1);
        SetLength(FdeltaPFactor, FListSize + 1);
        SetLength(DeltaV_old, FListSize + 1);
        SetLength(FVpuSolution, FListSize + 1, 2 + 1);
        SetLength(FRollAvgWindow, FListSize + 1);
        SetLength(FDRCRollAvgWindow, FListSize + 1);
        SetLength(FDRCRollAvgWindowpu, FListSize + 1);
        SetLength(priorRollAvgWindow, FListSize + 1);
        SetLength(priorDRCRollAvgWindow, FListSize + 1);
        SetLength(FlagChangeCurve, FListSize + 1);
        SetLength(FActiveVVCurve, FListSize + 1);
        SetLength(FPriorWattspu, FListSize + 1);
        SetLength(FPriorWatts, FListSize + 1);
        SetLength(FPriorPLimitOptionpu, FListSize + 1);
        SetLength(FPriorQDesireOptionpu, FListSize + 1);
        SetLength(kW_out_desiredpu, FListSize + 1);
        SetLength(kW_out_desired, FListSize + 1);
        SetLength(FPriorvarspu, FListSize + 1);
        SetLength(FPriorvars, FListSize + 1);
        SetLength(FFlagVWOperates, FListSize + 1);
        SetLength(FVVOperation, FListSize + 1);
        SetLength(FAVROperation, FListSize + 1);
        SetLength(FWPOperation, FListSize + 1);
        SetLength(FWVOperation, FListSize + 1);
        SetLength(FVWOperation, FListSize + 1);
        SetLength(FDRCOperation, FListSize + 1);
        SetLength(FVVDRCOperation, FListSize + 1);
        SetLength(FVBase, FListSize + 1);
        SetLength(FVarFollowInverter, FListSize + 1);
        SetLength(FInverterON, FListSize + 1);
        SetLength(FpresentkW, FListSize + 1);
        SetLength(FkVARating, FListSize + 1);
        SetLength(Fpresentkvar, FListSize + 1);
        SetLength(FkvarLimit, FListSize + 1);
        SetLength(FkvarLimitNeg, FListSize + 1);
        SetLength(FCurrentkvarLimit, FListSize + 1);
        SetLength(FCurrentkvarLimitNeg, FListSize + 1);
        SetLength(FDCkWRated, FListSize + 1);
        SetLength(FpctDCkWRated, FListSize + 1);
        SetLength(FEffFactor, FListSize + 1);
        SetLength(FDCkW, FListSize + 1);
        SetLength(FPPriority, FListSize + 1);
        SetLength(DQDV, FListSize + 1);
        SetLength(Fv_setpointLimited, FListSize + 1);
        SetLength(FAvgpAVRVpuPrior, FListSize + 1);


        for i := 1 to FListSize do
        begin
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

    end
    else
    begin
        {Search through the entire circuit for enabled PVSystem and Storage objects and add them to the list}

        // Adding PVSystem elements
        for i := 1 to PVSysClass.ElementCount do
        begin
            PVSys := PVSysClass.ElementList.Get(i);
            if PVSys.Enabled then
                FDERPointerList.Add(PVSys);
            DERNameList.Add(PVSys.FullName);
        end;
        // Adding Storage elements
        for i := 1 to StorageClass.ElementCount do
        begin
            Storage := StorageClass.ElementList.Get(i);
            if Storage.Enabled then
                FDERPointerList.Add(Storage);
            DERNameList.Add(Storage.FullName);
        end;

        FListSize := FDERPointerList.Count;

        SetLength(ControlledElement, FListSize + 1);
        SetLength(FAvgpVpuPrior, FListSize + 1);
        SetLength(FAvgpDRCVpuPrior, FListSize + 1);
        SetLength(FPresentVpu, FListSize + 1);
        SetLength(FPresentDRCVpu, FListSize + 1);
        SetLength(NPhasesDER, FListSize + 1);
        SetLength(NCondsDER, FListSize + 1);
        SetLength(CondOffset, FListSize + 1);
        SetLength(cBuffer, FListSize + 1, 7);  // assuming no more than 6 conductors
        SetLength(FPendingChange, FListSize + 1);
        SetLength(QDesiredVV, FListSize + 1);
        SetLength(QDesiredWP, FListSize + 1);
        SetLength(QDesiredWV, FListSize + 1);
        SetLength(QDesiredAVR, FListSize + 1);
        SetLength(QOld, FListSize + 1);
        SetLength(QOldVV, FListSize + 1);
        SetLength(QOldAVR, FListSize + 1);
        SetLength(QOldDRC, FListSize + 1);
        SetLength(QOldVVDRC, FListSize + 1);
        SetLength(QDesiredDRC, FListSize + 1);
        SetLength(QDesiredVVDRC, FListSize + 1);
        SetLength(QHeadroom, FListSize + 1);
        SetLength(QHeadroomNeg, FListSize + 1);
        SetLength(PBase, FListSize + 1);
        SetLength(Qoutputpu, FListSize + 1);
        SetLength(QoutputVVpu, FListSize + 1);
        SetLength(QoutputAVRpu, FListSize + 1);
        SetLength(QoutputDRCpu, FListSize + 1);
        SetLength(QoutputVVDRCpu, FListSize + 1);
        SetLength(QDesireEndpu, FListSize + 1);
        SetLength(QDesireVVpu, FListSize + 1);
        SetLength(QDesireWPpu, FListSize + 1);
        SetLength(QDesireWVpu, FListSize + 1);
        SetLength(QDesireAVRpu, FListSize + 1);
        SetLength(QDesireLimitedpu, FListSize + 1);
        SetLength(QDesireOptionpu, FListSize + 1);
        SetLength(PLimitEndpu, FListSize + 1);
        SetLength(PLimitVWpu, FListSize + 1);
        SetLength(PLimitLimitedpu, FListSize + 1);
        SetLength(PLimitOptionpu, FListSize + 1);
        SetLength(QDesireDRCpu, FListSize + 1);
        SetLength(PLimitVW, FListSize + 1);
        SetLength(POldVWpu, FListSize + 1);
        SetLength(FdeltaQFactor, FListSize + 1);
        SetLength(FdeltaPFactor, FListSize + 1);
        SetLength(DeltaV_old, FListSize + 1);
        SetLength(FRollAvgWindow, FListSize + 1);
        SetLength(FDRCRollAvgWindow, FListSize + 1);
        SetLength(FDRCRollAvgWindowpu, FListSize + 1);
        SetLength(deltaVDynReac, FListSize + 1);
        SetLength(priorRollAvgWindow, FListSize + 1);
        SetLength(priorDRCRollAvgWindow, FListSize + 1);
        SetLength(FVpuSolution, FListSize + 1, 2 + 1);
        SetLength(FlagChangeCurve, FListSize + 1);
        SetLength(FActiveVVCurve, FListSize + 1);
        SetLength(FPriorWattspu, FListSize + 1);
        SetLength(FPriorWatts, FListSize + 1);
        SetLength(FPriorPLimitOptionpu, FListSize + 1);
        SetLength(FPriorQDesireOptionpu, FListSize + 1);
        SetLength(kW_out_desiredpu, FListSize + 1);
        SetLength(kW_out_desired, FListSize + 1);
        SetLength(FPriorvarspu, FListSize + 1);
        SetLength(FPriorvars, FListSize + 1);
        SetLength(FFlagVWOperates, FListSize + 1);
        SetLength(FVVOperation, FListSize + 1);
        SetLength(FAVROperation, FListSize + 1);
        SetLength(FWVOperation, FListSize + 1);
        SetLength(FWPOperation, FListSize + 1);
        SetLength(FVWOperation, FListSize + 1);
        SetLength(FDRCOperation, FListSize + 1);
        SetLength(FVVDRCOperation, FListSize + 1);
        SetLength(FVBase, FListSize + 1);
        SetLength(FVarFollowInverter, FListSize + 1);
        SetLength(FInverterON, FListSize + 1);
        SetLength(FpresentkW, FListSize + 1);
        SetLength(FkVARating, FListSize + 1);
        SetLength(Fpresentkvar, FListSize + 1);
        SetLength(FkvarLimit, FListSize + 1);
        SetLength(FkvarLimitNeg, FListSize + 1);
        SetLength(FCurrentkvarLimit, FListSize + 1);
        SetLength(FCurrentkvarLimitNeg, FListSize + 1);
        SetLength(FDCkWRated, FListSize + 1);
        SetLength(FpctDCkWRated, FListSize + 1);
        SetLength(FEffFactor, FListSize + 1);
        SetLength(FDCkW, FListSize + 1);
        SetLength(FPPriority, FListSize + 1);
        SetLength(DQDV, FListSize + 1);
        SetLength(Fv_setpointLimited, FListSize + 1);
        SetLength(FAvgpAVRVpuPrior, FListSize + 1);


    end;  {else}

    //Initialize arrays

    for i := 1 to FlistSize do
    begin
        if StripExtension(AnsiLowerCase(DERNameList.Strings[i - 1])) = 'pvsystem' then
        begin
            PVSys := PVSysClass.Find(StripClassName(DERNameList.Strings[i - 1]));
            if (PVSys <> NIL) then
                DERElem := TPCElement(PVSys)
        end
        else
        begin
            Storage := StorageClass.Find(StripClassName(DERNameList.Strings[i - 1]));
            if (Storage <> NIL) then
                DERElem := TPCElement(Storage)
        end;


        for j := 1 to 6 do
            cBuffer[i, j] := cZERO;

        Set_NTerms(DERElem.NTerms);

        CondOffset[i] := 0;
        NPhasesDER[i] := DERElem.NPhases;
        NCondsDER[i] := DERElem.NConds;
        FAvgpVpuPrior[i] := 0.0;
        FAvgpDRCVpuPrior[i] := 0.0;
        FPresentVpu[i] := 0.0;
        FPresentDRCVpu[i] := 0.0;
        QDesiredVV[i] := 0.0;
        QDesiredWP[i] := 0.0;
        QDesiredWV[i] := 0.0;
        QOld[i] := -1.0;
        QOldVV[i] := -1.0;
        if PVSys = nil then
            QOldAVR[i] := 0.0
        else
            QOldAVR[i] := -PVSys.kvarLimitNeg / 2.0;
        QOldDRC[i] := -1.0;
        QOldVVDRC[i] := -1.0;
        QDesiredDRC[i] := 0.0;
        QDesiredVVDRC[i] := 0.0;
        PLimitVW[i] := 0.0;
        POldVWpu[i] := 0.0;
        PBase[i] := 0.0;
        QHeadroom[i] := 0.0;
        QHeadroomNeg[i] := 0.0;
        Qoutputpu[i] := 0.0;
        QoutputVVpu[i] := 0.0;
        QoutputAVRpu[i] := 0.0;
        QoutputDRCpu[i] := 0.0;
        QoutputVVDRCpu[i] := 0.0;
        QDesireEndpu[i] := 0.0;
        QDesireVVpu[i] := 0.0;
        QDesireWPpu[i] := 0.0;
        QDesireWVpu[i] := 0.0;
        QDesireAVRpu[i] := 0.0;
        QDesireLimitedpu[i] := 0.0;
        QDesireOptionpu[i] := 0.0;
        PLimitVWpu[i] := 0.0;
        PLimitLimitedpu[i] := 0.0;
        PLimitEndpu[i] := 0.0;
        PLimitOptionpu[i] := 0.0;
        QDesireDRCpu[i] := 0.0;
        FRollAvgWindow[i] := TRollAvgWindow.Create();
        FDRCRollAvgWindow[i] := TRollAvgWindow.Create();

        FdeltaQFactor[i] := DELTAQDEFAULT;
        FdeltaPFactor[i] := DELTAPDEFAULT;
        DeltaV_old[i] := -1.0;

        deltaVDynReac[i] := 0.0;
        FlagChangeCurve[i] := FALSE;
        FActiveVVCurve[i] := 1;
        priorRollAvgWindow[i] := 0.0;
        priorDRCRollAvgWindow[i] := 0.0;
        FPriorWattspu[i] := 0.0;
        FPriorWatts[i] := 0.0;
        FPriorPLimitOptionpu[i] := 0.0;
        FPriorQDesireOptionpu[i] := 0.0;
        kW_out_desiredpu[i] := 0.0;
        kW_out_desired[i] := 0.0;
        FPriorvarspu[i] := 0.0;
        FPriorvars[i] := 0.0;

        FFlagVWOperates[i] := FALSE;

        FVVOperation[i] := 0.0;
        FVWOperation[i] := 0.0;
        FDRCOperation[i] := 0.0;
        FVVDRCOperation[i] := 0.0;
        FWPOperation[i] := 0.0;
        FWVOperation[i] := 0.0;
        FAVROperation[i] := 0.0;

        for j := 1 to 2 do
            FVpuSolution[i, j] := 0.0;

        FPendingChange[i] := NONE;

        FVbase[i] := 0.0;
        FVarFollowInverter[i] := FALSE;
        FInverterON[i] := TRUE;
        FpresentkW[i] := 0.0;
        FkVARating[i] := 0.0;
        Fpresentkvar[i] := 0.0;
        FkvarLimit[i] := 0.0;
        FkvarLimitNeg[i] := 0.0;
        FCurrentkvarLimit[i] := 0.0;
        FCurrentkvarLimitNeg[i] := 0.0;
        FDCkWRated[i] := 0.0;
        FpctDCkWRated[i] := 0.0;
        FEffFactor[i] := 0.0;
        FDCkW[i] := 0.0;
        FPPriority[i] := FALSE;
        DQDV[i] := 0.0;
        Fv_setpointLimited[i] := 0.0;
        FAvgpAVRVpuPrior[i] := 0.0;
    end; {for}

    // RecalcElementData(); -- MakeDERList is only called FROM RecalcElementData, no need to call it again.
    if FDERPointerList.Count > 0 then
        Result := TRUE;
end;

procedure TInvControl2Obj.Reset;
begin
    // inherited;
end;

procedure TInvControl2Obj.Set_PendingChange(Value: Integer; DevIndex: Integer);
begin
    FPendingChange[DevIndex] := Value;
    DblTraceParameter := Value;
end;

procedure TInvControl2Obj.UpdateInvControl(i: Integer);
var
    j, k: Integer;
    solnvoltage: Double;
    tempVbuffer: pComplexArray;
    PVSys: TPVSystemObj = NIL;
    Storage: TStorageObj = NIL;
    BasekV: Double;
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

        if ControlledElement[j].DSSClassName = 'PVSystem' then
            PVSys := ControlledElement[j] as TPVSystemObj
        else
            Storage := ControlledElement[j] as TStorageObj;

        BasekV := FVBase[i] / 1000.0;


        FPriorPLimitOptionpu[j] := PLimitOptionpu[j];
        FPriorQDesireOptionpu[j] := QDesireOptionpu[j];

          // Used to update the VW resquested kW
        if ControlledElement[j].DSSClassName = 'PVSystem' then
            PVSys.VWmode := FALSE
        else
            Storage.VWMode := FALSE;

        if ControlledElement[j].DSSClassName = 'PVSystem' then
            PVSys.VVmode := FALSE
        else
            Storage.VVMode := FALSE;

        if ControlledElement[j].DSSClassName = 'PVSystem' then
            PVSys.DRCmode := FALSE
        else
            Storage.DRCMode := FALSE;


        FFlagVWOperates[j] := FALSE;

          // Reset DQDV - We might not need it
        DQDV[j] := 0.0;


          // Reset the operation flags for the new time step
        FVVOperation[j] := 0;
        FVWOperation[j] := 0;
        FDRCOperation[j] := 0;
        FVVDRCOperation[j] := 0;
        FWPOperation[j] := 0;
        FWVOperation[j] := 0;
        FAVROperation[j] := 0;

          // Reinitialize convergence arrays.
          //FdeltaQFactor[j] := DELTAQDEFAULT;
        FdeltaPFactor[j] := DELTAPDEFAULT;

          // allocated enough memory to buffer to hold voltages and initialize to cZERO
        Reallocmem(tempVbuffer, Sizeof(tempVbuffer^[1]) * ControlledElement[j].NConds);
        for k := 1 to ControlledElement[j].NConds do
            tempVbuffer[k] := cZERO;

        priorRollAvgWindow[j] := FRollAvgWindow[j].AvgVal;
        priorDRCRollAvgWindow[j] := FDRCRollAvgWindow[j].AvgVal;

          // compute the present terminal voltage
        ControlledElement[j].ComputeVterminal();
          //PVSys.Set_Variable(5,FDRCRollAvgWindow[j].AvgVal); // save rolling average voltage in monitor

        solnvoltage := 0.0;

        GetmonVoltage(solnvoltage, j, BasekV);

        // add present power flow solution voltage to the rolling average window
        FRollAvgWindow[j].Add(solnvoltage, ActiveCircuit.Solution.DynaVars.h, FRollAvgWindowLength);
        FDRCRollAvgWindow[j].Add(solnvoltage, ActiveCircuit.Solution.DynaVars.h, FDRCRollAvgWindowLength);

        FVpuSolution[j, FVpuSolutionIdx] := solnvoltage / ((ActiveCircuit.Buses^[ControlledElement[j].terminals[0].busRef].kVBase) * 1000.0);

        Reallocmem(tempVbuffer, 0);   // Clean up memory

    end;
end;

function TInvControl2Obj.Get_PendingChange(DevIndex: Integer): Integer;
begin
    Result := FPendingChange[DevIndex];
end;

procedure TInvControl2Obj.CalcVoltWatt_watts(j: Integer);
var
    DeltaPpu: Double;
  // PLimitEndpu[j] <= abs(kW_out_desiredpu[j] will always be true when we are in 'resquest' region of VW
  // That's what we want. In this region, VW will work similarly to VV. So we need to move slowly towards the VW curve point.
begin
    if ((PLimitEndpu[j] < 1.0) and (PLimitEndpu[j] <= abs(kW_out_desiredpu[j]))) or (FFlagVWOperates[j]) then
    begin
        if (ActiveCircuit.Solution.ControlIteration = 1) then
            POldVWpu[j] := abs(kW_out_desiredpu[j]); // take abs(kW_out_desiredpu[j]) because might be in charging mode.
        FFlagVWOperates[j] := TRUE;

        // PLimitEndpu might be negative here in 'requesting' region. Do we need to give POldVW a sign in this case?
        // Yes, it will naturally evolve to a negative value with the process. It will always positive only in the 1st control iteration.
        DeltaPpu := PLimitEndpu[j] - POldVWpu[j];

        if FdeltaP_factor = FLAGDELTAP then
            Change_deltaP_factor(j)
        else
            FdeltaPFactor[j] := FdeltaP_factor;

        PLimitVW[j] := (POldVWpu[j] + DeltaPpu * FdeltaPFactor[j]) * PBase[j];
    end
    else
    begin
        PLimitVW[j] := PLimitEndpu[j] * PBase[j];
    end;
end;

procedure TInvControl2Obj.Check_Plimits(j: Integer; P: Double);
var
    P_Ppriority: Double;
    pctDCkWRatedlimit: Double;

begin
    PLimitLimitedpu[j] := 1.0; // Not limited

    // volt-watt states
    if P < 1.0 then
        FVWOperation[j] := 1.0;

    pctDCkWRatedlimit := FpctDCkWRated[j] * FDCkWRated[j];

    // PLimitEndpu should be less than the P avaliable under var priority   (works for VV_VW)
    if FPPriority[j] = FALSE then
    begin
        P_Ppriority := Sqrt(SQR(FkVARating[j]) - SQR(Fpresentkvar[j]));
        if P_Ppriority < (abs(P) * PBase[j]) then   // P might be negative in requesting region for storage
        begin
            PLimitLimitedpu[j] := P_Ppriority / PBase[j] * sign(P);
            FVWOperation[j] := 0.0; // kVA exceeded under watt priority
        end;
    end;

    // PLimitEndpu should be less than pctPmpp
    if (abs(P) * PBase[j]) > pctDCkWRatedlimit then
    begin
        FVWOperation[j] := 0.0; // pctPmpp exceeded under watt priority
        PLimitLimitedpu[j] := pctDCkWRatedlimit / PBase[j] * sign(P);
    end;
end;

procedure TInvControl2Obj.CalcVoltVar_vars(j: Integer);
var
    DeltaQ: Double;

begin
    if (FlagChangeCurve[j] = FALSE) then
    begin
        if QDesireEndpu[j] >= 0.0 then
            DeltaQ := QDesireEndpu[j] * QHeadRoom[j] - QOldVV[j]
        else
            DeltaQ := QDesireEndpu[j] * QHeadRoomNeg[j] - QOldVV[j];

        if FdeltaQ_factor = FLAGDELTAQ then
            Change_deltaQ_factor(j)
        else
            FdeltaQFactor[j] := FdeltaQ_factor;

        QDesiredVV[j] := QOldVV[j] + DeltaQ * FdeltaQFactor[j];
    end
    // else, stay at present var output level
    else
    begin
        QDesiredVV[j] := Fpresentkvar[j]
    end;
end;

procedure TInvControl2Obj.CalcAVR_vars(j: Integer);
var
    DeltaQ: Double;

begin
    if QDesireEndpu[j] >= 0.0 then
        DeltaQ := QDesireEndpu[j] * QHeadRoom[j] - QOldAVR[j]
    else
        DeltaQ := QDesireEndpu[j] * QHeadRoomNeg[j] - QOldAVR[j];

    if FdeltaQ_factor = FLAGDELTAQ then
        Change_deltaQ_factor(j)
    else
        FdeltaQFactor[j] := FdeltaQ_factor;

    QDesiredAVR[j] := QOldAVR[j] + 0.2 * DeltaQ;

//      QDesiredAVR[j] := QDesireEndpu[j] * QHeadRoomNeg[j]

end;

procedure TInvControl2Obj.CalcWATTPF_vars(j: Integer);

begin
    if QDesireEndpu[j] >= 0.0 then
        QDesiredWP[j] := QDesireEndpu[j] * QHeadRoom[j]
    else
        QDesiredWP[j] := QDesireEndpu[j] * QHeadRoomNeg[j];
end;

procedure TInvControl2Obj.CalcWATTVAR_vars(j: Integer);

begin
    if QDesireEndpu[j] >= 0.0 then
        QDesiredWV[j] := QDesireEndpu[j] * QHeadRoom[j]
    else
        QDesiredWV[j] := QDesireEndpu[j] * QHeadRoomNeg[j];
end;

procedure TInvControl2Obj.CalcDRC_vars(j: Integer);
var
    DeltaQ: Double;

begin
    if QDesireEndpu[j] >= 0.0 then
        DeltaQ := QDesireEndpu[j] * QHeadRoom[j] - QOldDRC[j]
    else
        DeltaQ := QDesireEndpu[j] * QHeadRoomNeg[j] - QOldDRC[j];

    if FdeltaQ_factor = FLAGDELTAQ then
        Change_deltaQ_factor(j)
    else
        FdeltaQFactor[j] := FdeltaQ_factor;

    QDesiredDRC[j] := QOldDRC[j] + DeltaQ * FdeltaQFactor[j];
end;

procedure TInvControl2Obj.CalcVVDRC_vars(j: Integer);
var
    DeltaQ: Double;

begin
    if QDesireEndpu[j] >= 0.0 then
        DeltaQ := QDesireEndpu[j] * QHeadRoom[j] - QOldVVDRC[j]
    else
        DeltaQ := QDesireEndpu[j] * QHeadRoomNeg[j] - QOldVVDRC[j];

    if FdeltaQ_factor = FLAGDELTAQ then
        Change_deltaQ_factor(j)
    else
        FdeltaQFactor[j] := FdeltaQ_factor;

    QDesiredVVDRC[j] := QOldVVDRC[j] + DeltaQ * FdeltaQFactor[j];
end;

procedure TInvControl2Obj.Calc_PBase(j: Integer);
var
    DERelem: TPCElement;

begin
    DERelem := ControlledElement[j];

    if DERelem.DSSClassName = 'PVSystem' then
    begin
        if (FVoltwattYaxis = 0) then
            PBase[j] := FDCkW[j] * FEffFactor[j]

        else
        if (FVoltwattYaxis = 1) then
            PBase[j] := FDCkWRated[j]

        else
        if (FVoltwattYaxis = 2) then
            PBase[j] := FDCkWRated[j] * FpctDCkWRated[j]

        else
        if (FVoltwattYaxis = 3) then
            PBase[j] := FkVARating[j];
    end
    else
    begin
        if (FVoltwattYaxis = 0) then
            PBase[j] := TStorageObj(DERelem).DCkW * FEffFactor[j]

        else
        if (FVoltwattYaxis = 1) then
            PBase[j] := FDCkWRated[j]

        else
        if (FVoltwattYaxis = 2) then
            PBase[j] := FDCkWRated[j] * FpctDCkWRated[j]

        else
        if (FVoltwattYaxis = 3) then
            PBase[j] := FkVARating[j];

    end;
end;

procedure TInvControl2Obj.CalcLPF(m: Integer; powertype: Ansistring; LPF_desiredpu: Double);
var
    alpha: Double;

    // Applies the LPF:
    //  Return value is in kvar for VARS
    //  Return value is in puPmpp for WATTS

begin
    // Qoutput(t) = Qdesired(t) x {1- exp[-(t-t0)/tau]} + Qoutput(t-t0) x exp[-(t-t0)/tau]
    // calculate the alpha constant: alpha = exp[-(t-t0)/tau]
    alpha := exp(-1.0 * ActiveCircuit.Solution.DynaVars.h / LPFTau);

    if powertype = 'VARS' then
        QDesireOptionpu[m] := LPF_desiredpu * (1 - alpha) + FPriorQDesireOptionpu[m] * alpha;

    if powertype = 'WATTS' then
        PLimitOptionpu[m] := LPF_desiredpu * (1 - alpha) + FPriorPLimitOptionpu[m] * alpha

end;

procedure TInvControl2Obj.CalcRF(m: Integer; powertype: Ansistring; RF_desiredpu: Double);

begin
    // Applies the Rise/Fall limiting function:

    if powertype = 'VARS' then
    begin
        // rate of change rise/fall limit
        if (RF_desiredpu - FPriorQDesireOptionpu[m]) > (FRiseFallLimit * ActiveCircuit.Solution.DynaVars.h) then
            QDesireOptionpu[m] := FPriorQDesireOptionpu[m] + FRiseFallLimit * ActiveCircuit.Solution.DynaVars.h
        else
        if (RF_desiredpu - FPriorQDesireOptionpu[m]) < (-1 * FRiseFallLimit * ActiveCircuit.Solution.DynaVars.h) then
            QDesireOptionpu[m] := FPriorQDesireOptionpu[m] - FRiseFallLimit * ActiveCircuit.Solution.DynaVars.h
        else
            QDesireOptionpu[m] := RF_desiredpu;
    end;

    if powertype = 'WATTS' then
    begin
        // rate of change rise/fall limit
        if (RF_desiredpu - FPriorPLimitOptionpu[m]) > (FRiseFallLimit * ActiveCircuit.Solution.DynaVars.h) then
            PLimitOptionpu[m] := FPriorPLimitOptionpu[m] + (FRiseFallLimit * ActiveCircuit.Solution.DynaVars.h)
        else
        if (RF_desiredpu - FPriorPLimitOptionpu[m]) < (-1 * FRiseFallLimit * ActiveCircuit.Solution.DynaVars.h) then
            PLimitOptionpu[m] := FPriorPLimitOptionpu[m] - (FRiseFallLimit * ActiveCircuit.Solution.DynaVars.h)
        else
            PLimitOptionpu[m] := RF_desiredpu;
    end;
end;


procedure TInvControl2Obj.CalcPVWcurve_limitpu(j: Integer);
begin
    if ControlledElement[j].DSSClassName = 'PVSystem' then
        PLimitVWpu[j] := Fvoltwatt_curve.GetYValue(FPresentVpu[j])
    else
    begin
        if TStorageObj(ControlledElement[j]).StorageState = STORE_DISCHARGING then
        begin
            if TStorageObj(ControlledElement[j]).FVWStateRequested then
                PLimitVWpu[j] := FvoltwattCH_curve.GetYValue(FPresentVpu[j])
            else
                PLimitVWpu[j] := Fvoltwatt_curve.GetYValue(FPresentVpu[j]);

        end
        else
        if (TStorageObj(ControlledElement[j]).StorageState = STORE_CHARGING) and (FvoltwattCH_curve <> NIL) then
        begin
            if TStorageObj(ControlledElement[j]).FVWStateRequested then
                PLimitVWpu[j] := Fvoltwatt_curve.GetYValue(FPresentVpu[j])
            else
                PLimitVWpu[j] := FvoltwattCH_curve.GetYValue(FPresentVpu[j]) // try with positive PlimitVWpu
        end

        else
            PLimitVWpu[j] := 1.0; // don't limit if in idling state
    end;
end;


procedure TInvControl2Obj.CalcQVVcurve_desiredpu(j: Integer);
var
    voltagechangesolution: Double;
    QPresentpu: Double;
    VpuFromCurve: Double;

begin
    QDesireVVpu[j] := 0.0;

    if Fpresentkvar[j] >= 0.0 then
        QPresentpu := Fpresentkvar[j] / QHeadRoom[j]
    else
        QPresentpu := Fpresentkvar[j] / QHeadRoomNeg[j];

    voltagechangesolution := 0.0;

    // for first two seconds, keep voltagechangesolution equal to zero
    // we don't have solutions from the time-series power flow, yet
    if ((ActiveCircuit.Solution.DynaVars.dblHour * 3600.0 / ActiveCircuit.Solution.DynaVars.h) < 3.0) then
        voltagechangesolution := 0.0
    else
    if (FVpuSolutionIdx = 1) then
        voltagechangesolution := FVpuSolution[j, 1] - FVpuSolution[j, 2]
    else
    if (FVpuSolutionIdx = 2) then
        voltagechangesolution := FVpuSolution[j, 2] - FVpuSolution[j, 1];

    // if no hysteresis (Fvvc_curveOffset == 0), then just look up the value
    // from the volt-var curve
    if Fvvc_curveOffset = 0.0 then
    begin  // no hysteresis
        QDesireVVpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j])
    end // end of logic for the no-hysteresis case

    // else if we're going in the positive direction and on curve 1, stay
    // with curve 1
    else
    if (voltagechangesolution > 0) and (FActiveVVCurve[j] = 1) then
    begin
        if (FlagChangeCurve[j] = TRUE) then
        begin
            VpuFromCurve := Fvvc_curve.GetXValue(QPresentpu);
            if (Abs(FPresentVpu[j] - VpuFromCurve) < FVoltageChangeTolerance / 2.0) then
            begin
                QDesireVVpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j]);      //Y value = in per-unit of headroom
                FlagChangeCurve[j] := FALSE;
            end
            else
            begin
                QDesireVVpu[j] := QPresentpu;            // (PR) look at here
                FlagChangeCurve[j] := FALSE;
            end;
        end
        else
        begin
            QDesireVVpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j]);      //Y value = in per-unit of headroom
        end;
    end

    // with hysteresis if we're going in the positive direction on voltages
    // from last two power flow solutions, and we're using curve 2, keep vars
    // the same, and change to curve1 active
    else
    if (voltagechangesolution > 0) and (FActiveVVCurve[j] = 2) then
    begin
        QDesireVVpu[j] := QPresentpu;
        FActiveVVCurve[j] := 1;
        FlagChangeCurve[j] := TRUE;
    end

    // with hysteresis if we're going in the negative direction on voltages
    // from last two power flow solutions, and we're using curve 2, either
    // lookup the vars for the voltage we're at (with offset on curve1),
    // or if we've not just changed curves, stay at the current p.u.
    // var output
    else
    if (voltagechangesolution < 0) and (FActiveVVCurve[j] = 2) then
    begin
        if (FlagChangeCurve[j] = TRUE) then
        begin
            VpuFromCurve := Fvvc_curve.GetXValue(QPresentpu);
            VpuFromCurve := VpuFromCurve - Fvvc_curveOffset;
            if (Abs(FPresentVpu[j] - VpuFromCurve) < FVoltageChangeTolerance / 2.0) then
            begin
                QDesireVVpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j] - Fvvc_curveOffset);      //Y value = in per-unit of headroom
                FlagChangeCurve[j] := FALSE;
            end
            else
            begin
                QDesireVVpu[j] := QPresentpu;
                FlagChangeCurve[j] := FALSE;
            end;
        end
        else
        begin
            QDesireVVpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j] - Fvvc_curveOffset);      //Y value = in per-unit of headroom
        end;
    end

    // with hysteresis if we're going in the negative direction on voltages
    // from last two power flow solutions, and we're using curve 1, then
    // stay wjth present output vars and make curve2 active, set curve change
    // flag
    else
    if (voltagechangesolution < 0) and (FActiveVVCurve[j] = 1) then
    begin
        QDesireVVpu[j] := QPresentpu;
        FActiveVVCurve[j] := 2;
        FlagChangeCurve[j] := TRUE;
    end


    // if no change in voltage from one powerflow to the next, then
    // do one of the following
    else
    if (voltagechangesolution = 0) and (FActiveVVCurve[j] = 1) and (FlagChangeCurve[j] = FALSE) then
    begin
        QDesireVVpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j]);
    end
    else
    if (voltagechangesolution = 0) and (FlagChangeCurve[j] = TRUE) then
    begin
        QDesireVVpu[j] := QPresentpu;
    end

    else
    if (voltagechangesolution = 0) and (FActiveVVCurve[j] = 2) and (FlagChangeCurve[j] = FALSE) then
    begin
        QDesireVVpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j] - Fvvc_curveOffset);
    end;
end;

procedure TInvControl2Obj.CalcQWVcurve_desiredpu(j: Integer);
var
    // voltagechangesolution                    :Double;
    Pbase: Double;
begin
    QDesireWVpu[j] := 0.0;

    // voltagechangesolution := 0.0;

    // for first two seconds, keep voltagechangesolution equal to zero
    // we don't have solutions from the time-series power flow, yet
    // if ((ActiveCircuit.Solution.DynaVars.dblHour*3600.0 / ActiveCircuit.Solution.DynaVars.h)<3.0) then voltagechangesolution := 0.0
    // else if(FVpuSolutionIdx = 1) then voltagechangesolution := FVpuSolution[j,1] - FVpuSolution[j,2]
    // else if(FVpuSolutionIdx = 2) then voltagechangesolution := FVpuSolution[j,2] - FVpuSolution[j,1];

    Pbase := Min(FkVARating[j], FDCkWRated[j]); // Should include DC-to-AC and kW-to-KVA ratios to avoid to quick fix like this

    QDesireWVpu[j] := Fwattvar_curve.GetYValue(FDCkW[j] * FEffFactor[j] * FpctDCkWRated[j] / Pbase);
end;

procedure TInvControl2Obj.CalcQAVR_desiredpu(j: Integer);
var
    // voltagechangesolution                    : Double;
    DQ: Double;
    QPresentpu: Double;
    DQmax: Double;
    DeltaV: Double;
    v: Double;
begin
    DQmax := 0.1 * Fkvarlimit[j] / QHeadRoomNeg[j];

    QDesireAVRpu[j] := 0.0;

    if Fpresentkvar[j] >= 0.0 then
        QPresentpu := Fpresentkvar[j] / QHeadRoom[j]
    else
        QPresentpu := Fpresentkvar[j] / QHeadRoomNeg[j];

    if (ActiveCircuit.Solution.ControlIteration = 3) then
    begin
        v := FAvgpAVRVpuPrior[j];
        QPresentpu := 0.0;
        QOldAVR[j] := 0.0;
    end
    else
        v := FPresentVpu[j];

    // voltagechangesolution := 0.0;

    // for first two seconds, keep voltagechangesolution equal to zero
    // we don't have solutions from the time-series power flow, yet
    // if ((ActiveCircuit.Solution.DynaVars.dblHour*3600.0 / ActiveCircuit.Solution.DynaVars.h)<3.0) then voltagechangesolution := 0.0
    // else if(FVpuSolutionIdx = 1) then voltagechangesolution := FVpuSolution[j,1] - FVpuSolution[j,2]
    // else if(FVpuSolutionIdx = 2) then voltagechangesolution := FVpuSolution[j,2] - FVpuSolution[j,1];

    DeltaV := Abs(Fv_setpoint - FAvgpVpuPrior[j]);

    if (abs(DeltaV) < 0.005) and (FdeltaQFactor[j] > 0.2) then
        FdeltaQFactor[j] := FdeltaQFactor[j] + 0.1
    else
    if (abs(DeltaV) < 0.02) and (FdeltaQFactor[j] > 0.2) then
        FdeltaQFactor[j] := FdeltaQFactor[j] + 0.05
    else
    if (abs(DeltaV) > 0.02) and (FdeltaQFactor[j] < 0.9) then
        FdeltaQFactor[j] := FdeltaQFactor[j] - 0.05
    else
    if (abs(DeltaV) < 0.05) and (FdeltaQFactor[j] < 0.9) then
        FdeltaQFactor[j] := FdeltaQFactor[j] - 0.1;

    FdeltaQFactor[j] := 0.2;

    DeltaV_old[j] := Abs(FPresentVpu[j] - FAvgpVpuPrior[j]);

    if (FPresentVpu[j] - FAvgpVpuPrior[j] = 0) then
        DQ := 0
    else
        DQ := FdeltaQFactor[j] * DQDV[j] * (Fv_setpoint - v);
    if (Abs(DQ) > DQmax) then
        if (DQ < 0.0) then
            DQ := -DQmax
        else
            DQ := DQmax;

    QDesireAVRpu[j] := QPresentpu + DQ;
end;

procedure TInvControl2Obj.CalcQWPcurve_desiredpu(j: Integer);
var
    // voltagechangesolution                    :Double;
    p: Double;
    pf_priority: Boolean = False;
    QDesiredWP: Double;

begin
    QDesireWPpu[j] := 0.0;

    // voltagechangesolution := 0.0;

    // for first two seconds, keep voltagechangesolution equal to zero
    // we don't have solutions from the time-series power flow, yet
    // if ((ActiveCircuit.Solution.DynaVars.dblHour*3600.0 / ActiveCircuit.Solution.DynaVars.h)<3.0) then voltagechangesolution := 0.0
    // else if(FVpuSolutionIdx = 1) then voltagechangesolution := FVpuSolution[j,1] - FVpuSolution[j,2]
    // else if(FVpuSolutionIdx = 2) then voltagechangesolution := FVpuSolution[j,2] - FVpuSolution[j,1];

    pf_wp_nominal := Fwattpf_curve.GetYValue(FDCkW[j] * FEffFactor[j] * FpctDCkWRated[j] / FDCkWRated[j]);

    if ControlledElement[j].DSSClassName = 'PVSystem' then
        pf_priority := TPVSystemObj(ControlledElement[j]).PVSystemVars.PF_Priority
    else
    if ControlledElement[j].DSSClassName = 'Storage' then
        pf_priority := TStorageObj(ControlledElement[j]).StorageVars.PF_Priority;

    if (FPPriority[j] = FALSE) and (pf_priority = FALSE) then
        p := FDCkW[j] * FEffFactor[j] * FpctDCkWRated[j]
    else
        p := kW_out_desired[j];

    QDesiredWP := p * sqrt(1 / (pf_wp_nominal * pf_wp_nominal) - 1) * sign(pf_wp_nominal);


    if QDesiredWP >= 0.0 then
        QDesireWPpu[j] := QDesiredWP / QHeadRoom[j]
    else
        QDesireWPpu[j] := QDesiredWP / QHeadRoomNeg[j];
end;

procedure TInvControl2Obj.CalcQDRC_desiredpu(j: Integer);
var
    basekV: Double;

begin
    QDesireDRCpu[j] := 0.0;

    basekV := FVBase[j] / 1000.0; // It's a line-to-ground voltage

    // calculate deltaV quantity in per-unit from subtracting the rolling average
    // value (in p.u.) from the present p.u. terminal voltage (average of line-ground)
    // if more than one phase
    if (FDRCRollAvgWindow[j].AvgVal / (basekV * 1000.0)) = 0.0 then
        deltaVDynReac[j] := 0
    else
        deltaVDynReac[j] := FPresentDRCVpu[j] - FDRCRollAvgWindow[j].AvgVal / (basekV * 1000.0);

    // if below the lower deadband and deltaV quantity is non-zero then
    // calculate desired pu var output. In per-unit of kva rating (also
    // ampere rating), per report specifications.
    if (deltaVDynReac[j] <> 0) and (FPresentDRCVpu[j] < FDbVMin) then
        QDesireDRCpu[j] := -deltaVDynReac[j] * FArGraLowV

    // if above the upper deadband and deltaV quantity is non-zero then
    // calculate desired pu var output. In per-unit of kva rating (also
    // ampere rating), per report specifications.

    else
    if (deltaVDynReac[j] <> 0) and (FPresentDRCVpu[j] > FDbVMax) then
        QDesireDRCpu[j] := -deltaVDynReac[j] * FArGraHiV

    else
    if deltaVDynReac[j] = 0.0 then
        QDesireDRCpu[j] := 0.0;

    if (ActiveCircuit.Solution.Dynavars.t = 1) then
        QDesireDRCpu[j] := 0.0;
end;


procedure TInvControl2Obj.Check_Qlimits_WV(j: Integer; Q: Double);
var
    // Q_Ppriority                              :Double;
    currentkvarlimitpu: Double;
    currentkvarlimitnegpu: Double;
    FOperation: Double;
    error: Double;

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


    QDesireLimitedpu[j] := 1.0; // Not limited

    currentkvarlimitpu := FCurrentkvarLimit[j] / QHeadRoom[j];
    currentkvarlimitnegpu := FCurrentkvarLimitNeg[j] / QHeadRoomNeg[j];

    if currentkvarlimitpu > QDesireLimitedpu[j] then
        currentkvarlimitpu := QDesireLimitedpu[j];
    if currentkvarlimitnegpu > QDesireLimitedpu[j] then
        currentkvarlimitnegpu := QDesireLimitedpu[j];

    // Q curve desiredpu should be less than currentkvarlimit(neg)
    if (Q > 0.0) and (abs(Q) >= abs(currentkvarlimitpu)) then
    begin
        FOperation := 0.2 * sign(Q); // When kvarlimit is exceeded
        QDesireLimitedpu[j] := currentkvarlimitpu * sign(Q);
    end
    else
    if (Q < 0.0) and (abs(Q) >= abs(currentkvarlimitnegpu)) then
    begin
        FOperation := 0.2 * sign(Q); // When kvarlimitneg is exceeded
        QDesireLimitedpu[j] := currentkvarlimitnegpu * sign(Q);
    end;

    // States Flags
    if (ControlMode = WATTVAR) then
        FWVOperation[j] := FOperation;
end;

procedure TInvControl2Obj.Calc_PQ_WV(j: Integer);
var
    coeff: TCoeff;
  // QPratio, 
  // pre_S, 
    var_limit_operation_value,
    Pbase,
    Qbase,
  // Qbasesign, 
    A,
    B,
    C,
    a_line,
    b_line: Double;
begin
    Pbase := Min(FkVARating[j], FDCkWRated[j]);

    if QDesiredWV[j] >= 0.0 then
    begin
        Qbase := QHeadroom[j];
      // Qbasesign := 1.0;
    end
    else
    begin
        Qbase := QHeadroomNeg[j];
      // Qbasesign := -1.0;
    end;

    var_limit_operation_value := 0.2;
    if (abs(FWVOperation[j]) = var_limit_operation_value) then
        PLimitEndpu[j] := Fwattvar_curve.GetXValue(QDesireEndpu[j])
    else
        PLimitEndpu[j] := 1.0;

    CalcWATTVAR_vars(j);

  // Qdesiredpu should be less than the Q avaliable under watt priority  (works just for varmax)
    if (Sqrt(Sqr(FDCkW[j] * FEffFactor[j] * FpctDCkWRated[j] * PLimitEndpu[j]) + Sqr(QDesiredWV[j])) > FkVARating[j]) then
    begin
        coeff := Fwattvar_curve.GetCoefficients(FDCkW[j] * FEffFactor[j] * FpctDCkWRated[j] / Pbase);

        a_line := coeff[1] * Qbase / Pbase;
        b_line := coeff[2] * Qbase;

        A := 1 + Sqr(a_line);
        B := 2 * a_line * b_line;
        C := Sqr(b_line) - Sqr(FkVARating[j]);


        PLimitEndpu[j] := (-B + Sqrt(sqr(B) - 4 * A * C)) / (2 * A * Pbase);
        QDesireEndpu[j] := Fwattvar_curve.GetYValue(PLimitEndpu[j]);
    end;

    CalcWATTVAR_vars(j)
end;

procedure TInvControl2Obj.Check_Qlimits(j: Integer; Q: Double);
var
    Q_Ppriority: Double;
    currentkvarlimitpu: Double;
    currentkvarlimitnegpu: Double;
    FOperation: Double;
    error: Double;

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


    QDesireLimitedpu[j] := 1.0; // Not limited

    currentkvarlimitpu := FCurrentkvarLimit[j] / QHeadRoom[j];
    currentkvarlimitnegpu := FCurrentkvarLimitNeg[j] / QHeadRoomNeg[j];

    if currentkvarlimitpu > QDesireLimitedpu[j] then
        currentkvarlimitpu := QDesireLimitedpu[j];
    if currentkvarlimitnegpu > QDesireLimitedpu[j] then
        currentkvarlimitnegpu := QDesireLimitedpu[j];

    // Q curve desiredpu should be less than currentkvarlimit(neg)
    if (Q > 0.0) and (abs(Q) >= abs(currentkvarlimitpu)) then
    begin
        FOperation := 0.2 * sign(Q); // When kvarlimit is exceeded
        QDesireLimitedpu[j] := currentkvarlimitpu * sign(Q);
    end
    else
    if (Q < 0.0) and (abs(Q) >= abs(currentkvarlimitnegpu)) then
    begin
        FOperation := 0.2 * sign(Q); // When kvarlimitneg is exceeded
        QDesireLimitedpu[j] := currentkvarlimitnegpu * sign(Q);
    end;

    // Qdesiredpu should be less than the Q avaliable under watt priority  (works just for varmax)
    if FPPriority[j] and ((FReacPower_ref = ReacPower_VARMAX) or (ControlMode = WATTPF)) then
    begin
        if Q >= 0.0 then
            Q_Ppriority := Sqrt(SQR(FkVARating[j]) - SQR(FpresentkW[j])) / QHeadRoom[j]
        else
            Q_Ppriority := Sqrt(SQR(FkVARating[j]) - SQR(FpresentkW[j])) / QHeadRoomNeg[j];

        if (abs(Q_Ppriority) < abs(QDesireLimitedpu[j])) and (abs(Q_Ppriority) < abs(Q)) then
        begin
            FOperation := 0.6 * sign(Q); // kVA exceeded under watt priority is considered above
            if (abs(Q) < (0.01 / 100)) or (abs(Q_Ppriority) < epsilon) then
                FOperation := 0.0;
            QDesireLimitedpu[j] := Q_Ppriority * sign(Q);
        end;
    end;


    // States Flags
    if (ControlMode = VOLTVAR) then
        FVVOperation[j] := FOperation;
    if (ControlMode = WATTPF) then
        FWPOperation[j] := FOperation;
    if (ControlMode = WATTVAR) then
        FWVOperation[j] := FOperation;
    if (ControlMode = DRC) then
        FDRCOperation[j] := FOperation;
    if (ControlMode = AVR) then
        FAVROperation[j] := FOperation;
    if (CombiMode = VV_DRC) then
        FVVDRCOperation[j] := FOperation;
    if (CombiMode = VV_VW) then
        FVVOperation[j] := FOperation;
end;

procedure TInvControl2Obj.Calc_QHeadRoom(j: Integer);
begin
    if FReacPower_ref = ReacPower_VARAVAL then
    begin
        if (abs(FpresentkW[j]) < FkVARating[j]) then
            QHeadRoom[j] := SQRT(Sqr(FkVARating[j]) - Sqr(FpresentkW[j]))
        else
            QHeadRoom[j] := 0.0;

        QHeadRoomNeg[j] := QHeadRoom[j];
    end;

    if (FReacPower_ref = ReacPower_VARMAX) or (ControlMode = WATTPF) then
    begin
        QHeadRoom[j] := FkvarLimit[j];
        QHeadRoomNeg[j] := FkvarLimitNeg[j];
    end;

    if (QHeadRoom[j] = 0.0) then
        QHeadRoom[j] := FkvarLimit[j];
    if (QHeadRoomNeg[j] = 0.0) then
        QHeadRoomNeg[j] := FkvarLimitNeg[j];
end;

procedure TInvControl2Obj.Change_deltaQ_factor(j: Integer);
var
    DeltaV: Double;

begin
    DeltaV := Abs(FPresentVpu[j] - FAvgpVpuPrior[j]);

    if (DeltaV_old[j] >= 0.0) then
    begin
        if (abs(DeltaV) > 0.8 * DeltaV_old[j]) and (FdeltaQFactor[j] > 0.2) then
            FdeltaQFactor[j] := FdeltaQFactor[j] - 0.1
        else
        if (abs(DeltaV) > 0.6 * DeltaV_old[j]) and (FdeltaQFactor[j] > 0.2) then
            FdeltaQFactor[j] := FdeltaQFactor[j] - 0.05
        else
        if (abs(DeltaV) < 0.2 * DeltaV_old[j]) and (FdeltaQFactor[j] < 0.9) then
            FdeltaQFactor[j] := FdeltaQFactor[j] + 0.1
        else
        if (abs(DeltaV) < 0.4 * DeltaV_old[j]) and (FdeltaQFactor[j] < 0.9) then
            FdeltaQFactor[j] := FdeltaQFactor[j] + 0.05;
    end;

    DeltaV_old[j] := Abs(FPresentVpu[j] - FAvgpVpuPrior[j]);
end;

procedure TInvControl2Obj.Change_deltaP_factor(j: Integer);
var
    DeltaV: Double;

begin
    DeltaV := Abs(FPresentVpu[j] - FAvgpVpuPrior[j]);

    if DeltaV_old[j] >= 0.0 then
    begin
        if (abs(DeltaV) > 0.9 * DeltaV_old[j]) and (FdeltaPFactor[j] > 0.2) then
            FdeltaPFactor[j] := FdeltaPFactor[j] - 0.1
        else
        if (abs(DeltaV) > 0.8 * DeltaV_old[j]) and (FdeltaPFactor[j] > 0.1) then
            FdeltaPFactor[j] := FdeltaPFactor[j] - 0.05
        else
        if (abs(DeltaV) < 0.2 * DeltaV_old[j]) and (FdeltaPFactor[j] < 0.9) then
            FdeltaPFactor[j] := FdeltaPFactor[j] + 0.05
        else
        if (abs(DeltaV) < 0.1 * DeltaV_old[j]) and (FdeltaPFactor[j] < 0.9) then
            FdeltaPFactor[j] := FdeltaPFactor[j] + 0.1;
    end;

    DeltaV_old[j] := Abs(FPresentVpu[j] - FAvgpVpuPrior[j]);
end;

//Called at end of main power flow solution loop
procedure TInvControl2.UpdateAll();
var
    i: Integer;

begin
    for i := 1 to ElementList.Count do
        with TInvControl2Obj(ElementList.Get(i)) do
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
end.
