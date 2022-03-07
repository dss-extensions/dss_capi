unit StorageController2;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2019, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

//  A StorageController is a control element that is connected to a terminal of another
//  circuit element and sends dispatch  signals to a fleet of energy storage elements it controls
//
//  A StorageController is defined by a New command:
//
//  New StorageController.Name=myname Element=devclass.name terminal=[ 1|2|...] Elementlist = (elem1  elem2 ...)
//
//  or ... ElementList = [File=filename] where storage class elements are listed one to a line
//  If omitted, all storage elements found in the active circuit are included by default and controlled as a fleet.
//
//  Added new control mode for charging 12/19/2018
//  Proposed by Valentin Rigoni

interface

uses
    Command,
    ControlClass,
    ControlElem,
    CktElement,
    DSSClass,
    Arraydef,
    UComplex, DSSUcomplex,
    utilities,
    DSSPointerList,
    Classes,
    Loadshape;

const
    AVG = -1;
    MAXPHASE = -2;
    MINPHASE = -3;

type
{$SCOPEDENUMS ON}
    TStorageController2Prop = (
        INVALID = 0,
        Element = 1,
        Terminal = 2,
        MonPhase = 3,
        kWTarget = 4,
        kWTargetLow = 5,
        pctkWBand = 6,
        kWBand = 7,
        pctkWBandLow = 8,
        kWBandLow = 9,
        ElementList = 10,
        Weights = 11,
        ModeDischarge = 12,
        ModeCharge = 13,
        TimeDischargeTrigger = 14,
        TimeChargeTrigger = 15,
        pctRatekW = 16,
        pctRateCharge = 17,
        pctReserve = 18,
        kWhTotal = 19,
        kWTotal = 20,
        kWhActual = 21,
        kWActual = 22,
        kWneed = 23,
        Yearly = 24,
        Daily = 25,
        Duty = 26,
        EventLog = 27,
        InhibitTime = 28,
        Tup = 29,
        TFlat = 30,
        Tdn = 31,
        kWThreshold = 32,
        DispFactor = 33,
        ResetLevel = 34,
        Seasons = 35,
        SeasonTargets = 36,
        SeasonTargetsLow = 37
    );
{$SCOPEDENUMS OFF}

    TStorageController2 = class(TControlClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TStorageController2Obj = class(TControlElem)
    PRIVATE
        FkWTarget,
        FkWTargetLow,
        FkWThreshold,
        FpctkWBand,
        FkWBand,
        FpctkWBandLow,
        FkWBandLow,
        HalfkWBand,
        HalfkWBandLow,
        TotalWeight,
        UpRamptime,
        FlatTime,
        DnrampTime,
        UpPlusFlat,
        UpPlusFlatPlusDn,
        DischargeTriggerTime,
        ChargeTriggerTime,
        pctKWRate,
        pctChargeRate,
        LastpctDischargeRate,
        //TotalkWCapacity,
        //TotalkWhCapacity,
        pctFleetReserve,
        ResetLevel,
        kWNeeded,
        DispFactor: Double;  // for slower convergence

        FStorageNameList: TStringList;
        FleetPointerList: TDSSPointerList;
        SeasonTargets,
        SeasonTargetsLow: Array of Double;
        FWeights: pDoubleArray;
        cBuffer: pComplexArray;    // Complex Array buffer

        FleetListChanged,
        ChargingAllowed,
        DischargeTriggeredByTime,
        DischargeInhibited,
        OutOfOomph,
        FElementListSpecified,
        Wait4Step,
        FkWBandSpecified: Boolean;  // true if kWBand specified as an absolute value (for use in Follow Discharge Mode to update the target)

        Seasons,
        FleetSize,
        FleetState,
        DischargeMode,
        InhibitHrs,
        ChargeMode,
        FMonPhase,
        CondOffset: Integer;

        YearlyShapeObj: TLoadShapeObj;  // Shape for this Storage element
        DailyShapeObj: TLoadShapeObj;  // Daily Storage element Shape for this load
        DutyShapeObj: TLoadShapeObj;  // Shape for this Storage element

        LoadShapeMult: Complex;

        procedure SetAllFleetValues;
        procedure SetFleetkWRate(pctkw: Double);
        procedure SetFleetChargeRate;
        procedure SetFleetToCharge;
        procedure SetFleetToDisCharge;
        procedure SetFleetToIdle;
        procedure SetFleetToExternal;
        procedure SetFleetDesiredState(state: Integer);
        procedure CalcYearlyMult(Hr: Double);
        procedure CalcDailyMult(Hr: Double);
        procedure CalcDutyMult(Hr: Double);

        function MakeFleetList: Boolean;
        procedure DoLoadFollowMode();
        procedure DoLoadShapeMode();
        procedure DoTimeMode(Opt: Integer);
        procedure DoScheduleMode();
        procedure DoPeakShaveModeLow();
        procedure PushTimeOntoControlQueue(Code: Integer);
        function NormalizeToTOD(h: Integer; sec: Double): Double;
        procedure GetControlPower(var ControlPower: Complex);
        procedure GetControlCurrent(var ControlCurrent: Double);
        function Get_FleetkW: Double;
        function Get_FleetkWh: Double;
        function Get_FleetkWhRating: Double;
        function Get_FleetReservekWh: Double;

        function Get_DynamicTarget(THigh: Integer): Double;

    PUBLIC
        constructor Create(ParClass: TDSSClass; const StorageController2Name: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData(); OVERRIDE;

        procedure Sample(); OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state

        property FleetkWhRating: Double READ Get_FleetkWhRating;
        property FleetReservekWh: Double READ Get_FleetReservekWh;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    Storage2,
    Sysutils,
    uCmatrix,
    MathUtil,
    Math,
    Solution,
    Dynamics,
    XYCurve,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TStorageController2Obj;
    TProp = TStorageController2Prop;
const
    NumPropsThisClass = Ord(High(TProp));

//= = = = = = = = = = = = = = DEFINE CONTROL MODE CONSTANTS = = = = = = = = = = = = = = = = = = = = = = = = =

    MODEFOLLOW = 1;
    MODELOADSHAPE = 2;
    MODESUPPORT = 3;
    MODETIME = 4;
    MODEPEAKSHAVE = 5;
    MODESCHEDULE = 6;
    MODEPEAKSHAVELOW = 7;
    CURRENTPEAKSHAVE = 8;
    CURRENTPEAKSHAVELOW = 9;

//= = = = = = = = = = = = = = DEFINE OTHER CONSTANTS = = = = = = = = = = = = = = = = = = = = = = = = =
    RELEASE_INHIBIT = 999;

type
    TStorageObj = TStorage2Obj;
var
    PropInfo: Pointer = NIL;    
    ChargeModeEnum, DischargeModeEnum: TDSSEnum;

constructor TStorageController2.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        DischargeModeEnum := TDSSEnum.Create('StorageController: Discharge mode', False, 1, 2, 
            ['Peakshave', 'Follow', 'Support', 'Loadshape', 'Time', 'Schedule', 'I-Peakshave'],
            [MODEPEAKSHAVE, MODEFOLLOW, MODESUPPORT, MODELOADSHAPE, MODETIME, MODESCHEDULE, CURRENTPEAKSHAVE]);
        ChargeModeEnum := TDSSEnum.Create('StorageController: Charge mode', False, 1, 1, 
            ['Loadshape', 'Time', 'PeakshaveLow', 'I-PeakshaveLow'],
            [MODELOADSHAPE, MODETIME, MODEPEAKSHAVELOW, CURRENTPEAKSHAVELOW]);
    end;
    inherited Create(dssContext, Storage_CONTROL, 'StorageController');
end;

destructor TStorageController2.Destroy;
begin
    inherited Destroy;
end;

function GetkWActual(Obj: TObj): Double;
begin
    Result := Obj.Get_FleetkW();
end;

function GetkWhActual(Obj: TObj): Double;
begin
    Result := Obj.Get_FleetkWh();
end;

function GetkWhTotal(Obj: TObj): Double;
var
    pStorage: TStorageObj;
    i: Integer;
begin
    Result := 0.0;
    with obj do
        for i := 1 to FleetPointerList.Count do
        begin
            pStorage := FleetPointerList.Get(i);
            Result := Result + pStorage.StorageVars.kWhRating;
        end;
end;

function GetkWTotal(Obj: TObj): Double;
var
    pStorage: TStorageObj;
    i: Integer;
begin
    Result := 0.0;
    with obj do
        for i := 1 to FleetPointerList.Count do
        begin
            pStorage := FleetPointerList.Get(i);
            Result := Result + pStorage.StorageVars.kWRating;
        end;
end;

procedure TStorageController2.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // enum properties
    PropertyType[ord(TProp.ModeDischarge)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.ModeDischarge)] := ptruint(@obj.DischargeMode);
    PropertyOffset2[ord(TProp.ModeDischarge)] := PtrInt(DischargeModeEnum);

    PropertyType[ord(TProp.ModeCharge)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.ModeCharge)] := ptruint(@obj.ChargeMode);
    PropertyOffset2[ord(TProp.ModeCharge)] := PtrInt(ChargeModeEnum);

    PropertyType[ord(TProp.MonPhase)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.MonPhase)] := ptruint(@obj.FMonPhase);
    PropertyOffset2[ord(TProp.MonPhase)] := PtrInt(DSS.MonPhaseEnum);

    // boolean properties
    PropertyType[ord(TProp.EventLog)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.EventLog)] := ptruint(@obj.ShowEventLog);

    // string lists
    PropertyType[ord(TProp.ElementList)] := TPropertyType.StringListProperty;
    PropertyOffset[ord(TProp.ElementList)] := ptruint(@obj.FStorageNameList);

    // objects
    PropertyType[ord(TProp.yearly)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.daily)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.duty)] := TPropertyType.DSSObjectReferenceProperty;
    
    PropertyOffset[ord(TProp.yearly)] := ptruint(@obj.YearlyShapeObj);
    PropertyOffset[ord(TProp.daily)] := ptruint(@obj.DailyShapeObj);
    PropertyOffset[ord(TProp.duty)] := ptruint(@obj.DutyShapeObj);

    PropertyOffset2[ord(TProp.yearly)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.daily)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.duty)] := ptruint(DSS.LoadShapeClass);

    PropertyType[ord(TProp.Element)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.Element)] := ptruint(@obj.FMonitoredElement);
    PropertyWriteFunction[ord(TProp.Element)] := @SetMonitoredElement;
    PropertyFlags[ord(TProp.Element)] := [TPropertyFlag.WriteByFunction];//[TPropertyFlag.CheckForVar]; // not required for general cktelements

    // integer properties
    PropertyType[ord(TProp.Terminal)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Terminal)] := ptruint(@obj.ElementTerminal);

    PropertyType[ord(TProp.Seasons)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Seasons)] := ptruint(@obj.Seasons);

    PropertyType[ord(TProp.InhibitTime)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.InhibitTime)] := ptruint(@obj.Inhibithrs); // >=1, silent, handled as side effect

    // double arrays
    PropertyType[ord(TProp.SeasonTargets)] := TPropertyType.DoubleDArrayProperty;
    PropertyOffset[ord(TProp.SeasonTargets)] := ptruint(@obj.SeasonTargets);
    PropertyOffset2[ord(TProp.SeasonTargets)] := ptruint(@obj.Seasons);

    PropertyType[ord(TProp.SeasonTargetsLow)] := TPropertyType.DoubleDArrayProperty;
    PropertyOffset[ord(TProp.SeasonTargetsLow)] := ptruint(@obj.SeasonTargetsLow);
    PropertyOffset2[ord(TProp.SeasonTargetsLow)] := ptruint(@obj.Seasons);

    PropertyType[ord(TProp.Weights)] := TPropertyType.DoubleDArrayProperty;
    PropertyOffset[ord(TProp.Weights)] := ptruint(@obj.FWeights);
    PropertyOffset2[ord(TProp.Weights)] := ptruint(@obj.FleetSize);

    // double functions
    PropertyFlags[ord(TProp.kWhTotal)] := [TPropertyFlag.SilentReadOnly, TPropertyFlag.ReadByFunction];
    PropertyFlags[ord(TProp.kWTotal)] := [TPropertyFlag.SilentReadOnly, TPropertyFlag.ReadByFunction];
    PropertyFlags[ord(TProp.kWhActual)] := [TPropertyFlag.SilentReadOnly, TPropertyFlag.ReadByFunction];
    PropertyFlags[ord(TProp.kWActual)] := [TPropertyFlag.SilentReadOnly, TPropertyFlag.ReadByFunction];
    PropertyReadFunction[ord(TProp.kWhTotal)] := @GetkWhTotal;
    PropertyReadFunction[ord(TProp.kWTotal)] := @GetkWTotal;
    PropertyReadFunction[ord(TProp.kWhActual)] := @GetkWhActual;
    PropertyReadFunction[ord(TProp.kWActual)] := @GetkWActual;

    // double read-only
    PropertyFlags[ord(TProp.kWneed)] := [TPropertyFlag.SilentReadOnly];
    PropertyOffset[ord(TProp.kWneed)] := ptruint(@obj.kWNeeded);

    // double properties
    PropertyOffset[ord(TProp.DispFactor)] := ptruint(@obj.DispFactor);
    //PropertyFlags[ord(TProp.DispFactor)] := [TPropertyFlag.NotNegative, TPropertyFlag.NotZero];
    // >0,<=1

    PropertyOffset[ord(TProp.kWTarget)] := ptruint(@obj.FkWTarget);
    PropertyOffset[ord(TProp.kWTargetLow)] := ptruint(@obj.FkWTargetLow);
    PropertyOffset[ord(TProp.kWBand)] := ptruint(@obj.FkWBand);
    PropertyOffset[ord(TProp.pctkWBand)] := ptruint(@obj.FpctkWBand);
    PropertyOffset[ord(TProp.pctkWBandLow)] := ptruint(@obj.FpctkWBandLow);
    PropertyOffset[ord(TProp.kWBandLow)] := ptruint(@obj.FkWBandLow);
    PropertyOffset[ord(TProp.TimeDischargeTrigger)] := ptruint(@obj.DischargeTriggerTime);
    PropertyOffset[ord(TProp.TimeChargeTrigger)] := ptruint(@obj.ChargeTriggerTime);
    PropertyOffset[ord(TProp.pctRatekW)] := ptruint(@obj.pctkWRate);
    PropertyOffset[ord(TProp.pctRateCharge)] := ptruint(@obj.pctChargeRate);
    PropertyOffset[ord(TProp.pctReserve)] := ptruint(@obj.pctFleetReserve);
    PropertyOffset[ord(TProp.Tup)] := ptruint(@obj.UpRamptime);
    PropertyOffset[ord(TProp.TFlat)] := ptruint(@obj.FlatTime);
    PropertyOffset[ord(TProp.Tdn)] := ptruint(@obj.DnrampTime);
    PropertyOffset[ord(TProp.kWThreshold)] := ptruint(@obj.FkWThreshold);
    PropertyOffset[ord(TProp.ResetLevel)] := ptruint(@obj.ResetLevel);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TStorageController2.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TStorageController2Obj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    i: Integer;
    casemult: Double;
begin
    case Idx of
        ord(TProp.kWTarget):
        begin
            if DischargeMode = CURRENTPEAKSHAVE then  // evaluates the discharging mode to apply
                Casemult := 1000.0                 // a compensation value (for kamps)
            else
                Casemult := 1.0;

            FkWThreshold := FkWTarget * 0.75 * Casemult;

            HalfkWBand := FpctkWBand / 200.0 * FkWTarget * Casemult;
            FkWBand := 2.0 * HalfkWBand;
            FpctkWBand := FkWBand / FkWTarget * 100.0; // sync FpctkWBand

        end;
        ord(TProp.pctkWBand):
        begin
            if DischargeMode = CURRENTPEAKSHAVE then  // evaluates the discharging mode to apply
                Casemult := 1000.0                 // a compensation value (for kamps)
            else
                Casemult := 1.0;

            HalfkWBand := FpctkWBand / 200.0 * FkWTarget * Casemult;
            FkWBand := 2.0 * HalfkWBand;
            FkWBandSpecified := FALSE;

        end;
        ord(TProp.kWBand):
        begin
            if DischargeMode = CURRENTPEAKSHAVE then  // evaluates the discharging mode to apply
                Casemult := 1000.0                 // a compensation value (for kamps)
            else
                Casemult := 1.0;

            HalfkWBand := FkWBand / 2.0 * Casemult;
            FpctkWBand := FkWBand / FkWTarget * 100.0; // sync FpctkWBand
            FkWBandSpecified := TRUE;
        end;
        ord(TProp.kWTargetLow),
        ord(TProp.pctkWBandLow):
        begin
            if ChargeMode = CURRENTPEAKSHAVELOW then  // evaluates the charging mode to apply
                Casemult := 1000.0                 // a compensation value (for kamps)
            else
                Casemult := 1.0;

            HalfkWBandLow := FpctkWBandLow / 200.0 * FkWTargetLow * Casemult;
            FkWBandLow := HalfkWBandLow * 2.0;
        end;
        ord(TProp.kWBandLow):
        begin
            if ChargeMode = CURRENTPEAKSHAVELOW then  // evaluates the charging mode to apply
                Casemult := 1000.0                 // a compensation value (for kamps)
            else
                Casemult := 1.0;

            HalfkWBandLow := FkWBandLow / 2.0 * Casemult;
            FpctkWBand := FkWBandLow / FkWTarget * 100.0; // sync FpctkWBandLow
        end;
        ord(TProp.ModeDischarge):
            if DischargeMode = MODEFOLLOW then
                DischargeTriggerTime := 12.0; // Noon

        ord(TProp.MonPhase):
            if FMonPhase > FNphases then
            begin
                DoSimpleMsg('Error: Monitored phase (%d) must be less than or equal to number of phases (%d). ', [FMonPhase, FNphases], 35302);
                FMonPhase := 1;
            end;

        ord(TProp.ElementList):
        begin   // levelize the list
            FleetPointerList.Clear;  // clear this for resetting on first sample
            FleetListChanged := TRUE;
            FElementListSpecified := TRUE;
            FleetSize := FStorageNameList.count;
            // Realloc weights to be same size as possible number of storage elements
            Reallocmem(FWeights, Sizeof(FWeights^[1]) * FleetSize);
            for i := 1 to FleetSize do
                FWeights^[i] := 1.0;
        end;
        ord(TProp.Seasons):
        begin
            setlength(SeasonTargets, Seasons);
            setlength(SeasonTargetsLow, Seasons);
        end;
        ord(TProp.DispFactor):
            if (DispFactor <= 0) or (DispFactor > 1) then
                DispFactor := 1;
        ord(TProp.InhibitTime):
            Inhibithrs := Max(1, Inhibithrs);  // >=1, silent
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TStorageController2Obj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    FNPhases := Other.Fnphases;
    NConds := Other.Fnconds; // Force Reallocation of terminal stuff

    // ControlledElement := Other.ControlledElement;  // Pointer to target circuit element
    MonitoredElement := Other.MonitoredElement;  // Pointer to target circuit element
    ElementTerminal := Other.ElementTerminal;
    FMonPhase := Other.FMonPhase;
    CondOffset := Other.CondOffset;

    FkWTarget := Other.FkWTarget;
    FkWTargetLow := Other.FkWTargetLow;
    FkWThreshold := Other.FkWThreshold;

    DispFactor := Other.DispFactor;

    FpctkWBand := Other.FpctkWBand;
    FkWBand := Other.FkWBand;
    FpctkWBandLow := Other.FpctkWBandLow;
    FkWBandLow := Other.FkWBandLow;
    ResetLevel := Other.ResetLevel;

    FkWBandSpecified := Other.FkWBandSpecified;
    FStorageNameList.Clear;
    for i := 1 to Other.FStorageNameList.Count do
        FStorageNameList.Add(Other.FStorageNameList.Strings[i - 1]);

    FleetSize := FStorageNameList.count;
    if FleetSize > 0 then
    begin
        Reallocmem(FWeights, Sizeof(FWeights^[1]) * FleetSize);
        for i := 1 to FleetSize do
            FWeights^[i] := Other.FWeights^[i];
    end;

    DisChargeMode := Other.DisChargeMode;
    ChargeMode := Other.ChargeMode;
    DischargeTriggerTime := Other.DischargeTriggerTime;
    ChargeTriggerTime := Other.ChargeTriggerTime;
    pctkWRate := Other.pctkWRate;
    pctChargeRate := Other.pctChargeRate;
    pctFleetReserve := Other.pctFleetReserve;
    YearlyShapeObj := Other.YearlyShapeObj;
    DailyShapeObj := Other.DailyShapeObj;
    DutyShapeObj := Other.DutyShapeObj;
    ShowEventLog := Other.ShowEventLog;
    Inhibithrs := Other.Inhibithrs;

    UpRamptime := Other.UpRamptime;
    FlatTime := Other.FlatTime;
    DnrampTime := Other.DnrampTime;

    Seasons := Other.Seasons;
    if Seasons > 1 then
    begin
        setlength(SeasonTargets, Seasons);
        setlength(SeasonTargetsLow, Seasons);
        for i := 0 to (Seasons - 1) do
        begin
            SeasonTargets[i] := Other.SeasonTargets[i];
            SeasonTargetsLow[i] := Other.SeasonTargetsLow[i];
        end;
    end;
end;

constructor TStorageController2Obj.Create(ParClass: TDSSClass; const StorageController2Name: String);
begin
    inherited Create(ParClass);
    Name := AnsiLowerCase(StorageController2Name);
    DSSObjType := ParClass.DSSClassType;

    FNPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors

    ControlledElement := NIL;    // not used in this control
    ElementTerminal := 1;
    MonitoredElement := NIL;
    FMonPhase := MAXPHASE;
    cBuffer := NIL; // Complex buffer

    FStorageNameList := TSTringList.Create;
    FWeights := NIL;
    FleetPointerList := TDSSPointerList.Create(20);  // Default size and increment
    FleetSize := 0;
    FleetState := STORE_IDLING;
    FkWTarget := 8000.0;
    FkWTargetLow := 4000.0;
    FkWThreshold := 6000.0;
    DispFactor := 1.0;
    FpctkWBand := 2.0;
    FpctkWBandLow := 2.0;
    HalfkWBand := FpctkWBand / 200.0 * FkWTarget;
    HalfkWBandLow := FpctkWBandLow / 200.0 * FkWTargetLow;
    FkWBand := HalfkWBand * 2.0;
    FkWBandLow := HalfkWBandLow * 2.0;
    TotalWeight := 1.0;

//     FPFTarget                := 0.96;
//     FPFBand                  := 0.04;
//     HalfPFBand               := FPFBand / 2.0;
    kWNeeded := 0.0;

    DischargeMode := MODEPEAKSHAVE;
    ChargeMode := MODETIME;

    DischargeTriggerTime := -1.0;  // disabled
    ChargeTriggerTime := 2.0;   // 2 AM
    FElementListSpecified := FALSE;
    FleetListChanged := TRUE;  // force building of list
    FkWBandSpecified := FALSE;  // adopt pctkWBand by default

    pctkWRate := 20.0;
//     pctkvarRate              := 20.0;
    pctChargeRate := 20.0;
    pctFleetReserve := 25.0;

    ShowEventLog := FALSE;
//     DispatchVars             := FALSE;
    DischargeTriggeredByTime := FALSE;
    DischargeInhibited := FALSE;
    OutOfOomph := FALSE;
    InhibitHrs := 5;   // No. Hours to inhibit discharging after going into charge mode

    UpRamptime := 0.25; // hr
    FlatTime := 2.0;
    DnrampTime := 0.25;
    LastpctDischargeRate := 0.0;
    Wait4Step := FALSE;     // for sync discharge with charge when there is a transition
    ResetLevel := 0.8;
    Seasons := 1;         // For dynamic targets
    setlength(SeasonTargets, 1);
    SeasonTargets[0] := FkWTarget;
    setlength(SeasonTargetsLow, 1);
    SeasonTargetsLow[0] := FkWTargetLow;
end;

destructor TStorageController2Obj.Destroy;
begin
    if Assigned(cBuffer) then
        ReallocMem(cBuffer, 0);

    FleetPointerList.Free;
    FStorageNameList.Free;

    inherited Destroy;
end;

function TStorageController2Obj.Get_FleetkW: Double;

var
    pStorage: TStorageObj;
    i: Integer;
begin
    Result := 0.0;
    for I := 1 to FleetPointerList.Count do
    begin
        pStorage := FleetPointerList.Get(i);
        Result := Result + pStorage.PresentkW;
    end;
end;

function TStorageController2Obj.Get_FleetkWh: Double;
var
    pStorage: TStorageObj;
    i: Integer;
begin
    Result := 0.0;
    for I := 1 to FleetPointerList.Count do
    begin
        pStorage := FleetPointerList.Get(i);
        Result := Result + pStorage.StorageVars.kWhStored;
    end;
end;

function TStorageController2Obj.Get_FleetkWhRating: Double;
var
    pStorage: TStorageObj;
    i: Integer;
begin
    Result := 0.0;
    for I := 1 to FleetPointerList.Count do
    begin
        pStorage := FleetPointerList.Get(i);
        Result := Result + pStorage.StorageVars.kWhRating;
    end;
end;

function TStorageController2Obj.Get_FleetReservekWh: Double;
var
    pStorage: TStorageObj;
    i: Integer;
begin
    Result := 0.0;
    for I := 1 to FleetPointerList.Count do
    begin
        pStorage := FleetPointerList.Get(i);
        Result := Result + pStorage.StorageVars.kWhReserve;
    end;
end;

procedure TStorageController2Obj.RecalcElementData();
// Recalculate critical element values after changes have been made
begin
    {Check for existence of monitored element}

    if MonitoredElement <> NIL then
    begin
        if ElementTerminal > MonitoredElement.Nterms then
        begin
            DoErrorMsg(Format('StorageController: "%s"', [Name]),
                Format('Terminal no. "%d" Does not exist.', [ElementTerminal]),
                'Re-specify terminal no.', 371);
        end
        else
        begin
            FNphases := MonitoredElement.Nphases;
            NConds := FNphases;

            // Sets name of i-th terminal's connected bus in StorageController's buslist
            Setbus(1, MonitoredElement.GetBus(ElementTerminal));

            // Allocate a buffer bigenough to hold everything from the monitored element
            ReAllocMem(cBuffer, SizeOF(cBuffer^[1]) * MonitoredElement.Yorder);
            CondOffset := (ElementTerminal - 1) * MonitoredElement.NConds; // for speedy sampling
        end;
    end
    else
        DoSimpleMsg('Monitored Element in %s is not set', [FullName], 372);

    if FleetListChanged then
        if not MakeFleetList then
            DoSimpleMsg('No unassigned Storage Elements found to assign to %s', [FullName], 37201);

    // TotalkWCapacity := GetkWTotal(self);
    // TotalkWhCapacity := GetkWhTotal(self);

    if FleetSize > 0 then
    begin
        SetFleetToExternal;
        SetAllFleetValues;
    end;

    UpPlusFlat := UpRampTime + FlatTime;
    UpPlusFlatPlusDn := UpPlusFlat + DnRampTime;
end;

procedure TStorageController2Obj.MakePosSequence();
begin
    if MonitoredElement <> NIL then
    begin
        FNphases := MonitoredElement.NPhases;
        Nconds := FNphases;
        Setbus(1, MonitoredElement.GetBus(ElementTerminal));
        // Allocate a buffer big enough to hold everything from the monitored element
        ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder);
        CondOffset := (ElementTerminal - 1) * MonitoredElement.NConds; // for speedy sampling
    end;
    inherited;
end;

procedure TStorageController2Obj.DoPendingAction(const Code, ProxyHdl: Integer);
begin
    // Release  the discharge inhibit .
    // Do nothing for other codes

    if (Code = RELEASE_INHIBIT) and (DischargeMode <> MODEFOLLOW) then
        DischargeInhibited := FALSE;
end;

procedure TStorageController2Obj.DoScheduleMode();
{
  In SCHEDULE mode we ramp up the storage from zero to the specified pctkWRate.
  This value is held for the flattime or until they  turn themselves
  off when they are either fully discharged, or ramped down

  The discharge trigger time must be greater than 0
}

var
    TDiff: Double;
    pctDischargeRate: Double;
begin
    pctDischargeRate := 0.0;   // init for test
    if (DisChargeTriggerTime > 0.0) then
        with ActiveCircuit.Solution do
        begin
               // turn on if time within 1/2 time step
            if not (FleetState = STORE_DISCHARGING) then
            begin
                ChargingAllowed := TRUE;
                TDiff := NormalizeToTOD(DynaVars.intHour, DynaVars.t) - DisChargeTriggerTime;
                if abs(TDiff) < DynaVars.h / 7200.0 then
                begin
                        {Time is within 1 time step of the trigger time}
                    if ShowEventLog then
                        AppendToEventLog('StorageController.' + Self.Name, 'Fleet Set to Discharging (up ramp) by Schedule');
                    SetFleetToDischarge;
                    SetFleetDesiredState(STORE_DISCHARGING);
                    ChargingAllowed := FALSE;
                    pctDischargeRate := min(pctkWRate, max(pctKWRate * Tdiff / UpRampTime, 0.0));
                    SetFleetkWRate(pctDischargeRate);
                    DischargeInhibited := FALSE;
                    PushTimeOntoControlQueue(STORE_DISCHARGING);
                end;
            end
            else
            begin    // fleet is already discharging
                TDiff := NormalizeToTOD(DynaVars.intHour, DynaVars.t) - DisChargeTriggerTime;
                if TDiff < UpRampTime then
                begin
                    pctDischargeRate := min(pctkWRate, max(pctKWRate * Tdiff / UpRampTime, 0.0));
                    SetFleetDesiredState(STORE_DISCHARGING);

                    if pctDischargeRate <> LastpctDischargeRate then
                    begin
                        SetFleetkWRate(pctDischargeRate);
                        SetFleetToDischarge;
                    end;

                end
                else
                begin
                    if TDiff < UpPlusFlat then
                    begin
                        pctDischargeRate := pctkWRate;
                        SetFleetDesiredState(STORE_DISCHARGING);
                        if PctDischargeRate <> LastpctDischargeRate then
                            SetFleetkWRate(pctkWRate);  // on the flat part

                    end
                    else
                    if TDiff > UpPlusFlatPlusDn then
                    begin
                        SetFleetToIdle;
                        ChargingAllowed := TRUE;
                        pctDischargeRate := 0.0;
                        if ShowEventLog then
                            AppendToEventLog('StorageController.' + Self.Name, 'Fleet Set to Idling by Schedule');

                    end
                    else
                    begin  // We're on the down ramp

                        TDiff := UpPlusFlatPlusDn - TDiff;
                        pctDischargeRate := max(0.0, min(pctKWRate * Tdiff / DnRampTime, pctKWRate));
                        SetFleetDesiredState(STORE_DISCHARGING);
                        SetFleetkWRate(pctDischargeRate);

                    end;

                end;

                if pctDischargeRate <> LastpctDischargeRate then
                    PushTimeOntoControlQueue(STORE_DISCHARGING);

            end;  {If not fleetstate ...}
        end;
    LastpctDischargeRate := pctDischargeRate;   // remember this value
end;

procedure TStorageController2Obj.DoTimeMode(Opt: Integer);
{
  In Time mode we need to only turn the storage elements on. They will turn themselves
  off when they are either fully discharged, fully charged, or receive another command
  from the controller
}
var
    RemainingkWh,
    ReservekWh,
    TotalRatingkWh: Double;
begin
    TotalRatingkWh := FleetkWhRating;
    RemainingkWh := Get_FleetkWh();
    ReservekWh := FleetReservekWh;


    case Opt of

        1:
        begin
            if (DisChargeTriggerTime > 0.0) then
                with ActiveCircuit.Solution do
                begin
                 // turn on if time within 1/2 time step
                    if abs(NormalizeToTOD(DynaVars.intHour, DynaVars.t) - DisChargeTriggerTime) < DynaVars.h / 7200.0 then
                    begin
                        SetFleetDesiredState(STORE_DISCHARGING);
                        if not (FleetState = STORE_DISCHARGING) and (RemainingkWh > ReservekWh) then
                        begin
                        {Time is within 1 time step of the trigger time}
                            if ShowEventLog then
                                AppendToEventLog('StorageController.' + Self.Name, 'Fleet Set to Discharging by Time Trigger');
                            SetFleetToDischarge;
                            SetFleetkWRate(pctKWRate);
                            DischargeInhibited := FALSE;
                            if DischargeMode = MODEFOLLOW then
                                DischargeTriggeredByTime := TRUE
                            else
                                PushTimeOntoControlQueue(STORE_DISCHARGING);
                        end;
                    end
                    else
                        ChargingAllowed := TRUE;
                end;
        end; // Discharge mode
        2:
        begin
            if ChargeTriggerTime > 0.0 then
                with ActiveCircuit.Solution do
                begin
                    if abs(NormalizeToTOD(DynaVars.intHour, DynaVars.t) - ChargeTriggerTime) < DynaVars.h / 7200.0 then
                    begin
                        SetFleetDesiredState(STORE_CHARGING);
                        if not (FleetState = STORE_CHARGING) and (RemainingkWh < TotalRatingkWh) then
                        begin
                          {Time is within 1 time step of the trigger time}
                            if ShowEventLog then
                                AppendToEventLog('StorageController.' + Self.Name, 'Fleet Set to Charging by Time Trigger');
                            SetFleetToCharge;
                            DischargeInhibited := TRUE;
                            OutOfOomph := FALSE;
                            PushTimeOntoControlQueue(STORE_CHARGING);   // force re-solve at this time step
                          // Push message onto control queue to release inhibit at a later time
                            with ActiveCircuit do
                            begin
                                Solution.LoadsNeedUpdating := TRUE; // Force recalc of power parms
                                ControlQueue.Push(DynaVars.intHour + InhibitHrs, Dynavars.t, RELEASE_INHIBIT, 0, Self);
                            end;
                        end;
                    end;
                end;
        end; //Charge mode
    end;
end;

function TStorageController2Obj.NormalizeToTOD(h: Integer; sec: Double): Double;
// Normalize time to a floating point number representing time of day If Hour > 24
// time should be 0 to 23.999999....
var
    HourOfDay: Integer;

begin
    if h > 23 then
        HourOfDay := (h - (h div 24) * 24)
    else
        HourOfDay := h;

    Result := HourOfDay + sec / 3600.0;

    if Result >= 24.0 then
        Result := Result - 24.0;   // Wrap around

end;


procedure TStorageController2Obj.PushTimeOntoControlQueue(Code: Integer);
{
   Push present time onto control queue to force re solve at new dispatch value
}
begin
    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        LoadsNeedUpdating := TRUE; // Force recalc of power parms
        ControlQueue.Push(DynaVars.intHour, DynaVars.t, Code, 0, Self);
    end;
end;

function TStorageController2Obj.Get_DynamicTarget(THigh: Integer): Double;
var
    // Temp, temp2: Double;
    RatingIdx: Integer = 0;
    RSignal: TXYCurveObj;
begin
    if DSS.SeasonSignal <> '' then
    begin
        RSignal := DSS.XYCurveClass.Find(DSS.SeasonSignal);
        if RSignal <> NIL then
            RatingIdx := trunc(RSignal.GetYValue(ActiveCircuit.Solution.DynaVars.intHour));

        if (RatingIdx <= Seasons) and (Seasons > 1) then
        begin
            if THigh = 1 then
                Result := SeasonTargets[RatingIdx]
            else
                Result := SeasonTargetsLow[RatingIdx]
        end
        else
        begin
            if THigh = 1 then
                Result := FkWTarget
            else
                Result := FkWTargetLow
        end;
    end;
end;


procedure TStorageController2Obj.DoLoadFollowMode();

var
    i: Integer;
    S: Complex;
    StorageObj: TStorageObj;
    StorekWChanged,
    StorekvarChanged,
    SkipkWDispatch: Boolean;
    VoltsArr: pComplexArray;
    kWhActual,
    ElemVolts,
    Amps,
    AmpsDiff,
    PDiff,
//   PFDiff,
    DispatchkW,
//   Dispatchkvar,
    RemainingkWh,
    CtrlTarget,
    ReservekWh,
    ActualkWDispatch: Double;

begin
     // If list is not defined, go make one from all storage elements in circuit
    if FleetPointerList.Count = 0 then
        MakeFleetList;

    if FleetSize > 0 then
    begin
        StorekWChanged := FALSE;
        StorekvarChanged := FALSE;
        SkipkWDispatch := FALSE;

       //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
        if DischargeMode = CURRENTPEAKSHAVE then
        begin
            MonitoredElement.GetCurrents(cBuffer);
            GetControlCurrent(Amps);

//          Amps := MonitoredElement.MaxCurrent[ElementTerminal]; // Max current in active terminal  // old
        end
        else
            GetControlPower(S);

       // In case of having seasonal targets
        if DSS.SeasonalRating then
            CtrlTarget := Get_DynamicTarget(1)
        else
            CtrlTarget := FkWTarget;


        case DischargeMode of
             // Following Load; try to keep load below kW Target
            MODEFOLLOW:
            begin
                if DischargeTriggeredByTime then
                begin
                    if ShowEventLog then
                        AppendToEventLog('StorageController.' + Self.Name,
                            Format('Fleet Set to Discharging by Time Trigger; Old kWTarget = %-.6g; New = %-.6g', [FkwTarget, S.re * 0.001]));
                    FkwTarget := Max(FkWThreshold, S.re * 0.001);  // Capture present kW and reset target
                    if not FkWBandSpecified then
                        HalfkWBand := FpctkWBand / 200.0 * FkWTarget;  // Update band to new target if absolute kWBand hasn`t been specified
                    DischargeTriggeredByTime := FALSE;  // so we don't come back in here right away
                    SetFleetToIdle;
                    SetFleetDesiredState(STORE_IDLING);
                end;
                PDiff := S.re * 0.001 - FkWTarget;  // Assume S.re is normally positive
//                                  PFDiff        := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for peak shaving
            end;
             // supporting DG; Try to keep load above kW target
            MODESUPPORT:
            begin
                PDiff := S.re * 0.001 + FkWTarget;  // assume S.re is normally negative
//                                  PFDiff        := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for generator
            end;

            MODEPEAKSHAVE:
            begin
                PDiff := S.re * 0.001 - CtrlTarget;  // Assume S.re is normally positive
//                                  PFDiff        := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for peak shaving
            end;

            CURRENTPEAKSHAVE:
            begin
                PDiff := Amps - CtrlTarget * 1000;  // Gets the difference in terms of amps
//                                  DispatchVars  :=  False;
            end;
        else
            PDiff := 0.0;
        end;

        if Dischargemode = CURRENTPEAKSHAVE then    // convert Pdiff from Amps to kW
        begin
            MonitoredElement.ComputeVterminal();
            VoltsArr := MonitoredElement.Vterminal;
            ElemVolts := cabs(VoltsArr^[1]);
            kWNeeded := ((MonitoredElement.NPhases * Pdiff * ElemVolts) / 1000.0);
//         kWNeeded     :=  ((Pdiff * ElemVolts) / 1000.0);
            AmpsDiff := PDiff;
        end
        else
//         kWNeeded := Pdiff + FleetkW;
            kWNeeded := Pdiff;

       {  kW dispatch  }

       // Check if Fleet is Idling (FleetState is updated only if entire fleet is idling)
        if not (FleetState = STORE_IDLING) then
        begin
            for i := 1 to FleetSize do
            begin
                StorageObj := FleetPointerList.Get(i);
                if StorageObj.StorageState <> STORE_IDLING then
                    Break;
                if i = FleetSize then
                    FleetState := STORE_IDLING;
            end;
        end;

        if DischargeInhibited then
            SkipkWDispatch := TRUE
        else
        begin
//         If FleetState = STORE_CHARGING Then
//                   Begin
//                     if Not (Dischargemode = CURRENTPEAKSHAVE) then
//                      Pdiff :=  Pdiff + FleetkW  // ignore overload due to charging
//                     else
//                     Begin
//                       MonitoredElement.ComputeVterminal();
//                       VoltsArr     :=  MonitoredElement.Vterminal;
//                       ElemVolts    :=  cabs(VoltsArr^[1]);
//                       Pdiff        :=  Pdiff + (FleetkW * 1000 / ElemVolts);
//                     End;
//                   end;

//           If Not (FleetState = STORE_DISCHARGING) Then  // ignore overload due to charging or idling (trickle charging) - FleetkW < 0
            if (FleetState = STORE_CHARGING) then
            begin
                if not (DischargeMode = CURRENTPEAKSHAVE) then
                    Pdiff := Pdiff + Get_FleetkW()
                else
                begin
                    MonitoredElement.ComputeVterminal();
                    VoltsArr := MonitoredElement.Vterminal;
                    ElemVolts := cabs(VoltsArr^[1]);
                    Pdiff := Pdiff + (Get_FleetkW() * 1000 / (ElemVolts * MonitoredElement.NPhases));
//                 Pdiff        :=  Pdiff + (Get_FleetkW() * 1000 / (ElemVolts ));
                end;

            end;

            case FleetState of
                STORE_CHARGING,
                STORE_IDLING:
                    if (PDiff - HalfkWBand < 0.0) or OutOfOomph then
                    begin  // Don't bother trying to dispatch
                        ChargingAllowed := TRUE;
                        SkipkWDispatch := TRUE;
                        if OutofOomph then  // --------------------------------- new 04/20/2020 ----------
                        begin
                            for i := 1 to FleetSize do
                            begin
                                StorageObj := FleetPointerList.Get(i);
                                kWhActual := StorageObj.StorageVars.kWhStored / StorageObj.StorageVars.kWhRating;
                                OutOfOomph := OutOfOomph and (kWhActual >= ResetLevel);  // If we have more than the 80% we are good to dispatch
                            end;
                            OutOfOomph := not OutOfOomph;  // If everybody in the fleet has at least the 80% of the storage capacity full

                        end;    // -----------------------------------------------------------------------
                    end;
//                STORE_DISCHARGING: If ((PDiff + Get_FleetkW()) < 0.0)  or OutOfOomph Then
//                STORE_DISCHARGING: If (((PDiff + Get_FleetkW()) < 0.0) and (abs(PDiff) > HalfkWBand)) or OutOfOomph Then // CR: set to idle only if out of band
//                  Begin   // desired decrease is greater then present output; just cancel
//                        If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name,
//                        Format('Desired decrease is greater than present output. Pdiff = %-.6g, FleetkW = %-.6g. Setting Fleet to Idle', [PDiff, Get_FleetkW()]));
//                        SetFleetToIdle;   // also sets presentkW = 0
//                        For i := 1 to FleetSize Do Begin TStorageObj(FleetPointerList.Get(i)).SetNominalStorageOutput() End; // To Update Current kvarLimit
//                        PushTimeOntoControlQueue(STORE_IDLING);  // force a new power flow solution
//                        ChargingAllowed := TRUE;
//                        SkipkWDispatch  := TRUE;
//                        Wait4Step       := TRUE; // To tell to the charging section to wait for the next sim step
//                                                 // useful when workin with large simulation time steps
//                  End;
            end;
        end;


        if not SkipkWDispatch then
        begin
            RemainingkWh := Get_FleetkWh();
            ReservekWh := FleetReservekWh;
            if (RemainingkWh > ReservekWh) then
            begin
               //  don't dispatch kW  if not enough storage left or an endless control loop will occur
                if abs(PDiff) > HalfkWBand then
                begin // Attempt to change storage dispatch
                    if not (FleetState = STORE_DISCHARGING) then
                    begin
                        SetFleetToDischarge;
//                    StorekWChanged:= TRUE;  // if not already discharging, force new power flow.
                    end;
                    if ShowEventLog then
                        AppendToEventLog('StorageController.' + Self.Name, Format('Attempting to dispatch %-.6g kW with %-.6g kWh remaining and %-.6g kWh reserve.', [kWNeeded, RemainingkWh, ReservekWh]));
                    for i := 1 to FleetSize do
                    begin
                        StorageObj := FleetPointerList.Get(i);

                        if Dischargemode = CURRENTPEAKSHAVE then // Current to power
                        begin    //  (MonitoredElement.MaxVoltage[ElementTerminal] / 1000)
                            if StorageObj.NPhases = 1 then
                                kWNeeded := StorageObj.PresentkV * AmpsDiff
                            else
//                          kWNeeded :=  StorageObj.PresentkV * InvSQRT3 * AmpsDiff;
                                kWNeeded := StorageObj.PresentkV * SQRT3 * AmpsDiff;
                        end;

                        with StorageObj do
                        begin
                      // compute new dispatch value for this storage element ...
                            DispatchkW := Min(StorageVars.kWrating, (PresentkW + kWNeeded * DispFactor * (FWeights^[i] / TotalWeight))); // Dispatch kWNeeded

                            if DispatchkW <= 0.0 then // if kWNeeded is too low, DispatchkW may be negative depending on idling losses. In this case, just set it to idling
                            begin
                                StorageState := STORE_IDLING;  // overrides SetFleetToDischarge

                                if (abs(PresentkW) - StorageObj.kWOutIdling > EPSILON) then  // if not already idling
                                begin
                                    SetNominalStorageOutput();
                                    ActualkWDispatch := PresentkW;
                                    StorekWChanged := TRUE; // if not idling at first, force a new powerflow

                                    if ShowEventLog then
                                        AppendToEventLog('StorageController.' + Self.Name,
                                            Format('Requesting ' + StorageObj.FullName + ' to dispatch %-.6g kW. Setting ' + StorageObj.FullName + ' to idling state. Final kWOut is %-.6g kW', [DispatchkW, ActualkWDispatch]));
                                end
//                          DispatchkW := 0.0;

                            end
                            else
                            begin
                                if abs(kW - DispatchkW) / abs(DispatchkW) > 0.0001 then // redispatch only if change requested
                                begin
                                    if DispatchkW < Max(CutInkWAC, CutOutkWAC) then   // Necessary check to avoid the control to go into an infinite loop when DispatchkW is less than CutOutkWAC
                                    begin
                                        if InverterON = TRUE then  // request Dispatch only if the inverter is on (only once).
                                        begin
                                // Next time, the inverter will be OFF and the control won't dispatch a new power
                                            if StorageVars.kWhStored > StorageVars.kWhReserve then
                                            begin
                                                kW := DispatchkW;
                                                SetNominalStorageOutput();
                                                ActualkWDispatch := PresentkW;
                                                StorekWChanged := TRUE;     // This is what keeps the control iterations going

                                                if ShowEventLog then
                                                    AppendToEventLog('StorageController.' + Self.Name,
                                                        Format('Requesting ' + StorageObj.FullName + ' to dispatch %-.6g kW, less than CutIn/CutOut.' + ' Final kWOut is %-.6g kW', [DispatchkW, ActualkWDispatch]));
                                            end;
                                        end
                                        else
                                        begin
                                      // if inverter is already off, just override discharging state to
                                      // idling and update current kvarlimit for usage by InvControl

                                            StorageState := STORE_IDLING;     // overrides SetFleetToDischarge
                                            SetNominalStorageOutput(); // to update current kvarLimit
                                            ActualkWDispatch := PresentkW;
                                            if ShowEventLog then
                                                AppendToEventLog('StorageController.' + Self.Name,
                                                    Format('Requesting ' + StorageObj.FullName + ' to dispatch %-.6g kW, less than CutIn/CutOut.' + ' Inverter is OFF. Final kWOut is %-.6g kW', [DispatchkW, ActualkWDispatch]));
                                        end
                                    end
                                    else
                                    if StorageVars.kWhStored > StorageVars.kWhReserve then
                                    begin  // Attempt to set discharge kW;  Storage element will revert to idling if out of capacity

                                        kW := DispatchkW;
                                        SetNominalStorageOutput();
                                        ActualkWDispatch := PresentkW;
                                        StorekWChanged := TRUE;     // This is what keeps the control iterations going

                                        if ShowEventLog then
                                            AppendToEventLog('StorageController.' + Self.Name,
                                                Format('Requesting ' + StorageObj.FullName + ' to dispatch %-.6g kW. Final kWOut is %-.6g kW',
                                                [DispatchkW, ActualkWDispatch]));
                                    end;

                                end;

                            end;

                        end;
                    end;
                end
            end
            else
            begin
                if not FleetState = STORE_IDLING then
                begin
                    SetFleetToIdle;
                    PushTimeOntoControlQueue(STORE_IDLING);  // force a new power flow solution
                end;
                ChargingAllowed := TRUE;
                OutOfOomph := TRUE;
                if ShowEventLog then
                    AppendToEventLog('StorageController.' + Self.Name,
                        Format('Ran out of OOMPH: %-.6g kWh remaining and %-.6g reserve. Fleet has been set to idling state.', [RemainingkWh, ReservekWh]));
            end;
        end;

        if StorekWChanged or StorekvarChanged then  // Only push onto controlqueue If there has been a change
            PushTimeOntoControlQueue(STORE_DISCHARGING);

       {Else just continue}
    end;
end;

procedure TStorageController2Obj.DoPeakShaveModeLow();
    // This is the peakShaving mode for controlling the charging operation of the storage fleet
    // The objective is to charge the storage fleet when the power at a monitored element is bellow a specified KW target (kWTarget_low)
    // The storage will charge as much power as necessary to keet the power within the deadband around kWTarget_low

  // WILL NOT IMPLEMENT REACTIVE POWER CONTROL FOR NOW
var
    i: Integer;
    S: Complex;
    VoltsArr: PComplexArray;
    StorageObj: TStorageObj;
    StorekWChanged,
    SkipkWCharge: Boolean;
    ElemVolts,
    PDiff,
    kWNeeded,
    Amps,
    AmpsDiff,
    ChargekW,
    ActualkWh,
    // ActualkW,
    TotalRatingkWh,
    // KwtoPercentagekW,
    CtrlTarget,
    ActualkWDispatch: Double;

begin
     // If list is not defined, go make one from all storage elements in circuit
    if FleetPointerList.Count = 0 then
        MakeFleetList;

//     If (FleetSize>0) And(Not(FleetState = STORE_DISCHARGING)) Then
    if (FleetSize > 0) then
    begin
        StorekWChanged := FALSE;
        SkipkWCharge := FALSE;


        if DSS.SeasonalRating then
            CtrlTarget := Get_DynamicTarget(0)
        else
            CtrlTarget := FkWTargetLow;


       //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
        if Chargemode = CURRENTPEAKSHAVELOW then
        begin
            MonitoredElement.GetCurrents(cBuffer);
            GetControlCurrent(Amps);
//         Amps         := MonitoredElement.MaxCurrent[ElementTerminal]; // Max current in active terminal
            PDiff := Amps - CtrlTarget * 1000;  // Gets the difference in terms of amps
        end
        else
        begin
            GetControlPower(S);
            PDiff := S.re * 0.001 - CtrlTarget;  // Assume S.re is normally positive
        end;

        // ActualkW := Get_FleetkW();
        ActualkWh := Get_FleetkWh();
        TotalRatingkWh := FleetkWhRating;

        if Chargemode = CURRENTPEAKSHAVELOW then   // convert Pdiff from Amps to kW
        begin
            MonitoredElement.ComputeVterminal();
            VoltsArr := MonitoredElement.Vterminal;
            ElemVolts := cabs(VoltsArr^[1]);     // LN voltage
            kWNeeded := ((MonitoredElement.NPhases * PDiff * ElemVolts) / 1000.0);
//         kWNeeded     :=  (( PDiff * ElemVolts) / 1000.0);
            AmpsDiff := PDiff;
        end
        else
//         kWNeeded := Pdiff + Get_FleetkW();
            kWNeeded := Pdiff;


        // Check if Fleet is Idling (FleetState is updated only if entire fleet is idling)
        if not (FleetState = STORE_IDLING) then
        begin
            for i := 1 to FleetSize do
            begin
                StorageObj := FleetPointerList.Get(i);
                if StorageObj.StorageState <> STORE_IDLING then
                    Break;
                if i = FleetSize then
                    FleetState := STORE_IDLING;
            end;
        end;

//
//           CASE  FleetState of
//                STORE_CHARGING,
//                STORE_IDLING: If (PDiff < 0.0) or OutOfOomph Then
//                  Begin  // Don't bother trying to dispatch
//                       ChargingAllowed  := TRUE;
//                       SkipkWDispatch   := TRUE;
//                  End;
//           END;


//       If Not (FleetState = STORE_CHARGING) Then  // ignore underload due to discharging  (FleetkW > 0) and discount idlings losses (may delay the charging)
        if (FleetState = STORE_DISCHARGING) then
        begin
            if not (ChargeMode = CURRENTPEAKSHAVELOW) then
                Pdiff := Pdiff + Get_FleetkW()
            else
            begin
                MonitoredElement.ComputeVterminal();
                VoltsArr := MonitoredElement.Vterminal;
                ElemVolts := cabs(VoltsArr^[1]);
                Pdiff := Pdiff + (Get_FleetkW() * 1000 / (ElemVolts * MonitoredElement.NPhases));   // get actual Pdiff in Currents (discount FleetkW)  (assuming same number of phases of Fleet and Monitored Element)
//                 Pdiff        :=  Pdiff + (Get_FleetkW() * 1000 / (ElemVolts ));
            end;

        end;

        case FleetState of
            STORE_DISCHARGING,
            STORE_IDLING:
                if (PDiff > 0.0) or (ActualkWh >= TotalRatingkWh) or Wait4Step then
                begin  // Don't bother trying to charge
                    ChargingAllowed := FALSE;
                    SkipkWCharge := TRUE;
                    Wait4Step := FALSE;
                end
//                        End;
//          STORE_CHARGING: If (kWNeeded > 0.0) or (ActualkWh>=TotalRatingkWh) // old approach
//          STORE_CHARGING: If (Pdiff + Get_FleetkW() > 0.0) or (ActualkWh >= TotalRatingkWh) Then
//                          Begin   // desired decrease (in absolute value) is greater than present output; just cancel
//                                SetFleetToIdle;   // also sets presentkW = 0
//                                PushTimeOntoControlQueue(STORE_IDLING);  // force a new power flow solution
//                                ChargingAllowed := FALSE;
//                                SkipkWCharge  := TRUE;
//                          End;
        end;

        if not SkipkWCharge then
        begin
            if (ActualkWh < TotalRatingkWh) then
            begin
               //  don't dispatch kW  if fully charged or an endless control loop will occur
                if abs(PDiff) > HalfkWBandLow then
                begin // Attempt to change storage kW charge
                    if not (FleetState = STORE_CHARGING) then
                    begin
                        SetFleetToCharge;
//                      StorekWChanged:= TRUE;  // if not already charging, force new power flow.
                    end;
  //                       If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name, Format('Attempting to charge %-.6g kW with %-.6g kWh remaining and %-.6g rating.', [kWNeeded, (TotalRatingkWh-ActualkWh), TotalRatingkWh]));
                    if ShowEventLog then
                        AppendToEventLog('StorageController.' + Self.Name, Format('Attempting to charge %-.6g kW with %-.6g kWh remaining and %-.6g rating.', [kWNeeded, (TotalRatingkWh - ActualkWh), TotalRatingkWh]));
                    for i := 1 to FleetSize do
                    begin
                        StorageObj := FleetPointerList.Get(i);
                        with StorageObj do
                        begin
                        // Checks if PDiff needs to be adjusted considering the charging mode
                            if Chargemode = CURRENTPEAKSHAVELOW then
                            begin
                                if StorageObj.NPhases = 1 then
                                    kWNeeded := StorageObj.PresentkV * AmpsDiff
                                else
//                           kWNeeded :=  StorageObj.PresentkV * InvSQRT3 * AmpsDiff;
                                    kWNeeded := StorageObj.PresentkV * SQRT3 * AmpsDiff;
                            end;


                       // compute new charging value for this storage element ...
  //                                ChargekW := -1 * Min(StorageVars.kWrating, abs(PresentkW + Pdiff *(FWeights^[i]/TotalWeight)));  // old approach

                            ChargekW := PresentkW + kWNeeded * (FWeights^[i] / TotalWeight) * DispFactor; // may be positive or negative
                            if ChargekW < 0 then
                                ChargekW := Max(-1 * StorageVars.kWrating, ChargekW); // check against kVA rating

                            if ChargekW >= 0 then // chargekW may be positive if increase in demand is too high.
                            begin
                                StorageState := STORE_IDLING;  // overrides SetFleetToDischarge

                                if (abs(PresentkW) - StorageObj.kWOutIdling > EPSILON) then  // if not already idling
                                begin
                                    SetNominalStorageOutput();
                                    ActualkWDispatch := PresentkW;
                                    StorekWChanged := TRUE; // if not idling at first, force a new powerflow

                                    if ShowEventLog then
                                        AppendToEventLog('StorageController.' + Self.Name,
                                            Format('Requesting ' + StorageObj.FullName + ' to dispatch %-.6g kW. Setting ' + StorageObj.FullName + ' to idling state. Final kWOut is %-.6g kW', [ChargekW, ActualkWDispatch]));
                                end

                            end
                            else
                            begin
                          //                       If ChargekW <> PresentkW Then    // do only if change requested
                                if abs(StorageObj.kW - ChargekW) / abs(ChargekW) > 0.0001 then    // do only if change requested
                                begin
                                    if abs(ChargekW) < Max(CutInkWAC, CutOutkWAC) then   // Necessary check to avoid the control to go into an infinite loop when ChargekW is less than CutOutkWAC
                                    begin
                                        if InverterON = TRUE then  // request Dispatch only if the inverter is on (only once).
                                        begin
                                // Next time the inverter will be OFF and the control won't dispatch a new power
                                            if StorageVars.kWhStored > StorageVars.kWhReserve then
                                            begin
                                                kW := ChargekW;
                                                SetNominalStorageOutput();
                                                ActualkWDispatch := PresentkW;
                                                StorekWChanged := TRUE;     // This is what keeps the control iterations going

                                                if ShowEventLog then
                                                    AppendToEventLog('StorageController.' + Self.Name,
                                                        Format('Requesting ' + StorageObj.FullName + ' to dispatch %-.6g kW, less than CutIn/CutOut.' + ' Final kWOut is %-.6g kW', [ChargekW, ActualkWDispatch]));
                                            end;
                                        end
                                        else
                                        begin
                                      // if inverter is already off, just override discharging state to
                                      // idling and update current kvarlimit for usage by InvControl

                                            StorageState := STORE_IDLING;     // overrides SetFleetToCharge
                                            SetNominalStorageOutput(); // to update current kvarLimit
                                            ActualkWDispatch := PresentkW;
                                            if ShowEventLog then
                                                AppendToEventLog('StorageController.' + Self.Name,
                                                    Format('Requesting ' + StorageObj.FullName + ' to dispatch %-.6g kW, less than CutIn/CutOut.' + ' Inverter is OFF. Final kWOut is %-.6g kW', [ChargekW, ActualkWDispatch]));
                                        end
                                    end
                                    else
                                    if StorageVars.kWhStored < StorageVars.kWhRating then
                                    begin  // Attempt to set discharge kW;  Storage element will revert to idling if out of capacity
                                     //StorageObj.PresentkW  :=  ChargekW;
                                        kW := ChargekW;
                                        SetNominalStorageOutput();
                                        ActualkWDispatch := PresentkW;
        //                                           KwtoPercentagekW := (ChargekW*100) / StorageVars.kWrating;  // old approach
        //                                           StorageObj.pctkWin := abs(KwtoPercentagekW);                // old approach
                                        StorekWChanged := TRUE;     // This is what keeps the control iterations going

                                        if ShowEventLog then
                                            AppendToEventLog('StorageController.' + Self.Name,
                                                Format('Requesting ' + StorageObj.FullName + ' to dispatch %-.6g kW. Final kWOut is %-.6g kW',
                                                [ChargekW, ActualkWDispatch]));

                                    end;

                                end;
                            end;

                        end;
                    end;
                end
            end
            else
            begin
                if not FleetState = STORE_IDLING then
                begin
                    SetFleetToIdle;
                    PushTimeOntoControlQueue(STORE_IDLING);  // force a new power flow solution
                end;
                ChargingAllowed := FALSE;
                if ShowEventLog then
                    AppendToEventLog('StorageController.' + Self.Name, Format('Fully charged: %-.6g kWh of rated %-.6g.', [ActualkWh, TotalRatingkWh]));
            end;
        end;

        if StorekWChanged then  // Only push onto controlqueue If there has been a change
            PushTimeOntoControlQueue(STORE_CHARGING);
       {Else just continue}
    end;
end;

procedure TStorageController2Obj.Sample();

begin
    ChargingAllowed := FALSE;
//       UpdateFleetState;
{
  Check discharge mode first. Then if not discharging, we can check for charging
}
    Wait4Step := FALSE;        // Initializes the variable for the new control step
    case DischargeMode of
        MODEFOLLOW:
        begin
            DoTimeMode(1);
            DoLoadFollowMode();
        end;
        MODELOADSHAPE:
            DoLoadShapeMode();
        MODESUPPORT:
            DoLoadFollowMode();
        MODETIME:
            DoTimeMode(1);
        MODEPEAKSHAVE:
            DoLoadFollowMode();
        CURRENTPEAKSHAVE:
            DoLoadFollowMode();
        MODESCHEDULE:
            DoScheduleMode();
    else
        DoSimpleMsg('Invalid DisCharging Mode: %d', [DisChargeMode], 14408);
    end;

    if ChargingAllowed then
        case ChargeMode of
            MODELOADSHAPE: ; // DoLoadShapeMode;  already executed above
            MODETIME:
                DoTimeMode(2);
            MODEPEAKSHAVELOW:
                DoPeakShaveModeLow();
            CURRENTPEAKSHAVELOW:
                DoPeakShaveModeLow()
        else
            DoSimpleMsg('Invalid Charging Mode: %d', [ChargeMode], 14409);
        end;
end;


procedure TStorageController2Obj.CalcDailyMult(Hr: Double);

begin
    if (DailyShapeObj <> NIL) then
    begin
        LoadShapeMult := DailyShapeObj.GetMultAtHour(Hr);
    end
    else
        LoadShapeMult := CDoubleOne;  // Default to no  variation
end;


procedure TStorageController2Obj.CalcDutyMult(Hr: Double);

begin
    if DutyShapeObj <> NIL then
    begin
        LoadShapeMult := DutyShapeObj.GetMultAtHour(Hr);
    end
    else
        CalcDailyMult(Hr);  // Default to Daily Mult If no duty curve specified
end;

procedure TStorageController2Obj.CalcYearlyMult(Hr: Double);

begin
    if YearlyShapeObj <> NIL then
    begin
        LoadShapeMult := YearlyShapeObj.GetMultAtHour(Hr);
    end
    else
        CalcDailyMult(Hr);  // Defaults to Daily curve
end;

procedure TStorageController2Obj.DoLoadShapeMode();
var
    FleetStateSaved: Integer;
    RateChanged: Boolean;
    NewChargeRate: Double;
    NewkWRate: Double;
    //NewkvarRate: Double;
begin
    FleetStateSaved := FleetState;
    RateChanged := FALSE;

    // Get multiplier

    with ActiveCircuit.Solution do
        case Mode of
            TSolveMode.DAILYMODE:
                CalcDailyMult(DynaVars.dblHour); // Daily dispatch curve
            TSolveMode.YEARLYMODE:
                CalcYearlyMult(DynaVars.dblHour);
            TSolveMode.LOADDURATION2:
                CalcDailyMult(DynaVars.dblHour);
            TSolveMode.PEAKDAY:
                CalcDailyMult(DynaVars.dblHour);
            TSolveMode.DUTYCYCLE:
                CalcDutyMult(DynaVars.dblHour);
        end;

    if LoadShapeMult.re < 0.0 then
    begin
        ChargingAllowed := TRUE;
        NewChargeRate := Abs(LoadShapeMult.re) * 100.0;
        SetFleetDesiredState(STORE_CHARGING);

        if NewChargeRate <> pctChargeRate then
        begin
            RateChanged := TRUE;
            pctChargeRate := NewChargeRate;
            SetFleetChargeRate;
            SetFleetToCharge;
        end;
    end

    else
    if LoadShapeMult.re = 0.0 then
        SetFleetToIdle
    else
    begin   // Set fleet to discharging at a rate
        NewkWRate := LoadShapeMult.re * 100.0;
//           NewkvarRate := LoadShapeMult.im * 100.0;
        SetFleetDesiredState(STORE_DISCHARGING);

//           If (NewkWRate <> pctkWRate) or (NewkvarRate <> pctkvarRate) then
        if (NewkWRate <> pctkWRate) then
           // only set rate if it has changed. otherwise the debugtrace report will not report kWOut correctly.
        begin
            RateChanged := TRUE;
            pctkWRate := NewkWRate;
//              pctkvarRate := NewkvarRate;
            SetFleetkWRate(pctKWRate);
//              SetFleetkvarRate(pctkvarRate);
            SetFleetToDischarge;

            ActiveCircuit.Solution.LoadsNeedUpdating := TRUE; // Force recalc of power parms
        end;
    end;

    {Force a new power flow solution if fleet state has changed}
    if (FleetState <> FleetStateSaved) or RateChanged then
        PushTimeOntoControlQueue(0);
end;

procedure TStorageController2Obj.SetAllFleetValues;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        with TStorageObj(FleetPointerList.Get(i)) do
        begin
            pctkWin := pctChargeRate;
//              Fpctkvarout := pctkvarRate;  CR
            pctkWout := pctkWRate;
            pctReserve := pctFleetReserve;
        end;
end;

procedure TStorageController2Obj.SetFleetChargeRate;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        TStorageObj(FleetPointerList.Get(i)).pctkWin := pctChargeRate;
end;

//PROCEDURE TStorageController2Obj.SetFleetkvarRate;
//VAR
//      i   :Integer;
//Begin
//    {For side effects see pctkvarout property of Storage element}
////      For i := 1 to FleetPointerList.Count Do
////            TStorageObj(FleetPointerList.Get(i)).pctkvarout := pctkvarRate;
//End;

procedure TStorageController2Obj.SetFleetkWRate(pctkw: Double);
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        TStorageObj(FleetPointerList.Get(i)).pctkWout := pctkw;
end;

procedure TStorageController2Obj.SetFleetToCharge;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        TStorageObj(FleetPointerList.Get(i)).StorageState := STORE_CHARGING;
    FleetState := STORE_CHARGING;
end;

procedure TStorageController2Obj.SetFleetToDisCharge;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        TStorageObj(FleetPointerList.Get(i)).StorageState := STORE_DISCHARGING;
    FleetState := STORE_DISCHARGING;
end;

procedure TStorageController2Obj.SetFleetToIdle;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        with TStorageObj(FleetPointerList.Get(i)) do
        begin
            StorageState := STORE_IDLING;
//                  PresentkW := 0.0;
            kW := 0.0;
        end;
    FleetState := STORE_IDLING;
end;

//-----------------------------------------------------------------------------

procedure TStorageController2Obj.SetFleetDesiredState(state: Integer);
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        TStorageObj(FleetPointerList.Get(i)).StateDesired := state;
end;
procedure TStorageController2Obj.SetFleetToExternal;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        TStorageObj(FleetPointerList.Get(i)).DispatchMode := STORE_EXTERNALMODE;
end;

function TStorageController2Obj.MakeFleetList: Boolean;
var
    StorageObj: TStorageObj;
    i: Integer;

begin
    Result := FALSE;

    if FElementListSpecified then
    begin    // Name list is defined - Use it

        FleetPointerList.Clear;
        for i := 1 to FleetSize do
        begin
            StorageObj := DSS.Storage2Class.Find(FStorageNameList.Strings[i - 1]);
            if Assigned(StorageObj) then
            begin
                if StorageObj.Enabled then
                    FleetPointerList.Add(StorageObj);
            end
            else
            begin
                DoSimpleMsg('Error: Storage Element "%s" not found.', [FStorageNameList.Strings[i - 1]], 14403);
                Exit;
            end;
        end;

    end

    else
    begin
        // Search through the entire circuit for enabled Storage Elements and add them to the list
        FStorageNameList.Clear;
        FleetPointerList.Clear;
        for i := 1 to DSS.Storage2Class.ElementCount do
        begin
            StorageObj := DSS.Storage2Class.ElementList.Get(i);
            // Look for a storage element not already assigned
            if StorageObj.Enabled and (StorageObj.DispatchMode <> STORE_EXTERNALMODE) then
            begin
                FStorageNameList.Add(StorageObj.Name);  // Add to list of names
                FleetPointerList.Add(StorageObj);
            end;
        end;

        // Allocate uniform weights
        FleetSize := FleetPointerList.Count;
        Reallocmem(FWeights, Sizeof(FWeights^[1]) * FleetSize);
        for i := 1 to FleetSize do
            FWeights^[i] := 1.0;

    end;

   // Add up total weights
    TotalWeight := 0.0;
    for i := 1 to FleetSize do
        TotalWeight := TotalWeight + FWeights^[i];

    if FleetPointerList.Count > 0 then
        Result := TRUE;

    FleetListChanged := FALSE;
end;


procedure TStorageController2Obj.Reset;
begin
  // inherited;
    SetFleetToIdle;

 // do we want to set fleet to 100% charged storage?
end;

procedure TStorageController2Obj.GetControlPower(var ControlPower: Complex);
// Get power to control based on active power
var
    i: Integer;
    // ControlPowerPhase: Integer;
    TempPower: Double;

begin
    if MonitoredElement.NPhases = 1 then
    begin
        ControlPower := MonitoredElement.Power[ElementTerminal]; // just take the total power (works also for 1ph elements with 2 conductors)
    end
    else
    begin
        MonitoredElement.GetPhasePower(cBuffer);

        case FMonPhase of
            AVG:
            begin  // Get avg of all phases
                ControlPower := Cmplx(0.0, 0.0);
                for i := (1 + CondOffset) to (MonitoredElement.NConds + CondOffset) do
                    ControlPower := ControlPower + cBuffer^[i];
            end;
            MAXPHASE:
            begin  // Get abs max of all phases
                ControlPower := Cmplx(0.0, 0.0);
                for i := (1 + CondOffset) to (MonitoredElement.NConds + CondOffset) do
                begin
                    TempPower := abs(cBuffer^[i].re);
                    if TempPower > abs(ControlPower.re) then
                        ControlPower := cBuffer^[i];
                    // ControlPowerPhase := i;
                end;
                          // Compute equivalent total power of all phases assuming equal to max power in all phases
                ControlPower := ControlPower * Fnphases;
            end;
            MINPHASE:
            begin // Get abs min of all phases
                ControlPower := Cmplx(1.0e50, 1.0e50);
                for i := (1 + CondOffset) to (MonitoredElement.NConds + CondOffset) do
                begin
                    TempPower := abs(cBuffer^[i].re);
                    if TempPower < abs(ControlPower.re) then
                        ControlPower := cBuffer^[i];
                    // ControlPowerPhase := i;
                end;
                          // Compute equivalent total power of all phases assuming equal to min power in all phases
                ControlPower := ControlPower * Fnphases;  // sign according to phase with min abs value
            end;
        else
            // Compute equivalent total power of all phases assuming equal to power in selected phases
            ControlPower := Cbuffer^[FMonPhase] * Fnphases;  // monitored phase only
        end;
    end;

    {If this is a positive sequence circuit (Fnphases=1),
    then we need to multiply by 3 to get the 3-phase power}
    if ActiveCircuit.PositiveSequence then
        ControlPower := ControlPower * 3.0;
end;

procedure TStorageController2Obj.GetControlCurrent(var ControlCurrent: Double);
// Get current to control
var
    i: Integer;
begin
    case FMonPhase of
        AVG:
        begin
            ControlCurrent := 0.0;     // Get avg of all phases
            for i := (1 + CondOffset) to (MonitoredElement.NConds + CondOffset) do
                ControlCurrent := ControlCurrent + Cabs(cBuffer^[i]);
            ControlCurrent := ControlCurrent / Fnphases;
        end;
        MAXPHASE:
        begin
            ControlCurrent := 0.0;     // Get max of all phases
            for i := (1 + CondOffset) to (MonitoredElement.NConds + CondOffset) do
                ControlCurrent := max(ControlCurrent, Cabs(cBuffer^[i]));
            ControlCurrent := ControlCurrent;
        end;
        MINPHASE:
        begin
            ControlCurrent := 1.0e50;     // Get min of all phases
            for i := (1 + CondOffset) to (MonitoredElement.NConds + CondOffset) do
                ControlCurrent := min(ControlCurrent, Cabs(cBuffer^[i]));
            ControlCurrent := ControlCurrent;
        end;
    else
    {Just use one phase because that's what most controls do.}
        ControlCurrent := Cabs(Cbuffer^[FMonPhase]);  // monitored phase only
    end;
end;

finalization    ChargeModeEnum.Free;
    DischargeModeEnum.Free;
end.
