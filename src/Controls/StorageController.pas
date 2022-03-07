unit StorageController;

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

type
{$SCOPEDENUMS ON}
    TStorageControllerProp = (
        INVALID = 0,
        Element = 1,
        Terminal = 2,
        kWTarget = 3,
        kWTargetLow = 4,
        pctkWBand = 5,
        pctkWBandLow = 6,
        PFTarget = 7,
        PFBand = 8,
        ElementList = 9,
        Weights = 10,
        ModeDischarge = 11, 
        ModeCharge = 12, 
        TimeDischargeTrigger = 13,
        TimeChargeTrigger = 14,
        pctRatekW = 15,
        pctRatekvar = 16,
        pctRateCharge = 17,
        pctReserve = 18,
        kWhTotal = 19,
        kWTotal = 20,
        kWhActual = 21,
        kWActual = 22,
        kWneed = 23,
        pctParticipation = 24, // TODO: unused, remove?
        Yearly = 25,
        Daily = 26,
        Duty = 27,
        EventLog = 28,
        VarDispatch = 29,
        InhibitTime = 30,
        Tup = 31,
        TFlat = 32,
        Tdn = 33,
        kWThreshold = 34,
        ResetLevel = 35,
        Seasons = 36,
        SeasonTargets = 37,
        SeasonTargetsLow = 38
    );
{$SCOPEDENUMS OFF}

    TStorageController = class(TControlClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TStorageControllerObj = class(TControlElem)
    PRIVATE
        FkWTarget,
        FkWTargetLow,
        FkWThreshold,
        FpctkWBand,
        FpctkWBandLow,
        HalfkWBand,
        HalfkWBandLow,
        FPFTarget,                      // Range on this is 0..2 where 1..2 is leading
        TotalWeight,
        HalfPFBand,
        FPFBand,
        UpRamptime,
        FlatTime,
        DnrampTime,
        UpPlusFlat,
        UpPlusFlatPlusDn,
        DischargeTriggerTime,
        ChargeTriggerTime,
        pctKWRate,
        pctkvarRate,
        pctChargeRate,
        LastpctDischargeRate,
        //TotalkWCapacity,
        //TotalkWhCapacity,
        pctFleetReserve,
        ResetLevel,
        kWNeeded: Double;

        ParticipationStr: String;

        FStorageNameList: TStringList;
        FleetPointerList: TDSSPointerList;
        SeasonTargets,
        SeasonTargetsLow: Array of Double;
        FWeights: pDoubleArray;

        FleetListChanged,
        ChargingAllowed,
        DischargeTriggeredByTime,
        DischargeInhibited,
        OutOfOomph,
        FElementListSpecified,
        Wait4Step: Boolean;
        DispatchVars: LongBool;

        Seasons,
        FleetSize,
        FleetState,
        DischargeMode,
        InhibitHrs,
        ChargeMode: Integer;

        YearlyShapeObj: TLoadShapeObj;  // Shape for this Storage element
        DailyShapeObj: TLoadShapeObj;  // Daily Storage element Shape for this load
        DutyShapeObj: TLoadShapeObj;  // Shape for this Storage element

        LoadShapeMult: Complex;

           // PROCEDURE SetPctReserve;
        procedure SetAllFleetValues;
        procedure SetFleetkWRate(pctkw: Double);
        procedure SetFleetkvarRate(pctkvar: Double);
        procedure SetFleetChargeRate;
        procedure SetFleetToCharge;
        procedure SetFleetToDisCharge;
        procedure SetFleetToIdle;
        procedure SetFleetToExternal;

        procedure CalcYearlyMult(Hr: Double);
        procedure CalcDailyMult(Hr: Double);
        procedure CalcDutyMult(Hr: Double);

        function MakeFleetList: Boolean;
        procedure DoLoadFollowMode;
        procedure DoLoadShapeMode;
        procedure DoTimeMode(Opt: Integer);
        procedure DoScheduleMode;
        procedure DoPeakShaveModeLow;
        procedure PushTimeOntoControlQueue(Code: Integer);
        function NormalizeToTOD(h: Integer; sec: Double): Double;
        procedure Set_PFBand(const Value: Double);
        function Get_FleetkW: Double;
        function Get_FleetkWh: Double;
        function Get_FleetkWhRating: Double;
        function Get_FleetReservekWh: Double;

        function Get_DynamicTarget(THigh: Integer): Double;

    PUBLIC

        constructor Create(ParClass: TDSSClass; const StorageControllerName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData; OVERRIDE;

        procedure Sample; OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state
        
        property PFBand: Double READ FPFBand WRITE Set_PFBand;
        property FleetkWhRating: Double READ Get_FleetkWhRating;
        property FleetReservekWh: Double READ Get_FleetReservekWh;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    Storage,
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
    TObj = TStorageControllerObj;
    TProp = TStorageControllerProp;
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
var
    PropInfo: Pointer = NIL;    
    ChargeModeEnum, DischargeModeEnum: TDSSEnum;

function ConvertPFToPFRange2(const value: Double): Double;
// Convert PF from +/- 1 to 0..2 Where 1..2 is leading
begin
    if value < 0.0 then
        Result := 2.0 + Value
    else
        Result := Value;
end;

function ConvertPFRange2ToPF(const value: Double): Double;
begin
    if value > 1.0 then
        Result := value - 2.0
    else
        Result := Value;
end;

constructor TStorageController.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        DischargeModeEnum := TDSSEnum.Create('LegacyStorageController: Discharge mode', False, 1, 2, 
            ['Peakshave', 'Follow', 'Support', 'Loadshape', 'Time', 'Schedule', 'I-Peakshave'],
            [MODEPEAKSHAVE, MODEFOLLOW, MODESUPPORT, MODELOADSHAPE, MODETIME, MODESCHEDULE, CURRENTPEAKSHAVE]);
        ChargeModeEnum := TDSSEnum.Create('LegacyStorageController: Charge mode', False, 1, 1, 
            ['Loadshape', 'Time', 'PeakshaveLow', 'I-PeakshaveLow'],
            [MODELOADSHAPE, MODETIME, MODEPEAKSHAVELOW, CURRENTPEAKSHAVELOW]);
    end;
    inherited Create(dssContext, STORAGE_CONTROL, 'StorageController');
end;

destructor TStorageController.Destroy;
begin
    inherited Destroy;
end;

function GetkWhTotal(obj: TObj): Double;
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

function GetkWTotal(obj: TObj): Double;
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

function GetFleetkW(obj: TObj): Double;
begin
    Result := obj.Get_FleetkW();
end;

function GetFleetkWh(obj: TObj): Double;
begin
    Result := obj.Get_FleetkWh();
end;

function GetPctkvarRate(obj: TObj): Double;
begin
    Result := ConvertPFRange2ToPF(Obj.FPFTarget);
end;

procedure SetPctkvarRate(obj: TObj; Value: Double);
begin
    Obj.FPFTarget := ConvertPFToPFRange2(Value);
end;

procedure TStorageController.DefineProperties;
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

    // double functions
    PropertyFlags[ord(TProp.kWhTotal)] := [TPropertyFlag.SilentReadOnly, TPropertyFlag.ReadByFunction];
    PropertyFlags[ord(TProp.kWTotal)] := [TPropertyFlag.SilentReadOnly, TPropertyFlag.ReadByFunction];
    PropertyFlags[ord(TProp.kWhActual)] := [TPropertyFlag.SilentReadOnly, TPropertyFlag.ReadByFunction];
    PropertyFlags[ord(TProp.kWActual)] := [TPropertyFlag.SilentReadOnly, TPropertyFlag.ReadByFunction];
    PropertyReadFunction[ord(TProp.kWhTotal)] := @GetkWhTotal;
    PropertyReadFunction[ord(TProp.kWTotal)] := @GetkWTotal;
    PropertyReadFunction[ord(TProp.kWhActual)] := @GetFleetkWh;
    PropertyReadFunction[ord(TProp.kWActual)] := @GetFleetkW;

    // double read-only
    PropertyFlags[ord(TProp.kWneed)] := [TPropertyFlag.SilentReadOnly];
    PropertyOffset[ord(TProp.kWneed)] := ptruint(@obj.kWNeeded);

    // boolean properties
    PropertyType[ord(TProp.EventLog)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.EventLog)] := ptruint(@obj.ShowEventLog);

    PropertyType[ord(TProp.VarDispatch)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.VarDispatch)] := ptruint(@obj.DispatchVars);

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
    PropertyOffset2[ord(TProp.Element)] := 0;
    PropertyWriteFunction[ord(TProp.Element)] := @SetMonitoredElement;
    PropertyFlags[ord(TProp.Element)] := [TPropertyFlag.WriteByFunction];//[TPropertyFlag.CheckForVar]; // not required for general cktelements

    // string properties
    PropertyType[ord(TProp.pctParticipation)] := TPropertyType.StringProperty; // actually unused here
    PropertyOffset[ord(TProp.pctParticipation)] := ptruint(@obj.ParticipationStr);
    
    // string lists
    PropertyType[ord(TProp.ElementList)] := TPropertyType.StringListProperty;
    PropertyOffset[ord(TProp.ElementList)] := ptruint(@obj.FStorageNameList);

    // integers
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

    // doubles
    PropertyOffset[ord(TProp.ResetLevel)] := ptruint(@obj.ResetLevel);
    PropertyOffset[ord(TProp.PFBand)] := ptruint(@obj.FPFBand);
    PropertyOffset[ord(TProp.kWTarget)] := ptruint(@obj.FkWTarget);
    PropertyOffset[ord(TProp.kWTargetLow)] := ptruint(@obj.FkWTargetLow);
    PropertyOffset[ord(TProp.pctkWBand)] := ptruint(@obj.FpctkWBand);
    PropertyOffset[ord(TProp.pctkWBandLow)] := ptruint(@obj.FpctkWBandLow);
    PropertyOffset[ord(TProp.TimeDischargeTrigger)] := ptruint(@obj.DischargeTriggerTime);
    PropertyOffset[ord(TProp.TimeChargeTrigger)] := ptruint(@obj.ChargeTriggerTime);
    PropertyOffset[ord(TProp.pctRatekW)] := ptruint(@obj.pctkWRate);
    PropertyOffset[ord(TProp.pctRateCharge)] := ptruint(@obj.pctChargeRate);
    PropertyOffset[ord(TProp.pctReserve)] := ptruint(@obj.pctFleetReserve);
    PropertyOffset[ord(TProp.Tup)] := ptruint(@obj.UpRamptime);
    PropertyOffset[ord(TProp.TFlat)] := ptruint(@obj.FlatTime);
    PropertyOffset[ord(TProp.Tdn)] := ptruint(@obj.DnrampTime);
    PropertyOffset[ord(TProp.kWThreshold)] := ptruint(@obj.FkWThreshold);
    PropertyOffset[ord(TProp.pctRatekvar)] := ptruint(@obj.pctkvarRate);

    PropertyOffset[ord(TProp.PFTarget)] := 1;
    PropertyWriteFunction[ord(TProp.PFTarget)] := @SetPctkvarRate;
    PropertyReadFunction[ord(TProp.PFTarget)] := @GetPctkvarRate;
    PropertyFlags[ord(TProp.PFTarget)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.ReadByFunction];


    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TStorageController.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TStorageControllerObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    i: Integer;
    casemult: Double;
begin
    case Idx of
        ord(TProp.kWTarget),
        ord(TProp.pctkWBand):
        begin
            if DischargeMode = CURRENTPEAKSHAVE then  // evaluates the discharging mode to apply
                Casemult := 1000.0                 // a compensation value (for kamps)
            else
                Casemult := 1.0;

            HalfkWBand := FpctkWBand / 200.0 * FkWTarget * Casemult;
            FkWThreshold := FkWTarget * 0.75 * Casemult;
        end;
        ord(TProp.kWTargetLow),
        ord(TProp.pctkWBandLow):
        begin
            if ChargeMode = CURRENTPEAKSHAVELOW then  // evaluates the charging mode to apply
                Casemult := 1000.0                 // a compensation value (for kamps)
            else
                Casemult := 1.0;

            HalfkWBandLow := FpctkWBandLow / 200.0 * FkWTargetLow * Casemult;
        end;
        ord(TProp.PFBand):
            HalfPFBand := FPFBand / 2.0;
        ord(TProp.ModeDischarge):
            if DischargeMode = MODEFOLLOW then
                DischargeTriggerTime := 12.0; // Noon

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
        ord(TProp.InhibitTime):
            Inhibithrs := Max(1, Inhibithrs);  // >=1, silent
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TStorageControllerObj.MakeLike(OtherPtr: Pointer);
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

    FkWTarget := Other.FkWTarget;
    FkWTargetLow := Other.FkWTargetLow;
    FkWThreshold := Other.FkWThreshold;
    FpctkWBand := Other.FpctkWBand;
    FpctkWBandLow := Other.FpctkWBandLow;
    FPFTarget := Other.FPFTarget;
    FPFBand := Other.FPFBand;
    HalfPFBand := Other.HalfPFBand;
    ResetLevel := Other.ResetLevel;

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
    pctkvarRate := Other.pctkvarRate;
    pctChargeRate := Other.pctChargeRate;
    pctFleetReserve := Other.pctFleetReserve;
    YearlyShapeObj := Other.YearlyShapeObj;
    DailyShapeObj := Other.DailyShapeObj;
    DutyShapeObj := Other.DutyShapeObj;
    DispatchVars := Other.DispatchVars;
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

constructor TStorageControllerObj.Create(ParClass: TDSSClass; const StorageControllerName: String);
begin
    inherited Create(ParClass);
    Name := AnsiLowerCase(StorageControllerName);
    DSSObjType := ParClass.DSSClassType;

    FNPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors

    ParticipationStr := '';
    ControlledElement := NIL;  // not used in this control
    ElementTerminal := 1;
    MonitoredElement := NIL;

    FStorageNameList := TStringList.Create;
    FWeights := NIL;
    FleetPointerList := TDSSPointerList.Create(20);  // Default size and increment
    FleetSize := 0;
    FleetState := STORE_IDLING;
    FkWTarget := 8000.0;
    FkWTargetLow := 4000.0;
    FkWThreshold := 6000.0;
    FpctkWBand := 2.0;
    FpctkWBandLow := 2.0;
    TotalWeight := 1.0;
    HalfkWBand := FpctkWBand / 200.0 * FkWTarget;
    FPFTarget := 0.96;
    FPFBand := 0.04;
    HalfPFBand := FPFBand / 2.0;
    kWNeeded := 0.0;

    DischargeMode := MODEPEAKSHAVE;
    ChargeMode := MODETIME;

    DischargeTriggerTime := -1.0;  // disabled
    ChargeTriggerTime := 2.0;   // 2 AM
    FElementListSpecified := FALSE;
    FleetListChanged := TRUE;  // force building of list
    pctkWRate := 20.0;
    pctkvarRate := 20.0;
    pctChargeRate := 20.0;
    pctFleetReserve := 25.0;

    ShowEventLog := FALSE;
    DispatchVars := FALSE;
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

destructor TStorageControllerObj.Destroy;
begin
    FleetPointerList.Free;
    FStorageNameList.Free;

    inherited Destroy;
end;

function TStorageControllerObj.Get_FleetkW: Double;
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

function TStorageControllerObj.Get_FleetkWh: Double;
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

function TStorageControllerObj.Get_FleetkWhRating: Double;
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

function TStorageControllerObj.Get_FleetReservekWh: Double;
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

procedure TStorageControllerObj.RecalcElementData;
// Recalculate critical element values after changes have been made
begin
    {Check for existence of monitored element}
    if MonitoredElement <> NIL then
    begin
        if ElementTerminal > MonitoredElement.Nterms then
        begin
            DoErrorMsg(
                Format(_('StorageController: "%s"'), [Name]),
                Format(_('Terminal no. "%d" Does not exist.'), [MonitoredElement]),
                _('Re-specify terminal no.'), 371);
        end
        else
        begin
            FNphases := MonitoredElement.Nphases;
            NConds := FNphases;
            // Sets name of i-th terminal's connected bus in StorageController's buslist
            Setbus(1, MonitoredElement.GetBus(ElementTerminal));
        end;
    end
    else
        DoSimpleMsg('Monitored Element in "%s" is not set', [FullName], 372);

    if FleetListChanged then
        if not MakeFleetList then
            DoSimpleMsg('No unassigned Storage Elements found to assign to %s', [FullName], 37201);

    // TotalkWCapacity := GetkWTotal();
    // TotalkWhCapacity := GetkWhTotal();

    if FleetSize > 0 then
    begin
        SetFleetToExternal;
        SetAllFleetValues;
    end;

    UpPlusFlat := UpRampTime + FlatTime;
    UpPlusFlatPlusDn := UpPlusFlat + DnRampTime;
end;

procedure TStorageControllerObj.MakePosSequence();
begin
    if MonitoredElement <> NIL then
    begin
        FNphases := MonitoredElement.NPhases;
        Nconds := FNphases;
        Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    end;
    inherited;
end;

procedure TStorageControllerObj.DoPendingAction(const Code, ProxyHdl: Integer);
begin
    // Release  the discharge inhibit .
    // Do nothing for other codes
    if (Code = RELEASE_INHIBIT) and (DischargeMode <> MODEFOLLOW) then
        DischargeInhibited := FALSE;
end;

procedure TStorageControllerObj.DoScheduleMode;
// In SCHEDULE mode we ramp up the storage from zero to the specified pctkWRate.
// This value is held for the flattime or until they  turn themselves
// off when they are either fully discharged, or ramped down

// The discharge trigger time must be greater than 0
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
                        AppendToEventLog('StorageController.' + Self.Name, 'Fleet Set to Discharging (up ramp)by Schedule');
                    SetFleetToDischarge;
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
                    SetFleetkWRate(pctDischargeRate);

                end
                else
                begin
                    if TDiff < UpPlusFlat then
                    begin
                        pctDischargeRate := pctkWRate;
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
                        SetFleetkWRate(pctDischargeRate);

                    end;

                end;

                if pctDischargeRate <> LastpctDischargeRate then
                    PushTimeOntoControlQueue(STORE_DISCHARGING);

            end;  {If not fleetstate ...}
        end;
    LastpctDischargeRate := pctDischargeRate;   // remember this value
end;

procedure TStorageControllerObj.DoTimeMode(Opt: Integer);
//   In Time mode we need to only turn the storage elements on. They will turn themselves
//   off when they are either fully discharged, fully charged, or receive another command
//   from the controller
begin
    case Opt of

        1:
        begin
            if (DisChargeTriggerTime > 0.0) then
                with ActiveCircuit.Solution do
                begin
                    // turn on if time within 1/2 time step
                    if abs(NormalizeToTOD(DynaVars.intHour, DynaVars.t) - DisChargeTriggerTime) < DynaVars.h / 7200.0 then
                    begin
                        if not (FleetState = STORE_DISCHARGING) then
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
                        if not (FleetState = STORE_CHARGING) then
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
        end; //Charge mode
    end;
end;

function TStorageControllerObj.NormalizeToTOD(h: Integer; sec: Double): Double;
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

procedure TStorageControllerObj.PushTimeOntoControlQueue(Code: Integer);
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

function TStorageControllerObj.Get_DynamicTarget(THigh: Integer): Double;
var
    // Temp, temp2: Double;
    RatingIdx: Integer;
    RSignal: TXYCurveObj;
begin
    if DSS.SeasonSignal <> '' then
    begin
        RSignal := DSS.XYCurveClass.Find(DSS.SeasonSignal); //TODO: move this to TDSSContext
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

procedure TStorageControllerObj.DoLoadFollowMode;
var
    i: Integer;
    S: Complex;
    StorageObj: TSTorageObj;
    StorekWChanged,
    StorekvarChanged,
    SkipkWDispatch: Boolean;
    VoltsArr: pComplexArray;
    kWhActual,
    ElemVolts,
    Amps,
    AmpsDiff,
    PDiff,
    PFDiff,
    DispatchkW,
    Dispatchkvar,
    RemainingkWh,
    CtrlTarget,
    ReservekWh: Double;
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
            Amps := MonitoredElement.MaxCurrent[ElementTerminal]; // Max current in active terminal
        end
        else
            S := MonitoredElement.MaxPower[ElementTerminal];  // Max power in active terminal
       // In case of having seasonal targets
        if DSS.SeasonalRating then
            CtrlTarget := Get_DynamicTarget(1)
        else
            CtrlTarget := FkWTarget;

       // based on max phase current
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
                    DischargeTriggeredByTime := FALSE;  // so we don't come back in here right away
                    SetFleetToIdle;
                end;
                PDiff := S.re * 0.001 - FkWTarget;  // Assume S.re is normally positive
                PFDiff := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for peak shaving
            end;
             // supporting DG; Try to keep load above kW target
            MODESUPPORT:
            begin
                PDiff := S.re * 0.001 + FkWTarget;  // assume S.re is normally negative
                PFDiff := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for generator
            end;

            MODEPEAKSHAVE:
            begin
                PDiff := S.re * 0.001 - CtrlTarget;  // Assume S.re is normally positive
                PFDiff := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for peak shaving
            end;

            CURRENTPEAKSHAVE:
            begin
                PDiff := Amps - CtrlTarget * 1000;  // Gets the difference in terms of amps
                DispatchVars := FALSE;
            end;
        else
            PDiff := 0.0;
            PFDiff := 0.0;
        end;

        kWNeeded := PDiff;

       {  kW dispatch  }

        if DischargeInhibited then
            SkipkWDispatch := TRUE
        else
        begin
            if FleetState = STORE_CHARGING then
            begin
                if not (Dischargemode = CURRENTPEAKSHAVE) then
                    Pdiff := Pdiff + Get_FleetkW()  // ignore overload due to charging
                else
                begin
                    MonitoredElement.ComputeVterminal;
                    VoltsArr := MonitoredElement.Vterminal;
                    ElemVolts := cabs(VoltsArr^[1]);
                    Pdiff := Pdiff + (Get_FleetkW() * 1000 / ElemVolts);
                end;
            end;

            case FleetState of
                STORE_CHARGING,
                STORE_IDLING:
                    if (PDiff < 0.0) or OutOfOomph then
                    begin  // Don't bother trying to dispatch
                        ChargingAllowed := TRUE;
                        SkipkWDispatch := TRUE;
                        if OutofOomph then
                        begin
                            for i := 1 to FleetSize do
                            begin
                                StorageObj := FleetPointerList.Get(i);
                                kWhActual := StorageObj.StorageVars.kWhStored / StorageObj.StorageVars.kWhRating;
                                OutOfOomph := OutOfOomph and (kWhActual >= ResetLevel);  // If we have more than the 80% we are good to dispatch
                            end;
                            OutOfOomph := not OutOfOomph;  // If everybody in the fleet has at least the 80% of the storage capacity full

                        end;
                    end;
                STORE_DISCHARGING:
                    if ((PDiff + Get_FleetkW()) < 0.0) or OutOfOomph then
                    begin   // desired decrease is greater then present output; just cancel
                        SetFleetToIdle;   // also sets presentkW = 0
                        PushTimeOntoControlQueue(STORE_IDLING);  // force a new power flow solution
                        ChargingAllowed := TRUE;
                        SkipkWDispatch := TRUE;
                        Wait4Step := TRUE; // To tell to the charging section to wait for the next sim step
                                                 // useful when workin with large simulation time steps
                    end;
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
                        SetFleetToDischarge;
                    if ShowEventLog then
                        AppendToEventLog('StorageController.' + Self.Name, Format('Attempting to dispatch %-.6g kW with %-.6g kWh remaining and %-.6g reserve.', [kWneeded, RemainingkWh, ReservekWh]));
                    AmpsDiff := PDiff;
                    for i := 1 to FleetSize do
                    begin
                        StorageObj := FleetPointerList.Get(i);
                        if Dischargemode = CURRENTPEAKSHAVE then // Current to power
                        begin    //  (MonitoredElement.MaxVoltage[ElementTerminal] / 1000)
                            if StorageObj.NPhases = 1 then
                                PDiff := StorageObj.PresentkV * AmpsDiff
                            else
                                PDiff := StorageObj.PresentkV * invsqrt3 * AmpsDiff;
                        end;
                        with StorageObj do
                        begin
                            // compute new dispatch value for this storage element ...
                            DispatchkW := Min(StorageVars.kWrating, (PresentkW + PDiff * (FWeights^[i] / TotalWeight)));
                            if DispatchkW <> PresentkW then    // redispatch only if change requested
                                if StorageVars.kWhStored > StorageVars.kWhReserve then
                                begin  // Attempt to set discharge kW;  Storage element will revert to idling if out of capacity
                                    StorageObj.PresentkW := DispatchkW;
                                    StorekWChanged := TRUE;     // This is what keeps the control iterations going
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
                    AppendToEventLog('StorageController.' + Self.Name, Format('Ran out of OOMPH: %-.6g kWh remaining and %-.6g reserve.', [RemainingkWh, ReservekWh]));
            end;
        end;


       // kvar dispatch  NOTE: PFDiff computed from PF in range of 0..2
       // Redispatch the vars only if the PF is outside the band
        if DispatchVars and (Abs(PFDiff) > HalfPFBand) then
        begin
            if ShowEventLog then
                AppendToEventLog('StorageController.' + Self.Name, Format('Changed kvar Dispatch. PF Diff needed = %.6g', [PFDiff]));
          // Redispatch Storage elements
            for i := 1 to FleetSize do
            begin
                StorageObj := FleetPointerList.Get(i);
                    // compute new var dispatch value for this storage element ...
                if FPFTarget = 1.0 then
                    Dispatchkvar := 0.0
                else
                begin
                    Dispatchkvar := S.re * Sqrt(1.0 / SQR(ConvertPFRange2ToPF(FPFTarget)) - 1.0) * (FWeights^[i] / TotalWeight);
                    if FPFTarget > 1.0 then
                        Dispatchkvar := -Dispatchkvar;  // for watts and vars in opposite direction
                end;

                if Dispatchkvar <> StorageObj.Presentkvar then
                begin
                    StorageObj.Presentkvar := Dispatchkvar;  // Ask for this much kvar  but may be limited by element
                    StorekvarChanged := TRUE;
                end;
            end;
        end;

        if StorekWChanged or StorekvarChanged then  // Only push onto controlqueue If there has been a change
            PushTimeOntoControlQueue(STORE_DISCHARGING);


       {Else just continue}
    end;
end;

procedure TStorageControllerObj.DoPeakShaveModeLow;
    // This is the peakShaving mode for controlling the charging operation of the storage fleet
    // The objective is to charge the storage fleet when the power at a monitored element is bellow a specified KW target (kWTarget_low)
    // The storage will charge as much power as necessary to keet the power within the deadband around kWTarget_low

  // WILL NOT IMPLEMENT REACTIVE POWER CONTROL FOR NOW
var
    i: Integer;
    S: Complex;
    VoltsArr: PComplexArray;
    StorageObj: TSTorageObj;
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
    CtrlTarget,
    KwtoPercentagekW: Double;

begin
     // If list is not defined, go make one from all storage elements in circuit
    if FleetPointerList.Count = 0 then
        MakeFleetList;

    if (FleetSize > 0) and (not (FleetState = STORE_DISCHARGING)) then
    begin
        StorekWChanged := FALSE;
        SkipkWCharge := FALSE;

       //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
        if DSS.SeasonalRating then
            CtrlTarget := Get_DynamicTarget(0)
        else
            CtrlTarget := FkWTargetLow;

        if Chargemode = CURRENTPEAKSHAVELOW then
        begin
            Amps := MonitoredElement.MaxCurrent[ElementTerminal]; // Max current in active terminal
            PDiff := Amps - CtrlTarget * 1000;  // Gets the difference in terms of amps
        end
        else
        begin
            S := MonitoredElement.MaxPower[ElementTerminal];  // Power in active terminal
            PDiff := S.re * 0.001 - CtrlTarget;  // Assume S.re is normally positive
        end;

        // ActualkW := Get_FleetkW();
        ActualkWh := Get_FleetkWh();
        TotalRatingkWh := FleetkWhRating;

        if Chargemode = CURRENTPEAKSHAVELOW then
        begin
            MonitoredElement.ComputeVterminal;
            VoltsArr := MonitoredElement.Vterminal;
            ElemVolts := cabs(VoltsArr^[1]);
            kWNeeded := ((Pdiff * ElemVolts) / 1000.0) + Get_FleetkW();
        end
        else
            kWNeeded := Pdiff + Get_FleetkW();

        case FleetState of
            STORE_IDLING:
                if (PDiff > 0.0) or (ActualkWh >= TotalRatingkWh) or Wait4Step then
                begin  // Don't bother trying to charge
                    ChargingAllowed := FALSE;
                    SkipkWCharge := TRUE;
                    Wait4Step := FALSE;
                end;
            STORE_CHARGING:
                if (kWNeeded > 0.0) or (ActualkWh >= TotalRatingkWh) then
                begin   // desired decrease is greater then present output; just cancel
                    SetFleetToIdle;                                   // also sets presentkW = 0
                    PushTimeOntoControlQueue(STORE_IDLING);  // force a new power flow solution
                    ChargingAllowed := FALSE;
                    SkipkWCharge := TRUE;
                end;
        end;

        if not SkipkWCharge then
        begin
            if (ActualkWh < TotalRatingkWh) then
            begin
               //  don't dispatch kW  if fully charged or an endless control loop will occur
                if abs(PDiff) > HalfkWBandLow then
                begin // Attempt to change storage kW charge
                    if not (FleetState = STORE_CHARGING) then
                        SetFleetToCharge;
                    if ShowEventLog then
                        AppendToEventLog('StorageController.' + Self.Name, Format('Attempting to charge %-.6g kW with %-.6g kWh remaining and %-.6g rating.', [PDiff, (TotalRatingkWh - ActualkWh), TotalRatingkWh]));
                    AmpsDiff := PDiff;
                    for i := 1 to FleetSize do
                    begin
                        StorageObj := FleetPointerList.Get(i);
                        with StorageObj do
                        begin
                     // Checks if PDiff needs to be adjusted considering the charging mode
                            if Chargemode = CURRENTPEAKSHAVELOW then
                            begin
                                if StorageObj.NPhases = 1 then
                                    PDiff := StorageObj.PresentkV * AmpsDiff
                                else
                                    PDiff := StorageObj.PresentkV * invsqrt3 * AmpsDiff;
                            end;

                     // compute new charging value for this storage element ...
                            ChargekW := -1 * Min(StorageVars.kWrating, abs(PresentkW + PDiff * (FWeights^[i] / TotalWeight)));
                            if ChargekW <> PresentkW then    // do only if change requested
                                if StorageVars.kWhStored < StorageVars.kWhRating then
                                begin  // Attempt to set discharge kW;  Storage element will revert to idling if out of capacity
                         //StorageObj.PresentkW  :=  ChargekW;
                                    KwtoPercentagekW := (ChargekW * 100) / StorageVars.kWrating;
                                    StorageObj.pctkWin := abs(KwtoPercentagekW);
                                    StorekWChanged := TRUE;     // This is what keeps the control iterations going
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

procedure TStorageControllerObj.Sample;

begin
    ChargingAllowed := FALSE;
{
  Check discharge mode first. Then if not discharging, we can check for charging
}
    Wait4Step := FALSE;        // Initializes the variable for the new control step
    case DischargeMode of
        MODEFOLLOW:
        begin
            DoTimeMode(1);
            DoLoadFollowMode;
        end;
        MODELOADSHAPE:
            DoLoadShapeMode;
        MODESUPPORT:
            DoLoadFollowMode;
        MODETIME:
            DoTimeMode(1);
        MODEPEAKSHAVE:
            DoLoadFollowMode;
        CURRENTPEAKSHAVE:
            DoLoadFollowMode;
        MODESCHEDULE:
            DoScheduleMode;
    else
        DoSimpleMsg('Invalid DisCharging Mode: %d', [DisChargeMode], 14408);
    end;

    if ChargingAllowed then
        case ChargeMode of
            MODELOADSHAPE: ; // DoLoadShapeMode;  already executed above
            MODETIME:
                DoTimeMode(2);
            MODEPEAKSHAVELOW:
                DoPeakShaveModeLow;
            CURRENTPEAKSHAVELOW:
                DoPeakShaveModeLow
        else
            DoSimpleMsg('Invalid Charging Mode: %d', [ChargeMode], 14409);
        end;
end;


procedure TStorageControllerObj.CalcDailyMult(Hr: Double);

begin
    if (DailyShapeObj <> NIL) then
    begin
        LoadShapeMult := DailyShapeObj.GetMultAtHour(Hr);
    end
    else
        LoadShapeMult := CDoubleOne;  // Default to no  variation
end;


procedure TStorageControllerObj.CalcDutyMult(Hr: Double);

begin
    if DutyShapeObj <> NIL then
    begin
        LoadShapeMult := DutyShapeObj.GetMultAtHour(Hr);
    end
    else
        CalcDailyMult(Hr);  // Default to Daily Mult If no duty curve specified
end;

procedure TStorageControllerObj.CalcYearlyMult(Hr: Double);

begin
    if YearlyShapeObj <> NIL then
    begin
        LoadShapeMult := YearlyShapeObj.GetMultAtHour(Hr);
    end
    else
        CalcDailyMult(Hr);  // Defaults to Daily curve
end;

procedure TStorageControllerObj.DoLoadShapeMode;
var
    FleetStateSaved: Integer;
    RateChanged: Boolean;
    NewChargeRate: Double;
    NewkWRate,
    NewkvarRate: Double;
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
        if NewChargeRate <> pctChargeRate then
            RateChanged := TRUE;
        pctChargeRate := NewChargeRate;
        SetFleetChargeRate;
        SetFleetToCharge;
    end
    else
    if LoadShapeMult.re = 0.0 then
        SetFleetToIdle
    else
    begin   // Set fleet to discharging at a rate
        NewkWRate := LoadShapeMult.re * 100.0;
        NewkvarRate := LoadShapeMult.im * 100.0;
        if (NewkWRate <> pctkWRate) or (NewkvarRate <> pctkvarRate) then
            RateChanged := TRUE;
        pctkWRate := NewkWRate;
        pctkvarRate := NewkvarRate;
        SetFleetkWRate(pctKWRate);
        SetFleetkvarRate(pctkvarRate);
        SetFleetToDischarge;
        ActiveCircuit.Solution.LoadsNeedUpdating := TRUE; // Force recalc of power parms
    end;

    {Force a new power flow solution if fleet state has changed}
    if (FleetState <> FleetStateSaved) or RateChanged then
        PushTimeOntoControlQueue(0);
end;

procedure TStorageControllerObj.SetAllFleetValues;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        with TStorageObj(FleetPointerList.Get(i)) do
        begin
            pctkWin := pctChargeRate;
            Fpctkvarout := pctkvarRate;
            pctkWout := pctkWRate;
            pctReserve := pctFleetReserve;
        end;
end;

procedure TStorageControllerObj.SetFleetChargeRate;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        TStorageObj(FleetPointerList.Get(i)).pctkWin := pctChargeRate;
end;

procedure TStorageControllerObj.SetFleetkvarRate;
var
    i: Integer;
begin
    {For side effects see pctkvarout property of Storage element}
    for i := 1 to FleetPointerList.Count do
        TStorageObj(FleetPointerList.Get(i)).pctkvarout := pctkvarRate;
end;

procedure TStorageControllerObj.SetFleetkWRate(pctkw: Double);
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        TStorageObj(FleetPointerList.Get(i)).pctkWout := pctkw;
end;

procedure TStorageControllerObj.SetFleetToCharge;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        TStorageObj(FleetPointerList.Get(i)).StorageState := STORE_CHARGING;
    FleetState := STORE_CHARGING;
end;

procedure TStorageControllerObj.SetFleetToDisCharge;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        TStorageObj(FleetPointerList.Get(i)).StorageState := STORE_DISCHARGING;
    FleetState := STORE_DISCHARGING;
end;

procedure TStorageControllerObj.SetFleetToIdle;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        with TStorageObj(FleetPointerList.Get(i)) do
        begin
            StorageState := STORE_IDLING;
            PresentkW := 0.0;
        end;
    FleetState := STORE_IDLING;
end;

procedure TStorageControllerObj.Set_PFBand(const Value: Double);
begin
    FPFBand := Value;
    HalfPFBand := FPFBand / 2.0;
end;

procedure TStorageControllerObj.SetFleetToExternal;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        TStorageObj(FleetPointerList.Get(i)).DispatchMode := STORE_EXTERNALMODE;
end;

function TStorageControllerObj.MakeFleetList: Boolean;
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
            StorageObj := DSS.StorageClass.Find(FStorageNameList.Strings[i - 1]);
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
     {Search through the entire circuit for enabled Storage Elements and add them to the list}
        FStorageNameList.Clear;
        FleetPointerList.Clear;
        for i := 1 to DSS.StorageClass.ElementCount do
        begin
            StorageObj := DSS.StorageClass.ElementList.Get(i);
        // Look for a storage element not already assigned
            if StorageObj.Enabled and (StorageObj.DispatchMode <> STORE_EXTERNALMODE) then
            begin
                FStorageNameList.Add(StorageObj.Name);  // Add to list of names
                FleetPointerList.Add(StorageObj);
            end;
        end;

     {Allocate uniform weights}
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

procedure TStorageControllerObj.Reset;
begin
  // inherited;
    SetFleetToIdle;

 // do we want to set fleet to 100% charged storage?
end;

finalization    ChargeModeEnum.Free;
    DischargeModeEnum.Free;
end.
