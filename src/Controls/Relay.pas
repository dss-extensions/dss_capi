unit Relay;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

//  A Relay is a control element that is connected to a terminal of a
//  circuit element and controls the switches in the same or another terminal.
//
//  The control is usually placed in the
//  terminal of a line or transformer, but it could be any element
//
//  A Relay is defined by a New command:
//
//  New Relay.Name=myname Element=devclass.name terminal=[ 1|2|...] Switch = devclass.name   terminal=[ 1|2|...]
//  Type = [current | voltage]
//  Phase = TCCCurve
//  Ground = TCCCurve
//  OverVolt = TCCcurve
//  UnderVolt = TCCCurve
//  PhaseTrip =  Multipliers times curve
//  GroundTrip =
//  PhaseInst  =
//  GroundInst =
//  RecloseIntervals= (array of times, sec);
//  ResetTime =
//
//  CktElement to be controlled must already exist.
//
//  Voltage relay is a definite time relay that operates after the voltage stays out of bounds
//  for a fixed time interval.  It will then reclose a set time after the voltage comes back in the normal range.

interface

uses
    Classes,
    Command,
    ControlClass,
    ControlElem,
    CktElement,
    DSSClass,
    Arraydef,
    UComplex, DSSUcomplex,
    utilities,
    TCC_Curve,
    Math;

type
{$SCOPEDENUMS ON}
    TRelayProp = (
        INVALID = 0,
        MonitoredObj = 1,
        MonitoredTerm = 2,
        SwitchedObj = 3,
        SwitchedTerm = 4,
        typ = 5,
        Phasecurve = 6,
        Groundcurve = 7,
        PhaseTrip = 8,
        GroundTrip = 9,
        TDPhase = 10, // 28
        TDGround = 11, // 29
        PhaseInst = 12, // 10
        GroundInst = 13, // 11
        Reset = 14, // 12
        Shots = 15, // 13
        RecloseIntervals = 16, // 14
        Delay = 17, // 24
        Overvoltcurve = 18, // 15
        Undervoltcurve = 19, // 16
        kvbase = 20, // 17
        __47pctPickup = 21, // 25
        __46BaseAmps = 22, // 23
        __46pctPickup = 23, // 21
        __46isqt = 24, // 22
        Variable = 25, // 20
        overtrip = 26,
        undertrip = 27,
        Breakertime = 28, // 18
        action = 29, // 19
        Z1mag = 30,
        Z1ang = 31,
        Z0mag = 32,
        Z0ang = 33,
        Mphase = 34,
        Mground = 35,
        EventLog = 36,
        DebugTrace = 37,
        DistReverse = 38,
        Normal = 39,
        State = 40,
        DOC_TiltAngleLow = 41,
        DOC_TiltAngleHigh = 42,
        DOC_TripSettingLow = 43,
        DOC_TripSettingHigh = 44,
        DOC_TripSettingMag = 45,
        DOC_DelayInner = 46,
        DOC_PhaseCurveInner = 47,
        DOC_PhaseTripInner = 48,
        DOC_TDPhaseInner = 49
    );
{$SCOPEDENUMS OFF}

    TRelay = class(TControlClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TRelayObj = class(TControlElem)
    PRIVATE
        ControlType: Integer;

        // OverCurrent Relay
        PhaseCurve,
        GroundCurve: TTCC_CurveObj;

        PhaseTrip,
        GroundTrip,
        PhaseInst,
        GroundInst: Double;

        RecloseIntervals: pdoubleArray;
        NumReclose: Integer;

        ResetTime,
        Delay_Time,
        Breaker_time,
        TDPhase, TDGround: Double;

        RelayTarget: String;

        // over/Under Voltage Relay
        OVcurve,                 // Curves assumed in per unit of base voltage
        UVCurve: TTCC_CurveObj;

        Vbase,   // line-neut volts base
        kVBase: Double;

        // 46 Relay  Neg Seq Current
        PickupAmps46,
        PctPickup46,
        BaseAmps46,
        Isqt46: Double;

        // 47 Relay
        PickupVolts47,
        PctPickup47: Double;

        // Distance Relay
        Z1Mag,
        Z1Ang,
        Z0Mag,
        Z0Ang,
        Mphase,
        Mground: Double;
        Dist_Z1,
        Dist_Z0,
        Dist_K0: Complex;
        Dist_Reverse: Boolean;

        // TD21 Relay
        td21_i, // present ring buffer index into td21_h
        td21_next, // index to one cycle back, and next write location
        td21_pt: Integer; // number of time samples in td21_h
        td21_stride: Integer; // length of a time sample in td21_h
        td21_quiet: Integer; // wait this many samples after an operation
        td21_h: pComplexArray; // VI history pts, vi, phases
        td21_Uref: pComplexArray; // reference (pre-fault) voltages
        td21_dV: pComplexArray; // incremental voltages
        td21_dI: pComplexArray; // incremental currents

        // Directional Overcurrent Relay
        DOC_TiltAngleLow,  // Tilt angle for lower current magnitude
        DOC_TiltAngleHigh,  // Tilt angle for higher current magnitude
        DOC_TripSetLow,  // Trip setting for lower current magnitude
        DOC_TripSetHigh,  // Trip setting for higher current magnitude
        DOC_TripSetMag,  // Current magnitude trip setting (define a circle for the relay characteristics)
        DOC_DelayInner,  // Delay for trip in inner zone of the DOC characteristic
        DOC_PhaseTripInner, // Multiplier for TCC Curve for tripping in inner zone of the DOC characteristic
        DOC_TDPhaseInner: Double;  // Time Dial for DOC_PhaseTripInner
        DOC_PhaseCurveInner: TTCC_CurveObj;  // TCC Curve for tripping in inner zone of the DOC characteristic

        // Generic Relay
        OverTrip,
        UnderTrip: Double;

        OperationCount: Integer;

        LockedOut,
        ArmedForClose,
        ArmedForOpen,
        ArmedForReset,
        PhaseTarget,
        GroundTarget: Boolean;

        NextTriptime: Double;
        LastEventHandle: Integer;

        CondOffset: Integer; // Offset for monitored terminal

        cBuffer: pComplexArray; // Complexarray buffer for an operating quantity
        cvBuffer: pComplexArray; // for distance and td21 voltages, using cBuffer for the currents

        DebugTrace: Boolean;
        PreviousControlledElement: TDSSCktElement;

        function get_PresentState: EControlAction;
        procedure set_PresentState(const Value: EControlAction);

        procedure OvercurrentLogic;
        procedure VoltageLogic;
        procedure RevPowerLogic;
        procedure NegSeq46Logic;
        procedure NegSeq47Logic;
        procedure GenericLogic;
        procedure DistanceLogic;
        procedure TD21Logic;
        procedure DirectionalOvercurrentLogic;
    PUBLIC
        MonitoredElementTerminal: Integer;
        FPresentState,
        NormalState: EControlAction;
        NormalStateSet: Boolean;

        constructor Create(ParClass: TDSSClass; const RelayName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData; OVERRIDE;

        procedure Sample; OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state

        property PresentState: EControlAction Read get_PresentState write set_PresentState;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    PCElement,
    Sysutils,
    uCmatrix,
    MathUtil,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TRelayObj;
    TProp = TRelayProp;
const
    NumPropsThisClass = Ord(High(TProp));

    CURRENT = 0;  {Default}
    VOLTAGE = 1;
    REVPOWER = 3;
    NEGCURRENT = 4;
    NEGVOLTAGE = 5;
    GENERIC = 6; {Use this for frequency, etc.  Generic over/under relay}
    DISTANCE = 7;
    TD21 = 8;
    DOC = 9;

    MIN_DISTANCE_REACTANCE = -1.0e-8; // allow near-bolted faults to be detected
var
    PropInfo: Pointer = NIL;    
    ActionEnum, StateEnum: TDSSEnum;
    RelayTypeEnum : TDSSEnum;

constructor TRelay.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        RelayTypeEnum := TDSSEnum.Create('Relay: Type', False, 1, 2, 
            ['Current', 'Voltage', 'ReversePower', '46', '47', 'Generic', 'Distance', 'TD21', 'DOC'], 
            [0, 1, 3, 4, 5, 6, 7, 8, 9]
        );
        RelayTypeEnum.DefaultValue := 0;
        ActionEnum := TDSSEnum.Create('Relay: Action', False, 1, 1, 
            ['close', 'open', 'trip'], 
            [ord(CTRL_CLOSE), ord(CTRL_OPEN), ord(CTRL_OPEN)]);
        StateEnum := TDSSEnum.Create('Relay: State', False, 1, 1, 
            ['closed', 'open', 'trip'], 
            [ord(CTRL_CLOSE), ord(CTRL_OPEN), ord(CTRL_OPEN)]);
    end;
    inherited Create(dssContext, RELAY_CONTROL, 'Relay');
end;

destructor TRelay.Destroy;
begin
    inherited Destroy;
end;

procedure TRelay.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
    TCC_CurveClass: TDSSClass;
begin
    TCC_CurveClass := GetDSSClassPtr(DSS, 'TCC_Curve');

    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // enums
    PropertyType[ord(TProp.typ)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.typ)] := ptruint(@obj.ControlType);
    PropertyOffset2[ord(TProp.typ)] := PtrInt(RelayTypeEnum);

    PropertyType[ord(TProp.Action)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.Action)] := ptruint(@obj.FPresentState);
    PropertyOffset2[ord(TProp.Action)] := PtrInt(ActionEnum);
    PropertyFlags[ord(TProp.Action)] := [TPropertyFlag.Redundant];
    
    PropertyType[ord(TProp.Normal)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.Normal)] := ptruint(@obj.NormalState);
    PropertyOffset2[ord(TProp.Normal)] := PtrInt(StateEnum);

    PropertyType[ord(TProp.State)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.State)] := ptruint(@obj.FPresentState);
    PropertyOffset2[ord(TProp.State)] := PtrInt(StateEnum);


    // double arrays/vectors
    PropertyType[ord(TProp.RecloseIntervals)] := TPropertyType.DoubleVArrayProperty;
    PropertyOffset[ord(TProp.RecloseIntervals)] := ptruint(@obj.RecloseIntervals);
    PropertyOffset2[ord(TProp.RecloseIntervals)] := ptruint(@obj.NumReclose);
    PropertyOffset3[ord(TProp.RecloseIntervals)] := 4;
    PropertyFlags[ord(TProp.RecloseIntervals)] := [TPropertyFlag.AllowNone, TPropertyFlag.ArrayMaxSize];

    // object properties
    PropertyType[ord(TProp.PhaseCurve)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.PhaseCurve)] := ptruint(@obj.PhaseCurve);
    PropertyOffset2[ord(TProp.PhaseCurve)] := ptruint(TCC_CurveClass);

    PropertyType[ord(TProp.Groundcurve)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.Groundcurve)] := ptruint(@obj.Groundcurve);
    PropertyOffset2[ord(TProp.Groundcurve)] := ptruint(TCC_CurveClass);

    PropertyType[ord(TProp.Overvoltcurve)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.Overvoltcurve)] := ptruint(@obj.OVCurve);
    PropertyOffset2[ord(TProp.Overvoltcurve)] := ptruint(TCC_CurveClass);

    PropertyType[ord(TProp.Undervoltcurve)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.Undervoltcurve)] := ptruint(@obj.UVCurve);
    PropertyOffset2[ord(TProp.Undervoltcurve)] := ptruint(TCC_CurveClass);

    PropertyType[ord(TProp.DOC_TDPhaseInner)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.DOC_TDPhaseInner)] := ptruint(@obj.DOC_PhaseCurveInner);
    PropertyOffset2[ord(TProp.DOC_TDPhaseInner)] := ptruint(TCC_CurveClass);


    PropertyType[ord(TProp.MonitoredObj)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.MonitoredObj)] := ptruint(@obj.FMonitoredElement);
    PropertyOffset2[ord(TProp.MonitoredObj)] := 0;
    PropertyWriteFunction[ord(TProp.MonitoredObj)] := @SetMonitoredElement;
    PropertyFlags[ord(TProp.MonitoredObj)] := [TPropertyFlag.WriteByFunction];//[TPropertyFlag.CheckForVar]; // not required for general cktelements

    PropertyType[ord(TProp.SwitchedObj)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.SwitchedObj)] := ptruint(@obj.FControlledElement);
    PropertyOffset2[ord(TProp.SwitchedObj)] := 0;
    PropertyWriteFunction[ord(TProp.SwitchedObj)] := @SetControlledElement;
    PropertyFlags[ord(TProp.SwitchedObj)] := [TPropertyFlag.WriteByFunction];

    // boolean properties
    PropertyType[ord(TProp.EventLog)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.DebugTrace)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.DistReverse)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.EventLog)] := ptruint(@obj.ShowEventLog);
    PropertyOffset[ord(TProp.DebugTrace)] := ptruint(@obj.DebugTrace);
    PropertyOffset[ord(TProp.DistReverse)] := ptruint(@obj.Dist_Reverse);

    // string properties (with lower case transformation)
    PropertyType[ord(TProp.Variable)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.Variable)] := ptruint(@obj.MonitorVariable);

    // integer properties
    PropertyType[ord(TProp.SwitchedTerm)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.MonitoredTerm)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.SwitchedTerm)] := ptruint(@obj.ElementTerminal);
    PropertyOffset[ord(TProp.MonitoredTerm)] := ptruint(@obj.MonitoredElementTerminal);

    PropertyType[ord(TProp.Shots)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Shots)] := ptruint(@obj.NumReclose);
    PropertyFlags[ord(TProp.Shots)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero, TPropertyFlag.ValueOffset];
    PropertyValueOffset[ord(TProp.Shots)] := -1;

    // double properties (default type)
    PropertyOffset[ord(TProp.PhaseTrip)] := ptruint(@obj.PhaseTrip);
    PropertyOffset[ord(TProp.GroundTrip)] := ptruint(@obj.GroundTrip);
    PropertyOffset[ord(TProp.PhaseInst)] := ptruint(@obj.PhaseInst);
    PropertyOffset[ord(TProp.GroundInst)] := ptruint(@obj.GroundInst);
    PropertyOffset[ord(TProp.Reset)] := ptruint(@obj.ResetTime);
    PropertyOffset[ord(TProp.kvbase)] := ptruint(@obj.kVBase);
    PropertyOffset[ord(TProp.Breakertime)] := ptruint(@obj.Breaker_time);
    PropertyOffset[ord(TProp.__46pctPickup)] := ptruint(@obj.PctPickup46);
    PropertyOffset[ord(TProp.__46isqt)] := ptruint(@obj.Isqt46);
    PropertyOffset[ord(TProp.__46BaseAmps)] := ptruint(@obj.BaseAmps46);
    PropertyOffset[ord(TProp.Delay)] := ptruint(@obj.Delay_Time);
    PropertyOffset[ord(TProp.__47pctPickup)] := ptruint(@obj.PctPickup47);
    PropertyOffset[ord(TProp.overtrip)] := ptruint(@obj.Overtrip);
    PropertyOffset[ord(TProp.undertrip)] := ptruint(@obj.Undertrip);
    PropertyOffset[ord(TProp.TDPhase)] := ptruint(@obj.TDPhase);
    PropertyOffset[ord(TProp.TDGround)] := ptruint(@obj.TDGround);
    PropertyOffset[ord(TProp.Z1mag)] := ptruint(@obj.Z1mag);
    PropertyOffset[ord(TProp.Z1ang)] := ptruint(@obj.Z1ang);
    PropertyOffset[ord(TProp.Z0mag)] := ptruint(@obj.Z0mag);
    PropertyOffset[ord(TProp.Z0ang)] := ptruint(@obj.Z0ang);
    PropertyOffset[ord(TProp.Mphase)] := ptruint(@obj.Mphase);
    PropertyOffset[ord(TProp.Mground)] := ptruint(@obj.Mground);
    PropertyOffset[ord(TProp.DOC_TiltAngleLow)] := ptruint(@obj.DOC_TiltAngleLow);
    PropertyOffset[ord(TProp.DOC_TiltAngleHigh)] := ptruint(@obj.DOC_TiltAngleHigh);
    PropertyOffset[ord(TProp.DOC_TripSettingLow)] := ptruint(@obj.DOC_TripSetLow);
    PropertyOffset[ord(TProp.DOC_TripSettingHigh)] := ptruint(@obj.DOC_TripSetHigh);
    PropertyOffset[ord(TProp.DOC_TripSettingMag)] := ptruint(@obj.DOC_TripSetMag);
    PropertyOffset[ord(TProp.DOC_DelayInner)] := ptruint(@obj.DOC_DelayInner);
    PropertyOffset[ord(TProp.DOC_PhaseCurveInner)] := ptruint(@obj.DOC_PhaseTripInner);
    PropertyOffset[ord(TProp.DOC_PhaseTripInner)] := ptruint(@obj.DOC_TDPhaseInner);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TRelay.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TRelayObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    case Idx of
        // Default the controlled element to the monitored element
        ord(TProp.MonitoredObj):
            ControlledElement := MonitoredElement;
        ord(TProp.MonitoredTerm):
            ElementTerminal := MonitoredElementTerminal;
        // ord(TProp.RecloseIntervals): -- changed in r3326, zero allowed
        //     if NumReclose = 0 then
        //         NumReclose := 1;
        ord(TProp.Variable):
            MonitorVariable := LowerCase(MonitorVariable);
        ord(TProp.typ):
        begin // Set Default Reclose Intervals
            // Set Definite Time Defaults
            case ControlType of
                CURRENT:
                    Delay_Time := 0.0;
                VOLTAGE:
                    Delay_Time := 0.0;
                REVPOWER:
                    Delay_Time := 0.1;
                NEGCURRENT, NEGVOLTAGE:
                    Delay_Time := 0.1;
                GENERIC:
                    Delay_Time := 0.1;
                DISTANCE:
                    Delay_Time := 0.1;
                TD21:
                    Delay_Time := 0.1;
                DOC:
                    Delay_Time := 0.0;
            else
                Delay_Time := 0.0;
            end;

            case ControlType of
                CURRENT:
                begin
                    RecloseIntervals[1] := 0.5;
                    RecloseIntervals[2] := 2.0;
                    RecloseIntervals[3] := 2.0;
                    NumReclose := 3;
                    SetAsNextSeq(Ord(TProp.Shots));
                    SetAsNextSeq(Ord(TProp.RecloseIntervals));
                end;
                VOLTAGE:
                begin
                    RecloseIntervals[3] := 5.0;
                    NumReclose := 1;
                    SetAsNextSeq(Ord(TProp.Shots));
                    SetAsNextSeq(Ord(TProp.RecloseIntervals));
                end;
                DOC:
                begin
                    NumReclose := 0;
                    SetAsNextSeq(Ord(TProp.Shots));
                    SetAsNextSeq(Ord(TProp.RecloseIntervals));
                end;
            end;
        end;
        ord(TProp.Normal):
            NormalStateSet := TRUE;
        ord(TProp.action), ord(TProp.State):
            if not NormalStateSet then
            begin
                NormalStateSet := TRUE;  // 'normal state' defaults to 'state' only when the latter is specified for the first time
                NormalState := FPresentState;
            end;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TRelayObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    FNPhases := Other.Fnphases;
    NConds := Other.Fnconds; // Force Reallocation of terminal stuff
    ShowEventLog := Other.ShowEventLog; // but leave DebugTrace off

    ElementTerminal := Other.ElementTerminal;
    ControlledElement := Other.ControlledElement;  // Pointer to target circuit element

    MonitoredElement := Other.MonitoredElement;  // Pointer to target circuit element
    MonitoredElementTerminal := Other.MonitoredElementTerminal;  // Pointer to target circuit element

    PhaseCurve := Other.PhaseCurve;
    GroundCurve := Other.GroundCurve;
    OVCurve := Other.OVCurve;
    UVcurve := Other.UVcurve;
    PhaseTrip := Other.PhaseTrip;
    GroundTrip := Other.GroundTrip;
    TDPhase := Other.TDPhase;
    TDGround := Other.TDGround;
    PhaseInst := Other.PhaseInst;
    GroundInst := Other.GroundInst;
    ResetTime := Other.Resettime;
    NumReclose := Other.NumReclose;
    Delay_Time := Other.Delay_Time;
    Breaker_time := Other.Breaker_time;

    Reallocmem(RecloseIntervals, SizeOf(RecloseIntervals^[1]) * 4);      // Always make a max of 4
    for i := 1 to NumReclose do
        RecloseIntervals^[i] := Other.RecloseIntervals^[i];

    kVBase := Other.kVBase;
    LockedOut := Other.LockedOut;

    FPresentState := Other.FPresentState;
    NormalState := Other.NormalState;
    NormalStateSet := Other.NormalStateSet;

    ControlType := Other.ControlType;
    CondOffset := Other.CondOffset;

    // 46 Relay  Neg Seq Current
    PickupAmps46 := Other.PickupAmps46;
    PctPickup46 := Other.PctPickup46;
    BaseAmps46 := Other.BaseAmps46;
    Isqt46 := Other.Isqt46;

    // 47 Relay
    PickupVolts47 := Other.PickupVolts47;
    PctPickup47 := Other.PctPickup47;

    // Generic Relay
    MonitorVariable := Other.MonitorVariable;
    OverTrip := Other.OverTrip;
    UnderTrip := Other.UnderTrip;

    // Distance Relays
    Z1Mag := Other.Z1Mag;
    Z1Ang := Other.Z1Ang;
    Z0Mag := Other.Z0Mag;
    Z0Ang := Other.Z0Ang;
    Mphase := Other.Mphase;
    Mground := Other.Mground;
    Dist_Reverse := Other.Dist_Reverse;

    // Directional Overcurrent Relay
    DOC_TiltAngleLow := Other.DOC_TiltAngleLow;
    DOC_TiltAngleHigh := Other.DOC_TiltAngleHigh;
    DOC_TripSetLow := Other.DOC_TripSetLow;
    DOC_TripSetHigh := Other.DOC_TripSetHigh;
    DOC_TripSetMag := Other.DOC_TripSetMag;
    DOC_DelayInner := Other.DOC_DelayInner;
    DOC_PhaseCurveInner := Other.DOC_PhaseCurveInner;
    DOC_TDPhaseInner := Other.DOC_TDPhaseInner;
    DOC_PhaseTripInner := Other.DOC_PhaseTripInner;
end;

constructor TRelayObj.Create(ParClass: TDSSClass; const RelayName: String);
begin
    inherited Create(ParClass);
    Name := LowerCase(RelayName);
    DSSObjType := ParClass.DSSClassType;

    DebugTrace := FALSE;

    FNPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors in base class

    ControlledElement := NIL;
    PreviousControlledElement := NIL;
    ElementTerminal := 1;

    MonitoredElementTerminal := 1;
    MonitoredElement := NIL;

    RelayTarget := '';

    PhaseCurve := NIL;
    GroundCurve := NIL;
    OVCurve := NIL;
    UVcurve := NIL;
    PhaseTrip := 1.0;
    GroundTrip := 1.0;
    TDPhase := 1.0;
    TDGround := 1.0;
    PhaseInst := 0.0;
    GroundInst := 0.0;
    ResetTime := 15.0;
    NumReclose := 3;
    RecloseIntervals := NIL;

    Reallocmem(RecloseIntervals, SizeOf(RecloseIntervals^[1]) * 4); // fixed allocation of 4
    RecloseIntervals^[1] := 0.5;
    RecloseIntervals^[2] := 2.0;
    RecloseIntervals^[3] := 2.0;

    FPresentState := CTRL_CLOSE;
    NormalState := CTRL_CLOSE;
    NormalStateSet := FALSE;

    Isqt46 := 1.0;
    BaseAmps46 := 100.0;
    PctPickup46 := 20.0;
    PickupAmps46 := BaseAmps46 * PctPickup46 * 0.01;

    PctPickup47 := 2.0;

    overtrip := 1.2;
    undertrip := 0.8;

    Z1Mag := 0.7;
    Z1Ang := 64.0;
    Z0Mag := 2.1;
    Z0Ang := 68.0;
    Mphase := 0.7;
    Mground := 0.7;
    td21_i := -1;
    td21_h := NIL;
    td21_dV := NIL;
    td21_Uref := NIL;
    td21_dI := NIL;
    td21_pt := 0;
    td21_stride := 0;
    td21_quiet := 0;
    Dist_Reverse := FALSE;

    DOC_TiltAngleLow := 90.0;
    DOC_TiltAngleHigh := 90.0;
    DOC_TripSetLow := 0;
    DOC_TripSetHigh := -1.0;
    DOC_TripSetMag := -1.0;

    DOC_DelayInner := -1.0;
    DOC_PhaseCurveInner := NIL;
    DOC_PhaseTripInner := 1.0;
    DOC_TDPhaseInner := 1.0;

    Operationcount := 1;
    LockedOut := FALSE;
    ArmedForOpen := FALSE;
    ArmedForClose := FALSE;
    ArmedForReset := FALSE;
    PhaseTarget := FALSE;
    GroundTarget := FALSE;

    NextTripTime := -1.0;  // not set to trip

    cBuffer := NIL; // Complex buffer
    cvBuffer := NIL;

    DSSObjType := ParClass.DSSClassType; //cap_CONTROL;

   //  RecalcElementData;
end;

destructor TRelayObj.Destroy;
begin
    ReallocMem(RecloseIntervals, 0);
    if Assigned(cBuffer) then
        ReallocMem(cBuffer, 0);
    if Assigned (cvBuffer) then
        ReallocMem(cvBuffer, 0);
    if Assigned (td21_h) then
        ReallocMem (td21_h, 0);
    if Assigned (td21_dV) then
        ReallocMem (td21_dV, 0);
    if Assigned (td21_Uref) then
        ReallocMem (td21_Uref, 0);
    if Assigned (td21_dI) then
        ReallocMem (td21_dI, 0);

    inherited Destroy;
end;

procedure TRelayObj.RecalcElementData;
begin
    if DebugTrace then
        AppendToEventLog(
            'Relay.' + self.Name,
            Format('RecalcElementData NumReclose=%d', [NumReclose])
        );

    if MonitoredElement <> NIL then
    begin
        FNphases := MonitoredElement.NPhases;       // Force number of phases to be same
        if MonitoredElementTerminal > MonitoredElement.Nterms then
        begin
            DoErrorMsg(Format(_('Relay: "%s"'), [Name]),
                Format(_('Terminal no. "%d" does not exist.'), [MonitoredElementTerminal]),
                _('Re-specify terminal no.'), 384);
        end
        else
        begin
            // Sets name of i-th terminal's connected bus in Relay's buslist
            Setbus(1, MonitoredElement.GetBus(MonitoredElementTerminal));

            // Allocate a buffer big enough to hold everything from the monitored element
            ReAllocMem(cBuffer, SizeOf(cbuffer^[1]) * MonitoredElement.Yorder);

            if (ControlType = Distance) or (ControlType = TD21) or (ControlType = DOC) then
                ReAllocMem(cvBuffer, SizeOf(cvBuffer^[1]) * MonitoredElement.Yorder);

            CondOffset := (MonitoredElementTerminal - 1) * MonitoredElement.NConds; // for speedy sampling

            case ControlType of
                Generic:
                begin
                    if (MonitoredElement.DSSObjType and BASECLASSMASK) <> PC_ELEMENT then
                        DoSimpleMsg('Relay %s: Monitored element for Generic relay is not a PC Element.', [Name], 385)
                    else
                    begin
                        MonitorVarIndex := (MonitoredElement as TPCelement).LookupVariable(MonitorVariable);
                        if MonitorVarIndex < 1 then    // oops
                        begin
                            DoSimpleMsg('Relay "%s": Monitor variable "%s" does not exist.', [Name, MonitorVariable], 386);
                        end;
                    end;

                end;
            end;
        end;
    end;

    {Check for existence of Controlled Element}

    // If previously assigned, reset HasOCPDevice flag in case this is a move
    if (PreviousControlledElement <> NIL) then
    begin
        Exclude(PreviousControlledElement.Flags, Flg.HasOCPDevice);
        Exclude(PreviousControlledElement.Flags, Flg.HasAutoOCPDevice);
        PreviousControlledElement := ControlledElement;
    end;

    if ControlledElement <> NIL then
    begin  // Both CktElement and monitored element must already exist
        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Make the 1 st terminal active

        // If the relay becomes disabled, leave at False
        if Enabled then
        begin
            Include(ControlledElement.Flags, Flg.HasOCPDevice);  // For Reliability calcs
            Include(ControlledElement.Flags, Flg.HasAutoOCPDevice);  // For Reliability calcs
        end;

        // Open/Close State of controlled element based on state assigned to the control
        if FPresentState = CTRL_CLOSE then
        begin
            ControlledElement.Closed[0] := TRUE;
            LockedOut := FALSE;
            OperationCount := 1;
            ArmedForOpen := FALSE;
        end
        else
        begin
            ControlledElement.Closed[0] := FALSE;
            LockedOut := TRUE;
            OperationCount := NumReclose + 1;
            ArmedForClose := FALSE;
        end;
    end
    else
    begin
        // element not found/set
        DoErrorMsg(Format(_('Relay: "%s"'), [Self.Name]), 
            _('CktElement for SwitchedObj is not set.'),
            _('Element must be defined previously.'), 387);
    end;

    // Misc stuff

    PickupAmps46 := BaseAmps46 * PctPickup46 * 0.01;

    case FNPhases of
        1:
            vbase := kVBase * 1000.0;
    else
        vbase := kVBase / SQRT3 * 1000.0;
    end;

    PickupVolts47 := vbase * PctPickup47 * 0.01;

    if (ControlType = DISTANCE) or (ControlType = TD21) then
    begin
        Dist_Z1 := pclx(Z1Mag, Z1Ang / RadiansToDegrees);
        Dist_Z0 := pclx(Z0Mag, Z0Ang / RadiansToDegrees);
        Dist_K0 := ((Dist_Z0 - Dist_Z1) / 3.0) / Dist_Z1;
    end;
end;

procedure TRelayObj.MakePosSequence();
begin
    if MonitoredElement <> NIL then
    begin
        FNphases := MonitoredElement.NPhases;
        Nconds := FNphases;
        Setbus(1, MonitoredElement.GetBus(ElementTerminal));

        // Allocate a buffer big enough to hold everything from the monitored element
        ReAllocMem(cBuffer, SizeOf(cbuffer^[1]) * MonitoredElement.Yorder);

        if (ControlType = Distance) or (ControlType = TD21) or (ControlType = DOC) then
            ReAllocMem(cvBuffer, SizeOf(cvBuffer^[1]) * MonitoredElement.Yorder);

        CondOffset := (ElementTerminal - 1) * MonitoredElement.NConds; // for speedy sampling
    end;
    case FNPhases of
        1:
            vbase := kVBase * 1000.0;
    else
        vbase := kVBase / SQRT3 * 1000.0;
    end;
    PickupVolts47 := vbase * PctPickup47 * 0.01;
    inherited;
end;

procedure TRelayObj.DoPendingAction(const Code, ProxyHdl: Integer);
begin
    if DebugTrace then
        AppendToEventLog('Relay.' + self.Name, Format(
            'DoPendingAction Code=%d State=%d ArmedOpen=%s Close=%s Reset=%s Count=%d NumReclose=%d', [
                Integer (Code),
                Integer (FPresentState),
                BoolToStr(ArmedForOpen),
                BoolToStr(ArmedForClose),
                BoolToStr(ArmedForReset),
                OperationCount,
                NumReclose
        ]));

    ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal of CktElement to terminal 1

    with ControlledElement do
        case Code of
            Integer(CTRL_OPEN):
                if FPresentState = CTRL_CLOSE then
                    if ArmedForOpen then
                    begin   // ignore if we became disarmed in meantime
                        ControlledElement.Closed[0] := FALSE;   // Open all phases of active terminal
                        if (OperationCount > NumReclose) then
                        begin
                            LockedOut := TRUE;
                            if ShowEventLog then
                                AppendtoEventLog('Relay.' + Self.Name, Format(_('Opened on %s & Locked Out'), [RelayTarget]));
                        end
                        else
                        if ShowEventLog then
                            AppendtoEventLog('Relay.' + Self.Name, Format(_('Opened on %s'), [RelayTarget]));

                        if PhaseTarget and ShowEventLog then
                            AppendtoEventLog(' ', _('Phase Target'));
                        if GroundTarget and ShowEventLog then
                            AppendtoEventLog(' ', _('Ground Target'));

                        ArmedForOpen := FALSE;

                        if ControlType = td21 then
                            td21_quiet := td21_pt + 1;
                    end;

            Integer(CTRL_CLOSE):
                if FPresentState = CTRL_OPEN then
                    if ArmedForClose and not LockedOut then
                    begin
                        ControlledElement.Closed[0] := TRUE; // Close all phases of active terminal
                        Inc(OperationCount);
                        if ShowEventLog then
                            AppendtoEventLog('Relay.' + Self.Name, _('Closed'));

                        ArmedForClose := FALSE;

                        if ControlType = td21 then
                            td21_quiet := td21_pt div 2;
                    end;

            Integer(CTRL_RESET):
                if ArmedForClose and not LockedOut then
                begin
                    if ShowEventLog then
                        if ShowEventLog then AppendToEventLog('Relay.'+Self.Name, _('Reset'));

                    Reset();

                    if ControlType = td21 then
                        td21_quiet := td21_pt div 2
                end;

        end;
end;

procedure TRelayObj.Sample;
begin
    ControlledElement.ActiveTerminalIdx := ElementTerminal;
    if ControlledElement.Closed[0] // Check state of phases of active terminal
    then
        FPresentState := CTRL_CLOSE
    else
        FPresentState := CTRL_OPEN;

    case ControlType of
        CURRENT:
            OverCurrentLogic; // Current
        VOLTAGE:
            VoltageLogic; // Reclosing Voltage Relay - definite time
        REVPOWER:
            RevPowerLogic; // one shot to lockout
        NEGCURRENT:
            NegSeq46Logic; // one shot to lockout
        NEGVOLTAGE:
            NegSeq47Logic; // one shot to lockout
        GENERIC:
            GenericLogic; // one shot to lockout
        DISTANCE:
            DistanceLogic;
        TD21:
            TD21Logic;
        DOC:
            DirectionalOvercurrentLogic;
    end;
end;

procedure TRelayObj.Reset;
begin
    if ShowEventLog then
        AppendToEventLog ('Relay.' + self.Name, _('Resetting'));

    FPresentState := NormalState;

    ArmedForOpen := FALSE;
    ArmedForClose := FALSE;
    ArmedForReset := FALSE;
    PhaseTarget := FALSE;
    GroundTarget := FALSE;

    NextTripTime := -1.0;  // not set to trip

    if ControlledElement = NIL then
        Exit;

    ControlledElement.ActiveTerminalIdx := ElementTerminal;

    if NormalState = CTRL_OPEN then
    begin
        ControlledElement.Closed[0] := FALSE; // Open all phases of active terminal
        LockedOut := TRUE;
        OperationCount := NumReclose + 1;
    end
    else
    begin
        ControlledElement.Closed[0] := TRUE; // Close all phases of active terminal
        LockedOut := FALSE;
        OperationCount := 1;
    end;
end;

function TRelayObj.get_PresentState: EControlAction;
begin
    if ControlledElement <> NIL then
    begin
        ControlledElement.ActiveTerminalIdx := ElementTerminal;

        if not ControlledElement.Closed[0] then
            FPresentState:= CTRL_OPEN
        else
            FPresentState:= CTRL_CLOSE;
    end;
    Result := FPresentState;
end;

procedure TRelayObj.set_PresentState(const Value: EControlAction);
begin
    if FPresentState = Value then
        Exit;

    FPresentState := Value;

    if ControlledElement = NIL then
        Exit;

    ControlledElement.ActiveTerminalIdx := ElementTerminal;
    if Value = CTRL_OPEN then
    begin
        ControlledElement.Closed[0] := FALSE;
        LockedOut := TRUE;
        OperationCount := NumReclose + 1;
        ArmedForClose := FALSE;
        ArmedForReset := FALSE;
    end
    else
    begin
        ControlledElement.Closed[0] := TRUE;
        LockedOut := FALSE;
        OperationCount := 1;
        ArmedForOpen := FALSE;
        ArmedForReset := FALSE;
    end;
end;

procedure TRelayObj.GenericLogic;
// Generic relays only work on PC Elements With control terminals
var
    VarValue: Double;
begin
    with MonitoredElement do
    begin
        VarValue := TPCElement(MonitoredElement).Variable[MonitorVarIndex];

        // Check for Trip
        if (VarValue > OverTrip) or (VarValue < UnderTrip) then
        begin
            if not ArmedForOpen then  // push the trip operation and arm to trip
                with ActiveCircuit do
                begin
                    RelayTarget := TPCElement(MonitoredElement).VariableName(MonitorVarIndex);
                    LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + Delay_Time + Breaker_time, CTRL_OPEN, 0, Self);
                    OperationCount := NumReclose + 1;  // force a lockout
                    ArmedForOpen := TRUE;
                end
        end
        else // Within bounds
        begin // Less Than pickup value: reset if armed
            if ArmedForOpen then    // We became unarmed, so reset and disarm
                with ActiveCircuit do
                begin
                    LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self);
                    ArmedForOpen := FALSE;
                end;
        end;
    end;  {With MonitoredElement}
end;

procedure TRelayObj.NegSeq46Logic;
// Negative Sequence Current Relay
// Patterned after Basler relay
var
    NegSeqCurrentMag, TripTime: Double;
    iOffset: Integer;
    I012: array[1..3] of Complex;
begin
    with MonitoredElement do
    begin
        MonitoredElement.ActiveTerminalIdx := MonitoredElementTerminal;
        MonitoredElement.GetCurrents(cBuffer);
        iOffset := (MonitoredElementTerminal - 1) * MonitoredElement.NConds;  // offset for active terminal
        Phase2SymComp(pComplexArray(@cBuffer^[iOffset + 1]), pComplexArray(@I012));
        NegSeqCurrentMag := Cabs(I012[3]);
        if NegSeqCurrentMag >= PickupAmps46 then
        begin
            if not ArmedForOpen then  // push the trip operation and arm to trip
                with ActiveCircuit do
                begin
                    RelayTarget := '-Seq Curr';
                    // simple estimate of trip time assuming current will be constant
                    if Delay_Time > 0.0 then
                        Triptime := Delay_Time
                    else
                        Triptime := Isqt46 / sqr(NegSeqCurrentMag / BaseAmps46); // Sec
                    LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + Breaker_time, CTRL_OPEN, 0, Self);
                    OperationCount := NumReclose + 1;  // force a lockout
                    ArmedForOpen := TRUE;
                end
        end
        else
        begin // Less Than pickup value: reset if armed
            if ArmedForOpen then    // We became unarmed, so reset and disarm
                with ActiveCircuit do
                begin
                    LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self);
                    ArmedForOpen := FALSE;
                end;
        end;
    end; // With MonitoredElement
end;

procedure TRelayObj.OvercurrentLogic;
var
    i: Integer;
    Cmag: Double;
    CSum: Complex;

    GroundTime,
    PhaseTime,
    TripTime,
    TimeTest: Double;
begin
    with MonitoredElement do
    begin
        if FPresentState = CTRL_CLOSE then
        begin
            TripTime := -1.0;
            GroundTime := -1.0;
            PhaseTime := -1.0;  {No trip}

            // Check largest Current of all phases of monitored element
            MonitoredElement.GetCurrents(cBuffer);

            // Check Ground Trip, if any
            if ((GroundCurve <> NIL) or (Delay_Time > 0.0)) and (GroundTrip > 0.0) then
            begin
                Csum := CZERO;
                for i := (1 + CondOffset) to (Fnphases + CondOffset) do
                begin
                    Csum += cBuffer^[i];
                end;
                Cmag := Cabs(Csum);
                if (GroundInst > 0.0) and (Cmag >= GroundInst) and (OperationCount = 1) then
                    GroundTime := 0.01 + Breaker_time      // Inst trip on first operation
                else
                if Delay_Time > 0.0 then
                begin // Definite Time Ground Relay
                    if (Cmag >= GroundTrip) then
                        GroundTime := Delay_Time
                    else
                        GroundTime := -1.0;
                end
                else
                    GroundTime := TDGround * GroundCurve.GetTCCTime(Cmag / GroundTrip);

                if DebugTrace then
                    AppendToEventLog('Relay.' + Self.Name, Format(
                        _('Ground Trip: Mag=%.3g, Mult=%.3g, Time=%.3g'),
                        [Cmag, Cmag / GroundTrip, GroundTime]
                    ));
            end;

            if Groundtime > 0.0 then
            begin
                TripTime := GroundTime;
                GroundTarget := TRUE;
            end;

            // If GroundTime > 0 then we have a ground trip

            // Check Phase Trip, if any

            if ((PhaseCurve <> NIL) or (Delay_Time > 0.0)) and (PhaseTrip > 0.0) then
            begin
                for i := (1 + CondOffset) to (Fnphases + CondOffset) do
                begin
                    Cmag := Cabs(cBuffer^[i]);
                    if (PhaseInst > 0.0) and (Cmag >= PhaseInst) and (OperationCount = 1) then
                    begin
                        PhaseTime := 0.01 + Breaker_time;  // Inst trip on first operation
                        Break;  {FOR - if Inst, no sense checking other phases}
                    end
                    else
                    begin
                        if Delay_Time > 0.0 then
                        begin // Definite Time Phase Relay
                            if (Cmag >= PhaseTrip) then
                                TimeTest := Delay_Time
                            else
                                TimeTest := -1.0;
                        end
                        else
                            TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag / PhaseTrip);
                        if (TimeTest > 0.0) then
                        begin
                            if Phasetime < 0.0 then
                                PhaseTime := TimeTest
                            else
                                PhaseTime := Min(PhaseTime, TimeTest);
                        end;
                    end;
                end;

                if DebugTrace then
                    AppendToEventLog(
                        'Relay.' + Self.Name, Format(
                        _('Phase %d Trip: Mag=%.3g, Mult=%.3g, Time=%.3g'),
                        [i-CondOffset, Cmag, Cmag / PhaseTrip, PhaseTime]
                    ));
            end;
            // If PhaseTime > 0 then we have a phase trip

            if PhaseTime > 0.0 then
            begin
                PhaseTarget := TRUE;
                if TripTime > 0.0 then
                    TripTime := Min(TripTime, Phasetime)
                else
                    TripTime := PhaseTime;
            end;

            if TripTime > 0.0 then
            begin
                if not ArmedForOpen then
                    with ActiveCircuit do   // Then arm for an open operation
                    begin
                        RelayTarget := '';
                        if Phasetime > 0.0 then
                            RelayTarget := RelayTarget + 'Ph';
                        if Groundtime > 0.0 then
                            RelayTarget := RelayTarget + ' Gnd';
                        LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + Breaker_time, CTRL_OPEN, 0, Self);
                        if OperationCount <= NumReclose then
                            LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + Breaker_time + RecloseIntervals^[OperationCount], CTRL_CLOSE, 0, Self);
                        ArmedForOpen := TRUE;
                        ArmedForClose := TRUE;
                    end;
            end
            else
            begin
                if ArmedForOpen then
                    with ActiveCircuit do    // If current dropped below pickup, disarm trip and set for reset
                    begin
                        LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self);
                        ArmedForOpen := FALSE;
                        ArmedForClose := FALSE;
                        PhaseTarget := FALSE;
                        GroundTarget := FALSE;
                    end;
            end;
        end; // IF PresentState=CLOSE
    end; // With MonitoredElement
end;

procedure TRelayObj.DistanceLogic;
var
    i, j: Integer;
    Vloop, Iloop, Zloop, Ires, kIres, Zreach: Complex;
    i2, min_distance, fault_distance, t_event: Double;
    Targets: TStringList = NIL;
    PickedUp: Boolean;
begin
    If LockedOut Then
        Exit;

    with MonitoredElement do
    begin
        PickedUp := False;
        min_distance := 1.0e30;
        MonitoredElement.GetCurrents(cBuffer);

        if Dist_Reverse then
            for i := 1 to MonitoredElement.NPhases do
                cBuffer^[i + CondOffset] := -cBuffer^[i + CondOffset];

        Ires := cZERO;
        for i := 1 to MonitoredElement.Nphases do
            Ires += cBuffer^[i + CondOffset];

        kIres := Dist_K0 * Ires;
        MonitoredElement.GetTermVoltages(MonitoredElementTerminal, cvBuffer);

        for i := 1 to MonitoredElement.NPhases do
        begin
            for j := i to MonitoredElement.NPhases do
            begin
                if (i = j) then
                begin
                    Vloop := cvBuffer^[i];
                    Iloop := cBuffer^[i + CondOffset] + kIres;
                    Zreach := Dist_Z1 * Mground; // not Dist_Z0 because it's included in Dist_K0
                end
                else
                begin
                    Vloop := cvBuffer^[i] - cvBuffer^[j];
                    Iloop := cBuffer^[i + CondOffset] - cBuffer^[j + CondOffset];
                    Zreach := Dist_Z1 * Mphase;
                end;

                i2 := Iloop.re * Iloop.re + Iloop.im * Iloop.im;
                if i2 > 0.1 then
                begin
                    Zloop := Vloop / Iloop;

                    // start with a very simple rectangular characteristic
                    if DebugTrace and (ActiveCircuit.Solution.DynaVars.t > 0.043) then
                        AppendToEventLog(self.FullName, Format('Zloop[%d,%d]=%.4f+j%.4f', [i, j, Zloop.re, Zloop.im]));

                    if (Zloop.re >= 0) and (Zloop.im >= MIN_DISTANCE_REACTANCE) and (Zloop.re <= Zreach.re) and (Zloop.im <= Zreach.im) then
                    begin
                        if not PickedUp then
                        begin
                            Targets := TStringList.Create();
                            Targets.Sorted := True;
                        end;
                        if (i = j) then
                        begin
                            Targets.Add(Format('G%d', [i]));
                        end
                        else
                        begin
                            Targets.Add(Format('P%d%d', [i, j]));
                        end;

                        fault_distance := cabs2(zloop) / cabs2 (zreach);
                        if fault_distance < min_distance then
                            min_distance := fault_distance;

                        PickedUp := True;
                    end;
                end;
            end;
        end;

        if PickedUp then
        begin
            if DebugTrace then
                AppendToEventLog ('Relay.' + Self.Name, 'Picked up');

            if ArmedForReset then
            begin
                ActiveCircuit.ControlQueue.Delete(LastEventHandle);
                ArmedForReset := FALSE;
            end;

            if not ArmedForOpen then
                with ActiveCircuit do
                begin
                    RelayTarget := Format(_('21 %.3f pu dist'), [min_distance]);
                    t_event := Solution.DynaVars.t + Delay_Time + Breaker_time;
                    for i := 0 to pred(Targets.Count) do
                        RelayTarget := RelayTarget + ' ' + Targets[i];

                    LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, t_event, CTRL_OPEN, 0, Self);
                    ArmedForOpen := TRUE;
                    if OperationCount <= NumReclose then
                    begin
                        LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, t_event + RecloseIntervals^[OperationCount], CTRL_CLOSE, 0, Self);
                        ArmedForClose := TRUE;
                    end;
                end;

            Targets.Free();
        end
        else
        begin  // not picked up; reset if necessary
            if (OperationCount > 1) and (ArmedForReset = FALSE) then
            begin // this implements the reset, whether picked up or not
                ArmedForReset := TRUE;
                with ActiveCircuit do
                    LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self);
            end;
            if ArmedForOpen then
            begin // this implements the drop-out, if picked up
                ArmedForOpen := FALSE;
                ArmedForClose := FALSE;
            end;
        end;
    end; // With MonitoredElement
end;

procedure TRelayObj.TD21Logic;
var
    i, j: Integer;
    Vloop, Iloop, Zhsd, Zdir, Uhsd, Uref, Ires, kIres: Complex;
    i2, i2fault, min_distance, fault_distance, Uref2, Uhsd2, t_event, dt: Double;
    Targets: TStringList = NIL;
    PickedUp, FaultDetected: Boolean;
    ib, iv, ii: Integer;
begin
    dt := ActiveCircuit.Solution.DynaVars.h;
    if dt > 0.0 then
    begin
        if dt > 1.0 / ActiveCircuit.Solution.Frequency then
            DoErrorMsg(Format(_('Relay: "%s"'), [Name]),
                _('Has type TD21 with time step greater than one cycle.'),
                _('Reduce time step, or change type to Distance.'),
                388
            );

        i := round (1.0 / 60.0 / dt + 0.5);
        if i > td21_pt then
        begin
            td21_i := 0; // ring buffer index to be incremented before actual use
            td21_pt := i;
            td21_quiet := td21_pt + 1;
            td21_stride := 2 * Nphases;
            ReAllocMem(td21_h, SizeOf(td21_h^[1]) * td21_stride * td21_pt);
            ReAllocMem(td21_dV, SizeOf(td21_dV^[1]) * Nphases);
            ReAllocMem(td21_Uref, SizeOf(td21_Uref^[1]) * Nphases);
            ReAllocMem(td21_dI, SizeOf(td21_dI^[1]) * Nphases);
            if DebugTrace then
                AppendToEventLog(self.FullName,
                    Format(
                        _('TD21 prep %d phases, %.3g dt, %d points, %d elements'),
                        [NPhases, dt, td21_pt, td21_stride * td21_pt]
                    )
                );
        end;
    end;

    if LockedOut then
        Exit;

    with MonitoredElement Do
    begin
        FaultDetected := False;
        MonitoredElement.GetCurrents(cBuffer);

        if Dist_Reverse then
            for i := 1 to MonitoredElement.NPhases do
                cBuffer^[i+CondOffset] := -cBuffer^[i+CondOffset];

        i2fault := PhaseTrip * PhaseTrip;
        for i := 1 to Nphases do
        begin
            i2 := cabs2 (cBuffer^[i+CondOffset]);
            if i2 > i2fault then
                FaultDetected := True;
        end;

        if DebugTrace then
            AppendToEventLog('Relay.' + self.Name, Format(
                'FaultDetected=%s',
                [BoolToStr(FaultDetected)]
            ));

        MonitoredElement.GetTermVoltages(MonitoredElementTerminal, cvBuffer);
        if td21_i < 1 then
        begin
            if DebugTrace then
                AppendToEventLog ('Relay.' + self.Name, 'Initialize cqueue');

            for i := 1 to td21_pt do
            begin
                ib := (i - 1) * td21_stride;
                for j := 1 to Nphases do
                begin
                    iv := ib + j;
                    td21_h^[iv] := cvBuffer^[j];
                    ii := ib + Nphases + j;
                    td21_h^[ii] := cBuffer^[j+CondOffset];
                end;
            end;
            td21_i := 1;
        end;

        td21_next := (td21_i mod td21_pt) + 1;  // this points to the oldest sample, and the next write location

        // calculate the differential currents and voltages
        ib := (td21_next - 1) * td21_stride;
        for j := 1 to Nphases do
        begin
            iv := ib + j;
            td21_Uref^[j] := td21_h^[iv];
            td21_dV^[j] := cvBuffer^[j] - td21_h^[iv];
            ii := ib + Nphases + j;
            td21_dI^[j] := cBuffer^[j+CondOffset] - td21_h^[ii];
        end;

        // do the relay processing
        if ActiveCircuit.Solution.DynaVars.IterationFlag < 1 then
        begin
            ib := (td21_i - 1) * td21_stride;
            for j := 1 to Nphases do
            begin
                iv := ib + j;
                td21_h^[iv] := cvBuffer^[j];
                ii := ib + Nphases + j;
                td21_h^[ii] := cBuffer^[j+CondOffset];
            end;
            td21_i := td21_next;

            if td21_quiet > 0 then
                dec(td21_quiet);
        end;

        if td21_quiet <= 0 then
        begin  // one cycle since we started, or since the last operation
            PickedUp := False;
            min_distance := 1.0e30;
            Ires := cZERO;
            for i := 1 to MonitoredElement.Nphases do
                Ires += td21_dI^[i];

            kIres := Dist_K0 * Ires;
            for i := 1 to MonitoredElement.NPhases do
            begin
                for j := i to MonitoredElement.NPhases do
                begin
                    if (i = j) then
                    begin
                        Uref := td21_Uref^[i];
                        Vloop := td21_dV^[i];
                        Iloop := td21_dI^[i] + kIres;
                        Zhsd := Dist_Z1 * Mground; // not Dist_Z0 because it's included in Dist_K0
                    end
                    else
                    begin
                        Uref := td21_Uref^[i] - td21_Uref^[j];
                        Vloop := td21_dV^[i] - td21_dV^[j];
                        Iloop := td21_dI^[i] - td21_dI^[j];
                        Zhsd := Dist_Z1 * Mphase;
                    end;

                    i2 := cabs2 (Iloop);
                    Uref2 := cabs2 (Uref);
                    if FaultDetected and (i2 > 0.1) and (Uref2 > 0.1) then
                    begin
                        Zdir := -(Vloop / Iloop);
                        if DebugTrace then
                            AppendToEventLog('Relay.' + self.Name, Format(
                                'Zhsd[%d,%d]=%.4f+j%.4f, Zdir=%.4f+j%.4f',
                                [i, j, Zhsd.re, Zhsd.im, Zdir.re, Zdir.im]
                            ));

                        if (Zdir.re > 0.0) and (Zdir.im > 0.0) then
                        begin
                            Uhsd := Zhsd * Iloop - Vloop;
                            Uhsd2 := cabs2 (Uhsd);
                            if DebugTrace then
                                AppendToEventLog('Relay.' + self.Name, Format(
                                    '     Uhsd=%.2f, Uref=%.2f',
                                    [cabs(Uhsd), cabs(Uref)]
                                ));

                            if Uhsd2 / Uref2 > 1.0 then
                            begin // this loop trips
                                if not PickedUp then
                                begin
                                    Targets := TStringList.Create();
                                    Targets.Sorted := True;
                                end;
                                if (i = j) then
                                    Targets.Add(Format('G%d', [i]))
                                else
                                    Targets.Add(Format('P%d%d', [i, j]));

                                fault_distance := 1.0 / sqrt(Uhsd2 / Uref2);
                                if fault_distance < min_distance then
                                    min_distance := fault_distance;

                                PickedUp := True;
                            end;
                        end;
                    end;
                end;
            end;

            if PickedUp then
            begin
                if DebugTrace then
                    AppendToEventLog ('Relay.'+Self.Name, 'Picked up');

                if ArmedForReset then
                begin
                    ActiveCircuit.ControlQueue.Delete(LastEventHandle);
                    ArmedForReset := FALSE;
                    if DebugTrace then
                        AppendToEventLog('Relay.' + self.Name, 'Dropping last event.');
                end;

                if not ArmedForOpen then
                    with ActiveCircuit do
                    begin
                        RelayTarget := Format ('TD21 %.3f pu dist', [min_distance]);
                        t_event := Solution.DynaVars.t + Delay_Time + Breaker_time;
                        for i := 0 to pred(Targets.Count) do
                            RelayTarget := RelayTarget + ' ' + Targets[i];

                        LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, t_event, CTRL_OPEN, 0, Self);
                        if DebugTrace then
                            AppendToEventLog('Relay.' + self.Name, Format('Pushing trip event for %.3f', [t_event]));

                        ArmedForOpen := TRUE;

                        if OperationCount <= NumReclose then
                        begin
                            LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, t_event + RecloseIntervals^[OperationCount], CTRL_CLOSE, 0, Self);
                            if DebugTrace then
                                AppendToEventLog ('Relay.' + self.Name, Format(
                                    'Pushing reclose event for %.3f',
                                    [t_event + RecloseIntervals^[OperationCount]]
                                ));
                            ArmedForClose := TRUE;
                        end;
                    end;

                Targets.Free();
            end;

            if not FaultDetected then
            begin  // not picked up; reset if necessary
                if (OperationCount > 1) and (not ArmedForReset) then
                begin // this implements the reset, whether picked up or not
                    ArmedForReset := TRUE;
                    with ActiveCircuit do
                        LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self);

                    if DebugTrace then
                        AppendToEventLog('Relay.' + self.Name, Format(
                            'Pushing reset event for %.3f',
                            [ActiveCircuit.Solution.DynaVars.t + ResetTime]
                        ));
                end;

                if ArmedForOpen then
                begin
                    td21_quiet := td21_pt + 1;
                    ArmedForOpen := FALSE;
                    ArmedForClose := FALSE;
                    if DebugTrace then
                        AppendToEventLog('Relay.' + self.Name, Format (
                            'Dropping out at %.3f',
                            [ActiveCircuit.Solution.DynaVars.t]
                        ));
                end;
            end;
        end; // td21_quiet
    end; // With MonitoredElement
end;

procedure TRelayObj.DirectionalOvercurrentLogic();
var
    i: Integer;
    TripTime, TimeTest: Double;
    Cmag, Cangle: Double;
begin
    with MonitoredElement do
    begin
        if FPresentState = CTRL_CLOSE then
        begin
            TripTime := -1.0;

            MonitoredElement.GetCurrents(cBuffer);
            MonitoredElement.GetTermVoltages(MonitoredElementTerminal, cvBuffer);

            // Shift angle to cBuffer to be relative to cvBuffer
            for i := (1 + CondOffset) to (Fnphases + CondOffset) do
                cBuffer^[i] := PDEGtoCompLeX(Cabs(cBuffer^[i]), CDANG(cBuffer^[i]) - CDANG(cvBuffer^[i - CondOffset]));

            for i := (1 + CondOffset) to (Fnphases + CondOffset) do
            begin
                TimeTest := -1.0;
                Cmag := Cabs(cBuffer^[i]);
                Cangle := Cdang(cBuffer^[i]);

                if (DOC_TiltAngleLow = 90.0) or (DOC_TiltAngleLow = 270.0) then
                begin
                    if cBuffer^[i].re <= -1 * DOC_TripSetLow then
                    begin
                        if (DOC_TripSetMag > 0.0) then
                        begin // Circle Specified.
                            if Cmag <= DOC_TripSetMag then
                            begin // Within the Circle
                                if DOC_TripSetHigh > 0.0 then // High Straight-Line Specified.
                                begin
                                    if (DOC_TiltAngleHigh = 90.0) or (DOC_TiltAngleHigh = 270.0) then
                                    begin
                                        if cBuffer^[i].re < -1 * DOC_TripSetHigh then
                                        begin // Left-side of High Straight-Line
                                            if Delay_Time > 0.0 then
                                                TimeTest := Delay_Time
                                            else
                                            if PhaseCurve <> NIL then
                                                TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag / PhaseTrip)
                                            else
                                            if Delay_Time = 0.0 then
                                                TimeTest := Delay_Time;
                                        end
                                        else
                                        begin  // Right-Side of High Straight-Line
                                            if DOC_DelayInner > 0.0 then
                                                TimeTest := DOC_DelayInner
                                            else
                                            if DOC_PhaseCurveInner <> NIL then
                                                TimeTest := DOC_TDPhaseInner * DOC_PhaseCurveInner.GetTCCTime(Cmag / DOC_PhaseTripInner)
                                            else
                                            if DOC_DelayInner = 0.0 then
                                                TimeTest := Delay_Time;
                                        end;
                                    end
                                    else
                                    begin
                                        if cBuffer^[i].im < Tan(DegToRad(DOC_TiltAngleHigh)) * (cBuffer^[i].re + DOC_TripSetHigh) then
                                        begin // Left-side of High Straight-Line
                                            if Delay_Time > 0.0 then
                                                TimeTest := Delay_Time
                                            else
                                            if PhaseCurve <> NIL then
                                                TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag / PhaseTrip)
                                            else
                                            if Delay_Time = 0.0 then
                                                TimeTest := Delay_Time;
                                        end
                                        else
                                        begin // Right-Side of High Straight-Line
                                            if DOC_DelayInner > 0.0 then
                                                TimeTest := DOC_DelayInner
                                            else
                                            if DOC_PhaseCurveInner <> NIL then
                                                TimeTest := DOC_TDPhaseInner * DOC_PhaseCurveInner.GetTCCTime(Cmag / DOC_PhaseTripInner)
                                            else
                                            if DOC_DelayInner = 0.0 then
                                                TimeTest := Delay_Time;
                                        end;
                                    end;
                                end
                                else
                                begin // High Straight-Line Not Specified.
                                    if DOC_DelayInner > 0.0 then
                                        TimeTest := DOC_DelayInner
                                    else
                                    if DOC_PhaseCurveInner <> NIL then
                                        TimeTest := DOC_TDPhaseInner * DOC_PhaseCurveInner.GetTCCTime(Cmag / DOC_PhaseTripInner)
                                    else
                                    if DOC_DelayInner = 0.0 then
                                        TimeTest := Delay_Time;
                                end;
                            end
                            else
                            begin // Out of the Circle
                                if Delay_Time > 0.0 then
                                    TimeTest := Delay_Time
                                else
                                if PhaseCurve <> NIL then
                                    TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag / PhaseTrip)
                                else
                                if Delay_Time = 0.0 then
                                    TimeTest := Delay_Time;
                            end;
                        end
                        else
                        begin // Circle not Specified
                            if DOC_TripSetHigh > 0.0 then
                            begin // High Straight-Line Specified.
                                if (DOC_TiltAngleHigh = 90.0) or (DOC_TiltAngleHigh = 270.0) then
                                begin
                                    if cBuffer^[i].re < -1 * DOC_TripSetHigh then
                                    begin // Left-side of High Straight-Line
                                        if Delay_Time > 0.0 then
                                            TimeTest := Delay_Time
                                        else
                                        if PhaseCurve <> NIL then
                                            TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag / PhaseTrip)
                                        else
                                        if Delay_Time = 0.0 then
                                            TimeTest := Delay_Time;
                                    end
                                    else
                                    begin  // Right-Side of High Straight-Line
                                        if DOC_DelayInner > 0.0 then
                                            TimeTest := DOC_DelayInner
                                        else
                                        if DOC_PhaseCurveInner <> NIL then
                                            TimeTest := DOC_TDPhaseInner * DOC_PhaseCurveInner.GetTCCTime(Cmag / DOC_PhaseTripInner)
                                        else
                                        if DOC_DelayInner = 0.0 then
                                            TimeTest := Delay_Time;
                                    end;
                                end
                                else
                                begin
                                    if cBuffer^[i].im < Tan(DegToRad(DOC_TiltAngleHigh)) * (cBuffer^[i].re + DOC_TripSetHigh) then
                                    begin // Left-side of High Straight-Line
                                        if Delay_Time > 0.0 then
                                            TimeTest := Delay_Time
                                        else
                                        if PhaseCurve <> NIL then
                                            TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag / PhaseTrip)
                                        else
                                        if Delay_Time = 0.0 then
                                            TimeTest := Delay_Time;
                                    end
                                    else
                                    begin // Right-Side of High Straight-Line
                                        if DOC_DelayInner > 0.0 then
                                            TimeTest := DOC_DelayInner
                                        else
                                        if DOC_PhaseCurveInner <> NIL then
                                            TimeTest := DOC_TDPhaseInner * DOC_PhaseCurveInner.GetTCCTime(Cmag / DOC_PhaseTripInner)
                                        else
                                        if DOC_DelayInner = 0.0 then
                                            TimeTest := Delay_Time;
                                    end;
                                end;
                            end
                            else
                            begin  // High Straight-Line Not Specified.
                                if Delay_Time > 0.0 then
                                    TimeTest := Delay_Time
                                else
                                if PhaseCurve <> NIL then
                                    TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag / PhaseTrip)
                                else
                                if Delay_Time = 0.0 then
                                    TimeTest := Delay_Time;
                            end;
                        end;
                    end;
                end
                else
                begin // 90, 270
                    if cBuffer^[i].im < Tan(DegToRad(DOC_TiltAngleLow)) * (cBuffer^[i].re + DOC_TripSetLow) then
                    begin
                        if DOC_TripSetMag > 0.0 then
                        begin // Circle Specified.
                            if Cmag <= DOC_TripSetMag then
                            begin // Within the Circle
                                if DOC_TripSetHigh > 0.0 then // High Straight-Line Specified.
                                begin
                                    if (DOC_TiltAngleHigh = 90.0) or (DOC_TiltAngleHigh = 270.0) then
                                    begin
                                        if cBuffer^[i].re < -1 * DOC_TripSetHigh then
                                        begin // Left-side of High Straight-Line
                                            if Delay_Time > 0.0 then
                                                TimeTest := Delay_Time
                                            else
                                            if PhaseCurve <> NIL then
                                                TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag / PhaseTrip)
                                            else
                                            if Delay_Time = 0.0 then
                                                TimeTest := Delay_Time;
                                        end
                                        else
                                        begin  // Right-Side of High Straight-Line
                                            if DOC_DelayInner > 0.0 then
                                                TimeTest := DOC_DelayInner
                                            else
                                            if DOC_PhaseCurveInner <> NIL then
                                                TimeTest := DOC_TDPhaseInner * DOC_PhaseCurveInner.GetTCCTime(Cmag / DOC_PhaseTripInner)
                                            else
                                            if DOC_DelayInner = 0.0 then
                                                TimeTest := Delay_Time;
                                        end;
                                    end
                                    else
                                    begin
                                        if cBuffer^[i].im < Tan(DegToRad(DOC_TiltAngleHigh)) * (cBuffer^[i].re + DOC_TripSetHigh) then
                                        begin // Left-side of High Straight-Line
                                            if Delay_Time > 0.0 then
                                                TimeTest := Delay_Time
                                            else
                                            if PhaseCurve <> NIL then
                                                TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag / PhaseTrip)
                                            else
                                            if Delay_Time = 0.0 then
                                                TimeTest := Delay_Time;
                                        end
                                        else
                                        begin // Right-Side of High Straight-Line
                                            if DOC_DelayInner > 0.0 then
                                                TimeTest := DOC_DelayInner
                                            else
                                            if DOC_PhaseCurveInner <> NIL then
                                                TimeTest := DOC_TDPhaseInner * DOC_PhaseCurveInner.GetTCCTime(Cmag / DOC_PhaseTripInner)
                                            else
                                            if DOC_DelayInner = 0.0 then
                                                TimeTest := Delay_Time;
                                        end;
                                    end;
                                end
                                else
                                begin // High Straight-Line Not Specified.

                                    if DOC_DelayInner > 0.0 then
                                        TimeTest := DOC_DelayInner
                                    else
                                    if DOC_PhaseCurveInner <> NIL then
                                        TimeTest := DOC_TDPhaseInner * DOC_PhaseCurveInner.GetTCCTime(Cmag / DOC_PhaseTripInner)
                                    else
                                    if DOC_DelayInner = 0.0 then
                                        TimeTest := Delay_Time;
                                end;
                            end
                            else
                            begin // Out of the Circle
                                if Delay_Time > 0.0 then
                                    TimeTest := Delay_Time
                                else
                                if PhaseCurve <> NIL then
                                    TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag / PhaseTrip)
                                else
                                if Delay_Time = 0.0 then
                                    TimeTest := Delay_Time;
                            end;
                        end
                        else
                        begin // Circle not Specified
                            if DOC_TripSetHigh > 0.0 then
                            begin // High Straight-Line Specified.
                                if (DOC_TiltAngleHigh = 90.0) or (DOC_TiltAngleHigh = 270.0) then
                                begin
                                    if cBuffer^[i].re < -1 * DOC_TripSetHigh then
                                    begin // Left-side of High Straight-Line

                                        if Delay_Time > 0.0 then
                                            TimeTest := Delay_Time
                                        else
                                        if PhaseCurve <> NIL then
                                            TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag / PhaseTrip)
                                        else
                                        if Delay_Time = 0.0 then
                                            TimeTest := Delay_Time;
                                    end
                                    else
                                    begin  // Right-Side of High Straight-Line
                                        if DOC_DelayInner > 0.0 then
                                            TimeTest := DOC_DelayInner
                                        else
                                        if DOC_PhaseCurveInner <> NIL then
                                            TimeTest := DOC_TDPhaseInner * DOC_PhaseCurveInner.GetTCCTime(Cmag / DOC_PhaseTripInner)
                                        else
                                        if DOC_DelayInner = 0.0 then
                                            TimeTest := Delay_Time;
                                    end;
                                end
                                else
                                begin
                                    if cBuffer^[i].im < Tan(DegToRad(DOC_TiltAngleHigh)) * (cBuffer^[i].re + DOC_TripSetHigh) then
                                    begin // Left-side of High Straight-Line
                                        if Delay_Time > 0.0 then
                                            TimeTest := Delay_Time
                                        else
                                        if PhaseCurve <> NIL then
                                            TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag / PhaseTrip)
                                        else
                                        if Delay_Time = 0.0 then
                                            TimeTest := Delay_Time;
                                    end
                                    else
                                    begin // Right-Side of High Straight-Line
                                        if DOC_DelayInner > 0.0 then
                                            TimeTest := DOC_DelayInner
                                        else
                                        if DOC_PhaseCurveInner <> NIL then
                                            TimeTest := DOC_TDPhaseInner * DOC_PhaseCurveInner.GetTCCTime(Cmag / DOC_PhaseTripInner)
                                        else
                                        if DOC_DelayInner = 0.0 then
                                            TimeTest := Delay_Time;
                                    end;
                                end;
                            end
                            else
                            begin  // High Straight-Line Not Specified.
                                if Delay_Time > 0.0 then
                                    TimeTest := Delay_Time
                                else
                                if PhaseCurve <> NIL then
                                    TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag / PhaseTrip)
                                else
                                if Delay_Time = 0.0 then
                                    TimeTest := Delay_Time;
                            end;
                        end;
                    end
                    else
                    begin
                        // There might be an intersection between Straight Line Low and High depending on their angles.
                        // Straight Line High takes precedence.
                        if DOC_TripSetHigh > 0.0 then
                        begin
                            if (DOC_TiltAngleHigh = 90.0) or (DOC_TiltAngleHigh = 270.0) then
                            begin
                                if cBuffer^[i].re < -1 * DOC_TripSetHigh then
                                begin // Left-side of High Straight-Line
                                    if Delay_Time > 0.0 then
                                        TimeTest := Delay_Time
                                    else
                                    if PhaseCurve <> NIL then
                                        TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag / PhaseTrip)
                                    else
                                    if Delay_Time = 0.0 then
                                        TimeTest := Delay_Time;
                                end
                            end
                            else
                            begin
                                if cBuffer^[i].im < Tan(DegToRad(DOC_TiltAngleHigh)) * (cBuffer^[i].re + DOC_TripSetHigh) then
                                begin // Left-side of High Straight-Line
                                    if Delay_Time > 0.0 then
                                        TimeTest := Delay_Time
                                    else
                                    if PhaseCurve <> NIL then
                                        TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag / PhaseTrip)
                                    else
                                    if Delay_Time = 0.0 then
                                        TimeTest := Delay_Time;
                                end
                            end;
                        end;
                    end;
                end;

                if (TimeTest >= 0.0) then
                begin
                    if DebugTrace then
                        AppendToEventLog('Relay.' + Self.Name, Format('Directional Overcurrent - Phase %d Trip: Mag=%.5g, Ang=%.5g, Time=%.5g', [i - CondOffset, Cmag, Cangle, TimeTest]));
                    if TripTime < 0.0 then
                        TripTime := TimeTest
                    else
                        TripTime := Min(TripTime, TimeTest);
                end;
            end;

            if TripTime >= 0.0 then
            begin
                if not ArmedForOpen then
                    with ActiveCircuit do   // Then arm for an open operation
                    begin
                        RelayTarget := 'DOC';
                        LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + Breaker_time, CTRL_OPEN, 0, Self);
                        if OperationCount <= NumReclose then
                            LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + Breaker_time + RecloseIntervals^[OperationCount], CTRL_CLOSE, 0, Self);
                        ArmedForOpen := TRUE;
                        ArmedForClose := TRUE;
                    end;
            end
            else
            begin
                if ArmedForOpen then
                    with ActiveCircuit do    // If current dropped below pickup, disarm trip and set for reset
                    begin
                        LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self);
                        ArmedForOpen := FALSE;
                        ArmedForClose := FALSE;
                    end;
            end;
        end; // IF PresentState=CLOSE
    end;  // with MonitoredElement
end;

procedure TRelayObj.RevPowerLogic;
var
    S: Complex;
begin
    with MonitoredElement do
    begin
        // MonitoredElement.ActiveTerminalIdx := MonitoredElementTerminal;
        S := MonitoredElement.Power[MonitoredElementTerminal];
        if S.re < 0.0 then
        begin
            if Abs(S.Re) > PhaseInst * 1000.0 then
            begin
                if not ArmedForOpen then  // push the trip operation and arm to trip
                    with ActiveCircuit do
                    begin
                        RelayTarget := 'Rev P';
                        LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + Delay_Time + Breaker_time, CTRL_OPEN, 0, Self);
                        OperationCount := NumReclose + 1;  // force a lockout
                        ArmedForOpen := TRUE;
                    end
            end
            else
            if ArmedForOpen then    // We became unarmed, so reset and disarm
                with ActiveCircuit do
                begin
                    LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self);
                    ArmedForOpen := FALSE;
                end;
        end;
    end;
end;

procedure TRelayObj.VoltageLogic;
var
    i: Integer;
    VMax,
    Vmin,
    Vmag,
    OVTime,
    UVTime,
    TripTime: Double;
begin
    if LockedOut then
        Exit;

    with MonitoredElement do
    begin
        //**** Fix so that fastest trip time applies ****
        MonitoredElement.GetTermVoltages(MonitoredElementTerminal, cBuffer);

        Vmin := 1.0E50;
        Vmax := 0.0;
        for i := 1 to MonitoredElement.NPhases do
        begin
            Vmag := Cabs(cBuffer^[i]);
            if Vmag > Vmax then
                Vmax := Vmag;
            if Vmag < Vmin then
                Vmin := Vmag;
        end;

        // Convert to Per Unit
        Vmax := Vmax / Vbase;
        Vmin := Vmin / Vbase;

        if FPresentState = CTRL_CLOSE then
        begin
            TripTime := -1.0;
            OVTime := -1.0;
            UVTime := -1.0;


            // Check OverVoltage Trip, if any
            if OVCurve <> NIL then
                OVTime := OVCurve.GetOVtime(Vmax);

            if OVTime > 0.0 then
            begin
                TripTime := OVTime;
            end;

            // If OVTime > 0 then we have a OV trip

            // Check UV Trip, if any
            if UVCurve <> NIL then
            begin
                UVTime := UVCurve.GetUVtime(Vmin);
            end;

            // If UVTime > 0 then we have a UV trip

            if UVTime > 0.0 then
            begin
                if TripTime > 0.0 then
                begin
                    TripTime := Min(TripTime, UVTime)   // Min of UV or OV time
                end
                else
                begin
                    TripTime := UVTime;
                end;
            end;

            if TripTime > 0.0 then
                with ActiveCircuit do
                begin
                    if ArmedForOpen and ((Solution.DynaVars.t + TripTime + Breaker_time) < NextTripTime) then
                    begin
                        ControlQueue.Delete(LastEventHandle);  // Delete last event from Queue
                        ArmedForOpen := FALSE;  // force it to go through next IF
                    end;

                    if not ArmedForOpen then
                    begin  // Then arm for an open operation
                        if TripTime = UVTime then
                        begin
                            if TripTime = OVTime then
                                RelayTarget := 'UV + OV'
                            else
                                RelayTarget := 'UV';
                        end
                        else
                            Relaytarget := 'OV';

                        NextTripTime := Solution.DynaVars.t + TripTime + Breaker_time;
                        LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, NextTripTime, CTRL_OPEN, 0, Self);
                        ArmedforOpen := TRUE;
                    end;
                end
            else
            begin
                if ArmedForOpen then
                    with ActiveCircuit do // If voltage dropped below pickup, disarm trip and set for reset
                    begin
                        ControlQueue.Delete(LastEventHandle);  // Delete last event from Queue
                        NextTripTime := -1.0;
                        LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self);
                        ArmedForOpen := FALSE;
                    end;
            end;
        end  // IF PresentState=CLOSE
        else
        begin     
            // Present state is Open, Check for Voltage and then set reclose Interval
            if (OperationCount <= NumReclose) then
                if not ArmedForClose then
                begin
                    if (Vmax > 0.9) then
                        with ActiveCircuit do  // OK if voltage > 90%
                        begin
                            LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + RecloseIntervals^[OperationCount], CTRL_CLOSE, 0, Self);
                            ArmedForClose := TRUE;
                        end;
                end
                else // Armed, but check to see if voltage dropped before it reclosed and cancel action
                if Vmax < 0.9 then
                    ArmedForClose := FALSE;

        end;
    end;  // With MonitoredElement
end;

procedure TRelayObj.NegSeq47Logic;
// Neg Seq voltage Relay
var
    NegSeqVoltageMag: Double;
    V012: array[1..3] of Complex;
begin
    with MonitoredElement do
    begin
        MonitoredElement.GetTermVoltages(MonitoredElementTerminal, cBuffer);
        Phase2SymComp(cBuffer, pComplexArray(@V012)); // Phase to symmetrical components
        NegSeqVoltageMag := Cabs(V012[3]);
        if NegSeqVoltageMag >= PickupVolts47 then
        begin
            if not ArmedForOpen then  // push the trip operation and arm to trip
                with ActiveCircuit do
                begin
                    RelayTarget := '-Seq V';
                    LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + Delay_Time + Breaker_time, CTRL_OPEN, 0, Self);
                    OperationCount := NumReclose + 1;  // force a lockout
                    ArmedForOpen := TRUE;
                end
        end
        else
        begin  // Less Than pickup value: reset if armed
            if ArmedForOpen then    // We became unarmed, so reset and disarm
                with ActiveCircuit do
                begin
                    LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self);
                    ArmedForOpen := FALSE;
                end;
        end;
    end;
end;

finalization    ActionEnum.Free;
    StateEnum.Free;
    RelayTypeEnum.Free;
end.
