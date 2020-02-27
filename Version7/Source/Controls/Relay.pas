unit Relay;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
    Created 8-24-00 from CktElement Control
    9-20-00 Implemented Voltage relay and updated arming logic for all relays
    10-31-00 Added Event Logging
    11-1-00 Added shots=
    3-7-03  Added new property definition process
            Added Neg seq relays and Generic relay
            Added capability to monitor PC Element variable
    2-16-04 Fixed address bug in symmetrical component transformation in 46 relay
    5-1-06 Added Time Dial to Phase and ground
}
{
  A Relay is a control element that is connected to a terminal of a
  circuit element and controls the switches in the same or another terminal.

  The control is usually placed in the
  terminal of a line or transformer, but it could be any element

  A Relay is defined by a New command:

  New Relay.Name=myname Element=devclass.name terminal=[ 1|2|...] Switch = devclass.name   terminal=[ 1|2|...]
  Type = [current | voltage]
  Phase = TCCCurve
  Ground = TCCCurve
  OverVolt = TCCcurve
  UnderVolt = TCCCurve
  PhaseTrip =  Multipliers times curve
  GroundTrip =
  PhaseInst  =
  GroundInst =
  RecloseIntervals= (array of times, sec);
  ResetTime =

  CktElement to be controlled must already exist.

  Voltage relay is a definite time relay that operates after the voltage stays out of bounds
  for a fixed time interval.  It will then reclose a set time after the voltage comes back in the normal range.

}

interface

uses
    Command,
    ControlClass,
    ControlElem,
    CktElement,
    DSSClass,
    Arraydef,
    ucomplex,
    utilities,
    TCC_Curve,
    Math;

type

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TRelay = class(TControlClass)
    PRIVATE
        TCC_CurveClass: TDSSClass;
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const RelayName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;
        function GetTccCurve(const CurveName: String): TTCC_CurveObj;
    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TRelayObj = class(TControlElem)
    PRIVATE
        ControlType: Integer;


            {OverCurrent Relay}
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


            {over/Under Voltage Relay}
        OVcurve,                 // Curves assumed in per unit of base voltage
        UVCurve: TTCC_CurveObj;

        Vbase,   // line-neut volts base
        kVBase: Double;

            {46 Relay  Neg Seq Current}
        PickupAmps46,
        PctPickup46,
        BaseAmps46,
        Isqt46: Double;

            {47 Relay}
        PickupVolts47,
        PctPickup47: Double;

            {Generic Relay}
        OverTrip,
        UnderTrip: Double;

        PresentState: EControlAction;

        OperationCount: Integer;

        LockedOut,
        ArmedForClose,
        ArmedForOpen,
        PhaseTarget, GroundTarget: Boolean;

        NextTriptime: Double;
        LastEventHandle: Integer;

        CondOffset: Integer; // Offset for monitored terminal

        cBuffer: pComplexArray;    // Complexarray buffer

        procedure InterpretRelayAction(const Action: String);
        procedure InterpretRelayType(const S: String);

        procedure OvercurrentLogic;
        procedure VoltageLogic;
        procedure RevPowerLogic;
        procedure NegSeq46Logic;
        procedure NegSeq47Logic;
        procedure GenericLogic;

    PUBLIC

        MonitoredElementName: String;
        MonitoredElementTerminal: Integer;

        constructor Create(ParClass: TDSSClass; const RelayName: String);
        destructor Destroy; OVERRIDE;

        procedure MakePosSequence; OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;    // Always Zero for a Relay

        procedure Sample; OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state


        procedure GetCurrents(Curr: pComplexArray); OVERRIDE; // Get present value of terminal Curr
        procedure GetInjCurrents(Curr: pComplexArray); OVERRIDE;   // Returns Injextion currents

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

    end;


var
    ActiveRelayObj: TRelayObj;
    RelayClass: TRelay;

{--------------------------------------------------------------------------}
implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    PCElement,
    Sysutils,
    uCmatrix,
    MathUtil;

const

    NumPropsThisClass = 29;

    CURRENT = 0;  {Default}
    VOLTAGE = 1;
    REVPOWER = 3;
    NEGCURRENT = 4;
    NEGVOLTAGE = 5;
    GENERIC = 6; {Use this for frequency, etc.  Generic over/under relay}

{--------------------------------------------------------------------------}
constructor TRelay.Create;  // Creates superstructure for all Relay objects
begin
    inherited Create;

    Class_name := 'Relay';
    DSSClassType := DSSClassType + RELAY_CONTROL;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;

    TCC_CurveClass := GetDSSClassPtr('TCC_Curve');
    RelayClass := Self;
end;

{--------------------------------------------------------------------------}
destructor TRelay.Destroy;

begin
    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TRelay.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count

    AllocatePropertyArrays;   {see DSSClass}


     // Define Property names
     // Addproperty (property name,  internal property index (see Edit), Help string);

    AddProperty('MonitoredObj', 1,
        'Full object name of the circuit element, typically a line, transformer, load, or generator, ' +
        'to which the relay''s PT and/or CT are connected.' +
        ' This is the "monitored" element. ' +
        'There is no default; must be specified.');
    AddProperty('MonitoredTerm', 2,
        'Number of the terminal of the circuit element to which the Relay is connected. ' +
        '1 or 2, typically.  Default is 1.');
    AddProperty('SwitchedObj', 3,
        'Name of circuit element switch that the Relay controls. ' +
        'Specify the full object name.' +
        'Defaults to the same as the Monitored element. ' +
        'This is the "controlled" element.');
    AddProperty('SwitchedTerm', 4,
        'Number of the terminal of the controlled element in which the switch is controlled by the Relay. ' +
        '1 or 2, typically.  Default is 1.');
    AddProperty('type', 5, 'One of a legal relay type:' + CRLF +
        'Current' + CRLF + 'Voltage' + CRLF + 'Reversepower' + CRLF + '46 (neg seq current)' + CRLF +
        '47 (neg seq voltage)' + CRLF +
        'Generic (generic over/under relay)' + CRLF + CRLF +
        'Default is overcurrent relay (Current) ' +
        'Specify the curve and pickup settings appropriate for each type. ' +
        'Generic relays monitor PC Element Control variables and trip on out of over/under range in definite time.');
    AddProperty('Phasecurve', 6, 'Name of the TCC Curve object that determines the phase trip.  ' +
        'Must have been previously defined as a TCC_Curve object.' +
        ' Default is none (ignored). ' +
        'For overcurrent relay, multiplying the current values in the curve by the "phasetrip" value gives the actual current.');
    AddProperty('Groundcurve', 7, 'Name of the TCC Curve object that determines the ground trip.  Must have been previously defined as a TCC_Curve object.' +
        ' Default is none (ignored).' +
        'For overcurrent relay, multiplying the current values in the curve by the "groundtrip" valuw gives the actual current.');
    AddProperty('PhaseTrip', 8, 'Multiplier or actual phase amps for the phase TCC curve.  Defaults to 1.0.');
    AddProperty('GroundTrip', 9, 'Multiplier or actual ground amps (3I0) for the ground TCC curve.  Defaults to 1.0.');
    AddProperty('TDPhase', 28, 'Time dial for Phase trip curve. Multiplier on time axis of specified curve. Default=1.0.');
    AddProperty('TDGround', 29, 'Time dial for Ground trip curve. Multiplier on time axis of specified curve. Default=1.0.');
    AddProperty('PhaseInst', 10, 'Actual  amps (Current relay) or kW (reverse power relay) for instantaneous phase trip which is assumed to happen in 0.01 sec + Delay Time. Default is 0.0, which signifies no inst trip. ' +
        'Use this value for specifying the Reverse Power threshold (kW) for reverse power relays.');
    AddProperty('GroundInst', 11, 'Actual  amps for instantaneous ground trip which is assumed to happen in 0.01 sec + Delay Time.Default is 0.0, which signifies no inst trip.');
    AddProperty('Reset', 12, 'Reset time in sec for relay.  Default is 15. If ');
    AddProperty('Shots', 13, 'Number of shots to lockout.  Default is 4. This is one more than the number of reclose intervals.');
    AddProperty('RecloseIntervals', 14, 'Array of reclose intervals. If none, specify "NONE". Default for overcurrent relay is (0.5, 2.0, 2.0) seconds. ' +
        'Default for a voltage relay is (5.0). In a voltage relay, this is  seconds after restoration of ' +
        'voltage that the reclose occurs. ' +
        'Reverse power relay is one shot to lockout, ' +
        'so this is ignored.  A locked out relay must be closed manually (set action=close).');
    AddProperty('Delay', 24, 'Trip time delay (sec) for DEFINITE TIME relays. Default is 0.0 for current and voltage relays.  If >0 then this value is used instead of curves. ' +
        ' Used by Generic, RevPower, 46 and 47 relays. Defaults to 0.1 s for these relays.');
    AddProperty('Overvoltcurve', 15, 'TCC Curve object to use for overvoltage relay.  Curve is assumed to be defined with per unit voltage values. ' +
        'Voltage base should be defined for the relay. Default is none (ignored).');
    AddProperty('Undervoltcurve', 16, 'TCC Curve object to use for undervoltage relay.  Curve is assumed to be defined with per unit voltage values. ' +
        'Voltage base should be defined for the relay. Default is none (ignored).');
    AddProperty('kvbase', 17, 'Voltage base (kV) for the relay. Specify line-line for 3 phase devices); line-neutral for 1-phase devices.  Relay assumes ' +
        'the number of phases of the monitored element.  Default is 0.0, which results in assuming the voltage ' +
        'values in the "TCC" curve are specified in actual line-to-neutral volts.');
    AddProperty('47%Pickup', 25, 'Percent voltage pickup for 47 relay (Neg seq voltage). Default is 2. Specify also base voltage (kvbase) and delay time value.   ');
    AddProperty('46BaseAmps', 23, 'Base current, Amps, for 46 relay (neg seq current).' +
        '  Used for establishing pickup and per unit I-squared-t.');
    AddProperty('46%Pickup', 21, 'Percent pickup current for 46 relay (neg seq current).  Default is 20.0. ' +
        '  When current exceeds this value * BaseAmps, I-squared-t calc starts.');
    AddProperty('46isqt', 22, 'Negative Sequence I-squared-t trip value for 46 relay (neg seq current).' +
        '  Default is 1 (trips in 1 sec for 1 per unit neg seq current).  Should be 1 to 99.');
    AddProperty('Variable', 20, 'Name of variable in PC Elements being monitored.  Only applies to Generic relay.');
    AddProperty('overtrip', 26, 'Trip setting (high value) for Generic relay variable.  Relay trips in definite time if value of variable exceeds this value.');
    AddProperty('undertrip', 27, 'Trip setting (low value) for Generic relay variable.  Relay trips in definite time if value of variable is less than this value.');
    AddProperty('Breakertime', 18, 'Fixed delay time (sec) added to relay time. Default is 0.0. Designed to represent breaker time or some other delay after a trip decision is made.' +
        'Use Delay property for setting a fixed trip time delay.' +
        'Added to trip time of current and voltage relays. Could use in combination with inst trip value to obtain a definite time overcurrent relay.');
    AddProperty('action', 19, '{Trip/Open | Close}  Action that overrides the relay control. Simulates manual control on breaker. ' +
        '"Trip" or "Open" causes the controlled element to open and lock out. ' +
        '"Close" causes the controlled element to close and the relay to reset to its first operation.');

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

{--------------------------------------------------------------------------}
function TRelay.NewObject(const ObjName: String): Integer;
begin
    // Make a new Relay and add it to Relay class list
    with ActiveCircuit do
    begin
        ActiveCktElement := TRelayObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;

{--------------------------------------------------------------------------}

function TRelay.GetTccCurve(const CurveName: String): TTCC_CurveObj;

begin

    Result := TCC_CurveClass.Find(CurveName);

    if Result = NIL then
        DoSimpleMsg('TCC Curve object: "' + CurveName + '" not found.', 380);

end;

{--------------------------------------------------------------------------}
function TRelay.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

begin

  // continue parsing WITH contents of Parser
    ActiveRelayObj := ElementList.Active;
    ActiveCircuit.ActiveCktElement := ActiveRelayObj;

    Result := 0;

    with ActiveRelayObj do
    begin

        ParamPointer := 0;
        ParamName := Parser.NextParam;
        Param := Parser.StrValue;
        while Length(Param) > 0 do
        begin
            if Length(ParamName) = 0 then
                Inc(ParamPointer)
            else
                ParamPointer := CommandList.GetCommand(ParamName);

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[PropertyIdxMap[ParamPointer]] := Param
            else
                DoSimpleMsg('Unknown parameter "' + ParamName + '" for Relay "' + Name + '"', 381);

            if ParamPointer > 0 then
                case PropertyIdxMap[ParamPointer] of
           {internal Relay Property commands}
                    0:
                        DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 382);
                    1:
                        MonitoredElementName := lowercase(param);
                    2:
                        MonitoredElementTerminal := Parser.IntValue;
                    3:
                        ElementName := lowercase(param);
                    4:
                        ElementTerminal := Parser.IntValue;
                    5:
                        InterpretRelayType(Param);
                    6:
                        PhaseCurve := GetTccCurve(Param);
                    7:
                        GroundCurve := GetTCCCurve(Param);
                    8:
                        PhaseTrip := Parser.Dblvalue;
                    9:
                        GroundTrip := Parser.Dblvalue;
                    10:
                        PhaseInst := Parser.Dblvalue;
                    11:
                        GroundInst := Parser.Dblvalue;
                    12:
                        ResetTime := Parser.Dblvalue;
                    13:
                        NumReclose := Parser.Intvalue - 1;   // one less than number of shots
                    14:
                        if Comparetext(Param, 'NONE') = 0 then
                            NumReclose := 1
                        else
                            NumReclose := Parser.ParseAsVector(4, RecloseIntervals);   // max of 4 allowed
                    15:
                        OVCurve := GetTCCCurve(Param);
                    16:
                        UVCurve := GetTCCCurve(Param);
                    17:
                        kVBase := Parser.DblValue;
                    18:
                        Breaker_time := Parser.DblValue;
                    19:
                        InterpretRelayAction(Param);
                    20:
                        MonitorVariable := lowercase(param);  // for pc elements
                    21:
                        PctPickup46 := Parser.DblValue;
                    22:
                        Isqt46 := Parser.DblValue;
                    23:
                        BaseAmps46 := Parser.DblValue;
                    24:
                        Delay_Time := Parser.DblValue;
                    25:
                        PctPickup47 := Parser.DblValue;
                    26:
                        Overtrip := Parser.DblValue;
                    27:
                        Undertrip := Parser.DblValue;
                    28:
                        TDPhase := Parser.DblValue;
                    29:
                        TDGround := Parser.DblValue;
                else
           // Inherited parameters
                    ClassEdit(ActiveRelayObj, ParamPointer - NumPropsthisClass)
                end;

            if ParamPointer > 0 then
                case PropertyIdxMap[ParamPointer] of
              {Default the controlled element to the monitored element}
                    1:
                        ElementName := MonitoredElementName;
                    2:
                        ElementTerminal := MonitoredElementTerminal;
                    5:
                    begin        {Set Default Reclose Intervals}
                        case lowercase(param)[1] of
                            'c':
                                PropertyValue[14] := '(0.5, 2.0, 2.0)';
                            'v':
                                PropertyValue[14] := '(5.0)';
                        end;
                        AuxParser.CmdString := PropertyValue[14];
                        ParamName := AuxParser.NextParam;
                        NumReclose := AuxParser.ParseAsVector(4, RecloseIntervals);
                    end;
                end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

        RecalcElementData;
    end;

end;


{--------------------------------------------------------------------------}
function TRelay.MakeLike(const RelayName: String): Integer;
var
    OtherRelay: TRelayObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this Relay name in the present collection}
    OtherRelay := Find(RelayName);
    if OtherRelay <> NIL then
        with ActiveRelayObj do
        begin

            NPhases := OtherRelay.Fnphases;
            NConds := OtherRelay.Fnconds; // Force Reallocation of terminal stuff

            ElementName := OtherRelay.ElementName;
            ElementTerminal := OtherRelay.ElementTerminal;
            ControlledElement := OtherRelay.ControlledElement;  // Pointer to target circuit element

            MonitoredElement := OtherRelay.MonitoredElement;  // Pointer to target circuit element
            MonitoredElementName := OtherRelay.MonitoredElementName;  // Pointer to target circuit element
            MonitoredElementTerminal := OtherRelay.MonitoredElementTerminal;  // Pointer to target circuit element

            PhaseCurve := OtherRelay.PhaseCurve;
            GroundCurve := OtherRelay.GroundCurve;
            OVCurve := OtherRelay.OVCurve;
            UVcurve := OtherRelay.UVcurve;
            PhaseTrip := OtherRelay.PhaseTrip;
            GroundTrip := OtherRelay.GroundTrip;
            TDPhase := OtherRelay.TDPhase;
            TDGround := OtherRelay.TDGround;
            PhaseInst := OtherRelay.PhaseInst;
            GroundInst := OtherRelay.GroundInst;
            ResetTime := OtherRelay.Resettime;
            NumReclose := OtherRelay.NumReclose;
            Delay_Time := OtherRelay.Delay_Time;
            Breaker_time := OtherRelay.Breaker_time;

            Reallocmem(RecloseIntervals, SizeOf(RecloseIntervals^[1]) * 4);      // Always make a max of 4
            for i := 1 to NumReclose do
                RecloseIntervals^[i] := OtherRelay.RecloseIntervals^[i];

            kVBase := OtherRelay.kVBase;
            LockedOut := OtherRelay.LockedOut;

            ControlType := OtherRelay.ControlType;
            PresentState := OtherRelay.PresentState;
            CondOffset := OtherRelay.CondOffset;

        {46 Relay  Neg Seq Current}
            PickupAmps46 := OtherRelay.PickupAmps46;
            PctPickup46 := OtherRelay.PctPickup46;
            BaseAmps46 := OtherRelay.BaseAmps46;
            Isqt46 := OtherRelay.Isqt46;

        {47 Relay}
            PickupVolts47 := OtherRelay.PickupVolts47;
            PctPickup47 := OtherRelay.PctPickup47;

        {Generic Relay}
            MonitorVariable := OtherRelay.MonitorVariable;
            OverTrip := OtherRelay.OverTrip;
            UnderTrip := OtherRelay.UnderTrip;

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherRelay.PropertyValue[i];

        end
    else
        DoSimpleMsg('Error in Relay MakeLike: "' + RelayName + '" Not Found.', 383);

end;


{==========================================================================}
{                    TRelayObj                                           }
{==========================================================================}


{--------------------------------------------------------------------------}
constructor TRelayObj.Create(ParClass: TDSSClass; const RelayName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(RelayName);
    DSSObjType := ParClass.DSSClassType;

    NPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors
                         // in base class


    ElementName := '';
    ControlledElement := NIL;
    ElementTerminal := 1;

    MonitoredElementName := '';
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


    PresentState := CTRL_CLOSE;


    Isqt46 := 1.0;
    BaseAmps46 := 100.0;
    PctPickup46 := 20.0;
    PickupAmps46 := BaseAmps46 * PctPickup46 * 0.01;

    PctPickup47 := 2.0;

    overtrip := 1.2;
    undertrip := 0.8;

    Operationcount := 1;
    LockedOut := FALSE;
    ArmedForOpen := FALSE;
    ArmedForClose := FALSE;
    PhaseTarget := FALSE;
    GroundTarget := FALSE;

    NextTripTime := -1.0;  // not set to trip

    cBuffer := NIL; // Complex buffer

    DSSObjType := ParClass.DSSClassType; //cap_CONTROL;

    InitPropertyValues(0);


   //  RecalcElementData;

end;

destructor TRelayObj.Destroy;
begin
    MonitoredElementName := '';
    ReallocMem(RecloseIntervals, 0);
    if Assigned(cBuffer) then
        ReallocMem(cBuffer, 0);
    inherited Destroy;
end;

{--------------------------------------------------------------------------}
procedure TRelayObj.RecalcElementData;

var
    DevIndex: Integer;

begin

    Devindex := GetCktElementIndex(MonitoredElementName); // Global function
    if DevIndex > 0 then
    begin
        MonitoredElement := ActiveCircuit.CktElements.Get(DevIndex);
        Nphases := MonitoredElement.NPhases;       // Force number of phases to be same
        if MonitoredElementTerminal > MonitoredElement.Nterms then
        begin
            DoErrorMsg('Relay: "' + Name + '"',
                'Terminal no. "' + '" does not exist.',
                'Re-specify terminal no.', 384);
        end
        else
        begin
               // Sets name of i-th terminal's connected bus in Relay's buslist
            Setbus(1, MonitoredElement.GetBus(MonitoredElementTerminal));
               // Allocate a buffer bigenough to hold everything from the monitored element
            ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder);
            CondOffset := (MonitoredElementTerminal - 1) * MonitoredElement.NConds; // for speedy sampling

            case ControlType of
                Generic:
                begin
                    if (MonitoredElement.DSSObjType and BASECLASSMASK) <> PC_ELEMENT then
                        DoSimpleMsg('Relay ' + Name + ': Monitored element for Generic relay is not a PC Element.', 385)
                    else
                    begin
                        MonitorVarIndex := (MonitoredElement as TPCelement).LookupVariable(MonitorVariable);
                        if MonitorVarIndex < 1 then    // oops
                        begin
                            DoSimpleMsg('Relay ' + Name + ': Monitor variable "' + MonitorVariable + '" does not exist.', 386);
                        end;
                    end;

                end;
            else

            end;
        end;
    end;

{Check for existence of Controlled Element}

         // If previously assigned, reset HasOCPDevice flag in case this is a move
    if Assigned(ControlledElement) then
    begin
        ControlledElement.HasOCPDevice := FALSE;
        ControlledElement.HasAutoOCPDevice := FALSE;
    end;

    Devindex := GetCktElementIndex(ElementName); // Global function
    if DevIndex > 0 then
    begin  // Both CktElement and monitored element must already exist
        ControlledElement := ActiveCircuit.CktElements.Get(DevIndex);
        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Make the 1 st terminal active

             // If the relay becomes disabled, leave at False
        if Enabled then
        begin
            ControlledElement.HasOCPDevice := TRUE;  // For Reliability calcs
            ControlledElement.HasAutoOCPDevice := TRUE;  // For Reliability calcs
        end;

        if ControlledElement.Closed[0] then    // Check state of phases of active terminal
        begin
            PresentState := CTRL_CLOSE;
            LockedOut := FALSE;
            OperationCount := 1;
            ArmedForOpen := FALSE;
        end
        else
        begin
            PresentState := CTRL_OPEN;
            LockedOut := TRUE;
            OperationCount := NumReclose + 1;
            ArmedForClose := FALSE;
        end;
    end
    else
    begin
        ControlledElement := NIL;   // element not found
        DoErrorMsg('Relay: "' + Self.Name + '"', 'CktElement Element "' + ElementName + '" Not Found.',
            ' Element must be defined previously.', 387);
    end;

         {Misc stuff}

    PickupAmps46 := BaseAmps46 * PctPickup46 * 0.01;

    case FNPhases of
        1:
            vbase := kVBase * 1000.0;
    else
        vbase := kVBase / SQRT3 * 1000.0;
    end;

    PickupVolts47 := vbase * PctPickup47 * 0.01;
end;

procedure TRelayObj.MakePosSequence;
begin
    if MonitoredElement <> NIL then
    begin
        Nphases := MonitoredElement.NPhases;
        Nconds := FNphases;
        Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    // Allocate a buffer bigenough to hold everything from the monitored element
        ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder);
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

{--------------------------------------------------------------------------}
procedure TRelayObj.CalcYPrim;
begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
end;

{--------------------------------------------------------------------------}
procedure TRelayObj.GetCurrents(Curr: pComplexArray);
var
    i: Integer;
begin

    for i := 1 to Fnconds do
        Curr^[i] := CZERO;

end;

{--------------------------------------------------------------------------}

procedure TRelayObj.GetInjCurrents(Curr: pComplexArray);
var
    i: Integer;
begin
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

{--------------------------------------------------------------------------}
procedure TRelayObj.DoPendingAction(const Code, ProxyHdl: Integer);


begin
    with   ControlledElement do
    begin
        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal of CktElement to terminal 1
        case Code of
            Integer(CTRL_OPEN):
                case PresentState of
                    CTRL_CLOSE:
                        if ArmedForOpen then
                        begin   // ignore if we became disarmed in meantime
                            ControlledElement.Closed[0] := FALSE;   // Open all phases of active terminal
                            if OperationCount > NumReclose then
                            begin
                                LockedOut := TRUE;
                                AppendtoEventLog('Relay.' + Self.Name, 'Opened on ' + RelayTarget + ' & Locked Out ');
                            end
                            else
                                AppendtoEventLog('Relay.' + Self.Name, 'Opened');
                            if PhaseTarget then
                                AppendtoEventLog(' ', 'Phase Target');
                            if GroundTarget then
                                AppendtoEventLog(' ', 'Ground Target');
                            ArmedForOpen := FALSE;
                        end;
                else {nada}
                end;
            Integer(CTRL_CLOSE):
                case PresentState of
                    CTRL_OPEN:
                        if ArmedForClose and not LockedOut then
                        begin
                            ControlledElement.Closed[0] := TRUE;    // Close all phases of active terminal
                            Inc(OperationCount);
                            AppendtoEventLog('Relay.' + Self.Name, 'Closed');
                            ArmedForClose := FALSE;
                        end;
                else {Nada}
                end;
            Integer(CTRL_RESET):
                case PresentState of
                    CTRL_CLOSE:
                        if not ArmedForOpen then
                            OperationCount := 1;       // Don't reset if we just rearmed
                else  {Nada}
                end;
        else
            {Do Nothing }
        end;

    end;
end;

{--------------------------------------------------------------------------}


procedure TRelayObj.InterpretRelayAction(const Action: String);
begin

    if ControlledElement <> NIL then
    begin
        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal
        case LowerCase(Action)[1] of

            'o', 't':
            begin
                ControlledElement.Closed[0] := FALSE;   // Open all phases of active terminal
                LockedOut := TRUE;
                OperationCount := NumReclose + 1;
            end;
            'c':
            begin
                ControlledElement.Closed[0] := TRUE;    // Close all phases of active terminal
                LockedOut := FALSE;
                OperationCount := 1;
            end;
        end;
    end;

end;

{--------------------------------------------------------------------------}
procedure TRelayObj.Sample;

begin

    ControlledElement.ActiveTerminalIdx := ElementTerminal;
    if ControlledElement.Closed[0]      // Check state of phases of active terminal
    then
        PresentState := CTRL_CLOSE
    else
        PresentState := CTRL_OPEN;

    case ControlType of
        CURRENT:
            OverCurrentLogic; {Current}
        VOLTAGE:
            VoltageLogic; {Reclosing Voltage Relay - definite time}
        REVPOWER:
            RevPowerLogic;    // one shot to lockout
        NEGCURRENT:
            NegSeq46Logic; // one shot to lockout
        NEGVOLTAGE:
            NegSeq47Logic; // one shot to lockout
        GENERIC:
            GenericLogic;// one shot to lockout
    end;
end;


{--------------------------------------------------------------------------}
procedure TRelayObj.DumpProperties(var F: TextFile; Complete: Boolean);

{Note PropertyValue is aligned with the internal indices}

var
    i: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[PropertyIdxMap[i]]);
        end;

    if Complete then
    begin
        Writeln(F);
    end;

end;

function TRelayObj.GetPropertyValue(Index: Integer): String;
var
    i: Integer;
begin
    Result := '';
    with ParentClass do
        case PropertyIdxMap[Index] of
            14:
            begin
                Result := '(';
                if NumReclose = 0 then
                    Result := Result + 'NONE'
                else
                    for i := 1 to NumReclose do
                        Result := Result + Format('%-g, ', [RecloseIntervals^[i]]);
                Result := Result + ')';
            end;
        else
            Result := inherited GetPropertyValue(Index);
        end;
end;


procedure TRelayObj.Reset;
begin

    PresentState := CTRL_CLOSE;
    Operationcount := 1;
    LockedOut := FALSE;
    ArmedForOpen := FALSE;
    ArmedForClose := FALSE;
    PhaseTarget := FALSE;
    GroundTarget := FALSE;

    NextTripTime := -1.0;  // not set to trip

    if ControlledElement <> NIL then
    begin
        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal
        ControlledElement.Closed[0] := TRUE;    // Close all phases of active terminal
    end;


end;

procedure TRelayObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := ''; //'element';
    PropertyValue[2] := '1'; //'terminal';
    PropertyValue[3] := '';
    PropertyValue[4] := '1'; //'terminal';
    PropertyValue[5] := 'current';
    PropertyValue[6] := '';
    PropertyValue[7] := '';
    PropertyValue[8] := '1.0';
    PropertyValue[9] := '1.0';
    PropertyValue[10] := '0.0';
    PropertyValue[11] := '0.0';
    PropertyValue[12] := '15';
    PropertyValue[13] := '4';
    PropertyValue[14] := '(0.5, 2.0, 2.0)';
    PropertyValue[15] := '';
    PropertyValue[16] := '';
    PropertyValue[17] := '0.0';
    PropertyValue[18] := '0.0';
    PropertyValue[19] := '';
    PropertyValue[20] := '';
    PropertyValue[21] := '20';
    PropertyValue[22] := '1';
    PropertyValue[23] := '100';
    PropertyValue[24] := '0';
    PropertyValue[25] := '2';
    PropertyValue[26] := '1.2';
    PropertyValue[27] := '0.8';
    PropertyValue[28] := '1.0';
    PropertyValue[29] := '1.0';


    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TRelayObj.InterpretRelayType(const S: String);
begin

    case lowercase(S)[1] of
        'c':
            ControlType := CURRENT;
        'v':
            ControlType := VOLTAGE;
        'r':
            ControlType := REVPOWER;
        '4':
            case S[2] of
                '6':
                    ControlType := NEGCURRENT;
                '7':
                    ControlType := NEGVOLTAGE;
            end;
        '8':
            ControlType := GENERIC;
    else
        ControlType := CURRENT;
    end;

              {Set Definite Time Defaults}
    case lowercase(S)[1] of
        'c':
            Delay_Time := 0.0;
        'v':
            Delay_Time := 0.0;
        'r':
            Delay_Time := 0.1;
        '4':
            Delay_Time := 0.1;
        '8':
            Delay_Time := 0.1;
    else
        Delay_Time := 0.0;
    end;

    PropertyValue[24] := Format('%-.g', [Delay_Time]);
end;

procedure TRelayObj.GenericLogic;
{ Generic relays only work on PC Elements With control terminals
}

var
    VarValue: Double;

begin

    with   MonitoredElement do
    begin
        VarValue := TPCElement(MonitoredElement).Variable[MonitorVarIndex];

      {Check for Trip}
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
        else   {Within bounds}
        begin  {Less Than pickup value: reset if armed}
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

{
  Negative Sequence Current Relay
  Patterned after Basler relay
}

var
    NegSeqCurrentMag, TripTime: Double;
    iOffset: Integer;
    I012: array[1..3] of Complex;

begin

    with   MonitoredElement do
    begin
        MonitoredElement.ActiveTerminalIdx := MonitoredElementTerminal;
        MonitoredElement.GetCurrents(cBuffer);
        iOffset := (MonitoredElementTerminal - 1) * MonitoredElement.NConds;  // offset for active terminal
        Phase2SymComp(@cBuffer^[iOffset + 1], @I012);
        NegSeqCurrentMag := Cabs(I012[3]);
        if NegSeqCurrentMag >= PickupAmps46 then
        begin
            if not ArmedForOpen then  // push the trip operation and arm to trip
                with ActiveCircuit do
                begin
                    RelayTarget := '-Seq Curr';
              {simple estimate of trip time assuming current will be constant}
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
        begin  {Less Than pickup value: reset if armed}
            if ArmedForOpen then    // We became unarmed, so reset and disarm
                with ActiveCircuit do
                begin
                    LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self);
                    ArmedForOpen := FALSE;
                end;
        end;
    end;  {With MonitoredElement}


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

    with   MonitoredElement do
    begin
        if PresentState = CTRL_CLOSE then
        begin
            TripTime := -1.0;
            GroundTime := -1.0;
            PhaseTime := -1.0;  {No trip}

           // Check largest Current of all phases of monitored element
            MonitoredElement.GetCurrents(cBuffer);

           {Check Ground Trip, if any}
            if ((GroundCurve <> NIL) or (Delay_Time > 0.0)) and (GroundTrip > 0.0) then
            begin
                Csum := CZERO;
                for i := (1 + CondOffset) to (Fnphases + CondOffset) do
                begin
                    caccum(Csum, cBuffer^[i]);
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
            end;

            if Groundtime > 0.0 then
            begin
                TripTime := GroundTime;
                GroundTarget := TRUE;
            end;

           // If GroundTime > 0 then we have a ground trip

           {Check Phase Trip, if any}

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
        end;  {IF PresentState=CLOSE}

    end;  {With MonitoredElement}

end;

procedure TRelayObj.RevPowerLogic;

var

    S: Complex;

begin

    with   MonitoredElement do
    begin
      //----MonitoredElement.ActiveTerminalIdx := MonitoredElementTerminal;
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
    end;  {With MonitoredElement}
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

    if not LockedOut then
        with   MonitoredElement do
        begin
   {**** Fix so that fastest trip time applies ****}
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

     {Convert to Per Unit}
            Vmax := Vmax / Vbase;
            Vmin := Vmin / Vbase;

            if PresentState = CTRL_CLOSE then
            begin
                TripTime := -1.0;
                OVTime := -1.0;
                UVTime := -1.0;


           {Check OverVoltage Trip, if any}
                if OVCurve <> NIL then
                    OVTime := OVCurve.GetOVtime(Vmax);

                if OVTime > 0.0 then
                begin
                    TripTime := OVTime;
                end;

           // If OVTime > 0 then we have a OV trip

           {Check UV Trip, if any}
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
                        with ActiveCircuit do    // If voltage dropped below pickup, disarm trip and set for reset
                        begin
                            ControlQueue.Delete(LastEventHandle);  // Delete last event from Queue
                            NextTripTime := -1.0;
                            LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self);
                            ArmedForOpen := FALSE;
                        end;
                end;
            end  {IF PresentState=CLOSE}
            else
            begin     {Present state is Open, Check for Voltage and then set reclose Interval}
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
                    else   {Armed, but check to see if voltage dropped before it reclosed and cancel action}
                    if Vmax < 0.9 then
                        ArmedForClose := FALSE;

            end;


        end;  {With MonitoredElement}

end;

procedure TRelayObj.NegSeq47Logic;

{Neg Seq voltage Relay}

var
    NegSeqVoltageMag: Double;
    V012: array[1..3] of Complex;

begin

    with   MonitoredElement do
    begin
        MonitoredElement.GetTermVoltages(MonitoredElementTerminal, cBuffer);
        Phase2SymComp(cBuffer, @V012); // Phase to symmetrical components
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
        begin  {Less Than pickup value: reset if armed}
            if ArmedForOpen then    // We became unarmed, so reset and disarm
                with ActiveCircuit do
                begin
                    LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self);
                    ArmedForOpen := FALSE;
                end;
        end;
    end;  {With MonitoredElement}


end;

initialization

end.
