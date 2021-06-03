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
    2-9-21  Added distance (21) and incremental distance (TD21) functions
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

INTERFACE

USES
     Command, ControlClass, ControlElem, CktElement, DSSClass, Arraydef, ucomplex,
      utilities, TCC_Curve, Math;

TYPE

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TRelay = class(TControlClass)
     private
        TCC_CurveClass:TDSSClass;
     protected
        PROCEDURE DefineProperties;
        FUNCTION MakeLike(const RelayName:String):Integer; override;
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit(ActorID : Integer):Integer; override;     // uses global parser
       FUNCTION NewObject(const ObjName:String):Integer; override;
       Function GetTccCurve(Const CurveName:String):TTCC_CurveObj;
   end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TRelayObj = class(TControlElem)
     private
            ControlType :Integer;


            {OverCurrent Relay}
            PhaseCurve,
            GroundCurve :TTCC_CurveObj;


            PhaseTrip,
            GroundTrip,
            PhaseInst,
            GroundInst : Double;

            RecloseIntervals :pdoubleArray;
            NumReclose       :Integer;

            ResetTime,
            Delay_Time,
            Breaker_time,
            TDPhase, TDGround  :double;

            RelayTarget:String;


            {over/Under Voltage Relay}
            OVcurve,                 // Curves assumed in per unit of base voltage
            UVCurve  :TTCC_CurveObj;

            Vbase,   // line-neut volts base
            kVBase   :Double;

            {46 Relay  Neg Seq Current}
            PickupAmps46,
            PctPickup46,
            BaseAmps46,
            Isqt46: Double;

            {47 Relay}
            PickupVolts47,
            PctPickup47:Double;

            {Distance Relay}
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
            {TD21 Relay}
            td21_i,           // present ring buffer index into td21_h
            td21_next,        // index to one cycle back, and next write location
            td21_pt: Integer; // number of time samples in td21_h
            td21_stride: Integer;  // length of a time sample in td21_h
            td21_quiet: Integer;   // wait this many samples after an operation
            td21_h: pComplexArray; // VI history pts, vi, phases
            td21_Uref: pComplexArray; // reference (pre-fault) voltages
            td21_dV: pComplexArray; // incremental voltages
            td21_dI: pComplexArray; // incremental currents
   
            {Generic Relay}
            OverTrip,
            UnderTrip:Double;

            FPresentState,
            FNormalState  :EControlAction;

            OperationCount :Integer;

            LockedOut,
            ArmedForClose,
            ArmedForOpen,
            ArmedForReset,
            PhaseTarget, GroundTarget,
            NormalStateSet    :Boolean;

            NextTriptime    : Double;
            LastEventHandle : Integer;

            CondOffset      :Integer; // Offset for monitored terminal

            cBuffer         :pComplexArray; // Complexarray buffer for an operating quantity
            cvBuffer        :pComplexArray; // for distance and td21 voltages, using cBuffer for hte currents

            DebugTrace   :Boolean;
            PROCEDURE InterpretRelayState(ActorID : Integer;const Action:String; const property_name: String);
            FUNCTION get_State: EControlAction;
            PROCEDURE set_State(const Value: EControlAction);
            FUNCTION get_NormalState: EControlAction;
            PROCEDURE set_NormalState(const Value: EControlAction);

            PROCEDURE InterpretRelayType(const S:String);

            PROCEDURE OvercurrentLogic(ActorID : Integer);
            PROCEDURE VoltageLogic(ActorID : Integer);
            PROCEDURE RevPowerLogic(ActorID : Integer);
            PROCEDURE NegSeq46Logic(ActorID : Integer);
            PROCEDURE NegSeq47Logic(ActorID : Integer);
            PROCEDURE GenericLogic(ActorID : Integer);
            PROCEDURE DistanceLogic(ActorID : Integer);
            PROCEDURE TD21Logic(ActorID : Integer);

     public

       MonitoredElementName     :String;
       MonitoredElementTerminal :Integer;

       constructor Create(ParClass:TDSSClass; const RelayName:String);
       destructor Destroy; override;

       PROCEDURE MakePosSequence(ActorID : Integer); Override;  // Make a positive Sequence Model
       PROCEDURE RecalcElementData(ActorID : Integer); Override;
       PROCEDURE CalcYPrim(ActorID : Integer); Override;    // Always Zero for a Relay

       PROCEDURE Sample(ActorID : Integer);  Override;    // Sample control quantities and set action times in Control Queue
       PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer;ActorID : Integer); Override;   // Do the action that is pending from last sample
       PROCEDURE Reset(ActorID : Integer); Override;  // Reset to initial defined state


       PROCEDURE GetCurrents(Curr: pComplexArray; ActorID : Integer); Override; // Get present value of terminal Curr
       PROCEDURE GetInjCurrents(Curr: pComplexArray; ActorID : Integer); Override;   // Returns Injextion currents

       FUNCTION  GetPropertyValue(Index:Integer):String;Override;
       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

       Property PresentState:EControlAction Read get_State  write set_State;
       Property NormalState:EControlAction Read get_NormalState write set_NormalState;

   end;


VAR
    ActiveRelayObj : TRelayObj;
    RelayClass     : TRelay;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit, PCElement,  Sysutils, uCmatrix, MathUtil, Classes;

CONST

    NumPropsThisClass = 40;

    CURRENT = 0;  {Default}
    VOLTAGE = 1;
    REVPOWER = 3;
    NEGCURRENT = 4;
    NEGVOLTAGE = 5;
    GENERIC = 6; {Use this for frequency, etc.  Generic over/under relay}
    DISTANCE = 7;
    TD21 = 8;

{--------------------------------------------------------------------------}
constructor TRelay.Create;  // Creates superstructure for all Relay objects
Begin
     Inherited Create;

     Class_name   := 'Relay';
     DSSClassType := DSSClassType + RELAY_CONTROL;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;

     TCC_CurveClass := GetDSSClassPtr('TCC_Curve');
     RelayClass := Self;
End;

{--------------------------------------------------------------------------}
destructor TRelay.Destroy;

Begin
     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TRelay.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count

     AllocatePropertyArrays;   {see DSSClass}


     // Define Property names
     // Addproperty (property name,  internal property index (see Edit), Help string);

     AddProperty('MonitoredObj',   1,
                'Full object name of the circuit element, typically a line, transformer, load, or generator, '+
                'to which the relay''s PT and/or CT are connected.' +
                ' This is the "monitored" element. ' +
                'There is no default; must be specified.');
     AddProperty('MonitoredTerm',  2 ,
                 'Number of the terminal of the circuit element to which the Relay is connected. '+
                 '1 or 2, typically.  Default is 1.');
     AddProperty( 'SwitchedObj',3,
                  'Name of circuit element switch that the Relay controls. '+
                  'Specify the full object name.' +
                  'Defaults to the same as the Monitored element. '+
                  'This is the "controlled" element.');
     AddProperty( 'SwitchedTerm',4,
                  'Number of the terminal of the controlled element in which the switch is controlled by the Relay. '+
                  '1 or 2, typically.  Default is 1.');
     AddProperty( 'type',5, 'One of a legal relay type:' +CRLF+
                        '  Current'+CRLF+
                        '  Voltage'+CRLF+
                        '  Reversepower'+CRLF+
                        '  46 (neg seq current)'+CRLF+
                        '  47 (neg seq voltage)'+CRLF+
                        '  Generic (generic over/under relay)'+CRLF+
                        '  Distance'+CRLF+
                        '  TD21'+CRLF+CRLF+
                        'Default is overcurrent relay (Current) ' +
                        'Specify the curve and pickup settings appropriate for each type. '+
                        'Generic relays monitor PC Element Control variables and trip on out of over/under range in definite time.');
     AddProperty( 'Phasecurve',6, 'Name of the TCC Curve object that determines the phase trip.  '+
                        'Must have been previously defined as a TCC_Curve object.'+
                        ' Default is none (ignored). '+
                        'For overcurrent relay, multiplying the current values in the curve by the "phasetrip" value gives the actual current.');
     AddProperty( 'Groundcurve',7, 'Name of the TCC Curve object that determines the ground trip.  Must have been previously defined as a TCC_Curve object.'+
                        ' Default is none (ignored).'+
                        'For overcurrent relay, multiplying the current values in the curve by the "groundtrip" valuw gives the actual current.');
     AddProperty( 'PhaseTrip', 8, 'Multiplier or actual phase amps for the phase TCC curve.  Defaults to 1.0.');
     AddProperty( 'GroundTrip',9, 'Multiplier or actual ground amps (3I0) for the ground TCC curve.  Defaults to 1.0.');
     AddProperty( 'TDPhase', 28, 'Time dial for Phase trip curve. Multiplier on time axis of specified curve. Default=1.0.');
     AddProperty( 'TDGround', 29, 'Time dial for Ground trip curve. Multiplier on time axis of specified curve. Default=1.0.');
     AddProperty( 'PhaseInst',10, 'Actual  amps (Current relay) or kW (reverse power relay) for instantaneous phase trip which is assumed to happen in 0.01 sec + Delay Time. Default is 0.0, which signifies no inst trip. '+
                         'Use this value for specifying the Reverse Power threshold (kW) for reverse power relays.');
     AddProperty( 'GroundInst',11, 'Actual  amps for instantaneous ground trip which is assumed to happen in 0.01 sec + Delay Time.Default is 0.0, which signifies no inst trip.');
     AddProperty( 'Reset',12, 'Reset time in sec for relay.  Default is 15. If this much time passes between the last pickup event, and the relay has not locked out, the operation counter resets.');
     AddProperty( 'Shots',13, 'Number of shots to lockout.  Default is 4. This is one more than the number of reclose intervals.');
     AddProperty( 'RecloseIntervals',14, 'Array of reclose intervals. If none, specify "NONE". Default for overcurrent relay is (0.5, 2.0, 2.0) seconds. ' +
                         'Default for a voltage relay is (5.0). In a voltage relay, this is  seconds after restoration of ' +
                         'voltage that the reclose occurs. ' +
                         'Reverse power relay is one shot to lockout, '+
                         'so this is ignored.  A locked out relay must be closed manually (set action=close).');
     AddProperty( 'Delay', 24, 'Trip time delay (sec) for DEFINITE TIME relays. Default is 0.0 for current and voltage relays.  If >0 then this value is used instead of curves. '+
                                       ' Used by Generic, RevPower, 46 and 47 relays. Defaults to 0.1 s for these relays.');
     AddProperty( 'Overvoltcurve', 15, 'TCC Curve object to use for overvoltage relay.  Curve is assumed to be defined with per unit voltage values. '+
                         'Voltage base should be defined for the relay. Default is none (ignored).');
     AddProperty( 'Undervoltcurve', 16, 'TCC Curve object to use for undervoltage relay.  Curve is assumed to be defined with per unit voltage values. '+
                         'Voltage base should be defined for the relay. Default is none (ignored).');
     AddProperty( 'kvbase', 17, 'Voltage base (kV) for the relay. Specify line-line for 3 phase devices); line-neutral for 1-phase devices.  Relay assumes ' +
                         'the number of phases of the monitored element.  Default is 0.0, which results in assuming the voltage ' +
                         'values in the "TCC" curve are specified in actual line-to-neutral volts.');
     AddProperty('47%Pickup', 25, 'Percent voltage pickup for 47 relay (Neg seq voltage). Default is 2. Specify also base voltage (kvbase) and delay time value.   ');
     AddProperty('46BaseAmps', 23, 'Base current, Amps, for 46 relay (neg seq current).' +
                                   '  Used for establishing pickup and per unit I-squared-t.' );
     AddProperty('46%Pickup', 21, 'Percent pickup current for 46 relay (neg seq current).  Default is 20.0. ' +
                                   '  When current exceeds this value * BaseAmps, I-squared-t calc starts.' );
     AddProperty('46isqt',22, 'Negative Sequence I-squared-t trip value for 46 relay (neg seq current).' +
                               '  Default is 1 (trips in 1 sec for 1 per unit neg seq current).  Should be 1 to 99.');
     AddProperty('Variable',  20, 'Name of variable in PC Elements being monitored.  Only applies to Generic relay.');
     AddProperty('overtrip', 26, 'Trip setting (high value) for Generic relay variable.  Relay trips in definite time if value of variable exceeds this value.');
     AddProperty('undertrip',27,'Trip setting (low value) for Generic relay variable.  Relay trips in definite time if value of variable is less than this value.');
     AddProperty('Breakertime',18, 'Fixed delay time (sec) added to relay time. Default is 0.0. Designed to represent breaker time or some other delay after a trip decision is made.' +
                         'Use Delay property for setting a fixed trip time delay.' +
                         'Added to trip time of current and voltage relays. Could use in combination with inst trip value to obtain a definite time overcurrent relay.');
     AddProperty( 'action', 19, 'DEPRECATED. See "State" property');
     AddProperty('Z1mag', 30, 'Positive sequence reach impedance in primary ohms for Distance and TD21 functions. Default=0.7');
     AddProperty('Z1ang', 31, 'Positive sequence reach impedance angle in degrees for Distance and TD21 functions. Default=64.0');
     AddProperty('Z0mag', 32, 'Zero sequence reach impedance in primary ohms for Distance and TD21 functions. Default=2.1');
     AddProperty('Z0ang', 33, 'Zero sequence reach impedance angle in degrees for Distance and TD21 functions. Default=68.0');
     AddProperty('Mphase', 34, 'Phase reach multiplier in per-unit for Distance and TD21 functions. Default=0.7');
     AddProperty('Mground', 35, 'Ground reach multiplier in per-unit for Distance and TD21 functions. Default=0.7');
     AddProperty('EventLog', 36, '{Yes/True* | No/False} Default is Yes for Relay. Write trips, reclose and reset events to EventLog.');
     AddProperty('DebugTrace', 37, '{Yes/True* | No/False} Default is No for Relay. Write extra details to Eventlog.');
     AddProperty('DistReverse', 38, '{Yes/True* | No/False} Default is No; reverse direction for distance and td21 types.');
     AddProperty('Normal', 39, '{Open | Closed} Normal state of the relay. The relay reverts to this state for reset, change of mode, etc. '  +
                               'Defaults to "State" if not specifically declared.');
     AddProperty('State', 40, '{Open | Closed} Actual state of the relay. Upon setting, immediately forces state of the relay, overriding the Relay control. ' +
                              'Simulates manual control on relay. Defaults to Closed. "Open" causes the controlled element to open and lock out. "Closed" causes the ' +
                              'controlled element to close and the relay to reset to its first operation.');

     ActiveProperty  := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{--------------------------------------------------------------------------}
FUNCTION TRelay.NewObject(const ObjName:String):Integer;
Begin
    // Make a new Relay and add it to Relay class list
    WITH ActiveCircuit[ActiveActor] Do
    Begin
      ActiveCktElement := TRelayObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    End;
End;
{--------------------------------------------------------------------------}

Function TRelay.GetTccCurve(Const CurveName:String):TTCC_CurveObj;

Begin

     Result := TCC_CurveClass.Find(CurveName);

     IF Result = NIL
     THEN DoSimpleMsg('TCC Curve object: "'+CurveName+'" not found.', 380);

End;

{--------------------------------------------------------------------------}
FUNCTION TRelay.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

Begin

  // continue parsing WITH contents of Parser
  ActiveRelayObj := ElementList.Active;
  ActiveCircuit[ActorID].ActiveCktElement := ActiveRelayObj;

  Result := 0;

  WITH ActiveRelayObj Do Begin

     ParamPointer := 0;
     ParamName := Parser[ActorID].NextParam;
     Param := Parser[ActorID].StrValue;
     WHILE Length(Param)>0 Do Begin
         IF Length(ParamName) = 0
         THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties)
         THEN PropertyValue[PropertyIdxMap[ParamPointer]]:= Param
         ELSE DoSimpleMsg('Unknown parameter "'+ParamName+'" for Relay "'+Name+'"', 381);

         If ParamPointer > 0 Then
         CASE PropertyIdxMap[ParamPointer] OF
           {internal Relay Property commands}
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 382);
            1: MonitoredElementName     := lowercase(param);
            2: MonitoredElementTerminal := Parser[ActorID].IntValue;
            3: ElementName     := lowercase(param);
            4: ElementTerminal := Parser[ActorID].IntValue;
            5: InterpretRelayType(Param);
            6: PhaseCurve  := GetTccCurve(Param);
            7: GroundCurve := GetTCCCurve(Param);
            8: PhaseTrip   := Parser[ActorID].Dblvalue;
            9: GroundTrip  := Parser[ActorID].Dblvalue;
           10: PhaseInst   := Parser[ActorID].Dblvalue;
           11: GroundInst  := Parser[ActorID].Dblvalue;
           12: ResetTime   := Parser[ActorID].Dblvalue;
           13: NumReclose  := Parser[ActorID].Intvalue -1 ;   // one less than number of shots
           14: If Comparetext(Param, 'NONE')=0 Then NumReclose := 1 Else NumReclose  := Parser[ActorID].ParseAsVector(4, RecloseIntervals);   // max of 4 allowed
           15: OVCurve     := GetTCCCurve(Param);
           16: UVCurve     := GetTCCCurve(Param);
           17: kVBase      := Parser[ActorID].DblValue;
           18: Breaker_time   := Parser[ActorID].DblValue;
           20: MonitorVariable := lowercase(param);  // for pc elements
           21: PctPickup46 := Parser[ActorID].DblValue;
           22: Isqt46   :=  Parser[ActorID].DblValue;
           23: BaseAmps46 := Parser[ActorID].DblValue;
           24: Delay_Time := Parser[ActorID].DblValue;
           25: PctPickup47 := Parser[ActorID].DblValue;
           26: Overtrip  := Parser[ActorID].DblValue;
           27: Undertrip := Parser[ActorID].DblValue;
           28: TDPhase :=  Parser[ActorID].DblValue;
           29: TDGround :=  Parser[ActorID].DblValue;
           30: Z1mag := Parser[ActorID].DblValue;
           31: Z1ang := Parser[ActorID].DblValue;
           32: Z0mag := Parser[ActorID].DblValue;
           33: Z0ang := Parser[ActorID].DblValue;
           34: Mphase := Parser[ActorID].DblValue;
           35: Mground := Parser[ActorID].DblValue;
           36: ShowEventLog := InterpretYesNo(param);
           37: DebugTrace   := InterpretYesNo(Param);
           38: Dist_Reverse  := InterpretYesNo(Param);
           39: Begin
                  InterpretRelayState(ActorID, Param, ParamName);  // set normal state
                  if not NormalStateSet then NormalStateSet := TRUE;
               End;
           19, 40: InterpretRelayState(ActorID, Param, ParamName);  // set state
         ELSE
           // Inherited parameters
           ClassEdit( ActiveRelayObj, ParamPointer - NumPropsthisClass)
         End;

         If ParamPointer > 0 Then
         CASE PropertyIdxMap[ParamPointer] OF
              {Default the controlled element to the monitored element}
              1: ElementName     := MonitoredElementName;
              2: ElementTerminal := MonitoredElementTerminal;
              5: Begin        {Set Default Reclose Intervals}
                    CASE lowercase(param)[1] of
                      'c': PropertyValue[14] := '[0.5, 2.0, 2.0]';
                      'v': PropertyValue[14] := '[5.0]';
                    END;
                    AuxParser[ActorID].CmdString := PropertyValue[14];
                    ParamName := AuxParser[ActorID].NextParam;
                    NumReclose := AuxParser[ActorID].ParseAsVector(4, RecloseIntervals);
                 End;
              19, 40: if not NormalStateSet then
                       Begin
                          NormalStateSet := TRUE;  // 'normal state' defaults to 'state' only when the latter is specified for the first time
                          NormalState := FPresentState;
                       End;
         END;
         ParamName := Parser[ActorID].NextParam;
         Param := Parser[ActorID].StrValue;
     End;

     RecalcElementData(ActorID);
  End;

End;



{--------------------------------------------------------------------------}
FUNCTION TRelay.MakeLike(const RelayName:String):Integer;
VAR
   OtherRelay:TRelayObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this Relay name in the present collection}
   OtherRelay := Find(RelayName);
   IF OtherRelay<>Nil THEN
   WITH ActiveRelayObj Do
     Begin

        NPhases := OtherRelay.Fnphases;
        NConds  := OtherRelay.Fnconds; // Force Reallocation of terminal stuff
        ShowEventLog := OtherRelay.ShowEventLog; // but leave DebugTrace off

        ElementName       := OtherRelay.ElementName;
        ElementTerminal   := OtherRelay.ElementTerminal;
        ControlledElement := OtherRelay.ControlledElement;  // Pointer to target circuit element

        MonitoredElement      := OtherRelay.MonitoredElement;  // Pointer to target circuit element
        MonitoredElementName  := OtherRelay.MonitoredElementName;  // Pointer to target circuit element
        MonitoredElementTerminal  := OtherRelay.MonitoredElementTerminal;  // Pointer to target circuit element

        PhaseCurve     := OtherRelay.PhaseCurve;
        GroundCurve    := OtherRelay.GroundCurve;
        OVCurve        := OtherRelay.OVCurve;
        UVcurve        := OtherRelay.UVcurve;
        PhaseTrip      := OtherRelay.PhaseTrip;
        GroundTrip     := OtherRelay.GroundTrip;
        TDPhase        := OtherRelay.TDPhase;
        TDGround       := OtherRelay.TDGround;
        PhaseInst      := OtherRelay.PhaseInst;
        GroundInst     := OtherRelay.GroundInst;
        ResetTime      := OtherRelay.Resettime;
        NumReclose     := OtherRelay.NumReclose;
        Delay_Time     := OtherRelay.Delay_Time;
        Breaker_time   := OtherRelay.Breaker_time;

        Reallocmem(RecloseIntervals, SizeOf(RecloseIntervals^[1])*4);      // Always make a max of 4
        FOR i := 1 to NumReclose DO RecloseIntervals^[i] :=  OtherRelay.RecloseIntervals^[i];
       // deleted... if DebugTrace then AppendToEventLog ('Relay.'+self.Name, Format ('MakeLike NumReclose=%d',[NumReclose]), ActorID);

        kVBase         := OtherRelay.kVBase;
        LockedOut      := OtherRelay.LockedOut;

        FPresentState  := OtherRelay.FPresentState;
        NormalState    := OtherRelay.NormalState;

        ControlType    := OtherRelay.ControlType;
        CondOffset     := OtherRelay.CondOffset;

        {46 Relay  Neg Seq Current}
        PickupAmps46   := OtherRelay.PickupAmps46;
        PctPickup46    := OtherRelay.PctPickup46;
        BaseAmps46     := OtherRelay.BaseAmps46;
        Isqt46         := OtherRelay.Isqt46;

        {47 Relay}
        PickupVolts47  := OtherRelay.PickupVolts47;
        PctPickup47    := OtherRelay.PctPickup47;

        {Generic Relay}
        MonitorVariable    := OtherRelay.MonitorVariable;
        OverTrip      := OtherRelay.OverTrip;
        UnderTrip     := OtherRelay.UnderTrip;

        {Distance Relays}
        Z1Mag   := OtherRelay.Z1Mag;
        Z1Ang   := OtherRelay.Z1Ang;
        Z0Mag   := OtherRelay.Z0Mag;
        Z0Ang   := OtherRelay.Z0Ang;
        Mphase  := OtherRelay.Mphase;
        Mground := OtherRelay.Mground;
        Dist_Reverse := OtherRelay.Dist_Reverse;

        For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherRelay.PropertyValue[i];

     End
   ELSE  DoSimpleMsg('Error in Relay MakeLike: "' + RelayName + '" Not Found.', 383);

End;




{==========================================================================}
{                    TRelayObj                                           }
{==========================================================================}



{--------------------------------------------------------------------------}
constructor TRelayObj.Create(ParClass:TDSSClass; const RelayName:String);

Begin
     Inherited Create(ParClass);
     Name := LowerCase(RelayName);
     DSSObjType := ParClass.DSSClassType;

     NPhases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms := 1;  // this forces allocation of terminals and conductors
                         // in base class
     ElementName   := '';
     ControlledElement := NIL;
     ElementTerminal := 1;

     MonitoredElementName := '';
     MonitoredElementTerminal := 1;
     MonitoredElement := NIL;

     RelayTarget := '';

     PhaseCurve       := NIL;
     GroundCurve       := NIL;
     OVCurve        := NIL;
     UVcurve        := NIL;
     PhaseTrip      := 1.0;
     GroundTrip     := 1.0;
     TDPhase        := 1.0;
     TDGround       := 1.0;
     PhaseInst      := 0.0;
     GroundInst     := 0.0;
     ResetTime      := 15.0;
     NumReclose     := 3;
     RecloseIntervals := NIL;

     Reallocmem(RecloseIntervals, SizeOf(RecloseIntervals^[1])*4); // fixed allocation of 4
     RecloseIntervals^[1] := 0.5;
     RecloseIntervals^[2] := 2.0;
     RecloseIntervals^[3] := 2.0;

     FPresentState := CTRL_CLOSE;
     FNormalState  := CTRL_CLOSE;
     NormalStateSet := FALSE;

     Isqt46 := 1.0;
     BaseAmps46 := 100.0;
     PctPickup46 := 20.0;
     PickupAmps46 := BaseAmps46*PctPickup46*0.01;

     PctPickup47 := 2.0;

     overtrip  := 1.2;
     undertrip := 0.8;

     Z1Mag := 0.7;
     Z1Ang := 64.0;
     Z0Mag := 2.1;
     Z0Ang := 68.0;
     Mphase := 0.7;
     Mground := 0.7;
     td21_i := -1;
     td21_h := nil;
     td21_dV := nil;
     td21_Uref := nil;
     td21_dI := nil;
     td21_pt := 0;
     td21_stride := 0;
     td21_quiet := 0;
     Dist_Reverse := False;

     Operationcount   := 1;
     LockedOut        := FALSE;
     ArmedForOpen     := FALSE;
     ArmedForClose    := FALSE;
     ArmedForReset    := FALSE;
     PhaseTarget      := FALSE;
     GroundTarget     := FALSE;

     NextTripTime     := -1.0;  // not set to trip

     cBuffer := Nil; // Complex buffer
     cvBuffer := Nil;

     DSSObjType := ParClass.DSSClassType; //cap_CONTROL;

     InitPropertyValues(0);



   //  RecalcElementData;

End;

destructor TRelayObj.Destroy;
Begin
     MonitoredElementName := '';
     ReallocMem(RecloseIntervals, 0);
     if Assigned (cBuffer) then ReallocMem (cBuffer, 0);
     if Assigned (cvBuffer) then ReallocMem (cvBuffer, 0);
     if Assigned (td21_h) then ReallocMem (td21_h, 0);
     if Assigned (td21_dV) then ReallocMem (td21_dV, 0);
     if Assigned (td21_Uref) then ReallocMem (td21_Uref, 0);
     if Assigned (td21_dI) then ReallocMem (td21_dI, 0);
     Inherited Destroy;
End;

{--------------------------------------------------------------------------}
PROCEDURE TRelayObj.RecalcElementData(ActorID : Integer);

VAR
   DevIndex: Integer;

Begin
         if DebugTrace then begin
            AppendToEventLog ('Relay.'+self.Name, Format ('RecalcElementData NumReclose=%d',[NumReclose]), ActorID);
         end;
         Devindex := GetCktElementIndex(MonitoredElementName); // Global function
         IF   DevIndex>0 THEN
           Begin
             MonitoredElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
             Nphases := MonitoredElement.NPhases;       // Force number of phases to be same
             IF MonitoredElementTerminal > MonitoredElement.Nterms THEN
               Begin
                 DoErrorMsg('Relay: "' + Name + '"',
                                 'Terminal no. "' +'" does not exist.',
                                 'Re-specify terminal no.', 384);
               End
             ELSE
               Begin
               // Sets name of i-th terminal's connected bus in Relay's buslist
                 Setbus(1, MonitoredElement.GetBus(MonitoredElementTerminal));
               // Allocate a buffer bigenough to hold everything from the monitored element
                 ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder );
                 if (ControlType = Distance) or (ControlType = TD21) then
                   ReAllocMem(cvBuffer, SizeOF(cvBuffer^[1]) * MonitoredElement.Yorder);
                 CondOffset := (MonitoredElementTerminal-1) * MonitoredElement.NConds; // for speedy sampling

                 CASE ControlType of
                      Generic:
                        Begin
                          IF (MonitoredElement.DSSObjType And BASECLASSMASK) <> PC_ELEMENT Then
                            DoSimpleMsg('Relay '+Name+': Monitored element for Generic relay is not a PC Element.', 385)
                          ELSE
                           Begin
                              MonitorVarIndex := (MonitoredElement As TPCelement).LookupVariable(MonitorVariable);
                              If MonitorVarIndex < 1 Then    // oops
                               Begin
                                  DoSimpleMsg('Relay '+Name+': Monitor variable "'+MonitorVariable+'" does not exist.', 386);
                               End;
                           End;

                        End;
                 ELSE

                 END;
               End;
           End;

{Check for existence of Controlled Element}

         // If previously assigned, reset HasOCPDevice flag in case this is a move
         If Assigned(ControlledElement) Then Begin
            ControlledElement.HasOCPDevice := FALSE;
            ControlledElement.HasAutoOCPDevice := FALSE;
         End;

         Devindex := GetCktElementIndex(ElementName); // Global function
         IF   DevIndex>0   THEN
           Begin  // Both CktElement and monitored element must already exist
             ControlledElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
             ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Make the 1 st terminal active

             // If the relay becomes disabled, leave at False
             If Enabled Then Begin
               ControlledElement.HasOCPDevice     := TRUE;  // For Reliability calcs
               ControlledElement.HasAutoOCPDevice := TRUE;  // For Reliability calcs
             End;

             IF  FPresentState = CTRL_CLOSE  THEN    // Open/Close State of controlled element based on state assigned to the control
               Begin
                ControlledElement.Closed[0,ActorID] := TRUE;
                LockedOut := FALSE;
                OperationCount := 1;
                ArmedForOpen := FALSE;
               End
             ELSE
               Begin
                ControlledElement.Closed[0,ActorID] := FALSE;
                LockedOut := TRUE;
                OperationCount := NumReclose + 1;
                ArmedForClose := FALSE;
               End;
           End
         ELSE
           Begin
            ControlledElement := nil;   // element not found
            DoErrorMsg('Relay: "' + Self.Name + '"', 'CktElement Element "'+ ElementName + '" Not Found.',
                            ' Element must be defined previously.', 387);
           End;

         {Misc stuff}

         PickupAmps46 := BaseAmps46 * PctPickup46 * 0.01;

         CASE FNPhases of
            1: vbase := kVBase * 1000.0;
         ELSE
             vbase := kVBase/SQRT3 * 1000.0 ;
         END;

         PickupVolts47 := vbase * PctPickup47 * 0.01;

         if (ControlType = DISTANCE) or (ControlType = TD21) then begin
            Dist_Z1 := pclx (Z1Mag, Z1Ang/RadiansToDegrees);
            Dist_Z0 := pclx (Z0Mag, Z0Ang/RadiansToDegrees);
            Dist_K0 := cdiv (cdivreal (csub (Dist_Z0, Dist_Z1), 3.0), Dist_Z1);
         end;
End;

procedure TRelayObj.MakePosSequence(ActorID : Integer);
begin
  if MonitoredElement <> Nil then begin
    Nphases := MonitoredElement.NPhases;
    Nconds := FNphases;
    Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    // Allocate a buffer big enough to hold everything from the monitored element
    ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder );
    if (ControlType = Distance) or (ControlType = TD21) then
      ReAllocMem(cvBuffer, SizeOF(cvBuffer^[1]) * MonitoredElement.Yorder );
    CondOffset := (ElementTerminal-1) * MonitoredElement.NConds; // for speedy sampling
  end;
  CASE FNPhases of
    1: vbase := kVBase * 1000.0;
  ELSE
    vbase := kVBase/SQRT3 * 1000.0 ;
  END;
  PickupVolts47 := vbase * PctPickup47 * 0.01;
  inherited;
end;

{--------------------------------------------------------------------------}
PROCEDURE TRelayObj.CalcYPrim(ActorID : Integer);
Begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
End;

{--------------------------------------------------------------------------}
PROCEDURE TRelayObj.GetCurrents(Curr: pComplexArray; ActorID : Integer);
VAR
   i:Integer;
Begin

  For i := 1 to Fnconds Do Curr^[i] := CZERO;

End;
{--------------------------------------------------------------------------}

PROCEDURE TRelayObj.GetInjCurrents(Curr: pComplexArray; ActorID : Integer);
Var i:Integer;
Begin
     FOR i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

{--------------------------------------------------------------------------}
PROCEDURE TRelayObj.DoPendingAction(Const Code, ProxyHdl:Integer;ActorID : Integer);


begin
    if DebugTrace then begin
      AppendToEventLog ('Relay.'+self.Name,
        Format('DoPendingAction Code=%d State=%d ArmedOpen=%s Close=%s Reset=%s Count=%d NumReclose=%d',
          [Integer (Code), Integer (FPresentState), BoolToStr (ArmedForOpen), BoolToStr (ArmedForClose), BoolToStr (ArmedForReset),
          OperationCount, NumReclose]), ActorID);
    end;
    WITH   ControlledElement Do
      Begin
         ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal of CktElement to terminal 1
         CASE Code of
            Integer(CTRL_OPEN):   CASE FPresentState of
                         CTRL_CLOSE:IF ArmedForOpen THEN
                                 Begin   // ignore if we became disarmed in meantime
                                    ControlledElement.Closed[0,ActorID] := FALSE;   // Open all phases of active terminal
                                    IF OperationCount > NumReclose THEN
                                      Begin
                                          LockedOut := TRUE;
                                          if ShowEventLog then AppendtoEventLog('Relay.'+Self.Name, 'Opened on '+RelayTarget+' & Locked Out ', ActorID);
                                       End
                                    ELSE if ShowEventLog then AppendtoEventLog('Relay.'+Self.Name, 'Opened on ' + RelayTarget, ActorID);
                                    If PhaseTarget Then if ShowEventLog then AppendtoEventLog(' ', 'Phase Target', ActorID);
                                    If GroundTarget Then if ShowEventLog then AppendtoEventLog(' ', 'Ground Target', ActorID);
                                    ArmedForOpen := FALSE;
                                    if ControlType = td21 then td21_quiet := td21_pt + 1;
                                 END;
                    ELSE {nada}
                    END;
            Integer(CTRL_CLOSE):  CASE FPresentState of
                         CTRL_OPEN:IF ArmedForClose and Not LockedOut THEN
                                Begin
                                  ControlledElement.Closed[0, ActorID] := TRUE;    // Close all phases of active terminal
                                  Inc(OperationCount);
                                  if ShowEventLog then AppendtoEventLog('Relay.'+Self.Name, 'Closed', ActorID);
                                  ArmedForClose     := FALSE;
                                  if ControlType = td21 then td21_quiet := td21_pt div 2;
                                End;
                    ELSE {Nada}
                    END;
            Integer(CTRL_RESET): If ArmedForReset and Not LockedOut then begin
                                  if ShowEventLog then AppendToEventLog('Relay.'+Self.Name, 'Reset', ActorID);
                                  Reset(ActorID);
                                  if ControlType = td21 then td21_quiet := td21_pt div 2
                                End
         ELSE
            {Do Nothing }
         END;

    End;
end;

{--------------------------------------------------------------------------}


PROCEDURE TRelayObj.InterpretRelayState(ActorID: Integer; const Action:String; const property_name: String);
Begin

   if (LowerCase(property_name[1]) = 's') or (LowerCase(property_name[1]) = 'a')  then begin  // state or action (deprecated)
       Case LowerCase(Action)[1] of
            'o','t': FPresentState := CTRL_OPEN;
            'c': FPresentState := CTRL_CLOSE;
       END;
   end
   Else // Normal
   Begin

          Case LowerCase(Action)[1] of
          'o','t': FNormalState := CTRL_OPEN;
          'c': FNormalState := CTRL_CLOSE;
          END;

   End;

End;

{--------------------------------------------------------------------------}
PROCEDURE TRelayObj.Sample(ActorID : Integer);

begin

     ControlledElement.ActiveTerminalIdx := ElementTerminal;
     IF  ControlledElement.Closed [0,ActorID]      // Check state of phases of active terminal
     THEN FPresentState := CTRL_CLOSE
     ELSE FPresentState := CTRL_OPEN;

         CASE ControlType of
              CURRENT:     OverCurrentLogic(ActorID); {Current}
              VOLTAGE:     VoltageLogic(ActorID); {Reclosing Voltage Relay - definite time}
              REVPOWER:    RevPowerLogic(ActorID);    // one shot to lockout
              NEGCURRENT:  NegSeq46Logic(ActorID); // one shot to lockout
              NEGVOLTAGE:  NegSeq47Logic(ActorID); // one shot to lockout
              GENERIC:     GenericLogic(ActorID);// one shot to lockout
              DISTANCE:    DistanceLogic(ActorID);
              TD21:        TD21Logic(ActorID);
         End;
end;



{--------------------------------------------------------------------------}
PROCEDURE TRelayObj.DumpProperties(Var F:TextFile; Complete:Boolean);

{Note PropertyValue is aligned with the internal indices}

VAR
   i:Integer;

Begin
    Inherited DumpProperties(F,Complete);

    WITH ParentClass Do
     For i := 1 to NumProperties Do
       Begin
         Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[PropertyIdxMap[i]]);
       End;

    If Complete THEN
      Begin
        Writeln(F);
      End;

End;

FUNCTION TRelayObj.GetPropertyValue(Index: Integer): String;
VAR
   i: Integer;
begin
        Result := '';
        With ParentClass Do
          CASE Index of
            14: Begin
                  Result := '(';
                  If NumReclose=0 Then Result := Result + 'NONE' Else
                     FOR i := 1 to NumReclose Do Result := Result + Format('%-g, ' , [RecloseIntervals^[i]]);
                  Result := Result + ')';
                End;
            39: Begin
                  case FNormalState of
                    CTRL_OPEN: Result := 'open';
                    else
                    {CTRL_CLOSE:} Result := 'closed';
                  end;
                End;
         19,40: Begin
                  case FPresentState of
                    CTRL_OPEN: Result := 'open';
                    else
                    {CTRL_CLOSE:} Result := 'closed';
                  end;
                End
          ELSE
             Result := Inherited GetPropertyValue(Index);
          END;
end;


Procedure TRelayObj.Reset(ActorID : Integer);
Begin
     if ShowEventLog then AppendToEventLog (self.Name, 'Resetting', ActorID);

     FPresentState   := FNormalState;

     ArmedForOpen   := FALSE;
     ArmedForClose  := FALSE;
     ArmedForReset  := FALSE;
     PhaseTarget      := FALSE;
     GroundTarget     := FALSE;

     NextTripTime   := -1.0;  // not set to trip

    IF ControlledElement <> NIL  THEN
      Begin
         ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal

         case FNormalState of
           CTRL_OPEN: Begin
             ControlledElement.Closed[0, Activeactor] := FALSE; // Open all phases of active terminal
             LockedOut := TRUE;
             OperationCount := NumReclose + 1;
           End

           else
           {CTRL_CLOSE} Begin
                          ControlledElement.Closed[0,ActiveActor] := TRUE;    // Close all phases of active terminal
                          LockedOut := FALSE;
                          OperationCount := 1;
                        End;
         end;

      End;

end;

Function TRelayObj.get_State: EControlAction;
Begin

      IF ControlledElement <> NIL  THEN
      Begin

         ControlledElement.ActiveTerminalIdx  := ElementTerminal;  // Set active terminal
          case ControlledElement.Closed[0,ActiveActor] of
            FALSE:  FPresentState:= CTRL_OPEN;
          else
            {TRUE:} FPresentState:= CTRL_CLOSE;
          end;

      End;

      Result := FPresentState;
End;

Procedure TRelayObj.set_State(const Value: EControlAction);
Begin

        If PresentState <> Value Then Begin

            IF ControlledElement <> NIL  THEN
            Begin
              ControlledElement.ActiveTerminalIdx  := ElementTerminal;  // Set active terminal
              case Value of
                 CTRL_OPEN:   Begin
                                ControlledElement.Closed[0,ActiveActor] := FALSE;
                                LockedOut := TRUE;
                                OperationCount := NumReclose+1;
                                ArmedForClose := FALSE;
                                ArmedForReset := FALSE;
                              End

              else
                {CTRL_CLOSE:} Begin
                                ControlledElement.Closed[0,ActiveActor] := TRUE;
                                LockedOut := FALSE;
                                OperationCount := 1;
                                ArmedForOpen := FALSE;
                                ArmedForReset := FALSE;
                              end

              end;
            End;

            FPresentState := Value;
        End;
End;

Function TRelayObj.get_NormalState: EControlAction;
Begin
        Result := FNormalState;
End;

Procedure TRelayObj.set_NormalState(const Value: EControlAction);
Begin
      If FNormalState <> Value Then Begin
          FNormalState := Value;
      End;
End;

procedure TRelayObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1]  := ''; //'element';
     PropertyValue[2]  := '1'; //'terminal';
     PropertyValue[3]  := '';
     PropertyValue[4]  := '1'; //'terminal';
     PropertyValue[5]  := 'current';
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
     PropertyValue[19] := 'closed';
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
     PropertyValue[30] := '0.7';
     PropertyValue[31] := '64.0';
     PropertyValue[32] := '2.1';
     PropertyValue[33] := '68.0';
     PropertyValue[34] := '0.7';
     PropertyValue[35] := '0.7';
     PropertyValue[36] := 'Yes';
     PropertyValue[37] := 'No';
     PropertyValue[39] := 'closed';
     PropertyValue[40] := 'closed';

  inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TRelayObj.InterpretRelayType(const S: String);
begin

              CASE lowercase(S)[1] of
                    'c': ControlType := CURRENT;
                    'v': ControlType := VOLTAGE;
                    'r': ControlType := REVPOWER;
                    '4': Case S[2] of
                          '6':ControlType := NEGCURRENT;
                          '7':ControlType := NEGVOLTAGE;
                         End;
                    'g': ControlType := GENERIC;
                    'd': ControlType := DISTANCE;
                    't': ControlType := TD21;
               ELSE
                     ControlType := CURRENT;
               End;

              {Set Definite Time Defaults}
              CASE lowercase(S)[1] of
                    'c': Delay_Time := 0.0;
                    'v': Delay_Time := 0.0;
                    'r': Delay_Time := 0.1;
                    '4': Delay_Time := 0.1;
                    'g': Delay_Time := 0.1;
                    'd': Delay_Time := 0.1;
                    't': Delay_Time := 0.1;
               ELSE
                     Delay_Time := 0.0;
               End;

               PropertyValue[24] := Format('%-.g',[Delay_Time]);
end;

procedure TRelayObj.GenericLogic(ActorID : Integer);
{ Generic relays only work on PC Elements With control terminals
}

Var
   VarValue:Double;

begin

 WITH   MonitoredElement Do
   Begin
      VarValue := TPCElement(MonitoredElement).Variable[MonitorVarIndex];

      {Check for Trip}
      IF (VarValue >  OverTrip) or (VarValue < UnderTrip) THEN
        Begin
              IF Not ArmedForOpen THEN  // push the trip operation and arm to trip
               WITH ActiveCircuit[ActorID]  Do
                Begin
                 RelayTarget := TPCElement(MonitoredElement).VariableName(MonitorVarIndex);
                 LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + Delay_Time + Breaker_time, CTRL_OPEN, 0, Self, ActorID);
                 OperationCount := NumReclose + 1;  // force a lockout
                 ArmedForOpen := TRUE;
                End
        End
      ELSE   {Within bounds}
        Begin  {Less Than pickup value: reset if armed}
              IF ArmedForOpen  THEN    // We became unarmed, so reset and disarm
               WITH ActiveCircuit[ActorID] Do
                Begin
                 LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self, ActorID);
                 ArmedForOpen := FALSE;
                End;
        End;


   End;  {With MonitoredElement}

end;

procedure TRelayObj.NegSeq46Logic(ActorID : Integer);

{
  Negative Sequence Current Relay
  Patterned after Basler relay
}

VAR
    NegSeqCurrentMag, TripTime : Double;
    iOffset:Integer;
    I012 :Array[1..3] of Complex;

begin

 WITH   MonitoredElement Do
   Begin
      MonitoredElement.ActiveTerminalIdx := MonitoredElementTerminal;
      MonitoredElement.GetCurrents(cBuffer,ActorID);
      iOffset := (MonitoredElementTerminal - 1)*MonitoredElement.NConds;  // offset for active terminal
      Phase2SymComp(@cBuffer^[iOffset+1], @I012);
      NegSeqCurrentMag :=  Cabs(I012[3]);
      IF NegSeqCurrentMag >= PickupAmps46  THEN
        Begin
          IF Not ArmedForOpen THEN  // push the trip operation and arm to trip
           WITH ActiveCircuit[ActorID]  Do
            Begin
             RelayTarget := '-Seq Curr';
              {simple estimate of trip time assuming current will be constant}
             If Delay_Time > 0.0 Then Triptime := Delay_Time
             Else Triptime := Isqt46 / sqr(NegSeqCurrentMag/BaseAmps46); // Sec
             LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + Breaker_time, CTRL_OPEN, 0, Self, ActorID);
             OperationCount := NumReclose + 1;  // force a lockout
             ArmedForOpen := TRUE;
            End
        End
      ELSE
        Begin  {Less Than pickup value: reset if armed}
              IF ArmedForOpen  THEN    // We became unarmed, so reset and disarm
               WITH ActiveCircuit[ActorID] Do
                Begin
                 LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self, ActorID);
                 ArmedForOpen := FALSE;
                End;
        End;
   End;  {With MonitoredElement}


end;

procedure TRelayObj.OvercurrentLogic(ActorID : Integer);

VAR
   i     :Integer;
   Cmag  :Double;
   CSum  :Complex ;

   GroundTime,
   PhaseTime,
   TripTime,
   TimeTest :Double;

begin

 WITH   MonitoredElement Do
   Begin
     IF FPresentState = CTRL_CLOSE
     THEN Begin
           TripTime := -1.0;
           GroundTime := -1.0;
           PhaseTime := -1.0;  {No trip}

           // Check largest Current of all phases of monitored element
           MonitoredElement.GetCurrents(cBuffer, ActorID);

           {Check Ground Trip, if any}
           IF ((GroundCurve <> NIL) or (Delay_Time > 0.0)) and (GroundTrip > 0.0)
           THEN Begin
               Csum := CZERO;
               FOR i := (1 + CondOffset) to (Fnphases + CondOffset) Do
               Begin
                   caccum(Csum, cBuffer^[i] );
               End;
               Cmag  :=  Cabs(Csum);
               IF (GroundInst>0.0) AND (Cmag>=GroundInst) AND (OperationCount=1)
               THEN GroundTime := 0.01 + Breaker_time      // Inst trip on first operation
               ELSE
                 If Delay_Time > 0.0 Then  Begin // Definite Time Ground Relay
                    If  (Cmag >= GroundTrip) Then GroundTime := Delay_Time
                    Else GroundTime := -1.0;
                 End
                 Else GroundTime := TDGround *  GroundCurve.GetTCCTime(Cmag/ GroundTrip);
              if DebugTrace then
                AppendToEventLog ('Relay.'+Self.Name, Format ('Ground Trip: Mag=%.3g, Mult=%.3g, Time=%.3g',
                  [Cmag, Cmag / GroundTrip, GroundTime]),ActorID);
           End;

           IF Groundtime > 0.0 THEN Begin
             TripTime := GroundTime;
             GroundTarget := TRUE;
           End;

           // If GroundTime > 0 then we have a ground trip

           {Check Phase Trip, if any}

           IF ((PhaseCurve <> NIL) or (Delay_Time>0.0)) and (PhaseTrip > 0.0) Then
             Begin
               FOR i := (1 + CondOffset) to (Fnphases + CondOffset) Do
                 Begin
                   Cmag :=  Cabs( cBuffer^[i]);
                   IF (PhaseInst>0.0) AND (Cmag>=PhaseInst) AND (OperationCount=1)  THEN
                     Begin
                       PhaseTime := 0.01 + Breaker_time;  // Inst trip on first operation
                       Break;  {FOR - if Inst, no sense checking other phases}
                     End
                   ELSE
                     Begin
                       If Delay_Time>0.0 Then  Begin // Definite Time Phase Relay
                          If  (Cmag>=PhaseTrip) Then TimeTest := Delay_Time
                          Else TimeTest := -1.0;
                       End
                       Else TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag/PhaseTrip);
                       IF (TimeTest > 0.0) THEN
                         Begin
                           IF Phasetime<0.0 THEN PhaseTime := TimeTest
                           ELSE PhaseTime := Min(PhaseTime, TimeTest);
                         End;
                     End;
                    if DebugTrace then
                      AppendToEventLog ('Relay.'+Self.Name, Format ('Phase %d Trip: Mag=%.3g, Mult=%.3g, Time=%.3g',
                        [i-CondOffset, Cmag, Cmag / PhaseTrip, PhaseTime]),ActorID);
                 End;
             End;
           // If PhaseTime > 0 then we have a phase trip

           IF   PhaseTime > 0.0 THEN
             Begin
                PhaseTarget := TRUE;
                IF   TripTime > 0.0
                THEN TripTime := Min(TripTime, Phasetime)
                ELSE TripTime := PhaseTime;
             End;

           IF   TripTime > 0.0 THEN
             Begin
              IF Not ArmedForOpen THEN
               WITH ActiveCircuit[ActorID] Do   // Then arm for an open operation
                Begin
                   RelayTarget := '';
                   If Phasetime>0.0 Then   RelayTarget := RelayTarget + 'Ph';
                   If Groundtime>0.0 Then RelayTarget := RelayTarget + ' Gnd';
                   LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + Breaker_time, CTRL_OPEN, 0,Self, ActorID);
                   IF OperationCount <= NumReclose THEN LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + Breaker_time + RecloseIntervals^[OperationCount], CTRL_CLOSE, 0, Self, ActorID);
                   ArmedForOpen := TRUE;
                   ArmedForClose := TRUE;
                End;
             End
           ELSE
             Begin
               IF ArmedForOpen  THEN
                 WITH ActiveCircuit[ActorID] Do    // If current dropped below pickup, disarm trip and set for reset
                   Begin
                    LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self, ActorID);
                    ArmedForOpen := FALSE;
                    ArmedForClose := FALSE;
                    PhaseTarget      := FALSE;
                    GroundTarget     := FALSE;
                   End;
            End;
     End;  {IF PresentState=CLOSE}

   End;  {With MonitoredElement}

end;

procedure TRelayObj.DistanceLogic(ActorID : Integer);
var
  i, j: Integer;
  Vloop, Iloop, Zloop, Ires, kIres, Zreach: Complex;
  i2, min_distance, fault_distance, t_event: Double;
  Targets: TStringList;
  PickedUp: Boolean;
begin
  If Not LockedOut Then with MonitoredElement Do Begin
    PickedUp := False;
    min_distance := 1.0e30;
    MonitoredElement.GetCurrents(cBuffer, ActorID);
    if Dist_Reverse then
      for I := 1 to MonitoredElement.NPhases do
        cBuffer^[i+CondOffset] := cnegate (cBuffer^[i+CondOffset]);
    Ires := cZERO;
    for i := 1 to MonitoredElement.Nphases do caccum (Ires, cBuffer^[i+CondOffset]);
    kIres := cmul (Dist_K0, Ires);
    MonitoredElement.GetTermVoltages(MonitoredElementTerminal, cvBuffer, ActorID);
    for i := 1 to MonitoredElement.NPhases do begin
      for j := i to MonitoredElement.NPhases do begin
        if (i = j) then begin
          Vloop := cvBuffer^[i];
          Iloop := cadd (cBuffer^[i+CondOffset], kIres);
          Zreach := cmulreal (Dist_Z1, Mground); // not Dist_Z0 because it's included in Dist_K0
        end else begin
          Vloop := csub (cvBuffer^[i], cvBuffer^[j]);
          Iloop := csub (cBuffer^[i+CondOffset], cBuffer^[j+CondOffset]);
          Zreach := cmulreal (Dist_Z1, Mphase);
        end;
        i2 := Iloop.re * Iloop.re + Iloop.im * Iloop.im;
        if i2 > 0.1 then begin
          Zloop := cdiv (Vloop, Iloop);
          // start with a very simple rectangular characteristic
          if (Zloop.re >= 0) and (Zloop.im >= 0.0) and (Zloop.re <= Zreach.re) and (Zloop.im <= Zreach.im) then begin
            if not PickedUp then begin
              Targets := TStringList.Create();
              Targets.Sorted := True;
            end;
            if (i = j) then begin
              Targets.Add (Format('G%d', [i]));
            end else begin
              Targets.Add (Format('P%d%d', [i, j]));
            end;
            fault_distance := cabs2(zloop) / cabs2 (zreach);
            if fault_distance < min_distance then min_distance := fault_distance;
            PickedUp := True;
          end;
        end;
      end;
    end;
    if PickedUp then begin
      if DebugTrace then begin
        AppendToEventLog ('Relay.'+Self.Name, 'Picked up',ActorID);
      end;
      if ArmedForReset then begin
        ActiveCircuit[ActorID].ControlQueue.Delete (LastEventHandle,ActorID);
        ArmedForReset := FALSE;
      end;
      if not ArmedForOpen then with ActiveCircuit[ActorID] do begin
        RelayTarget := Format ('21 %.3f pu dist', [min_distance]);
        t_event := Solution.DynaVars.t + Delay_Time + Breaker_time;
        for i := 0 to pred(Targets.Count) do
          RelayTarget := RelayTarget + ' ' + Targets[i];
        LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, t_event, CTRL_OPEN, 0, Self, ActorID);
        ArmedForOpen := TRUE;
        if OperationCount <= NumReclose then begin
          LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, t_event + RecloseIntervals^[OperationCount], CTRL_CLOSE, 0, Self, ActorID);
          ArmedForClose := TRUE;
        end;
      End;
      Targets.Free();
    end else begin  // not picked up; reset if necessary
      if (OperationCount > 1) and (ArmedForReset = FALSE) then begin // this implements the reset, whether picked up or not
        ArmedForReset := TRUE;
        with ActiveCircuit[ActorID] do
          LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self, ActorID);
      end;
      if ArmedForOpen then begin // this implements the drop-out, if picked up
        ArmedForOpen := FALSE;
        ArmedForClose := FALSE;
      End;
    end;
  End;  {With MonitoredElement}
end;

procedure TRelayObj.TD21Logic(ActorID : Integer);
var
  i, j: Integer;
  Vloop, Iloop, Zhsd, Zdir, Uhsd, Uref, Ires, kIres: Complex;
  i2, i2fault, min_distance, fault_distance, Uref2, Uhsd2, t_event, dt: Double;
  Targets: TStringList;
  PickedUp, FaultDetected: Boolean;
  ib, iv, ii: Integer;
begin
  dt := ActiveCircuit[ActorID].Solution.DynaVars.h;
  if dt > 0.0 then begin
    if dt > 1.0 / ActiveCircuit[ActorID].Solution.Frequency then
      DoErrorMsg('Relay: "' + Name + '"',
        'Has type TD21 with time step greater than one cycle.',
        'Reduce time step, or change type to Distance.', 388);
    i := round (1.0 / 60.0 / dt + 0.5);
    if i > td21_pt then begin
      td21_i := 0; // ring buffer index to be incremented before actual use
      td21_pt := i;
      td21_quiet := td21_pt + 1;
      td21_stride := 2 * Nphases;
      ReAllocMem(td21_h, SizeOf(td21_h^[1]) * td21_stride * td21_pt);
      ReAllocMem(td21_dV, SizeOf(td21_dV^[1]) * Nphases);
      ReAllocMem(td21_Uref, SizeOf(td21_Uref^[1]) * Nphases);
      ReAllocMem(td21_dI, SizeOf(td21_dI^[1]) * Nphases);
      if DebugTrace then
        AppendToEventLog ('Relay.'+Self.Name, Format ('TD21 prep %d phases, %.3g dt, %d points, %d elements',
          [NPhases, dt, td21_pt, td21_stride * td21_pt]), ActorID);
    end;
  end;
  If Not LockedOut Then with MonitoredElement Do Begin
    FaultDetected := False;    
    MonitoredElement.GetCurrents(cBuffer, ActorID);

    if Dist_Reverse then
      for I := 1 to MonitoredElement.NPhases do
        cBuffer^[i+CondOffset] := cnegate (cBuffer^[i+CondOffset]);
    i2fault := PhaseTrip * PhaseTrip;
    for I := 1 to Nphases do begin
      i2 := cabs2 (cBuffer^[i+CondOffset]);
      if i2 > i2fault then FaultDetected := True;
    end;
    if DebugTrace then AppendToEventLog ('Relay.'+self.Name, Format ('FaultDetected=%s', [BoolToStr(FaultDetected)]),ActorID);
    MonitoredElement.GetTermVoltages(MonitoredElementTerminal, cvBuffer, ActorID);
    if td21_i < 1 then begin
      if DebugTrace then AppendToEventLog ('Relay.'+self.Name, 'Initialize cqueue', ActorID);
      for i := 1 to td21_pt do begin
        ib := (i - 1) * td21_stride;
        for j := 1 to Nphases do begin
          iv := ib + j;
          td21_h^[iv] := cvBuffer^[j];
          ii := ib + Nphases + j;
          td21_h^[ii] := cBuffer^[j+CondOffset];
        end;
      end;
      td21_i := 1;
    end;
    td21_next := (td21_i Mod td21_pt) + 1;  // this points to the oldest sample, and the next write location
    // calculate the differential currents and voltages
    ib := (td21_next - 1) * td21_stride;
    for j := 1 to Nphases do begin
      iv := ib + j;
      td21_Uref^[j] := td21_h^[iv];
      td21_dV^[j] := csub (cvBuffer^[j], td21_h^[iv]);
      ii := ib + Nphases + j;
      td21_dI^[j] := csub (cBuffer^[j+CondOffset], td21_h^[ii]);
    end;
//    if DebugTrace then begin
//      AppendToEventLog ('Relay.'+self.Name, Format ('Sample len=%d idx=%d next=%d', [td21_pt, td21_i, td21_next]));
//      AppendToEventLog ('Relay.'+self.Name, Format('Vp %10.2f+j%10.2f %10.2f+j%10.2f %10.2f+j%10.2f',
//        [cvBuffer^[1].re, cvBuffer^[1].im, cvBuffer^[2].re, cvBuffer^[2].im, cvBuffer^[3].re, cvBuffer^[3].im]));
//      AppendToEventLog ('Relay.'+self.Name, Format('Ip %10.2f+j%10.2f %10.2f+j%10.2f %10.2f+j%10.2f',
//        [cBuffer^[1 + CondOffset].re, cBuffer^[1 + CondOffset].im,
//         cBuffer^[2 + CondOffset].re, cBuffer^[2 + CondOffset].im,
//         cBuffer^[3 + CondOffset].re, cBuffer^[3 + CondOffset].im]));
//      AppendToEventLog ('Relay.'+self.Name, Format('DV %10.2f+j%10.2f %10.2f+j%10.2f %10.2f+j%10.2f',
//        [td21_dV^[1].re, td21_dV^[1].im, td21_dV^[2].re, td21_dV^[2].im, td21_dV^[3].re, td21_dV^[3].im]));
//      AppendToEventLog ('Relay.'+self.Name, Format('DI %10.2f+j%10.2f %10.2f+j%10.2f %10.2f+j%10.2f',
//        [td21_dI^[1].re, td21_dI^[1].im, td21_dI^[2].re, td21_dI^[2].im, td21_dI^[3].re, td21_dI^[3].im]));
//    end;
    // do the relay processing
    if ActiveCircuit[ ActorID].Solution.DynaVars.IterationFlag < 1 then begin
//      if DebugTrace then AppendToEventLog ('Relay.'+self.Name, 'Advance cqueue write pointer');
      ib := (td21_i - 1) * td21_stride;
      for j := 1 to Nphases do begin
        iv := ib + j;
        td21_h^[iv] := cvBuffer^[j];
        ii := ib + Nphases + j;
        td21_h^[ii] := cBuffer^[j+CondOffset];
      end;
      td21_i := td21_next;
      if td21_quiet > 0 then dec(td21_quiet);
    end;
    if td21_quiet <= 0 then begin  // one cycle since we started, or since the last operation
      PickedUp := False;
      min_distance := 1.0e30;
      Ires := cZERO;
      for i := 1 to MonitoredElement.Nphases do caccum (Ires, td21_dI^[i]);
      kIres := cmul (Dist_K0, Ires);
      for i := 1 to MonitoredElement.NPhases do begin
        for j := i to MonitoredElement.NPhases do begin
          if (i = j) then begin
            Uref := td21_Uref^[i];
            Vloop := td21_dV^[i];
            Iloop := cadd (td21_dI^[i], kIres);
            Zhsd := cmulreal (Dist_Z1, Mground); // not Dist_Z0 because it's included in Dist_K0
          end else begin
            Uref := csub (td21_Uref^[i], td21_Uref^[j]);
            Vloop := csub (td21_dV^[i], td21_dV^[j]);
            Iloop := csub (td21_dI^[i], td21_dI^[j]);
            Zhsd := cmulreal (Dist_Z1, Mphase);
          end;
          i2 := cabs2 (Iloop);
          Uref2 := cabs2 (Uref);
          if FaultDetected and (i2 > 0.1) and (Uref2 > 0.1) then begin
            Zdir := cnegate (cdiv (Vloop, Iloop));
            if DebugTrace then
              AppendToEventLog ('Relay.'+self.Name, Format ('Zhsd[%d,%d]=%.4f+j%.4f, Zdir=%.4f+j%.4f', [i, j, Zhsd.re, Zhsd.im, Zdir.re, Zdir.im]),ActorID);
            if (Zdir.re > 0.0) and (Zdir.im > 0.0) then begin
              Uhsd := csub (cmul (Zhsd, Iloop), Vloop);
              Uhsd2 := cabs2 (Uhsd);
              if DebugTrace then
                AppendToEventLog ('Relay.'+self.Name, Format ('     Uhsd=%.2f, Uref=%.2f', [cabs(Uhsd), cabs(Uref)]),ActorID);
              if Uhsd2 / Uref2 > 1.0 then begin // this loop trips
                if not PickedUp then begin
                  Targets := TStringList.Create();
                  Targets.Sorted := True;
                end;
                if (i = j) then begin
                  Targets.Add (Format('G%d', [i]));
                end else begin
                  Targets.Add (Format('P%d%d', [i, j]));
                end;
                fault_distance := 1.0 / sqrt(Uhsd2 / Uref2);
                if fault_distance < min_distance then min_distance := fault_distance;
                PickedUp := True;
              end;
            end;
          end;
        end;
      end;
      if PickedUp then begin
        if DebugTrace then begin
          AppendToEventLog ('Relay.'+Self.Name, 'Picked up', ActorID);
        end;
        if ArmedForReset then begin
          ActiveCircuit[ ActorID].ControlQueue.Delete (LastEventHandle, ActorID);
          ArmedForReset := FALSE;
          if DebugTrace then AppendToEventLog ('Relay.'+self.Name, 'Dropping last event.', ActorID);
        end;
        if not ArmedForOpen then with ActiveCircuit[ ActorID] do begin
          RelayTarget := Format ('TD21 %.3f pu dist', [min_distance]);
          t_event := Solution.DynaVars.t + Delay_Time + Breaker_time;
          for i := 0 to pred(Targets.Count) do
            RelayTarget := RelayTarget + ' ' + Targets[i];
          LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, t_event, CTRL_OPEN, 0, Self, ActorID);
          if DebugTrace then AppendToEventLog ('Relay.'+self.Name, Format ('Pushing trip event for %.3f', [t_event]), ActorID);
          ArmedForOpen := TRUE;
          if OperationCount <= NumReclose then begin
            LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, t_event + RecloseIntervals^[OperationCount], CTRL_CLOSE, 0, Self, ActorID);
            if DebugTrace then AppendToEventLog ('Relay.'+self.Name, Format ('Pushing reclose event for %.3f', [t_event + RecloseIntervals^[OperationCount]]), ActorID);
            ArmedForClose := TRUE;
          end;
        End;
        Targets.Free();
      end;
      if not FaultDetected then begin  // not picked up; reset if necessary
        if (OperationCount > 1) and (ArmedForReset = FALSE) then begin // this implements the reset, whether picked up or not
          ArmedForReset := TRUE;
          with ActiveCircuit[ ActorID] do
            LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self, ActorID);
            if DebugTrace then AppendToEventLog ('Relay.'+self.Name, Format ('Pushing reset event for %.3f', [ActiveCircuit[ActorID].Solution.DynaVars.t + ResetTime]), ActorID);
        end;
        if ArmedForOpen then begin
          td21_quiet := td21_pt + 1;
          ArmedForOpen := FALSE;
          ArmedForClose := FALSE;
          if DebugTrace then
            AppendToEventLog ('Relay.'+self.Name, Format ('Dropping out at %.3f', [ActiveCircuit[ActorID].Solution.DynaVars.t]),ActorID);
        End;
      end;
    end; { td21_quiet}
  End;  {With MonitoredElement}
end;


procedure TRelayObj.RevPowerLogic(ActorID : Integer);

VAR

   S:Complex ;

begin

 WITH   MonitoredElement Do
   Begin
      //----MonitoredElement.ActiveTerminalIdx := MonitoredElementTerminal;
      S := MonitoredElement.Power[MonitoredElementTerminal,ActorID];
      IF S.re < 0.0  THEN
        Begin
          IF Abs(S.Re) > PhaseInst * 1000.0 THEN
            Begin
              IF Not ArmedForOpen THEN  // push the trip operation and arm to trip
               WITH ActiveCircuit[ActorID]  Do
                Begin
                 RelayTarget := 'Rev P';
                 LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t +Delay_Time +  Breaker_time, CTRL_OPEN, 0, Self, ActorID);
                 OperationCount := NumReclose + 1;  // force a lockout
                 ArmedForOpen := TRUE;
                End
            End
          ELSE
              IF ArmedForOpen  THEN    // We became unarmed, so reset and disarm
               WITH ActiveCircuit[ActorID] Do
                Begin
                 LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self, ActorID);
                 ArmedForOpen := FALSE;
                End;
        End;
   End;  {With MonitoredElement}
end;

procedure TRelayObj.VoltageLogic(ActorID : Integer);

VAR
   i           :Integer;
   VMax,
   Vmin,
   Vmag,
   OVTime,
   UVTime,
   TripTime :Double;

begin

 If Not LockedOut Then
 WITH   MonitoredElement Do
   Begin
   {**** Fix so that fastest trip time applies ****}
     MonitoredElement.GetTermVoltages(MonitoredElementTerminal, cBuffer, ActorID);

     Vmin := 1.0E50;
     Vmax := 0.0;
     FOR i := 1 to MonitoredElement.NPhases Do
       Begin
          Vmag := Cabs(cBuffer^[i]);
          If Vmag > Vmax Then Vmax := Vmag;
          If Vmag < Vmin then Vmin := Vmag;
       End;

     {Convert to Per Unit}
     Vmax := Vmax / Vbase;
     Vmin := Vmin / Vbase;

     IF FPresentState = CTRL_CLOSE THEN
       Begin
           TripTime := -1.0;
           OVTime := -1.0;
           UVTime := -1.0;



           {Check OverVoltage Trip, if any}
           IF OVCurve <> NIL THEN OVTime := OVCurve.GetOVtime(Vmax);

           IF OVTime > 0.0 THEN Begin
             TripTime := OVTime;
           End;

           // If OVTime > 0 then we have a OV trip

           {Check UV Trip, if any}
           IF   UVCurve <> NIL  THEN
             Begin
                UVTime := UVCurve.GetUVtime(Vmin);
             End;

         // If UVTime > 0 then we have a UV trip

           IF   UVTime > 0.0  THEN
             Begin
                IF   TripTime > 0.0
                THEN Begin
                  TripTime := Min(TripTime, UVTime)   // Min of UV or OV time
                 End
                ELSE
                 Begin
                   TripTime := UVTime;
                 End;
             End;

           IF   TripTime > 0.0 THEN
             WITH ActiveCircuit[ActorID] Do
             Begin

              If  ArmedForOpen and ((Solution.DynaVars.t + TripTime + Breaker_time) < NextTripTime) Then
                Begin
                  ControlQueue.Delete (LastEventHandle, ActorID);  // Delete last event from Queue
                  ArmedForOpen := False;  // force it to go through next IF
                End;

              IF   Not ArmedForOpen THEN
                Begin  // Then arm for an open operation
                     If TripTime = UVTime Then Begin
                        If TripTime = OVTime Then RelayTarget := 'UV + OV'
                        Else  RelayTarget := 'UV' ;
                      End
                     Else Relaytarget := 'OV';
                     
                     NextTripTime :=  Solution.DynaVars.t + TripTime + Breaker_time;
                     LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, NextTripTime, CTRL_OPEN, 0, Self, ActorID);
                     ArmedforOpen := TRUE;
                End;
             End
           ELSE
             Begin
               IF ArmedForOpen THEN
               WITH ActiveCircuit[ActorID] Do    // If voltage dropped below pickup, disarm trip and set for reset
                 Begin
                    ControlQueue.Delete (LastEventHandle, ActorID);  // Delete last event from Queue
                    NextTripTime := -1.0;
                    LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self, ActorID);
                    ArmedForOpen := FALSE;
                 End;
             End;
     End  {IF PresentState=CLOSE}
   ELSE
     Begin     {Present state is Open, Check for Voltage and then set reclose Interval}
        IF (OperationCount <= NumReclose) Then
          IF Not ArmedForClose THEN
            Begin
              IF (Vmax > 0.9) THEN
              WITH ActiveCircuit[ActorID] Do  // OK if voltage > 90%
                Begin
                     LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t +  RecloseIntervals^[OperationCount], CTRL_CLOSE, 0, Self, ActorID);
                     ArmedForClose := TRUE;
                End;
            End
          ELSE   {Armed, but check to see if voltage dropped before it reclosed and cancel action}
             IF Vmax <0.9 THEN ArmedForClose := False;

      End;


   End;  {With MonitoredElement}

end;

procedure TRelayObj.NegSeq47Logic(ActorID : Integer);

{Neg Seq voltage Relay}

VAR
    NegSeqVoltageMag : Double;
    V012 :Array[1..3] of Complex;

begin

 WITH   MonitoredElement Do
   Begin
      MonitoredElement.GetTermVoltages (MonitoredElementTerminal, cBuffer, ActorID);
      Phase2SymComp(cBuffer, @V012); // Phase to symmetrical components
      NegSeqVoltageMag :=  Cabs(V012[3]);
      IF NegSeqVoltageMag >=  PickupVolts47 THEN
        Begin
              IF Not ArmedForOpen THEN  // push the trip operation and arm to trip
               WITH ActiveCircuit[ActorID]  Do
                Begin
                 RelayTarget := '-Seq V';
                 LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + Delay_Time + Breaker_time, CTRL_OPEN, 0, Self, ActorID);
                 OperationCount := NumReclose + 1;  // force a lockout
                 ArmedForOpen := TRUE;
                End
        End
      ELSE
        Begin  {Less Than pickup value: reset if armed}
              IF ArmedForOpen  THEN    // We became unarmed, so reset and disarm
               WITH ActiveCircuit[ActorID] Do
                Begin
                 LastEventHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self, ActorID);
                 ArmedForOpen := FALSE;
                End;
        End;
   End;  {With MonitoredElement}


end;

INITIALIZATION

end.
