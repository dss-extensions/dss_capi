unit Recloser;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
    Created 11-1-00 from Relay Control


}
{
  A Recloser is a control element that is connected to a terminal of a
  circuit element and controls the switches in the same or another terminal.

  The control is usually placed in the
  terminal of a line or transformer, but it could be any element

  CktElement to be controlled must already exist.

  7-18-2002  Fixed typos in help
  5-1-2006  Added Time Delays to be compatible with relays

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
    TRecloser = class(TControlClass)
    PRIVATE

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const RecloserName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TRecloserObj = class(TControlElem)
    PRIVATE

        PhaseDelayed,
        GroundDelayed,
        PhaseFast,
        GroundFast: TTCC_CurveObj;


        ResetTime,
        DelayTime,
        TDGrDelayed,
        TDPhDelayed,
        TDGrFast,
        TDPhFast: Double;

        MonitoredElement: TDSSCktElement;

        PresentState: EControlAction;

        OperationCount: Integer;

        LockedOut,
        ArmedForClose, ArmedForOpen, GroundTarget, PhaseTarget: Boolean;

        CondOffset: Integer; // Offset for monitored terminal

        cBuffer: pComplexArray;    // Complexarray buffer

        procedure InterpretRecloserAction(const Action: String);

    PUBLIC

        RecloseIntervals: pdoubleArray;
        NumFast,
        NumReclose: Integer;
        MonitoredElementName: String;
        MonitoredElementTerminal: Integer;
        PhaseTrip,
        GroundTrip,
        PhaseInst,
        GroundInst: Double;


        constructor Create(ParClass: TDSSClass; const RecloserName: String);
        destructor Destroy; OVERRIDE;

        procedure MakePosSequence(ActorID: Integer); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData(ActorID: Integer); OVERRIDE;
        procedure CalcYPrim(ActorID: Integer); OVERRIDE;    // Always Zero for a Recloser

        procedure Sample(ActorID: Integer); OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer; ActorID: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state


        procedure GetCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE; // Get present value of terminal Curr
        procedure GetInjCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;   // Returns Injextion currents

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

    end;


var
    ActiveRecloserObj: TRecloserObj;
    RecloserClass: TRecloser;


{--------------------------------------------------------------------------}
implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    Sysutils,
    uCmatrix,
    MathUtil;

const

    NumPropsThisClass = 22;

    CURRENT = 0;  {Default}
    VOLTAGE = 1;
    REVPOWER = 3;

var
    TCC_CurveClass: TDSSClass;

{General Module Function}

function GetTccCurve(const CurveName: String): TTCC_CurveObj;

begin

    Result := TCC_CurveClass.Find(CurveName);

    if Result = NIL then
        DoSimpleMsg('TCC Curve object: "' + CurveName + '" not found.', 388);

end;


{--------------------------------------------------------------------------}
constructor TRecloser.Create;  // Creates superstructure for all Recloser objects
begin
    inherited Create;

    Class_name := 'Recloser';
    DSSClassType := DSSClassType + RECLOSER_CONTROL;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;

    TCC_CurveClass := GetDSSClassPtr('TCC_Curve');
    RecloserClass := Self;
end;

{--------------------------------------------------------------------------}
destructor TRecloser.Destroy;

begin
    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TRecloser.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


     // Define Property names

    PropertyName[1] := 'MonitoredObj';
    PropertyName[2] := 'MonitoredTerm';
    PropertyName[3] := 'SwitchedObj';
    PropertyName[4] := 'SwitchedTerm';
    PropertyName[5] := 'NumFast';
    PropertyName[6] := 'PhaseFast';
    PropertyName[7] := 'PhaseDelayed';
    PropertyName[8] := 'GroundFast';
    PropertyName[9] := 'GroundDelayed';
    PropertyName[10] := 'PhaseTrip';
    PropertyName[11] := 'GroundTrip';
    PropertyName[12] := 'PhaseInst';
    PropertyName[13] := 'GroundInst';
    PropertyName[14] := 'Reset';
    PropertyName[15] := 'Shots';
    PropertyName[16] := 'RecloseIntervals';
    PropertyName[17] := 'Delay';
    PropertyName[18] := 'Action';
    PropertyName[19] := 'TDPhFast';
    PropertyName[20] := 'TDGrFast';
    PropertyName[21] := 'TDPhDelayed';
    PropertyName[22] := 'TDGrDelayed';

    PropertyHelp[1] := 'Full object name of the circuit element, typically a line, transformer, load, or generator, ' +
        'to which the Recloser''s PT and/or CT are connected.' +
        ' This is the "monitored" element. ' +
        'There is no default; must be specified.';
    PropertyHelp[2] := 'Number of the terminal of the circuit element to which the Recloser is connected. ' +
        '1 or 2, typically.  Default is 1.';
    PropertyHelp[3] := 'Name of circuit element switch that the Recloser controls. ' +
        'Specify the full object name.' +
        'Defaults to the same as the Monitored element. ' +
        'This is the "controlled" element.';
    PropertyHelp[4] := 'Number of the terminal of the controlled element in which the switch is controlled by the Recloser. ' +
        '1 or 2, typically.  Default is 1.';
    PropertyHelp[5] := 'Number of Fast (fuse saving) operations.  Default is 1. (See "Shots")';
    PropertyHelp[6] := 'Name of the TCC Curve object that determines the Phase Fast trip.  Must have been previously defined as a TCC_Curve object.' +
        ' Default is "A". ' +
        'Multiplying the current values in the curve by the "phasetrip" value gives the actual current.';
    PropertyHelp[7] := 'Name of the TCC Curve object that determines the Phase Delayed trip.  Must have been previously defined as a TCC_Curve object.' +
        ' Default is "D".' +
        'Multiplying the current values in the curve by the "phasetrip" value gives the actual current.';
    PropertyHelp[8] := 'Name of the TCC Curve object that determines the Ground Fast trip.  Must have been previously defined as a TCC_Curve object.' +
        ' Default is none (ignored). ' +
        'Multiplying the current values in the curve by the "groundtrip" value gives the actual current.';
    PropertyHelp[9] := 'Name of the TCC Curve object that determines the Ground Delayed trip.  Must have been previously defined as a TCC_Curve object.' +
        ' Default is none (ignored).' +
        'Multiplying the current values in the curve by the "groundtrip" value gives the actual current.';
    PropertyHelp[10] := 'Multiplier or actual phase amps for the phase TCC curve.  Defaults to 1.0.';
    PropertyHelp[11] := 'Multiplier or actual ground amps (3I0) for the ground TCC curve.  Defaults to 1.0.';
    PropertyHelp[12] := 'Actual amps for instantaneous phase trip which is assumed to happen in 0.01 sec + Delay Time. Default is 0.0, which signifies no inst trip. ';
    PropertyHelp[13] := 'Actual amps for instantaneous ground trip which is assumed to happen in 0.01 sec + Delay Time.Default is 0.0, which signifies no inst trip.';
    PropertyHelp[14] := 'Reset time in sec for Recloser.  Default is 15. ';
    PropertyHelp[15] := 'Total Number of fast and delayed shots to lockout.  Default is 4. This is one more than the number of reclose intervals.';
    PropertyHelp[16] := 'Array of reclose intervals.  Default for Recloser is (0.5, 2.0, 2.0) seconds. ' +
        'A locked out Recloser must be closed manually (action=close).';
    PropertyHelp[17] := 'Fixed delay time (sec) added to Recloser trip time. Default is 0.0. Used to represent breaker time or any other delay.';
    PropertyHelp[18] := '{Trip/Open | Close}  Action that overrides the Recloser control. Simulates manual control on recloser ' +
        '"Trip" or "Open" causes the controlled element to open and lock out. ' +
        '"Close" causes the controlled element to close and the Recloser to reset to its first operation.';
    PropertyHelp[19] := 'Time dial for Phase Fast trip curve. Multiplier on time axis of specified curve. Default=1.0.';
    PropertyHelp[20] := 'Time dial for Ground Fast trip curve. Multiplier on time axis of specified curve. Default=1.0.';
    PropertyHelp[21] := 'Time dial for Phase Delayed trip curve. Multiplier on time axis of specified curve. Default=1.0.';
    PropertyHelp[22] := 'Time dial for Ground Delayed trip curve. Multiplier on time axis of specified curve. Default=1.0.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

{--------------------------------------------------------------------------}
function TRecloser.NewObject(const ObjName: String): Integer;
begin
    // Make a new Recloser and add it to Recloser class list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveCktElement := TRecloserObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;

{--------------------------------------------------------------------------}


{--------------------------------------------------------------------------}
function TRecloser.Edit(ActorID: Integer): Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

begin

  // continue parsing WITH contents of Parser
    ActiveRecloserObj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActiveRecloserObj;

    Result := 0;

    with ActiveRecloserObj do
    begin

        ParamPointer := 0;
        ParamName := Parser[ActorID].NextParam;
        Param := Parser[ActorID].StrValue;
        while Length(Param) > 0 do
        begin
            if Length(ParamName) = 0 then
                Inc(ParamPointer)
            else
                ParamPointer := CommandList.GetCommand(ParamName);

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[ParamPointer] := Param;

            case ParamPointer of
                0:
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 390);
                1:
                    MonitoredElementName := lowercase(param);
                2:
                    MonitoredElementTerminal := Parser[ActorID].IntValue;
                3:
                    ElementName := lowercase(param);
                4:
                    ElementTerminal := Parser[ActorID].IntValue;
                5:
                    NumFast := Parser[ActorID].Intvalue;
                6:
                    PhaseFast := GetTccCurve(Param);
                7:
                    PhaseDelayed := GetTCCCurve(Param);
                8:
                    GroundFast := GetTccCurve(Param);
                9:
                    GroundDelayed := GetTCCCurve(Param);
                10:
                    PhaseTrip := Parser[ActorID].Dblvalue;
                11:
                    GroundTrip := Parser[ActorID].Dblvalue;
                12:
                    PhaseInst := Parser[ActorID].Dblvalue;
                13:
                    GroundInst := Parser[ActorID].Dblvalue;
                14:
                    Resettime := Parser[ActorID].Dblvalue;
                15:
                    NumReclose := Parser[ActorID].Intvalue - 1;   // one less than number of shots
                16:
                    NumReclose := Parser[ActorID].ParseAsVector(4, RecloseIntervals);   // max of 4 allowed
                17:
                    DelayTime := Parser[ActorID].DblValue;
                18:
                    InterpretRecloserAction(Param);
                19:
                    TDPhFast := Parser[ActorID].DblValue;
                20:
                    TDGrFast := Parser[ActorID].DblValue;
                21:
                    TDPhDelayed := Parser[ActorID].DblValue;
                22:
                    TDGrDelayed := Parser[ActorID].DblValue;

            else
           // Inherited parameters
                ClassEdit(ActiveRecloserObj, ParamPointer - NumPropsthisClass)
            end;

            case ParamPointer of
              {Default the controlled element to the monitored element}
                1:
                    ElementName := MonitoredElementName;
                2:
                    ElementTerminal := MonitoredElementTerminal;
            end;

            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;

        RecalcElementData(ActorID);
    end;

end;


{--------------------------------------------------------------------------}
function TRecloser.MakeLike(const RecloserName: String): Integer;
var
    OtherRecloser: TRecloserObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this Recloser name in the present collection}
    OtherRecloser := Find(RecloserName);
    if OtherRecloser <> NIL then
        with ActiveRecloserObj do
        begin

            NPhases := OtherRecloser.Fnphases;
            NConds := OtherRecloser.Fnconds; // Force Reallocation of terminal stuff

            ElementName := OtherRecloser.ElementName;
            ElementTerminal := OtherRecloser.ElementTerminal;
            ControlledElement := OtherRecloser.ControlledElement;  // Pointer to target circuit element

            MonitoredElement := OtherRecloser.MonitoredElement;  // Pointer to target circuit element
            MonitoredElementName := OtherRecloser.MonitoredElementName;  // Pointer to target circuit element
            MonitoredElementTerminal := OtherRecloser.MonitoredElementTerminal;  // Pointer to target circuit element

            PhaseDelayed := OtherRecloser.PhaseDelayed;
            GroundDelayed := OtherRecloser.GroundDelayed;
            PhaseFast := OtherRecloser.PhaseFast;
            GroundFast := OtherRecloser.GroundFast;
            PhaseTrip := OtherRecloser.PhaseTrip;
            GroundTrip := OtherRecloser.GroundTrip;
            PhaseInst := OtherRecloser.PhaseInst;
            GroundInst := OtherRecloser.GroundInst;
            Resettime := OtherRecloser.Resettime;
            NumReclose := OtherRecloser.NumReclose;
            NumFast := OtherRecloser.NumFast;

            Reallocmem(RecloseIntervals, SizeOf(RecloseIntervals^[1]) * 4);      // Always make a max of 4
            for i := 1 to NumReclose do
                RecloseIntervals^[i] := OtherRecloser.RecloseIntervals^[i];

            LockedOut := OtherRecloser.LockedOut;

            PresentState := OtherRecloser.PresentState;
            CondOffset := OtherRecloser.CondOffset;


            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherRecloser.PropertyValue[i];

        end
    else
        DoSimpleMsg('Error in Recloser MakeLike: "' + RecloserName + '" Not Found.', 391);

end;


{==========================================================================}
{                    TRecloserObj                                           }
{==========================================================================}


{--------------------------------------------------------------------------}
constructor TRecloserObj.Create(ParClass: TDSSClass; const RecloserName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(RecloserName);
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

    PhaseFast := GetTccCurve('a');
    PhaseDelayed := GetTccCurve('d');
    GroundFast := NIL;
    GroundDelayed := NIL;

    PhaseTrip := 1.0;
    GroundTrip := 1.0;
    PhaseInst := 0.0;
    GroundInst := 0.0;

    TDGrDelayed := 1.0;
    TDPhDelayed := 1.0;
    TDGrFast := 1.0;
    TDPhFast := 1.0;

    Resettime := 15.0;
    NumReclose := 3;
    NumFast := 1;

    RecloseIntervals := NIL;
    Reallocmem(RecloseIntervals, SizeOf(RecloseIntervals^[1]) * 4); // fixed allocation of 4
    RecloseIntervals^[1] := 0.5;
    RecloseIntervals^[2] := 2.0;
    RecloseIntervals^[3] := 2.0;


    PresentState := CTRL_CLOSE;


    Operationcount := 1;
    LockedOut := FALSE;
    ArmedForOpen := FALSE;
    ArmedForClose := FALSE;
    GroundTarget := FALSE;
    PhaseTarget := FALSE;


    cBuffer := NIL; // Complex buffer

    DSSObjType := ParClass.DSSClassType; //cap_CONTROL;

    InitPropertyValues(0);


   //  RecalcElementData;

end;

destructor TRecloserObj.Destroy;
begin
    MonitoredElementName := '';
    ReallocMem(RecloseIntervals, 0);
    ReallocMem(cBuffer, 0);
    inherited Destroy;
end;

{--------------------------------------------------------------------------}
procedure TRecloserObj.RecalcElementData(ActorID: Integer);

var
    DevIndex: Integer;

begin

    Devindex := GetCktElementIndex(MonitoredElementName); // Global function
    if DevIndex > 0 then
    begin

        MonitoredElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
        Nphases := MonitoredElement.NPhases;       // Force number of phases to be same
        if MonitoredElementTerminal > MonitoredElement.Nterms then
        begin
            DoErrorMsg('Recloser: "' + Name + '"',
                'Terminal no. "' + '" does not exist.',
                'Re-specify terminal no.', 392);
        end
        else
        begin
               // Sets name of i-th terminal's connected bus in Recloser's buslist
            Setbus(1, MonitoredElement.GetBus(MonitoredElementTerminal));
               // Allocate a buffer bigenough to hold everything from the monitored element
            ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder);
            CondOffset := (MonitoredElementTerminal - 1) * MonitoredElement.NConds; // for speedy sampling
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

        ControlledElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Make the 1 st terminal active

             // If the recloser becomes disabled, leave at False
        if Enabled then
        begin
            ControlledElement.HasOCPDevice := TRUE;  // For Reliability calcs
            ControlledElement.HasAutoOCPDevice := TRUE;  // For Reliability calcs
        end;

        if ControlledElement.Closed[0, ActorID]      // Check state of phases of active terminal
        then
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
        DoErrorMsg('Recloser: "' + Self.Name + '"', 'CktElement Element "' + ElementName + '" Not Found.',
            ' Element must be defined previously.', 393);
    end;
end;

procedure TRecloserObj.MakePosSequence(ActorID: Integer);
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
    inherited;
end;

{--------------------------------------------------------------------------}
procedure TRecloserObj.CalcYPrim(ActorID: Integer);
begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
end;

{--------------------------------------------------------------------------}
procedure TRecloserObj.GetCurrents(Curr: pComplexArray; ActorID: Integer);
var
    i: Integer;
begin

    for i := 1 to Fnconds do
        Curr^[i] := CZERO;

end;

{--------------------------------------------------------------------------}

procedure TRecloserObj.GetInjCurrents(Curr: pComplexArray; ActorID: Integer);
var
    i: Integer;
begin
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

{--------------------------------------------------------------------------}
procedure TRecloserObj.DoPendingAction(const Code, ProxyHdl: Integer; ActorID: Integer);


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
                            ControlledElement.Closed[0, ActorID] := FALSE;   // Open all phases of active terminal
                            if OperationCount > NumReclose then
                            begin
                                LockedOut := TRUE;
                                AppendtoEventLog('Recloser.' + Self.Name, 'Opened, Locked Out', ActorID);
                            end
                            else
                            begin
                                if OperationCount > NumFast then
                                    AppendtoEventLog('Recloser.' + Self.Name, 'Opened, Delayed', ActorID)
                                else
                                    AppendtoEventLog('Recloser.' + Self.Name, 'Opened, Fast', ActorID);
                            end;
                            if PhaseTarget then
                                AppendtoEventLog(' ', 'Phase Target', ActorID);
                            if GroundTarget then
                                AppendtoEventLog(' ', 'Ground Target', ActorID);
                            ArmedForOpen := FALSE;
                        end;
                else {nada}
                end;
            Integer(CTRL_CLOSE):
                case PresentState of
                    CTRL_OPEN:
                        if ArmedForClose and not LockedOut then
                        begin
                            ControlledElement.Closed[0, ActorID] := TRUE;    // Close all phases of active terminal
                            Inc(OperationCount);
                            AppendtoEventLog('Recloser.' + Self.Name, 'Closed', ActorID);
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


procedure TRecloserObj.InterpretRecloserAction(const Action: String);
begin

    if ControlledElement <> NIL then
    begin
        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal
        case LowerCase(Action)[1] of

            'o', 't':
            begin
                ControlledElement.Closed[0, ActiveActor] := FALSE;   // Open all phases of active terminal
                LockedOut := TRUE;
                OperationCount := NumReclose + 1;
            end;
            'c':
            begin
                ControlledElement.Closed[0, ActiveActor] := TRUE;    // Close all phases of active terminal
                LockedOut := FALSE;
                OperationCount := 1;
            end;
        end;
    end;

end;

{--------------------------------------------------------------------------}
procedure TRecloserObj.Sample(ActorID: Integer);

var
    i: Integer;
    cmag: Double;
    Csum: Complex;

    GroundCurve, PhaseCurve: TTCC_CurveObj;
    Groundtime, PhaseTime, TripTime, TimeTest: Double;
    TDPhase, TDGround: Double;

begin


    ControlledElement.ActiveTerminalIdx := ElementTerminal;

    if ControlledElement.Closed[0, ActorID]      // Check state of phases of active terminal
    then
        PresentState := CTRL_CLOSE
    else
        PresentState := CTRL_OPEN;


    with  MonitoredElement do
    begin

        if OperationCount > NumFast then
        begin
            GroundCurve := GroundDelayed;
            PhaseCurve := PhaseDelayed;
            TDGround := TDGrDelayed;
            TDPhase := TDPhDelayed;
        end
        else
        begin
            GroundCurve := GroundFast;
            PhaseCurve := PhaseFast;
            TDGround := TDGrFast;
            TDPhase := TDPhFast;
        end;

        if PresentState = CTRL_CLOSE then
        begin
            TripTime := -1.0;
            GroundTime := -1.0;
            PhaseTime := -1.0;  {No trip}

               // Check largest Current of all phases of monitored element
            MonitoredElement.GetCurrents(cBuffer, ActorID);

               {Check Ground Trip, if any}
            if GroundCurve <> NIL then
            begin
                Csum := CZERO;
                for i := (1 + CondOffset) to (Fnphases + CondOffset) do
                    caccum(Csum, cBuffer^[i]);
                Cmag := Cabs(Csum);
                if (GroundInst > 0.0) and (Cmag >= GroundInst) and (OperationCount = 1) then
                    GroundTime := 0.01 + DelayTime      // Inst trip on first operation
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

            if PhaseCurve <> NIL then
            begin
                for i := (1 + CondOffset) to (Fnphases + CondOffset) do
                begin
                    Cmag := Cabs(cBuffer^[i]);


                    if (PhaseInst > 0.0) and (Cmag >= PhaseInst) and (OperationCount = 1) then
                    begin
                        PhaseTime := 0.01 + DelayTime;  // Inst trip on first operation
                        Break;  {FOR - if Inst, no sense checking other phases}
                    end
                    else
                    begin
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
                    with ActiveCircuit[ActorID] do   // Then arm for an open operation
                    begin
                        ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + Delaytime, CTRL_OPEN, 0, Self, ActorID);
                        if OperationCount <= NumReclose then
                            ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + DelayTime + RecloseIntervals^[OperationCount], CTRL_CLOSE, 0, Self, ActorID);
                        ArmedForOpen := TRUE;
                        ArmedForClose := TRUE;
                    end;
            end
            else
            begin
                if ArmedForOpen then
                    with ActiveCircuit[ActorID] do    // If current dropped below pickup, disarm trip and set for reset
                    begin
                        ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self, ActorID);
                        ArmedForOpen := FALSE;
                        ArmedForClose := FALSE;
                        GroundTarget := FALSE;
                        PhaseTarget := FALSE;
                    end;
            end;
        end;  {IF PresentState=CLOSE}
    end; {With}
end;


{--------------------------------------------------------------------------}
procedure TRecloserObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
        end;

    if Complete then
    begin
        Writeln(F);
    end;

end;

function TRecloserObj.GetPropertyValue(Index: Integer): String;
var
    i: Integer;
begin
    Result := '';
    case Index of
        15:
            Result := Format('%d', [NumReclose + 1]);
        16:
        begin
            Result := '(';
            for i := 1 to NumReclose do
                Result := Result + Format('%-g, ', [RecloseIntervals^[i]]);
            Result := Result + ')';
        end;
    else
        Result := inherited GetPropertyValue(index);
    end;
end;


procedure TRecloserObj.Reset;
begin

    PresentState := CTRL_CLOSE;
    Operationcount := 1;
    LockedOut := FALSE;
    ArmedForOpen := FALSE;
    ArmedForClose := FALSE;
    GroundTarget := FALSE;
    PhaseTarget := FALSE;

    if ControlledElement <> NIL then
    begin
        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal
        ControlledElement.Closed[0, ActiveActor] := TRUE;             // Close all phases of active terminal
    end;


end;

procedure TRecloserObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := ''; //'element';
    PropertyValue[2] := '1'; //'terminal';
    PropertyValue[3] := '';
    PropertyValue[4] := '1'; //'terminal';
    PropertyValue[5] := IntToStr(NumFast);
    PropertyValue[6] := '';
    PropertyValue[7] := '';
    PropertyValue[8] := '';
    PropertyValue[9] := '';
    PropertyValue[10] := '1.0';
    PropertyValue[11] := '1.0';
    PropertyValue[12] := '0';
    PropertyValue[13] := '0';
    PropertyValue[14] := '15';
    PropertyValue[15] := '4';
    PropertyValue[16] := '(0.5, 2.0, 2.0)';
    PropertyValue[17] := '0.0';
    PropertyValue[18] := '';
    PropertyValue[19] := '1.0';
    PropertyValue[20] := '1.0';
    PropertyValue[21] := '1.0';
    PropertyValue[22] := '1.0';

    inherited  InitPropertyValues(NumPropsThisClass);

end;

initialization

end.
