unit Fuse;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
    Created 11-1-00 from Recloser Control
}
{
  A Fuse is a control element that is connected to a terminal of a
  circuit element and controls the switches in the same or another terminal.

  The control is usually placed in the
  terminal of a line or transformer, but it could be any element

  CktElement to be controlled must already exist.

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

const
    FUSEMAXDIM = 6;

type

    pStateArray = ^StateArray;
    StateArray = array[1..FUSEMAXDIM] of EControlAction;  // 0 = open 1 = close

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TFuse = class(TControlClass)
    PRIVATE

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const FuseName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TFuseObj = class(TControlElem)
    PRIVATE

        MonitoredElement: TDSSCktElement;

        hAction: array[1..FUSEMAXDIM] of Integer;         // handle to control queue actions

        FPresentState: pStateArray;
        FNormalState: pStateArray;

        ReadyToBlow: array[1..FUSEMAXDIM] of Boolean;

        CondOffset: Integer; // Offset for monitored terminal
        cBuffer: pComplexArray;    // Complexarray buffer

        NormalStateSet: Boolean;

        procedure InterpretFuseState(ActorID: Integer; const param: String; const property_name: String);
        function get_States(Idx: Integer): EControlAction;
        procedure set_States(Idx: Integer; const Value: EControlAction);
        function get_NormalStates(Idx: Integer): EControlAction;
        procedure set_NormalStates(Idx: Integer; const Value: EControlAction);


    PUBLIC

        FuseCurve: TTCC_CurveObj;
        RatedCurrent: Double;
        DelayTime: Double;

        MonitoredElementName: String;
        MonitoredElementTerminal: Integer;

        constructor Create(ParClass: TDSSClass; const FuseName: String);
        destructor Destroy; OVERRIDE;

        procedure RecalcElementData(ActorID: Integer); OVERRIDE;
        procedure CalcYPrim(ActorID: Integer); OVERRIDE;    // Always Zero for a Fuse

        procedure Sample(ActorID: Integer); OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Phs, ProxyHdl: Integer; ActorID: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset(ActorID: Integer); OVERRIDE;  // Reset to initial defined state

        procedure GetCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE; // Get present value of terminal Curr
        procedure GetInjCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;   // Returns Injextion currents

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

        property States[Idx: Integer]: EControlAction READ get_States WRITE set_States;
        property NormalStates[Idx: Integer]: EControlAction READ get_NormalStates WRITE set_NormalStates;

    end;

var
    ActiveFuseObj: TFuseObj;
    FuseClass: TFuse;

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

    NumPropsThisClass = 10;

var
    TCC_CurveClass: TDSSClass;

{General Module Function}

function GetTccCurve(const CurveName: String): TTCC_CurveObj;

begin

    Result := TCC_CurveClass.Find(CurveName);

    if Result = NIL then
        DoSimpleMsg('TCC Curve object: "' + CurveName + '" not found.', 401);

end;


{--------------------------------------------------------------------------}
constructor TFuse.Create;  // Creates superstructure for all Fuse objects
begin
    inherited Create;

    Class_name := 'Fuse';
    DSSClassType := DSSClassType + FUSE_CONTROL;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;

    TCC_CurveClass := GetDSSClassPtr('TCC_Curve');
    FuseClass := Self;
end;

{--------------------------------------------------------------------------}
destructor TFuse.Destroy;

begin
    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TFuse.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


     // Define Property names

    PropertyName[1] := 'MonitoredObj';
    PropertyName[2] := 'MonitoredTerm';
    PropertyName[3] := 'SwitchedObj';
    PropertyName[4] := 'SwitchedTerm';
    PropertyName[5] := 'FuseCurve';
    PropertyName[6] := 'RatedCurrent';
    PropertyName[7] := 'Delay';
    PropertyName[8] := 'Action';
    PropertyName[9] := 'Normal';
    PropertyName[10] := 'State';

    PropertyHelp[1] := 'Full object name of the circuit element, typically a line, transformer, load, or generator, ' +
        'to which the Fuse is connected.' +
        ' This is the "monitored" element. ' +
        'There is no default; must be specified.';
    PropertyHelp[2] := 'Number of the terminal of the circuit element to which the Fuse is connected. ' +
        '1 or 2, typically.  Default is 1.';
    PropertyHelp[3] := 'Name of circuit element switch that the Fuse controls. ' +
        'Specify the full object name.' +
        'Defaults to the same as the Monitored element. ' +
        'This is the "controlled" element.';
    PropertyHelp[4] := 'Number of the terminal of the controlled element in which the switch is controlled by the Fuse. ' +
        '1 or 2, typically.  Default is 1.  Assumes all phases of the element have a fuse of this type.';
    PropertyHelp[5] := 'Name of the TCC Curve object that determines the fuse blowing.  Must have been previously defined as a TCC_Curve object.' +
        ' Default is "Tlink". ' +
        'Multiplying the current values in the curve by the "RatedCurrent" value gives the actual current.';
    PropertyHelp[6] := 'Multiplier or actual phase amps for the phase TCC curve.  Defaults to 1.0.';
    PropertyHelp[7] := 'Fixed delay time (sec) added to Fuse blowing time determined from the TCC curve. Default is 0.0. Used to represent fuse clearing time or any other delay.';
    PropertyHelp[8] := 'DEPRECATED. See "State" property.';
    PropertyHelp[9] := 'ARRAY of strings {Open | Closed} representing the Normal state of the fuse in each phase of the controlled element. ' +
        'The fuse reverts to this state for reset, change of mode, etc. ' +
        'Defaults to "State" if not specifically declared.';
    PropertyHelp[10] := 'ARRAY of strings {Open | Closed} representing the Actual state of the fuse in each phase of the controlled element. ' +
        'Upon setting, immediately forces state of fuse(s). Simulates manual control on Fuse. Defaults to Closed for all phases.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

{--------------------------------------------------------------------------}
function TFuse.NewObject(const ObjName: String): Integer;
begin
    // Make a new Fuse and add it to Fuse class list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveCktElement := TFuseObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;

{--------------------------------------------------------------------------}


{--------------------------------------------------------------------------}
function TFuse.Edit(ActorID: Integer): Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    Devindex, i: Integer;

begin

  // continue parsing WITH contents of Parser
    ActiveFuseObj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActiveFuseObj;

    Result := 0;

    with ActiveFuseObj do
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 402);
                1:
                    MonitoredElementName := lowercase(param);
                2:
                    MonitoredElementTerminal := Parser[ActorID].IntValue;
                3:
                    ElementName := lowercase(param);
                4:
                    ElementTerminal := Parser[ActorID].IntValue;
                5:
                    FuseCurve := GetTCCCurve(Param);
                6:
                    RatedCurrent := Parser[ActorID].Dblvalue;
                7:
                    DelayTime := Parser[ActorID].DblValue;
                9:
                begin
                    InterpretFuseState(ActorID, Param, ParamName);  // set the normal state
                    if not NormalStateSet then
                        NormalStateSet := TRUE;
                end;
                8, 10:
                    InterpretFuseState(ActorID, Param, ParamName);  // set the present state

            else
           // Inherited parameters
                ClassEdit(ActiveFuseObj, ParamPointer - NumPropsthisClass)
            end;

         {Supplemental Actions}
            case ParamPointer of
              {Default the controlled element to the monitored element}
                1:
                    ElementName := MonitoredElementName;
                2:
                    ElementTerminal := MonitoredElementTerminal;
                10:
                begin
                    for i := 1 to FNPhases do
                        if not NormalStateSet then
                            FNormalState^[i] := FPresentState^[i];
                    NormalStateSet := TRUE;   // normal state will default to state only the 1st state is specified.
                end

            end;

            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;

        RecalcElementData(ActorID);
    end;

end;


{--------------------------------------------------------------------------}
function TFuse.MakeLike(const FuseName: String): Integer;
var
    OtherFuse: TFuseObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this Fuse name in the present collection}
    OtherFuse := Find(FuseName);
    if OtherFuse <> NIL then
        with ActiveFuseObj do
        begin

            NPhases := OtherFuse.Fnphases;
            NConds := OtherFuse.Fnconds; // Force Reallocation of terminal stuff

            ElementName := OtherFuse.ElementName;
            ElementTerminal := OtherFuse.ElementTerminal;
            ControlledElement := OtherFuse.ControlledElement;  // Pointer to target circuit element

            MonitoredElement := OtherFuse.MonitoredElement;  // Pointer to target circuit element
            MonitoredElementName := OtherFuse.MonitoredElementName;  // Pointer to target circuit element
            MonitoredElementTerminal := OtherFuse.MonitoredElementTerminal;  // Pointer to target circuit element

            FuseCurve := OtherFuse.FuseCurve;
            RatedCurrent := OtherFuse.RatedCurrent;

        // can't copy action handles
            for i := 1 to Min(FUSEMAXDIM, ControlledElement.Nphases) do
            begin
                FPresentState^[i] := OtherFuse.FPresentState^[i];
                FNormalState^[i] := OtherFuse.FNormalState^[i];
            end;

            CondOffset := OtherFuse.CondOffset;

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherFuse.PropertyValue[i];

        end
    else
        DoSimpleMsg('Error in Fuse MakeLike: "' + FuseName + '" Not Found.', 403);

end;


{==========================================================================}
{                    TFuseObj                                           }
{==========================================================================}


{--------------------------------------------------------------------------}
constructor TFuseObj.Create(ParClass: TDSSClass; const FuseName: String);

var
    i: Integer;

begin
    inherited Create(ParClass);
    Name := LowerCase(FuseName);
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

    FuseCurve := GetTccCurve('tlink');

    RatedCurrent := 1.0;


    FPresentState := NIL;
    FNormalState := NIL;

     // Reallocate arrays  (Must be initialized to nil for first call)
    Reallocmem(FPresentState, Sizeof(FPresentState^[1]) * FNPhases);
    Reallocmem(FNormalState, Sizeof(FNormalState^[1]) * FNPhases);

    for i := 1 to Min(FUSEMAXDIM, FNPhases) do
    begin
        FPresentState^[i] := CTRL_CLOSE;
        FNormalState^[i] := CTRL_CLOSE;  // default to present state;
        ReadyToBlow[i] := FALSE;
        hAction[i] := 0;
    end;

    NormalStateSet := FALSE;
    cBuffer := NIL; // Complex buffer

    DSSObjType := ParClass.DSSClassType; //cap_CONTROL;

    InitPropertyValues(0);

   //  RecalcElementData;

end;

destructor TFuseObj.Destroy;
begin
    MonitoredElementName := '';
    ReallocMem(FPresentState, 0);
    ReallocMem(FNormalState, 0);
    Reallocmem(cBuffer, 0);
    inherited Destroy;
end;

{--------------------------------------------------------------------------}
procedure TFuseObj.RecalcElementData(ActorID: Integer);

var
    DevIndex, i: Integer;

begin

    Devindex := GetCktElementIndex(MonitoredElementName); // Global function
    if DevIndex > 0 then
    begin
        MonitoredElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
        Nphases := MonitoredElement.NPhases;       // Force number of phases to be same
        if Fnphases > FUSEMAXDIM then
            DosimpleMsg('Warning: Fuse ' + Self.Name + ': Number of phases > Max fuse dimension.', 404);
        if MonitoredElementTerminal > MonitoredElement.Nterms then
        begin
            DoErrorMsg('Fuse: "' + Name + '"',
                'Terminal no. "' + '" does not exist.',
                'Re-specify terminal no.', 404);
        end
        else
        begin
               // Sets name of i-th terminal's connected bus in Fuse's buslist
            Setbus(1, MonitoredElement.GetBus(MonitoredElementTerminal));
               // Allocate a buffer bigenough to hold everything from the monitored element
            ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder);
            CondOffset := (MonitoredElementTerminal - 1) * MonitoredElement.NConds; // for speedy sampling
        end;
    end;

{Check for existence of Controlled Element}

         // If previously assigned, reset HasOCPDevice flag in case this is a move
    if Assigned(ControlledElement) then
        ControlledElement.HasOCPDevice := FALSE;

    Devindex := GetCktElementIndex(ElementName); // Global function
    if DevIndex > 0 then
    begin  // Both CktElement and monitored element must already exist
        ControlledElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Make the 1 st terminal active

        if Enabled then
            ControlledElement.HasOCPDevice := TRUE;  // For Reliability calcs

               // Open/Close State of controlled element based on state assigned to the control
        for i := 1 to Min(FUSEMAXDIM, ControlledElement.Nphases) do
            if FPresentState^[i] = CTRL_OPEN then
            begin
                ControlledElement.Closed[i, ActorID] := FALSE;
            end
            else
            begin
                ControlledElement.Closed[i, ActorID] := TRUE;
            end;

        for i := 1 to ControlledElement.Nphases do
            hAction[i] := 0;
        for i := 1 to Min(FUSEMAXDIM, ControlledElement.Nphases) do
            ReadyToBlow[i] := FALSE;
    end
    else
    begin
        ControlledElement := NIL;   // element not found
        DoErrorMsg('Fuse: "' + Self.Name + '"', 'CktElement Element "' + ElementName + '" Not Found.',
            ' Element must be defined previously.', 405);
    end;
end;

{--------------------------------------------------------------------------}
procedure TFuseObj.CalcYPrim(ActorID: Integer);
begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
end;

{--------------------------------------------------------------------------}
procedure TFuseObj.GetCurrents(Curr: pComplexArray; ActorID: Integer);
var
    i: Integer;
begin

    for i := 1 to Fnconds do
        Curr^[i] := CZERO;

end;

{--------------------------------------------------------------------------}

procedure TFuseObj.GetInjCurrents(Curr: pComplexArray; ActorID: Integer);
var
    i: Integer;
begin
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

{--------------------------------------------------------------------------}
procedure TFuseObj.DoPendingAction(const Phs, ProxyHdl: Integer; ActorID: Integer);
// Do what we're instructed by the control queue
// Theoretically, there shouldn't be anything on the queue unless we have to do something
{Only legal action is to open one phase}


begin
    if Phs <= FUSEMAXDIM then
        with  ControlledElement do
        begin
            ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal of CktElement to terminal 1
            case FPresentState^[Phs] of
                CTRL_CLOSE:
                    if ReadyToBlow[Phs] then
                    begin   // ignore if we became disarmed in meantime
                        ControlledElement.Closed[Phs, ActorID] := FALSE;   // Open phases of active terminal
                        AppendtoEventLog('Fuse.' + Self.Name, 'Phase ' + IntToStr(Phs) + ' Blown', ActorID);
                        hAction[phs] := 0;
                    end;
            else
            {Do Nothing }
            end;

        end;
end;

{--------------------------------------------------------------------------}


procedure TFuseObj.InterpretFuseState(ActorID: Integer; const param: String; const property_name: String);
var
    i: Integer;
    DataStr1, DataStr2: String;

begin

    if (LowerCase(property_name[1]) = 'a') then
    begin // action (deprecated)  Will be removed

        for i := 1 to FUSEMAXDIM do
        begin

            case LowerCase(param)[1] of
                'o':
                    States[i] := CTRL_OPEN;
                'c':
                    States[i] := CTRL_CLOSE;
            end;

        end;
    end
    else
    begin
        AuxParser[ActorID].CmdString := param;  // Load up Parser

        DataStr1 := AuxParser[ActorID].NextParam;  // ignore
        DataStr2 := AuxParser[ActorID].StrValue;

        i := 1;
        while (Length(DataStr2) > 0) and (i < FUSEMAXDIM) do
        begin


            if (LowerCase(property_name[1]) = 's') then
            begin  // state
                case LowerCase(DataStr2)[1] of
                    'o':
                        States[i] := CTRL_OPEN;
                    'c':
                        States[i] := CTRL_CLOSE;
                end;
            end
            else // 'normal'
            begin
                case LowerCase(DataStr2)[1] of
                    'o':
                        NormalStates[i] := CTRL_OPEN;
                    'c':
                        NormalStates[i] := CTRL_CLOSE;
                end;
            end;

            DataStr1 := AuxParser[ActorID].NextParam;  // ignore
            DataStr2 := AuxParser[ActorID].StrValue;
            inc(i);
        end;
    end;

end;

{--------------------------------------------------------------------------}
procedure TFuseObj.Sample(ActorID: Integer);

var
    i: Integer;
    Cmag: Double;
    TripTime: Double;

begin


    ControlledElement.ActiveTerminalIdx := ElementTerminal;
    MonitoredElement.GetCurrents(cBuffer, ActorID);

    with   MonitoredElement do
        for i := 1 to Min(FUSEMAXDIM, MonitoredElement.Nphases) do
        begin

            if ControlledElement.Closed[i, ActorID]      // Check state of phases of active terminal
            then
                FPresentState[i] := CTRL_CLOSE
            else
                FPresentState[i] := CTRL_OPEN;

            if FPresentState[i] = CTRL_CLOSE then
            begin
                TripTime := -1.0;

               {Check Phase Trip, if any}

                if FuseCurve <> NIL then
                begin
                    Cmag := Cabs(cBuffer^[i]);
                    TripTime := FuseCurve.GetTCCTime(Cmag / RatedCurrent);
                end;

                if TripTime > 0.0 then
                begin
                    if not ReadyToBlow[i] then
                        with ActiveCircuit[ActorID] do
                        begin  // Then arm for an open operation
                            hAction[i] := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + Delaytime, i, 0, Self, ActorID);
                            ReadyToBlow[i] := TRUE;
                        end; {With}
                end
                else
                begin
                    if ReadyToBlow[i] then
                    begin  //  Current has dropped below pickup and it hasn't blown yet
                        ActiveCircuit[ActorID].ControlQueue.Delete(hAction[i], ActorID);  // Delete the fuse blow action
                        ReadyToBlow[i] := FALSE;
                    end;
                end;

            end;  {IF PresentState=CLOSE}
        end; {With}
end;

procedure TFuseObj.DumpProperties(var F: TextFile; Complete: Boolean);

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
        Writeln(F);

end;

function TFuseObj.GetPropertyValue(Index: Integer): String;
var
    i: Integer;
begin

    case Index of
        9..10:
            Result := '[';

    else
        Result := '';
    end;
    case Index of  // Special cases
        10:
            if ControlledElement <> NIL then
            begin
                for i := 1 to ControlledElement.NPhases do
                begin
                    case FPresentState^[i] of
                        CTRL_OPEN:
                            Result := Result + 'open' + ', ';
                    else
                      {CTRL_CLOSE:} Result := Result + 'closed' + ', ';
                    end;
                end;
            end;
        9:
            if ControlledElement <> NIL then
            begin
                for i := 1 to ControlledElement.NPhases do
                begin
                    case FNormalState^[i] of
                        CTRL_OPEN:
                            Result := Result + 'open' + ', ';
                    else
                      {CTRL_CLOSE:} Result := Result + 'closed' + ', ';
                    end;
                end;
            end;
    else
        Result := inherited GetPropertyValue(index);
    end;
    case Index of
        9..10:
            Result := Result + ']';
    else
    end;
end;


procedure TFuseObj.Reset;
var
    i: Integer;
begin

    if ControlledElement <> NIL then
    begin

        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal

        for i := 1 to Min(FUSEMAXDIM, ControlledElement.Nphases) do
        begin
            FPresentState[i] := FNormalState[i];  // reset to normal state
            ReadyToBlow[i] := FALSE;
            hAction[i] := 0;

            case FNormalState[i] of
                CTRL_OPEN:
                    ControlledElement.Closed[i, ActiveActor] := FALSE;
            else
            {CTRL_CLOSE:} ControlledElement.Closed[i, ActiveActor] := TRUE;
            end;

        end;

    end;
end;

function TFuseObj.get_States(Idx: Integer): EControlAction;
begin

    if ControlledElement <> NIL then
    begin

        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal
        case ControlledElement.Closed[Idx, ActiveActor] of
            FALSE:
                FPresentState^[Idx] := CTRL_OPEN;
        else
            {TRUE:} FPresentState^[Idx] := CTRL_CLOSE;
        end;

    end;

    Result := FPresentState^[Idx];
end;

procedure TFuseObj.set_States(Idx: Integer; const Value: EControlAction);
begin

    if States[Idx] <> Value then
    begin

        if ControlledElement <> NIL then
        begin
            ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal
            case Value of
                CTRL_OPEN:
                    ControlledElement.Closed[Idx, ActiveActor] := FALSE;
            else
                {CTRL_CLOSE:} ControlledElement.Closed[Idx, ActiveActor] := TRUE;
            end;
        end;

        FPresentState^[Idx] := Value;
    end;
end;

function TFuseObj.get_NormalStates(Idx: Integer): EControlAction;
begin
    Result := FNormalState^[Idx];
end;

procedure TFuseObj.set_NormalStates(Idx: Integer; const Value: EControlAction);
begin
    if FNormalState^[Idx] <> Value then
    begin
        FNormalState^[Idx] := Value;
    end;
end;

procedure TFuseObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := ''; //'element';
    PropertyValue[2] := '1'; //'terminal';
    PropertyValue[3] := '';
    PropertyValue[4] := '1'; //'terminal';
    PropertyValue[5] := 'Tlink';
    PropertyValue[6] := '1.0';
    PropertyValue[7] := '0';
    PropertyValue[8] := '';  // action
    PropertyValue[9] := '[close, close, close]';  // normal
    PropertyValue[10] := '[close,close,close]';  // state

    inherited  InitPropertyValues(NumPropsThisClass);

end;

initialization

end.
