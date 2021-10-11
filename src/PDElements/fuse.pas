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
    Classes,
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
    StateArray = array[1..FUSEMAXDIM] of EControlAction;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TFuse = class(TControlClass)
    PRIVATE

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const FuseName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TFuseObj = class(TControlElem)
    PRIVATE

        MonitoredElement: TDSSCktElement;

        hAction: array[1..FUSEMAXDIM] of Integer;         // handle to control queue actions
        FPresentState, FNormalState: pStateArray;
        ReadyToBlow: array[1..FUSEMAXDIM] of Boolean;
        CondOffset: Integer; // Offset for monitored terminal
        cBuffer: pComplexArray; // Complexarray buffer

        NormalStateSet: Boolean;

        procedure InterpretFuseState(const param: string; const PropertyName: string);
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

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;    // Always Zero for a Fuse

        procedure Sample; OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Phs, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state


        procedure GetCurrents(Curr: pComplexArray); OVERRIDE; // Get present value of terminal Curr

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(F: TFileStream; Complete: Boolean); OVERRIDE;

        property States[Idx: Integer]: EControlAction read get_States write set_States;
        property NormalStates[Idx: Integer]: EControlAction read get_NormalStates write set_NormalStates;

    end;

{--------------------------------------------------------------------------}
implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    Sysutils,
    uCmatrix,
    MathUtil,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

const

    NumPropsThisClass = 10;

var
    TCC_CurveClass: TDSSClass;

{General Module Function}

function GetTccCurve(DSS: TDSSContext; const CurveName: String): TTCC_CurveObj;

begin

    Result := TCC_CurveClass.Find(CurveName);

    if Result = NIL then
        DoSimpleMsg(DSS, 'TCC Curve object: "' + CurveName + '" not found.', 401);

end;


{--------------------------------------------------------------------------}
constructor TFuse.Create(dssContext: TDSSContext);  // Creates superstructure for all Fuse objects
begin
    inherited Create(dssContext);

    Class_name := 'Fuse';
    DSSClassType := DSSClassType + FUSE_CONTROL;

    DefineProperties;

    CommandList := TCommandList.Create(SliceProps(PropertyName, NumProperties));
    CommandList.Abbrev := TRUE;

    TCC_CurveClass := GetDSSClassPtr(DSS, 'TCC_Curve');
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
    with ActiveCircuit do
    begin
        ActiveCktElement := TFuseObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;

{--------------------------------------------------------------------------}


{--------------------------------------------------------------------------}
function TFuse.Edit: Integer;
var
    ParamPointer, i: Integer;
    ParamName: String;
    Param: String;

begin

  // continue parsing WITH contents of Parser
    DSS.ActiveFuseObj := ElementList.Active;
    ActiveCircuit.ActiveCktElement := DSS.ActiveFuseObj;

    Result := 0;

    with DSS.ActiveFuseObj do
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
                PropertyValue[ParamPointer] := Param;

            case ParamPointer of
                0:
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 402);
                1:
                    MonitoredElementName := lowercase(param);
                2:
                    MonitoredElementTerminal := Parser.IntValue;
                3:
                    ElementName := lowercase(param);
                4:
                    ElementTerminal := Parser.IntValue;
                5:
                    FuseCurve := GetTCCCurve(DSS, Param);
                6:
                    RatedCurrent := Parser.Dblvalue;
                7:
                    DelayTime := Parser.DblValue;
                9: 
                begin
                    InterpretFuseState(Param, ParamName); // set the normal state
                    NormalStateSet := TRUE;
                end;
                8, 10:
                    InterpretFuseState(Param, ParamName); // set the present state
            else
                // Inherited parameters
                ClassEdit(DSS.ActiveFuseObj, ParamPointer - NumPropsthisClass)
            end;

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
                            FNormalState[i] := FPresentState[i];
                    
                    NormalStateSet := TRUE; // normal state will default to state only the 1st state is specified.
                end;
            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

        RecalcElementData;
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
    if OtherFuse = NIL then
    begin
        DoSimpleMsg('Error in Fuse MakeLike: "' + FuseName + '" Not Found.', 403);
        Exit;
    end;

    with DSS.ActiveFuseObj do
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

        for i := 1 to Min(FUSEMAXDIM, ControlledElement.Nphases) do 
        begin
            FPresentState[i] := OtherFuse.FPresentState[i];
            FNormalState[i] := OtherFuse.FNormalState[i];
        end;
        CondOffset := OtherFuse.CondOffset;

        for i := 1 to ParentClass.NumProperties do
            PropertyValue[i] := OtherFuse.PropertyValue[i];

    end
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

    FuseCurve := GetTccCurve(DSS, 'tlink');

    RatedCurrent := 1.0;

    FPresentState := NIL;
    FNormalState := NIL;

    // Reallocate arrays (Must be initialized to nil for first call)
    Reallocmem(FPresentState, Sizeof(FPresentState[1]) * FNPhases);
    Reallocmem(FNormalState, Sizeof(FNormalState[1]) * FNPhases);

    for i := 1 to Min(FUSEMAXDIM, FNPhases) do 
    begin
        FPresentState^[i] := CTRL_CLOSE;
        FNormalState^[i] := CTRL_CLOSE; // default to present state
        ReadyToBlow[i] := FALSE;
        hAction[i] := 0;
    End;
    NormalStateSet := FALSE;

    cBuffer := NIL; // Complex buffer

    DSSObjType := ParClass.DSSClassType; //cap_CONTROL;

    InitPropertyValues(0);


   //  RecalcElementData;

end;

destructor TFuseObj.Destroy;
begin
    MonitoredElementName := '';
    Reallocmem(cBuffer, 0);
    ReallocMem(FPresentState, 0);
    ReallocMem(FNormalState, 0);

    inherited Destroy;
end;

{--------------------------------------------------------------------------}
procedure TFuseObj.RecalcElementData;

var
    DevIndex, i: Integer;

begin

    Devindex := GetCktElementIndex(MonitoredElementName); // Global function
    if DevIndex > 0 then
    begin
        MonitoredElement := ActiveCircuit.CktElements.Get(DevIndex);
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
        ControlledElement := ActiveCircuit.CktElements.Get(DevIndex);
        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Make the 1 st terminal active

        if Enabled then
            ControlledElement.HasOCPDevice := TRUE;  // For Reliability calcs

        // Open/Close State of controlled element based on state assigned to the control
        for i := 1 to Min(FUSEMAXDIM, ControlledElement.Nphases) do
            if FPresentState[i] = CTRL_OPEN then
                ControlledElement.Closed[i] := FALSE
            else
                ControlledElement.Closed[i] := TRUE;

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
procedure TFuseObj.CalcYPrim;
begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
end;

{--------------------------------------------------------------------------}
procedure TFuseObj.GetCurrents(Curr: pComplexArray);
var
    i: Integer;
begin

    for i := 1 to Fnconds do
        Curr^[i] := CZERO;

end;

{--------------------------------------------------------------------------}
procedure TFuseObj.DoPendingAction(const Phs, ProxyHdl: Integer);
// Do what we're instructed by the control queue
// Theoretically, there shouldn't be anything on the queue unless we have to do something
{Only legal action is to open one phase}
begin
    if Phs > FUSEMAXDIM then
        Exit;

    with ControlledElement do
    begin
        ControlledElement.ActiveTerminalIdx := ElementTerminal;
        if FPresentState[Phs] = CTRL_CLOSE then
            if ReadyToBlow[Phs] then
            begin   // ignore if we became disarmed in meantime
                ControlledElement.Closed[Phs] := FALSE;   // Open all phases of active terminal
                AppendtoEventLog('Fuse.' + Self.Name, 'Phase ' + IntToStr(Phs) + ' Blown');
                hAction[phs] := 0;
            end;
    end;
end;

{--------------------------------------------------------------------------}
procedure TFuseObj.InterpretFuseState(const param: String; const PropertyName: String);
var
    i: Integer;
    DataStr2: String;
begin
    if (LowerCase(PropertyName[1]) = 'a') then
    begin // action (deprecated)  Will be removed
        for i := 1 to FUSEMAXDIM do
        begin
            case LowerCase(PropertyName[1]) of
                'o':
                    States[i] := CTRL_OPEN;
                'c':
                    States[i] := CTRL_CLOSE;
            end;
        end;

        Exit;
    end;

    AuxParser.CmdString := param;  // Load up Parser
    {DataStr1 :=} AuxParser.NextParam;  // ignore
    DataStr2 := AuxParser.StrValue;

    i := 1;
    while (Length(DataStr2) > 0) and (i < FUSEMAXDIM) do 
    begin
        if (LowerCase(PropertyName[1]) = 's') then 
        begin // state
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

        {DataStr1 :=} AuxParser.NextParam;  // ignore
        DataStr2 := AuxParser.StrValue;
        inc(i);
    end;
End;

{--------------------------------------------------------------------------}
procedure TFuseObj.Sample;
var
    i: Integer;
    Cmag: Double;
    TripTime: Double;

begin
    ControlledElement.ActiveTerminalIdx := ElementTerminal;
    MonitoredElement.GetCurrents(cBuffer);

    with MonitoredElement do
        for i := 1 to Min(FUSEMAXDIM, MonitoredElement.Nphases) do
        begin
            if ControlledElement.Closed[i]      // Check state of phases of active terminal
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
                        with ActiveCircuit do
                        begin  // Then arm for an open operation
                            hAction[i] := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + Delaytime, i, 0, Self);
                            ReadyToBlow[i] := TRUE;
                        end; {With}
                end
                else
                begin
                    if ReadyToBlow[i] then
                    begin  //  Current has dropped below pickup and it hasn't blown yet
                        ActiveCircuit.ControlQueue.Delete(hAction[i]);  // Delete the fuse blow action
                        ReadyToBlow[i] := FALSE;
                    end;
                end;

            end;  {IF PresentState=CLOSE}
        end; {With}
end;


{--------------------------------------------------------------------------}
procedure TFuseObj.DumpProperties(F: TFileStream; Complete: Boolean);

var
    i: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            FSWriteln(F, '~ ' + PropertyName^[i] + '=' + PropertyValue[i]);
        end;

    if Complete then
        FSWriteln(F);

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

    case index of  // Special cases
       10:
            if ControlledElement <> Nil Then
                for i := 1 to ControlledElement.NPhases do
                    if FPresentState[i] = CTRL_OPEN then
                        Result := Result + 'open' + ', '
                    else
                        Result := Result + 'closed' + ', ';
       9:
            if ControlledElement <> Nil Then
                for i := 1 to ControlledElement.NPhases do
                    if FNormalState[i] = CTRL_OPEN then
                        Result := Result + 'open' + ', '
                    else
                        Result := Result + 'closed' + ', ';

    else
        Result := inherited GetPropertyValue(index);
    end;

    case Index of
      9..10: 
        Result := Result + ']';
    end;
end;

procedure TFuseObj.Reset;
var
    i: Integer;
begin
    if ControlledElement = NIL then
        Exit;

    ControlledElement.ActiveTerminalIdx := ElementTerminal;

    for i := 1 to Min(FUSEMAXDIM, ControlledElement.Nphases) do
    begin
        FPresentState[i] := FNormalState[i];  // reset to normal state
        ReadyToBlow[i] := FALSE;
        hAction[i] := 0;

        if FNormalState[i] = CTRL_OPEN then
            ControlledElement.Closed[i] := FALSE
        else
            ControlledElement.Closed[i] := TRUE;
    end;
end;

function TFuseObj.get_States(Idx: Integer): EControlAction;
begin
    if ControlledElement <> NIL then
    begin
        ControlledElement.ActiveTerminalIdx := ElementTerminal; 
        if not ControlledElement.Closed[Idx] then
            FPresentState[Idx]:= CTRL_OPEN
        else
            FPresentState[Idx]:= CTRL_CLOSE;
    end;
    Result := FPresentState[Idx];
end;

procedure TFuseObj.set_States(Idx: Integer; const Value: EControlAction);
begin
    if States[Idx] = Value then 
        Exit;

    FPresentState[Idx] := Value;

    if ControlledElement = NIL then
        Exit;

    ControlledElement.ActiveTerminalIdx := ElementTerminal;
    if Value = CTRL_OPEN then
        ControlledElement.Closed[Idx] := FALSE
    else
        ControlledElement.Closed[Idx] := TRUE;
end;

procedure TFuseObj.set_NormalStates(Idx: Integer; const Value: EControlAction);
begin
    //TODO: validate Idx if exposed through the end-user API
    FNormalState[Idx] := Value;
    NormalStateSet := TRUE;
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
    PropertyValue[8] := ''; // action
    PropertyValue[9] := '[close, close, close]'; // normal
    PropertyValue[10] := '[close,close,close]'; // state

    inherited  InitPropertyValues(NumPropsThisClass);

end;

function TFuseObj.get_NormalStates(Idx: Integer): EControlAction;
begin
    Result := FNormalState[Idx];
end;

end.
