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

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TFuse = class(TControlClass)
    PRIVATE

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const FuseName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TFuseObj = class(TControlElem)
    PRIVATE

        MonitoredElement: TDSSCktElement;

        hAction: array[1..FUSEMAXDIM] of Integer;         // handle to control queue actions
        PresentState: array[1..FUSEMAXDIM] of EControlAction;  // 0 = open 1 = close
        ReadyToBlow: array[1..FUSEMAXDIM] of Boolean;

        CondOffset: Integer; // Offset for monitored terminal
        cBuffer: pComplexArray;    // Complexarray buffer

        procedure InterpretFuseAction(const Action: String);

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
        procedure GetInjCurrents(Curr: pComplexArray); OVERRIDE;   // Returns Injextion currents

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

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

    NumPropsThisClass = 8;

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
    PropertyHelp[8] := '{Trip/Open | Close}  Action that overrides the Fuse control. Simulates manual control on Fuse ' +
        '"Trip" or "Open" causes the controlled element to open and lock out. ' +
        '"Close" causes the controlled element to close and the Fuse to reset.';

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
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

begin

  // continue parsing WITH contents of Parser
    ActiveFuseObj := ElementList.Active;
    ActiveCircuit.ActiveCktElement := ActiveFuseObj;

    Result := 0;

    with ActiveFuseObj do
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
                    FuseCurve := GetTCCCurve(Param);
                6:
                    RatedCurrent := Parser.Dblvalue;
                7:
                    DelayTime := Parser.DblValue;
                8:
                    InterpretFuseAction(Param);

            else
           // Inherited parameters
                ClassEdit(ActiveFuseObj, ParamPointer - NumPropsthisClass)
            end;

            case ParamPointer of
              {Default the controlled element to the monitored element}
                1:
                    ElementName := MonitoredElementName;
                2:
                    ElementTerminal := MonitoredElementTerminal;
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
            PresentState := OtherFuse.PresentState;
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

    for i := 1 to Min(FUSEMAXDIM, FNPhases) do
        PresentState[i] := CTRL_CLOSE;
    for i := 1 to Min(FUSEMAXDIM, FNPhases) do
        ReadyToBlow[i] := FALSE;
    for i := 1 to Min(FUSEMAXDIM, FNPhases) do
        hAction[i] := 0;


    cBuffer := NIL; // Complex buffer

    DSSObjType := ParClass.DSSClassType; //cap_CONTROL;

    InitPropertyValues(0);


   //  RecalcElementData;

end;

destructor TFuseObj.Destroy;
begin
    MonitoredElementName := '';
    Reallocmem(cBuffer, 0);
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

        for i := 1 to Min(FUSEMAXDIM, ControlledElement.Nphases) do
            if ControlledElement.Closed[i]      // Check state of i-th phase of active terminal
            then
            begin
                PresentState[i] := CTRL_CLOSE;
            end
            else
            begin
                PresentState[i] := CTRL_OPEN;
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

procedure TFuseObj.GetInjCurrents(Curr: pComplexArray);
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
    if Phs <= FUSEMAXDIM then
        with  ControlledElement do
        begin
            ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal of CktElement to terminal 1
            case PresentState[Phs] of
                CTRL_CLOSE:
                    if ReadyToBlow[Phs] then
                    begin   // ignore if we became disarmed in meantime
                        ControlledElement.Closed[Phs] := FALSE;   // Open all phases of active terminal
                        AppendtoEventLog('Fuse.' + Self.Name, 'Phase ' + IntToStr(Phs) + ' Blown');
                        hAction[phs] := 0;
                    end;
            else
            {Do Nothing }
            end;

        end;
end;

{--------------------------------------------------------------------------}


procedure TFuseObj.InterpretFuseAction(const Action: String);
begin

    if ControlledElement <> NIL then
    begin
        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal
        case LowerCase(Action)[1] of

            'o', 't':
            begin
                ControlledElement.Closed[0] := FALSE;   // Open all phases of active terminal
            end;
            'c':
            begin
                ControlledElement.Closed[0] := TRUE;    // Close all phases of active terminal
            end;
        end;
    end;

end;

{--------------------------------------------------------------------------}
procedure TFuseObj.Sample;

var
    i: Integer;
    Cmag: Double;
    TripTime: Double;

begin


    ControlledElement.ActiveTerminalIdx := ElementTerminal;
    MonitoredElement.GetCurrents(cBuffer);

    with   MonitoredElement do
        for i := 1 to Min(FUSEMAXDIM, MonitoredElement.Nphases) do
        begin

            if ControlledElement.Closed[i]      // Check state of phases of active terminal
            then
                PresentState[i] := CTRL_CLOSE
            else
                PresentState[i] := CTRL_OPEN;

            if PresentState[i] = CTRL_CLOSE then
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

begin
    Result := inherited GetPropertyValue(index);
end;


procedure TFuseObj.Reset;
var
    i: Integer;
begin
    if ControlledElement <> NIL then
    begin
        for i := 1 to Min(FUSEMAXDIM, ControlledElement.Nphases) do
            PresentState[i] := CTRL_CLOSE;
        for i := 1 to Min(FUSEMAXDIM, ControlledElement.Nphases) do
            ReadyToBlow[i] := FALSE;
        for i := 1 to Min(FUSEMAXDIM, ControlledElement.Nphases) do
            hAction[i] := 0;
        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal
        ControlledElement.Closed[0] := TRUE;             // Close all phases of active terminal
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
    PropertyValue[8] := '';

    inherited  InitPropertyValues(NumPropsThisClass);

end;

initialization

end.
