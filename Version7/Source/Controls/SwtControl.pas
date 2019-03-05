unit SwtControl;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2016, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------}

interface

uses
    Command,
    ControlClass,
    ControlElem,
    CktElement,
    DSSClass,
    Arraydef,
    ucomplex;

type

    TSwtControl = class(TControlClass)
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const SwtControlName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;
    end;

    TSwtControlObj = class(TControlElem)
    PRIVATE
        FPresentState: EControlAction;
        FNormalState: EControlAction;
        ActionCommand: EControlAction;
        LockCommand: EControlAction;
        FLocked: Boolean;
        Armed: Boolean;

        procedure InterpretSwitchAction(const Action: String);
        procedure Set_NormalState(const Value: EControlAction);
        procedure set_Flocked(const Value: Boolean);
        procedure Set_LastAction(const Value: EControlAction);
        procedure Set_PresentState(const Value: EControlAction);
    PUBLIC
        constructor Create(ParClass: TDSSClass; const SwtControlName: String);
        destructor Destroy; OVERRIDE;

        procedure MakePosSequence; OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;    // Always Zero for a SwtControl

        procedure Sample; OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state

        procedure GetCurrents(Curr: pComplexArray); OVERRIDE; // Get present value of terminal Curr
        procedure GetInjCurrents(Curr: pComplexArray); OVERRIDE;   // Returns Injextion currents

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
        property NormalState: EControlAction READ FNormalState WRITE Set_NormalState;
        property PresentState: EControlAction READ FPresentState WRITE Set_PresentState;
        property IsLocked: Boolean READ FLocked;
        property Locked: Boolean READ Flocked WRITE set_Flocked;
        property CurrentAction: EControlAction READ ActionCommand WRITE Set_LastAction;
    end;

var
    ActiveSwtControlObj: TSwtControlObj;

{--------------------------------------------------------------------------}
implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    Sysutils,
    Utilities,
    solution;

const

    NumPropsThisClass = 8;

constructor TSwtControl.Create;  // Creates superstructure for all SwtControl objects
begin
    inherited Create;

    Class_name := 'SwtControl';
    DSSClassType := DSSClassType + SWT_CONTROL;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

destructor TSwtControl.Destroy;

begin
    inherited Destroy;
end;

procedure TSwtControl.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count

    AllocatePropertyArrays;   {see DSSClass}

    PropertyName[1] := 'SwitchedObj';
    PropertyName[2] := 'SwitchedTerm';
    PropertyName[3] := 'Action';
    PropertyName[4] := 'Lock';
    PropertyName[5] := 'Delay';
    PropertyName[6] := 'Normal';
    PropertyName[7] := 'State';
    PropertyName[8] := 'Reset';

    PropertyHelp[1] := 'Name of circuit element switch that the SwtControl operates. ' +
        'Specify the full object class and name.';
    PropertyHelp[2] := 'Terminal number of the controlled element switch. ' +
        '1 or 2, typically.  Default is 1.';
    PropertyHelp[3] := '{Open | Close}  After specified delay time, and if not locked, causes the controlled switch to open or close. ';
    PropertyHelp[4] := '{Yes | No} Delayed action. Sends CTRL_LOCK or CTRL_UNLOCK message to control queue. ' +
        'After delay time, controlled switch is locked in its present open / close state or unlocked. ' +
        'Switch will not respond to either manual (Action) or automatic (COM interface) control or internal OpenDSS Reset when locked.';
    PropertyHelp[5] := 'Operating time delay (sec) of the switch. Defaults to 120.';
    PropertyHelp[6] := '{Open | Closed] Normal state of the switch. If not Locked, the switch reverts to this state for reset, change of mode, etc.' +
        ' Defaults to first Action or State specified if not specifically declared.';
    PropertyHelp[7] := '{Open | Closed] Present state of the switch. Upon setting, immediately forces state of switch.';
    PropertyHelp[8] := '{Yes | No} If Yes, forces Reset of switch to Normal state and removes Lock independently of any internal ' +
        'reset command for mode change, etc.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

function TSwtControl.NewObject(const ObjName: String): Integer;
begin
    // Make a new SwtControl and add it to SwtControl class list
    with ActiveCircuit do
    begin
        ActiveCktElement := TSwtControlObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;

function TSwtControl.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    Devindex: Integer;

begin

  // continue parsing WITH contents of Parser
    ActiveSwtControlObj := ElementList.Active;
    ActiveCircuit.ActiveCktElement := ActiveSwtControlObj;

    Result := 0;

    with ActiveSwtControlObj do
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
           {internal SwtControl Property commands}
                0:
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 382);
                1:
                    ElementName := lowercase(Param);
                2:
                    ElementTerminal := Parser.IntValue;
                3:
                    InterpretSwitchAction(param);
                4:
                    Locked := InterpretYesNo(Param);
                5:
                    TimeDelay := Parser.DblValue;
                6:
                begin    // set the normal state
                    InterpretSwitchAction(param);
                    NormalState := ActionCommand;
                end;
                7:
                begin    // set the present state
                    InterpretSwitchAction(param);
                    PresentState := ActionCommand;
                end;
                8:
                    if InterpretYesNo(Param) then
                    begin  // force a reset
                        Locked := FALSE;
                        Reset;
                        PropertyValue[8] := 'n';
                    end;

            else
           // Inherited parameters
                ClassEdit(ActiveSwtControlObj, ParamPointer - NumPropsthisClass)
            end;

         {supplemental actions}
            case ParamPointer of

             // Default to first action specified for legacy scripts
                3:
                    if NormalState = CTRL_NONE then
                        NormalState := ActionCommand;

                4:
                    if Locked then
                        LockCommand := CTRL_LOCK
                    else
                        LockCommand := CTRL_UNLOCK;

                7:
                begin
                    if NormalState = CTRL_NONE then
                        NormalState := PresentState;
                    Devindex := GetCktElementIndex(ElementName);   // Set Controlled element
                    if DevIndex > 0 then
                    begin
                        ControlledElement := ActiveCircuit.CktElements.Get(DevIndex);
                        if ControlledElement <> NIL then
                            case PresentState of     // Force state
                                CTRL_OPEN:
                                    ControlledElement.Closed[0] := FALSE;
                                CTRL_CLOSE:
                                    ControlledElement.Closed[0] := TRUE;
                            end;
                    end;
                end;
            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

        RecalcElementData;
    end;

end;

function TSwtControl.MakeLike(const SwtControlName: String): Integer;
var
    OtherSwtControl: TSwtControlObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this SwtControl name in the present collection}
    OtherSwtControl := Find(SwtControlName);
    if OtherSwtControl <> NIL then
        with ActiveSwtControlObj do
        begin

            NPhases := OtherSwtControl.Fnphases;
            NConds := OtherSwtControl.Fnconds; // Force Reallocation of terminal stuff

            ElementName := OtherSwtControl.ElementName;
            ElementTerminal := OtherSwtControl.ElementTerminal;
            ControlledElement := OtherSwtControl.ControlledElement;  // Pointer to target circuit element

            TimeDelay := OtherSwtControl.TimeDelay;
            Locked := OtherSwtControl.Locked;
            PresentState := OtherSwtControl.PresentState;
            NormalState := OtherSwtControl.NormalState;
            ActionCommand := OtherSwtControl.ActionCommand;
            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherSwtControl.PropertyValue[i];

        end
    else
        DoSimpleMsg('Error in SwtControl MakeLike: "' + SwtControlName + '" Not Found.', 383);

end;

{==========================================================================}
{                    TSwtControlObj                                           }
{==========================================================================}

constructor TSwtControlObj.Create(ParClass: TDSSClass; const SwtControlName: String);
begin
    inherited Create(ParClass);
    Name := LowerCase(SwtControlName);
    DSSObjType := ParClass.DSSClassType;

    NPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors in base class

    ElementName := '';
    ControlledElement := NIL;
    ElementTerminal := 1;
    PresentState := CTRL_CLOSE;  // default to closed
    NormalState := CTRL_NONE;   // default to unspecified; set on first setting action or anything
    ActionCommand := PresentState;
    Lockcommand := CTRL_NONE;
    Locked := FALSE;
    Armed := FALSE;
    TimeDelay := 120.0; // 2 minutes

    InitPropertyValues(0);
end;

destructor TSwtControlObj.Destroy;
begin
    inherited Destroy;
end;

procedure TSwtControlObj.RecalcElementData;
var
    DevIndex: Integer;
begin
    Devindex := GetCktElementIndex(ElementName);
    if DevIndex > 0 then
    begin
        ControlledElement := ActiveCircuit.CktElements.Get(DevIndex);
        Nphases := ControlledElement.NPhases;
        Nconds := FNphases;
        ControlledElement.ActiveTerminalIdx := ElementTerminal;

        ControlledElement.HasSwtControl := TRUE;  // For Reliability calcs
{
    if not Locked then
      Case PresentState of
        CTRL_OPEN: ControlledElement.Closed[0] := FALSE;
        CTRL_CLOSE: ControlledElement.Closed[0] := TRUE;
      End;

}
    // attach controller bus to the switch bus - no space allocated for monitored variables
        Setbus(1, ControlledElement.GetBus(ElementTerminal));
    end
    else
    begin
        ControlledElement := NIL;   // element not found
        DoErrorMsg('SwtControl: "' + Self.Name + '"', 'CktElement Element "' + ElementName + '" Not Found.',
            ' Element must be defined previously.', 387);
    end;
end;

procedure TSwtControlObj.MakePosSequence;
begin
    if ControlledElement <> NIL then
    begin
        Nphases := ControlledElement.NPhases;
        Nconds := FNphases;
        Setbus(1, ControlledElement.GetBus(ElementTerminal));
    end;
    inherited;
end;

{--------------------------------------------------------------------------}
procedure TSwtControlObj.CalcYPrim;
begin
  // leave YPrims as nil
end;

procedure TSwtControlObj.GetCurrents(Curr: pComplexArray);
var
    i: Integer;
begin
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

procedure TSwtControlObj.GetInjCurrents(Curr: pComplexArray);
var
    i: Integer;
begin
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

procedure TSwtControlObj.DoPendingAction(const Code, ProxyHdl: Integer);
var
    ctrl_code: EControlAction;
begin
    ctrl_code := EControlAction(Code);  // change type
    ControlledElement.ActiveTerminalIdx := ElementTerminal;
    case Ctrl_Code of
        CTRL_LOCK:
            Locked := TRUE;
        CTRL_UNLOCK:
            Locked := FALSE;
    else
        if not Locked then
        begin
            if (Code = Integer(CTRL_OPEN)) and (PresentState = CTRL_CLOSE) then
            begin
                ControlledElement.Closed[0] := FALSE; // Open all phases of active terminal
                PresentState := CTRL_OPEN;
                AppendtoEventLog('SwtControl.' + Self.Name, 'Opened');
            end;
            if (Code = Integer(CTRL_CLOSE)) and (PresentState = CTRL_OPEN) then
            begin
                ControlledElement.Closed[0] := TRUE;    // Close all phases of active terminal
                PresentState := CTRL_CLOSE;
                AppendtoEventLog('SwtControl.' + Self.Name, 'Closed');
            end;
            Armed := FALSE;  // reset the switch
        end;
    end;
end;

procedure TSwtControlObj.InterpretSwitchAction(const Action: String);
begin
    if not Locked then
    begin
        case LowerCase(Action)[1] of
            'o':
                ActionCommand := CTRL_OPEN;
        else    // default is closed
            ActionCommand := CTRL_CLOSE;
        end;

    {   Changed to delayed action
    if ControlledElement <> nil then begin
      ControlledElement.ActiveTerminalIdx := ElementTerminal;
      Case PresentState of
        CTRL_OPEN: ControlledElement.Closed[0] := FALSE;
        CTRL_CLOSE: ControlledElement.Closed[0] := TRUE;
      End;
    End;
    }

    end;
end;

procedure TSwtControlObj.Sample;
begin

// push on the Lock command if any at the present time delay
    if LockCommand <> CTRL_NONE then
        with ActiveCircuit, ActiveCircuit.Solution do
        begin
            ControlQueue.Push(DynaVars.intHour, Dynavars.t + TimeDelay, LockCommand, 0, Self);
            LockCommand := CTRL_NONE;  // reset the lock command for next time
        end;

    if (ActionCommand <> PresentState) and not Armed then   // we need to operate this switch
        with ActiveCircuit, ActiveCircuit.Solution do
        begin
            ControlQueue.Push(DynaVars.intHour, Dynavars.t + TimeDelay, ActionCommand, 0, Self);
            Armed := TRUE;
        end;
  {ControlledElement.ActiveTerminalIdx := ElementTerminal;
  IF  ControlledElement.Closed [0]      // Check state of phases of active terminal
  THEN PresentState := CTRL_CLOSE
  ELSE PresentState := CTRL_OPEN; }
end;

procedure TSwtControlObj.set_Flocked(const Value: Boolean);
begin
    Flocked := Value;
end;

procedure TSwtControlObj.Set_LastAction(const Value: EControlAction);
begin
    ActionCommand := Value;

end;

procedure TSwtControlObj.Set_NormalState(const Value: EControlAction);
begin
    FNormalState := Value;
end;

procedure TSwtControlObj.Set_PresentState(const Value: EControlAction);
begin
    FPresentState := Value;
end;

procedure TSwtControlObj.DumpProperties(var F: TextFile; Complete: Boolean);
var
    i: Integer;
begin
    inherited DumpProperties(F, Complete);
    with ParentClass do
        for i := 1 to NumProperties do
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[PropertyIdxMap[i]]);
    if Complete then
        Writeln(F);
end;

function TSwtControlObj.GetPropertyValue(Index: Integer): String;
begin
    Result := '';
    case Index of
        1:
            Result := ElementName;
        2:
            Result := Format('%d', [ElementTerminal]);
        3:
            case ActionCommand of
                CTRL_OPEN:
                    Result := 'open';
            else
          {CTRL_CLOSE:} Result := 'close';
            end;
        4:
            if Locked then
                Result := 'Yes'
            else
                Result := 'No';
        5:
            Result := Format('%-.7g', [TimeDelay]);
        6:
            case FNormalState of
                CTRL_OPEN:
                    Result := 'open';
            else
          {CTRL_CLOSE:} Result := 'closed';
            end;
        7:
        begin
            ControlledElement.ActiveTerminalIdx := ElementTerminal;
            if ControlledElement.Closed[0] then
                Result := 'Closed'
            else
                Result := 'open';
        end;
        8:
            Result := 'n';  // Always no; yes is executed immediately
    else
        Result := inherited GetPropertyValue(Index);
    end;

end;

procedure TSwtControlObj.Reset;
begin
    if not Locked then
    begin
        PresentState := NormalState;
        ActionCommand := PresentState;
        Armed := FALSE;
        if ControlledElement <> NIL then
        begin
            ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal
            case FNormalState of
                CTRL_OPEN:
                    ControlledElement.Closed[0] := FALSE;
            else
            {CTRL_CLOSE:} ControlledElement.Closed[0] := TRUE;  // Close all phases of active terminal
            end;
        end;
    end;
end;

procedure TSwtControlObj.InitPropertyValues(ArrayOffset: Integer);
begin
    PropertyValue[1] := ''; //'element';
    PropertyValue[2] := '1'; //'terminal';
    PropertyValue[3] := 'c';
    PropertyValue[4] := 'n';
    PropertyValue[5] := '120.0';
    PropertyValue[6] := 'c';
    PropertyValue[7] := 'c';
    PropertyValue[8] := 'n';
    inherited  InitPropertyValues(NumPropsThisClass);
end;

end.
