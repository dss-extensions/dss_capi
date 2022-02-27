unit SwtControl;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2016, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------}

interface

uses
    Classes,
    Command,
    ControlClass,
    ControlElem,
    CktElement,
    DSSClass,
    Arraydef,
    UComplex, DSSUcomplex;

type
{$SCOPEDENUMS ON}
    TSwtControlProp = (
        INVALID = 0,
        SwitchedObj = 1,
        SwitchedTerm = 2,
        Action = 3,
        Lock = 4, 
        Delay = 5,
        Normal = 6, 
        State = 7, 
        Reset = 8
    );
{$SCOPEDENUMS OFF}

    TSwtControl = class(TControlClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TSwtControlObj = class(TControlElem)
    PUBLIC
        PresentState: EControlAction;
        NormalState: EControlAction;
        CurrentAction: EControlAction; // previously ActionCommand
        LockCommand: EControlAction;
        Locked: Boolean;
        Armed: Boolean;

        constructor Create(ParClass: TDSSClass; const SwtControlName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure Set_Enabled(Value: Boolean); OVERRIDE;
        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData; OVERRIDE;

        procedure Sample; OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state

    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    Sysutils,
    Utilities,
    solution,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TSwtControlObj;
    TProp = TSwtControlProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer;    
    ActionEnum, StateEnum: TDSSEnum;

constructor TSwtControl.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        ActionEnum := TDSSEnum.Create('SwtControl: Action', False, 1, 1, 
            ['close', 'open'], 
            [ord(CTRL_CLOSE), ord(CTRL_OPEN)]);
        ActionEnum.DefaultValue := ord(CTRL_CLOSE);
        StateEnum := TDSSEnum.Create('SwtControl: State', False, 1, 1, 
            ['closed', 'open'], 
            [ord(CTRL_CLOSE), ord(CTRL_OPEN)]);
        StateEnum.DefaultValue := ord(CTRL_CLOSE);
    end;

    inherited Create(dssContext, SWT_CONTROL, 'SwtControl');
end;

destructor TSwtControl.Destroy;
begin
    inherited Destroy;
end;

procedure DoReset(Obj: TObj);
begin
    // force a reset
    Obj.Locked := FALSE;
    Obj.Reset;
end;

function GetState(Obj: TObj): Integer;
begin
    with Obj do
    begin
        ControlledElement.ActiveTerminalIdx := ElementTerminal;
        if ControlledElement.Closed[0] then
            Result := ord(CTRL_CLOSE)
        else
            Result := ord(CTRL_OPEN);
    end;
end;

procedure TSwtControl.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // enum properties
    PropertyType[ord(TProp.Action)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.Action)] := ptruint(@obj.CurrentAction);
    PropertyOffset2[ord(TProp.Action)] := PtrInt(ActionEnum);
    PropertyOffset3[ord(TProp.Action)] := ptruint(@obj.Locked);
    PropertyFlags[ord(TProp.Action)] := [TPropertyFlag.ConditionalReadOnly, TPropertyFlag.Redundant];

    PropertyType[ord(TProp.Normal)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.Normal)] := ptruint(@obj.CurrentAction);
    PropertyOffset2[ord(TProp.Normal)] := PtrInt(StateEnum);
    PropertyOffset3[ord(TProp.Normal)] := ptruint(@obj.Locked);
    PropertyFlags[ord(TProp.Normal)] := [TPropertyFlag.ConditionalReadOnly];

    PropertyType[ord(TProp.State)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.State)] := ptruint(@obj.CurrentAction);
    PropertyOffset2[ord(TProp.State)] := PtrInt(StateEnum);
    PropertyOffset3[ord(TProp.State)] := ptruint(@obj.Locked);
    PropertyReadFunction[ord(TProp.State)] := @GetState;
    PropertyFlags[ord(TProp.State)] := [TPropertyFlag.ConditionalReadOnly, TPropertyFlag.ReadByFunction];

    // boolean
    PropertyType[ord(TProp.Lock)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.Lock)] := ptruint(@obj.Locked);

    // object references
    PropertyType[ord(TProp.SwitchedObj)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.SwitchedObj)] := ptruint(@obj.FControlledElement);
    PropertyOffset2[ord(TProp.SwitchedObj)] := 0;
    PropertyWriteFunction[ord(TProp.SwitchedObj)] := @SetControlledElement;
    PropertyFlags[ord(TProp.SwitchedObj)] := [TPropertyFlag.WriteByFunction]; //[TPropertyFlag.CheckForVar]; // not required for general cktelements

    // integer properties
    PropertyType[ord(TProp.SwitchedTerm)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.SwitchedTerm)] := ptruint(@obj.ElementTerminal);

    // double properties (default type)
    PropertyOffset[ord(TProp.Delay)] := ptruint(@obj.TimeDelay);

    // boolean action
    PropertyType[ord(TProp.Reset)] := TPropertyType.BooleanActionProperty;
    PropertyOffset[ord(TProp.Reset)] := ptruint(@DoReset);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TSwtControl.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TSwtControlObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    case Idx of
        // Default to first action specified for legacy scripts
        ord(TProp.Normal):
            NormalState := CurrentAction;
        ord(TProp.Action):
            if NormalState = CTRL_NONE then
                NormalState := CurrentAction;
        ord(TProp.Lock):
            if Locked then
                LockCommand := CTRL_LOCK
            else
                LockCommand := CTRL_UNLOCK;
        ord(TProp.State):
        begin
            PresentState := CurrentAction;
            if NormalState = CTRL_NONE then
                NormalState := PresentState;
            if ControlledElement <> NIL then
            begin
                ControlledElement.ActiveTerminalIdx := ElementTerminal;
                case PresentState of     // Force state
                    CTRL_OPEN:
                        ControlledElement.Closed[0] := FALSE;
                    CTRL_CLOSE:
                        ControlledElement.Closed[0] := TRUE;
                end;
            end;
        end;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TSwtControlObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    FNPhases := Other.Fnphases;
    NConds := Other.Fnconds; // Force Reallocation of terminal stuff

    ElementTerminal := Other.ElementTerminal;
    ControlledElement := Other.ControlledElement;  // Pointer to target circuit element

    TimeDelay := Other.TimeDelay;
    Locked := Other.Locked;
    PresentState := Other.PresentState;
    NormalState := Other.NormalState;
    CurrentAction := Other.CurrentAction;
end;

constructor TSwtControlObj.Create(ParClass: TDSSClass; const SwtControlName: String);
begin
    inherited Create(ParClass);
    Name := LowerCase(SwtControlName);
    DSSObjType := ParClass.DSSClassType;

    FNPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors in base class

    ControlledElement := NIL;
    ElementTerminal := 1;
    PresentState := CTRL_CLOSE;  // default to closed
    NormalState := CTRL_NONE;   // default to unspecified; set on first setting action or anything
    CurrentAction := PresentState;
    Lockcommand := CTRL_NONE;
    Locked := FALSE;
    Armed := FALSE;
    TimeDelay := 120.0; // 2 minutes
end;

destructor TSwtControlObj.Destroy;
begin
    inherited Destroy;
end;

procedure TSwtControlObj.RecalcElementData;
begin
    if ControlledElement = NIL then   // element not found
    begin
        DoErrorMsg(
            Format(_('SwtControl: "%s"'), [Self.Name]), 
            _('SwitchedObj is not set.'),
            _('Element must be defined previously.'), 387);
        Exit;
    end;

    FNphases := ControlledElement.NPhases;
    Nconds := FNphases;
    ControlledElement.ActiveTerminalIdx := ElementTerminal;

    // Include(ControlledElement.Flags, Flg.HasSwtControl);  // For Reliability calcs
    // attach controller bus to the switch bus - no space allocated for monitored variables
    Setbus(1, ControlledElement.GetBus(ElementTerminal));
end;

procedure TSwtControlObj.MakePosSequence();
begin
    if ControlledElement <> NIL then
    begin
        FNphases := ControlledElement.NPhases;
        Nconds := FNphases;
        Setbus(1, ControlledElement.GetBus(ElementTerminal));
    end;
    inherited;
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

procedure TSwtControlObj.Sample;
begin
    // push on the Lock command if any at the present time delay
    if LockCommand <> CTRL_NONE then
        with ActiveCircuit, ActiveCircuit.Solution do
        begin
            ControlQueue.Push(DynaVars.intHour, Dynavars.t + TimeDelay, LockCommand, 0, Self);
            LockCommand := CTRL_NONE;  // reset the lock command for next time
        end;

    if (CurrentAction <> PresentState) and not Armed then   // we need to operate this switch
        with ActiveCircuit, ActiveCircuit.Solution do
        begin
            ControlQueue.Push(DynaVars.intHour, Dynavars.t + TimeDelay, CurrentAction, 0, Self);
            Armed := TRUE;
        end;
end;

procedure TSwtControlObj.Reset;
begin
    if not Locked then
    begin
        PresentState := NormalState;
        CurrentAction := PresentState;
        Armed := FALSE;
        if ControlledElement <> NIL then
        begin
            ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal
            case NormalState of
                CTRL_OPEN:
                    ControlledElement.Closed[0] := FALSE;
            else
            {CTRL_CLOSE:} ControlledElement.Closed[0] := TRUE;  // Close all phases of active terminal
            end;
        end;
    end;
end;

procedure TSwtControlObj.Set_Enabled(Value: Boolean);
begin
    // Do nothing else besides toggling the flag,
    // we don't need BusNameRedefined from CktElement.pas
    FEnabled := Value;
end;

initialization
    PropInfo := NIL;
finalization
    ActionEnum.Free;
    StateEnum.Free;
end.
