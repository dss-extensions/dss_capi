unit SwtControl;

// ----------------------------------------------------------
// Copyright (c) 2008-2016, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

uses
    Classes,
    Command,
    ControlClass,
    ControlElem,
    CktElement,
    DSSClass,
    CAPI_Types,
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
    TSwtControlPropLegacy = TSwtControlProp;
{$SCOPEDENUMS OFF}

    TSwtControl = class(TControlClass)
    PROTECTED
        procedure DefineProperties(); override;
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
        Locked: LongBool;
        Armed: Boolean;

        constructor Create(ParClass: TDSSClass; const SwtControlName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure SetEnabled(Value: WordBool); OVERRIDE;
        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData(); OVERRIDE;

        procedure Sample(); OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset(); OVERRIDE;  // Reset to initial defined state

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
    TPropLegacy = TSwtControlPropLegacy;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;    
    ActionEnum, StateEnum: TDSSEnum;

constructor TSwtControl.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
        ActionEnum := TDSSEnum.Create('SwtControl: Action', False, 1, 1, 
            ['Close', 'Open'], 
            [ord(CTRL_CLOSE), ord(CTRL_OPEN)]);
        ActionEnum.DefaultValue := ord(CTRL_CLOSE);
        StateEnum := TDSSEnum.Create('SwtControl: State', False, 1, 1, 
            ['Closed', 'Open'], 
            [ord(CTRL_CLOSE), ord(CTRL_OPEN)]);
        StateEnum.DefaultValue := ord(CTRL_CLOSE);
    end;

    inherited Create(dssContext, SWT_CONTROL, 'SwtControl');
end;

destructor TSwtControl.Destroy;
begin
    inherited Destroy;
end;

procedure DoReset(obj: TObj);
begin
    // force a reset
    obj.Locked := FALSE;
    obj.Reset();
end;

function GetState(obj: TObj): Integer;
begin
    if obj.controlledElement = NIL then
    begin
        // If no element is attached, return CTRL_NONE to indicate we cannot tell the state            
        Result := ord(CTRL_NONE);
        Exit;
    end;
    obj.controlledElement.SetActiveTerminalIdx(obj.ElementTerminal);
    if obj.controlledElement.ConductorClosed(0) then
        Result := ord(CTRL_CLOSE)
    else
        Result := ord(CTRL_OPEN);
end;

procedure TSwtControl.DefineProperties();
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, PropInfoLegacy);

    // enum properties
    PropertyType[ord(TProp.Action)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.Action)] := PtrInt(@obj.CurrentAction);
    PropertyOffset2[ord(TProp.Action)] := PtrInt(ActionEnum);
    PropertyOffset3[ord(TProp.Action)] := PtrInt(@obj.Locked);
    PropertyFlags[ord(TProp.Action)] := [TPropertyFlag.ConditionalReadOnly, TPropertyFlag.Redundant];
    PropertyRedundantWith[ord(TProp.Action)] := ord(TProp.State);

    PropertyType[ord(TProp.Normal)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.Normal)] := PtrInt(@obj.NormalState);
    PropertyOffset2[ord(TProp.Normal)] := PtrInt(StateEnum);
    PropertyOffset3[ord(TProp.Normal)] := PtrInt(@obj.Locked);
    PropertyFlags[ord(TProp.Normal)] := [TPropertyFlag.ConditionalReadOnly, TPropertyFlag.DynamicDefault];

    PropertyType[ord(TProp.State)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.State)] := PtrInt(@obj.PresentState);
    PropertyOffset2[ord(TProp.State)] := PtrInt(StateEnum);
    PropertyOffset3[ord(TProp.State)] := PtrInt(@obj.Locked);
    PropertyReadFunction[ord(TProp.State)] := @GetState;
    PropertyFlags[ord(TProp.State)] := [TPropertyFlag.ConditionalReadOnly, TPropertyFlag.ReadByFunction, TPropertyFlag.NoDefault];

    // boolean
    PropertyType[ord(TProp.Lock)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.Lock)] := PtrInt(@obj.Locked);

    // object references
    PropertyType[ord(TProp.SwitchedObj)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.SwitchedObj)] := PtrInt(@obj.controlledElement);
    PropertyOffset2[ord(TProp.SwitchedObj)] := 0;
    PropertyWriteFunction[ord(TProp.SwitchedObj)] := @SetControlledElement;
    PropertyFlags[ord(TProp.SwitchedObj)] := [TPropertyFlag.WriteByFunction]; //[TPropertyFlag.CheckForVar]; // not required for general cktelements

    // integer properties
    PropertyType[ord(TProp.SwitchedTerm)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.SwitchedTerm)] := PtrInt(@obj.ElementTerminal);

    // double properties (default type)
    PropertyOffset[ord(TProp.Delay)] := PtrInt(@obj.TimeDelay);
    PropertyFlags[ord(TProp.Delay)] := [TPropertyFlag.Units_s];

    // boolean action
    PropertyType[ord(TProp.Reset)] := TPropertyType.BooleanActionProperty;
    PropertyOffset[ord(TProp.Reset)] := PtrInt(@DoReset);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties();
end;

function TSwtControl.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    obj: TObj;
begin
    obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.SetActiveCktElement(obj);
    obj.ClassIndex := AddObjectToList(obj, Activate);
    Result := obj;
end;

procedure TSwtControlObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags);
begin
    case Idx of
        ord(TProp.Normal):
            CurrentAction := NormalState;
        ord(TProp.Action):
            if NormalState = CTRL_NONE then
            begin
                // Default to first action specified for legacy scripts
                NormalState := CurrentAction;
            end;
        ord(TProp.Lock):
            if Locked then
                LockCommand := CTRL_LOCK
            else
                LockCommand := CTRL_UNLOCK;
        ord(TProp.State):
        begin
            CurrentAction := PresentState;
            if NormalState = CTRL_NONE then
                NormalState := PresentState;
            if controlledElement <> NIL then
            begin
                controlledElement.SetActiveTerminalIdx(ElementTerminal);
                case PresentState of     // Force state
                    CTRL_OPEN:
                        controlledElement.SetConductorClosed(0, FALSE);
                    CTRL_CLOSE:
                        controlledElement.SetConductorClosed(0, TRUE);
                end;
            end;
        end;
    end;
    inherited PropertySideEffects(Idx, previousIntVal, setterFlags);
end;

procedure TSwtControlObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    FNPhases := Other.Fnphases;
    SetNConds(Other.FNConds); // Force Reallocation of terminal stuff

    ElementTerminal := Other.ElementTerminal;
    SetControlledElement(Other.ControlledElement);  // Pointer to target circuit element

    TimeDelay := Other.TimeDelay;
    Locked := Other.Locked;
    PresentState := Other.PresentState;
    NormalState := Other.NormalState;
    CurrentAction := Other.CurrentAction;
end;

constructor TSwtControlObj.Create(ParClass: TDSSClass; const SwtControlName: String);
begin
    inherited Create(ParClass, SwtControlName);
    DSSObjType := ParClass.DSSClassType;

    FNPhases := 3;  // Directly set conds and phases
    FNConds := 3;
    SetNTerms(1);  // this forces allocation of terminals and conductors in base class

    SetControlledElement(NIL);
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

procedure TSwtControlObj.RecalcElementData();
begin
    if controlledElement = NIL then   // element not found
    begin
        DoErrorMsg(
            Format(_('SwtControl: "%s"'), [Self.Name]), 
            _('SwitchedObj is not set.'),
            _('Element must be defined previously.'), 387);
        Exit;
    end;

    FNphases := controlledElement.NPhases;
    SetNConds(FNphases);
    controlledElement.SetActiveTerminalIdx(ElementTerminal);

    // Include(controlledElement.Flags, Flg.HasSwtControl);  // For Reliability calcs
    // attach controller bus to the switch bus - no space allocated for monitored variables
    Setbus(1, controlledElement.GetBus(ElementTerminal));
end;

procedure TSwtControlObj.MakePosSequence();
begin
    if controlledElement <> NIL then
    begin
        FNphases := controlledElement.NPhases;
        SetNConds(FNphases);
        Setbus(1, controlledElement.GetBus(ElementTerminal));
    end;
    inherited;
end;

procedure TSwtControlObj.DoPendingAction(const Code, ProxyHdl: Integer);
var
    ctrl_code: EControlAction;
begin
    ctrl_code := EControlAction(Code);  // change type
    controlledElement.SetActiveTerminalIdx(ElementTerminal);
    case Ctrl_Code of
        CTRL_LOCK:
            Locked := TRUE;
        CTRL_UNLOCK:
            Locked := FALSE;
    else
        if Locked then
            Exit;

        if (Code = Integer(CTRL_OPEN)) and (PresentState = CTRL_CLOSE) then
        begin
            controlledElement.SetConductorClosed(0, FALSE); // Open all phases of active terminal
            PresentState := CTRL_OPEN;
            AppendtoEventLog(Self.FullName(), 'Opened');
        end;
        if (Code = Integer(CTRL_CLOSE)) and (PresentState = CTRL_OPEN) then
        begin
            controlledElement.SetConductorClosed(0, TRUE);    // Close all phases of active terminal
            PresentState := CTRL_CLOSE;
            AppendtoEventLog(Self.FullName(), 'Closed');
        end;
        Armed := FALSE;  // reset the switch
    end;
end;

procedure TSwtControlObj.Sample();
begin
    // push on the Lock command if any at the present time delay
    if LockCommand <> CTRL_NONE then
    begin
        ActiveCircuit.ControlQueue.Push(TimeDelay, LockCommand, 0, Self);
        LockCommand := CTRL_NONE;  // reset the lock command for next time
    end;

    if (CurrentAction <> PresentState) and not Armed then   // we need to operate this switch
    begin
        ActiveCircuit.ControlQueue.Push(TimeDelay, CurrentAction, 0, Self);
        Armed := TRUE;
    end;
end;

procedure TSwtControlObj.Reset();
begin
    if Locked then
        Exit;

    PresentState := NormalState;
    CurrentAction := PresentState;
    Armed := FALSE;
    if controlledElement <> NIL then
    begin
        controlledElement.SetActiveTerminalIdx(ElementTerminal);  // Set active terminal
        if NormalState = CTRL_OPEN then
            controlledElement.SetConductorClosed(0, FALSE)
        else
            controlledElement.SetConductorClosed(0, TRUE);  // Close all phases of active terminal
    end;
end;

procedure TSwtControlObj.SetEnabled(Value: WordBool);
begin
    // Do nothing else besides toggling the flag,
    // we don't need BusNameRedefined from CktElement.pas
    FEnabled := Value;
end;

finalization
    ActionEnum.Free;
    StateEnum.Free;
end.
