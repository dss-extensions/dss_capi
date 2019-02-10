unit ImplSwtControls;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl;

type
    TSwtControls = class(TAutoObject, ISwtControls)
    PROTECTED
        function Get_Action: ActionCodes; SAFECALL;
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Delay: Double; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_IsLocked: Wordbool; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_SwitchedObj: Widestring; SAFECALL;
        function Get_SwitchedTerm: Integer; SAFECALL;
        procedure Set_Action(Value: ActionCodes); SAFECALL;
        procedure Set_Delay(Value: Double); SAFECALL;
        procedure Set_IsLocked(Value: Wordbool); SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        procedure Set_SwitchedObj(const Value: Widestring); SAFECALL;
        procedure Set_SwitchedTerm(Value: Integer); SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_NormalState: ActionCodes; SAFECALL;
        procedure Set_NormalState(Value: ActionCodes); SAFECALL;
        function Get_State: ActionCodes; SAFECALL;
        procedure Set_State(Value: ActionCodes); SAFECALL;
        procedure Reset; SAFECALL;

    end;

implementation

uses
    ComServ,
    DSSGlobals,
    Executive,
    ControlElem,
    SwtControl,
    Variants,
    SysUtils,
    PointerList;

function ActiveSwtControl: TSwtControlObj;
begin
    Result := NIL;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].SwtControls.Active;
end;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit[ActiveActor]) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('swtcontrol.%s.%s=%s', [ActiveSwtControl.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;

function TSwtControls.Get_Action: ActionCodes;
var
    elem: TSwtControlObj;
begin
    Result := dssActionNone;
    elem := ActiveSwtControl;
    if elem <> NIL then
    begin
        case elem.CurrentAction of
            CTRL_OPEN:
                Result := dssActionOpen;
            CTRL_CLOSE:
                Result := dssActionClose;
        end;
    end;
end;

function TSwtControls.Get_AllNames: Olevariant;
var
    elem: TSwtControlObj;
    lst: TPointerList;
    k: Integer;
begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if SwtControls.ListSize > 0 then
            begin
                lst := SwtControls;
                Result := VarArrayCreate([0, lst.ListSize - 1], varOleStr);
                k := 0;
                elem := lst.First;
                while elem <> NIL do
                begin
                    Result[k] := elem.Name;
                    Inc(k);
                    elem := lst.Next;
                end;
            end;
end;

function TSwtControls.Get_Delay: Double;
var
    elem: TSwtControlObj;
begin
    Result := 0.0;
    elem := ActiveSwtControl;
    if elem <> NIL then
        Result := elem.TimeDelay;
end;

function TSwtControls.Get_First: Integer;
var
    elem: TSwtControlObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        lst := ActiveCircuit[ActiveActor].SwtControls;
        elem := lst.First;
        if elem <> NIL then
        begin
            repeat
                if elem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                    Result := 1;
                end
                else
                    elem := lst.Next;
            until (Result = 1) or (elem = NIL);
        end;
    end;
end;

function TSwtControls.Get_IsLocked: Wordbool;
var
    elem: TSwtControlObj;
begin
    Result := FALSE;
    elem := ActiveSwtControl;
    if elem <> NIL then
        Result := elem.IsLocked;   // Fixed bug here
end;

function TSwtControls.Get_Name: Widestring;
var
    elem: TSwtControlObj;
begin
    Result := '';
    elem := ActiveSwtControl;
    if elem <> NIL then
        Result := elem.Name;
end;

function TSwtControls.Get_Next: Integer;
var
    elem: TSwtControlObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        lst := ActiveCircuit[ActiveActor].SwtControls;
        elem := lst.Next;
        if elem <> NIL then
        begin
            repeat
                if elem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                    Result := lst.ActiveIndex;
                end
                else
                    elem := lst.Next;
            until (Result > 0) or (elem = NIL);
        end
    end;
end;

function TSwtControls.Get_SwitchedObj: Widestring;
var
    elem: TSwtControlObj;
begin
    Result := '';
    elem := ActiveSwtControl;
    if elem <> NIL then
        Result := elem.ElementName;
end;

function TSwtControls.Get_SwitchedTerm: Integer;
var
    elem: TSwtControlObj;
begin
    Result := 0;
    elem := ActiveSwtControl;
    if elem <> NIL then
        Result := elem.ElementTerminal;
end;

procedure TSwtControls.Set_Action(Value: ActionCodes);
var
    elem: TSwtControlObj;
begin
    elem := ActiveSwtControl;
    if elem <> NIL then
    begin
        case Value of
            dssActionOpen:
                elem.CurrentAction := CTRL_OPEN;
            dssActionClose:
                elem.CurrentAction := CTRL_CLOSE;
            dssActionReset:
                elem.Reset;
            dssActionLock:
                elem.Locked := TRUE;
            dssActionUnlock:
                elem.Locked := FALSE;
        else // TapUp, TapDown, None have no effect
        end;
    // Make sure the NormalState has an initial value  before taking action
        if elem.NormalState = CTRL_NONE then
            case value of
                dssActionOpen:
                    elem.NormalState := CTRL_OPEN;
                dssActionClose:
                    elem.NormalState := CTRL_CLOSE;
            end;
    end;
end;

procedure TSwtControls.Set_Delay(Value: Double);
var
    elem: TSwtControlObj;
begin
    elem := ActiveSwtControl;
    if elem <> NIL then
    begin
        elem.TimeDelay := Value;
    end;
end;

procedure TSwtControls.Set_IsLocked(Value: Wordbool);
var
    elem: TSwtControlObj;
begin
    elem := ActiveSwtControl;
    if elem <> NIL then
    begin
        elem.Locked := Value;
    end;

end;

procedure TSwtControls.Set_Name(const Value: Widestring);
var
    ActiveSave: Integer;
    S: String;
    Found: Boolean;
    elem: TSwtControlObj;
    lst: TPointerList;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        lst := ActiveCircuit[ActiveActor].SwtControls;
        S := Value;  // Convert to Pascal String
        Found := FALSE;
        ActiveSave := lst.ActiveIndex;
        elem := lst.First;
        while elem <> NIL do
        begin
            if (CompareText(elem.Name, S) = 0) then
            begin
                ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                Found := TRUE;
                Break;
            end;
            elem := lst.Next;
        end;
        if not Found then
        begin
            DoSimpleMsg('SwtControl "' + S + '" Not Found in Active Circuit.', 5003);
            elem := lst.Get(ActiveSave);    // Restore active Load
            ActiveCircuit[ActiveActor].ActiveCktElement := elem;
        end;
    end;
end;

procedure TSwtControls.Set_SwitchedObj(const Value: Widestring);
begin
    Set_Parameter('SwitchedObj', Value);
end;

procedure TSwtControls.Set_SwitchedTerm(Value: Integer);
begin
    Set_Parameter('SwitchedTerm', IntToStr(Value));
end;

function TSwtControls.Get_Count: Integer;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        Result := ActiveCircuit[ActiveActor].SwtControls.ListSize;
end;

function TSwtControls.Get_NormalState: ActionCodes;
var
    elem: TSwtControlObj;
begin
    elem := ActiveSwtControl;
    if elem <> NIL then
    begin
        case elem.NormalState of
            CTRL_OPEN:
                Result := dssActionOpen;
        else
            Result := dssActionClose;
        end;
    end;

end;

procedure TSwtControls.Set_NormalState(Value: ActionCodes);
var
    elem: TSwtControlObj;
begin
    elem := ActiveSwtControl;
    if elem <> NIL then
    begin
        case Value of
            dssActionOpen:
                elem.NormalState := CTRL_OPEN;
        else
            elem.NormalState := CTRL_CLOSE;
        end;
    end;
end;

function TSwtControls.Get_State: ActionCodes;
var
    elem: TSwtControlObj;
begin
    Result := dssActionNone;
    elem := ActiveSwtControl;
    if elem <> NIL then
    begin
        case elem.PresentState of
            CTRL_OPEN:
                Result := dssActionOpen;
            CTRL_CLOSE:
                Result := dssActionClose;
        end;
    end;
end;

procedure TSwtControls.Set_State(Value: ActionCodes);
var
    elem: TSwtControlObj;
begin
    elem := ActiveSwtControl;
    if elem <> NIL then
    begin
        case value of
            dssActionOpen:
                elem.PresentState := CTRL_OPEN;
            dssActionClose:
                elem.PresentState := CTRL_CLOSE;
        end;
    end;

end;

procedure TSwtControls.Reset;
var
    elem: TSwtControlObj;
begin
    elem := ActiveSwtControl;
    if elem <> NIL then
    begin
        elem.Locked := FALSE;
        elem.Reset;
    end;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TSwtControls, Class_SwtControls,
        ciInternal, tmApartment);
end.
