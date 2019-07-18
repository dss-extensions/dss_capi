unit CAPI_SwtControls;

{$inline on}

interface

uses
    CAPI_Utils;

function SwtControls_Get_Action(): Integer; CDECL;
procedure SwtControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
function SwtControls_Get_Delay(): Double; CDECL;
function SwtControls_Get_First(): Integer; CDECL;
function SwtControls_Get_IsLocked(): Wordbool; CDECL;
function SwtControls_Get_Name(): PAnsiChar; CDECL;
function SwtControls_Get_Next(): Integer; CDECL;
function SwtControls_Get_SwitchedObj(): PAnsiChar; CDECL;
function SwtControls_Get_SwitchedTerm(): Integer; CDECL;
procedure SwtControls_Set_Action(Value: Integer); CDECL;
procedure SwtControls_Set_Delay(Value: Double); CDECL;
procedure SwtControls_Set_IsLocked(Value: Wordbool); CDECL;
procedure SwtControls_Set_Name(const Value: PAnsiChar); CDECL;
procedure SwtControls_Set_SwitchedObj(const Value: PAnsiChar); CDECL;
procedure SwtControls_Set_SwitchedTerm(Value: Integer); CDECL;
function SwtControls_Get_Count(): Integer; CDECL;
function SwtControls_Get_NormalState(): Integer; CDECL;
procedure SwtControls_Set_NormalState(Value: Integer); CDECL;
function SwtControls_Get_State(): Integer; CDECL;
procedure SwtControls_Set_State(Value: Integer); CDECL;
procedure SwtControls_Reset(); CDECL;

// API extensions
function SwtControls_Get_idx(): Integer; CDECL;
procedure SwtControls_Set_idx(Value: Integer); CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    Executive,
    ControlElem,
    SwtControl,
    SysUtils,
    PointerList;

function ActiveSwtControl: TSwtControlObj;
begin
    Result := NIL;
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.SwtControls.Active;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('swtcontrol.%s.%s=%s', [ActiveSwtControl.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
function SwtControls_Get_Action(): Integer; CDECL;
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
//------------------------------------------------------------------------------
procedure SwtControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit = NIL then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, ActiveCircuit.SwtControls, False);
end;
//------------------------------------------------------------------------------
function SwtControls_Get_Delay(): Double; CDECL;
var
    elem: TSwtControlObj;
begin
    Result := 0.0;
    elem := ActiveSwtControl;
    if elem <> NIL then
        Result := elem.TimeDelay;
end;
//------------------------------------------------------------------------------
function SwtControls_Get_First(): Integer; CDECL;
var
    elem: TSwtControlObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        lst := ActiveCircuit.SwtControls;
        elem := lst.First;
        if elem <> NIL then
        begin
            repeat
                if elem.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := elem;
                    Result := 1;
                end
                else
                    elem := lst.Next;
            until (Result = 1) or (elem = NIL);
        end;
    end;
end;
//------------------------------------------------------------------------------
function SwtControls_Get_IsLocked(): Wordbool; CDECL;
var
    elem: TSwtControlObj;
begin
    Result := FALSE;
    elem := ActiveSwtControl;
    if elem <> NIL then
        Result := elem.IsLocked;   // Fixed bug here
end;
//------------------------------------------------------------------------------
function SwtControls_Get_Name_AnsiString(): Ansistring; inline;
var
    elem: TSwtControlObj;
begin
    Result := '';
    elem := ActiveSwtControl;
    if elem <> NIL then
        Result := elem.Name;
end;

function SwtControls_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(SwtControls_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function SwtControls_Get_Next(): Integer; CDECL;
var
    elem: TSwtControlObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        lst := ActiveCircuit.SwtControls;
        elem := lst.Next;
        if elem <> NIL then
        begin
            repeat
                if elem.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := elem;
                    Result := lst.ActiveIndex;
                end
                else
                    elem := lst.Next;
            until (Result > 0) or (elem = NIL);
        end
    end;
end;
//------------------------------------------------------------------------------
function SwtControls_Get_SwitchedObj_AnsiString(): Ansistring; inline;
var
    elem: TSwtControlObj;
begin
    Result := '';
    elem := ActiveSwtControl;
    if elem <> NIL then
        Result := elem.ElementName;
end;

function SwtControls_Get_SwitchedObj(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(SwtControls_Get_SwitchedObj_AnsiString());
end;
//------------------------------------------------------------------------------
function SwtControls_Get_SwitchedTerm(): Integer; CDECL;
var
    elem: TSwtControlObj;
begin
    Result := 0;
    elem := ActiveSwtControl;
    if elem <> NIL then
        Result := elem.ElementTerminal;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_Action(Value: Integer); CDECL;
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
//------------------------------------------------------------------------------
procedure SwtControls_Set_Delay(Value: Double); CDECL;
var
    elem: TSwtControlObj;
begin
    elem := ActiveSwtControl;
    if elem <> NIL then
    begin
        elem.TimeDelay := Value;
    end;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_IsLocked(Value: Wordbool); CDECL;
var
    elem: TSwtControlObj;
begin
    elem := ActiveSwtControl;
    if elem <> NIL then
    begin
        elem.Locked := Value;
    end;

end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if ActiveCircuit = NIL then
        Exit;
    if SwtControlClass.SetActive(Value) then
    begin
        ActiveCircuit.ActiveCktElement := SwtControlClass.ElementList.Active;
        ActiveCircuit.SwtControls.Get(SwtControlClass.Active);
    end
    else
    begin
        DoSimpleMsg('SwtControl "' + Value + '" Not Found in Active Circuit.', 5003);
    end;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_SwitchedObj(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter('SwitchedObj', Value);
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_SwitchedTerm(Value: Integer); CDECL;
begin
    Set_Parameter('SwitchedTerm', IntToStr(Value));
end;
//------------------------------------------------------------------------------
function SwtControls_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if Assigned(ActiveCircuit) then
        Result := ActiveCircuit.SwtControls.ListSize;
end;
//------------------------------------------------------------------------------
function SwtControls_Get_NormalState(): Integer; CDECL;
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
//------------------------------------------------------------------------------
procedure SwtControls_Set_NormalState(Value: Integer); CDECL;
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
//------------------------------------------------------------------------------
function SwtControls_Get_State(): Integer; CDECL;
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
//------------------------------------------------------------------------------
procedure SwtControls_Set_State(Value: Integer); CDECL;
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
//------------------------------------------------------------------------------
procedure SwtControls_Reset(); CDECL;
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
//------------------------------------------------------------------------------
function SwtControls_Get_idx(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.SwtControls.ActiveIndex
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_idx(Value: Integer); CDECL;
var
    pSwtControl: TSwtControlObj;
begin
    if ActiveCircuit = NIL then
        Exit;
    pSwtControl := ActiveCircuit.SwtControls.Get(Value);
    if pSwtControl = NIL then
    begin
        DoSimpleMsg('Invalid SwtControl index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    ActiveCircuit.ActiveCktElement := pSwtControl;
end;
//------------------------------------------------------------------------------
end.
