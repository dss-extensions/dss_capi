unit CAPI_SwtControls;

interface

uses
    CAPI_Utils;

function SwtControls_Get_Action(): Integer; CDECL;
procedure SwtControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure SwtControls_Get_AllNames_GR(); CDECL;
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

function _activeObj(out obj: TSwtControlObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit then
        Exit;
        
    obj := ActiveCircuit.SwtControls.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active SwtControl object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
        
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
    obj: TSwtControlObj;
begin
    if not _activeObj(obj) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('swtcontrol.%s.%s=%s', [obj.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
function SwtControls_Get_Action(): Integer; CDECL;
var
    elem: TSwtControlObj;
begin
    Result := dssActionNone;
    if not _activeObj(elem) then
        Exit;

    case elem.CurrentAction of
        CTRL_OPEN:
            Result := dssActionOpen;
        CTRL_CLOSE:
            Result := dssActionClose;
    end;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    if InvalidCircuit then
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
        Result[0] := DSS_CopyStringAsPChar('NONE');
        Exit;
    end;
    Generic_Get_AllNames(ResultPtr, ResultCount, ActiveCircuit.SwtControls, False);
end;

procedure SwtControls_Get_AllNames_GR(); CDECL;
// Same as SwtControls_Get_AllNames but uses global result (GR) pointers
begin
    SwtControls_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function SwtControls_Get_Delay(): Double; CDECL;
var
    elem: TSwtControlObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    
    Result := elem.TimeDelay;
end;
//------------------------------------------------------------------------------
function SwtControls_Get_First(): Integer; CDECL;
var
    elem: TSwtControlObj;
    lst: TPointerList;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
        
    lst := ActiveCircuit.SwtControls;
    elem := lst.First;
    if elem = NIL then
        Exit;

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
//------------------------------------------------------------------------------
function SwtControls_Get_IsLocked(): Wordbool; CDECL;
var
    elem: TSwtControlObj;
begin
    Result := FALSE;
    if not _activeObj(elem) then
        Exit;

    Result := elem.IsLocked;   // Fixed bug here
end;
//------------------------------------------------------------------------------
function SwtControls_Get_Name(): PAnsiChar; CDECL;
var
    elem: TSwtControlObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;

    Result := DSS_GetAsPAnsiChar(elem.Name);
end;
//------------------------------------------------------------------------------
function SwtControls_Get_Next(): Integer; CDECL;
var
    elem: TSwtControlObj;
    lst: TPointerList;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
        
    lst := ActiveCircuit.SwtControls;
    elem := lst.Next;
    if elem = NIL then
        Exit;
        
    repeat
        if elem.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := elem;
            Result := lst.ActiveIndex;
        end
        else
            elem := lst.Next;
    until (Result > 0) or (elem = NIL);
end;
//------------------------------------------------------------------------------
function SwtControls_Get_SwitchedObj(): PAnsiChar; CDECL;
var
    elem: TSwtControlObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;

    Result := DSS_GetAsPAnsiChar(elem.ElementName);
end;
//------------------------------------------------------------------------------
function SwtControls_Get_SwitchedTerm(): Integer; CDECL;
var
    elem: TSwtControlObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;

    Result := elem.ElementTerminal;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_Action(Value: Integer); CDECL;
var
    elem: TSwtControlObj;
begin
    if not _activeObj(elem) then
        Exit;

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
//------------------------------------------------------------------------------
procedure SwtControls_Set_Delay(Value: Double); CDECL;
var
    elem: TSwtControlObj;
begin
    if not _activeObj(elem) then
        Exit;

    elem.TimeDelay := Value;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_IsLocked(Value: Wordbool); CDECL;
var
    elem: TSwtControlObj;
begin
    if not _activeObj(elem) then
        Exit;

    elem.Locked := Value;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit then
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
    Result := 0;
    if not _activeObj(elem) then
        Exit;

    case elem.NormalState of
        CTRL_OPEN:
            Result := dssActionOpen;
    else
        Result := dssActionClose;
    end;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_NormalState(Value: Integer); CDECL;
var
    elem: TSwtControlObj;
begin
    if not _activeObj(elem) then
        Exit;

    case Value of
        dssActionOpen:
            elem.NormalState := CTRL_OPEN;
    else
        elem.NormalState := CTRL_CLOSE;
    end;
end;
//------------------------------------------------------------------------------
function SwtControls_Get_State(): Integer; CDECL;
var
    elem: TSwtControlObj;
begin
    Result := dssActionNone;
    if not _activeObj(elem) then
        Exit;

    case elem.PresentState of
        CTRL_OPEN:
            Result := dssActionOpen;
        CTRL_CLOSE:
            Result := dssActionClose;
    end;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_State(Value: Integer); CDECL;
var
    elem: TSwtControlObj;
begin
    if not _activeObj(elem) then
        Exit;

    case value of
        dssActionOpen:
            elem.PresentState := CTRL_OPEN;
        dssActionClose:
            elem.PresentState := CTRL_CLOSE;
    end;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Reset(); CDECL;
var
    elem: TSwtControlObj;
begin
    if not _activeObj(elem) then
        Exit;

    elem.Locked := FALSE;
    elem.Reset;
end;
//------------------------------------------------------------------------------
function SwtControls_Get_idx(): Integer; CDECL;
begin
    if InvalidCircuit then
        Result := ActiveCircuit.SwtControls.ActiveIndex
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_idx(Value: Integer); CDECL;
var
    pSwtControl: TSwtControlObj;
begin
    if InvalidCircuit then
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
