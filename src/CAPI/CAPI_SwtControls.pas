unit CAPI_SwtControls;

interface

uses
    CAPI_Utils,
    CAPI_Types;

function SwtControls_Get_Action(): Integer; CDECL;
procedure SwtControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure SwtControls_Get_AllNames_GR(); CDECL;
function SwtControls_Get_Delay(): Double; CDECL;
function SwtControls_Get_First(): Integer; CDECL;
function SwtControls_Get_IsLocked(): TAPIBoolean; CDECL;
function SwtControls_Get_Name(): PAnsiChar; CDECL;
function SwtControls_Get_Next(): Integer; CDECL;
function SwtControls_Get_SwitchedObj(): PAnsiChar; CDECL;
function SwtControls_Get_SwitchedTerm(): Integer; CDECL;
procedure SwtControls_Set_Action(Value: Integer); CDECL;
procedure SwtControls_Set_Delay(Value: Double); CDECL;
procedure SwtControls_Set_IsLocked(Value: TAPIBoolean); CDECL;
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
    DSSPointerList,
    DSSClass,
    DSSHelper,
    DSSObjectHelper;

function _activeObj(DSS: TDSSContext; out obj: TSwtControlObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
        
    obj := DSS.ActiveCircuit.SwtControls.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['SwtControl'], 8989);
        end;
        Exit;
    end;
        
    Result := True;
end;
//------------------------------------------------------------------------------
function SwtControls_Get_Action(): Integer; CDECL;
var
    elem: TSwtControlObj;
begin
    Result := dssActionNone;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := Ord(elem.CurrentAction);
end;
//------------------------------------------------------------------------------
procedure SwtControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.SwtControls, False);
end;

procedure SwtControls_Get_AllNames_GR(); CDECL;
// Same as SwtControls_Get_AllNames but uses global result (GR) pointers
begin
    SwtControls_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function SwtControls_Get_Delay(): Double; CDECL;
var
    elem: TSwtControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    
    Result := elem.TimeDelay;
end;
//------------------------------------------------------------------------------
function SwtControls_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime, DSSPrime.ActiveCircuit.SwtControls);
end;
//------------------------------------------------------------------------------
function SwtControls_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime, DSSPrime.ActiveCircuit.SwtControls);
end;
//------------------------------------------------------------------------------
function SwtControls_Get_IsLocked(): TAPIBoolean; CDECL;
var
    elem: TSwtControlObj;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.Locked;   // Fixed bug here
end;
//------------------------------------------------------------------------------
function SwtControls_Get_Name(): PAnsiChar; CDECL;
var
    elem: TSwtControlObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name);
end;
//------------------------------------------------------------------------------
function SwtControls_Get_SwitchedObj(): PAnsiChar; CDECL;
var
    elem: TSwtControlObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if elem.ControlledElement <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, AnsiLowerCase(elem.ControlledElement.FullName));
end;
//------------------------------------------------------------------------------
function SwtControls_Get_SwitchedTerm(): Integer; CDECL;
var
    elem: TSwtControlObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.ElementTerminal;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_Action(Value: Integer); CDECL;
var
    elem: TSwtControlObj;
begin
    if not _activeObj(DSSPrime, elem) then
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
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.TimeDelay := Value;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_IsLocked(Value: TAPIBoolean); CDECL;
var
    elem: TSwtControlObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.Locked := Value;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    if DSSPrime.SwtControlClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.SwtControlClass.ElementList.Active;
        DSSPrime.ActiveCircuit.SwtControls.Get(DSSPrime.SwtControlClass.Active);
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'SwtControl "%s" not found in Active Circuit.', [Value], 5003);
    end;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_SwitchedObj(const Value: PAnsiChar); CDECL;
var
    elem: TSwtControlObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    DSSPrime.SolutionAbort := FALSE;  // Reset for commands entered from outside
    elem.ParsePropertyValue(ord(TSwtControlProp.SwitchedObj), Value);
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_SwitchedTerm(Value: Integer); CDECL;
var
    elem: TSwtControlObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    DSSPrime.SolutionAbort := FALSE;  // Reset for commands entered from outside
    elem.SetInteger(ord(TSwtControlProp.SwitchedTerm), Value);
end;
//------------------------------------------------------------------------------
function SwtControls_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if Assigned(DSSPrime.ActiveCircuit) then
        Result := DSSPrime.ActiveCircuit.SwtControls.Count;
end;
//------------------------------------------------------------------------------
function SwtControls_Get_NormalState(): Integer; CDECL;
var
    elem: TSwtControlObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := Ord(elem.NormalState);
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_NormalState(Value: Integer); CDECL;
var
    elem: TSwtControlObj;
begin
    if not _activeObj(DSSPrime, elem) then
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
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := Ord(elem.PresentState);
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_State(Value: Integer); CDECL;
var
    elem: TSwtControlObj;
begin
    if not _activeObj(DSSPrime, elem) then
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
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.Locked := FALSE;
    elem.Reset;
end;
//------------------------------------------------------------------------------
function SwtControls_Get_idx(): Integer; CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Result := DSSPrime.ActiveCircuit.SwtControls.ActiveIndex
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_idx(Value: Integer); CDECL;
var
    pSwtControl: TSwtControlObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    pSwtControl := DSSPrime.ActiveCircuit.SwtControls.Get(Value);
    if pSwtControl = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['SwtControl', Value], 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pSwtControl;
end;
//------------------------------------------------------------------------------
end.
