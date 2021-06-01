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
    DSSHelper;

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
            DoSimpleMsg(DSS, 'No active SwtControl object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
        
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(DSS: TDSSContext; const parm: String; const val: String);
var
    cmd: String;
    obj: TSwtControlObj;
begin
    if not _activeObj(DSS, obj) then
        exit;
    DSS.SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('swtcontrol.%s.%s=%s', [obj.Name, parm, val]);
    DSS.DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
function SwtControls_Get_Action(): Integer; CDECL;
var
    elem: TSwtControlObj;
begin
    Result := dssActionNone;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    case elem.CurrentAction of
        CTRL_OPEN:
            Result := dssActionOpen;
        CTRL_CLOSE:
            Result := dssActionClose;
    end;
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

    Result := elem.IsLocked;   // Fixed bug here
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

    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.ElementName);
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
        DoSimpleMsg(DSSPrime, 'SwtControl "' + Value + '" Not Found in Active Circuit.', 5003);
    end;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_SwitchedObj(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter(DSSPrime, 'SwitchedObj', Value);
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_SwitchedTerm(Value: Integer); CDECL;
begin
    Set_Parameter(DSSPrime, 'SwitchedTerm', IntToStr(Value));
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
        DoSimpleMsg(DSSPrime, 'Invalid SwtControl index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pSwtControl;
end;
//------------------------------------------------------------------------------
end.
