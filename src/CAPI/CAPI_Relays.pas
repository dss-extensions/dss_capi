unit CAPI_Relays;

interface

uses
    CAPI_Utils;

procedure Relays_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
function Relays_Get_Count(): Integer; CDECL;
function Relays_Get_First(): Integer; CDECL;
function Relays_Get_Name(): PAnsiChar; CDECL;
function Relays_Get_Next(): Integer; CDECL;
procedure Relays_Set_Name(const Value: PAnsiChar); CDECL;
function Relays_Get_MonitoredObj(): PAnsiChar; CDECL;
procedure Relays_Set_MonitoredObj(const Value: PAnsiChar); CDECL;
function Relays_Get_MonitoredTerm(): Integer; CDECL;
function Relays_Get_SwitchedObj(): PAnsiChar; CDECL;
procedure Relays_Set_MonitoredTerm(Value: Integer); CDECL;
procedure Relays_Set_SwitchedObj(const Value: PAnsiChar); CDECL;
function Relays_Get_SwitchedTerm(): Integer; CDECL;
procedure Relays_Set_SwitchedTerm(Value: Integer); CDECL;
function Relays_Get_idx(): Integer; CDECL;
procedure Relays_Set_idx(Value: Integer); CDECL;

implementation

uses
    CAPI_Constants,
    Executive,
    Relay,
    Circuit,
    DSSGlobals,
    Sysutils,
    Pointerlist;

//------------------------------------------------------------------------------
function _activeObj(out obj: TRelayObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit then
        Exit;
    
    obj := ActiveCircuit.Relays.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active Relay object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
    elem: TRelayObj;
begin
    if not _activeObj(elem) then
        Exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('Relay.%s.%s=%s', [elem.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
procedure Relays_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, ActiveCircuit.Relays, False);
end;
//------------------------------------------------------------------------------
function Relays_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Relays.ListSize;
end;
//------------------------------------------------------------------------------
function Relays_Get_First(): Integer; CDECL;
var
    pElem: TRelayObj;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;

    pElem := ActiveCircuit.Relays.First;
    if pElem = NIL then
        Exit;

    repeat
        if pElem.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := pElem;
            Result := 1;
        end
        else
            pElem := ActiveCircuit.Relays.Next;
    until (Result = 1) or (pElem = NIL);
end;
//------------------------------------------------------------------------------
function Relays_Get_Name(): PAnsiChar; CDECL;
var
    elem: TRelayObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem.Name);
end;
//------------------------------------------------------------------------------
function Relays_Get_Next(): Integer; CDECL;
var
    pElem: TRelayObj;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    pElem := ActiveCircuit.Relays.Next;
    if pElem = NIL then
        Exit;
        
    repeat
        if pElem.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := pElem;
            Result := ActiveCircuit.Relays.ActiveIndex;
        end
        else
            pElem := ActiveCircuit.Relays.Next;
    until (Result > 0) or (pElem = NIL);
end;
//------------------------------------------------------------------------------
procedure Relays_Set_Name(const Value: PAnsiChar); CDECL;
// Set element active by name

begin
    if InvalidCircuit then
        Exit;
    if RelayClass.SetActive(Value) then
    begin
        ActiveCircuit.ActiveCktElement := RelayClass.ElementList.Active;
        ActiveCircuit.Relays.Get(RelayClass.Active);
    end
    else
    begin
        DoSimpleMsg('Relay "' + Value + '" Not Found in Active Circuit.', 77003);
    end;
end;
//------------------------------------------------------------------------------
function Relays_Get_MonitoredObj(): PAnsiChar; CDECL;
var
    elem: TRelayObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem.MonitoredElementName);
end;

//------------------------------------------------------------------------------
procedure Relays_Set_MonitoredObj(const Value: PAnsiChar); CDECL;
begin
    Set_parameter('monitoredObj', Value);
end;
//------------------------------------------------------------------------------
function Relays_Get_MonitoredTerm(): Integer; CDECL;
var
    elem: TRelayObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.MonitoredElementTerminal;
end;
//------------------------------------------------------------------------------
function Relays_Get_SwitchedObj(): PAnsiChar; CDECL;
var
    elem: TRelayObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem.ElementName);
end;

//------------------------------------------------------------------------------
procedure Relays_Set_MonitoredTerm(Value: Integer); CDECL;
begin
    Set_parameter('monitoredterm', IntToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Relays_Set_SwitchedObj(const Value: PAnsiChar); CDECL;
begin
    Set_parameter('SwitchedObj', Value);
end;
//------------------------------------------------------------------------------
function Relays_Get_SwitchedTerm(): Integer; CDECL;
var
    elem: TRelayObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.ElementTerminal;
end;
//------------------------------------------------------------------------------
procedure Relays_Set_SwitchedTerm(Value: Integer); CDECL;
begin
    Set_parameter('SwitchedTerm', IntToStr(Value));
end;
//------------------------------------------------------------------------------
function Relays_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Relays.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Relays_Set_idx(Value: Integer); CDECL;
var
    pRelay: TRelayObj;
begin
    if InvalidCircuit then
        Exit;
    pRelay := ActiveCircuit.Relays.Get(Value);
    if pRelay = NIL then
    begin
        DoSimpleMsg('Invalid Relay index: "' + IntToStr(Value) + '".', 656565);
    end;
    ActiveCircuit.ActiveCktElement := pRelay;
end;
//------------------------------------------------------------------------------
end.
