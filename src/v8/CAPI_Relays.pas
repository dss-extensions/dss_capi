unit CAPI_Relays;

{$inline on}

interface

uses
    CAPI_Utils;

procedure Relays_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure Relays_Get_AllNames_GR(); CDECL;
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

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit[ActiveActor]) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('Relay.%s.%s=%s', [TRelayObj(ActiveCircuit[ActiveActor].Relays.Active).Name, parm, val]);
    DSSExecutive[ActiveActor].Command := cmd;
end;
//------------------------------------------------------------------------------
procedure Relays_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    elem: TRelayObj;
    lst: TPointerList;
    k: Integer;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    with ActiveCircuit[ActiveActor] do
        if Relays.ListSize > 0 then
        begin
            lst := Relays;
            DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, lst.ListSize);
            k := 0;
            elem := lst.First;
            while elem <> NIL do
            begin
                Result[k] := DSS_CopyStringAsPChar(elem.Name);
                Inc(k);
                elem := lst.Next;
            end;
        end;
end;

procedure Relays_Get_AllNames_GR(); CDECL;
// Same as Relays_Get_AllNames but uses global result (GR) pointers
begin
    Relays_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Relays_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Relays.ListSize;
end;
//------------------------------------------------------------------------------
function Relays_Get_First(): Integer; CDECL;
var
    pElem: TRelayObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    pElem := ActiveCircuit[ActiveActor].Relays.First;
    if pElem <> NIL then
        repeat
            if pElem.Enabled then
            begin
                ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                Result := 1;
            end
            else
                pElem := ActiveCircuit[ActiveActor].Relays.Next;
        until (Result = 1) or (pElem = NIL);
end;
//------------------------------------------------------------------------------
function Relays_Get_Name_AnsiString(): Ansistring; inline;
var
    elem: TRelayObj;
begin
    Result := '';
    elem := ActiveCircuit[ActiveActor].Relays.Active;
    if elem <> NIL then
        Result := elem.Name;
end;

function Relays_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Relays_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Relays_Get_Next(): Integer; CDECL;
var
    pElem: TRelayObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    pElem := ActiveCircuit[ActiveActor].Relays.Next;
    if pElem <> NIL then
        repeat
            if pElem.Enabled then
            begin
                ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                Result := ActiveCircuit[ActiveActor].Relays.ActiveIndex;
            end
            else
                pElem := ActiveCircuit[ActiveActor].Relays.Next;
        until (Result > 0) or (pElem = NIL);
end;
//------------------------------------------------------------------------------
procedure Relays_Set_Name(const Value: PAnsiChar); CDECL;
// Set element active by name

begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    if RelayClass[ActiveActor].SetActive(Value) then
    begin
        ActiveCircuit[ActiveActor].ActiveCktElement := RelayClass[ActiveActor].ElementList.Active;
        ActiveCircuit[ActiveActor].Relays.Get(RelayClass[ActiveActor].Active);
    end
    else
    begin
        DoSimpleMsg('Relay "' + Value + '" Not Found in Active Circuit.', 77003);
    end;
end;
//------------------------------------------------------------------------------
function Relays_Get_MonitoredObj_AnsiString(): Ansistring; inline;
var
    elem: TRelayObj;
begin
    Result := '';
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := ActiveCircuit[ActiveActor].Relays.Active;
    if elem <> NIL then
        Result := elem.MonitoredElementName;
end;

function Relays_Get_MonitoredObj(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Relays_Get_MonitoredObj_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Relays_Set_MonitoredObj(const Value: PAnsiChar); CDECL;
var
    elem: TRelayObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := ActiveCircuit[ActiveActor].Relays.Active;
    if elem <> NIL then
        Set_parameter('monitoredObj', Value);
end;
//------------------------------------------------------------------------------
function Relays_Get_MonitoredTerm(): Integer; CDECL;
var
    elem: TRelayObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := ActiveCircuit[ActiveActor].Relays.Active;
    if elem <> NIL then
        Result := elem.MonitoredElementTerminal;
end;
//------------------------------------------------------------------------------
function Relays_Get_SwitchedObj_AnsiString(): Ansistring; inline;
var
    elem: TRelayObj;
begin
    Result := '';
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := ActiveCircuit[ActiveActor].Relays.Active;
    if elem <> NIL then
        Result := elem.ElementName;
end;

function Relays_Get_SwitchedObj(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Relays_Get_SwitchedObj_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Relays_Set_MonitoredTerm(Value: Integer); CDECL;
var
    elem: TRelayObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := ActiveCircuit[ActiveActor].Relays.Active;
    if elem <> NIL then
        Set_parameter('monitoredterm', IntToStr(Value));

end;
//------------------------------------------------------------------------------
procedure Relays_Set_SwitchedObj(const Value: PAnsiChar); CDECL;
var
    elem: TRelayObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := ActiveCircuit[ActiveActor].Relays.Active;
    if elem <> NIL then
        Set_parameter('SwitchedObj', Value);

end;
//------------------------------------------------------------------------------
function Relays_Get_SwitchedTerm(): Integer; CDECL;
var
    elem: TRelayObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := ActiveCircuit[ActiveActor].Relays.Active;
    if elem <> NIL then
        Result := elem.ElementTerminal;
end;
//------------------------------------------------------------------------------
procedure Relays_Set_SwitchedTerm(Value: Integer); CDECL;
var
    elem: TRelayObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := ActiveCircuit[ActiveActor].Relays.Active;
    if elem <> NIL then
        Set_parameter('SwitchedTerm', IntToStr(Value));
end;
//------------------------------------------------------------------------------
function Relays_Get_idx(): Integer; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Relays.ActiveIndex
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure Relays_Set_idx(Value: Integer); CDECL;
var
    pRelay: TRelayObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    pRelay := ActiveCircuit[ActiveActor].Relays.Get(Value);
    if pRelay = NIL then
    begin
        DoSimpleMsg('Invalid Relay index: "' + IntToStr(Value) + '".', 656565);
    end;
    ActiveCircuit[ActiveActor].ActiveCktElement := pRelay;
end;
//------------------------------------------------------------------------------
end.
