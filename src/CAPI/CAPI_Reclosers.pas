unit CAPI_Reclosers;

interface

uses
    CAPI_Utils;

procedure Reclosers_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
function Reclosers_Get_Count(): Integer; CDECL;
function Reclosers_Get_First(): Integer; CDECL;
function Reclosers_Get_Name(): PAnsiChar; CDECL;
function Reclosers_Get_Next(): Integer; CDECL;
procedure Reclosers_Set_Name(const Value: PAnsiChar); CDECL;
function Reclosers_Get_MonitoredTerm(): Integer; CDECL;
procedure Reclosers_Set_MonitoredTerm(Value: Integer); CDECL;
function Reclosers_Get_SwitchedObj(): PAnsiChar; CDECL;
procedure Reclosers_Set_SwitchedObj(const Value: PAnsiChar); CDECL;
function Reclosers_Get_MonitoredObj(): PAnsiChar; CDECL;
function Reclosers_Get_SwitchedTerm(): Integer; CDECL;
procedure Reclosers_Set_MonitoredObj(const Value: PAnsiChar); CDECL;
procedure Reclosers_Set_SwitchedTerm(Value: Integer); CDECL;
function Reclosers_Get_NumFast(): Integer; CDECL;
procedure Reclosers_Get_RecloseIntervals(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Reclosers_Get_RecloseIntervals_GR(); CDECL;
function Reclosers_Get_Shots(): Integer; CDECL;
procedure Reclosers_Set_NumFast(Value: Integer); CDECL;
procedure Reclosers_Set_Shots(Value: Integer); CDECL;
function Reclosers_Get_PhaseTrip(): Double; CDECL;
procedure Reclosers_Set_PhaseTrip(Value: Double); CDECL;
function Reclosers_Get_GroundInst(): Double; CDECL;
function Reclosers_Get_GroundTrip(): Double; CDECL;
function Reclosers_Get_PhaseInst(): Double; CDECL;
procedure Reclosers_Set_GroundInst(Value: Double); CDECL;
procedure Reclosers_Set_GroundTrip(Value: Double); CDECL;
procedure Reclosers_Set_PhaseInst(Value: Double); CDECL;
procedure Reclosers_Close(); CDECL;
procedure Reclosers_Open(); CDECL;
function Reclosers_Get_idx(): Integer; CDECL;
procedure Reclosers_Set_idx(Value: Integer); CDECL;

implementation

uses
    CAPI_Constants,
    Executive,
    Sysutils,
    Recloser,
    PointerList,
    DSSGlobals,
    DSSClassDefs;

//------------------------------------------------------------------------------
function _activeObj(out obj: TRecloserObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit then
        Exit;
    
    obj := ActiveCircuit.Reclosers.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active Recloser object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
    elem: TRecloserObj;
begin
    if not _activeObj(elem) then
        Exit;

    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('recloser.%s.%s=%s', [elem.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, ActiveCircuit.Reclosers, False);
end;
//------------------------------------------------------------------------------
function Reclosers_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Reclosers.ListSize;
end;
//------------------------------------------------------------------------------
function Reclosers_Get_First(): Integer; CDECL;
var
    pElem: TRecloserObj;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    pElem := ActiveCircuit.Reclosers.First;
    if pElem = NIL then
        Exit;
        
    repeat
        if pElem.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := pElem;
            Result := ActiveCircuit.Reclosers.ActiveIndex;
        end
        else
            pElem := ActiveCircuit.Reclosers.Next;
    until (Result > 0) or (pElem = NIL);
end;
//------------------------------------------------------------------------------
function Reclosers_Get_Name(): PAnsiChar; CDECL;
var
    elem: TRecloserObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem.Name);
end;
//------------------------------------------------------------------------------
function Reclosers_Get_Next(): Integer; CDECL;
var
    pElem: TRecloserObj;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    pElem := ActiveCircuit.Reclosers.Next;
    if pElem = NIL then Exit;
    repeat
        if pElem.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := pElem;
            Result := ActiveCircuit.Reclosers.ActiveIndex;
        end
        else
            pElem := ActiveCircuit.Reclosers.Next;
    until (Result > 0) or (pElem = NIL);
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_Name(const Value: PAnsiChar); CDECL;
// Set element active by name
begin
    if InvalidCircuit then
        Exit;
    if RecloserClass.SetActive(Value) then
    begin
        ActiveCircuit.ActiveCktElement := RecloserClass.ElementList.Active;
        ActiveCircuit.Reclosers.Get(RecloserClass.Active);
    end
    else
    begin
        DoSimpleMsg('Recloser "' + Value + '" Not Found in Active Circuit.', 77003);
    end;
end;
//------------------------------------------------------------------------------
function Reclosers_Get_MonitoredTerm(): Integer; CDECL;
var
    elem: TRecloserObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.MonitoredElementTerminal;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_MonitoredTerm(Value: Integer); CDECL;
begin
    Set_parameter('monitoredterm', IntToStr(Value));
end;
//------------------------------------------------------------------------------
function Reclosers_Get_SwitchedObj(): PAnsiChar; CDECL;
var
    elem: TRecloserObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem.ElementName);
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_SwitchedObj(const Value: PAnsiChar); CDECL;
begin
    Set_parameter('SwitchedObj', Value);
end;
//------------------------------------------------------------------------------
function Reclosers_Get_MonitoredObj(): PAnsiChar; CDECL;
var
    elem: TRecloserObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem.MonitoredElementName);
end;
//------------------------------------------------------------------------------
function Reclosers_Get_SwitchedTerm(): Integer; CDECL;
var
    elem: TRecloserObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.ElementTerminal;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_MonitoredObj(const Value: PAnsiChar); CDECL;
begin
    Set_parameter('monitoredObj', Value);
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_SwitchedTerm(Value: Integer); CDECL;
begin
    Set_parameter('SwitchedTerm', IntToStr(Value));
end;
//------------------------------------------------------------------------------
function Reclosers_Get_NumFast(): Integer; CDECL;
var
    elem: TRecloserObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.NumFast;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Get_RecloseIntervals(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// return reclose intervals in seconds
var
    Result: PDoubleArray;
    elem: TRecloserObj;
    i, k: Integer;
begin
    if not _activeObj(elem) then
    begin
        DefaultResult(ResultPtr, ResultCount, -1.0);
        Exit;
    end;
    
    DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, elem.NumReclose);
    k := 0;
    for i := 1 to elem.NumReclose do
    begin
        Result[k] := elem.RecloseIntervals^[i];
        Inc(k);
    end;
end;

procedure Reclosers_Get_RecloseIntervals_GR(); CDECL;
// Same as Reclosers_Get_RecloseIntervals but uses global result (GR) pointers
begin
    Reclosers_Get_RecloseIntervals(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function Reclosers_Get_Shots(): Integer; CDECL;
var
    elem: TRecloserObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.NumReclose + 1;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_NumFast(Value: Integer); CDECL;
begin
    Set_parameter('numfast', IntToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_Shots(Value: Integer); CDECL;
begin
    Set_parameter('shots', IntToStr(Value));
end;
//------------------------------------------------------------------------------
function Reclosers_Get_PhaseTrip(): Double; CDECL;
var
    elem: TRecloserObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.PhaseTrip;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_PhaseTrip(Value: Double); CDECL;
begin
    Set_parameter('PhaseTrip', Format('%.g', [Value]));
end;
//------------------------------------------------------------------------------
function Reclosers_Get_GroundInst(): Double; CDECL;
var
    elem: TRecloserObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.GroundInst;
end;
//------------------------------------------------------------------------------
function Reclosers_Get_GroundTrip(): Double; CDECL;
var
    elem: TRecloserObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.GroundTrip;
end;
//------------------------------------------------------------------------------
function Reclosers_Get_PhaseInst(): Double; CDECL;
var
    elem: TRecloserObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.PhaseInst;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_GroundInst(Value: Double); CDECL;
begin
    Set_parameter('GroundInst', Format('%.g', [Value]));
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_GroundTrip(Value: Double); CDECL;
begin
    Set_parameter('GroundTrip', Format('%.g', [Value]));
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_PhaseInst(Value: Double); CDECL;
begin
    Set_parameter('Phaseinst', Format('%.g', [Value]));
end;
//------------------------------------------------------------------------------
procedure Reclosers_Close(); CDECL;
begin
    Set_parameter('Action', 'close');
end;
//------------------------------------------------------------------------------
procedure Reclosers_Open(); CDECL;
begin
    Set_parameter('Action', 'open');
end;
//------------------------------------------------------------------------------
function Reclosers_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Reclosers.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_idx(Value: Integer); CDECL;
var
    pRecloser: TRecloserObj;
begin
    if InvalidCircuit then
        Exit;
    pRecloser := ActiveCircuit.Reclosers.Get(Value);
    if pRecloser = NIL then
    begin
        DoSimpleMsg('Invalid Recloser index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    ActiveCircuit.ActiveCktElement := pRecloser;
end;
//------------------------------------------------------------------------------
end.
