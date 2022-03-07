unit CAPI_Reclosers;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure Reclosers_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Reclosers_Get_AllNames_GR(); CDECL;
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
procedure Reclosers_Reset(); CDECL;
function Reclosers_Get_NormalState(): Integer; CDECL;
procedure Reclosers_Set_NormalState(Value: Integer); CDECL;
function Reclosers_Get_State(): Integer; CDECL;
procedure Reclosers_Set_State(Value: Integer); CDECL;

implementation

uses
    CAPI_Constants,
    ControlElem,
    Executive,
    Sysutils,
    Recloser,
    DSSPointerList,
    DSSGlobals,
    DSSClassDefs,
    DSSClass,
    DSSHelper,
    DSSObjectHelper;

type
    TObj = TRecloserObj;

//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.ActiveCircuit.Reclosers.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['Recloser'], 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(DSS: TDSSContext; const idx: Integer; const val: String); overload;
var
    elem: TObj;
begin
    if not _activeObj(DSS, elem) then
        Exit;
    DSS.SolutionAbort := FALSE;  // Reset for commands entered from outside
    elem.ParsePropertyValue(idx, val);
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(DSS: TDSSContext; const idx: Integer; const val: Double); overload;
var
    elem: TObj;
begin
    if not _activeObj(DSS, elem) then
        Exit;
    DSS.SolutionAbort := FALSE;  // Reset for commands entered from outside
    elem.SetDouble(idx, val);
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(DSS: TDSSContext; const idx: Integer; const val: Integer); overload;
var
    elem: TObj;
begin
    if not _activeObj(DSS, elem) then
        Exit;
    DSS.SolutionAbort := FALSE;  // Reset for commands entered from outside
    elem.SetInteger(idx, val);
end;
//------------------------------------------------------------------------------
procedure Reclosers_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.Reclosers, False);
end;

procedure Reclosers_Get_AllNames_GR(); CDECL;
// Same as Reclosers_Get_AllNames but uses global result (GR) pointers
begin
    Reclosers_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function Reclosers_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Reclosers.Count;
end;
//------------------------------------------------------------------------------
function Reclosers_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime, DSSPrime.ActiveCircuit.Reclosers);
end;
//------------------------------------------------------------------------------
function Reclosers_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime, DSSPrime.ActiveCircuit.Reclosers);
end;
//------------------------------------------------------------------------------
function Reclosers_Get_Name(): PAnsiChar; CDECL;
var
    elem: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name);
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_Name(const Value: PAnsiChar); CDECL;
// Set element active by name
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    if DSSPrime.RecloserClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.RecloserClass.ElementList.Active;
        DSSPrime.ActiveCircuit.Reclosers.Get(DSSPrime.RecloserClass.Active);
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'Recloser "%s" not found in Active Circuit.', [Value], 77003);
    end;
end;
//------------------------------------------------------------------------------
function Reclosers_Get_MonitoredTerm(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.MonitoredElementTerminal;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_MonitoredTerm(Value: Integer); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRecloserProp.monitoredterm), Value);
end;
//------------------------------------------------------------------------------
function Reclosers_Get_SwitchedObj(): PAnsiChar; CDECL;
var
    elem: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if elem.ControlledElement <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, AnsiLowerCase(elem.ControlledElement.FullName));
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_SwitchedObj(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRecloserProp.SwitchedObj), Value);
end;
//------------------------------------------------------------------------------
function Reclosers_Get_MonitoredObj(): PAnsiChar; CDECL;
var
    elem: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if elem.MonitoredElement <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, AnsiLowerCase(elem.MonitoredElement.FullName));
end;
//------------------------------------------------------------------------------
function Reclosers_Get_SwitchedTerm(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.ElementTerminal;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_MonitoredObj(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRecloserProp.monitoredObj), Value);
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_SwitchedTerm(Value: Integer); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRecloserProp.SwitchedTerm), Value);
end;
//------------------------------------------------------------------------------
function Reclosers_Get_NumFast(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.NumFast;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Get_RecloseIntervals(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// return reclose intervals in seconds
var
    Result: PDoubleArray0;
    elem: TObj;
    i, k: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
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
    Reclosers_Get_RecloseIntervals(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
function Reclosers_Get_Shots(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.NumReclose + 1;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_NumFast(Value: Integer); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRecloserProp.numfast), Value);
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_Shots(Value: Integer); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRecloserProp.shots), Value);
end;
//------------------------------------------------------------------------------
function Reclosers_Get_PhaseTrip(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.PhaseTrip;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_PhaseTrip(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRecloserProp.PhaseTrip), Value);
end;
//------------------------------------------------------------------------------
function Reclosers_Get_GroundInst(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.GroundInst;
end;
//------------------------------------------------------------------------------
function Reclosers_Get_GroundTrip(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.GroundTrip;
end;
//------------------------------------------------------------------------------
function Reclosers_Get_PhaseInst(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.PhaseInst;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_GroundInst(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRecloserProp.GroundInst), Value);
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_GroundTrip(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRecloserProp.GroundTrip), Value);
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_PhaseInst(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRecloserProp.PhaseInst), Value);
end;
//------------------------------------------------------------------------------
procedure Reclosers_Close(); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRecloserProp.Action), 'close');
end;
//------------------------------------------------------------------------------
procedure Reclosers_Open(); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRecloserProp.Action), 'open');
end;
//------------------------------------------------------------------------------
function Reclosers_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Reclosers.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_idx(Value: Integer); CDECL;
var
    pRecloser: TObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    pRecloser := DSSPrime.ActiveCircuit.Reclosers.Get(Value);
    if pRecloser = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['Recloser', Value], 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pRecloser;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Reset(); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.Reset();
end;
//------------------------------------------------------------------------------
function Reclosers_Get_NormalState(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := Ord(elem.NormalState);
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_NormalState(Value: Integer); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if Value = dssActionOpen then //TODO: use the same enum
    begin
        elem.NormalState := CTRL_OPEN;
        elem.NormalStateSet := True;
    end
    else if Value = dssActionClose then
    begin
        elem.NormalState := CTRL_CLOSE;
        elem.NormalStateSet := True;
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'Invalid Recloser normal state: "%d".', [Value], 656566);
    end;
end;
//------------------------------------------------------------------------------
function Reclosers_Get_State(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := Ord(elem.PresentState);
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_State(Value: Integer); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if Value = dssActionOpen then
        elem.PresentState := CTRL_OPEN
    else if Value = dssActionClose then
        elem.PresentState := CTRL_CLOSE
    else
    begin
        DoSimpleMsg(DSSPrime, 'Invalid Recloser state: "%d".', [Value], 656567);
    end;
end;
//------------------------------------------------------------------------------
end.
