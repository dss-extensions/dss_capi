unit CAPI_Reclosers;

{$inline on}

interface

uses
    CAPI_Utils;

procedure Reclosers_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
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
procedure Reclosers_Get_RecloseIntervals(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
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
    DSSClassDefs,
    DSSClass,
    DSSHelper;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(DSSPrime.ActiveCircuit) then
        exit;
    DSSPrime.SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('recloser.%s.%s=%s', [TRecloserObj(DSSPrime.ActiveCircuit.Reclosers.Active).Name, parm, val]);
    DSSPrime.DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.Reclosers, False);
end;
//------------------------------------------------------------------------------
function Reclosers_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.ActiveCircuit.Reclosers.ListSize;
end;
//------------------------------------------------------------------------------
function Reclosers_Get_First(): Integer; CDECL;
var
    pElem: TRecloserObj;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    pElem := DSSPrime.ActiveCircuit.Reclosers.First;
    if pElem <> NIL then
        repeat
            if pElem.Enabled then
            begin
                DSSPrime.ActiveCircuit.ActiveCktElement := pElem;
                Result := DSSPrime.ActiveCircuit.Reclosers.ActiveIndex;
            end
            else
                pElem := DSSPrime.ActiveCircuit.Reclosers.Next;
        until (Result > 0) or (pElem = NIL);
end;
//------------------------------------------------------------------------------
function Reclosers_Get_Name(): PAnsiChar; CDECL;
var
    elem: TRecloserObj;
begin
    Result := nil;
    if DSSPrime.ActiveCircuit = NIL then Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Result := DSS_GetAsPAnsiChar(elem.Name);
end;
//------------------------------------------------------------------------------
function Reclosers_Get_Next(): Integer; CDECL;
var
    pElem: TRecloserObj;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    pElem := DSSPrime.ActiveCircuit.Reclosers.Next;
    if pElem <> NIL then
        repeat
            if pElem.Enabled then
            begin
                DSSPrime.ActiveCircuit.ActiveCktElement := pElem;
                Result := DSSPrime.ActiveCircuit.Reclosers.ActiveIndex;
            end
            else
                pElem := DSSPrime.ActiveCircuit.Reclosers.Next;
        until (Result > 0) or (pElem = NIL);
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_Name(const Value: PAnsiChar); CDECL;
// Set element active by name

begin
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;

    if DSSPrime.RecloserClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.RecloserClass.ElementList.Active;
        DSSPrime.ActiveCircuit.Reclosers.Get(DSSPrime.RecloserClass.Active);
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'Recloser "' + Value + '" Not Found in Active Circuit.', 77003);
    end;
end;
//------------------------------------------------------------------------------
function Reclosers_Get_MonitoredTerm(): Integer; CDECL;
var
    elem: TRecloserObj;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Result := elem.MonitoredElementTerminal;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_MonitoredTerm(Value: Integer); CDECL;
var
    elem: TRecloserObj;
begin
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Set_parameter('monitoredterm', IntToStr(Value));
end;
//------------------------------------------------------------------------------
function Reclosers_Get_SwitchedObj(): PAnsiChar; CDECL;
var
    elem: TRecloserObj;
begin
    Result := nil;
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Result := DSS_GetAsPAnsiChar(elem.ElementName);
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_SwitchedObj(const Value: PAnsiChar); CDECL;
var
    elem: TRecloserObj;
begin
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Set_parameter('SwitchedObj', Value);
end;
//------------------------------------------------------------------------------
function Reclosers_Get_MonitoredObj(): PAnsiChar; CDECL;
var
    elem: TRecloserObj;
begin
    Result := nil;
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Result := DSS_GetAsPAnsiChar(elem.MonitoredElementName);
end;
//------------------------------------------------------------------------------
function Reclosers_Get_SwitchedTerm(): Integer; CDECL;
var
    elem: TRecloserObj;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Result := elem.ElementTerminal;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_MonitoredObj(const Value: PAnsiChar); CDECL;
var
    elem: TRecloserObj;
begin
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Set_parameter('monitoredObj', Value);
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_SwitchedTerm(Value: Integer); CDECL;
var
    elem: TRecloserObj;
begin
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Set_parameter('SwitchedTerm', IntToStr(Value));
end;
//------------------------------------------------------------------------------
function Reclosers_Get_NumFast(): Integer; CDECL;
var
    elem: TRecloserObj;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Result := elem.NumFast;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Get_RecloseIntervals(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
// return reclose intervals in seconds
var
    Result: PDoubleArray;
    elem: TRecloserObj;
    i, k: Integer;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    Result[0] := -1.0;
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        elem := DSSPrime.ActiveCircuit.Reclosers.Active;
        if elem <> NIL then
        begin
            DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, (elem.NumReclose - 1) + 1);
            k := 0;
            for i := 1 to elem.NumReclose do
            begin
                Result[k] := elem.RecloseIntervals^[i];
                Inc(k);
            end;
        end;
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
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Result := elem.NumReclose + 1;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_NumFast(Value: Integer); CDECL;
var
    elem: TRecloserObj;
begin
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Set_parameter('numfast', IntToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_Shots(Value: Integer); CDECL;
var
    elem: TRecloserObj;
begin
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Set_parameter('shots', IntToStr(Value));
end;
//------------------------------------------------------------------------------
function Reclosers_Get_PhaseTrip(): Double; CDECL;
var
    elem: TRecloserObj;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Result := elem.PhaseTrip;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_PhaseTrip(Value: Double); CDECL;
var
    elem: TRecloserObj;
begin
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Set_parameter('PhaseTrip', Format('%.g', [Value]));
end;
//------------------------------------------------------------------------------
function Reclosers_Get_GroundInst(): Double; CDECL;
var
    elem: TRecloserObj;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Result := elem.GroundInst;
end;
//------------------------------------------------------------------------------
function Reclosers_Get_GroundTrip(): Double; CDECL;
var
    elem: TRecloserObj;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Result := elem.GroundTrip;
end;
//------------------------------------------------------------------------------
function Reclosers_Get_PhaseInst(): Double; CDECL;
var
    elem: TRecloserObj;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Result := elem.PhaseInst;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_GroundInst(Value: Double); CDECL;
var
    elem: TRecloserObj;
begin
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Set_parameter('GroundInst', Format('%.g', [Value]));
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_GroundTrip(Value: Double); CDECL;
var
    elem: TRecloserObj;
begin
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Set_parameter('GroundTrip', Format('%.g', [Value]));
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_PhaseInst(Value: Double); CDECL;
var
    elem: TRecloserObj;
begin
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Set_parameter('Phaseinst', Format('%.g', [Value]));
end;
//------------------------------------------------------------------------------
procedure Reclosers_Close(); CDECL;
var
    elem: TRecloserObj;
begin
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Set_parameter('Action', 'close');
end;
//------------------------------------------------------------------------------
procedure Reclosers_Open(); CDECL;
var
    elem: TRecloserObj;
begin
    if DSSPrime.ActiveCircuit = NIL then 
        Exit;
    elem := DSSPrime.ActiveCircuit.Reclosers.Active;
    if elem <> NIL then
        Set_parameter('Action', 'open');
end;
//------------------------------------------------------------------------------
function Reclosers_Get_idx(): Integer; CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.ActiveCircuit.Reclosers.ActiveIndex
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_idx(Value: Integer); CDECL;
var
    pRecloser: TRecloserObj;
begin
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    pRecloser := DSSPrime.ActiveCircuit.Reclosers.Get(Value);
    if pRecloser = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid Recloser index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pRecloser;
end;
//------------------------------------------------------------------------------
end.
