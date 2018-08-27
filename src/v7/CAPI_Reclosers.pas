UNIT CAPI_Reclosers;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE Reclosers_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
function Reclosers_Get_Count():Integer;cdecl;
function Reclosers_Get_First():Integer;cdecl;
function Reclosers_Get_Name():PAnsiChar;cdecl;
function Reclosers_Get_Next():Integer;cdecl;
procedure Reclosers_Set_Name(const Value: PAnsiChar);cdecl;
function Reclosers_Get_MonitoredTerm():Integer;cdecl;
procedure Reclosers_Set_MonitoredTerm(Value: Integer);cdecl;
function Reclosers_Get_SwitchedObj():PAnsiChar;cdecl;
procedure Reclosers_Set_SwitchedObj(const Value: PAnsiChar);cdecl;
function Reclosers_Get_MonitoredObj():PAnsiChar;cdecl;
function Reclosers_Get_SwitchedTerm():Integer;cdecl;
procedure Reclosers_Set_MonitoredObj(const Value: PAnsiChar);cdecl;
procedure Reclosers_Set_SwitchedTerm(Value: Integer);cdecl;
function Reclosers_Get_NumFast():Integer;cdecl;
PROCEDURE Reclosers_Get_RecloseIntervals(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
function Reclosers_Get_Shots():Integer;cdecl;
procedure Reclosers_Set_NumFast(Value: Integer);cdecl;
procedure Reclosers_Set_Shots(Value: Integer);cdecl;
function Reclosers_Get_PhaseTrip():Double;cdecl;
procedure Reclosers_Set_PhaseTrip(Value: Double);cdecl;
function Reclosers_Get_GroundInst():Double;cdecl;
function Reclosers_Get_GroundTrip():Double;cdecl;
function Reclosers_Get_PhaseInst():Double;cdecl;
procedure Reclosers_Set_GroundInst(Value: Double);cdecl;
procedure Reclosers_Set_GroundTrip(Value: Double);cdecl;
procedure Reclosers_Set_PhaseInst(Value: Double);cdecl;
procedure Reclosers_Close();cdecl;
procedure Reclosers_Open();cdecl;
function Reclosers_Get_idx():Integer;cdecl;
procedure Reclosers_Set_idx(Value: Integer);cdecl;

IMPLEMENTATION

USES CAPI_Constants, Executive, Sysutils, Recloser, PointerList, DSSGlobals, DSSClassDefs;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('recloser.%s.%s=%s', [TRecloserObj(RecloserClass.GetActiveObj).Name, parm, val]);
  DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
PROCEDURE Reclosers_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  elem: TRecloserObj;
  pList: TPointerList;
  k: Integer;
Begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    IF ActiveCircuit <> Nil THEN
    Begin
        If RecloserClass.ElementList.ListSize > 0 then
        Begin
          pList := RecloserClass.ElementList;
          DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (pList.ListSize -1) + 1);
          k:=0;
          elem := pList.First;
          WHILE elem<>Nil DO Begin
              Result[k] := DSS_CopyStringAsPChar(elem.Name);
              Inc(k);
              elem := pList.next        ;
          End;
        End;
    End;
end;
//------------------------------------------------------------------------------
function Reclosers_Get_Count():Integer;cdecl;
begin
     Result := 0;
     If ActiveCircuit <> Nil Then
        Result := RecloserClass.ElementList.ListSize;
end;
//------------------------------------------------------------------------------
function Reclosers_Get_First():Integer;cdecl;
Var
   pElem : TRecloserObj;
begin
     Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
        pElem := RecloserClass.ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit.ActiveCktElement := pElem;
              Result := RecloserClass.ElementList.ActiveIndex;
          End
          Else pElem := RecloserClass.ElementList.Next;
        Until (Result >0) or (pElem = nil);
     End;
end;
//------------------------------------------------------------------------------
function Reclosers_Get_Name_AnsiString():AnsiString;inline;
Var
  elem: TRecloserObj;
Begin
  Result := '';
  elem := RecloserClass.GetActiveObj;
  If elem <> Nil Then Result := elem.Name;
end;

function Reclosers_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Reclosers_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Reclosers_Get_Next():Integer;cdecl;
Var
   pElem : TRecloserObj;
begin
     Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
        pElem := RecloserClass.ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit.ActiveCktElement := pElem;
              Result := RecloserClass.ElementList.ActiveIndex;
          End
          Else pElem := RecloserClass.ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_Name(const Value: PAnsiChar);cdecl;
// Set element active by name

begin
     If ActiveCircuit <> Nil Then
     Begin
          If RecloserClass.SetActive(Value) Then
          Begin
               ActiveCircuit.ActiveCktElement := RecloserClass.ElementList.Active ;
          End
          Else Begin
              DoSimpleMsg('Recloser "'+ Value +'" Not Found in Active Circuit.', 77003);
          End;
     End;
end;
//------------------------------------------------------------------------------
function Reclosers_Get_MonitoredTerm():Integer;cdecl;
var
  elem: TRecloserObj;
begin
  Result := 0;
  elem := RecloserClass.GetActiveObj  ;
  if elem <> nil then Result := elem.MonitoredElementTerminal ;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_MonitoredTerm(Value: Integer);cdecl;
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.GetActiveObj ;
  if elem <> nil then Set_parameter('monitoredterm', IntToStr(Value));
end;
//------------------------------------------------------------------------------
function Reclosers_Get_SwitchedObj_AnsiString():AnsiString;inline;
var
  elem: TRecloserObj;
begin
  Result := '';
  elem := RecloserClass.ElementList.Active ;
  if elem <> nil then Result := elem.ElementName  ;
end;

function Reclosers_Get_SwitchedObj():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Reclosers_Get_SwitchedObj_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_SwitchedObj(const Value: PAnsiChar);cdecl;
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.GetActiveObj ;
  if elem <> nil then Set_parameter('SwitchedObj', Value);
end;
//------------------------------------------------------------------------------
function Reclosers_Get_MonitoredObj_AnsiString():AnsiString;inline;
var
  elem: TRecloserObj;
begin
  Result := '';
  elem := RecloserClass.GetActiveObj  ;
  if elem <> nil then Result := elem.MonitoredElementName;
end;

function Reclosers_Get_MonitoredObj():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Reclosers_Get_MonitoredObj_AnsiString());
end;
//------------------------------------------------------------------------------
function Reclosers_Get_SwitchedTerm():Integer;cdecl;
var
  elem: TRecloserObj;
begin
  Result := 0;
  elem := RecloserClass.GetActiveObj  ;
  if elem <> nil then Result := elem.ElementTerminal  ;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_MonitoredObj(const Value: PAnsiChar);cdecl;
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.GetActiveObj ;
  if elem <> nil then Set_parameter('monitoredObj', Value);
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_SwitchedTerm(Value: Integer);cdecl;
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.GetActiveObj ;
  if elem <> nil then Set_parameter('SwitchedTerm', IntToStr(Value));
end;
//------------------------------------------------------------------------------
function Reclosers_Get_NumFast():Integer;cdecl;
var
  elem: TRecloserObj;
begin
  Result := 0;
  elem := RecloserClass.ElementList.Active;  ;
  if elem <> nil then Result := elem.NumFast ;
end;
//------------------------------------------------------------------------------
PROCEDURE Reclosers_Get_RecloseIntervals(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
// return reclose intervals in seconds
VAR
  Result: PDoubleArray;
  elem: TRecloserObj;
  i, k: Integer;
Begin
  Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
  Result[0] := -1.0;
  IF ActiveCircuit <> Nil THEN
  Begin
      elem := RecloserClass.ElementList.Active;
      If elem <> Nil Then
      Begin
        DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, (elem.NumReclose-1) + 1);
        k:=0;
        for i := 1 to elem.NumReclose  do
        Begin
            Result[k] := elem.RecloseIntervals ^[i];
            Inc(k);
        End;
      End;
  End;
end;
//------------------------------------------------------------------------------
function Reclosers_Get_Shots():Integer;cdecl;
var
  elem: TRecloserObj;
begin
  Result := 0;
  elem := RecloserClass.ElementList.Active;  ;
  if elem <> nil then Result := elem.NumReclose + 1;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_NumFast(Value: Integer);cdecl;
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.ElementList.Active  ;
  if elem <> nil then Set_parameter('numfast', IntToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_Shots(Value: Integer);cdecl;
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.ElementList.Active  ;
  if elem <> nil then Set_parameter('shots', IntToStr(Value));
end;
//------------------------------------------------------------------------------
function Reclosers_Get_PhaseTrip():Double;cdecl;
var
  elem: TRecloserObj;
begin
  Result := 0;
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Result := elem.PhaseTrip;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_PhaseTrip(Value: Double);cdecl;
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.ElementList.Active  ;
  if elem <> nil then Set_parameter('PhaseTrip', Format('%.g',[Value]));
end;
//------------------------------------------------------------------------------
function Reclosers_Get_GroundInst():Double;cdecl;
var
  elem: TRecloserObj;
begin
  Result := 0;
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Result := elem.GroundInst;
end;
//------------------------------------------------------------------------------
function Reclosers_Get_GroundTrip():Double;cdecl;
var
  elem: TRecloserObj;
begin
  Result := 0;
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Result := elem.GroundTrip;
end;
//------------------------------------------------------------------------------
function Reclosers_Get_PhaseInst():Double;cdecl;
var
  elem: TRecloserObj;
begin
  Result := 0;
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Result := elem.PhaseInst;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_GroundInst(Value: Double);cdecl;
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.ElementList.Active  ;
  if elem <> nil then Set_parameter('GroundInst', Format('%.g',[Value]));
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_GroundTrip(Value: Double);cdecl;
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.ElementList.Active  ;
  if elem <> nil then Set_parameter('GroundTrip', Format('%.g',[Value]));
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_PhaseInst(Value: Double);cdecl;
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.ElementList.Active  ;
  if elem <> nil then Set_parameter('Phaseinst', Format('%.g',[Value]));
end;
//------------------------------------------------------------------------------
procedure Reclosers_Close();cdecl;
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.ElementList.Active  ;
  if elem <> nil then Set_parameter('Action', 'close');
end;
//------------------------------------------------------------------------------
procedure Reclosers_Open();cdecl;
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.ElementList.Active  ;
  if elem <> nil then Set_parameter('Action', 'open');
end;
//------------------------------------------------------------------------------
function Reclosers_Get_idx():Integer;cdecl;
begin
    if ActiveCircuit <> Nil then
       Result := RecloserClass.ElementList.ActiveIndex
    else Result := 0;
end;
//------------------------------------------------------------------------------
procedure Reclosers_Set_idx(Value: Integer);cdecl;
Var
    pRecloser:TRecloserObj;
begin
    if ActiveCircuit <> Nil then   Begin
        pRecloser := RecloserClass.Elementlist.Get(Value);
        If pRecloser <> Nil Then ActiveCircuit.ActiveCktElement := pRecloser;
    End;
end;
//------------------------------------------------------------------------------
END.
