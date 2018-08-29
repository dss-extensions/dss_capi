UNIT CAPI_Relays;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE Relays_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE Relays_Get_AllNames_GR();cdecl;
function Relays_Get_Count():Integer;cdecl;
function Relays_Get_First():Integer;cdecl;
function Relays_Get_Name():PAnsiChar;cdecl;
function Relays_Get_Next():Integer;cdecl;
procedure Relays_Set_Name(const Value: PAnsiChar);cdecl;
function Relays_Get_MonitoredObj():PAnsiChar;cdecl;
procedure Relays_Set_MonitoredObj(const Value: PAnsiChar);cdecl;
function Relays_Get_MonitoredTerm():Integer;cdecl;
function Relays_Get_SwitchedObj():PAnsiChar;cdecl;
procedure Relays_Set_MonitoredTerm(Value: Integer);cdecl;
procedure Relays_Set_SwitchedObj(const Value: PAnsiChar);cdecl;
function Relays_Get_SwitchedTerm():Integer;cdecl;
procedure Relays_Set_SwitchedTerm(Value: Integer);cdecl;
function Relays_Get_idx():Integer;cdecl;
procedure Relays_Set_idx(Value: Integer);cdecl;

IMPLEMENTATION

USES CAPI_Constants, Executive, Relay, Circuit, DSSGlobals, Sysutils, Pointerlist;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('Relay.%s.%s=%s', [TRelayObj(RelayClass.GetActiveObj).Name, parm, val]);
  DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
PROCEDURE Relays_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  elem: TRelayObj;
  pList: TPointerList;
  k: Integer;
Begin
  Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
  Result[0] := DSS_CopyStringAsPChar('NONE');
  IF ActiveCircuit <> Nil THEN
  Begin
      If RelayClass.ElementList.ListSize > 0 then
      Begin
        pList := RelayClass.ElementList;
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
PROCEDURE Relays_Get_AllNames_GR();cdecl;
// Same as Relays_Get_AllNames but uses global result (GR) pointers
begin
   Relays_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Relays_Get_Count():Integer;cdecl;
begin
     Result := 0;
     If ActiveCircuit <> Nil Then
        Result := RelayClass.ElementList.ListSize;
end;
//------------------------------------------------------------------------------
function Relays_Get_First():Integer;cdecl;
Var
   pElem : TRelayObj;
begin
     Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
        pElem := RelayClass.ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit.ActiveCktElement := pElem;
              Result := 1;
          End
          Else pElem := RelayClass.ElementList.Next;
        Until (Result = 1) or (pElem = nil);
     End;
end;
//------------------------------------------------------------------------------
function Relays_Get_Name_AnsiString():AnsiString;inline;
Var
  elem: TRelayObj;
Begin
  Result := '';
  elem := RelayClass.GetActiveObj;
  If elem <> Nil Then Result := elem.Name;
end;

function Relays_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Relays_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Relays_Get_Next():Integer;cdecl;
Var
   pElem : TRelayObj;
begin
     Result := 0;
     If ActiveCircuit <> Nil Then
     Begin
        pElem := RelayClass.ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit.ActiveCktElement := pElem;
              Result := RelayClass.ElementList.ActiveIndex;
          End
          Else pElem := RelayClass.ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
end;
//------------------------------------------------------------------------------
procedure Relays_Set_Name(const Value: PAnsiChar);cdecl;
// Set element active by name

begin
     If ActiveCircuit <> Nil Then
     Begin
          If RelayClass.SetActive(Value) Then
          Begin
               ActiveCircuit.ActiveCktElement := RelayClass.ElementList.Active ;
          End
          Else Begin
              DoSimpleMsg('Relay "'+ Value +'" Not Found in Active Circuit.', 77003);
          End;
     End;
end;
//------------------------------------------------------------------------------
function Relays_Get_MonitoredObj_AnsiString():AnsiString;inline;
var
  elem: TRelayObj;
begin
  Result := '';
  elem := RelayClass.GetActiveObj  ;
  if elem <> nil then Result := elem.MonitoredElementName;
end;

function Relays_Get_MonitoredObj():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Relays_Get_MonitoredObj_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Relays_Set_MonitoredObj(const Value: PAnsiChar);cdecl;
var
  elem: TRelayObj;
begin
  elem := RelayClass.GetActiveObj ;
  if elem <> nil then Set_parameter('monitoredObj', Value);
end;
//------------------------------------------------------------------------------
function Relays_Get_MonitoredTerm():Integer;cdecl;
var
  elem: TRelayObj;
begin
  Result := 0;
  elem := RelayClass.GetActiveObj  ;
  if elem <> nil then Result := elem.MonitoredElementTerminal ;
end;
//------------------------------------------------------------------------------
function Relays_Get_SwitchedObj_AnsiString():AnsiString;inline;
var
  elem: TRelayObj;
begin
  Result := '';
  elem := RelayClass.ElementList.Active ;
  if elem <> nil then Result := elem.ElementName  ;

end;

function Relays_Get_SwitchedObj():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Relays_Get_SwitchedObj_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Relays_Set_MonitoredTerm(Value: Integer);cdecl;
var
  elem: TRelayObj;
begin
  elem := RelayClass.GetActiveObj ;
  if elem <> nil then Set_parameter('monitoredterm', IntToStr(Value));

end;
//------------------------------------------------------------------------------
procedure Relays_Set_SwitchedObj(const Value: PAnsiChar);cdecl;
var
  elem: TRelayObj;
begin
  elem := RelayClass.GetActiveObj ;
  if elem <> nil then Set_parameter('SwitchedObj', Value);

end;
//------------------------------------------------------------------------------
function Relays_Get_SwitchedTerm():Integer;cdecl;
var
  elem: TRelayObj;
begin
  Result := 0;
  elem := RelayClass.ElementList.Active   ;
  if elem <> nil then Result := elem.ElementTerminal  ;
end;
//------------------------------------------------------------------------------
procedure Relays_Set_SwitchedTerm(Value: Integer);cdecl;
var
  elem: TRelayObj;
begin
  elem := RelayClass.GetActiveObj ;
  if elem <> nil then Set_parameter('SwitchedTerm', IntToStr(Value));
end;
//------------------------------------------------------------------------------
function Relays_Get_idx():Integer;cdecl;
begin
    if ActiveCircuit <> Nil then
       Result := RelayClass.ElementList.ActiveIndex
    else Result := 0;
end;
//------------------------------------------------------------------------------
procedure Relays_Set_idx(Value: Integer);cdecl;
Var
    pRelay:TRelayObj;
begin
    if ActiveCircuit <> Nil then   Begin
        pRelay := Relayclass.Elementlist.Get(Value);
        If pRelay <> Nil Then ActiveCircuit.ActiveCktElement := pRelay;
    End;
end;
//------------------------------------------------------------------------------
END.
