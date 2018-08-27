UNIT CAPI_Fuses;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE Fuses_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
function Fuses_Get_Count():Integer;cdecl;
function Fuses_Get_First():Integer;cdecl;
function Fuses_Get_Name():PAnsiChar;cdecl;
function Fuses_Get_Next():Integer;cdecl;
procedure Fuses_Set_Name(const Value: PAnsiChar);cdecl;
function Fuses_Get_MonitoredObj():PAnsiChar;cdecl;
function Fuses_Get_MonitoredTerm():Integer;cdecl;
function Fuses_Get_SwitchedObj():PAnsiChar;cdecl;
procedure Fuses_Set_MonitoredObj(const Value: PAnsiChar);cdecl;
procedure Fuses_Set_MonitoredTerm(Value: Integer);cdecl;
procedure Fuses_Set_SwitchedObj(const Value: PAnsiChar);cdecl;
function Fuses_Get_SwitchedTerm():Integer;cdecl;
procedure Fuses_Set_SwitchedTerm(Value: Integer);cdecl;
function Fuses_Get_TCCcurve():PAnsiChar;cdecl;
procedure Fuses_Set_TCCcurve(const Value: PAnsiChar);cdecl;
function Fuses_Get_RatedCurrent():Double;cdecl;
procedure Fuses_Set_RatedCurrent(Value: Double);cdecl;
function Fuses_Get_Delay():Double;cdecl;
procedure Fuses_Open();cdecl;
procedure Fuses_Close();cdecl;
procedure Fuses_Set_Delay(Value: Double);cdecl;
function Fuses_IsBlown():WordBool;cdecl;
function Fuses_Get_idx():Integer;cdecl;
procedure Fuses_Set_idx(Value: Integer);cdecl;
function Fuses_Get_NumPhases():Integer;cdecl;

IMPLEMENTATION

USES CAPI_Constants, Executive, Sysutils, Fuse, Pointerlist, DSSGlobals;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit[ActiveActor]) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('Fuse.%s.%s=%s', [TFuseObj(FuseClass.GetActiveObj).Name, parm, val]);
  DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
PROCEDURE Fuses_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  elem: TFuseObj;
  pList: TPointerList;
  k: Integer;

Begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
        If FuseClass.ElementList.ListSize > 0 then
        Begin
          pList := FuseClass.ElementList;
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
function Fuses_Get_Count():Integer;cdecl;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
        Result := FuseClass.ElementList.ListSize;
end;
//------------------------------------------------------------------------------
function Fuses_Get_First():Integer;cdecl;
Var
   pElem : TFuseObj;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := FuseClass.ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := 1;
          End
          Else pElem := FuseClass.ElementList.Next;
        Until (Result = 1) or (pElem = nil);
     End;
end;
//------------------------------------------------------------------------------
function Fuses_Get_Name_AnsiString():AnsiString;inline;
Var
  elem: TFuseObj;
Begin
  Result := '';
  elem := FuseClass.GetActiveObj;
  If elem <> Nil Then Result := elem.Name;
end;

function Fuses_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Fuses_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Fuses_Get_Next():Integer;cdecl;
Var
   pElem : TFuseObj;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := FuseClass.ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := FuseClass.ElementList.ActiveIndex;
          End
          Else pElem := FuseClass.ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_Name(const Value: PAnsiChar);cdecl;
// Set element active by name

begin
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
          If FuseClass.SetActive(Value) Then
          Begin
               ActiveCircuit[ActiveActor].ActiveCktElement := FuseClass.ElementList.Active ;
          End
          Else Begin
              DoSimpleMsg('Fuse "'+ Value +'" Not Found in Active Circuit.', 77003);
          End;
     End;
end;
//------------------------------------------------------------------------------
function Fuses_Get_MonitoredObj_AnsiString():AnsiString;inline;
var
  elem: TFuseObj;
begin
  Result := '';
  elem := FuseClass.GetActiveObj  ;
  if elem <> nil then Result := elem.MonitoredElementName;
end;

function Fuses_Get_MonitoredObj():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Fuses_Get_MonitoredObj_AnsiString());
end;
//------------------------------------------------------------------------------
function Fuses_Get_MonitoredTerm():Integer;cdecl;
var
  elem: TFuseObj;
begin
  Result := 0;
  elem := FuseClass.GetActiveObj  ;
  if elem <> nil then Result := elem.MonitoredElementTerminal ;
end;
//------------------------------------------------------------------------------
function Fuses_Get_SwitchedObj_AnsiString():AnsiString;inline;
var
  elem: TFuseObj;
begin
  Result := '';
  elem := FuseClass.ElementList.Active ;
  if elem <> nil then Result := elem.ElementName  ;
end;

function Fuses_Get_SwitchedObj():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Fuses_Get_SwitchedObj_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_MonitoredObj(const Value: PAnsiChar);cdecl;
var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Set_parameter('monitoredObj', Value);
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_MonitoredTerm(Value: Integer);cdecl;
var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Set_parameter('monitoredterm', IntToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_SwitchedObj(const Value: PAnsiChar);cdecl;
Var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Set_parameter('SwitchedObj', Value);
end;
//------------------------------------------------------------------------------
function Fuses_Get_SwitchedTerm():Integer;cdecl;
var
  elem: TFuseObj;
begin
  Result := 0;
  elem := FuseClass.GetActiveObj  ;
  if elem <> nil then Result := elem.ElementTerminal  ;
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_SwitchedTerm(Value: Integer);cdecl;
Var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Set_parameter('SwitchedTerm', IntToStr(Value));
end;
//------------------------------------------------------------------------------
function Fuses_Get_TCCcurve_AnsiString():AnsiString;inline;
Var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Result := elem.FuseCurve.Name
  else Result := 'No Fuse Active!';
end;

function Fuses_Get_TCCcurve():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Fuses_Get_TCCcurve_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_TCCcurve(const Value: PAnsiChar);cdecl;
Var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Set_parameter('FuseCurve', Value);
end;
//------------------------------------------------------------------------------
function Fuses_Get_RatedCurrent():Double;cdecl;
Var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Result := elem.RatedCurrent
  else Result := -1.0;
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_RatedCurrent(Value: Double);cdecl;
Var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Set_parameter('RatedCurrent', Format('%.8g ',[Value]));
end;
//------------------------------------------------------------------------------
function Fuses_Get_Delay():Double;cdecl;
Var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Result := elem.DelayTime
  else Result := -1.0;
end;
//------------------------------------------------------------------------------
procedure Fuses_Open();cdecl;
Var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then elem.ControlledElement.Closed [0,ActiveActor] := FALSE; // Open all phases
end;
//------------------------------------------------------------------------------
procedure Fuses_Close();cdecl;
Var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then elem.Reset;
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_Delay(Value: Double);cdecl;
Var
  elem: TFuseObj;
begin
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Set_parameter('Delay', Format('%.8g ',[Value]));
end;
//------------------------------------------------------------------------------
function Fuses_IsBlown():WordBool;cdecl;
// Return TRUE if any phase blown
Var
  elem : TFuseObj;
  i : Integer;
begin
  Result :=FALSE;
  elem := FuseClass.GetActiveObj ;
  if elem <> nil then Begin
      for i := 1 to elem.nphases do
          If not elem.ControlledElement.Closed[i,ActiveActor] Then Result := TRUE;
  End;
end;
//------------------------------------------------------------------------------
function Fuses_Get_idx():Integer;cdecl;
begin
    if ActiveCircuit[ActiveActor] <> Nil then
       Result := FuseClass.ElementList.ActiveIndex
    else Result := 0;
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_idx(Value: Integer);cdecl;
Var
    pFuse:TFuseObj;
begin
    if ActiveCircuit[ActiveActor] <> Nil then   Begin
        pFuse := FuseClass.Elementlist.Get(Value);
        If pFuse <> Nil Then ActiveCircuit[ActiveActor].ActiveCktElement := pFuse;
    End;
end;
//------------------------------------------------------------------------------
function Fuses_Get_NumPhases():Integer;cdecl;
Var
    pFuse:TFuseObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> Nil then   Begin
        pFuse := FuseClass.GetActiveObj ;
        If pFuse <> Nil Then Result := pFuse.NPhases ;
    End;
end;
//------------------------------------------------------------------------------
END.
