UNIT CAPI_Capacitors;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE Capacitors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE Capacitors_Get_AllNames_GR();cdecl;
function Capacitors_Get_First():Integer;cdecl;
function Capacitors_Get_IsDelta():WordBool;cdecl;
function Capacitors_Get_kV():Double;cdecl;
function Capacitors_Get_kvar():Double;cdecl;
function Capacitors_Get_Name():PAnsiChar;cdecl;
function Capacitors_Get_Next():Integer;cdecl;
function Capacitors_Get_NumSteps():Integer;cdecl;
procedure Capacitors_Set_IsDelta(Value: WordBool);cdecl;
procedure Capacitors_Set_kV(Value: Double);cdecl;
procedure Capacitors_Set_kvar(Value: Double);cdecl;
procedure Capacitors_Set_Name(const Value: PAnsiChar);cdecl;
procedure Capacitors_Set_NumSteps(Value: Integer);cdecl;
function Capacitors_Get_Count():Integer;cdecl;
function Capacitors_AddStep():WordBool;cdecl;
function Capacitors_SubtractStep():WordBool;cdecl;
function Capacitors_Get_AvailableSteps():Integer;cdecl;
PROCEDURE Capacitors_Get_States(var ResultPtr: PInteger; ResultCount: PInteger);cdecl;
PROCEDURE Capacitors_Get_States_GR();cdecl;
procedure Capacitors_Set_States(ValuePtr: PInteger; ValueCount: Integer);cdecl;
procedure Capacitors_Open();cdecl;
procedure Capacitors_Close();cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, Executive, Capacitor, SysUtils, PointerList;

function ActiveCapacitor: TCapacitorObj;
begin
  Result := nil;
  if ActiveCircuit[ActiveActor] <> Nil then Result := ActiveCircuit[ActiveActor].ShuntCapacitors.Active;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit[ActiveActor]) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('capacitor.%s.%s=%s', [ActiveCapacitor.Name, parm, val]);
  DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
PROCEDURE Capacitors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  elem: TCapacitorObj;
  lst: TPointerList;
  k: Integer;
Begin
  Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
  Result[0] := DSS_CopyStringAsPChar('NONE');
  IF ActiveCircuit[ActiveActor] <> Nil THEN WITH ActiveCircuit[ActiveActor] DO
  If ShuntCapacitors.ListSize > 0 then
  Begin
    lst := ShuntCapacitors;
    DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (lst.ListSize-1) + 1);
    k:=0;
    elem := lst.First;
    WHILE elem<>Nil DO Begin
      Result[k] := DSS_CopyStringAsPChar(elem.Name);
      Inc(k);
      elem := lst.Next;
    End;
  End;
end;
PROCEDURE Capacitors_Get_AllNames_GR();cdecl;
// Same as Capacitors_Get_AllNames but uses global result (GR) pointers
begin
   Capacitors_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Capacitors_Get_First():Integer;cdecl;
Var
  elem: TCapacitorObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then begin
    lst := ActiveCircuit[ActiveActor].ShuntCapacitors;
    elem := lst.First;
    If elem <> Nil Then Begin
      Repeat
        If elem.Enabled Then Begin
          ActiveCircuit[ActiveActor].ActiveCktElement := elem;
          Result := 1;
        End
        Else elem := lst.Next;
      Until (Result = 1) or (elem = nil);
    End;
  End;
end;
//------------------------------------------------------------------------------
function Capacitors_Get_IsDelta():WordBool;cdecl;
var
  elem: TCapacitorObj;
begin
  Result := FALSE;
  elem := ActiveCapacitor;
  if elem <> nil then
    if elem.Connection > 0 then Result := TRUE;
end;
//------------------------------------------------------------------------------
function Capacitors_Get_kV():Double;cdecl;
var
  elem: TCapacitorObj;
begin
  Result := 0.0;
  elem := ActiveCapacitor;
  if elem <> nil then Result := elem.NomKV;
end;
//------------------------------------------------------------------------------
function Capacitors_Get_kvar():Double;cdecl;
var
  elem: TCapacitorObj;
begin
  Result := 0.0;
  elem := ActiveCapacitor;
  if elem <> nil then Result := elem.Totalkvar;
end;
//------------------------------------------------------------------------------
function Capacitors_Get_Name_AnsiString():AnsiString;inline;
Var
  elem: TCapacitorObj;
Begin
  Result := '';
  elem := ActiveCapacitor;
  If elem <> Nil Then Result := elem.Name;
end;

function Capacitors_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Capacitors_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Capacitors_Get_Next():Integer;cdecl;
Var
  elem: TCapacitorObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then Begin
    lst := ActiveCircuit[ActiveActor].ShuntCapacitors;
    elem := lst.Next;
    if elem <> nil then begin
      Repeat
        If elem.Enabled Then Begin
          ActiveCircuit[ActiveActor].ActiveCktElement := elem;
          Result := lst.ActiveIndex;
        End
        Else elem := lst.Next;
      Until (Result > 0) or (elem = nil);
    End
  End;
end;
//------------------------------------------------------------------------------
function Capacitors_Get_NumSteps():Integer;cdecl;
var
  elem: TCapacitorObj;
begin
  Result := 0;
  elem := ActiveCapacitor;
  if elem <> nil then Result := elem.NumSteps;
end;
//------------------------------------------------------------------------------
procedure Capacitors_Set_IsDelta(Value: WordBool);cdecl;
var
  elem: TCapacitorObj;
begin
  elem := ActiveCapacitor;
  if elem <> nil then elem.Connection := Integer (Value);
end;
//------------------------------------------------------------------------------
procedure Capacitors_Set_kV(Value: Double);cdecl;
begin
  Set_Parameter ('kv', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Capacitors_Set_kvar(Value: Double);cdecl;
begin
  Set_Parameter ('kvar', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Capacitors_Set_Name(const Value: PAnsiChar);cdecl;
var
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  elem: TCapacitorObj;
  lst: TPointerList;
Begin
  IF ActiveCircuit[ActiveActor] <> NIL THEN Begin
    lst := ActiveCircuit[ActiveActor].ShuntCapacitors;
    S := Value;  // Convert to Pascal String
    Found := FALSE;
    ActiveSave := lst.ActiveIndex;
    elem := lst.First;
    While elem <> NIL Do Begin
      IF (CompareText(elem.Name, S) = 0) THEN Begin
        ActiveCircuit[ActiveActor].ActiveCktElement := elem;
        Found := TRUE;
        Break;
      End;
      elem := lst.Next;
    End;
    IF NOT Found THEN Begin
      DoSimpleMsg('Capacitor "'+S+'" Not Found in Active Circuit.', 5003);
      elem := lst.Get(ActiveSave);    // Restore active Capacitor
      ActiveCircuit[ActiveActor].ActiveCktElement := elem;
    End;
  End;
end;
//------------------------------------------------------------------------------
procedure Capacitors_Set_NumSteps(Value: Integer);cdecl;
begin
  Set_Parameter ('numsteps', IntToStr (Value));
end;
//------------------------------------------------------------------------------
function Capacitors_Get_Count():Integer;cdecl;
begin
     If Assigned(ActiveCircuit[ActiveActor]) Then
          Result := ActiveCircuit[ActiveActor].ShuntCapacitors.ListSize;
end;
//------------------------------------------------------------------------------
function Capacitors_AddStep():WordBool;cdecl;
var
  elem: TCapacitorObj;
begin
  elem := ActiveCapacitor;
  if elem <> nil then Result := elem.AddStep(ActiveActor);
end;
//------------------------------------------------------------------------------
function Capacitors_SubtractStep():WordBool;cdecl;
var
  elem: TCapacitorObj;
begin
  elem := ActiveCapacitor;
  if elem <> nil then Result := elem.SubtractStep(ActiveActor);

end;
//------------------------------------------------------------------------------
function Capacitors_Get_AvailableSteps():Integer;cdecl;
var
  elem: TCapacitorObj;
begin
  elem := ActiveCapacitor;
  if elem <> nil then Result := elem.AvailableSteps ;
end;
//------------------------------------------------------------------------------
PROCEDURE Capacitors_Get_States(var ResultPtr: PInteger; ResultCount: PInteger);cdecl;
VAR
  Result: PIntegerArray;
  elem: TCapacitorObj;
  i, k: Integer;
Begin
  Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (0) + 1);
  Result[0] := -1;     // error code
  IF ActiveCircuit[ActiveActor] <> Nil THEN
  Begin
      Elem := ActiveCapacitor;
      If Elem <> nil Then
      Begin
        DSS_RecreateArray_PInteger(Result, ResultPtr, ResultCount, (elem.NumSteps  -1) + 1);
        k:=0;
        for i:= 1 to elem.Numsteps DO Begin
            Result[k] := elem.States[i,ActiveActor];
            Inc(k);
        End;
      End;
  End;

end;
PROCEDURE Capacitors_Get_States_GR();cdecl;
// Same as Capacitors_Get_States but uses global result (GR) pointers
begin
   Capacitors_Get_States(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
procedure Capacitors_Set_States(ValuePtr: PInteger; ValueCount: Integer);cdecl;
VAR
  Value: PIntegerArray;
  elem:TCapacitorObj;
  i, k, LoopLimit: Integer;

begin
    Value := PIntegerArray(ValuePtr);
    elem := ActiveCapacitor;
    If elem <> nil Then
    Begin
         // allocate space based on present value of NumSteps
         // setting NumSteps allocates the memory
         // only put as many elements as proviced up to nZIPV

         LoopLimit := (ValueCount - 1);
         If (LoopLimit - (0) + 1) > elem.NumSteps  Then   LoopLimit :=  (0) + elem.NumSteps -1;

         k := 1;
         for i := (0) to LoopLimit do
         Begin
             elem.States[k,ActiveActor] := Value[i];
             inc(k);
         End;

         elem.FindLastStepInService;
    End;

end;
//------------------------------------------------------------------------------
procedure Capacitors_Open();cdecl;
// Open all steps of capacitor
Var
    elem:TCapacitorObj;
    i : Integer;
Begin

  IF ActiveCircuit[ActiveActor] <> Nil THEN
   WITH ActiveCircuit[ActiveActor] DO
   Begin
      elem := ActiveCapacitor;
      If elem <> nil THEN
      WITH elem DO
      Begin
        for i := 1 to NumSteps  do  States[i,ActiveActor] := 0;   // open all steps
      End;
   End;

end;
//------------------------------------------------------------------------------
procedure Capacitors_Close();cdecl;
Var
    elem:TCapacitorObj;
    i : Integer;
Begin

  IF ActiveCircuit[ActiveActor] <> Nil THEN
   WITH ActiveCircuit[ActiveActor] DO
   Begin
      elem := ActiveCapacitor;
      If elem <> nil THEN
      WITH elem DO
      Begin
        ActiveTerminal := Terminals^[1];  // make sure terminal 1 is closed
        Closed[0,ActiveActor] := TRUE;    // closes all phases
        for i := 1 to NumSteps  do  States[i,ActiveActor] := 1;
      End;
   End;

end;
//------------------------------------------------------------------------------
END.
