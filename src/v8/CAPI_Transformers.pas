UNIT CAPI_Transformers;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE Transformers_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
function Transformers_Get_First():Integer;cdecl;
function Transformers_Get_IsDelta():WordBool;cdecl;
function Transformers_Get_kV():Double;cdecl;
function Transformers_Get_kVA():Double;cdecl;
function Transformers_Get_MaxTap():Double;cdecl;
function Transformers_Get_MinTap():Double;cdecl;
function Transformers_Get_Name():PAnsiChar;cdecl;
function Transformers_Get_Next():Integer;cdecl;
function Transformers_Get_NumTaps():Integer;cdecl;
function Transformers_Get_NumWindings():Integer;cdecl;
function Transformers_Get_R():Double;cdecl;
function Transformers_Get_Rneut():Double;cdecl;
function Transformers_Get_Tap():Double;cdecl;
function Transformers_Get_Wdg():Integer;cdecl;
function Transformers_Get_XfmrCode():PAnsiChar;cdecl;
function Transformers_Get_Xhl():Double;cdecl;
function Transformers_Get_Xht():Double;cdecl;
function Transformers_Get_Xlt():Double;cdecl;
function Transformers_Get_Xneut():Double;cdecl;
procedure Transformers_Set_IsDelta(Value: WordBool);cdecl;
procedure Transformers_Set_kV(Value: Double);cdecl;
procedure Transformers_Set_kVA(Value: Double);cdecl;
procedure Transformers_Set_MaxTap(Value: Double);cdecl;
procedure Transformers_Set_MinTap(Value: Double);cdecl;
procedure Transformers_Set_Name(const Value: PAnsiChar);cdecl;
procedure Transformers_Set_NumTaps(Value: Integer);cdecl;
procedure Transformers_Set_NumWindings(Value: Integer);cdecl;
procedure Transformers_Set_R(Value: Double);cdecl;
procedure Transformers_Set_Rneut(Value: Double);cdecl;
procedure Transformers_Set_Tap(Value: Double);cdecl;
procedure Transformers_Set_Wdg(Value: Integer);cdecl;
procedure Transformers_Set_XfmrCode(const Value: PAnsiChar);cdecl;
procedure Transformers_Set_Xhl(Value: Double);cdecl;
procedure Transformers_Set_Xht(Value: Double);cdecl;
procedure Transformers_Set_Xlt(Value: Double);cdecl;
procedure Transformers_Set_Xneut(Value: Double);cdecl;
function Transformers_Get_Count():Integer;cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, Executive, Transformer, SysUtils, PointerList;

function ActiveTransformer: TTransfObj;
begin
  Result := nil;
  if ActiveCircuit[ActiveActor] <> Nil then Result := ActiveCircuit[ActiveActor].Transformers.Active;
end;

// assuming the active winding has already been set
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit[ActiveActor]) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('transformer.%s.%s=%s', [ActiveTransformer.Name, parm, val]);
  DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
PROCEDURE Transformers_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  elem: TTransfObj;
  lst: TPointerList;
  k: Integer;
Begin
  Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
  Result[0] := DSS_CopyStringAsPChar('NONE');
  IF ActiveCircuit[ActiveActor] <> Nil THEN WITH ActiveCircuit[ActiveActor] DO
  If Transformers.ListSize > 0 Then
    Begin
      lst := Transformers;
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
//------------------------------------------------------------------------------
function Transformers_Get_First():Integer;cdecl;
Var
  elem: TTransfObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then begin
    lst := ActiveCircuit[ActiveActor].Transformers;
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
function Transformers_Get_IsDelta():WordBool;cdecl;
var
  elem: TTransfObj;
begin
  Result := FALSE;
  elem := ActiveTransformer;
  if elem <> nil then
    if elem.WdgConnection[elem.ActiveWinding] > 0 then Result := TRUE;
end;
//------------------------------------------------------------------------------
function Transformers_Get_kV():Double;cdecl;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.Winding^[elem.ActiveWinding].kvll;
end;
//------------------------------------------------------------------------------
function Transformers_Get_kVA():Double;cdecl;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.WdgKVA[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
function Transformers_Get_MaxTap():Double;cdecl;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.Maxtap[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
function Transformers_Get_MinTap():Double;cdecl;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.Mintap[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
function Transformers_Get_Name_AnsiString():AnsiString;inline;
Var
  elem: TTransfObj;
Begin
  Result := '';
  If ActiveCircuit[ActiveActor] <> Nil Then Begin
    elem := ActiveCircuit[ActiveActor].Transformers.Active;
    If elem <> Nil Then Result := elem.Name;
  End;
end;

function Transformers_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Transformers_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Transformers_Get_Next():Integer;cdecl;
Var
  elem: TTransfObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then Begin
    lst := ActiveCircuit[ActiveActor].Transformers;
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
function Transformers_Get_NumTaps():Integer;cdecl;
var
  elem: TTransfObj;
begin
  Result := 0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.NumTaps[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
function Transformers_Get_NumWindings():Integer;cdecl;
var
  elem: TTransfObj;
begin
  Result := 0;
  elem := ActiveTransformer;
  if elem <> nil then Result := elem.NumberOfWindings;
end;
//------------------------------------------------------------------------------
function Transformers_Get_R():Double;cdecl;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.WdgResistance[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
function Transformers_Get_Rneut():Double;cdecl;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.WdgRneutral[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
function Transformers_Get_Tap():Double;cdecl;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.PresentTap[elem.ActiveWinding,ActiveActor];
end;
//------------------------------------------------------------------------------
function Transformers_Get_Wdg():Integer;cdecl;
var
  elem: TTransfObj;
begin
  Result := 0;
  elem := ActiveTransformer;
  if elem <> nil then Result := elem.ActiveWinding;
end;
//------------------------------------------------------------------------------
function Transformers_Get_XfmrCode_AnsiString():AnsiString;inline;
var
  elem: TTransfObj;
begin
  Result := '';
  elem := ActiveTransformer;
  if elem <> nil then Result := elem.XfmrCode;
end;

function Transformers_Get_XfmrCode():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Transformers_Get_XfmrCode_AnsiString());
end;
//------------------------------------------------------------------------------
function Transformers_Get_Xhl():Double;cdecl;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then Result := elem.XhlVal;
end;
//------------------------------------------------------------------------------
function Transformers_Get_Xht():Double;cdecl;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then Result := elem.XhtVal;
end;
//------------------------------------------------------------------------------
function Transformers_Get_Xlt():Double;cdecl;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then Result := elem.XltVal;
end;
//------------------------------------------------------------------------------
function Transformers_Get_Xneut():Double;cdecl;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.WdgXneutral[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_IsDelta(Value: WordBool);cdecl;
begin
  if Value = TRUE then
    Set_Parameter ('Conn', 'Delta')
  else
    Set_Parameter ('Conn', 'Wye')
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_kV(Value: Double);cdecl;
begin
  Set_Parameter ('kv', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_kVA(Value: Double);cdecl;
begin
  Set_Parameter ('kva', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_MaxTap(Value: Double);cdecl;
begin
  Set_Parameter ('MaxTap', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_MinTap(Value: Double);cdecl;
begin
  Set_Parameter ('MinTap', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_Name(const Value: PAnsiChar);cdecl;
var
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  elem: TTransfObj;
  lst: TPointerList;
Begin
  IF ActiveCircuit[ActiveActor] <> NIL THEN Begin
    lst := ActiveCircuit[ActiveActor].Transformers;
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
      DoSimpleMsg('Transformer "'+S+'" Not Found in Active Circuit.', 5003);
      elem := lst.Get(ActiveSave);    // Restore active Load
      ActiveCircuit[ActiveActor].ActiveCktElement := elem;
    End;
  End;
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_NumTaps(Value: Integer);cdecl;
begin
  Set_Parameter ('NumTaps', IntToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_NumWindings(Value: Integer);cdecl;
var
  elem: TTransfObj;
begin
  elem := ActiveTransformer;
  if elem <> nil then
    elem.SetNumWindings (Value);
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_R(Value: Double);cdecl;
begin
  Set_Parameter ('%R', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_Rneut(Value: Double);cdecl;
begin
  Set_Parameter ('Rneut', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_Tap(Value: Double);cdecl;
begin
  Set_Parameter ('Tap', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_Wdg(Value: Integer);cdecl;
var
  elem: TTransfObj;
begin
  elem := ActiveTransformer;
  if elem <> nil then
    if (value > 0) and (value <= elem.NumberOfWindings) then
      elem.ActiveWinding := Value;
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_XfmrCode(const Value: PAnsiChar);cdecl;
begin
  Set_Parameter ('XfmrCode', Value);
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_Xhl(Value: Double);cdecl;
begin
  Set_Parameter ('Xhl', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_Xht(Value: Double);cdecl;
begin
  Set_Parameter ('Xht', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_Xlt(Value: Double);cdecl;
begin
  Set_Parameter ('Xlt', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_Xneut(Value: Double);cdecl;
begin
  Set_Parameter ('Xneut', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
function Transformers_Get_Count():Integer;cdecl;
begin
     If Assigned(ActiveCircuit[ActiveActor]) Then
          Result := ActiveCircuit[ActiveActor].Transformers.ListSize;
end;
//------------------------------------------------------------------------------
END.
