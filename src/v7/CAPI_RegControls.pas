UNIT CAPI_RegControls;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE RegControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
function RegControls_Get_CTPrimary():Double;cdecl;
function RegControls_Get_Delay():Double;cdecl;
function RegControls_Get_First():Integer;cdecl;
function RegControls_Get_ForwardBand():Double;cdecl;
function RegControls_Get_ForwardR():Double;cdecl;
function RegControls_Get_ForwardVreg():Double;cdecl;
function RegControls_Get_ForwardX():Double;cdecl;
function RegControls_Get_IsInverseTime():WordBool;cdecl;
function RegControls_Get_IsReversible():WordBool;cdecl;
function RegControls_Get_MaxTapChange():Integer;cdecl;
function RegControls_Get_MonitoredBus():PAnsiChar;cdecl;
function RegControls_Get_Name():PAnsiChar;cdecl;
function RegControls_Get_Next():Integer;cdecl;
function RegControls_Get_PTratio():Double;cdecl;
function RegControls_Get_ReverseBand():Double;cdecl;
function RegControls_Get_ReverseR():Double;cdecl;
function RegControls_Get_ReverseVreg():Double;cdecl;
function RegControls_Get_ReverseX():Double;cdecl;
function RegControls_Get_TapDelay():Double;cdecl;
function RegControls_Get_TapWinding():Integer;cdecl;
function RegControls_Get_Transformer():PAnsiChar;cdecl;
function RegControls_Get_VoltageLimit():Double;cdecl;
function RegControls_Get_Winding():Integer;cdecl;
function RegControls_Get_TapNumber():Integer;cdecl;
procedure RegControls_Set_CTPrimary(Value: Double);cdecl;
procedure RegControls_Set_Delay(Value: Double);cdecl;
procedure RegControls_Set_ForwardBand(Value: Double);cdecl;
procedure RegControls_Set_ForwardR(Value: Double);cdecl;
procedure RegControls_Set_ForwardVreg(Value: Double);cdecl;
procedure RegControls_Set_ForwardX(Value: Double);cdecl;
procedure RegControls_Set_IsInverseTime(Value: WordBool);cdecl;
procedure RegControls_Set_IsReversible(Value: WordBool);cdecl;
procedure RegControls_Set_MaxTapChange(Value: Integer);cdecl;
procedure RegControls_Set_MonitoredBus(const Value: PAnsiChar);cdecl;
procedure RegControls_Set_Name(const Value: PAnsiChar);cdecl;
procedure RegControls_Set_PTratio(Value: Double);cdecl;
procedure RegControls_Set_ReverseBand(Value: Double);cdecl;
procedure RegControls_Set_ReverseR(Value: Double);cdecl;
procedure RegControls_Set_ReverseVreg(Value: Double);cdecl;
procedure RegControls_Set_ReverseX(Value: Double);cdecl;
procedure RegControls_Set_TapDelay(Value: Double);cdecl;
procedure RegControls_Set_TapWinding(Value: Integer);cdecl;
procedure RegControls_Set_Transformer(const Value: PAnsiChar);cdecl;
procedure RegControls_Set_VoltageLimit(Value: Double);cdecl;
procedure RegControls_Set_Winding(Value: Integer);cdecl;
procedure RegControls_Set_TapNumber(Value: Integer);cdecl;
function RegControls_Get_Count():Integer;cdecl;
procedure RegControls_Reset();cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, Executive, ControlElem, RegControl, SysUtils, PointerList;

function ActiveRegControl: TRegControlObj;
begin
  Result := nil;
  if ActiveCircuit <> Nil then Result := ActiveCircuit.RegControls.Active;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('regcontrol.%s.%s=%s', [ActiveRegControl.Name, parm, val]);
  DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
PROCEDURE RegControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  elem: TRegControlObj;
  lst: TPointerList;
  k: Integer;
Begin
  Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
  Result[0] := DSS_CopyStringAsPChar('NONE');
  IF ActiveCircuit <> Nil THEN WITH ActiveCircuit DO Begin
    lst := RegControls;
    If lst.ListSize > 0 Then Begin
      DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (lst.ListSize-1) + 1);
      k:=0;
      elem := lst.First;
      WHILE elem<>Nil DO Begin
        Result[k] := DSS_CopyStringAsPChar(elem.Name);
        Inc(k);
        elem := lst.Next;
      End;
    End;
  End;
end;
//------------------------------------------------------------------------------
function RegControls_Get_CTPrimary():Double;cdecl;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.CT;
end;
//------------------------------------------------------------------------------
function RegControls_Get_Delay():Double;cdecl;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.InitialDelay;
end;
//------------------------------------------------------------------------------
function RegControls_Get_First():Integer;cdecl;
Var
  elem: TRegControlObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit <> Nil Then begin
    lst := ActiveCircuit.RegControls;
    elem := lst.First;
    If elem <> Nil Then Begin
      Repeat
        If elem.Enabled Then Begin
          ActiveCircuit.ActiveCktElement := elem;
          Result := 1;
        End
        Else elem := lst.Next;
      Until (Result = 1) or (elem = nil);
    End;
  End;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ForwardBand():Double;cdecl;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.BandVoltage;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ForwardR():Double;cdecl;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.LineDropR;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ForwardVreg():Double;cdecl;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.TargetVoltage;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ForwardX():Double;cdecl;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.LineDropX;
end;
//------------------------------------------------------------------------------
function RegControls_Get_IsInverseTime():WordBool;cdecl;
var
  elem: TRegControlObj;
begin
  Result := FALSE;
  elem := ActiveRegControl;
  if elem <> nil then
    if elem.IsInverseTime then Result := TRUE;
end;
//------------------------------------------------------------------------------
function RegControls_Get_IsReversible():WordBool;cdecl;
var
  elem: TRegControlObj;
begin
  Result := FALSE;
  elem := ActiveRegControl;
  if elem <> nil then
    if elem.UseReverseDrop then Result := TRUE;
end;
//------------------------------------------------------------------------------
function RegControls_Get_MaxTapChange():Integer;cdecl;
var
  elem: TRegControlObj;
begin
  Result := 0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.MaxTapChange;
end;
//------------------------------------------------------------------------------
function RegControls_Get_MonitoredBus_AnsiString():AnsiString;inline;
var
  elem: TRegControlObj;
begin
  Result := '';
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.ControlledBusName;
end;

function RegControls_Get_MonitoredBus():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(RegControls_Get_MonitoredBus_AnsiString());
end;
//------------------------------------------------------------------------------
function RegControls_Get_Name_AnsiString():AnsiString;inline;
var
  elem: TRegControlObj;
begin
  Result := '';
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.Name;
end;

function RegControls_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(RegControls_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function RegControls_Get_Next():Integer;cdecl;
Var
  elem: TRegControlObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit <> Nil Then Begin
    lst := ActiveCircuit.RegControls;
    elem := lst.Next;
    if elem <> nil then begin
      Repeat
        If elem.Enabled Then Begin
          ActiveCircuit.ActiveCktElement := elem;
          Result := lst.ActiveIndex;
        End
        Else elem := lst.Next;
      Until (Result > 0) or (elem = nil);
    End
  End;
end;
//------------------------------------------------------------------------------
function RegControls_Get_PTratio():Double;cdecl;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.PT;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ReverseBand():Double;cdecl;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.RevBandVoltage;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ReverseR():Double;cdecl;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.RevLineDropR;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ReverseVreg():Double;cdecl;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.RevTargetVoltage;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ReverseX():Double;cdecl;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.RevLineDropX;
end;
//------------------------------------------------------------------------------
function RegControls_Get_TapDelay():Double;cdecl;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.SubsequentDelay;
end;
//------------------------------------------------------------------------------
function RegControls_Get_TapWinding():Integer;cdecl;
var
  elem: TRegControlObj;
begin
  Result := 0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.TrWinding;  // has the taps
end;
//------------------------------------------------------------------------------
function RegControls_Get_Transformer_AnsiString():AnsiString;inline;
var
  elem: TRegControlObj;
begin
  Result := '';
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.Transformer.Name;
end;

function RegControls_Get_Transformer():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(RegControls_Get_Transformer_AnsiString());
end;
//------------------------------------------------------------------------------
function RegControls_Get_VoltageLimit():Double;cdecl;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.VoltageLimit;
end;
//------------------------------------------------------------------------------
function RegControls_Get_Winding():Integer;cdecl;
var
  elem: TRegControlObj;
begin
  Result := 0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.ElementTerminal;  // monitored winding
end;
//------------------------------------------------------------------------------
function RegControls_Get_TapNumber():Integer;cdecl;
var
  elem: TRegControlObj;
begin
  Result := 0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.TapNum;  // tap number on the controlled-winding of the transformer controlled by this regcontrol
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_CTPrimary(Value: Double);cdecl;
begin
  Set_Parameter ('CTprim', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_Delay(Value: Double);cdecl;
begin
  Set_Parameter ('Delay', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ForwardBand(Value: Double);cdecl;
begin
  Set_Parameter ('Band', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ForwardR(Value: Double);cdecl;
begin
  Set_Parameter ('R', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ForwardVreg(Value: Double);cdecl;
begin
  Set_Parameter ('Vreg', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ForwardX(Value: Double);cdecl;
begin
  Set_Parameter ('X', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_IsInverseTime(Value: WordBool);cdecl;
begin
  if Value = TRUE then
    Set_Parameter ('InverseTime', 'y')
  else
    Set_Parameter ('InverseTime', 'n');
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_IsReversible(Value: WordBool);cdecl;
begin
  if Value = TRUE then
    Set_Parameter ('Reversible', 'y')
  else
    Set_Parameter ('Reversible', 'n');
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_MaxTapChange(Value: Integer);cdecl;
begin
  Set_Parameter ('MaxTapChange', IntToStr (Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_MonitoredBus(const Value: PAnsiChar);cdecl;
begin
  Set_Parameter ('Bus', Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_Name(const Value: PAnsiChar);cdecl;
var
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  elem: TRegControlObj;
  lst: TPointerList;
Begin
  IF ActiveCircuit <> NIL THEN Begin
    lst := ActiveCircuit.RegControls;
    S := Value;  // Convert to Pascal String
    Found := FALSE;
    ActiveSave := lst.ActiveIndex;
    elem := lst.First;
    While elem <> NIL Do Begin
      IF (CompareText(elem.Name, S) = 0) THEN Begin
        ActiveCircuit.ActiveCktElement := elem;
        Found := TRUE;
        Break;
      End;
      elem := lst.Next;
    End;
    IF NOT Found THEN Begin
      DoSimpleMsg('RegControl "'+S+'" Not Found in Active Circuit.', 5003);
      elem := lst.Get(ActiveSave);    // Restore active Load
      ActiveCircuit.ActiveCktElement := elem;
    End;
  End;
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_PTratio(Value: Double);cdecl;
begin
  Set_Parameter ('PTratio', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ReverseBand(Value: Double);cdecl;
begin
  Set_Parameter ('RevBand', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ReverseR(Value: Double);cdecl;
begin
  Set_Parameter ('RevR', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ReverseVreg(Value: Double);cdecl;
begin
  Set_Parameter ('RevVreg', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ReverseX(Value: Double);cdecl;
begin
  Set_Parameter ('RevX', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_TapDelay(Value: Double);cdecl;
begin
  Set_Parameter ('TapDelay', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_TapWinding(Value: Integer);cdecl;
begin
  Set_Parameter ('TapWinding', IntToStr (Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_Transformer(const Value: PAnsiChar);cdecl;
begin
  Set_Parameter ('Transformer', Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_VoltageLimit(Value: Double);cdecl;
begin
  Set_Parameter ('Vlimit', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_Winding(Value: Integer);cdecl;
begin
  Set_Parameter ('Winding', IntToStr (Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_TapNumber(Value: Integer);cdecl;
begin
  Set_Parameter ('TapNum', IntToStr (Value));
end;
//------------------------------------------------------------------------------
function RegControls_Get_Count():Integer;cdecl;
begin
  If Assigned(Activecircuit) Then
     Result := ActiveCircuit.RegControls.ListSize;
end;
//------------------------------------------------------------------------------
procedure RegControls_Reset();cdecl;
var
  elem: TRegControlObj;
begin
  elem   := ActiveRegControl;
  if elem <> nil then begin
      elem.Reset;
  end;

end;
//------------------------------------------------------------------------------
END.
