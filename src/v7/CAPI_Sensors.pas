UNIT CAPI_Sensors;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE Sensors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
function Sensors_Get_Count():Integer;cdecl;
PROCEDURE Sensors_Get_Currents(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
function Sensors_Get_First():Integer;cdecl;
function Sensors_Get_IsDelta():WordBool;cdecl;
PROCEDURE Sensors_Get_kVARS(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Sensors_Get_kVS(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Sensors_Get_kWS(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
function Sensors_Get_MeteredElement():PAnsiChar;cdecl;
function Sensors_Get_MeteredTerminal():Integer;cdecl;
function Sensors_Get_Name():PAnsiChar;cdecl;
function Sensors_Get_Next():Integer;cdecl;
function Sensors_Get_PctError():Double;cdecl;
function Sensors_Get_ReverseDelta():WordBool;cdecl;
function Sensors_Get_Weight():Double;cdecl;
procedure Sensors_Reset();cdecl;
procedure Sensors_ResetAll();cdecl;
procedure Sensors_Set_Currents(ValuePtr: PDouble; ValueCount: Integer);cdecl;
procedure Sensors_Set_IsDelta(Value: WordBool);cdecl;
procedure Sensors_Set_kVARS(ValuePtr: PDouble; ValueCount: Integer);cdecl;
procedure Sensors_Set_kVS(ValuePtr: PDouble; ValueCount: Integer);cdecl;
procedure Sensors_Set_kWS(ValuePtr: PDouble; ValueCount: Integer);cdecl;
procedure Sensors_Set_MeteredElement(const Value: PAnsiChar);cdecl;
procedure Sensors_Set_MeteredTerminal(Value: Integer);cdecl;
procedure Sensors_Set_Name(const Value: PAnsiChar);cdecl;
procedure Sensors_Set_PctError(Value: Double);cdecl;
procedure Sensors_Set_ReverseDelta(Value: WordBool);cdecl;
procedure Sensors_Set_Weight(Value: Double);cdecl;
function Sensors_Get_kVbase():Double;cdecl;
procedure Sensors_Set_kVbase(Value: Double);cdecl;

IMPLEMENTATION

USES CAPI_Constants, Sensor, DSSGlobals, PointerList, Executive, SysUtils;

function ActiveSensor: TSensorObj;
begin
  Result := nil;
  if ActiveCircuit <> Nil then Result := ActiveCircuit.Sensors.Active;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('capacitor.%s.%s=%s', [ActiveSensor.Name, parm, val]);
  DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
PROCEDURE Sensors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  elem:TSensorObj;
  k:Integer;
Begin
  Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
  Result[0] := DSS_CopyStringAsPChar('NONE');
  IF ActiveCircuit <> Nil THEN
    WITH ActiveCircuit DO
      If Sensors.ListSize>0 Then Begin
        DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (Sensors.ListSize-1) + 1);
        k:=0;
        elem := Sensors.First;
        WHILE elem<>Nil DO Begin
          Result[k] := DSS_CopyStringAsPChar(elem.Name);
          Inc(k);
          elem := Sensors.Next;
        End;
      End;
end;
//------------------------------------------------------------------------------
function Sensors_Get_Count():Integer;cdecl;
begin
  If Assigned(ActiveCircuit) Then
    Result := ActiveCircuit.Sensors.ListSize;
end;
//------------------------------------------------------------------------------
PROCEDURE Sensors_Get_Currents(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
  elem :TSensorObj;
  k    :Integer;
Begin
  elem := ActiveSensor;
  if elem <> Nil then begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (elem.NPhases -1) + 1);
    for k := 0 to elem.NPhases-1 do Result[k] := elem.SensorCurrent^[k+1];
  end else
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
end;
//------------------------------------------------------------------------------
function Sensors_Get_First():Integer;cdecl;
Var
  elem: TSensorObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit <> Nil Then begin
    lst := ActiveCircuit.Sensors;
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
function Sensors_Get_IsDelta():WordBool;cdecl;
var
  elem: TSensorObj;
begin
  Result := FALSE;
  elem := ActiveSensor;
  if elem <> nil then
    if elem.Conn > 0 then Result := TRUE;
end;
//------------------------------------------------------------------------------
PROCEDURE Sensors_Get_kVARS(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
  elem :TSensorObj;
  k    :Integer;
Begin
  elem := ActiveSensor;
  if elem <> Nil then begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (elem.NPhases -1) + 1);
    for k := 0 to elem.NPhases-1 do Result[k] := elem.SensorQ^[k+1];
  end else
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
end;
//------------------------------------------------------------------------------
PROCEDURE Sensors_Get_kVS(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
  elem :TSensorObj;
  k    :Integer;
Begin
  elem := ActiveSensor;
  if elem <> Nil then begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (elem.NPhases -1) + 1);
    for k := 0 to elem.NPhases-1 do Result[k] := elem.SensorVoltage^[k+1];
  end else
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
end;
//------------------------------------------------------------------------------
PROCEDURE Sensors_Get_kWS(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
  elem :TSensorObj;
  k    :Integer;
Begin
  elem := ActiveSensor;
  if elem <> Nil then begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (elem.NPhases -1) + 1);
    for k := 0 to elem.NPhases-1 do Result[k] := elem.SensorP^[k+1];
  end else
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
end;
//------------------------------------------------------------------------------
function Sensors_Get_MeteredElement_AnsiString():AnsiString;inline;
Var
  elem: TSensorObj;
Begin
  Result := '';
  elem := ActiveSensor;
  If elem <> Nil Then Result := elem.ElementName;
end;

function Sensors_Get_MeteredElement():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Sensors_Get_MeteredElement_AnsiString());
end;
//------------------------------------------------------------------------------
function Sensors_Get_MeteredTerminal():Integer;cdecl;
Var
  elem: TSensorObj;
Begin
  Result := 0;
  elem := ActiveSensor;
  If elem <> Nil Then Result := elem.MeteredTerminal;
end;
//------------------------------------------------------------------------------
function Sensors_Get_Name_AnsiString():AnsiString;inline;
Var
  elem: TSensorObj;
Begin
  Result := '';
  elem := ActiveSensor;
  If elem <> Nil Then Result := elem.Name;
end;

function Sensors_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Sensors_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Sensors_Get_Next():Integer;cdecl;
Var
  elem: TSensorObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit <> Nil Then Begin
    lst := ActiveCircuit.Sensors;
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
function Sensors_Get_PctError():Double;cdecl;
Var
  elem: TSensorObj;
Begin
  Result := 0.0;
  elem := ActiveSensor;
  If elem <> Nil Then Result := elem.pctError;
end;
//------------------------------------------------------------------------------
function Sensors_Get_ReverseDelta():WordBool;cdecl;
var
  elem: TSensorObj;
begin
  Result := FALSE;
  elem := ActiveSensor;
  if elem <> nil then
    if elem.DeltaDirection < 0 then Result := TRUE;
end;
//------------------------------------------------------------------------------
function Sensors_Get_Weight():Double;cdecl;
Var
  elem: TSensorObj;
Begin
  Result := 0.0;
  elem := ActiveSensor;
  If elem <> Nil Then Result := elem.Weight;
end;
//------------------------------------------------------------------------------
procedure Sensors_Reset();cdecl;
Var
  elem: TSensorObj;
Begin
  elem := ActiveSensor;
  If elem <> Nil Then elem.ResetIt;
end;
//------------------------------------------------------------------------------
procedure Sensors_ResetAll();cdecl;
begin
  if assigned(ActiveCircuit) then SensorClass.ResetAll;
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_Currents(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  Value: PDoubleArray;
  elem: TSensorObj;
  i, k: Integer;
begin
    Value := PDoubleArray(ValuePtr);
  elem := ActiveSensor;
  if elem <> nil then begin
    k := (0);
    for i := 1 to elem.NPhases do begin
      elem.SensorCurrent^[i] := Value[k];
      inc(k);
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_IsDelta(Value: WordBool);cdecl;
var
  elem: TSensorObj;
begin
  elem := ActiveSensor;
  if elem <> nil then elem.Conn := Integer (Value);
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_kVARS(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  Value: PDoubleArray;
  elem: TSensorObj;
  i, k: Integer;
begin
    Value := PDoubleArray(ValuePtr);
  elem := ActiveSensor;
  if elem <> nil then begin
    k := (0);
    for i := 1 to elem.NPhases do begin
      elem.SensorQ^[i] := Value[k];
      inc(k);
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_kVS(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  Value: PDoubleArray;
  elem: TSensorObj;
  i, k: Integer;
begin
    Value := PDoubleArray(ValuePtr);
  elem := ActiveSensor;
  if elem <> nil then begin
    k := (0);
    for i := 1 to elem.NPhases do begin
      elem.SensorVoltage^[i] := Value[k];
      inc(k);
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_kWS(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  Value: PDoubleArray;
  elem: TSensorObj;
  i, k: Integer;
begin
    Value := PDoubleArray(ValuePtr);
  elem := ActiveSensor;
  if elem <> nil then begin
    k := (0);
    for i := 1 to elem.NPhases do begin
      elem.SensorP^[i] := Value[k];
      inc(k);
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_MeteredElement(const Value: PAnsiChar);cdecl;
begin
  Set_Parameter ('element', Value);
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_MeteredTerminal(Value: Integer);cdecl;
begin
  Set_Parameter ('terminal', IntToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_Name(const Value: PAnsiChar);cdecl;
var
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  elem: TSensorObj;
  lst: TPointerList;
begin
  IF ActiveCircuit <> NIL THEN Begin
    lst := ActiveCircuit.Sensors;
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
      DoSimpleMsg('Sensor "'+S+'" Not Found in Active Circuit.', 5003);
      elem := lst.Get(ActiveSave);
      ActiveCircuit.ActiveCktElement := elem;
    End;
  End;
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_PctError(Value: Double);cdecl;
begin
  Set_Parameter ('%error', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_ReverseDelta(Value: WordBool);cdecl;
begin
  if Value = TRUE then
    Set_Parameter ('DeltaDirection', '-1')
  else
    Set_Parameter ('DeltaDirection', '1');
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_Weight(Value: Double);cdecl;
begin
  Set_Parameter ('weight', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
function Sensors_Get_kVbase():Double;cdecl;
Var
  elem: TSensorObj;
Begin
  Result := 0.0;
  elem := ActiveSensor;
  If elem <> Nil Then Result := elem.BaseKV;
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_kVbase(Value: Double);cdecl;
begin
  Set_Parameter ('kvbase', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
END.
