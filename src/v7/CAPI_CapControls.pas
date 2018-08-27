UNIT CAPI_CapControls;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE CapControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
function CapControls_Get_Capacitor():PAnsiChar;cdecl;
function CapControls_Get_CTratio():Double;cdecl;
function CapControls_Get_DeadTime():Double;cdecl;
function CapControls_Get_Delay():Double;cdecl;
function CapControls_Get_DelayOff():Double;cdecl;
function CapControls_Get_First():Integer;cdecl;
function CapControls_Get_Mode():Integer;cdecl;
function CapControls_Get_MonitoredObj():PAnsiChar;cdecl;
function CapControls_Get_MonitoredTerm():Integer;cdecl;
function CapControls_Get_Name():PAnsiChar;cdecl;
function CapControls_Get_Next():Integer;cdecl;
function CapControls_Get_OFFSetting():Double;cdecl;
function CapControls_Get_ONSetting():Double;cdecl;
function CapControls_Get_PTratio():Double;cdecl;
function CapControls_Get_UseVoltOverride():WordBool;cdecl;
function CapControls_Get_Vmax():Double;cdecl;
function CapControls_Get_Vmin():Double;cdecl;
procedure CapControls_Set_Capacitor(const Value: PAnsiChar);cdecl;
procedure CapControls_Set_CTratio(Value: Double);cdecl;
procedure CapControls_Set_DeadTime(Value: Double);cdecl;
procedure CapControls_Set_Delay(Value: Double);cdecl;
procedure CapControls_Set_DelayOff(Value: Double);cdecl;
procedure CapControls_Set_Mode(Value: Integer);cdecl;
procedure CapControls_Set_MonitoredObj(const Value: PAnsiChar);cdecl;
procedure CapControls_Set_MonitoredTerm(Value: Integer);cdecl;
procedure CapControls_Set_Name(const Value: PAnsiChar);cdecl;
procedure CapControls_Set_OFFSetting(Value: Double);cdecl;
procedure CapControls_Set_ONSetting(Value: Double);cdecl;
procedure CapControls_Set_PTratio(Value: Double);cdecl;
procedure CapControls_Set_UseVoltOverride(Value: WordBool);cdecl;
procedure CapControls_Set_Vmax(Value: Double);cdecl;
procedure CapControls_Set_Vmin(Value: Double);cdecl;
function CapControls_Get_Count():Integer;cdecl;
procedure CapControls_Reset();cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, Executive, ControlElem, CapControl, CapControlVars, SysUtils, PointerList;

function ActiveCapControl: TCapControlObj;
begin
  Result := nil;
  if ActiveCircuit <> Nil then Result := ActiveCircuit.CapControls.Active;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('capcontrol.%s.%s=%s', [ActiveCapControl.Name, parm, val]);
  DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
PROCEDURE CapControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  elem: TCapControlObj;
  lst: TPointerList;
  k: Integer;
Begin
  Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
  Result[0] := DSS_CopyStringAsPChar('NONE');
  IF ActiveCircuit <> Nil THEN WITH ActiveCircuit DO
  If CapControls.ListSize > 0 Then
  Begin
    lst := CapControls;
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
function CapControls_Get_Capacitor_AnsiString():AnsiString;inline;
var
  elem: TCapControlObj;
begin
  Result := '';
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.This_Capacitor.Name;
end;

function CapControls_Get_Capacitor():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(CapControls_Get_Capacitor_AnsiString());
end;
//------------------------------------------------------------------------------
function CapControls_Get_CTratio():Double;cdecl;
var
  elem: TCapControlObj;
begin
  Result := 0.0;
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.CTRatioVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_DeadTime():Double;cdecl;
var
  elem: TCapControlObj;
begin
  Result := 0.0;
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.DeadTimeVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_Delay():Double;cdecl;
var
  elem: TCapControlObj;
begin
  Result := 0.0;
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.OnDelayVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_DelayOff():Double;cdecl;
var
  elem: TCapControlObj;
begin
  Result := 0.0;
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.OffDelayVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_First():Integer;cdecl;
Var
  elem: TCapControlObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit <> Nil Then begin
    lst := ActiveCircuit.CapControls;
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
function CapControls_Get_Mode():Integer;cdecl;
var
  elem: TCapControlObj;
begin
  Result := dssCapControlVoltage;
  elem := ActiveCapControl;
  if elem <> nil then begin
    case elem.CapControlType of
      CURRENTCONTROL: Result := dssCapControlCurrent;
      VOLTAGECONTROL: Result := dssCapControlVoltage;
      KVARCONTROL: Result := dssCapControlKvar;
      TIMECONTROL: Result := dssCapControlTime;
      PFCONTROL: Result := dssCapControlPF;
      USERCONTROL: Result := dssCapControlPF;
    end;
  end;
end;
//------------------------------------------------------------------------------
function CapControls_Get_MonitoredObj_AnsiString():AnsiString;inline;
var
  elem: TCapControlObj;
begin
  Result := '';
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.ElementName;
end;

function CapControls_Get_MonitoredObj():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(CapControls_Get_MonitoredObj_AnsiString());
end;
//------------------------------------------------------------------------------
function CapControls_Get_MonitoredTerm():Integer;cdecl;
var
  elem: TCapControlObj;
begin
  Result := 0;
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.ElementTerminal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_Name_AnsiString():AnsiString;inline;
var
  elem: TCapControlObj;
begin
  Result := '';
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.Name;
end;

function CapControls_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(CapControls_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function CapControls_Get_Next():Integer;cdecl;
Var
  elem: TCapControlObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit <> Nil Then Begin
    lst := ActiveCircuit.CapControls;
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
function CapControls_Get_OFFSetting():Double;cdecl;
var
  elem: TCapControlObj;
begin
  Result := 0.0;
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.OffValue;
end;
//------------------------------------------------------------------------------
function CapControls_Get_ONSetting():Double;cdecl;
var
  elem: TCapControlObj;
begin
  Result := 0.0;
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.OnValue;
end;
//------------------------------------------------------------------------------
function CapControls_Get_PTratio():Double;cdecl;
var
  elem: TCapControlObj;
begin
  Result := 0.0;
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.PTRatioVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_UseVoltOverride():WordBool;cdecl;
var
  elem: TCapControlObj;
begin
  Result := FALSE;
  elem := ActiveCapControl;
  if elem <> nil then
    if elem.UseVoltageOverride then Result := TRUE;
end;
//------------------------------------------------------------------------------
function CapControls_Get_Vmax():Double;cdecl;
var
  elem: TCapControlObj;
begin
  Result := 0.0;
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.VmaxVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_Vmin():Double;cdecl;
var
  elem: TCapControlObj;
begin
  Result := 0.0;
  elem := ActiveCapControl;
  if elem <> nil then Result := elem.VminVal;
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_Capacitor(const Value: PAnsiChar);cdecl;
begin
  Set_Parameter ('Capacitor', value);
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_CTratio(Value: Double);cdecl;
begin
  Set_Parameter ('CTratio', FloatToStr (value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_DeadTime(Value: Double);cdecl;
begin
  Set_Parameter ('DeadTime', FloatToStr (value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_Delay(Value: Double);cdecl;
begin
  Set_Parameter ('Delay', FloatToStr (value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_DelayOff(Value: Double);cdecl;
begin
  Set_Parameter ('DelayOff', FloatToStr (value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_Mode(Value: Integer);cdecl;
var
  elem: TCapControlObj;
begin
  elem := ActiveCapControl;
  if elem <> nil then begin
    case Value of
      dssCapControlCurrent: elem.CapControlType := CURRENTCONTROL;
      dssCapControlVoltage: elem.CapControlType := VOLTAGECONTROL;
      dssCapControlKvar: elem.CapControlType := KVARCONTROL;
      dssCapControlTime: elem.CapControlType := TIMECONTROL;
      dssCapControlPF: elem.CapControlType := PFCONTROL;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_MonitoredObj(const Value: PAnsiChar);cdecl;
begin
  Set_Parameter ('Element', value);
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_MonitoredTerm(Value: Integer);cdecl;
begin
  Set_Parameter ('Terminal', IntToStr (value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_Name(const Value: PAnsiChar);cdecl;
var
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  elem: TCapControlObj;
  lst: TPointerList;
Begin
  IF ActiveCircuit <> NIL THEN Begin
    lst := ActiveCircuit.CapControls;
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
      DoSimpleMsg('CapControl "'+S+'" Not Found in Active Circuit.', 5003);
      elem := lst.Get(ActiveSave);    // Restore active Load
      ActiveCircuit.ActiveCktElement := elem;
    End;
  End;
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_OFFSetting(Value: Double);cdecl;
begin
  Set_Parameter ('OffSetting', FloatToStr (value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_ONSetting(Value: Double);cdecl;
begin
  Set_Parameter ('OnSetting', FloatToStr (value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_PTratio(Value: Double);cdecl;
begin
  Set_Parameter ('PTratio', FloatToStr (value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_UseVoltOverride(Value: WordBool);cdecl;
begin
  if Value = true then
    Set_Parameter ('VoltOverride', 'Yes')
  else
    Set_Parameter ('VoltOverride', 'No');
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_Vmax(Value: Double);cdecl;
begin
  Set_Parameter ('Vmax', FloatToStr (value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_Vmin(Value: Double);cdecl;
begin
  Set_Parameter ('Vmin', FloatToStr (value));
end;
//------------------------------------------------------------------------------
function CapControls_Get_Count():Integer;cdecl;
begin
     If Assigned(ActiveCircuit) Then
              Result := ActiveCircuit.CapControls.ListSize ;
end;
//------------------------------------------------------------------------------
procedure CapControls_Reset();cdecl;
var
  elem: TCapControlObj;
begin
  elem   := ActiveCapControl;
  if elem <> nil then begin
      elem.Reset;
  end;

end;
//------------------------------------------------------------------------------
END.
