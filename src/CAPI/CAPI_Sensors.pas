unit CAPI_Sensors;

interface

uses
    CAPI_Utils;

procedure Sensors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Sensors_Get_AllNames_GR(); CDECL;
function Sensors_Get_Count(): Integer; CDECL;
procedure Sensors_Get_Currents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Sensors_Get_Currents_GR(); CDECL;
function Sensors_Get_First(): Integer; CDECL;
function Sensors_Get_IsDelta(): TAPIBoolean; CDECL;
procedure Sensors_Get_kVARS(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Sensors_Get_kVARS_GR(); CDECL;
procedure Sensors_Get_kVS(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Sensors_Get_kVS_GR(); CDECL;
procedure Sensors_Get_kWS(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Sensors_Get_kWS_GR(); CDECL;
function Sensors_Get_MeteredElement(): PAnsiChar; CDECL;
function Sensors_Get_MeteredTerminal(): Integer; CDECL;
function Sensors_Get_Name(): PAnsiChar; CDECL;
function Sensors_Get_Next(): Integer; CDECL;
function Sensors_Get_PctError(): Double; CDECL;
function Sensors_Get_ReverseDelta(): TAPIBoolean; CDECL;
function Sensors_Get_Weight(): Double; CDECL;
procedure Sensors_Reset(); CDECL;
procedure Sensors_ResetAll(); CDECL;
procedure Sensors_Set_Currents(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure Sensors_Set_IsDelta(Value: TAPIBoolean); CDECL;
procedure Sensors_Set_kVARS(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure Sensors_Set_kVS(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure Sensors_Set_kWS(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure Sensors_Set_MeteredElement(const Value: PAnsiChar); CDECL;
procedure Sensors_Set_MeteredTerminal(Value: Integer); CDECL;
procedure Sensors_Set_Name(const Value: PAnsiChar); CDECL;
procedure Sensors_Set_PctError(Value: Double); CDECL;
procedure Sensors_Set_ReverseDelta(Value: TAPIBoolean); CDECL;
procedure Sensors_Set_Weight(Value: Double); CDECL;
function Sensors_Get_kVbase(): Double; CDECL;
procedure Sensors_Set_kVbase(Value: Double); CDECL;

// API extensions
function Sensors_Get_idx(): Integer; CDECL;
procedure Sensors_Set_idx(Value: Integer); CDECL;


implementation

uses
    CAPI_Constants,
    Sensor,
    DSSGlobals,
    DSSPointerList,
    Executive,
    SysUtils;

//------------------------------------------------------------------------------
function _activeObj(out obj: TSensorObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit then
        Exit;
    
    obj := ActiveCircuit.Sensors.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active Sensor object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
    elem: TSensorObj;
begin
    if not _activeObj(elem) then
        Exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('sensor.%s.%s=%s', [elem.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
procedure Sensors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, ActiveCircuit.Sensors, False);
end;

procedure Sensors_Get_AllNames_GR(); CDECL;
// Same as Sensors_Get_AllNames but uses global result (GR) pointers
begin
    Sensors_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Sensors_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Sensors.Count;
end;
//------------------------------------------------------------------------------
procedure Sensors_Get_Currents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TSensorObj;
begin
    if not _activeObj(elem) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, elem.NPhases);
    Move(elem.SensorCurrent[1], ResultPtr^, elem.NPhases * SizeOf(Double));
end;

procedure Sensors_Get_Currents_GR(); CDECL;
// Same as Sensors_Get_Currents but uses global result (GR) pointers
begin
    Sensors_Get_Currents(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function Sensors_Get_First(): Integer; CDECL;
var
    elem: TSensorObj;
    lst: TDSSPointerList;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;

    lst := ActiveCircuit.Sensors;
    elem := lst.First;
    if elem = NIL then
        Exit;
        
    repeat
        if elem.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := elem;
            Result := 1;
        end
        else
            elem := lst.Next;
    until (Result = 1) or (elem = NIL);
end;
//------------------------------------------------------------------------------
function Sensors_Get_IsDelta(): TAPIBoolean; CDECL;
var
    elem: TSensorObj;
begin
    Result := FALSE;
    if not _activeObj(elem) then
        Exit;
    Result := (elem.Conn > 0);
end;
//------------------------------------------------------------------------------
procedure Sensors_Get_kVARS(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TSensorObj;
begin
    if not _activeObj(elem) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, elem.NPhases);
    Move(elem.SensorQ[1], ResultPtr^, elem.NPhases * SizeOf(Double));
end;

procedure Sensors_Get_kVARS_GR(); CDECL;
// Same as Sensors_Get_kVARS but uses global result (GR) pointers
begin
    Sensors_Get_kVARS(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Sensors_Get_kVS(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TSensorObj;
begin
    if not _activeObj(elem) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, elem.NPhases);
    Move(elem.SensorVoltage[1], ResultPtr^, elem.NPhases * SizeOf(Double));
end;

procedure Sensors_Get_kVS_GR(); CDECL;
// Same as Sensors_Get_kVS but uses global result (GR) pointers
begin
    Sensors_Get_kVS(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Sensors_Get_kWS(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TSensorObj;
begin
    if not _activeObj(elem) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, elem.NPhases);
    Move(elem.SensorP[1], ResultPtr^, elem.NPhases * SizeOf(Double));
end;

procedure Sensors_Get_kWS_GR(); CDECL;
// Same as Sensors_Get_kWS but uses global result (GR) pointers
begin
    Sensors_Get_kWS(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function Sensors_Get_MeteredElement(): PAnsiChar; CDECL;
var
    elem: TSensorObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem.ElementName);
end;
//------------------------------------------------------------------------------
function Sensors_Get_MeteredTerminal(): Integer; CDECL;
var
    elem: TSensorObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.MeteredTerminal;
end;
//------------------------------------------------------------------------------
function Sensors_Get_Name(): PAnsiChar; CDECL;
var
    elem: TSensorObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem.Name);
end;
//------------------------------------------------------------------------------
function Sensors_Get_Next(): Integer; CDECL;
var
    elem: TSensorObj;
    lst: TDSSPointerList;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;

    lst := ActiveCircuit.Sensors;
    elem := lst.Next;
    if elem = NIL then
        Exit;
        
    repeat
        if elem.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := elem;
            Result := lst.ActiveIndex;
        end
        else
            elem := lst.Next;
    until (Result > 0) or (elem = NIL);
end;
//------------------------------------------------------------------------------
function Sensors_Get_PctError(): Double; CDECL;
var
    elem: TSensorObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.pctError;
end;
//------------------------------------------------------------------------------
function Sensors_Get_ReverseDelta(): TAPIBoolean; CDECL;
var
    elem: TSensorObj;
begin
    Result := FALSE;
    if not _activeObj(elem) then
        Exit;
    Result := (elem.DeltaDirection < 0);
end;
//------------------------------------------------------------------------------
function Sensors_Get_Weight(): Double; CDECL;
var
    elem: TSensorObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.Weight;
end;
//------------------------------------------------------------------------------
procedure Sensors_Reset(); CDECL;
var
    elem: TSensorObj;
begin
    if not _activeObj(elem) then
        Exit;
    elem.ResetIt();
end;
//------------------------------------------------------------------------------
procedure Sensors_ResetAll(); CDECL;
begin
    if InvalidCircuit then
        Exit;
    SensorClass.ResetAll();
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_Currents(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    elem: TSensorObj;
begin
    if not _activeObj(elem) then
        Exit;

    if ValueCount <> elem.NPhases then
    begin
        DoSimpleMsg('The provided number of values does not match the element''s number of phases.', 5023);
        Exit;
    end;
    Move(ValuePtr^, elem.SensorCurrent[1], elem.NPhases * SizeOf(Double));
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_IsDelta(Value: TAPIBoolean); CDECL;
var
    elem: TSensorObj;
begin
    if not _activeObj(elem) then
        Exit;
    elem.Conn := Integer(Value);
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_kVARS(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    elem: TSensorObj;
begin
    if not _activeObj(elem) then
        Exit;

    if ValueCount <> elem.NPhases then
    begin
        DoSimpleMsg('The provided number of values does not match the element''s number of phases.', 5024);
        Exit;
    end;
    Move(ValuePtr^, elem.SensorQ[1], elem.NPhases * SizeOf(Double));
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_kVS(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    elem: TSensorObj;
begin
    if not _activeObj(elem) then
        Exit;

    if ValueCount <> elem.NPhases then
    begin
        DoSimpleMsg('The provided number of values does not match the element''s number of phases.', 5024);
        Exit;
    end;
    Move(ValuePtr^, elem.SensorVoltage[1], elem.NPhases * SizeOf(Double));
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_kWS(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    elem: TSensorObj;
begin
    if not _activeObj(elem) then
        Exit;

    if ValueCount <> elem.NPhases then
    begin
        DoSimpleMsg('The provided number of values does not match the element''s number of phases.', 5024);
        Exit;
    end;
    Move(ValuePtr^, elem.SensorP[1], elem.NPhases * SizeOf(Double));
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_MeteredElement(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter('element', Value);
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_MeteredTerminal(Value: Integer); CDECL;
begin
    Set_Parameter('terminal', IntToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit then
        Exit;
    if SensorClass.SetActive(Value) then
    begin
        ActiveCircuit.ActiveCktElement := SensorClass.ElementList.Active;
        ActiveCircuit.Sensors.Get(SensorClass.Active);
    end
    else
    begin
        DoSimpleMsg('Sensor "' + Value + '" Not Found in Active Circuit.', 5003);
    end;
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_PctError(Value: Double); CDECL;
begin
    Set_Parameter('%error', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_ReverseDelta(Value: TAPIBoolean); CDECL;
begin
    if Value = TRUE then
        Set_Parameter('DeltaDirection', '-1')
    else
        Set_Parameter('DeltaDirection', '1');
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_Weight(Value: Double); CDECL;
begin
    Set_Parameter('weight', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
function Sensors_Get_kVbase(): Double; CDECL;
var
    elem: TSensorObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.BaseKV;
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_kVbase(Value: Double); CDECL;
begin
    Set_Parameter('kvbase', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
function Sensors_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Sensors.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_idx(Value: Integer); CDECL;
var
    pSensor: TSensorObj;
begin
    if InvalidCircuit then
        Exit;
    pSensor := ActiveCircuit.Sensors.Get(Value);
    if pSensor = NIL then
    begin
        DoSimpleMsg('Invalid Sensor index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    ActiveCircuit.ActiveCktElement := pSensor;
end;
//------------------------------------------------------------------------------
end.
