unit CAPI_Sensors;

interface

uses
    CAPI_Utils,
    CAPI_Types;

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
procedure Sensors_Get_AllocationFactor(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Sensors_Get_AllocationFactor_GR(); CDECL;

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
    SysUtils,
    DSSClass,
    DSSHelper,
    DSSObjectHelper;

type
    TObj = TSensorObj;

//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.ActiveCircuit.Sensors.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['Sensor'], 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(DSS: TDSSContext; const idx: Integer; const val: String); overload;
var
    elem: TObj;
begin
    if not _activeObj(DSS, elem) then
        Exit;
    DSS.SolutionAbort := FALSE;  // Reset for commands entered from outside
    elem.ParsePropertyValue(idx, val);
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(DSS: TDSSContext; const idx: Integer; const val: Double); overload;
var
    elem: TObj;
begin
    if not _activeObj(DSS, elem) then
        Exit;
    DSS.SolutionAbort := FALSE;  // Reset for commands entered from outside
    elem.SetDouble(idx, val);
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(DSS: TDSSContext; const idx: Integer; const val: Integer); overload;
var
    elem: TObj;
begin
    if not _activeObj(DSS, elem) then
        Exit;
    DSS.SolutionAbort := FALSE;  // Reset for commands entered from outside
    elem.SetInteger(idx, val);
end;
//------------------------------------------------------------------------------
procedure Sensors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.Sensors, False);
end;

procedure Sensors_Get_AllNames_GR(); CDECL;
// Same as Sensors_Get_AllNames but uses global result (GR) pointers
begin
    Sensors_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function Sensors_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Sensors.Count;
end;
//------------------------------------------------------------------------------
procedure Sensors_Get_Currents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
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
    Sensors_Get_Currents(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
function Sensors_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime, DSSPrime.ActiveCircuit.Sensors);
end;
//------------------------------------------------------------------------------
function Sensors_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime, DSSPrime.ActiveCircuit.Sensors);
end;
//------------------------------------------------------------------------------
function Sensors_Get_IsDelta(): TAPIBoolean; CDECL;
var
    elem: TObj;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := (elem.FConn > 0);
end;
//------------------------------------------------------------------------------
procedure Sensors_Get_kVARS(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
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
    Sensors_Get_kVARS(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Sensors_Get_kVS(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
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
    Sensors_Get_kVS(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Sensors_Get_kWS(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
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
    Sensors_Get_kWS(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
function Sensors_Get_MeteredElement(): PAnsiChar; CDECL;
var
    elem: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if elem.MeteredElement <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, LowerCase(elem.MeteredElement.FullName));
end;
//------------------------------------------------------------------------------
function Sensors_Get_MeteredTerminal(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.MeteredTerminal;
end;
//------------------------------------------------------------------------------
function Sensors_Get_Name(): PAnsiChar; CDECL;
var
    elem: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name);
end;
//------------------------------------------------------------------------------
function Sensors_Get_PctError(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.pctError;
end;
//------------------------------------------------------------------------------
function Sensors_Get_ReverseDelta(): TAPIBoolean; CDECL;
var
    elem: TObj;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := (elem.DeltaDirection < 0);
end;
//------------------------------------------------------------------------------
function Sensors_Get_Weight(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.Weight;
end;
//------------------------------------------------------------------------------
procedure Sensors_Reset(); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.ResetIt();
end;
//------------------------------------------------------------------------------
procedure Sensors_ResetAll(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.SensorClass.ResetAll();
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_Currents(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if ValueCount <> elem.NPhases then
    begin
        DoSimpleMsg(DSSPrime, _('The provided number of values does not match the element''s number of phases.'), 5023);
        Exit;
    end;
    Move(ValuePtr^, elem.SensorCurrent[1], elem.NPhases * SizeOf(Double));
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_IsDelta(Value: TAPIBoolean); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.FConn := Integer(Value);
    elem.RecalcVbase;
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_kVARS(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if ValueCount <> elem.NPhases then
    begin
        DoSimpleMsg(DSSPrime, _('The provided number of values does not match the element''s number of phases.'), 5024);
        Exit;
    end;
    Move(ValuePtr^, elem.SensorQ[1], elem.NPhases * SizeOf(Double));
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_kVS(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if ValueCount <> elem.NPhases then
    begin
        DoSimpleMsg(DSSPrime, _('The provided number of values does not match the element''s number of phases.'), 5024);
        Exit;
    end;
    Move(ValuePtr^, elem.SensorVoltage[1], elem.NPhases * SizeOf(Double));
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_kWS(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if ValueCount <> elem.NPhases then
    begin
        DoSimpleMsg(DSSPrime, _('The provided number of values does not match the element''s number of phases.'), 5024);
        Exit;
    end;
    Move(ValuePtr^, elem.SensorP[1], elem.NPhases * SizeOf(Double));
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_MeteredElement(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TSensorProp.element), Value);
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_MeteredTerminal(Value: Integer); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TSensorProp.terminal), Value);
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    if DSSPrime.SensorClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.SensorClass.ElementList.Active;
        DSSPrime.ActiveCircuit.Sensors.Get(DSSPrime.SensorClass.Active);
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'Sensor "%s" not found in Active Circuit.', [Value], 5003);
    end;
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_PctError(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TSensorProp.pcterror), Value);
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_ReverseDelta(Value: TAPIBoolean); CDECL;
begin
    if Value then
        Set_Parameter(DSSPrime, ord(TSensorProp.DeltaDirection), -1)
    else
        Set_Parameter(DSSPrime, ord(TSensorProp.DeltaDirection), 1);
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_Weight(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TSensorProp.weight), Value);
end;
//------------------------------------------------------------------------------
function Sensors_Get_kVbase(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.BaseKV;
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_kVbase(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TSensorProp.kvbase), Value);
end;
//------------------------------------------------------------------------------
function Sensors_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Sensors.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_idx(Value: Integer); CDECL;
var
    pSensor: TObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    pSensor := DSSPrime.ActiveCircuit.Sensors.Get(Value);
    if pSensor = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['Sensor', Value], 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pSensor;
end;
//------------------------------------------------------------------------------
procedure Sensors_Get_AllocationFactor(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, elem.NPhases);
    Move(elem.PhsAllocationFactor[1], ResultPtr^, elem.NPhases * SizeOf(Double));
end;

procedure Sensors_Get_AllocationFactor_GR(); CDECL;
// Same as Sensors_Get_AllocationFactor but uses global result (GR) pointers
begin
    Sensors_Get_AllocationFactor(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
end.
