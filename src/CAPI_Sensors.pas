unit CAPI_Sensors;

{$inline on}

interface

uses
    CAPI_Utils;

procedure Sensors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
function Sensors_Get_Count(): Integer; CDECL;
procedure Sensors_Get_Currents(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Sensors_Get_Currents_GR(); CDECL;
function Sensors_Get_First(): Integer; CDECL;
function Sensors_Get_IsDelta(): Boolean; CDECL;
procedure Sensors_Get_kVARS(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Sensors_Get_kVARS_GR(); CDECL;
procedure Sensors_Get_kVS(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Sensors_Get_kVS_GR(); CDECL;
procedure Sensors_Get_kWS(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Sensors_Get_kWS_GR(); CDECL;
function Sensors_Get_MeteredElement(): PAnsiChar; CDECL;
function Sensors_Get_MeteredTerminal(): Integer; CDECL;
function Sensors_Get_Name(): PAnsiChar; CDECL;
function Sensors_Get_Next(): Integer; CDECL;
function Sensors_Get_PctError(): Double; CDECL;
function Sensors_Get_ReverseDelta(): Boolean; CDECL;
function Sensors_Get_Weight(): Double; CDECL;
procedure Sensors_Reset(); CDECL;
procedure Sensors_ResetAll(); CDECL;
procedure Sensors_Set_Currents(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure Sensors_Set_IsDelta(Value: Boolean); CDECL;
procedure Sensors_Set_kVARS(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure Sensors_Set_kVS(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure Sensors_Set_kWS(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure Sensors_Set_MeteredElement(const Value: PAnsiChar); CDECL;
procedure Sensors_Set_MeteredTerminal(Value: Integer); CDECL;
procedure Sensors_Set_Name(const Value: PAnsiChar); CDECL;
procedure Sensors_Set_PctError(Value: Double); CDECL;
procedure Sensors_Set_ReverseDelta(Value: Boolean); CDECL;
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
    PointerList,
    Executive,
    SysUtils,
    DSSClass,
    DSSHelper;

function ActiveSensor: TSensorObj;
begin
    Result := NIL;
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.ActiveCircuit.Sensors.Active;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(DSSPrime.ActiveCircuit) then
        exit;
    DSSPrime.SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('capacitor.%s.%s=%s', [ActiveSensor.Name, parm, val]);
    DSSPrime.DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
procedure Sensors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.Sensors, False);
end;
//------------------------------------------------------------------------------
function Sensors_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if Assigned(DSSPrime.ActiveCircuit) then
        Result := DSSPrime.ActiveCircuit.Sensors.ListSize;
end;
//------------------------------------------------------------------------------
procedure Sensors_Get_Currents(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    elem: TSensorObj;
    k: Integer;
begin
    elem := ActiveSensor;
    if elem <> NIL then
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (elem.NPhases - 1) + 1);
        for k := 0 to elem.NPhases - 1 do
            Result[k] := elem.SensorCurrent^[k + 1];
    end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
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
    lst: TPointerList;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        lst := DSSPrime.ActiveCircuit.Sensors;
        elem := lst.First;
        if elem <> NIL then
        begin
            repeat
                if elem.Enabled then
                begin
                    DSSPrime.ActiveCircuit.ActiveCktElement := elem;
                    Result := 1;
                end
                else
                    elem := lst.Next;
            until (Result = 1) or (elem = NIL);
        end;
    end;
end;
//------------------------------------------------------------------------------
function Sensors_Get_IsDelta(): Boolean; CDECL;
var
    elem: TSensorObj;
begin
    Result := FALSE;
    elem := ActiveSensor;
    if elem <> NIL then
        if elem.Conn > 0 then
            Result := TRUE;
end;
//------------------------------------------------------------------------------
procedure Sensors_Get_kVARS(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    elem: TSensorObj;
    k: Integer;
begin
    elem := ActiveSensor;
    if elem <> NIL then
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (elem.NPhases - 1) + 1);
        for k := 0 to elem.NPhases - 1 do
            Result[k] := elem.SensorQ^[k + 1];
    end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
end;

procedure Sensors_Get_kVARS_GR(); CDECL;
// Same as Sensors_Get_kVARS but uses global result (GR) pointers
begin
    Sensors_Get_kVARS(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Sensors_Get_kVS(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    elem: TSensorObj;
    k: Integer;
begin
    elem := ActiveSensor;
    if elem <> NIL then
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (elem.NPhases - 1) + 1);
        for k := 0 to elem.NPhases - 1 do
            Result[k] := elem.SensorVoltage^[k + 1];
    end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
end;

procedure Sensors_Get_kVS_GR(); CDECL;
// Same as Sensors_Get_kVS but uses global result (GR) pointers
begin
    Sensors_Get_kVS(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Sensors_Get_kWS(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    elem: TSensorObj;
    k: Integer;
begin
    elem := ActiveSensor;
    if elem <> NIL then
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (elem.NPhases - 1) + 1);
        for k := 0 to elem.NPhases - 1 do
            Result[k] := elem.SensorP^[k + 1];
    end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
end;

procedure Sensors_Get_kWS_GR(); CDECL;
// Same as Sensors_Get_kWS but uses global result (GR) pointers
begin
    Sensors_Get_kWS(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function Sensors_Get_MeteredElement_AnsiString(): Ansistring; inline;
var
    elem: TSensorObj;
begin
    Result := '';
    elem := ActiveSensor;
    if elem <> NIL then
        Result := elem.ElementName;
end;

function Sensors_Get_MeteredElement(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Sensors_Get_MeteredElement_AnsiString());
end;
//------------------------------------------------------------------------------
function Sensors_Get_MeteredTerminal(): Integer; CDECL;
var
    elem: TSensorObj;
begin
    Result := 0;
    elem := ActiveSensor;
    if elem <> NIL then
        Result := elem.MeteredTerminal;
end;
//------------------------------------------------------------------------------
function Sensors_Get_Name_AnsiString(): Ansistring; inline;
var
    elem: TSensorObj;
begin
    Result := '';
    elem := ActiveSensor;
    if elem <> NIL then
        Result := elem.Name;
end;

function Sensors_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Sensors_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Sensors_Get_Next(): Integer; CDECL;
var
    elem: TSensorObj;
    lst: TPointerList;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        lst := DSSPrime.ActiveCircuit.Sensors;
        elem := lst.Next;
        if elem <> NIL then
        begin
            repeat
                if elem.Enabled then
                begin
                    DSSPrime.ActiveCircuit.ActiveCktElement := elem;
                    Result := lst.ActiveIndex;
                end
                else
                    elem := lst.Next;
            until (Result > 0) or (elem = NIL);
        end
    end;
end;
//------------------------------------------------------------------------------
function Sensors_Get_PctError(): Double; CDECL;
var
    elem: TSensorObj;
begin
    Result := 0.0;
    elem := ActiveSensor;
    if elem <> NIL then
        Result := elem.pctError;
end;
//------------------------------------------------------------------------------
function Sensors_Get_ReverseDelta(): Boolean; CDECL;
var
    elem: TSensorObj;
begin
    Result := FALSE;
    elem := ActiveSensor;
    if elem <> NIL then
        if elem.DeltaDirection < 0 then
            Result := TRUE;
end;
//------------------------------------------------------------------------------
function Sensors_Get_Weight(): Double; CDECL;
var
    elem: TSensorObj;
begin
    Result := 0.0;
    elem := ActiveSensor;
    if elem <> NIL then
        Result := elem.Weight;
end;
//------------------------------------------------------------------------------
procedure Sensors_Reset(); CDECL;
var
    elem: TSensorObj;
begin
    elem := ActiveSensor;
    if elem <> NIL then
        elem.ResetIt;
end;
//------------------------------------------------------------------------------
procedure Sensors_ResetAll(); CDECL;
begin
    if assigned(DSSPrime.ActiveCircuit) then
        DSSPrime.SensorClass.ResetAll;
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_Currents(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    elem: TSensorObj;
    i, k: Integer;
begin
    Value := PDoubleArray(ValuePtr);
    elem := ActiveSensor;
    if elem <> NIL then
    begin
        k := (0);
        for i := 1 to elem.NPhases do
        begin
            elem.SensorCurrent^[i] := Value[k];
            inc(k);
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_IsDelta(Value: Boolean); CDECL;
var
    elem: TSensorObj;
begin
    elem := ActiveSensor;
    if elem <> NIL then
        elem.Conn := Integer(Value);
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_kVARS(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    elem: TSensorObj;
    i, k: Integer;
begin
    Value := PDoubleArray(ValuePtr);
    elem := ActiveSensor;
    if elem <> NIL then
    begin
        k := (0);
        for i := 1 to elem.NPhases do
        begin
            elem.SensorQ^[i] := Value[k];
            inc(k);
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_kVS(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    elem: TSensorObj;
    i, k: Integer;
begin
    Value := PDoubleArray(ValuePtr);
    elem := ActiveSensor;
    if elem <> NIL then
    begin
        k := (0);
        for i := 1 to elem.NPhases do
        begin
            elem.SensorVoltage^[i] := Value[k];
            inc(k);
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_kWS(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    elem: TSensorObj;
    i, k: Integer;
begin
    Value := PDoubleArray(ValuePtr);
    elem := ActiveSensor;
    if elem <> NIL then
    begin
        k := (0);
        for i := 1 to elem.NPhases do
        begin
            elem.SensorP^[i] := Value[k];
            inc(k);
        end;
    end;
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
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    if DSSPrime.SensorClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.SensorClass.ElementList.Active;
        DSSPrime.ActiveCircuit.Sensors.Get(DSSPrime.SensorClass.Active);
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'Sensor "' + Value + '" Not Found in Active Circuit.', 5003);
    end;
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_PctError(Value: Double); CDECL;
begin
    Set_Parameter('%error', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_ReverseDelta(Value: Boolean); CDECL;
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
    elem := ActiveSensor;
    if elem <> NIL then
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
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.ActiveCircuit.Sensors.ActiveIndex
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure Sensors_Set_idx(Value: Integer); CDECL;
var
    pSensor: TSensorObj;
begin
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    pSensor := DSSPrime.ActiveCircuit.Sensors.Get(Value);
    if pSensor = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid Sensor index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pSensor;
end;
//------------------------------------------------------------------------------
end.
