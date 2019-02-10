unit ImplSensors;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl;

type
    TSensors = class(TAutoObject, ISensors)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_Currents: Olevariant; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_IsDelta: Wordbool; SAFECALL;
        function Get_kVARS: Olevariant; SAFECALL;
        function Get_kVS: Olevariant; SAFECALL;
        function Get_kWS: Olevariant; SAFECALL;
        function Get_MeteredElement: Widestring; SAFECALL;
        function Get_MeteredTerminal: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_PctError: Double; SAFECALL;
        function Get_ReverseDelta: Wordbool; SAFECALL;
        function Get_Weight: Double; SAFECALL;
        procedure Reset; SAFECALL;
        procedure ResetAll; SAFECALL;
        procedure Set_Currents(Value: Olevariant); SAFECALL;
        procedure Set_IsDelta(Value: Wordbool); SAFECALL;
        procedure Set_kVARS(Value: Olevariant); SAFECALL;
        procedure Set_kVS(Value: Olevariant); SAFECALL;
        procedure Set_kWS(Value: Olevariant); SAFECALL;
        procedure Set_MeteredElement(const Value: Widestring); SAFECALL;
        procedure Set_MeteredTerminal(Value: Integer); SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        procedure Set_PctError(Value: Double); SAFECALL;
        procedure Set_ReverseDelta(Value: Wordbool); SAFECALL;
        procedure Set_Weight(Value: Double); SAFECALL;
        function Get_kVbase: Double; SAFECALL;
        procedure Set_kVbase(Value: Double); SAFECALL;

    end;

implementation

uses
    ComServ,
    Sensor,
    Variants,
    DSSGlobals,
    PointerList,
    Executive,
    SysUtils;

function ActiveSensor: TSensorObj;
begin
    Result := NIL;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Sensors.Active;
end;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit[ActiveActor]) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('capacitor.%s.%s=%s', [ActiveSensor.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;

function TSensors.Get_AllNames: Olevariant;
var
    elem: TSensorObj;
    k: Integer;
begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if Sensors.ListSize > 0 then
            begin
                VarArrayRedim(Result, Sensors.ListSize - 1);
                k := 0;
                elem := Sensors.First;
                while elem <> NIL do
                begin
                    Result[k] := elem.Name;
                    Inc(k);
                    elem := Sensors.Next;
                end;
            end;
end;

function TSensors.Get_Count: Integer;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        Result := ActiveCircuit[ActiveActor].Sensors.ListSize;
end;

function TSensors.Get_Currents: Olevariant;
var
    elem: TSensorObj;
    k: Integer;
begin
    elem := ActiveSensor;
    if elem <> NIL then
    begin
        Result := VarArrayCreate([0, elem.NPhases - 1], varDouble);
        for k := 0 to elem.NPhases - 1 do
            Result[k] := elem.SensorCurrent^[k + 1];
    end
    else
        Result := VarArrayCreate([0, 0], varDouble);
end;

function TSensors.Get_First: Integer;
var
    elem: TSensorObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        lst := ActiveCircuit[ActiveActor].Sensors;
        elem := lst.First;
        if elem <> NIL then
        begin
            repeat
                if elem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                    Result := 1;
                end
                else
                    elem := lst.Next;
            until (Result = 1) or (elem = NIL);
        end;
    end;
end;

function TSensors.Get_IsDelta: Wordbool;
var
    elem: TSensorObj;
begin
    Result := FALSE;
    elem := ActiveSensor;
    if elem <> NIL then
        if elem.Conn > 0 then
            Result := TRUE;
end;

function TSensors.Get_kVARS: Olevariant;
var
    elem: TSensorObj;
    k: Integer;
begin
    elem := ActiveSensor;
    if elem <> NIL then
    begin
        Result := VarArrayCreate([0, elem.NPhases - 1], varDouble);
        for k := 0 to elem.NPhases - 1 do
            Result[k] := elem.SensorQ^[k + 1];
    end
    else
        Result := VarArrayCreate([0, 0], varDouble);
end;

function TSensors.Get_kVS: Olevariant;
var
    elem: TSensorObj;
    k: Integer;
begin
    elem := ActiveSensor;
    if elem <> NIL then
    begin
        Result := VarArrayCreate([0, elem.NPhases - 1], varDouble);
        for k := 0 to elem.NPhases - 1 do
            Result[k] := elem.SensorVoltage^[k + 1];
    end
    else
        Result := VarArrayCreate([0, 0], varDouble);
end;

function TSensors.Get_kWS: Olevariant;
var
    elem: TSensorObj;
    k: Integer;
begin
    elem := ActiveSensor;
    if elem <> NIL then
    begin
        Result := VarArrayCreate([0, elem.NPhases - 1], varDouble);
        for k := 0 to elem.NPhases - 1 do
            Result[k] := elem.SensorP^[k + 1];
    end
    else
        Result := VarArrayCreate([0, 0], varDouble);
end;

function TSensors.Get_MeteredElement: Widestring;
var
    elem: TSensorObj;
begin
    Result := '';
    elem := ActiveSensor;
    if elem <> NIL then
        Result := elem.ElementName;
end;

function TSensors.Get_MeteredTerminal: Integer;
var
    elem: TSensorObj;
begin
    Result := 0;
    elem := ActiveSensor;
    if elem <> NIL then
        Result := elem.MeteredTerminal;
end;

function TSensors.Get_Name: Widestring;
var
    elem: TSensorObj;
begin
    Result := '';
    elem := ActiveSensor;
    if elem <> NIL then
        Result := elem.Name;
end;

function TSensors.Get_Next: Integer;
var
    elem: TSensorObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        lst := ActiveCircuit[ActiveActor].Sensors;
        elem := lst.Next;
        if elem <> NIL then
        begin
            repeat
                if elem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                    Result := lst.ActiveIndex;
                end
                else
                    elem := lst.Next;
            until (Result > 0) or (elem = NIL);
        end
    end;
end;

function TSensors.Get_PctError: Double;
var
    elem: TSensorObj;
begin
    Result := 0.0;
    elem := ActiveSensor;
    if elem <> NIL then
        Result := elem.pctError;
end;

function TSensors.Get_ReverseDelta: Wordbool;
var
    elem: TSensorObj;
begin
    Result := FALSE;
    elem := ActiveSensor;
    if elem <> NIL then
        if elem.DeltaDirection < 0 then
            Result := TRUE;
end;

function TSensors.Get_Weight: Double;
var
    elem: TSensorObj;
begin
    Result := 0.0;
    elem := ActiveSensor;
    if elem <> NIL then
        Result := elem.Weight;
end;

procedure TSensors.Reset;
var
    elem: TSensorObj;
begin
    elem := ActiveSensor;
    if elem <> NIL then
        elem.ResetIt;
end;

procedure TSensors.ResetAll;
begin
    if assigned(ActiveCircuit[ActiveActor]) then
        SensorClass[ActiveActor].ResetAll(ActiveActor);
end;

procedure TSensors.Set_Currents(Value: Olevariant);
var
    elem: TSensorObj;
    i, k: Integer;
begin
    elem := ActiveSensor;
    if elem <> NIL then
    begin
        k := VarArrayLowBound(Value, 1);
        for i := 1 to elem.NPhases do
        begin
            elem.SensorCurrent^[i] := Value[k];
            inc(k);
        end;
    end;
end;

procedure TSensors.Set_IsDelta(Value: Wordbool);
var
    elem: TSensorObj;
begin
    elem := ActiveSensor;
    if elem <> NIL then
        elem.Conn := Integer(Value);
end;

procedure TSensors.Set_kVARS(Value: Olevariant);
var
    elem: TSensorObj;
    i, k: Integer;
begin
    elem := ActiveSensor;
    if elem <> NIL then
    begin
        k := VarArrayLowBound(Value, 1);
        for i := 1 to elem.NPhases do
        begin
            elem.SensorQ^[i] := Value[k];
            inc(k);
        end;
    end;
end;

procedure TSensors.Set_kVS(Value: Olevariant);
var
    elem: TSensorObj;
    i, k: Integer;
begin
    elem := ActiveSensor;
    if elem <> NIL then
    begin
        k := VarArrayLowBound(Value, 1);
        for i := 1 to elem.NPhases do
        begin
            elem.SensorVoltage^[i] := Value[k];
            inc(k);
        end;
    end;
end;

procedure TSensors.Set_kWS(Value: Olevariant);
var
    elem: TSensorObj;
    i, k: Integer;
begin
    elem := ActiveSensor;
    if elem <> NIL then
    begin
        k := VarArrayLowBound(Value, 1);
        for i := 1 to elem.NPhases do
        begin
            elem.SensorP^[i] := Value[k];
            inc(k);
        end;
    end;
end;

procedure TSensors.Set_MeteredElement(const Value: Widestring);
begin
    Set_Parameter('element', Value);
end;

procedure TSensors.Set_MeteredTerminal(Value: Integer);
begin
    Set_Parameter('terminal', IntToStr(Value));
end;

procedure TSensors.Set_Name(const Value: Widestring);
var
    ActiveSave: Integer;
    S: String;
    Found: Boolean;
    elem: TSensorObj;
    lst: TPointerList;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        lst := ActiveCircuit[ActiveActor].Sensors;
        S := Value;  // Convert to Pascal String
        Found := FALSE;
        ActiveSave := lst.ActiveIndex;
        elem := lst.First;
        while elem <> NIL do
        begin
            if (CompareText(elem.Name, S) = 0) then
            begin
                ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                Found := TRUE;
                Break;
            end;
            elem := lst.Next;
        end;
        if not Found then
        begin
            DoSimpleMsg('Sensor "' + S + '" Not Found in Active Circuit.', 5003);
            elem := lst.Get(ActiveSave);
            ActiveCircuit[ActiveActor].ActiveCktElement := elem;
        end;
    end;
end;

procedure TSensors.Set_PctError(Value: Double);
begin
    Set_Parameter('%error', FloatToStr(Value));
end;

procedure TSensors.Set_ReverseDelta(Value: Wordbool);
begin
    if Value = TRUE then
        Set_Parameter('DeltaDirection', '-1')
    else
        Set_Parameter('DeltaDirection', '1');
end;

procedure TSensors.Set_Weight(Value: Double);
begin
    Set_Parameter('weight', FloatToStr(Value));
end;

function TSensors.Get_kVbase: Double;
var
    elem: TSensorObj;
begin
    Result := 0.0;
    elem := ActiveSensor;
    if elem <> NIL then
        Result := elem.BaseKV;
end;

procedure TSensors.Set_kVbase(Value: Double);
begin
    Set_Parameter('kvbase', FloatToStr(Value));
end;

initialization
    TAutoObjectFactory.Create(ComServer, TSensors, Class_Sensors,
        ciInternal, tmApartment);
end.
