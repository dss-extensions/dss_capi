unit DSensors;

interface

function SensorsI(mode: Longint; arg: Longint): Longint; CDECL;
function SensorsF(mode: Longint; arg: Double): Double; CDECL;
function SensorsS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure SensorsV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
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
    cmd := Format('sensor.%s.%s=%s', [ActiveSensor.Name, parm, val]);
    DSSExecutive[ActiveActor].Command := cmd;
end;

function SensorsI(mode: Longint; arg: Longint): Longint; CDECL;

var
    elem: TSensorObj;
    lst: TPointerList;

begin
    Result := 0;             // Default return value
    case mode of
        0:
        begin  // Sensors.count
            if Assigned(ActiveCircuit[ActiveActor]) then
                Result := ActiveCircuit[ActiveActor].Sensors.ListSize;
        end;
        1:
        begin // Sensors.First
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
        2:
        begin // Sensors.Next
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
        3:
        begin  // Sensors.IsDelta read
            Result := 0;
            elem := ActiveSensor;
            if elem <> NIL then
                if elem.Conn > 0 then
                    Result := 1;
        end;
        4:
        begin  // Sensors.IsDelta write
            elem := ActiveSensor;
            if elem <> NIL then
                elem.Conn := Integer(arg);
        end;
        5:
        begin  // Sensors.ReverseDelta read
            Result := 0;
            elem := ActiveSensor;
            if elem <> NIL then
                if elem.DeltaDirection < 0 then
                    Result := 1;
        end;
        6:
        begin  // Sensors.ReverseDelta write
            if arg = 1 then
                Set_Parameter('DeltaDirection', '-1')
            else
                Set_Parameter('DeltaDirection', '1');
        end;
        7:
        begin  // Sensors.MeteredTerminal read
            Result := 0;
            elem := ActiveSensor;
            if elem <> NIL then
                Result := elem.MeteredTerminal;
        end;
        8:
        begin  // Sensors.MeteredTerminal write
            Set_Parameter('terminal', IntToStr(arg));
        end;
        9:
        begin  // Sensors.Reset
            elem := ActiveSensor;
            if elem <> NIL then
                elem.ResetIt;
        end;
        10:
        begin  // Sensors.ResetAll
            if assigned(ActiveCircuit[ActiveActor]) then
                SensorClass[ActiveActor].ResetAll(ActiveActor);
        end
    else
        Result := -1;
    end;
end;

//***************************floating point type properties***********************
function SensorsF(mode: Longint; arg: Double): Double; CDECL;

var
    elem: TSensorObj;

begin
    Result := 0.0; // Default return value
    case mode of
        0:
        begin  // Sensors.PctError read
            Result := 0.0;
            elem := ActiveSensor;
            if elem <> NIL then
                Result := elem.pctError;
        end;
        1:
        begin  // Sensors.PctError write
            Set_Parameter('%error', FloatToStr(arg));
        end;
        2:
        begin  // Sensors.Weight read
            Result := 0.0;
            elem := ActiveSensor;
            if elem <> NIL then
                Result := elem.Weight;
        end;
        3:
        begin  // Sensors.weight write
            Set_Parameter('weight', FloatToStr(arg));
        end;
        4:
        begin  // Sensors.kVBase read
            Result := 0.0;
            elem := ActiveSensor;
            if elem <> NIL then
                Result := elem.BaseKV;
        end;
        5:
        begin  // Sensors.kVBase write
            Set_Parameter('kvbase', FloatToStr(arg));
        end
    else
        Result := -1.0;
    end;
end;

//*******************************String type properties***************************
function SensorsS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var
    elem: TSensorObj;
    ActiveSave: Integer;
    S: String;
    Found: Boolean;
    lst: TPointerList;

begin
    Result := Pansichar(Ansistring(''));// Default return value
    case mode of
        0:
        begin  // Sensors.Name read
            Result := Pansichar(Ansistring(''));
            elem := ActiveSensor;
            if elem <> NIL then
                Result := Pansichar(Ansistring(elem.Name));
        end;
        1:
        begin  // Sensors.Name write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                lst := ActiveCircuit[ActiveActor].Sensors;
                S := String(arg);  // Convert to Pascal String
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
        2:
        begin  // Sensors.MeteredElement read
            Result := Pansichar(Ansistring(''));
            elem := ActiveSensor;
            if elem <> NIL then
                Result := Pansichar(Ansistring(elem.ElementName));
        end;
        3:
        begin  // Sensors.MeteredElement write
            Set_Parameter('element', String(arg));
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not valid'));
    end;
end;

//***************************Variant type properties*****************************
procedure SensorsV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

var
    elem: TSensorObj;
    k,
    i: Integer;
    PDouble: ^Double;

begin
    case mode of
        0:
        begin // Sensors.AllNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    if Sensors.ListSize > 0 then
                    begin
                        elem := Sensors.First;
                        while elem <> NIL do
                        begin
                            WriteStr2Array(elem.Name);
                            WriteStr2Array(Char(0));
                            elem := Sensors.Next;
                        end;
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        1:
        begin // Sensors.Currents read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            elem := ActiveSensor;
            if elem <> NIL then
            begin
                setlength(myDBLArray, elem.NPhases);
                for k := 0 to elem.NPhases - 1 do
                    myDBLArray[k] := elem.SensorCurrent^[k + 1];
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        2:
        begin // Sensors.Currents write
            myType := 2;            // Double
            elem := ActiveSensor;
            k := 1;
            if elem <> NIL then
            begin
                for i := 1 to elem.NPhases do
                begin
                    PDouble := myPointer;
                    elem.SensorCurrent^[i] := PDouble^;
                    inc(Pbyte(myPointer), 8);
                    inc(k);
                end;
            end;
            mySize := k - 1;
        end;
        3:
        begin // Sensors.KVARS read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            elem := ActiveSensor;
            if elem <> NIL then
            begin
                setlength(myDBLArray, elem.NPhases);
                for k := 0 to elem.NPhases - 1 do
                    myDBLArray[k] := elem.SensorQ^[k + 1];
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        4:
        begin // Sensors.KVARS write
            myType := 2;            // Double
            k := 1;
            elem := ActiveSensor;
            if elem <> NIL then
            begin
                for i := 1 to elem.NPhases do
                begin
                    PDouble := myPointer;
                    elem.SensorQ^[i] := PDouble^;
                    inc(Pbyte(myPointer), 8);
                    inc(k);
                end;
            end;
            mySize := k - 1;
        end;
        5:
        begin // Sensors.KWS read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            elem := ActiveSensor;
            if elem <> NIL then
            begin
                setlength(myDBLArray, elem.NPhases);
                for k := 0 to elem.NPhases - 1 do
                    myDBLArray[k] := elem.SensorP^[k + 1];
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        6:
        begin // Sensors.KWS write
            myType := 2;            // Double
            k := 1;
            elem := ActiveSensor;
            if elem <> NIL then
            begin
                for i := 1 to elem.NPhases do
                begin
                    PDouble := myPointer;
                    elem.SensorP^[i] := PDouble^;
                    inc(Pbyte(myPointer), 8);
                    inc(k);
                end;
            end;
            mySize := k - 1;
        end;
        7:
        begin  // Sensors.AllocFactors read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            elem := ActiveSensor;
            if elem <> NIL then
            begin
                setlength(myDBLArray, elem.NPhases);
                for k := 0 to elem.NPhases - 1 do
                    myDBLArray[k] := elem.PhsAllocationFactor^[k + 1];
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end
    else
    begin
        myType := 4;        // String
        setlength(myStrArray, 0);
        WriteStr2Array('Error, parameter not recognized');
        myPointer := @(myStrArray[0]);
        mySize := Length(myStrArray);
    end;
    end;
end;

end.
