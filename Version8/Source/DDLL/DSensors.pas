unit DSensors;

interface

function SensorsI(mode:longint; arg:longint):longint;cdecl;
function SensorsF(mode:longint; arg:double):double;cdecl;
function SensorsS(mode:longint; arg:pAnsiChar):pAnsiChar;cdecl;
procedure SensorsV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

implementation

uses Sensor, Variants, DSSGlobals, PointerList, Executive, SysUtils;

function ActiveSensor: TSensorObj;
begin
  Result := nil;
  if ActiveCircuit[ActiveActor] <> Nil then Result := ActiveCircuit[ActiveActor].Sensors.Active;
end;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit[ActiveActor]) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('sensor.%s.%s=%s', [ActiveSensor.Name, parm, val]);
  DSSExecutive[ActiveActor].Command := cmd;
end;

function SensorsI(mode:longint; arg:longint):longint;cdecl;

Var
  elem: TSensorObj;
  lst: TPointerList;

begin
  Result:=0;             // Default return value
  case mode of
  0: begin  // Sensors.count
      If Assigned(ActiveCircuit[ActiveActor]) Then
        Result := ActiveCircuit[ActiveActor].Sensors.ListSize;
  end;
  1: begin // Sensors.First
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then begin
        lst := ActiveCircuit[ActiveActor].Sensors;
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
  2: begin // Sensors.Next
    Result := 0;
    If ActiveCircuit[ActiveActor] <> Nil Then Begin
      lst := ActiveCircuit[ActiveActor].Sensors;
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
  3: begin  // Sensors.IsDelta read
      Result := 0;
      elem := ActiveSensor;
      if elem <> nil then
        if elem.Conn > 0 then Result := 1;
  end;
  4: begin  // Sensors.IsDelta write
      elem := ActiveSensor;
      if elem <> nil then elem.Conn := Integer (arg);
  end;
  5: begin  // Sensors.ReverseDelta read
      Result := 0;
      elem := ActiveSensor;
      if elem <> nil then
        if elem.DeltaDirection < 0 then Result := 1;
  end;
  6: begin  // Sensors.ReverseDelta write
    if arg = 1 then
      Set_Parameter ('DeltaDirection', '-1')
    else
      Set_Parameter ('DeltaDirection', '1');
  end;
  7: begin  // Sensors.MeteredTerminal read
      Result := 0;
      elem := ActiveSensor;
      If elem <> Nil Then Result := elem.MeteredTerminal;
  end;
  8: begin  // Sensors.MeteredTerminal write
      Set_Parameter ('terminal', IntToStr(arg));
  end;
  9: begin  // Sensors.Reset
      elem := ActiveSensor;
      If elem <> Nil Then elem.ResetIt;
  end;
  10: begin  // Sensors.ResetAll
      if assigned(ActiveCircuit[ActiveActor]) then SensorClass[ActiveActor].ResetAll(ActiveActor);
  end
  else
      Result:=-1;
  end;
end;

//***************************floating point type properties***********************
function SensorsF(mode:longint; arg:double):double;cdecl;

Var
  elem: TSensorObj;

begin
  Result:=0.0; // Default return value
  case mode of
  0: begin  // Sensors.PctError read
      Result := 0.0;
      elem := ActiveSensor;
      If elem <> Nil Then Result := elem.pctError;
  end;
  1: begin  // Sensors.PctError write
      Set_Parameter ('%error', FloatToStr(arg));
  end;
  2: begin  // Sensors.Weight read
      Result := 0.0;
      elem := ActiveSensor;
      If elem <> Nil Then Result := elem.Weight;
  end;
  3: begin  // Sensors.weight write
      Set_Parameter ('weight', FloatToStr(arg));
  end;
  4: begin  // Sensors.kVBase read
      Result := 0.0;
      elem := ActiveSensor;
      If elem <> Nil Then Result := elem.BaseKV;
  end;
  5: begin  // Sensors.kVBase write
      Set_Parameter ('kvbase', FloatToStr(arg));
  end
  else
      Result:=-1.0;
  end;
end;

//*******************************String type properties***************************
function SensorsS(mode:longint; arg:pAnsiChar):pAnsiChar;cdecl;

Var
  elem: TSensorObj;
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  lst: TPointerList;

begin
  Result := pAnsiChar(AnsiString(''));// Default return value
  case mode of
  0: begin  // Sensors.Name read
      Result := pAnsiChar(AnsiString(''));
      elem := ActiveSensor;
      If elem <> Nil Then Result := pAnsiChar(AnsiString(elem.Name));
  end;
  1: begin  // Sensors.Name write
      IF ActiveCircuit[ActiveActor] <> NIL THEN Begin
        lst := ActiveCircuit[ActiveActor].Sensors;
        S := string(arg);  // Convert to Pascal String
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
          DoSimpleMsg('Sensor "'+S+'" Not Found in Active Circuit.', 5003);
          elem := lst.Get(ActiveSave);
          ActiveCircuit[ActiveActor].ActiveCktElement := elem;
        End;
      End;
  end;
  2: begin  // Sensors.MeteredElement read
      Result := pAnsiChar(AnsiString(''));
      elem := ActiveSensor;
      If elem <> Nil Then Result := pAnsiChar(AnsiString(elem.ElementName));
  end;
  3: begin  // Sensors.MeteredElement write
      Set_Parameter ('element', string(arg));
  end
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

//***************************Variant type properties*****************************
procedure SensorsV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

Var
  elem    : TSensorObj;
  k ,
  i       : Integer;
  PDouble : ^Double;

begin
  case mode of
  0:begin // Sensors.AllNames
      myType  :=  4;        // String
      setlength(myStrArray,0);
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        WITH ActiveCircuit[ActiveActor] DO
        Begin
          If Sensors.ListSize>0 Then
          Begin
            elem := Sensors.First;
            WHILE elem<>Nil DO Begin
              WriteStr2Array(elem.Name);
              WriteStr2Array(Char(0));
              elem := Sensors.Next;
            End;
          End;
        End;
      End;
      if (length(myStrArray) = 0) then
        WriteStr2Array('None');
      myPointer :=  @(myStrArray[0]);
      mySize    :=  Length(myStrArray);
    end;
  1:begin // Sensors.Currents read
      myType  :=  2;        // Double
      setlength(myDBLArray, 1);
      myDBLArray[0] := 0;
      elem := ActiveSensor;
      if elem <> Nil then
      begin
        setlength(myDBLArray, elem.NPhases );
        for k := 0 to elem.NPhases-1 do
          myDBLArray[k] := elem.SensorCurrent^[k+1];
      end;
      myPointer :=  @(myDBLArray[0]);
      mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
    end;
  2:begin // Sensors.Currents write
      myType:=  2;            // Double
      elem  := ActiveSensor;
      k     :=  1;
      if elem <> nil then
      begin
        for i := 1 to elem.NPhases do
        begin
          PDouble                 :=  myPointer;
          elem.SensorCurrent^[i]  :=  PDouble^;
          inc(PByte(myPointer),8);
          inc(k);
        end;
      end;
      mySize  :=  k - 1;
    end;
  3:begin // Sensors.KVARS read
      myType  :=  2;        // Double
      setlength(myDBLArray, 1);
      myDBLArray[0] := 0;
      elem := ActiveSensor;
      if elem <> Nil then begin
        setlength(myDBLArray, elem.NPhases);
        for k := 0 to elem.NPhases-1 do
          myDBLArray[k] := elem.SensorQ^[k+1];
      end;
      myPointer :=  @(myDBLArray[0]);
      mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
    end;
  4:begin // Sensors.KVARS write
      myType:=  2;            // Double
      k     :=  1;
      elem := ActiveSensor;
      if elem <> nil then
      begin
        for i := 1 to elem.NPhases do
        begin
          PDouble          :=  myPointer;
          elem.SensorQ^[i] := PDouble^;
          inc(PByte(myPointer),8);
          inc(k);
        end;
      end;
      mySize  :=  k - 1;
    end;
  5:begin // Sensors.KWS read
      myType  :=  2;        // Double
      setlength(myDBLArray, 1);
      myDBLArray[0] := 0;
      elem := ActiveSensor;
      if elem <> Nil then begin
        setlength(myDBLArray, elem.NPhases);
        for k := 0 to elem.NPhases-1 do
          myDBLArray[k] := elem.SensorP^[k+1];
      end;
      myPointer :=  @(myDBLArray[0]);
      mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
    end;
  6:begin // Sensors.KWS write
      myType:=  2;            // Double
      k     :=  1;
      elem := ActiveSensor;
      if elem <> nil then
      begin
        for i := 1 to elem.NPhases do
        begin
          PDouble          := myPointer;
          elem.SensorP^[i] := PDouble^;
          inc(PByte(myPointer),8);
          inc(k);
        end;
      end;
      mySize  :=  k - 1;
    end;
  7:begin  // Sensors.AllocFactors read
      myType  :=  2;        // Double
      setlength(myDBLArray, 1);
      myDBLArray[0] := 0;
      elem :=  ActiveSensor;
      If elem <> Nil Then
      Begin
          setlength(myDBLArray, elem.NPhases);
          FOR k := 0 to elem.NPhases-1 DO
            myDBLArray[k] := elem.PhsAllocationFactor^[k+1];
      End;
      myPointer :=  @(myDBLArray[0]);
      mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
    end
  else
    Begin
      myType  :=  4;        // String
      setlength(myStrArray, 0);
      WriteStr2Array('Error, parameter not recognized');
      myPointer :=  @(myStrArray[0]);
      mySize    :=  Length(myStrArray);
    End;
  end;
end;

end.
