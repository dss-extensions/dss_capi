unit DCapacitors;

interface

function CapacitorsI(mode: Longint; arg: Longint): Longint; CDECL;
function CapacitorsF(mode: Longint; arg: Double): Double; CDECL;
function CapacitorsS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure CapacitorsV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
    DSSGlobals,
    Executive,
    Capacitor,
    Variants,
    SysUtils,
    PointerList;

function ActiveCapacitor: TCapacitorObj;
begin
    Result := NIL;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].ShuntCapacitors.Active;
end;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit[ActiveActor]) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('capacitor.%s.%s=%s', [ActiveCapacitor.Name, parm, val]);
    DSSExecutive[ActiveActor].Command := cmd;
end;

function CapacitorsI(mode: Longint; arg: Longint): Longint; CDECL;

var
    elem: TCapacitorObj;
    lst: TPointerList;
    i: Integer;

begin
    Result := 0;
    case mode of
        0:
        begin  // Capacitors.NumSteps read
            Result := 0;
            elem := ActiveCapacitor;
            if elem <> NIL then
                Result := elem.NumSteps;
        end;
        1:
        begin  // Capacitors.NumSteps write
            Set_Parameter('numsteps', IntToStr(arg));
        end;
        2:
        begin  // Capacitors.IsDelta read
            Result := 0;
            elem := ActiveCapacitor;
            if elem <> NIL then
                if elem.Connection > 0 then
                    Result := 1;
        end;
        3:
        begin  // Capacitors.IsDelta write
            elem := ActiveCapacitor;
            if elem <> NIL then
                elem.Connection := Integer(arg);
        end;
        4:
        begin  // Capacitors.First
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                lst := ActiveCircuit[ActiveActor].ShuntCapacitors;
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
        5:
        begin  // Capacitors.Next
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                lst := ActiveCircuit[ActiveActor].ShuntCapacitors;
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
        6:
        begin  // Capacitors.Count
            if Assigned(ActiveCircuit[ActiveActor]) then
                Result := ActiveCircuit[ActiveActor].ShuntCapacitors.ListSize;
        end;
        7:
        begin  // Capacitors.AddStep
            elem := ActiveCapacitor;
            if elem <> NIL then
            begin
                if elem.AddStep(ActiveActor) then
                    Result := 1;
            end;
        end;
        8:
        begin  // Capacitors.SubtractStep
            elem := ActiveCapacitor;
            if elem <> NIL then
            begin
                if elem.SubtractStep(ActiveActor) then
                    Result := 1;
            end;
        end;
        9:
        begin  // Capacitors.AvailableSteps
            elem := ActiveCapacitor;
            if elem <> NIL then
                Result := elem.AvailableSteps;
        end;
        10:
        begin  // Capacitors.Open
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    elem := ActiveCapacitor;
                    if elem <> NIL then
                        with elem do
                        begin
                            for i := 1 to NumSteps do
                                States[i, ActiveActor] := 0;   // open all steps
                        end;
                end;
        end;
        11:
        begin  // Capacitors.Close
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    elem := ActiveCapacitor;
                    if elem <> NIL then
                        with elem do
                        begin
                            ActiveTerminal := Terminals^[1];  // make sure terminal 1 is closed
                            Closed[0, ActiveActor] := TRUE;    // closes all phases
                            for i := 1 to NumSteps do
                                States[i, ActiveActor] := 1;
                        end;
                end;
        end
    else
        Result := -1;
    end;
end;

//***************************Floating point type properties**********************
function CapacitorsF(mode: Longint; arg: Double): Double; CDECL;

var
    elem: TCapacitorObj;

begin
    Result := 0.0; // Default return value
    case mode of
        0:
        begin  // Capacitors.kV read
            Result := 0.0;
            elem := ActiveCapacitor;
            if elem <> NIL then
                Result := elem.NomKV;
        end;
        1:
        begin  // Capacitors.kV write
            Set_Parameter('kv', FloatToStr(arg));
        end;
        2:
        begin  // Capacitors.kvar read
            Result := 0.0;
            elem := ActiveCapacitor;
            if elem <> NIL then
                Result := elem.Totalkvar;
        end;
        3:
        begin  // Capacitors.kvar write
            Set_Parameter('kvar', FloatToStr(arg));
        end
    else
        Result := -1.0;
    end;
end;

//*******************************String type properties***************************
function CapacitorsS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var
    elem: TCapacitorObj;
    ActiveSave: Integer;
    S: String;
    Found: Boolean;
    lst: TPointerList;

begin
    Result := Pansichar(Ansistring('0'));  // default return value
    case mode of
        0:
        begin  // Capacitors.Name read
            Result := Pansichar(Ansistring(''));
            elem := ActiveCapacitor;
            if elem <> NIL then
                Result := Pansichar(Ansistring(elem.Name));
        end;
        1:
        begin  // Capacitors.Name write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                lst := ActiveCircuit[ActiveActor].ShuntCapacitors;
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
                    DoSimpleMsg('Capacitor "' + S + '" Not Found in Active Circuit.', 5003);
                    elem := lst.Get(ActiveSave);    // Restore active Capacitor
                    ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                end;
            end;
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not valid'));
    end;
end;

//*********************************Variant type properties***********************
procedure CapacitorsV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

var
    elem: TCapacitorObj;
    S: String;
    lst: TPointerList;
    i, k: Integer;
    Pint: ^Integer;

begin
    case mode of
        0:
        begin  // Capacitors.AllNames
            setlength(myStrArray, 0);
            mySize := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                    if ShuntCapacitors.ListSize > 0 then
                    begin
                        setlength(myStrArray, 0);
                        lst := ShuntCapacitors;
                        elem := lst.First;
                        while elem <> NIL do
                        begin
                            WriteStr2Array(elem.Name);
                            WriteStr2Array(Char(0));
                            elem := lst.Next;
                        end;
                    end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myType := 4;                  // String
            mySize := length(myStrArray);
            myPointer := @(myStrArray[0]);
        end;

        1:
        begin  // Capacitors.States read
            myType := 1;                  // Integer
            mySize := 4; // unless Numsteps > 1 below
            setlength(myIntArray, 1);
            myIntArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Elem := ActiveCapacitor;
                if Elem <> NIL then
                begin
                    setlength(myIntArray, elem.Numsteps);
                    mySize := 4 * (elem.Numsteps);
                    k := 0;
                    for i := 1 to elem.Numsteps do
                    begin
                        myIntArray[k] := elem.States[i, ActiveActor];
                        Inc(k);
                    end;
                end;
            end;
            myPointer := @myIntArray[0];
        end;
        2:
        begin  // Capacitors.States write
            elem := ActiveCapacitor;
            if elem <> NIL then
            begin
         // allocate space based on present value of NumSteps
         // setting NumSteps allocates the memory
         // only put as many elements as proviced up to nZIPV
//         myIntArray   :=  myPointer;
                for i := 1 to elem.Numsteps do
                begin
                    PInt := myPointer;
                    elem.States[i, ActiveActor] := PInt^;
                    inc(Pbyte(myPointer), 4);
                end;
                elem.FindLastStepInService;
            end;
            myType := 1;                  // Integer
        end
    else
        setlength(myStrArray, 1);
        myStrArray[0] := 0;
        myPointer := @(myStrArray[0]);
    end;
end;

end.
