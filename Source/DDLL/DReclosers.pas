unit DReclosers;

interface

function ReclosersI(mode: Longint; arg: Longint): Longint; CDECL;
function ReclosersF(mode: Longint; arg: Double): Double; CDECL;
function ReclosersS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure ReclosersV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
    Executive,
    Sysutils,
    ControlElem,
    Recloser,
    PointerList,
    Variants,
    DSSGlobals,
    DSSClassDefs;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit[ActiveActor]) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('recloser.%s.%s=%s', [TRecloserObj(RecloserClass.GetActiveObj).Name, parm, val]);
    DSSExecutive[ActiveActor].Command := cmd;
end;

function ReclosersI(mode: Longint; arg: Longint): Longint; CDECL;

var
    pElem: TRecloserObj;
    elem: TRecloserObj;
    pRecloser: TRecloserObj;

begin
    Result := 0; // Default return value
    case mode of
        0:
        begin  // Reclosers.Count
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := RecloserClass.ElementList.ListSize;
        end;
        1:
        begin  // Reclosers.First
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pElem := RecloserClass.ElementList.First;
                if pElem <> NIL then
                    repeat
                        if pElem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                            Result := 1;
                        end
                        else
                            pElem := RecloserClass.ElementList.Next;
                    until (Result = 1) or (pElem = NIL);
            end;
        end;
        2:
        begin  // Reclosers.Next
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pElem := RecloserClass.ElementList.Next;
                if pElem <> NIL then
                    repeat
                        if pElem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                            Result := RecloserClass.ElementList.ActiveIndex;
                        end
                        else
                            pElem := RecloserClass.ElementList.Next;
                    until (Result > 0) or (pElem = NIL);
            end;
        end;
        3:
        begin  // Reclosers.MonitoredTerm read
            Result := 0;
            elem := RecloserClass.GetActiveObj;
            if elem <> NIL then
                Result := elem.MonitoredElementTerminal;
        end;
        4:
        begin  // Reclosers.MonitoredTerm write
            elem := RecloserClass.GetActiveObj;
            if elem <> NIL then
                Set_parameter('monitoredterm', IntToStr(arg));
        end;
        5:
        begin  // Reclosers.SwitchedTerm read
            Result := 0;
            elem := RecloserClass.GetActiveObj;
            if elem <> NIL then
                Result := elem.ElementTerminal;
        end;
        6:
        begin  // Reclosers.SwitchedTerm write
            elem := RecloserClass.GetActiveObj;
            if elem <> NIL then
                Set_parameter('SwitchedTerm', IntToStr(arg));
        end;
        7:
        begin  // Reclosers.NumFast read
            Result := 0;
            elem := RecloserClass.ElementList.Active;
            ;
            if elem <> NIL then
                Result := elem.NumFast;
        end;
        8:
        begin  // Reclosers.NumFast write
            elem := RecloserClass.ElementList.Active;
            if elem <> NIL then
                Set_parameter('numfast', IntToStr(arg));
        end;
        9:
        begin  // Reclosers.Shots read
            Result := 0;
            elem := RecloserClass.ElementList.Active;
            ;
            if elem <> NIL then
                Result := elem.NumReclose + 1;
        end;
        10:
        begin  // Reclosers.Shots write
            elem := RecloserClass.ElementList.Active;
            if elem <> NIL then
                Set_parameter('shots', IntToStr(arg));
        end;
        11:
        begin  // Recloser.Open                                     // TODO
            elem := RecloserClass.ElementList.Active;
            if elem <> NIL then
                elem.PresentState := CTRL_OPEN;
        end;
        12:
        begin  // Reclosers.Close                                  // TODO
            elem := RecloserClass.ElementList.Active;
            if elem <> NIL then
                elem.PresentState := CTRL_CLOSE;
        end;
        13:
        begin // Reclosers.Idx read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := RecloserClass.ElementList.ActiveIndex
            else
                Result := 0;
        end;
        14:
        begin // Reclosers.Idx write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pRecloser := RecloserClass.Elementlist.Get(arg);
                if pRecloser <> NIL then
                    ActiveCircuit[ActiveActor].ActiveCktElement := pRecloser;
            end;
        end;
        15:
        begin  // Reclosers.Reset                                  // TODO
            elem := RecloserClass.ElementList.Active;
            if elem <> NIL then
                elem.Reset(ActiveActor);
        end
    else
        Result := -1;
    end;
end;

//********************Floating point type properties******************************
function ReclosersF(mode: Longint; arg: Double): Double; CDECL;

var
    elem: TRecloserObj;

begin
    Result := 0.0; // Default return value
    case mode of
        0:
        begin  // Reclosers.PhaseTrip read
            Result := 0;
            elem := RecloserClass.ElementList.Active;
            if elem <> NIL then
                Result := elem.PhaseTrip;
        end;
        1:
        begin  // Reclosers.PhaseTrip write
            elem := RecloserClass.ElementList.Active;
            if elem <> NIL then
                Set_parameter('PhaseTrip', Format('%.g', [arg]));
        end;
        2:
        begin  // Reclosers.PhaseInst read
            Result := 0;
            elem := RecloserClass.ElementList.Active;
            if elem <> NIL then
                Result := elem.PhaseInst;
        end;
        3:
        begin  // Reclosers.PhaseInst write
            elem := RecloserClass.ElementList.Active;
            if elem <> NIL then
                Set_parameter('Phaseinst', Format('%.g', [arg]));
        end;
        4:
        begin  // Reclosers.GroundTrip read
            Result := 0;
            elem := RecloserClass.ElementList.Active;
            if elem <> NIL then
                Result := elem.GroundTrip;
        end;
        5:
        begin  // Reclosers.GroundTrip write
            elem := RecloserClass.ElementList.Active;
            if elem <> NIL then
                Set_parameter('GroundTrip', Format('%.g', [arg]));
        end;
        6:
        begin  // Reclosers.GroundInst read
            Result := 0;
            elem := RecloserClass.ElementList.Active;
            if elem <> NIL then
                Result := elem.GroundInst;
        end;
        7:
        begin  // Reclosers.GroundInst write
            elem := RecloserClass.ElementList.Active;
            if elem <> NIL then
                Set_parameter('GroundInst', Format('%.g', [arg]));
        end
    else
        Result := -1.0;
    end;
end;

//********************String type properties******************************
function ReclosersS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var
    elem: TRecloserObj;

begin
    Result := Pansichar(Ansistring('')); // Default return value
    case mode of
        0:
        begin  // Reclosers.Name read
            Result := Pansichar(Ansistring(''));
            elem := RecloserClass.GetActiveObj;
            if elem <> NIL then
                Result := Pansichar(Ansistring(elem.Name));
        end;
        1:
        begin  // Reclosers.Name write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if RecloserClass.SetActive(String(arg)) then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := RecloserClass.ElementList.Active;
                end
                else
                begin
                    DoSimpleMsg('Recloser "' + arg + '" Not Found in Active Circuit.', 77003);
                end;
            end;
        end;
        2:
        begin  // Reclosers.MonitoredObj read
            Result := Pansichar(Ansistring(''));
            elem := RecloserClass.GetActiveObj;
            if elem <> NIL then
                Result := Pansichar(Ansistring(elem.MonitoredElementName));
        end;
        3:
        begin  // Reclosers.MonitoredObj write
            elem := RecloserClass.GetActiveObj;
            if elem <> NIL then
                Set_parameter('monitoredObj', String(arg));
        end;
        4:
        begin  // Reclosers.SwitchedObj read
            Result := Pansichar(Ansistring(''));
            elem := RecloserClass.ElementList.Active;
            if elem <> NIL then
                Result := Pansichar(Ansistring(elem.ElementName));
        end;
        5:
        begin  // Reclosers.SwitchedObj write
            elem := RecloserClass.GetActiveObj;
            if elem <> NIL then
                Set_parameter('SwitchedObj', String(arg));
        end;
        6:
        begin  // Reclosers.State read                                          // TODO
            Result := Pansichar(Ansistring(''));
            elem := RecloserClass.GetActiveObj;
            if elem <> NIL then
            begin
                if elem.PresentState = CTRL_CLOSE then
                    Result := Pansichar(Ansistring('closed'))
                else
                    Result := Pansichar(Ansistring('open'));
            end;

        end;
        7:
        begin  // Reclosers.State write                                         // TODO
            elem := RecloserClass.GetActiveObj;
            if elem <> NIL then
            begin
                if LowerCase(String(arg))[1] = 'c' then
                    elem.PresentState := CTRL_CLOSE
                else
                    elem.PresentState := CTRL_OPEN;
            end;
        end;
        8:
        begin  // Reclosers.Normal read                                         // TODO
            Result := Pansichar(Ansistring(''));
            elem := RecloserClass.GetActiveObj;
            if elem <> NIL then
            begin
                if elem.NormalState = CTRL_CLOSE then
                    Result := Pansichar(Ansistring('closed'))
                else
                    Result := Pansichar(Ansistring('open'));
            end;
        end;
        9:
        begin  // Reclosers.Normal write                                        // TODO
            elem := RecloserClass.GetActiveObj;
            if elem <> NIL then
            begin
                if LowerCase(String(arg))[1] = 'c' then
                    elem.NormalState := CTRL_CLOSE
                else
                    elem.NormalState := CTRL_OPEN;
            end;
        end;
    else
        Result := Pansichar(Ansistring('Error, parameter not valid'));
    end;
end;

//********************Variant type properties******************************
procedure ReclosersV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

var
    elem: TRecloserObj;
    pList: TPointerList;
    k, i: Integer;

begin
    case mode of
        0:
        begin  // Reclosers.AllNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if RecloserClass.ElementList.ListSize > 0 then
                begin
                    pList := RecloserClass.ElementList;
                    elem := pList.First;
                    while elem <> NIL do
                    begin
                        WriteStr2Array(elem.Name);
                        WriteStr2Array(Char(0));
                        elem := pList.next;
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        1:
        begin  // Reclosers.RecloseIntervals
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                elem := RecloserClass.ElementList.Active;
                if elem <> NIL then
                begin
                    setlength(myDBLArray, elem.NumReclose);
                    k := 0;
                    for i := 1 to elem.NumReclose do
                    begin
                        myDBLArray[k] := elem.RecloseIntervals^[i];
                        Inc(k);
                    end;
                end;
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
