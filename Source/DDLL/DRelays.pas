unit DRelays;

interface

function RelaysI(mode: Longint; arg: Longint): Longint; CDECL;
function RelaysS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure RelaysV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
    Executive,
    Relay,
    ControlElem,
    Circuit,
    DSSGlobals,
    Sysutils,
    Pointerlist,
    Variants;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit[ActiveActor]) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('Relay.%s.%s=%s', [TRelayObj(RelayClass.GetActiveObj).Name, parm, val]);
    DSSExecutive[ActiveActor].Command := cmd;
end;

function RelaysI(mode: Longint; arg: Longint): Longint; CDECL;

var
    pElem: TRelayObj;
    elem: TRelayObj;
    pRelay: TRelayObj;

begin
    Result := 0;          // Default return value
    case mode of
        0:
        begin  // Relays.Count
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := RelayClass.ElementList.ListSize;
        end;
        1:
        begin  // Relays.First
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pElem := RelayClass.ElementList.First;
                if pElem <> NIL then
                    repeat
                        if pElem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                            Result := 1;
                        end
                        else
                            pElem := RelayClass.ElementList.Next;
                    until (Result = 1) or (pElem = NIL);
            end;
        end;
        2:
        begin  // Relays.Next
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pElem := RelayClass.ElementList.Next;
                if pElem <> NIL then
                    repeat
                        if pElem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                            Result := RelayClass.ElementList.ActiveIndex;
                        end
                        else
                            pElem := RelayClass.ElementList.Next;
                    until (Result > 0) or (pElem = NIL);
            end;
        end;
        3:
        begin  // Relays.MonitoredTerm read
            Result := 0;
            elem := RelayClass.GetActiveObj;
            if elem <> NIL then
                Result := elem.MonitoredElementTerminal;
        end;
        4:
        begin  // Relays.MonitoredTerm write
            elem := RelayClass.GetActiveObj;
            if elem <> NIL then
                Set_parameter('monitoredterm', IntToStr(arg));
        end;
        5:
        begin  // Relays.SwitchedTerm read
            Result := 0;
            elem := RelayClass.ElementList.Active;
            if elem <> NIL then
                Result := elem.ElementTerminal;
        end;
        6:
        begin  // Relays.SwitchedTerm read
            elem := RelayClass.GetActiveObj;
            if elem <> NIL then
                Set_parameter('SwitchedTerm', IntToStr(arg));
        end;
        7:
        begin  // Relays.Idx read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := RelayClass.ElementList.ActiveIndex
            else
                Result := 0;
        end;
        8:
        begin  // Relays.Idx write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pRelay := Relayclass.Elementlist.Get(arg);
                if pRelay <> NIL then
                    ActiveCircuit[ActiveActor].ActiveCktElement := pRelay;
            end;
        end;
        9:
        begin  // Relays.Open
            elem := Relayclass.ElementList.Active;
            if elem <> NIL then
                elem.PresentState := CTRL_OPEN;
        end;
        10:
        begin  // Relays.Close
            elem := Relayclass.ElementList.Active;
            if elem <> NIL then
                elem.PresentState := CTRL_CLOSE;
        end;
        11:
        begin  // Relays.Reset
            elem := Relayclass.ElementList.Active;
            if elem <> NIL then
                elem.Reset(ActiveActor);
        end
    else
        Result := -1;
    end;
end;

//****************************String type properties****************************
function RelaysS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var
    elem: TRelayObj;

begin
    Result := Pansichar(Ansistring(''));    // Default return value
    case mode of
        0:
        begin   // Relays.Name read
            Result := Pansichar(Ansistring(''));
            elem := RelayClass.GetActiveObj;
            if elem <> NIL then
                Result := Pansichar(Ansistring(elem.Name));
        end;
        1:
        begin   // Relays.Name write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if RelayClass.SetActive(String(arg)) then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := RelayClass.ElementList.Active;
                end
                else
                begin
                    DoSimpleMsg('Relay "' + arg + '" Not Found in Active Circuit.', 77003);
                end;
            end;
        end;
        2:
        begin   // Relays.MonitoredObj read
            Result := Pansichar(Ansistring(''));
            elem := RelayClass.GetActiveObj;
            if elem <> NIL then
                Result := Pansichar(Ansistring(elem.MonitoredElementName));
        end;
        3:
        begin   // Relays.MonitoredObj write
            elem := RelayClass.GetActiveObj;
            if elem <> NIL then
                Set_parameter('monitoredObj', String(arg));
        end;
        4:
        begin   // Relays.SwitchedObj read
            Result := Pansichar(Ansistring(''));
            elem := RelayClass.ElementList.Active;
            if elem <> NIL then
                Result := Pansichar(Ansistring(elem.ElementName));
        end;
        5:
        begin   // Relays.SwitchedObj write
            elem := RelayClass.GetActiveObj;
            if elem <> NIL then
                Set_parameter('SwitchedObj', String(arg));
        end;
        6:
        begin  // Relays.State read
            Result := Pansichar(Ansistring(''));
            elem := RelayClass.GetActiveObj;
            if elem <> NIL then
            begin
                if elem.PresentState = CTRL_CLOSE then
                    Result := Pansichar(Ansistring('closed'))
                else
                    Result := Pansichar(Ansistring('open'));
            end;

        end;
        7:
        begin  // Relays.State write
            elem := RelayClass.GetActiveObj;
            if elem <> NIL then
            begin
                if LowerCase(String(arg))[1] = 'c' then
                    elem.PresentState := CTRL_CLOSE
                else
                    elem.PresentState := CTRL_OPEN;
            end;
        end;
        8:
        begin  // Relays.Normal read
            Result := Pansichar(Ansistring(''));
            elem := RelayClass.GetActiveObj;
            if elem <> NIL then
            begin
                if elem.NormalState = CTRL_CLOSE then
                    Result := Pansichar(Ansistring('closed'))
                else
                    Result := Pansichar(Ansistring('open'));
            end;
        end;
        9:
        begin  // Relays.Normal write
            elem := RelayClass.GetActiveObj;
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

//****************************Variant type properties****************************
procedure RelaysV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

var
    elem: TRelayObj;
    pList: TPointerList;

begin
    case mode of
        0:
        begin  // Relays.AllNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if RelayClass.ElementList.ListSize > 0 then
                begin
                    pList := RelayClass.ElementList;
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
