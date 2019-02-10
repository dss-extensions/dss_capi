unit DReclosers;

interface

function ReclosersI(mode: Longint; arg: Longint): Longint; CDECL;
function ReclosersF(mode: Longint; arg: Double): Double; CDECL;
function ReclosersS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure ReclosersV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    Executive,
    Sysutils,
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
    DSSExecutive.Command := cmd;
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
        begin  // Reclosers.Open
            elem := RecloserClass.ElementList.Active;
            if elem <> NIL then
                Set_parameter('Action', 'open');
        end;
        12:
        begin  // Reclosers.Close
            elem := RecloserClass.ElementList.Active;
            if elem <> NIL then
                Set_parameter('Action', 'close');
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
function ReclosersS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

var
    elem: TRecloserObj;

begin
    Result := pAnsiChar(Ansistring('')); // Default return value
    case mode of
        0:
        begin  // Reclosers.Name read
            Result := pAnsiChar(Ansistring(''));
            elem := RecloserClass.GetActiveObj;
            if elem <> NIL then
                Result := pAnsiChar(Ansistring(elem.Name));
        end;
        1:
        begin  // Reclosers.Name write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if RecloserClass.SetActive(Widestring(arg)) then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := RecloserClass.ElementList.Active;
                end
                else
                begin
                    DoSimpleMsg('Recloser "' + Widestring(arg) + '" Not Found in Active Circuit.', 77003);
                end;
            end;
        end;
        2:
        begin  // Reclosers.MonitoredObj read
            Result := pAnsiChar(Ansistring(''));
            elem := RecloserClass.GetActiveObj;
            if elem <> NIL then
                Result := pAnsiChar(Ansistring(elem.MonitoredElementName));
        end;
        3:
        begin  // Reclosers.MonitoredObj write
            elem := RecloserClass.GetActiveObj;
            if elem <> NIL then
                Set_parameter('monitoredObj', Widestring(arg));
        end;
        4:
        begin  // Reclosers.SwitchedObj read
            Result := pAnsiChar(Ansistring(''));
            elem := RecloserClass.ElementList.Active;
            if elem <> NIL then
                Result := pAnsiChar(Ansistring(elem.ElementName));
        end;
        5:
        begin  // Reclosers.SwitchedObj write
            elem := RecloserClass.GetActiveObj;
            if elem <> NIL then
                Set_parameter('SwitchedObj', Widestring(arg));
        end
    else
        Result := pAnsiChar(Ansistring('Error, parameter not valid'));
    end;
end;

//********************Variant type properties******************************
procedure ReclosersV(mode: Longint; out arg: Variant); CDECL;

var
    elem: TRecloserObj;
    pList: TPointerList;
    k, i: Integer;

begin
    case mode of
        0:
        begin  // Reclosers.AllNames
            arg := VarArrayCreate([0, 0], varOleStr);
            arg[0] := 'NONE';
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if RecloserClass.ElementList.ListSize > 0 then
                begin
                    pList := RecloserClass.ElementList;
                    VarArrayRedim(arg, pList.ListSize - 1);
                    k := 0;
                    elem := pList.First;
                    while elem <> NIL do
                    begin
                        arg[k] := elem.Name;
                        Inc(k);
                        elem := pList.next;
                    end;
                end;
            end;
        end;
        1:
        begin  // Reclosers.RecloseIntervals
            arg := VarArrayCreate([0, 0], varDouble);
            arg[0] := -1.0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                elem := RecloserClass.ElementList.Active;
                if elem <> NIL then
                begin
                    VarArrayRedim(arg, elem.NumReclose - 1);
                    k := 0;
                    for i := 1 to elem.NumReclose do
                    begin
                        arg[k] := elem.RecloseIntervals^[i];
                        Inc(k);
                    end;
                end;
            end;
        end
    else
        arg[0] := 'Error, parameter not valid';
    end;
end;

end.
