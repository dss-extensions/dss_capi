unit DFuses;

interface

function FusesI(mode: Longint; arg: Longint): Longint; CDECL;
function FusesF(mode: Longint; arg: Double): Double; CDECL;
function FusesS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure FusesV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    ComServ,
    Executive,
    Sysutils,
    ControlElem,
    Fuse,
    Pointerlist,
    DSSGlobals,
    Variants;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit[ActiveActor]) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('Fuse.%s.%s=%s', [TFuseObj(FuseClass.GetActiveObj).Name, parm, val]);
    DSSExecutive[ActiveActor].Command := cmd;
end;

function FusesI(mode: Longint; arg: Longint): Longint; CDECL;

var
    pElem: TFuseObj;
    elem: TFuseObj;
    i: Integer;
    pFuse: TFuseObj;

begin
    Result := 0;
    case mode of
        0:
        begin  // Fuses.Count
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := FuseClass.ElementList.ListSize;
        end;
        1:
        begin  // Fuses.First
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pElem := FuseClass.ElementList.First;
                if pElem <> NIL then
                    repeat
                        if pElem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                            Result := 1;
                        end
                        else
                            pElem := FuseClass.ElementList.Next;
                    until (Result = 1) or (pElem = NIL);
            end;
        end;
        2:
        begin  // Fuses.Next
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pElem := FuseClass.ElementList.Next;
                if pElem <> NIL then
                    repeat
                        if pElem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                            Result := FuseClass.ElementList.ActiveIndex;
                        end
                        else
                            pElem := FuseClass.ElementList.Next;
                    until (Result > 0) or (pElem = NIL);
            end;
        end;
        3:
        begin  // Fuse.MonitoredTerm read
            Result := 0;
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
                Result := elem.MonitoredElementTerminal;
        end;
        4:
        begin  // Fuse.MonitoredTerm write
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
                Set_parameter('monitoredterm', IntToStr(arg));
        end;
        5:
        begin  // Fuse.SwitchedTerm read
            Result := 0;
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
                Result := elem.ElementTerminal;
        end;
        6:
        begin  // Fuse.SwitchedTerm write
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
                Set_parameter('switchedterm', IntToStr(arg));
        end;
        7:
        begin  // Fuse.Open
            pFuse := FuseClass.GetActiveObj;
            if pFuse <> NIL then
            begin
                for i := 1 to pFuse.ControlledElement.NPhases do
                    pFuse.States[i] := CTRL_OPEN // Open all phases
            end;
        end;
        8:
        begin  // Fuse.Close
            pFuse := FuseClass.GetActiveObj;
            if pFuse <> NIL then
            begin
                for i := 1 to pFuse.ControlledElement.NPhases do
                    pFuse.States[i] := CTRL_CLOSE // Close all phases
            end;
        end;
        9:
        begin  // Fuse.IsBlown
            Result := 0;
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
            begin
                for i := 1 to elem.nphases do
                    if not elem.ControlledElement.Closed[i, ActiveActor] then
                        Result := 1;
            end;
        end;
        10:
        begin  // Fuse.Idx read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := FuseClass.ElementList.ActiveIndex
            else
                Result := 0;
        end;
        11:
        begin  // Fuse.Idx write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pFuse := FuseClass.Elementlist.Get(arg);
                if pFuse <> NIL then
                    ActiveCircuit[ActiveActor].ActiveCktElement := pFuse;
            end;
        end;
        12:
        begin  // Fuse.NumPhases
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pFuse := FuseClass.GetActiveObj;
                if pFuse <> NIL then
                    Result := pFuse.NPhases;
            end;
        end;
        13:
        begin  // Fuse.Reset
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pFuse := FuseClass.GetActiveObj;
                if pFuse <> NIL then
                    pFuse.Reset(ActiveActor);
            end;
        end
    else
        Result := -1;
    end;
end;

//******************************Floating point type properties********************
function FusesF(mode: Longint; arg: Double): Double; CDECL;

var
    elem: TFuseObj;

begin
    Result := 0.0; // Default return value
    case mode of
        0:
        begin  // Fuses.RatedCurrent read
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
                Result := elem.RatedCurrent
            else
                Result := -1.0;
        end;
        1:
        begin  // Fuses.RatedCurrent write
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
                Set_parameter('RatedCurrent', Format('%.8g ', [arg]));
        end
    else
        Result := -1.0;
    end;
end;

//******************************String type properties********************
function FusesS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

var
    elem: TFuseObj;

begin
    Result := pAnsiChar(Ansistring('')); // Default return value
    case mode of
        0:
        begin  // Fuses.Name read
            Result := pAnsiChar(Ansistring(''));
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
                Result := pAnsiChar(Ansistring(elem.Name));
        end;
        1:
        begin  // Fuses.Name write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if FuseClass.SetActive(Widestring(arg)) then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := FuseClass.ElementList.Active;
                end
                else
                begin
                    DoSimpleMsg('Fuse "' + Widestring(arg) + '" Not Found in Active Circuit.', 77003);
                end;
            end;
        end;
        2:
        begin  // Fuses. MonitoredObj read
            Result := pAnsiChar(Ansistring(''));
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
                Result := pAnsiChar(Ansistring(elem.MonitoredElementName));
        end;
        3:
        begin  // Fuses. MonitoredObj write
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
                Set_parameter('monitoredObj', Widestring(arg));
        end;
        4:
        begin  // Fuses.SwitchedObj read
            Result := pAnsiChar(Ansistring(''));
            elem := FuseClass.ElementList.Active;
            if elem <> NIL then
                Result := pAnsiChar(Ansistring(elem.ElementName));
        end;
        5:
        begin  // Fuses.SwitchedObj write
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
                Set_parameter('SwitchedObj', Widestring(arg));
        end;
        6:
        begin  // Fuses.TCCcurve read
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
                Result := pAnsiChar(Ansistring(elem.FuseCurve.Name))
            else
                Result := pAnsiChar(Ansistring('No Fuse Active!'));
        end;
        7:
        begin  // Fuses.TCCcurve write
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
                Set_parameter('FuseCurve', Widestring(arg));
        end
    else
        Result := pAnsiChar(Ansistring('Error, parameter not valid'));
    end;
end;

//******************************Variant type properties********************
procedure FusesV(mode: Longint; out arg: Variant); CDECL;

var
    elem: TFuseObj;
    pList: TPointerList;
    k, i, LoopLimit: Integer;

begin
    case mode of
        0:
        begin  // Fuses.AllName
            arg := VarArrayCreate([0, 0], varOleStr);
            arg[0] := 'NONE';
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if FuseClass.ElementList.ListSize > 0 then
                begin
                    pList := FuseClass.ElementList;
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
        begin  // Fuses.States read
            arg := VarArrayCreate([0, 0], varOleStr);
            arg[0] := 'NONE';     // error code
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Elem := FuseClass.GetActiveObj;
                if Elem <> NIL then
                begin
                    VarArrayRedim(arg, elem.ControlledElement.Nphases - 1);
                    k := 0;
                    for i := 1 to elem.ControlledElement.Nphases do
                    begin

                        if elem.States[i] = CTRL_CLOSE then
                            arg[k] := 'closed'
                        else
                            arg[k] := 'open';
                        Inc(k);
                    end;
                end;
            end;
        end;
        2:
        begin  // Fuses.States write
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
            begin
         // allocate space based on number of phases of controlled device
                LoopLimit := VarArrayHighBound(Widestring(arg), 1);
                if (LoopLimit - VarArrayLowBound(arg, 1) + 1) > elem.ControlledElement.NPhases then
                    LoopLimit := VarArrayLowBound(arg, 1) + elem.ControlledElement.NPhases - 1;
                k := 1;
                for i := VarArrayLowBound(arg, 1) to LoopLimit do
                begin

                    case LowerCase(arg[i])[1] of
                        'o':
                            elem.States[k] := CTRL_OPEN;
                        'c':
                            elem.States[k] := CTRL_CLOSE;
                    end;
                    inc(k);
                end;
            end;
        end;
        3:
        begin  // Fuses.NormalStates read
            arg := VarArrayCreate([0, 0], varOleStr);
            arg[0] := 'NONE';     // error code
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Elem := FuseClass.GetActiveObj;
                if Elem <> NIL then
                begin
                    VarArrayRedim(arg, elem.ControlledElement.Nphases - 1);
                    k := 0;
                    for i := 1 to elem.ControlledElement.Nphases do
                    begin

                        if elem.NormalStates[i] = CTRL_CLOSE then
                            arg[k] := 'closed'
                        else
                            arg[k] := 'open';
                        Inc(k);
                    end;
                end;
            end;
        end;
        4:
        begin  // Fuses.NormalStates write
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
            begin
         // allocate space based on number of phases of controlled device
                LoopLimit := VarArrayHighBound(Widestring(arg), 1);
                if (LoopLimit - VarArrayLowBound(arg, 1) + 1) > elem.ControlledElement.NPhases then
                    LoopLimit := VarArrayLowBound(arg, 1) + elem.ControlledElement.NPhases - 1;
                k := 1;
                for i := VarArrayLowBound(arg, 1) to LoopLimit do
                begin

                    case LowerCase(arg[i])[1] of
                        'o':
                            elem.NormalStates[k] := CTRL_OPEN;
                        'c':
                            elem.NormalStates[k] := CTRL_CLOSE;
                    end;
                    inc(k);
                end;
            end;
        end
    else
        arg[0] := 'Error, parameter not valid';
    end;
end;

end.
