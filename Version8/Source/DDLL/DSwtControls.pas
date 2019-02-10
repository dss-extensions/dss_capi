unit DSwtControls;

interface

function SwtControlsI(mode: Longint; arg: Longint): Longint; CDECL;
function SwtControlsF(mode: Longint; arg: Double): Double; CDECL;
function SwtControlsS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure SwtControlsV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    DSSGlobals,
    Executive,
    ControlElem,
    SwtControl,
    Variants,
    SysUtils,
    PointerList;

function ActiveSwtControl: TSwtControlObj;
begin
    Result := NIL;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].SwtControls.Active;
end;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit[ActiveActor]) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('swtcontrol.%s.%s=%s', [ActiveSwtControl.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;

function SwtControlsI(mode: Longint; arg: Longint): Longint; CDECL;

var
    elem: TSwtControlObj;
    lst: TPointerList;

begin
    Result := 0;      // Default return value
    case mode of
        0:
        begin  // SwtControls.First
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                lst := ActiveCircuit[ActiveActor].SwtControls;
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
        1:
        begin  // SwtControls.Next
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                lst := ActiveCircuit[ActiveActor].SwtControls;
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
        2:
        begin  // SwtControls.Action read
            Result := 0;
            elem := ActiveSwtControl;
            if elem <> NIL then
            begin
                case elem.CurrentAction of
                    CTRL_CLOSE:
                        Result := 2;
                    CTRL_OPEN:
                        Result := 1;
                end;
            end;
        end;
        3:
        begin  // SwtControls.Action write
            elem := ActiveSwtControl;
            if elem <> NIL then
            begin
                case arg of
                    1:
                        Set_Parameter('Action', 'o');
                    2:
                        Set_Parameter('Action', 'c');
                    3:
                    begin  // Reset means the shelf state
                        Set_Parameter('Lock', 'n');
                        Set_Parameter('Action', 'c');
                    end;
                    4:
                        Set_Parameter('Lock', 'y');
                    5:
                        Set_Parameter('Lock', 'n');
                else // TapUp, TapDown, None have no effect
                end;
            end
        end;
        4:
        begin  // SwtControls.IsLocked read
            Result := 0;
            elem := ActiveSwtControl;
            if elem <> NIL then
            begin
                if elem.IsIsolated then
                    Result := 1;
            end;
        end;
        5:
        begin  // SwtControls.IsLocked write
            if arg = 1 then
                Set_Parameter('Lock', 'y')
            else
                Set_Parameter('Lock', 'n');
        end;
        6:
        begin  // SwtControls.SwitchedTerm read
            Result := 0;
            elem := ActiveSwtControl;
            if elem <> NIL then
                Result := elem.ElementTerminal;
        end;
        7:
        begin
            Set_Parameter('SwitchedTerm', IntToStr(arg));
        end;
        8:
        begin  // SwtControls.Count
            if Assigned(ActiveCircuit[ActiveActor]) then
                Result := ActiveCircuit[ActiveActor].SwtControls.ListSize;
        end
    else
        Result := -1;
    end;
end;

//************************************Floating point type properties****************
function SwtControlsF(mode: Longint; arg: Double): Double; CDECL;

var
    elem: TSwtControlObj;

begin
    Result := 0.0; // Default return value
    case mode of
        0:
        begin  // SwtControls.Delay read
            Result := 0.0;
            elem := ActiveSwtControl;
            if elem <> NIL then
                Result := elem.TimeDelay;
        end;
        1:
        begin  // SwtControls.Delay write
            Set_Parameter('Delay', FloatToStr(arg));
        end
    else
        Result := -1.0;
    end;
end;

//************************************String type properties************************
function SwtControlsS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

var
    elem: TSwtControlObj;
    ActiveSave: Integer;
    S: String;
    Found: Boolean;
    lst: TPointerList;

begin
    Result := pAnsiChar(Ansistring('')); // Default return value
    case mode of
        0:
        begin  // SwtControls.Name read
            Result := pAnsiChar(Ansistring(''));
            elem := ActiveSwtControl;
            if elem <> NIL then
                Result := pAnsiChar(Ansistring(elem.Name));
        end;
        1:
        begin  // SwtControls.Name write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                lst := ActiveCircuit[ActiveActor].SwtControls;
                S := Widestring(arg);  // Convert to Pascal String
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
                    DoSimpleMsg('SwtControl "' + S + '" Not Found in Active Circuit.', 5003);
                    elem := lst.Get(ActiveSave);    // Restore active Load
                    ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                end;
            end;
        end;
        2:
        begin  // SwtControl.SwitchedObj read
            Result := pAnsiChar(Ansistring(''));
            elem := ActiveSwtControl;
            if elem <> NIL then
                Result := pAnsiChar(Ansistring(elem.ElementName));
        end;
        3:
        begin  // SwtControl.SwitchedObj write
            Set_Parameter('SwitchedObj', arg);
        end
    else
        Result := pAnsiChar(Ansistring('Error, parameter not valid'));
    end;
end;

//******************************Variant type properties*****************************
procedure SwtControlsV(mode: Longint; out arg: Variant); CDECL;

var
    elem: TSwtControlObj;
    lst: TPointerList;
    k: Integer;

begin
    case mode of
        0:
        begin  // SwtControls.AllNames
            arg := VarArrayCreate([0, 0], varOleStr);
            arg[0] := 'NONE';
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                    if SwtControls.ListSize > 0 then
                    begin
                        lst := SwtControls;
                        arg := VarArrayCreate([0, lst.ListSize - 1], varOleStr);
                        k := 0;
                        elem := lst.First;
                        while elem <> NIL do
                        begin
                            arg[k] := elem.Name;
                            Inc(k);
                            elem := lst.Next;
                        end;
                    end;
        end
    else
        arg[0] := 'Error, parameter not valid';
    end;
end;


end.
