unit DFuses;

interface

function FusesI(mode: Longint; arg: Longint): Longint; CDECL;
function FusesF(mode: Longint; arg: Double): Double; CDECL;
function FusesS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure FusesV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
{$IFNDEF FPC_DLL}
    ComServ,
{$ENDIF}
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
        end;
        2:
        begin  // Fuses.Delay read
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
                Result := elem.DelayTime
            else
                Result := -1.0;
        end;
        3:
        begin  // Fuses.Delay write
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
                Set_parameter('Delay', Format('%.8g ', [arg]));
        end
    else
        Result := -1.0;
    end;
end;

//******************************String type properties********************
function FusesS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var
    elem: TFuseObj;

begin
    Result := Pansichar(Ansistring('')); // Default return value
    case mode of
        0:
        begin  // Fuses.Name read
            Result := Pansichar(Ansistring(''));
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
                Result := Pansichar(Ansistring(elem.Name));
        end;
        1:
        begin  // Fuses.Name write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if FuseClass.SetActive(String(arg)) then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := FuseClass.ElementList.Active;
                end
                else
                begin
                    DoSimpleMsg('Fuse "' + arg + '" Not Found in Active Circuit.', 77003);
                end;
            end;
        end;
        2:
        begin  // Fuses. MonitoredObj read
            Result := Pansichar(Ansistring(''));
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
                Result := Pansichar(Ansistring(elem.MonitoredElementName));
        end;
        3:
        begin  // Fuses. MonitoredObj write
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
                Set_parameter('monitoredObj', String(arg));
        end;
        4:
        begin  // Fuses.SwitchedObj read
            Result := Pansichar(Ansistring(''));
            elem := FuseClass.ElementList.Active;
            if elem <> NIL then
                Result := Pansichar(Ansistring(elem.ElementName));
        end;
        5:
        begin  // Fuses.SwitchedObj write
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
                Set_parameter('SwitchedObj', String(arg));
        end;
        6:
        begin  // Fuses.TCCcurve read
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
                Result := Pansichar(Ansistring(elem.FuseCurve.Name))
            else
                Result := Pansichar(Ansistring('No Fuse Active!'));
        end;
        7:
        begin  // Fuses.TCCcurve write
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
                Set_parameter('FuseCurve', String(arg));
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not valid'));
    end;
end;

//******************************Variant type properties********************
procedure FusesV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

var
    elem: TFuseObj;
    pList: TPointerList;
    k,
    i: Integer;
    S: String;

begin
    case mode of
        0:
        begin  // Fuses.AllName
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if FuseClass.ElementList.ListSize > 0 then
                begin
                    pList := FuseClass.ElementList;
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
        begin  // Fuses.States read
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Elem := FuseClass.GetActiveObj;
                if Elem <> NIL then
                begin
                    for i := 1 to elem.ControlledElement.Nphases do
                    begin
                        if elem.States[i] = CTRL_CLOSE then
                            WriteStr2Array('closed')
                        else
                            WriteStr2Array('open');
                        WriteStr2Array(Char(0));
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        2:
        begin  // Fuses.States write
            myType := 4;          // String
            k := 0;
            elem := FuseClass.GetActiveObj;
            if elem <> NIL then
            begin

                for i := 1 to elem.ControlledElement.NPhases do
                begin
                    S := BArray2Str(myPointer, k);
                    if S = '' then
                        break
                    else
                    begin
                        case LowerCase(S)[1] of
                            'o':
                                elem.States[i] := CTRL_OPEN;
                            'c':
                                elem.States[i] := CTRL_CLOSE;
                        end;
                    end;
                end;
            end;
            mySize := k;
        end;
        3:
        begin  // Fuses.NormalStates read
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Elem := FuseClass.GetActiveObj;
                if Elem <> NIL then
                begin
                    for i := 1 to elem.ControlledElement.Nphases do
                    begin
                        if elem.NormalStates[i] = CTRL_CLOSE then
                            WriteStr2Array('closed')
                        else
                            WriteStr2Array('open');
                        WriteStr2Array(Char(0));
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        4:
        begin  // Fuses.NormalStates write
            elem := FuseClass.GetActiveObj;
            k := 0;
            if elem <> NIL then
            begin
      // allocate space based on number of phases of controlled device
                for i := 1 to elem.ControlledElement.NPhases do
                begin
                    S := BArray2Str(myPointer, k);
                    if S = '' then
                        break
                    else
                    begin
                        case LowerCase(S)[1] of
                            'o':
                                elem.NormalStates[i] := CTRL_OPEN;
                            'c':
                                elem.NormalStates[i] := CTRL_CLOSE;
                        end;
                    end;
                end;
            end;
            mySize := k;
        end
    else
        myType := 4;        // String
        setlength(myStrArray, 0);
        WriteStr2Array('Error, parameter not recognized');
        myPointer := @(myStrArray[0]);
        mySize := Length(myStrArray);
    end;
end;

end.
