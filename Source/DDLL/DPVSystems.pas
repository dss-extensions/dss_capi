unit DPVSystems;

interface

function PVsystemsI(mode: Longint; arg: Longint): Longint; CDECL;
function PVsystemsF(mode: Longint; arg: Double): Double; CDECL;
function PVsystemsS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure PVsystemsV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
{$IFNDEF FPC_DLL}
    ComServ,
{$ENDIF}
    DSSGlobals,
    PVSystem,
    Variants,
    SysUtils;

function PVsystemsI(mode: Longint; arg: Longint): Longint; CDECL;

var
    pPVSystem: TpVSystemObj;

begin
    Result := 0;    // Default return value
    case mode of
        0:
        begin  // PVSystems.Count
            if Assigned(ActiveCircuit[ActiveActor]) then
                Result := ActiveCircuit[ActiveActor].PVSystems.ListSize;
        end;
        1:
        begin  // PVSystems.First
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pPVSystem := ActiveCircuit[ActiveActor].pVSystems.First;
                if pPVSystem <> NIL then
                begin
                    repeat
                        if pPVSystem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pPVSystem;
                            Result := 1;
                        end
                        else
                            pPVSystem := ActiveCircuit[ActiveActor].pVSystems.Next;
                    until (Result = 1) or (pPVSystem = NIL);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        2:
        begin  // PVSystems.Next
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pPVSystem := ActiveCircuit[ActiveActor].PVSystems.Next;
                if pPVSystem <> NIL then
                begin
                    repeat
                        if pPVSystem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pPVSystem;
                            Result := ActiveCircuit[ActiveActor].PVSystems.ActiveIndex;
                        end
                        else
                            pPVSystem := ActiveCircuit[ActiveActor].PVSystems.Next;
                    until (Result > 0) or (pPVSystem = NIL);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        3:
        begin  // PVSystems.Idx read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].PVSystems.ActiveIndex
            else
                Result := 0;
        end;
        4:
        begin  // PVSystems.Idx write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pPVSystem := ActiveCircuit[ActiveActor].PVSystems.Get(arg);
                if pPVSystem <> NIL then
                    ActiveCircuit[ActiveActor].ActiveCktElement := pPVSystem;
            end;
        end
    else
        Result := -1;
    end;
end;

//***************************Floating point type properties*************************
function PVsystemsF(mode: Longint; arg: Double): Double; CDECL;
begin
    Result := 0.0;   // Default return value
    case mode of
        0:
        begin  // PVSystems.Irradiance read
            Result := -1.0;  // not set
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].PVSystems do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        Result := TPVSystemObj(Active).PVSystemVars.FIrradiance;
                    end;
                end;
            end;
        end;
        1:
        begin  // PVSystems.Irradiance write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].PVSystems do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        TPVSystemObj(Active).PVSystemVars.FIrradiance := arg;
                    end;
                end;
            end;
        end;
        2:
        begin  // PVSystems.kW
            Result := 0.0;  // not set
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].PVSystems do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        Result := TPVSystemObj(Active).PresentkW;
                    end;
                end;
            end;
        end;
        3:
        begin  // PVSystems.kvar read
            Result := 0.0;  // not set
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].PVSystems do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        Result := TPVSystemObj(Active).Presentkvar;
                    end;
                end;
            end;
        end;
        4:
        begin  // PVSystems.kvar write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].PVSystems do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        TPVSystemObj(Active).Presentkvar := arg;
                    end;
                end;
            end;
        end;
        5:
        begin  // PVSystems.pf read
            Result := 0.0;  // not set
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].PVSystems do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        Result := TPVSystemObj(Active).PowerFactor;
                    end;
                end;
            end;
        end;
        6:
        begin  // PVSystems.pf write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].PVSystems do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        TPVSystemObj(Active).PowerFactor := arg;
                    end;
                end;
            end;
        end;
        7:
        begin  // PVSystems.kVARated read
            Result := -1.0;  // not set
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].PVSystems do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        Result := TPVSystemObj(Active).kVARating;
                    end;
                end;
            end;
        end;
        8:
        begin  // PVSystems.kVARated write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].PVSystems do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        TPVSystemObj(Active).kVARating := arg;
                    end;
                end;
            end;
        end;
        9:
        begin  // PVSystems.Pmpp read
            Result := -1.0;  // not set
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].PVSystems do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        Result := TPVSystemObj(Active).pmpp;
                    end;
                end;
            end;
        end;
        10:
        begin  // PVSystems.Pmpp write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].PVSystems do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        TPVSystemObj(Active).pmpp := arg;
                    end;
                end;
            end;
        end;
        11:
        begin  // PVSystems.IrradianceNow
            Result := -1.0;  // not set
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].PVSystems do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        Result := TPVSystemObj(Active).IrradianceNow;
                    end;
                end;
            end;
        end
    else
        Result := -1.0;
    end;
end;

//***************************String type properties*************************
function PVsystemsS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var
    pPVSystem: TPVSystemObj;
    activesave: Integer;
    PVSystem: TPVSystemObj;
    S: String;
    Found: Boolean;

begin
    Result := Pansichar(Ansistring('0')); // Default return value
    case mode of
        0:
        begin  // PVSystems.Name read
            Result := Pansichar(Ansistring(''));
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pPVSystem := ActiveCircuit[ActiveActor].PVSystems.Active;
                if pPVSystem <> NIL then
                begin
                    Result := Pansichar(Ansistring(pPVSystem.Name));
                end
                else
                    Result := Pansichar(Ansistring(''));  // signify no name
            end;
        end;
        1:
        begin  // PVSystems.Name write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin      // Search list of PVSystems in active circuit for name
                with ActiveCircuit[ActiveActor].PVSystems do
                begin
                    S := String(arg);  // Convert to Pascal String
                    Found := FALSE;
                    ActiveSave := ActiveIndex;
                    PVSystem := First;
                    while PVSystem <> NIL do
                    begin
                        if (CompareText(PVSystem.Name, S) = 0) then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := PVSystem;
                            Found := TRUE;
                            Break;
                        end;
                        PVSystem := Next;
                    end;
                    if not Found then
                    begin
                        DoSimpleMsg('PVSystem "' + S + '" Not Found in Active Circuit.', 5003);
                        PVSystem := Get(ActiveSave);    // Restore active PVSystem
                        ActiveCircuit[ActiveActor].ActiveCktElement := PVSystem;
                    end;
                end;
            end;
        end;
        2:
        begin  // PVSystem.Sensor - read
            Result := Pansichar(Ansistring(''));
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pPVSystem := ActiveCircuit[ActiveActor].PVSystems.Active;
                if pPVSystem <> NIL then
                begin
                    Result := Pansichar(Ansistring(pPVSystem.SensorObj.ElementName));
                end
                else
                    Result := Pansichar(Ansistring(''));  // signify no name
            end;
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not valid'));
    end;
end;

//***************************Variant type properties*************************
procedure PVsystemsV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

var
    PVSystemElem: TPVSystemObj;

begin
    case mode of
        0:
        begin  // PVSystems.AllNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                    if PVSystems.ListSize > 0 then
                    begin
                        PVSystemElem := PVSystems.First;
                        while PVSystemElem <> NIL do
                        begin
                            WriteStr2Array(PVSystemElem.Name);
                            WriteStr2Array(Char(0));
                            PVSystemElem := PVSystems.Next;
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
