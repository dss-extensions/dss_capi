unit DPVSystems;

interface

function PVsystemsI(mode: Longint; arg: Longint): Longint; CDECL;
function PVsystemsF(mode: Longint; arg: Double): Double; CDECL;
function PVsystemsS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure PVsystemsV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
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
            if Assigned(Activecircuit) then
                Result := ActiveCircuit.PVSystems.ListSize;
        end;
        1:
        begin  // PVSystems.First
            Result := 0;
            if ActiveCircuit <> NIL then
            begin
                pPVSystem := ActiveCircuit.pVSystems.First;
                if pPVSystem <> NIL then
                begin
                    repeat
                        if pPVSystem.Enabled then
                        begin
                            ActiveCircuit.ActiveCktElement := pPVSystem;
                            Result := 1;
                        end
                        else
                            pPVSystem := ActiveCircuit.pVSystems.Next;
                    until (Result = 1) or (pPVSystem = NIL);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        2:
        begin  // PVSystems.Next
            Result := 0;
            if ActiveCircuit <> NIL then
            begin
                pPVSystem := ActiveCircuit.PVSystems.Next;
                if pPVSystem <> NIL then
                begin
                    repeat
                        if pPVSystem.Enabled then
                        begin
                            ActiveCircuit.ActiveCktElement := pPVSystem;
                            Result := ActiveCircuit.PVSystems.ActiveIndex;
                        end
                        else
                            pPVSystem := ActiveCircuit.PVSystems.Next;
                    until (Result > 0) or (pPVSystem = NIL);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        3:
        begin  // PVSystems.Idx read
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.PVSystems.ActiveIndex
            else
                Result := 0;
        end;
        4:
        begin  // PVSystems.Idx write
            if ActiveCircuit <> NIL then
            begin
                pPVSystem := ActiveCircuit.PVSystems.Get(arg);
                if pPVSystem <> NIL then
                    ActiveCircuit.ActiveCktElement := pPVSystem;
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
            if ActiveCircuit <> NIL then
            begin
                with ActiveCircuit.PVSystems do
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
            if ActiveCircuit <> NIL then
            begin
                with ActiveCircuit.PVSystems do
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
            if ActiveCircuit <> NIL then
            begin
                with ActiveCircuit.PVSystems do
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
            if ActiveCircuit <> NIL then
            begin
                with ActiveCircuit.PVSystems do
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
            if ActiveCircuit <> NIL then
            begin
                with ActiveCircuit.PVSystems do
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
            if ActiveCircuit <> NIL then
            begin
                with ActiveCircuit.PVSystems do
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
            if ActiveCircuit <> NIL then
            begin
                with ActiveCircuit.PVSystems do
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
            if ActiveCircuit <> NIL then
            begin
                with ActiveCircuit.PVSystems do
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
            if ActiveCircuit <> NIL then
            begin
                with ActiveCircuit.PVSystems do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        TPVSystemObj(Active).kVARating := arg;
                    end;
                end;
            end;
        end
    else
        Result := -1.0;
    end;
end;

//***************************String type properties*************************
function PVsystemsS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

var
    pPVSystem: TPVSystemObj;
    activesave: Integer;
    PVSystem: TPVSystemObj;
    S: String;
    Found: Boolean;

begin
    Result := pAnsiChar(Ansistring('0')); // Default return value
    case mode of
        0:
        begin  // PVSystems.Name read
            Result := pAnsiChar(Ansistring(''));
            if ActiveCircuit <> NIL then
            begin
                pPVSystem := ActiveCircuit.PVSystems.Active;
                if pPVSystem <> NIL then
                begin
                    Result := pAnsiChar(Ansistring(pPVSystem.Name));
                end
                else
                    Result := pAnsiChar(Ansistring(''));  // signify no name
            end;
        end;
        1:
        begin  // PVSystems.Name write
            if ActiveCircuit <> NIL then
            begin      // Search list of PVSystems in active circuit for name
                with ActiveCircuit.PVSystems do
                begin
                    S := String(arg);  // Convert to Pascal String
                    Found := FALSE;
                    ActiveSave := ActiveIndex;
                    PVSystem := First;
                    while PVSystem <> NIL do
                    begin
                        if (CompareText(PVSystem.Name, S) = 0) then
                        begin
                            ActiveCircuit.ActiveCktElement := PVSystem;
                            Found := TRUE;
                            Break;
                        end;
                        PVSystem := Next;
                    end;
                    if not Found then
                    begin
                        DoSimpleMsg('PVSystem "' + S + '" Not Found in Active Circuit.', 5003);
                        PVSystem := Get(ActiveSave);    // Restore active PVSystem
                        ActiveCircuit.ActiveCktElement := PVSystem;
                    end;
                end;
            end;
        end
    else
        Result := pAnsiChar(Ansistring('Error, parameter not valid'));
    end;
end;

//***************************Variant type properties*************************
procedure PVsystemsV(mode: Longint; out arg: Variant); CDECL;

var
    PVSystemElem: TPVSystemObj;
    k: Integer;

begin
    case mode of
        0:
        begin  // PVSystems.AllNames
            arg := VarArrayCreate([0, 0], varOleStr);
            arg[0] := 'NONE';
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                    if PVSystems.ListSize > 0 then
                    begin
                        VarArrayRedim(arg, PVSystems.ListSize - 1);
                        k := 0;
                        PVSystemElem := PVSystems.First;
                        while PVSystemElem <> NIL do
                        begin
                            arg[k] := PVSystemElem.Name;
                            Inc(k);
                            PVSystemElem := PVSystems.Next;
                        end;
                    end;
        end
    else
        arg[0] := 'Error, parameter not valid';
    end;
end;

end.
