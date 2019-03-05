unit DGenerators;

interface

function GeneratorsI(mode: Longint; arg: Longint): Longint; CDECL;
function GeneratorsF(mode: Longint; arg: Double): Double; CDECL;
function GeneratorsS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure GeneratorsV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    DSSCLassDefs,
    DSSGlobals,
    Generator,
    CktElement,
    SysUtils,
    Variants;

function GeneratorsI(mode: Longint; arg: Longint): Longint; CDECL;

var
    pGen: TGeneratorObj;

begin
    Result := 0; // Default return value
    case mode of
        0:
        begin  // Generators.First
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pGen := ActiveCircuit[ActiveActor].Generators.First;
                if pGen <> NIL then
                begin
                    repeat
                        if pGen.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pGen;
                            Result := 1;
                        end
                        else
                            pGen := ActiveCircuit[ActiveActor].Generators.Next;
                    until (Result = 1) or (pGen = NIL);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        1:
        begin   // Generators.Next
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pGen := ActiveCircuit[ActiveActor].Generators.Next;
                if pGen <> NIL then
                begin
                    repeat
                        if pGen.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pGen;
                            Result := ActiveCircuit[ActiveActor].Generators.ActiveIndex;
                        end
                        else
                            pGen := ActiveCircuit[ActiveActor].Generators.Next;
                    until (Result > 0) or (pGen = NIL);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        2:
        begin  // Generators.ForcedON read
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        if TGeneratorObj(Active).ForcedON then
                            Result := 1;
                    end;
                end;
            end;
        end;
        3:
        begin  // Generators.ForcedON Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        if arg = 1 then
                            TGeneratorObj(Active).ForcedON := TRUE
                        else
                            TGeneratorObj(Active).ForcedON := FALSE;
                    end;
                end;
            end;
        end;
        4:
        begin  // Generators.Phases read
            Result := 0;  // not set
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        Result := TGeneratorObj(Active).nphases;
                    end;
                end;
            end;
        end;
        5:
        begin  // Generators.Phases Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        TGeneratorObj(Active).Nphases := arg;
                    end;
                end;
            end;
        end;
        6:
        begin // Generators.Count
            if Assigned(ActiveCircuit[ActiveActor]) then
                Result := ActiveCircuit[ActiveActor].Generators.ListSize;
        end;
        7:
        begin // Generators.Idx read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].Generators.ActiveIndex
            else
                Result := 0;
        end;
        8:
        begin // Generators.Idx Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pGen := ActiveCircuit[ActiveActor].Generators.Get(arg);
                if pGen <> NIL then
                    ActiveCircuit[ActiveActor].ActiveCktElement := pGen;
            end;
        end;
        9:
        begin  // Generators.Model read
            Result := -1;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        Result := TGeneratorObj(Active).GenModel;
                    end;
                end;
            end;
        end;
        10:
        begin  // Generators.Model Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        with TGeneratorObj(Active) do
                        begin
                            GenModel := arg;
                       // Handle side effect
                            if GenModel = 3 then
                                ActiveCircuit[ActiveActor].Solution.SolutionInitialized := FALSE;
                        end;
                    end;
                end;
            end;
        end
    else
        Result := -1;   // The parameter is not valid
    end;
end;

//**************************Floating point type properties***********************
function GeneratorsF(mode: Longint; arg: Double): Double; CDECL;
begin
    Result := 0.0; // Default return value
    case mode of
        0:
        begin  // Generators.kV read
            Result := -1.0;  // not set
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        Result := TGeneratorObj(Active).GenVars.kVGeneratorBase;
                    end;
                end;
            end;
        end;
        1:
        begin  // Generators.kV Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        TGeneratorObj(Active).PresentkV := arg;
                    end;
                end;
            end;
        end;
        2:
        begin  // Generators.kW read
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        Result := TGeneratorObj(Active).PresentkW;
                    end;
                end;
            end;
        end;
        3:
        begin  // Generators.kW Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        TGeneratorObj(Active).PresentkW := arg;
                    end;
                end;
            end;
        end;
        4:
        begin // Generators.kvar read
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        Result := TGeneratorObj(Active).Presentkvar;
                    end;
                end;
            end;
        end;
        5:
        begin  // Generator.kvar Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        TGeneratorObj(Active).Presentkvar := arg;
                    end;
                end;
            end;
        end;
        6:
        begin  // Generators.PF read
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        Result := TGeneratorObj(Active).PowerFactor;
                    end;
                end;
            end;
        end;
        7:
        begin  // Generators.PF Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        TGeneratorObj(Active).PowerFactor := arg;
                    end;
                end;
            end;
        end;
        8:
        begin  // Generators.KVARated read
            Result := -1.0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        Result := TGeneratorObj(Active).Genvars.kVArating;
                    end;
                end;
            end;
        end;
        9:
        begin  // Generators.KVARated Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        with TGeneratorObj(Active) do
                        begin
                            Genvars.kVArating := arg;
                        end;
                    end;
                end;
            end;
        end;
        10:
        begin  // Generators.Vmaxpu read
            Result := -1.0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        Result := TGeneratorObj(Active).Vmaxpu;
                    end;
                end;
            end;
        end;
        11:
        begin  // Generators.Vmaxpu Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        with TGeneratorObj(Active) do
                        begin
                            VMaxPu := arg;
                        end;
                    end;
                end;
            end;
        end;
        12:
        begin  // Generators.Vminpu read
            Result := -1.0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        Result := TGeneratorObj(Active).Vminpu;
                    end;
                end;
            end;
        end;
        13:
        begin  // Generators.Vminpu Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        with TGeneratorObj(Active) do
                        begin
                            VMinPu := arg;
                        end;
                    end;
                end;
            end;
        end
    else
        Result := -1.0;
    end;
end;

//*******************************String type properties***************************
function GeneratorsS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

var
    pGen: TGeneratorObj;
    activesave: Integer;
    Gen: TGeneratorObj;
    S: String;
    Found: Boolean;

begin
    Result := pAnsiChar(Ansistring('')); // Default return value
    case mode of
        0:
        begin  // Generators.Name read
            Result := pAnsiChar(Ansistring(''));
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pGen := ActiveCircuit[ActiveActor].Generators.Active;
                if pGen <> NIL then
                begin
                    Result := pAnsiChar(Ansistring(pGen.Name));
                end
                else
                    Result := pAnsiChar(Ansistring(''));  // signify no name
            end;
        end;
        1:
        begin  // Generators.Name Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin      // Search list of generators in active circuit for name
                with ActiveCircuit[ActiveActor].Generators do
                begin
                    S := Widestring(arg);  // Convert to Pascal String
                    Found := FALSE;
                    ActiveSave := ActiveIndex;
                    Gen := First;
                    while Gen <> NIL do
                    begin
                        if (CompareText(Gen.Name, S) = 0) then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := Gen;
                            Found := TRUE;
                            Break;
                        end;
                        Gen := Next;
                    end;
                    if not Found then
                    begin
                        DoSimpleMsg('Generator "' + S + '" Not Found in Active Circuit.', 5003);
                        Gen := Get(ActiveSave);    // Restore active generator
                        ActiveCircuit[ActiveActor].ActiveCktElement := Gen;
                    end;
                end;
            end;
        end
    else
        Result := pAnsiChar(Ansistring('Error, Parameter not recognized'));
    end;
end;

//*******************************Variant type properties************************
procedure GeneratorsV(mode: Longint; out arg: Variant); CDECL;

var
    GenElem: TGeneratorObj;
    GeneratorClass: TGenerator;
    k: Integer;

begin
    case mode of
        0:
        begin  // Generators.AllNames
            arg := VarArrayCreate([0, 0], varOleStr);
            arg[0] := 'NONE';
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                    if Generators.ListSize > 0 then
                    begin
                        VarArrayRedim(arg, Generators.ListSize - 1);
                        k := 0;
                        GenElem := Generators.First;
                        while GenElem <> NIL do
                        begin
                            arg[k] := GenElem.Name;
                            Inc(k);
                            GenElem := Generators.Next;
                        end;
                    end;
        end;
        1:
        begin  // Generators.RegisterNames
            GeneratorClass := DssClassList[ActiveActor].Get(Classnames[ActiveActor].Find('Generator'));
            arg := VarArrayCreate([0, NumGenRegisters - 1], varOleStr);
            for k := 0 to NumGenRegisters - 1 do
            begin
                arg[k] := GeneratorClass.RegisterNames[k + 1];
            end;
        end;
        2:
        begin // Generators.RegisterValues
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                GenElem := TGeneratorObj(ActiveCircuit[ActiveActor].Generators.Active);
                if GenElem <> NIL then
                begin
                    arg := VarArrayCreate([0, numGenRegisters - 1], varDouble);
                    for k := 0 to numGenRegisters - 1 do
                    begin
                        arg[k] := GenElem.Registers[k + 1];
                    end;
                end
                else
                    arg := VarArrayCreate([0, 0], varDouble);
            end
            else
            begin
                arg := VarArrayCreate([0, 0], varDouble);
            end;
        end
    else
        arg[0] := 'Error, parameter not recognized'
    end;
end;

end.
