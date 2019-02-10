unit ImplGenerators;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
  1-12-00  Modified first..Next to reurn only enabled generators
}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSEngine_TLB,
    StdVcl;

type
    TGenerators = class(TAutoObject, IGenerators)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_RegisterNames: Olevariant; SAFECALL;
        function Get_RegisterValues: Olevariant; SAFECALL;
        function Get_ForcedON: Wordbool; SAFECALL;
        procedure Set_ForcedON(Value: Wordbool); SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_kV: Double; SAFECALL;
        function Get_kvar: Double; SAFECALL;
        function Get_kW: Double; SAFECALL;
        function Get_PF: Double; SAFECALL;
        function Get_Phases: Integer; SAFECALL;
        procedure Set_kV(Value: Double); SAFECALL;
        procedure Set_kvar(Value: Double); SAFECALL;
        procedure Set_kW(Value: Double); SAFECALL;
        procedure Set_PF(Value: Double); SAFECALL;
        procedure Set_Phases(Value: Integer); SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_idx: Integer; SAFECALL;
        procedure Set_idx(Value: Integer); SAFECALL;
        function Get_Model: Integer; SAFECALL;
        procedure Set_Model(Value: Integer); SAFECALL;
        function Get_kVArated: Double; SAFECALL;
        procedure Set_kVArated(Value: Double); SAFECALL;
        function Get_Vmaxpu: Double; SAFECALL;
        function Get_Vminpu: Double; SAFECALL;
        procedure Set_Vmaxpu(Value: Double); SAFECALL;
        procedure Set_Vminpu(Value: Double); SAFECALL;
    { Protected declarations }
    end;

implementation

uses
    ComServ,
    DSSCLassDefs,
    DSSGlobals,
    Generator,
    CktElement,
    SysUtils,
    Variants;

function IsGenerator(const CktElem: TDSSCktElement): Boolean;

begin
    Result := ((CktElem.DssObjtype and CLASSMASK) = GEN_ELEMENT);
    if not Result then
        DoSimpleMsg('GENERATOR Type Expected, but another found. Dss Class=' + CktElem.DSSClassName + CRLF +
            'Element name=' + CktElem.Name, 5002);
end;

function TGenerators.Get_AllNames: Olevariant;
var
    GenElem: TGeneratorObj;
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
            if Generators.ListSize > 0 then
            begin
                VarArrayRedim(result, Generators.ListSize - 1);
                k := 0;
                GenElem := Generators.First;
                while GenElem <> NIL do
                begin
                    Result[k] := GenElem.Name;
                    Inc(k);
                    GenElem := Generators.Next;
                end;
            end;
end;


function TGenerators.Get_First: Integer;
var
    pGen: TGeneratorObj;

begin

    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pGen := ActiveCircuit.Generators.First;
        if pGen <> NIL then
        begin
            repeat
                if pGen.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := pGen;
                    Result := 1;
                end
                else
                    pGen := ActiveCircuit.Generators.Next;
            until (Result = 1) or (pGen = NIL);
        end
        else
            Result := 0;  // signify no more
    end;
end;

function TGenerators.Get_Name: Widestring;

var
    pGen: TGeneratorObj;

begin
    Result := '';
    if ActiveCircuit <> NIL then
    begin
        pGen := ActiveCircuit.Generators.Active;
        if pGen <> NIL then
        begin
            Result := pGen.Name;
        end
        else
            Result := '';  // signify no name
    end;

end;

function TGenerators.Get_Next: Integer;
var
    pGen: TGeneratorObj;

begin

    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pGen := ActiveCircuit.Generators.Next;
        if pGen <> NIL then
        begin
            repeat
                if pGen.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := pGen;
                    Result := ActiveCircuit.Generators.ActiveIndex;
                end
                else
                    pGen := ActiveCircuit.Generators.Next;
            until (Result > 0) or (pGen = NIL);
        end
        else
            Result := 0;  // signify no more
    end;

end;

function TGenerators.Get_RegisterNames: Olevariant;
var
    GeneratorClass: TGenerator;
    k: Integer;

begin
    GeneratorClass := DssClassList.Get(Classnames.Find('Generator'));
    Result := VarArrayCreate([0, NumGenRegisters - 1], varOleStr);
    for k := 0 to NumGenRegisters - 1 do
    begin
        Result[k] := GeneratorClass.RegisterNames[k + 1];
    end;

end;

function TGenerators.Get_RegisterValues: Olevariant;
var
    Gen: TGeneratorObj;
    k: Integer;
begin

    if ActiveCircuit <> NIL then
    begin
        Gen := TGeneratorObj(ActiveCircuit.Generators.Active);
        if Gen <> NIL then
        begin
            Result := VarArrayCreate([0, numGenRegisters - 1], varDouble);
            for k := 0 to numGenRegisters - 1 do
            begin
                Result[k] := Gen.Registers[k + 1];
            end;
        end
        else
            Result := VarArrayCreate([0, 0], varDouble);
    end
    else
    begin
        Result := VarArrayCreate([0, 0], varDouble);
    end;


end;

function TGenerators.Get_ForcedON: Wordbool;
begin
    Result := FALSE;
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Generators do
        begin
            if ActiveIndex <> 0 then
            begin
                Result := TGeneratorObj(Active).ForcedON;
            end;
        end;
    end;
end;

procedure TGenerators.Set_ForcedON(Value: Wordbool);

begin

    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Generators do
        begin
            if ActiveIndex <> 0 then
            begin
                TGeneratorObj(Active).ForcedON := Value;
            end;
        end;
    end;

end;

procedure TGenerators.Set_Name(const Value: Widestring);

var
    activesave: Integer;
    Gen: TGeneratorObj;
    S: String;
    Found: Boolean;
begin


    if ActiveCircuit <> NIL then
    begin      // Search list of generators in active circuit for name
        with ActiveCircuit.Generators do
        begin
            S := Value;  // Convert to Pascal String
            Found := FALSE;
            ActiveSave := ActiveIndex;
            Gen := First;
            while Gen <> NIL do
            begin
                if (CompareText(Gen.Name, S) = 0) then
                begin
                    ActiveCircuit.ActiveCktElement := Gen;
                    Found := TRUE;
                    Break;
                end;
                Gen := Next;
            end;
            if not Found then
            begin
                DoSimpleMsg('Generator "' + S + '" Not Found in Active Circuit.', 5003);
                Gen := Get(ActiveSave);    // Restore active generator
                ActiveCircuit.ActiveCktElement := Gen;
            end;
        end;
    end;
end;

function TGenerators.Get_kV: Double;
begin
    Result := -1.0;  // not set
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Generators do
        begin
            if ActiveIndex <> 0 then
            begin
                Result := TGeneratorObj(Active).GenVars.kVGeneratorBase;
            end;
        end;
    end;
end;

function TGenerators.Get_kvar: Double;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Generators do
        begin
            if ActiveIndex <> 0 then
            begin
                Result := TGeneratorObj(Active).Presentkvar;
            end;
        end;
    end;
end;

function TGenerators.Get_kW: Double;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Generators do
        begin
            if ActiveIndex <> 0 then
            begin
                Result := TGeneratorObj(Active).PresentkW;
            end;
        end;
    end;
end;

function TGenerators.Get_PF: Double;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Generators do
        begin
            if ActiveIndex <> 0 then
            begin
                Result := TGeneratorObj(Active).PowerFactor;
            end;
        end;
    end;
end;

function TGenerators.Get_Phases: Integer;
begin
    Result := 0;  // not set
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Generators do
        begin
            if ActiveIndex <> 0 then
            begin
                Result := TGeneratorObj(Active).nphases;
            end;
        end;
    end;
end;

procedure TGenerators.Set_kV(Value: Double);
begin

    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Generators do
        begin
            if ActiveIndex <> 0 then
            begin
                TGeneratorObj(Active).PresentkV := Value;
            end;
        end;
    end;

end;

procedure TGenerators.Set_kvar(Value: Double);
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Generators do
        begin
            if ActiveIndex <> 0 then
            begin
                TGeneratorObj(Active).Presentkvar := Value;
            end;
        end;
    end;
end;

procedure TGenerators.Set_kW(Value: Double);
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Generators do
        begin
            if ActiveIndex <> 0 then
            begin
                TGeneratorObj(Active).PresentkW := Value;
            end;
        end;
    end;
end;

procedure TGenerators.Set_PF(Value: Double);
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Generators do
        begin
            if ActiveIndex <> 0 then
            begin
                TGeneratorObj(Active).PowerFactor := Value;
            end;
        end;
    end;
end;

procedure TGenerators.Set_Phases(Value: Integer);
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Generators do
        begin
            if ActiveIndex <> 0 then
            begin
                TGeneratorObj(Active).Nphases := Value;
            end;
        end;
    end;
end;

function TGenerators.Get_Count: Integer;
begin
    if Assigned(Activecircuit) then
        Result := ActiveCircuit.Generators.ListSize;
end;

function TGenerators.Get_idx: Integer;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Generators.ActiveIndex
    else
        Result := 0;
end;

procedure TGenerators.Set_idx(Value: Integer);
var
    pGen: TGeneratorObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pGen := ActiveCircuit.Generators.Get(Value);
        if pGen <> NIL then
            ActiveCircuit.ActiveCktElement := pGen;
    end;

end;

function TGenerators.Get_Model: Integer;
begin
    Result := -1;
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Generators do
        begin
            if ActiveIndex <> 0 then
            begin
                Result := TGeneratorObj(Active).GenModel;
            end;
        end;
    end;
end;

procedure TGenerators.Set_Model(Value: Integer);
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Generators do
        begin
            if ActiveIndex <> 0 then
            begin
                with TGeneratorObj(Active) do
                begin
                    GenModel := Value;
                     // Handle side effect
                    if GenModel = 3 then
                        ActiveCircuit.Solution.SolutionInitialized := FALSE;
                end;
            end;
        end;
    end;
end;

function TGenerators.Get_kVArated: Double;
begin
    Result := -1.0;
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Generators do
        begin
            if ActiveIndex <> 0 then
            begin
                Result := TGeneratorObj(Active).Genvars.kVArating;
            end;
        end;
    end;
end;

procedure TGenerators.Set_kVArated(Value: Double);
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Generators do
        begin
            if ActiveIndex <> 0 then
            begin
                with TGeneratorObj(Active) do
                begin
                    Genvars.kVArating := Value;
                end;
            end;
        end;
    end;
end;

function TGenerators.Get_Vmaxpu: Double;
begin
    Result := -1.0;
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Generators do
        begin
            if ActiveIndex <> 0 then
            begin
                Result := TGeneratorObj(Active).Vmaxpu;
            end;
        end;
    end;
end;

function TGenerators.Get_Vminpu: Double;
begin
    Result := -1.0;
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Generators do
        begin
            if ActiveIndex <> 0 then
            begin
                Result := TGeneratorObj(Active).Vminpu;
            end;
        end;
    end;
end;

procedure TGenerators.Set_Vmaxpu(Value: Double);
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Generators do
        begin
            if ActiveIndex <> 0 then
            begin
                with TGeneratorObj(Active) do
                begin
                    VMaxPu := Value;
                end;
            end;
        end;
    end;
end;

procedure TGenerators.Set_Vminpu(Value: Double);
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Generators do
        begin
            if ActiveIndex <> 0 then
            begin
                with TGeneratorObj(Active) do
                begin
                    VMinPu := Value;
                end;
            end;
        end;
    end;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TGenerators, Class_Generators,
        ciInternal, tmApartment);
end.
