unit ImplPVSystems;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl;

type
    TPVSystems = class(TAutoObject, IPVSystems)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_RegisterNames: Olevariant; SAFECALL;
        function Get_RegisterValues: Olevariant; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_idx: Integer; SAFECALL;
        procedure Set_idx(Value: Integer); SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_Irradiance: Double; SAFECALL;
        procedure Set_Irradiance(Value: Double); SAFECALL;
        function Get_kvar: Double; SAFECALL;
        function Get_kVArated: Double; SAFECALL;
        function Get_kW: Double; SAFECALL;
        function Get_PF: Double; SAFECALL;
        procedure Set_kVArated(Value: Double); STDCALL;
        procedure Set_PF(Value: Double); STDCALL;
        procedure Set_kvar(Value: Double); STDCALL;

    end;

implementation

uses
    ComServ,
    DSSGlobals,
    PVSystem,
    Variants,
    SysUtils;

function TPVSystems.Get_AllNames: Olevariant;
var
    PVSystemElem: TPVSystemObj;
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
            if PVSystems.ListSize > 0 then
            begin
                VarArrayRedim(result, PVSystems.ListSize - 1);
                k := 0;
                PVSystemElem := PVSystems.First;
                while PVSystemElem <> NIL do
                begin
                    Result[k] := PVSystemElem.Name;
                    Inc(k);
                    PVSystemElem := PVSystems.Next;
                end;
            end;
end;

function TPVSystems.Get_RegisterNames: Olevariant;
var
    k: Integer;

begin
    Result := VarArrayCreate([0, NumPVSystemRegisters - 1], varOleStr);
    for k := 0 to NumPVSystemRegisters - 1 do
    begin
        Result[k] := PVSystemClass.RegisterNames[k + 1];
    end;
end;

function TPVSystems.Get_RegisterValues: Olevariant;
var
    PVSystem: TPVSystemObj;
    k: Integer;
begin

    if ActiveCircuit <> NIL then
    begin
        PVSystem := TPVSystemObj(ActiveCircuit.PVSystems.Active);
        if PVSystem <> NIL then
        begin
            Result := VarArrayCreate([0, numPVSystemRegisters - 1], varDouble);
            for k := 0 to numPVSystemRegisters - 1 do
            begin
                Result[k] := PVSystem.Registers[k + 1];
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

function TPVSystems.Get_First: Integer;
var
    pPVSystem: TpVSystemObj;

begin

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

function TPVSystems.Get_Next: Integer;
var
    pPVSystem: TPVSystemObj;

begin

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

function TPVSystems.Get_Count: Integer;
begin
    if Assigned(Activecircuit) then
        Result := ActiveCircuit.PVSystems.ListSize;
end;

function TPVSystems.Get_idx: Integer;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.PVSystems.ActiveIndex
    else
        Result := 0;
end;

procedure TPVSystems.Set_idx(Value: Integer);
var
    pPVSystem: TPVSystemObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pPVSystem := ActiveCircuit.PVSystems.Get(Value);
        if pPVSystem <> NIL then
            ActiveCircuit.ActiveCktElement := pPVSystem;
    end;
end;

function TPVSystems.Get_Name: Widestring;
var
    pPVSystem: TPVSystemObj;

begin
    Result := '';
    if ActiveCircuit <> NIL then
    begin
        pPVSystem := ActiveCircuit.PVSystems.Active;
        if pPVSystem <> NIL then
        begin
            Result := pPVSystem.Name;
        end
        else
            Result := '';  // signify no name
    end;

end;

procedure TPVSystems.Set_Name(const Value: Widestring);
var
    activesave: Integer;
    PVSystem: TPVSystemObj;
    S: String;
    Found: Boolean;
begin


    if ActiveCircuit <> NIL then
    begin      // Search list of PVSystems in active circuit for name
        with ActiveCircuit.PVSystems do
        begin
            S := Value;  // Convert to Pascal String
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

end;

function TPVSystems.Get_Irradiance: Double;
begin
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

procedure TPVSystems.Set_Irradiance(Value: Double);
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.PVSystems do
        begin
            if ActiveIndex <> 0 then
            begin
                TPVSystemObj(Active).PVSystemVars.FIrradiance := Value;
            end;
        end;
    end;
end;

function TPVSystems.Get_kvar: Double;
begin
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

function TPVSystems.Get_kVArated: Double;
begin
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

function TPVSystems.Get_kW: Double;
begin
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

function TPVSystems.Get_PF: Double;
begin
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

procedure TPVSystems.Set_kVArated(Value: Double);
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.PVSystems do
        begin
            if ActiveIndex <> 0 then
            begin
                TPVSystemObj(Active).kVARating := Value;
            end;
        end;
    end;
end;


procedure TPVSystems.Set_PF(Value: Double);
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.PVSystems do
        begin
            if ActiveIndex <> 0 then
            begin
                TPVSystemObj(Active).PowerFactor := Value;
            end;
        end;
    end;
end;

procedure TPVSystems.Set_kvar(Value: Double);
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.PVSystems do
        begin
            if ActiveIndex <> 0 then
            begin
                TPVSystemObj(Active).Presentkvar := Value;
            end;
        end;
    end;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TPVSystems, Class_PVSystems,
        ciInternal, tmApartment);
end.
