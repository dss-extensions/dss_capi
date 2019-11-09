unit CAPI_PVSystems;

{$inline on}

interface

uses
    CAPI_Utils;

procedure PVSystems_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure PVSystems_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure PVSystems_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure PVSystems_Get_RegisterValues_GR(); CDECL;
function PVSystems_Get_First(): Integer; CDECL;
function PVSystems_Get_Next(): Integer; CDECL;
function PVSystems_Get_Count(): Integer; CDECL;
function PVSystems_Get_idx(): Integer; CDECL;
procedure PVSystems_Set_idx(Value: Integer); CDECL;
function PVSystems_Get_Name(): PAnsiChar; CDECL;
procedure PVSystems_Set_Name(const Value: PAnsiChar); CDECL;
function PVSystems_Get_Irradiance(): Double; CDECL;
procedure PVSystems_Set_Irradiance(Value: Double); CDECL;
function PVSystems_Get_kvar(): Double; CDECL;
function PVSystems_Get_kVArated(): Double; CDECL;
function PVSystems_Get_kW(): Double; CDECL;
function PVSystems_Get_PF(): Double; CDECL;
procedure PVSystems_Set_kVArated(Value: Double); CDECL;
procedure PVSystems_Set_PF(Value: Double); CDECL;
procedure PVSystems_Set_kvar(Value: Double); CDECL;

// API Extensions
function PVSystems_Get_daily(): PAnsiChar; CDECL;
procedure PVSystems_Set_daily(const Value: PAnsiChar); CDECL;
function PVSystems_Get_duty(): PAnsiChar; CDECL;
procedure PVSystems_Set_duty(const Value: PAnsiChar); CDECL;
function PVSystems_Get_yearly(): PAnsiChar; CDECL;
procedure PVSystems_Set_yearly(const Value: PAnsiChar); CDECL;
function PVSystems_Get_Tdaily(): PAnsiChar; CDECL;
procedure PVSystems_Set_Tdaily(const Value: PAnsiChar); CDECL;
function PVSystems_Get_Tduty(): PAnsiChar; CDECL;
procedure PVSystems_Set_Tduty(const Value: PAnsiChar); CDECL;
function PVSystems_Get_Tyearly(): PAnsiChar; CDECL;
procedure PVSystems_Set_Tyearly(const Value: PAnsiChar); CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    PVSystem,
    SysUtils,
    DSSClass,
    DSSHelper;

procedure PVSystems_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit = NIL then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, ActiveCircuit.PVSystems, False);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    k: Integer;

begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumPVSystemRegisters - 1) + 1);
    for k := 0 to NumPVSystemRegisters - 1 do
    begin
        Result[k] := DSS_CopyStringAsPChar(DSSPrime.PVSystemClass.RegisterNames[k + 1]);
    end;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    PVSystem: TPVSystemObj;
    k: Integer;
begin

    if ActiveCircuit <> NIL then
    begin
        PVSystem := TPVSystemObj(ActiveCircuit.PVSystems.Active);
        if PVSystem <> NIL then
        begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (numPVSystemRegisters - 1) + 1);
            for k := 0 to numPVSystemRegisters - 1 do
            begin
                Result[k] := PVSystem.Registers[k + 1];
            end;
        end
        else
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    end
    else
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    end;


end;

procedure PVSystems_Get_RegisterValues_GR(); CDECL;
// Same as PVSystems_Get_RegisterValues but uses global result (GR) pointers
begin
    PVSystems_Get_RegisterValues(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function PVSystems_Get_First(): Integer; CDECL;
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
//------------------------------------------------------------------------------
function PVSystems_Get_Next(): Integer; CDECL;
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
//------------------------------------------------------------------------------
function PVSystems_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if Assigned(Activecircuit) then
        Result := ActiveCircuit.PVSystems.ListSize;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_idx(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.PVSystems.ActiveIndex
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_idx(Value: Integer); CDECL;
var
    pPVSystem: TPVSystemObj;
begin
    if ActiveCircuit = NIL then
        Exit;
    pPVSystem := ActiveCircuit.PVSystems.Get(Value);
    if pPVSystem = NIL then
    begin
        DoSimpleMsg('Invalid PVSystem index: "' + IntToStr(Value) + '".', 656565);
    end;
    ActiveCircuit.ActiveCktElement := pPVSystem;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Name_AnsiString(): Ansistring; inline;
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

function PVSystems_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(PVSystems_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if ActiveCircuit = NIL then
        Exit;
    if DSSPrime.PVSystemClass.SetActive(Value) then
    begin
        ActiveCircuit.ActiveCktElement := DSSPrime.PVSystemClass.ElementList.Active;
        ActiveCircuit.PVSystems.Get(DSSPrime.PVSystemClass.Active);
    end
    else
    begin
        DoSimpleMsg('PVSystem "' + Value + '" Not Found in Active Circuit.', 5003);
    end;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Irradiance(): Double; CDECL;
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
//------------------------------------------------------------------------------
procedure PVSystems_Set_Irradiance(Value: Double); CDECL;
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
//------------------------------------------------------------------------------
function PVSystems_Get_kvar(): Double; CDECL;
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
//------------------------------------------------------------------------------
function PVSystems_Get_kVArated(): Double; CDECL;
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
//------------------------------------------------------------------------------
function PVSystems_Get_kW(): Double; CDECL;
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
//------------------------------------------------------------------------------
function PVSystems_Get_PF(): Double; CDECL;
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
//------------------------------------------------------------------------------
procedure PVSystems_Set_kVArated(Value: Double); CDECL;
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
//------------------------------------------------------------------------------
procedure PVSystems_Set_PF(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.PVSystems do
        begin
            if ActiveIndex <> 0 then
            begin
                TPVSystemObj(Active).Varmode := 0;
                TPVSystemObj(Active).PowerFactor := Value;
            end;
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_kvar(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.PVSystems do
        begin
            if ActiveIndex <> 0 then
            begin
                TPVSystemObj(Active).Varmode := VARMODEKVAR;
                TPVSystemObj(Active).Presentkvar := Value;
            end;
        end;
    end;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_daily(): PAnsiChar; CDECL;
begin
    Result := nil;
    if ActiveCircuit = nil then
        Exit;
    if ActiveCircuit.PVSystems.ActiveIndex = 0 then
        Exit;
        
    with ActiveCircuit.PVSystems do
    begin
        Result := DSS_GetAsPAnsiChar(TPVSystemObj(Active).DailyShape);
    end;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_daily(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
begin
    if ActiveCircuit = nil then
        Exit;
    if ActiveCircuit.PVSystems.ActiveIndex = 0 then
        Exit;
        
    elem := TPVSystemObj(ActiveCircuit.PVSystems.Active);
    with elem do
    begin
        DailyShape := Value;
        DailyShapeObj := DSSPrime.LoadShapeClass.Find(DailyShape);
    end;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_duty(): PAnsiChar; CDECL;
begin
    Result := nil;
    if ActiveCircuit = nil then
        Exit;
    if ActiveCircuit.PVSystems.ActiveIndex = 0 then
        Exit;
        
    with ActiveCircuit.PVSystems do
    begin
        Result := DSS_GetAsPAnsiChar(TPVSystemObj(Active).DutyShape);
    end;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_duty(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
begin
    if ActiveCircuit = nil then
        Exit;
    if ActiveCircuit.PVSystems.ActiveIndex = 0 then
        Exit;
        
    elem := TPVSystemObj(ActiveCircuit.PVSystems.Active);
    with elem do
    begin
        DutyShape := Value;
        DutyShapeObj := DSSPrime.LoadShapeClass.Find(DutyShape);
    end;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_yearly(): PAnsiChar; CDECL;
begin
    Result := nil;
    if ActiveCircuit = nil then
        Exit;
    if ActiveCircuit.PVSystems.ActiveIndex = 0 then
        Exit;
        
    with ActiveCircuit.PVSystems do
    begin
        Result := DSS_GetAsPAnsiChar(TPVSystemObj(Active).YearlyShape);
    end;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_yearly(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
begin
    if ActiveCircuit = nil then
        Exit;
    if ActiveCircuit.PVSystems.ActiveIndex = 0 then
        Exit;
        
    elem := TPVSystemObj(ActiveCircuit.PVSystems.Active);
    with elem do
    begin
        YearlyShape := Value;
        YearlyShapeObj := DSSPrime.LoadShapeClass.Find(YearlyShape);
    end;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Tdaily(): PAnsiChar; CDECL;
begin
    Result := nil;
    if ActiveCircuit = nil then
        Exit;
    if ActiveCircuit.PVSystems.ActiveIndex = 0 then
        Exit;
        
    with ActiveCircuit.PVSystems do
    begin
        Result := DSS_GetAsPAnsiChar(TPVSystemObj(Active).DailyTShape);
    end;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Tdaily(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
begin
    if ActiveCircuit = nil then
        Exit;
    if ActiveCircuit.PVSystems.ActiveIndex = 0 then
        Exit;
        
    elem := TPVSystemObj(ActiveCircuit.PVSystems.Active);
    with elem do
    begin
        DailyTShape := Value;
        DailyTShapeObj := DSSPrime.TShapeClass.Find(DailyTShape);
    end;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Tduty(): PAnsiChar; CDECL;
begin
    Result := nil;
    if ActiveCircuit = nil then
        Exit;
    if ActiveCircuit.PVSystems.ActiveIndex = 0 then
        Exit;
        
    with ActiveCircuit.PVSystems do
    begin
        Result := DSS_GetAsPAnsiChar(TPVSystemObj(Active).DutyTShape);
    end;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Tduty(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
begin
    if ActiveCircuit = nil then
        Exit;
    if ActiveCircuit.PVSystems.ActiveIndex = 0 then
        Exit;
        
    elem := TPVSystemObj(ActiveCircuit.PVSystems.Active);
    with elem do
    begin
        DutyTShape := Value;
        DutyTShapeObj := DSSPrime.TShapeClass.Find(DutyTShape);
    end;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Tyearly(): PAnsiChar; CDECL;
begin
    Result := nil;
    if ActiveCircuit = nil then
        Exit;
    if ActiveCircuit.PVSystems.ActiveIndex = 0 then
        Exit;
        
    with ActiveCircuit.PVSystems do
    begin
        Result := DSS_GetAsPAnsiChar(TPVSystemObj(Active).YearlyTShape);
    end;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Tyearly(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
begin
    if ActiveCircuit = nil then
        Exit;
    if ActiveCircuit.PVSystems.ActiveIndex = 0 then
        Exit;
        
    elem := TPVSystemObj(ActiveCircuit.PVSystems.Active);
    with elem do
    begin
        YearlyTShape := Value;
        YearlyTShapeObj := DSSPrime.TShapeClass.Find(YearlyTShape);
    end;
end;
//------------------------------------------------------------------------------
end.
