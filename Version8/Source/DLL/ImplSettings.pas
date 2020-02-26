unit ImplSettings;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSEngine_TLB,
    StdVcl;

type
    TSettings = class(TAutoObject, ISettings)
    PROTECTED
        function Get_AllowDuplicates: Wordbool; SAFECALL;
        function Get_AutoBusList: Widestring; SAFECALL;
        function Get_CktModel: Integer; SAFECALL;
        function Get_EmergVmaxpu: Double; SAFECALL;
        function Get_EmergVminpu: Double; SAFECALL;
        function Get_NormVmaxpu: Double; SAFECALL;
        function Get_NormVminpu: Double; SAFECALL;
        function Get_ZoneLock: Wordbool; SAFECALL;
        procedure Set_AllocationFactors(Value: Double); SAFECALL;
        procedure Set_AllowDuplicates(Value: Wordbool); SAFECALL;
        procedure Set_AutoBusList(const Value: Widestring); SAFECALL;
        procedure Set_CktModel(Value: Integer); SAFECALL;
        procedure Set_EmergVmaxpu(Value: Double); SAFECALL;
        procedure Set_EmergVminpu(Value: Double); SAFECALL;
        procedure Set_NormVmaxpu(Value: Double); SAFECALL;
        procedure Set_NormVminpu(Value: Double); SAFECALL;
        procedure Set_ZoneLock(Value: Wordbool); SAFECALL;
        function Get_LossRegs: Olevariant; SAFECALL;
        function Get_LossWeight: Double; SAFECALL;
        function Get_Trapezoidal: Wordbool; SAFECALL;
        function Get_UEregs: Olevariant; SAFECALL;
        function Get_UEweight: Double; SAFECALL;
        procedure Set_LossRegs(Value: Olevariant); SAFECALL;
        procedure Set_LossWeight(Value: Double); SAFECALL;
        procedure Set_Trapezoidal(Value: Wordbool); SAFECALL;
        procedure Set_UEregs(Value: Olevariant); SAFECALL;
        procedure Set_UEweight(Value: Double); SAFECALL;
        function Get_ControlTrace: Wordbool; SAFECALL;
        function Get_VoltageBases: Olevariant; SAFECALL;
        procedure Set_ControlTrace(Value: Wordbool); SAFECALL;
        procedure Set_VoltageBases(Value: Olevariant); SAFECALL;
        function Get_PriceCurve: Widestring; SAFECALL;
        function Get_PriceSignal: Double; SAFECALL;
        procedure Set_PriceCurve(const Value: Widestring); SAFECALL;
        procedure Set_PriceSignal(Value: Double); SAFECALL;
    { Protected declarations }
    end;

implementation

uses
    ComServ,
    DSSGlobals,
    ExecHelper,
    Variants;

function TSettings.Get_AllowDuplicates: Wordbool;
begin

    if ActiveCircuit[ActiveActor] <> NIL then

        Result := ActiveCircuit[ActiveActor].DuplicatesAllowed

    else
        Result := FALSE;

end;

function TSettings.Get_AutoBusList: Widestring;
var
    i: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor].AutoAddBusList do
        begin
            for i := 1 to ListSize do
                AppendGlobalResult(Get(i));
            Result := GlobalResult;
        end
    else
        Result := '';

end;

function TSettings.Get_CktModel: Integer;
begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin

        if ActiveCircuit[ActiveActor].PositiveSequence then
            Result := dssPositiveSeq
        else
            Result := dssMultiPhase;
    end
    else
        Result := 0;

end;

function TSettings.Get_EmergVmaxpu: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].EmergMaxVolts
    else
        Result := 0.0;
    ;

end;

function TSettings.Get_EmergVminpu: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].EmergMinVolts
    else
        Result := 0.0;
    ;

end;

function TSettings.Get_NormVmaxpu: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].NormalMaxVolts
    else
        Result := 0.0;
    ;


end;

function TSettings.Get_NormVminpu: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].NormalMinVolts
    else
        Result := 0.0;
    ;
end;

function TSettings.Get_ZoneLock: Wordbool;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].ZonesLocked;
    end
    else
        Result := FALSE;
end;

procedure TSettings.Set_AllocationFactors(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        DoSetAllocationFactors(Value);
end;

procedure TSettings.Set_AllowDuplicates(Value: Wordbool);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].DuplicatesAllowed := Value;
end;

procedure TSettings.Set_AutoBusList(const Value: Widestring);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        DoAutoAddBusList(Value);
end;

procedure TSettings.Set_CktModel(Value: Integer);
begin

    if ActiveCircuit[ActiveActor] <> NIL then

        case Value of
            dssPositiveSeq:
                ActiveCircuit[ActiveActor].PositiveSequence := TRUE;
        else
            ActiveCircuit[ActiveActor].PositiveSequence := FALSE;
        end;
end;

procedure TSettings.Set_EmergVmaxpu(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].EmergMaxVolts := Value;

end;

procedure TSettings.Set_EmergVminpu(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].EmergMinVolts := Value;
end;

procedure TSettings.Set_NormVmaxpu(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].NormalMaxVolts := Value;
end;

procedure TSettings.Set_NormVminpu(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].NormalMinVolts := Value;
end;

procedure TSettings.Set_ZoneLock(Value: Wordbool);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].ZonesLocked := Value;
end;

function TSettings.Get_LossRegs: Olevariant;
var
    i: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := VarArrayCreate([0, ActiveCircuit[ActiveActor].NumLossRegs - 1], varInteger);
        for i := 0 to ActiveCircuit[ActiveActor].NumLossRegs - 1 do
        begin
            Result[i] := ActiveCircuit[ActiveActor].LossRegs^[i + 1]
        end;
    end
    else
        Result := VarArrayCreate([0, 0], varInteger);

end;

function TSettings.Get_LossWeight: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].LossWeight;
    end
    else
        Result := 0.0;
end;

function TSettings.Get_Trapezoidal: Wordbool;
begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].TrapezoidalIntegration;
    end
    else
        Result := FALSE;
end;

function TSettings.Get_UEregs: Olevariant;
var
    i: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := VarArrayCreate([0, ActiveCircuit[ActiveActor].NumUERegs - 1], varInteger);
        for i := 0 to ActiveCircuit[ActiveActor].NumUERegs - 1 do
        begin
            Result[i] := ActiveCircuit[ActiveActor].UERegs^[i + 1]
        end;
    end
    else
        Result := VarArrayCreate([0, 0], varInteger);
end;

function TSettings.Get_UEweight: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].UEWeight
    end
    else
        Result := 0.0;
end;

procedure TSettings.Set_LossRegs(Value: Olevariant);
var
    i, j: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ReAllocMem(ActiveCircuit[ActiveActor].LossRegs, Sizeof(ActiveCircuit[ActiveActor].LossRegs^[1]) * (1 - VarArrayLowBound(Value, 1) + VarArrayHighBound(Value, 1)));
        j := 1;
        for i := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
        begin
            ActiveCircuit[ActiveActor].LossRegs^[j] := Value[i];
            Inc(j);
        end;
    end;
end;

procedure TSettings.Set_LossWeight(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].LossWeight := Value
    end;
end;

procedure TSettings.Set_Trapezoidal(Value: Wordbool);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].TrapezoidalIntegration := Value
    end;
end;

procedure TSettings.Set_UEregs(Value: Olevariant);
var
    i, j: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ReAllocMem(ActiveCircuit[ActiveActor].UERegs, Sizeof(ActiveCircuit[ActiveActor].UERegs^[1]) * (1 - VarArrayLowBound(Value, 1) + VarArrayHighBound(Value, 1)));
        j := 1;
        for i := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
        begin
            ActiveCircuit[ActiveActor].UERegs^[j] := Value[i];
            Inc(j);
        end;
    end;
end;

procedure TSettings.Set_UEweight(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].UEWeight := Value
    end;
end;

function TSettings.Get_ControlTrace: Wordbool;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].ControlQueue.TraceLog;
end;

function TSettings.Get_VoltageBases: Olevariant;

var
    i, Count: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
      {Count the number of voltagebases specified}
            i := 0;
            repeat
                Inc(i);
            until LegalVoltageBases^[i] = 0.0;
            Count := i - 1;

            Result := VarArrayCreate([0, Count - 1], varDouble);

            for i := 0 to Count - 1 do
                Result[i] := LegalVoltageBases^[i + 1];

        end
    else
        Result := VarArrayCreate([0, 0], varDouble);
end;

procedure TSettings.Set_ControlTrace(Value: Wordbool);
begin

    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].ControlQueue.TraceLog := Value;

end;

procedure TSettings.Set_VoltageBases(Value: Olevariant);

var
    i, j, Num: Integer;

begin

    Num := VarArrayHighBound(Value, 1) - VarArrayLowBound(Value, 1) + 1;

     {LegalVoltageBases is a zero-terminated array, so we have to allocate
      one more than the number of actual values}

    with ActiveCircuit[ActiveActor] do
    begin
        Reallocmem(LegalVoltageBases, Sizeof(LegalVoltageBases^[1]) * (Num + 1));
        j := 1;
        for i := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
        begin
            LegalVoltageBases^[j] := Value[i];
            Inc(j)
        end;
        LegalVoltageBases^[Num + 1] := 0.0;
    end;

end;

function TSettings.Get_PriceCurve: Widestring;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].PriceCurve
    else
        Result := '';
end;

function TSettings.Get_PriceSignal: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Pricesignal
    else
        Result := 0.0;

end;

procedure TSettings.Set_PriceCurve(const Value: Widestring);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            PriceCurve := Value;
            PriceCurveObj := LoadShapeClass[ActiveActor].Find(Pricecurve);
            if PriceCurveObj = NIL then
                DoSimpleMsg('Price Curve: "' + Pricecurve + '" not found.', 5006);
        end;
end;

procedure TSettings.Set_PriceSignal(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].PriceSignal := Value;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TSettings, Class_Settings,
        ciInternal, tmApartment);
end.
