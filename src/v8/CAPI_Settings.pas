unit CAPI_Settings;

{$inline on}

interface

uses
    CAPI_Utils;

function Settings_Get_AllowDuplicates(): Wordbool; CDECL;
function Settings_Get_AutoBusList(): PAnsiChar; CDECL;
function Settings_Get_CktModel(): Integer; CDECL;
function Settings_Get_EmergVmaxpu(): Double; CDECL;
function Settings_Get_EmergVminpu(): Double; CDECL;
function Settings_Get_NormVmaxpu(): Double; CDECL;
function Settings_Get_NormVminpu(): Double; CDECL;
function Settings_Get_ZoneLock(): Wordbool; CDECL;
procedure Settings_Set_AllocationFactors(Value: Double); CDECL;
procedure Settings_Set_AllowDuplicates(Value: Wordbool); CDECL;
procedure Settings_Set_AutoBusList(const Value: PAnsiChar); CDECL;
procedure Settings_Set_CktModel(Value: Integer); CDECL;
procedure Settings_Set_EmergVmaxpu(Value: Double); CDECL;
procedure Settings_Set_EmergVminpu(Value: Double); CDECL;
procedure Settings_Set_NormVmaxpu(Value: Double); CDECL;
procedure Settings_Set_NormVminpu(Value: Double); CDECL;
procedure Settings_Set_ZoneLock(Value: Wordbool); CDECL;
procedure Settings_Get_LossRegs(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
procedure Settings_Get_LossRegs_GR(); CDECL;
function Settings_Get_LossWeight(): Double; CDECL;
function Settings_Get_Trapezoidal(): Wordbool; CDECL;
procedure Settings_Get_UEregs(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
procedure Settings_Get_UEregs_GR(); CDECL;
function Settings_Get_UEweight(): Double; CDECL;
procedure Settings_Set_LossRegs(ValuePtr: PInteger; ValueCount: Integer); CDECL;
procedure Settings_Set_LossWeight(Value: Double); CDECL;
procedure Settings_Set_Trapezoidal(Value: Wordbool); CDECL;
procedure Settings_Set_UEregs(ValuePtr: PInteger; ValueCount: Integer); CDECL;
procedure Settings_Set_UEweight(Value: Double); CDECL;
function Settings_Get_ControlTrace(): Wordbool; CDECL;
procedure Settings_Get_VoltageBases(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Settings_Get_VoltageBases_GR(); CDECL;
procedure Settings_Set_ControlTrace(Value: Wordbool); CDECL;
procedure Settings_Set_VoltageBases(ValuePtr: PDouble; ValueCount: Integer); CDECL;
function Settings_Get_PriceCurve(): PAnsiChar; CDECL;
function Settings_Get_PriceSignal(): Double; CDECL;
procedure Settings_Set_PriceCurve(const Value: PAnsiChar); CDECL;
procedure Settings_Set_PriceSignal(Value: Double); CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    ExecHelper;

function Settings_Get_AllowDuplicates(): Wordbool; CDECL;
begin

    if ActiveCircuit[ActiveActor] <> NIL then

        Result := ActiveCircuit[ActiveActor].DuplicatesAllowed

    else
        Result := FALSE;

end;
//------------------------------------------------------------------------------
function Settings_Get_AutoBusList_AnsiString(): Ansistring; inline;
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

function Settings_Get_AutoBusList(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Settings_Get_AutoBusList_AnsiString());
end;
//------------------------------------------------------------------------------
function Settings_Get_CktModel(): Integer; CDECL;
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
//------------------------------------------------------------------------------
function Settings_Get_EmergVmaxpu(): Double; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].EmergMaxVolts
    else
        Result := 0.0;
    ;

end;
//------------------------------------------------------------------------------
function Settings_Get_EmergVminpu(): Double; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].EmergMinVolts
    else
        Result := 0.0;
    ;

end;
//------------------------------------------------------------------------------
function Settings_Get_NormVmaxpu(): Double; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].NormalMaxVolts
    else
        Result := 0.0;
    ;


end;
//------------------------------------------------------------------------------
function Settings_Get_NormVminpu(): Double; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].NormalMinVolts
    else
        Result := 0.0;
    ;
end;
//------------------------------------------------------------------------------
function Settings_Get_ZoneLock(): Wordbool; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].ZonesLocked;
    end
    else
        Result := FALSE;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_AllocationFactors(Value: Double); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        DoSetAllocationFactors(Value);
end;
//------------------------------------------------------------------------------
procedure Settings_Set_AllowDuplicates(Value: Wordbool); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].DuplicatesAllowed := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_AutoBusList(const Value: PAnsiChar); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        DoAutoAddBusList(Value);
end;
//------------------------------------------------------------------------------
procedure Settings_Set_CktModel(Value: Integer); CDECL;
begin

    if ActiveCircuit[ActiveActor] <> NIL then

        case Value of
            dssPositiveSeq:
                ActiveCircuit[ActiveActor].PositiveSequence := TRUE;
        else
            ActiveCircuit[ActiveActor].PositiveSequence := FALSE;
        end;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_EmergVmaxpu(Value: Double); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].EmergMaxVolts := Value;

end;
//------------------------------------------------------------------------------
procedure Settings_Set_EmergVminpu(Value: Double); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].EmergMinVolts := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_NormVmaxpu(Value: Double); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].NormalMaxVolts := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_NormVminpu(Value: Double); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].NormalMinVolts := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_ZoneLock(Value: Wordbool); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].ZonesLocked := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Get_LossRegs(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
var
    Result: PIntegerArray;
    i: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (ActiveCircuit[ActiveActor].NumLossRegs - 1) + 1);
        for i := 0 to ActiveCircuit[ActiveActor].NumLossRegs - 1 do
        begin
            Result[i] := ActiveCircuit[ActiveActor].LossRegs^[i + 1]
        end;
    end
    else
        Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (0) + 1);

end;

procedure Settings_Get_LossRegs_GR(); CDECL;
// Same as Settings_Get_LossRegs but uses global result (GR) pointers
begin
    Settings_Get_LossRegs(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
function Settings_Get_LossWeight(): Double; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].LossWeight;
    end
    else
        Result := 0.0;
end;
//------------------------------------------------------------------------------
function Settings_Get_Trapezoidal(): Wordbool; CDECL;
begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].TrapezoidalIntegration;
    end
    else
        Result := FALSE;
end;
//------------------------------------------------------------------------------
procedure Settings_Get_UEregs(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
var
    Result: PIntegerArray;
    i: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (ActiveCircuit[ActiveActor].NumUERegs - 1) + 1);
        for i := 0 to ActiveCircuit[ActiveActor].NumUERegs - 1 do
        begin
            Result[i] := ActiveCircuit[ActiveActor].UERegs^[i + 1]
        end;
    end
    else
        Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (0) + 1);
end;

procedure Settings_Get_UEregs_GR(); CDECL;
// Same as Settings_Get_UEregs but uses global result (GR) pointers
begin
    Settings_Get_UEregs(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
function Settings_Get_UEweight(): Double; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].UEWeight
    end
    else
        Result := 0.0;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_LossRegs(ValuePtr: PInteger; ValueCount: Integer); CDECL;
var
    Value: PIntegerArray;
    i, j: Integer;
begin
    Value := PIntegerArray(ValuePtr);
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ReAllocMem(ActiveCircuit[ActiveActor].LossRegs, Sizeof(ActiveCircuit[ActiveActor].LossRegs^[1]) * (1 - (0) + (ValueCount - 1)));
        j := 1;
        for i := (0) to (ValueCount - 1) do
        begin
            ActiveCircuit[ActiveActor].LossRegs^[j] := Value[i];
            Inc(j);
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_LossWeight(Value: Double); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].LossWeight := Value
    end;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_Trapezoidal(Value: Wordbool); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].TrapezoidalIntegration := Value
    end;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_UEregs(ValuePtr: PInteger; ValueCount: Integer); CDECL;
var
    Value: PIntegerArray;
    i, j: Integer;
begin
    Value := PIntegerArray(ValuePtr);
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ReAllocMem(ActiveCircuit[ActiveActor].UERegs, Sizeof(ActiveCircuit[ActiveActor].UERegs^[1]) * (1 - (0) + (ValueCount - 1)));
        j := 1;
        for i := (0) to (ValueCount - 1) do
        begin
            ActiveCircuit[ActiveActor].UERegs^[j] := Value[i];
            Inc(j);
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_UEweight(Value: Double); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].UEWeight := Value
    end;
end;
//------------------------------------------------------------------------------
function Settings_Get_ControlTrace(): Wordbool; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].ControlQueue.TraceLog;
end;
//------------------------------------------------------------------------------
procedure Settings_Get_VoltageBases(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
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

            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (Count - 1) + 1);

            for i := 0 to Count - 1 do
                Result[i] := LegalVoltageBases^[i + 1];

        end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
end;

procedure Settings_Get_VoltageBases_GR(); CDECL;
// Same as Settings_Get_VoltageBases but uses global result (GR) pointers
begin
    Settings_Get_VoltageBases(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Settings_Set_ControlTrace(Value: Wordbool); CDECL;
begin

    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].ControlQueue.TraceLog := Value;

end;
//------------------------------------------------------------------------------
procedure Settings_Set_VoltageBases(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    i, j, Num: Integer;

begin
    Value := PDoubleArray(ValuePtr);
    Num := (ValueCount - 1) - (0) + 1;

     {LegalVoltageBases is a zero-terminated array, so we have to allocate
      one more than the number of actual values}

    with ActiveCircuit[ActiveActor] do
    begin
        Reallocmem(LegalVoltageBases, Sizeof(LegalVoltageBases^[1]) * (Num + 1));
        j := 1;
        for i := (0) to (ValueCount - 1) do
        begin
            LegalVoltageBases^[j] := Value[i];
            Inc(j)
        end;
        LegalVoltageBases^[Num + 1] := 0.0;
    end;

end;
//------------------------------------------------------------------------------
function Settings_Get_PriceCurve_AnsiString(): Ansistring; inline;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].PriceCurve
    else
        Result := '';
end;

function Settings_Get_PriceCurve(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Settings_Get_PriceCurve_AnsiString());
end;
//------------------------------------------------------------------------------
function Settings_Get_PriceSignal(): Double; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Pricesignal
    else
        Result := 0.0;

end;
//------------------------------------------------------------------------------
procedure Settings_Set_PriceCurve(const Value: PAnsiChar); CDECL;
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
//------------------------------------------------------------------------------
procedure Settings_Set_PriceSignal(Value: Double); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].PriceSignal := Value;
end;
//------------------------------------------------------------------------------
end.
