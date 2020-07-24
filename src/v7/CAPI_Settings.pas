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

// API extensions
function Settings_Get_LoadsTerminalCheck(): Wordbool; CDECL;
procedure Settings_Set_LoadsTerminalCheck(Value: Wordbool); CDECL;
procedure Settings_Set_IterateDisabled(Value: Integer); CDECL;
function Settings_Get_IterateDisabled(): Integer; CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    ExecHelper;

function Settings_Get_AllowDuplicates(): Wordbool; CDECL;
begin
    Result := FALSE;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.DuplicatesAllowed
end;
//------------------------------------------------------------------------------
function Settings_Get_AutoBusList(): PAnsiChar; CDECL;
var
    i: Integer;
begin
    Result := NIL;
    if InvalidCircuit then
        Exit;

    GlobalResult := '';
    with ActiveCircuit.AutoAddBusList do
    begin
        for i := 1 to ListSize do
            AppendGlobalResult(Get(i));
        Result := DSS_GetAsPAnsiChar(GlobalResult);
    end
end;
//------------------------------------------------------------------------------
function Settings_Get_CktModel(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;

    if ActiveCircuit.PositiveSequence then
        Result := dssPositiveSeq
    else
        Result := dssMultiPhase;
end;
//------------------------------------------------------------------------------
function Settings_Get_EmergVmaxpu(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.EmergMaxVolts
end;
//------------------------------------------------------------------------------
function Settings_Get_EmergVminpu(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.EmergMinVolts
end;
//------------------------------------------------------------------------------
function Settings_Get_NormVmaxpu(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.NormalMaxVolts
end;
//------------------------------------------------------------------------------
function Settings_Get_NormVminpu(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.NormalMinVolts
end;
//------------------------------------------------------------------------------
function Settings_Get_ZoneLock(): Wordbool; CDECL;
begin
    Result := FALSE;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.ZonesLocked;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_AllocationFactors(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    DoSetAllocationFactors(Value);
end;
//------------------------------------------------------------------------------
procedure Settings_Set_AllowDuplicates(Value: Wordbool); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.DuplicatesAllowed := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_AutoBusList(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit then
        Exit;
    DoAutoAddBusList(Value);
end;
//------------------------------------------------------------------------------
procedure Settings_Set_CktModel(Value: Integer); CDECL;
begin
    if InvalidCircuit then
        Exit;

    case Value of
        dssPositiveSeq:
            ActiveCircuit.PositiveSequence := TRUE;
    else
        ActiveCircuit.PositiveSequence := FALSE;
    end;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_EmergVmaxpu(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.EmergMaxVolts := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_EmergVminpu(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.EmergMinVolts := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_NormVmaxpu(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.NormalMaxVolts := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_NormVminpu(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.NormalMinVolts := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_ZoneLock(Value: Wordbool); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.ZonesLocked := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Get_LossRegs(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
begin
    if InvalidCircuit then
    begin
        DSS_RecreateArray_PInteger(ResultPtr, ResultCount, 1);
        Exit;
    end;
    DSS_RecreateArray_PInteger(ResultPtr, ResultCount, ActiveCircuit.NumLossRegs);
    Move(ActiveCircuit.LossRegs[1], ResultPtr^, ActiveCircuit.NumLossRegs * SizeOf(Integer));
end;

procedure Settings_Get_LossRegs_GR(); CDECL;
// Same as Settings_Get_LossRegs but uses global result (GR) pointers
begin
    Settings_Get_LossRegs(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
function Settings_Get_LossWeight(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.LossWeight;
end;
//------------------------------------------------------------------------------
function Settings_Get_Trapezoidal(): Wordbool; CDECL;
begin
    Result := FALSE;
    if InvalidCircuit then
        Exit;

    Result := ActiveCircuit.TrapezoidalIntegration;
end;
//------------------------------------------------------------------------------
procedure Settings_Get_UEregs(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
begin
    if InvalidCircuit then
    begin
        DSS_RecreateArray_PInteger(ResultPtr, ResultCount, 1);
        Exit;
    end;
    DSS_RecreateArray_PInteger(ResultPtr, ResultCount, ActiveCircuit.NumUERegs);
    Move(ActiveCircuit.UERegs[1], ResultPtr^, ActiveCircuit.NumUERegs * SizeOf(Integer));
end;

procedure Settings_Get_UEregs_GR(); CDECL;
// Same as Settings_Get_UEregs but uses global result (GR) pointers
begin
    Settings_Get_UEregs(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
function Settings_Get_UEweight(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.UEWeight
end;
//------------------------------------------------------------------------------
procedure Settings_Set_LossRegs(ValuePtr: PInteger; ValueCount: Integer); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ReAllocMem(ActiveCircuit.LossRegs, Sizeof(Integer) * ValueCount);
    Move(ValuePtr^, ActiveCircuit.LossRegs[1], ValueCount * SizeOf(Integer));
    ActiveCircuit.NumLossRegs := ValueCount;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_LossWeight(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.LossWeight := Value
end;
//------------------------------------------------------------------------------
procedure Settings_Set_Trapezoidal(Value: Wordbool); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.TrapezoidalIntegration := Value
end;
//------------------------------------------------------------------------------
procedure Settings_Set_UEregs(ValuePtr: PInteger; ValueCount: Integer); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ReAllocMem(ActiveCircuit.UERegs, Sizeof(Integer) * ValueCount);
    Move(ValuePtr^, ActiveCircuit.UERegs[1], ValueCount * SizeOf(Integer));
    ActiveCircuit.NumUEregs := ValueCount;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_UEweight(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.UEWeight := Value
end;
//------------------------------------------------------------------------------
function Settings_Get_ControlTrace(): Wordbool; CDECL;
begin
    Result := FALSE;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.ControlQueue.TraceLog;
end;
//------------------------------------------------------------------------------
procedure Settings_Get_VoltageBases(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    i, Count: Integer;
begin
    if InvalidCircuit then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;

    with ActiveCircuit do
    begin
        {Count the number of voltagebases specified}
        i := 0;
        repeat
            Inc(i);
        until LegalVoltageBases^[i] = 0.0;
        Count := i - 1;

        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, Count);
        Move(LegalVoltageBases[1], ResultPtr^, Count * SizeOf(Double));
    end
end;

procedure Settings_Get_VoltageBases_GR(); CDECL;
// Same as Settings_Get_VoltageBases but uses global result (GR) pointers
begin
    Settings_Get_VoltageBases(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Settings_Set_ControlTrace(Value: Wordbool); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.ControlQueue.TraceLog := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_VoltageBases(ValuePtr: PDouble; ValueCount: Integer); CDECL;
begin
    if InvalidCircuit then
        Exit;
        
    {LegalVoltageBases is a zero-terminated array, so we have to allocate
     one more than the number of actual values}
    with ActiveCircuit do
    begin
        Reallocmem(LegalVoltageBases, SizeOf(Double) * (ValueCount + 1));
        Move(ValuePtr^, LegalVoltageBases[1], ValueCount * SizeOf(Double));
        LegalVoltageBases^[ValueCount + 1] := 0.0;
    end;
end;
//------------------------------------------------------------------------------
function Settings_Get_PriceCurve(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if InvalidCircuit then
        Exit;
    Result := DSS_GetAsPAnsiChar(ActiveCircuit.PriceCurve)
end;
//------------------------------------------------------------------------------
function Settings_Get_PriceSignal(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Pricesignal
end;
//------------------------------------------------------------------------------
procedure Settings_Set_PriceCurve(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit then
        Exit;
    with ActiveCircuit do
    begin
        PriceCurve := Value;
        PriceCurveObj := LoadShapeClass.Find(Pricecurve);
        if PriceCurveObj = NIL then
            DoSimpleMsg('Price Curve: "' + Pricecurve + '" not found.', 5006);
    end;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_PriceSignal(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.PriceSignal := Value;
end;
//------------------------------------------------------------------------------
function Settings_Get_LoadsTerminalCheck(): Wordbool; CDECL;
begin
    Result := DSS_CAPI_LOADS_TERMINAL_CHECK;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_LoadsTerminalCheck(Value: Wordbool); CDECL;
begin
    DSS_CAPI_LOADS_TERMINAL_CHECK := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_IterateDisabled(Value: Integer); CDECL;
begin
    DSS_CAPI_ITERATE_DISABLED := Value;
end;
//------------------------------------------------------------------------------
function Settings_Get_IterateDisabled(): Integer; CDECL;
begin
    Result := DSS_CAPI_ITERATE_DISABLED;
end;
//------------------------------------------------------------------------------
end.
