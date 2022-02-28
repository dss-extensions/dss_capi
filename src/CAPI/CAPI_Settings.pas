unit CAPI_Settings;

interface

uses
    CAPI_Utils,
    CAPI_Types;

function Settings_Get_AllowDuplicates(): TAPIBoolean; CDECL;
function Settings_Get_AutoBusList(): PAnsiChar; CDECL;
function Settings_Get_CktModel(): Integer; CDECL;
function Settings_Get_EmergVmaxpu(): Double; CDECL;
function Settings_Get_EmergVminpu(): Double; CDECL;
function Settings_Get_NormVmaxpu(): Double; CDECL;
function Settings_Get_NormVminpu(): Double; CDECL;
function Settings_Get_ZoneLock(): TAPIBoolean; CDECL;
procedure Settings_Set_AllocationFactors(Value: Double); CDECL;
procedure Settings_Set_AllowDuplicates(Value: TAPIBoolean); CDECL;
procedure Settings_Set_AutoBusList(const Value: PAnsiChar); CDECL;
procedure Settings_Set_CktModel(Value: Integer); CDECL;
procedure Settings_Set_EmergVmaxpu(Value: Double); CDECL;
procedure Settings_Set_EmergVminpu(Value: Double); CDECL;
procedure Settings_Set_NormVmaxpu(Value: Double); CDECL;
procedure Settings_Set_NormVminpu(Value: Double); CDECL;
procedure Settings_Set_ZoneLock(Value: TAPIBoolean); CDECL;
procedure Settings_Get_LossRegs(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
procedure Settings_Get_LossRegs_GR(); CDECL;
function Settings_Get_LossWeight(): Double; CDECL;
function Settings_Get_Trapezoidal(): TAPIBoolean; CDECL;
procedure Settings_Get_UEregs(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
procedure Settings_Get_UEregs_GR(); CDECL;
function Settings_Get_UEweight(): Double; CDECL;
procedure Settings_Set_LossRegs(ValuePtr: PInteger; ValueCount: TAPISize); CDECL;
procedure Settings_Set_LossWeight(Value: Double); CDECL;
procedure Settings_Set_Trapezoidal(Value: TAPIBoolean); CDECL;
procedure Settings_Set_UEregs(ValuePtr: PInteger; ValueCount: TAPISize); CDECL;
procedure Settings_Set_UEweight(Value: Double); CDECL;
function Settings_Get_ControlTrace(): TAPIBoolean; CDECL;
procedure Settings_Get_VoltageBases(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Settings_Get_VoltageBases_GR(); CDECL;
procedure Settings_Set_ControlTrace(Value: TAPIBoolean); CDECL;
procedure Settings_Set_VoltageBases(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
function Settings_Get_PriceCurve(): PAnsiChar; CDECL;
function Settings_Get_PriceSignal(): Double; CDECL;
procedure Settings_Set_PriceCurve(const Value: PAnsiChar); CDECL;
procedure Settings_Set_PriceSignal(Value: Double); CDECL;

// API extensions
function Settings_Get_LoadsTerminalCheck(): TAPIBoolean; CDECL;
procedure Settings_Set_LoadsTerminalCheck(Value: TAPIBoolean); CDECL;
procedure Settings_Set_IterateDisabled(Value: Integer); CDECL;
function Settings_Get_IterateDisabled(): Integer; CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    ExecHelper,
    Executive,
    DSSClass,
    DSSHelper,
    SysUtils;

function Settings_Get_AllowDuplicates(): TAPIBoolean; CDECL;
begin
    Result := FALSE;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.DuplicatesAllowed
end;
//------------------------------------------------------------------------------
function Settings_Get_AutoBusList(): PAnsiChar; CDECL;
var
    i: Integer;
begin
    Result := NIL;
    if InvalidCircuit(DSSPrime) then
        Exit;

    DSSPrime.GlobalResult := '';
    with DSSPrime.ActiveCircuit.AutoAddBusList do
    begin
        for i := 1 to Count do
            AppendGlobalResult(DSSPrime, NameOfIndex(i));
        Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.GlobalResult);
    end
end;
//------------------------------------------------------------------------------
function Settings_Get_CktModel(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;

    if DSSPrime.ActiveCircuit.PositiveSequence then
        Result := dssPositiveSeq
    else
        Result := dssMultiPhase;
end;
//------------------------------------------------------------------------------
function Settings_Get_EmergVmaxpu(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.EmergMaxVolts
end;
//------------------------------------------------------------------------------
function Settings_Get_EmergVminpu(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.EmergMinVolts
end;
//------------------------------------------------------------------------------
function Settings_Get_NormVmaxpu(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.NormalMaxVolts
end;
//------------------------------------------------------------------------------
function Settings_Get_NormVminpu(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.NormalMinVolts
end;
//------------------------------------------------------------------------------
function Settings_Get_ZoneLock(): TAPIBoolean; CDECL;
begin
    Result := FALSE;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.ZonesLocked;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_AllocationFactors(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.DSSExecutive.DoSetAllocationFactors(Value);
end;
//------------------------------------------------------------------------------
procedure Settings_Set_AllowDuplicates(Value: TAPIBoolean); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.DuplicatesAllowed := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_AutoBusList(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.DSSExecutive.DoAutoAddBusList(Value);
end;
//------------------------------------------------------------------------------
procedure Settings_Set_CktModel(Value: Integer); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    case Value of
        dssPositiveSeq:
            DSSPrime.ActiveCircuit.PositiveSequence := TRUE;
    else
        DSSPrime.ActiveCircuit.PositiveSequence := FALSE;
    end;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_EmergVmaxpu(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.EmergMaxVolts := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_EmergVminpu(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.EmergMinVolts := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_NormVmaxpu(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.NormalMaxVolts := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_NormVminpu(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.NormalMinVolts := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_ZoneLock(Value: TAPIBoolean); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.ZonesLocked := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Get_LossRegs(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    DSS_RecreateArray_PInteger(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.NumLossRegs);
    Move(DSSPrime.ActiveCircuit.LossRegs[1], ResultPtr^, DSSPrime.ActiveCircuit.NumLossRegs * SizeOf(Integer));
end;

procedure Settings_Get_LossRegs_GR(); CDECL;
// Same as Settings_Get_LossRegs but uses global result (GR) pointers
begin
    Settings_Get_LossRegs(DSSPrime.GR_DataPtr_PInteger, @DSSPrime.GR_Counts_PInteger[0])
end;

//------------------------------------------------------------------------------
function Settings_Get_LossWeight(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.LossWeight;
end;
//------------------------------------------------------------------------------
function Settings_Get_Trapezoidal(): TAPIBoolean; CDECL;
begin
    Result := FALSE;
    if InvalidCircuit(DSSPrime) then
        Exit;

    Result := DSSPrime.ActiveCircuit.TrapezoidalIntegration;
end;
//------------------------------------------------------------------------------
procedure Settings_Get_UEregs(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    DSS_RecreateArray_PInteger(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.NumUERegs);
    Move(DSSPrime.ActiveCircuit.UERegs[1], ResultPtr^, DSSPrime.ActiveCircuit.NumUERegs * SizeOf(Integer));
end;

procedure Settings_Get_UEregs_GR(); CDECL;
// Same as Settings_Get_UEregs but uses global result (GR) pointers
begin
    Settings_Get_UEregs(DSSPrime.GR_DataPtr_PInteger, @DSSPrime.GR_Counts_PInteger[0])
end;

//------------------------------------------------------------------------------
function Settings_Get_UEweight(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.UEWeight
end;
//------------------------------------------------------------------------------
procedure Settings_Set_LossRegs(ValuePtr: PInteger; ValueCount: TAPISize); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    ReAllocMem(DSSPrime.ActiveCircuit.LossRegs, Sizeof(Integer) * ValueCount);
    Move(ValuePtr^, DSSPrime.ActiveCircuit.LossRegs[1], ValueCount * SizeOf(Integer));
    DSSPrime.ActiveCircuit.NumLossRegs := ValueCount;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_LossWeight(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.LossWeight := Value
end;
//------------------------------------------------------------------------------
procedure Settings_Set_Trapezoidal(Value: TAPIBoolean); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.TrapezoidalIntegration := Value
end;
//------------------------------------------------------------------------------
procedure Settings_Set_UEregs(ValuePtr: PInteger; ValueCount: TAPISize); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    ReAllocMem(DSSPrime.ActiveCircuit.UERegs, Sizeof(Integer) * ValueCount);
    Move(ValuePtr^, DSSPrime.ActiveCircuit.UERegs[1], ValueCount * SizeOf(Integer));
    DSSPrime.ActiveCircuit.NumUEregs := ValueCount;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_UEweight(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.UEWeight := Value
end;
//------------------------------------------------------------------------------
function Settings_Get_ControlTrace(): TAPIBoolean; CDECL;
begin
    Result := FALSE;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.ControlQueue.TraceLog;
end;
//------------------------------------------------------------------------------
procedure Settings_Get_VoltageBases(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    i, Count: Integer;
begin
    if InvalidCircuit(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    with DSSPrime.ActiveCircuit do
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
    Settings_Get_VoltageBases(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Settings_Set_ControlTrace(Value: TAPIBoolean); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.ControlQueue.TraceLog := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_VoltageBases(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
        
    {LegalVoltageBases is a zero-terminated array, so we have to allocate
     one more than the number of actual values}
    with DSSPrime.ActiveCircuit do
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
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.ActiveCircuit.PriceCurve)
end;
//------------------------------------------------------------------------------
function Settings_Get_PriceSignal(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Pricesignal
end;
//------------------------------------------------------------------------------
procedure Settings_Set_PriceCurve(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    with DSSPrime.ActiveCircuit do
    begin
        PriceCurve := Value;
        PriceCurveObj := DSSPrime.LoadShapeClass.Find(Pricecurve);
        if PriceCurveObj = NIL then
            DoSimpleMsg(DSSPrime, 'Price Curve: "%s" not found.', [PriceCurve], 5006);
    end;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_PriceSignal(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.PriceSignal := Value;
end;
//------------------------------------------------------------------------------
function Settings_Get_LoadsTerminalCheck(): TAPIBoolean; CDECL;
begin
    Result := DSS_CAPI_LOADS_TERMINAL_CHECK;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_LoadsTerminalCheck(Value: TAPIBoolean); CDECL;
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
