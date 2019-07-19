unit CAPI_Settings;

{$inline on}

interface

uses
    CAPI_Utils;

function Settings_Get_AllowDuplicates(): Boolean; CDECL;
function Settings_Get_AutoBusList(): PAnsiChar; CDECL;
function Settings_Get_CktModel(): Integer; CDECL;
function Settings_Get_EmergVmaxpu(): Double; CDECL;
function Settings_Get_EmergVminpu(): Double; CDECL;
function Settings_Get_NormVmaxpu(): Double; CDECL;
function Settings_Get_NormVminpu(): Double; CDECL;
function Settings_Get_ZoneLock(): Boolean; CDECL;
procedure Settings_Set_AllocationFactors(Value: Double); CDECL;
procedure Settings_Set_AllowDuplicates(Value: Boolean); CDECL;
procedure Settings_Set_AutoBusList(const Value: PAnsiChar); CDECL;
procedure Settings_Set_CktModel(Value: Integer); CDECL;
procedure Settings_Set_EmergVmaxpu(Value: Double); CDECL;
procedure Settings_Set_EmergVminpu(Value: Double); CDECL;
procedure Settings_Set_NormVmaxpu(Value: Double); CDECL;
procedure Settings_Set_NormVminpu(Value: Double); CDECL;
procedure Settings_Set_ZoneLock(Value: Boolean); CDECL;
procedure Settings_Get_LossRegs(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
procedure Settings_Get_LossRegs_GR(); CDECL;
function Settings_Get_LossWeight(): Double; CDECL;
function Settings_Get_Trapezoidal(): Boolean; CDECL;
procedure Settings_Get_UEregs(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
procedure Settings_Get_UEregs_GR(); CDECL;
function Settings_Get_UEweight(): Double; CDECL;
procedure Settings_Set_LossRegs(ValuePtr: PInteger; ValueCount: Integer); CDECL;
procedure Settings_Set_LossWeight(Value: Double); CDECL;
procedure Settings_Set_Trapezoidal(Value: Boolean); CDECL;
procedure Settings_Set_UEregs(ValuePtr: PInteger; ValueCount: Integer); CDECL;
procedure Settings_Set_UEweight(Value: Double); CDECL;
function Settings_Get_ControlTrace(): Boolean; CDECL;
procedure Settings_Get_VoltageBases(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Settings_Get_VoltageBases_GR(); CDECL;
procedure Settings_Set_ControlTrace(Value: Boolean); CDECL;
procedure Settings_Set_VoltageBases(ValuePtr: PDouble; ValueCount: Integer); CDECL;
function Settings_Get_PriceCurve(): PAnsiChar; CDECL;
function Settings_Get_PriceSignal(): Double; CDECL;
procedure Settings_Set_PriceCurve(const Value: PAnsiChar); CDECL;
procedure Settings_Set_PriceSignal(Value: Double); CDECL;

// API extensions
function Settings_Get_LoadsTerminalCheck(): Boolean; CDECL;
procedure Settings_Set_LoadsTerminalCheck(Value: Boolean); CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    ExecHelper;

function Settings_Get_AllowDuplicates(): Boolean; CDECL;
begin

    if ActiveCircuit <> NIL then

        Result := ActiveCircuit.DuplicatesAllowed

    else
        Result := FALSE;

end;
//------------------------------------------------------------------------------
function Settings_Get_AutoBusList_AnsiString(): Ansistring; inline;
var
    i: Integer;
begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit.AutoAddBusList do
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

    if ActiveCircuit <> NIL then
    begin

        if ActiveCircuit.PositiveSequence then
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
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.EmergMaxVolts
    else
        Result := 0.0;
    ;

end;
//------------------------------------------------------------------------------
function Settings_Get_EmergVminpu(): Double; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.EmergMinVolts
    else
        Result := 0.0;
    ;

end;
//------------------------------------------------------------------------------
function Settings_Get_NormVmaxpu(): Double; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.NormalMaxVolts
    else
        Result := 0.0;
    ;


end;
//------------------------------------------------------------------------------
function Settings_Get_NormVminpu(): Double; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.NormalMinVolts
    else
        Result := 0.0;
    ;
end;
//------------------------------------------------------------------------------
function Settings_Get_ZoneLock(): Boolean; CDECL;
begin
    if ActiveCircuit <> NIL then
    begin
        Result := ActiveCircuit.ZonesLocked;
    end
    else
        Result := FALSE;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_AllocationFactors(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
        DoSetAllocationFactors(Value);
end;
//------------------------------------------------------------------------------
procedure Settings_Set_AllowDuplicates(Value: Boolean); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.DuplicatesAllowed := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_AutoBusList(const Value: PAnsiChar); CDECL;
begin
    if ActiveCircuit <> NIL then
        DoAutoAddBusList(Value);
end;
//------------------------------------------------------------------------------
procedure Settings_Set_CktModel(Value: Integer); CDECL;
begin

    if ActiveCircuit <> NIL then

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
    if ActiveCircuit <> NIL then
        ActiveCircuit.EmergMaxVolts := Value;

end;
//------------------------------------------------------------------------------
procedure Settings_Set_EmergVminpu(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.EmergMinVolts := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_NormVmaxpu(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.NormalMaxVolts := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_NormVminpu(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.NormalMinVolts := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_ZoneLock(Value: Boolean); CDECL;
begin
    if Activecircuit <> NIL then
        ActiveCircuit.ZonesLocked := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Get_LossRegs(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
var
    Result: PIntegerArray;
    i: Integer;
begin
    if ActiveCircuit <> NIL then
    begin
        Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (ActiveCircuit.NumLossRegs - 1) + 1);
        for i := 0 to ActiveCircuit.NumLossRegs - 1 do
        begin
            Result[i] := ActiveCircuit.LossRegs^[i + 1]
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
    if ActiveCircuit <> NIL then
    begin
        Result := ActiveCircuit.LossWeight;
    end
    else
        Result := 0.0;
end;
//------------------------------------------------------------------------------
function Settings_Get_Trapezoidal(): Boolean; CDECL;
begin

    if ActiveCircuit <> NIL then
    begin
        Result := ActiveCircuit.TrapezoidalIntegration;
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
    if ActiveCircuit <> NIL then
    begin
        Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (ActiveCircuit.NumUERegs - 1) + 1);
        for i := 0 to ActiveCircuit.NumUERegs - 1 do
        begin
            Result[i] := ActiveCircuit.UERegs^[i + 1]
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
    if ActiveCircuit <> NIL then
    begin
        Result := ActiveCircuit.UEWeight
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
    if ActiveCircuit <> NIL then
    begin
        ReAllocMem(ActiveCircuit.LossRegs, Sizeof(ActiveCircuit.LossRegs^[1]) * (1 - (0) + (ValueCount - 1)));
        j := 1;
        for i := (0) to (ValueCount - 1) do
        begin
            ActiveCircuit.LossRegs^[j] := Value[i];
            Inc(j);
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_LossWeight(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
    begin
        ActiveCircuit.LossWeight := Value
    end;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_Trapezoidal(Value: Boolean); CDECL;
begin
    if ActiveCircuit <> NIL then
    begin
        ActiveCircuit.TrapezoidalIntegration := Value
    end;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_UEregs(ValuePtr: PInteger; ValueCount: Integer); CDECL;
var
    Value: PIntegerArray;
    i, j: Integer;
begin
    Value := PIntegerArray(ValuePtr);
    if ActiveCircuit <> NIL then
    begin
        ReAllocMem(ActiveCircuit.UERegs, Sizeof(ActiveCircuit.UERegs^[1]) * (1 - (0) + (ValueCount - 1)));
        j := 1;
        for i := (0) to (ValueCount - 1) do
        begin
            ActiveCircuit.UERegs^[j] := Value[i];
            Inc(j);
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_UEweight(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
    begin
        ActiveCircuit.UEWeight := Value
    end;
end;
//------------------------------------------------------------------------------
function Settings_Get_ControlTrace(): Boolean; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.ControlQueue.TraceLog;
end;
//------------------------------------------------------------------------------
procedure Settings_Get_VoltageBases(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    i, Count: Integer;
begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
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
procedure Settings_Set_ControlTrace(Value: Boolean); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.ControlQueue.TraceLog := Value;
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

    with ActiveCircuit do
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
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.PriceCurve
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
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Pricesignal
    else
        Result := 0.0;

end;
//------------------------------------------------------------------------------
procedure Settings_Set_PriceCurve(const Value: PAnsiChar); CDECL;
begin
    if ActiveCircuit <> NIL then
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
    if ActiveCircuit <> NIL then
        ActiveCircuit.PriceSignal := Value;
end;
//------------------------------------------------------------------------------
function Settings_Get_LoadsTerminalCheck(): Boolean; CDECL;
begin
    Result := DSS_CAPI_LOADS_TERMINAL_CHECK;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_LoadsTerminalCheck(Value: Boolean); CDECL;
begin
    DSS_CAPI_LOADS_TERMINAL_CHECK := Value;
end;
//------------------------------------------------------------------------------
end.
