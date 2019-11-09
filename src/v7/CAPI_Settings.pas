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
    ExecHelper,
    DSSClass,
    DSSHelper;

function Settings_Get_AllowDuplicates(): Boolean; CDECL;
begin

    if DSSPrime.ActiveCircuit <> NIL then

        Result := DSSPrime.ActiveCircuit.DuplicatesAllowed

    else
        Result := FALSE;

end;
//------------------------------------------------------------------------------
function Settings_Get_AutoBusList_AnsiString(): Ansistring; inline;
var
    i: Integer;
begin
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        GlobalResult := '';
        with DSSPrime.ActiveCircuit.AutoAddBusList do
        begin
            for i := 1 to ListSize do
                AppendGlobalResult(Get(i));
            Result := DSSPrime.GlobalResult;
        end
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

    if DSSPrime.ActiveCircuit <> NIL then
    begin

        if DSSPrime.ActiveCircuit.PositiveSequence then
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
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.ActiveCircuit.EmergMaxVolts
    else
        Result := 0.0;
    ;

end;
//------------------------------------------------------------------------------
function Settings_Get_EmergVminpu(): Double; CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.ActiveCircuit.EmergMinVolts
    else
        Result := 0.0;
    ;

end;
//------------------------------------------------------------------------------
function Settings_Get_NormVmaxpu(): Double; CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.ActiveCircuit.NormalMaxVolts
    else
        Result := 0.0;
    ;


end;
//------------------------------------------------------------------------------
function Settings_Get_NormVminpu(): Double; CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.ActiveCircuit.NormalMinVolts
    else
        Result := 0.0;
    ;
end;
//------------------------------------------------------------------------------
function Settings_Get_ZoneLock(): Boolean; CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        Result := DSSPrime.ActiveCircuit.ZonesLocked;
    end
    else
        Result := FALSE;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_AllocationFactors(Value: Double); CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        DoSetAllocationFactors(Value);
end;
//------------------------------------------------------------------------------
procedure Settings_Set_AllowDuplicates(Value: Boolean); CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        DSSPrime.ActiveCircuit.DuplicatesAllowed := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_AutoBusList(const Value: PAnsiChar); CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        DoAutoAddBusList(Value);
end;
//------------------------------------------------------------------------------
procedure Settings_Set_CktModel(Value: Integer); CDECL;
begin

    if DSSPrime.ActiveCircuit <> NIL then

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
    if DSSPrime.ActiveCircuit <> NIL then
        DSSPrime.ActiveCircuit.EmergMaxVolts := Value;

end;
//------------------------------------------------------------------------------
procedure Settings_Set_EmergVminpu(Value: Double); CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        DSSPrime.ActiveCircuit.EmergMinVolts := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_NormVmaxpu(Value: Double); CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        DSSPrime.ActiveCircuit.NormalMaxVolts := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_NormVminpu(Value: Double); CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        DSSPrime.ActiveCircuit.NormalMinVolts := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_ZoneLock(Value: Boolean); CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        DSSPrime.ActiveCircuit.ZonesLocked := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Get_LossRegs(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
var
    Result: PIntegerArray;
    i: Integer;
begin
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (DSSPrime.ActiveCircuit.NumLossRegs - 1) + 1);
        for i := 0 to DSSPrime.ActiveCircuit.NumLossRegs - 1 do
        begin
            Result[i] := DSSPrime.ActiveCircuit.LossRegs^[i + 1]
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
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        Result := DSSPrime.ActiveCircuit.LossWeight;
    end
    else
        Result := 0.0;
end;
//------------------------------------------------------------------------------
function Settings_Get_Trapezoidal(): Boolean; CDECL;
begin

    if DSSPrime.ActiveCircuit <> NIL then
    begin
        Result := DSSPrime.ActiveCircuit.TrapezoidalIntegration;
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
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (DSSPrime.ActiveCircuit.NumUERegs - 1) + 1);
        for i := 0 to DSSPrime.ActiveCircuit.NumUERegs - 1 do
        begin
            Result[i] := DSSPrime.ActiveCircuit.UERegs^[i + 1]
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
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        Result := DSSPrime.ActiveCircuit.UEWeight
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
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        ReAllocMem(DSSPrime.ActiveCircuit.LossRegs, Sizeof(DSSPrime.ActiveCircuit.LossRegs^[1]) * (1 - (0) + (ValueCount - 1)));
        j := 1;
        for i := (0) to (ValueCount - 1) do
        begin
            DSSPrime.ActiveCircuit.LossRegs^[j] := Value[i];
            Inc(j);
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_LossWeight(Value: Double); CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        DSSPrime.ActiveCircuit.LossWeight := Value
    end;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_Trapezoidal(Value: Boolean); CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        DSSPrime.ActiveCircuit.TrapezoidalIntegration := Value
    end;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_UEregs(ValuePtr: PInteger; ValueCount: Integer); CDECL;
var
    Value: PIntegerArray;
    i, j: Integer;
begin
    Value := PIntegerArray(ValuePtr);
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        ReAllocMem(DSSPrime.ActiveCircuit.UERegs, Sizeof(DSSPrime.ActiveCircuit.UERegs^[1]) * (1 - (0) + (ValueCount - 1)));
        j := 1;
        for i := (0) to (ValueCount - 1) do
        begin
            DSSPrime.ActiveCircuit.UERegs^[j] := Value[i];
            Inc(j);
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_UEweight(Value: Double); CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        DSSPrime.ActiveCircuit.UEWeight := Value
    end;
end;
//------------------------------------------------------------------------------
function Settings_Get_ControlTrace(): Boolean; CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.ActiveCircuit.ControlQueue.TraceLog;
end;
//------------------------------------------------------------------------------
procedure Settings_Get_VoltageBases(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    i, Count: Integer;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        with DSSPrime.ActiveCircuit do
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
    if DSSPrime.ActiveCircuit <> NIL then
        DSSPrime.ActiveCircuit.ControlQueue.TraceLog := Value;
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

    with DSSPrime.ActiveCircuit do
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
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.ActiveCircuit.PriceCurve
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
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.ActiveCircuit.Pricesignal
    else
        Result := 0.0;

end;
//------------------------------------------------------------------------------
procedure Settings_Set_PriceCurve(const Value: PAnsiChar); CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        with DSSPrime.ActiveCircuit do
        begin
            PriceCurve := Value;
            PriceCurveObj := DSSPrime.LoadShapeClass.Find(Pricecurve);
            if PriceCurveObj = NIL then
                DoSimpleMsg('Price Curve: "' + Pricecurve + '" not found.', 5006);
        end;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_PriceSignal(Value: Double); CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        DSSPrime.ActiveCircuit.PriceSignal := Value;
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
