unit CAPI_DSSimComs;

interface

uses
    CAPI_Utils,
    CAPI_Types,
    UComplex, DSSUcomplex;

procedure DSSimComs_BusVoltagepu(var ResultPtr: PDouble; ResultCount: PAPISize; Index: PtrUInt); CDECL;
procedure DSSimComs_BusVoltagepu_GR(Index: PtrUInt); CDECL;
procedure DSSimComs_BusVoltage(var ResultPtr: PDouble; ResultCount: PAPISize; Index: PtrUInt); CDECL;
procedure DSSimComs_BusVoltage_GR(Index: PtrUInt); CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    Executive,
    SysUtils,
    solution,
    CktElement,
    Bus,
    DSSClass,
    DSSHelper;

procedure DSSimComs_BusVoltagepu(var ResultPtr: PDouble; ResultCount: PAPISize; Index: PtrUInt); CDECL;
var
    Result: PDoubleArray0;
    j: Integer;
    Volts, BaseFactor: Double;
    bus: TDSSBus;
begin
    if InvalidCircuit(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    
    if (not ((Index > 0) and (Index <= DSSPrime.ActiveCircuit.NumBuses))) or (DSSPrime.ActiveCircuit.Buses = NIL) then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSSPrime, _('Invalid bus index.'), 8989);
        end;
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    bus := DSSPrime.ActiveCircuit.Buses[Index];
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, bus.NumNodesThisBus);
    if bus.kVBase > 0.0 then
        BaseFactor := 1000.0 * bus.kVBase
    else
        BaseFactor := 1.0;
    for j := 1 to bus.NumNodesThisBus do
    begin
        Volts := Cabs(DSSPrime.ActiveCircuit.Solution.NodeV[bus.GetRef(j)]);
        Result[j - 1] := Volts / BaseFactor;
    end;
end;

procedure DSSimComs_BusVoltagepu_GR(Index: PtrUInt); CDECL;
// Same as DSSimComs_BusVoltagepu but uses global result (GR) pointers
begin
    DSSimComs_BusVoltagepu(DSSPrime.GR_DataPtr_PDouble, DSSPrime.GR_Counts_PDouble, Index)
end;

//------------------------------------------------------------------------------
procedure DSSimComs_BusVoltage(var ResultPtr: PDouble; ResultCount: PAPISize; Index: PtrUInt); CDECL;
var
    Result: PDoubleArray0;
    j, k: Integer;
    Volts: Complex;
    bus: TDSSBus;
begin
    if InvalidCircuit(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    
    if (not ((Index > 0) and (Index <= DSSPrime.ActiveCircuit.NumBuses))) or (DSSPrime.ActiveCircuit.Buses = NIL) then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSSPrime, _('Invalid bus index.'), 8989);
        end;
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    bus := DSSPrime.ActiveCircuit.Buses[Index];
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * bus.NumNodesThisBus);
    for j := 1 to bus.NumNodesThisBus do
    begin
        Volts := DSSPrime.ActiveCircuit.Solution.NodeV[bus.GetRef(j)];
        k := (j - 1) * 2;
        Result[k] := Volts.re;
        Result[k + 1] := Volts.im;
    end;
end;

procedure DSSimComs_BusVoltage_GR(Index: PtrUInt); CDECL;
// Same as DSSimComs_BusVoltage but uses global result (GR) pointers
begin
    DSSimComs_BusVoltage(DSSPrime.GR_DataPtr_PDouble, DSSPrime.GR_Counts_PDouble, Index)
end;

//------------------------------------------------------------------------------
end.
