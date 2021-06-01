unit CAPI_DSSimComs;

interface

uses
    CAPI_Utils,
    CAPI_Types,
    UComplex;

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
    DSSClass,
    DSSHelper;

procedure DSSimComs_BusVoltagepu(var ResultPtr: PDouble; ResultCount: PAPISize; Index: PtrUInt); CDECL;
var
    Result: PDoubleArray0;
    i, j: Integer;
    Volts, BaseFactor: Double;
begin
    if InvalidCircuit(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    with DSSPrime.ActiveCircuit do
    begin
        i := Index;
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, Buses^[i].NumNodesThisBus);
        if Buses^[i].kVBase > 0.0 then
            BaseFactor := 1000.0 * Buses^[i].kVBase
        else
            BaseFactor := 1.0;
        for j := 1 to Buses^[i].NumNodesThisBus do
        begin
            Volts := Cabs(DSSPrime.ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(j)]);
            Result[j - 1] := Volts / BaseFactor;
        end;
    end
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
    i, j, k: Integer;
    Volts: Complex;
begin
    if InvalidCircuit(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
        
    with DSSPrime.ActiveCircuit do
    begin
        i := Index;
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * Buses^[i].NumNodesThisBus);
        for j := 1 to Buses^[i].NumNodesThisBus do
        begin
            Volts := DSSPrime.ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(j)];
            k := (j - 1) * 2;
            Result[k] := Volts.re;
            Result[k + 1] := Volts.im;
        end;
    end
end;

procedure DSSimComs_BusVoltage_GR(Index: PtrUInt); CDECL;
// Same as DSSimComs_BusVoltage but uses global result (GR) pointers
begin
    DSSimComs_BusVoltage(DSSPrime.GR_DataPtr_PDouble, DSSPrime.GR_Counts_PDouble, Index)
end;

//------------------------------------------------------------------------------
end.
