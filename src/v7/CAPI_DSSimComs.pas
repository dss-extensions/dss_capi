unit CAPI_DSSimComs;

{$inline on}

interface

uses
    CAPI_Utils,
    UComplex;

procedure DSSimComs_BusVoltagepu(var ResultPtr: PDouble; ResultCount: PInteger; Index: PtrUInt); CDECL;
procedure DSSimComs_BusVoltagepu_GR(Index: PtrUInt); CDECL;
procedure DSSimComs_BusVoltage(var ResultPtr: PDouble; ResultCount: PInteger; Index: PtrUInt); CDECL;
procedure DSSimComs_BusVoltage_GR(Index: PtrUInt); CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    Executive,
    SysUtils,
    solution,
    CktElement;

procedure DSSimComs_BusVoltagepu(var ResultPtr: PDouble; ResultCount: PInteger; Index: PtrUInt); CDECL;
var
    Result: PDoubleArray;
    i, j: Integer;
    Volts, BaseFactor: Double;
begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            i := Index;
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (Buses^[i].NumNodesThisBus - 1) + 1);
            if Buses^[i].kVBase > 0.0 then
                BaseFactor := 1000.0 * Buses^[i].kVBase
            else
                BaseFactor := 1.0;
            for j := 1 to Buses^[i].NumNodesThisBus do
            begin
                Volts := Cabs(ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(j)]);
                Result[j - 1] := Volts / BaseFactor;
            end;
        end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
end;

procedure DSSimComs_BusVoltagepu_GR(Index: PtrUInt); CDECL;
// Same as DSSimComs_BusVoltagepu but uses global result (GR) pointers
begin
    DSSimComs_BusVoltagepu(GR_DataPtr_PDouble, GR_CountPtr_PDouble, Index)
end;

//------------------------------------------------------------------------------
procedure DSSimComs_BusVoltage(var ResultPtr: PDouble; ResultCount: PInteger; Index: PtrUInt); CDECL;
var
    Result: PDoubleArray;
    i, j, k: Integer;
    Volts: Complex;
begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            i := Index;
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2 * Buses^[i].NumNodesThisBus - 1) + 1);
            for j := 1 to Buses^[i].NumNodesThisBus do
            begin
                Volts := ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(j)];
                k := (j - 1) * 2;
                Result[k] := Volts.re;
                Result[k + 1] := Volts.im;
            end;
        end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;

procedure DSSimComs_BusVoltage_GR(Index: PtrUInt); CDECL;
// Same as DSSimComs_BusVoltage but uses global result (GR) pointers
begin
    DSSimComs_BusVoltage(GR_DataPtr_PDouble, GR_CountPtr_PDouble, Index)
end;

//------------------------------------------------------------------------------
end.
