// Created by PMeira based on DDLL/DYMatrix.pas, revision 2091
// Note: YMatrix_GetCompressedYMatrix is different from the original since 
// I wanted to export the original, non-factorized YMatrix in CSC form too.
unit CAPI_YMatrix;

interface

uses
    UComplex,
    Solution;

procedure YMatrix_GetCompressedYMatrix(factor: Boolean; var nBus, nNz: Longword; var ColPtr, RowIdxPtr: pInteger; var cValsPtr: PDouble); CDECL;

procedure YMatrix_ZeroInjCurr; CDECL;
procedure YMatrix_GetSourceInjCurrents; CDECL;
procedure YMatrix_GetPCInjCurr; CDECL;
procedure YMatrix_BuildYMatrixD(BuildOps, AllocateVI: Longint); CDECL;
procedure YMatrix_AddInAuxCurrents(SType: Integer); CDECL;
procedure YMatrix_getIpointer(var IvectorPtr: pNodeVarray); CDECL;
procedure YMatrix_getVpointer(var VvectorPtr: pNodeVarray); CDECL;
function YMatrix_SolveSystem(var NodeV: pNodeVarray): Integer; CDECL;

procedure YMatrix_Set_SystemYChanged(arg: Boolean); CDECL;
function YMatrix_Get_SystemYChanged(): Boolean; CDECL;
procedure YMatrix_Set_UseAuxCurrents(arg: Boolean); CDECL;
function YMatrix_Get_UseAuxCurrents(): Boolean; CDECL;


implementation

uses
    DSSGlobals,
    Ymatrix,
    KLUSolve,
    CAPI_Utils;

procedure YMatrix_GetCompressedYMatrix(factor: Boolean; var nBus, nNz: Longword; var ColPtr, RowIdxPtr: pInteger; var cValsPtr: PDouble); CDECL;
{Returns Pointers to column and row and matrix values}
var
    Yhandle: NativeUInt;
    NumNZ, NumBuses: Longword;
    tmpCnt: array[0..1] of Integer;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    Yhandle := ActiveCircuit[ActiveActor].Solution.hY;
    if Yhandle <= 0 then
    begin
        DoSimpleMsg('Y Matrix not Built.', 222);
        Exit;
    end;

    if factor then
        FactorSparseMatrix(Yhandle);

    GetNNZ(Yhandle, @NumNz);
    GetSize(Yhandle, @NumBuses);

    DSS_CreateArray_PInteger(ColPtr, @tmpCnt[0], NumBuses + 1);
    DSS_CreateArray_PInteger(RowIdxPtr, @tmpCnt[0], NumNZ);
    DSS_CreateArray_PDouble(cValsPtr, @tmpCnt[0], 2 * NumNZ);

    nBus := NumBuses;
    nNZ := NumNZ;

    // Fill in the memory
    GetCompressedMatrix(
        Yhandle,
        NumBuses + 1,
        NumNZ, @ColPtr[0], @RowIdxPtr[0],
        pComplex(cValsPtr)
    );
end;

procedure YMatrix_ZeroInjCurr; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].Solution.ZeroInjCurr(ActiveActor);
end;

procedure YMatrix_GetSourceInjCurrents; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].Solution.GetSourceInjCurrents(ActiveActor);
end;

procedure YMatrix_GetPCInjCurr; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].Solution.GetPCInjCurr(ActiveActor);
end;

procedure YMatrix_Set_SystemYChanged(arg: Boolean); CDECL;
begin
    ActiveCircuit[ActiveActor].Solution.SystemYChanged := arg;
end;

function YMatrix_Get_SystemYChanged(): Boolean; CDECL;
begin
    Result := ActiveCircuit[ActiveActor].Solution.SystemYChanged;
end;

procedure YMatrix_BuildYMatrixD(BuildOps, AllocateVI: Longint); CDECL;
var
    AllocateV: Boolean;
begin
    AllocateV := (AllocateVI <> 0);
    BuildYMatrix(BuildOps, AllocateV, ActiveActor);
end;

procedure YMatrix_Set_UseAuxCurrents(arg: Boolean); CDECL;
begin
    ActiveCircuit[ActiveActor].Solution.UseAuxCurrents := arg;
end;

function YMatrix_Get_UseAuxCurrents(): Boolean; CDECL;
begin
    Result := ActiveCircuit[ActiveActor].Solution.UseAuxCurrents;
end;

procedure YMatrix_AddInAuxCurrents(SType: Integer); CDECL;
begin
    ActiveCircuit[ActiveActor].Solution.AddInAuxCurrents(SType, ActiveActor);
end;

procedure YMatrix_getIpointer(var IvectorPtr: pNodeVarray); CDECL;
begin
    IVectorPtr := ActiveCircuit[ActiveActor].Solution.Currents;
end;

procedure YMatrix_getVpointer(var VvectorPtr: pNodeVarray); CDECL;
begin
    VVectorPtr := ActiveCircuit[ActiveActor].Solution.NodeV;
end;

function YMatrix_SolveSystem(var NodeV: pNodeVarray): Integer; CDECL;
begin
    Result := ActiveCircuit[ActiveActor].Solution.SolveSystem(NodeV, ActiveActor);
end;

//---------------------------------------------------------------------------------
end.
