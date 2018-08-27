// Created by PMeira based on DDLL/DYMatrix.pas, revision 2091
// Note: YMatrix_GetCompressedYMatrix is different from the original since 
// I wanted to export the original, non-factorized YMatrix in CSC form too.
unit CAPI_YMatrix;

interface

Uses Arraydef, UComplex, Solution;

Procedure YMatrix_GetCompressedYMatrix(factor: wordbool; var nBus, nNz:Longword; var ColPtr, RowIdxPtr:pInteger; var cValsPtr: PDouble); cdecl;

procedure YMatrix_ZeroInjCurr; cdecl;
procedure YMatrix_GetSourceInjCurrents; cdecl;
procedure YMatrix_GetPCInjCurr; cdecl;
procedure YMatrix_BuildYMatrixD(BuildOps, AllocateVI: longint); cdecl;
procedure YMatrix_AddInAuxCurrents(SType: integer); cdecl;
procedure YMatrix_getIpointer(var IvectorPtr: pNodeVarray);cdecl;
procedure YMatrix_getVpointer(var VvectorPtr: pNodeVarray);cdecl;
function YMatrix_SolveSystem(var NodeV:pNodeVarray): integer; cdecl;

procedure YMatrix_Set_SystemYChanged(arg: WordBool); cdecl;
function YMatrix_Get_SystemYChanged(): WordBool; cdecl;
procedure YMatrix_Set_UseAuxCurrents(arg: WordBool); cdecl;
function YMatrix_Get_UseAuxCurrents(): WordBool; cdecl;


implementation

Uses DSSGlobals, Ymatrix, KLUSolve, CAPI_Utils;

Procedure YMatrix_GetCompressedYMatrix(factor: wordbool; var nBus, nNz:Longword; Var ColPtr, RowIdxPtr:pInteger; Var cValsPtr: PDouble); cdecl;
{Returns Pointers to column and row and matrix values}
Var 
    Yhandle: NativeUInt;
    NumNZ, NumBuses: LongWord;
    YColumns, YRows: pIntegerArray;
  
    //tmpColPtrN, tmpRowIdxPtrN, tmpValsPtrN: PInteger;
    tmpCnt: array[0..1] of integer;
Begin
    If ActiveCircuit[ActiveActor]=Nil then Exit;
    Yhandle := ActiveCircuit[ActiveActor].Solution.hY;
    If Yhandle <= 0 Then 
    Begin
        DoSimpleMsg('Y Matrix not Built.', 222);
        Exit;
    End;
     
    if factor then FactorSparseMatrix(Yhandle);

    GetNNZ(Yhandle, @NumNz);
    GetSize(Yhandle, @NumBuses);

    YColumns := Arraydef.PIntegerArray(DSS_CreateArray_PInteger(ColPtr, @tmpCnt[0], NumBuses + 1));
    YRows := Arraydef.PIntegerArray(DSS_CreateArray_PInteger(RowIdxPtr, @tmpCnt[0], NumNZ));
    DSS_CreateArray_PDouble(cValsPtr, @tmpCnt[0], 2 * NumNZ);
    
    nBus := NumBuses;
    nNZ  := NumNZ;
    
    // Fill in the memory
    GetCompressedMatrix(
        Yhandle, 
        NumBuses + 1, 
        NumNZ, 
        @ColPtr[0], 
        @RowIdxPtr[0], 
        pComplex(cValsPtr)
    );
    
End;

procedure YMatrix_Set_SystemYChanged(arg: WordBool); cdecl;
begin
    ActiveCircuit[ActiveActor].Solution.SystemYChanged := arg;
end;

function YMatrix_Get_SystemYChanged(): WordBool; cdecl;
begin
    Result := ActiveCircuit[ActiveActor].Solution.SystemYChanged;
end;

procedure YMatrix_ZeroInjCurr; cdecl;
Begin
    IF (ActiveCircuit[ActiveActor] <> Nil) THEN 
        ActiveCircuit[ActiveActor].Solution.ZeroInjCurr(ActiveActor);
end;

procedure YMatrix_GetSourceInjCurrents; cdecl;
Begin
    IF (ActiveCircuit[ActiveActor] <> Nil) THEN 
        ActiveCircuit[ActiveActor].Solution.GetSourceInjCurrents(ActiveActor);
end;

procedure YMatrix_GetPCInjCurr; cdecl;
Begin
    IF (ActiveCircuit[ActiveActor] <> Nil) THEN 
        ActiveCircuit[ActiveActor].Solution.GetPCInjCurr(ActiveActor);
end;

procedure YMatrix_BuildYMatrixD(BuildOps, AllocateVI: longint); cdecl;
var
    AllocateV: boolean;
begin
    AllocateV := (AllocateVI <> 0);
    BuildYMatrix(BuildOps, AllocateV, ActiveActor);
end;

procedure YMatrix_Set_UseAuxCurrents(arg: WordBool); cdecl;
begin
    ActiveCircuit[ActiveActor].Solution.UseAuxCurrents := arg;
end;

function YMatrix_Get_UseAuxCurrents(): WordBool; cdecl;
begin
    Result := ActiveCircuit[ActiveActor].Solution.UseAuxCurrents;
end;

procedure YMatrix_AddInAuxCurrents(SType: integer); cdecl;
begin
    ActiveCircuit[ActiveActor].Solution.AddInAuxCurrents(SType, ActiveActor);
end;

procedure YMatrix_getIpointer(var IvectorPtr: pNodeVarray);cdecl;
begin
    IVectorPtr := ActiveCircuit[ActiveActor].Solution.Currents;
end;

procedure YMatrix_getVpointer(var VvectorPtr: pNodeVarray);cdecl;
begin
    VVectorPtr := ActiveCircuit[ActiveActor].Solution.NodeV;
end;

function YMatrix_SolveSystem(var NodeV: pNodeVarray): integer; cdecl;
begin
    Result := ActiveCircuit[ActiveActor].Solution.SolveSystem(NodeV, ActiveActor);
end;

//---------------------------------------------------------------------------------
end.
