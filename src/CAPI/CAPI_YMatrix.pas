// Created by PMeira based on DDLL/DYMatrix.pas, revision 2091
unit CAPI_YMatrix;

interface

uses
    UComplex,
    Solution,
    CAPI_Utils;

procedure YMatrix_GetCompressedYMatrix(factor: TAPIBoolean; var nBus, nNz: Longword; var ColPtr, RowIdxPtr: pInteger; var cValsPtr: PDouble); CDECL;

procedure YMatrix_ZeroInjCurr; CDECL;
procedure YMatrix_GetSourceInjCurrents; CDECL;
procedure YMatrix_GetPCInjCurr; CDECL;
procedure YMatrix_BuildYMatrixD(BuildOps, AllocateVI: Longint); CDECL;
procedure YMatrix_AddInAuxCurrents(SType: Integer); CDECL;
procedure YMatrix_getIpointer(var IvectorPtr: pNodeVarray); CDECL;
procedure YMatrix_getVpointer(var VvectorPtr: pNodeVarray); CDECL;
function YMatrix_SolveSystem(var NodeV: pNodeVarray): Integer; CDECL;

procedure YMatrix_Set_SystemYChanged(arg: TAPIBoolean); CDECL;
function YMatrix_Get_SystemYChanged(): TAPIBoolean; CDECL;
procedure YMatrix_Set_UseAuxCurrents(arg: TAPIBoolean); CDECL;
function YMatrix_Get_UseAuxCurrents(): TAPIBoolean; CDECL;

procedure YMatrix_Set_LoadsNeedUpdating(arg: TAPIBoolean); CDECL;
function YMatrix_Get_LoadsNeedUpdating(): TAPIBoolean; CDECL;
function YMatrix_CheckConvergence(): TAPIBoolean; CDECL;
procedure YMatrix_Set_Iteration(Value: Integer); CDECL;
function YMatrix_Get_Iteration(): Integer; CDECL;
procedure YMatrix_Set_SolutionInitialized(arg: TAPIBoolean); CDECL;
function YMatrix_Get_SolutionInitialized(): TAPIBoolean; CDECL;
procedure YMatrix_SetGeneratordQdV(); CDECL;
function YMatrix_Get_Handle(): NativeUInt; CDECL;
procedure YMatrix_Set_SolverOptions(opts: UInt64); CDECL;
function YMatrix_Get_SolverOptions():UInt64; CDECL;

implementation

uses
    DSSGlobals,
    Ymatrix,
    KLUSolve,
    DSSClass,
    DSSHelper;

procedure YMatrix_GetCompressedYMatrix(factor: TAPIBoolean; var nBus, nNz: Longword; var ColPtr, RowIdxPtr: pInteger; var cValsPtr: PDouble); CDECL;
{Returns Pointers to column and row and matrix values}
var
    Yhandle: NativeUInt;
    NumNZ, NumBuses: Longword;
    tmpCnt: array[0..1] of TAPISize;
begin
    if MissingSolution(DSSPrime) then
        Exit;
    Yhandle := DSSPrime.ActiveCircuit.Solution.hY;
    if Yhandle <= 0 then
    begin
        DoSimpleMsg(DSSPrime, 'Y Matrix not Built.', 222);
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
        NumNZ, 
        PLongWord(@ColPtr[0]),
        PLongWord(@RowIdxPtr[0]),
        pComplex(cValsPtr)
    );
end;

procedure YMatrix_ZeroInjCurr; CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.ZeroInjCurr;
end;

procedure YMatrix_GetSourceInjCurrents; CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.GetSourceInjCurrents;
end;

procedure YMatrix_GetPCInjCurr; CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.GetPCInjCurr;
end;

procedure YMatrix_Set_SystemYChanged(arg: TAPIBoolean); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.SystemYChanged := arg;
end;

function YMatrix_Get_SystemYChanged(): TAPIBoolean; CDECL;
begin
    Result := FALSE;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.SystemYChanged;
end;

procedure YMatrix_BuildYMatrixD(BuildOps, AllocateVI: Longint); CDECL;
var
    AllocateV: Boolean;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    AllocateV := (AllocateVI <> 0);
    BuildYMatrix(DSSPrime, BuildOps, AllocateV);
end;

procedure YMatrix_Set_UseAuxCurrents(arg: TAPIBoolean); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.UseAuxCurrents := arg;
end;

function YMatrix_Get_UseAuxCurrents(): TAPIBoolean; CDECL;
begin
    Result := FALSE;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.UseAuxCurrents;
end;

procedure YMatrix_AddInAuxCurrents(SType: Integer); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.AddInAuxCurrents(SType);
end;

procedure YMatrix_getIpointer(var IvectorPtr: pNodeVarray); CDECL;
begin
    if MissingSolution(DSSPrime) then
        Exit;
    IVectorPtr := DSSPrime.ActiveCircuit.Solution.Currents;
end;

procedure YMatrix_getVpointer(var VvectorPtr: pNodeVarray); CDECL;
begin
    if MissingSolution(DSSPrime) then
        Exit;
    VVectorPtr := DSSPrime.ActiveCircuit.Solution.NodeV;
end;

function YMatrix_SolveSystem(var NodeV: pNodeVarray): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    if (@NodeV <> NIL) then
        Result := DSSPrime.ActiveCircuit.Solution.SolveSystem(NodeV)
    else
        Result := DSSPrime.ActiveCircuit.Solution.SolveSystem(DSSPrime.ActiveCircuit.Solution.NodeV);
end;

procedure YMatrix_Set_LoadsNeedUpdating(arg: TAPIBoolean); CDECL;
begin
   DSSPrime.ActiveCircuit.Solution.LoadsNeedUpdating := arg;
end;

function YMatrix_Get_LoadsNeedUpdating: TAPIBoolean; CDECL;
begin
    Result := DSSPrime.ActiveCircuit.Solution.LoadsNeedUpdating;
end;

procedure YMatrix_Set_SolutionInitialized(arg: TAPIBoolean); CDECL;
begin
   DSSPrime.ActiveCircuit.Solution.SolutionInitialized := arg;
end;

function YMatrix_Get_SolutionInitialized: TAPIBoolean; CDECL;
begin
    Result := DSSPrime.ActiveCircuit.Solution.SolutionInitialized;
end;


function YMatrix_CheckConvergence: TAPIBoolean; CDECL;
begin
    Result := DSSPrime.ActiveCircuit.Solution.Converged;
end;

procedure YMatrix_Set_Iteration(Value: Integer); CDECL;
begin
    if InvalidCircuit(DSSPrime) then Exit;
   DSSPrime.ActiveCircuit.Solution.Iteration := Value;
end;

function YMatrix_Get_Iteration: Integer; CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.ActiveCircuit.Solution.Iteration
    else
        Result := -1;
end;

procedure YMatrix_SetGeneratordQdV; CDECL;
begin
    if InvalidCircuit(DSSPrime) then Exit;
    try
       DSSPrime.ActiveCircuit.Solution.SetGeneratordQdV;  // Set dQdV for Model 3 generators
    except
        ON E: EEsolv32Problem do
        begin
            DoSimpleMsg(DSSPrime, 'From DoPFLOWsolution.SetGeneratordQdV: ' + CRLF + E.Message + CheckYMatrixforZeroes(DSSPrime), 7073);
        end;
    end;
end;

function YMatrix_Get_Handle: NativeUInt; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then Exit;
    Result := DSSPrime.ActiveCircuit.Solution.hY
end;

procedure YMatrix_Set_SolverOptions(opts: UInt64); CDECL;
begin
    if InvalidCircuit(DSSPrime) then Exit;
    with DSSPrime.ActiveCircuit.Solution do
    begin
        SolverOptions := opts;
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
        if hY <> 0 then
            KLUSolve.SetOptions(hY, SolverOptions and $FFFFFFFF);
{$ELSE}
    DoSimpleMsg('This version of DSS C-API was not compiled with extended solver options.', 7074);
{$ENDIF}
    end;
end;

function YMatrix_Get_SolverOptions:UInt64; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then Exit;
    Result := DSSPrime.ActiveCircuit.Solution.SolverOptions;
end;

//---------------------------------------------------------------------------------
end.
