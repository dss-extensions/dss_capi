unit KLUSolve;


interface

uses
    uComplex,
    DSSGlobals,
    Windows,
    variants,
    sysutils;


{$IFDEF MSWindows}
const
    KLULib = 'klusolve.dll';

{
  procedure Create_KLU;
  procedure DestroyAll_KLU;

}

// in general, KLU arrays are 0-based
// function calls return 0 to indicate failure, 1 for success

// returns the non-zero handle of a new sparse matrix, if successful
// must call DeleteSparseSet on the valid handle when finished
function NewSparseSet(nBus: Longword): Nativeuint; STDCALL; EXTERNAL KLULib;
// return 1 for success, 0 for invalid handle
function DeleteSparseSet(id: Nativeuint): Longword; STDCALL; EXTERNAL KLULib;

// return 1 for success, 2 for singular, 0 for invalid handle
// factors matrix if needed
function SolveSparseSet(id: Nativeuint; x, b: pComplexArray): Longword; STDCALL; EXTERNAL KLULib;

// return 1 for success, 0 for invalid handle
function ZeroSparseSet(id: Nativeuint): Longword; STDCALL; EXTERNAL KLULib;

// return 1 for success, 2 for singular, 0 for invalid handle
// FactorSparseMatrix does no extra work if the factoring was done previously
function FactorSparseMatrix(id: Nativeuint): Longword; STDCALL; EXTERNAL KLULib;

// These "Get" functions for matrix information all return 1 for success, 0 for invalid handle
// Res is the matrix order (number of nodes)
function GetSize(id: Nativeuint; Res: pLongWord): Longword; STDCALL; EXTERNAL KLULib;

// the following function results are not known prior to factoring
// Res is the number of floating point operations to factor
function GetFlops(id: Nativeuint; Res: pDouble): Longword; STDCALL; EXTERNAL KLULib;
// Res is number of non-zero entries in the original matrix
function GetNNZ(id: Nativeuint; Res: pLongWord): Longword; STDCALL; EXTERNAL KLULib;
// Res is the number of non-zero entries in factored matrix
function GetSparseNNZ(id: Nativeuint; Res: pLongWord): Longword; STDCALL; EXTERNAL KLULib;
// Res is a column number corresponding to a singularity, or 0 if not singular
function GetSingularCol(id: Nativeuint; Res: pLongWord): Longword; STDCALL; EXTERNAL KLULib;
// Res is the pivot element growth factor
function GetRGrowth(id: Nativeuint; Res: pDouble): Longword; STDCALL; EXTERNAL KLULib;
// Res is aquick estimate of the reciprocal of condition number
function GetRCond(id: Nativeuint; Res: pDouble): Longword; STDCALL; EXTERNAL KLULib;
// Res is a more accurate estimate of condition number
function GetCondEst(id: Nativeuint; Res: pDouble): Longword; STDCALL; EXTERNAL KLULib;

// return 1 for success, 0 for invalid handle or a node number out of range
function AddPrimitiveMatrix(id: Nativeuint; nOrder: Longword; Nodes: pLongWord; Mat: pComplex): Longword; STDCALL; EXTERNAL KLULib;

// Action = 0 (close), 1 (rewrite) or 2 (append)
function SetLogFile(Path: Pchar; Action: Longword): Longword; STDCALL; EXTERNAL KLULib;

// fill sparse matrix in compressed column form
// return 1 for success, 0 for invalid handle, 2 for invalid array sizes
// pColP must be of length nColP == nBus + 1
// pRowIdx and pMat of length nNZ, which
//    must be at least the value returned by GetNNZ
function GetCompressedMatrix(id: Nativeuint; nColP, nNZ: Longword; pColP, pRowIdx: pLongWord; Mat: pComplex): Longword; STDCALL; EXTERNAL KLULib;

// fill sparse matrix in triplet form
// return 1 for success, 0 for invalid handle, 2 for invalid array sizes
// pRows, pCols, and Mat must all be of length nNZ
function GetTripletMatrix(id: Nativeuint; nNZ: Longword; pRows, pCols: pLongWord; Mat: pComplex): Longword; STDCALL; EXTERNAL KLULib;

// returns number of islands >= 1 by graph traversal
// pNodes contains the island number for each node
function FindIslands(id: Nativeuint; nOrder: Longword; pNodes: pLongWord): Longword; STDCALL; EXTERNAL KLULib;

// AddMatrixElement is deprecated, use AddPrimitiveMatrix instead
function AddMatrixElement(id: Nativeuint; i, j: Longword; Value: pComplex): Longword; STDCALL; EXTERNAL KLULib;

// GetMatrixElement is deprecated, use GetCompressedMatrix or GetTripletMatrix
function GetMatrixElement(id: Nativeuint; i, j: Longword; Value: pComplex): Longword; STDCALL; EXTERNAL KLULib;
{$ELSE}
const
    KLU_CALL = 'libklusolve.a';
    _PU = '';

function NewSparseSet(nBus: Longword): Nativeuint; CDECL; EXTERNAL KLU_CALL;

function DeleteSparseSet(id: Nativeuint): Longword; CDECL; EXTERNAL KLU_CALL;

function SolveSparseSet(id: Nativeuint; x, b: pComplexArray): Longword; CDECL; EXTERNAL KLU_CALL;

function ZeroSparseSet(id: Nativeuint): Longword; CDECL; EXTERNAL KLU_CALL;

function FactorSparseMatrix(id: Nativeuint): Longword; CDECL; EXTERNAL KLU_CALL;

function GetSize(id: Nativeuint; Res: pLongWord): Longword; CDECL; EXTERNAL KLU_CALL;

function GetFlops(id: Nativeuint; Res: pDouble): Longword; CDECL; EXTERNAL KLU_CALL;

function GetNNZ(id: Nativeuint; Res: pLongWord): Longword; CDECL; EXTERNAL KLU_CALL;

function GetSparseNNZ(id: Nativeuint; Res: pLongWord): Longword; CDECL; EXTERNAL KLU_CALL;

function GetSingularCol(id: Nativeuint; Res: pLongWord): Longword; CDECL; EXTERNAL KLU_CALL;

function GetRGrowth(id: Nativeuint; Res: pDouble): Longword; CDECL; EXTERNAL KLU_CALL;

function GetRCond(id: Nativeuint; Res: pDouble): Longword; CDECL; EXTERNAL KLU_CALL;

function GetCondEst(id: Nativeuint; Res: pDouble): Longword; CDECL; EXTERNAL KLU_CALL;

function AddPrimitiveMatrix(id: Nativeuint; nOrder: Longword; Nodes: pLongWord; Mat: pComplex): Longword; CDECL; EXTERNAL KLU_CALL;

function SetLogFile(Path: Pchar; Action: Longword): Longword; CDECL; EXTERNAL KLU_CALL;

function GetCompressedMatrix(id: Nativeuint; nColP, nNZ: Longword; pColP, pRowIdx: pLongWord; Mat: pComplex): Longword; CDECL; EXTERNAL KLU_CALL;

function GetTripletMatrix(id: Nativeuint; nNZ: Longword; pRows, pCols: pLongWord; Mat: pComplex): Longword; CDECL; EXTERNAL KLU_CALL;

function FindIslands(id: Nativeuint; nOrder: Longword; pNodes: pLongWord): Longword; CDECL; EXTERNAL KLU_CALL;

function AddMatrixElement(id: Nativeuint; i, j: Longword; Value: pComplex): Longword; CDECL; EXTERNAL KLU_CALL;

function GetMatrixElement(id: Nativeuint; i, j: Longword; Value: pComplex): Longword; CDECL; EXTERNAL KLU_CALL;
{$ENDIF}


implementation

initialization

    IsMultiThread := TRUE;

end.
