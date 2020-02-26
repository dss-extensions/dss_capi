unit KLUSolve;

interface

uses
    uComplex;

const
    KLULib = 'klusolve.dll';

// in general, KLU arrays are 0-based
// function calls return 0 to indicate failure, 1 for success

// returns the non-zero handle of a new sparse matrix, if successful
// must call DeleteSparseSet on the valid handle when finished
function NewSparseSet(nBus: Longword): NativeUInt; STDCALL; EXTERNAL KLULib;
// return 1 for success, 0 for invalid handle
function DeleteSparseSet(id: NativeUInt): Longword; STDCALL; EXTERNAL KLULib;

// return 1 for success, 2 for singular, 0 for invalid handle
// factors matrix if needed
function SolveSparseSet(id: NativeUInt; x, b: pComplexArray): Longword; STDCALL; EXTERNAL KLULib;

// return 1 for success, 0 for invalid handle
function ZeroSparseSet(id: NativeUInt): Longword; STDCALL; EXTERNAL KLULib;

// return 1 for success, 2 for singular, 0 for invalid handle
// FactorSparseMatrix does no extra work if the factoring was done previously
function FactorSparseMatrix(id: NativeUInt): Longword; STDCALL; EXTERNAL KLULib;

// These "Get" functions for matrix information all return 1 for success, 0 for invalid handle
// Res is the matrix order (number of nodes)
function GetSize(id: NativeUInt; Res: pLongWord): Longword; STDCALL; EXTERNAL KLULib;

// the following function results are not known prior to factoring
// Res is the number of floating point operations to factor
function GetFlops(id: NativeUInt; Res: pDouble): Longword; STDCALL; EXTERNAL KLULib;
// Res is number of non-zero entries in the original matrix
function GetNNZ(id: NativeUInt; Res: pLongWord): Longword; STDCALL; EXTERNAL KLULib;
// Res is the number of non-zero entries in factored matrix
function GetSparseNNZ(id: NativeUInt; Res: pLongWord): Longword; STDCALL; EXTERNAL KLULib;
// Res is a column number corresponding to a singularity, or 0 if not singular
function GetSingularCol(id: NativeUInt; Res: pLongWord): Longword; STDCALL; EXTERNAL KLULib;
// Res is the pivot element growth factor
function GetRGrowth(id: NativeUInt; Res: pDouble): Longword; STDCALL; EXTERNAL KLULib;
// Res is aquick estimate of the reciprocal of condition number
function GetRCond(id: NativeUInt; Res: pDouble): Longword; STDCALL; EXTERNAL KLULib;
// Res is a more accurate estimate of condition number
function GetCondEst(id: NativeUInt; Res: pDouble): Longword; STDCALL; EXTERNAL KLULib;

// return 1 for success, 0 for invalid handle or a node number out of range
function AddPrimitiveMatrix(id: NativeUInt; nOrder: Longword; Nodes: pLongWord; Mat: pComplex): Longword; STDCALL; EXTERNAL KLULib;

// Action = 0 (close), 1 (rewrite) or 2 (append)
function SetLogFile(Path: Pchar; Action: Longword): Longword; STDCALL; EXTERNAL KLULib;

// fill sparse matrix in compressed column form
// return 1 for success, 0 for invalid handle, 2 for invalid array sizes
// pColP must be of length nColP == nBus + 1
// pRowIdx and pMat of length nNZ, which
//    must be at least the value returned by GetNNZ
function GetCompressedMatrix(id: NativeUInt; nColP, nNZ: Longword; pColP, pRowIdx: pLongWord; Mat: pComplex): Longword; STDCALL; EXTERNAL KLULib;

// fill sparse matrix in triplet form
// return 1 for success, 0 for invalid handle, 2 for invalid array sizes
// pRows, pCols, and Mat must all be of length nNZ
function GetTripletMatrix(id: NativeUInt; nNZ: Longword; pRows, pCols: pLongWord; Mat: pComplex): Longword; STDCALL; EXTERNAL KLULib;

// returns number of islands >= 1 by graph traversal
// pNodes contains the island number for each node
function FindIslands(id: NativeUInt; nOrder: Longword; pNodes: pLongWord): Longword; STDCALL; EXTERNAL KLULib;

// AddMatrixElement is deprecated, use AddPrimitiveMatrix instead
function AddMatrixElement(id: NativeUInt; i, j: Longword; Value: pComplex): Longword; STDCALL; EXTERNAL KLULib;

// GetMatrixElement is deprecated, use GetCompressedMatrix or GetTripletMatrix
function GetMatrixElement(id: NativeUInt; i, j: Longword; Value: pComplex): Longword; STDCALL; EXTERNAL KLULib;

implementation

end.
