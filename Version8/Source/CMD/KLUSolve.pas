unit KLUSolve;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
    uComplex,
    DSSGlobals,
    {$IFDEF MSWINDOWS}
    Windows,
    {$ENDIF}
    variants,
    sysutils;

{$IFDEF MSWindows}
Const
  KLU_CALL ='libklusolve'
// in general, KLU arrays are 0-based
// function calls return 0 to indicate failure, 1 for success

// returns the non-zero handle of a new sparse matrix, if successful
// must call DeleteSparseSet on the valid handle when finished
FUNCTION NewSparseSet(nBus:LongWord):NativeUint;stdcall;external KLU_CALL;

// return 1 for success, 0 for invalid handle
FUNCTION DeleteSparseSet(id:NativeUInt):LongWord;stdcall;external KLU_CALL;

// return 1 for success, 2 for singular, 0 for invalid handle
// factors matrix if needed
FUNCTION SolveSparseSet(id:NativeUInt; x,b:pComplexArray):LongWord;stdcall;external KLU_CALL;

// return 1 for success, 0 for invalid handle
FUNCTION ZeroSparseSet(id:NativeUInt):LongWord;stdcall;external KLU_CALL;

// return 1 for success, 2 for singular, 0 for invalid handle
// FactorSparseMatrix does no extra work if the factoring was done previously
FUNCTION FactorSparseMatrix(id:NativeUInt):LongWord;stdcall;external KLU_CALL;

// These "Get" functions for matrix information all return 1 for success, 0 for invalid handle
// Res is the matrix order (number of nodes)
FUNCTION GetSize(id:NativeUInt; Res: pLongWord):LongWord;stdcall;external KLU_CALL;

// the following function results are not known prior to factoring
// Res is the number of floating point operations to factor
FUNCTION GetFlops(id:NativeUInt; Res: pDouble):LongWord;stdcall;external KLU_CALL;
// Res is number of non-zero entries in the original matrix
FUNCTION GetNNZ(id:NativeUInt; Res: pLongWord):LongWord;stdcall;external KLU_CALL;
// Res is the number of non-zero entries in factored matrix
FUNCTION GetSparseNNZ(id:NativeUInt; Res: pLongWord):LongWord;stdcall;external KLU_CALL;
// Res is a column number corresponding to a singularity, or 0 if not singular
FUNCTION GetSingularCol(id:NativeUInt; Res: pLongWord):LongWord;stdcall;external KLU_CALL;
// Res is the pivot element growth factor
FUNCTION GetRGrowth(id:NativeUInt; Res: pDouble):LongWord;stdcall;external KLU_CALL;
// Res is aquick estimate of the reciprocal of condition number
FUNCTION GetRCond(id:NativeUInt; Res: pDouble):LongWord;stdcall;external KLU_CALL;
// Res is a more accurate estimate of condition number
FUNCTION GetCondEst(id:NativeUInt; Res: pDouble):LongWord;stdcall;external KLU_CALL;

// return 1 for success, 0 for invalid handle or a node number out of range
FUNCTION AddPrimitiveMatrix(id:NativeUInt; nOrder:LongWord; Nodes: pLongWord; Mat: pComplex):LongWord;stdcall;external KLU_CALL;

// Action = 0 (close), 1 (rewrite) or 2 (append)
FUNCTION SetLogFile(Path: pChar; Action:LongWord):LongWord;stdcall;external KLU_CALL;

// fill sparse matrix in compressed column form
// return 1 for success, 0 for invalid handle, 2 for invalid array sizes
// pColP must be of length nColP == nBus + 1
// pRowIdx and pMat of length nNZ, which
//    must be at least the value returned by GetNNZ
FUNCTION GetCompressedMatrix(id:NativeUInt; nColP, nNZ:LongWord; pColP, pRowIdx: pLongWord; Mat: pComplex):LongWord;stdcall;external KLU_CALL;

// fill sparse matrix in triplet form
// return 1 for success, 0 for invalid handle, 2 for invalid array sizes
// pRows, pCols, and Mat must all be of length nNZ
FUNCTION GetTripletMatrix(id:NativeUInt; nNZ:LongWord; pRows, pCols: pLongWord; Mat: pComplex):LongWord;stdcall;external KLU_CALL;

// returns number of islands >= 1 by graph traversal
// pNodes contains the island number for each node
FUNCTION FindIslands(id:NativeUInt; nOrder:LongWord; pNodes: pLongWord):LongWord;stdcall;external KLU_CALL;

// AddMatrixElement is deprecated, use AddPrimitiveMatrix instead
FUNCTION AddMatrixElement(id:NativeUInt; i,j:LongWord; Value:pComplex):LongWord;stdcall;external KLU_CALL;

// GetMatrixElement is deprecated, use GetCompressedMatrix or GetTripletMatrix
FUNCTION GetMatrixElement(id:NativeUInt; i,j:LongWord; Value:pComplex):LongWord;stdcall;external KLU_CALL;

{$ELSE} // Darwin and Unix
Const
  KLU_CALL ='libklusolve';
  _PU = '';

FUNCTION NewSparseSet(nBus:LongWord):NativeUint;cdecl;external KLU_CALL;

FUNCTION DeleteSparseSet(id:NativeUInt):LongWord;cdecl;external KLU_CALL;

FUNCTION SolveSparseSet(id:NativeUInt; x,b:pComplexArray):LongWord;cdecl;external KLU_CALL;

FUNCTION ZeroSparseSet(id:NativeUInt):LongWord;cdecl;external KLU_CALL;

FUNCTION FactorSparseMatrix(id:NativeUInt):LongWord;cdecl;external KLU_CALL;

FUNCTION GetSize(id:NativeUInt; Res: pLongWord):LongWord;cdecl;external KLU_CALL;

FUNCTION GetFlops(id:NativeUInt; Res: pDouble):LongWord;cdecl;external KLU_CALL;

FUNCTION GetNNZ(id:NativeUInt; Res: pLongWord):LongWord;cdecl;external KLU_CALL;

FUNCTION GetSparseNNZ(id:NativeUInt; Res: pLongWord):LongWord;cdecl;external KLU_CALL;

FUNCTION GetSingularCol(id:NativeUInt; Res: pLongWord):LongWord;cdecl;external KLU_CALL;

FUNCTION GetRGrowth(id:NativeUInt; Res: pDouble):LongWord;cdecl;external KLU_CALL;

FUNCTION GetRCond(id:NativeUInt; Res: pDouble):LongWord;cdecl;external KLU_CALL;

FUNCTION GetCondEst(id:NativeUInt; Res: pDouble):LongWord;cdecl;external KLU_CALL;

FUNCTION AddPrimitiveMatrix(id:NativeUInt; nOrder:LongWord; Nodes: pLongWord; Mat: pComplex):LongWord;cdecl;external KLU_CALL;

FUNCTION SetLogFile(Path: pChar; Action:LongWord):LongWord;cdecl;external KLU_CALL;

FUNCTION GetCompressedMatrix(id:NativeUInt; nColP, nNZ:LongWord; pColP, pRowIdx: pLongWord; Mat: pComplex):LongWord;cdecl;external KLU_CALL;

FUNCTION GetTripletMatrix(id:NativeUInt; nNZ:LongWord; pRows, pCols: pLongWord; Mat: pComplex):LongWord;cdecl;external KLU_CALL;

FUNCTION FindIslands(id:NativeUInt; nOrder:LongWord; pNodes: pLongWord):LongWord;cdecl;external KLU_CALL;

FUNCTION AddMatrixElement(id:NativeUInt; i,j:LongWord; Value:pComplex):LongWord;cdecl;external KLU_CALL;

FUNCTION GetMatrixElement(id:NativeUInt; i,j:LongWord; Value:pComplex):LongWord;cdecl;external KLU_CALL;
{$ENDIF}

implementation

end.
