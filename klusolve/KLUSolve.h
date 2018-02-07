/* ------------------------------------------------------------------------- */
/* KLUSolve, Copyright (c) 2008, EnerNex Corporation. All rights reserved.   */
/* Licensed under the GNU Lesser General Public License (LGPL) v 2.1         */
/* ------------------------------------------------------------------------- */

#ifndef klusolve_included
#define klusolve_included

#ifndef _COMPLEX_DEFINED
#define _COMPLEX_DEFINED
typedef struct _complex {double x, y;} complex;
#endif

#ifndef KLU_API
#ifdef _WINDOWS
#define KLU_API unsigned int __stdcall
#define PTR_API ULONG_PTR __stdcall
#else
#define KLU_API unsigned int
#define PTR_API ULONG_PTR
#define ULONG_PTR unsigned long long
#define TRUE 1
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

// return handle of new sparse set, 0 if error
// be sure to DeleteSparseSet using the returned handle
PTR_API NewSparseSet (unsigned int nBus);

// return 1 if successful, 0 if not
KLU_API ZeroSparseSet (ULONG_PTR hSparse);

// return 1 if successful, 2 if singular, 0 if other error
KLU_API FactorSparseMatrix (ULONG_PTR hSparse);

/* 
  input: current injections in zero-based _acxB
  output: node voltages in zero-based _acxX
  no provision for voltage sources
*/
// return 1 if successful, 2 if singular, 0 if other error
KLU_API SolveSparseSet (ULONG_PTR hSparse, complex *_acxX, complex *_acxB);

// return 1 if successful, 0 if not
KLU_API DeleteSparseSet (ULONG_PTR hSparse);

/* i and j are 1-based for these */
// return 1 if successful, 0 if not
KLU_API AddMatrixElement (ULONG_PTR hSparse, unsigned int i, unsigned int j, complex *pcxVal);
KLU_API GetMatrixElement (ULONG_PTR hSparse, unsigned int i, unsigned int j, complex *pcxVal);

// new functions
KLU_API GetSize (ULONG_PTR hSparse, unsigned int *pResult);
KLU_API GetNNZ (ULONG_PTR hSparse, unsigned int *pResult);
KLU_API GetSparseNNZ (ULONG_PTR hSparse, unsigned int *pResult);
KLU_API GetRCond (ULONG_PTR hSparse, double *pResult);
KLU_API GetRGrowth (ULONG_PTR hSparse, double *pResult);
KLU_API GetCondEst (ULONG_PTR hSparse, double *pResult);
KLU_API GetFlops (ULONG_PTR hSparse, double *pResult);
KLU_API GetSingularCol (ULONG_PTR hSparse, unsigned int *pResult);

KLU_API AddPrimitiveMatrix (ULONG_PTR hSparse, unsigned int nOrder,
							unsigned int *pNodes, complex *pcY);
KLU_API GetCompressedMatrix (ULONG_PTR hSparse, unsigned int nColP, 
							 unsigned int nNZ, unsigned int *pColP, 
							 unsigned int *pRowIdx, complex *pcY);
KLU_API GetTripletMatrix (ULONG_PTR hSparse, unsigned int nNZ,
						  unsigned int *pRows, unsigned int *pCols, complex *pcY);
KLU_API FindIslands (ULONG_PTR hSparse, unsigned int nOrder, unsigned int *pNodes);

// iAction = 0 to close, 1 to rewrite, 2 to append
KLU_API SetLogFile (char *path, unsigned int iAction);

#ifdef __cplusplus
}
#endif

#endif // klusolve_included