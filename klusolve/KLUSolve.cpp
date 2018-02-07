/* ------------------------------------------------------------------------- */
/* KLUSolve, Copyright (c) 2008, EnerNex Corporation. All rights reserved.   */
/* Licensed under the GNU Lesser General Public License (LGPL) v 2.1         */
/* ------------------------------------------------------------------------- */

// KLUSolve.cpp : Defines the entry point for the DLL application.
//

#ifdef _WINDOWS
#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers
#include <windows.h>
#define KLU_API unsigned int __stdcall
#define PTR_API ULONG_PTR __stdcall
#else
#include <stdarg.h>
#define KLU_API unsigned int
#define PTR_API ULONG_PTR
#define ULONG_PTR unsigned long long
#define TRUE 1
#endif

#include "KLUSolve.h"
#include "KLUSystem.h"

#define SYMMETRIC_MATRIX

// we can't have static data in a multithreaded environment
/*
static FILE *lfp = NULL;

static void write_lfp (const char *fmt, ...)
{
	va_list args;
	va_start (args, fmt);

	if (lfp) {
		vfprintf (lfp, fmt, args);
		fflush (lfp);
	}

	va_end (args);
}

// iAction = 0 to close, 1 to rewrite, 2 to append
KLU_API SetLogFile (char *path, unsigned int iAction)
{
	unsigned int rc = 1;
	if (iAction == 0) {
		if (lfp) fclose (lfp);
	} else if (iAction == 1) {
		if (lfp) fclose (lfp);
		lfp = fopen (path, "w");
		if (!lfp) rc = 0;
	} else if (iAction == 2) {
		if (lfp) fclose (lfp);
		lfp = fopen (path, "a");
		if (!lfp) rc = 0;
	} else {
		rc = 0;
	}
	return rc;
}
*/

KLU_API SetLogFile (char *, unsigned int)
{
    return 0;
}

#ifdef _WINDOWS

BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
	switch (ul_reason_for_call) {
		case DLL_PROCESS_ATTACH:
			break;
		case DLL_PROCESS_DETACH:
			break;
		case DLL_THREAD_ATTACH:
			break;
		case DLL_THREAD_DETACH:
			break;
	}
    return TRUE;
}

#endif

// exported function definitions

PTR_API NewSparseSet (unsigned int nBus)
{
    ULONG_PTR rc = 0;

//	write_lfp ("NewSparseSet %u\n", nBus);

    KLUSystem *pSys = new KLUSystem ();
    if (pSys) {
        pSys->Initialize(nBus, 0, nBus);
        rc = reinterpret_cast<ULONG_PTR> (pSys);
    }
	return rc;
}

KLU_API ZeroSparseSet (ULONG_PTR hSparse)
{
    unsigned long rc = 0;

//	write_lfp ("ZeroSparseSet\n");

	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
	if (pSys) {
		pSys->zero();
		pSys->bFactored = false;
		rc = 1;
	}
	return rc;
}

KLU_API FactorSparseMatrix (ULONG_PTR hSparse)
{
    unsigned int rc = 0;

//	write_lfp ("FactorSparseMatrix\n");

	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
	if (pSys) {
		if (pSys->FactorSystem() == 0) { // success
			rc = 1;
		} else { // singular
			rc = 2;
		}
	}
	return rc;
}

/* 
  input: current injections in zero-based _acxB
  output: node voltages in zero-based _acxX
  no provision for voltage sources
*/
KLU_API SolveSparseSet(ULONG_PTR hSparse, complex *_acxX, complex *_acxB)
{
    unsigned int rc = 0;

	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
	if (pSys) {
		if (pSys->bFactored == false) {
			pSys->FactorSystem();
		}
		if (pSys->bFactored) {
			pSys->SolveSystem (_acxX, _acxB);
			rc = 1;
		} else {
			rc = 2;
		}
	}
//	write_lfp ("SolveSparseSet returning %u\n", rc);

	return rc;
}

KLU_API DeleteSparseSet(ULONG_PTR hSparse)
{
    unsigned int rc = 0;

//	write_lfp ("DeleteSparseSet %u\n", hSparse);

	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
	if (pSys) {
		delete pSys;
		rc = 1;
    }

	return rc;
}

/* i and j are 1-based for these */
KLU_API AddMatrixElement(ULONG_PTR hSparse, unsigned int i, unsigned int j, complex *pcxVal)
{
    unsigned int rc = 0;

//	write_lfp ("AddMatrixElement [%u,%u] = %G + j%G\n", i, j, pcxVal->x, pcxVal->y);

	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
	if (pSys) {
		pSys->AddElement (i, j, *pcxVal, TRUE);
#ifdef SYMMETRIC_MATRIX
		if (i != j) pSys->AddElement (j, i, *pcxVal, TRUE);
#endif
		pSys->bFactored = false;
		rc = 1;
	}
	return rc;
}

KLU_API GetMatrixElement(ULONG_PTR hSparse, unsigned int i, unsigned int j, complex *pcxVal)
{
    unsigned int rc = 0;

	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
	if (pSys) {
		pSys->GetElement (i, j, *pcxVal);
		rc = 1;
	}
	return rc;
}

// new functions
KLU_API GetSize (ULONG_PTR hSparse, unsigned int *pResult)
{
    unsigned int rc = 0;
	*pResult = 0;
	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
	if (pSys) {
		*pResult = pSys->GetSize();
		rc = 1;
	}
	return rc;
}

KLU_API GetNNZ (ULONG_PTR hSparse, unsigned int *pResult)
{
    unsigned int rc = 0;
	*pResult = 0;
	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
	if (pSys) {
		*pResult = pSys->GetNNZ();
		rc = 1;
	}
	return rc;
}

KLU_API GetSparseNNZ (ULONG_PTR hSparse, unsigned int *pResult)
{
    unsigned int rc = 0;
	*pResult = 0;
	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
	if (pSys) {
		*pResult = pSys->GetSparseNNZ();
		rc = 1;
	}
	return rc;
}

KLU_API GetRCond (ULONG_PTR hSparse, double *pResult)
{
    unsigned int rc = 0;
	*pResult = 0.0;
	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
	if (pSys) {
		*pResult = pSys->GetRCond();
		rc = 1;
	}
	return rc;
}

KLU_API GetRGrowth (ULONG_PTR hSparse, double *pResult)
{
    unsigned int rc = 0;
	*pResult = 0.0;
	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
	if (pSys) {
		*pResult = pSys->GetRGrowth();
		rc = 1;
	}
	return rc;
}

KLU_API GetCondEst (ULONG_PTR hSparse, double *pResult)
{
    unsigned int rc = 0;
	*pResult = 0.0;
	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
	if (pSys) {
		*pResult = pSys->GetCondEst();
		rc = 1;
	}
	return rc;
}

KLU_API GetFlops (ULONG_PTR hSparse, double *pResult)
{
    unsigned int rc = 0;
	*pResult = 0.0;
	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
	if (pSys) {
		*pResult = pSys->GetFlops();
		rc = 1;
	}
	return rc;
}

KLU_API GetSingularCol (ULONG_PTR hSparse, unsigned int *pResult)
{
    unsigned int rc = 0;
	*pResult = 0;
	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
	if (pSys) {
		*pResult = pSys->GetSingularCol();
		rc = 1;
	}
	return rc;
}

KLU_API AddPrimitiveMatrix (ULONG_PTR hSparse, unsigned int nOrder,
							unsigned int *pNodes, complex *pcY)
{
    unsigned int rc = 0;

/*
	if (lfp) {
		write_lfp ("AddPrimitiveMatrix, nOrder = %u\n", nOrder);
		for (unsigned i = 0; i < nOrder; i++) {
			unsigned idx = i;
			for (unsigned j = 0; j < nOrder; j++) {
				write_lfp ("\tLocal [%u,%u] System [%u,%u] Val(%u) = %G + j%G\n", 
					i, j, pNodes[i], pNodes[j], idx, pcY[idx].x, pcY[idx].y);
				idx += nOrder;
			}
		}
	}
*/

	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
	if (pSys) {
		rc = pSys->AddPrimitiveMatrix (nOrder, pNodes, pcY);
		pSys->bFactored = false;
	}
	return rc;
}

KLU_API GetCompressedMatrix (ULONG_PTR hSparse, unsigned int nColP, unsigned int nNZ,
				   unsigned int *pColP, unsigned int *pRowIdx, complex *pcY)
{
    unsigned int rc = 0;
	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
	if (pSys) {
		if (pSys->GetCompressedMatrix (nColP, nNZ, pColP, pRowIdx, pcY)) {
			rc = 1;
		} else { // probably a size mismatch
			rc = 2;
		}
	}
	return rc;
}

KLU_API GetTripletMatrix (ULONG_PTR hSparse, unsigned int nNZ,
						  unsigned int *pRows, unsigned int *pCols, complex *pcY)
{
    unsigned int rc = 0;
	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
	if (pSys) {
		if (pSys->GetTripletMatrix (nNZ, pRows, pCols, pcY)) {
			rc = 1;
		} else { // probably a size mismatch
			rc = 2;
		}
	}
	return rc;
}

KLU_API FindIslands (ULONG_PTR hSparse, unsigned int nOrder, unsigned int *pNodes)
{
    unsigned int rc = 0;
	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
	if (pSys && nOrder >= pSys->GetSize()) {
		rc = pSys->FindIslands (pNodes);
	}
	return rc;
}
