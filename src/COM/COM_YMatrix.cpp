// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_YMatrix.cpp : Implementation of CYMatrix

#include "COM_pch.h"
#include "COM_YMatrix.h"


STDMETHODIMP CYMatrix::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IYMatrix
    };

    for (int i = 0; i < sizeof(arr) / sizeof(arr[0]); i++)
    {
        if (InlineIsEqualGUID(*arr[i], riid))
        {
            return S_OK;
        }
    }
    return S_FALSE;
}

STDMETHODIMP CYMatrix::ZeroInjCurr()
{
    dss_capi.YMatrix_ZeroInjCurr(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CYMatrix::GetSourceInjCurrents()
{
    dss_capi.YMatrix_GetSourceInjCurrents(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CYMatrix::GetPCInjCurr()
{
    dss_capi.YMatrix_GetPCInjCurr(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CYMatrix::BuildYMatrixD(long BuildOps, VARIANT_BOOL AllocateVI)
{
    dss_capi.YMatrix_BuildYMatrixD(dss_capi_ctx, static_cast<int32_t>(BuildOps), AllocateVI);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CYMatrix::AddInAuxCurrents(long SType)
{
    dss_capi.YMatrix_AddInAuxCurrents(dss_capi_ctx, static_cast<int32_t>(SType));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CYMatrix::SolveSystem()
{
    dss_capi.YMatrix_SolveSystem(dss_capi_ctx, nullptr);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CYMatrix::SetGeneratordQdV()
{
    dss_capi.YMatrix_SetGeneratordQdV(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CYMatrix::CheckConvergence(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.YMatrix_CheckConvergence(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CYMatrix::get_SystemYChanged(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.YMatrix_Get_SystemYChanged(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CYMatrix::put_SystemYChanged(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.YMatrix_Set_SystemYChanged(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CYMatrix::get_UseAuxCurrents(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.YMatrix_Get_UseAuxCurrents(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CYMatrix::put_UseAuxCurrents(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.YMatrix_Set_UseAuxCurrents(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CYMatrix::get_LoadsNeedUpdating(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.YMatrix_Get_LoadsNeedUpdating(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CYMatrix::put_LoadsNeedUpdating(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.YMatrix_Set_LoadsNeedUpdating(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CYMatrix::get_SolutionInitialized(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.YMatrix_Get_SolutionInitialized(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CYMatrix::put_SolutionInitialized(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.YMatrix_Set_SolutionInitialized(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CYMatrix::get_SolverOptions(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.YMatrix_Get_SolverOptions(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CYMatrix::put_SolverOptions(long Value)
{
    dss_capi.YMatrix_Set_SolverOptions(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CYMatrix::get_Iteration(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.YMatrix_Get_Iteration(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CYMatrix::put_Iteration(long Value)
{
    dss_capi.YMatrix_Set_Iteration(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CYMatrix::GetCompressedYMatrix(VARIANT* ColPtr, VARIANT* RowIdxPtr, VARIANT* cVals)
{
    if (ColPtr == nullptr)
    {
        return E_POINTER;
    }
    if (RowIdxPtr == nullptr)
    {
        return E_POINTER;
    }
    if (cVals == nullptr)
    {
        return E_POINTER;
    }
    return dss_capi.YMatrix_GetCompressedYMatrixWrapped(dss_capi_ctx, ColPtr, RowIdxPtr, cVals);
}

