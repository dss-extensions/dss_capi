// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Error.cpp : Implementation of CError

#include "COM_pch.h"
#include "COM_Error.h"


STDMETHODIMP CError::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IError
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

STDMETHODIMP CError::get_Number(long* Number)
{
    if (Number == nullptr)
    {
        return E_POINTER;
    }
    *Number = dss_capi.Error_Get_Number(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CError::get_Description(BSTR* Description)
{
    if (Description == nullptr)
    {
        return E_POINTER;
    }
    *Description = CComBSTR(dss_capi.Error_Get_Description(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CError::get_EarlyAbort(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Error_Get_EarlyAbort(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CError::put_EarlyAbort(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Error_Set_EarlyAbort(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CError::get_UseExceptions(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Error_Get_UseExceptions(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CError::put_UseExceptions(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Error_Set_UseExceptions(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

