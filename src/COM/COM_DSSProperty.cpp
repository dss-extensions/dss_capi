// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_DSSProperty.cpp : Implementation of CDSSProperty

#include "COM_pch.h"
#include "COM_DSSProperty.h"


STDMETHODIMP CDSSProperty::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IDSSProperty
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

STDMETHODIMP CDSSProperty::get_Name(BSTR* Name)
{
    if (Name == nullptr)
    {
        return E_POINTER;
    }
    *Name = CComBSTR(dss_capi.DSSProperty_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSSProperty::get_Description(BSTR* Description)
{
    if (Description == nullptr)
    {
        return E_POINTER;
    }
    *Description = CComBSTR(dss_capi.DSSProperty_Get_Description(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSSProperty::get_Val(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.DSSProperty_Get_Val(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSSProperty::put_Val(BSTR Value)
{
    dss_capi.DSSProperty_Set_Val(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

