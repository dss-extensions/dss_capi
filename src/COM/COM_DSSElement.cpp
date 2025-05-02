// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_DSSElement.cpp : Implementation of CDSSElement

#include "COM_pch.h"
#include "COM_DSSElement.h"

extern CComPtr<IDSSProperty> FDSSProperty;


STDMETHODIMP CDSSElement::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IDSSElement
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

STDMETHODIMP CDSSElement::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.DSSElement_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSSElement::get_Properties(VARIANT Indx, IDSSProperty** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    int32_t i;
    switch (Indx.vt)
    {
        case VT_I2:
        case VT_I4:
        {
            i = static_cast<int32_t>((Indx.vt == VT_I4) ? Indx.lVal : Indx.iVal);
            dss_capi.DSSProperty_Set_Index(dss_capi_ctx, i);
            break;
        }
        case VT_BSTR:
        {
            dss_capi.DSSProperty_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Indx.bstrVal));
            break;
        }
        default:
        {
            return E_INVALIDARG;
        }
    }
    return AltDSS_COM_CheckError();

}

STDMETHODIMP CDSSElement::get_NumProperties(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.DSSElement_Get_NumProperties(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSSElement::get_AllPropertyNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.DSSElement_Get_AllPropertyNames, Value);
}

STDMETHODIMP CDSSElement::ToJSON(long options, BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.DSSElement_ToJSON(dss_capi_ctx, static_cast<int32_t>(options))).Detach();
    return AltDSS_COM_CheckError();
}

