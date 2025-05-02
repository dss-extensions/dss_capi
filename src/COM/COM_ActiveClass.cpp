// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_ActiveClass.cpp : Implementation of CActiveClass

#include "COM_pch.h"
#include "COM_ActiveClass.h"


STDMETHODIMP CActiveClass::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IActiveClass
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

STDMETHODIMP CActiveClass::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.ActiveClass_Get_AllNames, Value);
}

STDMETHODIMP CActiveClass::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.ActiveClass_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CActiveClass::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.ActiveClass_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CActiveClass::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.ActiveClass_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CActiveClass::put_Name(BSTR Value)
{
    dss_capi.ActiveClass_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CActiveClass::get_NumElements(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.ActiveClass_Get_NumElements(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CActiveClass::get_ActiveClassName(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.ActiveClass_Get_ActiveClassName(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CActiveClass::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.ActiveClass_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CActiveClass::get_ActiveClassParent(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.ActiveClass_Get_ActiveClassParent(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CActiveClass::ToJSON(long options, BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.ActiveClass_ToJSON(dss_capi_ctx, static_cast<int32_t>(options))).Detach();
    return AltDSS_COM_CheckError();
}

