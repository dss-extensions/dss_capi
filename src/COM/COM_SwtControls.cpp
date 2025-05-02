// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_SwtControls.cpp : Implementation of CSwtControls

#include "COM_pch.h"
#include "COM_SwtControls.h"


STDMETHODIMP CSwtControls::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_ISwtControls
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

STDMETHODIMP CSwtControls::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.SwtControls_Get_AllNames, Value);
}

STDMETHODIMP CSwtControls::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.SwtControls_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::put_Name(BSTR Value)
{
    dss_capi.SwtControls_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.SwtControls_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.SwtControls_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::get_Action(ActionCodes* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = static_cast<ActionCodes>(dss_capi.SwtControls_Get_Action(dss_capi_ctx));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::put_Action(ActionCodes Value)
{
    dss_capi.SwtControls_Set_Action(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::get_IsLocked(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.SwtControls_Get_IsLocked(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::put_IsLocked(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.SwtControls_Set_IsLocked(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::get_Delay(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.SwtControls_Get_Delay(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::put_Delay(double Value)
{
    dss_capi.SwtControls_Set_Delay(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::get_SwitchedObj(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.SwtControls_Get_SwitchedObj(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::put_SwitchedObj(BSTR Value)
{
    dss_capi.SwtControls_Set_SwitchedObj(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::get_SwitchedTerm(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.SwtControls_Get_SwitchedTerm(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::put_SwitchedTerm(long Value)
{
    dss_capi.SwtControls_Set_SwitchedTerm(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.SwtControls_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::get_NormalState(ActionCodes* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = static_cast<ActionCodes>(dss_capi.SwtControls_Get_NormalState(dss_capi_ctx));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::put_NormalState(ActionCodes Value)
{
    dss_capi.SwtControls_Set_NormalState(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::get_State(ActionCodes* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = static_cast<ActionCodes>(dss_capi.SwtControls_Get_State(dss_capi_ctx));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::put_State(ActionCodes Value)
{
    dss_capi.SwtControls_Set_State(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::Reset()
{
    dss_capi.SwtControls_Reset(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.SwtControls_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSwtControls::put_idx(long Value)
{
    dss_capi.SwtControls_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

