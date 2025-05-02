// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Capacitors.cpp : Implementation of CCapacitors

#include "COM_pch.h"
#include "COM_Capacitors.h"


STDMETHODIMP CCapacitors::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_ICapacitors
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

STDMETHODIMP CCapacitors::get_kV(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Capacitors_Get_kV(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapacitors::put_kV(double Value)
{
    dss_capi.Capacitors_Set_kV(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapacitors::get_kvar(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Capacitors_Get_kvar(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapacitors::put_kvar(double Value)
{
    dss_capi.Capacitors_Set_kvar(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapacitors::get_NumSteps(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Capacitors_Get_NumSteps(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapacitors::put_NumSteps(long Value)
{
    dss_capi.Capacitors_Set_NumSteps(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapacitors::get_IsDelta(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Capacitors_Get_IsDelta(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapacitors::put_IsDelta(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Capacitors_Set_IsDelta(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapacitors::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Capacitors_Get_AllNames, Value);
}

STDMETHODIMP CCapacitors::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Capacitors_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapacitors::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Capacitors_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapacitors::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Capacitors_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapacitors::put_Name(BSTR Value)
{
    dss_capi.Capacitors_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapacitors::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Capacitors_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapacitors::AddStep(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Capacitors_AddStep(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapacitors::SubtractStep(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Capacitors_SubtractStep(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapacitors::get_AvailableSteps(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Capacitors_Get_AvailableSteps(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapacitors::get_States(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetInt32s(dss_capi.Capacitors_Get_States_GR, Value);
}

STDMETHODIMP CCapacitors::put_States(VARIANT Value)
{
    return AltDSS_COM_SetInt32s(dss_capi.Capacitors_Set_States, Value);
}

STDMETHODIMP CCapacitors::Open()
{
    dss_capi.Capacitors_Open(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapacitors::Close()
{
    dss_capi.Capacitors_Close(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapacitors::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Capacitors_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapacitors::put_idx(long Value)
{
    dss_capi.Capacitors_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

