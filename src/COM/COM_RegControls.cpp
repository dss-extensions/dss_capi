// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_RegControls.cpp : Implementation of CRegControls

#include "COM_pch.h"
#include "COM_RegControls.h"


STDMETHODIMP CRegControls::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IRegControls
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

STDMETHODIMP CRegControls::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.RegControls_Get_AllNames, Value);
}

STDMETHODIMP CRegControls::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.RegControls_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_Name(BSTR Value)
{
    dss_capi.RegControls_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_MonitoredBus(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.RegControls_Get_MonitoredBus(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_MonitoredBus(BSTR Value)
{
    dss_capi.RegControls_Set_MonitoredBus(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_Transformer(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.RegControls_Get_Transformer(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_Transformer(BSTR Value)
{
    dss_capi.RegControls_Set_Transformer(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_TapWinding(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_TapWinding(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_TapWinding(long Value)
{
    dss_capi.RegControls_Set_TapWinding(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_Winding(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_Winding(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_Winding(long Value)
{
    dss_capi.RegControls_Set_Winding(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_CTPrimary(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_CTPrimary(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_CTPrimary(double Value)
{
    dss_capi.RegControls_Set_CTPrimary(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_PTratio(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_PTratio(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_PTratio(double Value)
{
    dss_capi.RegControls_Set_PTratio(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_ForwardR(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_ForwardR(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_ForwardR(double Value)
{
    dss_capi.RegControls_Set_ForwardR(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_ForwardX(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_ForwardX(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_ForwardX(double Value)
{
    dss_capi.RegControls_Set_ForwardX(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_ReverseR(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_ReverseR(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_ReverseR(double Value)
{
    dss_capi.RegControls_Set_ReverseR(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_ReverseX(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_ReverseX(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_ReverseX(double Value)
{
    dss_capi.RegControls_Set_ReverseX(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_IsReversible(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_IsReversible(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_IsReversible(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.RegControls_Set_IsReversible(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_IsInverseTime(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_IsInverseTime(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_IsInverseTime(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.RegControls_Set_IsInverseTime(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_Delay(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_Delay(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_Delay(double Value)
{
    dss_capi.RegControls_Set_Delay(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_TapDelay(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_TapDelay(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_TapDelay(double Value)
{
    dss_capi.RegControls_Set_TapDelay(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_MaxTapChange(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_MaxTapChange(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_MaxTapChange(long Value)
{
    dss_capi.RegControls_Set_MaxTapChange(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_VoltageLimit(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_VoltageLimit(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_VoltageLimit(double Value)
{
    dss_capi.RegControls_Set_VoltageLimit(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_ForwardBand(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_ForwardBand(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_ForwardBand(double Value)
{
    dss_capi.RegControls_Set_ForwardBand(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_ForwardVreg(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_ForwardVreg(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_ForwardVreg(double Value)
{
    dss_capi.RegControls_Set_ForwardVreg(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_ReverseBand(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_ReverseBand(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_ReverseBand(double Value)
{
    dss_capi.RegControls_Set_ReverseBand(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_ReverseVreg(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_ReverseVreg(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_ReverseVreg(double Value)
{
    dss_capi.RegControls_Set_ReverseVreg(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_TapNumber(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_TapNumber(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_TapNumber(long Value)
{
    dss_capi.RegControls_Set_TapNumber(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::Reset()
{
    dss_capi.RegControls_Reset(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.RegControls_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRegControls::put_idx(long Value)
{
    dss_capi.RegControls_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

