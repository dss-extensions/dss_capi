// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_PVSystems.cpp : Implementation of CPVSystems

#include "COM_pch.h"
#include "COM_PVSystems.h"


STDMETHODIMP CPVSystems::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IPVSystems
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

STDMETHODIMP CPVSystems::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.PVSystems_Get_AllNames, Value);
}

STDMETHODIMP CPVSystems::get_RegisterNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.PVSystems_Get_RegisterNames, Value);
}

STDMETHODIMP CPVSystems::get_RegisterValues(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.PVSystems_Get_RegisterValues_GR, Value);
}

STDMETHODIMP CPVSystems::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PVSystems_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PVSystems_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PVSystems_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PVSystems_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::put_idx(long Value)
{
    dss_capi.PVSystems_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.PVSystems_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::put_Name(BSTR Value)
{
    dss_capi.PVSystems_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::get_Irradiance(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PVSystems_Get_Irradiance(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::put_Irradiance(double Value)
{
    dss_capi.PVSystems_Set_Irradiance(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::get_kW(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PVSystems_Get_kW(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::get_kvar(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PVSystems_Get_kvar(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::put_kvar(double Value)
{
    dss_capi.PVSystems_Set_kvar(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::get_PF(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PVSystems_Get_PF(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::put_PF(double Value)
{
    dss_capi.PVSystems_Set_PF(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::get_kVArated(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PVSystems_Get_kVArated(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::put_kVArated(double Value)
{
    dss_capi.PVSystems_Set_kVArated(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::get_Pmpp(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PVSystems_Get_Pmpp(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::put_Pmpp(double Value)
{
    dss_capi.PVSystems_Set_Pmpp(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::get_IrradianceNow(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PVSystems_Get_IrradianceNow(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::get_Sensor(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.PVSystems_Get_Sensor(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::get_daily(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.PVSystems_Get_daily(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::put_daily(BSTR Value)
{
    dss_capi.PVSystems_Set_daily(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::get_duty(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.PVSystems_Get_duty(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::put_duty(BSTR Value)
{
    dss_capi.PVSystems_Set_duty(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::get_Yearly(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.PVSystems_Get_yearly(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::put_Yearly(BSTR Value)
{
    dss_capi.PVSystems_Set_yearly(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::get_Tdaily(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.PVSystems_Get_Tdaily(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::put_Tdaily(BSTR Value)
{
    dss_capi.PVSystems_Set_Tdaily(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::get_Tduty(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.PVSystems_Get_Tduty(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::put_Tduty(BSTR Value)
{
    dss_capi.PVSystems_Set_Tduty(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::get_Tyearly(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.PVSystems_Get_Tyearly(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPVSystems::put_Tyearly(BSTR Value)
{
    dss_capi.PVSystems_Set_Tyearly(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

