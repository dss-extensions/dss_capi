// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_GICSources.cpp : Implementation of CGICSources

#include "COM_pch.h"
#include "COM_GICSources.h"


STDMETHODIMP CGICSources::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IGICSources
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

STDMETHODIMP CGICSources::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.GICSources_Get_AllNames, Value);
}

STDMETHODIMP CGICSources::get_Bus1(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.GICSources_Get_Bus1(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::get_Bus2(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.GICSources_Get_Bus2(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.GICSources_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::put_Name(BSTR Value)
{
    dss_capi.GICSources_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::get_Phases(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.GICSources_Get_Phases(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::put_Phases(long Value)
{
    dss_capi.GICSources_Set_Phases(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::get_EN(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.GICSources_Get_EN(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::put_EN(double Value)
{
    dss_capi.GICSources_Set_EN(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::get_EE(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.GICSources_Get_EE(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::put_EE(double Value)
{
    dss_capi.GICSources_Set_EE(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::get_Lat1(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.GICSources_Get_Lat1(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::put_Lat1(double Value)
{
    dss_capi.GICSources_Set_Lat1(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::get_Lat2(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.GICSources_Get_Lat2(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::put_Lat2(double Value)
{
    dss_capi.GICSources_Set_Lat2(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::get_Lon1(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.GICSources_Get_Lon1(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::put_Lon1(double Value)
{
    dss_capi.GICSources_Set_Lon1(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::get_Lon2(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.GICSources_Get_Lon2(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::put_Lon2(double Value)
{
    dss_capi.GICSources_Set_Lon2(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::get_Volts(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.GICSources_Get_Volts(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::put_Volts(double Value)
{
    dss_capi.GICSources_Set_Volts(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.GICSources_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.GICSources_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGICSources::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.GICSources_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

