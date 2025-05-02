// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_DSS_Executive.cpp : Implementation of CDSS_Executive

#include "COM_pch.h"
#include "COM_DSS_Executive.h"


STDMETHODIMP CDSS_Executive::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IDSS_Executive
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

STDMETHODIMP CDSS_Executive::get_NumCommands(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.DSS_Executive_Get_NumCommands(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSS_Executive::get_NumOptions(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.DSS_Executive_Get_NumOptions(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSS_Executive::get_Command(long i, BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.DSS_Executive_Get_Command(dss_capi_ctx, static_cast<int32_t>(i))).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSS_Executive::get_Option(long i, BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.DSS_Executive_Get_Option(dss_capi_ctx, static_cast<int32_t>(i))).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSS_Executive::get_CommandHelp(long i, BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.DSS_Executive_Get_CommandHelp(dss_capi_ctx, static_cast<int32_t>(i))).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSS_Executive::get_OptionHelp(long i, BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.DSS_Executive_Get_OptionHelp(dss_capi_ctx, static_cast<int32_t>(i))).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSS_Executive::get_OptionValue(long i, BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.DSS_Executive_Get_OptionValue(dss_capi_ctx, static_cast<int32_t>(i))).Detach();
    return AltDSS_COM_CheckError();
}

