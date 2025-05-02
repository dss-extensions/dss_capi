// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_ReduceCkt.cpp : Implementation of CReduceCkt

#include "COM_pch.h"
#include "COM_ReduceCkt.h"


STDMETHODIMP CReduceCkt::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IReduceCkt
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

STDMETHODIMP CReduceCkt::get_Zmag(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.ReduceCkt_Get_Zmag(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReduceCkt::put_Zmag(double Value)
{
    dss_capi.ReduceCkt_Set_Zmag(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReduceCkt::get_KeepLoad(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.ReduceCkt_Get_KeepLoad(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReduceCkt::put_KeepLoad(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.ReduceCkt_Set_KeepLoad(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReduceCkt::get_EditString(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.ReduceCkt_Get_EditString(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReduceCkt::put_EditString(BSTR Value)
{
    dss_capi.ReduceCkt_Set_EditString(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReduceCkt::get_StartPDElement(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.ReduceCkt_Get_StartPDElement(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReduceCkt::put_StartPDElement(BSTR Value)
{
    dss_capi.ReduceCkt_Set_StartPDElement(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReduceCkt::get_EnergyMeter(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.ReduceCkt_Get_EnergyMeter(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReduceCkt::put_EnergyMeter(BSTR Value)
{
    dss_capi.ReduceCkt_Set_EnergyMeter(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReduceCkt::SaveCircuit(BSTR CktName)
{
    dss_capi.ReduceCkt_SaveCircuit(dss_capi_ctx, AltDSS_COM_CString(CktName));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReduceCkt::DoDefault()
{
    dss_capi.ReduceCkt_DoDefault(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReduceCkt::DoShortLines()
{
    dss_capi.ReduceCkt_DoShortLines(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReduceCkt::DoDangling()
{
    dss_capi.ReduceCkt_DoDangling(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReduceCkt::DoLoopBreak()
{
    dss_capi.ReduceCkt_DoLoopBreak(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReduceCkt::DoParallelLines()
{
    dss_capi.ReduceCkt_DoParallelLines(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReduceCkt::DoSwitches()
{
    dss_capi.ReduceCkt_DoSwitches(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReduceCkt::Do1phLaterals()
{
    dss_capi.ReduceCkt_Do1phLaterals(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReduceCkt::DoBranchRemove()
{
    dss_capi.ReduceCkt_DoBranchRemove(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

