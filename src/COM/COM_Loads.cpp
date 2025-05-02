// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Loads.cpp : Implementation of CLoads

#include "COM_pch.h"
#include "COM_Loads.h"


STDMETHODIMP CLoads::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_ILoads
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

STDMETHODIMP CLoads::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Loads_Get_AllNames, Value);
}

STDMETHODIMP CLoads::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Loads_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_Name(BSTR Value)
{
    dss_capi.Loads_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_idx(long Value)
{
    dss_capi.Loads_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_kW(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_kW(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_kW(double Value)
{
    dss_capi.Loads_Set_kW(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_kV(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_kV(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_kV(double Value)
{
    dss_capi.Loads_Set_kV(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_kvar(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_kvar(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_kvar(double Value)
{
    dss_capi.Loads_Set_kvar(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_PF(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_PF(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_PF(double Value)
{
    dss_capi.Loads_Set_PF(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_PctMean(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_PctMean(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_PctMean(double Value)
{
    dss_capi.Loads_Set_PctMean(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_PctStdDev(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_PctStdDev(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_PctStdDev(double Value)
{
    dss_capi.Loads_Set_PctStdDev(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_AllocationFactor(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_AllocationFactor(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_AllocationFactor(double Value)
{
    dss_capi.Loads_Set_AllocationFactor(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_Cfactor(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_Cfactor(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_Cfactor(double Value)
{
    dss_capi.Loads_Set_Cfactor(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_Class(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_Class_(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_Class(long Value)
{
    dss_capi.Loads_Set_Class_(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_IsDelta(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_IsDelta(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_IsDelta(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Loads_Set_IsDelta(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_CVRcurve(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Loads_Get_CVRcurve(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_CVRcurve(BSTR Value)
{
    dss_capi.Loads_Set_CVRcurve(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_CVRwatts(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_CVRwatts(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_CVRwatts(double Value)
{
    dss_capi.Loads_Set_CVRwatts(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_CVRvars(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_CVRvars(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_CVRvars(double Value)
{
    dss_capi.Loads_Set_CVRvars(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_daily(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Loads_Get_daily(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_daily(BSTR Value)
{
    dss_capi.Loads_Set_daily(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_duty(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Loads_Get_duty(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_duty(BSTR Value)
{
    dss_capi.Loads_Set_duty(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_kva(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_kva(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_kva(double Value)
{
    dss_capi.Loads_Set_kva(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_kwh(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_kwh(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_kwh(double Value)
{
    dss_capi.Loads_Set_kwh(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_kwhdays(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_kwhdays(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_kwhdays(double Value)
{
    dss_capi.Loads_Set_kwhdays(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_Model(LoadModels* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = static_cast<LoadModels>(dss_capi.Loads_Get_Model(dss_capi_ctx));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_Model(LoadModels Value)
{
    dss_capi.Loads_Set_Model(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_NumCust(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_NumCust(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_NumCust(long Value)
{
    dss_capi.Loads_Set_NumCust(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_Rneut(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_Rneut(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_Rneut(double Value)
{
    dss_capi.Loads_Set_Rneut(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_Spectrum(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Loads_Get_Spectrum(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_Spectrum(BSTR Value)
{
    dss_capi.Loads_Set_Spectrum(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_Vmaxpu(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_Vmaxpu(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_Vmaxpu(double Value)
{
    dss_capi.Loads_Set_Vmaxpu(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_Vminemerg(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_Vminemerg(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_Vminemerg(double Value)
{
    dss_capi.Loads_Set_Vminemerg(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_Vminnorm(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_Vminnorm(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_Vminnorm(double Value)
{
    dss_capi.Loads_Set_Vminnorm(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_Vminpu(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_Vminpu(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_Vminpu(double Value)
{
    dss_capi.Loads_Set_Vminpu(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_xfkVA(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_xfkVA(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_xfkVA(double Value)
{
    dss_capi.Loads_Set_xfkVA(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_Xneut(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_Xneut(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_Xneut(double Value)
{
    dss_capi.Loads_Set_Xneut(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_Yearly(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Loads_Get_Yearly(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_Yearly(BSTR Value)
{
    dss_capi.Loads_Set_Yearly(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_Status(LoadStatus* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = static_cast<LoadStatus>(dss_capi.Loads_Get_Status(dss_capi_ctx));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_Status(LoadStatus Value)
{
    dss_capi.Loads_Set_Status(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_Growth(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Loads_Get_Growth(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_Growth(BSTR Value)
{
    dss_capi.Loads_Set_Growth(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_ZIPV(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Loads_Get_ZIPV_GR, Value);
}

STDMETHODIMP CLoads::put_ZIPV(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.Loads_Set_ZIPV, Value);
}

STDMETHODIMP CLoads::get_pctSeriesRL(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_pctSeriesRL(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_pctSeriesRL(double Value)
{
    dss_capi.Loads_Set_pctSeriesRL(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_RelWeight(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_RelWeight(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_RelWeight(double Value)
{
    dss_capi.Loads_Set_RelWeight(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_Sensor(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Loads_Get_Sensor(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::get_Phases(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Loads_Get_Phases(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoads::put_Phases(long Value)
{
    dss_capi.Loads_Set_Phases(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

