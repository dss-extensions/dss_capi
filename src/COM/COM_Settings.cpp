// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Settings.cpp : Implementation of CSettings

#include "COM_pch.h"
#include "COM_Settings.h"


STDMETHODIMP CSettings::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_ISettings
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

STDMETHODIMP CSettings::get_AllowDuplicates(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Settings_Get_AllowDuplicates(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_AllowDuplicates(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Settings_Set_AllowDuplicates(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_ZoneLock(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Settings_Get_ZoneLock(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_ZoneLock(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Settings_Set_ZoneLock(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_AllocationFactors(double rhs)
{
    dss_capi.Settings_Set_AllocationFactors(dss_capi_ctx, rhs);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_AutoBusList(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Settings_Get_AutoBusList(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_AutoBusList(BSTR Value)
{
    dss_capi.Settings_Set_AutoBusList(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_CktModel(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Settings_Get_CktModel(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_CktModel(long Value)
{
    dss_capi.Settings_Set_CktModel(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_NormVminpu(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Settings_Get_NormVminpu(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_NormVminpu(double Value)
{
    dss_capi.Settings_Set_NormVminpu(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_NormVmaxpu(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Settings_Get_NormVmaxpu(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_NormVmaxpu(double Value)
{
    dss_capi.Settings_Set_NormVmaxpu(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_EmergVminpu(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Settings_Get_EmergVminpu(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_EmergVminpu(double Value)
{
    dss_capi.Settings_Set_EmergVminpu(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_EmergVmaxpu(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Settings_Get_EmergVmaxpu(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_EmergVmaxpu(double Value)
{
    dss_capi.Settings_Set_EmergVmaxpu(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_UEweight(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Settings_Get_UEweight(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_UEweight(double Value)
{
    dss_capi.Settings_Set_UEweight(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_LossWeight(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Settings_Get_LossWeight(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_LossWeight(double Value)
{
    dss_capi.Settings_Set_LossWeight(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_UEregs(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetInt32s(dss_capi.Settings_Get_UEregs_GR, Value);
}

STDMETHODIMP CSettings::put_UEregs(VARIANT Value)
{
    return AltDSS_COM_SetInt32s(dss_capi.Settings_Set_UEregs, Value);
}

STDMETHODIMP CSettings::get_LossRegs(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetInt32s(dss_capi.Settings_Get_LossRegs_GR, Value);
}

STDMETHODIMP CSettings::put_LossRegs(VARIANT Value)
{
    return AltDSS_COM_SetInt32s(dss_capi.Settings_Set_LossRegs, Value);
}

STDMETHODIMP CSettings::get_Trapezoidal(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Settings_Get_Trapezoidal(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_Trapezoidal(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Settings_Set_Trapezoidal(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_VoltageBases(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Settings_Get_VoltageBases_GR, Value);
}

STDMETHODIMP CSettings::put_VoltageBases(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.Settings_Set_VoltageBases, Value);
}

STDMETHODIMP CSettings::get_ControlTrace(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Settings_Get_ControlTrace(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_ControlTrace(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Settings_Set_ControlTrace(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_PriceSignal(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Settings_Get_PriceSignal(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_PriceSignal(double Value)
{
    dss_capi.Settings_Set_PriceSignal(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_PriceCurve(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Settings_Get_PriceCurve(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_PriceCurve(BSTR Value)
{
    dss_capi.Settings_Set_PriceCurve(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_AllowChangeDir(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.DSS_Get_AllowChangeDir(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_AllowChangeDir(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.DSS_Set_AllowChangeDir(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_AllowDOScmd(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.DSS_Get_AllowDOScmd(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_AllowDOScmd(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.DSS_Set_AllowDOScmd(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_AllowEditor(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.DSS_Get_AllowEditor(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_AllowEditor(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.DSS_Set_AllowEditor(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_COMErrorResults(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.DSS_Get_COMErrorResults(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_COMErrorResults(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.DSS_Set_COMErrorResults(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_CompatFlags(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.DSS_Get_CompatFlags(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_CompatFlags(long Value)
{
    dss_capi.DSS_Set_CompatFlags(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_IterateDisabled(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Settings_Get_IterateDisabled(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_IterateDisabled(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Settings_Set_IterateDisabled(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_LoadsTerminalCheck(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Settings_Get_LoadsTerminalCheck(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_LoadsTerminalCheck(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Settings_Set_LoadsTerminalCheck(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::SetPropertyNameStyle(DSSPropertyNameStyle Value)
{
    dss_capi.Settings_SetPropertyNameStyle(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_SkipFileRegExp(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Settings_Get_SkipFileRegExp(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::put_SkipFileRegExp(BSTR Value)
{
    dss_capi.Settings_Set_SkipFileRegExp(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSettings::get_SkipCommands(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return dss_capi.Settings_Get_SkipCommandsStrs(dss_capi_ctx, Value);
}

STDMETHODIMP CSettings::put_SkipCommands(VARIANT Value)
{
    return dss_capi.Settings_Set_SkipCommandsStrs(dss_capi_ctx, Value);
}

