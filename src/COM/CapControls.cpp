// CapControls.cpp : Implementation of CCapControls

#include "pch.h"
#include "CapControls.h"


STDMETHODIMP CCapControls::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_ICapControls
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

STDMETHODIMP CCapControls::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.CapControls_Get_AllNames, Value);
}

STDMETHODIMP CCapControls::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.CapControls_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::put_Name(BSTR Value)
{
    dss_capi.CapControls_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CapControls_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CapControls_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::get_Mode(CapControlModes* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = static_cast<CapControlModes>(dss_capi.CapControls_Get_Mode(dss_capi_ctx));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::put_Mode(CapControlModes Value)
{
    dss_capi.CapControls_Set_Mode(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::get_Capacitor(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.CapControls_Get_Capacitor(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::put_Capacitor(BSTR Value)
{
    dss_capi.CapControls_Set_Capacitor(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::get_MonitoredObj(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.CapControls_Get_MonitoredObj(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::put_MonitoredObj(BSTR Value)
{
    dss_capi.CapControls_Set_MonitoredObj(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::get_MonitoredTerm(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CapControls_Get_MonitoredTerm(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::put_MonitoredTerm(long Value)
{
    dss_capi.CapControls_Set_MonitoredTerm(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::get_CTratio(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CapControls_Get_CTratio(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::put_CTratio(double Value)
{
    dss_capi.CapControls_Set_CTratio(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::get_PTratio(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CapControls_Get_PTratio(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::put_PTratio(double Value)
{
    dss_capi.CapControls_Set_PTratio(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::get_ONSetting(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CapControls_Get_ONSetting(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::put_ONSetting(double Value)
{
    dss_capi.CapControls_Set_ONSetting(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::get_OFFSetting(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CapControls_Get_OFFSetting(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::put_OFFSetting(double Value)
{
    dss_capi.CapControls_Set_OFFSetting(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::get_Vmax(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CapControls_Get_Vmax(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::put_Vmax(double Value)
{
    dss_capi.CapControls_Set_Vmax(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::get_Vmin(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CapControls_Get_Vmin(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::put_Vmin(double Value)
{
    dss_capi.CapControls_Set_Vmin(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::get_UseVoltOverride(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CapControls_Get_UseVoltOverride(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::put_UseVoltOverride(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.CapControls_Set_UseVoltOverride(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::get_Delay(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CapControls_Get_Delay(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::put_Delay(double Value)
{
    dss_capi.CapControls_Set_Delay(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::get_DelayOff(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CapControls_Get_DelayOff(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::put_DelayOff(double Value)
{
    dss_capi.CapControls_Set_DelayOff(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::get_DeadTime(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CapControls_Get_DeadTime(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::put_DeadTime(double Value)
{
    dss_capi.CapControls_Set_DeadTime(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CapControls_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::Reset()
{
    dss_capi.CapControls_Reset(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CapControls_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCapControls::put_idx(long Value)
{
    dss_capi.CapControls_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

