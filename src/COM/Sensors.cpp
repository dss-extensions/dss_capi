// Sensors.cpp : Implementation of CSensors

#include "pch.h"
#include "Sensors.h"


STDMETHODIMP CSensors::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_ISensors
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

STDMETHODIMP CSensors::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Sensors_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::put_Name(BSTR Value)
{
    dss_capi.Sensors_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Sensors_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Sensors_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Sensors_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Sensors_Get_AllNames, Value);
}

STDMETHODIMP CSensors::get_IsDelta(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Sensors_Get_IsDelta(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::put_IsDelta(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Sensors_Set_IsDelta(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::get_ReverseDelta(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Sensors_Get_ReverseDelta(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::put_ReverseDelta(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Sensors_Set_ReverseDelta(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::get_PctError(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Sensors_Get_PctError(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::put_PctError(double Value)
{
    dss_capi.Sensors_Set_PctError(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::get_Weight(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Sensors_Get_Weight(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::put_Weight(double Value)
{
    dss_capi.Sensors_Set_Weight(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::get_MeteredElement(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Sensors_Get_MeteredElement(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::put_MeteredElement(BSTR Value)
{
    dss_capi.Sensors_Set_MeteredElement(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::get_MeteredTerminal(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Sensors_Get_MeteredTerminal(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::put_MeteredTerminal(long Value)
{
    dss_capi.Sensors_Set_MeteredTerminal(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::Reset()
{
    dss_capi.Sensors_Reset(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::ResetAll()
{
    dss_capi.Sensors_ResetAll(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::get_kVBase(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Sensors_Get_kVbase(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::put_kVBase(double Value)
{
    dss_capi.Sensors_Set_kVbase(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::get_Currents(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Sensors_Get_Currents_GR, Value);
}

STDMETHODIMP CSensors::put_Currents(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.Sensors_Set_Currents, Value);
}

STDMETHODIMP CSensors::get_kVS(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Sensors_Get_kVS_GR, Value);
}

STDMETHODIMP CSensors::put_kVS(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.Sensors_Set_kVS, Value);
}

STDMETHODIMP CSensors::get_kVARS(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Sensors_Get_kVARS_GR, Value);
}

STDMETHODIMP CSensors::put_kVARS(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.Sensors_Set_kVARS, Value);
}

STDMETHODIMP CSensors::get_kWS(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Sensors_Get_kWS_GR, Value);
}

STDMETHODIMP CSensors::put_kWS(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.Sensors_Set_kWS, Value);
}

STDMETHODIMP CSensors::get_AllocationFactor(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Sensors_Get_AllocationFactor_GR, Value);
}

STDMETHODIMP CSensors::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Sensors_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSensors::put_idx(long Value)
{
    dss_capi.Sensors_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

