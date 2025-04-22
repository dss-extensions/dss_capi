// Generators.cpp : Implementation of CGenerators

#include "pch.h"
#include "Generators.h"


STDMETHODIMP CGenerators::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IGenerators
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

STDMETHODIMP CGenerators::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Generators_Get_AllNames, Value);
}

STDMETHODIMP CGenerators::get_RegisterNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Generators_Get_RegisterNames, Value);
}

STDMETHODIMP CGenerators::get_RegisterValues(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Generators_Get_RegisterValues_GR, Value);
}

STDMETHODIMP CGenerators::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Generators_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Generators_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_ForcedON(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Generators_Get_ForcedON(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::put_ForcedON(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Generators_Set_ForcedON(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Generators_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::put_Name(BSTR Value)
{
    dss_capi.Generators_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_kV(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Generators_Get_kV(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::put_kV(double Value)
{
    dss_capi.Generators_Set_kV(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_kW(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Generators_Get_kW(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::put_kW(double Value)
{
    dss_capi.Generators_Set_kW(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_kvar(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Generators_Get_kvar(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::put_kvar(double Value)
{
    dss_capi.Generators_Set_kvar(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_PF(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Generators_Get_PF(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::put_PF(double Value)
{
    dss_capi.Generators_Set_PF(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_Phases(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Generators_Get_Phases(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::put_Phases(long Value)
{
    dss_capi.Generators_Set_Phases(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Generators_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Generators_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::put_idx(long Value)
{
    dss_capi.Generators_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_Model(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Generators_Get_Model(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::put_Model(long Value)
{
    dss_capi.Generators_Set_Model(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_kVArated(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Generators_Get_kVArated(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::put_kVArated(double Value)
{
    dss_capi.Generators_Set_kVArated(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_Vmaxpu(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Generators_Get_Vmaxpu(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::put_Vmaxpu(double Value)
{
    dss_capi.Generators_Set_Vmaxpu(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_Vminpu(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Generators_Get_Vminpu(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::put_Vminpu(double Value)
{
    dss_capi.Generators_Set_Vminpu(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_Bus1(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Generators_Get_Bus1(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::put_Bus1(BSTR Value)
{
    dss_capi.Generators_Set_Bus1(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_Class(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Generators_Get_Class_(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::put_Class(long Value)
{
    dss_capi.Generators_Set_Class_(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_daily(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Generators_Get_daily(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::put_daily(BSTR Value)
{
    dss_capi.Generators_Set_daily(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_duty(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Generators_Get_duty(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::put_duty(BSTR Value)
{
    dss_capi.Generators_Set_duty(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_IsDelta(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Generators_Get_IsDelta(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::put_IsDelta(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Generators_Set_IsDelta(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_kva(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Generators_Get_kva(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::put_kva(double Value)
{
    dss_capi.Generators_Set_kva(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_Status(LoadStatus* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = static_cast<LoadStatus>(dss_capi.Generators_Get_Status(dss_capi_ctx));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::put_Status(LoadStatus Value)
{
    dss_capi.Generators_Set_Status(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::get_Yearly(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Generators_Get_Yearly(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CGenerators::put_Yearly(BSTR Value)
{
    dss_capi.Generators_Set_Yearly(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

