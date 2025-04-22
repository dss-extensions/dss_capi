// Vsources.cpp : Implementation of CVsources

#include "pch.h"
#include "Vsources.h"


STDMETHODIMP CVsources::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IVsources
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

STDMETHODIMP CVsources::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Vsources_Get_AllNames, Value);
}

STDMETHODIMP CVsources::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Vsources_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CVsources::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Vsources_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CVsources::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Vsources_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CVsources::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Vsources_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CVsources::put_Name(BSTR Value)
{
    dss_capi.Vsources_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CVsources::get_BasekV(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Vsources_Get_BasekV(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CVsources::put_BasekV(double Value)
{
    dss_capi.Vsources_Set_BasekV(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CVsources::get_pu(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Vsources_Get_pu(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CVsources::put_pu(double Value)
{
    dss_capi.Vsources_Set_pu(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CVsources::get_AngleDeg(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Vsources_Get_AngleDeg(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CVsources::put_AngleDeg(double Value)
{
    dss_capi.Vsources_Set_AngleDeg(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CVsources::get_Frequency(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Vsources_Get_Frequency(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CVsources::put_Frequency(double Value)
{
    dss_capi.Vsources_Set_Frequency(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CVsources::get_Phases(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Vsources_Get_Phases(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CVsources::put_Phases(long Value)
{
    dss_capi.Vsources_Set_Phases(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CVsources::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Vsources_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CVsources::put_idx(long Value)
{
    dss_capi.Vsources_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

