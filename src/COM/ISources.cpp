// ISources.cpp : Implementation of CISources

#include "pch.h"
#include "ISources.h"


STDMETHODIMP CISources::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IISources
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

STDMETHODIMP CISources::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.ISources_Get_AllNames, Value);
}

STDMETHODIMP CISources::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.ISources_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CISources::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.ISources_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CISources::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.ISources_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CISources::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.ISources_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CISources::put_Name(BSTR Value)
{
    dss_capi.ISources_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CISources::get_Amps(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.ISources_Get_Amps(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CISources::put_Amps(double Value)
{
    dss_capi.ISources_Set_Amps(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CISources::get_AngleDeg(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.ISources_Get_AngleDeg(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CISources::put_AngleDeg(double Value)
{
    dss_capi.ISources_Set_AngleDeg(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CISources::get_Frequency(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.ISources_Get_Frequency(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CISources::put_Frequency(double Value)
{
    dss_capi.ISources_Set_Frequency(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CISources::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.ISources_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CISources::put_idx(long Value)
{
    dss_capi.ISources_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

