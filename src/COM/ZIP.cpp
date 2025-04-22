// ZIP.cpp : Implementation of CZIP

#include "pch.h"
#include "ZIP.h"


STDMETHODIMP CZIP::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IZIP
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

STDMETHODIMP CZIP::Open(BSTR FileName)
{
    dss_capi.ZIP_Open(dss_capi_ctx, AltDSS_COM_CString(FileName));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CZIP::Close()
{
    dss_capi.ZIP_Close(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CZIP::Redirect(BSTR FileName)
{
    dss_capi.ZIP_Redirect(dss_capi_ctx, AltDSS_COM_CString(FileName));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CZIP::Extract(BSTR FileName, VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetInt8s(dss_capi.ZIP_Extract_GR, Value, AltDSS_COM_CString(FileName));
}

STDMETHODIMP CZIP::List(BSTR regexp, VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.ZIP_List, Value, AltDSS_COM_CString(regexp));
}

STDMETHODIMP CZIP::Contains(BSTR Name, VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.ZIP_Contains(dss_capi_ctx, AltDSS_COM_CString(Name)) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

