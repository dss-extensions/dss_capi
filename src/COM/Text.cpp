// Text.cpp : Implementation of CText

#include "pch.h"
#include "Text.h"


STDMETHODIMP CText::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IText
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

STDMETHODIMP CText::get_Command(BSTR* Command)
{
    if (Command == nullptr)
    {
        return E_POINTER;
    }
    *Command = CComBSTR(dss_capi.Text_Get_Command(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CText::put_Command(BSTR Command)
{
    dss_capi.Text_Set_Command(dss_capi_ctx, AltDSS_COM_CString(Command));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CText::get_Result(BSTR* Result)
{
    if (Result == nullptr)
    {
        return E_POINTER;
    }
    *Result = CComBSTR(dss_capi.Text_Get_Result(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

