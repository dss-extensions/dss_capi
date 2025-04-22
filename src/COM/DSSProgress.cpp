// DSSProgress.cpp : Implementation of CDSSProgress

#include "pch.h"
#include "DSSProgress.h"


STDMETHODIMP CDSSProgress::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IDSSProgress
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

STDMETHODIMP CDSSProgress::put_PctProgress(long rhs)
{
    dss_capi.DSSProgress_Set_PctProgress(dss_capi_ctx, static_cast<int32_t>(rhs));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSSProgress::put_Caption(BSTR rhs)
{
    dss_capi.DSSProgress_Set_Caption(dss_capi_ctx, AltDSS_COM_CString(rhs));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSSProgress::Show()
{
    dss_capi.DSSProgress_Show(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSSProgress::Close()
{
    dss_capi.DSSProgress_Close(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

