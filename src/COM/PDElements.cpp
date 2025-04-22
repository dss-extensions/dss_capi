// PDElements.cpp : Implementation of CPDElements

#include "pch.h"
#include "PDElements.h"


STDMETHODIMP CPDElements::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IPDElements
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

STDMETHODIMP CPDElements::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PDElements_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPDElements::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PDElements_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPDElements::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PDElements_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPDElements::get_IsShunt(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PDElements_Get_IsShunt(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPDElements::get_FaultRate(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PDElements_Get_FaultRate(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPDElements::put_FaultRate(double Value)
{
    dss_capi.PDElements_Set_FaultRate(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPDElements::get_pctPermanent(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PDElements_Get_pctPermanent(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPDElements::put_pctPermanent(double Value)
{
    dss_capi.PDElements_Set_pctPermanent(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPDElements::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.PDElements_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPDElements::put_Name(BSTR Value)
{
    dss_capi.PDElements_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPDElements::get_Lambda(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PDElements_Get_Lambda(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPDElements::get_AccumulatedL(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PDElements_Get_AccumulatedL(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPDElements::get_Numcustomers(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PDElements_Get_Numcustomers(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPDElements::get_TotalCustomers(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PDElements_Get_Totalcustomers(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPDElements::get_ParentPDElement(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PDElements_Get_ParentPDElement(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPDElements::get_FromTerminal(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PDElements_Get_FromTerminal(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPDElements::get_TotalMiles(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PDElements_Get_TotalMiles(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPDElements::get_SectionID(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PDElements_Get_SectionID(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPDElements::get_RepairTime(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.PDElements_Get_RepairTime(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CPDElements::put_RepairTime(double Value)
{
    dss_capi.PDElements_Set_RepairTime(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

