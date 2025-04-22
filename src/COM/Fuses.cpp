// Fuses.cpp : Implementation of CFuses

#include "pch.h"
#include "Fuses.h"


STDMETHODIMP CFuses::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IFuses
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

STDMETHODIMP CFuses::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Fuses_Get_AllNames, Value);
}

STDMETHODIMP CFuses::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Fuses_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Fuses_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Fuses_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Fuses_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::put_Name(BSTR Value)
{
    dss_capi.Fuses_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::get_MonitoredObj(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Fuses_Get_MonitoredObj(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::put_MonitoredObj(BSTR Value)
{
    dss_capi.Fuses_Set_MonitoredObj(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::get_MonitoredTerm(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Fuses_Get_MonitoredTerm(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::put_MonitoredTerm(long Value)
{
    dss_capi.Fuses_Set_MonitoredTerm(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::get_SwitchedObj(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Fuses_Get_SwitchedObj(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::put_SwitchedObj(BSTR Value)
{
    dss_capi.Fuses_Set_SwitchedObj(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::get_SwitchedTerm(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Fuses_Get_SwitchedTerm(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::put_SwitchedTerm(long Value)
{
    dss_capi.Fuses_Set_SwitchedTerm(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::get_TCCcurve(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Fuses_Get_TCCcurve(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::put_TCCcurve(BSTR Value)
{
    dss_capi.Fuses_Set_TCCcurve(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::get_RatedCurrent(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Fuses_Get_RatedCurrent(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::put_RatedCurrent(double Value)
{
    dss_capi.Fuses_Set_RatedCurrent(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::get_Delay(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Fuses_Get_Delay(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::put_Delay(double Value)
{
    dss_capi.Fuses_Set_Delay(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::Open()
{
    dss_capi.Fuses_Open(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::Close()
{
    dss_capi.Fuses_Close(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::IsBlown(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Fuses_IsBlown(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Fuses_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::put_idx(long Value)
{
    dss_capi.Fuses_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::get_NumPhases(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Fuses_Get_NumPhases(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::Reset()
{
    dss_capi.Fuses_Reset(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CFuses::get_State(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Fuses_Get_State, Value);
}

STDMETHODIMP CFuses::put_State(VARIANT Value)
{
    return AltDSS_COM_SetStrs(dss_capi.Fuses_Set_State, Value);
}

STDMETHODIMP CFuses::get_NormalState(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Fuses_Get_NormalState, Value);
}

STDMETHODIMP CFuses::put_NormalState(VARIANT Value)
{
    return AltDSS_COM_SetStrs(dss_capi.Fuses_Set_NormalState, Value);
}

