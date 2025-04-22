// Relays.cpp : Implementation of CRelays

#include "pch.h"
#include "Relays.h"


STDMETHODIMP CRelays::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IRelays
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

STDMETHODIMP CRelays::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Relays_Get_AllNames, Value);
}

STDMETHODIMP CRelays::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Relays_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Relays_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Relays_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Relays_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::put_Name(BSTR Value)
{
    dss_capi.Relays_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::get_MonitoredObj(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Relays_Get_MonitoredObj(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::put_MonitoredObj(BSTR Value)
{
    dss_capi.Relays_Set_MonitoredObj(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::get_MonitoredTerm(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Relays_Get_MonitoredTerm(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::put_MonitoredTerm(long Value)
{
    dss_capi.Relays_Set_MonitoredTerm(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::get_SwitchedObj(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Relays_Get_SwitchedObj(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::put_SwitchedObj(BSTR Value)
{
    dss_capi.Relays_Set_SwitchedObj(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::get_SwitchedTerm(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Relays_Get_SwitchedTerm(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::put_SwitchedTerm(long Value)
{
    dss_capi.Relays_Set_SwitchedTerm(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Relays_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::put_idx(long Value)
{
    dss_capi.Relays_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::Open()
{
    dss_capi.Relays_Open(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::Close()
{
    dss_capi.Relays_Close(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::Reset()
{
    dss_capi.Relays_Reset(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::get_State(ActionCodes* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = static_cast<ActionCodes>(dss_capi.Relays_Get_State(dss_capi_ctx));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::put_State(ActionCodes Value)
{
    dss_capi.Relays_Set_State(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::get_NormalState(ActionCodes* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = static_cast<ActionCodes>(dss_capi.Relays_Get_NormalState(dss_capi_ctx));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CRelays::put_NormalState(ActionCodes Value)
{
    dss_capi.Relays_Set_NormalState(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

