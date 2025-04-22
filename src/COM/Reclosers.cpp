// Reclosers.cpp : Implementation of CReclosers

#include "pch.h"
#include "Reclosers.h"


STDMETHODIMP CReclosers::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IReclosers
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

STDMETHODIMP CReclosers::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Reclosers_Get_AllNames, Value);
}

STDMETHODIMP CReclosers::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reclosers_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reclosers_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reclosers_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Reclosers_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::put_Name(BSTR Value)
{
    dss_capi.Reclosers_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::get_MonitoredObj(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Reclosers_Get_MonitoredObj(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::put_MonitoredObj(BSTR Value)
{
    dss_capi.Reclosers_Set_MonitoredObj(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::get_MonitoredTerm(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reclosers_Get_MonitoredTerm(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::put_MonitoredTerm(long Value)
{
    dss_capi.Reclosers_Set_MonitoredTerm(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::get_SwitchedObj(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Reclosers_Get_SwitchedObj(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::put_SwitchedObj(BSTR Value)
{
    dss_capi.Reclosers_Set_SwitchedObj(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::get_SwitchedTerm(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reclosers_Get_SwitchedTerm(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::put_SwitchedTerm(long Value)
{
    dss_capi.Reclosers_Set_SwitchedTerm(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::get_NumFast(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reclosers_Get_NumFast(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::put_NumFast(long Value)
{
    dss_capi.Reclosers_Set_NumFast(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::get_Shots(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reclosers_Get_Shots(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::put_Shots(long Value)
{
    dss_capi.Reclosers_Set_Shots(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::get_RecloseIntervals(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Reclosers_Get_RecloseIntervals_GR, Value);
}

STDMETHODIMP CReclosers::get_PhaseTrip(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reclosers_Get_PhaseTrip(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::put_PhaseTrip(double Value)
{
    dss_capi.Reclosers_Set_PhaseTrip(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::get_PhaseInst(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reclosers_Get_PhaseInst(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::put_PhaseInst(double Value)
{
    dss_capi.Reclosers_Set_PhaseInst(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::get_GroundTrip(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reclosers_Get_GroundTrip(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::put_GroundTrip(double Value)
{
    dss_capi.Reclosers_Set_GroundTrip(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::get_GroundInst(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reclosers_Get_GroundInst(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::put_GroundInst(double Value)
{
    dss_capi.Reclosers_Set_GroundInst(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::Open()
{
    dss_capi.Reclosers_Open(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::Close()
{
    dss_capi.Reclosers_Close(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reclosers_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::put_idx(long Value)
{
    dss_capi.Reclosers_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::Reset()
{
    dss_capi.Reclosers_Reset(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::get_NormalState(ActionCodes* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = static_cast<ActionCodes>(dss_capi.Reclosers_Get_NormalState(dss_capi_ctx));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::put_NormalState(ActionCodes Value)
{
    dss_capi.Reclosers_Set_NormalState(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::get_State(ActionCodes* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = static_cast<ActionCodes>(dss_capi.Reclosers_Get_State(dss_capi_ctx));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReclosers::put_State(ActionCodes Value)
{
    dss_capi.Reclosers_Set_State(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

