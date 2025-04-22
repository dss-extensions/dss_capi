// Monitors.cpp : Implementation of CMonitors

#include "pch.h"
#include "Monitors.h"


STDMETHODIMP CMonitors::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IMonitors
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

STDMETHODIMP CMonitors::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Monitors_Get_AllNames, Value);
}

STDMETHODIMP CMonitors::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Monitors_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Monitors_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::Reset()
{
    dss_capi.Monitors_Reset(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::ResetAll()
{
    dss_capi.Monitors_ResetAll(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::Sample()
{
    dss_capi.Monitors_Sample(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::Save()
{
    dss_capi.Monitors_Save(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::Show()
{
    dss_capi.Monitors_Show(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::get_FileName(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Monitors_Get_FileName(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::get_Mode(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Monitors_Get_Mode(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::put_Mode(long Value)
{
    dss_capi.Monitors_Set_Mode(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Monitors_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::put_Name(BSTR Value)
{
    dss_capi.Monitors_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::get_ByteStream(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetInt8s(dss_capi.Monitors_Get_ByteStream_GR, Value);
}

STDMETHODIMP CMonitors::get_SampleCount(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Monitors_Get_SampleCount(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::SampleAll()
{
    dss_capi.Monitors_SampleAll(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::SaveAll()
{
    dss_capi.Monitors_SaveAll(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Monitors_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::Process()
{
    dss_capi.Monitors_Process(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::ProcessAll()
{
    dss_capi.Monitors_ProcessAll(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::get_FileVersion(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Monitors_Get_FileVersion(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::get_RecordSize(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Monitors_Get_RecordSize(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::get_Header(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Monitors_Get_Header, Value);
}

STDMETHODIMP CMonitors::get_dblHour(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Monitors_Get_dblHour_GR, Value);
}

STDMETHODIMP CMonitors::get_dblFreq(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Monitors_Get_dblFreq_GR, Value);
}

STDMETHODIMP CMonitors::get_Channel(long Index, VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Monitors_Get_Channel_GR, Value, static_cast<int32_t>(Index));
}

STDMETHODIMP CMonitors::get_NumChannels(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Monitors_Get_NumChannels(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::get_Element(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Monitors_Get_Element(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::put_Element(BSTR Value)
{
    dss_capi.Monitors_Set_Element(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::get_Terminal(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Monitors_Get_Terminal(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::put_Terminal(long Value)
{
    dss_capi.Monitors_Set_Terminal(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Monitors_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMonitors::put_idx(long Value)
{
    dss_capi.Monitors_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

