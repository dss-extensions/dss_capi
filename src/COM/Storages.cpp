// Storages.cpp : Implementation of CStorages

#include "pch.h"
#include "Storages.h"


STDMETHODIMP CStorages::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IStorages
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

STDMETHODIMP CStorages::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Storages_Get_AllNames, Value);
}

STDMETHODIMP CStorages::get_RegisterNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Storages_Get_RegisterNames, Value);
}

STDMETHODIMP CStorages::get_RegisterValues(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Storages_Get_RegisterValues_GR, Value);
}

STDMETHODIMP CStorages::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_idx(long Value)
{
    dss_capi.Storages_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Storages_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_Name(BSTR Value)
{
    dss_capi.Storages_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_State(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_State(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_State(long Value)
{
    dss_capi.Storages_Set_State(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_puSOC(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_puSOC(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_puSOC(double Value)
{
    dss_capi.Storages_Set_puSOC(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_EffCharge(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_EffCharge(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_EffCharge(double Value)
{
    dss_capi.Storages_Set_EffCharge(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_EffDischarge(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_EffDischarge(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_EffDischarge(double Value)
{
    dss_capi.Storages_Set_EffDischarge(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_kWRated(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_kWRated(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_kWRated(double Value)
{
    dss_capi.Storages_Set_kWRated(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_ControlMode(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_ControlMode(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_ControlMode(long Value)
{
    dss_capi.Storages_Set_ControlMode(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_Kp(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_Kp(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_Kp(double Value)
{
    dss_capi.Storages_Set_Kp(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_kV(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_kV(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_kV(double Value)
{
    dss_capi.Storages_Set_kV(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_kva(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_kVA(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_kva(double Value)
{
    dss_capi.Storages_Set_kVA(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_kvar(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_kvar(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_kvar(double Value)
{
    dss_capi.Storages_Set_kvar(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_kWhRated(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_kWhRated(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_kWhRated(double Value)
{
    dss_capi.Storages_Set_kWhRated(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_LimitCurrent(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_LimitCurrent(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_LimitCurrent(double Value)
{
    dss_capi.Storages_Set_LimitCurrent(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_PF(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_PF(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_PF(double Value)
{
    dss_capi.Storages_Set_PF(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_SafeMode(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_SafeMode(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_SafeVoltage(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_SafeVoltage(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_SafeVoltage(double Value)
{
    dss_capi.Storages_Set_SafeVoltage(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_AmpLimit(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_AmpLimit(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_AmpLimit(double Value)
{
    dss_capi.Storages_Set_AmpLimit(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_AmpLimitGain(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_AmpLimitGain(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_AmpLimitGain(double Value)
{
    dss_capi.Storages_Set_AmpLimitGain(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_kVDC(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_kVDC(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_kVDC(double Value)
{
    dss_capi.Storages_Set_kVDC(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_kW(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_kW(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_kW(double Value)
{
    dss_capi.Storages_Set_kW(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_PITol(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_PITol(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_PITol(double Value)
{
    dss_capi.Storages_Set_PITol(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_ChargeTrigger(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_ChargeTrigger(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_ChargeTrigger(double Value)
{
    dss_capi.Storages_Set_ChargeTrigger(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_DischargeTrigger(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_DischargeTrigger(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_DischargeTrigger(double Value)
{
    dss_capi.Storages_Set_DischargeTrigger(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_TimeChargeTrig(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_TimeChargeTrig(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_TimeChargeTrig(double Value)
{
    dss_capi.Storages_Set_TimeChargeTrig(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::get_VarFollowInverter(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Storages_Get_VarFollowInverter(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CStorages::put_VarFollowInverter(long Value)
{
    dss_capi.Storages_Set_VarFollowInverter(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

