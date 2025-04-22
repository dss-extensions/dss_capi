// WindGens.cpp : Implementation of CWindGens

#include "pch.h"
#include "WindGens.h"


STDMETHODIMP CWindGens::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IWindGens
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

STDMETHODIMP CWindGens::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.WindGens_Get_AllNames, Value);
}

STDMETHODIMP CWindGens::get_RegisterNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.WindGens_Get_RegisterNames, Value);
}

STDMETHODIMP CWindGens::get_RegisterValues(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.WindGens_Get_RegisterValues_GR, Value);
}

STDMETHODIMP CWindGens::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_Ag(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_Ag(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_Ag(double Value)
{
    dss_capi.WindGens_Set_Ag(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_idx(long Value)
{
    dss_capi.WindGens_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_Cp(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_Cp(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_Cp(double Value)
{
    dss_capi.WindGens_Set_Cp(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_kV(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_kV(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_kV(double Value)
{
    dss_capi.WindGens_Set_kV(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_kva(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_kVA(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_kva(double Value)
{
    dss_capi.WindGens_Set_kVA(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_kvar(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_kvar(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_kvar(double Value)
{
    dss_capi.WindGens_Set_kvar(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_kW(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_kW(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_kW(double Value)
{
    dss_capi.WindGens_Set_kW(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_Lamda(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_Lamda(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_Lamda(double Value)
{
    dss_capi.WindGens_Set_Lamda(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_N_WTG(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_N_WTG(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_N_WTG(long Value)
{
    dss_capi.WindGens_Set_N_WTG(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_NPoles(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_NPoles(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_NPoles(long Value)
{
    dss_capi.WindGens_Set_NPoles(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_pd(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_pd(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_pd(double Value)
{
    dss_capi.WindGens_Set_pd(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_PF(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_PF(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_PF(double Value)
{
    dss_capi.WindGens_Set_PF(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_PSS(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_PSS(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_PSS(double Value)
{
    dss_capi.WindGens_Set_PSS(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_QFlag(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_QFlag(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_QFlag(long Value)
{
    dss_capi.WindGens_Set_QFlag(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_QMode(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_QMode(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_QMode(long Value)
{
    dss_capi.WindGens_Set_QMode(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_QSS(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_QSS(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_QSS(double Value)
{
    dss_capi.WindGens_Set_QSS(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_Rad(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_Rad(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_Rad(double Value)
{
    dss_capi.WindGens_Set_Rad(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_RThev(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_RThev(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_RThev(double Value)
{
    dss_capi.WindGens_Set_RThev(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_VCutIn(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_VCutIn(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_VCutIn(double Value)
{
    dss_capi.WindGens_Set_VCutIn(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_VCutOut(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_VCutOut(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_VCutOut(double Value)
{
    dss_capi.WindGens_Set_VCutOut(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_Vss(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_Vss(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_Vss(double Value)
{
    dss_capi.WindGens_Set_Vss(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_WindSpeed(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_WindSpeed(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_WindSpeed(double Value)
{
    dss_capi.WindGens_Set_WindSpeed(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_XThev(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.WindGens_Get_XThev(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_XThev(double Value)
{
    dss_capi.WindGens_Set_XThev(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.WindGens_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CWindGens::put_Name(BSTR Value)
{
    dss_capi.WindGens_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

