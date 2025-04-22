// Reactors.cpp : Implementation of CReactors

#include "pch.h"
#include "Reactors.h"


STDMETHODIMP CReactors::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IReactors
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

STDMETHODIMP CReactors::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reactors_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reactors_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reactors_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Reactors_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::put_Name(BSTR Value)
{
    dss_capi.Reactors_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::get_kV(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reactors_Get_kV(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::put_kV(double Value)
{
    dss_capi.Reactors_Set_kV(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::get_kvar(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reactors_Get_kvar(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::put_kvar(double Value)
{
    dss_capi.Reactors_Set_kvar(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::get_LCurve(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Reactors_Get_LCurve(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::put_LCurve(BSTR Value)
{
    dss_capi.Reactors_Set_LCurve(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::get_lmH(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reactors_Get_LmH(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::put_lmH(double Value)
{
    dss_capi.Reactors_Set_LmH(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::get_Parallel(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reactors_Get_Parallel(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::put_Parallel(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Reactors_Set_Parallel(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::get_R(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reactors_Get_R(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::put_R(double Value)
{
    dss_capi.Reactors_Set_R(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::get_RCurve(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Reactors_Get_RCurve(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::put_RCurve(BSTR Value)
{
    dss_capi.Reactors_Set_RCurve(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::get_Rmatrix(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Reactors_Get_Rmatrix_GR, Value);
}

STDMETHODIMP CReactors::put_Rmatrix(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.Reactors_Set_Rmatrix, Value);
}

STDMETHODIMP CReactors::get_Rp(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reactors_Get_Rp(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::put_Rp(double Value)
{
    dss_capi.Reactors_Set_Rp(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::get_x(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reactors_Get_X(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::put_x(double Value)
{
    dss_capi.Reactors_Set_X(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::get_Xmatrix(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Reactors_Get_Xmatrix_GR, Value);
}

STDMETHODIMP CReactors::put_Xmatrix(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.Reactors_Set_Xmatrix, Value);
}

STDMETHODIMP CReactors::get_Z(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Reactors_Get_Z_GR, Value);
}

STDMETHODIMP CReactors::put_Z(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.Reactors_Set_Z, Value);
}

STDMETHODIMP CReactors::get_Z0(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Reactors_Get_Z0_GR, Value);
}

STDMETHODIMP CReactors::put_Z0(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.Reactors_Set_Z0, Value);
}

STDMETHODIMP CReactors::get_Z1(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Reactors_Get_Z1_GR, Value);
}

STDMETHODIMP CReactors::put_Z1(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.Reactors_Set_Z1, Value);
}

STDMETHODIMP CReactors::get_Z2(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Reactors_Get_Z2_GR, Value);
}

STDMETHODIMP CReactors::put_Z2(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.Reactors_Set_Z2, Value);
}

STDMETHODIMP CReactors::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Reactors_Get_AllNames, Value);
}

STDMETHODIMP CReactors::get_Bus1(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Reactors_Get_Bus1(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::put_Bus1(BSTR Value)
{
    dss_capi.Reactors_Set_Bus1(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::get_Bus2(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Reactors_Get_Bus2(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::put_Bus2(BSTR Value)
{
    dss_capi.Reactors_Set_Bus2(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::get_IsDelta(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reactors_Get_IsDelta(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::put_IsDelta(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Reactors_Set_IsDelta(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::get_Phases(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Reactors_Get_Phases(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CReactors::put_Phases(long Value)
{
    dss_capi.Reactors_Set_Phases(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

