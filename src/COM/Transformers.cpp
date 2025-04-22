// Transformers.cpp : Implementation of CTransformers

#include "pch.h"
#include "Transformers.h"


STDMETHODIMP CTransformers::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_ITransformers
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

STDMETHODIMP CTransformers::get_NumWindings(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_NumWindings(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::put_NumWindings(long Value)
{
    dss_capi.Transformers_Set_NumWindings(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_XfmrCode(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Transformers_Get_XfmrCode(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::put_XfmrCode(BSTR Value)
{
    dss_capi.Transformers_Set_XfmrCode(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_Wdg(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_Wdg(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::put_Wdg(long Value)
{
    dss_capi.Transformers_Set_Wdg(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_R(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_R(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::put_R(double Value)
{
    dss_capi.Transformers_Set_R(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_Tap(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_Tap(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::put_Tap(double Value)
{
    dss_capi.Transformers_Set_Tap(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_MinTap(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_MinTap(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::put_MinTap(double Value)
{
    dss_capi.Transformers_Set_MinTap(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_MaxTap(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_MaxTap(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::put_MaxTap(double Value)
{
    dss_capi.Transformers_Set_MaxTap(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_NumTaps(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_NumTaps(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::put_NumTaps(long Value)
{
    dss_capi.Transformers_Set_NumTaps(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_kV(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_kV(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::put_kV(double Value)
{
    dss_capi.Transformers_Set_kV(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_kva(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_kVA(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::put_kva(double Value)
{
    dss_capi.Transformers_Set_kVA(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_Xneut(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_Xneut(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::put_Xneut(double Value)
{
    dss_capi.Transformers_Set_Xneut(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_Rneut(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_Rneut(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::put_Rneut(double Value)
{
    dss_capi.Transformers_Set_Rneut(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_IsDelta(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_IsDelta(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::put_IsDelta(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Transformers_Set_IsDelta(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_Xhl(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_Xhl(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::put_Xhl(double Value)
{
    dss_capi.Transformers_Set_Xhl(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_Xht(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_Xht(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::put_Xht(double Value)
{
    dss_capi.Transformers_Set_Xht(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_Xlt(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_Xlt(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::put_Xlt(double Value)
{
    dss_capi.Transformers_Set_Xlt(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Transformers_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::put_Name(BSTR Value)
{
    dss_capi.Transformers_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Transformers_Get_AllNames, Value);
}

STDMETHODIMP CTransformers::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_WdgVoltages(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Transformers_Get_WdgVoltages_GR, Value);
}

STDMETHODIMP CTransformers::get_WdgCurrents(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Transformers_Get_WdgCurrents_GR, Value);
}

STDMETHODIMP CTransformers::get_strWdgCurrents(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Transformers_Get_strWdgCurrents(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_CoreType(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_CoreType(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::put_CoreType(long Value)
{
    dss_capi.Transformers_Set_CoreType(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_RdcOhms(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_RdcOhms(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::put_RdcOhms(double Value)
{
    dss_capi.Transformers_Set_RdcOhms(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Transformers_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::put_idx(long Value)
{
    dss_capi.Transformers_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTransformers::get_LossesByType(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Transformers_Get_LossesByType_GR, Value);
}

STDMETHODIMP CTransformers::get_AllLossesByType(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Transformers_Get_AllLossesByType_GR, Value);
}

