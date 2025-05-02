// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_LoadShapes.cpp : Implementation of CLoadShapes

#include "COM_pch.h"
#include "COM_LoadShapes.h"


STDMETHODIMP CLoadShapes::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_ILoadShapes
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

STDMETHODIMP CLoadShapes::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.LoadShapes_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::put_Name(BSTR Value)
{
    dss_capi.LoadShapes_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LoadShapes_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LoadShapes_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LoadShapes_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.LoadShapes_Get_AllNames, Value);
}

STDMETHODIMP CLoadShapes::get_Npts(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LoadShapes_Get_Npts(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::put_Npts(long Value)
{
    dss_capi.LoadShapes_Set_Npts(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::get_Pmult(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.LoadShapes_Get_Pmult_GR, Value);
}

STDMETHODIMP CLoadShapes::put_Pmult(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.LoadShapes_Set_Pmult, Value);
}

STDMETHODIMP CLoadShapes::get_Qmult(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.LoadShapes_Get_Qmult_GR, Value);
}

STDMETHODIMP CLoadShapes::put_Qmult(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.LoadShapes_Set_Qmult, Value);
}

STDMETHODIMP CLoadShapes::Normalize()
{
    dss_capi.LoadShapes_Normalize(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::get_TimeArray(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.LoadShapes_Get_TimeArray_GR, Value);
}

STDMETHODIMP CLoadShapes::put_TimeArray(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.LoadShapes_Set_TimeArray, Value);
}

STDMETHODIMP CLoadShapes::get_HrInterval(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LoadShapes_Get_HrInterval(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::put_HrInterval(double Value)
{
    dss_capi.LoadShapes_Set_HrInterval(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::get_MinInterval(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LoadShapes_Get_MinInterval(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::put_MinInterval(double Value)
{
    dss_capi.LoadShapes_Set_MinInterval(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::New(BSTR Name, long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LoadShapes_New(dss_capi_ctx, AltDSS_COM_CString(Name));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::get_Pbase(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LoadShapes_Get_PBase(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::put_Pbase(double Value)
{
    dss_capi.LoadShapes_Set_PBase(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::get_Qbase(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LoadShapes_Get_Qbase(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::put_Qbase(double Value)
{
    dss_capi.LoadShapes_Set_Qbase(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::get_UseActual(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LoadShapes_Get_UseActual(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::put_UseActual(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.LoadShapes_Set_UseActual(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::get_Sinterval(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LoadShapes_Get_SInterval(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::put_Sinterval(double Value)
{
    dss_capi.LoadShapes_Set_SInterval(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LoadShapes_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::put_idx(long Value)
{
    dss_capi.LoadShapes_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::UseFloat32()
{
    dss_capi.LoadShapes_UseFloat32(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLoadShapes::UseFloat64()
{
    dss_capi.LoadShapes_UseFloat64(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

