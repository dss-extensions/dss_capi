// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Lines.cpp : Implementation of CLines

#include "COM_pch.h"
#include "COM_Lines.h"


STDMETHODIMP CLines::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_ILines
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

STDMETHODIMP CLines::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Lines_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_Name(BSTR Value)
{
    dss_capi.Lines_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Lines_Get_AllNames, Value);
}

STDMETHODIMP CLines::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::New(BSTR Name, long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_New(dss_capi_ctx, AltDSS_COM_CString(Name));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_Bus1(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Lines_Get_Bus1(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_Bus1(BSTR Value)
{
    dss_capi.Lines_Set_Bus1(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_Bus2(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Lines_Get_Bus2(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_Bus2(BSTR Value)
{
    dss_capi.Lines_Set_Bus2(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_LineCode(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Lines_Get_LineCode(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_LineCode(BSTR Value)
{
    dss_capi.Lines_Set_LineCode(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_Length(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_Length(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_Length(double Value)
{
    dss_capi.Lines_Set_Length(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_Phases(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_Phases(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_Phases(long Value)
{
    dss_capi.Lines_Set_Phases(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_R1(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_R1(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_R1(double Value)
{
    dss_capi.Lines_Set_R1(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_X1(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_X1(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_X1(double Value)
{
    dss_capi.Lines_Set_X1(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_R0(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_R0(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_R0(double Value)
{
    dss_capi.Lines_Set_R0(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_X0(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_X0(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_X0(double Value)
{
    dss_capi.Lines_Set_X0(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_C1(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_C1(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_C1(double Value)
{
    dss_capi.Lines_Set_C1(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_C0(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_C0(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_C0(double Value)
{
    dss_capi.Lines_Set_C0(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_Rmatrix(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Lines_Get_Rmatrix_GR, Value);
}

STDMETHODIMP CLines::put_Rmatrix(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.Lines_Set_Rmatrix, Value);
}

STDMETHODIMP CLines::get_Xmatrix(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Lines_Get_Xmatrix_GR, Value);
}

STDMETHODIMP CLines::put_Xmatrix(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.Lines_Set_Xmatrix, Value);
}

STDMETHODIMP CLines::get_Cmatrix(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Lines_Get_Cmatrix_GR, Value);
}

STDMETHODIMP CLines::put_Cmatrix(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.Lines_Set_Cmatrix, Value);
}

STDMETHODIMP CLines::get_NormAmps(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_NormAmps(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_NormAmps(double Value)
{
    dss_capi.Lines_Set_NormAmps(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_EmergAmps(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_EmergAmps(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_EmergAmps(double Value)
{
    dss_capi.Lines_Set_EmergAmps(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_Geometry(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Lines_Get_Geometry(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_Geometry(BSTR Value)
{
    dss_capi.Lines_Set_Geometry(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_Rg(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_Rg(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_Rg(double Value)
{
    dss_capi.Lines_Set_Rg(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_Xg(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_Xg(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_Xg(double Value)
{
    dss_capi.Lines_Set_Xg(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_Rho(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_Rho(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_Rho(double Value)
{
    dss_capi.Lines_Set_Rho(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_Yprim(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Lines_Get_Yprim_GR, Value);
}

STDMETHODIMP CLines::put_Yprim(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.Lines_Set_Yprim, Value);
}

STDMETHODIMP CLines::get_NumCust(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_NumCust(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_TotalCust(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_TotalCust(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_Parent(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_Parent(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_Spacing(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Lines_Get_Spacing(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_Spacing(BSTR Value)
{
    dss_capi.Lines_Set_Spacing(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_Units(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_Units(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_Units(long Value)
{
    dss_capi.Lines_Set_Units(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_SeasonRating(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_SeasonRating(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_IsSwitch(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_IsSwitch(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_IsSwitch(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Lines_Set_IsSwitch(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Lines_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLines::put_idx(long Value)
{
    dss_capi.Lines_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

