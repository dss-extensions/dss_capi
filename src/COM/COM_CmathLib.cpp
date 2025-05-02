// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_CmathLib.cpp : Implementation of CCmathLib

#include "COM_pch.h"
#include "COM_CmathLib.h"


STDMETHODIMP CCmathLib::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_ICmathLib
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

STDMETHODIMP CCmathLib::get_cmplx(double RealPart, double ImagPart, VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CmathLib_Get_cmplx_GR, Value, RealPart, ImagPart);
}

STDMETHODIMP CCmathLib::get_cabs(double RealPart, double ImagPart, double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CmathLib_Get_cabs(dss_capi_ctx, RealPart, ImagPart);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCmathLib::get_cdang(double RealPart, double ImagPart, double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CmathLib_Get_cdang(dss_capi_ctx, RealPart, ImagPart);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCmathLib::get_ctopolardeg(double RealPart, double ImagPart, VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CmathLib_Get_ctopolardeg_GR, Value, RealPart, ImagPart);
}

STDMETHODIMP CCmathLib::get_pdegtocomplex(double magnitude, double angle, VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CmathLib_Get_pdegtocomplex_GR, Value, magnitude, angle);
}

STDMETHODIMP CCmathLib::get_cmul(double a1, double b1, double a2, double b2, VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CmathLib_Get_cmul_GR, Value, a1, b1, a2, b2);
}

STDMETHODIMP CCmathLib::get_cdiv(double a1, double b1, double a2, double b2, VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CmathLib_Get_cdiv_GR, Value, a1, b1, a2, b2);
}

