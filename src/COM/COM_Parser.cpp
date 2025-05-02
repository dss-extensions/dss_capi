// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Parser.cpp : Implementation of CParser

#include "COM_pch.h"
#include "COM_Parser.h"


STDMETHODIMP CParser::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IParser
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

STDMETHODIMP CParser::get_CmdString(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Parser_Get_CmdString(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParser::put_CmdString(BSTR Value)
{
    dss_capi.Parser_Set_CmdString(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParser::get_NextParam(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Parser_Get_NextParam(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParser::get_AutoIncrement(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Parser_Get_AutoIncrement(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParser::put_AutoIncrement(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Parser_Set_AutoIncrement(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParser::get_DblValue(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Parser_Get_DblValue(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParser::get_IntValue(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Parser_Get_IntValue(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParser::get_StrValue(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Parser_Get_StrValue(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParser::get_WhiteSpace(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Parser_Get_WhiteSpace(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParser::put_WhiteSpace(BSTR Value)
{
    dss_capi.Parser_Set_WhiteSpace(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParser::get_BeginQuote(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Parser_Get_BeginQuote(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParser::put_BeginQuote(BSTR Value)
{
    dss_capi.Parser_Set_BeginQuote(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParser::get_EndQuote(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Parser_Get_EndQuote(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParser::put_EndQuote(BSTR Value)
{
    dss_capi.Parser_Set_EndQuote(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParser::get_Delimiters(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Parser_Get_Delimiters(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParser::put_Delimiters(BSTR Value)
{
    dss_capi.Parser_Set_Delimiters(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParser::ResetDelimiters()
{
    dss_capi.Parser_ResetDelimiters(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParser::get_Vector(long ExpectedSize, VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Parser_Get_Vector_GR, Value, static_cast<int32_t>(ExpectedSize));
}

STDMETHODIMP CParser::get_Matrix(long ExpectedOrder, VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Parser_Get_Matrix_GR, Value, static_cast<int32_t>(ExpectedOrder));
}

STDMETHODIMP CParser::get_SymMatrix(long ExpectedOrder, VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Parser_Get_SymMatrix_GR, Value, static_cast<int32_t>(ExpectedOrder));
}

