// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_DSS.cpp : Implementation of CDSS

#include "COM_pch.h"
#include "COM_DSS.h"

CComPtr<IActiveClass> FActiveClass;
CComPtr<ICircuit> FCircuit;
CComPtr<ICmathLib> FCmathLib;
CComPtr<IDSSProgress> FDSSProgress;
CComPtr<IDSS_Executive> FDSS_Executive;
CComPtr<IDSSimComs> FDSSimComs;
CComPtr<IError> FError;
CComPtr<IParser> FParser;
CComPtr<IText> FText;
CComPtr<IYMatrix> FYMatrix;
CComPtr<IZIP> FZIP;


STDMETHODIMP CDSS::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IDSS
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

STDMETHODIMP CDSS::get_NumCircuits(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.DSS_Get_NumCircuits(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSS::get_Circuits(VARIANT idx, ICircuit** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    char* buffer;
    size_t buffer_size;
    switch (idx.vt)
    {
        case VT_I2:
        case VT_I4:
        {
            return E_NOTIMPL;
        }
        case VT_BSTR:
        {
            buffer_size = SysStringLen(idx.bstrVal) * 2 + 20;
            buffer = new char[buffer_size + 1];
            snprintf(buffer, buffer_size, "set circuit=%s", AltDSS_COM_CString(idx.bstrVal).c_str);
            dss_capi.Text_Set_Command(dss_capi_ctx, buffer);
            delete[] buffer;
            break;
        }
        default:
        {
            return E_INVALIDARG;
        }
    }
    return AltDSS_COM_CheckError();

}

STDMETHODIMP CDSS::get_ActiveCircuit(ICircuit** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Circuit, Value, FCircuit);
}

STDMETHODIMP CDSS::get_Text(IText** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Text, Value, FText);
}

STDMETHODIMP CDSS::get_Error(IError** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Error, Value, FError);
}

STDMETHODIMP CDSS::NewCircuit(BSTR Name, ICircuit** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    dss_capi.DSS_NewCircuit(dss_capi_ctx, AltDSS_COM_CString(Name));
    HRESULT result = AltDSS_COM_CheckError();
    if (FAILED(result))
    {
        return result;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Circuit, Value, FCircuit, result);
}

STDMETHODIMP CDSS::ClearAll()
{
    dss_capi.DSS_ClearAll(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSS::ShowPanel()
{
    dss_capi.DSS_ShowPanel(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSS::Start(long Code, VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.DSS_Start(dss_capi_ctx, static_cast<int32_t>(Code)) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSS::get_Version(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.DSS_Get_Version(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSS::get_DSSProgress(IDSSProgress** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_DSSProgress, Value, FDSSProgress);
}

STDMETHODIMP CDSS::get_Classes(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.DSS_Get_Classes, Value);
}

STDMETHODIMP CDSS::get_UserClasses(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.DSS_Get_UserClasses, Value);
}

STDMETHODIMP CDSS::get_NumClasses(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.DSS_Get_NumClasses(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSS::get_NumUserClasses(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.DSS_Get_NumUserClasses(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSS::get_DataPath(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.DSS_Get_DataPath(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSS::put_DataPath(BSTR Value)
{
    dss_capi.DSS_Set_DataPath(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSS::Reset()
{
    dss_capi.DSS_Reset(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSS::get_AllowForms(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.DSS_Get_AllowForms(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSS::put_AllowForms(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.DSS_Set_AllowForms(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSS::get_DefaultEditor(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.DSS_Get_DefaultEditor(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSS::get_ActiveClass(IActiveClass** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_ActiveClass, Value, FActiveClass);
}

STDMETHODIMP CDSS::SetActiveClass(BSTR ClassName, long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.DSS_SetActiveClass(dss_capi_ctx, AltDSS_COM_CString(ClassName));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CDSS::get_Executive(IDSS_Executive** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_DSS_Executive, Value, FDSS_Executive);
}

STDMETHODIMP CDSS::get_CmathLib(ICmathLib** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_CmathLib, Value, FCmathLib);
}

STDMETHODIMP CDSS::get_Parser(IParser** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Parser, Value, FParser);
}

STDMETHODIMP CDSS::get_DSSim_Coms(IDSSimComs** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_DSSimComs, Value, FDSSimComs);
}

STDMETHODIMP CDSS::get_ZIP(IZIP** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_ZIP, Value, FZIP);
}

STDMETHODIMP CDSS::get_YMatrix(IYMatrix** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_YMatrix, Value, FYMatrix);
}

STDMETHODIMP CDSS::LoadOpenDSS(BSTR dllpath, hyper liboptions)
{
    return dss_capi.DSS_LoadOpenDSS(dss_capi_ctx, AltDSS_COM_CString(dllpath), static_cast<uint64_t>(liboptions));
}

STDMETHODIMP CDSS::LoadAltDSS(BSTR dllpath, hyper liboptions)
{
    return dss_capi.DSS_LoadAltDSS(dss_capi_ctx, AltDSS_COM_CString(dllpath), static_cast<uint64_t>(liboptions));
}

