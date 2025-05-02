
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_DSS.h : Declaration of CDSS

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CDSS
class ATL_NO_VTABLE CDSS :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CDSS, &CLSID_DSS>,
    public ISupportErrorInfo,
    public IDispatchImpl<IDSS, &IID_IDSS, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CDSS, &IID_IDSS>

{
public:
    CDSS()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_DSS)

BEGIN_COM_MAP(CDSS)
    COM_INTERFACE_ENTRY(IDSS)
    COM_INTERFACE_ENTRY(IDispatch)
    COM_INTERFACE_ENTRY(ISupportErrorInfo)
END_COM_MAP()

    STDMETHOD(InterfaceSupportsErrorInfo)(REFIID riid);

    DECLARE_PROTECT_FINAL_CONSTRUCT()

    HRESULT FinalConstruct()
    {
        return AltDSSCAPIEx::AltDSS_COM_Init();
    }

    void FinalRelease()
    {
        AltDSSCAPIEx::AltDSS_COM_Dispose(dss_capi_ctx, dss_capi, true);
    }

public:
    STDMETHOD(get_NumCircuits)(long* Value);
    STDMETHOD_(HRESULT, get_Circuits)(VARIANT idx, ICircuit** Value);
    STDMETHOD(get_ActiveCircuit)(ICircuit** Value);
    STDMETHOD(get_Text)(IText** Value);
    STDMETHOD(get_Error)(IError** Value);
    STDMETHOD_(HRESULT, NewCircuit)(BSTR Name, ICircuit** Value);
    STDMETHOD(ClearAll)();
    STDMETHOD(ShowPanel)();
    STDMETHOD_(HRESULT, Start)(long Code, VARIANT_BOOL* Value);
    STDMETHOD(get_Version)(BSTR* Value);
    STDMETHOD(get_DSSProgress)(IDSSProgress** Value);
    STDMETHOD(get_Classes)(VARIANT* Value);
    STDMETHOD(get_UserClasses)(VARIANT* Value);
    STDMETHOD(get_NumClasses)(long* Value);
    STDMETHOD(get_NumUserClasses)(long* Value);
    STDMETHOD(get_DataPath)(BSTR* Value);
    STDMETHOD_(HRESULT, put_DataPath)(BSTR Value);
    STDMETHOD(Reset)();
    STDMETHOD(get_AllowForms)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_AllowForms)(VARIANT_BOOL Value);
    STDMETHOD(get_DefaultEditor)(BSTR* Value);
    STDMETHOD(get_ActiveClass)(IActiveClass** Value);
    STDMETHOD_(HRESULT, SetActiveClass)(BSTR ClassName, long* Value);
    STDMETHOD(get_Executive)(IDSS_Executive** Value);
    STDMETHOD(get_CmathLib)(ICmathLib** Value);
    STDMETHOD(get_Parser)(IParser** Value);
    STDMETHOD(get_DSSim_Coms)(IDSSimComs** Value);
    STDMETHOD(get_ZIP)(IZIP** Value);
    STDMETHOD(get_YMatrix)(IYMatrix** Value);
    STDMETHOD_(HRESULT, LoadOpenDSS)(BSTR dllpath, hyper liboptions);
    STDMETHOD_(HRESULT, LoadAltDSS)(BSTR dllpath, hyper liboptions);
};

OBJECT_ENTRY_AUTO(__uuidof(DSS), CDSS)
