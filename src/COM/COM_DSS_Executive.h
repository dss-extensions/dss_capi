
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_DSS_Executive.h : Declaration of CDSS_Executive

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CDSS_Executive
class ATL_NO_VTABLE CDSS_Executive :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CDSS_Executive, &CLSID_DSS_Executive>,
    public ISupportErrorInfo,
    public IDispatchImpl<IDSS_Executive, &IID_IDSS_Executive, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CDSS_Executive, &IID_IDSS_Executive>

{
public:
    CDSS_Executive()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_DSS_EXECUTIVE)

BEGIN_COM_MAP(CDSS_Executive)
    COM_INTERFACE_ENTRY(IDSS_Executive)
    COM_INTERFACE_ENTRY(IDispatch)
    COM_INTERFACE_ENTRY(ISupportErrorInfo)
END_COM_MAP()

    STDMETHOD(InterfaceSupportsErrorInfo)(REFIID riid);

    DECLARE_PROTECT_FINAL_CONSTRUCT()

    HRESULT FinalConstruct()
    {
        return AltDSS_COM_CheckError();
    }

    void FinalRelease()
    {
        return;
    }

public:
    STDMETHOD(get_NumCommands)(long* Value);
    STDMETHOD(get_NumOptions)(long* Value);
    STDMETHOD_(HRESULT, get_Command)(long i, BSTR* Value);
    STDMETHOD_(HRESULT, get_Option)(long i, BSTR* Value);
    STDMETHOD_(HRESULT, get_CommandHelp)(long i, BSTR* Value);
    STDMETHOD_(HRESULT, get_OptionHelp)(long i, BSTR* Value);
    STDMETHOD_(HRESULT, get_OptionValue)(long i, BSTR* Value);
};

OBJECT_ENTRY_AUTO(__uuidof(DSS_Executive), CDSS_Executive)
