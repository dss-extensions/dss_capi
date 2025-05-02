
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Vsources.h : Declaration of CVsources

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CVsources
class ATL_NO_VTABLE CVsources :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CVsources, &CLSID_Vsources>,
    public ISupportErrorInfo,
    public IDispatchImpl<IVsources, &IID_IVsources, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CVsources, &IID_IVsources>

{
public:
    CVsources()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_VSOURCES)

BEGIN_COM_MAP(CVsources)
    COM_INTERFACE_ENTRY(IVsources)
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
    STDMETHOD(get_AllNames)(VARIANT* Value);
    STDMETHOD(get_Count)(long* Value);
    STDMETHOD(get_First)(long* Value);
    STDMETHOD(get_Next)(long* Value);
    STDMETHOD(get_Name)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Name)(BSTR Value);
    STDMETHOD(get_BasekV)(double* Value);
    STDMETHOD_(HRESULT, put_BasekV)(double Value);
    STDMETHOD(get_pu)(double* Value);
    STDMETHOD_(HRESULT, put_pu)(double Value);
    STDMETHOD(get_AngleDeg)(double* Value);
    STDMETHOD_(HRESULT, put_AngleDeg)(double Value);
    STDMETHOD(get_Frequency)(double* Value);
    STDMETHOD_(HRESULT, put_Frequency)(double Value);
    STDMETHOD(get_Phases)(long* Value);
    STDMETHOD_(HRESULT, put_Phases)(long Value);
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Vsources), CVsources)
