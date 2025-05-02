
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Generators.h : Declaration of CGenerators

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CGenerators
class ATL_NO_VTABLE CGenerators :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CGenerators, &CLSID_Generators>,
    public ISupportErrorInfo,
    public IDispatchImpl<IGenerators, &IID_IGenerators, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CGenerators, &IID_IGenerators>

{
public:
    CGenerators()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_GENERATORS)

BEGIN_COM_MAP(CGenerators)
    COM_INTERFACE_ENTRY(IGenerators)
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
    STDMETHOD(get_RegisterNames)(VARIANT* Value);
    STDMETHOD(get_RegisterValues)(VARIANT* Value);
    STDMETHOD(get_First)(long* Value);
    STDMETHOD(get_Next)(long* Value);
    STDMETHOD(get_ForcedON)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_ForcedON)(VARIANT_BOOL Value);
    STDMETHOD(get_Name)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Name)(BSTR Value);
    STDMETHOD(get_kV)(double* Value);
    STDMETHOD_(HRESULT, put_kV)(double Value);
    STDMETHOD(get_kW)(double* Value);
    STDMETHOD_(HRESULT, put_kW)(double Value);
    STDMETHOD(get_kvar)(double* Value);
    STDMETHOD_(HRESULT, put_kvar)(double Value);
    STDMETHOD(get_PF)(double* Value);
    STDMETHOD_(HRESULT, put_PF)(double Value);
    STDMETHOD(get_Phases)(long* Value);
    STDMETHOD_(HRESULT, put_Phases)(long Value);
    STDMETHOD(get_Count)(long* Value);
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
    STDMETHOD(get_Model)(long* Value);
    STDMETHOD_(HRESULT, put_Model)(long Value);
    STDMETHOD(get_kVArated)(double* Value);
    STDMETHOD_(HRESULT, put_kVArated)(double Value);
    STDMETHOD(get_Vmaxpu)(double* Value);
    STDMETHOD_(HRESULT, put_Vmaxpu)(double Value);
    STDMETHOD(get_Vminpu)(double* Value);
    STDMETHOD_(HRESULT, put_Vminpu)(double Value);
    STDMETHOD(get_Bus1)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Bus1)(BSTR Value);
    STDMETHOD(get_Class)(long* Value);
    STDMETHOD_(HRESULT, put_Class)(long Value);
    STDMETHOD(get_daily)(BSTR* Value);
    STDMETHOD_(HRESULT, put_daily)(BSTR Value);
    STDMETHOD(get_duty)(BSTR* Value);
    STDMETHOD_(HRESULT, put_duty)(BSTR Value);
    STDMETHOD(get_IsDelta)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_IsDelta)(VARIANT_BOOL Value);
    STDMETHOD(get_kva)(double* Value);
    STDMETHOD_(HRESULT, put_kva)(double Value);
    STDMETHOD(get_Status)(LoadStatus* Value);
    STDMETHOD_(HRESULT, put_Status)(LoadStatus Value);
    STDMETHOD(get_Yearly)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Yearly)(BSTR Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Generators), CGenerators)
