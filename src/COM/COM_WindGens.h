
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_WindGens.h : Declaration of CWindGens

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CWindGens
class ATL_NO_VTABLE CWindGens :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CWindGens, &CLSID_WindGens>,
    public ISupportErrorInfo,
    public IDispatchImpl<IWindGens, &IID_IWindGens, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CWindGens, &IID_IWindGens>

{
public:
    CWindGens()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_WINDGENS)

BEGIN_COM_MAP(CWindGens)
    COM_INTERFACE_ENTRY(IWindGens)
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
    STDMETHOD(get_Count)(long* Value);
    STDMETHOD(get_Ag)(double* Value);
    STDMETHOD_(HRESULT, put_Ag)(double Value);
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
    STDMETHOD(get_Cp)(double* Value);
    STDMETHOD_(HRESULT, put_Cp)(double Value);
    STDMETHOD(get_kV)(double* Value);
    STDMETHOD_(HRESULT, put_kV)(double Value);
    STDMETHOD(get_kva)(double* Value);
    STDMETHOD_(HRESULT, put_kva)(double Value);
    STDMETHOD(get_kvar)(double* Value);
    STDMETHOD_(HRESULT, put_kvar)(double Value);
    STDMETHOD(get_kW)(double* Value);
    STDMETHOD_(HRESULT, put_kW)(double Value);
    STDMETHOD(get_Lamda)(double* Value);
    STDMETHOD_(HRESULT, put_Lamda)(double Value);
    STDMETHOD(get_N_WTG)(long* Value);
    STDMETHOD_(HRESULT, put_N_WTG)(long Value);
    STDMETHOD(get_NPoles)(long* Value);
    STDMETHOD_(HRESULT, put_NPoles)(long Value);
    STDMETHOD(get_pd)(double* Value);
    STDMETHOD_(HRESULT, put_pd)(double Value);
    STDMETHOD(get_PF)(double* Value);
    STDMETHOD_(HRESULT, put_PF)(double Value);
    STDMETHOD(get_PSS)(double* Value);
    STDMETHOD_(HRESULT, put_PSS)(double Value);
    STDMETHOD(get_QFlag)(long* Value);
    STDMETHOD_(HRESULT, put_QFlag)(long Value);
    STDMETHOD(get_QMode)(long* Value);
    STDMETHOD_(HRESULT, put_QMode)(long Value);
    STDMETHOD(get_QSS)(double* Value);
    STDMETHOD_(HRESULT, put_QSS)(double Value);
    STDMETHOD(get_Rad)(double* Value);
    STDMETHOD_(HRESULT, put_Rad)(double Value);
    STDMETHOD(get_RThev)(double* Value);
    STDMETHOD_(HRESULT, put_RThev)(double Value);
    STDMETHOD(get_VCutIn)(double* Value);
    STDMETHOD_(HRESULT, put_VCutIn)(double Value);
    STDMETHOD(get_VCutOut)(double* Value);
    STDMETHOD_(HRESULT, put_VCutOut)(double Value);
    STDMETHOD(get_Vss)(double* Value);
    STDMETHOD_(HRESULT, put_Vss)(double Value);
    STDMETHOD(get_WindSpeed)(double* Value);
    STDMETHOD_(HRESULT, put_WindSpeed)(double Value);
    STDMETHOD(get_XThev)(double* Value);
    STDMETHOD_(HRESULT, put_XThev)(double Value);
    STDMETHOD(get_Name)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Name)(BSTR Value);
};

OBJECT_ENTRY_AUTO(__uuidof(WindGens), CWindGens)
