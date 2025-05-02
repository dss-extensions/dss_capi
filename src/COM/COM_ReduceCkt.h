
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_ReduceCkt.h : Declaration of CReduceCkt

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CReduceCkt
class ATL_NO_VTABLE CReduceCkt :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CReduceCkt, &CLSID_ReduceCkt>,
    public ISupportErrorInfo,
    public IDispatchImpl<IReduceCkt, &IID_IReduceCkt, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CReduceCkt, &IID_IReduceCkt>

{
public:
    CReduceCkt()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_REDUCECKT)

BEGIN_COM_MAP(CReduceCkt)
    COM_INTERFACE_ENTRY(IReduceCkt)
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
    STDMETHOD(get_Zmag)(double* Value);
    STDMETHOD_(HRESULT, put_Zmag)(double Value);
    STDMETHOD(get_KeepLoad)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_KeepLoad)(VARIANT_BOOL Value);
    STDMETHOD(get_EditString)(BSTR* Value);
    STDMETHOD_(HRESULT, put_EditString)(BSTR Value);
    STDMETHOD(get_StartPDElement)(BSTR* Value);
    STDMETHOD_(HRESULT, put_StartPDElement)(BSTR Value);
    STDMETHOD(get_EnergyMeter)(BSTR* Value);
    STDMETHOD_(HRESULT, put_EnergyMeter)(BSTR Value);
    STDMETHOD_(HRESULT, SaveCircuit)(BSTR CktName);
    STDMETHOD(DoDefault)();
    STDMETHOD(DoShortLines)();
    STDMETHOD(DoDangling)();
    STDMETHOD(DoLoopBreak)();
    STDMETHOD(DoParallelLines)();
    STDMETHOD(DoSwitches)();
    STDMETHOD(Do1phLaterals)();
    STDMETHOD(DoBranchRemove)();
};

OBJECT_ENTRY_AUTO(__uuidof(ReduceCkt), CReduceCkt)
