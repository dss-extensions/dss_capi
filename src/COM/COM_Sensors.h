
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Sensors.h : Declaration of CSensors

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CSensors
class ATL_NO_VTABLE CSensors :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CSensors, &CLSID_Sensors>,
    public ISupportErrorInfo,
    public IDispatchImpl<ISensors, &IID_ISensors, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CSensors, &IID_ISensors>

{
public:
    CSensors()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_SENSORS)

BEGIN_COM_MAP(CSensors)
    COM_INTERFACE_ENTRY(ISensors)
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
    STDMETHOD(get_Name)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Name)(BSTR Value);
    STDMETHOD(get_Count)(long* Value);
    STDMETHOD(get_First)(long* Value);
    STDMETHOD(get_Next)(long* Value);
    STDMETHOD(get_AllNames)(VARIANT* Value);
    STDMETHOD(get_IsDelta)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_IsDelta)(VARIANT_BOOL Value);
    STDMETHOD(get_ReverseDelta)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_ReverseDelta)(VARIANT_BOOL Value);
    STDMETHOD(get_PctError)(double* Value);
    STDMETHOD_(HRESULT, put_PctError)(double Value);
    STDMETHOD(get_Weight)(double* Value);
    STDMETHOD_(HRESULT, put_Weight)(double Value);
    STDMETHOD(get_MeteredElement)(BSTR* Value);
    STDMETHOD_(HRESULT, put_MeteredElement)(BSTR Value);
    STDMETHOD(get_MeteredTerminal)(long* Value);
    STDMETHOD_(HRESULT, put_MeteredTerminal)(long Value);
    STDMETHOD(Reset)();
    STDMETHOD(ResetAll)();
    STDMETHOD(get_kVBase)(double* Value);
    STDMETHOD_(HRESULT, put_kVBase)(double Value);
    STDMETHOD(get_Currents)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_Currents)(VARIANT Value);
    STDMETHOD(get_kVS)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_kVS)(VARIANT Value);
    STDMETHOD(get_kVARS)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_kVARS)(VARIANT Value);
    STDMETHOD(get_kWS)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_kWS)(VARIANT Value);
    STDMETHOD(get_AllocationFactor)(VARIANT* Value);
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Sensors), CSensors)
