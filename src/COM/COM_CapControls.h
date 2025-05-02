
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_CapControls.h : Declaration of CCapControls

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CCapControls
class ATL_NO_VTABLE CCapControls :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CCapControls, &CLSID_CapControls>,
    public ISupportErrorInfo,
    public IDispatchImpl<ICapControls, &IID_ICapControls, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CCapControls, &IID_ICapControls>

{
public:
    CCapControls()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_CAPCONTROLS)

BEGIN_COM_MAP(CCapControls)
    COM_INTERFACE_ENTRY(ICapControls)
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
    STDMETHOD(get_Name)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Name)(BSTR Value);
    STDMETHOD(get_First)(long* Value);
    STDMETHOD(get_Next)(long* Value);
    STDMETHOD(get_Mode)(CapControlModes* Value);
    STDMETHOD_(HRESULT, put_Mode)(CapControlModes Value);
    STDMETHOD(get_Capacitor)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Capacitor)(BSTR Value);
    STDMETHOD(get_MonitoredObj)(BSTR* Value);
    STDMETHOD_(HRESULT, put_MonitoredObj)(BSTR Value);
    STDMETHOD(get_MonitoredTerm)(long* Value);
    STDMETHOD_(HRESULT, put_MonitoredTerm)(long Value);
    STDMETHOD(get_CTratio)(double* Value);
    STDMETHOD_(HRESULT, put_CTratio)(double Value);
    STDMETHOD(get_PTratio)(double* Value);
    STDMETHOD_(HRESULT, put_PTratio)(double Value);
    STDMETHOD(get_ONSetting)(double* Value);
    STDMETHOD_(HRESULT, put_ONSetting)(double Value);
    STDMETHOD(get_OFFSetting)(double* Value);
    STDMETHOD_(HRESULT, put_OFFSetting)(double Value);
    STDMETHOD(get_Vmax)(double* Value);
    STDMETHOD_(HRESULT, put_Vmax)(double Value);
    STDMETHOD(get_Vmin)(double* Value);
    STDMETHOD_(HRESULT, put_Vmin)(double Value);
    STDMETHOD(get_UseVoltOverride)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_UseVoltOverride)(VARIANT_BOOL Value);
    STDMETHOD(get_Delay)(double* Value);
    STDMETHOD_(HRESULT, put_Delay)(double Value);
    STDMETHOD(get_DelayOff)(double* Value);
    STDMETHOD_(HRESULT, put_DelayOff)(double Value);
    STDMETHOD(get_DeadTime)(double* Value);
    STDMETHOD_(HRESULT, put_DeadTime)(double Value);
    STDMETHOD(get_Count)(long* Value);
    STDMETHOD(Reset)();
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
};

OBJECT_ENTRY_AUTO(__uuidof(CapControls), CCapControls)
