
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Loads.h : Declaration of CLoads

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CLoads
class ATL_NO_VTABLE CLoads :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CLoads, &CLSID_Loads>,
    public ISupportErrorInfo,
    public IDispatchImpl<ILoads, &IID_ILoads, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CLoads, &IID_ILoads>

{
public:
    CLoads()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_LOADS)

BEGIN_COM_MAP(CLoads)
    COM_INTERFACE_ENTRY(ILoads)
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
    STDMETHOD(get_First)(long* Value);
    STDMETHOD(get_Next)(long* Value);
    STDMETHOD(get_Name)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Name)(BSTR Value);
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
    STDMETHOD(get_kW)(double* Value);
    STDMETHOD_(HRESULT, put_kW)(double Value);
    STDMETHOD(get_kV)(double* Value);
    STDMETHOD_(HRESULT, put_kV)(double Value);
    STDMETHOD(get_kvar)(double* Value);
    STDMETHOD_(HRESULT, put_kvar)(double Value);
    STDMETHOD(get_PF)(double* Value);
    STDMETHOD_(HRESULT, put_PF)(double Value);
    STDMETHOD(get_Count)(long* Value);
    STDMETHOD(get_PctMean)(double* Value);
    STDMETHOD_(HRESULT, put_PctMean)(double Value);
    STDMETHOD(get_PctStdDev)(double* Value);
    STDMETHOD_(HRESULT, put_PctStdDev)(double Value);
    STDMETHOD(get_AllocationFactor)(double* Value);
    STDMETHOD_(HRESULT, put_AllocationFactor)(double Value);
    STDMETHOD(get_Cfactor)(double* Value);
    STDMETHOD_(HRESULT, put_Cfactor)(double Value);
    STDMETHOD(get_Class)(long* Value);
    STDMETHOD_(HRESULT, put_Class)(long Value);
    STDMETHOD(get_IsDelta)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_IsDelta)(VARIANT_BOOL Value);
    STDMETHOD(get_CVRcurve)(BSTR* Value);
    STDMETHOD_(HRESULT, put_CVRcurve)(BSTR Value);
    STDMETHOD(get_CVRwatts)(double* Value);
    STDMETHOD_(HRESULT, put_CVRwatts)(double Value);
    STDMETHOD(get_CVRvars)(double* Value);
    STDMETHOD_(HRESULT, put_CVRvars)(double Value);
    STDMETHOD(get_daily)(BSTR* Value);
    STDMETHOD_(HRESULT, put_daily)(BSTR Value);
    STDMETHOD(get_duty)(BSTR* Value);
    STDMETHOD_(HRESULT, put_duty)(BSTR Value);
    STDMETHOD(get_kva)(double* Value);
    STDMETHOD_(HRESULT, put_kva)(double Value);
    STDMETHOD(get_kwh)(double* Value);
    STDMETHOD_(HRESULT, put_kwh)(double Value);
    STDMETHOD(get_kwhdays)(double* Value);
    STDMETHOD_(HRESULT, put_kwhdays)(double Value);
    STDMETHOD(get_Model)(LoadModels* Value);
    STDMETHOD_(HRESULT, put_Model)(LoadModels Value);
    STDMETHOD(get_NumCust)(long* Value);
    STDMETHOD_(HRESULT, put_NumCust)(long Value);
    STDMETHOD(get_Rneut)(double* Value);
    STDMETHOD_(HRESULT, put_Rneut)(double Value);
    STDMETHOD(get_Spectrum)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Spectrum)(BSTR Value);
    STDMETHOD(get_Vmaxpu)(double* Value);
    STDMETHOD_(HRESULT, put_Vmaxpu)(double Value);
    STDMETHOD(get_Vminemerg)(double* Value);
    STDMETHOD_(HRESULT, put_Vminemerg)(double Value);
    STDMETHOD(get_Vminnorm)(double* Value);
    STDMETHOD_(HRESULT, put_Vminnorm)(double Value);
    STDMETHOD(get_Vminpu)(double* Value);
    STDMETHOD_(HRESULT, put_Vminpu)(double Value);
    STDMETHOD(get_xfkVA)(double* Value);
    STDMETHOD_(HRESULT, put_xfkVA)(double Value);
    STDMETHOD(get_Xneut)(double* Value);
    STDMETHOD_(HRESULT, put_Xneut)(double Value);
    STDMETHOD(get_Yearly)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Yearly)(BSTR Value);
    STDMETHOD(get_Status)(LoadStatus* Value);
    STDMETHOD_(HRESULT, put_Status)(LoadStatus Value);
    STDMETHOD(get_Growth)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Growth)(BSTR Value);
    STDMETHOD(get_ZIPV)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_ZIPV)(VARIANT Value);
    STDMETHOD(get_pctSeriesRL)(double* Value);
    STDMETHOD_(HRESULT, put_pctSeriesRL)(double Value);
    STDMETHOD(get_RelWeight)(double* Value);
    STDMETHOD_(HRESULT, put_RelWeight)(double Value);
    STDMETHOD(get_Sensor)(BSTR* Value);
    STDMETHOD(get_Phases)(long* Value);
    STDMETHOD_(HRESULT, put_Phases)(long Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Loads), CLoads)
