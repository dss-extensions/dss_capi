
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Settings.h : Declaration of CSettings

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CSettings
class ATL_NO_VTABLE CSettings :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CSettings, &CLSID_Settings>,
    public ISupportErrorInfo,
    public IDispatchImpl<ISettings, &IID_ISettings, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CSettings, &IID_ISettings>

{
public:
    CSettings()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_SETTINGS)

BEGIN_COM_MAP(CSettings)
    COM_INTERFACE_ENTRY(ISettings)
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
    STDMETHOD(get_AllowDuplicates)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_AllowDuplicates)(VARIANT_BOOL Value);
    STDMETHOD(get_ZoneLock)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_ZoneLock)(VARIANT_BOOL Value);
    STDMETHOD_(HRESULT, put_AllocationFactors)(double rhs);
    STDMETHOD(get_AutoBusList)(BSTR* Value);
    STDMETHOD_(HRESULT, put_AutoBusList)(BSTR Value);
    STDMETHOD(get_CktModel)(long* Value);
    STDMETHOD_(HRESULT, put_CktModel)(long Value);
    STDMETHOD(get_NormVminpu)(double* Value);
    STDMETHOD_(HRESULT, put_NormVminpu)(double Value);
    STDMETHOD(get_NormVmaxpu)(double* Value);
    STDMETHOD_(HRESULT, put_NormVmaxpu)(double Value);
    STDMETHOD(get_EmergVminpu)(double* Value);
    STDMETHOD_(HRESULT, put_EmergVminpu)(double Value);
    STDMETHOD(get_EmergVmaxpu)(double* Value);
    STDMETHOD_(HRESULT, put_EmergVmaxpu)(double Value);
    STDMETHOD(get_UEweight)(double* Value);
    STDMETHOD_(HRESULT, put_UEweight)(double Value);
    STDMETHOD(get_LossWeight)(double* Value);
    STDMETHOD_(HRESULT, put_LossWeight)(double Value);
    STDMETHOD(get_UEregs)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_UEregs)(VARIANT Value);
    STDMETHOD(get_LossRegs)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_LossRegs)(VARIANT Value);
    STDMETHOD(get_Trapezoidal)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_Trapezoidal)(VARIANT_BOOL Value);
    STDMETHOD(get_VoltageBases)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_VoltageBases)(VARIANT Value);
    STDMETHOD(get_ControlTrace)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_ControlTrace)(VARIANT_BOOL Value);
    STDMETHOD(get_PriceSignal)(double* Value);
    STDMETHOD_(HRESULT, put_PriceSignal)(double Value);
    STDMETHOD(get_PriceCurve)(BSTR* Value);
    STDMETHOD_(HRESULT, put_PriceCurve)(BSTR Value);
    STDMETHOD(get_AllowChangeDir)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_AllowChangeDir)(VARIANT_BOOL Value);
    STDMETHOD(get_AllowDOScmd)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_AllowDOScmd)(VARIANT_BOOL Value);
    STDMETHOD(get_AllowEditor)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_AllowEditor)(VARIANT_BOOL Value);
    STDMETHOD(get_COMErrorResults)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_COMErrorResults)(VARIANT_BOOL Value);
    STDMETHOD(get_CompatFlags)(long* Value);
    STDMETHOD_(HRESULT, put_CompatFlags)(long Value);
    STDMETHOD(get_IterateDisabled)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_IterateDisabled)(VARIANT_BOOL Value);
    STDMETHOD(get_LoadsTerminalCheck)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_LoadsTerminalCheck)(VARIANT_BOOL Value);
    STDMETHOD_(HRESULT, SetPropertyNameStyle)(DSSPropertyNameStyle Value);
    STDMETHOD(get_SkipFileRegExp)(BSTR* Value);
    STDMETHOD_(HRESULT, put_SkipFileRegExp)(BSTR Value);
    STDMETHOD(get_SkipCommands)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_SkipCommands)(VARIANT Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Settings), CSettings)
