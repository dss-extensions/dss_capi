
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_RegControls.h : Declaration of CRegControls

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CRegControls
class ATL_NO_VTABLE CRegControls :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CRegControls, &CLSID_RegControls>,
    public ISupportErrorInfo,
    public IDispatchImpl<IRegControls, &IID_IRegControls, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CRegControls, &IID_IRegControls>

{
public:
    CRegControls()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_REGCONTROLS)

BEGIN_COM_MAP(CRegControls)
    COM_INTERFACE_ENTRY(IRegControls)
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
    STDMETHOD(get_MonitoredBus)(BSTR* Value);
    STDMETHOD_(HRESULT, put_MonitoredBus)(BSTR Value);
    STDMETHOD(get_Transformer)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Transformer)(BSTR Value);
    STDMETHOD(get_TapWinding)(long* Value);
    STDMETHOD_(HRESULT, put_TapWinding)(long Value);
    STDMETHOD(get_Winding)(long* Value);
    STDMETHOD_(HRESULT, put_Winding)(long Value);
    STDMETHOD(get_CTPrimary)(double* Value);
    STDMETHOD_(HRESULT, put_CTPrimary)(double Value);
    STDMETHOD(get_PTratio)(double* Value);
    STDMETHOD_(HRESULT, put_PTratio)(double Value);
    STDMETHOD(get_ForwardR)(double* Value);
    STDMETHOD_(HRESULT, put_ForwardR)(double Value);
    STDMETHOD(get_ForwardX)(double* Value);
    STDMETHOD_(HRESULT, put_ForwardX)(double Value);
    STDMETHOD(get_ReverseR)(double* Value);
    STDMETHOD_(HRESULT, put_ReverseR)(double Value);
    STDMETHOD(get_ReverseX)(double* Value);
    STDMETHOD_(HRESULT, put_ReverseX)(double Value);
    STDMETHOD(get_IsReversible)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_IsReversible)(VARIANT_BOOL Value);
    STDMETHOD(get_IsInverseTime)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_IsInverseTime)(VARIANT_BOOL Value);
    STDMETHOD(get_Delay)(double* Value);
    STDMETHOD_(HRESULT, put_Delay)(double Value);
    STDMETHOD(get_TapDelay)(double* Value);
    STDMETHOD_(HRESULT, put_TapDelay)(double Value);
    STDMETHOD(get_MaxTapChange)(long* Value);
    STDMETHOD_(HRESULT, put_MaxTapChange)(long Value);
    STDMETHOD(get_VoltageLimit)(double* Value);
    STDMETHOD_(HRESULT, put_VoltageLimit)(double Value);
    STDMETHOD(get_ForwardBand)(double* Value);
    STDMETHOD_(HRESULT, put_ForwardBand)(double Value);
    STDMETHOD(get_ForwardVreg)(double* Value);
    STDMETHOD_(HRESULT, put_ForwardVreg)(double Value);
    STDMETHOD(get_ReverseBand)(double* Value);
    STDMETHOD_(HRESULT, put_ReverseBand)(double Value);
    STDMETHOD(get_ReverseVreg)(double* Value);
    STDMETHOD_(HRESULT, put_ReverseVreg)(double Value);
    STDMETHOD(get_Count)(long* Value);
    STDMETHOD(get_TapNumber)(long* Value);
    STDMETHOD_(HRESULT, put_TapNumber)(long Value);
    STDMETHOD(Reset)();
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
};

OBJECT_ENTRY_AUTO(__uuidof(RegControls), CRegControls)
