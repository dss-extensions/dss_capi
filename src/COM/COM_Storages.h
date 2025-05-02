
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Storages.h : Declaration of CStorages

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CStorages
class ATL_NO_VTABLE CStorages :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CStorages, &CLSID_Storages>,
    public ISupportErrorInfo,
    public IDispatchImpl<IStorages, &IID_IStorages, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CStorages, &IID_IStorages>

{
public:
    CStorages()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_STORAGES)

BEGIN_COM_MAP(CStorages)
    COM_INTERFACE_ENTRY(IStorages)
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
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
    STDMETHOD(get_Name)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Name)(BSTR Value);
    STDMETHOD(get_State)(long* Value);
    STDMETHOD_(HRESULT, put_State)(long Value);
    STDMETHOD(get_puSOC)(double* Value);
    STDMETHOD_(HRESULT, put_puSOC)(double Value);
    STDMETHOD(get_EffCharge)(double* Value);
    STDMETHOD_(HRESULT, put_EffCharge)(double Value);
    STDMETHOD(get_EffDischarge)(double* Value);
    STDMETHOD_(HRESULT, put_EffDischarge)(double Value);
    STDMETHOD(get_kWRated)(double* Value);
    STDMETHOD_(HRESULT, put_kWRated)(double Value);
    STDMETHOD(get_ControlMode)(long* Value);
    STDMETHOD_(HRESULT, put_ControlMode)(long Value);
    STDMETHOD(get_Kp)(double* Value);
    STDMETHOD_(HRESULT, put_Kp)(double Value);
    STDMETHOD(get_kV)(double* Value);
    STDMETHOD_(HRESULT, put_kV)(double Value);
    STDMETHOD(get_kva)(double* Value);
    STDMETHOD_(HRESULT, put_kva)(double Value);
    STDMETHOD(get_kvar)(double* Value);
    STDMETHOD_(HRESULT, put_kvar)(double Value);
    STDMETHOD(get_kWhRated)(double* Value);
    STDMETHOD_(HRESULT, put_kWhRated)(double Value);
    STDMETHOD(get_LimitCurrent)(double* Value);
    STDMETHOD_(HRESULT, put_LimitCurrent)(double Value);
    STDMETHOD(get_PF)(double* Value);
    STDMETHOD_(HRESULT, put_PF)(double Value);
    STDMETHOD(get_SafeMode)(long* Value);
    STDMETHOD(get_SafeVoltage)(double* Value);
    STDMETHOD_(HRESULT, put_SafeVoltage)(double Value);
    STDMETHOD(get_AmpLimit)(double* Value);
    STDMETHOD_(HRESULT, put_AmpLimit)(double Value);
    STDMETHOD(get_AmpLimitGain)(double* Value);
    STDMETHOD_(HRESULT, put_AmpLimitGain)(double Value);
    STDMETHOD(get_kVDC)(double* Value);
    STDMETHOD_(HRESULT, put_kVDC)(double Value);
    STDMETHOD(get_kW)(double* Value);
    STDMETHOD_(HRESULT, put_kW)(double Value);
    STDMETHOD(get_PITol)(double* Value);
    STDMETHOD_(HRESULT, put_PITol)(double Value);
    STDMETHOD(get_ChargeTrigger)(double* Value);
    STDMETHOD_(HRESULT, put_ChargeTrigger)(double Value);
    STDMETHOD(get_DischargeTrigger)(double* Value);
    STDMETHOD_(HRESULT, put_DischargeTrigger)(double Value);
    STDMETHOD(get_TimeChargeTrig)(double* Value);
    STDMETHOD_(HRESULT, put_TimeChargeTrig)(double Value);
    STDMETHOD(get_VarFollowInverter)(long* Value);
    STDMETHOD_(HRESULT, put_VarFollowInverter)(long Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Storages), CStorages)
