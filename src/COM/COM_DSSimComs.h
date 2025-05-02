
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_DSSimComs.h : Declaration of CDSSimComs

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CDSSimComs
class ATL_NO_VTABLE CDSSimComs :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CDSSimComs, &CLSID_DSSimComs>,
    public ISupportErrorInfo,
    public IDispatchImpl<IDSSimComs, &IID_IDSSimComs, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CDSSimComs, &IID_IDSSimComs>

{
public:
    CDSSimComs()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_DSSIMCOMS)

BEGIN_COM_MAP(CDSSimComs)
    COM_INTERFACE_ENTRY(IDSSimComs)
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
    STDMETHOD_(HRESULT, BusVoltagepu)(unsigned int Index, VARIANT* Vpu);
    STDMETHOD_(HRESULT, BusVoltage)(unsigned int Index, VARIANT* Voltages);
};

OBJECT_ENTRY_AUTO(__uuidof(DSSimComs), CDSSimComs)
