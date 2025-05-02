
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Topology.h : Declaration of CTopology

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CTopology
class ATL_NO_VTABLE CTopology :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CTopology, &CLSID_Topology>,
    public ISupportErrorInfo,
    public IDispatchImpl<ITopology, &IID_ITopology, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CTopology, &IID_ITopology>

{
public:
    CTopology()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_TOPOLOGY)

BEGIN_COM_MAP(CTopology)
    COM_INTERFACE_ENTRY(ITopology)
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
    STDMETHOD(get_NumLoops)(long* Value);
    STDMETHOD(get_NumIsolatedBranches)(long* Value);
    STDMETHOD(get_AllLoopedPairs)(VARIANT* Value);
    STDMETHOD(get_AllIsolatedBranches)(VARIANT* Value);
    STDMETHOD(get_NumIsolatedLoads)(long* Value);
    STDMETHOD(get_AllIsolatedLoads)(VARIANT* Value);
    STDMETHOD(get_BranchName)(BSTR* Value);
    STDMETHOD_(HRESULT, put_BranchName)(BSTR Value);
    STDMETHOD(get_First)(long* Value);
    STDMETHOD(get_Next)(long* Value);
    STDMETHOD(get_ActiveBranch)(long* Value);
    STDMETHOD(get_ForwardBranch)(long* Value);
    STDMETHOD(get_BackwardBranch)(long* Value);
    STDMETHOD(get_LoopedBranch)(long* Value);
    STDMETHOD(get_ParallelBranch)(long* Value);
    STDMETHOD(get_FirstLoad)(long* Value);
    STDMETHOD(get_NextLoad)(long* Value);
    STDMETHOD(get_ActiveLevel)(long* Value);
    STDMETHOD(get_BusName)(BSTR* Value);
    STDMETHOD_(HRESULT, put_BusName)(BSTR Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Topology), CTopology)
