
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Parallel.h : Declaration of CParallel

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CParallel
class ATL_NO_VTABLE CParallel :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CParallel, &CLSID_Parallel>,
    public ISupportErrorInfo,
    public IDispatchImpl<IParallel, &IID_IParallel, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CParallel, &IID_IParallel>

{
public:
    CParallel()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_PARALLEL)

BEGIN_COM_MAP(CParallel)
    COM_INTERFACE_ENTRY(IParallel)
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
    STDMETHOD(get_NumCPUs)(long* Value);
    STDMETHOD(get_NumCores)(long* Value);
    STDMETHOD(get_ActiveActor)(long* Value);
    STDMETHOD_(HRESULT, put_ActiveActor)(long Value);
    STDMETHOD(CreateActor)();
    STDMETHOD(get_ActorCPU)(long* Value);
    STDMETHOD_(HRESULT, put_ActorCPU)(long Value);
    STDMETHOD(get_NumOfActors)(long* Value);
    STDMETHOD(Wait)();
    STDMETHOD(get_ActorProgress)(VARIANT* Value);
    STDMETHOD(get_ActorStatus)(VARIANT* Value);
    STDMETHOD(get_ActiveParallel)(long* Value);
    STDMETHOD_(HRESULT, put_ActiveParallel)(long Value);
    STDMETHOD(get_ConcatenateReports)(long* Value);
    STDMETHOD_(HRESULT, put_ConcatenateReports)(long Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Parallel), CParallel)
