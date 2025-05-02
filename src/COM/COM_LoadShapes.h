
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_LoadShapes.h : Declaration of CLoadShapes

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CLoadShapes
class ATL_NO_VTABLE CLoadShapes :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CLoadShapes, &CLSID_LoadShapes>,
    public ISupportErrorInfo,
    public IDispatchImpl<ILoadShapes, &IID_ILoadShapes, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CLoadShapes, &IID_ILoadShapes>

{
public:
    CLoadShapes()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_LOADSHAPES)

BEGIN_COM_MAP(CLoadShapes)
    COM_INTERFACE_ENTRY(ILoadShapes)
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
    STDMETHOD(get_Npts)(long* Value);
    STDMETHOD_(HRESULT, put_Npts)(long Value);
    STDMETHOD(get_Pmult)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_Pmult)(VARIANT Value);
    STDMETHOD(get_Qmult)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_Qmult)(VARIANT Value);
    STDMETHOD(Normalize)();
    STDMETHOD(get_TimeArray)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_TimeArray)(VARIANT Value);
    STDMETHOD(get_HrInterval)(double* Value);
    STDMETHOD_(HRESULT, put_HrInterval)(double Value);
    STDMETHOD(get_MinInterval)(double* Value);
    STDMETHOD_(HRESULT, put_MinInterval)(double Value);
    STDMETHOD_(HRESULT, New)(BSTR Name, long* Value);
    STDMETHOD(get_Pbase)(double* Value);
    STDMETHOD_(HRESULT, put_Pbase)(double Value);
    STDMETHOD(get_Qbase)(double* Value);
    STDMETHOD_(HRESULT, put_Qbase)(double Value);
    STDMETHOD(get_UseActual)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_UseActual)(VARIANT_BOOL Value);
    STDMETHOD(get_Sinterval)(double* Value);
    STDMETHOD_(HRESULT, put_Sinterval)(double Value);
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
    STDMETHOD(UseFloat32)();
    STDMETHOD(UseFloat64)();
};

OBJECT_ENTRY_AUTO(__uuidof(LoadShapes), CLoadShapes)
