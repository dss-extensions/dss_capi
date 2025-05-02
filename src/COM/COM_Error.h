
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Error.h : Declaration of CError

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CError
class ATL_NO_VTABLE CError :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CError, &CLSID_Error>,
    public ISupportErrorInfo,
    public IDispatchImpl<IError, &IID_IError, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CError, &IID_IError>

{
public:
    CError()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_ERROR)

BEGIN_COM_MAP(CError)
    COM_INTERFACE_ENTRY(IError)
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
    STDMETHOD(get_Number)(long* Number);
    STDMETHOD(get_Description)(BSTR* Description);
    STDMETHOD(get_EarlyAbort)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_EarlyAbort)(VARIANT_BOOL Value);
    STDMETHOD(get_UseExceptions)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_UseExceptions)(VARIANT_BOOL Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Error), CError)
