
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Text.h : Declaration of CText

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CText
class ATL_NO_VTABLE CText :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CText, &CLSID_Text>,
    public ISupportErrorInfo,
    public IDispatchImpl<IText, &IID_IText, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CText, &IID_IText>

{
public:
    CText()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_TEXT)

BEGIN_COM_MAP(CText)
    COM_INTERFACE_ENTRY(IText)
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
    STDMETHOD(get_Command)(BSTR* Command);
    STDMETHOD_(HRESULT, put_Command)(BSTR Command);
    STDMETHOD(get_Result)(BSTR* Result);
};

OBJECT_ENTRY_AUTO(__uuidof(Text), CText)
