
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Parser.h : Declaration of CParser

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CParser
class ATL_NO_VTABLE CParser :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CParser, &CLSID_Parser>,
    public ISupportErrorInfo,
    public IDispatchImpl<IParser, &IID_IParser, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CParser, &IID_IParser>

{
public:
    CParser()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_PARSER)

BEGIN_COM_MAP(CParser)
    COM_INTERFACE_ENTRY(IParser)
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
    STDMETHOD(get_CmdString)(BSTR* Value);
    STDMETHOD_(HRESULT, put_CmdString)(BSTR Value);
    STDMETHOD(get_NextParam)(BSTR* Value);
    STDMETHOD(get_AutoIncrement)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_AutoIncrement)(VARIANT_BOOL Value);
    STDMETHOD(get_DblValue)(double* Value);
    STDMETHOD(get_IntValue)(long* Value);
    STDMETHOD(get_StrValue)(BSTR* Value);
    STDMETHOD(get_WhiteSpace)(BSTR* Value);
    STDMETHOD_(HRESULT, put_WhiteSpace)(BSTR Value);
    STDMETHOD(get_BeginQuote)(BSTR* Value);
    STDMETHOD_(HRESULT, put_BeginQuote)(BSTR Value);
    STDMETHOD(get_EndQuote)(BSTR* Value);
    STDMETHOD_(HRESULT, put_EndQuote)(BSTR Value);
    STDMETHOD(get_Delimiters)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Delimiters)(BSTR Value);
    STDMETHOD(ResetDelimiters)();
    STDMETHOD_(HRESULT, get_Vector)(long ExpectedSize, VARIANT* Value);
    STDMETHOD_(HRESULT, get_Matrix)(long ExpectedOrder, VARIANT* Value);
    STDMETHOD_(HRESULT, get_SymMatrix)(long ExpectedOrder, VARIANT* Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Parser), CParser)
