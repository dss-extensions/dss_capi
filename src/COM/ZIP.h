
// ZIP.h : Declaration of the CZIP

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CZIP
class ATL_NO_VTABLE CZIP :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CZIP, &CLSID_ZIP>,
    public ISupportErrorInfo,
    public IDispatchImpl<IZIP, &IID_IZIP, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CZIP, &IID_IZIP>

{
public:
    CZIP()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_ZIP)

BEGIN_COM_MAP(CZIP)
    COM_INTERFACE_ENTRY(IZIP)
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
    STDMETHOD_(HRESULT, Open)(BSTR FileName);
    STDMETHOD(Close)();
    STDMETHOD_(HRESULT, Redirect)(BSTR FileName);
    STDMETHOD_(HRESULT, Extract)(BSTR FileName, VARIANT* Value);
    STDMETHOD_(HRESULT, List)(BSTR regexp, VARIANT* Value);
    STDMETHOD_(HRESULT, Contains)(BSTR Name, VARIANT_BOOL* Value);
};

OBJECT_ENTRY_AUTO(__uuidof(ZIP), CZIP)
