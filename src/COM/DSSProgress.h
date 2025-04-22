
// DSSProgress.h : Declaration of the CDSSProgress

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CDSSProgress
class ATL_NO_VTABLE CDSSProgress :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CDSSProgress, &CLSID_DSSProgress>,
    public ISupportErrorInfo,
    public IDispatchImpl<IDSSProgress, &IID_IDSSProgress, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CDSSProgress, &IID_IDSSProgress>

{
public:
    CDSSProgress()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_DSSPROGRESS)

BEGIN_COM_MAP(CDSSProgress)
    COM_INTERFACE_ENTRY(IDSSProgress)
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
    STDMETHOD_(HRESULT, put_PctProgress)(long rhs);
    STDMETHOD_(HRESULT, put_Caption)(BSTR rhs);
    STDMETHOD(Show)();
    STDMETHOD(Close)();
};

OBJECT_ENTRY_AUTO(__uuidof(DSSProgress), CDSSProgress)
