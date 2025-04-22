
// DSSProperty.h : Declaration of the CDSSProperty

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CDSSProperty
class ATL_NO_VTABLE CDSSProperty :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CDSSProperty, &CLSID_DSSProperty>,
    public ISupportErrorInfo,
    public IDispatchImpl<IDSSProperty, &IID_IDSSProperty, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CDSSProperty, &IID_IDSSProperty>

{
public:
    CDSSProperty()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_DSSPROPERTY)

BEGIN_COM_MAP(CDSSProperty)
    COM_INTERFACE_ENTRY(IDSSProperty)
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
    STDMETHOD(get_Name)(BSTR* Name);
    STDMETHOD(get_Description)(BSTR* Description);
    STDMETHOD(get_Val)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Val)(BSTR Value);
};

OBJECT_ENTRY_AUTO(__uuidof(DSSProperty), CDSSProperty)
