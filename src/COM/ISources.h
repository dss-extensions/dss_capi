
// ISources.h : Declaration of the CISources

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CISources
class ATL_NO_VTABLE CISources :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CISources, &CLSID_ISources>,
    public ISupportErrorInfo,
    public IDispatchImpl<IISources, &IID_IISources, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CISources, &IID_IISources>

{
public:
    CISources()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_ISOURCES)

BEGIN_COM_MAP(CISources)
    COM_INTERFACE_ENTRY(IISources)
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
    STDMETHOD(get_AllNames)(VARIANT* Value);
    STDMETHOD(get_Count)(long* Value);
    STDMETHOD(get_First)(long* Value);
    STDMETHOD(get_Next)(long* Value);
    STDMETHOD(get_Name)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Name)(BSTR Value);
    STDMETHOD(get_Amps)(double* Value);
    STDMETHOD_(HRESULT, put_Amps)(double Value);
    STDMETHOD(get_AngleDeg)(double* Value);
    STDMETHOD_(HRESULT, put_AngleDeg)(double Value);
    STDMETHOD(get_Frequency)(double* Value);
    STDMETHOD_(HRESULT, put_Frequency)(double Value);
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
};

OBJECT_ENTRY_AUTO(__uuidof(ISources), CISources)
