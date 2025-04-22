
// GICSources.h : Declaration of the CGICSources

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CGICSources
class ATL_NO_VTABLE CGICSources :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CGICSources, &CLSID_GICSources>,
    public ISupportErrorInfo,
    public IDispatchImpl<IGICSources, &IID_IGICSources, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CGICSources, &IID_IGICSources>

{
public:
    CGICSources()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_GICSOURCES)

BEGIN_COM_MAP(CGICSources)
    COM_INTERFACE_ENTRY(IGICSources)
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
    STDMETHOD(get_Bus1)(BSTR* Value);
    STDMETHOD(get_Bus2)(BSTR* Value);
    STDMETHOD(get_Name)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Name)(BSTR Value);
    STDMETHOD(get_Phases)(long* Value);
    STDMETHOD_(HRESULT, put_Phases)(long Value);
    STDMETHOD(get_EN)(double* Value);
    STDMETHOD_(HRESULT, put_EN)(double Value);
    STDMETHOD(get_EE)(double* Value);
    STDMETHOD_(HRESULT, put_EE)(double Value);
    STDMETHOD(get_Lat1)(double* Value);
    STDMETHOD_(HRESULT, put_Lat1)(double Value);
    STDMETHOD(get_Lat2)(double* Value);
    STDMETHOD_(HRESULT, put_Lat2)(double Value);
    STDMETHOD(get_Lon1)(double* Value);
    STDMETHOD_(HRESULT, put_Lon1)(double Value);
    STDMETHOD(get_Lon2)(double* Value);
    STDMETHOD_(HRESULT, put_Lon2)(double Value);
    STDMETHOD(get_Volts)(double* Value);
    STDMETHOD_(HRESULT, put_Volts)(double Value);
    STDMETHOD(get_Count)(long* Value);
    STDMETHOD(get_First)(long* Value);
    STDMETHOD(get_Next)(long* Value);
};

OBJECT_ENTRY_AUTO(__uuidof(GICSources), CGICSources)
