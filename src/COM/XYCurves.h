
// XYCurves.h : Declaration of the CXYCurves

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CXYCurves
class ATL_NO_VTABLE CXYCurves :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CXYCurves, &CLSID_XYCurves>,
    public ISupportErrorInfo,
    public IDispatchImpl<IXYCurves, &IID_IXYCurves, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CXYCurves, &IID_IXYCurves>

{
public:
    CXYCurves()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_XYCURVES)

BEGIN_COM_MAP(CXYCurves)
    COM_INTERFACE_ENTRY(IXYCurves)
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
    STDMETHOD(get_Count)(long* Value);
    STDMETHOD(get_First)(long* Value);
    STDMETHOD(get_Next)(long* Value);
    STDMETHOD(get_Name)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Name)(BSTR Value);
    STDMETHOD(get_Npts)(long* Value);
    STDMETHOD_(HRESULT, put_Npts)(long Value);
    STDMETHOD(get_Xarray)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_Xarray)(VARIANT Value);
    STDMETHOD(get_Yarray)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_Yarray)(VARIANT Value);
    STDMETHOD(get_x)(double* Value);
    STDMETHOD_(HRESULT, put_x)(double Value);
    STDMETHOD(get_y)(double* Value);
    STDMETHOD_(HRESULT, put_y)(double Value);
    STDMETHOD(get_Xshift)(double* Value);
    STDMETHOD_(HRESULT, put_Xshift)(double Value);
    STDMETHOD(get_Yshift)(double* Value);
    STDMETHOD_(HRESULT, put_Yshift)(double Value);
    STDMETHOD(get_Xscale)(double* Value);
    STDMETHOD_(HRESULT, put_Xscale)(double Value);
    STDMETHOD(get_Yscale)(double* Value);
    STDMETHOD_(HRESULT, put_Yscale)(double Value);
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
};

OBJECT_ENTRY_AUTO(__uuidof(XYCurves), CXYCurves)
