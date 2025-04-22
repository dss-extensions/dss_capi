
// Reactors.h : Declaration of the CReactors

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CReactors
class ATL_NO_VTABLE CReactors :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CReactors, &CLSID_Reactors>,
    public ISupportErrorInfo,
    public IDispatchImpl<IReactors, &IID_IReactors, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CReactors, &IID_IReactors>

{
public:
    CReactors()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_REACTORS)

BEGIN_COM_MAP(CReactors)
    COM_INTERFACE_ENTRY(IReactors)
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
    STDMETHOD(get_First)(long* Value);
    STDMETHOD(get_Next)(long* Value);
    STDMETHOD(get_Count)(long* Value);
    STDMETHOD(get_Name)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Name)(BSTR Value);
    STDMETHOD(get_kV)(double* Value);
    STDMETHOD_(HRESULT, put_kV)(double Value);
    STDMETHOD(get_kvar)(double* Value);
    STDMETHOD_(HRESULT, put_kvar)(double Value);
    STDMETHOD(get_LCurve)(BSTR* Value);
    STDMETHOD_(HRESULT, put_LCurve)(BSTR Value);
    STDMETHOD(get_lmH)(double* Value);
    STDMETHOD_(HRESULT, put_lmH)(double Value);
    STDMETHOD(get_Parallel)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_Parallel)(VARIANT_BOOL Value);
    STDMETHOD(get_R)(double* Value);
    STDMETHOD_(HRESULT, put_R)(double Value);
    STDMETHOD(get_RCurve)(BSTR* Value);
    STDMETHOD_(HRESULT, put_RCurve)(BSTR Value);
    STDMETHOD(get_Rmatrix)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_Rmatrix)(VARIANT Value);
    STDMETHOD(get_Rp)(double* Value);
    STDMETHOD_(HRESULT, put_Rp)(double Value);
    STDMETHOD(get_x)(double* Value);
    STDMETHOD_(HRESULT, put_x)(double Value);
    STDMETHOD(get_Xmatrix)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_Xmatrix)(VARIANT Value);
    STDMETHOD(get_Z)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_Z)(VARIANT Value);
    STDMETHOD(get_Z0)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_Z0)(VARIANT Value);
    STDMETHOD(get_Z1)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_Z1)(VARIANT Value);
    STDMETHOD(get_Z2)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_Z2)(VARIANT Value);
    STDMETHOD(get_AllNames)(VARIANT* Value);
    STDMETHOD(get_Bus1)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Bus1)(BSTR Value);
    STDMETHOD(get_Bus2)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Bus2)(BSTR Value);
    STDMETHOD(get_IsDelta)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_IsDelta)(VARIANT_BOOL Value);
    STDMETHOD(get_Phases)(long* Value);
    STDMETHOD_(HRESULT, put_Phases)(long Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Reactors), CReactors)
