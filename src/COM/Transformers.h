
// Transformers.h : Declaration of the CTransformers

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CTransformers
class ATL_NO_VTABLE CTransformers :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CTransformers, &CLSID_Transformers>,
    public ISupportErrorInfo,
    public IDispatchImpl<ITransformers, &IID_ITransformers, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CTransformers, &IID_ITransformers>

{
public:
    CTransformers()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_TRANSFORMERS)

BEGIN_COM_MAP(CTransformers)
    COM_INTERFACE_ENTRY(ITransformers)
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
    STDMETHOD(get_NumWindings)(long* Value);
    STDMETHOD_(HRESULT, put_NumWindings)(long Value);
    STDMETHOD(get_XfmrCode)(BSTR* Value);
    STDMETHOD_(HRESULT, put_XfmrCode)(BSTR Value);
    STDMETHOD(get_Wdg)(long* Value);
    STDMETHOD_(HRESULT, put_Wdg)(long Value);
    STDMETHOD(get_R)(double* Value);
    STDMETHOD_(HRESULT, put_R)(double Value);
    STDMETHOD(get_Tap)(double* Value);
    STDMETHOD_(HRESULT, put_Tap)(double Value);
    STDMETHOD(get_MinTap)(double* Value);
    STDMETHOD_(HRESULT, put_MinTap)(double Value);
    STDMETHOD(get_MaxTap)(double* Value);
    STDMETHOD_(HRESULT, put_MaxTap)(double Value);
    STDMETHOD(get_NumTaps)(long* Value);
    STDMETHOD_(HRESULT, put_NumTaps)(long Value);
    STDMETHOD(get_kV)(double* Value);
    STDMETHOD_(HRESULT, put_kV)(double Value);
    STDMETHOD(get_kva)(double* Value);
    STDMETHOD_(HRESULT, put_kva)(double Value);
    STDMETHOD(get_Xneut)(double* Value);
    STDMETHOD_(HRESULT, put_Xneut)(double Value);
    STDMETHOD(get_Rneut)(double* Value);
    STDMETHOD_(HRESULT, put_Rneut)(double Value);
    STDMETHOD(get_IsDelta)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_IsDelta)(VARIANT_BOOL Value);
    STDMETHOD(get_Xhl)(double* Value);
    STDMETHOD_(HRESULT, put_Xhl)(double Value);
    STDMETHOD(get_Xht)(double* Value);
    STDMETHOD_(HRESULT, put_Xht)(double Value);
    STDMETHOD(get_Xlt)(double* Value);
    STDMETHOD_(HRESULT, put_Xlt)(double Value);
    STDMETHOD(get_Name)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Name)(BSTR Value);
    STDMETHOD(get_First)(long* Value);
    STDMETHOD(get_Next)(long* Value);
    STDMETHOD(get_AllNames)(VARIANT* Value);
    STDMETHOD(get_Count)(long* Value);
    STDMETHOD(get_WdgVoltages)(VARIANT* Value);
    STDMETHOD(get_WdgCurrents)(VARIANT* Value);
    STDMETHOD(get_strWdgCurrents)(BSTR* Value);
    STDMETHOD(get_CoreType)(long* Value);
    STDMETHOD_(HRESULT, put_CoreType)(long Value);
    STDMETHOD(get_RdcOhms)(double* Value);
    STDMETHOD_(HRESULT, put_RdcOhms)(double Value);
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
    STDMETHOD(get_LossesByType)(VARIANT* Value);
    STDMETHOD(get_AllLossesByType)(VARIANT* Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Transformers), CTransformers)
