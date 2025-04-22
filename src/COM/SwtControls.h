
// SwtControls.h : Declaration of the CSwtControls

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CSwtControls
class ATL_NO_VTABLE CSwtControls :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CSwtControls, &CLSID_SwtControls>,
    public ISupportErrorInfo,
    public IDispatchImpl<ISwtControls, &IID_ISwtControls, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CSwtControls, &IID_ISwtControls>

{
public:
    CSwtControls()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_SWTCONTROLS)

BEGIN_COM_MAP(CSwtControls)
    COM_INTERFACE_ENTRY(ISwtControls)
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
    STDMETHOD(get_Name)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Name)(BSTR Value);
    STDMETHOD(get_First)(long* Value);
    STDMETHOD(get_Next)(long* Value);
    STDMETHOD(get_Action)(ActionCodes* Value);
    STDMETHOD_(HRESULT, put_Action)(ActionCodes Value);
    STDMETHOD(get_IsLocked)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_IsLocked)(VARIANT_BOOL Value);
    STDMETHOD(get_Delay)(double* Value);
    STDMETHOD_(HRESULT, put_Delay)(double Value);
    STDMETHOD(get_SwitchedObj)(BSTR* Value);
    STDMETHOD_(HRESULT, put_SwitchedObj)(BSTR Value);
    STDMETHOD(get_SwitchedTerm)(long* Value);
    STDMETHOD_(HRESULT, put_SwitchedTerm)(long Value);
    STDMETHOD(get_Count)(long* Value);
    STDMETHOD(get_NormalState)(ActionCodes* Value);
    STDMETHOD_(HRESULT, put_NormalState)(ActionCodes Value);
    STDMETHOD(get_State)(ActionCodes* Value);
    STDMETHOD_(HRESULT, put_State)(ActionCodes Value);
    STDMETHOD(Reset)();
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
};

OBJECT_ENTRY_AUTO(__uuidof(SwtControls), CSwtControls)
