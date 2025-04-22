
// Relays.h : Declaration of the CRelays

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CRelays
class ATL_NO_VTABLE CRelays :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CRelays, &CLSID_Relays>,
    public ISupportErrorInfo,
    public IDispatchImpl<IRelays, &IID_IRelays, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CRelays, &IID_IRelays>

{
public:
    CRelays()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_RELAYS)

BEGIN_COM_MAP(CRelays)
    COM_INTERFACE_ENTRY(IRelays)
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
    STDMETHOD(get_MonitoredObj)(BSTR* Value);
    STDMETHOD_(HRESULT, put_MonitoredObj)(BSTR Value);
    STDMETHOD(get_MonitoredTerm)(long* Value);
    STDMETHOD_(HRESULT, put_MonitoredTerm)(long Value);
    STDMETHOD(get_SwitchedObj)(BSTR* Value);
    STDMETHOD_(HRESULT, put_SwitchedObj)(BSTR Value);
    STDMETHOD(get_SwitchedTerm)(long* Value);
    STDMETHOD_(HRESULT, put_SwitchedTerm)(long Value);
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
    STDMETHOD(Open)();
    STDMETHOD(Close)();
    STDMETHOD(Reset)();
    STDMETHOD(get_State)(ActionCodes* Value);
    STDMETHOD_(HRESULT, put_State)(ActionCodes Value);
    STDMETHOD(get_NormalState)(ActionCodes* Value);
    STDMETHOD_(HRESULT, put_NormalState)(ActionCodes Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Relays), CRelays)
