
// Reclosers.h : Declaration of the CReclosers

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CReclosers
class ATL_NO_VTABLE CReclosers :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CReclosers, &CLSID_Reclosers>,
    public ISupportErrorInfo,
    public IDispatchImpl<IReclosers, &IID_IReclosers, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CReclosers, &IID_IReclosers>

{
public:
    CReclosers()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_RECLOSERS)

BEGIN_COM_MAP(CReclosers)
    COM_INTERFACE_ENTRY(IReclosers)
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
    STDMETHOD(get_NumFast)(long* Value);
    STDMETHOD_(HRESULT, put_NumFast)(long Value);
    STDMETHOD(get_Shots)(long* Value);
    STDMETHOD_(HRESULT, put_Shots)(long Value);
    STDMETHOD(get_RecloseIntervals)(VARIANT* Value);
    STDMETHOD(get_PhaseTrip)(double* Value);
    STDMETHOD_(HRESULT, put_PhaseTrip)(double Value);
    STDMETHOD(get_PhaseInst)(double* Value);
    STDMETHOD_(HRESULT, put_PhaseInst)(double Value);
    STDMETHOD(get_GroundTrip)(double* Value);
    STDMETHOD_(HRESULT, put_GroundTrip)(double Value);
    STDMETHOD(get_GroundInst)(double* Value);
    STDMETHOD_(HRESULT, put_GroundInst)(double Value);
    STDMETHOD(Open)();
    STDMETHOD(Close)();
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
    STDMETHOD(Reset)();
    STDMETHOD(get_NormalState)(ActionCodes* Value);
    STDMETHOD_(HRESULT, put_NormalState)(ActionCodes Value);
    STDMETHOD(get_State)(ActionCodes* Value);
    STDMETHOD_(HRESULT, put_State)(ActionCodes Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Reclosers), CReclosers)
