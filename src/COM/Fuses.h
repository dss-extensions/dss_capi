
// Fuses.h : Declaration of the CFuses

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CFuses
class ATL_NO_VTABLE CFuses :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CFuses, &CLSID_Fuses>,
    public ISupportErrorInfo,
    public IDispatchImpl<IFuses, &IID_IFuses, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CFuses, &IID_IFuses>

{
public:
    CFuses()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_FUSES)

BEGIN_COM_MAP(CFuses)
    COM_INTERFACE_ENTRY(IFuses)
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
    STDMETHOD(get_TCCcurve)(BSTR* Value);
    STDMETHOD_(HRESULT, put_TCCcurve)(BSTR Value);
    STDMETHOD(get_RatedCurrent)(double* Value);
    STDMETHOD_(HRESULT, put_RatedCurrent)(double Value);
    STDMETHOD(get_Delay)(double* Value);
    STDMETHOD_(HRESULT, put_Delay)(double Value);
    STDMETHOD(Open)();
    STDMETHOD(Close)();
    STDMETHOD(IsBlown)(VARIANT_BOOL* Value);
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
    STDMETHOD(get_NumPhases)(long* Value);
    STDMETHOD(Reset)();
    STDMETHOD(get_State)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_State)(VARIANT Value);
    STDMETHOD(get_NormalState)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_NormalState)(VARIANT Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Fuses), CFuses)
