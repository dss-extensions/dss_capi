
// Capacitors.h : Declaration of the CCapacitors

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CCapacitors
class ATL_NO_VTABLE CCapacitors :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CCapacitors, &CLSID_Capacitors>,
    public ISupportErrorInfo,
    public IDispatchImpl<ICapacitors, &IID_ICapacitors, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CCapacitors, &IID_ICapacitors>

{
public:
    CCapacitors()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_CAPACITORS)

BEGIN_COM_MAP(CCapacitors)
    COM_INTERFACE_ENTRY(ICapacitors)
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
    STDMETHOD(get_kV)(double* Value);
    STDMETHOD_(HRESULT, put_kV)(double Value);
    STDMETHOD(get_kvar)(double* Value);
    STDMETHOD_(HRESULT, put_kvar)(double Value);
    STDMETHOD(get_NumSteps)(long* Value);
    STDMETHOD_(HRESULT, put_NumSteps)(long Value);
    STDMETHOD(get_IsDelta)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_IsDelta)(VARIANT_BOOL Value);
    STDMETHOD(get_AllNames)(VARIANT* Value);
    STDMETHOD(get_First)(long* Value);
    STDMETHOD(get_Next)(long* Value);
    STDMETHOD(get_Name)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Name)(BSTR Value);
    STDMETHOD(get_Count)(long* Value);
    STDMETHOD(AddStep)(VARIANT_BOOL* Value);
    STDMETHOD(SubtractStep)(VARIANT_BOOL* Value);
    STDMETHOD(get_AvailableSteps)(long* Value);
    STDMETHOD(get_States)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_States)(VARIANT Value);
    STDMETHOD(Open)();
    STDMETHOD(Close)();
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Capacitors), CCapacitors)
