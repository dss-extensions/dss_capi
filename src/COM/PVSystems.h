
// PVSystems.h : Declaration of the CPVSystems

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CPVSystems
class ATL_NO_VTABLE CPVSystems :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CPVSystems, &CLSID_PVSystems>,
    public ISupportErrorInfo,
    public IDispatchImpl<IPVSystems, &IID_IPVSystems, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CPVSystems, &IID_IPVSystems>

{
public:
    CPVSystems()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_PVSYSTEMS)

BEGIN_COM_MAP(CPVSystems)
    COM_INTERFACE_ENTRY(IPVSystems)
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
    STDMETHOD(get_RegisterNames)(VARIANT* Value);
    STDMETHOD(get_RegisterValues)(VARIANT* Value);
    STDMETHOD(get_First)(long* Value);
    STDMETHOD(get_Next)(long* Value);
    STDMETHOD(get_Count)(long* Value);
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
    STDMETHOD(get_Name)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Name)(BSTR Value);
    STDMETHOD(get_Irradiance)(double* Value);
    STDMETHOD_(HRESULT, put_Irradiance)(double Value);
    STDMETHOD(get_kW)(double* Value);
    STDMETHOD(get_kvar)(double* Value);
    STDMETHOD_(HRESULT, put_kvar)(double Value);
    STDMETHOD(get_PF)(double* Value);
    STDMETHOD_(HRESULT, put_PF)(double Value);
    STDMETHOD(get_kVArated)(double* Value);
    STDMETHOD_(HRESULT, put_kVArated)(double Value);
    STDMETHOD(get_Pmpp)(double* Value);
    STDMETHOD_(HRESULT, put_Pmpp)(double Value);
    STDMETHOD(get_IrradianceNow)(double* Value);
    STDMETHOD(get_Sensor)(BSTR* Value);
    STDMETHOD(get_daily)(BSTR* Value);
    STDMETHOD_(HRESULT, put_daily)(BSTR Value);
    STDMETHOD(get_duty)(BSTR* Value);
    STDMETHOD_(HRESULT, put_duty)(BSTR Value);
    STDMETHOD(get_Yearly)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Yearly)(BSTR Value);
    STDMETHOD(get_Tdaily)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Tdaily)(BSTR Value);
    STDMETHOD(get_Tduty)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Tduty)(BSTR Value);
    STDMETHOD(get_Tyearly)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Tyearly)(BSTR Value);
};

OBJECT_ENTRY_AUTO(__uuidof(PVSystems), CPVSystems)
