
// Bus.h : Declaration of the CBus

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CBus
class ATL_NO_VTABLE CBus :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CBus, &CLSID_Bus>,
    public ISupportErrorInfo,
    public IDispatchImpl<IBus, &IID_IBus, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CBus, &IID_IBus>

{
public:
    CBus()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_BUS)

BEGIN_COM_MAP(CBus)
    COM_INTERFACE_ENTRY(IBus)
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
    STDMETHOD(get_Name)(BSTR* Name);
    STDMETHOD(get_NumNodes)(long* NumNodes);
    STDMETHOD(get_Voltages)(VARIANT* Voltages);
    STDMETHOD(get_SeqVoltages)(VARIANT* SeqVoltages);
    STDMETHOD(get_Nodes)(VARIANT* Nodes);
    STDMETHOD(get_Voc)(VARIANT* Voc);
    STDMETHOD(get_Isc)(VARIANT* Isc);
    STDMETHOD(get_puVoltages)(VARIANT* Value);
    STDMETHOD(get_kVBase)(double* Value);
    STDMETHOD(get_ZscMatrix)(VARIANT* Value);
    STDMETHOD(get_Zsc1)(VARIANT* Value);
    STDMETHOD(get_Zsc0)(VARIANT* Value);
    STDMETHOD(ZscRefresh)(VARIANT_BOOL* Value);
    STDMETHOD(get_YscMatrix)(VARIANT* Value);
    STDMETHOD(get_Coorddefined)(VARIANT_BOOL* Value);
    STDMETHOD(get_x)(double* Value);
    STDMETHOD_(HRESULT, put_x)(double Value);
    STDMETHOD(get_y)(double* Value);
    STDMETHOD_(HRESULT, put_y)(double Value);
    STDMETHOD(get_Distance)(double* Value);
    STDMETHOD_(HRESULT, GetUniqueNodeNumber)(long StartNumber, long* Value);
    STDMETHOD(get_CplxSeqVoltages)(VARIANT* Value);
    STDMETHOD(get_Lambda)(double* Value);
    STDMETHOD(get_N_interrupts)(double* Value);
    STDMETHOD(get_Int_Duration)(double* Value);
    STDMETHOD(get_Cust_Interrupts)(double* Value);
    STDMETHOD(get_Cust_Duration)(double* Value);
    STDMETHOD(get_N_Customers)(long* Value);
    STDMETHOD(get_VLL)(VARIANT* Value);
    STDMETHOD(get_puVLL)(VARIANT* Value);
    STDMETHOD(get_VMagAngle)(VARIANT* Value);
    STDMETHOD(get_puVmagAngle)(VARIANT* Value);
    STDMETHOD(get_TotalMiles)(double* Value);
    STDMETHOD(get_SectionID)(long* Value);
    STDMETHOD(get_LineList)(VARIANT* Value);
    STDMETHOD(get_LoadList)(VARIANT* Value);
    STDMETHOD(get_ZSC012Matrix)(VARIANT* Value);
    STDMETHOD(get_Latitude)(double* Value);
    STDMETHOD_(HRESULT, put_Latitude)(double Value);
    STDMETHOD(get_Longitude)(double* Value);
    STDMETHOD_(HRESULT, put_Longitude)(double Value);
    STDMETHOD(get_AllPCEatBus)(VARIANT* Value);
    STDMETHOD(get_AllPDEatBus)(VARIANT* Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Bus), CBus)
