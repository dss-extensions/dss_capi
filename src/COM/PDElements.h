
// PDElements.h : Declaration of the CPDElements

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CPDElements
class ATL_NO_VTABLE CPDElements :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CPDElements, &CLSID_PDElements>,
    public ISupportErrorInfo,
    public IDispatchImpl<IPDElements, &IID_IPDElements, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CPDElements, &IID_IPDElements>

{
public:
    CPDElements()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_PDELEMENTS)

BEGIN_COM_MAP(CPDElements)
    COM_INTERFACE_ENTRY(IPDElements)
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
    STDMETHOD(get_IsShunt)(VARIANT_BOOL* Value);
    STDMETHOD(get_FaultRate)(double* Value);
    STDMETHOD_(HRESULT, put_FaultRate)(double Value);
    STDMETHOD(get_pctPermanent)(double* Value);
    STDMETHOD_(HRESULT, put_pctPermanent)(double Value);
    STDMETHOD(get_Name)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Name)(BSTR Value);
    STDMETHOD(get_Lambda)(double* Value);
    STDMETHOD(get_AccumulatedL)(double* Value);
    STDMETHOD(get_Numcustomers)(long* Value);
    STDMETHOD(get_TotalCustomers)(long* Value);
    STDMETHOD(get_ParentPDElement)(long* Value);
    STDMETHOD(get_FromTerminal)(long* Value);
    STDMETHOD(get_TotalMiles)(double* Value);
    STDMETHOD(get_SectionID)(long* Value);
    STDMETHOD(get_RepairTime)(double* Value);
    STDMETHOD_(HRESULT, put_RepairTime)(double Value);
};

OBJECT_ENTRY_AUTO(__uuidof(PDElements), CPDElements)
