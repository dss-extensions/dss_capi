
// CktElement.h : Declaration of the CCktElement

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CCktElement
class ATL_NO_VTABLE CCktElement :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CCktElement, &CLSID_CktElement>,
    public ISupportErrorInfo,
    public IDispatchImpl<ICktElement, &IID_ICktElement, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CCktElement, &IID_ICktElement>

{
public:
    CCktElement()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_CKTELEMENT)

BEGIN_COM_MAP(CCktElement)
    COM_INTERFACE_ENTRY(ICktElement)
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
    STDMETHOD(get_Name)(BSTR* Value);
    STDMETHOD(get_NumTerminals)(long* Value);
    STDMETHOD(get_NumConductors)(long* Value);
    STDMETHOD(get_NumPhases)(long* Value);
    STDMETHOD(get_BusNames)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_BusNames)(VARIANT Value);
    STDMETHOD_(HRESULT, get_Properties)(VARIANT Indx, IDSSProperty** Value);
    STDMETHOD(get_Voltages)(VARIANT* Value);
    STDMETHOD(get_Currents)(VARIANT* Value);
    STDMETHOD(get_Powers)(VARIANT* Value);
    STDMETHOD(get_Losses)(VARIANT* Value);
    STDMETHOD(get_PhaseLosses)(VARIANT* Value);
    STDMETHOD(get_SeqVoltages)(VARIANT* Value);
    STDMETHOD(get_SeqCurrents)(VARIANT* Value);
    STDMETHOD(get_SeqPowers)(VARIANT* Value);
    STDMETHOD(get_Enabled)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_Enabled)(VARIANT_BOOL Value);
    STDMETHOD(get_NormalAmps)(double* Value);
    STDMETHOD_(HRESULT, put_NormalAmps)(double Value);
    STDMETHOD(get_EmergAmps)(double* Value);
    STDMETHOD_(HRESULT, put_EmergAmps)(double Value);
    STDMETHOD_(HRESULT, Open)(long Term, long Phs);
    STDMETHOD_(HRESULT, Close)(long Term, long Phs);
    STDMETHOD_(HRESULT, IsOpen)(long Term, long Phs, VARIANT_BOOL* Value);
    STDMETHOD(get_NumProperties)(long* Value);
    STDMETHOD(get_AllPropertyNames)(VARIANT* Value);
    STDMETHOD(get_Residuals)(VARIANT* Value);
    STDMETHOD(get_Yprim)(VARIANT* Value);
    STDMETHOD(get_DisplayName)(BSTR* Value);
    STDMETHOD_(HRESULT, put_DisplayName)(BSTR Value);
    STDMETHOD(get_Handle)(long* Value);
    STDMETHOD(get_GUID)(BSTR* Value);
    STDMETHOD(get_HasSwitchControl)(VARIANT_BOOL* Value);
    STDMETHOD(get_HasVoltControl)(VARIANT_BOOL* Value);
    STDMETHOD(get_EnergyMeter)(BSTR* Value);
    STDMETHOD_(HRESULT, get_Controller)(long idx, BSTR* Value);
    STDMETHOD(get_CplxSeqVoltages)(VARIANT* Value);
    STDMETHOD(get_CplxSeqCurrents)(VARIANT* Value);
    STDMETHOD(get_AllVariableNames)(VARIANT* Value);
    STDMETHOD(get_AllVariableValues)(VARIANT* Value);
    STDMETHOD_(HRESULT, get_Variable)(BSTR MyVarName, long* Code, double* Value);
    STDMETHOD_(HRESULT, get_Variablei)(long idx, long* Code, double* Value);
    STDMETHOD(get_NodeOrder)(VARIANT* Value);
    STDMETHOD(get_HasOCPDevice)(VARIANT_BOOL* Value);
    STDMETHOD(get_NumControls)(long* Value);
    STDMETHOD(get_OCPDevIndex)(long* Value);
    STDMETHOD(get_OCPDevType)(long* Value);
    STDMETHOD(get_CurrentsMagAng)(VARIANT* Value);
    STDMETHOD(get_VoltagesMagAng)(VARIANT* Value);
    STDMETHOD(get_TotalPowers)(VARIANT* Value);
    STDMETHOD_(HRESULT, get_VariableByName)(BSTR MyVarName, long* Code, double* Value);
    STDMETHOD_(HRESULT, put_VariableByName)(BSTR MyVarName, long* Code, double Value);
    STDMETHOD_(HRESULT, get_VariableByIndex)(long idx, long* Code, double* Value);
    STDMETHOD_(HRESULT, put_VariableByIndex)(long idx, long* Code, double Value);
    STDMETHOD(get_VariableName)(BSTR* Value);
    STDMETHOD_(HRESULT, put_VariableName)(BSTR Value);
    STDMETHOD(get_VariableValue)(double* Value);
    STDMETHOD_(HRESULT, put_VariableValue)(double Value);
    STDMETHOD(get_VariableIdx)(long* Value);
    STDMETHOD_(HRESULT, put_VariableIdx)(long Value);
    STDMETHOD(get_AllLosses)(VARIANT* Value);
    STDMETHOD(get_IsIsolated)(VARIANT_BOOL* Value);
    STDMETHOD(get_NodeRef)(VARIANT* Nodes);
};

OBJECT_ENTRY_AUTO(__uuidof(CktElement), CCktElement)
