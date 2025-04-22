
// Circuit.h : Declaration of the CCircuit

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CCircuit
class ATL_NO_VTABLE CCircuit :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CCircuit, &CLSID_Circuit>,
    public ISupportErrorInfo,
    public IDispatchImpl<ICircuit, &IID_ICircuit, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CCircuit, &IID_ICircuit>

{
public:
    CCircuit()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_CIRCUIT)

BEGIN_COM_MAP(CCircuit)
    COM_INTERFACE_ENTRY(ICircuit)
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
    STDMETHOD(get_NumCktElements)(long* Value);
    STDMETHOD(get_NumBuses)(long* Value);
    STDMETHOD(get_NumNodes)(long* Value);
    STDMETHOD_(HRESULT, get_Buses)(VARIANT Index, IBus** Value);
    STDMETHOD_(HRESULT, get_CktElements)(VARIANT idx, ICktElement** Value);
    STDMETHOD(get_Losses)(VARIANT* Value);
    STDMETHOD(get_LineLosses)(VARIANT* Value);
    STDMETHOD(get_SubstationLosses)(VARIANT* Value);
    STDMETHOD(get_TotalPower)(VARIANT* Value);
    STDMETHOD(get_AllBusVolts)(VARIANT* Value);
    STDMETHOD(get_AllBusVmag)(VARIANT* Value);
    STDMETHOD(get_AllElementNames)(VARIANT* Value);
    STDMETHOD(get_ActiveElement)(ICktElement** Value);
    STDMETHOD_(HRESULT, Disable)(BSTR myName);
    STDMETHOD_(HRESULT, Enable)(BSTR myName);
    STDMETHOD(get_Solution)(ISolution** Value);
    STDMETHOD(get_ActiveBus)(IBus** Value);
    STDMETHOD(FirstPCElement)(long* Value);
    STDMETHOD(NextPCElement)(long* Value);
    STDMETHOD(FirstPDElement)(long* Value);
    STDMETHOD(NextPDElement)(long* Value);
    STDMETHOD(get_AllBusNames)(VARIANT* Value);
    STDMETHOD(get_AllElementLosses)(VARIANT* Value);
    STDMETHOD(Sample)();
    STDMETHOD(SaveSample)();
    STDMETHOD(get_Monitors)(IMonitors** Value);
    STDMETHOD(get_Meters)(IMeters** Value);
    STDMETHOD(get_Generators)(IGenerators** Value);
    STDMETHOD(get_Settings)(ISettings** Value);
    STDMETHOD(get_Lines)(ILines** Value);
    STDMETHOD_(HRESULT, SetActiveElement)(BSTR FullName, long* Value);
    STDMETHOD_(HRESULT, Capacity)(double Start, double Increment, double* Value);
    STDMETHOD_(HRESULT, SetActiveBus)(BSTR BusName, long* Value);
    STDMETHOD_(HRESULT, SetActiveBusi)(long BusIndex, long* Value);
    STDMETHOD(get_AllBusVmagPu)(VARIANT* Value);
    STDMETHOD(get_AllNodeNames)(VARIANT* Value);
    STDMETHOD(get_SystemY)(VARIANT* Value);
    STDMETHOD(get_CtrlQueue)(ICtrlQueue** Value);
    STDMETHOD(get_AllBusDistances)(VARIANT* Value);
    STDMETHOD(get_AllNodeDistances)(VARIANT* Value);
    STDMETHOD_(HRESULT, get_AllNodeVmagByPhase)(long Phase, VARIANT* Value);
    STDMETHOD_(HRESULT, get_AllNodeVmagPUByPhase)(long Phase, VARIANT* Value);
    STDMETHOD_(HRESULT, get_AllNodeDistancesByPhase)(long Phase, VARIANT* Value);
    STDMETHOD_(HRESULT, get_AllNodeNamesByPhase)(long Phase, VARIANT* Value);
    STDMETHOD(get_Loads)(ILoads** Value);
    STDMETHOD(FirstElement)(long* Value);
    STDMETHOD(NextElement)(long* Value);
    STDMETHOD_(HRESULT, SetActiveClass)(BSTR ClassName, long* Value);
    STDMETHOD(get_ActiveDSSElement)(IDSSElement** Value);
    STDMETHOD(get_ActiveCktElement)(ICktElement** Value);
    STDMETHOD(get_ActiveClass)(IActiveClass** Value);
    STDMETHOD(get_Transformers)(ITransformers** Value);
    STDMETHOD(get_SwtControls)(ISwtControls** Value);
    STDMETHOD(get_CapControls)(ICapControls** Value);
    STDMETHOD(get_RegControls)(IRegControls** Value);
    STDMETHOD(get_Capacitors)(ICapacitors** Value);
    STDMETHOD(get_Topology)(ITopology** Value);
    STDMETHOD(get_Sensors)(ISensors** Value);
    STDMETHOD(UpdateStorage)();
    STDMETHOD(get_ParentPDElement)(long* Value);
    STDMETHOD(get_XYCurves)(IXYCurves** Value);
    STDMETHOD(get_PDElements)(IPDElements** Value);
    STDMETHOD(get_Reclosers)(IReclosers** Value);
    STDMETHOD(get_Relays)(IRelays** Value);
    STDMETHOD(get_LoadShapes)(ILoadShapes** Value);
    STDMETHOD(get_Fuses)(IFuses** Value);
    STDMETHOD(get_ISources)(IISources** Value);
    STDMETHOD(get_YNodeVarray)(VARIANT* Value);
    STDMETHOD(EndOfTimeStepUpdate)();
    STDMETHOD(get_DSSim_Coms)(IDSSimComs** Value);
    STDMETHOD(get_YNodeOrder)(VARIANT* Value);
    STDMETHOD(get_YCurrents)(VARIANT* Value);
    STDMETHOD(get_PVSystems)(IPVSystems** Value);
    STDMETHOD(get_Vsources)(IVsources** Value);
    STDMETHOD(get_Parallel)(IParallel** Value);
    STDMETHOD(get_LineCodes)(ILineCodes** Value);
    STDMETHOD(get_GICSources)(IGICSources** Value);
    STDMETHOD(get_ReduceCkt)(IReduceCkt** Value);
    STDMETHOD(get_Storages)(IStorages** Value);
    STDMETHOD(get_WindGens)(IWindGens** Value);
    STDMETHOD(get_Reactors)(IReactors** Value);
    STDMETHOD_(HRESULT, ElementLosses)(VARIANT Elements, VARIANT* Value);
    STDMETHOD_(HRESULT, ToJSON)(long options, BSTR* Value);
    STDMETHOD_(HRESULT, FromJSON)(BSTR data, long options);
    STDMETHOD_(HRESULT, Save)(BSTR dirOrFilePath, long saveFlags, BSTR* Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Circuit), CCircuit)
