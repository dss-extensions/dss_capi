// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Circuit.cpp : Implementation of CCircuit

#include "COM_pch.h"
#include "COM_Circuit.h"

extern CComPtr<IActiveClass> FActiveClass;
CComPtr<IBus> FBus;
CComPtr<ICapControls> FCapControls;
CComPtr<ICapacitors> FCapacitors;
CComPtr<ICktElement> FCktElement;
CComPtr<ICtrlQueue> FCtrlQueue;
CComPtr<IDSSElement> FDSSElement;
extern CComPtr<IDSSimComs> FDSSimComs;
CComPtr<IFuses> FFuses;
CComPtr<IGICSources> FGICSources;
CComPtr<IGenerators> FGenerators;
CComPtr<IISources> FISources;
CComPtr<ILineCodes> FLineCodes;
CComPtr<ILines> FLines;
CComPtr<ILoadShapes> FLoadShapes;
CComPtr<ILoads> FLoads;
CComPtr<IMeters> FMeters;
CComPtr<IMonitors> FMonitors;
CComPtr<IPDElements> FPDElements;
CComPtr<IPVSystems> FPVSystems;
CComPtr<IParallel> FParallel;
CComPtr<IReactors> FReactors;
CComPtr<IReclosers> FReclosers;
CComPtr<IReduceCkt> FReduceCkt;
CComPtr<IRegControls> FRegControls;
CComPtr<IRelays> FRelays;
CComPtr<ISensors> FSensors;
CComPtr<ISettings> FSettings;
CComPtr<ISolution> FSolution;
CComPtr<IStorages> FStorages;
CComPtr<ISwtControls> FSwtControls;
CComPtr<ITopology> FTopology;
CComPtr<ITransformers> FTransformers;
CComPtr<IVsources> FVsources;
CComPtr<IWindGens> FWindGens;
CComPtr<IXYCurves> FXYCurves;


STDMETHODIMP CCircuit::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_ICircuit
    };

    for (int i = 0; i < sizeof(arr) / sizeof(arr[0]); i++)
    {
        if (InlineIsEqualGUID(*arr[i], riid))
        {
            return S_OK;
        }
    }
    return S_FALSE;
}

STDMETHODIMP CCircuit::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Circuit_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::get_NumCktElements(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Circuit_Get_NumCktElements(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::get_NumBuses(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Circuit_Get_NumBuses(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::get_NumNodes(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Circuit_Get_NumNodes(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::get_Buses(VARIANT Index, IBus** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    int32_t i;
    switch (Index.vt)
    {
        case VT_I2:
        case VT_I4:
        {
            i = static_cast<int32_t>((Index.vt == VT_I4) ? Index.lVal : Index.iVal);
            dss_capi.Circuit_SetActiveBusi(dss_capi_ctx, i);
            break;
        }
        case VT_BSTR:  
        {
            dss_capi.Circuit_SetActiveBus(dss_capi_ctx, AltDSS_COM_CString(Index.bstrVal));
            break;
        }
        default:
        {
            return E_INVALIDARG;
        }
    }
    return AltDSS_COM_CheckError();

}

STDMETHODIMP CCircuit::get_CktElements(VARIANT idx, ICktElement** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    int32_t i;
    switch (idx.vt)
    {
        case VT_I2:
        case VT_I4:
        {
            i = static_cast<int32_t>((idx.vt == VT_I4) ? idx.lVal : idx.iVal);
            dss_capi.Circuit_SetCktElementIndex(dss_capi_ctx, i);
            break;
        }
        case VT_BSTR:
        {
            dss_capi.Circuit_SetActiveElement(dss_capi_ctx, AltDSS_COM_CString(idx.bstrVal));
            break;
        }
        default:
        {
            return E_INVALIDARG;
        }
    }
    return AltDSS_COM_CheckError();

}

STDMETHODIMP CCircuit::get_Losses(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Circuit_Get_Losses_GR, Value);
}

STDMETHODIMP CCircuit::get_LineLosses(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Circuit_Get_LineLosses_GR, Value);
}

STDMETHODIMP CCircuit::get_SubstationLosses(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Circuit_Get_SubstationLosses_GR, Value);
}

STDMETHODIMP CCircuit::get_TotalPower(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Circuit_Get_TotalPower_GR, Value);
}

STDMETHODIMP CCircuit::get_AllBusVolts(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Circuit_Get_AllBusVolts_GR, Value);
}

STDMETHODIMP CCircuit::get_AllBusVmag(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Circuit_Get_AllBusVmag_GR, Value);
}

STDMETHODIMP CCircuit::get_AllElementNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Circuit_Get_AllElementNames, Value);
}

STDMETHODIMP CCircuit::get_ActiveElement(ICktElement** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_CktElement, Value, FCktElement);
}

STDMETHODIMP CCircuit::Disable(BSTR myName)
{
    dss_capi.Circuit_Disable(dss_capi_ctx, AltDSS_COM_CString(myName));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::Enable(BSTR myName)
{
    dss_capi.Circuit_Enable(dss_capi_ctx, AltDSS_COM_CString(myName));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::get_Solution(ISolution** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Solution, Value, FSolution);
}

STDMETHODIMP CCircuit::get_ActiveBus(IBus** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Bus, Value, FBus);
}

STDMETHODIMP CCircuit::FirstPCElement(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Circuit_FirstPCElement(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::NextPCElement(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Circuit_NextPCElement(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::FirstPDElement(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Circuit_FirstPDElement(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::NextPDElement(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Circuit_NextPDElement(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::get_AllBusNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Circuit_Get_AllBusNames, Value);
}

STDMETHODIMP CCircuit::get_AllElementLosses(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Circuit_Get_AllElementLosses_GR, Value);
}

STDMETHODIMP CCircuit::Sample()
{
    dss_capi.Circuit_Sample(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::SaveSample()
{
    dss_capi.Circuit_SaveSample(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::get_Monitors(IMonitors** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Monitors, Value, FMonitors);
}

STDMETHODIMP CCircuit::get_Meters(IMeters** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Meters, Value, FMeters);
}

STDMETHODIMP CCircuit::get_Generators(IGenerators** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Generators, Value, FGenerators);
}

STDMETHODIMP CCircuit::get_Settings(ISettings** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Settings, Value, FSettings);
}

STDMETHODIMP CCircuit::get_Lines(ILines** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Lines, Value, FLines);
}

STDMETHODIMP CCircuit::SetActiveElement(BSTR FullName, long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Circuit_SetActiveElement(dss_capi_ctx, AltDSS_COM_CString(FullName));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::Capacity(double Start, double Increment, double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Circuit_Capacity(dss_capi_ctx, Start, Increment);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::SetActiveBus(BSTR BusName, long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Circuit_SetActiveBus(dss_capi_ctx, AltDSS_COM_CString(BusName));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::SetActiveBusi(long BusIndex, long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Circuit_SetActiveBusi(dss_capi_ctx, static_cast<int32_t>(BusIndex));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::get_AllBusVmagPu(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Circuit_Get_AllBusVmagPu_GR, Value);
}

STDMETHODIMP CCircuit::get_AllNodeNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Circuit_Get_AllNodeNames, Value);
}

STDMETHODIMP CCircuit::get_SystemY(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Circuit_Get_SystemY_GR, Value);
}

STDMETHODIMP CCircuit::get_CtrlQueue(ICtrlQueue** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_CtrlQueue, Value, FCtrlQueue);
}

STDMETHODIMP CCircuit::get_AllBusDistances(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Circuit_Get_AllBusDistances_GR, Value);
}

STDMETHODIMP CCircuit::get_AllNodeDistances(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Circuit_Get_AllNodeDistances_GR, Value);
}

STDMETHODIMP CCircuit::get_AllNodeVmagByPhase(long Phase, VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Circuit_Get_AllNodeVmagByPhase_GR, Value, static_cast<int32_t>(Phase));
}

STDMETHODIMP CCircuit::get_AllNodeVmagPUByPhase(long Phase, VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Circuit_Get_AllNodeVmagPUByPhase_GR, Value, static_cast<int32_t>(Phase));
}

STDMETHODIMP CCircuit::get_AllNodeDistancesByPhase(long Phase, VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Circuit_Get_AllNodeDistancesByPhase_GR, Value, static_cast<int32_t>(Phase));
}

STDMETHODIMP CCircuit::get_AllNodeNamesByPhase(long Phase, VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Circuit_Get_AllNodeNamesByPhase, Value, static_cast<int32_t>(Phase));
}

STDMETHODIMP CCircuit::get_Loads(ILoads** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Loads, Value, FLoads);
}

STDMETHODIMP CCircuit::FirstElement(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Circuit_FirstElement(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::NextElement(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Circuit_NextElement(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::SetActiveClass(BSTR ClassName, long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Circuit_SetActiveClass(dss_capi_ctx, AltDSS_COM_CString(ClassName));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::get_ActiveDSSElement(IDSSElement** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_DSSElement, Value, FDSSElement);
}

STDMETHODIMP CCircuit::get_ActiveCktElement(ICktElement** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_CktElement, Value, FCktElement);
}

STDMETHODIMP CCircuit::get_ActiveClass(IActiveClass** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_ActiveClass, Value, FActiveClass);
}

STDMETHODIMP CCircuit::get_Transformers(ITransformers** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Transformers, Value, FTransformers);
}

STDMETHODIMP CCircuit::get_SwtControls(ISwtControls** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_SwtControls, Value, FSwtControls);
}

STDMETHODIMP CCircuit::get_CapControls(ICapControls** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_CapControls, Value, FCapControls);
}

STDMETHODIMP CCircuit::get_RegControls(IRegControls** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_RegControls, Value, FRegControls);
}

STDMETHODIMP CCircuit::get_Capacitors(ICapacitors** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Capacitors, Value, FCapacitors);
}

STDMETHODIMP CCircuit::get_Topology(ITopology** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Topology, Value, FTopology);
}

STDMETHODIMP CCircuit::get_Sensors(ISensors** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Sensors, Value, FSensors);
}

STDMETHODIMP CCircuit::UpdateStorage()
{
    dss_capi.Circuit_UpdateStorage(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::get_ParentPDElement(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Circuit_Get_ParentPDElement(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::get_XYCurves(IXYCurves** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_XYCurves, Value, FXYCurves);
}

STDMETHODIMP CCircuit::get_PDElements(IPDElements** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_PDElements, Value, FPDElements);
}

STDMETHODIMP CCircuit::get_Reclosers(IReclosers** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Reclosers, Value, FReclosers);
}

STDMETHODIMP CCircuit::get_Relays(IRelays** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Relays, Value, FRelays);
}

STDMETHODIMP CCircuit::get_LoadShapes(ILoadShapes** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_LoadShapes, Value, FLoadShapes);
}

STDMETHODIMP CCircuit::get_Fuses(IFuses** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Fuses, Value, FFuses);
}

STDMETHODIMP CCircuit::get_ISources(IISources** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_ISources, Value, FISources);
}

STDMETHODIMP CCircuit::get_YNodeVarray(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Circuit_Get_YNodeVarray_GR, Value);
}

STDMETHODIMP CCircuit::EndOfTimeStepUpdate()
{
    dss_capi.Circuit_EndOfTimeStepUpdate(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::get_DSSim_Coms(IDSSimComs** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_DSSimComs, Value, FDSSimComs);
}

STDMETHODIMP CCircuit::get_YNodeOrder(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Circuit_Get_YNodeOrder, Value);
}

STDMETHODIMP CCircuit::get_YCurrents(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Circuit_Get_YCurrents_GR, Value);
}

STDMETHODIMP CCircuit::get_PVSystems(IPVSystems** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_PVSystems, Value, FPVSystems);
}

STDMETHODIMP CCircuit::get_Vsources(IVsources** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Vsources, Value, FVsources);
}

STDMETHODIMP CCircuit::get_Parallel(IParallel** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Parallel, Value, FParallel);
}

STDMETHODIMP CCircuit::get_LineCodes(ILineCodes** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_LineCodes, Value, FLineCodes);
}

STDMETHODIMP CCircuit::get_GICSources(IGICSources** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_GICSources, Value, FGICSources);
}

STDMETHODIMP CCircuit::get_ReduceCkt(IReduceCkt** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_ReduceCkt, Value, FReduceCkt);
}

STDMETHODIMP CCircuit::get_Storages(IStorages** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Storages, Value, FStorages);
}

STDMETHODIMP CCircuit::get_WindGens(IWindGens** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_WindGens, Value, FWindGens);
}

STDMETHODIMP CCircuit::get_Reactors(IReactors** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_EnsureInterface(CLSID_Reactors, Value, FReactors);
}

STDMETHODIMP CCircuit::ElementLosses(VARIANT Elements, VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    if (Elements.vt != VT_I4)
    {
        return E_INVALIDARG;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Circuit_Get_ElementLosses_GR, Value, AltDSS_COM_GetTempInt32Ptr(Elements), static_cast<int32_t>(Elements.parray->rgsabound[0].cElements));
}

STDMETHODIMP CCircuit::ToJSON(long options, BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Circuit_ToJSON(dss_capi_ctx, static_cast<int32_t>(options))).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::FromJSON(BSTR data, long options)
{
    dss_capi.Circuit_FromJSON(dss_capi_ctx, AltDSS_COM_CString(data), static_cast<int32_t>(options));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCircuit::Save(BSTR dirOrFilePath, long saveFlags, BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Circuit_Save(dss_capi_ctx, AltDSS_COM_CString(dirOrFilePath), static_cast<int32_t>(saveFlags))).Detach();
    return AltDSS_COM_CheckError();
}

