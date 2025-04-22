// Topology.cpp : Implementation of CTopology

#include "pch.h"
#include "Topology.h"


STDMETHODIMP CTopology::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_ITopology
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

STDMETHODIMP CTopology::get_NumLoops(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Topology_Get_NumLoops(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTopology::get_NumIsolatedBranches(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Topology_Get_NumIsolatedBranches(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTopology::get_AllLoopedPairs(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Topology_Get_AllLoopedPairs, Value);
}

STDMETHODIMP CTopology::get_AllIsolatedBranches(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Topology_Get_AllIsolatedBranches, Value);
}

STDMETHODIMP CTopology::get_NumIsolatedLoads(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Topology_Get_NumIsolatedLoads(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTopology::get_AllIsolatedLoads(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Topology_Get_AllIsolatedLoads, Value);
}

STDMETHODIMP CTopology::get_BranchName(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Topology_Get_BranchName(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTopology::put_BranchName(BSTR Value)
{
    dss_capi.Topology_Set_BranchName(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTopology::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Topology_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTopology::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Topology_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTopology::get_ActiveBranch(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Topology_Get_ActiveBranch(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTopology::get_ForwardBranch(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Topology_Get_ForwardBranch(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTopology::get_BackwardBranch(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Topology_Get_BackwardBranch(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTopology::get_LoopedBranch(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Topology_Get_LoopedBranch(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTopology::get_ParallelBranch(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Topology_Get_ParallelBranch(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTopology::get_FirstLoad(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Topology_Get_FirstLoad(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTopology::get_NextLoad(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Topology_Get_NextLoad(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTopology::get_ActiveLevel(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Topology_Get_ActiveLevel(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTopology::get_BusName(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Topology_Get_BusName(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CTopology::put_BusName(BSTR Value)
{
    dss_capi.Topology_Set_BusName(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

