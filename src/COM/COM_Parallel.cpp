// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Parallel.cpp : Implementation of CParallel

#include "COM_pch.h"
#include "COM_Parallel.h"


STDMETHODIMP CParallel::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IParallel
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

STDMETHODIMP CParallel::get_NumCPUs(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Parallel_Get_NumCPUs(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParallel::get_NumCores(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Parallel_Get_NumCores(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParallel::get_ActiveActor(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Parallel_Get_ActiveActor(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParallel::put_ActiveActor(long Value)
{
    dss_capi.Parallel_Set_ActiveActor(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParallel::CreateActor()
{
    dss_capi.Parallel_CreateActor(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParallel::get_ActorCPU(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Parallel_Get_ActorCPU(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParallel::put_ActorCPU(long Value)
{
    dss_capi.Parallel_Set_ActorCPU(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParallel::get_NumOfActors(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Parallel_Get_NumOfActors(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParallel::Wait()
{
    dss_capi.Parallel_Wait(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParallel::get_ActorProgress(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetInt32s(dss_capi.Parallel_Get_ActorProgress_GR, Value);
}

STDMETHODIMP CParallel::get_ActorStatus(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetInt32s(dss_capi.Parallel_Get_ActorStatus_GR, Value);
}

STDMETHODIMP CParallel::get_ActiveParallel(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Parallel_Get_ActiveParallel(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParallel::put_ActiveParallel(long Value)
{
    dss_capi.Parallel_Set_ActiveParallel(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParallel::get_ConcatenateReports(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Parallel_Get_ConcatenateReports(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CParallel::put_ConcatenateReports(long Value)
{
    dss_capi.Parallel_Set_ConcatenateReports(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

