// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_DSSimComs.cpp : Implementation of CDSSimComs

#include "COM_pch.h"
#include "COM_DSSimComs.h"


STDMETHODIMP CDSSimComs::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IDSSimComs
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

STDMETHODIMP CDSSimComs::BusVoltagepu(unsigned int Index, VARIANT* Vpu)
{
    if (Vpu == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.DSSimComs_BusVoltagepu_GR, Vpu, static_cast<int32_t>(Index));
}

STDMETHODIMP CDSSimComs::BusVoltage(unsigned int Index, VARIANT* Voltages)
{
    if (Voltages == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.DSSimComs_BusVoltage_GR, Voltages, static_cast<int32_t>(Index));
}

