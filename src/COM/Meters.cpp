// Meters.cpp : Implementation of CMeters

#include "pch.h"
#include "Meters.h"


STDMETHODIMP CMeters::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IMeters
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

STDMETHODIMP CMeters::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Meters_Get_AllNames, Value);
}

STDMETHODIMP CMeters::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_RegisterNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Meters_Get_RegisterNames, Value);
}

STDMETHODIMP CMeters::get_RegisterValues(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Meters_Get_RegisterValues_GR, Value);
}

STDMETHODIMP CMeters::Reset()
{
    dss_capi.Meters_Reset(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::ResetAll()
{
    dss_capi.Meters_ResetAll(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::Sample()
{
    dss_capi.Meters_Sample(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::Save()
{
    dss_capi.Meters_Save(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Meters_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::put_Name(BSTR Value)
{
    dss_capi.Meters_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_Totals(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Meters_Get_Totals_GR, Value);
}

STDMETHODIMP CMeters::get_Peakcurrent(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Meters_Get_Peakcurrent_GR, Value);
}

STDMETHODIMP CMeters::put_Peakcurrent(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.Meters_Set_Peakcurrent, Value);
}

STDMETHODIMP CMeters::get_CalcCurrent(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Meters_Get_CalcCurrent_GR, Value);
}

STDMETHODIMP CMeters::put_CalcCurrent(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.Meters_Set_CalcCurrent, Value);
}

STDMETHODIMP CMeters::get_AllocFactors(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Meters_Get_AllocFactors_GR, Value);
}

STDMETHODIMP CMeters::put_AllocFactors(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.Meters_Set_AllocFactors, Value);
}

STDMETHODIMP CMeters::get_MeteredElement(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Meters_Get_MeteredElement(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::put_MeteredElement(BSTR Value)
{
    dss_capi.Meters_Set_MeteredElement(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_MeteredTerminal(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_MeteredTerminal(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::put_MeteredTerminal(long Value)
{
    dss_capi.Meters_Set_MeteredTerminal(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_DIFilesAreOpen(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_DIFilesAreOpen(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::SampleAll()
{
    dss_capi.Meters_SampleAll(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::SaveAll()
{
    dss_capi.Meters_SaveAll(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::OpenAllDIFiles()
{
    dss_capi.Meters_OpenAllDIFiles(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::CloseAllDIFiles()
{
    dss_capi.Meters_CloseAllDIFiles(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_CountEndElements(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_CountEndElements(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_AllEndElements(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Meters_Get_AllEndElements, Value);
}

STDMETHODIMP CMeters::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_AllBranchesInZone(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Meters_Get_AllBranchesInZone, Value);
}

STDMETHODIMP CMeters::get_CountBranches(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_CountBranches(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_SAIFI(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_SAIFI(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_SequenceIndex(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_SequenceIndex(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::put_SequenceIndex(long Value)
{
    dss_capi.Meters_Set_SequenceIndex(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_SAIFIKW(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_SAIFIKW(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::DoReliabilityCalc(VARIANT_BOOL AssumeRestoration)
{
    if (AssumeRestoration != VARIANT_TRUE && AssumeRestoration != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Meters_DoReliabilityCalc(dss_capi_ctx, AssumeRestoration == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_SeqListSize(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_SeqListSize(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_TotalCustomers(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_TotalCustomers(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_SAIDI(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_SAIDI(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_CustInterrupts(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_CustInterrupts(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_NumSections(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_NumSections(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::SetActiveSection(long SectIdx)
{
    dss_capi.Meters_SetActiveSection(dss_capi_ctx, static_cast<int32_t>(SectIdx));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_OCPDeviceType(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_OCPDeviceType(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_NumSectionCustomers(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_NumSectionCustomers(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_NumSectionBranches(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_NumSectionBranches(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_AvgRepairTime(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_AvgRepairTime(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_FaultRateXRepairHrs(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_FaultRateXRepairHrs(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_SumBranchFltRates(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_SumBranchFltRates(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_SectSeqIdx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_SectSeqIdx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_SectTotalCust(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_SectTotalCust(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::get_ZonePCE(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Meters_Get_ZonePCE, Value);
}

STDMETHODIMP CMeters::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Meters_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CMeters::put_idx(long Value)
{
    dss_capi.Meters_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

