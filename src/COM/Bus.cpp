// Bus.cpp : Implementation of CBus

#include "pch.h"
#include "Bus.h"


STDMETHODIMP CBus::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IBus
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

STDMETHODIMP CBus::get_Name(BSTR* Name)
{
    if (Name == nullptr)
    {
        return E_POINTER;
    }
    *Name = CComBSTR(dss_capi.Bus_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::get_NumNodes(long* NumNodes)
{
    if (NumNodes == nullptr)
    {
        return E_POINTER;
    }
    *NumNodes = dss_capi.Bus_Get_NumNodes(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::get_Voltages(VARIANT* Voltages)
{
    if (Voltages == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Bus_Get_Voltages_GR, Voltages);
}

STDMETHODIMP CBus::get_SeqVoltages(VARIANT* SeqVoltages)
{
    if (SeqVoltages == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Bus_Get_SeqVoltages_GR, SeqVoltages);
}

STDMETHODIMP CBus::get_Nodes(VARIANT* Nodes)
{
    if (Nodes == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetInt32s(dss_capi.Bus_Get_Nodes_GR, Nodes);
}

STDMETHODIMP CBus::get_Voc(VARIANT* Voc)
{
    if (Voc == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Bus_Get_Voc_GR, Voc);
}

STDMETHODIMP CBus::get_Isc(VARIANT* Isc)
{
    if (Isc == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Bus_Get_Isc_GR, Isc);
}

STDMETHODIMP CBus::get_puVoltages(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Bus_Get_puVoltages_GR, Value);
}

STDMETHODIMP CBus::get_kVBase(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Bus_Get_kVBase(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::get_ZscMatrix(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Bus_Get_ZscMatrix_GR, Value);
}

STDMETHODIMP CBus::get_Zsc1(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Bus_Get_Zsc1_GR, Value);
}

STDMETHODIMP CBus::get_Zsc0(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Bus_Get_Zsc0_GR, Value);
}

STDMETHODIMP CBus::ZscRefresh(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Bus_ZscRefresh(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::get_YscMatrix(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Bus_Get_YscMatrix_GR, Value);
}

STDMETHODIMP CBus::get_Coorddefined(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Bus_Get_Coorddefined(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::get_x(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Bus_Get_x(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::put_x(double Value)
{
    dss_capi.Bus_Set_x(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::get_y(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Bus_Get_y(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::put_y(double Value)
{
    dss_capi.Bus_Set_y(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::get_Distance(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Bus_Get_Distance(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::GetUniqueNodeNumber(long StartNumber, long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Bus_GetUniqueNodeNumber(dss_capi_ctx, static_cast<int32_t>(StartNumber));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::get_CplxSeqVoltages(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Bus_Get_CplxSeqVoltages_GR, Value);
}

STDMETHODIMP CBus::get_Lambda(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Bus_Get_Lambda(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::get_N_interrupts(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Bus_Get_N_interrupts(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::get_Int_Duration(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Bus_Get_Int_Duration(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::get_Cust_Interrupts(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Bus_Get_Cust_Interrupts(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::get_Cust_Duration(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Bus_Get_Cust_Duration(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::get_N_Customers(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Bus_Get_N_Customers(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::get_VLL(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Bus_Get_VLL_GR, Value);
}

STDMETHODIMP CBus::get_puVLL(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Bus_Get_puVLL_GR, Value);
}

STDMETHODIMP CBus::get_VMagAngle(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Bus_Get_VMagAngle_GR, Value);
}

STDMETHODIMP CBus::get_puVmagAngle(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Bus_Get_puVmagAngle_GR, Value);
}

STDMETHODIMP CBus::get_TotalMiles(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Bus_Get_TotalMiles(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::get_SectionID(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Bus_Get_SectionID(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::get_LineList(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Bus_Get_LineList, Value);
}

STDMETHODIMP CBus::get_LoadList(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Bus_Get_LoadList, Value);
}

STDMETHODIMP CBus::get_ZSC012Matrix(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.Bus_Get_ZSC012Matrix_GR, Value);
}

STDMETHODIMP CBus::get_Latitude(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Bus_Get_Latitude(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::put_Latitude(double Value)
{
    dss_capi.Bus_Set_Latitude(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::get_Longitude(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Bus_Get_Longitude(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::put_Longitude(double Value)
{
    dss_capi.Bus_Set_Longitude(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CBus::get_AllPCEatBus(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Bus_Get_AllPCEatBus, Value);
}

STDMETHODIMP CBus::get_AllPDEatBus(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Bus_Get_AllPDEatBus, Value);
}

