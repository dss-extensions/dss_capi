// CktElement.cpp : Implementation of CCktElement

#include "pch.h"
#include "CktElement.h"

CComPtr<IDSSProperty> FDSSProperty;


STDMETHODIMP CCktElement::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_ICktElement
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

STDMETHODIMP CCktElement::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.CktElement_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_NumTerminals(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_NumTerminals(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_NumConductors(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_NumConductors(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_NumPhases(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_NumPhases(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_BusNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.CktElement_Get_BusNames, Value, false);
}

STDMETHODIMP CCktElement::put_BusNames(VARIANT Value)
{
    return AltDSS_COM_SetStrs(dss_capi.CktElement_Set_BusNames, Value);
}

STDMETHODIMP CCktElement::get_Properties(VARIANT Indx, IDSSProperty** Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    int32_t i;
    switch (Indx.vt)
    {
        case VT_I2:
        case VT_I4:
        {
            i = static_cast<int32_t>((Indx.vt == VT_I4) ? Indx.lVal : Indx.iVal);
            dss_capi.DSSProperty_Set_Index(dss_capi_ctx, i);
            break;
        }
        case VT_BSTR:
        {
            dss_capi.DSSProperty_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Indx.bstrVal));
            break;
        }
        default:
        {
            return E_INVALIDARG;
        }
    }
    return AltDSS_COM_CheckError();

}

STDMETHODIMP CCktElement::get_Voltages(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CktElement_Get_Voltages_GR, Value);
}

STDMETHODIMP CCktElement::get_Currents(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CktElement_Get_Currents_GR, Value);
}

STDMETHODIMP CCktElement::get_Powers(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CktElement_Get_Powers_GR, Value);
}

STDMETHODIMP CCktElement::get_Losses(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CktElement_Get_Losses_GR, Value);
}

STDMETHODIMP CCktElement::get_PhaseLosses(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CktElement_Get_PhaseLosses_GR, Value);
}

STDMETHODIMP CCktElement::get_SeqVoltages(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CktElement_Get_SeqVoltages_GR, Value);
}

STDMETHODIMP CCktElement::get_SeqCurrents(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CktElement_Get_SeqCurrents_GR, Value);
}

STDMETHODIMP CCktElement::get_SeqPowers(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CktElement_Get_SeqPowers_GR, Value);
}

STDMETHODIMP CCktElement::get_Enabled(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_Enabled(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::put_Enabled(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.CktElement_Set_Enabled(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_NormalAmps(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_NormalAmps(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::put_NormalAmps(double Value)
{
    dss_capi.CktElement_Set_NormalAmps(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_EmergAmps(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_EmergAmps(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::put_EmergAmps(double Value)
{
    dss_capi.CktElement_Set_EmergAmps(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::Open(long Term, long Phs)
{
    dss_capi.CktElement_Open(dss_capi_ctx, static_cast<int32_t>(Term), static_cast<int32_t>(Phs));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::Close(long Term, long Phs)
{
    dss_capi.CktElement_Close(dss_capi_ctx, static_cast<int32_t>(Term), static_cast<int32_t>(Phs));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::IsOpen(long Term, long Phs, VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_IsOpen(dss_capi_ctx, static_cast<int32_t>(Term), static_cast<int32_t>(Phs)) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_NumProperties(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_NumProperties(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_AllPropertyNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.CktElement_Get_AllPropertyNames, Value);
}

STDMETHODIMP CCktElement::get_Residuals(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CktElement_Get_Residuals_GR, Value);
}

STDMETHODIMP CCktElement::get_Yprim(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CktElement_Get_Yprim_GR, Value);
}

STDMETHODIMP CCktElement::get_DisplayName(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.CktElement_Get_DisplayName(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::put_DisplayName(BSTR Value)
{
    dss_capi.CktElement_Set_DisplayName(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_Handle(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_Handle(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_GUID(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.CktElement_Get_GUID(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_HasSwitchControl(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_HasSwitchControl(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_HasVoltControl(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_HasVoltControl(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_EnergyMeter(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.CktElement_Get_EnergyMeter(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_Controller(long idx, BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.CktElement_Get_Controller(dss_capi_ctx, static_cast<int32_t>(idx))).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_CplxSeqVoltages(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CktElement_Get_CplxSeqVoltages_GR, Value);
}

STDMETHODIMP CCktElement::get_CplxSeqCurrents(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CktElement_Get_CplxSeqCurrents_GR, Value);
}

STDMETHODIMP CCktElement::get_AllVariableNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.CktElement_Get_AllVariableNames, Value);
}

STDMETHODIMP CCktElement::get_AllVariableValues(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CktElement_Get_AllVariableValues_GR, Value);
}

STDMETHODIMP CCktElement::get_Variable(BSTR MyVarName, long* Code, double* Value)
{
    if (Code == nullptr)
    {
        return E_POINTER;
    }
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_Variable(dss_capi_ctx, AltDSS_COM_CString(MyVarName), reinterpret_cast<int32_t*>(Code));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_Variablei(long idx, long* Code, double* Value)
{
    if (Code == nullptr)
    {
        return E_POINTER;
    }
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_Variablei(dss_capi_ctx, static_cast<int32_t>(idx), reinterpret_cast<int32_t*>(Code));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_NodeOrder(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetInt32s(dss_capi.CktElement_Get_NodeOrder_GR, Value);
}

STDMETHODIMP CCktElement::get_HasOCPDevice(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_HasOCPDevice(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_NumControls(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_NumControls(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_OCPDevIndex(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_OCPDevIndex(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_OCPDevType(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_OCPDevType(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_CurrentsMagAng(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CktElement_Get_CurrentsMagAng_GR, Value);
}

STDMETHODIMP CCktElement::get_VoltagesMagAng(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CktElement_Get_VoltagesMagAng_GR, Value);
}

STDMETHODIMP CCktElement::get_TotalPowers(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.CktElement_Get_TotalPowers_GR, Value);
}

STDMETHODIMP CCktElement::get_VariableByName(BSTR MyVarName, long* Code, double* Value)
{
    if (Code == nullptr)
    {
        return E_POINTER;
    }
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_Variable(dss_capi_ctx, AltDSS_COM_CString(MyVarName), reinterpret_cast<int32_t*>(Code));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::put_VariableByName(BSTR MyVarName, long* Code, double Value)
{
    if (Code == nullptr)
    {
        return E_POINTER;
    }
    dss_capi.CktElement_Set_Variable(dss_capi_ctx, AltDSS_COM_CString(MyVarName), reinterpret_cast<int32_t*>(Code), Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_VariableByIndex(long idx, long* Code, double* Value)
{
    if (Code == nullptr)
    {
        return E_POINTER;
    }
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_Variablei(dss_capi_ctx, static_cast<int32_t>(idx), reinterpret_cast<int32_t*>(Code));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::put_VariableByIndex(long idx, long* Code, double Value)
{
    if (Code == nullptr)
    {
        return E_POINTER;
    }
    dss_capi.CktElement_Set_Variablei(dss_capi_ctx, static_cast<int32_t>(idx), reinterpret_cast<int32_t*>(Code), Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_VariableName(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.CktElement_Get_VariableName(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::put_VariableName(BSTR Value)
{
    dss_capi.CktElement_Set_VariableName(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_VariableValue(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_VariableValue(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::put_VariableValue(double Value)
{
    dss_capi.CktElement_Set_VariableValue(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_VariableIdx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_VariableIdx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::put_VariableIdx(long Value)
{
    dss_capi.CktElement_Set_VariableIdx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_IsIsolated(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CktElement_Get_IsIsolated(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCktElement::get_NodeRef(VARIANT* Nodes)
{
    if (Nodes == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetInt32s(dss_capi.CktElement_Get_NodeRef_GR, Nodes);
}

