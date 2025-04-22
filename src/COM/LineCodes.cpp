// LineCodes.cpp : Implementation of CLineCodes

#include "pch.h"
#include "LineCodes.h"


STDMETHODIMP CLineCodes::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_ILineCodes
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

STDMETHODIMP CLineCodes::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LineCodes_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LineCodes_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LineCodes_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.LineCodes_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::put_Name(BSTR Value)
{
    dss_capi.LineCodes_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::get_IsZ1Z0(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LineCodes_Get_IsZ1Z0(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::get_Units(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LineCodes_Get_Units(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::put_Units(long Value)
{
    dss_capi.LineCodes_Set_Units(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::get_Phases(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LineCodes_Get_Phases(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::put_Phases(long Value)
{
    dss_capi.LineCodes_Set_Phases(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::get_R1(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LineCodes_Get_R1(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::put_R1(double Value)
{
    dss_capi.LineCodes_Set_R1(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::get_X1(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LineCodes_Get_X1(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::put_X1(double Value)
{
    dss_capi.LineCodes_Set_X1(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::get_R0(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LineCodes_Get_R0(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::put_R0(double Value)
{
    dss_capi.LineCodes_Set_R0(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::get_X0(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LineCodes_Get_X0(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::put_X0(double Value)
{
    dss_capi.LineCodes_Set_X0(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::get_C1(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LineCodes_Get_C1(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::put_C1(double Value)
{
    dss_capi.LineCodes_Set_C1(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::get_C0(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LineCodes_Get_C0(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::put_C0(double Value)
{
    dss_capi.LineCodes_Set_C0(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::get_Rmatrix(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.LineCodes_Get_Rmatrix_GR, Value);
}

STDMETHODIMP CLineCodes::put_Rmatrix(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.LineCodes_Set_Rmatrix, Value);
}

STDMETHODIMP CLineCodes::get_Xmatrix(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.LineCodes_Get_Xmatrix_GR, Value);
}

STDMETHODIMP CLineCodes::put_Xmatrix(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.LineCodes_Set_Xmatrix, Value);
}

STDMETHODIMP CLineCodes::get_Cmatrix(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.LineCodes_Get_Cmatrix_GR, Value);
}

STDMETHODIMP CLineCodes::put_Cmatrix(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.LineCodes_Set_Cmatrix, Value);
}

STDMETHODIMP CLineCodes::get_NormAmps(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LineCodes_Get_NormAmps(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::put_NormAmps(double Value)
{
    dss_capi.LineCodes_Set_NormAmps(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::get_EmergAmps(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LineCodes_Get_EmergAmps(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::put_EmergAmps(double Value)
{
    dss_capi.LineCodes_Set_EmergAmps(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::get_AllNames(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.LineCodes_Get_AllNames, Value);
}

STDMETHODIMP CLineCodes::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.LineCodes_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CLineCodes::put_idx(long Value)
{
    dss_capi.LineCodes_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

