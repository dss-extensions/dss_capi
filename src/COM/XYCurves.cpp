// XYCurves.cpp : Implementation of CXYCurves

#include "pch.h"
#include "XYCurves.h"


STDMETHODIMP CXYCurves::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_IXYCurves
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

STDMETHODIMP CXYCurves::get_Count(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.XYCurves_Get_Count(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CXYCurves::get_First(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.XYCurves_Get_First(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CXYCurves::get_Next(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.XYCurves_Get_Next(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CXYCurves::get_Name(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.XYCurves_Get_Name(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CXYCurves::put_Name(BSTR Value)
{
    dss_capi.XYCurves_Set_Name(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CXYCurves::get_Npts(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.XYCurves_Get_Npts(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CXYCurves::put_Npts(long Value)
{
    dss_capi.XYCurves_Set_Npts(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CXYCurves::get_Xarray(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.XYCurves_Get_Xarray_GR, Value);
}

STDMETHODIMP CXYCurves::put_Xarray(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.XYCurves_Set_Xarray, Value);
}

STDMETHODIMP CXYCurves::get_Yarray(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetDoubles(dss_capi.XYCurves_Get_Yarray_GR, Value);
}

STDMETHODIMP CXYCurves::put_Yarray(VARIANT Value)
{
    return AltDSS_COM_SetDoubles(dss_capi.XYCurves_Set_Yarray, Value);
}

STDMETHODIMP CXYCurves::get_x(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.XYCurves_Get_x(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CXYCurves::put_x(double Value)
{
    dss_capi.XYCurves_Set_x(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CXYCurves::get_y(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.XYCurves_Get_y(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CXYCurves::put_y(double Value)
{
    dss_capi.XYCurves_Set_y(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CXYCurves::get_Xshift(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.XYCurves_Get_Xshift(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CXYCurves::put_Xshift(double Value)
{
    dss_capi.XYCurves_Set_Xshift(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CXYCurves::get_Yshift(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.XYCurves_Get_Yshift(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CXYCurves::put_Yshift(double Value)
{
    dss_capi.XYCurves_Set_Yshift(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CXYCurves::get_Xscale(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.XYCurves_Get_Xscale(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CXYCurves::put_Xscale(double Value)
{
    dss_capi.XYCurves_Set_Xscale(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CXYCurves::get_Yscale(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.XYCurves_Get_Yscale(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CXYCurves::put_Yscale(double Value)
{
    dss_capi.XYCurves_Set_Yscale(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CXYCurves::get_idx(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.XYCurves_Get_idx(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CXYCurves::put_idx(long Value)
{
    dss_capi.XYCurves_Set_idx(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

