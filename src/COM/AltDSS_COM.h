#pragma once
#ifndef DSS_EXTENSIONS_ALTDSS_COM_H
#define DSS_EXTENSIONS_ALTDSS_COM_H

#ifndef ALTDSS_CAPI_DLL
#define ALTDSS_CAPI_DLL
#define ALTDSS_CAPI_NAMESPACE
#define HAS_ALTDSS_LOADER_FUNC
#include "altdss/capi/capi.h"
#endif
#include <vector>

extern void const* dss_capi_ctx;

struct AltDSSCAPIEx : public altdss::capi::AltDSSCAPI {
    bool useExceptions = true;
    int32_t* errorPtr = nullptr;

    double** dataPtr_pdouble;
    int32_t** dataPtr_pinteger;
    int8_t** dataPtr_pbyte;

    int32_t* countPtr_pdouble;
    int32_t* countPtr_pinteger;
    int32_t* countPtr_pbyte;

    void Error_Set_UseExceptions(const void*, bool value)
    {
        useExceptions = value;
    }

    bool Error_Get_UseExceptions(const void*)
    {
        return useExceptions;
    }

    void DSS_ShowPanel(const void* ctx)
    {
        Text_Set_Command(ctx, "panel");
    }

    static HRESULT AltDSS_COM_Init();
    static HRESULT DSS_LoadAltDSS(const void*, const char* lib, uint64_t flags);
    static HRESULT DSS_LoadOpenDSS(const void*, const char* lib, uint64_t flags);
    static HRESULT AltDSS_COM_InitCommon();
    static void AltDSS_COM_Dispose(void const* ctx, AltDSSCAPIEx& capi, bool uninit=false);
    HRESULT Settings_Get_SkipCommandsStrs(const void* ctx, VARIANT* value);
    HRESULT Settings_Set_SkipCommandsStrs(const void* ctx, VARIANT& value);
    HRESULT YMatrix_GetCompressedYMatrixWrapped(void const* ctx, VARIANT* vCol, VARIANT* vRowIdx, VARIANT* vcVals);
};

extern AltDSSCAPIEx dss_capi;

struct AltDSS_COM_CString
{
    const char* c_str;

    AltDSS_COM_CString(BSTR& s)
    {
        c_str = _com_util::ConvertBSTRToString(s);
    }

    ~AltDSS_COM_CString()
    {
        delete[] c_str;
    }

    inline operator const char* ()
    {
        return c_str;
    }
};

struct AltDSS_COM_GetTempInt32Ptr
{
    int32_t* ptr;
    VARIANT& value;

    AltDSS_COM_GetTempInt32Ptr(VARIANT& v) : value(v)
    {
        HRESULT res = SafeArrayAccessData(value.parray, reinterpret_cast<void**>(&ptr));
        if (FAILED(res))
        {
            ptr = nullptr;
        }
    }

    ~AltDSS_COM_GetTempInt32Ptr()
    {
        if (ptr)
        {
            SafeArrayUnaccessData(value.parray);
        }
    }

    inline operator int32_t* ()
    {
        return ptr;
    }
};


template <typename T, const IID* iid>
struct CAltDSS_COM
{
    inline HRESULT AltDSS_COM_CheckError(HRESULT result = S_OK)
    {
        if (FAILED(result))
        {
            return result;
        }
        if ((*dss_capi.errorPtr) && dss_capi.useExceptions)
        {
            const int errorNumber = *dss_capi.errorPtr;
            BSTR errorMessage = _com_util::ConvertStringToBSTR(dss_capi.Error_Get_Description(dss_capi_ctx));
            TCHAR buffer[1024];
            LPCTSTR messageFormat = TEXT("(#%d) %s");
            result = StringCchPrintf(buffer, 1024, messageFormat, errorNumber, errorMessage);
            SysFreeString(errorMessage);
            static_cast<T*>(this)->Error(buffer, *iid, DISP_E_EXCEPTION);
            *dss_capi.errorPtr = 0;
            return DISP_E_EXCEPTION;
        }
        return S_OK;
    }

    template<class FuncT, class... Ts>
    HRESULT AltDSS_COM_SetDoubles(FuncT func, VARIANT& value, Ts... args)
    {
        if (value.vt != (VT_ARRAY | VT_R8))
        {
            return E_INVALIDARG;
        }

        SAFEARRAY* psa = V_ARRAY(&value);
        if (psa == nullptr)
        {
            return E_INVALIDARG;
        }

        double* data = NULL;
        HRESULT res = SafeArrayAccessData(psa, reinterpret_cast<void**>(&data));
        if (FAILED(res))
        {
            return res;
        }
        // int32_t lbound = psa->rgsabound[0].lLbound;
        int32_t count = psa->rgsabound[0].cElements;
        func(dss_capi_ctx, data, count, args...);
        res = SafeArrayUnaccessData(psa);
        return AltDSS_COM_CheckError(res);
    }


    template<class FuncT, class... Ts>
    HRESULT AltDSS_COM_SetInt32s(FuncT func, VARIANT& value, Ts... args)
    {
        if (value.vt != (VT_ARRAY | VT_R8))
        {
            return E_INVALIDARG;
        }

        SAFEARRAY* psa = V_ARRAY(&value);
        if (psa == nullptr)
        {
            return E_INVALIDARG;
        }

        int32_t* data = NULL;
        HRESULT res = SafeArrayAccessData(psa, reinterpret_cast<void**>(&data));
        if (FAILED(res))
        {
            return res;
        }
        // int32_t lbound = psa->rgsabound[0].lLbound;
        int32_t count = psa->rgsabound[0].cElements;
        func(dss_capi_ctx, data, count, args...);
        res = SafeArrayUnaccessData(psa);
        return AltDSS_COM_CheckError(res);
    }

    template<class FuncT, class... Ts>
    HRESULT AltDSS_COM_SetStrs(FuncT func, VARIANT& value, Ts... args)
    {
        if (value.vt != (VT_ARRAY | VT_BSTR))
        {
            return E_INVALIDARG;
        }

        SAFEARRAY* psa = V_ARRAY(&value);
        if (psa == nullptr)
        {
            return E_INVALIDARG;
        }

        BSTR* bstrs = NULL;
        HRESULT res = SafeArrayAccessData(psa, reinterpret_cast<void**>(&bstrs));
        if (FAILED(res))
        {
            return res;
        }
        // int32_t lbound = psa->rgsabound[0].lLbound;
        int32_t count = psa->rgsabound[0].cElements;
        char** cstrs = new char* [count];
        for (int32_t i = 0; i < count; ++i)
        {
            cstrs[i] = NULL;
        }
        for (int32_t i = 0; i < count; ++i)
        {
            cstrs[i] = _com_util::ConvertBSTRToString(bstrs[i]);
        }

        func(dss_capi_ctx, const_cast<const char**>(cstrs), count, args...);

        for (int32_t i = 0; i < count; ++i)
        {
            delete[] cstrs[i];
        }
        delete[] cstrs;
        res = SafeArrayUnaccessData(psa);
        return AltDSS_COM_CheckError(res);
    }

    template<class FuncT, class... Ts>
    HRESULT AltDSS_COM_GetStrs(FuncT func, VARIANT* value, Ts... args)
    {
        if (value == nullptr)
        {
            return E_POINTER;
        }

        int32_t dims[4] = { 0, 0, 0, 0 };
        char** cstrs = NULL;

        func(dss_capi_ctx, &cstrs, dims, args...);

        HRESULT res = AltDSS_COM_CheckError();
        if (FAILED(res))
        {
            return res;
        }

        CComSafeArray<BSTR> safeArray(dims[0]);
        SAFEARRAY* psa = safeArray.m_psa;
        BSTR* bstrs = reinterpret_cast<BSTR*>(psa->pvData);
        res = SafeArrayAccessData(psa, reinterpret_cast<void**>(&bstrs));
        if (bstrs == nullptr || FAILED(res))
        {
            dss_capi.DSS_Dispose_PPAnsiChar(&cstrs, dims[0]);
            return FAILED(res) ? res : E_FAIL;
        }

        for (int32_t i = 0; i < dims[0]; ++i)
        {
            const char* s = cstrs[i];

            if (!s)
            {
                bstrs[i] = SysAllocString(L"");
                continue;
            }

            // Little dance to get the size, allocate, finally convert.
            int wcCount = MultiByteToWideChar(CP_ACP, 0, s, -1, nullptr, 0);
            if (wcCount <= 0)
            {
                bstrs[i] = SysAllocString(L"");
                continue;
            }
            bstrs[i] = SysAllocStringLen(nullptr, wcCount - 1);
            MultiByteToWideChar(CP_ACP, 0, s, -1, bstrs[i], wcCount);
        }
        dss_capi.DSS_Dispose_PPAnsiChar(&cstrs, dims[0]);
        res = SafeArrayUnaccessData(psa);
        ::VariantInit(value);
        value->vt = (VT_ARRAY | VT_BSTR);
        value->parray = safeArray.Detach();
        return AltDSS_COM_CheckError(res);
    }

    template<class FuncT, class... Ts>
    HRESULT AltDSS_COM_GetDoubles(FuncT func, VARIANT* value, Ts... args)
    {
        if (value == nullptr)
        {
            return E_POINTER;
        }

        func(dss_capi_ctx, args...);

        HRESULT res = AltDSS_COM_CheckError();
        if (FAILED(res))
        {
            return res;
        }
        
        CComSafeArray<double> safeArray(static_cast<ULONG>(*dss_capi.countPtr_pdouble));
        memcpy(safeArray.m_psa->pvData, *dss_capi.dataPtr_pdouble, (*dss_capi.countPtr_pdouble) * sizeof(double));
        ::VariantInit(value);
        value->vt = (VT_ARRAY | VT_R8);
        value->parray = safeArray.Detach();

        return S_OK;
    }

    template<class FuncT, class... Ts>
    HRESULT AltDSS_COM_GetInt32s(FuncT func, VARIANT* value, Ts... args)
    {
        if (value == nullptr)
        {
            return E_POINTER;
        }

        func(dss_capi_ctx, args...);

        HRESULT res = AltDSS_COM_CheckError();
        if (FAILED(res))
        {
            return res;
        }

        CComSafeArray<long> safeArray(static_cast<ULONG>(*dss_capi.countPtr_pinteger));
        memcpy(safeArray.m_psa->pvData, *dss_capi.dataPtr_pinteger, (*dss_capi.countPtr_pinteger) * sizeof(int32_t));
        ::VariantInit(value);
        value->vt = (VT_ARRAY | VT_I4);
        value->parray = safeArray.Detach();

        return S_OK;
    }


    template<class FuncT, class... Ts>
    HRESULT AltDSS_COM_GetInt8s(FuncT func, VARIANT* value, Ts... args)
    {
        if (value == nullptr)
        {
            return E_POINTER;
        }

        func(dss_capi_ctx, args...);

        HRESULT res = AltDSS_COM_CheckError();
        if (FAILED(res))
        {
            return res;
        }

        CComSafeArray<char> safeArray(static_cast<ULONG>(*dss_capi.countPtr_pbyte));
        memcpy(safeArray.m_psa->pvData, *dss_capi.dataPtr_pbyte, (*dss_capi.countPtr_pbyte) * sizeof(int8_t));
        ::VariantInit(value);
        value->vt = (VT_ARRAY | VT_I1);
        value->parray = safeArray.Detach();

        return S_OK;
    }

    template<typename T>
    HRESULT AltDSS_COM_EnsureInterface(CLSID cid, T** Value, ATL::CComPtr<T>& cachedValue, HRESULT res = S_OK)
    {
        if (FAILED(res))
        {
            return res;
        }

        if (!Value)
        {
            return E_POINTER;
        }

        if (!cachedValue)
        {
            res = cachedValue.CoCreateInstance(cid);
            if (FAILED(res))
            {
                return res;
            }
        }

        *Value = cachedValue;
        if (*Value)
        {
            (*Value)->AddRef();
        }

        return S_OK;
    }

};
#endif
