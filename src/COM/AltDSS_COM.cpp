#include "pch.h"
#include "framework.h"
#include "DSS.h"
#define ALTDSS_ODDIE_NAMESPACE
#include "altdss/capi/oddie.h"

namespace altdss { namespace capi {
extern "C" int32_t AltDSSOddieCAPIInit(AltDSSCAPI* funcs, uint64_t size, uint64_t version, uint64_t reserved1, void* reserved2);
extern "C" int32_t AltDSSCAPIInit(AltDSSCAPI* funcs, uint64_t size, uint64_t version, uint64_t reserved1, void* reserved2);
}}

static_assert(sizeof(int32_t) == sizeof(long), "int32_t and long are expected to be the same size!");

AltDSSCAPIEx dss_capi;
void const* dss_capi_ctx = NULL;
static bool altdssCOMInitialized = false;
int32_t localErrorValue = 0;

struct AltDSS_COM_KeepPreviousDSS
{
    AltDSSCAPIEx saved;
    void const* savedCtx;
    bool prevInit;

    AltDSS_COM_KeepPreviousDSS()
    {
        printf("KeepPrevious: Saving\n");
        prevInit = altdssCOMInitialized;
        std::swap(saved, dss_capi);
        savedCtx = dss_capi_ctx;
    }

    ~AltDSS_COM_KeepPreviousDSS()
    {
        if (altdssCOMInitialized && prevInit && savedCtx && dss_capi_ctx != savedCtx)
        {
            printf("KeepPrevious: Disposing previous context\n");
            AltDSSCAPIEx::AltDSS_COM_Dispose(savedCtx, saved);
            return;
        }
        if (prevInit && !altdssCOMInitialized)
        {
            printf("KeepPrevious: Restoring\n");
            altdssCOMInitialized = prevInit;
            std::swap(saved, dss_capi);
            dss_capi_ctx = savedCtx;
            return;
        }

        printf("KeepPrevious: Nothing do to\n");
    }
};


void AltDSSCAPIEx::AltDSS_COM_Dispose(void const* ctx, AltDSSCAPIEx& capi, bool uninit)
{
    printf("Disposing previous context\n");
    if (!capi.isAltDSS)
    {
        capi.ctx_Dispose(ctx);
    }
    altdss::capi::AltDSSCAPILibClose(&capi);
    if (uninit)
    {
        altdssCOMInitialized = false;
    }
}

HRESULT AltDSSCAPIEx::AltDSS_COM_Init()
{
    if (altdssCOMInitialized)
    {
        return S_OK;
    }
    dss_capi.errorPtr = &localErrorValue;
    return DSS_LoadAltDSS(nullptr, nullptr, 0);
}

HRESULT AltDSSCAPIEx::DSS_LoadAltDSS(const void*, const char* lib, uint64_t flags)
{
    AltDSS_COM_KeepPreviousDSS keepDSS;
    altdssCOMInitialized = false;
    int32_t init_res = -999;
    if (lib == nullptr || lib[0] == 0)
    {
        // Try loading the internal, prelinked DLL.
        init_res = altdss::capi::AltDSSCAPIInit(&dss_capi, sizeof(altdss::capi::AltDSSCAPI), 1, 0, NULL);
        dss_capi.libHandle = nullptr;
    }

    if (init_res != 1)
    {
        // Use provided DLL path and options instead
        init_res = altdss::capi::AltDSSCAPILibInit(lib, flags? &flags : nullptr, "AltDSSCAPIInit", &dss_capi, sizeof(altdss::capi::AltDSSCAPI), 1, 0, NULL);
    }

    if (init_res != 1)
    {
        printf("AltDSSCAPILibInit error code: %d\n", init_res);
        HRESULT res = HRESULT_FROM_WIN32(GetLastError());
        if (FAILED(res))
        {
            return res;
        }
        return E_UNEXPECTED;
    }
    return AltDSS_COM_InitCommon();
}

HRESULT AltDSSCAPIEx::DSS_LoadOpenDSS(const void*, const char* lib, uint64_t flags)
{
    AltDSS_COM_KeepPreviousDSS keepDSS;
    altdssCOMInitialized = false;
    int32_t init_res = -999;
    init_res = altdss::capi::AltDSSOddieCAPIInit(&dss_capi, sizeof(altdss::capi::AltDSSCAPI), 1, 0, NULL);
    dss_capi.libHandle = nullptr;
    if (init_res != 1)
    {
        printf("AltDSSOddieCAPIInit error code: %d\n", init_res);
        HRESULT res = HRESULT_FROM_WIN32(GetLastError());
        if (FAILED(res))
        {
            return res;
        }
        return E_UNEXPECTED;
    }
    dss_capi.Oddie_SetLibOptions(lib ? lib : "OpenDSSDirect.DLL", nullptr);
    dss_capi.Oddie_SetOptions(nullptr, altdss::oddie::capi::OddieOptionFlags_Strict);
    printf("Oddie DSS passed first load step, %d\n", (int) dss_capi.isAltDSS);
    return AltDSS_COM_InitCommon();
}

HRESULT AltDSSCAPIEx::AltDSS_COM_InitCommon()
{
    if (!dss_capi.isAltDSS)
    {
        printf("Creating an Oddie DSS context\n");
        dss_capi_ctx = dss_capi.ctx_New();
    }
    else
    {
        printf("Getting the prime AltDSS context\n");
        dss_capi_ctx = dss_capi.ctx_Get_Prime();
    }
    if (dss_capi_ctx == nullptr)
    {
        printf("Could not grab a DSS context!\n");
        return E_UNEXPECTED;
    }
    if (!dss_capi.DSS_Start(dss_capi_ctx, 0))
    {
        printf("DSS_Start failed!\n");
        return E_UNEXPECTED;
    }
    dss_capi.errorPtr = dss_capi.Error_Get_NumberPtr(dss_capi_ctx);
    dss_capi.DSS_GetGRPointers(
        dss_capi_ctx,
        &dss_capi.dataPtr_pdouble,
        &dss_capi.dataPtr_pinteger,
        &dss_capi.dataPtr_pbyte,
        &dss_capi.countPtr_pdouble,
        &dss_capi.countPtr_pinteger,
        &dss_capi.countPtr_pbyte
    );

    altdssCOMInitialized = true;
    printf("InitCommon...\n");
    return S_OK;
}

HRESULT AltDSSCAPIEx::Settings_Get_SkipCommandsStrs(const void* ctx, VARIANT* value)
{
    Settings_Get_SkipCommands_GR(dss_capi_ctx);
    int32_t count = *dss_capi.countPtr_pinteger;
    const int32_t* cmds = *dss_capi.dataPtr_pinteger;

    ATL::CComSafeArray<BSTR> safeArray(count);
    SAFEARRAY* psa = safeArray.m_psa;
    BSTR* bstrs = reinterpret_cast<BSTR*>(psa->pvData);
    HRESULT res = SafeArrayAccessData(psa, reinterpret_cast<void**>(&bstrs));
    if (bstrs == nullptr)
    {
        return E_FAIL;
    }

    for (int32_t i = 0; i < count; ++i)
    {
        const char* s = DSS_Executive_Get_Command(dss_capi_ctx, cmds[i]);

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
    ::VariantInit(value);
    value->vt = (VT_ARRAY | VT_BSTR);
    value->parray = safeArray.Detach();
    return res;
}

HRESULT AltDSSCAPIEx::Settings_Set_SkipCommandsStrs(const void* ctx, VARIANT& value)
{
    std::vector<int32_t> numericValue;
    if (value.vt != (VT_ARRAY | VT_BSTR))
    {
        return E_INVALIDARG;
    }

    SAFEARRAY* psa = V_ARRAY(&value);
    if (psa == nullptr)
    {
        return E_INVALIDARG;
    }

    int32_t dims[4] = { 0, 0, 0, 0 };
    char** cstrs = NULL;
    int32_t numCommands = DSS_Executive_Get_NumCommands(dss_capi_ctx);
    std::vector<ATL::CComBSTR> allCommandStrs;
    for (int32_t i = 0; i < numCommands; numCommands)
    {
        allCommandStrs.emplace_back(DSS_Executive_Get_Command(dss_capi_ctx, i + 1));
        allCommandStrs[i].ToLower();
    }

    ATL::CComSafeArray<BSTR> safearray(V_ARRAY(&value));
    for (ULONG i = 0; i < safearray.GetCount(); ++i)
    {
        ATL::CComBSTR cmd = safearray.GetAt(i);
        cmd.ToLower();
        int32_t found = -1;
        for (size_t k = 0; k < allCommandStrs.size(); ++k)
        {
            if (allCommandStrs[k] == cmd)
            {
                found = static_cast<int32_t>(k);
                break;
            }
        }
        if (found == -1)
        {
            safearray.Detach();
            return E_INVALIDARG;
        }
        numericValue.push_back(found);
    }
    safearray.Detach();

    Settings_Set_SkipCommands(dss_capi_ctx, numericValue.data(), static_cast<int32_t>(numericValue.size()));

    return S_OK;
}

HRESULT AltDSSCAPIEx::YMatrix_GetCompressedYMatrixWrapped(void const *ctx, VARIANT* vCol, VARIANT* vRowIdx, VARIANT* vcVals)
{
    uint32_t nBus = 0;
    uint32_t nNz = 0;
    int32_t* ColPtr = nullptr;
    int32_t* RowIdxPtr = nullptr;
    double* cValsPtr = nullptr;

    YMatrix_GetCompressedYMatrix(ctx, true, &nBus, &nNz, &ColPtr, &RowIdxPtr, &cValsPtr);

    if (!nBus || !nNz)
    {
        return E_ABORT;
    }

    CComSafeArray<double> sVals(static_cast<ULONG>(nNz * 2));
    CComSafeArray<long> sRowIdx(static_cast<ULONG>(nNz));
    CComSafeArray<long> sCol(static_cast<ULONG>(nBus + 1));

    memcpy(sVals.m_psa->pvData, cValsPtr, nNz * 2 * sizeof(double));
    memcpy(sRowIdx.m_psa->pvData, RowIdxPtr, nNz * sizeof(int32_t));
    memcpy(sCol.m_psa->pvData, ColPtr, (nBus + 1) * sizeof(int32_t));

    ::VariantInit(vcVals);
    ::VariantInit(vRowIdx);
    ::VariantInit(vCol);
    
    vcVals->vt = (VT_ARRAY | VT_R8);
    vcVals->parray = sVals.Detach();
    vRowIdx->vt = (VT_ARRAY | VT_I4);
    vRowIdx->parray = sRowIdx.Detach();
    vCol->vt = (VT_ARRAY | VT_I4);
    vCol->parray = sCol.Detach();

    dss_capi.DSS_Dispose_PDouble(&cValsPtr);
    dss_capi.DSS_Dispose_PInteger(&RowIdxPtr);
    dss_capi.DSS_Dispose_PInteger(&ColPtr);

    return S_OK;
}
