// SPDX-FileCopyrightText: (C) 2023-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: BSD-3

/*
This file is work in progress

- Functions to return scalar batches
- Functions to get the inputs

*/
#include <assert.h>
#include <string.h>
#include "../../include/altdss/capi/capi.h"

typedef struct {
    AltDSSCAPI* dss;
    altdss_func_i32_cvp ffirst;
    altdss_func_i32_cvp fnext;
    altdss_func_i32_cvp fcount;
    altdss_func_i32_cvp_i32 fsetidx;
    
    double** f64Ptr;
    int32_t* f64Dims;

    int32_t** i32Ptr;
    int32_t* i32Dims;

    char*** strPtr;
    int32_t* strDims;
} BatchAux;

char** ensureStrArray(const AltDSSCAPI* dss, void* ctx, char*** arrayPtr, int32_t* dims, int32_t count)
{
    int32_t i, prevCount = dims[1];
    if (prevCount < count)
    {
        if (*arrayPtr)
        {
            for (i = 0; i < prevCount; ++i)
            {
                dss->DSS_FreeMem(ctx, *arrayPtr[i]);
            }
            dss->DSS_FreeMem(ctx, *arrayPtr);
        }
        dims[1] = count;
        *arrayPtr = dss->DSS_GetMem(ctx, count * sizeof(double));
        for (i = 0; i < count; ++i)
        {
            *arrayPtr[i] = NULL;
        }
    }
    return *arrayPtr;
}

double* ensureF64Array(const AltDSSCAPI* dss, void* ctx, double** arrayPtr, int32_t* dims, int32_t count)
{
    if (dims[1] < count)
    {
        if (*arrayPtr)
        {
            dss->DSS_FreeMem(ctx, *arrayPtr);
        }
        dims[1] = count;
        *arrayPtr = dss->DSS_GetMem(ctx, count * sizeof(double));
    }
    return *arrayPtr;
}

int32_t* ensureI32Array(const AltDSSCAPI* dss, void* ctx, int32_t** arrayPtr, int32_t* dims, int32_t count)
{
    if (dims[1] < count)
    {
        if (*arrayPtr)
        {
            dss->DSS_FreeMem(ctx, *arrayPtr);
        }
        dims[1] = count;
        *arrayPtr = dss->DSS_GetMem(ctx, count * sizeof(int32_t));
    }
    return *arrayPtr;
}

// #include "./batch_generated.c"

void SimpleBatch_Get_F64s(void *ctx, BatchAux* batch, altdss_func_v_cvp scalarFunc)
{
    int32_t expectedCount = batch->fcount(ctx);
    int32_t actualCount = 0;
    int32_t idx;
    const AltDSSCAPI* dss = batch->dss;
    int32_t *errorPtr = dss->Error_Get_NumberPtr(ctx);
    void* ptr;
    
    batch->vDims[0] = 0;
    if (!expectedCount || *errorPtr)
    {
        return;
    }
    ptr = ensurevArray(dss, ctx, batch->vPtr, batch->vDims, expectedCount);
    for (idx = batch->ffirst(ctx); idx; idx = batch->fnext(ctx))
    {
        *ptr = scalarFunc(ctx);

        if (*errorPtr)
        {
            return;
        }
        ++ptr;
    }

    assert(actualCount <= expectedCount);

    actualCount = (int32_t)(ptr - *batch->vPtr);
    batch->vDims[0] = actualCount;
    batch->vDims[2] = 0;
    batch->vDims[3] = 0;
}

/*
#define FASTDSS_GEN_BATCH_SET(SCALARNAME, SCALAREXPR, FUNCGETSCALAR, POINTERNAME, FUNCGETARRAYDATA, FUNCTYPE, ERROR_HANDLER) \
    if (nel == 1)\
    {\
        SCALARNAME = mxGetScalar(prhs[0]);\
        for (idx = ffirst(fastdssCtx->dssCtx); idx; idx = fnext(fastdssCtx->dssCtx))\
        {\
            ((FUNCTYPE)f->func)(fastdssCtx->dssCtx, SCALAREXPR);\
            if (*fastdssCtx->errorPtr)\
            {\
                goto ERROR_HANDLER;\
            }\
        }\
        if (*fastdssCtx->errorPtr)\
        {\
            goto ERROR_HANDLER;\
        }\
    }\
    else if (nel == batch_count)\
    {\
        POINTERNAME = FUNCGETARRAYDATA(prhs[0]);\
        for (idx = ffirst(fastdssCtx->dssCtx); idx; idx = fnext(fastdssCtx->dssCtx), POINTERNAME++)\
        {\
            SCALARNAME = *POINTERNAME;\
            ((FUNCTYPE)f->func)(fastdssCtx->dssCtx, SCALAREXPR);\
            if (*fastdssCtx->errorPtr)\
            {\
                goto ERROR_HANDLER;\
            }\
        }\
        if (*fastdssCtx->errorPtr)\
        {\
            goto ERROR_HANDLER;\
        }\
    }\
    else\
    {\
        mexErrMsgTxt("Invalid arguments for batch call (expected a single value or array with the size of the batch)");\
        return;\
    }


#define FASTDSS_GEN_BATCH_GET(POINTERNAME, FUNCGETARRAYDATA, FUNCTYPE, DATATYPE, REAL_OR_IMAG, ERROR_HANDLER) \
    plhs[0] = mxCreateUninitNumericArray(1, &batch_count, DATATYPE, REAL_OR_IMAG);\
    POINTERNAME = FUNCGETARRAYDATA(plhs[0]);\
    for (idx = ffirst(fastdssCtx->dssCtx); idx; idx = fnext(fastdssCtx->dssCtx), POINTERNAME++)\
    {\
        *POINTERNAME = ((FUNCTYPE)f->func)(fastdssCtx->dssCtx);\
        if (*fastdssCtx->errorPtr)\
        {\
            goto ERROR_HANDLER;\
        }\
    }\
    if (*fastdssCtx->errorPtr)\
    {\
        goto ERROR_HANDLER;\
    }\
    return;


#define FASTDSS_BATCH_PREAMBLE \
    mwSize batch_count;\
    int32_t idx, i;\
    func_i32_ctx fcount, ffirst, fnext;\
    if (batch_mode)\
    {\
        fcount = (func_i32_ctx) fastdssCtx->funcs[collections[collectionIdx].funcIdxCount].func;\
        ffirst = (func_i32_ctx) fastdssCtx->funcs[collections[collectionIdx].funcIdxFirst].func;\
        fnext = (func_i32_ctx) fastdssCtx->funcs[collections[collectionIdx].funcIdxNext].func;\
        batch_count = fcount ? fcount(fastdssCtx->dssCtx) : 0;\
        if (!fcount || !ffirst || !fnext || *fastdssCtx->errorPtr)\
        {\
            mexErrMsgTxt("Could not get batch info. The error interface may contain additional messages.");\
            return;\
        }\
        if (!batch_count)\
        {\
            mexErrMsgTxt("Batch call requested, but no items found.");\
            return;\
        }\
    }


static void FastDSS_ScalarSetter(FastDSS_MATLABContext* fastdssCtx, FastDSSFuncInfo* f, int nlhs, mxArray *plhs[], int nrhs, mxArray **prhs, int batch_mode, int batch_limit, int collectionIdx)
{
    int32_t cval_int, cval_int2;
    double cval_float64;
    char *cstr = NULL;
    size_t cstr_size = 0;
    double* pval_float64;
    int32_t* pval_int;
    int64_t nel;
    mxArray* tmp;

    FASTDSS_BATCH_PREAMBLE

    if (nlhs >= 1)
    {
        mexErrMsgTxt("This function returns no values!");
        return;
    }

    if (batch_mode)
    {
        nel = (nrhs == 1) ? mxGetNumberOfElements(prhs[0]) : -123;

        switch (f->funcArgSignature)
        {
            case fastdss_types_f64:
                if (nrhs != 1 || !mxIsNumeric(prhs[0]) || mxIsComplex(prhs[0]) || !mxIsScalar(prhs[0]) || (mxGetNumberOfElements(prhs[0]) != batch_count))
                {
                    mexErrMsgTxt("Invalid arguments for function (expected a float64 value or an array matching the batch size)");
                    return;
                }
                FASTDSS_GEN_BATCH_SET(cval_float64, cval_float64, mxGetScalar, pval_float64, mxGetDoubles, func_void_ctx_f64, FASTDSS_SCALARSETTER_ERROR_HANDLING)
    
            case fastdss_types_i32:
                if (nrhs != 1 || !mxIsNumeric(prhs[0]) || mxIsComplex(prhs[0]) || !mxIsScalar(prhs[0]) || (mxGetNumberOfElements(prhs[0]) != batch_count))
                {
                    mexErrMsgTxt("Invalid arguments for function (expected an int32 value or an array matching the batch size)");
                    return;
                }
                FASTDSS_GEN_BATCH_SET(cval_int, cval_int, mxGetScalar, pval_int, mxGetInt32s, func_void_ctx_i32, FASTDSS_SCALARSETTER_ERROR_HANDLING)
    
            case fastdss_types_i32_i32:
                mexErrMsgTxt("Batch operations are not implemented for this function.");
                return;
    
            case fastdss_types_b16:
                if (nrhs != 1 || !mxIsNumeric(prhs[0]) || mxIsComplex(prhs[0]) || !mxIsScalar(prhs[0]) || (mxGetNumberOfElements(prhs[0]) != batch_count))
                {
                    mexErrMsgTxt("Invalid arguments for function (expected a boolean value or an array matching the batch size)");
                    return;
                }
                FASTDSS_GEN_BATCH_SET(cval_float64, (cval_float64 ? (uint16_t)-1 : (uint16_t)0), mxGetScalar, pval_float64, mxGetDoubles, func_void_ctx_b16, FASTDSS_SCALARSETTER_ERROR_HANDLING)
    
            case fastdss_types_str:
                if (nrhs != 1 || !(mxIsChar(prhs[0]) || (mxIsCell(prhs[0]) && ((mxGetNumberOfElements(prhs[0]) != batch_count) || !mxIsChar(mxGetCell(prhs[0], 0))))))
                {
                FASTDSS_SCALER_SETTER_STR_BATCH_ERROR:                    
                    mexErrMsgTxt("Invalid arguments (expected a string/char value and a cell array of strings)");
                    return;
                }
                if (!mxIsCell(prhs[0]))
                {
                    cstr_size = mxGetNumberOfElements(prhs[0]) + 1;
                    cstr = mxCalloc(cstr_size, sizeof(char));
                    if (mxGetString(prhs[0], cstr, cstr_size) != 0)
                    {
                        mxFree((void*) cstr);
                        goto FASTDSS_SCALER_SETTER_STR_BATCH_ERROR;
                    }
                    for (idx = ffirst(fastdssCtx->dssCtx); idx; idx = fnext(fastdssCtx->dssCtx))
                    {
                        ((func_void_ctx_str)f->func)(fastdssCtx->dssCtx, cstr);
                        if (*fastdssCtx->errorPtr)
                        {
                            goto FASTDSS_SCALARSETTER_ERROR_HANDLING;
                        }
                    }
                    return;
                }
                for (idx = ffirst(fastdssCtx->dssCtx), i = 0; idx; idx = fnext(fastdssCtx->dssCtx), ++i)
                {
                    tmp = mxGetCell(prhs[0], i);
                    cstr_size = mxGetNumberOfElements(tmp) + 1;
                    cstr = mxCalloc(cstr_size, sizeof(char));
                    if (mxGetString(prhs[0], cstr, cstr_size) != 0)
                    {
                        mxFree((void*) cstr);
                        goto FASTDSS_SCALER_SETTER_STR_BATCH_ERROR;
                    }

                    ((func_void_ctx_str)f->func)(fastdssCtx->dssCtx, cstr);
                    if (*fastdssCtx->errorPtr)
                    {
                        goto FASTDSS_SCALARSETTER_ERROR_HANDLING;
                    }
                }
                return;
            case fastdss_types_void:
                mexErrMsgTxt("Batch operations are not implemented for this function.");
                return;
   
            default:
                mexErrMsgTxt("Invalid call signature");
                return;
        }
   
        return;
    }

    switch (f->funcArgSignature)
    {
        case fastdss_types_f64:
            if (nrhs != 1 || !mxIsScalar(prhs[0]))
            {
                mexErrMsgTxt("Invalid arguments for function (expected a float64 value)");
                return;
            }
            cval_float64 = mxGetScalar(prhs[0]);
            ((func_void_ctx_f64)f->func)(fastdssCtx->dssCtx, cval_float64);
            break;
        case fastdss_types_i32:
            if (nrhs != 1 || !mxIsScalar(prhs[0]))
            {
                mexErrMsgTxt("Invalid arguments (expected an integer value)");
                return;
            }
            cval_int = (int32_t) mxGetScalar(prhs[0]);
            ((func_void_ctx_i32)f->func)(fastdssCtx->dssCtx, cval_int);
            break;
        case fastdss_types_i32_i32:
            if (nrhs != 2 || !mxIsScalar(prhs[0]) || !mxIsScalar(prhs[1]))
            {
                mexErrMsgTxt("Invalid arguments (expected two integer values)");
                return;
            }
            cval_int = (int32_t) mxGetScalar(prhs[0]);
            cval_int2 = (int32_t) mxGetScalar(prhs[1]);
            ((func_void_ctx_i32_i32)f->func)(fastdssCtx->dssCtx, cval_int, cval_int2);
            break;
        case fastdss_types_b16:
            if (nrhs != 1 || !mxIsScalar(prhs[0]))
            {
                mexErrMsgTxt("Invalid arguments (expected a boolean value)");
                return;
            }
            cval_int = (mxGetScalar(prhs[0]) != 0);
            ((func_void_ctx_b16)f->func)(fastdssCtx->dssCtx, cval_int ? (uint16_t)-1 : (uint16_t)0);
            break;
        case fastdss_types_str:
            if (nrhs != 1 || !mxIsChar(prhs[0]))
            {
            FASTDSS_SCALER_SETTER_STR_ERROR:                
                mexErrMsgTxt("Invalid arguments (expected a string/char value)");
                return;
            }
            cstr_size = mxGetNumberOfElements(prhs[0]) + 1;
            cstr = mxCalloc(cstr_size, sizeof(char));
            if (mxGetString(prhs[0], cstr, cstr_size) != 0)
            {
                mxFree((void*) cstr);
                goto FASTDSS_SCALER_SETTER_STR_ERROR;
            }
            ((func_void_ctx_str)f->func)(fastdssCtx->dssCtx, cstr);
            break;
        case fastdss_types_void:
            ((func_void_ctx)f->func)(fastdssCtx->dssCtx);
            break;
        default:
            mexErrMsgTxt("Invalid call signature");
            return;
    }

FASTDSS_SCALARSETTER_ERROR_HANDLING:
    if (*fastdssCtx->errorPtr && ((fastdssCtx->settings) & FastDSSSettings_UseExceptions))
    {
        const char *errorDesc = fastdssCtx->dssCFuncs.Error_Get_Description(fastdssCtx->dssCtx);
        // int32_t num = *fastdssCtx->errorPtr;
        *fastdssCtx->errorPtr = 0;
        mexErrMsgTxt(errorDesc);
        // PyErr_SetObject(f->parent->DSSExceptionType, PyTuple_Pack(2, 
        //     PyLong_FromLong(num),
        //     PyUnicode_FromString(errorDesc)
        // ));
        return;
    }
}

static void FastDSS_ScalarGetter(FastDSS_MATLABContext* fastdssCtx, FastDSSFuncInfo* f, int nlhs, mxArray *plhs[], int nrhs, mxArray **prhs, int batch_mode, int batch_limit, int collectionIdx)
{
    int32_t argValue, argValue2;
    int32_t cval_int32 = -1;
    double cval_float64 = -1;
    double* pval_float64;
    int32_t *pval_int32;
    char *cstr = NULL;
    size_t cstr_size = 0;
    
    mxLogical* pval_logical;

    FASTDSS_BATCH_PREAMBLE

    if (nlhs > 1)
    {
        mexErrMsgTxt("Too many output parameters");
        return;
    }

    switch (f->funcArgSignature)
    {
        case fastdss_types_i32_i32:
            if (nrhs != 2 || !mxIsScalar(prhs[0]) || !mxIsScalar(prhs[1]))
            {
                mexErrMsgTxt("Invalid arguments on FastDSS_ScalarGetter call (expected two integer values)");
                return;
            }
            argValue = (int32_t) mxGetScalar(prhs[0]);
            argValue2 = (int32_t) mxGetScalar(prhs[1]);
            break;
        case fastdss_types_i32:
            if (nrhs != 1 || !mxIsScalar(prhs[0]))
            {
                mexErrMsgTxt("Invalid arguments on FastDSS_ScalarGetter call (expected an integer value)");
                return;
            }
            argValue = (int32_t) mxGetScalar(prhs[0]);
            break;
        case fastdss_types_str:
            if (nrhs != 1 || !mxIsChar(prhs[0]))
            {
            FASTDSS_SCALER_GETTER_STR_ERROR:
                mexErrMsgTxt("Invalid arguments on FastDSS_ScalarGetter call (expected a str value)");
                return;
            }
            cstr_size = mxGetNumberOfElements(prhs[0]) + 1;
            cstr = mxCalloc(cstr_size, sizeof(char));
            if (mxGetString(prhs[0], cstr, cstr_size) != 0)
            {
                mxFree((void*) cstr);
                goto FASTDSS_SCALER_GETTER_STR_ERROR;
            }
            break;
        case fastdss_types_void:
            if (nrhs != 0)
            {
                mexErrMsgTxt("Arguments passed where no arguments are expected on FastDSS_ScalarGetter call");
                return;
            }
            break;
        default:
            mexErrMsgTxt("Invalid arguments on FastDSS_ScalarGetter call (unknown signature)");
            return;
    }

    switch (f->resType)
    {
        case fastdss_types_b16:
        switch (f->funcArgSignature)
        {
            case fastdss_types_i32_i32:
                if (!batch_mode)
                {
                    cval_int32 = ((func_b16_ctx_i32_i32)f->func)(fastdssCtx->dssCtx, argValue, argValue2);
                }
                else
                {
                    mexErrMsgTxt("Batch operations are not implemented for this function.");
                }
            break;
            case fastdss_types_i32:
                if (!batch_mode)
                {
                    cval_int32 = ((func_b16_ctx_i32)f->func)(fastdssCtx->dssCtx, argValue);
                }
                else
                {
                    mexErrMsgTxt("Batch operations are not implemented for this function.");
                }
                break;
            case fastdss_types_str:
                if (!batch_mode)
                {
                    cval_int32 = ((func_b16_ctx_str)f->func)(fastdssCtx->dssCtx, cstr);
                }
                else
                {
                    mexErrMsgTxt("Batch operations are not implemented for this function.");
                }
                break;
            case fastdss_types_void:
                if (!batch_mode)
                {
                    cval_int32 = ((func_b16_ctx)f->func)(fastdssCtx->dssCtx);
                }
                else
                {
                    FASTDSS_GEN_BATCH_GET(pval_logical, mxGetLogicals, func_b16_ctx, mxLOGICAL_CLASS, mxREAL, FASTDSS_SCALARGETTER_ERROR_HANDLING)
                }
                break;
            default:
                mexErrMsgTxt("Internal error: unknown signature");
                return;
        }
        break;
        case fastdss_types_i32:
        switch (f->funcArgSignature)
        {
            case fastdss_types_i32:
                cval_int32 = ((func_i32_ctx_i32)f->func)(fastdssCtx->dssCtx, argValue);
                break;
            case fastdss_types_str:
                if (!batch_mode)
                {
                    cval_int32 = ((func_i32_ctx_str)f->func)(fastdssCtx->dssCtx, cstr);
                }
                else
                {
                    mexErrMsgTxt("Batch operations are not implemented for this function.");
                }
                break;
            case fastdss_types_void:
                if (!batch_mode)
                {
                    cval_int32 = ((func_i32_ctx)f->func)(fastdssCtx->dssCtx);
                }
                else
                {
                    FASTDSS_GEN_BATCH_GET(pval_int32, mxGetInt32s, func_i32_ctx, mxINT32_CLASS, mxREAL, FASTDSS_SCALARGETTER_ERROR_HANDLING)
                }
                break;
            default:
                mexErrMsgTxt("Internal error: unknown signature");
                return;
        }
        break;
        case fastdss_types_f64:
        switch (f->funcArgSignature)
        {
            case fastdss_types_i32:
                if (!batch_mode)
                {
                    cval_float64 = ((func_f64_ctx_i32)f->func)(fastdssCtx->dssCtx, argValue);
                }
                else
                {
                    mexErrMsgTxt("Batch operations are not implemented for this function.");
                }
                break;
            case fastdss_types_str:
                if (!batch_mode)
                {
                    cval_float64 = ((func_f64_ctx_str)f->func)(fastdssCtx->dssCtx, cstr);
                }
                else
                {
                    mexErrMsgTxt("Batch operations are not implemented for this function.");
                }
                break;
            case fastdss_types_void:
                if (!batch_mode)
                {
                    cval_float64 = ((func_f64_ctx)f->func)(fastdssCtx->dssCtx);
                }
                else
                {
                    FASTDSS_GEN_BATCH_GET(pval_float64, mxGetDoubles, func_f64_ctx, mxDOUBLE_CLASS, mxREAL, FASTDSS_SCALARGETTER_ERROR_HANDLING)
                }
                break;
            default:
                mexErrMsgTxt("Internal error: unknown signature");
                return;
        }
        break;
    }

FASTDSS_SCALARGETTER_ERROR_HANDLING:
//TODO: batch...
    if (*fastdssCtx->errorPtr && ((fastdssCtx->settings) & FastDSSSettings_UseExceptions))
    {
        const char *errorDesc = fastdssCtx->dssCFuncs.Error_Get_Description(fastdssCtx->dssCtx);
        // int32_t num = *fastdssCtx->errorPtr;
        *fastdssCtx->errorPtr = 0;
        mexErrMsgTxt(errorDesc);
        // PyErr_SetObject(f->parent->DSSExceptionType, PyTuple_Pack(2, 
        //     PyLong_FromLong(num),
        //     PyUnicode_FromString(errorDesc)
        // ));
        return;
    }

    // if (!nlhs)
    // {
    //     return;
    // }

    mwSize dims = 1;
    switch (f->resType)
    {
        case fastdss_types_b16:
            plhs[0] = mxCreateLogicalScalar(cval_int32 ? 1 : 0);
            if (!plhs[0])
            {
                mexErrMsgTxt("Internal error: could not create logical result");
            }
            return;
        case fastdss_types_i32:
            plhs[0] = mxCreateUninitNumericArray(1, &dims, mxINT32_CLASS, mxREAL);
            if (!plhs[0])
            {
                mexErrMsgTxt("Internal error: could not create integer result");
            }
            *((int32_t *)mxGetData(plhs[0])) = cval_int32;
            return;
        case fastdss_types_f64:
            plhs[0] = mxCreateDoubleScalar(cval_float64);
            if (!plhs[0])
            {
                mexErrMsgTxt("Internal error: could not create real result");
            }
            return;
    }
    return;
}

static void FastDSS_GRGetter(FastDSS_MATLABContext* fastdssCtx, FastDSSFuncInfo* f, int nlhs, mxArray *plhs[], int nrhs, mxArray **prhs, int batch_mode, int batch_limit, int collectionIdx)
{
    double float64Arg1, float64Arg2;
    int argValue = 0;
    int32_t resType = f->resType;
    const int32_t settings = fastdssCtx->settings;
    int nd = 2;
    int nitems, j;
    mwSize dims[2], dimsTranspose[2];
    // double *dblPtr;
    int32_t *i32Ptr;
    // int8_t *i8Ptr;
    bool arg1ok, arg2ok, arg3ok;
    mxArray* tmp;
    mwSize tmp_size, current_size;
    mxComplexity tmp_complexity;
    mxClassID tmp_data_type;
    int tmp_item_size, tmp_step, src_item_size;
    uint8_t *outPtr, *outPtrNext, *outPtrEnd, *current_data;
    int64_t current_delta;

    // complex* pval_complex128;
    // mxLogical* pval_logical;
    
    FASTDSS_BATCH_PREAMBLE

    if (((batch_mode < 4) && nlhs > 1) || ((batch_mode >= 4) && nlhs > 2))
    {
        mexErrMsgTxt("Too many output parameters for this function");
        return;
    }

    int32_t* countPtr;
    void **dataPtr;
    dataPtr = (void **) activeFastDSSContext->dataPtr_pdouble;
    countPtr = activeFastDSSContext->countPtr_pdouble;
    if (resType == fastdss_types_gr_i32s)
    {
        dataPtr = (void **) activeFastDSSContext->dataPtr_pinteger;
        countPtr = activeFastDSSContext->countPtr_pinteger;
    }
    else if (resType == fastdss_types_gr_i8s)
    {
        dataPtr = (void **) activeFastDSSContext->dataPtr_pbyte;
        countPtr = activeFastDSSContext->countPtr_pbyte;
    }

    if (batch_mode)
    {
        FASTDSS_MATLAB_DATA_GETTER getPtr;
        if (f->funcArgSignature != fastdss_types_void)
        {
            mexErrMsgTxt("Batch operations are not implemented for this function.");
        }
        switch (resType)
        {
            case fastdss_types_gr_z128:
                // COMPLEX_FASTDSS_GEN_BATCH_GET_GR(pval_complex128, mxGetDoubles, func_f64_ctx, mxDOUBLE_CLASS, mxCOMPLEX, complex*)
            case fastdss_types_gr_z128s:
            case fastdss_types_gr_f64s:
            case fastdss_types_gr_i32s:
                tmp_complexity = mxREAL;
                switch (resType)
                {
                    case fastdss_types_gr_z128:
                    case fastdss_types_gr_z128s: 
                        tmp_item_size = sizeof(double); // keep as 1 double, don't modify the number of items
                        tmp_step = batch_limit * tmp_item_size * 2;
                        tmp_data_type = mxDOUBLE_CLASS;
                        tmp_complexity = mxCOMPLEX;
                        getPtr = (FASTDSS_MATLAB_DATA_GETTER) mxGetComplexDoubles;    
                        break;
                    case fastdss_types_gr_f64s:
                        tmp_item_size = sizeof(double);
                        tmp_step = batch_limit * tmp_item_size;
                        tmp_data_type = mxDOUBLE_CLASS;
                        getPtr = (FASTDSS_MATLAB_DATA_GETTER) mxGetDoubles;
                        break;
                    case fastdss_types_gr_i32s:
                        tmp_item_size = sizeof(int32_t);
                        tmp_step = batch_limit * tmp_item_size;
                        tmp_data_type = mxINT32_CLASS;
                        getPtr = (FASTDSS_MATLAB_DATA_GETTER) mxGetInt32s;
                        break;
                    default:
                        mexErrMsgTxt("Batch operations are not implemented for this function.");    
                        return;
                }
                switch (batch_mode)
                {
                    case 1: // Cell mode
                    {
                        plhs[0] = mxCreateCellMatrix(1, batch_count);
                        for (idx = ffirst(fastdssCtx->dssCtx), i = 0; idx; idx = fnext(fastdssCtx->dssCtx), ++i)
                        {
                            ((gr_func_void_ctx)f->func)(fastdssCtx->dssCtx);
                            if (*fastdssCtx->errorPtr)
                            {
                                mxDestroyArray(plhs[0]);
                                plhs[0] = NULL;
                                break;
                            }
                            tmp_size = countPtr[0];
                            //TODO: AdvancedTypes
                            tmp = mxCreateUninitNumericArray(1, &tmp_size, tmp_data_type, tmp_complexity);
                            memcpy(getPtr(tmp), *dataPtr, countPtr[0] * tmp_item_size);
                            mxSetCell(plhs[0], i, tmp);
                        }
                        return;
                    }
                    case 2: // Matrix mode
                    {
                        dims[0] = batch_limit;
                        dims[1] = batch_count;
                        plhs[0] = mxCreateUninitNumericArray(2, dims, tmp_data_type, tmp_complexity);
                        outPtr = getPtr(plhs[0]);
                        if (resType == fastdss_types_gr_i32s)
                        {
                            int item_count = batch_count * batch_limit;
                            int32_t *outIntPtr = (int32_t *) outPtr;
                            for (i = 0; i < item_count; ++i, outIntPtr++)
                            {
                                *outIntPtr = -1; //TODO: add an option to set this value
                            }
                        }
                        else
                        {
                            double nan = mxGetNaN(); //TODO: add an option to set this value
                            double *outDblPtr = (double *) outPtr;
                            int item_count = batch_count * batch_limit * (tmp_complexity == mxCOMPLEX ? 2 : 1);
                            for (i = 0; i < item_count; ++i, outDblPtr++)
                            {
                                *outDblPtr = nan;
                            }
                        }
                        
                        for (idx = ffirst(fastdssCtx->dssCtx), i = 0; idx; idx = fnext(fastdssCtx->dssCtx), ++i)
                        {
                            ((gr_func_void_ctx)f->func)(fastdssCtx->dssCtx);
                            if (*fastdssCtx->errorPtr)
                            {
                                mxDestroyArray(plhs[0]);
                                plhs[0] = NULL;
                                break; //TODO: break is not enough here
                            }
                            tmp_size = countPtr[0];
                            if (tmp_size > batch_limit) 
                            {
                                tmp_size = batch_limit;
                            }

                            //TODO: AdvancedTypes?
                            memcpy(outPtr, *dataPtr, tmp_size * tmp_item_size);
                            outPtr += tmp_step;
                        }
                        // mexPrintf("Item count: %d, expected: %d\n", i, (int) batch_count);
                        return;
                    }
                    case 3: // Concat mode
                    {
                        if (batch_limit <= 0)
                        {
                            batch_limit = 3;
                        }
                        current_size = batch_limit * batch_count; // initial size, may grow or shrink later
                        // mexPrintf("Initial size: %d\n", current_size);
                        src_item_size = tmp_item_size;
                        if (tmp_complexity == mxCOMPLEX)
                        {
                            tmp_item_size *= 2;
                        }
                        current_data = mxMalloc(current_size * tmp_item_size);
                        // mexPrintf("Initial bytes: %d\n", current_size * tmp_item_size);
                        outPtr = current_data;
                        outPtrEnd = current_data + tmp_step * batch_count;
                        for (idx = ffirst(fastdssCtx->dssCtx); idx; idx = fnext(fastdssCtx->dssCtx), outPtr = outPtrNext)
                        {
                            ((gr_func_void_ctx)f->func)(fastdssCtx->dssCtx);
                            if (*fastdssCtx->errorPtr)
                            {
                                mxFree(current_data);
                                break; //TODO: break is not enough here
                            }
                            tmp_size = countPtr[0];
                            
                            outPtrNext = outPtr + tmp_size * src_item_size;
                            while (outPtrNext > outPtrEnd)
                            {
                                current_size = (current_size * 3) / 2 + 20;
                                current_delta = outPtr - current_data;
                                current_data = mxRealloc(current_data, current_size * tmp_item_size);
                                outPtr = current_data + current_delta;
                                outPtrNext = current_data + current_delta + tmp_size * src_item_size;
                                outPtrEnd = current_data + current_size * tmp_item_size;
                            }
                            memcpy(outPtr, *dataPtr, tmp_size * src_item_size);
                        }
                        // mexPrintf("Final bytes: %d\n", (outPtr - current_data));
                        current_size = (outPtr - current_data) / tmp_item_size;
                        // mexPrintf("Final size: %d\n", current_size);
                        plhs[0] = mxCreateUninitNumericArray(1, &current_size, tmp_data_type, tmp_complexity);
                        memcpy(getPtr(plhs[0]), current_data, current_size * tmp_item_size);
                        return;
                    }
                    case 4: // Terminal-matrix mode
                    case 5: // Terminal-matrix mode, map node to position
                    {
                        // First pass, extract info
                        int totalTerms = 0;
                        int maxConductors = 0;
                        int conductors = 0;
                        int terminals = 0;
                        int maxNodeNum = 0;
                        int nodeNum, n;
                        const bool nodeNumAsPosition = (batch_mode == 5);
                        const bool collectTerminalOwner = (nlhs == 2);

                        for (idx = ffirst(fastdssCtx->dssCtx); idx; idx = fnext(fastdssCtx->dssCtx))
                        {
                            totalTerms += fastdssCtx->dssCFuncs.CktElement_Get_NumTerminals(fastdssCtx->dssCtx);
                            conductors = fastdssCtx->dssCFuncs.CktElement_Get_NumConductors(fastdssCtx->dssCtx);
                            if (conductors > maxConductors)
                            {
                                maxConductors = conductors;
                            }
                            if (!nodeNumAsPosition)
                            {
                                continue;
                            }
                            fastdssCtx->dssCFuncs.CktElement_Get_NodeOrder_GR(fastdssCtx->dssCtx);
                            int32_t *intDataPtr = *fastdssCtx->dataPtr_pinteger;
                            for (n = 0; n < fastdssCtx->countPtr_pinteger[0]; ++n)
                            {
                                nodeNum = intDataPtr[n];
                                if (nodeNum > maxNodeNum)
                                {
                                    maxNodeNum = nodeNum;
                                }
                            }
                        }

                        // Allocate outputs
                        if (!nodeNumAsPosition)
                        {
                            dims[0] = maxConductors;
                        }
                        else
                        {
                            dims[0] = maxNodeNum + 1;
                        }
                        dims[1] = totalTerms;
                        //plhs[0] = mxCreateUninitNumericArray(2, dims, tmp_data_type, tmp_complexity); //TODO: fill!
                        plhs[0] = mxCreateNumericArray(2, dims, tmp_data_type, tmp_complexity);
                        outPtr = getPtr(plhs[0]);

                        int32_t *ownerIdxPtr = NULL;

                        if (collectTerminalOwner)
                        {
                            plhs[1] = mxCreateNumericArray(1, &dims[1], mxINT32_CLASS, mxREAL);
                            ownerIdxPtr = mxGetInt32s(plhs[1]);
                        }

                        // src_item_size = tmp_item_size;
                        if (tmp_complexity == mxCOMPLEX)
                        {
                            tmp_item_size *= 2;
                        }
                        const int ptrStep = dims[0] * tmp_item_size;
                        uint8_t* tmpDataPtr;

                        if (!nodeNumAsPosition)
                        {
                            for (idx = ffirst(fastdssCtx->dssCtx); idx; idx = fnext(fastdssCtx->dssCtx))
                            {
                                ((gr_func_void_ctx)f->func)(fastdssCtx->dssCtx);
                                if (*fastdssCtx->errorPtr)
                                {
                                    mxDestroyArray(plhs[0]);
                                    plhs[0] = NULL;
                                    break; //TODO: break is not enough here
                                }
                                conductors = countPtr[2];
                                terminals = countPtr[3];
                                tmpDataPtr = *dataPtr;
                                for (n = 0; n < terminals; n++)
                                {
                                    memcpy(outPtr, tmpDataPtr, countPtr[2] * tmp_item_size);
                                    outPtr += ptrStep;
                                    tmpDataPtr += countPtr[2] * tmp_item_size;
                                }
                                if (!ownerIdxPtr)
                                {
                                    continue;
                                }
                                for (n = 0; n < terminals; n++, ownerIdxPtr++)
                                {
                                    *ownerIdxPtr = idx; //TODO: workaround for collections that return 1 in Next?
                                }
                            }
                        }
                        else
                        {
                            int32_t *nodeOrderPtr = NULL;
                            int32_t nodeOrderCnt[4] = {0, 0, 0, 0};
                            int32_t c;
                            for (idx = ffirst(fastdssCtx->dssCtx); idx; idx = fnext(fastdssCtx->dssCtx))
                            {
                                ((gr_func_void_ctx)f->func)(fastdssCtx->dssCtx);
                                if (*fastdssCtx->errorPtr)
                                {
                                    mxDestroyArray(plhs[0]);
                                    plhs[0] = NULL;
                                    break; //TODO: break is not enough here
                                }
                                conductors = countPtr[2];
                                terminals = countPtr[3];
                                tmpDataPtr = *dataPtr;

                                fastdssCtx->dssCFuncs.CktElement_Get_NodeOrder(fastdssCtx->dssCtx, &nodeOrderPtr, nodeOrderCnt);
                                for (n = 0; n < terminals; n++)
                                {
                                    for (c = 0; c < conductors; ++c)
                                    {
                                        memcpy(outPtr + tmp_item_size * nodeOrderPtr[c], tmpDataPtr + tmp_item_size * c, tmp_item_size);
                                    }
                                    outPtr += ptrStep;
                                    tmpDataPtr += countPtr[2] * tmp_item_size;
                                }
                                if (!ownerIdxPtr)
                                {
                                    continue;
                                }
                                for (n = 0; n < terminals; n++, ownerIdxPtr++)
                                {
                                    *ownerIdxPtr = idx; //TODO: workaround for collections that return 1 in Next?
                                }
                            }
                            fastdssCtx->dssCFuncs.DSS_Dispose_PInteger(&nodeOrderPtr);
                        }
                        return;
                    }
                    default:
                        mexErrMsgTxt("Invalid batch mode.");
                        return;
                }
            default:
                mexErrMsgTxt("Batch operations are not implemented for this function.");
                return;
        }
    }    

    switch (f->funcArgSignature)
    {
        case fastdss_types_f64_f64_i32:
            arg1ok = (nrhs == 3) && mxIsScalar(prhs[0]);
            arg2ok = (nrhs == 3) && mxIsScalar(prhs[1]);
            arg3ok = (nrhs == 3) && mxIsScalar(prhs[2]);
            if (!arg1ok || !arg2ok || !arg3ok)
            {
                mexErrMsgTxt("Invalid arguments (expected float, float, integer arguments)");
                return;
            }
            float64Arg1 = mxGetScalar(prhs[0]);
            float64Arg2 = mxGetScalar(prhs[1]);
            argValue = (int32_t) mxGetScalar(prhs[2]);
            ((gr_func_void_ctx_f64_f64_i32)f->func)(fastdssCtx->dssCtx, float64Arg1, float64Arg2, argValue);
            break;
        case fastdss_types_i32:
            if (nrhs != 1 || !mxIsScalar(prhs[0]))
            {
                mexErrMsgTxt("Invalid arguments (expected an integer value)");
                return;
            }
            argValue = (int32_t) mxGetScalar(prhs[0]);
            ((gr_func_void_ctx_i32)f->func)(fastdssCtx->dssCtx, argValue);
            break;
        case fastdss_types_b16:
            if (nrhs != 1 || !mxIsScalar(prhs[0]))
            {
                mexErrMsgTxt("Invalid arguments (expected an integer or boolean value)");
                return;
            }
            argValue = (int32_t) mxGetScalar(prhs[0]);
            ((gr_func_void_ctx_b16)f->func)(fastdssCtx->dssCtx, argValue ? (uint16_t)-1 : (uint16_t)0);
            break;
        default:
            if (nrhs != 0)
            {
                mexErrMsgTxt("No arguments are expected");
                return;
            }
            ((gr_func_void_ctx)f->func)(fastdssCtx->dssCtx);
            break;
    }
    if (*fastdssCtx->errorPtr && (settings & FastDSSSettings_UseExceptions))
    {
        const char *errorDesc = fastdssCtx->dssCFuncs.Error_Get_Description(fastdssCtx->dssCtx);
        // int32_t num = *fastdssCtx->errorPtr;
        *fastdssCtx->errorPtr = 0;
        mexErrMsgTxt(errorDesc);
        // PyErr_SetObject(f->parent->DSSExceptionType, PyTuple_Pack(2, 
        //     PyLong_FromLong(num),
        //     PyUnicode_FromString(errorDesc)
        // ));
        return;
    }

    nitems = countPtr[0];

    dims[0] = 1;
    if (((settings & FastDSSSettings_AdvancedTypes) == 0))
    {
        dims[1] = nitems;
        if (resType == fastdss_types_gr_z128s || resType == fastdss_types_gr_z128)
        {
            resType = fastdss_types_gr_f64s;
        }
    }
    else if (countPtr[2] == 0)
    {
        if (resType == fastdss_types_gr_z128s)
        {
            nitems /= 2;
        }
        dims[1] = nitems;
    }
    else
    {
        nd = 2;
        dims[0] = countPtr[2];
        dims[1] = countPtr[3];
        if (resType == fastdss_types_gr_z128s)
        {
            nitems /= 2;
        }
    }

    switch (resType)
    {
        case fastdss_types_gr_z128:
            if (countPtr[0] != 2)
            {
                mexErrMsgTxt("Unexpected number of elements returned by API (complex number).");
                return;
            }
            plhs[0] = mxCreateDoubleMatrix(1, 1, mxCOMPLEX);
            if (plhs[0] == NULL)
            {
                goto array_error;
            }
            memcpy(mxGetComplexDoubles(plhs[0]), *(double**)dataPtr, 2 * sizeof(double));
            return;
        case fastdss_types_gr_z128s:
            if (countPtr[0] & 1)
            {
                mexErrMsgTxt("Unexpected number of elements returned by API (array of complex numbers).");
                return;
            }
            plhs[0] = mxCreateUninitNumericArray(nd, &dims[0], mxDOUBLE_CLASS, mxCOMPLEX);
            if (plhs[0] == NULL)
            {
                goto array_error;
            }
            memcpy(mxGetComplexDoubles(plhs[0]), *(double**)dataPtr, 2 * sizeof(double) * nitems);
            return;
        case fastdss_types_gr_f64s:
            plhs[0] = mxCreateUninitNumericArray(nd, &dims[0], mxDOUBLE_CLASS, mxREAL);
            if (plhs[0] == NULL)
            {
                goto array_error;
            }
            memcpy(mxGetDoubles(plhs[0]), *(double**)dataPtr, sizeof(double) * nitems);
            return;
        // case fastdss_types_f32: -- TODO: not used yet
        case fastdss_types_gr_i32s:
            if (nd == 1)
            {
                plhs[0] = mxCreateUninitNumericArray(nd, &dims[0], mxINT32_CLASS, mxREAL);
                if (plhs[0] == NULL)
                {
                    goto array_error;
                }
                memcpy(mxGetInt32s(plhs[0]), *(int32_t**)dataPtr, sizeof(int32_t) * nitems);
                return;
            }

            // We need to transpose it here; typically small matrices, better to do it ourselves.
            dimsTranspose[0] = dims[1];
            dimsTranspose[1] = dims[0];
            plhs[0] = mxCreateUninitNumericArray(nd, &dimsTranspose[0], mxINT32_CLASS, mxREAL);
            if (plhs[0] == NULL)
            {
                goto array_error;
            }
            i32Ptr = mxGetInt32s(plhs[0]);
            for (i = 0; i < dims[0]; i++)
            {
                for (j = 0; j < dims[1]; j++)
                {
                    i32Ptr[j + i * dims[0]] = (*(int32_t**)dataPtr)[i + j * dims[1]]; //TODO: test/check
                }
            }
            return;
        case fastdss_types_gr_i8s:
            plhs[0] = mxCreateUninitNumericArray(nd, &dims[0], mxINT8_CLASS, mxREAL);
            if (plhs[0] == NULL)
            {
                goto array_error;
            }
            memcpy(mxGetInt8s(plhs[0]), *(int8_t**)dataPtr, sizeof(int8_t) * nitems);
            return;
        default:
            mexErrMsgTxt("Invalid type specified (internal error).");
            return;
    }

    return;
array_error:
    mexErrMsgTxt("Could not copy array data.");
    return;
}

static void FastDSS_StrGetter(FastDSS_MATLABContext* fastdssCtx, FastDSSFuncInfo* f, int nlhs, mxArray *plhs[], int nrhs, mxArray **prhs, int batch_mode, int batch_limit, int collectionIdx)
{
    int argValue;
    char const* cstr;
    // int32_t nel;

    FASTDSS_BATCH_PREAMBLE

    if (nlhs > 1)
    {
        mexErrMsgTxt("Too many output arguments");
        return;
    }

    if (batch_mode && f->funcArgSignature != fastdss_types_void)
    {
        mexErrMsgTxt("Batch operations are not implemented for this function.");
        return;
    }

    switch (f->funcArgSignature)
    {
        case fastdss_types_i32:
            if (batch_mode)
            {
                mexErrMsgTxt("Batch operations are not implemented for this function.");
                return;   
            }
            if (nrhs != 1 || !mxIsScalar(prhs[0]))
            {
                mexErrMsgTxt("Invalid arguments (expected an integer value)");
                return;
            }
            argValue = (int32_t) mxGetScalar(prhs[0]);
            cstr = ((func_str_ctx_i32)f->func)(fastdssCtx->dssCtx, argValue);
            break;
        default:
            if (nrhs != 0)
            {
                mexErrMsgTxt("No arguments are expected");
                return;
            }
            if (!batch_mode)
            {
                cstr = ((func_str_ctx)f->func)(fastdssCtx->dssCtx);
            }
            else
            {
                plhs[0] = mxCreateCellMatrix(batch_count, 1);
                for (idx = ffirst(fastdssCtx->dssCtx), i = 0; idx; idx = fnext(fastdssCtx->dssCtx), ++i)
                {
                    cstr = ((func_str_ctx)f->func)(fastdssCtx->dssCtx);
                    if (*fastdssCtx->errorPtr)
                    {
                        mxDestroyArray(plhs[0]);
                        plhs[0] = NULL;
                        break; // TODO: error handling
                    }
                    mxSetCell(plhs[0], i, mxCreateString(cstr ? cstr : ""));
                }
                return;
            }
            break;
    }
    //TODO: for Alt functions, we will need to dispose the C string later, 
    // or take ownership of the pointer.
    if (*fastdssCtx->errorPtr && ((fastdssCtx->settings) & FastDSSSettings_UseExceptions))
    {
        if (fastdssCtx->dssCFuncs.Error_Get_Description != f->func)
        {
            const char *errorDesc = fastdssCtx->dssCFuncs.Error_Get_Description(fastdssCtx->dssCtx);
            // int32_t num = *fastdssCtx->errorPtr;
            *fastdssCtx->errorPtr = 0;
            puts(errorDesc);
            mexErrMsgTxt(errorDesc);
            // PyErr_SetObject(f->parent->DSSExceptionType, PyTuple_Pack(2, 
            //     PyLong_FromLong(num),
            //     PyUnicode_FromString(errorDesc)
            // ));
            return;
        }
        mexErrMsgTxt("Error mapping DSS error to MATLAB!");
        //PyErr_SetString(f->parent->DSSExceptionType, "Error mapping DSS error to MATLAB!");
        return;
    }

    // if (!nlhs)
    // {
    //     return;
    // }
    plhs[0] = mxCreateString(cstr ? cstr : "");
    if (!plhs[0])
    {
        mexErrMsgTxt("Error creating result string");
    }
}

static void FastDSS_StrListGetter(FastDSS_MATLABContext* fastdssCtx, FastDSSFuncInfo* f, int nlhs, mxArray *plhs[], int nrhs, mxArray **prhs, int batch_mode, int batch_limit, int collectionIdx)
{
    char** cstr_list = NULL;
    char *cstr = NULL;
    size_t cstr_size = 0;
    // char** sptr = NULL;
    int32_t count[4] = {0, 0, 0, 0};
    int32_t j;
    int argIntValue;
    const int32_t settings = fastdssCtx->settings;
    mxArray* tmp;

    FASTDSS_BATCH_PREAMBLE

    if (nlhs > 1)
    {
        mexErrMsgTxt("Too many output parameters");
        return;
    }

    if (batch_mode && f->funcArgSignature != fastdss_types_void)
    {
        mexErrMsgTxt("Batch operations are not implemented for this function.");
        return;
    }

    switch (f->funcArgSignature)
    {
        case fastdss_types_b16:
            if (nrhs != 1 || !mxIsScalar(prhs[0]))
            {
                mexErrMsgTxt("Invalid arguments (expected an integer or boolean value)");
                return;
            }
            argIntValue = (int32_t) mxGetScalar(prhs[0]);
            ((func_void_ctx_strs_i32)f->func)(fastdssCtx->dssCtx, &cstr_list, &count[0], argIntValue ? 1 : 0);
            break;
        case fastdss_types_i32:
            if (nrhs != 1 || !mxIsScalar(prhs[0]))
            {
                mexErrMsgTxt("Invalid arguments (expected an integer value)");
                return;
            }
            argIntValue = (int32_t) mxGetScalar(prhs[0]);
            ((func_void_ctx_strs_i32)f->func)(fastdssCtx->dssCtx, &cstr_list, &count[0], argIntValue);
            break;
        case fastdss_types_str:
            if (nrhs != 1 || !mxIsChar(prhs[0]))
            {
                FASTDSS_STRLIST_GETTER_STR_ERROR:
                mexErrMsgTxt("Invalid arguments (expected a str value)");
                return;
            }
            cstr_size = mxGetNumberOfElements(prhs[0]) + 1;
            cstr = mxCalloc(cstr_size, sizeof(char));
            if (mxGetString(prhs[0], cstr, cstr_size) != 0)
            {
                mxFree((void*) cstr);
                goto FASTDSS_STRLIST_GETTER_STR_ERROR;
            }
            ((func_void_ctx_strs_str)f->func)(fastdssCtx->dssCtx, &cstr_list, &count[0], cstr);
            break;
        default:
            if (nrhs != 0)
            {
                mexErrMsgTxt("No arguments are expected");
                return;
            }
            if (!batch_mode)
            {
                ((func_void_ctx_strs)f->func)(fastdssCtx->dssCtx, &cstr_list, &count[0]);//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX <-- check this in the original Python version
                break;
            }

            plhs[0] = mxCreateCellMatrix(batch_count, 1);
            tmp = NULL;
            for (idx = ffirst(fastdssCtx->dssCtx), i = 0; idx; idx = fnext(fastdssCtx->dssCtx), ++i)
            {
                ((func_void_ctx_strs)f->func)(fastdssCtx->dssCtx, &cstr_list, &count[0]);
                if (*fastdssCtx->errorPtr)
                {
                    mxDestroyArray(plhs[0]);
                    plhs[0] = NULL;
                    break;
                }

                tmp = mxCreateCellMatrix(count[0], 1);
                for(j = 0; j < count[0]; j++)
                {
                    mxSetCell(tmp, j, mxCreateString(cstr_list[j] ? cstr_list[j] : ""));
                }

                mxSetCell(plhs[0], i, tmp);
                tmp = NULL;
            }
            fastdssCtx->dssCFuncs.DSS_Dispose_PPAnsiChar(&cstr_list, count[1]);
            return;
    }

    if (*fastdssCtx->errorPtr && (settings & FastDSSSettings_UseExceptions))
    {
        const char *errorDesc = fastdssCtx->dssCFuncs.Error_Get_Description(fastdssCtx->dssCtx);
        // int32_t num = *fastdssCtx->errorPtr;
        *fastdssCtx->errorPtr = 0;
        mexErrMsgTxt(errorDesc);
        // PyErr_SetObject(f->parent->DSSExceptionType, PyTuple_Pack(2, 
        //     PyLong_FromLong(num),
        //     PyUnicode_FromString(errorDesc)
        // ));
        fastdssCtx->dssCFuncs.DSS_Dispose_PPAnsiChar(&cstr_list, count[1]);
        return;
    }

    //if (PyErr_Occurred())
    //{
    //    fastdssCtx->dssCFuncs.DSS_Dispose_PPAnsiChar(&cstr_list, count[1]);
    //    return;
    //}

    if ((settings & FastDSSSettings_ODDPyStrings) && (count[0] == 1))
    {
        cstr = cstr_list[0];
        if ((cstr == NULL) || (cstr[0] == 0))
        {
            count[0] = 0;
        }
        else if ((cstr) && (
            (cstr[0] == 'n' || cstr[0] == 'N') &&
            (cstr[1] == 'o' || cstr[1] == 'O') &&
            (cstr[2] == 'n' || cstr[2] == 'N') &&
            (cstr[3] == 'e' || cstr[3] == 'E') &&
            (cstr[4] == 0 )
        ))
        {
            count[0] = 0;
        }
    }

    if (nlhs == 0)
    {
        return; // ignore when no output vars are used
    }

    plhs[0] = mxCreateCellMatrix(count[0], 1);
    for (i = 0; i < count[0]; i++)
    {
        mxSetCell(plhs[0], i, mxCreateString(cstr_list[i] ? cstr_list[i] : ""));
    }
    fastdssCtx->dssCFuncs.DSS_Dispose_PPAnsiChar(&cstr_list, count[1]);
    return;
}

*/