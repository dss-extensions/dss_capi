// SPDX-FileCopyrightText: (C) 2023-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: BSD-3

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

void SimpleBatch_Get_F64s_I32(void *ctx, BatchAux* batch, altdss_func_v_cvp scalarFunc, int32_t valueCount, int32_t* arg1)
{
    int32_t expectedCount = batch->fcount(ctx);
    int32_t actualCount = 0;
    int32_t idx;
    const AltDSSCAPI* dss = batch->dss;
    int32_t *errorPtr = dss->Error_Get_NumberPtr(ctx);
    ptrdiff_t valuePtrStep = valueCount ? 1 : 0;
    void* ptr;
    
    batch->vDims[0] = 0;
    if (!expectedCount || *errorPtr)
    {
        return;
    }
    ptr = ensurevArray(dss, ctx, batch->vPtr, batch->vDims, expectedCount);
    for (idx = batch->ffirst(ctx); idx; idx = batch->fnext(ctx))
    {
        *ptr = scalarFunc(ctx, *arg1);

        if (*errorPtr)
        {
            return;
        }
        ++ptr;
        arg1 += valuePtrStep;
    }

    assert(actualCount <= expectedCount);

    actualCount = (int32_t)(ptr - *batch->vPtr);
    batch->vDims[0] = actualCount;
    batch->vDims[2] = 0;
    batch->vDims[3] = 0;
}

void SimpleBatch_Get_F64s_F64F64I32(void *ctx, BatchAux* batch, altdss_func_v_cvp scalarFunc, int32_t valueCount, double* arg1, double* arg2, int32_t* arg3)
{
    int32_t expectedCount = batch->fcount(ctx);
    int32_t actualCount = 0;
    int32_t idx;
    const AltDSSCAPI* dss = batch->dss;
    int32_t *errorPtr = dss->Error_Get_NumberPtr(ctx);
    ptrdiff_t valuePtrStep = valueCount ? 1 : 0;
    void* ptr;
    
    batch->vDims[0] = 0;
    if (!expectedCount || *errorPtr)
    {
        return;
    }
    ptr = ensurevArray(dss, ctx, batch->vPtr, batch->vDims, expectedCount);
    for (idx = batch->ffirst(ctx); idx; idx = batch->fnext(ctx))
    {
        *ptr = scalarFunc(ctx, *arg1, *arg2, *arg3);

        if (*errorPtr)
        {
            return;
        }
        ++ptr;
        arg1 += valuePtrStep;
        arg2 += valuePtrStep;
        arg3 += valuePtrStep;
    }

    assert(actualCount <= expectedCount);

    actualCount = (int32_t)(ptr - *batch->vPtr);
    batch->vDims[0] = actualCount;
    batch->vDims[2] = 0;
    batch->vDims[3] = 0;
}

void SimpleBatch_Get_F64s_U16(void *ctx, BatchAux* batch, altdss_func_v_cvp scalarFunc, int32_t valueCount, uint16_t* arg1)
{
    int32_t expectedCount = batch->fcount(ctx);
    int32_t actualCount = 0;
    int32_t idx;
    const AltDSSCAPI* dss = batch->dss;
    int32_t *errorPtr = dss->Error_Get_NumberPtr(ctx);
    ptrdiff_t valuePtrStep = valueCount ? 1 : 0;
    void* ptr;
    
    batch->vDims[0] = 0;
    if (!expectedCount || *errorPtr)
    {
        return;
    }
    ptr = ensurevArray(dss, ctx, batch->vPtr, batch->vDims, expectedCount);
    for (idx = batch->ffirst(ctx); idx; idx = batch->fnext(ctx))
    {
        *ptr = scalarFunc(ctx, *arg1);

        if (*errorPtr)
        {
            return;
        }
        ++ptr;
        arg1 += valuePtrStep;
    }

    assert(actualCount <= expectedCount);

    actualCount = (int32_t)(ptr - *batch->vPtr);
    batch->vDims[0] = actualCount;
    batch->vDims[2] = 0;
    batch->vDims[3] = 0;
}

void SimpleBatch_Get_Z128s(void *ctx, BatchAux* batch, altdss_func_v_cvp scalarFunc)
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

void SimpleBatch_Get_Z128s_F64F64I32(void *ctx, BatchAux* batch, altdss_func_v_cvp scalarFunc, int32_t valueCount, double* arg1, double* arg2, int32_t* arg3)
{
    int32_t expectedCount = batch->fcount(ctx);
    int32_t actualCount = 0;
    int32_t idx;
    const AltDSSCAPI* dss = batch->dss;
    int32_t *errorPtr = dss->Error_Get_NumberPtr(ctx);
    ptrdiff_t valuePtrStep = valueCount ? 1 : 0;
    void* ptr;
    
    batch->vDims[0] = 0;
    if (!expectedCount || *errorPtr)
    {
        return;
    }
    ptr = ensurevArray(dss, ctx, batch->vPtr, batch->vDims, expectedCount);
    for (idx = batch->ffirst(ctx); idx; idx = batch->fnext(ctx))
    {
        *ptr = scalarFunc(ctx, *arg1, *arg2, *arg3);

        if (*errorPtr)
        {
            return;
        }
        ++ptr;
        arg1 += valuePtrStep;
        arg2 += valuePtrStep;
        arg3 += valuePtrStep;
    }

    assert(actualCount <= expectedCount);

    actualCount = (int32_t)(ptr - *batch->vPtr);
    batch->vDims[0] = actualCount;
    batch->vDims[2] = 0;
    batch->vDims[3] = 0;
}

void SimpleBatch_Get_Z128(void *ctx, BatchAux* batch, altdss_func_v_cvp scalarFunc)
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

void SimpleBatch_Get_I32s(void *ctx, BatchAux* batch, altdss_func_v_cvp scalarFunc)
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

