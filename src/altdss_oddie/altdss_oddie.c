#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h> 
#include "altdss_oddie.h"
#include "./altdss_oddie_private.h"

#ifdef WIN32
#define strcasecmp _stricmp
#else
#include <strings.h>
#endif

#ifdef WIN32
#define ODDIE_LOAD_FUNC(FUNCNAME) ctx->FUNCNAME = GetProcAddress(ctx->dll_handle, #FUNCNAME); if (ctx->FUNCNAME == NULL) goto CTX_NEW_ERROR;
#else
#define ODDIE_LOAD_FUNC(FUNCNAME) ctx->FUNCNAME = dlsym(ctx->dll_handle, #FUNCNAME); if (ctx->FUNCNAME == NULL) goto CTX_NEW_ERROR;
#endif

#define CTX_OR_PRIME if (!ctx) ctx = ctxPrime;

enum ControlActions {
    CTRL_NONE = 0,
    CTRL_OPEN = 1,
    CTRL_CLOSE = 2,
    CTRL_RESET = 3,
    CTRL_LOCK = 4,
    CTRL_UNLOCK = 5,
    CTRL_TAP_UP = 6,
    CTRL_TAP_DOWN = 7
};

static const void* ctxPrime = NULL;
static const char* ODDIE_LIB_NAME = NULL;
#ifdef WIN32
static uint32_t ODDIE_LIB_OPTIONS = LOAD_WITH_ALTERED_SEARCH_PATH;
#else
static uint32_t ODDIE_LIB_OPTIONS = RTLD_NOW;
#endif

ALTDSS_ODDIE_DLL const void* ctx_Get_Prime(void)
{
    return ctxPrime;
}

ALTDSS_ODDIE_DLL const void* ctx_Set_Prime(const void *ctx)
{
    const void* previous = ctxPrime;
    if (previous == ctx)
    {
        return NULL;
    }
    ctxPrime = ctx;
    return previous;
}

ALTDSS_ODDIE_DLL void* ctx_New(void)
{
    OddieContext *ctx = (OddieContext*) calloc(1, sizeof(OddieContext));
    if (ctx == NULL)
    {
        return NULL;
    }

#ifdef WIN32
    ctx->dll_handle = LoadLibraryEx(ODDIE_LIB_NAME != NULL ? ODDIE_LIB_NAME : "OpenDSSDirect.dll", NULL, ODDIE_LIB_OPTIONS);
#else
#ifdef DARWIN
    ctx->dll_handle = dlopen(ODDIE_LIB_NAME != NULL ? ODDIE_LIB_NAME : "OpenDSSDirect.dylib", ODDIE_LIB_OPTIONS);
#else
    ctx->dll_handle = dlopen(ODDIE_LIB_NAME != NULL ? ODDIE_LIB_NAME : "OpenDSSDirect.so", ODDIE_LIB_OPTIONS);
#endif
#endif

    if (ctx->dll_handle == NULL)
    {
        goto CTX_NEW_ERROR;
    }

    ODDIE_LOAD_FUNC(GetPCInjCurr)
    ODDIE_LOAD_FUNC(GetSourceInjCurrents)
    ODDIE_LOAD_FUNC(ZeroInjCurr)
    ODDIE_LOAD_FUNC(getIpointer)
    ODDIE_LOAD_FUNC(getVpointer)
    ODDIE_LOAD_FUNC(SolveSystem)
    ODDIE_LOAD_FUNC(GetCompressedYMatrix)
    ODDIE_LOAD_FUNC(AddInAuxCurrents)
    ODDIE_LOAD_FUNC(BuildYMatrixD)
    ODDIE_LOAD_FUNC(InitAndGetYparams)
    ODDIE_LOAD_FUNC(SystemYChanged)
    ODDIE_LOAD_FUNC(UseAuxCurrents)
    ODDIE_LOAD_FUNC(ErrorCode)
    ODDIE_LOAD_FUNC(ErrorDesc)
    ODDIE_LOAD_FUNC(DSSPut_Command)
    ODDIE_LOAD_FUNC(ActiveClassS)
    ODDIE_LOAD_FUNC(BUSS)
    ODDIE_LOAD_FUNC(CapacitorsS)
    ODDIE_LOAD_FUNC(CapControlsS)
    ODDIE_LOAD_FUNC(CircuitS)
    ODDIE_LOAD_FUNC(CktElementS)
    ODDIE_LOAD_FUNC(DSSElementS)
    ODDIE_LOAD_FUNC(DSSExecutiveS)
    ODDIE_LOAD_FUNC(DSSLoadsS)
    ODDIE_LOAD_FUNC(DSSProgressS)
    ODDIE_LOAD_FUNC(DSSProperties)
    ODDIE_LOAD_FUNC(DSSS)
    ODDIE_LOAD_FUNC(FusesS)
    ODDIE_LOAD_FUNC(GeneratorsS)
    ODDIE_LOAD_FUNC(GICSourcesS)
    ODDIE_LOAD_FUNC(IsourceS)
    ODDIE_LOAD_FUNC(LineCodesS)
    ODDIE_LOAD_FUNC(LinesS)
    ODDIE_LOAD_FUNC(LoadShapeS)
    ODDIE_LOAD_FUNC(MetersS)
    ODDIE_LOAD_FUNC(MonitorsS)
    ODDIE_LOAD_FUNC(ParserS)
    ODDIE_LOAD_FUNC(PDElementsS)
    ODDIE_LOAD_FUNC(PVsystemsS)
    ODDIE_LOAD_FUNC(ReclosersS)
    ODDIE_LOAD_FUNC(ReduceCktS)
    ODDIE_LOAD_FUNC(RegControlsS)
    ODDIE_LOAD_FUNC(RelaysS)
    ODDIE_LOAD_FUNC(SensorsS)
    ODDIE_LOAD_FUNC(SettingsS)
    ODDIE_LOAD_FUNC(SolutionS)
    ODDIE_LOAD_FUNC(SwtControlsS)
    ODDIE_LOAD_FUNC(TopologyS)
    ODDIE_LOAD_FUNC(TransformersS)
    ODDIE_LOAD_FUNC(VsourcesS)
    ODDIE_LOAD_FUNC(XYCurvesS)
    ODDIE_LOAD_FUNC(BUSF)
    ODDIE_LOAD_FUNC(CapacitorsF)
    ODDIE_LOAD_FUNC(CapControlsF)
    ODDIE_LOAD_FUNC(CktElementF)
    ODDIE_LOAD_FUNC(DSSLoadsF)
    ODDIE_LOAD_FUNC(FusesF)
    ODDIE_LOAD_FUNC(GeneratorsF)
    ODDIE_LOAD_FUNC(GICSourcesF)
    ODDIE_LOAD_FUNC(IsourceF)
    ODDIE_LOAD_FUNC(LineCodesF)
    ODDIE_LOAD_FUNC(LinesF)
    ODDIE_LOAD_FUNC(LoadShapeF)
    ODDIE_LOAD_FUNC(MetersF)
    ODDIE_LOAD_FUNC(ParserF)
    ODDIE_LOAD_FUNC(PDElementsF)
    ODDIE_LOAD_FUNC(PVsystemsF)
    ODDIE_LOAD_FUNC(ReclosersF)
    ODDIE_LOAD_FUNC(ReduceCktF)
    ODDIE_LOAD_FUNC(RegControlsF)
    ODDIE_LOAD_FUNC(SensorsF)
    ODDIE_LOAD_FUNC(SettingsF)
    ODDIE_LOAD_FUNC(SolutionF)
    ODDIE_LOAD_FUNC(SwtControlsF)
    ODDIE_LOAD_FUNC(TransformersF)
    ODDIE_LOAD_FUNC(VsourcesF)
    ODDIE_LOAD_FUNC(XYCurvesF)
    ODDIE_LOAD_FUNC(CircuitF)
    ODDIE_LOAD_FUNC(CmathLibF)
    ODDIE_LOAD_FUNC(ActiveClassI)
    ODDIE_LOAD_FUNC(BUSI)
    ODDIE_LOAD_FUNC(CapacitorsI)
    ODDIE_LOAD_FUNC(CapControlsI)
    ODDIE_LOAD_FUNC(CircuitI)
    ODDIE_LOAD_FUNC(CktElementI)
    ODDIE_LOAD_FUNC(CtrlQueueI)
    ODDIE_LOAD_FUNC(DSSElementI)
    ODDIE_LOAD_FUNC(DSSExecutiveI)
    ODDIE_LOAD_FUNC(DSSI)
    ODDIE_LOAD_FUNC(DSSLoads)
    ODDIE_LOAD_FUNC(DSSProgressI)
    ODDIE_LOAD_FUNC(FusesI)
    ODDIE_LOAD_FUNC(GeneratorsI)
    ODDIE_LOAD_FUNC(GICSourcesI)
    ODDIE_LOAD_FUNC(IsourceI)
    ODDIE_LOAD_FUNC(LineCodesI)
    ODDIE_LOAD_FUNC(LinesI)
    ODDIE_LOAD_FUNC(LoadShapeI)
    ODDIE_LOAD_FUNC(MetersI)
    ODDIE_LOAD_FUNC(MonitorsI)
    ODDIE_LOAD_FUNC(ParallelI)
    ODDIE_LOAD_FUNC(ParserI)
    ODDIE_LOAD_FUNC(PDElementsI)
    ODDIE_LOAD_FUNC(PVsystemsI)
    ODDIE_LOAD_FUNC(ReclosersI)
    ODDIE_LOAD_FUNC(ReduceCktI)
    ODDIE_LOAD_FUNC(RegControlsI)
    ODDIE_LOAD_FUNC(RelaysI)
    ODDIE_LOAD_FUNC(SensorsI)
    ODDIE_LOAD_FUNC(SettingsI)
    ODDIE_LOAD_FUNC(SolutionI)
    ODDIE_LOAD_FUNC(SwtControlsI)
    ODDIE_LOAD_FUNC(TopologyI)
    ODDIE_LOAD_FUNC(TransformersI)
    ODDIE_LOAD_FUNC(VsourcesI)
    ODDIE_LOAD_FUNC(XYCurvesI)
    ODDIE_LOAD_FUNC(ActiveClassV)
    ODDIE_LOAD_FUNC(BUSV)
    ODDIE_LOAD_FUNC(CapacitorsV)
    ODDIE_LOAD_FUNC(CapControlsV)
    ODDIE_LOAD_FUNC(CircuitV)
    ODDIE_LOAD_FUNC(CktElementV)
    ODDIE_LOAD_FUNC(CmathLibV)
    ODDIE_LOAD_FUNC(CtrlQueueV)
    ODDIE_LOAD_FUNC(DSSElementV)
    ODDIE_LOAD_FUNC(DSSLoadsV)
    ODDIE_LOAD_FUNC(DSSV)
    ODDIE_LOAD_FUNC(FusesV)
    ODDIE_LOAD_FUNC(GeneratorsV)
    ODDIE_LOAD_FUNC(GICSourcesV)
    ODDIE_LOAD_FUNC(IsourceV)
    ODDIE_LOAD_FUNC(LineCodesV)
    ODDIE_LOAD_FUNC(LinesV)
    ODDIE_LOAD_FUNC(LoadShapeV)
    ODDIE_LOAD_FUNC(MetersV)
    ODDIE_LOAD_FUNC(MonitorsV)
    ODDIE_LOAD_FUNC(ParallelV)
    ODDIE_LOAD_FUNC(ParserV)
    ODDIE_LOAD_FUNC(PVsystemsV)
    ODDIE_LOAD_FUNC(ReclosersV)
    ODDIE_LOAD_FUNC(RegControlsV)
    ODDIE_LOAD_FUNC(RelaysV)
    ODDIE_LOAD_FUNC(SensorsV)
    ODDIE_LOAD_FUNC(SettingsV)
    ODDIE_LOAD_FUNC(SolutionV)
    ODDIE_LOAD_FUNC(SwtControlsV)
    ODDIE_LOAD_FUNC(TopologyV)
    ODDIE_LOAD_FUNC(TransformersV)
    ODDIE_LOAD_FUNC(VsourcesV)
    ODDIE_LOAD_FUNC(XYCurvesV)

    ctx->error_number = 0;
    ctx->error_desc[0] = '\0';
    ctx->error_desc[DSS_ERR_NUM_CHR] = '\0';
    ctx->map_errors = 1;
    ctx->PropIndex = 0;
    if (ctxPrime == NULL)
    {
        ctxPrime = ctx;
    }

    return ctx;

CTX_NEW_ERROR:
    ctx_Dispose(ctx);
    free(ctx);
    return NULL;
}

ALTDSS_ODDIE_DLL void ctx_Dispose(const void *ctx)
{
    if (ctx == NULL)
    {
        return;
    }
    OddieContext* oddie_ctx = (OddieContext*) ctx;

#ifdef WIN32
    FreeLibrary(oddie_ctx->dll_handle);
#else
    if (oddie_ctx->dll_handle)
        dlclose(oddie_ctx->dll_handle);
#endif
    oddie_ctx->dll_handle = NULL;
}

const char* oddie_int32_to_pchar(OddieContext* ctx, int32_t value)
{
    snprintf(ctx->char_buffer, DSS_STR_BUFFER_NUM_CHR, "%d", value);
    return ctx->char_buffer;
}

void oddie_map_error(const void* ctx_)
{
    OddieContext* ctx = (OddieContext*) (ctx_ ? ctx_ : ctxPrime);
    size_t num_chars = DSS_ERR_NUM_CHR;
    size_t num_actual;
    int32_t dss_error;
    const char* errorDesc;
    if (!ctx->map_errors) return;
    dss_error = ctx->ErrorCode();
    if (dss_error == 0)
    {
        return;
    }
    errorDesc = ctx->ErrorDesc();
    ctx->error_number = dss_error;
    
    num_actual = strlen(errorDesc);
    if (num_actual < DSS_ERR_NUM_CHR)
    {
        num_chars = num_actual;
    }

    //TODO: check if this would work or the lack of safety could blow up
    strncpy(ctx->error_desc, errorDesc, num_chars);
    ctx->error_desc[num_chars + 1] = '\0';
}

ALTDSS_ODDIE_DLL int32_t ctx_Error_Get_Number(const void* ctx)
{
    OddieContext* oddie_ctx = (OddieContext*)(ctx ? ctx : ctxPrime);
    if (oddie_ctx->map_errors)
    {
        if (!oddie_ctx->error_number)
        {
            oddie_map_error(oddie_ctx);
        }
        int32_t res = oddie_ctx->error_number;
        oddie_ctx->error_number = 0;
        return res;
    }
    return oddie_ctx->ErrorCode();
}

ALTDSS_ODDIE_DLL void ctx_Error_Set_Description(const void* ctx, const char* Value)
{
    OddieContext* oddie_ctx = (OddieContext*) (ctx ? ctx : ctxPrime);
    if (!oddie_ctx->map_errors)
    {
        return;
    }
    strncpy(oddie_ctx->error_desc, Value, DSS_ERR_NUM_CHR);
}

void oddie_error_not_implemented(OddieContext* ctx, const char* funcname)
{
    ctx->error_number = 2;
    strncpy(ctx->error_desc, "Not implemented: ", DSS_ERR_NUM_CHR);
    strncat(ctx->error_desc, funcname, DSS_ERR_NUM_CHR - 20);
}

int32_t oddie_check_vararray_complex(OddieContext* ctx, void *ptr, int32_t ptrType, int32_t *ptrSize)
{
    if (NULL == ptr)
    {
        // ctx->error_number = 10;
        // ctx_Error_Set_Description(ctx, "Variant array error, null pointer returned.");
        return 1;
    }

    if (ptrType != ODDIE_PTR_VAR_TYPE_COMPLEX)
    {
        ctx->error_number = 11;
        ctx_Error_Set_Description(ctx, "Variant array error, expected complex");
        return 1;
    }

    *ptrSize /= 2 * sizeof(double);
    return 0;
}

int32_t oddie_check_vararray_float64(OddieContext* ctx, void *ptr, int32_t ptrType, int32_t *ptrSize)
{
    if (NULL == ptr)
    {
        // ctx->error_number = 12;
        // ctx_Error_Set_Description(ctx, "Variant array error, null pointer returned.");
        return 1;
    }

    if (ptrType != ODDIE_PTR_VAR_TYPE_DOUBLE && ptrType != ODDIE_PTR_VAR_TYPE_COMPLEX)
    {
        ctx->error_number = 13;
        ctx_Error_Set_Description(ctx, "Variant array error, expected complex or double");
        return 1;
    }

    *ptrSize /= sizeof(double);
    return 0;
}

void oddie_vararray_float64_func(OddieContext* ctx, oddie_variant_func_t func, int32_t mode, double** resultPtr, int32_t* resultDims, double* inPtr)
{
    void* ptr = inPtr;
    int32_t ptrType = -1;
    int32_t ptrSize = 0;
    *resultPtr = inPtr;

    func(mode, &ptr, &ptrType, &ptrSize);
    oddie_map_error(ctx);

    if (oddie_check_vararray_float64(ctx, ptr, ptrType, &ptrSize))
    {
        *resultPtr = NULL;
        resultDims[0] = 0;
        return;
    }

    *resultPtr = (double*) ptr;
    resultDims[0] = ptrSize;
    resultDims[1] = ptrSize;
    resultDims[2] = 0;
    resultDims[3] = 0;
}

int32_t oddie_check_vararray_int32(OddieContext* ctx, void *ptr, int32_t ptrType, int32_t *ptrSize)
{
    if (NULL == ptr)
    {
        // ctx->error_number = 14;
        // ctx_Error_Set_Description(ctx, "Variant array error, null pointer returned.");
        return 1;
    }

    if (ptrType != ODDIE_PTR_VAR_TYPE_INTEGER)
    {
        ctx->error_number = 15;
        ctx_Error_Set_Description(ctx, "Variant array error, expected integer");
        return 1;
    }

    *ptrSize /= sizeof(int32_t);
    return 0;
}

void oddie_vararray_int32_func(OddieContext* ctx, oddie_variant_func_t func, int32_t mode, int32_t** resultPtr, int32_t* resultDims)
{
    void *ptr = NULL;
    int32_t ptrType = -1;
    int32_t ptrSize = 0;

    func(mode, &ptr, &ptrType, &ptrSize);
    oddie_map_error(ctx);

    if (oddie_check_vararray_int32(ctx, ptr, ptrType, &ptrSize))
    {
        *resultPtr = NULL;
        resultDims[0] = 0;
        return;
    }

    *resultPtr = (int32_t*) ptr;
    resultDims[0] = ptrSize;
    resultDims[1] = ptrSize;
    resultDims[2] = 0;
    resultDims[3] = 0;
}

int32_t oddie_check_vararray_string(OddieContext* ctx, void *ptr, int32_t ptrType, int32_t *ptrSize)
{
    if (NULL == ptr)
    {
        // ctx->error_number = 16;
        // ctx_Error_Set_Description(ctx, "Variant array error, null pointer returned.");
        return 1;
    }

    if (ptrType != ODDIE_PTR_VAR_TYPE_STRING)
    {
        ctx->error_number = 17;
        ctx_Error_Set_Description(ctx, "Variant array error, expected string");
        return 1;
    }

    return 0;
}

void oddie_vararray_stringarray_func(OddieContext* ctx, oddie_variant_func_t func, int32_t mode, char*** resultPtr, int32_t* resultDims, char* inPtr)
{
    void* ptr = inPtr;
    int32_t ptrType = -1;
    int32_t ptrSize = 0;
    int32_t strCount = 0;
    int32_t i;
    char* strPtr_begin;
    char* strPtr_end;
    char* strPtr;
    char* strPtr_prev;
    char** res;

    func(mode, &ptr, &ptrType, &ptrSize);
    oddie_map_error(ctx);

    if (oddie_check_vararray_string(ctx, ptr, ptrType, &ptrSize))
    {
        *resultPtr = NULL;
        resultDims[0] = 0;
        return;
    }
    strPtr_begin = (char*) ptr;
    strPtr_end = strPtr_begin + ptrSize;
    for (strPtr = strPtr_begin; strPtr != strPtr_end; ++strPtr)
    {
        if (*strPtr == 0)
        {
            ++strCount;
        }
    }

    res = (char**) malloc(sizeof(char*) * strCount);
    i = 0;
    strPtr_prev = strPtr_begin;
    for (strPtr = strPtr_begin; strPtr != strPtr_end; ++strPtr)
    {
        if (*strPtr == 0)
        {
            res[i] = strPtr_prev;
            strPtr_prev = strPtr + 1;
            ++i;
        }
    }
    *resultPtr = res;
    resultDims[0] = strCount;
    resultDims[1] = strCount;
    resultDims[2] = 0;
    resultDims[3] = 0;
}

int32_t oddie_check_vararray_int8(OddieContext* ctx, void *ptr, int32_t ptrType, int32_t *ptrSize)
{
    if (NULL == ptr)
    {
        // ctx->error_number = 18;
        // ctx_Error_Set_Description(ctx, "Variant array error, null pointer returned.");
        return 1;
    }

    if (ptrType != ODDIE_PTR_VAR_TYPE_STRING)
    {
        ctx->error_number = 19;
        ctx_Error_Set_Description(ctx, "Variant array error, expected string/bytes");
        return 1;
    }

    // *ptrSize /= sizeof(int8_t);
    return 0;
}

void oddie_vararray_int8_func(OddieContext* ctx, oddie_variant_func_t func, int32_t mode, int8_t** resultPtr, int32_t* resultDims)
{
    void *ptr = NULL;
    int32_t ptrType = -1;
    int32_t ptrSize = 0;

    func(mode, &ptr, &ptrType, &ptrSize);
    oddie_map_error(ctx);

    if (oddie_check_vararray_int8(ctx, ptr, ptrType, &ptrSize))
    {
        *resultPtr = NULL;
        resultDims[0] = 0;
        return;
    }

    *resultPtr = (int8_t*) ptr;
    resultDims[0] = ptrSize;
    resultDims[1] = ptrSize;
    resultDims[2] = 0;
    resultDims[3] = 0;
}

int32_t oddie_get_int_property(const void* ctx, const char* className, const char* name, const char* queryCmd, const bool firstChar)
{
    OddieContext* oddie_ctx = (OddieContext*) ctx;
    const char *res;
    if (oddie_ctx->error_number || name == NULL || name[0] == 0)
    {
        return 0;
    }
    if (oddie_ctx->error_number)
    {
        return 0;
    }
    ctx_DSS_SetActiveClass(ctx, className);
    if (oddie_ctx->error_number)
    {
        return 0;
    }
    ctx_Text_Set_Command(ctx, queryCmd);
    if (oddie_ctx->error_number)
    {
        return 0;
    }
    res = ctx_Text_Get_Result(ctx);
    if (oddie_ctx->error_number || res == NULL || res[0] == 0)
    {
        return 0;
    }
    if (firstChar) 
    {
        return res[0];
    }
    return atoi(res);
}

void oddie_set_char_property(const void* ctx, const char* className, const char* name, const char* prop, const char ch)
{
    OddieContext* oddie_ctx = (OddieContext*) ctx;
    if (oddie_ctx->error_number || name == NULL || name[0] == 0)
    {
        return;
    }
    if (snprintf(oddie_ctx->char_buffer, DSS_STR_BUFFER_NUM_CHR, "%s.%s.%s=%c", className, name, prop, ch) <= 0)
    {
        oddie_ctx->error_number = 25;
        ctx_Error_Set_Description(ctx, "Oddie ERROR: could not format string for command.");
        return;
    }
    ctx_Text_Set_Command(ctx, oddie_ctx->char_buffer);    
}


ALTDSS_ODDIE_DLL void ctx_Text_Set_Command(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    const char* output = ((OddieContext*) ctx)->DSSPut_Command(Value);
    strncpy(((OddieContext*) ctx)->char_buffer, output, DSS_STR_BUFFER_NUM_CHR);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Text_CommandArray(const void* ctx, const char** ValuePtr, int32_t ValueCount)
{
    int32_t i;
    const char* output = NULL;
    if (!ctx) ctx = ctxPrime;
    for (i = 0; i < ValueCount; ++i)
    {
        if (ValuePtr[i])
        {
            output = ((OddieContext*) ctx)->DSSPut_Command(ValuePtr[i]);
        }
        oddie_map_error(ctx);
        // Abort if an error happened in the current line, or previously
        if (((OddieContext*) ctx)->error_number)
        {
            break;
        }
    }
    if (output != NULL)
    {
        strncpy(((OddieContext*) ctx)->char_buffer, output, DSS_STR_BUFFER_NUM_CHR);
    }
}

ALTDSS_ODDIE_DLL void ctx_Text_CommandBlock(const void* ctx, const char* Value)
{
    // Ported from an older version DSS C-API
    const char* posCurrent = Value;
    char* posNext0 = NULL;
    char* posEnd = NULL;
    const char* output = NULL;
    if (!ctx) ctx = ctxPrime;
    posEnd = strchr(posCurrent, '\0');
    posNext0 = strchr(posCurrent, '\n');
    if (posNext0 == NULL)
    {
        ctx_Text_Set_Command(ctx, Value);
        return;
    }

    while ((posCurrent + 1) < posEnd)
    {
        *posNext0 = '\0'; // set a zero to mark the end of a string
        output = ((OddieContext*) ctx)->DSSPut_Command(posCurrent);
        *posNext0 = '\n'; // get it back to the original char
        oddie_map_error(ctx);

        // Abort if an error happened in the current line, or previously
        if (((OddieContext*) ctx)->error_number)
        {
            break;
        }

        posCurrent = posNext0 + 1;
        posNext0 = strchr(posCurrent, '\n');
        if (posNext0 == NULL)
        {
            posNext0 = posEnd;
        }
    }

    if (output != NULL)
    {
        strncpy(((OddieContext*) ctx)->char_buffer, output, DSS_STR_BUFFER_NUM_CHR);
    }
}

ALTDSS_ODDIE_DLL const char* ctx_Text_Get_Result(const void* ctx)
{
    return ((OddieContext*)(ctx ? ctx : ctxPrime))->char_buffer;
}

ALTDSS_ODDIE_DLL const char* DSS_Get_PAnsiChar(void *p, int32_t index)
{
    return ((const char**) p)[index];
}

ALTDSS_ODDIE_DLL void ctx_DSS_GetGRPointers(const void* ctx, 
    char**** DataPtr_PPAnsiChar,
    double*** DataPtr_PDouble,
    int32_t*** DataPtr_PInteger,
    int8_t*** DataPtr_PByte,
    int32_t** CountPtr_PPAnsiChar,
    int32_t** CountPtr_PDouble,
    int32_t** CountPtr_PInteger,
    int32_t** CountPtr_PByte
)
{
    OddieContext* oddie_ctx = (OddieContext*) (ctx ? ctx : ctxPrime);

    *DataPtr_PPAnsiChar = NULL;
    *DataPtr_PDouble = &oddie_ctx->GR_DataPtr_PDouble;
    *DataPtr_PInteger = &oddie_ctx->GR_DataPtr_PInteger;
    *DataPtr_PByte = &oddie_ctx->GR_DataPtr_PByte;
    *CountPtr_PPAnsiChar = NULL;
    *CountPtr_PDouble = &oddie_ctx->GR_Counts_PDouble[0];
    *CountPtr_PInteger = &oddie_ctx->GR_Counts_PInteger[0];
    *CountPtr_PByte = &oddie_ctx->GR_Counts_PByte[0];
}

ALTDSS_ODDIE_DLL void DSS_SetMessagesMO(const char* Value)
{
    // do nothing; no equivalent
}

ALTDSS_ODDIE_DLL void DSS_SetPropertiesMO(const char* Value)
{
    // do nothing; no equivalent
}

ALTDSS_ODDIE_DLL void DSS_Dispose_PByte(int8_t** p)
{
    // do nothing; we don't control that data
}

ALTDSS_ODDIE_DLL void DSS_Dispose_PDouble(double** p)
{
    // do nothing; we don't control that data
}

ALTDSS_ODDIE_DLL void DSS_Dispose_PInteger(int32_t** p)
{
    // do nothing; we don't control that data
}

ALTDSS_ODDIE_DLL void DSS_Dispose_PPAnsiChar(char ***p, int32_t cnt)
{
    // free our marker pointers, but not the actual string data (also out of our control)
    free(*p);
    *p = NULL;
}

ALTDSS_ODDIE_DLL void DSS_Dispose_PPointer(void*** p)
{
    // do nothing; unused in this impl.
}

ALTDSS_ODDIE_DLL void DSS_Dispose_String(char* S)
{
    // free(S); // NOTE: this is not really the safest alternative, but it's what we have.
}

ALTDSS_ODDIE_DLL void ctx_DSS_DisposeGRData(const void* ctx)
{
    // do nothing; we don't control that data
}

ALTDSS_ODDIE_DLL uint16_t ctx_YMatrix_Get_SystemYChanged(const void* ctx)
{
    uint16_t res;
    CTX_OR_PRIME
    res = ((OddieContext*) ctx)->SystemYChanged (0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_YMatrix_Get_UseAuxCurrents(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res = ((OddieContext*) ctx)->UseAuxCurrents(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_YMatrix_AddInAuxCurrents(const void* ctx, int32_t SType)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->AddInAuxCurrents(SType);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_YMatrix_BuildYMatrixD(const void* ctx, int32_t BuildOps, int32_t AllocateVI)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->BuildYMatrixD(BuildOps, AllocateVI);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void *ctx_YMatrix_Get_Handle(const void* ctx)
{
    void* hY = NULL;
    uint32_t nBus, nNZ;
    CTX_OR_PRIME
    if (!((OddieContext*) ctx)->InitAndGetYparams(&hY, &nBus, &nNZ))
    {
        oddie_map_error(ctx);
        return NULL;
    }
    oddie_map_error(ctx);
    return hY;
}

ALTDSS_ODDIE_DLL void ctx_YMatrix_GetCompressedYMatrix(const void* ctx, uint16_t factor, uint32_t *nBus, uint32_t *nNz, int32_t **ColPtr, int32_t **RowIdxPtr, double **cValsPtr)
{
    CTX_OR_PRIME
    OddieContext* oddie_ctx = (OddieContext*) ctx;
    void* hY = NULL;
    oddie_ctx->InitAndGetYparams(&hY, nBus, nNz);
    oddie_map_error(ctx);
    if (oddie_ctx->error_number)
    {
        return;
    }
    oddie_ctx->GetCompressedYMatrix(hY, *nBus, *nNz, ColPtr, RowIdxPtr, cValsPtr);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_YMatrix_getIpointer(const void* ctx, double **IvectorPtr)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->getIpointer(IvectorPtr);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_YMatrix_GetPCInjCurr(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GetPCInjCurr();
    oddie_map_error(ctx);

}

ALTDSS_ODDIE_DLL void ctx_YMatrix_GetSourceInjCurrents(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GetSourceInjCurrents();
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_YMatrix_getVpointer(const void* ctx, double **VvectorPtr)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->getVpointer(VvectorPtr);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_YMatrix_Set_SystemYChanged(const void* ctx, uint16_t arg)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SystemYChanged(1, arg ? 1 : 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_YMatrix_Set_UseAuxCurrents(const void* ctx, uint16_t arg)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->UseAuxCurrents(1, arg ? 1 : 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL int32_t ctx_YMatrix_SolveSystem(const void* ctx, double *NodeVPtr)
{
    CTX_OR_PRIME
    int32_t res = ((OddieContext*) ctx)->SolveSystem(&NodeVPtr);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_YMatrix_ZeroInjCurr(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ZeroInjCurr();
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_BuildYMatrix(const void* ctx, int32_t BuildOption, int32_t AllocateVI)
{
    CTX_OR_PRIME
    ctx_YMatrix_BuildYMatrixD(ctx, BuildOption, AllocateVI);
}

ALTDSS_ODDIE_DLL void ctx_DSS_ResetStringBuffer(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "DSS_ResetStringBuffer");
}

ALTDSS_ODDIE_DLL uint16_t ctx_DSS_Get_AllowChangeDir(const void* ctx)
{
    return 1;
}

ALTDSS_ODDIE_DLL void ctx_DSS_Set_AllowChangeDir(const void* ctx, uint16_t Value)
{
    oddie_error_not_implemented((OddieContext*) ctx, "DSS_Set_AllowChangeDir");
}

ALTDSS_ODDIE_DLL int32_t* ctx_DSS_GR_CountPtr_PByte(const void* ctx)
{
    CTX_OR_PRIME
    return &((OddieContext*) ctx)->GR_Counts_PByte[0];
}

ALTDSS_ODDIE_DLL int32_t* ctx_DSS_GR_CountPtr_PDouble(const void* ctx)
{
    CTX_OR_PRIME
    return &((OddieContext*) ctx)->GR_Counts_PDouble[0];
}

ALTDSS_ODDIE_DLL int32_t* ctx_DSS_GR_CountPtr_PInteger(const void* ctx)
{
    CTX_OR_PRIME
    return &((OddieContext*) ctx)->GR_Counts_PInteger[0];
}

ALTDSS_ODDIE_DLL int8_t* ctx_DSS_GR_DataPtr_PByte(const void* ctx)
{
    CTX_OR_PRIME
    return ((OddieContext*) ctx)->GR_DataPtr_PByte;
}

ALTDSS_ODDIE_DLL double* ctx_DSS_GR_DataPtr_PDouble(const void* ctx)
{
    CTX_OR_PRIME
    return ((OddieContext*) ctx)->GR_DataPtr_PDouble;
}

ALTDSS_ODDIE_DLL int32_t* ctx_DSS_GR_DataPtr_PInteger(const void* ctx)
{
    CTX_OR_PRIME
    return ((OddieContext*) ctx)->GR_DataPtr_PInteger;
}

ALTDSS_ODDIE_DLL uint16_t ctx_DSS_Get_AllowDOScmd(const void* ctx)
{
    return 1;
}

ALTDSS_ODDIE_DLL uint16_t ctx_Error_Get_EarlyAbort(const void* ctx)
{
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_DSS_Set_AllowDOScmd(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "DSS_Set_AllowDOScmd");
}

ALTDSS_ODDIE_DLL void ctx_Error_Set_EarlyAbort(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Error_Set_EarlyAbort");
}

ALTDSS_ODDIE_DLL uint16_t ctx_DSS_Get_AllowEditor(const void* ctx)
{
    return 1;
}

ALTDSS_ODDIE_DLL void ctx_DSS_Set_AllowEditor(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "DSS_Set_AllowEditor");
}

ALTDSS_ODDIE_DLL const char* ctx_Error_Get_Description(const void* ctx)
{
    CTX_OR_PRIME
    return &((OddieContext*) ctx)->error_desc[0];
}

ALTDSS_ODDIE_DLL int32_t* ctx_Error_Get_NumberPtr(const void* ctx)
{
    CTX_OR_PRIME
    return &((OddieContext*) ctx)->error_number;
}

ALTDSS_ODDIE_DLL double ctx_CktElement_Get_Variablei(const void* ctx, int32_t Idx, int32_t *Code)
{
    *Code = 0;
    CTX_OR_PRIME
    OddieContext* oddie_ctx = (OddieContext*) ctx;

    // int32_t ec = oddie_ctx->CktElementI(14, Idx);
    // if (ec != 0)
    // {
    //     oddie_ctx->error_number = 100002;
    //     strncpy(oddie_ctx->error_desc, "Invalid variable or not a PC element.", DSS_ERR_NUM_CHR);
    //     *Code = 1;
    //     return;
    // }
    double res = oddie_ctx->CktElementF(4, (double) Idx);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL double ctx_CktElement_Get_Variable(const void* ctx, const char* MyVarName, int32_t *Code)
{
    CTX_OR_PRIME
    OddieContext* oddie_ctx = (OddieContext*) ctx;
    double res = 0;

    ctx_CktElement_Set_VariableName(ctx, MyVarName);
    if (oddie_ctx->error_number)
    {
        *Code = 1;
        return res;
    }

    res = oddie_ctx->CktElementF(5, 0);
    oddie_map_error(ctx);
    if (oddie_ctx->error_number)
    {
        *Code = 1;
        return res;
    }
    *Code = 0;
    return res;
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Set_Variable(const void* ctx, const char* MyVarName, int32_t *Code, double Value)
{
    CTX_OR_PRIME
    OddieContext* oddie_ctx = (OddieContext*) ctx;
    
    ctx_CktElement_Set_VariableName(ctx, MyVarName);
    if (oddie_ctx->error_number)
    {
        *Code = 1;
        return;
    }

    double res = oddie_ctx->CktElementF(5, Value);
    oddie_map_error(ctx);
    if (oddie_ctx->error_number)
    {
        *Code = 1;
        return;
    }
    if (res != 0)
    {
        oddie_ctx->error_number = 100002;
        strncpy(oddie_ctx->error_desc, "Invalid variable or not a PC element.", DSS_ERR_NUM_CHR);
        *Code = 1;
        return;
    }
    *Code = 0;
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Set_VariableName(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    OddieContext* oddie_ctx = (OddieContext*) ctx;
    const char* res = oddie_ctx->CktElementS(6, Value);
    oddie_map_error(ctx);
    if (!oddie_ctx->error_number && res != NULL && res[0] != 0 && res[0] == 'O' && res[1] == 'K' && res[2] == 0)
    {
        oddie_ctx->error_number = 100002;
        strncpy(oddie_ctx->error_desc, "Invalid variable name.", DSS_ERR_NUM_CHR);
    }
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Set_Points(const void* ctx, int32_t Npts, void *HoursPtr, void *PMultPtr, void *QMultPtr, uint16_t ExternalMemory, uint16_t IsFloat32, int32_t Stride)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "LoadShapes_Set_Points");
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_UseFloat32(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "LoadShapes_UseFloat32");
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_UseFloat64(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "LoadShapes_UseFloat64");
}

int32_t oddie_join_to_char_buffer(const void* ctx, const char** ValuePtr, int32_t ValueCount, int32_t* total_chars)
{
    CTX_OR_PRIME
    OddieContext* oddie_ctx = (OddieContext*) ctx;
    int32_t str_chars = 0;
    int32_t i;
    char *buffer = oddie_ctx->char_buffer;
    *buffer = 0;
    *total_chars = 0;
    for (i = 0; i < ValueCount; ++i)
    {
        str_chars = strlen(ValuePtr[i]) + 1;
        *total_chars += str_chars;
        if (*total_chars > DSS_STR_BUFFER_NUM_CHR)
        {
            oddie_ctx->error_number = 3;
            strncpy(oddie_ctx->error_desc, "Cannot copy string data to internal buffer.", DSS_ERR_NUM_CHR);
            return 1; // cannot handle this
        }
        strncpy(buffer, ValuePtr[i], str_chars);
        buffer += str_chars;
        *buffer = 0;
    }
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Set_BusNames(const void* ctx, const char** ValuePtr, int32_t ValueCount)
{
    int32_t var_type = ODDIE_PTR_VAR_TYPE_STRING;
    int32_t total_chars = 0;
    void *inPtr = NULL;
    CTX_OR_PRIME
    if (oddie_join_to_char_buffer(ctx, ValuePtr, ValueCount, &total_chars))
    {
        return;
    }
    inPtr = ((OddieContext*) ctx)->char_buffer;
    ((OddieContext*) ctx)->CktElementV(1, &inPtr, &var_type, &total_chars);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Fuses_Set_NormalState(const void* ctx, const char** ValuePtr, int32_t ValueCount)
{
    int32_t var_type = ODDIE_PTR_VAR_TYPE_STRING;
    int32_t total_chars = 0;
    void *inPtr = NULL;
    CTX_OR_PRIME
    if (oddie_join_to_char_buffer(ctx, ValuePtr, ValueCount, &total_chars))
    {
        return;
    }
    inPtr = ((OddieContext*) ctx)->char_buffer;
    ((OddieContext*) ctx)->FusesV(4, &inPtr, &var_type, &total_chars);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Fuses_Set_State(const void* ctx, const char** ValuePtr, int32_t ValueCount)
{
    int32_t var_type = ODDIE_PTR_VAR_TYPE_STRING;
    int32_t total_chars = 0;
    void *inPtr = NULL;
    CTX_OR_PRIME
    if (oddie_join_to_char_buffer(ctx, ValuePtr, ValueCount, &total_chars))
    {
        return;
    }
    inPtr = ((OddieContext*) ctx)->char_buffer;
    ((OddieContext*) ctx)->FusesV(2, &inPtr, &var_type, &total_chars);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL int32_t ctx_Circuit_SetActiveBus(const void* ctx, const char* BusName)
{
    CTX_OR_PRIME
    const char *res = ((OddieContext*) ctx)->CircuitS(4, BusName);
    oddie_map_error(ctx);
    return (res != NULL && *res != 0) ? atoi(res) : -1;
}

ALTDSS_ODDIE_DLL int32_t ctx_CtrlQueue_Push(const void* ctx, int32_t Hour, double Seconds, int32_t ActionCode, int32_t DeviceHandle)
{
    double inData[4] = {Hour, Seconds, ActionCode, DeviceHandle};
    void *inPtr = &inData[0];
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    int32_t queueSize = -1;
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CtrlQueueV(1, &inPtr, &var_type, &queueSize);
    oddie_map_error(ctx);
    return queueSize;
}

ALTDSS_ODDIE_DLL int32_t ctx_Circuit_SetActiveClass(const void* ctx, const char* ClassName)
{
    CTX_OR_PRIME
    const char *res = ((OddieContext*) ctx)->CircuitS(5, ClassName);
    oddie_map_error(ctx);
    return (res != NULL && *res != 0) ? atoi(res) : -1;
}

ALTDSS_ODDIE_DLL int32_t ctx_Circuit_SetActiveElement(const void* ctx, const char* FullName)
{
    CTX_OR_PRIME
    const char *res = ((OddieContext*) ctx)->CircuitS(3, FullName);
    oddie_map_error(ctx);
    return (res != NULL && *res != 0) ? atoi(res) : -1;
}

ALTDSS_ODDIE_DLL void ctx_Relays_Set_NormalState(const void* ctx, int32_t Value)
{
    char action[2];
    action[0] = Value == CTRL_CLOSE ? 'c' : 'o';
    action[1] = 0;
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RelaysS(9, action);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Relays_Set_State(const void* ctx, int32_t Value)
{
    char action[2];
    action[0] = Value == CTRL_CLOSE ? 'c' : 'o';
    action[1] = 0;
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RelaysS(7, action);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Reclosers_Set_NormalState(const void* ctx, int32_t Value)
{
    char action[2];
    action[0] = Value == CTRL_CLOSE ? 'c' : 'o';
    action[1] = 0;
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReclosersS(9, action);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Reclosers_Set_State(const void* ctx, int32_t Value)
{
    char action[2];
    action[0] = Value == CTRL_CLOSE ? 'c' : 'o';
    action[1] = 0;
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReclosersS(7, action);
    oddie_map_error(ctx);
}

int32_t oddie_map_ctrl_state(const char* state)
{
    if (state == NULL || state == 0)
    {
        return -1;
    }
    return (state[0] == 'c' || state[0] == 'C') ? CTRL_CLOSE : CTRL_OPEN;
}

ALTDSS_ODDIE_DLL int32_t ctx_Reclosers_Get_NormalState(const void* ctx)
{
    CTX_OR_PRIME
    const char* res = ((OddieContext*) ctx)->ReclosersS(8, NULL);
    oddie_map_error(ctx);
    return oddie_map_ctrl_state(res);
}

ALTDSS_ODDIE_DLL int32_t ctx_Reclosers_Get_State(const void* ctx)
{
    CTX_OR_PRIME
    const char *res = ((OddieContext*) ctx)->ReclosersS(6, NULL);
    oddie_map_error(ctx);
    return oddie_map_ctrl_state(res);
}

ALTDSS_ODDIE_DLL int32_t ctx_Relays_Get_NormalState(const void* ctx)
{
    CTX_OR_PRIME
    const char *res = ((OddieContext*) ctx)->RelaysS(8, 0);
    oddie_map_error(ctx);
    return oddie_map_ctrl_state(res);
}

ALTDSS_ODDIE_DLL int32_t ctx_Relays_Get_State(const void* ctx)
{
    CTX_OR_PRIME
    const char *res = ((OddieContext*) ctx)->RelaysS(6, 0);
    oddie_map_error(ctx);
    return oddie_map_ctrl_state(res);
}

ALTDSS_ODDIE_DLL void Oddie_SetLibOptions(const char* libname, uint32_t* liboptions)
{
    ODDIE_LIB_NAME = libname;
    if (liboptions)
    {
        ODDIE_LIB_OPTIONS = *liboptions;
    }
}

ALTDSS_ODDIE_DLL void Oddie_SetOptions(const void *ctx, uint32_t flags)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->map_errors = flags & 0x01;
}

ALTDSS_ODDIE_DLL void ctx_XYCurves_Set_Xarray(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    CTX_OR_PRIME
    int32_t expected_cnt = ctx_XYCurves_Get_Npts(ctx);
    // Limit number of elements to avoid crash.
    // TODO: maybe change this to an error?
    if (expected_cnt < ValueCount)
    {
        ValueCount = expected_cnt;
    }
    ((OddieContext*) ctx)->XYCurvesV(1, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL int32_t ctx_Lines_New(const void* ctx, const char* Name)
{
    CTX_OR_PRIME
    OddieContext* oddie_ctx = (OddieContext*) ctx;
    if (snprintf(oddie_ctx->char_buffer, DSS_STR_BUFFER_NUM_CHR, "New Line.\"%s\"", Name) <= 0)
    {
        oddie_ctx->error_number = 20;
        ctx_Error_Set_Description(ctx, "Oddie ERROR: could not format string for command.");
        return 0;
    }
    ctx_Text_Set_Command(ctx, oddie_ctx->char_buffer);
    if (oddie_ctx->error_number)
    {
        return 0;
    }
    return 1;
}

ALTDSS_ODDIE_DLL int32_t ctx_LoadShapes_New(const void* ctx, const char* Name)
{
    CTX_OR_PRIME
    OddieContext* oddie_ctx = (OddieContext*) ctx;
    if (snprintf(oddie_ctx->char_buffer, DSS_STR_BUFFER_NUM_CHR, "New LoadShape.\"%s\"", Name) <= 0)
    {
        oddie_ctx->error_number = 21;
        ctx_Error_Set_Description(ctx, "Oddie ERROR: could not format string for command.");
        return 0;
    }
    ctx_Text_Set_Command(ctx, oddie_ctx->char_buffer);
    if (oddie_ctx->error_number)
    {
        return 0;
    }
    return 1;
}

ALTDSS_ODDIE_DLL int32_t ctx_DSS_SetActiveClass(const void* ctx, const char* ClassName)
{
    CTX_OR_PRIME
    const char* res = ((OddieContext*) ctx)->CircuitS(5, ClassName);
    oddie_map_error(ctx);
    return (res != NULL && *res != 0) ? atoi(res) : -1;
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_IsSwitch(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    OddieContext* oddie_ctx = (OddieContext*) ctx;
    ctx_Lines_Get_Units(ctx);
    if (oddie_ctx->error_number)
    {
        return;
    }
    oddie_set_char_property(ctx, "Line", ctx_Lines_Get_Name(ctx), "Switch", Value ? 'y' : 'n');
}

ALTDSS_ODDIE_DLL uint16_t ctx_Lines_Get_IsSwitch(const void* ctx)
{
    CTX_OR_PRIME
    OddieContext* oddie_ctx = (OddieContext*) ctx;
    char r;
    ctx_Lines_Get_Units(ctx);
    r = (char) oddie_get_int_property(ctx, "Line", ctx_Lines_Get_Name(ctx), "? Switch", true);
    return (r == 't') || (r == 'y') || (r == 'T') || (r == 'Y');
}

ALTDSS_ODDIE_DLL int32_t ctx_Loads_Get_Phases(const void* ctx)
{
    CTX_OR_PRIME
    return oddie_get_int_property(ctx, "Load", ctx_Loads_Get_Name(ctx), "? Phases", false);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_Phases(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    OddieContext* oddie_ctx = (OddieContext*) ctx;
    const char *name = ctx_Loads_Get_Name(ctx);
    if (oddie_ctx->error_number || name == NULL || name[0] == 0)
    {
        return;
    }
    if (snprintf(oddie_ctx->char_buffer, DSS_STR_BUFFER_NUM_CHR, "Load.%s.Phases=%d", name, Value) <= 0)
    {
        oddie_ctx->error_number = 23;
        ctx_Error_Set_Description(ctx, "Oddie ERROR: could not format string for command.");
        return;
    }
    ctx_Text_Set_Command(ctx, oddie_ctx->char_buffer);    
}

ALTDSS_ODDIE_DLL void ctx_CapControls_Reset(const void* ctx)
{
    CTX_OR_PRIME
    oddie_set_char_property(ctx, "CapControl", ctx_CapControls_Get_Name(ctx), "Reset", 'y');
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Reset(const void* ctx)
{
    CTX_OR_PRIME
    oddie_set_char_property(ctx, "RegControl", ctx_RegControls_Get_Name(ctx), "Reset", 'y');
}

ALTDSS_ODDIE_DLL void ctx_SwtControls_Reset(const void* ctx)
{
    CTX_OR_PRIME
    oddie_set_char_property(ctx, "SwtControl", ctx_SwtControls_Get_Name(ctx), "Reset", 'y');
}

ALTDSS_ODDIE_DLL int32_t ctx_SwtControls_Get_NormalState(const void* ctx)
{
    CTX_OR_PRIME
    char r = (char) oddie_get_int_property(ctx, "SwtControl", ctx_SwtControls_Get_Name(ctx), "? Normal", true);
    return (r == 'c' || r == 'C') ? CTRL_CLOSE : ((r == 'o' || r == 'O') ? CTRL_OPEN : CTRL_NONE);
}

ALTDSS_ODDIE_DLL int32_t ctx_SwtControls_Get_State(const void* ctx)
{
    CTX_OR_PRIME
    char r = (char) oddie_get_int_property(ctx, "SwtControl", ctx_SwtControls_Get_Name(ctx), "? State", true);
    return (r == 'c' || r == 'C') ? CTRL_CLOSE : ((r == 'o' || r == 'O') ? CTRL_OPEN : CTRL_NONE);
}

ALTDSS_ODDIE_DLL void ctx_SwtControls_Set_State(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_set_char_property(ctx, "SwtControl", ctx_SwtControls_Get_Name(ctx), "State", Value == CTRL_CLOSE ? 'c' : 'o');
}

ALTDSS_ODDIE_DLL void ctx_SwtControls_Set_NormalState(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_set_char_property(ctx, "SwtControl", ctx_SwtControls_Get_Name(ctx), "Normal", Value == CTRL_CLOSE ? 'c' : 'o');
}

ALTDSS_ODDIE_DLL void ctx_DSSProperty_Set_Index(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    const char* res;
    int32_t numProps = ctx_DSSElement_Get_NumProperties(ctx);
    OddieContext* oddie_ctx = (OddieContext*) ctx;
    if (oddie_ctx->error_number)
    {
        return;
    }
    ++Value;
    oddie_ctx->PropIndex = Value;
    if (Value <= 0 || Value > numProps)
    {
        oddie_ctx->error_number = 33;
        strncpy(oddie_ctx->error_desc, "Invalid property index.", DSS_ERR_NUM_CHR);
    }
}

ALTDSS_ODDIE_DLL void ctx_DSSProperty_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    const char* all_names;
    OddieContext* oddie_ctx = (OddieContext*) ctx;
    char** ResultPtr = NULL;
    int32_t ResultDims[4] = {0, 0, 0, 0};
    int32_t i = 0;
    int32_t found = 0;

    ctx_DSSElement_Get_AllPropertyNames(ctx, &ResultPtr, ResultDims);

    if (oddie_ctx->error_number || ResultDims[0] == 0 || ResultPtr == NULL || NULL == *ResultPtr)
    {
        if (ResultPtr)
        {
            DSS_Dispose_PPAnsiChar(&ResultPtr, ResultDims[0]);
        }
        return;
    }

    for (i = 0; i < ResultDims[0]; ++i)
    {
        if (strcasecmp(Value, ResultPtr[i]) == 0)
        {
            found = i + 1;
            oddie_ctx->PropIndex = found;
            break;
        }
    }

    if (ResultPtr)
    {
        DSS_Dispose_PPAnsiChar(&ResultPtr, ResultDims[0]);
    }

    if (found == 0)
    {
        oddie_ctx->error_number = 34;
        strncpy(oddie_ctx->error_desc, "Invalid property name.", DSS_ERR_NUM_CHR);
    }
}

ALTDSS_ODDIE_DLL const char* ctx_DSSProperty_Get_Description(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    OddieContext* oddie_ctx = (OddieContext*) ctx;
    res = oddie_ctx->DSSProperties(1, oddie_int32_to_pchar(oddie_ctx, oddie_ctx->PropIndex));
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_DSSProperty_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    OddieContext* oddie_ctx = (OddieContext*) ctx;
    res = oddie_ctx->DSSProperties(0, oddie_int32_to_pchar(oddie_ctx, oddie_ctx->PropIndex));
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_DSSProperty_Get_Val(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    OddieContext* oddie_ctx = (OddieContext*) ctx;
    res = oddie_ctx->DSSProperties(2, oddie_int32_to_pchar(oddie_ctx, oddie_ctx->PropIndex));
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_DSSProperty_Set_Val(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSProperties(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL const char* ctx_ActiveClass_Get_ActiveClassName(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->ActiveClassS(2, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_ActiveClass_Get_ActiveClassParent(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->ActiveClassS(3, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_ActiveClass_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->ActiveClassV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_ActiveClass_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->ActiveClassI(3, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_ActiveClass_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->ActiveClassI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_ActiveClass_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->ActiveClassS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_ActiveClass_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->ActiveClassI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_ActiveClass_Get_NumElements(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->ActiveClassI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_ActiveClass_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ActiveClassS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL int32_t ctx_Bus_GetUniqueNodeNumber(const void* ctx, int32_t StartNumber)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->BUSI(3, StartNumber);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_AllPCEatBus(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 18, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_AllPDEatBus(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 19, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL uint16_t ctx_Bus_Get_Coorddefined(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->BUSI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_CplxSeqVoltages(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 10, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_CplxSeqVoltages_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 10, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL double ctx_Bus_Get_Cust_Duration(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->BUSF(10, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Bus_Get_Cust_Interrupts(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->BUSF(9, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Bus_Get_Distance(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->BUSF(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Bus_Get_Int_Duration(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->BUSF(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_Isc(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 4, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_Isc_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 4, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL double ctx_Bus_Get_Lambda(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->BUSF(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_LineList(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 15, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_LoadList(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 16, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_Bus_Get_N_Customers(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->BUSI(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Bus_Get_N_interrupts(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->BUSF(7, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Bus_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->BUSS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_Nodes(const void* ctx, int32_t** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_int32_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 2, ResultPtr, ResultDims);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_Nodes_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_vararray_int32_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 2, &((OddieContext*) ctx)->GR_DataPtr_PInteger, &((OddieContext*) ctx)->GR_Counts_PInteger[0]);
}

ALTDSS_ODDIE_DLL int32_t ctx_Bus_Get_NumNodes(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->BUSI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Bus_Get_SectionID(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->BUSI(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_SeqVoltages(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 1, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_SeqVoltages_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 1, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL double ctx_Bus_Get_TotalMiles(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->BUSF(11, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_VLL(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 11, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_VLL_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 11, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_VMagAngle(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 13, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_VMagAngle_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 13, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_Voc(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 3, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_Voc_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 3, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_Voltages(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_Voltages_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 0, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_YscMatrix(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 9, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_YscMatrix_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 9, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_ZSC012Matrix(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 17, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_ZSC012Matrix_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 17, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_Zsc0(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 8, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_Zsc0_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 8, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_Zsc1(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 7, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_Zsc1_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 7, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_ZscMatrix(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 6, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_ZscMatrix_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 6, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL double ctx_Bus_Get_kVBase(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->BUSF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_puVLL(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 12, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_puVLL_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 12, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_puVmagAngle(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 14, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_puVmagAngle_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 14, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_puVoltages(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 5, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Get_puVoltages_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->BUSV, 5, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL double ctx_Bus_Get_x(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->BUSF(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Bus_Get_y(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->BUSF(3, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Bus_Set_x(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->BUSF(2, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Bus_Set_y(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->BUSF(4, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL uint16_t ctx_Bus_ZscRefresh(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->BUSI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_CapControls_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->CapControlsV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL double ctx_CapControls_Get_CTratio(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->CapControlsF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_CapControls_Get_Capacitor(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->CapControlsS(2, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_CapControls_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CapControlsI(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_CapControls_Get_DeadTime(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->CapControlsF(16, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_CapControls_Get_Delay(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->CapControlsF(12, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_CapControls_Get_DelayOff(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->CapControlsF(14, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_CapControls_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CapControlsI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_CapControls_Get_Mode(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CapControlsI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_CapControls_Get_MonitoredObj(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->CapControlsS(4, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_CapControls_Get_MonitoredTerm(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CapControlsI(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_CapControls_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->CapControlsS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_CapControls_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CapControlsI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_CapControls_Get_OFFSetting(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->CapControlsF(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_CapControls_Get_ONSetting(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->CapControlsF(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_CapControls_Get_PTratio(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->CapControlsF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_CapControls_Get_UseVoltOverride(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->CapControlsI(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_CapControls_Get_Vmax(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->CapControlsF(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_CapControls_Get_Vmin(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->CapControlsF(10, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_CapControls_Set_CTratio(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapControlsF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CapControls_Set_Capacitor(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapControlsS(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CapControls_Set_DeadTime(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapControlsF(17, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CapControls_Set_Delay(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapControlsF(13, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CapControls_Set_DelayOff(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapControlsF(15, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CapControls_Set_Mode(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapControlsI(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CapControls_Set_MonitoredObj(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapControlsS(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CapControls_Set_MonitoredTerm(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapControlsI(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CapControls_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapControlsS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CapControls_Set_OFFSetting(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapControlsF(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CapControls_Set_ONSetting(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapControlsF(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CapControls_Set_PTratio(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapControlsF(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CapControls_Set_UseVoltOverride(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapControlsI(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CapControls_Set_Vmax(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapControlsF(9, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CapControls_Set_Vmin(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapControlsF(11, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL uint16_t ctx_Capacitors_AddStep(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->CapacitorsI(7, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Capacitors_Close(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapacitorsI(11, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Capacitors_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->CapacitorsV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_Capacitors_Get_AvailableSteps(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CapacitorsI(9, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Capacitors_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CapacitorsI(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Capacitors_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CapacitorsI(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_Capacitors_Get_IsDelta(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->CapacitorsI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Capacitors_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->CapacitorsS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Capacitors_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CapacitorsI(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Capacitors_Get_NumSteps(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CapacitorsI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Capacitors_Get_States(const void* ctx, int32_t** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_int32_func((OddieContext*) ctx, ((OddieContext*) ctx)->CapacitorsV, 1, ResultPtr, ResultDims);
}

ALTDSS_ODDIE_DLL void ctx_Capacitors_Get_States_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_vararray_int32_func((OddieContext*) ctx, ((OddieContext*) ctx)->CapacitorsV, 1, &((OddieContext*) ctx)->GR_DataPtr_PInteger, &((OddieContext*) ctx)->GR_Counts_PInteger[0]);
}

ALTDSS_ODDIE_DLL double ctx_Capacitors_Get_kV(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->CapacitorsF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Capacitors_Get_kvar(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->CapacitorsF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Capacitors_Open(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapacitorsI(10, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Capacitors_Set_IsDelta(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapacitorsI(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Capacitors_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapacitorsS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Capacitors_Set_NumSteps(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapacitorsI(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Capacitors_Set_States(const void* ctx, const int32_t* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_INTEGER;
    ((OddieContext*) ctx)->CapacitorsV(2, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Capacitors_Set_kV(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapacitorsF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Capacitors_Set_kvar(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CapacitorsF(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL uint16_t ctx_Capacitors_SubtractStep(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->CapacitorsI(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Circuit_Capacity(const void* ctx, double Start, double Increment)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->CircuitF(0, Start, Increment);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Disable(const void* ctx, const char* Name)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CircuitS(1, Name);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Enable(const void* ctx, const char* Name)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CircuitS(2, Name);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_EndOfTimeStepUpdate(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CircuitI(14, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL int32_t ctx_Circuit_FirstElement(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CircuitI(10, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Circuit_FirstPCElement(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CircuitI(3, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Circuit_FirstPDElement(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CircuitI(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllBusDistances(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 12, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllBusDistances_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 12, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllBusNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 7, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllBusVmag(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 5, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllBusVmag_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 5, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllBusVmagPu(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 9, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllBusVmagPu_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 9, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllBusVolts(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 4, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllBusVolts_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 4, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllElementLosses(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 8, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllElementLosses_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 8, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllElementNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 6, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllNodeDistances(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 13, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllNodeDistances_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 13, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllNodeDistancesByPhase(const void* ctx, double** ResultPtr, int32_t* ResultDims, int32_t Phase)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 16, ResultPtr, ResultDims, (double*) &Phase);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllNodeDistancesByPhase_GR(const void* ctx, int32_t Phase)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 16, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], (double*) &Phase);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllNodeNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 10, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllNodeNamesByPhase(const void* ctx, char*** ResultPtr, int32_t* ResultDims, int32_t Phase)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 17, ResultPtr, ResultDims, (char*) &Phase);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllNodeVmagByPhase(const void* ctx, double** ResultPtr, int32_t* ResultDims, int32_t Phase)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 14, ResultPtr, ResultDims, (double*) &Phase);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllNodeVmagByPhase_GR(const void* ctx, int32_t Phase)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 14, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], (double*) &Phase);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllNodeVmagPUByPhase(const void* ctx, double** ResultPtr, int32_t* ResultDims, int32_t Phase)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 15, ResultPtr, ResultDims, (double*) &Phase);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_AllNodeVmagPUByPhase_GR(const void* ctx, int32_t Phase)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 15, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], (double*) &Phase);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_LineLosses(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 1, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_LineLosses_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 1, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_Losses(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_Losses_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 0, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL const char* ctx_Circuit_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->CircuitS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Circuit_Get_NumBuses(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CircuitI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Circuit_Get_NumCktElements(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CircuitI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Circuit_Get_NumNodes(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CircuitI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Circuit_Get_ParentPDElement(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CircuitI(13, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_SubstationLosses(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 2, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_SubstationLosses_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 2, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_SystemY(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 11, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_SystemY_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 11, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_TotalPower(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 3, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_TotalPower_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 3, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_YCurrents(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 20, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_YCurrents_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 20, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_YNodeOrder(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 19, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_YNodeVarray(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 18, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_YNodeVarray_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CircuitV, 18, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_Circuit_NextElement(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CircuitI(11, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Circuit_NextPCElement(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CircuitI(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Circuit_NextPDElement(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CircuitI(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Sample(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CircuitI(7, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Circuit_SaveSample(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CircuitI(8, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL int32_t ctx_Circuit_SetActiveBusi(const void* ctx, int32_t BusIndex)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CircuitI(9, BusIndex);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Circuit_UpdateStorage(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CircuitI(12, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Close(const void* ctx, int32_t Term, int32_t Phs)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CktElementI(4, Phs);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_AllPropertyNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 10, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_AllVariableNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 15, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_AllVariableValues(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 16, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_AllVariableValues_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 16, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_BusNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL const char* ctx_CktElement_Get_Controller(const void* ctx, int32_t idx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->CktElementS(5, oddie_int32_to_pchar((OddieContext*) ctx, idx));
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_CplxSeqCurrents(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 14, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_CplxSeqCurrents_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 14, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_CplxSeqVoltages(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 13, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_CplxSeqVoltages_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 13, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_Currents(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 3, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_Currents_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 3, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_CurrentsMagAng(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 18, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_CurrentsMagAng_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 18, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL const char* ctx_CktElement_Get_DisplayName(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->CktElementS(1, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_CktElement_Get_EmergAmps(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->CktElementF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_CktElement_Get_Enabled(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->CktElementI(12, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_CktElement_Get_EnergyMeter(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->CktElementS(4, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_CktElement_Get_GUID(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->CktElementS(3, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_CktElement_Get_HasSwitchControl(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->CktElementI(7, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_CktElement_Get_HasVoltControl(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->CktElementI(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_Losses(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 5, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_Losses_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 5, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL const char* ctx_CktElement_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->CktElementS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_NodeOrder(const void* ctx, int32_t** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_int32_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 17, ResultPtr, ResultDims);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_NodeOrder_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_vararray_int32_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 17, &((OddieContext*) ctx)->GR_DataPtr_PInteger, &((OddieContext*) ctx)->GR_Counts_PInteger[0]);
}

ALTDSS_ODDIE_DLL double ctx_CktElement_Get_NormalAmps(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->CktElementF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_CktElement_Get_NumConductors(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CktElementI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_CktElement_Get_NumControls(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CktElementI(9, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_CktElement_Get_NumPhases(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CktElementI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_CktElement_Get_NumProperties(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CktElementI(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_CktElement_Get_NumTerminals(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CktElementI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_CktElement_Get_OCPDevIndex(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CktElementI(10, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_CktElement_Get_OCPDevType(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CktElementI(11, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_PhaseLosses(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 6, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_PhaseLosses_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 6, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_Powers(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 4, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_Powers_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 4, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_Residuals(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 11, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_Residuals_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 11, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_SeqCurrents(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 8, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_SeqCurrents_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 8, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_SeqPowers(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 9, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_SeqPowers_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 9, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_SeqVoltages(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 7, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_SeqVoltages_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 7, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_TotalPowers(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 20, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_TotalPowers_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 20, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL const char* ctx_CktElement_Get_VariableName(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->CktElementS(6, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_CktElement_Get_VariableValue(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->CktElementF(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_Voltages(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 2, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_Voltages_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 2, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_VoltagesMagAng(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 19, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_VoltagesMagAng_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 19, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_Yprim(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 12, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_Yprim_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CktElementV, 12, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL uint16_t ctx_CktElement_IsOpen(const void* ctx, int32_t Term, int32_t Phs)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->CktElementI(5, Phs);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Open(const void* ctx, int32_t Term, int32_t Phs)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CktElementI(3, Phs);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Set_DisplayName(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CktElementS(2, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Set_EmergAmps(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CktElementF(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Set_Enabled(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CktElementI(13, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Set_NormalAmps(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CktElementF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Set_VariableIdx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CktElementI(14, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Set_VariableValue(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CktElementF(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL double ctx_CmathLib_Get_cabs(const void* ctx, double realpart, double imagpart)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->CmathLibF(0, realpart, imagpart);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_CmathLib_Get_cmplx(const void* ctx, double** ResultPtr, int32_t* ResultDims, double RealPart, double ImagPart)
{
    double inPtr[2] = {RealPart, ImagPart};
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CmathLibV, 0, ResultPtr, ResultDims, (double*) &inPtr[0]);
}

ALTDSS_ODDIE_DLL void ctx_CmathLib_Get_cmplx_GR(const void* ctx, double RealPart, double ImagPart)
{  
    CTX_OR_PRIME
    double inPtr[2] = {RealPart, ImagPart};
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CmathLibV, 0, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], (double*) &inPtr[0]);
}

ALTDSS_ODDIE_DLL void ctx_CmathLib_Get_ctopolardeg(const void* ctx, double** ResultPtr, int32_t* ResultDims, double RealPart, double ImagPart)
{
    double inPtr[2] = {RealPart, ImagPart};
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CmathLibV, 1, ResultPtr, ResultDims, (double*) &inPtr[0]);
}

ALTDSS_ODDIE_DLL void ctx_CmathLib_Get_ctopolardeg_GR(const void* ctx, double RealPart, double ImagPart)
{  
    CTX_OR_PRIME
    double inPtr[2] = {RealPart, ImagPart};
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CmathLibV, 1, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], (double*) &inPtr[0]);
}

ALTDSS_ODDIE_DLL void ctx_CmathLib_Get_pdegtocomplex(const void* ctx, double** ResultPtr, int32_t* ResultDims, double magnitude, double angle)
{
    double inPtr[2] = {magnitude, angle};
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CmathLibV, 2, ResultPtr, ResultDims, (double*) &inPtr[0]);
}

ALTDSS_ODDIE_DLL void ctx_CmathLib_Get_pdegtocomplex_GR(const void* ctx, double magnitude, double angle)
{  
    CTX_OR_PRIME
    double inPtr[2] = {magnitude, angle};
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->CmathLibV, 2, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], (double*) &inPtr[0]);
}

ALTDSS_ODDIE_DLL void ctx_CtrlQueue_ClearActions(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CtrlQueueI(7, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CtrlQueue_ClearQueue(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CtrlQueueI(0, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CtrlQueue_Delete(const void* ctx, int32_t ActionHandle)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CtrlQueueI(1, ActionHandle);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CtrlQueue_DoAllQueue(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CtrlQueueI(10, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL int32_t ctx_CtrlQueue_Get_ActionCode(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CtrlQueueI(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_CtrlQueue_Get_DeviceHandle(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CtrlQueueI(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_CtrlQueue_Get_NumActions(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CtrlQueueI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_CtrlQueue_Get_PopAction(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CtrlQueueI(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_CtrlQueue_Get_Queue(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->CtrlQueueV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_CtrlQueue_Get_QueueSize(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->CtrlQueueI(9, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_CtrlQueue_Set_Action(const void* ctx, int32_t Param1)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CtrlQueueI(3, Param1);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_CtrlQueue_Show(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->CtrlQueueI(6, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_DSSElement_Get_AllPropertyNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->DSSElementV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL const char* ctx_DSSElement_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->DSSElementS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_DSSElement_Get_NumProperties(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->DSSElementI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_DSSProgress_Close(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSProgressI(2, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_DSSProgress_Set_Caption(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSProgressS(0, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_DSSProgress_Set_PctProgress(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSProgressI(0, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_DSSProgress_Show(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSProgressI(1, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_DSS_ClearAll(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSI(1, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL const char* ctx_DSS_Executive_Get_Command(const void* ctx, int32_t i)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->DSSExecutiveS(0, oddie_int32_to_pchar((OddieContext*) ctx, i));
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_DSS_Executive_Get_CommandHelp(const void* ctx, int32_t i)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->DSSExecutiveS(2, oddie_int32_to_pchar((OddieContext*) ctx, i));
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_DSS_Executive_Get_NumCommands(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->DSSExecutiveI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_DSS_Executive_Get_NumOptions(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->DSSExecutiveI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_DSS_Executive_Get_Option(const void* ctx, int32_t i)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->DSSExecutiveS(1, oddie_int32_to_pchar((OddieContext*) ctx, i));
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_DSS_Executive_Get_OptionHelp(const void* ctx, int32_t i)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->DSSExecutiveS(3, oddie_int32_to_pchar((OddieContext*) ctx, i));
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_DSS_Executive_Get_OptionValue(const void* ctx, int32_t i)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->DSSExecutiveS(4, oddie_int32_to_pchar((OddieContext*) ctx, i));
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_DSS_Get_AllowForms(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->DSSI(7, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_DSS_Get_Classes(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->DSSV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL const char* ctx_DSS_Get_DataPath(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->DSSS(2, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_DSS_Get_DefaultEditor(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->DSSS(4, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_DSS_Get_NumCircuits(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->DSSI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_DSS_Get_NumClasses(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->DSSI(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_DSS_Get_NumUserClasses(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->DSSI(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_DSS_Get_UserClasses(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->DSSV, 1, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL const char* ctx_DSS_Get_Version(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->DSSS(1, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_DSS_NewCircuit(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSS(0, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_DSS_Reset(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSI(6, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_DSS_Set_AllowForms(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSI(8, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_DSS_Set_DataPath(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSS(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL uint16_t ctx_DSS_Start(const void* ctx, int32_t code)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->DSSI(3, code);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Fuses_Close(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->FusesI(8, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Fuses_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->FusesV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_Fuses_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->FusesI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Fuses_Get_Delay(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->FusesF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Fuses_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->FusesI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Fuses_Get_MonitoredObj(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->FusesS(2, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Fuses_Get_MonitoredTerm(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->FusesI(3, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Fuses_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->FusesS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Fuses_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->FusesI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Fuses_Get_NormalState(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->FusesV, 3, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_Fuses_Get_NumPhases(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->FusesI(12, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Fuses_Get_RatedCurrent(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->FusesF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Fuses_Get_State(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->FusesV, 1, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL const char* ctx_Fuses_Get_SwitchedObj(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->FusesS(4, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Fuses_Get_SwitchedTerm(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->FusesI(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Fuses_Get_TCCcurve(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->FusesS(6, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Fuses_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->FusesI(10, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_Fuses_IsBlown(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->FusesI(9, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Fuses_Open(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->FusesI(7, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Fuses_Reset(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->FusesI(13, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Fuses_Set_Delay(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->FusesF(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Fuses_Set_MonitoredObj(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->FusesS(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Fuses_Set_MonitoredTerm(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->FusesI(4, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Fuses_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->FusesS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Fuses_Set_RatedCurrent(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->FusesF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Fuses_Set_SwitchedObj(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->FusesS(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Fuses_Set_SwitchedTerm(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->FusesI(6, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Fuses_Set_TCCcurve(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->FusesS(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Fuses_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->FusesI(11, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_GICSources_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->GICSourcesV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL const char* ctx_GICSources_Get_Bus1(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->GICSourcesS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_GICSources_Get_Bus2(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->GICSourcesS(1, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_GICSources_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->GICSourcesI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_GICSources_Get_EE(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->GICSourcesF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_GICSources_Get_EN(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->GICSourcesF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_GICSources_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->GICSourcesI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_GICSources_Get_Lat1(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->GICSourcesF(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_GICSources_Get_Lat2(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->GICSourcesF(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_GICSources_Get_Lon1(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->GICSourcesF(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_GICSources_Get_Lon2(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->GICSourcesF(10, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_GICSources_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->GICSourcesS(2, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_GICSources_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->GICSourcesI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_GICSources_Get_Phases(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->GICSourcesI(3, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_GICSources_Get_Volts(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->GICSourcesF(12, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_GICSources_Set_EE(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GICSourcesF(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_GICSources_Set_EN(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GICSourcesF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_GICSources_Set_Lat1(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GICSourcesF(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_GICSources_Set_Lat2(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GICSourcesF(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_GICSources_Set_Lon1(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GICSourcesF(9, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_GICSources_Set_Lon2(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GICSourcesF(11, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_GICSources_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GICSourcesS(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_GICSources_Set_Phases(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GICSourcesI(4, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_GICSources_Set_Volts(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GICSourcesF(13, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Generators_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->GeneratorsV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_Generators_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->GeneratorsI(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Generators_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->GeneratorsI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_Generators_Get_ForcedON(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->GeneratorsI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Generators_Get_Model(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->GeneratorsI(9, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Generators_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->GeneratorsS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Generators_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->GeneratorsI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Generators_Get_PF(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->GeneratorsF(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Generators_Get_Phases(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->GeneratorsI(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Generators_Get_RegisterNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->GeneratorsV, 1, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Generators_Get_RegisterValues(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->GeneratorsV, 2, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Generators_Get_RegisterValues_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->GeneratorsV, 2, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL double ctx_Generators_Get_Vmaxpu(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->GeneratorsF(10, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Generators_Get_Vminpu(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->GeneratorsF(12, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Generators_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->GeneratorsI(7, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Generators_Get_kV(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->GeneratorsF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Generators_Get_kVArated(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->GeneratorsF(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Generators_Get_kW(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->GeneratorsF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Generators_Get_kvar(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->GeneratorsF(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Generators_Set_ForcedON(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GeneratorsI(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Generators_Set_Model(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GeneratorsI(10, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Generators_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GeneratorsS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Generators_Set_PF(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GeneratorsF(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Generators_Set_Phases(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GeneratorsI(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Generators_Set_Vmaxpu(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GeneratorsF(11, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Generators_Set_Vminpu(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GeneratorsF(13, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Generators_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GeneratorsI(8, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Generators_Set_kV(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GeneratorsF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Generators_Set_kVArated(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GeneratorsF(9, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Generators_Set_kW(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GeneratorsF(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Generators_Set_kvar(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->GeneratorsF(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_ISources_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->IsourceV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL double ctx_ISources_Get_Amps(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->IsourceF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_ISources_Get_AngleDeg(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->IsourceF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_ISources_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->IsourceI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_ISources_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->IsourceI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_ISources_Get_Frequency(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->IsourceF(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_ISources_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->IsourceS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_ISources_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->IsourceI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_ISources_Set_Amps(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->IsourceF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_ISources_Set_AngleDeg(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->IsourceF(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_ISources_Set_Frequency(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->IsourceF(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_ISources_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->IsourceS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->LineCodesV, 6, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL double ctx_LineCodes_Get_C0(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LineCodesF(10, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_LineCodes_Get_C1(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LineCodesF(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Get_Cmatrix(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->LineCodesV, 4, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Get_Cmatrix_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->LineCodesV, 4, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_LineCodes_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->LineCodesI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_LineCodes_Get_EmergAmps(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LineCodesF(14, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_LineCodes_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->LineCodesI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_LineCodes_Get_IsZ1Z0(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->LineCodesI(7, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_LineCodes_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->LineCodesS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_LineCodes_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->LineCodesI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_LineCodes_Get_NormAmps(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LineCodesF(12, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_LineCodes_Get_Phases(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->LineCodesI(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_LineCodes_Get_R0(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LineCodesF(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_LineCodes_Get_R1(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LineCodesF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Get_Rmatrix(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->LineCodesV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Get_Rmatrix_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->LineCodesV, 0, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_LineCodes_Get_Units(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->LineCodesI(3, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_LineCodes_Get_X0(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LineCodesF(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_LineCodes_Get_X1(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LineCodesF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Get_Xmatrix(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->LineCodesV, 2, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Get_Xmatrix_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->LineCodesV, 2, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Set_C0(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LineCodesF(11, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Set_C1(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LineCodesF(9, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Set_Cmatrix(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    ((OddieContext*) ctx)->LineCodesV(5, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Set_EmergAmps(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LineCodesF(15, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LineCodesS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Set_NormAmps(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LineCodesF(13, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Set_Phases(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LineCodesI(6, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Set_R0(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LineCodesF(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Set_R1(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LineCodesF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Set_Rmatrix(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    ((OddieContext*) ctx)->LineCodesV(1, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Set_Units(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LineCodesI(4, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Set_X0(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LineCodesF(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Set_X1(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LineCodesF(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Set_Xmatrix(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    ((OddieContext*) ctx)->LineCodesV(3, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->LinesV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL const char* ctx_Lines_Get_Bus1(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->LinesS(2, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Lines_Get_Bus2(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->LinesS(4, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Lines_Get_C0(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LinesF(12, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Lines_Get_C1(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LinesF(10, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Lines_Get_Cmatrix(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->LinesV, 5, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Get_Cmatrix_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->LinesV, 5, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_Lines_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->LinesI(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Lines_Get_EmergAmps(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LinesF(16, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Lines_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->LinesI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Lines_Get_Geometry(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->LinesS(8, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Lines_Get_Length(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LinesF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Lines_Get_LineCode(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->LinesS(6, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Lines_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->LinesS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Lines_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->LinesI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Lines_Get_NormAmps(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LinesF(14, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Lines_Get_NumCust(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->LinesI(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Lines_Get_Parent(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->LinesI(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Lines_Get_Phases(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->LinesI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Lines_Get_R0(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LinesF(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Lines_Get_R1(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LinesF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Lines_Get_Rg(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LinesF(18, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Lines_Get_Rho(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LinesF(22, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Lines_Get_Rmatrix(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->LinesV, 1, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Get_Rmatrix_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->LinesV, 1, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL double ctx_Lines_Get_SeasonRating(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LinesF(24, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Lines_Get_Spacing(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->LinesS(10, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Lines_Get_Units(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->LinesI(7, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Lines_Get_X0(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LinesF(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Lines_Get_X1(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LinesF(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Lines_Get_Xg(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LinesF(20, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Lines_Get_Xmatrix(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->LinesV, 3, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Get_Xmatrix_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->LinesV, 3, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Get_Yprim(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->LinesV, 7, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Get_Yprim_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->LinesV, 7, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_Bus1(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LinesS(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_Bus2(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LinesS(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_C0(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LinesF(13, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_C1(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LinesF(11, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_Cmatrix(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    ((OddieContext*) ctx)->LinesV(6, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_EmergAmps(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LinesF(17, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_Geometry(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LinesS(9, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_Length(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LinesF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_LineCode(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LinesS(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LinesS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_NormAmps(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LinesF(15, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_Phases(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LinesI(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_R0(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LinesF(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_R1(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LinesF(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_Rg(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LinesF(19, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_Rho(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LinesF(23, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_Rmatrix(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    ((OddieContext*) ctx)->LinesV(2, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_Spacing(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LinesS(11, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_Units(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LinesI(8, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_X0(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LinesF(9, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_X1(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LinesF(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_Xg(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LinesF(21, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_Xmatrix(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    ((OddieContext*) ctx)->LinesV(4, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_Yprim(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    ((OddieContext*) ctx)->LinesV(8, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->LoadShapeV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_LoadShapes_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->LoadShapeI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_LoadShapes_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->LoadShapeI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_LoadShapes_Get_HrInterval(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LoadShapeF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_LoadShapes_Get_MinInterval(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LoadShapeF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_LoadShapes_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->LoadShapeS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_LoadShapes_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->LoadShapeI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_LoadShapes_Get_Npts(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->LoadShapeI(3, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_LoadShapes_Get_PBase(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LoadShapeF(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Get_Pmult(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->LoadShapeV, 1, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Get_Pmult_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->LoadShapeV, 1, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL double ctx_LoadShapes_Get_Qbase(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LoadShapeF(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Get_Qmult(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->LoadShapeV, 3, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Get_Qmult_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->LoadShapeV, 3, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL double ctx_LoadShapes_Get_SInterval(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->LoadShapeF(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Get_TimeArray(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->LoadShapeV, 5, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Get_TimeArray_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->LoadShapeV, 5, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL uint16_t ctx_LoadShapes_Get_UseActual(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->LoadShapeI(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Normalize(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LoadShapeI(5, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Set_HrInterval(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LoadShapeF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Set_MinInterval(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LoadShapeF(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LoadShapeS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Set_Npts(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LoadShapeI(4, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Set_PBase(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LoadShapeF(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Set_Pmult(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    ((OddieContext*) ctx)->LoadShapeV(2, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Set_Qbase(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LoadShapeF(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Set_Qmult(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    ((OddieContext*) ctx)->LoadShapeV(4, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Set_SInterval(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LoadShapeF(9, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Set_TimeArray(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    ((OddieContext*) ctx)->LoadShapeV(6, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Set_UseActual(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->LoadShapeI(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->DSSLoadsV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_AllocationFactor(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(12, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Loads_Get_CVRcurve(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->DSSLoadsS(2, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_CVRvars(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(18, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_CVRwatts(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(16, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_Cfactor(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(14, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Loads_Get_Class_(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->DSSLoads(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Loads_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->DSSLoads(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Loads_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->DSSLoads(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Loads_Get_Growth(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->DSSLoadsS(12, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_Loads_Get_IsDelta(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->DSSLoads(13, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Loads_Get_Model(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->DSSLoads(7, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Loads_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->DSSLoadsS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Loads_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->DSSLoads(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Loads_Get_NumCust(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->DSSLoads(9, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_PF(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_PctMean(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_PctStdDev(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(10, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_RelWeight(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(42, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_Rneut(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(26, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Loads_Get_Sensor(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->DSSLoadsS(14, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Loads_Get_Spectrum(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->DSSLoadsS(8, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Loads_Get_Status(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->DSSLoads(11, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_Vmaxpu(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(28, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_Vminemerg(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(30, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_Vminnorm(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(32, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_Vminpu(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(34, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_Xneut(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(38, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Loads_Get_Yearly(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->DSSLoadsS(10, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Loads_Get_ZIPV(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->DSSLoadsV, 1, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Get_ZIPV_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->DSSLoadsV, 1, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL const char* ctx_Loads_Get_daily(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->DSSLoadsS(4, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Loads_Get_duty(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->DSSLoadsS(6, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Loads_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->DSSLoads(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_kV(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_kW(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_kva(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(20, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_kvar(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_kwh(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(22, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_kwhdays(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(24, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_pctSeriesRL(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(40, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Loads_Get_xfkVA(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->DSSLoadsF(36, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_AllocationFactor(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(13, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_CVRcurve(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsS(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_CVRvars(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(19, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_CVRwatts(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(17, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_Cfactor(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(15, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_Class_(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoads(6, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_Growth(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsS(13, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_IsDelta(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoads(14, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_Model(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoads(8, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_NumCust(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoads(10, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_PF(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_PctMean(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(9, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_PctStdDev(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(11, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_RelWeight(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(43, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_Rneut(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(27, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_Spectrum(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsS(9, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_Status(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoads(12, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_Vmaxpu(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(29, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_Vminemerg(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(31, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_Vminnorm(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(33, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_Vminpu(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(35, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_Xneut(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(39, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_Yearly(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsS(11, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_ZIPV(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    ((OddieContext*) ctx)->DSSLoadsV(2, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_daily(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsS(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_duty(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsS(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoads(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_kV(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_kW(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_kva(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(21, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_kvar(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_kwh(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(23, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_kwhdays(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(25, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_pctSeriesRL(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(41, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Loads_Set_xfkVA(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->DSSLoadsF(37, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Meters_CloseAllDIFiles(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MetersI(12, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Meters_DoReliabilityCalc(const void* ctx, uint16_t AssumeRestoration)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MetersI(18, AssumeRestoration);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Get_AllBranchesInZone(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->MetersV, 11, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Get_AllEndElements(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->MetersV, 10, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->MetersV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Get_AllocFactors(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->MetersV, 8, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Get_AllocFactors_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->MetersV, 8, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL double ctx_Meters_Get_AvgRepairTime(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->MetersF(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Meters_Get_CalcCurrent(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->MetersV, 6, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Get_CalcCurrent_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->MetersV, 6, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_Meters_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MetersI(14, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Meters_Get_CountBranches(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MetersI(15, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Meters_Get_CountEndElements(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MetersI(13, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Meters_Get_CustInterrupts(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->MetersF(3, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_Meters_Get_DIFilesAreOpen(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->MetersI(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Meters_Get_FaultRateXRepairHrs(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->MetersF(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Meters_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MetersI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Meters_Get_MeteredElement(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->MetersS(2, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Meters_Get_MeteredTerminal(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MetersI(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Meters_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->MetersS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Meters_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MetersI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Meters_Get_NumSectionBranches(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MetersI(25, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Meters_Get_NumSectionCustomers(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MetersI(24, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Meters_Get_NumSections(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MetersI(21, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Meters_Get_OCPDeviceType(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MetersI(23, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Meters_Get_Peakcurrent(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->MetersV, 4, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Get_Peakcurrent_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->MetersV, 4, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Get_RegisterNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->MetersV, 1, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Get_RegisterValues(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->MetersV, 2, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Get_RegisterValues_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->MetersV, 2, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL double ctx_Meters_Get_SAIDI(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->MetersF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Meters_Get_SAIFI(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->MetersF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Meters_Get_SAIFIKW(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->MetersF(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Meters_Get_SectSeqIdx(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MetersI(26, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Meters_Get_SectTotalCust(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MetersI(27, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Meters_Get_SeqListSize(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MetersI(19, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Meters_Get_SequenceIndex(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MetersI(16, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Meters_Get_SumBranchFltRates(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->MetersF(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Meters_Get_TotalCustomers(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MetersI(20, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Meters_Get_Totals(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->MetersV, 3, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Get_Totals_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->MetersV, 3, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Get_ZonePCE(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->MetersV, 12, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Meters_OpenAllDIFiles(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MetersI(11, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Reset(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MetersI(2, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Meters_ResetAll(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MetersI(3, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Sample(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MetersI(4, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Meters_SampleAll(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MetersI(9, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Save(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MetersI(5, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Meters_SaveAll(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MetersI(10, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Meters_SetActiveSection(const void* ctx, int32_t SectIdx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MetersI(22, SectIdx);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Set_AllocFactors(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    ((OddieContext*) ctx)->MetersV(9, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Set_CalcCurrent(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    ((OddieContext*) ctx)->MetersV(7, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Set_MeteredElement(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MetersS(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Set_MeteredTerminal(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MetersI(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MetersS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Set_Peakcurrent(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    ((OddieContext*) ctx)->MetersV(5, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Meters_Set_SequenceIndex(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MetersI(17, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->MonitorsV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_Get_ByteStream(const void* ctx, int8_t** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_int8_func((OddieContext*) ctx, ((OddieContext*) ctx)->MonitorsV, 1, ResultPtr, ResultDims);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_Get_ByteStream_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_vararray_int8_func((OddieContext*) ctx, ((OddieContext*) ctx)->MonitorsV, 1, &((OddieContext*) ctx)->GR_DataPtr_PByte, &((OddieContext*) ctx)->GR_Counts_PByte[0]);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_Get_Channel(const void* ctx, double** ResultPtr, int32_t* ResultDims, int32_t Index)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->MonitorsV, 5, ResultPtr, ResultDims, (double*) &Index);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_Get_Channel_GR(const void* ctx, int32_t Index)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->MonitorsV, 5, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], (double*) &Index);
}

ALTDSS_ODDIE_DLL int32_t ctx_Monitors_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MonitorsI(12, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Monitors_Get_Element(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->MonitorsS(3, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Monitors_Get_FileName(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->MonitorsS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Monitors_Get_FileVersion(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MonitorsI(15, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Monitors_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MonitorsI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Monitors_Get_Header(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->MonitorsV, 2, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_Monitors_Get_Mode(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MonitorsI(7, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Monitors_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->MonitorsS(1, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Monitors_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MonitorsI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Monitors_Get_NumChannels(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MonitorsI(17, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Monitors_Get_RecordSize(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MonitorsI(16, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Monitors_Get_SampleCount(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MonitorsI(9, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Monitors_Get_Terminal(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->MonitorsI(18, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Monitors_Get_dblFreq(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->MonitorsV, 4, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_Get_dblFreq_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->MonitorsV, 4, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_Get_dblHour(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->MonitorsV, 3, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_Get_dblHour_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->MonitorsV, 3, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_Process(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MonitorsI(13, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_ProcessAll(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MonitorsI(14, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_Reset(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MonitorsI(2, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_ResetAll(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MonitorsI(3, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_Sample(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MonitorsI(4, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_SampleAll(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MonitorsI(10, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_Save(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MonitorsI(5, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_SaveAll(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MonitorsI(11, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_Set_Element(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MonitorsS(4, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_Set_Mode(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MonitorsI(8, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MonitorsS(2, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_Set_Terminal(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MonitorsI(19, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Monitors_Show(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->MonitorsI(6, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL double ctx_PDElements_Get_AccumulatedL(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->PDElementsF(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_PDElements_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->PDElementsI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_PDElements_Get_FaultRate(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->PDElementsF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_PDElements_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->PDElementsI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_PDElements_Get_FromTerminal(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->PDElementsI(7, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_PDElements_Get_IsShunt(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->PDElementsI(3, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_PDElements_Get_Lambda(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->PDElementsF(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_PDElements_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->PDElementsS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_PDElements_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->PDElementsI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_PDElements_Get_Numcustomers(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->PDElementsI(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_PDElements_Get_ParentPDElement(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->PDElementsI(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_PDElements_Get_RepairTime(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->PDElementsF(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_PDElements_Get_SectionID(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->PDElementsI(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_PDElements_Get_TotalMiles(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->PDElementsF(7, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_PDElements_Get_Totalcustomers(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->PDElementsI(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_PDElements_Get_pctPermanent(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->PDElementsF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Set_FaultRate(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->PDElementsF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->PDElementsS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Set_pctPermanent(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->PDElementsF(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_PVSystems_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->PVsystemsV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_PVSystems_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->PVsystemsI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_PVSystems_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->PVsystemsI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_PVSystems_Get_Irradiance(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->PVsystemsF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_PVSystems_Get_IrradianceNow(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->PVsystemsF(11, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_PVSystems_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->PVsystemsS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_PVSystems_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->PVsystemsI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_PVSystems_Get_PF(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->PVsystemsF(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_PVSystems_Get_Pmpp(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->PVsystemsF(9, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_PVSystems_Get_Sensor(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->PVsystemsS(2, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_PVSystems_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->PVsystemsI(3, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_PVSystems_Get_kVArated(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->PVsystemsF(7, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_PVSystems_Get_kW(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->PVsystemsF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_PVSystems_Get_kvar(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->PVsystemsF(3, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_PVSystems_Set_Irradiance(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->PVsystemsF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_PVSystems_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->PVsystemsS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_PVSystems_Set_PF(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->PVsystemsF(6, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_PVSystems_Set_Pmpp(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->PVsystemsF(10, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_PVSystems_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->PVsystemsI(4, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_PVSystems_Set_kVArated(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->PVsystemsF(8, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_PVSystems_Set_kvar(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->PVsystemsF(4, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Parallel_CreateActor(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ParallelI(4, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL int32_t ctx_Parallel_Get_ActiveActor(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->ParallelI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Parallel_Get_ActiveParallel(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->ParallelI(9, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Parallel_Get_ActorCPU(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->ParallelI(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Parallel_Get_ActorProgress(const void* ctx, int32_t** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_int32_func((OddieContext*) ctx, ((OddieContext*) ctx)->ParallelV, 0, ResultPtr, ResultDims);
}

ALTDSS_ODDIE_DLL void ctx_Parallel_Get_ActorProgress_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_vararray_int32_func((OddieContext*) ctx, ((OddieContext*) ctx)->ParallelV, 0, &((OddieContext*) ctx)->GR_DataPtr_PInteger, &((OddieContext*) ctx)->GR_Counts_PInteger[0]);
}

ALTDSS_ODDIE_DLL void ctx_Parallel_Get_ActorStatus(const void* ctx, int32_t** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_int32_func((OddieContext*) ctx, ((OddieContext*) ctx)->ParallelV, 1, ResultPtr, ResultDims);
}

ALTDSS_ODDIE_DLL void ctx_Parallel_Get_ActorStatus_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_vararray_int32_func((OddieContext*) ctx, ((OddieContext*) ctx)->ParallelV, 1, &((OddieContext*) ctx)->GR_DataPtr_PInteger, &((OddieContext*) ctx)->GR_Counts_PInteger[0]);
}

ALTDSS_ODDIE_DLL int32_t ctx_Parallel_Get_ConcatenateReports(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->ParallelI(11, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Parallel_Get_NumCPUs(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->ParallelI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Parallel_Get_NumCores(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->ParallelI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Parallel_Get_NumOfActors(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->ParallelI(7, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Parallel_Set_ActiveActor(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ParallelI(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Parallel_Set_ActiveParallel(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ParallelI(10, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Parallel_Set_ActorCPU(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ParallelI(6, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Parallel_Set_ConcatenateReports(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ParallelI(12, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Parallel_Wait(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ParallelI(8, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL uint16_t ctx_Parser_Get_AutoIncrement(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->ParserI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Parser_Get_BeginQuote(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->ParserS(6, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Parser_Get_CmdString(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->ParserS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Parser_Get_DblValue(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->ParserF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Parser_Get_Delimiters(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->ParserS(10, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Parser_Get_EndQuote(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->ParserS(8, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Parser_Get_IntValue(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->ParserI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Parser_Get_Matrix(const void* ctx, double** ResultPtr, int32_t* ResultDims, int32_t ExpectedOrder)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->ParserV, 1, ResultPtr, ResultDims, (double*) &ExpectedOrder);
}

ALTDSS_ODDIE_DLL void ctx_Parser_Get_Matrix_GR(const void* ctx, int32_t ExpectedOrder)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->ParserV, 1, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], (double*) &ExpectedOrder);
}

ALTDSS_ODDIE_DLL const char* ctx_Parser_Get_NextParam(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->ParserS(2, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Parser_Get_StrValue(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->ParserS(3, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Parser_Get_SymMatrix(const void* ctx, double** ResultPtr, int32_t* ResultDims, int32_t ExpectedOrder)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->ParserV, 2, ResultPtr, ResultDims, (double*) &ExpectedOrder);
}

ALTDSS_ODDIE_DLL void ctx_Parser_Get_SymMatrix_GR(const void* ctx, int32_t ExpectedOrder)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->ParserV, 2, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], (double*) &ExpectedOrder);
}

ALTDSS_ODDIE_DLL void ctx_Parser_Get_Vector(const void* ctx, double** ResultPtr, int32_t* ResultDims, int32_t ExpectedSize)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->ParserV, 0, ResultPtr, ResultDims, (double*) &ExpectedSize);
}

ALTDSS_ODDIE_DLL void ctx_Parser_Get_Vector_GR(const void* ctx, int32_t ExpectedSize)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->ParserV, 0, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], (double*) &ExpectedSize);
}

ALTDSS_ODDIE_DLL const char* ctx_Parser_Get_WhiteSpace(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->ParserS(4, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Parser_ResetDelimiters(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ParserI(1, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Parser_Set_AutoIncrement(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ParserI(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Parser_Set_BeginQuote(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ParserS(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Parser_Set_CmdString(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ParserS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Parser_Set_Delimiters(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ParserS(11, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Parser_Set_EndQuote(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ParserS(9, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Parser_Set_WhiteSpace(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ParserS(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Reclosers_Close(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReclosersI(12, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Reclosers_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->ReclosersV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_Reclosers_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->ReclosersI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Reclosers_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->ReclosersI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Reclosers_Get_GroundInst(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->ReclosersF(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Reclosers_Get_GroundTrip(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->ReclosersF(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Reclosers_Get_MonitoredObj(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->ReclosersS(2, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Reclosers_Get_MonitoredTerm(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->ReclosersI(3, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Reclosers_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->ReclosersS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Reclosers_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->ReclosersI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Reclosers_Get_NumFast(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->ReclosersI(7, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Reclosers_Get_PhaseInst(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->ReclosersF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Reclosers_Get_PhaseTrip(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->ReclosersF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Reclosers_Get_RecloseIntervals(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->ReclosersV, 1, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Reclosers_Get_RecloseIntervals_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->ReclosersV, 1, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_Reclosers_Get_Shots(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->ReclosersI(9, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Reclosers_Get_SwitchedObj(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->ReclosersS(4, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Reclosers_Get_SwitchedTerm(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->ReclosersI(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Reclosers_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->ReclosersI(13, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Reclosers_Open(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReclosersI(11, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Reclosers_Reset(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReclosersI(15, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Reclosers_Set_GroundInst(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReclosersF(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Reclosers_Set_GroundTrip(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReclosersF(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Reclosers_Set_MonitoredObj(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReclosersS(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Reclosers_Set_MonitoredTerm(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReclosersI(4, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Reclosers_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReclosersS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Reclosers_Set_NumFast(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReclosersI(8, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Reclosers_Set_PhaseInst(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReclosersF(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Reclosers_Set_PhaseTrip(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReclosersF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Reclosers_Set_Shots(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReclosersI(10, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Reclosers_Set_SwitchedObj(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReclosersS(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Reclosers_Set_SwitchedTerm(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReclosersI(6, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Reclosers_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReclosersI(14, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_ReduceCkt_Do1phLaterals(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReduceCktI(0, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_ReduceCkt_DoBranchRemove(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReduceCktI(1, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_ReduceCkt_DoDangling(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReduceCktI(2, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_ReduceCkt_DoDefault(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReduceCktI(3, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_ReduceCkt_DoLoopBreak(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReduceCktI(4, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_ReduceCkt_DoParallelLines(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReduceCktI(5, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_ReduceCkt_DoShortLines(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReduceCktI(6, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_ReduceCkt_DoSwitches(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReduceCktI(7, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL const char* ctx_ReduceCkt_Get_EditString(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->ReduceCktS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_ReduceCkt_Get_EnergyMeter(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->ReduceCktS(2, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_ReduceCkt_Get_KeepLoad(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->ReduceCktI(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_ReduceCkt_Get_StartPDElement(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->ReduceCktS(5, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_ReduceCkt_Get_Zmag(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->ReduceCktF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_ReduceCkt_SaveCircuit(const void* ctx, const char* CktName)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReduceCktS(4, CktName);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_ReduceCkt_Set_EditString(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReduceCktS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_ReduceCkt_Set_EnergyMeter(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReduceCktS(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_ReduceCkt_Set_KeepLoad(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReduceCktI(9, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_ReduceCkt_Set_StartPDElement(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReduceCktS(6, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_ReduceCkt_Set_Zmag(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->ReduceCktF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->RegControlsV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL double ctx_RegControls_Get_CTPrimary(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->RegControlsF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_RegControls_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->RegControlsI(12, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_RegControls_Get_Delay(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->RegControlsF(12, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_RegControls_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->RegControlsI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_RegControls_Get_ForwardBand(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->RegControlsF(18, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_RegControls_Get_ForwardR(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->RegControlsF(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_RegControls_Get_ForwardVreg(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->RegControlsF(20, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_RegControls_Get_ForwardX(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->RegControlsF(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_RegControls_Get_IsInverseTime(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->RegControlsI(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_RegControls_Get_IsReversible(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->RegControlsI(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_RegControls_Get_MaxTapChange(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->RegControlsI(10, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_RegControls_Get_MonitoredBus(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->RegControlsS(2, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_RegControls_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->RegControlsS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_RegControls_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->RegControlsI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_RegControls_Get_PTratio(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->RegControlsF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_RegControls_Get_ReverseBand(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->RegControlsF(22, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_RegControls_Get_ReverseR(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->RegControlsF(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_RegControls_Get_ReverseVreg(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->RegControlsF(24, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_RegControls_Get_ReverseX(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->RegControlsF(10, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_RegControls_Get_TapDelay(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->RegControlsF(14, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_RegControls_Get_TapNumber(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->RegControlsI(13, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_RegControls_Get_TapWinding(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->RegControlsI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_RegControls_Get_Transformer(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->RegControlsS(4, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_RegControls_Get_VoltageLimit(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->RegControlsF(16, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_RegControls_Get_Winding(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->RegControlsI(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_CTPrimary(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_Delay(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsF(13, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_ForwardBand(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsF(19, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_ForwardR(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsF(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_ForwardVreg(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsF(21, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_ForwardX(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsF(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_IsInverseTime(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsI(9, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_IsReversible(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsI(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_MaxTapChange(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsI(11, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_MonitoredBus(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsS(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_PTratio(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsF(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_ReverseBand(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsF(23, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_ReverseR(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsF(9, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_ReverseVreg(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsF(25, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_ReverseX(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsF(11, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_TapDelay(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsF(15, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_TapNumber(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsI(14, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_TapWinding(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsI(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_Transformer(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsS(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_VoltageLimit(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsF(17, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_Winding(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RegControlsI(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Relays_Close(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RelaysI(10, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Relays_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->RelaysV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_Relays_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->RelaysI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Relays_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->RelaysI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Relays_Get_MonitoredObj(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->RelaysS(2, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Relays_Get_MonitoredTerm(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->RelaysI(3, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Relays_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->RelaysS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Relays_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->RelaysI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Relays_Get_SwitchedObj(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->RelaysS(4, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Relays_Get_SwitchedTerm(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->RelaysI(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Relays_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->RelaysI(7, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Relays_Open(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RelaysI(9, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Relays_Reset(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RelaysI(11, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Relays_Set_MonitoredObj(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RelaysS(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Relays_Set_MonitoredTerm(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RelaysI(4, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Relays_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RelaysS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Relays_Set_SwitchedObj(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RelaysS(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Relays_Set_SwitchedTerm(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RelaysI(6, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Relays_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->RelaysI(8, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->SensorsV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Get_AllocationFactor(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->SensorsV, 7, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Get_AllocationFactor_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->SensorsV, 7, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_Sensors_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SensorsI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Get_Currents(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->SensorsV, 1, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Get_Currents_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->SensorsV, 1, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_Sensors_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SensorsI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_Sensors_Get_IsDelta(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->SensorsI(3, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Sensors_Get_MeteredElement(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->SensorsS(2, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Sensors_Get_MeteredTerminal(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SensorsI(7, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Sensors_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->SensorsS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Sensors_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SensorsI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Sensors_Get_PctError(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SensorsF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_Sensors_Get_ReverseDelta(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->SensorsI(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Sensors_Get_Weight(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SensorsF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Get_kVARS(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->SensorsV, 3, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Get_kVARS_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->SensorsV, 3, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL double ctx_Sensors_Get_kVbase(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SensorsF(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Get_kWS(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->SensorsV, 5, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Get_kWS_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->SensorsV, 5, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Reset(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SensorsI(9, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Sensors_ResetAll(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SensorsI(10, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Set_Currents(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    ((OddieContext*) ctx)->SensorsV(2, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Set_IsDelta(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SensorsI(4, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Set_MeteredElement(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SensorsS(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Set_MeteredTerminal(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SensorsI(8, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SensorsS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Set_PctError(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SensorsF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Set_ReverseDelta(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SensorsI(6, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Set_Weight(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SensorsF(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Set_kVARS(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    ((OddieContext*) ctx)->SensorsV(4, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Set_kVbase(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SensorsF(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Set_kWS(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    ((OddieContext*) ctx)->SensorsV(6, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL uint16_t ctx_Settings_Get_AllowDuplicates(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->SettingsI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Settings_Get_AutoBusList(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->SettingsS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Settings_Get_CktModel(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SettingsI(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Settings_Get_EmergVmaxpu(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SettingsF(7, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Settings_Get_EmergVminpu(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SettingsF(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Settings_Get_LossRegs(const void* ctx, int32_t** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_int32_func((OddieContext*) ctx, ((OddieContext*) ctx)->SettingsV, 2, ResultPtr, ResultDims);
}

ALTDSS_ODDIE_DLL void ctx_Settings_Get_LossRegs_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_vararray_int32_func((OddieContext*) ctx, ((OddieContext*) ctx)->SettingsV, 2, &((OddieContext*) ctx)->GR_DataPtr_PInteger, &((OddieContext*) ctx)->GR_Counts_PInteger[0]);
}

ALTDSS_ODDIE_DLL double ctx_Settings_Get_LossWeight(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SettingsF(11, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Settings_Get_NormVmaxpu(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SettingsF(3, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Settings_Get_NormVminpu(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SettingsF(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Settings_Get_PriceCurve(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->SettingsS(2, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Settings_Get_PriceSignal(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SettingsF(13, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_Settings_Get_Trapezoidal(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->SettingsI(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Settings_Get_UEregs(const void* ctx, int32_t** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_int32_func((OddieContext*) ctx, ((OddieContext*) ctx)->SettingsV, 0, ResultPtr, ResultDims);
}

ALTDSS_ODDIE_DLL void ctx_Settings_Get_UEregs_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_vararray_int32_func((OddieContext*) ctx, ((OddieContext*) ctx)->SettingsV, 0, &((OddieContext*) ctx)->GR_DataPtr_PInteger, &((OddieContext*) ctx)->GR_Counts_PInteger[0]);
}

ALTDSS_ODDIE_DLL double ctx_Settings_Get_UEweight(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SettingsF(9, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Settings_Get_VoltageBases(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->SettingsV, 4, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Settings_Get_VoltageBases_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->SettingsV, 4, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL uint16_t ctx_Settings_Get_ZoneLock(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->SettingsI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Settings_Set_AllocationFactors(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SettingsF(0, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Settings_Set_AllowDuplicates(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SettingsI(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Settings_Set_AutoBusList(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SettingsS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Settings_Set_CktModel(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SettingsI(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Settings_Set_EmergVmaxpu(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SettingsF(8, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Settings_Set_EmergVminpu(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SettingsF(6, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Settings_Set_LossRegs(const void* ctx, const int32_t* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_INTEGER;
    ((OddieContext*) ctx)->SettingsV(3, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Settings_Set_LossWeight(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SettingsF(12, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Settings_Set_NormVmaxpu(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SettingsF(4, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Settings_Set_NormVminpu(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SettingsF(2, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Settings_Set_PriceCurve(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SettingsS(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Settings_Set_PriceSignal(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SettingsF(14, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Settings_Set_Trapezoidal(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SettingsI(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Settings_Set_UEregs(const void* ctx, const int32_t* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_INTEGER;
    ((OddieContext*) ctx)->SettingsV(1, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Settings_Set_UEweight(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SettingsF(10, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Settings_Set_VoltageBases(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    ((OddieContext*) ctx)->SettingsV(5, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Settings_Set_ZoneLock(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SettingsI(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_CheckControls(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(33, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_CheckFaultStatus(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(27, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Cleanup(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(45, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_DoControlActions(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(35, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_FinishTimeStep(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(44, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL int32_t ctx_Solution_Get_AddType(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SolutionI(16, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Solution_Get_Algorithm(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SolutionI(18, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Solution_Get_BusLevels(const void* ctx, int32_t** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_int32_func((OddieContext*) ctx, ((OddieContext*) ctx)->SolutionV, 2, ResultPtr, ResultDims);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Get_BusLevels_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_vararray_int32_func((OddieContext*) ctx, ((OddieContext*) ctx)->SolutionV, 2, &((OddieContext*) ctx)->GR_DataPtr_PInteger, &((OddieContext*) ctx)->GR_Counts_PInteger[0]);
}

ALTDSS_ODDIE_DLL double ctx_Solution_Get_Capkvar(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SolutionF(16, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_Solution_Get_ControlActionsDone(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->SolutionI(42, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Solution_Get_ControlIterations(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SolutionI(22, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Solution_Get_ControlMode(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SolutionI(20, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_Solution_Get_Converged(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->SolutionI(38, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Solution_Get_DefaultDaily(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->SolutionS(3, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Solution_Get_DefaultYearly(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->SolutionS(5, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Solution_Get_EventLog(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->SolutionV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL double ctx_Solution_Get_Frequency(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SolutionF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Solution_Get_GenMult(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SolutionF(18, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Solution_Get_GenPF(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SolutionF(14, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Solution_Get_GenkW(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SolutionF(12, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Solution_Get_Hour(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SolutionI(3, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Solution_Get_IncMatrix(const void* ctx, int32_t** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_int32_func((OddieContext*) ctx, ((OddieContext*) ctx)->SolutionV, 1, ResultPtr, ResultDims);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Get_IncMatrix_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_vararray_int32_func((OddieContext*) ctx, ((OddieContext*) ctx)->SolutionV, 1, &((OddieContext*) ctx)->GR_DataPtr_PInteger, &((OddieContext*) ctx)->GR_Counts_PInteger[0]);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Get_IncMatrixCols(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->SolutionV, 4, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Get_IncMatrixRows(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->SolutionV, 3, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_Solution_Get_Iterations(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SolutionI(7, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Solution_Get_LDCurve(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->SolutionS(1, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Solution_Get_Laplacian(const void* ctx, int32_t** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_int32_func((OddieContext*) ctx, ((OddieContext*) ctx)->SolutionV, 5, ResultPtr, ResultDims);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Get_Laplacian_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_vararray_int32_func((OddieContext*) ctx, ((OddieContext*) ctx)->SolutionV, 5, &((OddieContext*) ctx)->GR_DataPtr_PInteger, &((OddieContext*) ctx)->GR_Counts_PInteger[0]);
}

ALTDSS_ODDIE_DLL int32_t ctx_Solution_Get_LoadModel(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SolutionI(14, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Solution_Get_LoadMult(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SolutionF(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Solution_Get_MaxControlIterations(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SolutionI(24, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Solution_Get_MaxIterations(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SolutionI(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Solution_Get_Mode(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SolutionI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Solution_Get_ModeID(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->SolutionS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Solution_Get_MostIterationsDone(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SolutionI(41, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Solution_Get_Number(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SolutionI(10, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Solution_Get_Process_Time(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SolutionF(24, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Solution_Get_Random(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SolutionI(12, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Solution_Get_Seconds(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SolutionF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Solution_Get_StepSize(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SolutionF(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_Solution_Get_SystemYChanged(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->SolutionI(37, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Solution_Get_Tolerance(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SolutionF(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Solution_Get_Total_Time(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SolutionF(25, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Solution_Get_Totaliterations(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SolutionI(40, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Solution_Get_Year(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SolutionI(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Solution_Get_dblHour(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SolutionF(20, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Solution_Get_pctGrowth(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SolutionF(10, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Solution_InitSnap(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(32, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_SampleControlDevices(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(34, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Sample_DoControlActions(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(26, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_AddType(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(17, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_Algorithm(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(19, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_Capkvar(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionF(17, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_ControlActionsDone(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(43, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_ControlIterations(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(23, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_ControlMode(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(21, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_Converged(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(39, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_DefaultDaily(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionS(4, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_DefaultYearly(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionS(6, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_Frequency(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_GenMult(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionF(19, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_GenPF(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionF(15, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_GenkW(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionF(13, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_Hour(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(4, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_LDCurve(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionS(2, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_LoadModel(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(15, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_LoadMult(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionF(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_MaxControlIterations(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(25, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_MaxIterations(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(9, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_Mode(const void* ctx, int32_t Mode)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(2, Mode);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_Number(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(11, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_Random(const void* ctx, int32_t Random)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(13, Random);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_Seconds(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionF(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_StepSize(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionF(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_StepsizeHr(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionF(23, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_StepsizeMin(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionF(22, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_Tolerance(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionF(9, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_Total_Time(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionF(26, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_Year(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(6, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_dblHour(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionF(21, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_pctGrowth(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionF(11, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_Solve(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(0, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_SolveAll(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(46, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_SolveDirect(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(28, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_SolveNoControl(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(30, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_SolvePflow(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(29, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Solution_SolvePlusControl(const void* ctx)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SolutionI(31, 0);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL int32_t ctx_SwtControls_Get_Action(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SwtControlsI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_SwtControls_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->SwtControlsV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_SwtControls_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SwtControlsI(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_SwtControls_Get_Delay(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->SwtControlsF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_SwtControls_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SwtControlsI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_SwtControls_Get_IsLocked(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->SwtControlsI(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_SwtControls_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->SwtControlsS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_SwtControls_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SwtControlsI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_SwtControls_Get_SwitchedObj(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->SwtControlsS(2, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_SwtControls_Get_SwitchedTerm(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->SwtControlsI(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_SwtControls_Set_Action(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SwtControlsI(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_SwtControls_Set_Delay(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SwtControlsF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_SwtControls_Set_IsLocked(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SwtControlsI(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_SwtControls_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SwtControlsS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_SwtControls_Set_SwitchedObj(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SwtControlsS(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_SwtControls_Set_SwitchedTerm(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->SwtControlsI(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL int32_t ctx_Topology_Get_ActiveBranch(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->TopologyI(5, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Topology_Get_ActiveLevel(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->TopologyI(12, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Topology_Get_AllIsolatedBranches(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->TopologyV, 1, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Topology_Get_AllIsolatedLoads(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->TopologyV, 2, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Topology_Get_AllLoopedPairs(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->TopologyV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_Topology_Get_BackwardBranch(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->TopologyI(7, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Topology_Get_BranchName(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->TopologyS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Topology_Get_BusName(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->TopologyS(2, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Topology_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->TopologyI(3, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Topology_Get_FirstLoad(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->TopologyI(10, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Topology_Get_ForwardBranch(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->TopologyI(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Topology_Get_LoopedBranch(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->TopologyI(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Topology_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->TopologyI(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Topology_Get_NextLoad(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->TopologyI(11, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Topology_Get_NumIsolatedBranches(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->TopologyI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Topology_Get_NumIsolatedLoads(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->TopologyI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Topology_Get_NumLoops(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->TopologyI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Topology_Get_ParallelBranch(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->TopologyI(9, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Topology_Set_BranchName(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TopologyS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Topology_Set_BusName(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TopologyS(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->TransformersV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL int32_t ctx_Transformers_Get_CoreType(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->TransformersI(11, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Transformers_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->TransformersI(10, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Transformers_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->TransformersI(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL uint16_t ctx_Transformers_Get_IsDelta(const void* ctx)
{
    CTX_OR_PRIME
    uint16_t res;
    res = ((OddieContext*) ctx)->TransformersI(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Transformers_Get_MaxTap(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->TransformersF(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Transformers_Get_MinTap(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->TransformersF(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Transformers_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->TransformersS(2, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Transformers_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->TransformersI(9, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Transformers_Get_NumTaps(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->TransformersI(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Transformers_Get_NumWindings(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->TransformersI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Transformers_Get_R(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->TransformersF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Transformers_Get_RdcOhms(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->TransformersF(22, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Transformers_Get_Rneut(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->TransformersF(14, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Transformers_Get_Tap(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->TransformersF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Transformers_Get_Wdg(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->TransformersI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Get_WdgCurrents(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->TransformersV, 2, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Get_WdgCurrents_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->TransformersV, 2, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Get_WdgVoltages(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->TransformersV, 1, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Get_WdgVoltages_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->TransformersV, 1, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL const char* ctx_Transformers_Get_XfmrCode(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->TransformersS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Transformers_Get_Xhl(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->TransformersF(16, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Transformers_Get_Xht(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->TransformersF(18, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Transformers_Get_Xlt(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->TransformersF(20, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Transformers_Get_Xneut(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->TransformersF(12, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Transformers_Get_kV(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->TransformersF(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Transformers_Get_kVA(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->TransformersF(10, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Transformers_Get_strWdgCurrents(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->TransformersS(4, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Set_CoreType(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TransformersI(12, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Set_IsDelta(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TransformersI(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Set_MaxTap(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TransformersF(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Set_MinTap(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TransformersF(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TransformersS(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Set_NumTaps(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TransformersI(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Set_NumWindings(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TransformersI(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Set_R(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TransformersF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Set_RdcOhms(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TransformersF(23, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Set_Rneut(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TransformersF(15, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Set_Tap(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TransformersF(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Set_Wdg(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TransformersI(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Set_XfmrCode(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TransformersS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Set_Xhl(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TransformersF(17, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Set_Xht(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TransformersF(19, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Set_Xlt(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TransformersF(21, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Set_Xneut(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TransformersF(13, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Set_kV(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TransformersF(9, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Set_kVA(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->TransformersF(11, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Vsources_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_stringarray_func((OddieContext*) ctx, ((OddieContext*) ctx)->VsourcesV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL double ctx_Vsources_Get_AngleDeg(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->VsourcesF(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Vsources_Get_BasekV(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->VsourcesF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Vsources_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->VsourcesI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Vsources_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->VsourcesI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Vsources_Get_Frequency(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->VsourcesF(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_Vsources_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->VsourcesS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Vsources_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->VsourcesI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_Vsources_Get_Phases(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->VsourcesI(3, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_Vsources_Get_pu(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->VsourcesF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_Vsources_Set_AngleDeg(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->VsourcesF(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Vsources_Set_BasekV(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->VsourcesF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Vsources_Set_Frequency(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->VsourcesF(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Vsources_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->VsourcesS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Vsources_Set_Phases(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->VsourcesI(4, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_Vsources_Set_pu(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->VsourcesF(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL int32_t ctx_XYCurves_Get_Count(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->XYCurvesI(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_XYCurves_Get_First(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->XYCurvesI(1, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL const char* ctx_XYCurves_Get_Name(const void* ctx)
{
    CTX_OR_PRIME
    const char* res;
    res = ((OddieContext*) ctx)->XYCurvesS(0, NULL);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_XYCurves_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->XYCurvesI(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL int32_t ctx_XYCurves_Get_Npts(const void* ctx)
{
    CTX_OR_PRIME
    int32_t res;
    res = ((OddieContext*) ctx)->XYCurvesI(3, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_XYCurves_Get_Xarray(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->XYCurvesV, 0, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_XYCurves_Get_Xarray_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->XYCurvesV, 0, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL double ctx_XYCurves_Get_Xscale(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->XYCurvesF(8, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_XYCurves_Get_Xshift(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->XYCurvesF(4, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_XYCurves_Get_Yarray(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->XYCurvesV, 2, ResultPtr, ResultDims, NULL);
}

ALTDSS_ODDIE_DLL void ctx_XYCurves_Get_Yarray_GR(const void* ctx)
{  
    CTX_OR_PRIME
    oddie_vararray_float64_func((OddieContext*) ctx, ((OddieContext*) ctx)->XYCurvesV, 2, &((OddieContext*) ctx)->GR_DataPtr_PDouble, &((OddieContext*) ctx)->GR_Counts_PDouble[0], NULL);
}

ALTDSS_ODDIE_DLL double ctx_XYCurves_Get_Yscale(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->XYCurvesF(10, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_XYCurves_Get_Yshift(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->XYCurvesF(6, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_XYCurves_Get_x(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->XYCurvesF(0, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL double ctx_XYCurves_Get_y(const void* ctx)
{
    CTX_OR_PRIME
    double res;
    res = ((OddieContext*) ctx)->XYCurvesF(2, 0);
    oddie_map_error(ctx);
    return res;
}

ALTDSS_ODDIE_DLL void ctx_XYCurves_Set_Name(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->XYCurvesS(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_XYCurves_Set_Npts(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->XYCurvesI(4, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_XYCurves_Set_Xscale(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->XYCurvesF(9, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_XYCurves_Set_Xshift(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->XYCurvesF(5, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_XYCurves_Set_Yarray(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    int32_t var_type = ODDIE_PTR_VAR_TYPE_DOUBLE;
    ((OddieContext*) ctx)->XYCurvesV(3, (void **) &ValuePtr, &var_type, &ValueCount);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_XYCurves_Set_Yscale(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->XYCurvesF(11, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_XYCurves_Set_Yshift(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->XYCurvesF(7, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_XYCurves_Set_x(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->XYCurvesF(1, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL void ctx_XYCurves_Set_y(const void* ctx, double Value)
{
    CTX_OR_PRIME
    ((OddieContext*) ctx)->XYCurvesF(3, Value);
    oddie_map_error(ctx);
}

ALTDSS_ODDIE_DLL int32_t ctx_Bus_Get_Next(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Bus_Get_Next");
    return 0;
}

ALTDSS_ODDIE_DLL int32_t ctx_Capacitors_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Capacitors_Get_idx");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_Capacitors_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Capacitors_Set_idx");
}

ALTDSS_ODDIE_DLL int32_t ctx_CapControls_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "CapControls_Get_idx");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_CapControls_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "CapControls_Set_idx");
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_ElementLosses(const void* ctx, double** ResultPtr, int32_t* ResultDims, int32_t *ElementsPtr, int32_t ElementsCount)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Circuit_Get_ElementLosses");
}

ALTDSS_ODDIE_DLL void ctx_Circuit_Get_ElementLosses_GR(const void* ctx, int32_t *ElementsPtr, int32_t ElementsCount)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Circuit_Get_ElementLosses");
}

ALTDSS_ODDIE_DLL const char* ctx_Circuit_Save(const void* ctx, const char* dirOrFilePath, uint32_t saveFlags)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Circuit_Save");
    return NULL;
}

ALTDSS_ODDIE_DLL void ctx_Circuit_SetCktElementIndex(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Circuit_SetCktElementIndex");
}

ALTDSS_ODDIE_DLL void ctx_Circuit_SetCktElementName(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Circuit_SetCktElementName");
}

ALTDSS_ODDIE_DLL int32_t ctx_CktElement_Get_Handle(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "CktElement_Get_Handle");
    return 0;
}

ALTDSS_ODDIE_DLL uint16_t ctx_CktElement_Get_HasOCPDevice(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "CktElement_Get_HasOCPDevice");
    return 0;
}

ALTDSS_ODDIE_DLL uint16_t ctx_CktElement_Get_IsIsolated(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "CktElement_Get_IsIsolated");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_NodeRef(const void* ctx, int32_t** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "CktElement_Get_NodeRef");
}

ALTDSS_ODDIE_DLL void ctx_CktElement_Get_NodeRef_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "CktElement_Get_NodeRef");
}

ALTDSS_ODDIE_DLL int32_t ctx_CktElement_Get_VariableIdx(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "CktElement_Get_VariableIdx");
    return 0;
}

ALTDSS_ODDIE_DLL double ctx_CmathLib_Get_cdang(const void* ctx, double RealPart, double ImagPart)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "CmathLib_Get_cdang");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_CmathLib_Get_cdiv(const void* ctx, double** ResultPtr, int32_t* ResultDims, double a1, double b1, double a2, double b2)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "CmathLib_Get_cdiv");
}

ALTDSS_ODDIE_DLL void ctx_CmathLib_Get_cdiv_GR(const void* ctx, double a1, double b1, double a2, double b2)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "CmathLib_Get_cdiv");
}

ALTDSS_ODDIE_DLL void ctx_CmathLib_Get_cmul(const void* ctx, double** ResultPtr, int32_t* ResultDims, double a1, double b1, double a2, double b2)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "CmathLib_Get_cmul");
}

ALTDSS_ODDIE_DLL void ctx_CmathLib_Get_cmul_GR(const void* ctx, double a1, double b1, double a2, double b2)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "CmathLib_Get_cmul");
}

ALTDSS_ODDIE_DLL uint16_t ctx_DSS_Get_COMErrorResults(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "DSS_Get_COMErrorResults");
    return 0;
}

ALTDSS_ODDIE_DLL uint32_t ctx_DSS_Get_CompatFlags(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "DSS_Get_CompatFlags");
    return 0;
}

ALTDSS_ODDIE_DLL uint16_t ctx_DSS_Get_EnableArrayDimensions(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "DSS_Get_EnableArrayDimensions");
    return 0;
}

ALTDSS_ODDIE_DLL uint16_t ctx_DSS_Get_LegacyModels(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "DSS_Get_LegacyModels");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_DSS_Set_COMErrorResults(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "DSS_Set_COMErrorResults");
}

ALTDSS_ODDIE_DLL void ctx_DSS_Set_CompatFlags(const void* ctx, uint32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "DSS_Set_CompatFlags");
}

ALTDSS_ODDIE_DLL void ctx_DSS_Set_EnableArrayDimensions(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "DSS_Set_EnableArrayDimensions");
}

ALTDSS_ODDIE_DLL void ctx_DSS_Set_LegacyModels(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "DSS_Set_LegacyModels");
}

ALTDSS_ODDIE_DLL void ctx_DSSimComs_BusVoltage(const void* ctx, double** ResultPtr, int32_t* ResultDims, size_t Index)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "DSSimComs_BusVoltage");
}

ALTDSS_ODDIE_DLL void ctx_DSSimComs_BusVoltage_GR(const void* ctx, size_t Index)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "DSSimComs_BusVoltage");
}

ALTDSS_ODDIE_DLL void ctx_DSSimComs_BusVoltagepu(const void* ctx, double** ResultPtr, int32_t* ResultDims, size_t Index)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "DSSimComs_BusVoltagepu");
}

ALTDSS_ODDIE_DLL void ctx_DSSimComs_BusVoltagepu_GR(const void* ctx, size_t Index)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "DSSimComs_BusVoltagepu");
}

ALTDSS_ODDIE_DLL uint16_t ctx_Error_Get_ExtendedErrors(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Error_Get_ExtendedErrors");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_Error_Set_ExtendedErrors(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Error_Set_ExtendedErrors");
}

ALTDSS_ODDIE_DLL const char* ctx_Generators_Get_Bus1(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Generators_Get_Bus1");
    return NULL;
}

ALTDSS_ODDIE_DLL int32_t ctx_Generators_Get_Class_(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Generators_Get_Class_");
    return 0;
}

ALTDSS_ODDIE_DLL const char* ctx_Generators_Get_daily(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Generators_Get_daily");
    return NULL;
}

ALTDSS_ODDIE_DLL const char* ctx_Generators_Get_duty(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Generators_Get_duty");
    return NULL;
}

ALTDSS_ODDIE_DLL uint16_t ctx_Generators_Get_IsDelta(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Generators_Get_IsDelta");
    return 0;
}

ALTDSS_ODDIE_DLL double ctx_Generators_Get_kva(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Generators_Get_kva");
    return 0;
}

ALTDSS_ODDIE_DLL int32_t ctx_Generators_Get_Status(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Generators_Get_Status");
    return 0;
}

ALTDSS_ODDIE_DLL const char* ctx_Generators_Get_Yearly(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Generators_Get_Yearly");
    return NULL;
}

ALTDSS_ODDIE_DLL void ctx_Generators_Set_Bus1(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Generators_Set_Bus1");
}

ALTDSS_ODDIE_DLL void ctx_Generators_Set_Class_(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Generators_Set_Class_");
}

ALTDSS_ODDIE_DLL void ctx_Generators_Set_daily(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Generators_Set_daily");
}

ALTDSS_ODDIE_DLL void ctx_Generators_Set_duty(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Generators_Set_duty");
}

ALTDSS_ODDIE_DLL void ctx_Generators_Set_IsDelta(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Generators_Set_IsDelta");
}

ALTDSS_ODDIE_DLL void ctx_Generators_Set_kva(const void* ctx, double Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Generators_Set_kva");
}

ALTDSS_ODDIE_DLL void ctx_Generators_Set_Status(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Generators_Set_Status");
}

ALTDSS_ODDIE_DLL void ctx_Generators_Set_Yearly(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Generators_Set_Yearly");
}

ALTDSS_ODDIE_DLL int32_t ctx_GICSources_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "GICSources_Get_idx");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_GICSources_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "GICSources_Set_idx");
}

ALTDSS_ODDIE_DLL int32_t ctx_ISources_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "ISources_Get_idx");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_ISources_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "ISources_Set_idx");
}

ALTDSS_ODDIE_DLL int32_t ctx_LineCodes_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "LineCodes_Get_idx");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_LineCodes_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "LineCodes_Set_idx");
}

ALTDSS_ODDIE_DLL int32_t ctx_Lines_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Lines_Get_idx");
    return 0;
}

ALTDSS_ODDIE_DLL int32_t ctx_Lines_Get_TotalCust(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Lines_Get_TotalCust");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_Lines_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Lines_Set_idx");
}

ALTDSS_ODDIE_DLL int32_t ctx_LoadShapes_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "LoadShapes_Get_idx");
    return 0;
}

ALTDSS_ODDIE_DLL double ctx_LoadShapes_Get_MaxP(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "LoadShapes_Get_MaxP");
    return 0;
}

ALTDSS_ODDIE_DLL double ctx_LoadShapes_Get_MaxQ(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "LoadShapes_Get_MaxQ");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "LoadShapes_Set_idx");
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Set_MaxP(const void* ctx, double Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "LoadShapes_Set_MaxP");
}

ALTDSS_ODDIE_DLL void ctx_LoadShapes_Set_MaxQ(const void* ctx, double Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "LoadShapes_Set_MaxQ");
}

ALTDSS_ODDIE_DLL int32_t ctx_Meters_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Meters_Get_idx");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_Meters_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Meters_Set_idx");
}

ALTDSS_ODDIE_DLL int32_t ctx_Monitors_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Monitors_Get_idx");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_Monitors_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Monitors_Set_idx");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllCplxSeqCurrents(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllCplxSeqCurrents");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllCplxSeqCurrents_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllCplxSeqCurrents");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllCurrents(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllCurrents");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllCurrents_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllCurrents");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllCurrentsMagAng(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllCurrentsMagAng");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllCurrentsMagAng_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllCurrentsMagAng");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllMaxCurrents(const void* ctx, double** ResultPtr, int32_t* ResultDims, uint16_t AllNodes)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllMaxCurrents");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllMaxCurrents_GR(const void* ctx, uint16_t AllNodes)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllMaxCurrents");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllNames");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllNames_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllNames");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllNumConductors(const void* ctx, int32_t** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllNumConductors");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllNumConductors_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllNumConductors");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllNumPhases(const void* ctx, int32_t** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllNumPhases");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllNumPhases_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllNumPhases");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllNumTerminals(const void* ctx, int32_t** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllNumTerminals");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllNumTerminals_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllNumTerminals");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllPctEmerg(const void* ctx, double** ResultPtr, int32_t* ResultDims, uint16_t AllNodes)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllPctEmerg");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllPctEmerg_GR(const void* ctx, uint16_t AllNodes)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllPctEmerg");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllPctNorm(const void* ctx, double** ResultPtr, int32_t* ResultDims, uint16_t AllNodes)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllPctNorm");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllPctNorm_GR(const void* ctx, uint16_t AllNodes)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllPctNorm");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllPowers(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllPowers");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllPowers_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllPowers");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllSeqCurrents(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllSeqCurrents");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllSeqCurrents_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllSeqCurrents");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllSeqPowers(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllSeqPowers");
}

ALTDSS_ODDIE_DLL void ctx_PDElements_Get_AllSeqPowers_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PDElements_Get_AllSeqPowers");
}

ALTDSS_ODDIE_DLL const char* ctx_PVSystems_Get_daily(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PVSystems_Get_daily");
    return NULL;
}

ALTDSS_ODDIE_DLL const char* ctx_PVSystems_Get_duty(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PVSystems_Get_duty");
    return NULL;
}

ALTDSS_ODDIE_DLL void ctx_PVSystems_Get_RegisterNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PVSystems_Get_RegisterNames");
}

ALTDSS_ODDIE_DLL void ctx_PVSystems_Get_RegisterNames_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PVSystems_Get_RegisterNames");
}

ALTDSS_ODDIE_DLL void ctx_PVSystems_Get_RegisterValues(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PVSystems_Get_RegisterValues");
}

ALTDSS_ODDIE_DLL void ctx_PVSystems_Get_RegisterValues_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PVSystems_Get_RegisterValues");
}

ALTDSS_ODDIE_DLL const char* ctx_PVSystems_Get_Tdaily(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PVSystems_Get_Tdaily");
    return NULL;
}

ALTDSS_ODDIE_DLL const char* ctx_PVSystems_Get_Tduty(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PVSystems_Get_Tduty");
    return NULL;
}

ALTDSS_ODDIE_DLL const char* ctx_PVSystems_Get_Tyearly(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PVSystems_Get_Tyearly");
    return NULL;
}

ALTDSS_ODDIE_DLL const char* ctx_PVSystems_Get_yearly(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PVSystems_Get_yearly");
    return NULL;
}

ALTDSS_ODDIE_DLL void ctx_PVSystems_Set_daily(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PVSystems_Set_daily");
}

ALTDSS_ODDIE_DLL void ctx_PVSystems_Set_duty(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PVSystems_Set_duty");
}

ALTDSS_ODDIE_DLL void ctx_PVSystems_Set_Tdaily(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PVSystems_Set_Tdaily");
}

ALTDSS_ODDIE_DLL void ctx_PVSystems_Set_Tduty(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PVSystems_Set_Tduty");
}

ALTDSS_ODDIE_DLL void ctx_PVSystems_Set_Tyearly(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PVSystems_Set_Tyearly");
}

ALTDSS_ODDIE_DLL void ctx_PVSystems_Set_yearly(const void* ctx, const char* Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "PVSystems_Set_yearly");
}

ALTDSS_ODDIE_DLL int32_t ctx_RegControls_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "RegControls_Get_idx");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_RegControls_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "RegControls_Set_idx");
}

ALTDSS_ODDIE_DLL int32_t ctx_Sensors_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Sensors_Get_idx");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Get_kVS(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Sensors_Get_kVS");
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Get_kVS_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Sensors_Get_kVS");
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Sensors_Set_idx");
}

ALTDSS_ODDIE_DLL void ctx_Sensors_Set_kVS(const void* ctx, const double* ValuePtr, int32_t ValueCount)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Sensors_Set_kVS");
}

ALTDSS_ODDIE_DLL uint16_t ctx_Settings_Get_ControlTrace(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Settings_Get_ControlTrace");
    return 0;
}

ALTDSS_ODDIE_DLL int32_t ctx_Settings_Get_IterateDisabled(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Settings_Get_IterateDisabled");
    return 0;
}

ALTDSS_ODDIE_DLL uint16_t ctx_Settings_Get_LoadsTerminalCheck(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Settings_Get_LoadsTerminalCheck");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_Settings_Set_ControlTrace(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Settings_Set_ControlTrace");
}

ALTDSS_ODDIE_DLL void ctx_Settings_Set_IterateDisabled(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Settings_Set_IterateDisabled");
}

ALTDSS_ODDIE_DLL void ctx_Settings_Set_LoadsTerminalCheck(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Settings_Set_LoadsTerminalCheck");
}

ALTDSS_ODDIE_DLL void ctx_Settings_SetPropertyNameStyle(const void* ctx, int32_t style)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Settings_SetPropertyNameStyle");
}

ALTDSS_ODDIE_DLL double ctx_Solution_Get_IntervalHrs(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Solution_Get_IntervalHrs");
    return 0;
}

ALTDSS_ODDIE_DLL int32_t ctx_Solution_Get_MinIterations(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Solution_Get_MinIterations");
    return 0;
}

ALTDSS_ODDIE_DLL double ctx_Solution_Get_Time_of_Step(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Solution_Get_Time_of_Step");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_IntervalHrs(const void* ctx, double Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Solution_Set_IntervalHrs");
}

ALTDSS_ODDIE_DLL void ctx_Solution_Set_MinIterations(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Solution_Set_MinIterations");
}

ALTDSS_ODDIE_DLL void ctx_Solution_SolveSnap(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Solution_SolveSnap");
}

ALTDSS_ODDIE_DLL int32_t ctx_SwtControls_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "SwtControls_Get_idx");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_SwtControls_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "SwtControls_Set_idx");
}

ALTDSS_ODDIE_DLL const char* ctx_Text_Get_Command(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Text_Get_Command");
    return NULL;
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Get_AllLossesByType(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Transformers_Get_AllLossesByType");
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Get_AllLossesByType_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Transformers_Get_AllLossesByType");
}

ALTDSS_ODDIE_DLL int32_t ctx_Transformers_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Transformers_Get_idx");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Get_LossesByType(const void* ctx, double** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Transformers_Get_LossesByType");
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Get_LossesByType_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Transformers_Get_LossesByType");
}

ALTDSS_ODDIE_DLL void ctx_Transformers_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Transformers_Set_idx");
}

ALTDSS_ODDIE_DLL int32_t ctx_Vsources_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Vsources_Get_idx");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_Vsources_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "Vsources_Set_idx");
}

ALTDSS_ODDIE_DLL void ctx_XYCurves_Get_AllNames(const void* ctx, char*** ResultPtr, int32_t* ResultDims)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "XYCurves_Get_AllNames");
}

ALTDSS_ODDIE_DLL void ctx_XYCurves_Get_AllNames_GR(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "XYCurves_Get_AllNames");
}

ALTDSS_ODDIE_DLL int32_t ctx_XYCurves_Get_idx(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "XYCurves_Get_idx");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_XYCurves_Set_idx(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "XYCurves_Set_idx");
}

ALTDSS_ODDIE_DLL uint16_t ctx_YMatrix_CheckConvergence(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "YMatrix_CheckConvergence");
    return 0;
}

ALTDSS_ODDIE_DLL int32_t ctx_YMatrix_Get_Iteration(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "YMatrix_Get_Iteration");
    return 0;
}

ALTDSS_ODDIE_DLL uint16_t ctx_YMatrix_Get_LoadsNeedUpdating(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "YMatrix_Get_LoadsNeedUpdating");
    return 0;
}

ALTDSS_ODDIE_DLL uint16_t ctx_YMatrix_Get_SolutionInitialized(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "YMatrix_Get_SolutionInitialized");
    return 0;
}

ALTDSS_ODDIE_DLL uint64_t ctx_YMatrix_Get_SolverOptions(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "YMatrix_Get_SolverOptions");
    return 0;
}

ALTDSS_ODDIE_DLL void ctx_YMatrix_Set_Iteration(const void* ctx, int32_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "YMatrix_Set_Iteration");
}

ALTDSS_ODDIE_DLL void ctx_YMatrix_Set_LoadsNeedUpdating(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "YMatrix_Set_LoadsNeedUpdating");
}

ALTDSS_ODDIE_DLL void ctx_YMatrix_Set_SolutionInitialized(const void* ctx, uint16_t Value)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "YMatrix_Set_SolutionInitialized");
}

ALTDSS_ODDIE_DLL void ctx_YMatrix_Set_SolverOptions(const void* ctx, uint64_t opts)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "YMatrix_Set_SolverOptions");
}

ALTDSS_ODDIE_DLL void ctx_YMatrix_SetGeneratordQdV(const void* ctx)
{
    CTX_OR_PRIME
    oddie_error_not_implemented((OddieContext*) ctx, "YMatrix_SetGeneratordQdV");
}
