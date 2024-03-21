#include <stddef.h>
#include <stdint.h>
#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <libloaderapi.h>
#else
#include <dlfcn.h>
#endif
#include <string.h>

#define DSS_ERR_NUM_CHR 1023
#define DSS_STR_BUFFER_NUM_CHR 1023


typedef const char* (*oddie_str_func_t)(int32_t mode, const char* value);
typedef double (*oddie_float64_func_t)(int32_t mode, double value);
typedef double (*oddie_float64_func2_t)(int32_t mode, double value, double value2);
typedef int32_t (*oddie_int32_func_t)(int32_t mode, int32_t value);
typedef void (*oddie_variant_func_t)(int32_t mode, void **ptr, int32_t *ptrType, int32_t *ptrSize);
typedef void (*oddie_void_void_func_t)(void);
typedef void (*oddie_void_ppdouble_func_t)(double **);
typedef int32_t (*oddie_int32_ppdouble_func_t)(double **NodeV);
typedef int32_t (*oddie_int32_void_func_t)(void);
typedef const char* (*oddie_str_void_func_t)(void);
typedef const char* (*oddie_str_str_func_t)(const char *a);
typedef void (*oddie_get_y_csc_func_t)(void *hY, uint32_t nBus, uint32_t nNz, int32_t **ColPtr, int32_t **RowIdx, double **cVals);
typedef void (*oddie_add_in_aux_currents_func_t)(int32_t SType);
typedef void (*oddie_build_y_matrix_func_t)(int32_t BuildOps, int32_t AllocateVI);
typedef uint32_t (*oddie_y_params_func_t)(void **hY, uint32_t* nBus, uint32_t *nNZ);

typedef struct
{
#ifdef WIN32
    HMODULE dll_handle;
#else
    void* dll_handle;
#endif    
    int32_t error_number;
    char error_desc[DSS_ERR_NUM_CHR + 1];
    char char_buffer[DSS_STR_BUFFER_NUM_CHR + 1];
    int32_t PropIndex;
    int32_t map_errors;

    // C-API pointer data (GR mode)
    double *GR_DataPtr_PDouble;
    int32_t *GR_DataPtr_PInteger;
    int8_t *GR_DataPtr_PByte;
    int32_t GR_Counts_PDouble[4];
    int32_t GR_Counts_PInteger[4];
    int32_t GR_Counts_PByte[4];

    oddie_void_void_func_t GetPCInjCurr;
    oddie_void_void_func_t GetSourceInjCurrents;
    oddie_void_void_func_t ZeroInjCurr;
    oddie_void_ppdouble_func_t getIpointer;
    oddie_void_ppdouble_func_t getVpointer;
    oddie_int32_ppdouble_func_t SolveSystem;
    oddie_get_y_csc_func_t GetCompressedYMatrix;
    oddie_add_in_aux_currents_func_t AddInAuxCurrents;
    oddie_build_y_matrix_func_t BuildYMatrixD;
    oddie_y_params_func_t InitAndGetYparams;
    oddie_int32_func_t SystemYChanged;
    oddie_int32_func_t UseAuxCurrents;

    oddie_int32_void_func_t ErrorCode;
    oddie_str_void_func_t ErrorDesc;
    oddie_str_str_func_t DSSPut_Command;

    oddie_str_func_t ActiveClassS;
    oddie_str_func_t BUSS;
    oddie_str_func_t CapacitorsS;
    oddie_str_func_t CapControlsS;
    oddie_str_func_t CircuitS;
    oddie_str_func_t CktElementS;
    oddie_str_func_t DSSElementS;
    oddie_str_func_t DSSExecutiveS;
    oddie_str_func_t DSSLoadsS;
    oddie_str_func_t DSSProgressS;
    oddie_str_func_t DSSProperties;
    oddie_str_func_t DSSS;
    oddie_str_func_t FusesS;
    oddie_str_func_t GeneratorsS;
    oddie_str_func_t GICSourcesS;
    oddie_str_func_t IsourceS;
    oddie_str_func_t LineCodesS;
    oddie_str_func_t LinesS;
    oddie_str_func_t LoadShapeS;
    oddie_str_func_t MetersS;
    oddie_str_func_t MonitorsS;
    oddie_str_func_t ParserS;
    oddie_str_func_t PDElementsS;
    oddie_str_func_t PVsystemsS;
    oddie_str_func_t ReclosersS;
    oddie_str_func_t ReduceCktS;
    oddie_str_func_t RegControlsS;
    oddie_str_func_t RelaysS;
    oddie_str_func_t SensorsS;
    oddie_str_func_t SettingsS;
    oddie_str_func_t SolutionS;
    oddie_str_func_t SwtControlsS;
    oddie_str_func_t TopologyS;
    oddie_str_func_t TransformersS;
    oddie_str_func_t VsourcesS;
    oddie_str_func_t XYCurvesS;

    oddie_float64_func_t BUSF;
    oddie_float64_func_t CapacitorsF;
    oddie_float64_func_t CapControlsF;
    oddie_float64_func_t CktElementF;
    oddie_float64_func_t DSSLoadsF;
    oddie_float64_func_t FusesF;
    oddie_float64_func_t GeneratorsF;
    oddie_float64_func_t GICSourcesF;
    oddie_float64_func_t IsourceF;
    oddie_float64_func_t LineCodesF;
    oddie_float64_func_t LinesF;
    oddie_float64_func_t LoadShapeF;
    oddie_float64_func_t MetersF;
    oddie_float64_func_t ParserF;
    oddie_float64_func_t PDElementsF;
    oddie_float64_func_t PVsystemsF;
    oddie_float64_func_t ReclosersF;
    oddie_float64_func_t ReduceCktF;
    oddie_float64_func_t RegControlsF;
    oddie_float64_func_t SensorsF;
    oddie_float64_func_t SettingsF;
    oddie_float64_func_t SolutionF;
    oddie_float64_func_t SwtControlsF;
    oddie_float64_func_t TransformersF;
    oddie_float64_func_t VsourcesF;
    oddie_float64_func_t XYCurvesF;

    oddie_float64_func2_t CircuitF;
    oddie_float64_func2_t CmathLibF;

    oddie_int32_func_t ActiveClassI;
    oddie_int32_func_t BUSI;
    oddie_int32_func_t CapacitorsI;
    oddie_int32_func_t CapControlsI;
    oddie_int32_func_t CircuitI;
    oddie_int32_func_t CktElementI;
    oddie_int32_func_t CtrlQueueI;
    oddie_int32_func_t DSSElementI;
    oddie_int32_func_t DSSExecutiveI;
    oddie_int32_func_t DSSI;
    oddie_int32_func_t DSSLoads;
    oddie_int32_func_t DSSProgressI;
    oddie_int32_func_t FusesI;
    oddie_int32_func_t GeneratorsI;
    oddie_int32_func_t GICSourcesI;
    oddie_int32_func_t IsourceI;
    oddie_int32_func_t LineCodesI;
    oddie_int32_func_t LinesI;
    oddie_int32_func_t LoadShapeI;
    oddie_int32_func_t MetersI;
    oddie_int32_func_t MonitorsI;
    oddie_int32_func_t ParallelI;
    oddie_int32_func_t ParserI;
    oddie_int32_func_t PDElementsI;
    oddie_int32_func_t PVsystemsI;
    oddie_int32_func_t ReclosersI;
    oddie_int32_func_t ReduceCktI;
    oddie_int32_func_t RegControlsI;
    oddie_int32_func_t RelaysI;
    oddie_int32_func_t SensorsI;
    oddie_int32_func_t SettingsI;
    oddie_int32_func_t SolutionI;
    oddie_int32_func_t SwtControlsI;
    oddie_int32_func_t TopologyI;
    oddie_int32_func_t TransformersI;
    oddie_int32_func_t VsourcesI;
    oddie_int32_func_t XYCurvesI;

    oddie_variant_func_t ActiveClassV;
    oddie_variant_func_t BUSV;
    oddie_variant_func_t CapacitorsV;
    oddie_variant_func_t CapControlsV;
    oddie_variant_func_t CircuitV;
    oddie_variant_func_t CktElementV;
    oddie_variant_func_t CmathLibV;
    oddie_variant_func_t CtrlQueueV;
    oddie_variant_func_t DSSElementV;
    oddie_variant_func_t DSSLoadsV;
    oddie_variant_func_t DSSV;
    oddie_variant_func_t FusesV;
    oddie_variant_func_t GeneratorsV;
    oddie_variant_func_t GICSourcesV;
    oddie_variant_func_t IsourceV;
    oddie_variant_func_t LineCodesV;
    oddie_variant_func_t LinesV;
    oddie_variant_func_t LoadShapeV;
    oddie_variant_func_t MetersV;
    oddie_variant_func_t MonitorsV;
    oddie_variant_func_t ParallelV;
    oddie_variant_func_t ParserV;
    oddie_variant_func_t PVsystemsV;
    oddie_variant_func_t ReclosersV;
    oddie_variant_func_t RegControlsV;
    oddie_variant_func_t RelaysV;
    oddie_variant_func_t SensorsV;
    oddie_variant_func_t SettingsV;
    oddie_variant_func_t SolutionV;
    oddie_variant_func_t SwtControlsV;
    oddie_variant_func_t TopologyV;
    oddie_variant_func_t TransformersV;
    oddie_variant_func_t VsourcesV;
    oddie_variant_func_t XYCurvesV;
} OddieContext;

void oddie_error_not_implemented(OddieContext* ctx, const char* funcname);
void oddie_vararray_float64_func(OddieContext* ctx, oddie_variant_func_t func, int32_t mode, double** resultPtr, int32_t* resultDims, double* inPtr);
void oddie_vararray_int32_func(OddieContext* ctx, oddie_variant_func_t func, int32_t mode, int32_t** resultPtr, int32_t* resultDims);
void oddie_vararray_stringarray_func(OddieContext* ctx, oddie_variant_func_t func, int32_t mode, char*** resultPtr, int32_t* resultDims, char* inPtr);
void oddie_vararray_int8_func(OddieContext* ctx, oddie_variant_func_t func, int32_t mode, int8_t** resultPtr, int32_t* resultDims);
const char *oddie_int32_to_pchar(OddieContext* ctx, int32_t value);


const int32_t ODDIE_PTR_VAR_TYPE_INTEGER = 1;
const int32_t ODDIE_PTR_VAR_TYPE_DOUBLE = 2;
const int32_t ODDIE_PTR_VAR_TYPE_COMPLEX = 3;
const int32_t ODDIE_PTR_VAR_TYPE_STRING = 4;
