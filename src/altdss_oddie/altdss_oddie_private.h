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
typedef void (*oddie_void_str_func_t)(const char *a);

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
    const char* currentString;
    int32_t strict;
    int32_t PropIndex;
    int32_t map_errors;

    // C-API pointer data (GR mode)
    double *GR_DataPtr_PDouble;
    int32_t *GR_DataPtr_PInteger;
    int8_t *GR_DataPtr_PByte;
    int32_t GR_Counts_PDouble[4];
    int32_t GR_Counts_PInteger[4];
    int32_t GR_Counts_PByte[4];

    uint32_t compat_flags;

    oddie_void_str_func_t DSSDisposeString;

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
    oddie_str_func_t ReactorsS;
    oddie_str_func_t ReclosersS;
    oddie_str_func_t ReduceCktS;
    oddie_str_func_t RegControlsS;
    oddie_str_func_t RelaysS;
    oddie_str_func_t SensorsS;
    oddie_str_func_t SettingsS;
    oddie_str_func_t SolutionS;
    oddie_str_func_t StoragesS;
    oddie_str_func_t SwtControlsS;
    oddie_str_func_t TopologyS;
    oddie_str_func_t TransformersS;
    oddie_str_func_t VsourcesS;
    oddie_str_func_t WindGensS;
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
    oddie_float64_func_t ReactorsF;
    oddie_float64_func_t ReclosersF;
    oddie_float64_func_t ReduceCktF;
    oddie_float64_func_t RegControlsF;
    oddie_float64_func_t SensorsF;
    oddie_float64_func_t SettingsF;
    oddie_float64_func_t SolutionF;
    oddie_float64_func_t StoragesF;
    oddie_float64_func_t SwtControlsF;
    oddie_float64_func_t TransformersF;
    oddie_float64_func_t VsourcesF;
    oddie_float64_func_t WindGensF;
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
    oddie_int32_func_t ReactorsI;
    oddie_int32_func_t ReclosersI;
    oddie_int32_func_t ReduceCktI;
    oddie_int32_func_t RegControlsI;
    oddie_int32_func_t RelaysI;
    oddie_int32_func_t SensorsI;
    oddie_int32_func_t SettingsI;
    oddie_int32_func_t SolutionI;
    oddie_int32_func_t StoragesI;
    oddie_int32_func_t SwtControlsI;
    oddie_int32_func_t TopologyI;
    oddie_int32_func_t TransformersI;
    oddie_int32_func_t VsourcesI;
    oddie_int32_func_t WindGensI;
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
    oddie_variant_func_t ReactorsV;
    oddie_variant_func_t ReclosersV;
    oddie_variant_func_t RegControlsV;
    oddie_variant_func_t RelaysV;
    oddie_variant_func_t SensorsV;
    oddie_variant_func_t SettingsV;
    oddie_variant_func_t SolutionV;
    oddie_variant_func_t StoragesV;
    oddie_variant_func_t SwtControlsV;
    oddie_variant_func_t TopologyV;
    oddie_variant_func_t TransformersV;
    oddie_variant_func_t VsourcesV;
    oddie_variant_func_t WindGensV;
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
const int32_t ODDIE_PTR_VAR_TYPE_BYTES = 5;

#ifdef ALTDSS_ODDIE_LINK_OPENDSSDIRECT_API
#define ODDIE_ODD_EXTERN extern
#define ODDIE_STR_FUNC(FUNCNAME) ODDIE_ODD_EXTERN const char* FUNCNAME(int32_t mode, const char* value);
#define ODDIE_FLOAT64_FUNC(FUNCNAME) ODDIE_ODD_EXTERN double FUNCNAME(int32_t mode, double value);
#define ODDIE_FLOAT64_FUNC2(FUNCNAME) ODDIE_ODD_EXTERN double FUNCNAME(int32_t mode, double value, double value2);
#define ODDIE_INT32_FUNC(FUNCNAME) ODDIE_ODD_EXTERN int32_t FUNCNAME(int32_t mode, int32_t value);
#define ODDIE_VARIANT_FUNC(FUNCNAME) ODDIE_ODD_EXTERN void FUNCNAME(int32_t mode, void **ptr, int32_t *ptrType, int32_t *ptrSize);
#define ODDIE_VOID_VOID_FUNC(FUNCNAME) ODDIE_ODD_EXTERN void FUNCNAME(void);
#define ODDIE_VOID_PPDOUBLE_FUNC(FUNCNAME) ODDIE_ODD_EXTERN void FUNCNAME(double **);
#define ODDIE_INT32_PPDOUBLE_FUNC(FUNCNAME) ODDIE_ODD_EXTERN int32_t FUNCNAME(double **NodeV);
#define ODDIE_INT32_VOID_FUNC(FUNCNAME) ODDIE_ODD_EXTERN int32_t FUNCNAME(void);
#define ODDIE_STR_VOID_FUNC(FUNCNAME) ODDIE_ODD_EXTERN const char* FUNCNAME(void);
#define ODDIE_STR_STR_FUNC(FUNCNAME) ODDIE_ODD_EXTERN const char* FUNCNAME(const char *a);
#define ODDIE_GET_Y_CSC_FUNC(FUNCNAME) ODDIE_ODD_EXTERN void FUNCNAME(void *hY, uint32_t nBus, uint32_t nNz, int32_t **ColPtr, int32_t **RowIdx, double **cVals);
#define ODDIE_ADD_IN_AUX_CURRENTS_FUNC(FUNCNAME) ODDIE_ODD_EXTERN void FUNCNAME(int32_t SType);
#define ODDIE_BUILD_Y_MATRIX_FUNC(FUNCNAME) ODDIE_ODD_EXTERN void FUNCNAME(int32_t BuildOps, int32_t AllocateVI);
#define ODDIE_Y_PARAMS_FUNC(FUNCNAME) ODDIE_ODD_EXTERN uint32_t FUNCNAME(void **hY, uint32_t* nBus, uint32_t *nNZ);

ODDIE_VOID_VOID_FUNC(GetPCInjCurr)
ODDIE_VOID_VOID_FUNC(GetSourceInjCurrents)
ODDIE_VOID_VOID_FUNC(ZeroInjCurr)
ODDIE_VOID_PPDOUBLE_FUNC(getIpointer)
ODDIE_VOID_PPDOUBLE_FUNC(getVpointer)
ODDIE_INT32_PPDOUBLE_FUNC(SolveSystem)
ODDIE_GET_Y_CSC_FUNC(GetCompressedYMatrix)
ODDIE_ADD_IN_AUX_CURRENTS_FUNC(AddInAuxCurrents)
ODDIE_BUILD_Y_MATRIX_FUNC(BuildYMatrixD)
ODDIE_Y_PARAMS_FUNC(InitAndGetYparams)
ODDIE_INT32_FUNC(SystemYChanged)
ODDIE_INT32_FUNC(UseAuxCurrents)

ODDIE_INT32_VOID_FUNC(ErrorCode)
ODDIE_STR_VOID_FUNC(ErrorDesc)
ODDIE_STR_STR_FUNC(DSSPut_Command)

ODDIE_STR_FUNC(ActiveClassS)
ODDIE_STR_FUNC(BUSS)
ODDIE_STR_FUNC(CapacitorsS)
ODDIE_STR_FUNC(CapControlsS)
ODDIE_STR_FUNC(CircuitS)
ODDIE_STR_FUNC(CktElementS)
ODDIE_STR_FUNC(DSSElementS)
ODDIE_STR_FUNC(DSSExecutiveS)
ODDIE_STR_FUNC(DSSLoadsS)
ODDIE_STR_FUNC(DSSProgressS)
ODDIE_STR_FUNC(DSSProperties)
ODDIE_STR_FUNC(DSSS)
ODDIE_STR_FUNC(FusesS)
ODDIE_STR_FUNC(GeneratorsS)
ODDIE_STR_FUNC(GICSourcesS)
ODDIE_STR_FUNC(IsourceS)
ODDIE_STR_FUNC(LineCodesS)
ODDIE_STR_FUNC(LinesS)
ODDIE_STR_FUNC(LoadShapeS)
ODDIE_STR_FUNC(MetersS)
ODDIE_STR_FUNC(MonitorsS)
ODDIE_STR_FUNC(ParserS)
ODDIE_STR_FUNC(PDElementsS)
ODDIE_STR_FUNC(PVsystemsS)
ODDIE_STR_FUNC(ReactorsS)
ODDIE_STR_FUNC(ReclosersS)
ODDIE_STR_FUNC(ReduceCktS)
ODDIE_STR_FUNC(RegControlsS)
ODDIE_STR_FUNC(RelaysS)
ODDIE_STR_FUNC(SensorsS)
ODDIE_STR_FUNC(SettingsS)
ODDIE_STR_FUNC(SolutionS)
ODDIE_STR_FUNC(StoragesS)
ODDIE_STR_FUNC(SwtControlsS)
ODDIE_STR_FUNC(TopologyS)
ODDIE_STR_FUNC(TransformersS)
ODDIE_STR_FUNC(VsourcesS)
ODDIE_STR_FUNC(WindGensS)
ODDIE_STR_FUNC(XYCurvesS)

ODDIE_FLOAT64_FUNC(BUSF)
ODDIE_FLOAT64_FUNC(CapacitorsF)
ODDIE_FLOAT64_FUNC(CapControlsF)
ODDIE_FLOAT64_FUNC(CktElementF)
ODDIE_FLOAT64_FUNC(DSSLoadsF)
ODDIE_FLOAT64_FUNC(FusesF)
ODDIE_FLOAT64_FUNC(GeneratorsF)
ODDIE_FLOAT64_FUNC(GICSourcesF)
ODDIE_FLOAT64_FUNC(IsourceF)
ODDIE_FLOAT64_FUNC(LineCodesF)
ODDIE_FLOAT64_FUNC(LinesF)
ODDIE_FLOAT64_FUNC(LoadShapeF)
ODDIE_FLOAT64_FUNC(MetersF)
ODDIE_FLOAT64_FUNC(ParserF)
ODDIE_FLOAT64_FUNC(PDElementsF)
ODDIE_FLOAT64_FUNC(PVsystemsF)
ODDIE_FLOAT64_FUNC(ReactorsF)
ODDIE_FLOAT64_FUNC(ReclosersF)
ODDIE_FLOAT64_FUNC(ReduceCktF)
ODDIE_FLOAT64_FUNC(RegControlsF)
ODDIE_FLOAT64_FUNC(SensorsF)
ODDIE_FLOAT64_FUNC(SettingsF)
ODDIE_FLOAT64_FUNC(SolutionF)
ODDIE_FLOAT64_FUNC(StoragesF)
ODDIE_FLOAT64_FUNC(SwtControlsF)
ODDIE_FLOAT64_FUNC(TransformersF)
ODDIE_FLOAT64_FUNC(VsourcesF)
ODDIE_FLOAT64_FUNC(WindGensF)
ODDIE_FLOAT64_FUNC(XYCurvesF)

ODDIE_FLOAT64_FUNC2(CircuitF)
ODDIE_FLOAT64_FUNC2(CmathLibF)

ODDIE_INT32_FUNC(ActiveClassI)
ODDIE_INT32_FUNC(BUSI)
ODDIE_INT32_FUNC(CapacitorsI)
ODDIE_INT32_FUNC(CapControlsI)
ODDIE_INT32_FUNC(CircuitI)
ODDIE_INT32_FUNC(CktElementI)
ODDIE_INT32_FUNC(CtrlQueueI)
ODDIE_INT32_FUNC(DSSElementI)
ODDIE_INT32_FUNC(DSSExecutiveI)
ODDIE_INT32_FUNC(DSSI)
ODDIE_INT32_FUNC(DSSLoads)
ODDIE_INT32_FUNC(DSSProgressI)
ODDIE_INT32_FUNC(FusesI)
ODDIE_INT32_FUNC(GeneratorsI)
ODDIE_INT32_FUNC(GICSourcesI)
ODDIE_INT32_FUNC(IsourceI)
ODDIE_INT32_FUNC(LineCodesI)
ODDIE_INT32_FUNC(LinesI)
ODDIE_INT32_FUNC(LoadShapeI)
ODDIE_INT32_FUNC(MetersI)
ODDIE_INT32_FUNC(MonitorsI)
ODDIE_INT32_FUNC(ParallelI)
ODDIE_INT32_FUNC(ParserI)
ODDIE_INT32_FUNC(PDElementsI)
ODDIE_INT32_FUNC(PVsystemsI)
ODDIE_INT32_FUNC(ReactorsI)
ODDIE_INT32_FUNC(ReclosersI)
ODDIE_INT32_FUNC(ReduceCktI)
ODDIE_INT32_FUNC(RegControlsI)
ODDIE_INT32_FUNC(RelaysI)
ODDIE_INT32_FUNC(SensorsI)
ODDIE_INT32_FUNC(SettingsI)
ODDIE_INT32_FUNC(SolutionI)
ODDIE_INT32_FUNC(StoragesI)
ODDIE_INT32_FUNC(SwtControlsI)
ODDIE_INT32_FUNC(TopologyI)
ODDIE_INT32_FUNC(TransformersI)
ODDIE_INT32_FUNC(VsourcesI)
ODDIE_INT32_FUNC(WindGensI)
ODDIE_INT32_FUNC(XYCurvesI)

ODDIE_VARIANT_FUNC(ActiveClassV)
ODDIE_VARIANT_FUNC(BUSV)
ODDIE_VARIANT_FUNC(CapacitorsV)
ODDIE_VARIANT_FUNC(CapControlsV)
ODDIE_VARIANT_FUNC(CircuitV)
ODDIE_VARIANT_FUNC(CktElementV)
ODDIE_VARIANT_FUNC(CmathLibV)
ODDIE_VARIANT_FUNC(CtrlQueueV)
ODDIE_VARIANT_FUNC(DSSElementV)
ODDIE_VARIANT_FUNC(DSSLoadsV)
ODDIE_VARIANT_FUNC(DSSV)
ODDIE_VARIANT_FUNC(FusesV)
ODDIE_VARIANT_FUNC(GeneratorsV)
ODDIE_VARIANT_FUNC(GICSourcesV)
ODDIE_VARIANT_FUNC(IsourceV)
ODDIE_VARIANT_FUNC(LineCodesV)
ODDIE_VARIANT_FUNC(LinesV)
ODDIE_VARIANT_FUNC(LoadShapeV)
ODDIE_VARIANT_FUNC(MetersV)
ODDIE_VARIANT_FUNC(MonitorsV)
ODDIE_VARIANT_FUNC(ParallelV)
ODDIE_VARIANT_FUNC(ParserV)
ODDIE_VARIANT_FUNC(PVsystemsV)
ODDIE_VARIANT_FUNC(ReactorsV)
ODDIE_VARIANT_FUNC(ReclosersV)
ODDIE_VARIANT_FUNC(RegControlsV)
ODDIE_VARIANT_FUNC(RelaysV)
ODDIE_VARIANT_FUNC(SensorsV)
ODDIE_VARIANT_FUNC(SettingsV)
ODDIE_VARIANT_FUNC(SolutionV)
ODDIE_VARIANT_FUNC(StoragesV)
ODDIE_VARIANT_FUNC(SwtControlsV)
ODDIE_VARIANT_FUNC(TopologyV)
ODDIE_VARIANT_FUNC(TransformersV)
ODDIE_VARIANT_FUNC(VsourcesV)
ODDIE_VARIANT_FUNC(WindGensV)
ODDIE_VARIANT_FUNC(XYCurvesV)

ODDIE_ODD_EXTERN void DSSDisposeString(const char *a);

#endif