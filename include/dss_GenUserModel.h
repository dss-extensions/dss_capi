#include "dss_UserModels.h"

// These functions need to be implemented
DSS_MODEL_DLL(int32_t) New(struct TGeneratorVars* GenData, struct TDynamicsRec* DynaData, struct TDSSCallBacks* CallBacks);
DSS_MODEL_DLL(void) Delete(int32_t *ID);
DSS_MODEL_DLL(int32_t) Select(int32_t *ID);
DSS_MODEL_DLL(void) Init(double *V, double *I);
DSS_MODEL_DLL(void) Calc(double *V, double *I);
DSS_MODEL_DLL(void) Integrate(void);
DSS_MODEL_DLL(void) Edit(char *EditStr, uint32_t MaxLen);
DSS_MODEL_DLL(void) UpdateModel(void);
DSS_MODEL_DLL(int32_t) NumVars(void);
DSS_MODEL_DLL(void) GetAllVars(double *vars);
DSS_MODEL_DLL(double) GetVariable(int32_t *i);
DSS_MODEL_DLL(void) SetVariable(int32_t *i, double *value);
DSS_MODEL_DLL(void) GetVarName(int32_t *i, char *VarName, uint32_t MaxLen);
DSS_MODEL_DLL(void) Save(void);
DSS_MODEL_DLL(void) Restore(void);
