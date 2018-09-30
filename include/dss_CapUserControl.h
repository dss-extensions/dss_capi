#include "dss_UserModels.h"

// These functions need to be implemented
DSS_MODEL_DLL(int32_t) New(struct TDSSCallBacks* CallBacks);
DSS_MODEL_DLL(void) Delete(int32_t *ID);
DSS_MODEL_DLL(int32_t) Select(int32_t *ID);
DSS_MODEL_DLL(void) UpdateModel(void);
DSS_MODEL_DLL(void) Sample(void);
DSS_MODEL_DLL(void) DoPending(int32_t *Code, int32_t *ProxyHdl);
DSS_MODEL_DLL(void) Edit(char *EditStr, uint32_t MaxLen);


