/*! \file altdss_obj_capi.h */
#ifndef ALTDSS_OBJ_CAPI_DLL_H
#define ALTDSS_OBJ_CAPI_DLL_H

#ifdef __cplusplus
#ifdef ALTDSS_CAPI_NAMESPACE
namespace altdss { namespace capi {
#endif
extern "C" {
#else
#endif

    ALTDSS_CAPI_DLL void* Obj_New(const void* ctx, int32_t ClsIdx, const char* Name, uint16_t Activate, uint16_t BeginEdit);
    ALTDSS_CAPI_DLL int32_t Obj_GetCount(const void* ctx, int32_t ClsIdx);
    ALTDSS_CAPI_DLL void** Obj_GetListPointer(const void* ctx, int32_t ClsIdx);
    ALTDSS_CAPI_DLL void* Obj_GetHandleByName(const void* ctx, int32_t ClsIdx, const char* Name);
    ALTDSS_CAPI_DLL void* Obj_GetHandleByIdx(const void* ctx, int32_t ClsIdx, int32_t Idx);
    ALTDSS_CAPI_DLL uint16_t Obj_PropertySideEffects(void *obj, int32_t Index, int32_t PreviousInt, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Obj_BeginEdit(void *obj);
    ALTDSS_CAPI_DLL void Obj_EndEdit(void *obj, int32_t NumChanges);
    ALTDSS_CAPI_DLL int32_t Obj_GetNumProperties(void *obj);
    
    /*! 
    Returns an element's data as a JSON-encoded string.

    The `options` parameter contains bit-flags to toggle specific features.

    By default, only the properties explicitly set. The properties are returned in the order they are set in the input.
    As a reminder, OpenDSS is sensitive to the order of the properties.

    The `options` bit-flags are available in the `DSSJSONFlags` enum.

    (API Extension)
    */
    ALTDSS_CAPI_DLL const char* Obj_ToJSON(void *obj, uint32_t options);

    /*! 
    Returns the data (as a list) of the elements in a batch as a JSON-encoded string.

    The `options` parameter contains bit-flags to toggle specific features.
    See `Obj_ToJSON` for more. 
    
    Additionally, the `ExcludeDisabled` flag can be used to excluded disabled elements from the output.

    (API Extension)
    */
    ALTDSS_CAPI_DLL const char* Batch_ToJSON(void** batch, int32_t batchSize, uint32_t options);

    /*! 
    Returns the object name (direct access, no copy is done, no disposal required by the user; read only!)

    (API Extension)
    */
    ALTDSS_CAPI_DLL const char* Obj_GetName(void *obj);

    /*! 
    Returns a copy of the full object name, including class.
    
    Remember to dispose with `DSS_Dispose_String`.

    (API Extension)
    */
    ALTDSS_CAPI_DLL const char* Obj_GetFullName(void *obj);

    /*! 
    Returns the object's class name (direct access, no copy is done, no disposal required by the user; read only!)

    (API Extension)
    */
    ALTDSS_CAPI_DLL const char* Obj_GetClassName(void *obj);


    ALTDSS_CAPI_DLL int32_t Obj_GetIdx(void *obj);
    ALTDSS_CAPI_DLL int32_t Obj_GetClassIdx(void *obj);

    /*! 
    Activates an object. The object is set as the current
    active DSSObject or CktElement, and in the list of its parent class.
    If allLists is true, other internal lists of OpenDSS are also
    updated (implies slow/linear searches).

    (API Extension)
    */
    ALTDSS_CAPI_DLL void Obj_Activate(void *obj, altdss_bool_t allLists);

    /*! 
    Returns the pointer to the internal property fill sequence.
    
    First value (index 0) is what was previously known as "CurrentCount".
    Properties start at index 1.

    (API Extension)
    */
    ALTDSS_CAPI_DLL int32_t* Obj_GetPropSeqPtr(void *obj);

    /*!
    Copy of the internal flags (bitset from DSSObjectFlags) of a DSS object -- for expert users
    */
    ALTDSS_CAPI_DLL uint32_t Obj_GetFlags(void *obj);

    /*!
    Replace the internal flags of a DSS object -- for expert users
    */
    ALTDSS_CAPI_DLL void Obj_SetFlags(void *obj, uint32_t flags);

    ALTDSS_CAPI_DLL double Obj_GetFloat64(void *obj, int32_t Index);
    ALTDSS_CAPI_DLL int32_t Obj_GetInt32(void *obj, int32_t Index);
    ALTDSS_CAPI_DLL void* Obj_GetObject(void *obj, int32_t Index);
    
    // Note: strings returned by these two must be disposed with DSS_Dispose_String
    ALTDSS_CAPI_DLL const char* Obj_GetString(void *obj, int32_t Index);
    ALTDSS_CAPI_DLL const char* Obj_GetAsString(void *obj, int32_t Index);

    ALTDSS_CAPI_DLL void Obj_GetFloat64Array(double** ResultPtr, int32_t* ResultDims, void *obj, int32_t Index);
    ALTDSS_CAPI_DLL double Obj_GetFloat64ArrayElement(void *obj, int32_t Index, int32_t ElementIndex);
    ALTDSS_CAPI_DLL void Obj_GetInt32Array(int32_t** ResultPtr, int32_t* ResultDims, void *obj, int32_t Index);
    ALTDSS_CAPI_DLL int32_t Obj_GetInt32ArrayElement(void *obj, int32_t Index, int32_t ElementIndex);
    ALTDSS_CAPI_DLL void Obj_GetStringArray(char*** ResultPtr, int32_t* ResultDims, void *obj, int32_t Index);
    ALTDSS_CAPI_DLL const char* Obj_GetStringArrayElement(void *obj, int32_t Index, int32_t ElementIndex);
    ALTDSS_CAPI_DLL void Obj_GetObjectArray(void*** ResultPtr, int32_t* ResultDims, void *obj, int32_t Index);
    ALTDSS_CAPI_DLL void* Obj_GetObjectArrayElement(void *obj, int32_t Index, int32_t ElementIndex);

    ALTDSS_CAPI_DLL void Obj_SetAsString(void *obj, int32_t Index, const char* Value, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Obj_SetFloat64(void *obj, int32_t Index, double Value, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Obj_SetInt32(void *obj, int32_t Index, int32_t Value, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Obj_SetString(void *obj, int32_t Index, const char* Value, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Obj_SetObject(void *obj, int32_t Index, void* Value, uint32_t setterFlags);
    
    ALTDSS_CAPI_DLL void Obj_SetFloat64Array(void *obj, int32_t Index, double* Value, int32_t ValueCount, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Obj_SetFloat64ArrayElement(void *obj, int32_t Index, double ElementIndex, double Value, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Obj_SetInt32Array(void *obj, int32_t Index, int32_t* Value, int32_t ValueCount, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Obj_SetInt32ArrayElement(void *obj, int32_t Index, int32_t ElementIndex, int32_t Value, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Obj_SetStringArray(void *obj, int32_t Index, const char** Value, int32_t ValueCount, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Obj_SetStringArrayElement(void *obj, int32_t Index, int32_t ElementIndex, const char* Value, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Obj_SetObjectArray(void *obj, int32_t Index, void **Value, int32_t ValueCount, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Obj_SetObjectArrayElement(void *obj, int32_t Index, int32_t ElementIndex, void *Value, uint32_t setterFlags);

    ALTDSS_CAPI_DLL double Obj_CktElement_MaxCurrent(void *obj, int32_t terminalIdx);
    ALTDSS_CAPI_DLL void Obj_Circuit_Set_ActiveCktElement(void *obj);

    ALTDSS_CAPI_DLL void Batch_Dispose(void** batch);
    ALTDSS_CAPI_DLL void Batch_BeginEdit(void** batch, int32_t batchSize);
    ALTDSS_CAPI_DLL void Batch_EndEdit(void** batch, int32_t batchSize, int32_t numEdits);
    ALTDSS_CAPI_DLL void Batch_GetPropSeq(int32_t** ResultPtr, int32_t* ResultDims, void** batch, int32_t batchSize);

    ALTDSS_CAPI_DLL void Batch_CreateFromNew(const void* ctx, void*** ResultPtr, int32_t* ResultDims, int32_t clsid, const char** names, int32_t count, altdss_bool_t BeginEdit);
    ALTDSS_CAPI_DLL void Batch_CreateByClass(const void* ctx, void*** ResultPtr, int32_t* ResultDims, int32_t clsidx);
    ALTDSS_CAPI_DLL void Batch_CreateByRegExp(const void* ctx, void*** ResultPtr, int32_t* ResultDims, int32_t clsidx, const char* re);
    ALTDSS_CAPI_DLL void Batch_CreateByIndex(const void* ctx, void*** ResultPtr, int32_t* ResultDims, int32_t clsidx, int32_t* Value, int32_t ValueCount);
    ALTDSS_CAPI_DLL void Batch_CreateByInt32Property(const void* ctx, void*** ResultPtr, int32_t* ResultDims, int32_t ClsIdx, int32_t idx, int32_t value);
    ALTDSS_CAPI_DLL void Batch_CreateByFloat64PropertyRange(const void* ctx, void*** ResultPtr, int32_t* ResultDims, int32_t ClsIdx, int32_t idx, double valueMin, double valueMax);
    ALTDSS_CAPI_DLL void Batch_FilterByInt32Property(const void* ctx, void*** ResultPtr, int32_t* ResultDims, void** batch, int32_t batchSize, int32_t idx, int32_t value);
    ALTDSS_CAPI_DLL void Batch_FilterByFloat64PropertyRange(const void* ctx, void*** ResultPtr, int32_t* ResultDims, void** batch, int32_t batchSize, int32_t idx, double valueMin, double valueMax);

    ALTDSS_CAPI_DLL void Batch_GetFloat64(double** ResultPtr, int32_t* ResultDims, void** batch, int32_t batchSize, int32_t Index);
    ALTDSS_CAPI_DLL void Batch_GetFloat64FromFunc(double** ResultPtr, int32_t* ResultDims, void** batch, int32_t batchSize, dss_obj_float64_func_t func);
    ALTDSS_CAPI_DLL void Batch_GetFloat64FromFunc2(double** ResultPtr, int32_t* ResultDims, void** batch, int32_t batchSize, dss_obj_float64_int32_func_t func, int32_t funcArg);
    ALTDSS_CAPI_DLL void Batch_GetInt32(int32_t** ResultPtr, int32_t* ResultDims, void** batch, int32_t batchSize, int32_t Index);
    ALTDSS_CAPI_DLL void Batch_GetInt32FromFunc(int32_t** ResultPtr, int32_t* ResultDims, void** batch, int32_t batchSize, dss_obj_int32_func_t func);
    ALTDSS_CAPI_DLL void Batch_GetString(char*** ResultPtr, int32_t* ResultDims, void** batch, int32_t batchSize, int32_t Index);
    ALTDSS_CAPI_DLL void Batch_GetAsString(char*** ResultPtr, int32_t* ResultDims, void** batch, int32_t batchSize, int32_t Index);

    ALTDSS_CAPI_DLL void Batch_GetObject(void*** ResultPtr, int32_t* ResultDims, void** batch, int32_t batchSize, int32_t Index);

    // ALTDSS_CAPI_DLL void Batch_SetAsString(void** batch, int32_t batchSize, int32_t Index, const char* Value);
    ALTDSS_CAPI_DLL void Batch_Float64(void** batch, int32_t batchSize, int32_t Index, int32_t Operation, double Value, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Batch_Int32(void** batch, int32_t batchSize, int32_t Index, int32_t Operation, int32_t Value, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Batch_SetString(void** batch, int32_t batchSize, int32_t Index, const char* Value, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Batch_SetObject(void** batch, int32_t batchSize, int32_t Index, const void *Value, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Batch_Float64Array(void** batch, int32_t batchSize, int32_t Index, int32_t Operation, double* Value, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Batch_Int32Array(void** batch, int32_t batchSize, int32_t Index, int32_t Operation, int32_t* Value, uint32_t setterFlags);

    /*!
    DEPRECATED: use `Batch_Float64Array` with `Operation=BatchOperation_Set` instead
    */
    ALTDSS_CAPI_DLL void Batch_SetFloat64Array(void** batch, int32_t batchSize, int32_t Index, double* Value, uint32_t setterFlags);

    /*!
    DEPRECATED: use `Batch_Int32Array` with `Operation=BatchOperation_Set` instead
    */
    ALTDSS_CAPI_DLL void Batch_SetInt32Array(void** batch, int32_t batchSize, int32_t Index, int32_t* Value, uint32_t setterFlags);

    ALTDSS_CAPI_DLL void Batch_SetStringArray(void** batch, int32_t batchSize, int32_t Index, const char** Value, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Batch_SetObjectArray(void** batch, int32_t batchSize, int32_t Index, const void** Value, uint32_t setterFlags);

    ALTDSS_CAPI_DLL void Batch_CreateFromNewS(const void* ctx, void*** ResultPtr, int32_t* ResultDims, const char* clsname, const char** names, int32_t count, altdss_bool_t BeginEdit);
    ALTDSS_CAPI_DLL void Batch_CreateByClassS(const void* ctx, void*** ResultPtr, int32_t* ResultDims, const char* clsname);
    ALTDSS_CAPI_DLL void Batch_CreateByRegExpS(const void* ctx, void*** ResultPtr, int32_t* ResultDims, const char* clsname, const char* re);
    ALTDSS_CAPI_DLL void Batch_CreateByIndexS(const void* ctx, void*** ResultPtr, int32_t* ResultDims, const char* clsname, int32_t* Value, int32_t ValueCount);
    ALTDSS_CAPI_DLL void Batch_CreateByInt32PropertyS(const void* ctx, void*** ResultPtr, int32_t* ResultDims, const char* clsname, const char* Name, int32_t value);
    ALTDSS_CAPI_DLL void Batch_CreateByFloat64PropertyRangeS(const void* ctx, void*** ResultPtr, int32_t* ResultDims, const char* clsname, const char* Name, double valueMin, double valueMax);

    ALTDSS_CAPI_DLL void Batch_GetFloat64S(double** ResultPtr, int32_t* ResultDims, void** batch, int32_t batchSize, const char* Name);
    ALTDSS_CAPI_DLL void Batch_GetInt32S(int32_t** ResultPtr, int32_t* ResultDims, void** batch, int32_t batchSize, const char* Name);
    ALTDSS_CAPI_DLL void Batch_GetStringS(char*** ResultPtr, int32_t* ResultDims, void** batch, int32_t batchSize, const char* Name);
    ALTDSS_CAPI_DLL void Batch_GetAsStringS(char*** ResultPtr, int32_t* ResultDims, void** batch, int32_t batchSize, const char* Name);

    ALTDSS_CAPI_DLL void Batch_GetObjectS(void*** ResultPtr, int32_t* ResultDims, void** batch, int32_t batchSize, const char* Name);

    // ALTDSS_CAPI_DLL void Batch_SetAsStringS(void** batch, int32_t batchSize, const char* Name, const char* Value);
    ALTDSS_CAPI_DLL void Batch_Float64S(void** batch, int32_t batchSize, const char* Name, int32_t Operation, double Value, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Batch_Int32S(void** batch, int32_t batchSize, const char* Name, int32_t Operation, int32_t Value, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Batch_SetStringS(void** batch, int32_t batchSize, const char* Name, const char* Value, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Batch_SetObjectS(void** batch, int32_t batchSize, const char* Name, const void* Value, uint32_t setterFlags);

    ALTDSS_CAPI_DLL void Batch_Float64ArrayS(void** batch, int32_t batchSize, const char* Name, int32_t Operation, double* Value, uint32_t setterFlags);
    /*!
    DEPRECATED: use `Batch_Int32ArrayS` with `Operation=BatchOperation_Set` instead
    */
    ALTDSS_CAPI_DLL void Batch_SetFloat64ArrayS(void** batch, int32_t batchSize, const char* Name, double* Value, uint32_t setterFlags);

    ALTDSS_CAPI_DLL void Batch_Int32ArrayS(void** batch, int32_t batchSize, const char* Name, int32_t Operation, int32_t* Value, uint32_t setterFlags);
    /*!
    DEPRECATED: use `Batch_Int32ArrayS` with `Operation=BatchOperation_Set` instead
    */
    ALTDSS_CAPI_DLL void Batch_SetInt32ArrayS(void** batch, int32_t batchSize, const char* Name, int32_t* Value, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Batch_SetStringArrayS(void** batch, int32_t batchSize, const char* Name, const char** Value, uint32_t setterFlags);
    ALTDSS_CAPI_DLL void Batch_SetObjectArrayS(void** batch, int32_t batchSize, const char* Name, const void** Value, uint32_t setterFlags);



    // Relevant functions from the CktElement and PDElements API, working directly on the elements
    //TODO: copy comments and adapt

    ALTDSS_CAPI_DLL void Alt_CE_Get_BusNames(char*** resultPtr, int32_t *resultDims, void* elem, int32_t removeNodes);
    ALTDSS_CAPI_DLL int32_t Alt_CE_Get_NumConductors(void* elem);
    ALTDSS_CAPI_DLL int32_t Alt_CE_Get_NumPhases(void* elem);
    ALTDSS_CAPI_DLL int32_t Alt_CE_Get_NumTerminals(void* elem);
    ALTDSS_CAPI_DLL void Alt_CE_Set_BusNames(void* elem, const char** valuePtr, int32_t valueCount);
    ALTDSS_CAPI_DLL void Alt_CE_Get_Currents(double** resultPtr, int32_t *resultDims, void* elem);
    ALTDSS_CAPI_DLL void Alt_CE_Get_Voltages(double** resultPtr, int32_t *resultDims, void* elem);
    ALTDSS_CAPI_DLL void Alt_CE_Get_Losses(double** resultPtr, int32_t *resultDims, void* elem);
    ALTDSS_CAPI_DLL void Alt_CE_Get_PhaseLosses(double** resultPtr, int32_t *resultDims, void* elem);
    ALTDSS_CAPI_DLL void Alt_CE_Get_Powers(double** resultPtr, int32_t *resultDims, void* elem);
    ALTDSS_CAPI_DLL void Alt_CE_Get_SeqCurrents(double** resultPtr, int32_t *resultDims, void* elem);
    ALTDSS_CAPI_DLL void Alt_CE_Get_SeqPowers(double** resultPtr, int32_t *resultDims, void* elem);
    ALTDSS_CAPI_DLL void Alt_CE_Get_SeqVoltages(double** resultPtr, int32_t *resultDims, void* elem);
    ALTDSS_CAPI_DLL void Alt_CE_Close(void* elem, int32_t terminal, int32_t phase);
    ALTDSS_CAPI_DLL void Alt_CE_Open(void* elem, int32_t terminal, int32_t phase);
    ALTDSS_CAPI_DLL altdss_bool_t Alt_CE_IsOpen(void* elem, int32_t terminal, int32_t phase);
    ALTDSS_CAPI_DLL void Alt_CE_Get_Residuals(double** resultPtr, int32_t *resultDims, void* elem);
    ALTDSS_CAPI_DLL int32_t Alt_CE_Get_YPrimOrder(void* elem);
    ALTDSS_CAPI_DLL void Alt_CE_Get_YPrim(double** resultPtr, int32_t *resultDims, void* elem);
    ALTDSS_CAPI_DLL int32_t Alt_CE_Get_Handle(void* elem);
    ALTDSS_CAPI_DLL void Alt_CE_Get_Controllers(void*** resultPtr, int32_t* resultDims, void* elem);
    ALTDSS_CAPI_DLL altdss_bool_t Alt_CE_Get_HasVoltControl(void* elem);
    ALTDSS_CAPI_DLL altdss_bool_t Alt_CE_Get_HasSwitchControl(void* elem);
    ALTDSS_CAPI_DLL void Alt_CE_Get_ComplexSeqVoltages(double** resultPtr, int32_t *resultDims, void* elem);
    ALTDSS_CAPI_DLL void Alt_CE_Get_ComplexSeqCurrents(double** resultPtr, int32_t *resultDims, void* elem);
    ALTDSS_CAPI_DLL void Alt_CE_Get_NodeOrder(int32_t** resultPtr, int32_t *resultDims, void* elem);
    ALTDSS_CAPI_DLL altdss_bool_t Alt_CE_Get_HasOCPDevice(void* elem);
    ALTDSS_CAPI_DLL int32_t Alt_CE_Get_NumControllers(void* elem);
    ALTDSS_CAPI_DLL void* Alt_CE_Get_OCPDevice(void* elem);
    ALTDSS_CAPI_DLL int32_t Alt_CE_Get_OCPDeviceIndex(void* elem);
    ALTDSS_CAPI_DLL int32_t Alt_CE_Get_OCPDeviceType(void* elem);
    ALTDSS_CAPI_DLL void Alt_CE_Get_CurrentsMagAng(double** resultPtr, int32_t *resultDims, void* elem);
    ALTDSS_CAPI_DLL void Alt_CE_Get_VoltagesMagAng(double** resultPtr, int32_t *resultDims, void* elem);
    ALTDSS_CAPI_DLL void Alt_CE_Get_TotalPowers(double** resultPtr, int32_t *resultDims, void* elem);
    ALTDSS_CAPI_DLL altdss_bool_t Alt_CE_Get_IsIsolated(void* elem);
    ALTDSS_CAPI_DLL void Alt_CE_Get_NodeRef(int32_t** resultPtr, int32_t *resultDims, void* elem);
    ALTDSS_CAPI_DLL const char* Alt_CE_Get_DisplayName(void* pce);
    ALTDSS_CAPI_DLL const char* Alt_CE_Get_GUID(void* elem);
    ALTDSS_CAPI_DLL void Alt_CE_Set_DisplayName(void* elem, const char* value);
    ALTDSS_CAPI_DLL double Alt_CE_MaxCurrent(void* elem, int32_t terminalIdx);
    ALTDSS_CAPI_DLL void Alt_PCE_Get_VariableNames(char*** resultPtr, int32_t *resultDims, void* pce);
    ALTDSS_CAPI_DLL void Alt_PCE_Get_VariableValues(double** resultPtr, int32_t *resultDims, void* pce);
    ALTDSS_CAPI_DLL void Alt_PCE_Set_VariableValue(void* pce, int32_t varIdx, double value);
    ALTDSS_CAPI_DLL double Alt_PCE_Get_VariableValue(void* pce, int32_t varIdx);
    ALTDSS_CAPI_DLL void Alt_PCE_Set_VariableSValue(void* pce, const char* varName, double value);
    ALTDSS_CAPI_DLL double Alt_PCE_Get_VariableSValue(void* pce, const char* varName);
    ALTDSS_CAPI_DLL const char* Alt_PCE_Get_VariableName(void* pce, int32_t varIdx);
    ALTDSS_CAPI_DLL void* Alt_PCE_Get_EnergyMeter(void* elem);
    ALTDSS_CAPI_DLL const char* Alt_PCE_Get_EnergyMeterName(void* elem);
    ALTDSS_CAPI_DLL void Alt_CE_Get_RegisterNames(char*** resultPtr, int32_t *resultDims, void* pce);
    ALTDSS_CAPI_DLL void Alt_CE_Get_RegisterValues(double** resultPtr, int32_t *resultDims, void* pce);
    ALTDSS_CAPI_DLL void Alt_CEBatch_Get_Losses(double** resultPtr, int32_t *resultDims, void** batch, int32_t batchSize);
    ALTDSS_CAPI_DLL void Alt_CEBatch_Get_PhaseLosses(double** resultPtr, int32_t *resultDims, void** batch, int32_t batchSize);
    ALTDSS_CAPI_DLL void Alt_CEBatch_Get_Powers(double** resultPtr, int32_t *resultDims, void** batch, int32_t batchSize);
    ALTDSS_CAPI_DLL void Alt_CEBatch_Get_SeqPowers(double** resultPtr, int32_t *resultDims, void** batch, int32_t batchSize);
    ALTDSS_CAPI_DLL void Alt_CEBatch_Get_TotalPowers(double** resultPtr, int32_t *resultDims, void** batch, int32_t batchSize);
    ALTDSS_CAPI_DLL void Alt_CEBatch_Get_SeqCurrents(double** resultPtr, int32_t *resultDims, void** batch, int32_t batchSize);
    ALTDSS_CAPI_DLL void Alt_CEBatch_Get_ComplexSeqCurrents(double** resultPtr, int32_t *resultDims, void** batch, int32_t batchSize);
    ALTDSS_CAPI_DLL void Alt_CEBatch_Get_Currents(double** resultPtr, int32_t *resultDims, void** batch, int32_t batchSize);
    ALTDSS_CAPI_DLL void Alt_CEBatch_Get_CurrentsMagAng(double** resultPtr, int32_t *resultDims, void** batch, int32_t batchSize);
    ALTDSS_CAPI_DLL void Alt_CEBatch_Get_SeqVoltages(double** resultPtr, int32_t *resultDims, void** batch, int32_t batchSize);
    ALTDSS_CAPI_DLL void Alt_CEBatch_Get_ComplexSeqVoltages(double** resultPtr, int32_t *resultDims, void** batch, int32_t batchSize);
    ALTDSS_CAPI_DLL void Alt_CEBatch_Get_Voltages(double** resultPtr, int32_t *resultDims, void** batch, int32_t batchSize);
    ALTDSS_CAPI_DLL void Alt_CEBatch_Get_VoltagesMagAng(double** resultPtr, int32_t *resultDims, void** batch, int32_t batchSize);
    
    ALTDSS_CAPI_DLL altdss_bool_t Alt_PDE_Get_IsShunt(void* pde);
    ALTDSS_CAPI_DLL double Alt_PDE_Get_AccumulatedL(void* pde);
    ALTDSS_CAPI_DLL double Alt_PDE_Get_Lambda(void* pde);
    ALTDSS_CAPI_DLL int32_t Alt_PDE_Get_NumCustomers(void* pde);
    ALTDSS_CAPI_DLL void* Alt_PDE_Get_ParentPDElement(void* pde);
    ALTDSS_CAPI_DLL int32_t Alt_PDE_Get_TotalCustomers(void* pde);
    ALTDSS_CAPI_DLL int32_t Alt_PDE_Get_FromTerminal(void* pde);
    ALTDSS_CAPI_DLL double Alt_PDE_Get_TotalMiles(void* pde);
    ALTDSS_CAPI_DLL int32_t Alt_PDE_Get_SectionID(void* pde);
    ALTDSS_CAPI_DLL void* Alt_PDE_Get_EnergyMeter(void* elem);
    ALTDSS_CAPI_DLL const char* Alt_PDE_Get_EnergyMeterName(void* elem);
    // ALTDSS_CAPI_DLL double Alt_PDE_Get_MaxCurrent(void* elem, altdss_bool_t allNodes);
    ALTDSS_CAPI_DLL double Alt_PDE_Get_pctNorm(void* elem, altdss_bool_t allNodes);
    ALTDSS_CAPI_DLL double Alt_PDE_Get_pctEmerg(void* elem, altdss_bool_t allNodes);
    // ALTDSS_CAPI_DLL void Alt_PDEBatch_Get_MaxCurrent(double** resultPtr, int32_t* resultDims, void** batch, int32_t batchSize, altdss_bool_t allNodes);
    ALTDSS_CAPI_DLL void Alt_PDEBatch_Get_pctNorm(double** resultPtr, int32_t* resultDims, void** batch, int32_t batchSize, altdss_bool_t allNodes);
    ALTDSS_CAPI_DLL void Alt_PDEBatch_Get_pctEmerg(double** resultPtr, int32_t* resultDims, void** batch, int32_t batchSize, altdss_bool_t allNodes);

    ALTDSS_CAPI_DLL void Alt_LoadShape_Set_Points(void *objPtr, int32_t Npts, void *HoursPtr, void *PMultPtr, void *QMultPtr, altdss_bool_t ExternalMemory, altdss_bool_t IsFloat32, int32_t Stride);
    ALTDSS_CAPI_DLL void Alt_LoadShape_UseFloat64(void *objPtr);
    ALTDSS_CAPI_DLL void Alt_LoadShape_UseFloat32(void *objPtr);

    ALTDSS_CAPI_DLL void Alt_Monitor_Get_ByteStream(int8_t** resultPtr, int32_t* resultDims, void* pmon);
    ALTDSS_CAPI_DLL int32_t Alt_Monitor_Get_SampleCount(void* pmon);
    ALTDSS_CAPI_DLL const char* Alt_Monitor_Get_FileName(void* pmon);
    ALTDSS_CAPI_DLL int32_t Alt_Monitor_Get_NumChannels(void* pmon);
    ALTDSS_CAPI_DLL int32_t Alt_Monitor_Get_RecordSize(void* pmon);
    ALTDSS_CAPI_DLL void Alt_Monitor_Show(void* pmon);
    ALTDSS_CAPI_DLL void Alt_Monitor_Get_Channel(double** resultPtr, int32_t *resultDims, void* pmon, int32_t index);
    ALTDSS_CAPI_DLL void Alt_Monitor_Get_dblFreq(double** resultPtr, int32_t *resultDims, void* pmon);
    ALTDSS_CAPI_DLL void Alt_Monitor_Get_dblHour(double** resultPtr, int32_t *resultDims, void* pmon);
    ALTDSS_CAPI_DLL void Alt_Monitor_Get_Header(char*** resultPtr, int32_t *resultDims, void* pmon);

    ALTDSS_CAPI_DLL void Alt_Transformer_Get_WdgVoltages(double** resultPtr, int32_t *resultDims, void* elem, int32_t winding);
    ALTDSS_CAPI_DLL void Alt_Transformer_Get_WdgCurrents(double** resultPtr, int32_t *resultDims, void* elem);
    ALTDSS_CAPI_DLL void Alt_Transformer_Get_LossesByType(double** resultPtr, int32_t *resultDims, void* elem);

    ALTDSS_CAPI_DLL int32_t Alt_Meter_Get_TotalCustomers(void* elem);
    ALTDSS_CAPI_DLL int32_t Alt_Meter_Get_NumEndElements(void* elem);
    ALTDSS_CAPI_DLL int32_t Alt_Meter_Get_NumSections(void* elem);
    ALTDSS_CAPI_DLL int32_t Alt_Meter_Get_NumBranchesInZone(void* elem);

    ALTDSS_CAPI_DLL void Alt_Meter_Get_CalcCurrent(double** resultPtr, int32_t *resultDims, void* elem);
    ALTDSS_CAPI_DLL void Alt_Meter_Set_CalcCurrent(void* elem, const double* ValuePtr, int32_t valueCount);
    ALTDSS_CAPI_DLL void Alt_Meter_Get_AllocFactors(double** resultPtr, int32_t *resultDims, void* elem);
    ALTDSS_CAPI_DLL void Alt_Meter_Set_AllocFactors(void* elem, const double* ValuePtr, int32_t valueCount);
    ALTDSS_CAPI_DLL void Alt_Meter_DoReliabilityCalc(void* elem, altdss_bool_t assumeRestoration);

    ALTDSS_CAPI_DLL void Alt_Meter_Get_ZonePCEs(void*** resultPtr, int32_t* resultDims, void* elem);
    ALTDSS_CAPI_DLL void Alt_Meter_Get_EndElements(void*** resultPtr, int32_t* resultDims, void* elem);
    ALTDSS_CAPI_DLL void Alt_Meter_Get_BranchesInZone(void*** resultPtr, int32_t* resultDims, void* elem);
    ALTDSS_CAPI_DLL void Alt_Meter_Get_SequenceList(void*** resultPtr, int32_t* resultDims, void* elem);
    ALTDSS_CAPI_DLL void Alt_Meter_Get_Loads(void*** resultPtr, int32_t* resultDims, void* elem);

    ALTDSS_CAPI_DLL double Alt_MeterSection_AvgRepairTime(void* elem, int32_t idx);
    ALTDSS_CAPI_DLL double Alt_MeterSection_FaultRateXRepairHours(void* elem, int32_t idx);
    ALTDSS_CAPI_DLL int32_t Alt_MeterSection_NumBranches(void* elem, int32_t idx);
    ALTDSS_CAPI_DLL int32_t Alt_MeterSection_NumCustomers(void* elem, int32_t idx);
    ALTDSS_CAPI_DLL int32_t Alt_MeterSection_OCPDeviceType(void* elem, int32_t idx);
    ALTDSS_CAPI_DLL double Alt_MeterSection_SumBranchFaultRates(void* elem, int32_t idx);
    ALTDSS_CAPI_DLL int32_t Alt_MeterSection_SequenceIndex(void* elem, int32_t idx);
    ALTDSS_CAPI_DLL int32_t Alt_MeterSection_TotalCustomers(void* elem, int32_t idx);

    ALTDSS_CAPI_DLL const char* Alt_Bus_Get_Name(const void* ctx, void* pBus);
    ALTDSS_CAPI_DLL int32_t Alt_Bus_Get_NumNodes(const void* ctx, void* pBus);
    ALTDSS_CAPI_DLL double Alt_Bus_Get_kVBase(const void* ctx, void* pBus);
    ALTDSS_CAPI_DLL altdss_bool_t Alt_Bus_Get_CoordDefined(const void* ctx, void* pBus);
    ALTDSS_CAPI_DLL double Alt_Bus_Get_X(const void* ctx, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Set_X(const void* ctx, void* pBus, double value);
    ALTDSS_CAPI_DLL double Alt_Bus_Get_Y(const void* ctx, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Set_Y(const void* ctx, void* pBus, double value);
    ALTDSS_CAPI_DLL double Alt_Bus_Get_Distance(const void* ctx, void* pBus);
    ALTDSS_CAPI_DLL double Alt_Bus_Get_IntDuration(const void* ctx, void* pBus);
    ALTDSS_CAPI_DLL double Alt_Bus_Get_Lambda(const void* ctx, void* pBus);
    ALTDSS_CAPI_DLL double Alt_Bus_Get_CustDuration(const void* ctx, void* pBus);
    ALTDSS_CAPI_DLL double Alt_Bus_Get_CustInterrupts(const void* ctx, void* pBus);
    ALTDSS_CAPI_DLL int32_t Alt_Bus_Get_NumCustomers(const void* ctx, void* pBus);
    ALTDSS_CAPI_DLL double Alt_Bus_Get_NumInterrupts(const void* ctx, void* pBus);
    ALTDSS_CAPI_DLL double Alt_Bus_Get_TotalMiles(const void* ctx, void* pBus);
    ALTDSS_CAPI_DLL int32_t Alt_Bus_Get_SectionID(const void* ctx, void* pBus);
    ALTDSS_CAPI_DLL altdss_bool_t Alt_Bus_ZscRefresh(const void* ctx, void* pBus);
    ALTDSS_CAPI_DLL int32_t Alt_Bus_GetUniqueNodeNumber(void *ctx, void *pBus, int32_t startNumber);
    ALTDSS_CAPI_DLL void Alt_Bus_Get_Voltages(const void* ctx, double** resultPtr, int32_t* resultDims, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Get_Nodes(const void* ctx, int32_t** resultPtr, int32_t* resultDims, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Get_SeqVoltages(const void* ctx, double** resultPtr, int32_t* resultDims, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Get_Isc(const void* ctx, double** resultPtr, int32_t* resultDims, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Get_Voc(const void* ctx, double** resultPtr, int32_t* resultDims, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Get_puVoltages(const void* ctx, double** resultPtr, int32_t* resultDims, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Get_Zsc0(const void* ctx, double** resultPtr, int32_t* resultDims, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Get_Zsc1(const void* ctx, double** resultPtr, int32_t* resultDims, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Get_ZscMatrix(const void* ctx, double** resultPtr, int32_t* resultDims, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Get_YscMatrix(const void* ctx, double** resultPtr, int32_t* resultDims, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Get_ComplexSeqVoltages(const void* ctx, double** resultPtr, int32_t* resultDims, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Get_puVLL(const void* ctx, double** resultPtr, int32_t* resultDims, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Get_VLL(const void* ctx, double** resultPtr, int32_t* resultDims, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Get_puVMagAngle(const void* ctx, double** resultPtr, int32_t* resultDims, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Get_VMagAngle(const void* ctx, double** resultPtr, int32_t* resultDims, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Get_Zsc012Matrix(const void* ctx, double** resultPtr, int32_t* resultDims, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Get_Lines(const void* ctx, void*** resultPtr, int32_t* resultDims, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Get_Loads(const void* ctx, void*** resultPtr, int32_t* resultDims, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Get_PCElements(const void* ctx, void*** resultPtr, int32_t* resultDims, void* pBus);
    ALTDSS_CAPI_DLL void Alt_Bus_Get_PDElements(const void* ctx, void*** resultPtr, int32_t* resultDims, void* pBus);
    ALTDSS_CAPI_DLL void** Alt_Bus_GetListPtr(void *ctx);
    ALTDSS_CAPI_DLL void* Alt_Bus_GetByIndex(void *ctx, int32_t idx);
    ALTDSS_CAPI_DLL void* Alt_Bus_GetByName(void *ctx, const char* name);
    ALTDSS_CAPI_DLL const char* Alt_Bus_ToJSON(void *ctx, void* pBus, int32_t options);
    ALTDSS_CAPI_DLL void Alt_BusBatch_GetFloat64FromFunc(void *ctx, double** resultPtr, int32_t* resultDims, void** batch, int32_t batchSize, dss_ctx_bus_float64_func_t func);
    ALTDSS_CAPI_DLL void Alt_BusBatch_GetInt32FromFunc(void *ctx, int32_t** resultPtr, int32_t* resultDims, void** batch, int32_t batchSize, dss_ctx_bus_int32_func_t func);
    ALTDSS_CAPI_DLL const char* Alt_BusBatch_ToJSON(void *ctx, void** batch, int32_t batchSize, int32_t options);

#ifdef __cplusplus
} // extern "C"
#ifdef ALTDSS_CAPI_NAMESPACE
} } // namespace altdss::capi
#endif
#endif
#endif
