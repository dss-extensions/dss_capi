/*! \file dss.h */
#ifndef ALTDSS_CAPI_COMMON_H
#define ALTDSS_CAPI_COMMON_H

#ifndef ALTDSS_CAPI_DLL
#ifdef HAS_ALTDSS_LOADER_FUNC
#ifdef _WIN32
#if defined(altdss_capi_loader_EXPORTS)
#define ALTDSS_CAPI_DLL __declspec(dllexport)
#else
#define ALTDSS_CAPI_DLL __declspec(dllimport)
#endif
#else
#define ALTDSS_CAPI_DLL
#endif
#endif
#endif

#ifndef ALTDSS_CAPI_DLL
//#define ALTDSS_CAPI_DLL __declspec(dllimport)
#define ALTDSS_CAPI_DLL
#endif

#ifdef __cplusplus
#    include <cstdint>
#    include <cstddef>
#else
#    include <stdint.h>
#    include <stdbool.h>
#    include <stddef.h>
#endif

#include "./version.h"
#include "./enums.h"

#ifdef __cplusplus
#ifdef ALTDSS_CAPI_NAMESPACE
namespace altdss { namespace capi {
#endif
extern "C" {
#else
#endif
    /*!  
    Function types for plotting and writing/message callbacks. 
    Receives a string that contains the JSON-encoded parameters.
    
    EXPERIMENTAL
    */
    typedef int32_t (*altdss_callback_plot_t)(const void* ctx, char* jsonParams);
    typedef int32_t (*altdss_callback_message_t)(const void* ctx, char* messageStr, int32_t messageType, int64_t messageSize, int32_t messageSubType);
    typedef void (*altdss_callback_event_t)(const void* ctx, int32_t eventCode, int32_t step, void* ptr);

    /*!  
    Function types for extra object functions (used by the batch APIs)
   
    EXPERIMENTAL
    */
    typedef double (*dss_obj_float64_func_t)(void* obj);
    typedef int32_t (*dss_obj_int32_func_t)(void* obj);
    typedef double (*dss_obj_float64_int32_func_t)(void* obj, int32_t val);
    typedef double (*dss_ctx_bus_float64_func_t)(const void* ctx, void* obj);
    typedef int32_t (*dss_ctx_bus_int32_func_t)(const void* ctx, void* obj);

    /*!
    Typedefs for the Alt API
    */
    typedef int32_t altdss_bool_t;

    /* Functions start here */

    ALTDSS_CAPI_DLL void DSS_Dispose_PByte(int8_t** p);
    ALTDSS_CAPI_DLL void DSS_Dispose_PDouble(double** p);
    ALTDSS_CAPI_DLL void DSS_Dispose_PInteger(int32_t** p);
    ALTDSS_CAPI_DLL void DSS_Dispose_PPAnsiChar(char ***p, int32_t cnt);
    ALTDSS_CAPI_DLL const char* DSS_Get_PAnsiChar(void *p, int32_t index);

    /*
    Share general objects

    Share general DSS objects (e.g. LineCode, LineGeometry, LoadShape) from one DSS context to another.

    Default items are skipped! Use with caution.

    ***EXPERIMENTAL***

    (API Extension)
    */
    ALTDSS_CAPI_DLL void ctx_ShareGeneral(void* ctxFrom, void *ctxTo);

    /*! 
    Extract the current properties as a JSON encoded string.
    WARNING: this is unstable and subject to change.

    (API Extension)
    */
    ALTDSS_CAPI_DLL const char* DSS_ExtractSchema(void *ctx, uint16_t jsonSchema);

    ALTDSS_CAPI_DLL void DSS_Dispose_String(const char* S);
    ALTDSS_CAPI_DLL void DSS_Dispose_PPointer(void*** p);

    /*! 
    `DSS_BeginPascalThread` can be used to start a new thread from the Pascal side.
    Use this if you experience issues with your languages normal threads.
    
    `func` is the address of the function that will be run in the thread.
    `paramptr` is a pointer to the data to pass as a parameter when calling
    `func`.

    NOTE: this function will be removed in a future version if DSS C-API is
          reimplemented in another language.

    (API Extension)
    */
    ALTDSS_CAPI_DLL void *DSS_BeginPascalThread(void *func, void *paramptr);

    /*! 
    Use this function to wait for a thread started by `DSS_BeginPascalThread`
    to finish.

    NOTE: this function will be removed in a future version if DSS C-API is
          reimplemented in another language.

    (API Extension)
    */
    ALTDSS_CAPI_DLL void DSS_WaitPascalThread(void *handle);

    /*!
    Loads the gettext MO file from the path indicated by Value, to be used for
    general OpenDSS messages.
    On failure, messages are left as their default English versions as given
    in the main source-code in DSS C-API.
    No error is otherwise presented.

    This function is not intended for the typical user.

    (API Extension)
    */
    ALTDSS_CAPI_DLL void DSS_SetMessagesMO(const char* Value);
    
    /*!
    Loads the gettext MO file from the path indicated by Value, to be used for
    help of DSS properties.
    On failure, the property help strings are left as "NO HELP OR DESCRIPTION AVAILABLE."
    No error is otherwise presented.

    This function is not intended for the typical user.

    (API Extension)
    */
    ALTDSS_CAPI_DLL void DSS_SetPropertiesMO(const char* Value);

#ifdef __cplusplus
} // extern "C"
#ifdef ALTDSS_CAPI_NAMESPACE
} } // namespace altdss::capi
#endif
#endif
#endif
