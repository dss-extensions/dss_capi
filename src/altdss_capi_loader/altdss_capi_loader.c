#include <altdss/capi/capi.h>
#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <libloaderapi.h>
#else
#include <dlfcn.h>
#endif

#ifndef NDEBUG
#include <stdio.h>
#include <stdlib.h>
#endif

#ifdef WIN32
static uint64_t ALTDSS_LOADER_LIB_OPTIONS = LOAD_WITH_ALTERED_SEARCH_PATH;
#else
static uint64_t ALTDSS_LOADER_LIB_OPTIONS = RTLD_NOW;
#endif

ALTDSS_CAPI_DLL int32_t AltDSSCAPILibInit(const char* libName, uint64_t* libOptions, const char* libInitFuncName, AltDSSCAPI *funcs, uint64_t size, uint64_t version, uint64_t reserved1, void* reserved2)
{
    int32_t initResult;
    const char* defaultInitFuncName = "AltDSSCAPIInit";
    altdss_func_Init init = NULL;

    if (funcs == NULL)
    { 
        return -3;
    }

#ifdef WIN32
    funcs->libHandle = LoadLibraryEx(libname, NULL, (DWORD) (libOptions ? *libOptions : ALTDSS_LOADER_LIB_OPTIONS));
#else
    funcs->libHandle = dlopen(libName, (int) (libOptions ? *libOptions : ALTDSS_LOADER_LIB_OPTIONS));
#endif

    if (funcs->libHandle == NULL)
    {
#ifndef NDEBUG
#ifndef WIN32
        puts(dlerror());
#endif
#endif
        return -2;
    }

#ifdef WIN32
    init = (altdss_func_Init) GetProcAddress(funcs->libHandle, libInitFuncName ? libInitFuncName : defaultInitFuncName);
#else
    init = (altdss_func_Init) dlsym(funcs->libHandle, libInitFuncName ? libInitFuncName : defaultInitFuncName);
#endif

    if (init == NULL)
    {
        return -1;
    }

    initResult = init(funcs, size, version, reserved1, reserved2);
    if (initResult == 0)
    {
        AltDSSCAPILibClose(funcs);
    }
    return initResult;
}

ALTDSS_CAPI_DLL void AltDSSCAPILibClose(AltDSSCAPI *funcs)
{
#ifdef WIN32
    FreeLibrary(funcs->libHandle);
#else
    if (funcs->libHandle)
    {
        dlclose(funcs->libHandle);
    }
#endif
    funcs->libHandle = NULL;
}
