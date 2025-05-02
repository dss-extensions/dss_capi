// COM_dllmain.cpp : Implementation of DllMain.

#include "COM_pch.h"
#include "COM_framework.h"
#include "COM_resource.h"
#include "COM_DSSExtensions_i.h"
#include "COM_dllmain.h"

CDSSExtensionsModule _AtlModule;

// DLL Entry Point
extern "C" BOOL WINAPI DllMain(HINSTANCE hInstance, DWORD dwReason, LPVOID lpReserved)
{
	hInstance;
	return _AtlModule.DllMain(dwReason, lpReserved);
}
