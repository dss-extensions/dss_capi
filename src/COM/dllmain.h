// dllmain.h : Declaration of module class.

class CDSSExtensionsModule : public ATL::CAtlDllModuleT< CDSSExtensionsModule >
{
public :
	DECLARE_LIBID(LIBID_DSSExtensions)
	DECLARE_REGISTRY_APPID_RESOURCEID(IDR_DSSEXTENSIONS, "{6f083a5b-b17b-4e80-bd67-a5e3f86c8513}")
};

extern class CDSSExtensionsModule _AtlModule;
