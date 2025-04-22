
// CmathLib.h : Declaration of the CCmathLib

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CCmathLib
class ATL_NO_VTABLE CCmathLib :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CCmathLib, &CLSID_CmathLib>,
    public ISupportErrorInfo,
    public IDispatchImpl<ICmathLib, &IID_ICmathLib, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CCmathLib, &IID_ICmathLib>

{
public:
    CCmathLib()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_CMATHLIB)

BEGIN_COM_MAP(CCmathLib)
    COM_INTERFACE_ENTRY(ICmathLib)
    COM_INTERFACE_ENTRY(IDispatch)
    COM_INTERFACE_ENTRY(ISupportErrorInfo)
END_COM_MAP()

    STDMETHOD(InterfaceSupportsErrorInfo)(REFIID riid);

    DECLARE_PROTECT_FINAL_CONSTRUCT()

    HRESULT FinalConstruct()
    {
        return AltDSS_COM_CheckError();
    }

    void FinalRelease()
    {
        return;
    }

public:
    STDMETHOD_(HRESULT, get_cmplx)(double RealPart, double ImagPart, VARIANT* Value);
    STDMETHOD_(HRESULT, get_cabs)(double RealPart, double ImagPart, double* Value);
    STDMETHOD_(HRESULT, get_cdang)(double RealPart, double ImagPart, double* Value);
    STDMETHOD_(HRESULT, get_ctopolardeg)(double RealPart, double ImagPart, VARIANT* Value);
    STDMETHOD_(HRESULT, get_pdegtocomplex)(double magnitude, double angle, VARIANT* Value);
    STDMETHOD_(HRESULT, get_cmul)(double a1, double b1, double a2, double b2, VARIANT* Value);
    STDMETHOD_(HRESULT, get_cdiv)(double a1, double b1, double a2, double b2, VARIANT* Value);
};

OBJECT_ENTRY_AUTO(__uuidof(CmathLib), CCmathLib)
