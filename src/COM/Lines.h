
// Lines.h : Declaration of the CLines

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CLines
class ATL_NO_VTABLE CLines :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CLines, &CLSID_Lines>,
    public ISupportErrorInfo,
    public IDispatchImpl<ILines, &IID_ILines, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CLines, &IID_ILines>

{
public:
    CLines()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_LINES)

BEGIN_COM_MAP(CLines)
    COM_INTERFACE_ENTRY(ILines)
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
    STDMETHOD(get_Name)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Name)(BSTR Value);
    STDMETHOD(get_AllNames)(VARIANT* Value);
    STDMETHOD(get_First)(long* Value);
    STDMETHOD(get_Next)(long* Value);
    STDMETHOD_(HRESULT, New)(BSTR Name, long* Value);
    STDMETHOD(get_Bus1)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Bus1)(BSTR Value);
    STDMETHOD(get_Bus2)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Bus2)(BSTR Value);
    STDMETHOD(get_LineCode)(BSTR* Value);
    STDMETHOD_(HRESULT, put_LineCode)(BSTR Value);
    STDMETHOD(get_Length)(double* Value);
    STDMETHOD_(HRESULT, put_Length)(double Value);
    STDMETHOD(get_Phases)(long* Value);
    STDMETHOD_(HRESULT, put_Phases)(long Value);
    STDMETHOD(get_R1)(double* Value);
    STDMETHOD_(HRESULT, put_R1)(double Value);
    STDMETHOD(get_X1)(double* Value);
    STDMETHOD_(HRESULT, put_X1)(double Value);
    STDMETHOD(get_R0)(double* Value);
    STDMETHOD_(HRESULT, put_R0)(double Value);
    STDMETHOD(get_X0)(double* Value);
    STDMETHOD_(HRESULT, put_X0)(double Value);
    STDMETHOD(get_C1)(double* Value);
    STDMETHOD_(HRESULT, put_C1)(double Value);
    STDMETHOD(get_C0)(double* Value);
    STDMETHOD_(HRESULT, put_C0)(double Value);
    STDMETHOD(get_Rmatrix)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_Rmatrix)(VARIANT Value);
    STDMETHOD(get_Xmatrix)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_Xmatrix)(VARIANT Value);
    STDMETHOD(get_Cmatrix)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_Cmatrix)(VARIANT Value);
    STDMETHOD(get_NormAmps)(double* Value);
    STDMETHOD_(HRESULT, put_NormAmps)(double Value);
    STDMETHOD(get_EmergAmps)(double* Value);
    STDMETHOD_(HRESULT, put_EmergAmps)(double Value);
    STDMETHOD(get_Geometry)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Geometry)(BSTR Value);
    STDMETHOD(get_Rg)(double* Value);
    STDMETHOD_(HRESULT, put_Rg)(double Value);
    STDMETHOD(get_Xg)(double* Value);
    STDMETHOD_(HRESULT, put_Xg)(double Value);
    STDMETHOD(get_Rho)(double* Value);
    STDMETHOD_(HRESULT, put_Rho)(double Value);
    STDMETHOD(get_Yprim)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_Yprim)(VARIANT Value);
    STDMETHOD(get_NumCust)(long* Value);
    STDMETHOD(get_TotalCust)(long* Value);
    STDMETHOD(get_Parent)(long* Value);
    STDMETHOD(get_Count)(long* Value);
    STDMETHOD(get_Spacing)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Spacing)(BSTR Value);
    STDMETHOD(get_Units)(long* Value);
    STDMETHOD_(HRESULT, put_Units)(long Value);
    STDMETHOD(get_SeasonRating)(double* Value);
    STDMETHOD(get_IsSwitch)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_IsSwitch)(VARIANT_BOOL Value);
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Lines), CLines)
