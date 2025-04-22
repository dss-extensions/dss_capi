
// Monitors.h : Declaration of the CMonitors

#pragma once
#include "Resource.h"       // main symbols
#include "DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CMonitors
class ATL_NO_VTABLE CMonitors :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CMonitors, &CLSID_Monitors>,
    public ISupportErrorInfo,
    public IDispatchImpl<IMonitors, &IID_IMonitors, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CMonitors, &IID_IMonitors>

{
public:
    CMonitors()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_MONITORS)

BEGIN_COM_MAP(CMonitors)
    COM_INTERFACE_ENTRY(IMonitors)
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
    STDMETHOD(get_AllNames)(VARIANT* Value);
    STDMETHOD(get_First)(long* Value);
    STDMETHOD(get_Next)(long* Value);
    STDMETHOD(Reset)();
    STDMETHOD(ResetAll)();
    STDMETHOD(Sample)();
    STDMETHOD(Save)();
    STDMETHOD(Show)();
    STDMETHOD(get_FileName)(BSTR* Value);
    STDMETHOD(get_Mode)(long* Value);
    STDMETHOD_(HRESULT, put_Mode)(long Value);
    STDMETHOD(get_Name)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Name)(BSTR Value);
    STDMETHOD(get_ByteStream)(VARIANT* Value);
    STDMETHOD(get_SampleCount)(long* Value);
    STDMETHOD(SampleAll)();
    STDMETHOD(SaveAll)();
    STDMETHOD(get_Count)(long* Value);
    STDMETHOD(Process)();
    STDMETHOD(ProcessAll)();
    STDMETHOD(get_FileVersion)(long* Value);
    STDMETHOD(get_RecordSize)(long* Value);
    STDMETHOD(get_Header)(VARIANT* Value);
    STDMETHOD(get_dblHour)(VARIANT* Value);
    STDMETHOD(get_dblFreq)(VARIANT* Value);
    STDMETHOD_(HRESULT, get_Channel)(long Index, VARIANT* Value);
    STDMETHOD(get_NumChannels)(long* Value);
    STDMETHOD(get_Element)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Element)(BSTR Value);
    STDMETHOD(get_Terminal)(long* Value);
    STDMETHOD_(HRESULT, put_Terminal)(long Value);
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Monitors), CMonitors)
