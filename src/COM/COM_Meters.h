
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Meters.h : Declaration of CMeters

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CMeters
class ATL_NO_VTABLE CMeters :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CMeters, &CLSID_Meters>,
    public ISupportErrorInfo,
    public IDispatchImpl<IMeters, &IID_IMeters, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CMeters, &IID_IMeters>

{
public:
    CMeters()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_METERS)

BEGIN_COM_MAP(CMeters)
    COM_INTERFACE_ENTRY(IMeters)
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
    STDMETHOD(get_RegisterNames)(VARIANT* Value);
    STDMETHOD(get_RegisterValues)(VARIANT* Value);
    STDMETHOD(Reset)();
    STDMETHOD(ResetAll)();
    STDMETHOD(Sample)();
    STDMETHOD(Save)();
    STDMETHOD(get_Name)(BSTR* Value);
    STDMETHOD_(HRESULT, put_Name)(BSTR Value);
    STDMETHOD(get_Totals)(VARIANT* Value);
    STDMETHOD(get_Peakcurrent)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_Peakcurrent)(VARIANT Value);
    STDMETHOD(get_CalcCurrent)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_CalcCurrent)(VARIANT Value);
    STDMETHOD(get_AllocFactors)(VARIANT* Value);
    STDMETHOD_(HRESULT, put_AllocFactors)(VARIANT Value);
    STDMETHOD(get_MeteredElement)(BSTR* Value);
    STDMETHOD_(HRESULT, put_MeteredElement)(BSTR Value);
    STDMETHOD(get_MeteredTerminal)(long* Value);
    STDMETHOD_(HRESULT, put_MeteredTerminal)(long Value);
    STDMETHOD(get_DIFilesAreOpen)(VARIANT_BOOL* Value);
    STDMETHOD(SampleAll)();
    STDMETHOD(SaveAll)();
    STDMETHOD(OpenAllDIFiles)();
    STDMETHOD(CloseAllDIFiles)();
    STDMETHOD(get_CountEndElements)(long* Value);
    STDMETHOD(get_AllEndElements)(VARIANT* Value);
    STDMETHOD(get_Count)(long* Value);
    STDMETHOD(get_AllBranchesInZone)(VARIANT* Value);
    STDMETHOD(get_CountBranches)(long* Value);
    STDMETHOD(get_SAIFI)(double* Value);
    STDMETHOD(get_SequenceIndex)(long* Value);
    STDMETHOD_(HRESULT, put_SequenceIndex)(long Value);
    STDMETHOD(get_SAIFIKW)(double* Value);
    STDMETHOD_(HRESULT, DoReliabilityCalc)(VARIANT_BOOL AssumeRestoration);
    STDMETHOD(get_SeqListSize)(long* Value);
    STDMETHOD(get_TotalCustomers)(long* Value);
    STDMETHOD(get_SAIDI)(double* Value);
    STDMETHOD(get_CustInterrupts)(double* Value);
    STDMETHOD(get_NumSections)(long* Value);
    STDMETHOD_(HRESULT, SetActiveSection)(long SectIdx);
    STDMETHOD(get_OCPDeviceType)(long* Value);
    STDMETHOD(get_NumSectionCustomers)(long* Value);
    STDMETHOD(get_NumSectionBranches)(long* Value);
    STDMETHOD(get_AvgRepairTime)(double* Value);
    STDMETHOD(get_FaultRateXRepairHrs)(double* Value);
    STDMETHOD(get_SumBranchFltRates)(double* Value);
    STDMETHOD(get_SectSeqIdx)(long* Value);
    STDMETHOD(get_SectTotalCust)(long* Value);
    STDMETHOD(get_ZonePCE)(VARIANT* Value);
    STDMETHOD(get_idx)(long* Value);
    STDMETHOD_(HRESULT, put_idx)(long Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Meters), CMeters)
