
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_Solution.h : Declaration of CSolution

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CSolution
class ATL_NO_VTABLE CSolution :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CSolution, &CLSID_Solution>,
    public ISupportErrorInfo,
    public IDispatchImpl<ISolution, &IID_ISolution, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CSolution, &IID_ISolution>

{
public:
    CSolution()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_SOLUTION)

BEGIN_COM_MAP(CSolution)
    COM_INTERFACE_ENTRY(ISolution)
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
    STDMETHOD(Solve)();
    STDMETHOD(get_Mode)(long* Mode);
    STDMETHOD_(HRESULT, put_Mode)(long Mode);
    STDMETHOD(get_Frequency)(double* Frequency);
    STDMETHOD_(HRESULT, put_Frequency)(double Frequency);
    STDMETHOD(get_Hour)(long* Hour);
    STDMETHOD_(HRESULT, put_Hour)(long Hour);
    STDMETHOD(get_Seconds)(double* Seconds);
    STDMETHOD_(HRESULT, put_Seconds)(double Seconds);
    STDMETHOD(get_StepSize)(double* StepSize);
    STDMETHOD_(HRESULT, put_StepSize)(double StepSize);
    STDMETHOD(get_Year)(long* Year);
    STDMETHOD_(HRESULT, put_Year)(long Year);
    STDMETHOD(get_LoadMult)(double* LoadMult);
    STDMETHOD_(HRESULT, put_LoadMult)(double LoadMult);
    STDMETHOD(get_Iterations)(long* Iterations);
    STDMETHOD(get_MaxIterations)(long* MaxIterations);
    STDMETHOD_(HRESULT, put_MaxIterations)(long MaxIterations);
    STDMETHOD(get_Tolerance)(double* Tolerance);
    STDMETHOD_(HRESULT, put_Tolerance)(double Tolerance);
    STDMETHOD(get_Number)(long* Number);
    STDMETHOD_(HRESULT, put_Number)(long Number);
    STDMETHOD(get_Random)(long* Random);
    STDMETHOD_(HRESULT, put_Random)(long Random);
    STDMETHOD(get_ModeID)(BSTR* Value);
    STDMETHOD(get_LoadModel)(long* Value);
    STDMETHOD_(HRESULT, put_LoadModel)(long Value);
    STDMETHOD(get_LDCurve)(BSTR* Value);
    STDMETHOD_(HRESULT, put_LDCurve)(BSTR Value);
    STDMETHOD(get_pctGrowth)(double* Value);
    STDMETHOD_(HRESULT, put_pctGrowth)(double Value);
    STDMETHOD(get_AddType)(long* Value);
    STDMETHOD_(HRESULT, put_AddType)(long Value);
    STDMETHOD(get_GenkW)(double* Value);
    STDMETHOD_(HRESULT, put_GenkW)(double Value);
    STDMETHOD(get_GenPF)(double* Value);
    STDMETHOD_(HRESULT, put_GenPF)(double Value);
    STDMETHOD(get_Capkvar)(double* Value);
    STDMETHOD_(HRESULT, put_Capkvar)(double Value);
    STDMETHOD(get_Algorithm)(long* Value);
    STDMETHOD_(HRESULT, put_Algorithm)(long Value);
    STDMETHOD(get_ControlMode)(long* Value);
    STDMETHOD_(HRESULT, put_ControlMode)(long Value);
    STDMETHOD(get_GenMult)(double* Value);
    STDMETHOD_(HRESULT, put_GenMult)(double Value);
    STDMETHOD(get_DefaultDaily)(BSTR* Value);
    STDMETHOD_(HRESULT, put_DefaultDaily)(BSTR Value);
    STDMETHOD(get_DefaultYearly)(BSTR* Value);
    STDMETHOD_(HRESULT, put_DefaultYearly)(BSTR Value);
    STDMETHOD(get_EventLog)(VARIANT* Value);
    STDMETHOD(get_dblHour)(double* Value);
    STDMETHOD_(HRESULT, put_dblHour)(double Value);
    STDMETHOD_(HRESULT, put_StepsizeMin)(double rhs);
    STDMETHOD_(HRESULT, put_StepsizeHr)(double rhs);
    STDMETHOD(get_ControlIterations)(long* Value);
    STDMETHOD_(HRESULT, put_ControlIterations)(long Value);
    STDMETHOD(get_MaxControlIterations)(long* Value);
    STDMETHOD_(HRESULT, put_MaxControlIterations)(long Value);
    STDMETHOD(Sample_DoControlActions)();
    STDMETHOD(CheckFaultStatus)();
    STDMETHOD(SolveSnap)();
    STDMETHOD(SolveDirect)();
    STDMETHOD(SolvePflow)();
    STDMETHOD(SolveNoControl)();
    STDMETHOD(SolvePlusControl)();
    STDMETHOD(InitSnap)();
    STDMETHOD(CheckControls)();
    STDMETHOD(SampleControlDevices)();
    STDMETHOD(DoControlActions)();
    STDMETHOD_(HRESULT, BuildYMatrix)(long BuildOption, long AllocateVI);
    STDMETHOD(get_SystemYChanged)(VARIANT_BOOL* Value);
    STDMETHOD(get_Converged)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_Converged)(VARIANT_BOOL Value);
    STDMETHOD(get_Totaliterations)(long* Value);
    STDMETHOD(get_MostIterationsDone)(long* Value);
    STDMETHOD(get_ControlActionsDone)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_ControlActionsDone)(VARIANT_BOOL Value);
    STDMETHOD(FinishTimeStep)();
    STDMETHOD(Cleanup)();
    STDMETHOD(get_Total_Time)(double* Value);
    STDMETHOD_(HRESULT, put_Total_Time)(double Value);
    STDMETHOD(get_Process_Time)(double* Value);
    STDMETHOD(get_Time_of_Step)(double* Value);
    STDMETHOD(get_IntervalHrs)(double* Value);
    STDMETHOD_(HRESULT, put_IntervalHrs)(double Value);
    STDMETHOD(SolveAll)();
    STDMETHOD(get_IncMatrix)(VARIANT* Value);
    STDMETHOD(get_IncMatrixRows)(VARIANT* Value);
    STDMETHOD(get_IncMatrixCols)(VARIANT* Value);
    STDMETHOD(get_BusLevels)(VARIANT* Value);
    STDMETHOD(get_Laplacian)(VARIANT* Value);
    STDMETHOD(get_MinIterations)(long* Value);
    STDMETHOD_(HRESULT, put_MinIterations)(long Value);
};

OBJECT_ENTRY_AUTO(__uuidof(Solution), CSolution)
