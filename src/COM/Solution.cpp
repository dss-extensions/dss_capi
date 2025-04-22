// Solution.cpp : Implementation of CSolution

#include "pch.h"
#include "Solution.h"


STDMETHODIMP CSolution::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_ISolution
    };

    for (int i = 0; i < sizeof(arr) / sizeof(arr[0]); i++)
    {
        if (InlineIsEqualGUID(*arr[i], riid))
        {
            return S_OK;
        }
    }
    return S_FALSE;
}

STDMETHODIMP CSolution::Solve()
{
    dss_capi.Solution_Solve(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_Mode(long* Mode)
{
    if (Mode == nullptr)
    {
        return E_POINTER;
    }
    *Mode = dss_capi.Solution_Get_Mode(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_Mode(long Mode)
{
    dss_capi.Solution_Set_Mode(dss_capi_ctx, static_cast<int32_t>(Mode));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_Frequency(double* Frequency)
{
    if (Frequency == nullptr)
    {
        return E_POINTER;
    }
    *Frequency = dss_capi.Solution_Get_Frequency(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_Frequency(double Frequency)
{
    dss_capi.Solution_Set_Frequency(dss_capi_ctx, Frequency);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_Hour(long* Hour)
{
    if (Hour == nullptr)
    {
        return E_POINTER;
    }
    *Hour = dss_capi.Solution_Get_Hour(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_Hour(long Hour)
{
    dss_capi.Solution_Set_Hour(dss_capi_ctx, static_cast<int32_t>(Hour));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_Seconds(double* Seconds)
{
    if (Seconds == nullptr)
    {
        return E_POINTER;
    }
    *Seconds = dss_capi.Solution_Get_Seconds(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_Seconds(double Seconds)
{
    dss_capi.Solution_Set_Seconds(dss_capi_ctx, Seconds);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_StepSize(double* StepSize)
{
    if (StepSize == nullptr)
    {
        return E_POINTER;
    }
    *StepSize = dss_capi.Solution_Get_StepSize(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_StepSize(double StepSize)
{
    dss_capi.Solution_Set_StepSize(dss_capi_ctx, StepSize);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_Year(long* Year)
{
    if (Year == nullptr)
    {
        return E_POINTER;
    }
    *Year = dss_capi.Solution_Get_Year(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_Year(long Year)
{
    dss_capi.Solution_Set_Year(dss_capi_ctx, static_cast<int32_t>(Year));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_LoadMult(double* LoadMult)
{
    if (LoadMult == nullptr)
    {
        return E_POINTER;
    }
    *LoadMult = dss_capi.Solution_Get_LoadMult(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_LoadMult(double LoadMult)
{
    dss_capi.Solution_Set_LoadMult(dss_capi_ctx, LoadMult);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_Iterations(long* Iterations)
{
    if (Iterations == nullptr)
    {
        return E_POINTER;
    }
    *Iterations = dss_capi.Solution_Get_Iterations(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_MaxIterations(long* MaxIterations)
{
    if (MaxIterations == nullptr)
    {
        return E_POINTER;
    }
    *MaxIterations = dss_capi.Solution_Get_MaxIterations(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_MaxIterations(long MaxIterations)
{
    dss_capi.Solution_Set_MaxIterations(dss_capi_ctx, static_cast<int32_t>(MaxIterations));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_Tolerance(double* Tolerance)
{
    if (Tolerance == nullptr)
    {
        return E_POINTER;
    }
    *Tolerance = dss_capi.Solution_Get_Tolerance(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_Tolerance(double Tolerance)
{
    dss_capi.Solution_Set_Tolerance(dss_capi_ctx, Tolerance);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_Number(long* Number)
{
    if (Number == nullptr)
    {
        return E_POINTER;
    }
    *Number = dss_capi.Solution_Get_Number(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_Number(long Number)
{
    dss_capi.Solution_Set_Number(dss_capi_ctx, static_cast<int32_t>(Number));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_Random(long* Random)
{
    if (Random == nullptr)
    {
        return E_POINTER;
    }
    *Random = dss_capi.Solution_Get_Random(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_Random(long Random)
{
    dss_capi.Solution_Set_Random(dss_capi_ctx, static_cast<int32_t>(Random));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_ModeID(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Solution_Get_ModeID(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_LoadModel(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_LoadModel(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_LoadModel(long Value)
{
    dss_capi.Solution_Set_LoadModel(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_LDCurve(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Solution_Get_LDCurve(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_LDCurve(BSTR Value)
{
    dss_capi.Solution_Set_LDCurve(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_pctGrowth(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_pctGrowth(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_pctGrowth(double Value)
{
    dss_capi.Solution_Set_pctGrowth(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_AddType(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_AddType(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_AddType(long Value)
{
    dss_capi.Solution_Set_AddType(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_GenkW(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_GenkW(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_GenkW(double Value)
{
    dss_capi.Solution_Set_GenkW(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_GenPF(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_GenPF(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_GenPF(double Value)
{
    dss_capi.Solution_Set_GenPF(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_Capkvar(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_Capkvar(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_Capkvar(double Value)
{
    dss_capi.Solution_Set_Capkvar(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_Algorithm(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_Algorithm(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_Algorithm(long Value)
{
    dss_capi.Solution_Set_Algorithm(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_ControlMode(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_ControlMode(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_ControlMode(long Value)
{
    dss_capi.Solution_Set_ControlMode(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_GenMult(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_GenMult(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_GenMult(double Value)
{
    dss_capi.Solution_Set_GenMult(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_DefaultDaily(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Solution_Get_DefaultDaily(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_DefaultDaily(BSTR Value)
{
    dss_capi.Solution_Set_DefaultDaily(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_DefaultYearly(BSTR* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = CComBSTR(dss_capi.Solution_Get_DefaultYearly(dss_capi_ctx)).Detach();
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_DefaultYearly(BSTR Value)
{
    dss_capi.Solution_Set_DefaultYearly(dss_capi_ctx, AltDSS_COM_CString(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_EventLog(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Solution_Get_EventLog, Value);
}

STDMETHODIMP CSolution::get_dblHour(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_dblHour(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_dblHour(double Value)
{
    dss_capi.Solution_Set_dblHour(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_StepsizeMin(double rhs)
{
    dss_capi.Solution_Set_StepsizeMin(dss_capi_ctx, rhs);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_StepsizeHr(double rhs)
{
    dss_capi.Solution_Set_StepsizeHr(dss_capi_ctx, rhs);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_ControlIterations(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_ControlIterations(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_ControlIterations(long Value)
{
    dss_capi.Solution_Set_ControlIterations(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_MaxControlIterations(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_MaxControlIterations(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_MaxControlIterations(long Value)
{
    dss_capi.Solution_Set_MaxControlIterations(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::Sample_DoControlActions()
{
    dss_capi.Solution_Sample_DoControlActions(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::CheckFaultStatus()
{
    dss_capi.Solution_CheckFaultStatus(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::SolveSnap()
{
    dss_capi.Solution_SolveSnap(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::SolveDirect()
{
    dss_capi.Solution_SolveDirect(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::SolvePflow()
{
    dss_capi.Solution_SolvePflow(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::SolveNoControl()
{
    dss_capi.Solution_SolveNoControl(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::SolvePlusControl()
{
    dss_capi.Solution_SolvePlusControl(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::InitSnap()
{
    dss_capi.Solution_InitSnap(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::CheckControls()
{
    dss_capi.Solution_CheckControls(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::SampleControlDevices()
{
    dss_capi.Solution_SampleControlDevices(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::DoControlActions()
{
    dss_capi.Solution_DoControlActions(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::BuildYMatrix(long BuildOption, long AllocateVI)
{
    dss_capi.Solution_BuildYMatrix(dss_capi_ctx, static_cast<int32_t>(BuildOption), static_cast<int32_t>(AllocateVI));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_SystemYChanged(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_SystemYChanged(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_Converged(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_Converged(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_Converged(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Solution_Set_Converged(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_Totaliterations(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_Totaliterations(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_MostIterationsDone(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_MostIterationsDone(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_ControlActionsDone(VARIANT_BOOL* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_ControlActionsDone(dss_capi_ctx) ? VARIANT_TRUE : VARIANT_FALSE;
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_ControlActionsDone(VARIANT_BOOL Value)
{
    if (Value != VARIANT_TRUE && Value != VARIANT_FALSE)
    {
        return E_INVALIDARG;
    }
    dss_capi.Solution_Set_ControlActionsDone(dss_capi_ctx, Value == VARIANT_FALSE ? 0 : 1);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::FinishTimeStep()
{
    dss_capi.Solution_FinishTimeStep(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::Cleanup()
{
    dss_capi.Solution_Cleanup(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_Total_Time(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_Total_Time(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_Total_Time(double Value)
{
    dss_capi.Solution_Set_Total_Time(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_Process_Time(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_Process_Time(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_Time_of_Step(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_Time_of_Step(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_IntervalHrs(double* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_IntervalHrs(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_IntervalHrs(double Value)
{
    dss_capi.Solution_Set_IntervalHrs(dss_capi_ctx, Value);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::SolveAll()
{
    dss_capi.Solution_SolveAll(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::get_IncMatrix(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetInt32s(dss_capi.Solution_Get_IncMatrix_GR, Value);
}

STDMETHODIMP CSolution::get_IncMatrixRows(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Solution_Get_IncMatrixRows, Value);
}

STDMETHODIMP CSolution::get_IncMatrixCols(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.Solution_Get_IncMatrixCols, Value);
}

STDMETHODIMP CSolution::get_BusLevels(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetInt32s(dss_capi.Solution_Get_BusLevels_GR, Value);
}

STDMETHODIMP CSolution::get_Laplacian(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetInt32s(dss_capi.Solution_Get_Laplacian_GR, Value);
}

STDMETHODIMP CSolution::get_MinIterations(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.Solution_Get_MinIterations(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CSolution::put_MinIterations(long Value)
{
    dss_capi.Solution_Set_MinIterations(dss_capi_ctx, static_cast<int32_t>(Value));
    return AltDSS_COM_CheckError();
}

