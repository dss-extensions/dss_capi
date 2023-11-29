unit CAPI_Solution;

interface

uses
    CAPI_Utils,
    CAPI_Types;

function Solution_Get_Frequency(): Double; CDECL;
function Solution_Get_Hour(): Integer; CDECL;
function Solution_Get_Iterations(): Integer; CDECL;
function Solution_Get_LoadMult(): Double; CDECL;
function Solution_Get_MaxIterations(): Integer; CDECL;
function Solution_Get_Mode(): Integer; CDECL;
function Solution_Get_Number(): Integer; CDECL;
function Solution_Get_Random(): Integer; CDECL;
function Solution_Get_Seconds(): Double; CDECL;
function Solution_Get_StepSize(): Double; CDECL;
function Solution_Get_Tolerance(): Double; CDECL;
function Solution_Get_Year(): Integer; CDECL;
procedure Solution_Set_Frequency(Value: Double); CDECL;
procedure Solution_Set_Hour(Value: Integer); CDECL;
procedure Solution_Set_LoadMult(Value: Double); CDECL;
procedure Solution_Set_MaxIterations(Value: Integer); CDECL;
procedure Solution_Set_Mode(Mode: Integer); CDECL;
procedure Solution_Set_Number(Value: Integer); CDECL;
procedure Solution_Set_Random(Random: Integer); CDECL;
procedure Solution_Set_Seconds(Value: Double); CDECL;
procedure Solution_Set_StepSize(Value: Double); CDECL;
procedure Solution_Set_Tolerance(Value: Double); CDECL;
procedure Solution_Set_Year(Value: Integer); CDECL;
procedure Solution_Solve(); CDECL;
function Solution_Get_ModeID(): PAnsiChar; CDECL;
function Solution_Get_LoadModel(): Integer; CDECL;
procedure Solution_Set_LoadModel(Value: Integer); CDECL;
function Solution_Get_LDCurve(): PAnsiChar; CDECL;
procedure Solution_Set_LDCurve(const Value: PAnsiChar); CDECL;
function Solution_Get_pctGrowth(): Double; CDECL;
procedure Solution_Set_pctGrowth(Value: Double); CDECL;
function Solution_Get_AddType(): Integer; CDECL;
procedure Solution_Set_AddType(Value: Integer); CDECL;
function Solution_Get_GenkW(): Double; CDECL;
procedure Solution_Set_GenkW(Value: Double); CDECL;
function Solution_Get_GenPF(): Double; CDECL;
procedure Solution_Set_GenPF(Value: Double); CDECL;
function Solution_Get_Capkvar(): Double; CDECL;
procedure Solution_Set_Capkvar(Value: Double); CDECL;
function Solution_Get_Algorithm(): Integer; CDECL;
procedure Solution_Set_Algorithm(Value: Integer); CDECL;
function Solution_Get_ControlMode(): Integer; CDECL;
procedure Solution_Set_ControlMode(Value: Integer); CDECL;
function Solution_Get_GenMult(): Double; CDECL;
procedure Solution_Set_GenMult(Value: Double); CDECL;
function Solution_Get_DefaultDaily(): PAnsiChar; CDECL;
function Solution_Get_DefaultYearly(): PAnsiChar; CDECL;
procedure Solution_Set_DefaultDaily(const Value: PAnsiChar); CDECL;
procedure Solution_Set_DefaultYearly(const Value: PAnsiChar); CDECL;
procedure Solution_Get_EventLog(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Solution_Get_EventLog_GR(); CDECL;
function Solution_Get_dblHour(): Double; CDECL;
procedure Solution_Set_dblHour(Value: Double); CDECL;
procedure Solution_Set_StepsizeHr(Value: Double); CDECL;
procedure Solution_Set_StepsizeMin(Value: Double); CDECL;
function Solution_Get_ControlIterations(): Integer; CDECL;
function Solution_Get_MaxControlIterations(): Integer; CDECL;
procedure Solution_Sample_DoControlActions(); CDECL;
procedure Solution_Set_ControlIterations(Value: Integer); CDECL;
procedure Solution_Set_MaxControlIterations(Value: Integer); CDECL;
procedure Solution_CheckFaultStatus(); CDECL;
procedure Solution_SolveDirect(); CDECL;
procedure Solution_SolveNoControl(); CDECL;
procedure Solution_SolvePflow(); CDECL;
procedure Solution_SolvePlusControl(); CDECL;
procedure Solution_SolveSnap(); CDECL;
procedure Solution_CheckControls(); CDECL;
procedure Solution_InitSnap(); CDECL;
function Solution_Get_SystemYChanged(): TAPIBoolean; CDECL;
procedure Solution_BuildYMatrix(BuildOption, AllocateVI: Integer); CDECL;
procedure Solution_DoControlActions(); CDECL;
procedure Solution_SampleControlDevices(); CDECL;
function Solution_Get_Converged(): TAPIBoolean; CDECL;
procedure Solution_Set_Converged(Value: TAPIBoolean); CDECL;
function Solution_Get_Totaliterations(): Integer; CDECL;
function Solution_Get_MostIterationsDone(): Integer; CDECL;
function Solution_Get_ControlActionsDone(): TAPIBoolean; CDECL;
procedure Solution_Set_ControlActionsDone(Value: TAPIBoolean); CDECL;
procedure Solution_Cleanup(); CDECL;
procedure Solution_FinishTimeStep(); CDECL;
function Solution_Get_Process_Time(): Double; CDECL;
function Solution_Get_Total_Time(): Double; CDECL;
procedure Solution_Set_Total_Time(Value: Double); CDECL;
function Solution_Get_Time_of_Step(): Double; CDECL;
function Solution_Get_IntervalHrs(): Double; CDECL;
procedure Solution_Set_IntervalHrs(Value: Double); CDECL;
function Solution_Get_MinIterations(): Integer; CDECL;
procedure Solution_Set_MinIterations(Value: Integer); CDECL;
procedure Solution_SolveAll();cdecl;
procedure Solution_Get_IncMatrix(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
procedure Solution_Get_IncMatrix_GR(); CDECL;
procedure Solution_Get_Laplacian(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
procedure Solution_Get_Laplacian_GR(); CDECL;
procedure Solution_Get_BusLevels(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
procedure Solution_Get_BusLevels_GR(); CDECL;
procedure Solution_Get_IncMatrixRows(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Solution_Get_IncMatrixRows_GR(); CDECL;
procedure Solution_Get_IncMatrixCols(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Solution_Get_IncMatrixCols_GR(); CDECL;

implementation

uses
    CAPI_Constants,
    SysUtils,
    DSSGlobals,
    Math,
    LoadShape,
    Utilities,
    YMatrix,
    Solution,
    SolutionAlgs,
    ExecOptions,
    Sparse_Math,
    Dynamics,
    DSSClass,
    DSSHelper;

//------------------------------------------------------------------------------
function Solution_Get_Frequency(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.Frequency
end;
//------------------------------------------------------------------------------
function Solution_Get_Hour(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.DynaVars.intHour
end;
//------------------------------------------------------------------------------
function Solution_Get_Iterations(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.Iteration
end;
//------------------------------------------------------------------------------
function Solution_Get_LoadMult(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.LoadMultiplier
end;
//------------------------------------------------------------------------------
function Solution_Get_MaxIterations(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.MaxIterations
end;
//------------------------------------------------------------------------------
function Solution_Get_Mode(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Ord(DSSPrime.ActiveCircuit.Solution.Mode)
end;
//------------------------------------------------------------------------------
function Solution_Get_Number(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.NumberOfTimes
end;
//------------------------------------------------------------------------------
function Solution_Get_Random(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.RandomType
end;
//------------------------------------------------------------------------------
function Solution_Get_Seconds(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.dynavars.t
end;
//------------------------------------------------------------------------------
function Solution_Get_StepSize(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.dynavars.h
end;
//------------------------------------------------------------------------------
function Solution_Get_Tolerance(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.ConvergenceTolerance
end;
//------------------------------------------------------------------------------
function Solution_Get_Year(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.Year
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Frequency(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.Frequency := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Hour(Value: Integer); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.DynaVars.intHour := Value;
    DSSPrime.ActiveCircuit.Solution.Update_dblHour();
end;
//------------------------------------------------------------------------------
procedure Solution_Set_LoadMult(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.LoadMultiplier := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_MaxIterations(Value: Integer); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.MaxIterations := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Mode(Mode: Integer); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    if (Mode >= Ord(Low(TSolveMode))) and (Mode <= Ord(High(TSolveMode))) then
        DSSPrime.ActiveCircuit.Solution.Mode := TSolveMode(Mode)
    else
        DoSimpleMsg(DSSPrime, 'Invalid solution mode (%d).', [Mode], 5004);
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Number(Value: Integer); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.NumberOfTimes := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Random(Random: Integer); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.RandomType := Random;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Seconds(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    DSSPrime.ActiveCircuit.Solution.DynaVars.t := Value;
    DSSPrime.ActiveCircuit.Solution.Update_dblHour();
end;
//------------------------------------------------------------------------------
procedure Solution_Set_StepSize(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.dynavars.h := Value;
    Solution_Set_IntervalHrs(Value / 3600.0);     // Keep IntervalHrs in synch with time step size
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Tolerance(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.ConvergenceTolerance := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Year(Value: Integer); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.Year := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Solve(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.Solve;
end;
//------------------------------------------------------------------------------
function Solution_Get_ModeID(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.SolveModeEnum.OrdinalToString(ord(DSSPrime.ActiveCircuit.Solution.Mode)))
end;
//------------------------------------------------------------------------------
function Solution_Get_LoadModel(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.LoadModel
end;
//------------------------------------------------------------------------------
procedure Solution_Set_LoadModel(Value: Integer); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.LoadModel := Value;
    DSSPrime.ActiveCircuit.Solution.DefaultLoadModel := DSSPrime.ActiveCircuit.Solution.LoadModel;
end;
//------------------------------------------------------------------------------
function Solution_Get_LDCurve(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if InvalidCircuit(DSSPrime) then
        Exit;
    if DSSPrime.ActiveCircuit.LoadDurCurveObj <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.ActiveCircuit.LoadDurCurveObj.Name)
    else
        Result := DSS_GetAsPAnsiChar(DSSPrime, '');
end;
//------------------------------------------------------------------------------
procedure Solution_Set_LDCurve(const Value: PAnsiChar); CDECL;
var
    sValue: String;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    sValue := Value;
    DSSPrime.ActiveCircuit.LoadDurCurveObj := DSSPrime.LoadShapeClass.Find(sValue);
    if DSSPrime.ActiveCircuit.LoadDurCurveObj = NIL then
        DoSimpleMsg(DSSPrime, 'Load-Duration Curve "%s" not found.', [sValue], 5001);
end;
//------------------------------------------------------------------------------
function Solution_Get_pctGrowth(): Double; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := (DSSPrime.ActiveCircuit.DefaultGrowthRate - 1.0) * 100.0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_pctGrowth(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.DefaultGrowthRate := 1.0 + Value / 100.0;
    DSSPrime.ActiveCircuit.DefaultGrowthFactor := IntPower(DSSPrime.ActiveCircuit.DefaultGrowthRate, (DSSPrime.ActiveCircuit.Solution.Year - 1));
end;
//------------------------------------------------------------------------------
function Solution_Get_AddType(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.AutoAddObj.AddType
end;
//------------------------------------------------------------------------------
procedure Solution_Set_AddType(Value: Integer); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.AutoAddObj.AddType := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_GenkW(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.AutoAddObj.GenkW
end;
//------------------------------------------------------------------------------
procedure Solution_Set_GenkW(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.AutoAddObj.GenkW := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_GenPF(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.AutoAddObj.GenPF
end;
//------------------------------------------------------------------------------
procedure Solution_Set_GenPF(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.AutoAddObj.GenPF := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_Capkvar(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.AutoAddObj.Capkvar
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Capkvar(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.AutoAddObj.Capkvar := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_Algorithm(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.Algorithm
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Algorithm(Value: Integer); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.Algorithm := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_ControlMode(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.ControlMode
end;
//------------------------------------------------------------------------------
procedure Solution_Set_ControlMode(Value: Integer); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.ControlMode := Value;
    DSSPrime.ActiveCircuit.Solution.DefaultControlMode := DSSPrime.ActiveCircuit.Solution.ControlMode;
end;
//------------------------------------------------------------------------------
function Solution_Get_GenMult(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.GenMultiplier
end;
//------------------------------------------------------------------------------
procedure Solution_Set_GenMult(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.GenMultiplier := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_DefaultDaily(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.ActiveCircuit.DefaultDailyShapeObj.Name)
end;

//------------------------------------------------------------------------------
function Solution_Get_DefaultYearly(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.ActiveCircuit.DefaultYearlyShapeObj.Name)
end;
//------------------------------------------------------------------------------
procedure Solution_Set_DefaultDaily(const Value: PAnsiChar); CDECL;
var
    TestLoadShapeObj: TLoadShapeObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    TestLoadShapeObj := DSSPrime.LoadShapeClass.Find(Value);
    if TestLoadShapeObj = NIL then
        Exit;
    DSSPrime.ActiveCircuit.DefaultDailyShapeObj := TestLoadShapeObj;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_DefaultYearly(const Value: PAnsiChar); CDECL;
var
    TestLoadShapeObj: TLoadShapeObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    TestLoadShapeObj := DSSPrime.LoadShapeClass.Find(Value);
    if TestLoadShapeObj = NIL then
        Exit;
    DSSPrime.ActiveCircuit.DefaultYearlyShapeObj := TestLoadShapeObj;
end;
//------------------------------------------------------------------------------
procedure Solution_Get_EventLog(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray0;
    i: Integer;
begin
    if InvalidCircuit(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount, '');
        Exit;
    end;
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, DSSPrime.EventStrings.Count);
    for i := 0 to DSSPrime.EventStrings.Count - 1 do
    begin
        Result[i] := DSS_CopyStringAsPChar(DSSPrime.EventStrings.Strings[i]);
    end;
end;

procedure Solution_Get_EventLog_GR(); CDECL;
// Same as Solution_Get_EventLog but uses global result (GR) pointers
begin
    Solution_Get_EventLog(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function Solution_Get_dblHour(): Double; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.DynaVars.dblHour;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_dblHour(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    DSSPrime.ActiveCircuit.Solution.DynaVars.intHour := Trunc(Value);
    DSSPrime.ActiveCircuit.Solution.DynaVars.dblHour := Value;
    DSSPrime.ActiveCircuit.Solution.Dynavars.t := (Value - DSSPrime.ActiveCircuit.Solution.DynaVars.intHour) * 3600.0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_StepsizeHr(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.Dynavars.h := Value * 3600.0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_StepsizeMin(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.Dynavars.h := Value * 60.0;
end;
//------------------------------------------------------------------------------
function Solution_Get_ControlIterations(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.ControlIteration;
end;
//------------------------------------------------------------------------------
function Solution_Get_MaxControlIterations(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.MaxControlIterations;
end;
//------------------------------------------------------------------------------
procedure Solution_Sample_DoControlActions(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.Sample_DoControlActions;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_ControlIterations(Value: Integer); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.ControlIteration := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_MaxControlIterations(Value: Integer); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.MaxControlIterations := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_CheckFaultStatus(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.Check_Fault_Status;
end;
//------------------------------------------------------------------------------
procedure Solution_SolveDirect(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.SolveDirect;
end;
//------------------------------------------------------------------------------
procedure Solution_SolveNoControl(); CDECL;
// Solves without checking controls
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.SolveCircuit;
end;
//------------------------------------------------------------------------------
procedure Solution_SolvePflow(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.DoPflowSolution;
end;
//------------------------------------------------------------------------------
procedure Solution_SolvePlusControl(); CDECL;
// One Pass Through the solution and then dispatches controls
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.SolveCircuit();
    DSSPrime.ActiveCircuit.Solution.CheckControls();
end;
//------------------------------------------------------------------------------
procedure Solution_SolveSnap(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.SolveSnap;
end;
//------------------------------------------------------------------------------
procedure Solution_CheckControls(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.CheckControls;
end;
//------------------------------------------------------------------------------
procedure Solution_InitSnap(); CDECL;
// Initi some things that are done at the beginning of a snapshot solve
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.SnapShotInit;
end;
//------------------------------------------------------------------------------
function Solution_Get_SystemYChanged(): TAPIBoolean; CDECL;
begin
    Result := False;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.SystemYChanged;
end;
//------------------------------------------------------------------------------
procedure Solution_BuildYMatrix(BuildOption, AllocateVI: Integer); CDECL;
// Build Options
//   1 = Series elements only
//   2 = Whole Y matrix
//
// AllocateVI
//   TRUE:  Reallocate VI
//   FALSE: Do not Reallocate VI; leave as is
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    Ymatrix.BuildYMatrix(DSSPrime, BuildOption, AllocateVI <> 0)
end;
//------------------------------------------------------------------------------
procedure Solution_DoControlActions(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.DoControlActions;
end;
//------------------------------------------------------------------------------
procedure Solution_SampleControlDevices(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.SampleControlDevices;
end;
//------------------------------------------------------------------------------
function Solution_Get_Converged(): TAPIBoolean; CDECL;
begin
    Result := False;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Issolved;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Converged(Value: TAPIBoolean); CDECL;
// Set the flag directly to force its setting
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.ConvergedFlag := Value;
    DSSPrime.ActiveCircuit.Issolved := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_Totaliterations(): Integer; CDECL;
// Same as Iterations interface
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.Iteration
end;
//------------------------------------------------------------------------------
function Solution_Get_MostIterationsDone(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.MostIterationsDone
end;
//------------------------------------------------------------------------------
function Solution_Get_ControlActionsDone(): TAPIBoolean; CDECL;
begin
    Result := False;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.ControlActionsDone;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_ControlActionsDone(Value: TAPIBoolean); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.ControlActionsDone := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Cleanup(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.EndOfTimeStepCleanup();
end;
//------------------------------------------------------------------------------
procedure Solution_FinishTimeStep(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.MonitorClass.SampleAll();  // Make all monitors take a sample
    DSSPrime.ActiveCircuit.Solution.EndOfTimeStepCleanup();
    DSSPrime.ActiveCircuit.Solution.IncrementTime();
    // DefaultHourMult := DefaultDailyShapeObj.getmult(TDynamicsrec.dblHour);
end;
//------------------------------------------------------------------------------
function Solution_Get_Process_Time(): Double; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.Solve_Time_Elapsed;
end;
//------------------------------------------------------------------------------
function Solution_Get_Total_Time(): Double; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.Total_Time_Elapsed;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Total_Time(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.Total_Time_Elapsed := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_Time_of_Step(): Double; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.Step_Time_Elapsed;
end;
//------------------------------------------------------------------------------
function Solution_Get_IntervalHrs(): Double; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.IntervalHrs;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_IntervalHrs(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.IntervalHrs := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_MinIterations(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Solution.MinIterations
end;
//------------------------------------------------------------------------------
procedure Solution_Set_MinIterations(Value: Integer); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.Solution.MinIterations := Value;
end;

//------------------------------------------------------------------------------
procedure Solution_SolveAll();cdecl;
{$IFDEF DSS_CAPI_PM}
var
    i : Integer;
    PMParent: TDSSContext;
begin
    PMParent := DSSPrime.GetPrime();
    for i := 0 to High(PMParent.Children) do
    begin
        PMParent.ActiveChild := PMParent.Children[i];
        DSSPrime.CmdResult := DoSetCmd(PMParent.Children[i], 1);
    end;
{$ELSE}
begin
    DoSimpleMsg(DSSPrime, _('Parallel machine functions were not compiled'), 7983);
{$ENDIF}
end;
//------------------------------------------------------------------------------
procedure Solution_Get_Laplacian(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
var
    Result: PIntegerArray0;
    i,
    Counter,
    IMIdx,
    ArrSize: Integer;
    Laplacian: Tsparse_matrix;
begin
    if (not InvalidCircuit(DSSPrime)) and (DSSPrime.ActiveCircuit.Solution.Laplacian <> NIL) then
    begin
        Laplacian := DSSPrime.ActiveCircuit.Solution.Laplacian;
        ArrSize := Laplacian.NZero * 3;
        Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, ArrSize + 1); //TODO: remove the +1
        Counter := 0;
        IMIdx := 0;
        while IMIdx < ArrSize do
        begin
            for i := 0 to 2 do
            begin
                Result[IMIdx] := Laplacian.data[Counter][i];
                inc(IMIdx)
            end;
            inc(Counter)
        end;
        Exit;
    end;
    DefaultResult(ResultPtr, ResultCount);
end;

procedure Solution_Get_Laplacian_GR(); CDECL;
// Same as Solution_Get_Laplacian but uses global result (GR) pointers
begin
    Solution_Get_Laplacian(DSSPrime.GR_DataPtr_PInteger, @DSSPrime.GR_Counts_PInteger[0])
end;

//------------------------------------------------------------------------------
procedure Solution_Get_IncMatrix(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
var
    Result: PIntegerArray0;
    i,
    Counter,
    IMIdx,
    ArrSize: Integer;
    IncMat: Tsparse_matrix;
begin
    if (not InvalidCircuit(DSSPrime)) and (DSSPrime.ActiveCircuit.Solution.IncMat <> NIL) then
    begin
        IncMat := DSSPrime.ActiveCircuit.Solution.IncMat;
        ArrSize := IncMat.NZero * 3;
        Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, ArrSize + 1); // TODO: remove +1? Left for compatibility with the official version
        Counter := 0;
        IMIdx := 0;
        while IMIdx < ArrSize do
        begin
            for i := 0 to 2 do
            begin
                Result[IMIdx] := IncMat.data[Counter][i];
                inc(IMIdx)
            end;
            inc(Counter)
        end;
        Exit;
    end;
    DefaultResult(ResultPtr, ResultCount);
end;

procedure Solution_Get_IncMatrix_GR(); CDECL;
// Same as Solution_Get_IncMatrix but uses global result (GR) pointers
begin
    Solution_Get_IncMatrix(DSSPrime.GR_DataPtr_PInteger, @DSSPrime.GR_Counts_PInteger[0])
end;

//------------------------------------------------------------------------------
procedure Solution_Get_BusLevels(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
        
    DSS_RecreateArray_PInteger(ResultPtr, ResultCount, length(DSSPrime.ActiveCircuit.Solution.Inc_Mat_Levels));
    Move(DSSPrime.ActiveCircuit.Solution.Inc_Mat_levels[0], ResultPtr^, ResultCount^ * SizeOf(Integer));
end;

procedure Solution_Get_BusLevels_GR(); CDECL;
// Same as Solution_Get_BusLevels but uses global result (GR) pointers
begin
    Solution_Get_BusLevels(DSSPrime.GR_DataPtr_PInteger, @DSSPrime.GR_Counts_PInteger[0])
end;

//------------------------------------------------------------------------------
procedure Solution_Get_IncMatrixRows(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray0;
    IMIdx,
    ArrSize: Integer;
begin
    if InvalidCircuit(DSSPrime) or (DSSPrime.ActiveCircuit.Solution.Inc_Mat_Rows = NIL) then
    begin
        DefaultResult(ResultPtr, ResultCount, '');
        Exit;
    end;
    ArrSize := length(DSSPrime.ActiveCircuit.Solution.Inc_Mat_Rows);
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, ArrSize);
    for IMIdx := 0 to (ArrSize - 1) do
    begin
        Result[IMIdx] := DSS_CopyStringAsPChar(DSSPrime.ActiveCircuit.Solution.Inc_Mat_Rows[IMIdx]);
    end;
end;

procedure Solution_Get_IncMatrixRows_GR(); CDECL;
// Same as Solution_Get_IncMatrixRows but uses global result (GR) pointers
begin
    Solution_Get_IncMatrixRows(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
procedure Solution_Get_IncMatrixCols(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray0;
    i,
    IMIdx,
    ArrSize: Integer;
begin
    if InvalidCircuit(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount, '');
        Exit;
    end;
    if DSSPrime.IncMat_Ordered then
    begin
        if DSSPrime.ActiveCircuit.Solution.Inc_Mat_Cols = NIL then
        begin
            DefaultResult(ResultPtr, ResultCount, '');
            Exit;
        end;
            
        ArrSize := length(DSSPrime.ActiveCircuit.Solution.Inc_Mat_Cols);
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, ArrSize);
        for IMIdx := 0 to (ArrSize - 1) do
        begin
            Result[IMIdx] := DSS_CopyStringAsPChar(DSSPrime.ActiveCircuit.Solution.Inc_Mat_Cols[IMIdx]);
        end;
    end
    else
    begin
        if DSSPrime.ActiveCircuit.NumBuses = 0 then
        begin
            DefaultResult(ResultPtr, ResultCount, '');
            Exit;
        end;

        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.NumBuses);
        for i := 0 to DSSPrime.ActiveCircuit.NumBuses - 1 do
        begin
            Result[i] := DSS_CopyStringAsPChar(DSSPrime.ActiveCircuit.BusList.NameOfIndex(i + 1));
        end;
    end;
end;

procedure Solution_Get_IncMatrixCols_GR(); CDECL;
// Same as Solution_Get_IncMatrixCols but uses global result (GR) pointers
begin
    Solution_Get_IncMatrixCols(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
end.
