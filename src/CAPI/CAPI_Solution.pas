unit CAPI_Solution;

interface

uses
    CAPI_Utils;

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
procedure Solution_Get_EventLog(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
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
function Solution_Get_SystemYChanged(): Wordbool; CDECL;
procedure Solution_BuildYMatrix(BuildOption, AllocateVI: Integer); CDECL;
procedure Solution_DoControlActions(); CDECL;
procedure Solution_SampleControlDevices(); CDECL;
function Solution_Get_Converged(): Wordbool; CDECL;
procedure Solution_Set_Converged(Value: Wordbool); CDECL;
function Solution_Get_Totaliterations(): Integer; CDECL;
function Solution_Get_MostIterationsDone(): Integer; CDECL;
function Solution_Get_ControlActionsDone(): Wordbool; CDECL;
procedure Solution_Set_ControlActionsDone(Value: Wordbool); CDECL;
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
{V8-ONLY>
procedure Solution_SolveAll();cdecl;
<V8-ONLY}
procedure Solution_Get_IncMatrix(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
procedure Solution_Get_IncMatrix_GR(); CDECL;
procedure Solution_Get_Laplacian(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
procedure Solution_Get_Laplacian_GR(); CDECL;
procedure Solution_Get_BusLevels(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
procedure Solution_Get_BusLevels_GR(); CDECL;
procedure Solution_Get_IncMatrixRows(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure Solution_Get_IncMatrixRows_GR(); CDECL;
procedure Solution_Get_IncMatrixCols(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure Solution_Get_IncMatrixCols_GR(); CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    Math,
    LoadShape,
    Utilities,
    YMatrix,
    SolutionAlgs,
    Solution,
    ExecOptions;

//------------------------------------------------------------------------------
function Solution_Get_Frequency(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.Frequency
end;
//------------------------------------------------------------------------------
function Solution_Get_Hour(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.DynaVars.intHour
end;
//------------------------------------------------------------------------------
function Solution_Get_Iterations(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.Iteration
end;
//------------------------------------------------------------------------------
function Solution_Get_LoadMult(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.LoadMultiplier
end;
//------------------------------------------------------------------------------
function Solution_Get_MaxIterations(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.MaxIterations
end;
//------------------------------------------------------------------------------
function Solution_Get_Mode(): Integer; CDECL;
begin
     //If not InvalidCircuit Then Result := GetSolutionModeID      changed to integer 8/16/00
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.Mode
end;
//------------------------------------------------------------------------------
function Solution_Get_Number(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.NumberOfTimes
end;
//------------------------------------------------------------------------------
function Solution_Get_Random(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.RandomType
end;
//------------------------------------------------------------------------------
function Solution_Get_Seconds(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.dynavars.t
end;
//------------------------------------------------------------------------------
function Solution_Get_StepSize(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.dynavars.h
end;
//------------------------------------------------------------------------------
function Solution_Get_Tolerance(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.ConvergenceTolerance
end;
//------------------------------------------------------------------------------
function Solution_Get_Year(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.Year
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Frequency(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.Frequency := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Hour(Value: Integer); CDECL;
begin
    if InvalidCircuit then
        Exit;
    with  ActiveCircuit.Solution do
    begin
        DynaVars.intHour := Value;
        Update_dblHour;
    end;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_LoadMult(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.LoadMultiplier := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_MaxIterations(Value: Integer); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.MaxIterations := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Mode(Mode: Integer); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.Mode := Mode; //InterpretSolveMode(Value);
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Number(Value: Integer); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.NumberOfTimes := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Random(Random: Integer); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.RandomType := Random;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Seconds(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.dynavars.t := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_StepSize(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.dynavars.h := Value;
    Solution_Set_IntervalHrs(Value / 3600.0);     // Keep IntervalHrs in synch with time step size
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Tolerance(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.ConvergenceTolerance := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Year(Value: Integer); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.Year := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Solve(); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.Solve;
end;
//------------------------------------------------------------------------------
function Solution_Get_ModeID(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if InvalidCircuit then
        Exit;
    Result := DSS_GetAsPAnsiChar(GetSolutionModeID)
end;
//------------------------------------------------------------------------------
function Solution_Get_LoadModel(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.LoadModel
end;
//------------------------------------------------------------------------------
procedure Solution_Set_LoadModel(Value: Integer); CDECL;
begin
    if InvalidCircuit then
        Exit;
    with ActiveCircuit.Solution do
    begin
        LoadModel := Value;
        DefaultLoadModel := LoadModel;
    end;
end;
//------------------------------------------------------------------------------
function Solution_Get_LDCurve(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if InvalidCircuit then
        Exit;
    Result := DSS_GetAsPAnsiChar(ActiveCircuit.LoadDurCurve)
end;
//------------------------------------------------------------------------------
procedure Solution_Set_LDCurve(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit then
        Exit;
    with ActiveCircuit do
    begin
        LoadDurCurve := Value;
        LoadDurCurveObj := LoadShapeClass.Find(LoadDurCurve);
        if LoadDurCurveObj = NIL then
            DoSimpleMsg('Load-Duration Curve not found.', 5001);
    end;
end;
//------------------------------------------------------------------------------
function Solution_Get_pctGrowth(): Double; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    with ActiveCircuit do
    begin
        Result := (DefaultGrowthRate - 1.0) * 100.0;
        Exit;
    end;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_pctGrowth(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    with ActiveCircuit do
    begin
        DefaultGrowthRate := 1.0 + Value / 100.0;
        DefaultGrowthFactor := IntPower(DefaultGrowthRate, (Solution.Year - 1));
    end;
end;
//------------------------------------------------------------------------------
function Solution_Get_AddType(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.AutoAddObj.AddType
end;
//------------------------------------------------------------------------------
procedure Solution_Set_AddType(Value: Integer); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.AutoAddObj.AddType := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_GenkW(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.AutoAddObj.GenkW
end;
//------------------------------------------------------------------------------
procedure Solution_Set_GenkW(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.AutoAddObj.GenkW := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_GenPF(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.AutoAddObj.GenPF
end;
//------------------------------------------------------------------------------
procedure Solution_Set_GenPF(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.AutoAddObj.GenPF := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_Capkvar(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.AutoAddObj.Capkvar
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Capkvar(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.AutoAddObj.Capkvar := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_Algorithm(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.Algorithm
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Algorithm(Value: Integer); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.Algorithm := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_ControlMode(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.ControlMode
end;
//------------------------------------------------------------------------------
procedure Solution_Set_ControlMode(Value: Integer); CDECL;
begin
    if InvalidCircuit then
        Exit;
    with ActiveCircuit.Solution do
    begin
        ControlMode := Value;
        DefaultControlMode := ControlMode;
    end;
end;
//------------------------------------------------------------------------------
function Solution_Get_GenMult(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.GenMultiplier
end;
//------------------------------------------------------------------------------
procedure Solution_Set_GenMult(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.GenMultiplier := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_DefaultDaily(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if InvalidCircuit then
        Exit;
    Result := DSS_GetAsPAnsiChar(ActiveCircuit.DefaultDailyShapeObj.Name)
end;

//------------------------------------------------------------------------------
function Solution_Get_DefaultYearly(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if InvalidCircuit then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSS_GetAsPAnsiChar(ActiveCircuit.DefaultYearlyShapeObj.Name))
end;
//------------------------------------------------------------------------------
procedure Solution_Set_DefaultDaily(const Value: PAnsiChar); CDECL;
var
    TestLoadShapeObj: TLoadShapeObj;
begin
    if InvalidCircuit then
        Exit;
    TestLoadShapeObj := LoadShapeClass.Find(Value);
    if TestLoadShapeObj = NIL then
        Exit;
    ActiveCircuit.DefaultDailyShapeObj := TestLoadShapeObj;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_DefaultYearly(const Value: PAnsiChar); CDECL;
var
    TestLoadShapeObj: TLoadShapeObj;
begin
    if InvalidCircuit then
        Exit;
    TestLoadShapeObj := LoadShapeClass.Find(Value);
    if TestLoadShapeObj = NIL then
        Exit;
    ActiveCircuit.DefaultYearlyShapeObj := TestLoadShapeObj;
end;
//------------------------------------------------------------------------------
procedure Solution_Get_EventLog(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    i: Integer;
begin
    if InvalidCircuit then
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
        Exit;
    end;
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (EventStrings.Count - 1) + 1);
    for i := 0 to EventStrings.Count - 1 do
    begin
        Result[i] := DSS_CopyStringAsPChar(EventStrings.Strings[i]);
    end;
end;

procedure Solution_Get_EventLog_GR(); CDECL;
// Same as Solution_Get_EventLog but uses global result (GR) pointers
begin
    Solution_Get_EventLog(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Solution_Get_dblHour(): Double; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.DynaVars.dblHour;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_dblHour(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;

    with ActiveCircuit.Solution do
    begin
        DynaVars.intHour := Trunc(Value);
        DynaVars.dblHour := Value;
        Dynavars.t := (Value - DynaVars.intHour) * 3600.0;
    end;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_StepsizeHr(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.Dynavars.h := Value * 3600.0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_StepsizeMin(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.Dynavars.h := Value * 60.0;
end;
//------------------------------------------------------------------------------
function Solution_Get_ControlIterations(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.ControlIteration;
end;
//------------------------------------------------------------------------------
function Solution_Get_MaxControlIterations(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.MaxControlIterations;
end;
//------------------------------------------------------------------------------
procedure Solution_Sample_DoControlActions(); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.Sample_DoControlActions;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_ControlIterations(Value: Integer); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.ControlIteration := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_MaxControlIterations(Value: Integer); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.MaxControlIterations := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_CheckFaultStatus(); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.Check_Fault_Status;
end;
//------------------------------------------------------------------------------
procedure Solution_SolveDirect(); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.SolveDirect;
end;
//------------------------------------------------------------------------------
procedure Solution_SolveNoControl(); CDECL;
{Solves without checking controls}
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.SolveCircuit;
end;
//------------------------------------------------------------------------------
procedure Solution_SolvePflow(); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.DoPflowSolution;
end;
//------------------------------------------------------------------------------
procedure Solution_SolvePlusControl(); CDECL;
{One Pass Through the solution and then dispatches controls}
begin
    if InvalidCircuit then
        Exit;
    with ActiveCircuit.Solution do
    begin
        SolveCircuit;
        CheckControls;
    end;
end;
//------------------------------------------------------------------------------
procedure Solution_SolveSnap(); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.SolveSnap;
end;
//------------------------------------------------------------------------------
procedure Solution_CheckControls(); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.CheckControls;
end;
//------------------------------------------------------------------------------
procedure Solution_InitSnap(); CDECL;
{Initi some things that are done at the beginning of a snapshot solve}
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.SnapShotInit;
end;
//------------------------------------------------------------------------------
function Solution_Get_SystemYChanged(): Wordbool; CDECL;
begin
    Result := False;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.SystemYChanged;
end;
//------------------------------------------------------------------------------
procedure Solution_BuildYMatrix(BuildOption, AllocateVI: Integer); CDECL;
{
  Build Options
    1 = Series elements only
    2 = Whole Y matrix

  AllocateVI
    TRUE:  Reallocate VI
    FALSE: Do not Reallocate VI; leave as is
}
begin
    if InvalidCircuit then
        Exit;
    Ymatrix.BuildYMatrix(BuildOption, AllocateVI <> 0)
end;
//------------------------------------------------------------------------------
procedure Solution_DoControlActions(); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.DoControlActions;
end;
//------------------------------------------------------------------------------
procedure Solution_SampleControlDevices(); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.SampleControlDevices;
end;
//------------------------------------------------------------------------------
function Solution_Get_Converged(): Wordbool; CDECL;
begin
    Result := False;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Issolved;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Converged(Value: Wordbool); CDECL;
{Set the flag directly to force its setting}
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.ConvergedFlag := Value;
    ActiveCircuit.Issolved := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_Totaliterations(): Integer; CDECL;
// Same as Iterations interface
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.Iteration
end;
//------------------------------------------------------------------------------
function Solution_Get_MostIterationsDone(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.MostIterationsDone
end;
//------------------------------------------------------------------------------
function Solution_Get_ControlActionsDone(): Wordbool; CDECL;
begin
    Result := False;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.ControlActionsDone;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_ControlActionsDone(Value: Wordbool); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.ControlActionsDone := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Cleanup(); CDECL;
begin
    if InvalidCircuit then
        Exit;
    EndOfTimeStepCleanup();
end;
//------------------------------------------------------------------------------
procedure Solution_FinishTimeStep(); CDECL;
begin
    if InvalidCircuit then
        Exit;
    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        MonitorClass.SampleAll();  // Make all monitors take a sample
        EndOfTimeStepCleanup();
        Increment_time();
//               DefaultHourMult := DefaultDailyShapeObj.getmult(TDynamicsrec.dblHour);
    end;
end;
//------------------------------------------------------------------------------
function Solution_Get_Process_Time(): Double; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.Time_Solve;
end;
//------------------------------------------------------------------------------
function Solution_Get_Total_Time(): Double; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.Total_Time;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Total_Time(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.Total_Time := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_Time_of_Step(): Double; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.Time_Step;
end;
//------------------------------------------------------------------------------
function Solution_Get_IntervalHrs(): Double; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.IntervalHrs;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_IntervalHrs(Value: Double); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.IntervalHrs := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_MinIterations(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Solution.MinIterations
end;
//------------------------------------------------------------------------------
procedure Solution_Set_MinIterations(Value: Integer); CDECL;
begin
    if InvalidCircuit then
        Exit;
    ActiveCircuit.Solution.MinIterations := Value;
end;

{V8-ONLY>
//------------------------------------------------------------------------------
procedure Solution_SolveAll();cdecl;
var
  i : Integer;
begin
  for i := 1 to NumOfActors do
  begin
    ActiveActor :=  i;
    CmdResult   :=  DoSetCmd(1);
  end;
end;
<V8-ONLY}
//------------------------------------------------------------------------------
procedure Solution_Get_Laplacian(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
var
    Result: PIntegerArray;
    i,
    Counter,
    IMIdx,
    ArrSize: Integer;
begin
    if (not InvalidCircuit) and (ActiveCircuit.Solution.Laplacian <> NIL) then
    begin
        with ActiveCircuit.Solution do
        begin
            ArrSize := Laplacian.NZero * 3;
            Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (ArrSize) + 1);
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
    end;
    Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, 1);
end;

procedure Solution_Get_Laplacian_GR(); CDECL;
// Same as Solution_Get_Laplacian but uses global result (GR) pointers
begin
    Solution_Get_Laplacian(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
procedure Solution_Get_IncMatrix(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
var
    Result: PIntegerArray;
    i,
    Counter,
    IMIdx,
    ArrSize: Integer;
begin
    if (not InvalidCircuit) and (ActiveCircuit.Solution.IncMat <> NIL) then
    begin
        with ActiveCircuit.Solution do
        begin
            ArrSize := IncMat.NZero * 3;
            Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (ArrSize) + 1);
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
    end;
    Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, 1);
end;

procedure Solution_Get_IncMatrix_GR(); CDECL;
// Same as Solution_Get_IncMatrix but uses global result (GR) pointers
begin
    Solution_Get_IncMatrix(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
procedure Solution_Get_BusLevels(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
begin
    if InvalidCircuit then
    begin
        DSS_RecreateArray_PInteger(ResultPtr, ResultCount, 1);
        Exit;
    end;
        
    with ActiveCircuit.Solution do
    begin
        DSS_RecreateArray_PInteger(ResultPtr, ResultCount, length(Inc_Mat_Levels));
        Move(Inc_Mat_levels[0], ResultPtr^, length(Inc_Mat_Levels) * SizeOf(Integer));
        Exit;
    end;
end;

procedure Solution_Get_BusLevels_GR(); CDECL;
// Same as Solution_Get_BusLevels but uses global result (GR) pointers
begin
    Solution_Get_BusLevels(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
procedure Solution_Get_IncMatrixRows(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    IMIdx,
    ArrSize: Integer;
begin
    if InvalidCircuit or (ActiveCircuit.Solution.Inc_Mat_Rows = NIL) then
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
        Exit;
    end;
    with ActiveCircuit.Solution do
    begin
        ArrSize := length(Inc_Mat_Rows);
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, ArrSize);
        for IMIdx := 0 to (ArrSize - 1) do
        begin
            Result[IMIdx] := DSS_CopyStringAsPChar(Inc_Mat_Rows[IMIdx]);
        end;
    end;
end;

procedure Solution_Get_IncMatrixRows_GR(); CDECL;
// Same as Solution_Get_IncMatrixRows but uses global result (GR) pointers
begin
    Solution_Get_IncMatrixRows(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
procedure Solution_Get_IncMatrixCols(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    i,
    IMIdx,
    ArrSize: Integer;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    if InvalidCircuit then
        Exit;
    with ActiveCircuit.Solution, ActiveCircuit do
    begin
        if IncMat_Ordered then
        begin
            if Inc_Mat_Cols = NIL then
                Exit;
                
            ArrSize := length(Inc_Mat_Cols);
            Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, ArrSize);
            for IMIdx := 0 to (ArrSize - 1) do
            begin
                Result[IMIdx] := DSS_CopyStringAsPChar(Inc_Mat_Cols[IMIdx]);
            end;
        end
        else
        begin
            if NumBuses = 0 then
                Exit;

            Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, NumBuses);
            for i := 0 to NumBuses - 1 do
            begin
                Result[i] := DSS_CopyStringAsPChar(BusList.NameOfIndex(i + 1));
            end;
        end;
    end;
end;

procedure Solution_Get_IncMatrixCols_GR(); CDECL;
// Same as Solution_Get_IncMatrixCols but uses global result (GR) pointers
begin
    Solution_Get_IncMatrixCols(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
end.
