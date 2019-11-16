unit CAPI_Solution;

{$inline on}

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
function Solution_Get_SystemYChanged(): Boolean; CDECL;
procedure Solution_BuildYMatrix(BuildOption, AllocateVI: Integer); CDECL;
procedure Solution_DoControlActions(); CDECL;
procedure Solution_SampleControlDevices(); CDECL;
function Solution_Get_Converged(): Boolean; CDECL;
procedure Solution_Set_Converged(Value: Boolean); CDECL;
function Solution_Get_Totaliterations(): Integer; CDECL;
function Solution_Get_MostIterationsDone(): Integer; CDECL;
function Solution_Get_ControlActionsDone(): Boolean; CDECL;
procedure Solution_Set_ControlActionsDone(Value: Boolean); CDECL;
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

function Solution_Get_Frequency(): Double; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.Frequency
    else
        Result := 0.0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Hour(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.DynaVars.intHour
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Iterations(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.Iteration
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_LoadMult(): Double; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.LoadMultiplier
    else
        Result := 0.0;
end;
//------------------------------------------------------------------------------
function Solution_Get_MaxIterations(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.MaxIterations
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Mode(): Integer; CDECL;
begin
     //If ActiveCircuit <> Nil Then Result := GetSolutionModeID      changed to integer 8/16/00
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.Mode
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Number(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.NumberOfTimes
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Random(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.RandomType
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Seconds(): Double; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.dynavars.t
    else
        Result := 0.0;
end;
//------------------------------------------------------------------------------
function Solution_Get_StepSize(): Double; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.dynavars.h
    else
        Result := 0.0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Tolerance(): Double; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.ConvergenceTolerance
    else
        Result := 0.0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Year(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.Year
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Frequency(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.Solution.Frequency := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Hour(Value: Integer); CDECL;
begin
    if ActiveCircuit <> NIL then
        with  ActiveCircuit.Solution do
        begin
            DynaVars.intHour := Value;
            Update_dblHour;
        end;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_LoadMult(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.LoadMultiplier := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_MaxIterations(Value: Integer); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.Solution.MaxIterations := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Mode(Mode: Integer); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.Solution.Mode := Mode; //InterpretSolveMode(Value);
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Number(Value: Integer); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.Solution.NumberOfTimes := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Random(Random: Integer); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.Solution.RandomType := Random;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Seconds(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.Solution.dynavars.t := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_StepSize(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
    begin
        ActiveCircuit.Solution.dynavars.h := Value;
        Solution_Set_IntervalHrs(Value / 3600.0);     // Keep IntervalHrs in synch with time step size
    end;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Tolerance(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.Solution.ConvergenceTolerance := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Year(Value: Integer); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.Solution.Year := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Solve(); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.Solution.Solve;
end;
//------------------------------------------------------------------------------
function Solution_Get_ModeID_AnsiString(): Ansistring; inline;
begin
    if ActiveCircuit <> NIL then
        Result := GetSolutionModeID
    else
        Result := '';
end;

function Solution_Get_ModeID(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Solution_Get_ModeID_AnsiString());
end;
//------------------------------------------------------------------------------
function Solution_Get_LoadModel(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.LoadModel
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_LoadModel(Value: Integer); CDECL;
begin

    if ActiveCircuit <> NIL then
        with ActiveCircuit.Solution do
        begin
            LoadModel := Value;
            DefaultLoadModel := LoadModel;
        end;

end;
//------------------------------------------------------------------------------
function Solution_Get_LDCurve_AnsiString(): Ansistring; inline;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.LoadDurCurve
    else
        Result := '';
end;

function Solution_Get_LDCurve(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Solution_Get_LDCurve_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Solution_Set_LDCurve(const Value: PAnsiChar); CDECL;
begin
    if ActiveCircuit <> NIL then
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
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            Result := (DefaultGrowthRate - 1.0) * 100.0;
            Exit;
        end;
    Result := 0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_pctGrowth(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            DefaultGrowthRate := 1.0 + Value / 100.0;
            DefaultGrowthFactor := IntPower(DefaultGrowthRate, (Solution.Year - 1));
        end;
end;
//------------------------------------------------------------------------------
function Solution_Get_AddType(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.AutoAddObj.AddType
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_AddType(Value: Integer); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.AutoAddObj.AddType := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_GenkW(): Double; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.AutoAddObj.GenkW
    else
        Result := 0.0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_GenkW(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.AutoAddObj.GenkW := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_GenPF(): Double; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.AutoAddObj.GenPF
    else
        Result := 0.0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_GenPF(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.AutoAddObj.GenPF := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_Capkvar(): Double; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.AutoAddObj.Capkvar
    else
        Result := 0.0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Capkvar(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.AutoAddObj.Capkvar := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_Algorithm(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.Algorithm
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Algorithm(Value: Integer); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.Solution.Algorithm := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_ControlMode(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.ControlMode
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_ControlMode(Value: Integer); CDECL;
begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit.Solution do
        begin
            ControlMode := Value;
            DefaultControlMode := ControlMode;
        end;
end;
//------------------------------------------------------------------------------
function Solution_Get_GenMult(): Double; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.GenMultiplier
    else
        Result := 0.0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_GenMult(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.GenMultiplier := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_DefaultDaily_AnsiString(): Ansistring; inline;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.DefaultDailyShapeObj.Name
    else
        Result := '';
end;

function Solution_Get_DefaultDaily(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Solution_Get_DefaultDaily_AnsiString());
end;
//------------------------------------------------------------------------------
function Solution_Get_DefaultYearly_AnsiString(): Ansistring; inline;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.DefaultYearlyShapeObj.Name
    else
        Result := '';
end;

function Solution_Get_DefaultYearly(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Solution_Get_DefaultYearly_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Solution_Set_DefaultDaily(const Value: PAnsiChar); CDECL;
var
    TestLoadShapeObj: TLoadShapeObj;
begin
    if ActiveCircuit <> NIL then
    begin
        TestLoadShapeObj := LoadShapeClass.Find(Value);
        if TestLoadShapeObj <> NIL then
            ActiveCircuit.DefaultDailyShapeObj := TestLoadShapeObj;
    end;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_DefaultYearly(const Value: PAnsiChar); CDECL;
var
    TestLoadShapeObj: TLoadShapeObj;
begin
    if ActiveCircuit <> NIL then
    begin
        TestLoadShapeObj := LoadShapeClass.Find(Value);
        if TestLoadShapeObj <> NIL then
            ActiveCircuit.DefaultYearlyShapeObj := TestLoadShapeObj;
    end;
end;
//------------------------------------------------------------------------------
procedure Solution_Get_EventLog(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    i: Integer;
begin
    if ActiveCircuit <> NIL then
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (EventStrings.Count - 1) + 1);
        for i := 0 to EventStrings.Count - 1 do
        begin
            Result[i] := DSS_CopyStringAsPChar(EventStrings.Strings[i]);
        end;
        Exit;
    end;
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
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
    if ActiveCircuit <> NIL then
    begin
        Result := ActiveCircuit.Solution.DynaVars.dblHour;
    end;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_dblHour(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
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
    if ActiveCircuit <> NIL then
    begin
        ActiveCircuit.Solution.Dynavars.h := Value * 3600.0;
    end;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_StepsizeMin(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
    begin
        ActiveCircuit.Solution.Dynavars.h := Value * 60.0;
    end;
end;
//------------------------------------------------------------------------------
function Solution_Get_ControlIterations(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        Result := ActiveCircuit.Solution.ControlIteration;
    end;
end;
//------------------------------------------------------------------------------
function Solution_Get_MaxControlIterations(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        Result := ActiveCircuit.Solution.MaxControlIterations;
    end;
end;
//------------------------------------------------------------------------------
procedure Solution_Sample_DoControlActions(); CDECL;
begin
    if ActiveCircuit <> NIL then
    begin
        ActiveCircuit.Solution.Sample_DoControlActions;
    end;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_ControlIterations(Value: Integer); CDECL;
begin
    if ActiveCircuit <> NIL then
    begin
        ActiveCircuit.Solution.ControlIteration := Value;
    end;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_MaxControlIterations(Value: Integer); CDECL;
begin
    if ActiveCircuit <> NIL then
    begin
        ActiveCircuit.Solution.MaxControlIterations := Value;
    end;
end;
//------------------------------------------------------------------------------
procedure Solution_CheckFaultStatus(); CDECL;
begin
    if ActiveCircuit <> NIL then
    begin
        ActiveCircuit.Solution.Check_Fault_Status;
    end;
end;
//------------------------------------------------------------------------------
procedure Solution_SolveDirect(); CDECL;
begin
    if ActiveCircuit <> NIL then
    begin
        ActiveCircuit.Solution.SolveDirect;
    end;
end;
//------------------------------------------------------------------------------
procedure Solution_SolveNoControl(); CDECL;
{Solves without checking controls}
begin
    if ActiveCircuit <> NIL then
    begin
        ActiveCircuit.Solution.SolveCircuit;
    end;
end;
//------------------------------------------------------------------------------
procedure Solution_SolvePflow(); CDECL;
begin
    if ActiveCircuit <> NIL then
    begin
        ActiveCircuit.Solution.DoPflowSolution;
    end;
end;
//------------------------------------------------------------------------------
procedure Solution_SolvePlusControl(); CDECL;
{One Pass Through the solution and then dispatches controls}
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Solution do
        begin
            SolveCircuit;
            CheckControls;
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Solution_SolveSnap(); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.Solution.SolveSnap;
end;
//------------------------------------------------------------------------------
procedure Solution_CheckControls(); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.Solution.CheckControls;
end;
//------------------------------------------------------------------------------
procedure Solution_InitSnap(); CDECL;
{Initi some things that are done at the beginning of a snapshot solve}
begin
    if ActiveCircuit <> NIL then
    begin
        ActiveCircuit.Solution.SnapShotInit;
    end;
end;
//------------------------------------------------------------------------------
function Solution_Get_SystemYChanged(): Boolean; CDECL;
begin
    Result := False;
    if ActiveCircuit <> NIL then
    begin
        Result := ActiveCircuit.Solution.SystemYChanged;
    end;
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
    if ActiveCircuit <> NIL then
    begin
        if AllocateVI = 0 then
            Ymatrix.BuildYMatrix(BuildOption, FALSE)
        else
            Ymatrix.BuildYMatrix(BuildOption, TRUE)
    end;
end;
//------------------------------------------------------------------------------
procedure Solution_DoControlActions(); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.Solution.DoControlActions;
end;
//------------------------------------------------------------------------------
procedure Solution_SampleControlDevices(); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.Solution.SampleControlDevices;
end;
//------------------------------------------------------------------------------
function Solution_Get_Converged(): Boolean; CDECL;
begin
    Result := False;
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Issolved;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Converged(Value: Boolean); CDECL;
{Set the flag directly to force its setting}
begin

    if ActiveCircuit <> NIL then
    begin
        ActiveCircuit.Solution.ConvergedFlag := Value;
        ActiveCircuit.Issolved := Value;
    end;
end;
//------------------------------------------------------------------------------
function Solution_Get_Totaliterations(): Integer; CDECL;
// Same as Iterations interface

begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.Iteration
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_MostIterationsDone(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.MostIterationsDone
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_ControlActionsDone(): Boolean; CDECL;
begin
    Result := False;
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.ControlActionsDone;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_ControlActionsDone(Value: Boolean); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.Solution.ControlActionsDone := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Cleanup(); CDECL;
begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit, ActiveCircuit.Solution do
        begin
            EndOfTimeStepCleanup;
        end;
end;
//------------------------------------------------------------------------------
procedure Solution_FinishTimeStep(); CDECL;
begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit, ActiveCircuit.Solution do
        begin
            MonitorClass.SampleAll;  // Make all monitors take a sample
            EndOfTimeStepCleanup;
            Increment_time;
 //               DefaultHourMult := DefaultDailyShapeObj.getmult(TDynamicsrec.dblHour);
        end;
end;
//------------------------------------------------------------------------------
function Solution_Get_Process_Time(): Double; CDECL;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.Time_Solve;
end;
//------------------------------------------------------------------------------
function Solution_Get_Total_Time(): Double; CDECL;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.Total_Time;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Total_Time(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.Solution.Total_Time := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_Time_of_Step(): Double; CDECL;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.Time_Step;
end;
//------------------------------------------------------------------------------
function Solution_Get_IntervalHrs(): Double; CDECL;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.IntervalHrs;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_IntervalHrs(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.Solution.IntervalHrs := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_MinIterations(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Solution.MinIterations
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_MinIterations(Value: Integer); CDECL;
begin
    if ActiveCircuit <> NIL then
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
    Idx,
    ArrSize: Integer;
begin
    if (ActiveCircuit <> NIL) and (ActiveCircuit.Solution.Laplacian <> NIL) then
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
    Result[0] := 0;
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
    Idx,
    ArrSize: Integer;
begin
    if (ActiveCircuit <> NIL) and (ActiveCircuit.Solution.IncMat <> NIL) then
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
    Result[0] := 0;
end;

procedure Solution_Get_IncMatrix_GR(); CDECL;
// Same as Solution_Get_IncMatrix but uses global result (GR) pointers
begin
    Solution_Get_IncMatrix(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
procedure Solution_Get_BusLevels(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
var
    Result: PIntegerArray;
    i,
    IMIdx,
    Idx,
    ArrSize: Integer;
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Solution do
        begin
            ArrSize := length(Inc_Mat_Levels) - 1;    // Removes the 3 initial zeros and the extra index
                                                  // Since it starts on 0
            Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (ArrSize) + 1);
            for IMIdx := 0 to ArrSize do
            begin
                Result[IMIdx] := Inc_Mat_levels[IMIdx];
            end;
            Exit;
        end;
    end;
    Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, 1);
    Result[0] := 0;
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
    i,
    IMIdx,
    Idx,
    ArrSize: Integer;
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Solution do
        begin
            ArrSize := length(Inc_Mat_Rows) - 1;
            Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (ArrSize) + 1);
            for IMIdx := 0 to ArrSize do
            begin
                Result[IMIdx] := DSS_CopyStringAsPChar(Inc_Mat_Rows[IMIdx]);
            end;
            Exit;
        end;
    end;
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
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
    Idx,
    ArrSize: Integer;
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Solution, ActiveCircuit do
        begin
            if IncMat_Ordered then
            begin
                ArrSize := length(Inc_Mat_Cols) - 1;
                Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (ArrSize) + 1);
                for IMIdx := 0 to ArrSize do
                begin
                    Result[IMIdx] := DSS_CopyStringAsPChar(Inc_Mat_Cols[IMIdx]);
                end;
            end
            else
            begin
                Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumBuses - 1) + 1);
                for i := 0 to NumBuses - 1 do
                begin
                    Result[i] := DSS_CopyStringAsPChar(BusList.Get(i + 1));
                end;
            end;
            Exit;
        end;
    end;
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
end;

procedure Solution_Get_IncMatrixCols_GR(); CDECL;
// Same as Solution_Get_IncMatrixCols but uses global result (GR) pointers
begin
    Solution_Get_IncMatrixCols(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
end.
