UNIT CAPI_Solution;
{$inline on}

INTERFACE

USES CAPI_Utils;

function Solution_Get_Frequency():Double;cdecl;
function Solution_Get_Hour():Integer;cdecl;
function Solution_Get_Iterations():Integer;cdecl;
function Solution_Get_LoadMult():Double;cdecl;
function Solution_Get_MaxIterations():Integer;cdecl;
function Solution_Get_Mode():Integer;cdecl;
function Solution_Get_Number():Integer;cdecl;
function Solution_Get_Random():Integer;cdecl;
function Solution_Get_Seconds():Double;cdecl;
function Solution_Get_StepSize():Double;cdecl;
function Solution_Get_Tolerance():Double;cdecl;
function Solution_Get_Year():Integer;cdecl;
procedure Solution_Set_Frequency(Value: Double);cdecl;
procedure Solution_Set_Hour(Value: Integer);cdecl;
procedure Solution_Set_LoadMult(Value: Double);cdecl;
procedure Solution_Set_MaxIterations(Value: Integer);cdecl;
procedure Solution_Set_Mode(Mode: Integer);cdecl;
procedure Solution_Set_Number(Value: Integer);cdecl;
procedure Solution_Set_Random(Random: Integer);cdecl;
procedure Solution_Set_Seconds(Value: Double);cdecl;
procedure Solution_Set_StepSize(Value: Double);cdecl;
procedure Solution_Set_Tolerance(Value: Double);cdecl;
procedure Solution_Set_Year(Value: Integer);cdecl;
procedure Solution_Solve();cdecl;
function Solution_Get_ModeID():PAnsiChar;cdecl;
function Solution_Get_LoadModel():Integer;cdecl;
procedure Solution_Set_LoadModel(Value: Integer);cdecl;
function Solution_Get_LDCurve():PAnsiChar;cdecl;
procedure Solution_Set_LDCurve(const Value: PAnsiChar);cdecl;
function Solution_Get_pctGrowth():Double;cdecl;
procedure Solution_Set_pctGrowth(Value: Double);cdecl;
function Solution_Get_AddType():Integer;cdecl;
procedure Solution_Set_AddType(Value: Integer);cdecl;
function Solution_Get_GenkW():Double;cdecl;
procedure Solution_Set_GenkW(Value: Double);cdecl;
function Solution_Get_GenPF():Double;cdecl;
procedure Solution_Set_GenPF(Value: Double);cdecl;
function Solution_Get_Capkvar():Double;cdecl;
procedure Solution_Set_Capkvar(Value: Double);cdecl;
function Solution_Get_Algorithm():Integer;cdecl;
procedure Solution_Set_Algorithm(Value: Integer);cdecl;
function Solution_Get_ControlMode():Integer;cdecl;
procedure Solution_Set_ControlMode(Value: Integer);cdecl;
function Solution_Get_GenMult():Double;cdecl;
procedure Solution_Set_GenMult(Value: Double);cdecl;
function Solution_Get_DefaultDaily():PAnsiChar;cdecl;
function Solution_Get_DefaultYearly():PAnsiChar;cdecl;
procedure Solution_Set_DefaultDaily(const Value: PAnsiChar);cdecl;
procedure Solution_Set_DefaultYearly(const Value: PAnsiChar);cdecl;
PROCEDURE Solution_Get_EventLog(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE Solution_Get_EventLog_GR();cdecl;
function Solution_Get_dblHour():Double;cdecl;
procedure Solution_Set_dblHour(Value: Double);cdecl;
procedure Solution_Set_StepsizeHr(Value: Double);cdecl;
procedure Solution_Set_StepsizeMin(Value: Double);cdecl;
function Solution_Get_ControlIterations():Integer;cdecl;
function Solution_Get_MaxControlIterations():Integer;cdecl;
procedure Solution_Sample_DoControlActions();cdecl;
procedure Solution_Set_ControlIterations(Value: Integer);cdecl;
procedure Solution_Set_MaxControlIterations(Value: Integer);cdecl;
procedure Solution_CheckFaultStatus();cdecl;
procedure Solution_SolveDirect();cdecl;
procedure Solution_SolveNoControl();cdecl;
procedure Solution_SolvePflow();cdecl;
procedure Solution_SolvePlusControl();cdecl;
procedure Solution_SolveSnap();cdecl;
procedure Solution_CheckControls();cdecl;
procedure Solution_InitSnap();cdecl;
function Solution_Get_SystemYChanged():WordBool;cdecl;
procedure Solution_BuildYMatrix(BuildOption, AllocateVI: Integer);cdecl;
procedure Solution_DoControlActions();cdecl;
procedure Solution_SampleControlDevices();cdecl;
function Solution_Get_Converged():WordBool;cdecl;
procedure Solution_Set_Converged(Value: WordBool);cdecl;
function Solution_Get_Totaliterations():Integer;cdecl;
function Solution_Get_MostIterationsDone():Integer;cdecl;
function Solution_Get_ControlActionsDone():WordBool;cdecl;
procedure Solution_Set_ControlActionsDone(Value: WordBool);cdecl;
procedure Solution_Cleanup();cdecl;
procedure Solution_FinishTimeStep();cdecl;
function Solution_Get_Process_Time():Double;cdecl;
function Solution_Get_Total_Time():Double;cdecl;
procedure Solution_Set_Total_Time(Value: Double);cdecl;
function Solution_Get_Time_of_Step():Double;cdecl;
function Solution_Get_IntervalHrs():Double;cdecl;
procedure Solution_Set_IntervalHrs(Value: Double);cdecl;
function Solution_Get_MinIterations():Integer;cdecl;
procedure Solution_Set_MinIterations(Value: Integer);cdecl;
procedure Solution_SolveAll();cdecl;
PROCEDURE Solution_Get_IncMatrix(var ResultPtr: PInteger; ResultCount: PInteger);cdecl;
PROCEDURE Solution_Get_IncMatrix_GR();cdecl;
PROCEDURE Solution_Get_BusLevels(var ResultPtr: PInteger; ResultCount: PInteger);cdecl;
PROCEDURE Solution_Get_BusLevels_GR();cdecl;
PROCEDURE Solution_Get_IncMatrixRows(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE Solution_Get_IncMatrixRows_GR();cdecl;
PROCEDURE Solution_Get_IncMatrixCols(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE Solution_Get_IncMatrixCols_GR();cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, Math, LoadShape, Utilities, YMatrix, SolutionAlgs, Solution, ExecOptions;

function Solution_Get_Frequency():Double;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Frequency
     Else Result := 0.0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Hour():Integer;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.DynaVars.intHour
     Else Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Iterations():Integer;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Iteration
     Else Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_LoadMult():Double;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].LoadMultiplier
     Else Result := 0.0;
end;
//------------------------------------------------------------------------------
function Solution_Get_MaxIterations():Integer;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.MaxIterations
     Else Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Mode():Integer;cdecl;
begin
     //If ActiveCircuit <> Nil Then Result := GetSolutionModeID      changed to integer 8/16/00
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Mode
     Else Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Number():Integer;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.NumberOfTimes
     Else Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Random():Integer;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.RandomType
     Else Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Seconds():Double;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.dynavars.t
     Else Result := 0.0;
end;
//------------------------------------------------------------------------------
function Solution_Get_StepSize():Double;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.dynavars.h
     Else Result := 0.0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Tolerance():Double;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.ConvergenceTolerance
     Else Result := 0.0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Year():Integer;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Year
     Else Result := 0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Frequency(Value: Double);cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.Frequency  := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Hour(Value: Integer);cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then With  ActiveCircuit[ActiveActor].Solution Do Begin
        DynaVars.intHour  := Value;
        Update_dblHour;
     End;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_LoadMult(Value: Double);cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].LoadMultiplier  := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_MaxIterations(Value: Integer);cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.MaxIterations  := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Mode(Mode: Integer);cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.Mode := Mode; //InterpretSolveMode(Value);
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Number(Value: Integer);cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.NumberOfTimes  := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Random(Random: Integer);cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.RandomType := Random;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Seconds(Value: Double);cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.dynavars.t  := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_StepSize(Value: Double);cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Begin
         ActiveCircuit[ActiveActor].Solution.dynavars.h  := Value;
         Solution_Set_IntervalHrs(Value/3600.0);     // Keep IntervalHrs in synch with time step size
     End;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Tolerance(Value: Double);cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.ConvergenceTolerance  := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Year(Value: Integer);cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.Year  := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Solve();cdecl;
begin
  IF ActiveCircuit[ActiveActor] <> Nil THEN ActiveCircuit[ActiveActor].Solution.Solve(ActiveActor);
end;
//------------------------------------------------------------------------------
function Solution_Get_ModeID_AnsiString():AnsiString;inline;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Result := GetSolutionModeID
    ELSE Result := '';
end;

function Solution_Get_ModeID():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Solution_Get_ModeID_AnsiString());
end;
//------------------------------------------------------------------------------
function Solution_Get_LoadModel():Integer;cdecl;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.LoadModel
    ELSE Result := 0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_LoadModel(Value: Integer);cdecl;
begin

   If ActiveCircuit[ActiveActor] <> Nil Then  WITH ActiveCircuit[ActiveActor].Solution Do Begin
      LoadModel := Value;
      DefaultLoadModel := LoadModel;
   End;

end;
//------------------------------------------------------------------------------
function Solution_Get_LDCurve_AnsiString():AnsiString;inline;
begin
     IF ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].LoadDurCurve
     ELSE Result := '';
end;

function Solution_Get_LDCurve():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Solution_Get_LDCurve_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Solution_Set_LDCurve(const Value: PAnsiChar);cdecl;
begin
      IF ActiveCircuit[ActiveActor] <> Nil
      THEN With ActiveCircuit[ActiveActor] DO
      Begin
            LoadDurCurve    := Value;
            LoadDurCurveObj := LoadShapeClass[ActiveActor].Find(LoadDurCurve);
            IF LoadDurCurveObj=NIL THEN
             DoSimpleMsg('Load-Duration Curve not found.', 5001);
      End;

end;
//------------------------------------------------------------------------------
function Solution_Get_pctGrowth():Double;cdecl;
begin
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN With ActiveCircuit[ActiveActor] DO
     Begin
        Result := (DefaultGrowthRate-1.0)*100.0
     End;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_pctGrowth(Value: Double);cdecl;
begin
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN With ActiveCircuit[ActiveActor] DO
     Begin
        DefaultGrowthRate := 1.0 + Value/100.0;
        DefaultGrowthFactor :=  IntPower(DefaultGrowthRate, (Solution.Year-1));
     End;
end;
//------------------------------------------------------------------------------
function Solution_Get_AddType():Integer;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].AutoAddObj.AddType
     Else Result := 0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_AddType(Value: Integer);cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].AutoAddObj.AddType := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_GenkW():Double;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].AutoAddObj.GenkW
     Else Result := 0.0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_GenkW(Value: Double);cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].AutoAddObj.GenkW := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_GenPF():Double;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].AutoAddObj.GenPF
     Else Result := 0.0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_GenPF(Value: Double);cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].AutoAddObj.GenPF := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_Capkvar():Double;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].AutoAddObj.Capkvar
     Else Result := 0.0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Capkvar(Value: Double);cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].AutoAddObj.Capkvar := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_Algorithm():Integer;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Algorithm
     Else Result := 0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Algorithm(Value: Integer);cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.Algorithm := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_ControlMode():Integer;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.ControlMode
     Else Result := 0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_ControlMode(Value: Integer);cdecl;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then With ActiveCircuit[ActiveActor].Solution Do Begin
         ControlMode := Value;
         DefaultControlMode := ControlMode;
    End;
end;
//------------------------------------------------------------------------------
function Solution_Get_GenMult():Double;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].GenMultiplier
     Else Result := 0.0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_GenMult(Value: Double);cdecl;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].GenMultiplier := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_DefaultDaily_AnsiString():AnsiString;inline;
begin
     IF   ActiveCircuit[ActiveActor] <> Nil
     THEN Result := ActiveCircuit[ActiveActor].DefaultDailyShapeObj.Name
     ELSE Result := '';
end;

function Solution_Get_DefaultDaily():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Solution_Get_DefaultDaily_AnsiString());
end;
//------------------------------------------------------------------------------
function Solution_Get_DefaultYearly_AnsiString():AnsiString;inline;
begin
     IF   ActiveCircuit[ActiveActor] <> Nil
     THEN Result := ActiveCircuit[ActiveActor].DefaultYearlyShapeObj.Name
     ELSE Result := '';
end;

function Solution_Get_DefaultYearly():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Solution_Get_DefaultYearly_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Solution_Set_DefaultDaily(const Value: PAnsiChar);cdecl;
Var  TestLoadShapeObj :TLoadShapeObj;
begin
     If ActiveCircuit[ActiveActor] <> Nil
     Then
     Begin
           TestLoadShapeObj := LoadShapeClass[ActiveActor].Find(Value);
           IF TestLoadShapeObj <> NIL THEN ActiveCircuit[ActiveActor].DefaultDailyShapeObj  := TestLoadShapeObj;
     END;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_DefaultYearly(const Value: PAnsiChar);cdecl;
Var  TestLoadShapeObj :TLoadShapeObj;
begin
     If ActiveCircuit[ActiveActor] <> Nil
     Then
     Begin
           TestLoadShapeObj := LoadShapeClass[ActiveActor].Find(Value);
           IF TestLoadShapeObj <> NIL THEN ActiveCircuit[ActiveActor].DefaultYearlyShapeObj  := TestLoadShapeObj;
     END;

end;
//------------------------------------------------------------------------------
PROCEDURE Solution_Get_EventLog(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray; i:Integer;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Begin
       Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (EventStrings[ActiveActor].Count-1) + 1);
       For i := 0 to EventStrings[ActiveActor].Count-1 Do Begin
          Result[i] := DSS_CopyStringAsPChar(EventStrings[ActiveActor].Strings[i]);
       End;
    END
    Else Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);;

end;
PROCEDURE Solution_Get_EventLog_GR();cdecl;
// Same as Solution_Get_EventLog but uses global result (GR) pointers
begin
   Solution_Get_EventLog(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Solution_Get_dblHour():Double;cdecl;
begin
     If ActiveCircuit <> Nil Then  Begin
        Result := ActiveCircuit[ActiveActor].Solution.DynaVars.dblHour;
     End;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_dblHour(Value: Double);cdecl;
begin
  If ActiveCircuit[ActiveActor] <> Nil Then With ActiveCircuit[ActiveActor].Solution Do Begin
      DynaVars.intHour := Trunc(Value);
      DynaVars.dblHour := Value;
      Dynavars.t := (Value - DynaVars.intHour) * 3600.0;
  End;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_StepsizeHr(Value: Double);cdecl;
begin
  If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.Dynavars.h := Value * 3600.0;
  End;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_StepsizeMin(Value: Double);cdecl;
begin

  If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.Dynavars.h := Value * 60.0;
  End;

end;
//------------------------------------------------------------------------------
function Solution_Get_ControlIterations():Integer;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then  Begin
        Result := ActiveCircuit[ActiveActor].Solution.ControlIteration;
     End;
end;
//------------------------------------------------------------------------------
function Solution_Get_MaxControlIterations():Integer;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then  Begin
        Result := ActiveCircuit[ActiveActor].Solution.MaxControlIterations;
     End;
end;
//------------------------------------------------------------------------------
procedure Solution_Sample_DoControlActions();cdecl;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.Sample_DoControlActions(ActiveActor)  ;
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_ControlIterations(Value: Integer);cdecl;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.ControlIteration := Value;
    End;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_MaxControlIterations(Value: Integer);cdecl;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.MaxControlIterations := Value;
    End;
end;
//------------------------------------------------------------------------------
procedure Solution_CheckFaultStatus();cdecl;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.Check_Fault_Status(ActiveActor);
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_SolveDirect();cdecl;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.SolveDirect(ActiveActor);
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_SolveNoControl();cdecl;
{Solves without checking controls}
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.SolveCircuit(ActiveActor);
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_SolvePflow();cdecl;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.DoPflowSolution(ActiveActor);
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_SolvePlusControl();cdecl;
{One Pass Through the solution and then dispatches controls}
begin
   If ActiveCircuit <> Nil Then Begin
      With ActiveCircuit[ActiveActor].Solution Do Begin
         SolveCircuit(ActiveActor);
         CheckControls(ActiveActor);
      End;
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_SolveSnap();cdecl;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.SolveSnap(ActiveActor);
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_CheckControls();cdecl;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.CheckControls(ActiveActor);
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_InitSnap();cdecl;
{Initi some things that are done at the beginning of a snapshot solve}
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.SnapShotInit(ActiveActor);
   End;
end;
//------------------------------------------------------------------------------
function Solution_Get_SystemYChanged():WordBool;cdecl;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      Result := ActiveCircuit[ActiveActor].Solution.SystemYChanged;
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_BuildYMatrix(BuildOption, AllocateVI: Integer);cdecl;
{
  Build Options
    1 = Series elements only
    2 = Whole Y matrix

  AllocateVI
    TRUE:  Reallocate VI
    FALSE: Do not Reallocate VI; leave as is
}
begin
  If ActiveCircuit[ActiveActor] <> Nil then  Begin
    If AllocateVI = 0 then
       Ymatrix.BuildYMatrix(BuildOption, FALSE, ActiveActor)
    else
       Ymatrix.BuildYMatrix(BuildOption, TRUE, ActiveActor)
  End;
end;
//------------------------------------------------------------------------------
procedure Solution_DoControlActions();cdecl;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.DoControlActions(ActiveActor);
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_SampleControlDevices();cdecl;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Begin
      ActiveCircuit[ActiveActor].Solution.SampleControlDevices(ActiveActor);
   End;
end;
//------------------------------------------------------------------------------
function Solution_Get_Converged():WordBool;cdecl;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Begin
      Result := ActiveCircuit[ActiveActor].Issolved;
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Converged(Value: WordBool);cdecl;
{Set the flag directly to force its setting}
begin

   If ActiveCircuit[ActiveActor] <> Nil Then Begin
     ActiveCircuit[ActiveActor].Solution.ConvergedFlag := Value;
     ActiveCircuit[ActiveActor].Issolved := Value;
   End;
end;
//------------------------------------------------------------------------------
function Solution_Get_Totaliterations():Integer;cdecl;
// Same as Iterations interface

begin
    If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Iteration
     Else Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_MostIterationsDone():Integer;cdecl;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.MostIterationsDone
     Else Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_ControlActionsDone():WordBool;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.ControlActionsDone;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_ControlActionsDone(Value: WordBool);cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.ControlActionsDone := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Cleanup();cdecl;
begin
    If ActiveCircuit <> Nil Then
    WITH ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution Do
      Begin
                EndOfTimeStepCleanup(ActiveActor);
    End;
end;
//------------------------------------------------------------------------------
procedure Solution_FinishTimeStep();cdecl;
begin
    If ActiveCircuit <> Nil Then
    WITH ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution Do
      Begin
                MonitorClass[ActiveActor].SampleAll(ActiveActor);  // Make all monitors take a sample
                EndOfTimeStepCleanup(ActiveActor);
                Increment_time;
 //               DefaultHourMult := DefaultDailyShapeObj.getmult(TDynamicsrec.dblHour);
    End;
end;
//------------------------------------------------------------------------------
function Solution_Get_Process_Time():Double;cdecl;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Time_Solve;
end;
//------------------------------------------------------------------------------
function Solution_Get_Total_Time():Double;cdecl;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Total_Time;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Total_Time(Value: Double);cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.Total_Time   :=  Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_Time_of_Step():Double;cdecl;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.Time_Step;
end;
//------------------------------------------------------------------------------
function Solution_Get_IntervalHrs():Double;cdecl;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.IntervalHrs;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_IntervalHrs(Value: Double);cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then ActiveCircuit[ActiveActor].Solution.IntervalHrs := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_MinIterations():Integer;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit[ActiveActor].Solution.MinIterations
     Else Result := 0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_MinIterations(Value: Integer);cdecl;
begin
    If ActiveCircuit <> Nil Then ActiveCircuit[ActiveActor].Solution.MinIterations  := Value;
end;
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
//------------------------------------------------------------------------------
PROCEDURE Solution_Get_IncMatrix(var ResultPtr: PInteger; ResultCount: PInteger);cdecl;
VAR
  Result: PIntegerArray;
  IMIdx,
  Idx,
  ArrSize : Integer;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Begin
      with ACtiveCircuit[ActiveActor].Solution do
      begin
         ArrSize    :=  length(IncMatrix)-3;
         Result     :=  DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (ArrSize) + 1);
         for IMIdx  :=  0 to ArrSize Do
         Begin
            Result[IMIdx] := IncMatrix[IMIdx+3];
         End;
      end;
    END
    Else Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (0) + 1);
end;
PROCEDURE Solution_Get_IncMatrix_GR();cdecl;
// Same as Solution_Get_IncMatrix but uses global result (GR) pointers
begin
   Solution_Get_IncMatrix(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
PROCEDURE Solution_Get_BusLevels(var ResultPtr: PInteger; ResultCount: PInteger);cdecl;
VAR
  Result: PIntegerArray;
  i,
  IMIdx,
  Idx,
  ArrSize : Integer;
begin
      If ActiveCircuit[ActiveActor] <> Nil Then Begin
        with ACtiveCircuit[ActiveActor].Solution do
        begin
           ArrSize    :=  length(Inc_Mat_Levels)-1;    // Removes the 3 initial zeros and the extra index
                                                  // Since it starts on 0
           Result     :=  DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (ArrSize) + 1);
           for IMIdx  :=  0 to ArrSize Do
           Begin
              Result[IMIdx] := Inc_Mat_levels[IMIdx];
           End;
        end;
      END
      Else Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (0) + 1);
end;
PROCEDURE Solution_Get_BusLevels_GR();cdecl;
// Same as Solution_Get_BusLevels but uses global result (GR) pointers
begin
   Solution_Get_BusLevels(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
PROCEDURE Solution_Get_IncMatrixRows(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  i,
  IMIdx,
  Idx,
  ArrSize : Integer;
begin
      If ActiveCircuit[ActiveActor] <> Nil Then Begin
        with ACtiveCircuit[ActiveActor].Solution do
        begin
          ArrSize    :=  length(Inc_Mat_Rows)-1;
          Result     :=  DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (ArrSize) + 1);
          for IMIdx  :=  0 to ArrSize Do
          Begin
             Result[IMIdx] := DSS_CopyStringAsPChar(Inc_Mat_Rows[IMIdx]);
          End;
        end;
      END
      Else Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
end;
PROCEDURE Solution_Get_IncMatrixRows_GR();cdecl;
// Same as Solution_Get_IncMatrixRows but uses global result (GR) pointers
begin
   Solution_Get_IncMatrixRows(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
PROCEDURE Solution_Get_IncMatrixCols(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  i,
  IMIdx,
  Idx,
  ArrSize : Integer;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Begin
       with ActiveCircuit[ActiveActor].Solution,ActiveCircuit[ActiveActor]  do
       begin
        if IncMat_Ordered then
        begin
          ArrSize    :=  length(Inc_Mat_Cols)-1;
          Result     :=  DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (ArrSize) + 1);
          for IMIdx  :=  0 to ArrSize Do
          Begin
             Result[IMIdx] := DSS_CopyStringAsPChar(Inc_Mat_Cols[IMIdx]);
          End;
        end
        else
        begin
             Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumBuses-1) + 1);
             FOR i := 0 to NumBuses-1 DO
             Begin
                 Result[i] := DSS_CopyStringAsPChar(BusList.Get(i+1));
             End;
        end;
       end;
     End
     Else Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
end;
PROCEDURE Solution_Get_IncMatrixCols_GR();cdecl;
// Same as Solution_Get_IncMatrixCols but uses global result (GR) pointers
begin
   Solution_Get_IncMatrixCols(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
END.
