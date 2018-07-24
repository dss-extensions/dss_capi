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
PROCEDURE Solution_Get_EventLog(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
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

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, Math, LoadShape, Utilities, YMatrix, SolutionAlgs, Solution;

function Solution_Get_Frequency():Double;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.Frequency
     Else Result := 0.0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Hour():Integer;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.DynaVars.intHour
     Else Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Iterations():Integer;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.Iteration
     Else Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_LoadMult():Double;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.LoadMultiplier
     Else Result := 0.0;
end;
//------------------------------------------------------------------------------
function Solution_Get_MaxIterations():Integer;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.MaxIterations
     Else Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Mode():Integer;cdecl;
begin
     //If ActiveCircuit <> Nil Then Result := GetSolutionModeID      changed to integer 8/16/00
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.Mode
     Else Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Number():Integer;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.NumberOfTimes
     Else Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Random():Integer;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.RandomType
     Else Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Seconds():Double;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.dynavars.t
     Else Result := 0.0;
end;
//------------------------------------------------------------------------------
function Solution_Get_StepSize():Double;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.dynavars.h
     Else Result := 0.0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Tolerance():Double;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.ConvergenceTolerance
     Else Result := 0.0;
end;
//------------------------------------------------------------------------------
function Solution_Get_Year():Integer;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.Year
     Else Result := 0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Frequency(Value: Double);cdecl;
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.Frequency  := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Hour(Value: Integer);cdecl;
begin
     If ActiveCircuit <> Nil Then With  ActiveCircuit.Solution Do Begin
        DynaVars.intHour  := Value;
        Update_dblHour;
     End;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_LoadMult(Value: Double);cdecl;
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.LoadMultiplier  := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_MaxIterations(Value: Integer);cdecl;
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.MaxIterations  := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Mode(Mode: Integer);cdecl;
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.Mode := Mode; //InterpretSolveMode(Value);
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Number(Value: Integer);cdecl;
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.NumberOfTimes  := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Random(Random: Integer);cdecl;
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.RandomType := Random;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Seconds(Value: Double);cdecl;
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.dynavars.t  := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_StepSize(Value: Double);cdecl;
begin
     If ActiveCircuit <> Nil Then Begin
         ActiveCircuit.Solution.dynavars.h  := Value;
         Solution_Set_IntervalHrs(Value/3600.0);     // Keep IntervalHrs in synch with time step size
     End;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Tolerance(Value: Double);cdecl;
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.ConvergenceTolerance  := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Year(Value: Integer);cdecl;
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.Year  := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Solve();cdecl;
begin
  IF ActiveCircuit <> Nil THEN ActiveCircuit.Solution.Solve;
end;
//------------------------------------------------------------------------------
function Solution_Get_ModeID_AnsiString():AnsiString;inline;
begin
    If ActiveCircuit <> Nil Then Result := GetSolutionModeID
    ELSE Result := '';
end;

function Solution_Get_ModeID():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Solution_Get_ModeID_AnsiString());
end;
//------------------------------------------------------------------------------
function Solution_Get_LoadModel():Integer;cdecl;
begin
    If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.LoadModel
    ELSE Result := 0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_LoadModel(Value: Integer);cdecl;
begin

   If ActiveCircuit <> Nil Then  WITH ActiveCircuit.Solution Do Begin
      LoadModel := Value;
      DefaultLoadModel := LoadModel;
   End;

end;
//------------------------------------------------------------------------------
function Solution_Get_LDCurve_AnsiString():AnsiString;inline;
begin
     IF ActiveCircuit <> Nil Then Result := ActiveCircuit.LoadDurCurve
     ELSE Result := '';
end;

function Solution_Get_LDCurve():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Solution_Get_LDCurve_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Solution_Set_LDCurve(const Value: PAnsiChar);cdecl;
begin
      IF ActiveCircuit <> Nil
      THEN With ActiveCircuit DO
      Begin
            LoadDurCurve    := Value;
            LoadDurCurveObj := LoadShapeClass.Find(LoadDurCurve);
            IF LoadDurCurveObj=NIL THEN
             DoSimpleMsg('Load-Duration Curve not found.', 5001);
      End;

end;
//------------------------------------------------------------------------------
function Solution_Get_pctGrowth():Double;cdecl;
begin
     IF ActiveCircuit <> NIL
     THEN With ActiveCircuit DO
     Begin
        Result := (DefaultGrowthRate-1.0)*100.0
     End;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_pctGrowth(Value: Double);cdecl;
begin
     IF ActiveCircuit <> NIL
     THEN With ActiveCircuit DO
     Begin
        DefaultGrowthRate := 1.0 + Value/100.0;
        DefaultGrowthFactor :=  IntPower(DefaultGrowthRate, (Solution.Year-1));
     End;
end;
//------------------------------------------------------------------------------
function Solution_Get_AddType():Integer;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.AutoAddObj.AddType
     Else Result := 0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_AddType(Value: Integer);cdecl;
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.AutoAddObj.AddType := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_GenkW():Double;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.AutoAddObj.GenkW
     Else Result := 0.0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_GenkW(Value: Double);cdecl;
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.AutoAddObj.GenkW := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_GenPF():Double;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.AutoAddObj.GenPF
     Else Result := 0.0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_GenPF(Value: Double);cdecl;
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.AutoAddObj.GenPF := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_Capkvar():Double;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.AutoAddObj.Capkvar
     Else Result := 0.0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Capkvar(Value: Double);cdecl;
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.AutoAddObj.Capkvar := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_Algorithm():Integer;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.Algorithm
     Else Result := 0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Algorithm(Value: Integer);cdecl;
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.Algorithm := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_ControlMode():Integer;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.ControlMode
     Else Result := 0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_ControlMode(Value: Integer);cdecl;
begin
    If ActiveCircuit <> Nil Then With ActiveCircuit.Solution Do Begin
         ControlMode := Value;
         DefaultControlMode := ControlMode;
    End;
end;
//------------------------------------------------------------------------------
function Solution_Get_GenMult():Double;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.GenMultiplier
     Else Result := 0.0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_GenMult(Value: Double);cdecl;
begin
    If ActiveCircuit <> Nil Then ActiveCircuit.GenMultiplier := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_DefaultDaily_AnsiString():AnsiString;inline;
begin
     IF   ActiveCircuit <> Nil
     THEN Result := ActiveCircuit.DefaultDailyShapeObj.Name
     ELSE Result := '';
end;

function Solution_Get_DefaultDaily():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Solution_Get_DefaultDaily_AnsiString());
end;
//------------------------------------------------------------------------------
function Solution_Get_DefaultYearly_AnsiString():AnsiString;inline;
begin
     IF   ActiveCircuit <> Nil
     THEN Result := ActiveCircuit.DefaultYearlyShapeObj.Name
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
     If ActiveCircuit <> Nil
     Then
     Begin
           TestLoadShapeObj := LoadShapeClass.Find(Value);
           IF TestLoadShapeObj <> NIL THEN ActiveCircuit.DefaultDailyShapeObj  := TestLoadShapeObj;
     END;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_DefaultYearly(const Value: PAnsiChar);cdecl;
Var  TestLoadShapeObj :TLoadShapeObj;
begin
     If ActiveCircuit <> Nil
     Then
     Begin
           TestLoadShapeObj := LoadShapeClass.Find(Value);
           IF TestLoadShapeObj <> NIL THEN ActiveCircuit.DefaultYearlyShapeObj  := TestLoadShapeObj;
     END;

end;
//------------------------------------------------------------------------------
PROCEDURE Solution_Get_EventLog(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
VAR
  Result: PPAnsiCharArray; i:Integer;
begin
    If ActiveCircuit <> Nil Then Begin
       Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (EventStrings.Count-1) + 1);
       For i := 0 to EventStrings.Count-1 Do Begin
          Result[i] := DSS_CopyStringAsPChar(EventStrings.Strings[i]); 
       End;
    END
    Else Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);;

end;
//------------------------------------------------------------------------------
function Solution_Get_dblHour():Double;cdecl;
begin
     If ActiveCircuit <> Nil Then  Begin
        Result := ActiveCircuit.Solution.DynaVars.dblHour;
     End;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_dblHour(Value: Double);cdecl;
begin
  If ActiveCircuit <> Nil Then With ActiveCircuit.Solution Do Begin
      DynaVars.intHour := Trunc(Value);
      DynaVars.dblHour := Value;
      Dynavars.t := (Value - DynaVars.intHour) * 3600.0;
  End;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_StepsizeHr(Value: Double);cdecl;
begin
  If ActiveCircuit <> Nil Then Begin
      ActiveCircuit.Solution.Dynavars.h := Value * 3600.0;
  End;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_StepsizeMin(Value: Double);cdecl;
begin

  If ActiveCircuit <> Nil Then Begin
      ActiveCircuit.Solution.Dynavars.h := Value * 60.0;
  End;

end;
//------------------------------------------------------------------------------
function Solution_Get_ControlIterations():Integer;cdecl;
begin
     If ActiveCircuit <> Nil Then  Begin
        Result := ActiveCircuit.Solution.ControlIteration;
     End;
end;
//------------------------------------------------------------------------------
function Solution_Get_MaxControlIterations():Integer;cdecl;
begin
     If ActiveCircuit <> Nil Then  Begin
        Result := ActiveCircuit.Solution.MaxControlIterations;
     End;
end;
//------------------------------------------------------------------------------
procedure Solution_Sample_DoControlActions();cdecl;
begin
    If ActiveCircuit <> Nil Then Begin
      ActiveCircuit.Solution.Sample_DoControlActions  ;
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_ControlIterations(Value: Integer);cdecl;
begin
    If ActiveCircuit <> Nil Then Begin
      ActiveCircuit.Solution.ControlIteration := Value;
    End;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_MaxControlIterations(Value: Integer);cdecl;
begin
    If ActiveCircuit <> Nil Then Begin
      ActiveCircuit.Solution.MaxControlIterations := Value;
    End;
end;
//------------------------------------------------------------------------------
procedure Solution_CheckFaultStatus();cdecl;
begin
   If ActiveCircuit <> Nil Then Begin
      ActiveCircuit.Solution.Check_Fault_Status ;
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_SolveDirect();cdecl;
begin
   If ActiveCircuit <> Nil Then Begin
      ActiveCircuit.Solution.SolveDirect;
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_SolveNoControl();cdecl;
{Solves without checking controls}
begin
   If ActiveCircuit <> Nil Then Begin
      ActiveCircuit.Solution.SolveCircuit;
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_SolvePflow();cdecl;
begin
   If ActiveCircuit <> Nil Then Begin
      ActiveCircuit.Solution.DoPflowSolution;
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_SolvePlusControl();cdecl;
{One Pass Through the solution and then dispatches controls}
begin
   If ActiveCircuit <> Nil Then Begin
      With ActiveCircuit.Solution Do Begin
         SolveCircuit;
         CheckControls;
      End;
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_SolveSnap();cdecl;
begin
   If ActiveCircuit <> Nil Then Begin
      ActiveCircuit.Solution.SolveSnap;
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_CheckControls();cdecl;
begin
   If ActiveCircuit <> Nil Then Begin
      ActiveCircuit.Solution.CheckControls;
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_InitSnap();cdecl;
{Initi some things that are done at the beginning of a snapshot solve}
begin
   If ActiveCircuit <> Nil Then Begin
      ActiveCircuit.Solution.SnapShotInit;
   End;
end;
//------------------------------------------------------------------------------
function Solution_Get_SystemYChanged():WordBool;cdecl;
begin
   If ActiveCircuit <> Nil Then Begin
      Result := ActiveCircuit.Solution.SystemYChanged;
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
  If ActiveCircuit <> Nil then  Begin
    If AllocateVI = 0 then
       Ymatrix.BuildYMatrix(BuildOption, FALSE)
    else
       Ymatrix.BuildYMatrix(BuildOption, TRUE)
  End;
end;
//------------------------------------------------------------------------------
procedure Solution_DoControlActions();cdecl;
begin
   If ActiveCircuit <> Nil Then Begin
      ActiveCircuit.Solution.DoControlActions;
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_SampleControlDevices();cdecl;
begin
    If ActiveCircuit <> Nil Then Begin
      ActiveCircuit.Solution.SampleControlDevices;
   End;
end;
//------------------------------------------------------------------------------
function Solution_Get_Converged():WordBool;cdecl;
begin
   If ActiveCircuit <> Nil Then Begin
      Result := ActiveCircuit.Issolved;
   End;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Converged(Value: WordBool);cdecl;
{Set the flag directly to force its setting}
begin

   If ActiveCircuit <> Nil Then Begin
     ActiveCircuit.Solution.ConvergedFlag := Value;
     ActiveCircuit.Issolved := Value;
   End;
end;
//------------------------------------------------------------------------------
function Solution_Get_Totaliterations():Integer;cdecl;
// Same as Iterations interface

begin
    If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.Iteration
     Else Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_MostIterationsDone():Integer;cdecl;
begin
   If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.MostIterationsDone
     Else Result := 0;
end;
//------------------------------------------------------------------------------
function Solution_Get_ControlActionsDone():WordBool;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.ControlActionsDone;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_ControlActionsDone(Value: WordBool);cdecl;
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.ControlActionsDone := Value;
end;
//------------------------------------------------------------------------------
procedure Solution_Cleanup();cdecl;
begin
    If ActiveCircuit <> Nil Then
    WITH ActiveCircuit, ActiveCircuit.Solution Do
      Begin
                EndOfTimeStepCleanup;
    End;
end;
//------------------------------------------------------------------------------
procedure Solution_FinishTimeStep();cdecl;
begin
    If ActiveCircuit <> Nil Then
    WITH ActiveCircuit, ActiveCircuit.Solution Do
      Begin
                MonitorClass.SampleAll;  // Make all monitors take a sample
                EndOfTimeStepCleanup;
                Increment_time;
 //               DefaultHourMult := DefaultDailyShapeObj.getmult(TDynamicsrec.dblHour);
    End;
end;
//------------------------------------------------------------------------------
function Solution_Get_Process_Time():Double;cdecl;
begin
    If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.Time_Solve;
end;
//------------------------------------------------------------------------------
function Solution_Get_Total_Time():Double;cdecl;
begin
    If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.Total_Time;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_Total_Time(Value: Double);cdecl;
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.Total_Time   :=  Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_Time_of_Step():Double;cdecl;
begin
    If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.Time_Step;
end;
//------------------------------------------------------------------------------
function Solution_Get_IntervalHrs():Double;cdecl;
begin
    If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.IntervalHrs;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_IntervalHrs(Value: Double);cdecl;
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.IntervalHrs := Value;
end;
//------------------------------------------------------------------------------
function Solution_Get_MinIterations():Integer;cdecl;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.MinIterations
     Else Result := 0;
end;
//------------------------------------------------------------------------------
procedure Solution_Set_MinIterations(Value: Integer);cdecl;
begin
    If ActiveCircuit <> Nil Then ActiveCircuit.Solution.MinIterations  := Value;
end;
//------------------------------------------------------------------------------
END.
