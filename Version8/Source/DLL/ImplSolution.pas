unit ImplSolution;

{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSEngine_TLB,
    StdVcl;

type
    TSolution = class(TAutoObject, ISolution)
    PROTECTED
        function Get_Frequency: Double; SAFECALL;
        function Get_Hour: Integer; SAFECALL;
        function Get_Iterations: Integer; SAFECALL;
        function Get_LoadMult: Double; SAFECALL;
        function Get_MaxIterations: Integer; SAFECALL;
        function Get_Mode: Integer; SAFECALL;
        function Get_Number: Integer; SAFECALL;
        function Get_Random: Integer; SAFECALL;
        function Get_Seconds: Double; SAFECALL;
        function Get_StepSize: Double; SAFECALL;
        function Get_Tolerance: Double; SAFECALL;
        function Get_Year: Integer; SAFECALL;
        procedure Set_Frequency(Value: Double); SAFECALL;
        procedure Set_Hour(Value: Integer); SAFECALL;
        procedure Set_LoadMult(Value: Double); SAFECALL;
        procedure Set_MaxIterations(Value: Integer); SAFECALL;
        procedure Set_Mode(Mode: Integer); SAFECALL;
        procedure Set_Number(Value: Integer); SAFECALL;
        procedure Set_Random(Random: Integer); SAFECALL;
        procedure Set_Seconds(Value: Double); SAFECALL;
        procedure Set_StepSize(Value: Double); SAFECALL;
        procedure Set_Tolerance(Value: Double); SAFECALL;
        procedure Set_Year(Value: Integer); SAFECALL;
        procedure Solve; SAFECALL;
        function Get_ModeID: Widestring; SAFECALL;
        function Get_LoadModel: Integer; SAFECALL;
        procedure Set_LoadModel(Value: Integer); SAFECALL;
        function Get_LDCurve: Widestring; SAFECALL;
        procedure Set_LDCurve(const Value: Widestring); SAFECALL;
        function Get_pctGrowth: Double; SAFECALL;
        procedure Set_pctGrowth(Value: Double); SAFECALL;
        function Get_AddType: Integer; SAFECALL;
        procedure Set_AddType(Value: Integer); SAFECALL;
        function Get_GenkW: Double; SAFECALL;
        procedure Set_GenkW(Value: Double); SAFECALL;
        function Get_GenPF: Double; SAFECALL;
        procedure Set_GenPF(Value: Double); SAFECALL;
        function Get_Capkvar: Double; SAFECALL;
        procedure Set_Capkvar(Value: Double); SAFECALL;
        function Get_Algorithm: Integer; SAFECALL;
        procedure Set_Algorithm(Value: Integer); SAFECALL;
        function Get_ControlMode: Integer; SAFECALL;
        procedure Set_ControlMode(Value: Integer); SAFECALL;
        function Get_GenMult: Double; SAFECALL;
        procedure Set_GenMult(Value: Double); SAFECALL;
        function Get_DefaultDaily: Widestring; SAFECALL;
        function Get_DefaultYearly: Widestring; SAFECALL;
        procedure Set_DefaultDaily(const Value: Widestring); SAFECALL;
        procedure Set_DefaultYearly(const Value: Widestring); SAFECALL;
        function Get_EventLog: Olevariant; SAFECALL;
        function Get_dblHour: Double; SAFECALL;
        procedure Set_dblHour(Value: Double); SAFECALL;
        procedure Set_StepsizeHr(Value: Double); SAFECALL;
        procedure Set_StepsizeMin(Value: Double); SAFECALL;
        function Get_ControlIterations: Integer; SAFECALL;
        function Get_MaxControlIterations: Integer; SAFECALL;
        procedure Sample_DoControlActions; SAFECALL;
        procedure Set_ControlIterations(Value: Integer); SAFECALL;
        procedure Set_MaxControlIterations(Value: Integer); SAFECALL;
        procedure CheckFaultStatus; SAFECALL;
        procedure SolveDirect; SAFECALL;
        procedure SolveNoControl; SAFECALL;
        procedure SolvePflow; SAFECALL;
        procedure SolvePlusControl; SAFECALL;
        procedure SolveSnap; SAFECALL;
        procedure CheckControls; SAFECALL;
        procedure InitSnap; SAFECALL;
        function Get_SystemYChanged: Wordbool; SAFECALL;
        procedure BuildYMatrix(BuildOption, AllocateVI: Integer); SAFECALL;
        procedure DoControlActions; SAFECALL;
        procedure SampleControlDevices; SAFECALL;
        function Get_Converged: Wordbool; SAFECALL;
        procedure Set_Converged(Value: Wordbool); SAFECALL;
        function Get_Totaliterations: Integer; SAFECALL;
        function Get_MostIterationsDone: Integer; SAFECALL;
        function Get_ControlActionsDone: Wordbool; SAFECALL;
        procedure Set_ControlActionsDone(Value: Wordbool); SAFECALL;
        procedure Cleanup; SAFECALL;
        procedure FinishTimeStep; SAFECALL;
        function Get_Process_Time: Double; SAFECALL;
        function Get_Total_Time: Double; SAFECALL;
        procedure Set_Total_Time(Value: Double); SAFECALL;
        function Get_Time_of_Step: Double; SAFECALL;
        function Get_IntervalHrs: Double; SAFECALL;
        procedure Set_IntervalHrs(Value: Double); SAFECALL;
        function Get_MinIterations: Integer; SAFECALL;
        procedure Set_MinIterations(Value: Integer); SAFECALL;
        procedure SolveAll; SAFECALL;
        function Get_IncMatrix: Olevariant; SAFECALL;
        function Get_BusLevels: Olevariant; SAFECALL;
        function Get_IncMatrixRows: Olevariant; SAFECALL;
        function Get_IncMatrixCols: Olevariant; SAFECALL;
        function Get_Laplacian: Olevariant; SAFECALL;
    end;

implementation

uses
    ComServ,
    DSSGlobals,
    Math,
    LoadShape,
    Utilities,
    YMatrix,
    Variants,
    SolutionAlgs,
    Solution,
    ExecOptions;

function TSolution.Get_Frequency: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.Frequency
    else
        Result := 0.0;
end;

function TSolution.Get_Hour: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.DynaVars.intHour
    else
        Result := 0;
end;

function TSolution.Get_Iterations: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.Iteration
    else
        Result := 0;
end;

function TSolution.Get_LoadMult: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].LoadMultiplier
    else
        Result := 0.0;
end;

function TSolution.Get_MaxIterations: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.MaxIterations
    else
        Result := 0;
end;

function TSolution.Get_Mode: Integer;
begin
     //If ActiveCircuit <> Nil Then Result := GetSolutionModeID      changed to integer 8/16/00
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.Mode
    else
        Result := 0;
end;

function TSolution.Get_Number: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.NumberOfTimes
    else
        Result := 0;
end;

function TSolution.Get_Random: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.RandomType
    else
        Result := 0;
end;

function TSolution.Get_Seconds: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.dynavars.t
    else
        Result := 0.0;
end;

function TSolution.Get_StepSize: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.dynavars.h
    else
        Result := 0.0;
end;

function TSolution.Get_Tolerance: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.ConvergenceTolerance
    else
        Result := 0.0;
end;

function TSolution.Get_Year: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.Year
    else
        Result := 0;
end;

procedure TSolution.Set_Frequency(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].Solution.Frequency := Value;
end;

procedure TSolution.Set_Hour(Value: Integer);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with  ActiveCircuit[ActiveActor].Solution do
        begin
            DynaVars.intHour := Value;
            Update_dblHour;
        end;
end;

procedure TSolution.Set_LoadMult(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].LoadMultiplier := Value;
end;

procedure TSolution.Set_MaxIterations(Value: Integer);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].Solution.MaxIterations := Value;
end;

procedure TSolution.Set_Mode(Mode: Integer);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].Solution.Mode := Mode; //InterpretSolveMode(Value);
end;

procedure TSolution.Set_Number(Value: Integer);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].Solution.NumberOfTimes := Value;
end;

procedure TSolution.Set_Random(Random: Integer);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].Solution.RandomType := Random;
end;

procedure TSolution.Set_Seconds(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].Solution.dynavars.t := Value;
end;

procedure TSolution.Set_StepSize(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].Solution.dynavars.h := Value;
        Set_IntervalHrs(Value / 3600.0);     // Keep IntervalHrs in synch with time step size
    end;
end;

procedure TSolution.Set_Tolerance(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].Solution.ConvergenceTolerance := Value;
end;

procedure TSolution.Set_Year(Value: Integer);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].Solution.Year := Value;
end;

procedure TSolution.Solve;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].Solution.Solve(ActiveActor);
end;

function TSolution.Get_ModeID: Widestring;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := GetSolutionModeID
    else
        Result := '';
end;

function TSolution.Get_LoadModel: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.LoadModel
    else
        Result := 0;
end;

procedure TSolution.Set_LoadModel(Value: Integer);
begin

    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor].Solution do
        begin
            LoadModel := Value;
            DefaultLoadModel := LoadModel;
        end;

end;

function TSolution.Get_LDCurve: Widestring;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].LoadDurCurve
    else
        Result := '';
end;

procedure TSolution.Set_LDCurve(const Value: Widestring);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            LoadDurCurve := Value;
            LoadDurCurveObj := LoadShapeClass[ActiveActor].Find(LoadDurCurve);
            if LoadDurCurveObj = NIL then
                DoSimpleMsg('Load-Duration Curve not found.', 5001);
        end;

end;

function TSolution.Get_pctGrowth: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            Result := (DefaultGrowthRate - 1.0) * 100.0
        end;
end;

procedure TSolution.Set_pctGrowth(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            DefaultGrowthRate := 1.0 + Value / 100.0;
            DefaultGrowthFactor := IntPower(DefaultGrowthRate, (Solution.Year - 1));
        end;
end;

function TSolution.Get_AddType: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].AutoAddObj.AddType
    else
        Result := 0;
end;

procedure TSolution.Set_AddType(Value: Integer);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].AutoAddObj.AddType := Value;
end;

function TSolution.Get_GenkW: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].AutoAddObj.GenkW
    else
        Result := 0.0;
end;

procedure TSolution.Set_GenkW(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].AutoAddObj.GenkW := Value;
end;

function TSolution.Get_GenPF: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].AutoAddObj.GenPF
    else
        Result := 0.0;
end;

procedure TSolution.Set_GenPF(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].AutoAddObj.GenPF := Value;
end;

function TSolution.Get_Capkvar: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].AutoAddObj.Capkvar
    else
        Result := 0.0;
end;

procedure TSolution.Set_Capkvar(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].AutoAddObj.Capkvar := Value;
end;

function TSolution.Get_Algorithm: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.Algorithm
    else
        Result := 0;
end;

procedure TSolution.Set_Algorithm(Value: Integer);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].Solution.Algorithm := Value;
end;

function TSolution.Get_ControlMode: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.ControlMode
    else
        Result := 0;
end;

procedure TSolution.Set_ControlMode(Value: Integer);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor].Solution do
        begin
            ControlMode := Value;
            DefaultControlMode := ControlMode;
        end;
end;

function TSolution.Get_GenMult: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].GenMultiplier
    else
        Result := 0.0;
end;

procedure TSolution.Set_GenMult(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].GenMultiplier := Value;
end;

function TSolution.Get_DefaultDaily: Widestring;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].DefaultDailyShapeObj.Name
    else
        Result := '';
end;

function TSolution.Get_DefaultYearly: Widestring;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].DefaultYearlyShapeObj.Name
    else
        Result := '';
end;

procedure TSolution.Set_DefaultDaily(const Value: Widestring);
var
    TestLoadShapeObj: TLoadShapeObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        TestLoadShapeObj := LoadShapeClass[ActiveActor].Find(Value);
        if TestLoadShapeObj <> NIL then
            ActiveCircuit[ActiveActor].DefaultDailyShapeObj := TestLoadShapeObj;
    end;
end;

procedure TSolution.Set_DefaultYearly(const Value: Widestring);
var
    TestLoadShapeObj: TLoadShapeObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        TestLoadShapeObj := LoadShapeClass[ActiveActor].Find(Value);
        if TestLoadShapeObj <> NIL then
            ActiveCircuit[ActiveActor].DefaultYearlyShapeObj := TestLoadShapeObj;
    end;

end;

function TSolution.Get_EventLog: Olevariant;
var
    i: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := VarArrayCreate([0, EventStrings[ActiveActor].Count - 1], varOleStr);
        for i := 0 to EventStrings[ActiveActor].Count - 1 do
        begin
            Result[i] := EventStrings[ActiveActor].Strings[i];
        end;
    end
    else
        Result := VarArrayCreate([0, 0], varOleStr);
    ;

end;

function TSolution.Get_dblHour: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].Solution.DynaVars.dblHour;
    end;
end;

procedure TSolution.Set_dblHour(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor].Solution do
        begin
            DynaVars.intHour := Trunc(Value);
            DynaVars.dblHour := Value;
            Dynavars.t := (Value - DynaVars.intHour) * 3600.0;
        end;
end;

procedure TSolution.Set_StepsizeHr(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].Solution.Dynavars.h := Value * 3600.0;
    end;
end;

procedure TSolution.Set_StepsizeMin(Value: Double);
begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].Solution.Dynavars.h := Value * 60.0;
    end;

end;

function TSolution.Get_ControlIterations: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].Solution.ControlIteration;
    end;
end;

function TSolution.Get_MaxControlIterations: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].Solution.MaxControlIterations;
    end;
end;

procedure TSolution.Sample_DoControlActions;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].Solution.Sample_DoControlActions(ActiveActor);
    end;
end;

procedure TSolution.Set_ControlIterations(Value: Integer);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].Solution.ControlIteration := Value;
    end;
end;

procedure TSolution.Set_MaxControlIterations(Value: Integer);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].Solution.MaxControlIterations := Value;
    end;
end;

procedure TSolution.CheckFaultStatus;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].Solution.Check_Fault_Status(ActiveActor);
    end;
end;

procedure TSolution.SolveDirect;
begin
    IsSolveAll := FALSE;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].Solution.SolveDirect(ActiveActor);
    end;
end;

procedure TSolution.SolveNoControl;
{Solves without checking controls}
begin
    IsSolveAll := FALSE;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].Solution.SolveCircuit(ActiveActor);
    end;
end;

procedure TSolution.SolvePflow;
begin
    IsSolveAll := FALSE;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].Solution.DoPflowSolution(ActiveActor);
    end;
end;

procedure TSolution.SolvePlusControl;
{One Pass Through the solution and then dispatches controls}
begin
    IsSolveAll := FALSE;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        with ActiveCircuit[ActiveActor].Solution do
        begin
            SolveCircuit(ActiveActor);
            CheckControls(ActiveActor);
        end;
    end;
end;

procedure TSolution.SolveSnap;
begin
    IsSolveAll := FALSE;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].Solution.SolveSnap(ActiveActor);
    end;
end;

procedure TSolution.CheckControls;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].Solution.CheckControls(ActiveActor);
    end;
end;

procedure TSolution.InitSnap;
{Initi some things that are done at the beginning of a snapshot solve}
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].Solution.SnapShotInit(ActiveActor);
    end;
end;

function TSolution.Get_SystemYChanged: Wordbool;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].Solution.SystemYChanged;
    end;
end;

procedure TSolution.BuildYMatrix(BuildOption, AllocateVI: Integer);
{
  Build Options
    1 = Series elements only
    2 = Whole Y matrix

  AllocateVI
    TRUE:  Reallocate VI
    FALSE: Do not Reallocate VI; leave as is
}
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        if AllocateVI = 0 then
            Ymatrix.BuildYMatrix(BuildOption, FALSE, ActiveActor)
        else
            Ymatrix.BuildYMatrix(BuildOption, TRUE, ActiveActor)
    end;
end;

procedure TSolution.DoControlActions;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].Solution.DoControlActions(ActiveActor);
    end;
end;

procedure TSolution.SampleControlDevices;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].Solution.SampleControlDevices(ActiveActor);
    end;
end;

function TSolution.Get_Converged: Wordbool;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].Issolved;
    end;
end;

procedure TSolution.Set_Converged(Value: Wordbool);

{Set the flag directly to force its setting}
begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].Solution.ConvergedFlag := Value;
        ActiveCircuit[ActiveActor].Issolved := Value;
    end;
end;

function TSolution.Get_Totaliterations: Integer;

// Same as Iterations interface

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.Iteration
    else
        Result := 0;
end;

function TSolution.Get_MostIterationsDone: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.MostIterationsDone
    else
        Result := 0;
end;

function TSolution.Get_ControlActionsDone: Wordbool;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.ControlActionsDone;
end;

procedure TSolution.Set_ControlActionsDone(Value: Wordbool);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].Solution.ControlActionsDone := Value;
end;

procedure TSolution.Cleanup;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution do
        begin
            EndOfTimeStepCleanup(ActiveActor);
        end;
end;

procedure TSolution.FinishTimeStep;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution do
        begin
            MonitorClass[ActiveActor].SampleAll(ActiveActor);  // Make all monitors take a sample
            EndOfTimeStepCleanup(ActiveActor);
            Increment_time;
 //               DefaultHourMult := DefaultDailyShapeObj.getmult(TDynamicsrec.dblHour);
        end;
end;

function TSolution.Get_Process_Time: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.Time_Solve;
end;

function TSolution.Get_Total_Time: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.Total_Time;
end;

procedure TSolution.Set_Total_Time(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].Solution.Total_Time := Value;
end;

function TSolution.Get_Time_of_Step: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.Time_Step;
end;

function TSolution.Get_IntervalHrs: Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.IntervalHrs;
end;

procedure TSolution.Set_IntervalHrs(Value: Double);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].Solution.IntervalHrs := Value;
end;

function TSolution.Get_MinIterations: Integer;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit[ActiveActor].Solution.MinIterations
    else
        Result := 0;
end;

procedure TSolution.Set_MinIterations(Value: Integer);
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit[ActiveActor].Solution.MinIterations := Value;
end;

procedure TSolution.SolveAll;
var
    i: Integer;
begin
    IsSolveAll := TRUE;
    for i := 1 to NumOfActors do
    begin
        ActiveActor := i;
        CmdResult := DoSetCmd(1);
    end;
end;

function TSolution.Get_IncMatrix: Olevariant;
var
    i,
    Counter,
    IMIdx,
    Idx,
    ArrSize: Integer;
begin
    if (ActiveCircuit[ActiveActor] <> NIL) and (ActiveCircuit[ActiveActor].Solution.IncMat <> NIL) then
    begin
        with ActiveCircuit[ActiveActor].Solution do
        begin
            ArrSize := IncMat.NZero * 3;
            Result := VarArrayCreate([0, ArrSize], varInteger);
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
        end;
    end
    else
        Result := VarArrayCreate([0, 0], varInteger);
end;

function TSolution.Get_BusLevels: Olevariant;
var
    i,
    IMIdx,
    Idx,
    ArrSize: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        with ACtiveCircuit[ActiveActor].Solution do
        begin
            ArrSize := length(Inc_Mat_Levels) - 1;    // Removes the 3 initial zeros and the extra index
                                                  // Since it starts on 0
            Result := VarArrayCreate([0, ArrSize], varInteger);
            for IMIdx := 0 to ArrSize do
            begin
                Result[IMIdx] := Inc_Mat_levels[IMIdx];
            end;
        end;
    end
    else
        Result := VarArrayCreate([0, 0], varInteger);
end;

function TSolution.Get_IncMatrixRows: Olevariant;
var
    i,
    IMIdx,
    Idx,
    ArrSize: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        with ACtiveCircuit[ActiveActor].Solution do
        begin
            ArrSize := length(Inc_Mat_Rows) - 1;
            Result := VarArrayCreate([0, ArrSize], varOleStr);
            for IMIdx := 0 to ArrSize do
            begin
                Result[IMIdx] := Inc_Mat_Rows[IMIdx];
            end;
        end;
    end
    else
        Result := VarArrayCreate([0, 0], varInteger);
end;

function TSolution.Get_IncMatrixCols: Olevariant;
var
    i,
    IMIdx,
    Idx,
    ArrSize: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        with ActiveCircuit[ActiveActor].Solution, ActiveCircuit[ActiveActor] do
        begin
            if IncMat_Ordered then
            begin
                ArrSize := length(Inc_Mat_Cols) - 1;
                Result := VarArrayCreate([0, ArrSize], varOleStr);
                for IMIdx := 0 to ArrSize do
                begin
                    Result[IMIdx] := Inc_Mat_Cols[IMIdx];
                end;
            end
            else
            begin
                Result := VarArrayCreate([0, NumBuses - 1], varOleStr);
                for i := 0 to NumBuses - 1 do
                begin
                    Result[i] := BusList.Get(i + 1);
                end;
            end;
        end;
    end
    else
        Result := VarArrayCreate([0, 0], varInteger);
end;

function TSolution.Get_Laplacian: Olevariant;
var
    i,
    Counter,
    IMIdx,
    Idx,
    ArrSize: Integer;
begin
    if (ActiveCircuit[ActiveActor] <> NIL) and (ActiveCircuit[ActiveActor].Solution.Laplacian <> NIL) then
    begin
        with ActiveCircuit[ActiveActor].Solution do
        begin
            ArrSize := Laplacian.NZero * 3;
            Result := VarArrayCreate([0, ArrSize], varInteger);
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
        end;
    end
    else
        Result := VarArrayCreate([0, 0], varInteger);
end;

initialization
    TAutoObjectFactory.Create(ComServer, TSolution, Class_Solution, ciInternal, tmApartment);
    IsMultiThread := TRUE;
end.
