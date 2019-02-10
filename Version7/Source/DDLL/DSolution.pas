unit DSolution;

interface

function SolutionI(mode: Longint; arg: Longint): Longint; CDECL;
function SolutionF(mode: Longint; arg: Double): Double; CDECL;
function SolutionS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure SolutionV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    DSSGlobals,
    Math,
    LoadShape,
    Utilities,
    YMatrix,
    Variants,
    SolutionAlgs;

function SolutionI(mode: Longint; arg: Longint): Longint; CDECL;
begin
    Result := 0; // Default retirn value
    case mode of
        0:
        begin //Solution.solve
            if ActiveCircuit <> NIL then
                ActiveCircuit.Solution.Solve;
            Result := 0;
        end;
        1:
        begin // solution.mode - read
     //If ActiveCircuit <> Nil Then Result := GetSolutionModeID      changed to integer 8/16/00
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.Solution.Mode
            else
                Result := 0;
        end;
        2:
        begin // solution.mode - Write
            if ActiveCircuit <> NIL then
                ActiveCircuit.Solution.Mode := arg; //InterpretSolveMode(Value);
        end;
        3:
        begin // Solution.hour - read
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.Solution.DynaVars.intHour
            else
                Result := 0;
        end;
        4:
        begin // Solution.hour - Write
            if ActiveCircuit <> NIL then
                with  ActiveCircuit.Solution do
                begin
                    DynaVars.intHour := arg;
                    Update_dblHour;
                end;
        end;
        5:
        begin  // Solution.Year - read
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.Solution.Year
            else
                Result := 0;
        end;
        6:
        begin  // Solution.Year - Write
            if ActiveCircuit <> NIL then
                ActiveCircuit.Solution.Year := arg;
        end;
        7:
        begin  // Solution.Iterations
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.Solution.Iteration
            else
                Result := 0;
        end;
        8:
        begin  // Solution.MaxIterations - read
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.Solution.MaxIterations
            else
                Result := 0;
        end;
        9:
        begin  //Solution.MaxIterations - write
            if ActiveCircuit <> NIL then
                ActiveCircuit.Solution.MaxIterations := arg;
        end;
        10:
        begin // Solution.Number read
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.Solution.NumberOfTimes
            else
                Result := 0;
        end;
        11:
        begin  // Solution.Number write
            if ActiveCircuit <> NIL then
                ActiveCircuit.Solution.NumberOfTimes := arg;
        end;
        12:
        begin  // Solution.random read
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.Solution.RandomType
            else
                Result := 0;
        end;
        13:
        begin  // Solution.random write
            if ActiveCircuit <> NIL then
                ActiveCircuit.Solution.RandomType := arg;
        end;
        14:
        begin  // Solution.Loadmodel read
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.Solution.LoadModel
            else
                Result := 0;
        end;
        15:
        begin  // Solution.LoadModel Write
            if ActiveCircuit <> NIL then
                with ActiveCircuit.Solution do
                begin
                    LoadModel := arg;
                    DefaultLoadModel := LoadModel;
                end;
        end;
        16:
        begin  // Solution.AddType read
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.AutoAddObj.AddType
            else
                Result := 0;
        end;
        17:
        begin  // Solution.Addtype Write
            if ActiveCircuit <> NIL then
                ActiveCircuit.AutoAddObj.AddType := arg;
        end;
        18:
        begin  // Solution.Algorithm read
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.Solution.Algorithm
            else
                Result := 0;
        end;
        19:
        begin  // Solution.Algotihm Write
            if ActiveCircuit <> NIL then
                ActiveCircuit.Solution.Algorithm := arg;
        end;
        20:
        begin  // Solution.ControlMode read
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.Solution.ControlMode
            else
                Result := 0;
        end;
        21:
        begin // Solution.ControlMode Write
            if ActiveCircuit <> NIL then
                with ActiveCircuit.Solution do
                begin
                    ControlMode := arg;
                    DefaultControlMode := ControlMode;
                end;
        end;
        22:
        begin  // Solution.ControlIterations read
            if ActiveCircuit <> NIL then
            begin
                Result := ActiveCircuit.Solution.ControlIteration;
            end;
        end;
        23:
        begin  // Solution.ControlIterations Write
            if ActiveCircuit <> NIL then
            begin
                ActiveCircuit.Solution.ControlIteration := arg;
            end;
        end;
        24:
        begin  // Solution.MaxControlIterations read
            if ActiveCircuit <> NIL then
            begin
                Result := ActiveCircuit.Solution.MaxControlIterations;
            end;
        end;
        25:
        begin  // Solution.MaxControlIterations Write
            if ActiveCircuit <> NIL then
            begin
                ActiveCircuit.Solution.MaxControlIterations := arg;
            end;
        end;
        26:
        begin  // Solution.Sample_docontrolactions
            if ActiveCircuit <> NIL then
            begin
                ActiveCircuit.Solution.Sample_DoControlActions;
            end;
            Result := 0;
        end;
        27:
        begin // Solution.CheckFaultStatus
            if ActiveCircuit <> NIL then
            begin
                ActiveCircuit.Solution.Check_Fault_Status;
            end;
        end;
        28:
        begin  // Solution.SolveDirect
            if ActiveCircuit <> NIL then
            begin
                ActiveCircuit.Solution.SolveDirect;
            end;
        end;
        29:
        begin  // Solution.SolvePFlow
            if ActiveCircuit <> NIL then
            begin
                ActiveCircuit.Solution.DoPflowSolution;
            end;
        end;
        30:
        begin  // Solution.SolveNoControl
            if ActiveCircuit <> NIL then
            begin
                ActiveCircuit.Solution.SolveCircuit;
            end;
        end;
        31:
        begin  // Solution.SolvePlusControl
            if ActiveCircuit <> NIL then
            begin
                with ActiveCircuit.Solution do
                begin
                    SolveCircuit;
                    CheckControls;
                end;
            end;
        end;
        32:
        begin  // Solution.InitSnap
            if ActiveCircuit <> NIL then
            begin
                ActiveCircuit.Solution.SnapShotInit;
            end;
        end;
        33:
        begin  // Solution.CheckControls
            if ActiveCircuit <> NIL then
            begin
                ActiveCircuit.Solution.CheckControls;
            end;
        end;
        34:
        begin  // Solution.SampleControlDevices
            if ActiveCircuit <> NIL then
            begin
                ActiveCircuit.Solution.SampleControlDevices;
            end;
        end;
        35:
        begin  // Solution.DoControlActions
            if ActiveCircuit <> NIL then
            begin
                ActiveCircuit.Solution.DoControlActions;
            end;
        end;
        36:
        begin  // Solution.BuildYMatrix
            if ActiveCircuit <> NIL then
            begin
                Ymatrix.BuildYMatrix(arg, FALSE)
            end;
        end;
        37:
        begin // Solution.SystemYChanged
            if ActiveCircuit <> NIL then
            begin
                if ActiveCircuit.Solution.SystemYChanged then
                    Result := 1
                else
                    Result := 0;
            end;
        end;
        38:
        begin  // Solution.converged read
            if ActiveCircuit <> NIL then
            begin
                Result := 0;
                if ActiveCircuit.Issolved then
                    Result := 1;
            end;
        end;
        39:
        begin  // Solution.converged Write
            if ActiveCircuit <> NIL then
            begin
                if arg = 1 then
                begin
                    ActiveCircuit.Solution.ConvergedFlag := TRUE;
                    ActiveCircuit.Issolved := TRUE;
                end
                else
                begin
                    ActiveCircuit.Solution.ConvergedFlag := FALSE;
                    ActiveCircuit.Issolved := FALSE;
                end;
            end;
        end;
        40:
        begin // Solution.TotalIterations
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.Solution.Iteration
            else
                Result := 0;
        end;
        41:
        begin  // Solution.MostIterationsDone
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.Solution.MostIterationsDone
            else
                Result := 0;
        end;
        42:
        begin  // Solution.ControlActionsDone read
            Result := 0;
            if ActiveCircuit <> NIL then
                if ActiveCircuit.Solution.ControlActionsDone then
                    Result := 1;
        end;
        43:
        begin // Solution.ControlActionsDone Write
            if ActiveCircuit <> NIL then
                if arg = 1 then
                    ActiveCircuit.Solution.ControlActionsDone := TRUE
                else
                    ActiveCircuit.Solution.ControlActionsDone := FALSE;
        end;
        44:
        begin // Solution.FInishTimeStep
            if ActiveCircuit <> NIL then
                with ActiveCircuit, ActiveCircuit.Solution do
                begin
                    MonitorClass.SampleAll;  // Make all monitors take a sample
                    EndOfTimeStepCleanup;
                    Increment_time;
 //               DefaultHourMult := DefaultDailyShapeObj.getmult(TDynamicsrec.dblHour);
                end;
        end;
        45:
        begin  // Solution.cleanup
            if ActiveCircuit <> NIL then
                with ActiveCircuit, ActiveCircuit.Solution do
                begin
                    EndOfTimeStepCleanup;
                end;
        end
    else
        Result := -1;
    end;
end;

//***************************floating point variables*******************************
function SolutionF(mode: Longint; arg: Double): Double; CDECL;
begin
    Result := 0.0; // Default return value
    case mode of
        0:
        begin  // Solution.Frequency read
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.Solution.Frequency
            else
                Result := 0.0;
        end;
        1:
        begin  // Solution.Frequency Write
            if ActiveCircuit <> NIL then
                ActiveCircuit.Solution.Frequency := arg;
            Result := 0.0;
        end;
        2:
        begin  // Solution.Seconds read
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.Solution.dynavars.t
            else
                Result := 0.0;
        end;
        3:
        begin  // Solution.Seconds Write
            if ActiveCircuit <> NIL then
                ActiveCircuit.Solution.dynavars.t := arg;
            Result := 0.0;
        end;
        4:
        begin  // Solution.Stepsize read
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.Solution.dynavars.h
            else
                Result := 0.0;
        end;
        5:
        begin  // Solution.StepSize Write
            if ActiveCircuit <> NIL then
                ActiveCircuit.Solution.dynavars.h := arg;
            Result := 0.0;
        end;
        6:
        begin  // Solution.LoadMult read
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.LoadMultiplier
            else
                Result := 0.0;
        end;
        7:
        begin  // Solution.LoadMult Write
            if ActiveCircuit <> NIL then
                ActiveCircuit.LoadMultiplier := arg;
            Result := 0.0;
        end;
        8:
        begin  // Solution.Convergence read
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.Solution.ConvergenceTolerance
            else
                Result := 0.0;
        end;
        9:
        begin  // Solution.Convergence Write
            if ActiveCircuit <> NIL then
                ActiveCircuit.Solution.ConvergenceTolerance := arg;
            Result := 0.0;
        end;
        10:
        begin  // Solution.pctgrowth read
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    Result := (DefaultGrowthRate - 1.0) * 100.0
                end;
        end;
        11:
        begin  // Solution.pctGrowth Write
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    DefaultGrowthRate := 1.0 + arg / 100.0;
                    DefaultGrowthFactor := IntPower(DefaultGrowthRate, (Solution.Year - 1));
                end;
            Result := 0.0;
        end;
        12:
        begin  // Solution.GenkW read
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.AutoAddObj.GenkW
            else
                Result := 0.0;
        end;
        13:
        begin  // Solution.GenkW Write
            if ActiveCircuit <> NIL then
                ActiveCircuit.AutoAddObj.GenkW := arg;
            Result := 0.0;
        end;
        14:
        begin // Solution.GenPF read
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.AutoAddObj.GenPF
            else
                Result := 0.0;
        end;
        15:
        begin  // Solution.GenPF Write
            if ActiveCircuit <> NIL then
                ActiveCircuit.AutoAddObj.GenPF := arg;
            Result := 0.0;
        end;
        16:
        begin  // Solution.Capkvar read
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.AutoAddObj.Capkvar
            else
                Result := 0.0;
        end;
        17:
        begin  // Solution.Capkvar Write
            if ActiveCircuit <> NIL then
                ActiveCircuit.AutoAddObj.Capkvar := arg;
            Result := 0.0;
        end;
        18:
        begin  // Solution.GenMult read
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.GenMultiplier
            else
                Result := 0.0;
        end;
        19:
        begin // Solution.GenMult Write
            if ActiveCircuit <> NIL then
                ActiveCircuit.GenMultiplier := arg;
            Result := 0.0;
        end;
        20:
        begin  //Solution.dblHour read
            if ActiveCircuit <> NIL then
            begin
                Result := ActiveCircuit.Solution.DynaVars.dblHour;
            end;
        end;
        21:
        begin  // Solution.dblHour Write
            if ActiveCircuit <> NIL then
                with ActiveCircuit.Solution do
                begin
                    DynaVars.intHour := Trunc(arg);
                    DynaVars.dblHour := arg;
                    Dynavars.t := (arg - DynaVars.intHour) * 3600.0;
                end;
        end;
        22:
        begin  // Solution.StepSizeMin
            if ActiveCircuit <> NIL then
            begin
                ActiveCircuit.Solution.Dynavars.h := arg * 60.0;
            end;
            Result := 0.0;
        end;
        23:
        begin // Solution.StepSizeHr
            if ActiveCircuit <> NIL then
            begin
                ActiveCircuit.Solution.Dynavars.h := arg * 3600.0;
            end;
            Result := 0.0;
        end;
        24:
        begin // Solution.Process_Time
            if ActiveCircuit <> NIL then
            begin
                Result := ActiveCircuit.Solution.Time_Solve;
            end;
        end;
        25:
        begin // Solution.Total_Time read
            if ActiveCircuit <> NIL then
            begin
                Result := ActiveCircuit.Solution.Total_Time;
            end;
        end;
        26:
        begin // Solution.Total_Time Write
            if ActiveCircuit <> NIL then
            begin
                ActiveCircuit.Solution.Total_Time := arg;
            end;
        end;
        27:
        begin // Solution.Time_TimeStep
            if ActiveCircuit <> NIL then
            begin
                Result := ActiveCircuit.Solution.Time_Step;
            end;
        end
    else
        Result := -1.0;
    end;
end;

//***************************String type properties*******************************
function SolutionS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

var
    TestLoadShapeObj: TLoadShapeObj;

begin
    Result := pAnsiChar(Ansistring(''));  // Default return value
    case mode of
        0:
        begin  // Solution.ModeID
            if ActiveCircuit <> NIL then
                Result := pAnsiChar(Ansistring(GetSolutionModeID))
            else
                Result := pAnsiChar(Ansistring(''));
        end;
        1:
        begin  // Solution.LDCurve read
            if ActiveCircuit <> NIL then
                Result := pAnsiChar(Ansistring(ActiveCircuit.LoadDurCurve))
            else
                Result := pAnsiChar(Ansistring(''));
        end;
        2:
        begin  // Solution.LDCurve Write
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    LoadDurCurve := String(arg);
                    LoadDurCurveObj := LoadShapeClass.Find(LoadDurCurve);
                    if LoadDurCurveObj = NIL then
                        DoSimpleMsg('Load-Duration Curve not found.', 5001);
                end;
        end;
        3:
        begin  // Solution.DefaultDaily read
            if ActiveCircuit <> NIL then
                Result := pAnsiChar(Ansistring(ActiveCircuit.DefaultDailyShapeObj.Name))
            else
                Result := pAnsiChar(Ansistring(''));
        end;
        4:
        begin  // Solution.DefaultDaily Write
            if ActiveCircuit <> NIL then
            begin
                TestLoadShapeObj := LoadShapeClass.Find(String(arg));
                if TestLoadShapeObj <> NIL then
                    ActiveCircuit.DefaultDailyShapeObj := TestLoadShapeObj;
            end;
        end;
        5:
        begin  // Solution.DefaultYearly read
            if ActiveCircuit <> NIL then
                Result := pAnsiChar(Ansistring(ActiveCircuit.DefaultYearlyShapeObj.Name))
            else
                Result := pAnsiChar(Ansistring(''));
        end;
        6:
        begin  // Solution.DefaultYearly Write
            if ActiveCircuit <> NIL then
            begin
                TestLoadShapeObj := LoadShapeClass.Find(String(arg));
                if TestLoadShapeObj <> NIL then
                    ActiveCircuit.DefaultYearlyShapeObj := TestLoadShapeObj;
            end;
        end
    else
        Result := pAnsiChar(Ansistring('Error, paratemer not recognized'));
    end;
end;

//**********************************Variant type properties*******************************
procedure SolutionV(mode: Longint; out arg: Variant); CDECL;
var
    i: Integer;
begin
    case mode of
        0:
        begin  // Solution.EventLog
            if ActiveCircuit <> NIL then
            begin
                arg := VarArrayCreate([0, EventStrings.Count - 1], varOleStr);
                for i := 0 to EventStrings.Count - 1 do
                begin
                    arg[i] := EventStrings.Strings[i];
                end;
            end
            else
                arg := VarArrayCreate([0, 0], varOleStr);
            ;
        end
    else
        arg[0] := 'Error, paratemer not recognized';
    end;
end;

end.
