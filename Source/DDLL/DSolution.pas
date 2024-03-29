unit DSolution;

interface

function SolutionI(mode: Longint; arg: Longint): Longint; CDECL;
function SolutionF(mode: Longint; arg: Double): Double; CDECL;
function SolutionS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure SolutionV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
    DSSGlobals,
    Math,
    LoadShape,
    Utilities,
    YMatrix,
    Variants,
    SolutionAlgs,
    ExecOptions;

function SolutionI(mode: Longint; arg: Longint): Longint; CDECL;
var
    i: Integer;
begin
    Result := 0; // Default retirn value
    case mode of
        0:
        begin //Solution.solve
            if ActiveCircuit[ActiveActor] <> NIL then
                ActiveCircuit[ActiveActor].Solution.Solve(ActiveActor);
            Result := 0;
        end;
        1:
        begin // solution.mode - read
     //If ActiveCircuit[ActiveActor] <> Nil Then Result := GetSolutionModeID      changed to integer 8/16/00
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].Solution.Mode
            else
                Result := 0;
        end;
        2:
        begin // solution.mode - Write
            if ActiveCircuit[ActiveActor] <> NIL then
                ActiveCircuit[ActiveActor].Solution.Mode := arg; //InterpretSolveMode(Value);
        end;
        3:
        begin // Solution.hour - read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].Solution.DynaVars.intHour
            else
                Result := 0;
        end;
        4:
        begin // Solution.hour - Write
            if ActiveCircuit[ActiveActor] <> NIL then
                with  ActiveCircuit[ActiveActor].Solution do
                begin
                    DynaVars.intHour := arg;
                    Update_dblHour;
                end;
        end;
        5:
        begin  // Solution.Year - read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].Solution.Year
            else
                Result := 0;
        end;
        6:
        begin  // Solution.Year - Write
            if ActiveCircuit[ActiveActor] <> NIL then
                ActiveCircuit[ActiveActor].Solution.Year := arg;
        end;
        7:
        begin  // Solution.Iterations
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].Solution.Iteration
            else
                Result := 0;
        end;
        8:
        begin  // Solution.MaxIterations - read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].Solution.MaxIterations
            else
                Result := 0;
        end;
        9:
        begin  //Solution.MaxIterations - write
            if ActiveCircuit[ActiveActor] <> NIL then
                ActiveCircuit[ActiveActor].Solution.MaxIterations := arg;
        end;
        10:
        begin // Solution.Number read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].Solution.NumberOfTimes
            else
                Result := 0;
        end;
        11:
        begin  // Solution.Number write
            if ActiveCircuit[ActiveActor] <> NIL then
                ActiveCircuit[ActiveActor].Solution.NumberOfTimes := arg;
        end;
        12:
        begin  // Solution.random read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].Solution.RandomType
            else
                Result := 0;
        end;
        13:
        begin  // Solution.random write
            if ActiveCircuit[ActiveActor] <> NIL then
                ActiveCircuit[ActiveActor].Solution.RandomType := arg;
        end;
        14:
        begin  // Solution.Loadmodel read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].Solution.LoadModel
            else
                Result := 0;
        end;
        15:
        begin  // Solution.LoadModel Write
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor].Solution do
                begin
                    LoadModel := arg;
                    DefaultLoadModel := LoadModel;
                end;
        end;
        16:
        begin  // Solution.AddType read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].AutoAddObj.AddType
            else
                Result := 0;
        end;
        17:
        begin  // Solution.Addtype Write
            if ActiveCircuit[ActiveActor] <> NIL then
                ActiveCircuit[ActiveActor].AutoAddObj.AddType := arg;
        end;
        18:
        begin  // Solution.Algorithm read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].Solution.Algorithm
            else
                Result := 0;
        end;
        19:
        begin  // Solution.Algotihm Write
            if ActiveCircuit[ActiveActor] <> NIL then
                ActiveCircuit[ActiveActor].Solution.Algorithm := arg;
        end;
        20:
        begin  // Solution.ControlMode read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].Solution.ControlMode
            else
                Result := 0;
        end;
        21:
        begin // Solution.ControlMode Write
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor].Solution do
                begin
                    ControlMode := arg;
                    DefaultControlMode := ControlMode;
                end;
        end;
        22:
        begin  // Solution.ControlIterations read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Result := ActiveCircuit[ActiveActor].Solution.ControlIteration;
            end;
        end;
        23:
        begin  // Solution.ControlIterations Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ActiveCircuit[ActiveActor].Solution.ControlIteration := arg;
            end;
        end;
        24:
        begin  // Solution.MaxControlIterations read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Result := ActiveCircuit[ActiveActor].Solution.MaxControlIterations;
            end;
        end;
        25:
        begin  // Solution.MaxControlIterations Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ActiveCircuit[ActiveActor].Solution.MaxControlIterations := arg;
            end;
        end;
        26:
        begin  // Solution.Sample_docontrolactions
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ActiveCircuit[ActiveActor].Solution.Sample_DoControlActions(ActiveActor);
            end;
            Result := 0;
        end;
        27:
        begin // Solution.CheckFaultStatus
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ActiveCircuit[ActiveActor].Solution.Check_Fault_Status(ActiveActor);
            end;
        end;
        28:
        begin  // Solution.SolveDirect
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ActiveCircuit[ActiveActor].Solution.SolveDirect(ActiveActor);
            end;
        end;
        29:
        begin  // Solution.SolvePFlow
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ActiveCircuit[ActiveActor].Solution.DoPflowSolution(ActiveActor);
            end;
        end;
        30:
        begin  // Solution.SolveNoControl
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ActiveCircuit[ActiveActor].Solution.SolveCircuit(ActiveActor);
            end;
        end;
        31:
        begin  // Solution.SolvePlusControl
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Solution do
                begin
                    SolveCircuit(ActiveActor);
                    CheckControls(ActiveActor);
                end;
            end;
        end;
        32:
        begin  // Solution.InitSnap
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ActiveCircuit[ActiveActor].Solution.SnapShotInit(ActiveActor);
            end;
        end;
        33:
        begin  // Solution.CheckControls
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ActiveCircuit[ActiveActor].Solution.CheckControls(ActiveActor);
            end;
        end;
        34:
        begin  // Solution.SampleControlDevices
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ActiveCircuit[ActiveActor].Solution.SampleControlDevices(ActiveActor);
            end;
        end;
        35:
        begin  // Solution.DoControlActions
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ActiveCircuit[ActiveActor].Solution.DoControlActions(ActiveActor);
            end;
        end;
        36:
        begin  // Solution.BuildYMatrix
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Ymatrix.BuildYMatrix(arg, FALSE, ActiveActor)
            end;
        end;
        37:
        begin // Solution.SystemYChanged
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if ActiveCircuit[ActiveActor].Solution.SystemYChanged then
                    Result := 1
                else
                    Result := 0;
            end;
        end;
        38:
        begin  // Solution.converged read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Result := 0;
                if ActiveCircuit[ActiveActor].Solution.ConvergedFlag then
                    Result := 1;
            end;
        end;
        39:
        begin  // Solution.converged Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if arg = 1 then
                begin
                    ActiveCircuit[ActiveActor].Solution.ConvergedFlag := TRUE;
                    ActiveCircuit[ActiveActor].Issolved := TRUE;
                end
                else
                begin
                    ActiveCircuit[ActiveActor].Solution.ConvergedFlag := FALSE;
                    ActiveCircuit[ActiveActor].Issolved := FALSE;
                end;
            end;
        end;
        40:
        begin // Solution.TotalIterations
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].Solution.Iteration
            else
                Result := 0;
        end;
        41:
        begin  // Solution.MostIterationsDone
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].Solution.MostIterationsDone
            else
                Result := 0;
        end;
        42:
        begin  // Solution.ControlActionsDone read
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if ActiveCircuit[ActiveActor].Solution.ControlActionsDone then
                    Result := 1;
        end;
        43:
        begin // Solution.ControlActionsDone Write
            if ActiveCircuit[ActiveActor] <> NIL then
                if arg = 1 then
                    ActiveCircuit[ActiveActor].Solution.ControlActionsDone := TRUE
                else
                    ActiveCircuit[ActiveActor].Solution.ControlActionsDone := FALSE;
        end;
        44:
        begin // Solution.FInishTimeStep
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution do
                begin
                    MonitorClass[ActiveActor].SampleAll(ActiveActor);  // Make all monitors take a sample
                    EndOfTimeStepCleanup(ActiveActor);
                    Increment_time;
 //               DefaultHourMult := DefaultDailyShapeObj.getmult(TDynamicsrec.dblHour);
                end;
        end;
        45:
        begin  // Solution.cleanup
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution do
                begin
                    EndOfTimeStepCleanup(ActiveActor);
                end;
        end;
        46:
        begin  // Solution.SolveAll
            for i := 1 to NumOfActors do
            begin
                ActiveActor := i;
                CmdResult := DoSetCmd(1);
            end;
        end;
        47:
        begin  // Solution.CalcIncMatrix
            with ActiveCircuit[ActiveActor].Solution do
                Calc_Inc_Matrix(ActiveActor);
        end;
        48:
        begin  // Solution.CalcIncMatrix_O
            with ActiveCircuit[ActiveActor].Solution do
                Calc_Inc_Matrix_Org(ActiveActor);
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
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].Solution.Frequency
            else
                Result := 0.0;
        end;
        1:
        begin  // Solution.Frequency Write
            if ActiveCircuit[ActiveActor] <> NIL then
                ActiveCircuit[ActiveActor].Solution.Frequency := arg;
            Result := 0.0;
        end;
        2:
        begin  // Solution.Seconds read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].Solution.dynavars.t
            else
                Result := 0.0;
        end;
        3:
        begin  // Solution.Seconds Write
            if ActiveCircuit[ActiveActor] <> NIL then
                with  ActiveCircuit[ActiveActor].Solution do
                begin
                    DynaVars.t := arg;
                    Update_dblHour;
                end;
            Result := 0.0;
        end;
        4:
        begin  // Solution.Stepsize read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].Solution.dynavars.h
            else
                Result := 0.0;
        end;
        5:
        begin  // Solution.StepSize Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ActiveCircuit[ActiveActor].Solution.dynavars.h := arg;
      // Keep IntervalHrs in synch with time step size
                ActiveCircuit[ActiveActor].Solution.IntervalHrs := arg / 3600.0;
            end;
            Result := 0.0;
        end;
        6:
        begin  // Solution.LoadMult read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].LoadMultiplier
            else
                Result := 0.0;
        end;
        7:
        begin  // Solution.LoadMult Write
            if ActiveCircuit[ActiveActor] <> NIL then
                ActiveCircuit[ActiveActor].LoadMultiplier := arg;
            Result := 0.0;
        end;
        8:
        begin  // Solution.Convergence read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].Solution.ConvergenceTolerance
            else
                Result := 0.0;
        end;
        9:
        begin  // Solution.Convergence Write
            if ActiveCircuit[ActiveActor] <> NIL then
                ActiveCircuit[ActiveActor].Solution.ConvergenceTolerance := arg;
            Result := 0.0;
        end;
        10:
        begin  // Solution.pctgrowth read
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    Result := (DefaultGrowthRate - 1.0) * 100.0
                end;
        end;
        11:
        begin  // Solution.pctGrowth Write
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    DefaultGrowthRate := 1.0 + arg / 100.0;
                    DefaultGrowthFactor := IntPower(DefaultGrowthRate, (Solution.Year - 1));
                end;
            Result := 0.0;
        end;
        12:
        begin  // Solution.GenkW read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].AutoAddObj.GenkW
            else
                Result := 0.0;
        end;
        13:
        begin  // Solution.GenkW Write
            if ActiveCircuit[ActiveActor] <> NIL then
                ActiveCircuit[ActiveActor].AutoAddObj.GenkW := arg;
            Result := 0.0;
        end;
        14:
        begin // Solution.GenPF read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].AutoAddObj.GenPF
            else
                Result := 0.0;
        end;
        15:
        begin  // Solution.GenPF Write
            if ActiveCircuit[ActiveActor] <> NIL then
                ActiveCircuit[ActiveActor].AutoAddObj.GenPF := arg;
            Result := 0.0;
        end;
        16:
        begin  // Solution.Capkvar read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].AutoAddObj.Capkvar
            else
                Result := 0.0;
        end;
        17:
        begin  // Solution.Capkvar Write
            if ActiveCircuit[ActiveActor] <> NIL then
                ActiveCircuit[ActiveActor].AutoAddObj.Capkvar := arg;
            Result := 0.0;
        end;
        18:
        begin  // Solution.GenMult read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].GenMultiplier
            else
                Result := 0.0;
        end;
        19:
        begin // Solution.GenMult Write
            if ActiveCircuit[ActiveActor] <> NIL then
                ActiveCircuit[ActiveActor].GenMultiplier := arg;
            Result := 0.0;
        end;
        20:
        begin  //Solution.dblHour read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Result := ActiveCircuit[ActiveActor].Solution.DynaVars.dblHour;
            end;
        end;
        21:
        begin  // Solution.dblHour Write
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor].Solution do
                begin
                    DynaVars.intHour := Trunc(arg);
                    DynaVars.dblHour := arg;
                    Dynavars.t := (arg - DynaVars.intHour) * 3600.0;
                end;
        end;
        22:
        begin  // Solution.StepSizeMin
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ActiveCircuit[ActiveActor].Solution.Dynavars.h := arg * 60.0;
            end;
            Result := 0.0;
        end;
        23:
        begin // Solution.StepSizeHr
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ActiveCircuit[ActiveActor].Solution.Dynavars.h := arg * 3600.0;
            end;
            Result := 0.0;
        end;
        24:
        begin // Solution.Process_Time
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Result := ActiveCircuit[ActiveActor].Solution.Time_Solve;
            end;
        end;
        25:
        begin // Solution.Total_Time read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Result := ActiveCircuit[ActiveActor].Solution.Total_Time;
            end;
        end;
        26:
        begin // Solution.Total_Time Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ActiveCircuit[ActiveActor].Solution.Total_Time := arg;
            end;
        end;
        27:
        begin // Solution.Time_TimeStep
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Result := ActiveCircuit[ActiveActor].Solution.Time_Step;
            end;
        end
    else
        Result := -1.0;
    end;
end;

//***************************String type properties*******************************
function SolutionS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var
    TestLoadShapeObj: TLoadShapeObj;

begin
    Result := Pansichar(Ansistring(''));  // Default return value
    case mode of
        0:
        begin  // Solution.ModeID
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := Pansichar(Ansistring(GetSolutionModeID))
            else
                Result := Pansichar(Ansistring(''));
        end;
        1:
        begin  // Solution.LDCurve read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := Pansichar(Ansistring(ActiveCircuit[ActiveActor].LoadDurCurve))
            else
                Result := Pansichar(Ansistring(''));
        end;
        2:
        begin  // Solution.LDCurve Write
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    LoadDurCurve := String(arg);
                    LoadDurCurveObj := LoadShapeClass[ActiveActor].Find(LoadDurCurve);
                    if LoadDurCurveObj = NIL then
                        DoSimpleMsg('Load-Duration Curve not found.', 5001);
                end;
        end;
        3:
        begin  // Solution.DefaultDaily read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := Pansichar(Ansistring(ActiveCircuit[ActiveActor].DefaultDailyShapeObj.Name))
            else
                Result := Pansichar(Ansistring(''));
        end;
        4:
        begin  // Solution.DefaultDaily Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                TestLoadShapeObj := LoadShapeClass[ActiveActor].Find(String(arg));
                if TestLoadShapeObj <> NIL then
                    ActiveCircuit[ActiveActor].DefaultDailyShapeObj := TestLoadShapeObj;
            end;
        end;
        5:
        begin  // Solution.DefaultYearly read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := Pansichar(Ansistring(ActiveCircuit[ActiveActor].DefaultYearlyShapeObj.Name))
            else
                Result := Pansichar(Ansistring(''));
        end;
        6:
        begin  // Solution.DefaultYearly Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                TestLoadShapeObj := LoadShapeClass[ActiveActor].Find(String(arg));
                if TestLoadShapeObj <> NIL then
                    ActiveCircuit[ActiveActor].DefaultYearlyShapeObj := TestLoadShapeObj;
            end;
        end
    else
        Result := Pansichar(Ansistring('Error, paratemer not recognized'));
    end;
end;

//**********************************Variant type properties*******************************
procedure SolutionV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;
var
    Counter,
    i,
    IMIdx,
    ArrSize: Integer;
begin
    case mode of
        0:
        begin  // Solution.EventLog
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                for i := 0 to EventStrings[ActiveActor].Count - 1 do
                begin
                    WriteStr2Array(EventStrings[ActiveActor].Strings[i]);
                    WriteStr2Array(Char(0));
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        1:
        begin  // Solution.IncMatrix
            myType := 1;        // Integer
            setlength(myIntArray, 1);
            myIntArray[0] := 0;
            if (ActiveCircuit[ActiveActor] <> NIL) and (ActiveCircuit[ActiveActor].Solution.IncMat <> NIL) then
            begin
                with ActiveCircuit[ActiveActor].Solution do
                begin
                    ArrSize := IncMat.NZero * 3;
                    if ArrSize > 0 then
                        setlength(myIntArray, ArrSize);  // the while test 3 lines below should protect this if ArrSize <= 0
                    Counter := 0;
                    IMIdx := 0;
                    while IMIdx < ArrSize do
                    begin
                        for i := 0 to 2 do
                        begin
                            myIntArray[IMIdx] := IncMat.data[Counter][i];
                            inc(IMIdx)
                        end;
                        inc(Counter)
                    end;
                end;
            end;
            myPointer := @(myIntArray[0]);
            mySize := SizeOf(myIntArray[0]) * Length(myIntArray);
        end;
        2:
        begin  // Solution.BusLevels
            myType := 1;        // Integer
            setlength(myIntArray, 1);
            myIntArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ACtiveCircuit[ActiveActor].Solution do
                begin
                    ArrSize := length(Inc_Mat_Levels) - 1;    // Removes the 3 initial zeros and the extra index
                                                        // Since it starts on 0
                    if ArrSize > 0 then
                    begin
                        setlength(myIntArray, ArrSize);
                        for IMIdx := 0 to ArrSize do
                            myIntArray[IMIdx] := Inc_Mat_levels[IMIdx];
                    end;
                end;
            end;
            myPointer := @(myIntArray[0]);
            mySize := SizeOf(myIntArray[0]) * Length(myIntArray);
        end;
        3:
        begin  // Solution.IncMatrixRows
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ACtiveCircuit[ActiveActor].Solution do
                begin
                    ArrSize := length(Inc_Mat_Rows) - 1;
                    for IMIdx := 0 to ArrSize do
                    begin
                        WriteStr2Array(Inc_Mat_Rows[IMIdx]);
                        WriteStr2Array(Char(0));
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        4:
        begin  // Solution.IncMatrixCols
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor].Solution, ActiveCircuit[ActiveActor] do
                begin
                    if IncMat_Ordered then
                    begin
                        ArrSize := length(Inc_Mat_Cols) - 1;
                        for IMIdx := 0 to ArrSize do
                        begin
                            WriteStr2Array(Inc_Mat_Cols[IMIdx]);
                            WriteStr2Array(Char(0));
                        end;
                    end
                    else
                    begin
                        for i := 0 to NumBuses - 1 do
                        begin
                            WriteStr2Array(BusList.Get(i + 1));
                            WriteStr2Array(Char(0));
                        end;
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        5:
        begin  // Solution.Laplacian
            myType := 1;        // Integer
            setlength(myIntArray, 1);
            myIntArray[0] := 0;
            if (ActiveCircuit[ActiveActor] <> NIL) and (ActiveCircuit[ActiveActor].Solution.Laplacian <> NIL) then
            begin
                with ActiveCircuit[ActiveActor].Solution do
                begin
                    ArrSize := Laplacian.NZero * 3;
                    if ArrSize > 0 then
                        setlength(myIntArray, ArrSize);  // the while test 3 lines below should protect this if ArrSize <= 0
                    Counter := 0;
                    IMIdx := 0;
                    while IMIdx < ArrSize do
                    begin
                        for i := 0 to 2 do
                        begin
                            myIntArray[IMIdx] := Laplacian.data[Counter][i];
                            inc(IMIdx)
                        end;
                        inc(Counter)
                    end;
                end;
            end;
            myPointer := @(myIntArray[0]);
            mySize := SizeOf(myIntArray[0]) * Length(myIntArray);
        end
    else
    begin
        myType := 4;        // String
        setlength(myStrArray, 0);
        WriteStr2Array('Error, parameter not recognized');
        myPointer := @(myStrArray[0]);
        mySize := Length(myStrArray);
    end;
    end;

end;

end.
