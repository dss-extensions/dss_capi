unit SolutionAlgs;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{ Solution Algorithms}

{
   9-20-00  Added SolveDynamic

   1/22/01 Added SolutionAbort Check wherever solution in a potentially long loop
   4/2/04  Updated SolutionAbort to work through redirect files  and long scripts
}

interface

function SolveMonte1: Integer;   // Solve Monte Carlo Solution
function SolveMonte2: Integer;   // Solve Monte Carlo Solution
function SolveMonte3: Integer;   // Solve Monte Carlo Solution
function SolveMonteFault: Integer;  // Solve Monte Carlo Fault Study
function SolveFaultStudy: Integer;  // Full Fault Study
function SolveDaily: Integer;    // Solve Following Daily Cycle
function SolvePeakDay: Integer;   // Solve Following Daily Cycle at peak load
function SolveYearly: Integer;   // Solve Following Yearly Cycle
function SolveDuty: Integer;     // Solve Following Duty Cycle
function SolveDynamic: Integer;  // Solve Dynamics
function SolveLD1: Integer;      // solve Load-Duration Curve, 1
function SolveLD2: Integer;      // solve Load-Duration Curve, 2
function SolveHarmonic: Integer;
function SolveHarmonicT: Integer;  // Sequential-Time Harmonics, Added 07-06-2015
function SolveHarmTime: Integer;  // solve harmonics vs time (like general time mode) created by Davis Montenegro 25/06/2014
function SolveGeneralTime: Integer;

procedure ComputeYsc(iB: Integer);
procedure ComputeAllYsc;
procedure IntegratePCStates;
procedure EndOfTimeStepCleanup;
procedure FinishTimeStep;

implementation

uses
    ArrayDef,
    DSSGlobals,
{$IFDEF FPC}
    CmdForms,
{$ELSE}
    DSSForms,
{$ENDIF}
    Utilities,
    SysUtils,
    MathUtil,
    Math,
    Fault,
    uComplex,
    YMatrix,
    PCElement,
    Spectrum,
    Vsource,
    Isource,
    KLUSolve;

var
    ProgressCount: Integer;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure FinishTimeStep;
{
   Sample Cleanup and increment time

   For custom solutions.

}
begin
    MonitorClass.SampleAll;
    with ActiveCircuit.Solution do
    begin
        if SampleTheMeters then
            EnergyMeterClass.SampleAll;   // Save Demand interval Files

        EndOfTimeStepCleanup;
        Increment_time;
    end;
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure EndOfTimeStepCleanup;
{
   Put stuff in this procedure that needs to happen at the end of the time step
   in main solution loops (see below)
}
begin
    StorageClass.UpdateAll;
    InvControlClass.UpdateAll;
    ExpControlClass.UpdateAll;

    // End of Time Step Timer
    ActiveCircuit.Solution.UpdateLoopTime;
    MonitorClass.SampleAllMode5;  // sample all mode 5 monitors to get timings
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure Show10PctProgress(i, N: Integer);

begin
    if NoFormsAllowed then
        Exit;

    if ((i * 10) div N) > ProgressCount then
    begin
        Inc(ProgressCount);
        ShowPctProgress(ProgressCount * 10);
    end;
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveYearly: Integer;

var
    N, Twopct: Integer;

begin
    Result := 0;
    ProgressCaption('Solving Year ' + IntToStr(ActiveCircuit.Solution.Year));
    ProgressCount := 0;
    ShowPctProgress(ProgressCount);

    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        try
            IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage elements
            if not DIFilesAreOpen then
                EnergyMeterClass.OpenAllDIFiles;   // Open Demand Interval Files, if desired   Creates DI_Totals
            Twopct := Max(NumberOfTimes div 50, 1);
            for N := 1 to NumberOfTimes do
                if not SolutionAbort then
                    with Dynavars do
                    begin
                        Increment_time;
                        DefaultHourMult := DefaultYearlyShapeObj.getmult(dblHour);
                        if PriceCurveObj <> NIL then
                            PriceSignal := PriceCurveObj.GetPrice(dblHour);
                        SolveSnap;
                        MonitorClass.SampleAll;  // Make all monitors take a sample
                        if SampleTheMeters then
                            EnergyMeterClass.SampleAll; // Make all Energy Meters take a sample

                        EndOfTimeStepCleanup;

                        if (N mod Twopct) = 0 then
                            ShowPctProgress((N * 100) div NumberofTimes);
                    end;
        finally
            ProgressHide;
            MonitorClass.SaveAll;
    // EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files    See DIFilesAreOpen Logic
        end;
    end;
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveDaily: Integer;

{
  Solves following the daily load curve.
  Stepsize defaults to 1 hr and number of times = 24.
  Load is modified by yearly growth, time of day, and global load multiplier.
}

var
    N: Integer;

begin
    Result := 0;

    with ActiveCircuit, ActiveCircuit.Solution do
    begin
      // t:=0.0;
      // MonitorClass.ResetAll;
      // EnergyMeterClass.ResetAll;
        try

            IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters
            if not DIFilesAreOpen then
                EnergyMeterClass.OpenAllDIFiles;   // Append Demand Interval Files, if desired

            for N := 1 to NumberOfTimes do
                if not SolutionAbort then
                    with DynaVars do
                    begin
                        Increment_time;
                        DefaultHourMult := DefaultDailyShapeObj.getmult(dblHour);
                        if PriceCurveObj <> NIL then
                            PriceSignal := PriceCurveObj.GetPrice(dblHour);
                        SolveSnap;
                        MonitorClass.SampleAll;  // Make all monitors take a sample
                        if SampleTheMeters then
                            EnergyMeterClass.SampleAll; // Make all Energy Meters take a sample

                        EndOfTimeStepCleanup;

                    end;

        finally
            MonitorClass.SaveAll;
            if SampleTheMeters then
                EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files
        end; {Try}
    end;  {WITH}
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolvePeakDay: Integer;

{
 Solves peak day

    Takes the given load kW and assumes it represents the peak value.
    Load is modified by daily load curve and growth factor for the year.
    'h' defaults to 3600 (1 hr) but can be reset to anything.
    Differs from Daily mode in that the global load multiplier is ignored.
}

var
    N: Integer;

begin
    Result := 0;
    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        DynaVars.t := 0.0;

        // MonitorClass.ResetAll;
        // EnergyMeterClass.ResetAll;
        try
            DynaVars.intHour := 0;
            DynaVars.dblHour := 0.0;
            IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
            if not DIFilesAreOpen then
                EnergyMeterClass.OpenAllDIFiles;   // Open Demand Interval Files, if desired

            for N := 1 to NumberOfTimes do
                if not SolutionAbort then
                    with DynaVars do
                    begin
                        Increment_time;
                        DefaultHourMult := DefaultDailyShapeObj.GetMult(dblHour);
                        if PriceCurveObj <> NIL then
                            PriceSignal := PriceCurveObj.GetPrice(dblHour);
                        SolveSnap;
                        MonitorClass.SampleAll;  // Make all monitors take a sample
                        if SampleTheMeters then
                            EnergyMeterClass.SampleAll; // Make all Energy Meters take a sample

                        EndOfTimeStepCleanup;

                    end;
        finally
            MonitorClass.SaveAll;
            if SampleTheMeters then
                EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files
        end;
    end;  {WITH}
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveDuty: Integer;

var
    N, TwoPct: Integer;

begin
    Result := 0;

    ProgressCaption('Duty Cycle Solution');
    ProgressCount := 0;
    ShowPctProgress(0);

    with ActiveCircuit, ActiveCircuit.Solution do
    begin
     //   t:=0.0;
        // MonitorClass.ResetAll;
        TwoPct := Max(1, NumberOfTimes div 50);
        try
            IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
            for N := 1 to NumberOfTimes do
                if not SolutionAbort then
                    with DynaVars do
                    begin
                        Increment_time;
                        DefaultHourMult := DefaultDailyShapeObj.getmult(dblHour);
            // Assume pricesignal stays constant for dutycycle calcs
                        SolveSnap;
                        MonitorClass.SampleAll;  // Make all monitors take a sample
                        if SampleTheMeters then
                            EnergyMeterClass.SampleAll; // Make all Energy Meters take a sample

                        EndOfTimeStepCleanup;


                        if (N mod Twopct) = 0 then
                            ShowPctProgress((N * 100) div NumberofTimes);
                    end;
        finally
            MonitorClass.SaveAll;
            if SampleTheMeters then
                EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files
            ProgressHide;
        end;
    end;
end;

function SolveGeneralTime: Integer;

{
   For Rolling your own solution modes
}
var
    N: Integer;

begin
    Result := 0;

    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
        for N := 1 to NumberOfTimes do
            if not SolutionAbort then
                with DynaVars do
                begin
              {Compute basic multiplier from Default loadshape to use in generator dispatch, if any}
                    DefaultHourMult := DefaultDailyShapeObj.getmult(dblHour);

                    SolveSnap;

                    FinishTimeStep;

                end;
    end;
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure IntegratePCStates;
 {Integrate states in all PC Elements.  At present, only PC Elements
  can have dynamic states}

var
    pcelem: TPCElement;

begin
    with ActiveCircuit do
    begin
        pcelem := PCelements.First;
        while pcelem <> NIL do
        begin
            pcelem.IntegrateStates;
            pcelem := PCelements.Next;
        end;
    end;
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveDynamic: Integer;

var
    N: Integer;

begin
    Result := 0;

    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        try
            SolutionInitialized := TRUE; // If we're in dynamics mode, no need to re-initialize.
            IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
            for N := 1 to NumberOfTimes do
                if not SolutionAbort then
                    with DynaVars do
                    begin
                        Increment_time;
                        DefaultHourMult := DefaultDailyShapeObj.getmult(dblHour);
          // Assume price signal stays constant for dynamic calcs
       {Predictor}
                        IterationFlag := 0;
                        IntegratePCStates;
                        SolveSnap;
       {Corrector}
                        IterationFlag := 1;
                        IntegratePCStates;
                        SolveSnap;
                        MonitorClass.SampleAll;  // Make all monitors take a sample

                        EndOfTimeStepCleanup;

                    end;
        finally
            MonitorClass.SaveAll;
        end;
    end;
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveMonte1: Integer;

var
    N: Integer;

begin
    Result := 0;

    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        try
            LoadMultiplier := 1.0;   // Always set with prop in case matrix must be rebuilt
            IntervalHrs := 1.0;     // needed for energy meters and storage devices
            DynaVars.intHour := 0;
            DynaVars.dblHour := 0.0;// Use hour to denote Case number
            DynaVars.t := 0.0;

        // MonitorClass.ResetAll;
        // EnergyMeterClass.ResetAll;

            ProgressCaption('Monte Carlo Mode 1, ' + IntToStr(NumberofTimes) + ' Random Loads.');
            ProgressCount := 0;

            for N := 1 to NumberOfTimes do
                if not SolutionAbort then
                begin
                    Inc(DynaVars.intHour);
                    SolveSnap;
                    MonitorClass.SampleAll;  // Make all monitors take a sample
                    if SampleTheMeters then
                        EnergyMeterClass.SampleAll;  // Make all meters take a sample
                    Show10PctProgress(N, NumberOfTimes);
                end
                else
                begin
                    ErrorNumber := SOLUTION_ABORT;
                    CmdResult := ErrorNumber;
                    GlobalResult := 'Solution Aborted';
                    Break;
                end;
        finally
            MonitorClass.SaveAll;
            if SampleTheMeters then
                EnergyMeterClass.CloseAllDIFiles;
            ProgressHide;
        end;
    end;

end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveMonte2: Integer;

// Do a daily load solution for several Random days

var
    i, N, Ndaily: Integer;

begin
    Result := 0;

    with ActiveCircuit, ActiveCircuit.solution do
    begin
        try
            DynaVars.t := 0.0;
            DynaVars.intHour := 0;
            DynaVars.dblHour := 0.0;
        // MonitorClass.ResetAll;
        // EnergyMeterClass.ResetAll;
            IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
            Ndaily := Round(24.0 / IntervalHrs);

            if not DIFilesAreOpen then
                EnergyMeterClass.OpenAllDIFiles;   // Open Demand Interval Files, if desired

            ProgressCaption('Monte Carlo Mode 2, ' + IntToStr(NumberofTimes) + ' Days.');
            ProgressCount := 0;

            for N := 1 to NumberOfTimes do

                if not SolutionAbort then
                begin       // Number of Days

          // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
                    case Randomtype of
                        UNIFORM:
                            LoadMultiplier := Random;  // number between 0 and 1
                        GAUSSIAN:
                            LoadMultiplier := Gauss(DefaultDailyShapeObj.Mean, DefaultDailyShapeObj.StdDev);
                    end;

                    with DynaVars do
                        for i := 1 to Ndaily do
                        begin
                            Increment_time;
                            DefaultHourMult := DefaultDailyShapeObj.GetMult(dblHour);
                            SolveSnap;

                            MonitorClass.SampleAll;  // Make all monitors take a sample
                            if SampleTheMeters then
                                EnergyMeterClass.SampleAll;
                            ;  // Make all meters take a sample

                            EndOfTimeStepCleanup;

                        end;

                    Show10PctProgress(N, NumberOfTimes);

                end
                else
                begin
                    ErrorNumber := SOLUTION_ABORT;
                    CmdResult := ErrorNumber;
                    GlobalResult := 'Solution Aborted.';
                    Break;
                end;
        finally
            MonitorClass.SaveAll;
            if SampleTheMeters then
                EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files
            ProgressHide;
        end;
    end;
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveMonte3: Integer;

// Hold time fixed and just vary the global load multiplier

var
    N: Integer;

begin
    Result := 0;

    with ActiveCircuit, ActiveCircuit.Solution do
    begin
    // Time must be set beFore entering this routine
        try
        // MonitorClass.ResetAll;
        // EnergyMeterClass.ResetAll;
            IntervalHrs := 1.0;  // just get per unit energy and multiply result as necessary

            if not DIFilesAreOpen then
                EnergyMeterClass.OpenAllDIFiles;   // Open Demand Interval Files, if desired

            ProgressCaption('Monte Carlo Mode 3, ' + IntToStr(NumberofTimes) + ' Different Load Levels.');
            ProgressCount := 0;

            DefaultHourMult := DefaultDailyShapeObj.GetMult(DynaVars.dblHour);
            if PriceCurveObj <> NIL then
                PriceSignal := PriceCurveObj.GetPrice(DynaVars.dblHour);

            for N := 1 to NumberOfTimes do
                if not SolutionAbort then
                begin

        // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
                    case Randomtype of
                        UNIFORM:
                            LoadMultiplier := Random;  // number between 0 and 1
                        GAUSSIAN:
                            LoadMultiplier := Gauss(DefaultDailyShapeObj.Mean, DefaultDailyShapeObj.StdDev);
                        LOGNORMAL:
                            LoadMultiplier := QuasiLognormal(DefaultDailyShapeObj.Mean);
                    end;

                    SolveSnap;

                    MonitorClass.SampleAll;  // Make all monitors take a sample
                    if SampleTheMeters then
                        EnergyMeterClass.SampleAll;  // Make all meters take a sample

                    Show10PctProgress(N, NumberOfTimes);
                end
                else
                begin
                    CmdResult := SOLUTION_ABORT;
                    ErrorNumber := CmdResult;
                    GlobalResult := 'Solution Aborted';
                    Break;
                end;
        finally
            MonitorClass.SaveAll;
            if SampleTheMeters then
                EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files
            ProgressHide;
        end;
    end; {WITH}
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveLD1: Integer;

// Do a Daily Simulation based on a load duration curve

var
    N, Ndaily, i: Integer;


begin
    Result := 0;

    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        try
            if LoadDurCurveObj = NIL then
            begin
                Dosimplemsg('Load Duration Curve Not Defined (Set LDCurve=... command). Cannot perForm solution.', 470);
                Exit;
            end;

  // Time must be set beFore entering this routine

      // MonitorClass.ResetAll;
      // EnergyMeterClass.ResetAll;

            NDaily := Round(24.0 / DynaVars.h * 3600.0);

            if not DIFilesAreOpen then
                EnergyMeterClass.OpenAllDIFiles;   // Open Demand Interval Files, if desired

            ProgressCaption('Load-Duration Mode 1 Solution. ');

      // (set in Solve method) DefaultGrowthFactor :=  IntPower(DefaultGrowthRate, (Year-1));

            DynaVars.intHour := 0;
            with DynaVars do
                for i := 1 to Ndaily do
                begin

      // Set the time
                    Increment_time;

                    DefaultHourMult := DefaultDailyShapeObj.GetMult(dblHour);

                    if not SolutionAbort then
                    begin
                        for N := 1 to LoadDurCurveObj.NumPoints do
                        begin

                            LoadMultiplier := LoadDurCurveObj.Mult(N);  // Always set LoadMultiplier with prop in case matrix must be rebuilt
              // Adjust meter interval to interval on value of present Load-Duration Curve
                            IntervalHrs := LoadDurCurveObj.PresentInterval;

          // Price curve must correspond to load-duration curve
                            if PriceCurveObj <> NIL then
                                PriceSignal := PriceCurveObj.Price(N);

                            SolveSnap;

                            MonitorClass.SampleAll;     // Make all monitors take a sample
                            if SampleTheMeters then
                                EnergyMeterClass.SampleAll;  // Make all meters take a sample

                            EndOfTimeStepCleanup;


                        end;
                        ShowPctProgress((i * 100) div NDaily);
                    end
                    else
                    begin
                        CmdResult := SOLUTION_ABORT;
                        ErrorNumber := CmdResult;
                        GlobalResult := 'Solution Aborted';
                        Break;
                    end;

                end;
        finally
            MonitorClass.SaveAll;
            if SampleTheMeters then
                EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files
            ProgressHide;
        end;
    end; {WITH ActiveCircuit}

end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveLD2: Integer;

// Hold time fixed and just vary the global load multiplier according to the global
// Load-Duration Curve

var
    N: Integer;

begin
    Result := 0;

    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        if LoadDurCurveObj = NIL then
        begin
            Dosimplemsg('Load Duration Curve Not Defined (Set LDCurve=... command). Cannot perForm solution.', 471);
            Exit;
        end;

// Time must be set beFore entering this routine


    // MonitorClass.ResetAll;
    // EnergyMeterClass.ResetAll;

        DefaultHourMult := DefaultDailyShapeObj.GetMult(DynaVars.dblHour);
        if not DIFilesAreOpen then
            EnergyMeterClass.OpenAllDIFiles;   // Open Demand Interval Files, if desired

    // (set in Solve Method) DefaultGrowthFactor :=  IntPower(DefaultGrowthRate, (Year-1));

        try
            if SolutionAbort then
            begin
                CmdResult := SOLUTION_ABORT;
                ErrorNumber := CmdResult;
                GlobalResult := 'Solution Aborted.';
                Exit;
            end;

            for N := 1 to LoadDurCurveObj.NumPoints do
            begin

        // Adjust meter interval to interval on value of present Load-Duration Curve
                LoadMultiplier := LoadDurCurveObj.Mult(N);     // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
                IntervalHrs := LoadDurCurveObj.PresentInterval;

        // Price curve must correspond to load-duration curve
                if PriceCurveObj <> NIL then
                    PriceSignal := PriceCurveObj.Price(N);

                SolveSnap;

                MonitorClass.SampleAll;  // Make all monitors take a sample
                if SampleTheMeters then
                    EnergyMeterClass.SampleAll;  // Make all meters take a sample

                EndOfTimeStepCleanup;

            end;
        finally
            MonitorClass.SaveAll;
            if SampleTheMeters then
                EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files
        end;
    end; {WITH ActiveCircuit}

end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure PickAFault;
// Enable one of the faults in the circuit.  Disable the rest
var
    NumFaults, i, Whichone: Integer;
    FaultObj: TFaultObj;
begin
    NumFaults := ActiveCircuit.Faults.Listsize;
    Whichone := Trunc(Random * NumFaults) + 1;
    if Whichone > NumFaults then
        Whichone := NumFaults;

    for i := 1 to NumFaults do
    begin
        FaultObj := ActiveCircuit.Faults.Get(i);
        if i = Whichone then
        begin
            ActiveFaultObj := FaultObj; // in Fault Unit
            FaultObj.Enabled := TRUE;
        end
        else
            FaultObj.Enabled := FALSE;
    end;
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveMonteFault: Integer;

var
    N: Integer;

begin
    Result := 0;

    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        try
            LoadModel := ADMITTANCE;   // All Direct solution
            LoadMultiplier := 1.0;    // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
            DynaVars.intHour := 0;
            DynaVars.dblHour := 0.0; // Use hour to denote Case number
            DynaVars.t := 0.0;


      // MonitorClass.ResetAll;

            ProgressCaption('Monte Carlo Fault Study: ' + IntToStr(NumberofTimes) + ' Different Faults.');
            ProgressCount := 0;

            SetGeneratorDispRef;

            for N := 1 to NumberOfTimes do
                if not SolutionAbort then
                begin
                    Inc(DynaVars.intHour);
                    PickAFault;  // Randomly enable one of the faults
                    ActiveFaultObj.Randomize;  // Randomize the fault resistance
                    SolveDirect;
                    MonitorClass.SampleAll;  // Make all monitors take a sample

                    Show10PctProgress(N, NumberOfTimes);
                end;
        finally
            MonitorClass.SaveAll;
            ProgressHide;
        end;
    end;

end;

{--------------------------------------------------------------------------}
procedure AllocateAllSCParms;
var
    i: Integer;
begin
    with ActiveCircuit do
    begin
        for i := 1 to NumBuses do
            Buses^[i].AllocateBusQuantities;
    end;
end;


{--------------------------------------------------------------------------}
procedure ComputeIsc;
{ Compute Isc at all buses for current values of Voc and Ysc }
var
    i: Integer;
begin
    with ActiveCircuit do
    begin
        for i := 1 to NumBuses do
            with Buses^[i] do
            begin
                Ysc.MVMult(BusCurrent, VBus);
            end;
    end;
end;


{--------------------------------------------------------------------------}
procedure ComputeYsc(iB: Integer);

{Compute YSC for I-th bus}
{Assume InjCurr is zeroed}

var
    i,
    j,
    ref1: Integer;

begin
    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        with Buses^[iB] do
        begin
            Zsc.Clear;
            for i := 1 to NumNodesThisBus do
            begin
                ref1 := GetRef(i);
                if ref1 > 0 then
                begin
                    Currents^[ref1] := cONE;
          {SparseSet expects 1st element of voltage array, not 0-th element}
                    if SolveSparseSet(hYsystem, @NodeV^[1], @Currents^[1]) < 1 then
                        raise EEsolv32Problem.Create('Error Solving System Y Matrix in ComputeYsc. Problem with Sparse matrix solver.');
          {Extract Voltage Vector = column of Zsc}
                    for j := 1 to NumNodesThisBus do
                    begin
                        Zsc.SetElement(j, i, NodeV^[GetRef(j)]);
                    end;
                    Currents^[Ref1] := cZERO;
                end; {IF ref...}
            end;
            Ysc.CopyFrom(Zsc);
            Ysc.invert; {Save as admittance}
        end;
    end;
end;


{--------------------------------------------------------------------------}
procedure ComputeAllYsc;
var
    iB, j: Integer;


begin

    with ActiveCircuit, ActiveCircuit.Solution do
    begin

        for j := 1 to NumNodes do
            Currents^[j] := cZERO;

        ProgressCount := 0;

        for iB := 1 to NumBuses do
        begin
            ComputeYsc(iB);  // Compute YSC for iB-th Bus
            if ((iB * 10) div NumBuses) > ProgressCount then
            begin
                Inc(ProgressCount);
                ShowPctProgress(30 + ProgressCount * 5);
            end;
        end;
    end;
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure DisableAllFaults;
begin
    with ActiveCircuit do
    begin
        ActiveFaultObj := Faults.First;
        while ActiveFaultObj <> NIL do
        begin
            ActiveFaultObj.Enabled := FALSE;
            ActiveFaultObj := Faults.Next;
        end
    end;
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveFaultStudy: Integer;


begin
    Result := 0;

    ShowPctProgress(0);
    ProgressCaption('Computing Open-Circuit Voltages');

    with ActiveCircuit.solution do
    begin
        LoadModel := ADMITTANCE;
        DisableAllFaults;

        SolveDirect;   // This gets the open circuit voltages and bus lists corrected

        AllocateAllSCParms;   // Reallocate bus quantities
        UpdateVBus;  // Put present solution Voc's in bus quantities
    end;

    ProgressCaption('Computing Ysc Matrices for Each Bus');
    ShowPctProgress(30);
    ComputeAllYsc;

    ProgressCaption('Computing Short-circuit currents.');
    ShowPctProgress(80);
    ComputeIsc;

    ShowPctProgress(100);
    ProgressCaption('Done.');
    ProgressHide;
   // Now should have all we need to make a short circuit report

end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure AddFrequency(var FreqList: pDoublearray; var NumFreq, MaxFreq: Integer; F: Double);

{Add unique Frequency, F to list in ascending order, reallocating if necessary}

var
    i, j: Integer;

begin

     {See if F is in List}

    for i := 1 to NumFreq do
    begin
         {Allow a little tolerance (0.1 hz) for the Frequency for round off error}
        if Abs(F - FreqList^[i]) < 0.1 then
            Exit; // Already in List, nothing to do
    end;

     {OK, it's not in list, so let's Add it}
    Inc(NumFreq);
    if NumFreq > MaxFreq then
    begin  // Let's make a little more room
        Inc(MaxFreq, 20);
        ReallocMem(FreqList, SizeOf(FreqList^[1]) * MaxFreq);
    end;

     {Let's add it in ascending order}
    for i := 1 to NumFreq - 1 do
    begin
        if F < FreqList^[i] then
        begin
             {Push down array and insert it}
            for j := NumFreq - 1 downto i do
                FreqList^[j + 1] := FreqList^[j];
            FreqList^[i] := F;
            Exit;  // We're done!
        end;
    end;

     {If we fall through, tack it on to the end}
    FreqList^[NumFreq] := F;

end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function GetSourceFrequency(pc: TPCElement): Double; // TODO - applicable to VCCS?

var
    pVsrc: TVsourceObj;
    pIsrc: TIsourceObj;
begin

    if Comparetext(pc.DSSClassName, 'vsource') = 0 then
    begin
        pVsrc := pc as TVsourceObj;
        Result := pVsrc.srcFrequency;
    end
    else
    begin
        pIsrc := pc as TIsourceObj;
        Result := pIsrc.srcFrequency;
    end;

end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure CollectAllFrequencies(var FreqList: pDoubleArray; var NumFreq: Integer);

var
    SpectrumInUse: pIntegerArray;
    p: TPCElement;
    MaxFreq, i, j: Integer;
    pSpectrum: TSpectrumObj;
    f: Double;

begin
    {Make a List of all frequencies in Use}

    {accumulate all unique Frequencies}
    MaxFreq := 20;    // Initial List size
    NumFreq := 0;
    Reallocmem(FreqList, Sizeof(FreqList^[1]) * MaxFreq);

    with ActiveCircuit do
    begin
        {Check Sources -- each could have a different base frequency}
        p := Sources.First;
        while p <> NIL do
        begin
            if p.Enabled then
                if SpectrumClass.Find(p.Spectrum) <> NIL then
                begin
                    pSpectrum := SpectrumClass.GetActiveObj;
                    f := GetSourceFrequency(p);
                    for j := 1 to pSpectrum.NumHarm do
                    begin
                        AddFrequency(FreqList, NumFreq, MaxFreq, pSpectrum.HarmArray^[j] * f);
                    end;
                end;
            p := Sources.Next;
        end;
    end;

    {Mark Spectra being used}
        {Check loads and generators - these are assumed to be at fundamental frequency}
    SpectrumInUse := AllocMem(SizeOf(Integer) * SpectrumClass.ElementCount);  //Allocate and zero
    with ActiveCircuit do
    begin
        p := PCelements.First;
        while p <> NIL do
        begin
            if p.enabled then
                if SpectrumClass.Find(p.Spectrum) <> NIL then
                begin
                    SpectrumInUse^[SpectrumClass.Active] := 1;
                end;
            p := PCelements.Next;
        end;
    end; {With}

    {Add marked Spectra to list}
    for i := 1 to SpectrumClass.ElementCount do
    begin
        if SpectrumInUse^[i] = 1 then
        begin
            SpectrumClass.Active := i;
            pSpectrum := SpectrumClass.GetActiveObj;
            for j := 1 to pSpectrum.NumHarm do
            begin
                AddFrequency(FreqList, NumFreq, MaxFreq, pSpectrum.HarmArray^[j] * ActiveCircuit.Fundamental);
            end;
        end;
    end;

    ReallocMem(SpectrumInUse, 0);


end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveHarmonic: Integer;

var
    FrequencyList: pDoubleArray;
    i, NFreq: Integer;

begin
    Result := 0;

    FrequencyList := NIL;   // Set up for Reallocmem
    ShowPctProgress(0);
    ProgressCaption('Performing Harmonic Solution');

    with ActiveCircuit, ActiveCircuit.solution do
    begin
        try

            if Frequency <> Fundamental then
            begin     // Last solution was something other than fundamental
                Frequency := Fundamental;
                if not RetrieveSavedVoltages then
                    Exit;  {Get Saved fundamental frequency solution}
            end;

            MonitorClass.SampleAll;   // Store the fundamental frequency in the monitors

       { Get the list of Harmonic Frequencies to solve at}
            if DoAllHarmonics then
                CollectAllFrequencies(FrequencyList, NFreq)   // Allocates FrequencyList
            else
            begin
                Reallocmem(FrequencyList, Sizeof(FrequencyList^[1]) * HarmonicListSize);
                NFreq := HarmonicListSize;
                for i := 1 to NFreq do
                    FrequencyList^[i] := Fundamental * HarmonicList^[i];
            end;

            for i := 1 to NFreq do
            begin

                Frequency := FrequencyList^[i];   // forces rebuild of SystemY
                if Abs(Harmonic - 1.0) > EPSILON then
                begin    // Skip fundamental
                    ProgressCaption('Solving at Frequency = ' + Format('%-g', [Frequency]));
                    ShowPctProgress(Round((100.0 * i) / Nfreq));
                    SolveDirect;
                    MonitorClass.SampleAll;
               // Storage devices are assumed to stay the same since there is no time variation in this mode
                end;

            end; {FOR}

            ShowPctProgress(100);
            ProgressCaption('Done.');
        finally
            ProgressHide;
            MonitorClass.SaveAll;
            ReallocMem(FrequencyList, 0);
        end;
     // Now should have all we need to make a short circuit report

    end;

end;
//========================================================================================
function SolveHarmTime: Integer;     // It is based in SolveGeneralTime routine

begin
    Result := 0;

    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
        if not SolutionAbort then
            with DynaVars do
            begin
              {Compute basic multiplier from Default loadshape to use in generator dispatch, if any}
                DefaultHourMult := DefaultDailyShapeObj.getmult(dblHour);

                SolveSnap;
          //      Increment_time;  // This function is handeled from SolveHarmonics (04-10-2013)
            end;
    end;
end;
//=============================================================================
function SolveHarmonicT: Integer;
var
    FrequencyList: pDoubleArray;
    i, NFreq: Integer;

begin
    Result := 0;

    FrequencyList := NIL;   // Set up for Reallocmem

    with ActiveCircuit, ActiveCircuit.solution do
    begin
        IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
        try
            if Frequency <> Fundamental then
            begin     // Last solution was something other than fundamental
                Frequency := Fundamental;
                if not RetrieveSavedVoltages then
                    Exit;  {Get Saved fundamental frequency solution}
            end;
//     DefaultHourMult := DefaultDailyShapeObj.getmult(DynaVars.dblHour);
//     IF Load_Changed THEN Begin    //Added to update the current sources of all frequencies any time
            InitializeForHarmonics;  //the value of a load changes in a proportional way
//            Load_Changed:=FALSE;     // Added 05 dec 2013 - D. Montenegro
//     End;
            SolveSnap;
            MonitorClass.SampleAll;   // Store the fundamental frequency in the monitors
       { Get the list of Harmonic Frequencies to solve at}
            if DoAllHarmonics then
                CollectAllFrequencies(FrequencyList, NFreq)   // Allocates FrequencyList
            else
            begin
                Reallocmem(FrequencyList, Sizeof(FrequencyList^[1]) * HarmonicListSize);
                NFreq := HarmonicListSize;
                for i := 1 to NFreq do
                    FrequencyList^[i] := Fundamental * HarmonicList^[i];
            end;

            for i := 1 to NFreq do
            begin

                Frequency := FrequencyList^[i]; // forces rebuild of SystemY
                if Abs(Harmonic - 1.0) > EPSILON then
                begin    // Skip fundamental
//               DefaultHourMult := DefaultDailyShapeObj.getmult(DynaVars.dblHour);
                    SolveHarmTime;
                    MonitorClass.SampleAll;
                    EndOfTimeStepCleanup;
              // Storage devices are assumed to stay the same since there is no time variation in this mode  (Not necessarelly now)
                end;
            end; {FOR}
            Increment_time;
        finally
            MonitorClass.SaveAll;
            ReallocMem(FrequencyList, 0);
        end;
    end;

end;

end.
