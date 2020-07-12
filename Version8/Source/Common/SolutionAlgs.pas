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

function SolveMonte1(ActorID: Integer): Integer;   // Solve Monte Carlo Solution
function SolveMonte2(ActorID: Integer): Integer;   // Solve Monte Carlo Solution
function SolveMonte3(ActorID: Integer): Integer;   // Solve Monte Carlo Solution
function SolveMonteFault(ActorID: Integer): Integer;  // Solve Monte Carlo Fault Study
function SolveFaultStudy(ActorID: Integer): Integer;  // Full Fault Study
function SolveDaily(ActorID: Integer): Integer;    // Solve Following Daily Cycle
function SolvePeakDay(ActorID: Integer): Integer;   // Solve Following Daily Cycle at peak load
function SolveYearly(ActorID: Integer): Integer;   // Solve Following Yearly Cycle
function SolveDuty(ActorID: Integer): Integer;     // Solve Following Duty Cycle
function SolveDynamic(ActorID: Integer): Integer;  // Solve Dynamics
function SolveLD1(ActorID: Integer): Integer;      // solve Load-Duration Curve, 1
function SolveLD2(ActorID: Integer): Integer;      // solve Load-Duration Curve, 2
function SolveHarmonic(ActorID: Integer): Integer;
function SolveHarmonicT(ActorID: Integer): Integer;  // Sequential-Time Harmonics, Added 07-06-2015
function SolveHarmTime(ActorID: Integer): Integer;  // solve harmonics vs time (like general time mode) created by Davis Montenegro 25/06/2014
function SolveGeneralTime(ActorID: Integer): Integer;

procedure ComputeYsc(iB: Integer; ActorID: Integer);
procedure ComputeAllYsc(ActorID: Integer);
procedure IntegratePCStates(ActorID: Integer);
procedure EndOfTimeStepCleanup(ActorID: Integer);
procedure FinishTimeStep(ActorID: Integer);

implementation

uses
    ArrayDef,
    DSSGlobals,
{$IFNDEF FPC}
    DSSForms,
    System.Classes,
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
procedure FinishTimeStep(ActorID: Integer);
{
   Cample Cleanup and increment time

   For custom solutions.

}
begin
    MonitorClass[ActorID].SampleAll(ActorID);
    with ActiveCircuit[ActorID].Solution do
    begin
        if SampleTheMeters then
            EnergyMeterClass[ActorID].SampleAll(ActorID);   // Save Demand interval Files
        EndOfTimeStepCleanup(ActorID);
        Increment_time;
    end;
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure EndOfTimeStepCleanup(ActorID: Integer);
{
   Put stuff in this procedure that needs to happen at the end of the time step
   in main solution loops (see below)
}
begin
    StorageClass[ActorID].UpdateAll(ActorID);
//    Storage2Class[ActorID].UpdateAll(ActorID);
    InvControlClass[ActorID].UpdateAll(ActorID);
//    InvControl2Class[ActorID].UpdateAll(ActorID);
    ExpControlClass[ActorID].UpdateAll(ActorID);

    // End of Time Step Timer
    ActiveCircuit[ActorID].Solution.UpdateLoopTime;
    MonitorClass[ActorID].SampleAllMode5(ActorID);  // sample all mode 5 monitors to get timings
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure Show10PctProgress(i, N: Integer; ActorID: Integer);

begin
    if NoFormsAllowed then
        Exit;

    if ((i * 10) div N) > ProgressCount then
    begin
        Inc(ProgressCount);
//        ShowPctProgress( ProgressCount * 10, ActorID);
    end;
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveYearly(ActorID: Integer): Integer;

var
    N, Twopct: Integer;

begin
    Result := 0;
{ ProgressCaption( 'Solving Year '+ IntToStr(ActiveCircuit[ActorID].Solution.Year) + ' Actor: ' + IntToStr(ActorID) + ' CPU: ' + IntToStr(ActorCPU[ActorID]),ActorID);
 ActorProgressCount[ActorID] := 0;
 ShowPctProgress(ActorProgressCount[ActorID],ActorID);
}
    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
    begin
        try

            IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage elements
            if not DIFilesAreOpen[ActorID] then
                EnergyMeterClass[ActorID].OpenAllDIFiles(ActorID);   // Open Demand Interval Files, if desired   Creates DI_Totals
            Twopct := Max(NumberOfTimes div 50, 1);
            for N := 1 to NumberOfTimes do
                if not SolutionAbort then
                    with Dynavars do
                    begin
                        Increment_time;
                        DefaultHourMult := DefaultYearlyShapeObj.getmult(dblHour);
                        if PriceCurveObj <> NIL then
                            PriceSignal := PriceCurveObj.GetPrice(dblHour);
                        SolveSnap(ActorID);
                        MonitorClass[ActorID].SampleAll(ActorID);  // Make all monitors take a sample
                        if SampleTheMeters then
                            EnergyMeterClass[ActorID].SampleAll(ActorID); // Make all Energy Meters take a sample
                        EndOfTimeStepCleanup(ActorID);
                        ActorPctProgress[ActorID] := (N * 100) div NumberofTimes;
//          If (N mod Twopct)=0 Then ShowPctProgress((N*100) div NumberofTimes,ActorID);
                    end;
        finally
            MonitorClass[ActorID].SaveAll(ActorID);
//    EnergyMeterClass[ActorID].CloseAllDIFiles(ActorID);   // Save Demand interval Files    See DIFilesAreOpen Logic
        end;
    end;
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveDaily(ActorID: Integer): Integer;

{
  Solves following the daily load curve.
  Stepsize defaults to 1 hr and number of times = 24.
  Load is modified by yearly growth, time of day, and global load multiplier.
}

var
    N: Integer;

begin
    Result := 0;

    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
    begin
      // t:=0.0;
      // MonitorClass.ResetAll;
      // EnergyMeterClass.ResetAll;
        try

            IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters
            if not DIFilesAreOpen[ActorID] then
                EnergyMeterClass[ActorID].OpenAllDIFiles(ActorID);   // Append Demand Interval Files, if desired

            for N := 1 to NumberOfTimes do
                if not SolutionAbort then
                    with DynaVars do
                    begin
                        Increment_time;
                        DefaultHourMult := DefaultDailyShapeObj.getmult(dblHour);
                        if PriceCurveObj <> NIL then
                            PriceSignal := PriceCurveObj.GetPrice(dblHour);
                        SolveSnap(ActorID);
                        MonitorClass[ActorID].SampleAll(ActorID);  // Make all monitors take a sample
                        if SampleTheMeters then
                            EnergyMeterClass[ActorID].SampleAll(ActorID); // Make all Energy Meters take a sample

                        EndOfTimeStepCleanup(ActorID);
                        ActorPctProgress[ActorID] := (N * 100) div NumberofTimes;

                    end;

        finally
            MonitorClass[ActorID].SaveAll(ActorID);
            if SampleTheMeters then
                EnergyMeterClass[ActorID].CloseAllDIFiles(ActorID);   // Save Demand interval Files
        end; {Try}
    end;  {WITH}
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolvePeakDay(ActorID: Integer): Integer;

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
    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
    begin
        DynaVars.t := 0.0;

        // MonitorClass.ResetAll;
        // EnergyMeterClass.ResetAll;
        try
            DynaVars.intHour := 0;
            DynaVars.dblHour := 0.0;
            IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
            if not DIFilesAreOpen[ActorID] then
                EnergyMeterClass[ActorID].OpenAllDIFiles(ActorID);   // Open Demand Interval Files, if desired

            for N := 1 to NumberOfTimes do
                if not SolutionAbort then
                    with DynaVars do
                    begin
                        Increment_time;
                        DefaultHourMult := DefaultDailyShapeObj.GetMult(dblHour);
                        if PriceCurveObj <> NIL then
                            PriceSignal := PriceCurveObj.GetPrice(dblHour);
                        SolveSnap(ActorID);
                        MonitorClass[ActorID].SampleAll(ActorID);  // Make all monitors take a sample
                        if SampleTheMeters then
                            EnergyMeterClass[ActorID].SampleAll(ActorID); // Make all Energy Meters take a sample

                        EndOfTimeStepCleanup(ActorID);

                    end;
        finally
            MonitorClass[ActorID].SaveAll(ActorID);
            if SampleTheMeters then
                EnergyMeterClass[ActorID].CloseAllDIFiles(ActorID);   // Save Demand interval Files
        end;
    end;  {WITH}
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveDuty(ActorID: Integer): Integer;

var
    N, TwoPct, Temp0: Integer;
    Temp1: Boolean;

begin
    Result := 0;

{   ProgressCaption( 'Duty Cycle Solution', ActorID);
   ProgressCount := 0;
   ShowPctProgress(0, ActorID);       }

    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
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
                        ActiveCircuit[ActorID].Solution.Increment_time;
                        DefaultHourMult := DefaultDailyShapeObj.getmult(dblHour);
            // Assume pricesignal stays constant for dutycycle calcs
                        SolveSnap(ActorID);
                        MonitorClass[ActorID].SampleAll(ActorID);  // Make all monitors take a sample
                        if SampleTheMeters then
                            EnergyMeterClass[ActorID].SampleAll(ActorID); // Make all Energy Meters take a sample
                        EndOfTimeStepCleanup(ActorID);

                        ActorPctProgress[ActorID] := (N * 100) div NumberofTimes;
                        if SampleTheMeters then
                            EnergyMeterClass[ActorID].CloseAllDIFiles(ActorID);   // Save Demand interval Files
//            If (N mod Twopct)=0 Then ShowPctProgress((N*100) div NumberofTimes, ActorID);
                    end;
        finally
            MonitorClass[ActorID].SaveAll(ActorID);
//        ProgressHide(ActorID);
        end;
    end;
end;

function SolveGeneralTime(ActorID: Integer): Integer;

{
   For Rolling your own solution modes
}
var
    N: Integer;

begin
    Result := 0;

    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
    begin
        IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
        for N := 1 to NumberOfTimes do
            if not SolutionAbort then
                with DynaVars do
                begin
              {Compute basic multiplier from Default loadshape to use in generator dispatch, if any}
                    DefaultHourMult := DefaultDailyShapeObj.getmult(dblHour);

                    SolveSnap(ActorID);

                    FinishTimeStep(ActorID);
                    ActorPctProgress[ActorID] := (N * 100) div NumberofTimes;
                end;
    end;
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure IntegratePCStates(ActorID: Integer);
 {Integrate states in all PC Elements.  At present, only PC Elements
  can have dynamic states}

var
    pcelem: TPCElement;

begin
    with ActiveCircuit[ActorID] do
    begin
        pcelem := PCelements.First;
        while pcelem <> NIL do
        begin
            pcelem.IntegrateStates(ActorID);
            pcelem := PCelements.Next;
        end;
    end;
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveDynamic(ActorID: Integer): Integer;

var
    N: Integer;

begin
    Result := 0;

    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
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
                        IntegratePCStates(ActorID);
                        SolveSnap(ActorID);
       {Corrector}
                        IterationFlag := 1;
                        IntegratePCStates(ActorID);
                        SolveSnap(ActorID);
                        MonitorClass[ActorID].SampleAll(ActorID);  // Make all monitors take a sample
                        FmonitorClass[ActorID].update_sys_ld_info(ActorID);//get current value in INFO Broker -> UCF

          // attack and defense -> UCF
                        FmonitorClass[ActorID].update_atks(ActorID);
                        FmonitorClass[ActorID].update_defense_layer(ActorID);
          //-------------------------------
                        EndOfTimeStepCleanup(ActorID);

                    end;
        finally
            MonitorClass[ActorID].SaveAll(ActorID);
        end;
    end;
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveMonte1(ActorID: Integer): Integer;

var
    N: Integer;

begin
    Result := 0;

    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
    begin
        try
            LoadMultiplier := 1.0;   // Always set with prop in case matrix must be rebuilt
            IntervalHrs := 1.0;     // needed for energy meters and storage devices
            DynaVars.intHour := 0;
            DynaVars.dblHour := 0.0;// Use hour to denote Case number
            DynaVars.t := 0.0;

        // MonitorClass.ResetAll;
        // EnergyMeterClass.ResetAll;

{        ProgressCaption( 'Monte Carlo Mode 1, ' + IntToStr(NumberofTimes) + ' Random Loads.', ActorID);
        ProgressCount := 0;    }

            for N := 1 to NumberOfTimes do
                if not SolutionAbort then
                begin
                    Inc(DynaVars.intHour);
                    SolveSnap(ActorID);
                    MonitorClass[ActorID].SampleAll(ActorID);  // Make all monitors take a sample
                    if SampleTheMeters then
                        EnergyMeterClass[ActorID].SampleAll(ActorID);  // Make all meters take a sample
                    ActorPctProgress[ActorID] := (N * 100) div NumberofTimes;
//            Show10PctProgress(N, NumberOfTimes, ActorID);
                end
                else
                begin
                    ErrorNumber := SOLUTION_ABORT;
                    CmdResult := ErrorNumber;
                    GlobalResult := 'Solution Aborted';
                    Break;
                end;
        finally
            MonitorClass[ActorID].SaveAll(ActorID);
            if SampleTheMeters then
                EnergyMeterClass[ActorID].CloseAllDIFiles(ActorID);
//        ProgressHide(ActorID);
        end;
    end;

end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveMonte2(ActorID: Integer): Integer;

// Do a daily load solution for several Random days

var
    i, N, Ndaily: Integer;

begin
    Result := 0;

    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].solution do
    begin
        try
            DynaVars.t := 0.0;
            DynaVars.intHour := 0;
            DynaVars.dblHour := 0.0;
        // MonitorClass.ResetAll;
        // EnergyMeterClass.ResetAll;
            IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
            Ndaily := Round(24.0 / IntervalHrs);

            if not DIFilesAreOpen[ActorID] then
                EnergyMeterClass[ActorID].OpenAllDIFiles(ActorID);   // Open Demand Interval Files, if desired

{        ProgressCaption('Monte Carlo Mode 2, ' + IntToStr(NumberofTimes) + ' Days.', ActorID);
        ProgressCount := 0;       }

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
                            SolveSnap(ActorID);

                            MonitorClass[ActorID].SampleAll(ActorID);  // Make all monitors take a sample
                            if SampleTheMeters then
                                EnergyMeterClass[ActorID].SampleAll(ActorID);  // Make all meters take a sample

                            EndOfTimeStepCleanup(ActorID);

                        end;

                    ActorPctProgress[ActorID] := (N * 100) div NumberofTimes;
//          Show10PctProgress(N, NumberOfTimes, ActorID);

                end
                else
                begin
                    ErrorNumber := SOLUTION_ABORT;
                    CmdResult := ErrorNumber;
                    GlobalResult := 'Solution Aborted.';
                    Break;
                end;
        finally
            MonitorClass[ActorID].SaveAll(ActorID);
            if SampleTheMeters then
                EnergyMeterClass[ActorID].CloseAllDIFiles(ActorID);   // Save Demand interval Files
//        ProgressHide(ActorID);
        end;
    end;
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveMonte3(ActorID: Integer): Integer;

// Hold time fixed and just vary the global load multiplier

var
    N: Integer;

begin
    Result := 0;

    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
    begin
    // Time must be set beFore entering this routine
        try
        // MonitorClass.ResetAll;
        // EnergyMeterClass.ResetAll;
            IntervalHrs := 1.0;  // just get per unit energy and multiply result as necessary

            if not DIFilesAreOpen[ActorID] then
                EnergyMeterClass[ActorID].OpenAllDIFiles(ActorID);   // Open Demand Interval Files, if desired

{        ProgressCaption( 'Monte Carlo Mode 3, ' + IntToStr(NumberofTimes) + ' Different Load Levels.', ActorID);
        ProgressCount := 0;    }

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

                    SolveSnap(ActorID);

                    MonitorClass[ActorID].SampleAll(ActorID);  // Make all monitors take a sample
                    if SampleTheMeters then
                        EnergyMeterClass[ActorID].SampleAll(ActorID);  // Make all meters take a sample

                    ActorPctProgress[ActorID] := (N * 100) div NumberofTimes;
//            Show10PctProgress(N, NumberOfTimes, ActorID);
                end
                else
                begin
                    CmdResult := SOLUTION_ABORT;
                    ErrorNumber := CmdResult;
                    GlobalResult := 'Solution Aborted';
                    Break;
                end;
        finally
            MonitorClass[ActorID].SaveAll(ActorID);
            if SampleTheMeters then
                EnergyMeterClass[ActorID].CloseAllDIFiles(ActorID);   // Save Demand interval Files
//        ProgressHide(ActorID);
        end;
    end; {WITH}
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveLD1(ActorID: Integer): Integer;

// Do a Daily Simulation based on a load duration curve

var
    N, Ndaily, i: Integer;


begin
    Result := 0;

    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
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

            if not DIFilesAreOpen[ActorID] then
                EnergyMeterClass[ActorID].OpenAllDIFiles(ActorID);   // Open Demand Interval Files, if desired

//      ProgressCaption( 'Load-Duration Mode 1 Solution. ', ActorID);

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

                            SolveSnap(ActorID);

                            MonitorClass[ActorID].SampleAll(ActorID);     // Make all monitors take a sample
                            if SampleTheMeters then
                                EnergyMeterClass[ActorID].SampleAll(ActorID);  // Make all meters take a sample

                            EndOfTimeStepCleanup(ActorID);


                        end;

                        ActorPctProgress[ActorID] := (i * 100) div NDaily;
//           ShowPctProgress((i * 100) div NDaily, ActorID);
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
            MonitorClass[ActorID].SaveAll(ActorID);
            if SampleTheMeters then
                EnergyMeterClass[ActorID].CloseAllDIFiles(ActorID);   // Save Demand interval Files
//      ProgressHide(ActorID);
        end;
    end; {WITH ActiveCircuit[ActiveActor]}

end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveLD2(ActorID: Integer): Integer;

// Hold time fixed and just vary the global load multiplier according to the global
// Load-Duration Curve

var
    N: Integer;

begin
    Result := 0;

    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
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
        if not DIFilesAreOpen[ActorID] then
            EnergyMeterClass[ActorID].OpenAllDIFiles(ActorID);   // Open Demand Interval Files, if desired

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

                SolveSnap(ActorID);

                MonitorClass[ActorID].SampleAll(ActorID);  // Make all monitors take a sample
                if SampleTheMeters then
                    EnergyMeterClass[ActorID].SampleAll(ActorID);  // Make all meters take a sample

                EndOfTimeStepCleanup(ActorID);

            end;
        finally
            MonitorClass[ActorID].SaveAll(ActorID);
            if SampleTheMeters then
                EnergyMeterClass[ActorID].CloseAllDIFiles(ActorID);   // Save Demand interval Files
        end;
    end; {WITH ActiveCircuit[ActiveActor]}

end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure PickAFault(ActorID: Integer);
// Enable one of the faults in the circuit.  Disable the rest
var
    NumFaults, i, Whichone: Integer;
    FaultObj: TFaultObj;
begin
    NumFaults := ActiveCircuit[ActorID].Faults.Listsize;
    Whichone := Trunc(Random * NumFaults) + 1;
    if Whichone > NumFaults then
        Whichone := NumFaults;

    for i := 1 to NumFaults do
    begin
        FaultObj := ActiveCircuit[ActorID].Faults.Get(i);
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
function SolveMonteFault(ActorID: Integer): Integer;

var
    N: Integer;

begin
    Result := 0;

    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
    begin
        try
            LoadModel := ADMITTANCE;   // All Direct solution
            LoadMultiplier := 1.0;    // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
            DynaVars.intHour := 0;
            DynaVars.dblHour := 0.0; // Use hour to denote Case number
            DynaVars.t := 0.0;


      // MonitorClass.ResetAll;

{      ProgressCaption( 'Monte Carlo Fault Study: ' + IntToStr(NumberofTimes) + ' Different Faults.', ActorID);
      ProgressCount := 0;            }

            SetGeneratorDispRef(ActorID);

            for N := 1 to NumberOfTimes do
                if not SolutionAbort then
                begin
                    Inc(DynaVars.intHour);
                    PickAFault(ActorID);  // Randomly enable one of the faults
                    ActiveFaultObj.Randomize(ActorID);  // Randomize the fault resistance
                    SolveDirect(ActorID);
                    MonitorClass[ActorID].SampleAll(ActorID);  // Make all monitors take a sample

                    ActorPctProgress[ActorID] := (N * 100) div NumberOfTimes;
//          Show10PctProgress(N, NumberOfTimes, ActorID);
                end;
        finally
            MonitorClass[ActorID].SaveAll(ActorID);
//      ProgressHide(ActorID);
        end;
    end;

end;

{--------------------------------------------------------------------------}
procedure AllocateAllSCParms(ActorID: Integer);
var
    i: Integer;
begin
    with ActiveCircuit[ActorID] do
    begin
        for i := 1 to NumBuses do
            Buses^[i].AllocateBusQuantities;
    end;
end;


{--------------------------------------------------------------------------}
procedure ComputeIsc(ActorID: Integer);
{ Compute Isc at all buses for current values of Voc and Ysc }
var
    i: Integer;
begin
    with ActiveCircuit[ActorID] do
    begin
        for i := 1 to NumBuses do
            with Buses^[i] do
            begin
                Ysc.MVMult(BusCurrent, VBus);
            end;
    end;
end;


{--------------------------------------------------------------------------}
procedure ComputeYsc(iB: Integer; ActorID: Integer);

{Compute YSC for I-th bus}
{Assume InjCurr is zeroed}

var
    i,
    j,
    ref1: Integer;

begin
    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
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
procedure ComputeAllYsc(ActorID: Integer);
var
    iB, j: Integer;


begin

    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
    begin

        for j := 1 to NumNodes do
            Currents^[j] := cZERO;

        ActorProgressCount[ActorID] := 0;

        for iB := 1 to NumBuses do
        begin
            ComputeYsc(iB, ActorID);  // Compute YSC for iB-th Bus
            if ((iB * 10) div NumBuses) > ProgressCount then
            begin
                Inc(ActorProgressCount[ActorID]);
//            ShowPctProgress(30 + ActorProgressCount[ActorID] * 5, ActorID);
            end;
        end;
    end;
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure DisableAllFaults(ActorID: Integer);
begin
    with ActiveCircuit[ActorID] do
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
function SolveFaultStudy(ActorID: Integer): Integer;


begin
    Result := 0;
    ActorPctProgress[ActorID] := 0;
//   ShowPctProgress( 0, ActorID);
//   ProgressCaption( 'Computing Open-Circuit Voltages', ActorID);

    with ActiveCircuit[ActorID].solution do
    begin
        LoadModel := ADMITTANCE;
        DisableAllFaults(ActorID);

        SolveDirect(ActorID);   // This gets the open circuit voltages and bus lists corrected

        AllocateAllSCParms(ActorID);   // Reallocate bus quantities
        UpdateVBus(ActorID);  // Put present solution Voc's in bus quantities
    end;

    ActorPctProgress[ActorID] := 30;
  {   ProgressCaption ('Computing Ysc Matrices for Each Bus', ActorID);
     ShowPctProgress (30, ActorID);}
    ComputeAllYsc(ActorID);

    ActorPctProgress[ActorID] := 80;
  {   ProgressCaption( 'Computing Short-circuit currents.', ActorID);
     ShowPctProgress (80, ActorID);}
    ComputeIsc(ActorID);

    ActorPctProgress[ActorID] := 100;
  {   ShowPctProgress ( 100, ActorID);
     ProgressCaption ('Done.', ActorID);}
  //   ProgressHide(ActorID);
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
procedure CollectAllFrequencies(var FreqList: pDoubleArray; var NumFreq: Integer; ActorID: Integer);

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

    with ActiveCircuit[ActorID] do
    begin
        {Check Sources -- each could have a different base frequency}
        p := Sources.First;
        while p <> NIL do
        begin
            if p.Enabled then
                if SpectrumClass[ActorID].Find(p.Spectrum) <> NIL then
                begin
                    pSpectrum := SpectrumClass[ActorID].GetActiveObj;
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
    SpectrumInUse := AllocMem(SizeOf(SpectruminUse^[1]) * SpectrumClass[ActorID].ElementCount);  //Allocate and zero
    with ActiveCircuit[ActorID] do
    begin
        p := PCelements.First;
        while p <> NIL do
        begin
            if p.enabled then
                if SpectrumClass[ActorID].Find(p.Spectrum) <> NIL then
                begin
                    SpectrumInUse^[SpectrumClass[ActorID].Active] := 1;
                end;
            p := PCelements.Next;
        end;
    end; {With}

    {Add marked Spectra to list}
    for i := 1 to SpectrumClass[ActorID].ElementCount do
    begin
        if SpectrumInUse^[i] = 1 then
        begin
            SpectrumClass[ActorID].Active := i;
            pSpectrum := SpectrumClass[ActorID].GetActiveObj;
            for j := 1 to pSpectrum.NumHarm do
            begin
                AddFrequency(FreqList, NumFreq, MaxFreq, pSpectrum.HarmArray^[j] * ActiveCircuit[ActorID].Fundamental);
            end;
        end;
    end;

    ReallocMem(SpectrumInUse, 0);


end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function SolveHarmonic(ActorID: Integer): Integer;

var
    FrequencyList: pDoubleArray;
    i, NFreq: Integer;

begin
    Result := 0;

    FrequencyList := NIL;   // Set up for Reallocmem
{   ShowPctProgress ( 0, ActorID);
   ProgressCaption( 'Performing Harmonic Solution', ActorID);    }

    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].solution do
    begin
        try

            if Frequency <> Fundamental then
            begin     // Last solution was something other than fundamental
                Frequency := Fundamental;
                if not RetrieveSavedVoltages then
                    Exit;  {Get Saved fundamental frequency solution}
            end;

            MonitorClass[ActorID].SampleAll(ActorID);   // Store the fundamental frequency in the monitors

       { Get the list of Harmonic Frequencies to solve at}
            if DoAllHarmonics then
                CollectAllFrequencies(FrequencyList, NFreq, ActorID)   // Allocates FrequencyList
            else
            begin
                Reallocmem(FrequencyList, Sizeof(FrequencyList^[1]) * HarmonicListSize);
                NFreq := HarmonicListSize;
                for i := 1 to NFreq do
                    FrequencyList^[i] := Fundamental * HarmonicList^[i];
            end;

            for i := 1 to NFreq do
            begin

                Frequency := FrequencyList^[i];
                if Abs(Harmonic - 1.0) > EPSILON then
                begin    // Skip fundamental
{               ProgressCaption ( 'Solving at Frequency = ' + Format('%-g', [Frequency]), ActorID);
               ShowPctProgress ( Round((100.0*i)/Nfreq), ActorID);}
                    ActorPctProgress[ActorID] := Round((100.0 * i) / Nfreq);
                    SolveDirect(ActorID);
                    MonitorClass[ActorID].SampleAll(ActorID);
               // Storage devices are assumed to stay the same since there is no time variation in this mode
                end;

            end; {FOR}

{       ShowPctProgress ( 100, ActorID);
       ProgressCaption ( 'Done.',ActorID);    }
        finally
//       ProgressHide(ActorID);
            MonitorClass[ActorID].SaveAll(ActorID);
            ReallocMem(FrequencyList, 0);
        end;
     // Now should have all we need to make a short circuit report

    end;

end;
//========================================================================================
function SolveHarmTime(ActorID: Integer): Integer;     // It is based in SolveGeneralTime routine

begin
    Result := 0;

    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
    begin
        IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
        if not SolutionAbort then
            with DynaVars do
            begin
              {Compute basic multiplier from Default loadshape to use in generator dispatch, if any}
                DefaultHourMult := DefaultDailyShapeObj.getmult(dblHour);

                SolveSnap(ActorID);
          //      Increment_time;  // This function is handeled from SolveHarmonics (04-10-2013)
            end;
    end;
end;
//=============================================================================
function SolveHarmonicT(ActorID: Integer): Integer;
var
    FrequencyList: pDoubleArray;
    i, NFreq: Integer;

begin
    Result := 0;

    FrequencyList := NIL;   // Set up for Reallocmem

    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].solution do
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
            InitializeForHarmonics(ActorID);  //the value of a load changes in a proportional way
//            Load_Changed:=FALSE;     // Added 05 dec 2013 - D. Montenegro
//     End;
            SolveSnap(ActorID);
            MonitorClass[ActorID].SampleAll(ActorID);   // Store the fundamental frequency in the monitors
       { Get the list of Harmonic Frequencies to solve at}
            if DoAllHarmonics then
                CollectAllFrequencies(FrequencyList, NFreq, ActorID)   // Allocates FrequencyList
            else
            begin
                Reallocmem(FrequencyList, Sizeof(FrequencyList^[1]) * HarmonicListSize);
                NFreq := HarmonicListSize;
                for i := 1 to NFreq do
                    FrequencyList^[i] := Fundamental * HarmonicList^[i];
            end;

            for i := 1 to NFreq do
            begin

                Frequency := FrequencyList^[i];
                if Abs(Harmonic - 1.0) > EPSILON then
                begin    // Skip fundamental
//               DefaultHourMult := DefaultDailyShapeObj.getmult(DynaVars.dblHour);
                    SolveHarmTime(ActorID);
                    MonitorClass[ActorID].SampleAll(ActorID);
                    EndOfTimeStepCleanup(ActorID);
              // Storage devices are assumed to stay the same since there is no time variation in this mode  (Not necessarelly now)
                end;
            end; {FOR}
            Increment_time;
        finally
            MonitorClass[ActorID].SaveAll(ActorID);
            ReallocMem(FrequencyList, 0);
        end;
    end;

end;

initialization

    IsMultiThread := TRUE;

end.
