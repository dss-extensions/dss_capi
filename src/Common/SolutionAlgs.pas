unit SolutionAlgs;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

// Solution Algorithms

interface

uses DSSClass, ArrayDef, PCElement, Solution, Circuit, Bus;

type
    TSolutionAlgs = class helper for TSolutionObj
    private
        function get_ckt(): TDSSCircuit; inline;
    public
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

        procedure ComputeYsc(pBus: TDSSBus);
        procedure ComputeAllYsc;
        procedure IntegratePCStates;
        procedure EndOfTimeStepCleanup();
        procedure FinishTimeStep;
        property ckt: TDSSCircuit read get_ckt;
    private
        procedure Show10PctProgress(i, N: Integer);
        procedure PickAFault;
        procedure AllocateAllSCParms;
        procedure ComputeIsc;
        procedure DisableAllFaults;
        procedure AddFrequency(var FreqList: ArrayOfDouble; var NumFreq, MaxFreq: Integer; F: Double);
        function GetSourceFrequency(pc: TPCElement): Double;
        procedure CollectAllFrequencies(var FreqList: ArrayOfDouble);
    end;

implementation

uses
    DSSGlobals,
    Utilities,
    SysUtils,
    MathUtil,
    Math,
    Fault,
    UComplex, DSSUcomplex,
    YMatrix,
    Spectrum,
    Vsource,
    Isource,
    KLUSolve,
    DSSHelper;

function TSolutionAlgs.get_ckt(): TDSSCircuit;
begin
    Result := TDSSCircuit(cktptr);
end;
procedure TSolutionAlgs.FinishTimeStep;
// Sample Cleanup and increment time
// For custom solutions.
begin
    DSS.MonitorClass.SampleAll();
    if SampleTheMeters then
        DSS.EnergyMeterClass.SampleAll();   // Save Demand interval Files

    EndOfTimeStepCleanup();
    IncrementTime();
end;

procedure TSolutionAlgs.EndOfTimeStepCleanup();
// Put stuff in this procedure that needs to happen at the end of the time step
// in main solution loops (see below)
begin
    DSS.StorageClass.UpdateAll();
    DSS.InvControlClass.UpdateAll();
    DSS.ExpControlClass.UpdateAll();

    // End of Time Step Timer
    ckt.Solution.UpdateLoopTime;
    DSS.MonitorClass.SampleAllMode5;  // sample all mode 5 monitors to get timings
end;

procedure TSolutionAlgs.Show10PctProgress(i, N: Integer);

begin
    if NoFormsAllowed then
        Exit;

    if ((i * 10) div N) > ProgressCount then
    begin
        Inc(ProgressCount);
        DSS.ShowPctProgress(ProgressCount * 10);
    end;
end;

function TSolutionAlgs.SolveYearly: Integer;
var
    N, Twopct: Integer;
begin
    Result := 0;
    ProgressCount := 0;
{$IFDEF DSS_CAPI_PM}
    DSS.ActorPctProgress := 0;
{$ELSE}
    DSS.ProgressCaption('Solving Year ' + IntToStr(ckt.Solution.Year));
    DSS.ShowPctProgress(0);
{$ENDIF}

    try
        IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage elements
        if not DSS.DIFilesAreOpen then
            DSS.EnergyMeterClass.OpenAllDIFiles();   // Open Demand Interval Files, if desired   Creates DI_Totals
        Twopct := Max(NumberOfTimes div 50, 1);
        for N := 1 to NumberOfTimes do
            if not DSS.SolutionAbort then
                with Dynavars do
                begin
                    IncrementTime();
                    ckt.DefaultHourMult := ckt.DefaultYearlyShapeObj.GetMultAtHour(dblHour);
                    if ckt.PriceCurveObj <> NIL then
                        ckt.PriceSignal := ckt.PriceCurveObj.GetPrice(dblHour);
                    SolveSnap();
                    DSS.MonitorClass.SampleAll();  // Make all monitors take a sample
                    if SampleTheMeters then
                        DSS.EnergyMeterClass.SampleAll(); // Make all Energy Meters take a sample

                    EndOfTimeStepCleanup();
{$IFDEF DSS_CAPI_PM}
                    DSS.ActorPctProgress := (N * 100) div NumberofTimes;
{$ELSE}
                    if (N mod Twopct) = 0 then
                        DSS.ShowPctProgress((N * 100) div NumberofTimes);
{$ENDIF}
                end;
    finally
{$IFNDEF DSS_CAPI_PM}
        DSS.ProgressHide();
{$ENDIF}
        DSS.MonitorClass.SaveAll();
        // DSS.EnergyMeterClass.CloseAllDIFiles;   // Save Demand interval Files    See DIFilesAreOpen Logic
    end;
end;

function TSolutionAlgs.SolveDaily: Integer;
// Solves following the daily load curve.
// Stepsize defaults to 1 hr and number of times = 24.
// Load is modified by yearly growth, time of day, and global load multiplier.
var
    N: Integer;
begin
    Result := 0;

    // t:=0.0;
    // DSS.MonitorClass.ResetAll;
    // DSS.EnergyMeterClass.ResetAll;
    try

        IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters
        if not DSS.DIFilesAreOpen then
            DSS.EnergyMeterClass.OpenAllDIFiles();   // Append Demand Interval Files, if desired

        for N := 1 to NumberOfTimes do
            if not DSS.SolutionAbort then
                with DynaVars do
                begin
                    IncrementTime();
                    ckt.DefaultHourMult := ckt.DefaultDailyShapeObj.GetMultAtHour(dblHour);
                    if ckt.PriceCurveObj <> NIL then
                        ckt.PriceSignal := ckt.PriceCurveObj.GetPrice(dblHour);
                    SolveSnap();
                    DSS.MonitorClass.SampleAll();  // Make all monitors take a sample
                    if SampleTheMeters then
                        DSS.EnergyMeterClass.SampleAll(); // Make all Energy Meters take a sample

                    EndOfTimeStepCleanup();
{$IFDEF DSS_CAPI_PM}
                    DSS.ActorPctProgress := (N * 100) div NumberofTimes;
{$ENDIF}
                end;

    finally
        DSS.MonitorClass.SaveAll();
        if SampleTheMeters then
            DSS.EnergyMeterClass.CloseAllDIFiles();   // Save Demand interval Files
    end; // Try
end;

function TSolutionAlgs.SolvePeakDay: Integer;
//  Solves peak day
// 
//     Takes the given load kW and assumes it represents the peak value.
//     Load is modified by daily load curve and growth factor for the year.
//     'h' defaults to 3600 (1 hr) but can be reset to anything.
//     Differs from Daily mode in that the global load multiplier is ignored.
var
    N: Integer;
begin
    Result := 0;
    DynaVars.t := 0.0;

    // DSS.MonitorClass.ResetAll;
    // DSS.EnergyMeterClass.ResetAll;
    try
        DynaVars.intHour := 0;
        DynaVars.dblHour := 0.0;
        IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
        if not DSS.DIFilesAreOpen then
            DSS.EnergyMeterClass.OpenAllDIFiles();   // Open Demand Interval Files, if desired

        for N := 1 to NumberOfTimes do
            if not DSS.SolutionAbort then
                with DynaVars do
                begin
                    IncrementTime();
                    ckt.DefaultHourMult := ckt.DefaultDailyShapeObj.GetMultAtHour(dblHour);
                    if ckt.PriceCurveObj <> NIL then
                        ckt.PriceSignal := ckt.PriceCurveObj.GetPrice(dblHour);
                    SolveSnap();
                    DSS.MonitorClass.SampleAll();  // Make all monitors take a sample
                    if SampleTheMeters then
                        DSS.EnergyMeterClass.SampleAll(); // Make all Energy Meters take a sample

                    EndOfTimeStepCleanup();

                end;
    finally
        DSS.MonitorClass.SaveAll();
        if SampleTheMeters then
            DSS.EnergyMeterClass.CloseAllDIFiles();   // Save Demand interval Files
    end;
end;

function TSolutionAlgs.SolveDuty: Integer;
var
    N, TwoPct: Integer;
begin
    Result := 0;

    ProgressCount := 0;
{$IFDEF DSS_CAPI_PM}
    DSS.ActorPctProgress := 0;
{$ELSE}
    DSS.ProgressCaption('Duty Cycle Solution');
    DSS.ShowPctProgress(0);
{$ENDIF}

    //   t:=0.0;
    // DSS.MonitorClass.ResetAll;
    TwoPct := Max(1, NumberOfTimes div 50);
    try
        IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
        for N := 1 to NumberOfTimes do
            if not DSS.SolutionAbort then
                with DynaVars do
                begin
                    IncrementTime();
                    ckt.DefaultHourMult := ckt.DefaultDailyShapeObj.GetMultAtHour(dblHour);
                    // Assume pricesignal stays constant for dutycycle calcs
                    SolveSnap();
                    DSS.MonitorClass.SampleAll();  // Make all monitors take a sample
                    if SampleTheMeters then
                        DSS.EnergyMeterClass.SampleAll(); // Make all Energy Meters take a sample

                    EndOfTimeStepCleanup();
{$IFDEF DSS_CAPI_PM}
                    DSS.ActorPctProgress := (N * 100) div NumberofTimes;
{$ELSE}
                    if (N mod Twopct) = 0 then
                        DSS.ShowPctProgress((N * 100) div NumberofTimes);
{$ENDIF}
                end;
    finally
        DSS.MonitorClass.SaveAll();
        if SampleTheMeters then
            DSS.EnergyMeterClass.CloseAllDIFiles();   // Save Demand interval Files
{$IFNDEF DSS_CAPI_PM}
        DSS.ProgressHide();
{$ENDIF}
    end;
end;

function TSolutionAlgs.SolveGeneralTime: Integer;
// For Rolling your own solution modes
var
    N: Integer;
begin
    Result := 0;
    IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
    for N := 1 to NumberOfTimes do
        if not DSS.SolutionAbort then
            with DynaVars do
            begin
                // Compute basic multiplier from Default loadshape to use in generator dispatch, if any
                ckt.DefaultHourMult := ckt.DefaultDailyShapeObj.GetMultAtHour(dblHour);

                SolveSnap();

                FinishTimeStep();
{$IFDEF DSS_CAPI_PM}
                DSS.ActorPctProgress := (N * 100) div NumberofTimes;
{$ENDIF}
            end;
end;

procedure TSolutionAlgs.IntegratePCStates;
// Integrate states in all PC Elements.  At present, only PC Elements
// can have dynamic states
var
    pcelem: TPCElement;
begin
    for pcelem in ckt.PCelements do
    begin
        pcelem.IntegrateStates;
    end;
end;

function TSolutionAlgs.SolveDynamic: Integer;
var
    N: Integer;
begin
    Result := 0;

    try
        SolutionInitialized := TRUE; // If we're in dynamics mode, no need to re-initialize.
        IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
        for N := 1 to NumberOfTimes do
            if not DSS.SolutionAbort then
                with DynaVars do
                begin
                    IncrementTime();
                    ckt.DefaultHourMult := ckt.DefaultDailyShapeObj.GetMultAtHour(dblHour);
                    // Assume price signal stays constant for dynamic calcs
                    // Predictor
                    IterationFlag := 0;
                    IntegratePCStates();
                    SolveSnap();
                    // Corrector
                    IterationFlag := 1;
                    IntegratePCStates();
                    SolveSnap();
                    DSS.MonitorClass.SampleAll();  // Make all monitors take a sample

                    EndOfTimeStepCleanup();

                end;
    finally
        DSS.MonitorClass.SaveAll();
    end;
end;

function TSolutionAlgs.SolveMonte1: Integer;
var
    N: Integer;
begin
    Result := 0;
    try
        ckt.LoadMultiplier := 1.0;   // Always set with prop in case matrix must be rebuilt
        IntervalHrs := 1.0;     // needed for energy meters and storage devices
        DynaVars.intHour := 0;
        DynaVars.dblHour := 0.0;// Use hour to denote Case number
        DynaVars.t := 0.0;

    // DSS.MonitorClass.ResetAll;
    // DSS.EnergyMeterClass.ResetAll;

{$IFDEF DSS_CAPI_PM}
{$ELSE}
        DSS.ProgressCaption('Monte Carlo Mode 1, ' + IntToStr(NumberofTimes) + ' Random Loads.');
{$ENDIF}
        ProgressCount := 0;

        for N := 1 to NumberOfTimes do
            if not DSS.SolutionAbort then
            begin
                Inc(DynaVars.intHour);
                SolveSnap();
                DSS.MonitorClass.SampleAll();  // Make all monitors take a sample
                if SampleTheMeters then
                    DSS.EnergyMeterClass.SampleAll();  // Make all meters take a sample
{$IFDEF DSS_CAPI_PM}
                    DSS.ActorPctProgress := (N * 100) div NumberofTimes;
{$ELSE}
                    Show10PctProgress(N, NumberOfTimes);
{$ENDIF}
            end
            else
            begin
                DSS.ErrorNumber := SOLUTION_ABORT;
                DSS.GlobalResult := 'Solution Aborted';
                Break;
            end;
    finally
        DSS.MonitorClass.SaveAll();
        if SampleTheMeters then
            DSS.EnergyMeterClass.CloseAllDIFiles();
{$IFNDEF DSS_CAPI_PM}
        DSS.ProgressHide();
{$ENDIF}
    end;
end;

function TSolutionAlgs.SolveMonte2: Integer;
// Do a daily load solution for several Random days
var
    i, N, Ndaily: Integer;
begin
    Result := 0;

    try
        DynaVars.t := 0.0;
        DynaVars.intHour := 0;
        DynaVars.dblHour := 0.0;
        // DSS.MonitorClass.ResetAll;
        // DSS.EnergyMeterClass.ResetAll;
        IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
        Ndaily := Round(24.0 / IntervalHrs);

        if not DSS.DIFilesAreOpen then
            DSS.EnergyMeterClass.OpenAllDIFiles();   // Open Demand Interval Files, if desired

{$IFDEF DSS_CAPI_PM}
{$ELSE}
        DSS.ProgressCaption('Monte Carlo Mode 2, ' + IntToStr(NumberofTimes) + ' Days.');
{$ENDIF}
        ProgressCount := 0;

        for N := 1 to NumberOfTimes do // Number of Days

            if not DSS.SolutionAbort then
            begin
                // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
                case Randomtype of
                    UNIFORM:
                        ckt.LoadMultiplier := Random();  // number between 0 and 1
                    GAUSSIAN:
                        ckt.LoadMultiplier := Gauss(ckt.DefaultDailyShapeObj.Mean, ckt.DefaultDailyShapeObj.StdDev);
                end;

                with DynaVars do
                    for i := 1 to Ndaily do
                    begin
                        IncrementTime();
                        ckt.DefaultHourMult := ckt.DefaultDailyShapeObj.GetMultAtHour(dblHour);
                        SolveSnap();

                        DSS.MonitorClass.SampleAll();  // Make all monitors take a sample
                        if SampleTheMeters then
                            DSS.EnergyMeterClass.SampleAll(); // Make all meters take a sample

                        EndOfTimeStepCleanup();

                    end;
{$IFDEF DSS_CAPI_PM}
                DSS.ActorPctProgress := (N * 100) div NumberofTimes;
{$ELSE}
                Show10PctProgress(N, NumberOfTimes);
{$ENDIF}
            end
            else
            begin
                DSS.ErrorNumber := SOLUTION_ABORT;
                DSS.GlobalResult := 'Solution Aborted.';
                Break;
            end;
    finally
        DSS.MonitorClass.SaveAll();
        if SampleTheMeters then
            DSS.EnergyMeterClass.CloseAllDIFiles();   // Save Demand interval Files
{$IFNDEF DSS_CAPI_PM}
        DSS.ProgressHide();
{$ENDIF}
    end;
end;

function TSolutionAlgs.SolveMonte3: Integer;
// Hold time fixed and just vary the global load multiplier
var
    N: Integer;
begin
    Result := 0;
    // Time must be set beFore entering this routine
    try
        // DSS.MonitorClass.ResetAll;
        // DSS.EnergyMeterClass.ResetAll;
        IntervalHrs := 1.0;  // just get per unit energy and multiply result as necessary

        if not DSS.DIFilesAreOpen then
            DSS.EnergyMeterClass.OpenAllDIFiles();   // Open Demand Interval Files, if desired

{$IFDEF DSS_CAPI_PM}
{$ELSE}
        DSS.ProgressCaption('Monte Carlo Mode 3, ' + IntToStr(NumberofTimes) + ' Different Load Levels.');
{$ENDIF}
        ProgressCount := 0;

        ckt.DefaultHourMult := ckt.DefaultDailyShapeObj.GetMultAtHour(DynaVars.dblHour);
        if ckt.PriceCurveObj <> NIL then
            ckt.PriceSignal := ckt.PriceCurveObj.GetPrice(DynaVars.dblHour);

        for N := 1 to NumberOfTimes do
            if not DSS.SolutionAbort then
            begin
                // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
                case Randomtype of
                    UNIFORM:
                        ckt.LoadMultiplier := Random();  // number between 0 and 1
                    GAUSSIAN:
                        ckt.LoadMultiplier := Gauss(ckt.DefaultDailyShapeObj.Mean, ckt.DefaultDailyShapeObj.StdDev);
                    LOGNORMAL:
                        ckt.LoadMultiplier := QuasiLognormal(ckt.DefaultDailyShapeObj.Mean);
                end;

                SolveSnap();

                DSS.MonitorClass.SampleAll();  // Make all monitors take a sample
                if SampleTheMeters then
                    DSS.EnergyMeterClass.SampleAll();  // Make all meters take a sample

{$IFDEF DSS_CAPI_PM}
                DSS.ActorPctProgress := (N * 100) div NumberofTimes;
{$ELSE}
                Show10PctProgress(N, NumberOfTimes);
{$ENDIF}
            end
            else
            begin
                DSS.ErrorNumber := SOLUTION_ABORT;
                DSS.GlobalResult := 'Solution Aborted';
                Break;
            end;
    finally
        DSS.MonitorClass.SaveAll();
        if SampleTheMeters then
            DSS.EnergyMeterClass.CloseAllDIFiles();   // Save Demand interval Files
{$IFNDEF DSS_CAPI_PM}
        DSS.ProgressHide();
{$ENDIF}
    end;
end;

function TSolutionAlgs.SolveLD1: Integer;
// Do a Daily Simulation based on a load duration curve
var
    N, Ndaily, i: Integer;
begin
    Result := 0;

    try
        if ckt.LoadDurCurveObj = NIL then
        begin
            DoSimpleMsg(DSS, _('Load Duration Curve Not Defined (Set LDCurve=... command). Cannot perForm solution.'), 470);
            Exit;
        end;

        // Time must be set beFore entering this routine

        // DSS.MonitorClass.ResetAll;
        // DSS.EnergyMeterClass.ResetAll;

        NDaily := Round(24.0 / DynaVars.h * 3600.0);

        if not DSS.DIFilesAreOpen then
            DSS.EnergyMeterClass.OpenAllDIFiles;   // Open Demand Interval Files, if desired

{$IFDEF DSS_CAPI_PM}
{$ELSE}
        DSS.ProgressCaption('Load-Duration Mode 1 Solution. ');
{$ENDIF}
        // (set in Solve method) DefaultGrowthFactor :=  IntPower(DefaultGrowthRate, (Year-1));

        DynaVars.intHour := 0;
        with DynaVars do
            for i := 1 to Ndaily do
            begin
                // Set the time
                IncrementTime();

                ckt.DefaultHourMult := ckt.DefaultDailyShapeObj.GetMultAtHour(dblHour);

                if not DSS.SolutionAbort then
                begin
                    for N := 1 to ckt.LoadDurCurveObj.NumPoints do
                    begin
                        ckt.LoadMultiplier := ckt.LoadDurCurveObj.Mult(N);  // Always set LoadMultiplier with prop in case matrix must be rebuilt
                        // Adjust meter interval to interval on value of present Load-Duration Curve
                        IntervalHrs := ckt.LoadDurCurveObj.PresentInterval;

                        // Price curve must correspond to load-duration curve
                        if ckt.PriceCurveObj <> NIL then
                            ckt.PriceSignal := ckt.PriceCurveObj.Price(N);

                        SolveSnap();

                        DSS.MonitorClass.SampleAll();     // Make all monitors take a sample
                        if SampleTheMeters then
                            DSS.EnergyMeterClass.SampleAll();  // Make all meters take a sample

                        EndOfTimeStepCleanup();


                    end;
{$IFDEF DSS_CAPI_PM}
                    DSS.ActorPctProgress := (N * 100) div NDaily;
{$ELSE}
                    DSS.ShowPctProgress((i * 100) div NDaily);
{$ENDIF}
                end
                else
                begin
                    DSS.ErrorNumber := SOLUTION_ABORT;
                    DSS.GlobalResult := 'Solution Aborted';
                    Break;
                end;

            end;
    finally
        DSS.MonitorClass.SaveAll();
        if SampleTheMeters then
            DSS.EnergyMeterClass.CloseAllDIFiles();   // Save Demand interval Files
{$IFNDEF DSS_CAPI_PM}
        DSS.ProgressHide();
{$ENDIF}
    end;
end;

function TSolutionAlgs.SolveLD2: Integer;

// Hold time fixed and just vary the global load multiplier according to the global
// Load-Duration Curve
var
    N: Integer;
begin
    Result := 0;
    if ckt.LoadDurCurveObj = NIL then
    begin
        DoSimpleMsg(DSS, _('Load Duration Curve Not Defined (Set LDCurve=... command). Cannot perForm solution.'), 471);
        Exit;
    end;

    // Time must be set before entering this routine


    // DSS.MonitorClass.ResetAll;
    // DSS.EnergyMeterClass.ResetAll;
    ckt.DefaultHourMult := ckt.DefaultDailyShapeObj.GetMultAtHour(DynaVars.dblHour);
    if not DSS.DIFilesAreOpen then
        DSS.EnergyMeterClass.OpenAllDIFiles;   // Open Demand Interval Files, if desired

// (set in Solve Method) DefaultGrowthFactor :=  IntPower(DefaultGrowthRate, (Year-1));

    try
        if DSS.SolutionAbort then
        begin
            DSS.ErrorNumber := SOLUTION_ABORT;
            DSS.GlobalResult := 'Solution Aborted.';
            Exit;
        end;

        for N := 1 to ckt.LoadDurCurveObj.NumPoints do
        begin
            // Adjust meter interval to interval on value of present Load-Duration Curve
            ckt.LoadMultiplier := ckt.LoadDurCurveObj.Mult(N);     // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
            IntervalHrs := ckt.LoadDurCurveObj.PresentInterval;

            // Price curve must correspond to load-duration curve
            if ckt.PriceCurveObj <> NIL then
                ckt.PriceSignal := ckt.PriceCurveObj.Price(N);

            SolveSnap();

            DSS.MonitorClass.SampleAll();  // Make all monitors take a sample
            if SampleTheMeters then
                DSS.EnergyMeterClass.SampleAll();  // Make all meters take a sample

            EndOfTimeStepCleanup();

        end;
    finally
        DSS.MonitorClass.SaveAll();
        if SampleTheMeters then
            DSS.EnergyMeterClass.CloseAllDIFiles();   // Save Demand interval Files
    end;
end;

procedure TSolutionAlgs.PickAFault;
// Enable one of the faults in the circuit.  Disable the rest
var
    NumFaults, i, Whichone: Integer;
    FaultObj: TFaultObj;
begin
    NumFaults := ckt.Faults.Count;
    Whichone := Trunc(Random * NumFaults) + 1;
    if Whichone > NumFaults then
        Whichone := NumFaults;

    for i := 1 to NumFaults do
    begin
        FaultObj := ckt.Faults.Get(i);
        if i = Whichone then
        begin
            DSS.ActiveFaultObj := FaultObj; // in Fault Unit
            FaultObj.Enabled := TRUE;
        end
        else
            FaultObj.Enabled := FALSE;
    end;
end;

function TSolutionAlgs.SolveMonteFault: Integer;
var
    N: Integer;
begin
    Result := 0;
    try
        LoadModel := ADMITTANCE;   // All Direct solution
        ckt.LoadMultiplier := 1.0;    // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
        DynaVars.intHour := 0;
        DynaVars.dblHour := 0.0; // Use hour to denote Case number
        DynaVars.t := 0.0;


        // DSS.MonitorClass.ResetAll();

{$IFDEF DSS_CAPI_PM}
{$ELSE}
        DSS.ProgressCaption('Monte Carlo Fault Study: ' + IntToStr(NumberofTimes) + ' Different Faults.');
{$ENDIF}
        ProgressCount := 0;

        SetGeneratorDispRef();

        for N := 1 to NumberOfTimes do
            if not DSS.SolutionAbort then
            begin
                Inc(DynaVars.intHour);
                PickAFault();  // Randomly enable one of the faults
                DSS.ActiveFaultObj.Randomize();  // Randomize the fault resistance
                SolveDirect();
                DSS.MonitorClass.SampleAll();  // Make all monitors take a sample
{$IFDEF DSS_CAPI_PM}
                DSS.ActorPctProgress := (N * 100) div NumberOfTimes;
{$ELSE}
                Show10PctProgress(N, NumberOfTimes);
{$ENDIF}
            end;
    finally
        DSS.MonitorClass.SaveAll();
{$IFNDEF DSS_CAPI_PM}
        DSS.ProgressHide();
{$ENDIF}
    end;
end;

procedure TSolutionAlgs.AllocateAllSCParms;
var
    i: Integer;
begin
    for i := 1 to ckt.NumBuses do
        ckt.Buses[i].AllocateBusQuantities();
end;


procedure TSolutionAlgs.ComputeIsc;
//  Compute Isc at all buses for current values of Voc and Ysc 
var
    i: Integer;
begin
    for i := 1 to ckt.NumBuses do
        ckt.Buses[i].Ysc.MVMult(ckt.Buses[i].BusCurrent, ckt.Buses[i].VBus);
end;

procedure TSolutionAlgs.ComputeYsc(pBus: TDSSBus);
// Compute YSC for I-th bus
// Assume InjCurr is zeroed
var
    i,
    j,
    ref1: Integer;
begin
    pBus.Zsc.Clear();
    for i := 1 to pBus.NumNodesThisBus do
    begin
        ref1 := pBus.RefNo[i];
        if ref1 > 0 then
        begin
            Currents[ref1] := 1;
            // SparseSet expects 1st element of voltage array, not 0-th element
            if SolveSparseSet(hYsystem, pComplexArray(@NodeV[1]), pComplexArray(@Currents[1])) < 1 then
                raise EEsolv32Problem.Create('Error Solving System Y Matrix in ComputeYsc. Problem with Sparse matrix solver.');
            // Extract Voltage Vector = column of Zsc
            for j := 1 to pBus.NumNodesThisBus do
            begin
                pBus.Zsc[j, i] := NodeV[pBus.RefNo[j]];
            end;
            Currents[Ref1] := 0;
        end;
    end;
    pBus.Ysc.CopyFrom(pBus.Zsc);
    pBus.Ysc.Invert(); // Save as admittance
end;

procedure TSolutionAlgs.ComputeAllYsc;
var
    iB, j: Integer;
begin
    for j := 1 to ckt.NumNodes do
        Currents[j] := 0;

    ProgressCount := 0;

    for iB := 1 to ckt.NumBuses do
    begin
        ComputeYsc(ckt.Buses[iB]);  // Compute YSC for iB-th Bus
        if ((iB * 10) div ckt.NumBuses) > ProgressCount then
        begin
            Inc(ProgressCount);
{$IFDEF DSS_CAPI_PM}
{$ELSE}
            DSS.ShowPctProgress(30 + ProgressCount * 5);
{$ENDIF}
        end;
    end;
end;

procedure TSolutionAlgs.DisableAllFaults;
var 
    obj: TFaultObj;
begin
    for obj in ckt.Faults do
    begin
        DSS.ActiveFaultObj := obj; // for backwards compatibility
        obj.Enabled := FALSE;
    end
end;

function TSolutionAlgs.SolveFaultStudy: Integer;
begin
    Result := 0;

{$IFDEF DSS_CAPI_PM}
    DSS.ActorPctProgress := 0;
{$ELSE}
    DSS.ShowPctProgress(0);
    DSS.ProgressCaption('Computing Open-Circuit Voltages');
{$ENDIF}
    LoadModel := ADMITTANCE;
    DisableAllFaults();

    SolveDirect();   // This gets the open circuit voltages and bus lists corrected

    AllocateAllSCParms();   // Reallocate bus quantities
    UpdateVBus();  // Put present solution Voc's in bus quantities

{$IFDEF DSS_CAPI_PM}
    DSS.ActorPctProgress := 30;
{$ELSE}
    DSS.ProgressCaption('Computing Ysc Matrices for Each Bus');
    DSS.ShowPctProgress(30);
{$ENDIF}
    ComputeAllYsc();

{$IFDEF DSS_CAPI_PM}
    DSS.ActorPctProgress := 80;
{$ELSE}
    DSS.ProgressCaption('Computing Short-circuit currents.');
    DSS.ShowPctProgress(80);
{$ENDIF}
    ComputeIsc();

{$IFDEF DSS_CAPI_PM}
    DSS.ActorPctProgress := 100;
{$ELSE}
    DSS.ShowPctProgress(100);
    DSS.ProgressCaption('Done.');
    DSS.ProgressHide();
{$ENDIF}
   // Now should have all we need to make a short circuit report
end;

procedure TSolutionAlgs.AddFrequency(var FreqList: ArrayOfDouble; var NumFreq, MaxFreq: Integer; F: Double);
// Add unique Frequency, F to list in ascending order, reallocating if necessary
var
    i, j: Integer;
    otherF: Double;
begin
    // See if F is in List
    for otherF in FreqList do
    begin
         // Allow a little tolerance (0.1 hz) for the Frequency for round off error
        if Abs(F - otherF) < 0.1 then
            Exit; // Already in List, nothing to do
    end;

    // OK, it's not in list, so let's Add it
    Inc(NumFreq);
    if NumFreq > MaxFreq then
    begin  // Let's make a little more room
        Inc(MaxFreq, 20);
        SetLength(FreqList, MaxFreq);
    end;

    // Let's add it in ascending order
    for i := 1 to NumFreq - 1 do
    begin
        if F < FreqList[i - 1] then
        begin
            // Push down array and insert it
            for j := NumFreq - 1 downto i do
                FreqList[j] := FreqList[j - 1];
            FreqList[i - 1] := F;
            Exit;  // We're done!
        end;
    end;

    // If we fall through, tack it on to the end
    FreqList[NumFreq - 1] := F;
end;

function TSolutionAlgs.GetSourceFrequency(pc: TPCElement): Double; // TODO - applicable to VCCS?
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

procedure TSolutionAlgs.CollectAllFrequencies(var FreqList: ArrayOfDouble);
var
    SpectrumInUse: Array of Boolean;
    p: TPCElement;
    MaxFreq, i, j: Integer;
    pSpectrum: TSpectrumObj;
    f: Double;

    NumFreq: Integer;
begin
    // Make a List of all frequencies in Use

    // accumulate all unique Frequencies
    MaxFreq := 20;    // Initial List size
    NumFreq := 0;
    SetLength(FreqList, MaxFreq);

    // Check Sources -- each could have a different base frequency
    for p in ckt.Sources do
    begin
        if not p.Enabled then
            continue;

        if p.SpectrumObj <> NIL then
        begin
            pSpectrum := p.SpectrumObj;
            f := GetSourceFrequency(p);
            for j := 1 to pSpectrum.NumHarm do
            begin
                AddFrequency(FreqList, NumFreq, MaxFreq, pSpectrum.HarmArray[j] * f);
            end;
        end;
    end;

    // Mark Spectra being used
    // Check loads and generators - these are assumed to be at fundamental frequency
    SetLength(SpectrumInUse, DSS.SpectrumClass.ElementCount()); // Allocate and zero
    for p in ckt.PCelements  do
    begin
        if p.enabled then
            if (p.SpectrumObj <> NIL) and (DSS.SpectrumClass.Find(p.SpectrumObj.Name) <> NIL) then
                SpectrumInUse[DSS.SpectrumClass.Active - 1] := true;
    end;

    // Add marked Spectra to list
    for i := 1 to DSS.SpectrumClass.ElementCount() do
    begin
        if SpectrumInUse[i - 1] then
        begin
            DSS.SpectrumClass.Active := i;
            pSpectrum := DSS.SpectrumClass.GetActiveObj;
            for j := 1 to pSpectrum.NumHarm do
                AddFrequency(FreqList, NumFreq, MaxFreq, pSpectrum.HarmArray[j] * ckt.Fundamental);
        end;
    end;

    SetLength(FreqList, NumFreq);
end;

function TSolutionAlgs.SolveHarmonic: Integer;
var
    FrequencyList: Array Of Double;
    i: Integer;
begin
    Result := 0;

    FrequencyList := NIL;   // Set up for Reallocmem
{$IFDEF DSS_CAPI_PM}
    DSS.ActorPctProgress := 0;
{$ELSE}
    DSS.ShowPctProgress(0);
    DSS.ProgressCaption('Performing Harmonic Solution');
{$ENDIF}
    try

        if Frequency <> ckt.Fundamental then
        begin // Last solution was something other than fundamental
            Frequency := ckt.Fundamental;
            if not RetrieveSavedVoltages(DSS) then
                Exit; // Get Saved fundamental frequency solution
        end;

        DSS.MonitorClass.SampleAll();   // Store the fundamental frequency in the monitors

        // Get the list of Harmonic Frequencies to solve at
        if DoAllHarmonics then
            CollectAllFrequencies(FrequencyList)   // Allocates FrequencyList
        else
        begin
            SetLength(FrequencyList, Length(HarmonicList));
            for i := 0 to High(FrequencyList) do
                FrequencyList[i] := ckt.Fundamental * HarmonicList[i];
        end;

        for i := 0 to High(FrequencyList) do
        begin
            Frequency := FrequencyList[i];   // forces rebuild of SystemY
            if Abs(Harmonic - 1.0) > EPSILON then
            begin    // Skip fundamental
                {$IFDEF DSS_CAPI_PM}
                DSS.ActorPctProgress := Round((100.0 * i) / Length(FrequencyList));
                {$ELSE}
                DSS.ProgressCaption('Solving at Frequency = ' + Format('%-g', [Frequency]));
                DSS.ShowPctProgress(Round((100.0 * i) / Length(FrequencyList)));
                {$ENDIF}

                SolveDirect();
                DSS.MonitorClass.SampleAll();
                // Storage devices are assumed to stay the same since there is no time variation in this mode
            end;

        end;

        {$IFDEF DSS_CAPI_PM}
        DSS.ActorPctProgress := 100;
        {$ELSE}
        DSS.ShowPctProgress(100);
        DSS.ProgressCaption('Done.');
        {$ENDIF}
    finally
{$IFNDEF DSS_CAPI_PM}
        DSS.ProgressHide();
{$ENDIF}
        DSS.MonitorClass.SaveAll();
    end;
end;

function TSolutionAlgs.SolveHarmTime: Integer;     // It is based in SolveGeneralTime routine
begin
    Result := 0;

    IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
    if DSS.SolutionAbort then
        Exit;

    with DynaVars do
    begin
        // Compute basic multiplier from Default loadshape to use in generator dispatch, if any
        ckt.DefaultHourMult := ckt.DefaultDailyShapeObj.GetMultAtHour(dblHour);

        SolveSnap();
        // IncrementTime;  // This function is handled from SolveHarmonics (04-10-2013)
    end;
end;

function TSolutionAlgs.SolveHarmonicT: Integer;
var
    FrequencyList: Array Of Double;
    i: Integer;

begin
    Result := 0;

    IntervalHrs := DynaVars.h / 3600.0;  // needed for energy meters and storage devices
    try
        if Frequency <> ckt.Fundamental then
        begin     // Last solution was something other than fundamental
            Frequency := ckt.Fundamental;
            if not RetrieveSavedVoltages(DSS) then
                Exit; // Get Saved fundamental frequency solution
        end;
        // DefaultHourMult := DefaultDailyShapeObj.GetMultAtHour(DynaVars.dblHour);
        // IF Load_Changed THEN Begin    //Added to update the current sources of all frequencies any time
        InitializeForHarmonics(DSS);  //the value of a load changes in a proportional way
        //      Load_Changed:=FALSE;     // Added 05 dec 2013 - D. Montenegro
        // End;
        SolveSnap();
        DSS.MonitorClass.SampleAll();   // Store the fundamental frequency in the monitors
        // Get the list of Harmonic Frequencies to solve at
        if DoAllHarmonics then
            CollectAllFrequencies(FrequencyList)   // Allocates FrequencyList
        else
        begin
            SetLength(FrequencyList, Length(HarmonicList));
            for i := 0 to High(FrequencyList) do
                FrequencyList[i] := ckt.Fundamental * HarmonicList[i];
        end;

        for i := 0 to High(FrequencyList) do
        begin
            Frequency := FrequencyList[i]; // forces rebuild of SystemY
            if Abs(Harmonic - 1.0) > EPSILON then
            begin  // Skip fundamental
                // DefaultHourMult := DefaultDailyShapeObj.GetMultAtHour(DynaVars.dblHour);
                SolveHarmTime();
                DSS.MonitorClass.SampleAll();
                EndOfTimeStepCleanup();
                // Storage devices are assumed to stay the same since there is no time variation in this mode  (Not necessarelly now)
            end;
        end;
        IncrementTime();
    finally
        DSS.MonitorClass.SaveAll();
    end;
end;

end.
