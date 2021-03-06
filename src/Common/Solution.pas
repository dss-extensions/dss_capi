unit Solution;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{ Change Log
 8-14-99 Added progress display and abort on longer solution types
 11-3-99 added calc voltage base
 11-21-99 modified to  calc the voltage bases at the current load level set by the user.
 12-1-99 Added code to estimate starting point for P-V Generators
 12-2-99 Made more properties visible
 12-6-99 Merged properties with Set Command and removed from here
 12-15-99 Added global generatordispatchreference
 1-8-00   Fixed bug in autoadd generators to work with new generator model
          set vminpu=0 and vmaxpu=1000000
 1-30-00 to 2-1-00 Implemented control action check in solution
 2-19-00 Frequency changed reset to FALSE after being used (was causing all YPrims to be recomputed)
 2-23-00 Modified so that reset of meters and monitors is done upon setting the solution mode property.
         After that the user must reset else the monitors just accumulate.
 3-20-00 Fixed bug with setting generator disp reference - made uniform for all types
 6-11-00 Split into two modules + moved auto add stuff to AutoAdd
 9-20-00 Added Dynamic Mode
 10-25-00 Added Fundamental Freq and other stuff for Harmonics Solution
 5-30-01  Added control iterations check, mostIterationsdone.
          Fixed bug with controls off doing the solution too many times.

 8-14-01 Reset IntervalHrs on Mode change
 7-11-02 Added check for system Y change after computing currents

 9-28-03 Redefined V to NodeV and changed from an array from 1..n to 0..n where
         0-th element is alway ground(complex zero volts).
 8-14-06 Revised power flow initialization; removed forward/backward sweep

 9-14-16 Added SampleTheMeters Flag to allow sampling energy meters in Time and DutyCycle mode

}

interface

uses
    Classes,
    uCOMPLEX,
    Arraydef,
    Command,
    Monitor,
    DSSClass,
    DSSObject,
    Dynamics,
    EnergyMeter,
    Sparse_Math,
    VSource,
    SysUtils,
{$IFDEF DSS_CAPI_PM}
    Parallel_Lib,
{$ENDIF}
{$IFDEF MSWINDOWS}
    Windows,
{$ELSE}
    BaseUnix,
    Unix,
{$ENDIF}
    Strings,
    SyncObjs,
    ExecHelper,
    CktElement;

const

    NORMALSOLVE = 0;
    NEWTONSOLVE = 1;
// Constants for the actor's messaging
    SIMULATE = 0;
    EXIT_ACTOR = 1;

    ALL_ACTORS = 0; // Wait flag for all the actors
    AD_ACTORS = 1; // Wait flag to wait only for the A-Diakoptics actors



type

{$IFDEF DSS_CAPI_INCREMENTAL_Y}
{$SCOPEDENUMS ON}
    TSolverOptions = (
        // The values themselves are subject to change in future versions,
        // use this enum for easier upgrades
        ReuseNothing = 0,
        ReuseCompressedMatrix = 1, // Reuse only the prepared CSC matrix
        ReuseSymbolicFactorization = 2, // Reuse the symbolic factorization, implies ReuseCompressedMatrix
        ReuseNumericFactorization = 3, // Reuse the numeric factorization, implies ReuseSymbolicFactorization
        
        AlwaysResetYPrimInvalid = $100000000 // Bit flag, see CktElement.pas
    );
{$SCOPEDENUMS OFF}
{$ENDIF}

    EControlProblem = class(Exception);
    ESolveError = class(Exception);  // Raised when solution aborted

    TNodeVarray = array[0..1000] of Complex;
    pNodeVarray = ^TNodeVarray;


    TDSSSolution = class(TDSSClass)


    PRIVATE
//       CommandList:TCommandlist;
    PROTECTED
        procedure DefineProperties;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

    end;

{$IFDEF DSS_CAPI_PM}
    TInfoMessageCall = procedure(const info: String) of object;  // Creates the procedure for sending a message

    TSolver = class(TThread)
        constructor Create(dssContext: TDSSContext; Susp: Boolean; local_CPU: Integer; CallBack: TInfoMessageCall; AEvent: TEvent); OVERLOAD;
        procedure Execute; OVERRIDE;
        procedure Doterminate; OVERRIDE;
        destructor Destroy; OVERRIDE;

//*******************************Private components*****************************
    PROTECTED
        DSS: TDSSContext;

        FMessage,
        Msg_Cmd: String;
        UINotifier,
        FInfoProc: TInfoMessageCall;
        MsgType: Integer;
        UIEvent,
        ActorMsg: TEvent;
        AD_Init,          // used to know if the actors require a partial solution
        ActorActive,
        Processing: Boolean;

{$IFDEF DSS_CAPI_PM}
        procedure Start_Diakoptics();
{$ENDIF}
        procedure Notify_Main;
        function Get_Processing(): Boolean;
        procedure Set_Processing(Nval: Boolean);
        function Get_CPU(): Integer;
        procedure Set_CPU(CPU: Integer);
//*******************************Public components******************************
    PUBLIC
        procedure Send_Message(Msg: Integer);
        procedure CallCallBack;
        property Event: TEvent READ UIEvent;

        property Is_Busy: Boolean READ Get_Processing WRITE Set_Processing;
        property CPU: Integer READ Get_CPU WRITE Set_CPU;

    end;
{$ENDIF}

    TSolutionObj = class(TDSSObject)
    PRIVATE
        dV: pNodeVArray;   // Array of delta V for Newton iteration
        FFrequency: Double;

        function OK_for_Dynamics(const Value: TSolveMode): Boolean;
        function OK_for_Harmonics(const Value: TSolveMode): Boolean;


        procedure DoNewtonSolution;
        procedure DoNormalSolution;
//       PROCEDURE GetMachineInjCurrents;
        procedure SumAllCurrents;
        procedure Set_Frequency(const Value: Double);
        procedure Set_Mode(const Value: TSolveMode);
        procedure Set_Year(const Value: Integer);
        procedure Set_Total_Time(const Value: Double);

    PUBLIC

        Algorithm: Integer;      // NORMALSOLVE or NEWTONSOLVE
        AuxCurrents: pComplexArray;  // For injections like AutoAdd
        ControlActionsDone: Boolean;
        ControlIteration: Integer;
        ControlMode: Integer;     // EVENTDRIVEN, TIMEDRIVEN
        ConvergenceTolerance: Double;
        ConvergedFlag: Boolean;
        DefaultControlMode: Integer;    // EVENTDRIVEN, TIMEDRIVEN
        DefaultLoadModel: Integer;     // 1=POWERFLOW  2=ADMITTANCE
        DoAllHarmonics: Boolean;
        DynamicsAllowed: Boolean;
        DynaVars: TDynamicsRec;
        ErrorSaved: pDoubleArray;
        // FirstIteration: Boolean; -- unused
        FrequencyChanged: Boolean;  // Flag set to true if something has altered the frequency
        Fyear: Integer;
        Harmonic: Double;
        HarmonicList: pDoubleArray;
        HarmonicListSize: Integer;
        hYsystem: NativeUint;   {Handle for main (system) Y matrix}
        hYseries: NativeUint;   {Handle for series Y matrix}
        hY: NativeUint;         {either hYsystem or hYseries}
        IntervalHrs: Double;   // Solution interval since last solution, hrs.
        IsDynamicModel: Boolean;
        IsHarmonicModel: Boolean;
        Iteration: Integer;
        LoadModel: Integer;        // 1=POWERFLOW  2=ADMITTANCE
        LastSolutionWasDirect: Boolean;
        LoadsNeedUpdating: Boolean;
        MaxControlIterations: Integer;
        MaxError: Double;
        MaxIterations,
        MinIterations: Integer;
        MostIterationsDone: Integer;
        NodeVbase: pDoubleArray;
        NumberOfTimes: Integer;  // Number of times to solve
        PreserveNodeVoltages: Boolean;
        RandomType: Integer;     //0 = none; 1 = gaussian; 2 = UNIFORM
        SampleTheMeters: Boolean;  // Flag to allow sampling of EnergyMeters
        SeriesYInvalid: Boolean;
        SolutionCount: Integer;  // Counter incremented for each solution
        SolutionInitialized: Boolean;
        SystemYChanged: Boolean;
        UseAuxCurrents: Boolean;
        VmagSaved: pDoubleArray;
        VoltageBaseChanged: Boolean;
        ProgressCount: Integer; // used in SolutionAlgs
        SolverOptions: Uint64;   // KLUSolveX options


        {Voltage and Current Arrays}
        NodeV: pNodeVArray;    // Main System Voltage Array   allows NodeV^[0]=0
        Currents: pNodeVArray;      // Main System Currents Array
{$IFDEF DSS_CAPI_PM}
       {A-Diakoptics variables}
        Node_dV: pNodeVArray;     // Used to store the partial solution voltage
        Ic_Local: pNodeVArray;     // Used to store the complementary curret
{$ENDIF}

//******************************************************************************
        IncMat: Tsparse_matrix; // Incidence sparse matrix
        Laplacian: Tsparse_matrix; // Laplacian sparse matrix

//****************************Timing variables**********************************
        SolveStartTime: Int64;
        SolveEndtime: Int64;
        GStartTime: Int64;
        Gendtime: Int64;
        LoopEndtime: Int64;
        Total_Time_Elapsed: Double;
        Solve_Time_Elapsed: Double;
        Total_Solve_Time_Elapsed: Double;
        Step_Time_Elapsed: Double;
//******************************************************************************
// ActiveCell of the Incidence Matrix:
// [0] = row
// [1] = col
// [2] = value
        ActiveIncCell: array[0..2] of Integer;
//******************************************************************************
// IncMatrix Row and column descriptors
// Rows array (array of strings that tells what is the order of the PDElements)
// Columns array (array of strigns with the names of the cols of the Inc matrix)'
// Levels array (array of integers that describes the proximity level for each
// bus to the circuit's backbone)
        Inc_Mat_Rows: array of String;
        Inc_Mat_Cols: array of String;
        Inc_Mat_levels: array of Integer;
        temp_counter: Integer;
        Active_Cols: array of Integer;
        Active_Cols_Idx: array of Integer;
//******************************************************************************
//********************Diakoptics solution mode variables************************
{$IFDEF DSS_CAPI_PM}
        ADiakoptics_ready: Boolean;
        ADiakoptics_Actors: Integer;
        LockNodeV: TCriticalSection;
{$ENDIF}
//******************************************************************************
        constructor Create(ParClass: TDSSClass; const solutionname: String);
        destructor Destroy; OVERRIDE;

        function Converged: Boolean;
        procedure SetGeneratordQdV;

        procedure ZeroAuxCurrents;
        function SolveZeroLoadSnapShot: Integer;
        procedure DoPFLOWsolution;

        procedure Solve;                // Main Solution dispatch
        procedure SnapShotInit;
        function SolveSnap: Integer;    // solve for now once
        function SolveDirect: Integer;  // solve for now once, direct solution
        function SolveYDirect: Integer; // Similar to SolveDirect; used for initialization
        function SolveCircuit: Integer; // SolveSnap sans control iteration
        procedure CheckControls;       // Snapshot checks with matrix rebuild
        procedure SampleControlDevices;
        procedure DoControlActions;
        procedure Sample_DoControlActions;    // Sample and Do
        procedure Check_Fault_Status;

        procedure SetGeneratorDispRef;
        procedure SetVoltageBases;

        procedure SaveVoltages;
        procedure UpdateVBus; // updates voltages for each bus    from NodeV
        procedure RestoreNodeVfromVbus;  // opposite   of updatebus

        function VDiff(i, j: Integer): Complex;  // Difference between two node voltages

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(F: TFileStream; Complete: Boolean); OVERRIDE;
        procedure WriteConvergenceReport(const Fname: String);
        procedure Update_dblHour;
        procedure Increment_time;

        procedure UpdateLoopTime;

        property Mode: TSolveMode READ dynavars.SolutionMode WRITE Set_Mode;
        property Frequency: Double READ FFrequency WRITE Set_Frequency;
        property Year: Integer READ FYear WRITE Set_Year;
        property Time_Solve: Double READ Solve_Time_Elapsed;
        property Time_TotalSolve: Double READ Total_Solve_Time_Elapsed;
        property Time_Step: Double READ Step_Time_Elapsed;     // Solve + sample
        property Total_Time: Double READ Total_Time_Elapsed WRITE Set_Total_Time;

 // Procedures that use to be private before 01-20-2016

        procedure AddInAuxCurrents(SolveType: Integer);
        function SolveSystem(V: pNodeVArray): Integer;
        procedure GetPCInjCurr;
        procedure GetSourceInjCurrents;
        procedure ZeroInjCurr;
        procedure Upload2IncMatrix;

        procedure Calc_Inc_Matrix;                // Calculates the incidence matrix for the Circuit
        procedure Calc_Inc_Matrix_Org;            // Calculates the incidence matrix hierarchically organized for the Circuit

        function get_IncMatrix_Row(Col: Integer): Integer;          // Gets the index of the Row connected to the specified Column
        function get_IncMatrix_Col(Row: Integer): Integer;          // Gets the index of the Column connected to the specified Row
        function CheckLocationIdx(Idx: Integer): Integer;           // Evaluates the area covered by the tearing point to see if there is a better one

        procedure AddLines2IncMatrix;             // Adds the Lines to the Incidence matrix arrays
        procedure AddXfmr2IncMatrix;              // Adds the Xfmrs to the Incidence matrix arrays
        procedure AddSeriesCap2IncMatrix;         // Adds capacitors in series to the Incidence matrix arrays
        procedure AddSeriesReac2IncMatrix;        // Adds Reactors in series to the Incidence matrix arrays

    end;

{==========================================================================}

implementation

uses
    SolutionAlgs,
    DSSClassDefs,
    DSSGlobals,
{$IFDEF MSWINDOWS}
    SHELLAPI,
{$ENDIF}
    CmdForms,
    PDElement,
    ControlElem,
    Fault,
    Executive,
    AutoAdd,
    YMatrix,
    ParserDel,
    Generator,
    Load,
    CKtTree,
    Capacitor,
    Transformer,
    Reactor,
    Math,
    Circuit,
    Utilities,
    KLUSolve,
    DSSPointerList,
    Line,
{$IFDEF DSS_CAPI_PM}
    Diakoptics,
{$ENDIF}
    DSSHelper;

const
    NumPropsThisClass = 1;

// ===========================================================================================
constructor TDSSSolution.Create(dssContext: TDSSContext);  // Collection of all solution objects
begin
    inherited Create(dssContext);
    Class_Name := 'Solution';
    DSSClassType := DSS_OBJECT + HIDDEN_ELEMENT;

    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

// ===========================================================================================
destructor TDSSSolution.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;

end;

// ===========================================================================================
procedure TDSSSolution.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


     // Define Property names
    PropertyName[1] := '-------';


     // define Property help values
    PropertyHelp[1] := 'Use Set Command to set Solution properties.';


    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;


// ===========================================================================================
function TDSSSolution.NewObject(const ObjName: String): Integer;
begin
    // Make a new Solution Object and add it to Solution class list
    DSS.ActiveSolutionObj := TSolutionObj.Create(Self, ObjName);
    // this one is different than the rest of the objects.
    Result := AdDobjectToList(DSS.ActiveSolutionObj);
end;

// ===========================================================================================
constructor TSolutionObj.Create(ParClass: TDSSClass; const SolutionName: String);
// ===========================================================================================
begin
    inherited Create(ParClass);
    Name := LowerCase(SolutionName);

    SolverOptions := 0;

    FYear := 0;
    DynaVars.intHour := 0;
    DynaVars.t := 0.0;
    DynaVars.dblHour := 0.0;
    DynaVars.tstart := 0.0;
    DynaVars.tstop := 0.0;
    //duration := 0.0;
    DynaVars.h := 0.001;  // default for dynasolve

    LoadsNeedUpdating := TRUE;
    VoltageBaseChanged := TRUE;  // Forces Building of convergence check arrays

    MaxIterations := 15;
    MinIterations := 2;
    MaxControlIterations := 10;
    ConvergenceTolerance := 0.0001;
    ConvergedFlag := FALSE;

    SampleTheMeters := FALSE;  // Flag to tell solution algorithm to sample the Energymeters

    IsDynamicModel := FALSE;
    IsHarmonicModel := FALSE;

    Frequency := DSS.DefaultBaseFreq;
    Harmonic := 1.0;

    FrequencyChanged := TRUE;  // Force Building of YPrim matrices
    DoAllHarmonics := TRUE;
    // FirstIteration := TRUE;
    DynamicsAllowed := FALSE;
    SystemYChanged := TRUE;
    SeriesYInvalid := TRUE;

    {Define default harmonic list}
    HarmonicListSize := 5;
    HarmonicList := AllocMem(SizeOf(harmonicList^[1]) * HarmonicListSize);
    HarmonicList^[1] := 1.0;
    HarmonicList^[2] := 5.0;
    HarmonicList^[3] := 7.0;
    HarmonicList^[4] := 11.0;
    HarmonicList^[5] := 13.0;

    SolutionInitialized := FALSE;
    LoadModel := POWERFLOW;
    DefaultLoadModel := LoadModel;
    LastSolutionWasDirect := FALSE;

    hYseries := 0;
    hYsystem := 0;
    hY := 0;

    NodeV := NIL;
    dV := NIL;
    Currents := NIL;
    AuxCurrents := NIL;
    VMagSaved := NIL;
    ErrorSaved := NIL;
    NodeVbase := NIL;

    UseAuxCurrents := FALSE;

    SolutionCount := 0;

    Dynavars.SolutionMode := TSolveMode.SNAPSHOT;
    ControlMode := CTRLSTATIC;
    DefaultControlMode := ControlMode;
    Algorithm := NORMALSOLVE;

    RandomType := GAUSSIAN;  // default to gaussian
    NumberOfTimes := 100;
    IntervalHrs := 1.0;

    InitPropertyValues(0);

{$IFDEF DSS_CAPI_PM}
    ADiakoptics_Ready := FALSE;   // A-Diakoptics needs to be initialized
    if not Assigned(DSS.ActorMA_Msg) then
        DSS.ActorMA_Msg := TEvent.Create(NIL, TRUE, FALSE, '');

    LockNodeV := SyncObjs.TCriticalSection.Create;
{$ENDIF}
end;

// ===========================================================================================
destructor TSolutionObj.Destroy;
begin
    Reallocmem(AuxCurrents, 0);
    Reallocmem(Currents, 0);
    Reallocmem(dV, 0);
    Reallocmem(ErrorSaved, 0);
    Reallocmem(NodeV, 0);
    Reallocmem(NodeVbase, 0);
    Reallocmem(VMagSaved, 0);

    if hYsystem <> 0 then
        DeleteSparseSet(hYsystem);
    if hYseries <> 0 then
        DeleteSparseSet(hYseries);

//      SetLogFile ('c:\\temp\\KLU_Log.txt', 0);

    Reallocmem(HarmonicList, 0);
{$IFDEF DSS_CAPI_PM}    
    // Sends a message to the working actor
    with DSS do 
    begin
        ActorMA_Msg.SetEvent();
        if ActorThread <> NIL then
        begin
            ActorThread.Send_Message(EXIT_ACTOR);
            ActorThread.WaitFor();
            ActorThread.Free();
            ActorThread := nil;
        end;
        ActorMA_Msg.Free;
        ActorMA_Msg := NIL;
    end;
    LockNodeV.Free;
{$ENDIF}

    inherited Destroy;
end;


// ===========================================================================================
function TDSSSolution.Edit: Integer;

begin
    Result := 0;

    DSS.ActiveSolutionObj := DSS.ActiveCircuit.Solution;

    with DSS.ActiveSolutionObj do
    begin

       // This is all we do here now...
        Solve;

    end;  {WITH}
end;

// ===========================================================================================
procedure TSolutionObj.Solve;

begin
    DSS.ActiveCircuit.Issolved := FALSE;
    DSS.SolutionWasAttempted := TRUE;

    InitProgressForm; // initialize Progress Form;

{Check of some special conditions that must be met before executing solutions}

    if DSS.ActiveCircuit.EmergMinVolts >= DSS.ActiveCircuit.NormalMinVolts then
    begin
        DoSimpleMsg('Error: Emergency Min Voltage Must Be Less Than Normal Min Voltage!' +
            CRLF + 'Solution Not Executed.', 480);
        Exit;
    end;

    if DSS.SolutionAbort then
    begin
        DSS.GlobalResult := 'Solution aborted.';
        DSS.CmdResult := SOLUTION_ABORT;
        DSS.ErrorNumber := DSS.CmdResult;
        Exit;
    end;
    try
{Main solution Algorithm dispatcher}
        with DSS.ActiveCircuit do
        begin

            case Year of
                0:
                    DefaultGrowthFactor := 1.0;    // RCD 8-17-00
            else
                DefaultGrowthFactor := IntPower(DefaultGrowthRate, (year - 1));
            end;
        end;
{$IFDEF DLL_ENGINE}
        Fire_InitControls;
{$ENDIF}
    {CheckFaultStatus;  ???? needed here??}
     {$IFDEF MSWINDOWS}
        QueryPerformanceCounter(GStartTime);
{$ENDIF}
{$IFNDEF DSS_CAPI_PM}
        case Dynavars.SolutionMode of
            TSolveMode.SNAPSHOT:
                SolveSnap;
            TSolveMode.YEARLYMODE:
                SolveYearly;
            TSolveMode.DAILYMODE:
                SolveDaily;
            TSolveMode.DUTYCYCLE:
                SolveDuty;
            TSolveMode.DYNAMICMODE:
                SolveDynamic;
            TSolveMode.MONTECARLO1:
                SolveMonte1;
            TSolveMode.MONTECARLO2:
                SolveMonte2;
            TSolveMode.MONTECARLO3:
                SolveMonte3;
            TSolveMode.PEAKDAY:
                SolvePeakDay;
            TSolveMode.LOADDURATION1:
                SolveLD1;
            TSolveMode.LOADDURATION2:
                SolveLD2;
            TSolveMode.DIRECT:
                SolveDirect;
            TSolveMode.MONTEFAULT:
                SolveMonteFault;  // Monte Carlo Fault Cases
            TSolveMode.FAULTSTUDY:
                SolveFaultStudy;
            TSolveMode.AUTOADDFLAG:
                DSS.ActiveCircuit.AutoAddObj.Solve;
            TSolveMode.HARMONICMODE:
                SolveHarmonic;
            TSolveMode.GENERALTIME:
                SolveGeneralTime;
            TSolveMode.HARMONICMODET:
                SolveHarmonicT;  //Declares the Hsequential-time harmonics
        else
            DoSimpleMsg('Unknown solution mode.', 481);
        end;
    {$IFDEF MSWINDOWS}
        QueryPerformanceCounter(GEndTime);
    {$ENDIF}
         Total_Solve_Time_Elapsed := ((GEndTime - GStartTime) / CPU_Freq) * 1000000;
         Total_Time_Elapsed := Total_Time_Elapsed + Total_Solve_Time_Elapsed;
{$ELSE} // DSS_CAPI_PM
        // Creates the actor again in case of being terminated due to an error before
        if (DSS.ActorThread = NIL) or DSS.ActorThread.Terminated then
        begin
            if DSS.ActorThread.Terminated then
                DSS.ActorThread.Free;

            New_Actor(DSS);
        end;
        {CheckFaultStatus;  ???? needed here??}

        // Resets the event for receiving messages from the active actor
        // Updates the status of the Actor in the GUI
        DSS.ActorStatus := TActorStatus.Busy;
        DSS.ActorMA_Msg.ResetEvent;
        {$IFNDEF FPC}
        if not DSS.ADiakoptics then
        begin
            if not IsDLL then
                ScriptEd.UpdateSummaryForm('1');
        end
        else
        begin
            if DSS.IsPrime then
                if not IsDLL then
                    ScriptEd.UpdateSummaryForm('1');
        end;
        {$ENDIF}
        
        {$IFDEF MSWINDOWS}
        QueryPerformanceCounter(GEndTime);
        {$ENDIF}
        Total_Solve_Time_Elapsed := ((GEndTime - GStartTime) / CPU_Freq) * 1000000;
        Total_Time_Elapsed := Total_Time_Elapsed + Total_Solve_Time_Elapsed;

        // Sends message to start the Simulation
        DSS.ActorThread.Send_Message(SIMULATE);
        // If the parallel mode is not active, Waits until the actor finishes
        if not DSS.Parallel_enabled then
        begin
            Wait4Actors(DSS, ALL_ACTORS);
            {$IFNDEF FPC}
            if not DSS.ADiakoptics then
            begin
                if not IsDLL then
                    ScriptEd.UpdateSummaryForm('1');
            end
            else
            begin
                if DSS.IsPrime then
                    if not IsDLL then
                        ScriptEd.UpdateSummaryForm('1');
            end;
            {$ENDIF}
        end;
{$ENDIF} // DSS_CAPI_PM


    except

        On E: Exception do
        begin
            DoSimpleMsg('Error Encountered in Solve: ' + E.Message, 482);
            DSS.SolutionAbort := TRUE;
        end;

    end;

end;

// ===========================================================================================
function TSolutionObj.Converged: Boolean;

var
    i: Integer;
    VMag: Double;
begin

// base convergence on voltage magnitude
    MaxError := 0.0;
    for i := 1 to DSS.ActiveCircuit.NumNodes do
    begin
        VMag := Cabs(NodeV^[i]);

        { If base specified, use it; otherwise go on present magnitude  }
        if NodeVbase^[i] > 0.0 then
            ErrorSaved^[i] := Abs(Vmag - VmagSaved^[i]) / NodeVbase^[i]
        else
        if Vmag <> 0.0 then
            ErrorSaved^[i] := Abs(1.0 - VmagSaved^[i] / Vmag);

        VMagSaved^[i] := Vmag;  // for next go-'round
        MaxError := Math.Max(MaxError, ErrorSaved^[i]);  // update max error
    end;

    
    if (MaxError <= ConvergenceTolerance) and (not IsNaN(MaxError)) then
        Result := TRUE
    else
        Result := FALSE;


    ConvergedFlag := Result;
end;


// ===========================================================================================
procedure TSolutionObj.GetSourceInjCurrents;

// Add in the contributions of all source type elements to the global solution vector InjCurr

var
    pElem: TDSSCktElement;

begin

    with DSS.ActiveCircuit do
    begin

        pElem := Sources.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
                pElem.InjCurrents; // uses NodeRef to add current into InjCurr Array;
            pElem := Sources.Next;
        end;

    end;

end;

// ===========================================================================================
procedure TSolutionObj.SetGeneratorDispRef;

// Set the global generator dispatch reference

begin
    with DSS.ActiveCircuit do
        case Dynavars.SolutionMode of

            TSolveMode.SNAPSHOT:
                GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor;
            TSolveMode.YEARLYMODE:
                GeneratorDispatchReference := DefaultGrowthFactor * DefaultHourMult.re;
            TSolveMode.DAILYMODE:
                GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor * DefaultHourMult.re;
            TSolveMode.DUTYCYCLE:
                GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor * DefaultHourMult.re;
            TSolveMode.GENERALTIME:
                GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor * DefaultHourMult.re;
            TSolveMode.DYNAMICMODE:
                GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor;
            TSolveMode.HARMONICMODE:
                GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor;
            TSolveMode.MONTECARLO1:
                GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor;
            TSolveMode.MONTECARLO2:
                GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor * DefaultHourMult.re;
            TSolveMode.MONTECARLO3:
                GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor * DefaultHourMult.re;
            TSolveMode.PEAKDAY:
                GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor * DefaultHourMult.re;
            TSolveMode.LOADDURATION1:
                GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor * DefaultHourMult.re;
            TSolveMode.LOADDURATION2:
                GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor * DefaultHourMult.re;
            TSolveMode.DIRECT:
                GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor;
            TSolveMode.MONTEFAULT:
                GeneratorDispatchReference := 1.0;  // Monte Carlo Fault Cases solve  at peak load only base case
            TSolveMode.FAULTSTUDY:
                GeneratorDispatchReference := 1.0;
            TSolveMode.AUTOADDFLAG:
                GeneratorDispatchReference := DefaultGrowthFactor;   // peak load only
            TSolveMode.HARMONICMODET:
                GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor * DefaultHourMult.re;
        else
            DoSimpleMsg('Unknown solution mode.', 483);
        end;
end;
// ===========================================================================================
procedure TSolutionObj.SetGeneratordQdV;

var
    pGen: TGeneratorObj;
    Did_One: Boolean;
    GenDispSave: Double;

begin
    Did_One := FALSE;

     // Save the generator dispatch level and set on high enough to
     // turn all generators on
    GenDispSave := DSS.ActiveCircuit.GeneratorDispatchReference;
    DSS.ActiveCircuit.GeneratorDispatchReference := 1000.0;

    with DSS.ActiveCircuit do
    begin

        pGen := Generators.First;
        while pGen <> NIL do
        begin

            if pGen.Enabled then
            begin

              // for PV generator models only ...
                if pGen.genModel = 3 then
                begin

                    pGen.InitDQDVCalc;

                   // solve at base var setting
                    Iteration := 0;
                    repeat
                        Inc(Iteration);
                        ZeroInjCurr;
                        GetSourceInjCurrents;
                        pGen.InjCurrents;   // get generator currents with nominal vars
                        SolveSystem(NodeV);
                    until Converged or (Iteration >= Maxiterations);

                    pGen.RememberQV;  // Remember Q and V
                    pGen.BumpUpQ;

                   // solve after changing vars
                    Iteration := 0;
                    repeat
                        Inc(Iteration);
                        ZeroInjCurr;
                        GetSourceInjCurrents;
                        pGen.InjCurrents;   // get generator currents with nominal vars
                        SolveSystem(NodeV);
                    until Converged or (Iteration >= Maxiterations);

                    pGen.CalcdQdV; // bssed on remembered Q and V and present values of same
                    pGen.ResetStartPoint;

                    Did_One := TRUE;
                end;
            end;
            pGen := Generators.Next;
        end;

    end;

     // Restore generator dispatch reference
    DSS.ActiveCircuit.GeneratorDispatchReference := GenDispSave;
    try
        if Did_One        // Reset Initial Solution
        then
            SolveZeroLoadSnapShot;
    except
        ON E: EEsolv32Problem do
        begin
            DoSimpleMsg('From SetGenerator DQDV, SolveZeroLoadSnapShot: ' + CRLF + E.Message + CheckYMatrixforZeroes(DSS), 7071);
            raise ESolveError.Create('Aborting');
        end;
    end;

end;

// ===========================================================================================
procedure TSolutionObj.DoNormalSolution;

{ Normal fixed-point solution

   Vn+1 = [Y]-1 Injcurr

   Where Injcurr includes only PC elements  (loads, generators, etc.)
   i.e., the shunt elements.

   Injcurr are the current injected INTO the NODE
        (need to reverse current direction for loads)
}

begin


    Iteration := 0;

 {**** Main iteration loop ****}
    with DSS.ActiveCircuit do
        repeat
            Inc(Iteration);

            if LogEvents then
                LogThisEvent(DSS, 'Solution Iteration ' + IntToStr(Iteration));

    { Get injcurrents for all PC devices  }
            ZeroInjCurr;
            GetSourceInjCurrents;  // sources
            GetPCInjCurr;  // Get the injection currents from all the power conversion devices and feeders

       // The above call could change the primitive Y matrix, so have to check
            if SystemYChanged {$IFDEF DSS_CAPI_INCREMENTAL_Y}or (DSS.ActiveCircuit.IncrCktElements.Count <> 0){$ENDIF} then
            begin
                BuildYMatrix(DSS, WHOLEMATRIX, FALSE);  // Does not realloc V, I
            end;
            if UseAuxCurrents then
                AddInAuxCurrents(NORMALSOLVE);

      // Solve for voltages                      {Note:NodeV[0] = 0 + j0 always}
            if LogEvents then
                LogThisEvent(DSS, 'Solve Sparse Set DoNormalSolution ...');
            SolveSystem(NodeV);
            LoadsNeedUpdating := FALSE;

        until (Converged and (Iteration >= MinIterations)) or (Iteration >= MaxIterations);

end;


// ===========================================================================================
procedure TSolutionObj.DoNewtonSolution;

{ Newton Iteration

   Vn+1 =  Vn - [Y]-1 Termcurr

   Where Termcurr includes currents from all elements and we are
   attempting to get the  currents to sum to zero at all nodes.

   Termcurr is the sum of all currents going INTO THE TERMINALS of
   the elements.

   For PD Elements, Termcurr = Yprim*V

   For Loads, Termcurr = (Sload/V)*
   For Generators, Termcurr = -(Sgen/V)*

}

var
    i: Integer;

begin

    with DSS.ActiveCircuit do
    begin
        ReAllocMem(dV, SizeOf(dV^[1]) * (NumNodes + 1)); // Make sure this is always big enough

        if ControlIteration = 1 then
            GetPCInjCurr;  // Update the load multipliers for this solution

        Iteration := 0;
        repeat
            Inc(Iteration);
            Inc(SolutionCount);    // SumAllCurrents Uses ITerminal  So must force a recalc

        // Get sum of currents at all nodes for all  devices
            ZeroInjCurr;
            SumAllCurrents;

           // Call to current calc could change YPrim for some devices
            if SystemYChanged then
            begin
                BuildYMatrix(DSS, WHOLEMATRIX, FALSE);   // Does not realloc V, I
            end;

            if UseAuxCurrents then
                AddInAuxCurrents(NEWTONSOLVE);

        // Solve for change in voltages
            SolveSystem(dV);

            LoadsNeedUpdating := FALSE;

         // Compute new guess at voltages
            for i := 1 to NumNodes do     // 0 node is always 0
                with NodeV^[i] do
                begin
                    re := re - dV^[i].re;
                    im := im - dV^[i].im;
                end;

        until (Converged and (Iteration >= MinIterations)) or (Iteration >= MaxIterations);
    end;
end;


// ===========================================================================================
procedure TSolutionObj.DoPFLOWsolution;


begin

    Inc(SolutionCount);    //Unique number for this solution

    if VoltageBaseChanged then
        InitializeNodeVbase(DSS); // for convergence test

    if not SolutionInitialized then
    begin

        if DSS.ActiveCircuit.LogEvents then
            LogThisEvent(DSS, 'Initializing Solution');
        try
        //SolveZeroLoadSnapShot;
            SolveYDirect;  // 8-14-06 This should give a better answer than zero load snapshot
        except
            ON E: EEsolv32Problem do
            begin
                DoSimpleMsg('From DoPFLOWsolution.SolveYDirect: ' + CRLF + E.Message + CheckYMatrixforZeroes(DSS), 7072);
                raise ESolveError.Create('Aborting');
            end;
        end;
        if DSS.SolutionAbort then
            Exit; // Initialization can result in abort

        try
            SetGeneratordQdV;  // Set dQdV for Model 3 generators
        except
            ON E: EEsolv32Problem do
            begin
                DoSimpleMsg('From DoPFLOWsolution.SetGeneratordQdV: ' + CRLF + E.Message + CheckYMatrixforZeroes(DSS), 7073);
                raise ESolveError.Create('Aborting');
            end;
        end;

        { The above resets the active sparse set to hY }
        SolutionInitialized := TRUE;
    end;


    case Algorithm of
        NORMALSOLVE:
            DoNormalSolution;
        NEWTONSOLVE:
            DoNewtonSolution;
    end;

    DSS.ActiveCircuit.Issolved := ConvergedFlag;
    LastSolutionWasDirect := FALSE;

end;

// ===========================================================================================
function TSolutionObj.SolveZeroLoadSnapShot: Integer;

// Solve without load for initialization purposes;

begin
    Result := 0;

    if SystemYChanged or SeriesYInvalid then
    begin
        BuildYMatrix(DSS, SERIESONLY, TRUE);   // Side Effect: Allocates V
    end;
    Inc(SolutionCount);    //Unique number for this solution

    ZeroInjCurr;   // Side Effect: Allocates InjCurr
    GetSourceInjCurrents;    // Vsource, Isource and VCCS only

    {Make the series Y matrix the active matrix}
    if hYseries = 0 then
        raise EEsolv32Problem.Create('Series Y matrix not built yet in SolveZeroLoadSnapshot.');
    hY := hYseries;

    if DSS.ActiveCircuit.LogEvents then
        LogThisEvent(DSS, 'Solve Sparse Set ZeroLoadSnapshot ...');

    SolveSystem(NodeV);  // also sets voltages in radial part of the circuit if radial solution

    { Reset the main system Y as the solution matrix}
    if (hYsystem > 0) and not DSS.SolutionAbort then
        hY := hYsystem;
end;

// ===========================================================================================
procedure TSolutionObj.SetVoltageBases;

// Set voltage bases using voltage at first node (phase) of a bus

var
    i: Integer;
    bZoneCalc, bZoneLock: Boolean;

begin

    try
    // don't allow the meter zones to auto-build in this load flow solution, because the
    // voltage bases are not available yet

        bZoneCalc := DSS.ActiveCircuit.MeterZonesComputed;
        bZoneLock := DSS.ActiveCircuit.ZonesLocked;
        DSS.ActiveCircuit.MeterZonesComputed := TRUE;
        DSS.ActiveCircuit.ZonesLocked := TRUE;

        SolveZeroLoadSnapShot;

        with DSS.ActiveCircuit do
            for i := 1 to NumBuses do
                with Buses^[i] do
                    kVBase := NearestBasekV(DSS, Cabs(NodeV^[GetRef(1)]) * 0.001732) / SQRT3;  // l-n base kV

        InitializeNodeVbase(DSS);      // for convergence test

        DSS.ActiveCircuit.Issolved := TRUE;

    // now build the meter zones
        DSS.ActiveCircuit.MeterZonesComputed := bZoneCalc;
        DSS.ActiveCircuit.ZonesLocked := bZoneLock;
        DSS.ActiveCircuit.DoResetMeterZones;

    except
        ON E: EEsolv32Problem do
        begin
            DoSimpleMsg('From SetVoltageBases.SolveZeroLoadSnapShot: ' + CRLF + E.Message + CheckYMatrixforZeroes(DSS), 7075);
            raise ESolveError.Create('Aborting');
        end;
    end;

end;

procedure TSolutionObj.SnapShotInit;

begin

    SetGeneratorDispRef;
    ControlIteration := 0;
    ControlActionsDone := FALSE;
    MostIterationsDone := 0;
    LoadsNeedUpdating := TRUE;  // Force the loads to update at least once

end;

procedure TSolutionObj.CheckControls;

begin
    if ControlIteration < MaxControlIterations then
    begin
        if ConvergedFlag then
        begin
            if DSS.ActiveCircuit.LogEvents then
                LogThisEvent(DSS, 'Control Iteration ' + IntToStr(ControlIteration));
            Sample_DoControlActions;
            Check_Fault_Status;
        end
        else
            ControlActionsDone := TRUE; // Stop solution process if failure to converge
    end;

    if SystemYChanged {$IFDEF DSS_CAPI_INCREMENTAL_Y}or (DSS.ActiveCircuit.IncrCktElements.Count <> 0){$ENDIF} then
    begin
        BuildYMatrix(DSS, WHOLEMATRIX, FALSE); // Rebuild Y matrix, but V stays same
    end;
end;

// ===========================================================================================
function TSolutionObj.SolveSnap: Integer;  // solve for now once

var
    TotalIterations: Integer;

begin
    SnapShotInit;
    TotalIterations := 0;
   {$IFDEF MSWINDOWS}
    QueryPerformanceCounter(SolveStartTime);
{$ENDIF}
    repeat

        Inc(ControlIteration);

        Result := SolveCircuit;  // Do circuit solution w/o checking controls
       {Now Check controls}
{$IFDEF DLL_ENGINE}
        Fire_CheckControls;
{$ENDIF}
        CheckControls;

       {For reporting max iterations per control iteration}
        if Iteration > MostIterationsDone then
            MostIterationsDone := Iteration;

        TotalIterations := TotalIterations + Iteration;

    until ControlActionsDone or (ControlIteration >= MaxControlIterations);

    if not ControlActionsDone and (ControlIteration >= MaxControlIterations) then
    begin
        DoSimpleMsg('Warning Max Control Iterations Exceeded. ' + CRLF + 'Tip: Show Eventlog to debug control settings.', 485);
        DSS.SolutionAbort := TRUE;   // this will stop this message in dynamic power flow modes
    end;

    if DSS.ActiveCircuit.LogEvents then
        LogThisEvent(DSS, 'Solution Done');

{$IFDEF DLL_ENGINE}
    Fire_StepControls;
{$ENDIF}
{$IFDEF MSWINDOWS}
    QueryPerformanceCounter(SolveEndTime);
{$ENDIF}
    Solve_Time_Elapsed := ((SolveEndtime - SolveStartTime) / CPU_Freq) * 1000000;
    Iteration := TotalIterations;  { so that it reports a more interesting number }

end;

// ===========================================================================================
function TSolutionObj.SolveDirect: Integer;  // solve for now once, direct solution

begin
    Result := 0;

    LoadsNeedUpdating := TRUE;  // Force possible update of loads and generators
   {$IFDEF MSWINDOWS}
    QueryPerformanceCounter(SolveStartTime);
{$ENDIF}

    if SystemYChanged {$IFDEF DSS_CAPI_INCREMENTAL_Y}or (DSS.ActiveCircuit.IncrCktElements.Count <> 0){$ENDIF} then
    begin
        BuildYMatrix(DSS, WHOLEMATRIX, TRUE);   // Side Effect: Allocates V
    end;

    Inc(SolutionCount);   // Unique number for this solution

    ZeroInjCurr;   // Side Effect: Allocates InjCurr
    GetSourceInjCurrents;

   // Pick up PCELEMENT injections for Harmonics mode and Dynamics mode
   // Ignore these injections for powerflow; Use only admittance in Y matrix
    if IsDynamicModel or IsHarmonicModel then
        GetPCInjCurr;

    if SolveSystem(NodeV) = 1   // Solve with Zero injection current
    then
    begin
        DSS.ActiveCircuit.IsSolved := TRUE;
        ConvergedFlag := TRUE;
    end;

   {$IFDEF MSWINDOWS}
    QueryPerformanceCounter(SolveEndTime);
{$ENDIF}
    Solve_Time_Elapsed := ((SolveEndtime - SolveStartTime) / CPU_Freq) * 1000000;
    Total_Time_Elapsed := Total_Time_Elapsed + Solve_Time_Elapsed;
    Iteration := 1;
    LastSolutionWasDirect := TRUE;

end;


function TSolutionObj.SolveCircuit: Integer;
begin
    Result := 0;
    if LoadModel = ADMITTANCE then
        try
            SolveDirect     // no sense horsing around when it's all admittance
        except
            ON E: EEsolv32Problem do
            begin
                DoSimpleMsg('From SolveSnap.SolveDirect: ' + CRLF + E.Message + CheckYMatrixforZeroes(DSS), 7075);
                raise ESolveError.Create('Aborting');
            end;
        end
    else
    begin
        try
            if SystemYChanged then
            begin
                BuildYMatrix(DSS, WHOLEMATRIX, TRUE);   // Side Effect: Allocates V
            end;
            DoPFLOWsolution;
        except
            ON E: EEsolv32Problem do
            begin
                DoSimpleMsg('From SolveSnap.DoPflowSolution: ' + CRLF + E.Message + CheckYMatrixforZeroes(DSS), 7074);
                raise ESolveError.Create('Aborting');
            end;
        end
    end;

end;

// ===========================================================================================
procedure TSolutionObj.ZeroInjCurr;
var
    I: Integer;
begin
    for i := 0 to DSS.ActiveCircuit.NumNodes do
        Currents^[i] := CZERO;
end;
// ===========================================================================================
procedure TSolutionObj.Upload2IncMatrix;
begin
  // Uploads the values to the incidence matrix
    IncMat.insert((ActiveIncCell[0] - 1), (ActiveIncCell[1] - 2), ActiveIncCell[2]);
    ActiveIncCell[2] := -1;
end;
// ===========================================================================================
procedure TSolutionObj.AddLines2IncMatrix;
var
    LineBus: String;
    elem: TLineObj;
    TermIdx,
    BusdotIdx: Integer;
    EndFlag: Boolean;
begin
    // This rouitne adds the Lines to the incidence matrix vectors
    with DSS.ActiveCircuit do
    begin
        elem := Lines.First;
        while elem <> NIL do
        begin
            if elem.Enabled then
            begin
                ActiveIncCell[2] := 1;
                inc(temp_counter);
                setlength(Inc_Mat_Rows, temp_counter);
                Inc_Mat_Rows[temp_counter - 1] := 'Line.' + elem.Name;
                for TermIdx := 1 to 2 do
                begin
                    LineBus := elem.GetBus(TermIdx);
                    BusdotIdx := ansipos('.', LineBus);
                    if BusdotIdx <> 0 then
                        LineBus := Copy(LineBus, 0, BusdotIdx - 1);  // removes the dot from the Bus Name
                    // Evaluates the position of the Bus in the array
                    ActiveIncCell[1] := 1;
                    EndFlag := TRUE;
                    while (ActiveIncCell[1] <= NumBuses) and (EndFlag) do
                    begin
                        if LineBus = BusList.NameOfIndex(ActiveIncCell[1]) then
                            EndFlag := FALSE;
                        ActiveIncCell[1] := ActiveIncCell[1] + 1;
                    end;
                    Upload2IncMatrix;
                end;
                inc(ActiveIncCell[0]);
            end;
            elem := Lines.Next;
        end;
    end;
end;
// ===========================================================================================
procedure TSolutionObj.AddXfmr2IncMatrix;
var
    LineBus: String;
    elem: TTransfObj;
    TermIdx,
    BusdotIdx: Integer;
    EndFlag: Boolean;
    lst: TDSSPointerList;
begin
// This routine adds the Transformers to the incidence matrix vectors
    with DSS.ActiveCircuit do
    begin
        lst := DSS.ActiveCircuit.Transformers;
        elem := lst.First;
        while elem <> NIL do
        begin
            if elem.Enabled then
            begin
                ActiveIncCell[2] := 1;
                inc(temp_counter);
                setlength(Inc_Mat_Rows, temp_counter);
                Inc_Mat_Rows[temp_counter - 1] := 'Transformer.' + elem.Name;
                for TermIdx := 1 to elem.NumberOfWindings do
                begin
                    LineBus := elem.GetBus(TermIdx);
                    BusdotIdx := ansipos('.', LineBus);
                    if BusdotIdx <> 0 then
                        LineBus := Copy(LineBus, 0, BusdotIdx - 1);  // removes the dot from the Bus Name
            // Evaluates the position of the Bus in the array
                    ActiveIncCell[1] := 1;
                    EndFlag := TRUE;
                    while (ActiveIncCell[1] <= NumBuses) and (EndFlag) do
                    begin
                        if LineBus = BusList.NameOfIndex(ActiveIncCell[1]) then
                            EndFlag := FALSE;
                        ActiveIncCell[1] := ActiveIncCell[1] + 1;
                    end;
                    Upload2IncMatrix;
                end;
                inc(ActiveIncCell[0]);
            end;
            elem := lst.Next;
        end;
    end;
end;
// ===========================================================================================
procedure TSolutionObj.AddSeriesCap2IncMatrix;
var
    CapBus: String;
    elem: TCapacitorObj;
    lst: TDSSPointerList;
    CapTermIdx,
    BusdotIdx: Integer;
    CapEndFlag: Boolean;
begin
// This rouitne adds the series capacitors to the incidence matrix vectors
    with DSS.ActiveCircuit do
    begin
        lst := ShuntCapacitors;
        elem := lst.First;
        while elem <> NIL do
        begin
            if elem.NumTerminals > 1 then
            begin
                if elem.Enabled then
                begin
                    inc(temp_counter);
                    setlength(Inc_Mat_Rows, temp_counter);
                    Inc_Mat_Rows[temp_counter - 1] := 'Capacitor.' + elem.Name;
                    ActiveIncCell[2] := 1;
                    for CapTermIdx := 1 to 2 do
                    begin
                        CapBus := elem.GetBus(CapTermIdx);
                        BusdotIdx := ansipos('.', CapBus);
                        if BusdotIdx <> 0 then
                            CapBus := Copy(CapBus, 0, BusdotIdx - 1);  // removes the dot from the Bus Name
            // Evaluates the position of the Bus in the array
                        ActiveIncCell[1] := 1;
                        CapEndFlag := TRUE;
                        while (ActiveIncCell[1] <= NumBuses) and (CapEndFlag) do
                        begin
                            if CapBus = BusList.NameOfIndex(ActiveIncCell[1]) then
                                CapEndFlag := FALSE;
                            ActiveIncCell[1] := ActiveIncCell[1] + 1;
                        end;
                        Upload2IncMatrix;
                    end;
                    inc(ActiveIncCell[0]);
                end;
            end;
            elem := lst.Next;
        end;
    end;
end;
// ===========================================================================================
procedure TSolutionObj.AddSeriesReac2IncMatrix;
var
    RBus: String;
    elem,
    DevClassIndex: Integer;
    TermIdx,
    BusdotIdx: Integer;
    EndFlag: Boolean;
begin
// This rouitne adds the series reactors to the incidence matrix vectors
    with DSS.ActiveCircuit do
    begin
        DevClassIndex := DSS.ClassNames.Find('reactor');
        DSS.LastClassReferenced := DevClassIndex;
        DSS.ActiveDSSClass := DSS.DSSClassList.Get(DSS.LastClassReferenced);
        elem := DSS.ActiveDSSClass.First;
        while elem <> 0 do
        begin
            RBus := ActiveCktElement.GetBus(2);
            BusdotIdx := ansipos('.0', RBus);
            if BusdotIdx = 0 then
            begin
                inc(temp_counter);
                setlength(Inc_Mat_Rows, temp_counter);
                Inc_Mat_Rows[temp_counter - 1] := 'Reactor.' + ActiveCktElement.Name;
                ActiveIncCell[2] := 1;
                for TermIdx := 1 to 2 do
                begin
                    RBus := ActiveCktElement.GetBus(TermIdx);
                    BusdotIdx := ansipos('.', RBus);
                    if BusdotIdx <> 0 then
                        RBus := Copy(RBus, 0, BusdotIdx - 1);  // removes the dot from the Bus Name
          // Evaluates the position of the Bus in the array
                    ActiveIncCell[1] := 1;
                    EndFlag := TRUE;
                    while (ActiveIncCell[1] <= NumBuses) and (EndFlag) do
                    begin
                        if RBus = BusList.NameOfIndex(ActiveIncCell[1]) then
                            EndFlag := FALSE;
                        ActiveIncCell[1] := ActiveIncCell[1] + 1;
                    end;
                    Upload2IncMatrix;
                end;
            end;
            elem := DSS.ActiveDSSClass.Next;
            inc(ActiveIncCell[0]);
        end;
    end;
end;
//*********Routine for extracting the Branch to Node incidence matrix***********
//*     The order depends on the way the lines, xfmr, series cap and reactors  *
//******************************************************************************
procedure TSolutionObj.Calc_Inc_Matrix;
begin
  // If the sparse matrix obj doesn't exists creates it, otherwise deletes the content
    if IncMat = NIL then
        IncMat := Tsparse_matrix.Create
    else
        IncMat.reset;

    if DSS.ActiveCircuit <> NIL then
        with DSS.ActiveCircuit do
        begin
            temp_counter := 0;
            ActiveIncCell[0] := 1;           // Activates row 1 of the incidence matrix
      // Now we proceed to evaluate the link branches
            AddLines2IncMatrix;      // Includes the Lines
            AddXfmr2IncMatrix;       // Includes the Xfmrs
            AddSeriesCap2IncMatrix;  // Includes Series Cap
            AddSeriesReac2IncMatrix; // Includes Series Reactors
            DSS.IncMat_Ordered := FALSE;
        end;
end;

{*******************************************************************************
* This function delivers the Row index connected to the Column at the input    *
*                   Inside the B2N incidence Matrix                            *
********************************************************************************}
function TSolutionObj.get_IncMatrix_Row(Col: Integer): Integer;
var
    Tflag: Boolean;
    idx_1: Integer;
begin
    Result := -1;
    Tflag := TRUE;
    for idx_1 := 1 to (IncMat.NZero - 1) do    //Looks for the Column in the IncMatrix
    begin
        if (IncMat.data[idx_1][1] = Col) and Tflag then
        begin
            Result := IncMat.data[idx_1][0];
            Tflag := FALSE;
        end;
    end;
end;

{*******************************************************************************
* This function delivers the Column index connected to the Row at the input    *
*                   Inside the B2N incidence Matrix                            *
********************************************************************************}
function TSolutionObj.get_IncMatrix_Col(Row: Integer): Integer;
var
    Tflag: Boolean;
    Idx_1: Integer;
begin
    Result := -1;
    Tflag := TRUE;    // Detection Flag
    for Idx_1 := 1 to (IncMat.NZero - 1) do    //Looks for the row in the IncMatrix
    begin
        if (IncMat.data[Idx_1][0] = Row) and Tflag then
        begin
            setlength(Active_Cols, 2);
            setlength(Active_Cols_Idx, 2);
            Active_Cols[0] := IncMat.data[Idx_1][1];     //Stores the indexes of both columns for the link branch
            Active_Cols[1] := IncMat.data[Idx_1 + 1][1]; //In case they need to be used in the future by the caller
            Active_Cols_Idx[0] := IncMat.data[Idx_1 - 1][2]; //Stores the indexes of both columns for the link branch
            Active_Cols_Idx[1] := IncMat.data[Idx_1][2];     //In case they need to be used in the future by the caller
            Result := IncMat.data[Idx_1][1];
            Tflag := FALSE;
        end;
    end;
end;

//*********Routine for extracting the Branch to Node incidence matrix***********
//*     Organized hierarchically. This routine also calculates the             *
//*     Levels vector for defining the proximity of the bus to the circuit's   *
//*     Backbone. To do it, this routine uses the CktTree class                *
//******************************************************************************
procedure TSolutionObj.Calc_Inc_Matrix_Org;

var
    pdElem: TPDElement;
    topo: TCktTree;
//  TreeNm,                                           // For debugging
//  FileRoot,                                         // For debugging
    PDE_Name: String;                            // Name of the PDElement
    PDE_Buses: array of String;                   // Buses of the PDElement
    Temp_Array: array of Integer;                  // Local Shared variable
    nLevels,                                            // Current number of levels for the active Bus
    i,                                                  // Default counter
    j,                                                  // Default counter
    j2,                                                 // Default counter
    ZeroLevel,                                          // Number of Zero level Buses
    BusdotIdx,
    row,
    col,
    val,                                          // Local Shared variable
    nPDE: Integer;                           // PDElements index
begin
    try
        if DSS.ActiveCircuit <> NIL then
        begin
//      TreeNm := FileRoot + 'TopoTree_Cols.csv';   // For debuging
            topo := DSS.ActiveCircuit.GetTopology;
            nLevels := 0;
            nPDE := 0;
            setlength(Inc_Mat_Cols, 0);
      //Init the spaser matrix
            if IncMat = NIL then
                IncMat := Tsparse_matrix.Create
            else
                IncMat.reset;

            ActiveIncCell[0] := -1;           // Activates row 1 of the incidence matrix
            if Assigned(topo) then
            begin
                PDElem := topo.First;
                while Assigned(PDElem) do
                begin
                    nLevels := topo.Level;
                    PDE_Name := PDElem.ParentClass.Name + '.' + PDElem.Name;
//******************Gets the buses to which the PDE is connected****************
                    with DSS.ActiveCircuit do
                    begin
                        DSS.ActiveCircuit.SetElementActive(PDE_Name);
                        SetLength(PDE_Buses, ActiveCktElement.Nterms);
                        for i := 1 to ActiveCktElement.Nterms do
                        begin
                            PDE_Buses[i - 1] := ActiveCktElement.GetBus(i);
                            BusdotIdx := ansipos('.', PDE_Buses[i - 1]);
                            if BusdotIdx <> 0 then
                                PDE_Buses[i - 1] := Copy(PDE_Buses[i - 1], 0, BusdotIdx - 1);  // removes the dot from the Bus Name
                        end;
                        if length(Inc_Mat_Cols) = 0 then  //First iteration so the Cols array will be loaded
                        begin
                            setlength(Inc_Mat_Cols, 1);
                            setlength(Inc_Mat_Levels, 1);
                            Inc_Mat_Cols[0] := PDE_Buses[0];
                            Inc_Mat_levels[0] := nLevels;
                        end
                        else                               //The Cols array is populated with something
                        begin
                            inc(nPDE);
                            setlength(Inc_Mat_Rows, nPDE);
                            Inc_Mat_Rows[nPDE - 1] := PDE_Name;
                            for j := 0 to ActiveCktElement.Nterms - 1 do
                            begin
                                row := ActiveIncCell[0];                 //Sets the row
                                BusdotIdx := -1;               // Flag to not create a new variable
                                for i := 0 to length(Inc_Mat_Cols) - 1 do   // Checks if the bus already exists in the Cols array
                                    if Inc_Mat_Cols[i] = PDE_Buses[j] then
                                        BusdotIdx := i;
                                if BusdotIdx >= 0 then
                                    col := BusdotIdx   //Sets the Col
                                else
                                begin
                                    setlength(Inc_Mat_Cols, length(Inc_Mat_Cols) + 1);
                                    setlength(Inc_Mat_levels, length(Inc_Mat_levels) + 1);
                                    Inc_Mat_Cols[length(Inc_Mat_Cols) - 1] := PDE_Buses[j];
                                    Inc_Mat_levels[length(Inc_Mat_Cols) - 1] := nLevels;
                                    col := length(Inc_Mat_Cols) - 1; //Sets the Col
                                end;
                                if j = 0 then
                                    val := 1 //Sets the value
                                else
                                    val := -1;
                                IncMat.insert(row, col, val);
                            end;
                        end;
                    end;
                    inc(ActiveIncCell[0]);
                    PDElem := topo.GoForward;
                end;
            end;
{*******************************************************************************
*   Now the levels array needs to be reprocessed to get the 0 level buses,     *
*   they are on a continuous path from the feeder head to the feeder end       *
********************************************************************************}
            BusdotIdx := MaxIntValue(Inc_Mat_levels);
            for i := 0 to length(Inc_Mat_levels) do
                if Inc_Mat_levels[i] = BusdotIdx then
                    nLevels := i;
            for j := 1 to BusdotIdx - 1 do
            begin
                for i := 0 to nLevels do
                begin
                    if Inc_Mat_levels[i] = j then
                        ZeroLevel := i;
                end;
                Inc_Mat_levels[ZeroLevel] := 0;
            end;
//**********Normalize the branches of the level between zero level buses********
            BusdotIdx := 0;
            j := 0;
            ZeroLevel := 0;
            SetLength(Temp_Array, 0);
            for i := 0 to high(Inc_Mat_levels) do
            begin
                if (Inc_Mat_levels[i] = 0) then
                begin
                    if length(Temp_Array) > 0 then    // The array subset is large enough for
                    begin                             //Normalizing it
                        BusdotIdx := MinIntValue(Temp_Array) - 1;
                        for j2 := ZeroLevel to (length(Temp_Array) + ZeroLevel - 1) do
                            Inc_Mat_levels[j2] := Inc_Mat_levels[j2] - BusdotIdx;
                        SetLength(Temp_Array, 0);
                    end;
                    ZeroLevel := i + 1;
                end
                else
                begin
                    setlength(Temp_Array, (length(Temp_Array) + 1));
                    Temp_Array[High(Temp_Array)] := Inc_Mat_levels[i];
                end;
            end;
//************Verifies is something else was missing at the end*****************
            if (ZeroLevel < (length(Inc_Mat_levels) - 1)) then
            begin
                BusdotIdx := 0;                                                // Counter for defining the level
                j := 0;                                                // Stores the previous value (shift reg)
                for j2 := ZeroLevel to High(Inc_Mat_levels) do
                begin
                    if Inc_Mat_levels[j2] >= j then
                        inc(BusdotIdx)
                    else
                    begin
                        ActiveIncCell[1] := get_IncMatrix_Row(j2);                //Looks for the Column in the IncMatrix
                        if ActiveIncCell[1] < 0 then                                //Checks if the col was located (just in case)
                            BusdotIdx := 1
                        else
                        begin
                            ActiveIncCell[2] := get_IncMatrix_Col(ActiveIncCell[1]);  //Looks for the row in the IncMatrix
                            if Active_Cols[0] = j2 then
                                BusdotIdx := Inc_Mat_levels[Active_Cols[1]] + 1
                            else
                                BusdotIdx := Inc_Mat_levels[ActiveIncCell[2]] + 1;
                        end;
                    end;
                    j := Inc_Mat_levels[j2];
                    Inc_Mat_levels[j2] := BusdotIdx;
                end;
            end;
            DSS.IncMat_Ordered := TRUE;
        end;
    finally

    end;
end;

{*******************************************************************************
*   This routine evaluates if the current location is the best or if its       *
*   Necessary to move back one PDE just to cover a wider area                  *
********************************************************************************}
function TSolutionObj.CheckLocationIdx(Idx: Integer): Integer;
begin
    if Inc_Mat_Levels[Idx - 1] = 0 then
        Result := idx - 1
    else
        Result := idx;
end;
// ===========================================================================================
procedure TSolutionObj.GetPCInjCurr;
var
    pElem: TDSSCktElement;

{ Get inj currents from all enabled PC devices }

begin

    with DSS.ActiveCircuit do
    begin
        pElem := PCElements.First;
        while pElem <> NIL do
        begin
            with pElem do
                if Enabled then
                    InjCurrents; // uses NodeRef to add current into InjCurr Array;
            pElem := PCElements.Next;
        end;
    end;

end;

procedure TSolutionObj.DumpProperties(F: TFileStream; complete: Boolean);

var
    i, j: Integer;

   // for dumping the matrix in compressed columns
    p: Longword;
    hY: NativeUInt;
    nBus, nNZ: Longword;
    ColPtr, RowIdx: array of Longword;
    cVals: array of Complex;
begin

    FSWriteln(F, '! OPTIONS');

  // Inherited DumpProperties(F,Complete);

    FSWriteln(F, '! NumNodes = ', IntToStr(DSS.ActiveCircuit.NumNodes));

    {WITH ParentClass Do
     FOR i := 1 to NumProperties Do
     Begin
        FSWriteln(F,'Set ',PropertyName^[i],'=',PropertyValue^[i]);
     End;
     }
    FSWriteln(F, 'Set Mode=', GetSolutionModeID(DSS));
    FSWriteln(F, 'Set ControlMode=', GetControlModeID(DSS));
    FSWriteln(F, 'Set Random=', GetRandomModeID(DSS));
    FSWriteln(F, 'Set hour=', IntToStr(DynaVars.intHour));
    FSWriteln(F, 'Set sec=', Format('%-g', [DynaVars.t]));
    FSWriteln(F, 'Set year=', IntToStr(Year));
    FSWriteln(F, 'Set frequency=', Format('%-g', [Frequency]));
    FSWriteln(F, 'Set stepsize=', Format('%-g', [DynaVars.h]));
    FSWriteln(F, 'Set number=', IntToStr(NumberOfTimes));
    FSWriteln(F, 'Set circuit=', DSS.ActiveCircuit.Name);
    FSWriteln(F, 'Set editor=', DefaultEditor);
    FSWriteln(F, 'Set tolerance=', Format('%-g', [ConvergenceTolerance]));
    FSWriteln(F, 'Set maxiterations=', IntToStr(MaxIterations));
    FSWriteln(F, 'Set miniterations=', IntToStr(MinIterations));
    FSWriteln(F, 'Set loadmodel=', GetLoadModel(DSS));

    FSWriteln(F, 'Set loadmult=', Format('%-g', [DSS.ActiveCircuit.LoadMultiplier]));
    FSWriteln(F, 'Set Normvminpu=', Format('%-g', [DSS.ActiveCircuit.NormalMinVolts]));
    FSWriteln(F, 'Set Normvmaxpu=', Format('%-g', [DSS.ActiveCircuit.NormalMaxVolts]));
    FSWriteln(F, 'Set Emergvminpu=', Format('%-g', [DSS.ActiveCircuit.EmergMinVolts]));
    FSWriteln(F, 'Set Emergvmaxpu=', Format('%-g', [DSS.ActiveCircuit.EmergMaxVolts]));
    FSWriteln(F, 'Set %mean=', Format('%-.4g', [DSS.ActiveCircuit.DefaultDailyShapeObj.Mean * 100.0]));
    FSWriteln(F, 'Set %stddev=', Format('%-.4g', [DSS.ActiveCircuit.DefaultDailyShapeObj.StdDev * 100.0]));
    FSWriteln(F, 'Set LDCurve=', DSS.ActiveCircuit.LoadDurCurve);  // Load Duration Curve
    FSWriteln(F, 'Set %growth=', Format('%-.4g', [((DSS.ActiveCircuit.DefaultGrowthRate - 1.0) * 100.0)]));  // default growth rate
    with DSS.ActiveCircuit.AutoAddObj do
    begin
        FSWriteln(F, 'Set genkw=', Format('%-g', [GenkW]));
        FSWriteln(F, 'Set genpf=', Format('%-g', [GenPF]));
        FSWriteln(F, 'Set capkvar=', Format('%-g', [Capkvar]));
        FSWrite(F, 'Set addtype=');
        case Addtype of
            GENADD:
                FSWriteln(F, 'generator');
            CAPADD:
                FSWriteln(F, 'capacitor');
        end;
    end;
    FSWrite(F, 'Set allowduplicates=');
    FSWriteln(F, StrYorN(DSS.ActiveCircuit.DuplicatesAllowed));
    FSWrite(F, 'Set zonelock=');
    FSWriteln(F, StrYorN(DSS.ActiveCircuit.ZonesLocked));
    FSWriteln(F, Format('Set ueweight=%8.2g', [DSS.ActiveCircuit.UEWeight]));
    FSWriteln(F, Format('Set lossweight=%8.2g', [DSS.ActiveCircuit.LossWeight]));
    FSWriteln(F, 'Set ueregs=', IntArraytoString(DSS.ActiveCircuit.UEregs, DSS.ActiveCircuit.NumUERegs));
    FSWriteln(F, 'Set lossregs=', IntArraytoString(DSS.ActiveCircuit.Lossregs, DSS.ActiveCircuit.NumLossRegs));
    FSWrite(F, 'Set voltagebases=(');  //  changes the default voltage base rules
    i := 1;
    with DSS.ActiveCircuit do
        while LegalVoltageBases^[i] > 0.0 do
        begin
            FSWrite(F, Format('%10.2g', [LegalVoltageBases^[i]]));
            inc(i);
        end;
    FSWriteln(F, ')');
    case Algorithm of
        NORMALSOLVE:
            FSWriteln(F, 'Set algorithm=normal');
        NEWTONSOLVE:
            FSWriteln(F, 'Set algorithm=newton');
    end;
    FSWrite(F, 'Set Trapezoidal=');
    FSWriteln(F, StrYorN(DSS.ActiveCircuit.TrapezoidalIntegration));
    FSWriteln(F, 'Set genmult=', Format('%-g', [DSS.ActiveCircuit.GenMultiplier]));

    FSWriteln(F, 'Set Basefrequency=', Format('%-g', [DSS.ActiveCircuit.Fundamental]));

    FSWrite(F, 'Set harmonics=(');  //  changes the default voltage base rules
    if DoAllHarmonics then
        FSWrite(F, 'ALL')
    else
        for i := 1 to HarmonicListSize do
            FSWrite(F, Format('%-g, ', [HarmonicList^[i]]));
    FSWriteln(F, ')');
    FSWriteln(F, 'Set maxcontroliter=', IntToStr(MaxControlIterations));
    FSWriteln(F);

    if Complete then
        with DSS.ActiveCircuit do
        begin

            hY := Solution.hY;

      // get the compressed columns out of KLU
            FactorSparseMatrix(hY); // no extra work if already done
            GetNNZ(hY, @nNZ);
            GetSize(hY, @nBus);
            SetLength(ColPtr, nBus + 1);
            SetLength(RowIdx, nNZ);
            SetLength(cVals, nNZ);
            GetCompressedMatrix(hY, nBus + 1, nNZ, @ColPtr[0], @RowIdx[0], @cVals[0]);

            FSWriteln(F, 'System Y Matrix (Lower Triangle by Columns)');
            FSWriteln(F);
            FSWriteln(F, '  Row  Col               G               B');
            FSWriteln(F);

      // traverse the compressed column format
            for j := 0 to nBus - 1 do
            begin /// the zero-based column
                for p := ColPtr[j] to ColPtr[j + 1] - 1 do
                begin
                    i := RowIdx[p];  // the zero-based row
                    FSWriteln(F, Format('[%4d,%4d] = %12.5g + j%12.5g', [i + 1, j + 1, cVals[p].re, cVals[p].im]));
                end;
            end;
        end;
end;

function TSolutionObj.VDiff(i, j: Integer): Complex;

begin
    Result := Csub(NodeV^[i], NodeV^[j]);  // V1-V2
end;


procedure TSolutionObj.WriteConvergenceReport(const Fname: String);
var
    i: Integer;
    F: TFileStream = nil;
    sout: String;
begin
    try
        F := TFileStream.Create(Fname, fmCreate);

        FSWriteln(F);
        FSWriteln(F, '-------------------');
        FSWriteln(F, 'Convergence Report:');
        FSWriteln(F, '-------------------');
        FSWriteln(F, '"Bus.Node", "Error", "|V|","Vbase"');
        with DSS.ActiveCircuit do
            for i := 1 to NumNodes do
                with MapNodeToBus^[i] do
                begin
                    WriteStr(sout, 
                        '"', pad((BusList.NameOfIndex(Busref) + '.' + IntToStr(NodeNum) + '"'), 18),
                        ', ', ErrorSaved^[i]: 10: 5,
                        ', ', VmagSaved^[i]: 14,
                        ', ', NodeVbase^[i]: 14
                    );
                    FSWrite(F, sout);
                    FSWriteln(F);
                end;

        FSWriteln(F);
        WriteStr(sout, 'Max Error = ', MaxError: 10: 5);
        FSWriteln(F, sout);

    finally
        FreeAndNil(F);
        FireOffEditor(DSS, Fname);

    end;

end;

// =========================================================================================== =
procedure TSolutionObj.SumAllCurrents;

var
    pelem: TDSSCktElement;

begin
    with  DSS.ActiveCircuit do
    begin
        pelem := CktElements.First;
        while pelem <> NIL do
        begin
            pelem.SumCurrents;   // sum terminal currents into system Currents Array
            pelem := CktElements.Next;
        end;
    end;
end;

// =========================================================================================== =
procedure TSolutionObj.DoControlActions;
var
    XHour: Integer;
    XSec: Double;
begin
    with DSS.ActiveCircuit do
    begin
        case ControlMode of

            CTRLSTATIC:
            begin  //  execute the nearest set of control actions but leaves time where it is
                if ControlQueue.IsEmpty then
                    ControlActionsDone := TRUE
                else
                    ControlQueue.DoNearestActions(xHour, XSec); // ignore time advancement
            end;
            EVENTDRIVEN:
            begin  //  execute the nearest set of control actions and advance time to that time
                 // **** Need to update this to set the "Intervalhrs" variable for EnergyMeters for Event-Driven Simulation ****
                if not ControlQueue.DoNearestActions(DynaVars.intHour, DynaVars.t) // these arguments are var type
                then
                    ControlActionsDone := TRUE;// Advances time to the next event
            end;
            TIMEDRIVEN:
            begin   // Do all actions having an action time <= specified time
                if not ControlQueue.DoActions(DynaVars.intHour, DynaVars.t) then
                    ControlActionsDone := TRUE;
            end;
            MULTIRATE:
            begin  //  execute the nearest set of control actions but leaves time where it is
                if not ControlQueue.DoMultiRate(DynaVars.intHour, DynaVars.t) then
                    ControlActionsDone := TRUE;
            end;
        end;
    end;

end;

// =========================================================================================== =
procedure TSolutionObj.SampleControlDevices;

var
    ControlDevice: TControlElem;

begin
    with DSS.ActiveCircuit do
    begin
        ControlDevice := NIL;
        try
            // Sample all controls and set action times in control Queue
            ControlDevice := DSSControls.First;
            while ControlDevice <> NIL do
            begin
                if ControlDevice.Enabled then
                    ControlDevice.Sample;
                ControlDevice := DSSControls.Next;
            end;

        except
            On E: Exception do
            begin
                DoSimpleMsg(Format('Error Sampling Control Device "%s.%s" %s  Error = %s', [ControlDevice.ParentClass.Name, ControlDevice.Name, CRLF, E.message]), 484);
                raise EControlProblem.Create('Solution aborted.');
            end;
        end;
    end;

end;

// =========================================================================================== =
procedure TSolutionObj.Sample_DoControlActions;


begin

    if ControlMode = CONTROLSOFF then
        ControlActionsDone := TRUE
    else
    begin

        SampleControlDevices;
        DoControlActions;

     {This variable lets control devices know the bus list has changed}
        DSS.ActiveCircuit.Control_BusNameRedefined := FALSE;  // Reset until next change
    end;

end;

procedure TSolutionObj.Set_Mode(const Value: TSolveMode);


begin

    DynaVars.intHour := 0;
    DynaVars.t := 0.0;
    Update_dblHour;
    DSS.ActiveCircuit.TrapezoidalIntegration := FALSE;

    if not OK_for_Dynamics(Value) then
        Exit;
    if not OK_for_Harmonics(Value) then
        Exit;

    Dynavars.SolutionMode := Value;

    ControlMode := DefaultControlMode;   // Revert to default mode
    LoadModel := DefaultLoadModel;

    IsDynamicModel := FALSE;
    IsHarmonicModel := FALSE;

    SolutionInitialized := FALSE;   // reinitialize solution when mode set (except dynamics)
    PreserveNodeVoltages := FALSE;  // don't do this unless we have to
    SampleTheMeters := FALSE;
   // Reset defaults for solution modes
    case Dynavars.SolutionMode of

        TSolveMode.PEAKDAY,
        TSolveMode.DAILYMODE:
        begin
            DynaVars.h := 3600.0;
            NumberOfTimes := 24;
            SampleTheMeters := TRUE;
        end;
        TSolveMode.SNAPSHOT:
        begin
            IntervalHrs := 1.0;
            NumberOfTimes := 1;
        end;
        TSolveMode.YEARLYMODE:
        begin
            IntervalHrs := 1.0;
            DynaVars.h := 3600.0;
            NumberOfTimes := 8760;
            SampleTheMeters := TRUE;
        end;
        TSolveMode.DUTYCYCLE:
        begin
            DynaVars.h := 1.0;
            ControlMode := TIMEDRIVEN;
        end;
        TSolveMode.DYNAMICMODE:
        begin
            DynaVars.h := 0.001;
            ControlMode := TIMEDRIVEN;
            IsDynamicModel := TRUE;
            PreserveNodeVoltages := TRUE;  // need to do this in case Y changes during this mode
        end;
        TSolveMode.GENERALTIME:
        begin
            IntervalHrs := 1.0;
            DynaVars.h := 3600.0;
            NumberOfTimes := 1;  // just one time step per Solve call expected
        end;
        TSolveMode.MONTECARLO1:
        begin
            IntervalHrs := 1.0;
            SampleTheMeters := TRUE;
        end;
        TSolveMode.MONTECARLO2:
        begin
            DynaVars.h := 3600.0;
            SampleTheMeters := TRUE;
        end;
        TSolveMode.MONTECARLO3:
        begin
            IntervalHrs := 1.0;
            SampleTheMeters := TRUE;
        end;
        TSolveMode.MONTEFAULT:
        begin
            IsDynamicModel := TRUE;
        end;
        TSolveMode.FAULTSTUDY:
        begin
            IsDynamicModel := TRUE;
        end;
        TSolveMode.LOADDURATION1:
        begin
            DynaVars.h := 3600.0;
            DSS.ActiveCircuit.TrapezoidalIntegration := TRUE;
            SampleTheMeters := TRUE;
        end;
        TSolveMode.LOADDURATION2:
        begin
            DynaVars.intHour := 1;
            DSS.ActiveCircuit.TrapezoidalIntegration := TRUE;
            SampleTheMeters := TRUE;
        end;
        TSolveMode.AUTOADDFLAG:
        begin
            IntervalHrs := 1.0;
            DSS.ActiveCircuit.AutoAddObj.ModeChanged := TRUE;
        end;
        TSolveMode.HARMONICMODE:
        begin
            ControlMode := CONTROLSOFF;
            IsHarmonicModel := TRUE;
            LoadModel := ADMITTANCE;
            PreserveNodeVoltages := TRUE;  // need to do this in case Y changes during this mode
        end;
        TSolveMode.HARMONICMODET:
        begin
            IntervalHrs := 1.0;
            DynaVars.h := 3600.0;
            NumberOfTimes := 1;
            ControlMode := CONTROLSOFF;
            IsHarmonicModel := TRUE;
            LoadModel := ADMITTANCE;
            PreserveNodeVoltages := TRUE;  // need to do this in case Y changes during this mode
        end;
    end;

   {Moved here 9-8-2007 so that mode is changed before reseting monitors, etc.}

   // Reset Meters and Monitors
    DSS.MonitorClass.ResetAll;
    DSS.EnergyMeterClass.ResetAll;
    DoResetFaults(DSS);
    DoResetControls(DSS);

end;

procedure TSolutionObj.AddInAuxCurrents(SolveType: Integer);

begin
    {FOR i := 1 to DSS.ActiveCircuit.NumNodes Do Caccum(Currents^[i], AuxCurrents^[i]);}
    // For Now, only AutoAdd Obj uses this

    if Dynavars.SolutionMode = TSolveMode.AUTOADDFLAG then
        DSS.ActiveCircuit.AutoAddObj.AddCurrents(SolveType);

end;

procedure TSolutionObj.ZeroAuxCurrents;
var
    i: Integer;
begin
    for i := 1 to DSS.ActiveCircuit.NumNodes do
        AuxCurrents^[i] := CZERO;
end;

procedure TSolutionObj.Check_Fault_Status;

var
    pFault: TFaultOBj;

begin
    with DSS.ActiveCircuit do
    begin

        pFault := TFaultObj(Faults.First);
        while pFault <> NIL do
        begin
            pFault.CheckStatus(ControlMode);
            pFault := TFaultObj(Faults.Next);
        end;

    end;  {End With}
end;


{ This procedure is called for Solve Direct and any other solution method
  that does not get the injection currents for PC elements normally. In Dynamics mode,
  Generators are voltage sources ...

Procedure TSolutionObj.GetMachineInjCurrents;

Var
  pElem:TDSSCktElement;

begin
     // do machines in Dynamics Mode
     IF   IsDynamicModel THEN
      With DSS.ActiveCircuit DO  Begin

         pElem := Generators.First;
         WHILE pElem<>nil Do Begin
             IF pElem.Enabled THEN pElem.InjCurrents; // uses NodeRef to add current into InjCurr Array;
             pElem := Generators.Next;
         End;

       End;

end;
}

function TSolutionObj.OK_for_Dynamics(const Value: TSolveMode): Boolean;

var
    ValueIsDynamic: Boolean;

begin

    Result := TRUE;

    case Value of
        TSolveMode.MONTEFAULT,
        TSolveMode.DYNAMICMODE,
        TSolveMode.FAULTSTUDY:
            ValueIsDynamic := TRUE;
    else
        ValueIsDynamic := FALSE;
    end;

   {When we go in and out of Dynamics mode, we have to do some special things}
    if IsDynamicModel and not ValueIsDynamic then
        InvalidateAllPCELEMENTS(DSS);  // Force Recomp of YPrims when we leave Dynamics mode

    if not IsDynamicModel and ValueIsDynamic then
    begin   // see if conditions right for going into dynamics

        if DSS.ActiveCircuit.IsSolved then
            CalcInitialMachineStates(DSS)   // set state variables for machines (loads and generators)
        else
        begin
           {Raise Error Message if not solved}
            DoSimpleMsg('Circuit must be solved in a non-dynamic mode before entering Dynamics or Fault study modes!' + CRLF +
                'If you attempted to solve, then the solution has not yet converged.', 486);
            if DSS.In_ReDirect then
                DSS.Redirect_Abort := TRUE;  // Get outta here
            Result := FALSE;
        end;
    end;

end;

function TSolutionObj.OK_for_Harmonics(const Value: TSolveMode): Boolean;

 {When we go in and out of Harmonics mode, we have to do some special things}
begin

    Result := TRUE;

    if IsHarmonicModel and not ((Value = TSolveMode.HARMONICMODE) or (Value = TSolveMode.HARMONICMODET)) then
    begin
        InvalidateAllPCELEMENTS(DSS);  // Force Recomp of YPrims when we leave Harmonics mode
        Frequency := DSS.ActiveCircuit.Fundamental;   // Resets everything to norm
    end;

    if not IsHarmonicModel and ((Value = TSolveMode.HARMONICMODE) or (Value = TSolveMode.HARMONICMODET)) then
    begin   // see if conditions right for going into Harmonics

        if (DSS.ActiveCircuit.IsSolved) and (Frequency = DSS.ActiveCircuit.Fundamental) then
        begin
            if not InitializeForHarmonics(DSS)   // set state variables for machines (loads and generators) and sources
            then
            begin
                Result := FALSE;
                if DSS.In_ReDirect then
                    DSS.Redirect_Abort := TRUE;  // Get outta here
            end;
        end
        else
        begin

            DoSimpleMsg('Circuit must be solved in a fundamental frequency power flow or direct mode before entering Harmonics mode!', 487);
            if DSS.In_ReDirect then
                DSS.Redirect_Abort := TRUE;  // Get outta here
            Result := FALSE;
        end;
    end;

end;

procedure TSolutionObj.Set_Frequency(const Value: Double);
begin
    if FFrequency <> Value then
    begin
        FrequencyChanged := TRUE;  // Force Rebuild of all Y Primitives
        SystemYChanged := TRUE;  // Force rebuild of System Y
    end;

    FFrequency := Value;
    if DSS.ActiveCircuit <> NIL then
        Harmonic := FFrequency / DSS.ActiveCircuit.Fundamental;  // Make Sure Harmonic stays in synch
end;

procedure TSolutionObj.Increment_time;
begin
    with Dynavars do
    begin
        t := t + h;
        while t >= 3600.0 do
        begin
            Inc(intHour);
            t := t - 3600.0;
        end;
        Update_dblHour;
    end;
end;

procedure TSolutionObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := '';

    inherited InitPropertyValues(NumPropsThisClass);

end;

procedure TSolutionObj.Set_Year(const Value: Integer);
begin
    if DSS.DIFilesAreOpen then
        DSS.EnergyMeterClass.CloseAllDIFiles;
    FYear := Value;
    DynaVars.intHour := 0;  {Change year, start over}
    Dynavars.t := 0.0;
    Update_dblHour;
    DSS.EnergyMeterClass.ResetAll;  // force any previous year data to complete
end;

procedure TSolutionObj.Set_Total_Time(const Value: Double);
begin
    Total_Time_Elapsed := Value;
end;

procedure TSolutionObj.SaveVoltages;

var
    F: TFileStream = nil;
    Volts: Complex;
    i, j: Integer;
    BusName: String;
    sout: String;
begin

    try

        try
            F := TFileStream.Create(DSS.OutputDirectory {CurrentDSSDir} + DSS.CircuitName_ + 'SavedVoltages.Txt', fmCreate);

            with DSS.ActiveCircuit do
                for i := 1 to NumBuses do
                begin
                    BusName := BusList.NameOfIndex(i);
                    for j := 1 to Buses^[i].NumNodesThisBus do
                    begin
                        Volts := NodeV^[Buses^[i].GetRef(j)];
                        WriteStr(sout, BusName, ', ', Buses^[i].GetNum(j): 0, Format(', %-.7g, %-.7g', [Cabs(Volts), CDang(Volts)]));
                        FSWriteln(F, sout);
                    end;
                end;

        except
            On E: Exception do
            begin
                DoSimpleMsg('Error opening Saved Voltages File: ' + E.message, 488);
                Exit;
            end;
        end;


    finally
        FreeAndNil(F);
        DSS.GlobalResult := DSS.OutputDirectory {CurrentDSSDir} + DSS.CircuitName_ + 'SavedVoltages.Txt';

    end;

end;

{  *************  MAIN SOLVER CALL  ************************}

function TSolutionObj.SolveSystem(V: pNodeVArray): Integer;

var
    RetCode: Integer;
    iRes: Longword;
    dRes: Double;

begin

 {Note: NodeV[0] = 0 + j0 always.  Therefore, pass the address of the element 1 of the array.
 }
    try
        RetCode := SolveSparseSet(hY, pComplexArray(@V^[1]), pComplexArray(@Currents^[1]));  // Solve for present InjCurr
{$IFDEF DSS_CAPI}
        if (DSS_CAPI_INFO_SPARSE_COND) then // Disabled by default with DSS C-API
        begin
{$ENDIF}
    // new information functions
            GetFlops(hY, @dRes);
            GetRGrowth(hY, @dRes);
            GetRCond(hY, @dRes);
    // GetCondEst (hY, @dRes); // this can be expensive
{$IFDEF DSS_CAPI}
        end;
{$ENDIF}
        GetSize(hY, @iRes);
        GetNNZ(hY, @iRes);
        GetSparseNNZ(hY, @iRes);
        GetSingularCol(hY, @iRes);
    except
        On E: Exception do
            raise  EEsolv32Problem.Create('Error Solving System Y Matrix.  Sparse matrix solver reports numerical error: ' + E.Message);
    end;

    Result := RetCode;

end;

procedure TSolutionObj.Update_dblHour;
begin
    DynaVars.dblHour := DynaVars.intHour + dynavars.t / 3600.0;
end;

procedure TSolutionObj.UpdateLoopTime;
begin

// Update Loop time is called from end of time step cleanup
// Timer is based on beginning of SolveSnap time

   {$IFDEF MSWINDOWS}
    QueryPerformanceCounter(LoopEndTime);
{$ENDIF}
    Step_Time_Elapsed := ((LoopEndtime - SolveStartTime) / CPU_Freq) * 1000000;

end;

procedure TSolutionObj.UpdateVBus;

// Save present solution vector values to buses
var
    i, j: Integer;
begin
    with DSS.ActiveCircuit do
        for i := 1 to NumBuses do
            with Buses^[i] do
                if Assigned(Vbus) then
                    for j := 1 to NumNodesThisBus do
                        VBus^[j] := NodeV^[GetRef(j)];
end;

procedure TSolutionObj.RestoreNodeVfromVbus;
var
    i, j: Integer;
begin
    with DSS.ActiveCircuit do
        for i := 1 to NumBuses do
            with Buses^[i] do
                if Assigned(Vbus) then
                    for j := 1 to NumNodesThisBus do
                        NodeV^[GetRef(j)] := VBus^[j];

end;

function TSolutionObj.SolveYDirect: Integer;

{ Solves present Y matrix with no injection sources except voltage and current sources }

begin

    Result := 0;

    ZeroInjCurr;   // Side Effect: Allocates InjCurr
    GetSourceInjCurrents;
    if IsDynamicModel then
        GetPCInjCurr;  // Need this in dynamics mode to pick up additional injections

    SolveSystem(NodeV); // Solve with Zero injection current

end;


{$IFDEF DSS_CAPI_PM}
//TODO: move DelFilesFromDir to Utilities, it's also used in Circuit
{*******************************************************************************
*           Routine created to empty a recently created folder                 *
********************************************************************************}
    {$IFDEF MSWINDOWS}
procedure DelFilesFromDir(Directory, FileMask: String; DelSubDirs: Boolean);
var
    SourceLst: String;
    FOS: TSHFileOpStruct;
begin
    FillChar(FOS, SizeOf(FOS), 0);
    FOS.wFunc := FO_DELETE;
    SourceLst := Directory + PathDelim + FileMask + #0;
    FOS.pFrom := Pchar(SourceLst);
    if not DelSubDirs then
        FOS.fFlags := FOS.fFlags or FOF_FILESONLY;
  // Remove the next line if you want a confirmation dialog box
    FOS.fFlags := FOS.fFlags or FOF_NOCONFIRMATION;
  // Add the next line for a "silent operation" (no progress box)
    FOS.fFlags := FOS.fFlags or FOF_SILENT;
    SHFileOperation(FOS);
end;

    {$ENDIF}
    {$IFDEF UNIX}
procedure DeltreeDir(Directory: String);
var
    Info: TSearchRec;
begin
    if FindFirst(Directory + PathDelim + '*', faAnyFile and faDirectory, Info) = 0 then
    begin
        repeat
            with Info do
            begin
                if (name = '.') or (name = '..') then
                    continue;
                if (Attr and faDirectory) = faDirectory then
                begin
                    DeltreeDir(Directory + PathDelim + Name)
                end
                else
                begin
                    DeleteFile(Directory + PathDelim + Name);
                end;
            end;
        until FindNext(info) <> 0;
    end;
    rmdir(Directory);
end;

procedure DelFilesFromDir(Directory, FileMask: String; DelSubDirs: Boolean);
var
    Info: TSearchRec;
    flags: Longint;
begin
    if DelSubDirs then
        flags := faAnyFile and faDirectory
    else
        flags := faAnyFile;

    if FindFirst(Directory + PathDelim + FileMask, flags, Info) = 0 then
    begin
        repeat
            with Info do
            begin
                if (name = '.') or (name = '..') then
                    continue;
                if (Attr and faDirectory) = faDirectory then
                begin
                    try
                        DeltreeDir(Directory + PathDelim + Name)
                    except
                        Writeln('Could not remove directory ' + Directory + PathDelim + Name);
                    end;
                end
                else
                begin
                    DeleteFile(Directory + PathDelim + Name);
                end;
            end;
        until FindNext(info) <> 0;
    end;
end;

    {$ENDIF}
{*******************************************************************************
*             Used to create the OpenDSS Solver thread                         *
********************************************************************************
}

constructor TSolver.Create(dssContext: TDSSContext; Susp: Boolean; local_CPU: Integer; CallBack: TInfoMessageCall; AEvent: TEvent);

var
    Parallel: TParallel_Lib;
    Thpriority: String;
begin
    DSS := dssContext;

    UIEvent := AEvent;
    FInfoProc := CallBack;
    FreeOnTerminate := FALSE;
    ActorMsg := TEvent.Create(NIL, TRUE, FALSE, '');
    MsgType := -1;
    ActorActive := TRUE;
    Processing := FALSE;

    inherited Create(Susp);
  {$IFDEF MSWINDOWS}              // Only for windows
//  Parallel.Set_Process_Priority(GetCurrentProcess(), REALTIME_PRIORITY_CLASS);
    Parallel.Set_Thread_affinity(handle, local_CPU);
//  Parallel.Set_Thread_Priority(handle,THREAD_PRIORITY_TIME_CRITICAL);
  {$ELSE}
//  Parallel.Set_Thread_Priority(self,THREAD_PRIORITY_TIME_CRITICAL);
    Parallel.Set_Thread_affinity(handle, local_CPU);
  {$ENDIF}

end;

procedure TSolver.Send_Message(Msg: Integer);
begin
    MsgType := Msg;
    ActorMsg.SetEvent;
end;

procedure TSolver.Set_Processing(NVal: Boolean);
begin
    Processing := NVal;
end;

function TSolver.Get_Processing(): Boolean;
begin
    Result := Processing;
end;

function TSolver.Get_CPU(): Integer;
begin
    Result := DSS.CPU;
end;

procedure TSolver.Set_CPU(CPU: Integer);
var
    Parallel: TParallel_Lib;
begin
    DSS.CPU := CPU;
    Parallel.Set_Thread_affinity(handle, CPU);
//  Parallel.Set_Thread_Priority(handle,THREAD_PRIORITY_TIME_CRITICAL);
end;

{*******************************************************************************
*             executes the selected solution algorithm                         *
********************************************************************************
}

procedure TSolver.Execute;
var
{$IFNDEF FPC}
    ScriptEd: TScriptEdit;
{$ENDIF}
    i,
    j,
    idx: Integer;
    VSourceObj: TVsourceObj;
    Volts: Polar;

begin
    with DSS.ActiveCircuit, Solution do
    begin
        while ActorActive do
        begin
            ActorMsg.WaitFor(INFINITE);
            ActorMsg.ResetEvent;
            Processing := TRUE;
            
            // Evaluates the incoming message
            case MsgType of             
                SIMULATE: // Simulates the active ciruit on this actor
                    try
                        begin                   // Checks if this is the coordinator actor in A-Diakoptics mode
                            if DSS.ADiakoptics and DSS.IsPrime and (not DSS.IsSolveAll) then
                            begin
                                Solve_Diakoptics(DSS)
                            end
                            else
                            begin
                                // Verifies if there is an A-Diakoptics simulation running to update the local Vsources
                                if DSS.ADiakoptics and (not DSS.IsSolveAll) then
                                    Start_Diakoptics();

                                // Normal solution routine
                                case Dynavars.SolutionMode of
                                    TSolveMode.SNAPSHOT:
                                        SolveSnap;
                                    TSolveMode.YEARLYMODE:
                                        SolveYearly;
                                    TSolveMode.DAILYMODE:
                                        SolveDaily;
                                    TSolveMode.DUTYCYCLE:
                                        SolveDuty;
                                    TSolveMode.DYNAMICMODE:
                                        SolveDynamic;
                                    TSolveMode.MONTECARLO1:
                                        SolveMonte1;
                                    TSolveMode.MONTECARLO2:
                                        SolveMonte2;
                                    TSolveMode.MONTECARLO3:
                                        SolveMonte3;
                                    TSolveMode.PEAKDAY:
                                        SolvePeakDay;
                                    TSolveMode.LOADDURATION1:
                                        SolveLD1;
                                    TSolveMode.LOADDURATION2:
                                        SolveLD2;
                                    TSolveMode.DIRECT:
                                        SolveDirect;
                                    TSolveMode.MONTEFAULT:
                                        SolveMonteFault;  // Monte Carlo Fault Cases
                                    TSolveMode.FAULTSTUDY:
                                        SolveFaultStudy;
                                    TSolveMode.AUTOADDFLAG:
                                        AutoAddObj.Solve;
                                    TSolveMode.HARMONICMODE:
                                        SolveHarmonic;
                                    TSolveMode.GENERALTIME:
                                        SolveGeneralTime;
                                    TSolveMode.HARMONICMODET:
                                        SolveHarmonicT;  //Declares the Hsequential-time harmonics
                                else
                                    DoSimpleMsg('Unknown solution mode.', 481);
                                end;
                            end;
                            {$IFDEF MSWINDOWS}
                            QueryPerformanceCounter(GEndTime);
                            {$ELSE}
                            GEndTime := GetTickCount64;
                            {$ENDIF}
                            Total_Solve_Time_Elapsed := ((GEndTime - GStartTime) / CPU_Freq) * 1000000;
                            Total_Time_Elapsed := Total_Time_Elapsed + Total_Solve_Time_Elapsed;
                            Processing := FALSE;
                            FMessage := '1';
                            DSS.ActorStatus := TActorStatus.Idle;

                  // If this is an A-Diakoptics actor reports the results to the coordinator (Actor 1)
                            if DSS.ADiakoptics and not DSS.IsPrime then
                                Notify_Main();

                  // Sends a message to Actor Object (UI) to notify that the actor has finised
                            UIEvent.SetEvent;
                  {$IFDEF MSWINDOWS}
                            if not DSS.ADiakoptics then
                            begin
                                if DSS.Parallel_enabled then
                                    if not IsDLL then
                                        queue(CallCallBack); // Refreshes the GUI if running asynchronously
                            end
                            else
                            begin
                                if (DSS.Parallel_enabled and DSS.IsPrime) then
                                    if not IsDLL then
                                        queue(CallCallBack); // Refreshes the GUI if running asynchronously
                            end;
                  {$ENDIF}
                        end;
                    except
                        On E: Exception do
                        begin
                            FMessage := '1';
                            DSS.ActorStatus := TActorStatus.Idle;
                            DSS.SolutionAbort := TRUE;
                            UIEvent.SetEvent;
                            if not DSS.ADiakoptics then
                            begin
                                if DSS.Parallel_enabled then
                                    if not IsDLL then
                                        Queue(CallCallBack); // Refreshes the GUI if running asynchronously
                            end
                            else
                            begin
                                if (DSS.Parallel_enabled and DSS.IsPrime) then
                                    if not IsDLL then
                                        queue(CallCallBack); // Refreshes the GUI if running asynchronously
                            end;
                            if not DSS.Parallel_enabled then
                                DoSimpleMsg('Error Encountered in Solve: ' + E.Message, 482);
                        end;
                    end;
                EXIT_ACTOR:                // Terminates the thread
                begin
                    ActorActive := FALSE;
                end
            else                       // I don't know what the message is
                DoSimpleMsg('Unknown Message.', 7010);
            end;

        end;
    end;
end;

procedure TSolver.CallCallBack;
begin
    if Assigned(FInfoProc) then
        FInfoProc(FMessage);
end;

// Initializes the variables of the A-Diakoptics worker
procedure TSolver.Start_Diakoptics();
var
    row,
    j,
    i: Integer;
    VSource: TVsourceObj;
    Volts: Polar;
    CNum: Complex;
    PrimeCircuit: TDSSCircuit;
begin
    PrimeCircuit := DSS.GetPrime().ActiveCircuit;

    with DSS.ActiveCircuit, Solution do
    begin
        j := PrimeCircuit.Ic.NZero - 1;    // Brings the number of Non-Zero elements from Ic
        if j > 0 then
        begin
            // Clears the local Ic vector
            for i := 1 to NumNodes do
                Ic_Local[i] := cZERO;  // probably not necessary

            // Brings the section of the vector needed for this actor
            for i := 0 to j do
            begin
                if PrimeCircuit.Ic.CData[i].row >= VIndex then
                begin
                    row := PrimeCircuit.Ic.CData[i].row;
                    Ic_Local[row - VIndex + 1] := PrimeCircuit.Ic.CData[i].Value;
                end;
            end;

            // Solves to find the total solution
            SolveSparseSet(hY, @Node_dV[1], @Ic_Local[1]);

            // Sends the total voltage for this part to the coordinator
            for i := 1 to NumNodes do
            begin
                CNum := csub(NodeV[i], Node_dV[i]);
                PrimeCircuit.Solution.NodeV[i + VIndex] := CNum;
            end;

            // Sets the voltage at the feeder head
            VSource := DSS.VSourceClass.ElementList.First;
            for i := 1 to 3 do
            begin
                CNum := cadd(NodeV[i], Node_dV[i]);
                Volts := ctopolardeg(CNum);
                VSource.kVBase := Volts.mag / 1000;   // is in kV
                VSource.Angle := Volts.ang;
                VSource := DSS.VSourceClass.ElementList.Next;
            end;
        end;
    end;
end;

procedure TSolver.Notify_Main;
var
    CNum: Complex;
    i, j,
    idx: Integer;
    PrimeCircuit: TDSSCircuit;
begin
    PrimeCircuit := DSS.GetPrime().ActiveCircuit;

    // Will do something
    with DSS.ActiveCircuit, Solution do
    begin
        i := NumNodes;
        for idx := 1 to i do
        begin
            // if it doesn't includes any power injection element (Isource, VSource)
            // returns dV to correct the interconnection equation
            if not DSS.IsPrime then
                CNum := csub(NodeV^[idx], Node_dV^[idx])
            else
                CNum := NodeV^[idx];

            PrimeCircuit.V_0.Insert((idx + VIndex - 1), 0, CNum);
        end;
    end;

end;

procedure TSolver.DoTerminate;        // Is the end of the thread
var
    ex: TObject;
begin
    ActorActive := FALSE;
    Processing := FALSE;
    DSS.ActorStatus := TActorStatus.Idle;
    UIEvent.SetEvent;
    ActorMsg.Free;
//    FreeAndNil(UIEvent);
    inherited;
end;

destructor TSolver.Destroy;
begin
    inherited destroy;
end;

{$ENDIF} //DSS_CAPI_PM
end.
