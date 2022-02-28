unit Solution;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Classes,
    UComplex, DSSUcomplex,
    Arraydef,
    Command,
    Monitor,
    DSSClass,
    DSSObject,
    Dynamics,
    EnergyMeter,
    Sparse_Math,
    VSource,
    ISource,
    SysUtils,
    generics.collections,
{$IFDEF MSWINDOWS}
    Windows,
{$ELSE}
    BaseUnix,
    Unix,
{$ENDIF}
    Strings,
{$IFDEF DSS_CAPI_PM}
    SyncObjs,
    ExecHelper,
{$ENDIF}
    CktElement,
    DSSPointerList;

const
    NORMALSOLVE = 0;
    NEWTONSOLVE = 1;

{$IFDEF DSS_CAPI_ADIAKOPTICS}
    AD_ACTORS = 1; // Wait flag to wait only for the A-Diakoptics actors
{$ENDIF}
    ALL_ACTORS = 0; // Wait flag for all the actors

type
{$SCOPEDENUMS ON}
    TActorMessage = (
        // Constants for the actor's messaging
        SIMULATE = 0,
        EXIT_ACTOR,
        CALC_INJ_CURR,  // Uses the total solution to estiamte the injection currents
        DO_CTRL_ACTIONS,  // Does the control actions distributedly
        ZEROIVECTOR,  // Zeroes the actor's I vector
        GETCURRINJ,  // Gets the current injections for the actor and uploades them in the local I vector
        CHECKYBUS, // Rebuilds the YBus if needed at local level
        CHECK_FAULT // Checks the fault status at local level
{$IFDEF DSS_CAPI_ADIAKOPTICS}
        ,
        INIT_ADIAKOPTICS, // Initializes the environment for the children actors
        SOLVE_AD1, // solves for the actors with Power Injections -> E(0)
        SOLVE_AD2, // Solves the sub-system and adds to the partial solution
        GETCTRLMODE // Sync the local control mode with actor 1
{$ENDIF}

    );
{$SCOPEDENUMS OFF}
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

{$IFDEF DSS_CAPI_PM}
   TSolver = class(TThread)
        constructor Create(dssContext: TDSSContext; Susp: Boolean; local_CPU: Integer; AEvent: TEvent); OVERLOAD;
        procedure Execute; OVERRIDE;
        procedure Doterminate; OVERRIDE;
        destructor Destroy; OVERRIDE;

    PROTECTED
        DSS: TDSSContext;

        FMessage: String;
        UINotifier,
        // ActorID,
        StatusEvent,
        MessageQueueEvent: TEvent;
        ActorIsActive,
        Processing: Boolean;
        
        actorMessages: TQueue<TActorMessage>;  // A queue for messaging to actors, the aim is to reduce inconsistency
        actorMessagesLock: TCriticalSection;

{$IFDEF DSS_CAPI_ADIAKOPTICS}
        AD_Init: Boolean;          // used to know if the actors require a partial solution

        procedure Start_Diakoptics();
        procedure IndexBuses(); // Locates the actor buses within the bus array in Actor 1 (interconnected)
{$ENDIF}
        function Get_CPU(): Integer;
        procedure Set_CPU(CPU: Integer);

    PUBLIC
        procedure Send_Message(Msg: TActorMessage);
        property CPU: Integer READ Get_CPU WRITE Set_CPU;
    end;
{$ENDIF}

    TSolutionObj = class(TObject)
    PRIVATE
        dV: pNodeVArray;   // Array of delta V for Newton iteration
        FFrequency: Double;

        function OK_for_Dynamics(const Value: TSolveMode): Boolean;
        function OK_for_Harmonics(const Value: TSolveMode): Boolean;


        procedure DoNewtonSolution;
        procedure DoNormalSolution;
        procedure SumAllCurrents;
        procedure Set_Frequency(const Value: Double);
        procedure Set_Mode(const Value: TSolveMode);
        procedure Set_Year(const Value: Integer);
        procedure Set_Total_Time(const Value: Double);

    PUBLIC
        DSS: TDSSContext;
        Algorithm: Integer;      // NORMALSOLVE or NEWTONSOLVE
        AuxCurrents: pComplexArray; // For injections like AutoAdd
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
        hYsystem: NativeUint;   // Handle for main (system) Y matrix
        hYseries: NativeUint;   // Handle for series Y matrix
        hY: NativeUint;         // either hYsystem or hYseries
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

        // Voltage and Current Arrays
        NodeV: pNodeVArray;    // Main System Voltage Array   allows NodeV^[0]=0
        Currents: pNodeVArray;      // Main System Currents Array

// ******************************************************************************
        IncMat: Tsparse_matrix; // Incidence sparse matrix
        Laplacian: Tsparse_matrix; // Laplacian sparse matrix

// ****************************Timing variables**********************************
        SolveStartTime: Int64;
        SolveEndtime: Int64;
        GStartTime: Int64;
        Gendtime: Int64;
        LoopEndtime: Int64;
        Total_Time_Elapsed: Double;
        Solve_Time_Elapsed: Double;
        Total_Solve_Time_Elapsed: Double;
        Step_Time_Elapsed: Double;
// ******************************************************************************
// ActiveCell of the Incidence Matrix:
// [0] = row
// [1] = col
// [2] = value
        ActiveIncCell: array[0..2] of Integer;
// ******************************************************************************
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
{$IFDEF DSS_CAPI_ADIAKOPTICS}
        // A-Diakoptics variables

        ADiakoptics: Boolean;
        ADiak_Init: Boolean;
        ADiak_PCInj: Boolean;

        Node_dV: pNodeVArray;     // Used to store the partial solution voltage
        Ic_Local: pNodeVArray;     // Used to store the complementary curret
// ******************************************************************************
// ********************Diakoptics solution mode variables************************
        ADiakoptics_ready: Boolean;
        ADiakoptics_Actors: Integer;
        LockNodeV: TCriticalSection;
        LocalBusIdx: array of Integer;
        AD_IBus: TList<Integer>;       // Location of the Current injection bus
        AD_ISrcIdx: TList<Integer>;       // Locator of the ISource bus in actor 1

        function SolveAD(Initialize: Boolean): Integer;    // solve one of the A-Diakoptics stages locally
        procedure SendCmd2Actors(Msg: Integer); // Sends a message to other actors different than 1
        procedure UpdateISrc; // Updates the local ISources using the data available at Ic for actor 1
        function VoltInActor1(NodeIdx: Integer): complex; // returns the voltage indicated in NodeIdx in the context of the actor 1
{$ENDIF}

        constructor Create(dssContext: TDSSContext; const solutionname: String);
        destructor Destroy; OVERRIDE;

        function Converged: Boolean;
        procedure SetGeneratordQdV;

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

        procedure DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean = False);
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

        procedure AddInAuxCurrents(SolveType: Integer);
        function SolveSystem(V: pNodeVArray): Integer;
        procedure GetPCInjCurr;
        procedure GetSourceInjCurrents;
        procedure ZeroInjCurr;
        procedure Upload2IncMatrix;

        procedure Calc_Inc_Matrix; // Calculates the incidence matrix for the Circuit
        procedure Calc_Inc_Matrix_Org; // Calculates the incidence matrix hierarchically organized for the Circuit

        function get_IncMatrix_Row(Col: Integer): Integer; // Gets the index of the Row connected to the specified Column
        function get_IncMatrix_Col(Row: Integer): Integer; // Gets the index of the Column connected to the specified Row
        function CheckLocationIdx(Idx: Integer): Integer; // Evaluates the area covered by the tearing point to see if there is a better one
        function get_PDE_Bus1_Location(myPDE: String): Integer; // Gets the index of myPDE -> bus1 within the Inc matrix

        procedure AddLines2IncMatrix; // Adds the Lines to the Incidence matrix arrays
        procedure AddXfmr2IncMatrix; // Adds the Xfmrs to the Incidence matrix arrays
        procedure AddSeriesCap2IncMatrix; // Adds capacitors in series to the Incidence matrix arrays
        procedure AddSeriesReac2IncMatrix; // Adds Reactors in series to the Incidence matrix arrays

    end;

implementation

uses
    BufStream,
    SolutionAlgs,
    DSSClassDefs,
    DSSGlobals,
{$IFDEF MSWINDOWS}
    SHELLAPI,
{$ELSE}    
    {$IFDEF DSS_CAPI_PM}
    initc, 
    cpucount, 
    {$ENDIF}
{$ENDIF}
    CmdForms,
    PDElement,
    ControlElem,
    Fault,
    Executive,
    AutoAdd,
    YMatrix,
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
    Line,
{$IFDEF DSS_CAPI_ADIAKOPTICS}
    Diakoptics,
{$ENDIF}
    DSSHelper;

const
    NumPropsThisClass = 1;

{$IFDEF DSS_CAPI_PM}
    {$if defined(WINDOWS)}
function Set_Thread_Affinity(Hnd : THandle; CPU : integer): Integer;
var
    CPU_bit: integer;
    Op_Result: Dword;
begin
    if CPU >= 0 then
        CPU_bit := 1 shl CPU
    else
        CPU_bit := -1;

    Op_Result := SetThreadAffinityMask(Hnd, CPU_bit);
    if Op_Result = 0 then
        raise Exception.Create(Format(_('Error setting thread affinity mask: %d'), [GetLastError]));
    Result := Op_Result;
end;
    {$ELSEIF defined(freebsd) or defined(darwin)}
function Set_Thread_Affinity(Hnd: TThreadId; CPU: integer): Integer;
begin
    Result := 0;
end;
    {$ELSE}

function pthread_setaffinity_np(pid : PtrUint; cpusetsize: NativeUInt; cpuset: Pointer): NativeInt; cdecl; external;

function Set_Thread_Affinity(Hnd: TThreadId; CPU: integer): Integer;
const
    cpu_SetSize = SizeOf(QWord); // 8 bytes, 64 cores max -- to match the limitations of the Windows version
var
    cpu_set : QWord;
    Op_Result : longint;
begin
    if CPU >= 0 then
        cpu_set := 1 shl CPU
    else
    begin
        cpu_set := 0;
        cpu_set := not cpu_set;
    end;
    Op_Result := pthread_setaffinity_np(Hnd, cpu_SetSize, @cpu_set);
    if Op_Result <> 0 then raise Exception.Create('Error setting thread affinity mask');
    Result := Op_Result;
end;
    {$ENDIF}
{$ENDIF}

constructor TSolutionObj.Create(dssContext: TDSSContext; const SolutionName: String);
begin
    inherited Create;
    // Name := LowerCase(SolutionName);
    DSS := dssContext;

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

    // Define default harmonic list
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

    RandomType := GAUSSIAN; // default to gaussian
    NumberOfTimes := 100;
    IntervalHrs := 1.0;
{$IFDEF DSS_CAPI_PM}
    if not Assigned(DSS.ThreadStatusEvent) then
        DSS.ThreadStatusEvent := TEvent.Create(NIL, TRUE, FALSE, '');
{$ENDIF}
{$IFDEF DSS_CAPI_ADIAKOPTICS}
    ADiakoptics := False;
    ADiak_Init := False;
    ADiak_PCInj := False;

    Node_dV := NIL;
    Ic_Local := NIL;
    AD_IBus := NIL;
    AD_ISrcIdx := NIL;
    ADiakoptics_Ready := FALSE; // A-Diakoptics needs to be initialized
    LockNodeV := SyncObjs.TCriticalSection.Create9);
{$ENDIF}
end;

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

    Reallocmem(HarmonicList, 0);
{$IFDEF DSS_CAPI_PM}    
    // Sends a message to the working actor
    with DSS do 
    begin
        // ThreadStatusEvent.SetEvent();
        if ActorThread <> NIL then
        begin
            SolutionAbort := True;
            ActorThread.Send_Message(TActorMessage.EXIT_ACTOR);
            ActorThread.WaitFor();
            ActorThread.Free();
            ActorThread := nil;
        end;
        ThreadStatusEvent.Free;
        ThreadStatusEvent := NIL;
    end;
{$ENDIF}
{$IFDEF DSS_CAPI_ADIAKOPTICS}
    LockNodeV.Free;
{$ENDIF}

    inherited Destroy;
end;

procedure TSolutionObj.Solve;
{$IFDEF DSS_CAPI_PM}
var
    PMParent: TDSSContext;
begin
    PMParent := DSS.GetPrime();
{$ELSE}
begin
{$ENDIF}
    DSS.ActiveCircuit.Issolved := FALSE;
    DSS.SolutionWasAttempted := TRUE;

    InitProgressForm; // initialize Progress Form;

    // Check of some special conditions that must be met before executing solutions
    if DSS.ActiveCircuit.EmergMinVolts >= DSS.ActiveCircuit.NormalMinVolts then
    begin
        DoSimpleMsg(DSS, _('Error: Emergency Min Voltage Must Be Less Than Normal Min Voltage! Solution Not Executed.'), 480);
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
        // Main solution Algorithm dispatcher
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
        // CheckFaultStatus;  ???? needed here??

{$IFDEF DSS_CAPI_PM}
        // If we won't run in parallel and don't have a thread already,
        // don't use a new thread
        if (not PMParent.Parallel_enabled) and (DSS.ActorThread = NIL) then
        begin
{$ENDIF}
            {$IFDEF WINDOWS}
            QueryPerformanceCounter(GStartTime);
            {$ELSE}
            GStartTime := GetTickCount64;
            {$ENDIF}
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
                DoSimpleMsg(DSS, _('Unknown solution mode.'), 481);
            end;
            {$IFDEF WINDOWS}
            QueryPerformanceCounter(GEndTime);
            {$ELSE}
            GEndTime := GetTickCount64;
            {$ENDIF}
            Total_Solve_Time_Elapsed := ((GEndTime - GStartTime) / CPU_Freq) * 1000000;
            Total_Time_Elapsed := Total_Time_Elapsed + Total_Solve_Time_Elapsed;
            Exit;
{$IFDEF DSS_CAPI_PM}
        end;
        // Creates the actor again in case of being terminated due to an error before
        if (DSS.ActorThread = NIL) or DSS.ActorThread.Terminated then
        begin
            if (DSS.ActorThread <> NIL) and DSS.ActorThread.Terminated then
                DSS.ActorThread.Free;

            DSS.ActorThread := TSolver.Create(DSS, True, DSS.CPU, DSS.ThreadStatusEvent);
            // DSS.ActorThread.Priority := tpTimeCritical;
            DSS.ActorStatus := TActorStatus.Busy;
            DSS.ActorThread.Start();
        end;
        // CheckFaultStatus;  ???? needed here??

        // Resets the event for receiving messages from the active actor
        // Updates the status of the Actor in the GUI
        
        // DSS.ActorStatus := TActorStatus.Busy;
        // DSS.ThreadStatusEvent.SetEvent();

        {$IFDEF WINDOWS}
        QueryPerformanceCounter(GStartTime);
        {$ELSE}
        GStartTime := GetTickCount64;
        {$ENDIF}

        // Sends message to start the Simulation
        DSS.ActorThread.Send_Message(TActorMessage.SIMULATE);

        // If the parallel mode is not active, Waits until the actor finishes
        if not DSS.GetPrime().Parallel_enabled then
            Wait4Actors(DSS, ALL_ACTORS);
{$ENDIF} // DSS_CAPI_PM
    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error Encountered in Solve: %s', [E.Message], 482);
            DSS.SolutionAbort := TRUE;
        end;

    end;
end;

function TSolutionObj.Converged: Boolean;
var
    i: Integer;
    VMag: Double;
begin
    // base convergence on voltage magnitude
    MaxError := 0.0;
    for i := 1 to DSS.ActiveCircuit.NumNodes do
    begin
{$IFDEF DSS_CAPI_ADIAKOPTICS}
        if not ADiakoptics or (DSS.Parent = NIL) then
{$ENDIF}
            VMag := Cabs(NodeV^[i])
{$IFDEF DSS_CAPI_ADIAKOPTICS}
        else
            VMag := Cabs(VoltInActor1(i));
{$ELSE}
        ;
{$ENDIF}
        // If base specified, use it; otherwise go on present magnitude
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
            DoSimpleMsg(DSS, _('Unknown solution mode.'), 483);
        end;
end;

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
            DoSimpleMsg(DSS, 'From SetGenerator DQDV, SolveZeroLoadSnapShot: %s', [CRLF + E.Message + CheckYMatrixforZeroes(DSS)], 7071);
            raise ESolveError.Create('Aborting');
        end;
    end;
end;

procedure TSolutionObj.DoNormalSolution;
// Normal fixed-point solution
//
//   Vn+1 = [Y]-1 Injcurr
//
//   Where Injcurr includes only PC elements  (loads, generators, etc.)
//   i.e., the shunt elements.
//
//   Injcurr are the current injected INTO the NODE
//        (need to reverse current direction for loads)
begin

    Iteration := 0;

    // **** Main iteration loop ****
    with DSS.ActiveCircuit do
        repeat
            Inc(Iteration);

            if LogEvents then
                LogThisEvent(DSS, 'Solution Iteration ' + IntToStr(Iteration));

{$IFDEF DSS_CAPI_ADIAKOPTICS}
            if (not ADiakoptics) or (DSS.Parent <> NIL) then // Normal simulation
            begin // In A-Diakoptics, all other actors do normal solution
{$ENDIF}
                // Get injcurrents for all PC devices
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

                // Solve for voltages -- Note: NodeV[0] = 0 + j0 always
                if LogEvents then
                    LogThisEvent(DSS, 'Solve Sparse Set DoNormalSolution ...');
                SolveSystem(NodeV);
                LoadsNeedUpdating := FALSE;
{$IFDEF DSS_CAPI_ADIAKOPTICS}
            end
            else
            begin
                ADiak_PCInj := TRUE;
                Solve_Diakoptics(DSS); // A-Diakoptics
            end;
{$ENDIF}
        until (Converged and (Iteration >= MinIterations)) or (Iteration >= MaxIterations);
end;

procedure TSolutionObj.DoNewtonSolution;
// Newton Iteration
//
//   Vn+1 =  Vn - [Y]-1 Termcurr
//
//   Where Termcurr includes currents from all elements and we are
//   attempting to get the  currents to sum to zero at all nodes.
//
//   Termcurr is the sum of all currents going INTO THE TERMINALS of
//   the elements.
//
//   For PD Elements, Termcurr = Yprim*V
//
//   For Loads, Termcurr = (Sload/V)*
//   For Generators, Termcurr = -(Sgen/V)*
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
            if SystemYChanged {$IFDEF DSS_CAPI_INCREMENTAL_Y}or (DSS.ActiveCircuit.IncrCktElements.Count <> 0){$ENDIF} then
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
                DoSimpleMsg(DSS, 'From DoPFLOWsolution.SolveYDirect: %s', [CRLF + E.Message + CheckYMatrixforZeroes(DSS)], 7072);
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
                DoSimpleMsg(DSS, 'From DoPFLOWsolution.SetGeneratordQdV: %s', [CRLF + E.Message + CheckYMatrixforZeroes(DSS)], 7073);
                raise ESolveError.Create('Aborting');
            end;
        end;

        // The above resets the active sparse set to hY
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

    // Make the series Y matrix the active matrix
    if hYseries = 0 then
        raise EEsolv32Problem.Create('Series Y matrix not built yet in SolveZeroLoadSnapshot.');
    hY := hYseries;

    if DSS.ActiveCircuit.LogEvents then
        LogThisEvent(DSS, 'Solve Sparse Set ZeroLoadSnapshot ...');

    SolveSystem(NodeV);  // also sets voltages in radial part of the circuit if radial solution

    // Reset the main system Y as the solution matrix
    if (hYsystem > 0) and not DSS.SolutionAbort then
        hY := hYsystem;
end;

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
                    kVBase := NearestBasekV(DSS, Cabs(NodeV^[RefNo[1]]) * 0.001732) / SQRT3;  // l-n base kV

        InitializeNodeVbase(DSS);      // for convergence test

        DSS.ActiveCircuit.Issolved := TRUE;

    // now build the meter zones
        DSS.ActiveCircuit.MeterZonesComputed := bZoneCalc;
        DSS.ActiveCircuit.ZonesLocked := bZoneLock;
        DSS.ActiveCircuit.DoResetMeterZones;

    except
        ON E: EEsolv32Problem do
        begin
            DoSimpleMsg(DSS, 'From SetVoltageBases.SolveZeroLoadSnapShot: %s', [CRLF + E.Message + CheckYMatrixforZeroes(DSS)], 7075);
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
{$IFDEF DSS_CAPI_ADIAKOPTICS}
var
    i: Integer;
begin
    if not ADiakoptics or (DSS.Parent <> NIL) then
    begin
{$ENDIF}
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
{$IFDEF DSS_CAPI_ADIAKOPTICS}
    end
    else
    begin
        if ControlIteration < MaxControlIterations then
        begin
            if DSS.ActiveCircuit.LogEvents then
                LogThisEvent(DSS, 'Control Iteration ' + IntToStr(ControlIteration));

            SendCmd2Actors(DO_CTRL_ACTIONS);
            // Checks if there are pending ctrl actions at the actors
            ControlActionsDone := TRUE;
            for i := 2 to DSS.NumOfActors do
                ControlActionsDone := ControlActionsDone and DSS.Children[i - 1].ActiveCircuit.Solution.ControlActionsDone;
        end;
    end;
{$ENDIF}
end;

function TSolutionObj.SolveSnap: Integer;  // solve for now once
var
    TotalIterations: Integer;
begin
    SnapShotInit;
    TotalIterations := 0;
   {$IFDEF MSWINDOWS}
    QueryPerformanceCounter(SolveStartTime);
   {$ELSE}
    SolveStartTime := GetTickCount64;
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
        DoSimpleMsg(DSS, _('Warning Max Control Iterations Exceeded.') + CRLF + _('Tip: Show Eventlog to debug control settings.'), 485);
        DSS.SolutionAbort := TRUE;   // this will stop this message in dynamic power flow modes
    end;

    if DSS.ActiveCircuit.LogEvents then
        LogThisEvent(DSS, 'Solution Done');

    {$IFDEF DLL_ENGINE}
    Fire_StepControls;
    {$ENDIF}

    {$IFDEF MSWINDOWS}
    QueryPerformanceCounter(SolveEndTime);
    {$ELSE}
    SolveEndTime := GetTickCount64;
    {$ENDIF}
    Solve_Time_Elapsed := ((SolveEndtime - SolveStartTime) / CPU_Freq) * 1000000;
    Iteration := TotalIterations;  { so that it reports a more interesting number }

end;

function TSolutionObj.SolveDirect: Integer;  // solve for now once, direct solution
begin
    Result := 0;

    LoadsNeedUpdating := TRUE;  // Force possible update of loads and generators
    {$IFDEF MSWINDOWS}
    QueryPerformanceCounter(SolveStartTime);
    {$ELSE}
    SolveStartTime := GetTickCount64;
    {$ENDIF}

    Inc(SolutionCount); // Unique number for this solution

{$IFDEF DSS_CAPI_ADIAKOPTICS}
    if not ADiakoptics or (DSS.Parent <> NIL) then
    begin
{$ENDIF}    
        if SystemYChanged then
        begin
            BuildYMatrix(DSS, WHOLEMATRIX, TRUE); // Side Effect: Allocates V
        end;

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
{$IFDEF DSS_CAPI_ADIAKOPTICS}
    end
    else
    begin
        ADiak_PCInj := FALSE;
        Solve_Diakoptics(DSS); // A-Diakoptics

        DSS.ActiveCircuit.IsSolved := TRUE;
        ConvergedFlag := TRUE;
    end;
{$ENDIF}
    {$IFDEF MSWINDOWS}
    QueryPerformanceCounter(SolveEndTime);
    {$ELSE}
    SolveEndTime := GetTickCount64;
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
                DoSimpleMsg(DSS, 'From SolveSnap.SolveDirect: %s', [CRLF + E.Message + CheckYMatrixforZeroes(DSS)], 7075);
                raise ESolveError.Create('Aborting');
            end;
        end
    else
    begin
        try
            if SystemYChanged then
            begin
{$IFDEF DSS_CAPI_ADIAKOPTICS}
                if not ADiakoptics or (DSS.Parent <> NIL) then
{$ENDIF}
                    BuildYMatrix(DSS, WHOLEMATRIX, TRUE);   // Side Effect: Allocates V
            end;
            DoPFLOWsolution;
        except
            ON E: EEsolv32Problem do
            begin
                DoSimpleMsg(DSS, 'From SolveSnap.DoPflowSolution: %s', [CRLF + E.Message + CheckYMatrixforZeroes(DSS)], 7074);
                raise ESolveError.Create('Aborting');
            end;
        end
    end;
end;

procedure TSolutionObj.ZeroInjCurr;
var
    I: Integer;
begin
    for i := 0 to DSS.ActiveCircuit.NumNodes do
        Currents^[i] := CZERO;
end;

procedure TSolutionObj.Upload2IncMatrix;
begin
  // Uploads the values to the incidence matrix
    IncMat.insert((ActiveIncCell[0] - 1), (ActiveIncCell[1] - 2), ActiveIncCell[2]);
    ActiveIncCell[2] := -1;
end;

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
                for TermIdx := 1 to elem.NumWindings do
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

procedure TSolutionObj.AddSeriesCap2IncMatrix;
var
    CapBus: String;
    elem: TCapacitorObj;
    lst: TDSSPointerList;
    CapTermIdx,
    BusdotIdx: Integer;
    CapEndFlag: Boolean;
begin
    // This routine adds the series capacitors to the incidence matrix vectors
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

procedure TSolutionObj.AddSeriesReac2IncMatrix;
var
    RBus: String;
    elem,
    DevClassIndex: Integer;
    TermIdx,
    BusdotIdx: Integer;
    EndFlag: Boolean;
begin
    // This routine adds the series reactors to the incidence matrix vectors
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

// Routine for extracting the Branch to Node incidence matrix
// The order depends on the way the lines, xfmr, series cap and reactors
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

// This function returns the index of bus 1 with the Incidence
// matrix for the given PDE
function TSolutionObj.get_PDE_Bus1_Location(myPDE: String): Integer;
var
    i,
    j: Integer;
    myBUS: String;
begin
    with DSS.ActiveCircuit do
    begin
        SetElementActive(myPDE);
        myBUS := ActiveCktElement.GetBus(2);
        j := ansipos('.', myBus);
        if j <> 0 then
            myBUS := copy(myBUS, 0, j - 1);
        for i := 0 to High(Inc_Mat_Cols) do
            if Inc_Mat_Cols[i] = myBUS then
                break;
        Result := i;
    end;

end;

// This function delivers the Row index connected to the Column at the input
// Inside the B2N incidence Matrix
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

// This function delivers the Column index connected to the Row at the input
// Inside the B2N incidence Matrix
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

// Routine for extracting the Branch to Node incidence matrix
//
// Organized hierarchically. This routine also calculates the
// Levels vector for defining the proximity of the bus to the circuit's
// Backbone. To do it, this routine uses the CktTree class
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
    ZeroLevel := 0;
    try
        if DSS.ActiveCircuit <> NIL then
        begin
            topo := DSS.ActiveCircuit.GetTopology;
            nLevels := 0;
            nPDE := 0;
            setlength(Inc_Mat_Cols, 0);
            //Init the sparse matrix
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
                    PDE_Name := PDElem.FullName;
                    // Gets the buses to which the PDE is connected
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
            // Now the levels array needs to be reprocessed to get the 0 level buses,
            // they are on a continuous path from the feeder head to the feeder end
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
            // **********Normalize the branches of the level between zero level buses********
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
            // ************Verifies if something else was missing at the end*****************
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

// This routine evaluates if the current location is the best or if its
// Necessary to move back one PDE just to cover a wider area
function TSolutionObj.CheckLocationIdx(Idx: Integer): Integer;
begin
    if Inc_Mat_Levels[Idx - 1] = 0 then
        Result := idx - 1
    else
        Result := idx;
end;

procedure TSolutionObj.GetPCInjCurr;
// Get inj currents from all enabled PC devices 
var
    pElem: TDSSCktElement;
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

procedure TSolutionObj.DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean);
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

    FSWriteln(F, 'Set Mode=', DSS.SolveModeEnum.OrdinalToString(ord(DSS.ActiveCircuit.Solution.mode)));
    FSWriteln(F, 'Set ControlMode=', DSS.ControlModeEnum.OrdinalToString(DSS.ActiveCircuit.Solution.Controlmode));
    FSWriteln(F, 'Set Random=', DSS.RandomModeEnum.OrdinalToString(DSS.ActiveCircuit.Solution.RandomType));
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
    FSWriteln(F, 'Set loadmodel=', DSS.DefaultLoadModelEnum.OrdinalToString(DSS.ActiveCircuit.solution.LoadModel));

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
    FSWriteln(F, Format('Set ueweight=%8.2f', [DSS.ActiveCircuit.UEWeight]));
    FSWriteln(F, Format('Set lossweight=%8.2f', [DSS.ActiveCircuit.LossWeight]));
    FSWriteln(F, 'Set ueregs=', IntArraytoString(DSS.ActiveCircuit.UEregs, DSS.ActiveCircuit.NumUERegs));
    FSWriteln(F, 'Set lossregs=', IntArraytoString(DSS.ActiveCircuit.Lossregs, DSS.ActiveCircuit.NumLossRegs));
    FSWrite(F, 'Set voltagebases=(');  //  changes the default voltage base rules
    i := 1;
    with DSS.ActiveCircuit do
        while LegalVoltageBases^[i] > 0.0 do
        begin
            FSWrite(F, Format('%10.2f', [LegalVoltageBases^[i]]));
            inc(i);
        end;
    FSWriteln(F, ')');
    FSWriteln(F, 'Set algorithm=' + DSS.SolveAlgEnum.OrdinalToString(Algorithm));
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
{$IFDEF DSS_CAPI_ADIAKOPTICS}
    if ADiakoptics and (DSS.Parent <> NIL) then
        Result := VoltInActor1(i) - VoltInActor1(j)  // V1-V2
    else
{$ENDIF}    
        Result := NodeV^[i] - NodeV^[j];  // V1-V2
end;

procedure TSolutionObj.WriteConvergenceReport(const Fname: String);
var
    i: Integer;
    F: TFileStream = nil;
    sout: String;
begin
    try
        F := TBufferedFileStream.Create(Fname, fmCreate);

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
                DoSimpleMsg(DSS, 'Error Sampling Control Device "%s". Error = %s', [ControlDevice.FullName, E.message], 484);
                raise EControlProblem.Create('Solution aborted.');
            end;
        end;
    end;
end;

procedure TSolutionObj.Sample_DoControlActions;
begin
    if ControlMode = CONTROLSOFF then
        ControlActionsDone := TRUE
    else
    begin
        SampleControlDevices;
        DoControlActions;

        // This variable lets control devices know the bus list has changed
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

    // Reset Meters and Monitors
    DSS.MonitorClass.ResetAll;
    DSS.EnergyMeterClass.ResetAll;
    DoResetFaults(DSS);
    DoResetControls(DSS);
end;

procedure TSolutionObj.AddInAuxCurrents(SolveType: Integer);
begin
    // For Now, only AutoAdd Obj uses this
    if Dynavars.SolutionMode = TSolveMode.AUTOADDFLAG then
        DSS.ActiveCircuit.AutoAddObj.AddCurrents(SolveType);
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

    end;
end;

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

    // When we go in and out of Dynamics mode, we have to do some special things
    if IsDynamicModel and not ValueIsDynamic then
        InvalidateAllPCELEMENTS(DSS);  // Force Recomp of YPrims when we leave Dynamics mode

    if not IsDynamicModel and ValueIsDynamic then
    begin   // see if conditions right for going into dynamics

        if DSS.ActiveCircuit.IsSolved then
            CalcInitialMachineStates(DSS)   // set state variables for machines (loads and generators)
        else
        begin
            // Raise Error Message if not solved
            DoSimpleMsg(DSS, _('Circuit must be solved in a non-dynamic mode before entering Dynamics or Fault study modes!') + CRLF +
                _('If you attempted to solve, then the solution has not yet converged.'), 486);
            if DSS.In_ReDirect then
                DSS.Redirect_Abort := TRUE;  // Get outta here
            Result := FALSE;
        end;
    end;
end;

function TSolutionObj.OK_for_Harmonics(const Value: TSolveMode): Boolean;
// When we go in and out of Harmonics mode, we have to do some special things
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
            DoSimpleMsg(DSS, _('Circuit must be solved in a fundamental frequency power flow or direct mode before entering Harmonics mode!'), 487);
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
            F := TBufferedFileStream.Create(DSS.OutputDirectory {CurrentDSSDir} + DSS.CircuitName_ + 'SavedVoltages.txt', fmCreate);

            with DSS.ActiveCircuit do
                for i := 1 to NumBuses do
                begin
                    BusName := BusList.NameOfIndex(i);
                    for j := 1 to Buses^[i].NumNodesThisBus do
                    begin
                        Volts := NodeV^[Buses^[i].RefNo[j]];
                        WriteStr(sout, BusName, ', ', Buses^[i].GetNum(j): 0, Format(', %-.7g, %-.7g', [Cabs(Volts), CDang(Volts)]));
                        FSWriteln(F, sout);
                    end;
                end;

        except
            On E: Exception do
            begin
                DoSimpleMsg(DSS, 'Error opening Saved Voltages File: %s', [E.message], 488);
                Exit;
            end;
        end;


    finally
        FreeAndNil(F);
        DSS.GlobalResult := DSS.OutputDirectory {CurrentDSSDir} + DSS.CircuitName_ + 'SavedVoltages.txt';

    end;
end;

// *************  MAIN SOLVER CALL  ************************

function TSolutionObj.SolveSystem(V: pNodeVArray): Integer;
var
    iRes: LongWord;
    dRes: Double;
begin
    Result := 0;

    // Note: NodeV[0] = 0 + j0 always.  Therefore, pass the address of the element 1 of the array.
    try
{$IFDEF DSS_CAPI_ADIAKOPTICS}
        if not ADiakoptics or (DSS.Parent = NIL) then
{$ENDIF}
            Result := SolveSparseSet(hY, pComplexArray(@V^[1]), pComplexArray(@Currents^[1])) // Solve for present InjCurr
{$IFDEF DSS_CAPI_ADIAKOPTICS}
        else
            Result := SolveSparseSet(hY, pComplexArray(@V^[LocalBusIdx[0]]), pComplexArray(@Currents^[1]));  // Solve for present InjCurr in Actor 1 context
{$ELSE}
        ;
{$ENDIF}

        if (DSS_CAPI_INFO_SPARSE_COND) then // Disabled by default with DSS C-API
        begin
            // new information functions
            GetFlops(hY, @dRes);
            GetRGrowth(hY, @dRes);
            GetRCond(hY, @dRes);
            // GetCondEst (hY, @dRes); // this can be expensive
            GetSize(hY, @iRes);
            GetNNZ(hY, @iRes);
            GetSparseNNZ(hY, @iRes);
            GetSingularCol(hY, @iRes);
        end;
    except
        On E: Exception do //Raise
        begin
            DoSimpleMsg(DSS, 'Error Solving System Y Matrix.  Sparse matrix solver reports numerical error: %s', [E.Message], 0);
            DSS.SolutionAbort := TRUE;
        end;
    end;
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
    {$ELSE}
    LoopEndTime := GetTickCount64;
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
                        VBus^[j] := NodeV^[RefNo[j]];
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
                        NodeV^[RefNo[j]] := VBus^[j];
end;

function TSolutionObj.SolveYDirect: Integer;
// Solves present Y matrix with no injection sources except voltage and current sources 
begin
    Result := 0;
{$IFDEF DSS_CAPI_ADIAKOPTICS}
    if not ADiakoptics or (DSS.Parent <> NIL) then
    begin
{$ENDIF}
        ZeroInjCurr; // Side Effect: Allocates InjCurr
        GetSourceInjCurrents;
        if IsDynamicModel then
            GetPCInjCurr; // Need this in dynamics mode to pick up additional injections

        SolveSystem(NodeV); // Solve with Zero injection current
{$IFDEF DSS_CAPI_ADIAKOPTICS}
    end
    else
    begin
        ADiak_PCInj := False;
        Solve_Diakoptics(DSS); // A-Diakoptics
    end;
{$ENDIF}    
end;

{$IFDEF DSS_CAPI_PM}
// Used to create the OpenDSS Solver thread
constructor TSolver.Create(dssContext: TDSSContext; Susp: Boolean; local_CPU: Integer; AEvent: TEvent);
begin
    DSS := dssContext;

    StatusEvent := AEvent;
    FreeOnTerminate := FALSE;
    // ActorID := ID;
    MessageQueueEvent := TEvent.Create(NIL, TRUE, FALSE, '');
    actorMessagesLock := SyncObjs.TCriticalSection.Create();
    ActorIsActive := TRUE;
    Processing := FALSE;
    actorMessages := TQueue<TActorMessage>.Create;

    inherited Create(Susp);
    if local_CPU >= 0 then
        Set_Thread_affinity(handle, local_CPU);
    // Parallel.Set_Thread_Priority(handle, THREAD_PRIORITY_TIME_CRITICAL);
end;

destructor TSolver.Destroy;
begin
    inherited destroy;
end;

// Send a message to the actor
procedure TSolver.Send_Message(Msg: TActorMessage);
begin
    try
        actorMessagesLock.Acquire();
        actorMessages.Enqueue(Msg);
        DSS.ActorStatus := TActorStatus.Busy;
        DSS.ThreadStatusEvent.SetEvent();
        MessageQueueEvent.SetEvent();
    finally
        actorMessagesLock.Release();
    end;
end;

// Returns the CPU assigned to the actor
function TSolver.Get_CPU(): Integer;
begin
    Result := DSS.CPU;
end;

// Sets the CPU assigned to the actor
procedure TSolver.Set_CPU(CPU: Integer);
begin
    DSS.CPU := CPU;
    Set_Thread_affinity(handle, CPU);
//  Parallel.Set_Thread_Priority(handle,THREAD_PRIORITY_TIME_CRITICAL);
end;

// Executes the selected solution algorithm
procedure TSolver.Execute;
var
    MsgType: TActorMessage;
begin
    with DSS.ActiveCircuit, Solution do
        while ActorIsActive do
        begin
            MessageQueueEvent.WaitFor(INFINITE);
            DSS.ActorStatus := TActorStatus.Busy;
            StatusEvent.SetEvent();
            while True do
            begin
                try
                    actorMessagesLock.Acquire();
                    if actorMessages.Count = 0  then
                        break;

                    //TODO: peak and check if last message is EXIT?
                    MsgType := actorMessages.Dequeue();                    
                finally
                    actorMessagesLock.Release();
                end;
                MessageQueueEvent.ResetEvent();
                
                Processing := TRUE;
                case MsgType of // Evaluates the incoming message
                    TActorMessage.SIMULATE: // Simulates the circuit on this actor
                        try
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
                                    SolveMonteFault; // Monte Carlo Fault Cases
                                TSolveMode.FAULTSTUDY:
                                    SolveFaultStudy;
                                TSolveMode.AUTOADDFLAG:
                                    AutoAddObj.Solve;
                                TSolveMode.HARMONICMODE:
                                    SolveHarmonic;
                                TSolveMode.GENERALTIME:
                                    SolveGeneralTime;
                                TSolveMode.HARMONICMODET:
                                    SolveHarmonicT; //Declares the Hsequential-time harmonics
                            else
                                DoSimpleMsg(DSS, _('Unknown solution mode.'), 481);
                            end;

                            {$IFDEF WINDOWS}
                            QueryPerformanceCounter(GEndTime);
                            {$ELSE}
                            GEndTime := GetTickCount64;
                            {$ENDIF}
                            Total_Solve_Time_Elapsed := ((GEndTime - GStartTime) / CPU_Freq) * 1000000;
                            Total_Time_Elapsed := Total_Time_Elapsed + Total_Solve_Time_Elapsed;
                            Processing := FALSE;
                            FMessage := '1';
                            // Sends a message to Actor Object (UI) to notify that the actor has finised
                            // UIEvent.SetEvent;
                            // if DSS.GetPrime().Parallel_enabled then
                            //     if not IsDLL then
                            //         queue(CallCallBack); // Refreshes the GUI if running asynchronously
                        except
                            On E: Exception do
                            begin
                                FMessage := '1';
                                DSS.ActorStatus := TActorStatus.Idle;
                                StatusEvent.SetEvent();
                                DSS.SolutionAbort := TRUE;
                                // if DSS.GetPrime().Parallel_enabled then
                                // begin
                                //     if not IsDLL then
                                //         queue(CallCallBack); // Refreshes the GUI if running asynchronously
                                // end
                                // else
                                    DoSimpleMsg(DSS, 'Error Encountered in Solve: %s', [E.Message], 482);
                            end;
                        end;
{$IFDEF DSS_CAPI_ADIAKOPTICS}
                    TActorMessage.INIT_ADIAKOPTICS:
                    begin
                        if DSS.Parent <> NIL then
                            Start_Diakoptics; // Initializes the actor for Diakoptics (if needed)
                        IndexBuses;
                    end;
                    TActorMessage.SOLVE_AD1:
                        SolveAD(True); // Solves the model if the actor has PIE
                    TActorMessage.SOLVE_AD2:
                        SolveAD(False); // Complements the solution
                    TActorMessage.GETCTRLMODE:
                    begin
                        // Brings the control mode from actor 1
                        ControlMode := DSS.Parent.ActiveCircuit.Solution.ControlMode;
                        DefaultControlMode := ControlMode;
                        MaxControlIterations := DSS.Parent.ActiveCircuit.Solution.MaxControlIterations;
                    end;
{$ENDIF}                        
                    TActorMessage.ZEROIVECTOR:
                        ZeroInjCurr;
                    TActorMessage.GETCURRINJ:
                        GetSourceInjCurrents;
                    TActorMessage.CALC_INJ_CURR:
                        GetPCInjCurr;
                    TActorMessage.DO_CTRL_ACTIONS:
                    begin
                        ControlActionsDone := FALSE;
                        Sample_DoControlActions;
                        try
                            actorMessagesLock.Acquire();
                            actorMessages.enqueue(TActorMessage.CHECK_FAULT);
                            actorMessages.enqueue(TActorMessage.CHECKYBUS);
                        finally
                            actorMessagesLock.Release();
                        end;
                    end;
                    TActorMessage.CHECK_FAULT:
                        Check_Fault_Status;
                    TActorMessage.CHECKYBUS:
                        if SystemYChanged {$IFDEF DSS_CAPI_INCREMENTAL_Y}or (IncrCktElements.Count <> 0){$ENDIF} then
                            BuildYMatrix(DSS, WHOLEMATRIX, FALSE);  // Does not realloc V, I
                    TActorMessage.EXIT_ACTOR: // Terminates the thread
                    begin
                        ActorIsActive := FALSE;
                        try
                            actorMessagesLock.Acquire();
                            actorMessages.Clear();
                        finally
                            actorMessagesLock.Release();
                        end;
                    end;
                else 
                    // I don't know what the message is
                    DoSimpleMsg(DSS, _('Unknown Message.'), 7010);
                end;

            end; // while actorMessages.Count > 0
            DSS.ActorStatus := TActorStatus.Idle;
            StatusEvent.SetEvent();
        end; // while ActorIsActive
end;

procedure TSolver.DoTerminate;        // Is the end of the thread
var
    ex: TObject;
begin
    ActorIsActive := FALSE;
    Processing := FALSE;
    DSS.ActorStatus := TActorStatus.Idle;
    StatusEvent.SetEvent();
    MessageQueueEvent.Free;
    actorMessagesLock.Free;
    actorMessages.Free;
    inherited;
end;

{$ENDIF} //DSS_CAPI_PM
{$IFDEF DSS_CAPI_ADIAKOPTICS}
function TSolutionObj.SolveAD(Initialize: Boolean): Integer;  // solves a step for Adiakoptics locally
begin
    Result := 0;
    if Initialize then
    begin
        ZeroInjCurr;
        GetSourceInjCurrents;  // sources
        if ADiak_PCInj then
        begin
            LoadsNeedUpdating := TRUE;  // Force the loads to update at least once
            GetPCInjCurr  // Get the injection currents from all the power conversion devices and feeders
        end
        else
        if IsDynamicModel or IsHarmonicModel then
        begin
            LoadsNeedUpdating := TRUE;  // Force the loads to update at least once
            GetPCInjCurr; // for direct solve
        end;
        // The above call could change the primitive Y matrix, so have to check
        if SystemYChanged {$IFDEF DSS_CAPI_INCREMENTAL_Y}or (DSS.ActiveCircuit.IncrCktElements.Count <> 0){$ENDIF} then
            BuildYMatrix(DSS, WHOLEMATRIX, FALSE);  // Does not realloc V, I

        if UseAuxCurrents then
            AddInAuxCurrents(NORMALSOLVE);
    end
    else
        UpdateISrc;

    // Solve for voltages -- Note:NodeV[0] = 0 + j0 always
    SolveSystem(DSS.Parent.ActiveCircuit.Solution.NodeV);
    LoadsNeedUpdating := FALSE;
    LastSolutionWasDirect := TRUE;
    DSS.ActiveCircuit.IsSolved := TRUE;
end;

procedure TSolutionObj.SendCmd2Actors(Msg: Integer);
var
    i: Integer;
    ChDSS: TDSSContext;
begin
    for i := 2 to DSS.NumOfActors do
    begin
        ChDSS := DSS.Children[i - 1];
        ChDSS.ActorStatus := TActorStatus.Busy;
        if ChDSS.ActorThread <> NIL then
            ChDSS.ActorThread.Send_Message(Msg);
    end;
    Wait4Actors(DSS, AD_ACTORS);
end;

// Initializes the variables of the A-Diakoptics worker
procedure TSolver.Start_Diakoptics();
var
    jj: Integer;
    VSourceObj: TVsourceObj;
    BusName: String;
    myPDEList: ArrayOfString;
begin
    with DSS.ActiveCircuit, Solution do
    begin
        // Select the main voltage source
        VSourceObj := DSS.VsourceClass.ElementList.First;
        // Gets the name of the branch directly connected to the feeder head to remove it
        // (applies to all actors but actor 2 - first chunk of the system)
        BusName := VSourceObj.GetBus(1);
        jj := ansipos('.', BusName);   // removes the dot
        if (jj > 0) then
            BusName := BusName.Substring(0, (jj - 1));
        SetActiveBus(DSS, BusName);                            // Activates the Bus
        myPDEList := getPDEatBus(BusList.NameOfIndex(ActiveBusIndex));
        // Disables the link branch
        DSS.DssExecutive.Command := myPDEList[0] + '.enabled=False'; //TODO: rewrite
        // Now disables all the VSources added artificially
        while VSourceObj <> NIL do
        begin
            BusName := Lowercase(VSourceObj.Name);
            if (BusName = 'source') then
                VSourceObj.Enabled := FALSE                   // Disables the artificial VSource phase 1
            else
            if (BusName = 'vph_2') then
                VSourceObj.Enabled := FALSE                 // Disables the artificial VSource phase 2
            else
            if (BusName = 'vph_3') then
                VSourceObj.Enabled := FALSE; // Disables the artificial VSource phase 3

            VSourceObj := DSS.VsourceClass.ElementList.Next;
        end;
    end;
end;

// // Uploads the local voltage array in the masters
// // using the index map obtained in previous steps
// procedure TSolutionObj.UploadV2Master;
// var
//     idx,
//     i: Integer;
// begin
//     with DSS.ActiveCircuit do
//     begin
//         for i := 1 to NumNodes do
//         begin
//             idx := LocalBusIdx[i - 1];
//             DSS.Parent.ActiveCircuit.Solution.NodeV^[idx] := Solution.NodeV^[i];
//         end;
//     end;
// end;

// Returns the voltage at the node given at NodeIdx in
// context of actor 1  (A-Diakoptics)
function TSolutionObj.VoltInActor1(NodeIdx: Integer): Complex;
begin
    if NodeIdx <> 0 then
        NodeIdx := NodeIdx + (LocalBusIdx[0] - 1);
    // In the context of actor 1
    Result := DSS.Parent.ActiveCircuit.Solution.NodeV^[NodeIdx];
end;

// Updates the local ISources using the data obtained
// for Ic in actor 1
procedure TSolutionObj.UpdateISrc;
var
    idx,
    i: Integer;
    Found: Boolean;
    myCmplx: Complex;

begin
    Found := FALSE;
    with DSS.ActiveCircuit, Solution do
    begin
        for i := 0 to (AD_IBus.Count - 1) do
        begin
            for idx := 0 to (DSS.Parent.ActiveCircuit.Ic.NZero - 1) do
            begin
                if DSS.Parent.ActiveCircuit.Ic.CData[idx].Row = AD_ISrcIdx.Items[i] then
                begin
                    Found := TRUE;
                    break;
                end;
            end;
            if Found then
            begin
                // Adds the present current with the adjustment
                myCmplx := DSS.Parent.ActiveCircuit.Ic.CData[idx].Value * (-1.0);
                Currents^[AD_IBus.Items[i]] := myCmplx + Currents^[AD_IBus.Items[i]];
            end;  // Otherwise is just another ISource in the zone
        end;
    end;
end;
// Checks if the actor has power injection Obj
// function TSolver.HasInjObj(): Boolean;
// var
//     ListSize,
//     jj: Integer;
//     VSourceObj: TVsourceObj;
//     ISourceObj: TIsourceObj;
// begin
//     if ActorID = 2 then
//         Result := TRUE
//     else
//     begin
//         Result := FALSE;
//         with DSS.ActiveCircuit, Solution do
//         begin
//             // Starts looking for VSource
//             VSourceObj := DSS.VsourceClass.ElementList.First;
//             while VSourceObj <> NIL do
//             begin
//                 if VSourceObj.enabled then
//                 begin
//                     Result := TRUE;
//                     break;
//                 end;
//                 VSourceObj := DSS.VsourceClass.ElementList.Next;
//             end;
//             if not Result then
//             begin
//                 // Goes for ISources
//                 ISourceObj := DSS.IsourceClass.ElementList.First;
//                 while ISourceObj <> NIL do
//                 begin
//                     if ISourceObj.enabled then
//                     begin
//                         Result := TRUE;
//                         break;
//                     end;
//                     ISourceObj := DSS.IsourceClass.ElementList.Next;
//                 end;
//             end;
//         end;
//     end;
// end;

// locates the local buses into actor 1's bus array
procedure TSolver.IndexBuses();
var
    Found: Boolean;
    k,
    i,
    j: Integer;
    myBus: String;
    LclBus,
    SrcBus: array of String;
begin
    // First, get the list of buses in actor 1
    setlength(SrcBus, 1);
    if DSS.Parent.ActiveCircuit <> NIL then
    begin
        with DSS.Parent.ActiveCircuit do
        begin
            for i := 1 to NumNodes do
            begin
                with MapNodeToBus^[i] do
                    SrcBus[high(SrcBus)] := Format('%s.%-d', [Uppercase(BusList.NameOfIndex(Busref)), NodeNum]);
                setlength(SrcBus, (length(SrcBus) + 1));
            end;
        end;
    end;
    // rebuilds the Y matrix to relocate the local buses
    BuildYMatrix(DSS, WHOLEMATRIX, TRUE);   // Side Effect: Allocates V
    // Then, get the list of buses in my actor
    setlength(LclBus, 1);
    if DSS.ActiveCircuit <> NIL then
    begin
        with DSS.ActiveCircuit do
        begin
            for i := 1 to NumNodes do
            begin
                with MapNodeToBus^[i] do
                    LclBus[high(LclBus)] := Format('%s.%-d', [Uppercase(BusList.NameOfIndex(Busref)), NodeNum]);
                setlength(LclBus, (length(LclBus) + 1));
            end;
        end;
    end;
    // Initializes the bus index vector
    with DSS.ActiveCircuit.solution do
    begin
        Setlength(LocalBusIdx, length(LclBus) - 1);
        for i := 0 to High(LocalBusIdx) do
        begin
            for j := 0 to High(SrcBus) do
                if LclBus[i] = SrcBus[j] then
                    break;
            LocalBusIdx[i] := j + 1;
        end;

        // Initializes the list for easy accessing the ADiakoptics Isources
        if AD_IBus = NIL then
            AD_IBus := TList<Integer>.Create
        else
            AD_IBus.Clear;
        if AD_ISrcIdx = NIL then
            AD_ISrcIdx := TList<Integer>.Create
        else
            AD_ISrcIdx.Clear;
        // Locates the ISource used for the local ADiakoptics algorithm
        for j := 0 to (DSS.Parent.ActiveCircuit.Contours.NZero - 1) do
        begin
            myBus := SrcBus[DSS.Parent.ActiveCircuit.Contours.CData[j].Row];
            // checks if the bus in in this circuit
            Found := FALSE;
            for k := 0 to High(LclBus) do
            begin
                if LclBus[k] = myBus then
                begin
                    Found := TRUE;
                    break;
                end;
            end;
            if Found then     // If found, add it to the indexed list
            begin
                AD_IBus.Add(k + 1);
                AD_ISrcIdx.Add(DSS.Parent.ActiveCircuit.Contours.CData[j].Row);
            end;
        end;

    end;
end;
{$ENDIF}
end.
