unit Solution;

// ----------------------------------------------------------
// Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

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
        
        AlwaysResetYPrimInvalid = $10000000 // Bit flag, see CktElement.pas
    );
{$SCOPEDENUMS OFF}
{$ENDIF}

    EControlProblem = class(Exception);
    ESolveError = class(Exception);  // Raised when solution aborted

    TNodeVarray = array[0..1000] of Complex;
    pNodeVarray = ^TNodeVarray;
{$IFDEF DSS_CAPI_PM}
    TSolutionObj = class;

    TSolver = class(TThread)
        constructor Create(sol: TSolutionObj; Susp: Boolean; local_CPU: Integer; AEvent: TEvent); OVERLOAD;
        procedure Execute; OVERRIDE;
        procedure Doterminate; OVERRIDE;
        destructor Destroy; OVERRIDE;

    PROTECTED
        DSS: TDSSContext;
        cktptr: Pointer; // Using a plain pointer to avoid circular references
        solution: TSolutionObj;

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

    PUBLIC
        DSS: TDSSContext;
        cktptr: Pointer;
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
        HarmonicList: ArrayOfDouble;
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
        NodeV: pNodeVArray;    // Main System Voltage Array   allows NodeV[0]=0
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

        constructor Create(dssContext: TDSSContext; dssCkt: Pointer; const solutionname: String);
        destructor Destroy; OVERRIDE;

        function Converged(): Boolean;
        procedure SetGeneratordQdV;

        procedure SolveZeroLoadSnapShot();
        procedure DoPFLOWsolution();

        procedure Solve();                // Main Solution dispatch
        procedure SnapShotInit();
        procedure SolveSnap();    // solve for now once
        procedure SolveDirect();  // solve for now once, direct solution
        procedure SolveYDirect(); // Similar to SolveDirect; used for initialization
        procedure SolveCircuit(); // SolveSnap sans control iteration
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

        procedure DumpProperties(F: TStream; Complete: Boolean; Leaf: Boolean = False);
        procedure WriteConvergenceReport(F: TStream);
        procedure Update_dblHour;
        procedure IncrementTime;

        procedure UpdateLoopTime;

        property Mode: TSolveMode READ dynavars.SolutionMode WRITE Set_Mode;
        property Frequency: Double READ FFrequency WRITE Set_Frequency;
        property Year: Integer READ FYear WRITE Set_Year;

        procedure AddInAuxCurrents(SolveType: Integer);
        function SolveSystem(V: pNodeVArray): Integer;
        procedure GetPCInjCurr(GFMOnly: Boolean = FALSE);
        procedure GetSourceInjCurrents;
        procedure ZeroInjCurr;
        procedure Upload2IncMatrix;

        procedure Calc_Inc_Matrix; // Calculates the incidence matrix for the Circuit
        procedure Calc_Inc_Matrix_Org; // Calculates the incidence matrix hierarchically organized for the Circuit

        function get_IncMatrix_Row(Col: Integer): Integer; // Gets the index of the Row connected to the specified Column
        function get_IncMatrix_Col(Row: Integer): Integer; // Gets the index of the Column connected to the specified Row
        function CheckLocationIdx(Idx: Integer): Integer; // Evaluates the area covered by the tearing point to see if there is a better one

        procedure AddLines2IncMatrix; // Adds the Lines to the Incidence matrix arrays
        procedure AddXfmr2IncMatrix; // Adds the Xfmrs to the Incidence matrix arrays
        procedure AddSeriesCap2IncMatrix; // Adds capacitors in series to the Incidence matrix arrays
        procedure AddSeriesReac2IncMatrix; // Adds Reactors in series to the Incidence matrix arrays

        function TimeOfDay(useEpsilon: Boolean = false): Double;
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
    Bus,
    Line,
    InvBasedPCE,
    PCElement,
{$IFDEF DSS_CAPI_ADIAKOPTICS}
    Diakoptics,
{$ENDIF}
    DSSHelper,
    StrUtils;

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

constructor TSolutionObj.Create(dssContext: TDSSContext; dssCkt: Pointer; const SolutionName: String);
begin
    inherited Create;
    // Name := AnsiLowerCase(SolutionName);
    DSS := dssContext;
    cktptr := dssCkt;

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
    SetLength(HarmonicList, 5);
    HarmonicList[0] := 1.0;
    HarmonicList[1] := 5.0;
    HarmonicList[2] := 7.0;
    HarmonicList[3] := 11.0;
    HarmonicList[4] := 13.0;

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

{$IFDEF DSS_CAPI_PM}    
    // Sends a message to the working actor
    // DSS.ThreadStatusEvent.SetEvent();
    if DSS.ActorThread <> NIL then
    begin
        DSS.SolutionAbort := True;
        DSS.ActorThread.Send_Message(TActorMessage.EXIT_ACTOR);
        DSS.ActorThread.WaitFor();
        DSS.ActorThread.Free();
        DSS.ActorThread := nil;
    end;
    DSS.ThreadStatusEvent.Free;
    DSS.ThreadStatusEvent := NIL;
{$ENDIF}
{$IFDEF DSS_CAPI_ADIAKOPTICS}
    LockNodeV.Free;
{$ENDIF}

    inherited Destroy;
end;

procedure TSolutionObj.Solve;
var
{$IFDEF DSS_CAPI_PM}
    PMParent: TDSSContext;
{$ENDIF}
begin
{$IFDEF DSS_CAPI_PM}
    PMParent := DSS.GetPrime();
{$ENDIF}
    ckt.Issolved := FALSE;
    DSS.SolutionWasAttempted := TRUE;

    DSS.InitProgressForm(); // initialize Progress Form;

    // Check of some special conditions that must be met before executing solutions
    if ckt.EmergMinVolts >= ckt.NormalMinVolts then
    begin
        DoSimpleMsg(DSS, _('Error: Emergency Min Voltage Must Be Less Than Normal Min Voltage! Solution Not Executed.'), 480);
        Exit;
    end;

    if DSS.SolutionAbort then
    begin
        DSS.GlobalResult := 'Solution aborted.';
        DSS.ErrorNumber := SOLUTION_ABORT;
        Exit;
    end;
    try
        // Main solution Algorithm dispatcher
        if Year = 0 then
            ckt.DefaultGrowthFactor := 1.0    // RCD 8-17-00
        else
            ckt.DefaultGrowthFactor := IntPower(ckt.DefaultGrowthRate, (year - 1));
        DSS.SignalEvent(TAltDSSEvent.Legacy_InitControls);

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
                    SolveSnap();
                TSolveMode.YEARLYMODE:
                    SolveYearly();
                TSolveMode.DAILYMODE:
                    SolveDaily();
                TSolveMode.DUTYCYCLE:
                    SolveDuty();
                TSolveMode.DYNAMICMODE:
                    SolveDynamic();
                TSolveMode.MONTECARLO1:
                    SolveMonte1();
                TSolveMode.MONTECARLO2:
                    SolveMonte2();
                TSolveMode.MONTECARLO3:
                    SolveMonte3();
                TSolveMode.PEAKDAY:
                    SolvePeakDay();
                TSolveMode.LOADDURATION1:
                    SolveLD1();
                TSolveMode.LOADDURATION2:
                    SolveLD2();
                TSolveMode.DIRECT:
                    SolveDirect();
                TSolveMode.MONTEFAULT:
                    SolveMonteFault();  // Monte Carlo Fault Cases
                TSolveMode.FAULTSTUDY:
                    SolveFaultStudy();
                TSolveMode.AUTOADDFLAG:
                    ckt.AutoAddObj.Solve();
                TSolveMode.HARMONICMODE:
                    SolveHarmonic();
                TSolveMode.GENERALTIME:
                    SolveGeneralTime();
                TSolveMode.HARMONICMODET:
                    SolveHarmonicT();  //Declares the Hsequential-time harmonics
            else
                DoSimpleMsg(DSS, _('Unknown solution mode.'), 481);
            end;
            {$IFDEF WINDOWS}
            QueryPerformanceCounter(GEndTime);
            {$ELSE}
            GEndTime := GetTickCount64();
            {$ENDIF}
            Total_Solve_Time_Elapsed := ((GEndTime - GStartTime) / CPU_Freq) * 1000000;
            Total_Time_Elapsed += Total_Solve_Time_Elapsed;
            Exit;
{$IFDEF DSS_CAPI_PM}
        end;
        // Creates the actor again in case of being terminated due to an error before
        if (DSS.ActorThread = NIL) or DSS.ActorThread.Terminated then
        begin
            if (DSS.ActorThread <> NIL) and DSS.ActorThread.Terminated then
                DSS.ActorThread.Free;

            DSS.ActorThread := TSolver.Create(self, True, DSS.CPU, DSS.ThreadStatusEvent);
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
    for i := 1 to ckt.NumNodes do
    begin
{$IFDEF DSS_CAPI_ADIAKOPTICS}
        if not ADiakoptics or (DSS.Parent = NIL) then
{$ENDIF}
            VMag := Cabs(NodeV[i])
{$IFDEF DSS_CAPI_ADIAKOPTICS}
        else
            VMag := Cabs(VoltInActor1(i));
{$ELSE}
        ;
{$ENDIF}
        // If base specified, use it; otherwise go on present magnitude
        if NodeVbase[i] > 0.0 then
            ErrorSaved[i] := Abs(Vmag - VmagSaved[i]) / NodeVbase[i]
        else
        if Vmag <> 0.0 then
            ErrorSaved[i] := Abs(1.0 - VmagSaved[i] / Vmag);

        VMagSaved[i] := Vmag;  // for next go-'round
        MaxError := Math.Max(MaxError, ErrorSaved[i]);  // update max error
    end;

{$IFNDEF DSS_CAPI_NOCOMPATFLAGS}
    if (DSS_EXTENSIONS_COMPAT and ord(TDSSCompatFlags.NoSolverFloatChecks)) = 0 then
{$ENDIF}
        Result := (MaxError <= ConvergenceTolerance) and (not IsNaN(MaxError)) and (not IsInfinite(MaxError))
{$IFNDEF DSS_CAPI_NOCOMPATFLAGS}
    else 
    if IsNaN(MaxError) or IsInfinite(MaxError) then
    begin
        Result := ConvergedFlag; // 
        Exit;
    end;
{$ENDIF}
    ConvergedFlag := Result;
end;

procedure TSolutionObj.GetSourceInjCurrents;
// Add in the contributions of all source type elements to the global solution vector InjCurr
var
    pElem: TDSSCktElement;
begin
    for pElem in ckt.Sources do
    begin
        if pElem.Enabled then
            pElem.InjCurrents(); // uses NodeRef to add current into InjCurr Array;
    end;

    // Adds GFM PCE as well
    GetPCInjCurr(TRUE);
end;

procedure TSolutionObj.SetGeneratorDispRef;
// Set the global generator dispatch reference
begin
    case Dynavars.SolutionMode of
        TSolveMode.SNAPSHOT:
            ckt.GeneratorDispatchReference := ckt.LoadMultiplier * ckt.DefaultGrowthFactor;
        TSolveMode.YEARLYMODE:
            ckt.GeneratorDispatchReference := ckt.DefaultGrowthFactor * ckt.DefaultHourMult.re;
        TSolveMode.DAILYMODE:
            ckt.GeneratorDispatchReference := ckt.LoadMultiplier * ckt.DefaultGrowthFactor * ckt.DefaultHourMult.re;
        TSolveMode.DUTYCYCLE:
            ckt.GeneratorDispatchReference := ckt.LoadMultiplier * ckt.DefaultGrowthFactor * ckt.DefaultHourMult.re;
        TSolveMode.GENERALTIME:
            ckt.GeneratorDispatchReference := ckt.LoadMultiplier * ckt.DefaultGrowthFactor * ckt.DefaultHourMult.re;
        TSolveMode.DYNAMICMODE:
            ckt.GeneratorDispatchReference := ckt.LoadMultiplier * ckt.DefaultGrowthFactor;
        TSolveMode.HARMONICMODE:
            ckt.GeneratorDispatchReference := ckt.LoadMultiplier * ckt.DefaultGrowthFactor;
        TSolveMode.MONTECARLO1:
            ckt.GeneratorDispatchReference := ckt.LoadMultiplier * ckt.DefaultGrowthFactor;
        TSolveMode.MONTECARLO2:
            ckt.GeneratorDispatchReference := ckt.LoadMultiplier * ckt.DefaultGrowthFactor * ckt.DefaultHourMult.re;
        TSolveMode.MONTECARLO3:
            ckt.GeneratorDispatchReference := ckt.LoadMultiplier * ckt.DefaultGrowthFactor * ckt.DefaultHourMult.re;
        TSolveMode.PEAKDAY:
            ckt.GeneratorDispatchReference := ckt.LoadMultiplier * ckt.DefaultGrowthFactor * ckt.DefaultHourMult.re;
        TSolveMode.LOADDURATION1:
            ckt.GeneratorDispatchReference := ckt.LoadMultiplier * ckt.DefaultGrowthFactor * ckt.DefaultHourMult.re;
        TSolveMode.LOADDURATION2:
            ckt.GeneratorDispatchReference := ckt.LoadMultiplier * ckt.DefaultGrowthFactor * ckt.DefaultHourMult.re;
        TSolveMode.DIRECT:
            ckt.GeneratorDispatchReference := ckt.LoadMultiplier * ckt.DefaultGrowthFactor;
        TSolveMode.MONTEFAULT:
            ckt.GeneratorDispatchReference := 1.0;  // Monte Carlo Fault Cases solve  at peak load only base case
        TSolveMode.FAULTSTUDY:
            ckt.GeneratorDispatchReference := 1.0;
        TSolveMode.AUTOADDFLAG:
            ckt.GeneratorDispatchReference := ckt.DefaultGrowthFactor;   // peak load only
        TSolveMode.HARMONICMODET:
            ckt.GeneratorDispatchReference := ckt.LoadMultiplier * ckt.DefaultGrowthFactor * ckt.DefaultHourMult.re;
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
    GenDispSave := ckt.GeneratorDispatchReference;
    ckt.GeneratorDispatchReference := 1000.0;

    for pGen in ckt.Generators do
    begin
        if not pGen.Enabled then
            continue;

        // for PV generator models only ...
        if pGen.genModel = 3 then
        begin
            pGen.InitDQDVCalc();

            // NOTE: The following was commented in https://sourceforge.net/p/electricdss/code/3534/
            //       The Y matrix element is used since then.
            //
            //    // solve at base var setting
            //     Iteration := 0;
            //     repeat
            //         Inc(Iteration);
            //         ZeroInjCurr;
            //         GetSourceInjCurrents;
            //         pGen.InjCurrents;   // get generator currents with nominal vars
            //         SolveSystem(NodeV);
            //     until Converged or (Iteration >= Maxiterations);
            //
            //     pGen.RememberQV;  // Remember Q and V
            //     pGen.BumpUpQ;
            //
            //    // solve after changing vars
            //     Iteration := 0;
            //     repeat
            //         Inc(Iteration);
            //         ZeroInjCurr;
            //         GetSourceInjCurrents;
            //         pGen.InjCurrents;   // get generator currents with nominal vars
            //         SolveSystem(NodeV);
            //     until Converged or (Iteration >= Maxiterations);

            pGen.CalcdQdV(); // bssed on remembered Q and V and present values of same
            pGen.ResetStartPoint();

            Did_One := TRUE;
        end;
    end;

    // Restore generator dispatch reference
    ckt.GeneratorDispatchReference := GenDispSave;
    try
        if Did_One        // Reset Initial Solution
        then
            SolveZeroLoadSnapShot();
    except
        ON E: EEsolv32Problem do
        begin
            DoSimpleMsg(DSS, 'From SetGenerator DQDV, SolveZeroLoadSnapShot: %s', [CRLF + E.Message + CheckYMatrixforZeroes(ckt)], 7071);
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
    repeat
        Inc(Iteration);

        if ckt.LogEvents then
            DSS.LogThisEvent('Solution Iteration ' + IntToStr(Iteration));

{$IFDEF DSS_CAPI_ADIAKOPTICS}
        if (not ADiakoptics) or (DSS.Parent <> NIL) then // Normal simulation
        begin // In A-Diakoptics, all other actors do normal solution
{$ENDIF}
            // Get injcurrents for all PC devices
            ZeroInjCurr();
            GetSourceInjCurrents();  // sources
            GetPCInjCurr();  // Get the injection currents from all the power conversion devices and feeders

            // The above call could change the primitive Y matrix, so have to check
            if SystemYChanged {$IFDEF DSS_CAPI_INCREMENTAL_Y}or (ckt.IncrCktElements.Count <> 0){$ENDIF} then
            begin
                BuildYMatrix(DSS, WHOLEMATRIX, FALSE);  // Does not realloc V, I
            end;
            if UseAuxCurrents then
                AddInAuxCurrents(NORMALSOLVE);

            // Solve for voltages -- Note: NodeV[0] = 0 + j0 always
            if ckt.LogEvents then
                DSS.LogThisEvent('Solve Sparse Set DoNormalSolution ...');
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
    ReAllocMem(dV, SizeOf(dV[1]) * (ckt.NumNodes + 1)); // Make sure this is always big enough

    if ControlIteration = 1 then
        GetPCInjCurr;  // Update the load multipliers for this solution

    Iteration := 0;
    repeat
        Inc(Iteration);
        Inc(SolutionCount);    // SumAllCurrents Uses ITerminal  So must force a recalc

        // Get sum of currents at all nodes for all  devices
        ZeroInjCurr();
        SumAllCurrents();

        // Call to current calc could change YPrim for some devices
        if SystemYChanged {$IFDEF DSS_CAPI_INCREMENTAL_Y}or (ckt.IncrCktElements.Count <> 0){$ENDIF} then
        begin
            BuildYMatrix(DSS, WHOLEMATRIX, FALSE);   // Does not realloc V, I
        end;

        if UseAuxCurrents then
            AddInAuxCurrents(NEWTONSOLVE);

        // Solve for change in voltages
        SolveSystem(dV);

        LoadsNeedUpdating := FALSE;

        // Compute new guess at voltages
        for i := 1 to ckt.NumNodes do     // 0 node is always 0
        begin
            NodeV[i] -= dV[i];
        end;

    until (Converged and (Iteration >= MinIterations)) or (Iteration >= MaxIterations);
end;

procedure TSolutionObj.DoPFLOWsolution;
begin
    Inc(SolutionCount);    //Unique number for this solution

    if VoltageBaseChanged then
        InitializeNodeVbase(ckt); // for convergence test

    if not SolutionInitialized then
    begin
        if ckt.LogEvents then
            DSS.LogThisEvent('Initializing Solution');
        try
            //SolveZeroLoadSnapShot;
            SolveYDirect;  // 8-14-06 This should give a better answer than zero load snapshot
        except
            ON E: EEsolv32Problem do
            begin
                DoSimpleMsg(DSS, 'From DoPFLOWsolution.SolveYDirect: %s', [CRLF + E.Message + CheckYMatrixforZeroes(ckt)], 7072);
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
                DoSimpleMsg(DSS, 'From DoPFLOWsolution.SetGeneratordQdV: %s', [CRLF + E.Message + CheckYMatrixforZeroes(ckt)], 7073);
                raise ESolveError.Create('Aborting');
            end;
        end;

        if DSS.SolutionAbort then
            Exit;

        // The above resets the active sparse set to hY
        SolutionInitialized := TRUE;
    end;

    case Algorithm of
        NEWTONSOLVE:
            DoNewtonSolution;
        else // was NORMALSOLVE:
            DoNormalSolution;
    end;

    ckt.Issolved := ConvergedFlag;
    LastSolutionWasDirect := FALSE;
end;

procedure TSolutionObj.SolveZeroLoadSnapShot();
// Solve without load for initialization purposes;
begin
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

    if ckt.LogEvents then
        DSS.LogThisEvent('Solve Sparse Set ZeroLoadSnapshot ...');

    SolveSystem(NodeV);  // also sets voltages in radial part of the circuit if radial solution

    // Reset the main system Y as the solution matrix
    if (hYsystem > 0) and not DSS.SolutionAbort then
        hY := hYsystem;
end;

function nearestBasekV(DSS: TDSSContext; kV: Double): Double;
// Find closest base voltage
var
    TestkV: Double;
    i: Integer;
    Diff,
    MinDiff: Double;
begin
    Result := 0;
    MinDiff := 1.0E50;  // Big whompin number
    for i := 0 to High(DSS.ActiveCircuit.LegalVoltageBases) do
    begin
        TestkV := DSS.ActiveCircuit.LegalVoltageBases[i];
        if TestkV = 0 then
            break;

        Diff := Abs(1.0 - kV / TestkV);     // Get Per unit difference
        if Diff < MinDiff then
        begin
            MinDiff := Diff;
            Result := TestkV;
        end;
    end;
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

        bZoneCalc := ckt.MeterZonesComputed;
        bZoneLock := ckt.ZonesLocked;
        ckt.MeterZonesComputed := TRUE;
        ckt.ZonesLocked := TRUE;

        SolveZeroLoadSnapShot;

        for i := 1 to ckt.NumBuses do
            ckt.Buses[i].kVBase := nearestBasekV(DSS, Cabs(NodeV[ckt.Buses[i].RefNo[1]]) * 0.001732) / SQRT3;  // l-n base kV

        InitializeNodeVbase(ckt); // for convergence test

        ckt.IsSolved := TRUE;

        // now build the meter zones
        ckt.MeterZonesComputed := bZoneCalc;
        ckt.ZonesLocked := bZoneLock;
        ckt.DoResetMeterZones;

    except
        ON E: EEsolv32Problem do
        begin
            DoSimpleMsg(DSS, 'From SetVoltageBases.SolveZeroLoadSnapShot: %s', [CRLF + E.Message + CheckYMatrixforZeroes(ckt)], 7075);
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
{$IFDEF DSS_CAPI_ADIAKOPTICS}
var
     i: Integer;
{$ENDIF}
begin
{$IFDEF DSS_CAPI_ADIAKOPTICS}
    if not ADiakoptics or (DSS.Parent <> NIL) then
    begin
{$ENDIF}
        if ControlIteration < MaxControlIterations then
        begin
            if ConvergedFlag then
            begin
                if ckt.LogEvents then
                    DSS.LogThisEvent('Control Iteration ' + IntToStr(ControlIteration));
                Sample_DoControlActions;
                Check_Fault_Status;
            end
            else
                ControlActionsDone := TRUE; // Stop solution process if failure to converge
        end;

        if SystemYChanged {$IFDEF DSS_CAPI_INCREMENTAL_Y}or (ckt.IncrCktElements.Count <> 0){$ENDIF} then
        begin
            BuildYMatrix(DSS, WHOLEMATRIX, FALSE); // Rebuild Y matrix, but V stays same
        end;
{$IFDEF DSS_CAPI_ADIAKOPTICS}
    end
    else
    begin
        if ControlIteration < MaxControlIterations then
        begin
            if ckt.LogEvents then
                DSS.LogThisEvent('Control Iteration ' + IntToStr(ControlIteration));

            SendCmd2Actors(DO_CTRL_ACTIONS);
            // Checks if there are pending ctrl actions at the actors
            ControlActionsDone := TRUE;
            for i := 2 to DSS.NumOfActors do
                ControlActionsDone := ControlActionsDone and DSS.Children[i - 1].ActiveCircuit.Solution.ControlActionsDone;
        end;
    end;
{$ENDIF}
end;

procedure TSolutionObj.SolveSnap();  // solve for now once
var
    TotalIterations: Integer;
begin
    SnapShotInit();
    TotalIterations := 0;
   {$IFDEF MSWINDOWS}
    QueryPerformanceCounter(SolveStartTime);
   {$ELSE}
    SolveStartTime := GetTickCount64;
   {$ENDIF}
    repeat
        Inc(ControlIteration);

        SolveCircuit();  // Do circuit solution w/o checking controls
        // Now Check controls
        DSS.SignalEvent(TAltDSSEvent.Legacy_CheckControls);
        CheckControls;

        // For reporting max iterations per control iteration
        if Iteration > MostIterationsDone then
            MostIterationsDone := Iteration;

        TotalIterations := TotalIterations + Iteration;

    until ControlActionsDone or (ControlIteration >= MaxControlIterations);

    if not ControlActionsDone and (ControlIteration >= MaxControlIterations) then
    begin
        DoSimpleMsg(DSS, _('Warning Max Control Iterations Exceeded.') + CRLF + _('Tip: Show Eventlog to debug control settings.'), 485);
        DSS.SolutionAbort := TRUE;   // this will stop this message in dynamic power flow modes
    end;

    if ckt.LogEvents then
        DSS.LogThisEvent('Solution Done');

    DSS.SignalEvent(TAltDSSEvent.Legacy_StepControls);

    {$IFDEF MSWINDOWS}
    QueryPerformanceCounter(SolveEndTime);
    {$ELSE}
    SolveEndTime := GetTickCount64;
    {$ENDIF}
    Solve_Time_Elapsed := ((SolveEndtime - SolveStartTime) / CPU_Freq) * 1000000;
    Iteration := TotalIterations;  //  so that it reports a more interesting number 

end;

procedure TSolutionObj.SolveDirect();  // solve for now once, direct solution
begin
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
            ckt.IsSolved := TRUE;
            ConvergedFlag := TRUE;
        end;
{$IFDEF DSS_CAPI_ADIAKOPTICS}
    end
    else
    begin
        ADiak_PCInj := FALSE;
        Solve_Diakoptics(DSS); // A-Diakoptics

        ckt.IsSolved := TRUE;
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

procedure TSolutionObj.SolveCircuit();
begin
    if LoadModel = ADMITTANCE then
        try
            SolveDirect     // no sense horsing around when it's all admittance
        except
            ON E: EEsolv32Problem do
            begin
                DoSimpleMsg(DSS, 'From SolveSnap.SolveDirect: %s', [CRLF + E.Message + CheckYMatrixforZeroes(ckt)], 7075);
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

            if DSS.SolutionAbort then
                Exit;

            DoPFLOWsolution;

            if DSS.SolutionAbort then
                Exit;
        except
            ON E: EEsolv32Problem do
            begin
                DoSimpleMsg(DSS, 'From SolveSnap.DoPflowSolution: %s', [CRLF + E.Message + CheckYMatrixforZeroes(ckt)], 7074);
                raise ESolveError.Create('Aborting');
            end;
        end
    end;
end;

procedure TSolutionObj.ZeroInjCurr;
var
    I: Integer;
begin
    if Currents = NIL then
    begin
        DoSimpleMsg(DSS, _('General error: internal Currents vector is NIL. Please check your input data and retry.'), 11002);
        DSS.SolutionAbort := True;
        Exit;
    end;

    for i := 0 to ckt.NumNodes do
        Currents[i] := 0;
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
    for elem in ckt.Lines do
    begin
        if not elem.Enabled then
            continue;

        ActiveIncCell[2] := 1;
        inc(temp_counter);
        setlength(Inc_Mat_Rows, temp_counter);
        Inc_Mat_Rows[temp_counter - 1] := elem.FullName;
        for TermIdx := 1 to 2 do
        begin
            LineBus := elem.GetBus(TermIdx);
            BusdotIdx := ansipos('.', LineBus);
            if BusdotIdx <> 0 then
                LineBus := Copy(LineBus, 0, BusdotIdx - 1);  // removes the dot from the Bus Name
            // Evaluates the position of the Bus in the array
            ActiveIncCell[1] := 1;
            EndFlag := TRUE;
            while (ActiveIncCell[1] <= ckt.NumBuses) and (EndFlag) do
            begin
                if LineBus = ckt.BusList.NameOfIndex(ActiveIncCell[1]) then
                    EndFlag := FALSE;
                ActiveIncCell[1] := ActiveIncCell[1] + 1;
            end;
            Upload2IncMatrix();
        end;
        inc(ActiveIncCell[0]);
    end;
end;

procedure TSolutionObj.AddXfmr2IncMatrix;
var
    LineBus: String;
    elem: TTransfObj;
    TermIdx,
    BusdotIdx: Integer;
    EndFlag: Boolean;
begin
    // This routine adds the Transformers to the incidence matrix vectors
    for elem in ckt.Transformers do
    begin
        if not elem.Enabled then
            continue;
        
        ActiveIncCell[2] := 1;
        inc(temp_counter);
        setlength(Inc_Mat_Rows, temp_counter);
        Inc_Mat_Rows[temp_counter - 1] := elem.FullName;
        for TermIdx := 1 to elem.NumWindings do
        begin
            LineBus := elem.GetBus(TermIdx);
            BusdotIdx := ansipos('.', LineBus);
            if BusdotIdx <> 0 then
                LineBus := Copy(LineBus, 0, BusdotIdx - 1);  // removes the dot from the Bus Name
            // Evaluates the position of the Bus in the array
            ActiveIncCell[1] := 1;
            EndFlag := TRUE;
            while (ActiveIncCell[1] <= ckt.NumBuses) and (EndFlag) do
            begin
                if LineBus = ckt.BusList.NameOfIndex(ActiveIncCell[1]) then
                    EndFlag := FALSE;
                ActiveIncCell[1] := ActiveIncCell[1] + 1;
            end;
            Upload2IncMatrix();
        end;
        inc(ActiveIncCell[0]);
    end;
end;

procedure TSolutionObj.AddSeriesCap2IncMatrix;
var
    CapBus: String;
    elem: TCapacitorObj;
    CapTermIdx,
    BusdotIdx: Integer;
    CapEndFlag: Boolean;
begin
    // This routine adds the series capacitors to the incidence matrix vectors
    for elem in ckt.ShuntCapacitors do
    begin
        if not (elem.NumTerminals > 1) or not elem.Enabled then
            continue;

        inc(temp_counter);
        setlength(Inc_Mat_Rows, temp_counter);
        Inc_Mat_Rows[temp_counter - 1] := elem.FullName;
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
            while (ActiveIncCell[1] <= ckt.NumBuses) and (CapEndFlag) do
            begin
                if CapBus = ckt.BusList.NameOfIndex(ActiveIncCell[1]) then
                    CapEndFlag := FALSE;
                ActiveIncCell[1] := ActiveIncCell[1] + 1;
            end;
            Upload2IncMatrix();
        end;
        inc(ActiveIncCell[0]);
    end;
end;

procedure TSolutionObj.AddSeriesReac2IncMatrix;
var
    RBus: String;
    TermIdx,
    BusdotIdx: Integer;
    EndFlag: Boolean;
    elem: TDSSCktElement;
begin
    // This routine adds the series reactors to the incidence matrix vectors
    for elem in DSS.ReactorClass do
    begin
        RBus := elem.GetBus(2);
        BusdotIdx := ansipos('.0', RBus);
        if BusdotIdx = 0 then
        begin
            inc(temp_counter);
            setlength(Inc_Mat_Rows, temp_counter);
            Inc_Mat_Rows[temp_counter - 1] := elem.FullName;
            ActiveIncCell[2] := 1;
            for TermIdx := 1 to 2 do
            begin
                RBus := elem.GetBus(TermIdx);
                BusdotIdx := ansipos('.', RBus);
                if BusdotIdx <> 0 then
                    RBus := Copy(RBus, 0, BusdotIdx - 1);  // removes the dot from the Bus Name
                // Evaluates the position of the Bus in the array
                ActiveIncCell[1] := 1;
                EndFlag := TRUE;
                while (ActiveIncCell[1] <= ckt.NumBuses) and (EndFlag) do
                begin
                    if RBus = ckt.BusList.NameOfIndex(ActiveIncCell[1]) then
                        EndFlag := FALSE;
                    ActiveIncCell[1] := ActiveIncCell[1] + 1;
                end;
                Upload2IncMatrix();
            end;
        end;
        inc(ActiveIncCell[0]);
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

    if cktptr = NIL then //TODO: is this even possible?
        Exit; 

    temp_counter := 0;
    ActiveIncCell[0] := 1;           // Activates row 1 of the incidence matrix
    // Now we proceed to evaluate the link branches
    AddLines2IncMatrix;      // Includes the Lines
    AddXfmr2IncMatrix;       // Includes the Xfmrs
    AddSeriesCap2IncMatrix;  // Includes Series Cap
    AddSeriesReac2IncMatrix; // Includes Series Reactors
    DSS.IncMat_Ordered := FALSE;
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

    if ckt = NIL then //TODO: is this even possible?
        Exit; 
    
    topo := ckt.GetTopology;
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
        PDElem := topo.First();
        while Assigned(PDElem) do
        begin
            nLevels := topo.Level;
            PDE_Name := PDElem.FullName;
            // Gets the buses to which the PDE is connected
            ckt.SetElementActive(PDE_Name);
            SetLength(PDE_Buses, ckt.ActiveCktElement.Nterms);
            for i := 1 to ckt.ActiveCktElement.Nterms do
            begin
                PDE_Buses[i - 1] := ckt.ActiveCktElement.GetBus(i);
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
                for j := 0 to ckt.ActiveCktElement.Nterms - 1 do
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
            inc(ActiveIncCell[0]);
            PDElem := topo.GoForward();
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
            if length(Temp_Array) > 0 then // The array subset is large enough for
            begin //Normalizing it
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
        BusdotIdx := 0; // Counter for defining the level
        j := 0; // Stores the previous value (shift reg)
        for j2 := ZeroLevel to High(Inc_Mat_levels) do
        begin
            if Inc_Mat_levels[j2] >= j then
                inc(BusdotIdx)
            else
            begin
                ActiveIncCell[1] := get_IncMatrix_Row(j2); //Looks for the Column in the IncMatrix
                if ActiveIncCell[1] < 0 then //Checks if the col was located (just in case)
                    BusdotIdx := 1
                else
                begin
                    ActiveIncCell[2] := get_IncMatrix_Col(ActiveIncCell[1]); //Looks for the row in the IncMatrix
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

// This routine evaluates if the current location is the best or if its
// Necessary to move back one PDE just to cover a wider area
function TSolutionObj.CheckLocationIdx(Idx: Integer): Integer;
begin
    if Inc_Mat_Levels[Idx - 1] = 0 then
        Result := idx - 1
    else
        Result := idx;
end;

procedure TSolutionObj.GetPCInjCurr(GFMOnly: Boolean = FALSE);
// Get inj currents from all enabled PC devices 
var
    pElem: TDSSCktElement;
    valid, onGFM: Boolean;
begin
    for pElem in ckt.PCElements do
    begin
        onGFM := ((pElem is TInvBasedPCE) and (TInvBasedPCE(pElem).GFM_Mode));
        valid := (not (GFMOnly xor onGFM)) and pElem.Enabled;
        //TODO: depending on the system size, a dedicated list could be faster/better
        // e.g. could check the lists from Circuit directly instead of looping through all elements
        if valid then
            pElem.InjCurrents(); // uses NodeRef to add current into InjCurr Array;
    end;
end;

procedure TSolutionObj.DumpProperties(F: TStream; Complete: Boolean; Leaf: Boolean);
var
    i, j: Integer;

    // for dumping the matrix in compressed columns
    p: Longword;
    nBus, nNZ: Longword;
    ColPtr, RowIdx: array of Longword;
    cVals: array of Complex;
    harm: Double;
begin
    FSWriteln(F, '! OPTIONS');

    // Inherited DumpProperties(F,Complete);

    FSWriteln(F, '! NumNodes = ', IntToStr(ckt.NumNodes));

    FSWriteln(F, 'Set Mode=', DSS.SolveModeEnum.OrdinalToString(ord(mode)));
    FSWriteln(F, 'Set ControlMode=', DSS.ControlModeEnum.OrdinalToString(Controlmode));
    FSWriteln(F, 'Set Random=', DSS.RandomModeEnum.OrdinalToString(RandomType));
    FSWriteln(F, 'Set hour=', IntToStr(DynaVars.intHour));
    FSWriteln(F, 'Set sec=', Format('%-g', [DynaVars.t]));
    FSWriteln(F, 'Set year=', IntToStr(Year));
    FSWriteln(F, 'Set frequency=', Format('%-g', [Frequency]));
    FSWriteln(F, 'Set stepsize=', Format('%-g', [DynaVars.h]));
    FSWriteln(F, 'Set number=', IntToStr(NumberOfTimes));
    FSWriteln(F, 'Set circuit=', ckt.Name);
    FSWriteln(F, 'Set editor=', DefaultEditor);
    FSWriteln(F, 'Set tolerance=', Format('%-g', [ConvergenceTolerance]));
    FSWriteln(F, 'Set maxiterations=', IntToStr(MaxIterations));
    FSWriteln(F, 'Set miniterations=', IntToStr(MinIterations));
    FSWriteln(F, 'Set loadmodel=', DSS.DefaultLoadModelEnum.OrdinalToString(LoadModel));

    FSWriteln(F, 'Set loadmult=', Format('%-g', [ckt.LoadMultiplier]));
    FSWriteln(F, 'Set Normvminpu=', Format('%-g', [ckt.NormalMinVolts]));
    FSWriteln(F, 'Set Normvmaxpu=', Format('%-g', [ckt.NormalMaxVolts]));
    FSWriteln(F, 'Set Emergvminpu=', Format('%-g', [ckt.EmergMinVolts]));
    FSWriteln(F, 'Set Emergvmaxpu=', Format('%-g', [ckt.EmergMaxVolts]));
    FSWriteln(F, 'Set %mean=', Format('%-.4g', [ckt.DefaultDailyShapeObj.Mean * 100.0]));
    FSWriteln(F, 'Set %stddev=', Format('%-.4g', [ckt.DefaultDailyShapeObj.StdDev * 100.0]));
    FSWriteln(F, 'Set LDCurve=', StrUtils.IfThen(ckt.LoadDurCurveObj <> NIL, ckt.LoadDurCurveObj.Name, ''));  // Load Duration Curve
    FSWriteln(F, 'Set %growth=', Format('%-.4g', [((ckt.DefaultGrowthRate - 1.0) * 100.0)]));  // default growth rate

    FSWriteln(F, 'Set genkw=', Format('%-g', [ckt.AutoAddObj.GenkW]));
    FSWriteln(F, 'Set genpf=', Format('%-g', [ckt.AutoAddObj.GenPF]));
    FSWriteln(F, 'Set capkvar=', Format('%-g', [ckt.AutoAddObj.Capkvar]));
    FSWrite(F, 'Set addtype=');
    case ckt.AutoAddObj.Addtype of
        GENADD:
            FSWriteln(F, 'generator');
        CAPADD:
            FSWriteln(F, 'capacitor');
    end;
    FSWrite(F, 'Set allowduplicates=');
    FSWriteln(F, StrYorN(ckt.DuplicatesAllowed));
    FSWrite(F, 'Set zonelock=');
    FSWriteln(F, StrYorN(ckt.ZonesLocked));
    FSWriteln(F, Format('Set ueweight=%8.2f', [ckt.UEWeight]));
    FSWriteln(F, Format('Set lossweight=%8.2f', [ckt.LossWeight]));
    FSWriteln(F, 'Set ueregs=', IntArraytoString(ckt.UEregs));
    FSWriteln(F, 'Set lossregs=', IntArraytoString(ckt.Lossregs));
    FSWrite(F, 'Set voltagebases=(');  //  changes the default voltage base rules
    for i := 0 to High(ckt.LegalVoltageBases) do
    begin
        FSWrite(F, Format('%10.2f', [ckt.LegalVoltageBases[i]]));
    end;
    FSWriteln(F, ')');
    FSWriteln(F, 'Set algorithm=' + DSS.SolveAlgEnum.OrdinalToString(Algorithm));
    FSWrite(F, 'Set Trapezoidal=');
    FSWriteln(F, StrYorN(ckt.TrapezoidalIntegration));
    FSWriteln(F, 'Set genmult=', Format('%-g', [ckt.GenMultiplier]));

    FSWriteln(F, 'Set Basefrequency=', Format('%-g', [ckt.Fundamental]));

    FSWrite(F, 'Set harmonics=(');  //  changes the default voltage base rules
    if DoAllHarmonics then
        FSWrite(F, 'ALL')
    else
        for harm in HarmonicList do
            FSWrite(F, Format('%-g, ', [harm]));
    FSWriteln(F, ')');
    FSWriteln(F, 'Set maxcontroliter=', IntToStr(MaxControlIterations));
    FSWriteln(F);

    if Complete then
    begin
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
        Result := NodeV[i] - NodeV[j];  // V1-V2
end;

procedure TSolutionObj.WriteConvergenceReport(F: TStream);
var
    i: Integer;
    sout: String;
begin
    FSWriteln(F);
    FSWriteln(F, '-------------------');
    FSWriteln(F, 'Convergence Report:');
    FSWriteln(F, '-------------------');
    FSWriteln(F, '"Bus.Node", "Error", "|V|","Vbase"');
    for i := 1 to ckt.NumNodes do
    begin
        WriteStr(sout, 
            '"', pad((ckt.BusList.NameOfIndex(ckt.MapNodeToBus[i].Busref) + '.' + IntToStr(ckt.MapNodeToBus[i].NodeNum) + '"'), 18),
            ', ', ErrorSaved[i]: 10: 5,
            ', ', VmagSaved[i]: 14,
            ', ', NodeVbase[i]: 14
        );
        FSWrite(F, sout);

        FSWriteln(F);
    end;

    FSWriteln(F);
    WriteStr(sout, 'Max Error = ', MaxError: 10: 5);
    FSWriteln(F, sout);
end;

procedure TSolutionObj.SumAllCurrents;
var
    pelem: TDSSCktElement;
begin
    for pelem in ckt.CktElements do
    begin
        pelem.SumCurrents;   // sum terminal currents into system Currents Array
    end;
end;

procedure TSolutionObj.DoControlActions;
var
    XHour: Integer;
    XSec: Double;
begin
    case ControlMode of
        CTRLSTATIC:
        begin  //  execute the nearest set of control actions but leaves time where it is
            if ckt.ControlQueue.IsEmpty then
                ControlActionsDone := TRUE
            else
                ckt.ControlQueue.DoNearestActions(xHour, XSec); // ignore time advancement
        end;
        EVENTDRIVEN:
        begin  //  execute the nearest set of control actions and advance time to that time
                // **** Need to update this to set the "Intervalhrs" variable for EnergyMeters for Event-Driven Simulation ****
            if not ckt.ControlQueue.DoNearestActions(DynaVars.intHour, DynaVars.t) // these arguments are var type
            then
                ControlActionsDone := TRUE;// Advances time to the next event
        end;
        TIMEDRIVEN:
        begin   // Do all actions having an action time <= specified time
            if not ckt.ControlQueue.DoActions(DynaVars.intHour, DynaVars.t) then
                ControlActionsDone := TRUE;
        end;
        MULTIRATE:
        begin  //  execute the nearest set of control actions but leaves time where it is
            if not ckt.ControlQueue.DoMultiRate(DynaVars.intHour, DynaVars.t) then
                ControlActionsDone := TRUE;
        end;
    end;
end;

procedure TSolutionObj.SampleControlDevices;
var
    ControlDevice: TControlElem;
begin
    ControlDevice := NIL;
    try
        // Sample all controls and set action times in control Queue
        for ControlDevice in ckt.DSSControls do
        begin
            if ControlDevice.Enabled then
                ControlDevice.Sample();
        end;

    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error Sampling Control Device "%s". Error = %s', [ControlDevice.FullName, E.message], 484);
            raise EControlProblem.Create('Solution aborted.');
        end;
    end;
end;

procedure TSolutionObj.Sample_DoControlActions;
begin
    if ControlMode = CONTROLSOFF then
        ControlActionsDone := TRUE
    else
    begin
        SampleControlDevices();
        DoControlActions();

        // This variable lets control devices know the bus list has changed
        ckt.Control_BusNameRedefined := FALSE;  // Reset until next change
    end;
end;

procedure TSolutionObj.Set_Mode(const Value: TSolveMode);
begin
    DynaVars.intHour := 0;
    DynaVars.t := 0.0;
    Update_dblHour;
    ckt.TrapezoidalIntegration := FALSE;

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
            ckt.TrapezoidalIntegration := TRUE;
            SampleTheMeters := TRUE;
        end;
        TSolveMode.LOADDURATION2:
        begin
            DynaVars.intHour := 1;
            ckt.TrapezoidalIntegration := TRUE;
            SampleTheMeters := TRUE;
        end;
        TSolveMode.AUTOADDFLAG:
        begin
            IntervalHrs := 1.0;
            ckt.AutoAddObj.ModeChanged := TRUE;
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
        ckt.AutoAddObj.AddCurrents(SolveType);
end;

procedure TSolutionObj.Check_Fault_Status;
var
    pFault: TFaultObj;
begin
    for pFault in ckt.Faults do
    begin
        pFault.CheckStatus(ControlMode);
    end;
end;

procedure calcInitialMachineStates(DSS: TDSSContext);
var
    pcelem: TPCElement;
begin
    // Do All PC Elements
    // If state variables not defined for a PC class, does nothing
    for pcelem in DSS.ActiveCircuit.PCElements do
    begin
        if pcelem.Enabled then
            pcelem.InitStateVars();
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
        ckt.InvalidateAllPCELEMENTS();  // Force Recomp of YPrims when we leave Dynamics mode

    if not IsDynamicModel and ValueIsDynamic then
    begin   // see if conditions right for going into dynamics

        if ckt.IsSolved then
            calcInitialMachineStates(DSS)   // set state variables for machines (loads and generators)
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
        ckt.InvalidateAllPCELEMENTS();  // Force Recomp of YPrims when we leave Harmonics mode
        Frequency := ckt.Fundamental;   // Resets everything to norm
    end;

    if not IsHarmonicModel and ((Value = TSolveMode.HARMONICMODE) or (Value = TSolveMode.HARMONICMODET)) then
    begin   // see if conditions right for going into Harmonics

        if (ckt.IsSolved) and (Frequency = ckt.Fundamental) then
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
    if ckt <> NIL then
        Harmonic := FFrequency / ckt.Fundamental;  // Make Sure Harmonic stays in synch
end;

procedure TSolutionObj.IncrementTime;
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
    DynaVars.intHour := 0;  // Change year, start over
    Dynavars.t := 0.0;
    Update_dblHour;
    DSS.EnergyMeterClass.ResetAll;  // force any previous year data to complete
end;

procedure TSolutionObj.SaveVoltages;

var
    F: TStream = nil;
    Volts: Complex;
    i, j: Integer;
    BusName: String;
    sout: String;
begin
    try
        try
            F := DSS.GetOutputStreamEx(DSS.OutputDirectory + DSS.CircuitName_ + 'SavedVoltages.txt', fmCreate); // CurrentDSSDir

            for i := 1 to ckt.NumBuses do
            begin
                BusName := ckt.BusList.NameOfIndex(i);
                for j := 1 to ckt.Buses[i].NumNodesThisBus do
                begin
                    Volts := NodeV[ckt.Buses[i].RefNo[j]];
                    WriteStr(sout, BusName, ', ', ckt.Buses[i].GetNum(j): 0, Format(', %-.7g, %-.7g', [Cabs(Volts), CDang(Volts)]));
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
        DSS.GlobalResult := DSS.OutputDirectory + DSS.CircuitName_ + 'SavedVoltages.txt'; // CurrentDSSDir

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
            Result := SolveSparseSet(hY, pComplexArray(@V[1]), pComplexArray(@Currents[1])) // Solve for present InjCurr
{$IFDEF DSS_CAPI_ADIAKOPTICS}
        else
            Result := SolveSparseSet(hY, pComplexArray(@V[LocalBusIdx[0]]), pComplexArray(@Currents[1]));  // Solve for present InjCurr in Actor 1 context
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
    pBus: TDSSBus;
begin
    for i := 1 to ckt.NumBuses do
    begin
        pBus := ckt.Buses[i];
            if pBus.VBus <> NIL then
                for j := 1 to pBus.NumNodesThisBus do
                    pBus.VBus[j] := NodeV[pBus.RefNo[j]];
    end;
end;

procedure TSolutionObj.RestoreNodeVfromVbus;
var
    i, j: Integer;
    pBus: TDSSBus;
begin
    for i := 1 to ckt.NumBuses do
    begin
        pBus := ckt.Buses[i];
            if pBus.VBus <> NIL then
                for j := 1 to pBus.NumNodesThisBus do
                    NodeV[pBus.RefNo[j]] := pBus.VBus[j];
    end;
end;

procedure TSolutionObj.SolveYDirect();
// Solves present Y matrix with no injection sources except voltage and current sources 
begin
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
constructor TSolver.Create(sol: TSolutionObj; Susp: Boolean; local_CPU: Integer; AEvent: TEvent);
begin
    DSS := sol.DSS;
    cktptr := DSS.ActiveCircuit;
    solution := DSS.ActiveCircuit.Solution;

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
{$IFDEF DARWIN}{$IFDEF CPUAARCH64}
    // FPU is not correctly initialized on macOS aarch64
    // See https://gitlab.com/freepascal.org/fpc/source/-/issues/38230 (Exception in system functions on Apple M1)
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
{$ENDIF}{$ENDIF}
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
                        case solution.Dynavars.SolutionMode of
                            TSolveMode.SNAPSHOT:
                                solution.SolveSnap();
                            TSolveMode.YEARLYMODE:
                                solution.SolveYearly();
                            TSolveMode.DAILYMODE:
                                solution.SolveDaily();
                            TSolveMode.DUTYCYCLE:
                                solution.SolveDuty();
                            TSolveMode.DYNAMICMODE:
                                solution.SolveDynamic();
                            TSolveMode.MONTECARLO1:
                                solution.SolveMonte1();
                            TSolveMode.MONTECARLO2:
                                solution.SolveMonte2();
                            TSolveMode.MONTECARLO3:
                                solution.SolveMonte3();
                            TSolveMode.PEAKDAY:
                                solution.SolvePeakDay();
                            TSolveMode.LOADDURATION1:
                                solution.SolveLD1();
                            TSolveMode.LOADDURATION2:
                                solution.SolveLD2();
                            TSolveMode.DIRECT:
                                solution.SolveDirect();
                            TSolveMode.MONTEFAULT:
                                solution.SolveMonteFault(); // Monte Carlo Fault Cases
                            TSolveMode.FAULTSTUDY:
                                solution.SolveFaultStudy();
                            TSolveMode.AUTOADDFLAG:
                                solution.ckt.AutoAddObj.Solve();
                            TSolveMode.HARMONICMODE:
                                solution.SolveHarmonic();
                            TSolveMode.GENERALTIME:
                                solution.SolveGeneralTime();
                            TSolveMode.HARMONICMODET:
                                solution.SolveHarmonicT(); //Declares the Hsequential-time harmonics
                        else
                            DoSimpleMsg(DSS, _('Unknown solution mode.'), 481);
                        end;

                        {$IFDEF WINDOWS}
                        QueryPerformanceCounter(solution.GEndTime);
                        {$ELSE}
                        solution.GEndTime := GetTickCount64();
                        {$ENDIF}
                        solution.Total_Solve_Time_Elapsed := ((solution.GEndTime - solution.GStartTime) / CPU_Freq) * 1000000;
                        solution.Total_Time_Elapsed := solution.Total_Time_Elapsed + solution.Total_Solve_Time_Elapsed;
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
                        Start_Diakoptics(); // Initializes the actor for Diakoptics (if needed)
                    IndexBuses();
                end;
                TActorMessage.SOLVE_AD1:
                    solution.SolveAD(True); // Solves the model if the actor has PIE
                TActorMessage.SOLVE_AD2:
                    solution.SolveAD(False); // Complements the solution
                TActorMessage.GETCTRLMODE:
                begin
                    // Brings the control mode from actor 1
                    solution.ControlMode := DSS.Parent.ActiveCircuit.Solution.ControlMode;
                    solution.DefaultControlMode := ControlMode;
                    solution.MaxControlIterations := DSS.Parent.ActiveCircuit.Solution.MaxControlIterations;
                end;
{$ENDIF}                        
                TActorMessage.ZEROIVECTOR:
                    solution.ZeroInjCurr();
                TActorMessage.GETCURRINJ:
                    solution.GetSourceInjCurrents();
                TActorMessage.CALC_INJ_CURR:
                    solution.GetPCInjCurr();
                TActorMessage.DO_CTRL_ACTIONS:
                begin
                    solution.ControlActionsDone := FALSE;
                    solution.Sample_DoControlActions;
                    try
                        actorMessagesLock.Acquire();
                        actorMessages.enqueue(TActorMessage.CHECK_FAULT);
                        actorMessages.enqueue(TActorMessage.CHECKYBUS);
                    finally
                        actorMessagesLock.Release();
                    end;
                end;
                TActorMessage.CHECK_FAULT:
                    solution.Check_Fault_Status();
                TActorMessage.CHECKYBUS:
                    if solution.SystemYChanged {$IFDEF DSS_CAPI_INCREMENTAL_Y}or (solution.ckt.IncrCktElements.Count <> 0){$ENDIF} then
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

procedure TSolver.DoTerminate; // Is the end of the thread
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
        if SystemYChanged {$IFDEF DSS_CAPI_INCREMENTAL_Y}or (ckt.IncrCktElements.Count <> 0){$ENDIF} then
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
    ckt.IsSolved := TRUE;
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
    pdeList: ArrayOfString;
begin
    // Select the main voltage source
    VSourceObj := DSS.VsourceClass.ElementList.First();
    // Gets the name of the branch directly connected to the feeder head to remove it
    // (applies to all actors but actor 2 - first chunk of the system)
    BusName := VSourceObj.GetBus(1);
    jj := ansipos('.', BusName);   // removes the dot
    if (jj > 0) then
        BusName := BusName.Substring(0, (jj - 1));
    SetActiveBus(DSS, BusName); // Activates the Bus
    pdeList := ckt.getPDEatBus(ActiveBusIndex);
    // Disables the link branch
    DSS.DssExecutive.ParseCommand(pdeList[0] + '.enabled=False'); //TODO: rewrite
    // Now disables all the VSources added artificially
    for VSourceObj in DSS.VsourceClass.ElementList do
    begin
        BusName := AnsiLowerCase(VSourceObj.Name);
        if (BusName = 'source') then
            VSourceObj.Enabled := FALSE // Disables the artificial VSource phase 1
        else
        if (BusName = 'vph_2') then
            VSourceObj.Enabled := FALSE // Disables the artificial VSource phase 2
        else
        if (BusName = 'vph_3') then
            VSourceObj.Enabled := FALSE; // Disables the artificial VSource phase 3
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
//             DSS.Parent.ActiveCircuit.Solution.NodeV[idx] := Solution.NodeV[i];
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
    Result := DSS.Parent.ActiveCircuit.Solution.NodeV[NodeIdx];
end;

// Updates the local ISources using the data obtained
// for Ic in actor 1
procedure TSolutionObj.UpdateISrc;
var
    idx,
    i: Integer;
    Found: Boolean;
    c: Complex;

begin
    Found := FALSE;
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
            c := DSS.Parent.ActiveCircuit.Ic.CData[idx].Value * (-1.0);
            Currents[AD_IBus.Items[i]] := c + Currents[AD_IBus.Items[i]];
        end;  // Otherwise is just another ISource in the zone
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
//             for VSourceObj in DSS.VsourceClass.ElementList do
//             begin
//                 if VSourceObj.enabled then
//                 begin
//                     Result := TRUE;
//                     break;
//                 end;
//             end;
//             if not Result then
//             begin
//                 // Goes for ISources
//                 for ISourceObj in DSS.IsourceClass.ElementList do
//                 begin
//                     if ISourceObj.enabled then
//                     begin
//                         Result := TRUE;
//                         break;
//                     end;
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
        for i := 1 to DSS.Parent.ActiveCircuit.NumNodes do
        begin
            SrcBus[high(SrcBus)] := Format('%s.%-d', [AnsiUpperCase(BusList.NameOfIndex(DSS.Parent.ActiveCircuit.MapNodeToBus[i].Busref)), DSS.Parent.ActiveCircuit.MapNodeToBus[i].NodeNum]);
            setlength(SrcBus, (length(SrcBus) + 1));
        end;
    end;
    // rebuilds the Y matrix to relocate the local buses
    BuildYMatrix(DSS, WHOLEMATRIX, TRUE);   // Side Effect: Allocates V
    // Then, get the list of buses in my actor
    setlength(LclBus, 1);
    if solution.ckt = NIL then // TODO: is this possible? Previously DSS.ActiveCircuit
        Exit;

    for i := 1 to ckt.NumNodes do
    begin
        LclBus[high(LclBus)] := Format('%s.%-d', [AnsiUpperCase(ckt.BusList.NameOfIndex(ckt.MapNodeToBus[i].Busref)), ckt.MapNodeToBus[i].NodeNum]);
        setlength(LclBus, (length(LclBus) + 1));
    end;
    // Initializes the bus index vector
    Setlength(LocalBusIdx, length(LclBus) - 1);
    for i := 0 to High(LocalBusIdx) do
    begin
        for j := 0 to High(SrcBus) do
            if LclBus[i] = SrcBus[j] then
                break;
        LocalBusIdx[i] := j + 1;
    end;

    // Initializes the list for easy accessing the ADiakoptics Isources
    if solution.AD_IBus = NIL then
        solution.AD_IBus := TList<Integer>.Create
    else
        solution.AD_IBus.Clear;
    if solution.AD_ISrcIdx = NIL then
        solution.AD_ISrcIdx := TList<Integer>.Create
    else
        solution.AD_ISrcIdx.Clear;
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
            solution.AD_IBus.Add(k + 1);
            solution.AD_ISrcIdx.Add(DSS.Parent.ActiveCircuit.Contours.CData[j].Row);
        end;
    end;
end;
{$ENDIF}

function TSolutionObj.TimeOfDay(useEpsilon: Boolean): Double;
// Normalize time to a floating point number representing time of day if Hour > 24
// Resulting time should be 0:00+ to 24:00 inclusive.
var
    h: Integer; 
    sec: Double;
    HourOfDay: Integer;
begin
    h := DynaVars.intHour;
    sec := DynaVars.t;

    if useEpsilon then
    begin
        // If the TOD is at least slightly greater than 24:00 wrap around to 0:00
        //
        // **NOTE (DSS-Extensions)**: the original versions in TStorage/TStorageController
        // did not include epsilon, hence the option; without the setting the option
        // to false, the event logs can be slight different and could mislead users
        // when comparing results.
        // Otherwise, looks like this whole function could be rewritten as...
        //     ((DynaVars.intHour + DynaVars.t / 3600.0) MOD 24.0)
        if h > 24 then
            HourOfDay := (h - ((h - 1) div 24) * 24)  // creates numbers 1..24
        else
            HourOfDay := h;

        Result := HourOfDay + sec / 3600.0;
        if Result - 24.0 > Epsilon then
            Result := Result - 24.0;   // Wrap around

        Exit;
    end;

    if h > 23 then
        HourOfDay := (h - (h div 24) * 24)
    else
        HourOfDay := h;

    Result := HourOfDay + sec / 3600.0;
    if Result > 24.0 then
        Result := Result - 24.0; // Wrap around
end;

end.
