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

USES
    uCOMPLEX, Arraydef,
    Command,
    Monitor,
    DSSClass,
    DSSObject,
    Dynamics,
    EnergyMeter,
    VSource,
    SysUtils,
    {$IFDEF FPC}
    Classes,
    {$ELSE}
//    {$IFDEF MSWINDOWS}
    System.Diagnostics,
    System.TimeSpan,
    System.Classes,
    Parallel_Lib,
    Windows,
    {$ENDIF}
    Dialogs,
    Sparse_Math,
    SyncObjs,
    ExecHelper,
    CktElement;

CONST

     NORMALSOLVE = 0;
     NEWTONSOLVE = 1;
// Constants for the actor's messaging
     SIMULATE    = 0;
     EXIT_ACTOR  = 1;

     ALL_ACTORS  = 0; // Wait flag for all the actors
     AD_ACTORS   = 1; // Wait flag to wait only for the A-Diakoptics actors
TYPE

   EControlProblem = class(Exception);
   ESolveError = Class(Exception);  // Raised when solution aborted

   TNodeVarray = Array[0..1000] of Complex;
   pNodeVarray = ^TNodeVarray;
   ///////////////////////////////////
     {define LD_FM_Arry-by dahei}
     TLDs_sys_fms = packed Record
     //properties for Nodes
           // highest voltage node
           clstr_num_hghst: integer;
           ndnum_hghst : integer;
           b_ctrl_hghst : boolean; //can contribute more to the high volt problem
           volt_hghst : double;    //p.u.
           volt_hgh_lmt : double;  //p.u.
           Pinjec_hghst : double; //net P injection on this node
           // lowest voltage node
           clstr_num_lwst: integer;
           ndnum_lwst : integer;
           b_ctrl_lwst : boolean; //can contribute more to the high volt problem
           volt_lwst : double;   //p.u.
           volt_lw_lmt : double;  //p.u.
           Pinjec_lwst : double; // net P injection on this node
      End;
   //////////////////////////////////////

   TDSSSolution = CLASS(TDSSClass)


     private
//       CommandList:TCommandlist;
     Protected
       PROCEDURE DefineProperties;
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit(ActorID : Integer):Integer; override;
       FUNCTION Init(Handle:Integer; ActorID : Integer):Integer; override;
       FUNCTION NewObject(const ObjName:String):Integer; override;

   End;
   TInfoMessageCall = Procedure(const info:String) of Object;  // Creates the procedure for sending a message
   TSolver=class(TThread)
      Constructor Create(Susp:Boolean;local_CPU: integer; ID : integer; CallBack: TInfoMessageCall;AEvent: TEvent);overload;
      procedure Execute; override;
      procedure Doterminate; override;
      destructor Destroy; override;

//*******************************Private components*****************************
    protected
      FMessage,
      Msg_Cmd       : string;
      UINotifier,
      FInfoProc     : TInfoMessageCall;
      ActorID,
      MsgType       : Integer;
      UIEvent,
      ActorMsg      : TEvent;
      AD_Init,          // used to know if the actors require a partial solution
      ActorActive,
      Processing    : Boolean;

      procedure Start_Diakoptics();
      procedure Notify_Main;
      function Get_Processing(): Boolean;
      procedure Set_Processing(Nval : Boolean);
      function Get_CPU(): Integer;
      procedure Set_CPU(CPU : Integer);
//*******************************Public components******************************
    Public
      Procedure Send_Message(Msg  : Integer);
      procedure CallCallBack;
      property Event: TEvent read UIEvent;

      property Is_Busy: Boolean read  Get_Processing write Set_Processing;
      property  CPU : Integer read Get_CPU write Set_CPU;

   end;

   TSolutionObj = class(TDSSObject)
     private

       dV :pNodeVArray;   // Array of delta V for Newton iteration
       FFrequency:Double;
       // //by Dahei
       nNZ_yii   :LongWord;       //how many lines in Yii
       pColIdx_Yii, pRowIdx_Yii   :pLongIntArray;//array of LongWord;  //cols and rows
       pcVals_Yii                 :pComplexArray;   //vals of yii
       // =========

       FUNCTION Converged(ActorID : Integer):Boolean;
       FUNCTION OK_for_Dynamics(const Value:Integer):Boolean;
       FUNCTION OK_for_Harmonics(const Value:Integer):Boolean;



       PROCEDURE DoNewtonSolution(ActorID : Integer);
       PROCEDURE DoNormalSolution(ActorID : Integer);
//       PROCEDURE GetMachineInjCurrents;
       PROCEDURE SetGeneratordQdV(ActorID : Integer);
       PROCEDURE SumAllCurrents(ActorID : Integer);
       procedure Set_Frequency(const Value: Double);
       PROCEDURE Set_Mode(const Value: Integer);
       procedure Set_Year(const Value: Integer);
       procedure Set_Total_Time(const Value: Double);

     public

       Algorithm :Integer;      // NORMALSOLVE or NEWTONSOLVE
       AuxCurrents  :pComplexArray;  // For injections like AutoAdd
       ControlActionsDone :Boolean;
       ControlIteration :Integer;
       ControlMode :Integer;     // EVENTDRIVEN, TIMEDRIVEN
       ConvergenceTolerance :Double;
       ConvergedFlag:Boolean;
       DefaultControlMode :Integer;    // EVENTDRIVEN, TIMEDRIVEN
       DefaultLoadModel :Integer;     // 1=POWERFLOW  2=ADMITTANCE
       DoAllHarmonics : Boolean;
       DynamicsAllowed :Boolean;
       DynaVars:TDynamicsRec;
       ErrorSaved :pDoubleArray;
       FirstIteration :Boolean;
       FrequencyChanged:Boolean;  // Flag set to true if something has altered the frequency
       Fyear :Integer;
       Harmonic   :Double;
       HarmonicList  :pDoubleArray;
       HarmonicListSize :Integer;
       hYsystem :NativeUint;   {Handle for main (system) Y matrix}
       hYseries :NativeUint;   {Handle for series Y matrix}
       hY :NativeUint;         {either hYsystem or hYseries}
       IntervalHrs:Double;   // Solution interval since last solution, hrs.
       IsDynamicModel :Boolean;
       IsHarmonicModel :Boolean;
       Iteration :Integer;
       LoadModel :Integer;        // 1=POWERFLOW  2=ADMITTANCE
       LastSolutionWasDirect :Boolean;
       LoadsNeedUpdating :Boolean;
       MaxControlIterations :Integer;
       MaxError :Double;
       MaxIterations,
       MinIterations :Integer;
       MostIterationsDone :Integer;
       NodeVbase :pDoubleArray;
       NumberOfTimes :Integer;  // Number of times to solve
       PreserveNodeVoltages:Boolean;
       RandomType :Integer;     //0 = none; 1 = gaussian; 2 = UNIFORM
       SampleTheMeters : Boolean;  // Flag to allow sampling of EnergyMeters
       SeriesYInvalid :Boolean;
       SolutionCount :Integer;  // Counter incremented for each solution
       SolutionInitialized :Boolean;
       SystemYChanged  : Boolean;
       UseAuxCurrents  : Boolean;
       VmagSaved : pDoubleArray;
       VoltageBaseChanged : Boolean;

       {Voltage and Current Arrays}
       NodeV     : pNodeVArray;     // Main System Voltage Array   allows NodeV^[0]=0
       Currents  : pNodeVArray;     // Main System Currents Array

       {A-Diakoptics variables}
       Node_dV   : pNodeVArray;     // Used to store the partial solution voltage
       Ic_Local  : pNodeVArray;     // Used to store the complementary curret

//******************************************************************************
       IncMat                 :  Tsparse_matrix; // Incidence sparse matrix
       Laplacian              :  Tsparse_matrix; // Laplacian sparse matrix
       {by Dahei for FMonitor}
              {------------------}
       NodeYii                : pNodeVArray;         // Main System Y = G + jB, Bii for all nodes

       NodeYiiEmpty           : Boolean;
       {Leaders of all FMonitors}
       clstr_num_hghst, clstr_num_lwst: integer;
       LD_FM                  : array [0..3] of TLDs_sys_fms;
       bCurtl                 : boolean;
//****************************Timing variables**********************************
       SolveStartTime         : int64;
       SolveEndtime           : int64;
       GStartTime             : int64;
       Gendtime               : int64;
       LoopEndtime            : int64;
       Total_Time_Elapsed     : double;
       Solve_Time_Elapsed     : double;
       Total_Solve_Time_Elapsed  : double;
       Step_Time_Elapsed      : double;
//******************************************************************************
// ActiveCell of the Incidence Matrix:
// [0] = row
// [1] = col
// [2] = value
      ActiveIncCell  :  Array[0..2] of Integer;
//******************************************************************************
// IncMatrix Row and column descriptors
// Rows array (array of strings that tells what is the order of the PDElements)
// Columns array (array of strigns with the names of the cols of the Inc matrix)'
// Levels array (array of integers that describes the proximity level for each
// bus to the circuit's backbone)
      Inc_Mat_Rows    :  Array of String;
      Inc_Mat_Cols    :  Array of String;
      Inc_Mat_levels  :  Array of Integer;
      temp_counter    : Integer;
      Active_Cols     : Array of Integer;
      Active_Cols_Idx :  Array of Integer;
//******************************************************************************
//********************Diakoptics solution mode variables************************
      ADiakoptics_ready  : Boolean;
      ADiakoptics_Actors :  Integer;
//******************************************************************************
       constructor Create(ParClass:TDSSClass; const solutionname:String);
       destructor  Destroy; override;

       PROCEDURE ZeroAuxCurrents(ActorID: Integer);
       FUNCTION  SolveZeroLoadSnapShot(ActorID : Integer) :Integer;
       PROCEDURE DoPFLOWsolution(ActorID : Integer);

       PROCEDURE Solve(ActorID : Integer);                // Main Solution dispatch
       PROCEDURE SnapShotInit(ActorID : Integer);
       FUNCTION  SolveSnap(ActorID : integer):Integer;    // solve for now once
       FUNCTION  SolveDirect(ActorID : integer):Integer;  // solve for now once, direct solution
       FUNCTION  SolveYDirect(ActorID : Integer):Integer; // Similar to SolveDirect; used for initialization
       FUNCTION  SolveCircuit(ActorID : integer):Integer; // SolveSnap sans control iteration
       PROCEDURE CheckControls(ActorID : Integer);       // Snapshot checks with matrix rebuild
       PROCEDURE SampleControlDevices(ActorID : Integer);
       PROCEDURE DoControlActions(ActorID : Integer);
       PROCEDURE Sample_DoControlActions(ActorID : Integer);    // Sample and Do
       PROCEDURE Check_Fault_Status(ActorID : Integer);

       PROCEDURE SetGeneratorDispRef(ActorID : Integer);
       PROCEDURE SetVoltageBases(ActorID : Integer);

       PROCEDURE SaveVoltages;
       PROCEDURE UpdateVBus(ActorID : Integer); // updates voltages for each bus    from NodeV
       PROCEDURE RestoreNodeVfromVbus;  // opposite   of updatebus

       FUNCTION  VDiff(i,j:Integer):Complex;  // Difference between two node voltages
       {by Dahei}
       PROCEDURE Get_Yiibus; // updates voltages for each bus    from NodeV
       FUNCTION Get_Yij(node_ref_i, node_ref_j : integer) : Complex; // get Gij + j Bij
       {}
       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean); Override;
       PROCEDURE WriteConvergenceReport(const Fname:String);
       PROCEDURE Update_dblHour;
       PROCEDURE Increment_time;

       PROCEDURE UpdateLoopTime;

       Property  Mode         :Integer  Read dynavars.SolutionMode Write Set_Mode;
       Property  Frequency    :Double   Read FFrequency            Write Set_Frequency;
       Property  Year         :Integer  Read FYear                 Write Set_Year;
       Property  Time_Solve :Double  Read Solve_Time_Elapsed;
       Property  Time_TotalSolve:Double  Read Total_Solve_Time_Elapsed;
       Property  Time_Step:Double      Read Step_Time_Elapsed;     // Solve + sample
       Property  Total_Time   :Double  Read Total_Time_Elapsed      Write Set_Total_Time;

 // Procedures that use to be private before 01-20-2016

       PROCEDURE AddInAuxCurrents(SolveType:Integer; ActorID : Integer);
       Function SolveSystem(V:pNodeVArray; ActorID : Integer):Integer;
       PROCEDURE GetPCInjCurr(ActorID : Integer);
       PROCEDURE GetSourceInjCurrents(ActorID : Integer);
       PROCEDURE ZeroInjCurr(ActorID : Integer);
       PROCEDURE Upload2IncMatrix;

       PROCEDURE Calc_Inc_Matrix(ActorID : Integer);                // Calculates the incidence matrix for the Circuit
       PROCEDURE Calc_Inc_Matrix_Org(ActorID : Integer);            // Calculates the incidence matrix hierarchically organized for the Circuit

       function get_IncMatrix_Row(Col : integer): Integer;          // Gets the index of the Row connected to the specified Column
       function get_IncMatrix_Col(Row : integer): Integer;          // Gets the index of the Column connected to the specified Row
       function CheckLocationIdx(Idx : Integer): Integer;           // Evaluates the area covered by the tearing point to see if there is a better one

       procedure AddLines2IncMatrix(ActorID : Integer);             // Adds the Lines to the Incidence matrix arrays
       procedure AddXfmr2IncMatrix(ActorID : Integer);              // Adds the Xfmrs to the Incidence matrix arrays
       procedure AddSeriesCap2IncMatrix(ActorID : Integer);         // Adds capacitors in series to the Incidence matrix arrays
       procedure AddSeriesReac2IncMatrix(ActorID : Integer);        // Adds Reactors in series to the Incidence matrix arrays

   End;
{==========================================================================}

VAR
   ActiveSolutionObj:TSolutionObj;


implementation

USES  SolutionAlgs,
      DSSClassDefs, DSSGlobals,
{$IFNDEF FPC}
      SHELLAPI,
      DSSForms,
      ScriptEdit, 
{$ELSE}
      CmdForms,
{$ENDIF}
      PDElement,  ControlElem, Fault,
      AutoAdd,  YMatrix, Load,CKtTree,
      ParserDel, Generator,Capacitor,
{$IFDEF DLL_ENGINE}
      ImplGlobals,  // to fire events
{$ENDIF}
{$IFDEF FPC}
      FNCS, // to check for messages at the end of each time step
{$ENDIF}
      Math,  Circuit, Utilities, KLUSolve, PointerList, Line,
      Transformer, Reactor, Diakoptics
;

Const NumPropsThisClass = 1;

{$DEFINE debugtrace}

{$UNDEF debugtrace}  {turn it off  delete this line to activate debug trace}

{$IFDEF debugtrace}
var FDebug:TextFile;
{$ENDIF}

// ===========================================================================================
constructor TDSSSolution.Create;  // Collection of all solution objects
Begin
     Inherited Create;
     Class_Name := 'Solution';
     DSSClassType := DSS_OBJECT + HIDDEN_ELEMENT;

     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := True;
End;

// ===========================================================================================
Destructor TDSSSolution.Destroy;

Begin
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;

End;

// ===========================================================================================
PROCEDURE TDSSSolution.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names
     PropertyName[1] := '-------';


     // define Property help values
     PropertyHelp[1] := 'Use Set Command to set Solution properties.';


     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;


// ===========================================================================================
FUNCTION TDSSSolution.NewObject(const ObjName:String):Integer;
Begin
    // Make a new Solution Object and add it to Solution class list
      ActiveSolutionObj := TSolutionObj.Create(Self, ObjName);
    // this one is different than the rest of the objects.
      Result := AdDobjectToList(ActiveSolutionObj);
End;

// ===========================================================================================
constructor TSolutionObj.Create(ParClass:TDSSClass; const SolutionName:String);
// ===========================================================================================
Begin
    Inherited Create(ParClass);
    Name := LowerCase(SolutionName);

//    i := SetLogFile ('c:\\temp\\KLU_Log.txt', 1);

    FYear    := 0;
    DynaVars.intHour     := 0;
    DynaVars.t        := 0.0;
    DynaVars.dblHour  := 0.0;
    DynaVars.tstart   := 0.0;
    DynaVars.tstop    := 0.0;
    //duration := 0.0;
    DynaVars.h        := 0.001;  // default for dynasolve

    LoadsNeedUpdating := TRUE;
    VoltageBaseChanged := TRUE;  // Forces Building of convergence check arrays

    MaxIterations    := 15;
    MinIterations    := 2;
    MaxControlIterations  := 10;
    ConvergenceTolerance := 0.0001;
    ConvergedFlag := FALSE;

    SampleTheMeters := FALSE;  // Flag to tell solution algorithm to sample the Energymeters

    IsDynamicModel   := FALSE;
    IsHarmonicModel  := FALSE;

    Frequency := DefaultBaseFreq;
    {Fundamental := 60.0; Moved to Circuit and used as default base frequency}
    Harmonic := 1.0;

    FrequencyChanged := TRUE;  // Force Building of YPrim matrices
    DoAllHarmonics   := TRUE;
    FirstIteration   := TRUE;
    DynamicsAllowed  := FALSE;
    SystemYChanged   := TRUE;
    SeriesYInvalid   := TRUE;

    {Define default harmonic list}
    HarmonicListSize := 5;
    HarmonicList := AllocMem(SizeOf(harmonicList^[1])*HarmonicListSize);
    HarmonicList^[1] := 1.0;
    HarmonicList^[2] := 5.0;
    HarmonicList^[3] := 7.0;
    HarmonicList^[4] := 11.0;
    HarmonicList^[5] := 13.0;

    SolutionInitialized := FALSE;
    LoadModel        := POWERFLOW;
    DefaultLoadModel := LoadModel;
    LastSolutionWasDirect := False;

    hYseries := 0;
    hYsystem := 0;
    hY := 0;

    NodeV      := nil;
    dV         := nil;
    Currents   := nil;
    AuxCurrents:= nil;
    VMagSaved  := nil;
    ErrorSaved := nil;
    NodeVbase  := nil;

    UseAuxCurrents := FALSE;

    SolutionCount := 0;

    Dynavars.SolutionMode := SNAPSHOT;
    ControlMode           := CTRLSTATIC;
    DefaultControlMode    := ControlMode;
    Algorithm             := NORMALSOLVE;

    RandomType    := GAUSSIAN;  // default to gaussian
    NumberOfTimes := 100;
    IntervalHrs   := 1.0;

    InitPropertyValues(0);
    ADiakoptics_Ready         :=  False;   // A-Diakoptics needs to be initialized
    if not Assigned(ActorMA_Msg[ActiveActor]) then
      ActorMA_Msg[ActiveActor]  :=  TEvent.Create(nil, True, False,'');
End;

// ===========================================================================================
destructor TSolutionObj.Destroy;
Begin

      Reallocmem(AuxCurrents, 0);
      Reallocmem(Currents, 0);
      Reallocmem(dV, 0);
      Reallocmem(ErrorSaved, 0);
      Reallocmem(NodeV, 0);
      Reallocmem(NodeVbase, 0);
      Reallocmem(VMagSaved, 0);

      If hYsystem <> 0 THEN DeleteSparseSet(hYsystem);
      If hYseries <> 0 THEN DeleteSparseSet(hYseries);
      {by Dahei: }
      Reallocmem(NodeYii, 0);  // for bii
      Reallocmem(pColIdx_Yii, 0);
      Reallocmem(pRowIdx_Yii, 0);
      Reallocmem(pcVals_Yii, 0);
      {---------------------------}
//      SetLogFile ('c:\\temp\\KLU_Log.txt', 0);

      Reallocmem(HarmonicList,0);
      ActorMA_Msg[ActiveActor].SetEvent;

// Sends a message to the working actor
      if ActorHandle[ActiveActor] <> nil then
      Begin
        ActorHandle[ActiveActor].Send_Message(EXIT_ACTOR);
        ActorHandle[ActiveActor].WaitFor;
        FreeandNil(ActorHandle[ActiveActor]);
      End;
      ActorMA_Msg[ActiveActor].Free;
      ActorMA_Msg[ActiveActor]  :=  nil;

      Inherited Destroy;
End;


// ===========================================================================================
FUNCTION TDSSSolution.Edit(ActorID : Integer):Integer;

Begin
     Result := 0;

     ActiveSolutionObj := ActiveCircuit[ActorID].Solution;

     WITH ActiveSolutionObj Do Begin

       // This is all we do here now...
         Solve(ActorID);

     End;  {WITH}
End;

// ===========================================================================================
PROCEDURE TSolutionObj.Solve(ActorID : Integer);
{$IFNDEF FPC}
var
  ScriptEd    : TScriptEdit;
{$ENDIF}
Begin
     ActiveCircuit[ActorID].Issolved := False;
     SolutionWasAttempted[ActorID]   := TRUE;

{Check of some special conditions that must be met before executing solutions}

    If ActiveCircuit[ActorID].EmergMinVolts >= ActiveCircuit[ActorID].NormalMinVolts Then  Begin
            DoSimpleMsg('Error: Emergency Min Voltage Must Be Less Than Normal Min Voltage!' +
                         CRLF + 'Solution Not Executed.', 480);
            Exit;
    End;

    If SolutionAbort Then  Begin
         GlobalResult:= 'Solution aborted.';
         CmdResult := SOLUTION_ABORT;
         ErrorNumber := CmdResult;
         Exit;
    End;
Try
{Main solution Algorithm dispatcher}
    WITH ActiveCircuit[ActorID] Do  Begin

       CASE Year of
         0:  DefaultGrowthFactor := 1.0;    // RCD 8-17-00
       ELSE
           DefaultGrowthFactor := IntPower(DefaultGrowthRate, (year-1));
       END;
    End;
{$IFDEF DLL_ENGINE}
    Fire_InitControls;
{$ENDIF}
    // Creates the actor again in case of being terminated due to an error before
    if ActorHandle[ActorID].Terminated or (ActorHandle[ActorID] = nil)  then
    Begin
      if ActorHandle[ActorID].Terminated then ActorHandle[ActorID].Free;
      New_Actor(ActorID);
    End;
    {CheckFaultStatus;  ???? needed here??}


    // Resets the event for receiving messages from the active actor
      // Updates the status of the Actor in the GUI
      ActorStatus[ActorID]      :=  0;    // Global to indicate that the actor is busy
      ActorMA_Msg[ActorID].ResetEvent;

      {$IFNDEF FPC}
      QueryPerformanceCounter(GStartTime);
      {$ELSE}
      GStartTime := GetTickCount64;
      {$ENDIF}

      If Not NoFormsAllowed Then
      Begin
        if Not IsProgressON and DSSProgressFrm then
        Begin
          case Dynavars.SolutionMode OF
            YEARLYMODE, DUTYCYCLE, LOADDURATION1,
            LOADDURATION2, HARMONICMODE, HARMONICMODET:
            Begin
              if Progress_Actor <> nil then
              Begin
                Progress_Actor.Terminate;
                Progress_Actor  :=  nil;
              End;
              Progress_Actor  :=  TProgressActor.Create();
            End
            Else
            Begin
            // Just other simulation modes, nothing to do
            End;
          End;
        End;
      End;

      // Sends message to start the Simulation
      ActorHandle[ActorID].Send_Message(SIMULATE);
      // If the parallel mode is not active, Waits until the actor finishes
      if not Parallel_enabled then
      Begin
        Wait4Actors(ALL_ACTORS);
        {$IFNDEF FPC}
{        if Not ADiakoptics then
        Begin
          if Not IsDLL then ScriptEd.UpdateSummaryForm('1');
        End
        else
        Begin
          if ActorID = 1 then
            if Not IsDLL then ScriptEd.UpdateSummaryForm('1');
        End;   }
        {$ENDIF}
      End;


Except

    On E:Exception Do Begin
       DoSimpleMsg('Error Encountered in Solve: ' + E.Message, 482);
       SolutionAbort := TRUE;
    End;

End;

End;

// ===========================================================================================
FUNCTION TSolutionObj.Converged(ActorID : Integer):Boolean;

VAR
  i:Integer;
  VMag:Double;
Begin

// base convergence on voltage magnitude
    MaxError := 0.0;
    FOR i := 1 to ActiveCircuit[ActorID].NumNodes Do  Begin
      
        VMag := Cabs(NodeV^[i]);

    { If base specified, use it; otherwise go on present magnitude  }
        If      NodeVbase^[i] > 0.0 Then ErrorSaved^[i] := Abs(Vmag - VmagSaved^[i])/NodeVbase^[i]
        Else If Vmag <> 0.0         Then ErrorSaved^[i] := Abs(1.0 - VmagSaved^[i]/Vmag);

        VMagSaved^[i] := Vmag;  // for next go-'round

        MaxError := Max(MaxError, ErrorSaved^[i]);  // update max error

    End;


{$IFDEF debugtrace}
              Assignfile(Fdebug, 'Debugtrace.csv');
              Append(FDebug);
              If Iteration=1 Then Begin
                Write(Fdebug,'Iter');
                For i := 1 to ActiveCircuit[ActorID].NumNodes Do
                    Write(Fdebug, ', ', ActiveCircuit[ActorID].Buslist.get(ActiveCircuit[ActorID].MapNodeToBus^[i].BusRef), '.', ActiveCircuit[ActiveActor].MapNodeToBus^[i].NodeNum:0);
                Writeln(Fdebug);
              End;
              {*****}
                Write(Fdebug,Iteration:2);
                For i := 1 to ActiveCircuit[ActorID].NumNodes Do
                    Write(Fdebug, ', ', VMagSaved^[i]:8:1);
                Writeln(Fdebug);
                Write(Fdebug,'Err');
                For i := 1 to ActiveCircuit[ActorID].NumNodes Do
                    Write(Fdebug, ', ', Format('%-.5g',[ErrorSaved^[i]]));
                Writeln(Fdebug);
                Write(Fdebug,'Curr');
                For i := 1 to ActiveCircuit[ActorID].NumNodes Do
                    Write(Fdebug, ', ', Cabs(Currents^[i]):8:1);
                Writeln(Fdebug);
              {*****}
                CloseFile(FDebug);
{$ENDIF};

    IF MaxError <= ConvergenceTolerance THEN Result := TRUE
                                        ELSE Result := FALSE;


    ConvergedFlag := Result;
End;


// ===========================================================================================
PROCEDURE TSolutionObj.GetSourceInjCurrents(ActorID : Integer);

// Add in the contributions of all source type elements to the global solution vector InjCurr

VAR
   pElem:TDSSCktElement;

Begin

  WITH ActiveCircuit[ActorID]
  Do Begin

     pElem := Sources.First;
     WHILE pElem<>nil
     Do Begin
       IF pElem.Enabled THEN pElem.InjCurrents(ActorID); // uses NodeRef to add current into InjCurr Array;
       pElem := Sources.Next;
     End;

  End;

End;

// ===========================================================================================
PROCEDURE TSolutionObj.SetGeneratorDispRef(ActorID : Integer);

// Set the global generator dispatch reference

Begin
    WITH ActiveCircuit[ActorID] Do
    Case Dynavars.SolutionMode OF
    
         SNAPSHOT:     GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor;
         YEARLYMODE:   GeneratorDispatchReference := DefaultGrowthFactor * DefaultHourMult.re;
         DAILYMODE:    GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor * DefaultHourMult.re;
         DUTYCYCLE:    GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor * DefaultHourMult.re;
         GENERALTIME:  GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor * DefaultHourMult.re;
         DYNAMICMODE:  GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor;
         HARMONICMODE: GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor;
         MONTECARLO1:  GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor;
         MONTECARLO2:  GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor * DefaultHourMult.re;
         MONTECARLO3:  GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor * DefaultHourMult.re;
         PEAKDAY:      GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor * DefaultHourMult.re;
         LOADDURATION1:GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor * DefaultHourMult.re;
         LOADDURATION2:GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor * DefaultHourMult.re;
         DIRECT:       GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor;
         MONTEFAULT:   GeneratorDispatchReference := 1.0;  // Monte Carlo Fault Cases solve  at peak load only base case
         FAULTSTUDY:   GeneratorDispatchReference := 1.0;
         AUTOADDFLAG:  GeneratorDispatchReference := DefaultGrowthFactor;   // peak load only
         HARMONICMODET: GeneratorDispatchReference := LoadMultiplier * DefaultGrowthFactor * DefaultHourMult.re;
     Else
         DosimpleMsg('Unknown solution mode.', 483);
     End;
End;
// ===========================================================================================
PROCEDURE TSolutionObj.SetGeneratordQdV(ActorID : Integer);

Var
   pGen  :TGeneratorObj;
   Did_One : Boolean;
   GenDispSave :Double;

Begin
     Did_One := False;

     // Save the generator dispatch level and set on high enough to
     // turn all generators on
     GenDispSave := ActiveCircuit[ActorID].GeneratorDispatchReference;
     ActiveCircuit[ActorID].GeneratorDispatchReference := 1000.0;

     WITH ActiveCircuit[ActorID] Do Begin
     
         pGen := Generators.First;
         WHILE pGen <> nil Do Begin
         
           If pGen.Enabled Then Begin

              // for PV generator models only ...
              If pGen.genModel = 3 Then Begin
              
                   pGen.InitDQDVCalc;

                   // solve at base var setting
                   Iteration := 0;
                   Repeat
                       Inc(Iteration);
                       ZeroInjCurr(ActorID);
                       GetSourceInjCurrents(ActorID);
                       pGen.InjCurrents(ActorID);   // get generator currents with nominal vars
                       SolveSystem(NodeV, ActorID);
                   Until Converged(ActorID) or (Iteration >= Maxiterations);

                   pGen.RememberQV(ActorID);  // Remember Q and V
                   pGen.BumpUpQ;

                   // solve after changing vars
                   Iteration := 0;
                   Repeat
                       Inc(Iteration);
                       ZeroInjCurr(ActorID);
                       GetSourceInjCurrents(ActorID);
                       pGen.InjCurrents(ActorID);   // get generator currents with nominal vars
                       SolveSystem(NodeV, ActorID);
                   Until Converged(ActorID) or (Iteration >= Maxiterations);

                   pGen.CalcdQdV(ActorID); // bssed on remembered Q and V and present values of same
                   pGen.ResetStartPoint;

                   Did_One := True;
              End;
          End;
          pGen := Generators.Next;
         End;

     End;

     // Restore generator dispatch reference
     ActiveCircuit[ActorID].GeneratorDispatchReference := GenDispSave;
    TRY
       If Did_One        // Reset Initial Solution
       THEN  SolveZeroLoadSnapShot(ActorID);
    EXCEPT
        ON E:EEsolv32Problem Do Begin
          DoSimpleMsg('From SetGenerator DQDV, SolveZeroLoadSnapShot: ' + CRLF + E.Message  + CheckYMatrixforZeroes(ActorID), 7071);
          Raise ESolveError.Create('Aborting');
        End;
    End;

End;

// ===========================================================================================
PROCEDURE TSolutionObj.DoNormalSolution(ActorID : Integer);

{ Normal fixed-point solution

   Vn+1 = [Y]-1 Injcurr

   Where Injcurr includes only PC elements  (loads, generators, etc.)
   i.e., the shunt elements.

   Injcurr are the current injected INTO the NODE
        (need to reverse current direction for loads)
}

Begin


   Iteration := 0;

 {**** Main iteration loop ****}
   With ActiveCircuit[ActorID] Do
   Repeat
       Inc(Iteration);

       If LogEvents Then LogThisEvent('Solution Iteration ' + IntToStr(Iteration),ActorID);

    { Get injcurrents for all PC devices  }
       ZeroInjCurr(ActorID);
       GetSourceInjCurrents(ActorID);  // sources
       GetPCInjCurr(ActorID);  // Get the injection currents from all the power conversion devices and feeders

       // The above call could change the primitive Y matrix, so have to check
        IF SystemYChanged THEN
        begin
          BuildYMatrix(WHOLEMATRIX, FALSE, ActorID);  // Does not realloc V, I
        end;
        {by Dahei}IF NodeYiiEmpty THEN Get_Yiibus;  //

        IF UseAuxCurrents THEN AddInAuxCurrents(NORMALSOLVE, ActorID);

      // Solve for voltages                      {Note:NodeV[0] = 0 + j0 always}
       If LogEvents Then LogThisEvent('Solve Sparse Set DoNormalSolution ...',ActorID);
       SolveSystem(NodeV, ActorID);
       LoadsNeedUpdating := FALSE;

   Until (Converged(ActorID) and (Iteration >= MinIterations)) or (Iteration >= MaxIterations);

End;


// ===========================================================================================
PROCEDURE TSolutionObj.DoNewtonSolution(ActorID : Integer);

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

Var
   i    :Integer;

Begin

   WITH ActiveCircuit[ActorID] Do
   Begin
       ReAllocMem(dV, SizeOf(dV^[1]) * (NumNodes+1)); // Make sure this is always big enough

       IF   ControlIteration = 1
       THEN GetPCInjCurr(ActorID);  // Update the load multipliers for this solution
       
       Iteration := 0;
       REPEAT
           Inc(Iteration);
           Inc(SolutionCount);    // SumAllCurrents Uses ITerminal  So must force a recalc

        // Get sum of currents at all nodes for all  devices
           ZeroInjCurr(ActorID);
           SumAllCurrents(ActorID);

           // Call to current calc could change YPrim for some devices
           IF SystemYChanged THEN
           begin
              BuildYMatrix(WHOLEMATRIX, FALSE, ActorID);   // Does not realloc V, I
           end;
          {by Dahei}IF NodeYiiEmpty THEN Get_Yiibus;  //

           IF UseAuxCurrents THEN AddInAuxCurrents(NEWTONSOLVE, ActorID);

        // Solve for change in voltages
           SolveSystem(dV, ActorID);

           LoadsNeedUpdating := FALSE;

         // Compute new guess at voltages
           FOR i := 1 to NumNodes Do     // 0 node is always 0
           WITH NodeV^[i] Do
           Begin
                re := re - dV^[i].re;
                im := im - dV^[i].im;
           End;

       UNTIL (Converged(ActorID) and (Iteration >= MinIterations)) or (Iteration >= MaxIterations);
    End;
End;


// ===========================================================================================
PROCEDURE TSolutionObj.DoPFLOWsolution(ActorID : Integer);


Begin

   Inc(SolutionCount);    //Unique number for this solution

   If VoltageBaseChanged Then  InitializeNodeVbase(ActorID); // for convergence test

   IF Not SolutionInitialized THEN Begin
     
        If ActiveCircuit[ActorID].LogEvents Then LogThisEvent('Initializing Solution',ActorID);
      TRY
        //SolveZeroLoadSnapShot;
        SolveYDirect(ActorID);  // 8-14-06 This should give a better answer than zero load snapshot
      EXCEPT
        ON E:EEsolv32Problem Do Begin
          DoSimpleMsg('From DoPFLOWsolution.SolveYDirect: ' + CRLF + E.Message  + CheckYMatrixforZeroes(ActorID), 7072);
          Raise ESolveError.Create('Aborting');
        End;
      END;
        If SolutionAbort Then Exit; // Initialization can result in abort

      TRY
        SetGeneratordQdV(ActorID);  // Set dQdV for Model 3 generators
      EXCEPT
        ON E:EEsolv32Problem Do Begin
          DoSimpleMsg('From DoPFLOWsolution.SetGeneratordQdV: ' + CRLF + E.Message  + CheckYMatrixforZeroes(ActorID), 7073);
          Raise ESolveError.Create('Aborting');
        End;
      END;

        { The above resets the active sparse set to hY }
        SolutionInitialized := True;
   End;


   CASE Algorithm of
      NORMALSOLVE : DoNormalSolution(ActorID);
      NEWTONSOLVE : DoNewtonSolution(ActorID);
   End;

   ActiveCircuit[ActorID].Issolved := ConvergedFlag;
   LastSolutionWasDirect := False;

End;

// ===========================================================================================
FUNCTION TSolutionObj.SolveZeroLoadSnapShot(ActorID : Integer):Integer;

// Solve without load for initialization purposes;

Begin
   Result := 0;

    IF SystemYChanged OR SeriesYInvalid THEN
    Begin
        BuildYMatrix(SERIESONLY, TRUE, ActorID);   // Side Effect: Allocates V
    End;
    Inc(SolutionCount);    //Unique number for this solution

    ZeroInjCurr(ActorID);   // Side Effect: Allocates InjCurr
    GetSourceInjCurrents(ActorID);    // Vsource, Isource and VCCS only

    {Make the series Y matrix the active matrix}
    IF  hYseries = 0 THEN
        Raise EEsolv32Problem.Create('Series Y matrix not built yet in SolveZeroLoadSnapshot.');
    hY := hYseries;

    If ActiveCircuit[ActiveActor].LogEvents Then LogThisEvent('Solve Sparse Set ZeroLoadSnapshot ...',ActorID);

    SolveSystem(NodeV, ActorID);  // also sets voltages in radial part of the circuit if radial solution

    { Reset the main system Y as the solution matrix}
    IF   (hYsystem > 0) and Not SolutionAbort THEN
      hY := hYsystem;
End;

// ===========================================================================================
PROCEDURE  TSolutionObj.SetVoltageBases(ActorID : Integer);

// Set voltage bases using voltage at first node (phase) of a bus

Var
  i:Integer;
  bZoneCalc, bZoneLock: Boolean;

Begin

  TRY
    // don't allow the meter zones to auto-build in this load flow solution, because the
    // voltage bases are not available yet

    bZoneCalc := ActiveCircuit[ActorID].MeterZonesComputed;
    bZoneLock := ActiveCircuit[ActorID].ZonesLocked;
    ActiveCircuit[ActorID].MeterZonesComputed := True;
    ActiveCircuit[ActorID].ZonesLocked := True;

    SolveZeroLoadSnapShot(ActorID);

    WITH ActiveCircuit[ActorID] Do
      FOR i := 1 to NumBuses Do
        WITH Buses^[i] Do
          kVBase := NearestBasekV( Cabs(NodeV^[GetRef(1)]) * 0.001732) / SQRT3;  // l-n base kV

    InitializeNodeVbase(ActorID);      // for convergence test

    ActiveCircuit[ActorID].Issolved := True;

    // now build the meter zones
    ActiveCircuit[ActorID].MeterZonesComputed := bZoneCalc;
    ActiveCircuit[ActorID].ZonesLocked := bZoneLock;
    ActiveCircuit[ActorID].DoResetMeterZones(ActorID);

  EXCEPT
    ON E:EEsolv32Problem Do Begin
      DoSimpleMsg('From SetVoltageBases.SolveZeroLoadSnapShot: ' + CRLF + E.Message  + CheckYMatrixforZeroes(ActorID), 7075);
      Raise ESolveError.Create('Aborting');
    End;
  END;

End;

PROCEDURE TSolutionObj.SnapShotInit(ActorID : Integer);

Begin

   SetGeneratorDispRef(ActorID);
   ControlIteration   := 0;
   ControlActionsDone := False;
   MostIterationsDone := 0;
   LoadsNeedUpdating := TRUE;  // Force the loads to update at least once

End;

PROCEDURE TSolutionObj.CheckControls(ActorID : Integer);

Begin
      If ControlIteration < MaxControlIterations then Begin
           IF ConvergedFlag Then Begin
               If ActiveCircuit[ActorID].LogEvents Then LogThisEvent('Control Iteration ' + IntToStr(ControlIteration),ActorID);
               Sample_DoControlActions(ActorID);
               Check_Fault_Status(ActorID);
           End
           ELSE
               ControlActionsDone := TRUE; // Stop solution process if failure to converge
       End;

       IF SystemYChanged THEN
       begin
          BuildYMatrix(WHOLEMATRIX, FALSE, ActorID); // Rebuild Y matrix, but V stays same
       end;
       {by Dahei}IF NodeYiiEmpty THEN Get_Yiibus;
End;

// ===========================================================================================
FUNCTION TSolutionObj.SolveSnap(ActorID : integer):Integer;  // solve for now once

VAR
   TotalIterations  :Integer;

Begin
//      if Solution then
   SnapShotInit(ActorID);
   TotalIterations    := 0;
   {$IFNDEF FPC}
   QueryPerformanceCounter(SolveStartTime);
   {$ELSE}
   SolveStartTime := GetTickCount64;
   {$ENDIF}
   REPEAT

       Inc(ControlIteration);

       Result := SolveCircuit(ActorID);  // Do circuit solution w/o checking controls
       {Now Check controls}
{$IFDEF DLL_ENGINE}
       Fire_CheckControls;
{$ENDIF}
       CheckControls(ActorID);

       {For reporting max iterations per control iteration}
       If Iteration > MostIterationsDone  THEN MostIterationsDone := Iteration;

       TotalIterations := TotalIterations + Iteration;

   UNTIL ControlActionsDone or (ControlIteration >= MaxControlIterations);

   If Not ControlActionsDone and (ControlIteration >= MaxControlIterations) then
   Begin
    Windows.MessageBox(0,'Warning Max Control Iterations Exceeded. ' + CRLF + 'Tip: Show Eventlog to debug control settings.','Warning',MB_OK);
    SolutionAbort := TRUE;   // this will stop this message in dynamic power flow modes
   End;

   If ActiveCircuit[ActorID].LogEvents Then LogThisEvent('Solution Done',ActorID);

{$IFDEF DLL_ENGINE}
   Fire_StepControls;
{$ENDIF}
   {$IFNDEF FPC}
   QueryPerformanceCounter(SolveEndtime);
   {$ELSE}
   SolveEndTime := GetTickCount64;
   {$ENDIF}
   Solve_Time_Elapsed := ((SolveEndtime-SolveStartTime)/CPU_Freq)*1000000;
   Iteration := TotalIterations;  { so that it reports a more interesting number }

End;

// ===========================================================================================
FUNCTION TSolutionObj.SolveDirect(ActorID : integer):Integer;  // solve for now once, direct solution

Begin
   Result := 0;

   LoadsNeedUpdating := TRUE;  // Force possible update of loads and generators
   {$IFNDEF FPC}
   QueryPerformanceCounter(SolveStartTime);
   {$ELSE}
   SolveStartTime := GetTickCount64;
   {$ENDIF}

   If SystemYChanged THEN
   begin
        BuildYMatrix(WHOLEMATRIX, TRUE, ActorID);   // Side Effect: Allocates V
   end;
   {by Dahei}IF NodeYiiEmpty THEN Get_Yiibus;  //

   Inc(SolutionCount);   // Unique number for this solution

   ZeroInjCurr(ActorID);   // Side Effect: Allocates InjCurr
   GetSourceInjCurrents(ActorID);

   // Pick up PCELEMENT injections for Harmonics mode and Dynamics mode
   // Ignore these injections for powerflow; Use only admittance in Y matrix
   If IsDynamicModel or IsHarmonicModel Then  GetPCInjCurr(ActorID);

   IF   SolveSystem(NodeV, ActorID) = 1   // Solve with Zero injection current
   THEN Begin
       ActiveCircuit[ActorID].IsSolved := TRUE;
       ConvergedFlag := TRUE;
   End;
   {$IFNDEF FPC}
   QueryPerformanceCounter(SolveEndtime);
   {$ELSE}
   SolveEndTime := GetTickCount64;
   {$ENDIF}
   Solve_Time_Elapsed  := ((SolveEndtime-SolveStartTime)/CPU_Freq)*1000000;
   Total_Time_Elapsed  :=  Total_Time_Elapsed + Solve_Time_Elapsed;
   Iteration := 1;
   LastSolutionWasDirect := TRUE;

End;


function TSolutionObj.SolveCircuit(ActorID : integer): Integer;
begin
       Result := 0;
       IF LoadModel=ADMITTANCE
       Then
            TRY
              SolveDirect(ActorID)     // no sense horsing around when it's all admittance
            EXCEPT
                ON E:EEsolv32Problem
                Do Begin
                  DoSimpleMsg('From SolveSnap.SolveDirect: ' + CRLF + E.Message  + CheckYMatrixforZeroes(ActorID), 7075);
                  Raise ESolveError.Create('Aborting');
                End;
            END
       Else  Begin
           TRY
              IF SystemYChanged THEN
              begin
                  BuildYMatrix(WHOLEMATRIX, TRUE, ActorID);   // Side Effect: Allocates V
              end;
              {by Dahei: Get Y matrix for solution}
              IF NodeYiiEmpty  THEN Get_Yiibus;   //IF SystemYChanged
              {-------------------------------------}

              DoPFLOWsolution(ActorID);
           EXCEPT
             ON E:EEsolv32Problem
             Do Begin
               DoSimpleMsg('From SolveSnap.DoPflowSolution: ' + CRLF + E.Message  + CheckYMatrixforZeroes(ActorID), 7074);
               Raise ESolveError.Create('Aborting');
             End;
           END
       End;

end;

// ===========================================================================================
PROCEDURE TSolutionObj.ZeroInjCurr(ActorID : Integer);
VAR
    I:Integer;
Begin
    FOR i := 0 to ActiveCircuit[ActorID].NumNodes Do Currents^[i] := CZERO;
End;
// ===========================================================================================
PROCEDURE TSolutionObj.Upload2IncMatrix;
var
  CIdx  : Integer;

Begin
  // Uploads the values to the incidence matrix
  IncMat.insert((ActiveIncCell[0] - 1),(ActiveIncCell[1] - 2),ActiveIncCell[2]);
  ActiveIncCell[2]  :=  -1;
End;
// ===========================================================================================
procedure TSolutionObj.AddLines2IncMatrix(ActorID : Integer);
var
  LineBus       : String;
  elem          :TLineObj;
  TermIdx,
  CIdx,
  BusdotIdx     : Integer;
  EndFlag       : Boolean;
  counter       : Integer;
begin
// This rouitne adds the Lines to the incidence matrix vectors
  with ActiveCircuit[ActorID] do
  Begin
    elem        :=  Lines.First;
//    Counter     :=  0;
    while elem <> nil do
    Begin
      if elem.Enabled then
      Begin
        ActiveIncCell[2]  :=  1;
        inc(temp_counter);
        setlength(Inc_Mat_Rows,temp_counter);
        Inc_Mat_Rows[temp_counter - 1]  :=  'Line.'+elem.Name;
        for TermIdx := 1 to 2 do
        Begin
            LineBus           :=  elem.GetBus(TermIdx);
            BusdotIdx         :=  ansipos('.',LineBus);
            if BusdotIdx <> 0 then
              LineBus         :=  Copy(LineBus,0,BusdotIdx-1);  // removes the dot from the Bus Name
            // Evaluates the position of the Bus in the array
            ActiveIncCell[1]  :=  1;
            EndFlag  :=  True;
            while (ActiveIncCell[1] <= NumBuses) and (EndFlag) DO
            Begin
                if LineBus = BusList.Get(ActiveIncCell[1]) then EndFlag := False;
                ActiveIncCell[1]  :=  ActiveIncCell[1]  + 1;
            End;
            Upload2IncMatrix;
//            inc(Counter);
        End;
        inc(ActiveIncCell[0]);
      End
      else
      begin
        Counter :=0;
      end;
      elem  :=  Lines.Next;
    End;
//    Counter :=  Counter;
  End;
end;
// ===========================================================================================
procedure TSolutionObj.AddXfmr2IncMatrix(ActorID : Integer);
var
  LineBus      : String;
  elem         :TTransfObj;
  TermIdx,
  BusdotIdx,
  CIdx        : Integer;
  EndFlag     : Boolean;
  lst         : TPointerList;
  Counter     : Integer;

begin
// This rouitne adds the Transformers to the incidence matrix vectors
  with ActiveCircuit[ActorID] do
  Begin
    lst := ActiveCircuit[ActorID].Transformers;
    elem        :=  lst.First;
    while elem <> nil do
    Begin
      if elem.Enabled then
      Begin
        ActiveIncCell[2]       :=  1;
        inc(temp_counter);
        setlength(Inc_Mat_Rows,temp_counter);
        Inc_Mat_Rows[temp_counter - 1]  :=  'Transformer.'+elem.Name;
        for TermIdx := 1 to elem.NumberOfWindings do
        Begin
            LineBus     :=  elem.GetBus(TermIdx);
            BusdotIdx   :=  ansipos('.',LineBus);
            if BusdotIdx <> 0 then
              LineBus   :=  Copy(LineBus,0,BusdotIdx-1);  // removes the dot from the Bus Name
            // Evaluates the position of the Bus in the array
            ActiveIncCell[1]  :=  1;
            EndFlag  :=  True;
            while (ActiveIncCell[1] <= NumBuses) and (EndFlag) DO
            Begin
                if LineBus = BusList.Get(ActiveIncCell[1]) then EndFlag := False;
                ActiveIncCell[1]  :=  ActiveIncCell[1]  + 1;
            End;
            Upload2IncMatrix;
       End;
        inc(ActiveIncCell[0]);
      End;
      elem  :=  lst.Next;
    End;
  End;
end;
// ===========================================================================================
procedure TSolutionObj.AddSeriesCap2IncMatrix(ActorID : Integer);
var
  CapBus      : String;
  elem        : TCapacitorObj;
  lst         : TPointerList;
  CapTermIdx,
  BusdotIdx,
  CIdx        : Integer;
  CapEndFlag  : Boolean;
begin
// This rouitne adds the series capacitors to the incidence matrix vectors
  with ActiveCircuit[ActorID] do
  Begin
    lst         :=  ShuntCapacitors;
    elem        := lst.First;
    while elem <> nil do
    Begin
      if elem.NumTerminals > 1 then
      Begin
        if elem.Enabled then
        Begin
          inc(temp_counter);
          setlength(Inc_Mat_Rows,temp_counter);
          Inc_Mat_Rows[temp_counter - 1]  :=  'Capacitor.'+elem.Name;
          ActiveIncCell[2]  :=  1;
          for CapTermIdx := 1 to 2 do
          Begin
            CapBus      :=  elem.GetBus(CapTermIdx);
            BusdotIdx   :=  ansipos('.',CapBus);
            if BusdotIdx <> 0 then
              CapBus    :=  Copy(CapBus,0,BusdotIdx-1);  // removes the dot from the Bus Name
            // Evaluates the position of the Bus in the array
            ActiveIncCell[1]  :=  1;
            CapEndFlag  :=  True;
            while (ActiveIncCell[1] <= NumBuses) and (CapEndFlag) DO
            Begin
                if CapBus = BusList.Get(ActiveIncCell[1]) then  CapEndFlag := False;
                ActiveIncCell[1]  :=  ActiveIncCell[1]  + 1;
            End;
            Upload2IncMatrix;
          End;
          inc(ActiveIncCell[0]);
        End;
      End;
      elem  :=  lst.Next;
    End;
  End;
end;
// ===========================================================================================
procedure TSolutionObj.AddSeriesReac2IncMatrix(ActorID : Integer);
var
  RBus      : String;
  elem,
  DevClassIndex :Integer;
  TermIdx,
  BusdotIdx,
  CIdx     : Integer;
  EndFlag  : Boolean;
begin
// This rouitne adds the series reactors to the incidence matrix vectors
  with ActiveCircuit[ActorID] do
  Begin
    DevClassIndex := ClassNames[ActorID].Find('reactor');
    LastClassReferenced[ActorID] := DevClassIndex;
    ActiveDSSClass[ActorID] := DSSClassList[ActorID].Get(LastClassReferenced[ActorID]);
    elem        := ActiveDSSClass[ActorID].First;
    while elem <> 0 do
    Begin
      RBus        :=  ActiveCktElement.GetBus(2);
      BusdotIdx   :=  ansipos('.0',RBus);
      if BusdotIdx = 0 then
      Begin
        inc(temp_counter);
        setlength(Inc_Mat_Rows,temp_counter);
        Inc_Mat_Rows[temp_counter - 1]  :=  'Reactor.'+ActiveCktElement.Name;
        ActiveIncCell[2]  :=  1;
        for TermIdx := 1 to 2 do
        Begin
          RBus      :=  ActiveCktElement.GetBus(TermIdx);
          BusdotIdx   :=  ansipos('.',RBus);
          if BusdotIdx <> 0 then
            RBus    :=  Copy(RBus,0,BusdotIdx-1);  // removes the dot from the Bus Name
          // Evaluates the position of the Bus in the array
          ActiveIncCell[1]  :=  1;
          EndFlag  :=  True;
          while (ActiveIncCell[1] <= NumBuses) and (EndFlag) DO
          Begin
              if RBus = BusList.Get(ActiveIncCell[1]) then  EndFlag := False;
              ActiveIncCell[1]  :=  ActiveIncCell[1]  + 1;
          End;
          Upload2IncMatrix;
        End;
      End;
      elem  :=  ActiveDSSClass[ActorID].Next;
      inc(ActiveIncCell[0]);
    End;
  End;
end;
//*********Routine for extracting the Branch to Node incidence matrix***********
//*     The order depends on the way the lines, xfmr, series cap and reactors  *
//******************************************************************************
PROCEDURE TSolutionObj.Calc_Inc_Matrix(ActorID : Integer);
var
  dlong : Integer;
Begin
  // If the sparse matrix obj doesn't exists creates it, otherwise deletes the content
  if IncMat = nil then
    IncMat  :=  Tsparse_matrix.Create
  else
    IncMat.reset;

  if ActiveCircuit[ActorID]<>nil then
    with ActiveCircuit[ActorID] do
    Begin
      temp_counter  :=  0;
      ActiveIncCell[0]  := 1;           // Activates row 1 of the incidence matrix
      // Now we proceed to evaluate the link branches
      AddLines2IncMatrix(ActorID);      // Includes the Lines
      AddXfmr2IncMatrix(ActorID);       // Includes the Xfmrs
      AddSeriesCap2IncMatrix(ActorID);  // Includes Series Cap
      AddSeriesReac2IncMatrix(ActorID); // Includes Series Reactors
      IncMat_Ordered  :=  False;
    End;
End;
{*******************************************************************************
* This function delivers the Row index connected to the Column at the input    *
*                   Inside the B2N incidence Matrix                            *
********************************************************************************}
function TSolutionObj.get_IncMatrix_Row(Col:Integer):Integer;
var
  Tflag : Boolean;
  idx_1 : Integer;
begin
  Result            :=  -1;
  Tflag             :=  True;
  for idx_1 := 1 to (IncMat.NZero - 1) do    //Looks for the Column in the IncMatrix
  begin
    if (IncMat.data[idx_1][1] = Col) and Tflag then
    begin
      Result :=   IncMat.data[idx_1][0];
      Tflag  :=   False;
    end;
  end;
end;
{*******************************************************************************
* This function delivers the Column index connected to the Row at the input    *
*                   Inside the B2N incidence Matrix                            *
********************************************************************************}
function TSolutionObj.get_IncMatrix_Col(Row:Integer):Integer;
var
  Tflag : Boolean;
  Idx_1 : Integer;
Begin
  Result  :=  -1;
  Tflag   :=  True;    // Detection Flag
  for Idx_1 := 1 to (IncMat.NZero - 1) do    //Looks for the row in the IncMatrix
  begin
    if (IncMat.data[Idx_1][0] = Row) and Tflag then
    begin
      setlength(Active_Cols,2);
      setlength(Active_Cols_Idx,2);
      Active_Cols[0]      :=  IncMat.data[Idx_1][1];     //Stores the indexes of both columns for the link branch
      Active_Cols[1]      :=  IncMat.data[Idx_1 + 1][1]; //In case they need to be used in the future by the caller
      Active_Cols_Idx[0]  :=  IncMat.data[Idx_1 - 1][2]; //Stores the indexes of both columns for the link branch
      Active_Cols_Idx[1]  :=  IncMat.data[Idx_1][2];     //In case they need to be used in the future by the caller
      Result :=  IncMat.data[Idx_1][1];
      Tflag  :=  False;
    end;
  end;
End;

//*********Routine for extracting the Branch to Node incidence matrix***********
//*     Organized hierarchically. This routine also calculates the             *
//*     Levels vector for defining the proximity of the bus to the circuit's   *
//*     Backbone. To do it, this routine uses the CktTree class                *
//******************************************************************************
PROCEDURE TSolutionObj.Calc_Inc_Matrix_Org(ActorID : Integer);

Var
//  Ftree       : TextFile;                           // For debugging
  pdElem         : TPDElement;
  topo           : TCktTree;
//  TreeNm,                                           // For debugging
//  FileRoot,                                         // For debugging
  PDE_Name       : String;                            // Name of the PDElement
  PDE_Buses      : Array of string;                   // Buses of the PDElement
  Temp_Array     : Array of Integer;                  // Local Shared variable
  nLevels,                                            // Current number of levels for the active Bus
  i,                                                  // Default counter
  j,                                                  // Default counter
  j2,                                                 // Default counter
  ZeroLevel,                                          // Number of Zero level Buses
  BusdotIdx,
  row,
  col,
  val,                                          // Local Shared variable
  nPDE           : Integer;                           // PDElements index
Begin
  Try
    if ActiveCircuit[ActorID]<>nil then
    begin
//      TreeNm := FileRoot + 'TopoTree_Cols.csv';   // For debuging
      topo := ActiveCircuit[ActiveActor].GetTopology;
      nLevels     := 0;
      nPDE        := 0;
      setlength(Inc_Mat_Cols,0);
      //Init the spaser matrix
      if IncMat = nil then
        IncMat  :=  Tsparse_matrix.Create
      else
        IncMat.reset;

      ActiveIncCell[0]  := -1;           // Activates row 1 of the incidence matrix
      If Assigned (topo) Then Begin
        PDElem                  := topo.First;
        While Assigned (PDElem) do begin
          nLevels                 := topo.Level;
          PDE_Name                :=  PDElem.ParentClass.Name+'.'+PDElem.Name;
//******************Gets the buses to which the PDE is connected****************
          With ActiveCircuit[ActorID] do
          Begin
            ActiveCircuit[ActorID].SetElementActive(PDE_Name);
            SetLength(PDE_Buses,ActiveCktElement.Nterms);
            For i := 1 to  ActiveCktElement.Nterms Do
            Begin
              PDE_Buses[i-1] :=  ActiveCktElement.GetBus(i);
              BusdotIdx         :=  ansipos('.',PDE_Buses[i-1]);
              if BusdotIdx <> 0 then
                PDE_Buses[i-1]  :=  Copy(PDE_Buses[i-1],0,BusdotIdx-1);  // removes the dot from the Bus Name
            End;
            if length(Inc_Mat_Cols) = 0 then  //First iteration so the Cols array will be loaded
            begin
              setlength(Inc_Mat_Cols,1);
              setlength(Inc_Mat_Levels,1);
              Inc_Mat_Cols[0]   :=  PDE_Buses[0];
              Inc_Mat_levels[0] :=  nLevels;
            end
            else                               //The Cols array is populated with something
            begin
              inc(nPDE);
              setlength(Inc_Mat_Rows,nPDE);
              Inc_Mat_Rows[nPDE-1]    :=  PDE_Name;
              for j := 0 to ActiveCktElement.Nterms-1 do
              Begin
                row :=  ActiveIncCell[0];                 //Sets the row
                BusdotIdx           :=  -1;               // Flag to not create a new variable
                for i := 0 to length(Inc_Mat_Cols)-1 do   // Checks if the bus already exists in the Cols array
                  if Inc_Mat_Cols[i] = PDE_Buses[j] then  BusdotIdx :=  i;
                if BusdotIdx >= 0 then  col :=  BusdotIdx   //Sets the Col
                else
                Begin
                  setlength(Inc_Mat_Cols,length(Inc_Mat_Cols)+1);
                  setlength(Inc_Mat_levels,length(Inc_Mat_levels)+1);
                  Inc_Mat_Cols[length(Inc_Mat_Cols)-1]  :=  PDE_Buses[j];
                  Inc_Mat_levels[length(Inc_Mat_Cols)-1]:=  nLevels;
                  col :=  length(Inc_Mat_Cols) - 1; //Sets the Col
                End;
                if j = 0 then val  :=  1 //Sets the value
                else val :=  -1;
                IncMat.insert(row,col,val);
              End;
            end;
          End;
          inc(ActiveIncCell[0]);
          PDElem := topo.GoForward;
        End;
      End;
{*******************************************************************************
*   Now the levels array needs to be reprocessed to get the 0 level buses,     *
*   they are on a continuous path from the feeder head to the feeder end       *
********************************************************************************}
      BusdotIdx :=  MaxIntValue(Inc_Mat_levels);
      for i := 0 to length(Inc_Mat_levels) do
        if Inc_Mat_levels[i] = BusdotIdx then nLevels := i;
      for j := 1 to BusdotIdx-1 do
      begin
        for i := 0 to nLevels do
        begin
          if Inc_Mat_levels[i] = j then ZeroLevel := i;
        end;
        Inc_Mat_levels[ZeroLevel]  := 0;
      end;
//**********Normalize the branches of the level between zero level buses********
      BusdotIdx :=  0;
      j         :=  0;
      ZeroLevel :=  0;
      SetLength(Temp_Array,0);
      for i := 0 to high(Inc_Mat_levels) do
      begin
          if (Inc_Mat_levels[i] = 0) then
          begin
            if length(Temp_Array) > 0 then    // The array subset is large enough for
            begin                             //Normalizing it
              BusdotIdx :=  MinIntValue(Temp_Array) - 1;
              for j2 := ZeroLevel to (length(Temp_Array) + ZeroLevel - 1) do
                Inc_Mat_levels[j2]  :=  Inc_Mat_levels[j2] - BusdotIdx;
              SetLength(Temp_Array,0);
            end;
            ZeroLevel :=  i + 1;
          end
          else
          begin
            setlength(Temp_Array,(length(Temp_Array) + 1));
            Temp_Array[High(Temp_Array)]  :=  Inc_Mat_levels[i];
          end;
      end;
//************Verifies is something else was missing at the end*****************
      if (ZeroLevel < (length(Inc_Mat_levels) - 1)) then
      begin
        BusdotIdx :=  0;                                                // Counter for defining the level
        j         :=  0;                                                // Stores the previous value (shift reg)
        for j2 := ZeroLevel to High(Inc_Mat_levels) do
        begin
          if Inc_Mat_levels[j2] >= j then inc(BusdotIdx)
          else
          begin
            ActiveIncCell[1]  :=  get_IncMatrix_Row(j2);                //Looks for the Column in the IncMatrix
            if ActiveIncCell[1] < 0 then                                //Checks if the col was located (just in case)
              BusdotIdx     :=  1
            else
            begin
              ActiveIncCell[2] := get_IncMatrix_Col(ActiveIncCell[1]);  //Looks for the row in the IncMatrix
              if Active_Cols[0] = j2 then BusdotIdx :=  Inc_Mat_levels[Active_Cols[1]]  + 1
              else  BusdotIdx :=  Inc_Mat_levels[ActiveIncCell[2]]  + 1;
            end;
          end;
          j :=  Inc_Mat_levels[j2];
          Inc_Mat_levels[j2]  :=  BusdotIdx;
        end;
      end;
      IncMat_Ordered  :=  True;
    end;
  Finally

  End;
End;
{*******************************************************************************
*           Routine created to empty a recently created folder                 *
********************************************************************************}
procedure DelFilesFromDir(Directory, FileMask: string; DelSubDirs: Boolean);
{$IFNDEF FPC}
var
  SourceLst: string;
  FOS: TSHFileOpStruct;
{$ENDIF}
begin
{$IFNDEF FPC}
  FillChar(FOS, SizeOf(FOS), 0);
  FOS.wFunc := FO_DELETE;
  SourceLst := Directory + '\' + FileMask + #0;
  FOS.pFrom := PChar(SourceLst);
  if not DelSubDirs then
    FOS.fFlags := FOS.fFlags OR FOF_FILESONLY;
  // Remove the next line if you want a confirmation dialog box
  FOS.fFlags := FOS.fFlags OR FOF_NOCONFIRMATION;
  // Add the next line for a "silent operation" (no progress box)
  FOS.fFlags := FOS.fFlags OR FOF_SILENT;
  {$IFDEF MSWINDOWS}
  SHFileOperation(FOS);
  {$ENDIF}
{$ENDIF}
end;
{*******************************************************************************
*   This routine evaluates if the current location is the best or if its       *
*   Necessary to move back one PDE just to cover a wider area                  *
********************************************************************************}
function TSolutionObj.CheckLocationIdx(Idx : Integer): Integer;
begin
  if Inc_Mat_Levels[Idx - 1] = 0 then  Result :=  idx - 1
  else Result :=  idx;
end;

//----------------------------------------------------------------------------
FUNCTION TDSSSolution.Init(Handle:Integer; ActorID : Integer):Integer;

Begin
   DoSimpleMsg('Need to implement TSolution.Init', -1);
   Result := 0;
End;


// ===========================================================================================
PROCEDURE TSolutionObj.GetPCInjCurr(ActorID : Integer);
  VAR
     pElem:TDSSCktElement;

{ Get inj currents from all enabled PC devices }

Begin

  WITH ActiveCircuit[ActorID]
  Do Begin
     pElem := PCElements.First;
     WHILE pElem <> nil
     Do Begin
       WITH pElem Do IF Enabled  THEN InjCurrents(ActorID); // uses NodeRef to add current into InjCurr Array;
       pElem := PCElements.Next;
     End;
   End;

End;

PROCEDURE TSolutionObj.DumpProperties(Var F:TextFile; complete:Boolean);

VAR
   i,j              :Integer;

   // for dumping the matrix in compressed columns
   p                :LongWord;
   hY               :NativeUInt;
   nBus, nNZ        :LongWord;
   ColPtr, RowIdx   :array of LongWord;
   cVals            :array of Complex;
Begin

  Writeln(F, '! OPTIONS');

  // Inherited DumpProperties(F,Complete);

  Writeln(F, '! NumNodes = ',ActiveCircuit[ActiveActor].NumNodes:0);

    {WITH ParentClass Do
     FOR i := 1 to NumProperties Do
     Begin
        Writeln(F,'Set ',PropertyName^[i],'=',PropertyValue^[i]);
     End;
     }
     Writeln(F, 'Set Mode=', GetSolutionModeID);
     Writeln(F, 'Set ControlMode=', GetControlModeID);
     Writeln(F, 'Set Random=', GetRandomModeID);
     Writeln(F, 'Set hour=',   DynaVars.intHour:0);
     Writeln(F, 'Set sec=',    Format('%-g', [DynaVars.t]));
     Writeln(F, 'Set year=',   Year:0);
     Writeln(F, 'Set frequency=',   Format('%-g',[Frequency]));
     Writeln(F, 'Set stepsize=',    Format('%-g', [DynaVars.h]));
     Writeln(F, 'Set number=',   NumberOfTimes:0);
     Writeln(F, 'Set circuit=',  ActiveCircuit[ActiveActor].Name);
     Writeln(F, 'Set editor=',   DefaultEditor);
     Writeln(F, 'Set tolerance=', Format('%-g', [ConvergenceTolerance]));
     Writeln(F, 'Set maxiterations=',   MaxIterations:0);
     Writeln(F, 'Set miniterations=',   MinIterations:0);
     Writeln(F, 'Set loadmodel=', GetLoadModel);

     Writeln(F, 'Set loadmult=',    Format('%-g', [ActiveCircuit[ActiveActor].LoadMultiplier]));
     Writeln(F, 'Set Normvminpu=',  Format('%-g', [ActiveCircuit[ActiveActor].NormalMinVolts]));
     Writeln(F, 'Set Normvmaxpu=',  Format('%-g', [ActiveCircuit[ActiveActor].NormalMaxVolts]));
     Writeln(F, 'Set Emergvminpu=', Format('%-g', [ActiveCircuit[ActiveActor].EmergMinVolts]));
     Writeln(F, 'Set Emergvmaxpu=', Format('%-g', [ActiveCircuit[ActiveActor].EmergMaxVolts]));
     Writeln(F, 'Set %mean=',   Format('%-.4g', [ActiveCircuit[ActiveActor].DefaultDailyShapeObj.Mean * 100.0]));
     Writeln(F, 'Set %stddev=', Format('%-.4g', [ActiveCircuit[ActiveActor].DefaultDailyShapeObj.StdDev * 100.0]));
     Writeln(F, 'Set LDCurve=', ActiveCircuit[ActiveActor].LoadDurCurve);  // Load Duration Curve
     Writeln(F, 'Set %growth=', Format('%-.4g', [((ActiveCircuit[ActiveActor].DefaultGrowthRate-1.0)*100.0)]));  // default growth rate
     With ActiveCircuit[ActiveActor].AutoAddObj Do
     Begin
       Writeln(F, 'Set genkw=', Format('%-g', [GenkW]));
       Writeln(F, 'Set genpf=', Format('%-g', [GenPF]));
       Writeln(F, 'Set capkvar=', Format('%-g', [Capkvar]));
       Write(F, 'Set addtype=');
       Case Addtype of
          GENADD:Writeln(F,'generator');
          CAPADD:Writeln(F,'capacitor');
       End;
     End;
     Write(F, 'Set allowduplicates=');
     If  ActiveCircuit[ActiveActor].DuplicatesAllowed THEN Writeln(F, 'Yes') ELSE Writeln(F,'No');
     Write(F, 'Set zonelock=');
     IF ActiveCircuit[ActiveActor].ZonesLocked THEN Writeln(F, 'Yes') ELSE Writeln(F,'No');
     Writeln(F, 'Set ueweight=',    ActiveCircuit[ActiveActor].UEWeight:8:2);
     Writeln(F, 'Set lossweight=',  ActiveCircuit[ActiveActor].LossWeight:8:2);
     Writeln(F, 'Set ueregs=',   IntArraytoString(ActiveCircuit[ActiveActor].UEregs,   ActiveCircuit[ActiveActor].NumUERegs));
     Writeln(F, 'Set lossregs=', IntArraytoString(ActiveCircuit[ActiveActor].Lossregs, ActiveCircuit[ActiveActor].NumLossRegs));
     Write(F, 'Set voltagebases=(');  //  changes the default voltage base rules
     i:=1;
     WITH ActiveCircuit[ActiveActor] Do
     WHILE LegalVoltageBases^[i] > 0.0 Do
     Begin
         Write(F, LegalVoltageBases^[i]:10:2);
         inc(i);
     End;
     Writeln(F,')');
     Case Algorithm of
       NORMALSOLVE: Writeln(F, 'Set algorithm=normal');
       NEWTONSOLVE: Writeln(F, 'Set algorithm=newton');
     End;
     Write(F,'Set Trapezoidal=');
     IF   ActiveCircuit[ActiveActor].TrapezoidalIntegration
     THEN Writeln(F, 'yes')
     ELSE Writeln(F, 'no');
     Writeln(F, 'Set genmult=', Format('%-g', [ActiveCircuit[ActiveActor].GenMultiplier]));

     Writeln(F, 'Set Basefrequency=', Format('%-g', [ActiveCircuit[ActiveActor].Fundamental]));

     Write(F, 'Set harmonics=(');  //  changes the default voltage base rules
       IF DoAllHarmonics Then Write(F, 'ALL')
       ELSE FOR i := 1 to HarmonicListSize Do Write(F, Format('%-g, ', [HarmonicList^[i]]));
     Writeln(F,')');
     Writeln(F, 'Set maxcontroliter=',  MaxControlIterations:0);
     Writeln(F);

  If Complete THEN With ActiveCircuit[ActiveActor] Do Begin

      hY := Solution.hY;

      // get the compressed columns out of KLU
      FactorSparseMatrix(hY); // no extra work if already done
      GetNNZ(hY, @nNZ);
      GetSize(hY, @nBus);
      SetLength (ColPtr, nBus + 1);
      SetLength (RowIdx, nNZ);
      SetLength (cVals, nNZ);
      GetCompressedMatrix(hY, nBus + 1, nNZ, @ColPtr[0], @RowIdx[0], @cVals[0]);

      Writeln(F,'System Y Matrix (Lower Triangle by Columns)');
      Writeln(F);
      Writeln(F,'  Row  Col               G               B');
      Writeln(F);

      // traverse the compressed column format
      for j := 0 to nBus - 1 do begin /// the zero-based column
        for p := ColPtr[j] to ColPtr[j+1] - 1
        do begin
              i := RowIdx[p];  // the zero-based row
              Writeln (F, Format('[%4d,%4d] = %12.5g + j%12.5g', [i+1, j+1, cVals[p].re, cVals[p].im]));
        end;
      end;
  End;
End;

FUNCTION TSolutionObj.VDiff(i,j:Integer):Complex;

Begin
    Result := Csub(NodeV^[i], NodeV^[j]);  // V1-V2
End;


PROCEDURE TSolutionObj.WriteConvergenceReport(const Fname:String);
Var
   i:Integer;
   F:TextFile;
Begin
   TRY
     Assignfile(F,Fname);
     ReWrite(F);

     Writeln(F);
     Writeln(F,'-------------------');
     Writeln(F, 'Convergence Report:');
     Writeln(F,'-------------------');
     Writeln(F,'"Bus.Node", "Error", "|V|","Vbase"');
     WITH ActiveCircuit[ActiveActor] Do
       FOR i := 1 to NumNodes Do
         WITH MapNodeToBus^[i]   Do
         Begin
              Write(F, '"', pad((BusList.Get(Busref)+'.'+IntToStr(NodeNum)+'"'), 18) );
              Write(F,', ', ErrorSaved^[i]:10:5);
              Write(F,', ', VmagSaved^[i]:14);
              Write(F,', ', NodeVbase^[i]:14);
              Writeln(F);
         End;

    Writeln(F);
    Writeln(F, 'Max Error = ', MaxError:10:5);

  FINALLY

     CloseFile(F);
     FireOffEditor(Fname);

  End;

End;

// =========================================================================================== =
PROCEDURE TSolutionObj.SumAllCurrents(ActorID : Integer);

Var
   pelem :TDSSCktElement;

begin
     WITH  ActiveCircuit[ActorID] Do
       Begin
          pelem := CktElements.First;
          WHILE pelem <> nil Do
          Begin
              pelem.SumCurrents(ActorID) ;   // sum terminal currents into system Currents Array
              pelem := CktElements.Next;
          End;
       End;
end;

// =========================================================================================== =
PROCEDURE TSolutionObj.DoControlActions(ActorID : Integer);
VAR
   XHour:Integer;
   XSec :Double;
Begin
    With ActiveCircuit[ActorID] Do
      Begin
          CASE ControlMode of

              CTRLSTATIC:
                 Begin  //  execute the nearest set of control actions but leaves time where it is
                      IF   ControlQueue.IsEmpty
                      THEN ControlActionsDone := TRUE
                      ELSE ControlQueue.DoNearestActions(xHour, XSec, ActorID); // ignore time advancement
                 End;
              EVENTDRIVEN:
                 Begin  //  execute the nearest set of control actions and advance time to that time
                 // **** Need to update this to set the "Intervalhrs" variable for EnergyMeters for Event-Driven Simulation ****
                      IF NOT ControlQueue.DoNearestActions(DynaVars.intHour, DynaVars.t, ActorID) // these arguments are var type
                      THEN ControlActionsDone := TRUE;// Advances time to the next event
                 End;
              TIMEDRIVEN:
                 Begin   // Do all actions having an action time <= specified time
                      IF NOT ControlQueue.DoActions (DynaVars.intHour, DynaVars.t, ActorID)
                      THEN ControlActionsDone := TRUE;
                 End;
              MULTIRATE:
                 Begin  //  execute the nearest set of control actions but leaves time where it is
                      IF NOT ControlQueue.DoMultiRate(DynaVars.intHour, DynaVars.t, ActorID)
                      THEN ControlActionsDone := TRUE;
                 End;
          END;
      End;

End;

// =========================================================================================== =
PROCEDURE TSolutionObj.SampleControlDevices(ActorID : Integer);

Var
    ControlDevice:TControlElem;

Begin
    With ActiveCircuit[ActorID] Do Begin
          ControlDevice := Nil;
          TRY
            // Sample all controls and set action times in control Queue
            ControlDevice := DSSControls.First;
            WHILE ControlDevice <> Nil Do
            Begin
                 IF ControlDevice.Enabled THEN ControlDevice.Sample(ActorID);
                 ControlDevice := DSSControls.Next;
            End;

          EXCEPT
             On E: Exception DO  Begin
             DoSimpleMsg(Format('Error Sampling Control Device "%s.%s" %s  Error = %s',[ControlDevice.ParentClass.Name, ControlDevice.Name, CRLF, E.message]), 484);
             Raise EControlProblem.Create('Solution aborted.');
             End;
          END;
    End;

End;

// =========================================================================================== =
PROCEDURE TSolutionObj.Sample_DoControlActions(ActorID : Integer);



begin

     IF ControlMode = CONTROLSOFF THEN ControlActionsDone := TRUE
     ELSE  Begin

          SampleControlDevices(ActorID);
          DoControlActions(ActorID);

     {This variable lets control devices know the bus list has changed}
         ActiveCircuit[ActorID].Control_BusNameRedefined := False;  // Reset until next change
     End;

end;

PROCEDURE TSolutionObj.Set_Mode(const Value: Integer);


begin

   DynaVars.intHour       := 0;
   DynaVars.t    := 0.0;
   Update_dblHour;
   ActiveCircuit[ActiveActor].TrapezoidalIntegration := FALSE;

   IF Not OK_for_Dynamics(Value)  Then Exit;
   IF Not OK_for_Harmonics(Value) Then Exit;

   Dynavars.SolutionMode := Value;

   ControlMode := DefaultControlMode;   // Revert to default mode
   LoadModel   := DefaultLoadModel;

   IsDynamicModel  := FALSE;
   IsHarmonicModel := FALSE;

   SolutionInitialized := FALSE;   // reinitialize solution when mode set (except dynamics)
   PreserveNodeVoltages := FALSE;  // don't do this unless we have to
   SampleTheMeters := FALSE;
   // Reset defaults for solution modes
   Case Dynavars.SolutionMode of

       PEAKDAY,
       DAILYMODE:     Begin
                           DynaVars.h    := 3600.0;
                           NumberOfTimes := 24;
                           SampleTheMeters := TRUE;
                      End;
       SNAPSHOT:      Begin
                           IntervalHrs   := 1.0;
                           NumberOfTimes := 1;
                      End;
       YEARLYMODE:    Begin
                           IntervalHrs   := 1.0;
                           DynaVars.h    := 3600.0;
                           NumberOfTimes := 8760;
                           SampleTheMeters := TRUE;
                      End;
       DUTYCYCLE:     Begin
                           DynaVars.h  := 1.0;
                           ControlMode := TIMEDRIVEN;
                      End;
       DYNAMICMODE:   Begin
                           DynaVars.h     := 0.001;
                           ControlMode    := TIMEDRIVEN;
                           IsDynamicModel := TRUE;
                           PreserveNodeVoltages := TRUE;  // need to do this in case Y changes during this mode
                      End;
       GENERALTIME:   Begin
                           IntervalHrs   := 1.0;
                           DynaVars.h    := 3600.0;
                           NumberOfTimes := 1;  // just one time step per Solve call expected
                      End;
       MONTECARLO1:   Begin IntervalHrs    := 1.0;      SampleTheMeters := TRUE; End;
       MONTECARLO2:   Begin DynaVars.h     := 3600.0;   SampleTheMeters := TRUE; End;
       MONTECARLO3:   Begin IntervalHrs    := 1.0;      SampleTheMeters := TRUE;  End;
       MONTEFAULT:    Begin IsDynamicModel := TRUE;  END;
       FAULTSTUDY:    Begin
                            IsDynamicModel := TRUE;
                      END;
       LOADDURATION1: Begin
                           DynaVars.h := 3600.0;
                           ActiveCircuit[ActiveActor].TrapezoidalIntegration := TRUE;
                           SampleTheMeters := TRUE;
                      End;
       LOADDURATION2: Begin
                           DynaVars.intHour := 1;
                           ActiveCircuit[ActiveActor].TrapezoidalIntegration := TRUE;
                           SampleTheMeters := TRUE;
                      End;
       AUTOADDFLAG :  Begin
                           IntervalHrs := 1.0;
                           ActiveCircuit[ActiveActor].AutoAddObj.ModeChanged := TRUE;
                      End;
       HARMONICMODE:  Begin
                          ControlMode     := CONTROLSOFF;
                          IsHarmonicModel := TRUE;
                          LoadModel       := ADMITTANCE;
                          PreserveNodeVoltages := TRUE;  // need to do this in case Y changes during this mode
                      End;
       HARMONICMODET: Begin
                          IntervalHrs   := 1.0;
                          DynaVars.h    := 3600.0;
                          NumberOfTimes := 1;
                          ControlMode     := CONTROLSOFF;
                          IsHarmonicModel := TRUE;
                          LoadModel       := ADMITTANCE;
                          PreserveNodeVoltages := TRUE;  // need to do this in case Y changes during this mode
       End;
   End;

   {Moved here 9-8-2007 so that mode is changed before reseting monitors, etc.}
   
   // Reset Meters and Monitors
   MonitorClass[ActiveActor].ResetAll(ActiveActor);
   {by Dahei}FMonitorClass[ActiveActor].ResetAll(ActiveActor);
   EnergyMeterClass[ActiveActor].ResetAll(ActiveActor);
   DoResetFaults;
   DoResetControls;

end;

PROCEDURE TSolutionObj.AddInAuxCurrents(SolveType:Integer; ActorID : Integer);

BEGIN
    {FOR i := 1 to ActiveCircuit[ActiveActor].NumNodes Do Caccum(Currents^[i], AuxCurrents^[i]);}
    // For Now, only AutoAdd Obj uses this

    IF Dynavars.SolutionMode = AUTOADDFLAG THEN ActiveCircuit[ActorID].AutoAddObj.AddCurrents(SolveType, ActorID);

END;

PROCEDURE TSolutionObj.ZeroAuxCurrents(ActorID: Integer);
VAR i:Integer;
BEGIN
//    FOR i := 1 to ActiveCircuit[ActorID].NumNodes Do AuxCurrents^[i] := CZERO;
END;

PROCEDURE TSolutionObj.Check_Fault_Status(ActorID : Integer);

VAR
   pFault:TFaultOBj;

begin
     WITH ActiveCircuit[ActorID]
     Do Begin
       
          pFault := TFaultObj(Faults.First);
          WHILE pFault <> NIL
          Do Begin
             pFault.CheckStatus(ControlMode, ActorID);
             pFault := TFaultObj(Faults.Next);
          End;

     End;  {End With}
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
      With ActiveCircuit[ActiveActor] DO  Begin

         pElem := Generators.First;
         WHILE pElem<>nil Do Begin
             IF pElem.Enabled THEN pElem.InjCurrents; // uses NodeRef to add current into InjCurr Array;
             pElem := Generators.Next;
         End;

       End;

end;
}

FUNCTION TSolutionObj.OK_for_Dynamics(Const Value:Integer): Boolean;

VAR
   ValueIsDynamic :Boolean;

begin

   Result := TRUE;

   CASE Value of
        MONTEFAULT,
        DYNAMICMODE,
        FAULTSTUDY: ValueIsDynamic := TRUE;
   ELSE
       ValueIsDynamic := FALSE;
   END;

   {When we go in and out of Dynamics mode, we have to do some special things}
   If   IsDynamicModel and NOT ValueIsDynamic
   THEN InvalidateAllPCELEMENTS;  // Force Recomp of YPrims when we leave Dynamics mode

   IF NOT IsDynamicModel and ValueIsDynamic
   THEN Begin   // see if conditions right for going into dynamics

       IF ActiveCircuit[ActiveActor].IsSolved
       THEN  CalcInitialMachineStates   // set state variables for machines (loads and generators)
       ELSE Begin
           {Raise Error Message if not solved}
            DoSimpleMsg('Circuit must be solved in a non-dynamic mode before entering Dynamics or Fault study modes!' + CRLF +
                        'If you attempted to solve, then the solution has not yet converged.', 486);
            IF In_ReDirect Then Redirect_Abort := TRUE;  // Get outta here
            Result := FALSE;
       End;
   End;

end;

FUNCTION TSolutionObj.OK_for_Harmonics(const Value:Integer): Boolean;

 {When we go in and out of Harmonics mode, we have to do some special things}
begin

   Result := TRUE;

   If IsHarmonicModel and NOT ((Value=HARMONICMODE)or(Value=HARMONICMODET))
   THEN Begin
       InvalidateAllPCELEMENTS;  // Force Recomp of YPrims when we leave Harmonics mode
       Frequency := ActiveCircuit[ActiveActor].Fundamental;   // Resets everything to norm
   End;

   IF NOT IsHarmonicModel and ((Value=HARMONICMODE)or(Value=HARMONICMODET))
   THEN Begin   // see if conditions right for going into Harmonics

       IF (ActiveCircuit[ActiveActor].IsSolved) and (Frequency = ActiveCircuit[ActiveActor].Fundamental)
       THEN  Begin
           IF Not InitializeForHarmonics(ActiveActor)   // set state variables for machines (loads and generators) and sources
           THEN Begin
                Result := FALSE;
                IF In_ReDirect Then Redirect_Abort := TRUE;  // Get outta here
             End;
         End
       ELSE Begin
         
            DoSimpleMsg('Circuit must be solved in a fundamental frequency power flow or direct mode before entering Harmonics mode!', 487);
            IF In_ReDirect Then Redirect_Abort := TRUE;  // Get outta here
            Result := FALSE;
       End;
   End;

end;

procedure TSolutionObj.Set_Frequency(const Value: Double);
begin
      IF  FFrequency <> Value
      Then Begin
           FrequencyChanged := TRUE;  // Force Rebuild of all Y Primitives
           SystemYChanged := TRUE;  // Force rebuild of System Y
      End;

      FFrequency := Value;
      If ActiveCircuit[ActiveActor] <> Nil Then Harmonic := FFrequency / ActiveCircuit[ActiveActor].Fundamental;  // Make Sure Harmonic stays in synch
end;

procedure TSolutionObj.Increment_time;
{$IFDEF FPC}
var
  next_fncs: fncs_time;
{$ENDIF}
begin
  With Dynavars Do Begin
    t := t+h;
    while t >= 3600.0 do Begin
      Inc(intHour);
      t := t - 3600.0;
    End;
    Update_dblHour;
  {$IFDEF FPC}
    if Assigned (ActiveFNCS) then begin
      if ActiveFNCS.IsReady then begin
        next_fncs := fncs_time (intHour) * fncs_time (3600) + Trunc(t);
//        writeln('Loop time is ' + format('%d', [intHour]) + ':' + format('%d', [Trunc(t)]));
        ActiveFNCS.FncsTimeRequest (next_fncs);
      end;
    end;
  {$ENDIF}
  End;
end;

procedure TSolutionObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := '';

     Inherited InitPropertyValues(NumPropsThisClass);

end;

procedure TSolutionObj.Set_Year(const Value: Integer);
begin
      If DIFilesAreOpen[ActiveActor] Then EnergyMeterClass[ActiveActor].CloseAllDIFiles(ActiveActor);
      FYear := Value;
      DynaVars.intHour := 0;  {Change year, start over}
      Dynavars.t := 0.0;
      Update_dblHour;
      EnergyMeterClass[ActiveActor].ResetAll(ActiveActor);  // force any previous year data to complete
end;

procedure TSolutionObj.Set_Total_Time(const Value: Double);
begin
      Total_Time_Elapsed :=  Value;
end;

procedure TSolutionObj.SaveVoltages;

Var F:TextFile;
    Volts:Complex;
    i,j:integer;
    BusName:String;

begin

  Try

      Try
           AssignFile(F, CircuitName_[ActiveActor] + 'SavedVoltages.Txt');
           Rewrite(F);

           WITH ActiveCircuit[ActiveActor] DO
               FOR i := 1 to NumBuses
               DO Begin
                   BusName := BusList.Get(i);
                   For j := 1 to Buses^[i].NumNodesThisBus
                   DO Begin
                       Volts := NodeV^[Buses^[i].GetRef(j)];
                       Writeln(F, BusName, ', ', Buses^[i].GetNum(j):0, Format(', %-.7g, %-.7g',[Cabs(Volts), CDang(Volts)]));
                   End;
               End;

      Except
            On E:Exception Do Begin
             DoSimpleMsg('Error opening Saved Voltages File: '+E.message, 488);
             Exit;
            End;
      End;


  Finally

     CloseFile(F);
     GlobalResult := CircuitName_[ActiveActor] + 'SavedVoltages.Txt';

  End;

end;

{  *************  MAIN SOLVER CALL  ************************}

FUNCTION TSolutionObj.SolveSystem(V:pNodeVArray; ActorID : Integer): Integer;

Var
  RetCode:Integer;
  iRes: LongWord;
  dRes: Double;

BEGIN

 {Note: NodeV[0] = 0 + j0 always.  Therefore, pass the address of the element 1 of the array.
 }
  Try
    // new function to log KLUSolve.DLL function calls; same information as stepping through in Delphi debugger
    // SetLogFile ('KLU_Log.txt', 1);
    RetCode := SolveSparseSet(hY, @V^[1], @Currents^[1]);  // Solve for present InjCurr
    // new information functions
    GetFlops(hY, @dRes);
    GetRGrowth(hY, @dRes);
    GetRCond(hY, @dRes);
    // GetCondEst (hY, @dRes); // this can be expensive
    GetSize(hY, @iRes);
    GetNNZ(hY, @iRes);
    GetSparseNNZ(hY, @iRes);
    GetSingularCol(hY, @iRes);
  Except
    On E:Exception Do Raise  EEsolv32Problem.Create('Error Solving System Y Matrix.  Sparse matrix solver reports numerical error: '
                   +E.Message);
  End;

   Result := RetCode;

END;

procedure TSolutionObj.Update_dblHour;
begin
     DynaVars.dblHour := DynaVars.intHour + dynavars.t/3600.0;
end;

procedure TSolutionObj.UpdateLoopTime;
begin

// Update Loop time is called from end of time step cleanup
// Timer is based on beginning of SolveSnap time
   {$IFNDEF FPC}
   QueryPerformanceCounter(LoopEndtime);
   {$ELSE}
   LoopEndTime := GetTickCount64;
   {$ENDIF}
   Step_Time_Elapsed  := ((LoopEndtime-SolveStartTime)/CPU_Freq)*1000000;

end;

procedure TSolutionObj.UpdateVBus(ActorID : Integer);

// Save present solution vector values to buses
Var
   i, j:Integer;
Begin
   WITH ActiveCircuit[ActorID] Do
   Begin
    FOR i := 1 to NumBuses Do
     WITH Buses^[i] Do
       If Assigned(Vbus)
       Then FOR j := 1 to NumNodesThisBus Do  VBus^[j] := NodeV^[GetRef(j)];
   End;
End;

procedure TSolutionObj.RestoreNodeVfromVbus;
Var
   i, j:Integer;
Begin
   WITH ActiveCircuit[ActiveActor] Do
    FOR i := 1 to NumBuses Do
     WITH Buses^[i] Do
       If Assigned(Vbus)
       Then FOR j := 1 to NumNodesThisBus Do NodeV^[GetRef(j)]  := VBus^[j];

end;

FUNCTION TSolutionObj.SolveYDirect(ActorID : Integer): Integer;

{ Solves present Y matrix with no injection sources except voltage and current sources }

BEGIN

   Result := 0;

   ZeroInjCurr(ActorID);   // Side Effect: Allocates InjCurr
   GetSourceInjCurrents(ActorID);
   If IsDynamicModel Then GetPCInjCurr(ActorID);  // Need this in dynamics mode to pick up additional injections

   SolveSystem(NodeV, ActorID); // Solve with Zero injection current

END;

PROCEDURE TSolutionObj.Get_Yiibus;
Var
     nNZ   :LongWord;
    i, row, col      :LongWord;
    re, im           :Double;
    ColIdx, RowIdx   :array of LongWord;
    cVals            :array of Complex;
    sf : integer;

Begin
   If hY <= 0 then Begin
     DoSimpleMsg('Y Matrix not Built.', 222);
     Exit;
  End;
  // print lower triangle of G and B using new functions
  // this compresses the entries if necessary - no extra work if already solved
  FactorSparseMatrix (hY);
  GetNNZ (hY, @nNZ);
  //GetSize (temphY, @nBus); // we should already know this

  Try
    SetLength (ColIdx, nNZ);
    SetLength (RowIdx, nNZ);
    SetLength (cVals, nNZ);
    nNZ_Yii := nNZ;//how many lines of Y sparse
    IF (pColIdx_Yii  <> Nil)   Then ReallocMem(pColIdx_Yii,0);
    pColIdx_Yii      := AllocMem(sizeof(pColIdx_Yii^[1]) * nNZ_Yii);
    IF (pRowIdx_Yii  <> Nil)   Then ReallocMem(pRowIdx_Yii,0);
    pRowIdx_Yii      := AllocMem(sizeof(pRowIdx_Yii^[1]) * nNZ_Yii);
    IF (pcVals_Yii  <> Nil)   Then ReallocMem(pcVals_Yii,0);
    pcVals_Yii      := AllocMem(Sizeof(pcVals_Yii^[1]) * nNZ_Yii);

    sf := GetTripletMatrix (hY,  nNZ, @ColIdx[0], @RowIdx[0], @cVals[0]);
    // shows how to easily traverse the triplet format
    for i := 0 to nNZ - 1 do begin
      col := ColIdx[i] + 1;
      row := RowIdx[i] + 1;
      if row = col then begin //diagnal
        re := cVals[i].re;
        im := cVals[i].im;
        NodeYii^[row] := cmplx(re,im);
      end;
      // pColIdx_Yii,  pRowIdx_Yii,  pcVals_Yii
      pColIdx_Yii^[i+1] :=  col;           // begin from 1
      pRowIdx_Yii^[i+1] :=  row;
      pcVals_Yii^[i+1] := cVals[i];
    end;
    NodeYiiEmpty := False;
  Finally

  End;
End;

FUNCTION TSolutionObj.Get_Yij(node_ref_i, node_ref_j : integer) : Complex; // get Gij + j Bij
VAR
  i, col, row: integer;
       //nNZ_yii   :LongWord;       //how many lines in Yii
       //pColIdx_Yii, pRowIdx_Yii   :pLongIntArray;//array of LongWord;  //cols and rows
       //pcVals_Yii                 :pComplexArray;   //vals of yii
begin
      for i := 1 to nNZ_yii do begin
        col := pColIdx_Yii[i] ;
        row := pRowIdx_Yii[i] ;
        if (row=node_ref_i) and (col=node_ref_j) then
            result := pcVals_Yii^[i] ;
      end;
end;

{*******************************************************************************
*             Used to create the OpenDSS Solver thread                         *
********************************************************************************
}

{$IFNDEF FPC}
constructor TSolver.Create(Susp: Boolean; local_CPU: integer; ID : integer; CallBack: TInfoMessageCall; AEvent: TEvent);
var
  Parallel    : TParallel_Lib;
  Thpriority  : String;
begin
  UIEvent                   :=  AEvent;
  FInfoProc                 :=  CallBack;
  FreeOnTerminate           :=  False;
  ActorID                   :=  ID;
  ActorMsg                  :=  TEvent.Create(nil, True, False, '');
  MsgType                   :=  -1;
  ActorActive               :=  True;
  Processing                :=  False;

  Inherited Create(Susp);
  {$IFDEF MSWINDOWS}              // Only for windows
//  Parallel.Set_Process_Priority(GetCurrentProcess(), REALTIME_PRIORITY_CLASS);
  Parallel.Set_Thread_affinity(handle,local_CPU);
//  Set_Priority(THREAD_PRIORITY_TIME_CRITICAL);
  {$ENDIF}
end;
{$ELSE}
constructor TSolver.Create(Susp: Boolean; local_CPU: integer; ID : integer; CallBack: TInfoMessageCall; AEvent: TEvent);
begin
  UIEvent                   :=  AEvent;
  FInfoProc                 :=  CallBack;
  FreeOnTerminate           :=  False;
  ActorID                   :=  ID;
  ActorMsg                  :=  TEvent.Create(nil, True, False, '');
  MsgType                   :=  -1;
  ActorActive               :=  True;
  Processing                :=  False;
  Inherited Create(Susp);
end;
{$ENDIF}

Procedure TSolver.Send_Message(Msg  : Integer);
Begin
  MsgType :=  Msg;
  ActorMsg.SetEvent;
End;

procedure TSolver.Set_Processing(NVal : Boolean);
Begin
  Processing  :=  NVal;
End;

function TSolver.Get_Processing(): Boolean;
Begin
  Result  :=  Processing;
End;

function TSolver.Get_CPU(): Integer;
Begin
  Result  :=  ActorCPU[ActorID];
End;

{$IFNDEF FPC}
procedure TSolver.Set_CPU(CPU : Integer);
var
  Parallel  : TParallel_Lib;
Begin
  ActorCPU[ActorID] :=  CPU;
  {$IFDEF MSWINDOWS}
  Parallel.Set_Thread_affinity(handle,CPU);
//  Parallel.Set_Thread_Priority(handle,THREAD_PRIORITY_TIME_CRITICAL);
  {$ENDIF}
End;
{$ELSE}
procedure TSolver.Set_CPU(CPU : Integer);
Begin
  ActorCPU[ActorID] :=  CPU;
End;
{$ENDIF}

{*******************************************************************************
*             executes the selected solution algorithm                         *
********************************************************************************
}

procedure TSolver.Execute;
var
{$IFNDEF FPC}
  ScriptEd    : TScriptEdit;
{$ENDIF}
  i,
  j,
  idx         : Integer;
  VSourceObj  : TVsourceObj;
  Volts       : Polar;

  begin
    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
    begin
      while ActorActive do
      Begin
            ActorMsg.WaitFor(INFINITE);
            ActorMsg.ResetEvent;
            Processing                  := True;
            case MsgType of             // Evaluates the incomming message
              SIMULATE  :               // Simulates the active ciruit on this actor
                Try
                Begin                   // Checks if this is the coordinator actor in A-Diakoptics mode
                  if ((ADiakoptics and (ActorID = 1)) and not IsSolveAll) then  Solve_Diakoptics()
                  else
                  Begin
                  // Verifies if there is an A-Diakoptics simulation running to update the local Vsources
                    if (ADiakoptics and not IsSolveAll) then Start_Diakoptics();
                  // Normal solution routine
                    Case Dynavars.SolutionMode OF
                        SNAPSHOT       : SolveSnap(ActorID);
                        YEARLYMODE     : SolveYearly(ActorID);
                        DAILYMODE      : SolveDaily(ActorID);
                        DUTYCYCLE      : SolveDuty(ActorID);
                        DYNAMICMODE    : SolveDynamic(ActorID);
                        MONTECARLO1    : SolveMonte1(ActorID);
                        MONTECARLO2    : SolveMonte2(ActorID);
                        MONTECARLO3    : SolveMonte3(ActorID);
                        PEAKDAY        : SolvePeakDay(ActorID);
                        LOADDURATION1  : SolveLD1(ActorID);
                        LOADDURATION2  : SolveLD2(ActorID);
                        DIRECT         : SolveDirect(ActorID);
                        MONTEFAULT     : SolveMonteFault(ActorID);  // Monte Carlo Fault Cases
                        FAULTSTUDY     : SolveFaultStudy(ActorID);
                        AUTOADDFLAG    : ActiveCircuit[ActorID].AutoAddObj.Solve(ActorID);
                        HARMONICMODE   : SolveHarmonic(ActorID);
                        GENERALTIME    : SolveGeneralTime(ActorID);
                        HARMONICMODET  : SolveHarmonicT(ActorID);  //Declares the Hsequential-time harmonics
                    Else
                        DosimpleMsg('Unknown solution mode.', 481);
                        Windows.MessageBox(0,'Unknown solution mode. ErrorCode 481','Error',MB_OK)
                    End;
                  end;
                  {$IFNDEF FPC}
                  QueryPerformanceCounter(GEndTime);
                  {$ELSE}
                  GEndTime := GetTickCount64;
                  {$ENDIF}

                  Total_Solve_Time_Elapsed  :=  ((GEndTime-GStartTime)/CPU_Freq)*1000000;
                  Total_Time_Elapsed        :=  Total_Time_Elapsed + Total_Solve_Time_Elapsed;
                  Processing                :=  False;
                  FMessage                  :=  '1';
                  ActorStatus[ActorID]      :=  1;      // Global to indicate that the actor is ready

                  // If this is an A-Diakoptics actor reports the results to the coordinator (Actor 1)
                  if ADiakoptics and (ActorID <> 1) then Notify_Main;

                  // Sends a message to Actor Object (UI) to notify that the actor has finised
                  UIEvent.SetEvent;
                  if Not ADiakoptics then
                  Begin
                    if Parallel_enabled then
                      if Not IsDLL then queue(CallCallBack); // Refreshes the GUI if running asynchronously
                  End
                  else
                  Begin
                    if (Parallel_enabled and (ActorID = 1)) then
                      if Not IsDLL then queue(CallCallBack); // Refreshes the GUI if running asynchronously
                  End;
                End;
              Except
                On E:Exception Do
                Begin
                  FMessage                  :=  '1';
                  ActorStatus[ActorID]      :=  1;      // Global to indicate that the actor is ready
                  SolutionAbort := TRUE;
                  UIEvent.SetEvent;
                  if Not ADiakoptics then
                  Begin
                    if Parallel_enabled then
                      if Not IsDLL then queue(CallCallBack); // Refreshes the GUI if running asynchronously
                  End
                  else
                  Begin
                    if (Parallel_enabled and (ActorID = 1)) then
                      if Not IsDLL then queue(CallCallBack); // Refreshes the GUI if running asynchronously
                  End;
                if not Parallel_enabled then
                  DoSimpleMsg('Error Encountered in Solve: ' + E.Message, 482);
                End;
              End;
              EXIT_ACTOR:                // Terminates the thread
              Begin
                ActorActive  :=  False;
              End
              else                       // I don't know what the message is
                Windows.MessageBox(0,'Unknown message. ErrorCode 7010','Error',MB_OK)
            End;

      end;
    end;
  end;

procedure TSolver.CallCallBack;
  begin
    if Assigned(FInfoProc) then  FInfoProc(FMessage);
  end;

// Initializes the variables of the A-Diakoptics worker
procedure TSolver.Start_Diakoptics();
var
  row,
  j,
  i           : Integer;
  VSource     : TVsourceObj;
  Volts       : Polar;
  CNum        : Complex;
Begin
  with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
  Begin
    j   :=  ActiveCircuit[1].Ic.NZero - 1;    // Brings the number of Non-Zero elements from Ic
    if j  > 0 then
    Begin
      // Clears the local Ic vector
      for i := 1 to NumNodes do Ic_Local^[i]  :=  cZERO;  // probably not necessary
      // Brings the section of the vector needed for this actor
      for i := 0 to j do
      Begin
        if ActiveCircuit[1].Ic.CData[i].row >= VIndex then
        Begin
          row                 :=  ActiveCircuit[1].Ic.CData[i].row;
          Ic_Local^[row - VIndex + 1]  :=  ActiveCircuit[1].Ic.CData[i].Value;
        End;
      End;
      // Solves to find the total solution
      SolveSparseSet(hY, @Node_dV^[1], @Ic_Local^[1]);
      // Sends the total voltage for this part to the coordinator
      for i := 1 to NumNodes do
      Begin
        CNum                                          :=  csub(NodeV^[i],Node_dV^[i]);
        ActiveCircuit[1].Solution.NodeV^[i + VIndex]  :=  CNum;
      End;

      // Sets the voltage at the feeder head
      VSource       := ActiveVSource[ActorID].ElementList.First;
      for i := 1 to 3 do
      Begin
        CNum              :=  cadd(NodeV^[i],Node_dV^[i]);
        Volts             :=  ctopolardeg(CNum);
        VSource.kVBase    :=  Volts.mag / 1000;   // is in kV
        VSource.Angle     :=  Volts.ang;
        VSource           :=  ActiveVSource[ActorID].ElementList.Next;
      End;
    End;
  End;
End;

procedure TSolver.Notify_Main;
var
  CNum  : Complex;
  i,j,
  idx   : Integer;
Begin
  // Will do something
  With ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
  Begin
    i   :=  NumNodes;
    for idx := 1 to i do
    Begin
      // if it doesn't includes any power injection element (Isource, VSource)
      // returns dV to correct the interconnection equation
      if ActorID > 2 then
        CNum  :=  csub(NodeV^[idx],Node_dV^[idx])
      else
        CNum  :=  NodeV^[idx];
      ActiveCircuit[1].V_0.Insert((idx + VIndex - 1),0,CNum);
    End;
  End;

End;

procedure TSolver.DoTerminate;        // Is the end of the thread
var
  ex: TObject;
begin
    ActorActive               :=  False;
    Processing                :=  False;
    ActorStatus[ActorID]      :=  1;      // Global to indicate that the actor is ready
    UIEvent.SetEvent;
    ActorMsg.Free;
//    Freeandnil(UIEvent);
    inherited;
End;

destructor TSolver.Destroy;
Begin
  inherited destroy;
End;

initialization

    IsMultiThread :=  True;
    {$IFDEF debugtrace}
    Assignfile(Fdebug, 'Debugtrace.csv');
    Rewrite(Fdebug);
    CloseFile(Fdebug);
   {$ENDIF}

End.

