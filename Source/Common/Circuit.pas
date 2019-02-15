unit Circuit;
{
   ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
 Change Log
   10-12-99 Added DuplicatesAllowed and ZonesLocked
   10-24-99 Added Losses Property
   12-15-99 Added Default load shapes and generator dispatch reference
   4-17=00  Add Loads List
   5-30-00  Added Positive Sequence Flag
   8-24-00  Added PriceCurve stuff   Updated 3-6-11
   8-1-01  Modified Compute Capacity to report up to loadmult=1
   9-25-15  Fixed broken repository
}

{$WARN UNIT_PLATFORM OFF}

interface

USES
     Classes, Solution, SysUtils, ArrayDef, HashList, PointerList, CktElement,
     DSSClass, {DSSObject,} Bus, LoadShape, PriceShape, ControlQueue, uComplex,
     AutoAdd, EnergyMeter, NamedObject, CktTree,
     {$IFNDEF FPC}MeTIS_Exec, {$IFDEF MSWINDOWS}Graphics, vcl.dialogs, {$ENDIF} {$ENDIF}
     math, Sparse_Math;

TYPE
    TReductionStrategy = (rsDefault, rsShortlines, {rsTapEnds,} rsMergeParallel, rsBreakLoop, rsDangling, rsSwitches, rsLaterals);

    CktElementDef = RECORD
        CktElementClass:Integer;
        devHandle:Integer;
    END;

    pCktElementDefArray = ^CktElementDefArray;
    CktElementDefArray = Array[1..1] of CktElementDef;


     // for adding markers to Plot
    TBusMarker = class(TObject)
    // Must be defined before calling circuit plot
    private

    public
      BusName: String;
      {$IFNDEF FPC}
        {$IFDEF MSWINDOWS}
        AddMarkerColor: Tcolor;
        {$ELSE}
        AddMarkerColor,
        {$ENDIF}
      {$ELSE}
      AddMarkerColor,
      {$ENDIF}
      AddMarkerCode,
      AddMarkerSize: Integer;

      constructor Create;
      destructor Destroy; override;
    end;

    TDSSCircuit = CLASS(TNamedObject)

      Private
          NodeBuffer          :pIntegerArray;
          NodeBufferMax       :Integer;
          FBusNameRedefined   :Boolean;
          FActiveCktElement   :TDSSCktElement;
          FCaseName           :String;

          // Temp arrays for when the bus swap takes place
          SavedBuses          :pTBusArray;
          SavedBusNames       :pStringArray;
          SavedNumBuses       :Integer;
          FLoadMultiplier     :Double;  // global multiplier for every load

          AbortBusProcess     :Boolean;

          Branch_List         : TCktTree; // topology from the first source, lazy evaluation
          BusAdjPC, BusAdjPD  : TAdjArray; // bus adjacency lists of PD and PC elements

          Procedure AddDeviceHandle(Handle:Integer);
          Procedure AddABus;
          Procedure AddANodeBus;
          Function  AddBus(const BusName:String; NNodes:Integer;ActorID: integer):Integer;
          Procedure Set_ActiveCktElement(Value:TDSSCktElement);
          Procedure Set_BusNameRedefined(Value:Boolean);
          Function Get_Losses(ActorID: Integer):Complex; //Total Circuit losses
          Procedure Set_LoadMultiplier(Value :Double);
          Procedure SaveBusInfo;
          Procedure RestoreBusInfo;

          Function SaveMasterFile:Boolean;
          Function SaveDSSObjects:Boolean;
          Function SaveFeeders:Boolean;
          Function SaveBusCoords:Boolean;
          Function SaveVoltageBases:Boolean;

          procedure ReallocDeviceList(ActorID: integer);
          procedure Set_CaseName(const Value: String);

          function Get_Name:String;




      Public

          ActiveBusIndex :Integer;
          Fundamental    :Double;    // fundamental and default base frequency

          Control_BusNameRedefined  :Boolean;  // Flag for use by control elements to detect redefinition of buses

          BusList,
          AutoAddBusList,
          DeviceList      :THashList;
          DeviceRef       :pCktElementDefArray;  //Type and handle of device

          // lists of pointers to different elements by class
          Faults,
          PDElements,
          PCElements,
          DSSControls,
          Sources,
          MeterElements,
          Sensors,
          Monitors,
          //by dahei
          FMonitors,
          //Generic5OrderMach,
          //
          EnergyMeters,
          Generators,
          StorageElements,
          PVSystems,
          Substations,
          Transformers,
          CapControls,
          RegControls,
          Lines,
          Loads,
          ShuntCapacitors,
          Feeders,
          Reactors,
          Relays,
          Fuses,
          Reclosers,
          SwtControls        :PointerList.TPointerList;
          CktElements        : PointerList.TPointerList;

          ControlQueue:TControlQueue;

          Solution   :TSolutionObj;
          AutoAddObj :TAutoAdd;

          // For AutoAdd stuff
          UEWeight,
          LossWeight   :Double;

          NumUEregs,
          NumLossRegs  :Integer;
          Ueregs,
          LossRegs      :pIntegerArray;

          CapacityStart,
          CapacityIncrement:  Double;
                                   
          TrapezoidalIntegration,   // flag for trapezoidal integratio
          LogEvents  :Boolean;

          LoadDurCurve:String;
          LoadDurCurveObj:TLoadShapeObj;
          PriceCurve:String;
          PriceCurveObj:TPriceShapeObj;

          NumDevices, NumBuses, NumNodes:Integer;
          MaxDevices, MaxBuses, MaxNodes:Integer;
          IncDevices, IncBuses, IncNodes:Integer;

          // Variables for the tearing Algorithm

          Coverage,                     // Used for the user to stablish the coverage for the algorithm
          Actual_Coverage   : Double;   // Indicates the actual coverage of the circuit after running the tearing algorithm
          Longest_paths     : Array of Integer;   //Stores the coordinates of the longest paths in the circuit
          Path_Idx          : Array of integer;   //Stores the indexes from where the areas where formed on the linearized graph
          Buses_Covered     : Array of integer;   //Stores the number of buses (estimated - 1 quadrant) per path
          Path_Size         : Array of Integer;   //Stores the estimated size of each path
          New_Graph         : Array of Integer;   //Stores the latest weighted graph
          Num_SubCkts       : Integer;            // Stores the number of subcircuits for tearing the circuit when executing the "tear_Circuit" command
          Link_Branches     : Array of String;    // Stores the names of the Link branches for Diakoptics
          PConn_Names       : Array of String;    // Stores the names of the buses (bus1) of the link branches
          PConn_Voltages    : Array of Double;    // Stores the voltages at the point of connection of the subcircuits
          Locations         : Array of Integer;   // Stores the indexes of the locations

          // Variables for Diakoptics
          Contours         :  TSparse_Complex;    //  Contours matrix
          ZLL              :  TSparse_Complex;    //  Link branch matrix
          ZCT              :  TSparse_Complex;    //  The transformation matrix (to go from one to other domain)
          ZCC              :  TSparse_Complex;    //  Interconnections matrix
          Y4               :  TSparse_Complex;    //  The inverse of the interconnections matrix
          V_0              :  TSparse_Complex;    //  The voltages of the partial solutions
          Ic               :  TSparse_Complex;    //  The complementary Currents vector
          VIndex           :  Integer;  // To store the index of the sub-circuit in the interconnected system
          VLength          :  Integer;  // To store the length of the sub-circuit in the interconnected system
          AD_Init          :  Boolean;    // This is used only by the A-Diakoptics coordiantor (ID = 1)

          // Bus and Node stuff
          Buses:    pTBusArray;
          MapNodeToBus:pTNodeBusArray;

          // Flags
          Issolved          :Boolean;
          DuplicatesAllowed :Boolean;
          ZonesLocked       :Boolean;
          MeterZonesComputed:Boolean;
          PositiveSequence  :Boolean;  // Model is to be interpreted as Pos seq
          NeglectLoadY      :Boolean;

          // Voltage limits
          NormalMinVolts,
          NormalMaxVolts,
          EmergMaxVolts,
          EmergMinVolts     :Double;  //per unit voltage restraints for this circuit
          LegalVoltageBases :pDoubleArray;

          // Global circuit multipliers
          GeneratorDispatchReference,
          DefaultGrowthFactor,
          DefaultGrowthRate,
          GenMultiplier,   // global multiplier for every generator
          HarmMult   :Double;
          DefaultHourMult: Complex;

          PriceSignal:Double; // price signal for entire circuit

          // EnergyMeter Totals
          RegisterTotals:TRegisterArray;

          DefaultDailyShapeObj,
          DefaultYearlyShapeObj :TLoadShapeObj;

          CurrentDirectory   :String;

          ReductionStrategy:TReductionStrategy;
          {ReductionMaxAngle,} ReductionZmag:double;
          ReduceLateralsKeepLoad:Boolean;
          ReductionStrategyString:String;

          PctNormalFactor:Double;

          {------Plot Marker Circuit Globals---------}
              NodeMarkerCode   :Integer;
              NodeMarkerWidth  :Integer;
              SwitchMarkerCode :Integer;

              TransMarkerSize  :Integer;
              CapMarkerSize    :Integer;
              RegMarkerSize    :Integer;
              PVMarkerSize     :Integer;
              StoreMarkerSize  :Integer;
              FuseMarkerSize   :Integer;
              RecloserMarkerSize   :Integer;
              RelayMarkerSize  :Integer;

              TransMarkerCode  :Integer;
              CapMarkerCode    :Integer;
              RegMarkerCode    :Integer;
              PVMarkerCode     :Integer;
              StoreMarkerCode  :Integer;
              FuseMarkerCode   :Integer;
              RecloserMarkerCode   :Integer;
              RelayMarkerCode   :Integer;

              MarkSwitches     :Boolean;
              MarkTransformers :Boolean;
              MarkCapacitors   :Boolean;
              MarkRegulators   :Boolean;
              MarkPVSystems    :Boolean;
              MarkStorage      :Boolean;
              MarkFuses        :Boolean;
              MarkReclosers    :Boolean;
              MarkRelays       :Boolean;
              NumCircuits      :integer;

              BusMarkerList  :TList;  // list of buses to mark

          {---------------------------------}

          ActiveLoadShapeClass: Integer;


          Constructor Create(const aName:String);
          Destructor Destroy; Override;

          Procedure AddCktElement(Handle:Integer);  // Adds last DSS object created to circuit
          Procedure ClearBusMarkers;

          Procedure TotalizeMeters;
          Function ComputeCapacity(ActorID : Integer): Boolean;

          Function Save(Dir:String): Boolean;

          Procedure ProcessBusDefs(ActorID : Integer);
          Procedure ReProcessBusDefs(ActorID : Integer);
          Procedure DoResetMeterZones(ActorID : Integer);
          Function  SetElementActive(Const FullObjectName:String) : Integer;
          Procedure InvalidateAllPCElements;

          Procedure DebugDump(Var F:TextFile);

          // Access to topology from the first source
          Function GetTopology: TCktTree;
          Procedure FreeTopology;
          Function GetBusAdjacentPDLists(ActorID: integer): TAdjArray;
          Function GetBusAdjacentPCLists(ActorID: integer): TAdjArray;
          function Tear_Circuit(): Integer;                            // Tears the circuit considering the number of Buses of the original Circuit
          procedure Save_SubCircuits();
          function get_Line_Bus(LName: String; NBus: Integer):String;
          procedure  get_longest_path();
          function Append2PathsArray(New_Path :  array of integer): Integer;//  appends a new path to the array and returns the index(1D)
          procedure Normalize_graph();
          procedure Get_paths_4_Coverage();                             // Calculates the paths inside the graph
                                                                        // To guarantee the desired coverage when tearing the system
          procedure Format_SubCircuits(Path  : String; NumCkts  : Integer); // Arrange the files of the subcircuits to make them independent

          property Name             : String         Read Get_Name;
          Property CaseName         : String         Read FCaseName         Write Set_CaseName;
          Property ActiveCktElement : TDSSCktElement Read FActiveCktElement Write Set_ActiveCktElement;
          Property Losses[ActorID: Integer]           : Complex        Read Get_Losses;  // Total Circuit PD Element losses
          Property BusNameRedefined : Boolean        Read FBusNameRedefined Write Set_BusNameRedefined;
          Property LoadMultiplier   : Double         Read FLoadMultiplier   write Set_LoadMultiplier;

      End;

implementation

USES
     PDElement, CktElementClass,
     ParserDel,  DSSClassDefs, DSSGlobals, Dynamics,
     Line, Transformer,  Vsource,
     Utilities, Executive,
     StrUtils,
     {$IFNDEF FPC}
     DSSForms,
     SHELLAPI,
     windows;
     {$ELSE}
     CmdForms;
     {$ENDIF}


//----------------------------------------------------------------------------
Constructor TDSSCircuit.Create(const aName:String);

// Var Retval:Integer;

BEGIN
     inherited Create('Circuit');

     IsSolved := False;
     {*Retval   := *} SolutionClass[ActiveActor].NewObject(Name);
     Solution := ActiveSolutionObj;

     LocalName   := LowerCase(aName);

     CaseName    := aName;  // Default case name to circuitname
                            // Sets CircuitName_

     Fundamental      := DefaultBaseFreq;
     ActiveCktElement := nil;
     ActiveBusIndex   := 1;    // Always a bus

     // initial allocations increased from 100 to 1000 to speed things up

     MaxBuses   := 1000;  // good sized allocation to start
     MaxDevices := 1000;
     MaxNodes   := 3*MaxBuses;
     IncDevices := 1000;
     IncBuses   := 1000;
     IncNodes   := 3000;

     // Allocate some nominal sizes
     BusList        := THashList.Create(900);  // Bus name list Nominal size to start; gets reallocated
     DeviceList     := THashList.Create(900);
     AutoAddBusList := THashList.Create(100);

     NumBuses   := 0;  // Eventually allocate a single source
     NumDevices := 0;
     NumNodes   := 0;

     Faults          := TPointerList.Create(2);
     CktElements     := TPointerList.Create(1000);
     PDElements      := TPointerList.Create(1000);
     PCElements      := TPointerList.Create(1000);
     DSSControls     := TPointerList.Create(10);
     Sources         := TPointerList.Create(10);
     MeterElements   := TPointerList.Create(20);
     Monitors        := TPointerList.Create(20);
     {by Dahei}
     FMonitors        := TPointerList.Create(20);
     {}
     EnergyMeters    := TPointerList.Create(5);
     Sensors         := TPointerList.Create(5);
     Generators      := TPointerList.Create(5);
     StorageElements := TPointerList.Create(5);
     PVSystems       := TPointerList.Create(5);
     Feeders         := TPointerList.Create(10);
     Substations     := TPointerList.Create(5);
     Transformers    := TPointerList.Create(10);
     CapControls     := TPointerList.Create(10);
     SwtControls     := TPointerList.Create(50);
     RegControls     := TPointerList.Create(5);
     Lines           := TPointerList.Create(1000);
     Loads           := TPointerList.Create(1000);
     ShuntCapacitors := TPointerList.Create(20);
     Reactors        := TPointerList.Create(5);
     Reclosers       := TPointerList.Create(10);
     Relays          := TPointerList.Create(10);
     Fuses           := TPointerList.Create(50);

     Buses        := Allocmem(Sizeof(Buses^[1])        * Maxbuses);
     MapNodeToBus := Allocmem(Sizeof(MapNodeToBus^[1]) * MaxNodes);
     DeviceRef    := AllocMem(SizeOf(DeviceRef^[1])    * MaxDevices);

     ControlQueue := TControlQueue.Create;

     LegalVoltageBases := AllocMem(SizeOf(LegalVoltageBases^[1]) * 8);
     // Default Voltage Bases
     LegalVoltageBases^[1] := 0.208;
     LegalVoltageBases^[2] := 0.480;
     LegalVoltageBases^[3] := 12.47;
     LegalVoltageBases^[4] := 24.9;
     LegalVoltageBases^[5] := 34.5;
     LegalVoltageBases^[6] := 115.0;
     LegalVoltageBases^[7] := 230.0;
     LegalVoltageBases^[8] := 0.0;  // terminates array

     ActiveLoadShapeClass := USENONE; // Signify not set

     NodeBufferMax := 50;
     NodeBuffer    := AllocMem(SizeOf(NodeBuffer^[1]) * NodeBufferMax); // A place to hold the nodes

     // Init global circuit load and harmonic source multipliers
     FLoadMultiplier := 1.0;
     GenMultiplier   := 1.0;
     HarmMult        := 1.0;

     PriceSignal     := 25.0;   // $25/MWH

     // Factors for Autoadd stuff
     UEWeight       := 1.0;  // Default to weighting UE same as losses
     LossWeight     := 1.0;
     NumUEregs      := 1;
     NumLossRegs    := 1;
     UEregs         := nil;  // set to something so it wont break reallocmem
     LossRegs       := nil;
     Reallocmem(UEregs, sizeof(UEregs^[1])*NumUEregs);
     Reallocmem(Lossregs, sizeof(Lossregs^[1])*NumLossregs);
     UEregs^[1]     := 10;   // Overload UE
     LossRegs^[1]   := 13;   // Zone Losses

     CapacityStart := 0.9;     // for Capacity search
     CapacityIncrement := 0.005;

     LoadDurCurve    := '';
     LoadDurCurveObj := nil;
     PriceCurve    := '';
     PriceCurveObj := nil;

     // Flags
     DuplicatesAllowed   := False;
     ZonesLocked         := False;   // Meter zones recomputed after each change
     MeterZonesComputed  := False;
     PositiveSequence    := False;
     NeglectLoadY        := False;

     NormalMinVolts := 0.95;
     NormalMaxVolts := 1.05;
     EmergMaxVolts  := 1.08;
     EmergMinVolts  := 0.90;

     NodeMarkerCode := 16;
     NodeMarkerWidth:= 1;
     MarkSwitches     := FALSE;
     MarkTransformers := FALSE;
     MarkCapacitors   := FALSE;
     MarkRegulators   := FALSE;
     MarkPVSystems    := FALSE;
     MarkStorage      := FALSE;
     MarkFuses        := FALSE;
     MarkReclosers    := FALSE;

     SwitchMarkerCode := 5;
     TransMarkerCode  := 35;
     CapMarkerCode    := 38;
     RegMarkerCode    := 17; //47;
     PVMarkerCode     := 15;
     StoreMarkerCode  := 9;
     FuseMarkerCode   := 25;
     RecloserMarkerCode := 17;
     RelayMarkerCode := 17;

     TransMarkerSize  := 1;
     CapMarkerSize    := 3;
     RegMarkerSize    := 5; //1;
     PVMarkerSize     := 1;
     StoreMarkerSize  := 1;
     FuseMarkerSize   := 1;
     RecloserMarkerSize := 5;
     RelayMarkerSize := 5;

     BusMarkerList := TList.Create;
     BusMarkerList.Clear;

     TrapezoidalIntegration := FALSE;  // Default to Euler method
     LogEvents := FALSE;

     GeneratorDispatchReference := 0.0;
     DefaultGrowthRate          := 1.025;
     DefaultGrowthFactor        := 1.0;

     DefaultDailyShapeObj  := LoadShapeClass[ActiveActor].Find('default');
     DefaultYearlyShapeObj := LoadShapeClass[ActiveActor].Find('default');

     CurrentDirectory   := '';

     BusNameRedefined   := True;  // set to force rebuild of buslists, nodelists

     SavedBuses         := nil;
     SavedBusNames      := nil;

     ReductionStrategy  := rsDefault;
//     ReductionMaxAngle := 15.0;
     ReductionZmag      := 0.02;
     NumCircuits        := 0;
     ReduceLateralsKeepLoad := TRUE;

   {Misc objects}
   AutoAddObj           := TAutoAdd.Create;

   Branch_List          := nil;
   BusAdjPC             := nil;
   BusAdjPD             := nil;

   // tearing algorithm vars initialization


  Coverage              :=  0.9;      // 90% coverage expected by default
  Actual_coverage       :=  -1;       //No coverage
  Num_SubCkts           :=  CPU_Cores-1;

  setlength(Longest_paths,0);
  setlength(Path_Idx,0);
  setlength(Buses_Covered,0);
  setlength(Path_size,0);

  // Diakoptics variables
  Contours  :=  TSparse_Complex.Create;
  ZLL       :=  TSparse_Complex.Create;
  ZCC       :=  TSparse_Complex.Create;
  ZCT       :=  TSparse_Complex.Create;
  Y4        :=  TSparse_Complex.Create;
  V_0       :=  TSparse_Complex.Create;
  Ic        :=  TSparse_Complex.Create;

END;

//----------------------------------------------------------------------------
Destructor  TDSSCircuit.Destroy;
VAR
    i:Integer;
    pCktElem :TDSSCktElement;
    ElemName :String;

BEGIN
     For i := 1 to NumDevices Do Begin
           TRY
              pCktElem := TDSSCktElement(CktElements.Get(i));
              ElemName := pCktElem.ParentClass.name + '.' + pCktElem.Name;
              pCktElem.Free;

           EXCEPT
             ON E: Exception Do
               DoSimpleMsg('Exception Freeing Circuit Element:'  + ElemName + CRLF + E.Message, 423);
           END;
     End;

     FOR i := 1 to NumBuses Do Buses^[i].Free;  // added 10-29-00

     Reallocmem(DeviceRef, 0);
     Reallocmem(Buses,     0);
     Reallocmem(MapNodeToBus, 0);
     Reallocmem(NodeBuffer, 0);
     Reallocmem(UEregs, 0);
     Reallocmem(Lossregs, 0);
     Reallocmem(LegalVoltageBases, 0);

     DeviceList.Free;
     BusList.Free;
     AutoAddBusList.Free;
     Solution.Free;
     PDElements.Free;
     PCElements.Free;
     DSSControls.Free;
     Sources.Free;
     Faults.Free;
     CktElements.Free;
     MeterElements.Free;
     Monitors.Free;
     EnergyMeters.Free;
     Sensors.Free;
     Generators.Free;
     StorageElements.Free;
     PVSystems.Free;
     Feeders.Free;
     Substations.Free;
     Transformers.Free;
     CapControls.Free;
     SwtControls.Free;
     RegControls.Free;
     Loads.Free;
     Lines.Free;
     ShuntCapacitors.Free;
     Reactors.Free;
     {by Dahei}
     FMonitors.Free;
     {}
     Reclosers.Free;
     Relays.Free;
     Fuses.Free;

     ControlQueue.Free;

     ClearBusMarkers;
     BusMarkerList.Free;

     AutoAddObj.Free;

     FreeTopology;

//  Release all ADiakoptics matrixes

     Contours.Free;
     ZLL.Free;
     ZCC.Free;
     ZCT.Free;
     Y4.Free;
     V_0.Free;
     Ic.Free;

     Inherited Destroy;
END;

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
*         This routine retuns the index of the element within the array        *
********************************************************************************}
function get_element_Idx(graph_in: array of integer; element: Integer): Integer;
var
  Found,                  // To indicate that the element was found
  End_Flag  : Boolean;
  Graph_size,
  Local_idx : Integer;
begin
  Result      :=  -1;     // In case the element is not in the array
  End_Flag    :=  True;   //  To control the algorithm execution (while based)
  Local_idx   :=  0;
  Found       :=  False;  //  Not found yet
  Graph_size  :=  length(graph_in);
  while (End_Flag) and (Local_idx < Graph_Size) do
  Begin
    if graph_in[Local_idx] = element then 
    Begin
      End_Flag  :=  False;
      Found     :=  True;
    End
    else
    begin
      inc(Local_idx);
    end;
  End;
  if Found then Result  :=  Local_Idx;
end;
{*******************************************************************************
*         This routine calculates the longest path within a linearized         *
*         graph considering the zero level buses as the beginning of           *
*         new path                                                             *
********************************************************************************}
procedure  TDSSCircuit.get_longest_path();
var
  End_flag        : Boolean;    //  Terminates the process
  Current_Idx,                  //  Stores the Index value of the current level   
  Current_level   : Integer;    //  Stores the current level traced
Begin
  with solution do
  Begin
    Current_level   :=  maxintvalue(Inc_Mat_Levels);                    //  Init level
    Current_idx     :=  get_element_idx(Inc_Mat_Levels,Current_level);  //  Init Index
    End_flag        :=  True;
    setlength(New_graph,0);
    while End_flag do
    Begin
      //Checks the termination cirteria
      if (Current_level > Inc_Mat_Levels[Current_idx]) or (Inc_Mat_Levels[Current_idx] = 0)then
      End_Flag  :=  False;
      // Is the current bus part of the new backbone?
      if Inc_Mat_Levels[Current_idx] = Current_level then
      Begin
        dec(Current_level);
        setlength(New_graph,(length(New_graph) + 1));
        New_graph[High(New_graph)]  :=  Current_idx;
      End;
      dec(Current_idx);
    End;
  End;
End;
{*******************************************************************************
*   This routine appends an array to the paths array and returns its index     *
********************************************************************************}
function TDSSCircuit.Append2PathsArray(New_Path :  array of integer): Integer;
var
  local_idx : Integer;
begin
    Result :=  High(Longest_paths) + 1;
    for local_idx := 0 to High(New_path) do
    Begin
      setlength(Longest_paths,(length(Longest_paths) + 1));
      Longest_paths[High(Longest_paths)]  :=  New_Path[Local_idx];
    End;
end;
{*******************************************************************************
*     This routine normalizes the Inc_matrix levels                            *
********************************************************************************}
procedure TDSSCircuit.Normalize_graph();
var
  Curr_level,                                   // To set the active level
  idx           : Integer;                      //
  Ref_detected  : Boolean;                      // To detect if there is a zero
Begin
  Curr_level    := -1;                          // Initializing values
  Ref_detected  :=  False;
  with Solution do
  Begin
    for idx := 0 to High(Inc_Mat_Levels) do     // Sweeps the whole graph
    Begin
      if Inc_Mat_Levels[idx] = 0 then Ref_detected  :=  True
      else
      Begin
        if (Curr_level >= Inc_Mat_Levels[idx]) or Ref_detected then
        Begin
          Ref_detected        :=  False;
          Curr_level          :=  Inc_Mat_Levels[idx] - 1;
          Inc_Mat_Levels[idx] :=  1;
        End
        else  Inc_Mat_Levels[idx] :=  Inc_Mat_Levels[idx] - Curr_level;
      End;
    End;
  End;

End;
{*******************************************************************************
*  Traces the paths (0) in the graph to guarantee the desired coverage         *
********************************************************************************}
procedure TDSSCircuit.Get_paths_4_Coverage();
var
  DBLTemp,                                          //  For storing temoprary doubles
  Sys_Size      : Double;                           //  Stores the number of buses contained in the system
  SMEnd         : Boolean;                          //  Terminates the state machine
  i,
  State         : Integer;                          // The current state of the state machine
  Candidates    : array of integer;                // Array for 0 level buses idx
Begin
  with solution do
  Begin
    SMEnd           :=  True;
    State           :=  0;
    Sys_Size        :=  double(length(Inc_Mat_Cols));
    setlength(Buses_Covered,1);
    setlength(Path_Idx,1);
    Actual_Coverage :=  -1;
    while SMEnd do                                            // The state machine starts
    Begin
      case State of
        0:  Begin                                             // Processes the first path
            setlength(Candidates,0);
            for i := 0 to (length(Inc_Mat_Levels) - 1) do     //Extracts the 0 Level Buses
            Begin
              if solution.Inc_Mat_Levels[i] = 0 then
              Begin
                setlength(Candidates,length(Candidates)+1);
                Candidates[High(Candidates)]  :=  i;
              End;
            End;
            setlength(Longest_paths,0);
            Buses_covered[0]    :=  MaxIntValue(Candidates);       //  Extracts the maximum level covered
            Path_Idx[0]         :=  Append2PathsArray(Candidates); //  No shifting in the graph
            State               :=  1;                             //  Go to the next state
        End;
        1:  Begin                                                  // Extracts a new path from the longest branch to
            get_longest_path();                                    // the backbone (Zeros)
            setlength(Path_Idx,(length(Path_Idx) + 1));
            Path_Idx[High(Path_Idx)]  :=  Append2PathsArray(New_Graph); //  Adds the new candidates
            //  Estimates the amount of buses covered in this path
            setlength(Buses_covered,(length(Buses_covered) + 1));
            Buses_covered[High(Buses_covered)]  :=  New_Graph[0] - New_Graph[High(New_Graph)];
            // Replaces the latest path with 0 in the Bus levels array
            for i := Path_Idx[High(Path_Idx)] to High(Longest_paths) do
              Inc_Mat_Levels[Longest_paths[i]] := 0;
            Normalize_graph;
            // remains in the same state
        End;
      end;
      //  Checks the coverage index to stablish if is necessary to keep tracing paths to increase the coverage
      DBLTemp         :=  0.0;
      for i := Low(Buses_covered) to High(Buses_covered) do
        DBLtemp         :=  DBLTemp + (0.0+Buses_Covered[i]);
      DBLtemp         :=  DBLTemp/Sys_Size;
{      If the New coverage is different from the previous one and is below the expected coverage keep going
       The first criteria is to avoid keep working on a path that will not contribute to improve the coverage}
      if (DBLTemp <> Actual_Coverage) and (DBLTemp >= Coverage) then SMEnd :=  False;
      Actual_Coverage :=  DBLTemp;
    end;
  End;
End;
{*******************************************************************************
* This routine reads the master file of the torn circuit and creates the       *
*  header definitions for declaring separate subcircuits in OpenDSS            *
********************************************************************************}
procedure TDSSCircuit.Format_SubCircuits(Path  : String; NumCkts  : Integer);
var
  myFile      : TextFile;
  Temp_txt,
  Temp_txt2,
  text        : string;
  Xtra,
  File_Struc  : Array of String;
  Str_Found   : Boolean;
  Local_Temp,
  FS_Idx,
  FS_Idx1,
  FS_Idx2     : Integer;
const
  Reference   : array[0..5] of string =                   // To filter the source file
    ('Redirect EnergyM', 'Redirect Monitor', 'MakeBu', 'Redirect BusVolta', 'Buscoords busco', 'Redirect zone');

begin
    // Reads the master file
    AssignFile(myFile, Path  + '\master.dss');
    Reset(myFile);                                        // Prepares for reading
    setlength(File_Struc,0);
    FS_Idx    :=  0;
    while not Eof(myFile) do                              // Extracts the file content as an array of strings
    begin
      setlength(File_Struc,(length(File_Struc) + 1));
      ReadLn(myFile, text);
      File_Struc[FS_Idx]  :=  text;
      inc(FS_Idx);
    end;
    CloseFile(myFile);
    //  Creates the copy for the interconnected system
    setlength(Xtra,0);
    AssignFile(myFile, Path  + '\Master_Interconnected.dss');
    ReWrite(myFile);                                      // Prepares for writing
    for FS_Idx := 0 to High(File_Struc) do
    Begin
      Str_Found   :=  False;
      for FS_Idx1 := 0 to 5 do
      Begin
        Local_Temp  :=  ansipos(Reference[FS_Idx1], File_Struc[FS_Idx]);
        Str_Found   :=  (Local_Temp   <>  0) or Str_Found;
      End;
      if Str_found then
      Begin
        setlength(Xtra,(length(Xtra) + 1));
        Xtra[High(Xtra)]  :=  File_Struc[FS_Idx];
      End
      else  WriteLn(myFile,File_Struc[FS_Idx]);
    End;
    // Adds the zones and the rest to the file
    for FS_Idx := 0 to High(Xtra) do
    Begin
      WriteLn(myFile,Xtra[FS_Idx])
    End;

    CloseFile(myFile);

    // removes the unnecessary information from the master file (deletes the other zones)
    AssignFile(myFile, Path  + '\master.dss');
    ReWrite(myFile);                                      // Prepares for writing
    for FS_Idx := 0 to High(File_Struc) do
    Begin
      Local_Temp  :=  ansipos('Redirect zone', File_Struc[FS_Idx]);
      if Local_Temp   =  0 then
      Begin
        Local_Temp  :=  ansipos('Redirect EnergyM', File_Struc[FS_Idx]);
        if Local_Temp   =  0 then
        Begin
          Local_Temp  :=  ansipos('Redirect Monitor', File_Struc[FS_Idx]);
          if Local_Temp   =  0 then
            WriteLn(myFile,File_Struc[FS_Idx]);
        End;
      End;
    End;
    CloseFile(myFile);
    // Copies the support files to the zones directories
    FS_Idx    :=  0;
    while FS_Idx <> -1 do
    Begin
      Local_Temp  :=  ansipos('Redirect zone', File_Struc[FS_Idx]);
      if Local_Temp   =  0 then
      Begin
        Local_Temp  :=  ansipos('Redirect ', File_Struc[FS_Idx]);
        if Local_temp <> 0 then
        Begin
          text    :=  stringreplace(File_Struc[FS_Idx], 'Redirect ', '',[rfReplaceAll, rfIgnoreCase]);
          {$IFNDEF FPC}
            {$IFDEF MSWINDOWS}
            for FS_Idx1 := 2 to NumCkts do
              CopyFile(PChar(Path + '\' + text), PChar(Path + '\zone_' + inttostr(FS_Idx1) + '\' + text), true);
            {$ENDIF}
          {$ENDIF}
        End;
        inc(FS_Idx);
      End
      else
        FS_Idx  :=  -1;                             // Ends the routine
    End;
    // Creates the master file for each subcircuit
    for FS_Idx := 2 to NumCkts do
    Begin
      AssignFile(myFile, Path  + '\zone_' + inttostr(FS_Idx) + '\master.dss');
      ReWrite(myFile);
      WriteLn(myFile,'Clear');
      WriteLn(myFile,'New Circuit.Zone_' + inttostr(FS_Idx));
      FS_Idx1    :=  2;
      while FS_Idx1 <> -1 do                      // Writes the global files
      Begin
        Local_Temp  :=  ansipos('Redirect zone', File_Struc[FS_Idx1]);
        if Local_Temp   =  0 then
        Begin
          WriteLn(myFile,File_Struc[FS_Idx1]);
          inc(FS_Idx1);
        End
        else
          FS_Idx1   :=  -1;
      End;
      for FS_Idx1 := 0 to High(File_Struc) do   // Writes the zone files
      Begin
        Local_Temp  :=  ansipos('Redirect zone_' + inttostr(FS_Idx), File_Struc[FS_Idx1]);
        if Local_Temp   <>  0 then
        Begin
          text    :=  stringreplace(File_Struc[FS_Idx1], 'zone_' + inttostr(FS_Idx) + '\', '',[rfReplaceAll, rfIgnoreCase]);
          WriteLn(myFile,text);
        End;
      End;
      CloseFile(myFile);
    End;
    // Sets the properties of the VSource on each subcricuit based on the latest voltage measured
    FS_Idx1     :=    0;
    for FS_Idx := 1 to NumCkts do
    Begin
       if FS_Idx = 1 then
         AssignFile(myFile, Path  +  '\VSource.dss')
       else
         AssignFile(myFile, Path  + '\zone_' + inttostr(FS_Idx) + '\VSource.dss');

       ReWrite(myFile);
       for FS_Idx2 := 1 to 3 do
       Begin
         if FS_Idx2 = 1 then
         Begin
           Temp_txt := 'source';
           Temp_txt2 := 'Edit '
         End
         else
         Begin
           Temp_txt  :=  'Vph_' + inttostr(FS_Idx2);
           Temp_txt2 := 'New '
         End;

         text  :=  Temp_txt2 + 'Vsource.' + Temp_txt +
                   ' bus1=' + PConn_Names[FS_Idx - 1] + '.' + inttostr(FS_Idx2) +
                   ' phases=1 pu=1.0' +
                   ' basekv=' + floattostrF(PConn_Voltages[FS_Idx1],ffGeneral, 8, 3) +
                   ' angle=' + floattostrF(PConn_Voltages[FS_Idx1 + 1],ffGeneral, 8, 3) +
                   ' R1=0 X1=0.001 R0=0 X0=0.001';
         WriteLn(myFile,text);
         FS_Idx1    :=  FS_Idx1 + 2;
       End;
       CloseFile(myFile);
    End;
end;

{*******************************************************************************
*        Saves the subcircuits created in memory into the hard drive           *
********************************************************************************
}
procedure TDSSCircuit.Save_SubCircuits();
var
  Fileroot    : String;
Begin
    // Prepares everything to save the base of the torn circuit on a separate folder
    Fileroot              :=  GetCurrentDir;
    Fileroot              :=  Fileroot  + '\Torn_Circuit';
    CreateDir(Fileroot);                        // Creates the folder for storing the modified circuit
    DelFilesFromDir(Fileroot,'*',True);         // Removes all the files inside the new directory (if exists)
    DssExecutive.Command  :=  'save circuit Dir="' + Fileroot + '"';
    // This routine extracts and modifies the file content to separate the subsystems as OpenDSS projects indepedently
    Format_SubCircuits(FileRoot, length(Locations));
End;

{*******************************************************************************
*       Delivers the name of the bus at the specific line and terminal         *
*******************************************************************************}
Function TDSSCircuit.get_Line_Bus(LName: String; NBus: Integer):String;
VAR
    i,
    activesave  : integer;
    pLine       : TLineObj;
    S           : String;
    Found       : Boolean;
    NBuses      : Array of String;
Begin

  setlength(NBuses,2);
  IF ActiveCircuit[ActiveActor] <> NIL THEN
  Begin      // Search list of Lines in active circuit for name
    WITH ActiveCircuit[ActiveActor].Lines DO
    Begin
      S := LName;  // Convert to Pascal String
      Found := FALSE;
      ActiveSave := ActiveIndex;
      pLine := First;
      While pLine <> NIL Do
      Begin
        IF (CompareText(pLine.Name, S) = 0) THEN
        Begin
          ActiveCircuit[ActiveActor].ActiveCktElement := pLine;
          Found := TRUE;
          Break;
        End;
        pLine := Next;
      End;
      IF NOT Found THEN
      Begin
        DoSimpleMsg('Line "'+S+'" Not Found in Active Circuit.', 5008);
        pLine := Get(ActiveSave);    // Restore active Line
        ActiveCircuit[ActiveActor].ActiveCktElement := pLine;
      End;
    End;
    For i := 1 to  ActiveCircuit[ActiveActor].ActiveCktElement.Nterms Do
        NBuses[i-1] := ActiveCircuit[ActiveActor].ActiveCktElement.GetBus(i);
    // returns the name of the desired bus
    Result  :=  NBuses[NBus - 1];
  End;
end;

{*******************************************************************************
*         This routine tears the circuit into many pieces as CPUs are          *
*         available in the local computer (in the best case)                   *
********************************************************************************}
function TDSSCircuit.Tear_Circuit(): Integer;
{$IFDEF FPC}
begin
  DoErrorMsg('Tear_Circuit','MeTIS cannot start.',
             'The MeTIS program is not supported in FPC; TFileSearchReplace is unavailable.', 7006)
end;
{$ELSE}
var
  FileCreated   : Boolean;
  Ftree,
  F             : TextFile;
  TreeNm,                                           // For debugging
  MeTISCmd,
  BusName,
  Terminal,
  TextCmd,
  PDElement,
  FileName      : String;
  {$IFDEF MSWINDOWS}
  SEInfo        : TShellExecuteInfo;                // Shell Info handle
  {$ENDIF}
  NodeIdx,
  Num_Pieces,
  Location_idx,                                     // Active Location
  j,jj,dbg,dbg2,
  i             : Integer;                          // Generic counter variables
  Candidates    : Array of Integer;                 // Array for 0 level buses idx

  EMeter        : TEnergyMeterObj;
  pBus          : TDSSBus;
  Volts         : Polar;
  Term_volts    : Array of Double;                  // To verify the connection of the branch
  MeTISZones    : TStringList;                      // The list for assigning a zone to a bus after tearing
  BusZones      : Array of String;
  Replacer      : TFileSearchReplace;

Begin
  Num_pieces    :=  Num_SubCkts;
  with solution do
  Begin

    // Calculates the incidence matrix and laplacian to generate the graph file to be
    // send to MeTiS
    Calc_Inc_Matrix_Org(ActiveActor);                       //Calculates the ordered incidence matrix
    Laplacian := IncMat.Transpose();                        // Transposes the Incidence Matrix
    Laplacian := Laplacian.multiply(IncMat);                // Laplacian Matrix calculated
    // Generates the graph file
    {******************************************************************************************}
    FileName  := GetOutputDirectory + CircuitName_[ActiveActor] + '.graph';
    Assignfile(F,FileName);
    ReWrite(F);
    Writeln(F,inttostr(length(Inc_Mat_Cols)) + ' ' + inttostr(length(Inc_Mat_Cols) - 1)); // it should be the rank of the incidence matrix
    jj        :=  0;
    for i := 1 to Laplacian.NZero do
    Begin
      if (Laplacian.data[i-1][0] = jj) and (Laplacian.data[i-1][0] <> Laplacian.data[i-1][1]) then
        Write(F,inttostr(Laplacian.data[i-1][1] + 1) + ' ')
      else
      Begin
        if (Laplacian.data[i-1][0] <> jj) then
        Begin
          Writeln(F,'');
          Write(F,inttostr(Laplacian.data[i-1][1] + 1) + ' ');
          inc(jj);
        End;
      End;
    End;
    CloseFile(F);
    {******************************************************************************************}
    if Num_pieces <= 8 then MeTISCmd   :=  'pmetis.exe'  // For less than 8 zones use pMeTIS
    else MeTISCmd   :=  'kmetis.exe';                    // For more than 8 zonez use k-Way (kMeTIS)
    {******************************************************************************************}
    {$IFDEF MSWINDOWS}
    if fileexists(pchar(FileName + '.part.' + inttostr(Num_pieces))) then    // Checks if the file exists before
      deletefile(pchar(FileName + '.part.' + inttostr(Num_pieces)));
    {$ENDIF}
    repeat
      {$IFDEF MSWINDOWS}
      TextCmd  :=  RunMeTIS(DSSDirectory + MeTISCmd + ' ' + FileName + ' ' + inttostr(Num_pieces));  // Executes MeTIS
      {$ENDIF}
      Flag      :=  ContainsText(TextCmd,'I detected an error');
      if Flag then       // The # of edges was wrong, use the one proposed by MeTIS
      Begin
        TextCmd  :=  GetNumEdges(TextCmd);                     // Gest the # of edges proposed by MeTIS
        jj        :=  length(inttostr(length(Inc_Mat_Cols))) + 2;// Caculates the index for replacing the number in the Graph File
        // Replaces the old data with the new at the file header
        Replacer:=TFileSearchReplace.Create(FileName);
        try
          Replacer.Replace(inttostr(length(Inc_Mat_Cols)) + ' ' + inttostr(length(Inc_Mat_Cols) - 1),
                          inttostr(length(Inc_Mat_Cols)) + ' ' + TextCmd, [rfIgnoreCase]);
        finally
          Replacer.Free;
        end;
      End;
    until Not flag;
    {******************************************************************************************}
    // Verifies if there was no error executing MeTIS and the zones file was created
    if (TextCmd <> '**Error**') and fileexists(pchar(FileName + '.part.' + inttostr(Num_pieces))) then
    Begin
      MeTISZones  :=  TStringList.Create;                     // Opens the file containing the tearing results
      MeTISZones.LoadFromFile(FileName + '.part.' + inttostr(Num_pieces));
      setlength(Locations,1);
      setlength(BusZones,1);
      for i := 0 to (MeTISZones.Count - 1) do
      Begin
        if i = 0 then
        Begin
          Locations[i]  :=  0;
          BusZones[i]   :=  MeTISZones[i];
        End
        else
        Begin
          if MeTISZones[i] <> BusZones[high(BusZones)] then   // Moving to another zone in the file
          Begin
            j :=  0;
            if i < (MeTISZones.Count - 1) then                // If not lower means the zone is only 1 bus
              j := Integer(MeTISZones[i] = MeTISZones[i + 1]);
            if j = 1 then                                     // Varifies that the zone is big enough
            Begin
              j   :=  0;                                      // Verifies that this zone hasn't been counted before
              for jj := 0 to High(BusZones) do
              Begin
                if MeTISZones[i] = BusZones[jj] then
                Begin
                  inc(j);
                  Break;
                End;
              End;
              if j = 0 then                                   // Is not in the list, add the new location
              Begin
                setlength(Locations,Length(Locations) + 1);
                setlength(BusZones,Length(BusZones) + 1);
                Locations[High(Locations)]  :=  i;
                BusZones[High(BusZones)]    :=  MeTISZones[i];
              End;
            End;
          End;
        End;
      End;

    //***********The directory is ready for storing the new circuit****************
      EMeter    := EnergyMeters.First;
      while EMeter  <> Nil do
      begin
        EMeter.Enabled  :=  False;
        EMeter          :=  EnergyMeters.Next;
      end;
    //************ Creates the meters at the tearing locations  ********************
      Result        :=  1;                                  // Resets the result variable (Return)
      setlength(PConn_Voltages,length(Locations)*6);        //  Sets the memory space for storing the voltage at the point of conn
      setlength(Link_branches,length(Locations));           //  Sets the memory space for storing the link branches names
      setlength(PConn_Names,length(Locations));             //  Sets the memory space for storing the Bus names
      SolutionAbort := FALSE;
      j             :=  0;
      for i := 0 to High(Locations) do
      begin
        if Locations[i] > 0 then
        Begin
          inc(Result);
          // Gets the name of the PDE for placing the EnergyMeter
           with solution do
           Begin
             PDElement        :=  Inc_Mat_Rows[get_IncMatrix_Row(Locations[i])];
             Link_Branches[i] :=  PDElement;
             dbg              :=  get_IncMatrix_Col(Locations[i]);      // Temporary stores the given location
      // Checks the branch orientation across the feeder by substracting the voltages around the branch
      // Start with Bus 1
             setlength(Term_volts,2);
             for dbg := 0 to 1 do
             Begin
               BusName          :=  Inc_Mat_Cols[Active_Cols[dbg]];
               SetActiveBus(BusName);           // Activates the Bus
               pBus             :=  Buses^[ActiveBusIndex];
               jj               :=  1;
         // this code so nodes come out in order from smallest to larges
               Repeat
                 NodeIdx := pBus.FindIdx(jj);   // Get the index of the Node that matches jj
                 inc(jj)
               Until NodeIdx>0;
               Volts              := ctopolardeg(Solution.NodeV^[pBus.GetRef(NodeIdx)]);  // referenced to pBus
               Term_volts[dbg]     :=  Volts.mag;
             End;

        // Determines the best place to connect the EnergyMeter
             Term_volts[0]      :=  Term_volts[0] - Term_volts[1];
//             if Term_volts[0] >= 0 then jj  :=  0   // Forces to use always the first terminal
//             else   jj  :=  1;                            // removes the line.
             jj               :=  ansipos('.',Link_Branches[i]);
             BusName          :=  get_line_bus(Link_Branches[i].Substring(jj),2);
             jj               :=  ansipos('.',BusName);     // removes the dot
             if jj > 0 then
               BusName          :=  BusName.Substring(0,jj - 1);
             Terminal         :=  'terminal=1';

             PConn_Names[i]   :=  BusName;
             SetActiveBus(BusName);           // Activates the Bus
             pBus             :=  Buses^[ActiveBusIndex];

             for jj := 1 to 3 do
             Begin
               // this code so nodes come out in order from smallest to larges
               NodeIdx := pBus.FindIdx(jj);   // Get the index of the Node that matches jj

               Volts              := ctopolardeg(Solution.NodeV^[pBus.GetRef(NodeIdx)]);  // referenced to pBus
               PConn_Voltages[j]  :=  (Volts.mag/1000);
               inc(j);
               PConn_Voltages[j]  :=  Volts.ang;
               inc(j);
             End;

           End;
          // Generates the OpenDSS Command;
          DssExecutive.Command := 'New EnergyMeter.Zone_' + inttostr(i + 1) + ' element=' + PDElement + ' ' + Terminal + ' option=R action=C';
        End
        else
        Begin
          if Locations[i] = 0 then    // The reference bus (Actor 1)
          Begin
             BusName          :=  Inc_Mat_Cols[0];
             PConn_Names[i]   :=  BusName;
             SetActiveBus(BusName);           // Activates the Bus
             pBus             :=  Buses^[ActiveBusIndex];
             // Stores the voltages for the Reference bus first
             for jj := 1 to 3 do
             Begin
               // this code so nodes come out in order from smallest to larges
               NodeIdx := pBus.FindIdx(jj);   // Get the index of the Node that matches jj

               Volts              := ctopolardeg(Solution.NodeV^[pBus.GetRef(NodeIdx)]);  // referenced to pBus
               PConn_Voltages[j]  :=  (Volts.mag/1000);
               inc(j);
               PConn_Voltages[j]  :=  Volts.ang;
               inc(j);
             End;
          End;
        End;
      end;
    End
    else
    Begin
      if (TextCmd = '**Error**') then
        DoErrorMsg('Tear_Circuit','MeTIS cannot start.',
                   'The MeTIS program (pmetis.exe/kmetis.exe) cannot be executed/found.', 7006)
      else
        DoErrorMsg('Tear_Circuit','The graph file is incorrect.',
                   'MeTIS cannot process the graph file because is incorrect' +
                   '(The number of edges is incorrect).', 7007);
    End;
  End;
End;
{$ENDIF}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
Procedure TDSSCircuit.ProcessBusDefs(ActorID : Integer);
VAR
   BusName:String;
   NNodes, NP,  Ncond, i, j, iTerm, RetVal:Integer;
   NodesOK:Boolean;

BEGIN
   WITH ActiveCktElement DO
     BEGIN
      np    := NPhases;
      Ncond := NConds;

      Parser[ActorID].Token := FirstBus;     // use parser functions to decode
      FOR iTerm := 1 to Nterms DO
        BEGIN
           NodesOK := TRUE;
           // Assume normal phase rotation  for default
           FOR i := 1 to np DO NodeBuffer^[i] := i; // set up buffer with defaults

           // Default all other conductors to a ground connection
           // If user wants them ungrounded, must be specified explicitly!
           For i := np + 1 to NCond DO NodeBuffer^[i] := 0;

           // Parser will override bus connection if any specified
           BusName :=  Parser[ActorID].ParseAsBusName(NNodes, NodeBuffer,ActorID);

           // Check for error in node specification
           For j := 1 to NNodes Do
           Begin
               If NodeBuffer^[j] < 0 Then
               Begin
                   retval := DSSMessageDlg('Error in Node specification for Element: "'
                     +ParentClass.Name+'.'+Name+'"'+CRLF+
                     'Bus Spec: "'+Parser[ActorID].Token+'"',FALSE);
                   NodesOK := FALSE;
                   If  retval=-1 Then Begin
                       AbortBusProcess := TRUE;
                       AppendGlobalresult('Aborted bus process.');
                       Exit
                   End;
                   Break;
               End;
           End;


           // Node -Terminal Connnections
           // Caution: Magic -- AddBus replaces values in nodeBuffer to correspond
           // with global node reference number.
           If NodesOK Then
           Begin
             ActiveTerminalIdx := iTerm;
             ActiveTerminal.BusRef := AddBus(BusName,   Ncond, ActorID);
             SetNodeRef(iTerm, NodeBuffer);  // for active circuit
           End;
           Parser[ActorID].Token := NextBus;
         END;
     END;
END;


//----------------------------------------------------------------------------
Procedure TDSSCircuit.AddABus;
BEGIN
    If NumBuses > MaxBuses THEN BEGIN
        Inc(MaxBuses, IncBuses);
        ReallocMem(Buses, SizeOf(Buses^[1]) * MaxBuses);
    END;
END;

//----------------------------------------------------------------------------
Procedure TDSSCircuit.AddANodeBus;
BEGIN
    If NumNodes > MaxNodes THEN BEGIN
        Inc(MaxNodes, IncNodes);
        ReallocMem(MapNodeToBus, SizeOf(MapNodeToBus^[1]) * MaxNodes);
    END;
END;

//----------------------------------------------------------------------------
Function TDSSCircuit.AddBus(const BusName:String; NNodes:Integer;ActorID: integer):Integer;

VAR
   NodeRef, i :Integer;
BEGIN

// Trap error in bus name
    IF Length(BusName) = 0 THEN BEGIN  // Error in busname
       DoErrorMsg('TDSSCircuit.AddBus', 'BusName for Object "' + ActiveCktElement.Name + '" is null.',
                  'Error in definition of object.', 424);
       For i := 1 to ActiveCktElement.NConds DO NodeBuffer^[i] := 0;
       Result := 0;
       Exit;
    END;

    Result := BusList.Find(BusName);
    If Result=0 THEN BEGIN
         Result := BusList.Add(BusName);    // Result is index of bus
         Inc(NumBuses);
         AddABus;   // Allocates more memory if necessary
         Buses^[NumBuses] := TDSSBus.Create;
    END;

    {Define nodes belonging to the bus}
    {Replace Nodebuffer values with global reference number}
    WITH Buses^[Result] DO BEGIN
      FOR i := 1 to NNodes DO BEGIN
         NodeRef := Add(NodeBuffer^[i],ActorID);
         If NodeRef=NumNodes THEN BEGIN  // This was a new node so Add a NodeToBus element ????
             AddANodeBus;   // Allocates more memory if necessary
             MapNodeToBus^[NumNodes].BusRef  := Result;
             MapNodeToBus^[NumNodes].NodeNum := NodeBuffer^[i]
         END;
         NodeBuffer^[i] := NodeRef;  //  Swap out in preparation to setnoderef call
      END;
    END;
END;

//----------------------------------------------------------------------------
Procedure TDSSCircuit.AddDeviceHandle(Handle:Integer);
BEGIN
    If NumDevices>MaxDevices THEN BEGIN
        MaxDevices := MaxDevices + IncDevices;
        ReallocMem(DeviceRef, Sizeof(DeviceRef^[1]) * MaxDevices);
    END;
    DeviceRef^[NumDevices].devHandle := Handle;    // Index into CktElements
    DeviceRef^[NumDevices].CktElementClass := LastClassReferenced[ActiveActor];
END;


//----------------------------------------------------------------------------
Function TDSSCircuit.SetElementActive(Const FullObjectName:String):Integer;

// Fast way to set a cktelement active
VAR
   Devindex      :Integer;
   DevClassIndex:Integer;
   DevType,
   DevName :String;

BEGIN

     Result := 0;

     ParseObjectClassandName(FullObjectName, DevType, DevName);
     DevClassIndex := ClassNames[ActiveActor].Find(DevType);
     If DevClassIndex = 0 Then DevClassIndex := LastClassReferenced[ActiveActor];
     if DevName <> '' then
     begin
       Devindex := DeviceList.Find(DevName);
       WHILE DevIndex>0 DO BEGIN
           IF DeviceRef^[Devindex].CktElementClass=DevClassIndex THEN   // we got a match
            BEGIN
              ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(DevClassIndex);
              LastClassReferenced[ActiveActor] := DevClassIndex;
              Result := DeviceRef^[Devindex].devHandle;
             // ActiveDSSClass[ActiveActor].Active := Result;
            //  ActiveCktElement := ActiveDSSClass.GetActiveObj;
              ActiveCktElement := CktElements.Get(Result);
              Break;
            END;
           Devindex := Devicelist.FindNext;   // Could be duplicates
       END;
     end;

     CmdResult := Result;

END;

//----------------------------------------------------------------------------
Procedure TDSSCircuit.Set_ActiveCktElement(Value:TDSSCktElement);
BEGIN
    FActiveCktElement := Value;
    ActiveDSSObject[ActiveActor] := Value;
END;

//----------------------------------------------------------------------------
Procedure TDSSCircuit.AddCktElement(Handle:Integer);


BEGIN

   // Update lists that keep track of individual circuit elements
   Inc(NumDevices);

   // Resize DeviceList if no. of devices greatly exceeds allocation
   If Cardinal(NumDevices)> 2*DeviceList.InitialAllocation Then ReAllocDeviceList(ActiveActor);
   DeviceList.Add(ActiveCktElement.Name);
   CktElements.Add(ActiveCktElement);

   {Build Lists of PC and PD elements}
   CASE (ActiveCktElement.DSSObjType and BaseClassMask) OF
       PD_ELEMENT:   PDElements.Add(ActiveCktElement);
       PC_ELEMENT:   PCElements.Add(ActiveCktElement);
       CTRL_ELEMENT: DSSControls.Add(ActiveCktElement);
       METER_ELEMENT :MeterElements.Add(ActiveCktElement);
   Else
       {Nothing}
   End;

   {Build  lists of Special elements and generic types}
   CASE (ActiveCktElement.DSSObjType and CLASSMASK) OF
       MON_ELEMENT      :Monitors.Add(ActiveCktElement);
       ENERGY_METER     :EnergyMeters.Add(ActiveCktElement);
       SENSOR_ELEMENT   :Sensors.Add(ActiveCktElement);
       GEN_ELEMENT      :Generators.Add(ActiveCktElement);
       SOURCE           :Sources.Add(ActiveCktElement);
       CAP_CONTROL      :CapControls.Add(ActiveCktElement);
       SWT_CONTROL      :SwtControls.Add(ActiveCktElement);
       REG_CONTROL      :RegControls.Add(ActiveCktElement);
       LOAD_ELEMENT     :Loads.Add(ActiveCktElement);
       CAP_ELEMENT      :ShuntCapacitors.Add(ActiveCktElement);
       REACTOR_ELEMENT  :Reactors.Add(ActiveCktElement);
       RELAY_CONTROL    :Relays.Add(ActiveCktElement);
       FUSE_CONTROL     :Fuses.Add(ActiveCktElement);
       RECLOSER_CONTROL :Reclosers.Add(ActiveCktElement);
       FMON_ELEMENT     :FMonitors.Add(ActiveCktElement);

       { Keep Lines, Transformer, and Lines and Faults in PDElements and separate lists
         so we can find them quickly.}
       XFMR_ELEMENT   :Transformers.Add(ActiveCktElement);
       LINE_ELEMENT   :Lines.Add(ActiveCktElement);
       FAULTOBJECT    :Faults.Add(ActiveCktElement);
       FEEDER_ELEMENT :Feeders.Add(ActiveCktElement);

       STORAGE_ELEMENT:StorageElements.Add(ActiveCktElement);
       PVSYSTEM_ELEMENT:PVSystems.Add(ActiveCktElement);
   END;

  // AddDeviceHandle(Handle); // Keep Track of this device result is handle
  AddDeviceHandle(CktElements.ListSize); // Handle is global index into CktElements
  ActiveCktElement.Handle := CktElements.ListSize;

END;

//----------------------------------------------------------------------------
Procedure TDSSCircuit.DoResetMeterZones(ActorID : Integer);

BEGIN

 { Do this only if meterzones unlocked .  Normally, Zones will remain unlocked
   so that all changes to the circuit will result in rebuilding the lists}
  If Not MeterZonesComputed or Not ZonesLocked Then
  Begin
     If LogEvents Then LogThisEvent('Resetting Meter Zones',ActorID);
     EnergyMeterClass[ActorID].ResetMeterZonesAll(ActorID);
     MeterZonesComputed := True;
     If LogEvents Then LogThisEvent('Done Resetting Meter Zones',ActorID);
  End;

  FreeTopology;

END;

//----------------------------------------------------------------------------
Procedure TDSSCircuit.SaveBusInfo;
Var
   i  :Integer;

Begin

{Save existing bus definitions and names for info that needs to be restored}
     SavedBuses    := Allocmem(Sizeof(SavedBuses^[1]) * NumBuses);
     SavedBusNames := Allocmem(Sizeof(SavedBusNames^[1]) * NumBuses);

     For i := 1 to NumBuses Do Begin
         SavedBuses^[i] := Buses^[i];
         SavedBusNames^[i] := BusList.get(i);
     End;
     SavedNumBuses := NumBuses;

End;

//----------------------------------------------------------------------------
Procedure TDSSCircuit.RestoreBusInfo;

Var
   i,j,idx, jdx:Integer;
   pBus:TDSSBus;

Begin

// Restore  kV bases, other values to buses still in the list
     For i := 1 to SavedNumBuses Do
       Begin
           idx := BusList.Find(SavedBusNames^[i]);
           If   idx <> 0 Then
             With Buses^[idx] Do Begin
               pBus    := SavedBuses^[i];
               kvBase  := pBus.kVBase;
               x       := pBus.x;
               Y       := pBus.y;
               CoordDefined := pBus.CoordDefined;
               Keep    := pBus.Keep;
               {Restore Voltages in new bus def that existed in old bus def}
               If assigned(pBus.VBus) Then Begin
                   For j := 1 to pBus.NumNodesThisBus Do Begin
                       jdx := FindIdx(pBus.GetNum(j));  // Find index in new bus for j-th node  in old bus
                       If jdx > 0 Then Vbus^[jdx] := pBus.VBus^[j];
                   End;
               End;
             End;
           SavedBusNames^[i] := ''; // De-allocate string
       End;

     If Assigned(SavedBuses) Then For i := 1 to SavedNumBuses Do SavedBuses^[i].Free;  // gets rid of old bus voltages, too

     ReallocMem(SavedBuses, 0);
     ReallocMem(SavedBusNames, 0);

End;

//----------------------------------------------------------------------------
Procedure TDSSCircuit.ReProcessBusDefs(ActorID : Integer);

// Redo all Buslists, nodelists

VAR
    CktElementSave :TDSSCktElement;
    i:integer;

BEGIN
     If LogEvents Then LogThisEvent('Reprocessing Bus Definitions',ActorID);

     AbortBusProcess := FALSE;
     SaveBusInfo;  // So we don't have to keep re-doing this
     // Keeps present definitions of bus objects until new ones created

     // get rid of old bus lists
     BusList.Free;  // Clears hash list of Bus names for adding more
     BusList := THashList.Create(NumDevices);  // won't have many more buses than this

     NumBuses := 0;  // Leave allocations same, but start count over
     NumNodes := 0;

     // Now redo all enabled circuit elements
     CktElementSave := ActiveCktElement;
     ActiveCktElement := CktElements.First;
     WHILE ActiveCktElement <> nil DO  BEGIN
       IF ActiveCktElement.Enabled THEN ProcessBusDefs(ActorID);
       IF AbortBusProcess then Exit;
       ActiveCktElement := CktElements.Next;
     END;

     ActiveCktElement := CktElementSave;  // restore active circuit element

     FOR i := 1 to NumBuses Do Buses^[i].AllocateBusVoltages;
     FOR i := 1 to NumBuses Do Buses^[i].AllocateBusCurrents;

     RestoreBusInfo;     // frees old bus info, too
     DoResetMeterZones(ActorID);  // Fix up meter zones to correspond

     BusNameRedefined := False;  // Get ready for next time
END;

//----------------------------------------------------------------------------
Procedure TDSSCircuit.Set_BusNameRedefined(Value:Boolean);
BEGIN
    FBusNameRedefined := Value;

    IF Value THEN Begin
      Solution.SystemYChanged := True;  // Force Rebuilding of SystemY if bus def has changed
      Control_BusNameRedefined := True;  // So controls will know buses redefined
    End;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TDSSCircuit.Get_Losses(ActorID:Integer):Complex;

Var
   pdelem :TPDElement;
Begin

{Return total losses in all PD Elements}

        pdelem := PDElements.First;
        Result := cZERO;
        While pdelem <> nil Do Begin
            IF pdelem.enabled Then Begin
              {Ignore Shunt Elements}
              If Not pdElem.IsShunt Then Caccum(Result, pdelem.losses[ActorID]);
            End;
            pdelem := PDElements.Next;
        End;

End;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TDSSCircuit.DebugDump(Var F:TextFile);

VAR
   i,j:Integer;

BEGIN

     Writeln(F, 'NumBuses= ', NumBuses:0);
     Writeln(F, 'NumNodes= ', NumNodes:0);
     Writeln(F, 'NumDevices= ', NumDevices:0);
     Writeln(F,'BusList:');
     For i := 1 to NumBuses Do BEGIN
       Write(F,'  ',Pad(BusList.Get(i),12));
       Write(F,' (', Buses^[i].NumNodesThisBus:0,' Nodes)');
       FOR j := 1 to Buses^[i].NumNodesThisBus Do Write(F,' ',Buses^[i].Getnum(j):0);
       Writeln(F);
     END;
     Writeln(F,'DeviceList:');
     For i := 1 to NumDevices Do BEGIN
        Write(F,'  ',Pad(DeviceList.Get(i),12));
        ActiveCktElement := CktElements.Get(i);
        If Not ActiveCktElement.Enabled THEN Write(F, '  DISABLED');
        Writeln(F);
     END ;
     Writeln(F,'NodeToBus Array:');
     For i := 1 to NumNodes DO BEGIN
       j :=  MapNodeToBus^[i].BusRef;
       Write(F,'  ',i:2,' ',j:2,' (=',BusList.Get(j),'.',MapNodeToBus^[i].NodeNum:0,')');
       Writeln(F);
     END;



END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TDSSCircuit.InvalidateAllPCElements;

VAR
   p:TDSSCktElement;

BEGIN

   p := PCElements.First;
   WHILE (p <> nil)
   DO BEGIN
        p.YprimInvalid[ActiveActor] := True;
        p := PCElements.Next;
   END;

   Solution.SystemYChanged := True;  // Force rebuild of matrix on next solution

END;


// - - ------------------------------------------------------
Procedure TDSSCircuit.Set_LoadMultiplier(Value :Double);

Begin

     If (Value <> FLoadMultiplier)
     Then   // We may have to change the Y matrix if the load multiplier  has changed
         Case Solution.LoadModel Of
              ADMITTANCE:  InvalidateAllPCElements
         Else
            {nada}
         End;

     FLoadMultiplier := Value;

End;

procedure TDSSCircuit.TotalizeMeters;

{ Totalize all energymeters in the problem}

Var
    pEM:TEnergyMeterObj;
    i: Integer;

begin
      For i := 1 to NumEMRegisters Do RegisterTotals[i] := 0.;

      pEM := EnergyMeters.First;
      While pEM <> Nil Do With PEM Do Begin

          For i := 1 to NumEMRegisters Do RegisterTotals[i] := RegisterTotals[i] + Registers[i] * TotalsMask[i];

         pEM := EnergyMeters.Next;
      End;
end;

FUNCTION TDSSCircuit.ComputeCapacity(ActorID : Integer): Boolean;
Var
   CapacityFound :Boolean;

    FUNCTION SumSelectedRegisters(Const mtrRegisters:TRegisterArray; Regs: pIntegerArray;  count: Integer): Double;
    VAR
       i  :Integer;
    begin
         Result := 0.0;
         FOR i := 1 to count Do Begin
              Result := Result + mtrRegisters[regs^[i]];
         End;
    end;

begin
     Result := FALSE;
     If (EnergyMeters.ListSize = 0) Then Begin
       DoSimpleMsg('Cannot compute system capacity with EnergyMeter objects!', 430);
       Exit;
     End;

     If (NumUeRegs = 0) Then Begin
       DoSimpleMsg('Cannot compute system capacity with no UE resisters defined.  Use SET UEREGS=(...) command.', 431);
       Exit;
     End;

     Solution.Mode := SNAPSHOT;
     LoadMultiplier  := CapacityStart;
     CapacityFound := False;

     Repeat
          EnergyMeterClass[ActorID].ResetAll(ActorID);
          Solution.Solve(ActorID);
          EnergyMeterClass[ActorID].SampleAll(ActorID);
          TotalizeMeters;

           // Check for non-zero in UEregs
           IF SumSelectedRegisters(RegisterTotals, UEregs, NumUEregs) <> 0.0 Then CapacityFound := True;
           // LoadMultiplier is a property ...
           IF Not CapacityFound Then LoadMultiplier := LoadMultiplier + CapacityIncrement;
     Until (LoadMultiplier > 1.0) or CapacityFound;
     If LoadMultiplier>1.0 Then LoadMultiplier := 1.0;
     Result := TRUE;
end;

Function TDSSCircuit.Save(Dir:String):Boolean;
{Save the present circuit - Enabled devices only}

var
   i:Integer;
   Success:Boolean;
   CurrDir,SaveDir :String;

begin
   Result := FALSE;

// Make a new subfolder in the present folder based on the circuit name and
// a unique sequence number
   SaveDir :=  GetCurrentDir;  // remember where to come back to
   Success := FALSE;
   If Length(Dir)=0 Then Begin
     dir := Name;

     CurrDir := Dir;
     For i := 0 to 999 Do  // Find a unique dir name
      Begin
         If Not DirectoryExists(CurrDir) Then
          Begin
              If CreateDir(CurrDir) Then
               Begin
                  SetCurrentDir(CurrDir);
                  Success := TRUE;
                  Break;
               End;
          End;
         CurrDir :=  dir + Format('%.3d',[i]);
      End;
    End Else Begin
       If Not DirectoryExists(Dir) Then Begin
          CurrDir :=  dir;
          If CreateDir(CurrDir) Then
           Begin
              SetCurrentDir(CurrDir);
              Success := TRUE;
           End;
       End Else Begin  // Exists - overwrite
          CurrDir := Dir;
          SetCurrentDir(CurrDir);
          Success := TRUE;
       End;
    End;

    If Not Success Then
     Begin
       DoSimpleMsg('Could not create a folder "'+Dir+'" for saving the circuit.', 432);
       Exit;
     End;

    SavedFileList[ActiveActor].Clear;  {This list keeps track of all files saved}

    // Initialize so we will know when we have saved the circuit elements
    For i := 1 to CktElements.ListSize Do TDSSCktElement(CktElements.Get(i)).HasBeenSaved := False;

    // Initialize so we don't save a class twice
    For i := 1 to DSSClassList[ActiveActor].ListSize Do TDssClass(DSSClassList[ActiveActor].Get(i)).Saved := FALSE;

    {Ignore Feeder Class -- gets saved with Energymeters}
   // FeederClass.Saved := TRUE;  // will think this class is already saved

    {Define voltage sources first}
    Success :=  WriteVsourceClassFile(GetDSSClassPtr('vsource'), TRUE);
    {Write library files so that they will be available to lines, loads, etc}
    {Use default filename=classname}
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('wiredata'),'', FALSE);
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('cndata'),'', FALSE);
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('tsdata'),'', FALSE);
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('linegeometry'),'', FALSE);
    // If Success Then Success :=  WriteClassFile(GetDssClassPtr('linecode'),'', FALSE);
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('linespacing'),'', FALSE);
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('linecode'),'', FALSE);
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('xfmrcode'),'', FALSE);
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('loadshape'),'', FALSE);
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('TShape'),'', FALSE);
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('priceshape'),'', FALSE);
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('growthshape'),'', FALSE);
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('XYcurve'),'', FALSE);
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('TCC_Curve'),'', FALSE);
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('Spectrum'),'', FALSE);
    If Success Then Success := SaveFeeders; // Save feeders first
    If Success Then Success := SaveDSSObjects;  // Save rest ot the objects
    If Success Then Success := SaveVoltageBases;
    If Success Then Success := SaveBusCoords;
    If Success Then Success := SaveMasterFile;



    If Success Then DoSimpleMsg('Circuit saved in directory: ' + GetCurrentDir, 433)
               Else DoSimpleMsg('Error attempting to save circuit in ' + GetCurrentDir, 434);
    // Return to Original directory
    SetCurrentDir(SaveDir);

    Result := TRUE;

end;

function TDSSCircuit.SaveDSSObjects: Boolean;
Var

   Dss_Class:TDSSClass;
   i:integer;

begin
  Result := FALSE;

  // Write Files for all populated DSS Classes  Except Solution Class
  For i := 1 to DSSClassList[ActiveActor].ListSize Do
   Begin
      Dss_Class := DSSClassList[ActiveActor].Get(i);
      If (DSS_Class = SolutionClass[ActiveActor]) or Dss_Class.Saved Then Continue;   // Cycle to next
            {use default filename=classname}
      IF Not WriteClassFile(Dss_Class,'', (DSS_Class is TCktElementClass) ) Then Exit;  // bail on error
      DSS_Class.Saved := TRUE;
   End;

  Result := TRUE;

end;

function TDSSCircuit.SaveVoltageBases: Boolean;
Var  F:TextFile;
     i:integer;
     VBases:string;
Begin

     Result := FALSE;
     Try
        AssignFile(F, 'BusVoltageBases.DSS');
        Rewrite(F);

//        For i := 1 to NumBuses do
//          If Buses^[i].kVBase > 0.0 Then
//            Writeln(F, Format('SetkVBase Bus=%s  kvln=%.7g ', [BusList.Get(i), Buses^[i].kVBase]));
        DSSExecutive.Command := 'get voltagebases';
        VBases := GlobalResult;
        Writeln(F, 'Set Voltagebases='+VBases);
        Writeln(F, 'CalcVoltagebases');
        CloseFile(F);
        Result := TRUE;
     Except
      On E:Exception Do DoSimpleMsg('Error Saving BusVoltageBases File: '+E.Message, 43501);
     End;

End;

function TDSSCircuit.SaveMasterFile: Boolean;

Var
   F:TextFile;
   i:integer;

begin
  Result := FALSE;
  Try
      AssignFile(F, 'Master.DSS');
      Rewrite(F);

      Writeln(F, 'Clear');
      Writeln(F,'New Circuit.' + Name);
      Writeln(F);
      If PositiveSequence Then Writeln(F, 'Set Cktmodel=Positive');
      If DuplicatesAllowed Then Writeln(F, 'set allowdup=yes');
      Writeln(F);

      // Write Redirect for all populated DSS Classes  Except Solution Class
      For i := 1 to SavedFileList[ActiveActor].Count  Do
       Begin
          Writeln(F, 'Redirect ', SavedFileList[ActiveActor].Strings[i-1]);
       End;

      Writeln(F,'MakeBusList');
      Writeln(F,'Redirect BusVoltageBases.dss  ! set voltage bases');

      If FileExists('buscoords.dss') Then
      Begin
         Writeln(F, 'Buscoords buscoords.dss');
      End;

      CloseFile(F);
      Result := TRUE;
  Except
      On E:Exception Do DoSimpleMsg('Error Saving Master File: '+E.Message, 435);
  End;

end;

function TDSSCircuit.SaveFeeders: Boolean;
Var
   i:Integer;
   SaveDir, CurrDir:String;
   Meter:TEnergyMeterObj;
begin

   Result := TRUE;
{Write out all energy meter  zones to separate subdirectories}
   SaveDir := GetCurrentDir;
   For i := 1 to EnergyMeters.ListSize Do
    Begin
        Meter := EnergyMeters.Get(i); // Recast pointer
        CurrDir :=  Meter.Name;
        If DirectoryExists(CurrDir) Then
         Begin
            SetCurrentDir(CurrDir);
            Meter.SaveZone(CurrDir);
            SetCurrentDir(SaveDir);
         End
        Else Begin
             If CreateDir(CurrDir) Then
             Begin
                SetCurrentDir(CurrDir);
                Meter.SaveZone(CurrDir);
                SetCurrentDir(SaveDir);
             End
             Else Begin
                DoSimpleMsg('Cannot create directory: '+CurrDir, 436);
                Result := FALSE;
                SetCurrentDir(SaveDir);  // back to whence we came
                Break;
             End;
        End;
    End;  {For}
    
end;

function TDSSCircuit.SaveBusCoords: Boolean;
Var
        F:TextFile;
        i:Integer;
begin

   Result := FALSE;

   Try
       AssignFile(F, 'BusCoords.dss');
       Rewrite(F);



       For i := 1 to NumBuses Do
       Begin
           If Buses^[i].CoordDefined then Writeln(F, CheckForBlanks(BusList.Get(i)), Format(', %-g, %-g', [Buses^[i].X, Buses^[i].Y]));
       End;

       Closefile(F);

       Result := TRUE;

   Except
       On E:Exception Do DoSimpleMsg('Error creating Buscoords.dss.', 437);
   End;

end;

procedure TDSSCircuit.ReallocDeviceList(ActorID: integer);

Var
    TempList:THashList;
    i:Integer;

begin
{Reallocate the device list to improve the performance of searches}
    If LogEvents Then LogThisEvent('Reallocating Device List',ActorID);
    TempList := THashList.Create(2*NumDevices);

    For i := 1 to DeviceList.ListSize Do
    Begin
        Templist.Add(DeviceList.Get(i));
    End;

    DeviceList.Free; // Throw away the old one.
    Devicelist := TempList;

end;

procedure TDSSCircuit.Set_CaseName(const Value: String);
begin
  FCaseName := Value;
  CircuitName_[ActiveActor] := Value + '_';
end;

function TDSSCircuit.Get_Name:String;
begin
   Result:=LocalName;
end;

Function TDSSCircuit.GetBusAdjacentPDLists(ActorID: integer): TAdjArray;
begin
  if not Assigned (BusAdjPD) then BuildActiveBusAdjacencyLists (BusAdjPD, BusAdjPC,ActorID);
  Result := BusAdjPD;
end;

Function TDSSCircuit.GetBusAdjacentPCLists(ActorID: integer): TAdjArray;
begin
  if not Assigned (BusAdjPC) then BuildActiveBusAdjacencyLists (BusAdjPD, BusAdjPC,ActorID);
  Result := BusAdjPC;
end;

Function TDSSCircuit.GetTopology: TCktTree;
var
  i: Integer;
  elem: TDSSCktElement;
begin
  if Not assigned(Branch_List) then begin
    {Initialize all Circuit Elements and Buses to not checked, then build a new tree}
    elem := CktElements.First;
    WHILE assigned (elem) Do Begin
      elem.Checked := False;
      For i := 1 to elem.Nterms Do elem.Terminals^[i].Checked := FALSE;
      elem.IsIsolated := TRUE; // till proven otherwise
      elem := CktElements.Next;
    End;
    FOR i := 1 to NumBuses Do Buses^[i].BusChecked := FALSE;
    Branch_List := GetIsolatedSubArea (Sources.First, TRUE);  // calls back to build adjacency lists
  end;
  Result := Branch_List;
end;

Procedure TDSSCircuit.FreeTopology;
begin
  if Assigned (Branch_List) then Branch_List.Free;
  Branch_List := nil;
  if Assigned (BusAdjPC) then FreeAndNilBusAdjacencyLists (BusAdjPD, BusAdjPC);
end;

PROCEDURE TDSSCircuit.ClearBusMarkers;
Var
    i:Integer;
Begin
    For i := 0 to BusMarkerList.count-1 do TBusMarker(BusMarkerList.Items[i]).Free;
    BusMarkerList.Clear;
End ;

{====================================================================}
{ TBusMarker }
{====================================================================}

constructor TBusMarker.Create;
begin
  inherited;
  BusName := '';
  AddMarkerColor := {$IFNDEF FPC}{$IFDEF MSWINDOWS}clBlack{$ELSE}0{$ENDIF}{$ELSE}0{$ENDIF};
  AddMarkerCode := 4;
  AddMarkerSize := 1;
end;

destructor TBusMarker.Destroy;
begin
  BusName := '';
  inherited;
end;



end.
