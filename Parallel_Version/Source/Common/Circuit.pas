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
     AutoAdd, EnergyMeter, NamedObject, CktTree, Graphics, math;


TYPE
    TReductionStrategy = (rsDefault, rsStubs, rsTapEnds, rsMergeParallel, rsBreakLoop, rsDangling, rsSwitches);

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
      AddMarkerColor: Tcolor;
      AddMarkerCode,
      AddMarkerSize: Integer;

      constructor Create;
      destructor Destroy; override;
    end;

    TDSSCircuit = CLASS(TNamedObject)

      Private
          NodeBuffer        :pIntegerArray;
          NodeBufferMax     :Integer;
          FBusNameRedefined :Boolean;
          FActiveCktElement :TDSSCktElement;
          FCaseName         :String;

          // Temp arrays for when the bus swap takes place
          SavedBuses    :pTBusArray;
          SavedBusNames :pStringArray;
          SavedNumBuses :Integer;
          FLoadMultiplier :Double;  // global multiplier for every load

          AbortBusProcess :Boolean;

          Branch_List: TCktTree; // topology from the first source, lazy evaluation
          BusAdjPC, BusAdjPD: TAdjArray; // bus adjacency lists of PD and PC elements


          Procedure AddDeviceHandle(Handle:Integer);
          Procedure AddABus;
          Procedure AddANodeBus;
          Function  AddBus(const BusName:String; NNodes:Integer;ActorID: integer):Integer;
          Procedure Set_ActiveCktElement(Value:TDSSCktElement);
          Procedure Set_BusNameRedefined(Value:Boolean);
          Function Get_Losses:Complex; //Total Circuit losses
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
          Longest_paths     : Array of Array of Integer;   //Stores the coordinates of the longest paths in the circuit
          Path_Idx          : Array of integer;   //Stores the indexes from where the areas where formed on the linearized graph
          Buses_Covered     : Array of integer;   //Stores the number of buses (estimated - 1 quadrant) per path
          Path_Size         : Array of Integer;   //Stores the estimated size of each path
          New_Graph         : Array of Integer;   //Stores the latest weighted graph

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
          ReductionMaxAngle, ReductionZmag:double;
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
          procedure  get_longest_path(graph_in: array of integer);          

          property Name             : String         Read Get_Name;
          Property CaseName         : String         Read FCaseName         Write Set_CaseName;
          Property ActiveCktElement : TDSSCktElement Read FActiveCktElement Write Set_ActiveCktElement;
          Property Losses           : Complex        Read Get_Losses;  // Total Circuit PD Element losses
          Property BusNameRedefined : Boolean        Read FBusNameRedefined Write Set_BusNameRedefined;
          Property LoadMultiplier   : Double         Read FLoadMultiplier   write Set_LoadMultiplier;

      End;

implementation

USES
     PDElement, CktElementClass,
     ParserDel,  DSSClassDefs, DSSGlobals, Dynamics,
     Line, Transformer,  Vsource,
     Utilities,  DSSForms, Executive, SHELLAPI;

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

     CurrentDirectory := '';

     BusNameRedefined := True;  // set to force rebuild of buslists, nodelists

     SavedBuses    := nil;
     SavedBusNames := nil;

     ReductionStrategy := rsDefault;
     ReductionMaxAngle := 15.0;
     ReductionZmag     := 0.02;
     NumCircuits       := 0;

   {Misc objects}
   AutoAddObj := TAutoAdd.Create;

   Branch_List := nil;
   BusAdjPC    := nil;
   BusAdjPD    := nil;

   // tearing algorithm vars initialization


  Coverage        :=  0.7;      // 70% coverage expected by default
  Actual_coverage :=  -1;       //No coverage
  setlength(Longest_paths,0);
  setlength(Path_Idx,0);
  setlength(Buses_Covered,0);
  setlength(Path_size,0);

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

     ControlQueue.Free;

     ClearBusMarkers;
     BusMarkerList.Free;

     AutoAddObj.Free;

     FreeTopology;

     Inherited Destroy;
END;

{*******************************************************************************
*           Routine created to empty a recently created folder                 *
********************************************************************************}
procedure DelFilesFromDir(Directory, FileMask: string; DelSubDirs: Boolean);
var
  SourceLst: string;
  FOS: TSHFileOpStruct;
begin
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
  SHFileOperation(FOS);
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
procedure  TDSSCircuit.get_longest_path(graph_in: array of integer);
var
  End_flag        : Boolean;    //  Terminates the process
  Current_Idx,                  //  Stores the Index value of the current level   
  Current_level   : Integer;    //  Stores the current level traced
Begin
  Current_level   :=  maxintvalue(graph_in);                    //  Init level
  Current_idx     :=  get_element_idx(graph_in,Current_level);  //  Init Index
  End_flag        :=  True;
  setlength(New_graph,0);
  while End_flag do
  Begin  
    //Checks the termination cirteria
    if (Current_level > graph_in[Current_idx]) or (graph_in[Current_idx] = 0)then
    End_Flag  :=  False;
    // Is the current bus part of the new backbone?
    if graph_in[Current_idx] = Current_level then  
    Begin
      dec(Current_level);
      setlength(New_graph,(length(New_graph) + 1));
      New_graph[High(New_graph)]  :=  Current_idx;
    End;
    dec(Current_idx);
  End;
End;

{*******************************************************************************
*         This routine tears the circuit into many pieces as CPUs are          *
*         available in the local computer (in the best case)                   *
********************************************************************************}
function TDSSCircuit.Tear_Circuit(): Integer;
var
  Num_Pieces,                                       // Max Number of pieces considering the number of local CPUs
  Num_buses,                                        // Goal for number of buses on each subsystem
  Num_target,                                       // Generic accumulator
  Location_idx,                                     // Active Location
  i,                                                // Generic counter variables
  State         : Integer;                          // The current state of the state machine
  Candidates,                                       // Array for 0 level buses idx
  Active_graph,                                     // Is the active linearized graph  
  Locations     : Array of Integer;                 // Array for the best tearing locations
  Locations_V   : Array of Double;                  // Array to store the VMag and angle at the location
  TreeNm,                                           // For debugging
  Fileroot,
  PDElement     : String;
  Ftree         : TextFile;
  flag,                                             //  Stop flag
  SMEnd         : Boolean;                          //  Terminates the state machine
  EMeter        : TEnergyMeterObj;
  DBLTemp,                                          //  For storing temoprary doubles
  Sys_Size      : Double;                           //  Stores the number of buses contained in the system 

Begin
  Num_Pieces  :=  CPU_Cores-1;
  with solution do
  Begin
    Calc_Inc_Matrix_Org(ActiveActor);                         //Calculates the ordered incidence matrix
    SMEnd           :=  True;
    State           :=  0;
    Num_buses       :=  length(Inc_Mat_Cols) div Num_Pieces;  // Estimates the number of buses for each subsystem
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
            setlength(Longest_paths,1);
            setlength(Active_graph,length(Inc_Mat_Levels));
            for i := Low(Active_graph) to High(Active_graph) do
              Active_graph[i]        :=  Inc_Mat_Levels[i];
//            Longest_paths[0]    :=  Candidates;                 //  Adds the initial path proposed by the Ckt-Tree
            Buses_covered[0]    :=  MaxIntValue(Candidates);    //  Extracts the maximum level covered
            Path_Idx[0]         :=  0;                          //  No shifting in the graph
            State               :=  1;                          //  Go to the next state
        End;
        1:  Begin                                               // Extracts a new path from the longest branch
            get_longest_path(Active_graph);
            setlength(Longest_paths,(length(Longest_paths) + 1));
//            Longest_paths[High(Longest_paths)]  :=  Candidates; // Adds the new candidates to the results
//            Candidates  :=  Candidates - Candidates[High(Candidates)];
//            for i := 0 to High(Candidates) do
//              Candidates[Idx] :=
            
        End;
      end;
      //  Checks the coverage index to stablish if is necessary to keep tracing paths to increase the coverage
      DBLTemp         :=  0;
      for i := Low(Buses_covered) to High(Buses_covered) do
        DBLtemp         :=  DBLTemp + double(Buses_Covered[i]);
      DBLtemp         :=  DBLTemp/Sys_Size;
{      If the New coverage is different from the previous one and is below the expected coverage keep going
       The first criteria is to avoid keep working on a path that will not contribute to improve the coverage}
      if (DBLTemp <> Actual_Coverage) and (DBLTemp < Coverage) then SMEnd :=  False;
      Actual_Coverage :=  DBLTemp;
    end;








    setlength(Locations,Num_pieces-1);                // Setups the array for the tearing locations

    for i := 0 to (length(Locations) - 1) do Locations[i] :=  -1; // Initializes the locations array
    Num_target    :=  Num_buses;
    Location_idx  :=  0;

    for i := 0 to (length(Candidates) - 1) do
    Begin
      if (Candidates[i] >= Num_target) and (Location_idx < (Num_pieces-1)) then
      begin
        Num_target :=  Num_target + Num_buses;
        if Candidates[i] > Num_target then
          Locations[Location_idx] :=  Candidates[i-1]
        else
          Locations[Location_idx] :=  Candidates[i];
        inc(Location_idx);
      end;
    End;


  //***********The directory is ready for storing the new circuit****************
      EMeter    := EnergyMeters.First;
      while EMeter  <> Nil do
      begin
        EMeter.Enabled  :=  False;
        EMeter          :=  EnergyMeters.Next;
      end;
  //************ Creates the meters at the tearing locations  ********************
    Result  :=  1;                              // Resets the result variable (Return)
    setlength(Locations_V,length(Locations)*2);
    SolutionAbort := FALSE;
    for i := 0 to (length(Locations)-1) do
    begin
      if Locations[i] >= 0 then
      Begin
        inc(Result);
        // Gets the name of the PDE for placing the EnergyMeter
         with solution do
         Begin
           PDElement :=  Inc_Mat_Rows[get_IncMatrix_Row(Locations[i])];
           SetActiveBus(Inc_Mat_Cols[get_IncMatrix_Col(Locations[i])]);  // Activates the Bus
         End;
        // Generates the OpenDSS Command;
        DssExecutive.Command := 'New EnergyMeter.Zone_' + inttostr(i + 1) + ' element=' + PDElement + ' term=1 option=R action=C';
      End;
    end;
    DssExecutive.Command := 'set mode=snap';
    Solve(ActiveActor);

    Fileroot  :=  GetCurrentDir;
    Fileroot  :=  Fileroot  + '\Torn_Circuit';
    CreateDir(Fileroot);                        // Creates the folder for storing the modified circuit
    DelFilesFromDir(Fileroot,'*',True);         // Removes all the files inside the new directory (if exists)

    DssExecutive.Command :=  'save circuit Dir="' + Fileroot + '"';
  End;
End;
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
Function TDSSCircuit.Get_Losses:Complex;

Var
   pdelem :TPDElement;
Begin

{Return total losses in all PD Elements}

        pdelem := PDElements.First;
        Result := cZERO;
        While pdelem <> nil Do Begin
            IF pdelem.enabled Then Begin
              {Ignore Shunt Elements}
              If Not pdElem.IsShunt Then Caccum(Result, pdelem.losses);
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
  AddMarkerColor := clBlack;
  AddMarkerCode := 4;
  AddMarkerSize := 1;
end;

destructor TBusMarker.Destroy;
begin
  BusName := '';
  inherited;
end;



end.




