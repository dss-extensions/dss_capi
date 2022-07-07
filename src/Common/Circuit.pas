unit Circuit;

{
   ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Classes,
    Solution,
    SysUtils,
    ArrayDef,
    HashList,
    DSSPointerList,
    CktElement,
    DSSClass,
    Bus,
    LoadShape,
    PriceShape,
    ControlQueue,
    UComplex, DSSUcomplex,
    AutoAdd,
    EnergyMeter,
    NamedObject,
    CktTree,
    MeTIS_Exec,
    Monitor,
    PCClass,
    PDClass,
    math,
    Sparse_Math,
    Process
     {$IFDEF DSS_CAPI_PM}
    ,
    syncobjs
     {$ENDIF}
    ;

type
    TReductionStrategy = (rsDefault, rsShortlines, {rsTapEnds,} rsMergeParallel, rsBreakLoop, rsDangling, rsSwitches, rsLaterals);

    // for adding markers to Plot
    TBusMarker = class(TObject)
    // Must be defined before calling circuit plot
    PRIVATE

    PUBLIC
        BusName: String;
        AddMarkerColor: Integer;
        AddMarkerCode,
        AddMarkerSize: Integer;

        constructor Create;
        destructor Destroy; OVERRIDE;
    end;

    TDSSCircuit = class(TNamedObject)
    PRIVATE
        NodeBuffer: pIntegerArray;
        NodeBufferMax: Integer;
        FBusNameRedefined: Boolean;
        FActiveCktElement: TDSSCktElement;
        FCaseName: String;

        // Temp arrays for when the bus swap takes place
        SavedBuses: pTBusArray;
        SavedBusNames: pStringArray;
        SavedNumBuses: Integer;
        FLoadMultiplier: Double;  // global multiplier for every load

        AbortBusProcess: Boolean;

        Branch_List: TCktTree; // topology from the first source, lazy evaluation
        BusAdjPC, BusAdjPD: TAdjArray; // bus adjacency lists of PD and PC elements

        procedure AddABus;
        procedure AddANodeBus;
        function AddBus(const BusName: String; NNodes: Integer): Integer;
        procedure Set_ActiveCktElement(Value: TDSSCktElement);
        procedure Set_BusNameRedefined(Value: Boolean);
        function Get_Losses: Complex; //Total Circuit losses
        procedure Set_LoadMultiplier(Value: Double);
        procedure SaveBusInfo;
        procedure RestoreBusInfo;

        function SaveMasterFile: Boolean;
        function SaveDSSObjects: Boolean;
        function SaveFeeders: Boolean;
        function SaveBusCoords: Boolean;
        function SaveVoltageBases: Boolean;

        procedure ReallocDeviceList;
        procedure Set_CaseName(const Value: String);

        function Get_Name: String;

    PUBLIC
        DSS: TDSSContext;

        ActiveBusIndex: Integer;
        Fundamental: Double;    // fundamental and default base frequency

        Control_BusNameRedefined: Boolean;  // Flag for use by control elements to detect redefinition of buses

        BusList,
        AutoAddBusList: TBusHashListType;
        DeviceList: THashList;

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
        AutoTransformers,
        CapControls,
        RegControls,
        Lines,
        Loads,
        ShuntCapacitors,
        Reactors, // added for CIM XML export
        Relays, // added for CIM XML export
        Fuses, // added for CIM XML export
        Reclosers, // added for CIM XML export
        InvControls,
        ExpControls,
        SwtControls: TDSSPointerList;
        CktElements: TDSSPointerList;
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
        IncrCktElements: TDSSPointerList;
{$ENDIF}
{$IFDEF DSS_CAPI_PM}
        LockIc: TCriticalSection;
{$ENDIF}

        ControlQueue: TControlQueue;

        Solution: TSolutionObj;
        AutoAddObj: TAutoAdd;

        // For AutoAdd stuff
        UEWeight,
        LossWeight: Double;

        NumUEregs,
        NumLossRegs: Integer;
        Ueregs,
        LossRegs: pIntegerArray;

        CapacityStart,
        CapacityIncrement: Double;

        TrapezoidalIntegration,
        LogEvents: Boolean;

        LoadDurCurve: String;
        LoadDurCurveObj: TLoadShapeObj;
        PriceCurve: String;
        PriceCurveObj: TPriceShapeObj;

        NumDevices, NumBuses, NumNodes: Integer;
        MaxDevices, MaxBuses, MaxNodes: Integer;
        IncDevices, IncBuses, IncNodes: Integer;

        // Bus and Node stuff
        Buses: pTBusArray;
        MapNodeToBus: pTNodeBusArray;

        // Flags
        Issolved: Boolean;
        DuplicatesAllowed: Boolean;
        ZonesLocked: Boolean;
        MeterZonesComputed: Boolean;
        PositiveSequence: Boolean;  // Model is to be interpreted as Pos seq
        NeglectLoadY: Boolean;

        // Voltage limits
        NormalMinVolts,
        NormalMaxVolts,
        EmergMaxVolts,
        EmergMinVolts: Double;  //per unit voltage restraints for this circuit
        LegalVoltageBases: pDoubleArray;

        // Global circuit multipliers
        GeneratorDispatchReference,
        DefaultGrowthFactor,
        DefaultGrowthRate,
        GenMultiplier,   // global multiplier for every generator
        HarmMult: Double;
        DefaultHourMult: Complex;

        PriceSignal: Double; // price signal for entire circuit

        // EnergyMeter Totals
        RegisterTotals: TRegisterArray;

        DefaultDailyShapeObj,
        DefaultYearlyShapeObj: TLoadShapeObj;

        CurrentDirectory: String;

        ReductionStrategy: TReductionStrategy;
        ReductionZmag: Double;
        ReduceLateralsKeepLoad: Boolean;
        ReductionStrategyString: String;

        PctNormalFactor: Double;

        // ------Plot Marker Circuit Globals---------
        NodeMarkerCode: Integer;
        NodeMarkerWidth: Integer;
        SwitchMarkerCode: Integer;

        TransMarkerSize: Integer;
        CapMarkerSize: Integer;
        RegMarkerSize: Integer;
        PVMarkerSize: Integer;
        StoreMarkerSize: Integer;
        FuseMarkerSize: Integer;
        RecloserMarkerSize: Integer;
        RelayMarkerSize: Integer;

        TransMarkerCode: Integer;
        CapMarkerCode: Integer;
        RegMarkerCode: Integer;
        PVMarkerCode: Integer;
        StoreMarkerCode: Integer;
        FuseMarkerCode: Integer;
        RecloserMarkerCode: Integer;
        RelayMarkerCode: Integer;

        MarkSwitches: Boolean;
        MarkTransformers: Boolean;
        MarkCapacitors: Boolean;
        MarkRegulators: Boolean;
        MarkPVSystems: Boolean;
        MarkStorage: Boolean;
        MarkFuses: Boolean;
        MarkReclosers: Boolean;
        MarkRelays: Boolean;

        BusMarkerList: TList;  // list of buses to mark
        // ---------------------------------

        ActiveLoadShapeClass: Integer;


        // Variables for the tearing Algorithm
{$IFDEF DSS_CAPI_ADIAKOPTICS}
        VIndex: Integer; // To store the index of the sub-circuit in the interconnected system
        VLength: Integer; // To store the length of the sub-circuit in the interconnected system
        AD_Init: Boolean; // This is used only by the A-Diakoptics coordiantor (ID = 1)

        Coverage,                     // Used for the user to stablish the coverage for the algorithm
        Actual_Coverage: Double;   // Indicates the actual coverage of the circuit after running the tearing algorithm
        Longest_paths: array of Integer;   //Stores the coordinates of the longest paths in the circuit
        Path_Idx: array of Integer;   //Stores the indexes from where the areas where formed on the linearized graph
        Buses_Covered: array of Integer;   //Stores the number of buses (estimated - 1 quadrant) per path
        Path_Size: array of Integer;   //Stores the estimated size of each path
        New_Graph: array of Integer;   //Stores the latest weighted graph
        Num_SubCkts: Integer;            // Stores the number of subcircuits for tearing the circuit when executing the "tear_Circuit" command
        Link_Branches: array of String;    // Stores the names of the Link branches for Diakoptics
        PConn_Names: array of String;    // Stores the names of the buses (bus1) of the link branches
        PConn_Voltages: array of Double;    // Stores the voltages at the point of connection of the subcircuits
        Locations: array of Integer;   // Stores the indexes of the locations
        BusZones: array of String;

        // Variables for Diakoptics
        //TODO: migrate TSparse_Complex to KLUSolveX (most functionality already present in Eigen)
        ContoursT: TSparse_Complex; //  Contours matrix transposed
        Contours: TSparse_Complex; //  Contours matrix
        ZLL: TSparse_Complex; //  Link branch matrix
        ZCT: TSparse_Complex; //  The transformation matrix (to go from one to other domain)
        ZCC: TSparse_Complex; //  Interconnections matrix
        Y4: TSparse_Complex; //  The inverse of the interconnections matrix
        // V_0: TSparse_Complex; //  The voltages of the partial solutions
        Ic: TSparse_Complex; //  The complementary Currents vector
        MeTISZones: TStringList; // The list for assigning a zone to a bus after tearing

        procedure AggregateProfiles(mode: String);
        procedure Disable_All_DER();
        function Tear_Circuit(): Integer; // Tears the circuit considering the number of Buses of the original Circuit
        function Create_MeTIS_graph(): String; // Generates the graph dscribing the model for MeTiS
        function Create_MeTIS_Zones(Filename: String): String; // Executes MeTiS and loads the zones into memory for further use
        procedure Save_SubCircuits(AddISrc: Boolean);
                                                                                        // To guarantee the desired coverage when tearing the system
        procedure Format_SubCircuits(Path: String; NumCkts: Integer; AddISrc: Boolean); // Arrange the files of the subcircuits to make them independent
        procedure AppendIsources(Path: String; BusNum: Integer; LinkBranch: String);

        procedure get_longest_path();
        function Append2PathsArray(New_Path: array of Integer): Integer;//  appends a new path to the array and returns the index(1D)
        procedure Normalize_graph();
        procedure Get_paths_4_Coverage(); // Calculates the paths inside the graph
{$ENDIF}

        constructor Create(dssContext: TDSSContext; const aName: String);
        destructor Destroy; OVERRIDE;

        procedure AddCktElement(Obj: TDSSCktElement);
        procedure ClearBusMarkers;

        procedure TotalizeMeters;
        function ComputeCapacity: Boolean;

        function Save(Dir: String): Boolean;

        procedure ProcessBusDefs;
        procedure ReProcessBusDefs;
        procedure DoResetMeterZones;
        function SetElementActive(const FullObjectName: String): Integer;
        procedure InvalidateAllPCElements;

        procedure DebugDump(var F: TFileStream);

          // Access to topology from the first source
        function GetTopology: TCktTree;
        procedure FreeTopology;
        function GetBusAdjacentPDLists: TAdjArray;
        function GetBusAdjacentPCLists: TAdjArray;
        function getPCEatBus(BusName: String; useNone: Boolean = TRUE): ArrayOfString;
        function getPDEatBus(BusName: String; useNone: Boolean = TRUE): ArrayOfString;
        function ReportPCEatBus(BusName: String): String;
        function ReportPDEatBus(BusName: String): String;

        property Name: String READ Get_Name;
        property CaseName: String READ FCaseName WRITE Set_CaseName;
        property ActiveCktElement: TDSSCktElement READ FActiveCktElement WRITE Set_ActiveCktElement;
        property Losses: Complex READ Get_Losses;  // Total Circuit PD Element losses
        property BusNameRedefined: Boolean READ FBusNameRedefined WRITE Set_BusNameRedefined;
        property LoadMultiplier: Double READ FLoadMultiplier WRITE Set_LoadMultiplier;
    end;

implementation

uses
    BufStream,
    PDElement,
    CktElementClass,
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Dynamics,
    Line,
    Transformer,
    Vsource,
    Utilities,
    CmdForms,
     {$IFDEF MSWINDOWS}
    Windows,
    SHELLAPI,
{$ELSE}
    BaseUnix,
    Unix,
{$ENDIF}
    Executive,
    StrUtils,
    Load,
    PVSystem,
    DSSHelper;

constructor TDSSCircuit.Create(dssContext: TDSSContext; const aName: String);
begin
    inherited Create('Circuit');

    DSS := dssContext;

    IsSolved := FALSE;
    Solution := TSolutionObj.Create(DSS, Name);

    LocalName := AnsiLowerCase(aName);

    CaseName := aName;  // Default case name to circuitname
                        // Sets CircuitName_

    Fundamental := DSS.DefaultBaseFreq;
    ActiveCktElement := NIL;
    ActiveBusIndex := 1;    // Always a bus

     // initial allocations increased from 100 to 1000 to speed things up

    MaxBuses := 1000;  // good sized allocation to start
    MaxDevices := 1000;
    MaxNodes := 3 * MaxBuses;
    IncDevices := 1000;
    IncBuses := 1000;
    IncNodes := 3000;

     // Allocate some nominal sizes
    BusList := TBusHashListType.Create(900);  // Bus name list Nominal size to start; gets reallocated
    DeviceList := THashList.Create(900);
    AutoAddBusList := TBusHashListType.Create(100);

    NumBuses := 0;  // Eventually allocate a single source
    NumDevices := 0;
    NumNodes := 0;

    Faults := TDSSPointerList.Create(2);
    CktElements := TDSSPointerList.Create(1000);
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
    IncrCktElements := TDSSPointerList.Create(1000);
{$ENDIF}
    PDElements := TDSSPointerList.Create(1000);
    PCElements := TDSSPointerList.Create(1000);
    DSSControls := TDSSPointerList.Create(10);
    Sources := TDSSPointerList.Create(10);
    MeterElements := TDSSPointerList.Create(20);
    Monitors := TDSSPointerList.Create(20);
    EnergyMeters := TDSSPointerList.Create(5);
    Sensors := TDSSPointerList.Create(5);
    Generators := TDSSPointerList.Create(5);
    StorageElements := TDSSPointerList.Create(5);
    PVSystems := TDSSPointerList.Create(5);
    InvControls := TDSSPointerList.Create(5);
    ExpControls := TDSSPointerList.Create(5);
    Substations := TDSSPointerList.Create(5);
    Transformers := TDSSPointerList.Create(10);
    AutoTransformers := TDSSPointerList.Create(10);
    CapControls := TDSSPointerList.Create(10);
    SwtControls := TDSSPointerList.Create(50);
    RegControls := TDSSPointerList.Create(5);
    Lines := TDSSPointerList.Create(1000);
    Loads := TDSSPointerList.Create(1000);
    ShuntCapacitors := TDSSPointerList.Create(20);
    Reactors := TDSSPointerList.Create(5);
    Reclosers := TDSSPointerList.Create(10);
    Relays := TDSSPointerList.Create(10);
    Fuses := TDSSPointerList.Create(50);

    Buses := Allocmem(Sizeof(Buses^[1]) * Maxbuses);
    MapNodeToBus := Allocmem(Sizeof(MapNodeToBus^[1]) * MaxNodes);

    ControlQueue := TControlQueue.Create(DSS);

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
    NodeBuffer := AllocMem(SizeOf(NodeBuffer^[1]) * NodeBufferMax); // A place to hold the nodes

     // Init global circuit load and harmonic source multipliers
    FLoadMultiplier := 1.0;
    GenMultiplier := 1.0;
    HarmMult := 1.0;

    PriceSignal := 25.0;   // $25/MWH

     // Factors for Autoadd stuff
    UEWeight := 1.0;  // Default to weighting UE same as losses
    LossWeight := 1.0;
    NumUEregs := 1;
    NumLossRegs := 1;
    UEregs := NIL;  // set to something so it wont break reallocmem
    LossRegs := NIL;
    Reallocmem(UEregs, sizeof(UEregs^[1]) * NumUEregs);
    Reallocmem(Lossregs, sizeof(Lossregs^[1]) * NumLossregs);
    UEregs^[1] := 10;   // Overload UE
    LossRegs^[1] := 13;   // Zone Losses

    CapacityStart := 0.9;     // for Capacity search
    CapacityIncrement := 0.005;

    LoadDurCurve := '';
    LoadDurCurveObj := NIL;
    PriceCurve := '';
    PriceCurveObj := NIL;

     // Flags
    DuplicatesAllowed := FALSE;
    ZonesLocked := FALSE;   // Meter zones recomputed after each change
    MeterZonesComputed := FALSE;
    PositiveSequence := FALSE;
    NeglectLoadY := FALSE;

    NormalMinVolts := 0.95;
    NormalMaxVolts := 1.05;
    EmergMaxVolts := 1.08;
    EmergMinVolts := 0.90;

    NodeMarkerCode := 16;
    NodeMarkerWidth := 1;
    MarkSwitches := FALSE;
    MarkTransformers := FALSE;
    MarkCapacitors := FALSE;
    MarkRegulators := FALSE;
    MarkPVSystems := FALSE;
    MarkStorage := FALSE;
    MarkFuses := FALSE;
    MarkReclosers := FALSE;

    SwitchMarkerCode := 5;
    TransMarkerCode := 35;
    CapMarkerCode := 38;
    RegMarkerCode := 17; //47;
    PVMarkerCode := 15;
    StoreMarkerCode := 9;
    FuseMarkerCode := 25;
    RecloserMarkerCode := 17;
    RelayMarkerCode := 17;

    TransMarkerSize := 1;
    CapMarkerSize := 3;
    RegMarkerSize := 5; //1;
    PVMarkerSize := 1;
    StoreMarkerSize := 1;
    FuseMarkerSize := 1;
    RecloserMarkerSize := 5;
    RelayMarkerSize := 5;

    BusMarkerList := TList.Create;
    BusMarkerList.Clear;

    TrapezoidalIntegration := FALSE;  // Default to Euler method
    LogEvents := FALSE;

    GeneratorDispatchReference := 0.0;
    DefaultGrowthRate := 1.025;
    DefaultGrowthFactor := 1.0;

    DefaultDailyShapeObj := DSS.LoadShapeClass.Find('default');
    DefaultYearlyShapeObj := DSS.LoadShapeClass.Find('default');

    CurrentDirectory := '';

    BusNameRedefined := TRUE;  // set to force rebuild of buslists, nodelists

    SavedBuses := NIL;
    SavedBusNames := NIL;

    ReductionStrategy := rsDefault;
    ReductionZmag := 0.02;
    ReduceLateralsKeepLoad := TRUE;

    // Misc objects
    AutoAddObj := TAutoAdd.Create(DSS);

    Branch_List := NIL;
    BusAdjPC := NIL;
    BusAdjPD := NIL;

{$IFDEF DSS_CAPI_ADIAKOPTICS}
   // tearing algorithm vars initialization
    Coverage := 0.9;      // 90% coverage expected by default
    Actual_coverage := -1;       // No coverage
    Num_SubCkts := CPU_Cores - 1;

    // Diakoptics variables
    Contours := TSparse_Complex.Create;
    ZLL := TSparse_Complex.Create;
    ZCC := TSparse_Complex.Create;
    ZCT := TSparse_Complex.Create;
    Y4 := TSparse_Complex.Create;
    // V_0 := TSparse_Complex.Create;
    Ic := TSparse_Complex.Create;

    setlength(Longest_paths, 0);
    setlength(Path_Idx, 0);
    setlength(Buses_Covered, 0);
    setlength(Path_size, 0);

{$ENDIF}

{$IFDEF DSS_CAPI_PM}
    LockIc := syncobjs.TCriticalSection.Create;
{$ENDIF}
end;

destructor TDSSCircuit.Destroy;
var
    i: Integer;
    pCktElem: TDSSCktElement;
    ElemName: String;

begin
    for i := 1 to NumDevices do
    begin
        try
            pCktElem := TDSSCktElement(CktElements.Get(i));
            ElemName := pCktElem.ParentClass.name + '.' + pCktElem.Name;
            pCktElem.Free;
        except
            ON E: Exception do
                DoSimpleMsg(DSS, 'Exception Freeing Circuit Element: %s %s', [ElemName, CRLF + E.Message], 423);
        end;
    end;
    for i := 1 to NumBuses do
        Buses^[i].Free;

    Reallocmem(Buses, 0);
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
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
    IncrCktElements.Free;
{$ENDIF}
    MeterElements.Free;
    Monitors.Free;
    EnergyMeters.Free;
    Sensors.Free;
    Generators.Free;
    StorageElements.Free;
    PVSystems.Free;
    Substations.Free;
    Transformers.Free;
    CapControls.Free;
    SwtControls.Free;
    InvControls.Free;
    ExpControls.Free;
    RegControls.Free;
    Loads.Free;
    Lines.Free;
    ShuntCapacitors.Free;
    Reactors.Free;
    Reclosers.Free;
    Relays.Free;
    Fuses.Free;
    AutoTransformers.Free;

    ControlQueue.Free;

    ClearBusMarkers;
    BusMarkerList.Free;

    AutoAddObj.Free;

    FreeTopology;

{$IFDEF DSS_CAPI_ADIAKOPTICS}
    //  Release all ADiakoptics matrices
    Contours.Free;
    ZLL.Free;
    ZCC.Free;
    ZCT.Free;
    Y4.Free;
    // V_0.Free;
    Ic.Free;
{$ENDIF}
{$IFDEF DSS_CAPI_PM}
    LockIc.Free;
{$ENDIF}
    inherited Destroy;
end;

// This routine retuns the index of the element within the array
function get_element_Idx(graph_in: array of Integer; element: Integer): Integer;
var
    Found,                  // To indicate that the element was found
    End_Flag: Boolean;
    Graph_size,
    Local_idx: Integer;
begin
    Result := -1;     // In case the element is not in the array
    End_Flag := TRUE;   //  To control the algorithm execution (while based)
    Local_idx := 0;
    Found := FALSE;  //  Not found yet
    Graph_size := length(graph_in);
    while (End_Flag) and (Local_idx < Graph_Size) do
    begin
        if graph_in[Local_idx] = element then
        begin
            End_Flag := FALSE;
            Found := TRUE;
        end
        else
        begin
            inc(Local_idx);
        end;
    end;
    if Found then
        Result := Local_Idx;
end;
{$IFDEF DSS_CAPI_ADIAKOPTICS}
// This routine calculates the longest path within a linearized
// graph considering the zero level buses as the beginning of
// new path
procedure TDSSCircuit.get_longest_path();
var
    End_flag: Boolean;    //  Terminates the process
    Current_Idx,                  //  Stores the Index value of the current level
    Current_level: Integer;    //  Stores the current level traced
begin
    with solution do
    begin
        Current_level := maxintvalue(Inc_Mat_Levels);                    //  Init level
        Current_idx := get_element_idx(Inc_Mat_Levels, Current_level);  //  Init Index
        End_flag := TRUE;
        setlength(New_graph, 0);
        while End_flag do
        begin
            //Checks the termination criteria
            if (Current_level > Inc_Mat_Levels[Current_idx]) or (Inc_Mat_Levels[Current_idx] = 0) then
                End_Flag := FALSE;
            // Is the current bus part of the new backbone?
            if Inc_Mat_Levels[Current_idx] = Current_level then
            begin
                dec(Current_level);
                setlength(New_graph, (length(New_graph) + 1));
                New_graph[High(New_graph)] := Current_idx;
            end;
            dec(Current_idx);
        end;
    end;
end;

// This routine appends an array to the paths array and returns its index
function TDSSCircuit.Append2PathsArray(New_Path: array of Integer): Integer;
var
    local_idx: Integer;
begin
    Result := High(Longest_paths) + 1;
    for local_idx := 0 to High(New_path) do
    begin
        setlength(Longest_paths, (length(Longest_paths) + 1));
        Longest_paths[High(Longest_paths)] := New_Path[Local_idx];
    end;
end;

// This routine normalizes the Inc_matrix levels
procedure TDSSCircuit.Normalize_graph();
var
    Curr_level,  // To set the active level
    idx: Integer;
    Ref_detected: Boolean; // To detect if there is a zero
begin
    Curr_level := -1; // Initializing values
    Ref_detected := FALSE;
    with Solution do
    begin
        for idx := 0 to High(Inc_Mat_Levels) do     // Sweeps the whole graph
        begin
            if Inc_Mat_Levels[idx] = 0 then
                Ref_detected := TRUE
            else
            begin
                if (Curr_level >= Inc_Mat_Levels[idx]) or Ref_detected then
                begin
                    Ref_detected := FALSE;
                    Curr_level := Inc_Mat_Levels[idx] - 1;
                    Inc_Mat_Levels[idx] := 1;
                end
                else
                    Inc_Mat_Levels[idx] := Inc_Mat_Levels[idx] - Curr_level;
            end;
        end;
    end;
end;

// Traces the paths (0) in the graph to guarantee the desired coverage
procedure TDSSCircuit.Get_paths_4_Coverage();
var
    DBLTemp, //  For storing temoprary doubles
    Sys_Size: Double; //  Stores the number of buses contained in the system
    SMEnd: Boolean; //  Terminates the state machine
    i,
    State: Integer; // The current state of the state machine
    Candidates: array of Integer; // Array for 0 level buses idx
begin
    with solution do
    begin
        SMEnd := TRUE;
        State := 0;
        Sys_Size := length(Inc_Mat_Cols);
        setlength(Buses_Covered, 1);
        setlength(Path_Idx, 1);
        Actual_Coverage := -1;
        while SMEnd do // The state machine starts
        begin
            case State of
                0:
                begin // Processes the first path
                    setlength(Candidates, 0);
                    for i := 0 to (length(Inc_Mat_Levels) - 1) do // Extracts the 0 Level Buses
                    begin
                        if solution.Inc_Mat_Levels[i] = 0 then
                        begin
                            setlength(Candidates, length(Candidates) + 1);
                            Candidates[High(Candidates)] := i;
                        end;
                    end;
                    setlength(Longest_paths, 0);
                    Buses_covered[0] := MaxIntValue(Candidates); // Extracts the maximum level covered
                    Path_Idx[0] := Append2PathsArray(Candidates); // No shifting in the graph
                    State := 1; // Go to the next state
                end;
                1:
                begin // Extracts a new path from the longest branch to
                    get_longest_path(); // the backbone (Zeros)
                    setlength(Path_Idx, (length(Path_Idx) + 1));
                    Path_Idx[High(Path_Idx)] := Append2PathsArray(New_Graph); // Adds the new candidates
                    // Estimates the amount of buses covered in this path
                    setlength(Buses_covered, (length(Buses_covered) + 1));
                    Buses_covered[High(Buses_covered)] := New_Graph[0] - New_Graph[High(New_Graph)];
                    // Replaces the latest path with 0 in the Bus levels array
                    for i := Path_Idx[High(Path_Idx)] to High(Longest_paths) do
                        Inc_Mat_Levels[Longest_paths[i]] := 0;
                    Normalize_graph;
                    // remains in the same state
                end;
            end;
            // Checks the coverage index to stablish if is necessary to keep tracing paths to increase the coverage
            DBLTemp := 0.0;
            for i := Low(Buses_covered) to High(Buses_covered) do
                DBLtemp := DBLTemp + (0.0 + Buses_Covered[i]);
            DBLtemp := DBLTemp / Sys_Size;
            // If the New coverage is different from the previous one and is below the expected coverage keep going
            // The first criteria is to avoid keep working on a path that will not contribute to improve the coverage
            if (DBLTemp <> Actual_Coverage) and (DBLTemp >= Coverage) then
                SMEnd := FALSE;
            Actual_Coverage := DBLTemp;
        end;
    end;
end;

// Appends single phase ISources to the each node of bus specified
// if the given linkBranch. This actions take place within the given file.
procedure TDSSCircuit.AppendIsources(Path: String; BusNum: Integer; LinkBranch: String);
var
    jj,
    kk: Integer;
    text,
    BusName: String;
    pBus: TDSSBus;
    F: TFileStream = NIL;
begin
    F := TBufferedFileStream.Create(Path, fmOpenReadWrite);
    F.Seek(0, soEnd);
    SetElementActive(LinkBranch);
    BusName := ActiveCktElement.GetBus(BusNum);
    jj := ansipos('.', BusName);     // removes the dot
    if jj > 0 then
        BusName := BusName.Substring(0, jj - 1);

    SetActiveBus(DSS, BusName);
    pBus := Buses^[ActiveBusIndex];
    for kk := 1 to pBus.NumNodesThisBus do
    begin
        text := 'New ISource.' + inttostr(BusNum) + '_' + inttostr(kk) + ' phases=1 bus1=' + BusName + '.' + inttostr(kk) + ' amps=0.000001 angle=0';
        FSWriteLn(F, text);
    end;
    F.Free;
end;

{$IFDEF UNIX}
procedure CopyFile(inFn: String; outFn: String; failIfExists: Boolean);
var
    inStream, outStream: TFilestream;
    exists: Boolean;
begin
    exists := FileExists(outFn);
    if FileExists(inFn) and ((not exists) or (exists and not failIfExists)) then
    begin
        inStream := TBufferedFileStream.Create(inFn, fmOpenRead or fmShareDenyWrite);
        try
            outStream := TBufferedFileStream.Create(inFn, fmOpenwrite);
            try
                outStream.Position := outStream.size;
                outStream.CopyFrom(inStream, 0);
            finally
                inStream.Free;
            end;
        finally
            outStream.Free;
        end;
    end;
end;
{$ENDIF}

// This routine reads the master file of the torn circuit and creates the
// header definitions for declaring separate subcircuits in OpenDSS
procedure TDSSCircuit.Format_SubCircuits(Path: String; NumCkts: Integer; AddISrc: Boolean);
var
    myFile: TFileStream = NIL;
    Temp_txt,
    Temp_txt2,
    text: String;
    Xtra,
    File_Struc: array of String;
    Str_Found: Boolean;
    Local_Temp,
    FS_Idx,
    FS_Idx1,
    FS_Idx2: Integer;
const
    Reference: array[0..5] of String = // To filter the source file
        ('Redirect EnergyM', 'Redirect Monitor', 'MakeBu', 'Redirect BusVolta', 'Buscoords busco', 'Redirect zone');

begin
    // Reads the master file
    myFile := TBufferedFileStream.Create(Path + PathDelim + 'Master.dss', fmOpenRead or fmShareDenyWrite);
    setlength(File_Struc, 0);
    FS_Idx := 0;
    while (myFile.Position + 1) < myFile.Size do // Extracts the file content as an array of strings
    begin
        setlength(File_Struc, (length(File_Struc) + 1));
        FSReadLn(myFile, text);
        File_Struc[FS_Idx] := text;
        inc(FS_Idx);
    end;
    FreeAndNil(myFile);
    // Creates the copy for the interconnected system
    setlength(Xtra, 0);

    myFile := TBufferedFileStream.Create(Path + PathDelim + 'Master_Interconnected.dss', fmCreate);
    for FS_Idx := 0 to High(File_Struc) do
    begin
        Str_Found := FALSE;
        for FS_Idx1 := 0 to 5 do
        begin
            Local_Temp := ansipos(Reference[FS_Idx1], File_Struc[FS_Idx]);
            Str_Found := (Local_Temp <> 0) or Str_Found;
        end;
        if Str_found then
        begin
            setlength(Xtra, (length(Xtra) + 1));
            Xtra[High(Xtra)] := File_Struc[FS_Idx];
        end
        else
            FSWriteLn(myFile, File_Struc[FS_Idx]);
    end;
    // Adds the zones and the rest to the file
    for FS_Idx := 0 to High(Xtra) do
    begin
        FSWriteLn(myFile, Xtra[FS_Idx])
    end;
    FreeAndNil(myFile);

    // removes the unnecessary information from the master file (deletes the other zones)
    myFile := TBufferedFileStream.Create(Path + PathDelim + 'Master.dss', fmCreate);
    for FS_Idx := 0 to High(File_Struc) do
    begin
        Local_Temp := ansipos('Redirect zone', File_Struc[FS_Idx]);
        if Local_Temp = 0 then
        begin
            Local_Temp := ansipos('Redirect EnergyM', File_Struc[FS_Idx]);
            if Local_Temp = 0 then
            begin
                Local_Temp := ansipos('Redirect Monitor', File_Struc[FS_Idx]);
                if Local_Temp = 0 then
                    FSWriteLn(myFile, File_Struc[FS_Idx]);
            end;
        end;
    end;
    FreeAndNil(myFile);

    // Adds Isources at the link branch edges if requested
    if AddISrc then
        AppendIsources(Path + PathDelim + 'Master.dss', 1, Link_Branches[1]);

    // Copies the support files to the zones directories
    FS_Idx := 0;
    while FS_Idx <> -1 do
    begin
        Local_Temp := ansipos('Redirect zone', File_Struc[FS_Idx]);
        if Local_Temp = 0 then
        begin
            Local_Temp := ansipos('Redirect ', File_Struc[FS_Idx]);
            if Local_temp <> 0 then
            begin
                text := stringreplace(File_Struc[FS_Idx], 'Redirect ', '', [rfReplaceAll, rfIgnoreCase]);
                for FS_Idx1 := 2 to NumCkts do
                    CopyFile(Pchar(Path + PathDelim + text), Pchar(Path + PathDelim + 'zone_' + inttostr(FS_Idx1) + PathDelim + text), TRUE);
            end;
            inc(FS_Idx);
        end
        else
            FS_Idx := -1; // Ends the routine
    end;
    // Creates the master file for each subcircuit
    for FS_Idx := 2 to NumCkts do
    begin
        myFile := TBufferedFileStream.Create(Path + PathDelim + 'zone_' + inttostr(FS_Idx) + PathDelim + 'Master.dss', fmCreate);
        FSWriteLn(myFile, 'Clear');
        FSWriteLn(myFile, 'New Circuit.Zone_' + inttostr(FS_Idx));
        FS_Idx1 := 2;
        while FS_Idx1 <> -1 do // Writes the global files
        begin
            Local_Temp := ansipos('Redirect zone', File_Struc[FS_Idx1]);
            if Local_Temp = 0 then
            begin
                FSWriteLn(myFile, File_Struc[FS_Idx1]);
                inc(FS_Idx1);
            end
            else
                FS_Idx1 := -1;
        end;
        for FS_Idx1 := 0 to High(File_Struc) do // Writes the zone files
        begin
            Local_Temp := ansipos('Redirect zone_' + inttostr(FS_Idx), File_Struc[FS_Idx1]);
            if Local_Temp <> 0 then
            begin
                text := stringreplace(File_Struc[FS_Idx1], 'zone_' + inttostr(FS_Idx) + PathDelim, '', [rfReplaceAll, rfIgnoreCase]);
                FSWriteLn(myFile, text);
            end;
        end;
        FreeAndNil(myFile);
        // Adds Isources at the link branch edges if requested
        if AddISrc then
        begin
            text := Path + PathDelim + 'zone_' + inttostr(FS_Idx) + PathDelim + 'Master.dss';
            AppendIsources(text, 2, Link_Branches[FS_Idx - 1]);
            // If there is another link branch, means that this zone conencts with other through ZCC
            // Add Another current source at the point of connection
            if Length(Link_Branches) > FS_Idx then
                AppendIsources(text, 1, Link_Branches[FS_Idx]);

        end;

    end;
    // Sets the properties of the VSource on each subcricuit based on the latest voltage measured
    FS_Idx1 := 0;
    for FS_Idx := 1 to NumCkts do
    begin
        if FS_Idx = 1 then
            myFile := TBufferedFileStream.Create(Path + PathDelim + 'VSource.dss', fmCreate)
        else
            myFile := TBufferedFileStream.Create(Path + PathDelim + 'zone_' + inttostr(FS_Idx) + PathDelim + 'VSource.dss', fmCreate);

        for FS_Idx2 := 1 to 3 do
        begin
            if FS_Idx2 = 1 then
            begin
                Temp_txt := 'source';
                Temp_txt2 := 'Edit '
            end
            else
            begin
                Temp_txt := 'Vph_' + inttostr(FS_Idx2);
                Temp_txt2 := 'New '
            end;

            text := Temp_txt2 + 'Vsource.' + Temp_txt +
                ' bus1=' + PConn_Names[FS_Idx - 1] + '.' + inttostr(FS_Idx2) +
                ' phases=1 pu=1.0' +
                ' basekv=' + floattostrF(PConn_Voltages[FS_Idx1], ffGeneral, 8, 3) +
                ' angle=' + floattostrF(PConn_Voltages[FS_Idx1 + 1], ffGeneral, 8, 3) +
                ' R1=0 X1=0.001 R0=0 X0=0.001';
            FSWriteLn(myFile, text);
            FS_Idx1 := FS_Idx1 + 2;
        end;
        FreeAndNil(myFile);
    end;
end;

// Saves the subcircuits created in memory into the hard drive
// The flag AddISrc indicates if its necessary to create
// Isources at the edges of the link branches, the ISource
// magnitude is equal to 0.000001, angle 0 (for A-Diakoptics)
procedure TDSSCircuit.Save_SubCircuits(AddISrc: Boolean);
var
    Fileroot: String;
begin
    // Prepares everything to save the base of the torn circuit on a separate folder
    Fileroot := DSS.OutputDirectory; {CurrentDSSDir;}
    Fileroot := Fileroot + PathDelim + 'Torn_Circuit';
    CreateDir(Fileroot); // Creates the folder for storing the modified circuit
    DelFilesFromDir(Fileroot); // Removes all the files inside the new directory (if exists)
    DSS.DssExecutive.Command := 'save circuit Dir="' + Fileroot + '"';
    // This routine extracts and modifies the file content to separate the subsystems as OpenDSS projects indepedently
    Format_SubCircuits(FileRoot, length(Locations), AddISrc);
end;

// Generates the graph file for MeTIS within the project's folder
function TDSSCircuit.Create_MeTIS_graph(): String;
var
    exists: Boolean;
    myIntVar,
    k,
    jj,
    i: Integer;
    myClass,
    myName,
    FileName: String;
    F: TFileStream = NIL;
    myPDEList,
    MyGraph: array of String;
    MyIdx: array of Integer;
begin
    with solution do
    begin
        // Calculates the incidence matrix and laplacian to generate the graph file to be
        // send to MeTiS
        Calc_Inc_Matrix_Org; //Calculates the ordered incidence matrix
        // Initializes the METIS related variables
        setlength(myPDEList, 1);
        setlength(myGraph, 1);

        Laplacian := IncMat.Transpose(); // Transposes the Incidence Matrix
        Laplacian := Laplacian.multiply(IncMat); // Laplacian Matrix calculated
        // Filters the incidence matrix to remove duplicated branches (parallel)
        for i := 0 to High(Inc_Mat_Cols) do
        begin
            setlength(myIdx, 1);
            MyName := Inc_Mat_Cols[i];
            // first, get the name of all PDE conencted to this Bus
            for jj := 0 to (IncMat.NZero - 1) do
            begin
                if IncMat.data[jj][1] = i then
                begin
                    // Check if this is not a parallel branch
                    exists := FALSE;
                    if length(myIdx) > 1 then // Only if it's not the first time
                    begin
                        for k := 0 to (length(myIdx) - 2) div 2 do
                        begin
                            // Checks for the other terminal
                            if jj < High(IncMat.data) then
                            begin
                                if IncMat.data[jj + 1][0] = IncMat.data[jj][0] then
                                    myIntVar := IncMat.data[jj + 1][1]
                                else
                                    myIntVar := IncMat.data[jj - 1][1];
                            end
                            else
                                myIntVar := IncMat.data[jj - 1][1];

                            if myIdx[k * 2] = myIntVar then
                            begin
                                exists := TRUE;
                                break;
                            end;
                        end;
                    end;

                    if not exists then
                    begin
                        // Stores the name of the PDE
                        myName := Inc_Mat_Rows[IncMat.data[jj][0]];
                        myPDEList[High(myPDEList)] := myName;
                        setlength(myPDEList, length(myPDEList) + 1);
                        // Checks for the other terminal
                        if jj < High(IncMat.data) then
                        begin
                            if IncMat.data[jj + 1][0] = IncMat.data[jj][0] then
                                myIdx[High(myIdx)] := IncMat.data[jj + 1][1]
                            else
                                myIdx[High(myIdx)] := IncMat.data[jj - 1][1];
                        end
                        else
                            myIdx[High(myIdx)] := IncMat.data[jj - 1][1];

                        setlength(myIdx, length(myIdx) + 1);
                        // Now, get the number of Phases
                        myIntVar := ansipos('.', myName);
                        myClass := myName.Substring(0, (myIntVar - 1));
                        // if transformer, the weigth is the lowest
                        if myClass <> 'Transformer' then
                        begin
                            DSS.ActiveCircuit.SetElementActive(MyName);
                            myIntVar := DSS.ActiveCircuit.ActiveCktElement.NPhases;
                        end
                        else
                            myIntVar := 1;

                        myIdx[High(myIdx)] := myIntVar;
                        setlength(myIdx, length(myIdx) + 1);
                    end;
                end;
            end;
            setlength(myIdx, length(myIdx) - 1);
            myName := '';
            for jj := 0 to High(myIdx) do
                myName := myName + inttostr(myIdx[jj]) + ' ';
            myGraph[High(myGraph)] := myName;
            setlength(myGraph, length(myGraph) + 1);

        end;
        setlength(myGraph, length(myGraph) - 1);
        setlength(myPDEList, length(myPDEList) - 1);

        // Generates the graph file
        // First, get the number of branches in the model excluding parallel branches
        jj := 0;
        for i := 0 to High(Inc_Mat_Rows) do
        begin
            // check if it's on the list
            for k := 0 to High(myPDEList) do
            begin
                if AnsiLowerCase(Inc_Mat_Rows[i]) = AnsiLowerCase(myPDEList[k]) then
                begin
                    inc(jj);
                    break;
                end;
            end;
        end;

        FileName := DSS.OutputDirectory + DSS.CircuitName_ + '.graph';
        F := TBufferedFileStream.Create(FileName, fmCreate);
        FSWriteln(F, inttostr(length(Inc_Mat_Cols)) + ' ' + inttostr(jj) + ' 1'); // it should be the rank of the incidence matrix
        for i := 1 to High(myGraph) do
            FSWriteln(F, myGraph[i]);
        F.Free();

    end;

    Result := FileName;
end;

// Executes MeTIS and gets the names of the link branches between zones
function TDSSCircuit.Create_MeTIS_Zones(Filename: String): String;
var
    MeTISCmd,
    TextCmd: String;
    Num_Pieces,
    j, jj,
    i: Integer;
    Replacer: TFileSearchReplace;
    Flag: Boolean;
begin
    Num_pieces := Num_SubCkts;
    with solution do
    begin
{$IFNDEF UNIX}
//    if Num_pieces <= 8 then MeTISCmd   :=  'kmetis.exe'  // For less than 8 zones use pMeTIS
//    else MeTISCmd   :=  'kmetis.exe';                    // For more than 8 zonez use k-Way (kMeTIS)
    // In the past we use to use pmetis and kmetis, however, in our latest update we realized kmetis is enough
    // update 09-24-2020 by Davis Montenegro
        MeTISCmd := 'kmetis.exe';
{$ELSE}
        MeTISCmd := 'kmetis';
{$ENDIF}
        if fileexists(Pchar(FileName + '.part.' + inttostr(Num_pieces))) then // Checks if the file exists before
            deletefile(Pchar(FileName + '.part.' + inttostr(Num_pieces)));
        repeat
            Process.RunCommand(DSSDirectory + MeTISCmd, [Filename, inttostr(Num_pieces)], TextCmd); // Executes MeTIS
            Flag := ANSIContainsText(TextCmd, 'I detected an error');
            if Flag then // The # of edges was wrong, use the one proposed by MeTIS
            begin
                TextCmd := GetNumEdges(TextCmd); // Gest the # of edges proposed by MeTIS
                jj := length(inttostr(length(Inc_Mat_Cols))) + 2;// Caculates the index for replacing the number in the Graph File
                // Replaces the old data with the new at the file header
                Replacer := TFileSearchReplace.Create(FileName);
                try
                    Replacer.Replace(inttostr(length(Inc_Mat_Cols)) + ' ' + inttostr(length(Inc_Mat_Cols) - 1),
                        inttostr(length(Inc_Mat_Cols)) + ' ' + TextCmd, [rfIgnoreCase]);
                finally
                    Replacer.Free;
                end;
            end;
        until not flag;

        // Verifies if there was no error executing MeTIS and the zones file was created
        if (TextCmd <> '**Error**') and fileexists(Pchar(FileName + '.part.' + inttostr(Num_pieces))) then
        begin
            MeTISZones := TStringList.Create; // Opens the file containing the tearing results
            MeTISZones.LoadFromFile(FileName + '.part.' + inttostr(Num_pieces));
            TextCmd := MeTISZones.Strings[1];
            MeTISZones.Delete(0);
            MetisZones.Insert(0, TextCmd);
            setlength(Locations, 1);
            setlength(BusZones, 1);
            for i := 0 to (MeTISZones.Count - 1) do
            begin
                if i = 0 then
                begin
                    Locations[i] := 0;
                    BusZones[i] := MeTISZones[i];
                end
                else
                begin
                    if MeTISZones[i] <> BusZones[high(BusZones)] then // Moving to another zone in the file
                    begin
                        j := 0;
                        if i < (MeTISZones.Count - 1) then // If not lower means the zone is only 1 bus
                            j := Integer(MeTISZones[i] = MeTISZones[i + 1]);
                        if j = 1 then // Varifies that the zone is big enough
                        begin
                            j := 0; // Verifies that this zone hasn't been counted before
                            for jj := 0 to High(BusZones) do
                            begin
                                if MeTISZones[i] = BusZones[jj] then
                                begin
                                    inc(j);
                                    Break;
                                end;
                            end;
                            if j = 0 then // Is not in the list, add the new location
                            begin
                                setlength(Locations, Length(Locations) + 1);
                                setlength(BusZones, Length(BusZones) + 1);
                                Locations[High(Locations)] := i;
                                BusZones[High(BusZones)] := MeTISZones[i];
                            end;
                        end;
                    end;
                end;
            end;

        end;
        for j := 0 to High(Locations) do
            inc(Locations[j]); //Adjust the location coords

    end;
    Result := TextCmd

end;

// Disables all DER present in the model
procedure TDSSCircuit.Disable_All_DER();
var
    myDERIdx,
    myIdx,
    DevClassIndex: Integer;
    myDERList: array of String;

begin
    setlength(myDERList, 3);
    myDERList := ['PVSystem', 'Generator', 'Storage'];

    for myDERIdx := 0 to High(myDERList) do
    begin
        DevClassIndex := DSS.ClassNames.Find(myDERList[myDERIdx]);
        DSS.LastClassReferenced := DevClassIndex;
        DSS.ActiveDSSClass := DSS.DSSClassList.Get(DSS.LastClassReferenced);
        if DSS.ActiveDSSClass.ElementCount > 0 then
        begin
            myIdx := DSS.ActiveDSSClass.First;
            repeat
                ActiveCktElement.Enabled := FALSE;
                myIdx := DSS.ActiveDSSClass.Next;
            until (myIdx <= 0);
        end;
    end;
end;

// Aggregates profiles using the number of zones defined by the user
procedure TDSSCircuit.AggregateProfiles(mode: String);
var
    F: TFileStream = NIL;
    FileRoot,
    myPCE,
    TextCmd,
    myFilename: String;
    iElem,
    k,
    j,
    i: Integer;
    EMeter: TEnergyMeterObj;
    pMonitor: TMonitorObj;
    ActiveLSObject: TLoadshapeObj;
    pLine: TLineObj;
    myPF,
    myWeight,
    mykW,
    mykvar,
    TotalkW: Double;
    myLoadShapes,
    myLoads: array of String;
    mykvarShape,
    myLoadShape: array of Double;
    PFSpecified,
    UseActual: Boolean;
    qmult: Double;
begin
    UseActual := FALSE;
    if AnsiLowerCase(mode) = 'actual' then
        UseActual := TRUE;

    myFileName := Create_MeTIS_Graph();
    TextCmd := Create_MeTIS_Zones(myFileName);
    // Gets the link branches from the MeTIS estimation
    with solution do
    begin
        setlength(Link_Branches, High(Locations));
        for i := 1 to High(Locations) do
            Link_Branches[i - 1] := Inc_Mat_Rows[get_IncMatrix_Row(Locations[i])];
    end;
    // Disables DER if any
    // Disable_All_DER();
    // Disables Monitors and EnergyMeters if any
    EMeter := EnergyMeters.First;
    while EMeter <> NIL do
    begin
        EMeter.Enabled := FALSE;
        EMeter := EnergyMeters.Next;
    end;
    pMonitor := Monitors.First;
    while pMonitor <> NIL do
    begin
        pMonitor.Enabled := FALSE;
        pMonitor := Monitors.Next;
    end;

    // Add monitors and Energy Meters at link branches
    // Creates and EnergyMeter at the feeder head
    pLine := Lines.First;
    DSS.DSSExecutive.Command := 'New EnergyMeter.myEMZoneFH element=' + CheckForBlanks(pLine.FullName) + ' terminal=1';
    for i := 0 to High(Link_Branches) do
    begin
        DSS.DSSExecutive.Command := 'New EnergyMeter.myEMZone' + InttoStr(i) + ' element=' + Link_Branches[i] + ' terminal=1';
    end;
    // Gets the max number of iterations to configure the simulation using the existing loadshapes
    iElem := DSS.LoadshapeClass.First;
    j := 0;
    while iElem <> 0 do
    begin
        ActiveLSObject := DSS.ActiveDSSObject as TLoadShapeObj;
        k := ActiveLSObject.NumPoints;
        if k > j then
            j := k;
        iElem := DSS.LoadshapeClass.Next;
    end;
    // Configures simulation
    solution.Mode := TSolveMode.SNAPSHOT;
    solution.MaxIterations := 100;
    Solution.MaxControlIterations := 100;

    // solves the circuit
    solution.Solve();

    // Creates the folder for storign the results
    Fileroot := DSS.OutputDirectory; {CurrentDSSDir;}
    Fileroot := Fileroot + 'Aggregated_model';
    CreateDir(Fileroot); // Creates the folder for storing the modified circuit
    DelFilesFromDir(Fileroot); // Removes all the files inside the new directory (if exists)
    // Now starts aggregating the loadshapes per zone
    EnergyMeters.First;
    setlength(myLoadShapes, 1);
    for i := 1 to EnergyMeters.Count do
    begin
        EMeter := EnergyMeters.Active;
        if EMeter.Enabled then
        begin
            // First, get the total load at nominal value for the zone
            EMeter.GetPCEatZone;
            TotalkW := 0;
            setlength(myLoads, 1);
            k := 0; // the load count
            for j := 0 to (High(EMeter.ZonePCE) - 1) do
            begin
                myPCE := stripextension(EMeter.ZonePCE[j]);
                if myPCE = 'Load' then
                begin
                    SetElementActive(EMeter.ZonePCE[j]);
                    TotalkW := TotalkW + TLoadObj(DSS.ActiveDSSObject).kWBase;
                    myLoads[k] := TLoadObj(DSS.ActiveDSSObject).Name;
                    setlength(myLoads, length(myLoads) + 1);
                    inc(k);
                end;
            end;
            setlength(myLoads, length(myLoads) - 1);
            // initializes the length of the aggregated vector
            setlength(myLoadShape, 0);     // clears the arrays first
            setlength(mykvarShape, 0);

            if length(myLoads) > 0 then
            begin
                SetElementActive('Load.' + myLoads[0]);
                setlength(myLoadShape, TLoadObj(DSS.ActiveDSSObject).YearlyShapeObj.NumPoints);
                setlength(mykvarShape, length(myLoadShape));
                // Next, aggregate the load profiles for the zone
                for j := 0 to High(myLoads) do
                begin
                    SetElementActive('Load.' + myLoads[j]);
                    myWeight := 0.0;
                    myPF := TLoadObj(DSS.ActiveDSSObject).PFNominal;
                    PFSpecified := TLoadObj(DSS.ActiveDSSObject).IsPFSpecified;
                    if TLoadObj(DSS.ActiveDSSObject).YearlyShapeObj <> NIL then
                        DSS.LoadshapeClass.SetActive(TLoadObj(DSS.ActiveDSSObject).YearlyShapeObj.Name)
                    else
                        DSS.LoadshapeClass.SetActive('');

                    DSS.ActiveDSSObject := DSS.LoadshapeClass.ElementList.Active;
                    for iElem := 0 to High(myLoadShape) do
                    begin
                        myLoadShape[iElem] := myLoadShape[iElem] + TLoadshapeObj(DSS.ActiveDSSObject).PMult(iElem);

                        if TLoadshapeObj(DSS.ActiveDSSObject).QMult(iElem, qmult) then
                        begin
                            myWeight := qmult;
                            if myWeight = 0 then
                            begin
                                if PFSpecified and (myPF <> 1.0) then  // Qmult not specified but PF was
                                begin  // user specified the PF for this load
                                    myWeight := TLoadshapeObj(DSS.ActiveDSSObject).PMult(iElem) * SQRT((1.0 / SQR(myPF) - 1));
                                    if myPF < 0.0 then // watts and vare are in opposite directions
                                        myWeight := -myWeight;
                                end
                            end
                        end
                        else
                        begin
                            myWeight := 0.0;
                            if PFSpecified and (myPF <> 1.0) then  // Qmult not specified but PF was
                            begin  // user specified the PF for this load
                                myWeight := TLoadshapeObj(DSS.ActiveDSSObject).PMult(iElem) * SQRT((1.0 / SQR(myPF) - 1));
                                if myPF < 0.0 then // watts and vare are in opposite directions
                                    myWeight := -myWeight;
                            end
                        end;
                        mykvarShape[iElem] := mykvarShape[iElem] + myWeight;
                    end;
                end;
                TotalkW := length(myLoads);
                // Normalizes the waveforms
                for iElem := 0 to High(myLoadShape) do
                begin
                    myLoadShape[iElem] := myLoadShape[iElem] / TotalkW;
                    mykvarShape[iElem] := mykvarShape[iElem] / TotalkW;
                end;

                // Saves the profile on disk
                myLoadShapes[High(myLoadShapes)] := DSS.OutputDirectory {CurrentDSSDir} + 'loadShape_' + EMeter.Name + '.csv';

                F := TBufferedFileStream.Create(myLoadShapes[High(myLoadShapes)], fmCreate);
                for j := 0 to High(myLoadShape) do
                    FSWriteln(F, floattostr(myLoadShape[j]) + ',' + floattostr(mykvarShape[j]));

                FreeAndNil(F);

                setlength(myLoadShapes, length(myLoadShapes) + 1);
            end;
        end;
        EnergyMeters.Next;
    end;
    setlength(myLoadShapes, length(myLoadShapes) - 1);
    // Deactivate all loadshapes in the model
    DSS.LoadshapeClass.First;
    for j := 1 to DSS.LoadshapeClass.ElementCount do
    begin
        DSS.ActiveDSSObject := DSS.LoadshapeClass.ElementList.Active;
        TLoadshapeObj(DSS.ActiveDSSObject).Enabled := FALSE;
        DSS.LoadshapeClass.Next;
    end;

    // Declare the new loadshapes in the model
    for j := 0 to High(myLoadShapes) do
    begin
        TextCmd := '';
        if UseActual then
            TextCmd := ' UseActual=Yes';
        TextCmd := 'New LoadShape.myShape_' + inttostr(j) + ' npts=' +
            floattostr(length(myLoadShape)) + ' interval=1 mult=(file=' + myLoadShapes[j] + ' col=1, header=No)' +
            'Qmult=(file=' + myLoadShapes[j] + ' col=2, header=No)' + TextCmd;
        DSS.DSSExecutive.Command := TextCmd;
    end;
    // Assigns the new loadshapes to all the loads in the zone
    // also, disables the energy meters created and restores the originals
    //  DSS.DSSExecutive.Command :=  'solve snap';
    k := 0;
    EnergyMeters.First;
    for i := 1 to EnergyMeters.Count do
    begin
        EMeter := EnergyMeters.Active;
        if EMeter.Enabled then
        begin
            EMeter.GetPCEatZone;
            if length(EMeter.ZonePCE) > 1 then
            begin
                for j := 0 to High(EMeter.ZonePCE) do
                begin
                    myPCE := stripextension(EMeter.ZonePCE[j]);
                    if myPCE = 'Load' then
                    begin
                        // Stores the reference values for the loads in memory to be consistent in
                        // the feeder's definition
                        SetElementActive(EMeter.ZonePCE[j]);
                        mykW := TLoadObj(DSS.ActiveDSSObject).kWref;
                        mykvar := TLoadObj(DSS.ActiveDSSObject).kVARref;
                        DSS.DSSExecutive.Command := EMeter.ZonePCE[j] + '.yearly=myShape_' + inttostr(k);
                        SetElementActive(EMeter.ZonePCE[j]);
                        // Restores the nominal values for saving the file
                        TLoadObj(DSS.ActiveDSSObject).kWBase := mykW;
                        TLoadObj(DSS.ActiveDSSObject).kvarBase := mykvar;
                        TLoadObj(DSS.ActiveDSSObject).kWref := mykW;
                        TLoadObj(DSS.ActiveDSSObject).kvarref := mykvar;
                        TLoadObj(DSS.ActiveDSSObject).kVABase := mykW / Abs(TLoadObj(DSS.ActiveDSSObject).PFNominal);
                    end;
                end;
                inc(k);
            end;
            EMeter.Enabled := FALSE;
        end
        else
            EMeter.Enabled := TRUE;
        EnergyMeters.Next;
    end;

    // saves the new model
    DSS.DssExecutive.Command := 'save circuit Dir="' + Fileroot + '"';
end;

// This routine tears the circuit into many pieces as CPUs are
// available in the local computer (in the best case)
function TDSSCircuit.Tear_Circuit(): Integer;
var
    BusName,
    Terminal,
    TextCmd,
    PDElement,
    FileName: String;
    NodeIdx,
    Num_Pieces,
    j, jj, dbg,
    i: Integer; // Generic counter variables
    EMeter: TEnergyMeterObj;
    pBus: TDSSBus;
    Volts: Polar;
    Term_volts: array of Double; // To verify the connection of the branch
begin
    Num_pieces := Num_SubCkts;
    with solution do
    begin
        FileName := Create_METIS_Graph();
        TextCmd := Create_METIS_Zones(FileName);

        // Verifies if there was no error executing MeTIS and the zones file was created
        if (TextCmd <> '**Error**') and fileexists(Pchar(FileName + '.part.' + inttostr(Num_pieces))) then
        begin
            // ***********The directory is ready for storing the new circuit****************
            EMeter := EnergyMeters.First;
            while EMeter <> NIL do
            begin
                EMeter.Enabled := FALSE;
                EMeter := EnergyMeters.Next;
            end;
            // ************ Creates the meters at the tearing locations  ********************
            Result := 1; // Resets the result variable (Return)
            setlength(PConn_Voltages, length(Locations) * 6); //  Sets the memory space for storing the voltage at the point of conn
            setlength(Link_branches, length(Locations)); //  Sets the memory space for storing the link branches names
            setlength(PConn_Names, length(Locations)); //  Sets the memory space for storing the Bus names
            DSS.SolutionAbort := FALSE;
            j := 0;
            for i := 0 to High(Locations) do
            begin
                if Locations[i] > 0 then
                begin
                    inc(Result);
                    // Gets the name of the PDE for placing the EnergyMeter
                    with solution do
                    begin
                        PDElement := Inc_Mat_Rows[get_IncMatrix_Row(Locations[i])];
                        Link_Branches[i] := PDElement;
                        dbg := get_IncMatrix_Col(Locations[i]); // Temporary stores the given location
                        // Checks the branch orientation across the feeder by substracting the voltages around the branch
                        // Start with Bus 1
                        setlength(Term_volts, 2);
                        for dbg := 0 to 1 do
                        begin
                            BusName := Inc_Mat_Cols[Active_Cols[dbg]];
                            SetActiveBus(DSS, BusName); // Activates the Bus
                            pBus := Buses^[ActiveBusIndex];
                            jj := 1;
                            // this code so nodes come out in order from smallest to larges
                            repeat
                                NodeIdx := pBus.FindIdx(jj); // Get the index of the Node that matches jj
                                inc(jj)
                            until NodeIdx > 0;
                            Volts := ctopolardeg(Solution.NodeV^[pBus.RefNo[NodeIdx]]);  // referenced to pBus
                            Term_volts[dbg] := Volts.mag;
                        end;

                        // Determines the best place to connect the EnergyMeter
                        Term_volts[0] := Term_volts[0] - Term_volts[1];
                        if Term_volts[0] >= 0 then
                            jj := 0
                        else
                            jj := 1;
                        BusName := Inc_Mat_Cols[Active_Cols[jj]];
                        Terminal := 'terminal=' + inttostr(jj + 1);

                        PConn_Names[i] := BusName;
                        SetActiveBus(DSS, BusName);           // Activates the Bus
                        pBus := Buses^[ActiveBusIndex];

                        for jj := 1 to 3 do
                        begin
                            // this code so nodes come out in order from smallest to larges
                            NodeIdx := pBus.FindIdx(jj);   // Get the index of the Node that matches jj

                            Volts := ctopolardeg(Solution.NodeV^[pBus.RefNo[NodeIdx]]);  // referenced to pBus
                            PConn_Voltages[j] := (Volts.mag / 1000);
                            inc(j);
                            PConn_Voltages[j] := Volts.ang;
                            inc(j);
                        end;

                    end;
                    // Generates the OpenDSS Command;
                    DSS.DssExecutive.Command := 'New EnergyMeter.Zone_' + inttostr(i + 1) + ' element=' + PDElement + ' ' + Terminal + ' option=R action=C';
                end
                else
                begin
                    if Locations[i] = 0 then    // The reference bus (Actor 1)
                    begin
                        BusName := Inc_Mat_Cols[0];
                        PConn_Names[i] := BusName;
                        SetActiveBus(DSS, BusName);           // Activates the Bus
                        pBus := Buses^[ActiveBusIndex];
                        // Stores the voltages for the Reference bus first
                        for jj := 1 to 3 do
                        begin
                            // this code so nodes come out in order from smallest to larges
                            NodeIdx := pBus.FindIdx(jj);   // Get the index of the Node that matches jj

                            Volts := ctopolardeg(Solution.NodeV^[pBus.GetRef(NodeIdx)]);  // referenced to pBus
                            PConn_Voltages[j] := (Volts.mag / 1000);
                            inc(j);
                            PConn_Voltages[j] := Volts.ang;
                            inc(j);
                        end;
                    end;
                end;
            end;
        end
        else
        begin
            if (TextCmd = '**Error**') then
                DoErrorMsg(DSS, 'Tear_Circuit', 'MeTIS cannot start.',
                    'The MeTIS program (pmetis.exe/kmetis.exe) cannot be executed/found.', 7006)
            else
                DoErrorMsg(DSS, 'Tear_Circuit', 'The graph file is incorrect.',
                    'MeTIS cannot process the graph file because is incorrect' +
                    '(The number of edges is incorrect).', 7007);
        end;
    end;
end;

{$ENDIF}

// Returns the list of all PDE connected to the bus name given at BusName
function TDSSCircuit.getPDEatBus(BusName: String; useNone: Boolean): ArrayOfString;
var
    Dss_Class: TDSSClass;
    j, i: Integer;
    myBus: array of String;
begin
    SetLength(myBus, 2);
    SetLength(Result, 0);
    BusName := AnsiLowerCase(BusName);
    for i := 1 to DSS.DSSClassList.Count do
    begin
        Dss_Class := DSS.DSSClassList.Get(i);
        if (DSS_Class is TCktElementClass) then
        begin
            // Checks if it is a PCE class
            if not (DSS_Class.ClassType.InheritsFrom(TPDClass)) then
                continue;

            // If it is, checks all the elements to verify if one or more are
            // connected to the bus given
            DSS_Class.First;
            for j := 1 to DSS_Class.ElementCount do
            begin
                myBus[0] := AnsiLowerCase(StripExtension(ActiveCktElement.GetBus(1)));
                myBus[1] := AnsiLowerCase(StripExtension(ActiveCktElement.GetBus(2)));
                if ((myBus[0] = BusName) or (myBus[1] = BusName)) and (myBus[0] <> myBus[1]) then
                begin
                    SetLength(Result, length(Result) + 1);
                    Result[High(Result)] := ActiveCktElement.FullName;
                end;
                DSS_Class.Next;
            end;
        end;
    end;
    if (length(Result) = 0) and useNone then
    begin
        SetLength(Result, 1);
        Result[0] := 'None';
    end;
end;

// Returns the list of all PCE connected to the bus nam given at BusName
function TDSSCircuit.getPCEatBus(BusName: String; useNone: Boolean): ArrayOfString;
var
    Dss_Class: TDSSClass;
    j, i: Integer;
    myBus: String;
begin
    SetLength(Result, 0);
    BusName := AnsiLowerCase(BusName);
    for i := 1 to DSS.DSSClassList.Count do
    begin
        Dss_Class := DSS.DSSClassList.Get(i);
        if not (DSS_Class is TCktElementClass) then
            continue;

        // Checks if it is a PCE class
        if not (DSS_Class.ClassType.InheritsFrom(TPCClass) or (DSS_Class.Name = 'Capacitor') or (DSS_Class.Name = 'Reactor')) then
            continue;

        // If it is, checks all the elements to verify if one or more are
        // connected to the bus given
        DSS_Class.First;
        for j := 1 to DSS_Class.ElementCount do
        begin
            myBus := AnsiLowerCase(StripExtension(ActiveCktElement.GetBus(1)));
            if myBus = BusName then
            begin
                SetLength(Result, length(Result) + 1);
                Result[High(Result)] := ActiveCktElement.FullName;
            end;
            DSS_Class.Next;
        end;
    end;
    if (length(Result) = 0) and useNone then
    begin
        SetLength(Result, 1);
        Result[0] := 'None';
    end;
end;

// Gets all PCE at given bus and returns the list as string
function TDSSCircuit.ReportPCEatBus(BusName: String): String;
var
    i: Integer;
    myPCEList: array of String;

begin
    myPCEList := getPCEatBus(BusName);
    Result := '';
    for i := 0 to High(myPCEList) do
        if myPCEList[i] <> '' then
            Result := Result + myPCEList[i] + ',';
end;

// Gets all PDE at given bus and returns the list as string
function TDSSCircuit.ReportPDEatBus(BusName: String): String;
var
    i: Integer;
    myPDEList: array of String;

begin
    myPDEList := getPDEatBus(BusName);
    Result := '';
    for i := 0 to High(myPDEList) do
        if myPDEList[i] <> '' then
            Result := Result + myPDEList[i] + ',';
end;

procedure TDSSCircuit.ProcessBusDefs;
var
    CurrentBus, BusName: String;
    NNodes, NP, Ncond, i, j, iTerm, RetVal: Integer;
    NodesOK: Boolean;
begin
    with ActiveCktElement do
    begin
        np := NPhases;
        Ncond := NConds;

        CurrentBus := FirstBus;     // use parser functions to decode
        for iTerm := 1 to Nterms do
        begin
            NodesOK := TRUE;
           // Assume normal phase rotation  for default
            for i := 1 to np do
                NodeBuffer^[i] := i; // set up buffer with defaults
 
            // Default all other conductors to a ground connection
            // If user wants them ungrounded, must be specified explicitly!
            for i := np + 1 to NCond do
                NodeBuffer^[i] := 0;

            // Parser will override bus connection if any specified
            BusName := DSS.Parser.ParseAsBusName(CurrentBus, NNodes, NodeBuffer);

            // Check for error in node specification
            for j := 1 to NNodes do
            begin
                if NodeBuffer^[j] < 0 then
                begin
                    retval := DSSMessageDlg('Error in Node specification for Element: "' + ParentClass.Name + '.' + Name + '"' + CRLF +
                        'Bus Spec: "' + DSS.Parser.Token + '"', FALSE);
                    NodesOK := FALSE;
                    if retval = -1 then
                    begin
                        AbortBusProcess := TRUE;
                        AppendGlobalresult(DSS, 'Aborted bus process.');
                        Exit
                    end;
                    Break;
                end;
            end;

            // Node -Terminal Connnections
            // Caution: Magic -- AddBus replaces values in nodeBuffer to correspond
            // with global node reference number.
            if NodesOK then
            begin
                ActiveTerminalIdx := iTerm;
                ActiveTerminal.BusRef := AddBus(BusName, Ncond);
                SetNodeRef(iTerm, NodeBuffer);  // for active circuit
            end;
            CurrentBus := NextBus;
        end;
    end;
end;


procedure TDSSCircuit.AddABus;
begin
    //TODO: check if other growth rates are better from large systems
    if NumBuses > MaxBuses then
    begin
        Inc(MaxBuses, IncBuses);
        ReallocMem(Buses, SizeOf(Buses^[1]) * MaxBuses);
    end;
end;

procedure TDSSCircuit.AddANodeBus;
begin
    if NumNodes > MaxNodes then
    begin
        Inc(MaxNodes, IncNodes);
        ReallocMem(MapNodeToBus, SizeOf(MapNodeToBus^[1]) * MaxNodes);
    end;
end;

function TDSSCircuit.AddBus(const BusName: String; NNodes: Integer): Integer;

var
    NodeRef, i: Integer;
begin
    // Trap error in bus name
    if Length(BusName) = 0 then
    begin  // Error in busname
        DoErrorMsg(DSS, 'TDSSCircuit.AddBus', 'BusName for Object "' + ActiveCktElement.Name + '" is null.',
            'Error in definition of object.', 424);
        for i := 1 to ActiveCktElement.NConds do
            NodeBuffer^[i] := 0;
        Result := 0;
        Exit;
    end;

    Result := BusList.Find(BusName);
    if Result = 0 then
    begin
        Result := BusList.Add(BusName);    // Result is index of bus
        Inc(NumBuses);
        AddABus;   // Allocates more memory if necessary
        Buses^[NumBuses] := TDSSBus.Create(DSS);
    end;

    // Define nodes belonging to the bus
    // Replace Nodebuffer values with global reference number
    with Buses^[Result] do
    begin
        for i := 1 to NNodes do
        begin
            NodeRef := Add(self, NodeBuffer^[i]);
            if NodeRef = NumNodes then
            begin  // This was a new node so Add a NodeToBus element ????
                AddANodeBus;   // Allocates more memory if necessary
                MapNodeToBus^[NumNodes].BusRef := Result;
                MapNodeToBus^[NumNodes].NodeNum := NodeBuffer^[i]
            end;
            NodeBuffer^[i] := NodeRef;  //  Swap out in preparation to setnoderef call
        end;
    end;
end;

function TDSSCircuit.SetElementActive(const FullObjectName: String): Integer;
// Fast way to set a cktelement active
var
    DevIndex: Integer;
    DevClassIndex: Integer;
    DevType,
    DevName: String;
    DevCls: TDSSClass;
    element: TDSSCktElement;
begin
    Result := 0;
    ParseObjectClassandName(DSS, FullObjectName, DevType, DevName);
    DevClassIndex := DSS.ClassNames.Find(DevType);
    if DevClassIndex = 0 then
        DevClassIndex := DSS.LastClassReferenced;
    DevCls := DSS.DSSClassList.At(DevClassIndex);

    if DevName = '' then
    begin
        DSS.CmdResult := Result;
        Exit;
    end;

    if not DuplicatesAllowed then
    begin
        element := TDSSCktElement(DevCls.Find(DevName, FALSE));
        if element <> NIL then
        begin
            DSS.ActiveDSSClass := DSS.DSSClassList.Get(DevClassIndex);
            DSS.LastClassReferenced := DevClassIndex;
            Result := element.Handle;
            ActiveCktElement := CktElements.Get(Result);
        end;
    end
    else
    begin
        Devindex := DeviceList.Find(DevName);
        while DevIndex > 0 do
        begin
            if TDSSCktElement(CktElements.At(Devindex)).ParentClass = DevCls then   // we got a match
            begin
                DSS.ActiveDSSClass := DSS.DSSClassList.Get(DevClassIndex);
                DSS.LastClassReferenced := DevClassIndex;
                Result := Devindex;
                ActiveCktElement := CktElements.Get(Result);
                break;
            end;
            Devindex := Devicelist.FindNext;   // Could be duplicates
        end;
    end;
    DSS.CmdResult := Result;
end;

procedure TDSSCircuit.Set_ActiveCktElement(Value: TDSSCktElement);
begin
    FActiveCktElement := Value;
    DSS.ActiveDSSObject := Value;
end;

procedure TDSSCircuit.AddCktElement(Obj: TDSSCktElement);
begin
    // Update lists that keep track of individual circuit elements
    Inc(NumDevices);

    // Resize DeviceList if no. of devices greatly exceeds allocation
    if Cardinal(NumDevices) > 2 * DeviceList.InitialAllocation then
        ReAllocDeviceList;
    DeviceList.Add(Obj.Name);
    CktElements.Add(Obj);

    // Build Lists of PC and PD elements
    case (Obj.DSSObjType and BaseClassMask) of
        PD_ELEMENT:
            PDElements.Add(Obj);
        PC_ELEMENT:
            PCElements.Add(Obj);
        CTRL_ELEMENT:
            DSSControls.Add(Obj);
        METER_ELEMENT:
            MeterElements.Add(Obj);
    else
       // Nothing
    end;

    // Build  lists of Special elements and generic types
    case (Obj.DSSObjType and CLASSMASK) of
        MON_ELEMENT:
            Monitors.Add(Obj);
        ENERGY_METER:
            EnergyMeters.Add(Obj);
        SENSOR_ELEMENT:
            Sensors.Add(Obj);
        GEN_ELEMENT:
            Generators.Add(Obj);
        SOURCE:
            Sources.Add(Obj);
        CAP_CONTROL:
            CapControls.Add(Obj);
        SWT_CONTROL:
            SwtControls.Add(Obj);
        REG_CONTROL:
            RegControls.Add(Obj);
        LOAD_ELEMENT:
            Loads.Add(Obj);
        CAP_ELEMENT:
            ShuntCapacitors.Add(Obj);
        REACTOR_ELEMENT:
            Reactors.Add(Obj);
        RELAY_CONTROL:
            Relays.Add(Obj);
        FUSE_CONTROL:
            Fuses.Add(Obj);
        RECLOSER_CONTROL:
            Reclosers.Add(Obj);

       // Keep Lines, Transformer, and Lines and Faults in PDElements and separate lists
       // so we can find them quickly.
        AUTOTRANS_ELEMENT:
            AutoTransformers.Add(Obj);
        XFMR_ELEMENT:
            Transformers.Add(Obj);
        LINE_ELEMENT:
            Lines.Add(Obj);
        FAULTOBJECT:
            Faults.Add(Obj);

        STORAGE_ELEMENT:
            StorageElements.Add(Obj);
        PVSYSTEM_ELEMENT:
            PVSystems.Add(Obj);
        INV_CONTROL:
            InvControls.Add(Obj);
        EXP_CONTROL:
            ExpControls.Add(Obj);
    end;

    Obj.Handle := CktElements.Count;
end;

procedure TDSSCircuit.DoResetMeterZones;
begin
    // Do this only if meterzones unlocked .  Normally, Zones will remain unlocked
    // so that all changes to the circuit will result in rebuilding the lists
    if not MeterZonesComputed or not ZonesLocked then
    begin
        if LogEvents then
            LogThisEvent(DSS, 'Resetting Meter Zones');
        DSS.EnergyMeterClass.ResetMeterZonesAll;
        MeterZonesComputed := TRUE;
        if LogEvents then
            LogThisEvent(DSS, 'Done Resetting Meter Zones');
    end;
    FreeTopology;
end;

procedure TDSSCircuit.SaveBusInfo;
var
    i: Integer;
begin
    // Save existing bus definitions and names for info that needs to be restored
    SavedBuses := Allocmem(Sizeof(SavedBuses^[1]) * NumBuses);
    SavedBusNames := Allocmem(Sizeof(SavedBusNames^[1]) * NumBuses);

    for i := 1 to NumBuses do
    begin
        SavedBuses^[i] := Buses^[i];
        SavedBusNames^[i] := BusList.NameOfIndex(i);
    end;
    SavedNumBuses := NumBuses;
end;

procedure TDSSCircuit.RestoreBusInfo;
var
    i, j, idx, jdx: Integer;
    pBus: TDSSBus;
begin
    // Restore  kV bases, other values to buses still in the list
    for i := 1 to SavedNumBuses do
    begin
        idx := BusList.Find(SavedBusNames^[i]);
        if idx <> 0 then
            with Buses^[idx] do
            begin
                pBus := SavedBuses^[i];
                kvBase := pBus.kVBase;
                x := pBus.x;
                Y := pBus.y;
                CoordDefined := pBus.CoordDefined;
                Keep := pBus.Keep;
                // Restore Voltages in new bus def that existed in old bus def
                if assigned(pBus.VBus) then
                begin
                    for j := 1 to pBus.NumNodesThisBus do
                    begin
                        jdx := FindIdx(pBus.GetNum(j));  // Find index in new bus for j-th node  in old bus
                        if jdx > 0 then
                            Vbus^[jdx] := pBus.VBus^[j];
                    end;
                end;
            end;
        SavedBusNames^[i] := ''; // De-allocate string
    end;

    if Assigned(SavedBuses) then
        for i := 1 to SavedNumBuses do
            SavedBuses^[i].Free;  // gets rid of old bus voltages, too

    ReallocMem(SavedBuses, 0);
    ReallocMem(SavedBusNames, 0);
end;

procedure TDSSCircuit.ReProcessBusDefs;
// Redo all Buslists, nodelists
var
    CktElementSave: TDSSCktElement;
    i: Integer;
begin
    if LogEvents then
        LogThisEvent(DSS, 'Reprocessing Bus Definitions');

    AbortBusProcess := FALSE;
    SaveBusInfo;  // So we don't have to keep re-doing this
    // Keeps present definitions of bus objects until new ones created

    // get rid of old bus lists
    BusList.Free;  // Clears hash list of Bus names for adding more
    BusList := TBusHashListType.Create(NumDevices);  // won't have many more buses than this

    NumBuses := 0;  // Leave allocations same, but start count over
    NumNodes := 0;

    // Now redo all enabled circuit elements
    CktElementSave := ActiveCktElement;
    ActiveCktElement := CktElements.First;
    while ActiveCktElement <> NIL do
    begin
        if ActiveCktElement.Enabled then
            ProcessBusDefs;
        if AbortBusProcess then
            Exit;
        ActiveCktElement := CktElements.Next;
    end;

    ActiveCktElement := CktElementSave;  // restore active circuit element

    for i := 1 to NumBuses do
        Buses^[i].AllocateBusState;
    RestoreBusInfo;     // frees old bus info, too
    DoResetMeterZones;  // Fix up meter zones to correspond

    BusNameRedefined := FALSE;  // Get ready for next time
end;

procedure TDSSCircuit.Set_BusNameRedefined(Value: Boolean);
begin
    FBusNameRedefined := Value;

    if Value then
    begin
        Solution.SystemYChanged := TRUE;  // Force Rebuilding of SystemY if bus def has changed
        Control_BusNameRedefined := TRUE;  // So controls will know buses redefined
    end;
end;

function TDSSCircuit.Get_Losses: Complex;
var
    pdelem: TPDElement;
begin
    // Return total losses in all PD Elements
    pdelem := PDElements.First;
    Result := cZERO;
    while pdelem <> NIL do
    begin
        if pdelem.enabled then
        begin
            // Ignore Shunt Elements
            if not pdElem.IsShunt then
                Result += pdelem.losses;
        end;
        pdelem := PDElements.Next;
    end;
end;

procedure TDSSCircuit.DebugDump(var F: TFileStream);
var
    i, j: Integer;
    sout: String;
begin
    FSWriteln(F, 'NumBuses= ', IntToStr(NumBuses));
    FSWriteln(F, 'NumNodes= ', IntToStr(NumNodes));
    FSWriteln(F, 'NumDevices= ', IntToStr(NumDevices));
    FSWriteln(F, 'BusList:');
    for i := 1 to NumBuses do
    begin
        FSWrite(F, '  ', Pad(BusList.NameOfIndex(i), 12));
        FSWrite(F, ' (', IntToStr(Buses^[i].NumNodesThisBus), ' Nodes)');
        for j := 1 to Buses^[i].NumNodesThisBus do
            FSWrite(F, ' ', IntToStr(Buses^[i].Getnum(j)));
        FSWriteln(F);
    end;
    FSWriteln(F, 'DeviceList:');
    for i := 1 to NumDevices do
    begin
        FSWrite(F, '  ', Pad(DeviceList.NameOfIndex(i), 12));
        ActiveCktElement := CktElements.Get(i);
        if not ActiveCktElement.Enabled then
            FSWrite(F, '  DISABLED');
        FSWriteln(F);
    end;
    FSWriteln(F, 'NodeToBus Array:');
    for i := 1 to NumNodes do
    begin
        j := MapNodeToBus^[i].BusRef;
        WriteStr(sout, '  ', i: 2, ' ', j: 2, ' (=', BusList.NameOfIndex(j), '.', MapNodeToBus^[i].NodeNum: 0, ')');
        FSWrite(F, sout);
        FSWriteln(F);
    end;
end;

procedure TDSSCircuit.InvalidateAllPCElements;
var
    p: TDSSCktElement;
begin
    p := PCElements.First;
    while (p <> NIL) do
    begin
        p.YprimInvalid := TRUE;
        p := PCElements.Next;
    end;

    Solution.SystemYChanged := TRUE;  // Force rebuild of matrix on next solution
end;

procedure TDSSCircuit.Set_LoadMultiplier(Value: Double);
begin
    if (Value <> FLoadMultiplier) then   // We may have to change the Y matrix if the load multiplier  has changed
        case Solution.LoadModel of
            ADMITTANCE:
                InvalidateAllPCElements
        else
            {nada}
        end;

    FLoadMultiplier := Value;
end;

procedure TDSSCircuit.TotalizeMeters;
//  Totalize all energymeters in the problem
var
    pEM: TEnergyMeterObj;
    i: Integer;
begin
    for i := 1 to NumEMRegisters do
        RegisterTotals[i] := 0.;

    pEM := EnergyMeters.First;
    while pEM <> NIL do
        with PEM do
        begin
            for i := 1 to NumEMRegisters do
                RegisterTotals[i] := RegisterTotals[i] + Registers[i] * TotalsMask[i];

            pEM := EnergyMeters.Next;
        end;
end;

function TDSSCircuit.ComputeCapacity: Boolean;
var
    CapacityFound: Boolean;

    function SumSelectedRegisters(const mtrRegisters: TRegisterArray; Regs: pIntegerArray; count: Integer): Double;
    var
        i: Integer;
    begin
        Result := 0.0;
        for i := 1 to count do
        begin
            Result := Result + mtrRegisters[regs^[i]];
        end;
    end;

begin
    Result := FALSE;
    if (EnergyMeters.Count = 0) then
    begin
        DoSimpleMsg(DSS, _('Cannot compute system capacity with EnergyMeter objects!'), 430);
        Exit;
    end;

    if (NumUeRegs = 0) then
    begin
        DoSimpleMsg(DSS, _('Cannot compute system capacity with no UE resisters defined.  Use SET UEREGS=(...) command.'), 431);
        Exit;
    end;

    Solution.Mode := TSolveMode.SNAPSHOT;
    LoadMultiplier := CapacityStart;
    CapacityFound := FALSE;

    repeat
        DSS.EnergyMeterClass.ResetAll;
        Solution.Solve;
        DSS.EnergyMeterClass.SampleAll;
        TotalizeMeters;

        // Check for non-zero in UEregs
        if SumSelectedRegisters(RegisterTotals, UEregs, NumUEregs) <> 0.0 then
            CapacityFound := TRUE;
        // LoadMultiplier is a property ...
        if not CapacityFound then
            LoadMultiplier := LoadMultiplier + CapacityIncrement;
    until (LoadMultiplier > 1.0) or CapacityFound;
    if LoadMultiplier > 1.0 then
        LoadMultiplier := 1.0;
    Result := TRUE;
end;

function TDSSCircuit.Save(Dir: String): Boolean;
// Save the present circuit - Enabled devices only
var
    i: Integer;
    Success: Boolean;
    CurrDir, SaveDir: String;
begin
    Result := FALSE;
    // Make a new subfolder in the present folder based on the circuit name and
    // a unique sequence number
    SaveDir := DSS.CurrentDSSDir;  // remember where to come back to
    Success := FALSE;
    if Length(Dir) = 0 then
    begin
        Dir := Name;
        if DSS_CAPI_ALLOW_CHANGE_DIR then
            CurrDir := Dir
        else
            CurrDir := SaveDir + Dir;

        for i := 0 to 999 do  // Find a unique dir name
        begin
            if not DirectoryExists(CurrDir) then
            begin
                if CreateDir(CurrDir) then
                begin
                    DSS.SetCurrentDSSDir(CurrDir);
                    Success := TRUE;
                    Break;
                end;
            end;
            CurrDir := dir + Format('%.3d', [i]);
        end;
    end
    else
    begin
        if not DSS_CAPI_ALLOW_CHANGE_DIR then
        begin
            // There doesn't seem to exist a function for "is absolute path" in FPC,
            // so let's do some simple checks instead
{$ifdef WINDOWS}
            if ((Length(Dir) >= 3) and (AnsiUpperCase(Dir[1]) >= 'A') and (AnsiUpperCase(Dir[1]) <= 'Z') and (Dir[2] = ':')) or // "X:"?
                ((Length(Dir) >= 2) and (Dir[1] = '\') and (Dir[2] = '\')) // \\networkdrive?
            then
            begin
                // At least looks like a full path, nothing to do
            end
            else if ((Length(Dir) >= 2) and ((Dir[1] = '\') or (Dir[1] = '/'))) then // root path
            begin
                // Looks like a root path, so add the current drive
                Dir := ExtractFileDrive(SaveDir) + ':' + Dir;
            end
{$else}
            if ((Length(Dir) >= 2) and (Dir[1] = '/')) then // root path
            begin
                // Looks like a root path, nothing to do
            end
{$endif}
            else
            begin
                // Probably a relative path
                Dir := SaveDir + Dir;
            end;
        end;

        if not DirectoryExists(Dir) then
        begin
            CurrDir := dir;
            if CreateDir(CurrDir) then
            begin
                DSS.SetCurrentDSSDir(CurrDir);
                Success := TRUE;
            end;
        end
        else
        begin  // Exists - overwrite
            CurrDir := Dir;
            DSS.SetCurrentDSSDir(CurrDir);
            Success := TRUE;
        end;
    end;

    if not Success then
    begin
        DoSimpleMsg(DSS, 'Could not create a folder "%s" for saving the circuit.', [Dir], 432);
        Exit;
    end;

    DSS.SavedFileList.Clear;  // This list keeps track of all files saved

    // Initialize so we will know when we have saved the circuit elements
    for i := 1 to CktElements.Count do
        Exclude(TDSSCktElement(CktElements.Get(i)).Flags, Flg.HasBeenSaved);

    // Initialize so we don't save a class twice
    for i := 1 to DSS.DSSClassList.Count do
        TDssClass(DSS.DSSClassList.Get(i)).Saved := FALSE;

    // Define voltage sources first
    Success := WriteVsourceClassFile(DSS, GetDssClassPtr(DSS, 'vsource'), TRUE);
    // Write library files so that they will be available to lines, loads, etc
    // Use default filename=classname
    if Success then
        Success := WriteClassFile(DSS, GetDssClassPtr(DSS, 'wiredata'), '', FALSE);
    if Success then
        Success := WriteClassFile(DSS, GetDssClassPtr(DSS, 'cndata'), '', FALSE);
    if Success then
        Success := WriteClassFile(DSS, GetDssClassPtr(DSS, 'tsdata'), '', FALSE);
    if Success then
        Success := WriteClassFile(DSS, GetDssClassPtr(DSS, 'linegeometry'), '', FALSE);
    // If Success Then Success :=  WriteClassFile(DSS, GetDssClassPtr(DSS, 'linecode'),'', FALSE);
    if Success then
        Success := WriteClassFile(DSS, GetDssClassPtr(DSS, 'linespacing'), '', FALSE);
    if Success then
        Success := WriteClassFile(DSS, GetDssClassPtr(DSS, 'linecode'), '', FALSE);
    if Success then
        Success := WriteClassFile(DSS, GetDssClassPtr(DSS, 'xfmrcode'), '', FALSE);
    if Success then
        Success := WriteClassFile(DSS, GetDssClassPtr(DSS, 'loadshape'), '', FALSE);
    if Success then
        Success := WriteClassFile(DSS, GetDssClassPtr(DSS, 'TShape'), '', FALSE);
    if Success then
        Success := WriteClassFile(DSS, GetDssClassPtr(DSS, 'priceshape'), '', FALSE);
    if Success then
        Success := WriteClassFile(DSS, GetDssClassPtr(DSS, 'growthshape'), '', FALSE);
    if Success then
        Success := WriteClassFile(DSS, GetDssClassPtr(DSS, 'XYcurve'), '', FALSE);
    if Success then
        Success := WriteClassFile(DSS, GetDssClassPtr(DSS, 'TCC_Curve'), '', FALSE);
    if Success then
        Success := WriteClassFile(DSS, GetDssClassPtr(DSS, 'Spectrum'), '', FALSE);
    if Success then
        Success := SaveFeeders; // Save feeders first
    if Success then
        Success := SaveDSSObjects;  // Save rest ot the objects
    if Success then
        Success := SaveVoltageBases;
    if Success then
        Success := SaveBusCoords;
    if Success then
        Success := SaveMasterFile;

    if Success then
    begin
        DSS.GlobalResult := Format('Circuit saved in directory: %s', [DSS.CurrentDSSDir]);
        // DoSimpleMsg('Circuit saved in directory: ' + CurrentDSSDir, 433)
    end
    else
        DoSimpleMsg(DSS, 'Error attempting to save circuit in %s', [DSS.CurrentDSSDir], 434);

    // Return to Original directory
    DSS.SetCurrentDSSDir(SaveDir);

    Result := TRUE;
end;

function TDSSCircuit.SaveDSSObjects: Boolean;
var

    Dss_Class: TDSSClass;
    i: Integer;

begin
    Result := FALSE;

    // Write Files for all populated DSS Classes  Except Solution Class
    for i := 1 to DSS.DSSClassList.Count do
    begin
        Dss_Class := DSS.DSSClassList.Get(i);
        if Dss_Class.Saved then
            Continue;   // Cycle to next
        //use default filename=classname
        if not WriteClassFile(DSS, Dss_Class, '', (DSS_Class is TCktElementClass)) then
            Exit;  // bail on error
        DSS_Class.Saved := TRUE;
    end;

    Result := TRUE;
end;

function TDSSCircuit.SaveVoltageBases: Boolean;
var
    F: TFileStream = NIL;
    VBases: String;
begin
    Result := FALSE;
    try
        F := TBufferedFileStream.Create(DSS.CurrentDSSDir + 'BusVoltageBases.dss', fmCreate);
        DSS.DssExecutive.Command := 'get voltagebases';
        VBases := DSS.GlobalResult;
        FSWriteln(F, 'Set Voltagebases=' + VBases);
        FreeAndNil(F);
        Result := TRUE;
    except
        On E: Exception do
            DoSimpleMsg(DSS, 'Error Saving BusVoltageBases File: %s', [E.Message], 43501);
    end;

    FreeAndNil(F);
end;

function TDSSCircuit.SaveMasterFile: Boolean;
var
    F: TFileStream = NIL;
    i: Integer;
begin
    Result := FALSE;
    try
        F := TBufferedFileStream.Create(DSS.CurrentDSSDir + 'Master.dss', fmCreate);
        FSWriteln(F, 'Clear');
        FSWriteln(F, 'Set DefaultBaseFreq=', FloatToStr(DSS.DefaultBaseFreq));
        FSWriteln(F, 'New Circuit.' + Name);
        FSWriteln(F);
        If PositiveSequence Then 
            FSWriteln(F, 'Set Cktmodel=', DSS.CktModelEnum.OrdinalToString(Integer(PositiveSequence)));
        if DuplicatesAllowed then
            FSWriteln(F, 'set allowdup=yes');
        FSWriteln(F);

        // Write Redirect for all populated DSS Classes  Except Solution Class
        for i := 1 to DSS.SavedFileList.Count do
        begin
            FSWrite(F, 'Redirect ');
            FSWriteln(F, ExtractRelativePath(DSS.CurrentDSSDir, DSS.SavedFileList.Strings[i - 1]));
        end;

        FSWriteln(F, 'MakeBusList');
        FSWriteln(F, 'Redirect BusVoltageBases.dss  ! set voltage bases');

        if FileExists('BusCoords.dss') then
        begin
            FSWriteln(F, 'BusCoords BusCoords.dss');
        end;

        FreeAndNil(F);
        Result := TRUE;
    except
        On E: Exception do
            DoSimpleMsg(DSS, 'Error Saving Master File: %s', [E.Message], 435);
    end;
    FreeAndNil(F);
end;

function TDSSCircuit.SaveFeeders: Boolean;
var
    i: Integer;
    SaveDir, CurrDir: String;
    Meter: TEnergyMeterObj;
begin
    Result := TRUE;
    // Write out all energy meter  zones to separate subdirectories
    SaveDir := DSS.CurrentDSSDir;
    for i := 1 to EnergyMeters.Count do
    begin
        Meter := EnergyMeters.Get(i); // Recast pointer
        CurrDir := SaveDir + Meter.Name;
        if not Meter.Enabled then // Only active meters
            continue;

        if DirectoryExists(CurrDir) then
        begin
            DSS.SetCurrentDSSDir(CurrDir);
            Meter.SaveZone();
            DSS.SetCurrentDSSDir(SaveDir);
        end
        else
        begin
            if CreateDir(CurrDir) then
            begin
                DSS.SetCurrentDSSDir(CurrDir);
                Meter.SaveZone();
                DSS.SetCurrentDSSDir(SaveDir);
            end
            else
            begin
                DoSimpleMsg(DSS, 'Cannot create directory: "%s"', [CurrDir], 436);
                Result := FALSE;
                DSS.SetCurrentDSSDir(SaveDir);  // back to whence we came
                Break;
            end;
        end;
    end;
end;

function TDSSCircuit.SaveBusCoords: Boolean;
var
    F: TFileStream = NIL;
    i: Integer;
begin
    Result := FALSE;
    try
        F := TBufferedFileStream.Create(DSS.CurrentDSSDir + 'BusCoords.dss', fmCreate);

        for i := 1 to NumBuses do
        begin
            if Buses^[i].CoordDefined then
            begin
                FSWrite(F, CheckForBlanks(BusList.NameOfIndex(i)));
                FSWriteLn(F, Format(', %-g, %-g', [Buses^[i].X, Buses^[i].Y]));
            end;
        end;

        FreeAndNil(F);

        Result := TRUE;

    except
        On E: Exception do
            DoSimpleMsg(DSS, _('Error creating %s.'), ['BusCoords.dss'], 437);
    end;
    FreeAndNil(F);
end;

procedure TDSSCircuit.ReallocDeviceList;
var
    TempList: THashList;
    i: Integer;
begin
    // Reallocate the device list to improve the performance of searches
    if LogEvents then
        LogThisEvent(DSS, _('Reallocating Device List'));
    TempList := THashList.Create(2 * NumDevices);

    for i := 1 to DeviceList.Count do
    begin
        Templist.Add(DeviceList.NameOfIndex(i));
    end;

    DeviceList.Free; // Throw away the old one.
    Devicelist := TempList;
end;

procedure TDSSCircuit.Set_CaseName(const Value: String);
begin
    FCaseName := Value;
    DSS.CircuitName_ := Value + '_';
end;

function TDSSCircuit.Get_Name: String;
begin
    Result := LocalName;
end;

function TDSSCircuit.GetBusAdjacentPDLists: TAdjArray;
begin
    if not Assigned(BusAdjPD) then
        BuildActiveBusAdjacencyLists(self, BusAdjPD, BusAdjPC);
    Result := BusAdjPD;
end;

function TDSSCircuit.GetBusAdjacentPCLists: TAdjArray;
begin
    if not Assigned(BusAdjPC) then
        BuildActiveBusAdjacencyLists(self, BusAdjPD, BusAdjPC);
    Result := BusAdjPC;
end;

function TDSSCircuit.GetTopology: TCktTree;
var
    i: Integer;
    elem: TDSSCktElement;
begin
    if not assigned(Branch_List) then
    begin
        // Initialize all Circuit Elements and Buses to not checked, then build a new tree
        elem := CktElements.First;
        while assigned(elem) do
        begin
            Exclude(elem.Flags, Flg.Checked);
            for i := 1 to elem.Nterms do
                elem.TerminalsChecked[i - 1] := FALSE;
            Include(elem.Flags, Flg.IsIsolated); // till proven otherwise
            elem := CktElements.Next;
        end;
        for i := 1 to NumBuses do
            Buses^[i].BusChecked := FALSE;
        Branch_List := GetIsolatedSubArea(self, Sources.First, TRUE);  // calls back to build adjacency lists
    end;
    Result := Branch_List;
end;

procedure TDSSCircuit.FreeTopology;
begin
    if Assigned(Branch_List) then
        Branch_List.Free;
    Branch_List := NIL;
    if Assigned(BusAdjPC) then
        FreeAndNilBusAdjacencyLists(BusAdjPD, BusAdjPC);
end;

procedure TDSSCircuit.ClearBusMarkers;
var
    i: Integer;
begin
    for i := 0 to BusMarkerList.count - 1 do
        TBusMarker(BusMarkerList.Items[i]).Free;
    BusMarkerList.Clear;
end;

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
