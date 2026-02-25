unit Bus;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

uses
    CAPI_Types,
    UComplex, DSSUcomplex,
    uCMatrix,
    NamedObject,
    DSSClass,
    DSSObject;

type
    TDSSBus = class(TNamedObject)
    PRIVATE
        procedure AddANode();
    PUBLIC
        nodes: pIntegerArray;
        numNodesThisBus: SmallInt;
        refNo: pIntegerArray;

        VBus,
        BusCurrent: pComplexArray;
        Zsc,
        Ysc,
        Zsc012: TCMatrix;

        x, y,              // coordinates
        kVBase,           // Base kV for each node to ground (0)
        DistFromMeter: Double;

        CoordDefined,
        BusChecked,
        Keep: Boolean;  // Flag for general use in bus searches

        allocation: SmallInt;

        idx: Integer;

        // ***** Reliability Variables
        BusFltRate: Double;  // Accumulated failure rate  downstream from this bus faults per year
        Bus_Num_Interrupt: Double;  // Number of interruptions this bus per year
        Bus_Int_Duration: Double; // Avg Annual Interruption duration for this bus
        BusCustInterrupts: Double; // Accumulated Number of customer interruptions from this bus
        BusCustDurations: Double; // Accumulated Customer outage durations
        BusTotalNumCustomers: Integer;  // Total Number of customers served from this bus
        BusTotalMiles: Double;  // Total length of lines downstream from this bus for Duke siting algorithm
        BusSectionID: Integer; // ID of the feeder section this bus belongs to

        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        procedure AllocateBusQuantities();
        procedure AllocateBusState();
        procedure ZeroReliabilityAccums();

        function Add(Circuit: TNamedObject; NodeNum: SmallInt): Integer;
        function Find(NodeNum: SmallInt): Integer; // Returns reference num for node by node number
        function FindIdx(NodeNum: SmallInt): Integer; // Returns index of node by node number
        function GetRef(NodeIndex: Integer): Integer; // Returns reference Num for node by node index
        function GetNum(NodeIndex: Integer): SmallInt; // Returns ith node number designation

        function GetZsc0(): Complex;
        function GetZsc1(): Complex;

        function Name(): String; // Reuse LocalName
        procedure SetName(value: String);
    end;

    // Bus Collection
    PBusArray = ^TBusArray;
    PDSSBus = ^TDSSBus;
    TBusArray = array[1..99999] of TDSSBus;

    TNodeBus = record
        BusRef: Integer;   // Ref to Bus in circuit's BusList
        NodeNum: SmallInt;
    end;
    PNodeBusArray = ^TNodeBusArray;
    TNodeBusArray = array[1..99999] of TNodeBus;
    PNodeBus = ^TNodeBus;

implementation

uses
    DSSGlobals,
    SysUtils,
    Circuit;

function TDSSBus.Name(): String;
begin
    result := LocalName;
end;

procedure TDSSBus.SetName(value: String);
begin
    LocalName := value;
end;

constructor TDSSBus.Create(dssContext: TDSSContext);
begin
    inherited Create('Bus');
    Allocation := 4;
    nodes := AllocMem(Sizeof(nodes[1]) * Allocation);
    refNo := AllocMem(Sizeof(refNo[1]) * Allocation);
    numNodesThisBus := 0;
    Ysc := NIL;
    Zsc := NIL;
    Zsc012 := NIL;
    VBus := NIL;
    BusCurrent := NIL;
    kVBase := 0.0;  // Signify that it has not been set
    x := 0.0;
    y := 0.0;
    DistFromMeter := 0.0;
    BusFltRate := 0.0;  // accummulated failure rate
    Bus_Int_Duration := 0.0;
    BusCustInterrupts := 0.0; // Accumulated Number of customer interruptions from this bus
    BusCustDurations := 0.0; // Accumulated Customer outage durations
    BusTotalNumCustomers := 0;
    BusTotalMiles := 0.0;  // total length of line downstream
    CoordDefined := FALSE;
    Keep := FALSE;
end;

destructor TDSSBus.Destroy;
begin
    FreeMem(nodes);
    FreeMem(refNo);
    if Ysc <> NIL then
        Ysc.Free;
    if Zsc <> NIL then
        Zsc.Free;
    FreeMem(VBus);
    FreeMem(BusCurrent);

    inherited Destroy;
end;

procedure TDSSBus.AddANode();
begin
    Inc(numNodesThisBus);
    if numNodesThisBus > Allocation then
    begin
        Allocation := Allocation + 1;
        ReallocMem(nodes, Sizeof(nodes[1]) * Allocation);
        ReallocMem(refNo, Sizeof(refNo[1]) * Allocation);
    end;
end;

function TDSSBus.Add(Circuit: TNamedObject; NodeNum: SmallInt): Integer;
var
    circ: TDSSCircuit;
begin
    if NodeNum = 0 then
        Result := 0
    else
    begin
        Result := Find(NodeNum);
        if Result = 0 then
        begin
             // Add a node to the bus
            AddANode;
            nodes[numNodesThisBus] := NodeNum;

            circ := TDSSCircuit(Circuit);
            Inc(circ.NumNodes);  // Global node number for circuit
            refNo[numNodesThisBus] := circ.NumNodes;
            Result := circ.NumNodes;  // Return global node number
        end;
    end;
end;

function TDSSBus.Find(NodeNum: SmallInt): Integer;
// Returns reference number
var
    i: Integer;
begin
    for i := 1 to numNodesThisBus do
    begin
        if nodes[i] = NodeNum then
        begin
            Result := refNo[i];
            Exit;
        end;
    end;
    Result := 0;
end;

function TDSSBus.GetRef(NodeIndex: Integer): Integer;
begin
    Result := 0;
    if (NodeIndex > 0) and (NodeIndex <= numNodesThisBus) then
        Result := refNo[NodeIndex];
end;

function TDSSBus.GetNum(NodeIndex: Integer): SmallInt;
begin
    Result := 0;
    if (NodeIndex > 0) and (NodeIndex <= numNodesThisBus) then
        Result := nodes[NodeIndex];
end;

procedure TDSSBus.AllocateBusQuantities();
// Have to perform a short circuit study to get this allocated
begin
    if Assigned(Ysc) then
        Ysc.Free;
    if Assigned(Zsc) then
        Zsc.Free;
    if Assigned(Zsc012) then
        Zsc012.Free;
    Ysc := Tcmatrix.CreateMatrix(numNodesThisBus);
    Zsc := Tcmatrix.CreateMatrix(numNodesThisBus);
    Zsc012 := Tcmatrix.CreateMatrix(3); //  can only be 3x3  -- 0, 1, 2
    AllocateBusState;
end;

function TDSSBus.GetZsc0(): Complex;
// = Zs + 2 Zm
begin
    if Assigned(Zsc) then
        Result := Zsc.AvgDiagonal + Zsc.AvgOffDiagonal * 2
    else
        Result := 0;
end;

function TDSSBus.GetZsc1(): Complex;
// = Zs-Zm
begin
    if Assigned(Zsc) then
        Result := Zsc.AvgDiagonal - Zsc.AvgOffDiagonal
    else
        Result := 0;
end;

function TDSSBus.FindIdx(NodeNum: SmallInt): Integer;
// Returns Index
var
    i: Integer;
begin
    for i := 1 to numNodesThisBus do
    begin
        if nodes[i] = NodeNum then
        begin
            Result := i;
            Exit;
        end;
    end;
    Result := 0;
end;

procedure TDSSBus.AllocateBusState();
begin
    FreeMem(VBus);
    FreeMem(BusCurrent);
    VBus := AllocMem(Sizeof(Complex) * numNodesThisBus);
    BusCurrent := AllocMem(Sizeof(Complex) * numNodesThisBus);
end;

procedure TDSSBus.ZeroReliabilityAccums();
begin
    BusCustInterrupts := 0.0;
    BusFltRate := 0.0;
    BusTotalNumCustomers := 0;
    BusTotalMiles := 0.0;
    BusCustDurations := 0.0;
    Bus_Num_Interrupt := 0.0;
    BusSectionID := -1; // signify not set
end;

end.
