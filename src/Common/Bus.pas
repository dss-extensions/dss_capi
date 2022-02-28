unit Bus;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    ArrayDef,
    UComplex, DSSUcomplex,
    uCMatrix,
    NamedObject,
    DSSClass,
    DSSObject;

type
    TDSSBus = class(TNamedObject)
    PRIVATE
        FNumNodesThisBus: SmallInt;

        Nodes: pIntegerArray;
        Allocation: SmallInt;

        procedure AddANode;
        function Get_Zsc0: Complex;
        function Get_Zsc1: Complex;

    PUBLIC
        RefNo: pIntegerArray;

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

        procedure AllocateBusQuantities;
        procedure AllocateBusState;

        function Add(Circuit: TNamedObject; NodeNum: SmallInt): Integer;
        function Find(NodeNum: SmallInt): Integer; // Returns reference num for node by node number
        function FindIdx(NodeNum: SmallInt): Integer; // Returns index of node by node number
        function GetRef(NodeIndex: Integer): Integer; // Returns reference Num for node by node index
        function GetNum(NodeIndex: Integer): SmallInt; // Returns ith node number designation

        property NumNodesThisBus: SmallInt READ FNumNodesThisBus;
        property Zsc1: Complex READ Get_Zsc1;
        property Zsc0: Complex READ Get_Zsc0;

    end;

    // Bus Collection
    pTBusArray = ^TBusArray;
    TBusArray = array[1..10] of TDSSBus;

    TNodeBus = record
        BusRef: Integer;   // Ref to Bus in circuit's BusList
        NodeNum: SmallInt;
    end;
    pTNodeBusArray = ^TNodeBusArray;
    TNodeBusArray = array[1..2] of TNodeBus;

implementation

uses
    DSSGlobals,
    SysUtils,
    Circuit;

constructor TDSSBus.Create(dssContext: TDSSContext);
begin
    inherited Create('Bus');
    Allocation := 4;
    Nodes := AllocMem(Sizeof(Nodes^[1]) * Allocation);
    RefNo := AllocMem(Sizeof(RefNo^[1]) * Allocation);
    FNumNodesThisBus := 0;
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
    FreeMem(Nodes);
    FreeMem(RefNo);
    if Ysc <> NIL then
        Ysc.Free;
    if Zsc <> NIL then
        Zsc.Free;
    FreeMem(VBus);
    FreeMem(BusCurrent);

    inherited Destroy;
end;

procedure TDSSBus.AddANode;
begin
    Inc(FNumNodesThisBus);
    if FNumNodesThisBus > Allocation then
    begin
        Allocation := Allocation + 1;
        ReallocMem(Nodes, Sizeof(Nodes^[1]) * Allocation);
        ReallocMem(RefNo, Sizeof(RefNo^[1]) * Allocation);
    end;
end;

function TDSSBus.Add(Circuit: TNamedObject; NodeNum: SmallInt): Integer;
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
            Nodes^[FNumNodesThisBus] := NodeNum;

            with TDSSCircuit(Circuit) do
            begin
                INC(NumNodes);  // Global node number for circuit
                RefNo^[FNumNodesThisBus] := NumNodes;
                Result := NumNodes;  // Return global node number
            end;
        end;
    end;
end;

function TDSSBus.Find(NodeNum: SmallInt): Integer;
// Returns reference number
var
    i: Integer;
begin
    for i := 1 to FNumNodesThisBus do
    begin
        if Nodes^[i] = NodeNum then
        begin
            Result := RefNo^[i];
            Exit;
        end;
    end;
    Result := 0;
end;

function TDSSBus.GetRef(NodeIndex: Integer): Integer;
begin
    Result := 0;
    if (NodeIndex > 0) and (NodeIndex <= FNumNodesThisBus) then
        Result := Refno^[NodeIndex];
end;

function TDSSBus.GetNum(NodeIndex: Integer): SmallInt;
begin
    Result := 0;
    if (NodeIndex > 0) and (NodeIndex <= FNumNodesThisBus) then
        Result := Nodes^[NodeIndex];
end;

procedure TDSSBus.AllocateBusQuantities;
// Have to perform a short circuit study to get this allocated
begin
    if Assigned(Ysc) then
        Ysc.Free;
    if Assigned(Zsc) then
        Zsc.Free;
    if Assigned(Zsc012) then
        Zsc012.Free;
    Ysc := Tcmatrix.CreateMatrix(FNumNodesThisBus);
    Zsc := Tcmatrix.CreateMatrix(FNumNodesThisBus);
    Zsc012 := Tcmatrix.CreateMatrix(3); //  can only be 3x3  -- 0, 1, 2
    AllocateBusState;
end;

function TDSSBus.Get_Zsc0: Complex;
// = Zs + 2 Zm
begin
    if Assigned(Zsc) then
        Result := Zsc.AvgDiagonal + Zsc.AvgOffDiagonal * 2
    else
        Result := cZERO;
end;

function TDSSBus.Get_Zsc1: Complex;
// = Zs-Zm
begin
    if Assigned(Zsc) then
        Result := Zsc.AvgDiagonal - Zsc.AvgOffDiagonal
    else
        Result := cZERO;
end;

function TDSSBus.FindIdx(NodeNum: SmallInt): Integer;
// Returns Index
var
    i: Integer;
begin
    for i := 1 to FNumNodesThisBus do
    begin
        if Nodes^[i] = NodeNum then
        begin
            Result := i;
            Exit;
        end;
    end;
    Result := 0;
end;

procedure TDSSBus.AllocateBusState;
begin
    FreeMem(VBus);
    FreeMem(BusCurrent);
    VBus := AllocMem(Sizeof(Complex) * FNumNodesThisBus);
    BusCurrent := AllocMem(Sizeof(Complex) * FNumNodesThisBus);
end;

end.
