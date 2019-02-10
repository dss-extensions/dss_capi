unit Bus;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
 2/4/03 added Zsc and Zsc1, Zsc0 properties
}

interface

uses
    ArrayDef,
    uComplex,
    uCMatrix,
    NamedObject;

type


    TDSSBus = class(TNamedObject)
    PRIVATE

        FNumNodesThisBus: Integer;
        Nodes: pIntegerArray;
        Allocation: Integer;
        RefNo: pIntegerArray;

        procedure AddANode;
        function Get_Zsc0: Complex;
        function Get_Zsc1: Complex;

    PUBLIC

        VBus,
        BusCurrent: pComplexArray;
        Zsc,
        Ysc: TCMatrix;

        x, y,              // coordinates
        kVBase,           // Base kV for each node to ground (0)
        DistFromMeter: Double;

        CoordDefined,
        BusChecked,
        Keep,
        IsRadialBus: Boolean;  // Flag for general use in bus searches

       // ***** Reliability Variables
        BusFltRate: Double;  // Accumulated failure rate  downstream from this bus faults per year
        Bus_Num_Interrupt: Double;  // Number of interruptions this bus per year
        Bus_Int_Duration: Double; // Avg Annual Interruption duration for this bus
        BusCustInterrupts: Double; // Accumulated Number of customer interruptions from this bus
        BusCustDurations: Double; // Accumulated Customer outage durations
        BusTotalNumCustomers: Integer;  // Total Number of customers served from this bus
        BusTotalMiles: Double;  // Total length of lines downstream from this bus for Duke siting algorithm
        BusSectionID: Integer; // ID of the feeder section this bus belongs to

        constructor Create;
        destructor Destroy; OVERRIDE;

        procedure AllocateBusQuantities;
        procedure AllocateBusVoltages;
        procedure AllocateBusCurrents;

        function Add(NodeNum: Integer): Integer;
        function Find(NodeNum: Integer): Integer; // Returns reference num for node by node number
        function FindIdx(NodeNum: Integer): Integer; // Returns index of node by node number
        function GetRef(NodeIndex: Integer): Integer; // Returns reference Num for node by node index
        function GetNum(NodeIndex: Integer): Integer; // Returns ith node number designation

        property NumNodesThisBus: Integer READ FNumNodesThisBus;
        property Zsc1: Complex READ Get_Zsc1;
        property Zsc0: Complex READ Get_Zsc0;

    end;

   // Bus Collection
    pTBusArray = ^TBusArray;
    TBusArray = array[1..10] of TDSSBus;

    TNodeBus = record
        BusRef: Integer;   // Ref to Bus in circuit's BusList
        NodeNum: Integer;
    end;
    pTNodeBusArray = ^TNodeBusArray;
    TNodeBusArray = array[1..2] of TNodeBus;

implementation

uses
    DSSGlobals,
    SysUtils;

constructor TDSSBus.Create;
begin
    inherited Create('Bus');
    Allocation := 3;
    Nodes := AllocMem(Sizeof(Nodes^[1]) * Allocation);
    RefNo := AllocMem(Sizeof(RefNo^[1]) * Allocation);
    FNumNodesThisBus := 0;
    Ysc := NIL;
    Zsc := NIL;
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
    IsRadialBus := FALSE;
end;

destructor TDSSBus.Destroy;
begin
    ReallocMem(Nodes, 0);
    ReallocMem(RefNo, 0);
    if Ysc <> NIL then
        Ysc.Free;
    if Zsc <> NIL then
        Zsc.Free;
    if VBus <> NIL then
        Reallocmem(VBus, 0);
    if BusCurrent <> NIL then
        Reallocmem(BusCurrent, 0);

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

function TDSSBus.Add(NodeNum: Integer): Integer;
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

            with ActiveCircuit do
            begin
                INC(NumNodes);  // Global node number for circuit
                RefNo^[FNumNodesThisBus] := NumNodes;
                Result := NumNodes;  // Return global node number
            end;
        end;
    end;
end;


function TDSSBus.Find(NodeNum: Integer): Integer;
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

function TDSSBus.GetNum(NodeIndex: Integer): Integer;
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
    Ysc := Tcmatrix.CreateMatrix(FNumNodesThisBus);
    Zsc := Tcmatrix.CreateMatrix(FNumNodesThisBus);
    AllocateBusVoltages;
    AllocateBusCurrents;

end;

function TDSSBus.Get_Zsc0: Complex;
// = Zs + 2 Zm
begin
    if Assigned(Zsc) then
        Result := Cadd(Zsc.AvgDiagonal, CmulReal(Zsc.AvgOffDiagonal, 2.0))
    else
        Result := cZERO;
end;

function TDSSBus.Get_Zsc1: Complex;
// = Zs-Zm
begin

    if Assigned(Zsc) then
        Result := Csub(Zsc.AvgDiagonal, Zsc.AvgOffDiagonal)
    else
        Result := cZERO;

end;

function TDSSBus.FindIdx(NodeNum: Integer): Integer;
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

procedure TDSSBus.AllocateBusVoltages;
var
    i: Integer;
begin
    Reallocmem(VBus, Sizeof(VBus^[1]) * FNumNodesThisBus);
    for i := 1 to FNumNodesThisBus do
        VBus^[i] := CZERO;
end;

procedure TDSSBus.AllocateBusCurrents;
var
    i: Integer;
begin
    Reallocmem(BusCurrent, Sizeof(BusCurrent^[1]) * FNumNodesThisBus);
    for i := 1 to FNumNodesThisBus do
        BusCurrent^[i] := CZERO;
end;

end.
