unit MeterElement;

 {
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    CktElement,
    Bus,
    UComplex, DSSUcomplex,
    DSSClass,
    Arraydef;

type
    TMeterElement = class(TDSSCktElement)

    PUBLIC
        MeteredElement: TDSSCktElement;  // Pointer to target circuit element
        MeteredTerminal: Integer;
        MeteredElementChanged: Boolean;

        SensorCurrent: pDoubleArray;
        SensorVoltage: pDoubleArray;
        PhsAllocationFactor: pDoubleArray;
        CalculatedCurrent: pComplexArray;
        CalculatedVoltage: pComplexArray;
        AvgAllocFactor: Double; {Average Allocation Factor}

        constructor Create(ParClass: TDSSClass);
        destructor Destroy; OVERRIDE;

        procedure TakeSample; VIRTUAL;    // Sample control quantities and set action times in Control Queue
        procedure AllocateSensorArrays;
        procedure CalcAllocationFactors;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Sysutils;

procedure TMeterElement.AllocateSensorArrays;
var
    i: Integer;
begin
    if Assigned(Meteredelement) then
    begin
        ReallocMem(CalculatedCurrent, Sizeof(Complex) * MeteredElement.Yorder);
        ReallocMem(CalculatedVoltage, Sizeof(Complex) * MeteredElement.Yorder);

        // To avoid random values
        for i := 1 to MeteredElement.Yorder do
        begin
            CalculatedCurrent[i] := 0;
            CalculatedVoltage[i] := 0;
        end;
    end;
    ReAllocMem(SensorCurrent, Sizeof(Double) * Fnphases);
    ReAllocMem(SensorVoltage, Sizeof(Double) * Fnphases);
    ReAllocMem(PhsAllocationFactor, Sizeof(Double) * Fnphases);

    // To avoid random values
    for i := 1 to Fnphases do
        PhsAllocationFactor[i] := 0;
end;

procedure TMeterElement.CalcAllocationFactors;
var
    iOffset: Integer;
    i: Integer;
    Mag: Double;
begin
    MeteredElement.GetCurrents(CalculatedCurrent);

    // The Phase Allocation Factor is the amount that the load must change to match the measured peak
    iOffset := (MeteredTerminal - 1) * MeteredElement.NConds;
    AvgAllocFactor := 0.0;
    for i := 1 to Fnphases do
    begin
        Mag := Cabs(CalculatedCurrent^[i + iOffset]);
        if Mag > 0.0 then
            PhsAllocationFactor^[i] := SensorCurrent^[i] / Mag
        else
            PhsAllocationFactor^[i] := 1.0; // No change
        AvgAllocFactor := AvgAllocFactor + PhsAllocationFactor^[i];
    end;
    AvgAllocFactor := AvgAllocFactor / Fnphases;   // Factor for 2- and 3-phase loads

end;

constructor TMeterElement.Create(ParClass: TDSSClass);
begin
    inherited Create(ParClass);
    DSSObjType := METER_ELEMENT;

    MeteredElement := NIL;
    MeteredTerminal := 1;
    SensorCurrent := NIL;
    SensorVoltage := NIL;
    PhsAllocationFactor := NIL;
    CalculatedCurrent := NIL;
    CalculatedVoltage := NIL;
end;

destructor TMeterElement.Destroy;
begin
    if assigned(SensorCurrent) then
        Reallocmem(SensorCurrent, 0);
    if assigned(SensorVoltage) then
        Reallocmem(SensorVoltage, 0);
    if assigned(CalculatedCurrent) then
        Reallocmem(CalculatedCurrent, 0);
    if assigned(CalculatedVoltage) then
        Reallocmem(CalculatedVoltage, 0);
    if assigned(PhsAllocationFactor) then
        Reallocmem(PhsAllocationFactor, 0);

    inherited Destroy;
end;


procedure TMeterElement.TakeSample;
begin
  // virtual function - should be overridden
    DoSimpleMsg('Programming Error: Reached base Meterelement class for TakeSample.' + CRLF + 'Device: ' + Name, 723);
end;

end.
