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
    ucomplex,
    DSSClass,
    Arraydef;

type
    TMeterElement = class(TDSSCktElement)

    PUBLIC

        ElementName: String;
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
begin
    if Assigned(Meteredelement) then
        ReallocMem(CalculatedCurrent, Sizeof(CalculatedCurrent^[1]) * MeteredElement.Yorder);
    if Assigned(Meteredelement) then
        ReallocMem(CalculatedVoltage, Sizeof(CalculatedVoltage^[1]) * MeteredElement.Yorder);
    ReAllocMem(SensorCurrent, Sizeof(SensorCurrent^[1]) * Fnphases);
    ReAllocMem(SensorVoltage, Sizeof(SensorVoltage^[1]) * Fnphases);
    ReAllocMem(PhsAllocationFactor, Sizeof(PhsAllocationFactor^[1]) * Fnphases);
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

    ElementName := '';
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
    Reallocmem(SensorCurrent, 0);
    Reallocmem(SensorVoltage, 0);
    Reallocmem(CalculatedCurrent, 0);
    Reallocmem(CalculatedVoltage, 0);
    Reallocmem(PhsAllocationFactor, 0);

    inherited Destroy;
end;


procedure TMeterElement.TakeSample;
begin
  // virtual function - should be overridden
    DoSimpleMsg('Programming Error:  Reached base Meterelement class for TakeSample.' + CRLF + 'Device: ' + Name, 723);
end;

end.
