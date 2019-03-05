unit PDElement;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   Change Log
   1/10/00  Fixed bug where OverLoad_EEN, _UE was not being set for elements
            where the rating was not specified.
   4/11/01  Fixed error in computin excess kVAs (factor readjustment)
}

interface

uses
    CktElement,
    ucomplex,
    ucmatrix,
    DSSClass,
    MeterElement;

type

    TPDElement = class(TDSSCktElement)
    PRIVATE

        function Get_ExcessKVANorm(idxTerm: Integer): Complex;
        function Get_ExcessKVAEmerg(idxTerm: Integer): Complex;

    PUBLIC

        NormAmps,
        EmergAmps,
        FaultRate,  // annual faults per year
        PctPerm,    // percent of faults that are permanent in this element
        BranchFltRate,    // net failure rate for this branch
        AccumulatedBrFltRate,  // accumulated failure rate for this branch
        MilesThisLine,  // length in miles if line
        AccumulatedMilesDownStream, // total miles downstream
        HrsToRepair: Double;

        FromTerminal,
        ToTerminal: Integer;  // Set by Meter zone for radial feeder
        IsShunt: Boolean;

        BranchNumCustomers: Integer;
        BranchTotalCustomers: Integer;

        BranchCustWeight: Double; // Weighting factor for customers on this elemebt
        BranchSectionID: Integer; // ID of the section that this PD element belongs to

        ParentPDElement: TPDElement;

        MeterObj,                     {Upline energymeter}
        SensorObj: TMeterElement; // Upline Sensor for this element  for allocation and estimation

        Overload_UE,
        OverLoad_EEN: Double;  // Indicate amount of branch overload

        constructor Create(ParClass: TDSSClass);
        destructor Destroy; OVERRIDE;

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure GetCurrents(Curr: pComplexArray); OVERRIDE; // Get present values of terminal

        procedure CalcFltRate; VIRTUAL;  // Calc failure rates for section and buses
        procedure AccumFltRate;
        procedure CalcNum_Int(var SectionCount: Integer; AssumeRestoration: Boolean);  // Calc Number of Interruptions in forward sweep
        procedure CalcCustInterrupts;
        procedure ZeroReliabilityAccums; // Zero out reliability accumulators

        property ExcesskVANorm[idxTerm: Integer]: Complex READ Get_ExcesskVANorm;
        property ExcesskVAEmerg[idxTerm: Integer]: Complex READ Get_ExcesskVAEmerg;

    end;


implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    Bus;

{---------Summing Utility proc-------}
procedure accumsum(var a: Double; b: Double); inline;
begin
    a := a + b;
end;

{------------------------------------}

procedure TPDElement.AccumFltRate;

var
    FromBus: TDSSBus;
    ToBus: TDSSBus;

begin

    with ActiveCircuit do
    begin
        if FromTerminal = 2 then
            Toterminal := 1
        else
            ToTerminal := 2;

        {Get fault Rate for TO bus and add it to this section failure rate}
        ToBus := Buses^[Terminals^[ToTerminal].BusRef];
        AccumulatedBrFltRate := ToBus.BusFltRate + BranchFltRate;
        FromBus := Buses^[Terminals^[FromTerminal].BusRef];
        FromBus.BusTotalNumCustomers := FromBus.BusTotalNumCustomers + BranchTotalCustomers;

        AccumulatedMilesDownStream := ToBus.BusTotalMiles + MilesThisLine;
        accumsum(FromBus.BusTotalMiles, AccumulatedMilesDownStream);

        {Compute accumulated to FROM Bus; if a fault interrupter, assume it isolates all downline faults}
        if not HasOcpDevice then
        begin
            // accumlate it to FROM bus
            accumsum(FromBus.BusFltRate, AccumulatedBrFltRate);
        end;
    end;

end;

procedure TPDElement.CalcFltRate;   {Virtual function  -- LINE is different, for one}

begin
      {Default base algorithm for radial fault rate calculation}
      {May be overridden by specific device class behavior}

    BranchFltRate := Faultrate * pctperm * 0.01;

end;

procedure TPDElement.CalcCustInterrupts;
var
    FromBus: TDSSBus;
begin
    FromBus := ActiveCircuit.Buses^[Terminals^[FromTerminal].BusRef];
    with  FromBus do
    begin
        accumsum(BusCustInterrupts, Bus_Num_Interrupt * BranchTotalCustomers);
    end;
end;

procedure TPDElement.CalcNum_Int(var SectionCount: Integer; AssumeRestoration: Boolean);
{This is called on the forward sweep to set the number of interruptions at the To bus.}
var
    FromBus: TDSSBus;
    ToBus: TDSSBus;

begin

    with ActiveCircuit do
    begin
        if FromTerminal = 2 then
            ToTerminal := 1
        else
            ToTerminal := 2;
        ToBus := Buses^[Terminals^[ToTerminal].BusRef];
        FromBus := Buses^[Terminals^[FromTerminal].BusRef];

        // If no interrupting device then the downline bus will have the same num of interruptions
        ToBus.Bus_Num_Interrupt := FromBus.Bus_Num_Interrupt;

        { If Interrupting device (on FROM side)then downline bus will have
          additional interruptions  ---- including for fused lateral
         If assuming restoration and the device is an automatic device, the To bus will be
         interrupted only for  faults on the main section, not including fused sections.
        }
        if HasOCPDevice then
        begin
            if AssumeRestoration and HasAutoOCPDevice then
               {To Bus will be interrupted only for faults on this section.
                AccumulatedBrFltRate does not include Branches down from
                Branches with OCP devics}
                ToBus.Bus_Num_Interrupt := AccumulatedBrFltRate
            else
                accumsum(ToBus.Bus_Num_Interrupt, AccumulatedBrFltRate);

            {If there is an OCP device on this PDElement, this is the
             beginning of a new section.}
            inc(SectionCount);
            ToBus.BusSectionID := SectionCount; // Assign it to the new section
        end
        else
            ToBus.BusSectionID := FromBus.BusSectionID;   // else it's in the same section

        BranchSectionID := ToBus.BusSectionID;
    end;

end;

constructor TPDElement.Create(ParClass: TDSSClass);
begin
    inherited Create(ParClass);

    IsShunt := FALSE;

    FromTerminal := 1;
    BranchNumCustomers := 0;
    BranchTotalCustomers := 0;
    AccumulatedBrFltRate := 0.0;
    MilesThisLine := 0.0;
    SensorObj := NIL;
    MeterObj := NIL;
    ParentPDElement := NIL;
    DSSObjType := PD_ELEMENT;


end;

destructor TPDElement.Destroy;
begin

    inherited Destroy;
end;

procedure TPDElement.GetCurrents(Curr: pComplexArray);
var
    i: Integer;
begin
    try

        if Enabled then
        begin

            with ActiveCircuit.Solution do
                for i := 1 to Yorder do
                    Vterminal^[i] := NodeV^[NodeRef^[i]];

            YPrim.MVMult(Curr, Vterminal);
        end
        else
            for i := 1 to Yorder do
                Curr^[i] := cZero;

    except
        On E: Exception do
            DoErrorMsg(('Trying to Get Currents for Element: ' + Name + '.'), E.Message,
                'Has the circuit been solved?', 660);
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - -
function TPDElement.Get_ExcessKVANorm(idxTerm: Integer): Complex;

var
    Factor: Double;
    kVA: Complex;
begin

    if (NormAmps = 0.0) or not Enabled then
    begin
        OverLoad_EEN := 0.0;  // bug fixed 1/10/00
        Result := cZero;
        Exit;
    end;

    kVA := CmulReal(Power[idxTerm], 0.001);  // Also forces computation of Current into Itemp
    Factor := (MaxTerminalOneIMag / NormAmps - 1.0);
    if (Factor > 0.0) then
    begin
        OverLoad_EEN := Factor;
        Factor := 1.0 - 1.0 / (Factor + 1.0);   // To get factor
        Result := CmulReal(kVA, Factor);
    end
    else
    begin
        OverLoad_EEN := 0.0;
        Result := cZero;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - -
function TPDElement.Get_ExcessKVAEmerg(idxTerm: Integer): Complex;
var
    Factor: Double;
    kVA: Complex;
begin
    if (EmergAmps = 0.0) or not Enabled then
    begin
        Overload_UE := 0.0;  // bug fixed 1/10/00
        Result := cZero;
        Exit;
    end;

    kVA := CmulReal(Power[idxTerm], 0.001);  // Also forces computation of Current into Itemp

    Factor := (MaxTerminalOneIMag / EmergAmps - 1.0);
    if Factor > 0.0 then
    begin
        Overload_UE := Factor;
        Factor := 1.0 - 1.0 / (Factor + 1.0);  // To get Excess
        Result := CmulReal(kVA, Factor);
    end
    else
    begin
        Overload_UE := 0.0;
        Result := cZero;
    end;

end;

procedure TPDElement.InitPropertyValues(ArrayOffset: Integer);
begin


    PropertyValue[ArrayOffset + 1] := '400';  //Normamps
    PropertyValue[ArrayOffset + 2] := '600';  //emergamps
    PropertyValue[ArrayOffset + 3] := '0.1';  //Fault rate
    PropertyValue[ArrayOffset + 4] := '20';   // Pct Perm
    PropertyValue[ArrayOffset + 5] := '3';    // Hrs to repair

    inherited initPropertyValues(ArrayOffset + 5);

end;


procedure TPDElement.ZeroReliabilityAccums;
var
    FromBus: TDSSBus;

begin
    FromBus := ActiveCircuit.Buses^[Terminals^[FromTerminal].BusRef];
    with  FromBus do
    begin
        BusCustInterrupts := 0.0;
        BusFltRate := 0.0;
        BusTotalNumCustomers := 0;
        BusTotalMiles := 0.0;
        BusCustDurations := 0.0;
        Bus_Num_Interrupt := 0.0;
        BusSectionID := -1; // signify not set
    end;

end;

end.
