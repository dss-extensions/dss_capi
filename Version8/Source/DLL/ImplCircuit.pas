unit ImplCircuit;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
 10/14/99  Corrected the calculation of Circuit Losses
 1/12/00  Modified first..next routines to ignore disabled devices
 8/19/13  Several mods
}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSEngine_TLB,
    StdVcl;

type
    TCircuit = class(TAutoObject, ICircuit)
    PROTECTED
        function Get_Buses(Index: Olevariant): IBus; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        function Get_NumBuses: Integer; SAFECALL;
        function Get_NumCktElements: Integer; SAFECALL;
        function Get_NumNodes: Integer; SAFECALL;
        function Get_CktElements(Idx: Olevariant): ICktElement; SAFECALL;
        function Get_LineLosses: Olevariant; SAFECALL;
        function Get_Losses: Olevariant; SAFECALL;
        function Get_ActiveElement: ICktElement; SAFECALL;
        function Get_AllBusVmag: Olevariant; SAFECALL;
        function Get_AllBusVolts: Olevariant; SAFECALL;
        function Get_AllElementNames: Olevariant; SAFECALL;
        function Get_SubstationLosses: Olevariant; SAFECALL;
        function Get_TotalPower: Olevariant; SAFECALL;
        procedure Disable(const Name: Widestring); SAFECALL;
        procedure Enable(const Name: Widestring); SAFECALL;
        function Get_Solution: ISolution; SAFECALL;
        function Get_ActiveBus: IBus; SAFECALL;
        function FirstPCElement: Integer; SAFECALL;
        function FirstPDElement: Integer; SAFECALL;
        function NextPCElement: Integer; SAFECALL;
        function NextPDElement: Integer; SAFECALL;
        function Get_AllBusNames: Olevariant; SAFECALL;
        function Get_AllElementLosses: Olevariant; SAFECALL;
        procedure Sample; SAFECALL;
        procedure SaveSample; SAFECALL;
        function Get_Generators: IGenerators; SAFECALL;
        function Get_Meters: IMeters; SAFECALL;
        function Get_Monitors: IMonitors; SAFECALL;
        function Get_Settings: ISettings; SAFECALL;
        function Get_Lines: ILines; SAFECALL;
        function SetActiveElement(const FullName: Widestring): Integer; SAFECALL;
        function Capacity(Start, Increment: Double): Double; SAFECALL;
        function Get_AllBusVmagPu: Olevariant; SAFECALL;
        function SetActiveBus(const BusName: Widestring): Integer; SAFECALL;
        function SetActiveBusi(BusIndex: Integer): Integer; SAFECALL;
        function Get_AllNodeNames: Olevariant; SAFECALL;
        function Get_SystemY: Olevariant; SAFECALL;
        function Get_CtrlQueue: ICtrlQueue; SAFECALL;
        function Get_AllBusDistances: Olevariant; SAFECALL;
        function Get_AllNodeDistances: Olevariant; SAFECALL;
        function Get_AllNodeDistancesByPhase(Phase: Integer): Olevariant; SAFECALL;
        function Get_AllNodeVmagByPhase(Phase: Integer): Olevariant; SAFECALL;
        function Get_AllNodeVmagPUByPhase(Phase: Integer): Olevariant; SAFECALL;
        function Get_AllNodeNamesByPhase(Phase: Integer): Olevariant; SAFECALL;
        function Get_Loads: ILoads; SAFECALL;
        function SetActiveClass(const ClassName: Widestring): Integer; SAFECALL;
        function FirstElement: Integer; SAFECALL;
        function NextElement: Integer; SAFECALL;
        function Get_ActiveCktElement: ICktElement; SAFECALL;
        function Get_ActiveDSSElement: IDSSElement; SAFECALL;
        function Get_ActiveClass: IActiveClass; SAFECALL;
        function Get_CapControls: ICapControls; SAFECALL;
        function Get_RegControls: IRegControls; SAFECALL;
        function Get_SwtControls: ISwtControls; SAFECALL;
        function Get_Transformers: ITransformers; SAFECALL;
        function Get_Capacitors: ICapacitors; SAFECALL;
        function Get_Topology: ITopology; SAFECALL;
        function Get_Sensors: ISensors; SAFECALL;
        procedure UpdateStorage; SAFECALL;
        function Get_ParentPDElement: Integer; SAFECALL;
        function Get_XYCurves: IXYCurves; SAFECALL;
        function Get_PDElements: IPDElements; SAFECALL;
        function Get_Reclosers: IReclosers; SAFECALL;
        function Get_Relays: IRelays; SAFECALL;
        function Get_LoadShapes: ILoadShapes; SAFECALL;
        function Get_Fuses: Fuses; SAFECALL;
        function Get_Isources: IISources; SAFECALL;
        procedure EndOfTimeStepUpdate; SAFECALL;
        function Get_DSSim_Coms: IDSSimComs; SAFECALL;  //Declares DSSim_Coms
        function Get_YNodeOrder: Olevariant; SAFECALL;
        function Get_YCurrents: Olevariant; SAFECALL;
        function Get_YNodeVarray: Olevariant; SAFECALL;
        function Get_PVSystems: IPVSystems; SAFECALL;
        function Get_Vsources: IVsources; SAFECALL;
        function Get_Parallel: IParallel; SAFECALL;
        function Get_LineCodes: ILineCodes; SAFECALL;
//    function Get_Loads: ILoads; safecall;  function ICircuit.Get_Loads = ICircuit_Get_Loads;

//  function ICircuit_Get_Loads: IUnknown; safecall;


    end;

implementation

uses
    ComServ,
    DSSClassDefs,
    DSSGlobals,
    ImplGlobals,
    Line,
    UComplex,
    sysutils,
    CktElement,
    ImplDSSElement,
    DSSObject,
    DSSClass,
    Transformer,
    PCElement,
    PDElement,
    Monitor,
    EnergyMeter,
    dialogs,
    YMatrix,
    Variants,
    arrayDef,
    Utilities,
    SolutionAlgs,
    KLUSolve;

function TCircuit.Get_Buses(Index: Olevariant): IBus;

var
    i: Integer;

begin


   {Index is zero based -- matches variant arrays}

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        case (VarType(Index) and varTypeMask) of
            VarSmallint, VarInteger:
                with ActiveCircuit[ActiveActor] do
                begin
                    i := Index;  // Type conversion
                    if NumBuses > i then
                    begin
                        ActiveBusIndex := i + 1;
                    end;
                end;
            VarOleStr:
                with ActiveCircuit[ActiveActor] do
                begin
                    ActiveBusIndex := Buslist.Find(Index);
                end;
        else
            DoSimpleMsg('Illegal Var Type Passed to Buses Interface: ' + Format('$%x', [VarType(Index)]), 5013);
        end;

    end;

    Result := FBus as IBus;  // Return Interface to active Bus

end;

function TCircuit.Get_Name: Widestring;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Name
    else
        Result := '';
end;

function TCircuit.Get_NumBuses: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].NumBuses
    else
        Result := 0;
end;

function TCircuit.Get_NumCktElements: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].NumDevices;
end;

function TCircuit.Get_NumNodes: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].NumNodes;
end;

function TCircuit.Get_CktElements(Idx: Olevariant): ICktElement;

var
    i: Integer;
    S: String;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        case (VarType(Idx) and varTypeMask) of
            varSmallint, VarInteger:
                with ActiveCircuit[ActiveActor] do
                begin
                    i := Idx;
                    if NumDevices > i then
                        ActiveCktElement := CktElements.Get(i + 1);
                end;
            VarOleStr:
            begin
                S := Idx;
                ActiveCircuit[ActiveActor].SetElementActive(S); // By name
            end;
        else
            DoSimpleMsg('Illegal Var Type Passed to CktElements Interface: ' + Format('$%x', [VarType(Idx)]), 5014);
        end;

    end;

   // Now that the element has been set active, return the intf that deals with the active circuit element
    Result := FCktElement as ICktElement;  // Return Interface to active CktElement

end;

function TCircuit.Get_LineLosses: Olevariant;

var
    pLine: TLineObj;
    Loss: Complex;
    V: Variant;

begin
    V := VarArrayCreate([0, 1], varDouble);
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            pLine := Lines.First;
            Loss := Cmplx(0.0, 0.0);
            while pLine <> NIL do
            begin
                CAccum(Loss, pLine.Losses[Activeactor]);
                pLine := Lines.Next;
            end;
            V[0] := Loss.re * 0.001;
            V[1] := Loss.im * 0.001;
        end;

    Result := V;

end;

function TCircuit.Get_Losses: Olevariant;
var
    LossValue: complex;
begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := VarArrayCreate([0, 1], varDouble);
        LossValue := ActiveCircuit[ActiveActor].Losses[ActiveActor];
        Result[0] := LossValue.re;
        Result[1] := LossValue.im;
    end
    else
        Result := VarArrayCreate([0, 0], varDouble);

end;

function TCircuit.Get_ActiveElement: ICktElement;
begin
    Result := FCktElement as ICktElement;
end;

function TCircuit.Get_AllBusVmag: Olevariant;
var
    i, j, k: Integer;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            Result := VarArrayCreate([0, NumNodes - 1], varDouble);
            k := 0;
            for i := 1 to NumBuses do
            begin
                for j := 1 to Buses^[i].NumNodesThisBus do
                begin
                    Result[k] := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(j)]);
                    Inc(k);
                end;
            end;
        end
    else
        Result := VarArrayCreate([0, 0], varDouble);
end;

function TCircuit.Get_AllBusVolts: Olevariant;

var
    i, j, k: Integer;
    Volts: Complex;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            Result := VarArrayCreate([0, 2 * NumNodes - 1], varDouble);
            k := 0;
            for i := 1 to NumBuses do
            begin
                for j := 1 to Buses^[i].NumNodesThisBus do
                begin
                    Volts := ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(j)];
                    Result[k] := Volts.re;
                    Inc(k);
                    Result[k] := Volts.im;
                    Inc(k);
                end;
            end;
        end
    else
        Result := VarArrayCreate([0, 0], varDouble);

end;

function TCircuit.Get_AllElementNames: Olevariant;
var
    i: Integer;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            Result := VarArrayCreate([0, NumDevices - 1], varOleStr);
            for i := 1 to NumDevices do
            begin
                with  TDSSCktElement(CktElements.Get(i)) do
                    Result[i - 1] := ParentClass.Name + '.' + Name;
            end;
        end
    else
        Result := VarArrayCreate([0, 0], varOleStr);
end;

function TCircuit.Get_SubstationLosses: Olevariant;

var
    pTransf: TTransfObj;
    Loss: Complex;

begin
    Result := VarArrayCreate([0, 1], varDouble);
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            pTransf := Transformers.First;
            Loss := Cmplx(0.0, 0.0);
            while pTransf <> NIL do
            begin
                if pTransf.Issubstation then
                    Caccum(Loss, pTransf.Losses[Activeactor]);
                pTransf := Transformers.Next;
            end;
            Result[0] := Loss.re * 0.001;
            Result[1] := Loss.im * 0.001;
        end
    else
    begin
        Result[0] := 0.0;
        Result[1] := 0.0;
    end;

end;

function TCircuit.Get_TotalPower: Olevariant;
// Total power being consumed in the circuit.
// Add up all power being contributed by sources.

// Returns result in kW

var
    pCktElem: TDSSCktElement;
    cPower: Complex;

begin

    Result := VarArrayCreate([0, 1], varDouble);
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            pCktElem := Sources.First;
            cPower := Cmplx(0.0, 0.0);
            while pCktElem <> NIL do
            begin
                CAccum(cPower, pcktElem.Power[1, ActiveActor]);
                pCktElem := Sources.Next;
            end;
            Result[0] := cPower.re * 0.001;
            Result[1] := cPower.im * 0.001;
        end
    else
    begin
        Result[0] := 0.0;
        Result[1] := 0.0;
    end;
end;

procedure TCircuit.Disable(const Name: Widestring);
begin

    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            SetElementActive(Name);
            if ActiveCktElement <> NIL then
                ActiveCktElement.Enabled := FALSE;
        end;

end;

procedure TCircuit.Enable(const Name: Widestring);
begin

    with ActiveCircuit[ActiveActor] do
    begin
        SetElementActive(Name);
        if ActiveCktElement <> NIL then
            ActiveCktElement.Enabled := TRUE;
    end;

end;


function TCircuit.Get_Solution: ISolution;
begin
    Result := FSolution as ISolution;
end;

function TCircuit.Get_ActiveBus: IBus;
begin
    Result := FBus as IBus;
end;

function TCircuit.FirstPCElement: Integer;
var
    p: TDSSCktElement;

{ Returns first enabled element}

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        p := ActiveCircuit[ActiveActor].PCElements.First;
        if p <> NIL then
        begin
            repeat
                if p.enabled then
                begin
                    Result := 1;
                    ActiveCircuit[ActiveActor].ActiveCktElement := p;
                end
                else
                    p := ActiveCircuit[ActiveActor].PCElements.Next;

            until (Result = 1) or (p = NIL);
        end
        else
            Result := 0;
    end;
end;

function TCircuit.FirstPDElement: Integer;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActivePDElement := ActiveCircuit[ActiveActor].PDElements.First;
        if ActivePDElement <> NIL then
        begin
            repeat
                if ActivePDElement.enabled then
                begin
                    Result := 1;
                    ActiveCircuit[ActiveActor].ActiveCktElement := ActivePDElement;
                end
                else
                    ActivePDElement := ActiveCircuit[ActiveActor].PDElements.Next;
            until (Result = 1) or (ActivePDELement = NIL);
        end
        else
            Result := 0;
    end;

end;

function TCircuit.NextPCElement: Integer;

var
    p: TDSSCktElement;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        p := ActiveCircuit[ActiveActor].PCElements.Next;
        if p <> NIL then
        begin
            repeat
                if p.enabled then
                begin
                    Result := ActiveCircuit[ActiveActor].PCElements.ActiveIndex;
                    ActiveCircuit[ActiveActor].ActiveCktElement := p;
                end
                else
                    p := ActiveCircuit[ActiveActor].PCElements.Next;
            until (Result > 0) or (p = NIL);
        end
        else
            Result := 0;
    end;
end;

function TCircuit.NextPDElement: Integer;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActivePDElement := ActiveCircuit[ActiveActor].PDElements.Next;
        if ActivePDElement <> NIL then
        begin
            repeat
                if ActivePDElement.Enabled then
                begin
                    Result := ActiveCircuit[ActiveActor].PDElements.ActiveIndex;
                    ActiveCircuit[ActiveActor].ActiveCktElement := ActivePDElement;
                end
                else
                    ActivePDElement := ActiveCircuit[ActiveActor].PDElements.Next;
            until (Result > 0) or (ActivePDElement = NIL);
        end
        else
        begin
            Result := 0;
        end;
    end;
end;

function TCircuit.Get_AllBusNames: Olevariant;

// Just Bus names      modified 2/7/03

var
    i: Integer;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            Result := VarArrayCreate([0, NumBuses - 1], varOleStr);
            for i := 0 to NumBuses - 1 do
            begin
                Result[i] := BusList.Get(i + 1);
            end;
        end
    else
        Result := VarArrayCreate([0, 0], varOleStr);
end;

function TCircuit.Get_AllElementLosses: Olevariant;

var
    pCktElem: TDSSCktElement;
    cLoss: Complex;
    k: Integer;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            Result := VarArrayCreate([0, 2 * NumDevices - 1], varDouble);
            k := 0;
            pCktElem := CktElements.First;
            while pCktElem <> NIL do
            begin
                cLoss := pCktElem.Losses[ActiveActor];
                Result[k] := cLoss.re * 0.001;
                Inc(k);
                Result[k] := cLoss.im * 0.001;
                Inc(k);
                pCktElem := CktElements.Next;
            end;
        end
    else
        Result := VarArrayCreate([0, 0], varDouble);
end;

procedure TCircuit.Sample;
// Sample all meters and monitors

begin

    MonitorClass[ActiveActor].SampleAll(ActiveActor);

    EnergyMeterClass[ActiveActor].SampleAll(ActiveActor);

end;

procedure TCircuit.SaveSample;
// Save all meters and monitors registers and buffers

var
    Mon: TDSSMonitor;
    Mtr: TEnergyMeter;

begin
    Mon := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('monitor'));
    Mon.SaveAll(ActiveActor);

    Mtr := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('energymeter'));
    Mtr.SaveAll(ActiveActor);
end;

function TCircuit.Get_Generators: IGenerators;
begin
    Result := FGenerators as IGenerators;
end;

function TCircuit.Get_Meters: IMeters;
begin
    Result := FMeters as IMeters;
end;

function TCircuit.Get_Monitors: IMonitors;
begin
    Result := FMonitors as Imonitors;
end;

function TCircuit.Get_Settings: ISettings;
begin
    Result := FSettings as ISettings;
end;

function TCircuit.Get_Lines: ILines;
begin
    Result := FLines as ILines;
end;

function TCircuit.SetActiveElement(const FullName: Widestring): Integer;
begin
    Result := -1;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].SetElementActive(FullName) - 1;   // make zero based to be compatible with collections and variant arrays
    end
    else
        DoSimpleMsg('Create a circuit before trying to set an element active!', 5015);
end;

function TCircuit.Capacity(Start, Increment: Double): Double;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            CapacityStart := Start;
            CapacityIncrement := Increment;
            if ComputeCapacity(ActiveActor) then
                Result := RegisterTotals[3] + RegisterTotals[19]
            else
                Result := 0.0;
        end
    else
    begin
        Result := 0.0;
    end;
end;

function TCircuit.Get_AllBusVmagPu: Olevariant;
var
    i, j, k: Integer;
    Volts, BaseFactor: Double;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            Result := VarArrayCreate([0, NumNodes - 1], varDouble);
            k := 0;
            for i := 1 to NumBuses do
            begin
                if Buses^[i].kVBase > 0.0 then
                    BaseFactor := 1000.0 * Buses^[i].kVBase
                else
                    BaseFactor := 1.0;
                for j := 1 to Buses^[i].NumNodesThisBus do
                begin
                    Volts := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(j)]);
                    Result[k] := Volts / BaseFactor;
                    Inc(k);
                end;
            end;
        end
    else
        Result := VarArrayCreate([0, 0], varDouble);
end;

function TCircuit.SetActiveBus(const BusName: Widestring): Integer;
begin
    DSSGlobals.SetActiveBus(StripExtension(BusName));
    if Assigned(ActiveCircuit[ActiveActor]) then
        Result := ActiveCircuit[ActiveActor].ActiveBusIndex - 1
    else
        Result := -1;
end;

function TCircuit.SetActiveBusi(BusIndex: Integer): Integer;

{ BusIndex is Zero Based}
begin
    Result := -1;   // Signifies Error
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            if (BusIndex >= 0) and (BusIndex < Numbuses) then
            begin
                ActiveBusIndex := BusIndex + 1;
                Result := 0;
            end;
        end;
end;


function TCircuit.Get_AllNodeNames: Olevariant;

// Return all node names (Busname.nodenumber)
// Same order as current solution array.

var
    i, j, k: Integer;
    BusName: String;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            Result := VarArrayCreate([0, NumNodes - 1], varOleStr);
            k := 0;
            for i := 1 to NumBuses do
            begin
                BusName := BusList.Get(i);
                for j := 1 to Buses^[i].NumNodesThisBus do
                begin
                    Result[k] := BusName + '.' + IntToStr(Buses^[i].GetNum(j));
                    Inc(k);
                end;
            end;
        end
    else
        Result := VarArrayCreate([0, 0], varOleStr);
end;

// this calls the compressed column function from KLUSolve, but
// still returns the full square matrix (including zeros) through COM
function TCircuit.Get_SystemY: Olevariant;

{Return System Y matrix, complex form}

var
    iV, nBus, nNZ,
    i, j, p,
    NValues: Longword;
    hY: NativeUint;
    ColPtr, RowIdx: array of Longword;
    cVals: array of Complex;

begin

{ Return zero length Array if no circuit or no Y matrix}
    if ActiveCircuit[ActiveActor] = NIL then
        Result := VarArrayCreate([0, 0], varDouble)
    else
    if ActiveCircuit[ActiveActor].Solution.hY = 0 then
        Result := VarArrayCreate([0, 0], varDouble)
    else
        with ActiveCircuit[ActiveActor] do
        begin
            hY := ActiveCircuit[ActiveActor].Solution.hY;

      // get the compressed columns out of KLU
            FactorSparseMatrix(hY); // no extra work if already done
            GetNNZ(hY, @nNZ);
            GetSize(hY, @nBus);
            SetLength(ColPtr, nBus + 1);
            SetLength(RowIdx, nNZ);
            SetLength(cVals, nNZ);
            GetCompressedMatrix(hY, nBus + 1, nNZ, @ColPtr[0], @RowIdx[0], @cVals[0]);

      // allocate a square matrix
            NValues := SQR(NumNodes);
            Result := VarArrayCreate([0, 2 * NValues - 1], varDouble);  // Make variant array for complex

      // the new way, first set all elements to zero
            for iV := 0 to 2 * NValues - 1 do
                Result[iV] := 0.0;
      // then back-fill the non-zero values
            for j := 0 to nBus - 1 do
            begin /// the zero-based column
                for p := ColPtr[j] to ColPtr[j + 1] - 1 do
                begin
                    i := RowIdx[p];  // the zero-based row
                    iV := i * nBus + j; // the zero-based, row-wise, complex result index
                    Result[iV * 2] := cVals[p].re;
                    Result[iV * 2 + 1] := cVals[p].im;
                end;
            end;
        end;

end;

function TCircuit.Get_CtrlQueue: ICtrlQueue;
begin
    Result := FCtrlQueue as ICtrlQueue;
end;

function TCircuit.Get_AllBusDistances: Olevariant;
{Return distances from each bus to its parent energymeter in an array that aligns with the buslist}
var
    i: Integer;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            Result := VarArrayCreate([0, NumBuses - 1], varDouble);
            for i := 0 to NumBuses - 1 do
            begin
                Result[i] := Buses^[i + 1].DistFromMeter;
            end;
        end
    else
        Result := VarArrayCreate([0, 0], varDouble);

end;

function TCircuit.Get_AllNodeDistances: Olevariant;
{Return distance from each Node back to parent EnergyMeter}
{Array sequence is same as all bus Vmag and Vmagpu}
var
    i, j, k: Integer;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            Result := VarArrayCreate([0, NumNodes - 1], varDouble);
            k := 0;
            for i := 1 to NumBuses do
            begin
                for j := 1 to Buses^[i].NumNodesThisBus do
                begin
                    Result[k] := Buses^[i].DistFromMeter;
                    Inc(k);
                end;
            end;
        end
    else
        Result := VarArrayCreate([0, 0], varDouble);

end;

function TCircuit.Get_AllNodeDistancesByPhase(Phase: Integer): Olevariant;
var
    i, k, NodeIdx: Integer;
    Temp: pDoubleArray;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
       // Make a Temporary Array big enough to hold all nodes
            Temp := AllocMem(SizeOF(Temp^[1]) * NumNodes);

       // Find nodes connected to specified phase
            k := 0;
            for i := 1 to NumBuses do
            begin
                NodeIdx := Buses^[i].FindIdx(Phase);
                if NodeIdx > 0 then   // Node found with this phase number
                begin
                    Inc(k);
                    Temp^[k] := Buses^[i].DistFromMeter;
                end;
            end;

       // Assign to result and free temp array
            Result := VarArrayCreate([0, k - 1], varDouble);
            for i := 0 to k - 1 do
                Result[i] := Temp^[i + 1];

            Freemem(Temp, SizeOF(Temp^[1]) * NumNodes);
        end
    else
        Result := VarArrayCreate([0, 0], varDouble);

end;

function TCircuit.Get_AllNodeVmagByPhase(Phase: Integer): Olevariant;
var
    i, k, NodeIdx: Integer;
    Temp: pDoubleArray;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
       // Make a Temporary Array big enough to hold all nodes
            Temp := AllocMem(SizeOF(Temp^[1]) * NumNodes);

       // Find nodes connected to specified phase
            k := 0;
            for i := 1 to NumBuses do
            begin
                NodeIdx := Buses^[i].FindIdx(Phase);
                if NodeIdx > 0 then   // Node found with this phase number
                begin
                    Inc(k);
                    Temp^[k] := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(NodeIdx)]);
                end;
            end;

       // Assign to result and free temp array
            Result := VarArrayCreate([0, k - 1], varDouble);
            for i := 0 to k - 1 do
                Result[i] := Temp^[i + 1];

            Freemem(Temp, SizeOF(Temp^[1]) * NumNodes);
        end
    else
        Result := VarArrayCreate([0, 0], varDouble);

end;

function TCircuit.Get_AllNodeVmagPUByPhase(Phase: Integer): Olevariant;
var
    i, k, NodeIdx: Integer;
    Temp: pDoubleArray;
    BaseFactor: Double;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
       // Make a Temporary Array big enough to hold all nodes
            Temp := AllocMem(SizeOF(Temp^[1]) * NumNodes);

       // Find nodes connected to specified phase
            k := 0;
            for i := 1 to NumBuses do
            begin
                NodeIdx := Buses^[i].FindIdx(Phase);
                if NodeIdx > 0 then   // Node found with this phase number
                begin
                    if Buses^[i].kVBase > 0.0 then
                        BaseFactor := 1000.0 * Buses^[i].kVBase
                    else
                        BaseFactor := 1.0;
                    Inc(k);
                    Temp^[k] := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(NodeIdx)]) / Basefactor;
                end;
            end;

       // Assign to result and free temp array
            Result := VarArrayCreate([0, k - 1], varDouble);
            for i := 0 to k - 1 do
                Result[i] := Temp^[i + 1];

            Freemem(Temp, SizeOF(Temp^[1]) * NumNodes);
        end
    else
        Result := VarArrayCreate([0, 0], varDouble);

end;

function TCircuit.Get_AllNodeNamesByPhase(Phase: Integer): Olevariant;
var
    i, k, NodeIdx: Integer;
    Temp: pStringArray;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
       // Make a Temporary Array big enough to hold all nodes
            Temp := AllocStringArray(NumNodes);

       // Find nodes connected to specified phase
            k := 0;
            for i := 1 to NumBuses do
            begin
                NodeIdx := Buses^[i].FindIdx(Phase);
                if NodeIdx > 0 then   // Node found with this phase number
                begin
                    Inc(k);
                    Temp^[k] := Format('%s.%d', [BusList.Get(i), Phase]);
                end;
            end;

       // Assign to result and free temp array
            Result := VarArrayCreate([0, k - 1], varOleStr);
            for i := 0 to k - 1 do
                Result[i] := Temp^[i + 1];

            FreeStringArray(Temp, NumNodes);
        end
    else
        Result := VarArrayCreate([0, 0], varOleStr);

end;


function TCircuit.Get_Loads: ILoads;
begin
    Result := FLoads as ILoads;
end;

function TCircuit.SetActiveClass(const ClassName: Widestring): Integer;
var
    DevClassIndex: Integer;

begin
    Result := 0;
    DevClassIndex := ClassNames[ActiveActor].Find(ClassName);
    if DevClassIndex = 0 then
    begin
        DoSimplemsg('Error: Class ' + ClassName + ' not found.', 5016);
        Exit;
    end;

    LastClassReferenced[ActiveActor] := DevClassIndex;
    ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
    Result := LastClassReferenced[ActiveActor];
end;

function TCircuit.FirstElement: Integer;
{ Sets first  element in active class to be active}

begin

    Result := 0;
    if (ActiveCircuit[ActiveActor] <> NIL) and Assigned(ActiveDSSClass) then
    begin
        Result := ActiveDSSClass[ActiveActor].First;
    end
    else
        Result := 0;

end;

function TCircuit.NextElement: Integer;
{ Sets next  element in active class to be active}

begin

    Result := 0;
    if (ActiveCircuit[ActiveActor] <> NIL) and Assigned(ActiveDSSClass[ActiveActor]) then
    begin
        Result := ActiveDSSClass[ActiveActor].Next;
    end
    else
        Result := 0;

end;

function TCircuit.Get_ActiveCktElement: ICktElement;
begin
    Result := FCktElement as ICktElement;
end;

function TCircuit.Get_ActiveDSSElement: IDSSElement;
begin
    Result := FDSSElement as IDSSElement;
end;

function TCircuit.Get_ActiveClass: IActiveClass;
begin
    Result := FActiveClass as IActiveClass;
end;

function TCircuit.Get_CapControls: ICapControls;
begin
    Result := FCapControls as ICapControls;
end;

function TCircuit.Get_RegControls: IRegControls;
begin
    Result := FRegControls as IRegControls;
end;

function TCircuit.Get_SwtControls: ISwtControls;
begin
    Result := FSwtControls as ISwtControls;
end;

function TCircuit.Get_Transformers: ITransformers;
begin
    Result := FTransformers as ITransformers;
end;

function TCircuit.Get_Capacitors: ICapacitors;
begin
    Result := FCapacitors as ICapacitors;
end;

function TCircuit.Get_Topology: ITopology;
begin
    Result := FTopology as ITopology;
end;

function TCircuit.Get_Sensors: ISensors;
begin
    Result := FSensors as ISensors;
end;

procedure TCircuit.UpdateStorage;
begin
    StorageClass[ActiveActor].UpdateAll(ActiveActor);
end;

function TCircuit.Get_ParentPDElement: Integer;
// Make parent PD element the active element if it exists
var
    ActivePDElement: TPDElement;
begin

    Result := 0;
    with ActiveCircuit[ActiveActor] do
        if ActiveCktElement is TPDElement then
        begin
            ActivePDElement := TPDElement(ActiveCktElement).ParentPDElement;
            if ActivePDElement <> NIL then
            begin
                ActiveCktElement := ActivePDElement;
                Result := ActivePDElement.ClassIndex;  // should be >0
            end;
        end;

end;

function TCircuit.Get_XYCurves: IXYCurves;
begin
    Result := FXYCurves as IXYCurves;
end;

function TCircuit.Get_PDElements: IPDElements;
begin
    Result := FPDElements as IPDElements;
end;

function TCircuit.Get_Reclosers: IReclosers;
begin
    Result := FReclosers as IReclosers;
end;

function TCircuit.Get_Relays: IRelays;
begin
    Result := FRelays as IRelays;
end;

function TCircuit.Get_LoadShapes: ILoadShapes;
begin
    Result := FLoadShapes as ILoadShapes;     // Loadshapes interface
end;

function TCircuit.Get_DSSim_Coms: IDSSimComs; //Points to the class
begin
    Result := FDSSim_Coms as IDSSimComs;
end;

function TCircuit.Get_Fuses: Fuses;
begin
    Result := FFuses as IFuses;
end;

function TCircuit.Get_Isources: IISources;
begin
    Result := FIsources as IISources;
end;


procedure TCircuit.EndOfTimeStepUpdate;
begin
    EndOfTimeStepCleanup(ActiveActor);
end;

function TCircuit.Get_YNodeOrder: Olevariant;
var
    i, k: Integer;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            Result := VarArrayCreate([0, NumNodes - 1], varOleStr);
            k := 0;
            for i := 1 to NumNodes do
            begin
                with MapNodeToBus^[i] do
                    Result[k] := Format('%s.%-d', [Uppercase(BusList.Get(Busref)), NodeNum]);
                Inc(k);
            end;
        end
    else
        Result := VarArrayCreate([0, 0], varOleStr);

end;


function TCircuit.Get_YCurrents: Olevariant;
var
    i, k: Integer;
    Curr: Complex;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            Result := VarArrayCreate([0, 2 * NumNodes - 1], varDouble);
            k := 0;
            for i := 1 to NumNodes do
            begin
                Curr := ActiveCircuit[ActiveActor].Solution.Currents^[i];
                Result[k] := Curr.re;
                Inc(k);
                Result[k] := Curr.im;
                Inc(k);
            end;
        end
    else
        Result := VarArrayCreate([0, 0], varDouble);

end;

function TCircuit.Get_YNodeVarray: Olevariant;
var
    i, k: Integer;
    Volts: Complex;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            Result := VarArrayCreate([0, 2 * NumNodes - 1], varDouble);
            k := 0;
            for i := 1 to NumNodes do
            begin
                Volts := ActiveCircuit[ActiveActor].Solution.NodeV^[i];
                Result[k] := Volts.re;
                Inc(k);
                Result[k] := Volts.im;
                Inc(k);
            end;
        end
    else
        Result := VarArrayCreate([0, 0], varDouble);

end;

function TCircuit.Get_PVSystems: IPVSystems;
begin
    Result := FPVSystems as IPVSystems;
end;

function TCircuit.Get_Vsources: IVsources;
begin
    Result := FVsources as IVSources;
end;

function TCircuit.Get_Parallel: IParallel;
begin
    Result := FParallel as IParallel;
end;

function TCircuit.Get_LineCodes: ILineCodes;
begin
    Result := FLineCodes as ILineCodes;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TCircuit, Class_Circuit, ciInternal, tmApartment);
end.
