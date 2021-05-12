unit CAPI_Circuit;

interface

uses
    CAPI_Utils;

function Circuit_Get_Name(): PAnsiChar; CDECL;
function Circuit_Get_NumBuses(): Integer; CDECL;
function Circuit_Get_NumCktElements(): Integer; CDECL;
function Circuit_Get_NumNodes(): Integer; CDECL;
procedure Circuit_Get_LineLosses(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Circuit_Get_LineLosses_GR(); CDECL;
procedure Circuit_Get_Losses(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Circuit_Get_Losses_GR(); CDECL;
procedure Circuit_Get_AllBusVmag(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Circuit_Get_AllBusVmag_GR(); CDECL;
procedure Circuit_Get_AllBusVolts(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Circuit_Get_AllBusVolts_GR(); CDECL;
procedure Circuit_Get_AllElementNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Circuit_Get_AllElementNames_GR(); CDECL;
procedure Circuit_Get_SubstationLosses(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Circuit_Get_SubstationLosses_GR(); CDECL;
procedure Circuit_Get_TotalPower(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Circuit_Get_TotalPower_GR(); CDECL;
procedure Circuit_Disable(const Name: PAnsiChar); CDECL;
procedure Circuit_Enable(const Name: PAnsiChar); CDECL;
function Circuit_FirstPCElement(): Integer; CDECL;
function Circuit_FirstPDElement(): Integer; CDECL;
function Circuit_NextPCElement(): Integer; CDECL;
function Circuit_NextPDElement(): Integer; CDECL;
procedure Circuit_Get_AllBusNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Circuit_Get_AllBusNames_GR(); CDECL;
procedure Circuit_Get_AllElementLosses(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Circuit_Get_AllElementLosses_GR(); CDECL;
procedure Circuit_Sample(); CDECL;
procedure Circuit_SaveSample(); CDECL;
function Circuit_SetActiveElement(const FullName: PAnsiChar): Integer; CDECL;
function Circuit_Capacity(Start, Increment: Double): Double; CDECL;
procedure Circuit_Get_AllBusVmagPu(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Circuit_Get_AllBusVmagPu_GR(); CDECL;
function Circuit_SetActiveBus(const BusName: PAnsiChar): Integer; CDECL;
function Circuit_SetActiveBusi(BusIndex: Integer): Integer; CDECL;
procedure Circuit_Get_AllNodeNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Circuit_Get_AllNodeNames_GR(); CDECL;
procedure Circuit_Get_SystemY(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Circuit_Get_SystemY_GR(); CDECL;
procedure Circuit_Get_AllBusDistances(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Circuit_Get_AllBusDistances_GR(); CDECL;
procedure Circuit_Get_AllNodeDistances(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Circuit_Get_AllNodeDistances_GR(); CDECL;
procedure Circuit_Get_AllNodeDistancesByPhase(var ResultPtr: PDouble; ResultCount: PAPISize; Phase: Integer); CDECL;
procedure Circuit_Get_AllNodeDistancesByPhase_GR(Phase: Integer); CDECL;
procedure Circuit_Get_AllNodeVmagByPhase(var ResultPtr: PDouble; ResultCount: PAPISize; Phase: Integer); CDECL;
procedure Circuit_Get_AllNodeVmagByPhase_GR(Phase: Integer); CDECL;
procedure Circuit_Get_AllNodeVmagPUByPhase(var ResultPtr: PDouble; ResultCount: PAPISize; Phase: Integer); CDECL;
procedure Circuit_Get_AllNodeVmagPUByPhase_GR(Phase: Integer); CDECL;
procedure Circuit_Get_AllNodeNamesByPhase(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; Phase: Integer); CDECL;
procedure Circuit_Get_AllNodeNamesByPhase_GR(Phase: Integer); CDECL;
function Circuit_SetActiveClass(const ClassName: PAnsiChar): Integer; CDECL;
function Circuit_FirstElement(): Integer; CDECL;
function Circuit_NextElement(): Integer; CDECL;
procedure Circuit_UpdateStorage(); CDECL;
function Circuit_Get_ParentPDElement(): Integer; CDECL;
procedure Circuit_EndOfTimeStepUpdate(); CDECL;
procedure Circuit_Get_YNodeOrder(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Circuit_Get_YNodeOrder_GR(); CDECL;
procedure Circuit_Get_YCurrents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Circuit_Get_YCurrents_GR(); CDECL;
procedure Circuit_Get_YNodeVarray(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Circuit_Get_YNodeVarray_GR(); CDECL;
procedure Circuit_SetCktElementIndex(const Value: Integer); CDECL;
procedure Circuit_SetCktElementName(const Value: PAnsiChar); CDECL;

// Extensions
procedure Circuit_Get_ElementLosses(var ResultPtr: PDouble; ResultCount: PAPISize; ElementsPtr: PInteger; ElementsCount: TAPISize); CDECL;
procedure Circuit_Get_ElementLosses_GR(ElementsPtr: PInteger; ElementsCount: TAPISize); CDECL;

implementation

uses
    CAPI_Constants,
    DSSClassDefs,
    DSSGlobals,
    Line,
    UComplex,
    sysutils,
    CktElement,
    DSSObject,
    DSSClass,
    Transformer,
    PCElement,
    PDElement,
    Monitor,
    EnergyMeter,
    YMatrix,
    Utilities,
    SolutionAlgs,
    KLUSolve;

//------------------------------------------------------------------------------
function Circuit_Get_Name(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if InvalidCircuit then
        Exit;
    Result := DSS_GetAsPAnsiChar(ActiveCircuit.Name)
end;
//------------------------------------------------------------------------------
function Circuit_Get_NumBuses(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.NumBuses
end;
//------------------------------------------------------------------------------
function Circuit_Get_NumCktElements(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.NumDevices;
end;
//------------------------------------------------------------------------------
function Circuit_Get_NumNodes(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.NumNodes;
end;
//------------------------------------------------------------------------------
procedure Circuit_Get_LineLosses(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray;
    pLine: TLineObj;
    Loss: Complex;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
    if MissingSolution then
        Exit;
    with ActiveCircuit do
    begin
        pLine := Lines.First;
        Loss := Cmplx(0.0, 0.0);
        while pLine <> NIL do
        begin
            CAccum(Loss, pLine.Losses);
            pLine := Lines.Next;
        end;
        Result[0] := Loss.re * 0.001;
        Result[1] := Loss.im * 0.001;
    end;
end;

procedure Circuit_Get_LineLosses_GR(); CDECL;
// Same as Circuit_Get_LineLosses but uses global result (GR) pointers
begin
    Circuit_Get_LineLosses(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Circuit_Get_Losses(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray;
    LossValue: complex;
begin
    if InvalidCircuit then
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
    LossValue := ActiveCircuit.Losses;
    Result[0] := LossValue.re;
    Result[1] := LossValue.im;
end;

procedure Circuit_Get_Losses_GR(); CDECL;
// Same as Circuit_Get_Losses but uses global result (GR) pointers
begin
    Circuit_Get_Losses(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Circuit_Get_AllBusVmag(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray;
    i, j, k: Integer;

begin
    if MissingSolution then
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;
    with ActiveCircuit do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, NumNodes);
        k := 0;
        for i := 1 to NumBuses do
        begin
            for j := 1 to Buses^[i].NumNodesThisBus do
            begin
                Result[k] := Cabs(ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(j)]);
                Inc(k);
            end;
        end;
    end
end;

procedure Circuit_Get_AllBusVmag_GR(); CDECL;
// Same as Circuit_Get_AllBusVmag but uses global result (GR) pointers
begin
    Circuit_Get_AllBusVmag(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Circuit_Get_AllBusVolts(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray;
    i, j, k: Integer;
    Volts: Complex;

begin
    if MissingSolution then
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;
    with ActiveCircuit do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2 * NumNodes - 1) + 1);
        k := 0;
        for i := 1 to NumBuses do
        begin
            for j := 1 to Buses^[i].NumNodesThisBus do
            begin
                Volts := ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(j)];
                Result[k] := Volts.re;
                Inc(k);
                Result[k] := Volts.im;
                Inc(k);
            end;
        end;
    end
end;

procedure Circuit_Get_AllBusVolts_GR(); CDECL;
// Same as Circuit_Get_AllBusVolts but uses global result (GR) pointers
begin
    Circuit_Get_AllBusVolts(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Circuit_Get_AllElementNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray;
    i: Integer;

begin
    if InvalidCircuit then
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
        Exit;
    end;
    
    with ActiveCircuit do
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, NumDevices);
        for i := 1 to NumDevices do
        begin
            with  TDSSCktElement(CktElements.Get(i)) do
                Result[i - 1] := DSS_CopyStringAsPChar(ParentClass.Name + '.' + Name);
        end;
    end
end;

procedure Circuit_Get_AllElementNames_GR(); CDECL;
// Same as Circuit_Get_AllElementNames but uses global result (GR) pointers
begin
    Circuit_Get_AllElementNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
procedure Circuit_Get_SubstationLosses(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray;
    pTransf: TTransfObj;
    Loss: Complex;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
    if MissingSolution then 
        Exit;
    
    with ActiveCircuit do
    begin
        pTransf := Transformers.First;
        Loss := Cmplx(0.0, 0.0);
        while pTransf <> NIL do
        begin
            if pTransf.Issubstation then
                Caccum(Loss, pTransf.Losses);
            pTransf := Transformers.Next;
        end;
        Result[0] := Loss.re * 0.001;
        Result[1] := Loss.im * 0.001;
    end
end;

procedure Circuit_Get_SubstationLosses_GR(); CDECL;
// Same as Circuit_Get_SubstationLosses but uses global result (GR) pointers
begin
    Circuit_Get_SubstationLosses(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Circuit_Get_TotalPower(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Total power being consumed in the circuit.
// Add up all power being contributed by sources.
// Returns result in kW
var
    Result: PDoubleArray;
    pCktElem: TDSSCktElement;
    cPower: Complex;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
    if MissingSolution then
        Exit;
    
    with ActiveCircuit do
    begin
        pCktElem := Sources.First;
        cPower := Cmplx(0.0, 0.0);
        while pCktElem <> NIL do
        begin
            CAccum(cPower, pcktElem.Power[1]);
            pCktElem := Sources.Next;
        end;
        Result[0] := cPower.re * 0.001;
        Result[1] := cPower.im * 0.001;
    end
end;

procedure Circuit_Get_TotalPower_GR(); CDECL;
// Same as Circuit_Get_TotalPower but uses global result (GR) pointers
begin
    Circuit_Get_TotalPower(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Circuit_Disable(const Name: PAnsiChar); CDECL;
begin
    if InvalidCircuit then
        Exit;
        
    with ActiveCircuit do
    begin
        SetElementActive(Name);
        if ActiveCktElement <> NIL then
            ActiveCktElement.Enabled := FALSE;
    end;
end;
//------------------------------------------------------------------------------
procedure Circuit_Enable(const Name: PAnsiChar); CDECL;
begin
    if InvalidCircuit then
        Exit;

    with ActiveCircuit do
    begin
        SetElementActive(Name);
        if ActiveCktElement <> NIL then
            ActiveCktElement.Enabled := TRUE;
    end;
end;
//------------------------------------------------------------------------------
function Circuit_FirstPCElement(): Integer; CDECL;
var
    p: TDSSCktElement;

{ Returns first enabled element}
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
        
    p := ActiveCircuit.PCElements.First;
    if p = NIL then
        Exit;

    repeat
        if p.enabled then
        begin
            Result := 1;
            ActiveCircuit.ActiveCktElement := p;
        end
        else
            p := ActiveCircuit.PCElements.Next;

    until (Result = 1) or (p = NIL);
end;
//------------------------------------------------------------------------------
function Circuit_FirstPDElement(): Integer; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
        
    ActivePDElement := ActiveCircuit.PDElements.First;
    if ActivePDElement = NIL then
        Exit;
        
    repeat
        if ActivePDElement.enabled then
        begin
            Result := 1;
            ActiveCircuit.ActiveCktElement := ActivePDElement;
        end
        else
            ActivePDElement := ActiveCircuit.PDElements.Next;
    until (Result = 1) or (ActivePDELement = NIL);
end;
//------------------------------------------------------------------------------
function Circuit_NextPCElement(): Integer; CDECL;
var
    p: TDSSCktElement;

begin
    Result := 0;
    if InvalidCircuit then
        Exit;
        
    p := ActiveCircuit.PCElements.Next;
    if p = NIL then
        Exit;
        
    repeat
        if p.enabled then
        begin
            Result := ActiveCircuit.PCElements.ActiveIndex;
            ActiveCircuit.ActiveCktElement := p;
        end
        else
            p := ActiveCircuit.PCElements.Next;
    until (Result > 0) or (p = NIL);
end;
//------------------------------------------------------------------------------
function Circuit_NextPDElement(): Integer; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
        
    ActivePDElement := ActiveCircuit.PDElements.Next;
    if ActivePDElement = NIL then
        Exit;
        
    repeat
        if ActivePDElement.Enabled then
        begin
            Result := ActiveCircuit.PDElements.ActiveIndex;
            ActiveCircuit.ActiveCktElement := ActivePDElement;
        end
        else
            ActivePDElement := ActiveCircuit.PDElements.Next;
    until (Result > 0) or (ActivePDElement = NIL);
end;
//------------------------------------------------------------------------------
procedure Circuit_Get_AllBusNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
// Just Bus names      modified 2/7/03
var
    Result: PPAnsiCharArray;
    i: Integer;

begin
    if InvalidCircuit then
    begin
        DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
        Exit;
    end;
    with ActiveCircuit do
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, NumBuses);
        for i := 0 to NumBuses - 1 do
        begin
            Result[i] := DSS_CopyStringAsPChar(BusList.NameOfIndex(i + 1));
        end;
    end
end;

procedure Circuit_Get_AllBusNames_GR(); CDECL;
// Same as Circuit_Get_AllBusNames but uses global result (GR) pointers
begin
    Circuit_Get_AllBusNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
procedure Circuit_Get_AllElementLosses(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray;
    CResultPtr: pComplex;
    pCktElem: TDSSCktElement;
    i: Integer;
begin
    if MissingSolution then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;
    with ActiveCircuit do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NumDevices);
        CResultPtr := pComplex(ResultPtr);
        pCktElem := CktElements.First;
        while pCktElem <> NIL do
        begin
            CResultPtr^ := pCktElem.Losses;
            Inc(CResultPtr);
            pCktElem := CktElements.Next;
        end;
        for i := 0 to 2*NumDevices - 1 do
            Result[i] := Result[i] * 0.001;
    end
end;

procedure Circuit_Get_AllElementLosses_GR(); CDECL;
// Same as Circuit_Get_AllElementLosses but uses global result (GR) pointers
begin
    Circuit_Get_AllElementLosses(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Circuit_Sample(); CDECL;
// Sample all meters and monitors

begin

    MonitorClass.SampleAll;

    EnergyMeterClass.SampleAll;

end;
//------------------------------------------------------------------------------
procedure Circuit_SaveSample(); CDECL;
// Save all meters and monitors registers and buffers

var
    Mon: TDSSMonitor;
    Mtr: TEnergyMeter;

begin
    Mon := DSSClassList.Get(ClassNames.Find('monitor'));
    Mon.SaveAll;

    Mtr := DSSClassList.Get(ClassNames.Find('energymeter'));
    Mtr.SaveAll;
end;
//------------------------------------------------------------------------------
function Circuit_SetActiveElement(const FullName: PAnsiChar): Integer; CDECL;
begin
    Result := -1;
    if InvalidCircuit then
    begin
        DoSimpleMsg('Create a circuit before trying to set an element active!', 5015);
        Exit;
    end;

    Result := ActiveCircuit.SetElementActive(FullName) - 1;   // make zero based to be compatible with collections and variant arrays
end;
//------------------------------------------------------------------------------
function Circuit_Capacity(Start, Increment: Double): Double; CDECL;
begin
    if InvalidCircuit then
    begin
        Result := 0.0;
        Exit;
    end;
    with ActiveCircuit do
    begin
        CapacityStart := Start;
        CapacityIncrement := Increment;
        if ComputeCapacity then
            Result := RegisterTotals[3] + RegisterTotals[19]
        else
            Result := 0.0;
    end
end;
//------------------------------------------------------------------------------
procedure Circuit_Get_AllBusVmagPu(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray;
    i, j, k: Integer;
    Volts, BaseFactor: Double;
begin
    if MissingSolution then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;
    with ActiveCircuit do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, NumNodes);
        k := 0;
        for i := 1 to NumBuses do
        begin
            if Buses^[i].kVBase > 0.0 then
                BaseFactor := 1000.0 * Buses^[i].kVBase
            else
                BaseFactor := 1.0;
            for j := 1 to Buses^[i].NumNodesThisBus do
            begin
                Volts := Cabs(ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(j)]);
                Result[k] := Volts / BaseFactor;
                Inc(k);
            end;
        end;
    end
end;

procedure Circuit_Get_AllBusVmagPu_GR(); CDECL;
// Same as Circuit_Get_AllBusVmagPu but uses global result (GR) pointers
begin
    Circuit_Get_AllBusVmagPu(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function Circuit_SetActiveBus(const BusName: PAnsiChar): Integer; CDECL;
begin
    DSSGlobals.SetActiveBus(StripExtension(BusName));
    if Assigned(Activecircuit) then
        Result := ActiveCircuit.ActiveBusIndex - 1
    else
        Result := -1;
end;
//------------------------------------------------------------------------------
function Circuit_SetActiveBusi(BusIndex: Integer): Integer; CDECL;
{ BusIndex is Zero Based}
begin
    Result := -1;   // Signifies Error
    if InvalidCircuit then
        Exit;

    with ActiveCircuit do
    begin
        if (BusIndex >= 0) and (BusIndex < Numbuses) then
        begin
            ActiveBusIndex := BusIndex + 1;
            Result := 0;
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Circuit_Get_AllNodeNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
// Return all node names (Busname.nodenumber)
// Same order as current solution array.
var
    Result: PPAnsiCharArray;
    i, j, k: Integer;
    BusName: String;
begin
    if InvalidCircuit then
    begin
        DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1); 
        Exit;
    end;
    with ActiveCircuit do
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, NumNodes);
        k := 0;
        for i := 1 to NumBuses do
        begin
            BusName := BusList.NameOfIndex(i);
            for j := 1 to Buses^[i].NumNodesThisBus do
            begin
                Result[k] := DSS_CopyStringAsPChar(BusName + '.' + IntToStr(Buses^[i].GetNum(j)));
                Inc(k);
            end;
        end;
    end;
end;

// this calls the compressed column
procedure Circuit_Get_AllNodeNames_GR(); CDECL;
// Same as Circuit_Get_AllNodeNames but uses global result (GR) pointers
begin
    Circuit_Get_AllNodeNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
procedure Circuit_Get_SystemY(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
{Return System Y matrix, complex form}

var
    Result: PDoubleArray;
    iV: Longword;
    i, j, p: Longword;
    NValues: Longword;
    hY: NativeUint;
    nBus, nNZ: Longword;
    ColPtr, RowIdx: array of Longword;
    cVals: array of Complex;

begin
    { Return zero length Array if no circuit or no Y matrix}
    if (InvalidCircuit) or (ActiveCircuit.Solution.hY = 0) then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;
    with ActiveCircuit do
    begin
        hY := ActiveCircuit.Solution.hY;

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
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NValues);  // Make array for complex

        // DSS_RecreateArray_PDouble already zero-fills the array
            
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

procedure Circuit_Get_SystemY_GR(); CDECL;
// Same as Circuit_Get_SystemY but uses global result (GR) pointers
begin
    Circuit_Get_SystemY(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Circuit_Get_AllBusDistances(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
{Return distances from each bus to its parent energymeter in an array that aligns with the buslist}
var
    Result: PDoubleArray;
    i: Integer;

begin
    if InvalidCircuit then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;
    with ActiveCircuit do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, NumBuses);
        for i := 0 to NumBuses - 1 do
        begin
            Result[i] := Buses^[i + 1].DistFromMeter;
        end;
    end;
end;

procedure Circuit_Get_AllBusDistances_GR(); CDECL;
// Same as Circuit_Get_AllBusDistances but uses global result (GR) pointers
begin
    Circuit_Get_AllBusDistances(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Circuit_Get_AllNodeDistances(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
{Return distance from each Node back to parent EnergyMeter}
{Array sequence is same as all bus Vmag and Vmagpu}
var
    Result: PDoubleArray;
    i, j, k: Integer;
begin
    if InvalidCircuit then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;
    with ActiveCircuit do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, NumNodes);
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
end;

procedure Circuit_Get_AllNodeDistances_GR(); CDECL;
// Same as Circuit_Get_AllNodeDistances but uses global result (GR) pointers
begin
    Circuit_Get_AllNodeDistances(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Circuit_Get_AllNodeDistancesByPhase(var ResultPtr: PDouble; ResultCount: PAPISize; Phase: Integer); CDECL;
var
    Result: PDoubleArray;
    i, k, NodeIdx: Integer;
begin
    if InvalidCircuit then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;
    
    with ActiveCircuit do
    begin
        // First allocate as many elements as nodes
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, NumNodes);

        // Find nodes connected to specified phase
        k := 0;
        for i := 1 to NumBuses do
        begin
            NodeIdx := Buses^[i].FindIdx(Phase);
            if NodeIdx > 0 then   // Node found with this phase number
            begin
                Result[k] := Buses^[i].DistFromMeter;
                Inc(k);
            end;
        end;

        // Finally "resize" the end result
        ResultCount[0] := k;
    end
end;

procedure Circuit_Get_AllNodeDistancesByPhase_GR(Phase: Integer); CDECL;
// Same as Circuit_Get_AllNodeDistancesByPhase but uses global result (GR) pointers
begin
    Circuit_Get_AllNodeDistancesByPhase(GR_DataPtr_PDouble, GR_CountPtr_PDouble, Phase)
end;

//------------------------------------------------------------------------------
procedure Circuit_Get_AllNodeVmagByPhase(var ResultPtr: PDouble; ResultCount: PAPISize; Phase: Integer); CDECL;
var
    Result: PDoubleArray;
    i, k, NodeIdx: Integer;
begin
    if MissingSolution then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;
    with ActiveCircuit do
    begin
        // First allocate as many elements as nodes
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, NumNodes);

        // Find nodes connected to specified phase
        k := 0;
        for i := 1 to NumBuses do
        begin
            NodeIdx := Buses^[i].FindIdx(Phase);
            if NodeIdx > 0 then   // Node found with this phase number
            begin
                Result[k] := Cabs(ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(NodeIdx)]);
                Inc(k);
            end;
        end;

        // Finally "resize" the end result
        ResultCount[0] := k;
    end
end;

procedure Circuit_Get_AllNodeVmagByPhase_GR(Phase: Integer); CDECL;
// Same as Circuit_Get_AllNodeVmagByPhase but uses global result (GR) pointers
begin
    Circuit_Get_AllNodeVmagByPhase(GR_DataPtr_PDouble, GR_CountPtr_PDouble, Phase)
end;

//------------------------------------------------------------------------------
procedure Circuit_Get_AllNodeVmagPUByPhase(var ResultPtr: PDouble; ResultCount: PAPISize; Phase: Integer); CDECL;
var
    Result: PDoubleArray;
    i, k, NodeIdx: Integer;
    BaseFactor: Double;
begin
    if MissingSolution then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;
    
    with ActiveCircuit do
    begin
        // First allocate as many elements as nodes
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, NumNodes);

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
                Result[k] := Cabs(ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(NodeIdx)]) / Basefactor;
                Inc(k);
            end;
        end;

        // Finally "resize" the end result
        ResultCount[0] := k;
    end
end;

procedure Circuit_Get_AllNodeVmagPUByPhase_GR(Phase: Integer); CDECL;
// Same as Circuit_Get_AllNodeVmagPUByPhase but uses global result (GR) pointers
begin
    Circuit_Get_AllNodeVmagPUByPhase(GR_DataPtr_PDouble, GR_CountPtr_PDouble, Phase)
end;

//------------------------------------------------------------------------------
procedure Circuit_Get_AllNodeNamesByPhase(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; Phase: Integer); CDECL;
var
    Result: PPAnsiCharArray;
    i, k, NodeIdx: Integer;
    Temp: Array of String;

begin
    if InvalidCircuit then
    begin
        DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
        Exit;
    end;
    
    with ActiveCircuit do
    begin
        // Make a Temporary Array big enough to hold all nodes
        SetLength(Temp, NumNodes);

        // Find nodes connected to specified phase
        k := 0;
        for i := 1 to NumBuses do
        begin
            NodeIdx := Buses^[i].FindIdx(Phase);
            if NodeIdx > 0 then   // Node found with this phase number
            begin
                Temp[k] := Format('%s.%d', [BusList.NameOfIndex(i), Phase]);
                Inc(k);
            end;
        end;

        // Assign to result and free temp array
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, k);
        for i := 0 to k - 1 do
            Result[i] := DSS_CopyStringAsPChar(Temp[i]);

        SetLength(Temp, 0);
    end;
end;

procedure Circuit_Get_AllNodeNamesByPhase_GR(Phase: Integer); CDECL;
// Same as Circuit_Get_AllNodeNamesByPhase but uses global result (GR) pointers
begin
    Circuit_Get_AllNodeNamesByPhase(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar, Phase)
end;

//------------------------------------------------------------------------------
function Circuit_SetActiveClass(const ClassName: PAnsiChar): Integer; CDECL;
var
    DevClassIndex: Integer;
begin
    Result := 0;
    DevClassIndex := ClassNames.Find(ClassName);
    if DevClassIndex = 0 then
    begin
        DoSimplemsg('Error: Class ' + ClassName + ' not found.', 5016);
        Exit;
    end;

    LastClassReferenced := DevClassIndex;
    ActiveDSSClass := DSSClassList.Get(LastClassReferenced);
    Result := LastClassReferenced;
end;
//------------------------------------------------------------------------------
function Circuit_FirstElement(): Integer; CDECL;
{ Sets first element in active class to be active}
begin
    Result := 0;
    if (not InvalidCircuit) and Assigned(ActiveDSSClass) then
        Result := ActiveDSSClass.First;
end;
//------------------------------------------------------------------------------
function Circuit_NextElement(): Integer; CDECL;
{ Sets next element in active class to be active}
begin
    Result := 0;
    if (not InvalidCircuit) and Assigned(ActiveDSSClass) then
        Result := ActiveDSSClass.Next;
end;
//------------------------------------------------------------------------------
procedure Circuit_UpdateStorage(); CDECL;
begin
    if DSS_CAPI_LEGACY_MODELS then
        StorageClass.UpdateAll
    else
        Storage2Class.UpdateAll;
end;
//------------------------------------------------------------------------------
function Circuit_Get_ParentPDElement(): Integer; CDECL;
// Make parent PD element the active element if it exists
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;

    with ActiveCircuit do
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
//------------------------------------------------------------------------------
procedure Circuit_EndOfTimeStepUpdate(); CDECL;
begin
    EndOfTimeStepCleanup;
end;
//------------------------------------------------------------------------------
procedure Circuit_Get_YNodeOrder(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray;
    i, k: Integer;

begin
    if InvalidCircuit then
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
        Exit;
    end;
    
    with ActiveCircuit do
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, NumNodes);
        k := 0;
        for i := 1 to NumNodes do
        begin
            with MapNodeToBus^[i] do
                Result[k] := DSS_CopyStringAsPChar(Format('%s.%-d', [Uppercase(BusList.NameOfIndex(Busref)), NodeNum]));
            Inc(k);
        end;
    end

end;

procedure Circuit_Get_YNodeOrder_GR(); CDECL;
// Same as Circuit_Get_YNodeOrder but uses global result (GR) pointers
begin
    Circuit_Get_YNodeOrder(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
procedure Circuit_Get_YCurrents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    CResultPtr: pComplex;
    i: Integer;
begin
    if InvalidCircuit then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;
    with ActiveCircuit do
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NumNodes);
        CResultPtr := pComplex(ResultPtr);
        for i := 1 to NumNodes do
        begin
            CResultPtr^ := ActiveCircuit.Solution.Currents^[i];
            Inc(CResultPtr);
        end;
    end
end;

procedure Circuit_Get_YCurrents_GR(); CDECL;
// Same as Circuit_Get_YCurrents but uses global result (GR) pointers
begin
    Circuit_Get_YCurrents(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Circuit_Get_YNodeVarray(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    CResultPtr: pComplex;
    i: Integer;
begin
    if InvalidCircuit then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;
        with ActiveCircuit do
        begin
            DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NumNodes);
            CResultPtr := pComplex(ResultPtr);
            for i := 1 to NumNodes do
            begin
                CResultPtr^ := ActiveCircuit.Solution.NodeV^[i];
                Inc(CResultPtr);
            end;
        end
end;

procedure Circuit_Get_YNodeVarray_GR(); CDECL;
// Same as Circuit_Get_YNodeVarray but uses global result (GR) pointers
begin
    Circuit_Get_YNodeVarray(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Circuit_SetCktElementIndex(const Value: Integer); CDECL;
begin
    if InvalidCircuit then
    begin
        DoSimpleMsg('Create a circuit before trying to set an element active!', 5015);
        Exit;
    end;

    with ActiveCircuit do
    begin
        if NumDevices > Value then
            ActiveCktElement := CktElements.Get(Value + 1)
        else
            DoSimpleMsg('Invalid CktElement index', 5030);
    end;
end;

procedure Circuit_SetCktElementName(const Value: PAnsiChar); CDECL;
begin
    if Circuit_SetActiveElement(Value) = -1 then
    begin
        DoSimpleMsg(Format('Invalid CktElement name "%s"', [Value]), 5031);
    end;
end;
//------------------------------------------------------------------------------
procedure Circuit_Get_ElementLosses(var ResultPtr: PDouble; ResultCount: PAPISize; ElementsPtr: PInteger; ElementsCount: TAPISize); CDECL;
var
    Result: PDoubleArray;
    Elements: PIntegerArray;
    CResultPtr: pComplex;
    pCktElem: TDSSCktElement;
    i: TAPISize;
begin
    if MissingSolution then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;
    
    Elements := PIntegerArray(ElementsPtr);
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * ElementsCount);
    CResultPtr := pComplex(ResultPtr);

    i := 0;
    while i < ElementsCount do
    begin
        pCktElem := ActiveCircuit.CktElements.Get(Elements[i]);
        CResultPtr^ := pCktElem.Losses;
        Inc(CResultPtr);
        Inc(i);
    end;

    i := 0;
    while i < 2*ElementsCount do
    begin
        Result[i] := Result[i] * 0.001;
        Inc(i);
    end;
end;

procedure Circuit_Get_ElementLosses_GR(ElementsPtr: PInteger; ElementsCount: TAPISize); CDECL;
// Same as Circuit_Get_ElementLosses but uses global result (GR) pointers for output
begin
    Circuit_Get_ElementLosses(GR_DataPtr_PDouble, GR_CountPtr_PDouble, ElementsPtr, ElementsCount)
end;

//------------------------------------------------------------------------------
end.
