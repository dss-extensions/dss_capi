unit DCircuit;

interface

function CircuitI(mode: Longint; arg: Longint): Longint; CDECL;
function CircuitF(mode: Longint; arg1, arg2: Double): Double; CDECL;
function CircuitS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure CircuitV(mode: Longint; out arg: Variant; arg2: Longint); CDECL;

implementation

uses
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
    dialogs,
    YMatrix,
    Variants,
    arrayDef,
    Utilities,
    SolutionAlgs,
    KLUSolve;

function CircuitI(mode: Longint; arg: Longint): Longint; CDECL;

var

    p: TDSSCktElement;
    Mon: TDSSMonitor;
    Mtr: TEnergyMeter;

begin
    Result := 0; // Default return value
    case mode of
        0:
        begin                                             // Circuit.NumCktElements
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].NumDevices;
        end;
        1:
        begin                                             // Circuit.NumBuses
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].NumBuses
            else
                Result := 0;
        end;
        2:
        begin                                             // Circuit.NumNodes
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].NumNodes;
        end;
        3:
        begin                                             // Circuit.FirstPCElement
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
        4:
        begin                                             // Circuit.NextPCElement
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
        5:
        begin                                             // Circuit.FirstPDElement
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                p := ActiveCircuit[ActiveActor].PDElements.First;
                if p <> NIL then
                begin
                    repeat
                        if p.enabled then
                        begin
                            Result := 1;
                            ActiveCircuit[ActiveActor].ActiveCktElement := p;
                        end
                        else
                            p := ActiveCircuit[ActiveActor].PDElements.Next;
                    until (Result = 1) or (p = NIL);
                end
                else
                    Result := 0;
            end;
        end;
        6:
        begin                                             // Circuit.NextPDElement
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                p := ActiveCircuit[ActiveActor].PDElements.Next;
                if p <> NIL then
                begin
                    repeat
                        if p.Enabled then
                        begin
                            Result := ActiveCircuit[ActiveActor].PDElements.ActiveIndex;
                            ActiveCircuit[ActiveActor].ActiveCktElement := p;
                        end
                        else
                            p := ActiveCircuit[ActiveActor].PDElements.Next;
                    until (Result > 0) or (p = NIL);
                end
                else
                begin
                    Result := 0;
                end;
            end;
        end;
        7:
        begin                                             // Circuit.Sample
            MonitorClass[ActiveActor].SampleAll(ActiveActor);
            EnergyMeterClass[ActiveActor].SampleAll(ActiveActor);
            Result := 0;
        end;
        8:
        begin                                             // Circuit.SaveSample
            Mon := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('monitor'));
            Mon.SaveAll(ActiveActor);
            Mtr := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('energymeter'));
            Mtr.SaveAll(ActiveActor);
        end;
        9:
        begin                                             // Circuit.SetActiveBusi
            Result := -1;   // Signifies Error
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    if (arg >= 0) and (arg < Numbuses) then
                    begin
                        ActiveBusIndex := arg + 1;
                        Result := 0;
                    end;
                end;
        end;
        10:
        begin                                            // Circuit.FirstElement
            Result := 0;
            if (ActiveCircuit[ActiveActor] <> NIL) and Assigned(ActiveDSSClass[ActiveActor]) then
            begin
                Result := ActiveDSSClass[ActiveActor].First;
            end
            else
                Result := 0;
        end;
        11:
        begin                                            // Circuit.NextElement
            Result := 0;
            if (ActiveCircuit[ActiveActor] <> NIL) and Assigned(ActiveDSSClass[ActiveActor]) then
            begin
                Result := ActiveDSSClass[ActiveActor].Next;
            end
            else
                Result := 0;
        end;
        12:
        begin                                            // Circuit.UpdateStorage
            StorageClass[ActiveActor].UpdateAll(ActiveActor);
        end;
        13:
        begin                                            // Circuit.ParentPDElement
            Result := 0;
            with ActiveCircuit[ActiveActor] do
                if ActiveCktElement is TPDElement then
                begin
                    p := TPDElement(ActiveCktElement).ParentPDElement;
                    if p <> NIL then
                    begin
                        ActiveCktElement := p;
                        Result := p.ClassIndex;  // should be >0
                    end;
                end;
        end;
        14:
        begin                                            // Circuit.EndOfTimeStepUpdate
            EndOfTimeStepCleanup(ActiveActor);
            Result := 0;
        end
    else
        Result := -1;
    end;

end;

//**************************floating point properties*****************************
function CircuitF(mode: Longint; arg1, arg2: Double): Double; CDECL;
begin
    Result := 0.0; // Default return value
    case mode of
        0:
        begin                                             // Circuit.Capacity
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    CapacityStart := arg1;
                    CapacityIncrement := arg2;
                    if ComputeCapacity(ActiveActor) then
                        Result := RegisterTotals[3] + RegisterTotals[19]
                    else
                        Result := 0.0;
                end
            else
            begin
                Result := 0.0;
            end;
        end
    else
        Result := -1.0;
    end;
end;

//**************************String type properties*****************************
function CircuitS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

var

    DevClassIndex: Integer;

begin
    Result := pAnsiChar(Ansistring('')); // Default return value
    case mode of
        0:
        begin                                             // Circuit.Name
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := pAnsiChar(Ansistring(ActiveCircuit[ActiveActor].Name))
            else
                Result := pAnsiChar(Ansistring(''));
        end;
        1:
        begin                                             // Circuit.Disable
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    SetElementActive(Widestring(arg));
                    if ActiveCktElement <> NIL then
                        ActiveCktElement.Enabled := FALSE;
                end;
        end;
        2:
        begin                                             // Circuit.Enable
            with ActiveCircuit[ActiveActor] do
            begin
                SetElementActive(Widestring(arg));
                if ActiveCktElement <> NIL then
                    ActiveCktElement.Enabled := TRUE;
            end;
        end;
        3:
        begin                                             // Circuit.SetActiveElement
            Result := pAnsiChar(Ansistring('-1'));
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Result := pAnsiChar(Ansistring(Inttostr(ActiveCircuit[ActiveActor].SetElementActive(Widestring(arg)) - 1)));   // make zero based to be compatible with collections and variant arrays
            end
            else
                DoSimpleMsg('Create a circuit before trying to set an element active!', 5015);
        end;
        4:
        begin                                             // Circuit.SetActiveBus
            DSSGlobals.SetActiveBus(StripExtension(Widestring(arg)));
            if Assigned(ActiveCircuit[ActiveActor]) then
                Result := pAnsiChar(Ansistring(InttoStr(ActiveCircuit[ActiveActor].ActiveBusIndex - 1)))
            else
                Result := pAnsiChar(Ansistring('-1'));
        end;
        5:
        begin                                             // Circuit.SetActiveClass
            Result := pAnsiChar(Ansistring('0'));
            DevClassIndex := ClassNames[ActiveActor].Find(Widestring(arg));
            if DevClassIndex = 0 then
            begin
                DoSimplemsg('Error: Class ' + Widestring(arg) + ' not found.', 5016);
                Exit;
            end;

            LastClassReferenced[ActiveActor] := DevClassIndex;
            ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
            Result := pAnsiChar(Ansistring(InttoStr(LastClassReferenced[ActiveActor])));
        end
    else
        Result := pAnsiChar(Ansistring('Error, parameter not recognized'));
    end;
end;
//**************************Variant type properties*****************************
procedure CircuitV(mode: Longint; out arg: Variant; arg2: Longint); CDECL;

var
    LossValue: complex;
    pLine: TLineObj;
    Loss: Complex;
    pTransf: TTransfObj;
    pCktElem: TDSSCktElement;
    cPower, cLoss, Volts, Curr: Complex;
    i, j, k, NodeIdx, Phase: Integer;
    BaseFactor, VoltsD: Double;
    BusName: String;
    iV, p: Longword;
    NValues: Longword;
    nBus, nNZ: Longword;
    hY: NativeUInt;
    ColPtr, RowIdx: array of Longword;
    cVals: array of Complex;
    Temp: pDoubleArray;
    Temp2: pStringArray;

begin
    case mode of
        0:
        begin                                             // Circuit.Losses
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                arg := VarArrayCreate([0, 1], varDouble);
                LossValue := ActiveCircuit[ActiveActor].Losses[ActiveActor];
                arg[0] := LossValue.re;
                arg[1] := LossValue.im;
            end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        1:
        begin                                             // Circuit.LineLosses
            arg := VarArrayCreate([0, 1], varDouble);
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    pLine := Lines.First;
                    Loss := Cmplx(0.0, 0.0);
                    while pLine <> NIL do
                    begin
                        CAccum(Loss, pLine.Losses[ActiveActor]);
                        pLine := Lines.Next;
                    end;
                    arg[0] := Loss.re * 0.001;
                    arg[1] := Loss.im * 0.001;
                end;
        end;
        2:
        begin                                             // Circuit.SubstationLosses
            arg := VarArrayCreate([0, 1], varDouble);
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    pTransf := Transformers.First;
                    Loss := Cmplx(0.0, 0.0);
                    while pTransf <> NIL do
                    begin
                        if pTransf.Issubstation then
                            Caccum(Loss, pTransf.Losses[ActiveActor]);
                        pTransf := Transformers.Next;
                    end;
                    arg[0] := Loss.re * 0.001;
                    arg[1] := Loss.im * 0.001;
                end
            else
            begin
                arg[0] := 0.0;
                arg[1] := 0.0;
            end;
        end;
        3:
        begin                                             // Circuit.TotalPower
            arg := VarArrayCreate([0, 1], varDouble);
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
                    arg[0] := cPower.re * 0.001;
                    arg[1] := cPower.im * 0.001;
                end
            else
            begin
                arg[0] := 0.0;
                arg[1] := 0.0;
            end;
        end;
        4:
        begin                                             // Circuit.AllBusVolts
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    arg := VarArrayCreate([0, 2 * NumNodes - 1], varDouble);
                    k := 0;
                    for i := 1 to NumBuses do
                    begin
                        for j := 1 to Buses^[i].NumNodesThisBus do
                        begin
                            Volts := ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(j)];
                            arg[k] := Volts.re;
                            Inc(k);
                            arg[k] := Volts.im;
                            Inc(k);
                        end;
                    end;
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        5:
        begin                                             // Circuit.AllBusVMag
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    arg := VarArrayCreate([0, NumNodes - 1], varDouble);
                    k := 0;
                    for i := 1 to NumBuses do
                    begin
                        if Buses^[i].kVBase > 0.0 then
                            BaseFactor := 1000.0 * Buses^[i].kVBase
                        else
                            BaseFactor := 1.0;
                        for j := 1 to Buses^[i].NumNodesThisBus do
                        begin
                            VoltsD := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(j)]);
                            arg[k] := VoltsD / BaseFactor;
                            Inc(k);
                        end;
                    end;
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        6:
        begin                                             // Circuit.AllElementNames
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    arg := VarArrayCreate([0, NumDevices - 1], varOleStr);
                    for i := 1 to NumDevices do
                    begin
                        with  TDSSCktElement(CktElements.Get(i)) do
                            arg[i - 1] := ParentClass.Name + '.' + Name;
                    end;
                end
            else
                arg := VarArrayCreate([0, 0], varOleStr);
        end;
        7:
        begin                                             // Circuit.AllBusNames
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    arg := VarArrayCreate([0, NumBuses - 1], varOleStr);
                    for i := 0 to NumBuses - 1 do
                    begin
                        arg[i] := BusList.Get(i + 1);
                    end;
                end
            else
                arg := VarArrayCreate([0, 0], varOleStr);
        end;
        8:
        begin                                             // Circuit.AllElementLosses
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    arg := VarArrayCreate([0, 2 * NumDevices - 1], varDouble);
                    k := 0;
                    pCktElem := CktElements.First;
                    while pCktElem <> NIL do
                    begin
                        cLoss := pCktElem.Losses[ActiveActor];
                        arg[k] := cLoss.re * 0.001;
                        Inc(k);
                        arg[k] := cLoss.im * 0.001;
                        Inc(k);
                        pCktElem := CktElements.Next;
                    end;
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        9:
        begin                                             // Circuit.AllBusMagPu
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    arg := VarArrayCreate([0, NumNodes - 1], varDouble);
                    k := 0;
                    for i := 1 to NumBuses do
                    begin
                        if Buses^[i].kVBase > 0.0 then
                            BaseFactor := 1000.0 * Buses^[i].kVBase
                        else
                            BaseFactor := 1.0;
                        for j := 1 to Buses^[i].NumNodesThisBus do
                        begin
                            VoltsD := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(j)]);
                            arg[k] := VoltsD / BaseFactor;
                            Inc(k);
                        end;
                    end;
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        10:
        begin                                            // Circuit.AllNodeNames
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    arg := VarArrayCreate([0, NumNodes - 1], varOleStr);
                    k := 0;
                    for i := 1 to NumBuses do
                    begin
                        BusName := BusList.Get(i);
                        for j := 1 to Buses^[i].NumNodesThisBus do
                        begin
                            arg[k] := BusName + '.' + IntToStr(Buses^[i].GetNum(j));
                            Inc(k);
                        end;
                    end;
                end
            else
                arg := VarArrayCreate([0, 0], varOleStr);
        end;
        11:
        begin                                            // Circuit.SystemY
    { Return zero length Array if no circuit or no Y matrix}
            if ActiveCircuit[ActiveActor] = NIL then
                arg := VarArrayCreate([0, 0], varDouble)
            else
            if ActiveCircuit[ActiveActor].Solution.hY = 0 then
                arg := VarArrayCreate([0, 0], varDouble)
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
                    arg := VarArrayCreate([0, 2 * NValues - 1], varDouble);  // Make variant array for complex

      // the new way, first set all elements to zero
                    for iV := 0 to 2 * NValues - 1 do
                        arg[iV] := 0.0;
      // then back-fill the non-zero values
                    for j := 0 to nBus - 1 do
                    begin /// the zero-based column
                        for p := ColPtr[j] to ColPtr[j + 1] - 1 do
                        begin
                            i := RowIdx[p];  // the zero-based row
                            iV := i * nBus + j; // the zero-based, row-wise, complex result index
                            arg[iV * 2] := cVals[p].re;
                            arg[iV * 2 + 1] := cVals[p].im;
                        end;
                    end;
                end;
        end;
        12:
        begin                                            // Circuit.AllBusDistances
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    arg := VarArrayCreate([0, NumBuses - 1], varDouble);
                    for i := 0 to NumBuses - 1 do
                    begin
                        arg[i] := Buses^[i + 1].DistFromMeter;
                    end;
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        13:
        begin                                            // Circuit.AllNodeDistances
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    arg := VarArrayCreate([0, NumNodes - 1], varDouble);
                    k := 0;
                    for i := 1 to NumBuses do
                    begin
                        for j := 1 to Buses^[i].NumNodesThisBus do
                        begin
                            arg[k] := Buses^[i].DistFromMeter;
                            Inc(k);
                        end;
                    end;
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        14:
        begin                                            // Circuit.AllNodeVmagByPhase
            Phase := Integer(arg2);
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
                    arg := VarArrayCreate([0, k - 1], varDouble);
                    for i := 0 to k - 1 do
                        arg[i] := Temp^[i + 1];

                    Freemem(Temp, SizeOF(Temp^[1]) * NumNodes);
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        15:
        begin                                            // Circuit.AllNodeVmagPUByPhase
            Phase := Integer(arg2);
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
                            Temp^[k] := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(NodeIdx)]) / BaseFactor;
                        end;
                    end;
       // Assign to result and free temp array
                    arg := VarArrayCreate([0, k - 1], varDouble);
                    for i := 0 to k - 1 do
                        arg[i] := Temp^[i + 1];
                    Freemem(Temp, SizeOF(Temp^[1]) * NumNodes);
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        16:
        begin                                            // Circuit.AllNodeDistancesByPhase
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
       // Make a Temporary Array big enough to hold all nodes
                    Temp := AllocMem(SizeOF(Temp^[1]) * NumNodes);

       // Find nodes connected to specified phase
                    k := 0;
                    for i := 1 to NumBuses do
                    begin
                        NodeIdx := Buses^[i].FindIdx(Integer(arg2));
                        if NodeIdx > 0 then   // Node found with this phase number
                        begin
                            Inc(k);
                            Temp^[k] := Buses^[i].DistFromMeter;
                        end;
                    end;

       // Assign to result and free temp array
                    arg := VarArrayCreate([0, k - 1], varDouble);
                    for i := 0 to k - 1 do
                        arg[i] := Temp^[i + 1];

                    Freemem(Temp, SizeOF(Temp^[1]) * NumNodes);
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        17:
        begin                                            // Circuit.AllNodeNamesByPhase
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
       // Make a Temporary Array big enough to hold all nodes
                    Temp2 := AllocStringArray(NumNodes);

       // Find nodes connected to specified phase
                    k := 0;
                    for i := 1 to NumBuses do
                    begin
                        NodeIdx := Buses^[i].FindIdx(Integer(arg2));
                        if NodeIdx > 0 then   // Node found with this phase number
                        begin
                            Inc(k);
                            Temp2^[k] := Format('%s.%d', [BusList.Get(i), Integer(arg)]);
                        end;
                    end;

       // Assign to result and free temp array
                    arg := VarArrayCreate([0, k - 1], varOleStr);
                    for i := 0 to k - 1 do
                        arg[i] := Temp2^[i + 1];

                    FreeStringArray(Temp2, NumNodes);
                end
            else
                arg := VarArrayCreate([0, 0], varOleStr);
        end;
        18:
        begin                                            // Circuit.YNodeVArray
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    arg := VarArrayCreate([0, 2 * NumNodes - 1], varDouble);
                    k := 0;
                    for i := 1 to NumNodes do
                    begin
                        Volts := ActiveCircuit[ActiveActor].Solution.NodeV^[i];
                        arg[k] := Volts.re;
                        Inc(k);
                        arg[k] := Volts.im;
                        Inc(k);
                    end;
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        19:
        begin                                            // Circuit.YNodeOrder
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    arg := VarArrayCreate([0, NumNodes - 1], varOleStr);
                    k := 0;
                    for i := 1 to NumNodes do
                    begin
                        with MapNodeToBus^[i] do
                            arg[k] := Format('%s.%-d', [Uppercase(BusList.Get(Busref)), NodeNum]);
                        Inc(k);
                    end;
                end
            else
                arg := VarArrayCreate([0, 0], varOleStr);
        end;
        20:
        begin                                            // Circuit.YCurrents
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    arg := VarArrayCreate([0, 2 * NumNodes - 1], varDouble);
                    k := 0;
                    for i := 1 to NumNodes do
                    begin
                        Curr := ActiveCircuit[ActiveActor].Solution.Currents^[i];
                        arg[k] := Curr.re;
                        Inc(k);
                        arg[k] := Curr.im;
                        Inc(k);
                    end;
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end
    else
        arg[0] := 'Error, parameter not recognized';
    end;
end;

end.
