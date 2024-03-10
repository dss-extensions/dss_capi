unit DCircuit;

interface

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
     {$IFNDEF FPC_DLL}
    dialogs,
{$ENDIF}
    YMatrix,
    Variants,
    arrayDef,
    Utilities,
    SolutionAlgs,
    KLUSolve;

function CircuitI(mode: Longint; arg: Longint): Longint; CDECL;
function CircuitF(mode: Longint; arg1, arg2: Double): Double; CDECL;
function CircuitS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure CircuitV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;


implementation

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
            if (ActiveCircuit[ActiveActor] <> NIL) and Assigned(ActiveDSSClass[ActiveActor]) then
            begin
                Result := ActiveDSSClass[ActiveActor].First;
            end
            else
                Result := 0;
        end;
        11:
        begin                                            // Circuit.NextElement
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
                        Result := RegisterTotals[3] + RegisterTotals[19];
                end;
        end
    else
        Result := -1.0;
    end;
end;

//**************************String type properties*****************************
function CircuitS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var

    DevClassIndex: Integer;

begin
    Result := Pansichar(Ansistring('')); // Default return value
    case mode of
        0:
        begin                                             // Circuit.Name
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := Pansichar(Ansistring(ActiveCircuit[ActiveActor].Name))
            else
                Result := Pansichar(Ansistring(''));
        end;
        1:
        begin                                             // Circuit.Disable
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    SetElementActive(String(arg));
                    if ActiveCktElement <> NIL then
                        ActiveCktElement.Enabled := FALSE;
                end;
        end;
        2:
        begin                                             // Circuit.Enable
            with ActiveCircuit[ActiveActor] do
            begin
                SetElementActive(String(arg));
                if ActiveCktElement <> NIL then
                    ActiveCktElement.Enabled := TRUE;
            end;
        end;
        3:
        begin                                             // Circuit.SetActiveElement
            Result := Pansichar(Ansistring('-1'));
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Result := Pansichar(Ansistring(Inttostr(ActiveCircuit[ActiveActor].SetElementActive(String(arg)) - 1)));   // make zero based to be compatible with collections and variant arrays
            end
            else
                DoSimpleMsg('Create a circuit before trying to set an element active!', 5015);
        end;
        4:
        begin                                             // Circuit.SetActiveBus
            DSSGlobals.SetActiveBus(StripExtension(String(arg)));
            if Assigned(ActiveCircuit[ActiveActor]) then
                Result := Pansichar(Ansistring(InttoStr(ActiveCircuit[ActiveActor].ActiveBusIndex - 1)))
            else
                Result := Pansichar(Ansistring('-1'));
        end;
        5:
        begin                                             // Circuit.SetActiveClass
            Result := Pansichar(Ansistring('0'));
            DevClassIndex := ClassNames[ActiveActor].Find(String(arg));
            if DevClassIndex = 0 then
            begin
                DoSimplemsg('Error: Class ' + arg + ' not found.', 5016);
                Exit;
            end;

            LastClassReferenced[ActiveActor] := DevClassIndex;
            ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
            Result := Pansichar(Ansistring(InttoStr(LastClassReferenced[ActiveActor])));
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not recognized'));
    end;
end;
//**************************Variant type properties*****************************
procedure CircuitV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

var
    pLine: TLineObj;
    Loss: Complex;
    pTransf: TTransfObj;
    pCktElem: TDSSCktElement;
    cPower: Complex;
    i, j, k,
    NodeIdx,
    Phase: Integer;
    BaseFactor: Double;
    BusName: String;
    iV, p, i_lwd, j_lwd,
    NValues,
    nBus, nNZ: Longword;
    hY: Nativeuint;
    ColPtr, RowIdx: array of Longword;
    cVals: array of Complex;
    Temp: pDoubleArray;
    Temp2: pStringArray;
    Pint: ^Integer;

begin
  {$IFDEF FPC_DLL}
    initialize(Temp);
{$ENDIF}
    case mode of
        0:
        begin                                             // Circuit.Losses
            myType := 3;          // Complex
            setlength(myCmplxArray, 1);
            myCmplxArray[0] := CZero;
            if ActiveCircuit[ActiveActor] <> NIL then
                myCmplxArray[0] := ActiveCircuit[ActiveActor].Losses[ActiveActor];

            myPointer := @(myCmplxArray[0]);
            mySize := SizeOf(myCmplxArray[0]) * Length(myCmplxArray);

        end;
        1:
        begin                                             // Circuit.LineLosses
            myType := 3;        // Complex
            setlength(myCmplxArray, 1);
            myCmplxArray[0] := CZero;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    pLine := Lines.First;
                    Loss := Cmplx(0.0, 0.0);
                    while pLine <> NIL do
                    begin
                        CAccum(Loss, pLine.Losses[ActiveActor]);
                        pLine := Lines.Next;
                    end;
                    myCmplxArray[0] := cmulreal(Loss, 0.001);
                end;
            end;

            myPointer := @(myCmplxArray[0]);
            mySize := SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
        end;
        2:
        begin                                             // Circuit.SubstationLosses
            myType := 3;        // Complex
            setlength(myCmplxArray, 1);
            myCmplxArray[0] := CZero;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
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
                    myCmplxArray[0] := cmulreal(Loss, 0.001);
                end
            end;

            myPointer := @(myCmplxArray[0]);
            mySize := SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
        end;
        3:
        begin                                             // Circuit.TotalPower
            myType := 3;        // Complex
            setlength(myCmplxArray, 1);
            myCmplxArray[0] := CZero;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    pCktElem := Sources.First;
                    cPower := Cmplx(0.0, 0.0);
                    while pCktElem <> NIL do
                    begin
                        CAccum(cPower, pcktElem.Power[1, ActiveActor]);
                        pCktElem := Sources.Next;
                    end;
                    myCmplxArray[0] := cmulreal(cPower, 0.001);
                end
            end;

            myPointer := @(myCmplxArray[0]);
            mySize := SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
        end;
        4:
        begin                                             // Circuit.AllBusVolts
            myType := 3;        // Complex
            setlength(myCmplxArray, 1);
            myCmplxArray[0] := CZero;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    setlength(myCmplxArray, NumNodes);
                    k := 0;
                    for i := 1 to NumBuses do
                    begin
                        for j := 1 to Buses^[i].NumNodesThisBus do
                        begin
                            myCmplxArray[k] := ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(j)];
                            Inc(k);
                        end;
                    end;
                end
            end;

            myPointer := @(myCmplxArray[0]);
            mySize := SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
        end;
        5:
        begin                                             // Circuit.AllBusVMag
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    setlength(myDBLArray, NumNodes);
                    k := 0;
                    for i := 1 to NumBuses do
                    begin
          // If Buses^[i].kVBase >0.0 then BaseFactor :=  1000.0* Buses^[i].kVBase  Else BaseFactor := 1.0;
                        for j := 1 to Buses^[i].NumNodesThisBus do
                        begin
                            myDBLArray[k] := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(j)]);
                            Inc(k);
                        end;
                    end;
                end
            end;

            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        6:
        begin                                             // Circuit.AllElementNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    for i := 1 to NumDevices do
                    begin
                        with  TDSSCktElement(CktElements.Get(i)) do
                            WriteStr2Array(ParentClass.Name + '.' + Name);
                        WriteStr2Array(Char(0));
                    end;
                end
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');

            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        7:
        begin                                             // Circuit.AllBusNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    for i := 0 to NumBuses - 1 do
                    begin
                        WriteStr2Array(BusList.Get(i + 1));
                        WriteStr2Array(Char(0));
                    end;
                end
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        8:
        begin                                             // Circuit.AllElementLosses
            myType := 3;        // Complex
            setlength(myCmplxArray, 1);
            myCmplxArray[0] := CZero;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    setlength(myCmplxArray, NumDevices);
                    k := 0;
                    pCktElem := CktElements.First;
                    while pCktElem <> NIL do
                    begin
                        myCmplxArray[k] := cmulreal(pCktElem.Losses[ActiveActor], 0.001);
                        Inc(k);
                        pCktElem := CktElements.Next;
                    end;
                end
            end;

            myPointer := @(myCmplxArray[0]);
            mySize := SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
        end;
        9:
        begin                                             // Circuit.AllBusMagPu
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    setlength(myDBLArray, NumNodes);
                    k := 0;
                    for i := 1 to NumBuses do
                    begin
                        if Buses^[i].kVBase > 0.0 then
                            BaseFactor := 1000.0 * Buses^[i].kVBase
                        else
                            BaseFactor := 1.0;
                        for j := 1 to Buses^[i].NumNodesThisBus do
                        begin
                            myDBLArray[k] := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(j)]) / BaseFactor;
                            Inc(k);
                        end;
                    end;
                end
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        10:
        begin                                            // Circuit.AllNodeNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    for i := 1 to NumBuses do
                    begin
                        BusName := BusList.Get(i);
                        for j := 1 to Buses^[i].NumNodesThisBus do
                        begin
                            WriteStr2Array(BusName + '.' + IntToStr(Buses^[i].GetNum(j)));
                            WriteStr2Array(Char(0));
                        end;
                    end;
                end
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        11:
        begin                                            // Circuit.SystemY
    { Return zero length Array if no circuit or no Y matrix}
            myType := 3;        // Complex
            setlength(myCmplxArray, 1);
            myCmplxArray[0] := CZero;
            if (not (ActiveCircuit[ActiveActor] = NIL)) and (not (ActiveCircuit[ActiveActor].Solution.hY = 0)) then
            begin
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
                    setlength(myCmplxArray, NValues);  // Make variant array for complex

        // the new way, first set all elements to zero
                    for iV := 0 to (NValues - 1) do
                        myCmplxArray[iV] := cmplx(0, 0);
        // then back-fill the non-zero values
                    for j_lwd := 0 to nBus - 1 do
                    begin /// the zero-based column
                        for p := ColPtr[j_lwd] to (ColPtr[j_lwd + 1] - 1) do
                        begin
                            i_lwd := RowIdx[p];  // the zero-based row
                            iV := i_lwd * nBus + j_lwd; // the zero-based, row-wise, complex result index
                            myCmplxArray[iV] := cVals[p];
                        end;
                    end;
                end;
            end;
            myPointer := @(myCmplxArray[0]);
            mySize := SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
        end;
        12:
        begin                                            // Circuit.AllBusDistances
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    setlength(myDBLArray, NumBuses);
                    for i := 0 to NumBuses - 1 do
                        myDBLArray[i] := Buses^[i + 1].DistFromMeter;
                end
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        13:
        begin                                            // Circuit.AllNodeDistances
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    setlength(myDBLArray, NumNodes);
                    k := 0;
                    for i := 1 to NumBuses do
                    begin
                        for j := 1 to Buses^[i].NumNodesThisBus do
                        begin
                            myDBLArray[k] := Buses^[i].DistFromMeter;
                            Inc(k);
                        end;
                    end;
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        14:
        begin                                            // Circuit.AllNodeVmagByPhase
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            PInt := myPointer;
            Phase := Integer(PInt^);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
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
                    setlength(myDBLArray, k);
                    for i := 0 to (k - 1) do
                        myDBLArray[i] := Temp^[i + 1];
                    Freemem(Temp, SizeOF(Temp^[1]) * NumNodes);
                end
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        15:
        begin                                            // Circuit.AllNodeVmagPUByPhase
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            PInt := myPointer;
            Phase := Integer(PInt^);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
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
                    setlength(myDBLArray, k);
                    for i := 0 to (k - 1) do
                        myDBLArray[i] := Temp^[i + 1];
                    Freemem(Temp, SizeOF(Temp^[1]) * NumNodes);
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        16:
        begin                                            // Circuit.AllNodeDistancesByPhase
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            PInt := myPointer;
            Phase := Integer(PInt^);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
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
                    setlength(myDBLArray, k);
                    for i := 0 to k - 1 do
                        myDBLArray[i] := Temp^[i + 1];

                    Freemem(Temp, SizeOF(Temp^[1]) * NumNodes);
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        17:
        begin                                            // Circuit.AllNodeNamesByPhase
            myType := 4;        // String
            setlength(myStrArray, 0);
            PInt := myPointer;
            Phase := Integer(PInt^);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
        // Make a Temporary Array big enough to hold all nodes
                    Temp2 := AllocStringArray(NumNodes);

        // Find nodes connected to specified phase
                    k := 0;
                    for i := 1 to NumBuses do
                    begin
                        NodeIdx := Buses^[i].FindIdx(Phase);
                        if NodeIdx > 0 then   // Node found with this phase number
                        begin
                            Inc(k);
                            Temp2^[k] := Format('%s.%d', [BusList.Get(i), Phase]);
                        end;
                    end;

        // Assign to result and free temp array
                    for i := 0 to k - 1 do
                    begin
                        WriteStr2Array(Temp2^[i + 1]);
                        WriteStr2Array(Char(0));
                    end;

                    FreeStringArray(Temp2, NumNodes);
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        18:
        begin                                            // Circuit.YNodeVArray
            myType := 3;        // Complex
            setlength(myCmplxArray, 1);
            myCmplxArray[0] := CZero;
            myPointer := @(myCmplxArray[0]);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    myPointer := @(ActiveCircuit[ActiveActor].Solution.NodeV^[1]);
                end;
            end;
            mySize := SizeOf(ActiveCircuit[ActiveActor].Solution.NodeV^[1]) * ActiveCircuit[ActiveActor].NumNodes;
        end;
        19:
        begin                                            // Circuit.YNodeOrder
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    for i := 1 to NumNodes do
                    begin
                        with MapNodeToBus^[i] do
                            WriteStr2Array(Format('%s.%-d', [Uppercase(BusList.Get(Busref)), NodeNum]));
                        WriteStr2Array(Char(0));
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        20:
        begin                                            // Circuit.YCurrents
            myType := 3;        // Complex
            setlength(myCmplxArray, 1);
            myCmplxArray[0] := CZero;
            myPointer := @(myCmplxArray[0]);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                    myPointer := @(ActiveCircuit[ActiveActor].Solution.Currents^[1]);
            end;
            mySize := SizeOf(ActiveCircuit[ActiveActor].Solution.Currents^[1]) * ActiveCircuit[ActiveActor].NumNodes;
        end
    else
        myType := 4;        // String
        setlength(myStrArray, 0);
        WriteStr2Array('Error, parameter not recognized');
        myPointer := @(myStrArray[0]);
        mySize := Length(myStrArray);
    end;
end;

end.
