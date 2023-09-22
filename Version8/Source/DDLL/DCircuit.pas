unit DCircuit;

interface

uses DSSClassDefs,
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

function CircuitI(mode:longint; arg: longint):longint;cdecl;
function CircuitF(mode:longint; arg1, arg2: double):double;cdecl;
function CircuitS(mode:longint; arg: pAnsiChar):pAnsiChar;cdecl;
procedure CircuitV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;


implementation

function CircuitI(mode:longint; arg: longint):longint;cdecl;

var

   p:TDSSCktElement;
   Mon :TDSSMonitor;
   Mtr :TEnergyMeter;

begin
  Result:=0; // Default return value
  case mode of
  0: begin                                             // Circuit.NumCktElements
       If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].NumDevices;
  end;
  1: begin                                             // Circuit.NumBuses
      If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].NumBuses
      Else Result := 0;
  end;
  2: begin                                             // Circuit.NumNodes
      If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].NumNodes;
  end;
  3: begin                                             // Circuit.FirstPCElement
      Result := 0;
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        p:= ActiveCircuit[ActiveActor].PCElements.First;
        IF p <> Nil  THEN Begin
           Repeat
               If p.enabled Then Begin
                   Result := 1;
                   ActiveCircuit[ActiveActor].ActiveCktElement := p;
               End
               Else  p := ActiveCircuit[ActiveActor].PCElements.Next;

           Until (Result = 1) or (p = nil);
        End
        ELSE Result := 0;
      End;
  end;
  4: begin                                             // Circuit.NextPCElement
      Result := 0;
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        p:= ActiveCircuit[ActiveActor].PCElements.Next;
        IF p<> Nil THEN
        Begin
             Repeat
                 If p.enabled
                 Then Begin
                   Result := ActiveCircuit[ActiveActor].PCElements.ActiveIndex;
                   ActiveCircuit[ActiveActor].ActiveCktElement := p;
                 End
                 Else p :=  ActiveCircuit[ActiveActor].PCElements.Next;
             Until (Result > 0) or (p = nil);
        End ELSE  Result := 0;
      End;
  end;
  5: begin                                             // Circuit.FirstPDElement
      Result := 0;
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
       p := ActiveCircuit[ActiveActor].PDElements.First;
       IF p<> Nil THEN
         Begin
              Repeat
                If p.enabled
                Then Begin
                  Result := 1;
                  ActiveCircuit[ActiveActor].ActiveCktElement := p;
                end
                Else  p := ActiveCircuit[ActiveActor].PDElements.Next;
              Until (Result = 1) or (p = nil);
         End
       ELSE Result := 0;
      End;
  end;
  6: begin                                             // Circuit.NextPDElement
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        p:= ActiveCircuit[ActiveActor].PDElements.Next;
        IF p <> Nil
        THEN Begin
           Repeat
             If p.Enabled
             Then Begin
                 Result := ActiveCircuit[ActiveActor].PDElements.ActiveIndex;
                 ActiveCircuit[ActiveActor].ActiveCktElement := p;
             End
             Else p:= ActiveCircuit[ActiveActor].PDElements.Next;
           Until (Result > 0) or (p = Nil);
        End
        ELSE Begin
           Result := 0;
        End;
      End;
  end;
  7: begin                                             // Circuit.Sample
      MonitorClass[ActiveActor].SampleAll(ActiveActor);
      EnergyMeterClass[ActiveActor].SampleAll(ActiveActor);
      Result:=0;
  end;
  8: begin                                             // Circuit.SaveSample
      Mon := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('monitor'));
      Mon.SaveAll(ActiveActor);
      Mtr := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('energymeter'));
      Mtr.SaveAll(ActiveActor);
  end;
  9: begin                                             // Circuit.SetActiveBusi
      Result := -1;   // Signifies Error
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If (arg >= 0) and (arg < Numbuses) Then Begin
             ActiveBusIndex := arg + 1;
             Result := 0;
          End;
      End;
  end;
  10: begin                                            // Circuit.FirstElement
      Result := 0;
      IF (ActiveCircuit[ActiveActor] <> Nil) and Assigned(ActiveDSSClass[ActiveActor]) THEN
      Begin
         Result := ActiveDSSClass[ActiveActor].First;
      End
        ELSE Result := 0;
  end;
  11: begin                                            // Circuit.NextElement
      Result := 0;
      IF (ActiveCircuit[ActiveActor] <> Nil) and Assigned(ActiveDSSClass[ActiveActor]) THEN
      Begin
         Result := ActiveDSSClass[ActiveActor].Next;
      End
        ELSE Result := 0;
  end;
  12: begin                                            // Circuit.UpdateStorage
     StorageClass[ActiveActor].UpdateAll(ActiveActor);
  end;
  13: begin                                            // Circuit.ParentPDElement
     Result := 0;
     With ActiveCircuit[ActiveActor] Do
     If ActiveCktElement is TPDElement Then
     Begin
         p := TPDElement(ActiveCktElement).ParentPDElement;
         If p <> Nil Then
         Begin
           ActiveCktElement :=  p;
           Result := p.ClassIndex;  // should be >0
         End;
     End;
  end;
  14: begin                                            // Circuit.EndOfTimeStepUpdate
      EndOfTimeStepCleanup(ActiveActor);
      Result:=0;
  end
  else
      Result:=-1;
  end;

end;

//**************************floating point properties*****************************
function CircuitF(mode:longint; arg1, arg2: double):double;cdecl;
begin
  Result:=0.0; // Default return value
  case mode of
  0: begin                                             // Circuit.Capacity
     If ActiveCircuit[ActiveActor] <> Nil Then  With ActiveCircuit[ActiveActor] Do
      Begin
           CapacityStart := arg1;
           CapacityIncrement := arg2;
           If ComputeCapacity(ActiveActor) Then
               Result := RegisterTotals[3] + RegisterTotals[19]
           Else
               Result := 0.0;
      End
      Else Begin
          Result := 0.0;
      End;
  end
  else
      Result:=-1.0;
  end;
end;

//**************************String type properties*****************************
function CircuitS(mode:longint; arg: pAnsiChar):pAnsiChar;cdecl;

var

   DevClassIndex :Integer;

begin
  Result:=pAnsiChar(AnsiString('')); // Default return value
  case mode of
  0: begin                                             // Circuit.Name
      If ActiveCircuit[ActiveActor] <> Nil Then Result := pAnsiChar(AnsiString(ActiveCircuit[ActiveActor].Name))
      Else Result := pAnsiChar(AnsiString(''));
  end;
  1: begin                                             // Circuit.Disable
       IF ActiveCircuit[ActiveActor] <> Nil THEN
       WITH ActiveCircuit[ActiveActor] DO
       Begin
          SetElementActive(arg);
          If ActiveCktElement<>nil THEN ActiveCktElement.Enabled := FALSE;
       End;
  end;
  2: begin                                             // Circuit.Enable
       WITH ActiveCircuit[ActiveActor] DO Begin
          SetElementActive(arg);
          If ActiveCktElement<>nil THEN ActiveCktElement.Enabled := TRUE;
       End;
  end;
  3: begin                                             // Circuit.SetActiveElement
      Result := pAnsiChar(AnsiString('-1'));
       IF ActiveCircuit[ActiveActor] <> NIL
       THEN Begin
           Result := pAnsiChar(AnsiString(Inttostr(ActiveCircuit[ActiveActor].SetElementActive(arg) - 1)));   // make zero based to be compatible with collections and variant arrays
       End
       ELSE DoSimpleMsg('Create a circuit before trying to set an element active!', 5015);
  end;
  4: begin                                             // Circuit.SetActiveBus
     DSSGlobals.SetActiveBus(StripExtension(arg));
     If Assigned(ActiveCircuit[ActiveActor]) then Result := pAnsiChar(AnsiString(InttoStr(ActiveCircuit[ActiveActor].ActiveBusIndex - 1))) Else Result := pAnsiChar(AnsiString('-1'));
  end;
  5: begin                                             // Circuit.SetActiveClass
     Result := pAnsiChar(AnsiString('0'));
     DevClassIndex := ClassNames[ActiveActor].Find(arg);
     If DevClassIndex = 0 Then  Begin
        DoSimplemsg('Error: Class ' + arg + ' not found.' , 5016);
        Exit;
     End;

     LastClassReferenced[ActiveActor] := DevClassIndex;
     ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
     Result := pAnsiChar(AnsiString(InttoStr(LastClassReferenced[ActiveActor])));
  end
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not recognized'));
  end;
end;
//**************************Variant type properties*****************************
procedure CircuitV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

var
   LossValue        : complex;
   pLine            : TLineObj;
   Loss             : Complex;
   pTransf          : TTransfObj;
   pCktElem         : TDSSCktElement;
   cPower,
   cLoss,
   Volts,
   Curr             : Complex;
   i,j,k,
   NodeIdx,
   Phase            :Integer;
   BaseFactor,
   VoltsD           : double;
   BusName          : String;
   iV, p,
   NValues,
   nBus, nNZ        : LongWord;
   hY               : NativeUInt;
   ColPtr, RowIdx   : array of LongWord;
   cVals            : array of Complex;
   Temp             : pDoubleArray;
   Temp2            : pStringArray;
   Pint             : ^Integer;

begin
  case mode of
  0: begin                                             // Circuit.Losses
    myType  :=  3;          // Complex
    setlength(myCmplxArray, 1);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
       myCmplxArray[0] := ActiveCircuit[ActiveActor].Losses[ActiveActor]
    else
      myCmplxArray[0] := cmplx( 0, 0 );
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);

  end;
  1: begin                                             // Circuit.LineLosses
    myType  :=  3;        // Complex
    setlength(myCmplxArray, 1);
    IF ActiveCircuit[ActiveActor] <> NIL THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        pLine := Lines.First;
        Loss := Cmplx(0.0,0.0);
        WHILE pLine<>nil DO
        Begin
           CAccum(Loss, pLine.Losses[ActiveActor]);
           pLine := Lines.Next;
        End;
        myCmplxArray[0] := cmulreal(Loss, 0.001);
      End;
    End
    else
      myCmplxArray[0] := cmplx(0,0);
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  2: begin                                             // Circuit.SubstationLosses
    myType  :=  3;        // Complex
    setlength(myCmplxArray, 1);
    IF ActiveCircuit[ActiveActor] <> nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
       pTransf := Transformers.First;
       Loss := Cmplx(0.0,0.0);
       WHILE pTransf<>nil DO
       Begin
          IF pTransf.Issubstation THEN Caccum(Loss, pTransf.Losses[ActiveActor]);
          pTransf := Transformers.Next;
       End;
        myCmplxArray[0] := cmulreal(Loss, 0.001);
      End
    End
    else
      myCmplxArray[0] := cmplx(0,0);
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  3: begin                                             // Circuit.TotalPower
    myType  :=  3;        // Complex
    setlength(myCmplxArray, 1);
    IF ActiveCircuit[ActiveActor] <> nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO Begin
        pCktElem := Sources.First;
        cPower := Cmplx(0.0, 0.0);
        WHILE pCktElem<>nil  DO Begin
           CAccum(cPower, pcktElem.Power[1,ActiveActor]);
           pCktElem := Sources.Next;
        End;
        myCmplxArray[0] := cmulreal(cPower, 0.001);
      End
    end
    else
      myCmplxArray[0] := cmplx(0,0);
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  4: begin                                             // Circuit.AllBusVolts
    myType  :=  3;        // Complex
    setlength(myCmplxArray, 1);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       setlength(myCmplxArray, NumNodes);
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
         For j := 1 to Buses^[i].NumNodesThisBus DO
         Begin
           myCmplxArray[k] := ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(j)];
           Inc(k);
         End;
       End;
     End
    End
    ELSE myCmplxArray[0] := cmplx(0,0);
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  5: begin                                             // Circuit.AllBusVMag
    myType  :=  2;        // Double
    setlength(myDBLArray, 1);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        setlength(myDBLArray, NumNodes);
        k:=0;
        FOR i := 1 to NumBuses DO
        Begin
          If Buses^[i].kVBase >0.0 then BaseFactor :=  1000.0* Buses^[i].kVBase  Else BaseFactor := 1.0;
          For j := 1 to Buses^[i].NumNodesThisBus  DO
          Begin
           myDBLArray[k] := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(j)]);
           Inc(k);
          End;
        End;
      End
    End
    ELSE myDBLArray[0] := 0;
    myPointer :=  @(myDBLArray[0]);
    mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
  end;
  6: begin                                             // Circuit.AllElementNames
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        FOR i := 1 to NumDevices DO
        Begin
          WITH  TDSSCktElement(CktElements.Get(i)) DO
            WriteStr2Array(ParentClass.Name + '.' + Name);
          WriteStr2Array(Char(0));
        End;
      End
    End
    ELSE WriteStr2Array('');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
  7: begin                                             // Circuit.AllBusNames
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        FOR i := 0 to NumBuses-1 DO
        Begin
          WriteStr2Array(BusList.Get(i + 1));
          WriteStr2Array(Char(0));
        End;
      End
    End
    ELSE WriteStr2Array('');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
  8: begin                                             // Circuit.AllElementLosses
    myType  :=  3;        // Complex
    setlength(myCmplxArray, 1);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        setlength(myCmplxArray, NumDevices);
        k:=0;
        pCktElem := CktElements.First;
        WHILE pCktElem <> Nil DO
        Begin
          myCmplxArray[k] := cmulreal(pCktElem.Losses[ActiveActor], 0.001);
          Inc(k);
          pCktElem := CktElements.Next;
        End;
      End
    End
    ELSE myCmplxArray[0]  :=  cmplx(0,0);
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  9: begin                                             // Circuit.AllBusMagPu
    myType  :=  2;        // Double
    setlength(myDBLArray, 1);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        setlength(myDBLArray, NumNodes);
        k:=0;
        FOR i := 1 to NumBuses DO
        Begin
           If Buses^[i].kVBase >0.0 then BaseFactor :=  1000.0* Buses^[i].kVBase  Else BaseFactor := 1.0;
            For j := 1 to Buses^[i].NumNodesThisBus  DO
             Begin
              myDBLArray[k] := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(j)]) / BaseFactor;
              Inc(k);
            End;
        End;
      End
    End
    ELSE myDBLArray[0] := 0;
    myPointer :=  @(myDBLArray[0]);
    mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
  end;
  10: begin                                            // Circuit.AllNodeNames
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        FOR i := 1 to NumBuses DO
        Begin
          BusName := BusList.Get(i);
          FOR j := 1 to Buses^[i].NumNodesThisBus DO
          Begin
            WriteStr2Array(BusName + '.' + IntToStr(Buses^[i].GetNum(j)));
            WriteStr2Array(Char(0));
          End;
        End;
      End
    End
    ELSE WriteStr2Array('');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
  11: begin                                            // Circuit.SystemY
    { Return zero length Array if no circuit or no Y matrix}
    myType  :=  3;        // Complex
    setlength(myCmplxArray, 1);
    IF ActiveCircuit[ActiveActor] = nil                Then myCmplxArray[0] :=  cmplx(0,0)
    ELSE If ActiveCircuit[ActiveActor].Solution.hY = 0 Then myCmplxArray[0] :=  cmplx(0,0)
    ELSE
    Begin
      With ActiveCircuit[ActiveActor] Do
      Begin
        hY := ActiveCircuit[ActiveActor].Solution.hY;
        // get the compressed columns out of KLU
        FactorSparseMatrix(hY); // no extra work if already done
        GetNNZ (hY, @nNZ);
        GetSize (hY, @nBus);
        SetLength (ColPtr, nBus + 1);
        SetLength (RowIdx, nNZ);
        SetLength (cVals, nNZ);
        GetCompressedMatrix (hY, nBus + 1, nNZ, @ColPtr[0], @RowIdx[0], @cVals[0]);

        // allocate a square matrix
        NValues := SQR(NumNodes);
        setlength(myCmplxArray, NValues);  // Make variant array for complex

        // the new way, first set all elements to zero
        for iV := 0 to (NValues - 1) do myCmplxArray[iV] := cmplx(0,0);
        // then back-fill the non-zero values
        for j := 0 to nBus - 1 do begin /// the zero-based column
          for p := ColPtr[j] to ( ColPtr[j + 1] - 1 ) do begin
            i := RowIdx[p];  // the zero-based row
            iV := i * nBus + j; // the zero-based, row-wise, complex result index
            myCmplxArray[iV] := cVals[p];
          End;
        End;
      END;
    End;
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  12: begin                                            // Circuit.AllBusDistances
    myType  :=  2;        // Double
    setlength(myDBLArray, 1);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        setlength(myDBLArray, NumBuses);
        FOR i := 0 to NumBuses-1 DO
          myDBLArray[i] := Buses^[i + 1].DistFromMeter;
      End
    End
    ELSE myDBLArray[0] := 0;
    myPointer :=  @(myDBLArray[0]);
    mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
  end;
  13: begin                                            // Circuit.AllNodeDistances
    myType  :=  2;        // Double
    setlength(myDBLArray, 1);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        setlength(myDBLArray, NumNodes);
        k:=0;
        FOR i := 1 to NumBuses DO
        Begin
          FOR j := 1 to Buses^[i].NumNodesThisBus DO
          Begin
              myDBLArray[k] := Buses^[i].DistFromMeter;
              Inc(k);
          End;
        End;
      End;
    End
    ELSE myDBLArray[0]  := 0;
    myPointer :=  @(myDBLArray[0]);
    mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
  end;
  14: begin                                            // Circuit.AllNodeVmagByPhase
    myType  :=  2;        // Double
    setlength(myDBLArray, 1);
    PInt  :=  myPointer;
    Phase :=  integer(PInt^);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        // Make a Temporary Array big enough to hold all nodes
        Temp := AllocMem(SizeOF(Temp^[1]) * NumNodes);
        // Find nodes connected to specified phase
        k:=0;
        FOR i := 1 to NumBuses DO
        Begin
          NodeIdx := Buses^[i].FindIdx(Phase);
          If NodeIdx > 0 then   // Node found with this phase number
          Begin
              Inc(k);
              Temp^[k] := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(NodeIdx)]);
          End;
        End;
        // Assign to result and free temp array
        setlength(myDBLArray, k);
        For i := 0 to (k - 1) do  myDBLArray[i] := Temp^[i + 1];
        Freemem(Temp, SizeOF(Temp^[1])*NumNodes);
      End
    End
    ELSE myDBLArray[0] := 0;
    myPointer :=  @(myDBLArray[0]);
    mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
  end;
  15: begin                                            // Circuit.AllNodeVmagPUByPhase
    myType  :=  2;        // Double
    setlength(myDBLArray, 1);
    PInt  :=  myPointer;
    Phase :=  integer(PInt^);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        // Make a Temporary Array big enough to hold all nodes
        Temp := AllocMem(SizeOF(Temp^[1]) * NumNodes);
        // Find nodes connected to specified phase
        k:=0;
        FOR i := 1 to NumBuses DO  Begin
          NodeIdx := Buses^[i].FindIdx(Phase);
          If NodeIdx > 0 then   // Node found with this phase number
          Begin
            If Buses^[i].kVBase >0.0 then BaseFactor :=  1000.0* Buses^[i].kVBase  Else BaseFactor := 1.0;
            Inc(k);
            Temp^[k] := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(NodeIdx)])/BaseFactor;
          End;
        End;
        // Assign to result and free temp array
        setlength(myDBLArray, k);
        For i := 0 to (k - 1) do  myDBLArray[i] := Temp^[i+1];
        Freemem(Temp, SizeOF(Temp^[1])*NumNodes);
      End;
    End
    ELSE myDBLArray[0]  :=  0;
    myPointer :=  @(myDBLArray[0]);
    mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
  end;
  16: begin                                            // Circuit.AllNodeDistancesByPhase
    myType  :=  2;        // Double
    setlength(myDBLArray, 1);
    PInt  :=  myPointer;
    Phase :=  integer(PInt^);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        // Make a Temporary Array big enough to hold all nodes
        Temp := AllocMem(SizeOF(Temp^[1]) * NumNodes);

        // Find nodes connected to specified phase
        k:=0;
        FOR i := 1 to NumBuses DO
        Begin
          NodeIdx := Buses^[i].FindIdx(Phase);
          If NodeIdx > 0 then   // Node found with this phase number
          Begin
              Inc(k);
              Temp^[k] := Buses^[i].DistFromMeter;
          End;
        End;

        // Assign to result and free temp array
        setlength(myDBLArray, k);
        For i := 0 to k-1 do
          myDBLArray[i] := Temp^[i + 1];

        Freemem(Temp, SizeOF(Temp^[1])*NumNodes);
      End;
    End
    ELSE myDBLArray[0]  :=  0;
    myPointer :=  @(myDBLArray[0]);
    mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
  end;
  17: begin                                            // Circuit.AllNodeNamesByPhase
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    PInt  :=  myPointer;
    Phase :=  integer(PInt^);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        // Make a Temporary Array big enough to hold all nodes
        Temp2 := AllocStringArray(NumNodes);

        // Find nodes connected to specified phase
        k:=0;
        FOR i := 1 to NumBuses DO  Begin
          NodeIdx := Buses^[i].FindIdx(Phase);
          If NodeIdx > 0 then   // Node found with this phase number
          Begin
            Inc(k);
            Temp2^[k] := Format('%s.%d',[BusList.Get(i), Phase]);
          End;
        End;

        // Assign to result and free temp array
        For i := 0 to k-1 do
        Begin
          WriteStr2Array(Temp2^[i + 1]);
          WriteStr2Array(Char(0));
        End;

        FreeStringArray(Temp2, NumNodes);
      End;
    End
    ELSE WriteStr2Array('');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
  18: begin                                            // Circuit.YNodeVArray
    myType  :=  3;        // Complex
    setlength(myCmplxArray, 1);
    myPointer :=  @(myCmplxArray[0]);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
          myPointer := @(ActiveCircuit[ActiveActor].Solution.NodeV^[1]);
      End;
    End
    ELSE  myCmplxArray[0] := cmplx(0,0);
    mySize    :=  SizeOf(ActiveCircuit[ActiveActor].Solution.NodeV^[1]) * ActiveCircuit[ActiveActor].NumNodes;
  end;
  19: begin                                            // Circuit.YNodeOrder
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        FOR i := 1 to NumNodes DO
        Begin
          With MapNodeToBus^[i] do
            WriteStr2Array(Format('%s.%-d',[Uppercase(BusList.Get(Busref)), NodeNum]));
            WriteStr2Array(Char(0));
        End;
      End;
    End
    ELSE WriteStr2Array('');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
  20: begin                                            // Circuit.YCurrents
    myType  :=  3;        // Complex
    setlength(myCmplxArray, 1);
    myPointer :=  @(myCmplxArray[0]);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
          myPointer := @(ActiveCircuit[ActiveActor].Solution.Currents^[1]);
    End
    ELSE myCmplxArray[k] := cmplx(0,0);
    mySize    :=  SizeOf(ActiveCircuit[ActiveActor].Solution.Currents^[1]) * ActiveCircuit[ActiveActor].NumNodes;
  end
  else
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    WriteStr2Array('Error, parameter not recognized');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
end;
end.
