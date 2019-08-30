unit ExportResults;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
   2-25-00 Created
   5-30-00 Added code for handling positive sequence mode
}

INTERFACE

Uses EnergyMeter;

Procedure ExportVoltages(FileNm:String);
Procedure ExportSeqVoltages(FileNm:String);
Procedure ExportCurrents(FileNm:String);
Procedure ExportEstimation(Filenm:String);
Procedure ExportSeqCurrents(FileNm:String);
Procedure ExportPowers(FileNm:String; opt :Integer);
Procedure ExportPbyphase(FileNm:String; opt :Integer);
Procedure ExportSeqPowers(FileNm:String; opt :Integer);
Procedure ExportFaultStudy(FileNm:String);
Procedure ExportMeters(FileNm:String);
Procedure ExportGenMeters(FileNm:String);
Procedure ExportPVSystemMeters(FileNm:String);
Procedure ExportPVSystem2Meters(FileNm:String);
Procedure ExportStorageMeters(FileNm:String);
Procedure ExportLoads(FileNm :String);
Procedure ExportCapacity(FileNm:String);
Procedure ExportOverloads(FileNm:String);
Procedure ExportUnserved(FileNm:String; UE_Only:Boolean);
Procedure ExportYprim(FileNm:String);
Procedure ExportY(FileNm:String; TripletOpt:Boolean);
Procedure ExportSeqZ(FileNm:String);
Procedure ExportBusCoords(FileNm:String);
Procedure ExportLosses(FileNm:String);
Procedure ExportGuids(FileNm:String);
Procedure ExportCounts(FileNm:String);
Procedure ExportSummary(FileNm:String);
Procedure ExportProfile(FileNm:String; PhasesToPlot:Integer);
Procedure ExportEventLog(FileNm:String);
Procedure ExportVoltagesElements(FileNm:String);
Procedure ExportGICMvar(FileNm:String);
Procedure ExportBusReliability(FileNm:String);
Procedure ExportBranchReliability(FileNm:String);
Procedure ExportNodeNames(FileNm:String);
Procedure ExportTaps(FileNm:String);
Procedure ExportNodeOrder(FileNm:String);
Procedure ExportElemCurrents(FileNm:String);
Procedure ExportElemVoltages(FileNm:String);
Procedure ExportElemPowers(FileNm:String);
Procedure ExportResult(FileNm:String);
Procedure ExportYNodeList(FileNM:String);
Procedure ExportYVoltages(FileNM:String);
Procedure ExportYCurrents(FileNM:String);
Procedure ExportSections(FileNM:String; pMeter:TEnergyMeterObj);
Procedure ExportErrorLog(FileNm:String);
Procedure ExportIncMatrix(FileNm:String);
Procedure ExportIncMatrixRows(FileNm:String);
Procedure ExportIncMatrixCols(FileNm:String);
Procedure ExportBusLevels(FileNm:String);
Procedure ExportLaplacian(FileNm:String);
Procedure ExportZLL(FileNm:String);
Procedure ExportZCC(FileNm:String);
Procedure ExportY4(FileNm:String);
Procedure ExportC(FileNm:String);


IMPLEMENTATION

Uses uComplex,  Arraydef, Sysutils,   Circuit, DSSClassDefs, DSSGlobals,
     uCMatrix,  solution, CktElement, Utilities, Bus, MathUtil, DSSClass,
     PDElement, PCElement, Generator,  Sensor, Load, RegControl, Transformer,
     ParserDel, Math, Ymatrix, LineGeometry, WireData, LineCode, XfmrCode, NamedObject,
     GICTransformer, PVSystem, PVSystem2, Storage, KLUSolve;

Procedure WriteElementVoltagesExportFile(Var F:TextFile; pElem:TDSSCktElement;MaxNumNodes:Integer);

Var
     NCond, Nterm, i,j,k,m,  nref, bref:Integer;
     Busname:String;
     Volts:Complex;
     Vpu, Vmag:Double;

Begin
  NCond := pElem.NConds;
  Nterm := pElem.Nterms;
  k:=0;
  BusName := (StripExtension(pElem.FirstBus));
  Write(F, Format('%s.%s',[pElem.DSSClassName, pElem.Name]));


  Write(F, Format(',%d',[NTerm]));

  FOR j := 1 to NTerm Do Begin
    Write(F, Format(',%d,',[j]));
    Write(F, Format('%d,%d,',[NCond,pElem.NPhases]));

    Write(F,Format('%s,',[UpperCase(BusName)]));
    For i := 1 to NCond Do Begin
       Inc(k);
       nref := pElem.NodeRef^[k];
       Volts := ActiveCircuit[ActiveActor].Solution.NodeV^[nref];
       Vmag := Cabs(Volts)*0.001;
       IF nref=0 Then Vpu := 0.0
       else With ActiveCircuit[ActiveActor] Do Begin
           bref := MapNodeToBus^[nref].BusRef;
           If Buses^[bref].kvbase <> 0.0
           Then Vpu := Vmag / Buses^[bref].kVBase
           Else Vpu := 0.0;

           if (i = 1) Then Write (F, Format('%6.3f',[Buses^[bref].kvBase*sqrt(3)]));
       End;
       With ActiveCircuit[ActiveActor] Do Begin
           Write(F,Format(', %d, %10.6g, %6.3f, %9.5g',[k, Vmag, cdang(Volts), Vpu]));
       END; //end with ActiveCircuit

    End; //end numconductors

   {Zero Fill row}
    For m :=  (NCond + 1) to (MaxNumNodes) DO Write(F, ', 0, 0, 0, 0');

    BusName := StripExtension(pElem.Nextbus);
  End; // end for numterminals
End; //end procedure



// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportSeqVoltages(FileNm:String);

// Export Symmetrical Component bus voltages

Var
   F :TextFile;
   i, j :Integer;
   nref :Integer;
   Vph, VphLL, V012 : Array[1..3] of Complex;

   V0, V1, V2,
   Vpu, V2V1 ,V0V1  : Double;
   Vresidual        : Complex;
   V_NEMA           : Double;

Begin

  Try
     Assignfile(F, FileNm);
     ReWrite(F);

     Writeln(F,'Bus,  V1,  p.u.,Base kV, V2, %V2/V1, V0, %V0/V1, Vresidual, %NEMA');
     WITH ActiveCircuit[ActiveActor] DO
     BEGIN
       FOR i := 1 to NumBuses DO
       BEGIN

           IF Buses^[i].NumNodesThisBus < 3
           THEN BEGIN
                V0 := 0.0;
                V2 := 0.0;
                V_NEMA := 0.0;
                IF (Buses^[i].NumNodesThisBus = 1) and PositiveSequence
                THEN  BEGIN // first node
                   nref := Buses^[i].GetRef(1);
                   Vph[1] := ActiveCircuit[ActiveActor].Solution.NodeV^[nref];
                   V1 := Cabs(Vph[1]);
                END
                ELSE V1 := 0.0;
           END
           ELSE BEGIN

             With  ActiveCircuit[ActiveActor].Solution, Buses^[i] Do
             FOR j := 1 to 3 DO
             BEGIN      // first nodes named  1, 2, 3
               Vph[j] := NodeV^[GetRef(FindIdx(j))];
             END;

             {Compute LL voltages for Nema unbalance calc}
             VphLL[1] := Csub(Vph[1], Vph[2]);
             VphLL[2] := Csub(Vph[2], Vph[3]);
             VphLL[3] := Csub(Vph[3], Vph[1]);

             Phase2SymComp(@Vph, @V012);

             V0 := Cabs(V012[1]);
             V1 := Cabs(V012[2]);
             V2 := Cabs(V012[3]);

             V_NEMA := PctNemaUnbalance(@VphLL);
         END;

         IF   Buses^[i].kvbase <> 0.0
         THEN Vpu := 0.001 * V1 / Buses^[i].kVBase
         ELSE Vpu := 0.0;

         IF V1>0.0 THEN Begin
            V2V1 := 100.0*V2/V1;
            V0V1 := 100.0*V0/V1;
         End Else Begin
            V2V1 := 0.0;
            V0V1 := 0.0;
         End;

         Vresidual := CZERO;
         With ActiveCircuit[ActiveActor].Solution do
         For j := 1 to Buses^[i].NumNodesThisBus Do Caccum(Vresidual, NodeV^[Buses^[i].GetRef(j)]);

         Writeln(F,
         Format('"%s", %10.6g, %9.5g, %8.2f, %10.6g, %8.4g, %10.6g, %8.4g, %10.6g, %8.4g',
                [Uppercase(BusList.Get(i)), V1, Vpu, (Buses^[i].kvbase*SQRT3), V2, V2V1, V0, V0V1, Cabs(Vresidual), V_NEMA]
         ));


       END;
     END;


     GlobalResult := FileNm;

  FINALLY

     CloseFile(F);
  End;

End;

//-------------------------------------------------------------------
Procedure ExportVoltages(FileNm:String);

// Export Symmetrical Component bus voltages

Var
   MaxNumNodes  :Integer ;
   F         :TextFile;
   i, j, jj  :Integer;
   BusName   :String;
   Volts     :Complex;
   nref      :Integer;
   NodeIdx   :Integer;
   Vmag,
   Vpu       : Double;


Begin

  {Find max nodes at a bus}
  MaxNumNodes := 0;
  With ActiveCircuit[ActiveActor] Do
  For i := 1 to NumBuses Do
     MaxNumNodes := max(MaxNumNodes, Buses^[i].NumNodesThisBus);

  Try
     Assignfile(F, FileNm);
     ReWrite(F);


     Write(F,'Bus, BasekV');
     For i := 1 to MaxNumNodes Do Write(F,Format(', Node%d, Magnitude%d, Angle%d, pu%d',[i, i, i, i]));
     Writeln(F);

     WITH ActiveCircuit[ActiveActor] DO BEGIN
       FOR i := 1 to NumBuses DO BEGIN
           BusName := BusList.Get(i);
           Write(F,Format('"%s", %.5g', [UpperCase(BusName), Buses^[i].kvbase*SQRT3]));

           jj := 1;
           With Buses^[i] Do
           For j := 1 to NumNodesThisBus DO BEGIN
             Repeat
                 NodeIdx := FindIdx(jj);     // Try to find nodes in order
                 inc(jj)
             Until NodeIdx>0;
             nref := GetRef(NodeIdx);
             Volts := ActiveCircuit[ActiveActor].Solution.NodeV^[nref];
             Vmag := Cabs(Volts);
             If kvbase <> 0.0 then Vpu := 0.001 * Vmag / kVBase
             Else Vpu := 0.0;

             Write(F,
             Format(', %d, %10.6g, %6.1f, %9.5g',
                    [GetNum(NodeIdx), Vmag, cdang(Volts), Vpu]));
           END;
           {Zero Fill row}
           For j :=  Buses^[i].NumNodesThisBus+1 to MaxNumNodes DO Write(F, ', 0, 0, 0, 0');
           Writeln(F);
       END;
     END;

    GlobalResult := FileNm;

  FINALLY

     CloseFile(F);

  End;

End;
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

PROCEDURE CalcAndWriteSeqCurrents(Var F:TextFile; j:Integer; pelem:TDSSCktElement; cBuffer:pComplexArray; DoRatings:Boolean);
VAR
  I0,I1,I2, I2I1, I0I1, iNormal,iEmerg :Double;
  i,k,NCond:integer;
  Iph, I012 : Array[1..3] of Complex;
  Iresidual:Complex;
  I_NEMA  :Double;


Begin
  NCond := pelem.NConds;
  IF (pelem.Nphases >= 3) THEN BEGIN

      For i := 1 to 3 Do
      Begin
         k := (j-1)*Ncond + i;
         Iph[i] :=cBuffer^[k];
      End;

      Phase2SymComp(@Iph, @I012);
      I0 := Cabs(I012[1]);
      I1 := Cabs(I012[2]);
      I2 := Cabs(I012[3]);

      I_NEMA := PctNemaUnbalance(@Iph);

  END
  ELSE BEGIN
     I0 := 0.0;
     I1 := 0.0;
     I2 := 0.0;
     I_NEMA := 0.0;
     IF ActiveCircuit[ActiveActor].PositiveSequence    // Use phase 1 only
     THEN I1 := Cabs(Iph[1]);

  END;

   IF I1>0.0 THEN Begin
    I2I1 := 100.0*I2/I1;
    I0I1 := 100.0*I0/I1;
   End Else Begin
    I2I1 := 0.0;
    I0I1 := 0.0;
   End;

   IF  DoRatings And (j = 1)  // Only for 1st Terminal
   THEN Begin
        iNormal := TPDElement(Pelem).NormAmps;
        IF iNormal > 0.0 Then iNormal := I1/iNormal*100.0;
        iEmerg :=  TPDElement(Pelem).EmergAmps;
        IF iEmerg > 0.0 THEN iEmerg := I1/iEmerg*100.0;
   End
   ELSE Begin
        iNormal := 0.0;
        iEmerg := 0.0;
   End;

   Iresidual := CZERO;
   For i := 1 to Ncond Do Caccum(Iresidual, cBuffer^[i]);


  Writeln(F, Format('"%s", %3d, %10.6g, %8.4g, %8.4g, %10.6g, %8.4g, %10.6g, %8.4g, %10.6g, %8.4g',
                    [(pelem.DSSClassName + '.' + UpperCase(pelem.Name)),j,I1,iNormal,iEmerg,I2,I2I1,I0,I0I1, Cabs(Iresidual), I_NEMA]));
End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportSeqCurrents(FileNm:String);

Var
    F       :TextFile;
    j:Integer;
    pElem :TDSSCktElement;
    PDElem:TPDElement;
    PCelem:TPCelement;
    cBuffer :pComplexArray;  // Allocate to max total conductors

Begin

  cBuffer := nil;

  Try
     Assignfile(F, FileNm);
     ReWrite(F);


     {Sequence Currents}
     Writeln(F,'Element, Terminal,  I1, %Normal, %Emergency, I2, %I2/I1, I0, %I0/I1, Iresidual, %NEMA');

     {Allocate cBuffer big enough for largest circuit element}
     Getmem(cbuffer, sizeof(cBuffer^[1])* GetMaxCktElementSize);


     //Sources First
     Pelem := ActiveCircuit[ActiveActor].Sources.First;
     WHILE pelem<>nil DO BEGIN
       IF (pelem.Enabled)  THEN BEGIN
        pelem.GetCurrents(cBuffer, ActiveActor);
        FOR j := 1 to pelem.Nterms Do CalcAndWriteSeqCurrents(F, j, pelem, cBuffer, FALSE);
       END;
        pelem := ActiveCircuit[ActiveActor].Sources.Next;
     END;


     // PDELEMENTS Next
     PDelem := ActiveCircuit[ActiveActor].PDElements.First;

     WHILE PDelem<>nil DO BEGIN
       IF (PDelem.Enabled) THEN BEGIN
        PDelem.GetCurrents(cBuffer, ActiveActor);
        FOR j := 1 to PDelem.Nterms Do  CalcAndWriteSeqCurrents(F, j, pDelem, cBuffer, TRUE);
       END;
        PDelem := ActiveCircuit[ActiveActor].PDElements.Next;
     END;

    // PCelemENTS next
     PCelem := ActiveCircuit[ActiveActor].PCelements.First;

     WHILE PCelem<>nil DO BEGIN
       IF (PCelem.Enabled)
       THEN BEGIN
        PCelem.GetCurrents(cBuffer, ActiveActor);
        FOR j := 1 to PCelem.Nterms Do CalcAndWriteSeqCurrents(F, j, pCelem, cBuffer, FALSE);
       END;
        PCelem := ActiveCircuit[ActiveActor].PCelements.Next;
     END;


     //Faults Next
     Pelem := ActiveCircuit[ActiveActor].Faults.First;
     WHILE pelem<>nil DO BEGIN
       IF (pelem.Enabled)
       THEN BEGIN
        pelem.GetCurrents(cBuffer, ActiveActor);
        FOR j := 1 to pelem.Nterms Do CalcAndWriteSeqCurrents(F, j, pelem, cBuffer, FALSE);
       END;
        pelem := ActiveCircuit[ActiveActor].Faults.Next;
     END;

     GlobalResult := FileNm;
     

  FINALLY
     If Assigned(Cbuffer) then Freemem(cBuffer);
     CloseFile(F);

  End;
End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

PROCEDURE CalcAndWriteCurrents(Var F:TextFile; pElem:TDSSCktElement; Cbuffer:pComplexArray; CondWidth, TermWidth:Integer);
VAr
    i,j,k:Integer;
    Iresid:Complex;

Begin
    k:=0;
    Write(F, Format('%s', [pelem.DSSClassName+'.'+UpperCase(pElem.Name)]));
    For      j := 1 to pElem.Nterms Do Begin
      Iresid := CZERO;
      For    i := 1 to pElem.NConds  Do Begin
         Inc(k);
         Write(F,
         Format(', %10.6g, %8.2f',  [Cabs(cBuffer^[k]),cdang(cBuffer^[k])]));
         Caccum(Iresid,cBuffer^[k]);
      End;
      For i := pElem.Nconds+1 To CondWidth do Write(F, Format(', %10.6g, %8.2f',  [0.0,0.0]));
      Write(F,Format(', %10.6g, %8.2f',  [Cabs(Iresid),cdang(Iresid)]));
    END;

    {Filler if no. terms less than termwidth}
    For j := pElem.Nterms+1 to TermWidth Do
      For i := 1 to Condwidth+1 Do Write(F, Format(', %10.6g, %8.2f',  [0.0,0.0]));

    Writeln(F);
End;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

PROCEDURE CalcAndWriteMaxCurrents(Var F:TextFile; pElem:TPDElement; Cbuffer:pComplexArray);
VAr
    i :Integer;
    Currmag, MaxCurrent :Double;
    LocalPower :Complex;

Begin
    Write(F, Format('%s.%s', [pelem.DSSClassName, UpperCase(pElem.Name)]));
    MaxCurrent := 0.0;
    For    i := 1 to pElem.Nphases  Do Begin
       Currmag := Cabs(Cbuffer^[i]);
       If Currmag  > MaxCurrent then   MaxCurrent :=  Currmag;
    End;
    //----pElem.ActiveTerminalIdx := 1;
    LocalPower := CmulReal(pElem.Power[1,ActiveActor], 0.001);
    If (pElem.NormAmps=0.0) or (pElem.EmergAmps=0.0) then
         Write(F,Format(', %10.6g, %8.2f, %8.2f',  [MaxCurrent, 0.0 , 0.0]))
    Else Write(F,Format(', %10.6g, %8.2f, %8.2f',  [MaxCurrent, MaxCurrent/pElem.NormAmps*100.0 , MaxCurrent/pElem.Emergamps*100.0]));
    Write(F, Format(', %10.6g, %10.6g, %d, %d, %d', [Localpower.re, Localpower.im, pElem.BranchNumCustomers, pElem.BranchTotalCustomers, pElem.NPhases   ]));
    With ActiveCircuit[ActiveActor] Do Write(F, Format(', %-.3g ', [Buses^[MapNodeToBus^[PElem.NodeRef^[1]].BusRef].kVBase ]));
    Writeln(F);
End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportCurrents(FileNm:String);

Var
    F          :TextFile;
    cBuffer    :pComplexArray;
    pElem      :TDSSCktElement;
    MaxCond, MaxTerm   :Integer;
    i,j        :Integer;

Begin

  cBuffer := nil;

  Try
     Assignfile(F, FileNm);
     ReWrite(F);

     Getmem(cBuffer, sizeof(cBuffer^[1])*GetMaxCktElementSize);

     {Calculate the width of the file}
     MaxCond := 1;
     MaxTerm := 2;
     pElem := ActiveCircuit[ActiveActor].CktElements.First;
     While pElem<>nil Do Begin
        If pelem.NTerms > MaxTerm Then MaxTerm := pelem.NTerms;
        If pelem.NConds > MaxCond Then MaxCond:= pelem.NConds;
        pElem := ActiveCircuit[ActiveActor].CktElements.Next;
     End;


     {Branch Currents}
     Write(F,'Element');
     For i := 1 to MaxTerm Do Begin
     For j := 1 to MaxCond Do Write(F, Format(', I%d_%d, Ang%d_%d',[i,j,i,j]));
     Write(F, Format(', Iresid%d, AngResid%d',[i ,i]));
     End;
     Writeln(F);


     // Sources first
     pElem := ActiveCircuit[ActiveActor].Sources.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
          pElem.GetCurrents(cBuffer, ActiveActor);
          CalcAndWriteCurrents(F, pElem, Cbuffer, maxcond, maxterm);
       END;
        pElem := ActiveCircuit[ActiveActor].Sources.Next;
     END;


     // PDELEMENTS first
     pElem := ActiveCircuit[ActiveActor].PDElements.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
        pElem.GetCurrents(cBuffer, ActiveActor);
        CalcAndWriteCurrents(F, pElem, Cbuffer, maxcond, maxterm);
       END;
        pElem := ActiveCircuit[ActiveActor].PDElements.Next;
     END;

     // Faults
     pElem := ActiveCircuit[ActiveActor].Faults.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
        pElem.GetCurrents(cBuffer, ActiveActor);
        CalcAndWriteCurrents(F, pElem, Cbuffer, maxcond, maxterm);
       END;
        pElem := ActiveCircuit[ActiveActor].Faults.Next;
     END;

     // PCELEMENTS next
     pElem := ActiveCircuit[ActiveActor].PCElements.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
        pElem.GetCurrents(cBuffer, ActiveActor);
        CalcAndWriteCurrents(F, pElem, Cbuffer, maxcond, maxterm);
       END;
        pElem := ActiveCircuit[ActiveActor].PCElements.Next;
     END;

     GlobalResult := FileNm;


  FINALLY
     If Assigned(cBuffer) Then Freemem(cBuffer);
     CloseFile(F);

  End;

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

PROCEDURE WriteNodeList(Var F:TextFile; const CktElementName:String);
VAR
  NValues, i: Integer;


Begin


  If ActiveCircuit[ActiveActor] <> nil Then
  If Not ActiveCircuit[ActiveActor].Issolved  Then
  Begin
      DoSimpleMsg('Circuit must be solved for this command to execute properly.', 222001);
      Exit;
  End;


  If Length(CktElementName) > 0  Then
  Begin
    SetObject(CktElementName);



      If Assigned(ActiveCircuit[ActiveActor].ActiveCktElement) Then
       WITH ActiveCircuit[ActiveActor].ActiveCktElement DO
       Begin
           Write(F, Format('"%s", %d, %d',[CktElementName, Nterms, Nconds ]));
           NValues := NConds*Nterms;
           For i := 1 to  NValues DO
           Begin
              Write(F, Format(', %d',[GetNodeNum(NodeRef^[i]) ]));
           End;
           Writeln(F);
       End
  End;


end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportNodeOrder(FileNm:String);

{ Writes NodeLists in same order as Export Currents function
}

Var
    F          :TextFile;
    pElem      :TDSSCktElement;
    strName    :String;

Begin


  Try
     Assignfile(F, FileNm);
     ReWrite(F);

     {Header Record}
     Write(F,'Element, Nterminals, Nconductors, Node-1, Node-2, Node-3, ...');
     Writeln(F);


     // Sources first
     pElem := ActiveCircuit[ActiveActor].Sources.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
           strName := pElem.ParentClass.Name + '.' + pElem.Name;
           WriteNodeList(F, strName);
       END;
        pElem := ActiveCircuit[ActiveActor].Sources.Next;
     END;


     // PDELEMENTS first
     pElem := ActiveCircuit[ActiveActor].PDElements.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
           strName := pElem.ParentClass.Name + '.' + pElem.Name;
           WriteNodeList(F, strName);
       END;
        pElem := ActiveCircuit[ActiveActor].PDElements.Next;
     END;

     // Faults
     pElem := ActiveCircuit[ActiveActor].Faults.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
           strName := pElem.ParentClass.Name + '.' + pElem.Name;
           WriteNodeList(F, strName);
       END;
        pElem := ActiveCircuit[ActiveActor].Faults.Next;
     END;

     // PCELEMENTS next
     pElem := ActiveCircuit[ActiveActor].PCElements.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
           strName := pElem.ParentClass.Name + '.' + pElem.Name;
           WriteNodeList(F, strName);
       END;
        pElem := ActiveCircuit[ActiveActor].PCElements.Next;
     END;

     GlobalResult := FileNm;


  FINALLY
     CloseFile(F);

  End;

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

PROCEDURE WriteElemCurrents(Var F:TextFile; const CktElementName:String);
VAR
  NValues, i: Integer;



Begin


  If ActiveCircuit[ActiveActor] <> nil Then
  If Not ActiveCircuit[ActiveActor].Issolved  Then
  Begin
      DoSimpleMsg('Circuit must be solved for this command to execute properly.', 222001);
      Exit;
  End;


  If Length(CktElementName) > 0  Then
  Begin
    SetObject(CktElementName);

      If Assigned(ActiveCircuit[ActiveActor].ActiveCktElement) Then
       WITH ActiveCircuit[ActiveActor].ActiveCktElement DO
       Begin
           ComputeIterminal(ActiveActor);
           Write(F, Format('"%s", %d, %d',[CktElementName, Nterms, Nconds ]));
           NValues := NConds*Nterms;
           For i := 1 to  NValues DO
           Begin
              Write(F, Format(', %10.6g, %8.2f',  [Cabs(Iterminal^[i]), cdang(Iterminal^[i])]));
           End;
           Writeln(F);
       End
  End;


end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportElemCurrents(FileNm:String);

{ Export currents in same order as NodeOrder export
}
Var
    F          :TextFile;
    pElem      :TDSSCktElement;
    strName    :String;

Begin


  Try
     Assignfile(F, FileNm);
     ReWrite(F);

     {Header Record}
     Write(F,'Element, Nterminals, Nconductors, I_1, Ang_1, ...');
     Writeln(F);


     // Sources first
     pElem := ActiveCircuit[ActiveActor].Sources.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
           strName := pElem.ParentClass.Name + '.' + pElem.Name;
           WriteElemCurrents(F, strName);
       END;
        pElem := ActiveCircuit[ActiveActor].Sources.Next;
     END;


     // PDELEMENTS first
     pElem := ActiveCircuit[ActiveActor].PDElements.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
           strName := pElem.ParentClass.Name + '.' + pElem.Name;
           WriteElemCurrents(F, strName);
       END;
        pElem := ActiveCircuit[ActiveActor].PDElements.Next;
     END;

     // Faults
     pElem := ActiveCircuit[ActiveActor].Faults.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
           strName := pElem.ParentClass.Name + '.' + pElem.Name;
           WriteElemCurrents(F, strName);
       END;
        pElem := ActiveCircuit[ActiveActor].Faults.Next;
     END;

     // PCELEMENTS next
     pElem := ActiveCircuit[ActiveActor].PCElements.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
           strName := pElem.ParentClass.Name + '.' + pElem.Name;
           WriteElemCurrents(F, strName);
       END;
        pElem := ActiveCircuit[ActiveActor].PCElements.Next;
     END;

     GlobalResult := FileNm;


  FINALLY
     CloseFile(F);

  End;
End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

PROCEDURE WriteElemVoltages(Var F:TextFile; const CktElementName:String);
VAR
  NValues, i: Integer;



Begin


  If ActiveCircuit[ActiveActor] <> nil Then
  If Not ActiveCircuit[ActiveActor].Issolved  Then
  Begin
      DoSimpleMsg('Circuit must be solved for this command to execute properly.', 222001);
      Exit;
  End;


  If Length(CktElementName) > 0  Then
  Begin
    SetObject(CktElementName);

      If Assigned(ActiveCircuit[ActiveActor].ActiveCktElement) Then
       WITH ActiveCircuit[ActiveActor].ActiveCktElement DO
       Begin
           ComputeVterminal(ActiveActor);
           Write(F, Format('"%s", %d, %d',[CktElementName, Nterms, Nconds ]));
           NValues := NConds*Nterms;
           For i := 1 to  NValues DO
           Begin
              Write(F, Format(', %10.6g, %8.2f',  [Cabs(Vterminal^[i]), cdang(Vterminal^[i])]));
           End;
           Writeln(F);
       End
  End;


end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportElemVoltages(FileNm:String);
{ Export conductor voltages in same order as NodeOrder export
}
Var
    F          :TextFile;
    pElem      :TDSSCktElement;
    strName    :String;

Begin


  Try
     Assignfile(F, FileNm);
     ReWrite(F);

     {Header Record}
     Write(F,'Element, Nterminals, Nconductors, V_1, Ang_1, ...');
     Writeln(F);


     // Sources first
     pElem := ActiveCircuit[ActiveActor].Sources.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
           strName := pElem.ParentClass.Name + '.' + pElem.Name;
           WriteElemVoltages(F, strName);
       END;
        pElem := ActiveCircuit[ActiveActor].Sources.Next;
     END;


     // PDELEMENTS first
     pElem := ActiveCircuit[ActiveActor].PDElements.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
           strName := pElem.ParentClass.Name + '.' + pElem.Name;
           WriteElemVoltages(F, strName);
       END;
        pElem := ActiveCircuit[ActiveActor].PDElements.Next;
     END;

     // Faults
     pElem := ActiveCircuit[ActiveActor].Faults.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
           strName := pElem.ParentClass.Name + '.' + pElem.Name;
           WriteElemVoltages(F, strName);
       END;
        pElem := ActiveCircuit[ActiveActor].Faults.Next;
     END;

     // PCELEMENTS next
     pElem := ActiveCircuit[ActiveActor].PCElements.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
           strName := pElem.ParentClass.Name + '.' + pElem.Name;
           WriteElemVoltages(F, strName);
       END;
        pElem := ActiveCircuit[ActiveActor].PCElements.Next;
     END;

     GlobalResult := FileNm;


  FINALLY
     CloseFile(F);

  End;

End;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

PROCEDURE WriteElemPowers(Var F:TextFile; const CktElementName:String);
VAR
  NValues, i: Integer;
  S : Complex;

Begin


  If ActiveCircuit[ActiveActor] <> nil Then
  If Not ActiveCircuit[ActiveActor].Issolved  Then
  Begin
      DoSimpleMsg('Circuit must be solved for this command to execute properly.', 222001);
      Exit;
  End;


  If Length(CktElementName) > 0  Then
  Begin
    SetObject(CktElementName);

      If Assigned(ActiveCircuit[ActiveActor].ActiveCktElement) Then
       WITH ActiveCircuit[ActiveActor].ActiveCktElement DO
       Begin
           ComputeVterminal(ActiveActor);
           ComputeIterminal(ActiveActor);
           Write(F, Format('"%s", %d, %d',[CktElementName, Nterms, Nconds ]));
           NValues := NConds*Nterms;
           For i := 1 to  NValues DO
           Begin
              S := Cmul(Vterminal^[i], Conjg( Iterminal^[i]));
              Write(F, Format(', %10.6g, %10.6g',  [S.re * 0.001, S.im * 0.001]));
           End;
           Writeln(F);
       End
  End;


end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportElemPowers(FileNm:String);

{ Export conductor powers in same order as NodeOrder export
}
Var
    F          :TextFile;
    pElem      :TDSSCktElement;
    strName    :String;

Begin


  Try
     Assignfile(F, FileNm);
     ReWrite(F);

     {Header Record}
     Write(F,'Element, Nterminals, Nconductors, P_1, Q_1, ...');
     Writeln(F);


     // Sources first
     pElem := ActiveCircuit[ActiveActor].Sources.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
           strName := pElem.ParentClass.Name + '.' + pElem.Name;
           WriteElemPowers(F, strName);
       END;
        pElem := ActiveCircuit[ActiveActor].Sources.Next;
     END;


     // PDELEMENTS first
     pElem := ActiveCircuit[ActiveActor].PDElements.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
           strName := pElem.ParentClass.Name + '.' + pElem.Name;
           WriteElemPowers(F, strName);
       END;
        pElem := ActiveCircuit[ActiveActor].PDElements.Next;
     END;

     // Faults
     pElem := ActiveCircuit[ActiveActor].Faults.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
           strName := pElem.ParentClass.Name + '.' + pElem.Name;
           WriteElemPowers(F, strName);
       END;
        pElem := ActiveCircuit[ActiveActor].Faults.Next;
     END;

     // PCELEMENTS next
     pElem := ActiveCircuit[ActiveActor].PCElements.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
           strName := pElem.ParentClass.Name + '.' + pElem.Name;
           WriteElemPowers(F, strName);
       END;
        pElem := ActiveCircuit[ActiveActor].PCElements.Next;
     END;

     GlobalResult := FileNm;


  FINALLY
     CloseFile(F);

  End;

End;




// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportPowers(FileNm:String; opt :Integer);

{Opt = 0: kVA
 opt = 1: MVA
 }

Var
    F :TextFile;
    Nterm,  j :Integer;
    PDElem :TPDElement;
    PCElem :TPCElement;
    S:Complex;
    Separator :String;

Begin


  Try
     Assignfile(F, FileNm);
     ReWrite(F);
     Separator := ', ';


     CASE Opt of
          1:Writeln(F,'Element, Terminal, P(MW), Q(Mvar), P_Normal, Q_Normal, P_Emergency, Q_Emergency');
     ELSE
          Writeln(F,'Element, Terminal, P(kW), Q(kvar),  P_Normal, Q_Normal, P_Emergency, Q_Emergency');
     End;

     // PDELEMENTS first
     PDElem := ActiveCircuit[ActiveActor].PDElements.First;

     WHILE PDElem <> nil DO
     BEGIN
       IF (PDElem.Enabled)
       THEN BEGIN
        Nterm := pDElem.Nterms;

        FOR j := 1 to NTerm Do
        Begin
          Write(F,  Pad('"'+PDelem.DSSClassName + '.' + UpperCase(PDElem.Name)+'"', 24), Separator, j:3);
           //----PDElem.ActiveTerminalIdx := j;
           S := PDElem.Power[j,ActiveActor];
           If Opt=1 Then S := CmulReal(S, 0.001);
           Write(F, Separator, S.re*0.001:11:1);
           Write(F, Separator, S.im*0.001:11:1);
           If j = 1  Then begin
             //----PDelem.ActiveTerminalIdx := 1;
             S := PDElem.ExcesskVANorm[1,ActiveActor];
             If Opt=1 Then S := CmulReal(S, 0.001);
             Write(F, Separator, Abs(S.re):11:1);
             Write(F, Separator, Abs(S.im):11:1);
             S := PDElem.ExcesskVAEmerg[1,ActiveActor];
             If Opt=1 Then S := CmulReal(S, 0.001);
             Write(F, Separator, Abs(S.re):11:1);
             Write(F, Separator, Abs(S.im):11:1);
           End;
           Writeln(F);
        END;
       END;
        PDElem := ActiveCircuit[ActiveActor].PDElements.Next;
     END;

     // PCELEMENTS Next
     PCElem := ActiveCircuit[ActiveActor].PCElements.First;

     WHILE PCElem <> nil DO
     BEGIN

       IF (PCElem.Enabled) THEN
       BEGIN
        Nterm := PCElem.Nterms;

        FOR j := 1 to NTerm Do
        Begin
           Write(F,  Pad('"'+PCElem.DSSClassName + '.' + UpperCase(PCElem.Name)+'"', 24), Separator, j:3);
           //----pcElem.ActiveTerminalIdx := j;
           S := pCElem.Power[j,ActiveActor] ;
           If Opt=1 Then S := CmulReal(S, 0.001);
           Write(F, Separator, S.re*0.001:11:1);
           Write(F, Separator, S.im*0.001:11:1);
           Writeln(F);

        END;
       END;
        PCElem := ActiveCircuit[ActiveActor].PCElements.Next;
     END;

     GlobalResult := FileNm;

  FINALLY
     CloseFile(F);

  End;
End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportLosses(FileNm:String);

{Opt = 0: kVA
 opt = 1: MVA
 }

Var
    F :TextFile;
    PDElem :TPDElement;
    S_total, S_Load, S_NoLoad:Complex;

Begin


  Try
     Assignfile(F, FileNm);
     ReWrite(F);

     Writeln(F,'Element,  Total(W), Total(var),  I2R(W), I2X(var), No-load(W), No-load(var)');
     // PDELEMENTS first
     PDElem := ActiveCircuit[ActiveActor].PDElements.First;

     WHILE PDElem <> nil DO
     BEGIN
       IF (PDElem.Enabled)
       THEN BEGIN
            PDElem.GetLosses(S_total, S_Load, S_NoLoad, ActiveActor);
            Writeln(F, Format('%s.%s, %.7g, %.7g, %.7g, %.7g, %.7g, %.7g', [PDElem.ParentClass.Name, UpperCase(PDElem.Name), S_total.re, S_total.im, S_Load.re, S_Load.im, S_NoLoad.re, S_NoLoad.im]));
       END;
        PDElem := ActiveCircuit[ActiveActor].PDElements.Next;
     END;

     GlobalResult := FileNm;

  FINALLY
     CloseFile(F);

  End;
End;

// ===============================================================================
Procedure ExportPbyphase(FileNm:String; opt :Integer);

{ Export Powers by phase }

{Opt = 0: kVA
 opt = 1: MVA
 }

Var
    F :TextFile;
    i :Integer;
    PDElem :TPDElement;
    PCElem :TPCElement;
    S:Complex;

Begin


  Try
     Assignfile(F, FileNm);
     ReWrite(F);

     CASE Opt of
          1: Writeln(F,'Element, NumTerminals, NumConductors, NumPhases, MW1, Mvar1, MW2, Mvar2, MW3, Mvar3, ... ');
     ELSE
          Writeln(F,'Element, NumTerminals, NumConductors, NumPhases, kW1, kvar1, kW2, kvar2, kW3, kvar3, ... ');
     End;

     // PDELEMENTS first
     PDElem := ActiveCircuit[ActiveActor].PDElements.First;

     WHILE PDElem <> nil DO
     BEGIN
       IF (PDElem.Enabled) THEN
       BEGIN
        With PDElem Do Begin
          ComputeITerminal(ActiveActor);
          ComputeVTerminal(ActiveActor);
          Write(F,  Format('"%s.%s", %d, %d, %d', [DSSClassName, Uppercase(Name),  NTerms, NConds, Nphases ]));
          FOR i := 1 to Yorder Do Begin
             S := CmulReal(Cmul(Vterminal^[i], conjg(ITerminal^[i])), 0.001);
             If Opt=1 Then S := CmulReal(S, 0.001);   // convert to MVA
             Write(F, Format(', %10.3f, %10.3f', [S.re, S.im]));
          END;
        End;
        Writeln(F);
       END;
        PDElem := ActiveCircuit[ActiveActor].PDElements.Next;
     END;

     // PCELEMENTS Next
     PCElem := ActiveCircuit[ActiveActor].PCElements.First;

     WHILE PCElem <> nil DO
     BEGIN

       IF (PCElem.Enabled) THEN
       BEGIN
        With PCelem Do Begin
          ComputeITerminal(ActiveActor);
          ComputeVTerminal(ActiveActor);
          Write(F,  Format('"%s.%s", %d, %d, %d', [DSSClassName, Uppercase(Name),  NTerms, NConds, NPhases ]));
          FOR i := 1 to Yorder Do
          Begin
             S := CmulReal(Cmul(Vterminal^[i], conjg(ITerminal^[i])), 0.001);
             If Opt=1 Then S := CmulReal(S, 0.001);   // convert to MVA
             Write(F, Format(', %10.3f, %10.3f', [S.re, S.im]));
          END;
        End;
        Writeln(F);

       END;
        PCElem := ActiveCircuit[ActiveActor].PCElements.Next;
     END;

     GlobalResult := FileNm;

  FINALLY
     CloseFile(F);

  End;
End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportSeqPowers(FileNm:String; opt :Integer);

{Opt = 0: kVA
 opt = 1: MVA
 }

Var
    F :TextFile;
    cBuffer :pComplexArray;
    NCond, Nterm, i, j, k :Integer;
    PDElem :TPDElement;
    PCElem :TPCElement;
    Volts:Complex;
    S:Complex;
    nref:Integer;
    Vph, V012:Array[1..3] of Complex;
    Iph, I012:Array[1..3] of Complex;
    Separator :String;


Begin

  cBuffer := nil;

  Try
     Assignfile(F, FileNm);
     ReWrite(F);
     Separator := ', ';

     Getmem(cBuffer, sizeof(cBuffer^[1]) * GetMaxCktElementSize);

     CASE Opt of
          1:Writeln(F,'Element, Terminal, P1(MW), Q1(Mvar), P2, Q2, P0, Q0, P_Normal, Q_Normal, P_Emergency, Q_Emergency');
     ELSE
          Writeln(F,'Element, Terminal, P1(kW), Q1(kvar), P2, Q2, P0, Q0, P_Normal, Q_Normal, P_Emergency, Q_Emergency');
     End;

     // PDELEMENTS first
     PDElem := ActiveCircuit[ActiveActor].PDElements.First;

     WHILE PDElem <> nil DO
     BEGIN
       IF (PDElem.Enabled)
       THEN BEGIN
        NCond := pDElem.NConds;
        Nterm := pDElem.Nterms;
        PDElem.GetCurrents(cBuffer, ActiveActor);

        FOR j := 1 to NTerm Do
        Begin
          Write(F,  Pad('"'+PDelem.DSSClassName + '.' + Uppercase(PDElem.Name) +'"', 24), Separator, j:3);
          For i := 1 to PDElem.NPhases Do
          Begin
             k := (j-1)*Ncond + i;
             nref := pDElem.NodeRef^[k];
             Volts := ActiveCircuit[ActiveActor].Solution.NodeV^[nref];
             Iph[i] := cBuffer^[k];
             Vph[i] := volts;
          End;
          IF  (PDElem.Nphases>=3)
          THEN Begin
             Phase2SymComp(@Iph, @I012);
             Phase2SymComp(@Vph, @V012);
          End
          ELSE Begin
             V012[1] := CZERO;  I012[1] := CZERO;
             V012[3] := CZERO;  I012[3] := CZERO;
             IF ActiveCircuit[ActiveActor].PositiveSequence
             THEN Begin
                  V012[2] := Vph[1];
                  I012[2] := Iph[1];
             End
             ELSE Begin
                  V012[2] := CZERO;  I012[2] := CZERO;
             End;
          End;

           S := Cmul(V012[2], conjg(I012[2]));
           If Opt=1 Then S := CmulReal(S, 0.001);
           Write(F, Separator, S.re*0.003:11:1);
           Write(F, Separator, S.im*0.003:11:1);
           S := Cmul(V012[3], conjg(I012[3]));
           If Opt=1 Then S := CmulReal(S, 0.001);
           Write(F, Separator, S.re*0.003:11:1);
           Write(F, Separator, S.im*0.003:11:1);
           S := Cmul(V012[1], conjg(I012[1]));
           If Opt=1 Then S := CmulReal(S, 0.001);
           Write(F, Separator, S.re*0.003:8:1);
           Write(F, Separator, S.im*0.003:8:1);

             If j = 1
             Then begin
                 //----PDelem.ActiveTerminalIdx := 1;
                 S := PDElem.ExcesskVANorm[1,ActiveActor];
                 If Opt=1 Then S := CmulReal(S, 0.001);
                 Write(F, Separator, Abs(S.re):11:1);
                 Write(F, Separator, Abs(S.im):11:1);
                 S := PDElem.ExcesskVAEmerg[1,ActiveActor];
                 If Opt=1 Then S := CmulReal(S, 0.001);
                 Write(F, Separator, Abs(S.re):11:1);
                 Write(F, Separator, Abs(S.im):11:1);
             End;
             Writeln(F);

        END;
       END;
        PDElem := ActiveCircuit[ActiveActor].PDElements.Next;
     END;

     // PCELEMENTS Next
     PCElem := ActiveCircuit[ActiveActor].PCElements.First;

     WHILE PCElem <> nil DO
     BEGIN

       IF (PCElem.Enabled)
       THEN BEGIN
        NCond := PCElem.NConds;
        Nterm := PCElem.Nterms;
        PCElem.GetCurrents(cBuffer, ActiveActor);

        FOR j := 1 to NTerm Do
        Begin
          Write(F,  Pad('"'+PCElem.DSSClassName + '.' + Uppercase(PCElem.Name) +'"', 24), Separator, j:3);
          For i := 1 to PCElem.NPhases Do
          Begin
             k := (j-1)*Ncond + i;
             nref := PCElem.NodeRef^[k];
             Volts := ActiveCircuit[ActiveActor].Solution.NodeV^[nref];
             Iph[i] := cBuffer^[k];
             Vph[i] := volts;
          End;
          IF  (PCElem.Nphases>=3)
          THEN Begin
             Phase2SymComp(@Iph, @I012);
             Phase2SymComp(@Vph, @V012);
          End
          ELSE Begin
             V012[1] := CZERO;
             I012[1] := CZERO;
             V012[3] := CZERO;
             I012[3] := CZERO;
             IF ActiveCircuit[ActiveActor].PositiveSequence
             THEN Begin
                  V012[2] := Vph[1];
                  I012[2] := Iph[1];
             End
             ELSE Begin
                  V012[2] := CZERO;  I012[2] := CZERO;
             End;
          End;

           S := Cmul(V012[2], conjg(I012[2]));
           If Opt=1 Then S := CmulReal(S, 0.001);
           Write(F, Separator, S.re*0.003:11:1);
           Write(F, Separator, S.im*0.003:11:1);
           S := Cmul(V012[3], conjg(I012[3]));
           If Opt=1 Then S := CmulReal(S, 0.001);
           Write(F, Separator, S.re*0.003:11:1);
           Write(F, Separator, S.im*0.003:11:1);
           S := Cmul(V012[1], conjg(I012[1]));
           If Opt=1 Then S := CmulReal(S, 0.001);
           Write(F, Separator, S.re*0.003:8:1);
           Write(F, Separator, S.im*0.003:8:1);

           Writeln(F);

        END;
       END;
        PCElem := ActiveCircuit[ActiveActor].PCElements.Next;
     END;

     GlobalResult := FileNm;

  FINALLY
     If Assigned(cBuffer) Then Freemem(CBuffer);
     CloseFile(F);

  End;
End;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportFaultStudy(FileNm:String);

Var
   i, iBus, iphs:Integer;
   YFault  :Tcmatrix;
   Vfault  :pComplexArray;  {Big temp array}
   F       :Textfile ;
   GFault  :complex;
   Separator :String;
   MaxCurr,
   CurrMag :Double;

Begin

  Try

     Assignfile(F, FileNm);
     ReWrite(F);

     Separator := ', ';

   { Set source voltage injection currents }
     With ActiveCircuit[ActiveActor] Do begin
       With Solution Do Begin

     {All Phase Faults}
           Writeln(F,'Bus,  3-Phase,  1-Phase,  L-L');
           FOR iBus := 1 to NumBuses DO
           {Bus Norton Equivalent Current, Isc has been previously computed}
           WITH Buses^[iBus] Do
           Begin
               Write(F,Pad(Uppercase(BusList.Get(iBus)),12));
               MaxCurr := 0.0;
               For i := 1 to NumNodesThisBus Do
               Begin
                  IF MaxCurr < Cabs(BusCurrent^[i])
                  THEN MaxCurr := Cabs(BusCurrent^[i]);;
               End;
               Write(F, Separator, maxCurr:10:0);

           {One Phase Faults}

   { Solve for Fault Injection Currents}

             YFault := TcMatrix.CreateMatrix(NumNodesThisBus);
             Getmem(VFault, Sizeof(Complex)* NumNodesThisBus);

             {Build YscTemp}

             GFault := Cmplx(10000.0, 0.0);

             MaxCurr := 0.0;

             For iphs := 1 to NumNodesThisBus Do Begin
                   YFault.CopyFrom(Ysc);
                   YFault.AddElement(iphs, iphs, GFault);

                   { Solve for Injection Currents}
                   YFault.Invert;
                   YFault.MvMult(VFault, BusCurrent);  {Gets voltage appearing at fault}

                   Currmag := Cabs(Cmul(VFault^[iphs], GFault));
                   If CurrMag > MaxCurr THEN MaxCurr := Currmag;

             End; {For iphase}
             {Now, Stuff it in the Css Array where it belongs}
             Write(F, Separator, maxCurr:10:0);

             Freemem(VFault);
             YFault.Free;

           {Node-Node Faults}

           {Bus Norton Equivalent Current, Isc has been previously computed}

             YFault := TcMatrix.CreateMatrix(NumNodesThisBus);
             Getmem(VFault, Sizeof(VFault^[1])* NumNodesThisBus);

             GFault := Cmplx(10000.0, 0.0);

             MaxCurr := 0.0;

             For iphs := 1 to NumNodesThisBus-1 Do Begin
                   YFault.CopyFrom(Ysc);
                   YFault.AddElement(iphs, iphs, GFault);
                   YFault.AddElement(iphs+1, iphs+1, GFault);
                   YFault.AddElemSym(iphs, iphs+1, Cnegate(GFault));

                   { Solve for Injection Currents}
                   YFault.Invert;
                   YFault.MvMult(VFault,BusCurrent);  {Gets voltage appearing at fault}

                   CurrMag :=   Cabs(Cmul(Csub(VFault^[iphs],VFault^[iphs+1]),GFault));
                   If CurrMag > MaxCurr  THEN MaxCurr := CurrMag;
             End; {For iphase}
             {Now, Stuff it in the Css Array where it belongs}

             Write(F, Separator, MaxCurr:10:0);

             Freemem(VFault);
             YFault.Free;

             Writeln(F);
           End;  {With bus}

       End; {With Solution}
     End; {With ActiveCircuit}

     GlobalResult := Filenm;

  FINALLY

       CloseFile(F);

  End;
End;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportEstimation(Filenm:String);

Var
   F  :TextFile;
   i  :Integer;
   pEnergyMeterObj  :TEnergyMeterObj;
   pSensorObj       :TSensorObj;
   TempX  :Array[1..3] of Double; // temp number buffer

   Procedure ZeroTempXArray;
   var ii:Integer;
   Begin  For ii := 1 to 3 do TempX[ii] := 0.0; End;


Begin

    TRY
          AssignFile(F,  FileNm);
          Rewrite(F);   // clears file

          {Do the EnergyMeters first}
          Writeln(F, '"Energy Meters" ');
          Writeln(F, '"energyMeter", "I1 Target", "I2 Target", "I3 Target", "I1 Calc", "I2 Calc", "I3 Calc", "I1 %Err", "I2 %Err", "I3 %Err"'{, "I1 Factor", "I2 Factor", "I3 Factor"'});

           pEnergyMeterObj := ActiveCircuit[ActiveActor].energyMeters.First;
           WHILE pEnergyMeterObj <> NIL Do  Begin
              IF pEnergyMeterObj.Enabled THEN   BEGIN
                  Write(F, Format('"Energymeter.%s"',[Uppercase(pEnergyMeterObj.Name) ]));
                  {Sensor currents (Target)}
                  ZeroTempXArray;
                  For i := 1 to pEnergyMeterObj.Nphases do TempX[i] := pEnergyMeterObj.SensorCurrent^[i];
                  For i := 1 to 3 do Write(F, Format(', %.6g',[TempX[i]]));
                  {Calculated Currents}
                  ZeroTempXArray;
                  For i := 1 to pEnergyMeterObj.Nphases do TempX[i] := Cabs(pEnergyMeterObj.CalculatedCurrent^[i]);
                  For i := 1 to 3 do Write(F, Format(', %.6g',[TempX[i]]));
                  {Percent Error}
                  For i := 1 to pEnergyMeterObj.Nphases do TempX[i] := (1.0 - TempX[i]/Max(0.001, pEnergyMeterObj.SensorCurrent^[i])) * 100.0;
                  For i := 1 to 3 do Write(F, Format(', %.6g',[TempX[i]]));

                  (****  Not all that useful
                  {Allocation Factors}
                  ZeroTempXArray;
                  For i := 1 to pEnergyMeterObj.Nphases do TempX[i] := pEnergyMeterObj.PhsAllocationFactor^[i];
                  For i := 1 to 3 do Write(F, Format(' %.6g,',[TempX[i]]));
                  *****)

                  Writeln(F);
              END;
              pEnergyMeterObj := ActiveCircuit[ActiveActor].EnergyMeters.Next;
           End;

          {Do the Sensors Next}
          Writeln(F);
          Writeln(F, '"Sensors" ');
          Write(F, '"Sensor", "I1 Target", "I2 Target", "I3 Target", "I1 Calc", "I2 Calc", "I3 Calc", "I1 %Err", "I2 %Err", "I3 %Err",');
          Writeln(F, ' "V1 Target", "V2 Target", "V3 Target", "V1 Calc", "V2 Calc", "V3 Calc", "V1 %Err", "V2 %Err", "V3 %Err", "WLS Voltage Err", "WLS Current Err"');

           pSensorObj := ActiveCircuit[ActiveActor].Sensors.First;
           WHILE pSensorObj <> NIL Do  Begin
              IF pSensorObj.Enabled THEN   BEGIN
                  Write(F, Format('"Sensor.%s"',[Uppercase(pSensorObj.Name) ]));
                  {Sensor currents (Target)}
                  ZeroTempXArray;
                  For i := 1 to pSensorObj.Nphases do TempX[i] := pSensorObj.SensorCurrent^[i];
                  For i := 1 to 3 do Write(F, Format(', %.6g',[TempX[i]]));
                  {Calculated Currents}
                  ZeroTempXArray;
                  For i := 1 to pSensorObj.Nphases do TempX[i] := Cabs(pSensorObj.CalculatedCurrent^[i]);
                  For i := 1 to 3 do Write(F, Format(', %.6g',[TempX[i]]));
                  {Percent Error}
                  For i := 1 to pSensorObj.Nphases do TempX[i] := (1.0 - TempX[i]/Max(0.001, pSensorObj.SensorCurrent^[i])) * 100.0;
                  For i := 1 to 3 do Write(F, Format(', %.6g',[TempX[i]]));
                  {Sensor Voltage (Target)}
                  ZeroTempXArray;
                  For i := 1 to pSensorObj.Nphases do TempX[i] := pSensorObj.SensorVoltage^[i];
                  For i := 1 to 3 do Write(F, Format(', %.6g',[TempX[i]]));
                  {Calculated Voltage}
                  ZeroTempXArray;
                  For i := 1 to pSensorObj.Nphases do TempX[i] := Cabs(pSensorObj.CalculatedVoltage^[i]);
                  For i := 1 to 3 do Write(F, Format(', %.6g',[TempX[i]]));
                  {Percent Error}
                  For i := 1 to pSensorObj.Nphases do TempX[i] := (1.0 - TempX[i]/Max(0.001, pSensorObj.SensorVoltage^[i])) * 100.0;
                  For i := 1 to 3 do Write(F, Format(', %.6g',[TempX[i]]));
                  {WLS Errors}
                  ZeroTempXArray;
                  Write(F, Format(', %.6g, %.6g',[pSensorObj.WLSVoltageError , pSensorObj.WLSCurrentError]));

                  Writeln(F);
              END;
              pSensorObj := ActiveCircuit[ActiveActor].Sensors.Next;
           End;


      FINALLY
        AppendGlobalResult(FileNm);
        CloseFile(F);

      END;


End;



// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure WriteMultipleMeterFiles;

Var
   F  :TextFile;
   i,j:Integer;
   pElem  :TEnergyMeterObj;
   MeterClass:TEnergyMeter;
   FileNm,
   Separator :String;

Begin

      MeterClass := TEnergyMeter(GetDSSClassPtr('Energymeter'));
      If MeterClass = NIL THEN Exit;  // oops somewhere!!
      Separator := ', ';

     pElem := ActiveCircuit[ActiveActor].energyMeters.First;
     WHILE pElem <> NIL Do
     Begin
        IF pElem.Enabled THEN
        BEGIN
          TRY
            FileNm := GetOutputDirectory + 'EXP_MTR_'+Uppercase(pElem.Name)+'.CSV';

            IF Not FileExists(FileNm)
            THEN Begin
                AssignFile(F,  FileNm);
                Rewrite(F);
                {Write New Header}
                Write(F, 'Year, LDCurve, Hour, Meter');
                For i := 1 to NumEMRegisters Do Write(F, Separator, '"'+ pelem.RegisterNames[i]+'"');
                Writeln(F);
                CloseFile(F);
            End;

            AssignFile(F,  FileNm);
            Append(F);
            Write(F,ActiveCircuit[ActiveActor].Solution.Year:0, Separator);
            Write(F,ActiveCircuit[ActiveActor].LoadDurCurve,    Separator);
            Write(F,ActiveCircuit[ActiveActor].Solution.DynaVars.intHour:0, Separator);
            Write(F,Pad('"'+Uppercase(pElem.Name)+'"', 14));
            FOR j := 1 to NumEMRegisters Do Write(F, Separator, PElem.Registers[j]:10:0);
            Writeln(F);
            AppendGlobalResult(FileNm);
          FINALLY
            CloseFile(F);
          END;

        END;
        pElem := ActiveCircuit[ActiveActor].EnergyMeters.Next;
     End;



End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure WriteSingleMeterFile(const FileNm:String);
Var
   F  :TextFile;
   i,j:Integer;
   pElem  :TEnergyMeterObj;
   TestStr,
   Separator :String;
   RewriteFile  :Boolean;
Begin

  Separator := ', ';

 TRY

    IF FileExists(FileNm)
    THEN Begin  // See if it has already been written on
         Assignfile(F, FileNm);
         Reset(F);
         IF  Not EOF(F)
         THEN Begin
             Read(F, TestStr);
             {See if it likely that the file is OK}
             IF  CompareText(Copy(TestStr,1,4), 'Year')=0
             THEN RewriteFile := FALSE       // Assume the file is OK
             ELSE RewriteFile := TRUE;
         End
         ELSE RewriteFile := TRUE;

         CloseFile(F);

    End
    ELSE Begin
         ReWriteFile := TRUE;
         AssignFile(F,  FileNm);
    End;

   {Either open or append the file}
    IF RewriteFile  THEN Begin
        ReWrite(F);
        {Write New Header}
        pElem := ActiveCircuit[ActiveActor].energyMeters.First;
        Write(F, 'Year, LDCurve, Hour, Meter');
        For i := 1 to NumEMRegisters Do Write(F, Separator, '"'+ pElem.RegisterNames[i]+'"');
        Writeln(F);
    END
    ELSE Append(F);


     pElem := ActiveCircuit[ActiveActor].energyMeters.First;
     WHILE pElem <> NIL Do  Begin
        IF pElem.Enabled THEN   BEGIN
            Write(F,ActiveCircuit[ActiveActor].Solution.Year:0, Separator);
            Write(F,ActiveCircuit[ActiveActor].LoadDurCurve,    Separator);
            Write(F,ActiveCircuit[ActiveActor].Solution.DynaVars.intHour:0, Separator);
            Write(F,Pad('"'+ Uppercase(pElem.Name) +'"', 14));
            FOR j := 1 to NumEMRegisters Do Write(F, Separator, PElem.Registers[j]:10:0);
            Writeln(F);
        END;
        pElem := ActiveCircuit[ActiveActor].EnergyMeters.Next;
     End;

     GlobalResult := FileNm;

  FINALLY

     CloseFile(F);

  End;

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportMeters(FileNm:String);

// Export Values of  Meter Elements

// These records are appended to an existing file so a running account is kept for some kinds of simulations

// If switch /m is specified, a separate file is created for each meter using the meter's name

Begin


  If Lowercase(Copy(FileNm,1,2)) = '/m' THEN WriteMultipleMeterFiles
                                        ELSE WriteSingleMeterFile(FileNM);
End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure WriteMultipleGenMeterFiles;

Var
   F  :TextFile;
   i,j:Integer;
   pElem  :TGeneratorObj;
   GeneratorClass:TGenerator;
   FileNm,
   Separator :String;

Begin

      GeneratorClass := TGenerator(GetDSSClassPtr('generator'));
      If GeneratorClass = NIL THEN Exit;  // oops somewhere!!
      Separator := ', ';

     pElem := ActiveCircuit[ActiveActor].Generators.First;
     WHILE pElem <> NIL Do
     Begin
        IF pElem.Enabled THEN
        BEGIN
          TRY
            FileNm := GetOutputDirectory + 'EXP_GEN_' + Uppercase(pElem.Name) + '.CSV';

            IF Not FileExists(FileNm)
            THEN Begin
                AssignFile(F, FileNm);
                Rewrite(F);
                {Write New Header}
                Write(F, 'Year, LDCurve, Hour, Generator');
                For i := 1 to NumGenRegisters Do Write(F, Separator, '"' + GeneratorClass.RegisterNames[i]+'"');
                Writeln(F);
                CloseFile(F);
            End;

            AssignFile(F, FileNm);
            Append(F);
            With ActiveCircuit[ActiveActor] Do Begin
                Write(F,Solution.Year:0, Separator);
                Write(F,LoadDurCurve, Separator);
                Write(F,Solution.DynaVars.intHour:0, Separator);
                Write(F,Pad('"'+Uppercase(pElem.Name)+'"', 14));
                FOR j := 1 to NumGenRegisters Do Write(F, Separator, PElem.Registers[j]:10:0);
                Writeln(F);
            End;
            AppendGlobalResult(FileNm);
          FINALLY
            CloseFile(F);
          END;

        END;
        pElem := ActiveCircuit[ActiveActor].Generators.Next;
     End;

End;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure WriteSingleGenMeterFile(FileNm:String);

Var
   F  :TextFile;
   i,j:Integer;
   pElem  :TGeneratorObj;
   GeneratorClass:TGenerator;
   Separator, TestStr :String;
   ReWriteFile :Boolean;

Begin


  GeneratorClass := TGenerator(GetDSSClassPtr('generator'));
  If GeneratorClass = NIL THEN Exit;  // oops somewhere!!
  Separator := ', ';


 TRY

    IF FileExists(FileNm)
    THEN Begin  // See if it has already been written on
         Assignfile(F,FileNm);
         Reset(F);
         IF  Not EOF(F)
         THEN Begin
             Read(F, TestStr);
             {See if it likely that the file is OK}
             IF  CompareText(Copy(TestStr,1,4), 'Year')=0
             THEN RewriteFile := FALSE       // Assume the file is OK
             ELSE RewriteFile := TRUE;
         End
         ELSE RewriteFile := TRUE;

         CloseFile(F);

    End
    ELSE Begin
         ReWriteFile := TRUE;
         AssignFile(F, FileNm);
    End;

   {Either open or append the file}
    IF RewriteFile
    THEN Begin
        ReWrite(F);
        {Write New Header}
        Write(F, 'Year, LDCurve, Hour, Generator');
        For i := 1 to NumGenRegisters Do Write(F, Separator, '"'+ GeneratorClass.RegisterNames[i]+'"');
        Writeln(F);
    END
    ELSE Append(F);


     pElem := ActiveCircuit[ActiveActor].Generators.First;
     WHILE pElem <> NIL Do
     Begin
        IF pElem.Enabled THEN With ActiveCircuit[ActiveActor] Do
        BEGIN
            Write(F,Solution.Year:0, Separator);
            Write(F,LoadDurCurve, Separator);
            Write(F,Solution.DynaVars.intHour:0, Separator);
            Write(F,Pad('"'+Uppercase(pElem.Name)+'"', 14));
            FOR j := 1 to NumGenRegisters Do Write(F, Separator, PElem.Registers[j]:10:0);
            Writeln(F);
        END;

        pElem := ActiveCircuit[ActiveActor].Generators.Next;
     End;

     GlobalResult := FileNm;

  FINALLY

     CloseFile(F);

  End;


End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure WriteMultiplePVSystemMeterFiles;

Var
   F  :TextFile;
   i,j:Integer;
   pElem  :TPVSystemObj;
   FileNm,
   Separator :String;

Begin

     If PVSystemClass[ActiveActor] = NIL THEN Exit;  // oops somewhere!!
     Separator := ', ';

     pElem := ActiveCircuit[ActiveActor].PVSystems.First;
     WHILE pElem <> NIL Do
     Begin
        IF pElem.Enabled THEN
        BEGIN
          TRY
            FileNm := GetOutputDirectory + 'EXP_PV_' + Uppercase(pElem.Name) + '.CSV';

            IF Not FileExists(FileNm)
            THEN Begin
                AssignFile(F, FileNm);
                Rewrite(F);
                {Write New Header}
                Write(F, 'Year, LDCurve, Hour, PVSystem');
                For i := 1 to NumPVSystemRegisters Do Write(F, Separator, '"' + PVSystemClass[ActiveActor].RegisterNames[i]+'"');
                Writeln(F);
                CloseFile(F);
            End;

            AssignFile(F, FileNm);
            Append(F);
            With ActiveCircuit[ActiveActor] Do Begin
                Write(F,Solution.Year:0, Separator);
                Write(F,LoadDurCurve, Separator);
                Write(F,Solution.DynaVars.intHour:0, Separator);
                Write(F,Pad('"'+Uppercase(pElem.Name)+'"', 14));
                FOR j := 1 to NumPVSystemRegisters Do Write(F, Separator, PElem.Registers[j]:10:0);
                Writeln(F);
            End;
            AppendGlobalResult(FileNm);
          FINALLY
            CloseFile(F);
          END;

        END;
        pElem := ActiveCircuit[ActiveActor].PVSystems.Next;
     End;

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure WriteMultiplePVSystem2MeterFiles;

Var
   F  :TextFile;
   i,j:Integer;
   pElem  :TPVSystem2Obj;
   FileNm,
   Separator :String;

Begin

     If PVSystem2Class[ActiveActor] = NIL THEN Exit;  // oops somewhere!!
     Separator := ', ';

     pElem := ActiveCircuit[ActiveActor].PVSystems2.First;
     WHILE pElem <> NIL Do
     Begin
        IF pElem.Enabled THEN
        BEGIN
          TRY
            FileNm := GetOutputDirectory + 'EXP_PV_' + Uppercase(pElem.Name) + '.CSV';

            IF Not FileExists(FileNm)
            THEN Begin
                AssignFile(F, FileNm);
                Rewrite(F);
                {Write New Header}
                Write(F, 'Year, LDCurve, Hour, PVSystem2');
                For i := 1 to NumPVSystem2Registers Do Write(F, Separator, '"' + PVSystem2Class[ActiveActor].RegisterNames[i]+'"');
                Writeln(F);
                CloseFile(F);
            End;

            AssignFile(F, FileNm);
            Append(F);
            With ActiveCircuit[ActiveActor] Do Begin
                Write(F,Solution.Year:0, Separator);
                Write(F,LoadDurCurve, Separator);
                Write(F,Solution.DynaVars.intHour:0, Separator);
                Write(F,Pad('"'+Uppercase(pElem.Name)+'"', 14));
                FOR j := 1 to NumPVSystem2Registers Do Write(F, Separator, PElem.Registers[j]:10:0);
                Writeln(F);
            End;
            AppendGlobalResult(FileNm);
          FINALLY
            CloseFile(F);
          END;

        END;
        pElem := ActiveCircuit[ActiveActor].PVSystems2.Next;
     End;

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure WriteSinglePVSystemMeterFile(FileNm:String);

Var
   F  :TextFile;
   i,j:Integer;
   pElem  :TPVSystemObj;
   Separator, TestStr :String;
   ReWriteFile :Boolean;

Begin


  If PVSystemClass[ActiveActor] = NIL THEN Exit;  // oops somewhere!!
  Separator := ', ';


 TRY

    IF FileExists(FileNm)
    THEN Begin  // See if it has already been written on
         Assignfile(F,FileNm);
         Reset(F);
         IF  Not EOF(F)
         THEN Begin
             Read(F, TestStr);
             {See if it likely that the file is OK}
             IF  CompareText(Copy(TestStr,1,4), 'Year')=0
             THEN RewriteFile := FALSE       // Assume the file is OK
             ELSE RewriteFile := TRUE;
         End
         ELSE RewriteFile := TRUE;

         CloseFile(F);

    End
    ELSE Begin
         ReWriteFile := TRUE;
         AssignFile(F, FileNm);
    End;

   {Either open or append the file}
    IF RewriteFile
    THEN Begin
        ReWrite(F);
        {Write New Header}
        Write(F, 'Year, LDCurve, Hour, PVSystem');
        For i := 1 to NumGenRegisters Do Write(F, Separator, '"'+ PVSystemClass[ActiveActor].RegisterNames[i]+'"');
        Writeln(F);
    END
    ELSE Append(F);


     pElem := ActiveCircuit[ActiveActor].PVSystems.First;
     WHILE pElem <> NIL Do
     Begin
        IF pElem.Enabled THEN With ActiveCircuit[ActiveActor] Do
        BEGIN
            Write(F,Solution.Year:0, Separator);
            Write(F,LoadDurCurve, Separator);
            Write(F,Solution.DynaVars.intHour:0, Separator);
            Write(F,Pad('"'+Uppercase(pElem.Name)+'"', 14));
            FOR j := 1 to NumPVSystemRegisters Do Write(F, Separator, PElem.Registers[j]:10:0);
            Writeln(F);
        END;

        pElem := ActiveCircuit[ActiveActor].PVSystems.Next;
     End;

     GlobalResult := FileNm;

  FINALLY

     CloseFile(F);

  End;


End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure WriteSinglePVSystem2MeterFile(FileNm:String);

Var
   F  :TextFile;
   i,j:Integer;
   pElem  :TPVSystem2Obj;
   Separator, TestStr :String;
   ReWriteFile :Boolean;

Begin


  If PVSystem2Class[ActiveActor] = NIL THEN Exit;  // oops somewhere!!
  Separator := ', ';


 TRY

    IF FileExists(FileNm)
    THEN Begin  // See if it has already been written on
         Assignfile(F,FileNm);
         Reset(F);
         IF  Not EOF(F)
         THEN Begin
             Read(F, TestStr);
             {See if it likely that the file is OK}
             IF  CompareText(Copy(TestStr,1,4), 'Year')=0
             THEN RewriteFile := FALSE       // Assume the file is OK
             ELSE RewriteFile := TRUE;
         End
         ELSE RewriteFile := TRUE;

         CloseFile(F);

    End
    ELSE Begin
         ReWriteFile := TRUE;
         AssignFile(F, FileNm);
    End;

   {Either open or append the file}
    IF RewriteFile
    THEN Begin
        ReWrite(F);
        {Write New Header}
        Write(F, 'Year, LDCurve, Hour, PVSystem2');
        For i := 1 to NumGenRegisters Do Write(F, Separator, '"'+ PVSystem2Class[ActiveActor].RegisterNames[i]+'"');
        Writeln(F);
    END
    ELSE Append(F);


     pElem := ActiveCircuit[ActiveActor].PVSystems2.First;
     WHILE pElem <> NIL Do
     Begin
        IF pElem.Enabled THEN With ActiveCircuit[ActiveActor] Do
        BEGIN
            Write(F,Solution.Year:0, Separator);
            Write(F,LoadDurCurve, Separator);
            Write(F,Solution.DynaVars.intHour:0, Separator);
            Write(F,Pad('"'+Uppercase(pElem.Name)+'"', 14));
            FOR j := 1 to NumPVSystem2Registers Do Write(F, Separator, PElem.Registers[j]:10:0);
            Writeln(F);
        END;

        pElem := ActiveCircuit[ActiveActor].PVSystems2.Next;
     End;

     GlobalResult := FileNm;

  FINALLY

     CloseFile(F);

  End;


End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure WriteMultipleStorageMeterFiles;

Var
   F  :TextFile;
   i,j:Integer;
   pElem  :TStorageObj;
   FileNm,
   Separator :String;

Begin

     If StorageClass = NIL THEN Exit;  // oops somewhere!!
     Separator := ', ';

     pElem := ActiveCircuit[ActiveActor].StorageElements.First;
     WHILE pElem <> NIL Do
     Begin
        IF pElem.Enabled THEN
        BEGIN
          TRY
            FileNm := GetOutputDirectory + 'EXP_PV_' + Uppercase(pElem.Name) + '.CSV';

            IF Not FileExists(FileNm)
            THEN Begin
                AssignFile(F, FileNm);
                Rewrite(F);
                {Write New Header}
                Write(F, 'Year, LDCurve, Hour, Storage');
                For i := 1 to NumStorageRegisters Do Write(F, Separator, '"' + StorageClass[ActiveActor].RegisterNames[i]+'"');
                Writeln(F);
                CloseFile(F);
            End;

            AssignFile(F, FileNm);
            Append(F);
            With ActiveCircuit[ActiveActor] Do Begin
                Write(F,Solution.Year:0, Separator);
                Write(F,LoadDurCurve, Separator);
                Write(F,Solution.DynaVars.intHour:0, Separator);
                Write(F,Pad('"'+Uppercase(pElem.Name)+'"', 14));
                FOR j := 1 to NumStorageRegisters Do Write(F, Separator, PElem.Registers[j]:10:0);
                Writeln(F);
            End;
            AppendGlobalResult(FileNm);
          FINALLY
            CloseFile(F);
          END;

        END;
        pElem := ActiveCircuit[ActiveActor].StorageElements.Next;
     End;

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure WriteSingleStorageMeterFile(FileNm:String);

Var
   F  :TextFile;
   i,j:Integer;
   pElem  :TStorageObj;
   Separator, TestStr :String;
   ReWriteFile :Boolean;

Begin


  If StorageClass = NIL THEN Exit;  // oops somewhere!!
  Separator := ', ';


 TRY

    IF FileExists(FileNm)
    THEN Begin  // See if it has already been written on
         Assignfile(F,FileNm);
         Reset(F);
         IF  Not EOF(F)
         THEN Begin
             Read(F, TestStr);
             {See if it likely that the file is OK}
             IF  CompareText(Copy(TestStr,1,4), 'Year')=0
             THEN RewriteFile := FALSE       // Assume the file is OK
             ELSE RewriteFile := TRUE;
         End
         ELSE RewriteFile := TRUE;

         CloseFile(F);

    End
    ELSE Begin
         ReWriteFile := TRUE;
         AssignFile(F, FileNm);
    End;

   {Either open or append the file}
    IF RewriteFile
    THEN Begin
        ReWrite(F);
        {Write New Header}
        Write(F, 'Year, LDCurve, Hour, Storage');
        For i := 1 to NumStorageRegisters Do Write(F, Separator, '"'+ StorageClass[ActiveActor].RegisterNames[i]+'"');
        Writeln(F);
    END
    ELSE Append(F);


     pElem := ActiveCircuit[ActiveActor].StorageElements.First;
     WHILE pElem <> NIL Do
     Begin
        IF pElem.Enabled THEN With ActiveCircuit[ActiveActor] Do
        BEGIN
            Write(F,Solution.Year:0, Separator);
            Write(F,LoadDurCurve, Separator);
            Write(F,Solution.DynaVars.intHour:0, Separator);
            Write(F,Pad('"'+Uppercase(pElem.Name)+'"', 14));
            FOR j := 1 to NumStorageRegisters Do Write(F, Separator, PElem.Registers[j]:10:0);
            Writeln(F);
        END;

        pElem := ActiveCircuit[ActiveActor].StorageElements.Next;
     End;

     GlobalResult := FileNm;

  FINALLY

     CloseFile(F);

  End;


End;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportGenMeters(FileNm:String);

// Export Values of Generator Meter Elements
// If switch /m is specified, a separate file is created for each generator using the generator's name

Begin


  If Lowercase(Copy(FileNm,1,2)) = '/m'
  THEN WriteMultipleGenMeterFiles
  ELSE WriteSingleGenMeterFile(FileNM);

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportPVSystemMeters(FileNm:String);

// Export Values of Generator Meter Elements
// If switch /m is specified, a separate file is created for each generator using the generator's name

Begin


  If Lowercase(Copy(FileNm,1,2)) = '/m'
  THEN WriteMultiplePVSystemMeterFiles
  ELSE WriteSinglePVSystemMeterFile(FileNM);

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportPVSystem2Meters(FileNm:String);

// Export Values of Generator Meter Elements
// If switch /m is specified, a separate file is created for each generator using the generator's name

Begin


  If Lowercase(Copy(FileNm,1,2)) = '/m'
  THEN WriteMultiplePVSystem2MeterFiles
  ELSE WriteSinglePVSystem2MeterFile(FileNM);

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportStorageMeters(FileNm:String);

// Export Values of Generator Meter Elements
// If switch /m is specified, a separate file is created for each generator using the generator's name

Begin


  If Lowercase(Copy(FileNm,1,2)) = '/m'
  THEN WriteMultipleStorageMeterFiles
  ELSE WriteSingleStorageMeterFile(FileNM);

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportLoads(FileNm:String);

// Export Loads to view present allocation


Var
   F  :TextFile;
   pElem  :TLoadObj;
   Separator :String;

Begin

  Separator := ', ';


 TRY

     AssignFile(F, FileNm);
     ReWrite(F);
     {Write  Header}
     Writeln(F, 'Load, Connected KVA, Allocation Factor, Phases, kW, kvar, PF, Model');

     pElem := ActiveCircuit[ActiveActor].Loads.First;
     WHILE pElem <> NIL Do
     Begin
        IF pElem.Enabled THEN
        WITH pElem Do
        BEGIN
            Write(F, Uppercase(Name));
            Write(F, Separator, ConnectedkVA:8:1);
            Write(F, Separator, kVAAllocationFactor:5:3);
            Write(F, Separator, NPhases:0);
            Write(F, Separator, kWBase:8:1);
            Write(F, Separator, kvarBase:8:1);
            Write(F, Separator, PFNominal:5:3);
            Write(F, Separator, FLoadModel:0);
        END;
        Writeln(F);
        pElem := ActiveCircuit[ActiveActor].Loads.Next;
     End;

     GlobalResult := FileNm;

  FINALLY

     CloseFile(F);

  End;

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportCapacity(FileNm:String);

{
 Similar to export currents except does only max of the phases and compares that
 to the Normamps and Emergamps rating
}

Var
    F          :TextFile;
    cBuffer    :pComplexArray;
    pElem      :TPDElement;

Begin

  cBuffer := nil;

  Try
     Assignfile(F, FileNm);
     ReWrite(F);

     Getmem(cBuffer, sizeof(cBuffer^[1]) * GetMaxCktElementSize);

     Writeln(F, 'Name, Imax, %normal, %emergency, kW, kvar, NumCustomers, TotalCustomers, NumPhases, kVBase');

     // PDELEMENTS ONLY
     pElem := ActiveCircuit[ActiveActor].PDElements.First;
     WHILE pElem<>nil DO BEGIN
       IF pElem.Enabled THEN BEGIN
          pElem.GetCurrents(cBuffer, ActiveActor);
          CalcAndWriteMaxCurrents(F, pElem, Cbuffer);
       END;
        pElem := ActiveCircuit[ActiveActor].PDElements.Next;
     END;

     GlobalResult := FileNm;

  FINALLY
     If Assigned(cBuffer) Then Freemem(cBuffer);
     CloseFile(F);

  End;


End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportOverloads(FileNm:String);

Var
    F              :TextFile;
    cBuffer        :pComplexArray;  // Allocate to max total conductors
    NCond,
    i,
    j              :Integer;
    PDElem         :TPDElement;
    Iph,
    I012           :Array[1..3] of Complex;
    I0,I1,I2,
    iNormal,
    iEmerg, Cmax   :Double;
    Separator      :String;
    Spower         :Double;

Begin

  cBuffer := nil;
  
  Try
     Assignfile(F,FileNm);
     ReWrite(F);

    {Allocate cBuffer big enough for largest circuit element}
     Getmem(cbuffer, sizeof(cBuffer^[1])* GetMaxCktElementSize);

     {Sequence Currents}
     Writeln(F,'Element, Terminal,  I1, AmpsOver, kVAOver, %Normal, %Emergency, I2, %I2/I1, I0, %I0/I1');

     Separator := ', ';

     // PDELEMENTS Only
     PDelem := ActiveCircuit[ActiveActor].PDElements.First;

     WHILE PDelem<>nil DO BEGIN
       IF (PDelem.Enabled)
       THEN IF (CLASSMASK AND PDElem.DSSObjType) <> CAP_ELEMENT    // ignore caps
       THEN BEGIN
        NCond := PDelem.NConds;
        PDelem.GetCurrents(cBuffer, ActiveActor);

        FOR j := 1 to 1 Do       // only for terminal 1
        BEGIN
            Cmax := 0.0;
            For i := 1 to Min(PDelem.Nphases, 3) Do Begin   // Check only first 3 phases
               Iph[i] :=cBuffer^[(j-1)*Ncond + i];
               Cmax := max(Cmax, Cabs(Iph[i]));
            End;
            IF (PDelem.Nphases >= 3)
              THEN BEGIN   // Report Symmetrical Component Currents for
                  Phase2SymComp(@Iph, @I012);
                  I0 := Cabs(I012[1]);   // Get abs values to report
                  I1 := Cabs(I012[2]);
                  I2 := Cabs(I012[3]);
              END
              ELSE BEGIN   // Other than 3-phase
                 I0 := 0.0;
                 I1 := Cabs(Iph[1]);    // Ambiguous: Report only first phase
                 I2 := 0.0;
                 Cmax := I1;
              END;

            IF (PdElem.Normamps > 0.0) OR (PdElem.Emergamps>0.0)
            THEN
             IF (CMax > PDElem.NormAmps) OR (Cmax > pdelem.EmergAmps)
             THEN Begin
               // Get terminal 1 power
                 Spower := Cabs(PDElem.Power[1,ActiveActor]) * 0.001;   // kW

                 Write(F, Format('%s, %d, ' ,[Pad(('"'+pDelem.DSSClassName + '.' + Uppercase(pDelem.Name)+'"'), 22),j]));
                 Write(F, Format('%8.2f, ',[I1 ]));
                 IF  j = 1 THEN Begin // Only for 1st Terminal
                      iNormal := PDelem.NormAmps;
                      IF iNormal > 0.0
                          THEN Begin
                                    Write(F, Format('%8.2f, %10.2f', [(Cmax - iNormal), Spower*(Cmax-iNormal)/iNormal ]));
                                    Write(F, Separator, Cmax/iNormal*100.0:8:1);
                               End
                          ELSE Write(F, Separator, '     0.0');
                      iEmerg :=  PDelem.EmergAmps;
                      IF iEmerg > 0.0
                          THEN Write(F, Separator, Cmax/iEmerg*100.0:8:1)
                          ELSE Write(F, Separator, '     0.0');
                   End
                 ELSE Write(F, Separator, '       0', Separator, '       0');
                 Write(F, Separator, I2:8:1);
                 IF I1>0.0 THEN Write(F, Separator, 100.0*I2/I1:8:1) ELSE Write(F, Separator,'0.0');
                 Write(F, Separator, I0:8:1);
                 IF I1>0.0 THEN Write(F, Separator, 100.0*I0/I1:8:1) ELSE Write(F, Separator,'0.0');
                 Writeln(F);
             End;

        END;
       END;
        PDelem := ActiveCircuit[ActiveActor].PDElements.Next;
     END;

     GlobalResult := FileNm;
     

  FINALLY
     If Assigned(Cbuffer) then Freemem(cBuffer);
     CloseFile(F);

  End;
End;

Procedure ExportUnserved(FileNm:String; UE_Only:Boolean);

Var
    F       :TextFile;
    PLoad:TLoadObj;
    DoIt :Boolean;

Begin

  Try
     Assignfile(F,FileNm);
     ReWrite(F);

     Writeln(F,'Load, Bus, kW, EEN_Factor,  UE_Factor');

     // Load
     pLoad := ActiveCircuit[ActiveActor].Loads.First;
     WHILE pLoad<>nil DO
     Begin
           IF (pLoad.Enabled)
           THEN Begin
              DoIt := FALSE;
              IF UE_Only THEN Begin
                IF pLoad.Unserved[ActiveActor] THEN DoIt := TRUE; end
              ELSE
                IF pLoad.Get_ExceedsNormal(ActiveActor) THEN DoIt := TRUE;

              IF DoIt
              THEN Begin
                  Write(F, Uppercase(pLoad.Name),', ');
                  Write(F, pLoad.GetBus(1),', ');
                  Write(F, pLoad.kWBase:8:0,', ');
                  Write(F, pLoad.EEN_Factor:9:3,', ');
                  Write(F, pLoad.UE_Factor:9:3);
                  Writeln(F);
              End;

           End;
       pLoad := ActiveCircuit[ActiveActor].Loads.Next;
     End;

     GlobalResult := FileNm;

  Finally

     CloseFile(F);

  End;

End;

Procedure ExportYprim(FileNm:String);

{Exports  YPrim matrices for all  Circuit Elements}

Var
    F       :TextFile;
    i, j, k :Integer;
    cValues :pComplexArray;

Begin

  If ActiveCircuit[ActiveActor]=Nil then Exit;

  Try
     Assignfile(F,FileNm);
     ReWrite(F);

     With ActiveCircuit[ActiveActor] Do Begin
        For k := 1 to NumDevices Do BEGIN
          ActiveCktElement := CktElements.Get(k);
          If  ActiveCktElement.Enabled THEN Begin
              If (ActiveCktElement is TPDElement) or (ActiveCktElement is TPCElement) then
              With ActiveCktElement Do  Begin
                Writeln(F, ParentClass.Name,'.',Uppercase(Name));
                cValues := GetYprimValues(ALL_YPRIM);
                For i := 1 to Yorder Do  Begin
                 For j := 1 to Yorder Do Write(F, Format('%-13.10g, %-13.10g, ',[cValues^[i+(j-1)*Yorder].re, cValues^[i+(j-1)*Yorder].im]));
                 Writeln(F);
                End;
              End;
          End;
        End;
     END ;


     GlobalResult := FileNm;

  Finally

     CloseFile(F);

  End;

End;

// illustrate retrieval of System Y using compressed column format
Procedure ExportY(FileNm:String; TripletOpt:Boolean);

{Exports System Y Matrix in Node Order}

Var
    F                :TextFile;
    i,j,p            :LongWord;
    col,row          :LongWord;
    hY               :NativeUInt;
    nBus, nNZ        :LongWord;
    ColPtr, RowIdx   :array of LongWord;
    cVals            :array of Complex;
    re, im           :Double;

Begin

  If ActiveCircuit[ActiveActor]=Nil then Exit;
  hY := ActiveCircuit[ActiveActor].Solution.hY;
  If hY <= 0 Then Begin
     DoSimpleMsg('Y Matrix not Built.', 222);
     Exit;
  End;
  // this compresses the entries if necessary - no extra work if already solved
  FactorSparseMatrix(hY);
  GetNNZ(hY, @nNZ);
  GetSize(hY, @nBus); // we should already know this

  Try
     Assignfile(F,FileNm);
     ReWrite(F);

     if TripletOpt then begin
       SetLength (ColPtr, nNZ);
       SetLength (RowIdx, nNZ);
       SetLength (cVals, nNZ);
       GetTripletMatrix (hY, nNZ, @RowIdx[0], @ColPtr[0], @cVals[0]);
       Writeln(F, 'Row,Col,G,B');
       for i := 0 to nNZ - 1 do begin
         col := ColPtr[i] + 1;
         row := RowIdx[i] + 1;
         if row >= col then begin
           re := cVals[i].re;
           im := cVals[i].im;
           Writeln (F, Format('%d,%d,%.10g,%.10g', [row, col, re, im]));
         end;
       end;
     end else begin
       SetLength (ColPtr, nBus + 1);
       SetLength (RowIdx, nNZ);
       SetLength (cVals, nNZ);
       GetCompressedMatrix (hY, nBus + 1, nNZ, @ColPtr[0], @RowIdx[0], @cVals[0]);
       {Write out fully qualified Bus Names}
       With ActiveCircuit[ActiveActor] Do Begin
          Writeln(F, Format('%d, ',[NumNodes]));
  (*        For i := 1 to NumNodes DO BEGIN
             j :=  MapNodeToBus^[i].BusRef;
             Write(F, Format('%s.%-d, +j,',[BusList.Get(j), MapNodeToBus^[i].NodeNum]));
          END;
          Writeln(F);
  *)
          For i := 1 to NumNodes Do Begin
             j :=  MapNodeToBus^[i].BusRef;
             Write(F, Format('"%s.%-d", ',[Uppercase(BusList.Get(j)), MapNodeToBus^[i].NodeNum]));
             For j := 1 to NumNodes Do Begin
                re := 0.0;
                im := 0.0;
                // search for a non-zero element [i,j]
                //  DSS indices are 1-based, KLU indices are 0-based
                for p := ColPtr[j-1] to ColPtr[j] - 1 do begin
                  if RowIdx[p] + 1 = i then begin
                    re := cVals[p].re;
                    im := cVals[p].im;
                  end;
                end;
                Write(F, Format('%-13.10g, +j %-13.10g,', [re, im]));
             End;
             Writeln(F);
          End;
        End;
     end;

     GlobalResult := FileNm;
  Finally
     CloseFile(F);
  End;
End;

Procedure ExportSeqZ(FileNm:String);

// Export Symmetrical Component Impedances at each bus

Var
   F :TextFile;
   i:Integer;
   Z1, Z0 :Complex;
   X1R1, X0R0 : Double;


Begin

  Try
     Assignfile(F, FileNm);
     ReWrite(F);

     Writeln(F,'Bus,  NumNodes, R1, X1, R0, X0, Z1, Z0, "X1/R1", "X0/R0"');
     WITH ActiveCircuit[ActiveActor] DO
     BEGIN
       FOR i := 1 to NumBuses DO
       BEGIN

         Z1 := Buses^[i].Zsc1;
         Z0 := Buses^[i].Zsc0;
         If Z1.re<>0.0 then  X1R1 := Z1.im/Z1.re Else X1R1 := 1000.0;
         If Z0.re<>0.0 then  X0R0 := Z0.im/Z0.re Else X0R0 := 1000.0;

         Writeln(F,
         Format('"%s", %d, %10.6g, %10.6g, %10.6g, %10.6g, %10.6g, %10.6g, %8.4g, %8.4g',
                [Uppercase(BusList.Get(i)), Buses^[i].NumNodesThisBus,
                 Z1.re, Z1.im, Z0.Re, Z0.im, Cabs(Z1), Cabs(Z0), X1R1, X0R0 ]
         ));

       END;
     END;


     GlobalResult := FileNm;

  FINALLY

     CloseFile(F);
  End;

End;

Procedure ExportGuids(FileNm:String);
Var
  F       : TextFile;
  clsCode : TLineCode;
  clsGeom : TLineGeometry;
  clsWire : TWireData;
  clsXfmr : TXfmrCode;
  pName   : TNamedObject;
Begin
  Try
    clsCode := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('linecode'));
    clsWire := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('wiredata'));
    clsGeom := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('linegeometry'));
    clsXfmr := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('xfmrcode'));

    Assignfile(F, FileNm);
    ReWrite(F);

    pName := ActiveCircuit[ActiveActor];
    Writeln (F, Format ('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));

    pName := ActiveCircuit[ActiveActor].CktElements.First;
    while pName <> nil do begin
      Writeln (F, Format ('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));
      pName := ActiveCircuit[ActiveActor].CktElements.Next;
    End;

    pName := clsCode.ElementList.First;
    while pName <> nil do begin
      Writeln (F, Format ('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));
      pName := clsCode.ElementList.Next;
    End;

    pName := clsWire.ElementList.First;
    while pName <> nil do begin
      Writeln (F, Format ('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));
      pName := clsWire.ElementList.Next;
    End;

    pName := clsGeom.ElementList.First;
    while pName <> nil do begin
      Writeln (F, Format ('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));
      pName := clsGeom.ElementList.Next;
    End;

    pName := clsXfmr.ElementList.First;
    while pName <> nil do begin
      Writeln (F, Format ('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));
      pName := clsXfmr.ElementList.Next;
    End;

  Finally
    CloseFile(F);
  End;
End;

Procedure ExportCounts(FileNm:String);
Var
  F   : TextFile;
  cls :TDSSClass;
Begin
  Try
    Assignfile(F, FileNm);
    ReWrite(F);
    Writeln (F, 'Format: DSS Class Name = Instance Count');
    Writeln (F);
    cls := DSSClassList[ActiveActor].First;
    while cls <> nil do begin
      Writeln (F, Format ('%s = %d', [cls.Name, cls.ElementCount]));
      cls := DSSClassList[ActiveActor].Next;
    end;
  Finally
    CloseFile(F);
  End;
End;

Procedure ExportSummary(FileNm:String);
Var
  F   : TextFile;
  cPower, cLosses :Complex;

Begin
  Try

    Assignfile(F, FileNm);
    If FileExists(FileNm) then Append(F) Else
    Begin    // Create and write the header
        ReWrite(F);
        Write(F, 'DateTime, CaseName, ');
        Write (F, 'Status, Mode, Number, LoadMult, NumDevices, NumBuses, NumNodes');
        Write (F, ', Iterations, ControlMode, ControlIterations');
        Write (F, ', MostIterationsDone');
        If ActiveCircuit[ActiveActor] <> Nil Then
          If ActiveCircuit[ActiveActor].Issolved and not ActiveCircuit[ActiveActor].BusNameRedefined Then
          Begin
              Write(F, ', Year, Hour, MaxPuVoltage, MinPuVoltage, TotalMW, TotalMvar');
              Write(F, ', MWLosses, pctLosses, MvarLosses, Frequency');
          End;

        Writeln (F);
    End;

    Write(F, Format('"%s", ',  [DateTimeToStr(Now)]));
    If ActiveCircuit[ActiveActor] <> Nil Then Write(F, Format('%s, ',  [ActiveCircuit[ActiveActor].CaseName]))
                            Else Write(F, 'NONE, ');

    IF ActiveCircuit[ActiveActor].Issolved Then Write(F,'SOLVED')
                              Else Write(F,'UnSolved');

    Write(F, Format(', %s',    [GetSolutionModeID]));
    Write(F, Format(', %d',    [ActiveCircuit[ActiveActor].Solution.NumberofTimes]));
    Write(F, Format(', %8.3f', [ActiveCircuit[ActiveActor].LoadMultiplier]));
    Write(F, Format(', %d',    [ActiveCircuit[ActiveActor].NumDevices]));
    Write(F, Format(', %d',    [ActiveCircuit[ActiveActor].NumBuses]));
    Write(F, Format(', %d',    [ActiveCircuit[ActiveActor].NumNodes]));
    Write(F, Format(', %d',    [ActiveCircuit[ActiveActor].Solution.Iteration]));
    Write(F, Format(', %s',    [GetControlModeID]));
    Write(F, Format(', %d',    [ActiveCircuit[ActiveActor].Solution.ControlIteration]));
    Write(F, Format(', %d',    [ActiveCircuit[ActiveActor].Solution.MostIterationsDone ]));
    If ActiveCircuit[ActiveActor] <> Nil Then
      If ActiveCircuit[ActiveActor].Issolved and not ActiveCircuit[ActiveActor].BusNameRedefined Then
      Begin
          Write(F, Format(', %d',    [ActiveCircuit[ActiveActor].Solution.Year]));
          Write(F, Format(', %d',    [ActiveCircuit[ActiveActor].Solution.DynaVars.intHour]));
          Write(F, Format(', %-.5g', [GetMaxPUVoltage]));
          Write(F, Format(', %-.5g', [GetMinPUVoltage(TRUE)]));
          cPower :=  CmulReal(GetTotalPowerFromSources(ActiveActor), 0.000001);  // MVA
          Write(F, Format(', %-.6g', [cPower.re]));
          Write(F, Format(', %-.6n', [cPower.im]));
          cLosses := CmulReal(ActiveCircuit[ActiveActor].Losses[ActiveActor], 0.000001);
          If cPower.re <> 0.0 Then Write(F, Format(', %-.6g, %-.4g',[cLosses.re,(Closses.re/cPower.re*100.0)]))
          Else Write(F, 'Total Active Losses:   ****** MW, (**** %%)');
          Write(F, Format(', %-.6g',  [cLosses.im]));
          Write(F, Format(', %-g',    [ActiveCircuit[ActiveActor].Solution.Frequency]));
      End;

    Writeln(F);

    GlobalResult := FileNm;

  Finally
    CloseFile(F);
  End;
End;

Procedure ExportBusCoords(FileNm:String);
// Export bus x, y coordinates

Var
   F :TextFile;
   i:Integer;


Begin

  Try
     Assignfile(F, FileNm);
     ReWrite(F);

     With ActiveCircuit[ActiveActor] Do
     For i := 1 to NumBuses Do
       Begin
           If Buses^[i].CoordDefined then Writeln(F, Format('%s, %-13.11g, %-13.11g', [CheckForBlanks(Uppercase(BusList.Get(i))), Buses^[i].X, Buses^[i].Y]));
       End;

     GlobalResult := FileNm;

  FINALLY

     CloseFile(F);
  End;
End;

Procedure WriteNewLine(Var F:TextFile;
                       Const CktELementName:String; DistFromMeter1, puV1, DistFromMeter2, puV2:Double;
                       ColorCode, Thickness, LineType:Integer;
                       MarkCenter:Integer;
                       CenterMarkerCode,  NodeMarkerCode, NodeMarkerWidth:Integer );

Begin
       Write(F, Format('%s, %.6g, %.6g, %.6g, %.6g,',[Uppercase(CktElementName), DistFromMeter1, puV1, DistFromMeter2, puV2 ]));
       Write(F, Format('%d, %d, %d, ',[ColorCode, Thickness, LineType ]));
       Write(F, Format('%d, ',[ MarkCenter ]));
       Write(F, Format('%d, %d, %d',[CenterMarkerCode,  NodeMarkerCode, NodeMarkerWidth ]));
       Writeln(F);
End;


Procedure ExportProfile(FileNm:String; PhasesToPlot:Integer);

Var
   iEnergyMeter       :Integer;
   ActiveEnergyMeter  :TEnergyMeterObj;
   PresentCktElement  :TDSSCktElement;
   Bus1, Bus2         :TDSSbus;
   puV1, puV2         :Double;
   iphs               :Integer;
   iphs2              :Integer;
   S                  :String;
   F                  :TextFile;
   Linetype           :Integer;

begin

  Try
     Assignfile(F, FileNm);
     ReWrite(F);

     Write(F, 'Name, Distance1, puV1, Distance2, puV2, Color, Thickness, Linetype, Markcenter, Centercode, NodeCode, NodeWidth,');

    {New graph created before this routine is entered}
       case phasesToPlot of
           PROFILELL, PROFILELLALL, PROFILELLPRI:  S  := 'L-L Voltage Profile';
       else
            S  := 'L-N Voltage Profile';
       end;

       Writeln(F, 'Title=',S, ', Distance in km');

      iEnergyMeter := EnergyMeterClass[ActiveActor].First;
      while iEnergyMeter >0  do  Begin

          ActiveEnergyMeter := EnergyMeterClass[ActiveActor].GetActiveObj;
          {Go down each branch list and draw a line}
          PresentCktElement := ActiveEnergyMeter.BranchList.First;
          while PresentCktElement <> Nil Do Begin
          If IslineElement(PresentCktElement) Then  With ActiveCircuit[ActiveActor] Do Begin
              Bus1 := Buses^[PresentCktElement.Terminals^[1].BusRef];
              Bus2 := Buses^[PresentCktElement.Terminals^[2].BusRef];
            {Now determin which phase to plot}
              If (Bus1.kVBase > 0.0) and (Bus2.kVBase > 0.0) then
              CASE PhasesToPlot of
                  {3ph only}
                  PROFILE3PH: If (PresentCktElement.NPhases >= 3) and (Bus1.kVBase > 1.0) then
                                For iphs := 1 to 3 do Begin
                                  puV1 := CABS(Solution.NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))]) / Bus1.kVBase / 1000.0;
                                  puV2 := CABS(Solution.NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))]) / Bus2.kVBase / 1000.0;
                                  WriteNewLine(F, PresentCktElement.Name, Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                                         iphs, 2, 0,   0, 0,  NodeMarkerCode, NodeMarkerWidth );
                                End;
                  {Plot all phases present (between 1 and 3)}
                  PROFILEALL: Begin
                                For iphs := 1 to 3 do
                                  if (Bus1.FindIdx(Iphs)>0) and (Bus2.FindIdx(Iphs)>0) then Begin
                                    if Bus1.kVBase < 1.0 then  Linetype := 2 else Linetype := 0;
                                    puV1 := CABS(Solution.NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))]) / Bus1.kVBase / 1000.0;
                                    puV2 := CABS(Solution.NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))]) / Bus2.kVBase / 1000.0;
                                    WriteNewLine(F, PresentCktElement.Name, Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                                           iphs, 2, Linetype,   0, 0,  NodeMarkerCode, NodeMarkerWidth );
                                End;
                              End;
                  {Plot all phases present (between 1 and 3) for Primary only}
                  PROFILEALLPRI: Begin
                                If Bus1.kVBase > 1.0 then
                                For iphs := 1 to 3 do
                                  if (Bus1.FindIdx(Iphs)>0) and (Bus2.FindIdx(Iphs)>0) then Begin
                                    if Bus1.kVBase < 1.0 then Linetype := 2 else Linetype := 0;
                                    puV1 := CABS(Solution.NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))]) / Bus1.kVBase / 1000.0;
                                    puV2 := CABS(Solution.NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))]) / Bus2.kVBase / 1000.0;
                                    WriteNewLine(F, PresentCktElement.Name, Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                                           iphs, 2, Linetype,  0, 0,  NodeMarkerCode, NodeMarkerWidth );
                                End;
                              End;
                  PROFILELL: Begin
                                If (PresentCktElement.NPhases >= 3)  then
                                For iphs := 1 to 3 do Begin
                                    iphs2 := iphs + 1; If iphs2 > 3 Then iphs2 := 1;
                                    if (Bus1.FindIdx(Iphs)>0)  and (Bus2.FindIdx(Iphs)>0)  and
                                       (Bus1.FindIdx(Iphs2)>0) and (Bus2.FindIdx(Iphs2)>0) then
                                    Begin
                                      if Bus1.kVBase < 1.0 then  Linetype := 2 else Linetype := 0;
                                      With Solution Do Begin
                                        puV1 := CABS(CSUB(NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))],NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs2))])) / Bus1.kVBase / 1732.0;
                                        puV2 := CABS(CSUB(NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))],NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs2))])) / Bus2.kVBase / 1732.0;
                                      End;
                                      WriteNewLine(F, PresentCktElement.Name, Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                                             iphs, 2, Linetype,  0, 0,  NodeMarkerCode, NodeMarkerWidth );
                                    End;
                                End;
                              End;
                  PROFILELLALL: Begin
                                For iphs := 1 to 3 do Begin
                                    iphs2 := iphs + 1; If iphs2 > 3 Then iphs2 := 1;
                                    if (Bus1.FindIdx(Iphs)>0)  and (Bus2.FindIdx(Iphs)>0)  and
                                       (Bus1.FindIdx(Iphs2)>0) and (Bus2.FindIdx(Iphs2)>0) then
                                    Begin
                                      if Bus1.kVBase < 1.0 then  Linetype := 2 else Linetype := 0;
                                      With Solution Do Begin
                                        puV1 := CABS(CSUB(NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))],NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs2))])) / Bus1.kVBase / 1732.0;
                                        puV2 := CABS(CSUB(NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))],NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs2))])) / Bus2.kVBase / 1732.0;
                                      End;
                                      WriteNewLine(F, PresentCktElement.Name, Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                                             iphs, 2, Linetype,  0, 0,  NodeMarkerCode, NodeMarkerWidth );
                                    End;
                                End;
                              End;
                  PROFILELLPRI: Begin
                                If Bus1.kVBase > 1.0 then
                                For iphs := 1 to 3 do Begin
                                    iphs2 := iphs + 1; If iphs2 > 3 Then iphs2 := 1;
                                    if (Bus1.FindIdx(Iphs)>0)  and (Bus2.FindIdx(Iphs)>0)  and
                                       (Bus1.FindIdx(Iphs2)>0) and (Bus2.FindIdx(Iphs2)>0) then
                                    Begin
                                      if Bus1.kVBase < 1.0 then  Linetype := 2 else Linetype := 0;
                                      With Solution Do Begin
                                        puV1 := CABS(CSUB(NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))],NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs2))])) / Bus1.kVBase / 1732.0;
                                        puV2 := CABS(CSUB(NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))],NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs2))])) / Bus2.kVBase / 1732.0;
                                      End;
                                      WriteNewLine(F, PresentCktElement.Name, Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                                             iphs, 2, Linetype, 0, 0,  NodeMarkerCode, NodeMarkerWidth );
                                    End;
                                End;
                              End;
                  ELSE     // plot just the selected phase
                      iphs := PhasesToPlot;
                      if (Bus1.FindIdx(Iphs)>0) and (Bus2.FindIdx(Iphs)>0) then  Begin
                          if Bus1.kVBase < 1.0 then Linetype := 2 else Linetype := 0;
                          puV1 := CABS(ActiveCircuit[ActiveActor].Solution.NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))]) / Bus1.kVBase / 1000.0;
                          puV2 := CABS(ActiveCircuit[ActiveActor].Solution.NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))]) / Bus2.kVBase / 1000.0;
                          WriteNewLine(F, PresentCktElement.Name, Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                             iphs, 2, Linetype, 0, 0,
                                  NodeMarkerCode, NodeMarkerWidth);
                      End;

              END;

            End;

             PresentCktElement := ActiveEnergyMeter.BranchList.GoForward;
          End;

          iEnergyMeter := EnergyMeterClass[ActiveActor].Next;
      End;

      GlobalResult := FileNm;

  FINALLY

     CloseFile(F);

  End;

end;

Procedure ExportEventLog(FileNm:String);
// Export the present set of EventStrings
Begin
     EventStrings[ActiveActor].SaveToFile(FileNm);
     GlobalResult := FileNm;
End;

Procedure ExportErrorLog(FileNm:String);
// Export the present set of ErrorStrings
Begin
     ErrorStrings[ActiveActor].SaveToFile(FileNm);
     GlobalResult := FileNm;
End;

Procedure ExportIncMatrix(FileNm:String);
var
  F             : TextFile;
  i             : Integer;
Begin
  with ActiveCircuit[ActiveActor].Solution do
  Begin
    Assignfile(F,FileNm);
    ReWrite(F);
    Writeln(F,'Row,Col,Value');
    for i := 0 to (IncMat.NZero -1) do
    begin
      Writeln(F,inttostr(IncMat.data[i][0]) + ',' + inttostr(IncMat.data[i][1]) + ',' + inttostr(IncMat.data[i][2]));
    end;
    GlobalResult := FileNm;
    CloseFile(F);
  End;
End;

Procedure ExportIncMatrixRows(FileNm:String);
var
  F             : TextFile;
  i             : Integer;
Begin
  with ActiveCircuit[ActiveActor].Solution do
  Begin
    Assignfile(F,FileNm);
    ReWrite(F);
    Writeln(F,'B2N Incidence Matrix Row Names (PDElements)');
    for i := 0 to (length(Inc_Mat_Rows)-1) do
    begin
      Writeln(F,Inc_Mat_Rows[i]);
    end;
    GlobalResult := FileNm;
    CloseFile(F);
  End;
End;
//-------------------------------------------------------------------
Procedure ExportIncMatrixCols(FileNm:String);
var
  F             : TextFile;
  i             : Integer;
Begin
  with ActiveCircuit[ActiveActor].Solution do
  Begin
    Assignfile(F,FileNm);
    ReWrite(F);
    Writeln(F,'B2N Incidence Matrix Column Names (Buses)');
    for i := 0 to (length(Inc_Mat_Cols)-1) do
    begin
      Writeln(F,Inc_Mat_Cols[i]);
    end;
    GlobalResult := FileNm;
    CloseFile(F);
  End;
End;
//-------------------------------------------------------------------
Procedure ExportBusLevels(FileNm:String);
var
  F             : TextFile;
  i             : Integer;
Begin
  with ActiveCircuit[ActiveActor].Solution do
  Begin
    Assignfile(F,FileNm);
    ReWrite(F);
    Writeln(F,'B2N Incidence Matrix Column Names (Buses) and their level within the matrix');
    Writeln(F,'Bus Name,Bus Level');
    for i := 0 to (length(Inc_Mat_Cols)-1) do
    begin
      Writeln(F,Inc_Mat_Cols[i] + ',' + inttostr(Inc_Mat_levels[i]));
    end;
    GlobalResult := FileNm;
    CloseFile(F);
  End;
End;
//-------------------------------------------------------------------
Procedure ExportLaplacian(FileNm:String);
var
  F             : TextFile;
  i             : Integer;
Begin
  with ActiveCircuit[ActiveActor].Solution do
  Begin
    Assignfile(F,FileNm);
    ReWrite(F);
    Writeln(F,'Row,Col,Value');
    for i := 0 to (Laplacian.NZero -1) do
    begin
      Writeln(F,inttostr(Laplacian.data[i][0]) + ',' + inttostr(Laplacian.data[i][1]) + ',' + inttostr(Laplacian.data[i][2]));
    end;
    GlobalResult := FileNm;
    CloseFile(F);
  End;
End;
//-------------------------------------------------------------------
Procedure ExportZLL(FileNm:String);
var
  F             : TextFile;
  i             : Integer;
Begin
  if ADiakoptics then
  Begin
    with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution do
    Begin
      Assignfile(F,FileNm);
      ReWrite(F);
      Writeln(F,'Row,Col,Value(Real), Value(Imag)');
      for i := 0 to (ZLL.NZero -1) do
      begin
        Writeln(F,inttostr(ZLL.CData[i].Row) + ',' + inttostr(ZLL.CData[i].Col) + ',' + floattostr(ZLL.CData[i].Value.Re)+ ',' + floattostr(ZLL.CData[i].Value.Im));
      end;
      GlobalResult := FileNm;
      CloseFile(F);
    End;
  End;
End;
//-------------------------------------------------------------------
Procedure ExportZCC(FileNm:String);
var
  F             : TextFile;
  i             : Integer;
Begin
  if ADiakoptics then
  Begin
    with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution do
    Begin
      Assignfile(F,FileNm);
      ReWrite(F);
      Writeln(F,'Row,Col,Value(Real), Value(Imag)');
      for i := 0 to (ZCC.NZero -1) do
      begin
        Writeln(F,inttostr(ZCC.CData[i].Row) + ',' + inttostr(ZCC.CData[i].Col) + ',' + floattostr(ZCC.CData[i].Value.Re)+ ',' + floattostr(ZCC.CData[i].Value.Im));
      end;
      GlobalResult := FileNm;
      CloseFile(F);
    End;
  End;
End;
//-------------------------------------------------------------------
Procedure ExportY4(FileNm:String);
var
  F             : TextFile;
  i             : Integer;
Begin
  if ADiakoptics then
  Begin
    with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution do
    Begin
      Assignfile(F,FileNm);
      ReWrite(F);
      Writeln(F,'Row,Col,Value(Real), Value(Imag)');
      for i := 0 to (Y4.NZero -1) do
      begin
        Writeln(F,inttostr(Y4.CData[i].Row) + ',' + inttostr(Y4.CData[i].Col) + ',' + floattostr(Y4.CData[i].Value.Re)+ ',' + floattostr(Y4.CData[i].Value.Im));
      end;
      GlobalResult := FileNm;
      CloseFile(F);
    End;
  End;
End;
//-------------------------------------------------------------------
Procedure ExportC(FileNm:String);
var
  F             : TextFile;
  i             : Integer;
Begin
  if ADiakoptics then
  Begin
    with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution do
    Begin
      Assignfile(F,FileNm);
      ReWrite(F);
      Writeln(F,'Row,Col,Value');
      for i := 0 to (Contours.NZero -1) do
      begin
        Writeln(F,inttostr(Contours.CData[i].Row) + ',' + inttostr(Contours.CData[i].Col) + ',' + floattostr(Contours.CData[i].Value.Re));
      end;
      GlobalResult := FileNm;
      CloseFile(F);
    End;
  End;
End;
//-------------------------------------------------------------------
Procedure ExportVoltagesElements(FileNm:String);

// Export element voltages, by terminal and node/bus

Var
   MaxNumNodes  :Integer ;
   MaxNumTerminals : Integer;
   F         :TextFile;
   i, j      :Integer;
   pElem     :TDSSCktElement;

Begin

     MaxNumTerminals := 2;
     MaxNumNodes := 0;
     pElem := ActiveCircuit[ActiveActor].CktElements.First;
     While pElem<>nil Do Begin
        MaxNumTerminals := max(MaxNumTerminals, pElem.NTerms );
        MaxNumNodes := max(MaxNumNodes, pElem.NConds );
        pElem := ActiveCircuit[ActiveActor].CktElements.Next;
     End;
{
    MaxNumNodes := 0;
    With ActiveCircuit Do
    For j := 1 to NumBuses Do
       MaxNumNodes := max(MaxNumNodes, Buses^[j].NumNodesThisBus);
}

    Try
       Assignfile(F, FileNm);
       ReWrite(F);

       Write(F, 'Element,NumTerminals');

       //Write out the header
       for i := 1 to MaxNumTerminals do
        begin
          Write(F,Format(', Terminal%d',[i]));
          Write(F,',NumConductors,NPhases,');
          Write(F,'Bus, BasekV');
          For j := 1 to MaxNumNodes Do Write(F,Format(', Node%d_%d, Magnitude%d_%d, Angle%d_%d, pu%d_%d',[i, j, i, j, i, j, i, j]));
        end;

       Writeln(F);

       //Go through all the sources
       WITH ActiveCircuit[ActiveActor] DO BEGIN
         pElem := sources.First;

         WHILE pElem<>nil DO Begin
         IF pElem.Enabled THEN begin
               WriteElementVoltagesExportFile(F, pElem, MaxNumNodes);
               Writeln(F);
               end;
          pElem := ActiveCircuit[ActiveActor].sources.Next;
         End;


       //Go through all the PDElements
     pElem := ActiveCircuit[ActiveActor].PDElements.First;

     WHILE pElem<>nil DO Begin
       IF pElem.Enabled THEN
       begin
         WriteElementVoltagesExportFile(F, pElem, MaxNumNodes);
         Writeln(F);
       end;
       pElem := ActiveCircuit[ActiveActor].PDElements.Next;
     End;



     //Go through all the PCElements
     pElem := ActiveCircuit[ActiveActor].PCElements.First;

     WHILE pElem<>nil DO Begin
       IF pElem.Enabled THEN
       begin
         WriteElementVoltagesExportFile(F, pElem, MaxNumNodes);
         Writeln(F);
       end;
       pElem := ActiveCircuit[ActiveActor].PCElements.Next;
     End;
   End;

      GlobalResult := FileNm;

   FINALLY

       CloseFile(F);

   End;

End;
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

Procedure ExportGICMvar(FileNm:String);

Var
    F          :TextFile;
    pElem      :TGICTransformerObj;
    GICClass   :TGICTransformer;

Begin


  Try
     Assignfile(F, FileNm);
     ReWrite(F);

     GICClass := TGICTransformer(GetDSSClassPtr('GICTransformer'));

     Writeln(F, 'Bus, Mvar, GIC Amps per phase');
     pElem := TGICTransformerObj(GICClass.ElementList.First);
     while PElem <> Nil do
     Begin
         pElem.WriteVarOutputRecord(F, ActiveActor);
         pElem := TGICTransformerObj(GICClass.ElementList.Next);
     End;

     GlobalResult := FileNm;

  Finally
     CloseFile(F);
  End;

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportBusReliability(FileNm:String);
Var
   F :TextFile;
   i:Integer;

Begin

  Try
     Assignfile(F, FileNm);
     ReWrite(F);
     Writeln(F, 'Bus, Lambda, Num-Interruptions, Num-Customers, Cust-Interruptions, Duration, Total-Miles');
     With ActiveCircuit[ActiveActor] Do
     For i := 1 to NumBuses Do
       With Buses^[i] Do
       Begin
           Writeln(F, Format('%s, %-.11g, %-.11g, %d, %-.11g, %-.11g, %-.11g',
              [CheckForBlanks(Uppercase(BusList.Get(i))), BusFltRate, Bus_Num_Interrupt, BusTotalNumCustomers, BusCustInterrupts, Bus_Int_Duration, BusTotalMiles ]));
       End;

     GlobalResult := FileNm;

  FINALLY

     CloseFile(F);
  End;

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportBranchReliability(FileNm:String);
Var
   F : TextFile;
   pElem : TPDElement;
   pBus  : TDSSBus;
   SAIFI : Double;
   MaxCustomers : Integer;

Begin

  Try
     Assignfile(F, FileNm);
     ReWrite(F);
     Writeln(F, 'Element, Lambda, "Accumulated-Lambda", Num-Customers, Total-Customers, Num-Interrupts, Cust-Interruptions, Cust-Durations, Total-Miles, Cust-Miles, SAIFI');
     With ActiveCircuit[ActiveActor] Do
     Begin

     // Find Maxcustomers of any PDElement for Duke Recloser siting algorithm
       MaxCustomers := 0;
       pElem := ActiveCircuit[ActiveActor].PDElements.First;
       WHILE pElem <> nil DO
       BEGIN
         IF pElem.Enabled THEN WITH pElem Do
            BEGIN
                pBus := Buses^[Terminals^[FromTerminal].BusRef] ;
                With pBus Do If BusTotalNumCustomers > MaxCustomers Then
                     MaxCustomers := BusTotalNumCustomers;
            END;
         pElem := ActiveCircuit[ActiveActor].PDElements.Next;
       END;


     // write report for PDELEMENTS only
       pElem := ActiveCircuit[ActiveActor].PDElements.First;
       WHILE pElem <> nil DO
       BEGIN
         IF pElem.Enabled THEN WITH pElem Do
            BEGIN
                pBus := Buses^[Terminals^[FromTerminal].BusRef] ;
                With pBus Do If BusTotalNumCustomers>0 Then
                     SAIFI := BusCustInterrupts/BusTotalNumCustomers Else SAIFI := 0.0 ;

                Writeln(F, Format('%s.%s, %-.11g, %-.11g, %d, %d, %-.11g, %-.11g, %-.11g, %-.11g, %-.11g, %-.11g',
                [ParentClass.Name, Name, BranchFltRate, AccumulatedBrFltRate, BranchNumCustomers, BranchTotalCustomers,
                 pBus.Bus_Num_Interrupt, BranchTotalCustomers*pBus.Bus_Num_Interrupt, pBus.BusCustDurations,
                 AccumulatedMilesDownStream, (MaxCustomers - BranchTotalCustomers) * AccumulatedMilesDownStream, SAIFI ]));
            END;
         pElem := ActiveCircuit[ActiveActor].PDElements.Next;
       END;
     END;


     GlobalResult := FileNm;

  FINALLY

     CloseFile(F);
  End;

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportNodeNames(FileNm:String);
Var
   F : TextFile;
   i : Integer;
   j : Integer;
   BusName : String;

Begin

  Try
     Assignfile(F, FileNm);
     ReWrite(F);
     Writeln(F, 'Node_Name');
     With ActiveCircuit[ActiveActor] Do
     Begin

       FOR i := 1 to NumBuses DO
       Begin
           BusName := BusList.Get(i);
           With Buses^[i] Do
           FOR j := 1 to NumNodesThisBus DO
           Begin
                Writeln(F, Format('%s.%d ', [BusName, GetNum(j)]) );
           End;
       End;

     END;


     GlobalResult := FileNm;

  FINALLY

     CloseFile(F);
  End;

End;
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

Function TapPosition(const Transformer:TTransfObj; iWind:Integer):Integer;

{Assumes 0  is 1.0 per unit tap}

Begin
        With Transformer Do
        Result :=   Round((PresentTap[iWind,ActiveActor] - (Maxtap[iWind] + Mintap[iWind])/2.0 ) / TapIncrement[iWind]  );

End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure ExportTaps(FileNm:String);
Var
   F : TextFile;
   iWind :Integer;
   pReg: TRegControlObj;
Begin

  Try

         Assignfile(F,FileNm);
         ReWrite(F);
         Writeln(F, 'Name, Tap, Min, Max, Step, Position');

         WITH ActiveCircuit[ActiveActor] Do
         Begin
             pReg := RegControls.First;
             WHILE pReg <> NIL Do
             Begin
                  WITH pReg.Transformer Do
                  Begin
                       iWind := pReg.TrWinding;
                       Write(F, Name );
                       Writeln(F, Format(', %8.5f, %8.5f, %8.5f, %8.5f, %d' , [PresentTap[iWind,ActiveActor], MinTap[iWind], MaxTap[iWind], TapIncrement[iWind], TapPosition(pREg.Transformer, iWind)]));
                  End;
                 pReg := RegControls.Next;
             End;
         End;


     GlobalResult := FileNm;

  FINALLY

     CloseFile(F);
  End;

End;

Procedure ExportResult(FileNm:String);

Var
   F : TextFile;

Begin

  Try
     Assignfile(F, FileNm);
     ReWrite(F);
     Parservars.Lookup('@result');
     Writeln(F, Parservars.Value );

     GlobalResult := FileNm;

  FINALLY

     CloseFile(F);
  End;


End;

Procedure ExportYNodeList(FileNM:String);
VAR
   i : Integer;
   F : TextFile;

Begin

  Try
     Assignfile(F, FileNm);
     ReWrite(F);

    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       FOR i := 1 to NumNodes DO
       Begin
             With MapNodeToBus^[i] do
             Writeln(F, Format('"%s.%-d"',[Uppercase(BusList.Get(Busref)), NodeNum]));
       End;
     End;

    GlobalResult := FileNm;

  Finally
      CloseFile(F);
  End;

End;

Procedure ExportYVoltages(FileNM:String);
VAR
   i : Integer;
   F : TextFile;

Begin

  Try
     Assignfile(F, FileNm);
     ReWrite(F);

    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       FOR i := 1 to NumNodes DO
       Begin
            With solution.NodeV^[i] Do
             Writeln(F, Format('%10.6g, %10.6g',[re, im]));
       End;
     End;

    GlobalResult := FileNm;

  Finally
      CloseFile(F);
  End;


End;

Procedure ExportYCurrents(FileNM:String);
VAR
   i : Integer;
   F : TextFile;

Begin

  Try
     Assignfile(F, FileNm);
     ReWrite(F);

    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       FOR i := 1 to NumNodes DO
       Begin
            With solution.Currents^[i] Do
             Writeln(F, Format('%10.6g, %10.6g',[re, im]));
       End;
     End;

    GlobalResult := FileNm;

  Finally
      CloseFile(F);
  End;

End;


Procedure ExportSections(FileNM:String; pMeter:TEnergyMeterObj);

Var
   MyMeterPtr:TEnergyMeterObj;
   iMeter, i : Integer;
   F : TextFile;


Begin

  Try
     Assignfile(F, FileNm);
     ReWrite(F);

     // Write Header
     Writeln(F, 'Meter, SectionID, SeqIndex, DeviceType, NumCustomers, NumBranches, AvgRepairHrs, TotalDownlineCust, SectFaultRate,SumFltRatesXRepairHrs, SumBranchFltRates, HeadBranch ');

   If Assigned(pMeter) Then
     // If a meter is specified, export that meter only
     With pMeter Do Begin
         for i  := 1 to SectionCount  do
           With FeederSections^[i] Do Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := TDSSCktElement(sequenceList.Get(SeqIndex));
              Writeln(F, Format('%s, %d, %d, %s, %d, %d, %-.6g, %d, %-.6g, %-.6g, %-.6g, %s',
              [Name, i, SeqIndex, GetOCPDeviceTypeString(OCPDeviceType), NCustomers, NBranches, AverageRepairTime, TotalCustomers, SectFaultRate, SumFltRatesXRepairHrs, SumBranchFltRates,
               FullName(ActiveCircuit[ActiveActor].ActiveCktElement)  ]));
           End;
     End
   Else    // export sections for all meters
     Begin
        iMeter := EnergyMeterClass[ActiveActor].First;
        while iMeter>0 do Begin
             MyMeterPtr:=EnergyMeterClass[ActiveActor].GetActiveObj;
             With MyMeterPtr Do Begin
                 for i  := 1 to SectionCount  do
                    With FeederSections^[i] Do Begin
                      ActiveCircuit[ActiveActor].ActiveCktElement := TDSSCktElement(sequenceList.Get(SeqIndex));
                      Writeln(F, Format('%s, %d, %d, %s, %d, %d, %-.6g, %d, %-.6g, %-.6g, %-.6g, %s',
                      [Name, i, SeqIndex, GetOCPDeviceTypeString(OCPDeviceType), NCustomers, NBranches, AverageRepairTime, TotalCustomers, SectFaultRate, SumFltRatesXRepairHrs, SumBranchFltRates,
                      FullName(ActiveCircuit[ActiveActor].ActiveCktElement)  ]));
                    End;
                iMeter := EnergyMeterClass[ActiveActor].Next;
            End;
        End;
     End;

     GlobalResult := FileNm;

  Finally
     CloseFile(F);
  End;

End;


end.


