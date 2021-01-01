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

interface

uses
    EnergyMeter,
    XYCurve;

procedure ExportVoltages(FileNm: String);
procedure ExportSeqVoltages(FileNm: String);
procedure ExportCurrents(FileNm: String);
procedure ExportEstimation(Filenm: String);
procedure ExportSeqCurrents(FileNm: String);
procedure ExportPowers(FileNm: String; opt: Integer);
procedure ExportPbyphase(FileNm: String; opt: Integer);
procedure ExportSeqPowers(FileNm: String; opt: Integer);
procedure ExportFaultStudy(FileNm: String);
procedure ExportMeters(FileNm: String);
procedure ExportGenMeters(FileNm: String);
procedure ExportPVSystemMeters(FileNm: String);
procedure ExportPVSystem2Meters(FileNm: String);
procedure ExportStorageMeters(FileNm: String);
procedure ExportStorage2Meters(FileNm: String);
procedure ExportLoads(FileNm: String);
procedure ExportCapacity(FileNm: String);
procedure ExportOverloads(FileNm: String);
procedure ExportUnserved(FileNm: String; UE_Only: Boolean);
procedure ExportYprim(FileNm: String);
procedure ExportY(FileNm: String; TripletOpt: Boolean);
procedure ExportSeqZ(FileNm: String);
procedure ExportBusCoords(FileNm: String);
procedure ExportLosses(FileNm: String);
procedure ExportUuids(FileNm: String);
procedure ExportCounts(FileNm: String);
procedure ExportSummary(FileNm: String);
procedure ExportProfile(FileNm: String; PhasesToPlot: Integer);
procedure ExportEventLog(FileNm: String);
procedure ExportVoltagesElements(FileNm: String);
procedure ExportGICMvar(FileNm: String);
procedure ExportBusReliability(FileNm: String);
procedure ExportBranchReliability(FileNm: String);
procedure ExportNodeNames(FileNm: String);
procedure ExportTaps(FileNm: String);
procedure ExportNodeOrder(FileNm: String);
procedure ExportElemCurrents(FileNm: String);
procedure ExportElemVoltages(FileNm: String);
procedure ExportElemPowers(FileNm: String);
procedure ExportResult(FileNm: String);
procedure ExportYNodeList(FileNM: String);
procedure ExportYVoltages(FileNM: String);
procedure ExportYCurrents(FileNM: String);
procedure ExportSections(FileNM: String; pMeter: TEnergyMeterObj);
procedure ExportErrorLog(FileNm: String);
procedure ExportIncMatrix(FileNm: String);
procedure ExportIncMatrixRows(FileNm: String);
procedure ExportIncMatrixCols(FileNm: String);
procedure ExportBusLevels(FileNm: String);
procedure ExportLaplacian(FileNm: String);
procedure ExportZLL(FileNm: String);
procedure ExportZCC(FileNm: String);
procedure ExportY4(FileNm: String);
procedure ExportC(FileNm: String);


implementation

uses
    Classes,
    uComplex,
    Arraydef,
    Sysutils,
    Circuit,
    DSSClassDefs,
    DSSGlobals,
    uCMatrix,
    solution,
    CktElement,
    Utilities,
    Bus,
    MathUtil,
    DSSClass,
    PDElement,
    PCElement,
    Generator,
    Sensor,
    Load,
    RegControl,
    Transformer,
    ParserDel,
    Math,
    Ymatrix,
    LineGeometry,
    WireData,
    LineCode,
    XfmrCode,
    NamedObject,
    GICTransformer,
    PVSystem,
    PVSystem2,
    Storage,
    Storage2,
    KLUSolve,
    ExportCIMXML,
    LineSpacing,
    CNData,
    TSData,
    BufStream;

procedure WriteElementVoltagesExportFile(F: TFileStream; pElem: TDSSCktElement; MaxNumNodes: Integer);

var
    NCond, Nterm, i, j, k, m, nref, bref: Integer;
    Busname: String;
    Volts: Complex;
    Vpu, Vmag: Double;

begin
    NCond := pElem.NConds;
    Nterm := pElem.Nterms;
    k := 0;
    BusName := (StripExtension(pElem.FirstBus));
    FSWrite(F, Format('%s.%s', [pElem.DSSClassName, pElem.Name]));


    FSWrite(F, Format(',%d', [NTerm]));

    for j := 1 to NTerm do
    begin
        FSWrite(F, Format(',%d,', [j]));
        FSWrite(F, Format('%d,%d,', [NCond, pElem.NPhases]));

        FSWrite(F, Format('%s,', [UpperCase(BusName)]));
        for i := 1 to NCond do
        begin
            Inc(k);
            nref := pElem.NodeRef^[k];
            Volts := ActiveCircuit.Solution.NodeV^[nref];
            Vmag := Cabs(Volts) * 0.001;
            if nref = 0 then
                Vpu := 0.0
            else
                with ActiveCircuit do
                begin
                    bref := MapNodeToBus^[nref].BusRef;
                    if Buses^[bref].kvbase <> 0.0 then
                        Vpu := Vmag / Buses^[bref].kVBase
                    else
                        Vpu := 0.0;

                    if (i = 1) then
                        FSWrite(F, Format('%6.3f', [Buses^[bref].kvBase * sqrt(3)]));
                end;
            with ActiveCircuit do
            begin
                FSWrite(F, Format(', %d, %10.6g, %6.3f, %9.5g', [k, Vmag, cdang(Volts), Vpu]));
            end; //end with ActiveCircuit

        end; //end numconductors

   {Zero Fill row}
        for m := (NCond + 1) to (MaxNumNodes) do
            FSWrite(F, ', 0, 0, 0, 0');

        BusName := StripExtension(pElem.Nextbus);
    end; // end for numterminals
end; //end procedure


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportSeqVoltages(FileNm: String);

// Export Symmetrical Component bus voltages

var
    F: TFileStream = nil;
    i, j: Integer;
    nref: Integer;
    Vph, VphLL, V012: array[1..3] of Complex;

    V0, V1, V2,
    Vpu, V2V1, V0V1: Double;
    Vresidual: Complex;
    V_NEMA: Double;

begin

    try
        F := TFileStream.Create(FileNm, fmCreate);

        FSWriteln(F, 'Bus,  V1,  p.u.,Base kV, V2, %V2/V1, V0, %V0/V1, Vresidual, %NEMA');
        with ActiveCircuit do
        begin
            for i := 1 to NumBuses do
            begin

                if Buses^[i].NumNodesThisBus < 3 then
                begin
                    V0 := 0.0;
                    V2 := 0.0;
                    V_NEMA := 0.0;
                    if (Buses^[i].NumNodesThisBus = 1) and PositiveSequence then
                    begin // first node
                        nref := Buses^[i].GetRef(1);
                        Vph[1] := ActiveCircuit.Solution.NodeV^[nref];
                        V1 := Cabs(Vph[1]);
                    end
                    else
                        V1 := 0.0;
                end
                else
                begin

                    with  ActiveCircuit.Solution, Buses^[i] do
                        for j := 1 to 3 do
                        begin      // first nodes named  1, 2, 3
                            Vph[j] := NodeV^[GetRef(FindIdx(j))];
                        end;

             {Compute LL voltages for Nema unbalance calc}
                    VphLL[1] := Csub(Vph[1], Vph[2]);
                    VphLL[2] := Csub(Vph[2], Vph[3]);
                    VphLL[3] := Csub(Vph[3], Vph[1]);

                    Phase2SymComp(@Vph, @V012);

                    V0 := Cabs(V012[1]);
                    V1 := Cabs(V012[2]);
                    V2 := Cabs(V012[3]);

                    V_NEMA := PctNemaUnbalance(@VphLL);
                end;

                if Buses^[i].kvbase <> 0.0 then
                    Vpu := 0.001 * V1 / Buses^[i].kVBase
                else
                    Vpu := 0.0;

                if V1 > 0.0 then
                begin
                    V2V1 := 100.0 * V2 / V1;
                    V0V1 := 100.0 * V0 / V1;
                end
                else
                begin
                    V2V1 := 0.0;
                    V0V1 := 0.0;
                end;

                Vresidual := CZERO;
                with ActiveCircuit.Solution do
                    for j := 1 to Buses^[i].NumNodesThisBus do
                        Caccum(Vresidual, NodeV^[Buses^[i].GetRef(j)]);

                FSWriteln(F,
                    Format('"%s", %10.6g, %9.5g, %8.2f, %10.6g, %8.4g, %10.6g, %8.4g, %10.6g, %8.4g',
                    [Uppercase(BusList.Get(i)), V1, Vpu, (Buses^[i].kvbase * SQRT3), V2, V2V1, V0, V0V1, Cabs(Vresidual), V_NEMA]
                    ));


            end;
        end;


        GlobalResult := FileNm;

    finally

        FreeAndNil(F);
    end;

end;

//-------------------------------------------------------------------
procedure ExportVoltages(FileNm: String);

// Export Symmetrical Component bus voltages

var
    MaxNumNodes: Integer;
    F: TFileStream = nil;
    i, j, jj: Integer;
    BusName: String;
    Volts: Complex;
    nref: Integer;
    NodeIdx: Integer;
    Vmag,
    Vpu: Double;


begin

  {Find max nodes at a bus}
    MaxNumNodes := 0;
    with ActiveCircuit do
        for i := 1 to NumBuses do
            MaxNumNodes := max(MaxNumNodes, Buses^[i].NumNodesThisBus);

    try
        F := TFileStream.Create(FileNm, fmCreate);

        FSWrite(F, 'Bus, BasekV');
        for i := 1 to MaxNumNodes do
            FSWrite(F, Format(', Node%d, Magnitude%d, Angle%d, pu%d', [i, i, i, i]));
        FSWriteln(F);

        with ActiveCircuit do
        begin
            for i := 1 to NumBuses do
            begin
                BusName := BusList.Get(i);
                FSWrite(F, Format('"%s", %.5g', [UpperCase(BusName), Buses^[i].kvbase * SQRT3]));

                jj := 1;
                with Buses^[i] do
                    for j := 1 to NumNodesThisBus do
                    begin
                        repeat
                            NodeIdx := FindIdx(jj);     // Try to find nodes in order
                            inc(jj)
                        until NodeIdx > 0;
                        nref := GetRef(NodeIdx);
                        Volts := ActiveCircuit.Solution.NodeV^[nref];
                        Vmag := Cabs(Volts);
                        if kvbase <> 0.0 then
                            Vpu := 0.001 * Vmag / kVBase
                        else
                            Vpu := 0.0;

                        FSWrite(F,
                            Format(', %d, %10.6g, %6.1f, %9.5g',
                            [GetNum(NodeIdx), Vmag, cdang(Volts), Vpu]));
                    end;
           {Zero Fill row}
                for j := Buses^[i].NumNodesThisBus + 1 to MaxNumNodes do
                    FSWrite(F, ', 0, 0, 0, 0');
                FSWriteln(F);
            end;
        end;

        GlobalResult := FileNm;

    finally

        FreeAndNil(F);

    end;

end;
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure CalcAndWriteSeqCurrents(F: TFileStream; j: Integer; pelem: TDSSCktElement; cBuffer: pComplexArray; DoRatings: Boolean);
var
    I0, I1, I2, I2I1, I0I1, iNormal, iEmerg: Double;
    i, k, NCond: Integer;
    Iph, I012: array[1..3] of Complex;
    Iresidual: Complex;
    I_NEMA: Double;


begin
    NCond := pelem.NConds;
    if (pelem.Nphases >= 3) then
    begin

        for i := 1 to 3 do
        begin
            k := (j - 1) * Ncond + i;
            Iph[i] := cBuffer^[k];
        end;

        Phase2SymComp(@Iph, @I012);
        I0 := Cabs(I012[1]);
        I1 := Cabs(I012[2]);
        I2 := Cabs(I012[3]);

        I_NEMA := PctNemaUnbalance(@Iph);

    end
    else
    begin
        I0 := 0.0;
        I1 := 0.0;
        I2 := 0.0;
        I_NEMA := 0.0;
        if ActiveCircuit.PositiveSequence    // Use phase 1 only
        then
        begin
            k := (j - 1) * Ncond + 1;
            Iph[1] := cBuffer^[k];
            I1 := Cabs(Iph[1]);
        end;

    end;

    if I1 > 0.0 then
    begin
        I2I1 := 100.0 * I2 / I1;
        I0I1 := 100.0 * I0 / I1;
    end
    else
    begin
        I2I1 := 0.0;
        I0I1 := 0.0;
    end;

    if DoRatings and (j = 1)  // Only for 1st Terminal
    then
    begin
        iNormal := TPDElement(Pelem).NormAmps;
        if iNormal > 0.0 then
            iNormal := I1 / iNormal * 100.0;
        iEmerg := TPDElement(Pelem).EmergAmps;
        if iEmerg > 0.0 then
            iEmerg := I1 / iEmerg * 100.0;
    end
    else
    begin
        iNormal := 0.0;
        iEmerg := 0.0;
    end;

    Iresidual := CZERO;
    for i := 1 to Ncond do
        Caccum(Iresidual, cBuffer^[i]);


    FSWriteln(F, Format('"%s", %3d, %10.6g, %8.4g, %8.4g, %10.6g, %8.4g, %10.6g, %8.4g, %10.6g, %8.4g',
        [(pelem.DSSClassName + '.' + UpperCase(pelem.Name)), j, I1, iNormal, iEmerg, I2, I2I1, I0, I0I1, Cabs(Iresidual), I_NEMA]));
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportSeqCurrents(FileNm: String);

var
    F: TFileStream = nil;
    j: Integer;
    pElem: TDSSCktElement;
    PDElem: TPDElement;
    PCelem: TPCelement;
    cBuffer: pComplexArray;  // Allocate to max total conductors

begin

    cBuffer := NIL;

    try
        F := TFileStream.Create(FileNm, fmCreate);

        {Sequence Currents}
        FSWriteln(F, 'Element, Terminal,  I1, %Normal, %Emergency, I2, %I2/I1, I0, %I0/I1, Iresidual, %NEMA');

        {Allocate cBuffer big enough for largest circuit element}
        Getmem(cbuffer, sizeof(Complex) * GetMaxCktElementSize);


        //Sources First
        Pelem := ActiveCircuit.Sources.First;
        while pelem <> NIL do
        begin
            if (pelem.Enabled) then
            begin
                pelem.GetCurrents(cBuffer);
                for j := 1 to pelem.Nterms do
                    CalcAndWriteSeqCurrents(F, j, pelem, cBuffer, FALSE);
            end;
            pelem := ActiveCircuit.Sources.Next;
        end;


        // PDELEMENTS Next
        PDelem := ActiveCircuit.PDElements.First;

        while PDelem <> NIL do
        begin
            if (PDelem.Enabled) then
            begin
                PDelem.GetCurrents(cBuffer);
                for j := 1 to PDelem.Nterms do
                    CalcAndWriteSeqCurrents(F, j, pDelem, cBuffer, TRUE);
            end;
            PDelem := ActiveCircuit.PDElements.Next;
        end;

        // PCelemENTS next
        PCelem := ActiveCircuit.PCelements.First;

        while PCelem <> NIL do
        begin
            if (PCelem.Enabled) then
            begin
                PCelem.GetCurrents(cBuffer);
                for j := 1 to PCelem.Nterms do
                    CalcAndWriteSeqCurrents(F, j, pCelem, cBuffer, FALSE);
            end;
            PCelem := ActiveCircuit.PCelements.Next;
        end;


        //Faults Next
        Pelem := ActiveCircuit.Faults.First;
        while pelem <> NIL do
        begin
            if (pelem.Enabled) then
            begin
                pelem.GetCurrents(cBuffer);
                for j := 1 to pelem.Nterms do
                    CalcAndWriteSeqCurrents(F, j, pelem, cBuffer, FALSE);
            end;
            pelem := ActiveCircuit.Faults.Next;
        end;

        GlobalResult := FileNm;


    finally
        if Assigned(Cbuffer) then
            Freemem(cBuffer);
        FreeAndNil(F);

    end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure CalcAndWriteCurrents(F: TFileStream; pElem: TDSSCktElement; Cbuffer: pComplexArray; CondWidth, TermWidth: Integer);
var
    i, j, k: Integer;
    Iresid: Complex;

begin
    k := 0;
    FSWrite(F, Format('%s', [pelem.DSSClassName + '.' + UpperCase(pElem.Name)]));
    for      j := 1 to pElem.Nterms do
    begin
        Iresid := CZERO;
        for    i := 1 to pElem.NConds do
        begin
            Inc(k);
            FSWrite(F,
                Format(', %10.6g, %8.2f', [Cabs(cBuffer^[k]), cdang(cBuffer^[k])]));
            Caccum(Iresid, cBuffer^[k]);
        end;
        for i := pElem.Nconds + 1 to CondWidth do
            FSWrite(F, Format(', %10.6g, %8.2f', [0.0, 0.0]));
        FSWrite(F, Format(', %10.6g, %8.2f', [Cabs(Iresid), cdang(Iresid)]));
    end;

    {Filler if no. terms less than termwidth}
    for j := pElem.Nterms + 1 to TermWidth do
        for i := 1 to Condwidth + 1 do
            FSWrite(F, Format(', %10.6g, %8.2f', [0.0, 0.0]));

    FSWriteln(F);
end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure CalcAndWriteMaxCurrents(F: TFileStream; pElem: TPDElement; Cbuffer: pComplexArray);
var
    RatingIdx,
    i: Integer;
    EmergAmps,
    NormAmps,
    Currmag,
    MaxCurrent: Double;
    LocalPower: Complex;
    RSignal: TXYCurveObj;

begin
    // Initializes NomrAmps and EmergAmps with the default values for the PDElement
    NormAmps := pElem.NormAmps;
    EmergAmps := pElem.EmergAmps;

    if SeasonalRating then
    begin
        if SeasonSignal <> '' then
        begin
            RSignal := XYCurveClass.Find(SeasonSignal);
            if RSignal <> NIL then
            begin
                RatingIdx := trunc(RSignal.GetYValue(ActiveCircuit.Solution.DynaVars.intHour));
          // Brings the seasonal ratings for the PDElement
                if (RatingIdx <= PElem.NumAmpRatings) and (PElem.NumAmpRatings > 1) then
                begin
                    NormAmps := pElem.AmpRatings[RatingIdx];
                    EmergAmps := pElem.AmpRatings[RatingIdx];
                end;
            end
            else
                SeasonalRating := FALSE;   // The XYCurve defined doesn't exist
        end
        else
            SeasonalRating := FALSE;    // The user didn't define the seasonal signal
    end;

    FSWrite(F, Format('%s.%s', [pelem.DSSClassName, UpperCase(pElem.Name)]));
    MaxCurrent := 0.0;
    for    i := 1 to pElem.Nphases do
    begin
        Currmag := Cabs(Cbuffer^[i]);
        if Currmag > MaxCurrent then
            MaxCurrent := Currmag;
    end;
    //----pElem.ActiveTerminalIdx := 1;
    LocalPower := CmulReal(pElem.Power[1], 0.001);
    if (pElem.NormAmps = 0.0) or (pElem.EmergAmps = 0.0) then
        FSWrite(F, Format(', %10.6g, %8.2f, %8.2f', [MaxCurrent, 0.0, 0.0]))
    else
        FSWrite(F, Format(', %10.6g, %8.2f, %8.2f', [MaxCurrent, MaxCurrent / NormAmps * 100.0, MaxCurrent / Emergamps * 100.0]));

    FSWrite(F, Format(', %10.6g, %10.6g, %d, %d, %d', [Localpower.re, Localpower.im, pElem.BranchNumCustomers, pElem.BranchTotalCustomers, pElem.NPhases]));
    with ActiveCircuit do
        FSWrite(F, Format(', %-.3g ', [Buses^[MapNodeToBus^[PElem.NodeRef^[1]].BusRef].kVBase]));
    FSWriteln(F);
end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportCurrents(FileNm: String);

var
    F: TFileStream = nil;
    cBuffer: pComplexArray;
    pElem: TDSSCktElement;
    MaxCond, MaxTerm: Integer;
    i, j: Integer;

begin

    cBuffer := NIL;

    try
        F := TFileStream.Create(FileNm, fmCreate);

        Getmem(cBuffer, sizeof(cBuffer^[1]) * GetMaxCktElementSize);

        {Calculate the width of the file}
        MaxCond := 1;
        MaxTerm := 2;
        pElem := ActiveCircuit.CktElements.First;
        while pElem <> NIL do
        begin
            if pelem.NTerms > MaxTerm then
                MaxTerm := pelem.NTerms;
            if pelem.NConds > MaxCond then
                MaxCond := pelem.NConds;
            pElem := ActiveCircuit.CktElements.Next;
        end;


        {Branch Currents}
        FSWrite(F, 'Element');
        for i := 1 to MaxTerm do
        begin
            for j := 1 to MaxCond do
                FSWrite(F, Format(', I%d_%d, Ang%d_%d', [i, j, i, j]));
            FSWrite(F, Format(', Iresid%d, AngResid%d', [i, i]));
        end;
        FSWriteln(F);


     // Sources first
        pElem := ActiveCircuit.Sources.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                pElem.GetCurrents(cBuffer);
                CalcAndWriteCurrents(F, pElem, Cbuffer, maxcond, maxterm);
            end;
            pElem := ActiveCircuit.Sources.Next;
        end;


     // PDELEMENTS first
        pElem := ActiveCircuit.PDElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                pElem.GetCurrents(cBuffer);
                CalcAndWriteCurrents(F, pElem, Cbuffer, maxcond, maxterm);
            end;
            pElem := ActiveCircuit.PDElements.Next;
        end;

     // Faults
        pElem := ActiveCircuit.Faults.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                pElem.GetCurrents(cBuffer);
                CalcAndWriteCurrents(F, pElem, Cbuffer, maxcond, maxterm);
            end;
            pElem := ActiveCircuit.Faults.Next;
        end;

     // PCELEMENTS next
        pElem := ActiveCircuit.PCElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                pElem.GetCurrents(cBuffer);
                CalcAndWriteCurrents(F, pElem, Cbuffer, maxcond, maxterm);
            end;
            pElem := ActiveCircuit.PCElements.Next;
        end;

        GlobalResult := FileNm;


    finally
        if Assigned(cBuffer) then
            Freemem(cBuffer);
        FreeAndNil(F);

    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure WriteNodeList(F: TFileStream; const CktElementName: String);
var
    NValues, i: Integer;


begin


    if ActiveCircuit <> NIL then
        if not ActiveCircuit.Issolved then
        begin
            DoSimpleMsg('Circuit must be solved for this command to execute properly.', 222001);
            Exit;
        end;


    if Length(CktElementName) > 0 then
    begin
        SetObject(CktElementName);


        if Assigned(ActiveCircuit.ActiveCktElement) then
            with ActiveCircuit.ActiveCktElement do
            begin
                FSWrite(F, Format('"%s", %d, %d', [CktElementName, Nterms, Nconds]));
                NValues := NConds * Nterms;
                for i := 1 to NValues do
                begin
                    FSWrite(F, Format(', %d', [GetNodeNum(NodeRef^[i])]));
                end;
                FSWriteln(F);
            end
    end;


end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportNodeOrder(FileNm: String);

{ Writes NodeLists in same order as Export Currents function
}

var
    F: TFileStream = nil;
    pElem: TDSSCktElement;
    strName: String;

begin


    try
        F := TFileStream.Create(FileNm, fmCreate);

     {Header Record}
        FSWrite(F, 'Element, Nterminals, Nconductors, Node-1, Node-2, Node-3, ...');
        FSWriteln(F);


     // Sources first
        pElem := ActiveCircuit.Sources.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteNodeList(F, strName);
            end;
            pElem := ActiveCircuit.Sources.Next;
        end;


     // PDELEMENTS first
        pElem := ActiveCircuit.PDElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteNodeList(F, strName);
            end;
            pElem := ActiveCircuit.PDElements.Next;
        end;

     // Faults
        pElem := ActiveCircuit.Faults.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteNodeList(F, strName);
            end;
            pElem := ActiveCircuit.Faults.Next;
        end;

     // PCELEMENTS next
        pElem := ActiveCircuit.PCElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteNodeList(F, strName);
            end;
            pElem := ActiveCircuit.PCElements.Next;
        end;

        GlobalResult := FileNm;


    finally
        FreeAndNil(F);

    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure WriteElemCurrents(F: TFileStream; const CktElementName: String);
var
    NValues, i: Integer;


begin


    if ActiveCircuit <> NIL then
        if not ActiveCircuit.Issolved then
        begin
            DoSimpleMsg('Circuit must be solved for this command to execute properly.', 222001);
            Exit;
        end;


    if Length(CktElementName) > 0 then
    begin
        SetObject(CktElementName);

        if Assigned(ActiveCircuit.ActiveCktElement) then
            with ActiveCircuit.ActiveCktElement do
            begin
                ComputeIterminal;
                FSWrite(F, Format('"%s", %d, %d', [CktElementName, Nterms, Nconds]));
                NValues := NConds * Nterms;
                for i := 1 to NValues do
                begin
                    FSWrite(F, Format(', %10.6g, %8.2f', [Cabs(Iterminal^[i]), cdang(Iterminal^[i])]));
                end;
                FSWriteln(F);
            end
    end;


end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportElemCurrents(FileNm: String);

{ Export currents in same order as NodeOrder export
}
var
    F: TFileStream = nil;
    pElem: TDSSCktElement;
    strName: String;

begin


    try
        F := TFileStream.Create(FileNm, fmCreate);

     {Header Record}
        FSWrite(F, 'Element, Nterminals, Nconductors, I_1, Ang_1, ...');
        FSWriteln(F);


     // Sources first
        pElem := ActiveCircuit.Sources.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemCurrents(F, strName);
            end;
            pElem := ActiveCircuit.Sources.Next;
        end;


     // PDELEMENTS first
        pElem := ActiveCircuit.PDElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemCurrents(F, strName);
            end;
            pElem := ActiveCircuit.PDElements.Next;
        end;

     // Faults
        pElem := ActiveCircuit.Faults.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemCurrents(F, strName);
            end;
            pElem := ActiveCircuit.Faults.Next;
        end;

     // PCELEMENTS next
        pElem := ActiveCircuit.PCElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemCurrents(F, strName);
            end;
            pElem := ActiveCircuit.PCElements.Next;
        end;

        GlobalResult := FileNm;


    finally
        FreeAndNil(F);

    end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure WriteElemVoltages(F: TFileStream; const CktElementName: String);
var
    NValues, i: Integer;


begin


    if ActiveCircuit <> NIL then
        if not ActiveCircuit.Issolved then
        begin
            DoSimpleMsg('Circuit must be solved for this command to execute properly.', 222001);
            Exit;
        end;


    if Length(CktElementName) > 0 then
    begin
        SetObject(CktElementName);

        if Assigned(ActiveCircuit.ActiveCktElement) then
            with ActiveCircuit.ActiveCktElement do
            begin
                ComputeVterminal;
                FSWrite(F, Format('"%s", %d, %d', [CktElementName, Nterms, Nconds]));
                NValues := NConds * Nterms;
                for i := 1 to NValues do
                begin
                    FSWrite(F, Format(', %10.6g, %8.2f', [Cabs(Vterminal^[i]), cdang(Vterminal^[i])]));
                end;
                FSWriteln(F);
            end
    end;


end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportElemVoltages(FileNm: String);
{ Export conductor voltages in same order as NodeOrder export
}
var
    F: TFileStream = nil;
    pElem: TDSSCktElement;
    strName: String;

begin


    try
        F := TFileStream.Create(FileNm, fmCreate);

     {Header Record}
        FSWrite(F, 'Element, Nterminals, Nconductors, V_1, Ang_1, ...');
        FSWriteln(F);


     // Sources first
        pElem := ActiveCircuit.Sources.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemVoltages(F, strName);
            end;
            pElem := ActiveCircuit.Sources.Next;
        end;


     // PDELEMENTS first
        pElem := ActiveCircuit.PDElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemVoltages(F, strName);
            end;
            pElem := ActiveCircuit.PDElements.Next;
        end;

     // Faults
        pElem := ActiveCircuit.Faults.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemVoltages(F, strName);
            end;
            pElem := ActiveCircuit.Faults.Next;
        end;

     // PCELEMENTS next
        pElem := ActiveCircuit.PCElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemVoltages(F, strName);
            end;
            pElem := ActiveCircuit.PCElements.Next;
        end;

        GlobalResult := FileNm;


    finally
        FreeAndNil(F);

    end;

end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure WriteElemPowers(F: TFileStream; const CktElementName: String);
var
    NValues, i: Integer;
    S: Complex;

begin


    if ActiveCircuit <> NIL then
        if not ActiveCircuit.Issolved then
        begin
            DoSimpleMsg('Circuit must be solved for this command to execute properly.', 222001);
            Exit;
        end;


    if Length(CktElementName) > 0 then
    begin
        SetObject(CktElementName);

        if Assigned(ActiveCircuit.ActiveCktElement) then
            with ActiveCircuit.ActiveCktElement do
            begin
                ComputeVterminal;
                ComputeIterminal;
                FSWrite(F, Format('"%s", %d, %d', [CktElementName, Nterms, Nconds]));
                NValues := NConds * Nterms;
                for i := 1 to NValues do
                begin
                    S := Cmul(Vterminal^[i], Conjg(Iterminal^[i]));
                    FSWrite(F, Format(', %10.6g, %10.6g', [S.re * 0.001, S.im * 0.001]));
                end;
                FSWriteln(F);
            end
    end;


end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportElemPowers(FileNm: String);

{ Export conductor powers in same order as NodeOrder export
}
var
    F: TFileStream = nil;
    pElem: TDSSCktElement;
    strName: String;

begin


    try
        F := TFileStream.Create(FileNm, fmCreate);

     {Header Record}
        FSWrite(F, 'Element, Nterminals, Nconductors, P_1, Q_1, ...');
        FSWriteln(F);


     // Sources first
        pElem := ActiveCircuit.Sources.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemPowers(F, strName);
            end;
            pElem := ActiveCircuit.Sources.Next;
        end;


     // PDELEMENTS first
        pElem := ActiveCircuit.PDElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemPowers(F, strName);
            end;
            pElem := ActiveCircuit.PDElements.Next;
        end;

     // Faults
        pElem := ActiveCircuit.Faults.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemPowers(F, strName);
            end;
            pElem := ActiveCircuit.Faults.Next;
        end;

     // PCELEMENTS next
        pElem := ActiveCircuit.PCElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemPowers(F, strName);
            end;
            pElem := ActiveCircuit.PCElements.Next;
        end;

        GlobalResult := FileNm;


    finally
        FreeAndNil(F);

    end;

end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportPowers(FileNm: String; opt: Integer);

{Opt = 0: kVA
 opt = 1: MVA
 }

var
    F: TFileStream = nil;
    Nterm, j: Integer;
    PDElem: TPDElement;
    PCElem: TPCElement;
    S: Complex;
    Separator: String;
    sout: String;
begin


    try
        F := TFileStream.Create(FileNm, fmCreate);
        Separator := ', ';


        case Opt of
            1:
                FSWriteln(F, 'Element, Terminal, P(MW), Q(Mvar), P_Normal, Q_Normal, P_Emergency, Q_Emergency');
        else
            FSWriteln(F, 'Element, Terminal, P(kW), Q(kvar),  P_Normal, Q_Normal, P_Emergency, Q_Emergency');
        end;

     // PDELEMENTS first
        PDElem := ActiveCircuit.PDElements.First;

        while PDElem <> NIL do
        begin
            if (PDElem.Enabled) then
            begin
                Nterm := pDElem.Nterms;

                for j := 1 to NTerm do
                begin
                    WriteStr(sout, Pad('"' + PDelem.DSSClassName + '.' + UpperCase(PDElem.Name) + '"', 24), Separator, j: 3);
                    FSWrite(F, sout);
           //----PDElem.ActiveTerminalIdx := j;
                    S := PDElem.Power[j];
                    if Opt = 1 then
                        S := CmulReal(S, 0.001);
                    WriteStr(sout, Separator, S.re * 0.001: 11: 1);
                    FSWrite(F, sout);
                    WriteStr(sout, Separator, S.im * 0.001: 11: 1);
                    FSWrite(F, sout);
                    if j = 1 then
                    begin
             //----PDelem.ActiveTerminalIdx := 1;
                        S := PDElem.ExcesskVANorm[1];
                        if Opt = 1 then
                            S := CmulReal(S, 0.001);
                        WriteStr(sout, Separator, Abs(S.re): 11: 1);
                        FSWrite(F, sout);
                        WriteStr(sout, Separator, Abs(S.im): 11: 1);
                        FSWrite(F, sout);
                        S := PDElem.ExcesskVAEmerg[1];
                        if Opt = 1 then
                            S := CmulReal(S, 0.001);
                        WriteStr(sout, Separator, Abs(S.re): 11: 1);
                        FSWrite(F, sout);
                        WriteStr(sout, Separator, Abs(S.im): 11: 1);
                        FSWrite(F, sout);
                    end;
                    FSWriteln(F);
                end;
            end;
            PDElem := ActiveCircuit.PDElements.Next;
        end;

     // PCELEMENTS Next
        PCElem := ActiveCircuit.PCElements.First;

        while PCElem <> NIL do
        begin

            if (PCElem.Enabled) then
            begin
                Nterm := PCElem.Nterms;

                for j := 1 to NTerm do
                begin
                    FSWrite(F, Pad('"' + PCElem.DSSClassName + '.' + UpperCase(PCElem.Name) + '"', 24), Separator, Format('%3d', [j]));
           //----pcElem.ActiveTerminalIdx := j;
                    S := pCElem.Power[j];
                    if Opt = 1 then
                        S := CmulReal(S, 0.001);
                    
                    WriteStr(sout, Separator, S.re * 0.001: 11: 1);
                    FSWrite(F, sout);
                    WriteStr(sout, Separator, S.im * 0.001: 11: 1);
                    FSWrite(F, sout);
                    FSWriteln(F);

                end;
            end;
            PCElem := ActiveCircuit.PCElements.Next;
        end;

        GlobalResult := FileNm;

    finally
        FreeAndNil(F);

    end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportLosses(FileNm: String);

{Opt = 0: kVA
 opt = 1: MVA
 }

var
    F: TFileStream = nil;
    PDElem: TPDElement;
    S_total, S_Load, S_NoLoad: Complex;

begin


    try
        F := TFileStream.Create(FileNm, fmCreate);

        FSWriteln(F, 'Element,  Total(W), Total(var),  I2R(W), I2X(var), No-load(W), No-load(var)');
     // PDELEMENTS first
        PDElem := ActiveCircuit.PDElements.First;

        while PDElem <> NIL do
        begin
            if (PDElem.Enabled) then
            begin
                PDElem.GetLosses(S_total, S_Load, S_NoLoad);
                FSWriteln(F, Format('%s.%s, %.7g, %.7g, %.7g, %.7g, %.7g, %.7g', [PDElem.ParentClass.Name, UpperCase(PDElem.Name), S_total.re, S_total.im, S_Load.re, S_Load.im, S_NoLoad.re, S_NoLoad.im]));
            end;
            PDElem := ActiveCircuit.PDElements.Next;
        end;

        GlobalResult := FileNm;

    finally
        FreeAndNil(F);

    end;
end;

// ===============================================================================
procedure ExportPbyphase(FileNm: String; opt: Integer);

{ Export Powers by phase }

{Opt = 0: kVA
 opt = 1: MVA
 }

var
    F: TFileStream = nil;
    i: Integer;
    PDElem: TPDElement;
    PCElem: TPCElement;
    S: Complex;

begin


    try
        F := TFileStream.Create(FileNm, fmCreate);

        case Opt of
            1:
                FSWriteln(F, 'Element, NumTerminals, NumConductors, NumPhases, MW1, Mvar1, MW2, Mvar2, MW3, Mvar3, ... ');
        else
            FSWriteln(F, 'Element, NumTerminals, NumConductors, NumPhases, kW1, kvar1, kW2, kvar2, kW3, kvar3, ... ');
        end;

     // PDELEMENTS first
        PDElem := ActiveCircuit.PDElements.First;

        while PDElem <> NIL do
        begin
            if (PDElem.Enabled) then
            begin
                with PDElem do
                begin
                    ComputeITerminal;
                    ComputeVTerminal;
                    FSWrite(F, Format('"%s.%s", %d, %d, %d', [DSSClassName, Uppercase(Name), NTerms, NConds, Nphases]));
                    for i := 1 to Yorder do
                    begin
                        S := CmulReal(Cmul(Vterminal^[i], conjg(ITerminal^[i])), 0.001);
                        if Opt = 1 then
                            S := CmulReal(S, 0.001);   // convert to MVA
                        FSWrite(F, Format(', %10.3f, %10.3f', [S.re, S.im]));
                    end;
                end;
                FSWriteln(F);
            end;
            PDElem := ActiveCircuit.PDElements.Next;
        end;

     // PCELEMENTS Next
        PCElem := ActiveCircuit.PCElements.First;

        while PCElem <> NIL do
        begin

            if (PCElem.Enabled) then
            begin
                with PCelem do
                begin
                    ComputeITerminal;
                    ComputeVTerminal;
                    FSWrite(F, Format('"%s.%s", %d, %d, %d', [DSSClassName, Uppercase(Name), NTerms, NConds, NPhases]));
                    for i := 1 to Yorder do
                    begin
                        S := CmulReal(Cmul(Vterminal^[i], conjg(ITerminal^[i])), 0.001);
                        if Opt = 1 then
                            S := CmulReal(S, 0.001);   // convert to MVA
                        FSWrite(F, Format(', %10.3f, %10.3f', [S.re, S.im]));
                    end;
                end;
                FSWriteln(F);

            end;
            PCElem := ActiveCircuit.PCElements.Next;
        end;

        GlobalResult := FileNm;

    finally
        FreeAndNil(F);

    end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportSeqPowers(FileNm: String; opt: Integer);

{Opt = 0: kVA
 opt = 1: MVA
 }

var
    F: TFileStream = nil;
    cBuffer: pComplexArray;
    NCond, Nterm, i, j, k: Integer;
    PDElem: TPDElement;
    PCElem: TPCElement;
    Volts: Complex;
    S: Complex;
    nref: Integer;
    Vph, V012: array[1..3] of Complex;
    Iph, I012: array[1..3] of Complex;
    Separator: String;
    sout: String;

begin

    cBuffer := NIL;

    try
        F := TFileStream.Create(FileNm, fmCreate);
        Separator := ', ';

        Getmem(cBuffer, sizeof(cBuffer^[1]) * GetMaxCktElementSize);

        case Opt of
            1:
                FSWriteln(F, 'Element, Terminal, P1(MW), Q1(Mvar), P2, Q2, P0, Q0, P_Normal, Q_Normal, P_Emergency, Q_Emergency');
        else
            FSWriteln(F, 'Element, Terminal, P1(kW), Q1(kvar), P2, Q2, P0, Q0, P_Normal, Q_Normal, P_Emergency, Q_Emergency');
        end;

     // PDELEMENTS first
        PDElem := ActiveCircuit.PDElements.First;

        while PDElem <> NIL do
        begin
            if (PDElem.Enabled) then
            begin
                NCond := pDElem.NConds;
                Nterm := pDElem.Nterms;
                PDElem.GetCurrents(cBuffer);

                for j := 1 to NTerm do
                begin
                    FSWrite(F, Pad('"' + PDelem.DSSClassName + '.' + Uppercase(PDElem.Name) + '"', 24), Separator, Format('%3d', [j]));
                    for i := 1 to PDElem.NPhases do
                    begin
                        k := (j - 1) * Ncond + i;
                        nref := pDElem.NodeRef^[k];
                        Volts := ActiveCircuit.Solution.NodeV^[nref];
                        Iph[i] := cBuffer^[k];
                        Vph[i] := volts;
                    end;
                    if (PDElem.Nphases >= 3) then
                    begin
                        Phase2SymComp(@Iph, @I012);
                        Phase2SymComp(@Vph, @V012);
                    end
                    else
                    begin
                        V012[1] := CZERO;
                        I012[1] := CZERO;
                        V012[3] := CZERO;
                        I012[3] := CZERO;
                        if ActiveCircuit.PositiveSequence then
                        begin
                            V012[2] := Vph[1];
                            I012[2] := Iph[1];
                        end
                        else
                        begin
                            V012[2] := CZERO;
                            I012[2] := CZERO;
                        end;
                    end;

                    S := Cmul(V012[2], conjg(I012[2]));
                    if Opt = 1 then
                        S := CmulReal(S, 0.001);
                    
                    WriteStr(sout, Separator, S.re * 0.003: 11: 1);
                    FSWrite(F, sout);
                    WriteStr(sout, Separator, S.im * 0.003: 11: 1);
                    FSWrite(F, sout);
                    S := Cmul(V012[3], conjg(I012[3]));
                    if Opt = 1 then
                        S := CmulReal(S, 0.001);
                    WriteStr(sout, Separator, S.re * 0.003: 11: 1);
                    FSWrite(F, sout);
                    WriteStr(sout, Separator, S.im * 0.003: 11: 1);
                    FSWrite(F, sout);
                    S := Cmul(V012[1], conjg(I012[1]));
                    if Opt = 1 then
                        S := CmulReal(S, 0.001);
                    WriteStr(sout, Separator, S.re * 0.003: 8: 1);
                    FSWrite(F, sout);
                    WriteStr(sout, Separator, S.im * 0.003: 8: 1);
                    FSWrite(F, sout);

                    if j = 1 then
                    begin
                 //----PDelem.ActiveTerminalIdx := 1;
                        S := PDElem.ExcesskVANorm[1];
                        if Opt = 1 then
                            S := CmulReal(S, 0.001);
                        WriteStr(sout, Separator, Abs(S.re): 11: 1);
                        FSWrite(F, sout);
                        WriteStr(sout, Separator, Abs(S.im): 11: 1);
                        FSWrite(F, sout);
                        S := PDElem.ExcesskVAEmerg[1];
                        if Opt = 1 then
                            S := CmulReal(S, 0.001);
                        WriteStr(sout, Separator, Abs(S.re): 11: 1);
                        FSWrite(F, sout);
                        WriteStr(sout, Separator, Abs(S.im): 11: 1);
                        FSWrite(F, sout);
                    end;
                    FSWriteln(F);

                end;
            end;
            PDElem := ActiveCircuit.PDElements.Next;
        end;

     // PCELEMENTS Next
        PCElem := ActiveCircuit.PCElements.First;

        while PCElem <> NIL do
        begin

            if (PCElem.Enabled) then
            begin
                NCond := PCElem.NConds;
                Nterm := PCElem.Nterms;
                PCElem.GetCurrents(cBuffer);

                for j := 1 to NTerm do
                begin
                    FSWrite(F, Pad('"' + PCElem.DSSClassName + '.' + Uppercase(PCElem.Name) + '"', 24), Separator, Format('%3d', [j]));
                    for i := 1 to PCElem.NPhases do
                    begin
                        k := (j - 1) * Ncond + i;
                        nref := PCElem.NodeRef^[k];
                        Volts := ActiveCircuit.Solution.NodeV^[nref];
                        Iph[i] := cBuffer^[k];
                        Vph[i] := volts;
                    end;
                    if (PCElem.Nphases >= 3) then
                    begin
                        Phase2SymComp(@Iph, @I012);
                        Phase2SymComp(@Vph, @V012);
                    end
                    else
                    begin
                        V012[1] := CZERO;
                        I012[1] := CZERO;
                        V012[3] := CZERO;
                        I012[3] := CZERO;
                        if ActiveCircuit.PositiveSequence then
                        begin
                            V012[2] := Vph[1];
                            I012[2] := Iph[1];
                        end
                        else
                        begin
                            V012[2] := CZERO;
                            I012[2] := CZERO;
                        end;
                    end;

                    S := Cmul(V012[2], conjg(I012[2]));
                    if Opt = 1 then
                        S := CmulReal(S, 0.001);
                    WriteStr(sout, Separator, S.re * 0.003: 11: 1);
                    FSWrite(F, sout);
                    WriteStr(sout, Separator, S.im * 0.003: 11: 1);
                    FSWrite(F, sout);
                    S := Cmul(V012[3], conjg(I012[3]));
                    if Opt = 1 then
                        S := CmulReal(S, 0.001);
                    WriteStr(sout, Separator, S.re * 0.003: 11: 1);
                    FSWrite(F, sout);
                    WriteStr(sout, Separator, S.im * 0.003: 11: 1);
                    FSWrite(F, sout);
                    S := Cmul(V012[1], conjg(I012[1]));
                    if Opt = 1 then
                        S := CmulReal(S, 0.001);
                    WriteStr(sout, Separator, S.re * 0.003: 8: 1);
                    FSWrite(F, sout);
                    WriteStr(sout, Separator, S.im * 0.003: 8: 1);
                    FSWrite(F, sout);

                    FSWriteln(F);

                end;
            end;
            PCElem := ActiveCircuit.PCElements.Next;
        end;

        GlobalResult := FileNm;

    finally
        if Assigned(cBuffer) then
            Freemem(CBuffer);
        FreeAndNil(F);

    end;
end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportFaultStudy(FileNm: String);

var
    i, iBus, iphs: Integer;
    YFault: Tcmatrix;
    Vfault: pComplexArray;  {Big temp array}
    F: TFileStream = nil;
    GFault: complex;
    Separator: String;
    MaxCurr,
    CurrMag: Double;

begin

    try

        F := TFileStream.Create(FileNm, fmCreate);

        Separator := ', ';

   { Set source voltage injection currents }
        with ActiveCircuit do
        begin
            with Solution do
            begin

     {All Phase Faults}
                FSWriteln(F, 'Bus,  3-Phase,  1-Phase,  L-L');
                for iBus := 1 to NumBuses do
           {Bus Norton Equivalent Current, Isc has been previously computed}
                    with Buses^[iBus] do
                    begin
                        FSWrite(F, Pad(Uppercase(BusList.Get(iBus)), 12));
                        MaxCurr := 0.0;
                        for i := 1 to NumNodesThisBus do
                        begin
                            if MaxCurr < Cabs(BusCurrent^[i]) then
                                MaxCurr := Cabs(BusCurrent^[i]);
                            ;
                        end;
                        FSWrite(F, Separator, Format('%-10g', [maxCurr]));

           {One Phase Faults}

   { Solve for Fault Injection Currents}

                        YFault := TcMatrix.CreateMatrix(NumNodesThisBus);
                        Getmem(VFault, Sizeof(Complex) * NumNodesThisBus);

             {Build YscTemp}

                        GFault := Cmplx(10000.0, 0.0);

                        MaxCurr := 0.0;

                        for iphs := 1 to NumNodesThisBus do
                        begin
                            YFault.CopyFrom(Ysc);
                            YFault.AddElement(iphs, iphs, GFault);

                   { Solve for Injection Currents}
                            YFault.Invert;
                            YFault.MvMult(VFault, BusCurrent);  {Gets voltage appearing at fault}

                            Currmag := Cabs(Cmul(VFault^[iphs], GFault));
                            if CurrMag > MaxCurr then
                                MaxCurr := Currmag;

                        end; {For iphase}
             {Now, Stuff it in the Css Array where it belongs}
                        FSWrite(F, Separator, Format('%-10g', [maxCurr]));

                        Freemem(VFault);
                        YFault.Free;

           {Node-Node Faults}

           {Bus Norton Equivalent Current, Isc has been previously computed}

                        YFault := TcMatrix.CreateMatrix(NumNodesThisBus);
                        Getmem(VFault, Sizeof(VFault^[1]) * NumNodesThisBus);

                        GFault := Cmplx(10000.0, 0.0);

                        MaxCurr := 0.0;

                        for iphs := 1 to NumNodesThisBus - 1 do
                        begin
                            YFault.CopyFrom(Ysc);
                            YFault.AddElement(iphs, iphs, GFault);
                            YFault.AddElement(iphs + 1, iphs + 1, GFault);
                            YFault.AddElemSym(iphs, iphs + 1, Cnegate(GFault));

                   { Solve for Injection Currents}
                            YFault.Invert;
                            YFault.MvMult(VFault, BusCurrent);  {Gets voltage appearing at fault}

                            CurrMag := Cabs(Cmul(Csub(VFault^[iphs], VFault^[iphs + 1]), GFault));
                            if CurrMag > MaxCurr then
                                MaxCurr := CurrMag;
                        end; {For iphase}
             {Now, Stuff it in the Css Array where it belongs}

                        FSWrite(F, Separator, Format('%-10g', [maxCurr]));

                        Freemem(VFault);
                        YFault.Free;

                        FSWriteln(F);
                    end;  {With bus}

            end; {With Solution}
        end; {With ActiveCircuit}

        GlobalResult := Filenm;

    finally

        FreeAndNil(F);

    end;
end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportEstimation(Filenm: String);

var
    F: TFileStream = nil;
    i: Integer;
    pEnergyMeterObj: TEnergyMeterObj;
    pSensorObj: TSensorObj;
    TempX: array[1..3] of Double; // temp number buffer

    procedure ZeroTempXArray;
    var
        ii: Integer;
    begin
        for ii := 1 to 3 do
            TempX[ii] := 0.0;
    end;


begin

    try
        F := TFileStream.Create(FileNm, fmCreate);   // clears file

          {Do the EnergyMeters first}
        FSWriteln(F, '"Energy Meters" ');
        FSWriteln(F, '"energyMeter", "I1 Target", "I2 Target", "I3 Target", "I1 Calc", "I2 Calc", "I3 Calc", "I1 %Err", "I2 %Err", "I3 %Err"'{, "I1 Factor", "I2 Factor", "I3 Factor"'});

        pEnergyMeterObj := ActiveCircuit.energyMeters.First;
        while pEnergyMeterObj <> NIL do
        begin
            if pEnergyMeterObj.Enabled then
            begin
                FSWrite(F, Format('"Energymeter.%s"', [Uppercase(pEnergyMeterObj.Name)]));
                  {Sensor currents (Target)}
                ZeroTempXArray;
                for i := 1 to pEnergyMeterObj.Nphases do
                    TempX[i] := pEnergyMeterObj.SensorCurrent^[i];
                for i := 1 to 3 do
                    FSWrite(F, Format(', %.6g', [TempX[i]]));
                  {Calculated Currents}
                ZeroTempXArray;
                for i := 1 to pEnergyMeterObj.Nphases do
                    TempX[i] := Cabs(pEnergyMeterObj.CalculatedCurrent^[i]);
                for i := 1 to 3 do
                    FSWrite(F, Format(', %.6g', [TempX[i]]));
                  {Percent Error}
                for i := 1 to pEnergyMeterObj.Nphases do
                    TempX[i] := (1.0 - TempX[i] / Max(0.001, pEnergyMeterObj.SensorCurrent^[i])) * 100.0;
                for i := 1 to 3 do
                    FSWrite(F, Format(', %.6g', [TempX[i]]));

                  (****  Not all that useful
                  {Allocation Factors}
                  ZeroTempXArray;
                  For i := 1 to pEnergyMeterObj.Nphases do TempX[i] := pEnergyMeterObj.PhsAllocationFactor^[i];
                  For i := 1 to 3 do FSWrite(F, Format(' %.6g,',[TempX[i]]));
                  *****)

                FSWriteln(F);
            end;
            pEnergyMeterObj := ActiveCircuit.EnergyMeters.Next;
        end;

          {Do the Sensors Next}
        FSWriteln(F);
        FSWriteln(F, '"Sensors" ');
        FSWrite(F, '"Sensor", "I1 Target", "I2 Target", "I3 Target", "I1 Calc", "I2 Calc", "I3 Calc", "I1 %Err", "I2 %Err", "I3 %Err",');
        FSWriteln(F, ' "V1 Target", "V2 Target", "V3 Target", "V1 Calc", "V2 Calc", "V3 Calc", "V1 %Err", "V2 %Err", "V3 %Err", "WLS Voltage Err", "WLS Current Err"');

        pSensorObj := ActiveCircuit.Sensors.First;
        while pSensorObj <> NIL do
        begin
            if pSensorObj.Enabled then
            begin
                FSWrite(F, Format('"Sensor.%s"', [Uppercase(pSensorObj.Name)]));
                  {Sensor currents (Target)}
                ZeroTempXArray;
                for i := 1 to pSensorObj.Nphases do
                    TempX[i] := pSensorObj.SensorCurrent^[i];
                for i := 1 to 3 do
                    FSWrite(F, Format(', %.6g', [TempX[i]]));
                  {Calculated Currents}
                ZeroTempXArray;
                for i := 1 to pSensorObj.Nphases do
                    TempX[i] := Cabs(pSensorObj.CalculatedCurrent^[i]);
                for i := 1 to 3 do
                    FSWrite(F, Format(', %.6g', [TempX[i]]));
                  {Percent Error}
                for i := 1 to pSensorObj.Nphases do
                    TempX[i] := (1.0 - TempX[i] / Max(0.001, pSensorObj.SensorCurrent^[i])) * 100.0;
                for i := 1 to 3 do
                    FSWrite(F, Format(', %.6g', [TempX[i]]));
                  {Sensor Voltage (Target)}
                ZeroTempXArray;
                for i := 1 to pSensorObj.Nphases do
                    TempX[i] := pSensorObj.SensorVoltage^[i];
                for i := 1 to 3 do
                    FSWrite(F, Format(', %.6g', [TempX[i]]));
                  {Calculated Voltage}
                ZeroTempXArray;
                for i := 1 to pSensorObj.Nphases do
                    TempX[i] := Cabs(pSensorObj.CalculatedVoltage^[i]);
                for i := 1 to 3 do
                    FSWrite(F, Format(', %.6g', [TempX[i]]));
                  {Percent Error}
                for i := 1 to pSensorObj.Nphases do
                    TempX[i] := (1.0 - TempX[i] / Max(0.001, pSensorObj.SensorVoltage^[i])) * 100.0;
                for i := 1 to 3 do
                    FSWrite(F, Format(', %.6g', [TempX[i]]));
                  {WLS Errors}
                ZeroTempXArray;
                FSWrite(F, Format(', %.6g, %.6g', [pSensorObj.WLSVoltageError, pSensorObj.WLSCurrentError]));

                FSWriteln(F);
            end;
            pSensorObj := ActiveCircuit.Sensors.Next;
        end;


    finally
        AppendGlobalResult(FileNm);
        FreeAndNil(F);

    end;


end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure WriteMultipleMeterFiles;

var
    F: TFileStream = nil;
    i, j: Integer;
    pElem: TEnergyMeterObj;
    MeterClass: TEnergyMeter;
    FileNm,
    Separator: String;

begin

    MeterClass := TEnergyMeter(GetDSSClassPtr('Energymeter'));
    if MeterClass = NIL then
        Exit;  // oops somewhere!!
    Separator := ', ';

    pElem := ActiveCircuit.energyMeters.First;
    while pElem <> NIL do
    begin
        if pElem.Enabled then
        begin
            try
                FileNm := GetOutputDirectory + 'EXP_MTR_' + Uppercase(pElem.Name) + '.CSV';

                if not FileExists(FileNm) then
                begin
                    F := TFileStream.Create(FileNm, fmCreate);
                {Write New Header}
                    FSWrite(F, 'Year, LDCurve, Hour, Meter');
                    for i := 1 to NumEMRegisters do
                        FSWrite(F, Separator, '"' + pelem.RegisterNames[i] + '"');
                    FSWriteln(F);
                    FreeAndNil(F);
                end;

                F := TFileStream.Create(FileNm, fmOpenReadWrite);
                F.Seek(0, soEnd);
                FSWrite(F, IntToStr(ActiveCircuit.Solution.Year), Separator);
                FSWrite(F, ActiveCircuit.LoadDurCurve, Separator);
                FSWrite(F, IntToStr(ActiveCircuit.Solution.DynaVars.intHour), Separator);
                FSWrite(F, Pad('"' + Uppercase(pElem.Name) + '"', 14));
                for j := 1 to NumEMRegisters do
                    FSWrite(F, Separator, Format('%-10.0g', [PElem.Registers[j]]));
                FSWriteln(F);
                AppendGlobalResult(FileNm);
            finally
                FreeAndNil(F);
            end;

        end;
        pElem := ActiveCircuit.EnergyMeters.Next;
    end;


end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure WriteSingleMeterFile(const FileNm: String);
var
    F: TFileStream = nil;
    i, j: Integer;
    pElem: TEnergyMeterObj;
    TestStr,
    Separator: String;
    RewriteFile: Boolean;
begin

    Separator := ', ';

    try

        if FileExists(FileNm) then
        begin  // See if it has already been written on
            F := TFileStream.Create(FileNm, fmOpenRead);
            
            if ((F.Position+1) < F.Size) then
            begin
                FSReadLn(F, TestStr);
             {See if it likely that the file is OK}
                if CompareText(Copy(TestStr, 1, 4), 'Year') = 0 then
                    RewriteFile := FALSE       // Assume the file is OK
                else
                    RewriteFile := TRUE;
            end
            else
                RewriteFile := TRUE;

            FreeAndNil(F);

        end
        else
        begin
            ReWriteFile := TRUE;
        end;

   {Either open or append the file}
        if RewriteFile then
        begin
            FreeAndNil(F);
            F := TFileStream.Create(FileNm, fmCreate);
        {Write New Header}
            pElem := ActiveCircuit.energyMeters.First;
            FSWrite(F, 'Year, LDCurve, Hour, Meter');
            for i := 1 to NumEMRegisters do
                FSWrite(F, Separator, '"' + pElem.RegisterNames[i] + '"');
            FSWriteln(F);
        end
        else
        begin
            FreeAndNil(F);
            F := TFileStream.Create(FileNm, fmOpenReadWrite); 
            F.Seek(0, soEnd);
        end;


        pElem := ActiveCircuit.energyMeters.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                FSWrite(F, IntToStr(ActiveCircuit.Solution.Year), Separator);
                FSWrite(F, ActiveCircuit.LoadDurCurve, Separator);
                FSWrite(F, IntToStr(ActiveCircuit.Solution.DynaVars.intHour), Separator);
                FSWrite(F, Pad('"' + Uppercase(pElem.Name) + '"', 14));
                for j := 1 to NumEMRegisters do
                    FSWrite(F, Separator, Format('%-10.0g', [PElem.Registers[j]]));
                FSWriteln(F);
            end;
            pElem := ActiveCircuit.EnergyMeters.Next;
        end;

        GlobalResult := FileNm;

    finally

        FreeAndNil(F);

    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportMeters(FileNm: String);

// Export Values of  Meter Elements

// These records are appended to an existing file so a running account is kept for some kinds of simulations

// If switch /m is specified, a separate file is created for each meter using the meter's name

begin


    if Lowercase(Copy(FileNm, 1, 2)) = '/m' then
        WriteMultipleMeterFiles
    else
        WriteSingleMeterFile(FileNM);
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure WriteMultipleGenMeterFiles;

var
    F: TFileStream = nil;
    i, j: Integer;
    pElem: TGeneratorObj;
    GeneratorClass: TGenerator;
    FileNm,
    Separator: String;

begin

    GeneratorClass := TGenerator(GetDSSClassPtr('generator'));
    if GeneratorClass = NIL then
        Exit;  // oops somewhere!!
    Separator := ', ';

    pElem := ActiveCircuit.Generators.First;
    while pElem <> NIL do
    begin
        if pElem.Enabled then
        begin
            try
                FileNm := GetOutputDirectory + 'EXP_GEN_' + Uppercase(pElem.Name) + '.CSV';

                if not FileExists(FileNm) then
                begin
                    F := TFileStream.Create(FileNm, fmCreate);
                {Write New Header}
                    FSWrite(F, 'Year, LDCurve, Hour, Generator');
                    for i := 1 to NumGenRegisters do
                        FSWrite(F, Separator, '"' + GeneratorClass.RegisterNames[i] + '"');
                    FSWriteln(F);
                    FreeAndNil(F);
                end;

                F := TFileStream.Create(FileNm, fmOpenReadWrite);
                F.Seek(0, soEnd);
                with ActiveCircuit do
                begin
                    FSWrite(F, IntToStr(Solution.Year), Separator);
                    FSWrite(F, LoadDurCurve, Separator);
                    FSWrite(F, IntToStr(Solution.DynaVars.intHour), Separator);
                    FSWrite(F, Pad('"' + Uppercase(pElem.Name) + '"', 14));
                    for j := 1 to NumGenRegisters do
                        FSWrite(F, Separator, Format('%-10.0g', [PElem.Registers[j]]));
                    FSWriteln(F);
                end;
                AppendGlobalResult(FileNm);
            finally
                FreeAndNil(F);
            end;

        end;
        pElem := ActiveCircuit.Generators.Next;
    end;

end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure WriteSingleGenMeterFile(FileNm: String);

var
    F: TFileStream = nil;
    i, j: Integer;
    pElem: TGeneratorObj;
    GeneratorClass: TGenerator;
    Separator, TestStr: String;
    ReWriteFile: Boolean;

begin


    GeneratorClass := TGenerator(GetDSSClassPtr('generator'));
    if GeneratorClass = NIL then
        Exit;  // oops somewhere!!
    Separator := ', ';


    try

        if FileExists(FileNm) then
        begin  // See if it has already been written on
            F := TFileStream.Create(FileNm, fmOpenRead);
            if ((F.Position+1) < F.Size) then
            begin
                FSReadLn(F, TestStr);
             {See if it likely that the file is OK}
                if CompareText(Copy(TestStr, 1, 4), 'Year') = 0 then
                    RewriteFile := FALSE       // Assume the file is OK
                else
                    RewriteFile := TRUE;
            end
            else
                RewriteFile := TRUE;

            FreeAndNil(F);

        end
        else
        begin
            ReWriteFile := TRUE;
        end;

   {Either open or append the file}
        if RewriteFile then
        begin
            FreeAndNil(F);
            F := TFileStream.Create(FileNm, fmCreate); 
        {Write New Header}
            FSWrite(F, 'Year, LDCurve, Hour, Generator');
            for i := 1 to NumGenRegisters do
                FSWrite(F, Separator, '"' + GeneratorClass.RegisterNames[i] + '"');
            FSWriteln(F);
        end
        else
        begin
            FreeAndNil(F);
            F := TFileStream.Create(FileNm, fmOpenReadWrite); 
            F.Seek(0, soEnd);
        end;


        pElem := ActiveCircuit.Generators.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
                with ActiveCircuit do
                begin
                    FSWrite(F, IntToStr(Solution.Year), Separator);
                    FSWrite(F, LoadDurCurve, Separator);
                    FSWrite(F, IntToStr(Solution.DynaVars.intHour), Separator);
                    FSWrite(F, Pad('"' + Uppercase(pElem.Name) + '"', 14));
                    for j := 1 to NumGenRegisters do
                        FSWrite(F, Separator, Format('%-10.0g', [PElem.Registers[j]]));                        
                    FSWriteln(F);
                end;

            pElem := ActiveCircuit.Generators.Next;
        end;

        GlobalResult := FileNm;

    finally

        FreeAndNil(F);

    end;


end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure WriteMultiplePVSystem2MeterFiles; forward;

procedure WriteMultiplePVSystemMeterFiles;

var
    F: TFileStream = nil;
    i, j: Integer;
    pElem: TPVSystemObj;
    FileNm,
    Separator: String;

begin
    if not DSS_CAPI_LEGACY_MODELS then
    begin
        WriteMultiplePVSystem2MeterFiles();
        Exit;
    end;

    if PVSystemClass = NIL then
        Exit;  // oops somewhere!!
    Separator := ', ';

    pElem := ActiveCircuit.PVSystems.First;
    while pElem <> NIL do
    begin
        if pElem.Enabled then
        begin
            try
                FileNm := GetOutputDirectory + 'EXP_PV_' + Uppercase(pElem.Name) + '.CSV';

                if not FileExists(FileNm) then
                begin
                    F := TFileStream.Create(FileNm, fmCreate);
                {Write New Header}
                    FSWrite(F, 'Year, LDCurve, Hour, PVSystem');
                    for i := 1 to NumPVSystemRegisters do
                        FSWrite(F, Separator, '"' + PVSystemClass.RegisterNames[i] + '"');
                    FSWriteln(F);
                    FreeAndNil(F);
                end;

                F := TFileStream.Create(FileNm, fmOpenReadWrite);
                F.Seek(0, soEnd);
                with ActiveCircuit do
                begin
                    FSWrite(F, IntToStr(Solution.Year), Separator);
                    FSWrite(F, LoadDurCurve, Separator);
                    FSWrite(F, IntToStr(Solution.DynaVars.intHour), Separator);
                    FSWrite(F, Pad('"' + Uppercase(pElem.Name) + '"', 14));
                    for j := 1 to NumPVSystemRegisters do
                        FSWrite(F, Separator, Format('%-10.0g', [PElem.Registers[j]]));
                    FSWriteln(F);
                end;
                AppendGlobalResult(FileNm);
            finally
                FreeAndNil(F);
            end;

        end;
        pElem := ActiveCircuit.PVSystems.Next;
    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure WriteMultiplePVSystem2MeterFiles;

var
    F: TFileStream = nil;
    i, j: Integer;
    pElem: TPVSystem2Obj;
    FileNm,
    Separator: String;

begin

    if PVSystem2Class = NIL then
        Exit;  // oops somewhere!!
    Separator := ', ';

    pElem := ActiveCircuit.PVSystems.First;
    while pElem <> NIL do
    begin
        if pElem.Enabled then
        begin
            try
                FileNm := GetOutputDirectory + 'EXP_PV_' + Uppercase(pElem.Name) + '.CSV';

                if not FileExists(FileNm) then
                begin
                    F := TFileStream.Create(FileNm, fmCreate);
                {Write New Header}
                    FSWrite(F, 'Year, LDCurve, Hour, PVSystem');
                    for i := 1 to NumPVSystem2Registers do
                        FSWrite(F, Separator, '"' + PVSystem2Class.RegisterNames[i] + '"');
                    FSWriteln(F);
                    FreeAndNil(F);
                end;

                F := TFileStream.Create(FileNm, fmOpenReadWrite);
                F.Seek(0, soEnd);
                with ActiveCircuit do
                begin
                    FSWrite(F, IntToStr(Solution.Year), Separator);
                    FSWrite(F, LoadDurCurve, Separator);
                    FSWrite(F, IntToStr(Solution.DynaVars.intHour), Separator);
                    FSWrite(F, Pad('"' + Uppercase(pElem.Name) + '"', 14));
                    for j := 1 to NumPVSystem2Registers do
                        FSWrite(F, Separator, Format('%-10.0g', [PElem.Registers[j]]));
                    FSWriteln(F);
                end;
                AppendGlobalResult(FileNm);
            finally
                FreeAndNil(F);
            end;

        end;
        pElem := ActiveCircuit.PVSystems.Next;
    end;

end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure WriteSinglePVSystem2MeterFile(FileNm: String); forward;

procedure WriteSinglePVSystemMeterFile(FileNm: String);

var
    F: TFileStream = nil;
    i, j: Integer;
    pElem: TPVSystemObj;
    Separator, TestStr: String;
    ReWriteFile: Boolean;

begin
    if not DSS_CAPI_LEGACY_MODELS then
    begin
        WriteSinglePVSystem2MeterFile(FileNm);
        Exit;
    end;


    if PVSystemClass = NIL then
        Exit;  // oops somewhere!!
    Separator := ', ';


    try

        if FileExists(FileNm) then
        begin  // See if it has already been written on
            F := TFileStream.Create(FileNm, fmOpenRead);
            if ((F.Position+1) < F.Size) then
            begin
                FSReadLn(F, TestStr);
             {See if it likely that the file is OK}
                if CompareText(Copy(TestStr, 1, 4), 'Year') = 0 then
                    RewriteFile := FALSE       // Assume the file is OK
                else
                    RewriteFile := TRUE;
            end
            else
                RewriteFile := TRUE;

            FreeAndNil(F);

        end
        else
        begin
            ReWriteFile := TRUE;
        end;

   {Either open or append the file}
        if RewriteFile then
        begin
            FreeAndNil(F);
            F := TFileStream.Create(FileNm, fmCreate); 
        {Write New Header}
            FSWrite(F, 'Year, LDCurve, Hour, PVSystem');
            for i := 1 to NumGenRegisters do
                FSWrite(F, Separator, '"' + PVSystemClass.RegisterNames[i] + '"');
            FSWriteln(F);
        end
        else
        begin
            FreeAndNil(F);
            F := TFileStream.Create(FileNm, fmOpenReadWrite); 
            F.Seek(0, soEnd);
        end;


        pElem := ActiveCircuit.PVSystems.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
                with ActiveCircuit do
                begin
                    FSWrite(F, IntToStr(Solution.Year), Separator);
                    FSWrite(F, LoadDurCurve, Separator);
                    FSWrite(F, IntToStr(Solution.DynaVars.intHour), Separator);
                    FSWrite(F, Pad('"' + Uppercase(pElem.Name) + '"', 14));
                    for j := 1 to NumPVSystemRegisters do
                        FSWrite(F, Separator, Format('%-10.0g', [PElem.Registers[j]]));
                    FSWriteln(F);
                end;

            pElem := ActiveCircuit.PVSystems.Next;
        end;

        GlobalResult := FileNm;

    finally

        FreeAndNil(F);

    end;


end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure WriteSinglePVSystem2MeterFile(FileNm: String);

var
    F: TFileStream = nil;
    i, j: Integer;
    pElem: TPVSystem2Obj;
    Separator, TestStr: String;
    ReWriteFile: Boolean;

begin


    if PVSystem2Class = NIL then
        Exit;  // oops somewhere!!
    Separator := ', ';


    try

        if FileExists(FileNm) then
        begin  // See if it has already been written on
            F := TFileStream.Create(FileNm, fmOpenRead);
            if ((F.Position+1) < F.Size) then
            begin
                FSReadLn(F, TestStr);
             {See if it likely that the file is OK}
                if CompareText(Copy(TestStr, 1, 4), 'Year') = 0 then
                    RewriteFile := FALSE       // Assume the file is OK
                else
                    RewriteFile := TRUE;
            end
            else
                RewriteFile := TRUE;

            FreeAndNil(F);
        end
        else
        begin
            ReWriteFile := TRUE;
        end;

   {Either open or append the file}
        if RewriteFile then
        begin
            FreeAndNil(F);
            F := TFileStream.Create(FileNm, fmCreate); 
        {Write New Header}
            FSWrite(F, 'Year, LDCurve, Hour, PVSystem');
            for i := 1 to NumGenRegisters do
                FSWrite(F, Separator, '"' + PVSystem2Class.RegisterNames[i] + '"');
            FSWriteln(F);
        end
        else
        begin
            FreeAndNil(F);
            F := TFileStream.Create(FileNm, fmOpenReadWrite); 
            F.Seek(0, soEnd);
        end;


        pElem := ActiveCircuit.PVSystems.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
                with ActiveCircuit do
                begin
                    FSWrite(F, IntToStr(Solution.Year), Separator);
                    FSWrite(F, LoadDurCurve, Separator);
                    FSWrite(F, IntToStr(Solution.DynaVars.intHour), Separator);
                    FSWrite(F, Pad('"' + Uppercase(pElem.Name) + '"', 14));
                    for j := 1 to NumPVSystem2Registers do
                        FSWrite(F, Separator, Format('%-10.0g', [PElem.Registers[j]]));
                    FSWriteln(F);
                end;

            pElem := ActiveCircuit.PVSystems.Next;
        end;

        GlobalResult := FileNm;

    finally

        FreeAndNil(F);

    end;


end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure WriteMultipleStorage2MeterFiles; forward;

procedure WriteMultipleStorageMeterFiles;

var
    F: TFileStream = nil;
    i, j: Integer;
    pElem: TStorageObj;
    FileNm,
    Separator: String;

begin
    if not DSS_CAPI_LEGACY_MODELS then
    begin
        WriteMultipleStorage2MeterFiles;
        Exit;
    end;

    if StorageClass = NIL then
        Exit;  // oops somewhere!!
    Separator := ', ';

    pElem := ActiveCircuit.StorageElements.First;
    while pElem <> NIL do
    begin
        if pElem.Enabled then
        begin
            try
                FileNm := GetOutputDirectory + 'EXP_PV_' + Uppercase(pElem.Name) + '.CSV';

                if not FileExists(FileNm) then
                begin
                    F := TFileStream.Create(FileNm, fmCreate);
                {Write New Header}
                    FSWrite(F, 'Year, LDCurve, Hour, Storage');
                    for i := 1 to NumStorageRegisters do
                        FSWrite(F, Separator, '"' + StorageClass.RegisterNames[i] + '"');
                    FSWriteln(F);
                    FreeAndNil(F);
                end;

                F := TFileStream.Create(FileNm, fmOpenReadWrite);
                F.Seek(0, soEnd);
                with ActiveCircuit do
                begin
                    FSWrite(F, IntToStr(Solution.Year), Separator);
                    FSWrite(F, LoadDurCurve, Separator);
                    FSWrite(F, IntToStr(Solution.DynaVars.intHour), Separator);
                    FSWrite(F, Pad('"' + Uppercase(pElem.Name) + '"', 14));
                    for j := 1 to NumStorageRegisters do
                        FSWrite(F, Separator, Format('%-10.0g', [PElem.Registers[j]]));
                    FSWriteln(F);
                end;
                AppendGlobalResult(FileNm);
            finally
                FreeAndNil(F);
            end;

        end;
        pElem := ActiveCircuit.StorageElements.Next;
    end;

end;

procedure WriteMultipleStorage2MeterFiles;

var
    F: TFileStream = nil;
    i, j: Integer;
    pElem: TStorage2Obj;
    FileNm,
    Separator: String;

begin

    if Storage2Class = NIL then
        Exit;  // oops somewhere!!
    Separator := ', ';

    pElem := ActiveCircuit.StorageElements.First;
    while pElem <> NIL do
    begin
        if pElem.Enabled then
        begin
            try
                FileNm := GetOutputDirectory + 'EXP_PV_' + Uppercase(pElem.Name) + '.CSV';

                if not FileExists(FileNm) then
                begin
                    F := TFileStream.Create(FileNm, fmCreate);
                {Write New Header}
                    FSWrite(F, 'Year, LDCurve, Hour, Storage');
                    for i := 1 to NumStorage2Registers do
                        FSWrite(F, Separator, '"' + Storage2Class.RegisterNames[i] + '"');
                    FSWriteln(F);
                    FreeAndNil(F);
                end;

                F := TFileStream.Create(FileNm, fmOpenReadWrite);
                F.Seek(0, soEnd);
                with ActiveCircuit do
                begin
                    FSWrite(F, IntToStr(Solution.Year), Separator);
                    FSWrite(F, LoadDurCurve, Separator);
                    FSWrite(F, IntToStr(Solution.DynaVars.intHour), Separator);
                    FSWrite(F, Pad('"' + Uppercase(pElem.Name) + '"', 14));
                    for j := 1 to NumStorage2Registers do
                        FSWrite(F, Separator, Format('%-10.0g', [PElem.Registers[j]]));
                    FSWriteln(F);
                end;
                AppendGlobalResult(FileNm);
            finally
                FreeAndNil(F);
            end;

        end;
        pElem := ActiveCircuit.StorageElements.Next;
    end;

end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure WriteSingleStorage2MeterFile(FileNm: String); forward;

procedure WriteSingleStorageMeterFile(FileNm: String);

var
    F: TFileStream = nil;
    i, j: Integer;
    pElem: TStorageObj;
    Separator, TestStr: String;
    ReWriteFile: Boolean;

begin
    if not DSS_CAPI_LEGACY_MODELS then
    begin
        WriteSingleStorage2MeterFile(FileNm);
        Exit;
    end;

    if StorageClass = NIL then
        Exit;  // oops somewhere!!
    Separator := ', ';


    try

        if FileExists(FileNm) then
        begin  // See if it has already been written on
            F := TFileStream.Create(FileNm, fmOpenRead);
            if ((F.Position+1) < F.Size) then
            begin
                FSReadLn(F, TestStr);
             {See if it likely that the file is OK}
                if CompareText(Copy(TestStr, 1, 4), 'Year') = 0 then
                    RewriteFile := FALSE       // Assume the file is OK
                else
                    RewriteFile := TRUE;
            end
            else
                RewriteFile := TRUE;

            FreeAndNil(F);

        end
        else
        begin
            ReWriteFile := TRUE;
        end;

   {Either open or append the file}
        if RewriteFile then
        begin
            FreeAndNil(F);
            F := TFileStream.Create(FileNm, fmCreate); 
        {Write New Header}
            FSWrite(F, 'Year, LDCurve, Hour, Storage');
            for i := 1 to NumStorageRegisters do
                FSWrite(F, Separator, '"' + StorageClass.RegisterNames[i] + '"');
            FSWriteln(F);
        end
        else
        begin
            FreeAndNil(F);
            F := TFileStream.Create(FileNm, fmOpenReadWrite); 
            F.Seek(0, soEnd);
        end;

        pElem := ActiveCircuit.StorageElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
                with ActiveCircuit do
                begin
                    FSWrite(F, IntToStr(Solution.Year), Separator);
                    FSWrite(F, LoadDurCurve, Separator);
                    FSWrite(F, IntToStr(Solution.DynaVars.intHour), Separator);
                    FSWrite(F, Pad('"' + Uppercase(pElem.Name) + '"', 14));
                    for j := 1 to NumStorageRegisters do
                        FSWrite(F, Separator, Format('%-10.0g', [PElem.Registers[j]]));
                    FSWriteln(F);
                end;

            pElem := ActiveCircuit.StorageElements.Next;
        end;

        GlobalResult := FileNm;

    finally

        FreeAndNil(F);

    end;


end;


procedure WriteSingleStorage2MeterFile(FileNm: String);

var
    F: TFileStream = nil;
    i, j: Integer;
    pElem: TStorage2Obj;
    Separator, TestStr: String;
    ReWriteFile: Boolean;

begin


    if StorageClass = NIL then
        Exit;  // oops somewhere!!
    Separator := ', ';


    try

        if FileExists(FileNm) then
        begin  // See if it has already been written on
            F := TFileStream.Create(FileNm, fmOpenRead);
            if ((F.Position+1) < F.Size) then
            begin
                FSReadLn(F, TestStr);
             {See if it likely that the file is OK}
                if CompareText(Copy(TestStr, 1, 4), 'Year') = 0 then
                    RewriteFile := FALSE       // Assume the file is OK
                else
                    RewriteFile := TRUE;
            end
            else
                RewriteFile := TRUE;

            FreeAndNil(F);

        end
        else
        begin
            ReWriteFile := TRUE;
        end;

   {Either open or append the file}
        if RewriteFile then
        begin
            FreeAndNil(F);
            F := TFileStream.Create(FileNm, fmCreate);
        {Write New Header}
            FSWrite(F, 'Year, LDCurve, Hour, Storage');
            for i := 1 to NumStorage2Registers do
                FSWrite(F, Separator, '"' + Storage2Class.RegisterNames[i] + '"');
            FSWriteln(F);
        end
        else
        begin
            FreeAndNil(F);
            F := TFileStream.Create(FileNm, fmOpenReadWrite);
            F.Seek(0, soEnd);
        end;


        pElem := ActiveCircuit.StorageElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
                with ActiveCircuit do
                begin
                    FSWrite(F, IntToStr(Solution.Year), Separator);
                    FSWrite(F, LoadDurCurve, Separator);
                    FSWrite(F, IntToStr(Solution.DynaVars.intHour), Separator);
                    FSWrite(F, Pad('"' + Uppercase(pElem.Name) + '"', 14));
                    for j := 1 to NumStorage2Registers do
                        FSWrite(F, Separator, Format('%-10.0g', [PElem.Registers[j]]));
                    FSWriteln(F);
                end;

            pElem := ActiveCircuit.StorageElements.Next;
        end;

        GlobalResult := FileNm;

    finally

        FreeAndNil(F);

    end;


end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportGenMeters(FileNm: String);

// Export Values of Generator Meter Elements
// If switch /m is specified, a separate file is created for each generator using the generator's name

begin


    if Lowercase(Copy(FileNm, 1, 2)) = '/m' then
        WriteMultipleGenMeterFiles
    else
        WriteSingleGenMeterFile(FileNM);

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportPVSystemMeters(FileNm: String);

// Export Values of Generator Meter Elements
// If switch /m is specified, a separate file is created for each generator using the generator's name

begin


    if Lowercase(Copy(FileNm, 1, 2)) = '/m' then
        WriteMultiplePVSystemMeterFiles
    else
        WriteSinglePVSystemMeterFile(FileNM);

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportPVSystem2Meters(FileNm: String);

// Export Values of Generator Meter Elements
// If switch /m is specified, a separate file is created for each generator using the generator's name

begin


    if Lowercase(Copy(FileNm, 1, 2)) = '/m' then
        WriteMultiplePVSystem2MeterFiles
    else
        WriteSinglePVSystem2MeterFile(FileNM);

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportStorageMeters(FileNm: String);

// Export Values of Generator Meter Elements
// If switch /m is specified, a separate file is created for each generator using the generator's name

begin


    if Lowercase(Copy(FileNm, 1, 2)) = '/m' then
        WriteMultipleStorageMeterFiles
    else
        WriteSingleStorageMeterFile(FileNM);

end;

procedure ExportStorage2Meters(FileNm: String);

// Export Values of Generator Meter Elements
// If switch /m is specified, a separate file is created for each generator using the generator's name

begin


    if Lowercase(Copy(FileNm, 1, 2)) = '/m' then
        WriteMultipleStorage2MeterFiles
    else
        WriteSingleStorage2MeterFile(FileNM);

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportLoads(FileNm: String);

// Export Loads to view present allocation


var
    F: TFileStream = nil;
    pElem: TLoadObj;
    Separator: String;
    sout: String;
begin

    Separator := ', ';


    try

        F := TFileStream.Create(FileNm, fmCreate);
     {Write  Header}
        FSWriteln(F, 'Load, Connected KVA, Allocation Factor, Phases, kW, kvar, PF, Model');

        pElem := ActiveCircuit.Loads.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
                with pElem do
                begin
                    WriteStr(sout, 
                        Uppercase(Name),
                        Separator, ConnectedkVA: 8: 1,
                        Separator, kVAAllocationFactor: 5: 3,
                        Separator, NPhases: 0,
                        Separator, kWBase: 8: 1,
                        Separator, kvarBase: 8: 1,
                        Separator, PFNominal: 5: 3,
                        Separator, FLoadModel: 0
                    );
                    FSWrite(F, sout);
                end;
            FSWriteln(F);
            pElem := ActiveCircuit.Loads.Next;
        end;

        GlobalResult := FileNm;

    finally

        FreeAndNil(F);

    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportCapacity(FileNm: String);

{
 Similar to export currents except does only max of the phases and compares that
 to the Normamps and Emergamps rating
}

var
    F: TFileStream = nil;
    cBuffer: pComplexArray;
    pElem: TPDElement;

begin

    cBuffer := NIL;

    try
        F := TFileStream.Create(FileNm, fmCreate);

        Getmem(cBuffer, sizeof(Complex) * GetMaxCktElementSize);

        FSWriteln(F, 'Name, Imax, %normal, %emergency, kW, kvar, NumCustomers, TotalCustomers, NumPhases, kVBase');

     // PDELEMENTS ONLY
        pElem := ActiveCircuit.PDElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                pElem.GetCurrents(cBuffer);
                CalcAndWriteMaxCurrents(F, pElem, Cbuffer);
            end;
            pElem := ActiveCircuit.PDElements.Next;
        end;

        GlobalResult := FileNm;

    finally
        if Assigned(cBuffer) then
            Freemem(cBuffer);
        FreeAndNil(F);

    end;


end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportOverloads(FileNm: String);

var
    F: TFileStream = nil;
    cBuffer: pComplexArray;  // Allocate to max total conductors
    NCond,
    i,
    j: Integer;
    PDElem: TPDElement;
    Iph,
    I012: array[1..3] of Complex;
    I0, I1, I2,
    iNormal,
    iEmerg, Cmax: Double;
    Separator: String;
    Spower: Double;
    sout: String;
begin

    cBuffer := NIL;

    try
        F := TFileStream.Create(FileNm, fmCreate);

    {Allocate cBuffer big enough for largest circuit element}
        Getmem(cbuffer, sizeof(cBuffer^[1]) * GetMaxCktElementSize);

     {Sequence Currents}
        FSWriteln(F, 'Element, Terminal,  I1, AmpsOver, kVAOver, %Normal, %Emergency, I2, %I2/I1, I0, %I0/I1');

        Separator := ', ';

     // PDELEMENTS Only
        PDelem := ActiveCircuit.PDElements.First;

        while PDelem <> NIL do
        begin
            if (PDelem.Enabled) then
                if (CLASSMASK and PDElem.DSSObjType) <> CAP_ELEMENT    // ignore caps
                then
                begin
                    NCond := PDelem.NConds;
                    PDelem.GetCurrents(cBuffer);

                    for j := 1 to 1 do       // only for terminal 1
                    begin
                        Cmax := 0.0;
                        for i := 1 to Min(PDelem.Nphases, 3) do
                        begin   // Check only first 3 phases
                            Iph[i] := cBuffer^[(j - 1) * Ncond + i];
                            Cmax := max(Cmax, Cabs(Iph[i]));
                        end;
                        if (PDelem.Nphases >= 3) then
                        begin   // Report Symmetrical Component Currents for
                            Phase2SymComp(@Iph, @I012);
                            I0 := Cabs(I012[1]);   // Get abs values to report
                            I1 := Cabs(I012[2]);
                            I2 := Cabs(I012[3]);
                        end
                        else
                        begin   // Other than 3-phase
                            I0 := 0.0;
                            I1 := Cabs(Iph[1]);    // Ambiguous: Report only first phase
                            I2 := 0.0;
                            Cmax := I1;
                        end;

                        if (PdElem.Normamps > 0.0) or (PdElem.Emergamps > 0.0) then
                            if (CMax > PDElem.NormAmps) or (Cmax > pdelem.EmergAmps) then
                            begin
               // Get terminal 1 power
                                Spower := Cabs(PDElem.Power[1]) * 0.001;   // kW

                                FSWrite(F, Format('%s, %d, ', [Pad(('"' + pDelem.DSSClassName + '.' + Uppercase(pDelem.Name) + '"'), 22), j]));
                                FSWrite(F, Format('%8.2f, ', [I1]));
                                if j = 1 then
                                begin // Only for 1st Terminal
                                    iNormal := PDelem.NormAmps;
                                    if iNormal > 0.0 then
                                    begin
                                        FSWrite(F, Format('%8.2f, %10.2f', [(Cmax - iNormal), Spower * (Cmax - iNormal) / iNormal]));
                                        WriteStr(sout, Separator, Cmax / iNormal * 100.0: 8: 1);
                                        FSWrite(F, sout);
                                    end
                                    else
                                        FSWrite(F, Separator, '     0.0');
                                    iEmerg := PDelem.EmergAmps;
                                    if iEmerg > 0.0 then
                                    begin
                                        WriteStr(sout, Separator, Cmax / iEmerg * 100.0: 8: 1);
                                        FSWrite(F, sout)
                                    end
                                    else
                                        FSWrite(F, Separator, '     0.0');
                                end
                                else
                                    FSWrite(F, Separator + '       0' + Separator + '       0');

                                WriteStr(sout, Separator, I2: 8: 1);
                                FSWrite(F, sout);
                                if I1 > 0.0 then
                                begin
                                    WriteStr(sout, Separator, 100.0 * I2 / I1: 8: 1);
                                    FSWrite(F, sout)
                                end
                                else
                                    FSWrite(F, Separator, '0.0');
                                    
                                WriteStr(sout, Separator, I0: 8: 1);
                                FSWrite(F, sout);
                                if I1 > 0.0 then
                                begin
                                    WriteStr(sout, Separator, 100.0 * I0 / I1: 8: 1);
                                    FSWrite(F, sout)
                                end
                                else
                                    FSWrite(F, Separator, '0.0');

                                FSWriteln(F);
                            end;

                    end;
                end;
            PDelem := ActiveCircuit.PDElements.Next;
        end;

        GlobalResult := FileNm;


    finally
        if Assigned(Cbuffer) then
            Freemem(cBuffer);
        FreeAndNil(F);

    end;
end;

procedure ExportUnserved(FileNm: String; UE_Only: Boolean);

var
    F: TFileStream = nil;
    PLoad: TLoadObj;
    DoIt: Boolean;
    sout: String;
begin

    try
        F := TFileStream.Create(FileNm, fmCreate);

        FSWriteln(F, 'Load, Bus, kW, EEN_Factor,  UE_Factor');

     // Load
        pLoad := ActiveCircuit.Loads.First;
        while pLoad <> NIL do
        begin
            if (pLoad.Enabled) then
            begin
                DoIt := FALSE;
                if UE_Only then
                begin
                    if pLoad.Unserved then
                        DoIt := TRUE;
                end
                else
                if pLoad.ExceedsNormal then
                    DoIt := TRUE;

                if DoIt then
                begin
                    WriteStr(sout, 
                        Uppercase(pLoad.Name), ', ',
                        pLoad.GetBus(1), ', ',
                        pLoad.kWBase: 8: 0, ', ',
                        pLoad.EEN_Factor: 9: 3, ', ',
                        pLoad.UE_Factor: 9: 3
                    );
                    FSWrite(F, sout);
                    FSWriteln(F);
                end;

            end;
            pLoad := ActiveCircuit.Loads.Next;
        end;

        GlobalResult := FileNm;

    finally

        FreeAndNil(F);

    end;

end;

procedure ExportYprim(FileNm: String);

{Exports  YPrim matrices for all  Circuit Elements}

var
    F: TFileStream = nil;
    i, j, k: Integer;
    cValues: pComplexArray;

begin

    if ActiveCircuit = NIL then
        Exit;

    try
        F := TFileStream.Create(FileNm, fmCreate);

        with ActiveCircuit do
        begin
            for k := 1 to NumDevices do
            begin
                ActiveCktElement := CktElements.Get(k);
                if ActiveCktElement.Enabled then
                begin
                    if (ActiveCktElement is TPDElement) or (ActiveCktElement is TPCElement) then
                        with ActiveCktElement do
                        begin
                            FSWriteln(F, ParentClass.Name, '.', Uppercase(Name));
                            cValues := GetYprimValues(ALL_YPRIM);
                            for i := 1 to Yorder do
                            begin
                                for j := 1 to Yorder do
                                    FSWrite(F, Format('%-13.10g, %-13.10g, ', [cValues^[i + (j - 1) * Yorder].re, cValues^[i + (j - 1) * Yorder].im]));
                                FSWriteln(F);
                            end;
                        end;
                end;
            end;
        end;


        GlobalResult := FileNm;

    finally

        FreeAndNil(F);

    end;

end;

// illustrate retrieval of System Y using compressed column format
procedure ExportY(FileNm: String; TripletOpt: Boolean);

{Exports System Y Matrix in Node Order}

var
    F: TFileStream = nil;
    i, j, p: Longword;
    col, row: Longword;
    hY: NativeUInt;
    nNZ, nBus: Longword;
    ColPtr, RowIdx: array of Longword;
    cVals: array of Complex;
    re, im: Double;

begin

    if ActiveCircuit = NIL then
        Exit;
    hY := ActiveCircuit.Solution.hY;
    if hY <= 0 then
    begin
        DoSimpleMsg('Y Matrix not Built.', 222);
        Exit;
    end;
  // this compresses the entries if necessary - no extra work if already solved
    FactorSparseMatrix(hY);
    GetNNZ(hY, @nNZ);
    GetSize(hY, @nBus); // we should already know this

    try
        F := TFileStream.Create(FileNm, fmCreate);

        if TripletOpt then
        begin
            SetLength(ColPtr, nNZ);
            SetLength(RowIdx, nNZ);
            SetLength(cVals, nNZ);
            GetTripletMatrix(hY, nNZ, @RowIdx[0], @ColPtr[0], @cVals[0]);
            FSWriteln(F, 'Row,Col,G,B');
            for i := 0 to nNZ - 1 do
            begin
                col := ColPtr[i] + 1;
                row := RowIdx[i] + 1;
                if row >= col then
                begin
                    re := cVals[i].re;
                    im := cVals[i].im;
                    FSWriteln(F, Format('%d,%d,%.10g,%.10g', [row, col, re, im]));
                end;
            end;
        end
        else
        begin
            SetLength(ColPtr, nBus + 1);
            SetLength(RowIdx, nNZ);
            SetLength(cVals, nNZ);
            GetCompressedMatrix(hY, nBus + 1, nNZ, @ColPtr[0], @RowIdx[0], @cVals[0]);
       {Write out fully qualified Bus Names}
            with ActiveCircuit do
            begin
                FSWriteln(F, Format('%d, ', [NumNodes]));
  (*        For i := 1 to NumNodes DO BEGIN
             j :=  MapNodeToBus^[i].BusRef;
             FSWrite(F, Format('%s.%-d, +j,',[BusList.Get(j), MapNodeToBus^[i].NodeNum]));
          END;
          FSWriteln(F);
  *)
                for i := 1 to NumNodes do
                begin
                    j := MapNodeToBus^[i].BusRef;
                    FSWrite(F, Format('"%s.%-d", ', [Uppercase(BusList.Get(j)), MapNodeToBus^[i].NodeNum]));
                    for j := 1 to NumNodes do
                    begin
                        re := 0.0;
                        im := 0.0;
                // search for a non-zero element [i,j]
                //  DSS indices are 1-based, KLU indices are 0-based
                        for p := ColPtr[j - 1] to ColPtr[j] - 1 do
                        begin
                            if RowIdx[p] + 1 = i then
                            begin
                                re := cVals[p].re;
                                im := cVals[p].im;
                            end;
                        end;
                        FSWrite(F, Format('%-13.10g, +j %-13.10g,', [re, im]));
                    end;
                    FSWriteln(F);
                end;
            end;
        end;

        GlobalResult := FileNm;
    finally
        FreeAndNil(F);
    end;
end;

procedure ExportSeqZ(FileNm: String);

// Export Symmetrical Component Impedances at each bus

var
    F: TFileStream = nil;
    i: Integer;
    Z1, Z0: Complex;
    X1R1, X0R0: Double;


begin

    try
        F := TFileStream.Create(FileNm, fmCreate);

        FSWriteln(F, 'Bus,  NumNodes, R1, X1, R0, X0, Z1, Z0, "X1/R1", "X0/R0"');
        with ActiveCircuit do
        begin
            for i := 1 to NumBuses do
            begin

                Z1 := Buses^[i].Zsc1;
                Z0 := Buses^[i].Zsc0;
                if Z1.re <> 0.0 then
                    X1R1 := Z1.im / Z1.re
                else
                    X1R1 := 1000.0;
                if Z0.re <> 0.0 then
                    X0R0 := Z0.im / Z0.re
                else
                    X0R0 := 1000.0;

                FSWriteln(F,
                    Format('"%s", %d, %10.6g, %10.6g, %10.6g, %10.6g, %10.6g, %10.6g, %8.4g, %8.4g',
                    [Uppercase(BusList.Get(i)), Buses^[i].NumNodesThisBus,
                    Z1.re, Z1.im, Z0.Re, Z0.im, Cabs(Z1), Cabs(Z0), X1R1, X0R0]
                    ));

            end;
        end;


        GlobalResult := FileNm;

    finally

        FreeAndNil(F);
    end;

end;

procedure ExportUuids(FileNm: String);
var
    F: TFileStream = nil;
    clsLnCd: TLineCode;
    clsGeom: TLineGeometry;
    clsWire: TWireData;
    clsXfCd: TXfmrCode;
    clsSpac: TLineSpacing;
    clsTape: TTSData;
    clsConc: TCNData;
    pName: TNamedObject;
    i: integer;
begin
    try
        clsLnCd := DSSClassList.Get(ClassNames.Find('linecode'));
        clsWire := DSSClassList.Get(ClassNames.Find('wiredata'));
        clsGeom := DSSClassList.Get(ClassNames.Find('linegeometry'));
        clsXfCd := DSSClassList.Get(ClassNames.Find('xfmrcode'));
        clsSpac := DSSClassList.Get(ClassNames.Find('linespacing'));
        clsTape := DSSClassList.Get(ClassNames.Find('TSData'));
        clsConc := DSSClassList.Get(ClassNames.Find('CNData'));
    
        F := TFileStream.Create(FileNm, fmCreate);

        pName := ActiveCircuit;
        FSWriteln(F, Format('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));
 
        for i :=1 to ActiveCircuit.NumBuses do 
        begin
            pName := ActiveCircuit.Buses^[i];
            FSWriteln(F, Format ('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));
        end;
    
        pName := ActiveCircuit.CktElements.First;
        while pName <> NIL do
        begin
            FSWriteln(F, Format('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));
            pName := ActiveCircuit.CktElements.Next;
        end;

        pName := clsLnCd.ElementList.First;
        while pName <> NIL do
        begin
            FSWriteln(F, Format('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));
            pName := clsLnCd.ElementList.Next;
        end;

        pName := clsWire.ElementList.First;
        while pName <> NIL do
        begin
            FSWriteln(F, Format('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));
            pName := clsWire.ElementList.Next;
        end;

        pName := clsGeom.ElementList.First;
        while pName <> NIL do
        begin
            FSWriteln(F, Format('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));
            pName := clsGeom.ElementList.Next;
        end;

        pName := clsXfCd.ElementList.First;
        while pName <> nil do 
        begin
            FSWriteln(F, Format ('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));
            pName := clsXfCd.ElementList.Next;
        end;
        
        pName := clsSpac.ElementList.First;
        while pName <> NIL do begin
            FSWriteln(F, Format ('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));
            pName := clsSpac.ElementList.Next;
        end;
        
        pName := clsTape.ElementList.First;
        while pName <> NIL do 
        begin
            FSWriteln(F, Format ('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));
            pName := clsTape.ElementList.Next;
        end;

        pName := clsConc.ElementList.First;
        while pName <> NIL do
        begin
            FSWriteln(F, Format('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));
            pName := clsConc.ElementList.Next;
        end;

        WriteHashedUUIDs (F);

    finally
        FreeAndNil(F);
        FreeUuidList;
    end;
end;

procedure ExportCounts(FileNm: String);
var
    F: TFileStream = nil;
    cls: TDSSClass;
begin
    try
        F := TFileStream.Create(FileNm, fmCreate);
        FSWriteln(F, 'Format: DSS Class Name = Instance Count');
        FSWriteln(F);
        cls := DSSClassList.First;
        while cls <> NIL do
        begin
            FSWriteln(F, Format('%s = %d', [cls.Name, cls.ElementCount]));
            cls := DSSClassList.Next;
        end;
    finally
        FreeAndNil(F);
    end;
end;

procedure ExportSummary(FileNm: String);
var
    F: TFileStream = nil;
    cPower, cLosses: Complex;

begin
    try

        if FileExists(FileNm) then
        begin
            F := TFileStream.Create(FileNm, fmOpenReadWrite); 
            F.Seek(0, soEnd);
        end
        else
        begin    // Create and write the header
            F := TFileStream.Create(FileNm, fmCreate); 
            FSWrite(F, 'DateTime, CaseName, ');
            FSWrite(F, 'Status, Mode, Number, LoadMult, NumDevices, NumBuses, NumNodes');
            FSWrite(F, ', Iterations, ControlMode, ControlIterations');
            FSWrite(F, ', MostIterationsDone');
            if ActiveCircuit <> NIL then
                if ActiveCircuit.Issolved and not ActiveCircuit.BusNameRedefined then
                begin
                    FSWrite(F, ', Year, Hour, MaxPuVoltage, MinPuVoltage, TotalMW, TotalMvar');
                    FSWrite(F, ', MWLosses, pctLosses, MvarLosses, Frequency');
                end;

            FSWriteln(F);
        end;

        FSWrite(F, Format('"%s", ', [DateTimeToStr(Now)]));
        if ActiveCircuit <> NIL then
            FSWrite(F, Format('%s, ', [ActiveCircuit.CaseName]))
        else
            FSWrite(F, 'NONE, ');

        if ActiveCircuit.Issolved then
            FSWrite(F, 'SOLVED')
        else
            FSWrite(F, 'UnSolved');

        FSWrite(F, Format(', %s', [GetSolutionModeID]));
        FSWrite(F, Format(', %d', [ActiveCircuit.Solution.NumberofTimes]));
        FSWrite(F, Format(', %8.3f', [ActiveCircuit.LoadMultiplier]));
        FSWrite(F, Format(', %d', [ActiveCircuit.NumDevices]));
        FSWrite(F, Format(', %d', [ActiveCircuit.NumBuses]));
        FSWrite(F, Format(', %d', [ActiveCircuit.NumNodes]));
        FSWrite(F, Format(', %d', [ActiveCircuit.Solution.Iteration]));
        FSWrite(F, Format(', %s', [GetControlModeID]));
        FSWrite(F, Format(', %d', [ActiveCircuit.Solution.ControlIteration]));
        FSWrite(F, Format(', %d', [ActiveCircuit.Solution.MostIterationsDone]));
        if ActiveCircuit <> NIL then
            if ActiveCircuit.Issolved and not ActiveCircuit.BusNameRedefined then
            begin
                FSWrite(F, Format(', %d', [ActiveCircuit.Solution.Year]));
                FSWrite(F, Format(', %d', [ActiveCircuit.Solution.DynaVars.intHour]));
                FSWrite(F, Format(', %-.5g', [GetMaxPUVoltage]));
                FSWrite(F, Format(', %-.5g', [GetMinPUVoltage(TRUE)]));
                cPower := CmulReal(GetTotalPowerFromSources, 0.000001);  // MVA
                FSWrite(F, Format(', %-.6g', [cPower.re]));
                FSWrite(F, Format(', %-.6g', [cPower.im]));
                cLosses := CmulReal(ActiveCircuit.Losses, 0.000001);
                if cPower.re <> 0.0 then
                    FSWrite(F, Format(', %-.6g, %-.4g', [cLosses.re, (Closses.re / cPower.re * 100.0)]))
                else
                    FSWrite(F, 'Total Active Losses:   ****** MW, (**** %%)');
                FSWrite(F, Format(', %-.6g', [cLosses.im]));
                FSWrite(F, Format(', %-g', [ActiveCircuit.Solution.Frequency]));
            end;

        FSWriteln(F);

        GlobalResult := FileNm;

    finally
        FreeAndNil(F);
    end;
end;

procedure ExportBusCoords(FileNm: String);
// Export bus x, y coordinates

var
    F: TFileStream = nil;
    i: Integer;


begin

    try
        F := TFileStream.Create(FileNm, fmCreate);

        with ActiveCircuit do
            for i := 1 to NumBuses do
            begin
                if Buses^[i].CoordDefined then
                    FSWriteln(F, Format('%s, %-13.11g, %-13.11g', [CheckForBlanks(Uppercase(BusList.Get(i))), Buses^[i].X, Buses^[i].Y]));
            end;

        GlobalResult := FileNm;

    finally

        FreeAndNil(F);
    end;
end;

procedure WriteNewLine(F: TFileStream;
    const CktELementName: String; DistFromMeter1, puV1, DistFromMeter2, puV2: Double;
    ColorCode, Thickness, LineType: Integer;
    MarkCenter: Integer;
    CenterMarkerCode, NodeMarkerCode, NodeMarkerWidth: Integer);

begin
    FSWrite(F, Format('%s, %.6g, %.6g, %.6g, %.6g,', [Uppercase(CktElementName), DistFromMeter1, puV1, DistFromMeter2, puV2]));
    FSWrite(F, Format('%d, %d, %d, ', [ColorCode, Thickness, LineType]));
    FSWrite(F, Format('%d, ', [MarkCenter]));
    FSWrite(F, Format('%d, %d, %d', [CenterMarkerCode, NodeMarkerCode, NodeMarkerWidth]));
    FSWriteln(F);
end;


procedure ExportProfile(FileNm: String; PhasesToPlot: Integer);

var
    iEnergyMeter: Integer;
    ActiveEnergyMeter: TEnergyMeterObj;
    PresentCktElement: TDSSCktElement;
    Bus1, Bus2: TDSSbus;
    puV1, puV2: Double;
    iphs: Integer;
    iphs2: Integer;
    S: String;
    F: TFileStream = nil;
    Linetype: Integer;

begin

    try
        F := TFileStream.Create(FileNm, fmCreate);

        FSWrite(F, 'Name, Distance1, puV1, Distance2, puV2, Color, Thickness, Linetype, Markcenter, Centercode, NodeCode, NodeWidth,');

    {New graph created before this routine is entered}
        case phasesToPlot of
            PROFILELL, PROFILELLALL, PROFILELLPRI:
                S := 'L-L Voltage Profile';
        else
            S := 'L-N Voltage Profile';
        end;

        FSWriteln(F, 'Title=', S, ', Distance in km');

        iEnergyMeter := EnergyMeterClass.First;
        while iEnergyMeter > 0 do
        begin

            ActiveEnergyMeter := EnergyMeterClass.GetActiveObj;
          {Go down each branch list and draw a line}
            PresentCktElement := ActiveEnergyMeter.BranchList.First;
            while PresentCktElement <> NIL do
            begin
                if IslineElement(PresentCktElement) then
                    with ActiveCircuit do
                    begin
                        Bus1 := Buses^[PresentCktElement.Terminals[0].BusRef];
                        Bus2 := Buses^[PresentCktElement.Terminals[1].BusRef];
            {Now determin which phase to plot}
                        if (Bus1.kVBase > 0.0) and (Bus2.kVBase > 0.0) then
                            case PhasesToPlot of
                  {3ph only}
                                PROFILE3PH:
                                    if (PresentCktElement.NPhases >= 3) and (Bus1.kVBase > 1.0) then
                                        for iphs := 1 to 3 do
                                        begin
                                            puV1 := CABS(Solution.NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))]) / Bus1.kVBase / 1000.0;
                                            puV2 := CABS(Solution.NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))]) / Bus2.kVBase / 1000.0;
                                            WriteNewLine(F, PresentCktElement.Name, Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                                                iphs, 2, 0, 0, 0, NodeMarkerCode, NodeMarkerWidth);
                                        end;
                  {Plot all phases present (between 1 and 3)}
                                PROFILEALL:
                                begin
                                    for iphs := 1 to 3 do
                                        if (Bus1.FindIdx(Iphs) > 0) and (Bus2.FindIdx(Iphs) > 0) then
                                        begin
                                            if Bus1.kVBase < 1.0 then
                                                Linetype := 2
                                            else
                                                Linetype := 0;
                                            puV1 := CABS(Solution.NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))]) / Bus1.kVBase / 1000.0;
                                            puV2 := CABS(Solution.NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))]) / Bus2.kVBase / 1000.0;
                                            WriteNewLine(F, PresentCktElement.Name, Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                                                iphs, 2, Linetype, 0, 0, NodeMarkerCode, NodeMarkerWidth);
                                        end;
                                end;
                  {Plot all phases present (between 1 and 3) for Primary only}
                                PROFILEALLPRI:
                                begin
                                    if Bus1.kVBase > 1.0 then
                                        for iphs := 1 to 3 do
                                            if (Bus1.FindIdx(Iphs) > 0) and (Bus2.FindIdx(Iphs) > 0) then
                                            begin
                                                if Bus1.kVBase < 1.0 then
                                                    Linetype := 2
                                                else
                                                    Linetype := 0;
                                                puV1 := CABS(Solution.NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))]) / Bus1.kVBase / 1000.0;
                                                puV2 := CABS(Solution.NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))]) / Bus2.kVBase / 1000.0;
                                                WriteNewLine(F, PresentCktElement.Name, Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                                                    iphs, 2, Linetype, 0, 0, NodeMarkerCode, NodeMarkerWidth);
                                            end;
                                end;
                                PROFILELL:
                                begin
                                    if (PresentCktElement.NPhases >= 3) then
                                        for iphs := 1 to 3 do
                                        begin
                                            iphs2 := iphs + 1;
                                            if iphs2 > 3 then
                                                iphs2 := 1;
                                            if (Bus1.FindIdx(Iphs) > 0) and (Bus2.FindIdx(Iphs) > 0) and
                                                (Bus1.FindIdx(Iphs2) > 0) and (Bus2.FindIdx(Iphs2) > 0) then
                                            begin
                                                if Bus1.kVBase < 1.0 then
                                                    Linetype := 2
                                                else
                                                    Linetype := 0;
                                                with Solution do
                                                begin
                                                    puV1 := CABS(CSUB(NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))], NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs2))])) / Bus1.kVBase / 1732.0;
                                                    puV2 := CABS(CSUB(NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))], NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs2))])) / Bus2.kVBase / 1732.0;
                                                end;
                                                WriteNewLine(F, PresentCktElement.Name, Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                                                    iphs, 2, Linetype, 0, 0, NodeMarkerCode, NodeMarkerWidth);
                                            end;
                                        end;
                                end;
                                PROFILELLALL:
                                begin
                                    for iphs := 1 to 3 do
                                    begin
                                        iphs2 := iphs + 1;
                                        if iphs2 > 3 then
                                            iphs2 := 1;
                                        if (Bus1.FindIdx(Iphs) > 0) and (Bus2.FindIdx(Iphs) > 0) and
                                            (Bus1.FindIdx(Iphs2) > 0) and (Bus2.FindIdx(Iphs2) > 0) then
                                        begin
                                            if Bus1.kVBase < 1.0 then
                                                Linetype := 2
                                            else
                                                Linetype := 0;
                                            with Solution do
                                            begin
                                                puV1 := CABS(CSUB(NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))], NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs2))])) / Bus1.kVBase / 1732.0;
                                                puV2 := CABS(CSUB(NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))], NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs2))])) / Bus2.kVBase / 1732.0;
                                            end;
                                            WriteNewLine(F, PresentCktElement.Name, Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                                                iphs, 2, Linetype, 0, 0, NodeMarkerCode, NodeMarkerWidth);
                                        end;
                                    end;
                                end;
                                PROFILELLPRI:
                                begin
                                    if Bus1.kVBase > 1.0 then
                                        for iphs := 1 to 3 do
                                        begin
                                            iphs2 := iphs + 1;
                                            if iphs2 > 3 then
                                                iphs2 := 1;
                                            if (Bus1.FindIdx(Iphs) > 0) and (Bus2.FindIdx(Iphs) > 0) and
                                                (Bus1.FindIdx(Iphs2) > 0) and (Bus2.FindIdx(Iphs2) > 0) then
                                            begin
                                                if Bus1.kVBase < 1.0 then
                                                    Linetype := 2
                                                else
                                                    Linetype := 0;
                                                with Solution do
                                                begin
                                                    puV1 := CABS(CSUB(NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))], NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs2))])) / Bus1.kVBase / 1732.0;
                                                    puV2 := CABS(CSUB(NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))], NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs2))])) / Bus2.kVBase / 1732.0;
                                                end;
                                                WriteNewLine(F, PresentCktElement.Name, Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                                                    iphs, 2, Linetype, 0, 0, NodeMarkerCode, NodeMarkerWidth);
                                            end;
                                        end;
                                end;
                            else     // plot just the selected phase
                                iphs := PhasesToPlot;
                                if (Bus1.FindIdx(Iphs) > 0) and (Bus2.FindIdx(Iphs) > 0) then
                                begin
                                    if Bus1.kVBase < 1.0 then
                                        Linetype := 2
                                    else
                                        Linetype := 0;
                                    puV1 := CABS(ActiveCircuit.Solution.NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))]) / Bus1.kVBase / 1000.0;
                                    puV2 := CABS(ActiveCircuit.Solution.NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))]) / Bus2.kVBase / 1000.0;
                                    WriteNewLine(F, PresentCktElement.Name, Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                                        iphs, 2, Linetype, 0, 0,
                                        NodeMarkerCode, NodeMarkerWidth);
                                end;

                            end;

                    end;

                PresentCktElement := ActiveEnergyMeter.BranchList.GoForward;
            end;

            iEnergyMeter := EnergyMeterClass.Next;
        end;

        GlobalResult := FileNm;

    finally

        FreeAndNil(F);

    end;

end;

procedure ExportEventLog(FileNm: String);
// Export the present set of EventStrings
begin
    EventStrings.SaveToFile(FileNm);
    GlobalResult := FileNm;
end;

procedure ExportErrorLog(FileNm: String);
// Export the present set of ErrorStrings
begin
    ErrorStrings.SaveToFile(FileNm);
    GlobalResult := FileNm;
end;

procedure ExportIncMatrix(FileNm: String);
var
    F: TFileStream = nil;
    i: Integer;
begin
    with ActiveCircuit.Solution do
    begin
        F := TFileStream.Create(FileNm, fmCreate);
        FSWriteln(F, 'Row,Col,Value');
        for i := 0 to (IncMat.NZero - 1) do
        begin
            FSWriteln(F, inttostr(IncMat.data[i][0]) + ',' + inttostr(IncMat.data[i][1]) + ',' + inttostr(IncMat.data[i][2]));
        end;
        GlobalResult := FileNm;
        FreeAndNil(F);
    end;
end;

procedure ExportIncMatrixRows(FileNm: String);
var
    F: TFileStream = nil;
    i: Integer;
begin
    with ActiveCircuit.Solution do
    begin
        F := TFileStream.Create(FileNm, fmCreate);
        FSWriteln(F, 'B2N Incidence Matrix Row Names (PDElements)');
        for i := 0 to (length(Inc_Mat_Rows) - 1) do
        begin
            FSWriteln(F, Inc_Mat_Rows[i]);
        end;
        GlobalResult := FileNm;
        FreeAndNil(F);
    end;
end;
//-------------------------------------------------------------------
procedure ExportIncMatrixCols(FileNm: String);
var
    F: TFileStream = nil;
    i: Integer;
begin
    with ActiveCircuit.Solution do
    begin
        F := TFileStream.Create(FileNm, fmCreate);
        FSWriteln(F, 'B2N Incidence Matrix Column Names (Buses)');
        for i := 0 to (length(Inc_Mat_Cols) - 1) do
        begin
            FSWriteln(F, Inc_Mat_Cols[i]);
        end;
        GlobalResult := FileNm;
        FreeAndNil(F);
    end;
end;
//-------------------------------------------------------------------
procedure ExportBusLevels(FileNm: String);
var
    F: TFileStream = nil;
    i: Integer;
begin
    with ActiveCircuit.Solution do
    begin
        F := TFileStream.Create(FileNm, fmCreate);
        FSWriteln(F, 'B2N Incidence Matrix Column Names (Buses) and their level within the matrix');
        FSWriteln(F, 'Bus Name,Bus Level');
        for i := 0 to (length(Inc_Mat_Cols) - 1) do
        begin
            FSWriteln(F, Inc_Mat_Cols[i] + ',' + inttostr(Inc_Mat_levels[i]));
        end;
        GlobalResult := FileNm;
        FreeAndNil(F);
    end;
end;
//-------------------------------------------------------------------
procedure ExportLaplacian(FileNm: String);
var
    F: TFileStream = nil;
    i: Integer;
begin
    with ActiveCircuit.Solution do
    begin
        F := TFileStream.Create(FileNm, fmCreate);
        FSWriteln(F, 'Row,Col,Value');
        for i := 0 to (Laplacian.NZero - 1) do
        begin
            FSWriteln(F, inttostr(Laplacian.data[i][0]) + ',' + inttostr(Laplacian.data[i][1]) + ',' + inttostr(Laplacian.data[i][2]));
        end;
        GlobalResult := FileNm;
        FreeAndNil(F);
    end;
end;
//-------------------------------------------------------------------
procedure ExportZLL(FileNm: String);
var
    F: TFileStream = nil;
    i: Integer;
begin
    if ADiakoptics then
    begin
        with ActiveCircuit, ActiveCircuit.Solution do
        begin
            F := TFileStream.Create(FileNm, fmCreate); 
            FSWriteln(F, 'Row,Col,Value(Real), Value(Imag)');
            for i := 0 to (ZLL.NZero - 1) do
            begin
                FSWriteln(F, inttostr(ZLL.CData[i].Row) + ',' + inttostr(ZLL.CData[i].Col) + ',' + floattostr(ZLL.CData[i].Value.Re) + ',' + floattostr(ZLL.CData[i].Value.Im));
            end;
            GlobalResult := FileNm;
            FreeAndNil(F);
        end;
    end;
end;
//-------------------------------------------------------------------
procedure ExportZCC(FileNm: String);
var
    F: TFileStream = nil;
    i: Integer;
begin
    if ADiakoptics then
    begin
        with ActiveCircuit, ActiveCircuit.Solution do
        begin
            F := TFileStream.Create(FileNm, fmCreate);
            FSWriteln(F, 'Row,Col,Value(Real), Value(Imag)');
            for i := 0 to (ZCC.NZero - 1) do
            begin
                FSWriteln(F, inttostr(ZCC.CData[i].Row) + ',' + inttostr(ZCC.CData[i].Col) + ',' + floattostr(ZCC.CData[i].Value.Re) + ',' + floattostr(ZCC.CData[i].Value.Im));
            end;
            GlobalResult := FileNm;
            FreeAndNil(F);
        end;
    end;
end;
//-------------------------------------------------------------------
procedure ExportY4(FileNm: String);
var
    F: TFileStream = nil;
    i: Integer;
begin
    if ADiakoptics then
    begin
        with ActiveCircuit, ActiveCircuit.Solution do
        begin
            F := TFileStream.Create(FileNm, fmCreate);
            FSWriteln(F, 'Row,Col,Value(Real), Value(Imag)');
            for i := 0 to (Y4.NZero - 1) do
            begin
                FSWriteln(F, inttostr(Y4.CData[i].Row) + ',' + inttostr(Y4.CData[i].Col) + ',' + floattostr(Y4.CData[i].Value.Re) + ',' + floattostr(Y4.CData[i].Value.Im));
            end;
            GlobalResult := FileNm;
            FreeAndNil(F);
        end;
    end;
end;
//-------------------------------------------------------------------
procedure ExportC(FileNm: String);
var
    F: TFileStream = nil;
    i: Integer;
begin
    if ADiakoptics then
    begin
        with ActiveCircuit, ActiveCircuit.Solution do
        begin
            F := TFileStream.Create(FileNm, fmCreate);
            FSWriteln(F, 'Row,Col,Value');
            for i := 0 to (Contours.NZero - 1) do
            begin
                FSWriteln(F, inttostr(Contours.CData[i].Row) + ',' + inttostr(Contours.CData[i].Col) + ',' + floattostr(Contours.CData[i].Value.Re));
            end;
            GlobalResult := FileNm;
            FreeAndNil(F);
        end;
    end;
end;
//-------------------------------------------------------------------
procedure ExportVoltagesElements(FileNm: String);

// Export element voltages, by terminal and node/bus

var
    MaxNumNodes: Integer;
    MaxNumTerminals: Integer;
    F: TFileStream = nil;
    i, j: Integer;
    pElem: TDSSCktElement;

begin

    MaxNumTerminals := 2;
    MaxNumNodes := 0;
    pElem := ActiveCircuit.CktElements.First;
    while pElem <> NIL do
    begin
        MaxNumTerminals := max(MaxNumTerminals, pElem.NTerms);
        MaxNumNodes := max(MaxNumNodes, pElem.NConds);
        pElem := ActiveCircuit.CktElements.Next;
    end;
{
    MaxNumNodes := 0;
    With ActiveCircuit Do
    For j := 1 to NumBuses Do
       MaxNumNodes := max(MaxNumNodes, Buses^[j].NumNodesThisBus);
}

    try
        F := TFileStream.Create(FileNm, fmCreate);

        FSWrite(F, 'Element,NumTerminals');

       //Write out the header
        for i := 1 to MaxNumTerminals do
        begin
            FSWrite(F, Format(', Terminal%d', [i]));
            FSWrite(F, ',NumConductors,NPhases,');
            FSWrite(F, 'Bus, BasekV');
            for j := 1 to MaxNumNodes do
                FSWrite(F, Format(', Node%d_%d, Magnitude%d_%d, Angle%d_%d, pu%d_%d', [i, j, i, j, i, j, i, j]));
        end;

        FSWriteln(F);

       //Go through all the sources
        with ActiveCircuit do
        begin
            pElem := sources.First;

            while pElem <> NIL do
            begin
                if pElem.Enabled then
                begin
                    WriteElementVoltagesExportFile(F, pElem, MaxNumNodes);
                    FSWriteln(F);
                end;
                pElem := ActiveCircuit.sources.Next;
            end;


       //Go through all the PDElements
            pElem := ActiveCircuit.PDElements.First;

            while pElem <> NIL do
            begin
                if pElem.Enabled then
                begin
                    WriteElementVoltagesExportFile(F, pElem, MaxNumNodes);
                    FSWriteln(F);
                end;
                pElem := ActiveCircuit.PDElements.Next;
            end;


     //Go through all the PCElements
            pElem := ActiveCircuit.PCElements.First;

            while pElem <> NIL do
            begin
                if pElem.Enabled then
                begin
                    WriteElementVoltagesExportFile(F, pElem, MaxNumNodes);
                    FSWriteln(F);
                end;
                pElem := ActiveCircuit.PCElements.Next;
            end;
        end;

        GlobalResult := FileNm;

    finally

        FreeAndNil(F);

    end;

end;
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure ExportGICMvar(FileNm: String);

var
    F: TFileStream = nil;
    pElem: TGICTransformerObj;
    GICClass: TGICTransformer;

begin


    try
        F := TFileStream.Create(FileNm, fmCreate);

        GICClass := TGICTransformer(GetDSSClassPtr('GICTransformer'));

        FSWriteln(F, 'Bus, Mvar, GIC Amps per phase');
        pElem := TGICTransformerObj(GICClass.ElementList.First);
        while PElem <> NIL do
        begin
            pElem.WriteVarOutputRecord(F);
            pElem := TGICTransformerObj(GICClass.ElementList.Next);
        end;

        GlobalResult := FileNm;

    finally
        FreeAndNil(F);
    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportBusReliability(FileNm: String);
var
    F: TFileStream = nil;
    i: Integer;

begin

    try
        F := TFileStream.Create(FileNm, fmCreate);
        FSWriteln(F, 'Bus, Lambda, Num-Interruptions, Num-Customers, Cust-Interruptions, Duration, Total-Miles');
        with ActiveCircuit do
            for i := 1 to NumBuses do
                with Buses^[i] do
                begin
                    FSWriteln(F, Format('%s, %-.11g, %-.11g, %d, %-.11g, %-.11g, %-.11g',
                        [CheckForBlanks(Uppercase(BusList.Get(i))), BusFltRate, Bus_Num_Interrupt, BusTotalNumCustomers, BusCustInterrupts, Bus_Int_Duration, BusTotalMiles]));
                end;

        GlobalResult := FileNm;

    finally

        FreeAndNil(F);
    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportBranchReliability(FileNm: String);
var
    F: TFileStream = nil;
    pElem: TPDElement;
    pBus: TDSSBus;
    SAIFI: Double;
    MaxCustomers: Integer;

begin

    try
        F := TFileStream.Create(FileNm, fmCreate);
        FSWriteln(F, 'Element, Lambda, "Accumulated-Lambda", Num-Customers, Total-Customers, Num-Interrupts, Cust-Interruptions, Cust-Durations, Total-Miles, Cust-Miles, SAIFI');
        with ActiveCircuit do
        begin

     // Find Maxcustomers of any PDElement for Duke Recloser siting algorithm
            MaxCustomers := 0;
            pElem := ActiveCircuit.PDElements.First;
            while pElem <> NIL do
            begin
                if pElem.Enabled then
                    with pElem do
                    begin
                        pBus := Buses^[Terminals[FromTerminal - 1].BusRef];
                        with pBus do
                            if BusTotalNumCustomers > MaxCustomers then
                                MaxCustomers := BusTotalNumCustomers;
                    end;
                pElem := ActiveCircuit.PDElements.Next;
            end;


     // write report for PDELEMENTS only
            pElem := ActiveCircuit.PDElements.First;
            while pElem <> NIL do
            begin
                if pElem.Enabled then
                    with pElem do
                    begin
                        pBus := Buses^[Terminals[FromTerminal - 1].BusRef];
                        with pBus do
                            if BusTotalNumCustomers > 0 then
                                SAIFI := BusCustInterrupts / BusTotalNumCustomers
                            else
                                SAIFI := 0.0;

                        FSWriteln(F, Format('%s.%s, %-.11g, %-.11g, %d, %d, %-.11g, %-.11g, %-.11g, %-.11g, %-.11g, %-.11g',
                            [ParentClass.Name, Name, BranchFltRate, AccumulatedBrFltRate, BranchNumCustomers, BranchTotalCustomers,
                            pBus.Bus_Num_Interrupt, BranchTotalCustomers * pBus.Bus_Num_Interrupt, pBus.BusCustDurations,
                            AccumulatedMilesDownStream, (MaxCustomers - BranchTotalCustomers) * AccumulatedMilesDownStream, SAIFI]));
                    end;
                pElem := ActiveCircuit.PDElements.Next;
            end;
        end;


        GlobalResult := FileNm;

    finally

        FreeAndNil(F);
    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportNodeNames(FileNm: String);
var
    F: TFileStream = nil;
    i: Integer;
    j: Integer;
    BusName: String;

begin

    try
        F := TFileStream.Create(FileNm, fmCreate);
        FSWriteln(F, 'Node_Name');
        with ActiveCircuit do
        begin

            for i := 1 to NumBuses do
            begin
                BusName := BusList.Get(i);
                with Buses^[i] do
                    for j := 1 to NumNodesThisBus do
                    begin
                        FSWriteln(F, Format('%s.%d ', [BusName, GetNum(j)]));
                    end;
            end;

        end;


        GlobalResult := FileNm;

    finally

        FreeAndNil(F);
    end;

end;
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

function TapPosition(const Transformer: TTransfObj; iWind: Integer): Integer;

{Assumes 0  is 1.0 per unit tap}

begin
    with Transformer do
        Result := Round((PresentTap[iWind] - (Maxtap[iWind] + Mintap[iWind]) / 2.0) / TapIncrement[iWind]);

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportTaps(FileNm: String);
var
    F: TFileStream = nil;
    iWind: Integer;
    pReg: TRegControlObj;
begin

    try

        F := TFileStream.Create(FileNm, fmCreate);
        FSWriteln(F, 'Name, Tap, Min, Max, Step, Position');

        with ActiveCircuit do
        begin
            pReg := RegControls.First;
            while pReg <> NIL do
            begin
                with pReg.Transformer do
                begin
                    iWind := pReg.TrWinding;
                    FSWrite(F, Name);
                    FSWriteln(F, Format(', %8.5f, %8.5f, %8.5f, %8.5f, %d', [PresentTap[iWind], MinTap[iWind], MaxTap[iWind], TapIncrement[iWind], TapPosition(pREg.Transformer, iWind)]));
                end;
                pReg := RegControls.Next;
            end;
        end;


        GlobalResult := FileNm;

    finally

        FreeAndNil(F);
    end;

end;

procedure ExportResult(FileNm: String);

var
    F: TFileStream = nil;

begin

    try
        F := TFileStream.Create(FileNm, fmCreate);
        Parservars.Lookup('@result');
        FSWriteln(F, Parservars.Value);

        GlobalResult := FileNm;

    finally

        FreeAndNil(F);
    end;


end;

procedure ExportYNodeList(FileNM: String);
var
    i: Integer;
    F: TFileStream = nil;

begin

    try
        F := TFileStream.Create(FileNm, fmCreate);

        if ActiveCircuit <> NIL then
            with ActiveCircuit do
            begin
                for i := 1 to NumNodes do
                begin
                    with MapNodeToBus^[i] do
                        FSWriteln(F, Format('"%s.%-d"', [Uppercase(BusList.Get(Busref)), NodeNum]));
                end;
            end;

        GlobalResult := FileNm;

    finally
        FreeAndNil(F);
    end;

end;

procedure ExportYVoltages(FileNM: String);
var
    i: Integer;
    F: TFileStream = nil;

begin

    try
        F := TFileStream.Create(FileNm, fmCreate);

        if ActiveCircuit <> NIL then
            with ActiveCircuit do
            begin
                for i := 1 to NumNodes do
                begin
                    with solution.NodeV^[i] do
                        FSWriteln(F, Format('%10.6g, %10.6g', [re, im]));
                end;
            end;

        GlobalResult := FileNm;

    finally
        FreeAndNil(F);
    end;


end;

procedure ExportYCurrents(FileNM: String);
var
    i: Integer;
    F: TFileStream = nil;

begin

    try
        F := TFileStream.Create(FileNm, fmCreate);

        if ActiveCircuit <> NIL then
            with ActiveCircuit do
            begin
                for i := 1 to NumNodes do
                begin
                    with solution.Currents^[i] do
                        FSWriteln(F, Format('%10.6g, %10.6g', [re, im]));
                end;
            end;

        GlobalResult := FileNm;

    finally
        FreeAndNil(F);
    end;

end;


procedure ExportSections(FileNM: String; pMeter: TEnergyMeterObj);

var
    MyMeterPtr: TEnergyMeterObj;
    iMeter, i: Integer;
    F: TFileStream = nil;


begin

    try
        F := TFileStream.Create(FileNm, fmCreate);

     // Write Header
        FSWriteln(F, 'Meter, SectionID, SeqIndex, DeviceType, NumCustomers, NumBranches, AvgRepairHrs, TotalDownlineCust, SectFaultRate, SumFltRatesXRepairHrs, SumBranchFltRates, HeadBranch ');

        if Assigned(pMeter) then
     // If a meter is specified, export that meter only
            with pMeter do
            begin
                for i := 1 to SectionCount do
                    with FeederSections^[i] do
                    begin
                        ActiveCircuit.ActiveCktElement := TDSSCktElement(sequenceList.Get(SeqIndex));
                        FSWriteln(F, Format('%s, %d, %d, %s, %d, %d, %-.6g, %d, %-.6g, %-.6g, %-.6g, %s',
                            [Name, i, SeqIndex, GetOCPDeviceTypeString(OCPDeviceType), NCustomers, NBranches, AverageRepairTime, TotalCustomers, SectFaultRate, SumFltRatesXRepairHrs, SumBranchFltRates,
                            FullName(ActiveCircuit.ActiveCktElement)]));
                    end;
            end
        else    // export sections for all meters
        begin

            iMeter := EnergyMeterClass.First;
            while iMeter > 0 do
            begin
                MyMeterPtr := EnergyMeterClass.GetActiveObj;
                with MyMeterPtr do
                begin
                    for i := 1 to SectionCount do
                        with FeederSections^[i] do
                        begin
                            ActiveCircuit.ActiveCktElement := TDSSCktElement(sequenceList.Get(SeqIndex));
                            FSWriteln(F, Format('%s, %d, %d, %s, %d, %d, %-.6g, %d, %-.6g, %-.6g, %-.6g, %s',
                                [Name, i, SeqIndex, GetOCPDeviceTypeString(OCPDeviceType), NCustomers, NBranches, AverageRepairTime, TotalCustomers, SectFaultRate, SumFltRatesXRepairHrs, SumBranchFltRates,
                                FullName(ActiveCircuit.ActiveCktElement)]));
                        end;
                end;
                iMeter := EnergyMeterClass.Next;
            end;

        end;

        GlobalResult := FileNm;

    finally
        FreeAndNil(F);
    end;

end;


end.
