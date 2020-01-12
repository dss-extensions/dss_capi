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
    DSSClass;

procedure ExportVoltages(DSS: TDSSContext; FileNm: String);
procedure ExportSeqVoltages(DSS: TDSSContext; FileNm: String);
procedure ExportCurrents(DSS: TDSSContext; FileNm: String);
procedure ExportEstimation(DSS: TDSSContext; Filenm: String);
procedure ExportSeqCurrents(DSS: TDSSContext; FileNm: String);
procedure ExportPowers(DSS: TDSSContext; FileNm: String; opt: Integer);
procedure ExportPbyphase(DSS: TDSSContext; FileNm: String; opt: Integer);
procedure ExportSeqPowers(DSS: TDSSContext; FileNm: String; opt: Integer);
procedure ExportFaultStudy(DSS: TDSSContext; FileNm: String);
procedure ExportMeters(DSS: TDSSContext; FileNm: String);
procedure ExportGenMeters(DSS: TDSSContext; FileNm: String);
procedure ExportPVSystemMeters(DSS: TDSSContext; FileNm: String);
procedure ExportStorageMeters(DSS: TDSSContext; FileNm: String);
procedure ExportLoads(DSS: TDSSContext; FileNm: String);
procedure ExportCapacity(DSS: TDSSContext; FileNm: String);
procedure ExportOverloads(DSS: TDSSContext; FileNm: String);
procedure ExportUnserved(DSS: TDSSContext; FileNm: String; UE_Only: Boolean);
procedure ExportYprim(DSS: TDSSContext; FileNm: String);
procedure ExportY(DSS: TDSSContext; FileNm: String; TripletOpt: Boolean);
procedure ExportSeqZ(DSS: TDSSContext; FileNm: String);
procedure ExportBusCoords(DSS: TDSSContext; FileNm: String);
procedure ExportLosses(DSS: TDSSContext; FileNm: String);
procedure ExportUuids(DSS: TDSSContext; FileNm: String);
procedure ExportCounts(DSS: TDSSContext; FileNm: String);
procedure ExportSummary(DSS: TDSSContext; FileNm: String);
procedure ExportProfile(DSS: TDSSContext; FileNm: String; PhasesToPlot: Integer);
procedure ExportEventLog(DSS: TDSSContext; FileNm: String);
procedure ExportVoltagesElements(DSS: TDSSContext; FileNm: String);
procedure ExportGICMvar(DSS: TDSSContext; FileNm: String);
procedure ExportBusReliability(DSS: TDSSContext; FileNm: String);
procedure ExportBranchReliability(DSS: TDSSContext; FileNm: String);
procedure ExportNodeNames(DSS: TDSSContext; FileNm: String);
procedure ExportTaps(DSS: TDSSContext; FileNm: String);
procedure ExportNodeOrder(DSS: TDSSContext; FileNm: String);
procedure ExportElemCurrents(DSS: TDSSContext; FileNm: String);
procedure ExportElemVoltages(DSS: TDSSContext; FileNm: String);
procedure ExportElemPowers(DSS: TDSSContext; FileNm: String);
procedure ExportResult(DSS: TDSSContext; FileNm: String);
procedure ExportYNodeList(DSS: TDSSContext; FileNM: String);
procedure ExportYVoltages(DSS: TDSSContext; FileNM: String);
procedure ExportYCurrents(DSS: TDSSContext; FileNM: String);
procedure ExportSections(DSS: TDSSContext; FileNM: String; pMeter: TEnergyMeterObj);
procedure ExportErrorLog(DSS: TDSSContext; FileNm: String);
procedure ExportIncMatrix(DSS: TDSSContext; FileNm: String);
procedure ExportIncMatrixRows(DSS: TDSSContext; FileNm: String);
procedure ExportIncMatrixCols(DSS: TDSSContext; FileNm: String);
procedure ExportBusLevels(DSS: TDSSContext; FileNm: String);
procedure ExportLaplacian(DSS: TDSSContext; FileNm: String);
{$IFDEF DSS_CAPI_PM}
procedure ExportZLL(DSS: TDSSContext; FileNm: String);
procedure ExportZCC(DSS: TDSSContext; FileNm: String);
procedure ExportY4(DSS: TDSSContext; FileNm: String);
procedure ExportC(DSS: TDSSContext; FileNm: String);
{$ENDIF}


implementation

uses
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
    Storage,
    KLUSolve,
    DSSHelper;

procedure WriteElementVoltagesExportFile(DSS: TDSSContext; var F: TextFile; pElem: TDSSCktElement; MaxNumNodes: Integer);

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
    Write(F, Format('%s.%s', [pElem.DSSClassName, pElem.Name]));


    Write(F, Format(',%d', [NTerm]));

    for j := 1 to NTerm do
    begin
        Write(F, Format(',%d,', [j]));
        Write(F, Format('%d,%d,', [NCond, pElem.NPhases]));

        Write(F, Format('%s,', [UpperCase(BusName)]));
        for i := 1 to NCond do
        begin
            Inc(k);
            nref := pElem.NodeRef^[k];
            Volts := DSS.ActiveCircuit.Solution.NodeV^[nref];
            Vmag := Cabs(Volts) * 0.001;
            if nref = 0 then
                Vpu := 0.0
            else
                with DSS.ActiveCircuit do
                begin
                    bref := MapNodeToBus^[nref].BusRef;
                    if Buses^[bref].kvbase <> 0.0 then
                        Vpu := Vmag / Buses^[bref].kVBase
                    else
                        Vpu := 0.0;

                    if (i = 1) then
                        Write(F, Format('%6.3f', [Buses^[bref].kvBase * sqrt(3)]));
                end;
            with DSS.ActiveCircuit do
            begin
                Write(F, Format(', %d, %10.6g, %6.3f, %9.5g', [k, Vmag, cdang(Volts), Vpu]));
            end; //end with ActiveCircuit

        end; //end numconductors

   {Zero Fill row}
        for m := (NCond + 1) to (MaxNumNodes) do
            Write(F, ', 0, 0, 0, 0');

        BusName := StripExtension(pElem.Nextbus);
    end; // end for numterminals
end; //end procedure


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportSeqVoltages(DSS: TDSSContext; FileNm: String);

// Export Symmetrical Component bus voltages

var
    F: TextFile;
    i, j: Integer;
    nref: Integer;
    Vph, VphLL, V012: array[1..3] of Complex;

    V0, V1, V2,
    Vpu, V2V1, V0V1: Double;
    Vresidual: Complex;
    V_NEMA: Double;

begin

    try
        Assignfile(F, FileNm);
        ReWrite(F);

        Writeln(F, 'Bus,  V1,  p.u.,Base kV, V2, %V2/V1, V0, %V0/V1, Vresidual, %NEMA');
        with DSS.ActiveCircuit do
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
                        Vph[1] := DSS.ActiveCircuit.Solution.NodeV^[nref];
                        V1 := Cabs(Vph[1]);
                    end
                    else
                        V1 := 0.0;
                end
                else
                begin

                    with  DSS.ActiveCircuit.Solution, Buses^[i] do
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
                with DSS.ActiveCircuit.Solution do
                    for j := 1 to Buses^[i].NumNodesThisBus do
                        Caccum(Vresidual, NodeV^[Buses^[i].GetRef(j)]);

                Writeln(F,
                    Format('"%s", %10.6g, %9.5g, %8.2f, %10.6g, %8.4g, %10.6g, %8.4g, %10.6g, %8.4g',
                    [Uppercase(BusList.Get(i)), V1, Vpu, (Buses^[i].kvbase * SQRT3), V2, V2V1, V0, V0V1, Cabs(Vresidual), V_NEMA]
                    ));


            end;
        end;


        DSS.GlobalResult := FileNm;

    finally

        CloseFile(F);
    end;

end;

//-------------------------------------------------------------------
procedure ExportVoltages(DSS: TDSSContext; FileNm: String);

// Export Symmetrical Component bus voltages

var
    MaxNumNodes: Integer;
    F: TextFile;
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
    with DSS.ActiveCircuit do
        for i := 1 to NumBuses do
            MaxNumNodes := max(MaxNumNodes, Buses^[i].NumNodesThisBus);

    try
        Assignfile(F, FileNm);
        ReWrite(F);


        Write(F, 'Bus, BasekV');
        for i := 1 to MaxNumNodes do
            Write(F, Format(', Node%d, Magnitude%d, Angle%d, pu%d', [i, i, i, i]));
        Writeln(F);

        with DSS.ActiveCircuit do
        begin
            for i := 1 to NumBuses do
            begin
                BusName := BusList.Get(i);
                Write(F, Format('"%s", %.5g', [UpperCase(BusName), Buses^[i].kvbase * SQRT3]));

                jj := 1;
                with Buses^[i] do
                    for j := 1 to NumNodesThisBus do
                    begin
                        repeat
                            NodeIdx := FindIdx(jj);     // Try to find nodes in order
                            inc(jj)
                        until NodeIdx > 0;
                        nref := GetRef(NodeIdx);
                        Volts := DSS.ActiveCircuit.Solution.NodeV^[nref];
                        Vmag := Cabs(Volts);
                        if kvbase <> 0.0 then
                            Vpu := 0.001 * Vmag / kVBase
                        else
                            Vpu := 0.0;

                        Write(F,
                            Format(', %d, %10.6g, %6.1f, %9.5g',
                            [GetNum(NodeIdx), Vmag, cdang(Volts), Vpu]));
                    end;
           {Zero Fill row}
                for j := Buses^[i].NumNodesThisBus + 1 to MaxNumNodes do
                    Write(F, ', 0, 0, 0, 0');
                Writeln(F);
            end;
        end;

        DSS.GlobalResult := FileNm;

    finally

        CloseFile(F);

    end;

end;
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure CalcAndWriteSeqCurrents(DSS: TDSSContext; var F: TextFile; j: Integer; pelem: TDSSCktElement; cBuffer: pComplexArray; DoRatings: Boolean);
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
        if DSS.ActiveCircuit.PositiveSequence    // Use phase 1 only
        then
            I1 := Cabs(Iph[1]);

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


    Writeln(F, Format('"%s", %3d, %10.6g, %8.4g, %8.4g, %10.6g, %8.4g, %10.6g, %8.4g, %10.6g, %8.4g',
        [(pelem.DSSClassName + '.' + UpperCase(pelem.Name)), j, I1, iNormal, iEmerg, I2, I2I1, I0, I0I1, Cabs(Iresidual), I_NEMA]));
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportSeqCurrents(DSS: TDSSContext; FileNm: String);

var
    F: TextFile;
    j: Integer;
    pElem: TDSSCktElement;
    PDElem: TPDElement;
    PCelem: TPCelement;
    cBuffer: pComplexArray;  // Allocate to max total conductors

begin

    cBuffer := NIL;

    try
        Assignfile(F, FileNm);
        ReWrite(F);


     {Sequence Currents}
        Writeln(F, 'Element, Terminal,  I1, %Normal, %Emergency, I2, %I2/I1, I0, %I0/I1, Iresidual, %NEMA');

     {Allocate cBuffer big enough for largest circuit element}
        Getmem(cbuffer, sizeof(cBuffer^[1]) * GetMaxCktElementSize(DSS));


     //Sources First
        Pelem := DSS.ActiveCircuit.Sources.First;
        while pelem <> NIL do
        begin
            if (pelem.Enabled) then
            begin
                pelem.GetCurrents(cBuffer);
                for j := 1 to pelem.Nterms do
                    CalcAndWriteSeqCurrents(DSS, F, j, pelem, cBuffer, FALSE);
            end;
            pelem := DSS.ActiveCircuit.Sources.Next;
        end;


     // PDELEMENTS Next
        PDelem := DSS.ActiveCircuit.PDElements.First;

        while PDelem <> NIL do
        begin
            if (PDelem.Enabled) then
            begin
                PDelem.GetCurrents(cBuffer);
                for j := 1 to PDelem.Nterms do
                    CalcAndWriteSeqCurrents(DSS, F, j, pDelem, cBuffer, TRUE);
            end;
            PDelem := DSS.ActiveCircuit.PDElements.Next;
        end;

    // PCelemENTS next
        PCelem := DSS.ActiveCircuit.PCelements.First;

        while PCelem <> NIL do
        begin
            if (PCelem.Enabled) then
            begin
                PCelem.GetCurrents(cBuffer);
                for j := 1 to PCelem.Nterms do
                    CalcAndWriteSeqCurrents(DSS, F, j, pCelem, cBuffer, FALSE);
            end;
            PCelem := DSS.ActiveCircuit.PCelements.Next;
        end;


     //Faults Next
        Pelem := DSS.ActiveCircuit.Faults.First;
        while pelem <> NIL do
        begin
            if (pelem.Enabled) then
            begin
                pelem.GetCurrents(cBuffer);
                for j := 1 to pelem.Nterms do
                    CalcAndWriteSeqCurrents(DSS, F, j, pelem, cBuffer, FALSE);
            end;
            pelem := DSS.ActiveCircuit.Faults.Next;
        end;

        DSS.GlobalResult := FileNm;


    finally
        if Assigned(Cbuffer) then
            Freemem(cBuffer);
        CloseFile(F);

    end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure CalcAndWriteCurrents(var F: TextFile; pElem: TDSSCktElement; Cbuffer: pComplexArray; CondWidth, TermWidth: Integer);
var
    i, j, k: Integer;
    Iresid: Complex;

begin
    k := 0;
    Write(F, Format('%s', [pelem.DSSClassName + '.' + UpperCase(pElem.Name)]));
    for      j := 1 to pElem.Nterms do
    begin
        Iresid := CZERO;
        for    i := 1 to pElem.NConds do
        begin
            Inc(k);
            Write(F,
                Format(', %10.6g, %8.2f', [Cabs(cBuffer^[k]), cdang(cBuffer^[k])]));
            Caccum(Iresid, cBuffer^[k]);
        end;
        for i := pElem.Nconds + 1 to CondWidth do
            Write(F, Format(', %10.6g, %8.2f', [0.0, 0.0]));
        Write(F, Format(', %10.6g, %8.2f', [Cabs(Iresid), cdang(Iresid)]));
    end;

    {Filler if no. terms less than termwidth}
    for j := pElem.Nterms + 1 to TermWidth do
        for i := 1 to Condwidth + 1 do
            Write(F, Format(', %10.6g, %8.2f', [0.0, 0.0]));

    Writeln(F);
end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure CalcAndWriteMaxCurrents(DSS: TDSSContext; var F: TextFile; pElem: TPDElement; Cbuffer: pComplexArray);
var
    i: Integer;
    Currmag, MaxCurrent: Double;
    LocalPower: Complex;

begin
    Write(F, Format('%s.%s', [pelem.DSSClassName, UpperCase(pElem.Name)]));
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
        Write(F, Format(', %10.6g, %8.2f, %8.2f', [MaxCurrent, 0.0, 0.0]))
    else
        Write(F, Format(', %10.6g, %8.2f, %8.2f', [MaxCurrent, MaxCurrent / pElem.NormAmps * 100.0, MaxCurrent / pElem.Emergamps * 100.0]));
    Write(F, Format(', %10.6g, %10.6g, %d, %d, %d', [Localpower.re, Localpower.im, pElem.BranchNumCustomers, pElem.BranchTotalCustomers, pElem.NPhases]));
    with DSS.ActiveCircuit do
        Write(F, Format(', %-.3g ', [Buses^[MapNodeToBus^[PElem.NodeRef^[1]].BusRef].kVBase]));
    Writeln(F);
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportCurrents(DSS: TDSSContext; FileNm: String);

var
    F: TextFile;
    cBuffer: pComplexArray;
    pElem: TDSSCktElement;
    MaxCond, MaxTerm: Integer;
    i, j: Integer;

begin

    cBuffer := NIL;

    try
        Assignfile(F, FileNm);
        ReWrite(F);

        Getmem(cBuffer, sizeof(cBuffer^[1]) * GetMaxCktElementSize(DSS));

     {Calculate the width of the file}
        MaxCond := 1;
        MaxTerm := 2;
        pElem := DSS.ActiveCircuit.CktElements.First;
        while pElem <> NIL do
        begin
            if pelem.NTerms > MaxTerm then
                MaxTerm := pelem.NTerms;
            if pelem.NConds > MaxCond then
                MaxCond := pelem.NConds;
            pElem := DSS.ActiveCircuit.CktElements.Next;
        end;


     {Branch Currents}
        Write(F, 'Element');
        for i := 1 to MaxTerm do
        begin
            for j := 1 to MaxCond do
                Write(F, Format(', I%d_%d, Ang%d_%d', [i, j, i, j]));
            Write(F, Format(', Iresid%d, AngResid%d', [i, i]));
        end;
        Writeln(F);


     // Sources first
        pElem := DSS.ActiveCircuit.Sources.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                pElem.GetCurrents(cBuffer);
                CalcAndWriteCurrents(F, pElem, Cbuffer, maxcond, maxterm);
            end;
            pElem := DSS.ActiveCircuit.Sources.Next;
        end;


     // PDELEMENTS first
        pElem := DSS.ActiveCircuit.PDElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                pElem.GetCurrents(cBuffer);
                CalcAndWriteCurrents(F, pElem, Cbuffer, maxcond, maxterm);
            end;
            pElem := DSS.ActiveCircuit.PDElements.Next;
        end;

     // Faults
        pElem := DSS.ActiveCircuit.Faults.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                pElem.GetCurrents(cBuffer);
                CalcAndWriteCurrents(F, pElem, Cbuffer, maxcond, maxterm);
            end;
            pElem := DSS.ActiveCircuit.Faults.Next;
        end;

     // PCELEMENTS next
        pElem := DSS.ActiveCircuit.PCElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                pElem.GetCurrents(cBuffer);
                CalcAndWriteCurrents(F, pElem, Cbuffer, maxcond, maxterm);
            end;
            pElem := DSS.ActiveCircuit.PCElements.Next;
        end;

        DSS.GlobalResult := FileNm;


    finally
        if Assigned(cBuffer) then
            Freemem(cBuffer);
        CloseFile(F);

    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure WriteNodeList(DSS: TDSSContext; var F: TextFile; const CktElementName: String);
var
    NValues, i: Integer;


begin


    if DSS.ActiveCircuit <> NIL then
        if not DSS.ActiveCircuit.IsSolved then
        begin
            DoSimpleMsg(DSS, 'Circuit must be solved for this command to execute properly.', 222001);
            Exit;
        end;


    if Length(CktElementName) > 0 then
    begin
        SetObject(DSS, CktElementName);


        if Assigned(DSS.ActiveCircuit.ActiveCktElement) then
            with DSS.ActiveCircuit.ActiveCktElement do
            begin
                Write(F, Format('"%s", %d, %d', [CktElementName, Nterms, Nconds]));
                NValues := NConds * Nterms;
                for i := 1 to NValues do
                begin
                    Write(F, Format(', %d', [GetNodeNum(DSS, NodeRef^[i])]));
                end;
                Writeln(F);
            end
    end;


end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportNodeOrder(DSS: TDSSContext; FileNm: String);

{ Writes NodeLists in same order as Export Currents function
}

var
    F: TextFile;
    pElem: TDSSCktElement;
    strName: String;

begin


    try
        Assignfile(F, FileNm);
        ReWrite(F);

     {Header Record}
        Write(F, 'Element, Nterminals, Nconductors, Node-1, Node-2, Node-3, ...');
        Writeln(F);


     // Sources first
        pElem := DSS.ActiveCircuit.Sources.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteNodeList(DSS, F, strName);
            end;
            pElem := DSS.ActiveCircuit.Sources.Next;
        end;


     // PDELEMENTS first
        pElem := DSS.ActiveCircuit.PDElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteNodeList(DSS, F, strName);
            end;
            pElem := DSS.ActiveCircuit.PDElements.Next;
        end;

     // Faults
        pElem := DSS.ActiveCircuit.Faults.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteNodeList(DSS, F, strName);
            end;
            pElem := DSS.ActiveCircuit.Faults.Next;
        end;

     // PCELEMENTS next
        pElem := DSS.ActiveCircuit.PCElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteNodeList(DSS, F, strName);
            end;
            pElem := DSS.ActiveCircuit.PCElements.Next;
        end;

        DSS.GlobalResult := FileNm;


    finally
        CloseFile(F);

    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure WriteElemCurrents(DSS: TDSSContext; var F: TextFile; const CktElementName: String);
var
    NValues, i: Integer;


begin


    if DSS.ActiveCircuit <> NIL then
        if not DSS.ActiveCircuit.Issolved then
        begin
            DoSimpleMsg(DSS, 'Circuit must be solved for this command to execute properly.', 222001);
            Exit;
        end;


    if Length(CktElementName) > 0 then
    begin
        SetObject(DSS, CktElementName);

        if Assigned(DSS.ActiveCircuit.ActiveCktElement) then
            with DSS.ActiveCircuit.ActiveCktElement do
            begin
                ComputeIterminal;
                Write(F, Format('"%s", %d, %d', [CktElementName, Nterms, Nconds]));
                NValues := NConds * Nterms;
                for i := 1 to NValues do
                begin
                    Write(F, Format(', %10.6g, %8.2f', [Cabs(Iterminal^[i]), cdang(Iterminal^[i])]));
                end;
                Writeln(F);
            end
    end;


end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportElemCurrents(DSS: TDSSContext; FileNm: String);

{ Export currents in same order as NodeOrder export
}
var
    F: TextFile;
    pElem: TDSSCktElement;
    strName: String;

begin


    try
        Assignfile(F, FileNm);
        ReWrite(F);

     {Header Record}
        Write(F, 'Element, Nterminals, Nconductors, I_1, Ang_1, ...');
        Writeln(F);


     // Sources first
        pElem := DSS.ActiveCircuit.Sources.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemCurrents(DSS, F, strName);
            end;
            pElem := DSS.ActiveCircuit.Sources.Next;
        end;


     // PDELEMENTS first
        pElem := DSS.ActiveCircuit.PDElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemCurrents(DSS, F, strName);
            end;
            pElem := DSS.ActiveCircuit.PDElements.Next;
        end;

     // Faults
        pElem := DSS.ActiveCircuit.Faults.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemCurrents(DSS, F, strName);
            end;
            pElem := DSS.ActiveCircuit.Faults.Next;
        end;

     // PCELEMENTS next
        pElem := DSS.ActiveCircuit.PCElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemCurrents(DSS, F, strName);
            end;
            pElem := DSS.ActiveCircuit.PCElements.Next;
        end;

        DSS.GlobalResult := FileNm;


    finally
        CloseFile(F);

    end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure WriteElemVoltages(DSS: TDSSContext; var F: TextFile; const CktElementName: String);
var
    NValues, i: Integer;


begin


    if DSS.ActiveCircuit <> NIL then
        if not DSS.ActiveCircuit.Issolved then
        begin
            DoSimpleMsg(DSS, 'Circuit must be solved for this command to execute properly.', 222001);
            Exit;
        end;


    if Length(CktElementName) > 0 then
    begin
        SetObject(DSS, CktElementName);

        if Assigned(DSS.ActiveCircuit.ActiveCktElement) then
            with DSS.ActiveCircuit.ActiveCktElement do
            begin
                ComputeVterminal;
                Write(F, Format('"%s", %d, %d', [CktElementName, Nterms, Nconds]));
                NValues := NConds * Nterms;
                for i := 1 to NValues do
                begin
                    Write(F, Format(', %10.6g, %8.2f', [Cabs(Vterminal^[i]), cdang(Vterminal^[i])]));
                end;
                Writeln(F);
            end
    end;


end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportElemVoltages(DSS: TDSSContext; FileNm: String);
{ Export conductor voltages in same order as NodeOrder export
}
var
    F: TextFile;
    pElem: TDSSCktElement;
    strName: String;

begin


    try
        Assignfile(F, FileNm);
        ReWrite(F);

     {Header Record}
        Write(F, 'Element, Nterminals, Nconductors, V_1, Ang_1, ...');
        Writeln(F);


     // Sources first
        pElem := DSS.ActiveCircuit.Sources.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemVoltages(DSS, F, strName);
            end;
            pElem := DSS.ActiveCircuit.Sources.Next;
        end;


     // PDELEMENTS first
        pElem := DSS.ActiveCircuit.PDElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemVoltages(DSS, F, strName);
            end;
            pElem := DSS.ActiveCircuit.PDElements.Next;
        end;

     // Faults
        pElem := DSS.ActiveCircuit.Faults.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemVoltages(DSS, F, strName);
            end;
            pElem := DSS.ActiveCircuit.Faults.Next;
        end;

     // PCELEMENTS next
        pElem := DSS.ActiveCircuit.PCElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemVoltages(DSS, F, strName);
            end;
            pElem := DSS.ActiveCircuit.PCElements.Next;
        end;

        DSS.GlobalResult := FileNm;


    finally
        CloseFile(F);

    end;

end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure WriteElemPowers(DSS: TDSSContext; var F: TextFile; const CktElementName: String);
var
    NValues, i: Integer;
    S: Complex;

begin


    if DSS.ActiveCircuit <> NIL then
        if not DSS.ActiveCircuit.Issolved then
        begin
            DoSimpleMsg(DSS, 'Circuit must be solved for this command to execute properly.', 222001);
            Exit;
        end;


    if Length(CktElementName) > 0 then
    begin
        SetObject(DSS, CktElementName);

        if Assigned(DSS.ActiveCircuit.ActiveCktElement) then
            with DSS.ActiveCircuit.ActiveCktElement do
            begin
                ComputeVterminal;
                ComputeIterminal;
                Write(F, Format('"%s", %d, %d', [CktElementName, Nterms, Nconds]));
                NValues := NConds * Nterms;
                for i := 1 to NValues do
                begin
                    S := Cmul(Vterminal^[i], Conjg(Iterminal^[i]));
                    Write(F, Format(', %10.6g, %10.6g', [S.re * 0.001, S.im * 0.001]));
                end;
                Writeln(F);
            end
    end;


end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportElemPowers(DSS: TDSSContext; FileNm: String);

{ Export conductor powers in same order as NodeOrder export
}
var
    F: TextFile;
    pElem: TDSSCktElement;
    strName: String;

begin


    try
        Assignfile(F, FileNm);
        ReWrite(F);

     {Header Record}
        Write(F, 'Element, Nterminals, Nconductors, P_1, Q_1, ...');
        Writeln(F);


     // Sources first
        pElem := DSS.ActiveCircuit.Sources.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemPowers(DSS, F, strName);
            end;
            pElem := DSS.ActiveCircuit.Sources.Next;
        end;


     // PDELEMENTS first
        pElem := DSS.ActiveCircuit.PDElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemPowers(DSS, F, strName);
            end;
            pElem := DSS.ActiveCircuit.PDElements.Next;
        end;

     // Faults
        pElem := DSS.ActiveCircuit.Faults.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemPowers(DSS, F, strName);
            end;
            pElem := DSS.ActiveCircuit.Faults.Next;
        end;

     // PCELEMENTS next
        pElem := DSS.ActiveCircuit.PCElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                strName := pElem.ParentClass.Name + '.' + pElem.Name;
                WriteElemPowers(DSS, F, strName);
            end;
            pElem := DSS.ActiveCircuit.PCElements.Next;
        end;

        DSS.GlobalResult := FileNm;


    finally
        CloseFile(F);

    end;

end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportPowers(DSS: TDSSContext; FileNm: String; opt: Integer);

{Opt = 0: kVA
 opt = 1: MVA
 }

var
    F: TextFile;
    Nterm, j: Integer;
    PDElem: TPDElement;
    PCElem: TPCElement;
    S: Complex;
    Separator: String;

begin


    try
        Assignfile(F, FileNm);
        ReWrite(F);
        Separator := ', ';


        case Opt of
            1:
                Writeln(F, 'Element, Terminal, P(MW), Q(Mvar), P_Normal, Q_Normal, P_Emergency, Q_Emergency');
        else
            Writeln(F, 'Element, Terminal, P(kW), Q(kvar),  P_Normal, Q_Normal, P_Emergency, Q_Emergency');
        end;

     // PDELEMENTS first
        PDElem := DSS.ActiveCircuit.PDElements.First;

        while PDElem <> NIL do
        begin
            if (PDElem.Enabled) then
            begin
                Nterm := pDElem.Nterms;

                for j := 1 to NTerm do
                begin
                    Write(F, Pad('"' + PDelem.DSSClassName + '.' + UpperCase(PDElem.Name) + '"', 24), Separator, j: 3);
           //----PDElem.ActiveTerminalIdx := j;
                    S := PDElem.Power[j];
                    if Opt = 1 then
                        S := CmulReal(S, 0.001);
                    Write(F, Separator, S.re * 0.001: 11: 1);
                    Write(F, Separator, S.im * 0.001: 11: 1);
                    if j = 1 then
                    begin
             //----PDelem.ActiveTerminalIdx := 1;
                        S := PDElem.ExcesskVANorm[1];
                        if Opt = 1 then
                            S := CmulReal(S, 0.001);
                        Write(F, Separator, Abs(S.re): 11: 1);
                        Write(F, Separator, Abs(S.im): 11: 1);
                        S := PDElem.ExcesskVAEmerg[1];
                        if Opt = 1 then
                            S := CmulReal(S, 0.001);
                        Write(F, Separator, Abs(S.re): 11: 1);
                        Write(F, Separator, Abs(S.im): 11: 1);
                    end;
                    Writeln(F);
                end;
            end;
            PDElem := DSS.ActiveCircuit.PDElements.Next;
        end;

     // PCELEMENTS Next
        PCElem := DSS.ActiveCircuit.PCElements.First;

        while PCElem <> NIL do
        begin

            if (PCElem.Enabled) then
            begin
                Nterm := PCElem.Nterms;

                for j := 1 to NTerm do
                begin
                    Write(F, Pad('"' + PCElem.DSSClassName + '.' + UpperCase(PCElem.Name) + '"', 24), Separator, j: 3);
           //----pcElem.ActiveTerminalIdx := j;
                    S := pCElem.Power[j];
                    if Opt = 1 then
                        S := CmulReal(S, 0.001);
                    Write(F, Separator, S.re * 0.001: 11: 1);
                    Write(F, Separator, S.im * 0.001: 11: 1);
                    Writeln(F);

                end;
            end;
            PCElem := DSS.ActiveCircuit.PCElements.Next;
        end;

        DSS.GlobalResult := FileNm;

    finally
        CloseFile(F);

    end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportLosses(DSS: TDSSContext; FileNm: String);

{Opt = 0: kVA
 opt = 1: MVA
 }

var
    F: TextFile;
    PDElem: TPDElement;
    S_total, S_Load, S_NoLoad: Complex;

begin


    try
        Assignfile(F, FileNm);
        ReWrite(F);

        Writeln(F, 'Element,  Total(W), Total(var),  I2R(W), I2X(var), No-load(W), No-load(var)');
     // PDELEMENTS first
        PDElem := DSS.ActiveCircuit.PDElements.First;

        while PDElem <> NIL do
        begin
            if (PDElem.Enabled) then
            begin
                PDElem.GetLosses(S_total, S_Load, S_NoLoad);
                Writeln(F, Format('%s.%s, %.7g, %.7g, %.7g, %.7g, %.7g, %.7g', [PDElem.ParentClass.Name, UpperCase(PDElem.Name), S_total.re, S_total.im, S_Load.re, S_Load.im, S_NoLoad.re, S_NoLoad.im]));
            end;
            PDElem := DSS.ActiveCircuit.PDElements.Next;
        end;

        DSS.GlobalResult := FileNm;

    finally
        CloseFile(F);

    end;
end;

// ===============================================================================
procedure ExportPbyphase(DSS: TDSSContext; FileNm: String; opt: Integer);

{ Export Powers by phase }

{Opt = 0: kVA
 opt = 1: MVA
 }

var
    F: TextFile;
    i: Integer;
    PDElem: TPDElement;
    PCElem: TPCElement;
    S: Complex;

begin


    try
        Assignfile(F, FileNm);
        ReWrite(F);

        case Opt of
            1:
                Writeln(F, 'Element, NumTerminals, NumConductors, NumPhases, MW1, Mvar1, MW2, Mvar2, MW3, Mvar3, ... ');
        else
            Writeln(F, 'Element, NumTerminals, NumConductors, NumPhases, kW1, kvar1, kW2, kvar2, kW3, kvar3, ... ');
        end;

     // PDELEMENTS first
        PDElem := DSS.ActiveCircuit.PDElements.First;

        while PDElem <> NIL do
        begin
            if (PDElem.Enabled) then
            begin
                with PDElem do
                begin
                    ComputeITerminal;
                    ComputeVTerminal;
                    Write(F, Format('"%s.%s", %d, %d, %d', [DSSClassName, Uppercase(Name), NTerms, NConds, Nphases]));
                    for i := 1 to Yorder do
                    begin
                        S := CmulReal(Cmul(Vterminal^[i], conjg(ITerminal^[i])), 0.001);
                        if Opt = 1 then
                            S := CmulReal(S, 0.001);   // convert to MVA
                        Write(F, Format(', %10.3f, %10.3f', [S.re, S.im]));
                    end;
                end;
                Writeln(F);
            end;
            PDElem := DSS.ActiveCircuit.PDElements.Next;
        end;

     // PCELEMENTS Next
        PCElem := DSS.ActiveCircuit.PCElements.First;

        while PCElem <> NIL do
        begin

            if (PCElem.Enabled) then
            begin
                with PCelem do
                begin
                    ComputeITerminal;
                    ComputeVTerminal;
                    Write(F, Format('"%s.%s", %d, %d, %d', [DSSClassName, Uppercase(Name), NTerms, NConds, NPhases]));
                    for i := 1 to Yorder do
                    begin
                        S := CmulReal(Cmul(Vterminal^[i], conjg(ITerminal^[i])), 0.001);
                        if Opt = 1 then
                            S := CmulReal(S, 0.001);   // convert to MVA
                        Write(F, Format(', %10.3f, %10.3f', [S.re, S.im]));
                    end;
                end;
                Writeln(F);

            end;
            PCElem := DSS.ActiveCircuit.PCElements.Next;
        end;

        DSS.GlobalResult := FileNm;

    finally
        CloseFile(F);

    end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportSeqPowers(DSS: TDSSContext; FileNm: String; opt: Integer);

{Opt = 0: kVA
 opt = 1: MVA
 }

var
    F: TextFile;
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


begin

    cBuffer := NIL;

    try
        Assignfile(F, FileNm);
        ReWrite(F);
        Separator := ', ';

        Getmem(cBuffer, sizeof(cBuffer^[1]) * GetMaxCktElementSize(DSS));

        case Opt of
            1:
                Writeln(F, 'Element, Terminal, P1(MW), Q1(Mvar), P2, Q2, P0, Q0, P_Normal, Q_Normal, P_Emergency, Q_Emergency');
        else
            Writeln(F, 'Element, Terminal, P1(kW), Q1(kvar), P2, Q2, P0, Q0, P_Normal, Q_Normal, P_Emergency, Q_Emergency');
        end;

     // PDELEMENTS first
        PDElem := DSS.ActiveCircuit.PDElements.First;

        while PDElem <> NIL do
        begin
            if (PDElem.Enabled) then
            begin
                NCond := pDElem.NConds;
                Nterm := pDElem.Nterms;
                PDElem.GetCurrents(cBuffer);

                for j := 1 to NTerm do
                begin
                    Write(F, Pad('"' + PDelem.DSSClassName + '.' + Uppercase(PDElem.Name) + '"', 24), Separator, j: 3);
                    for i := 1 to PDElem.NPhases do
                    begin
                        k := (j - 1) * Ncond + i;
                        nref := pDElem.NodeRef^[k];
                        Volts := DSS.ActiveCircuit.Solution.NodeV^[nref];
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
                        if DSS.ActiveCircuit.PositiveSequence then
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
                    Write(F, Separator, S.re * 0.003: 11: 1);
                    Write(F, Separator, S.im * 0.003: 11: 1);
                    S := Cmul(V012[3], conjg(I012[3]));
                    if Opt = 1 then
                        S := CmulReal(S, 0.001);
                    Write(F, Separator, S.re * 0.003: 11: 1);
                    Write(F, Separator, S.im * 0.003: 11: 1);
                    S := Cmul(V012[1], conjg(I012[1]));
                    if Opt = 1 then
                        S := CmulReal(S, 0.001);
                    Write(F, Separator, S.re * 0.003: 8: 1);
                    Write(F, Separator, S.im * 0.003: 8: 1);

                    if j = 1 then
                    begin
                 //----PDelem.ActiveTerminalIdx := 1;
                        S := PDElem.ExcesskVANorm[1];
                        if Opt = 1 then
                            S := CmulReal(S, 0.001);
                        Write(F, Separator, Abs(S.re): 11: 1);
                        Write(F, Separator, Abs(S.im): 11: 1);
                        S := PDElem.ExcesskVAEmerg[1];
                        if Opt = 1 then
                            S := CmulReal(S, 0.001);
                        Write(F, Separator, Abs(S.re): 11: 1);
                        Write(F, Separator, Abs(S.im): 11: 1);
                    end;
                    Writeln(F);

                end;
            end;
            PDElem := DSS.ActiveCircuit.PDElements.Next;
        end;

     // PCELEMENTS Next
        PCElem := DSS.ActiveCircuit.PCElements.First;

        while PCElem <> NIL do
        begin

            if (PCElem.Enabled) then
            begin
                NCond := PCElem.NConds;
                Nterm := PCElem.Nterms;
                PCElem.GetCurrents(cBuffer);

                for j := 1 to NTerm do
                begin
                    Write(F, Pad('"' + PCElem.DSSClassName + '.' + Uppercase(PCElem.Name) + '"', 24), Separator, j: 3);
                    for i := 1 to PCElem.NPhases do
                    begin
                        k := (j - 1) * Ncond + i;
                        nref := PCElem.NodeRef^[k];
                        Volts := DSS.ActiveCircuit.Solution.NodeV^[nref];
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
                        if DSS.ActiveCircuit.PositiveSequence then
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
                    Write(F, Separator, S.re * 0.003: 11: 1);
                    Write(F, Separator, S.im * 0.003: 11: 1);
                    S := Cmul(V012[3], conjg(I012[3]));
                    if Opt = 1 then
                        S := CmulReal(S, 0.001);
                    Write(F, Separator, S.re * 0.003: 11: 1);
                    Write(F, Separator, S.im * 0.003: 11: 1);
                    S := Cmul(V012[1], conjg(I012[1]));
                    if Opt = 1 then
                        S := CmulReal(S, 0.001);
                    Write(F, Separator, S.re * 0.003: 8: 1);
                    Write(F, Separator, S.im * 0.003: 8: 1);

                    Writeln(F);

                end;
            end;
            PCElem := DSS.ActiveCircuit.PCElements.Next;
        end;

        DSS.GlobalResult := FileNm;

    finally
        if Assigned(cBuffer) then
            Freemem(CBuffer);
        CloseFile(F);

    end;
end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportFaultStudy(DSS: TDSSContext; FileNm: String);

var
    i, iBus, iphs: Integer;
    YFault: Tcmatrix;
    Vfault: pComplexArray;  {Big temp array}
    F: Textfile;
    GFault: complex;
    Separator: String;
    MaxCurr,
    CurrMag: Double;

begin

    try

        Assignfile(F, FileNm);
        ReWrite(F);

        Separator := ', ';

   { Set source voltage injection currents }
        with DSS.ActiveCircuit do
        begin
            with Solution do
            begin

     {All Phase Faults}
                Writeln(F, 'Bus,  3-Phase,  1-Phase,  L-L');
                for iBus := 1 to NumBuses do
           {Bus Norton Equivalent Current, Isc has been previously computed}
                    with Buses^[iBus] do
                    begin
                        Write(F, Pad(Uppercase(BusList.Get(iBus)), 12));
                        MaxCurr := 0.0;
                        for i := 1 to NumNodesThisBus do
                        begin
                            if MaxCurr < Cabs(BusCurrent^[i]) then
                                MaxCurr := Cabs(BusCurrent^[i]);
                            ;
                        end;
                        Write(F, Separator, maxCurr: 10: 0);

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
                        Write(F, Separator, maxCurr: 10: 0);

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

                        Write(F, Separator, MaxCurr: 10: 0);

                        Freemem(VFault);
                        YFault.Free;

                        Writeln(F);
                    end;  {With bus}

            end; {With Solution}
        end; {With DSS.ActiveCircuit}

        DSS.GlobalResult := Filenm;

    finally

        CloseFile(F);

    end;
end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportEstimation(DSS: TDSSContext; FileNm: String);

var
    F: TextFile;
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
        AssignFile(F, FileNm);
        Rewrite(F);   // clears file

          {Do the EnergyMeters first}
        Writeln(F, '"Energy Meters" ');
        Writeln(F, '"energyMeter", "I1 Target", "I2 Target", "I3 Target", "I1 Calc", "I2 Calc", "I3 Calc", "I1 %Err", "I2 %Err", "I3 %Err"'{, "I1 Factor", "I2 Factor", "I3 Factor"'});

        pEnergyMeterObj := DSS.ActiveCircuit.energyMeters.First;
        while pEnergyMeterObj <> NIL do
        begin
            if pEnergyMeterObj.Enabled then
            begin
                Write(F, Format('"Energymeter.%s"', [Uppercase(pEnergyMeterObj.Name)]));
                  {Sensor currents (Target)}
                ZeroTempXArray;
                for i := 1 to pEnergyMeterObj.Nphases do
                    TempX[i] := pEnergyMeterObj.SensorCurrent^[i];
                for i := 1 to 3 do
                    Write(F, Format(', %.6g', [TempX[i]]));
                  {Calculated Currents}
                ZeroTempXArray;
                for i := 1 to pEnergyMeterObj.Nphases do
                    TempX[i] := Cabs(pEnergyMeterObj.CalculatedCurrent^[i]);
                for i := 1 to 3 do
                    Write(F, Format(', %.6g', [TempX[i]]));
                  {Percent Error}
                for i := 1 to pEnergyMeterObj.Nphases do
                    TempX[i] := (1.0 - TempX[i] / Max(0.001, pEnergyMeterObj.SensorCurrent^[i])) * 100.0;
                for i := 1 to 3 do
                    Write(F, Format(', %.6g', [TempX[i]]));

                  (****  Not all that useful
                  {Allocation Factors}
                  ZeroTempXArray;
                  For i := 1 to pEnergyMeterObj.Nphases do TempX[i] := pEnergyMeterObj.PhsAllocationFactor^[i];
                  For i := 1 to 3 do Write(F, Format(' %.6g,',[TempX[i]]));
                  *****)

                Writeln(F);
            end;
            pEnergyMeterObj := DSS.ActiveCircuit.EnergyMeters.Next;
        end;

          {Do the Sensors Next}
        Writeln(F);
        Writeln(F, '"Sensors" ');
        Write(F, '"Sensor", "I1 Target", "I2 Target", "I3 Target", "I1 Calc", "I2 Calc", "I3 Calc", "I1 %Err", "I2 %Err", "I3 %Err",');
        Writeln(F, ' "V1 Target", "V2 Target", "V3 Target", "V1 Calc", "V2 Calc", "V3 Calc", "V1 %Err", "V2 %Err", "V3 %Err", "WLS Voltage Err", "WLS Current Err"');

        pSensorObj := DSS.ActiveCircuit.Sensors.First;
        while pSensorObj <> NIL do
        begin
            if pSensorObj.Enabled then
            begin
                Write(F, Format('"Sensor.%s"', [Uppercase(pSensorObj.Name)]));
                  {Sensor currents (Target)}
                ZeroTempXArray;
                for i := 1 to pSensorObj.Nphases do
                    TempX[i] := pSensorObj.SensorCurrent^[i];
                for i := 1 to 3 do
                    Write(F, Format(', %.6g', [TempX[i]]));
                  {Calculated Currents}
                ZeroTempXArray;
                for i := 1 to pSensorObj.Nphases do
                    TempX[i] := Cabs(pSensorObj.CalculatedCurrent^[i]);
                for i := 1 to 3 do
                    Write(F, Format(', %.6g', [TempX[i]]));
                  {Percent Error}
                for i := 1 to pSensorObj.Nphases do
                    TempX[i] := (1.0 - TempX[i] / Max(0.001, pSensorObj.SensorCurrent^[i])) * 100.0;
                for i := 1 to 3 do
                    Write(F, Format(', %.6g', [TempX[i]]));
                  {Sensor Voltage (Target)}
                ZeroTempXArray;
                for i := 1 to pSensorObj.Nphases do
                    TempX[i] := pSensorObj.SensorVoltage^[i];
                for i := 1 to 3 do
                    Write(F, Format(', %.6g', [TempX[i]]));
                  {Calculated Voltage}
                ZeroTempXArray;
                for i := 1 to pSensorObj.Nphases do
                    TempX[i] := Cabs(pSensorObj.CalculatedVoltage^[i]);
                for i := 1 to 3 do
                    Write(F, Format(', %.6g', [TempX[i]]));
                  {Percent Error}
                for i := 1 to pSensorObj.Nphases do
                    TempX[i] := (1.0 - TempX[i] / Max(0.001, pSensorObj.SensorVoltage^[i])) * 100.0;
                for i := 1 to 3 do
                    Write(F, Format(', %.6g', [TempX[i]]));
                  {WLS Errors}
                ZeroTempXArray;
                Write(F, Format(', %.6g, %.6g', [pSensorObj.WLSVoltageError, pSensorObj.WLSCurrentError]));

                Writeln(F);
            end;
            pSensorObj := DSS.ActiveCircuit.Sensors.Next;
        end;


    finally
        AppendGlobalResult(DSS, FileNm);
        CloseFile(F);

    end;


end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure WriteMultipleMeterFiles(DSS: TDSSContext);

var
    F: TextFile;
    i, j: Integer;
    pElem: TEnergyMeterObj;
    MeterClass: TEnergyMeter;
    FileNm,
    Separator: String;

begin

    MeterClass := TEnergyMeter(GetDSSClassPtr(DSS, 'Energymeter'));
    if MeterClass = NIL then
        Exit;  // oops somewhere!!
    Separator := ', ';

    pElem := DSS.ActiveCircuit.energyMeters.First;
    while pElem <> NIL do
    begin
        if pElem.Enabled then
        begin
            try
                FileNm := DSS.OutputDirectory + 'EXP_MTR_' + Uppercase(pElem.Name) + '.CSV';

                if not FileExists(FileNm) then
                begin
                    AssignFile(F, FileNm);
                    Rewrite(F);
                {Write New Header}
                    Write(F, 'Year, LDCurve, Hour, Meter');
                    for i := 1 to NumEMRegisters do
                        Write(F, Separator, '"' + pelem.RegisterNames[i] + '"');
                    Writeln(F);
                    CloseFile(F);
                end;

                AssignFile(F, FileNm);
                Append(F);
                Write(F, DSS.ActiveCircuit.Solution.Year: 0, Separator);
                Write(F, DSS.ActiveCircuit.LoadDurCurve, Separator);
                Write(F, DSS.ActiveCircuit.Solution.DynaVars.intHour: 0, Separator);
                Write(F, Pad('"' + Uppercase(pElem.Name) + '"', 14));
                for j := 1 to NumEMRegisters do
                    Write(F, Separator, PElem.Registers[j]: 10: 0);
                Writeln(F);
                AppendGlobalResult(DSS, FileNm);
            finally
                CloseFile(F);
            end;

        end;
        pElem := DSS.ActiveCircuit.EnergyMeters.Next;
    end;


end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure WriteSingleMeterFile(DSS: TDSSContext; const FileNm: String);
var
    F: TextFile;
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
            Assignfile(F, FileNm);
            Reset(F);
            if not EOF(F) then
            begin
                Read(F, TestStr);
             {See if it likely that the file is OK}
                if CompareText(Copy(TestStr, 1, 4), 'Year') = 0 then
                    RewriteFile := FALSE       // Assume the file is OK
                else
                    RewriteFile := TRUE;
            end
            else
                RewriteFile := TRUE;

            CloseFile(F);

        end
        else
        begin
            ReWriteFile := TRUE;
            AssignFile(F, FileNm);
        end;

   {Either open or append the file}
        if RewriteFile then
        begin
            ReWrite(F);
        {Write New Header}
            pElem := DSS.ActiveCircuit.energyMeters.First;
            Write(F, 'Year, LDCurve, Hour, Meter');
            for i := 1 to NumEMRegisters do
                Write(F, Separator, '"' + pElem.RegisterNames[i] + '"');
            Writeln(F);
        end
        else
            Append(F);


        pElem := DSS.ActiveCircuit.energyMeters.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                Write(F, DSS.ActiveCircuit.Solution.Year: 0, Separator);
                Write(F, DSS.ActiveCircuit.LoadDurCurve, Separator);
                Write(F, DSS.ActiveCircuit.Solution.DynaVars.intHour: 0, Separator);
                Write(F, Pad('"' + Uppercase(pElem.Name) + '"', 14));
                for j := 1 to NumEMRegisters do
                    Write(F, Separator, PElem.Registers[j]: 10: 0);
                Writeln(F);
            end;
            pElem := DSS.ActiveCircuit.EnergyMeters.Next;
        end;

        DSS.GlobalResult := FileNm;

    finally

        CloseFile(F);

    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportMeters(DSS: TDSSContext; FileNm: String);

// Export Values of  Meter Elements

// These records are appended to an existing file so a running account is kept for some kinds of simulations

// If switch /m is specified, a separate file is created for each meter using the meter's name

begin


    if Lowercase(Copy(FileNm, 1, 2)) = '/m' then
        WriteMultipleMeterFiles(DSS)
    else
        WriteSingleMeterFile(DSS, FileNM);
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure WriteMultipleGenMeterFiles(DSS: TDSSContext);

var
    F: TextFile;
    i, j: Integer;
    pElem: TGeneratorObj;
    GeneratorClass: TGenerator;
    FileNm,
    Separator: String;

begin

    GeneratorClass := TGenerator(GetDSSClassPtr(DSS, 'generator'));
    if GeneratorClass = NIL then
        Exit;  // oops somewhere!!
    Separator := ', ';

    pElem := DSS.ActiveCircuit.Generators.First;
    while pElem <> NIL do
    begin
        if pElem.Enabled then
        begin
            try
                FileNm := DSS.OutputDirectory + 'EXP_GEN_' + Uppercase(pElem.Name) + '.CSV';

                if not FileExists(FileNm) then
                begin
                    AssignFile(F, FileNm);
                    Rewrite(F);
                {Write New Header}
                    Write(F, 'Year, LDCurve, Hour, Generator');
                    for i := 1 to NumGenRegisters do
                        Write(F, Separator, '"' + GeneratorClass.RegisterNames[i] + '"');
                    Writeln(F);
                    CloseFile(F);
                end;

                AssignFile(F, FileNm);
                Append(F);
                with DSS.ActiveCircuit do
                begin
                    Write(F, Solution.Year: 0, Separator);
                    Write(F, LoadDurCurve, Separator);
                    Write(F, Solution.DynaVars.intHour: 0, Separator);
                    Write(F, Pad('"' + Uppercase(pElem.Name) + '"', 14));
                    for j := 1 to NumGenRegisters do
                        Write(F, Separator, PElem.Registers[j]: 10: 0);
                    Writeln(F);
                end;
                AppendGlobalResult(DSS, FileNm);
            finally
                CloseFile(F);
            end;

        end;
        pElem := DSS.ActiveCircuit.Generators.Next;
    end;

end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure WriteSingleGenMeterFile(DSS: TDSSContext; FileNm: String);

var
    F: TextFile;
    i, j: Integer;
    pElem: TGeneratorObj;
    GeneratorClass: TGenerator;
    Separator, TestStr: String;
    ReWriteFile: Boolean;

begin


    GeneratorClass := TGenerator(GetDSSClassPtr(DSS, 'generator'));
    if GeneratorClass = NIL then
        Exit;  // oops somewhere!!
    Separator := ', ';


    try

        if FileExists(FileNm) then
        begin  // See if it has already been written on
            Assignfile(F, FileNm);
            Reset(F);
            if not EOF(F) then
            begin
                Read(F, TestStr);
             {See if it likely that the file is OK}
                if CompareText(Copy(TestStr, 1, 4), 'Year') = 0 then
                    RewriteFile := FALSE       // Assume the file is OK
                else
                    RewriteFile := TRUE;
            end
            else
                RewriteFile := TRUE;

            CloseFile(F);

        end
        else
        begin
            ReWriteFile := TRUE;
            AssignFile(F, FileNm);
        end;

   {Either open or append the file}
        if RewriteFile then
        begin
            ReWrite(F);
        {Write New Header}
            Write(F, 'Year, LDCurve, Hour, Generator');
            for i := 1 to NumGenRegisters do
                Write(F, Separator, '"' + GeneratorClass.RegisterNames[i] + '"');
            Writeln(F);
        end
        else
            Append(F);


        pElem := DSS.ActiveCircuit.Generators.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
                with DSS.ActiveCircuit do
                begin
                    Write(F, Solution.Year: 0, Separator);
                    Write(F, LoadDurCurve, Separator);
                    Write(F, Solution.DynaVars.intHour: 0, Separator);
                    Write(F, Pad('"' + Uppercase(pElem.Name) + '"', 14));
                    for j := 1 to NumGenRegisters do
                        Write(F, Separator, PElem.Registers[j]: 10: 0);
                    Writeln(F);
                end;

            pElem := DSS.ActiveCircuit.Generators.Next;
        end;

        DSS.GlobalResult := FileNm;

    finally

        CloseFile(F);

    end;


end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure WriteMultiplePVSystemMeterFiles(DSS: TDSSContext);

var
    F: TextFile;
    i, j: Integer;
    pElem: TPVSystemObj;
    FileNm,
    Separator: String;

begin

    if DSS.PVSystemClass = NIL then
        Exit;  // oops somewhere!!
    Separator := ', ';

    pElem := DSS.ActiveCircuit.PVSystems.First;
    while pElem <> NIL do
    begin
        if pElem.Enabled then
        begin
            try
                FileNm := DSS.OutputDirectory + 'EXP_PV_' + Uppercase(pElem.Name) + '.CSV';

                if not FileExists(FileNm) then
                begin
                    AssignFile(F, FileNm);
                    Rewrite(F);
                {Write New Header}
                    Write(F, 'Year, LDCurve, Hour, PVSystem');
                    for i := 1 to NumPVSystemRegisters do
                        Write(F, Separator, '"' + DSS.PVSystemClass.RegisterNames[i] + '"');
                    Writeln(F);
                    CloseFile(F);
                end;

                AssignFile(F, FileNm);
                Append(F);
                with DSS.ActiveCircuit do
                begin
                    Write(F, Solution.Year: 0, Separator);
                    Write(F, LoadDurCurve, Separator);
                    Write(F, Solution.DynaVars.intHour: 0, Separator);
                    Write(F, Pad('"' + Uppercase(pElem.Name) + '"', 14));
                    for j := 1 to NumPVSystemRegisters do
                        Write(F, Separator, PElem.Registers[j]: 10: 0);
                    Writeln(F);
                end;
                AppendGlobalResult(DSS, FileNm);
            finally
                CloseFile(F);
            end;

        end;
        pElem := DSS.ActiveCircuit.PVSystems.Next;
    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure WriteSinglePVSystemMeterFile(DSS: TDSSContext; FileNm: String);

var
    F: TextFile;
    i, j: Integer;
    pElem: TPVSystemObj;
    Separator, TestStr: String;
    ReWriteFile: Boolean;

begin


    if DSS.PVSystemClass = NIL then
        Exit;  // oops somewhere!!
    Separator := ', ';


    try

        if FileExists(FileNm) then
        begin  // See if it has already been written on
            Assignfile(F, FileNm);
            Reset(F);
            if not EOF(F) then
            begin
                Read(F, TestStr);
             {See if it likely that the file is OK}
                if CompareText(Copy(TestStr, 1, 4), 'Year') = 0 then
                    RewriteFile := FALSE       // Assume the file is OK
                else
                    RewriteFile := TRUE;
            end
            else
                RewriteFile := TRUE;

            CloseFile(F);

        end
        else
        begin
            ReWriteFile := TRUE;
            AssignFile(F, FileNm);
        end;

   {Either open or append the file}
        if RewriteFile then
        begin
            ReWrite(F);
        {Write New Header}
            Write(F, 'Year, LDCurve, Hour, PVSystem');
            for i := 1 to NumGenRegisters do
                Write(F, Separator, '"' + DSS.PVSystemClass.RegisterNames[i] + '"');
            Writeln(F);
        end
        else
            Append(F);


        pElem := DSS.ActiveCircuit.PVSystems.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
                with DSS.ActiveCircuit do
                begin
                    Write(F, Solution.Year: 0, Separator);
                    Write(F, LoadDurCurve, Separator);
                    Write(F, Solution.DynaVars.intHour: 0, Separator);
                    Write(F, Pad('"' + Uppercase(pElem.Name) + '"', 14));
                    for j := 1 to NumPVSystemRegisters do
                        Write(F, Separator, PElem.Registers[j]: 10: 0);
                    Writeln(F);
                end;

            pElem := DSS.ActiveCircuit.PVSystems.Next;
        end;

        DSS.GlobalResult := FileNm;

    finally

        CloseFile(F);

    end;


end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure WriteMultipleStorageMeterFiles(DSS: TDSSContext);

var
    F: TextFile;
    i, j: Integer;
    pElem: TStorageObj;
    FileNm,
    Separator: String;

begin

    if DSS.StorageClass = NIL then
        Exit;  // oops somewhere!!
    Separator := ', ';

    pElem := DSS.ActiveCircuit.StorageElements.First;
    while pElem <> NIL do
    begin
        if pElem.Enabled then
        begin
            try
                FileNm := DSS.OutputDirectory + 'EXP_PV_' + Uppercase(pElem.Name) + '.CSV';

                if not FileExists(FileNm) then
                begin
                    AssignFile(F, FileNm);
                    Rewrite(F);
                {Write New Header}
                    Write(F, 'Year, LDCurve, Hour, Storage');
                    for i := 1 to NumStorageRegisters do
                        Write(F, Separator, '"' + DSS.StorageClass.RegisterNames[i] + '"');
                    Writeln(F);
                    CloseFile(F);
                end;

                AssignFile(F, FileNm);
                Append(F);
                with DSS.ActiveCircuit do
                begin
                    Write(F, Solution.Year: 0, Separator);
                    Write(F, LoadDurCurve, Separator);
                    Write(F, Solution.DynaVars.intHour: 0, Separator);
                    Write(F, Pad('"' + Uppercase(pElem.Name) + '"', 14));
                    for j := 1 to NumStorageRegisters do
                        Write(F, Separator, PElem.Registers[j]: 10: 0);
                    Writeln(F);
                end;
                AppendGlobalResult(DSS, FileNm);
            finally
                CloseFile(F);
            end;

        end;
        pElem := DSS.ActiveCircuit.StorageElements.Next;
    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure WriteSingleStorageMeterFile(DSS: TDSSContext; FileNm: String);

var
    F: TextFile;
    i, j: Integer;
    pElem: TStorageObj;
    Separator, TestStr: String;
    ReWriteFile: Boolean;

begin


    if DSS.StorageClass = NIL then
        Exit;  // oops somewhere!!
    Separator := ', ';


    try

        if FileExists(FileNm) then
        begin  // See if it has already been written on
            Assignfile(F, FileNm);
            Reset(F);
            if not EOF(F) then
            begin
                Read(F, TestStr);
             {See if it likely that the file is OK}
                if CompareText(Copy(TestStr, 1, 4), 'Year') = 0 then
                    RewriteFile := FALSE       // Assume the file is OK
                else
                    RewriteFile := TRUE;
            end
            else
                RewriteFile := TRUE;

            CloseFile(F);

        end
        else
        begin
            ReWriteFile := TRUE;
            AssignFile(F, FileNm);
        end;

   {Either open or append the file}
        if RewriteFile then
        begin
            ReWrite(F);
        {Write New Header}
            Write(F, 'Year, LDCurve, Hour, Storage');
            for i := 1 to NumStorageRegisters do
                Write(F, Separator, '"' + DSS.StorageClass.RegisterNames[i] + '"');
            Writeln(F);
        end
        else
            Append(F);


        pElem := DSS.ActiveCircuit.StorageElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
                with DSS.ActiveCircuit do
                begin
                    Write(F, Solution.Year: 0, Separator);
                    Write(F, LoadDurCurve, Separator);
                    Write(F, Solution.DynaVars.intHour: 0, Separator);
                    Write(F, Pad('"' + Uppercase(pElem.Name) + '"', 14));
                    for j := 1 to NumStorageRegisters do
                        Write(F, Separator, PElem.Registers[j]: 10: 0);
                    Writeln(F);
                end;

            pElem := DSS.ActiveCircuit.StorageElements.Next;
        end;

        DSS.GlobalResult := FileNm;

    finally

        CloseFile(F);

    end;


end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportGenMeters(DSS: TDSSContext; FileNm: String);

// Export Values of Generator Meter Elements
// If switch /m is specified, a separate file is created for each generator using the generator's name

begin


    if Lowercase(Copy(FileNm, 1, 2)) = '/m' then
        WriteMultipleGenMeterFiles(DSS)
    else
        WriteSingleGenMeterFile(DSS, FileNM);

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportPVSystemMeters(DSS: TDSSContext; FileNm: String);

// Export Values of Generator Meter Elements
// If switch /m is specified, a separate file is created for each generator using the generator's name

begin


    if Lowercase(Copy(FileNm, 1, 2)) = '/m' then
        WriteMultiplePVSystemMeterFiles(DSS)
    else
        WriteSinglePVSystemMeterFile(DSS, FileNM);

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportStorageMeters(DSS: TDSSContext; FileNm: String);

// Export Values of Generator Meter Elements
// If switch /m is specified, a separate file is created for each generator using the generator's name

begin


    if Lowercase(Copy(FileNm, 1, 2)) = '/m' then
        WriteMultipleStorageMeterFiles(DSS)
    else
        WriteSingleStorageMeterFile(DSS, FileNM);

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportLoads(DSS: TDSSContext; FileNm: String);

// Export Loads to view present allocation


var
    F: TextFile;
    pElem: TLoadObj;
    Separator: String;

begin

    Separator := ', ';


    try

        AssignFile(F, FileNm);
        ReWrite(F);
     {Write  Header}
        Writeln(F, 'Load, Connected KVA, Allocation Factor, Phases, kW, kvar, PF, Model');

        pElem := DSS.ActiveCircuit.Loads.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
                with pElem do
                begin
                    Write(F, Uppercase(Name));
                    Write(F, Separator, ConnectedkVA: 8: 1);
                    Write(F, Separator, kVAAllocationFactor: 5: 3);
                    Write(F, Separator, NPhases: 0);
                    Write(F, Separator, kWBase: 8: 1);
                    Write(F, Separator, kvarBase: 8: 1);
                    Write(F, Separator, PFNominal: 5: 3);
                    Write(F, Separator, FLoadModel: 0);
                end;
            Writeln(F);
            pElem := DSS.ActiveCircuit.Loads.Next;
        end;

        DSS.GlobalResult := FileNm;

    finally

        CloseFile(F);

    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportCapacity(DSS: TDSSContext; FileNm: String);

{
 Similar to export currents except does only max of the phases and compares that
 to the Normamps and Emergamps rating
}

var
    F: TextFile;
    cBuffer: pComplexArray;
    pElem: TPDElement;

begin

    cBuffer := NIL;

    try
        Assignfile(F, FileNm);
        ReWrite(F);

        Getmem(cBuffer, sizeof(cBuffer^[1]) * GetMaxCktElementSize(DSS));

        Writeln(F, 'Name, Imax, %normal, %emergency, kW, kvar, NumCustomers, TotalCustomers, NumPhases, kVBase');

     // PDELEMENTS ONLY
        pElem := DSS.ActiveCircuit.PDElements.First;
        while pElem <> NIL do
        begin
            if pElem.Enabled then
            begin
                pElem.GetCurrents(cBuffer);
                CalcAndWriteMaxCurrents(DSS, F, pElem, Cbuffer);
            end;
            pElem := DSS.ActiveCircuit.PDElements.Next;
        end;

        DSS.GlobalResult := FileNm;

    finally
        if Assigned(cBuffer) then
            Freemem(cBuffer);
        CloseFile(F);

    end;


end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportOverloads(DSS: TDSSContext; FileNm: String);

var
    F: TextFile;
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

begin

    cBuffer := NIL;

    try
        Assignfile(F, FileNm);
        ReWrite(F);

    {Allocate cBuffer big enough for largest circuit element}
        Getmem(cbuffer, sizeof(cBuffer^[1]) * GetMaxCktElementSize(DSS));

     {Sequence Currents}
        Writeln(F, 'Element, Terminal,  I1, AmpsOver, kVAOver, %Normal, %Emergency, I2, %I2/I1, I0, %I0/I1');

        Separator := ', ';

     // PDELEMENTS Only
        PDelem := DSS.ActiveCircuit.PDElements.First;

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

                                Write(F, Format('%s, %d, ', [Pad(('"' + pDelem.DSSClassName + '.' + Uppercase(pDelem.Name) + '"'), 22), j]));
                                Write(F, Format('%8.2f, ', [I1]));
                                if j = 1 then
                                begin // Only for 1st Terminal
                                    iNormal := PDelem.NormAmps;
                                    if iNormal > 0.0 then
                                    begin
                                        Write(F, Format('%8.2f, %10.2f', [(Cmax - iNormal), Spower * (Cmax - iNormal) / iNormal]));
                                        Write(F, Separator, Cmax / iNormal * 100.0: 8: 1);
                                    end
                                    else
                                        Write(F, Separator, '     0.0');
                                    iEmerg := PDelem.EmergAmps;
                                    if iEmerg > 0.0 then
                                        Write(F, Separator, Cmax / iEmerg * 100.0: 8: 1)
                                    else
                                        Write(F, Separator, '     0.0');
                                end
                                else
                                    Write(F, Separator, '       0', Separator, '       0');
                                Write(F, Separator, I2: 8: 1);
                                if I1 > 0.0 then
                                    Write(F, Separator, 100.0 * I2 / I1: 8: 1)
                                else
                                    Write(F, Separator, '0.0');
                                Write(F, Separator, I0: 8: 1);
                                if I1 > 0.0 then
                                    Write(F, Separator, 100.0 * I0 / I1: 8: 1)
                                else
                                    Write(F, Separator, '0.0');
                                Writeln(F);
                            end;

                    end;
                end;
            PDelem := DSS.ActiveCircuit.PDElements.Next;
        end;

        DSS.GlobalResult := FileNm;


    finally
        if Assigned(Cbuffer) then
            Freemem(cBuffer);
        CloseFile(F);

    end;
end;

procedure ExportUnserved(DSS: TDSSContext; FileNm: String; UE_Only: Boolean);

var
    F: TextFile;
    PLoad: TLoadObj;
    DoIt: Boolean;

begin

    try
        Assignfile(F, FileNm);
        ReWrite(F);

        Writeln(F, 'Load, Bus, kW, EEN_Factor,  UE_Factor');

     // Load
        pLoad := DSS.ActiveCircuit.Loads.First;
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
                    Write(F, Uppercase(pLoad.Name), ', ');
                    Write(F, pLoad.GetBus(1), ', ');
                    Write(F, pLoad.kWBase: 8: 0, ', ');
                    Write(F, pLoad.EEN_Factor: 9: 3, ', ');
                    Write(F, pLoad.UE_Factor: 9: 3);
                    Writeln(F);
                end;

            end;
            pLoad := DSS.ActiveCircuit.Loads.Next;
        end;

        DSS.GlobalResult := FileNm;

    finally

        CloseFile(F);

    end;

end;

procedure ExportYprim(DSS: TDSSContext; FileNm: String);

{Exports  YPrim matrices for all  Circuit Elements}

var
    F: TextFile;
    i, j, k: Integer;
    cValues: pComplexArray;

begin

    if DSS.ActiveCircuit = NIL then
        Exit;

    try
        Assignfile(F, FileNm);
        ReWrite(F);

        with DSS.ActiveCircuit do
        begin
            for k := 1 to NumDevices do
            begin
                ActiveCktElement := CktElements.Get(k);
                if ActiveCktElement.Enabled then
                begin
                    if (ActiveCktElement is TPDElement) or (ActiveCktElement is TPCElement) then
                        with ActiveCktElement do
                        begin
                            Writeln(F, ParentClass.Name, '.', Uppercase(Name));
                            cValues := GetYprimValues(ALL_YPRIM);
                            for i := 1 to Yorder do
                            begin
                                for j := 1 to Yorder do
                                    Write(F, Format('%-13.10g, %-13.10g, ', [cValues^[i + (j - 1) * Yorder].re, cValues^[i + (j - 1) * Yorder].im]));
                                Writeln(F);
                            end;
                        end;
                end;
            end;
        end;


        DSS.GlobalResult := FileNm;

    finally

        CloseFile(F);

    end;

end;

// illustrate retrieval of System Y using compressed column format
procedure ExportY(DSS: TDSSContext; FileNm: String; TripletOpt: Boolean);

{Exports System Y Matrix in Node Order}

var
    F: TextFile;
    i, j, p: Longword;
    col, row: Longword;
    hY: NativeUInt;
    nNZ, nBus: Longword;
    ColPtr, RowIdx: array of Longword;
    cVals: array of Complex;
    re, im: Double;

begin

    if DSS.ActiveCircuit = NIL then
        Exit;
    hY := DSS.ActiveCircuit.Solution.hY;
    if hY <= 0 then
    begin
        DoSimpleMsg(DSS, 'Y Matrix not Built.', 222);
        Exit;
    end;
  // this compresses the entries if necessary - no extra work if already solved
    FactorSparseMatrix(hY);
    GetNNZ(hY, @nNZ);
    GetSize(hY, @nBus); // we should already know this

    try
        Assignfile(F, FileNm);
        ReWrite(F);

        if TripletOpt then
        begin
            SetLength(ColPtr, nNZ);
            SetLength(RowIdx, nNZ);
            SetLength(cVals, nNZ);
            GetTripletMatrix(hY, nNZ, @RowIdx[0], @ColPtr[0], @cVals[0]);
            Writeln(F, 'Row,Col,G,B');
            for i := 0 to nNZ - 1 do
            begin
                col := ColPtr[i] + 1;
                row := RowIdx[i] + 1;
                if row >= col then
                begin
                    re := cVals[i].re;
                    im := cVals[i].im;
                    Writeln(F, Format('%d,%d,%.10g,%.10g', [row, col, re, im]));
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
            with DSS.ActiveCircuit do
            begin
                Writeln(F, Format('%d, ', [NumNodes]));
  (*        For i := 1 to NumNodes DO BEGIN
             j :=  MapNodeToBus^[i].BusRef;
             Write(F, Format('%s.%-d, +j,',[BusList.Get(j), MapNodeToBus^[i].NodeNum]));
          END;
          Writeln(F);
  *)
                for i := 1 to NumNodes do
                begin
                    j := MapNodeToBus^[i].BusRef;
                    Write(F, Format('"%s.%-d", ', [Uppercase(BusList.Get(j)), MapNodeToBus^[i].NodeNum]));
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
                        Write(F, Format('%-13.10g, +j %-13.10g,', [re, im]));
                    end;
                    Writeln(F);
                end;
            end;
        end;

        DSS.GlobalResult := FileNm;
    finally
        CloseFile(F);
    end;
end;

procedure ExportSeqZ(DSS: TDSSContext; FileNm: String);

// Export Symmetrical Component Impedances at each bus

var
    F: TextFile;
    i: Integer;
    Z1, Z0: Complex;
    X1R1, X0R0: Double;


begin

    try
        Assignfile(F, FileNm);
        ReWrite(F);

        Writeln(F, 'Bus,  NumNodes, R1, X1, R0, X0, Z1, Z0, "X1/R1", "X0/R0"');
        with DSS.ActiveCircuit do
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

                Writeln(F,
                    Format('"%s", %d, %10.6g, %10.6g, %10.6g, %10.6g, %10.6g, %10.6g, %8.4g, %8.4g',
                    [Uppercase(BusList.Get(i)), Buses^[i].NumNodesThisBus,
                    Z1.re, Z1.im, Z0.Re, Z0.im, Cabs(Z1), Cabs(Z0), X1R1, X0R0]
                    ));

            end;
        end;


        DSS.GlobalResult := FileNm;

    finally

        CloseFile(F);
    end;

end;

procedure ExportUuids(DSS: TDSSContext; FileNm: String);
var
    F: TextFile;
    clsCode: TLineCode;
    clsGeom: TLineGeometry;
    clsWire: TWireData;
    clsXfmr: TXfmrCode;
    pName: TNamedObject;
begin
    try
        clsCode := DSS.DSSClassList.Get(DSS.ClassNames.Find('linecode'));
        clsWire := DSS.DSSClassList.Get(DSS.ClassNames.Find('wiredata'));
        clsGeom := DSS.DSSClassList.Get(DSS.ClassNames.Find('linegeometry'));
        clsXfmr := DSS.DSSClassList.Get(DSS.ClassNames.Find('xfmrcode'));

        Assignfile(F, FileNm);
        ReWrite(F);

        pName := DSS.ActiveCircuit;
        Writeln(F, Format('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));

        pName := DSS.ActiveCircuit.CktElements.First;
        while pName <> NIL do
        begin
            Writeln(F, Format('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));
            pName := DSS.ActiveCircuit.CktElements.Next;
        end;

        pName := clsCode.ElementList.First;
        while pName <> NIL do
        begin
            Writeln(F, Format('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));
            pName := clsCode.ElementList.Next;
        end;

        pName := clsWire.ElementList.First;
        while pName <> NIL do
        begin
            Writeln(F, Format('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));
            pName := clsWire.ElementList.Next;
        end;

        pName := clsGeom.ElementList.First;
        while pName <> NIL do
        begin
            Writeln(F, Format('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));
            pName := clsGeom.ElementList.Next;
        end;

        pName := clsXfmr.ElementList.First;
        while pName <> NIL do
        begin
            Writeln(F, Format('%s.%s %s', [pName.DSSClassName, pName.LocalName, pName.ID]));
            pName := clsXfmr.ElementList.Next;
        end;

    finally
        CloseFile(F);
    end;
end;

procedure ExportCounts(DSS: TDSSContext; FileNm: String);
var
    F: TextFile;
    cls: TDSSClass;
begin
    try
        Assignfile(F, FileNm);
        ReWrite(F);
        Writeln(F, 'Format: DSS Class Name = Instance Count');
        Writeln(F);
        cls := DSS.DSSClassList.First;
        while cls <> NIL do
        begin
            Writeln(F, Format('%s = %d', [cls.Name, cls.ElementCount]));
            cls := DSS.DSSClassList.Next;
        end;
    finally
        CloseFile(F);
    end;
end;

procedure ExportSummary(DSS: TDSSContext; FileNm: String);
var
    F: TextFile;
    cPower, cLosses: Complex;

begin
    try

        Assignfile(F, FileNm);
        if FileExists(FileNm) then
            Append(F)
        else
        begin    // Create and write the header
            ReWrite(F);
            Write(F, 'DateTime, CaseName, ');
            Write(F, 'Status, Mode, Number, LoadMult, NumDevices, NumBuses, NumNodes');
            Write(F, ', Iterations, ControlMode, ControlIterations');
            Write(F, ', MostIterationsDone');
            if DSS.ActiveCircuit <> NIL then
                if DSS.ActiveCircuit.Issolved and not DSS.ActiveCircuit.BusNameRedefined then
                begin
                    Write(F, ', Year, Hour, MaxPuVoltage, MinPuVoltage, TotalMW, TotalMvar');
                    Write(F, ', MWLosses, pctLosses, MvarLosses, Frequency');
                end;

            Writeln(F);
        end;

        Write(F, Format('"%s", ', [DateTimeToStr(Now)]));
        if DSS.ActiveCircuit <> NIL then
            Write(F, Format('%s, ', [DSS.ActiveCircuit.CaseName]))
        else
            Write(F, 'NONE, ');

        if DSS.ActiveCircuit.Issolved then
            Write(F, 'SOLVED')
        else
            Write(F, 'UnSolved');

        Write(F, Format(', %s', [GetSolutionModeID(DSS)]));
        Write(F, Format(', %d', [DSS.ActiveCircuit.Solution.NumberofTimes]));
        Write(F, Format(', %8.3f', [DSS.ActiveCircuit.LoadMultiplier]));
        Write(F, Format(', %d', [DSS.ActiveCircuit.NumDevices]));
        Write(F, Format(', %d', [DSS.ActiveCircuit.NumBuses]));
        Write(F, Format(', %d', [DSS.ActiveCircuit.NumNodes]));
        Write(F, Format(', %d', [DSS.ActiveCircuit.Solution.Iteration]));
        Write(F, Format(', %s', [GetControlModeID(DSS)]));
        Write(F, Format(', %d', [DSS.ActiveCircuit.Solution.ControlIteration]));
        Write(F, Format(', %d', [DSS.ActiveCircuit.Solution.MostIterationsDone]));
        if DSS.ActiveCircuit <> NIL then
            if DSS.ActiveCircuit.Issolved and not DSS.ActiveCircuit.BusNameRedefined then
            begin
                Write(F, Format(', %d', [DSS.ActiveCircuit.Solution.Year]));
                Write(F, Format(', %d', [DSS.ActiveCircuit.Solution.DynaVars.intHour]));
                Write(F, Format(', %-.5g', [GetMaxPUVoltage(DSS)]));
                Write(F, Format(', %-.5g', [GetMinPUVoltage(DSS, TRUE)]));
                cPower := CmulReal(GetTotalPowerFromSources(DSS), 0.000001);  // MVA
                Write(F, Format(', %-.6g', [cPower.re]));
                Write(F, Format(', %-.6g', [cPower.im]));
                cLosses := CmulReal(DSS.ActiveCircuit.Losses, 0.000001);
                if cPower.re <> 0.0 then
                    Write(F, Format(', %-.6g, %-.4g', [cLosses.re, (Closses.re / cPower.re * 100.0)]))
                else
                    Write(F, 'Total Active Losses:   ****** MW, (**** %%)');
                Write(F, Format(', %-.6g', [cLosses.im]));
                Write(F, Format(', %-g', [DSS.ActiveCircuit.Solution.Frequency]));
            end;

        Writeln(F);

        DSS.GlobalResult := FileNm;

    finally
        CloseFile(F);
    end;
end;

procedure ExportBusCoords(DSS: TDSSContext; FileNm: String);
// Export bus x, y coordinates

var
    F: TextFile;
    i: Integer;


begin

    try
        Assignfile(F, FileNm);
        ReWrite(F);

        with DSS.ActiveCircuit do
            for i := 1 to NumBuses do
            begin
                if Buses^[i].CoordDefined then
                    Writeln(F, Format('%s, %-13.11g, %-13.11g', [CheckForBlanks(Uppercase(BusList.Get(i))), Buses^[i].X, Buses^[i].Y]));
            end;

        DSS.GlobalResult := FileNm;

    finally

        CloseFile(F);
    end;
end;

procedure WriteNewLine(var F: TextFile;
    const CktELementName: String; DistFromMeter1, puV1, DistFromMeter2, puV2: Double;
    ColorCode, Thickness, LineType: Integer;
    MarkCenter: Integer;
    CenterMarkerCode, NodeMarkerCode, NodeMarkerWidth: Integer);

begin
    Write(F, Format('%s, %.6g, %.6g, %.6g, %.6g,', [Uppercase(CktElementName), DistFromMeter1, puV1, DistFromMeter2, puV2]));
    Write(F, Format('%d, %d, %d, ', [ColorCode, Thickness, LineType]));
    Write(F, Format('%d, ', [MarkCenter]));
    Write(F, Format('%d, %d, %d', [CenterMarkerCode, NodeMarkerCode, NodeMarkerWidth]));
    Writeln(F);
end;


procedure ExportProfile(DSS: TDSSContext; FileNm: String; PhasesToPlot: Integer);

var
    iEnergyMeter: Integer;
    ActiveEnergyMeter: TEnergyMeterObj;
    PresentCktElement: TDSSCktElement;
    Bus1, Bus2: TDSSbus;
    puV1, puV2: Double;
    iphs: Integer;
    iphs2: Integer;
    S: String;
    F: TextFile;
    Linetype: Integer;

begin

    try
        Assignfile(F, FileNm);
        ReWrite(F);

        Write(F, 'Name, Distance1, puV1, Distance2, puV2, Color, Thickness, Linetype, Markcenter, Centercode, NodeCode, NodeWidth,');

    {New graph created before this routine is entered}
        case phasesToPlot of
            PROFILELL, PROFILELLALL, PROFILELLPRI:
                S := 'L-L Voltage Profile';
        else
            S := 'L-N Voltage Profile';
        end;

        Writeln(F, 'Title=', S, ', Distance in km');

        iEnergyMeter := DSS.EnergyMeterClass.First;
        while iEnergyMeter > 0 do
        begin

            ActiveEnergyMeter := DSS.EnergyMeterClass.GetActiveObj;
          {Go down each branch list and draw a line}
            PresentCktElement := ActiveEnergyMeter.BranchList.First;
            while PresentCktElement <> NIL do
            begin
                if IslineElement(PresentCktElement) then
                    with DSS.ActiveCircuit do
                    begin
                        Bus1 := Buses^[PresentCktElement.Terminals^[1].BusRef];
                        Bus2 := Buses^[PresentCktElement.Terminals^[2].BusRef];
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
                                    puV1 := CABS(DSS.ActiveCircuit.Solution.NodeV^[Bus1.GetRef(Bus1.FindIdx(iphs))]) / Bus1.kVBase / 1000.0;
                                    puV2 := CABS(DSS.ActiveCircuit.Solution.NodeV^[Bus2.GetRef(Bus2.FindIdx(iphs))]) / Bus2.kVBase / 1000.0;
                                    WriteNewLine(F, PresentCktElement.Name, Bus1.DistFromMeter, puV1, Bus2.DistFromMeter, puV2,
                                        iphs, 2, Linetype, 0, 0,
                                        NodeMarkerCode, NodeMarkerWidth);
                                end;

                            end;

                    end;

                PresentCktElement := ActiveEnergyMeter.BranchList.GoForward;
            end;

            iEnergyMeter := DSS.EnergyMeterClass.Next;
        end;

        DSS.GlobalResult := FileNm;

    finally

        CloseFile(F);

    end;

end;

procedure ExportEventLog(DSS: TDSSContext; FileNm: String);
// Export the present set of EventStrings
begin
    DSS.EventStrings.SaveToFile(FileNm);
    DSS.GlobalResult := FileNm;
end;

procedure ExportErrorLog(DSS: TDSSContext; FileNm: String);
// Export the present set of ErrorStrings
begin
    DSS.ErrorStrings.SaveToFile(FileNm);
    DSS.GlobalResult := FileNm;
end;

procedure ExportIncMatrix(DSS: TDSSContext; FileNm: String);
var
    F: TextFile;
    i: Integer;
begin
    with DSS.ActiveCircuit.Solution do
    begin
        Assignfile(F, FileNm);
        ReWrite(F);
        Writeln(F, 'Row,Col,Value');
        for i := 0 to (IncMat.NZero - 1) do
        begin
            Writeln(F, inttostr(IncMat.data[i][0]) + ',' + inttostr(IncMat.data[i][1]) + ',' + inttostr(IncMat.data[i][2]));
        end;
        DSS.GlobalResult := FileNm;
        CloseFile(F);
    end;
end;

procedure ExportIncMatrixRows(DSS: TDSSContext; FileNm: String);
var
    F: TextFile;
    i: Integer;
begin
    with DSS.ActiveCircuit.Solution do
    begin
        Assignfile(F, FileNm);
        ReWrite(F);
        Writeln(F, 'B2N Incidence Matrix Row Names (PDElements)');
        for i := 0 to (length(Inc_Mat_Rows) - 1) do
        begin
            Writeln(F, Inc_Mat_Rows[i]);
        end;
        DSS.GlobalResult := FileNm;
        CloseFile(F);
    end;
end;
//-------------------------------------------------------------------
procedure ExportIncMatrixCols(DSS: TDSSContext; FileNm: String);
var
    F: TextFile;
    i: Integer;
begin
    with DSS.ActiveCircuit.Solution do
    begin
        Assignfile(F, FileNm);
        ReWrite(F);
        Writeln(F, 'B2N Incidence Matrix Column Names (Buses)');
        for i := 0 to (length(Inc_Mat_Cols) - 1) do
        begin
            Writeln(F, Inc_Mat_Cols[i]);
        end;
        DSS.GlobalResult := FileNm;
        CloseFile(F);
    end;
end;
//-------------------------------------------------------------------
procedure ExportBusLevels(DSS: TDSSContext; FileNm: String);
var
    F: TextFile;
    i: Integer;
begin
    with DSS.ActiveCircuit.Solution do
    begin
        Assignfile(F, FileNm);
        ReWrite(F);
        Writeln(F, 'B2N Incidence Matrix Column Names (Buses) and their level within the matrix');
        Writeln(F, 'Bus Name,Bus Level');
        for i := 0 to (length(Inc_Mat_Cols) - 1) do
        begin
            Writeln(F, Inc_Mat_Cols[i] + ',' + inttostr(Inc_Mat_levels[i]));
        end;
        DSS.GlobalResult := FileNm;
        CloseFile(F);
    end;
end;
//-------------------------------------------------------------------
procedure ExportLaplacian(DSS: TDSSContext; FileNm: String);
var
    F: TextFile;
    i: Integer;
begin
    with DSS.ActiveCircuit.Solution do
    begin
        Assignfile(F, FileNm);
        ReWrite(F);
        Writeln(F, 'Row,Col,Value');
        for i := 0 to (Laplacian.NZero - 1) do
        begin
            Writeln(F, inttostr(Laplacian.data[i][0]) + ',' + inttostr(Laplacian.data[i][1]) + ',' + inttostr(Laplacian.data[i][2]));
        end;
        DSS.GlobalResult := FileNm;
        CloseFile(F);
    end;
end;
//-------------------------------------------------------------------
{$IFDEF DSS_CAPI_PM}
procedure ExportZLL(DSS: TDSSContext; FileNm: String);
var
    F: TextFile;
    i: Integer;
begin
    if DSS.ADiakoptics then
    begin
        with DSS.ActiveCircuit, DSS.ActiveCircuit.Solution do
        begin
            Assignfile(F, FileNm);
            ReWrite(F);
            Writeln(F, 'Row,Col,Value(Real), Value(Imag)');
            for i := 0 to (ZLL.NZero - 1) do
            begin
                Writeln(F, inttostr(ZLL.CData[i].Row) + ',' + inttostr(ZLL.CData[i].Col) + ',' + floattostr(ZLL.CData[i].Value.Re) + ',' + floattostr(ZLL.CData[i].Value.Im));
            end;
            DSS.GlobalResult := FileNm;
            CloseFile(F);
        end;
    end;
end;
//-------------------------------------------------------------------
procedure ExportZCC(DSS: TDSSContext; FileNm: String);
var
    F: TextFile;
    i: Integer;
begin
    if DSS.ADiakoptics then
    begin
        with DSS.ActiveCircuit, DSS.ActiveCircuit.Solution do
        begin
            Assignfile(F, FileNm);
            ReWrite(F);
            Writeln(F, 'Row,Col,Value(Real), Value(Imag)');
            for i := 0 to (ZCC.NZero - 1) do
            begin
                Writeln(F, inttostr(ZCC.CData[i].Row) + ',' + inttostr(ZCC.CData[i].Col) + ',' + floattostr(ZCC.CData[i].Value.Re) + ',' + floattostr(ZCC.CData[i].Value.Im));
            end;
            DSS.GlobalResult := FileNm;
            CloseFile(F);
        end;
    end;
end;
//-------------------------------------------------------------------
procedure ExportY4(DSS: TDSSContext; FileNm: String);
var
    F: TextFile;
    i: Integer;
begin
    if DSS.ADiakoptics then
    begin
        with DSS.ActiveCircuit, DSS.ActiveCircuit.Solution do
        begin
            Assignfile(F, FileNm);
            ReWrite(F);
            Writeln(F, 'Row,Col,Value(Real), Value(Imag)');
            for i := 0 to (Y4.NZero - 1) do
            begin
                Writeln(F, inttostr(Y4.CData[i].Row) + ',' + inttostr(Y4.CData[i].Col) + ',' + floattostr(Y4.CData[i].Value.Re) + ',' + floattostr(Y4.CData[i].Value.Im));
            end;
            DSS.GlobalResult := FileNm;
            CloseFile(F);
        end;
    end;
end;
//-------------------------------------------------------------------
procedure ExportC(DSS: TDSSContext; FileNm: String);
var
    F: TextFile;
    i: Integer;
begin
    if DSS.ADiakoptics then
    begin
        with DSS.ActiveCircuit, DSS.ActiveCircuit.Solution do
        begin
            Assignfile(F, FileNm);
            ReWrite(F);
            Writeln(F, 'Row,Col,Value');
            for i := 0 to (Contours.NZero - 1) do
            begin
                Writeln(F, inttostr(Contours.CData[i].Row) + ',' + inttostr(Contours.CData[i].Col) + ',' + floattostr(Contours.CData[i].Value.Re));
            end;
            DSS.GlobalResult := FileNm;
            CloseFile(F);
        end;
    end;
end;
{$ENDIF}
//-------------------------------------------------------------------
procedure ExportVoltagesElements(DSS: TDSSContext; FileNm: String);

// Export element voltages, by terminal and node/bus

var
    MaxNumNodes: Integer;
    MaxNumTerminals: Integer;
    F: TextFile;
    i, j: Integer;
    pElem: TDSSCktElement;

begin

    MaxNumTerminals := 2;
    MaxNumNodes := 0;
    pElem := DSS.ActiveCircuit.CktElements.First;
    while pElem <> NIL do
    begin
        MaxNumTerminals := max(MaxNumTerminals, pElem.NTerms);
        MaxNumNodes := max(MaxNumNodes, pElem.NConds);
        pElem := DSS.ActiveCircuit.CktElements.Next;
    end;
{
    MaxNumNodes := 0;
    With DSS.ActiveCircuit Do
    For j := 1 to NumBuses Do
       MaxNumNodes := max(MaxNumNodes, Buses^[j].NumNodesThisBus);
}

    try
        Assignfile(F, FileNm);
        ReWrite(F);

        Write(F, 'Element,NumTerminals');

       //Write out the header
        for i := 1 to MaxNumTerminals do
        begin
            Write(F, Format(', Terminal%d', [i]));
            Write(F, ',NumConductors,NPhases,');
            Write(F, 'Bus, BasekV');
            for j := 1 to MaxNumNodes do
                Write(F, Format(', Node%d_%d, Magnitude%d_%d, Angle%d_%d, pu%d_%d', [i, j, i, j, i, j, i, j]));
        end;

        Writeln(F);

       //Go through all the sources
        with DSS.ActiveCircuit do
        begin
            pElem := sources.First;

            while pElem <> NIL do
            begin
                if pElem.Enabled then
                begin
                    WriteElementVoltagesExportFile(DSS, F, pElem, MaxNumNodes);
                    Writeln(F);
                end;
                pElem := DSS.ActiveCircuit.sources.Next;
            end;


       //Go through all the PDElements
            pElem := DSS.ActiveCircuit.PDElements.First;

            while pElem <> NIL do
            begin
                if pElem.Enabled then
                begin
                    WriteElementVoltagesExportFile(DSS, F, pElem, MaxNumNodes);
                    Writeln(F);
                end;
                pElem := DSS.ActiveCircuit.PDElements.Next;
            end;


     //Go through all the PCElements
            pElem := DSS.ActiveCircuit.PCElements.First;

            while pElem <> NIL do
            begin
                if pElem.Enabled then
                begin
                    WriteElementVoltagesExportFile(DSS, F, pElem, MaxNumNodes);
                    Writeln(F);
                end;
                pElem := DSS.ActiveCircuit.PCElements.Next;
            end;
        end;

        DSS.GlobalResult := FileNm;

    finally

        CloseFile(F);

    end;

end;
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure ExportGICMvar(DSS: TDSSContext; FileNm: String);

var
    F: TextFile;
    pElem: TGICTransformerObj;
    GICClass: TGICTransformer;

begin


    try
        Assignfile(F, FileNm);
        ReWrite(F);

        GICClass := TGICTransformer(GetDSSClassPtr(DSS, 'GICTransformer'));

        Writeln(F, 'Bus, Mvar, GIC Amps per phase');
        pElem := TGICTransformerObj(GICClass.ElementList.First);
        while PElem <> NIL do
        begin
            pElem.WriteVarOutputRecord(F);
            pElem := TGICTransformerObj(GICClass.ElementList.Next);
        end;

        DSS.GlobalResult := FileNm;

    finally
        CloseFile(F);
    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportBusReliability(DSS: TDSSContext; FileNm: String);
var
    F: TextFile;
    i: Integer;

begin

    try
        Assignfile(F, FileNm);
        ReWrite(F);
        Writeln(F, 'Bus, Lambda, Num-Interruptions, Num-Customers, Cust-Interruptions, Duration, Total-Miles');
        with DSS.ActiveCircuit do
            for i := 1 to NumBuses do
                with Buses^[i] do
                begin
                    Writeln(F, Format('%s, %-.11g, %-.11g, %d, %-.11g, %-.11g, %-.11g',
                        [CheckForBlanks(Uppercase(BusList.Get(i))), BusFltRate, Bus_Num_Interrupt, BusTotalNumCustomers, BusCustInterrupts, Bus_Int_Duration, BusTotalMiles]));
                end;

        DSS.GlobalResult := FileNm;

    finally

        CloseFile(F);
    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportBranchReliability(DSS: TDSSContext; FileNm: String);
var
    F: TextFile;
    pElem: TPDElement;
    pBus: TDSSBus;
    SAIFI: Double;
    MaxCustomers: Integer;

begin

    try
        Assignfile(F, FileNm);
        ReWrite(F);
        Writeln(F, 'Element, Lambda, "Accumulated-Lambda", Num-Customers, Total-Customers, Num-Interrupts, Cust-Interruptions, Cust-Durations, Total-Miles, Cust-Miles, SAIFI');
        with DSS.ActiveCircuit do
        begin

     // Find Maxcustomers of any PDElement for Duke Recloser siting algorithm
            MaxCustomers := 0;
            pElem := DSS.ActiveCircuit.PDElements.First;
            while pElem <> NIL do
            begin
                if pElem.Enabled then
                    with pElem do
                    begin
                        pBus := Buses^[Terminals^[FromTerminal].BusRef];
                        with pBus do
                            if BusTotalNumCustomers > MaxCustomers then
                                MaxCustomers := BusTotalNumCustomers;
                    end;
                pElem := DSS.ActiveCircuit.PDElements.Next;
            end;


     // write report for PDELEMENTS only
            pElem := DSS.ActiveCircuit.PDElements.First;
            while pElem <> NIL do
            begin
                if pElem.Enabled then
                    with pElem do
                    begin
                        pBus := Buses^[Terminals^[FromTerminal].BusRef];
                        with pBus do
                            if BusTotalNumCustomers > 0 then
                                SAIFI := BusCustInterrupts / BusTotalNumCustomers
                            else
                                SAIFI := 0.0;

                        Writeln(F, Format('%s.%s, %-.11g, %-.11g, %d, %d, %-.11g, %-.11g, %-.11g, %-.11g, %-.11g, %-.11g',
                            [ParentClass.Name, Name, BranchFltRate, AccumulatedBrFltRate, BranchNumCustomers, BranchTotalCustomers,
                            pBus.Bus_Num_Interrupt, BranchTotalCustomers * pBus.Bus_Num_Interrupt, pBus.BusCustDurations,
                            AccumulatedMilesDownStream, (MaxCustomers - BranchTotalCustomers) * AccumulatedMilesDownStream, SAIFI]));
                    end;
                pElem := DSS.ActiveCircuit.PDElements.Next;
            end;
        end;


        DSS.GlobalResult := FileNm;

    finally

        CloseFile(F);
    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ExportNodeNames(DSS: TDSSContext; FileNm: String);
var
    F: TextFile;
    i: Integer;
    j: Integer;
    BusName: String;

begin

    try
        Assignfile(F, FileNm);
        ReWrite(F);
        Writeln(F, 'Node_Name');
        with DSS.ActiveCircuit do
        begin

            for i := 1 to NumBuses do
            begin
                BusName := BusList.Get(i);
                with Buses^[i] do
                    for j := 1 to NumNodesThisBus do
                    begin
                        Writeln(F, Format('%s.%d ', [BusName, GetNum(j)]));
                    end;
            end;

        end;


        DSS.GlobalResult := FileNm;

    finally

        CloseFile(F);
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
procedure ExportTaps(DSS: TDSSContext; FileNm: String);
var
    F: TextFile;
    iWind: Integer;
    pReg: TRegControlObj;
begin

    try

        Assignfile(F, FileNm);
        ReWrite(F);
        Writeln(F, 'Name, Tap, Min, Max, Step, Position');

        with DSS.ActiveCircuit do
        begin
            pReg := RegControls.First;
            while pReg <> NIL do
            begin
                with pReg.Transformer do
                begin
                    iWind := pReg.TrWinding;
                    Write(F, Name);
                    Writeln(F, Format(', %8.5f, %8.5f, %8.5f, %8.5f, %d', [PresentTap[iWind], MinTap[iWind], MaxTap[iWind], TapIncrement[iWind], TapPosition(pREg.Transformer, iWind)]));
                end;
                pReg := RegControls.Next;
            end;
        end;


        DSS.GlobalResult := FileNm;

    finally

        CloseFile(F);
    end;

end;

procedure ExportResult(DSS: TDSSContext; FileNm: String);

var
    F: TextFile;

begin

    try
        Assignfile(F, FileNm);
        ReWrite(F);
        DSS.ParserVars.Lookup('@result');
        Writeln(F, DSS.ParserVars.Value);

        DSS.GlobalResult := FileNm;

    finally

        CloseFile(F);
    end;


end;

procedure ExportYNodeList(DSS: TDSSContext; FileNm: String);
var
    i: Integer;
    F: TextFile;

begin

    try
        Assignfile(F, FileNm);
        ReWrite(F);

        if DSS.ActiveCircuit <> NIL then
            with DSS.ActiveCircuit do
            begin
                for i := 1 to NumNodes do
                begin
                    with MapNodeToBus^[i] do
                        Writeln(F, Format('"%s.%-d"', [Uppercase(BusList.Get(Busref)), NodeNum]));
                end;
            end;

        DSS.GlobalResult := FileNm;

    finally
        CloseFile(F);
    end;

end;

procedure ExportYVoltages(DSS: TDSSContext; FileNm: String);
var
    i: Integer;
    F: TextFile;

begin

    try
        Assignfile(F, FileNm);
        ReWrite(F);

        if DSS.ActiveCircuit <> NIL then
            with DSS.ActiveCircuit do
            begin
                for i := 1 to NumNodes do
                begin
                    with solution.NodeV^[i] do
                        Writeln(F, Format('%10.6g, %10.6g', [re, im]));
                end;
            end;

        DSS.GlobalResult := FileNm;

    finally
        CloseFile(F);
    end;


end;

procedure ExportYCurrents(DSS: TDSSContext; FileNm: String);
var
    i: Integer;
    F: TextFile;

begin

    try
        Assignfile(F, FileNm);
        ReWrite(F);

        if DSS.ActiveCircuit <> NIL then
            with DSS.ActiveCircuit do
            begin
                for i := 1 to NumNodes do
                begin
                    with solution.Currents^[i] do
                        Writeln(F, Format('%10.6g, %10.6g', [re, im]));
                end;
            end;

        DSS.GlobalResult := FileNm;

    finally
        CloseFile(F);
    end;

end;


procedure ExportSections(DSS: TDSSContext; FileNM: String; pMeter: TEnergyMeterObj);

var
    MyMeterPtr: TEnergyMeterObj;
    iMeter, i: Integer;
    F: TextFile;


begin

    try
        Assignfile(F, FileNm);
        ReWrite(F);

     // Write Header
        Writeln(F, 'Meter, SectionID, SeqIndex, DeviceType, NumCustomers, NumBranches, AvgRepairHrs, TotalDownlineCust, SectFaultRate, SumFltRatesXRepairHrs, SumBranchFltRates, HeadBranch ');

        if Assigned(pMeter) then
     // If a meter is specified, export that meter only
            with pMeter do
            begin
                for i := 1 to SectionCount do
                    with FeederSections^[i] do
                    begin
                        DSS.ActiveCircuit.ActiveCktElement := TDSSCktElement(sequenceList.Get(SeqIndex));
                        Writeln(F, Format('%s, %d, %d, %s, %d, %d, %-.6g, %d, %-.6g, %-.6g, %-.6g, %s',
                            [Name, i, SeqIndex, GetOCPDeviceTypeString(OCPDeviceType), NCustomers, NBranches, AverageRepairTime, TotalCustomers, SectFaultRate, SumFltRatesXRepairHrs, SumBranchFltRates,
                            FullName(DSS.ActiveCircuit.ActiveCktElement)]));
                    end;
            end
        else    // export sections for all meters
        begin

            iMeter := DSS.EnergyMeterClass.First;
            while iMeter > 0 do
            begin
                MyMeterPtr := DSS.EnergyMeterClass.GetActiveObj;
                with MyMeterPtr do
                begin
                    for i := 1 to SectionCount do
                        with FeederSections^[i] do
                        begin
                            DSS.ActiveCircuit.ActiveCktElement := TDSSCktElement(sequenceList.Get(SeqIndex));
                            Writeln(F, Format('%s, %d, %d, %s, %d, %d, %-.6g, %d, %-.6g, %-.6g, %-.6g, %s',
                                [Name, i, SeqIndex, GetOCPDeviceTypeString(OCPDeviceType), NCustomers, NBranches, AverageRepairTime, TotalCustomers, SectFaultRate, SumFltRatesXRepairHrs, SumBranchFltRates,
                                FullName(DSS.ActiveCircuit.ActiveCktElement)]));
                        end;
                end;
                iMeter := DSS.EnergyMeterClass.Next;
            end;

        end;

        DSS.GlobalResult := FileNm;

    finally
        CloseFile(F);
    end;

end;


end.