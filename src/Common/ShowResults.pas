unit ShowResults;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015,  Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses 
    DSSClass;

procedure ShowVoltages(DSS: TDSSContext; FileNm: String; LL: Boolean; ShowOptionCode: Integer);
procedure ShowCurrents(DSS: TDSSContext; FileNm: String; ShowResidual: Boolean; ShowOptionCode: Integer);
procedure ShowPowers(DSS: TDSSContext; FileNm: String; opt, ShowOptionCode: Integer);
procedure ShowBusPowers(DSS: TDSSContext; FileNm, BusName: String; opt, ShowOptionCode: Integer);
procedure ShowFaultStudy(DSS: TDSSContext; FileNm: String);
procedure ShowElements(DSS: TDSSContext; FileNm: String; ClassName: String);
procedure ShowBuses(DSS: TDSSContext; FileNm: String);
procedure ShowMeters(DSS: TDSSContext; FileNm: String);
procedure ShowGenMeters(DSS: TDSSContext; FileNm: String);
procedure ShowMeterZone(DSS: TDSSContext; FileNm: String);
procedure ShowLosses(DSS: TDSSContext; FileNm: String);
procedure ShowRegulatorTaps(DSS: TDSSContext; FileNm: String);
procedure ShowOverloads(DSS: TDSSContext; FileNm: String);
procedure ShowUnserved(DSS: TDSSContext; FileNm: String; UE_Only: Boolean);
procedure ShowVariables(DSS: TDSSContext; FileNm: String);
procedure ShowIsolated(DSS: TDSSContext; FileNm: String);
procedure ShowRatings(DSS: TDSSContext; FileNm: String);
procedure ShowLoops(DSS: TDSSContext; FileNm: String);
procedure ShowLineConstants(DSS: TDSSContext; FileNm: String; Freq: Double; Units: Integer; Rho: Double);
procedure ShowYPrim(DSS: TDSSContext; FileNm: String);
procedure ShowY(DSS: TDSSContext; FileNm: String);
procedure ShowTopology(DSS: TDSSContext; FileRoot: String); // summary and tree-view to separate files
procedure ShowNodeCurrentSum(DSS: TDSSContext; FileNm: String);
procedure ShowkVBaseMismatch(DSS: TDSSContext; FileNm: String);
procedure ShowDeltaV(DSS: TDSSContext; FileNm: String);
procedure ShowControlledElements(DSS: TDSSContext; FileNm: String);
procedure ShowResult(DSS: TDSSContext; FileNm: String);
procedure ShowEventLog(DSS: TDSSContext; FileNm: String);

implementation

uses
    Classes,
    BufStream,
    UComplex, DSSUcomplex,
    Arraydef,
    sysutils,
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
    Transformer,
    EnergyMeter,
    Load,
    RegControl,
    ParserDel,
    CktTree,
    CmdForms,
    Math,
    Line,
    LineUnits,
    LineGeometry,
    YMatrix,
    SwtControl,
    KLUSolve,
    DSSHelper;

var
    MaxBusNameLength: Integer;
    MaxDeviceNameLength: Integer;

const
    TABCHAR: Char = chr(9);

procedure SetMaxBusNameLength(DSS: TDSSContext);
var
    i: Integer;
begin
    MaxBusNameLength := 4;
    with DSS.ActiveCircuit do
        for i := 1 to NumBuses do
            MaxBusNameLength := Max(MaxBusNameLength, Length(BusList.NameOfIndex(i)));
end;

procedure SetMaxDeviceNameLength(DSS: TDSSContext);
var
    i: Integer;
    element: TDSSCktElement;
begin
    MaxDeviceNameLength := 0;
    with DSS.ActiveCircuit do
        for i := 1 to NumDevices do
        begin
            element := TDSSCktElement(CktElements.At(i));
            MaxDeviceNameLength := Max(MaxDeviceNameLength, (Length(element.Name) + Length(element.ParentClass.Name) + 1));
        end;
end;

procedure WriteSeqVoltages(DSS: TDSSContext; F: TFileStream; i: Integer; LL: Boolean);

var
    j, k: Integer;
    Vph, VLL, V012: Complex3;
    V0, V1, V2,
    Vpu, V2V1, V0V1: Double;


begin
    with DSS.ActiveCircuit do
    begin
        if Buses^[i].NumNodesThisBus >= 3 then
        begin
            // compute sequence voltages for Nodes 1, 2, and 3 only

            with Buses^[i] do
                for j := 1 to 3 do
                    Vph[j] := Solution.NodeV^[GetRef(FindIdx(j))];

            if LL then
            begin
                for j := 1 to 3 do
                begin
                    k := j + 1;
                    if k > 3 then
                        k := 1;
                    VLL[j] := Vph[j] - Vph[k];
                end;
                Phase2SymComp(@VLL, @V012);
            end
            else
            begin
                Phase2SymComp(@Vph, @V012);
            end;
            V0 := Cabs(V012[1]);
            V1 := Cabs(V012[2]);
            V2 := Cabs(V012[3]);
        end

        else
        begin
            Vph[1] := DSS.ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(1)];
            V0 := 0.0;
            V1 := Cabs(Vph[1]);     // Use first phase value for non-three phase buses
            V2 := 0.0;
        end;

        V1 := V1 / 1000.0;    {Convert to kV}
        V2 := V2 / 1000.0;
        V0 := V0 / 1000.0;

     // Calc per unit value
        if Buses^[i].kvbase <> 0.0 then
            Vpu := V1 / Buses^[i].kVBase
        else
            Vpu := 0.0;
        if LL then
            Vpu := Vpu / SQRT3;

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

        FSWriteln(F, Format('%s %9.4g  %9.4g  %9.4g  %9.4g %9.4g %9.4g', [Pad(BusList.NameOfIndex(i), MaxBusNameLength), V1, Vpu, V2, V2V1, V0, V0V1]));

    end; {With}


end;

procedure WriteBusVoltages(DSS: TDSSContext; F: TFileStream; i: Integer; LL: Boolean);

// 6/11/14 Modified to write both LL and LN voltages out for LN case

var
    nref1, nref2, j, k: Integer;
    Volts, VoltsLL: Complex;
    Vmag, VmagLL, Vpu, VpuLL: Double;
    Bname: String;
    NodeName, NodeNameLL: String;
    NodeIdx: Integer;
    jj, kk: Integer;

begin
    with DSS.ActiveCircuit do
    begin
        jj := 1;
        with Buses^[i] do
            for j := 1 to NumNodesThisBus do
            begin
         // Get the index of the next Node in numerical sequence

                repeat
                    NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                    inc(jj)
                until NodeIdx > 0;

                nref1 := GetRef(NodeIdx);   // Get the overall node reference number
                Volts := DSS.ActiveCircuit.Solution.NodeV^[nref1];

                kk := 1; // keep compiler from complaining
                if {LL and} (jj <= 4) then
         // Line-to-line voltages
                begin         // Convert to Line-Line assuming no more than 3 phases
              // k is 1, 2, or 3
                    k := jj;
                    if k > 3 then
                        k := 1;
                    kk := FindIdx(k);
                    if kk <= NumNodesThisBus then
                    begin
                        nref2 := Buses^[i].GetRef(kk); // reference for next phase in sequence
                        VoltsLL := Volts - DSS.ActiveCircuit.Solution.NodeV^[nref2];
                    end;
                end;

                Vmag := Cabs(Volts) * 0.001;
                VmagLL := Cabs(VoltsLL) * 0.001;
                if kvbase <> 0.0 then
                begin
                    Vpu := Vmag / kVBase;
                    VpuLL := VmagLL / kVBase / SQRT3;
                end
                else
                begin
                    Vpu := 0.0;
                    VpuLL := 0.0;
                end;
                if {LL and} (jj <= 4) then
                begin
            // Vpu := Vpu/SQRT3;
                    NodeNameLL := Format('%d-%d', [GetNum(NodeIdx), GetNum(kk)]);
                end;
                NodeName := Format('%d  ', [GetNum(NodeIdx)]);

                if j = 1 then
                    Bname := Paddots(BusList.NameOfIndex(i), MaxBusNameLength);

                if LL then
                begin
                    if kk > 0 then
                    begin
                        FSWriteln(F, Format('%s %s %10.5g /_ %6.1f %9.5g %9.3f', [AnsiUpperCase(Bname), NodeNameLL, VmagLL, cdang(VoltsLL), VpuLL, kvbase * SQRT3]));
                        Bname := Pad('   -', MaxBusNameLength);
                    end;
                end
                else
                begin
                    FSWrite(F, Format('%s %s %10.5g /_ %6.1f %9.5g %9.3f', [AnsiUpperCase(Bname), NodeName, Vmag, cdang(Volts), Vpu, kvbase * SQRT3]));
                    if (NumNodesThisBus > 1) and (kk > 0) and (jj <= 4) then
                        FSWrite(F, Format('        %s %10.5g /_ %6.1f %9.5g', [NodeNameLL, VmagLL, cdang(VoltsLL), VpuLL]));
                    FSWriteln(F);
                    BName := Pad('   -', MaxBusNameLength);
                end;
            end;
    end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure WriteElementVoltages(DSS: TDSSContext; F: TFileStream; pElem: TDSSCktElement; LL: Boolean);
var
    NCond, Nterm, i, j, k, nref, bref: Integer;
    Busname: String;
    Volts: Complex;
    Vpu, Vmag: Double;

begin
    NCond := pElem.NConds;
    Nterm := pElem.Nterms;
    k := 0;
    BusName := Pad(StripExtension(pElem.FirstBus), MaxBusNameLength);
    FSWriteln(F, 'ELEMENT = "' + pElem.dssclassname + '.' + AnsiUpperCase(pElem.Name) + '"');
    for j := 1 to NTerm do
    begin
        for i := 1 to NCond do
        begin
            Inc(k);
            nref := pElem.NodeRef^[k];
            Volts := DSS.ActiveCircuit.Solution.NodeV^[nref];
            Vmag := Cabs(Volts) * 0.001;
            with DSS.ActiveCircuit do
            begin
                if nref = 0 then
                    Vpu := 0.0
                else
                begin
                    bref := MapNodeToBus^[nref].BusRef;
                    if Buses^[bref].kvbase <> 0.0 then
                        Vpu := Vmag / Buses^[bref].kVBase
                    else
                        Vpu := 0.0;
                end;
                if LL then
                    Vpu := Vpu / SQRT3;
                FSWriteln(F, Format('%s  (%3d) %4d    %13.5g (%8.4g) /_ %6.1f', [AnsiUpperCase(BusName), nref, MapNodeToBus^[nref].nodenum, Vmag, Vpu, cdang(Volts)]));
            end;
        end;
        if j < Nterm then
            FSWriteln(F, '------------');
        BusName := Pad(StripExtension(pElem.Nextbus), MaxBusNameLength);
    end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure WriteElementDeltaVoltages(DSS: TDSSContext; F: TFileStream; pElem: TDSSCktElement);
var
    NCond,
    Node1, Node2,
    Bus1, Bus2,
    i: Integer;
    Vmag: Double;
    Volts1,
    Volts2: Complex;
    ElemName: String;

begin
    NCond := pElem.NConds;

    ElemName := Pad(pElem.dssclassname + '.' + AnsiUpperCase(pElem.Name), MaxDeviceNameLength);
    for i := 1 to NCond do
    begin
        Node1 := pElem.NodeRef^[i];
        Node2 := pElem.NodeRef^[i + Ncond];
        if Node1 > 0 then
            Bus1 := DSS.ActiveCircuit.MapNodeToBus^[Node1].BusRef
        else
            Bus1 := 0;
        if Node2 > 0 then
            Bus2 := DSS.ActiveCircuit.MapNodeToBus^[Node2].BusRef
        else
            Bus2 := 0;
        if (Bus1 > 0) and (Bus2 > 0) then
        begin
            Volts1 := DSS.ActiveCircuit.Solution.NodeV^[Node1];   // OK if Node1 or Node2 = 0
            Volts2 := DSS.ActiveCircuit.Solution.NodeV^[Node2];
            Volts1 := Volts1 - Volts2;   // diff voltage
            with DSS.ActiveCircuit do
            begin
                if Buses^[Bus1].kVBase <> Buses^[Bus2].kVBase then
                    Vmag := 0.0
                else
                begin
                    if Buses^[Bus1].kVBase > 0.0 then
                        Vmag := Cabs(Volts1) / (1000.0 * Buses^[Bus1].kVBase) * 100.0
                    else
                        Vmag := 0.0;
                end;
                FSWriteln(F, Format('%s,  %4d,    %12.5g, %12.5g, %12.5g, %6.1f', [ElemName, i, Cabs(Volts1), Vmag, Buses^[Bus1].kVBase, cdang(Volts1)]));
            end;
        end;
    end;
end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowVoltages(DSS: TDSSContext; FileNm: String; LL: Boolean; ShowOptionCode: Integer);

// Show bus voltages by circuit element terminal

var
    F: TFileStream = nil;
    i: Integer;
    pElem: TDSSCktElement;

begin
    try
        SetMaxBusNameLength(DSS);

        F := TBufferedFileStream.Create(FileNm, fmCreate);

        case ShowOptionCode of
            0:
            begin
                FSWriteln(F);
                if LL then
                    FSWriteln(F, 'SYMMETRICAL COMPONENT PHASE-PHASE VOLTAGES BY BUS (for 3-phase buses)')
                else
                    FSWriteln(F, 'SYMMETRICAL COMPONENT VOLTAGES BY BUS (for 3-phase buses)');
                FSWriteln(F);
                FSWriteln(F, pad('Bus', MaxBusNameLength), '  Mag:   V1 (kV)    p.u.     V2 (kV)   %V2/V1    V0 (kV)    %V0/V1');
                FSWriteln(F);
                for i := 1 to DSS.ActiveCircuit.NumBuses do
                    WriteSeqVoltages(DSS, F, i, LL);

            end; {ShowOptionCode Case 0}

            1:
            begin
                FSWriteln(F);
                if LL then
                    FSWriteln(F, 'LINE-LINE VOLTAGES BY BUS & NODE')
                else
                    FSWriteln(F, 'LINE-GROUND and LINE-LINE VOLTAGES BY BUS & NODE');
                FSWriteln(F);
                if LL then
                    FSWriteln(F, pad('Bus', MaxBusNameLength), ' Node    VLN (kV)   Angle      pu     Base kV ')
                else
                    FSWriteln(F, pad('Bus', MaxBusNameLength), ' Node    VLN (kV)   Angle      pu     Base kV    Node-Node   VLL (kV)  Angle      pu');
                FSWriteln(F);

                for i := 1 to DSS.ActiveCircuit.NumBuses do
                    WriteBusVoltages(DSS, F, i, LL);

            end; {ShowOptionCode Case 1}

            2:
            begin
                FSWriteln(F);
                FSWriteln(F, 'NODE-GROUND VOLTAGES BY CIRCUIT ELEMENT');
                FSWriteln(F);
                FSWriteln(F, 'Power Delivery Elements');
                FSWriteln(F);
                FSWriteln(F, pad('Bus', MaxBusNameLength), ' (node ref)  Phase    Magnitude, kV (pu)    Angle');
                FSWriteln(F);


       // SOURCES first
                pElem := DSS.ActiveCircuit.sources.First;

                while pElem <> NIL do
                begin
                    if pElem.Enabled then
                        WriteElementVoltages(DSS, F, pElem, LL);
                    FSWriteln(F);
                    pElem := DSS.ActiveCircuit.sources.Next;
                end;

       // PDELEMENTS first
                pElem := DSS.ActiveCircuit.PDElements.First;

                while pElem <> NIL do
                begin
                    if pElem.Enabled then
                        WriteElementVoltages(DSS, F, pElem, LL);
                    FSWriteln(F);
                    pElem := DSS.ActiveCircuit.PDElements.Next;
                end;

                FSWriteln(F, '= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =');
                FSWriteln(F);
                FSWriteln(F, 'Power Conversion Elements');
                FSWriteln(F);
                FSWriteln(F, pad('Bus', MaxBusNameLength), ' (node ref)  Phase    Magnitude, kV (pu)    Angle');
                FSWriteln(F);

       // PCELEMENTS next
                pElem := DSS.ActiveCircuit.PCElements.First;

                while pElem <> NIL do
                begin
                    if pElem.Enabled then
                        WriteElementVoltages(DSS, F, pElem, LL);
                    pElem := DSS.ActiveCircuit.PCElements.Next;
                    FSWriteln(F);
                end;

            end; {ShowOptionCode Case 2}
        else
       {nada}
        end;


    finally

        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);

    end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure GetI0I1I2(var I0, I1, I2, Cmax: Double; Nphases, koffset: Integer; cBuffer: pComplexArray);
var
    cmag: Double;
    i: Integer;
    Iph, I012: Complex3;

begin
    if Nphases >= 3 then
    begin
        Cmax := 0.0;
        for i := 1 to 3 do
        begin
            Iph[i] := cBuffer^[koffset + i];
            Cmag := Cabs(Iph[i]);
            if Cmag > Cmax then
                Cmax := Cmag;
        end;
        Phase2SymComp(@Iph, @I012);
        I0 := Cabs(I012[1]);
        I1 := Cabs(I012[2]);
        I2 := Cabs(I012[3]);
    end
    else
    begin
        I0 := 0.0;
        I1 := Cabs(cBuffer^[1 + koffset]);
        I2 := 0.0;
        Cmax := I1;
    end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


procedure WriteSeqCurrents(F: TFileStream; const PaddedBrName: String; I0, I1, I2, Cmax, NormAmps, EmergAmps: Double; j, DSSObjType: Integer);

var
    Inormal, Iemerg, I2I1, I0I1: Double;
    Name: String;

begin
    Inormal := 0.0;
    Iemerg := 0.0;
    if j = 1 then
        Name := PaddedBrName
    else
        Name := Pad('   -', Length(PaddedBrName));

    if I1 > 0.0 then
        I2I1 := 100.0 * I2 / I1
    else
        I2I1 := 0.0;
    if I1 > 0.0 then
        I0I1 := 100.0 * I0 / I1
    else
        I0I1 := 0.0;
    if ((CLASSMASK and DSSObjType) <> CAP_ELEMENT) and (j = 1) then
    begin    // only write overloads for non-capacitors and terminal 1
        if Normamps > 0.0 then
            Inormal := Cmax / Normamps * 100.0;
        if Emergamps > 0.0 then
            Iemerg := Cmax / Emergamps * 100.0;
    end;

    FSWriteln(F,
        Format('%s %3d  %10.5g   %10.5g %8.2f  %10.5g %8.2f  %8.2f %8.2f',
        [AnsiUpperCase(Name), j, I1, I2, I2I1, I0, I0I1, Inormal, Iemerg]));
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure WriteTerminalCurrents(DSS: TDSSContext; F: TFileStream; pElem: TDSSCktElement; ShowResidual: Boolean);

var

    j, i, k, Ncond, Nterm, Ntimes: Integer;
    cBuffer: pComplexArray;
    FromBus: String;
    Ctotal: Complex;
    ResidPolar: Polar;

begin
    cBuffer := NIL;

    NCond := pElem.NConds;
    Nterm := pElem.Nterms;

    try
        Getmem(cBuffer, sizeof(cBuffer^[1]) * Ncond * Nterm);
        pElem.GetCurrents(cBuffer);
        k := 0;
        FromBus := Pad(StripExtension(pElem.FirstBus), MaxBusNameLength);
        FSWriteln(F, 'ELEMENT = ', EncloseQuotes(Pelem.FullName));
        for      j := 1 to NTerm do
        begin
            Ctotal := CZERO;
            if (CLASSMASK and pElem.DSSObjType) = AUTOTRANS_ELEMENT then
                Ntimes := pElem.Nphases    // Special case for AutoTrans
            else 
                Ntimes := NCond;

            for i := 1 to Ntimes do
            begin
                Inc(k);
                if ShowResidual then
                    Ctotal += cBuffer^[k];
                FSWriteln(F, Format('%s  %4d    %13.5g /_ %6.1f =  %9.5g +j %9.5g', [AnsiUpperCase(FromBus), GetNodeNum(DSS, pElem.NodeRef^[k]), Cabs(cBuffer^[k]), cdang(cBuffer^[k]), cBuffer^[k].re, cBuffer^[k].im]));
            end;
            if ShowResidual and (pElem.NPhases > 1) then
            begin
                ResidPolar := CtoPolardeg(-Ctotal);
                FSWriteln(F, Format('%s Resid    %13.5g /_ %6.1f =   %9.5g +j %9.5g', [AnsiUpperCase(FromBus), ResidPolar.mag, ResidPolar.ang, -cTotal.re, -Ctotal.im]));
            end;
            if j < Nterm then
                FSWriteln(F, '------------');
            FromBus := Pad(StripExtension(pElem.Nextbus), MaxBusNameLength);
            if (CLASSMASK and pElem.DSSObjType) = AUTOTRANS_ELEMENT then 
                Inc(k, Ntimes);  // Special case for AutoTrans
        end;
        FSWriteln(F);

    finally

        if Assigned(cBuffer) then
            Freemem(cBuffer);

    end;
end;
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowCurrents(DSS: TDSSContext; FileNm: String; ShowResidual: Boolean; ShowOptionCode: Integer);


var
    F: TFileStream = nil;
    cBuffer: pComplexArray;
    NCond, Nterm, j: Integer;
    pElem: TDSSCktElement;
    PDElem: TPDElement;
    PCelem: TPCelement;
    I0, I1, I2,
    Cmax: Double;

begin
    SetMaxDeviceNameLength(DSS);
    SetMaxBusNameLength(DSS);
    try
        try

            F := TBufferedFileStream.Create(FileNm, fmCreate);
            case ShowOptionCode of

                0:
                begin  {Sequence Currents}

                    FSWriteln(F);
                    FSWriteln(F, 'SYMMETRICAL COMPONENT CURRENTS BY CIRCUIT ELEMENT (first 3 phases)');
                    FSWriteln(F);
                    FSWriteln(F, Pad('Element', maxDeviceNameLength + 2), ' Term      I1         I2         %I2/I1    I0         %I0/I1   %Normal %Emergency');
                    FSWriteln(F);


//Sources First
                    Pelem := DSS.ActiveCircuit.Sources.First;
                    while pelem <> NIL do
                    begin
                        if (pelem.Enabled) then
                        begin
                            NCond := pelem.NConds;
                            Nterm := pelem.Nterms;
                            Getmem(cBuffer, Sizeof(Complex) * NCond * Nterm);
                            pelem.GetCurrents(cBuffer);

                            for j := 1 to NTerm do
                            begin
                                GetI0I1I2(I0, I1, I2, Cmax, pelem.Nphases, (j - 1) * Ncond, cBuffer);
                                with PElem do
                                    WriteSeqCurrents(F, Paddots(EncloseQuotes(pElem.FullName), MaxDeviceNameLength + 2), I0, I1, I2, Cmax, 0.0, 0.0, j, DSSObjType);
                            end;
                            Freemem(cBuffer);
                        end;
                        pelem := DSS.ActiveCircuit.Sources.Next;
                    end;


     // PDELEMENTS Next
                    PDelem := DSS.ActiveCircuit.PDElements.First;

                    while PDelem <> NIL do
                    begin
                        if (PDelem.Enabled) then
                        begin
                            NCond := PDelem.NConds;
                            Nterm := PDelem.Nterms;
                            Getmem(cBuffer, Sizeof(cBuffer^[1]) * NCond * Nterm);
                            PDelem.GetCurrents(cBuffer);

                            for j := 1 to NTerm do
                            begin
                                GetI0I1I2(I0, I1, I2, Cmax, pDelem.Nphases, (j - 1) * Ncond, cBuffer);
                                with PDElem do
                                    WriteSeqCurrents(F, Paddots(EncloseQuotes(pdElem.FullName), MaxDeviceNameLength + 2), I0, I1, I2, Cmax, Normamps, Emergamps, j, DSSObjType);
                            end; {For}
                            Freemem(cBuffer);
                        end;
                        PDelem := DSS.ActiveCircuit.PDElements.Next;
                    end;

    // PCelemENTS next
                    PCelem := DSS.ActiveCircuit.PCelements.First;

                    while PCelem <> NIL do
                    begin
                        if (PCelem.Enabled) then
                        begin
                            NCond := PCelem.NConds;
                            Nterm := PCelem.Nterms;
                            Getmem(cBuffer, Sizeof(cBuffer^[1]) * NCond * Nterm);
                            PCelem.GetCurrents(cBuffer);

                            for j := 1 to NTerm do
                            begin
                                GetI0I1I2(I0, I1, I2, Cmax, pCelem.Nphases, (j - 1) * Ncond, cBuffer);
                                with PCElem do
                                    WriteSeqCurrents(F, Paddots(EncloseQuotes(pcElem.FullName), MaxDeviceNameLength + 2), I0, I1, I2, Cmax, 0.0, 0.0, j, DSSObjType);
                            end;
                            Freemem(cBuffer);
                        end;
                        PCelem := DSS.ActiveCircuit.PCelements.Next;
                    end;


     //Faults next
                    Pelem := DSS.ActiveCircuit.Faults.First;
                    while pelem <> NIL do
                    begin
                        if (pelem.Enabled) then
                        begin
                            NCond := pelem.NConds;
                            Nterm := pelem.Nterms;
                            Getmem(cBuffer, Sizeof(cBuffer^[1]) * NCond * Nterm);
                            pelem.GetCurrents(cBuffer);

                            for j := 1 to NTerm do
                            begin
                                GetI0I1I2(I0, I1, I2, Cmax, pelem.Nphases, (j - 1) * Ncond, cBuffer);
                                with PElem do
                                    WriteSeqCurrents(F, Paddots(EncloseQuotes(pElem.FullName), MaxDeviceNameLength + 2), I0, I1, I2, Cmax, 0.0, 0.0, j, DSSObjType);
                            end;
                            Freemem(cBuffer);
                        end;
                        pelem := DSS.ActiveCircuit.Faults.Next;
                    end;

                end; {Code 0:}

                1:
                begin  {Element branch Currents}


                    FSWriteln(F);
                    FSWriteln(F, 'CIRCUIT ELEMENT CURRENTS');
                    FSWriteln(F);
                    FSWriteln(F, '(Currents into element from indicated bus)');
                    FSWriteln(F);
                    FSWriteln(F, 'Power Delivery Elements');
                    FSWriteln(F);
                    FSWriteln(F, Pad('  Bus', MaxBusNameLength), ' Phase    Magnitude, A     Angle      (Real)   +j  (Imag)');
                    FSWriteln(F);


     // Sources first
                    pElem := DSS.ActiveCircuit.Sources.First;

                    while pElem <> NIL do
                    begin
                        if pElem.Enabled then
                            WriteTerminalCurrents(DSS, F, pElem, FALSE);
                        pElem := DSS.ActiveCircuit.Sources.Next;
                    end;

     // PDELEMENTS first
                    pElem := DSS.ActiveCircuit.PDElements.First;

                    while pElem <> NIL do
                    begin
                        if pElem.Enabled then
                            WriteTerminalCurrents(DSS, F, pElem, ShowResidual);
                        pElem := DSS.ActiveCircuit.PDElements.Next;
                    end;

     // Faults
                    pElem := DSS.ActiveCircuit.Faults.First;

                    while pElem <> NIL do
                    begin
                        if pElem.Enabled then
                            WriteTerminalCurrents(DSS, F, pElem, FALSE);
                        pElem := DSS.ActiveCircuit.Faults.Next;
                    end;


                    FSWriteln(F, '= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =');
                    FSWriteln(F);
                    FSWriteln(F, 'Power Conversion Elements');
                    FSWriteln(F);
                    FSWriteln(F, Pad('  Bus', MaxBusNameLength), ' Phase    Magnitude, A     Angle      (Real)   +j  (Imag)');
                    FSWriteln(F);

     // PCELEMENTS next
                    pElem := DSS.ActiveCircuit.PCElements.First;

                    while pElem <> NIL do
                    begin
                        if pElem.Enabled then
                            WriteTerminalCurrents(DSS, F, pElem, FALSE);
                        pElem := DSS.ActiveCircuit.PCElements.Next;
                    end;

                end;  {code:1}

            else

            end; {CASE}

        finally

            FreeAndNil(F);
            FireOffEditor(DSS, FileNm);
            DSS.ParserVars.Add('@lastshowfile', FileNm);

        end;

    except
        On E: Exception do
            DoSimpleMsg(DSS, 'Exception raised in ShowCurrents: %s', [E.Message], 2190);
    end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowPowers(DSS: TDSSContext; FileNm: String; opt, ShowOptionCode: Integer);

{Opt = 0: kVA
 opt = 1: MVA
 }

var
    FromBus: String;
    F: TFileStream = nil;
    c_Buffer: pComplexArray;
    NCond, Nterm, Ntimes, i, j, k: Integer;
    p_Elem: TDSSCktElement;
    PDElem: TPDElement;
    PCElem: TPCElement;
    Volts: Complex;
    S,
    Saccum: Complex;
    nref, nref1, nref2: Integer;
    Vph, V012: Complex3;
    Iph, I012: Complex3;
    sout: String;

begin
    c_Buffer := NIL;
    SetMaxDeviceNameLength(DSS);
    SetMaxBusNameLength(DSS);

    try
        F := TBufferedFileStream.Create(FileNm, fmCreate);

      {Allocate c_Buffer big enough for largest circuit element}
        Getmem(c_buffer, sizeof(c_Buffer^[1]) * GetMaxCktElementSize(DSS));

        case ShowOptionCode of
            0:
            begin
     {Sequence Currents}
                FSWriteln(F);
                FSWriteln(F, 'SYMMETRICAL COMPONENT POWERS BY CIRCUIT ELEMENT (first 3 phases)                                     Excess Power');
                FSWriteln(F);
                case Opt of
                    1:
                        FSWriteln(F, Pad('Element', MaxDeviceNameLength + 2), ' Term    P1(MW)   Q1(Mvar)       P2         Q2      P0      Q0       P_Norm      Q_Norm     P_Emerg    Q_Emerg');
                else
                    FSWriteln(F, Pad('Element', MaxDeviceNameLength + 2), ' Term    P1(kW)   Q1(kvar)       P2         Q2      P0      Q0       P_Norm      Q_Norm     P_Emerg    Q_Emerg');
                end;
                FSWriteln(F);

     // Sources first
                p_Elem := DSS.ActiveCircuit.Sources.First;

                while p_Elem <> NIL do
                begin
                    if (p_Elem.Enabled) then
                    begin
                        NCond := p_Elem.NConds;
                        Nterm := p_Elem.Nterms;
                        p_Elem.GetCurrents(c_Buffer);

                        for j := 1 to NTerm do
                        begin
                            FSWrite(F, Pad(EncloseQuotes(p_Elem.FullName), MaxDeviceNameLength + 2) + Format('%3d', [j]));
                            for i := 1 to min(3, p_Elem.Nphases) do
                            begin
                                k := (j - 1) * Ncond + i;
                                nref := p_Elem.NodeRef^[k];
                                Volts := DSS.ActiveCircuit.Solution.NodeV^[nref];
                                Iph[i] := c_Buffer^[k];
                                Vph[i] := volts;
                            end;
                            if (p_Elem.Nphases >= 3) then
                            begin
                                Phase2SymComp(@Iph, @I012);
                                Phase2SymComp(@Vph, @V012);
                            end
                            else
                            begin      // Handle single phase and pos seq models
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

                            S := V012[2] * cong(I012[2]);
                            if Opt = 1 then
                                S := S * 0.001;
                            FSWrite(F, Format('%11.1f', [S.re * 0.003]));
                            FSWrite(F, Format('%11.1f', [S.im * 0.003]));
                            S := V012[3] * cong(I012[3]);
                            if Opt = 1 then
                                S := S * 0.001;
                            FSWrite(F, Format('%11.1f', [S.re * 0.003]));
                            FSWrite(F, Format('%11.1f', [S.im * 0.003]));
                            S := V012[1] * cong(I012[1]);
                            if Opt = 1 then
                                S := S * 0.001;
                            FSWrite(F, Format('%8.1f', [S.re * 0.003]));
                            FSWrite(F, Format('%8.1f', [S.im * 0.003]));
                            FSWriteln(F);

                        end;
                    end;
                    p_Elem := DSS.ActiveCircuit.Sources.Next;
                end;


     // PDELEMENTS next
                PDElem := DSS.ActiveCircuit.PDElements.First;

                while PDElem <> NIL do
                begin
                    if (PDElem.Enabled) then
                    begin
                        NCond := pDElem.NConds;
                        Nterm := pDElem.Nterms;
                        PDElem.GetCurrents(c_Buffer);

                        for j := 1 to NTerm do
                        begin
                            FSWrite(F, Pad(EncloseQuotes(pDElem.FullName), MaxDeviceNameLength + 2) + Format('%3d', [j]));
                            for i := 1 to Min(3, pdelem.Nphases) do
                            begin
                                k := (j - 1) * Ncond + i;
                                nref := pDElem.NodeRef^[k];
                                Volts := DSS.ActiveCircuit.Solution.NodeV^[nref];
                                Iph[i] := c_Buffer^[k];
                                Vph[i] := volts;
                            end;
                            if (PDElem.Nphases >= 3) then
                            begin
                                Phase2SymComp(@Iph, @I012);
                                Phase2SymComp(@Vph, @V012);
                            end
                            else
                            begin      // Handle single phase and pos seq models
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

                            S := V012[2] * cong(I012[2]);
                            if Opt = 1 then
                                S := S * 0.001;
                            FSWrite(F, Format('%11.1f', [S.re * 0.003]));
                            FSWrite(F, Format('%11.1f', [S.im * 0.003]));
                            S := V012[3] * cong(I012[3]);
                            if Opt = 1 then
                                S := S * 0.001;
                            FSWrite(F, Format('%11.1f', [S.re * 0.003]));
                            FSWrite(F, Format('%11.1f', [S.im * 0.003]));
                            S := V012[1] * cong(I012[1]);
                            if Opt = 1 then
                                S := S * 0.001;
                            FSWrite(F, Format('%8.1f', [S.re * 0.003]));
                            FSWrite(F, Format('%8.1f', [S.im * 0.003]));

                            if j = 1 then
                            begin
               //----PDelem.ActiveTerminalIdx := 1;
                                S := PDElem.ExcesskVANorm[1];
                                if Opt = 1 then
                                    S := S * 0.001;
                                FSWrite(F, Format('%11.1f', [S.re]));
                                FSWrite(F, Format('%11.1f', [S.im]));
                                S := PDElem.ExcesskVAEmerg[1];
                                if Opt = 1 then
                                    S := S * 0.001;
                                FSWrite(F, Format('%11.1f', [S.re]));
                                FSWrite(F, Format('%11.1f', [S.im]));
                            end;
                            FSWriteln(F);

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
                        PCElem.GetCurrents(c_Buffer);

                        for j := 1 to NTerm do
                        begin
                            FSWrite(F, Pad(EncloseQuotes(pCElem.FullName), MaxDeviceNameLength + 2) + Format('%3d', [j]));
                            for i := 1 to min(3, pcElem.Nphases) do
                            begin
                                k := (j - 1) * Ncond + i;
                                nref := PCElem.NodeRef^[k];
                                Volts := DSS.ActiveCircuit.Solution.NodeV^[nref];
                                Iph[i] := c_Buffer^[k];
                                Vph[i] := volts;
                            end;

                            if (PCElem.Nphases >= 3) then
                            begin
                                Phase2SymComp(@Iph, @I012);
                                Phase2SymComp(@Vph, @V012);
                            end
                            else
                            begin   // Handle single phase and pos seq models
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

                            S := V012[2] * cong(I012[2]);
                            if Opt = 1 then
                                S := S * 0.001;
                            FSWrite(F, Format('%11.1f', [S.re * 0.003]));
                            FSWrite(F, Format('%11.1f', [S.im * 0.003]));
                            S := V012[3] * cong(I012[3]);
                            if Opt = 1 then
                                S := S * 0.001;
                            FSWrite(F, Format('%11.1f', [S.re * 0.003]));
                            FSWrite(F, Format('%11.1f', [S.im * 0.003]));
                            S := V012[1] * cong(I012[1]);
                            if Opt = 1 then
                                S := S * 0.001;
                            FSWrite(F, Format('%8.1f', [S.re * 0.003]));
                            FSWrite(F, Format('%8.1f', [S.im * 0.003]));


                            FSWriteln(F);

                        end;
                    end;
                    PCElem := DSS.ActiveCircuit.PCElements.Next;
                end;
            end; {ShowOptionCode=0}

            1:
            begin
     {Branch Powers}
                FSWriteln(F);
                FSWriteln(F, 'CIRCUIT ELEMENT POWER FLOW');
                FSWriteln(F);
                FSWriteln(F, '(Power Flow into element from indicated Bus)');
                FSWriteln(F);
                FSWriteln(F, 'Power Delivery Elements');
                FSWriteln(F);
                case Opt of
                    1:
                        FSWriteln(F, Pad('  Bus', MaxBusNameLength), ' Phase     MW     +j   Mvar         MVA         PF');
                else
                    FSWriteln(F, Pad('  Bus', MaxBusNameLength), ' Phase     kW     +j   kvar         kVA         PF');
                end;
                FSWriteln(F);


     // Sources first
                p_Elem := DSS.ActiveCircuit.sources.First;

                while p_Elem <> NIL do
                begin
                    if p_Elem.Enabled then
                    begin
                        NCond := p_Elem.NConds;
                        Nterm := p_Elem.Nterms;
                        p_Elem.GetCurrents(c_Buffer);
                        k := 0;
                        FromBus := Pad(StripExtension(p_Elem.FirstBus), MaxBusNameLength);
                        FSWriteln(F, 'ELEMENT = ', EncloseQuotes(P_Elem.FullName));
                        for j := 1 to NTerm do
                        begin
                            Saccum := CZERO;
                            for i := 1 to NCond do
                            begin
                                Inc(k);
                                nref := p_Elem.NodeRef^[k];
                                Volts := DSS.ActiveCircuit.Solution.NodeV^[nref];
                                S := Volts * cong(c_Buffer^[k]);
                                if { (p_Elem.nphases=1) and } DSS.ActiveCircuit.PositiveSequence then
                                    S := S * 3;
                                if Opt = 1 then
                                    S := S * 0.001;
                                Saccum += S;
                                FSWrite(F, Format('%s %4d    %8.1f +j %8.1f', 
                                    [AnsiUpperCase(FromBus), GetNodeNum(DSS, p_Elem.NodeRef^[k]), S.re / 1000.0, S.im / 1000.0]
                                ));
                                FSWriteln(F, Format('   %8.1f     %8.4f', [Cabs(S) / 1000.0, PowerFactor(S)]));
                            end;
                            FSWrite(F, Format('%s%8.1f +j %8.1f', [Paddots('   TERMINAL TOTAL', MaxBusNameLength + 10), Saccum.re / 1000.0, Saccum.im / 1000.0]));
                            FSWriteln(F, Format('   %8.1f     %8.4f', [Cabs(Saccum) / 1000.0, PowerFactor(Saccum)]));
                            FromBus := Pad(StripExtension(p_Elem.Nextbus), MaxBusNameLength);
                        end;
                        FSWriteln(F);
                    end;
                    p_Elem := DSS.ActiveCircuit.sources.Next;
                end;

     // PDELEMENTS first
                p_Elem := DSS.ActiveCircuit.PDElements.First;

                while p_Elem <> NIL do
                begin
                    if p_Elem.Enabled then
                    begin
                        NCond := p_Elem.NConds;
                        Nterm := p_Elem.Nterms;
                        p_Elem.GetCurrents(c_Buffer);
                        k := 0;
                        FromBus := Pad(StripExtension(p_Elem.FirstBus), MaxBusNameLength);
                        FSWriteln(F, 'ELEMENT = ', EncloseQuotes(p_elem.FullName));

                        if (CLASSMASK and P_Elem.DSSObjType) = AUTOTRANS_ELEMENT then
                            Ntimes := P_Elem.Nphases    // Special case for AutoTrans
                        else
                            Ntimes := NCond;

                        {**** Added April 6 2020 *****}
                        if (NTerm = 2) and (NCond = 1) then // 1-phase devices with two terminals that might be floating
                        begin
                            Saccum := CZERO;
                            Inc(k);
                            nref1 := p_Elem.NodeRef^[k];
                            nref2 := p_Elem.NodeRef^[k + 1];
                            with DSS.ActiveCircuit.Solution do
                                Volts := NodeV^[nref1] - NodeV^[nref2];
                            S := Volts * cong(c_Buffer^[k]);
                            if { (p_Elem.nphases=1) and } DSS.ActiveCircuit.PositiveSequence then
                                S := S * 3;
                            if Opt = 1 then
                                S := S * 0.001;
                            Saccum += S;
                            
                            WriteStr(sout, AnsiUpperCase(FromBus), '  ', GetNodeNum(DSS, p_Elem.NodeRef^[k]): 4, '    ', S.re / 1000.0: 8: 1, ' +j ', S.im / 1000.0: 8: 1);
                            FSWrite(F, sout);

                            WriteStr(sout, '   ', Cabs(S) / 1000.0: 8: 1, '     ', PowerFactor(S): 8: 4);
                            FSWriteln(F, sout);

                            WriteStr(sout, Paddots('   TERMINAL TOTAL', MaxBusNameLength + 10), Saccum.re / 1000.0: 8: 1, ' +j ', Saccum.im / 1000.0: 8: 1);
                            FSWrite(F, sout);

                            WriteStr(sout, '   ', Cabs(Saccum) / 1000.0: 8: 1, '     ', PowerFactor(Saccum): 8: 4);
                            FSWriteln(F, sout);

                            FromBus := Pad(StripExtension(p_Elem.Nextbus), MaxBusNameLength);
                        end
                        else
                        for j := 1 to NTerm do
                        begin
                            Saccum := CZERO;
                            for i := 1 to Ntimes do
                            begin
                                Inc(k);
                                nref := p_Elem.NodeRef^[k];
                                Volts := DSS.ActiveCircuit.Solution.NodeV^[nref];
                                S := Volts * cong(c_Buffer^[k]);
                                if { (p_Elem.nphases=1) and } DSS.ActiveCircuit.PositiveSequence then
                                    S := S * 3;
                                if Opt = 1 then
                                    S := S * 0.001;
                                Saccum += S;
                                WriteStr(sout, AnsiUpperCase(FromBus), '  ', GetNodeNum(DSS, p_Elem.NodeRef^[k]): 4, '    ', S.re / 1000.0: 8: 1, ' +j ', S.im / 1000.0: 8: 1);
                                FSWrite(F, sout);
                                WriteStr(sout, '   ', Cabs(S) / 1000.0: 8: 1, '     ', PowerFactor(S): 8: 4);
                                FSWriteln(F, sout);
                            end;
                            WriteStr(sout, Paddots('   TERMINAL TOTAL', MaxBusNameLength + 10), Saccum.re / 1000.0: 8: 1, ' +j ', Saccum.im / 1000.0: 8: 1);
                            FSWrite(F, sout);
                            WriteStr(sout, '   ', Cabs(Saccum) / 1000.0: 8: 1, '     ', PowerFactor(Saccum): 8: 4);
                            FSWriteln(F, sout);
                            FromBus := Pad(StripExtension(p_Elem.Nextbus), MaxBusNameLength);
                        end;
                        FSWriteln(F);
                        if (CLASSMASK and P_Elem.DSSObjType) = AUTOTRANS_ELEMENT then 
                            Inc(k, Ntimes);
                    end;
                    p_Elem := DSS.ActiveCircuit.PDElements.Next;
                end;

                FSWriteln(F, '= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =');
                FSWriteln(F);
                FSWriteln(F, 'Power Conversion Elements');
                FSWriteln(F);
                case Opt of
                    1:
                        FSWriteln(F, Pad('  Bus', MaxBusNameLength), ' Phase     MW   +j  Mvar         MVA         PF');
                else
                    FSWriteln(F, Pad('  Bus', MaxBusNameLength), ' Phase     kW   +j  kvar         kVA         PF');
                end;
                FSWriteln(F);

     // PCELEMENTS next
                p_Elem := DSS.ActiveCircuit.PCElements.First;

                while p_Elem <> NIL do
                begin
                    if p_Elem.Enabled then
                    begin
                        NCond := p_Elem.NConds;
                        Nterm := p_Elem.Nterms;
                        p_Elem.GetCurrents(c_Buffer);
                        k := 0;
                        FromBus := Pad(StripExtension(p_Elem.FirstBus), MaxBusNameLength);
                        FSWriteln(F, 'ELEMENT = ', EncloseQuotes(P_Elem.FullName));
                        for j := 1 to NTerm do
                        begin
                            Saccum := CZERO;
                            for i := 1 to NCond do
                            begin
                                Inc(k);
                                nref := p_Elem.NodeRef^[k];
                                Volts := DSS.ActiveCircuit.Solution.NodeV^[nref];
                                S := Volts * cong(c_Buffer^[k]);
                                if { (p_Elem.nphases=1) and } DSS.ActiveCircuit.PositiveSequence then
                                    S := S * 3;
                                if Opt = 1 then
                                    S := S * 0.001;
                                Saccum += S;
                                WriteStr(sout, AnsiUpperCase(FromBus), '  ', GetNodeNum(DSS, p_Elem.NodeRef^[k]): 4, '    ', S.re / 1000.0: 6: 1, ' +j ', S.im / 1000.0: 6: 1);
                                FSWrite(F, sout);
                                WriteStr(sout, '   ', Cabs(S) / 1000.0: 8: 1, '     ', PowerFactor(S): 8: 4);
                                FSWriteln(F, sout);
                            end;
                            WriteStr(sout, Paddots('  TERMINAL TOTAL ', MaxBusNameLength + 10), Saccum.re / 1000.0: 8: 1, ' +j ', Saccum.im / 1000.0: 8: 1);
                            FSWrite(F, sout);
                            WriteStr(sout, '   ', Cabs(Saccum) / 1000.0: 8: 1, '     ', PowerFactor(Saccum): 8: 4);
                            FSWriteln(F, sout);
                            FromBus := Pad(StripExtension(p_Elem.Nextbus), MaxBusNameLength);
                        end;
                        FSWriteln(F);
                    end;
                    p_Elem := DSS.ActiveCircuit.PCElements.Next;
                end;

            end; {ShowOptionCode=1}
        end; {CASE}

        FSWriteln(F);
        S := DSS.ActiveCircuit.Losses * 0.001;
        if Opt = 1 then
            S := S * 0.001;
        
        WriteStr(sout, 'Total Circuit Losses = ', S.re: 6: 1, ' +j ', S.im: 6: 1);
        FSWriteln(F, sout);

    finally

        if Assigned(C_buffer) then
            Freemem(c_Buffer);
        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);


    end;
end;

function CheckBusReference(cktElem: TDSSCktElement; BusReference: Integer; var TerminalIndex: Integer): Boolean;

{Check all terminals of cktelement to see if bus connected to busreference}

var
    i: Integer;
begin
    Result := FALSE;
    with cktElem do
        for i := 1 to NTerms do
        begin
            if Terminals[i - 1].BusRef = BusReference then
            begin
                TerminalIndex := i;
                Result := TRUE;
                Break;
            end;
        end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure WriteTerminalPowerSeq(DSS: TDSSContext; F: TFileStream; cktElem: TDSSCktElement; j, opt: Integer);
var
    i, k, Ncond, nref: Integer;
    Volts, S: Complex;
    Vph, V012: Complex3;
    Iph, I012: Complex3;
    c_Buffer: pComplexArray;  // Allocate to max total conductors

begin
    c_Buffer := NIL;


    try
 {Allocate c_Buffer big enough for this circuit element}
        Getmem(c_buffer, sizeof(c_Buffer^[1]) * cktElem.Yorder);

        NCond := cktElem.NConds;
        cktElem.GetCurrents(c_Buffer);
        FSWrite(F, Pad(EncloseQuotes(cktElem.FullName), MaxDeviceNameLength + 2) + IntToStr(j));
        for i := 1 to Min(cktElem.Nphases, 3) do
        begin
            k := (j - 1) * Ncond + i;
            nref := cktElem.NodeRef^[k];
            Volts := DSS.ActiveCircuit.Solution.NodeV^[nref];
            Iph[i] := c_Buffer^[k];
            Vph[i] := volts;
        end;

        if (cktElem.Nphases >= 3) then
        begin
            Phase2SymComp(@Iph, @I012);
            Phase2SymComp(@Vph, @V012);
        end
        else
        begin      // Handle single phase and pos seq models
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


  // Pos Seq or Single Phase
        case cktElem.Nphases of
            1:
                S := Vph[1] * cong(Iph[1]);
            2:
                S := Vph[1] * cong(Iph[1]) + Vph[2] * cong(Iph[3]);
        else
            S := V012[2] * cong(I012[2]);
        end;

        if Opt = 1 then
            S := S * 0.001;
        FSWrite(F, Format('%11.1f', [S.re * 0.003]));
        FSWrite(F, Format('%11.1f', [S.im * 0.003]));
        S := V012[3] * cong(I012[3]);
        if Opt = 1 then
            S := S * 0.001;
        FSWrite(F, Format('%11.1f', [S.re * 0.003]));
        FSWrite(F, Format('%11.1f', [S.im * 0.003]));
        S := V012[1] * cong(I012[1]);
        if Opt = 1 then
            S := S * 0.001;
        FSWrite(F, Format('%8.1f', [S.re * 0.003]));
        FSWrite(F, Format('%8.1f', [S.im * 0.003]));

        FSWriteln(F);

    finally
        if Assigned(C_buffer) then
            Freemem(c_Buffer);
    end;
end;


procedure WriteTerminalPower(DSS: TDSSContext; F: TFileStream; CktElem: TDSSCktElement; jTerm, opt: Integer);

var

    i, k, Ncond, nref: Integer;
    Volts, S: Complex;
    Saccum: Complex;
    c_Buffer: pComplexArray;  // Allocate to max total conductors
    FromBus: String;

begin
    c_Buffer := NIL;

    try

        Getmem(c_buffer, sizeof(c_Buffer^[1]) * CktElem.Yorder);

        NCond := CktElem.NConds;
        CktElem.GetCurrents(c_Buffer);
        FromBus := Pad(StripExtension(CktElem.GetBus(jTerm)), 12);
        FSWriteln(F, 'ELEMENT = ', Pad(EncloseQuotes(cktElem.FullName), MaxDeviceNameLength + 2));

        Saccum := CZERO;
        for i := 1 to NCond do
        begin
            k := (jTerm - 1) * Ncond + i;
            nref := CktElem.NodeRef^[k];
            Volts := DSS.ActiveCircuit.Solution.NodeV^[nref];
            S := Volts * cong(c_Buffer^[k]);
            if { (CktElem.nphases=1) and } DSS.ActiveCircuit.PositiveSequence then
                S := S * 3;
            if Opt = 1 then
                S := S * 0.001;
            Saccum += S;
            FSWriteln(F, Format('%s %4d %10.5g +j %10.5g    %10.5g    %8.4f',
                [AnsiUpperCase(FromBus), GetNodeNum(DSS, CktElem.NodeRef^[k]), S.re / 1000.0, S.im / 1000.0,
                Cabs(S) / 1000.0, PowerFactor(S)]));
        end;
        FSWriteln(F, Format(' TERMINAL TOTAL   %10.5g +j %10.5g    %10.5g    %8.4f',
            [Saccum.re / 1000.0, Saccum.im / 1000.0, Cabs(Saccum) / 1000.0,
            PowerFactor(Saccum)]));

    finally
        if Assigned(C_buffer) then
            Freemem(c_Buffer);
    end;
end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowBusPowers(DSS: TDSSContext; FileNm, BusName: String; opt, ShowOptionCode: Integer);

{Report power flow around a specified Bus}

{Opt = 0: kVA
 opt = 1: MVA
 }

var

    F: TFileStream = nil;

    j, Ncond, Nterm: Integer;
    p_Elem: TDSSCktElement;
    PDElem: TPDElement;
    PCElem: TPCElement;
    I0, I1, I2, Cmax: Double;
    c_Buffer: pComplexArray;  // Allocate to max total conductors
    BusReference: Integer;
    jTerm: Integer;


begin
    SetMaxDeviceNameLength(DSS);
    c_Buffer := NIL;

  {Get Bus Reference}
    BusReference := DSS.ActiveCircuit.BusList.Find(BusName);
    if BusReference = 0 then
    begin
        DoSimpleMsg(DSS, 'Bus "%s" not found.', [AnsiUpperCase(BusName)], 219);
        Exit;
    end;
    try
        F := TBufferedFileStream.Create(FileNm, fmCreate);

      {Allocate c_Buffer big enough for largest circuit element}
        Getmem(c_buffer, sizeof(c_Buffer^[1]) * GetMaxCktElementSize(DSS));

        case ShowOptionCode of
            0:
            begin
     {Write Bus Voltage}

                FSWriteln(F);
                FSWriteln(F, 'Bus      V1 (kV)    p.u.    V2 (kV)      %V2/V1    V0 (kV)  %V0/V1');
                FSWriteln(F);

                WriteSeqVoltages(DSS, F, BusReference, FALSE);

     {Sequence Currents}
                FSWriteln(F);
                FSWriteln(F, 'SYMMETRICAL COMPONENT CURRENTS BY CIRCUIT ELEMENT (first 3 phases)');
                FSWriteln(F);
                FSWriteln(F, 'Element                Term      I1         I2       %I2/I1       I0      %I0/I1   %Normal %Emergency');
                FSWriteln(F);

     // Sources first
                p_Elem := DSS.ActiveCircuit.Sources.First;

                while p_Elem <> NIL do
                begin
                    if (p_Elem.Enabled) then
                        if CheckBusReference(p_Elem, BusReference, j) then
                        begin
                            NCond := p_elem.NConds;
                            Nterm := p_elem.Nterms;
                            p_elem.GetCurrents(c_Buffer);

                            for j := 1 to NTerm do
                            begin
                                GetI0I1I2(I0, I1, I2, Cmax, p_elem.Nphases, (j - 1) * Ncond, c_Buffer);
                                with p_elem do
                                    WriteSeqCurrents(F, Paddots(EncloseQuotes(p_Elem.FullName), MaxDeviceNameLength + 2), I0, I1, I2, Cmax, 0.0, 0.0, j, DSSObjType);
                            end;

                        end;
                    p_Elem := DSS.ActiveCircuit.Sources.Next;
                end;


     // PDELEMENTS next
                PDElem := DSS.ActiveCircuit.PDElements.First;
                while PDElem <> NIL do
                begin
                    if (PDElem.Enabled) then
                        if CheckBusReference(PDElem, BusReference, j) then
                        begin  // Is this connected to the bus
                            NCond := PDElem.NConds;
                            Nterm := PDElem.Nterms;
                            PDElem.GetCurrents(c_Buffer);

                            for j := 1 to NTerm do
                            begin
                                GetI0I1I2(I0, I1, I2, Cmax, PDElem.Nphases, (j - 1) * Ncond, c_Buffer);
                                with PDElem do
                                    WriteSeqCurrents(F, Paddots(EncloseQuotes(PDElem.FullName), MaxDeviceNameLength + 2), I0, I1, I2, Cmax, 0.0, 0.0, j, DSSObjType);
                            end;
                        end;
                    PDElem := DSS.ActiveCircuit.PDElements.Next;
                end;

     // PCELEMENTS Next
                PCElem := DSS.ActiveCircuit.PCElements.First;
                while PCElem <> NIL do
                begin
                    if (PCElem.Enabled) then
                        if CheckBusReference(PCElem, BusReference, j) then
                        begin
                            NCond := PCElem.NConds;
                            Nterm := PCElem.Nterms;
                            PCElem.GetCurrents(c_Buffer);

                            for j := 1 to NTerm do
                            begin
                                GetI0I1I2(I0, I1, I2, Cmax, PCElem.Nphases, (j - 1) * Ncond, c_Buffer);
                                with PCElem do
                                    WriteSeqCurrents(F, Paddots(EncloseQuotes(PCelem.FullName), MaxDeviceNameLength + 2), I0, I1, I2, Cmax, 0.0, 0.0, j, DSSObjType);
                            end;
                        end;
                    PCElem := DSS.ActiveCircuit.PCElements.Next;
                end;

     {Sequence Powers}
                FSWriteln(F);
                FSWriteln(F, 'SYMMETRICAL COMPONENT POWERS BY CIRCUIT ELEMENT (first 3 phases)');
                FSWriteln(F);
                case Opt of
                    1:
                        FSWriteln(F, 'Element                      Term    P1(MW)   Q1(Mvar)       P2         Q2      P0      Q0   ');
                else
                    FSWriteln(F, 'Element                      Term    P1(kW)   Q1(kvar)         P2         Q2      P0      Q0  ');
                end;
                FSWriteln(F);

     // Sources first
                p_Elem := DSS.ActiveCircuit.Sources.First;

                while p_Elem <> NIL do
                begin
                    if (p_Elem.Enabled) then
                        if CheckBusReference(p_Elem, BusReference, j) then
                        begin
        {Use j set by CheckBusReference}
                            WriteTerminalPowerSeq(DSS, F, p_Elem, j, opt);
                        end;
                    p_Elem := DSS.ActiveCircuit.Sources.Next;
                end;


     // PDELEMENTS next
                PDElem := DSS.ActiveCircuit.PDElements.First;
                while PDElem <> NIL do
                begin
                    if (PDElem.Enabled) then
                        if CheckBusReference(PDElem, BusReference, j) then
                        begin  // Is this connected to the bus
                            WriteTerminalPowerSeq(DSS, F, PDElem, j, opt);
                        end;
                    PDElem := DSS.ActiveCircuit.PDElements.Next;
                end;

     // PCELEMENTS Next
                PCElem := DSS.ActiveCircuit.PCElements.First;
                while PCElem <> NIL do
                begin
                    if (PCElem.Enabled) then
                        if CheckBusReference(PCElem, BusReference, j) then
                        begin
                            WriteTerminalPowerSeq(DSS, F, PCElem, j, opt)
                        end;
                    PCElem := DSS.ActiveCircuit.PCElements.Next;
                end;
            end; {ShowOptionCode=0}

            1:
            begin
     {Write Bus Voltage}

                FSWriteln(F);
                FSWriteln(F, '  Bus   (node ref)  Node       V (kV)    Angle    p.u.   Base kV');
                FSWriteln(F);
                WriteBusVoltages(DSS, F, BusReference, FALSE);

     {Element Currents}
                FSWriteln(F);
                FSWriteln(F, 'CIRCUIT ELEMENT CURRENTS');
                FSWriteln(F);
                FSWriteln(F, '(Currents into element from indicated bus)');
                FSWriteln(F);
                FSWriteln(F, 'Power Delivery Elements');
                FSWriteln(F);
                FSWriteln(F, '  Bus         Phase    Magnitude, A     Angle      (Real)   +j  (Imag)');
                FSWriteln(F);


          // Sources first
                p_Elem := DSS.ActiveCircuit.sources.First;
                while p_Elem <> NIL do
                begin
                    if p_Elem.Enabled then
                        if CheckBusReference(p_Elem, BusReference, j) then
                        begin
                            WriteTerminalCurrents(DSS, F, p_Elem, FALSE);
                            FSWriteln(F);
                        end;
                    p_Elem := DSS.ActiveCircuit.sources.Next;
                end;


     // PDELEMENTS first
                p_Elem := DSS.ActiveCircuit.PDElements.First;

                while p_Elem <> NIL do
                begin
                    if p_Elem.Enabled then
                        if CheckBusReference(p_Elem, BusReference, j) then
                        begin
                            WriteTerminalCurrents(DSS, F, p_Elem, TRUE);
                            FSWriteln(F);
                        end;
                    p_Elem := DSS.ActiveCircuit.PDElements.Next;
                end;

                FSWriteln(F, '= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =');
                FSWriteln(F);
                FSWriteln(F, 'Power Conversion Elements');
                FSWriteln(F);
                FSWriteln(F, '  Bus         Phase    Magnitude, A     Angle      (Real)   +j  (Imag)');
                FSWriteln(F);

     // PCELEMENTS next
                p_Elem := DSS.ActiveCircuit.PCElements.First;

                while p_Elem <> NIL do
                begin
                    if p_Elem.Enabled then
                        if CheckBusReference(p_Elem, BusReference, j) then
                        begin
                            WriteTerminalCurrents(DSS, F, p_Elem, FALSE);
                            FSWriteln(F);
                        end;

                    p_Elem := DSS.ActiveCircuit.PCElements.Next;
                end;

      // FAULTs next
                p_Elem := DSS.ActiveCircuit.Faults.First;

                while p_Elem <> NIL do
                begin
                    if p_Elem.Enabled then
                        if CheckBusReference(p_Elem, BusReference, j) then
                        begin
                            WriteTerminalCurrents(DSS, F, p_Elem, FALSE);
                            FSWriteln(F);
                        end;

                    p_Elem := DSS.ActiveCircuit.Faults.Next;
                end;

     {Branch Powers}
                FSWriteln(F);
                FSWriteln(F, 'CIRCUIT ELEMENT POWER FLOW');
                FSWriteln(F);
                FSWriteln(F, '(Power Flow into element from indicated Bus)');
                FSWriteln(F);
                case Opt of
                    1:
                        FSWriteln(F, '  Bus       Phase     MW     +j   Mvar           MVA           PF');
                else
                    FSWriteln(F, '  Bus       Phase     kW     +j   kvar           kVA           PF');
                end;
                FSWriteln(F);

     // Sources first
                p_Elem := DSS.ActiveCircuit.sources.First;
                while p_Elem <> NIL do
                begin
                    if p_Elem.Enabled then
                        if CheckBusReference(p_Elem, BusReference, j) then
                        begin
                            WriteTerminalPower(DSS, F, p_Elem, j, opt);
                            FSWriteln(F);
                        end;
                    p_Elem := DSS.ActiveCircuit.sources.Next;
                end;


     // PDELEMENTS first
                p_Elem := DSS.ActiveCircuit.PDElements.First;

                while p_Elem <> NIL do
                begin
                    if p_Elem.Enabled then
                        if CheckBusReference(p_Elem, BusReference, jTerm) then
                        begin
                            WriteTerminalPower(DSS, F, p_Elem, jTerm, opt);

          {Get the other buses for the report}
                            for j := 1 to p_Elem.nterms do
                                if j <> jTerm then
                                begin
                                    FSWriteln(F, '------------');
                                    WriteTerminalPower(DSS, F, p_Elem, j, opt);
                                end;
                        end;
                    p_Elem := DSS.ActiveCircuit.PDElements.Next;
                end;

                FSWriteln(F, '= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =');
                FSWriteln(F);
                FSWriteln(F, 'Power Conversion Elements');
                FSWriteln(F);
                case Opt of
                    1:
                        FSWriteln(F, '  Bus         Phase     MW   +j  Mvar         MVA         PF');
                else
                    FSWriteln(F, '  Bus         Phase     kW   +j  kvar         kVA         PF');
                end;
                FSWriteln(F);

     // PCELEMENTS next
                p_Elem := DSS.ActiveCircuit.PCElements.First;

                while p_Elem <> NIL do
                begin
                    if p_Elem.Enabled then
                        if CheckBusReference(p_Elem, BusReference, jTerm) then
                        begin
                            WriteTerminalPower(DSS, F, p_Elem, jTerm, opt);
                            FSWriteln(F);
                        end;

                    p_Elem := DSS.ActiveCircuit.PCElements.Next;
                end;

            end; {ShowOptionCode=1}

        else

        end; {CASE}


    finally
        if Assigned(C_buffer) then
            Freemem(c_Buffer);
        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);

    end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowFaultStudy(DSS: TDSSContext; FileNm: String);

var
    i, iBus, iphs: Integer;
    YFault, ZFault: Tcmatrix;
    Vfault: pComplexArray;  {Big temp array}
    F: TFileStream = nil;
    GFault, IFault: complex;
    Vphs: Double;
    CurrMag: Double;
    S: String;
    sout: String;

begin
    SetMaxBusNameLength(DSS);

    try

        F := TBufferedFileStream.Create(FileNm, fmCreate);

   { Set source voltage injection currents }
        with DSS.ActiveCircuit do
        begin
            with Solution do
            begin
     {All Phase Faults}
                FSWriteln(F, 'FAULT STUDY REPORT');
                FSWriteln(F);
                FSWriteln(F, 'ALL-Node Fault Currents');
                FSWriteln(F);
                FSWriteln(F, Pad('Bus', MaxBusNameLength), '       Node 1  X/R        Node 2  X/R        Node 3  X/R   ...  (Amps)');
                FSWriteln(F);
                for iBus := 1 to NumBuses do
           {Bus Norton Equivalent Current, Isc has been previously computed}
                    with Buses^[iBus] do
                    begin
                        WriteStr(sout, Pad(EncloseQuotes(AnsiUpperCase(BusList.NameOfIndex(iBus))) + ',', MaxBusNameLength + 2));
                        FSWrite(F, sout);
                        for i := 1 to NumNodesThisBus do
                        begin
                            CurrMag := Cabs(BusCurrent^[i]);
                            if i > 1 then
                                FSWrite(F, ', ');
                                
                            
                            WriteStr(sout, CurrMag: 10: 0);
                            FSWrite(F, sout);
                            
                            if Currmag > 0.0 then
                            begin
                                WriteStr(sout, ', ', GetXR(VBus^[i] / BusCurrent^[i]): 5: 1);
                                FSWrite(F, sout)
                            end
                            else
                                FSWrite(F, ',   N/A');
                        end;
                        FSWriteln(F);
                    end;

                FSWriteln(F);

           {One Phase Faults}
                FSWriteln(F);
                FSWriteln(F, 'ONE-Node to ground Faults');
                FSWriteln(F);
                FSWriteln(F, '                                      pu Node Voltages (L-N Volts if no base)');
                FSWriteln(F, Pad('Bus', MaxBusNameLength), '   Node  Amps         Node 1     Node 2     Node 3    ...');
                FSWriteln(F);

   { Solve for Fault Injection Currents}
                for iBus := 1 to NumBuses do
           {Bus Norton Equivalent Current, Isc has been previously computed}
                    with Buses^[iBus] do
                    begin
                        ZFault := TcMatrix.CreateMatrix(NumNodesThisBus);
                        ZFault.CopyFrom(Zsc);

                        for iphs := 1 to NumNodesThisBus do
                        begin
                            IFault := VBus[iphs] / Zsc.GetElement(iphs, iphs);

                            S := Format('%s %4u %12.0f ', [Pad(EncloseQuotes(AnsiUpperCase(BusList.NameOfIndex(iBus))), MaxBusNameLength + 2), GetNum(iphs), Cabs(Ifault)]);
                            FSWrite(F, S, '   ');
                            for i := 1 to NumNodesThisBus do
                            begin
                                Vphs := Cabs(VBus[i] - (Zsc.GetElement(i, iphs) * IFault));
                                if kVbase > 0.0 then
                                begin
                                    VPhs := 0.001 * Vphs / kVBase;
                                    WriteStr(sout, ' ', Vphs: 10: 3);
                                    FSWrite(F, sout);
                                end
                                else
                                begin
                                    WriteStr(sout, ' ', Vphs: 10: 1);
                                    FSWrite(F, sout);
                                end;
                            end;
                            FSWriteln(F);

                        end; {For iphase}
             {Now, Stuff it in the Css Array where it belongs}

                        ZFault.Free;
                    end;  {With bus}

           {Node-Node Faults}
                FSWriteln(F);
                FSWriteln(F, 'Adjacent Node-Node Faults');
                FSWriteln(F);
                FSWriteln(F, '                                        pu Node Voltages (L-N Volts if no base)');
                FSWriteln(F, 'Bus          Node-Node      Amps        Node 1     Node 2     Node 3    ...');
                FSWriteln(F);

                // Solve for Fault Injection Currents
                for iBus := 1 to NumBuses do
                    // Bus Norton Equivalent Current, Isc has been previously computed
                    with Buses^[iBus] do
                    begin
                        YFault := TcMatrix.CreateMatrix(NumNodesThisBus);
                        Getmem(VFault, Sizeof(Complex) * NumNodesThisBus);

                        GFault := Cmplx(10000.0, 0.0);

                        for iphs := 1 to NumNodesThisBus - 1 do
                        begin
                            YFault.CopyFrom(Ysc);
                            YFault.AddElement(iphs, iphs, GFault);
                            YFault.AddElement(iphs + 1, iphs + 1, GFault);
                            YFault.AddElemSym(iphs, iphs + 1, -GFault);

                            // Solve for Injection Currents
                            YFault.Invert;
                            YFault.MvMult(VFault, BusCurrent); // Gets voltage appearing at fault

                            WriteStr(sout, Pad(EncloseQuotes(AnsiUpperCase(BusList.NameOfIndex(iBus))), MaxBusNameLength + 2), GetNum(Iphs): 4, GetNum(Iphs + 1): 4, Cabs((VFault^[iphs] - VFault^[iphs + 1]) * GFault): 12: 0, '   ');
                            FSWrite(F, sout);
                            for i := 1 to NumNodesThisBus do
                            begin
                                Vphs := Cabs(VFault^[i]);
                                if kvbase > 0.0 then
                                begin
                                    Vphs := 0.001 * Vphs / kVBase;
                                    WriteStr(sout, ' ', Vphs: 10: 3);
                                    FSWrite(F, sout);
                                end
                                else
                                begin
                                    WriteStr(sout, ' ', Vphs: 10: 1);
                                    FSWrite(F, sout);
                                end;
                            end;
                            FSWriteln(F);

                        end; // For iphase

                        // Now, Stuff it in the Css Array where it belongs
                        Freemem(VFault);
                        YFault.Free;

                    end;  //With bus

            end; // With Solution
        end; // With ActiveCircuit

    finally

        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);
    end;
end;

procedure WriteElementRecord(F: TFileStream; pElem: TDSSCktElement);
var
    Nterm, j: Integer;
    BusName: String;
begin
    Nterm := pElem.Nterms;
    BusName := Pad(StripExtension(pElem.FirstBus), MaxBusNameLength);
    FSWrite(F, Pad(EncloseQuotes(PElem.FullName), MaxDeviceNameLength + 2), ' ');
    for j := 1 to NTerm do
    begin
        FSWrite(F, AnsiUpperCase(Busname), ' ');
        BusName := Pad(StripExtension(pElem.Nextbus), MaxBusNameLength);
    end;
    FSWriteln(F);
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowElements(DSS: TDSSContext; FileNm: String; ClassName: String);

// Show Elements and bus connections

var
    F: TFileStream = nil;
    FDisabled: TFileStream = nil;
    i: Integer;
    DisabledFileNm: String;
    pElem: TDSSCktElement;

begin
    SetMaxBusNameLength(DSS);
    SetMaxDeviceNameLength(DSS);

    try
        try
            F := TBufferedFileStream.Create(FileNm, fmCreate);
        except
            On E: Exception do
                DoSimpleMsg(DSS, 'Error trying to open element file "%s" file: %s', [FileNm, E.message], 219000);
        end;

        try
            DisabledFileNm := StripExtension(FileNm) + '_Disabled.txt';
            FDisabled := TBufferedFileStream.Create(DisabledFileNm, fmCreate);
        except
            On E: Exception do
                DoSimpleMsg(DSS, 'Error trying to open disabled element file "%s" file: %s', [DisabledFilenm, E.message], 219000);
        end;

        if Length(ClassName) > 0 then
        begin  // Just give a list of Active elements of a particular Class
            if SetObjectClass(DSS, ClassName) then
            begin
                FSWriteln(F, 'All Elements in Class "', ClassName, '"');
                FSWriteln(F);
                FSWriteln(Fdisabled, 'All DISABLED Elements in Class "', ClassName, '"');
                FSWriteln(Fdisabled);
                DSS.ActiveDSSClass := DSS.DSSClassList.Get(DSS.LastClassReferenced);
                for i := 1 to DSS.ActiveDSSClass.ElementCount do
                begin
                    DSS.ActiveDSSClass.Active := i;
                    if (DSS.ActiveDSSClass.DSSClassType and BASECLASSMASK) > 0 then
                    begin
                        if TDSSCktElement(DSS.ActiveDSSObject).Enabled then
                            FSWriteln(F, AnsiUpperCase(DSS.ActiveDssObject.Name))
                        else
                            FSWriteln(Fdisabled, AnsiUpperCase(DSS.ActiveDssObject.Name));
                    end
                    else
                        FSWriteln(F, AnsiUpperCase(DSS.ActiveDssObject.Name));   // non cktelements
                end;
            end;
        end
        else
        begin  // Default - Just do PD and PC Element in active circuit

            FSWriteln(F);
            FSWriteln(F, 'Elements in Active Circuit: ' + DSS.ActiveCircuit.Name);
            FSWriteln(F);
            FSWriteln(F, 'Power Delivery Elements');
            FSWriteln(F);
            FSWriteln(F, Pad('Element', MaxDeviceNameLength + 2) + Pad(' Bus1', MaxBusNameLength) + Pad(' Bus2', MaxBusNameLength) + Pad(' Bus3', MaxBusNameLength) + ' ...');
            FSWriteln(F);


            FSWriteln(Fdisabled);
            FSWriteln(Fdisabled, 'DISABLED Elements in Active Circuit: ' + DSS.ActiveCircuit.Name);
            FSWriteln(Fdisabled);
            FSWriteln(Fdisabled, 'DISABLED Power Delivery Elements');
            FSWriteln(Fdisabled);
            FSWriteln(Fdisabled, Pad('DISABLED Element', MaxDeviceNameLength + 2) + Pad(' Bus1', MaxBusNameLength) + Pad(' Bus2', MaxBusNameLength) + Pad(' Bus3', MaxBusNameLength) + ' ...');
            FSWriteln(Fdisabled);

     // PDELEMENTS first
            pElem := DSS.ActiveCircuit.PDElements.First;

            while pElem <> NIL do
            begin
                if pElem.Enabled then
                begin
                    WriteElementRecord(F, pElem);
                end
                else
                begin
                    WriteElementRecord(Fdisabled, pElem);
                end;
                pElem := DSS.ActiveCircuit.PDElements.Next;
            end;

            FSWriteln(F);
            FSWriteln(F, 'Power Conversion Elements');
            FSWriteln(F);
            FSWriteln(F, Pad('Element', MaxDeviceNameLength + 2) + Pad(' Bus1', MaxBusNameLength) + Pad(' Bus2', MaxBusNameLength) + Pad(' Bus3', MaxBusNameLength) + ' ...');
            FSWriteln(F);

            FSWriteln(Fdisabled);
            FSWriteln(Fdisabled, 'DISABLED Power Conversion Elements');
            FSWriteln(Fdisabled);
            FSWriteln(Fdisabled, Pad('DISABLED Element', MaxDeviceNameLength + 2) + Pad(' Bus1', MaxBusNameLength) + Pad(' Bus2', MaxBusNameLength) + Pad(' Bus3', MaxBusNameLength) + ' ...');
            FSWriteln(Fdisabled);

     // PCELEMENTS next
            pElem := DSS.ActiveCircuit.PCElements.First;

            while pElem <> NIL do
            begin
                if pElem.Enabled then
                begin
                    WriteElementRecord(F, pElem);
                end
                else
                begin
                    WriteElementRecord(Fdisabled, pElem);
                end;
                pElem := DSS.ActiveCircuit.PCElements.Next;
            end;


        end;


    finally

        FreeAndNil(FDisabled);
        FireOffEditor(DSS, DisabledFileNm);
        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);

    end;
end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowBuses(DSS: TDSSContext; FileNm: String);

// Show bus names and nodes in uses

var
    F: TFileStream = nil;
    i, j: Integer;
    pBus: TDSSBus;

begin
    try
        SetMaxBusNameLength(DSS);
        Inc(MaxBusNameLength, 2);
        F := TBufferedFileStream.Create(FileNm, fmCreate);

        FSWriteln(F);
        FSWriteln(F, 'BUSES AND NODES IN ACTIVE CIRCUIT: ' + DSS.ActiveCircuit.name);
        FSWriteln(F);
        FSWriteln(F, Pad('     ', MaxBusNameLength), '                         Coord                                 Number of     Nodes');
        FSWriteln(F, Pad('  Bus', MaxBusNameLength), '    Base kV             (x, y)                      Keep?       Nodes        connected ...');
        FSWriteln(F);
        with DSS.ActiveCircuit do
        begin
            for i := 1 to NumBuses do
            begin
                FSWrite(F, Pad(EncloseQuotes(BusList.NameOfIndex(i)), MaxBusNameLength) + ' ');
                pBus := Buses^[i];
                if pBus.kVBase > 0.0 then
                    FSWrite(F, Format('%7.3f', [pBus.kVbase * SQRT3]))
                else
                    FSWrite(F, '   NA ');
                FSWrite(F, '          (');
                if pBus.CoordDefined then
                    FSWrite(F, Format(' %-13.11g, %-13.11g)', [pBus.x, pBus.y]))
                else
                    FSWrite(F, '           NA,            NA )');
                if pBus.Keep then
                    FSWrite(F, '     Yes  ')
                else
                    FSWrite(F, '     No  ');
                FSWrite(F, '     ');
                FSWrite(F, Format('%5d', [pBus.NumNodesThisBus]));
                FSWrite(F, '       ');
                for j := 1 to pBus.NumNodesThisBus do
                begin
                    FSWrite(F, Format('%4d ', [pBus.GetNum(j)]));
                end;
                FSWriteln(F);
            end;
        end;

    finally

        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);

    end;
end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowMeters(DSS: TDSSContext; FileNm: String);

// Show Values of  Meter Elements

var
    F: TFileStream = nil;
    i, j: Integer;
    pElem: TEnergyMeterObj;
    MeterClass: TEnergyMeter;

begin
    try
        F := TBufferedFileStream.Create(FileNm, fmCreate);

        FSWriteln(F);
        FSWriteln(F, 'ENERGY METER VALUES');
        FSWriteln(F);
        FSWriteln(F, 'Registers:');
        MeterClass := TEnergyMeter(GetDSSClassPtr(DSS, 'Energymeter'));
        if MeterClass = NIL then
            Exit;  // oops somewhere!!
        if MeterClass.ElementCount = 0 then
        begin
            FSWriteln(F, 'No Energymeter Elements Defined.');
        end
        else
        begin
            ;

            pElem := DSS.ActiveCircuit.energyMeters.First;   // write registernames for first meter only
            for i := 1 to NumEMRegisters do
                FSWriteln(F, 'Reg ' + IntToStr(i) + ' = ', pElem.RegisterNames[i]);
            FSWriteln(F);

            pElem := DSS.ActiveCircuit.energyMeters.First;
            if pElem <> NIL then
            begin
                FSWrite(F, 'Meter        ');
                for i := 1 to NumEMRegisters do
                    FSWrite(F, Pad('   Reg ' + IntToStr(i), 11));
                FSWriteln(F);
                FSWriteln(F);
                while pElem <> NIL do
                begin
                    if pElem.Enabled then
                    begin
                        FSWrite(F, Pad(pElem.Name, 12));
                        for j := 1 to NumEMRegisters do
                        begin
                            FSWrite(F, Format('%10.0f ', [PElem.Registers[j]]));
                        end;
                    end;
                    pElem := DSS.ActiveCircuit.EnergyMeters.Next;
                    FSWriteln(F);
                end;
            end;

        end;

    finally

        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);

    end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowGenMeters(DSS: TDSSContext; FileNm: String);

// Show Values of Generator Meter Elements

var
    F: TFileStream = nil;
    i, j: Integer;
    pElem: TGeneratorObj;
    GeneratorClass: TGenerator;

begin
    try
        F := TBufferedFileStream.Create(FileNm, fmCreate);

        FSWriteln(F);
        FSWriteln(F, 'GENERATOR ENERGY METER VALUES');
        FSWriteln(F);
        pElem := DSS.ActiveCircuit.Generators.First;
        if pElem <> NIL then
        begin
            GeneratorClass := TGenerator(pElem.ParentClass);
            FSWrite(F, 'Generator          ');
            for i := 1 to NumGenRegisters do
                FSWrite(F, Pad(GeneratorClass.RegisterNames[i], 11));
            FSWriteln(F);
            FSWriteln(F);
            while pElem <> NIL do
            begin
                if pElem.Enabled then
                begin
                    FSWrite(F, Pad(pElem.Name, 12));
                    for j := 1 to NumGenRegisters do
                    begin
                        FSWrite(F, Format('%10.0f ', [PElem.Registers[j]]));
                    end;
                end;
                pElem := DSS.ActiveCircuit.Generators.Next;
                FSWriteln(F);
            end;
        end;

    finally

        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);

    end;
end;

function TapPosition(const Transformer: TTransfObj; iWind: Integer): Integer;

{Assumes 0  is 1.0 per unit tap}

begin
    with Transformer do
        Result := Round((PresentTap[iWind] - (Maxtap[iWind] + Mintap[iWind]) / 2.0) / TapIncrement[iWind]);
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowRegulatorTaps(DSS: TDSSContext; FileNm: String);

var
    F: TFileStream = nil;
    pReg: TRegControlObj;
    iWind: Integer;


begin
    try
        F := TBufferedFileStream.Create(FileNm, fmCreate);

        FSWriteln(F);
        FSWriteln(F, 'CONTROLLED TRANSFORMER TAP SETTINGS');
        FSWriteln(F);
        FSWriteln(F, 'Name            Tap      Min       Max     Step  Position');
        FSWriteln(F);

        with DSS.ActiveCircuit do
        begin
            pReg := RegControls.First;
            while pReg <> NIL do
            begin
                with pReg.Transformer do
                begin
                    iWind := pReg.TrWinding;
                    FSWrite(F, Pad(Name, 12), ' ');
                    FSWriteln(F, Format('%8.5f %8.5f %8.5f %8.5f     %d', [PresentTap[iWind], MinTap[iWind], MaxTap[iWind], TapIncrement[iWind], TapPosition(pREg.Transformer, iWind)]));
                end;
                pReg := RegControls.Next;
            end;
        end;
    finally
        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);
    end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowMeterZone(DSS: TDSSContext; FileNm: String);

var
    F: TFileStream = nil;
    i: Integer;
    pMtr: TEnergyMeterObj;
    pMtrClass: TEnergyMeter;
    PDelem: TPDelement;
    LoadElem: TLoadObj;
    // ParamName: String;
    Param: String;

begin
    try
        FileNm := StripExtension(FileNm);
        {ParamName :=} DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;

        FileNm := FileNm + '_' + Param + '.txt';

        F := TBufferedFileStream.Create(FileNm, fmCreate);

        DSS.GlobalResult := FileNm;

        pMtrClass := DSS.DSSClassList.Get(DSS.ClassNames.Find('energymeter'));

        if Length(Param) > 0 then
        begin
            pMtr := pMtrClass.Find(Param);
            if pMtr = NIL then
                DoSimpleMsg(DSS, 'EnergyMeter "%s" not found.', [Param], 220)
            else
            if pMtr.BranchList <> NIL then
            begin
                FSWriteln(F, 'Branches and Load in Zone for EnergyMeter ', Param);
                FSWriteln(F);
                PDElem := pMtr.BranchList.First;
                while PDElem <> NIL do
                begin
                    for i := 1 to pMtr.Branchlist.Level do
                        FSWrite(F, TABCHAR);
                 //Write(F, pMtr.BranchList.Level:0,' ');
                    FSWrite(F, PDElem.ParentClass.Name, '.', PDelem.Name);
                    with pMtr.BranchList.PresentBranch do
                    begin
                        if IsParallel then
                            FSWrite(F, '(PARALLEL:' + TDSSCktElement(LoopLineObj).Name + ')');
                        if IsLoopedHere then
                            FSWrite(F, '(LOOP:' + TDSSCktElement(LoopLineObj).FullName + ')');
                    end;
                    if Assigned(PDElem.SensorObj) then
                        FSWrite(F, Format(' (Sensor: %s) ', [PDElem.SensorObj.FullName]))
                    else
                        FSWrite(F, ' (Sensor: NIL)');
                    FSWriteln(F);
                    LoadElem := pMtr.Branchlist.FirstObject;
                    while LoadElem <> NIL do
                    begin
                        for i := 1 to pMtr.Branchlist.Level + 1 do
                            FSWrite(F, TABCHAR);
                        FSWrite(F, LoadElem.ParentClass.Name, '.', LoadElem.Name);
                        if Assigned(LoadElem.SensorObj) then
                            FSWrite(F, Format(' (Sensor: %s) ', [LoadElem.SensorObj.FullName]))
                        else
                            FSWrite(F, ' (Sensor: NIL)');
                        FSWriteln(F);
                        LoadElem := pMtr.BranchList.NextObject
                    end;
                    PDElem := pMtr.BranchList.GoForward;
                end;
            end;
        end
        else
            DoSimpleMsg(DSS, 'Meter Name Not Specified. %s', [CRLF + DSS.Parser.CmdString], 221);

    finally

        FreeAndNil(F);

        DSS.Parser.NextParam;
        Param := DSS.Parser.strvalue;

        case length(Param) of
            0:
                FireOffEditor(DSS, FileNm);
        else
            ShowTreeView(FileNm);
        end;
        DSS.ParserVars.Add('@lastshowfile', FileNm);
     //
    end;
end;

procedure ShowOverloads(DSS: TDSSContext; FileNm: String);

var
    F: TFileStream = nil;
    c_Buffer: pComplexArray;  // Allocate to max total conductors
    NCond, i, j, k: Integer;
    PDElem: TPDElement;
    Iph, I012: Complex3;
    I0, I1, I2,
    Cmag, Cmax: Double;

begin
    c_Buffer := NIL;
    SetMaxDeviceNameLength(DSS);

    try

        F := TBufferedFileStream.Create(FileNm, fmCreate);

      {Allocate c_Buffer big enough for largest circuit element}
        Getmem(c_buffer, sizeof(c_Buffer^[1]) * GetMaxCktElementSize(DSS));

     {Sequence Currents}
        FSWriteln(F);
        FSWriteln(F, 'Power Delivery Element Overload Report');
        FSWriteln(F);
        FSWriteln(F, 'SYMMETRICAL COMPONENT CURRENTS BY CIRCUIT ELEMENT ');
        FSWriteln(F);
        FSWriteln(F, 'Element                             Term    I1    IOver %Normal  %Emerg     I2    %I2/I1    I0    %I0/I1');
        FSWriteln(F);


     // PDELEMENTS
        PDelem := DSS.ActiveCircuit.PDElements.First;
        while PDelem <> NIL do
        begin
            if (PDelem.Enabled) then
                if (CLASSMASK and PDElem.DSSObjType) <> CAP_ELEMENT     // Ignore capacitors
                then
                begin
                    NCond := PDelem.NConds;
                    PDelem.GetCurrents(c_Buffer);

                    for j := 1 to 1 do     // Check only terminal 1 for overloads
                    begin
                        if PDelem.Nphases >= 3 then
                        begin
                            Cmax := 0.0;
                            for i := 1 to 3 do
                            begin
                                k := (j - 1) * Ncond + i;
                                Iph[i] := c_Buffer^[k];
                                Cmag := Cabs(Iph[i]);
                                if Cmag > Cmax then
                                    Cmax := Cmag;
                            end;
                            Phase2SymComp(@Iph, @I012);
                            I0 := Cabs(I012[1]);
                            I1 := Cabs(I012[2]);
                            I2 := Cabs(I012[3]);
                        end
                        else
                        begin
                            I0 := 0.0;
                            I1 := Cabs(c_Buffer^[1 + (j - 1) * NCond]);
                            I2 := 0.0;
                            Cmax := I1;
                        end;

                        if (PdElem.Normamps > 0.0) or (PdElem.Emergamps > 0.0) then
                            if (CMax > PDElem.NormAmps) or (Cmax > pdelem.EmergAmps) then
                            begin
                                FSWrite(F, Pad(EncloseQuotes(PDelem.FullName), MaxDeviceNameLength + 2));
                                FSWrite(F, Format('%3d%8.1f', [j, I1]));
                                if PDElem.Normamps > 0.0 then
                                    FSWrite(F, Format('%8.2f', [Cmax - PDElem.Normamps]))
                                else
                                    FSWrite(F, '     0.0');
                                if PDElem.Normamps > 0.0 then
                                    FSWrite(F, Format('%8.1f', [Cmax / PDElem.Normamps * 100.0]))
                                else
                                    FSWrite(F, '     0.0');
                                if PDElem.Emergamps > 0.0 then
                                    FSWrite(F, Format('%8.1f', [Cmax / PDElem.Emergamps * 100.0]))
                                else
                                    FSWrite(F, '     0.0');
                                FSWrite(F, Format('%8.1f', [I2]));
                                if I1 > 0.0 then
                                    FSWrite(F, Format('%8.1f', [100.0 * I2 / I1]))
                                else
                                    FSWrite(F, '     0.0');
                                FSWrite(F, Format('%8.1f', [I0]));
                                if I1 > 0.0 then
                                    FSWrite(F, Format('%8.1f', [100.0 * I0 / I1]))
                                else
                                    FSWrite(F, '     0.0');
                                FSWriteln(F);
                            end;
                    end; {For}
                end;
            PDelem := DSS.ActiveCircuit.PDElements.Next;
        end;

    finally
        if Assigned(C_buffer) then
            Freemem(c_Buffer);
        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);

    end;
end;

{ - -- - - - - ------------------------------}

procedure ShowUnserved(DSS: TDSSContext; FileNm: String; UE_Only: Boolean);

var
    F: TFileStream = nil;
    PLoad: TLoadObj;
    DoIt: Boolean;

begin
    try

        F := TBufferedFileStream.Create(FileNm, fmCreate);

        FSWriteln(F);
        FSWriteln(F, 'UNSERVED  LOAD  REPORT');
        FSWriteln(F);
        FSWriteln(F, 'Load Element        Bus        Load kW  EEN Factor  UE Factor');
        FSWriteln(F);

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
                    FSWrite(F, Pad(pLoad.Name, 20));
                    FSWrite(F, Pad(pLoad.GetBus(1), 10));
                    FSWrite(F, Format('%8.0f', [pLoad.kWBase]));
                    FSWrite(F, Format('%9.3f', [pLoad.EEN_Factor]));
                    FSWrite(F, Format('%9.3f', [pLoad.UE_Factor]));
                    FSWriteln(F);
                end;

            end;
            pLoad := DSS.ActiveCircuit.Loads.Next;
        end;

    finally

        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);

    end;
end;


procedure ShowLosses(DSS: TDSSContext; FileNm: String);


var
    F: TFileStream = nil;
    PDElem: TPDElement;
    PCElem: TPCElement;

    kLosses,
    TotalLosses,
    LineLosses,
    TransLosses,
    TermPower, LoadPower: Complex;

    sout: String;
begin
    setMaxDeviceNameLength(DSS);

    try

        F := TBufferedFileStream.Create(FileNm, fmCreate);

     {Sequence Currents}
        FSWriteln(F);
        FSWriteln(F, 'LOSSES REPORT');
        FSWriteln(F);
        FSWriteln(F, 'Power Delivery Element Loss Report');
        FSWriteln(F);
        FSWriteln(F, 'Element                  kW Losses    % of Power   kvar Losses');
        FSWriteln(F);


        TotalLosses := CZERO;
        LineLosses := CZERO;
        TransLosses := CZERO;

     // PDELEMENTS
        PDelem := DSS.ActiveCircuit.PDElements.First;
        while PDelem <> NIL do
        begin
            if (PDelem.Enabled)
       {THEN IF (CLASSMASK AND PDElem.DSSObjType) <>  CAP_ELEMENT }    // Ignore capacitors
            then
            begin
        //----PDelem.ActiveTerminalIdx := 1;  // activate 1st terminal for Power call
                kLosses := PDelem.Losses * 0.001;   // kW Losses in element
                TotalLosses += kLosses;
                TermPower := PDelem.power[1] * 0.001;     // Terminal 1 power

                if (CLASSMASK and PDElem.DSSObjType) = XFMR_ELEMENT then
                    TransLosses += kLosses;
                if (CLASSMASK and PDElem.DSSObjType) = AUTOTRANS_ELEMENT then
                    TransLosses += kLosses;
                if (CLASSMASK and PDElem.DSSObjType) = LINE_ELEMENT then
                    LineLosses += kLosses;

                FSWrite(F, Pad(EncloseQuotes(PDelem.FullName), MaxDeviceNameLength + 2));
                FSWrite(F, Format('%10.5f, ', [kLosses.re]));
                if (TermPower.re <> 0.0) and (kLosses.re > 0.0009) then
                    FSWrite(F, Format('%8.2f', [(kLosses.re / Abs(TermPower.re) * 100.0)]))
                else
                    FSWrite(F, Format('%8.1f', [CZERO.RE]));
                FSWrite(F, Format('     %.6g', [kLosses.im]));
                FSWriteln(F);
            end;
            PDelem := DSS.ActiveCircuit.PDElements.Next;
        end;      {While}

        FSWriteln(F);
        WriteStr(sout, Pad('LINE LOSSES=', 30), LineLosses.re: 10: 1, ' kW');
        FSWriteln(F, sout);
        
        WriteStr(sout, Pad('TRANSFORMER LOSSES=', 30), TransLosses.re: 10: 1, ' kW');
        FSWriteln(F, sout);
        
        FSWriteln(F);
        
        WriteStr(sout, Pad('TOTAL LOSSES=', 30), TotalLosses.re: 10: 1, ' kW');
        FSWriteln(F, sout);

        LoadPower := CZERO;
     // Sum the total load kW being served in the Ckt Model
        PCelem := DSS.ActiveCircuit.Loads.First;
        while Pcelem <> NIL do
        begin
            if PcElem.Enabled then
            begin
                LoadPower += PCelem.Power[1];
            end;
            PCelem := DSS.ActiveCircuit.Loads.Next;
        end;
        LoadPower := LoadPower * 0.001;

        FSWriteln(F);
        WriteStr(sout, Pad('TOTAL LOAD POWER = ', 30), Abs(LoadPower.re): 10: 1, ' kW');
        FSWriteln(F, sout);
        
        FSWrite(F, Pad('Percent Losses for Circuit = ', 30));
        if LoadPower.re <> 0.0 then
        begin
            WriteStr(sout, Abs(TotalLosses.re / LoadPower.re) * 100.0: 8: 2, ' %');
            FSWriteln(F, sout);
        end;

    finally

        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);

    end;
end;

procedure ShowVariables(DSS: TDSSContext; FileNm: String);

var
    F: TFileStream = nil;

    pcElem: TPCElement;
    i: Integer;

begin
    try

        F := TBufferedFileStream.Create(FileNm, fmCreate);

     {Sequence Currents}
        FSWriteln(F);
        FSWriteln(F, 'VARIABLES REPORT');
        FSWriteln(F);
        FSWriteln(F, 'Present values of all variables in PC Elements in the circuit.');
        FSWriteln(F);

        pcElem := DSS.ActiveCircuit.PCElements.First;

        while pcElem <> NIL do
        begin
            if pcElem.Enabled and (pcElem.Numvariables > 0) then
            begin
                FSWriteln(F, ['ELEMENT: ', pcElem.ParentClass.Name, '.', pcElem.Name]);
                FSWriteln(F, 'No. of variables: ', IntToStr(pcElem.Numvariables));
                for  i := 1 to pcElem.Numvariables do
                begin
                    FSWriteln(F, ['  ', pcElem.VariableName(i), ' = ', Format('%-.6g', [pcElem.Variable[i]])]);
                end;
                FSWriteln(F);
            end;
            pCElem := DSS.ActiveCircuit.PCElements.Next;
        end;

    finally

        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);

    end;
end;

procedure ShowIsolated(DSS: TDSSContext; FileNm: String);
// Show isolated buses/branches in present circuit
var
    Branch_List,
    SubArea: TCktTree;      // Pointers to all circuit elements

    F: TFileStream = nil;
    TestElement, TestBranch, pElem: TDSSCktElement;

    i, j: Integer;
    sout: String;
begin
     // Make sure bus list is built
    if DSS.ActiveCircuit.BusNameRedefined then
        DSS.ActiveCircuit.ReProcessBusDefs;

    with DSS.ActiveCircuit do
    begin
        // Initialize all Circuit Elements to not checked
        TestElement := CktElements.First;
        while (TestElement <> NIL) do
        begin
            with TestElement do
            begin
                Exclude(Flags, Flg.Checked);
                for i := 1 to Nterms do
                    TerminalsChecked[i - 1] := FALSE;
            end;
            TestElement := CktElements.Next;
        end;

         // initialize the Checked Flag for all Buses
        for j := 1 to NumBuses do
            Buses^[j].BusChecked := FALSE;

    end;

    // Get Started at main voltage source
    TestElement := DSS.ActiveCircuit.Sources.First;
    Branch_List := GetIsolatedSubArea(DSS.ActiveCircuit, TestElement);

    // Show Report of Elements connected and not connected
    try
        F := TBufferedFileStream.Create(FileNm, fmCreate);

        FSWriteln(F);
        FSWriteln(F, 'ISOLATED CIRCUIT ELEMENT REPORT');
        FSWriteln(F);
        FSWriteln(F);
        FSWriteln(F, '***  THE FOLLOWING BUSES HAVE NO CONNECTION TO THE SOURCE ***');
        FSWriteln(F);

        with DSS.ActiveCircuit do
        begin
            for j := 1 to NumBuses do
                if not Buses^[j].BusChecked then
                    FSWriteln(F, EncloseQuotes(BusList.NameOfIndex(j)));
        end;

        FSWriteln(F);
        FSWriteln(F, '***********  THE FOLLOWING SUB NETWORKS ARE ISOLATED ************');
        FSWriteln(F);

        with DSS.ActiveCircuit do
        begin
            TestElement := CktElements.First;

            while TestElement <> NIL do
            begin
                if TestElement.Enabled then
                    if not (Flg.Checked in TestElement.Flags) then
                        if (TestElement.DSSObjType and BASECLASSMASK) = PD_ELEMENT then
                        begin
                            SubArea := GetIsolatedSubArea(DSS.ActiveCircuit, TestElement);
                            FSWriteln(F, '*** START SUBAREA ***');
                            TestBranch := SubArea.First;
                            while TestBranch <> NIL do
                            begin
                                WriteStr(sout, '(', SubArea.Level: 0, ') ', TestBranch.ParentClass.Name, '.', TestBranch.Name);
                                FSWriteln(F, sout);
                                pElem := SubArea.FirstObject;
                                while pElem <> NIL do
                                begin
                                    FSWriteln(F, ['[SHUNT], ', pElem.ParentClass.Name, '.', pElem.Name]);
                                    pElem := Subarea.NextObject
                                end;
                                TestBranch := SubArea.GoForward;
                            end;
                            SubArea.Free;
                            FSWriteln(F);
                        end;
                TestElement := CktElements.Next;
            end;
        end;

        FSWriteln(F);
        FSWriteln(F, '***********  THE FOLLOWING ENABLED ELEMENTS ARE ISOLATED ************');
        FSWriteln(F);

        with DSS.ActiveCircuit do
        begin
            // Mark all controls, energy meters and monitors as checked so they don't show up

            for i := 1 to DSSControls.Count do
                Include(TDSSCktElement(DSSControls.Get(i)).Flags, Flg.Checked);
            for i := 1 to MeterElements.Count do
                Include(TDSSCktElement(MeterElements.Get(i)).Flags, Flg.Checked);

            TestElement := CktElements.First;

            while TestElement <> NIL do
            begin
                if TestElement.Enabled then
                    if not (Flg.Checked in TestElement.Flags) then
                    begin
                        FSWrite(F, '"' + TestElement.FullName + '"');
                        FSWrite(F, '  Buses:');
                        for j := 1 to TestElement.nterms do
                            FSWrite(F, '  "', TestElement.GetBus(j), '"');
                        FSWriteln(F);
                    end;
                TestElement := CktElements.Next;
            end;
        end;

        FSWriteln(F);
        FSWriteln(F, '***  THE FOLLOWING BUSES ARE NOT CONNECTED TO ANY POWER DELIVERY ELEMENT ***');
        FSWriteln(F);

        with DSS.ActiveCircuit do
        begin
            for j := 1 to NumBuses do
                if not Buses^[j].BusChecked then
                    FSWriteln(F, EncloseQuotes(BusList.NameOfIndex(j)));
        end;


        FSWriteln(F);
        FSWriteln(F, '***********  CONNECTED CIRCUIT ELEMENT TREE ************');
        FSWriteln(F);
        FSWriteln(F, '(Lexical Level) Element name');
        FSWriteln(F);

        TestBranch := Branch_List.First;
        while TestBranch <> NIL do
        begin
            FSWriteln(F, ['(', IntToStr(Branch_List.Level), ') ', TestBranch.ParentClass.Name, '.', TestBranch.Name]);
            TestElement := Branch_List.FirstObject;
            while TestElement <> NIL do
            begin
                FSWriteln(F, ['[SHUNT], ', TestElement.ParentClass.Name, '.', TestElement.Name]);
                TestElement := Branch_List.NextObject
            end;
            TestBranch := Branch_List.GoForward;
        end;


    finally

        FreeAndNil(F);
        Branch_List.Free;
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);
    end;
end;

procedure ShowRatings(DSS: TDSSContext; FileNm: String);

var
    F: TFileStream = nil;
    pdElem: TPDElement;

begin
    try

        F := TBufferedFileStream.Create(FileNm, fmCreate);

        FSWriteln(F, 'Power Delivery Elements Normal and Emergency (max) Ratings');
        FSWriteln(F);

        pdElem := DSS.ActiveCircuit.PDElements.First;
        while pdElem <> NIL do
        begin
            FSWrite(F, '"' + pdElem.FullName + '", normamps=');
            FSWrite(F, Format('%-.4g,  %-.4g  !Amps', [pdElem.Normamps, pdElem.EmergAmps]));
            FSWriteln(F);

            pdElem := DSS.ActiveCircuit.PDElements.Next;
        end;
    finally

        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);
    end;
end;

procedure ShowLoops(DSS: TDSSContext; FileNm: String);
// Show loops and paralleled branches in Meter zones
var
    F: TFileStream = nil;
    pdElem: TPDElement;
    hMeter: Integer;
    pMtr: TEnergyMeterObj;
begin
    try
        F := TBufferedFileStream.Create(FileNm, fmCreate);

        FSWriteln(F, 'Loops and Paralleled Lines in all EnergyMeter Zones');
        FSWriteln(F);

        hMeter := DSS.EnergyMeterClass.First;

        while hMeter > 0 do
        begin
            pMtr := TEnergyMeterObj(DSS.ActiveDSSObject);

            if pMtr.BranchList <> NIL then
            begin
                PDElem := pMtr.BranchList.First;
                while PDElem <> NIL do
                begin
                    with pMtr.BranchList.PresentBranch do
                    begin
                        if IsParallel then
                            FSWriteln(F, ['(', pMtr.Name, ') ', PDElem.ParentClass.Name, '.', AnsiUpperCase(PDelem.Name), ': PARALLEL WITH ', TDSSCktElement(LoopLineObj).Parentclass.Name, '.', TDSSCktElement(LoopLineObj).Name]);
                        if IsLoopedHere then
                            FSWriteln(F, ['(', pMtr.Name, ') ', PDElem.ParentClass.Name, '.', AnsiUpperCase(PDelem.Name), ': LOOPED TO     ', TDSSCktElement(LoopLineObj).parentclass.Name, '.', TDSSCktElement(LoopLineObj).Name]);
                    end;
                    PDElem := pMtr.BranchList.GoForward;
                end;
            end;

            hMeter := DSS.EnergyMeterClass.Next
        end;

    finally

        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);
    end;
end;

procedure TopoLevelTabs(F: TFileStream; nLevel: Integer);
var
    nTabs, i: Integer;
begin
    nTabs := 30;
    if nLevel < nTabs then
        nTabs := nLevel;
    for i := 1 to nTabs do
        FSWrite(F, TABCHAR);
    if nLevel > nTabs then
        FSWrite(F, Format('(* %d *)', [nLevel]));
end;

procedure ShowTopology(DSS: TDSSContext; FileRoot: String);
var
    F: TFileStream = nil;
    Ftree: TFileStream = nil;
    FileNm, TreeNm: String;
    pdElem: TPDElement;
    pControlElem: TDSSCktElement;
    LoadElem: TLoadObj;
    topo: TCktTree;
    nLoops, nParallel, nLevels, nIsolated, nSwitches: Integer;
begin
    try
        FileNm := FileRoot + 'TopoSumm.txt';
        TreeNm := FileRoot + 'TopoTree.txt';

        F := TBufferedFileStream.Create(FileNm, fmCreate);
        FSWriteln(F, 'Topology analysis for switch control algorithms');
        FSWriteln(F);

        Ftree := TBufferedFileStream.Create(TreeNm, fmCreate);
        FSWriteln(Ftree, 'Branches and Loads in Circuit ' + DSS.ActiveCircuit.Name);
        FSWriteln(Ftree);

        topo := DSS.ActiveCircuit.GetTopology;
        nLoops := 0;
        nParallel := 0;
        nLevels := 0;
        nIsolated := 0;
        nSwitches := 0;

        if Assigned(topo) then
        begin
            PDElem := topo.First;
            while Assigned(PDElem) do
            begin
                if topo.Level > nLevels then
                    nLevels := topo.Level;
                TopoLevelTabs(Ftree, topo.Level);
                FSWrite(Ftree, PDElem.ParentClass.Name, '.', PDElem.Name);
                with topo.PresentBranch do
                begin
                    if IsParallel then
                    begin
                        Inc(nParallel);
                        FSWrite(Ftree, '(PARALLEL:' + TDSSCktElement(LoopLineObj).FullName + ')');
                    end;
                    if IsLoopedHere then
                    begin
                        Inc(nLoops);
                        FSWrite(Ftree, '(LOOP:' + TDSSCktElement(LoopLineObj).FullName + ')');
                    end;
                    if Flg.HasSensorObj in PDElem.Flags then
                        FSWrite(Ftree, Format(' (Sensor: %s) ', [PDElem.SensorObj.FullName]));
                    if Flg.HasControl in PDElem.Flags then
                    begin
                        pControlElem := PDElem.ControlElementList.First;
                        while pControlElem <> NIL do
                        begin                                // accommodate multiple controls on same branch
                            FSWrite(Ftree, Format(' (Control: %s) ', [pControlElem.FullName]));
                            if ((pControlElem.DSSObjType and CLASSMASK) = SWT_CONTROL) then
                                Inc(nSwitches);
                            pControlElem := PDElem.ControlElementList.Next;
                        end;
                    end;
                    if Flg.HasEnergyMeter in PDElem.Flags then
                        FSWrite(Ftree, Format(' (Meter: %s) ', [PDElem.MeterObj.Name]));
                end;
                FSWriteln(Ftree);

                LoadElem := topo.FirstObject;
                while Assigned(LoadElem) do
                begin
                    TopoLevelTabs(Ftree, topo.Level + 1);
                    FSWrite(Ftree, LoadElem.ParentClass.Name, '.', LoadElem.Name);
                    if Flg.HasSensorObj in LoadElem.Flags then
                        FSWrite(Ftree, Format(' (Sensor: %s) ',[LoadElem.SensorObj.FullName]));
                    if Flg.HasControl in LoadElem.Flags then
                    begin
                        pControlElem := LoadElem.ControlElementList.First;
                        while pControlElem <> NIL do
                        begin                                // accommodate multiple controls on same branch
                            FSWrite(Ftree, Format(' (Control: %s) ', [pControlElem.FullName]));
                            if ((pControlElem.DSSObjType and CLASSMASK) = SWT_CONTROL) then
                                Inc(nSwitches);
                            pControlElem := LoadElem.ControlElementList.Next;
                        end;
                    end;
                    if Flg.HasEnergyMeter in LoadElem.Flags then
                        FSWrite(Ftree, Format(' (Meter: %s) ', [LoadElem.MeterObj.Name]));
                    FSWriteln(Ftree);
                    LoadElem := topo.NextObject
                end;

                PDElem := topo.GoForward;
            end;
        end;

        pdElem := DSS.ActiveCircuit.PDElements.First;
        while assigned(pdElem) do
        begin
            if Flg.IsIsolated in pdElem.Flags then
            begin
                FSWrite(Ftree, Format('Isolated: %s', [PDElem.FullName]));
                if Flg.HasSensorObj in PDElem.Flags then
                    FSWrite(Ftree, Format(' (Sensor: %s) ', [PDElem.SensorObj.FullName]));
                if Flg.HasControl in PDElem.Flags then
                begin
                    pControlElem := PDElem.ControlElementList.First;
                    while pControlElem <> NIL do
                    begin                                // accommodate multiple controls on same branch
                        FSWrite(Ftree, Format(' (Control: %s) ', [pControlElem.FullName]));
                        if ((pControlElem.DSSObjType and CLASSMASK) = SWT_CONTROL) then
                            Inc(nSwitches);
                        pControlElem := PDElem.ControlElementList.Next;
                    end;

                end;
                if Flg.HasEnergyMeter in PDElem.Flags then
                    FSWrite(Ftree, Format(' (Meter: %s) ', [PDElem.MeterObj.Name]));
                FSWriteln(Ftree);
                Inc(nIsolated);
            end;
            pdElem := DSS.ActiveCircuit.PDElements.Next;
        end;

        nLoops := nLoops div 2;  // TODO, see if parallel lines also counted twice
        FSWriteln(F, Format('%d Levels Deep', [nLevels]));
        FSWriteln(F, Format('%d Loops', [nLoops]));
        FSWriteln(F, Format('%d Parallel PD elements', [nParallel]));
        FSWriteln(F, Format('%d Isolated PD components', [nIsolated]));
        FSWriteln(F, Format('%d Controlled Switches', [nSwitches]));

    finally
        FreeAndNil(F);
        FreeAndNil(Ftree);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);
        ShowTreeView(TreeNm);
    end;
end;

procedure ShowLineConstants(DSS: TDSSContext; FileNm: String; Freq: Double; Units: Integer; Rho: Double);
var
    F: TFileStream = nil;
    F2: TFileStream = nil;
    p: Integer;
    Pelem: TLineGeometryObj;
    Z, YC: TCMatrix;
    i, j: Integer;
    w: Double;
    ZS, ZM,
    z1, z0: Complex;
    CS, CM: Double;
    C1, C0: Double;
    YCM: Complex;
    XCM: Double;
    CCM: Double;  // Common mode capacitance
    LineCodesFileNm: String;
begin
    try
        F := TBufferedFileStream.Create(FileNm, fmCreate);

        FSWriteln(F, 'LINE CONSTANTS');
        FSWriteln(F, Format('Frequency = %.6g Hz, Earth resistivity = %.6g ohm-m', [Freq, Rho]));
        FSWriteln(F, 'Earth Model = ', DSS.EarthModelEnum.OrdinalToString(DSS.DefaultEarthModel));
        FSWriteln(F);

        // DSS C-API change: saves the file in the same folder as FileNm
        LineCodesFileNm := ExtractFilePath(FileNm) + 'LineConstantsCode.dss';
        F2 := TBufferedFileStream.Create(LineCodesFileNm, fmCreate);        

        FSWriteln(F2, '!--- OpenDSS Linecodes file generated from Show LINECONSTANTS command');
        FSWriteln(F2, Format('!--- Frequency = %.6g Hz, Earth resistivity = %.6g ohm-m', [Freq, Rho]));
        FSWriteln(F2, '!--- Earth Model = ', DSS.EarthModelEnum.OrdinalToString(DSS.DefaultEarthModel));

        Z := NIL;
        YC := NIL;

        DSS.ActiveEarthModel := DSS.DefaultEarthModel;

        p := DSS.LineGeometryClass.first;
        while p > 0 do
        begin
            Pelem := DSS.LineGeometryClass.GetActiveObj;
            Z.Free;
            YC.Free;

            try
                // Get impedances per unit length
                pelem.RhoEarth := Rho;
                Z := pelem.Zmatrix[freq, 1.0, Units];
                YC := pelem.YCmatrix[freq, 1.0, Units];
            except
                on E: Exception do
                    DoSimpleMsg(DSS, 'Error computing line constants for %s; Error message: %s', [pelem.FullName, E.Message], 9934);
            end;

            FSWriteln(F);
            FSWriteln(F, '--------------------------------------------------');
            FSWriteln(F, 'Geometry Code = ', Pelem.Name);
            FSWriteln(F);
            FSWriteln(F, 'R MATRIX, ohms per ', LineUnitsStr(Units));
            for i := 1 to Z.order do
            begin
                for j := 1 to i do
                begin
                    FSWrite(F, Format('%.6g, ', [Z.GetElement(i, j).re]));
                end;
                FSWriteln(F);
            end;

            FSWriteln(F);
            FSWriteln(F, 'jX MATRIX, ohms per ', LineUnitsStr(Units));
            for i := 1 to Z.order do
            begin
                for j := 1 to i do
                begin
                    FSWrite(F, Format('%.6g, ', [Z.GetElement(i, j).im]));
                end;
                FSWriteln(F);
            end;

            FSWriteln(F);
            FSWriteln(F, 'Susceptance (jB) MATRIX, S per ', LineUnitsStr(Units));
            for i := 1 to Yc.order do
            begin
                for j := 1 to i do
                begin
                    FSWrite(F, Format('%.6g, ', [YC.GetElement(i, j).im]));
                end;
                FSWriteln(F);
            end;

            w := freq * twopi / 1.0E3;
            FSWriteln(F);
            FSWriteln(F, 'L MATRIX, mH per ', LineUnitsStr(Units));
            for i := 1 to Z.order do
            begin
                for j := 1 to i do
                begin
                    FSWrite(F, Format('%.6g, ', [Z.GetElement(i, j).im / w]));
                end;
                FSWriteln(F);
            end;

            w := freq * twopi / 1.0E9;
            FSWriteln(F);
            FSWriteln(F, 'C MATRIX, nF per ', LineUnitsStr(Units));
            for i := 1 to Yc.order do
            begin
                for j := 1 to i do
                begin
                    FSWrite(F, Format('%.6g, ', [YC.GetElement(i, j).im / w]));
                end;
                FSWriteln(F);
            end;

            //Write DSS LineCode record
            //Writeln(F);
            //Writeln(F,'-------------------------------------------------------------------');
            //Writeln(F,'-------------------DSS Linecode Definition-------------------------');
            //Writeln(F,'-------------------------------------------------------------------');
            FSWriteln(F2);

            FSWriteln(F2, Format('New Linecode.%s nphases=%d  Units=%s', [pelem.Name, z.order, LineUnitsStr(Units)]));

            FSWrite(F2, '~ Rmatrix=[');
            for i := 1 to Z.order do
            begin
                for j := 1 to i do
                    FSWrite(F2, Format('%.6g  ', [Z.GetElement(i, j).re]));
                if i < Z.order then
                    FSWrite(F2, '|');
            end;
            FSWriteln(F2, ']');

            FSWrite(F2, '~ Xmatrix=[');
            for i := 1 to Z.order do
            begin
                for j := 1 to i do
                    FSWrite(F2, Format('%.6g  ', [Z.GetElement(i, j).im]));
                if i < Z.order then
                    FSWrite(F2, '|');
            end;
            FSWriteln(F2, ']');

            w := freq * twopi / 1.0E9;
            FSWrite(F2, '~ Cmatrix=[');
            for i := 1 to Yc.order do
            begin
                for j := 1 to i do
                    FSWrite(F2, Format('%.6g  ', [YC.GetElement(i, j).im / w]));
                if i < Yc.order then
                    FSWrite(F2, '|');
            end;
            FSWriteln(F2, ']');

            {Add pos- and zero-sequence approximation here}
            {Kron reduce to 3 phases first}
            {Average diagonals and off-diagonals}

            Zs := CZERO;
            Zm := CZERO;
            CS := 0.0;
            CM := 0.0;

            if Z.order = 3 then
            begin
                FSWriteln(F);
                FSWriteln(F, '-------------------------------------------------------------------');
                FSWriteln(F, '-------------------Equiv Symmetrical Component --------------------');
                FSWriteln(F, '-------------------------------------------------------------------');
                FSWriteln(F);
                for i := 1 to 3 do
                    Zs += Z.GetElement(i, i);
                for i := 1 to 3 do
                    for j := 1 to i - 1 do
                        Zm += Z.GetElement(i, j);

                Z1 := (Zs - Zm) / 3;
                Z0 := (Zm * 2 + Zs) / 3;
                w := freq * twopi / 1000.0;
                FSWriteln(F);
                FSWriteln(F, 'Z1, ohms per ', LineUnitsStr(Units), Format(' = %.6g + j %.6g (L1 = %.6g mH) ', [Z1.re, Z1.im, Z1.im / w]));
                FSWriteln(F, 'Z0, ohms per ', LineUnitsStr(Units), Format(' = %.6g + j %.6g (L0 = %.6g mH) ', [Z0.re, Z0.im, Z0.im / w]));
                FSWriteln(F);

               {Compute Common Mode Series Impedance}
                Z.Invert;
                YCM := CZERO;
                for i := 1 to 3 do    // Add up all elements of Z inverse
                    for j := 1 to 3 do
                        YCM += Z.GetElement(i, j);
                XCM := Cinv(YCM).im;

                w := freq * twopi / 1.0E9;
               {Capacitance}
                for i := 1 to 3 do
                    CS := CS + YC.GetElement(i, i).im;
                for i := 1 to 3 do
                    for j := 1 to i - 1 do
                        CM := CM + YC.GetElement(i, j).im;

                C1 := (CS - CM) / 3.0 / w;   // nF
                C0 := (CS + 2.0 * CM) / 3.0 / w;

               {Compute Common Mode Shunt Capacitance}
                YCM := CZERO;
                for i := 1 to 3 do    // Add up all elements of Z inverse
                    for j := 1 to 3 do
                        YCM += YC.GetElement(i, j);
                CCM := YCM.im / w;

                FSWriteln(F, 'C1, nF per ', LineUnitsStr(Units), Format(' = %.6g', [C1]));
                FSWriteln(F, 'C0, nF per ', LineUnitsStr(Units), Format(' = %.6g', [C0]));
                FSWriteln(F);

                w := freq * twopi;
                FSWriteln(F, 'Surge Impedance:');
                FSWriteln(F, Format('  Positive sequence = %.6g ohms', [Sqrt(Z1.im / w / (C1 * 1.0e-9))]));
                FSWriteln(F, Format('  Zero sequence     = %.6g ohms', [Sqrt(Z0.im / w / (C0 * 1.0e-9))]));
                FSWriteln(F, Format('  Common Mode       = %.6g ohms', [Sqrt(XCM / w / (CCM * 1.0e-9))]));
                FSWriteln(F);

                FSWriteln(F, 'Propagation Velocity (Percent of speed of light):');
                FSWriteln(F, Format('  Positive sequence = %.6g ', [1.0 / (Sqrt(Z1.im / w * (C1 * 1.0e-9))) / 299792458.0 / To_per_Meter(Units) * 100.0]));
                FSWriteln(F, Format('  Zero sequence     = %.6g ', [1.0 / (Sqrt(Z0.im / w * (C0 * 1.0e-9))) / 299792458.0 / To_per_Meter(Units) * 100.0]));
                FSWriteln(F);
            end;

            p := DSS.LineGeometryClass.Next;
        end;

    finally

        FreeAndNil(F);
        FreeAndNil(F2);
        FireOffEditor(DSS, FileNm);
        FireOffEditor(DSS, LineCodesFileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);
    end;
end;

procedure ShowYPrim(DSS: TDSSContext; FileNm: String);

var
    F: TFileStream = nil;
    cValues: pComplexArray;
    i, j: Integer;

begin
    if (DSS.ActiveCircuit = NIL) or (DSS.ActiveCircuit.ActiveCktElement = NIL) then
        Exit;
    with DSS.ActiveCircuit, ActiveCktElement do
    try
        F := TBufferedFileStream.Create(FileNm, fmCreate);
        FSWriteln(F, 'Yprim of active circuit element: ', FullName);
        FSWriteln(F);

        cValues := GetYprimValues(ALL_YPRIM);
        if cValues <> NIL then
        begin
            FSWriteln(F);
            FSWriteln(F, 'G matrix (conductance), S');
            FSWriteln(F);

            for i := 1 to Yorder do
            begin
                for j := 1 to i do
                    FSWrite(F, Format('%13.10g ', [cValues^[i + (j - 1) * Yorder].re]));
                FSWriteln(F);
            end;

            FSWriteln(F);
            FSWriteln(F, 'jB matrix (Susceptance), S');
            FSWriteln(F);

            for i := 1 to Yorder do
            begin
                for j := 1 to i do
                    FSWrite(F, Format('%13.10g ', [cValues^[i + (j - 1) * Yorder].im]));
                FSWriteln(F);
            end;
        end
        else
            FSWriteln(F, 'Yprim matrix is Nil');
    finally
        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);
    end;
end;

// shows how to retrieve the System Y in Triplet form
procedure ShowY(DSS: TDSSContext; FileNm: String);

var
    F: TFileStream = nil;
    hY: NativeUInt;
    nNZ, nBus: Longword;
    i, row, col: Longword;
    re, im: Double;
    ColIdx, RowIdx: array of Longword;
    cVals: array of Complex;

begin
    if DSS.ActiveCircuit = NIL then
        Exit;
    hY := DSS.ActiveCircuit.Solution.hY;
    if hY <= 0 then
    begin
        DoSimpleMsg(DSS, _('Y Matrix not Built.'), 222);
        Exit;
    end;
  // print lower triangle of G and B using new functions
  // this compresses the entries if necessary - no extra work if already solved
    FactorSparseMatrix(hY);
    GetNNZ(hY, @nNZ);
    GetSize(hY, @nBus); // we should already know this

    try
        SetLength(ColIdx, nNZ);
        SetLength(RowIdx, nNZ);
        SetLength(cVals, nNZ);
        GetTripletMatrix(hY, nNZ, @RowIdx[0], @ColIdx[0], @cVals[0]);

        F := TBufferedFileStream.Create(FileNm, fmCreate);

        FSWriteln(F, 'System Y Matrix (Lower Triangle by Columns)');
        FSWriteln(F);
        FSWriteln(F, '  Row  Col               G               B');
        FSWriteln(F);

    // shows how to easily traverse the triplet format
        for i := 0 to nNZ - 1 do
        begin
            col := ColIdx[i] + 1;
            row := RowIdx[i] + 1;
            if row >= col then
            begin
                re := cVals[i].re;
                im := cVals[i].im;
                FSWriteln(F, Format('[%4d,%4d] = %13.10g + j%13.10g', [row, col, re, im]));
            end;
        end;

    finally
        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);
    end;
end;

procedure ShowNodeCurrentSum(DSS: TDSSContext; FileNm: String);

type
    pNodeDoubleArray = ^NodeDoubleArray;
    NodeDoubleArray = array[0..100] of Double;

var
    F: TFileStream = nil;
    i, j, k: Integer;
    nRef: Integer;
    Bname: String;

    pCktElement: TDSSCktElement;
    MaxNodeCurrent: pNodeDoubleArray;
    Ctemp: Complex;
    pctError: String;
    dTemp: Double;

begin
    MaxNodeCurrent := NIL;
    try
        F := TBufferedFileStream.Create(FileNm, fmCreate);

        with DSS.ActiveCircuit, DSS.ActiveCircuit.solution do
        begin
        // Zero out the nodal current array
            for i := 0 to NumNodes do
                Currents^[i] := CZERO;
        // Make temp storage for max current at node
            ReallocMem(MaxNodeCurrent, Sizeof(MaxNodeCurrent^[1]) * (NumNodes + 1));
            for i := 0 to NumNodes do
                MaxNodeCurrent^[i] := 0.0;
        // Now Sum in each device current, keep track of the largest current at a node.
            pCktElement := CktElements.First;
            while pCktElement <> NIL do
            begin
                if pCktElement.Enabled then
                    with pCktElement do
                    begin
                        ComputeIterminal;
                        IF (CLASSMASK AND pCktElement.DSSObjType) =  AUTOTRANS_ELEMENT   Then 
                        Begin
                            k:=0; // Special for Autotransformer
                            for i:= 1 to Nterms do
                            begin
                                for j := 1 to Nphases do 
                                begin
                                    Inc(k);
                                    Ctemp := Iterminal^[k];
                                    nRef := NodeRef^[k];
                                    Currents^[nRef] += Ctemp;  // Noderef=0 is OK
                                    if Cabs(Ctemp) > MaxNodeCurrent^[nRef] then
                                        MaxNodeCurrent^[nRef] := Cabs(Ctemp);
                                end;
                                Inc(k, Nphases);
                            end;
                        end
                        else
                        begin
                            for i := 1 to Yorder do
                            begin
                                Ctemp := Iterminal^[i];
                                nRef := NodeRef^[i];
                                Currents^[nRef] += Ctemp;  // Noderef=0 is OK
                                if Cabs(Ctemp) > MaxNodeCurrent^[nRef] then
                                    MaxNodeCurrent^[nRef] := Cabs(Ctemp);
                            end;
                        end;
                    end;
                pCktElement := CktElements.Next;
            end;

        // Now write report

            SetMaxBusNameLength(DSS);
            MaxBusNameLength := MaxBusNameLength + 2;
            FSWriteln(F);
            FSWriteln(F, 'Node Current Mismatch Report');
            FSWriteln(F);
            FSWriteln(F);
            FSWriteln(F, pad('Bus,', MaxBusNameLength), ' Node, "Current Sum (A)", "%error", "Max Current (A)"');

          // Ground Bus
            nref := 0;
            dTemp := Cabs(Currents^[nref]);
            if (MaxNodeCurrent^[nRef] = 0.0) or (MaxNodeCurrent^[nRef] = dTemp) then
                pctError := Format('%10.1f', [0.0])
            else
                pctError := Format('%10.6f', [dTemp / MaxNodeCurrent^[nRef] * 100.0]);
            BName := Pad('"System Ground"', MaxBusNameLength);
            FSWriteln(F, Format('%s, %2d, %10.5f,       %s, %10.5f', [Bname, nref, dTemp, pctError, MaxNodeCurrent^[nRef]]));


            for i := 1 to DSS.ActiveCircuit.NumBuses do
            begin
                for j := 1 to Buses^[i].NumNodesThisBus do
                begin
                    nref := Buses^[i].GetRef(j);
                    dTemp := Cabs(Currents^[nref]);
                    if (MaxNodeCurrent^[nRef] = 0.0) or (MaxNodeCurrent^[nRef] = dTemp) then
                        pctError := Format('%10.1f', [0.0])
                    else
                        pctError := Format('%10.6f', [dTemp / MaxNodeCurrent^[nRef] * 100.0]);
                    if j = 1 then
                        Bname := Paddots(EncloseQuotes(BusList.NameOfIndex(i)), MaxBusNameLength)
                    else
                        BName := Pad('"   -"', MaxBusNameLength);
                    FSWriteln(F, Format('%s, %2d, %10.5f,       %s, %10.5f', [Bname, Buses^[i].GetNum(j), dTemp, pctError, MaxNodeCurrent^[nRef]]));
                end;
            end;
        end;

    finally
        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);
        ReallocMem(MaxNodeCurrent, 0); // Dispose of temp memory
    end;
end;


procedure ShowkVBaseMismatch(DSS: TDSSContext; FileNm: String);

var
    F: TFileStream = nil;

    pLoad: TLoadObj;
    pGen: TGeneratorObj;
    pBus: TDSSBus;
    BuskV: Double;
    BusName: String;

begin
    try
        F := TBufferedFileStream.Create(FileNm, fmCreate);

        {Check Loads}
        if DSS.ActiveCircuit.Loads.Count > 0 then
        begin
            FSWriteln(F);
            FSWriteln(F, '!!!  LOAD VOLTAGE BASE MISMATCHES');
            FSWriteln(F);
        end;


        pLoad := DSS.ActiveCircuit.Loads.First;
        while pLoad <> NIL do
        begin
           {Find Bus To Which Load Connected}
            pBus := DSS.ActiveCircuit.Buses^[pLoad.Terminals[0].BusRef];
            BusName := DSS.ActiveCircuit.BusList.NameOfIndex(pLoad.Terminals[0].BusRef);
            if pBus.kVBase <> 0.0 then
            begin
                if (pLoad.Nphases = 1) and (pLoad.Connection = TLoadConnection.Wye) then
                begin
                    if abs(pLoad.kVLoadBase - pBus.kVBase) > 0.10 * pBus.kVBase then
                    begin
                        FSWriteln(F, Format('!!!!! Voltage Base Mismatch, %s.kV=%.6g, Bus %s LN kvBase = %.6g', [pLoad.FullName, pLoad.kVLoadBase, pLoad.GetBus(1), pBus.kVBase]));
                        FSWriteln(F, Format('!setkvbase %s kVLN=%.6g', [Busname, pLoad.kVLoadBase]));
                        FSWriteln(F, Format('!%s.kV=%.6g', [pLoad.FullName, pBus.kVBase]));
                    end;
                end
                else
                begin
                    BuskV := pBus.kVBase * SQRT3;
                    if abs(pLoad.kVLoadBase - BuskV) > 0.10 * BuskV then
                    begin
                        FSWriteln(F, Format('!!!!! Voltage Base Mismatch, %s.kV=%.6g, Bus %s kvBase = %.6g', [pLoad.FullName, pLoad.kVLoadBase, pLoad.GetBus(1), BuskV]));
                        FSWriteln(F, Format('!setkvbase %s kVLL=%.6g', [Busname, pLoad.kVLoadBase]));
                        FSWriteln(F, Format('!%s.kV=%.6g', [pLoad.FullName, BuskV]));
                    end;
                end;
            end;
            pLoad := DSS.ActiveCircuit.Loads.Next;
        end;


        {Check Generators}

        if DSS.ActiveCircuit.Generators.Count > 0 then
        begin
            FSWriteln(F);
            FSWriteln(F, '!!!  GENERATOR VOLTAGE BASE MISMATCHES');
            FSWriteln(F);
        end;


        pGen := DSS.ActiveCircuit.Generators.First;
        while pGen <> NIL do
        begin
           {Find Bus To Which Generator Connected}
            pBus := DSS.ActiveCircuit.Buses^[pGen.Terminals[0].BusRef];
            BusName := DSS.ActiveCircuit.BusList.NameOfIndex(pGen.Terminals[0].BusRef);
            if pBus.kVBase <> 0.0 then
            begin
                if (pGen.Nphases = 1) and (pGen.Connection = 0) then
                begin
                    if abs(pGen.Genvars.kVGeneratorBase - pBus.kVBase) > 0.10 * pBus.kVBase then
                    begin
                        FSWriteln(F, Format('!!! Voltage Base Mismatch, %s.kV=%.6g, Bus %s LN kvBase = %.6g', [pGen.FullName, pGen.Genvars.kVGeneratorBase, pGen.GetBus(1), pBus.kVBase]));
                        FSWriteln(F, Format('!setkvbase %s kVLN=%.6g', [Busname, pGen.Genvars.kVGeneratorBase]));
                        FSWriteln(F, Format('!%s.kV=%.6g', [pGen.FullName, pBus.kVBase]));
                    end;
                end
                else
                begin
                    BuskV := pBus.kVBase * SQRT3;
                    if abs(pGen.Genvars.kVGeneratorBase - BuskV) > 0.10 * BuskV then
                    begin
                        FSWriteln(F, Format('!!! Voltage Base Mismatch, %s.kV=%.6g, Bus %s kvBase = %.6g', [pGen.FullName, pGen.Genvars.kVGeneratorBase, pGen.GetBus(1), BuskV]));
                        FSWriteln(F, Format('!setkvbase %s kVLL=%.6g', [Busname, pGen.Genvars.kVGeneratorBase]));
                        FSWriteln(F, Format('!%s.kV=%.6g', [pGen.FullName, BuskV]));
                    end;
                end;
            end;

            pGen := DSS.ActiveCircuit.Generators.Next;
        end;

    finally
        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);
    end;
end;

procedure ShowDeltaV(DSS: TDSSContext; FileNm: String);

var
    F: TFileStream = nil;
    pElem: TDSSCktElement;


begin
    try
        F := TBufferedFileStream.Create(FileNm, fmCreate);

        SetMaxDeviceNameLength(DSS);

        FSWriteln(F);
        FSWriteln(F, 'VOLTAGES ACROSS CIRCUIT ELEMENTS WITH 2 TERMINALS');
        FSWriteln(F);
        FSWriteln(F, 'Source Elements');
        FSWriteln(F);
        FSWriteln(F, pad('Element,', MaxDeviceNameLength), ' Conductor,     Volts,   Percent,           kVBase,  Angle');
        FSWriteln(F);


         // SOURCES first
        pElem := DSS.ActiveCircuit.sources.First;

        while pElem <> NIL do
        begin
            if pElem.Enabled and (pElem.NTerms = 2) then
            begin
                WriteElementDeltaVoltages(DSS, F, pElem);
                FSWriteln(F);
            end;
            pElem := DSS.ActiveCircuit.sources.Next;
        end;

        FSWriteln(F);
        FSWriteln(F, 'Power Delivery Elements');
        FSWriteln(F);
        FSWriteln(F, pad('Element,', MaxDeviceNameLength), ' Conductor,     Volts,   Percent,           kVBase,  Angle');
        FSWriteln(F);


         // PDELEMENTS next
        pElem := DSS.ActiveCircuit.PDElements.First;

        while pElem <> NIL do
        begin
            if pElem.Enabled and (pElem.NTerms = 2) then
            begin
                WriteElementDeltaVoltages(DSS, F, pElem);
                FSWriteln(F);
            end;
            pElem := DSS.ActiveCircuit.PDElements.Next;
        end;

        FSWriteln(F, '= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =');
        FSWriteln(F);
        FSWriteln(F, 'Power Conversion Elements');
        FSWriteln(F);
        FSWriteln(F, pad('Element,', MaxDeviceNameLength), ' Conductor,     Volts,   Percent,           kVBase,  Angle');
        FSWriteln(F);

         // PCELEMENTS next
        pElem := DSS.ActiveCircuit.PCElements.First;

        while pElem <> NIL do
        begin
            if pElem.Enabled and (pElem.NTerms = 2) then
            begin
                WriteElementDeltaVoltages(DSS, F, pElem);
                FSWriteln(F);
            end;
            pElem := DSS.ActiveCircuit.PCElements.Next;
        end;


    finally
        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);
    end;
end;

procedure ShowControlledElements(DSS: TDSSContext; FileNm: String);
var
    F: TFileStream = nil;
    pdelem: TPDElement;
    pctrlelem: TDSSCktElement;
    i: Integer;
begin
    try
        F := TBufferedFileStream.Create(FileNm, fmCreate);

        pdelem := DSS.ActiveCircuit.PDElements.First;
        while pdelem <> NIL do
        begin
            if Flg.HasControl in pdelem.Flags then
            begin
                with pdelem do
                    FSWrite(F, FullName);
                for i := 1 to pdelem.ControlElementList.Count do
                begin
                    pctrlelem := pdelem.ControlElementList.Get(i);
                    with  pctrlelem do
                        FSWrite(F, Format(', %s ', [FullName]));
                end;
                FSWriteln(F);
            end;
            pdelem := DSS.ActiveCircuit.PDElements.Next;
        end;

    finally
        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);
    end;
end;

procedure ShowResult(DSS: TDSSContext; FileNm: String);
var
    F: TFileStream = nil;
begin
    try
        F := TBufferedFileStream.Create(FileNm, fmCreate);

        DSS.ParserVars.Lookup('@result');
        FSWriteln(F, DSS.Parservars.Value);

        DSS.GlobalResult := FileNm;

    finally

        FreeAndNil(F);
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);
    end;
end;

procedure ShowEventLog(DSS: TDSSContext; FileNm: String);


begin
    try

        DSS.EventStrings.SaveToFile(FileNm);
        DSS.GlobalResult := FileNm;

    finally
        FireOffEditor(DSS, FileNm);
        DSS.ParserVars.Add('@lastshowfile', FileNm);
    end;
end;


initialization

    MaxDeviceNameLength := 30;
    MaxBusNameLength := 12;

end.
