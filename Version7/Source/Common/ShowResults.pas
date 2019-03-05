unit ShowResults;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015,  Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
   5-30-00 Added code for handling positive sequence mode
}

interface

procedure ShowVoltages(FileNm: String; LL: Boolean; ShowOptionCode: Integer);
procedure ShowCurrents(FileNm: String; ShowResidual: Boolean; ShowOptionCode: Integer);
procedure ShowPowers(FileNm: String; opt, ShowOptionCode: Integer);
procedure ShowBusPowers(FileNm, BusName: String; opt, ShowOptionCode: Integer);
procedure ShowFaultStudy(FileNm: String);
procedure ShowElements(FileNm: String; ClassName: String);
procedure ShowBuses(FileNm: String);
procedure ShowMeters(FileNm: String);
procedure ShowGenMeters(FileNm: String);
procedure ShowMeterZone(FileNm: String);
procedure ShowLosses(FileNm: String);
procedure ShowRegulatorTaps(FileNm: String);
procedure ShowOverloads(FileNm: String);
procedure ShowUnserved(FileNm: String; UE_Only: Boolean);
procedure ShowVariables(FileNm: String);
procedure ShowIsolated(FileNm: String);
procedure ShowRatings(FileNm: String);
procedure ShowLoops(FileNm: String);
procedure ShowLineConstants(FileNm: String; Freq: Double; Units: Integer; Rho: Double);
procedure ShowYPrim(Filenm: String);
procedure ShowY(FileNm: String);
procedure ShowTopology(FileRoot: String); // summary and tree-view to separate files
procedure ShowNodeCurrentSum(FileNm: String);
procedure ShowkVBaseMismatch(FileNm: String);
procedure ShowDeltaV(FileNm: String);
procedure ShowControlledElements(FileNm: String);
procedure ShowResult(FileNm: String);
procedure ShowEventLog(FileNm: String);

implementation

uses
    uComplex,
    Arraydef,
    sysutils,
    Circuit,
    DSSClass,
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
{$IFDEF FPC}
    CmdForms,
{$ELSE}
    DSSForms,
{$ENDIF}
    Math,
    Line,
    LineUnits,
    LineGeometry,
    YMatrix,
    SwtControl,
    KLUSolve;

var
    MaxBusNameLength: Integer;
    MaxDeviceNameLength: Integer;

const
    TABCHAR: Char = chr(9);

procedure SetMaxBusNameLength;
var
    i: Integer;
begin
    MaxBusNameLength := 4;
    with ActiveCircuit do
        for i := 1 to NumBuses do
            MaxBusNameLength := Max(MaxBusNameLength, Length(BusList.Get(i)));
end;

procedure SetMaxDeviceNameLength;
var
    i: Integer;
    DevName, DevClassName: String;

begin
    MaxDeviceNameLength := 0;
    with ActiveCircuit do
        for i := 1 to NumDevices do
        begin
            DevName := DeviceList.Get(i);
            DevClassName := TDSSClass(DSSClassList.Get(DeviceRef^[i].CktElementClass)).Name;
            MaxDeviceNameLength := Max(MaxDeviceNameLength, (Length(DevName) + Length(DevClassName) + 1));
        end;
end;

procedure WriteSeqVoltages(var F: TextFile; i: Integer; LL: Boolean);

var
    j, k: Integer;
    Vph, VLL, V012: array[1..3] of Complex;
    V0, V1, V2,
    Vpu, V2V1, V0V1: Double;


begin

    with ActiveCircuit do
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
                    VLL[j] := Csub(Vph[j], Vph[k]);
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
            Vph[1] := ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(1)];
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

        Writeln(F, Format('%s %9.4g  %9.4g  %9.4g  %9.4g %9.4g %9.4g', [Pad(BusList.Get(i), MaxBusNameLength), V1, Vpu, V2, V2V1, V0, V0V1]));

    end; {With}


end;

procedure WriteBusVoltages(var F: TextFile; i: Integer; LL: Boolean);

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
    with ActiveCircuit do
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
                Volts := ActiveCircuit.Solution.NodeV^[nref1];

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
                        VoltsLL := Csub(Volts, ActiveCircuit.Solution.NodeV^[nref2]);
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
                    Bname := Paddots(BusList.Get(i), MaxBusNameLength);

                if LL then
                begin
                    if kk > 0 then
                    begin
                        Writeln(F, Format('%s %s %10.5g /_ %6.1f %9.5g %9.3f', [UpperCase(Bname), NodeNameLL, VmagLL, cdang(VoltsLL), VpuLL, kvbase * SQRT3]));
                        Bname := Pad('   -', MaxBusNameLength);
                    end;
                end
                else
                begin
                    Write(F, Format('%s %s %10.5g /_ %6.1f %9.5g %9.3f', [UpperCase(Bname), NodeName, Vmag, cdang(Volts), Vpu, kvbase * SQRT3]));
                    if (NumNodesThisBus > 1) and (kk > 0) and (jj <= 4) then
                        Write(F, Format('        %s %10.5g /_ %6.1f %9.5g', [NodeNameLL, VmagLL, cdang(VoltsLL), VpuLL]));
                    Writeln(F);
                    BName := Pad('   -', MaxBusNameLength);
                end;
            end;
    end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure WriteElementVoltages(var F: TextFile; pElem: TDSSCktElement; LL: Boolean);
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
    Writeln(F, 'ELEMENT = "' + pElem.dssclassname + '.' + UpperCase(pElem.Name) + '"');
    for j := 1 to NTerm do
    begin
        for i := 1 to NCond do
        begin
            Inc(k);
            nref := pElem.NodeRef^[k];
            Volts := ActiveCircuit.Solution.NodeV^[nref];
            Vmag := Cabs(Volts) * 0.001;
            with ActiveCircuit do
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
                Writeln(F, Format('%s  (%3d) %4d    %13.5g (%8.4g) /_ %6.1f', [UpperCase(BusName), nref, MapNodeToBus^[nref].nodenum, Vmag, Vpu, cdang(Volts)]));
            end;
        end;
        if j < Nterm then
            Writeln(F, '------------');
        BusName := Pad(StripExtension(pElem.Nextbus), MaxBusNameLength);
    end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure WriteElementDeltaVoltages(var F: TextFile; pElem: TDSSCktElement);
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

    ElemName := Pad(pElem.dssclassname + '.' + UpperCase(pElem.Name), MaxDeviceNameLength);
    for i := 1 to NCond do
    begin
        Node1 := pElem.NodeRef^[i];
        Node2 := pElem.NodeRef^[i + Ncond];
        if Node1 > 0 then
            Bus1 := ActiveCircuit.MapNodeToBus^[Node1].BusRef
        else
            Bus1 := 0;
        if Node2 > 0 then
            Bus2 := ActiveCircuit.MapNodeToBus^[Node2].BusRef
        else
            Bus2 := 0;
        if (Bus1 > 0) and (Bus2 > 0) then
        begin
            Volts1 := ActiveCircuit.Solution.NodeV^[Node1];   // OK if Node1 or Node2 = 0
            Volts2 := ActiveCircuit.Solution.NodeV^[Node2];
            Volts1 := Csub(Volts1, Volts2);   // diff voltage
            with ActiveCircuit do
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
                Writeln(F, Format('%s,  %4d,    %12.5g, %12.5g, %12.5g, %6.1f', [ElemName, i, Cabs(Volts1), Vmag, Buses^[Bus1].kVBase, cdang(Volts1)]));
            end;
        end;
    end;
end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowVoltages(FileNm: String; LL: Boolean; ShowOptionCode: Integer);

// Show bus voltages by circuit element terminal

var
    F: TextFile;
    i: Integer;
    pElem: TDSSCktElement;

begin

    try
        SetMaxBusNameLength;

        Assignfile(F, FileNm);
        ReWrite(F);

        case ShowOptionCode of
            0:
            begin
                Writeln(F);
                if LL then
                    Writeln(F, 'SYMMETRICAL COMPONENT PHASE-PHASE VOLTAGES BY BUS (for 3-phase buses)')
                else
                    Writeln(F, 'SYMMETRICAL COMPONENT VOLTAGES BY BUS (for 3-phase buses)');
                Writeln(F);
                Writeln(F, pad('Bus', MaxBusNameLength), '  Mag:   V1 (kV)    p.u.     V2 (kV)   %V2/V1    V0 (kV)    %V0/V1');
                Writeln(F);
                for i := 1 to ActiveCircuit.NumBuses do
                    WriteSeqVoltages(F, i, LL);

            end; {ShowOptionCode Case 0}

            1:
            begin

                Writeln(F);
                if LL then
                    Writeln(F, 'LINE-LINE VOLTAGES BY BUS & NODE')
                else
                    Writeln(F, 'LINE-GROUND and LINE-LINE VOLTAGES BY BUS & NODE');
                Writeln(F);
                if LL then
                    Writeln(F, pad('Bus', MaxBusNameLength), ' Node    VLN (kV)   Angle      pu     Base kV ')
                else
                    Writeln(F, pad('Bus', MaxBusNameLength), ' Node    VLN (kV)   Angle      pu     Base kV    Node-Node   VLL (kV)  Angle      pu');
                Writeln(F);

                for i := 1 to ActiveCircuit.NumBuses do
                    WriteBusVoltages(F, i, LL);

            end; {ShowOptionCode Case 1}

            2:
            begin
                Writeln(F);
                Writeln(F, 'NODE-GROUND VOLTAGES BY CIRCUIT ELEMENT');
                Writeln(F);
                Writeln(F, 'Power Delivery Elements');
                Writeln(F);
                Writeln(F, pad('Bus', MaxBusNameLength), ' (node ref)  Phase    Magnitude, kV (pu)    Angle');
                Writeln(F);


       // SOURCES first
                pElem := ActiveCircuit.sources.First;

                while pElem <> NIL do
                begin
                    if pElem.Enabled then
                        WriteElementVoltages(F, pElem, LL);
                    Writeln(F);
                    pElem := ActiveCircuit.sources.Next;
                end;

       // PDELEMENTS first
                pElem := ActiveCircuit.PDElements.First;

                while pElem <> NIL do
                begin
                    if pElem.Enabled then
                        WriteElementVoltages(F, pElem, LL);
                    Writeln(F);
                    pElem := ActiveCircuit.PDElements.Next;
                end;

                Writeln(F, '= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =');
                Writeln(F);
                Writeln(F, 'Power Conversion Elements');
                Writeln(F);
                Writeln(F, pad('Bus', MaxBusNameLength), ' (node ref)  Phase    Magnitude, kV (pu)    Angle');
                Writeln(F);

       // PCELEMENTS next
                pElem := ActiveCircuit.PCElements.First;

                while pElem <> NIL do
                begin
                    if pElem.Enabled then
                        WriteElementVoltages(F, pElem, LL);
                    pElem := ActiveCircuit.PCElements.Next;
                    Writeln(F);
                end;

            end; {ShowOptionCode Case 2}
        else
       {nada}
        end;


    finally

        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);

    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure GetI0I1I2(var I0, I1, I2, Cmax: Double; Nphases, koffset: Integer; cBuffer: pComplexArray);
var
    cmag: Double;
    i: Integer;
    Iph, I012: array[1..3] of Complex;

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


procedure WriteSeqCurrents(var F: TextFile; const PaddedBrName: String; I0, I1, I2, Cmax, NormAmps, EmergAmps: Double; j, DSSObjType: Integer);

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

    Writeln(F,
        Format('%s %3d  %10.5g   %10.5g %8.2f  %10.5g %8.2f  %8.2f %8.2f',
        [UpperCase(Name), j, I1, I2, I2I1, I0, I0I1, Inormal, Iemerg]));

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure WriteTerminalCurrents(var F: TextFile; pElem: TDSSCktElement; ShowResidual: Boolean);

var

    j, i, k, Ncond, Nterm: Integer;
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
        Writeln(F, 'ELEMENT = ', FullName(Pelem));
        for      j := 1 to NTerm do
        begin
            Ctotal := CZERO;
            for    i := 1 to NCond do
            begin
                Inc(k);
                if ShowResidual then
                    Caccum(Ctotal, cBuffer^[k]);
                Writeln(F, Format('%s  %4d    %13.5g /_ %6.1f =  %9.5g +j %9.5g', [UpperCase(FromBus), GetNodeNum(pElem.NodeRef^[k]), Cabs(cBuffer^[k]), cdang(cBuffer^[k]), cBuffer^[k].re, cBuffer^[k].im]));
            end;
            if ShowResidual and (pElem.NPhases > 1) then
            begin
                ResidPolar := CtoPolardeg(cnegate(Ctotal));
                Writeln(F, Format('%s Resid    %13.5g /_ %6.1f =   %9.5g +j %9.5g', [UpperCase(FromBus), ResidPolar.mag, ResidPolar.ang, -cTotal.re, -Ctotal.im]));
            end;
            if j < Nterm then
                Writeln(F, '------------');
            FromBus := Pad(StripExtension(pElem.Nextbus), MaxBusNameLength);
        end;
        Writeln(F);

    finally

        if Assigned(cBuffer) then
            Freemem(cBuffer);

    end;

end;
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowCurrents(FileNm: String; ShowResidual: Boolean; ShowOptionCode: Integer);


var
    F: TextFile;
    cBuffer: pComplexArray;
    NCond, Nterm, j: Integer;
    pElem: TDSSCktElement;
    PDElem: TPDElement;
    PCelem: TPCelement;
    I0, I1, I2,
    Cmax: Double;

begin

    SetMaxDeviceNameLength;
    SetMaxBusNameLength;
    try
        try

            Assignfile(F, FileNm);
            ReWrite(F);
            case ShowOptionCode of

                0:
                begin  {Sequence Currents}

                    Writeln(F);
                    Writeln(F, 'SYMMETRICAL COMPONENT CURRENTS BY CIRCUIT ELEMENT (first 3 phases)');
                    Writeln(F);
                    Writeln(F, Pad('Element', maxDeviceNameLength + 2), ' Term      I1         I2         %I2/I1    I0         %I0/I1   %Normal %Emergency');
                    Writeln(F);


//Sources First
                    Pelem := ActiveCircuit.Sources.First;
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
                                    WriteSeqCurrents(F, Paddots(FullName(pElem), MaxDeviceNameLength + 2), I0, I1, I2, Cmax, 0.0, 0.0, j, DSSObjType);
                            end;
                            Freemem(cBuffer);
                        end;
                        pelem := ActiveCircuit.Sources.Next;
                    end;


     // PDELEMENTS Next
                    PDelem := ActiveCircuit.PDElements.First;

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
                                    WriteSeqCurrents(F, Paddots(FullName(pdElem), MaxDeviceNameLength + 2), I0, I1, I2, Cmax, Normamps, Emergamps, j, DSSObjType);
                            end; {For}
                            Freemem(cBuffer);
                        end;
                        PDelem := ActiveCircuit.PDElements.Next;
                    end;

    // PCelemENTS next
                    PCelem := ActiveCircuit.PCelements.First;

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
                                    WriteSeqCurrents(F, Paddots(FullName(pcElem), MaxDeviceNameLength + 2), I0, I1, I2, Cmax, 0.0, 0.0, j, DSSObjType);
                            end;
                            Freemem(cBuffer);
                        end;
                        PCelem := ActiveCircuit.PCelements.Next;
                    end;


     //Faults next
                    Pelem := ActiveCircuit.Faults.First;
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
                                    WriteSeqCurrents(F, Paddots(FullName(pElem), MaxDeviceNameLength + 2), I0, I1, I2, Cmax, 0.0, 0.0, j, DSSObjType);
                            end;
                            Freemem(cBuffer);
                        end;
                        pelem := ActiveCircuit.Faults.Next;
                    end;

                end; {Code 0:}

                1:
                begin  {Element branch Currents}


                    Writeln(F);
                    Writeln(F, 'CIRCUIT ELEMENT CURRENTS');
                    Writeln(F);
                    Writeln(F, '(Currents into element from indicated bus)');
                    Writeln(F);
                    Writeln(F, 'Power Delivery Elements');
                    Writeln(F);
                    Writeln(F, Pad('  Bus', MaxBusNameLength), ' Phase    Magnitude, A     Angle      (Real)   +j  (Imag)');
                    Writeln(F);


     // Sources first
                    pElem := ActiveCircuit.Sources.First;

                    while pElem <> NIL do
                    begin
                        if pElem.Enabled then
                            WriteTerminalCurrents(F, pElem, FALSE);
                        pElem := ActiveCircuit.Sources.Next;
                    end;

     // PDELEMENTS first
                    pElem := ActiveCircuit.PDElements.First;

                    while pElem <> NIL do
                    begin
                        if pElem.Enabled then
                            WriteTerminalCurrents(F, pElem, ShowResidual);
                        pElem := ActiveCircuit.PDElements.Next;
                    end;

     // Faults
                    pElem := ActiveCircuit.Faults.First;

                    while pElem <> NIL do
                    begin
                        if pElem.Enabled then
                            WriteTerminalCurrents(F, pElem, FALSE);
                        pElem := ActiveCircuit.Faults.Next;
                    end;


                    Writeln(F, '= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =');
                    Writeln(F);
                    Writeln(F, 'Power Conversion Elements');
                    Writeln(F);
                    Writeln(F, Pad('  Bus', MaxBusNameLength), ' Phase    Magnitude, A     Angle      (Real)   +j  (Imag)');
                    Writeln(F);

     // PCELEMENTS next
                    pElem := ActiveCircuit.PCElements.First;

                    while pElem <> NIL do
                    begin
                        if pElem.Enabled then
                            WriteTerminalCurrents(F, pElem, FALSE);
                        pElem := ActiveCircuit.PCElements.Next;
                    end;

                end;  {code:1}

            else

            end; {CASE}

        finally

            CloseFile(F);
            FireOffEditor(FileNm);
            ParserVars.Add('@lastshowfile', FileNm);

        end;

    except
        On E: Exception do
            DoSimpleMsg('Exception raised in ShowCurrents: ' + E.Message, 2190);
    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowPowers(FileNm: String; opt, ShowOptionCode: Integer);

{Opt = 0: kVA
 opt = 1: MVA
 }

var
    FromBus: String;
    F: TextFile;
    c_Buffer: pComplexArray;
    NCond, Nterm, i, j, k: Integer;
    p_Elem: TDSSCktElement;
    PDElem: TPDElement;
    PCElem: TPCElement;
    Volts: Complex;
    S, Saccum: Complex;
    nref: Integer;
    Vph, V012: array[1..3] of Complex;
    Iph, I012: array[1..3] of Complex;


begin

    c_Buffer := NIL;
    SetMaxDeviceNameLength;
    SetMaxBusNameLength;

    try
        Assignfile(F, FileNm);
        ReWrite(F);

      {Allocate c_Buffer big enough for largest circuit element}
        Getmem(c_buffer, sizeof(c_Buffer^[1]) * GetMaxCktElementSize);

        case ShowOptionCode of
            0:
            begin
     {Sequence Currents}
                Writeln(F);
                Writeln(F, 'SYMMETRICAL COMPONENT POWERS BY CIRCUIT ELEMENT (first 3 phases)                                     Excess Power');
                Writeln(F);
                case Opt of
                    1:
                        Writeln(F, Pad('Element', MaxDeviceNameLength + 2), ' Term    P1(MW)   Q1(Mvar)       P2         Q2      P0      Q0       P_Norm      Q_Norm     P_Emerg    Q_Emerg');
                else
                    Writeln(F, Pad('Element', MaxDeviceNameLength + 2), ' Term    P1(kW)   Q1(kvar)       P2         Q2      P0      Q0       P_Norm      Q_Norm     P_Emerg    Q_Emerg');
                end;
                Writeln(F);

     // Sources first
                p_Elem := ActiveCircuit.Sources.First;

                while p_Elem <> NIL do
                begin
                    if (p_Elem.Enabled) then
                    begin
                        NCond := p_Elem.NConds;
                        Nterm := p_Elem.Nterms;
                        p_Elem.GetCurrents(c_Buffer);

                        for j := 1 to NTerm do
                        begin
                            Write(F, Pad(FullName(p_Elem), MaxDeviceNameLength + 2), j: 3);
                            for i := 1 to min(3, p_Elem.Nphases) do
                            begin
                                k := (j - 1) * Ncond + i;
                                nref := p_Elem.NodeRef^[k];
                                Volts := ActiveCircuit.Solution.NodeV^[nref];
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
                            Write(F, S.re * 0.003: 11: 1);
                            Write(F, S.im * 0.003: 11: 1);
                            S := Cmul(V012[3], conjg(I012[3]));
                            if Opt = 1 then
                                S := CmulReal(S, 0.001);
                            Write(F, S.re * 0.003: 11: 1);
                            Write(F, S.im * 0.003: 11: 1);
                            S := Cmul(V012[1], conjg(I012[1]));
                            if Opt = 1 then
                                S := CmulReal(S, 0.001);
                            Write(F, S.re * 0.003: 8: 1);
                            Write(F, S.im * 0.003: 8: 1);
                            Writeln(F);

                        end;
                    end;
                    p_Elem := ActiveCircuit.Sources.Next;
                end;


     // PDELEMENTS next
                PDElem := ActiveCircuit.PDElements.First;

                while PDElem <> NIL do
                begin
                    if (PDElem.Enabled) then
                    begin
                        NCond := pDElem.NConds;
                        Nterm := pDElem.Nterms;
                        PDElem.GetCurrents(c_Buffer);

                        for j := 1 to NTerm do
                        begin
                            Write(F, Pad(FullName(pDElem), MaxDeviceNameLength + 2), j: 3);
                            for i := 1 to Min(3, pdelem.Nphases) do
                            begin
                                k := (j - 1) * Ncond + i;
                                nref := pDElem.NodeRef^[k];
                                Volts := ActiveCircuit.Solution.NodeV^[nref];
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
                            Write(F, S.re * 0.003: 11: 1);
                            Write(F, S.im * 0.003: 11: 1);
                            S := Cmul(V012[3], conjg(I012[3]));
                            if Opt = 1 then
                                S := CmulReal(S, 0.001);
                            Write(F, S.re * 0.003: 11: 1);
                            Write(F, S.im * 0.003: 11: 1);
                            S := Cmul(V012[1], conjg(I012[1]));
                            if Opt = 1 then
                                S := CmulReal(S, 0.001);
                            Write(F, S.re * 0.003: 8: 1);
                            Write(F, S.im * 0.003: 8: 1);

                            if j = 1 then
                            begin
               //----PDelem.ActiveTerminalIdx := 1;
                                S := PDElem.ExcesskVANorm[1];
                                if Opt = 1 then
                                    S := CmulReal(S, 0.001);
                                Write(F, S.re: 11: 1);
                                Write(F, S.im: 11: 1);
                                S := PDElem.ExcesskVAEmerg[1];
                                if Opt = 1 then
                                    S := CmulReal(S, 0.001);
                                Write(F, S.re: 11: 1);
                                Write(F, S.im: 11: 1);
                            end;
                            Writeln(F);

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
                        PCElem.GetCurrents(c_Buffer);

                        for j := 1 to NTerm do
                        begin
                            Write(F, Pad(FullName(pCElem), MaxDeviceNameLength + 2), j: 3);
                            for i := 1 to min(3, pcElem.Nphases) do
                            begin
                                k := (j - 1) * Ncond + i;
                                nref := PCElem.NodeRef^[k];
                                Volts := ActiveCircuit.Solution.NodeV^[nref];
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
                            Write(F, S.re * 0.003: 11: 1);
                            Write(F, S.im * 0.003: 11: 1);
                            S := Cmul(V012[3], conjg(I012[3]));
                            if Opt = 1 then
                                S := CmulReal(S, 0.001);
                            Write(F, S.re * 0.003: 11: 1);
                            Write(F, S.im * 0.003: 11: 1);
                            S := Cmul(V012[1], conjg(I012[1]));
                            if Opt = 1 then
                                S := CmulReal(S, 0.001);
                            Write(F, S.re * 0.003: 8: 1);
                            Write(F, S.im * 0.003: 8: 1);


                            Writeln(F);

                        end;
                    end;
                    PCElem := ActiveCircuit.PCElements.Next;
                end;
            end; {ShowOptionCode=0}

            1:
            begin

     {Branch Powers}
                Writeln(F);
                Writeln(F, 'CIRCUIT ELEMENT POWER FLOW');
                Writeln(F);
                Writeln(F, '(Power Flow into element from indicated Bus)');
                Writeln(F);
                Writeln(F, 'Power Delivery Elements');
                Writeln(F);
                case Opt of
                    1:
                        Writeln(F, Pad('  Bus', MaxBusNameLength), ' Phase     MW     +j   Mvar         MVA         PF');
                else
                    Writeln(F, Pad('  Bus', MaxBusNameLength), ' Phase     kW     +j   kvar         kVA         PF');
                end;
                Writeln(F);


     // Sources first
                p_Elem := ActiveCircuit.sources.First;

                while p_Elem <> NIL do
                begin
                    if p_Elem.Enabled then
                    begin
                        NCond := p_Elem.NConds;
                        Nterm := p_Elem.Nterms;
                        p_Elem.GetCurrents(c_Buffer);
                        k := 0;
                        FromBus := Pad(StripExtension(p_Elem.FirstBus), MaxBusNameLength);
                        Writeln(F, 'ELEMENT = ', FullName(P_Elem));
                        for j := 1 to NTerm do
                        begin
                            Saccum := CZERO;
                            for i := 1 to NCond do
                            begin
                                Inc(k);
                                nref := p_Elem.NodeRef^[k];
                                Volts := ActiveCircuit.Solution.NodeV^[nref];
                                S := Cmul(Volts, conjg(c_Buffer^[k]));
                                if { (p_Elem.nphases=1) and } ActiveCircuit.PositiveSequence then
                                    S := CmulReal(S, 3.0);
                                if Opt = 1 then
                                    S := CmulReal(S, 0.001);
                                Caccum(Saccum, S);
                                Write(F, UpperCase(FromBus), '  ', GetNodeNum(p_Elem.NodeRef^[k]): 4, '    ', S.re / 1000.0: 8: 1, ' +j ', S.im / 1000.0: 8: 1);
                                Writeln(F, '   ', Cabs(S) / 1000.0: 8: 1, '     ', PowerFactor(S): 8: 4);
                            end;
                            Write(F, Paddots('   TERMINAL TOTAL', MaxBusNameLength + 10), Saccum.re / 1000.0: 8: 1, ' +j ', Saccum.im / 1000.0: 8: 1);
                            Writeln(F, '   ', Cabs(Saccum) / 1000.0: 8: 1, '     ', PowerFactor(Saccum): 8: 4);
                            FromBus := Pad(StripExtension(p_Elem.Nextbus), MaxBusNameLength);
                        end;
                        Writeln(F);
                    end;
                    p_Elem := ActiveCircuit.sources.Next;
                end;

     // PDELEMENTS first
                p_Elem := ActiveCircuit.PDElements.First;

                while p_Elem <> NIL do
                begin
                    if p_Elem.Enabled then
                    begin
                        NCond := p_Elem.NConds;
                        Nterm := p_Elem.Nterms;
                        p_Elem.GetCurrents(c_Buffer);
                        k := 0;
                        FromBus := Pad(StripExtension(p_Elem.FirstBus), MaxBusNameLength);
                        Writeln(F, 'ELEMENT = ', FullName(p_elem));
                        for j := 1 to NTerm do
                        begin
                            Saccum := CZERO;
                            for i := 1 to NCond do
                            begin
                                Inc(k);
                                nref := p_Elem.NodeRef^[k];
                                Volts := ActiveCircuit.Solution.NodeV^[nref];
                                S := Cmul(Volts, conjg(c_Buffer^[k]));
                                if { (p_Elem.nphases=1) and } ActiveCircuit.PositiveSequence then
                                    S := CmulReal(S, 3.0);
                                if Opt = 1 then
                                    S := CmulReal(S, 0.001);
                                Caccum(Saccum, S);
                                Write(F, UpperCase(FromBus), '  ', GetNodeNum(p_Elem.NodeRef^[k]): 4, '    ', S.re / 1000.0: 8: 1, ' +j ', S.im / 1000.0: 8: 1);
                                Writeln(F, '   ', Cabs(S) / 1000.0: 8: 1, '     ', PowerFactor(S): 8: 4);
                            end;
                            Write(F, Paddots('   TERMINAL TOTAL', MaxBusNameLength + 10), Saccum.re / 1000.0: 8: 1, ' +j ', Saccum.im / 1000.0: 8: 1);
                            Writeln(F, '   ', Cabs(Saccum) / 1000.0: 8: 1, '     ', PowerFactor(Saccum): 8: 4);
                            FromBus := Pad(StripExtension(p_Elem.Nextbus), MaxBusNameLength);
                        end;
                        Writeln(F);
                    end;
                    p_Elem := ActiveCircuit.PDElements.Next;
                end;

                Writeln(F, '= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =');
                Writeln(F);
                Writeln(F, 'Power Conversion Elements');
                Writeln(F);
                case Opt of
                    1:
                        Writeln(F, Pad('  Bus', MaxBusNameLength), ' Phase     MW   +j  Mvar         MVA         PF');
                else
                    Writeln(F, Pad('  Bus', MaxBusNameLength), ' Phase     kW   +j  kvar         kVA         PF');
                end;
                Writeln(F);

     // PCELEMENTS next
                p_Elem := ActiveCircuit.PCElements.First;

                while p_Elem <> NIL do
                begin
                    if p_Elem.Enabled then
                    begin

                        NCond := p_Elem.NConds;
                        Nterm := p_Elem.Nterms;
                        p_Elem.GetCurrents(c_Buffer);
                        k := 0;
                        FromBus := Pad(StripExtension(p_Elem.FirstBus), MaxBusNameLength);
                        Writeln(F, 'ELEMENT = ', Fullname(P_Elem));
                        for j := 1 to NTerm do
                        begin
                            Saccum := CZERO;
                            for i := 1 to NCond do
                            begin
                                Inc(k);
                                nref := p_Elem.NodeRef^[k];
                                Volts := ActiveCircuit.Solution.NodeV^[nref];
                                S := Cmul(Volts, conjg(c_Buffer^[k]));
                                if { (p_Elem.nphases=1) and } ActiveCircuit.PositiveSequence then
                                    S := CmulReal(S, 3.0);
                                if Opt = 1 then
                                    S := CmulReal(S, 0.001);
                                Caccum(Saccum, S);
                                Write(F, UpperCase(FromBus), '  ', GetNodeNum(p_Elem.NodeRef^[k]): 4, '    ', S.re / 1000.0: 6: 1, ' +j ', S.im / 1000.0: 6: 1);
                                Writeln(F, '   ', Cabs(S) / 1000.0: 8: 1, '     ', PowerFactor(S): 8: 4);
                            end;
                            Write(F, Paddots('  TERMINAL TOTAL ', MaxBusNameLength + 10), Saccum.re / 1000.0: 8: 1, ' +j ', Saccum.im / 1000.0: 8: 1);
                            Writeln(F, '   ', Cabs(Saccum) / 1000.0: 8: 1, '     ', PowerFactor(Saccum): 8: 4);
                            FromBus := Pad(StripExtension(p_Elem.Nextbus), MaxBusNameLength);
                        end;
                        Writeln(F);
                    end;
                    p_Elem := ActiveCircuit.PCElements.Next;
                end;

            end; {ShowOptionCode=1}

        else

        end; {CASE}

        Writeln(F);
        S := CmulReal(ActiveCircuit.Losses, 0.001);
        if Opt = 1 then
            S := CmulReal(S, 0.001);
        Writeln(F, 'Total Circuit Losses = ', S.re: 6: 1, ' +j ', S.im: 6: 1);

    finally

        if Assigned(C_buffer) then
            Freemem(c_Buffer);
        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);


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
            if Terminals^[i].BusRef = BusReference then
            begin
                TerminalIndex := i;
                Result := TRUE;
                Break;
            end;
        end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure WriteTerminalPowerSeq(var F: TextFile; cktElem: TDSSCktElement; j, opt: Integer);
var
    i, k, Ncond, nref: Integer;
    Volts, S: Complex;
    Vph, V012: array[1..3] of Complex;
    Iph, I012: array[1..3] of Complex;
    c_Buffer: pComplexArray;  // Allocate to max total conductors

begin

    c_Buffer := NIL;


    try
 {Allocate c_Buffer big enough for this circuit element}
        Getmem(c_buffer, sizeof(c_Buffer^[1]) * cktElem.Yorder);

        NCond := cktElem.NConds;
        cktElem.GetCurrents(c_Buffer);
        Write(F, Pad(FullName(cktElem), MaxDeviceNameLength + 2), j: 3);
        for i := 1 to Min(cktElem.Nphases, 3) do
        begin
            k := (j - 1) * Ncond + i;
            nref := cktElem.NodeRef^[k];
            Volts := ActiveCircuit.Solution.NodeV^[nref];
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


  // Pos Seq or Single Phase
        case cktElem.Nphases of
            1:
                S := Cmul(Vph[1], conjg(Iph[1]));
            2:
                S := Cadd(Cmul(Vph[1], conjg(Iph[1])), Cmul(Vph[2], conjg(Iph[3])));
        else
            S := Cmul(V012[2], conjg(I012[2]));
        end;

        if Opt = 1 then
            S := CmulReal(S, 0.001);
        Write(F, S.re * 0.003: 11: 1);
        Write(F, S.im * 0.003: 11: 1);
        S := Cmul(V012[3], conjg(I012[3]));
        if Opt = 1 then
            S := CmulReal(S, 0.001);
        Write(F, S.re * 0.003: 11: 1);
        Write(F, S.im * 0.003: 11: 1);
        S := Cmul(V012[1], conjg(I012[1]));
        if Opt = 1 then
            S := CmulReal(S, 0.001);
        Write(F, S.re * 0.003: 8: 1);
        Write(F, S.im * 0.003: 8: 1);

        Writeln(F);

    finally
        if Assigned(C_buffer) then
            Freemem(c_Buffer);
    end;

end;


procedure WriteTerminalPower(var F: TextFile; CktElem: TDSSCktElement; jTerm, opt: Integer);

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
        Writeln(F, 'ELEMENT = ', Pad(FullName(cktElem), MaxDeviceNameLength + 2));

        Saccum := CZERO;
        for i := 1 to NCond do
        begin
            k := (jTerm - 1) * Ncond + i;
            nref := CktElem.NodeRef^[k];
            Volts := ActiveCircuit.Solution.NodeV^[nref];
            S := Cmul(Volts, conjg(c_Buffer^[k]));
            if { (CktElem.nphases=1) and } ActiveCircuit.PositiveSequence then
                S := CmulReal(S, 3.0);
            if Opt = 1 then
                S := CmulReal(S, 0.001);
            Caccum(Saccum, S);
            Writeln(F, Format('%s %4d %10.5g +j %10.5g    %10.5g    %8.4f',
                [UpperCase(FromBus), GetNodeNum(CktElem.NodeRef^[k]), S.re / 1000.0, S.im / 1000.0,
                Cabs(S) / 1000.0, PowerFactor(S)]));
        end;
        Writeln(F, Format(' TERMINAL TOTAL   %10.5g +j %10.5g    %10.5g    %8.4f',
            [Saccum.re / 1000.0, Saccum.im / 1000.0, Cabs(Saccum) / 1000.0,
            PowerFactor(Saccum)]));

    finally
        if Assigned(C_buffer) then
            Freemem(c_Buffer);
    end;

end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowBusPowers(FileNm, BusName: String; opt, ShowOptionCode: Integer);

{Report power flow around a specified Bus}

{Opt = 0: kVA
 opt = 1: MVA
 }

var

    F: TextFile;

    j, Ncond, Nterm: Integer;
    p_Elem: TDSSCktElement;
    PDElem: TPDElement;
    PCElem: TPCElement;
    I0, I1, I2, Cmax: Double;
    c_Buffer: pComplexArray;  // Allocate to max total conductors
    BusReference: Integer;
    jTerm: Integer;


begin

    SetMaxDeviceNameLength;
    c_Buffer := NIL;

  {Get Bus Reference}
    BusReference := ActiveCircuit.BusList.Find(BusName);
    if BusReference = 0 then
    begin
        DoSimpleMsg('Bus "' + UpperCase(BusName) + '" not found.', 219);
        Exit;
    end;
    try
        Assignfile(F, FileNm);
        ReWrite(F);

      {Allocate c_Buffer big enough for largest circuit element}
        Getmem(c_buffer, sizeof(c_Buffer^[1]) * GetMaxCktElementSize);

        case ShowOptionCode of
            0:
            begin

     {Write Bus Voltage}

                Writeln(F);
                Writeln(F, 'Bus      V1 (kV)    p.u.    V2 (kV)      %V2/V1    V0 (kV)  %V0/V1');
                Writeln(F);

                WriteSeqVoltages(F, BusReference, FALSE);

     {Sequence Currents}
                Writeln(F);
                Writeln(F, 'SYMMETRICAL COMPONENT CURRENTS BY CIRCUIT ELEMENT (first 3 phases)');
                Writeln(F);
                Writeln(F, 'Element                Term      I1         I2       %I2/I1       I0      %I0/I1   %Normal %Emergency');
                Writeln(F);

     // Sources first
                p_Elem := ActiveCircuit.Sources.First;

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
                                    WriteSeqCurrents(F, Paddots(FullName(p_Elem), MaxDeviceNameLength + 2), I0, I1, I2, Cmax, 0.0, 0.0, j, DSSObjType);
                            end;

                        end;
                    p_Elem := ActiveCircuit.Sources.Next;
                end;


     // PDELEMENTS next
                PDElem := ActiveCircuit.PDElements.First;
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
                                    WriteSeqCurrents(F, Paddots(FullName(PDElem), MaxDeviceNameLength + 2), I0, I1, I2, Cmax, 0.0, 0.0, j, DSSObjType);
                            end;
                        end;
                    PDElem := ActiveCircuit.PDElements.Next;
                end;

     // PCELEMENTS Next
                PCElem := ActiveCircuit.PCElements.First;
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
                                    WriteSeqCurrents(F, Paddots(FullName(PCelem), MaxDeviceNameLength + 2), I0, I1, I2, Cmax, 0.0, 0.0, j, DSSObjType);
                            end;
                        end;
                    PCElem := ActiveCircuit.PCElements.Next;
                end;

     {Sequence Powers}
                Writeln(F);
                Writeln(F, 'SYMMETRICAL COMPONENT POWERS BY CIRCUIT ELEMENT (first 3 phases)');
                Writeln(F);
                case Opt of
                    1:
                        Writeln(F, 'Element                      Term    P1(MW)   Q1(Mvar)       P2         Q2      P0      Q0   ');
                else
                    Writeln(F, 'Element                      Term    P1(kW)   Q1(kvar)         P2         Q2      P0      Q0  ');
                end;
                Writeln(F);

     // Sources first
                p_Elem := ActiveCircuit.Sources.First;

                while p_Elem <> NIL do
                begin
                    if (p_Elem.Enabled) then
                        if CheckBusReference(p_Elem, BusReference, j) then
                        begin
        {Use j set by CheckBusReference}
                            WriteTerminalPowerSeq(F, p_Elem, j, opt);
                        end;
                    p_Elem := ActiveCircuit.Sources.Next;
                end;


     // PDELEMENTS next
                PDElem := ActiveCircuit.PDElements.First;
                while PDElem <> NIL do
                begin
                    if (PDElem.Enabled) then
                        if CheckBusReference(PDElem, BusReference, j) then
                        begin  // Is this connected to the bus
                            WriteTerminalPowerSeq(F, PDElem, j, opt);
                        end;
                    PDElem := ActiveCircuit.PDElements.Next;
                end;

     // PCELEMENTS Next
                PCElem := ActiveCircuit.PCElements.First;
                while PCElem <> NIL do
                begin
                    if (PCElem.Enabled) then
                        if CheckBusReference(PCElem, BusReference, j) then
                        begin
                            WriteTerminalPowerSeq(F, PCElem, j, opt)
                        end;
                    PCElem := ActiveCircuit.PCElements.Next;
                end;
            end; {ShowOptionCode=0}

            1:
            begin

     {Write Bus Voltage}

                Writeln(F);
                Writeln(F, '  Bus   (node ref)  Node       V (kV)    Angle    p.u.   Base kV');
                Writeln(F);
                WriteBusVoltages(F, BusReference, FALSE);

     {Element Currents}
                Writeln(F);
                Writeln(F, 'CIRCUIT ELEMENT CURRENTS');
                Writeln(F);
                Writeln(F, '(Currents into element from indicated bus)');
                Writeln(F);
                Writeln(F, 'Power Delivery Elements');
                Writeln(F);
                Writeln(F, '  Bus         Phase    Magnitude, A     Angle      (Real)   +j  (Imag)');
                Writeln(F);


          // Sources first
                p_Elem := ActiveCircuit.sources.First;
                while p_Elem <> NIL do
                begin
                    if p_Elem.Enabled then
                        if CheckBusReference(p_Elem, BusReference, j) then
                        begin
                            WriteTerminalCurrents(F, p_Elem, FALSE);
                            Writeln(F);
                        end;
                    p_Elem := ActiveCircuit.sources.Next;
                end;


     // PDELEMENTS first
                p_Elem := ActiveCircuit.PDElements.First;

                while p_Elem <> NIL do
                begin
                    if p_Elem.Enabled then
                        if CheckBusReference(p_Elem, BusReference, j) then
                        begin
                            WriteTerminalCurrents(F, p_Elem, TRUE);
                            Writeln(F);
                        end;
                    p_Elem := ActiveCircuit.PDElements.Next;
                end;

                Writeln(F, '= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =');
                Writeln(F);
                Writeln(F, 'Power Conversion Elements');
                Writeln(F);
                Writeln(F, '  Bus         Phase    Magnitude, A     Angle      (Real)   +j  (Imag)');
                Writeln(F);

     // PCELEMENTS next
                p_Elem := ActiveCircuit.PCElements.First;

                while p_Elem <> NIL do
                begin
                    if p_Elem.Enabled then
                        if CheckBusReference(p_Elem, BusReference, j) then
                        begin
                            WriteTerminalCurrents(F, p_Elem, FALSE);
                            Writeln(F);
                        end;

                    p_Elem := ActiveCircuit.PCElements.Next;
                end;

      // FAULTs next
                p_Elem := ActiveCircuit.Faults.First;

                while p_Elem <> NIL do
                begin
                    if p_Elem.Enabled then
                        if CheckBusReference(p_Elem, BusReference, j) then
                        begin
                            WriteTerminalCurrents(F, p_Elem, FALSE);
                            Writeln(F);
                        end;

                    p_Elem := ActiveCircuit.Faults.Next;
                end;

     {Branch Powers}
                Writeln(F);
                Writeln(F, 'CIRCUIT ELEMENT POWER FLOW');
                Writeln(F);
                Writeln(F, '(Power Flow into element from indicated Bus)');
                Writeln(F);
                case Opt of
                    1:
                        Writeln(F, '  Bus       Phase     MW     +j   Mvar           MVA           PF');
                else
                    Writeln(F, '  Bus       Phase     kW     +j   kvar           kVA           PF');
                end;
                Writeln(F);

     // Sources first
                p_Elem := ActiveCircuit.sources.First;
                while p_Elem <> NIL do
                begin
                    if p_Elem.Enabled then
                        if CheckBusReference(p_Elem, BusReference, j) then
                        begin
                            WriteTerminalPower(F, p_Elem, j, opt);
                            Writeln(F);
                        end;
                    p_Elem := ActiveCircuit.sources.Next;
                end;


     // PDELEMENTS first
                p_Elem := ActiveCircuit.PDElements.First;

                while p_Elem <> NIL do
                begin
                    if p_Elem.Enabled then
                        if CheckBusReference(p_Elem, BusReference, jTerm) then
                        begin
                            WriteTerminalPower(F, p_Elem, jTerm, opt);

          {Get the other buses for the report}
                            for j := 1 to p_Elem.nterms do
                                if j <> jTerm then
                                begin
                                    Writeln(F, '------------');
                                    WriteTerminalPower(F, p_Elem, j, opt);
                                end;
                        end;
                    p_Elem := ActiveCircuit.PDElements.Next;
                end;

                Writeln(F, '= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =');
                Writeln(F);
                Writeln(F, 'Power Conversion Elements');
                Writeln(F);
                case Opt of
                    1:
                        Writeln(F, '  Bus         Phase     MW   +j  Mvar         MVA         PF');
                else
                    Writeln(F, '  Bus         Phase     kW   +j  kvar         kVA         PF');
                end;
                Writeln(F);

     // PCELEMENTS next
                p_Elem := ActiveCircuit.PCElements.First;

                while p_Elem <> NIL do
                begin
                    if p_Elem.Enabled then
                        if CheckBusReference(p_Elem, BusReference, jTerm) then
                        begin
                            WriteTerminalPower(F, p_Elem, jTerm, opt);
                            Writeln(F);
                        end;

                    p_Elem := ActiveCircuit.PCElements.Next;
                end;

            end; {ShowOptionCode=1}

        else

        end; {CASE}


    finally
        if Assigned(C_buffer) then
            Freemem(c_Buffer);
        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);

    end;
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowFaultStudy(FileNm: String);

var
    i, iBus, iphs: Integer;
    YFault, ZFault: Tcmatrix;
    Vfault: pComplexArray;  {Big temp array}
    F: Textfile;
    GFault, IFault: complex;
    Vphs: Double;
    CurrMag: Double;
    S: String;

begin

    SetMaxBusNameLength;

    try

        Assignfile(F, FileNm);
        ReWrite(F);

   { Set source voltage injection currents }
        with ActiveCircuit do
        begin
            with Solution do
            begin

     {All Phase Faults}
                Writeln(F, 'FAULT STUDY REPORT');
                Writeln(F);
                Writeln(F, 'ALL-Node Fault Currents');
                Writeln(F);
                Writeln(F, Pad('Bus', MaxBusNameLength), '       Node 1  X/R        Node 2  X/R        Node 3  X/R   ...  (Amps)');
                Writeln(F);
                for iBus := 1 to NumBuses do
           {Bus Norton Equivalent Current, Isc has been previously computed}
                    with Buses^[iBus] do
                    begin
                        Write(F, Pad(EncloseQuotes(UpperCase(BusList.Get(iBus))) + ',', MaxBusNameLength + 2));
                        for i := 1 to NumNodesThisBus do
                        begin
                            CurrMag := Cabs(BusCurrent^[i]);
                            if i > 1 then
                                Write(F, ', ');
                            Write(F, CurrMag: 10: 0);
                            if Currmag > 0.0 then
                                Write(F, ', ', GetXR(Cdiv(VBus^[i], BusCurrent^[i])): 5: 1)
                            else
                                Write(F, ',   N/A');
                        end;
                        Writeln(F);
                    end;

                Writeln(F);

           {One Phase Faults}
                Writeln(F);
                Writeln(F, 'ONE-Node to ground Faults');
                Writeln(F);
                Writeln(F, '                                      pu Node Voltages (L-N Volts if no base)');
                Writeln(F, Pad('Bus', MaxBusNameLength), '   Node  Amps         Node 1     Node 2     Node 3    ...');
                Writeln(F);

   { Solve for Fault Injection Currents}
                for iBus := 1 to NumBuses do
           {Bus Norton Equivalent Current, Isc has been previously computed}
                    with Buses^[iBus] do
                    begin
                        ZFault := TcMatrix.CreateMatrix(NumNodesThisBus);
                        ZFault.CopyFrom(Zsc);

                        for iphs := 1 to NumNodesThisBus do
                        begin
                            IFault := Cdiv(VBus[iphs], Zsc.GetElement(iphs, iphs));

                            S := Format('%s %4u %12.0f ', [Pad(EncloseQuotes(UpperCase(BusList.Get(iBus))), MaxBusNameLength + 2), GetNum(iphs), Cabs(Ifault)]);
                            Write(F, S, '   ');
                            for i := 1 to NumNodesThisBus do
                            begin
                                Vphs := Cabs(Csub(VBus[i], Cmul(Zsc.GetElement(i, iphs), IFault)));
                                if kVbase > 0.0 then
                                begin
                                    VPhs := 0.001 * Vphs / kVBase;
                                    Write(F, ' ', Vphs: 10: 3);
                                end
                                else
                                    Write(F, ' ', Vphs: 10: 1);
                            end;
                            Writeln(F);

                        end; {For iphase}
             {Now, Stuff it in the Css Array where it belongs}

                        ZFault.Free;
                    end;  {With bus}

           {Node-Node Faults}
                Writeln(F);
                Writeln(F, 'Adjacent Node-Node Faults');
                Writeln(F);
                Writeln(F, '                                        pu Node Voltages (L-N Volts if no base)');
                Writeln(F, 'Bus          Node-Node      Amps        Node 1     Node 2     Node 3    ...');
                Writeln(F);

   { Solve for Fault Injection Currents}
                for iBus := 1 to NumBuses do
           {Bus Norton Equivalent Current, Isc has been previously computed}
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
                            YFault.AddElemSym(iphs, iphs + 1, Cnegate(GFault));

                   { Solve for Injection Currents}
                            YFault.Invert;
                            YFault.MvMult(VFault, BusCurrent);  {Gets voltage appearing at fault}

                            Write(F, Pad(EncloseQuotes(UpperCase(BusList.Get(iBus))), MaxBusNameLength + 2), GetNum(Iphs): 4, GetNum(Iphs + 1): 4, Cabs(Cmul(Csub(VFault^[iphs], VFault^[iphs + 1]), GFault)): 12: 0, '   ');
                            for i := 1 to NumNodesThisBus do
                            begin
                                Vphs := Cabs(VFault^[i]);
                                if kvbase > 0.0 then
                                begin
                                    Vphs := 0.001 * Vphs / kVBase;
                                    Write(F, ' ', Vphs: 10: 3);
                                end
                                else
                                    Write(F, ' ', Vphs: 10: 1);
                            end;
                            Writeln(F);

                        end; {For iphase}
             {Now, Stuff it in the Css Array where it belongs}

                        Freemem(VFault);
                        YFault.Free;

                    end;  {With bus}

            end; {With Solution}
        end; {With ActiveCircuit}

    finally

        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);
    end;
end;

procedure WriteElementRecord(var F: TextFile; pElem: TDSSCktElement);
var
    Nterm, j: Integer;
    BusName: String;
begin
    Nterm := pElem.Nterms;
    BusName := Pad(StripExtension(pElem.FirstBus), MaxBusNameLength);
    Write(F, Pad(FullName(PElem), MaxDeviceNameLength + 2), ' ');
    for j := 1 to NTerm do
    begin
        Write(F, UpperCase(Busname), ' ');
        BusName := Pad(StripExtension(pElem.Nextbus), MaxBusNameLength);
    end;
    Writeln(F);
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowElements(FileNm: String; ClassName: String);

// Show Elements and bus connections

var
    F, FDisabled: TextFile;
    i: Integer;
    DisabledFileNm: String;
    pElem: TDSSCktElement;

begin
    SetMaxBusNameLength;
    SetMaxDeviceNameLength;

    try
        try
            Assignfile(F, FileNm);
            ReWrite(F);
        except
            On E: Exception do
                DoSimpleMsg('Error Trying to open element file "' + FileNm + '" file:' + E.message, 219000);
        end;

        try
            DisabledFileNm := StripExtension(FileNm) + '_Disabled.txt';
            AssignFile(FDisabled, DisabledFilenm);
            ReWrite(FDisabled);
        except
            On E: Exception do
                DoSimpleMsg('Error Trying to open disabled element file "' + DisabledFilenm + '" file:' + E.message, 219000);
        end;

        if Length(ClassName) > 0 then
        begin  // Just give a list of Active elements of a particular Class
            if SetObjectClass(ClassName) then
            begin
                Writeln(F, 'All Elements in Class "', ClassName, '"');
                Writeln(F);
                Writeln(Fdisabled, 'All DISABLED Elements in Class "', ClassName, '"');
                Writeln(Fdisabled);
                ActiveDSSClass := DSSClassList.Get(LastClassReferenced);
                for i := 1 to ActiveDSSClass.ElementCount do
                begin
                    ActiveDSSClass.Active := i;
                    if (ActiveDSSClass.DSSClassType and BASECLASSMASK) > 0 then
                    begin
                        if TDSSCktElement(ActiveDSSObject).Enabled then
                            Writeln(F, UpperCase(ActiveDssObject.Name))
                        else
                            Writeln(Fdisabled, UpperCase(ActiveDssObject.Name));
                    end
                    else
                        Writeln(F, UpperCase(ActiveDssObject.Name));   // non cktelements
                end;
            end;
        end
        else
        begin  // Default - Just do PD and PC Element in active circuit

            Writeln(F);
            Writeln(F, 'Elements in Active Circuit: ' + ActiveCircuit.Name);
            Writeln(F);
            Writeln(F, 'Power Delivery Elements');
            Writeln(F);
            Writeln(F, Pad('Element', MaxDeviceNameLength + 2), Pad(' Bus1', MaxBusNameLength), Pad(' Bus2', MaxBusNameLength), Pad(' Bus3', MaxBusNameLength), ' ...');
            Writeln(F);


            Writeln(Fdisabled);
            Writeln(Fdisabled, 'DISABLED Elements in Active Circuit: ' + ActiveCircuit.Name);
            Writeln(Fdisabled);
            Writeln(Fdisabled, 'DISABLED Power Delivery Elements');
            Writeln(Fdisabled);
            Writeln(Fdisabled, Pad('DISABLED Element', MaxDeviceNameLength + 2), Pad(' Bus1', MaxBusNameLength), Pad(' Bus2', MaxBusNameLength), Pad(' Bus3', MaxBusNameLength), ' ...');
            Writeln(Fdisabled);

     // PDELEMENTS first
            pElem := ActiveCircuit.PDElements.First;

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
                pElem := ActiveCircuit.PDElements.Next;
            end;

            Writeln(F);
            Writeln(F, 'Power Conversion Elements');
            Writeln(F);
            Writeln(F, Pad('Element', MaxDeviceNameLength + 2), Pad(' Bus1', MaxBusNameLength), Pad(' Bus2', MaxBusNameLength), Pad(' Bus3', MaxBusNameLength), ' ...');
            Writeln(F);

            Writeln(Fdisabled);
            Writeln(Fdisabled, 'DISABLED Power Conversion Elements');
            Writeln(Fdisabled);
            Writeln(Fdisabled, Pad('DISABLED Element', MaxDeviceNameLength + 2), Pad(' Bus1', MaxBusNameLength), Pad(' Bus2', MaxBusNameLength), Pad(' Bus3', MaxBusNameLength), ' ...');
            Writeln(Fdisabled);

     // PCELEMENTS next
            pElem := ActiveCircuit.PCElements.First;

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
                pElem := ActiveCircuit.PCElements.Next;
            end;


        end;


    finally

        CloseFile(FDisabled);
        FireOffEditor(DisabledFileNm);
        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);

    end;

end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowBuses(FileNm: String);

// Show bus names and nodes in uses

var
    F: TextFile;
    i, j: Integer;
    pBus: TDSSBus;

begin

    try
        SetMaxBusNameLength;
        Inc(MaxBusNameLength, 2);
        Assignfile(F, FileNm);
        ReWrite(F);

        Writeln(F);
        Writeln(F, 'BUSES AND NODES IN ACTIVE CIRCUIT: ' + ActiveCircuit.name);
        Writeln(F);
        Writeln(F, Pad('     ', MaxBusNameLength), '                         Coord                                 Number of     Nodes');
        Writeln(F, Pad('  Bus', MaxBusNameLength), '    Base kV             (x, y)                      Keep?       Nodes        connected ...');
        Writeln(F);
        with ActiveCircuit do
        begin
            for i := 1 to NumBuses do
            begin
                Write(F, Pad(EncloseQuotes(BusList.Get(i)), MaxBusNameLength), ' ');
                pBus := Buses^[i];
                if pBus.kVBase > 0.0 then
                    Write(F, (pBus.kVbase * SQRT3): 7: 3)
                else
                    Write(F, '   NA ');
                Write(F, '          (');
                if pBus.CoordDefined then
                    Write(F, Format(' %-13.11g, %-13.11g)', [pBus.x, pBus.y]))
                else
                    Write(F, '           NA,            NA )');
                if pBus.Keep then
                    Write(F, '     Yes  ')
                else
                    Write(F, '     No  ');
                Write(F, '     ');
                Write(F, pBus.NumNodesThisBus: 5);
                Write(F, '       ');
                for j := 1 to pBus.NumNodesThisBus do
                begin
                    Write(F, pBus.GetNum(j): 4, ' ');
                end;
                Writeln(F);
            end;
        end;

    finally

        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);

    end;

end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowMeters(FileNm: String);

// Show Values of  Meter Elements

var
    F: TextFile;
    i, j: Integer;
    pElem: TEnergyMeterObj;
    MeterClass: TEnergyMeter;

begin

    try
        Assignfile(F, FileNm);
        ReWrite(F);
        Writeln(F);
        Writeln(F, 'ENERGY METER VALUES');
        Writeln(F);
        Writeln(F, 'Registers:');
        MeterClass := TEnergyMeter(GetDSSClassPtr('Energymeter'));
        if MeterClass = NIL then
            Exit;  // oops somewhere!!
        if MeterClass.ElementCount = 0 then
        begin
            Writeln(F, 'No Energymeter Elements Defined.');
        end
        else
        begin
            ;

            pElem := ActiveCircuit.energyMeters.First;   // write registernames for first meter only
            for i := 1 to NumEMRegisters do
                Writeln(F, 'Reg ' + IntToStr(i) + ' = ', pElem.RegisterNames[i]);
            Writeln(F);

            pElem := ActiveCircuit.energyMeters.First;
            if pElem <> NIL then
            begin
                Write(F, 'Meter        ');
                for i := 1 to NumEMRegisters do
                    Write(F, Pad('   Reg ' + IntToStr(i), 11));
                Writeln(F);
                Writeln(F);
                while pElem <> NIL do
                begin
                    if pElem.Enabled then
                    begin
                        Write(F, Pad(pElem.Name, 12));
                        for j := 1 to NumEMRegisters do
                        begin
                            Write(F, PElem.Registers[j]: 10: 0, ' ');
                        end;
                    end;
                    pElem := ActiveCircuit.EnergyMeters.Next;
                    Writeln(F);
                end;
            end;

        end;

    finally

        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);

    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowGenMeters(FileNm: String);

// Show Values of Generator Meter Elements

var
    F: TextFile;
    i, j: Integer;
    pElem: TGeneratorObj;
    GeneratorClass: TGenerator;

begin

    try
        Assignfile(F, FileNm);
        ReWrite(F);
        Writeln(F);
        Writeln(F, 'GENERATOR ENERGY METER VALUES');
        Writeln(F);
        pElem := ActiveCircuit.Generators.First;
        if pElem <> NIL then
        begin
            GeneratorClass := TGenerator(pElem.ParentClass);
            Write(F, 'Generator          ');
            for i := 1 to NumGenRegisters do
                Write(F, Pad(GeneratorClass.RegisterNames[i], 11));
            Writeln(F);
            Writeln(F);
            while pElem <> NIL do
            begin
                if pElem.Enabled then
                begin
                    Write(F, Pad(pElem.Name, 12));
                    for j := 1 to NumGenRegisters do
                    begin
                        Write(F, PElem.Registers[j]: 10: 0, ' ');
                    end;
                end;
                pElem := ActiveCircuit.Generators.Next;
                Writeln(F);
            end;
        end;

    finally

        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);

    end;

end;

function TapPosition(const Transformer: TTransfObj; iWind: Integer): Integer;

{Assumes 0  is 1.0 per unit tap}

begin
    with Transformer do
        Result := Round((PresentTap[iWind] - (Maxtap[iWind] + Mintap[iWind]) / 2.0) / TapIncrement[iWind]);

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowRegulatorTaps(FileNm: String);

var
    F: TextFile;
    pReg: TRegControlObj;
    iWind: Integer;


begin

    try
        Assignfile(F, FileNm);
        ReWrite(F);
        Writeln(F);
        Writeln(F, 'CONTROLLED TRANSFORMER TAP SETTINGS');
        Writeln(F);
        Writeln(F, 'Name            Tap      Min       Max     Step  Position');
        Writeln(F);

        with ActiveCircuit do
        begin
            pReg := RegControls.First;
            while pReg <> NIL do
            begin
                with pReg.Transformer do
                begin
                    iWind := pReg.TrWinding;
                    Write(F, Pad(Name, 12), ' ');
                    Writeln(F, Format('%8.5f %8.5f %8.5f %8.5f     %d', [PresentTap[iWind], MinTap[iWind], MaxTap[iWind], TapIncrement[iWind], TapPosition(pREg.Transformer, iWind)]));
                end;
                pReg := RegControls.Next;
            end;
        end;
    finally
        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);
    end;

end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ShowMeterZone(FileNm: String);

var
    F: TextFile;
    i: Integer;
    pMtr: TEnergyMeterObj;
    pMtrClass: TEnergyMeter;
    PDelem: TPDelement;
    LoadElem: TLoadObj;
    ParamName: String;
    Param: String;

begin

    try
        FileNm := StripExtension(FileNm);
        ParamName := Parser.NextParam;
        Param := Parser.StrValue;

        FileNm := FileNm + '_' + Param + '.txt';

        Assignfile(F, FileNm);
        ReWrite(F);

        GlobalResult := FileNm;

        pMtrClass := DSSClassList.Get(ClassNames.Find('energymeter'));

        if Length(Param) > 0 then
        begin
            pMtr := pMtrClass.Find(Param);
            if pMtr = NIL then
                DoSimpleMsg('EnergyMeter "' + Param + '" not found.', 220)
            else
            if pMtr.BranchList <> NIL then
            begin
                Writeln(F, 'Branches and Load in Zone for EnergyMeter ', Param);
                Writeln(F);
                PDElem := pMtr.BranchList.First;
                while PDElem <> NIL do
                begin
                    for i := 1 to pMtr.Branchlist.Level do
                        Write(F, TABCHAR);
                 //Write(F, pMtr.BranchList.Level:0,' ');
                    Write(F, PDElem.ParentClass.Name, '.', PDelem.Name);
                    with pMtr.BranchList.PresentBranch do
                    begin
                        if IsParallel then
                            Write(F, '(PARALLEL:' + TDSSCktElement(LoopLineObj).Name + ')');
                        if IsLoopedHere then
                            Write(F, '(LOOP:' + TDSSCktElement(LoopLineObj).ParentClass.Name + '.' + TDSSCktElement(LoopLineObj).Name + ')');
                    end;
                    if Assigned(PDElem.SensorObj) then
                        Write(F, Format(' (Sensor: %s.%s) ', [PDElem.SensorObj.ParentClass.Name, PDElem.SensorObj.Name]))
                    else
                        Write(F, ' (Sensor: NIL)');
                    Writeln(F);
                    LoadElem := pMtr.Branchlist.FirstObject;
                    while LoadElem <> NIL do
                    begin
                        for i := 1 to pMtr.Branchlist.Level + 1 do
                            Write(F, TABCHAR);
                        Write(F, LoadElem.ParentClass.Name, '.', LoadElem.Name);
                        if Assigned(LoadElem.SensorObj) then
                            Write(F, Format(' (Sensor: %s.%s) ', [LoadElem.SensorObj.ParentClass.Name, LoadElem.SensorObj.Name]))
                        else
                            Write(F, ' (Sensor: NIL)');
                        Writeln(F);
                        LoadElem := pMtr.BranchList.NextObject
                    end;
                    PDElem := pMtr.BranchList.GoForward;
                end;
            end;
        end
        else
            DoSimpleMsg('Meter Name Not Specified.' + CRLF + parser.CmdString, 221);

    finally

        CloseFile(F);

        ParamName := Parser.NextParam;
        Param := Parser.strvalue;

        case length(Param) of
            0:
                FireOffEditor(FileNm);
        else
            ShowTreeView(FileNm);
        end;
        ParserVars.Add('@lastshowfile', FileNm);
     //
    end;

end;

procedure ShowOverloads(FileNm: String);

var
    F: TextFile;
    c_Buffer: pComplexArray;  // Allocate to max total conductors
    NCond, i, j, k: Integer;
    PDElem: TPDElement;
    Iph, I012: array[1..3] of Complex;
    I0, I1, I2,
    Cmag, Cmax: Double;

begin

    c_Buffer := NIL;
    SetMaxDeviceNameLength;

    try

        Assignfile(F, FileNm);
        ReWrite(F);

      {Allocate c_Buffer big enough for largest circuit element}
        Getmem(c_buffer, sizeof(c_Buffer^[1]) * GetMaxCktElementSize);

     {Sequence Currents}
        Writeln(F);
        Writeln(F, 'Power Delivery Element Overload Report');
        Writeln(F);
        Writeln(F, 'SYMMETRICAL COMPONENT CURRENTS BY CIRCUIT ELEMENT ');
        Writeln(F);
        Writeln(F, 'Element                             Term    I1    IOver %Normal  %Emerg     I2    %I2/I1    I0    %I0/I1');
        Writeln(F);


     // PDELEMENTS
        PDelem := ActiveCircuit.PDElements.First;
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
                                Write(F, Pad(FullName(PDelem), MaxDeviceNameLength + 2), j: 3);
                                Write(F, I1: 8: 1);
                                if PDElem.Normamps > 0.0 then
                                    Write(F, (Cmax - PDElem.Normamps): 8: 2)
                                else
                                    Write(F, '     0.0');
                                if PDElem.Normamps > 0.0 then
                                    Write(F, Cmax / PDElem.Normamps * 100.0: 8: 1)
                                else
                                    Write(F, '     0.0');
                                if PDElem.Emergamps > 0.0 then
                                    Write(F, Cmax / PDElem.Emergamps * 100.0: 8: 1)
                                else
                                    Write(F, '     0.0');
                                Write(F, I2: 8: 1);
                                if I1 > 0.0 then
                                    Write(F, 100.0 * I2 / I1: 8: 1)
                                else
                                    Write(F, '     0.0');
                                Write(F, I0: 8: 1);
                                if I1 > 0.0 then
                                    Write(F, 100.0 * I0 / I1: 8: 1)
                                else
                                    Write(F, '     0.0');
                                Writeln(F);
                            end;
                    end; {For}
                end;
            PDelem := ActiveCircuit.PDElements.Next;
        end;

    finally
        if Assigned(C_buffer) then
            Freemem(c_Buffer);
        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);

    end;

end;

{ - -- - - - - ------------------------------}

procedure ShowUnserved(FileNm: String; UE_Only: Boolean);

var
    F: TextFile;
    PLoad: TLoadObj;
    DoIt: Boolean;

begin

    try

        Assignfile(F, FileNm);
        ReWrite(F);

        Writeln(F);
        Writeln(F, 'UNSERVED  LOAD  REPORT');
        Writeln(F);
        Writeln(F, 'Load Element        Bus        Load kW  EEN Factor  UE Factor');
        Writeln(F);

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
                    Write(F, Pad(pLoad.Name, 20));
                    Write(F, Pad(pLoad.GetBus(1), 10));
                    Write(F, pLoad.kWBase: 8: 0);
                    Write(F, pLoad.EEN_Factor: 9: 3);
                    Write(F, pLoad.UE_Factor: 9: 3);
                    Writeln(F);
                end;

            end;
            pLoad := ActiveCircuit.Loads.Next;
        end;

    finally

        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);

    end;

end;


procedure ShowLosses(FileNm: String);


var
    F: TextFile;
    PDElem: TPDElement;
    PCElem: TPCElement;

    kLosses,
    TotalLosses,
    LineLosses,
    TransLosses,
    TermPower, LoadPower: Complex;

begin

    setMaxDeviceNameLength;

    try

        Assignfile(F, FileNm);
        ReWrite(F);

     {Sequence Currents}
        Writeln(F);
        Writeln(F, 'LOSSES REPORT');
        Writeln(F);
        Writeln(F, 'Power Delivery Element Loss Report');
        Writeln(F);
        Writeln(F, 'Element                  kW Losses    % of Power   kvar Losses');
        Writeln(F);


        TotalLosses := CZERO;
        LineLosses := CZERO;
        TransLosses := CZERO;

     // PDELEMENTS
        PDelem := ActiveCircuit.PDElements.First;
        while PDelem <> NIL do
        begin
            if (PDelem.Enabled)
       {THEN IF (CLASSMASK AND PDElem.DSSObjType) <>  CAP_ELEMENT }    // Ignore capacitors
            then
            begin
        //----PDelem.ActiveTerminalIdx := 1;  // activate 1st terminal for Power call
                kLosses := CmulReal(PDelem.Losses, 0.001);   // kW Losses in element
                Caccum(TotalLosses, kLosses);
                TermPower := CmulReal(PDelem.power[1], 0.001);     // Terminal 1 power

                if (CLASSMASK and PDElem.DSSObjType) = XFMR_ELEMENT then
                    Caccum(TransLosses, kLosses);
                if (CLASSMASK and PDElem.DSSObjType) = LINE_ELEMENT then
                    Caccum(LineLosses, kLosses);

                Write(F, Pad(FullName(PDelem), MaxDeviceNameLength + 2));
                Write(F, Format('%10.5f, ', [kLosses.re]));
                if (TermPower.re <> 0.0) and (kLosses.re > 0.0009) then
                    Write(F, (kLosses.re / Abs(TermPower.re) * 100.0): 8: 2)
                else
                    Write(F, CZERO.RE: 8: 1);
                Write(F, Format('     %.6g', [kLosses.im]));
                Writeln(F);
            end;
            PDelem := ActiveCircuit.PDElements.Next;
        end;      {While}

        Writeln(F);
        Writeln(F, Pad('LINE LOSSES=', 30), LineLosses.re: 10: 1, ' kW');
        Writeln(F, Pad('TRANSFORMER LOSSES=', 30), TransLosses.re: 10: 1, ' kW');
        Writeln(F);
        Writeln(F, Pad('TOTAL LOSSES=', 30), TotalLosses.re: 10: 1, ' kW');

        LoadPower := CZERO;
     // Sum the total load kW being served in the Ckt Model
        PCelem := ActiveCircuit.Loads.First;
        while Pcelem <> NIL do
        begin
            if PcElem.Enabled then
            begin
                Caccum(LoadPower, PCelem.Power[1]);
            end;
            PCelem := ActiveCircuit.Loads.Next;
        end;
        LoadPower := CmulReal(LoadPower, 0.001);

        Writeln(F);
        Writeln(F, Pad('TOTAL LOAD POWER = ', 30), Abs(LoadPower.re): 10: 1, ' kW');
        Write(F, Pad('Percent Losses for Circuit = ', 30));
        if LoadPower.re <> 0.0 then
            Writeln(F, Abs(TotalLosses.re / LoadPower.re) * 100.0: 8: 2, ' %');

    finally

        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);

    end;

end;

procedure ShowVariables(FileNm: String);

var
    F: Textfile;

    pcElem: TPCElement;
    i: Integer;

begin

    try

        Assignfile(F, FileNm);
        ReWrite(F);

     {Sequence Currents}
        Writeln(F);
        Writeln(F, 'VARIABLES REPORT');
        Writeln(F);
        Writeln(F, 'Present values of all variables in PC Elements in the circuit.');
        Writeln(F);

        pcElem := ActiveCircuit.PCElements.First;

        while pcElem <> NIL do
        begin
            if pcElem.Enabled and (pcElem.Numvariables > 0) then
            begin
                Writeln(F, 'ELEMENT: ', pcElem.ParentClass.Name, '.', pcElem.Name);
                Writeln(F, 'No. of variables: ', pcElem.Numvariables: 0);
                for  i := 1 to pcElem.Numvariables do
                begin
                    Writeln(F, '  ', pcElem.VariableName(i), ' = ', Format('%-.6g', [pcElem.Variable[i]]));
                end;
                Writeln(F);
            end;
            pCElem := ActiveCircuit.PCElements.Next;
        end;

    finally

        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);

    end;

end;

procedure ShowIsolated(FileNm: String);

{Show isolated buses/branches in present circuit}

var

    Branch_List,
    SubArea: TCktTree;      // Pointers to all circuit elements

    F: TextFile;
    TestElement, TestBranch, pElem: TDSSCktElement;

    i, j: Integer;

begin

     // Make sure bus list is built
    if ActiveCircuit.BusNameRedefined then
        ActiveCircuit.ReProcessBusDefs;

    with ActiveCircuit do
    begin

         {Initialize all Circuit Elements to not checked}
        TestElement := CktElements.First;
        while (TestElement <> NIL) do
        begin
            with TestElement do
            begin
                Checked := FALSE;
                for i := 1 to Nterms do
                    Terminals^[i].Checked := FALSE;
            end;
            TestElement := CktElements.Next;
        end;

         // initialize the Checked Flag for all Buses
        for j := 1 to NumBuses do
            Buses^[j].BusChecked := FALSE;

    end;

    // Get Started at main voltage source
    TestElement := ActiveCircuit.Sources.First;
    Branch_List := GetIsolatedSubArea(TestElement);

    {Show Report of Elements connected and not connected}
    try

        Assignfile(F, FileNm);
        ReWrite(F);

        Writeln(F);
        Writeln(F, 'ISOLATED CIRCUIT ELEMENT REPORT');
        Writeln(F);
        Writeln(F);
        Writeln(F, '***  THE FOLLOWING BUSES HAVE NO CONNECTION TO THE SOURCE ***');
        Writeln(F);

        with ActiveCircuit do
        begin
            for j := 1 to NumBuses do
                if not Buses^[j].BusChecked then
                    Writeln(F, EncloseQuotes(BusList.Get(j)));
        end;


        Writeln(F);
        Writeln(F, '***********  THE FOLLOWING SUB NETWORKS ARE ISOLATED ************');
        Writeln(F);

        with ActiveCircuit do
        begin
            TestElement := CktElements.First;

            while TestElement <> NIL do
            begin
                if TestElement.Enabled then
                    if not TestElement.Checked then
                        if (TestElement.DSSObjType and BASECLASSMASK) = PD_ELEMENT then
                        begin

                            SubArea := GetIsolatedSubArea(TestElement);
                            Writeln(F, '*** START SUBAREA ***');
                            TestBranch := SubArea.First;
                            while TestBranch <> NIL do
                            begin
                                Writeln(F, '(', SubArea.Level: 0, ') ', TestBranch.ParentClass.Name, '.', TestBranch.Name);
                                pElem := SubArea.FirstObject;
                                while pElem <> NIL do
                                begin
                                    Writeln(F, '[SHUNT], ', pElem.ParentClass.Name, '.', pElem.Name);
                                    pElem := Subarea.NextObject
                                end;
                                TestBranch := SubArea.GoForward;
                            end;
                            SubArea.Free;
                            Writeln(F);
                        end;
                TestElement := CktElements.Next;
            end;
        end;

        Writeln(F);
        Writeln(F, '***********  THE FOLLOWING ENABLED ELEMENTS ARE ISOLATED ************');
        Writeln(F);

        with ActiveCircuit do
        begin

       {Mark all controls, energy meters and monitors as checked so they don't show up}

            for i := 1 to DSSControls.ListSize do
                TDSSCktElement(DSSControls.Get(i)).Checked := TRUE;
            for i := 1 to MeterElements.ListSize do
                TDSSCktElement(MeterElements.Get(i)).Checked := TRUE;

            TestElement := CktElements.First;

            while TestElement <> NIL do
            begin
                if TestElement.Enabled then
                    if not TestElement.Checked then
                    begin
                        Write(F, '"', TestElement.ParentClass.Name, '.', TestElement.Name, '"');
                        Write(F, '  Buses:');
                        for j := 1 to TestElement.nterms do
                            Write(F, '  "', TestElement.GetBus(j), '"');
                        Writeln(F);
                    end;
                TestElement := CktElements.Next;
            end;
        end;

        Writeln(F);
        Writeln(F, '***  THE FOLLOWING BUSES ARE NOT CONNECTED TO ANY POWER DELIVERY ELEMENT ***');
        Writeln(F);

        with ActiveCircuit do
        begin
            for j := 1 to NumBuses do
                if not Buses^[j].BusChecked then
                    Writeln(F, EncloseQuotes(BusList.Get(j)));
        end;


        Writeln(F);
        Writeln(F, '***********  CONNECTED CIRCUIT ELEMENT TREE ************');
        Writeln(F);
        Writeln(F, '(Lexical Level) Element name');
        Writeln(F);

        TestBranch := Branch_List.First;
        while TestBranch <> NIL do
        begin
            Writeln(F, '(', Branch_List.Level: 0, ') ', TestBranch.ParentClass.Name, '.', TestBranch.Name);
            TestElement := Branch_List.FirstObject;
            while TestElement <> NIL do
            begin
                Writeln(F, '[SHUNT], ', TestElement.ParentClass.Name, '.', TestElement.Name);
                TestElement := Branch_List.NextObject
            end;
            TestBranch := Branch_List.GoForward;
        end;


    finally

        CloseFile(F);
        Branch_List.Free;
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);
    end;

end;

procedure ShowRatings(FileNm: String);

var
    F: TextFile;
    pdElem: TPDElement;

begin
    try

        Assignfile(F, FileNm);
        ReWrite(F);

        Writeln(F, 'Power Delivery Elements Normal and Emergency (max) Ratings');
        Writeln(F);

        pdElem := ActiveCircuit.PDElements.First;
        while pdElem <> NIL do
        begin

            Write(F, '"', pdElem.ParentClass.Name, '.', pdElem.Name, '", normamps=');
            Write(F, Format('%-.4g,  %-.4g  !Amps', [pdElem.Normamps, pdElem.EmergAmps]));
            Writeln(F);

            pdElem := ActiveCircuit.PDElements.Next;
        end;
    finally

        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);
    end;

end;

procedure ShowLoops(FileNm: String);
{Show loops and paralleled branches in Meter zones}

var
    F: TextFile;
    pdElem: TPDElement;
    hMeter: Integer;
    pMtr: TEnergyMeterObj;

begin
    try

        Assignfile(F, FileNm);
        ReWrite(F);

        Writeln(F, 'Loops and Paralleled Lines in all EnergyMeter Zones');
        Writeln(F);

        hMeter := EnergyMeterClass.First;

        while hMeter > 0 do
        begin

            pMtr := TEnergyMeterObj(ActiveDSSObject);

            if pMtr.BranchList <> NIL then
            begin

                PDElem := pMtr.BranchList.First;
                while PDElem <> NIL do
                begin

                    with pMtr.BranchList.PresentBranch do
                    begin
                        if IsParallel then
                            Writeln(F, '(', pMtr.Name, ') ', PDElem.ParentClass.Name, '.', UpperCase(PDelem.Name), ': PARALLEL WITH ', TDSSCktElement(LoopLineObj).Parentclass.Name, '.', TDSSCktElement(LoopLineObj).Name);
                        if IsLoopedHere then
                            Writeln(F, '(', pMtr.Name, ') ', PDElem.ParentClass.Name, '.', UpperCase(PDelem.Name), ': LOOPED TO     ', TDSSCktElement(LoopLineObj).parentclass.Name, '.', TDSSCktElement(LoopLineObj).Name);
                    end;
                    PDElem := pMtr.BranchList.GoForward;
                end;
            end;

            hMeter := EnergyMeterClass.Next
        end;

    finally

        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);
    end;
end;

procedure TopoLevelTabs(var F: TextFile; nLevel: Integer);
var
    nTabs, i: Integer;
begin
    nTabs := 30;
    if nLevel < nTabs then
        nTabs := nLevel;
    for i := 1 to nTabs do
        Write(F, TABCHAR);
    if nLevel > nTabs then
        Write(F, Format('(* %d *)', [nLevel]));
end;

procedure ShowTopology(FileRoot: String);
var
    F, Ftree: TextFile;
    FileNm, TreeNm: String;
    pdElem: TPDElement;
    pControlElem: TDSSCktElement;
    LoadElem: TLoadObj;
    topo: TCktTree;
    nLoops, nParallel, nLevels, nIsolated, nSwitches: Integer;
begin
    try
        FileNm := FileRoot + 'TopoSumm.Txt';
        TreeNm := FileRoot + 'TopoTree.Txt';

        Assignfile(F, FileNm);
        ReWrite(F);
        Writeln(F, 'Topology analysis for switch control algorithms');
        Writeln(F);

        Assignfile(Ftree, TreeNm);
        ReWrite(Ftree);
        Writeln(Ftree, 'Branches and Loads in Circuit ' + ActiveCircuit.Name);
        Writeln(Ftree);

        topo := ActiveCircuit.GetTopology;
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
                Write(Ftree, PDElem.ParentClass.Name, '.', PDElem.Name);
                with topo.PresentBranch do
                begin
                    if IsParallel then
                    begin
                        Inc(nParallel);
                        Write(Ftree, '(PARALLEL:' + TDSSCktElement(LoopLineObj).Name + ')');
                    end;
                    if IsLoopedHere then
                    begin
                        Inc(nLoops);
                        Write(Ftree, '(LOOP:' + TDSSCktElement(LoopLineObj).ParentClass.Name + '.' + TDSSCktElement(LoopLineObj).Name + ')');
                    end;
                    if PDElem.HasSensorObj then
                        Write(Ftree, Format(' (Sensor: %s.%s) ',
                            [PDElem.SensorObj.ParentClass.Name, PDElem.SensorObj.Name]));
                    if PDElem.HasControl then
                    begin
                        pControlElem := PDElem.ControlElementList.First;
                        while pControlElem <> NIL do
                        begin                                // accommodate multiple controls on same branch
                            Write(Ftree, Format(' (Control: %s.%s) ',
                                [pControlElem.ParentClass.Name, pControlElem.Name]));
                            if ((pControlElem.DSSObjType and CLASSMASK) = SWT_CONTROL) then
                                Inc(nSwitches);
                            pControlElem := PDElem.ControlElementList.Next;
                        end;
                    end;
                    if PDElem.HasEnergyMeter then
                        Write(Ftree, Format(' (Meter: %s) ', [PDElem.MeterObj.Name]));
                end;
                Writeln(Ftree);

                LoadElem := topo.FirstObject;
                while Assigned(LoadElem) do
                begin
                    TopoLevelTabs(Ftree, topo.Level + 1);
                    Write(Ftree, LoadElem.ParentClass.Name, '.', LoadElem.Name);
                    if LoadElem.HasSensorObj then
                        Write(Ftree, Format(' (Sensor: %s.%s) ',
                            [LoadElem.SensorObj.ParentClass.Name, LoadElem.SensorObj.Name]));
                    if LoadElem.HasControl then
                    begin

                        pControlElem := LoadElem.ControlElementList.First;
                        while pControlElem <> NIL do
                        begin                                // accommodate multiple controls on same branch
                            Write(Ftree, Format(' (Control: %s.%s) ',
                                [pControlElem.ParentClass.Name, pControlElem.Name]));
                            if ((pControlElem.DSSObjType and CLASSMASK) = SWT_CONTROL) then
                                Inc(nSwitches);
                            pControlElem := LoadElem.ControlElementList.Next;
                        end;
                    end;
                    if LoadElem.HasEnergyMeter then
                        Write(Ftree, Format(' (Meter: %s) ', [LoadElem.MeterObj.Name]));
                    Writeln(Ftree);
                    LoadElem := topo.NextObject
                end;

                PDElem := topo.GoForward;
            end;
        end;

        pdElem := ActiveCircuit.PDElements.First;
        while assigned(pdElem) do
        begin
            if pdElem.IsIsolated then
            begin
                Write(Ftree, Format('Isolated: %s.%s', [PDElem.ParentClass.Name, PDElem.Name]));
                if PDElem.HasSensorObj then
                    Write(Ftree, Format(' (Sensor: %s.%s) ',
                        [PDElem.SensorObj.ParentClass.Name, PDElem.SensorObj.Name]));
                if PDElem.HasControl then
                begin
                    pControlElem := PDElem.ControlElementList.First;
                    while pControlElem <> NIL do
                    begin                                // accommodate multiple controls on same branch
                        Write(Ftree, Format(' (Control: %s.%s) ',
                            [pControlElem.ParentClass.Name, pControlElem.Name]));
                        if ((pControlElem.DSSObjType and CLASSMASK) = SWT_CONTROL) then
                            Inc(nSwitches);
                        pControlElem := PDElem.ControlElementList.Next;
                    end;

                end;
                if PDElem.HasEnergyMeter then
                    Write(Ftree, Format(' (Meter: %s) ', [PDElem.MeterObj.Name]));
                Writeln(Ftree);
                Inc(nIsolated);
            end;
            pdElem := ActiveCircuit.PDElements.Next;
        end;

        nLoops := nLoops div 2;  // TODO, see if parallel lines also counted twice
        Writeln(F, Format('%d Levels Deep', [nLevels]));
        Writeln(F, Format('%d Loops', [nLoops]));
        Writeln(F, Format('%d Parallel PD elements', [nParallel]));
        Writeln(F, Format('%d Isolated PD components', [nIsolated]));
        Writeln(F, Format('%d Controlled Switches', [nSwitches]));

    finally
        CloseFile(F);
        CloseFile(Ftree);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);
        ShowTreeView(TreeNm);
    end;
end;

procedure ShowLineConstants(FileNm: String; Freq: Double; Units: Integer; Rho: Double);
var
    F, F2: TextFile;
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

        Assignfile(F, FileNm);
        ReWrite(F);

        Writeln(F, 'LINE CONSTANTS');
        Writeln(F, Format('Frequency = %.6g Hz, Earth resistivity = %.6g ohm-m', [Freq, Rho]));
        Writeln(F, 'Earth Model = ', GetEarthModel(DefaultEarthModel));
        Writeln(F);

        LineCodesFileNm := 'LineConstantsCode.DSS';
        Assignfile(F2, LineCodesFileNm);
        ReWrite(F2);

        Writeln(F2, '!--- OpenDSS Linecodes file generated from Show LINECONSTANTS command');
        Writeln(F2, Format('!--- Frequency = %.6g Hz, Earth resistivity = %.6g ohm-m', [Freq, Rho]));
        Writeln(F2, '!--- Earth Model = ', GetEarthModel(DefaultEarthModel));

        LineGeometryClass := DSSClassList.Get(ClassNames.Find('LineGeometry'));
        Z := NIL;
        YC := NIL;

        ActiveEarthModel := DefaultEarthModel;

        p := LineGeometryClass.first;
        while p > 0 do
        begin
            Pelem := LineGeometryClass.GetActiveObj;
            Z.Free;
            YC.Free;

            try
                // Get impedances per unit length
                pelem.RhoEarth := Rho;
                Z := pelem.Zmatrix[freq, 1.0, Units];
                YC := pelem.YCmatrix[freq, 1.0, Units];
            except
                on E: Exception do
                    DoSimpleMsg('Error computing line constants for LineGeometry.' + pelem.Name +
                        '; Error message: ' + E.Message, 9934);
            end;

            Writeln(F);
            Writeln(F, '--------------------------------------------------');
            Writeln(F, 'Geometry Code = ', Pelem.Name);
            Writeln(F);
            Writeln(F, 'R MATRIX, ohms per ', LineUnitsStr(Units));
            for i := 1 to Z.order do
            begin
                for j := 1 to i do
                begin
                    Write(F, Format('%.6g, ', [Z.GetElement(i, j).re]));
                end;
                Writeln(F);
            end;

            Writeln(F);
            Writeln(F, 'jX MATRIX, ohms per ', LineUnitsStr(Units));
            for i := 1 to Z.order do
            begin
                for j := 1 to i do
                begin
                    Write(F, Format('%.6g, ', [Z.GetElement(i, j).im]));
                end;
                Writeln(F);
            end;

            Writeln(F);
            Writeln(F, 'Susceptance (jB) MATRIX, S per ', LineUnitsStr(Units));
            for i := 1 to Yc.order do
            begin
                for j := 1 to i do
                begin
                    Write(F, Format('%.6g, ', [YC.GetElement(i, j).im]));
                end;
                Writeln(F);
            end;

            w := freq * twopi / 1.0E3;
            Writeln(F);
            Writeln(F, 'L MATRIX, mH per ', LineUnitsStr(Units));
            for i := 1 to Z.order do
            begin
                for j := 1 to i do
                begin
                    Write(F, Format('%.6g, ', [Z.GetElement(i, j).im / w]));
                end;
                Writeln(F);
            end;

            w := freq * twopi / 1.0E9;
            Writeln(F);
            Writeln(F, 'C MATRIX, nF per ', LineUnitsStr(Units));
            for i := 1 to Yc.order do
            begin
                for j := 1 to i do
                begin
                    Write(F, Format('%.6g, ', [YC.GetElement(i, j).im / w]));
                end;
                Writeln(F);
            end;

            {Write DSS LineCode record}
            //Writeln(F);
            //Writeln(F,'-------------------------------------------------------------------');
            //Writeln(F,'-------------------DSS Linecode Definition-------------------------');
            //Writeln(F,'-------------------------------------------------------------------');
            Writeln(F2);

            Writeln(F2, Format('New Linecode.%s nphases=%d  Units=%s', [pelem.Name, z.order, LineUnitsStr(Units)]));

            Write(F2, '~ Rmatrix=[');
            for i := 1 to Z.order do
            begin
                for j := 1 to i do
                    Write(F2, Format('%.6g  ', [Z.GetElement(i, j).re]));
                if i < Z.order then
                    Write(F2, '|');
            end;
            Writeln(F2, ']');

            Write(F2, '~ Xmatrix=[');
            for i := 1 to Z.order do
            begin
                for j := 1 to i do
                    Write(F2, Format('%.6g  ', [Z.GetElement(i, j).im]));
                if i < Z.order then
                    Write(F2, '|');
            end;
            Writeln(F2, ']');

            w := freq * twopi / 1.0E9;
            Write(F2, '~ Cmatrix=[');
            for i := 1 to Yc.order do
            begin
                for j := 1 to i do
                    Write(F2, Format('%.6g  ', [YC.GetElement(i, j).im / w]));
                if i < Yc.order then
                    Write(F2, '|');
            end;
            Writeln(F2, ']');

            {Add pos- and zero-sequence approximation here}
            {Kron reduce to 3 phases first}
            {Average diagonals and off-diagonals}

            Zs := CZERO;
            Zm := CZERO;
            CS := 0.0;
            CM := 0.0;

            if Z.order = 3 then
            begin
                Writeln(F);
                Writeln(F, '-------------------------------------------------------------------');
                Writeln(F, '-------------------Equiv Symmetrical Component --------------------');
                Writeln(F, '-------------------------------------------------------------------');
                Writeln(F);
                for i := 1 to 3 do
                    Caccum(Zs, Z.GetElement(i, i));
                for i := 1 to 3 do
                    for j := 1 to i - 1 do
                        Caccum(Zm, Z.GetElement(i, j));

                Z1 := CDivReal(Csub(Zs, Zm), 3.0);
                Z0 := CDivReal(Cadd(CMulReal(Zm, 2.0), Zs), 3.0);
                w := freq * twopi / 1000.0;
                Writeln(F);
                Writeln(F, 'Z1, ohms per ', LineUnitsStr(Units), Format(' = %.6g + j %.6g (L1 = %.6g mH) ', [Z1.re, Z1.im, Z1.im / w]));
                Writeln(F, 'Z0, ohms per ', LineUnitsStr(Units), Format(' = %.6g + j %.6g (L0 = %.6g mH) ', [Z0.re, Z0.im, Z0.im / w]));
                Writeln(F);

               {Compute Common Mode Series Impedance}
                Z.Invert;
                YCM := CZERO;
                for i := 1 to 3 do    // Add up all elements of Z inverse
                    for j := 1 to 3 do
                        Caccum(YCM, Z.GetElement(i, j));
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
                        Caccum(YCM, YC.GetElement(i, j));
                CCM := YCM.im / w;

                Writeln(F, 'C1, nF per ', LineUnitsStr(Units), Format(' = %.6g', [C1]));
                Writeln(F, 'C0, nF per ', LineUnitsStr(Units), Format(' = %.6g', [C0]));
                Writeln(F);

                w := freq * twopi;
                Writeln(F, 'Surge Impedance:');
                Writeln(F, Format('  Positive sequence = %.6g ohms', [Sqrt(Z1.im / w / (C1 * 1.0e-9))]));
                Writeln(F, Format('  Zero sequence     = %.6g ohms', [Sqrt(Z0.im / w / (C0 * 1.0e-9))]));
                Writeln(F, Format('  Common Mode       = %.6g ohms', [Sqrt(XCM / w / (CCM * 1.0e-9))]));
                Writeln(F);

                Writeln(F, 'Propagation Velocity (Percent of speed of light):');
                Writeln(F, Format('  Positive sequence = %.6g ', [1.0 / (Sqrt(Z1.im / w * (C1 * 1.0e-9))) / 299792458.0 / To_per_Meter(Units) * 100.0]));
                Writeln(F, Format('  Zero sequence     = %.6g ', [1.0 / (Sqrt(Z0.im / w * (C0 * 1.0e-9))) / 299792458.0 / To_per_Meter(Units) * 100.0]));
                Writeln(F);
            end;

            p := LineGeometryClass.Next;
        end;

    finally

        CloseFile(F);
        CloseFile(F2);
        FireOffEditor(FileNm);
        FireOffEditor(LineCodesFileNm);
        ParserVars.Add('@lastshowfile', FileNm);
    end;
end;

procedure ShowYPrim(Filenm: String);

var
    F: TextFile;
    cValues: pComplexArray;
    i, j: Integer;

begin

    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if ActiveCktElement <> NIL then
            begin
                try
                    Assignfile(F, FileNm);
                    ReWrite(F);

                    with ActiveCktElement do
                    begin

                        Writeln(F, 'Yprim of active circuit element: ', ParentClass.Name, '.', Name);
                        Writeln(F);

                        cValues := GetYprimValues(ALL_YPRIM);
                        if cValues <> NIL then
                        begin

                            Writeln(F);
                            Writeln(F, 'G matrix (conductance), S');
                            Writeln(F);

                            for i := 1 to Yorder do
                            begin
                                for j := 1 to i do
                                    Write(F, Format('%13.10g ', [cValues^[i + (j - 1) * Yorder].re]));
                                Writeln(F);
                            end;

                            Writeln(F);
                            Writeln(F, 'jB matrix (Susceptance), S');
                            Writeln(F);

                            for i := 1 to Yorder do
                            begin
                                for j := 1 to i do
                                    Write(F, Format('%13.10g ', [cValues^[i + (j - 1) * Yorder].im]));
                                Writeln(F);
                            end;
                        end
                        else
                            Writeln(F, 'Yprim matrix is Nil');

                    end;

                finally

                    CloseFile(F);
                    FireOffEditor(FileNm);
                    ParserVars.Add('@lastshowfile', FileNm);
                end;


            end;
        end;

end;

// shows how to retrieve the System Y in Triplet form
procedure ShowY(FileNm: String);

var
    F: TextFile;
    hY: NativeUInt;
    nNZ, nBus: Longword;
    i, row, col: Longword;
    re, im: Double;
    ColIdx, RowIdx: array of Longword;
    cVals: array of Complex;

begin

    if ActiveCircuit = NIL then
        Exit;
    hY := ActiveCircuit.Solution.hY;
    if hY <= 0 then
    begin
        DoSimpleMsg('Y Matrix not Built.', 222);
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

        Assignfile(F, FileNm);
        ReWrite(F);

        Writeln(F, 'System Y Matrix (Lower Triangle by Columns)');
        Writeln(F);
        Writeln(F, '  Row  Col               G               B');
        Writeln(F);

    // shows how to easily traverse the triplet format
        for i := 0 to nNZ - 1 do
        begin
            col := ColIdx[i] + 1;
            row := RowIdx[i] + 1;
            if row >= col then
            begin
                re := cVals[i].re;
                im := cVals[i].im;
                Writeln(F, Format('[%4d,%4d] = %13.10g + j%13.10g', [row, col, re, im]));
            end;
        end;

    finally
        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);
    end;

end;

procedure ShowNodeCurrentSum(FileNm: String);

type
    pNodeDoubleArray = ^NodeDoubleArray;
    NodeDoubleArray = array[0..100] of Double;

var
    F: Textfile;
    i, j: Integer;
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
        Assignfile(F, FileNm);
        ReWrite(F);

        with ActiveCircuit, ActiveCircuit.solution do
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
                        for i := 1 to Yorder do
                        begin
                            Ctemp := Iterminal^[i];
                            nRef := NodeRef^[i];
                            Caccum(Currents^[nRef], Ctemp);  // Noderef=0 is OK
                            if Cabs(Ctemp) > MaxNodeCurrent^[nRef] then
                                MaxNodeCurrent^[nRef] := Cabs(Ctemp);
                        end;
                    end;
                pCktElement := CktElements.Next;
            end;

        // Now write report

            SetMaxBusNameLength;
            MaxBusNameLength := MaxBusNameLength + 2;
            Writeln(F);
            Writeln(F, 'Node Current Mismatch Report');
            Writeln(F);
            Writeln(F);
            Writeln(F, pad('Bus,', MaxBusNameLength), ' Node, "Current Sum (A)", "%error", "Max Current (A)"');

          // Ground Bus
            nref := 0;
            dTemp := Cabs(Currents^[nref]);
            if (MaxNodeCurrent^[nRef] = 0.0) or (MaxNodeCurrent^[nRef] = dTemp) then
                pctError := Format('%10.1f', [0.0])
            else
                pctError := Format('%10.6f', [dTemp / MaxNodeCurrent^[nRef] * 100.0]);
            BName := Pad('"System Ground"', MaxBusNameLength);
            Writeln(F, Format('%s, %2d, %10.5f,       %s, %10.5f', [Bname, nref, dTemp, pctError, MaxNodeCurrent^[nRef]]));


            for i := 1 to ActiveCircuit.NumBuses do
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
                        Bname := Paddots(EncloseQuotes(BusList.Get(i)), MaxBusNameLength)
                    else
                        BName := Pad('"   -"', MaxBusNameLength);
                    Writeln(F, Format('%s, %2d, %10.5f,       %s, %10.5f', [Bname, Buses^[i].GetNum(j), dTemp, pctError, MaxNodeCurrent^[nRef]]));
                end;
            end;
        end;

    finally
        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);
        ReallocMem(MaxNodeCurrent, 0); // Dispose of temp memory
    end;
end;


procedure ShowkVBaseMismatch(FileNm: String);

var
    F: TextFile;

    pLoad: TLoadObj;
    pGen: TGeneratorObj;
    pBus: TDSSBus;
    BuskV: Double;
    BusName: String;

begin

    try
        Assignfile(F, FileNm);
        ReWrite(F);

        {Check Loads}
        if ActiveCircuit.Loads.ListSize > 0 then
        begin
            Writeln(F);
            Writeln(F, '!!!  LOAD VOLTAGE BASE MISMATCHES');
            Writeln(F);
        end;


        pLoad := ActiveCircuit.Loads.First;
        while pLoad <> NIL do
        begin
           {Find Bus To Which Load Connected}
            pBus := ActiveCircuit.Buses^[pLoad.Terminals^[1].BusRef];
            BusName := ActiveCircuit.BusList.Get(pLoad.Terminals^[1].BusRef);
            if pBus.kVBase <> 0.0 then
            begin
                if (pLoad.Nphases = 1) and (pLoad.Connection = 0) then
                begin
                    if abs(pLoad.kVLoadBase - pBus.kVBase) > 0.10 * pBus.kVBase then
                    begin
                        Writeln(F, Format('!!!!! Voltage Base Mismatch, Load.%s.kV=%.6g, Bus %s LN kvBase = %.6g', [pLoad.Name, pLoad.kVLoadBase, pLoad.GetBus(1), pBus.kVBase]));
                        Writeln(F, Format('!setkvbase %s kVLN=%.6g', [Busname, pLoad.kVLoadBase]));
                        Writeln(F, Format('!Load.%s.kV=%.6g', [pLoad.Name, pBus.kVBase]));
                    end;
                end
                else
                begin
                    BuskV := pBus.kVBase * SQRT3;
                    if abs(pLoad.kVLoadBase - BuskV) > 0.10 * BuskV then
                    begin
                        Writeln(F, Format('!!!!! Voltage Base Mismatch, Load.%s.kV=%.6g, Bus %s kvBase = %.6g', [pLoad.Name, pLoad.kVLoadBase, pLoad.GetBus(1), BuskV]));
                        Writeln(F, Format('!setkvbase %s kVLL=%.6g', [Busname, pLoad.kVLoadBase]));
                        Writeln(F, Format('!Load.%s.kV=%.6g', [pLoad.Name, BuskV]));
                    end;
                end;
            end;
            pLoad := ActiveCircuit.Loads.Next;
        end;


        {Check Generators}

        if ActiveCircuit.Generators.ListSize > 0 then
        begin
            Writeln(F);
            Writeln(F, '!!!  GENERATOR VOLTAGE BASE MISMATCHES');
            Writeln(F);
        end;


        pGen := ActiveCircuit.Generators.First;
        while pGen <> NIL do
        begin
           {Find Bus To Which Generator Connected}
            pBus := ActiveCircuit.Buses^[pGen.Terminals^[1].BusRef];
            BusName := ActiveCircuit.BusList.Get(pGen.Terminals^[1].BusRef);
            if pBus.kVBase <> 0.0 then
            begin
                if (pGen.Nphases = 1) and (pGen.Connection = 0) then
                begin
                    if abs(pGen.Genvars.kVGeneratorBase - pBus.kVBase) > 0.10 * pBus.kVBase then
                    begin
                        Writeln(F, Format('!!! Voltage Base Mismatch, Generator.%s.kV=%.6g, Bus %s LN kvBase = %.6g', [pGen.Name, pGen.Genvars.kVGeneratorBase, pGen.GetBus(1), pBus.kVBase]));
                        Writeln(F, Format('!setkvbase %s kVLN=%.6g', [Busname, pGen.Genvars.kVGeneratorBase]));
                        Writeln(F, Format('!Generator.%s.kV=%.6g', [pGen.Name, pBus.kVBase]));
                    end;
                end
                else
                begin
                    BuskV := pBus.kVBase * SQRT3;
                    if abs(pGen.Genvars.kVGeneratorBase - BuskV) > 0.10 * BuskV then
                    begin
                        Writeln(F, Format('!!! Voltage Base Mismatch, Generator.%s.kV=%.6g, Bus %s kvBase = %.6g', [pGen.Name, pGen.Genvars.kVGeneratorBase, pGen.GetBus(1), BuskV]));
                        Writeln(F, Format('!setkvbase %s kVLL=%.6g', [Busname, pGen.Genvars.kVGeneratorBase]));
                        Writeln(F, Format('!Generator.%s.kV=%.6g', [pGen.Name, BuskV]));
                    end;
                end;
            end;

            pGen := ActiveCircuit.Generators.Next;
        end;

    finally
        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);
    end;
end;

procedure ShowDeltaV(FileNm: String);

var
    F: TextFile;
    pElem: TDSSCktElement;


begin

    try
        Assignfile(F, FileNm);
        ReWrite(F);

        SetMaxDeviceNameLength;

        Writeln(F);
        Writeln(F, 'VOLTAGES ACROSS CIRCUIT ELEMENTS WITH 2 TERMINALS');
        Writeln(F);
        Writeln(F, 'Source Elements');
        Writeln(F);
        Writeln(F, pad('Element,', MaxDeviceNameLength), ' Conductor,     Volts,   Percent,           kVBase,  Angle');
        Writeln(F);


         // SOURCES first
        pElem := ActiveCircuit.sources.First;

        while pElem <> NIL do
        begin
            if pElem.Enabled and (pElem.NTerms = 2) then
            begin
                WriteElementDeltaVoltages(F, pElem);
                Writeln(F);
            end;
            pElem := ActiveCircuit.sources.Next;
        end;

        Writeln(F);
        Writeln(F, 'Power Delivery Elements');
        Writeln(F);
        Writeln(F, pad('Element,', MaxDeviceNameLength), ' Conductor,     Volts,   Percent,           kVBase,  Angle');
        Writeln(F);


         // PDELEMENTS next
        pElem := ActiveCircuit.PDElements.First;

        while pElem <> NIL do
        begin
            if pElem.Enabled and (pElem.NTerms = 2) then
            begin
                WriteElementDeltaVoltages(F, pElem);
                Writeln(F);
            end;
            pElem := ActiveCircuit.PDElements.Next;
        end;

        Writeln(F, '= = = = = = = = = = = = = = = = = = =  = = = = = = = = = = =  = =');
        Writeln(F);
        Writeln(F, 'Power Conversion Elements');
        Writeln(F);
        Writeln(F, pad('Element,', MaxDeviceNameLength), ' Conductor,     Volts,   Percent,           kVBase,  Angle');
        Writeln(F);

         // PCELEMENTS next
        pElem := ActiveCircuit.PCElements.First;

        while pElem <> NIL do
        begin
            if pElem.Enabled and (pElem.NTerms = 2) then
            begin
                WriteElementDeltaVoltages(F, pElem);
                Writeln(F);
            end;
            pElem := ActiveCircuit.PCElements.Next;
        end;


    finally
        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);
    end;

end;

procedure ShowControlledElements(FileNm: String);

var
    F: Textfile;
    pdelem: TPDElement;
    pctrlelem: TDSSCktElement;
    i: Integer;

begin
    try

        Assignfile(F, FileNm);
        ReWrite(F);

        pdelem := ActiveCircuit.PDElements.First;
        while pdelem <> NIL do
        begin
            if pdelem.HasControl then
            begin
                with pdelem do
                    Write(F, Format('%s.%s', [ParentClass.Name, Name]));
                for i := 1 to pdelem.ControlElementList.ListSize do
                begin
                    pctrlelem := pdelem.ControlElementList.Get(i);
                    with  pctrlelem do
                        Write(F, Format(', %s.%s ', [ParentClass.Name, Name]));
                end;
                Writeln(F);
            end;
            pdelem := ActiveCircuit.PDElements.Next;
        end;

    finally

        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);

    end;

end;

procedure ShowResult(FileNm: String);

var
    F: TextFile;

begin

    try
        Assignfile(F, FileNm);
        ReWrite(F);
        Parservars.Lookup('@result');
        Writeln(F, Parservars.Value);

        GlobalResult := FileNm;

    finally

        CloseFile(F);
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);
    end;


end;

procedure ShowEventLog(FileNm: String);


begin
    try

        EventStrings.SaveToFile(FileNm);
        GlobalResult := FileNm;

    finally
        FireOffEditor(FileNm);
        ParserVars.Add('@lastshowfile', FileNm);
    end;

end;


initialization

    MaxDeviceNameLength := 30;
    MaxBusNameLength := 12;

end.
