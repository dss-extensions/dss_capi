unit AutoAdd;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

//  Unit for processing the AutoAdd Solution FUNCTIONs
//
//  Note: Make sure this class in instantiated after energymeter class
//
//  There is one of these per circuit

interface

uses
    UComplex, DSSUcomplex,
    EnergyMeter,
    HashList,
    Arraydef,
    Generator,
    Capacitor,
    Classes,
    DSSClass;

type
    TAutoAdd = record
    PRIVATE
        BusIdxList: ArrayOfInteger;
        LastAddedGenerator,
        LastAddedCapacitor: Integer;

        BusIndex,
        Phases: Integer;

        Ycap: Double;
        GenVA: Complex;

        kWLosses, BaseLosses, puLossImprovement: Double;
        kWEEN, BaseEEN, puEENImprovement: Double;

        function Get_WeightedLosses: Double;

        procedure ComputekWLosses_EEN;
        procedure SetBaseLosses;

        function GetUniqueGenName: String;
        function GetUniqueCapName: String;
    PUBLIC
        // Autoadd mode Variables
        GenkW,
        GenPF,
        Genkvar,
        Capkvar: Double;
        AddType: Integer;

        ModeChanged: Boolean;
        
        DSS: TDSSContext;

        procedure Init(dssContext: TDSSContext);

        procedure MakeBusList;
        procedure AppendToFile(const WhichFile, S: String);
        procedure AddCurrents(SolveType: Integer);

        function Solve: Integer; // Automatically add caps or generators

        property WeightedLosses: Double READ Get_WeightedLosses;
    end;

implementation

uses
    BufStream,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    PDElement,
    Utilities,
    SysUtils,
    Executive,
    // ProgressForm,
    // Forms,
    Solution,
    DSSHelper;

function SumSelectedRegisters(Mtr: TEnergyMeterObj; Regs: Array of Integer): Double;
var
    reg: Integer;
begin
    Result := 0.0;
    for reg in Regs do
    begin
        Result += Mtr.Registers[reg] * Mtr.TotalsMask[reg];
    end;
end;


procedure TAutoAdd.Init(dssContext: TDSSContext);
begin
    DSS := dssContext;

    // AutoAdd defaults
    GenkW := 1000.0;
    GenPF := 1.0;
    Capkvar := 600.0;
    AddType := GENADD;
    LastAddedGenerator := 0;
    LastAddedCapacitor := 0;

    ModeChanged := TRUE;
end;

procedure TAutoAdd.MakeBusList;
// Make a list of unique busnames
// IF AutoAddBusList in ActiveCircuit is not nil, use this list.
// ELSE, Use the element lists in Energy Meters
// IF no Energy Meters, use all the buses in the active circuit
var
    pMeter: TEnergyMeterObj;
    retval: Integer;
    Bname: String;
    i: Integer;
    PDElem: TPDElement;
    FBusList: TBusHashListType = NIL;
begin
    SetLength(BusIdxList, 0);

    // Autoaddbuslist exists in Active Circuit, use it  (see set Autobuslist=)
    if DSS.ActiveCircuit.AutoAddBusList.Count > 0 then
        FBusList := DSS.ActiveCircuit.AutoAddBusList
    else

    if DSS.ActiveCircuit.EnergyMeters.Count = 0 then
    begin
        // No energymeters in circuit
        // Include all buses in the circuit
        SetLength(BusIdxList, DSS.ActiveCircuit.BusList.Count);

        for i := 0 to High(BusIdxList) do
        begin
            BusIdxList[i] := i;
        end;
        Exit;
    end
    else
    begin
        // Construct Bus List from Energy Meters Zone Lists
        // Include only buses in EnergyMeter lists
        // Consider all meters
        FBusList := TBusHashListType.Create(DSS.ActiveCircuit.NumBuses);
        for pMeter in DSS.ActiveCircuit.EnergyMeters do
        begin
            if pMeter.BranchList = NIL then
                continue;

            PDElem := pMeter.BranchList.First();
            while PDElem <> NIL do
            begin // add only unique busnames
                for i := 1 to PDElem.Nterms do
                begin
                    Bname := StripExtension(PDElem.GetBus(i));
                    retval := FBusList.Find(Bname);
                    if retval = 0 then
                    begin
                        FBusList.Add(BName);    // return value is index of bus
                    end;
                end;
                PDElem := pMeter.BranchList.GoForward();
            end;
        end;
    end;

    // Make busIdxList from FBusList
    SetLength(BusIdxList, FBusList.Count);
    for i := 1 to Length(BusIdxList) do
    begin
        BusIdxList[i - 1] := DSS.ActiveCircuit.BusList.Find(FBusList.NameOfIndex(i));
    end;

    FreeAndNil(FBusList);
end;


function TAutoAdd.Get_WeightedLosses: Double;
// Returns losses in metered part of circuit +
// weighted EEN values
//
// If no meters, returns just total losses in circuit
//
// Base everything on gen kW
begin
    ComputekWLosses_EEN;
    puLossImprovement := (BaseLosses - kWLosses) / GenkW;

    if DSS.ActiveCircuit.EnergyMeters.Count = 0 then
    begin
        // No energymeters in circuit
        // Just go by total system losses
        puEENImprovement := 0.0;
        Result := puLossImprovement;
        Exit;
    end;
    
    puEENImprovement := (BaseEEN - kWEEN) / GenkW;
    Result := DSS.ActiveCircuit.LossWeight * puLossImprovement + DSS.ActiveCircuit.UEWeight * puEENImprovement;
end;

procedure TAutoAdd.AppendToFile(const WhichFile, S: String);
var
    F: TStream;
    Fname: String;
begin
    F := nil;
    try
        FName := DSS.OutputDirectory + DSS.CircuitName_ + 'AutoAdded' + WhichFile + '.txt';
        if FileExists(FName) then
        begin
            F := DSS.GetOutputStreamEx(Fname, fmOpenReadWrite);
            F.Seek(0, soEnd);
        end
        else
            F := DSS.GetOutputStreamEx(Fname, fmCreate);

        FSWriteLn(F, S);
    except
        On E: EXCEPTion do
            DoSimpleMsg(DSS, 'Error trying to append to "%s": %s', [Fname, E.Message], 438);
    end;
    if F <> nil then
        F.Free();
end;

function TAutoAdd.GetUniqueGenName: String;
var
    TrialName: String;
    Done: Boolean;
begin
    repeat
        Done := TRUE;
        Inc(LastAddedGenerator);
        TrialName := 'Gadd' + IntToStr(LastAddedGenerator);
        if DSS.GeneratorClass.Find(TrialName) <> NIL then
            Done := FALSE;
    until Done;
    Result := TrialName;
end;

function TAutoAdd.GetUniqueCapName: String;
var
    TrialName: String;
    Done: Boolean;
begin
    repeat
        Done := TRUE;
        Inc(LastAddedCapacitor);
        TrialName := 'Cadd' + IntToStr(LastAddedCapacitor);
        if DSS.CapacitorClass.Find(TrialName) <> NIL then
            Done := FALSE;
    until Done;

    Result := TrialName;
end;


function TAutoAdd.Solve: Integer; // Automatically add caps or generators
// Automatically add a specified size of generator or capacitor at the location
// that results in the lowest losses in either metered part of circuit or
// total circuit, if no meters.
// 
// If metered, EEN is also added in WITH a selected weighting factor (see
// set ueweight= ... command).
// 
// Thus, this algorithm placed generators and capacitors to minimize losses and
// potential unserved energy.
var
    LossImproveFactor,
    MaxLossImproveFactor: Double;
    MinLossBus,
    MinBusPhases: Integer;
    Testbus: String;

    i: Integer;

    CommandString: String;

    kVrat, TestGenkW,
    TestCapkvar: Double;
    ProgressMax: Integer;

    ckt: TDSSCircuit;
    solution: TSolutionObj;
    FLog: TStream;  // Log File
begin
    FLog := nil;
//  Algorithm:
//     1) makes a list of buses to check, either
//        a. Previously defined list
//        b. Meter zone lists
//        c. All buses, if neither of the above
//     2) Inject a current corresponding to the generator
//     3) Check test criteria
//     4) Save result
//     5) Add generator/capacitor to circuit
//
    Result := 0;
    ckt := DSS.ActiveCircuit;
    solution := ckt.Solution;
    try
        if (solution.LoadModel = ADMITTANCE) then
        begin
            solution.LoadModel := POWERFLOW;
            solution.SystemYChanged := TRUE;  // Force rebuild of System Y without Loads
        end;

        // Do a preliminary snapshot solution to Force definition of meter zones
        // And set bus lists
        DSS.EnergyMeterClass.ResetAll();
        if solution.SystemYChanged or ckt.BusNameRedefined then
        begin
            solution.SolveSnap();
            ModeChanged := TRUE;
        end;

        DSS.EnergyMeterClass.SampleAll();

        //  Check to see if bus base voltages have been defined 
        if ckt.Buses[ckt.NumBuses].kVBase = 0.0 then
            solution.SetVoltageBases();

        if ModeChanged then
        begin
            MakeBusList();  // Make list of buses to check
            ModeChanged := FALSE;  // Keep same BusIdxList if no changes
        end;

        solution.IntervalHrs := 1.0;

        // Start up Log File

        FLog := DSS.GetOutputStreamEx(DSS.OutputDirectory + DSS.CircuitName_ + 'AutoAddLog.csv', fmCreate);
        FSWriteLn(FLog, '"Bus", "Base kV", "kW Losses", "% Improvement", "kW UE", "% Improvement", "Weighted Total", "Iterations"');

        // for this solution mode, only the peak load condition is taken into account
        // load is adjusted for growth by year.
        solution.SetGeneratorDispRef();

        // Turn regulators and caps off while we are searching
        solution.ControlMode := CONTROLSOFF;

        SetBaseLosses();  // Establish base values

        case AddType of
            GENADD:
            begin
                if ckt.PositiveSequence then
                    TestGenkW := GenkW / 3.0
                else
                    TestGenkW := GenkW;

                if GenPF <> 0.0 then
                begin
                    Genkvar := TestGenkW * sqrt(1.0 / sqr(GenPF) - 1.0);
                    if GenPF < 0.0 then
                        Genkvar := -Genkvar;
                end
                else
                begin   // Someone goofed and specified 0.0 PF
                    GenPF := 1.0;
                    Genkvar := 0.0;
                end;

                MinLossBus := 0;   // null string
                MaxLossImproveFactor := -1.0e50;  // Some very large neg number
                MinBusPhases := 3;

                // Progress meter
                DSS.ProgressCaption('AutoAdding Generators');
                ProgressMax := Length(BusIdxList);
                solution.ProgressCount := 0;

                DSS.ProgressFormCaption(Format('Testing %d buses. Please Wait... ', [Length(BusIdxList)]));
                DSS.ShowPctProgress(0);

                for i := 1 to Length(BusIdxList) do
                begin
                    Inc(solution.ProgressCount);

                    BusIndex := BusIdxList[i - 1];

                    if BusIndex > 0 then
                    begin
                        TestBus := ckt.BusList.NameOfIndex(BusIndex);
                            // ProgressFormCaption( 'Testing bus ' + TestBus);
                        if ((solution.ProgressCount mod 20) = 0) or (i = Length(BusIdxList)) then
                        begin
                            DSS.ProgressFormCaption(Format('Testing bus %d/%d. ', [i, Length(BusIdxList)]));
                            DSS.ShowPctProgress(Round((100 * solution.ProgressCount) / ProgressMax));
                        end;

                        DSS.EnergyMeterClass.ResetAll();

                        // Get the Number of Phases at this bus and the Node Ref and add into the Aux Current Array

                        // Assume either a 3-phase or 1-phase generator
                        if ckt.Buses[BusIndex].NumNodesThisBus < 3 then
                            Phases := 1
                        else
                            Phases := 3;

                        GenVA := Cmplx(1000.0 * TestGenkW / Phases, 1000.0 * Genkvar / Phases);

                        //  - -- - - - - - - Solution - - - - - - - - - - - - - - -
                        ckt.Issolved := FALSE;

                        solution.UseAuxCurrents := TRUE;   // Calls InjCurrents on callback
                        solution.SolveSnap();

                        if ckt.IsSolved then
                        begin
                            // Only do this if solution converged ELSE something might break
                            // in meter sampling

                            DSS.EnergyMeterClass.SampleAll();

                            LossImproveFactor := WeightedLosses;

                            FSWrite(Flog, Format('"%s", %-g', [TestBus, ckt.Buses[BusIndex].kVBase * SQRT3]));
                            FSWrite(Flog, Format(', %-g, %-g', [kWLosses, puLossImprovement * 100.0]));
                            FSWrite(Flog, Format(', %-g, %-g', [kWEEN, puEENImprovement * 100.0]));
                            FSWriteln(Flog, Format(', %-g, %d', [LossImproveFactor, solution.Iteration]));

                            if LossImproveFactor > MaxLossImproveFactor then
                            begin
                                MaxLossImproveFactor := LossImproveFactor;
                                MinLossBus := BusIndex;
                                MinBusPhases := Phases;
                            end;
                        end;
                    end;
                    if DSS.SolutionAbort then
                        Break;
                end;

                // Put Control mode back to default before inserting Generator for real
                solution.ControlMode := CTRLSTATIC;
                solution.UseAuxCurrents := FALSE;

                if MinLossBus > 0 then
                begin
                    if MinBusPhases >= 3 then
                        kVrat := ckt.Buses[MinLossBus].kVBase * SQRT3
                    else
                        kVrat := ckt.Buses[MinLossBus].kVBase;

                    CommandString := 'New generator.' + GetUniqueGenName() +
                        ', bus1="' + ckt.BusList.NameOfIndex(MinLossBus) +
                        '", phases=' + IntToStr(MinBusPhases) +
                        ', kV=' + Format('%-g', [kVrat]) +
                        ', kW=' + Format('%-g', [TestGenkW]) +
                        ', ' + Format('%5.2f', [GenPF]) +
                        Format('! Factor =  %-g (%-.3g, %-.3g)', [MaxLossImproveFactor, ckt.LossWeight, ckt.UEWeight]);

                    DSS.DSSExecutive.ParseCommand(CommandString);    // Defines Generator

                    // AppEnd this command to '...AutoAddedGenerators.txt'
                    AppendToFile('Generators', CommandString);

                    solution.SolveSnap();  // Force rebuilding of lists
                end;
                // Return location of added generator so that it can
                // be picked up through the result string of the COM interface
                DSS.GlobalResult := ckt.BusList.NameOfIndex(MinLossBus) +Format(', %-g', [MaxLossImproveFactor]);

                DSS.ProgressHide();

                // note that the command that added the generator can be
                // picked up from the Command property of the COM interface.
            end;


            CAPADD:
            begin
                MinLossBus := 0;   // null string
                MaxLossImproveFactor := -1.0e50;  // Some very large number
                MinBusPhases := 3;

                if DSS.ActiveCircuit.PositiveSequence then
                    TestCapkvar := Capkvar / 3.0
                else
                    TestCapkvar := Capkvar;

                    // Progress meter
                DSS.ProgressCaption('AutoAdding Capacitors');
                ProgressMax := Length(BusIdxList);
                solution.ProgressCount := 0;

                for i := 1 to Length(BusIdxList) do
                begin
                    Inc(solution.ProgressCount);
                    // Make sure testbus is actually in the circuit
                    BusIndex := BusIdxList[i - 1];
                    if BusIndex > 0 then
                    begin
                        TestBus := ckt.BusList.NameOfIndex(BusIndex);
                        DSS.ProgressFormCaption('Testing bus ' + TestBus);
                        DSS.ShowPctProgress(Round((100 * solution.ProgressCount) / ProgressMax));

                        DSS.EnergyMeterClass.ResetAll();

                        // Get the Number of Phases at this bus and the Node Ref and add into the Aux Current Array

                        // Assume either a 3-phase or 1-phase Capacitor
                        if ckt.Buses[BusIndex].NumNodesThisBus < 3 then
                            Phases := 1
                        else
                            Phases := 3;

                        // Apply the capacitor at the bus rating

                        kVrat := ckt.Buses[BusIndex].kVBase;  // L-N Base kV
                        Ycap := (TestCapkvar * 0.001 / Phases) / (kVRat * kVRat);


                        // - -- - - - - - - Solution - - - - - - - - - - - - - - -
                        ckt.IsSolved := FALSE;

                        solution.UseAuxCurrents := TRUE; // Calls InjCurrents on callback
                        solution.SolveSnap();

                        if ckt.IsSolved then
                        begin
                            // Only do this if solution converged ELSE something might break
                            // in meter sampling

                            DSS.EnergyMeterClass.SampleAll();

                            LossImproveFactor := WeightedLosses;

                            FSWrite(Flog, Format('"%s", %-g', [TestBus, ckt.Buses[BusIndex].kVBase * SQRT3]));
                            FSWrite(Flog, Format(', %-g, %-g', [kWLosses, puLossImprovement * 100.0]));
                            FSWrite(Flog, Format(', %-g, %-g', [kWEEN, puEENImprovement * 100.0]));
                            FSWriteln(Flog, Format(', %-g, %d', [LossImproveFactor, solution.Iteration]));

                            if LossImproveFactor > MaxLossImproveFactor then
                            begin
                                MaxLossImproveFactor := LossImproveFactor;
                                MinLossBus := BusIndex;
                                MinBusPhases := Phases;
                            end;
                        end;
                    end;
                    if DSS.SolutionAbort then
                        Break;
                end;


                // Put Control mode back to default before inserting Capacitor for real
                solution.ControlMode := CTRLSTATIC;
                solution.UseAuxCurrents := FALSE;

                if MinLossBus > 0 then
                begin
                    if MinBusPhases >= 3 then
                        kVrat := ckt.Buses[MinLossBus].kVBase * SQRT3
                    else
                        kVrat := ckt.Buses[MinLossBus].kVBase;

                    CommandString := 'New Capacitor.' + GetUniqueCapName() +
                        ', bus1="' + ckt.BusList.NameOfIndex(MinLossBus) +
                        '", phases=' + IntToStr(MinBusPhases) +
                        ', kvar=' + Format('%-g', [TestCapkvar]) +
                        ', kv=' + Format('%-g', [kVrat]);
                    
                    DSS.DSSExecutive.ParseCommand(CommandString);     // Defines capacitor

                    // Append this command to 'DSSAutoAddedCapacitors.txt'
                    AppendToFile('Capacitors', CommandString);

                    solution.SolveSnap();  // for rebuilding of lists, etc.
                end;

                // Return location of added generator so that it can
                // be picked up through the result string of the COM interface
                DSS.GlobalResult := ckt.BusList.NameOfIndex(MinLossBus);

                // note that the command that added the generator can be
                // picked up from the Command property of the COM interface.
            end;
        end;
    finally
        FLog.Free();
    end;
end;

procedure TAutoAdd.AddCurrents(SolveType: Integer);
// Compute injection Currents for generator or capacitor and add into
// system Currents array
var
    BusV: Complex;
    i, Nref: Integer;
    ckt: TDSSCircuit;
    solution: TSolutionObj;
begin
    ckt := DSS.ActiveCircuit;
    solution := ckt.Solution;

    case AddType of
        GENADD:
        // For buses with voltage <> 0, add into aux current array
        for i := 1 to Phases do
        begin
            Nref := ckt.Buses[BusIndex].GetRef(i);
            if Nref > 0 then
            begin   // add in only non-ground currents
                BusV := solution.NodeV[Nref];
                if (BusV.re <> 0.0) or (BusV.im <> 0.0) then
                    // Current  INTO the system network
                    case SolveType of
                        NEWTONSOLVE:
                            solution.Currents[NRef] -= cong(GenVA / BusV);  // Terminal Current
                        NORMALSOLVE:
                            solution.Currents[NRef] += cong(GenVA / BusV);   // Injection Current
                    end;
            end;
        end;
        CAPADD:
        // For buses with voltage <> 0, add into aux current array
        for i := 1 to Phases do
        begin
            Nref := ckt.Buses[BusIndex].GetRef(i);
            if Nref > 0 then
            begin
                BusV := solution.NodeV[Nref];
                if (BusV.re <> 0.0) or (BusV.im <> 0.0) then
                    // Current  INTO the system network
                    case SolveType of
                        NEWTONSOLVE:
                            solution.Currents[NRef] += Cmplx(0.0, Ycap) * BusV; // Terminal Current
                        NORMALSOLVE:
                            solution.Currents[NRef] += Cmplx(0.0, -Ycap) * BusV; // Injection Current
                    end;  // Constant Y model
            end;
        end;
    end;
end;

procedure TAutoAdd.ComputekWLosses_EEN;
var
    pMeter: TEnergyMeterObj;
begin
    if DSS.ActiveCircuit.EnergyMeters.Count = 0 then
    begin
        // No energymeters in circuit
        // Just go by total system losses
        kWLosses := DSS.ActiveCircuit.Losses.re * 0.001;
        kWEEN := 0.0;
    end
    else
    begin   // Sum losses in energy meters and add EEN
        kWLosses := 0.0;
        kWEEN := 0.0;
        for pMeter in DSS.ActiveCircuit.Energymeters do
        begin
            kWLosses += SumSelectedRegisters(pMeter, DSS.ActiveCircuit.LossRegs);
            kWEEN += SumSelectedRegisters(pMeter, DSS.ActiveCircuit.UEregs);
        end;
    end;
end;

procedure TAutoAdd.SetBaseLosses;
begin
    ComputekWLosses_EEN;
    BaseLosses := kWLosses;
    BaseEEN := kWEEN;
end;

end.
