unit AutoAdd;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{  Unit for processing the AutoAdd Solution FUNCTIONs

  Note: Make sure this class in instantiated after energymeter class

  There is one of these per circuit

  6/11/00 - reorganized object
  6/14/00 - resolved sign issue with normal and Newton solution in AddCurrents
  9/13/03 - Modified to use pu improvement in losses and EEN instead of kW
}

{$M+}

interface

uses
    uComplex,
    EnergyMeter,
    HashList,
    Arraydef,
    Generator,
    Capacitor;

type
    TAutoAdd = class(TObject)
    PRIVATE
        GeneratorClass: TGenerator;
        CapacitorClass: TCapacitor;

        BusIdxList: pIntegerArray;
        BusIdxListSize: Integer;
        BusIdxListCreated: Boolean;
        LastAddedGenerator,
        LastAddedCapacitor: Integer;

        BusIndex,
        Phases: Integer;

        Ycap: Double;
        GenVA: Complex;

        kWLosses, BaseLosses, puLossImprovement: Double;
        kWEEN, BaseEEN, puEENImprovement: Double;

        FLog: Textfile;  // Log File

        ProgressCount: Integer;

        function Get_WeightedLosses: Double;

        procedure ComputekWLosses_EEN;
        procedure SetBaseLosses;

        function GetUniqueGenName: String;
        function GetUniqueCapName: String;

    PROTECTED

    PUBLIC

    {Autoadd mode Variables}
        GenkW,
        GenPF,
        Genkvar,
        Capkvar: Double;
        AddType: Integer;

        ModeChanged: Boolean;

        constructor Create;
        destructor Destroy; OVERRIDE;

        procedure MakeBusList;
        procedure AppendToFile(const WhichFile, S: String);
        procedure AddCurrents(SolveType: Integer);

        function Solve: Integer; // Automatically add caps or generators

        property WeightedLosses: Double READ Get_WeightedLosses;

    PUBLISHED

    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    PDElement,
    Utilities,
    SysUtils,
    Executive,
{$IFDEF FPC}
    CmdForms,
{$ELSE}
    DSSForms,
{$ENDIF}
      {ProgressForm, Forms,} Solution;

function SumSelectedRegisters(Mtr: TEnergyMeterObj; Regs: pIntegerArray; count: Integer): Double;
var
    i: Integer;
begin
    Result := 0.0;
    with Mtr do
        for i := 1 to count do
        begin
            Result := Result + Registers[regs^[i]] * TotalsMask[Regs^[i]];
        end;
end;


constructor TAutoAdd.Create;
begin

    BusIdxListCreated := FALSE;
    GeneratorClass := DSSClassList.Get(ClassNames.Find('generator'));
    CapacitorClass := DSSClassList.Get(ClassNames.Find('capacitor'));

         // AutoAdd defaults
    GenkW := 1000.0;
    GenPF := 1.0;
    Capkvar := 600.0;
    AddType := GENADD;
    LastAddedGenerator := 0;
    LastAddedCapacitor := 0;

    ModeChanged := TRUE;


end;

destructor TAutoAdd.Destroy;
begin
    if BusIdxListCreated then
        ReallocMem(BusIdxList, 0);
    inherited;
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
    FBusList: THashList;
    FBusListCreatedHere: Boolean;

begin

    if (BusIdxListCreated) then
        ReallocMem(BusIdxList, 0);

    FBusListCreatedHere := FALSE;
    BusIdxListCreated := FALSE;

    // Autoaddbuslist exists in Active Circuit, use it  (see set Autobuslist=)
    if ActiveCircuit.AutoAddBusList.ListSize > 0 then
        FBusList := ActiveCircuit.AutoAddBusList
    else

    if ActiveCircuit.EnergyMeters.ListSize = 0 then
    begin
        // No energymeters in circuit
        // Include all buses in the circuit
        BusIdxListSize := ActiveCircuit.BusList.listsize;
        BusIdxList := AllocMem(Sizeof(BusIdxList^[1]) * BusIdxListSize);

        for i := 1 to BusIdxListSize do
        begin
            BusIdxList^[i] := i;
        end;

        BusIdxListCreated := TRUE;
        Exit;
    end
    else
    begin
         {Construct Bus List from Energy Meters Zone Lists}
         // Include only buses in EnergyMeter lists
             // Consider all meters
        FBusListCreatedHere := TRUE;
        FBusList := THashList.Create(ActiveCircuit.NumBuses);
        pMeter := ActiveCircuit.EnergyMeters.First;
        while pMeter <> NIL do
        begin

            if pMeter.BranchList <> NIL then
            begin
                PDElem := pMeter.BranchList.First;
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
                    PDElem := pMeter.BranchList.GoForward;
                end;
            end;
            pMeter := ActiveCircuit.EnergyMeters.Next;
        end;
    end;

     // Make busIdxList from FBusList
    BusIdxListSize := FBusList.listsize;
    BusIdxList := AllocMem(Sizeof(BusIdxList^[i]) * BusIdxListSize);

    for i := 1 to BusIdxListSize do
    begin
        BusIdxList^[i] := ActiveCircuit.BusList.Find(FbusList.Get(i));
    end;

    if FBusListCreatedHere then
        FBusList.Free;
    BusIdxListCreated := TRUE;
end;


function TAutoAdd.Get_WeightedLosses: Double;

// Returns losses in metered part of circuit +
// weighted EEN values

{If no meters, returns just total losses in circuit}

{Base everything on gen kW}


begin

    ComputekWLosses_EEN;

    if ActiveCircuit.EnergyMeters.ListSize = 0 then
    begin
        // No energymeters in circuit
        // Just go by total system losses
        puLossImprovement := (BaseLosses - kWLosses) / GenkW;
        puEENImprovement := 0.0;
        Result := puLossImprovement;
    end
    else
        with ActiveCircuit do
        begin
            puLossImprovement := (BaseLosses - kWLosses) / GenkW;
            puEENImprovement := (BaseEEN - kWEEN) / GenkW;
            Result := LossWeight * puLossImprovement + UEWeight * puEENImprovement;
        end;
end;

procedure TAutoAdd.AppendToFile(const WhichFile, S: String);

var
    F: TextFile;
    Fname: String;

begin

    try
        FName := GetOutputDirectory + CircuitName_ + 'AutoAdded' + WhichFile + '.txt';
        AssignFile(F, Fname);

        if FileExists(FName) then
            Append(F)
        else
            ReWrite(F);

        Writeln(F, S);

    except
        On E: EXCEPTion do
            DoSimpleMsg('Error TRYing to append to ' + Fname + CRLF +
                E.Message, 438);
    end;
    CloseFile(F);
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function TAutoAdd.GetUniqueGenName: String;

var
  // TimeStmp:        TTimeStamp;
    TrialName: String;
    Done: Boolean;

begin

    repeat
        Done := TRUE;
        Inc(LastAddedGenerator);
        TrialName := 'Gadd' + IntToStr(LastAddedGenerator);
        if GeneratorClass.Find(TrialName) <> NIL then
            Done := FALSE;
    until Done;

    Result := TrialName;

end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function TAutoAdd.GetUniqueCapName: String;

var
  // TimeStmp:        TTimeStamp;
    TrialName: String;
    Done: Boolean;

begin
    // TimeStmp := DateTimeToTimeStamp(Now);
    // Result := IntToStr(TimeStmp.date-730000)+'_'+IntToStr(TimeStmp.time);
    repeat
        Done := TRUE;
        Inc(LastAddedCapacitor);
        TrialName := 'Cadd' + IntToStr(LastAddedCapacitor);
        if CapacitorClass.Find(TrialName) <> NIL then
            Done := FALSE;
    until Done;

    Result := TrialName;

end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function TAutoAdd.Solve: Integer; // Automatically add caps or generators
{
 Automatically add a specified size of generator or capacitor at the location
 that results in the lowest losses in either metered part of circuit or
 total circuit, if no meters.

 If metered, EEN is also added in WITH a selected weighting factor (see
 set ueweight= ... command).

 Thus, this algorithm placed generators and capacitors to minimize losses and
 potential unserved energy.

}

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

begin

{  Algorithm:
     1) makes a list of buses to check, either
        a. Previously defined list
        b. Meter zone lists
        c. All buses, if neither of the above
     2) Inject a current corresponding to the generator
     3) Check test criteria
     4) Save result
     5) Add generator/capacitor to circuit

}
    Result := 0;

    with ActiveCircuit, ActiveCircuit.Solution do
    begin


        if (LoadModel = ADMITTANCE) then
        begin
            LoadModel := POWERFLOW;
            SystemYChanged := TRUE;  // Force rebuild of System Y without Loads
        end;

    {Do a preliminary snapshot solution to Force definition of meter zones
     And set bus lists}
        EnergyMeterClass.ResetAll;
        if SystemYChanged or ActiveCircuit.BusNameRedefined then
        begin
            SolveSnap;
            ModeChanged := TRUE;
        end;

        EnergyMeterClass.SampleAll;

    { Check to see if bus base voltages have been defined }
        if Buses^[NumBuses].kVBase = 0.0 then
            SetVoltageBases;

        if ModeChanged then
        begin
            MakeBusList;  // Make list of buses to check
            ModeChanged := FALSE;  {Keep same BusIdxList if no changes}
        end;

        IntervalHrs := 1.0;

    {Start up Log File}

        AssignFile(FLog, GetOutputDirectory + CircuitName_ + 'AutoAddLog.CSV');
        Rewrite(FLog);
        Writeln(Flog, '"Bus", "Base kV", "kW Losses", "% Improvement", "kW UE", "% Improvement", "Weighted Total", "Iterations"');
        CloseFile(FLog); // Close it now after clearing it out

    // for this solution mode, only the peak load condition is taken into account
    // load is adjusted for growth by year.
        SetGeneratorDispRef;

    {Turn regulators and caps off while we are searching}
        ControlMode := CONTROLSOFF;

        SetBaseLosses;  {Establish base values}

        case AddType of

            GENADD:
            begin
                if ActiveCircuit.PositiveSequence then
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


                   {Progress meter}
                ProgressCaption('AutoAdding Generators');
                ProgressMax := BusIdxListSize;
                ProgressCount := 0;

                ProgressFormCaption(Format('Testing %d buses. Please Wait... ', [BusIdxListSize]));
                ShowPctProgress(0);


                for i := 1 to BusIdxListSize do
                begin

                    Inc(ProgressCount);

                    BusIndex := BusIdxList^[i];

                    if BusIndex > 0 then
                    begin

                        TestBus := BusList.Get(BusIndex);
                         // ProgressFormCaption( 'Testing bus ' + TestBus);
                        if ((ProgressCount mod 20) = 0) or (i = BusIdxListSize) then
                        begin
                            ProgressFormCaption(Format('Testing bus %d/%d. ', [i, BusIdxListSize]));
                            ShowPctProgress(Round((100 * ProgressCount) / ProgressMax));
                        end;

                        EnergyMeterClass.ResetAll;

                         {Get the Number of Phases at this bus and the Node Ref and add into the Aux Current Array}

                         {Assume either a 3-phase or 1-phase generator}
                        if Buses^[BusIndex].NumNodesThisBus < 3 then
                            Phases := 1
                        else
                            Phases := 3;

                        GenVA := Cmplx(1000.0 * TestGenkW / Phases, 1000.0 * Genkvar / Phases);

                         { - -- - - - - - - Solution - - - - - - - - - - - - - - -}
                        Issolved := FALSE;

                        UseAuxCurrents := TRUE;   // Calls InjCurrents on callback
                        SolveSnap;

                        if IsSolved then
                        begin
                              {Only do this if solution converged ELSE something might break
                               in meter sampling}

                            EnergyMeterClass.SampleAll;

                            LossImproveFactor := WeightedLosses;

                            try
                                Append(Flog);
                                Write(Flog, Format('"%s", %-g', [TestBus, Buses^[BusIndex].kVBase * SQRT3]));
                                Write(Flog, Format(', %-g, %-g', [kWLosses, puLossImprovement * 100.0]));
                                Write(Flog, Format(', %-g, %-g', [kWEEN, puEENImprovement * 100.0]));
                                Writeln(Flog, Format(', %-g, %d', [LossImproveFactor, Iteration]));
                            finally
                                CloseFile(Flog);
                            end;

                            if LossImproveFactor > MaxLossImproveFactor then
                            begin
                                MaxLossImproveFactor := LossImproveFactor;
                                MinLossBus := BusIndex;
                                MinBusPhases := Phases;
                            end;

                        end;
                    end;
                    if SolutionAbort then
                        Break;
                end;

                   {Put Control mode back to default before inserting Generator for real}
                ControlMode := CTRLSTATIC;
                UseAuxCurrents := FALSE;

                if MinLossBus > 0 then
                    with DSSExecutive do
                    begin

                        if MinBusPhases >= 3 then
                            kVrat := Buses^[MinLossBus].kVBase * SQRT3
                        else
                            kVrat := Buses^[MinLossBus].kVBase;
                        CommandString := 'New, generator.' + GetUniqueGenName +
                            ', bus1="' + BusList.Get(MinLossBus) +
                            '", phases=' + IntToStr(MinBusPhases) +
                            ', kv=' + Format('%-g', [kVrat]) +
                            ', kw=' + Format('%-g', [TestGenkW]) +
                            ', ' + Format('%5.2f', [GenPF]) +
                            Format('! Factor =  %-g (%-.3g, %-.3g)', [MaxLossImproveFactor, LossWeight, UEWeight]);
                        Command := CommandString;    // Defines Generator

                       // AppEnd this command to '...AutoAddedGenerators.Txt'
                        AppendToFile('Generators', CommandString);

                        SolveSnap;  // Force rebuilding of lists

                    end;
                   // Return location of added generator so that it can
                   // be picked up through the result string of the COM interface
                GlobalResult := BusList.Get(MinLossBus) +
                    Format(', %-g', [MaxLossImproveFactor]);

                ProgressHide;

                   // note that the command that added the generator can be
                   // picked up from the Command property of the COM interface.
            end;


            CAPADD:
            begin

                MinLossBus := 0;   // null string
                MaxLossImproveFactor := -1.0e50;  // Some very large number
                MinBusPhases := 3;

                if ActiveCircuit.PositiveSequence then
                    TestCapkvar := Capkvar / 3.0
                else
                    TestCapkvar := Capkvar;

                   {Progress meter}
                ProgressCaption('AutoAdding Capacitors');
                ProgressMax := BusIdxListSize;
                ProgressCount := 0;

                for i := 1 to BusIdxListSize do
                begin

                    Inc(ProgressCount);
                   {Make sure testbus is actually in the circuit}
                    BusIndex := BusIdxList^[i];
                    if BusIndex > 0 then
                    begin
                        TestBus := BusList.Get(BusIndex);
                        ProgressFormCaption('Testing bus ' + TestBus);
                        ShowPctProgress(Round((100 * ProgressCount) / ProgressMax));

                        EnergyMeterClass.ResetAll;

                       {Get the Number of Phases at this bus and the Node Ref and add into the Aux Current Array}

                      {Assume either a 3-phase or 1-phase Capacitor}
                        if Buses^[BusIndex].NumNodesThisBus < 3 then
                            Phases := 1
                        else
                            Phases := 3;

                           // Apply the capacitor at the bus rating

                        kVrat := Buses^[BusIndex].kVBase;  // L-N Base kV
                        Ycap := (TestCapkvar * 0.001 / Phases) / (kVRat * kVRat);


                         { - -- - - - - - - Solution - - - - - - - - - - - - - - -}
                        Issolved := FALSE;

                        UseAuxCurrents := TRUE;    // Calls InjCurrents on callback
                        SolveSnap;

                        if IsSolved then
                        begin
                              {Only do this if solution converged ELSE something might break
                               in meter sampling}

                            EnergyMeterClass.SampleAll;

                            LossImproveFactor := WeightedLosses;

                            try
                                Append(Flog);
                                Write(Flog, Format('"%s", %-g', [TestBus, Buses^[BusIndex].kVBase * SQRT3]));
                                Write(Flog, Format(', %-g, %-g', [kWLosses, puLossImprovement * 100.0]));
                                Write(Flog, Format(', %-g, %-g', [kWEEN, puEENImprovement * 100.0]));
                                Writeln(Flog, Format(', %-g, %d', [LossImproveFactor, Iteration]));
                            finally
                                CloseFile(Flog);
                            end;

                            if LossImproveFactor > MaxLossImproveFactor then
                            begin
                                MaxLossImproveFactor := LossImproveFactor;
                                MinLossBus := BusIndex;
                                MinBusPhases := Phases;
                            end;
                        end;
                    end;
                    if SolutionAbort then
                        Break;
                end;


                   {Put Control mode back to default before inserting Capacitor for real}
                ControlMode := CTRLSTATIC;
                UseAuxCurrents := FALSE;

                if MinLossBus > 0 then
                    with DSSExecutive do
                    begin

                        if MinBusPhases >= 3 then
                            kVrat := Buses^[MinLossBus].kVBase * SQRT3
                        else
                            kVrat := Buses^[MinLossBus].kVBase;

                        CommandString := 'New, Capacitor.' + GetUniqueCapName +
                            ', bus1="' + BusList.Get(MinLossBus) +
                            '", phases=' + IntToStr(MinBusPhases) +
                            ', kvar=' + Format('%-g', [TestCapkvar]) +
                            ', kv=' + Format('%-g', [kVrat]);
                        Command := CommandString;     // Defines capacitor

                       // AppEnd this command to 'DSSAutoAddedCapacitors.Txt'
                        AppendToFile('Capacitors', CommandString);


                        SolveSnap;  // for rebuilding of lists, etc.

                    end;
                   // Return location of added generator so that it can
                   // be picked up through the result string of the COM interface
                GlobalResult := BusList.Get(MinLossBus);

                   // note that the command that added the generator can be
                   // picked up from the Command property of the COM interface.

            end;
        end;
    end;

end;

procedure TAutoAdd.AddCurrents(SolveType: Integer);

{ Compute injection Currents for generator or capacitor and add into
  system Currents array
}

var

    BusV: Complex;
    i,
    Nref: Integer;

begin

    case AddType of

        GENADD:
            with ActiveCircuit, ActiveCircuit.Solution do
            begin
           {For buses with voltage <> 0, add into aux current array}
                for i := 1 to Phases do
                begin
                    Nref := Buses^[BusIndex].GetRef(i);
                    if Nref > 0 then
                    begin   // add in only non-ground currents
                        BusV := NodeV^[Nref];
                        if (BusV.re <> 0.0) or (BusV.im <> 0.0) then
                      {Current  INTO the system network}
                            case SolveType of
                                NEWTONSOLVE:
                                    Caccum(Currents^[NRef], Cnegate(Conjg(Cdiv(GenVA, BusV))));  // Terminal Current
                                NORMALSOLVE:
                                    Caccum(Currents^[NRef], Conjg(Cdiv(GenVA, BusV)));   // Injection Current
                            end;
                    end;
                end;
            end;

        CAPADD:
            with ActiveCircuit, ActiveCircuit.Solution do
            begin

           {For buses with voltage <> 0, add into aux current array}
                for i := 1 to Phases do
                begin
                    Nref := Buses^[BusIndex].GetRef(i);
                    if Nref > 0 then
                    begin
                        BusV := NodeV^[Nref];
                        if (BusV.re <> 0.0) or (BusV.im <> 0.0) then
                         {Current  INTO the system network}
                            case SolveType of
                                NEWTONSOLVE:
                                    Caccum(Currents^[NRef], Cmul(Cmplx(0.0, Ycap), BusV)); // Terminal Current
                                NORMALSOLVE:
                                    Caccum(Currents^[NRef], Cmul(Cmplx(0.0, -Ycap), BusV)); // Injection Current
                            end;  // Constant Y model
                    end;
                end;
            end;

    end; {CASE}

end;

procedure TAutoAdd.ComputekWLosses_EEN;
var
    pMeter: TEnergyMeterObj;

begin

    if ActiveCircuit.EnergyMeters.ListSize = 0 then
    begin

        // No energymeters in circuit
        // Just go by total system losses
        kWLosses := ActiveCircuit.Losses.re * 0.001;
        kWEEN := 0.0;

    end
    else
    begin   // Sum losses in energy meters and add EEN
        kWLosses := 0.0;
        kWEEN := 0.0;

        with ActiveCircuit do
        begin

            pMeter := ActiveCircuit.Energymeters.First;
            while pMeter <> NIL do
            begin

                kWLosses := kWLosses + SumSelectedRegisters(pMeter, LossRegs, NumLossRegs);
                kWEEN := kWEEN + SumSelectedRegisters(pMeter, UEregs, NumUEregs);

                pMeter := ActiveCircuit.EnergyMeters.Next;
            end;
        end;
    end;


end;

procedure TAutoAdd.SetBaseLosses;
begin
    ComputekWLosses_EEN;
    BaseLosses := kWLosses;
    BaseEEN := kWEEN;
end;

initialization


end.
