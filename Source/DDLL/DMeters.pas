unit DMeters;

interface

function MetersI(mode: Longint; arg: Longint): Longint; CDECL;
function MetersF(mode: Longint; arg: Double): Double; CDECL;
function MetersS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure MetersV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
    EnergyMeter,
    DSSGlobals,
    SysUtils,
    ucomplex,
    Variants,
    CktElement,
    PDElement,
    MemoryMap_lib,
    CktTree;

function MetersI(mode: Longint; arg: Longint): Longint; CDECL;

var
    pMeter: TEnergyMeterObj;
    AssumeRestoration: Wordbool;
    PD_Element: TPDElement;

begin
    Result := 0; // Default return value
    case mode of
        0:
        begin   // Meters.First
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeter := EnergyMeters.First;
                    if pMeter <> NIL then
                    begin
                        repeat
                            if pMeter.Enabled then
                            begin
                                ActiveCktElement := pMeter;
                                Result := 1;
                            end
                            else
                                pMeter := EnergyMeters.Next;
                        until (Result = 1) or (pMeter = NIL);
                    end
                    else
                        Result := 0;  // signify no more
                end;
        end;
        1:
        begin  // Meters.Next
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMeter := ActiveCircuit[ActiveActor].EnergyMeters.next;
                if pMeter <> NIL then
                begin
                    repeat   // Find an Enabled Meter
                        if pMeter.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pMeter;
                            Result := ActiveCircuit[ActiveActor].EnergyMeters.ActiveIndex;
                        end
                        else
                            pMeter := ActiveCircuit[ActiveActor].EnergyMeters.next;
                    until (Result > 0) or (pMeter = NIL);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        2:
        begin  // Meters.Reset
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMeter := ActiveCircuit[ActiveActor].EnergyMeters.Active;
                if pMeter <> NIL then
                    pMeter.ResetRegisters;
            end;
        end;
        3:
        begin  // Meters.ResetAll
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                EnergyMeterClass[ActiveActor].ResetAll(ActiveActor);
            end;
        end;
        4:
        begin  // Meters.Sample
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMeter := ActiveCircuit[ActiveActor].EnergyMeters.Active;
                if pMeter <> NIL then
                    pMeter.TakeSample(ActiveActor);
            end;
        end;
        5:
        begin  // Meters.Save
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMeter := ActiveCircuit[ActiveActor].EnergyMeters.Active;
                if pMeter <> NIL then
                    pMeter.SaveRegisters(ActiveActor);
            end;
        end;
        6:
        begin  // Meters.MeteredTerminal read
  // First make sure active circuit element is a meter
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMeter := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
                if pMeter <> NIL then
                begin
                    Result := pMeter.MeteredTerminal;
                end
                else
                    Result := 0;
            end
            else
            begin
                Result := 0;
            end;
        end;    // Meters.MeteredTerminal Write
        7:
        begin
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMeter := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
                if pMeter <> NIL then
                begin
                    pMeter.MeteredTerminal := arg;
                    pMeter.MeteredElementChanged := TRUE;
                    pMeter.RecalcElementData(ActiveActor);
                end;
            end;
        end;
        8:
        begin  // Meters.DIFilesAreOpen
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if DIFilesAreOpen[ActiveActor] then
                    Result := 1;    // Global variable
            end;
        end;
        9:
        begin  // Meters.SampleAll
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                EnergyMeterClass[ActiveActor].SampleAll(ActiveActor);
            end;
            Result := 0;
        end;
        10:
        begin  // Meters.SaveAll
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                EnergyMeterClass[ActiveActor].SaveAll(ActiveActor);
            end;
            Result := 0;
        end;
        11:
        begin  // Meters.OpenAllDIFiles
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                EnergyMeterClass[ActiveActor].OpenAllDIFiles(ActiveActor);
            end;
            Result := 0;
        end;
        12:
        begin  // Meters.CloseAllDIFiles
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                EnergyMeterClass[ActiveActor].CloseAllDIFiles(ActiveActor);
            end;
            Result := 0;
        end;
        13:
        begin  // Meters.CountEndElements
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMeter := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
                if pMeter <> NIL then
                begin
                    Result := pMeter.BranchList.ZoneEndsList.NumEnds;
                end;
            end;
        end;
        14:
        begin  // Meters.Count
            if Assigned(ActiveCircuit[ActiveActor]) then
                Result := ActiveCircuit[ActiveActor].EnergyMeters.ListSize;
        end;
        15:
        begin  // Meters.CountBranches
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeter := EnergyMeters.Active;
                    if pMeter <> NIL then
                        Result := pMeter.SequenceList.ListSize;
      (*
      If pMeterObj.BranchList <> Nil then Begin
          // Get count of branches
        pElem := pMeterObj.BranchList.First;
        while pElem <> Nil do   Begin
           inc(Result);
           pElem := pMeterObj.BranchList.GoForward;
        End;
      end;
      *)
                end;
        end;
        16:
        begin   // Meters.SequenceList read
            Result := 0;
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeter := TEnergyMeterObj(EnergyMeters.Active);
                    if pMeter <> NIL then
                    begin
                        Result := pMeter.SequenceList.ActiveIndex;
                    end;
                end;
        end;
        17:
        begin   // Meters.SequenceList Write
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeter := TEnergyMeterObj(EnergyMeters.Active);
                    if pMeter <> NIL then
                        with pMeter do
                        begin
                            if (arg > 0) and (arg <= SequenceList.ListSize) then
                                ActiveCktElement := SequenceList.Get(arg)
                            else
                                DoSimpleMsg(Format('Invalid index for SequenceList: %d. List size is %d.', [arg, SequenceList.ListSize]), 500501);
                        end;
                end;
        end;
        18:
        begin  // Meters.DoReliabilityCalc
            AssumeRestoration := FALSE;
            if arg = 1 then
                AssumeRestoration := TRUE;
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeter := TEnergyMeterObj(EnergyMeters.Active);
                    if pMeter <> NIL then
                    begin
                        pMeter.CalcReliabilityIndices(AssumeRestoration, ActiveActor);

                    end;
                end;
        end;
        19:
        begin  // Meters.SeqListSize
            Result := 0;
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeter := TEnergyMeterObj(EnergyMeters.Active);
                    if pMeter <> NIL then
                    begin
                        Result := pMeter.SequenceList.ListSize;
                    end;
                end;
        end;
        20:
        begin  // Meters.TotalCustomers
            Result := 0;
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeter := TEnergyMeterObj(EnergyMeters.Active);
                    if pMeter <> NIL then
                    begin
                        PD_Element := pMeter.SequenceList.Get(1);
                        if Assigned(PD_Element) then
                            with PD_Element do
                                Result := Buses^[Terminals^[FromTerminal].BusRef].BusTotalNumCustomers;
                    end;
                end;
        end;
        21:
        begin  // Meters.NumSections
            Result := 0;
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeter := TEnergyMeterObj(EnergyMeters.Active);
                    if pMeter <> NIL then
                    begin
                        Result := pMeter.SectionCount;
                    end;
                end;
        end;
        22:
        begin  // Meters.SetActiveSection
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeter := TEnergyMeterObj(EnergyMeters.Active);
                    if pMeter <> NIL then
                    begin
                        if (arg > 0) and (arg <= pMeter.SectionCount) then
                            pMeter.ActiveSection := arg
                        else
                            pMeter.ActiveSection := 0;
                    end;
                end;
        end;
        23:
        begin  // Meters.OCPDeviceType
            Result := 0;
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeter := TEnergyMeterObj(EnergyMeters.Active);
                    if pMeter <> NIL then
                        with pMeter do
                        begin
                            if ActiveSection > 0 then
                                Result := FeederSections^[ActiveSection].OCPDeviceType;
                        end;
                end;
        end;
        24:
        begin  // Meters.NumSectionCustomers
            Result := 0;
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeter := TEnergyMeterObj(EnergyMeters.Active);
                    if pMeter <> NIL then
                        with pMeter do
                        begin
                            if ActiveSection > 0 then
                                Result := FeederSections^[ActiveSection].NCustomers;
                        end;
                end;
        end;
        25:
        begin // Meters.NumSectionBranches
            Result := 0;
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeter := TEnergyMeterObj(EnergyMeters.Active);
                    if pMeter <> NIL then
                        with pMeter do
                        begin
                            if ActiveSection > 0 then
                                Result := FeederSections^[ActiveSection].NBranches;
                        end;
                end;
        end;     // Meters.SectSeqidx
        26:
        begin
            Result := 0;
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeter := TEnergyMeterObj(EnergyMeters.Active);
                    if pMeter <> NIL then
                        with pMeter do
                        begin
                            if ActiveSection > 0 then
                                Result := FeederSections^[ActiveSection].SeqIndex;
                        end;
                end;
        end;
        27:
        begin  // Meters.SectTotalCust
            Result := 0;
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeter := TEnergyMeterObj(EnergyMeters.Active);
                    if pMeter <> NIL then
                        with pMeter do
                        begin
                            if ActiveSection > 0 then
                                Result := FeederSections^[ActiveSection].TotalCustomers;
                        end;
                end;
        end
    else
        Result := -1; // The parameter is not valid
    end;
end;

//*************************Floating point type properties***************************
function MetersF(mode: Longint; arg: Double): Double; CDECL;

var
    pMeterObj: TEnergyMeterObj;

begin
    Result := 0.0;  // Default return value
    case mode of
        0:
        begin  // Meters.SAIFI
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
                    if pMeterObj <> NIL then
                    begin

                        Result := pMeterObj.SAIFI;

                    end;
                end;
        end;
        1:
        begin   // Meters.SAIFIkW
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
                    if pMeterObj <> NIL then
                    begin

                        Result := pMeterObj.SAIFIkW;

                    end;
                end;
        end;
        2:
        begin  // Meters.SAIDI
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
                    if pMeterObj <> NIL then
                    begin
                        Result := pMeterObj.SAIDI;
                    end;
                end;
        end;
        3:
        begin  // Meters.CustItnerrupts
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
                    if pMeterObj <> NIL then
                    begin
                        Result := pMeterObj.CustInterrupts;
                    end;
                end;
        end;
        4:
        begin  // Meters.AvgRepairTime
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
                    if pMeterObj <> NIL then
                        with pMeterObj do
                        begin
                            if ActiveSection > 0 then
                                Result := FeederSections^[ActiveSection].AverageRepairTime;
                        end;
                end;
        end;
        5:
        begin  // Meters.FaultRateXRepairHrs
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
                    if pMeterObj <> NIL then
                        with pMeterObj do
                        begin
                            if ActiveSection > 0 then
                                Result := FeederSections^[ActiveSection].SumFltRatesXRepairHrs;
                        end;
                end;
        end;
        6:
        begin  // Meters.SumBranchFltRates
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
                    if pMeterObj <> NIL then
                        with pMeterObj do
                        begin
                            if ActiveSection > 0 then
                                Result := FeederSections^[ActiveSection].SumBranchFltRates;
                        end;
                end;
        end
    else
        Result := -1.0;
    end;
end;

//********************************String type properties**************************
function MetersS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var
    pMeterObj: TEnergyMeterObj;
    activesave: Integer;
    TestStr: String;
    Found: Boolean;

begin
    Result := Pansichar(Ansistring('0')); // Default return value
    case mode of
        0:
        begin  // Meters.Name read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
                if pMeterObj <> NIL then
                    Result := Pansichar(Ansistring(pMeterObj.name));
            end;
        end;
        1:
        begin // Meters.Name Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin      // Search list of EnergyMeters in active circuit for name
                with ActiveCircuit[ActiveActor].EnergyMeters do
                begin
                    TestStr := String(arg);  // Convert to Pascal String for testing
                    Found := FALSE;
                    ActiveSave := ActiveIndex;
                    pMeterObj := First;
                    while pMeterObj <> NIL do
                    begin
                        if (CompareText(pMeterObj.Name, TestStr) = 0) then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pMeterObj;
                            Found := TRUE;
                            Break;
                        end;
                        pMeterObj := Next;
                    end;
                    if not Found then
                    begin
                        DoSimpleMsg('EnergyMeter "' + TestStr + '" Not Found in Active Circuit.', 5005);
                        pMeterObj := Get(ActiveSave);    // Restore active Meter
                        ActiveCircuit[ActiveActor].ActiveCktElement := pMeterObj;
                    end;
                end;
            end;
        end;
        2:
        begin   // Meters.MeteredElement read
  // First make sure active circuit element is a meter
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
                if pMeterObj <> NIL then
                begin
                    Result := Pansichar(Ansistring(pMeterObj.ElementName));
                end
                else
                    Result := Pansichar(Ansistring(''));
            end
            else
            begin
                Result := Pansichar(Ansistring(''));
            end;
        end;
        3:
        begin  // Meters.MeteredElement Write
    // First make sure active circuit element is a meter
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
                if pMeterObj <> NIL then
                begin
                    pMeterObj.elementName := String(arg);
                    pMeterObj.MeteredElementChanged := TRUE;
                    pMeterObj.RecalcElementData(ActiveActor);
                end;
            end;
        end
    else
        Result := Pansichar(Ansistring('Error, Parameter not recognized'));
    end;
end;

//***************************Variant type properties******************************
procedure MetersV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

var
    pMeter,
    pMeterObj,
    MeterElem: TEnergyMeterObj;
    BranchCount,
    last,
    k,
    i: Integer;
    pElem,
    elem: TDSSCktElement;
    node: TCktTreeNode;
    PDouble: ^Double;

begin
    case mode of
        0:
        begin  // Meters.AllNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    if (EnergyMeters.ListSize > 0) then
                    begin
                        MeterElem := EnergyMeters.First;
                        while MeterElem <> NIL do
                        begin
                            WriteStr2Array(MeterElem.Name);
                            WriteStr2Array(Char(0));
                            MeterElem := EnergyMeters.Next;
                        end;
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        1:
        begin  // Meters.RegisterNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
            if Assigned(pMeterObj) then
            begin
                for k := 0 to NumEMRegisters - 1 do
                begin
                    WriteStr2Array(pMeterObj.RegisterNames[k + 1]);
                    WriteStr2Array(Char(0));
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        2:
        begin  // Meters.RegisterValues
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
                if pMeterObj <> NIL then
                begin
                    setlength(myDBLArray, numEMRegisters);
                    for k := 0 to numEMRegisters - 1 do
                    begin
                        myDBLArray[k] := pMeterObj.Registers[k + 1];
                    end;
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        3:
        begin  // Meters.Totals
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    TotalizeMeters;
                    setlength(myDBLArray, NumEMRegisters);
                    for i := 1 to NumEMregisters do
                        myDBLArray[i - 1] := RegisterTotals[i];
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        4:
        begin  // Meters.PeakCurrent read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
                if pMeterObj <> NIL then
                begin
                    setlength(myDBLArray, pMeterObj.NPhases);
                    for k := 0 to pMeterObj.NPhases - 1 do
                        myDBLArray[k] := pMeterObj.SensorCurrent^[k + 1];
                end
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        5:
        begin  // Meters.PeakCurrent Write
            myType := 2;        // Double
            k := 1;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
                if pMeterObj <> NIL then
                begin
                    for i := 1 to pMeterObj.NPhases do
                    begin
                        PDouble := myPointer;
                        pMeterObj.SensorCurrent^[i] := PDouble^;
                        inc(k);
                        inc(Pbyte(myPointer), 8);
                    end;
                end;
            end;
            mySize := k - 1;
        end;
        6:
        begin  // Meter.CalcCurrent read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
                if pMeterObj <> NIL then
                begin
                    setlength(myDBLArray, pMeterObj.NPhases);
                    for k := 0 to pMeterObj.NPhases - 1 do
                        myDBLArray[k] := Cabs(pMeterObj.CalculatedCurrent^[k + 1]);
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        7:
        begin  // Meters.CalcCurrent Write
    // First make sure active circuit element is a meter
            myType := 2;        // Double
            k := 1;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
                if pMeterObj <> NIL then
                begin
                    for i := 1 to pMeterObj.NPhases do
                    begin
                        PDouble := myPointer;
                        pMeterObj.CalculatedCurrent^[i] := cmplx(PDouble^, 0.0);   // Just set the real part
                        inc(k);
                        inc(Pbyte(myPointer), 8);
                    end;
                end;
            end;
            mySize := k - 1;
        end;
        8:
        begin  // Meters.AllocFactors read
    // First make sure active circuit element is a meter
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
                if pMeterObj <> NIL then
                begin
                    setlength(myDBLArray, pMeterObj.NPhases);
                    for k := 0 to pMeterObj.NPhases - 1 do
                        myDBLArray[k] := pMeterObj.PhsAllocationFactor^[k + 1];
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        9:
        begin   // Meters.AllocFactors Write
    // First make sure active circuit element is a meter
            myType := 2;        // Double
            k := 1;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
                if pMeterObj <> NIL then
                begin
                    for i := 1 to pMeterObj.NPhases do
                    begin
                        PDouble := myPointer;
                        pMeterObj.PhsAllocationFactor^[i] := PDouble^;   // Just set the real part
                        inc(k);
                        inc(Pbyte(myPointer), 8);
                    end;
                end;
            end;
            mySize := k - 1;
        end;
        10:
        begin  // Meters.AllEndElements
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeterObj := EnergyMeters.Active;
                    if pMeterObj <> NIL then
                    begin
                        last := pMeterObj.BranchList.ZoneEndsList.NumEnds - 1;
                        for k := 0 to last do
                        begin
                            pMeterObj.BranchList.ZoneEndsList.Get(k + 1, node);
                            elem := node.CktObject;
                            WriteStr2Array(Format('%s.%s', [elem.ParentClass.Name, elem.Name]));
                            WriteStr2Array(Char(0));
                        end;
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        11:
        begin  // Meters.ALlBranchesInZone
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeterObj := EnergyMeters.Active;
                    if pMeterObj <> NIL then
                    begin
            // Get count of branches
                        BranchCount := MetersI(15, 0);
                        if BranchCount > 0 then
                        begin
                            pElem := pMeterObj.BranchList.First;
                            while pElem <> NIL do
                            begin
                                WriteStr2Array(Format('%s.%s', [pElem.ParentClass.Name, pElem.Name]));
                                WriteStr2Array(Char(0));
                                pElem := pMeterObj.BranchList.GoForward;
                            end;
                        end;
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        12:
        begin  // Meters.ALLPCEinZone
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    pMeter := EnergyMeters.Active;
                    if pMeter <> NIL then
                    begin
                        pMeter.GetPCEatZone;
            // moves the list to the variant output
                        if (length(pMeter.ZonePCE) > 0) and (pMeter.ZonePCE[0] <> '') then
                        begin
                            for k := 0 to High(pMeter.ZonePCE) do
                            begin
                                WriteStr2Array(pMeter.ZonePCE[k]);
                                WriteStr2Array(Char(0));
                            end;
                        end;
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end
    else
    begin
        myType := 4;        // String
        setlength(myStrArray, 0);
        WriteStr2Array('Error, parameter not recognized');
        myPointer := @(myStrArray[0]);
        mySize := Length(myStrArray);
    end;
    end;
end;

end.
