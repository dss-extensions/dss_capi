unit DMeters;

interface

function MetersI(mode: Longint; arg: Longint): Longint; CDECL;
function MetersF(mode: Longint; arg: Double): Double; CDECL;
function MetersS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure MetersV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    EnergyMeter,
    DSSGlobals,
    SysUtils,
    ucomplex,
    Variants,
    CktElement,
    PDElement,
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
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
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
            if ActiveCircuit <> NIL then
            begin
                pMeter := ActiveCircuit.EnergyMeters.next;
                if pMeter <> NIL then
                begin
                    repeat   // Find an Enabled Meter
                        if pMeter.Enabled then
                        begin
                            ActiveCircuit.ActiveCktElement := pMeter;
                            Result := ActiveCircuit.EnergyMeters.ActiveIndex;
                        end
                        else
                            pMeter := ActiveCircuit.EnergyMeters.next;
                    until (Result > 0) or (pMeter = NIL);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        2:
        begin  // Meters.Reset
            if ActiveCircuit <> NIL then
            begin
                pMeter := ActiveCircuit.EnergyMeters.Active;
                if pMeter <> NIL then
                    pMeter.ResetRegisters;
            end;
        end;
        3:
        begin  // Meters.ResetAll
            if ActiveCircuit <> NIL then
            begin
                EnergyMeterClass.ResetAll;
            end;
        end;
        4:
        begin  // Meters.Sample
            if ActiveCircuit <> NIL then
            begin
                pMeter := ActiveCircuit.EnergyMeters.Active;
                if pMeter <> NIL then
                    pMeter.TakeSample;
            end;
        end;
        5:
        begin  // Meters.Save
            if ActiveCircuit <> NIL then
            begin
                pMeter := ActiveCircuit.EnergyMeters.Active;
                if pMeter <> NIL then
                    pMeter.SaveRegisters;
            end;
        end;
        6:
        begin  // Meters.MeteredTerminal read
  // First make sure active circuit element is a meter
            if ActiveCircuit <> NIL then
            begin
                pMeter := TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
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
            if ActiveCircuit <> NIL then
            begin
                pMeter := TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
                if pMeter <> NIL then
                begin
                    pMeter.MeteredTerminal := arg;
                    pMeter.MeteredElementChanged := TRUE;
                    pMeter.RecalcElementData;
                end;
            end;
        end;
        8:
        begin  // Meters.DIFilesAreOpen
            if ActiveCircuit <> NIL then
            begin
                Result := 0;
                if DIFilesAreOpen then
                    Result := 1;    // Global variable
            end;
            Result := 0;
        end;
        9:
        begin  // Meters.SampleAll
            if ActiveCircuit <> NIL then
            begin
                EnergyMeterClass.SampleAll;
            end;
            Result := 0;
        end;
        10:
        begin  // Meters.SaveAll
            if ActiveCircuit <> NIL then
            begin
                EnergyMeterClass.SaveAll;
            end;
            Result := 0;
        end;
        11:
        begin  // Meters.OpenAllDIFiles
            if ActiveCircuit <> NIL then
            begin
                EnergyMeterClass.OpenAllDIFiles;
            end;
            Result := 0;
        end;
        12:
        begin  // Meters.CloseAllDIFiles
            if ActiveCircuit <> NIL then
            begin
                EnergyMeterClass.CloseAllDIFiles;
            end;
            Result := 0;
        end;
        13:
        begin  // Meters.CountEndElements
            Result := 0;
            if ActiveCircuit <> NIL then
            begin
                pMeter := TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
                if pMeter <> NIL then
                begin
                    Result := pMeter.BranchList.ZoneEndsList.NumEnds;
                end;
            end;
        end;
        14:
        begin  // Meters.Count
            if Assigned(ActiveCircuit) then
                Result := ActiveCircuit.EnergyMeters.ListSize;
        end;
        15:
        begin  // Meters.CountBranches
            Result := 0;
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
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
            if Assigned(ActiveCircuit) then
                with ActiveCircuit do
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
            if Assigned(ActiveCircuit) then
                with ActiveCircuit do
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
            if Assigned(ActiveCircuit) then
                with ActiveCircuit do
                begin
                    pMeter := TEnergyMeterObj(EnergyMeters.Active);
                    if pMeter <> NIL then
                    begin
                        pMeter.CalcReliabilityIndices(AssumeRestoration);

                    end;
                end;
        end;
        19:
        begin  // Meters.SeqListSize
            Result := 0;
            if Assigned(ActiveCircuit) then
                with ActiveCircuit do
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
            if Assigned(ActiveCircuit) then
                with ActiveCircuit do
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
            if Assigned(ActiveCircuit) then
                with ActiveCircuit do
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
            if Assigned(ActiveCircuit) then
                with ActiveCircuit do
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
            if Assigned(ActiveCircuit) then
                with ActiveCircuit do
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
            if Assigned(ActiveCircuit) then
                with ActiveCircuit do
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
            if Assigned(ActiveCircuit) then
                with ActiveCircuit do
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
            if Assigned(ActiveCircuit) then
                with ActiveCircuit do
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
            if Assigned(ActiveCircuit) then
                with ActiveCircuit do
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
            Result := 0.0;
            if Assigned(ActiveCircuit) then
                with ActiveCircuit do
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
            Result := 0.0;
            if Assigned(ActiveCircuit) then
                with ActiveCircuit do
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
            Result := 0.0;
            if Assigned(ActiveCircuit) then
                with ActiveCircuit do
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
            Result := 0.0;
            if Assigned(ActiveCircuit) then
                with ActiveCircuit do
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
            Result := 0.0;
            if Assigned(ActiveCircuit) then
                with ActiveCircuit do
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
            Result := 0.0;
            if Assigned(ActiveCircuit) then
                with ActiveCircuit do
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
            Result := 0.0;
            if Assigned(ActiveCircuit) then
                with ActiveCircuit do
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
function MetersS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

var
    pMeterObj: TEnergyMeterObj;
    activesave: Integer;
    TestStr: String;
    Found: Boolean;

begin
    Result := pAnsiChar(Ansistring('0')); // Default return value
    case mode of
        0:
        begin  // Meters.Name read
            if ActiveCircuit <> NIL then
            begin
                pMeterObj := TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
                if pMeterObj <> NIL then
                    Result := pAnsiChar(Ansistring(pMeterObj.name));
            end;
        end;
        1:
        begin // Meters.Name Write
            if ActiveCircuit <> NIL then
            begin      // Search list of EnergyMeters in active circuit for name
                with ActiveCircuit.EnergyMeters do
                begin
                    TestStr := String(arg);  // Convert to Pascal String for testing
                    Found := FALSE;
                    ActiveSave := ActiveIndex;
                    pMeterObj := First;
                    while pMeterObj <> NIL do
                    begin
                        if (CompareText(pMeterObj.Name, TestStr) = 0) then
                        begin
                            ActiveCircuit.ActiveCktElement := pMeterObj;
                            Found := TRUE;
                            Break;
                        end;
                        pMeterObj := Next;
                    end;
                    if not Found then
                    begin
                        DoSimpleMsg('EnergyMeter "' + TestStr + '" Not Found in Active Circuit.', 5005);
                        pMeterObj := Get(ActiveSave);    // Restore active Meter
                        ActiveCircuit.ActiveCktElement := pMeterObj;
                    end;
                end;
            end;
        end;
        2:
        begin   // Meters.MeteredElement read
  // First make sure active circuit element is a meter
            if ActiveCircuit <> NIL then
            begin
                pMeterObj := TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
                if pMeterObj <> NIL then
                begin
                    Result := pAnsiChar(Ansistring(pMeterObj.ElementName));
                end
                else
                    Result := String(Ansistring(''));
            end
            else
            begin
                Result := String(Ansistring(''));
            end;
        end;
        3:
        begin  // Meters.MeteredElement Write
    // First make sure active circuit element is a meter
            if ActiveCircuit <> NIL then
            begin
                pMeterObj := TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
                if pMeterObj <> NIL then
                begin
                    pMeterObj.elementName := String(arg);
                    pMeterObj.MeteredElementChanged := TRUE;
                    pMeterObj.RecalcElementData;
                end;
            end;
        end
    else
        Result := String(Ansistring('Error, Parameter not recognized'));
    end;
end;

//***************************Variant type properties******************************
procedure MetersV(mode: Longint; out arg: Variant); CDECL;

var
    MeterElem: TEnergyMeterObj;
    k, i: Integer;
    pMeterObj: TEnergyMeterObj;
    last: Integer;
    elem: TDSSCktElement;
    node: TCktTreeNode;
    BranchCount: Integer;
    pElem: TDSSCktElement;

begin
    case mode of
        0:
        begin  // Meters.AllNames
            arg := VarArrayCreate([0, 0], varOleStr);
            arg[0] := 'NONE';
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                    if EnergyMeters.ListSize > 0 then
                    begin
                        VarArrayRedim(arg, EnergyMeters.ListSize - 1);
                        k := 0;
                        MeterElem := EnergyMeters.First;
                        while MeterElem <> NIL do
                        begin
                            arg[k] := MeterElem.Name;
                            Inc(k);
                            MeterElem := EnergyMeters.Next;
                        end;
                    end;
        end;
        1:
        begin  // Meters.RegisterNames
            pMeterObj := TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
            if Assigned(pMeterObj) then
            begin
                arg := VarArrayCreate([0, NumEMRegisters - 1], varOleStr);
                for k := 0 to NumEMRegisters - 1 do
                begin
                    arg[k] := pMeterObj.RegisterNames[k + 1];
                end;
            end
            else
                arg := VarArrayCreate([0, 0], varOleStr); // null array
        end;
        2:
        begin  // Meters.RegisterValues
            if ActiveCircuit <> NIL then
            begin
                pMeterObj := TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
                if pMeterObj <> NIL then
                begin
                    arg := VarArrayCreate([0, numEMRegisters - 1], varDouble);
                    for k := 0 to numEMRegisters - 1 do
                    begin
                        arg[k] := pMeterObj.Registers[k + 1];
                    end;
                end
                else
                    arg := VarArrayCreate([0, 0], varDouble);
            end
            else
            begin
                arg := VarArrayCreate([0, 0], varDouble);
            end;
        end;
        3:
        begin  // Meters.Totals
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    TotalizeMeters;
                    arg := VarArrayCreate([0, NumEMRegisters - 1], varDouble);
                    for i := 1 to NumEMregisters do
                        arg[i - 1] := RegisterTotals[i];
                end
            else
            begin
                arg := VarArrayCreate([0, 0], varDouble);
            end;
        end;
        4:
        begin  // Meters.PeakCurrent read
            if ActiveCircuit <> NIL then
            begin
                pMeterObj := TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
                if pMeterObj <> NIL then
                begin
                    arg := VarArrayCreate([0, pMeterObj.NPhases - 1], varDouble);
                    for k := 0 to pMeterObj.NPhases - 1 do
                        arg[k] := pMeterObj.SensorCurrent^[k + 1];
                end
                else
                    arg := VarArrayCreate([0, 0], varDouble);
            end
            else
            begin
                arg := VarArrayCreate([0, 0], varDouble);
            end;
        end;
        5:
        begin  // Meters.PeakCurrent Write
            if ActiveCircuit <> NIL then
            begin
                pMeterObj := TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
                if pMeterObj <> NIL then
                begin
                    k := VarArrayLowBound(arg, 1);   // get starting index for Value array
                    for i := 1 to pMeterObj.NPhases do
                    begin
                        pMeterObj.SensorCurrent^[i] := arg[k];
                        inc(k);
                    end;
                end;
            end;
        end;
        6:
        begin  // Meter.CalcCurrent read
            if ActiveCircuit <> NIL then
            begin
                pMeterObj := TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
                if pMeterObj <> NIL then
                begin
                    arg := VarArrayCreate([0, pMeterObj.NPhases - 1], varDouble);
                    for k := 0 to pMeterObj.NPhases - 1 do
                        arg[k] := Cabs(pMeterObj.CalculatedCurrent^[k + 1]);
                end
                else
                    arg := VarArrayCreate([0, 0], varDouble);
            end
            else
            begin
                arg := VarArrayCreate([0, 0], varDouble);
            end;
        end;
        7:
        begin  // Meters.CalcCurrent Write
    // First make sure active circuit element is a meter
            if ActiveCircuit <> NIL then
            begin
                pMeterObj := TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
                if pMeterObj <> NIL then
                begin
                    k := VarArrayLowBound(arg, 1);   // get starting index for Value array
                    for i := 1 to pMeterObj.NPhases do
                    begin
                        pMeterObj.CalculatedCurrent^[i] := cmplx(arg[k], 0.0);   // Just set the real part
                        inc(k);
                    end;
                end;
            end;
        end;
        8:
        begin  // Meters.AllocFactors read
    // First make sure active circuit element is a meter
            if ActiveCircuit <> NIL then
            begin
                pMeterObj := TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
                if pMeterObj <> NIL then
                begin
                    arg := VarArrayCreate([0, pMeterObj.NPhases - 1], varDouble);
                    for k := 0 to pMeterObj.NPhases - 1 do
                        arg[k] := pMeterObj.PhsAllocationFactor^[k + 1];
                end
                else
                    arg := VarArrayCreate([0, 0], varDouble);
            end
            else
            begin
                arg := VarArrayCreate([0, 0], varDouble);
            end;
        end;
        9:
        begin   // Meters.AllocFactors Write
    // First make sure active circuit element is a meter
            if ActiveCircuit <> NIL then
            begin
                pMeterObj := TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
                if pMeterObj <> NIL then
                begin
                    k := VarArrayLowBound(arg, 1);   // get starting index for Value array
                    for i := 1 to pMeterObj.NPhases do
                    begin
                        pMeterObj.PhsAllocationFactor^[i] := arg[k];   // Just set the real part
                        inc(k);
                    end;
                end;
            end;
        end;
        10:
        begin  // Meters.AllEndElements
            arg := VarArrayCreate([0, 0], varOleStr);
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    pMeterObj := EnergyMeters.Active;
                    if pMeterObj <> NIL then
                    begin
                        last := pMeterObj.BranchList.ZoneEndsList.NumEnds - 1;
                        VarArrayRedim(arg, last);
                        for k := 0 to last do
                        begin
                            pMeterObj.BranchList.ZoneEndsList.Get(k + 1, node);
                            elem := node.CktObject;
                            arg[k] := Format('%s.%s', [elem.ParentClass.Name, elem.Name]);
                        end;
                    end;
                end;
        end;
        11:
        begin  // Meters.ALlBranchesInZone
            arg := VarArrayCreate([0, 0], varOleStr);
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    pMeterObj := EnergyMeters.Active;
                    if pMeterObj <> NIL then
                    begin
          // Get count of branches
                        BranchCount := MetersI(15, 0);
                        if BranchCount > 0 then
                        begin
                            VarArrayRedim(arg, BranchCount - 1);
                            pElem := pMeterObj.BranchList.First;
                            k := 0;
                            while pElem <> NIL do
                            begin
                                arg[k] := Format('%s.%s', [pElem.ParentClass.Name, pElem.Name]);
                                inc(k);
                                pElem := pMeterObj.BranchList.GoForward;
                            end;
                        end;
                    end;
                end;
        end
    else
        arg[0] := 'Error, Parameter not recognized';
    end;
end;

end.
