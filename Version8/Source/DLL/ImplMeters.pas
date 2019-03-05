unit ImplMeters;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   1-12-00  Modified first..next to return only enabled Meters
   7/19/01 Added Totals
}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSEngine_TLB,
    StdVcl;

type
    TMeters = class(TAutoObject, IMeters)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_RegisterNames: Olevariant; SAFECALL;
        function Get_RegisterValues: Olevariant; SAFECALL;
        procedure Reset; SAFECALL;
        procedure ResetAll; SAFECALL;
        procedure Sample; SAFECALL;
        procedure Save; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_Totals: Olevariant; SAFECALL;
        function Get_Peakcurrent: Olevariant; SAFECALL;
        procedure Set_Peakcurrent(Value: Olevariant); SAFECALL;
        function Get_CalcCurrent: Olevariant; SAFECALL;
        procedure Set_CalcCurrent(Value: Olevariant); SAFECALL;
        function Get_AllocFactors: Olevariant; SAFECALL;
        procedure Set_AllocFactors(Value: Olevariant); SAFECALL;
        function Get_MeteredElement: Widestring; SAFECALL;
        function Get_MeteredTerminal: Integer; SAFECALL;
        procedure Set_MeteredElement(const Value: Widestring); SAFECALL;
        procedure Set_MeteredTerminal(Value: Integer); SAFECALL;
        function Get_DIFilesAreOpen: Wordbool; SAFECALL;
        procedure CloseAllDIFiles; SAFECALL;
        procedure OpenAllDIFiles; SAFECALL;
        procedure SampleAll; SAFECALL;
        procedure SaveAll; SAFECALL;
        function Get_AllEndElements: Olevariant; SAFECALL;
        function Get_CountEndElements: Integer; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_AllBranchesInZone: Olevariant; SAFECALL;
        function Get_CountBranches: Integer; SAFECALL;
        function Get_SAIFI: Double; SAFECALL;
        function Get_SequenceIndex: Integer; SAFECALL;
        procedure Set_SequenceIndex(Value: Integer); SAFECALL;
        function Get_SAIFIKW: Double; SAFECALL;
        procedure DoReliabilityCalc(AssumeRestoration: Wordbool); SAFECALL;
        function Get_SeqListSize: Integer; SAFECALL;
        function Get_TotalCustomers: Integer; SAFECALL;
        function Get_SAIDI: Double; SAFECALL;
        function Get_CustInterrupts: Double; SAFECALL;
        function Get_NumSections: Integer; SAFECALL;
        procedure SetActiveSection(SectIdx: Integer); SAFECALL;
        function Get_AvgRepairTime: Double; SAFECALL;
        function Get_FaultRateXRepairHrs: Double; SAFECALL;
        function Get_NumSectionBranches: Integer; SAFECALL;
        function Get_NumSectionCustomers: Integer; SAFECALL;
        function Get_OCPDeviceType: Integer; SAFECALL;
        function Get_SumBranchFltRates: Double; SAFECALL;
        function Get_SectSeqIdx: Integer; SAFECALL;
        function Get_SectTotalCust: Integer; SAFECALL;
    { Protected declarations }
    end;

implementation

uses
    ComServ,
    EnergyMeter,
    DSSGlobals,
    SysUtils,
    ucomplex,
    Variants,
    CktElement,
    PDElement,
    CktTree;

function TMeters.Get_AllNames: Olevariant;
var
    MeterElem: TEnergyMeterObj;
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if EnergyMeters.ListSize > 0 then
            begin
                VarArrayRedim(Result, EnergyMeters.ListSize - 1);
                k := 0;
                MeterElem := EnergyMeters.First;
                while MeterElem <> NIL do
                begin
                    Result[k] := MeterElem.Name;
                    Inc(k);
                    MeterElem := EnergyMeters.Next;
                end;
            end;

end;

function TMeters.Get_First: Integer;
var
    pMeter: TEnergyMeterObj;

begin

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

function TMeters.Get_Name: Widestring;
var
    pMeterObj: TEnergyMeterObj;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        if pMeterObj <> NIL then
            Result := pMeterObj.name;
    end;
end;

function TMeters.Get_Next: Integer;

var
    pMeterObj: TEnergyMeterObj;

begin

    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := ActiveCircuit[ActiveActor].EnergyMeters.next;
        if pMeterObj <> NIL then
        begin
            repeat   // Find an Enabled Meter
                if pMeterObj.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := pMeterObj;
                    Result := ActiveCircuit[ActiveActor].EnergyMeters.ActiveIndex;
                end
                else
                    pMeterObj := ActiveCircuit[ActiveActor].EnergyMeters.next;
            until (Result > 0) or (pMeterObj = NIL);
        end
        else
            Result := 0;  // signify no more
    end;

end;

function TMeters.Get_RegisterNames: Olevariant;

var
    pMeterObj: TEnergyMeterObj;
    k: Integer;

begin
    pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
    if Assigned(pMeterObj) then
    begin
        Result := VarArrayCreate([0, NumEMRegisters - 1], varOleStr);
        for k := 0 to NumEMRegisters - 1 do
        begin
            Result[k] := pMeterObj.RegisterNames[k + 1];
        end;
    end
    else
        Result := VarArrayCreate([0, 0], varOleStr); // null array
end;

function TMeters.Get_RegisterValues: Olevariant;

var
    pMeterObj: TEnergyMeterObj;
    k: Integer;
begin

// First make sure active circuit element is a meter
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        if pMeterObj <> NIL then
        begin
            Result := VarArrayCreate([0, numEMRegisters - 1], varDouble);
            for k := 0 to numEMRegisters - 1 do
            begin
                Result[k] := pMeterObj.Registers[k + 1];
            end;
        end
        else
            Result := VarArrayCreate([0, 0], varDouble);
    end
    else
    begin
        Result := VarArrayCreate([0, 0], varDouble);
    end;

end;

procedure TMeters.Reset;
var
    pMeter: TEnergyMeterObj;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeter := ActiveCircuit[ActiveActor].EnergyMeters.Active;
        if pMeter <> NIL then
            pMeter.ResetRegisters;
    end;

end;

procedure TMeters.ResetAll;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        EnergyMeterClass[ActiveActor].ResetAll(ActiveActor);
    end;
end;

procedure TMeters.Sample;

var
    pMeter: TEnergyMeterObj;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeter := ActiveCircuit[ActiveActor].EnergyMeters.Active;
        if pMeter <> NIL then
            pMeter.TakeSample(ActiveActor);
    end;

end;

procedure TMeters.Save;

var
    pMeter: TEnergyMeterObj;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeter := ActiveCircuit[ActiveActor].EnergyMeters.Active;
        if pMeter <> NIL then
            pMeter.SaveRegisters(ActiveActor);
    end;

end;

procedure TMeters.Set_Name(const Value: Widestring);
var
    activesave: Integer;
    pMeterObj: TEnergyMeterObj;
    TestStr: String;
    Found: Boolean;
begin


    if ActiveCircuit[ActiveActor] <> NIL then
    begin      // Search list of EnergyMeters in active circuit for name
        with ActiveCircuit[ActiveActor].EnergyMeters do
        begin
            TestStr := Value;  // Convert to Pascal String for testing
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

function TMeters.Get_Totals: Olevariant;
var
    i: Integer;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            TotalizeMeters;
            Result := VarArrayCreate([0, NumEMRegisters - 1], varDouble);
            for i := 1 to NumEMregisters do
                Result[i - 1] := RegisterTotals[i];
        end
    else
    begin
        Result := VarArrayCreate([0, 0], varDouble);
    end;

end;

function TMeters.Get_Peakcurrent: Olevariant;
var
    pMeterObj: TEnergyMeterObj;
    k: Integer;
begin

// First make sure active circuit element is a meter
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        if pMeterObj <> NIL then
        begin
            Result := VarArrayCreate([0, pMeterObj.NPhases - 1], varDouble);
            for k := 0 to pMeterObj.NPhases - 1 do
                Result[k] := pMeterObj.SensorCurrent^[k + 1];
        end
        else
            Result := VarArrayCreate([0, 0], varDouble);
    end
    else
    begin
        Result := VarArrayCreate([0, 0], varDouble);
    end;

end;

procedure TMeters.Set_Peakcurrent(Value: Olevariant);
var
    pMeterObj: TEnergyMeterObj;
    k, i: Integer;
begin

// First make sure active circuit element is a meter
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        if pMeterObj <> NIL then
        begin
            k := VarArrayLowBound(Value, 1);   // get starting index for Value array
            for i := 1 to pMeterObj.NPhases do
            begin
                pMeterObj.SensorCurrent^[i] := Value[k];
                inc(k);
            end;
        end;
    end;

end;

function TMeters.Get_CalcCurrent: Olevariant;
var
    pMeterObj: TEnergyMeterObj;
    k: Integer;
begin

// First make sure active circuit element is a meter
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        if pMeterObj <> NIL then
        begin
            Result := VarArrayCreate([0, pMeterObj.NPhases - 1], varDouble);
            for k := 0 to pMeterObj.NPhases - 1 do
                Result[k] := Cabs(pMeterObj.CalculatedCurrent^[k + 1]);
        end
        else
            Result := VarArrayCreate([0, 0], varDouble);
    end
    else
    begin
        Result := VarArrayCreate([0, 0], varDouble);
    end;

end;

procedure TMeters.Set_CalcCurrent(Value: Olevariant);
var
    pMeterObj: TEnergyMeterObj;
    k, i: Integer;
begin

// First make sure active circuit element is a meter
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        if pMeterObj <> NIL then
        begin
            k := VarArrayLowBound(Value, 1);   // get starting index for Value array
            for i := 1 to pMeterObj.NPhases do
            begin
                pMeterObj.CalculatedCurrent^[i] := cmplx(Value[k], 0.0);   // Just set the real part
                inc(k);
            end;
        end;
    end;

end;

function TMeters.Get_AllocFactors: Olevariant;
var
    pMeterObj: TEnergyMeterObj;
    k: Integer;
begin

// First make sure active circuit element is a meter
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        if pMeterObj <> NIL then
        begin
            Result := VarArrayCreate([0, pMeterObj.NPhases - 1], varDouble);
            for k := 0 to pMeterObj.NPhases - 1 do
                Result[k] := pMeterObj.PhsAllocationFactor^[k + 1];
        end
        else
            Result := VarArrayCreate([0, 0], varDouble);
    end
    else
    begin
        Result := VarArrayCreate([0, 0], varDouble);
    end;

end;

procedure TMeters.Set_AllocFactors(Value: Olevariant);
var
    pMeterObj: TEnergyMeterObj;
    k, i: Integer;
begin

// First make sure active circuit element is a meter
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        if pMeterObj <> NIL then
        begin
            k := VarArrayLowBound(Value, 1);   // get starting index for Value array
            for i := 1 to pMeterObj.NPhases do
            begin
                pMeterObj.PhsAllocationFactor^[i] := Value[k];   // Just set the real part
                inc(k);
            end;
        end;
    end;

end;

function TMeters.Get_MeteredElement: Widestring;
var
    pMeterObj: TEnergyMeterObj;
begin

// First make sure active circuit element is a meter
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        if pMeterObj <> NIL then
        begin
            Result := pMeterObj.ElementName;
        end
        else
            Result := '';
    end
    else
    begin
        Result := '';
    end;

end;

function TMeters.Get_MeteredTerminal: Integer;
var
    pMeterObj: TEnergyMeterObj;
begin

// First make sure active circuit element is a meter
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        if pMeterObj <> NIL then
        begin
            Result := pMeterObj.MeteredTerminal;
        end
        else
            Result := 0;
    end
    else
    begin
        Result := 0;
    end;

end;

procedure TMeters.Set_MeteredElement(const Value: Widestring);
var
    pMeterObj: TEnergyMeterObj;
begin

// First make sure active circuit element is a meter
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        if pMeterObj <> NIL then
        begin
            pMeterObj.elementName := Value;
            pMeterObj.MeteredElementChanged := TRUE;
            pMeterObj.RecalcElementData(ActiveActor);
        end;
    end;

end;

procedure TMeters.Set_MeteredTerminal(Value: Integer);
var
    pMeterObj: TEnergyMeterObj;
begin

// First make sure active circuit element is a meter
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        if pMeterObj <> NIL then
        begin
            pMeterObj.MeteredTerminal := Value;
            pMeterObj.MeteredElementChanged := TRUE;
            pMeterObj.RecalcElementData(ActiveActor);
        end;
    end;

end;

function TMeters.Get_DIFilesAreOpen: Wordbool;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := DIFilesAreOpen[ActiveActor];    // Global variable
    end;
end;

procedure TMeters.CloseAllDIFiles;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        EnergyMeterClass[ActiveActor].CloseAllDIFiles(ActiveActor);
    end;
end;

procedure TMeters.OpenAllDIFiles;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        EnergyMeterClass[ActiveActor].OpenAllDIFiles(ActiveActor);
    end;
end;

procedure TMeters.SampleAll;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        EnergyMeterClass[ActiveActor].SampleAll(ActiveActor);
    end;
end;

procedure TMeters.SaveAll;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        EnergyMeterClass[ActiveActor].SaveAll(ActiveActor);
    end;
end;

function TMeters.Get_AllEndElements: Olevariant;
var
    pMeterObj: TEnergyMeterObj;
    k, last: Integer;
    elem: TDSSCktElement;
    node: TCktTreeNode;
begin
    Result := VarArrayCreate([0, 0], varOleStr);
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            pMeterObj := EnergyMeters.Active;
            if pMeterObj <> NIL then
            begin
                last := pMeterObj.BranchList.ZoneEndsList.NumEnds - 1;
                VarArrayRedim(Result, last);
                for k := 0 to last do
                begin
                    pMeterObj.BranchList.ZoneEndsList.Get(k + 1, node);
                    elem := node.CktObject;
                    Result[k] := Format('%s.%s', [elem.ParentClass.Name, elem.Name]);
                end;
            end;
        end;
end;

function TMeters.Get_CountEndElements: Integer;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        if pMeterObj <> NIL then
        begin
            Result := pMeterObj.BranchList.ZoneEndsList.NumEnds;
        end;
    end;
end;

function TMeters.Get_Count: Integer;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        Result := ActiveCircuit[ActiveActor].EnergyMeters.ListSize;
end;

function TMeters.Get_AllBranchesInZone: Olevariant;
var
    pMeterObj: TEnergyMeterObj;
    k: Integer;
    BranchCount: Integer;
    pElem: TDSSCktElement;
begin
    Result := VarArrayCreate([0, 0], varOleStr);
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            pMeterObj := EnergyMeters.Active;
            if pMeterObj <> NIL then
            begin
      // Get count of branches
                BranchCount := Get_CountBranches;
                if BranchCount > 0 then
                begin
                    VarArrayRedim(Result, BranchCount - 1);
                    pElem := pMeterObj.BranchList.First;
                    k := 0;
                    while pElem <> NIL do
                    begin
                        Result[k] := Format('%s.%s', [pElem.ParentClass.Name, pElem.Name]);
                        inc(k);
                        pElem := pMeterObj.BranchList.GoForward;
                    end;
                end;
            end;
        end;

end;

function TMeters.Get_CountBranches: Integer;

var
    pMeterObj: TEnergyMeterObj;
  // pelem : TDSSCktElement;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            pMeterObj := EnergyMeters.Active;
            if pMeterObj <> NIL then
                Result := pMeterObj.SequenceList.ListSize;

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

function TMeters.Get_SAIFI: Double;
var
    pMeterObj: TEnergyMeterObj;

begin
    Result := 0.0;
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

function TMeters.Get_SequenceIndex: Integer;

var
    pMeterObj: TEnergyMeterObj;

begin
    Result := 0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
            if pMeterObj <> NIL then
            begin
                Result := pMeterObj.SequenceList.ActiveIndex;
            end;
        end;
end;

procedure TMeters.Set_SequenceIndex(Value: Integer);

var
    pMeterObj: TEnergyMeterObj;

begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
            if pMeterObj <> NIL then
                with pMeterObj do
                begin
                    if (Value > 0) and (Value <= SequenceList.ListSize) then
                        ActiveCktElement := SequenceList.Get(Value)
                    else
                        DoSimpleMsg(Format('Invalid index for SequenceList: %d. List size is %d.', [Value, SequenceList.ListSize]), 500501);
                end;
        end;
end;

function TMeters.Get_SAIFIKW: Double;
var
    pMeterObj: TEnergyMeterObj;

begin
    Result := 0.0;
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


procedure TMeters.DoReliabilityCalc(AssumeRestoration: Wordbool);
var
    pMeterObj: TEnergyMeterObj;

begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
            if pMeterObj <> NIL then
            begin

                pMeterObj.CalcReliabilityIndices(AssumeRestoration, ActiveActor);

            end;
        end;
end;

function TMeters.Get_SeqListSize: Integer;
var
    pMeterObj: TEnergyMeterObj;

begin
    Result := 0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
            if pMeterObj <> NIL then
            begin
                Result := pMeterObj.SequenceList.ListSize;
            end;
        end;
end;

function TMeters.Get_TotalCustomers: Integer;
var
    pMeterObj: TEnergyMeterObj;
    PD_Element: TPDElement;

begin
    Result := 0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
            if pMeterObj <> NIL then
            begin
                PD_Element := pMeterObj.SequenceList.Get(1);
                if Assigned(PD_Element) then
                    with PD_Element do
                        Result := Buses^[Terminals^[FromTerminal].BusRef].BusTotalNumCustomers;
            end;
        end;
end;

function TMeters.Get_SAIDI: Double;
var
    pMeterObj: TEnergyMeterObj;

begin
    Result := 0.0;
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

function TMeters.Get_CustInterrupts: Double;
var
    pMeterObj: TEnergyMeterObj;

begin
    Result := 0.0;
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

function TMeters.Get_NumSections: Integer;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
            if pMeterObj <> NIL then
            begin
                Result := pMeterObj.SectionCount;
            end;
        end;
end;

procedure TMeters.SetActiveSection(SectIdx: Integer);
var
    pMeterObj: TEnergyMeterObj;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
            if pMeterObj <> NIL then
            begin
                if (SectIdx > 0) and (SectIdx <= pMeterObj.SectionCount) then
                    pMeterObj.ActiveSection := SectIdx
                else
                    pMeterObj.ActiveSection := 0;
            end;
        end;

end;

function TMeters.Get_AvgRepairTime: Double;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0.0;
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

function TMeters.Get_FaultRateXRepairHrs: Double;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0.0;
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

function TMeters.Get_NumSectionBranches: Integer;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
            if pMeterObj <> NIL then
                with pMeterObj do
                begin
                    if ActiveSection > 0 then
                        Result := FeederSections^[ActiveSection].NBranches;
                end;
        end;
end;

function TMeters.Get_NumSectionCustomers: Integer;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
            if pMeterObj <> NIL then
                with pMeterObj do
                begin
                    if ActiveSection > 0 then
                        Result := FeederSections^[ActiveSection].NCustomers;
                end;
        end;
end;

function TMeters.Get_OCPDeviceType: Integer;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
            if pMeterObj <> NIL then
                with pMeterObj do
                begin
                    if ActiveSection > 0 then
                        Result := FeederSections^[ActiveSection].OCPDeviceType;
                end;
        end;
end;

function TMeters.Get_SumBranchFltRates: Double;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0.0;
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

end;

function TMeters.Get_SectSeqIdx: Integer;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
            if pMeterObj <> NIL then
                with pMeterObj do
                begin
                    if ActiveSection > 0 then
                        Result := FeederSections^[ActiveSection].SeqIndex;
                end;
        end;

end;

function TMeters.Get_SectTotalCust: Integer;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
            if pMeterObj <> NIL then
                with pMeterObj do
                begin
                    if ActiveSection > 0 then
                        Result := FeederSections^[ActiveSection].TotalCustomers;
                end;
        end;


end;

initialization
    TAutoObjectFactory.Create(ComServer, TMeters, Class_Meters,
        ciInternal, tmApartment);
end.
