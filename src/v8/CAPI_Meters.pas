unit CAPI_Meters;

{$inline on}

interface

uses
    CAPI_Utils;

procedure Meters_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure Meters_Get_AllNames_GR(); CDECL;
function Meters_Get_First(): Integer; CDECL;
function Meters_Get_Name(): PAnsiChar; CDECL;
function Meters_Get_Next(): Integer; CDECL;
procedure Meters_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure Meters_Get_RegisterNames_GR(); CDECL;
procedure Meters_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Meters_Get_RegisterValues_GR(); CDECL;
procedure Meters_Reset(); CDECL;
procedure Meters_ResetAll(); CDECL;
procedure Meters_Sample(); CDECL;
procedure Meters_Save(); CDECL;
procedure Meters_Set_Name(const Value: PAnsiChar); CDECL;
procedure Meters_Get_Totals(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Meters_Get_Totals_GR(); CDECL;
procedure Meters_Get_Peakcurrent(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Meters_Get_Peakcurrent_GR(); CDECL;
procedure Meters_Set_Peakcurrent(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure Meters_Get_CalcCurrent(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Meters_Get_CalcCurrent_GR(); CDECL;
procedure Meters_Set_CalcCurrent(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure Meters_Get_AllocFactors(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Meters_Get_AllocFactors_GR(); CDECL;
procedure Meters_Set_AllocFactors(ValuePtr: PDouble; ValueCount: Integer); CDECL;
function Meters_Get_MeteredElement(): PAnsiChar; CDECL;
function Meters_Get_MeteredTerminal(): Integer; CDECL;
procedure Meters_Set_MeteredElement(const Value: PAnsiChar); CDECL;
procedure Meters_Set_MeteredTerminal(Value: Integer); CDECL;
function Meters_Get_DIFilesAreOpen(): Wordbool; CDECL;
procedure Meters_CloseAllDIFiles(); CDECL;
procedure Meters_OpenAllDIFiles(); CDECL;
procedure Meters_SampleAll(); CDECL;
procedure Meters_SaveAll(); CDECL;
procedure Meters_Get_AllEndElements(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure Meters_Get_AllEndElements_GR(); CDECL;
function Meters_Get_CountEndElements(): Integer; CDECL;
function Meters_Get_Count(): Integer; CDECL;
procedure Meters_Get_AllBranchesInZone(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure Meters_Get_AllBranchesInZone_GR(); CDECL;
function Meters_Get_CountBranches(): Integer; CDECL;
function Meters_Get_SAIFI(): Double; CDECL;
function Meters_Get_SequenceIndex(): Integer; CDECL;
procedure Meters_Set_SequenceIndex(Value: Integer); CDECL;
function Meters_Get_SAIFIKW(): Double; CDECL;
procedure Meters_DoReliabilityCalc(AssumeRestoration: Wordbool); CDECL;
function Meters_Get_SeqListSize(): Integer; CDECL;
function Meters_Get_TotalCustomers(): Integer; CDECL;
function Meters_Get_SAIDI(): Double; CDECL;
function Meters_Get_CustInterrupts(): Double; CDECL;
function Meters_Get_NumSections(): Integer; CDECL;
procedure Meters_SetActiveSection(SectIdx: Integer); CDECL;
function Meters_Get_AvgRepairTime(): Double; CDECL;
function Meters_Get_FaultRateXRepairHrs(): Double; CDECL;
function Meters_Get_NumSectionBranches(): Integer; CDECL;
function Meters_Get_NumSectionCustomers(): Integer; CDECL;
function Meters_Get_OCPDeviceType(): Integer; CDECL;
function Meters_Get_SumBranchFltRates(): Double; CDECL;
function Meters_Get_SectSeqIdx(): Integer; CDECL;
function Meters_Get_SectTotalCust(): Integer; CDECL;

// API extensions
function Meters_Get_idx(): Integer; CDECL;
procedure Meters_Set_idx(Value: Integer); CDECL;


implementation

uses
    CAPI_Constants,
    EnergyMeter,
    DSSGlobals,
    SysUtils,
    ucomplex,
    CktElement,
    PDElement,
    CktTree;

//------------------------------------------------------------------------------
procedure InvalidActiveSection(); inline;
begin
    DoSimpleMsg('Invalid active section. Has SetActiveSection been called?', 5055);
end;
//------------------------------------------------------------------------------
procedure Meters_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    MeterElem: TEnergyMeterObj;
    k: Integer;

begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if EnergyMeters.ListSize > 0 then
            begin
                DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (EnergyMeters.ListSize - 1) + 1);
                k := 0;
                MeterElem := EnergyMeters.First;
                while MeterElem <> NIL do
                begin
                    Result[k] := DSS_CopyStringAsPChar(MeterElem.Name);
                    Inc(k);
                    MeterElem := EnergyMeters.Next;
                end;
            end;

end;

procedure Meters_Get_AllNames_GR(); CDECL;
// Same as Meters_Get_AllNames but uses global result (GR) pointers
begin
    Meters_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Meters_Get_First(): Integer; CDECL;
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
//------------------------------------------------------------------------------
function Meters_Get_Name_AnsiString(): Ansistring; inline;
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

function Meters_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Meters_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Meters_Get_Next(): Integer; CDECL;
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
//------------------------------------------------------------------------------
procedure Meters_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    pMeterObj: TEnergyMeterObj;
    k: Integer;

begin
    pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
    if Assigned(pMeterObj) then
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumEMRegisters - 1) + 1);
        for k := 0 to NumEMRegisters - 1 do
        begin
            Result[k] := DSS_CopyStringAsPChar(pMeterObj.RegisterNames[k + 1]);
        end;
    end
    else
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1); // null array
end;

procedure Meters_Get_RegisterNames_GR(); CDECL;
// Same as Meters_Get_RegisterNames but uses global result (GR) pointers
begin
    Meters_Get_RegisterNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
procedure Meters_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    pMeterObj: TEnergyMeterObj;
    k: Integer;
begin

// First make sure active circuit element is a meter
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        if pMeterObj <> NIL then
        begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (numEMRegisters - 1) + 1);
            for k := 0 to numEMRegisters - 1 do
            begin
                Result[k] := pMeterObj.Registers[k + 1];
            end;
        end
        else
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    end
    else
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    end;

end;

procedure Meters_Get_RegisterValues_GR(); CDECL;
// Same as Meters_Get_RegisterValues but uses global result (GR) pointers
begin
    Meters_Get_RegisterValues(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Meters_Reset(); CDECL;
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
//------------------------------------------------------------------------------
procedure Meters_ResetAll(); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        EnergyMeterClass[ActiveActor].ResetAll(ActiveActor);
    end;
end;
//------------------------------------------------------------------------------
procedure Meters_Sample(); CDECL;
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
//------------------------------------------------------------------------------
procedure Meters_Save(); CDECL;
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
//------------------------------------------------------------------------------
procedure Meters_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    if EnergyMeterClass[ActiveActor].SetActive(Value) then
    begin
        ActiveCircuit[ActiveActor].ActiveCktElement := EnergyMeterClass[ActiveActor].ElementList.Active;
        ActiveCircuit[ActiveActor].EnergyMeters.Get(EnergyMeterClass[ActiveActor].Active);
    end
    else
    begin
        DoSimpleMsg('EnergyMeter "' + Value + '" Not Found in Active Circuit.', 5005);
    end;
end;
//------------------------------------------------------------------------------
procedure Meters_Get_Totals(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    i: Integer;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            TotalizeMeters;
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (NumEMRegisters - 1) + 1);
            for i := 1 to NumEMregisters do
                Result[i - 1] := RegisterTotals[i];
        end
    else
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    end;

end;

procedure Meters_Get_Totals_GR(); CDECL;
// Same as Meters_Get_Totals but uses global result (GR) pointers
begin
    Meters_Get_Totals(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Meters_Get_Peakcurrent(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    pMeterObj: TEnergyMeterObj;
    k: Integer;
begin

// First make sure active circuit element is a meter
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        if pMeterObj <> NIL then
        begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (pMeterObj.NPhases - 1) + 1);
            for k := 0 to pMeterObj.NPhases - 1 do
                Result[k] := pMeterObj.SensorCurrent^[k + 1];
        end
        else
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    end
    else
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    end;

end;

procedure Meters_Get_Peakcurrent_GR(); CDECL;
// Same as Meters_Get_Peakcurrent but uses global result (GR) pointers
begin
    Meters_Get_Peakcurrent(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Meters_Set_Peakcurrent(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    pMeterObj: TEnergyMeterObj;
    k, i: Integer;
begin
    Value := PDoubleArray(ValuePtr);
// First make sure active circuit element is a meter
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        if pMeterObj <> NIL then
        begin
            k := (0);   // get starting index for Value array
            for i := 1 to pMeterObj.NPhases do
            begin
                pMeterObj.SensorCurrent^[i] := Value[k];
                inc(k);
            end;
        end;
    end;

end;
//------------------------------------------------------------------------------
procedure Meters_Get_CalcCurrent(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    pMeterObj: TEnergyMeterObj;
    k: Integer;
begin

// First make sure active circuit element is a meter
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        if pMeterObj <> NIL then
        begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (pMeterObj.NPhases - 1) + 1);
            for k := 0 to pMeterObj.NPhases - 1 do
                Result[k] := Cabs(pMeterObj.CalculatedCurrent^[k + 1]);
        end
        else
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    end
    else
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    end;

end;

procedure Meters_Get_CalcCurrent_GR(); CDECL;
// Same as Meters_Get_CalcCurrent but uses global result (GR) pointers
begin
    Meters_Get_CalcCurrent(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Meters_Set_CalcCurrent(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    pMeterObj: TEnergyMeterObj;
    k, i: Integer;
begin
    Value := PDoubleArray(ValuePtr);
// First make sure active circuit element is a meter
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        if pMeterObj <> NIL then
        begin
            k := (0);   // get starting index for Value array
            for i := 1 to pMeterObj.NPhases do
            begin
                pMeterObj.CalculatedCurrent^[i] := cmplx(Value[k], 0.0);   // Just set the real part
                inc(k);
            end;
        end;
    end;

end;
//------------------------------------------------------------------------------
procedure Meters_Get_AllocFactors(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    pMeterObj: TEnergyMeterObj;
    k: Integer;
begin

// First make sure active circuit element is a meter
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        if pMeterObj <> NIL then
        begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (pMeterObj.NPhases - 1) + 1);
            for k := 0 to pMeterObj.NPhases - 1 do
                Result[k] := pMeterObj.PhsAllocationFactor^[k + 1];
        end
        else
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    end
    else
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    end;

end;

procedure Meters_Get_AllocFactors_GR(); CDECL;
// Same as Meters_Get_AllocFactors but uses global result (GR) pointers
begin
    Meters_Get_AllocFactors(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Meters_Set_AllocFactors(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    pMeterObj: TEnergyMeterObj;
    k, i: Integer;
begin
    Value := PDoubleArray(ValuePtr);
// First make sure active circuit element is a meter
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        if pMeterObj <> NIL then
        begin
            k := (0);   // get starting index for Value array
            for i := 1 to pMeterObj.NPhases do
            begin
                pMeterObj.PhsAllocationFactor^[i] := Value[k];   // Just set the real part
                inc(k);
            end;
        end;
    end;

end;
//------------------------------------------------------------------------------
function Meters_Get_MeteredElement_AnsiString(): Ansistring; inline;
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

function Meters_Get_MeteredElement(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Meters_Get_MeteredElement_AnsiString());
end;
//------------------------------------------------------------------------------
function Meters_Get_MeteredTerminal(): Integer; CDECL;
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
//------------------------------------------------------------------------------
procedure Meters_Set_MeteredElement(const Value: PAnsiChar); CDECL;
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
//------------------------------------------------------------------------------
procedure Meters_Set_MeteredTerminal(Value: Integer); CDECL;
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
//------------------------------------------------------------------------------
function Meters_Get_DIFilesAreOpen(): Wordbool; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := DIFilesAreOpen[ActiveActor];    // Global variable
    end;
end;
//------------------------------------------------------------------------------
procedure Meters_CloseAllDIFiles(); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        EnergyMeterClass[ActiveActor].CloseAllDIFiles(ActiveActor);
    end;
end;
//------------------------------------------------------------------------------
procedure Meters_OpenAllDIFiles(); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        EnergyMeterClass[ActiveActor].OpenAllDIFiles(ActiveActor);
    end;
end;
//------------------------------------------------------------------------------
procedure Meters_SampleAll(); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        EnergyMeterClass[ActiveActor].SampleAll(ActiveActor);
    end;
end;
//------------------------------------------------------------------------------
procedure Meters_SaveAll(); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        EnergyMeterClass[ActiveActor].SaveAll(ActiveActor);
    end;
end;
//------------------------------------------------------------------------------
procedure Meters_Get_AllEndElements(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    pMeterObj: TEnergyMeterObj;
    k, last: Integer;
    elem: TDSSCktElement;
    node: TCktTreeNode;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    with ActiveCircuit[ActiveActor] do
    begin
        pMeterObj := EnergyMeters.Active;
        if pMeterObj = NIL then
            Exit;
        if not pMeterObj.CheckBranchList(5502) then
            Exit;
        last := pMeterObj.BranchList.ZoneEndsList.NumEnds - 1;
        DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (last) + 1);
        for k := 0 to last do
        begin
            pMeterObj.BranchList.ZoneEndsList.Get(k + 1, node);
            elem := node.CktObject;
            Result[k] := DSS_CopyStringAsPChar(Format('%s.%s', [elem.ParentClass.Name, elem.Name]));
        end;

    end;
end;

procedure Meters_Get_AllEndElements_GR(); CDECL;
// Same as Meters_Get_AllEndElements but uses global result (GR) pointers
begin
    Meters_Get_AllEndElements(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Meters_Get_CountEndElements(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
    if pMeterObj = NIL then
        Exit;
    if not pMeterObj.CheckBranchList(5500) then
        Exit;
    Result := pMeterObj.BranchList.ZoneEndsList.NumEnds;
end;
//------------------------------------------------------------------------------
function Meters_Get_Count(): Integer; CDECL;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        Result := ActiveCircuit[ActiveActor].EnergyMeters.ListSize;
end;
//------------------------------------------------------------------------------
procedure Meters_Get_AllBranchesInZone(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    pMeterObj: TEnergyMeterObj;
    k: Integer;
    BranchCount: Integer;
    pElem: TDSSCktElement;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;

    with ActiveCircuit[ActiveActor] do
    begin
        pMeterObj := EnergyMeters.Active;
        if pMeterObj = NIL then
            Exit;
    // Get count of branches
        if not pMeterObj.CheckBranchList(5501) then
            Exit;

        BranchCount := Meters_Get_CountBranches;
        if BranchCount > 0 then
        begin
            DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, BranchCount);
            pElem := pMeterObj.BranchList.First;
            k := 0;
            while pElem <> NIL do
            begin
                Result[k] := DSS_CopyStringAsPChar(Format('%s.%s', [pElem.ParentClass.Name, pElem.Name]));
                inc(k);
                pElem := pMeterObj.BranchList.GoForward;
            end;
        end;
    end;

end;

procedure Meters_Get_AllBranchesInZone_GR(); CDECL;
// Same as Meters_Get_AllBranchesInZone but uses global result (GR) pointers
begin
    Meters_Get_AllBranchesInZone(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Meters_Get_CountBranches(): Integer; CDECL;
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
//------------------------------------------------------------------------------
function Meters_Get_SAIFI(): Double; CDECL;
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
//------------------------------------------------------------------------------
function Meters_Get_SequenceIndex(): Integer; CDECL;
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
//------------------------------------------------------------------------------
procedure Meters_Set_SequenceIndex(Value: Integer); CDECL;
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
//------------------------------------------------------------------------------
function Meters_Get_SAIFIKW(): Double; CDECL;
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
//------------------------------------------------------------------------------
procedure Meters_DoReliabilityCalc(AssumeRestoration: Wordbool); CDECL;
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
//------------------------------------------------------------------------------
function Meters_Get_SeqListSize(): Integer; CDECL;
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
//------------------------------------------------------------------------------
function Meters_Get_TotalCustomers(): Integer; CDECL;
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
//------------------------------------------------------------------------------
function Meters_Get_SAIDI(): Double; CDECL;
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
//------------------------------------------------------------------------------
function Meters_Get_CustInterrupts(): Double; CDECL;
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
//------------------------------------------------------------------------------
function Meters_Get_NumSections(): Integer; CDECL;
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
//------------------------------------------------------------------------------
procedure Meters_SetActiveSection(SectIdx: Integer); CDECL;
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
//------------------------------------------------------------------------------
function Meters_Get_AvgRepairTime(): Double; CDECL;
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
                        Result := FeederSections^[ActiveSection].AverageRepairTime
                    else
                        InvalidActiveSection();
                end;
        end;
end;
//------------------------------------------------------------------------------
function Meters_Get_FaultRateXRepairHrs(): Double; CDECL;
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
                        Result := FeederSections^[ActiveSection].SumFltRatesXRepairHrs
                    else
                        InvalidActiveSection();
                end;
        end;
end;
//------------------------------------------------------------------------------
function Meters_Get_NumSectionBranches(): Integer; CDECL;
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
                        Result := FeederSections^[ActiveSection].NBranches
                    else
                        InvalidActiveSection();
                end;
        end;
end;
//------------------------------------------------------------------------------
function Meters_Get_NumSectionCustomers(): Integer; CDECL;
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
                        Result := FeederSections^[ActiveSection].NCustomers
                    else
                        InvalidActiveSection();
                end;
        end;
end;
//------------------------------------------------------------------------------
function Meters_Get_OCPDeviceType(): Integer; CDECL;
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
                        Result := FeederSections^[ActiveSection].OCPDeviceType
                    else
                        InvalidActiveSection();
                end;
        end;
end;
//------------------------------------------------------------------------------
function Meters_Get_SumBranchFltRates(): Double; CDECL;
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
                        Result := FeederSections^[ActiveSection].SumBranchFltRates
                    else
                        InvalidActiveSection();
                end;
        end;

end;
//------------------------------------------------------------------------------
function Meters_Get_SectSeqIdx(): Integer; CDECL;
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
                        Result := FeederSections^[ActiveSection].SeqIndex
                    else
                        InvalidActiveSection();
                end;
        end;

end;
//------------------------------------------------------------------------------
function Meters_Get_SectTotalCust(): Integer; CDECL;
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
                        Result := FeederSections^[ActiveSection].TotalCustomers
                    else
                        InvalidActiveSection();
                end;
        end;


end;
//------------------------------------------------------------------------------
function Meters_Get_idx(): Integer; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].EnergyMeters.ActiveIndex
    else
        Result := 0
end;
//------------------------------------------------------------------------------
procedure Meters_Set_idx(Value: Integer); CDECL;
var
    pEnergyMeter: TEnergyMeterObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    pEnergyMeter := ActiveCircuit[ActiveActor].EnergyMeters.Get(Value);
    if pEnergyMeter = NIL then
    begin
        DoSimpleMsg('Invalid Meter index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    ActiveCircuit[ActiveActor].ActiveCktElement := pEnergyMeter;
end;
//------------------------------------------------------------------------------
end.
