unit CAPI_Meters;

interface

uses
    CAPI_Utils;

procedure Meters_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
function Meters_Get_First(): Integer; CDECL;
function Meters_Get_Name(): PAnsiChar; CDECL;
function Meters_Get_Next(): Integer; CDECL;
procedure Meters_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Meters_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Meters_Get_RegisterValues_GR(); CDECL;
procedure Meters_Reset(); CDECL;
procedure Meters_ResetAll(); CDECL;
procedure Meters_Sample(); CDECL;
procedure Meters_Save(); CDECL;
procedure Meters_Set_Name(const Value: PAnsiChar); CDECL;
procedure Meters_Get_Totals(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Meters_Get_Totals_GR(); CDECL;
procedure Meters_Get_Peakcurrent(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Meters_Get_Peakcurrent_GR(); CDECL;
procedure Meters_Set_Peakcurrent(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure Meters_Get_CalcCurrent(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Meters_Get_CalcCurrent_GR(); CDECL;
procedure Meters_Set_CalcCurrent(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure Meters_Get_AllocFactors(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Meters_Get_AllocFactors_GR(); CDECL;
procedure Meters_Set_AllocFactors(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
function Meters_Get_MeteredElement(): PAnsiChar; CDECL;
function Meters_Get_MeteredTerminal(): Integer; CDECL;
procedure Meters_Set_MeteredElement(const Value: PAnsiChar); CDECL;
procedure Meters_Set_MeteredTerminal(Value: Integer); CDECL;
function Meters_Get_DIFilesAreOpen(): Boolean; CDECL;
procedure Meters_CloseAllDIFiles(); CDECL;
procedure Meters_OpenAllDIFiles(); CDECL;
procedure Meters_SampleAll(); CDECL;
procedure Meters_SaveAll(); CDECL;
procedure Meters_Get_AllEndElements(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
function Meters_Get_CountEndElements(): Integer; CDECL;
function Meters_Get_Count(): Integer; CDECL;
procedure Meters_Get_AllBranchesInZone(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
function Meters_Get_CountBranches(): Integer; CDECL;
function Meters_Get_SAIFI(): Double; CDECL;
function Meters_Get_SequenceIndex(): Integer; CDECL;
procedure Meters_Set_SequenceIndex(Value: Integer); CDECL;
function Meters_Get_SAIFIKW(): Double; CDECL;
procedure Meters_DoReliabilityCalc(AssumeRestoration: Boolean); CDECL;
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
procedure Meters_Get_ZonePCE(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;

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
function _activeObj(out obj: TEnergyMeterObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit then
        Exit;
    
    obj := ActiveCircuit.EnergyMeters.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active EnergyMeter object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure InvalidActiveSection(); inline;
begin
    DoSimpleMsg('Invalid active section. Has SetActiveSection been called?', 5055);
end;
//------------------------------------------------------------------------------
procedure Meters_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, ActiveCircuit.EnergyMeters, False);
end;
//------------------------------------------------------------------------------
function Meters_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := Generic_CktElement_Get_First(ActiveCircuit.EnergyMeters);
end;
//------------------------------------------------------------------------------
function Meters_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := Generic_CktElement_Get_Next(ActiveCircuit.EnergyMeters);        
end;
//------------------------------------------------------------------------------
function Meters_Get_Name(): PAnsiChar; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := NIL;
    if not _activeObj(pMeterObj) then
        Exit;
    
    Result := DSS_GetAsPAnsiChar(pMeterObj.name);
end;
//------------------------------------------------------------------------------
procedure Meters_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray;
    pMeterObj: TEnergyMeterObj;
    k: Integer;
begin
    if not _activeObj(pMeterObj) then
    begin
        DefaultResult(ResultPtr, ResultCount, '');
        Exit;
    end;

    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, NumEMRegisters);
    for k := 0 to NumEMRegisters - 1 do
    begin
        Result[k] := DSS_CopyStringAsPChar(pMeterObj.RegisterNames[k + 1]);
    end;
end;
//------------------------------------------------------------------------------
procedure Meters_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    if not _activeObj(pMeterObj) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, numEMRegisters);
    Move(pMeterObj.Registers[1], ResultPtr^, numEMRegisters * SizeOf(Double));
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
    if not _activeObj(pMeter) then
        Exit;
        
    pMeter.ResetRegisters();
end;
//------------------------------------------------------------------------------
procedure Meters_ResetAll(); CDECL;
begin
    if InvalidCircuit then
        Exit;
    EnergyMeterClass.ResetAll;
end;
//------------------------------------------------------------------------------
procedure Meters_Sample(); CDECL;
var
    pMeter: TEnergyMeterObj;
begin
    if not _activeObj(pMeter) then
        Exit;
    
    pMeter.TakeSample();
end;
//------------------------------------------------------------------------------
procedure Meters_Save(); CDECL;
var
    pMeter: TEnergyMeterObj;
begin
    if not _activeObj(pMeter) then
        Exit;

    pMeter.SaveRegisters();
end;
//------------------------------------------------------------------------------
procedure Meters_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit then
        Exit;
    if EnergyMeterClass.SetActive(Value) then
    begin
        ActiveCircuit.ActiveCktElement := EnergyMeterClass.ElementList.Active;
        ActiveCircuit.EnergyMeters.Get(EnergyMeterClass.Active);
    end
    else
    begin
        DoSimpleMsg('EnergyMeter "' + Value + '" Not Found in Active Circuit.', 5005);
    end;
end;
//------------------------------------------------------------------------------
procedure Meters_Get_Totals(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
begin
    if InvalidCircuit then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    
    with ActiveCircuit do
    begin
        TotalizeMeters();
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, NumEMRegisters);
        Move(RegisterTotals[1], ResultPtr^, ResultCount^ * SizeOf(Double));
    end
end;

procedure Meters_Get_Totals_GR(); CDECL;
// Same as Meters_Get_Totals but uses global result (GR) pointers
begin
    Meters_Get_Totals(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Meters_Get_Peakcurrent(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    if not _activeObj(pMeterObj) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, pMeterObj.NPhases);
    Move(pMeterObj.SensorCurrent[1], ResultPtr^, ResultCount^ * SizeOf(Double));
end;

procedure Meters_Get_Peakcurrent_GR(); CDECL;
// Same as Meters_Get_Peakcurrent but uses global result (GR) pointers
begin
    Meters_Get_Peakcurrent(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Meters_Set_Peakcurrent(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    if not _activeObj(pMeterObj) then
        Exit;

    if ValueCount <> pMeterObj.NPhases then
    begin
        DoSimpleMsg('The provided number of values does not match the element''s number of phases.', 5026);
        Exit;
    end;
    Move(ValuePtr^, pMeterObj.SensorCurrent[1], ValueCount * SizeOf(Double));    
end;
//------------------------------------------------------------------------------
procedure Meters_Get_CalcCurrent(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray;
    pMeterObj: TEnergyMeterObj;
    k: Integer;
begin
    if not _activeObj(pMeterObj) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, pMeterObj.NPhases);
    for k := 0 to pMeterObj.NPhases - 1 do
        Result[k] := Cabs(pMeterObj.CalculatedCurrent^[k + 1]);
end;

procedure Meters_Get_CalcCurrent_GR(); CDECL;
// Same as Meters_Get_CalcCurrent but uses global result (GR) pointers
begin
    Meters_Get_CalcCurrent(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Meters_Set_CalcCurrent(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    Value: PDoubleArray;
    pMeterObj: TEnergyMeterObj;
    i: Integer;
begin
    if not _activeObj(pMeterObj) then
        Exit;

    if ValueCount <> pMeterObj.NPhases then
    begin
        DoSimpleMsg('The provided number of values does not match the element''s number of phases.', 5025);
        Exit;
    end;
        
    Value := PDoubleArray(ValuePtr);
    for i := 1 to pMeterObj.NPhases do
        pMeterObj.CalculatedCurrent^[i] := cmplx(Value[i - 1], 0.0);   // Just set the real part
end;
//------------------------------------------------------------------------------
procedure Meters_Get_AllocFactors(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    if not _activeObj(pMeterObj) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, pMeterObj.NPhases);
    Move(pMeterObj.PhsAllocationFactor[1], ResultPtr^, ResultCount^ * SizeOf(Double));
end;

procedure Meters_Get_AllocFactors_GR(); CDECL;
// Same as Meters_Get_AllocFactors but uses global result (GR) pointers
begin
    Meters_Get_AllocFactors(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Meters_Set_AllocFactors(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    Value: PDoubleArray;
    pMeterObj: TEnergyMeterObj;
    i: Integer;
begin
    if not _activeObj(pMeterObj) then
        Exit;

    Value := PDoubleArray(ValuePtr);
    if ValueCount <> pMeterObj.NPhases then
    begin
        DoSimpleMsg('The provided number of values does not match the element''s number of phases.', 5026);
        Exit;
    end;

    for i := 1 to pMeterObj.NPhases do
    begin
        pMeterObj.PhsAllocationFactor^[i] := Value[i - 1];
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_MeteredElement(): PAnsiChar; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := NIL;
    if not _activeObj(pMeterObj) then
        Exit;

    Result := DSS_GetAsPAnsiChar(pMeterObj.ElementName);
end;
//------------------------------------------------------------------------------
function Meters_Get_MeteredTerminal(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if not _activeObj(pMeterObj) then
        Exit;

    Result := pMeterObj.MeteredTerminal;
end;
//------------------------------------------------------------------------------
procedure Meters_Set_MeteredElement(const Value: PAnsiChar); CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    if not _activeObj(pMeterObj) then
        Exit;

    pMeterObj.elementName := Value;
    pMeterObj.MeteredElementChanged := TRUE;
    pMeterObj.RecalcElementData;
end;
//------------------------------------------------------------------------------
procedure Meters_Set_MeteredTerminal(Value: Integer); CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    if not _activeObj(pMeterObj) then
        Exit;

    pMeterObj.MeteredTerminal := Value;
    pMeterObj.MeteredElementChanged := TRUE;
    pMeterObj.RecalcElementData;
end;
//------------------------------------------------------------------------------
function Meters_Get_DIFilesAreOpen(): Boolean; CDECL;
begin
    Result := False;
    if InvalidCircuit then
        Exit;
    Result := DIFilesAreOpen;    // Global variable
end;
//------------------------------------------------------------------------------
procedure Meters_CloseAllDIFiles(); CDECL;
begin
    if InvalidCircuit then
        Exit;
    EnergyMeterClass.CloseAllDIFiles;
end;
//------------------------------------------------------------------------------
procedure Meters_OpenAllDIFiles(); CDECL;
begin
    if InvalidCircuit then
        Exit;
    EnergyMeterClass.OpenAllDIFiles;
end;
//------------------------------------------------------------------------------
procedure Meters_SampleAll(); CDECL;
begin
    if InvalidCircuit then
        Exit;
    EnergyMeterClass.SampleAll;
end;
//------------------------------------------------------------------------------
procedure Meters_SaveAll(); CDECL;
begin
    if InvalidCircuit then
        Exit;
    EnergyMeterClass.SaveAll;
end;
//------------------------------------------------------------------------------
procedure Meters_Get_AllEndElements(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray;
    pMeterObj: TEnergyMeterObj;
    k, num: Integer;
    elem: TDSSCktElement;
    node: TCktTreeNode;
begin
    DefaultResult(ResultPtr, ResultCount, '');
    if not _activeObj(pMeterObj) then
        Exit;

    if not pMeterObj.CheckBranchList(5502) then
        Exit;
    
    if pMeterObj.BranchList.ZoneEndsList = NIL then
        Exit;

    num := pMeterObj.BranchList.ZoneEndsList.NumEnds;
    DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, num);
    for k := 0 to num - 1 do
    begin
        pMeterObj.BranchList.ZoneEndsList.Get(k + 1, node);
        elem := node.CktObject;
        Result[k] := DSS_CopyStringAsPChar(Format('%s.%s', [elem.ParentClass.Name, elem.Name]));
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_CountEndElements(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if not _activeObj(pMeterObj) then
        Exit;
    
    if not pMeterObj.CheckBranchList(5500) then
        Exit;

    if pMeterObj.BranchList.ZoneEndsList = NIL then
        Exit;

    Result := pMeterObj.BranchList.ZoneEndsList.NumEnds;
end;
//------------------------------------------------------------------------------
function Meters_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.EnergyMeters.ListSize;
end;
//------------------------------------------------------------------------------
procedure Meters_Get_AllBranchesInZone(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray;
    pMeterObj: TEnergyMeterObj;
    k: Integer;
    BranchCount: Integer;
    pElem: TDSSCktElement;
begin
    DefaultResult(ResultPtr, ResultCount, '');
    if not _activeObj(pMeterObj) then
        Exit;

    if not pMeterObj.CheckBranchList(5501) then
        Exit;

    // Get count of branches
    BranchCount := Meters_Get_CountBranches;
    if BranchCount <= 0 then 
        Exit;
        
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
//------------------------------------------------------------------------------
function Meters_Get_CountBranches(): Integer; CDECL; //TODO: check -- same as Meters_Get_SeqListSize?
var
    pMeterObj: TEnergyMeterObj;
  // pelem : TDSSCktElement;
begin
    Result := 0;
    if not _activeObj(pMeterObj) then
        Exit;
        
    if pMeterObj.SequenceList = NIL then
        Exit;
        
    Result := pMeterObj.SequenceList.ListSize;
    (*
      while pElem <> Nil do   
      Begin
         inc(Result);
         pElem := pMeterObj.BranchList.GoForward;
      End;
    *)
end;
//------------------------------------------------------------------------------
function Meters_Get_SAIFI(): Double; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0.0;
    if not _activeObj(pMeterObj) then
        Exit;

    Result := pMeterObj.SAIFI;
end;
//------------------------------------------------------------------------------
function Meters_Get_SequenceIndex(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if not _activeObj(pMeterObj) then
        Exit;

    Result := pMeterObj.SequenceList.ActiveIndex;
end;
//------------------------------------------------------------------------------
procedure Meters_Set_SequenceIndex(Value: Integer); CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    if not _activeObj(pMeterObj) then
        Exit;

    with pMeterObj do
    begin
        if (Value > 0) and (Value <= SequenceList.ListSize) then
            ActiveCircuit.ActiveCktElement := SequenceList.Get(Value)
        else
            DoSimpleMsg(Format('Invalid index for SequenceList: %d. List size is %d.', [Value, SequenceList.ListSize]), 500501);
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_SAIFIKW(): Double; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0.0;
    if not _activeObj(pMeterObj) then
        Exit;

    Result := pMeterObj.SAIFIkW;
end;
//------------------------------------------------------------------------------
procedure Meters_DoReliabilityCalc(AssumeRestoration: Boolean); CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    if not _activeObj(pMeterObj) then
        Exit;
        
    pMeterObj.CalcReliabilityIndices(AssumeRestoration);
end;
//------------------------------------------------------------------------------
function Meters_Get_SeqListSize(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if not _activeObj(pMeterObj) then
        Exit;
        
    Result := pMeterObj.SequenceList.ListSize;
end;
//------------------------------------------------------------------------------
function Meters_Get_TotalCustomers(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
    PD_Element: TPDElement;

begin
    Result := 0;
    if not _activeObj(pMeterObj) then
        Exit;

    with ActiveCircuit do
    begin
        if Buses = NIL then 
            Exit;
    
        pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
        if pMeterObj = NIL then
            Exit;
        
        PD_Element := pMeterObj.SequenceList.Get(1);
        if PD_Element = NIL then
            Exit;
            
        with PD_Element do
            Result := Buses^[Terminals[FromTerminal - 1].BusRef].BusTotalNumCustomers;
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_SAIDI(): Double; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0.0;
    if not _activeObj(pMeterObj) then
        Exit;
    
    Result := pMeterObj.SAIDI;
end;
//------------------------------------------------------------------------------
function Meters_Get_CustInterrupts(): Double; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0.0;
    if not _activeObj(pMeterObj) then
        Exit;

    Result := pMeterObj.CustInterrupts;
end;
//------------------------------------------------------------------------------
function Meters_Get_NumSections(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if not _activeObj(pMeterObj) then
        Exit;

    Result := pMeterObj.SectionCount;
end;
//------------------------------------------------------------------------------
procedure Meters_SetActiveSection(SectIdx: Integer); CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    if not _activeObj(pMeterObj) then
        Exit;

    if (SectIdx > 0) and (SectIdx <= pMeterObj.SectionCount) then
        pMeterObj.ActiveSection := SectIdx
    else
        pMeterObj.ActiveSection := 0;
end;
//------------------------------------------------------------------------------
function Meters_Get_AvgRepairTime(): Double; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0.0;
    if not _activeObj(pMeterObj) then
        Exit;

    with pMeterObj do
    begin
        if (ActiveSection > 0) and (ActiveSection <= SectionCount) then
            Result := FeederSections^[ActiveSection].AverageRepairTime
        else
            InvalidActiveSection();
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_FaultRateXRepairHrs(): Double; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0.0;
    if not _activeObj(pMeterObj) then
        Exit;

    with pMeterObj do
    begin
        if (ActiveSection > 0) and (ActiveSection <= SectionCount) then
            Result := FeederSections^[ActiveSection].SumFltRatesXRepairHrs
        else
            InvalidActiveSection();
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_NumSectionBranches(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if not _activeObj(pMeterObj) then
        Exit;

    with pMeterObj do
    begin
        if (ActiveSection > 0) and (ActiveSection <= SectionCount) then
            Result := FeederSections^[ActiveSection].NBranches
        else
            InvalidActiveSection();
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_NumSectionCustomers(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if not _activeObj(pMeterObj) then
        Exit;
        
    with pMeterObj do
    begin
        if (ActiveSection > 0) and (ActiveSection <= SectionCount) then
            Result := FeederSections^[ActiveSection].NCustomers
        else
            InvalidActiveSection();
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_OCPDeviceType(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if not _activeObj(pMeterObj) then
        Exit;
    
    with pMeterObj do
    begin
        if (ActiveSection > 0) and (ActiveSection <= SectionCount) then
            Result := FeederSections^[ActiveSection].OCPDeviceType
        else
            InvalidActiveSection();
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_SumBranchFltRates(): Double; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0.0;
    if not _activeObj(pMeterObj) then
        Exit;
    
    with pMeterObj do
    begin
        if (ActiveSection > 0) and (ActiveSection <= SectionCount) then
            Result := FeederSections^[ActiveSection].SumBranchFltRates
        else
            InvalidActiveSection();
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_SectSeqIdx(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if not _activeObj(pMeterObj) then
        Exit;

    with pMeterObj do
    begin
        if (ActiveSection > 0) and (ActiveSection <= SectionCount) then
            Result := FeederSections^[ActiveSection].SeqIndex
        else
            InvalidActiveSection();
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_SectTotalCust(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if not _activeObj(pMeterObj) then
        Exit;

    with pMeterObj do
    begin
        if (ActiveSection > 0) and (ActiveSection <= SectionCount) then
            Result := FeederSections^[ActiveSection].TotalCustomers
        else
            InvalidActiveSection();
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.EnergyMeters.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Meters_Set_idx(Value: Integer); CDECL;
var
    pEnergyMeter: TEnergyMeterObj;
begin
    if InvalidCircuit then
        Exit;
    pEnergyMeter := ActiveCircuit.EnergyMeters.Get(Value);
    if pEnergyMeter = NIL then
    begin
        DoSimpleMsg('Invalid Meter index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    ActiveCircuit.ActiveCktElement := pEnergyMeter;
end;
//------------------------------------------------------------------------------
procedure Meters_Get_ZonePCE(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
   pMeter: TEnergyMeterObj;
   k: integer;
   Result: PPAnsiCharArray;
begin
    DefaultResult(ResultPtr, ResultCount, '');
    if InvalidCircuit then
        Exit;

    pMeter := ActiveCircuit.EnergyMeters.Active;
    if pMeter = nil then 
        Exit;

    pMeter.GetPCEatZone(True);
    
    if not ((Length(pMeter.ZonePCE) > 0) and (pMeter.ZonePCE[0] <> '')) then
        Exit;
        
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, length(pMeter.ZonePCE));
    for k := 0 to High(pMeter.ZonePCE) do
        Result[k] := DSS_CopyStringAsPChar(pMeter.ZonePCE[k]);
end;
//------------------------------------------------------------------------------
end.
