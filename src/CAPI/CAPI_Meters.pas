unit CAPI_Meters;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure Meters_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Meters_Get_AllNames_GR(); CDECL;
function Meters_Get_First(): Integer; CDECL;
function Meters_Get_Name(): PAnsiChar; CDECL;
function Meters_Get_Next(): Integer; CDECL;
procedure Meters_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Meters_Get_RegisterNames_GR(); CDECL;
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
function Meters_Get_DIFilesAreOpen(): TAPIBoolean; CDECL;
procedure Meters_CloseAllDIFiles(); CDECL;
procedure Meters_OpenAllDIFiles(); CDECL;
procedure Meters_SampleAll(); CDECL;
procedure Meters_SaveAll(); CDECL;
procedure Meters_Get_AllEndElements(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Meters_Get_AllEndElements_GR(); CDECL;
function Meters_Get_CountEndElements(): Integer; CDECL;
function Meters_Get_Count(): Integer; CDECL;
procedure Meters_Get_AllBranchesInZone(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Meters_Get_AllBranchesInZone_GR(); CDECL;
function Meters_Get_CountBranches(): Integer; CDECL;
function Meters_Get_SAIFI(): Double; CDECL;
function Meters_Get_SequenceIndex(): Integer; CDECL;
procedure Meters_Set_SequenceIndex(Value: Integer); CDECL;
function Meters_Get_SAIFIKW(): Double; CDECL;
procedure Meters_DoReliabilityCalc(AssumeRestoration: TAPIBoolean); CDECL;
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
    UComplex, DSSUcomplex,
    CktElement,
    PDElement,
    CktTree,
    DSSClass,
    DSSHelper,
    DSSObjectHelper;

//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TEnergyMeterObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.ActiveCircuit.EnergyMeters.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['EnergyMeter'], 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure InvalidActiveSection(DSS: TDSSContext); inline;
begin
    DoSimpleMsg(DSS, _('Invalid active section. Has SetActiveSection been called?'), 5055);
end;
//------------------------------------------------------------------------------
procedure Meters_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.EnergyMeters, False);
end;

procedure Meters_Get_AllNames_GR(); CDECL;
// Same as Meters_Get_AllNames but uses global result (GR) pointers
begin
    Meters_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function Meters_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime, DSSPrime.ActiveCircuit.EnergyMeters);
end;
//------------------------------------------------------------------------------
function Meters_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime, DSSPrime.ActiveCircuit.EnergyMeters);        
end;
//------------------------------------------------------------------------------
function Meters_Get_Name(): PAnsiChar; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;
    
    Result := DSS_GetAsPAnsiChar(DSSPrime, pMeterObj.name);
end;
//------------------------------------------------------------------------------
procedure Meters_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray0;
    pMeterObj: TEnergyMeterObj;
    k: Integer;
begin
    if not _activeObj(DSSPrime, pMeterObj) then
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

procedure Meters_Get_RegisterNames_GR(); CDECL;
// Same as Meters_Get_RegisterNames but uses global result (GR) pointers
begin
    Meters_Get_RegisterNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
procedure Meters_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    if not _activeObj(DSSPrime, pMeterObj) then
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
    Meters_Get_RegisterValues(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Meters_Reset(); CDECL;
var
    pMeter: TEnergyMeterObj;
begin
    if not _activeObj(DSSPrime, pMeter) then
        Exit;
        
    pMeter.ResetRegisters();
end;
//------------------------------------------------------------------------------
procedure Meters_ResetAll(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.EnergyMeterClass.ResetAll;
end;
//------------------------------------------------------------------------------
procedure Meters_Sample(); CDECL;
var
    pMeter: TEnergyMeterObj;
begin
    if not _activeObj(DSSPrime, pMeter) then
        Exit;
    
    pMeter.TakeSample();
end;
//------------------------------------------------------------------------------
procedure Meters_Save(); CDECL;
var
    pMeter: TEnergyMeterObj;
begin
    if not _activeObj(DSSPrime, pMeter) then
        Exit;

    pMeter.SaveRegisters();
end;
//------------------------------------------------------------------------------
procedure Meters_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    if DSSPrime.EnergyMeterClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.EnergyMeterClass.ElementList.Active;
        DSSPrime.ActiveCircuit.EnergyMeters.Get(DSSPrime.EnergyMeterClass.Active);
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'EnergyMeter "%s" not found in Active Circuit.', [Value], 5005);
    end;
end;
//------------------------------------------------------------------------------
procedure Meters_Get_Totals(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    
    with DSSPrime.ActiveCircuit do
    begin
        TotalizeMeters();
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, NumEMRegisters);
        Move(RegisterTotals[1], ResultPtr^, ResultCount^ * SizeOf(Double));
    end
end;

procedure Meters_Get_Totals_GR(); CDECL;
// Same as Meters_Get_Totals but uses global result (GR) pointers
begin
    Meters_Get_Totals(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Meters_Get_Peakcurrent(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    if not _activeObj(DSSPrime, pMeterObj) then
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
    Meters_Get_Peakcurrent(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Meters_Set_Peakcurrent(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;

    if ValueCount <> pMeterObj.NPhases then
    begin
        DoSimpleMsg(DSSPrime, _('The provided number of values does not match the element''s number of phases.'), 5026);
        Exit;
    end;
    Move(ValuePtr^, pMeterObj.SensorCurrent[1], ValueCount * SizeOf(Double));    
end;
//------------------------------------------------------------------------------
procedure Meters_Get_CalcCurrent(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    pMeterObj: TEnergyMeterObj;
    k: Integer;
begin
    if not _activeObj(DSSPrime, pMeterObj) then
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
    Meters_Get_CalcCurrent(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Meters_Set_CalcCurrent(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    Value: PDoubleArray0;
    pMeterObj: TEnergyMeterObj;
    i: Integer;
begin
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;

    if ValueCount <> pMeterObj.NPhases then
    begin
        DoSimpleMsg(DSSPrime, _('The provided number of values does not match the element''s number of phases.'), 5025);
        Exit;
    end;
        
    Value := PDoubleArray0(ValuePtr);
    for i := 1 to pMeterObj.NPhases do
        pMeterObj.CalculatedCurrent^[i] := cmplx(Value[i - 1], 0.0);   // Just set the real part
end;
//------------------------------------------------------------------------------
procedure Meters_Get_AllocFactors(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    if not _activeObj(DSSPrime, pMeterObj) then
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
    Meters_Get_AllocFactors(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Meters_Set_AllocFactors(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    Value: PDoubleArray0;
    pMeterObj: TEnergyMeterObj;
    i: Integer;
begin
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;

    Value := PDoubleArray0(ValuePtr);
    if ValueCount <> pMeterObj.NPhases then
    begin
        DoSimpleMsg(DSSPrime, _('The provided number of values does not match the element''s number of phases.'), 5026);
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
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;

    if pMeterObj.MeteredElement <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, AnsiLowerCase(pMeterObj.MeteredElement.FullName));
end;
//------------------------------------------------------------------------------
function Meters_Get_MeteredTerminal(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;

    Result := pMeterObj.MeteredTerminal;
end;
//------------------------------------------------------------------------------
procedure Meters_Set_MeteredElement(const Value: PAnsiChar); CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;

    pMeterObj.ParsePropertyValue(ord(TEnergyMeterProp.element), Value);
    pMeterObj.MeteredElementChanged := TRUE;
    pMeterObj.RecalcElementData;
end;
//------------------------------------------------------------------------------
procedure Meters_Set_MeteredTerminal(Value: Integer); CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;

    pMeterObj.MeteredTerminal := Value;
    pMeterObj.MeteredElementChanged := TRUE;
    pMeterObj.RecalcElementData;
end;
//------------------------------------------------------------------------------
function Meters_Get_DIFilesAreOpen(): TAPIBoolean; CDECL;
begin
    Result := False;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.DIFilesAreOpen;    // Global variable
end;
//------------------------------------------------------------------------------
procedure Meters_CloseAllDIFiles(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.EnergyMeterClass.CloseAllDIFiles;
end;
//------------------------------------------------------------------------------
procedure Meters_OpenAllDIFiles(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.EnergyMeterClass.OpenAllDIFiles;
end;
//------------------------------------------------------------------------------
procedure Meters_SampleAll(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.EnergyMeterClass.SampleAll;
end;
//------------------------------------------------------------------------------
procedure Meters_SaveAll(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.EnergyMeterClass.SaveAll;
end;
//------------------------------------------------------------------------------
procedure Meters_Get_AllEndElements(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray0;
    pMeterObj: TEnergyMeterObj;
    k, num: Integer;
    elem: TDSSCktElement;
    node: TCktTreeNode;
begin
    DefaultResult(ResultPtr, ResultCount, '');
    if not _activeObj(DSSPrime, pMeterObj) then
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
        Result[k] := DSS_CopyStringAsPChar(elem.FullName);
    end;
end;

procedure Meters_Get_AllEndElements_GR(); CDECL;
// Same as Meters_Get_AllEndElements but uses global result (GR) pointers
begin
    Meters_Get_AllEndElements(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function Meters_Get_CountEndElements(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pMeterObj) then
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
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.EnergyMeters.Count;
end;
//------------------------------------------------------------------------------
procedure Meters_Get_AllBranchesInZone(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray0;
    pMeterObj: TEnergyMeterObj;
    k: Integer;
    BranchCount: Integer;
    pElem: TDSSCktElement;
begin
    DefaultResult(ResultPtr, ResultCount, '');
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;

    if not pMeterObj.CheckBranchList(5501) then
        Exit;

    // Get count of branches
    BranchCount := Meters_Get_CountBranches();
    if BranchCount <= 0 then 
        Exit;
        
    DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, BranchCount);
    pElem := pMeterObj.BranchList.First;
    k := 0;
    while pElem <> NIL do
    begin
        Result[k] := DSS_CopyStringAsPChar(pElem.FullName);
        inc(k);
        pElem := pMeterObj.BranchList.GoForward;
    end;
end;

procedure Meters_Get_AllBranchesInZone_GR(); CDECL;
// Same as Meters_Get_AllBranchesInZone but uses global result (GR) pointers
begin
    Meters_Get_AllBranchesInZone(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function Meters_Get_CountBranches(): Integer; CDECL; //TODO: check -- same as Meters_Get_SeqListSize?
var
    pMeterObj: TEnergyMeterObj;
  // pelem : TDSSCktElement;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;
        
    if pMeterObj.SequenceList = NIL then
        Exit;
        
    Result := pMeterObj.SequenceList.Count;
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
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;

    Result := pMeterObj.SAIFI;
end;
//------------------------------------------------------------------------------
function Meters_Get_SequenceIndex(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;

    Result := pMeterObj.SequenceList.ActiveIndex;
end;
//------------------------------------------------------------------------------
procedure Meters_Set_SequenceIndex(Value: Integer); CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;

    with pMeterObj do
    begin
        if (Value > 0) and (Value <= SequenceList.Count) then
            DSSPrime.ActiveCircuit.ActiveCktElement := SequenceList.Get(Value)
        else
            DoSimpleMsg('Invalid index for SequenceList: %d. List size is %d.', [Value, SequenceList.Count], 500501);
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_SAIFIKW(): Double; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;

    Result := pMeterObj.SAIFIkW;
end;
//------------------------------------------------------------------------------
procedure Meters_DoReliabilityCalc(AssumeRestoration: TAPIBoolean); CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;
        
    pMeterObj.CalcReliabilityIndices(AssumeRestoration);
end;
//------------------------------------------------------------------------------
function Meters_Get_SeqListSize(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;
        
    Result := pMeterObj.SequenceList.Count;
end;
//------------------------------------------------------------------------------
function Meters_Get_TotalCustomers(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
    PD_Element: TPDElement;

begin
    Result := 0;
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;

    with DSSPrime.ActiveCircuit do
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
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;
    
    Result := pMeterObj.SAIDI;
end;
//------------------------------------------------------------------------------
function Meters_Get_CustInterrupts(): Double; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;

    Result := pMeterObj.CustInterrupts;
end;
//------------------------------------------------------------------------------
function Meters_Get_NumSections(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;

    Result := pMeterObj.SectionCount;
end;
//------------------------------------------------------------------------------
procedure Meters_SetActiveSection(SectIdx: Integer); CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    if not _activeObj(DSSPrime, pMeterObj) then
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
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;

    with pMeterObj do
    begin
        if (ActiveSection > 0) and (ActiveSection <= SectionCount) then
            Result := FeederSections^[ActiveSection].AverageRepairTime
        else
            InvalidActiveSection(DSSPrime);
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_FaultRateXRepairHrs(): Double; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;

    with pMeterObj do
    begin
        if (ActiveSection > 0) and (ActiveSection <= SectionCount) then
            Result := FeederSections^[ActiveSection].SumFltRatesXRepairHrs
        else
            InvalidActiveSection(DSSPrime);
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_NumSectionBranches(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;

    with pMeterObj do
    begin
        if (ActiveSection > 0) and (ActiveSection <= SectionCount) then
            Result := FeederSections^[ActiveSection].NBranches
        else
            InvalidActiveSection(DSSPrime);
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_NumSectionCustomers(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;
        
    with pMeterObj do
    begin
        if (ActiveSection > 0) and (ActiveSection <= SectionCount) then
            Result := FeederSections^[ActiveSection].NCustomers
        else
            InvalidActiveSection(DSSPrime);
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_OCPDeviceType(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;
    
    with pMeterObj do
    begin
        if (ActiveSection > 0) and (ActiveSection <= SectionCount) then
            Result := FeederSections^[ActiveSection].OCPDeviceType
        else
            InvalidActiveSection(DSSPrime);
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_SumBranchFltRates(): Double; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;
    
    with pMeterObj do
    begin
        if (ActiveSection > 0) and (ActiveSection <= SectionCount) then
            Result := FeederSections^[ActiveSection].SumBranchFltRates
        else
            InvalidActiveSection(DSSPrime);
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_SectSeqIdx(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;

    with pMeterObj do
    begin
        if (ActiveSection > 0) and (ActiveSection <= SectionCount) then
            Result := FeederSections^[ActiveSection].SeqIndex
        else
            InvalidActiveSection(DSSPrime);
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_SectTotalCust(): Integer; CDECL;
var
    pMeterObj: TEnergyMeterObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pMeterObj) then
        Exit;

    with pMeterObj do
    begin
        if (ActiveSection > 0) and (ActiveSection <= SectionCount) then
            Result := FeederSections^[ActiveSection].TotalCustomers
        else
            InvalidActiveSection(DSSPrime);
    end;
end;
//------------------------------------------------------------------------------
function Meters_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.EnergyMeters.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Meters_Set_idx(Value: Integer); CDECL;
var
    pEnergyMeter: TEnergyMeterObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    pEnergyMeter := DSSPrime.ActiveCircuit.EnergyMeters.Get(Value);
    if pEnergyMeter = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['Meter', Value], 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pEnergyMeter;
end;
//------------------------------------------------------------------------------
procedure Meters_Get_ZonePCE(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
   pMeter: TEnergyMeterObj;
   k: integer;
   Result: PPAnsiCharArray0;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 0);
    if InvalidCircuit(DSSPrime) then
        Exit;

    pMeter := DSSPrime.ActiveCircuit.EnergyMeters.Active;
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
