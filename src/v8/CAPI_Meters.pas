UNIT CAPI_Meters;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE Meters_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE Meters_Get_AllNames_GR();cdecl;
function Meters_Get_First():Integer;cdecl;
function Meters_Get_Name():PAnsiChar;cdecl;
function Meters_Get_Next():Integer;cdecl;
PROCEDURE Meters_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE Meters_Get_RegisterNames_GR();cdecl;
PROCEDURE Meters_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Meters_Get_RegisterValues_GR();cdecl;
procedure Meters_Reset();cdecl;
procedure Meters_ResetAll();cdecl;
procedure Meters_Sample();cdecl;
procedure Meters_Save();cdecl;
procedure Meters_Set_Name(const Value: PAnsiChar);cdecl;
PROCEDURE Meters_Get_Totals(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Meters_Get_Totals_GR();cdecl;
PROCEDURE Meters_Get_Peakcurrent(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Meters_Get_Peakcurrent_GR();cdecl;
procedure Meters_Set_Peakcurrent(ValuePtr: PDouble; ValueCount: Integer);cdecl;
PROCEDURE Meters_Get_CalcCurrent(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Meters_Get_CalcCurrent_GR();cdecl;
procedure Meters_Set_CalcCurrent(ValuePtr: PDouble; ValueCount: Integer);cdecl;
PROCEDURE Meters_Get_AllocFactors(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Meters_Get_AllocFactors_GR();cdecl;
procedure Meters_Set_AllocFactors(ValuePtr: PDouble; ValueCount: Integer);cdecl;
function Meters_Get_MeteredElement():PAnsiChar;cdecl;
function Meters_Get_MeteredTerminal():Integer;cdecl;
procedure Meters_Set_MeteredElement(const Value: PAnsiChar);cdecl;
procedure Meters_Set_MeteredTerminal(Value: Integer);cdecl;
function Meters_Get_DIFilesAreOpen():WordBool;cdecl;
procedure Meters_CloseAllDIFiles();cdecl;
procedure Meters_OpenAllDIFiles();cdecl;
procedure Meters_SampleAll();cdecl;
procedure Meters_SaveAll();cdecl;
PROCEDURE Meters_Get_AllEndElements(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE Meters_Get_AllEndElements_GR();cdecl;
function Meters_Get_CountEndElements():Integer;cdecl;
function Meters_Get_Count():Integer;cdecl;
PROCEDURE Meters_Get_AllBranchesInZone(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE Meters_Get_AllBranchesInZone_GR();cdecl;
function Meters_Get_CountBranches():Integer;cdecl;
function Meters_Get_SAIFI():Double;cdecl;
function Meters_Get_SequenceIndex():Integer;cdecl;
procedure Meters_Set_SequenceIndex(Value: Integer);cdecl;
function Meters_Get_SAIFIKW():Double;cdecl;
procedure Meters_DoReliabilityCalc(AssumeRestoration: WordBool);cdecl;
function Meters_Get_SeqListSize():Integer;cdecl;
function Meters_Get_TotalCustomers():Integer;cdecl;
function Meters_Get_SAIDI():Double;cdecl;
function Meters_Get_CustInterrupts():Double;cdecl;
function Meters_Get_NumSections():Integer;cdecl;
procedure Meters_SetActiveSection(SectIdx: Integer);cdecl;
function Meters_Get_AvgRepairTime():Double;cdecl;
function Meters_Get_FaultRateXRepairHrs():Double;cdecl;
function Meters_Get_NumSectionBranches():Integer;cdecl;
function Meters_Get_NumSectionCustomers():Integer;cdecl;
function Meters_Get_OCPDeviceType():Integer;cdecl;
function Meters_Get_SumBranchFltRates():Double;cdecl;
function Meters_Get_SectSeqIdx():Integer;cdecl;
function Meters_Get_SectTotalCust():Integer;cdecl;

IMPLEMENTATION

USES CAPI_Constants, EnergyMeter, DSSGlobals, SysUtils, ucomplex, CktElement, PDElement, CktTree;

PROCEDURE Meters_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  MeterElem:TEnergyMeterObj;
  k:Integer;

Begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     If EnergyMeters.ListSize>0 Then
     Begin
       DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (EnergyMeters.ListSize-1) + 1);
       k:=0;
       MeterElem := EnergyMeters.First;
       WHILE MeterElem<>Nil DO
       Begin
          Result[k] := DSS_CopyStringAsPChar(MeterElem.Name);
          Inc(k);
          MeterElem := EnergyMeters.Next;
       End;
     End;

end;
PROCEDURE Meters_Get_AllNames_GR();cdecl;
// Same as Meters_Get_AllNames but uses global result (GR) pointers
begin
   Meters_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Meters_Get_First():Integer;cdecl;
Var
   pMeter:TEnergyMeterObj;

Begin

   Result := 0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   With ActiveCircuit[ActiveActor] Do
   Begin
        pMeter := EnergyMeters.First;
        If pMeter <> Nil Then
        Begin
          Repeat
            If pMeter.Enabled
            Then Begin
              ActiveCktElement := pMeter;
              Result := 1;
            End
            Else  pMeter := EnergyMeters.Next;
          Until (Result = 1) or (pMeter = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;
//------------------------------------------------------------------------------
function Meters_Get_Name_AnsiString():AnsiString;inline;
Var
   pMeterObj:TEnergyMeterObj;

Begin

   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        If pMeterObj <> Nil Then   Result := pMeterObj.name;
   End;
end;

function Meters_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Meters_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Meters_Get_Next():Integer;cdecl;
Var
   pMeterObj :TEnergyMeterObj;

Begin

   Result := 0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pMeterObj := ActiveCircuit[ActiveActor].EnergyMeters.next;
        If pMeterObj <> Nil Then
        Begin
          Repeat   // Find an Enabled Meter
            If pMeterObj.Enabled  Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pMeterObj;
              Result := ActiveCircuit[ActiveActor].EnergyMeters.ActiveIndex;
            End
            Else  pMeterObj := ActiveCircuit[ActiveActor].EnergyMeters.next;
          Until (Result > 0) or (pMeterObj = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;
//------------------------------------------------------------------------------
PROCEDURE Meters_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
    pMeterObj :TEnergyMeterObj;
    k :integer;

Begin
    pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
    if Assigned(pMeterObj) then  Begin
      Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumEMRegisters - 1) + 1);
      For k := 0 to  NumEMRegisters - 1  Do Begin
         Result[k] := DSS_CopyStringAsPChar(pMeterObj.RegisterNames[k + 1]);
      End;
    End
    Else Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1); // null array
end;
PROCEDURE Meters_Get_RegisterNames_GR();cdecl;
// Same as Meters_Get_RegisterNames but uses global result (GR) pointers
begin
   Meters_Get_RegisterNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
PROCEDURE Meters_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
   pMeterObj :TEnergyMeterObj;
   k         :Integer;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit[ActiveActor] <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (numEMRegisters-1) + 1);
            FOR k := 0 to numEMRegisters-1 DO
            Begin
                Result[k] := pMeterObj.Registers[k+1];
            End;
        End
        Else
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
   End
   ELSE Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
   End;

end;
PROCEDURE Meters_Get_RegisterValues_GR();cdecl;
// Same as Meters_Get_RegisterValues but uses global result (GR) pointers
begin
   Meters_Get_RegisterValues(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Meters_Reset();cdecl;
Var
   pMeter:TEnergyMeterObj;

Begin

   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pMeter := ActiveCircuit[ActiveActor].EnergyMeters.Active;
        If pMeter <> Nil Then pMeter.ResetRegisters;
   End;

end;
//------------------------------------------------------------------------------
procedure Meters_ResetAll();cdecl;
Begin
     IF ActiveCircuit[ActiveActor] <> Nil THEN Begin
        EnergyMeterClass[ActiveActor].ResetAll(ActiveActor);
     End;
end;
//------------------------------------------------------------------------------
procedure Meters_Sample();cdecl;
Var
   pMeter:TEnergyMeterObj;

Begin

   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pMeter := ActiveCircuit[ActiveActor].EnergyMeters.Active;
        If pMeter <> Nil Then
          pMeter.TakeSample(ActiveActor);
   End;

end;
//------------------------------------------------------------------------------
procedure Meters_Save();cdecl;
Var
   pMeter:TEnergyMeterObj;

Begin

   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pMeter := ActiveCircuit[ActiveActor].EnergyMeters.Active;
        If pMeter <> Nil Then
          pMeter.SaveRegisters(ActiveActor);
   End;

end;
//------------------------------------------------------------------------------
procedure Meters_Set_Name(const Value: PAnsiChar);cdecl;
VAR
    activesave :integer;
    pMeterObj:TEnergyMeterObj;
    TestStr: String;
    Found :Boolean;
Begin


  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin      // Search list of EnergyMeters in active circuit for name
       WITH ActiveCircuit[ActiveActor].EnergyMeters DO
         Begin
             TestStr := Value;  // Convert to Pascal String for testing
             Found := FALSE;
             ActiveSave := ActiveIndex;
             pMeterObj := First;
             While pMeterObj <> NIL Do
             Begin
                IF (CompareText(pMeterObj.Name, TestStr) = 0)
                THEN Begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := pMeterObj;
                    Found := TRUE;
                    Break;
                End;
                pMeterObj := Next;
             End;
             IF NOT Found
             THEN Begin
                 DoSimpleMsg('EnergyMeter "'+TestStr+'" Not Found in Active Circuit.', 5005);
                 pMeterObj := Get(ActiveSave);    // Restore active Meter
                 ActiveCircuit[ActiveActor].ActiveCktElement := pMeterObj;
             End;
         End;
  End;

end;
//------------------------------------------------------------------------------
PROCEDURE Meters_Get_Totals(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
   i:Integer;
   
begin

     If ActiveCircuit[ActiveActor] <> Nil Then With ActiveCircuit[ActiveActor] Do Begin
          TotalizeMeters;
          Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (NumEMRegisters-1) + 1);
          For i := 1 to NumEMregisters Do Result[i-1] := RegisterTotals[i];
     End
     Else Begin
          Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
     End;

end;
PROCEDURE Meters_Get_Totals_GR();cdecl;
// Same as Meters_Get_Totals but uses global result (GR) pointers
begin
   Meters_Get_Totals(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE Meters_Get_Peakcurrent(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
   pMeterObj :TEnergyMeterObj;
   k         :Integer;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit[ActiveActor] <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (pMeterObj.NPhases -1) + 1);
            FOR k := 0 to pMeterObj.NPhases-1 DO  Result[k] := pMeterObj.SensorCurrent^[k+1];
        End
        Else Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
   End
   ELSE Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
   End;

end;
PROCEDURE Meters_Get_Peakcurrent_GR();cdecl;
// Same as Meters_Get_Peakcurrent but uses global result (GR) pointers
begin
   Meters_Get_Peakcurrent(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Meters_Set_Peakcurrent(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  Value: PDoubleArray;
   pMeterObj :TEnergyMeterObj;
   k, i      :Integer;
Begin
   Value := PDoubleArray(ValuePtr);
// First make sure active circuit element is a meter
   IF ActiveCircuit[ActiveActor] <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            k := (0);   // get starting index for Value array
            FOR i := 1 to pMeterObj.NPhases DO Begin
               pMeterObj.SensorCurrent^[i] := Value[k];
               inc(k);
            End;
        End;
   End;

end;
//------------------------------------------------------------------------------
PROCEDURE Meters_Get_CalcCurrent(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
   pMeterObj :TEnergyMeterObj;
   k         :Integer;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit[ActiveActor] <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (pMeterObj.NPhases -1) + 1);
            FOR k := 0 to pMeterObj.NPhases-1 DO  Result[k] := Cabs(pMeterObj.CalculatedCurrent^[k+1]);
        End
        Else Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
   End
   ELSE Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
   End;

end;
PROCEDURE Meters_Get_CalcCurrent_GR();cdecl;
// Same as Meters_Get_CalcCurrent but uses global result (GR) pointers
begin
   Meters_Get_CalcCurrent(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Meters_Set_CalcCurrent(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  Value: PDoubleArray;
   pMeterObj :TEnergyMeterObj;
   k, i      :Integer;
Begin
   Value := PDoubleArray(ValuePtr);
// First make sure active circuit element is a meter
   IF ActiveCircuit[ActiveActor] <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            k := (0);   // get starting index for Value array
            FOR i := 1 to pMeterObj.NPhases DO Begin
               pMeterObj.CalculatedCurrent^[i] := cmplx(Value[k], 0.0);   // Just set the real part
               inc(k);
            End;
        End;
   End;

end;
//------------------------------------------------------------------------------
PROCEDURE Meters_Get_AllocFactors(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
   pMeterObj :TEnergyMeterObj;
   k         :Integer;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit[ActiveActor] <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (pMeterObj.NPhases -1) + 1);
            FOR k := 0 to pMeterObj.NPhases-1 DO  Result[k] := pMeterObj.PhsAllocationFactor^[k+1];
        End
        Else Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
   End
   ELSE Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
   End;

end;
PROCEDURE Meters_Get_AllocFactors_GR();cdecl;
// Same as Meters_Get_AllocFactors but uses global result (GR) pointers
begin
   Meters_Get_AllocFactors(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Meters_Set_AllocFactors(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  Value: PDoubleArray;
   pMeterObj :TEnergyMeterObj;
   k, i      :Integer;
Begin
   Value := PDoubleArray(ValuePtr);
// First make sure active circuit element is a meter
   IF ActiveCircuit[ActiveActor] <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            k := (0);   // get starting index for Value array
            FOR i := 1 to pMeterObj.NPhases DO Begin
               pMeterObj.PhsAllocationFactor^[i] := Value[k];   // Just set the real part
               inc(k);
            End;
        End;
   End;

end;
//------------------------------------------------------------------------------
function Meters_Get_MeteredElement_AnsiString():AnsiString;inline;
Var
   pMeterObj :TEnergyMeterObj;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit[ActiveActor] <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            Result := pMeterObj.ElementName;
        End
        Else Result := '';
   End
   ELSE Begin
        Result := '';
   End;

end;

function Meters_Get_MeteredElement():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Meters_Get_MeteredElement_AnsiString());
end;
//------------------------------------------------------------------------------
function Meters_Get_MeteredTerminal():Integer;cdecl;
Var
   pMeterObj :TEnergyMeterObj;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit[ActiveActor] <> Nil THEN
     Begin
          pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
          If pMeterObj <> Nil Then
            Begin
                Result := pMeterObj.MeteredTerminal;
            End
          Else Result := 0;
     End
   ELSE Begin
        Result := 0;
   End;

end;
//------------------------------------------------------------------------------
procedure Meters_Set_MeteredElement(const Value: PAnsiChar);cdecl;
Var
   pMeterObj :TEnergyMeterObj;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit[ActiveActor] <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            pMeterObj.elementName := Value;
            pMeterObj.MeteredElementChanged := TRUE;
            pMeterObj.RecalcElementData(ActiveActor);
        End;
   End;

end;
//------------------------------------------------------------------------------
procedure Meters_Set_MeteredTerminal(Value: Integer);cdecl;
Var
   pMeterObj :TEnergyMeterObj;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit[ActiveActor] <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            pMeterObj.MeteredTerminal := Value;
            pMeterObj.MeteredElementChanged := TRUE;
            pMeterObj.RecalcElementData(ActiveActor);
        End;
   End;

end;
//------------------------------------------------------------------------------
function Meters_Get_DIFilesAreOpen():WordBool;cdecl;
begin
     IF ActiveCircuit[ActiveActor] <> Nil THEN Begin
            Result := DIFilesAreOpen[ActiveActor];    // Global variable
     End;
end;
//------------------------------------------------------------------------------
procedure Meters_CloseAllDIFiles();cdecl;
begin
     IF ActiveCircuit[ActiveActor] <> Nil THEN Begin
        EnergyMeterClass[ActiveActor].CloseAllDIFiles(ActiveActor);
     End;
end;
//------------------------------------------------------------------------------
procedure Meters_OpenAllDIFiles();cdecl;
begin
     IF ActiveCircuit[ActiveActor] <> Nil THEN Begin
        EnergyMeterClass[ActiveActor].OpenAllDIFiles(ActiveActor);
     End;
end;
//------------------------------------------------------------------------------
procedure Meters_SampleAll();cdecl;
begin
     IF ActiveCircuit[ActiveActor] <> Nil THEN Begin
        EnergyMeterClass[ActiveActor].SampleAll(ActiveActor);
     End;
end;
//------------------------------------------------------------------------------
procedure Meters_SaveAll();cdecl;
begin
     IF ActiveCircuit[ActiveActor] <> Nil THEN Begin
        EnergyMeterClass[ActiveActor].SaveAll(ActiveActor);
     End;
end;
//------------------------------------------------------------------------------
PROCEDURE Meters_Get_AllEndElements(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  pMeterObj :TEnergyMeterObj;
  k, last:Integer;
  elem : TDSSCktElement;
  node : TCktTreeNode;
Begin
  Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
  IF ActiveCircuit[ActiveActor] <> Nil THEN WITH ActiveCircuit[ActiveActor] DO Begin
    pMeterObj := EnergyMeters.Active;
    if pMeterObj <> Nil then begin
      last := pMeterObj.BranchList.ZoneEndsList.NumEnds - 1;
      DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (last) + 1);
      for k := 0 to last do begin
        pMeterObj.BranchList.ZoneEndsList.Get(k+1, node);
        elem := node.CktObject;
        Result[k] := DSS_CopyStringAsPChar(Format ('%s.%s', [elem.ParentClass.Name, elem.Name]));
      end;
    end;
  End;
end;
PROCEDURE Meters_Get_AllEndElements_GR();cdecl;
// Same as Meters_Get_AllEndElements but uses global result (GR) pointers
begin
   Meters_Get_AllEndElements(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Meters_Get_CountEndElements():Integer;cdecl;
Var
  pMeterObj :TEnergyMeterObj;
begin
  Result := 0;
  if ActiveCircuit[ActiveActor] <> Nil then begin
    pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
    If pMeterObj <> Nil Then Begin
      Result := pMeterObj.BranchList.ZoneEndsList.NumEnds;
    End;
  End;
end;
//------------------------------------------------------------------------------
function Meters_Get_Count():Integer;cdecl;
begin
     If Assigned(ActiveCircuit[ActiveActor]) Then
       Result := ActiveCircuit[ActiveActor].EnergyMeters.ListSize;
end;
//------------------------------------------------------------------------------
PROCEDURE Meters_Get_AllBranchesInZone(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  pMeterObj   :TEnergyMeterObj;
  k           :Integer;
  BranchCount :Integer;
  pElem       :TDSSCktElement;
Begin
  Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
  IF ActiveCircuit[ActiveActor] <> Nil THEN WITH ActiveCircuit[ActiveActor] DO Begin
    pMeterObj := EnergyMeters.Active;
    if pMeterObj <> Nil then begin
      // Get count of branches
      BranchCount := Meters_Get_CountBranches;
      If BranchCount > 0 Then Begin
          DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (BranchCount-1) + 1);
          pElem := pMeterObj.BranchList.First;
          k := 0;
          while pElem <> Nil do   Begin
             Result[k] := DSS_CopyStringAsPChar(Format ('%s.%s', [pElem.ParentClass.Name, pElem.Name]));
             inc(k);
             pElem := pMeterObj.BranchList.GoForward;
          End;
      End;
    end;
  End;

end;
PROCEDURE Meters_Get_AllBranchesInZone_GR();cdecl;
// Same as Meters_Get_AllBranchesInZone but uses global result (GR) pointers
begin
   Meters_Get_AllBranchesInZone(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Meters_Get_CountBranches():Integer;cdecl;
Var
  pMeterObj :TEnergyMeterObj;
  // pelem : TDSSCktElement;
Begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> Nil THEN WITH ActiveCircuit[ActiveActor] DO Begin
    pMeterObj := EnergyMeters.Active;
    if pMeterObj <> Nil then
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

  End;
end;
//------------------------------------------------------------------------------
function Meters_Get_SAIFI():Double;cdecl;
Var
  pMeterObj :TEnergyMeterObj;

begin
     Result := 0.0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin

             Result := pMeterObj.SAIFI;

         End;
     End;
end;
//------------------------------------------------------------------------------
function Meters_Get_SequenceIndex():Integer;cdecl;
Var
  pMeterObj :TEnergyMeterObj;

begin
     Result := 0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin
             Result := pMeterObj.SequenceList.ActiveIndex;
         End;
     End;
end;
//------------------------------------------------------------------------------
procedure Meters_Set_SequenceIndex(Value: Integer);cdecl;
Var
  pMeterObj :TEnergyMeterObj;

begin
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then With pMeterObj Do
         Begin
             If (Value>0) and (Value<=SequenceList.ListSize) Then
                      ActiveCktElement := SequenceList.Get(Value)
             Else
                DoSimpleMsg(Format('Invalid index for SequenceList: %d. List size is %d.',[Value, SequenceList.ListSize]), 500501);
         End;
     End;
end;
//------------------------------------------------------------------------------
function Meters_Get_SAIFIKW():Double;cdecl;
Var
  pMeterObj :TEnergyMeterObj;

begin
     Result := 0.0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin

             Result := pMeterObj.SAIFIkW;

         End;
     End;
end;
//------------------------------------------------------------------------------
procedure Meters_DoReliabilityCalc(AssumeRestoration: WordBool);cdecl;
Var
  pMeterObj :TEnergyMeterObj;

begin
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin

                pMeterObj.CalcReliabilityIndices(AssumeRestoration);

         End;
     End;
end;
//------------------------------------------------------------------------------
function Meters_Get_SeqListSize():Integer;cdecl;
Var
  pMeterObj :TEnergyMeterObj;

begin
     Result := 0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin
             Result := pMeterObj.SequenceList.ListSize ;
         End;
     End;
end;
//------------------------------------------------------------------------------
function Meters_Get_TotalCustomers():Integer;cdecl;
Var
  pMeterObj :TEnergyMeterObj;
  PD_Element   :TPDElement;

begin
     Result := 0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin
             PD_Element := pMeterObj.SequenceList.Get(1) ;
             If Assigned (PD_Element) Then With PD_Element Do
                 Result := Buses^[Terminals^[FromTerminal].BusRef].BusTotalNumCustomers;
         End;
     End;
end;
//------------------------------------------------------------------------------
function Meters_Get_SAIDI():Double;cdecl;
Var
  pMeterObj :TEnergyMeterObj;

begin
     Result := 0.0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin
             Result := pMeterObj.SAIDI;
         End;
     End;
end;
//------------------------------------------------------------------------------
function Meters_Get_CustInterrupts():Double;cdecl;
Var
  pMeterObj :TEnergyMeterObj;

begin
     Result := 0.0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin
             Result := pMeterObj.CustInterrupts;
         End;
     End;
end;
//------------------------------------------------------------------------------
function Meters_Get_NumSections():Integer;cdecl;
Var
  pMeterObj :TEnergyMeterObj;
begin
     Result := 0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin
             Result := pMeterObj.SectionCount ;
         End;
     End;
end;
//------------------------------------------------------------------------------
procedure Meters_SetActiveSection(SectIdx: Integer);cdecl;
Var
  pMeterObj :TEnergyMeterObj;
begin
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin
             If (SectIdx > 0) and (SectIdx <= pMeterObj.SectionCount) Then
                pMeterObj.ActiveSection := SectIdx
             Else pMeterObj.ActiveSection := 0;
         End;
     End;

end;
//------------------------------------------------------------------------------
function Meters_Get_AvgRepairTime():Double;cdecl;
Var
  pMeterObj :TEnergyMeterObj;
begin
     Result := 0.0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then With pMeterObj Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].AverageRepairTime ;
         End;
     End;
end;
//------------------------------------------------------------------------------
function Meters_Get_FaultRateXRepairHrs():Double;cdecl;
Var
  pMeterObj :TEnergyMeterObj;
begin
     Result := 0.0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then With pMeterObj Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].SumFltRatesXRepairHrs  ;
         End;
     End;
end;
//------------------------------------------------------------------------------
function Meters_Get_NumSectionBranches():Integer;cdecl;
Var
  pMeterObj :TEnergyMeterObj;
begin
     Result := 0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then With pMeterObj Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].NBranches  ;
         End;
     End;
end;
//------------------------------------------------------------------------------
function Meters_Get_NumSectionCustomers():Integer;cdecl;
Var
  pMeterObj :TEnergyMeterObj;
begin
     Result := 0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then With pMeterObj Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].NCustomers   ;
         End;
     End;
end;
//------------------------------------------------------------------------------
function Meters_Get_OCPDeviceType():Integer;cdecl;
Var
  pMeterObj :TEnergyMeterObj;
begin
     Result := 0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then With pMeterObj Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].OCPDeviceType ;
         End;
     End;
end;
//------------------------------------------------------------------------------
function Meters_Get_SumBranchFltRates():Double;cdecl;
Var
  pMeterObj :TEnergyMeterObj;
begin
     Result := 0.0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then With pMeterObj Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].SumBranchFltRates ;
         End;
     End;

end;
//------------------------------------------------------------------------------
function Meters_Get_SectSeqIdx():Integer;cdecl;
Var
  pMeterObj :TEnergyMeterObj;
begin
     Result := 0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then With pMeterObj Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].SeqIndex ;
         End;
     End;

end;
//------------------------------------------------------------------------------
function Meters_Get_SectTotalCust():Integer;cdecl;
Var
  pMeterObj :TEnergyMeterObj;
begin
     Result := 0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then With pMeterObj Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].TotalCustomers ;
         End;
     End;


end;
//------------------------------------------------------------------------------
END.
