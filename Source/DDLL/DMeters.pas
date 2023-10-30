unit DMeters;

interface

function MetersI(mode: longint; arg: longint):Longint;cdecl;
function MetersF(mode: longint; arg: double):double;cdecl;
function MetersS(mode: longint; arg: pAnsiChar):pAnsiChar;cdecl;
procedure MetersV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

implementation

uses EnergyMeter,
     DSSGlobals,
     SysUtils,
     ucomplex,
     Variants,
     CktElement,
     PDElement,
     MemoryMap_lib,
     CktTree;

function MetersI(mode: longint; arg: longint):Longint;cdecl;

var
   pMeter             : TEnergyMeterObj;
   AssumeRestoration  : WordBool;
   PD_Element         : TPDElement;

begin
  Result:=0; // Default return value
  case mode of
  0: begin   // Meters.First
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
  1: begin  // Meters.Next
    Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
          pMeter := ActiveCircuit[ActiveActor].EnergyMeters.next;
          If pMeter <> Nil Then
          Begin
            Repeat   // Find an Enabled Meter
              If pMeter.Enabled  Then Begin
                ActiveCircuit[ActiveActor].ActiveCktElement := pMeter;
                Result := ActiveCircuit[ActiveActor].EnergyMeters.ActiveIndex;
              End
              Else  pMeter := ActiveCircuit[ActiveActor].EnergyMeters.next;
            Until (Result > 0) or (pMeter = nil);
          End
          Else
              Result := 0;  // signify no more
     End;
  end;
  2: begin  // Meters.Reset
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pMeter := ActiveCircuit[ActiveActor].EnergyMeters.Active;
        If pMeter <> Nil Then pMeter.ResetRegisters;
     End;
  end;
  3: begin  // Meters.ResetAll
     IF ActiveCircuit[ActiveActor] <> Nil THEN Begin
        EnergyMeterClass[ActiveActor].ResetAll(ActiveActor);
     End;
  end;
  4: begin  // Meters.Sample
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
          pMeter := ActiveCircuit[ActiveActor].EnergyMeters.Active;
          If pMeter <> Nil Then
            pMeter.TakeSample(ActiveActor);
     End;
  end;
  5: begin  // Meters.Save
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
          pMeter := ActiveCircuit[ActiveActor].EnergyMeters.Active;
          If pMeter <> Nil Then
            pMeter.SaveRegisters(ActiveActor);
     End;
  end;
  6: begin  // Meters.MeteredTerminal read
  // First make sure active circuit element is a meter
     IF ActiveCircuit[ActiveActor] <> Nil THEN
       Begin
            pMeter :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
            If pMeter <> Nil Then
              Begin
                  Result := pMeter.MeteredTerminal;
              End
            Else Result := 0;
       End
     ELSE Begin
          Result := 0;
     End;
  end;    // Meters.MeteredTerminal Write
  7: begin
     IF ActiveCircuit[ActiveActor] <> Nil THEN
     Begin
          pMeter :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
          If pMeter <> Nil Then
          Begin
              pMeter.MeteredTerminal := arg;
              pMeter.MeteredElementChanged := TRUE;
              pMeter.RecalcElementData(ActiveActor);
          End;
     End;
  end;
  8: begin  // Meters.DIFilesAreOpen
       IF ActiveCircuit[ActiveActor] <> Nil THEN Begin
          if DIFilesAreOpen[ActiveActor] then Result:=1;    // Global variable
       End;
  end;
  9: begin  // Meters.SampleAll
     IF ActiveCircuit[ActiveActor] <> Nil THEN Begin
        EnergyMeterClass[ActiveActor].SampleAll(ActiveActor);
     End;
     Result:=0;
  end;
  10: begin  // Meters.SaveAll
     IF ActiveCircuit[ActiveActor] <> Nil THEN Begin
        EnergyMeterClass[ActiveActor].SaveAll(ActiveActor);
     End;
     Result:=0;
  end;
  11: begin  // Meters.OpenAllDIFiles
     IF ActiveCircuit[ActiveActor] <> Nil THEN Begin
        EnergyMeterClass[ActiveActor].OpenAllDIFiles(ActiveActor);
     End;
     Result:=0;
  end;
  12: begin  // Meters.CloseAllDIFiles
     IF ActiveCircuit[ActiveActor] <> Nil THEN Begin
        EnergyMeterClass[ActiveActor].CloseAllDIFiles(ActiveActor);
     End;
     Result:=0;
  end;
  13: begin  // Meters.CountEndElements
    Result := 0;
    if ActiveCircuit[ActiveActor] <> Nil then begin
      pMeter :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
      If pMeter <> Nil Then Begin
        Result := pMeter.BranchList.ZoneEndsList.NumEnds;
      End;
    End;
  end;
  14: begin  // Meters.Count
     If Assigned(ActiveCircuit[ActiveActor]) Then
       Result := ActiveCircuit[ActiveActor].EnergyMeters.ListSize;
  end;
  15: begin  // Meters.CountBranches
    Result := 0;
    IF ActiveCircuit[ActiveActor] <> Nil THEN WITH ActiveCircuit[ActiveActor] DO Begin
      pMeter := EnergyMeters.Active;
      if pMeter <> Nil then
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
    End;
  end;
  16: begin   // Meters.SequenceList read
     Result := 0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then Begin
             Result := pMeter.SequenceList.ActiveIndex;
         End;
     End;
  end;
  17: begin   // Meters.SequenceList Write
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then With pMeter Do
         Begin
             If (arg>0) and (arg<=SequenceList.ListSize) Then
                      ActiveCktElement := SequenceList.Get(arg)
             Else
                DoSimpleMsg(Format('Invalid index for SequenceList: %d. List size is %d.',[arg, SequenceList.ListSize]), 500501);
         End;
     End;
  end;
  18: begin  // Meters.DoReliabilityCalc
     AssumeRestoration:=FALSE;
     if arg=1 then AssumeRestoration:=TRUE;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then Begin
                pMeter.CalcReliabilityIndices(AssumeRestoration, ActiveActor);

         End;
     End;
  end;
  19: begin  // Meters.SeqListSize
     Result := 0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then Begin
             Result := pMeter.SequenceList.ListSize ;
         End;
     End;
  end;
  20: begin  // Meters.TotalCustomers
     Result := 0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then Begin
             PD_Element := pMeter.SequenceList.Get(1) ;
             If Assigned (PD_Element) Then With PD_Element Do
                 Result := Buses^[Terminals^[FromTerminal].BusRef].BusTotalNumCustomers;
         End;
     End;
  end;
  21: begin  // Meters.NumSections
     Result := 0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then Begin
             Result := pMeter.SectionCount ;
         End;
     End;
  end;
  22: begin  // Meters.SetActiveSection
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then Begin
             If (arg > 0) and (arg <= pMeter.SectionCount) Then
                pMeter.ActiveSection := arg
             Else pMeter.ActiveSection := 0;
         End;
     End;
  end;
  23: begin  // Meters.OCPDeviceType
     Result := 0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then With pMeter Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].OCPDeviceType ;
         End;
     End;
  end;
  24: begin  // Meters.NumSectionCustomers
     Result := 0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then With pMeter Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].NCustomers   ;
         End;
     End;
  end;
  25: begin // Meters.NumSectionBranches
     Result := 0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then With pMeter Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].NBranches  ;
         End;
     End;
  end;     // Meters.SectSeqidx
  26: begin
     Result := 0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then With pMeter Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].SeqIndex ;
         End;
     End;
  end;
  27: begin  // Meters.SectTotalCust
     Result := 0;
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeter := TEnergyMeterObj(EnergyMeters.Active);
         If pMeter <> Nil Then With pMeter Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].TotalCustomers ;
         End;
     End;
  end
  else
      Result:=-1; // The parameter is not valid
  end;
end;

//*************************Floating point type properties***************************
function MetersF(mode: longint; arg: double):double;cdecl;

Var
  pMeterObj :TEnergyMeterObj;

begin
  Result:=0.0;  // Default return value
  case mode of
  0: begin  // Meters.SAIFI
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin

             Result := pMeterObj.SAIFI;

         End;
     End;
  end;
  1: begin   // Meters.SAIFIkW
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin

             Result := pMeterObj.SAIFIkW;

         End;
     End;
  end;
  2: begin  // Meters.SAIDI
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin
             Result := pMeterObj.SAIDI;
         End;
     End;
  end;
  3: begin  // Meters.CustItnerrupts
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then Begin
             Result := pMeterObj.CustInterrupts;
         End;
     End;
  end;
  4: begin  // Meters.AvgRepairTime
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then With pMeterObj Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].AverageRepairTime ;
         End;
     End;
  end;
  5: begin  // Meters.FaultRateXRepairHrs
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then With pMeterObj Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].SumFltRatesXRepairHrs  ;
         End;
     End;
  end;
  6: begin  // Meters.SumBranchFltRates
     If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
     Begin
         pMeterObj := TEnergyMeterObj(EnergyMeters.Active);
         If pMeterObj <> Nil Then With pMeterObj Do Begin
             If ActiveSection>0 then Result := FeederSections^[ActiveSection].SumBranchFltRates ;
         End;
     End;
  end
  else
      Result:=-1.0;
  end;
end;

//********************************String type properties**************************
function MetersS(mode: longint; arg: pAnsiChar):pAnsiChar;cdecl;

Var
   pMeterObj:TEnergyMeterObj;
   activesave :integer;
   TestStr: String;
   Found :Boolean;

begin
  Result := pAnsiChar(AnsiString('0')); // Default return value
  case mode of
  0: begin  // Meters.Name read
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        If pMeterObj <> Nil Then   Result := pAnsiChar(AnsiString(pMeterObj.name));
   End;
  end;
  1: begin // Meters.Name Write
     IF ActiveCircuit[ActiveActor] <> NIL
      THEN Begin      // Search list of EnergyMeters in active circuit for name
           WITH ActiveCircuit[ActiveActor].EnergyMeters DO
             Begin
                 TestStr := string(arg);  // Convert to Pascal String for testing
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
  2: begin   // Meters.MeteredElement read
  // First make sure active circuit element is a meter
     IF ActiveCircuit[ActiveActor] <> Nil THEN
     Begin
          pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
          If pMeterObj <> Nil Then
          Begin
              Result := pAnsiChar(AnsiString(pMeterObj.ElementName));
          End
          Else Result := pAnsiChar(AnsiString(''));
     End
     ELSE Begin
          Result := pAnsiChar(AnsiString(''));
     End;
  end;
  3: begin  // Meters.MeteredElement Write
    // First make sure active circuit element is a meter
       IF ActiveCircuit[ActiveActor] <> Nil THEN
       Begin
            pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
            If pMeterObj <> Nil Then
            Begin
                pMeterObj.elementName := string(arg);
                pMeterObj.MeteredElementChanged := TRUE;
                pMeterObj.RecalcElementData(ActiveActor);
            End;
       End;
  end
  else
      Result:=pAnsiChar(AnsiString('Error, Parameter not recognized'));
  end;
end;

//***************************Variant type properties******************************
procedure MetersV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

Var
  pMeter,
  pMeterObj,
  MeterElem     : TEnergyMeterObj;
  BranchCount,
  last,
  k,
  i             : Integer;
  pElem,
  elem          : TDSSCktElement;
  node          : TCktTreeNode;
  PDouble       : ^Double;

begin
  case mode of
  0:begin  // Meters.AllNames
      myType  :=  4;        // String
      setlength(myStrArray,0);
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        WITH ActiveCircuit[ActiveActor] DO
        Begin
          If (EnergyMeters.ListSize > 0) Then
          Begin
            MeterElem := EnergyMeters.First;
            WHILE MeterElem<>Nil DO
            Begin
              WriteStr2Array(MeterElem.Name);
              WriteStr2Array(Char(0));
              MeterElem := EnergyMeters.Next;
            End;
          End;
        End;
      End
      Else  WriteStr2Array('');
      myPointer :=  @(myStrArray[0]);
      mySize    :=  Length(myStrArray);
    end;
  1:begin  // Meters.RegisterNames
      myType  :=  4;        // String
      setlength(myStrArray,0);
      pMeterObj := TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
      if Assigned(pMeterObj) then
      Begin
        For k := 0 to  NumEMRegisters - 1  Do Begin
           WriteStr2Array(pMeterObj.RegisterNames[k + 1]);
           WriteStr2Array(Char(0));
        End;
      End
      Else WriteStr2Array('');
      myPointer :=  @(myStrArray[0]);
      mySize    :=  Length(myStrArray);
    end;
  2:begin  // Meters.RegisterValues
      myType  :=  2;        // Double
      setlength(myDBLArray, 1);
      myDBLArray[0] := 0;
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
          setlength(myDBLArray, numEMRegisters);
          FOR k := 0 to numEMRegisters-1 DO
          Begin
              myDBLArray[k] := pMeterObj.Registers[k+1];
          End;
        End;
      End;
      myPointer :=  @(myDBLArray[0]);
      mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
    end;
  3:begin  // Meters.Totals
      myType  :=  2;        // Double
      setlength(myDBLArray, 1);
      myDBLArray[0] := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
      Begin
        With ActiveCircuit[ActiveActor] Do
        Begin
            TotalizeMeters;
            setlength(myDBLArray, NumEMRegisters);
            For i := 1 to NumEMregisters Do myDBLArray[i-1] := RegisterTotals[i];
        End;
      End;
      myPointer :=  @(myDBLArray[0]);
      mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
    end;
  4:begin  // Meters.PeakCurrent read
      myType  :=  2;        // Double
      setlength(myDBLArray, 1);
      myDBLArray[0] := 0;
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
          pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
          If pMeterObj <> Nil Then
          Begin
              setlength(myDBLArray, pMeterObj.NPhases);
              FOR k := 0 to pMeterObj.NPhases-1 DO  myDBLArray[k] := pMeterObj.SensorCurrent^[k+1];
          End
      End;
      myPointer :=  @(myDBLArray[0]);
      mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
    end;
  5:begin  // Meters.PeakCurrent Write
      myType  :=  2;        // Double
      k       :=  1;
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
          FOR i := 1 to pMeterObj.NPhases DO
          Begin
            PDouble                     :=  myPointer;
            pMeterObj.SensorCurrent^[i] := PDouble^;
            inc(k);
            inc(PByte(myPointer),8);
          End;
        End;
      End;
      mySize  :=  k - 1;
    end;
  6:begin  // Meter.CalcCurrent read
      myType        :=  2;        // Double
      setlength(myDBLArray, 1);
      myDBLArray[0] := 0;
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
          setlength(myDBLArray, pMeterObj.NPhases);
          FOR k := 0 to pMeterObj.NPhases-1 DO  myDBLArray[k] := Cabs(pMeterObj.CalculatedCurrent^[k+1]);
        End;
      End;
      myPointer :=  @(myDBLArray[0]);
      mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
    end;
  7:begin  // Meters.CalcCurrent Write
    // First make sure active circuit element is a meter
      myType  :=  2;        // Double
      k :=  1;
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
          FOR i := 1 to pMeterObj.NPhases DO
          Begin
            PDouble :=  myPointer;
            pMeterObj.CalculatedCurrent^[i] := cmplx(PDouble^, 0.0);   // Just set the real part
            inc(k);
            inc(PByte(myPointer),8);
          End;
        End;
      End;
      mySize  :=  k - 1;
    end;
  8:begin  // Meters.AllocFactors read
    // First make sure active circuit element is a meter
      myType        :=  2;        // Double
      setlength(myDBLArray, 1);
      myDBLArray[0] := 0;
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            setlength(myDBLArray, pMeterObj.NPhases);
            FOR k := 0 to pMeterObj.NPhases-1 DO  myDBLArray[k] := pMeterObj.PhsAllocationFactor^[k+1];
        End;
      End;
      myPointer :=  @(myDBLArray[0]);
      mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
    end;
  9:begin   // Meters.AllocFactors Write
    // First make sure active circuit element is a meter
      myType  :=  2;        // Double
      k       :=  1;
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit[ActiveActor].EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
          FOR i := 1 to pMeterObj.NPhases DO
          Begin
            PDouble :=  myPointer;
            pMeterObj.PhsAllocationFactor^[i] := PDouble^;   // Just set the real part
            inc(k);
            inc(PByte(myPointer),8);
          End;
        End;
      End;
      mySize  :=  k - 1;
    end;
  10:begin  // Meters.AllEndElements
      myType  :=  4;        // String
      setlength(myStrArray,0);
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        WITH ActiveCircuit[ActiveActor] DO
        Begin
          pMeterObj := EnergyMeters.Active;
          if pMeterObj <> Nil then begin
            last := pMeterObj.BranchList.ZoneEndsList.NumEnds - 1;
            for k := 0 to last do begin
              pMeterObj.BranchList.ZoneEndsList.Get(k+1, node);
              elem := node.CktObject;
              WriteStr2Array(Format('%s.%s', [elem.ParentClass.Name, elem.Name]));
              WriteStr2Array(Char(0));
            end;
          end;
        End;
      End
      Else  WriteStr2Array('');
      myPointer :=  @(myStrArray[0]);
      mySize    :=  Length(myStrArray);
    end;
  11:begin  // Meters.ALlBranchesInZone
      myType  :=  4;        // String
      setlength(myStrArray,0);
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        WITH ActiveCircuit[ActiveActor] DO
        Begin
          pMeterObj := EnergyMeters.Active;
          if pMeterObj <> Nil then
          begin
            // Get count of branches
            BranchCount := MetersI(15,0);
            If BranchCount > 0 Then
            Begin
              pElem := pMeterObj.BranchList.First;
              while pElem <> Nil do   Begin
                 WriteStr2Array( Format ('%s.%s', [pElem.ParentClass.Name, pElem.Name]) );
                 WriteStr2Array(Char(0));
                 pElem := pMeterObj.BranchList.GoForward;
              End;
            End;
          end;
        End;
      End
      Else  WriteStr2Array('');
      myPointer :=  @(myStrArray[0]);
      mySize    :=  Length(myStrArray);
    end;
  12: begin  // Meters.ALLPCEinZone
      myType  :=  4;        // String
      setlength(myStrArray,0);
      If ActiveCircuit[ActiveActor] <> Nil Then
      Begin
        With ActiveCircuit[ActiveActor] Do
        Begin
          pMeter                  := EnergyMeters.Active;
          if pMeter <> nil then
          Begin
            pMeter.GetPCEatZone;
            // moves the list to the variant output
            if (length(pMeter.ZonePCE) > 0) and (pMeter.ZonePCE[0] <> '') then
            Begin
              for k := 0 to High(pMeter.ZonePCE) do
              Begin
                WriteStr2Array( pMeter.ZonePCE[k] );
                WriteStr2Array(Char(0));
              End;
            End;
          End;
        End;
      End
      Else  WriteStr2Array('');
      myPointer :=  @(myStrArray[0]);
      mySize    :=  Length(myStrArray);
    end
  else
    Begin
      myType  :=  4;        // String
      setlength(myStrArray, 0);
      WriteStr2Array('Error, parameter not recognized');
      myPointer :=  @(myStrArray[0]);
      mySize    :=  Length(myStrArray);
    End;
  end;
end;

end.
