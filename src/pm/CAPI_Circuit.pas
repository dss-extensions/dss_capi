UNIT CAPI_Circuit;
{$inline on}

INTERFACE

USES ArrayDef, CAPI_Utils;

function Circuit_Get_Name():PAnsiChar;cdecl;
function Circuit_Get_NumBuses():Integer;cdecl;
function Circuit_Get_NumCktElements():Integer;cdecl;
function Circuit_Get_NumNodes():Integer;cdecl;
PROCEDURE Circuit_Get_LineLosses(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
PROCEDURE Circuit_Get_Losses(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
PROCEDURE Circuit_Get_AllBusVmag(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
PROCEDURE Circuit_Get_AllBusVolts(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
PROCEDURE Circuit_Get_AllElementNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
PROCEDURE Circuit_Get_SubstationLosses(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
PROCEDURE Circuit_Get_TotalPower(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
procedure Circuit_Disable(const Name: PAnsiChar);cdecl;
procedure Circuit_Enable(const Name: PAnsiChar);cdecl;
function Circuit_FirstPCElement():Integer;cdecl;
function Circuit_FirstPDElement():Integer;cdecl;
function Circuit_NextPCElement():Integer;cdecl;
function Circuit_NextPDElement():Integer;cdecl;
PROCEDURE Circuit_Get_AllBusNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
PROCEDURE Circuit_Get_AllElementLosses(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
procedure Circuit_Sample();cdecl;
procedure Circuit_SaveSample();cdecl;
function Circuit_SetActiveElement(const FullName: PAnsiChar):Integer;cdecl;
function Circuit_Capacity(Start, Increment: Double):Double;cdecl;
PROCEDURE Circuit_Get_AllBusVmagPu(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
function Circuit_SetActiveBus(const BusName: PAnsiChar):Integer;cdecl;
function Circuit_SetActiveBusi(BusIndex: Integer):Integer;cdecl;
PROCEDURE Circuit_Get_AllNodeNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
PROCEDURE Circuit_Get_SystemY(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
PROCEDURE Circuit_Get_AllBusDistances(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
PROCEDURE Circuit_Get_AllNodeDistances(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
PROCEDURE Circuit_Get_AllNodeDistancesByPhase(var ResultPtr: PDouble; var ResultCount: Integer; Phase: Integer);cdecl;
PROCEDURE Circuit_Get_AllNodeVmagByPhase(var ResultPtr: PDouble; var ResultCount: Integer; Phase: Integer);cdecl;
PROCEDURE Circuit_Get_AllNodeVmagPUByPhase(var ResultPtr: PDouble; var ResultCount: Integer; Phase: Integer);cdecl;
PROCEDURE Circuit_Get_AllNodeNamesByPhase(var ResultPtr: PPAnsiChar; var ResultCount: Integer; Phase: Integer);cdecl;
function Circuit_SetActiveClass(const ClassName: PAnsiChar):Integer;cdecl;
function Circuit_FirstElement():Integer;cdecl;
function Circuit_NextElement():Integer;cdecl;
procedure Circuit_UpdateStorage();cdecl;
function Circuit_Get_ParentPDElement():Integer;cdecl;
procedure Circuit_EndOfTimeStepUpdate();cdecl;
PROCEDURE Circuit_Get_YNodeOrder(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
PROCEDURE Circuit_Get_YCurrents(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
PROCEDURE Circuit_Get_YNodeVarray(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
procedure Circuit_SetCktElementIndex(const Value: Integer);cdecl;
procedure Circuit_SetCktElementName(const Value: PAnsiChar);cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSClassDefs, DSSGlobals, Line, UComplex, sysutils, CktElement, DSSObject, DSSClass, Transformer, PCElement, PDElement, Monitor, EnergyMeter, YMatrix, Utilities, SolutionAlgs, KLUSolve;

function Circuit_Get_Name_AnsiString():AnsiString;inline;
begin
      If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].Name
      Else Result := '';
end;

function Circuit_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Circuit_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Circuit_Get_NumBuses():Integer;cdecl;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].NumBuses
    Else Result := 0;
end;
//------------------------------------------------------------------------------
function Circuit_Get_NumCktElements():Integer;cdecl;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].NumDevices;
end;
//------------------------------------------------------------------------------
function Circuit_Get_NumNodes():Integer;cdecl;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Result := ActiveCircuit[ActiveActor].NumNodes;
end;
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_LineLosses(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray; pLine :TLineObj;
    Loss :Complex;
    V   : PDoubleArray;

Begin
    V := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (1) + 1);
    IF ActiveCircuit[ActiveActor] <> NIL THEN
    WITH ActiveCircuit[ActiveActor] DO
    Begin
      pLine := Lines.First;
      Loss := Cmplx(0.0,0.0);
      WHILE pLine<>nil DO
      Begin
         CAccum(Loss, pLine.Losses);
         pLine := Lines.Next;
      End;
      V[0] := Loss.re * 0.001;
      V[1] := Loss.im * 0.001;
    End;

    Result := V;

end;
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_Losses(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray;
   LossValue :complex;
begin

     IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
         Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (1) + 1);
         LossValue := ActiveCircuit[ActiveActor].Losses;
         Result[0] := LossValue.re;
         Result[1] := LossValue.im;
      End
    ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_AllBusVmag(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray;
   i,j,k:Integer;

Begin
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (NumNodes-1) + 1);
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
           For j := 1 to Buses^[i].NumNodesThisBus  DO
           Begin
              Result[k] := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(j)]);
              Inc(k);
           End;
       End;
     End
    ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
End;
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_AllBusVolts(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray;
   i,j,k:Integer;
   Volts:Complex;

Begin
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (2*NumNodes-1) + 1);
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
         For j := 1 to Buses^[i].NumNodesThisBus DO
         Begin
           Volts := ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(j)];
             Result[k] := Volts.re;
             Inc(k);
             Result[k] := Volts.im;
             Inc(k);
         End;
       End;
     End
    ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_AllElementNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
VAR
  Result: PPAnsiCharArray;
   i:Integer;

Begin
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumDevices-1) + 1);
       FOR i := 1 to NumDevices DO
       Begin
            WITH  TDSSCktElement(CktElements.Get(i)) DO
             Result[i-1] := DSS_CopyStringAsPChar(ParentClass.Name + '.' + Name);
       End;
     End
    ELSE Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
end;
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_SubstationLosses(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray; pTransf:TTransfObj;
    Loss:Complex;

Begin
    Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (1) + 1);
    IF ActiveCircuit[ActiveActor] <> nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       pTransf := Transformers.First;
       Loss := Cmplx(0.0,0.0);
       WHILE pTransf<>nil DO
       Begin
          IF pTransf.Issubstation THEN Caccum(Loss, pTransf.Losses);
          pTransf := Transformers.Next;
       End;
       Result[0] := Loss.re * 0.001;
       Result[1] := Loss.im * 0.001;
     End
    ELSE
     Begin
       Result[0] := 0.0;
       Result[1] := 0.0;
     End;

end;
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_TotalPower(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
// Total power being consumed in the circuit.
// Add up all power being contributed by sources.
// Returns result in kW

VAR
  Result: PDoubleArray; pCktElem:TDSSCktElement;
    cPower:Complex;

Begin

    Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (1) + 1);
    IF ActiveCircuit[ActiveActor] <> nil THEN
      WITH ActiveCircuit[ActiveActor] DO Begin
        pCktElem := Sources.First;
        cPower := Cmplx(0.0, 0.0);
        WHILE pCktElem<>nil  DO Begin
           CAccum(cPower, pcktElem.Power[1,ActiveActor]);
           pCktElem := Sources.Next;
        End;
        Result[0] := cPower.re * 0.001;
        Result[1] := cPower.im * 0.001;
      End
    ELSE
      Begin
        Result[0] := 0.0;
        Result[1] := 0.0;
      End;
end;
//------------------------------------------------------------------------------
procedure Circuit_Disable(const Name: PAnsiChar);cdecl;
begin

   IF ActiveCircuit[ActiveActor] <> Nil THEN
   WITH ActiveCircuit[ActiveActor] DO
   Begin
      SetElementActive(Name);
      If ActiveCktElement<>nil THEN ActiveCktElement.Enabled := FALSE;
   End;

end;
//------------------------------------------------------------------------------
procedure Circuit_Enable(const Name: PAnsiChar);cdecl;
begin

   WITH ActiveCircuit[ActiveActor] DO Begin
      SetElementActive(Name);
      If ActiveCktElement<>nil THEN ActiveCktElement.Enabled := TRUE;
   End;

end;
//------------------------------------------------------------------------------
function Circuit_FirstPCElement():Integer;cdecl;
VAR
   p:TDSSCktElement;

{ Returns first enabled element}

Begin
      Result := 0;
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        p:= ActiveCircuit[ActiveActor].PCElements.First;
        IF p <> Nil  THEN Begin
           Repeat
               If p.enabled Then Begin
                   Result := 1;
                   ActiveCircuit[ActiveActor].ActiveCktElement := p;
               End
               Else  p := ActiveCircuit[ActiveActor].PCElements.Next;

           Until (Result = 1) or (p = nil);
        End
        ELSE Result := 0;
      End;
end;
//------------------------------------------------------------------------------
function Circuit_FirstPDElement():Integer;cdecl;
Var
   ActivePDElement :TPDElement;
Begin
      Result := 0;
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
       ActivePDElement := ActiveCircuit[ActiveActor].PDElements.First;
       IF ActivePDElement<> Nil THEN
         Begin
              Repeat
                If ActivePDElement.enabled
                Then Begin
                  Result := 1;
                  ActiveCircuit[ActiveActor].ActiveCktElement := ActivePDElement;
                end
                Else  ActivePDElement := ActiveCircuit[ActiveActor].PDElements.Next;
              Until (Result = 1) or (ActivePDELement = nil);
         End
       ELSE Result := 0;
      End;

end;
//------------------------------------------------------------------------------
function Circuit_NextPCElement():Integer;cdecl;
VAR
   p:TDSSCktElement;

Begin
      Result := 0;
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        p:= ActiveCircuit[ActiveActor].PCElements.Next;
        IF p<> Nil THEN
        Begin
             Repeat
                 If p.enabled
                 Then Begin
                   Result := ActiveCircuit[ActiveActor].PCElements.ActiveIndex;
                   ActiveCircuit[ActiveActor].ActiveCktElement := p;
                 End
                 Else p :=  ActiveCircuit[ActiveActor].PCElements.Next;
             Until (Result > 0) or (p = nil);
        End ELSE  Result := 0;
      End;
end;
//------------------------------------------------------------------------------
function Circuit_NextPDElement():Integer;cdecl;
Var
   ActivePDElement :TPDElement;
Begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        ActivePDElement:= ActiveCircuit[ActiveActor].PDElements.Next;
        IF ActivePDElement <> Nil
        THEN Begin
           Repeat
             If ActivePDElement.Enabled
             Then Begin
                 Result := ActiveCircuit[ActiveActor].PDElements.ActiveIndex;
                 ActiveCircuit[ActiveActor].ActiveCktElement := ActivePDElement;
             End
             Else ActivePDElement:= ActiveCircuit[ActiveActor].PDElements.Next;
           Until (Result > 0) or (ActivePDElement = Nil);
        End
        ELSE Begin
           Result := 0;
        End;
      End;
end;
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_AllBusNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
// Just Bus names      modified 2/7/03
VAR
  Result: PPAnsiCharArray;
   i:Integer;

Begin
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumBuses-1) + 1);
       FOR i := 0 to NumBuses-1 DO
       Begin
           Result[i] := DSS_CopyStringAsPChar(BusList.Get(i+1));
       End;
     End
    ELSE Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
end;
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_AllElementLosses(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray;
  pCktElem:TDSSCktElement;
  cLoss:Complex;
  k:Integer;

Begin
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (2*NumDevices-1) + 1);
       k:=0;
       pCktElem := CktElements.First;
       WHILE pCktElem<>Nil DO
       Begin
          cLoss := pCktElem.Losses;
          Result[k] := cLoss.re * 0.001;
          Inc(k);
          Result[k] := cLoss.im * 0.001;
          Inc(k);
          pCktElem := CktElements.Next;
       End;
     End
    ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
end;
//------------------------------------------------------------------------------
procedure Circuit_Sample();cdecl;
// Sample all meters and monitors

Begin

      MonitorClass[ActiveActor].SampleAll(ActiveActor);

      EnergyMeterClass[ActiveActor].SampleAll(ActiveActor);

end;
//------------------------------------------------------------------------------
procedure Circuit_SaveSample();cdecl;
// Save all meters and monitors registers and buffers

VAR
    Mon :TDSSMonitor;
    Mtr :TEnergyMeter;

Begin
    Mon := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('monitor'));
    Mon.SaveAll(ActiveActor);

    Mtr := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('energymeter'));
    Mtr.SaveAll(ActiveActor);
end;
//------------------------------------------------------------------------------
function Circuit_SetActiveElement(const FullName: PAnsiChar):Integer;cdecl;
begin
   Result := -1;
   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
       Result := ActiveCircuit[ActiveActor].SetElementActive(FullName) - 1;   // make zero based to be compatible with collections and variant arrays
   End
   ELSE DoSimpleMsg('Create a circuit before trying to set an element active!', 5015);
end;
//------------------------------------------------------------------------------
function Circuit_Capacity(Start, Increment: Double):Double;cdecl;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then  With ActiveCircuit[ActiveActor] Do
    Begin
         CapacityStart := Start;
         CapacityIncrement := Increment;
         If ComputeCapacity(ActiveActor) Then
             Result := RegisterTotals[3] + RegisterTotals[19]
         Else
             Result := 0.0;
    End
    Else Begin
        Result := 0.0;
    End;
end;
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_AllBusVmagPu(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray;
   i,j,k:Integer;
   Volts,BaseFactor:Double;

Begin
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (NumNodes-1) + 1);
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
          If Buses^[i].kVBase >0.0 then BaseFactor :=  1000.0* Buses^[i].kVBase  Else BaseFactor := 1.0;
           For j := 1 to Buses^[i].NumNodesThisBus  DO
           Begin
             Volts := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(j)]);
             Result[k] := Volts/BaseFactor;
             Inc(k);
           End;
       End;
     End
    ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
end;
//------------------------------------------------------------------------------
function Circuit_SetActiveBus(const BusName: PAnsiChar):Integer;cdecl;
begin
     DSSGlobals.SetActiveBus(StripExtension(BusName));
     If Assigned(ActiveCircuit[ActiveActor]) then Result := ActiveCircuit[ActiveActor].ActiveBusIndex - 1 Else Result := -1;
end;
//------------------------------------------------------------------------------
function Circuit_SetActiveBusi(BusIndex: Integer):Integer;cdecl;
{ BusIndex is Zero Based}
begin
    Result := -1;   // Signifies Error
    If Assigned(ActiveCircuit[ActiveActor]) Then
    With ActiveCircuit[ActiveActor] Do Begin
        If (BusIndex >= 0) and (BusIndex < Numbuses) Then Begin
           ActiveBusIndex := BusIndex + 1;
           Result := 0;
        End;
    End;
end;
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_AllNodeNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
// Return all node names (Busname.nodenumber)
// Same order as current solution array.
VAR
  Result: PPAnsiCharArray;
   i,j,k:Integer;
   BusName:String;

Begin
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumNodes-1) + 1);
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
           BusName := BusList.Get(i);
           FOR j := 1 to Buses^[i].NumNodesThisBus DO
           Begin
                Result[k] := DSS_CopyStringAsPChar(BusName + '.' + IntToStr(Buses^[i].GetNum(j)));
                Inc(k);
           End;
       End;
     End
    ELSE Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
end;

// this calls the compressed column
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_SystemY(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
{Return System Y matrix, complex form}

VAR
  Result: PDoubleArray;
   iV               :LongWord;
   i,j,p            :LongWord;
   NValues          :LongWord;
   hY               :NativeUint;
   nBus, nNZ        :LongWord;
   ColPtr, RowIdx   :array of LongWord;
   cVals            :array of Complex;

begin

{ Return zero length Array if no circuit or no Y matrix}
   IF ActiveCircuit[ActiveActor] = nil                Then Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1)
   ELSE If ActiveCircuit[ActiveActor].Solution.hY = 0 Then Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1)
   ELSE
   With ActiveCircuit[ActiveActor] Do Begin
      hY := ActiveCircuit[ActiveActor].Solution.hY;

      // get the compressed columns out of KLU
      FactorSparseMatrix (hY); // no extra work if already done
      GetNNZ (hY, @nNZ);
      GetSize (hY, @nBus);
      SetLength (ColPtr, nBus + 1);
      SetLength (RowIdx, nNZ);
      SetLength (cVals, nNZ);
      GetCompressedMatrix (hY, nBus + 1, nNZ, @ColPtr[0], @RowIdx[0], @cVals[0]);

      // allocate a square matrix
      NValues := SQR(NumNodes);
      Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (2*NValues -1) + 1);  // Make variant array for complex

      // the new way, first set all elements to zero
      for iV := 0 to 2*NValues - 1 do Result[iV] := 0.0;
      // then back-fill the non-zero values
      for j := 0 to nBus - 1 do begin /// the zero-based column
        for p := ColPtr[j] to ColPtr[j+1] - 1 do begin
          i := RowIdx[p];  // the zero-based row
          iV := i * nBus + j; // the zero-based, row-wise, complex result index
          Result[iV*2] := cVals[p].re;
          Result[iV*2+1] := cVals[p].im;
        end;
      end;
   END;

end;
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_AllBusDistances(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
{Return distances from each bus to its parent energymeter in an array that aligns with the buslist}
VAR
  Result: PDoubleArray;
   i:Integer;

Begin
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (NumBuses-1) + 1);
       FOR i := 0 to NumBuses-1 DO
       Begin
           Result[i] := Buses^[i+1].DistFromMeter;
       End;
     End
    ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_AllNodeDistances(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
{Return distance from each Node back to parent EnergyMeter}
{Array sequence is same as all bus Vmag and Vmagpu}
VAR
  Result: PDoubleArray;
   i,j,k:Integer;

Begin
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (NumNodes-1) + 1);
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
           FOR j := 1 to Buses^[i].NumNodesThisBus DO
           Begin
                Result[k] := Buses^[i].DistFromMeter;
                Inc(k);
           End;
       End;
     End
    ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_AllNodeDistancesByPhase(var ResultPtr: PDouble; var ResultCount: Integer; Phase: Integer);cdecl;
VAR
  Result: PDoubleArray;
   i,k, NodeIdx:Integer;
   Temp:ArrayDef.PDoubleArray;

Begin
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       // Make a Temporary Array big enough to hold all nodes
       Temp := AllocMem(SizeOF(Temp^[1]) * NumNodes);

       // Find nodes connected to specified phase
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
           NodeIdx := Buses^[i].FindIdx(Phase);
           If NodeIdx > 0 then   // Node found with this phase number
           Begin
                Inc(k);
                Temp^[k] := Buses^[i].DistFromMeter;
           End;
       End;

       // Assign to result and free temp array
       Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (k-1) + 1);
       For i := 0 to k-1 do
          Result[i] := Temp^[i+1];

       Freemem(Temp, SizeOF(Temp^[1])*NumNodes);
     End
    ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_AllNodeVmagByPhase(var ResultPtr: PDouble; var ResultCount: Integer; Phase: Integer);cdecl;
VAR
  Result: PDoubleArray;
   i,k, NodeIdx:Integer;
   Temp:ArrayDef.PDoubleArray;

Begin
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       // Make a Temporary Array big enough to hold all nodes
       Temp := AllocMem(SizeOF(Temp^[1]) * NumNodes);

       // Find nodes connected to specified phase
       k:=0;
       FOR i := 1 to NumBuses DO
       Begin
           NodeIdx := Buses^[i].FindIdx(Phase);
           If NodeIdx > 0 then   // Node found with this phase number
           Begin
                Inc(k);
                Temp^[k] := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(NodeIdx)]);
           End;
       End;

       // Assign to result and free temp array
       Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (k-1) + 1);
       For i := 0 to k-1 do  Result[i] := Temp^[i+1];

       Freemem(Temp, SizeOF(Temp^[1])*NumNodes);
     End
    ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_AllNodeVmagPUByPhase(var ResultPtr: PDouble; var ResultCount: Integer; Phase: Integer);cdecl;
VAR
  Result: PDoubleArray;
   i,k, NodeIdx:Integer;
   Temp:ArrayDef.PDoubleArray;
   BaseFactor :Double;

Begin
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       // Make a Temporary Array big enough to hold all nodes
       Temp := AllocMem(SizeOF(Temp^[1]) * NumNodes);

       // Find nodes connected to specified phase
       k:=0;
       FOR i := 1 to NumBuses DO  Begin
           NodeIdx := Buses^[i].FindIdx(Phase);
           If NodeIdx > 0 then   // Node found with this phase number
           Begin
                If Buses^[i].kVBase >0.0 then BaseFactor :=  1000.0* Buses^[i].kVBase  Else BaseFactor := 1.0;
                Inc(k);
                Temp^[k] := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[Buses^[i].GetRef(NodeIdx)])/Basefactor;
           End;
       End;

       // Assign to result and free temp array
       Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (k-1) + 1);
       For i := 0 to k-1 do  Result[i] := Temp^[i+1];

       Freemem(Temp, SizeOF(Temp^[1])*NumNodes);
     End
    ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_AllNodeNamesByPhase(var ResultPtr: PPAnsiChar; var ResultCount: Integer; Phase: Integer);cdecl;
VAR
  Result: PPAnsiCharArray;
   i,k, NodeIdx:Integer;
   Temp:pStringArray;

Begin
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       // Make a Temporary Array big enough to hold all nodes
       Temp := AllocStringArray(NumNodes);

       // Find nodes connected to specified phase
       k:=0;
       FOR i := 1 to NumBuses DO  Begin
           NodeIdx := Buses^[i].FindIdx(Phase);
           If NodeIdx > 0 then   // Node found with this phase number
           Begin
                Inc(k);
                Temp^[k] := Format('%s.%d',[BusList.Get(i), Phase]);
           End;
       End;

       // Assign to result and free temp array
       Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (k-1) + 1);
       For i := 0 to k-1 do  Result[i] := DSS_CopyStringAsPChar(Temp^[i+1]);

       FreeStringArray(Temp, NumNodes);
     End
    ELSE Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);

end;
//------------------------------------------------------------------------------
function Circuit_SetActiveClass(const ClassName: PAnsiChar):Integer;cdecl;
Var
   DevClassIndex :Integer;

begin
     Result := 0;
     DevClassIndex := ClassNames[ActiveActor].Find(ClassName);
     If DevClassIndex = 0 Then  Begin
        DoSimplemsg('Error: Class ' + ClassName + ' not found.' , 5016);
        Exit;
     End;

     LastClassReferenced[ActiveActor] := DevClassIndex;
     ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
     Result := LastClassReferenced[ActiveActor];
end;
//------------------------------------------------------------------------------
function Circuit_FirstElement():Integer;cdecl;
{ Sets first  element in active class to be active}

Begin

      Result := 0;
      IF (ActiveCircuit[ActiveActor] <> Nil) and Assigned(ActiveDSSClass) THEN
      Begin
         Result := ActiveDSSClass[ActiveActor].First;
      End
        ELSE Result := 0;

end;
//------------------------------------------------------------------------------
function Circuit_NextElement():Integer;cdecl;
{ Sets next  element in active class to be active}

Begin

      Result := 0;
      IF (ActiveCircuit[ActiveActor] <> Nil) and Assigned(ActiveDSSClass[ActiveActor]) THEN
      Begin
         Result := ActiveDSSClass[ActiveActor].Next;
      End
        ELSE Result := 0;

end;
//------------------------------------------------------------------------------
procedure Circuit_UpdateStorage();cdecl;
begin
     StorageClass[ActiveActor].UpdateAll(ActiveActor);
end;
//------------------------------------------------------------------------------
function Circuit_Get_ParentPDElement():Integer;cdecl;
// Make parent PD element the active element if it exists
Var
   ActivePDElement :TPDElement;
begin

   Result := 0;
   With ActiveCircuit[ActiveActor] Do
   If ActiveCktElement is TPDElement Then
   Begin
       ActivePDElement := TPDElement(ActiveCktElement).ParentPDElement;
       If ActivePDElement <> Nil Then
       Begin
         ActiveCktElement :=  ActivePDElement;
         Result := ActivePDElement.ClassIndex;  // should be >0
       End;
   End;

end;
//------------------------------------------------------------------------------
procedure Circuit_EndOfTimeStepUpdate();cdecl;
begin
      EndOfTimeStepCleanup(ActiveActor);
end;
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_YNodeOrder(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
VAR
  Result: PPAnsiCharArray;
   i, k:Integer;

Begin
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumNodes-1) + 1);
       k:=0;
       FOR i := 1 to NumNodes DO
       Begin
             With MapNodeToBus^[i] do
             Result[k] := DSS_CopyStringAsPChar(Format('%s.%-d',[Uppercase(BusList.Get(Busref)), NodeNum]));
             Inc(k);
       End;
     End
    ELSE Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);

end;
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_YCurrents(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray;
   i,k:Integer;
   Curr:Complex;

Begin
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (2*NumNodes-1) + 1);
       k:=0;
       FOR i := 1 to NumNodes DO
       Begin
             Curr := ActiveCircuit[ActiveActor].Solution.Currents^[i];
             Result[k] := Curr.re;
             Inc(k);
             Result[k] := Curr.im;
             Inc(k);
       End;
     End
    ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;
//------------------------------------------------------------------------------
PROCEDURE Circuit_Get_YNodeVarray(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray;
   i,k:Integer;
   Volts:Complex;

Begin
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (2*NumNodes-1) + 1);
       k:=0;
       FOR i := 1 to NumNodes DO
       Begin
             Volts := ActiveCircuit[ActiveActor].Solution.NodeV^[i];
             Result[k] := Volts.re;
             Inc(k);
             Result[k] := Volts.im;
             Inc(k);
       End;
     End
    ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;
//------------------------------------------------------------------------------
procedure Circuit_SetCktElementIndex(const Value: Integer);
begin
   If ActiveCircuit[ActiveActor] <> Nil Then With ActiveCircuit[ActiveActor] Do 
   Begin
      If NumDevices > Value Then
      Begin
         ActiveCktElement := CktElements.Get(Value+1);
      End;
   End;
end;

procedure Circuit_SetCktElementName(const Value: PAnsiChar);
begin
   If ActiveCircuit[ActiveActor] <> Nil Then With ActiveCircuit[ActiveActor] Do 
   Begin
      ActiveCircuit[ActiveActor].SetElementActive(Value); // By name
   End;
end;
//------------------------------------------------------------------------------
END.
