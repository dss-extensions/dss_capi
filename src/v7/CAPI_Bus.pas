UNIT CAPI_Bus;
{$inline on}

INTERFACE

USES CAPI_Utils;

function Bus_Get_Name():PAnsiChar;cdecl;
function Bus_Get_NumNodes():Integer;cdecl;
PROCEDURE Bus_Get_SeqVoltages(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Bus_Get_SeqVoltages_GR();cdecl;
PROCEDURE Bus_Get_Voltages(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Bus_Get_Voltages_GR();cdecl;
PROCEDURE Bus_Get_Nodes(var ResultPtr: PInteger; ResultCount: PInteger);cdecl;
PROCEDURE Bus_Get_Nodes_GR();cdecl;
PROCEDURE Bus_Get_Isc(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Bus_Get_Isc_GR();cdecl;
PROCEDURE Bus_Get_Voc(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Bus_Get_Voc_GR();cdecl;
function Bus_Get_kVBase():Double;cdecl;
PROCEDURE Bus_Get_puVoltages(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Bus_Get_puVoltages_GR();cdecl;
PROCEDURE Bus_Get_Zsc0(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Bus_Get_Zsc0_GR();cdecl;
PROCEDURE Bus_Get_Zsc1(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Bus_Get_Zsc1_GR();cdecl;
PROCEDURE Bus_Get_ZscMatrix(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Bus_Get_ZscMatrix_GR();cdecl;
function Bus_ZscRefresh():WordBool;cdecl;
PROCEDURE Bus_Get_YscMatrix(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Bus_Get_YscMatrix_GR();cdecl;
function Bus_Get_Coorddefined():WordBool;cdecl;
function Bus_Get_x():Double;cdecl;
procedure Bus_Set_x(Value: Double);cdecl;
function Bus_Get_y():Double;cdecl;
procedure Bus_Set_y(Value: Double);cdecl;
function Bus_Get_Distance():Double;cdecl;
function Bus_GetUniqueNodeNumber(StartNumber: Integer):Integer;cdecl;
PROCEDURE Bus_Get_CplxSeqVoltages(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Bus_Get_CplxSeqVoltages_GR();cdecl;
function Bus_Get_Int_Duration():Double;cdecl;
function Bus_Get_Lambda():Double;cdecl;
function Bus_Get_Cust_Duration():Double;cdecl;
function Bus_Get_Cust_Interrupts():Double;cdecl;
function Bus_Get_N_Customers():Integer;cdecl;
function Bus_Get_N_interrupts():Double;cdecl;
PROCEDURE Bus_Get_puVLL(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Bus_Get_puVLL_GR();cdecl;
PROCEDURE Bus_Get_VLL(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Bus_Get_VLL_GR();cdecl;
PROCEDURE Bus_Get_puVmagAngle(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Bus_Get_puVmagAngle_GR();cdecl;
PROCEDURE Bus_Get_VMagAngle(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Bus_Get_VMagAngle_GR();cdecl;
function Bus_Get_TotalMiles():Double;cdecl;
function Bus_Get_SectionID():Integer;cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, Circuit, Ucomplex, MathUtil, sysutils, ExecHelper, SolutionAlgs, Utilities, Bus;

function Bus_Get_Name_AnsiString():AnsiString;inline;
begin
     Result := '';

     If (ActiveCircuit <> Nil) Then With ActiveCircuit Do
     IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
              Result := BusList.Get(ActiveBusIndex);
end;

function Bus_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Bus_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Bus_Get_NumNodes():Integer;cdecl;
begin
     Result := 0;
     If (ActiveCircuit <> Nil) Then With ActiveCircuit Do
     IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Result := ActiveCircuit.Buses^[ActiveCircuit.ActiveBusIndex].NumNodesThisBus;

end;
//------------------------------------------------------------------------------
PROCEDURE Bus_Get_SeqVoltages(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
// Compute sequence voltages for Active Bus
// magnitude only
// returns a set of seq voltages (3)
VAR
  Result: PDoubleArray;
  Nvalues,i, iV:Integer;
  VPh, V012 : Array[1..3] of Complex;

Begin
   IF ActiveCircuit = nil Then Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1)
   End
   ELSE With ActiveCircuit Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      Nvalues := Buses^[ActiveBusIndex].NumNodesThisBus;
      If Nvalues >3 then Nvalues := 3;

      // Assume nodes 1, 2, and 3 are the 3 phases
      Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2) + 1);
      IF Nvalues<>3 THEN
         For i := 1 to 3 DO Result[i-1] := -1.0  // Signify seq voltages n/A for less then 3 phases
      ELSE
      Begin

          iV := 0;
          FOR i := 1 to  3 DO
          Begin
            Vph[i]  := Solution.NodeV^[Buses^[ActiveBusIndex].Find(i)];
          End;

          Phase2SymComp(@Vph, @V012);   // Compute Symmetrical components

          For i := 1 to 3 DO  // Stuff it in the result
          Begin
             Result[iV] := Cabs(V012[i]);
             Inc(iV);
          End;

      End;
   End
  ELSE Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);


end;
PROCEDURE Bus_Get_SeqVoltages_GR();cdecl;
// Same as Bus_Get_SeqVoltages but uses global result (GR) pointers
begin
   Bus_Get_SeqVoltages(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE Bus_Get_Voltages(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
// Return Complex for all nodes of voltages for Active Bus
VAR
  Result: PDoubleArray;
  Nvalues,i,  iV, NodeIdx, jj : Integer;
  Volts : Complex;
  pBus : TDSSBus;

Begin
   IF ActiveCircuit = nil Then Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1)
   End
   ELSE With ActiveCircuit Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      pBus    := Buses^[ActiveBusIndex];
      Nvalues := pBus.NumNodesThisBus;
      Result  := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2*NValues -1) + 1);
      iV := 0;
      jj := 1;
      WITH pBus DO
      FOR i := 1 to  NValues DO
      Begin
            // this code so nodes come out in order from smallest to larges
            Repeat
                 NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                 inc(jj)
            Until NodeIdx>0;

            Volts      := Solution.NodeV^[GetRef(NodeIdx)];  // referenced to pBus
            Result[iV] := Volts.re;
            Inc(iV);
            Result[iV] := Volts.im;
            Inc(iV);
      End;
  End
  ELSE Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);  // just return null array

end;
PROCEDURE Bus_Get_Voltages_GR();cdecl;
// Same as Bus_Get_Voltages but uses global result (GR) pointers
begin
   Bus_Get_Voltages(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE Bus_Get_Nodes(var ResultPtr: PInteger; ResultCount: PInteger);cdecl;
// return array of node numbers corresponding to voltages
VAR
  Result: PIntegerArray;
  Nvalues,i, iV, NodeIdx, jj:Integer;
  pBus : TDSSBus;

Begin
   IF ActiveCircuit = nil Then Begin
        Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (0) + 1)
   End
   ELSE With ActiveCircuit Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      pBus := Buses^[ActiveBusIndex];
      With pBus Do
      Begin
          Nvalues := NumNodesThisBus;
          Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (NValues -1) + 1);
          iV := 0;
          jj := 1;
          FOR i := 1 to  NValues DO
          Begin
                // this code so nodes come out in order from smallest to larges
              Repeat
                   NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                   inc(jj)
              Until NodeIdx>0;
             Result[iV] := Buses^[ActiveBusIndex].GetNum(NodeIdx);
             Inc(iV);
          End;
      End;
  End
  ELSE Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (0) + 1);  // just return null array

end;
PROCEDURE Bus_Get_Nodes_GR();cdecl;
// Same as Bus_Get_Nodes but uses global result (GR) pointers
begin
   Bus_Get_Nodes(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
PROCEDURE Bus_Get_Isc(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
// Return the short circuit current
VAR
  Result: PDoubleArray;
   Isc :Complex;
   i, iV, NValues :Integer;

begin

   IF ActiveCircuit = nil Then Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1)
   End
   ELSE With ActiveCircuit Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      If Buses^[ActiveBusIndex].BusCurrent <> nil Then
      Begin
        NValues := Buses^[ActiveBusIndex].NumNodesThisBus;
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2*NValues -1) + 1);
        iV := 0;
        FOR i := 1 to  NValues DO
        Begin
           Isc := Buses^[ActiveBusIndex].BusCurrent^[i];
           Result[iV] := Isc.Re;
           Inc(iV);
           Result[iV] := Isc.Im;
           Inc(iV);
        End;
      End
      Else
          Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
  End
  ELSE Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1) ;  // just return null array

end;
PROCEDURE Bus_Get_Isc_GR();cdecl;
// Same as Bus_Get_Isc but uses global result (GR) pointers
begin
   Bus_Get_Isc(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE Bus_Get_Voc(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
// Return the Open circuit Voltage for this bus
VAR
  Result: PDoubleArray;
   Voc :Complex;
   i, iV, NValues :Integer;

begin

   IF ActiveCircuit = nil Then Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1)
   End
   ELSE With ActiveCircuit Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      If Buses^[ActiveBusIndex].VBus <> nil Then
      Begin
        NValues := Buses^[ActiveBusIndex].NumNodesThisBus;
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2*NValues -1) + 1);
        iV := 0;
        FOR i := 1 to  NValues DO
        Begin
           Voc := Buses^[ActiveBusIndex].VBus^[i];
           Result[iV] := Voc.Re;
           Inc(iV);
           Result[iV] := Voc.Im;
           Inc(iV);
        End;
      End
      Else
          Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
  End
  ELSE Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1) ;  // just return null array


end;
PROCEDURE Bus_Get_Voc_GR();cdecl;
// Same as Bus_Get_Voc but uses global result (GR) pointers
begin
   Bus_Get_Voc(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function Bus_Get_kVBase():Double;cdecl;
begin
     Result := 0.0;
     If (ActiveCircuit <> Nil) Then With ActiveCircuit Do
     IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Result := ActiveCircuit.Buses^[ActiveCircuit.ActiveBusIndex].kVBase ;

end;
//------------------------------------------------------------------------------
PROCEDURE Bus_Get_puVoltages(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
// Returns voltages at bus in per unit.  However, if kVBase=0, returns actual volts
VAR
  Result: PDoubleArray;
  Nvalues,i, iV,  NodeIdx, jj :Integer;
  Volts:Complex;
  BaseFactor:Double;
  pBus : TDSSBus;

Begin
   IF ActiveCircuit = nil Then Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1)
   End
   ELSE With ActiveCircuit Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      pBus    := Buses^[ActiveBusIndex];
      With pBus Do
      Begin
          Nvalues := NumNodesThisBus;
          Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2*NValues -1) + 1);
          iV := 0;
          jj := 1;
          If kVBase>0.0 Then BaseFactor := 1000.0*kVBase
                        Else BaseFactor := 1.0;
          FOR i := 1 to  NValues DO
          Begin
                // this code so nodes come out in order from smallest to larges
                Repeat
                     NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                     inc(jj)
                Until NodeIdx>0;

                Volts      := Solution.NodeV^[GetRef(NodeIdx)];
                Result[iV] := Volts.re/BaseFactor;
                Inc(iV);
                Result[iV] := Volts.im/BaseFactor;
                Inc(iV);
          End;
      End;
  End
  ELSE Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);  // just return null array

end;
PROCEDURE Bus_Get_puVoltages_GR();cdecl;
// Same as Bus_Get_puVoltages but uses global result (GR) pointers
begin
   Bus_Get_puVoltages(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE Bus_Get_Zsc0(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;  Z:Complex;

begin
    IF ActiveCircuit = nil Then Begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1)
    End
    ELSE With ActiveCircuit Do
    IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
    Begin
            Z := Buses^[ActiveBusIndex].Zsc0;
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (1) + 1);
            Result[0] := Z.Re;
            Result[1] := Z.Im;
    End
    ELSE Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1) ;  // just return null array
end;
PROCEDURE Bus_Get_Zsc0_GR();cdecl;
// Same as Bus_Get_Zsc0 but uses global result (GR) pointers
begin
   Bus_Get_Zsc0(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE Bus_Get_Zsc1(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray; Z:Complex;

begin

    IF ActiveCircuit = nil Then Begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1)
    End
    ELSE With ActiveCircuit Do
    IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
    Begin
            Z := Buses^[ActiveBusIndex].Zsc1;
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (1) + 1);
            Result[0] := Z.Re;
            Result[1] := Z.Im;
    End
    ELSE Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1) ;  // just return null array

end;
PROCEDURE Bus_Get_Zsc1_GR();cdecl;
// Same as Bus_Get_Zsc1 but uses global result (GR) pointers
begin
   Bus_Get_Zsc1(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE Bus_Get_ZscMatrix(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
    Nelements, iV, i, j : Integer;
    Z:Complex;

begin

    IF ActiveCircuit = nil Then Begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1)
    End
    ELSE

    Try

    With ActiveCircuit Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
      Begin
        If Assigned(Buses^[ActiveBusIndex].Zsc) Then Begin
          Nelements := Buses^[ActiveBusIndex].Zsc.Order;
          Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (((2*Nelements*Nelements)-1)) + 1);
          iV := 0;
          With Buses^[ActiveBusIndex] Do
          For i := 1 to Nelements Do
            For j := 1 to Nelements Do  Begin
              Z := Zsc.GetElement(i,j);
              Result[iV] := Z.Re;
              Inc(iV);
              Result[iV] := Z.Im;
              Inc(iV);
            End;

        End
        Else  Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1) ;  // just return null array
      End
      ELSE Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1) ;  // just return null array

      Except
          On E:Exception Do DoSimpleMsg('ZscMatrix Error: ' + E.message + CRLF , 5016);
      End;

end;
PROCEDURE Bus_Get_ZscMatrix_GR();cdecl;
// Same as Bus_Get_ZscMatrix but uses global result (GR) pointers
begin
   Bus_Get_ZscMatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function Bus_ZscRefresh():WordBool;cdecl;
begin

   Result := False;   // Init in case of failure

   If ExecHelper.DoZscRefresh = 0 Then Result := TRUE;

end;
//------------------------------------------------------------------------------
PROCEDURE Bus_Get_YscMatrix(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
    Nelements, iV, i, j : Integer;
    Y1:Complex;

begin

    IF ActiveCircuit = nil Then Begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1)
    End
    ELSE

    Try

    With ActiveCircuit Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
      Begin
        If Assigned(Buses^[ActiveBusIndex].Ysc) Then Begin
          Nelements := Buses^[ActiveBusIndex].Ysc.Order;
          Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (((2*Nelements*Nelements)-1)) + 1);
          iV := 0;
          With Buses^[ActiveBusIndex] Do
          For i := 1 to Nelements Do
            For j := 1 to Nelements Do  Begin
              Y1 := Ysc.GetElement(i,j);
              Result[iV] := Y1.Re;
              Inc(iV);
              Result[iV] := Y1.Im;
              Inc(iV);
            End;

        End
        Else  Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1) ;  // just return null array
      End
      ELSE Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1) ;  // just return null array

      Except
          On E:Exception Do DoSimpleMsg('ZscMatrix Error: ' + E.message + CRLF, 5017 );
      End;


end;
PROCEDURE Bus_Get_YscMatrix_GR();cdecl;
// Same as Bus_Get_YscMatrix but uses global result (GR) pointers
begin
   Bus_Get_YscMatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function Bus_Get_Coorddefined():WordBool;cdecl;
begin
     Result := FALSE;
     If (ActiveCircuit <> Nil) Then With ActiveCircuit Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       IF (Buses^[ActiveCircuit.ActiveBusIndex].Coorddefined) Then Result := TRUE;
end;
//------------------------------------------------------------------------------
function Bus_Get_x():Double;cdecl;
begin
    Result := 0.0;
     If (ActiveCircuit <> Nil) Then With ActiveCircuit Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       IF (Buses^[ActiveCircuit.ActiveBusIndex].Coorddefined) Then
         Result := Buses^[ActiveCircuit.ActiveBusIndex].x;
end;
//------------------------------------------------------------------------------
procedure Bus_Set_x(Value: Double);cdecl;
begin
     If (ActiveCircuit <> Nil) Then With ActiveCircuit Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Begin
         Buses^[ActiveCircuit.ActiveBusIndex].Coorddefined := TRUE;
         Buses^[ActiveCircuit.ActiveBusIndex].x := Value;
       End;
end;
//------------------------------------------------------------------------------
function Bus_Get_y():Double;cdecl;
begin
    Result := 0.0;
     If (ActiveCircuit <> Nil) Then With ActiveCircuit Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       IF (Buses^[ActiveCircuit.ActiveBusIndex].Coorddefined) Then
         Result := Buses^[ActiveCircuit.ActiveBusIndex].y;
end;
//------------------------------------------------------------------------------
procedure Bus_Set_y(Value: Double);cdecl;
begin
   If (ActiveCircuit <> Nil) Then With ActiveCircuit Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Begin
         Buses^[ActiveBusIndex].Coorddefined := TRUE;
         Buses^[ActiveBusIndex].y := Value;
       End;
end;
//------------------------------------------------------------------------------
function Bus_Get_Distance():Double;cdecl;
begin
    Result := 0.0;
    If (ActiveCircuit <> Nil) Then
     With ActiveCircuit Do
      IF ((ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses)) Then
        Result := Buses^[ActiveBusIndex].DistFromMeter;
end;
//------------------------------------------------------------------------------
function Bus_GetUniqueNodeNumber(StartNumber: Integer):Integer;cdecl;
begin
    if ActiveCircuit <> Nil then
    With ActiveCircuit Do

    if ActiveBusIndex > 0 then
      Result := Utilities.GetUniqueNodeNumber(BusList.Get(ActiveBusIndex), StartNumber);
end;
//------------------------------------------------------------------------------
PROCEDURE Bus_Get_CplxSeqVoltages(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
// Compute sequence voltages for Active Bus
// Complex values
// returns a set of seq voltages (3) in 0, 1, 2 order
VAR
  Result: PDoubleArray;
  Nvalues,i, iV:Integer;
  VPh, V012 : Array[1..3] of Complex;

Begin
   IF ActiveCircuit = nil Then Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1)
   End
   ELSE With ActiveCircuit Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      Nvalues := Buses^[ActiveBusIndex].NumNodesThisBus;
      If Nvalues > 3 then Nvalues := 3;

      // Assume nodes labelled 1, 2, and 3 are the 3 phases
      Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (5) + 1);
      IF Nvalues <> 3 THEN
          For i := 1 to 6 DO Result[i-1] := -1.0  // Signify seq voltages n/A for less then 3 phases
      ELSE
      Begin
          iV := 0;
          FOR i := 1 to 3 DO Vph[i] := Solution.NodeV^[Buses^[ActiveBusIndex].Find(i)];

          Phase2SymComp(@Vph, @V012);   // Compute Symmetrical components

          For i := 1 to 3 DO  // Stuff it in the result
          Begin
             Result[iV] := V012[i].re;
             Inc(iV);
             Result[iV] := V012[i].im;
             Inc(iV);
          End;
      End;
   End
  ELSE Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;
PROCEDURE Bus_Get_CplxSeqVoltages_GR();cdecl;
// Same as Bus_Get_CplxSeqVoltages but uses global result (GR) pointers
begin
   Bus_Get_CplxSeqVoltages(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function Bus_Get_Int_Duration():Double;cdecl;
begin
  Result := 0.0;
  if ActiveCircuit <> Nil then
    With ActiveCircuit Do
      if ActiveBusIndex > 0 then
         Result := Buses^[ActiveBusIndex].Bus_Int_Duration;
end;
//------------------------------------------------------------------------------
function Bus_Get_Lambda():Double;cdecl;
begin
  Result := 0.0;
  if ActiveCircuit <> Nil then
    With ActiveCircuit Do
      if ActiveBusIndex > 0 then
         Result := Buses^[ActiveBusIndex].BusFltRate;
end;
//------------------------------------------------------------------------------
function Bus_Get_Cust_Duration():Double;cdecl;
begin
  Result := 0.0;
  if ActiveCircuit <> Nil then
    With ActiveCircuit Do
      if ActiveBusIndex > 0 then
         Result := Buses^[ActiveBusIndex].BusCustDurations ;
end;
//------------------------------------------------------------------------------
function Bus_Get_Cust_Interrupts():Double;cdecl;
begin
  Result := 0.0;
  if ActiveCircuit <> Nil then
    With ActiveCircuit Do
      if ActiveBusIndex > 0 then
         Result := Buses^[ActiveBusIndex].BusCustDurations ;
end;
//------------------------------------------------------------------------------
function Bus_Get_N_Customers():Integer;cdecl;
begin
  Result := 0;
  if ActiveCircuit <> Nil then
    With ActiveCircuit Do
      if ActiveBusIndex > 0 then
         Result := Buses^[ActiveBusIndex].BusTotalNumCustomers  ;
end;
//------------------------------------------------------------------------------
function Bus_Get_N_interrupts():Double;cdecl;
begin
  Result := 0.0;
  if ActiveCircuit <> Nil then
    With ActiveCircuit Do
      if ActiveBusIndex > 0 then
         Result := Buses^[ActiveBusIndex].Bus_Num_Interrupt ;
end;
//------------------------------------------------------------------------------
PROCEDURE Bus_Get_puVLL(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
  Nvalues,i,  iV, NodeIdxi, NodeIdxj, jj : Integer;
  Volts : Complex;
  pBus : TDSSBus;
  BaseFactor : double;

Begin
   IF ActiveCircuit = nil Then Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1)
   End
   ELSE With ActiveCircuit Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      pBus    := Buses^[ActiveBusIndex];
      Nvalues := pBus.NumNodesThisBus;
      If Nvalues > 3 Then Nvalues := 3;

      If Nvalues > 1 Then
      Begin
          If Nvalues = 2 Then  Nvalues := 1;  // only one L-L voltage if 2 phase
          Result  := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2*NValues -1) + 1);
          iV := 0;
          WITH pBus DO
          Begin
            If kVBase>0.0 Then BaseFactor := 1000.0*kVBase*sqrt3
                          Else BaseFactor := 1.0;
            FOR i := 1 to  NValues DO     // for 2- or 3-phases
            Begin
                  // this code assumes the nodes are ordered 1, 2, 3
                  NodeIdxi := FindIdx(i);  // Get the index of the Node that matches i
                  jj := i+1;
                  if jj>3 then jj := 1; // wrap around
                  NodeIdxj := FindIdx(jj);

                  With Solution Do Volts := Csub(NodeV^[GetRef(NodeIdxi)], NodeV^[GetRef(NodeIdxj)]);
                  Result[iV] := Volts.re / BaseFactor;
                  Inc(iV);
                  Result[iV] := Volts.im / BaseFactor;
                  Inc(iV);
            End;
          End;  {With pBus}
      End
      ELSE Begin  // for 1-phase buses, do not attempt to compute.
          Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (1) + 1);  // just return -1's in array
          Result[0] := -99999.0;
          Result[1] := 0.0;
      End;
  End
  ELSE Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);  // just return null array


end;
PROCEDURE Bus_Get_puVLL_GR();cdecl;
// Same as Bus_Get_puVLL but uses global result (GR) pointers
begin
   Bus_Get_puVLL(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE Bus_Get_VLL(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
  Nvalues,i,  iV, NodeIdxi, NodeIdxj, jj : Integer;
  Volts : Complex;
  pBus : TDSSBus;

Begin
   IF ActiveCircuit = nil Then Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1)
   End
   ELSE With ActiveCircuit Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      pBus    := Buses^[ActiveBusIndex];
      Nvalues := pBus.NumNodesThisBus;
      If Nvalues > 3 Then Nvalues := 3;

      If Nvalues > 1 Then
      Begin
          If Nvalues = 2 Then  Nvalues := 1;  // only one L-L voltage if 2 phase
          Result  := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2*NValues -1) + 1);
          iV := 0;
          WITH pBus DO
            FOR i := 1 to  NValues DO     // for 2- or 3-phases
            Begin

                  // this code assumes the nodes are ordered 1, 2, 3
                  // this code so nodes come out in order from smallest to largest
                  NodeIdxi := FindIdx(i);  // Get the index of the Node that matches i
                  jj := i+1;
                  if jj>3 then jj := 1; // wrap around
                  NodeIdxj := FindIdx(jj);

                  With Solution Do Volts := Csub(NodeV^[GetRef(NodeIdxi)], NodeV^[GetRef(NodeIdxj)]);
                  Result[iV] := Volts.re;
                  Inc(iV);
                  Result[iV] := Volts.im;
                  Inc(iV);
            End;
      End
      ELSE Begin  // for 1-phase buses, do not attempt to compute.
          Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (1) + 1);  // just return -1's in array
          Result[0] := -99999.0;
          Result[1] := 0.0;
      End;
  End
  ELSE Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);  // just return null array

end;
PROCEDURE Bus_Get_VLL_GR();cdecl;
// Same as Bus_Get_VLL but uses global result (GR) pointers
begin
   Bus_Get_VLL(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE Bus_Get_puVmagAngle(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
// Return mag/angle for all nodes of voltages for Active Bus
VAR
  Result: PDoubleArray;
  Nvalues,i,  iV, NodeIdx, jj : Integer;
  Volts : polar;
  pBus : TDSSBus;
  Basefactor : Double;

Begin

   IF ActiveCircuit = nil Then Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1)
   End
   ELSE With ActiveCircuit Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      pBus    := Buses^[ActiveBusIndex];
      Nvalues := pBus.NumNodesThisBus;
      Result  := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2*NValues -1) + 1);
      iV := 0;
      jj := 1;
      WITH pBus DO Begin
          If kVBase>0.0 Then BaseFactor := 1000.0 * kVBase
                        Else BaseFactor := 1.0;

          FOR i := 1 to  NValues DO
          Begin
                // this code so nodes come out in order from smallest to larges
                Repeat
                      NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                      inc(jj)
                Until NodeIdx>0;

                Volts      := ctopolardeg(Solution.NodeV^[GetRef(NodeIdx)]);  // referenced to pBus
                Result[iV] := Volts.mag / BaseFactor;
                Inc(iV);
                Result[iV] := Volts.ang;
                Inc(iV);
          End;
      End;
  End
  ELSE Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);  // just return null array

end;
PROCEDURE Bus_Get_puVmagAngle_GR();cdecl;
// Same as Bus_Get_puVmagAngle but uses global result (GR) pointers
begin
   Bus_Get_puVmagAngle(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE Bus_Get_VMagAngle(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
// Return mag/angle for all nodes of voltages for Active Bus
VAR
  Result: PDoubleArray;
  Nvalues,i,  iV, NodeIdx, jj : Integer;
  Volts : polar;
  pBus : TDSSBus;

Begin

   IF ActiveCircuit = nil Then Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1)
   End
   ELSE With ActiveCircuit Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      pBus    := Buses^[ActiveBusIndex];
      Nvalues := pBus.NumNodesThisBus;
      Result  := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2*NValues -1) + 1);
      iV := 0;
      jj := 1;
      WITH pBus DO
      FOR i := 1 to  NValues DO
      Begin
            // this code so nodes come out in order from smallest to larges
            Repeat
                 NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                 inc(jj)
            Until NodeIdx>0;

            Volts      := ctopolardeg(Solution.NodeV^[GetRef(NodeIdx)]);  // referenced to pBus
            Result[iV] := Volts.mag;
            Inc(iV);
            Result[iV] := Volts.ang;
            Inc(iV);
      End;
  End
  ELSE Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);  // just return null array

end;
PROCEDURE Bus_Get_VMagAngle_GR();cdecl;
// Same as Bus_Get_VMagAngle but uses global result (GR) pointers
begin
   Bus_Get_VMagAngle(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function Bus_Get_TotalMiles():Double;cdecl;
begin
  Result := 0.0;
  if ActiveCircuit <> Nil then
    With ActiveCircuit Do
      if ActiveBusIndex > 0 then
         Result := Buses^[ActiveBusIndex].BusTotalMiles  ;
end;
//------------------------------------------------------------------------------
function Bus_Get_SectionID():Integer;cdecl;
begin
  Result := 0;
  if ActiveCircuit <> Nil then
    With ActiveCircuit Do
      if ActiveBusIndex > 0 then
         Result := Buses^[ActiveBusIndex].BusSectionID  ;
end;
//------------------------------------------------------------------------------
END.
