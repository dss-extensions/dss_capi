UNIT CAPI_CktElement;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE CktElement_Get_BusNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
function CktElement_Get_Name():PAnsiChar;cdecl;
function CktElement_Get_NumConductors():Integer;cdecl;
function CktElement_Get_NumPhases():Integer;cdecl;
function CktElement_Get_NumTerminals():Integer;cdecl;
procedure CktElement_Set_BusNames(ValuePtr: PPAnsiChar; ValueCount: Integer);cdecl;
PROCEDURE CktElement_Get_Currents(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
PROCEDURE CktElement_Get_Voltages(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
function CktElement_Get_EmergAmps():Double;cdecl;
function CktElement_Get_Enabled():WordBool;cdecl;
PROCEDURE CktElement_Get_Losses(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
function CktElement_Get_NormalAmps():Double;cdecl;
PROCEDURE CktElement_Get_PhaseLosses(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
PROCEDURE CktElement_Get_Powers(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
PROCEDURE CktElement_Get_SeqCurrents(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
PROCEDURE CktElement_Get_SeqPowers(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
PROCEDURE CktElement_Get_SeqVoltages(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
procedure CktElement_Close(Term, Phs: Integer);cdecl;
procedure CktElement_Open(Term, Phs: Integer);cdecl;
procedure CktElement_Set_EmergAmps(Value: Double);cdecl;
procedure CktElement_Set_Enabled(Value: WordBool);cdecl;
procedure CktElement_Set_NormalAmps(Value: Double);cdecl;
function CktElement_IsOpen(Term, Phs: Integer):WordBool;cdecl;
PROCEDURE CktElement_Get_AllPropertyNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
function CktElement_Get_NumProperties():Integer;cdecl;
PROCEDURE CktElement_Get_Residuals(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
PROCEDURE CktElement_Get_Yprim(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
function CktElement_Get_DisplayName():PAnsiChar;cdecl;
function CktElement_Get_GUID():PAnsiChar;cdecl;
function CktElement_Get_Handle():Integer;cdecl;
procedure CktElement_Set_DisplayName(const Value: PAnsiChar);cdecl;
function CktElement_Get_Controller(idx: Integer):PAnsiChar;cdecl;
function CktElement_Get_EnergyMeter():PAnsiChar;cdecl;
function CktElement_Get_HasVoltControl():WordBool;cdecl;
function CktElement_Get_HasSwitchControl():WordBool;cdecl;
PROCEDURE CktElement_Get_CplxSeqVoltages(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
PROCEDURE CktElement_Get_CplxSeqCurrents(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
PROCEDURE CktElement_Get_AllVariableNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
PROCEDURE CktElement_Get_AllVariableValues(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
function CktElement_Get_Variable(const MyVarName: PAnsiChar;  out Code: Integer):Double;cdecl;
function CktElement_Get_Variablei(Idx: Integer;  out Code: Integer):Double;cdecl;
PROCEDURE CktElement_Get_NodeOrder(var ResultPtr: PInteger; var ResultCount: Integer);cdecl;
function CktElement_Get_HasOCPDevice():WordBool;cdecl;
function CktElement_Get_NumControls():Integer;cdecl;
function CktElement_Get_OCPDevIndex():Integer;cdecl;
function CktElement_Get_OCPDevType():Integer;cdecl;
PROCEDURE CktElement_Get_CurrentsMagAng(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
PROCEDURE CktElement_Get_VoltagesMagAng(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSClassDefs, DSSGlobals, UComplex, Sysutils, PDElement, PCElement, MathUtil, CktElement, Utilities;

Procedure CalcSeqCurrents(pActiveElement:TDSSCktElement; i012:pComplexArray);
{Assumes V012 is properly allocated before call.}
VAR
    Nvalues,i,j,k,iV  :Integer;
    IPh, I012a        :Array[1..3] of Complex;
    cBuffer:pComplexArray;

BEGIN
    With pActiveElement, ActiveCircuit Do BEGIN
      Nvalues := NPhases;
      IF Nvalues <> 3 THEN Begin
        {Handle non-3 phase elements}
           IF (Nphases = 1) and PositiveSequence THEN
           Begin
                NValues := NConds*NTerms;
                cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
                GetCurrents(cBuffer);

                For i := 1 to  3*NTerms DO i012^[i] := CZERO;   // Initialize Result
                iV := 2;  // pos seq is 2nd element in array
                {Populate only phase 1 quantities in Pos seq}
                FOR j := 1 to NTerms Do Begin
                    k := (j - 1) * NConds;
                    i012^[iV] := cBuffer^[1 + k];
                    Inc(iV, 3);  // inc to pos seq of next terminal
                End;
                Reallocmem(cBuffer, 0);
           END
           // if neither 3-phase or pos seq model, just put in -1.0 for each element
           ELSE  For i := 1 to  3*NTerms Do i012^[i] := Cmplx(-1.0, 0.0);  // Signify n/A
      End
      ELSE Begin    // for 3-phase elements
           iV := 1;
           NValues := NConds * NTerms;
           cBuffer := Allocmem(sizeof(cBuffer^[1]) * NValues);
           GetCurrents(cBuffer);
           FOR j := 1 to NTerms Do
           Begin
                k := (j-1)*NConds;
                For i := 1 to  3 DO Iph[i] := cBuffer^[k+i];
                Phase2SymComp(@Iph, @I012a);

                For i := 1 to 3 DO  Begin     // Stuff it in the result array
                   i012^[iV] := i012a[i];
                   Inc(iV);
                End;
           End;
           Reallocmem(cBuffer, 0);
      End;
    END;
END;


{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
Procedure CalcSeqVoltages(pActiveElement:TDSSCktElement; V012:pComplexArray);
{Assumes V012 is properly allocated before call.}
VAR
    Nvalues,i,j,k,iV  :Integer;
    VPh, V012a        :Array[1..3] of Complex;
BEGIN
    With pActiveElement, ActiveCircuit Do BEGIN
      Nvalues := NPhases;
      IF Nvalues <> 3 THEN Begin
        {Handle non-3 phase elements}
           IF (Nphases = 1) and PositiveSequence THEN
           Begin
                For i := 1 to  3*NTerms DO V012^[i] := CZERO;   // Initialize Result
                iV := 2;  // pos seq is 2nd element in array
                {Populate only phase 1 quantities in Pos seq}
                FOR j := 1 to NTerms Do Begin
                    k := (j - 1) * NConds;
                    V012^[iV] := Solution.NodeV^[NodeRef^[1 + k]];
                    Inc(iV, 3);  // inc to pos seq of next terminal
                End;
           END
           // if neither 3-phase or pos seq model, just put in -1.0 for each element
           ELSE  For i := 1 to  3*NTerms Do V012^[i] := Cmplx(-1.0, 0.0);  // Signify n/A
      End
      ELSE Begin    // for 3-phase elements
           iV := 1;
           FOR j := 1 to NTerms Do
           Begin
                k :=(j-1)*NConds;
                FOR i := 1 to  3 DO Vph[i] := Solution.NodeV^[NodeRef^[i+k]];
                Phase2SymComp(@Vph, @V012a);   // Compute Symmetrical components

                For i := 1 to 3 DO  Begin     // Stuff it in the result array
                   V012^[iV] := V012a[i];
                   Inc(iV);
                End;
           End;
      End;
    END;
END;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
FUNCTION IsPDElement : Boolean;
Begin
    Result :=  ((ActiveCircuit.ActiveCktElement.DSSObjType and 3) = PD_ELEMENT)
End;
{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
PROCEDURE CktElement_Get_BusNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
VAR
  Result: PPAnsiCharArray;
   i :Integer;
begin

     If ActiveCircuit <> Nil Then
     Begin
       With ActiveCircuit Do Begin
         Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (ActiveCktElement.Nterms-1) + 1);
         For i := 1 to  ActiveCktElement.Nterms Do Begin
             Result[i-1] := DSS_CopyStringAsPChar(ActiveCktElement.GetBus(i));
         End;
       End;
     End
     Else
         Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
function CktElement_Get_Name_AnsiString():AnsiString;inline;
begin
   If ActiveCircuit <> Nil Then
      WITH ActiveCircuit.ActiveCktElement DO
      Begin
        Result := ParentClass.Name + '.' + Name;
      End
   Else
      Result := '';
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

function CktElement_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(CktElement_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function CktElement_Get_NumConductors():Integer;cdecl;
begin
   If ActiveCircuit <> Nil Then
        Result := ActiveCircuit.ActiveCktElement.NConds
   Else Result := 0;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
function CktElement_Get_NumPhases():Integer;cdecl;
begin
   If ActiveCircuit <> Nil Then
        Result := ActiveCircuit.ActiveCktElement.NPhases
   Else Result := 0;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
function CktElement_Get_NumTerminals():Integer;cdecl;
begin
   If ActiveCircuit <> Nil Then
        Result := ActiveCircuit.ActiveCktElement.NTerms
   Else Result := 0;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
procedure CktElement_Set_BusNames(ValuePtr: PPAnsiChar; ValueCount: Integer);cdecl;
VAR
  Value: PPAnsiCharArray;
   i :Integer;
   Count, Low :Integer;
begin
    Value := PPAnsiCharArray(ValuePtr);

     If ActiveCircuit <> Nil Then
     Begin
       With ActiveCircuit Do Begin
         Low := (0);
         Count := (ValueCount - 1) - Low + 1;
         If Count >  ActiveCktElement.NTerms Then Count := ActiveCktElement.NTerms;
         For i := 1 to Count Do Begin
             ActiveCktElement.SetBus(i, Value[i-1 + Low]);
         End;
       End;
     End;
end;



{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
PROCEDURE CktElement_Get_Currents(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray;
  cBuffer: pComplexArray;
  NValues, iV ,i: Integer;

Begin
  If ActiveCircuit <> Nil Then
     WITH ActiveCircuit.ActiveCktElement DO
     Begin
         NValues := NConds*NTerms;
         Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (2*NValues-1) + 1);
         cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
         GetCurrents(cBuffer);
         iV :=0;
         For i := 1 to  NValues DO
         Begin
            Result[iV] := cBuffer^[i].re;
            Inc(iV);
            Result[iV] := cBuffer^[i].im;
            Inc(iV);
         End;
         Reallocmem(cBuffer,0);
     End
  Else
     Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
PROCEDURE CktElement_Get_Voltages(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
// Bus Voltages at active terminal
VAR
  Result: PDoubleArray;
  numcond, i,n,iV:Integer;
  Volts:Complex;

Begin

// Return voltages for all terminals

     IF ActiveCircuit <> Nil THEN
      WITH ActiveCircuit DO
      Begin
        If ActiveCktElement<>Nil THEN
        WITH ActiveCktElement DO
        Begin
         numcond := NConds*Nterms;
         Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (2*numcond-1) + 1);
         // k := (Terminal-1)*numcond;    // RCD 8-30-00 Changed
         iV :=0;
         FOR i := 1 to  numcond DO
         Begin
            n := ActiveCktElement.NodeRef^[i];
            Volts := Solution.NodeV^[n]; // ok if =0
            Result[iV] := Volts.re;
            Inc(iV);
            Result[iV] := Volts.im;
            Inc(iV);
         End;
        End;
      End
    ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
function CktElement_Get_EmergAmps():Double;cdecl;
begin
     If ActiveCircuit <> Nil Then
     With ActiveCircuit Do
     Begin
         If (ActiveCktElement.DSSObjType and 3) = PD_ELEMENT Then
         Begin
             With ActiveCktElement As TPDElement Do Result := EmergAmps ;
         End
         Else Result := 0.0;
     End;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
function CktElement_Get_Enabled():WordBool;cdecl;
Begin

   If ActiveCircuit <> Nil Then
      Result := ActiveCircuit.ActiveCktElement.Enabled
   Else
       Result := False;

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
PROCEDURE CktElement_Get_Losses(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray;
   LossValue :complex;
begin

     IF ActiveCircuit <> Nil THEN
      WITH ActiveCircuit DO
      Begin
        If ActiveCktElement<>Nil THEN
        Begin
         Result    := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (1) + 1);
         LossValue := ActiveCktElement.Losses;
         Result[0] := LossValue.re;
         Result[1] := LossValue.im;
        End;
      End
    ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
function CktElement_Get_NormalAmps():Double;cdecl;
begin
     If ActiveCircuit <> Nil Then
     With ActiveCircuit Do
     Begin
         If (ActiveCktElement.DSSObjType and 3) = PD_ELEMENT Then
         Begin
             With ActiveCktElement As TPDElement Do Result := NormAmps ;
         End
         Else Result := 0.0;
     End;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
PROCEDURE CktElement_Get_PhaseLosses(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
// Returns Phase losses in kW, kVar
VAR
  Result: PDoubleArray;
  cBuffer:pComplexArray;
  NValues,  i, iV : Integer;

Begin


 IF ActiveCircuit <> Nil THEN

  WITH ActiveCircuit.ActiveCktElement DO
  Begin
      NValues := NPhases;
      Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (2*NValues-1) + 1);
      cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
      GetPhaseLosses( NValues, cBuffer);
      iV :=0;
      For i := 1 to  NValues DO Begin
           Result[iV] := cBuffer^[i].re*0.001;
           Inc(iV);
           Result[iV] := cBuffer^[i].im*0.001;
           Inc(iV);
      End;
      Reallocmem(cBuffer,0);
  End
 ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
PROCEDURE CktElement_Get_Powers(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
// Return complex kW, kvar in each conductor for each terminal
VAR
  Result: PDoubleArray;
   cBuffer:pComplexArray;
   NValues,
   i,
   iV : Integer;

Begin

 IF ActiveCircuit <> Nil THEN
  WITH ActiveCircuit.ActiveCktElement DO
  Begin
      NValues := NConds*Nterms;
      Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (2*NValues-1) + 1);
      cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
      GetPhasePower(cBuffer);
      iV :=0;
      For i := 1 to  NValues DO Begin
           Result[iV] := cBuffer^[i].re*0.001;
           Inc(iV);
           Result[iV] := cBuffer^[i].im*0.001;
           Inc(iV);
      End;
      Reallocmem(cBuffer,0);
  End
 ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
PROCEDURE CktElement_Get_SeqCurrents(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
// All sequence currents of active ciruit element
// returns magnitude only.
VAR
  Result: PDoubleArray;
    i  :Integer;
    i012 :pComplexArray;
    S :String;

Begin
  IF   ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
     If ActiveCktElement <> Nil THEN
     WITH ActiveCktElement DO
     If Enabled Then
     Begin
         TRY
            Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (3*NTerms-1) + 1);

            i012 := Allocmem(Sizeof(i012^[1]) * 3 * Nterms);
            // get complex seq voltages
            CalcSeqCurrents(ActiveCktElement, i012);
            // return 0 based array
            For i := 1 to 3*Nterms do Result[i-1] := Cabs(i012^[i]);  // return mag only

            Reallocmem(i012, 0);  // throw away temp memory

          EXCEPT
             On E:Exception Do
             Begin
                S:= E.message + CRLF +
                    'Element='+ ActiveCktElement.Name + CRLF+
                    'Nphases=' + IntToStr(Nphases) + CRLF +
                    'NTerms=' + IntToStr(NTerms) + CRLF +
                    'NConds =' + IntToStr(NConds);
                DoSimpleMsg(S, 5012);
              End;
          END;
     End
     Else
         Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);  // Disabled

   End
  ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
PROCEDURE CktElement_Get_SeqPowers(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
// All seq Powers of active 3-phase ciruit element
// returns kW + j kvar
VAR
  Result: PDoubleArray;
  Nvalues,i,j,k,n, icount:Integer;
  S:Complex;
  VPh, V012 : Array[1..3] of Complex;
  IPh, I012 : Array[1..3] of Complex;
  cBuffer:pComplexArray;

Begin

 IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO Begin
     If ActiveCktElement<>Nil THEN
     WITH ActiveCktElement DO Begin
      Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (2*3*NTerms-1) + 1); // allocate for kW and kvar
      IF NPhases <> 3 THEN
      Begin
           IF (Nphases = 1) and PositiveSequence THEN
           Begin
                NValues := NConds*NTerms;
                cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
                GetCurrents(cBuffer);

                For i := 0 to  2*3*NTerms-1 DO Result[i] := 0.0;   // Initialize Result
                iCount := 2;  // Start with kW1
                {Put only phase 1 quantities in Pos seq}
                FOR j := 1 to NTerms Do
                Begin
                    k := (j-1)*NConds;
                    n := NodeRef^[k+1];
                    Vph[1] := Solution.NodeV^[n];  // Get voltage at node
                    S := Cmul(Vph[1], conjg(cBuffer^[k+1]));   // Compute power per phase
                    Result[icount] := S.re*0.003; // 3-phase kW conversion
                    inc(icount);
                    Result[icount] := S.im*0.003; // 3-phase kvar conversion
                    inc(icount, 6);
                End;
                Reallocmem(cBuffer,0);
           END

           ELSE  For i := 0 to  2*3*NTerms-1 DO Result[i] := -1.0;  // Signify n/A
      END
      ELSE Begin
          NValues := NConds*NTerms;
          cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
          GetCurrents(cBuffer);
          icount := 0;
          FOR j := 1 to NTerms Do Begin
             k :=(j-1)*NConds;
             FOR i := 1 to  3 DO Vph[i] := Solution.NodeV^[NodeRef^[i+k]];
             For i := 1 to  3 DO Iph[i] := cBuffer^[k+i];
             Phase2SymComp(@Iph, @I012);
             Phase2SymComp(@Vph, @V012);
             For i := 1 to 3 DO  Begin
                 S := Cmul(V012[i], conjg(I012[i]));
                 Result[icount] := S.re*0.003; // 3-phase kW conversion
                 inc(icount);
                 Result[icount] := S.im*0.003; // 3-phase kW conversion
                 inc(icount);
             End;
          End;
          Reallocmem(cBuffer,0);
      End;
     End;
   End
 ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
PROCEDURE CktElement_Get_SeqVoltages(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
// All voltages of active ciruit element
// magnitude only
// returns a set of seq voltages (3) for each terminal
// 0, 1, 2 sequence  (0, +, -)
VAR
  Result: PDoubleArray;
    i  :Integer;
    V012 :pComplexArray;
    S :String;

Begin
  IF   ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
     If ActiveCktElement <> Nil THEN
     WITH ActiveCktElement DO
     If Enabled Then
     Begin
         TRY
            Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (3*NTerms-1) + 1);

            V012 := Allocmem(Sizeof(V012^[1]) * 3 * Nterms);
            // get complex seq voltages
            CalcSeqVoltages(ActiveCktElement, V012);
            // return 0 based array
            For i := 1 to 3*Nterms do Result[i-1] := Cabs(V012^[i]);  // return mag only

            Reallocmem(V012, 0);  // throw away temp memory

          EXCEPT
             On E:Exception Do
             Begin
                S:= E.message + CRLF +
                    'Element='+ ActiveCktElement.Name + CRLF+
                    'Nphases=' + IntToStr(Nphases) + CRLF +
                    'NTerms=' + IntToStr(NTerms) + CRLF +
                    'NConds =' + IntToStr(NConds);
                DoSimpleMsg(S, 5012);
              End;
          END;
     End
     Else
         Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);  // Disabled

   End
  ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
procedure CktElement_Close(Term, Phs: Integer);cdecl;
Begin

   IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
      If ActiveCktElement<>nil THEN
      WITH ActiveCktElement DO
      Begin
        ActiveTerminal := Terminals^[Term];
        Closed[Phs] := TRUE;
      End;
   End;

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
procedure CktElement_Open(Term, Phs: Integer);cdecl;
Begin
   IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
      If ActiveCktElement<>nil THEN
      WITH ActiveCktElement DO
      Begin
        ActiveTerminal := Terminals^[Term];
        Closed[Phs] := FALSE;
      End;
   End;

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
procedure CktElement_Set_EmergAmps(Value: Double);cdecl;
begin

     If ActiveCircuit <> Nil Then
     With ActiveCircuit Do
     Begin
         If IsPDElement Then
         Begin
             With ActiveCktElement As TPDElement Do EmergAmps := Value;
         End;  {Else Do Nothing}
     End;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
procedure CktElement_Set_Enabled(Value: WordBool);cdecl;
begin
   If ActiveCircuit <> Nil Then
      ActiveCircuit.ActiveCktElement.Enabled := Value;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
procedure CktElement_Set_NormalAmps(Value: Double);cdecl;
begin
     If ActiveCircuit <> Nil Then
     Begin
         If IsPDElement Then
         Begin
             With ActiveCircuit Do With ActiveCktElement As TPDElement Do NormAmps := Value;
         End;  {Else Do Nothing}
     End;
end;
//------------------------------------------------------------------------------
function CktElement_IsOpen(Term, Phs: Integer):WordBool;cdecl;
Var
   i  :Integer;

begin
     If ActiveCircuit <> Nil Then
     With ActiveCircuit Do
     Begin
         With ActiveCktElement Do ActiveTerminal := Terminals^[Term];
         If Phs=0 Then // At least one must be open
         Begin
             Result := False;
             For i := 1 to ActiveCktElement.NConds Do
                 If not ActiveCktElement.Closed[i] Then
                 Begin
                    Result :=  True;
                    Exit;
                 End;
         End
         Else // Check a specific phase or conductor
         Begin
             Result := Not ActiveCktElement.Closed[Phs];
         End;
     End;
end;
//------------------------------------------------------------------------------
PROCEDURE CktElement_Get_AllPropertyNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
VAR
  Result: PPAnsiCharArray;
   k:Integer;
begin
  Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
  IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
     If ActiveCktElement<>Nil THEN
     WITH ActiveCktElement DO
     Begin
          WITH ParentClass Do
          Begin
              Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumProperties-1) + 1);
              For k := 1 to NumProperties DO Begin
                  Result[k-1] := DSS_CopyStringAsPChar(PropertyName^[k]);
              End;
          End;
     End
   End;
end;
//------------------------------------------------------------------------------
function CktElement_Get_NumProperties():Integer;cdecl;
begin
  Result := 0;
  IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
     If ActiveCktElement<>Nil THEN
     WITH ActiveCktElement DO
     Begin
          Result := ParentClass.NumProperties ;
     End
   End;


end;
//------------------------------------------------------------------------------
PROCEDURE CktElement_Get_Residuals(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray;
  cBuffer       :pComplexArray;
  iV ,i, j, k   :Integer;
  cResid        :Complex;

Begin

  If ActiveCircuit <> Nil Then
     WITH ActiveCircuit.ActiveCktElement DO
     Begin
         Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (2*NTerms-1) + 1);    // 2 values per terminal
         cBuffer := Allocmem(sizeof(cBuffer^[1])*Yorder);
         GetCurrents(cBuffer);
         iV :=0;
         For i := 1 to  NTerms DO
         Begin
            cResid := CZERO;
            k :=(i-1)*Nconds;
            For j := 1 to Nconds Do Begin
                inc(k);
                Caccum(cResid, CBuffer^[k]);
            End;
            Result[iV] := Cabs(cResid);
            Inc(iV);
            Result[iV] := CDang(cResid);
            Inc(iV);
         End;
         Reallocmem(cBuffer,0);
     End
  Else
     Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;
//------------------------------------------------------------------------------
PROCEDURE CktElement_Get_Yprim(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
{ Return the YPrim matrix for this element }

VAR
  Result: PDoubleArray;
   iV      : Integer;
   i       : Integer;
   NValues : Integer;
   cValues : pComplexArray;

begin
   IF ActiveCircuit = nil Then Begin
        Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
   End
   ELSE With ActiveCircuit Do
      If ActiveCktElement<>Nil THEN
      WITH ActiveCktElement Do  Begin
          NValues := SQR(Yorder);
          cValues := GetYprimValues(ALL_YPRIM);  // Get pointer to complex array of values
          If cValues=Nil Then Begin   // check for unassigned array
                            Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);  // just return null array
                            Exit;  // Get outta here
                         End;
          Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (2*NValues -1) + 1);  // Make variant array
          iV := 0;

          FOR i := 1 to  NValues DO  Begin    // Plunk the values in the variant array
              Result[iV] := cValues^[i].re;
              Inc(iV);
              Result[iV] := cValues^[i].im;
              Inc(iV);
          End;
      End
      ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);  // just return null array

end;
//------------------------------------------------------------------------------
function CktElement_Get_DisplayName_AnsiString():AnsiString;inline;
begin
   If ActiveCircuit <> Nil Then
      Result := ActiveCircuit.ActiveCktElement.DisplayName
   Else
      Result := '';
end;

function CktElement_Get_DisplayName():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(CktElement_Get_DisplayName_AnsiString());
end;
//------------------------------------------------------------------------------
function CktElement_Get_GUID_AnsiString():AnsiString;inline;
begin
   If ActiveCircuit <> Nil Then
      Result := ActiveCircuit.ActiveCktElement.ID
   Else
      Result := '';
end;

function CktElement_Get_GUID():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(CktElement_Get_GUID_AnsiString());
end;
//------------------------------------------------------------------------------
function CktElement_Get_Handle():Integer;cdecl;
begin
   If ActiveCircuit <> Nil Then
      Result := ActiveCircuit.ActiveCktElement.Handle
   Else
      Result := 0;
end;
//------------------------------------------------------------------------------
procedure CktElement_Set_DisplayName(const Value: PAnsiChar);cdecl;
begin
   If ActiveCircuit <> Nil Then
      ActiveCircuit.ActiveCktElement.DisplayName := Value;
end;
//------------------------------------------------------------------------------
function CktElement_Get_Controller_AnsiString(idx: Integer):AnsiString;inline;
var
  ctrl: TDSSCktElement;
begin
  Result := '';
  If ActiveCircuit <> Nil Then With ActiveCircuit Do begin
    If (idx>0) and (idx <= ActiveCktElement.ControlElementList.Listsize) Then
    Begin
      ctrl := ActiveCktElement.ControlElementList.Get(idx);
      If ctrl <> Nil Then
        Result := Format('%s.%s', [ctrl.ParentClass.Name, ctrl.Name]);
    End;
  end;
end;

function CktElement_Get_Controller(idx: Integer):PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(CktElement_Get_Controller_AnsiString(idx));
end;
//------------------------------------------------------------------------------
function CktElement_Get_EnergyMeter_AnsiString():AnsiString;inline;
var
  pd: TPDElement;
begin
  Result := '';
  If ActiveCircuit <> Nil Then begin
    if ActiveCircuit.ActiveCktElement.HasEnergyMeter then begin
      pd := ActiveCircuit.ActiveCktElement as TPDElement;
      Result := pd.MeterObj.Name;
    end;
  end;
end;

function CktElement_Get_EnergyMeter():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(CktElement_Get_EnergyMeter_AnsiString());
end;
//------------------------------------------------------------------------------
function CktElement_Get_HasVoltControl():WordBool;cdecl;
// Returns true if any of the controls is a capcontrol or a regcontrol
var
  ctrl: TDSSCktElement;
begin
  Result := FALSE;
  If ActiveCircuit <> Nil Then begin
    ctrl := ActiveCircuit.ActiveCktElement.ControlElementlist.First;
    While ctrl <> Nil Do Begin
      case (ctrl.DSSObjType And CLASSMASK) of
        CAP_CONTROL,
        REG_CONTROL: Result := True;
      else
        Result := False;
      end;
      If Result Then  Exit;

      ctrl := ActiveCircuit.ActiveCktElement.ControlElementlist.Next;
    End;
  end;
end;
//------------------------------------------------------------------------------
function CktElement_Get_HasSwitchControl():WordBool;cdecl;
var
  ctrl: TDSSCktElement;
begin
  Result := FALSE;
  If ActiveCircuit <> Nil Then begin
    ctrl := ActiveCircuit.ActiveCktElement.ControlElementList.First;
    While ctrl <> Nil Do
    Begin
      case (ctrl.DSSObjType And CLASSMASK) of
        SWT_CONTROL: Result := True;
      else
        Result := False;
      end;
      If Result Then  Exit;

      ctrl := ActiveCircuit.ActiveCktElement.ControlElementlist.Next;
    End;
  end;
end;
//------------------------------------------------------------------------------
PROCEDURE CktElement_Get_CplxSeqVoltages(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
{returns Seq Voltages as array of complex values}
VAR
  Result: PDoubleArray;
    i, iV  :Integer;
    V012 :pComplexArray;
    S :String;

Begin

  IF   ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
     If ActiveCktElement <> Nil THEN
     WITH ActiveCktElement DO
     If Enabled Then
     Begin
         TRY
            Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (2*3*NTerms-1) + 1);

            V012 := Allocmem(Sizeof(V012^[1]) * 3 * Nterms);
            // get complex seq voltages
            CalcSeqVoltages(ActiveCktElement, V012);
            // return 0 based array
            iV := 0;
            For i := 1 to 3*Nterms do Begin
                Result[iV] := V012^[i].re;
                inc(iV);
                Result[iV] := V012^[i].im;
                inc(iV);
            End;

            Reallocmem(V012, 0);  // throw away temp memory

          EXCEPT
             On E:Exception Do
             Begin
                S:= E.message + CRLF +
                    'Element='+ ActiveCktElement.Name + CRLF+
                    'Nphases=' + IntToStr(Nphases) + CRLF +
                    'NTerms=' + IntToStr(NTerms) + CRLF +
                    'NConds =' + IntToStr(NConds);
                DoSimpleMsg(S, 5012);
              End;
          END;
     End
     Else
         Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);  // Disabled

   End
  ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;
//------------------------------------------------------------------------------
PROCEDURE CktElement_Get_CplxSeqCurrents(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
{returns Seq Voltages as array of complex values}
VAR
  Result: PDoubleArray;
    i, iV  :Integer;
    i012 :pComplexArray;
    S :String;

Begin

  IF   ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
     If ActiveCktElement <> Nil THEN
     WITH ActiveCktElement DO
     If Enabled Then
     Begin
         TRY
            Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (2*3*NTerms-1) + 1);

            i012 := Allocmem(Sizeof(i012^[1]) * 3 * Nterms);
            // get complex seq voltages
            CalcSeqCurrents(ActiveCktElement, i012);
            // return 0 based array
            iV := 0;
            For i := 1 to 3*Nterms do Begin
                Result[iV] := i012^[i].re;
                inc(iV);
                Result[iV] := i012^[i].im;
                inc(iV);
            End;

            Reallocmem(i012, 0);  // throw away temp memory

          EXCEPT
             On E:Exception Do
             Begin
                S:= E.message + CRLF +
                    'Element='+ ActiveCktElement.Name + CRLF+
                    'Nphases=' + IntToStr(Nphases) + CRLF +
                    'NTerms=' + IntToStr(NTerms) + CRLF +
                    'NConds =' + IntToStr(NConds);
                DoSimpleMsg(S, 5012);
              End;
          END;
     End
     Else
         Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);  // Disabled

   End
  ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);


end;
//------------------------------------------------------------------------------
PROCEDURE CktElement_Get_AllVariableNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
VAR
  Result: PPAnsiCharArray;
   k:Integer;
   pPCElem :TPCElement;

begin

  Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
  IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
     If ActiveCktElement<>Nil THEN
     WITH ActiveCktElement DO
     Begin

         If (DSSObjType And BASECLASSMASK) = PC_ELEMENT Then
          Begin
              pPCElem := (ActiveCktElement as TPCElement);
              Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (pPCElem.NumVariables-1) + 1);
              For k := 1 to pPCElem.NumVariables DO
              Begin
                  Result[k-1] := DSS_CopyStringAsPChar(pPCElem.VariableName(k));
              End;
          End;

         {Else zero-length array null string}
     End
   End;

end;
//------------------------------------------------------------------------------
PROCEDURE CktElement_Get_AllVariableValues(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
{Return array of doubles with values of all variables if PCElement}
VAR
  Result: PDoubleArray;
   k:Integer;
   pPCElem :TPCElement;

begin

  Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
  IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
     If ActiveCktElement<>Nil THEN
     WITH ActiveCktElement DO
     Begin

         If (DSSObjType And BASECLASSMASK) = PC_ELEMENT Then
          Begin
              pPCElem := (ActiveCktElement as TPCElement);
              Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (pPCElem.NumVariables-1) + 1);
              For k := 1 to pPCElem.NumVariables DO
              Begin
                  Result[k-1] := pPCElem.Variable[k];
              End;
          End;

         {Else zero-length array null string}
     End
   End;

end;
//------------------------------------------------------------------------------
function CktElement_Get_Variable(const MyVarName: PAnsiChar;  out Code: Integer):Double;cdecl;
Var
      pPCElem:TPCElement;
      VarIndex :Integer;

begin
  Result := 0.0; Code := 1; // Signifies an error; no value set
  IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
     If ActiveCktElement<>Nil THEN
     WITH ActiveCktElement DO
     Begin

         If (DSSObjType And BASECLASSMASK) = PC_ELEMENT Then
          Begin
              pPCElem := (ActiveCktElement as TPCElement);
              VarIndex := pPCElem.LookupVariable(MyVarName);
              If (VarIndex>0) and (VarIndex <= pPCElem.NumVariables) Then
              Begin
                   Result := pPCElem.Variable[VarIndex];
                   Code := 0;  // Signify result is OK.
              End;
          End;

         {Else zero-length array null string}
     End
   End;

end;
//------------------------------------------------------------------------------
function CktElement_Get_Variablei(Idx: Integer;  out Code: Integer):Double;cdecl;
{Get Value of a variable by index}
Var
      pPCElem:TPCElement;

begin
  Result := 0.0; Code := 1; // Signifies an error; no value set
  IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
     If ActiveCktElement<>Nil THEN
     WITH ActiveCktElement DO
     Begin

         If (DSSObjType And BASECLASSMASK) = PC_ELEMENT Then
          Begin
              pPCElem := (ActiveCktElement as TPCElement);
              If (Idx>0) and (Idx <= pPCElem.NumVariables) Then
              Begin
                   Result := pPCElem.Variable[Idx];
                   Code := 0;  // Signify result is OK.
              End;
          End;

         {Else zero-length array null string}
     End
   End;

end;
//------------------------------------------------------------------------------
PROCEDURE CktElement_Get_NodeOrder(var ResultPtr: PInteger; var ResultCount: Integer);cdecl;
VAR
  Result: PIntegerArray;
   k : Integer;
   i : Integer;
   j : Integer;
begin

      Result := DSS_CreateArray_PInteger(ResultPtr, ResultCount, (0) + 1);
      If ActiveCircuit <> Nil Then With ActiveCircuit Do
      Begin

         If ActiveCktElement<>Nil THEN
         WITH ActiveCktElement DO
         Begin
              k := 0;
              Result := DSS_CreateArray_PInteger(ResultPtr, ResultCount, (NTerms*Nconds-1) + 1);

              for i := 1 to Nterms do
              Begin
                  for j := (i-1)*NConds+1 to i*Nconds do
                  Begin
                       Result[k] := GetNodeNum(NodeRef^[j]);
                       inc(k);
                  End;
              End;
         End
      End;


end;
//------------------------------------------------------------------------------
function CktElement_Get_HasOCPDevice():WordBool;cdecl;
// Check for presence of a fuse, recloser, etc.
begin
  Result := FALSE;
  If ActiveCircuit <> Nil Then begin
    Result := ActiveCircuit.ActiveCktElement.HasOCPDevice;
  end;
end;
//------------------------------------------------------------------------------
function CktElement_Get_NumControls():Integer;cdecl;
begin
  Result := 0;
  If ActiveCircuit <> Nil Then begin
    Result := ActiveCircuit.ActiveCktElement.ControlElementList.listSize;
  end;
end;
//------------------------------------------------------------------------------
function CktElement_Get_OCPDevIndex():Integer;cdecl;
Var
   iControl : integer;
   pCktElement : TDSSCktElement;

begin
     Result := 0;
     If ActiveCircuit <> Nil Then  With ActiveCircuit Do
     Begin
         iControl := 1;
         Repeat
           // cycle through the list of controls until we find a fuse, recloser, or relay
              pCktElement :=  ActiveCktElement.ControlElementList.Get(iControl);
              If pCktElement <> Nil Then
              Case (pCktElement.DSSObjType and CLASSMASK) of

                  FUSE_CONTROL     : Result := iControl;
                  RECLOSER_CONTROL : Result := iControl;
                  RELAY_CONTROL    : Result := iControl;

              End;
              inc(iControl);
         Until (iControl > ActiveCktElement.ControlElementList.listSize) or (Result > 0);
     End;
end;
//------------------------------------------------------------------------------
function CktElement_Get_OCPDevType():Integer;cdecl;
begin
     Result := 0;
     If ActiveCircuit <> Nil Then  With ActiveCircuit Do
         Result := GetOCPDeviceType(ActiveCktElement);     // see Utilities.pas
end;
//------------------------------------------------------------------------------
PROCEDURE CktElement_Get_CurrentsMagAng(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
// return currents in magnitude, angle array
VAR
  Result: PDoubleArray;
  cBuffer: pComplexArray;
  CMagAng: polar;
  NValues, iV ,i: Integer;

Begin

  If ActiveCircuit <> Nil Then
     WITH ActiveCircuit.ActiveCktElement DO
     Begin
         NValues := NConds*NTerms;
         Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (2*NValues-1) + 1);
         cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
         GetCurrents(cBuffer);
         iV :=0;
         For i := 1 to  NValues DO
         Begin
            CMagAng := ctopolardeg(cBuffer^[i]); // convert to mag/angle
            Result[iV] := CMagAng.mag ;
            Inc(iV);
            Result[iV] := CMagAng.ang ;
            Inc(iV);
         End;
         Reallocmem(cBuffer,0);
     End
  Else
     Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;
//------------------------------------------------------------------------------
PROCEDURE CktElement_Get_VoltagesMagAng(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
// Bus Voltages in magnitude, angle at all terminal
VAR
  Result: PDoubleArray;
  numcond, i,n,iV:Integer;
  Volts:Polar;

Begin

// Return voltages for all terminals

     IF ActiveCircuit <> Nil THEN
      WITH ActiveCircuit DO
      Begin
        If ActiveCktElement<>Nil THEN
        WITH ActiveCktElement DO
        Begin
         numcond := NConds*Nterms;
         Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (2*numcond-1) + 1);
         // k := (Terminal-1)*numcond;    // RCD 8-30-00 Changed
         iV :=0;
         FOR i := 1 to  numcond DO
         Begin
            n := ActiveCktElement.NodeRef^[i];
            Volts := ctopolardeg(Solution.NodeV^[n]); // ok if =0
            Result[iV] := Volts.mag;
            Inc(iV);
            Result[iV] := Volts.ang;
            Inc(iV);
         End;
        End;
      End
    ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;
//------------------------------------------------------------------------------
END.
