unit DCktElement;

interface

uses DSSClassDefs, DSSGlobals, UComplex, Sysutils,
     PDElement, PCElement, MathUtil, Variants, CktElement, Utilities;

function CktElementI(mode:longint; arg:longint):longint;cdecl;
function CktElementF(mode:longint; arg:double):double;cdecl;
function CktElementS(mode:longint; arg:pAnsiChar):pAnsiChar;cdecl;
procedure CktElementV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;


implementation

var
  ctrl          : TDSSCktElement;
  pPCElem       : TPCElement;
  pPDElem       : TPDElement;
  BData         :  wordbool;
  i,
  count,
  low,
  numcond,
  n,
  iV,
  VarIdx        : Integer;
  Volts,
  cResid        : Complex;
  cBuffer       : pComplexArray;
  S:String;

Procedure CalcSeqCurrents(pActiveElement:TDSSCktElement; i012:pComplexArray);
{Assumes V012 is properly allocated before call.}
VAR
    Nvalues,i,j,k,iV  :Integer;
    IPh, I012a        :Array[1..3] of Complex;
    cBuffer:pComplexArray;

BEGIN
    With pActiveElement, ActiveCircuit[ActiveActor] Do BEGIN
      Nvalues := NPhases;
      IF Nvalues <> 3 THEN Begin
        {Handle non-3 phase elements}
           IF (Nphases = 1) and PositiveSequence THEN
           Begin
                NValues := NConds*NTerms;
                cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
                GetCurrents(cBuffer, ActiveActor);

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
           GetCurrents(cBuffer,ActiveActor);
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


Procedure CalcSeqVoltages(pActiveElement:TDSSCktElement; V012:pComplexArray);
{Assumes V012 is properly allocated before call.}
VAR
    Nvalues,i,j,k,iV  :Integer;
    VPh, V012a        :Array[1..3] of Complex;
BEGIN
    With pActiveElement, ActiveCircuit[ActiveActor] Do BEGIN
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

FUNCTION IsPDElement : Boolean;
Begin
    Result :=  ((ActiveCircuit[ActiveActor].ActiveCktElement.DSSObjType and 3) = PD_ELEMENT)
End;

function CktElementI(mode:longint; arg:longint):longint;cdecl;
var
   pCktElement  : TDSSCktElement;
   iControl     : integer;
   pPCElem      : TPCElement;

begin
    Result:=0;  // Default return value
    case mode of
        0: begin                                    // CktElement.Numterminals
            If ActiveCircuit[ActiveActor] <> Nil Then
            Result := ActiveCircuit[ActiveActor].ActiveCktElement.NTerms
            Else Result := 0;
        end;
        1: begin                                    // CktElement.NumConductors
            If ActiveCircuit[ActiveActor] <> Nil Then
            Result := ActiveCircuit[ActiveActor].ActiveCktElement.NConds
            Else Result := 0;
        end;
        2: begin                                    // CktElement.NumPhases
            If ActiveCircuit[ActiveActor] <> Nil Then
            Result := ActiveCircuit[ActiveActor].ActiveCktElement.NPhases
            Else Result := 0;
        end;
        3: begin                                    // CktElement.Open
           IF ActiveCircuit[ActiveActor] <> Nil THEN
           WITH ActiveCircuit[ActiveActor] DO
           Begin
              If ActiveCktElement<>nil THEN
              WITH ActiveCktElement DO
              Begin
               ActiveTerminal := Terminals^[arg];
              Closed[3,ActiveActor] := FALSE;
              End;
           End;
           Result:=0;
        end;
        4: begin                                    // CktElement.Close
           IF ActiveCircuit[ActiveActor] <> Nil THEN
           WITH ActiveCircuit[ActiveActor] DO
           Begin
              If ActiveCktElement<>nil THEN
              WITH ActiveCktElement DO
              Begin
               ActiveTerminal := Terminals^[arg];
              Closed[3,ActiveActor] := TRUE;
              End;
           End;
           Result:=0;
        end;
        5: begin                                    // CktElement.IsOpen
            If ActiveCircuit[ActiveActor] <> Nil Then
           With ActiveCircuit[ActiveActor] Do
           Begin
               With ActiveCktElement Do ActiveTerminal := Terminals^[arg];
               Result := 0;
               For i := 1 to ActiveCktElement.NConds Do
                  If not ActiveCktElement.Closed[i,ActiveActor] Then
                    Begin
                       Result :=  1;
                       Exit;
                    End;
           End;
        end;
        6: begin                                    // CktElement.NumProperties
            Result := 0;
            IF ActiveCircuit[ActiveActor] <> Nil THEN
             WITH ActiveCircuit[ActiveActor] DO
             Begin
               If ActiveCktElement<>Nil THEN
               WITH ActiveCktElement DO
               Begin
                    Result := ParentClass.NumProperties ;
               End
             End;
        end;
        7: begin                                    // CktElement.HasSwitchControl
            Result := 0;
            If ActiveCircuit[ActiveActor] <> Nil Then begin
              ctrl := ActiveCircuit[ActiveActor].ActiveCktElement.ControlElementList.First;
              While ctrl <> Nil Do
              Begin
                case (ctrl.DSSObjType And CLASSMASK) of
                  SWT_CONTROL: Result := 1;
                else
                  Result := 0;
                end;
                If Result=1 Then  Exit;
                ctrl := ActiveCircuit[ActiveActor].ActiveCktElement.ControlElementlist.Next;
              End;
            end;
        end;
        8: begin                                    // CktElement.HasVoltControl
            Result := 0;
            If ActiveCircuit[ActiveActor] <> Nil Then begin
              ctrl := ActiveCircuit[ActiveActor].ActiveCktElement.ControlElementlist.First;
              While ctrl <> Nil Do Begin
                case (ctrl.DSSObjType And CLASSMASK) of
                  CAP_CONTROL,
                  REG_CONTROL: Result := 1;
                else
                  Result := 0;
                end;
                If Result=1 Then  Exit;
                ctrl := ActiveCircuit[ActiveActor].ActiveCktElement.ControlElementlist.Next;
              End;
            end;
        end;
        9: begin                                    // CktElement.NumControls
            Result := 0;
            If ActiveCircuit[ActiveActor] <> Nil Then begin
              Result := ActiveCircuit[ActiveActor].ActiveCktElement.ControlElementList.listSize;
            end;
        end;
        10: begin                                   // CktElement.OCPDevIndex
            Result := 0;
            If ActiveCircuit[ActiveActor] <> Nil Then  With ActiveCircuit[ActiveActor] Do
            Begin
                 iControl :=  1;
                 Repeat
           // cycle through the list of controls until we find a fuse, recloser, or relay
                      pCktElement :=  ActiveCktElement.ControlElementList.Get(iControl);
                      If pCktElement <> Nil Then
                      Case (pCktElement.DSSObjType and CLASSMASK) of
                          FUSE_CONTROL     : Result := longint(iControl);
                          RECLOSER_CONTROL : Result := longint(iControl);
                          RELAY_CONTROL    : Result := longint(iControl);
                      End;
                      inc(iControl);
                 Until (iControl > ActiveCktElement.ControlElementList.listSize) or (Result > 0);
             End;
        end;
        11: begin                                   // CktElement.OCPDevType
               Result := 0;
               If ActiveCircuit[ActiveActor] <> Nil Then  With ActiveCircuit[ActiveActor] Do
                 Result := GetOCPDeviceType(ActiveCktElement);     // see Utilities.pas
        end;
        12: begin                                   // CktElement.enabled -read
            Result:=0;
            If ActiveCircuit[ActiveActor] <> Nil Then begin
               if ActiveCircuit[ActiveActor].ActiveCktElement.Enabled then
                   Result:=1;
            end
            Else
               Result := 0;
        end;
        13: begin                                   // CktElement.enabled -Write
            if arg=1 then BData:=TRUE
            else BData:=FALSE;
            If ActiveCircuit[ActiveActor] <> Nil Then
                ActiveCircuit[ActiveActor].ActiveCktElement.Enabled := BData;
        end;
        14: begin                                   // CktElement.ActiveVariableIndex -Write
          Result := -1; // Signifies an error; no variable found
          IF ActiveCircuit[ActiveActor] <> Nil THEN
           WITH ActiveCircuit[ActiveActor] DO
           Begin
             If ActiveCktElement<>Nil THEN
             WITH ActiveCktElement DO
             Begin

                 If (DSSObjType And BASECLASSMASK) = PC_ELEMENT Then
                  Begin
                      pPCElem := (ActiveCktElement as TPCElement);
                      If (Arg>0) and (Arg <= pPCElem.NumVariables) Then
                      Begin
                        Result := 0;  // the variable seems to exist
                        VarIdx := Arg;
                      End;
                  End;

                 {Else zero-length array null string}
             End
           End;
        end
    else
        Result:=-1;
    end;

end;

//**************************Float commands****************************************
function CktElementF(mode:longint; arg:double):double;cdecl;
begin
    Result:=0.0;  // Default return value
    case mode of
    0: begin                                        // CktElement.NormalAmps - read
          If ActiveCircuit[ActiveActor] <> Nil Then
         With ActiveCircuit[ActiveActor] Do
           Begin
             If (ActiveCktElement.DSSObjType and 3) = PD_ELEMENT Then
             Begin
                 With ActiveCktElement As TPDElement Do Result := NormAmps ;
             End
           Else Result := 0.0;
         End;
    end;
    1: begin                                        // CktElement.NormalAmps - Write
       If ActiveCircuit[ActiveActor] <> Nil Then
       Begin
           If IsPDElement Then
           Begin
               With ActiveCircuit[ActiveActor] Do With ActiveCktElement As TPDElement Do NormAmps := arg;
          End;  {Else Do Nothing}
       End;
    end;
      2: begin                                      // CktElement.EmergAmps - read
       If ActiveCircuit[ActiveActor] <> Nil Then
       With ActiveCircuit[ActiveActor] Do
       Begin
           If (ActiveCktElement.DSSObjType and 3) = PD_ELEMENT Then
           Begin
               With ActiveCktElement As TPDElement Do Result := EmergAmps ;
           End
           Else Result := 0.0;
       End;
    end;
    3: begin                                        // CktElement.EmergAmps - Write
      If ActiveCircuit[ActiveActor] <> Nil Then
      With ActiveCircuit[ActiveActor] Do
      Begin
          If IsPDElement Then
          Begin
              With ActiveCktElement As TPDElement Do EmergAmps := arg;
          End;  {Else Do Nothing}
      End;
    end;
    4: begin                                        // CktElement.variablei
        Result := 0.0; // Signifies an error; no value set
        i := trunc(arg);
        IF ActiveCircuit[ActiveActor] <> Nil THEN
         WITH ActiveCircuit[ActiveActor] DO
         Begin
           If ActiveCktElement<>Nil THEN
           WITH ActiveCktElement DO
           Begin
               If (DSSObjType And BASECLASSMASK) = PC_ELEMENT Then
                Begin
                    pPCElem := (ActiveCktElement as TPCElement);
                    If (i>0) and (i <= pPCElem.NumVariables) Then
                    Begin
                         Result := pPCElem.Variable[i];
                    End;
                End;
               {Else zero-length array null string}
           End
         End;
    end;
    5: begin                                        // CktElement.SetActiveVariable
      Result    :=  -1;
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      WITH ActiveCircuit[ActiveActor] DO
      Begin
         If ActiveCktElement <> Nil Then
         WITH ActiveCktElement DO
         Begin

           If (VarIdx>0) and (VarIdx <= pPCElem.NumVariables) Then       //Checks that the active Idx is valid
           Begin
            Result                    := 0;        // No error, the variable exists and is set
            pPCElem.Variable[VarIdx]  := Arg;
           End

         End;
      End;
    end;
    6: begin                                        // CktElement.GetActiveVariable
      Result    :=  -1;
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      WITH ActiveCircuit[ActiveActor] DO
      Begin
         If ActiveCktElement <> Nil Then
         WITH ActiveCktElement DO
         Begin

           If (VarIdx>0) and (VarIdx <= pPCElem.NumVariables) Then       //Checks that the active Idx is valid
            Result                    := pPCElem.Variable[VarIdx];        // No error, the variable exists and is returned

         End;
      End;
    end
    else
        Result:=-1;
    end;
end;

//**************************String commands****************************************
function CktElementS(mode:longint; arg:pAnsiChar):pAnsiChar;cdecl;
begin
  Result:=pAnsiChar(AnsiString('0'));  // Default return value
  case mode of
  0: begin                                          // CktElement.Name
     If ActiveCircuit[ActiveActor] <> Nil Then
       WITH ActiveCircuit[ActiveActor].ActiveCktElement DO
       Begin
         Result := pAnsiChar(AnsiString(ParentClass.Name + '.' + Name));
       End
    Else
       Result :=pAnsiChar(AnsiString( ''));
  end;
  1: begin                                          // CktElement.Display - read
     If ActiveCircuit[ActiveActor] <> Nil Then
       Result := pAnsiChar(AnsiString(ActiveCircuit[ActiveActor].ActiveCktElement.DisplayName))
    Else
       Result :=pAnsiChar(AnsiString(''));
  end;
  2: begin                                          // CktElement.Display - Write
     If ActiveCircuit[ActiveActor] <> Nil Then
        ActiveCircuit[ActiveActor].ActiveCktElement.DisplayName := widestring(arg);
     Result := pAnsiChar(AnsiString(''));
  end;
  3: begin                                          // CktElement.GUID
     If ActiveCircuit[ActiveActor] <> Nil Then
       Result := pAnsiChar(AnsiString(ActiveCircuit[ActiveActor].ActiveCktElement.ID))
    Else
       Result := pAnsiChar(AnsiString(''));
  end;
  4: begin                                          // CktElement.EnergyMeter
        Result := pAnsiChar(AnsiString(''));
      If ActiveCircuit[ActiveActor] <> Nil Then begin
        if ActiveCircuit[ActiveActor].ActiveCktElement.HasEnergyMeter then begin
          pPDElem := ActiveCircuit[ActiveActor].ActiveCktElement as TPDElement;
          Result := pAnsiChar(AnsiString(pPDElem.MeterObj.Name));
        end;
      end;
  end;
  5: begin                                          // CktElement.Controller
      Result := pAnsiChar(AnsiString(''));
      i   :=  strtoInt(arg);
      If ActiveCircuit[ActiveActor] <> Nil Then With ActiveCircuit[ActiveActor] Do begin
        If (i>0) and (i <= ActiveCktElement.ControlElementList.Listsize) Then
        Begin
          ctrl := ActiveCktElement.ControlElementList.Get(i);
          If ctrl <> Nil Then
            Result := pAnsiChar(AnsiString(Format('%s.%s', [ctrl.ParentClass.Name, ctrl.Name])));
        End;
      end;
  end;
  6: begin                                          // CktElement.ActiveVariableName
    Result := AnsiString('Error');  // Signifies an error; the variable doesn't exist
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       If ActiveCktElement<>Nil THEN
       WITH ActiveCktElement DO
       Begin

           If (DSSObjType And BASECLASSMASK) = PC_ELEMENT Then
            Begin
                pPCElem := (ActiveCktElement as TPCElement);
                VarIdx := pPCElem.LookupVariable(Arg);
                If (VarIdx>0) and (VarIdx <= pPCElem.NumVariables) Then
                  Result := AnsiString('OK');     // we are good, the variable seems
            End;

           {Else zero-length array null string}
       End
     End;
  end
  else
    Result:=pAnsiChar(AnsiString('Error'));
  end;
end;
//**************************Variant commands****************************************
procedure CktElementV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

var
  VPh,
  V012,
  IPh,
  I012        : Array[1..3] of Complex;
  myInit,
  myEnd,
  j,
  k,
  i,
  NValues     : Integer;
  cValues     : pComplexArray;
  CMagAng     : polar;
  myBuffer    : Array of Complex;
  S           : String;

begin
  case mode of
  0: begin                                          // CktElement.BusNames - read
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    If ActiveCircuit[ActiveActor] <> Nil Then
    Begin
      With ActiveCircuit[ActiveActor] Do
      Begin
        For i := 1 to  ActiveCktElement.Nterms Do
        Begin
           WriteStr2Array(ActiveCktElement.GetBus(i));
           WriteStr2Array(Char(0));
        End;
      End;
    End
    Else
      WriteStr2Array('');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
  1: begin                                          // CktElement.BusNames - Write
    myType  :=  4;          // String
    j     :=  0;
    If ActiveCircuit[ActiveActor] <> Nil Then
    Begin
      With ActiveCircuit[ActiveActor] Do Begin
        Count := ActiveCktElement.NTerms;
        For i := 1 to Count Do
        Begin
          S := BArray2Str(myPointer, j);
          if S = '' then
            break
          else
            ActiveCktElement.SetBus(i, S);
        End;
      End;
    End;
    mySize  :=  j;
  end;
  2: begin                                          // CktElement.Voltages
    myType  :=  3;        // Complex
    setlength(myCmplxArray, 1);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        If ActiveCktElement<>Nil THEN
        WITH ActiveCktElement DO
        Begin
          numcond := NConds*Nterms;
          setlength(myCmplxArray, numcond);
          // k := (Terminal-1)*numcond;    // RCD 8-30-00 Changed
          iV :=0;
          FOR i := 1 to  numcond DO
          Begin
            n := ActiveCktElement.NodeRef^[i];
            myCmplxArray[iV] := Solution.NodeV^[n]; // ok if =0
            Inc(iV);
          End;
        End;
      End;
    End
    ELSE myCmplxArray[0] := cmplx(0,0);
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  3: begin                                          // CktElement.Currents
    myType  :=  3;        // Complex
    setlength(myCmplxArray, 1);
    If ActiveCircuit[ActiveActor] <> Nil Then
    Begin
      WITH ActiveCircuit[ActiveActor].ActiveCktElement DO
      Begin
         numcond := NConds*NTerms;
         setlength(myCmplxArray, numcond);
         cBuffer := Allocmem(sizeof(cBuffer^[1])*numcond);
         GetCurrents(cBuffer,ActiveActor);
         iV :=0;
         For i := 1 to  numcond DO
         Begin
            myCmplxArray[iV] := cBuffer^[i];
            Inc(iV);
         End;
         Reallocmem(cBuffer,0);
      End
    End
    Else  myCmplxArray[0] := cmplx(0,0);
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  4: begin                                          // CktElement.Powers
    myType  :=  3;        // Complex
    setlength(myCmplxArray, 1);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor].ActiveCktElement DO
      Begin
        numcond := NConds*Nterms;
        setlength(myCmplxArray,numcond);
        cBuffer := Allocmem(sizeof(cBuffer^[1])*numcond);
        GetPhasePower(cBuffer, ActiveActor);
        iV :=0;
        For i := 1 to  numcond DO Begin
         myCmplxArray[iV] := cmulreal(cBuffer^[i], 0.001);
         Inc(iV);
        End;
        Reallocmem(cBuffer,0);
      End
    End
    ELSE myCmplxArray[0] := cmplx(0,0);
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  5: begin                                          // CktElement.Losses
    myType  :=  3;        // Complex
    setlength(myCmplxArray, 1);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        If ActiveCktElement<>Nil THEN
        Begin
         myCmplxArray[0] := ActiveCktElement.Losses[ActiveActor];
        End;
      End
    End
    ELSE myCmplxArray[0] := cmplx(0,0);
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  6: begin                                          // CktElement.Phaselosses
    myType  :=  3;        // Complex
    setlength(myCmplxArray, 1);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor].ActiveCktElement DO
      Begin
        numcond := NPhases;
        setlength(myCmplxArray, numcond);
        cBuffer := Allocmem(sizeof(cBuffer^[1])*numcond);
        GetPhaseLosses(numcond, cBuffer, ActiveActor);
        iV :=0;
        For i := 1 to  numcond DO Begin
          myCmplxArray[iV] := cmulreal(cBuffer^[i], 0.001);
          Inc(iV);
        End;
        Reallocmem(cBuffer,0);
      End
    End
    ELSE myCmplxArray[0] := cmplx(0,0);
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  7: begin                                          // CktElement.SeqVoltages
    myType  :=  2;        // Double
    setlength(myDBLArray, 1);
    myDBLArray[0] := 0;
    IF   ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        If ActiveCktElement <> Nil THEN
        Begin
          WITH ActiveCktElement DO
          If Enabled Then
          Begin
            TRY
              setlength(myDBLArray, 3 * NTerms);

              cbuffer := Allocmem(Sizeof(cbuffer^[1]) * 3 * Nterms);
              // get complex seq voltages
              CalcSeqVoltages(ActiveCktElement, cbuffer);
              // return 0 based array
              For i := 1 to (3 * Nterms) do myDBLArray[i-1] := Cabs(cbuffer^[i]);  // return mag only
              Reallocmem(cbuffer, 0);  // throw away temp memory
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
        End
      End;
    End;
    myPointer :=  @(myDBLArray[0]);
    mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
  end;
  8: begin                                          // CktElement.SeqCurrents
    myType  :=  2;        // Double
    setlength(myDBLArray, 1);
    myDBLArray[0] := 0;
    IF   ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        If ActiveCktElement <> Nil THEN
        Begin
          WITH ActiveCktElement DO
          If Enabled Then
          Begin
            TRY
              setlength(myDBLArray, (3 * NTerms));
              cbuffer := Allocmem(Sizeof(cbuffer^[1]) * 3 * Nterms);
              // get complex seq voltages
              CalcSeqCurrents(ActiveCktElement, cbuffer);
              // return 0 based array
              For i := 1 to (3 * Nterms) do myDBLArray[i-1] := Cabs(cbuffer^[i]);  // return mag only
                 Reallocmem(cbuffer, 0);  // throw away temp memory
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
        End
      End
    End;
    myPointer :=  @(myDBLArray[0]);
    mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
  end;
  9: begin                                          // CktElement.Seqpowers
    myType  :=  3;        // Complex
    setlength(myCmplxArray, 1);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        If ActiveCktElement<>Nil THEN
        Begin
          WITH ActiveCktElement DO
          Begin
            setlength(myCmplxArray, (3 * NTerms)); // allocate for kW and kvar
            IF NPhases <> 3 THEN
            Begin
              IF (Nphases = 1) and PositiveSequence THEN
              Begin
                numcond := NConds*NTerms;
                cBuffer := Allocmem(sizeof(cBuffer^[1])*numcond);
                GetCurrents(cBuffer, ActiveActor);
                For i := 0 to  ((3 * NTerms) - 1) DO myCmplxArray[i] := cmplx(0,0);   // Initialize Result
                Count := 2;  // Start with kW1
                {Put only phase 1 quantities in Pos seq}
                FOR j := 1 to NTerms Do
                Begin
                    k := (j-1)*NConds;
                    n := NodeRef^[k+1];
                    Vph[1] := Solution.NodeV^[n];  // Get voltage at node
                    myCmplxArray[count] := cmulreal(Cmul(Vph[1], conjg(cBuffer^[k+1])), 0.003);   // Compute power per phase
                    inc(count);
                End;
                Reallocmem(cBuffer,0);
              END
              ELSE  For i := 0 to  ((3 * NTerms) - 1) DO myCmplxArray[i] := cmplx(-1.0, 0);  // Signify n/A
            END
            ELSE
            Begin
              numcond := NConds*NTerms;
              cBuffer := Allocmem(sizeof(cBuffer^[1])*numcond);
              GetCurrents(cBuffer,ActiveActor);
              count := 0;
              FOR j := 1 to NTerms Do
              Begin
                k :=(j-1)*NConds;
                FOR i := 1 to  3 DO Vph[i] := Solution.NodeV^[NodeRef^[i+k]];
                For i := 1 to  3 DO Iph[i] := cBuffer^[k+i];
                Phase2SymComp(@Iph, @I012);
                Phase2SymComp(@Vph, @V012);
                For i := 1 to 3 DO  Begin
                  myCmplxArray[count] := cmulreal(Cmul(V012[i], conjg(I012[i])), 0.003);
                  inc(count);
                End;
              End;
              Reallocmem(cBuffer,0);
            End;
          End;
        End;
      End
    End
    Else myCmplxArray[0] := cmplx(0,0);
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  10: begin                                         // CktElement.AllpropertyNames
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        If ActiveCktElement<>Nil THEN
        WITH ActiveCktElement DO
        Begin
          WITH ParentClass Do
          Begin
            For k := 1 to NumProperties DO
            Begin
                WriteStr2Array(PropertyName^[k]);
                WriteStr2Array(Char(0));
            End;
          End;
        End
      End;
    End
    ELSE WriteStr2Array('');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
  11: begin                                         // CktElement.Residuals
    myType  :=  3;        // Complex
    setlength(myPolarArray, 1);
    If ActiveCircuit[ActiveActor] <> Nil Then
    Begin
      WITH ActiveCircuit[ActiveActor].ActiveCktElement DO
      Begin
        setlength(myPolarArray, NTerms);    // 2 values per terminal
        cBuffer := Allocmem(sizeof(cBuffer^[1])*Yorder);
        GetCurrents(cBuffer,ActiveActor);
        iV :=0;
        For i := 1 to  NTerms DO
        Begin
          cResid := CZERO;
          k :=(i-1)*Nconds;
          For j := 1 to Nconds Do Begin
            inc(k);
            Caccum(cResid, CBuffer^[k]);
          End;
          myPolarArray[iV] := Ctopolardeg(cResid);
          Inc(iV);
        End;
        Reallocmem(cBuffer,0);
      End
    End
    Else myPolarArray[0] := ctopolar(cmplx(0,0));
    myPointer :=  @(myPolarArray[0]);
    mySize    :=  SizeOf(myPolarArray[0]) * Length(myPolarArray);
  end;
  12: begin                                         // CktElement.YPrim
    myType  :=  3;        // Complex
    setlength(myCmplxArray, 1);
    IF ActiveCircuit[ActiveActor] <> nil Then
    Begin
      With ActiveCircuit[ActiveActor] Do
      If ActiveCktElement<>Nil THEN
      Begin
        WITH ActiveCktElement Do
        Begin
          NValues := SQR(Yorder);
          cValues := GetYprimValues(ALL_YPRIM);  // Get pointer to complex array of values
          If cValues=Nil Then
          Begin   // check for unassigned array
            Exit;  // Get outta here
          End;
          setlength(myCmplxArray, NValues);  // Make  array
          iV := 0;
          FOR i := 1 to  NValues DO  Begin    // Plunk the values in the variant array
            myCmplxArray[iV] := cValues^[i];
            Inc(iV);
          End;
        End;
      End
    End
    ELSE myCmplxArray[0] := cmplx(0,0);
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  13: begin                                         // CktElement.CplxSeqVoltages
    myType  :=  3;        // Complex
    setlength(myCmplxArray, 1);
    myCmplxArray[0] := cmplx(0,0);
    IF   ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        If ActiveCktElement <> Nil THEN
        Begin
          WITH ActiveCktElement DO
          Begin
            If Enabled Then
            Begin
              TRY
                setlength(myCmplxArray,  (3 * NTerms));
                cValues := Allocmem(Sizeof(cValues^[1]) * 3 * Nterms);
                // get complex seq voltages
                CalcSeqVoltages(ActiveCktElement, cValues);
                // return 0 based array
                iV := 0;
                For i := 1 to (3  *Nterms) do
                Begin
                    myCmplxArray[iV] := cValues^[i];
                    inc(iV);
                End;
                Reallocmem(cValues, 0);  // throw away temp memory
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
          End;
        End;
      End;
    End;
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  14: begin                                         // CktElement.CplxSeqCurrents
    myType  :=  3;        // Complex
    setlength(myCmplxArray, 1);
    myCmplxArray[0] := cmplx(0,0);
    IF   ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        If ActiveCktElement <> Nil THEN
        Begin
          WITH ActiveCktElement DO
          Begin
            If Enabled Then
            Begin
              TRY
                setlength(myCmplxArray, (3 * NTerms));
                cValues := Allocmem(Sizeof(cValues^[1]) * 3 * Nterms);
                // get complex seq voltages
                CalcSeqCurrents(ActiveCktElement, cValues);
                // return 0 based array
                iV := 0;
                For i := 1 to 3*Nterms do
                Begin
                    myCmplxArray[iV] := cValues^[i];
                    inc(iV);
                End;
                Reallocmem(cValues, 0);  // throw away temp memory
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
            End;
          End;
        End;
      End;
    End;
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  15: begin                                         // CktElement.AllVariableNames
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        If ActiveCktElement<>Nil THEN
        WITH ActiveCktElement DO
        Begin
          If (DSSObjType And BASECLASSMASK) = PC_ELEMENT Then
          Begin
            pPCElem := (ActiveCktElement as TPCElement);
            For k := 1 to pPCElem.NumVariables DO
            Begin
                WriteStr2Array(pPCElem.VariableName(k));
                WriteStr2Array(Char(0));
            End;
          End;
          {Else zero-length array null string}
        End
      End;
    End
    ELSE WriteStr2Array('');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
  16: begin                                         // CktElement.AllVariableValues
    myType  :=  2;        // Double
    setlength(myDBLArray, 1);
    myDBLArray[0] :=  0;
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        If ActiveCktElement<>Nil THEN
        WITH ActiveCktElement DO
        Begin
          If (DSSObjType And BASECLASSMASK) = PC_ELEMENT Then
          Begin
            pPCElem := (ActiveCktElement as TPCElement);
            setlength(myDBLArray, pPCElem.NumVariables);
            For k := 1 to pPCElem.NumVariables DO
            Begin
                myDBLArray[k - 1] := pPCElem.Variable[k];
            End;
          End;
         {Else zero-length array null string}
        End
      End;
    End;
    myPointer :=  @(myDBLArray[0]);
    mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
  end;
  17: begin                                         // CktElement.Nodeorder
    myType  :=  1;        // Integer
    setlength(myIntArray, 1);
    myIntArray[0] :=  0;
    If ActiveCircuit[ActiveActor] <> Nil Then With ActiveCircuit[ActiveActor] Do
    Begin
      If ActiveCktElement<>Nil THEN
      Begin
        WITH ActiveCktElement DO
        Begin
          k := 0;
          setlength(myIntArray, NTerms*Nconds);
          for i := 1 to Nterms do
          Begin
            for j := (i-1)*NConds+1 to i*Nconds do
            Begin
                 myIntArray[k] := GetNodeNum(NodeRef^[j]);
                 inc(k);
            End;
          End;
        End;
      End;
    End;
    myPointer :=  @(myIntArray[0]);
    mySize    :=  SizeOf(myIntArray[0]) * Length(myIntArray);
  end;
  18: begin                                         // CktElement.CurrentsMagAng
    myType  :=  3;        // Complex
    setlength(myPolarArray, 1);
    If ActiveCircuit[ActiveActor] <> Nil Then
    Begin
       WITH ActiveCircuit[ActiveActor].ActiveCktElement DO
       Begin
         NValues := NConds*NTerms;
         setlength(myPolarArray, NValues);
         cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
         GetCurrents(cBuffer,ActiveActor);
         iV :=0;
         For i := 1 to  NValues DO
         Begin
            myPolarArray[iV] := ctopolardeg(cBuffer^[i]); // convert to mag/angle
            Inc(iV);
         End;
         Reallocmem(cBuffer,0);
       End
    End
    Else  myPolarArray[0] := ctopolar(cmplx(0,0));
    myPointer :=  @(myPolarArray[0]);
    mySize    :=  SizeOf(myPolarArray[0]) * Length(myPolarArray);
  end;
  19: begin
// Return voltages for all terminals
    myType  :=  3;        // Complex
    setlength(myPolarArray, 1);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        If ActiveCktElement<>Nil THEN
        WITH ActiveCktElement DO
        Begin
          numcond := NConds*Nterms;
          setlength(myPolarArray,numcond);
          // k := (Terminal-1)*numcond;    // RCD 8-30-00 Changed
          iV :=0;
          FOR i := 1 to  numcond DO
          Begin
            n := ActiveCktElement.NodeRef^[i];
            myPolarArray[iV] := ctopolardeg(Solution.NodeV^[n]); // ok if =0
            Inc(iV);
          End;
        End;
      End;
    End
    ELSE myPolarArray[0] := ctopolar(cmplx(0,0));
    myPointer :=  @(myPolarArray[0]);
    mySize    :=  SizeOf(myPolarArray[0]) * Length(myPolarArray);
  end;
  20: begin
// Return total powers for the active element at all terminals
    myType  :=  3;        // Complex
    setlength(myCmplxArray, 1);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor].ActiveCktElement DO
      Begin
        NValues := NConds*Nterms;
        setlength(myCmplxArray, Nterms);
        cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
        GetPhasePower(cBuffer, Activeactor);
        iV :=0;
        setlength(myBuffer,Nterms);
        for j := 1 to Nterms do
        Begin
          myBuffer[j - 1] :=  cmplx(0.0, 0.0);
          myInit          :=  (j - 1) * NConds + 1;
          myEnd           :=  NConds * j;
          For i := myInit to myEnd DO
          Begin
            myBuffer[j - 1] :=  cadd(myBuffer[j - 1], cBuffer^[i]);
          End;
          myCmplxArray[iV]  :=  cmulreal(myBuffer[j - 1], 0.001);
          inc(iV);
        End;
        Reallocmem(cBuffer,0);
      End;
    End
    ELSE myCmplxArray[0] := cmplx(0,0);
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end
  else
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    WriteStr2Array('Error, parameter not recognized');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
end;

end.
