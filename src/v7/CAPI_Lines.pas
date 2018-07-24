UNIT CAPI_Lines;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE Lines_Get_AllNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
function Lines_Get_Bus1():PAnsiChar;cdecl;
function Lines_Get_Bus2():PAnsiChar;cdecl;
function Lines_Get_First():Integer;cdecl;
function Lines_Get_Length():Double;cdecl;
function Lines_Get_LineCode():PAnsiChar;cdecl;
function Lines_Get_Name():PAnsiChar;cdecl;
function Lines_Get_Next():Integer;cdecl;
function Lines_Get_Phases():Integer;cdecl;
function Lines_Get_R1():Double;cdecl;
function Lines_Get_X1():Double;cdecl;
function Lines_New(const Name: PAnsiChar):Integer;cdecl;
procedure Lines_Set_Bus1(const Value: PAnsiChar);cdecl;
procedure Lines_Set_Bus2(const Value: PAnsiChar);cdecl;
procedure Lines_Set_Length(Value: Double);cdecl;
procedure Lines_Set_LineCode(const Value: PAnsiChar);cdecl;
procedure Lines_Set_Name(const Value: PAnsiChar);cdecl;
procedure Lines_Set_Phases(Value: Integer);cdecl;
procedure Lines_Set_R1(Value: Double);cdecl;
procedure Lines_Set_X1(Value: Double);cdecl;
function Lines_Get_C0():Double;cdecl;
function Lines_Get_C1():Double;cdecl;
PROCEDURE Lines_Get_Cmatrix(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
function Lines_Get_R0():Double;cdecl;
PROCEDURE Lines_Get_Rmatrix(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
function Lines_Get_X0():Double;cdecl;
PROCEDURE Lines_Get_Xmatrix(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
procedure Lines_Set_C0(Value: Double);cdecl;
procedure Lines_Set_C1(Value: Double);cdecl;
procedure Lines_Set_Cmatrix(ValuePtr: PDouble; ValueCount: Integer);cdecl;
procedure Lines_Set_R0(Value: Double);cdecl;
procedure Lines_Set_Rmatrix(ValuePtr: PDouble; ValueCount: Integer);cdecl;
procedure Lines_Set_X0(Value: Double);cdecl;
procedure Lines_Set_Xmatrix(ValuePtr: PDouble; ValueCount: Integer);cdecl;
function Lines_Get_EmergAmps():Double;cdecl;
function Lines_Get_NormAmps():Double;cdecl;
procedure Lines_Set_EmergAmps(Value: Double);cdecl;
procedure Lines_Set_NormAmps(Value: Double);cdecl;
function Lines_Get_Geometry():PAnsiChar;cdecl;
procedure Lines_Set_Geometry(const Value: PAnsiChar);cdecl;
function Lines_Get_Rg():Double;cdecl;
function Lines_Get_Rho():Double;cdecl;
function Lines_Get_Xg():Double;cdecl;
procedure Lines_Set_Rg(Value: Double);cdecl;
procedure Lines_Set_Rho(Value: Double);cdecl;
procedure Lines_Set_Xg(Value: Double);cdecl;
PROCEDURE Lines_Get_Yprim(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
procedure Lines_Set_Yprim(ValuePtr: PDouble; ValueCount: Integer);cdecl;
function Lines_Get_NumCust():Integer;cdecl;
function Lines_Get_TotalCust():Integer;cdecl;
function Lines_Get_Parent():Integer;cdecl;
function Lines_Get_Count():Integer;cdecl;
function Lines_Get_Spacing():PAnsiChar;cdecl;
procedure Lines_Set_Spacing(const Value: PAnsiChar);cdecl;
function Lines_Get_Units():Integer;cdecl;
procedure Lines_Set_Units(Value: Integer);cdecl;

IMPLEMENTATION

USES CAPI_Constants, Line, DSSClassDefs, DSSGlobals, CktElement, uComplex, ExecHelper, Sysutils, ParserDel, Math, LineUnits;

Function IsLine(Const CktElem:TDSSCktElement):Boolean;

Begin
      Result := ((CktElem.DssObjtype AND CLASSMASK) = LINE_ELEMENT);
      If Not Result THEN
       DoSimpleMsg('Line Type Expected, but another found. Dss Class=' + CktElem.DSSClassName + CRLF +
       'Element name='+ CktElem.Name, 5007) ;
END;
//------------------------------------------------------------------------------
PROCEDURE Lines_Get_AllNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
VAR
  Result: PPAnsiCharArray;
  LineElem:TLineObj;
  k:Integer;

Begin
    Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     If Lines.ListSize>0 Then
     Begin
       DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (Lines.ListSize-1) + 1);
       k:=0;
       LineElem := Lines.First;
       WHILE LineElem<>Nil DO
       Begin
          Result[k] := DSS_CopyStringAsPChar(LineElem.Name);
          Inc(k);
          LineElem := Lines.Next;
       End;
     End;

end;
//------------------------------------------------------------------------------
function Lines_Get_Bus1_AnsiString():AnsiString;inline;
begin
  Result := '';
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := ActiveCircuit.ActiveCktElement.GetBus(1);
  End

end;

function Lines_Get_Bus1():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Lines_Get_Bus1_AnsiString());
end;
//------------------------------------------------------------------------------
function Lines_Get_Bus2_AnsiString():AnsiString;inline;
begin
  Result := '';
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := ActiveCircuit.ActiveCktElement.GetBus(2);
  End
end;

function Lines_Get_Bus2():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Lines_Get_Bus2_AnsiString());
end;
//------------------------------------------------------------------------------
function Lines_Get_First():Integer;cdecl;
Var
   pLine:TLineObj;

Begin

   Result := 0;
   If ActiveCircuit <> Nil Then
   Begin
        pLine := ActiveCircuit.Lines.First;
        If pLine <> Nil Then
        Begin
          Repeat
            If pLine.Enabled
            Then Begin
              ActiveCircuit.ActiveCktElement := pLine;
              Result := 1;
            End
            Else pLine := ActiveCircuit.Lines.Next;
          Until (Result = 1) or (pLine = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;
//------------------------------------------------------------------------------
function Lines_Get_Length():Double;cdecl;
begin
  Result := 0.0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).Len;
  End
end;
//------------------------------------------------------------------------------
function Lines_Get_LineCode_AnsiString():AnsiString;inline;
begin
  Result := '';
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).CondCode;
  End

end;

function Lines_Get_LineCode():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Lines_Get_LineCode_AnsiString());
end;
//------------------------------------------------------------------------------
function Lines_Get_Name_AnsiString():AnsiString;inline;
Var
   pLine:TDSSCktElement;

Begin
   Result := '';  // signify no name
   If ActiveCircuit <> Nil Then
   Begin
        pLine := ActiveCircuit.ActiveCktElement;
        If pLine <> Nil Then
        Begin
          Result := pLine.Name;
        End;
   End;

end;

function Lines_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Lines_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Lines_Get_Next():Integer;cdecl;
Var
   pLine:TLineObj;

Begin

   Result := 0;
   If ActiveCircuit <> Nil Then
   Begin
        pLine := ActiveCircuit.Lines.Next;
        If pLine <> Nil Then
        Begin
          Repeat
            If pLine.Enabled
            Then Begin
              ActiveCircuit.ActiveCktElement := pLine;
              Result := ActiveCircuit.Lines.ActiveIndex;
            End
            Else pLine := ActiveCircuit.Lines.Next;
          Until (Result > 0) or (pLine = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;
//------------------------------------------------------------------------------
function Lines_Get_Phases():Integer;cdecl;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := ActiveCircuit.ActiveCktElement.Nphases;
  End

end;
//------------------------------------------------------------------------------
function Lines_Get_R1():Double;cdecl;
begin
  Result := 0.0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN With TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
       Result := R1/UnitsConvert;
  End;

end;
//------------------------------------------------------------------------------
function Lines_Get_X1():Double;cdecl;
begin
  Result := 0.0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN With TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
       Result := X1/UnitsConvert;
  End;

end;
//------------------------------------------------------------------------------
function Lines_New(const Name: PAnsiChar):Integer;cdecl;
begin
      Result := AddObject('line', Name);    // Returns handle to object
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Bus1(const Value: PAnsiChar);cdecl;
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         SetBus(1, Value);
       END;
  End;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Bus2(const Value: PAnsiChar);cdecl;
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         SetBus(2, Value);
       END;
  End;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Length(Value: Double);cdecl;
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         Len := Value;
         YprimInvalid := True;
       END;
  End;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_LineCode(const Value: PAnsiChar);cdecl;
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       With TLineObj(ActiveCircuit.ActiveCktElement) DO Begin
         FetchLineCode(Value);
         YprimInvalid := True;
       End;
  End;

end;
//------------------------------------------------------------------------------
procedure Lines_Set_Name(const Value: PAnsiChar);cdecl;
VAR
    activesave :integer;
    pLine:TLineObj;
    S: String;
    Found :Boolean;
Begin


  IF ActiveCircuit <> NIL
  THEN Begin      // Search list of Lines in active circuit for name
       WITH ActiveCircuit.Lines DO
         Begin
             S := Value;  // Convert to Pascal String
             Found := FALSE;
             ActiveSave := ActiveIndex;
             pLine := First;
             While pLine <> NIL Do
             Begin
                IF (CompareText(pLine.Name, S) = 0)
                THEN Begin
                    ActiveCircuit.ActiveCktElement := pLine;
                    Found := TRUE;
                    Break;
                End;
                pLine := Next;
             End;
             IF NOT Found
             THEN Begin
                 DoSimpleMsg('Line "'+S+'" Not Found in Active Circuit.', 5008);
                 pLine := Get(ActiveSave);    // Restore active Line
                 ActiveCircuit.ActiveCktElement := pLine;
             End;
         End;
  End;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Phases(Value: Integer);cdecl;
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       With TLineObj(ActiveCircuit.ActiveCktElement)Do Begin
         Nphases := Value;
         YprimInvalid := True;
       End;
  End;

end;
//------------------------------------------------------------------------------
procedure Lines_Set_R1(Value: Double);cdecl;
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         R1 := Value;
         SymComponentsChanged := True;
         YprimInvalid := True;
       END;
  End;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_X1(Value: Double);cdecl;
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) DO Begin
         X1 := Value;
         SymComponentsChanged := True;
         YprimInvalid := True;
       END;
  End;
end;
//------------------------------------------------------------------------------
function Lines_Get_C0():Double;cdecl;
begin
  Result := 0.0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN With TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
       Result := C0/UnitsConvert * 1.0e9;
  End

end;
//------------------------------------------------------------------------------
function Lines_Get_C1():Double;cdecl;
begin
  Result := 0.0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN With TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
       Result := C1/UnitsConvert * 1.0e9;
  End;
  
end;
//------------------------------------------------------------------------------
PROCEDURE Lines_Get_Cmatrix(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray;
   i,j, k:Integer;
   Factor :Double;

begin

  Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
         Factor  := TwoPi * BaseFrequency  * 1.0e-9 * UnitsConvert;  // corrected 2.9.2018 RCD
         Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (Sqr(Nphases) - 1) + 1);
         k := 0;
         FOR i := 1 to NPhases DO
          FOR j := 1 to Nphases DO
          Begin
             Result[k] :=  Yc.GetElement(i,j).im / Factor;
             Inc(k);
          End;
       End;
  End;

end;
//------------------------------------------------------------------------------
function Lines_Get_R0():Double;cdecl;
begin
  Result := 0.0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN With TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
       Result := R0/UnitsConvert;
  End;

end;
//------------------------------------------------------------------------------
PROCEDURE Lines_Get_Rmatrix(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray;
   i,j, k:Integer;

begin
  Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
         Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (Sqr(Nphases) - 1) + 1);
         k := 0;
         FOR i := 1 to NPhases DO
          FOR j := 1 to Nphases DO
          Begin
             Result[k] :=  Z.GetElement(i,j).Re/UnitsConvert;
             Inc(k);
          End;
       End;
  End;
end;
//------------------------------------------------------------------------------
function Lines_Get_X0():Double;cdecl;
begin

  Result := 0.0;

  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN With TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
       Result := X0/UnitsConvert;
  End;
 
end;
//------------------------------------------------------------------------------
PROCEDURE Lines_Get_Xmatrix(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray;
   i,j, k:Integer;
begin
  Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
         Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (Sqr(Nphases) - 1) + 1);
         k := 0;
         FOR i := 1 to NPhases DO
          FOR j := 1 to Nphases DO
          Begin
             Result[k] :=  Z.GetElement(i,j).im/UnitsConvert;
             Inc(k);
          End;
       End;
  End;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_C0(Value: Double);cdecl;
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) DO Begin
         C0 := Value * 1.0e-9;
         SymComponentsChanged := True;
         YprimInvalid := True;
       End;
  End;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_C1(Value: Double);cdecl;
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) DO Begin
         C1 := Value * 1.0e-9;
         SymComponentsChanged := True;
         YprimInvalid := True;
       End;
  End;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Cmatrix(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  Value: PDoubleArray;
   i,j, k:Integer;
   Factor:Double;
begin
    Value := PDoubleArray(ValuePtr);
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
         Factor  := TwoPi * BaseFrequency  * 1.0e-9;
         k := (0);
         FOR i := 1 to NPhases DO
          FOR j := 1 to Nphases DO
          Begin
             Yc.SetElement(i,j, Cmplx(0.0, Value[k]*Factor));
             Inc(k);
          End;
         YprimInvalid := True;
       End;
  End;

end;
//------------------------------------------------------------------------------
procedure Lines_Set_R0(Value: Double);cdecl;
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
           R0 := Value;
           SymComponentsChanged := True;
           YprimInvalid := True;
       END;
  End;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Rmatrix(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  Value: PDoubleArray;
   i,j, k:Integer;
   Ztemp:complex;
begin
    Value := PDoubleArray(ValuePtr);
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
         k := (0);
         FOR i := 1 to NPhases DO
          FOR j := 1 to Nphases DO
          Begin
             ZTemp := Z.GetElement(i,j);
             Z.SetElement(i,j, Cmplx( Value[k], ZTemp.im));
             Inc(k);
          End;
         YprimInvalid := True;
       End;
  End;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_X0(Value: Double);cdecl;
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
         X0 := Value;
         SymComponentsChanged := True;
         YprimInvalid := True;
       End;
  End;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Xmatrix(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  Value: PDoubleArray;
   i,j, k:Integer;
   Ztemp:complex;
begin
    Value := PDoubleArray(ValuePtr);
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement)DO Begin
         k := (0);
         FOR i := 1 to NPhases DO
          FOR j := 1 to Nphases DO
          Begin
             ZTemp := Z.GetElement(i,j);
             Z.SetElement(i,j, Cmplx(Ztemp.re, Value[k]));
             Inc(k);
          End;
          YprimInvalid := True;

       End;
  End;
end;
//------------------------------------------------------------------------------
function Lines_Get_EmergAmps():Double;cdecl;
begin
   Result := 0.0;
   IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).EmergAmps;
  End
 
end;
//------------------------------------------------------------------------------
function Lines_Get_NormAmps():Double;cdecl;
begin
  Result := 0.0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).NormAmps;
  End
end;
//------------------------------------------------------------------------------
procedure Lines_Set_EmergAmps(Value: Double);cdecl;
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         EmergAmps := Value;
       END;
  End;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_NormAmps(Value: Double);cdecl;
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         NormAmps := Value;
       END;
  End;
end;
//------------------------------------------------------------------------------
function Lines_Get_Geometry_AnsiString():AnsiString;inline;
begin
  Result := '';
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).GeometryCode;
  End
end;

function Lines_Get_Geometry():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Lines_Get_Geometry_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Geometry(const Value: PAnsiChar);cdecl;
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         Parser.CmdString := 'geometry='+Value;
         Edit;
         YprimInvalid := True;
       END;
  End;
end;
//------------------------------------------------------------------------------
function Lines_Get_Rg():Double;cdecl;
begin
  Result := 0.0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).Rg;
  End
end;
//------------------------------------------------------------------------------
function Lines_Get_Rho():Double;cdecl;
begin
  Result := 0.0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).rho;
  End
end;
//------------------------------------------------------------------------------
function Lines_Get_Xg():Double;cdecl;
begin
  Result := 0.0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).Xg;
  End
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Rg(Value: Double);cdecl;
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         Parser.CmdString := Format('rg=%.7g', [Value]);
         Edit;
         YprimInvalid := True;
       END;
  End;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Rho(Value: Double);cdecl;
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         Parser.CmdString := Format('rho=%.7g', [Value]);
         Edit;
         YprimInvalid := True;
       END;
  End;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Xg(Value: Double);cdecl;
begin
  IF ActiveCircuit <> NIL THEN
  If IsLine(ActiveCircuit.ActiveCktElement) THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         Parser.CmdString := Format('xg=%.7g', [Value]);
         Edit;
         YprimInvalid := True;
       END;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE Lines_Get_Yprim(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
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
      If IsLine(ActiveCircuit.ActiveCktElement) THEN
      WITH TLineObj(ActiveCircuit.ActiveCktElement) Do  Begin
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
procedure Lines_Set_Yprim(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
    Value: PDoubleArray;

begin
    Value := PDoubleArray(ValuePtr);
     IF ActiveCircuit <> NIL Then  Begin
       {Do Nothing for now}
     End;
end;
//------------------------------------------------------------------------------
function Lines_Get_NumCust():Integer;cdecl;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).BranchNumCustomers  ;
  End
end;
//------------------------------------------------------------------------------
function Lines_Get_TotalCust():Integer;cdecl;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).BranchTotalCustomers  ;
  End
end;
//------------------------------------------------------------------------------
function Lines_Get_Parent():Integer;cdecl;
{ Sets the Active Line to the immediately upline Line obj, if any}
{ Returns line index  or 0 if it fails or no more lines}

Var
   pLine:TLineObj;

Begin

   Result := 0;
   If ActiveCircuit <> Nil Then
     If IsLine(ActiveCircuit.ActiveCktElement) then
     Begin
          pLine := TLineObj(ActiveCircuit.ActiveCktElement);
          If pLine.ParentPDelement <> Nil Then
          Begin
              If (pLine.ParentPDelement.Enabled) and (IsLine(pLine.ParentPDelement)) Then
              Begin
                ActiveCircuit.ActiveCktElement := pLine.ParentPDElement;
                Result := ActiveCircuit.Lines.ActiveIndex;
              End;
          End;
     End;

end;
//------------------------------------------------------------------------------
function Lines_Get_Count():Integer;cdecl;
begin
    If Assigned(Activecircuit) Then
          Result := ActiveCircuit.Lines.ListSize ;
end;
//------------------------------------------------------------------------------
function Lines_Get_Spacing_AnsiString():AnsiString;inline;
begin
  Result := '';
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).SpacingCode;
  End
end;

function Lines_Get_Spacing():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Lines_Get_Spacing_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Spacing(const Value: PAnsiChar);cdecl;
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
         Parser.CmdString := 'spacing='+Value;
         Edit;
         YprimInvalid := True;
       END;
  End;
end;
//------------------------------------------------------------------------------
function Lines_Get_Units():Integer;cdecl;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       Result := TLineObj(ActiveCircuit.ActiveCktElement).LengthUnits;
  End

end;
//------------------------------------------------------------------------------
procedure Lines_Set_Units(Value: Integer);cdecl;
{
 This code assumes the present value of line units is NONE.
 The Set functions in this interface all set values in this length unit.
}
begin
  IF ActiveCircuit <> NIL
  THEN If IsLine(ActiveCircuit.ActiveCktElement)
  THEN Begin
       WITH TLineObj(ActiveCircuit.ActiveCktElement) Do Begin
          if Value < dssLineUnitsMaxnum  then
            begin
               Parser.CmdString := Format('units=%s', [LineUnitsStr(Value)]);
               Edit;
               YprimInvalid := True;
            end
          else
            DoSimpleMsg('Invalid line units integer sent via COM interface.  Please enter a value within range.',183);

       END;
  End;
end;
//------------------------------------------------------------------------------
END.
