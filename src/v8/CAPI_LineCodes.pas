UNIT CAPI_LineCodes;
{$inline on}

INTERFACE

USES CAPI_Utils, LineCode;

function LineCodes_Get_Count():Integer;cdecl;
function LineCodes_Get_First():Integer;cdecl;
function LineCodes_Get_Next():Integer;cdecl;
function LineCodes_Get_Name():PAnsiChar;cdecl;
procedure LineCodes_Set_Name(const Value: PAnsiChar);cdecl;
function LineCodes_Get_IsZ1Z0():WordBool;cdecl;
function LineCodes_Get_Units():Integer;cdecl;
procedure LineCodes_Set_Units(Value: Integer);cdecl;
function LineCodes_Get_Phases():Integer;cdecl;
procedure LineCodes_Set_Phases(Value: Integer);cdecl;
function LineCodes_Get_R1():Double;cdecl;
procedure LineCodes_Set_R1(Value: Double);cdecl;
function LineCodes_Get_X1():Double;cdecl;
procedure LineCodes_Set_X1(Value: Double);cdecl;
function LineCodes_Get_R0():Double;cdecl;
function LineCodes_Get_X0():Double;cdecl;
procedure LineCodes_Set_R0(Value: Double);cdecl;
procedure LineCodes_Set_X0(Value: Double);cdecl;
function LineCodes_Get_C0():Double;cdecl;
function LineCodes_Get_C1():Double;cdecl;
procedure LineCodes_Set_C0(Value: Double);cdecl;
procedure LineCodes_Set_C1(Value: Double);cdecl;
PROCEDURE LineCodes_Get_Cmatrix(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
PROCEDURE LineCodes_Get_Rmatrix(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
PROCEDURE LineCodes_Get_Xmatrix(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
procedure LineCodes_Set_Cmatrix(ValuePtr: PDouble; ValueCount: Integer);cdecl;
procedure LineCodes_Set_Rmatrix(ValuePtr: PDouble; ValueCount: Integer);cdecl;
procedure LineCodes_Set_Xmatrix(ValuePtr: PDouble; ValueCount: Integer);cdecl;
function LineCodes_Get_NormAmps():Double;cdecl;
procedure LineCodes_Set_NormAmps(Value: Double);cdecl;
function LineCodes_Get_EmergAmps():Double;cdecl;
procedure LineCodes_Set_EmergAmps(Value: Double);cdecl;
PROCEDURE LineCodes_Get_AllNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;

IMPLEMENTATION

USES CAPI_Constants, sysutils, DSSGlobals, LineUnits, ParserDel, Ucomplex;

function LineCodes_Get_Count():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := LineCodeClass.ElementCount;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_First():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := LineCodeClass.First;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_Next():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := LineCodeClass.Next;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_Name_AnsiString():AnsiString;inline;
Var
   pLineCode:TLineCodeObj;

Begin
   Result := '';  // signify no name
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pLineCode := LineCodeClass.GetActiveObj ;
        If pLineCode <> Nil Then
        Begin
              Result := pLineCode.Name;
        End;
   End;

end;

function LineCodes_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(LineCodes_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_Name(const Value: PAnsiChar);cdecl;
// set LineCode active by name

Begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        If Not LineCodeClass.SetActive (Value) Then
         DoSimpleMsg('LineCode "'+ Value +'" Not Found in Active Circuit.', 51008);

         // Still same active object if not found
   End;

end;
//------------------------------------------------------------------------------
function LineCodes_Get_IsZ1Z0():WordBool;cdecl;
Var
   pLineCode:TLineCodeObj;

Begin
   Result := TRUE;
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pLineCode := LineCodeClass.GetActiveObj ;
        If pLineCode <> Nil Then
        Begin
              Result := pLineCode.SymComponentsModel ;
        End;
   End;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_Units():Integer;cdecl;
Var
   pLineCode:TLineCodeObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       Result := pLineCode.Units;
  End
end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_Units(Value: Integer);cdecl;
Var
   pLineCode:TLineCodeObj;

begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
      pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode Do Begin
          if Value < dssLineUnitsMaxnum  then
            begin
               Parser[ActiveActor].CmdString := Format('units=%s', [LineUnitsStr(Value)]);
               Edit(ActiveActor);
            end
          else
            DoSimpleMsg('Invalid line units integer sent via COM interface.  Please enter a value within range.',183);

       END;
  End;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_Phases():Integer;cdecl;
Var
   pLineCode:TLineCodeObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       Result := pLineCode.FNPhases;
  End

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_Phases(Value: Integer);cdecl;
Var
   pLineCode:TLineCodeObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       pLineCode.NumPhases := Value ;   // use property value to force reallocations
  End

end;
//------------------------------------------------------------------------------
function LineCodes_Get_R1():Double;cdecl;
Var
   pLineCode:TLineCodeObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       Result := pLineCode.R1 ;
  End

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_R1(Value: Double);cdecl;
Var
   pLineCode:TLineCodeObj;

begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
      pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode Do Begin
               Parser[ActiveActor].CmdString := Format('R1=%g', [Value]);
               Edit(ActiveActor);
       END;
  End;

end;
//------------------------------------------------------------------------------
function LineCodes_Get_X1():Double;cdecl;
Var
   pLineCode:TLineCodeObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       Result := pLineCode.X1 ;
  End

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_X1(Value: Double);cdecl;
Var
   pLineCode:TLineCodeObj;

begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
      pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode Do Begin
               Parser[ActiveActor].CmdString := Format('X1=%g', [Value]);
               Edit(ActiveActor);
       END;
  End;

end;
//------------------------------------------------------------------------------
function LineCodes_Get_R0():Double;cdecl;
Var
   pLineCode:TLineCodeObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       Result := pLineCode.R0 ;
  End

end;
//------------------------------------------------------------------------------
function LineCodes_Get_X0():Double;cdecl;
Var
   pLineCode:TLineCodeObj;

begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       Result := pLineCode.X0 ;
  End

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_R0(Value: Double);cdecl;
Var
   pLineCode:TLineCodeObj;

begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
      pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode Do Begin
               Parser[ActiveActor].CmdString := Format('R0=%g', [Value]);
               Edit(ActiveActor);
       END;
  End;

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_X0(Value: Double);cdecl;
Var
   pLineCode:TLineCodeObj;

begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
      pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode Do Begin
               Parser[ActiveActor].CmdString := Format('X0=%g', [Value]);
               Edit(ActiveActor);
       END;
  End;

end;
//------------------------------------------------------------------------------
function LineCodes_Get_C0():Double;cdecl;
Var
   pLineCode:TLineCodeObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       Result := pLineCode.C0 ;
  End

end;
//------------------------------------------------------------------------------
function LineCodes_Get_C1():Double;cdecl;
Var
   pLineCode:TLineCodeObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       Result := pLineCode.C1 ;
  End

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_C0(Value: Double);cdecl;
Var
   pLineCode:TLineCodeObj;

begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
      pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode Do Begin
               Parser[ActiveActor].CmdString := Format('C0=%g', [Value]);
               Edit(ActiveActor);
       END;
  End;

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_C1(Value: Double);cdecl;
Var
   pLineCode:TLineCodeObj;

begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
      pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode Do Begin
               Parser[ActiveActor].CmdString := Format('C1=%g', [Value]);
               Edit(ActiveActor);
       END;
  End;

end;
//------------------------------------------------------------------------------
PROCEDURE LineCodes_Get_Cmatrix(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray;
   i,j, k:Integer;
   pLineCode:TLineCodeObj;
   Factor:Double;

begin

  Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode DO Begin
         Factor := (TwoPi * BaseFrequency  * 1.0e-9);
         Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (Sqr(FNphases) - 1) + 1);
         k := 0;
         FOR i := 1 to FNPhases DO
          FOR j := 1 to FNphases DO
          Begin
             Result[k] :=  YC.GetElement(i,j).im/Factor;
             Inc(k);
          End;
       End;
  End;

end;
//------------------------------------------------------------------------------
PROCEDURE LineCodes_Get_Rmatrix(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray;
   i,j, k:Integer;
   pLineCode:TLineCodeObj;

begin

  Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode DO Begin
         Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (Sqr(FNphases) - 1) + 1);
         k := 0;
         FOR i := 1 to FNPhases DO
          FOR j := 1 to FNphases DO
          Begin
             Result[k] :=  Z.GetElement(i,j).re;
             Inc(k);
          End;
       End;
  End;

end;
//------------------------------------------------------------------------------
PROCEDURE LineCodes_Get_Xmatrix(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray;
   i,j, k:Integer;
   pLineCode:TLineCodeObj;

begin
  Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode DO Begin
         Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (Sqr(FNphases) - 1) + 1);
         k := 0;
         FOR i := 1 to FNPhases DO
          FOR j := 1 to FNphases DO
          Begin
             Result[k] :=  Z.GetElement(i,j).im;
             Inc(k);
          End;
       End;
  End;

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_Cmatrix(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  Value: PDoubleArray;
   i,j, k:Integer;
   Factor:Double;
   pLineCode:TLineCodeObj;

begin
    Value := PDoubleArray(ValuePtr);

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode DO Begin
         Factor  := TwoPi * BaseFrequency  * 1.0e-9;
         k := (0);
         FOR i := 1 to FNPhases DO
          FOR j := 1 to FNphases DO
          Begin
             Yc.SetElement(i,j, Cmplx(0.0, Value[k]*Factor));
             Inc(k);
          End;
       End;
  End;

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_Rmatrix(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  Value: PDoubleArray;
   i,j, k:Integer;
   pLineCode:TLineCodeObj;
   Ztemp:complex;

begin
    Value := PDoubleArray(ValuePtr);

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode DO Begin
         k := (0);
         FOR i := 1 to FNPhases DO
          FOR j := 1 to FNphases DO
          Begin
             ZTemp := Z.GetElement(i,j);
             Z.SetElement(i,j, Cmplx( Value[k], ZTemp.im));
             Inc(k);
          End;
       End;
  End;

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_Xmatrix(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  Value: PDoubleArray;
   i,j, k:Integer;
   pLineCode:TLineCodeObj;
   Ztemp:complex;

begin
    Value := PDoubleArray(ValuePtr);

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       WITH pLineCode DO Begin
         k := (0);
         FOR i := 1 to FNPhases DO
          FOR j := 1 to FNphases DO
          Begin
             ZTemp := Z.GetElement(i,j);
             Z.SetElement(i,j, Cmplx( ZTemp.re, Value[k] ));
             Inc(k);
          End;
       End;
  End;

end;
//------------------------------------------------------------------------------
function LineCodes_Get_NormAmps():Double;cdecl;
Var
   pLineCode:TLineCodeObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       Result := pLineCode.NormAmps  ;
  End

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_NormAmps(Value: Double);cdecl;
Var
   pLineCode:TLineCodeObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       pLineCode.NormAmps  := Value   ;
  End

end;
//------------------------------------------------------------------------------
function LineCodes_Get_EmergAmps():Double;cdecl;
Var
   pLineCode:TLineCodeObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       Result := pLineCode.EmergAmps   ;
  End

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_EmergAmps(Value: Double);cdecl;
Var
   pLineCode:TLineCodeObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineCode := LineCodeClass.GetActiveObj ;
       pLineCode.EmergAmps := Value   ;
  End

end;
//------------------------------------------------------------------------------
PROCEDURE LineCodes_Get_AllNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
VAR
  Result: PPAnsiCharArray;
  LineCodeElem:TLineCodeObj;
  k:Integer;

Begin
    Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     If LineCodeClass.ElementList.ListSize  >0 Then
     Begin
       DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (LineCodeClass.ElementList.ListSize-1) + 1);
       k:=0;
       LineCodeElem := LineCodeClass.ElementList.First;
       WHILE LineCodeElem<>Nil DO
       Begin
          Result[k] := DSS_CopyStringAsPChar(LineCodeElem.Name);
          Inc(k);
          LineCodeElem := LineCodeClass.ElementList.Next;
       End;
     End;

end;
//------------------------------------------------------------------------------
END.
