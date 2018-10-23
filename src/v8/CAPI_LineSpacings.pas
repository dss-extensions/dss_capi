UNIT CAPI_LineSpacings;
{$inline on}

INTERFACE

USES CAPI_Utils, LineSpacing;

FUNCTION LineSpacings_Get_Count():Integer;cdecl;
FUNCTION LineSpacings_Get_First():Integer;cdecl;
FUNCTION LineSpacings_Get_Next():Integer;cdecl;
FUNCTION LineSpacings_Get_Name():PAnsiChar;cdecl;
PROCEDURE LineSpacings_Set_Name(const Value: PAnsiChar);cdecl;
FUNCTION LineSpacings_Get_Nconds():Integer;cdecl;
PROCEDURE LineSpacings_Set_Nconds(Value: Integer);cdecl;
FUNCTION LineSpacings_Get_Phases():Integer;cdecl;
PROCEDURE LineSpacings_Set_Phases(Value: Integer);cdecl;
FUNCTION LineSpacings_Get_Units(): Integer;cdecl;
PROCEDURE LineSpacings_Set_Units(Value: Integer);cdecl;
PROCEDURE LineSpacings_Get_Xcoords(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE LineSpacings_Get_Xcoords_GR();cdecl;
PROCEDURE LineSpacings_Set_Xcoords(ValuePtr: PDouble; ValueCount: Integer);cdecl;
PROCEDURE LineSpacings_Get_Ycoords(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE LineSpacings_Get_Ycoords_GR();cdecl;
PROCEDURE LineSpacings_Set_Ycoords(ValuePtr: PDouble; ValueCount: Integer);cdecl;
PROCEDURE LineSpacings_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE LineSpacings_Get_AllNames_GR();cdecl;

IMPLEMENTATION

USES CAPI_Constants, sysutils, DSSGlobals, LineUnits, ParserDel, Ucomplex, Line, UcMatrix;

FUNCTION LineSpacings_Get_Count():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := LineSpacingClass[ActiveActor].ElementCount;
end;
//------------------------------------------------------------------------------
FUNCTION LineSpacings_Get_First():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := LineSpacingClass[ActiveActor].First;
end;
//------------------------------------------------------------------------------
FUNCTION LineSpacings_Get_Next():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := LineSpacingClass[ActiveActor].Next;
end;
//------------------------------------------------------------------------------
FUNCTION LineSpacings_Get_Name_AnsiString():AnsiString;inline;
Var
   pLineSpacing:TLineSpacingObj;

Begin
   Result := '';  // signify no name
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pLineSpacing := LineSpacingClass[ActiveActor].GetActiveObj ;
        If pLineSpacing <> Nil Then
        Begin
              Result := pLineSpacing.Name;
        End;
   End;

end;

FUNCTION LineSpacings_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(LineSpacings_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
PROCEDURE LineSpacings_Set_Name(const Value: PAnsiChar);cdecl;
// set LineCode active by name

Begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        If Not LineSpacingClass[ActiveActor].SetActive (Value) Then
         DoSimpleMsg('LineSpacing "'+ Value +'" Not Found in Active Circuit.', 51008);

         // Still same active object if not found
   End;

end;
//------------------------------------------------------------------------------
FUNCTION LineSpacings_Get_Nconds():Integer;cdecl;
Var
   pLineSpacing:TLineSpacingObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineSpacing := LineSpacingClass[ActiveActor].GetActiveObj ;
       Result := pLineSpacing.NWires;
  End
end;
//------------------------------------------------------------------------------
PROCEDURE LineSpacings_Set_Nconds(Value: Integer);cdecl;
Var
   pLineSpacing:TLineSpacingObj;

begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
      pLineSpacing := LineSpacingClass[ActiveActor].GetActiveObj ;
      if (Value < 1) then 
      begin
         DoSimpleMsg('Invalid number of conductors sent via COM interface.  Please enter a value within range.',183);
      end
      else 
      begin
         pLineSpacing.DataChanged := TRUE;
         pLineSpacing.NWires := Value;
      end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION LineSpacings_Get_Phases():Integer;cdecl;
Var
   pLineSpacing:TLineSpacingObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineSpacing := LineSpacingClass[ActiveActor].GetActiveObj ;
       Result := pLineSpacing.NPhases;
  End

end;
//------------------------------------------------------------------------------
PROCEDURE LineSpacings_Set_Phases(Value: Integer);cdecl;
Var
   pLineSpacing:TLineSpacingObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineSpacing := LineSpacingClass[ActiveActor].GetActiveObj ;
       with pLineSpacing do 
       begin
          DataChanged := TRUE;
          NPhases := Value ;
       end;
  End
end;

//------------------------------------------------------------------------------
PROCEDURE LineSpacings_Set_Units(Value: Integer);cdecl;
VAR
  pLineSpacing:TLineSpacingObj;
Begin
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN Begin
         pLineSpacing := LineSpacingClass[ActiveActor].GetActiveObj ;
         with pLineSpacing do
         begin
            Units := Value;
            DataChanged := TRUE;
         end;
    End
end;
//------------------------------------------------------------------------------
FUNCTION LineSpacings_Get_Units(): Integer;cdecl;
VAR
  pLineSpacing:TLineSpacingObj;
Begin
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN Begin
         pLineSpacing := LineSpacingClass[ActiveActor].GetActiveObj ;
         with pLineSpacing do
         begin
            Result := Units;
         end;
    End
end;
//------------------------------------------------------------------------------
PROCEDURE LineSpacings_Set_Ycoords(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  pLineSpacing:TLineSpacingObj;
  i:Integer;
Begin
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN Begin
         pLineSpacing := LineSpacingClass[ActiveActor].GetActiveObj ;
         with pLineSpacing do
         begin
            if NWires <> ValueCount then
            begin
                DoSimpleMsg('Invalid number of items sent via COM interface.  Please enter a value within range.',183);
                Exit;
            end;
            
            for i:= 1 to ValueCount do Ycoord[i] := ValuePtr[i - 1];
            
            DataChanged := TRUE;
         end;
    End
end;
//------------------------------------------------------------------------------
PROCEDURE LineSpacings_Get_Ycoords(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
  pLineSpacing:TLineSpacingObj;
  i:Integer;
Begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN Begin
         pLineSpacing := LineSpacingClass[ActiveActor].GetActiveObj ;
         with pLineSpacing do
         begin
            DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, (NWires-1) + 1);
            for i:= 1 to NWires do Result[i - 1] := Ycoord[i];
         end;
    End
end;
PROCEDURE LineSpacings_Get_Ycoords_GR();cdecl;
// Same as LineSpacings_Get_Ycoords but uses global result (GR) pointers
begin
   LineSpacings_Get_Ycoords(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE LineSpacings_Set_Xcoords(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  pLineSpacing:TLineSpacingObj;
  i:Integer;
Begin
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN Begin
         pLineSpacing := LineSpacingClass[ActiveActor].GetActiveObj ;
         with pLineSpacing do
         begin
            if NWires <> ValueCount then
            begin
                DoSimpleMsg('Invalid number of items sent via COM interface.  Please enter a value within range.',183);
                Exit;
            end;
            
            for i:= 1 to ValueCount do Xcoord[i] := ValuePtr[i - 1];
            
            DataChanged := TRUE;
         end;
    End
end;
//------------------------------------------------------------------------------
PROCEDURE LineSpacings_Get_Xcoords(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
  pLineSpacing:TLineSpacingObj;
  i:Integer;
Begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN Begin
         pLineSpacing := LineSpacingClass[ActiveActor].GetActiveObj ;
         with pLineSpacing do
         begin
            DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, (NWires-1) + 1);
            for i:= 1 to NWires do Result[i - 1] := Xcoord[i];
         end;
    End
end;
PROCEDURE LineSpacings_Get_Xcoords_GR();cdecl;
// Same as LineSpacings_Get_Xcoords but uses global result (GR) pointers
begin
   LineSpacings_Get_Xcoords(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE LineSpacings_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  LineSpacingElem:TLineSpacingObj;
  k:Integer;

Begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     If LineSpacingClass[ActiveActor].ElementList.ListSize  >0 Then
     Begin
       DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (LineSpacingClass[ActiveActor].ElementList.ListSize-1) + 1);
       k:=0;
       LineSpacingElem := LineSpacingClass[ActiveActor].ElementList.First;
       WHILE LineSpacingElem<>Nil DO
       Begin
          Result[k] := DSS_CopyStringAsPChar(LineSpacingElem.Name);
          Inc(k);
          LineSpacingElem := LineSpacingClass[ActiveActor].ElementList.Next;
       End;
     End;

end;
PROCEDURE LineSpacings_Get_AllNames_GR();cdecl;
// Same as LineSpacings_Get_AllNames but uses global result (GR) pointers
begin
   LineSpacings_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
END.
