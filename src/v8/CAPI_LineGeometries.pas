UNIT CAPI_LineGeometries;
{$inline on}

INTERFACE

USES CAPI_Utils, LineGeometry;

FUNCTION LineGeometries_Get_Count():Integer;cdecl;
FUNCTION LineGeometries_Get_First():Integer;cdecl;
FUNCTION LineGeometries_Get_Next():Integer;cdecl;
FUNCTION LineGeometries_Get_Name():PAnsiChar;cdecl;
PROCEDURE LineGeometries_Set_Name(const Value: PAnsiChar);cdecl;
FUNCTION LineGeometries_Get_Nconds():Integer;cdecl;
PROCEDURE LineGeometries_Set_Nconds(Value: Integer);cdecl;
FUNCTION LineGeometries_Get_Phases():Integer;cdecl;
PROCEDURE LineGeometries_Set_Phases(Value: Integer);cdecl;
PROCEDURE LineGeometries_Get_Cmatrix(var ResultPtr: PDouble; ResultCount: PInteger; Frequency, Length:double; Units:Integer);cdecl;
PROCEDURE LineGeometries_Get_Cmatrix_GR(Frequency, Length:double; Units:Integer);cdecl;
PROCEDURE LineGeometries_Get_Rmatrix(var ResultPtr: PDouble; ResultCount: PInteger; Frequency, Length:double; Units:Integer);cdecl;
PROCEDURE LineGeometries_Get_Rmatrix_GR(Frequency, Length:double; Units:Integer);cdecl;
PROCEDURE LineGeometries_Get_Xmatrix(var ResultPtr: PDouble; ResultCount: PInteger; Frequency, Length:double; Units:Integer);cdecl;
PROCEDURE LineGeometries_Get_Xmatrix_GR(Frequency, Length:double; Units:Integer);cdecl;
PROCEDURE LineGeometries_Get_Zmatrix(var ResultPtr: PDouble; ResultCount: PInteger; Frequency, Length:double; Units:Integer);cdecl;
PROCEDURE LineGeometries_Get_Zmatrix_GR(Frequency, Length:double; Units:Integer);cdecl;
PROCEDURE LineGeometries_Get_Units(var ResultPtr: PInteger; ResultCount: PInteger);cdecl;
PROCEDURE LineGeometries_Get_Units_GR();cdecl;
PROCEDURE LineGeometries_Set_Units(ValuePtr: PInteger; ValueCount: Integer);cdecl;
PROCEDURE LineGeometries_Get_Xcoords(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE LineGeometries_Get_Xcoords_GR();cdecl;
PROCEDURE LineGeometries_Set_Xcoords(ValuePtr: PDouble; ValueCount: Integer);cdecl;
PROCEDURE LineGeometries_Get_Ycoords(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE LineGeometries_Get_Ycoords_GR();cdecl;
PROCEDURE LineGeometries_Set_Ycoords(ValuePtr: PDouble; ValueCount: Integer);cdecl;
PROCEDURE LineGeometries_Get_Conductors(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE LineGeometries_Get_Conductors_GR();cdecl;
FUNCTION LineGeometries_Get_Reduce():WordBool;cdecl;
PROCEDURE LineGeometries_Set_Reduce(Value: WordBool);cdecl;
FUNCTION LineGeometries_Get_RhoEarth():Double;cdecl;
PROCEDURE LineGeometries_Set_RhoEarth(Value: Double);cdecl;
FUNCTION LineGeometries_Get_NormAmps():Double;cdecl;
PROCEDURE LineGeometries_Set_NormAmps(Value: Double);cdecl;
FUNCTION LineGeometries_Get_EmergAmps():Double;cdecl;
PROCEDURE LineGeometries_Set_EmergAmps(Value: Double);cdecl;
PROCEDURE LineGeometries_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE LineGeometries_Get_AllNames_GR();cdecl;

IMPLEMENTATION

USES CAPI_Constants, sysutils, DSSGlobals, LineUnits, ParserDel, Ucomplex, Line, UcMatrix;

FUNCTION LineGeometries_Get_Count():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := LineGeometryClass.ElementCount;
end;
//------------------------------------------------------------------------------
FUNCTION LineGeometries_Get_First():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := LineGeometryClass.First;
end;
//------------------------------------------------------------------------------
FUNCTION LineGeometries_Get_Next():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := LineGeometryClass.Next;
end;
//------------------------------------------------------------------------------
FUNCTION LineGeometries_Get_Name_AnsiString():AnsiString;inline;
Var
   pLineGeometry:TLineGeometryObj;

Begin
   Result := '';  // signify no name
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pLineGeometry := LineGeometryClass.GetActiveObj ;
        If pLineGeometry <> Nil Then
        Begin
              Result := pLineGeometry.Name;
        End;
   End;

end;

FUNCTION LineGeometries_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(LineGeometries_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
PROCEDURE LineGeometries_Set_Name(const Value: PAnsiChar);cdecl;
// set LineCode active by name

Begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        If Not LineGeometryClass.SetActive (Value) Then
         DoSimpleMsg('LineGeometry "'+ Value +'" Not Found in Active Circuit.', 51008);

         // Still same active object if not found
   End;

end;
//------------------------------------------------------------------------------
FUNCTION LineGeometries_Get_Nconds():Integer;cdecl;
Var
   pLineGeometry:TLineGeometryObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineGeometry := LineGeometryClass.GetActiveObj ;
       Result := pLineGeometry.Nconds;
  End
end;
//------------------------------------------------------------------------------
PROCEDURE LineGeometries_Set_Nconds(Value: Integer);cdecl;
Var
   pLineGeometry:TLineGeometryObj;

begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
      pLineGeometry := LineGeometryClass.GetActiveObj ;
      if (Value < 1) then 
      begin
         DoSimpleMsg('Invalid number of conductors sent via COM interface.  Please enter a value within range.',183);
      end
      else 
      begin
         pLineGeometry.DataChanged := TRUE;
         pLineGeometry.Nconds := Value;
      end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION LineGeometries_Get_Phases():Integer;cdecl;
Var
   pLineGeometry:TLineGeometryObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineGeometry := LineGeometryClass.GetActiveObj ;
       Result := pLineGeometry.NPhases;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE LineGeometries_Set_Phases(Value: Integer);cdecl;
Var
   pLineGeometry:TLineGeometryObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineGeometry := LineGeometryClass.GetActiveObj ;
       with pLineGeometry do 
       begin
          DataChanged := TRUE;
          NPhases := Value ;
       end;
  End
end;
//------------------------------------------------------------------------------
PROCEDURE LineGeometries_Get_Cmatrix(var ResultPtr: PDouble; ResultCount: PInteger; Frequency, Length:double; Units:Integer);cdecl;
VAR
   Result: PDoubleArray;
   i,j, k:Integer;
   pLineGeometry:TLineGeometryObj;
   Factor:Double;
   mat: TcMatrix;
begin
  Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
    pLineGeometry := LineGeometryClass.GetActiveObj ;
    mat := pLineGeometry.YCmatrix[Frequency, Length, Units];
    Factor := (TwoPi * Frequency  * 1.0e-9);
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (Sqr(mat.Order) - 1) + 1);
    k := 0;
    FOR i := 1 to mat.Order DO
        FOR j := 1 to mat.Order DO
        Begin
            Result[k] :=  mat.GetElement(i,j).im/Factor;
            Inc(k);
        End;
  End;
end;
PROCEDURE LineGeometries_Get_Cmatrix_GR(Frequency, Length:double; Units:Integer);cdecl;
// Same as LineGeometries_Get_Cmatrix but uses global result (GR) pointers
begin
   LineGeometries_Get_Cmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble, Frequency, Length, Units)
end;

//------------------------------------------------------------------------------
PROCEDURE LineGeometries_Get_Rmatrix(var ResultPtr: PDouble; ResultCount: PInteger; Frequency, Length:double; Units:Integer);cdecl;
VAR
   Result: PDoubleArray;
   i,j, k:Integer;
   pLineGeometry:TLineGeometryObj;
   mat: Tcmatrix;
begin
  Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
        pLineGeometry := LineGeometryClass.GetActiveObj ;
        mat := pLineGeometry.Zmatrix[Frequency, Length, Units];
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (Sqr(mat.Order) - 1) + 1);
        k := 0;
        FOR i := 1 to mat.Order DO
            FOR j := 1 to mat.Order DO
            Begin
                Result[k] :=  mat.GetElement(i,j).re;
                Inc(k);
            End;
  End;
end;
PROCEDURE LineGeometries_Get_Rmatrix_GR(Frequency, Length:double; Units:Integer);cdecl;
// Same as LineGeometries_Get_Rmatrix but uses global result (GR) pointers
begin
   LineGeometries_Get_Rmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble, Frequency, Length, Units)
end;

//------------------------------------------------------------------------------
PROCEDURE LineGeometries_Get_Xmatrix(var ResultPtr: PDouble; ResultCount: PInteger; Frequency, Length:double; Units:Integer);cdecl;
VAR
   Result: PDoubleArray;
   i,j, k:Integer;
   pLineGeometry:TLineGeometryObj;
   mat: Tcmatrix;
begin
  Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
        pLineGeometry := LineGeometryClass.GetActiveObj ;
        mat := pLineGeometry.Zmatrix[Frequency, Length, Units];
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (Sqr(mat.Order) - 1) + 1);
        k := 0;
        FOR i := 1 to mat.Order DO
            FOR j := 1 to mat.Order DO
            Begin
                Result[k] :=  mat.GetElement(i,j).im;
                Inc(k);
            End;
  End;
end;
PROCEDURE LineGeometries_Get_Xmatrix_GR(Frequency, Length:double; Units:Integer);cdecl;
// Same as LineGeometries_Get_Xmatrix but uses global result (GR) pointers
begin
   LineGeometries_Get_Xmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble, Frequency, Length, Units)
end;

//------------------------------------------------------------------------------
PROCEDURE LineGeometries_Get_Zmatrix(var ResultPtr: PDouble; ResultCount: PInteger; Frequency, Length:double; Units:Integer);cdecl;
VAR
   Result: PDoubleArray;
   i,j, k:Integer;
   pLineGeometry:TLineGeometryObj;
   mat: Tcmatrix;
   cval: Complex;
begin
  Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
        pLineGeometry := LineGeometryClass.GetActiveObj ;
        mat := pLineGeometry.Zmatrix[Frequency, Length, Units];
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2*Sqr(mat.Order) - 1) + 1);
        k := 0;
        FOR i := 1 to mat.Order DO
            FOR j := 1 to mat.Order DO
            Begin
                cval := mat.GetElement(i,j);
                Result[k] := cval.re;Inc(k);
                Result[k] := cval.im;Inc(k);
            End;
  End;
end;
PROCEDURE LineGeometries_Get_Zmatrix_GR(Frequency, Length:double; Units:Integer);cdecl;
// Same as LineGeometries_Get_Zmatrix but uses global result (GR) pointers
begin
   LineGeometries_Get_Zmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble, Frequency, Length, Units)
end;

//------------------------------------------------------------------------------
FUNCTION LineGeometries_Get_Reduce():WordBool;cdecl;
Var
   pLineGeometry:TLineGeometryObj;
begin
  Result := false;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineGeometry := LineGeometryClass.GetActiveObj;
       Result := pLineGeometry.FReduce;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE LineGeometries_Set_Reduce(Value: WordBool);cdecl;
Var
   pLineGeometry:TLineGeometryObj;
   
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineGeometry := LineGeometryClass.GetActiveObj;
       with pLineGeometry do
       begin
          DataChanged := TRUE;
          FReduce := Value;
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION LineGeometries_Get_RhoEarth():Double;cdecl;
Var
   pLineGeometry:TLineGeometryObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineGeometry := LineGeometryClass.GetActiveObj;
       Result := pLineGeometry.RhoEarth;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE LineGeometries_Set_RhoEarth(Value: Double);cdecl;
Var
   pLineGeometry:TLineGeometryObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineGeometry := LineGeometryClass.GetActiveObj;
       with pLineGeometry do 
       begin
          RhoEarth := Value;
          DataChanged := TRUE;
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION LineGeometries_Get_NormAmps():Double;cdecl;
Var
   pLineGeometry:TLineGeometryObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineGeometry := LineGeometryClass.GetActiveObj;
       Result := pLineGeometry.NormAmps;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE LineGeometries_Set_NormAmps(Value: Double);cdecl;
Var
   pLineGeometry:TLineGeometryObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineGeometry := LineGeometryClass.GetActiveObj;
       pLineGeometry.NormAmps := Value;
  End

end;
//------------------------------------------------------------------------------
FUNCTION LineGeometries_Get_EmergAmps():Double;cdecl;
Var
   pLineGeometry:TLineGeometryObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineGeometry := LineGeometryClass.GetActiveObj;
       Result := pLineGeometry.EmergAmps;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE LineGeometries_Set_EmergAmps(Value: Double);cdecl;
Var
   pLineGeometry:TLineGeometryObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pLineGeometry := LineGeometryClass.GetActiveObj;
       pLineGeometry.EmergAmps := Value;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE LineGeometries_Set_Units(ValuePtr: PInteger; ValueCount: Integer);cdecl;
VAR
  pLineGeometry:TLineGeometryObj;
  i:Integer;
Begin
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN Begin
         pLineGeometry := LineGeometryClass.GetActiveObj ;
         with pLineGeometry do
         begin
            if Nconds <> ValueCount then
            begin
                DoSimpleMsg('Invalid number of items sent via COM interface.  Please enter a value within range.',183);
                Exit;
            end;
            
            for i:= 1 to ValueCount do Units[i] := ValuePtr[i - 1];
            
            DataChanged := TRUE;
         end;
    End
end;
//------------------------------------------------------------------------------
PROCEDURE LineGeometries_Get_Units(var ResultPtr: PInteger; ResultCount: PInteger);cdecl;
VAR
  Result: PIntegerArray;
  pLineGeometry:TLineGeometryObj;
  i:Integer;
Begin
    Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (0) + 1);
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN Begin
         pLineGeometry := LineGeometryClass.GetActiveObj ;
         with pLineGeometry do
         begin
            DSS_RecreateArray_PInteger(Result, ResultPtr, ResultCount, (Nconds-1) + 1);
            for i:= 1 to Nconds do Result[i - 1] := Units[i];
         end;
    End
end;
PROCEDURE LineGeometries_Get_Units_GR();cdecl;
// Same as LineGeometries_Get_Units but uses global result (GR) pointers
begin
   LineGeometries_Get_Units(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
PROCEDURE LineGeometries_Set_Ycoords(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  pLineGeometry:TLineGeometryObj;
  i:Integer;
Begin
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN Begin
         pLineGeometry := LineGeometryClass.GetActiveObj ;
         with pLineGeometry do
         begin
            if Nconds <> ValueCount then
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
PROCEDURE LineGeometries_Get_Ycoords(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
  pLineGeometry:TLineGeometryObj;
  i:Integer;
Begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN Begin
         pLineGeometry := LineGeometryClass.GetActiveObj ;
         with pLineGeometry do
         begin
            DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, (Nconds-1) + 1);
            for i:= 1 to Nconds do Result[i - 1] := Ycoord[i];
         end;
    End
end;
PROCEDURE LineGeometries_Get_Ycoords_GR();cdecl;
// Same as LineGeometries_Get_Ycoords but uses global result (GR) pointers
begin
   LineGeometries_Get_Ycoords(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE LineGeometries_Set_Xcoords(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  pLineGeometry:TLineGeometryObj;
  i:Integer;
Begin
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN Begin
         pLineGeometry := LineGeometryClass.GetActiveObj ;
         with pLineGeometry do
         begin
            if Nconds <> ValueCount then
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
PROCEDURE LineGeometries_Get_Xcoords(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
  pLineGeometry:TLineGeometryObj;
  i:Integer;
Begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN Begin
         pLineGeometry := LineGeometryClass.GetActiveObj ;
         with pLineGeometry do
         begin
            DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, (Nconds-1) + 1);
            for i:= 1 to Nconds do Result[i - 1] := Xcoord[i];
         end;
    End
end;
PROCEDURE LineGeometries_Get_Xcoords_GR();cdecl;
// Same as LineGeometries_Get_Xcoords but uses global result (GR) pointers
begin
   LineGeometries_Get_Xcoords(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE LineGeometries_Get_Conductors(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  pLineGeometry:TLineGeometryObj;
  i:Integer;
Begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN Begin
         pLineGeometry := LineGeometryClass.GetActiveObj ;
         with pLineGeometry do
         begin
            DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (Nconds-1) + 1);
            for i:= 1 to Nconds do Result[i - 1] := DSS_CopyStringAsPChar(ConductorName[i]);
         end;
    End
end;
PROCEDURE LineGeometries_Get_Conductors_GR();cdecl;
// Same as LineGeometries_Get_Conductors but uses global result (GR) pointers
begin
   LineGeometries_Get_Conductors(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
PROCEDURE LineGeometries_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  LineGeometryElem:TLineGeometryObj;
  k:Integer;

Begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     If LineGeometryClass.ElementList.ListSize  >0 Then
     Begin
       DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (LineGeometryClass.ElementList.ListSize-1) + 1);
       k:=0;
       LineGeometryElem := LineGeometryClass.ElementList.First;
       WHILE LineGeometryElem<>Nil DO
       Begin
          Result[k] := DSS_CopyStringAsPChar(LineGeometryElem.Name);
          Inc(k);
          LineGeometryElem := LineGeometryClass.ElementList.Next;
       End;
     End;

end;
PROCEDURE LineGeometries_Get_AllNames_GR();cdecl;
// Same as LineGeometries_Get_AllNames but uses global result (GR) pointers
begin
   LineGeometries_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
END.
