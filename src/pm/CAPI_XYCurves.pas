UNIT CAPI_XYCurves;
{$inline on}

INTERFACE

USES CAPI_Utils, XYCurve, DSSClass;

function XYCurves_Get_Count():Integer;cdecl;
function XYCurves_Get_First():Integer;cdecl;
function XYCurves_Get_Name():PAnsiChar;cdecl;
function XYCurves_Get_Next():Integer;cdecl;
procedure XYCurves_Set_Name(const Value: PAnsiChar);cdecl;
function XYCurves_Get_Npts():Integer;cdecl;
PROCEDURE XYCurves_Get_Xarray(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
procedure XYCurves_Set_Npts(Value: Integer);cdecl;
procedure XYCurves_Set_Xarray(ValuePtr: PDouble; ValueCount: Integer);cdecl;
function XYCurves_Get_x():Double;cdecl;
function XYCurves_Get_y():Double;cdecl;
PROCEDURE XYCurves_Get_Yarray(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
procedure XYCurves_Set_x(Value: Double);cdecl;
procedure XYCurves_Set_y(Value: Double);cdecl;
function XYCurves_Get_Xscale():Double;cdecl;
function XYCurves_Get_Xshift():Double;cdecl;
function XYCurves_Get_Yscale():Double;cdecl;
function XYCurves_Get_Yshift():Double;cdecl;
procedure XYCurves_Set_Xscale(Value: Double);cdecl;
procedure XYCurves_Set_Xshift(Value: Double);cdecl;
procedure XYCurves_Set_Yscale(Value: Double);cdecl;
procedure XYCurves_Set_Yshift(Value: Double);cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, DSSObject;

function XYCurves_Get_Count():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := XYCurveClass[ActiveActor].ElementCount;
end;
//------------------------------------------------------------------------------
function XYCurves_Get_First():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := XYCurveClass[ActiveActor].First;
end;
//------------------------------------------------------------------------------
function XYCurves_Get_Name_AnsiString():AnsiString;inline;
Var
   pXYCurve:TXYCurveObj;

Begin
   Result := '';  // signify no name
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pXYCurve := XYCurveClass[ActiveActor].GetActiveObj ;
        If pXYCurve <> Nil Then
        Begin
              Result := pXYCurve.Name;
        End;
   End;

end;

function XYCurves_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(XYCurves_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function XYCurves_Get_Next():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := XYCurveClass[ActiveActor].Next;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Name(const Value: PAnsiChar);cdecl;
// set XYCurve active by name

Begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        If Not XYCurveClass[ActiveActor].SetActive (Value) Then
         DoSimpleMsg('XYCurve "'+ Value +'" Not Found in Active Circuit.', 51008);

         // Still same active object if not found
   End;

end;
//------------------------------------------------------------------------------
function XYCurves_Get_Npts():Integer;cdecl;
Var
   pXYCurve:TXYCurveObj;

begin
    Result := 0;
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
        If pXYCurve <> Nil Then Begin
            Result := pXYCurve.NumPoints;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51009);
        End;
     End;
end;
//------------------------------------------------------------------------------
PROCEDURE XYCurves_Get_Xarray(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray;
   pXYCurve:TXYCurveObj;
   k:Integer;

begin
        Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
        Result[0] := 0.0;  // error condition: one element array=0
        If ActiveCircuit[ActiveActor] <> Nil Then
         Begin
            pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
            If pXYCurve <> Nil Then Begin
                 DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, (pXYCurve.NumPoints-1) + 1);
                 For k:=0 to pXYCurve.NumPoints-1 Do
                      Result[k] := pXYCurve.XValue_pt[k+1];
            End Else Begin
               DoSimpleMsg('No active XYCurve Object found.',51013);
            End;
         End;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Npts(Value: Integer);cdecl;
Var
   pXYCurve:TXYCurveObj;

begin
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
        If pXYCurve <> Nil Then Begin
            pXYCurve.NumPoints := Value;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51014);
        End;
     End;

end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Xarray(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  Value: PDoubleArray;
   pXYCurve:TXYCurveObj;
   i, k, LoopLimit: Integer;

begin
    Value := PDoubleArray(ValuePtr);
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
        If pXYCurve <> Nil Then Begin

        // Only put in as many points as we have allocated
         LoopLimit := (ValueCount - 1);
         If (LoopLimit - (0) + 1) > pXYCurve.NumPoints  Then   LoopLimit :=  (0) + pXYCurve.NumPoints - 1;

         k := 1;
         for i := (0) to LoopLimit do
         Begin
             pXYCurve.XValue_pt[k] := Value[i];
             inc(k);
         End;

        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51015);
        End;
     End;

end;
//------------------------------------------------------------------------------
function XYCurves_Get_x():Double;cdecl;
Var
   pXYCurve:TXYCurveObj;

begin
    Result := 0;
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
        If pXYCurve <> Nil Then Begin
            Result := pXYCurve.X;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51010);
        End;
     End;
end;
//------------------------------------------------------------------------------
function XYCurves_Get_y():Double;cdecl;
Var
   pXYCurve:TXYCurveObj;

begin
    Result := 0;
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
        If pXYCurve <> Nil Then Begin
            Result := pXYCurve.Y;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51011);
        End;
     End;

end;
//------------------------------------------------------------------------------
PROCEDURE XYCurves_Get_Yarray(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray;
   pXYCurve:TXYCurveObj;
   k:Integer;

begin
        Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
        Result[0] := 0.0;  // error condition: one element array=0
        If ActiveCircuit[ActiveActor] <> Nil Then
         Begin
            pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
            If pXYCurve <> Nil Then Begin
                 DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, (pXYCurve.NumPoints-1) + 1);
                 For k:=0 to pXYCurve.NumPoints-1 Do
                      Result[k] := pXYCurve.YValue_pt[k+1];
            End Else Begin
               DoSimpleMsg('No active XYCurve Object found.',51013);
            End;
         End;

end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_x(Value: Double);cdecl;
Var
   pXYCurve:TXYCurveObj;

begin
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
        If pXYCurve <> Nil Then Begin
            pXYCurve.X := Value;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51010);
        End;
     End;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_y(Value: Double);cdecl;
Var
   pXYCurve:TXYCurveObj;

begin
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
        If pXYCurve <> Nil Then Begin
            pXYCurve.Y := Value;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51010);
        End;
     End;
end;
//------------------------------------------------------------------------------
function XYCurves_Get_Xscale():Double;cdecl;
Var
   pXYCurve:TXYCurveObj;

begin
    Result := 0;
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
        If pXYCurve <> Nil Then Begin
            Result := pXYCurve.FXscale;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51011);
        End;
     End;

end;
//------------------------------------------------------------------------------
function XYCurves_Get_Xshift():Double;cdecl;
Var
   pXYCurve:TXYCurveObj;

begin
    Result := 0;
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
        If pXYCurve <> Nil Then Begin
            Result := pXYCurve.FXshift;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51011);
        End;
     End;


end;
//------------------------------------------------------------------------------
function XYCurves_Get_Yscale():Double;cdecl;
Var
   pXYCurve:TXYCurveObj;

begin
    Result := 0;
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
        If pXYCurve <> Nil Then Begin
            Result := pXYCurve.FYscale;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51011);
        End;
     End;


end;
//------------------------------------------------------------------------------
function XYCurves_Get_Yshift():Double;cdecl;
Var
   pXYCurve:TXYCurveObj;

begin
    Result := 0;
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
        If pXYCurve <> Nil Then Begin
            Result := pXYCurve.FYshift;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51011);
        End;
     End;

end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Xscale(Value: Double);cdecl;
Var
   pXYCurve:TXYCurveObj;

begin
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
        If pXYCurve <> Nil Then Begin
            pXYCurve.FXScale := Value;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51010);
        End;
     End;

end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Xshift(Value: Double);cdecl;
Var
   pXYCurve:TXYCurveObj;

begin
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
        If pXYCurve <> Nil Then Begin
            pXYCurve.FXShift := Value;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51010);
        End;
     End;

end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Yscale(Value: Double);cdecl;
Var
   pXYCurve:TXYCurveObj;

begin
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
        If pXYCurve <> Nil Then Begin
            pXYCurve.FYScale := Value;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51010);
        End;
     End;

end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Yshift(Value: Double);cdecl;
Var
   pXYCurve:TXYCurveObj;

begin
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
        If pXYCurve <> Nil Then Begin
            pXYCurve.FYShift := Value;
        End Else Begin
           DoSimpleMsg('No active XYCurve Object found.',51010);
        End;
     End;

end;
//------------------------------------------------------------------------------
END.
