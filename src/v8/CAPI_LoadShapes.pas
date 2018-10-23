UNIT CAPI_LoadShapes;
{$inline on}

INTERFACE

USES CAPI_Utils;

function LoadShapes_Get_Name():PAnsiChar;cdecl;
procedure LoadShapes_Set_Name(const Value: PAnsiChar);cdecl;
function LoadShapes_Get_Count():Integer;cdecl;
function LoadShapes_Get_First():Integer;cdecl;
function LoadShapes_Get_Next():Integer;cdecl;
PROCEDURE LoadShapes_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE LoadShapes_Get_AllNames_GR();cdecl;
function LoadShapes_Get_Npts():Integer;cdecl;
PROCEDURE LoadShapes_Get_Pmult(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE LoadShapes_Get_Pmult_GR();cdecl;
PROCEDURE LoadShapes_Get_Qmult(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE LoadShapes_Get_Qmult_GR();cdecl;
procedure LoadShapes_Set_Npts(Value: Integer);cdecl;
procedure LoadShapes_Set_Pmult(ValuePtr: PDouble; ValueCount: Integer);cdecl;
procedure LoadShapes_Set_Qmult(ValuePtr: PDouble; ValueCount: Integer);cdecl;
procedure LoadShapes_Normalize();cdecl;
PROCEDURE LoadShapes_Get_TimeArray(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE LoadShapes_Get_TimeArray_GR();cdecl;
procedure LoadShapes_Set_TimeArray(ValuePtr: PDouble; ValueCount: Integer);cdecl;
function LoadShapes_Get_HrInterval():Double;cdecl;
function LoadShapes_Get_MinInterval():Double;cdecl;
function LoadShapes_Get_sInterval():Double;cdecl;
procedure LoadShapes_Set_HrInterval(Value: Double);cdecl;
procedure LoadShapes_Set_MinInterval(Value: Double);cdecl;
procedure LoadShapes_Set_Sinterval(Value: Double);cdecl;
function LoadShapes_New(const Name: PAnsiChar):Integer;cdecl;
function LoadShapes_Get_PBase():Double;cdecl;
function LoadShapes_Get_Qbase():Double;cdecl;
procedure LoadShapes_Set_PBase(Value: Double);cdecl;
procedure LoadShapes_Set_Qbase(Value: Double);cdecl;
function LoadShapes_Get_UseActual():WordBool;cdecl;
procedure LoadShapes_Set_UseActual(Value: WordBool);cdecl;

IMPLEMENTATION

USES CAPI_Constants, Loadshape, DSSGlobals, PointerList, ExecHelper;

Var
    ActiveLSObject: TLoadshapeObj;

function LoadShapes_Get_Name_AnsiString():AnsiString;inline;
Var
  elem: TLoadshapeObj;
Begin
  Result := '';
  elem := LoadShapeClass[ActiveActor].GetActiveObj;
  If elem <> Nil Then Result := elem.Name;

end;

function LoadShapes_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(LoadShapes_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Name(const Value: PAnsiChar);cdecl;
// Set element active by name

begin
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
          If LoadShapeClass[ActiveActor].SetActive(Value) Then
          Begin
               ActiveLSObject := LoadShapeClass[ActiveActor].ElementList.Active ;
               ActiveDSSObject[ActiveActor]    := ActiveLSObject;
          End
          Else Begin
              DoSimpleMsg('Relay "'+ Value +'" Not Found in Active Circuit.', 77003);
          End;
     End;

end;
//------------------------------------------------------------------------------
function LoadShapes_Get_Count():Integer;cdecl;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
        Result := LoadShapeClass[ActiveActor].ElementList.ListSize;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_First():Integer;cdecl;
Var
   iElem : Integer;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        iElem := LoadShapeClass[ActiveActor].First;
        If iElem <> 0 Then
        Begin
            ActiveLSObject := ActiveDSSObject[ActiveActor] as TLoadShapeObj;
            Result := 1;
        End
     End;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_Next():Integer;cdecl;
Var
   iElem : Integer;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        iElem := LoadShapeClass[ActiveActor].Next;
        If iElem <> 0 Then
        Begin
            ActiveLSObject := ActiveDSSObject[ActiveActor] as TLoadShapeObj;
            Result := iElem;
        End
     End;
end;
//------------------------------------------------------------------------------
PROCEDURE LoadShapes_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  elem: TLoadshapeObj;
  pList: TPointerList;
  k: Integer;

Begin
  Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
  Result[0] := DSS_CopyStringAsPChar('NONE');
  IF ActiveCircuit[ActiveActor] <> Nil THEN
  Begin
      If LoadShapeClass[ActiveActor].ElementList.ListSize > 0 then
      Begin
        pList := LoadShapeClass[ActiveActor].ElementList;
        DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (pList.ListSize -1) + 1);
        k:=0;
        elem := pList.First;
        WHILE elem<>Nil DO Begin
            Result[k] := DSS_CopyStringAsPChar(elem.Name);
            Inc(k);
            elem := pList.next        ;
        End;
      End;
  End;

end;
PROCEDURE LoadShapes_Get_AllNames_GR();cdecl;
// Same as LoadShapes_Get_AllNames but uses global result (GR) pointers
begin
   LoadShapes_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function LoadShapes_Get_Npts():Integer;cdecl;
begin
   Result := 0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     Result := ActiveLSObject.NumPoints;
end;
//------------------------------------------------------------------------------
PROCEDURE LoadShapes_Get_Pmult(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
   k:Integer;

begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
        Result[0] := 0.0;  // error condition: one element array=0
        If ActiveCircuit[ActiveActor] <> Nil Then
         Begin
            If ActiveLSObject <> Nil Then Begin
                 DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, (ActiveLSObject.NumPoints-1) + 1);
                 For k:=0 to ActiveLSObject.NumPoints-1 Do
                      Result[k] := ActiveLSObject.PMultipliers^[k+1];
            End Else Begin
               DoSimpleMsg('No active Loadshape Object found.',61001);
            End;
         End;
end;
PROCEDURE LoadShapes_Get_Pmult_GR();cdecl;
// Same as LoadShapes_Get_Pmult but uses global result (GR) pointers
begin
   LoadShapes_Get_Pmult(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE LoadShapes_Get_Qmult(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
   k:Integer;

begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
        Result[0] := 0.0;  // error condition: one element array=0
        If ActiveCircuit[ActiveActor] <> Nil Then
         Begin
            If ActiveLSObject <> Nil Then
            Begin
              If assigned(ActiveLSObject.QMultipliers) Then
              Begin
                   DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, (ActiveLSObject.NumPoints-1) + 1);
                   For k:=0 to ActiveLSObject.NumPoints-1 Do
                        Result[k] := ActiveLSObject.QMultipliers^[k+1];
              End;
            End Else
            Begin
               DoSimpleMsg('No active Loadshape Object found.',61001);
            End;
         End;
end;
PROCEDURE LoadShapes_Get_Qmult_GR();cdecl;
// Same as LoadShapes_Get_Qmult but uses global result (GR) pointers
begin
   LoadShapes_Get_Qmult(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure LoadShapes_Set_Npts(Value: Integer);cdecl;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
        ActiveLSObject.NumPoints := Value;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Pmult(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  Value: PDoubleArray;
   i, k, LoopLimit: Integer;

begin
    Value := PDoubleArray(ValuePtr);
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        If ActiveLSObject <> Nil Then With ActiveLSObject Do Begin

        // Only put in as many points as we have allocated
         LoopLimit := (ValueCount - 1);
         If (LoopLimit - (0) + 1) > NumPoints  Then
             LoopLimit :=  (0) + NumPoints - 1;

         ReallocMem(PMultipliers, Sizeof(PMultipliers^[1])*NumPoints);
         k := 1;
         for i := (0) to LoopLimit do
         Begin
             ActiveLSObject.Pmultipliers^[k] := Value[i];
             inc(k);
         End;

        End Else Begin
           DoSimpleMsg('No active Loadshape Object found.',61002);
        End;
     End;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Qmult(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  Value: PDoubleArray;
   i, k, LoopLimit: Integer;

begin
    Value := PDoubleArray(ValuePtr);
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        If ActiveLSObject <> Nil Then With ActiveLSObject Do Begin

        // Only put in as many points as we have allocated
         LoopLimit := (ValueCount - 1);
         If (LoopLimit - (0) + 1) > NumPoints  Then
             LoopLimit :=  (0) + NumPoints - 1;

         ReallocMem(QMultipliers, Sizeof(QMultipliers^[1])*NumPoints);
         k := 1;
         for i := (0) to LoopLimit do
         Begin
             ActiveLSObject.Qmultipliers^[k] := Value[i];
             inc(k);
         End;

        End Else Begin
           DoSimpleMsg('No active Loadshape Object found.',61002);
        End;
     End;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Normalize();cdecl;
begin

   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
      ActiveLSObject.Normalize;
end;
//------------------------------------------------------------------------------
PROCEDURE LoadShapes_Get_TimeArray(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
   k:Integer;

begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
        Result[0] := 0.0;  // error condition: one element array=0
        If ActiveCircuit[ActiveActor] <> Nil Then
         Begin
            If ActiveLSObject <> Nil Then Begin
               If ActiveLSObject.hours <> Nil Then  Begin
                 DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, (ActiveLSObject.NumPoints-1) + 1);
                 For k:=0 to ActiveLSObject.NumPoints-1 Do
                      Result[k] := ActiveLSObject.Hours^[k+1];
               End
            End Else Begin
               DoSimpleMsg('No active Loadshape Object found.',61001);
            End;
         End;
end;
PROCEDURE LoadShapes_Get_TimeArray_GR();cdecl;
// Same as LoadShapes_Get_TimeArray but uses global result (GR) pointers
begin
   LoadShapes_Get_TimeArray(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure LoadShapes_Set_TimeArray(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  Value: PDoubleArray;
   i, k, LoopLimit: Integer;

begin
    Value := PDoubleArray(ValuePtr);
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        If ActiveLSObject <> Nil Then With ActiveLSObject Do Begin

        // Only put in as many points as we have allocated
         LoopLimit := (ValueCount - 1);
         If (LoopLimit - (0) + 1) > NumPoints  Then
             LoopLimit :=  (0) + NumPoints - 1;

         ReallocMem(Hours, Sizeof(Hours^[1])*NumPoints);
         k := 1;
         for i := (0) to LoopLimit do
         Begin
             ActiveLSObject.Hours^[k] := Value[i];
             inc(k);
         End;

        End Else Begin
           DoSimpleMsg('No active Loadshape Object found.',61002);
        End;
     End;

end;
//------------------------------------------------------------------------------
function LoadShapes_Get_HrInterval():Double;cdecl;
Begin
   Result := 0.0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     Result := ActiveLSObject.Interval ;

end;
//------------------------------------------------------------------------------
function LoadShapes_Get_MinInterval():Double;cdecl;
begin
   Result := 0.0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     Result := ActiveLSObject.Interval * 60.0 ;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_sInterval():Double;cdecl;
begin
   Result := 0.0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     Result := ActiveLSObject.Interval * 3600.0 ;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_HrInterval(Value: Double);cdecl;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     ActiveLSObject.Interval := Value ;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_MinInterval(Value: Double);cdecl;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     ActiveLSObject.Interval := Value / 60.0 ;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Sinterval(Value: Double);cdecl;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     ActiveLSObject.Interval := Value / 3600.0 ;
end;
//------------------------------------------------------------------------------
function LoadShapes_New(const Name: PAnsiChar):Integer;cdecl;
begin
      Result := AddObject('loadshape', Name);    // Returns handle to object
      ActiveLSObject := ActiveDSSObject[ActiveActor] as TLoadShapeObj;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_PBase():Double;cdecl;
begin
   Result := 0.0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     Result := ActiveLSObject.baseP ;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_Qbase():Double;cdecl;
begin
   Result := 0.0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     Result := ActiveLSObject.baseQ ;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_PBase(Value: Double);cdecl;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     ActiveLSObject.baseP := Value ;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Qbase(Value: Double);cdecl;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     ActiveLSObject.baseQ := Value ;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_UseActual():WordBool;cdecl;
begin
   Result := False;
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     Result := ActiveLSObject.UseActual ;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_UseActual(Value: WordBool);cdecl;
begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   If ActiveLSObject <> Nil Then
     ActiveLSObject.UseActual  := Value ;
end;
//------------------------------------------------------------------------------
END.
