UNIT CAPI_Vsources;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE Vsources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE Vsources_Get_AllNames_GR();cdecl;
function Vsources_Get_Count():Integer;cdecl;
function Vsources_Get_First():Integer;cdecl;
function Vsources_Get_Next():Integer;cdecl;
function Vsources_Get_Name():PAnsiChar;cdecl;
procedure Vsources_Set_Name(const Value: PAnsiChar);cdecl;
function Vsources_Get_BasekV():Double;cdecl;
function Vsources_Get_pu():Double;cdecl;
procedure Vsources_Set_BasekV(Value: Double);cdecl;
procedure Vsources_Set_pu(Value: Double);cdecl;
function Vsources_Get_AngleDeg():Double;cdecl;
function Vsources_Get_Frequency():Double;cdecl;
function Vsources_Get_Phases():Integer;cdecl;
procedure Vsources_Set_AngleDeg(Value: Double);cdecl;
procedure Vsources_Set_Frequency(Value: Double);cdecl;
procedure Vsources_Set_Phases(Value: Integer);cdecl;

IMPLEMENTATION

USES CAPI_Constants, Vsource, PointerList, DSSGlobals, CktElement;

PROCEDURE Vsources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  elem: TVsourceObj;
  pList: TPointerList;
  k: Integer;

Begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
        If VsourceClass.ElementList.ListSize > 0 then
        Begin
          pList := VsourceClass.ElementList;
          DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (pList.ListSize -1) + 1);
          k:=0;
          elem := pList.First;
          WHILE elem<>Nil DO Begin
              Result[k] := DSS_CopyStringAsPChar(elem.Name);
              Inc(k);
              elem := pList.next ;
          End;
        End;
    End;

end;
PROCEDURE Vsources_Get_AllNames_GR();cdecl;
// Same as Vsources_Get_AllNames but uses global result (GR) pointers
begin
   Vsources_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Vsources_Get_Count():Integer;cdecl;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
        Result := VsourceClass.ElementList.ListSize;
end;
//------------------------------------------------------------------------------
function Vsources_Get_First():Integer;cdecl;
Var
   pElem : TVsourceObj;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := VsourceClass.ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := 1;
          End
          Else pElem := VsourceClass.ElementList.Next;
        Until (Result = 1) or (pElem = nil);
     End;
end;
//------------------------------------------------------------------------------
function Vsources_Get_Next():Integer;cdecl;
Var
   pElem : TVsourceObj;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := VsourceClass.ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := VsourceClass.ElementList.ActiveIndex;
          End
          Else pElem := VsourceClass.ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
end;
//------------------------------------------------------------------------------
function Vsources_Get_Name_AnsiString():AnsiString;inline;
Var
   elem: TDSSCktElement;
Begin
    Result := '';
    elem := ActiveCircuit[ActiveActor].ActiveCktElement;
    If elem <> Nil Then Result := elem.Name;
end;

function Vsources_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Vsources_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_Name(const Value: PAnsiChar);cdecl;
// Set element active by name

begin
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
          If VsourceClass.SetActive(Value) Then
          Begin
               ActiveCircuit[ActiveActor].ActiveCktElement := VsourceClass.ElementList.Active ;
          End
          Else Begin
              DoSimpleMsg('Vsource "'+ Value +'" Not Found in Active Circuit.', 77003);
          End;
     End;
end;
//------------------------------------------------------------------------------
function Vsources_Get_BasekV():Double;cdecl;
var
  elem: TVsourceObj;
begin
  Result := 0.0;
  elem := VsourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.kVBase ;
end;
//------------------------------------------------------------------------------
function Vsources_Get_pu():Double;cdecl;
var
  elem: TVsourceObj;
begin
  Result := 0.0;
  elem := VsourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.perunit ;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_BasekV(Value: Double);cdecl;
var
  elem: TVsourceObj;
begin
  elem := VsourceClass.GetActiveObj ;
  if elem <> nil then elem.kVBase := Value;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_pu(Value: Double);cdecl;
var
  elem: TVsourceObj;
begin
  elem := VsourceClass.GetActiveObj ;
  if elem <> nil then elem.PerUnit := Value;
end;
//------------------------------------------------------------------------------
function Vsources_Get_AngleDeg():Double;cdecl;
var
  elem: TVsourceObj;
begin
  Result := 0.0;
  elem := VsourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.angle ;

end;
//------------------------------------------------------------------------------
function Vsources_Get_Frequency():Double;cdecl;
var
  elem: TVsourceObj;
begin
  Result := 0.0;
  elem := VsourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.SrcFrequency  ;

end;
//------------------------------------------------------------------------------
function Vsources_Get_Phases():Integer;cdecl;
var
  elem: TVsourceObj;
begin
  Result := 0;
  elem := VsourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.NPhases ;

end;
//------------------------------------------------------------------------------
procedure Vsources_Set_AngleDeg(Value: Double);cdecl;
var
  elem: TVsourceObj;
begin
  elem := VsourceClass.GetActiveObj ;
  if elem <> nil then elem.Angle := Value;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_Frequency(Value: Double);cdecl;
var
  elem: TVsourceObj;
begin
  elem := VsourceClass.GetActiveObj ;
  if elem <> nil then elem.SrcFrequency := Value;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_Phases(Value: Integer);cdecl;
var
  elem: TVsourceObj;
begin
  elem := VsourceClass.GetActiveObj ;
  if elem <> nil then elem.Nphases := Value;
end;
//------------------------------------------------------------------------------
END.
