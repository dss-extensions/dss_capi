UNIT CAPI_ISources;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE ISources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
function ISources_Get_Count():Integer;cdecl;
function ISources_Get_First():Integer;cdecl;
function ISources_Get_Next():Integer;cdecl;
function ISources_Get_Name():PAnsiChar;cdecl;
procedure ISources_Set_Name(const Value: PAnsiChar);cdecl;
function ISources_Get_Amps():Double;cdecl;
procedure ISources_Set_Amps(Value: Double);cdecl;
function ISources_Get_AngleDeg():Double;cdecl;
function ISources_Get_Frequency():Double;cdecl;
procedure ISources_Set_AngleDeg(Value: Double);cdecl;
procedure ISources_Set_Frequency(Value: Double);cdecl;

IMPLEMENTATION

USES CAPI_Constants, PointerList, Isource, DSSGlobals, CktElement;

PROCEDURE ISources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  elem: TIsourceObj;
  pList: TPointerList;
  k: Integer;

Begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
        If IsourceClass.ElementList.ListSize > 0 then
        Begin
          pList := IsourceClass.ElementList;
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
//------------------------------------------------------------------------------
function ISources_Get_Count():Integer;cdecl;
Begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
        Result := IsourceClass.ElementList.ListSize;
end;
//------------------------------------------------------------------------------
function ISources_Get_First():Integer;cdecl;
Var
   pElem : TIsourceObj;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := IsourceClass.ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := 1;
          End
          Else pElem := IsourceClass.ElementList.Next;
        Until (Result = 1) or (pElem = nil);
     End;
end;
//------------------------------------------------------------------------------
function ISources_Get_Next():Integer;cdecl;
Var
   pElem : TIsourceObj;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := IsourceClass.ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := IsourceClass.ElementList.ActiveIndex;
          End
          Else pElem := IsourceClass.ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
end;
//------------------------------------------------------------------------------
function ISources_Get_Name_AnsiString():AnsiString;inline;
Var
   elem: TDSSCktElement;
Begin
    Result := '';
    elem := ActiveCircuit[ActiveActor].ActiveCktElement;
    If elem <> Nil Then Result := elem.Name;
end;

function ISources_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(ISources_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure ISources_Set_Name(const Value: PAnsiChar);cdecl;
// Set element active by name

begin
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
          If IsourceClass.SetActive(Value) Then
          Begin
               ActiveCircuit[ActiveActor].ActiveCktElement := IsourceClass.ElementList.Active ;
          End
          Else Begin
              DoSimpleMsg('Isource "'+ Value +'" Not Found in Active Circuit.', 77003);
          End;
     End;
end;
//------------------------------------------------------------------------------
function ISources_Get_Amps():Double;cdecl;
var
  elem: TIsourceObj;
begin
  Result := 0.0;
  elem := IsourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.Amps   ;
end;
//------------------------------------------------------------------------------
procedure ISources_Set_Amps(Value: Double);cdecl;
var
  elem: TIsourceObj;
begin
  elem := IsourceClass.GetActiveObj ;
  if elem <> nil then elem.Amps := Value;
end;
//------------------------------------------------------------------------------
function ISources_Get_AngleDeg():Double;cdecl;
var
  elem: TIsourceObj;
begin
  Result := 0.0;
  elem := IsourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.Angle ;
end;
//------------------------------------------------------------------------------
function ISources_Get_Frequency():Double;cdecl;
var
  elem: TIsourceObj;
begin
  Result := 0.0;
  elem := IsourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.SrcFrequency  ;
end;
//------------------------------------------------------------------------------
procedure ISources_Set_AngleDeg(Value: Double);cdecl;
var
  elem: TIsourceObj;
begin
  elem := IsourceClass.GetActiveObj ;
  if elem <> nil then elem.Angle := Value;
end;
//------------------------------------------------------------------------------
procedure ISources_Set_Frequency(Value: Double);cdecl;
var
  elem: TIsourceObj;
begin
  elem := IsourceClass.GetActiveObj ;
  if elem <> nil then elem.SrcFrequency := Value;
end;
//------------------------------------------------------------------------------
END.
