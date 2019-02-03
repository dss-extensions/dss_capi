UNIT CAPI_GICSources;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE GICSources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE GICSources_Get_AllNames_GR();cdecl;
function GICSources_Get_Count():Integer;cdecl;
function GICSources_Get_First():Integer;cdecl;
function GICSources_Get_Next():Integer;cdecl;
function GICSources_Get_Name():PAnsiChar;cdecl;
procedure GICSources_Set_Name(const Value: PAnsiChar);cdecl;
function GICSources_Get_Phases():Integer;cdecl;
procedure GICSources_Set_Phases(Value: Integer);cdecl;
function GICSources_Get_Bus1():PAnsiChar;cdecl;
function GICSources_Get_Bus2():PAnsiChar;cdecl;
function GICSources_Get_EN():Double;cdecl;
procedure GICSources_Set_EN(Value: Double);cdecl;
function GICSources_Get_EE():Double;cdecl;
procedure GICSources_Set_EE(Value: Double);cdecl;
function GICSources_Get_Lat1():Double;cdecl;
procedure GICSources_Set_Lat1(Value: Double);cdecl;
function GICSources_Get_Lat2():Double;cdecl;
procedure GICSources_Set_Lat2(Value: Double);cdecl;
function GICSources_Get_Lon1():Double;cdecl;
procedure GICSources_Set_Lon1(Value: Double);cdecl;
function GICSources_Get_Lon2():Double;cdecl;
procedure GICSources_Set_Lon2(Value: Double);cdecl;
function GICSources_Get_Volts():Double;cdecl;
procedure GICSources_Set_Volts(Value: Double);cdecl;

IMPLEMENTATION

USES CAPI_Constants, GICsource, PointerList, DSSGlobals, CktElement;
//------------------------------------------------------------------------------
PROCEDURE GICSources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  elem: TGICSourceObj;
  pList: TPointerList;
  k: Integer;

Begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
        If GICsourceClass.ElementList.ListSize > 0 then
        Begin
          pList := GICsourceClass.ElementList;
          DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (pList.ListSize -1) + 1);
          k:=0;
          elem := pList.First;
          WHILE elem<>Nil DO Begin
              Result[k] := DSS_CopyStringAsPChar(elem.Name);
              Inc(k);
              elem := pList.next;
          End;
        End;
    End;

end;
PROCEDURE GICSources_Get_AllNames_GR();cdecl;
// Same as GICSources_Get_AllNames but uses global result (GR) pointers
begin
   GICSources_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;
//------------------------------------------------------------------------------
function GICSources_Get_Count():Integer;cdecl;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
        Result := GICsourceClass.ElementList.ListSize;
end;
//------------------------------------------------------------------------------
function GICSources_Get_First():Integer;cdecl;
Var
   pElem : TGICSourceObj;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := GICsourceClass.ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := 1;
          End
          Else pElem := GICsourceClass.ElementList.Next;
        Until (Result = 1) or (pElem = nil);
     End;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Next():Integer;cdecl;
Var
   pElem : TGICSourceObj;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := GICsourceClass.ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := GICsourceClass.ElementList.ActiveIndex;
          End
          Else pElem := GICsourceClass.ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Name_AnsiString():AnsiString;inline;
Var
   elem: TDSSCktElement;
Begin
    Result := '';
    If ActiveCircuit[ActiveActor] = Nil Then Exit;
    elem := ActiveCircuit[ActiveActor].ActiveCktElement;
    If elem <> Nil Then Result := elem.Name;
end;

function GICSources_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(GICSources_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Name(const Value: PAnsiChar);cdecl;
begin
  If ActiveCircuit[ActiveActor] = Nil Then Exit;
  If GICsourceClass.SetActive(Value) Then
  Begin
       ActiveCircuit[ActiveActor].ActiveCktElement := GICsourceClass.ElementList.Active;
  End
  Else 
  Begin
      DoSimpleMsg('GICSource "'+ Value +'" Not Found in Active Circuit.', 77003);
  End;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Phases():Integer;cdecl;
var
  elem: TGICSourceObj;
begin
  Result := 0;
  elem := GICsourceClass.ElementList.Active;
  if elem <> nil then Result := elem.NPhases;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Phases(Value: Integer);cdecl;
var
  elem: TGICSourceObj;
begin
  elem := GICsourceClass.GetActiveObj;
  if elem <> nil then
  Begin
    elem.nphases := Value;
    Elem.NConds := Value;  // Force reallocation of terminal info
  End;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Bus1_AnsiString():AnsiString;inline;
begin
  Result := '';
  If ActiveCircuit[ActiveActor] = Nil Then Exit;  
  Result := ActiveCircuit[ActiveActor].ActiveCktElement.GetBus(1);
end;

function GICSources_Get_Bus1():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(GICSources_Get_Bus1_AnsiString());
end;
//------------------------------------------------------------------------------
function GICSources_Get_Bus2_AnsiString():AnsiString;inline;
begin
  Result := '';
  If ActiveCircuit[ActiveActor] = Nil Then Exit;
  Result := ActiveCircuit[ActiveActor].ActiveCktElement.GetBus(2);
end;

function GICSources_Get_Bus2():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(GICSources_Get_Bus2_AnsiString());
end;
//------------------------------------------------------------------------------
function GICSources_Get_EN():Double;cdecl;
var
  elem: TGICsourceObj;
begin
  Result := 0.0;
  elem := GICSourceClass.ElementList.Active;
  if elem <> nil then Result := elem.ENorth;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_EN(Value: Double);cdecl;
var
  elem: TGICsourceObj;
begin
  elem := GICSourceClass.ElementList.Active;
  if elem <> nil then elem.ENorth := Value;
end;
//------------------------------------------------------------------------------
function GICSources_Get_EE():Double;cdecl;
var
  elem: TGICsourceObj;
begin
  Result := 0.0;
  elem := GICSourceClass.ElementList.Active;
  if elem <> nil then Result := elem.EEast;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_EE(Value: Double);cdecl;
var
  elem: TGICsourceObj;
begin
  elem := GICSourceClass.ElementList.Active;
  if elem <> nil then elem.EEast := Value;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Lat1():Double;cdecl;
var
  elem: TGICsourceObj;
begin
  Result := 0.0;
  elem := GICSourceClass.ElementList.Active;
  if elem <> nil then Result := elem.Lat1;

end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Lat1(Value: Double);cdecl;
var
  elem: TGICsourceObj;
begin
  elem := GICSourceClass.ElementList.Active;
  if elem <> nil then 
  Begin
     elem.Lat1 := Value;
     elem.VoltsSpecified := FALSE;
  End;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Lat2():Double;cdecl;
var
  elem: TGICsourceObj;
begin
  Result := 0.0;
  elem := GICSourceClass.ElementList.Active;
  if elem <> nil then Result := elem.Lat2;

end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Lat2(Value: Double);cdecl;
var
  elem: TGICsourceObj;
begin
  elem := GICSourceClass.ElementList.Active;
  if elem <> nil then
  Begin
     elem.Lat2 := Value;
     elem.VoltsSpecified := FALSE;
  End;

end;
//------------------------------------------------------------------------------
function GICSources_Get_Lon1():Double;cdecl;
var
  elem: TGICsourceObj;
begin
  Result := 0.0;
  elem := GICSourceClass.ElementList.Active;
  if elem <> nil then Result := elem.Lon1;

end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Lon1(Value: Double);cdecl;
var
  elem: TGICsourceObj;
begin
  elem := GICSourceClass.ElementList.Active;
  if elem <> nil then 
  Begin
     elem.Lon1 := Value;
     elem.VoltsSpecified := FALSE;
  End;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Lon2():Double;cdecl;
var
  elem: TGICsourceObj;
begin
  Result := 0.0;
  elem := GICSourceClass.ElementList.Active;
  if elem <> nil then Result := elem.Lon2;

end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Lon2(Value: Double);cdecl;
var
  elem: TGICsourceObj;
begin
  elem := GICSourceClass.ElementList.Active;
  if elem <> nil then
  Begin
     elem.Lon2 := Value;
     elem.VoltsSpecified := FALSE;
  End;

end;
//------------------------------------------------------------------------------
function GICSources_Get_Volts():Double;cdecl;
var
  elem: TGICsourceObj;
begin
  Result := 0.0;
  elem := GICSourceClass.ElementList.Active;
  if elem <> nil then Result := elem.Volts;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Volts(Value: Double);cdecl;
var
  elem: TGICsourceObj;
begin
  elem := GICSourceClass.ElementList.Active;
  if elem <> nil then 
  Begin
     elem.Volts := Value;
     elem.VoltsSpecified := TRUE;
  End;
end;
//------------------------------------------------------------------------------
END.
