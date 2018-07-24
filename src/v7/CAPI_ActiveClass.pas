UNIT CAPI_ActiveClass;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE ActiveClass_Get_AllNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
function ActiveClass_Get_First():Integer;cdecl;
function ActiveClass_Get_Next():Integer;cdecl;
function ActiveClass_Get_Name():PAnsiChar;cdecl;
procedure ActiveClass_Set_Name(const Value: PAnsiChar);cdecl;
function ActiveClass_Get_NumElements():Integer;cdecl;
function ActiveClass_Get_ActiveClassName():PAnsiChar;cdecl;
function ActiveClass_Get_Count():Integer;cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, DSSObject, CktElement;

PROCEDURE ActiveClass_Get_AllNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
VAR
  Result: PPAnsiCharArray;
  idx: Integer;
  k:Integer;

Begin
    If (ActiveCircuit <> Nil) and Assigned(ActiveDSSClass) Then
     WITH ActiveCircuit DO
     Begin
       Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (ActiveDSSClass.ElementCount-1) + 1);
       k:=0;
       idx := ActiveDSSClass.First;
       WHILE idx > 0 DO  Begin
          Result[k] := DSS_CopyStringAsPChar(ActiveDSSObject.Name);
          Inc(k);
          idx := ActiveDSSClass.Next;
       End;
     End
    ELSE Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);

end;
//------------------------------------------------------------------------------
function ActiveClass_Get_First():Integer;cdecl;
Begin

   Result := 0;
   If (ActiveCircuit <> Nil) and Assigned(ActiveDSSClass) Then
   Begin
        Result := ActiveDSSClass.First;  // sets active objects
   End;

end;
//------------------------------------------------------------------------------
function ActiveClass_Get_Next():Integer;cdecl;
Begin

   Result := 0;
   If (ActiveCircuit <> Nil) and Assigned(ActiveDSSClass) Then
   Begin
        Result := ActiveDSSClass.Next;  // sets active objects
   End;

end;
//------------------------------------------------------------------------------
function ActiveClass_Get_Name_AnsiString():AnsiString;inline;
begin
      if Assigned(ActiveDSSObject) then  Result := ActiveDSSObject.Name
      Else Result := '';
end;

function ActiveClass_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(ActiveClass_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure ActiveClass_Set_Name(const Value: PAnsiChar);cdecl;
// set object active by name
Var
  pelem:TDSSObject;
begin
     If  Assigned(ActiveDSSClass) Then  Begin
         pelem := ActiveDSSClass.Find(Value);
         if pelem <> Nil then Begin
            if pelem is TDSSCktElement then
             ActiveCircuit.ActiveCktElement := TDSSCktElement(pelem)  // sets ActiveDSSobject
          Else
             ActiveDSSObject := pelem;
         End;
     End;
end;
//------------------------------------------------------------------------------
function ActiveClass_Get_NumElements():Integer;cdecl;
begin
    if Assigned(ActiveDSSClass) then  Result := ActiveDSSCLass.ElementCount
     Else Result := 0;
end;
//------------------------------------------------------------------------------
function ActiveClass_Get_ActiveClassName_AnsiString():AnsiString;inline;
begin
     if Assigned(ActiveDSSClass) then  Result := ActiveDSSCLass.Name
     Else Result := '';
end;

function ActiveClass_Get_ActiveClassName():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(ActiveClass_Get_ActiveClassName_AnsiString());
end;
//------------------------------------------------------------------------------
function ActiveClass_Get_Count():Integer;cdecl;
begin
     if Assigned(ActiveDSSClass) then  Result := ActiveDSSCLass.ElementCount
     Else Result := 0;
end;
//------------------------------------------------------------------------------
END.
