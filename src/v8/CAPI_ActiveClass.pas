UNIT CAPI_ActiveClass;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE ActiveClass_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
function ActiveClass_Get_First():Integer;cdecl;
function ActiveClass_Get_Next():Integer;cdecl;
function ActiveClass_Get_Name():PAnsiChar;cdecl;
procedure ActiveClass_Set_Name(const Value: PAnsiChar);cdecl;
function ActiveClass_Get_NumElements():Integer;cdecl;
function ActiveClass_Get_ActiveClassName():PAnsiChar;cdecl;
function ActiveClass_Get_Count():Integer;cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, DSSObject, CktElement;

PROCEDURE ActiveClass_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  idx: Integer;
  k:Integer;

Begin
    If (ActiveCircuit[ActiveActor] <> Nil) and Assigned(ActiveDSSClass) Then
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (ActiveDSSClass[ActiveActor].ElementCount-1) + 1);
       k:=0;
       idx := ActiveDSSClass[ActiveActor].First;
       WHILE idx > 0 DO  Begin
          Result[k] := DSS_CopyStringAsPChar(ActiveDSSObject[ActiveActor].Name);
          Inc(k);
          idx := ActiveDSSClass[ActiveActor].Next;
       End;
     End
    ELSE Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);

end;
//------------------------------------------------------------------------------
function ActiveClass_Get_First():Integer;cdecl;
Begin

   Result := 0;
   If (ActiveCircuit[ActiveActor] <> Nil) and Assigned(ActiveDSSClass[ActiveActor]) Then
   Begin
        Result := ActiveDSSClass[ActiveActor].First;  // sets active objects
   End;

end;
//------------------------------------------------------------------------------
function ActiveClass_Get_Next():Integer;cdecl;
Begin

   Result := 0;
   If (ActiveCircuit[ActiveActor] <> Nil) and Assigned(ActiveDSSClass[ActiveActor]) Then
   Begin
        Result := ActiveDSSClass[ActiveActor].Next;  // sets active objects
   End;

end;
//------------------------------------------------------------------------------
function ActiveClass_Get_Name_AnsiString():AnsiString;inline;
begin
      if Assigned(ActiveDSSObject[ActiveActor]) then  Result := ActiveDSSObject[ActiveActor].Name
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
     If  Assigned(ActiveDSSClass[ActiveActor]) Then  Begin
         pelem := ActiveDSSClass[ActiveActor].Find(Value);
         if pelem <> Nil then Begin
            if pelem is TDSSCktElement then
             ActiveCircuit[ActiveActor].ActiveCktElement := TDSSCktElement(pelem)  // sets ActiveDSSobject
          Else
             ActiveDSSObject[ActiveActor] := pelem;
         End;
     End;
end;
//------------------------------------------------------------------------------
function ActiveClass_Get_NumElements():Integer;cdecl;
begin
    if Assigned(ActiveDSSClass[ActiveActor]) then  Result := ActiveDSSCLass[ActiveActor].ElementCount
     Else Result := 0;
end;
//------------------------------------------------------------------------------
function ActiveClass_Get_ActiveClassName_AnsiString():AnsiString;inline;
begin
     if Assigned(ActiveDSSClass[ActiveActor]) then  Result := ActiveDSSCLass[ActiveActor].Name
     Else Result := '';
end;

function ActiveClass_Get_ActiveClassName():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(ActiveClass_Get_ActiveClassName_AnsiString());
end;
//------------------------------------------------------------------------------
function ActiveClass_Get_Count():Integer;cdecl;
begin
     if Assigned(ActiveDSSClass[ActiveActor]) then  Result := ActiveDSSCLass[ActiveActor].ElementCount
     Else Result := 0;
end;
//------------------------------------------------------------------------------
END.
