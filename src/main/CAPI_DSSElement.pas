UNIT CAPI_DSSElement;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE DSSElement_Get_AllPropertyNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
function DSSElement_Get_Name():PAnsiChar;cdecl;
function DSSElement_Get_NumProperties():Integer;cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, Sysutils;

PROCEDURE DSSElement_Get_AllPropertyNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
VAR
  Result: PPAnsiCharArray;
   k:Integer;
begin
  Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
  IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
     If ActiveDSSObject<>Nil THEN
     WITH ActiveDSSObject DO
     Begin
          WITH ParentClass Do
          Begin
              Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumProperties-1) + 1);
              For k := 1 to NumProperties DO Begin
                  Result[k-1] := DSS_CopyStringAsPChar(PropertyName^[k]);
              End;
          End;
     End
   End;

end;
//------------------------------------------------------------------------------
function DSSElement_Get_Name_AnsiString():AnsiString;inline;
Begin
   If ActiveCircuit <> Nil Then
     if ActiveDSSObject <> Nil then
      WITH ActiveDSSObject DO
      Begin
        Result := ParentClass.Name + '.' + Name;
      End
   Else
      Result := '';
end;

function DSSElement_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(DSSElement_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function DSSElement_Get_NumProperties():Integer;cdecl;
begin
  Result := 0;
  IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
     If ActiveDSSObject<>Nil THEN
     WITH ActiveDSSObject DO
     Begin
          Result := ParentClass.NumProperties ;
     End
   End;
end;
//------------------------------------------------------------------------------
END.
