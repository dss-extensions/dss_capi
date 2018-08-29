UNIT CAPI_DSSElement;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE DSSElement_Get_AllPropertyNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE DSSElement_Get_AllPropertyNames_GR();cdecl;
function DSSElement_Get_Name():PAnsiChar;cdecl;
function DSSElement_Get_NumProperties():Integer;cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, Sysutils;

PROCEDURE DSSElement_Get_AllPropertyNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
   k:Integer;
begin
  Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
  IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit[ActiveActor] DO
   Begin
     If ActiveDSSObject[ActiveActor]<>Nil THEN
     WITH ActiveDSSObject[ActiveActor] DO
     Begin
          WITH ParentClass Do
          Begin
              Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumProperties-1) + 1);
              For k := 1 to NumProperties DO Begin
                  Result[k-1] := DSS_CopyStringAsPChar(PropertyName^[k]);
              End;
          End;
     End
   End;

end;
PROCEDURE DSSElement_Get_AllPropertyNames_GR();cdecl;
// Same as DSSElement_Get_AllPropertyNames but uses global result (GR) pointers
begin
   DSSElement_Get_AllPropertyNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function DSSElement_Get_Name_AnsiString():AnsiString;inline;
Begin
   If ActiveCircuit <> Nil Then
     if ActiveDSSObject <> Nil then
      WITH ActiveDSSObject[ActiveActor] DO
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
   WITH ActiveCircuit[ActiveActor] DO
   Begin
     If ActiveDSSObject[ActiveActor]<>Nil THEN
     WITH ActiveDSSObject[ActiveActor] DO
     Begin
          Result := ParentClass.NumProperties ;
     End
   End;
end;
//------------------------------------------------------------------------------
END.
