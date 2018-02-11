UNIT CAPI_DSSProperty;
{$inline on}

INTERFACE

USES CAPI_Utils;

function DSSProperty_Get_Description():PAnsiChar;cdecl;
function DSSProperty_Get_Name():PAnsiChar;cdecl;
function DSSProperty_Get_Val():PAnsiChar;cdecl;
procedure DSSProperty_Set_Val(const Value: PAnsiChar);cdecl;

IMPLEMENTATION

USES CAPI_Constants, CAPI_Globals, DSSClass, DSSGlobals, Executive, SysUtils;

function DSSProperty_Get_Description_AnsiString():AnsiString;inline;
begin
      Result := '';
      If (ActiveCircuit<> Nil) and (FPropIndex <> 0) {and (FPropClass <> Nil)} Then
      With  ActiveDSSObject.ParentClass Do
        If FPropIndex <= NumProperties Then
          Result := PropertyHelp^[FPropIndex];

end;

function DSSProperty_Get_Description():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(DSSProperty_Get_Description_AnsiString());
end;
//------------------------------------------------------------------------------
function DSSProperty_Get_Name_AnsiString():AnsiString;inline;
begin
      Result := '';
      If (ActiveCircuit<> Nil) and (FPropIndex <> 0) {and (FPropClass <> Nil)} Then
        With  ActiveDSSObject.ParentClass   Do
        If FPropIndex <= NumProperties Then
          Result := PropertyName^[FPropIndex];

end;

function DSSProperty_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(DSSProperty_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function DSSProperty_Get_Val_AnsiString():AnsiString;inline;
begin
       Result := '';
      If (ActiveCircuit<> Nil)
      THEN  With ActiveDSSObject Do
        If FPropIndex <= ParentClass.NumProperties Then
              Result := PropertyValue[ParentClass.PropertyIdxMap[FPropIndex]];

end;

function DSSProperty_Get_Val():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(DSSProperty_Get_Val_AnsiString());
end;
//------------------------------------------------------------------------------
procedure DSSProperty_Set_Val(const Value: PAnsiChar);cdecl;
begin
      If (ActiveCircuit<> Nil)
      THEN  With ActiveDSSObject Do
        If FPropIndex <= ParentClass.NumProperties Then
              DSSExecutive.Command := 'Edit ' + ParentClass.Name + '.' + Name + ' ' +
                     ParentClass.PropertyName^[FPropIndex] + '=' +
                     String(Value);
End;
//------------------------------------------------------------------------------
END.
