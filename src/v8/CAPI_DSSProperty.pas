unit CAPI_DSSProperty;

{$inline on}

interface

uses
    CAPI_Utils;

function DSSProperty_Get_Description(): PAnsiChar; CDECL;
function DSSProperty_Get_Name(): PAnsiChar; CDECL;
function DSSProperty_Get_Val(): PAnsiChar; CDECL;
procedure DSSProperty_Set_Val(const Value: PAnsiChar); CDECL;
procedure DSSProperty_Set_Index(const Value: Integer); CDECL;
procedure DSSProperty_Set_Name(const Value: PAnsiChar); CDECL;

implementation

uses
    CAPI_Constants,
    CAPI_Globals,
    DSSClass,
    DSSGlobals,
    Executive,
    SysUtils;

//------------------------------------------------------------------------------
function IsPropIndexInvalid(errorNum: Integer): Boolean;
begin
    Result := False;

    if (FPropIndex > ActiveDSSObject[ActiveActor].ParentClass.NumProperties) or (FPropIndex < 1) then
    begin
        DoSimpleMsg(Format(
            'Invalid property index "%d" for "%s.%s"',
            [FPropIndex, ActiveDSSObject[ActiveActor].ParentClass.Name, ActiveDSSObject[ActiveActor].Name]),
            errorNum
        );
        Result := True;
    end;
end;
//------------------------------------------------------------------------------
function DSSProperty_Get_Description_AnsiString(): Ansistring; inline;
begin
    Result := '';
    if (ActiveCircuit[ActiveActor] <> NIL) and (FPropIndex <> 0) {and (FPropClass <> Nil)} then
        with ActiveDSSObject[ActiveActor].ParentClass do
            if not IsPropIndexInvalid(33006) then
                Result := PropertyHelp^[FPropIndex];
end;

function DSSProperty_Get_Description(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSProperty_Get_Description_AnsiString());
end;
//------------------------------------------------------------------------------
function DSSProperty_Get_Name_AnsiString(): Ansistring; inline;
begin
    Result := '';
    if (ActiveCircuit[ActiveActor] <> NIL) and (FPropIndex <> 0) {and (FPropClass <> Nil)} then
        with ActiveDSSObject[ActiveActor].ParentClass do
        begin
            if IsPropIndexInvalid(33005) then Exit;
            Result := PropertyName^[FPropIndex];
        end;
end;

function DSSProperty_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSProperty_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function DSSProperty_Get_Val_AnsiString(): Ansistring; inline;
begin
    Result := '';
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    with ActiveDSSObject[ActiveActor] do
    begin
        if IsPropIndexInvalid(33004) then Exit;
        Result := PropertyValue[ParentClass.PropertyIdxMap[FPropIndex]];
    end;
end;

function DSSProperty_Get_Val(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSProperty_Get_Val_AnsiString());
end;
//------------------------------------------------------------------------------
procedure DSSProperty_Set_Val(const Value: PAnsiChar); CDECL;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;

    with ActiveDSSObject[ActiveActor] do
    begin
        if IsPropIndexInvalid(33001) then Exit;
        DSSExecutive.Command := 'Edit ' + ParentClass.Name + '.' + Name + ' ' + ParentClass.PropertyName^[FPropIndex] + '=' + (Value);
    end;
end;
//------------------------------------------------------------------------------
procedure DSSProperty_Set_Index(const Value: Integer); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        FPropIndex := Value + 1;
        FPropClass := ActiveDSSObject[ActiveActor].ParentClass;
        if IsPropIndexInvalid(33002) then Exit;
    end;
end;
//------------------------------------------------------------------------------
procedure DSSProperty_Set_Name(const Value: PAnsiChar); CDECL;
var
    i: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        FPropClass := ActiveDSSObject[ActiveActor].ParentClass;
        FPropIndex := 0;
        if FPropClass <> NIL then
            with FPropClass do
                for i := 1 to NumProperties do
                begin
                    if CompareText(Value, PropertyName^[i]) = 0 then
                    begin
                        FPropIndex := i;
                        Exit;
                    end;
                end;

        DoSimpleMsg(Format(
            'Invalid property name "%s" for "%s.%s"',
            [String(Value), FPropClass.Name, ActiveDSSObject[ActiveActor].Name]),
            33003
        );
    end;
end;
//------------------------------------------------------------------------------
end.
