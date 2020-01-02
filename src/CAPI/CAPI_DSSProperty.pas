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
    DSSClass,
    DSSGlobals,
    Executive,
    SysUtils,
    DSSHelper;

//------------------------------------------------------------------------------
function IsPropIndexInvalid(errorNum: Integer): Boolean;
begin
    Result := FALSE;

    if (DSSPrime.FPropIndex > DSSPrime.ActiveDSSObject.ParentClass.NumProperties) or (DSSPrime.FPropIndex < 1) then
    begin
        DoSimpleMsg(DSSPrime, Format(
            'Invalid property index "%d" for "%s.%s"',
            [DSSPrime.FPropIndex, DSSPrime.ActiveDSSObject.ParentClass.Name, DSSPrime.ActiveDSSObject.Name]),
            errorNum
            );
        Result := TRUE;
    end;
end;
//------------------------------------------------------------------------------
function DSSProperty_Get_Description(): PAnsiChar; CDECL;
begin
    Result := nil;
    if (DSSPrime.ActiveCircuit <> NIL) and (DSSPrime.FPropIndex <> 0) {and (DSSPrime.FPropClass <> Nil)} then
        with DSSPrime.ActiveDSSObject.ParentClass do
            if not IsPropIndexInvalid(33006) then
                Result := DSS_GetAsPAnsiChar(PropertyHelp^[DSSPrime.FPropIndex]);
end;
//------------------------------------------------------------------------------
function DSSProperty_Get_Name(): PAnsiChar; CDECL;
begin
    Result := nil;
    if (DSSPrime.ActiveCircuit <> NIL) and (DSSPrime.FPropIndex <> 0) {and (DSSPrime.FPropClass <> Nil)} then
        with DSSPrime.ActiveDSSObject.ParentClass do
        begin
            if IsPropIndexInvalid(33005) then
                Exit;
            Result := DSS_GetAsPAnsiChar(PropertyName^[DSSPrime.FPropIndex]);
        end;
end;
//------------------------------------------------------------------------------
function DSSProperty_Get_Val(): PAnsiChar; CDECL;
begin
    Result := nil;
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    with DSSPrime.ActiveDSSObject do
    begin
        if IsPropIndexInvalid(33004) then
            Exit;

        Result := DSS_GetAsPAnsiChar(PropertyValue[ParentClass.PropertyIdxMap[DSSPrime.FPropIndex]]);
    end;
end;
//------------------------------------------------------------------------------
procedure DSSProperty_Set_Val(const Value: PAnsiChar); CDECL;
begin
    if DSSPrime.ActiveCircuit = NIL then
        Exit;

    with DSSPrime.ActiveDSSObject do
    begin
        if IsPropIndexInvalid(33001) then
            Exit;
        DSSPrime.DSSExecutive.Command := 'Edit ' + ParentClass.Name + '.' + Name + ' ' + ParentClass.PropertyName^[DSSPrime.FPropIndex] + '=' + (Value);
    end;
end;
//------------------------------------------------------------------------------
procedure DSSProperty_Set_Index(const Value: Integer); CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        DSSPrime.FPropIndex := Value + 1;
        DSSPrime.FPropClass := DSSPrime.ActiveDSSObject.ParentClass;
        if IsPropIndexInvalid(33002) then
            Exit;
    end;
end;
//------------------------------------------------------------------------------
procedure DSSProperty_Set_Name(const Value: PAnsiChar); CDECL;
var
    i: Integer;
begin
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        DSSPrime.FPropClass := DSSPrime.ActiveDSSObject.ParentClass;
        DSSPrime.FPropIndex := 0;
        if DSSPrime.FPropClass <> NIL then
            with DSSPrime.FPropClass do
                for i := 1 to NumProperties do
                begin
                    if CompareText(Value, PropertyName^[i]) = 0 then
                    begin
                        DSSPrime.FPropIndex := i;
                        Exit;
                    end;
                end;

        DoSimpleMsg(DSSPrime, Format(
            'Invalid property name "%s" for "%s.%s"',
            [String(Value), DSSPrime.FPropClass.Name, DSSPrime.ActiveDSSObject.Name]),
            33003
            );
    end;
end;
//------------------------------------------------------------------------------
end.
