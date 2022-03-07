unit CAPI_DSSProperty;

interface

uses
    CAPI_Utils,
    CAPI_Types;

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
function IsPropIndexInvalid(DSS: TDSSContext; errorNum: Integer): Boolean;
begin
    Result := FALSE;

    if (DSS.FPropIndex > DSS.ActiveDSSObject.ParentClass.NumProperties) or (DSS.FPropIndex < 1) then
    begin
        DoSimpleMsg(DSS,
            'Invalid property index "%d" for "%s"',
            [DSS.FPropIndex, DSS.ActiveDSSObject.FullName],
            errorNum
        );
        Result := TRUE;
    end;
end;
//------------------------------------------------------------------------------
function DSSProperty_Get_Description(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if InvalidCircuit(DSSPrime) then
        Exit;
        
    if DSSPrime.ActiveDSSObject = NIL then
    begin
        DoSimpleMsg(DSSPrime, _('No active DSS object found! Activate one and try again.'), 33100);
        Exit;
    end;

    with DSSPrime.ActiveDSSObject.ParentClass do
        if not IsPropIndexInvalid(DSSPrime, 33006) then
            Result := DSS_GetAsPAnsiChar(DSSPrime, GetPropertyHelp(DSSPrime.FPropIndex));
end;
//------------------------------------------------------------------------------
function DSSProperty_Get_Name(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if InvalidCircuit(DSSPrime) then
        Exit;

    if DSSPrime.ActiveDSSObject = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSSPrime, _('No active DSS object found! Activate one and try again.'), 33101);
        end;
        Exit;
    end;

    if IsPropIndexInvalid(DSSPrime, 33005) then
        Exit;

    Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.ActiveDSSObject.ParentClass.PropertyName^[DSSPrime.FPropIndex]);
end;
//------------------------------------------------------------------------------
function DSSProperty_Get_Val(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if InvalidCircuit(DSSPrime) then
        Exit;

    if DSSPrime.ActiveDSSObject = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSSPrime, _('No active DSS object found! Activate one and try again.'), 33102);
        end;
        Exit;
    end;

    if IsPropIndexInvalid(DSSPrime, 33004) then
        Exit;
        
    with DSSPrime.ActiveDSSObject do
        Result := DSS_GetAsPAnsiChar(DSSPrime, PropertyValue[DSSPrime.FPropIndex]);
end;

//------------------------------------------------------------------------------
procedure DSSProperty_Set_Val(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    if DSSPrime.ActiveDSSObject = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSSPrime, _('No active DSS object found! Activate one and try again.'), 33103);
        end;
        Exit;
    end;
    
    if IsPropIndexInvalid(DSSPrime, 33001) then
        Exit;

    with DSSPrime.ActiveDSSObject do
        DSSPrime.DSSExecutive.Command := 'Edit ' + FullName + ' ' + ParentClass.PropertyName^[DSSPrime.FPropIndex] + '=' + (Value);
end;
//------------------------------------------------------------------------------
procedure DSSProperty_Set_Index(const Value: Integer); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    if DSSPrime.ActiveDSSObject = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSSPrime, _('No active DSS object found! Activate one and try again.'), 33104);
        end;
        Exit;
    end;
        
    DSSPrime.FPropIndex := Value + 1;
    DSSPrime.FPropClass := DSSPrime.ActiveDSSObject.ParentClass;
    if IsPropIndexInvalid(DSSPrime, 33002) then
        Exit;
end;
//------------------------------------------------------------------------------
procedure DSSProperty_Set_Name(const Value: PAnsiChar); CDECL;
var
    i: Integer;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    if DSSPrime.ActiveDSSObject = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSSPrime, _('No active DSS object found! Activate one and try again.'), 33105);
        end;
        Exit;
    end;
        
    DSSPrime.FPropClass := DSSPrime.ActiveDSSObject.ParentClass;
    DSSPrime.FPropIndex := 0;
    if DSSPrime.FPropClass <> NIL then
    begin
        for i := 1 to DSSPrime.FPropClass.NumProperties do
        begin
            if CompareText(Value, DSSPrime.FPropClass.PropertyName^[i]) = 0 then
            begin
                DSSPrime.FPropIndex := i;
                Exit;
            end;
        end;
    end;
    
    DoSimpleMsg(DSSPrime,
        'Invalid property name "%s" for "%s"',
        [String(Value), DSSPrime.ActiveDSSObject.FullName],
        33003
    );
end;
//------------------------------------------------------------------------------
end.
