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
    Result := FALSE;

    if (FPropIndex > ActiveDSSObject.ParentClass.NumProperties) or (FPropIndex < 1) then
    begin
        DoSimpleMsg(Format(
            'Invalid property index "%d" for "%s.%s"',
            [FPropIndex, ActiveDSSObject.ParentClass.Name, ActiveDSSObject.Name]),
            errorNum
        );
        Result := TRUE;
    end;
end;
//------------------------------------------------------------------------------
function DSSProperty_Get_Description(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if InvalidCircuit then
        Exit;
        
    if ActiveDSSObject = NIL then
    begin
        DoSimpleMsg('No active DSS object found! Activate one and try again.', 33100);
        Exit;
    end;

    with ActiveDSSObject.ParentClass do
        if not IsPropIndexInvalid(33006) then
            Result := DSS_GetAsPAnsiChar(PropertyHelp^[FPropIndex]);
end;
//------------------------------------------------------------------------------
function DSSProperty_Get_Name(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if InvalidCircuit then
        Exit;

    if ActiveDSSObject = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active DSS object found! Activate one and try again.', 33101);
        end;
        Exit;
    end;

    if IsPropIndexInvalid(33005) then
        Exit;

    Result := DSS_GetAsPAnsiChar(ActiveDSSObject.ParentClass.PropertyName^[FPropIndex]);
end;
//------------------------------------------------------------------------------
function DSSProperty_Get_Val(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if InvalidCircuit then
        Exit;

    if ActiveDSSObject = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active DSS object found! Activate one and try again.', 33102);
        end;
        Exit;
    end;

    if IsPropIndexInvalid(33004) then
        Exit;
        
    with ActiveDSSObject do
        Result := DSS_GetAsPAnsiChar(PropertyValue[ParentClass.PropertyIdxMap[FPropIndex]]);
end;

//------------------------------------------------------------------------------
procedure DSSProperty_Set_Val(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit then
        Exit;

    if ActiveDSSObject = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active DSS object found! Activate one and try again.', 33103);
        end;
        Exit;
    end;
    
    if IsPropIndexInvalid(33001) then
        Exit;

    with ActiveDSSObject do
        DSSExecutive.Command := 'Edit ' + ParentClass.Name + '.' + Name + ' ' + ParentClass.PropertyName^[FPropIndex] + '=' + (Value);
end;
//------------------------------------------------------------------------------
procedure DSSProperty_Set_Index(const Value: Integer); CDECL;
begin
    if InvalidCircuit then
        Exit;

    if ActiveDSSObject = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active DSS object found! Activate one and try again.', 33104);
        end;
        Exit;
    end;
        
    FPropIndex := Value + 1;
    FPropClass := ActiveDSSObject.ParentClass;
    if IsPropIndexInvalid(33002) then
        Exit;
end;
//------------------------------------------------------------------------------
procedure DSSProperty_Set_Name(const Value: PAnsiChar); CDECL;
var
    i: Integer;
begin
    if InvalidCircuit then
        Exit;

    if ActiveDSSObject = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active DSS object found! Activate one and try again.', 33105);
        end;
        Exit;
    end;
        
    FPropClass := ActiveDSSObject.ParentClass;
    FPropIndex := 0;
    if FPropClass <> NIL then
    begin
        for i := 1 to FPropClass.NumProperties do
        begin
            if CompareText(Value, FPropClass.PropertyName^[i]) = 0 then
            begin
                FPropIndex := i;
                Exit;
            end;
        end;
    end;
    
    DoSimpleMsg(Format(
        'Invalid property name "%s" for "%s.%s"',
        [String(Value), FPropClass.Name, ActiveDSSObject.Name]),
        33003
    );
end;
//------------------------------------------------------------------------------
end.
