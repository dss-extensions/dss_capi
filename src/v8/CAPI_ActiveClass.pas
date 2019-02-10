unit CAPI_ActiveClass;

{$inline on}

interface

uses
    CAPI_Utils;

procedure ActiveClass_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure ActiveClass_Get_AllNames_GR(); CDECL;
function ActiveClass_Get_First(): Integer; CDECL;
function ActiveClass_Get_Next(): Integer; CDECL;
function ActiveClass_Get_Name(): PAnsiChar; CDECL;
procedure ActiveClass_Set_Name(const Value: PAnsiChar); CDECL;
function ActiveClass_Get_NumElements(): Integer; CDECL;
function ActiveClass_Get_ActiveClassName(): PAnsiChar; CDECL;
function ActiveClass_Get_Count(): Integer; CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    DSSObject,
    CktElement;

procedure ActiveClass_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    idx: Integer;
    k: Integer;

begin
    if (ActiveCircuit[ActiveActor] <> NIL) and Assigned(ActiveDSSClass[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (ActiveDSSClass[ActiveActor].ElementCount - 1) + 1);
            k := 0;
            idx := ActiveDSSClass[ActiveActor].First;
            while idx > 0 do
            begin
                Result[k] := DSS_CopyStringAsPChar(ActiveDSSObject[ActiveActor].Name);
                Inc(k);
                idx := ActiveDSSClass[ActiveActor].Next;
            end;
        end
    else
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);

end;

procedure ActiveClass_Get_AllNames_GR(); CDECL;
// Same as ActiveClass_Get_AllNames but uses global result (GR) pointers
begin
    ActiveClass_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function ActiveClass_Get_First(): Integer; CDECL;
begin

    Result := 0;
    if (ActiveCircuit[ActiveActor] <> NIL) and Assigned(ActiveDSSClass[ActiveActor]) then
    begin
        Result := ActiveDSSClass[ActiveActor].First;  // sets active objects
    end;

end;
//------------------------------------------------------------------------------
function ActiveClass_Get_Next(): Integer; CDECL;
begin

    Result := 0;
    if (ActiveCircuit[ActiveActor] <> NIL) and Assigned(ActiveDSSClass[ActiveActor]) then
    begin
        Result := ActiveDSSClass[ActiveActor].Next;  // sets active objects
    end;

end;
//------------------------------------------------------------------------------
function ActiveClass_Get_Name_AnsiString(): Ansistring; inline;
begin
    if Assigned(ActiveDSSObject[ActiveActor]) then
        Result := ActiveDSSObject[ActiveActor].Name
    else
        Result := '';
end;

function ActiveClass_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(ActiveClass_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure ActiveClass_Set_Name(const Value: PAnsiChar); CDECL;
// set object active by name
var
    pelem: TDSSObject;
begin
    if Assigned(ActiveDSSClass[ActiveActor]) then
    begin
        pelem := ActiveDSSClass[ActiveActor].Find(Value);
        if pelem <> NIL then
        begin
            if pelem is TDSSCktElement then
                ActiveCircuit[ActiveActor].ActiveCktElement := TDSSCktElement(pelem)  // sets ActiveDSSobject
            else
                ActiveDSSObject[ActiveActor] := pelem;
        end;
    end;
end;
//------------------------------------------------------------------------------
function ActiveClass_Get_NumElements(): Integer; CDECL;
begin
    if Assigned(ActiveDSSClass[ActiveActor]) then
        Result := ActiveDSSClass[ActiveActor].ElementCount
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
function ActiveClass_Get_ActiveClassName_AnsiString(): Ansistring; inline;
begin
    if Assigned(ActiveDSSClass[ActiveActor]) then
        Result := ActiveDSSClass[ActiveActor].Name
    else
        Result := '';
end;

function ActiveClass_Get_ActiveClassName(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(ActiveClass_Get_ActiveClassName_AnsiString());
end;
//------------------------------------------------------------------------------
function ActiveClass_Get_Count(): Integer; CDECL;
begin
    if Assigned(ActiveDSSClass[ActiveActor]) then
        Result := ActiveDSSClass[ActiveActor].ElementCount
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
end.
