unit CAPI_DSSElement;

interface

uses
    CAPI_Utils;

procedure DSSElement_Get_AllPropertyNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
function DSSElement_Get_Name(): PAnsiChar; CDECL;
function DSSElement_Get_NumProperties(): Integer; CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    Sysutils;

procedure DSSElement_Get_AllPropertyNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray;
    k: Integer;
begin
    if (InvalidCircuit) or (ActiveDSSObject = NIL) then
    begin
        DefaultResult(ResultPtr, ResultCount, '');
        Exit;
    end;
        
    with ActiveDSSObject do
        with ParentClass do
        begin
            Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, NumProperties);
            for k := 1 to NumProperties do
            begin
                Result[k - 1] := DSS_CopyStringAsPChar(PropertyName^[k]);
            end;
        end;
end;
//------------------------------------------------------------------------------
function DSSElement_Get_Name(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if (InvalidCircuit) or (ActiveDSSObject = NIL) then
        Exit;
    with ActiveDSSObject do
        Result := DSS_GetAsPAnsiChar(ParentClass.Name + '.' + Name);
end;
//------------------------------------------------------------------------------
function DSSElement_Get_NumProperties(): Integer; CDECL;
begin
    Result := 0;
    if (InvalidCircuit) or (ActiveDSSObject = NIL) then
        Exit;
        
    with ActiveDSSObject do
        Result := ParentClass.NumProperties;
end;
//------------------------------------------------------------------------------
end.
