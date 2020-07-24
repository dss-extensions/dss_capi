unit CAPI_DSSElement;

{$inline on}

interface

uses
    CAPI_Utils;

procedure DSSElement_Get_AllPropertyNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure DSSElement_Get_AllPropertyNames_GR(); CDECL;
function DSSElement_Get_Name(): PAnsiChar; CDECL;
function DSSElement_Get_NumProperties(): Integer; CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    Sysutils;

procedure DSSElement_Get_AllPropertyNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    k: Integer;
begin
    if (InvalidCircuit) or (ActiveDSSObject = NIL) then
    begin
        DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
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

procedure DSSElement_Get_AllPropertyNames_GR(); CDECL;
// Same as DSSElement_Get_AllPropertyNames but uses global result (GR) pointers
begin
    DSSElement_Get_AllPropertyNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
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
