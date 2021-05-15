unit CAPI_DSSElement;

interface

uses
    CAPI_Utils;

procedure DSSElement_Get_AllPropertyNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure DSSElement_Get_AllPropertyNames_GR(); CDECL;
function DSSElement_Get_Name(): PAnsiChar; CDECL;
function DSSElement_Get_NumProperties(): Integer; CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    Sysutils,
    DSSClass,
    DSSHelper;

procedure DSSElement_Get_AllPropertyNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray;
    k: Integer;
begin
    if (InvalidCircuit(DSSPrime)) or (DSSPrime.ActiveDSSObject = NIL) then
    begin
        DefaultResult(ResultPtr, ResultCount, '');
        Exit;
    end;
        
    with DSSPrime.ActiveDSSObject do
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
    if (InvalidCircuit(DSSPrime)) or (DSSPrime.ActiveDSSObject = NIL) then
        Exit;
    with DSSPrime.ActiveDSSObject do
        Result := DSS_GetAsPAnsiChar(ParentClass.Name + '.' + Name);
end;
//------------------------------------------------------------------------------
function DSSElement_Get_NumProperties(): Integer; CDECL;
begin
    Result := 0;
    if (InvalidCircuit(DSSPrime)) or (DSSPrime.ActiveDSSObject = NIL) then
        Exit;
        
    with DSSPrime.ActiveDSSObject do
        Result := ParentClass.NumProperties;
end;
//------------------------------------------------------------------------------
end.
