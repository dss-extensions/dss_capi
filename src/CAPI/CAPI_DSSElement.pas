unit CAPI_DSSElement;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure DSSElement_Get_AllPropertyNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure DSSElement_Get_AllPropertyNames_GR(); CDECL;
function DSSElement_Get_Name(): PAnsiChar; CDECL;
function DSSElement_Get_NumProperties(): Integer; CDECL;
function DSSElement_ToJSON(Options: Integer): PAnsiChar; CDECL;
function DSSElement_Get_Pointer(): Pointer; CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    Sysutils,
    DSSClass,
    DSSHelper,
    CAPI_Obj;

procedure DSSElement_Get_AllPropertyNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray0;
    k: Integer;
    cls: TDSSClass;
begin
    if (InvalidCircuit(DSSPrime)) or (DSSPrime.ActiveDSSObject = NIL) then
    begin
        DefaultResult(ResultPtr, ResultCount, '');
        Exit;
    end;
        
    cls := DSSPrime.ActiveDSSObject.ParentClass;
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, cls.NumProperties);
    for k := 1 to cls.NumProperties do
    begin
        Result[k - 1] := DSS_CopyStringAsPChar(cls.PropertyName[k]);
    end;
end;

procedure DSSElement_Get_AllPropertyNames_GR(); CDECL;
// Same as DSSElement_Get_AllPropertyNames but uses global result (GR) pointers
begin
    DSSElement_Get_AllPropertyNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function DSSElement_Get_Name(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if (InvalidCircuit(DSSPrime)) or (DSSPrime.ActiveDSSObject = NIL) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.ActiveDSSObject.FullName);
end;
//------------------------------------------------------------------------------
function DSSElement_Get_NumProperties(): Integer; CDECL;
begin
    Result := 0;
    if (InvalidCircuit(DSSPrime)) or (DSSPrime.ActiveDSSObject = NIL) then
        Exit;
        
    Result := DSSPrime.ActiveDSSObject.ParentClass.NumProperties;
end;
//------------------------------------------------------------------------------
function DSSElement_ToJSON(Options: Integer): PAnsiChar; CDECL;
begin
    Result := NIL;
    if (InvalidCircuit(DSSPrime)) or (DSSPrime.ActiveDSSObject = NIL) then
        Exit;

    Result := DSS_GetAsPAnsiChar(DSSPrime, Obj_ToJSON_(DSSPrime.ActiveDSSObject, options));
end;
//------------------------------------------------------------------------------
function DSSElement_Get_Pointer(): Pointer; CDECL;
begin
    Result := NIL;
    if (InvalidCircuit(DSSPrime)) then
        Exit;

    Result := DSSPrime.ActiveDSSObject
end;
//------------------------------------------------------------------------------
end.
