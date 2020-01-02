unit CAPI_ActiveClass;

{$inline on}

interface

uses
    CAPI_Utils;

procedure ActiveClass_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
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
    CktElement,
    DSSClass,
    DSSHelper;

procedure ActiveClass_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    idx: Integer;
    k: Integer;

begin
    if (DSSPrime.ActiveCircuit <> NIL) and Assigned(DSSPrime.ActiveDSSClass) then
        with DSSPrime.ActiveCircuit do
        begin
            Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, DSSPrime.ActiveDSSClass.ElementCount);
            k := 0;
            idx := DSSPrime.ActiveDSSClass.First;
            while idx > 0 do
            begin
                Result[k] := DSS_CopyStringAsPChar(DSSPrime.ActiveDSSObject.Name);
                Inc(k);
                idx := DSSPrime.ActiveDSSClass.Next;
            end;
        end
    else
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);

end;
//------------------------------------------------------------------------------
function ActiveClass_Get_First(): Integer; CDECL;
begin

    Result := 0;
    if (DSSPrime.ActiveCircuit <> NIL) and Assigned(DSSPrime.ActiveDSSClass) then
    begin
        Result := DSSPrime.ActiveDSSClass.First;  // sets active objects
    end;

end;
//------------------------------------------------------------------------------
function ActiveClass_Get_Next(): Integer; CDECL;
begin

    Result := 0;
    if (DSSPrime.ActiveCircuit <> NIL) and Assigned(DSSPrime.ActiveDSSClass) then
    begin
        Result := DSSPrime.ActiveDSSClass.Next;  // sets active objects
    end;

end;
//------------------------------------------------------------------------------
function ActiveClass_Get_Name(): PAnsiChar; CDECL;
begin
    if Assigned(DSSPrime.ActiveDSSObject) then
        Result := DSS_GetAsPAnsiChar(DSSPrime.ActiveDSSObject.Name)
    else
        Result := nil;
end;
//------------------------------------------------------------------------------
procedure ActiveClass_Set_Name(const Value: PAnsiChar); CDECL;
// set object active by name
var
    pelem: TDSSObject;
begin
    if Assigned(DSSPrime.ActiveDSSClass) then
    begin
        pelem := DSSPrime.ActiveDSSClass.Find(Value);
        if pelem <> NIL then
        begin
            if pelem is TDSSCktElement then
                DSSPrime.ActiveCircuit.ActiveCktElement := TDSSCktElement(pelem)  // sets DSSPrime.ActiveDSSObject
            else
                DSSPrime.ActiveDSSObject := pelem;
        end;
    end;
end;
//------------------------------------------------------------------------------
function ActiveClass_Get_NumElements(): Integer; CDECL;
begin
    if Assigned(DSSPrime.ActiveDSSClass) then
        Result := DSSPrime.ActiveDSSClass.ElementCount
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
function ActiveClass_Get_ActiveClassName(): PAnsiChar; CDECL;
begin
    if Assigned(DSSPrime.ActiveDSSClass) then
        Result := DSS_GetAsPAnsiChar(DSSPrime.ActiveDSSClass.Name)
    else
        Result := nil;
end;
//------------------------------------------------------------------------------
function ActiveClass_Get_Count(): Integer; CDECL;
begin
    if Assigned(DSSPrime.ActiveDSSClass) then
        Result := DSSPrime.ActiveDSSClass.ElementCount
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
end.
