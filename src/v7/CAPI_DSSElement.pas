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
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if ActiveDSSObject <> NIL then
                with ActiveDSSObject do
                begin
                    with ParentClass do
                    begin
                        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumProperties - 1) + 1);
                        for k := 1 to NumProperties do
                        begin
                            Result[k - 1] := DSS_CopyStringAsPChar(PropertyName^[k]);
                        end;
                    end;
                end
        end;

end;

procedure DSSElement_Get_AllPropertyNames_GR(); CDECL;
// Same as DSSElement_Get_AllPropertyNames but uses global result (GR) pointers
begin
    DSSElement_Get_AllPropertyNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function DSSElement_Get_Name_AnsiString(): Ansistring; inline;
begin
    if ActiveCircuit <> NIL then
        if ActiveDSSObject <> NIL then
            with ActiveDSSObject do
            begin
                Result := ParentClass.Name + '.' + Name;
            end
        else
            Result := '';
end;

function DSSElement_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSElement_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function DSSElement_Get_NumProperties(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if ActiveDSSObject <> NIL then
                with ActiveDSSObject do
                begin
                    Result := ParentClass.NumProperties;
                end
        end;
end;
//------------------------------------------------------------------------------
end.
