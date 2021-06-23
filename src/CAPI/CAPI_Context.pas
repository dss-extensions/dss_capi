unit CAPI_Context;

interface

uses
    DSSClass,
    CAPI_Utils,
    CAPI_Types;

function ctx_New(): TDSSContext; CDECL;
procedure ctx_Dispose(DSS: TDSSContext); CDECL;
function ctx_Get_Prime(): TDSSContext; CDECL;
function ctx_Set_Prime(DSS: TDSSContext): TDSSContext; CDECL;

implementation

uses
    DSSGlobals,
    DSSHelper,
    KLUSolve;

//------------------------------------------------------------------------------
function ctx_New(): TDSSContext; CDECL;
begin
    Result := TDSSContext.Create(nil, False);
end;
//------------------------------------------------------------------------------
procedure ctx_Dispose(DSS: TDSSContext); CDECL;
begin
    if DSS <> nil then
    begin
        DSS.Free();
    end;
end;
//------------------------------------------------------------------------------
function ctx_Get_Prime(): TDSSContext; CDECL;
begin
    Result := DSSPrime;
end;
//------------------------------------------------------------------------------
function ctx_Set_Prime(DSS: TDSSContext): TDSSContext; CDECL;
begin
    if DSS = DSSPrime then
    begin
        Result := NIL;
        Exit;
    end;
    Result := DSSPrime;
    DSSPrime := DSS;
end;
//------------------------------------------------------------------------------
end.
