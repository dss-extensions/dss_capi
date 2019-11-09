unit CAPI_Error;

{$inline on}

interface

uses
    CAPI_Utils;

function Error_Get_Description(): PAnsiChar; CDECL;
function Error_Get_Number(): Integer; CDECL;
function Error_Get_NumberPtr(): PInteger; CDECL;
function Error_Get_EarlyAbort(): Boolean; CDECL;
procedure Error_Set_EarlyAbort(Value: Boolean); CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    DSSClass,
    DSSHelper;

function Error_Get_Description_AnsiString(): Ansistring; inline;
begin
    Result := DSSPrime.LastErrorMessage;
    DSSPrime.LastErrorMessage := ''; // Reset after retrieving message
end;

function Error_Get_Description(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Error_Get_Description_AnsiString());
end;
//------------------------------------------------------------------------------
function Error_Get_Number(): Integer; CDECL;
begin
    Result := DSSPrime.ErrorNumber;
    DSSPrime.ErrorNumber := 0;  // Reset after retrieving ErrorNumber
end;
//------------------------------------------------------------------------------
function Error_Get_NumberPtr(): PInteger; CDECL;
begin
    Result := @DSSPrime.ErrorNumber; // Remember to reset it to zero after the error treatment!
end;
//------------------------------------------------------------------------------
function Error_Get_EarlyAbort(): Boolean; CDECL;
begin
    Result := DSS_CAPI_EARLY_ABORT;
end;
//------------------------------------------------------------------------------
procedure Error_Set_EarlyAbort(Value: Boolean); CDECL;
begin
    DSS_CAPI_EARLY_ABORT := Value;
end;
//------------------------------------------------------------------------------
end.
