unit CAPI_Error;

interface

uses
    CAPI_Utils;

function Error_Get_Description(): PAnsiChar; CDECL;
function Error_Get_Number(): Integer; CDECL;

// API Extensions
function Error_Get_NumberPtr(): PAPISize; CDECL;
function Error_Get_EarlyAbort(): TAPIBoolean; CDECL;
procedure Error_Set_EarlyAbort(Value: TAPIBoolean); CDECL;
function Error_Get_ExtendedErrors(): TAPIBoolean; CDECL;
procedure Error_Set_ExtendedErrors(Value: TAPIBoolean); CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals;

function Error_Get_Description_AnsiString(): Ansistring; inline;
begin
    Result := LastErrorMessage;
    LastErrorMessage := ''; // Reset after retrieving message
end;

function Error_Get_Description(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Error_Get_Description_AnsiString());
end;
//------------------------------------------------------------------------------
function Error_Get_Number(): Integer; CDECL;
begin
    Result := ErrorNumber;
    ErrorNumber := 0;  // Reset after retrieving ErrorNumber
end;
//------------------------------------------------------------------------------
function Error_Get_NumberPtr(): PAPISize; CDECL;
begin
    Result := @ErrorNumber; // Remember to reset it to zero after the error treatment!
end;
//------------------------------------------------------------------------------
function Error_Get_EarlyAbort(): TAPIBoolean; CDECL;
begin
    Result := DSS_CAPI_EARLY_ABORT;
end;
//------------------------------------------------------------------------------
procedure Error_Set_EarlyAbort(Value: TAPIBoolean); CDECL;
begin
    DSS_CAPI_EARLY_ABORT := Value;
end;
//------------------------------------------------------------------------------
function Error_Get_ExtendedErrors(): TAPIBoolean; CDECL;
begin
    Result := DSS_CAPI_EXT_ERRORS;
end;
//------------------------------------------------------------------------------
procedure Error_Set_ExtendedErrors(Value: TAPIBoolean); CDECL;
begin
    DSS_CAPI_EXT_ERRORS := Value;
end;
//------------------------------------------------------------------------------
end.
