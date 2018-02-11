UNIT CAPI_Error;
{$inline on}

INTERFACE

USES CAPI_Utils;

function Error_Get_Description():PAnsiChar;cdecl;
function Error_Get_Number():Integer;cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals;

function Error_Get_Description_AnsiString():AnsiString;inline;
begin
    Result := LastErrorMessage;
    LastErrorMessage := ''; // Reset after retrieving message
end;

function Error_Get_Description():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Error_Get_Description_AnsiString());
end;
//------------------------------------------------------------------------------
function Error_Get_Number():Integer;cdecl;
begin
    Result := ErrorNumber;
    ErrorNumber := 0;  // Reset after retrieving ErrorNumber
end;
//------------------------------------------------------------------------------
END.
