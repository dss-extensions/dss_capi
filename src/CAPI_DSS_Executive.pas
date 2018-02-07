UNIT CAPI_DSS_Executive;
{$inline on}

INTERFACE

USES CAPI_Utils;

function DSS_Executive_Get_Command(i: Integer):PAnsiChar;cdecl;
function DSS_Executive_Get_NumCommands():Integer;cdecl;
function DSS_Executive_Get_NumOptions():Integer;cdecl;
function DSS_Executive_Get_Option(i: Integer):PAnsiChar;cdecl;
function DSS_Executive_Get_CommandHelp(i: Integer):PAnsiChar;cdecl;
function DSS_Executive_Get_OptionHelp(i: Integer):PAnsiChar;cdecl;
function DSS_Executive_Get_OptionValue(i: Integer):PAnsiChar;cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, ExecCommands, ExecOptions, Executive;

function DSS_Executive_Get_Command_AnsiString(i: Integer):AnsiString;inline;
begin
     Result := ExecCommand[i];
end;

function DSS_Executive_Get_Command(i: Integer):PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(DSS_Executive_Get_Command_AnsiString(i));
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_NumCommands():Integer;cdecl;
begin
     Result :=  NumExecCommands;
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_NumOptions():Integer;cdecl;
begin
     Result :=  NumExecOptions;
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_Option_AnsiString(i: Integer):AnsiString;inline;
begin
     Result := ExecOption[i];
end;

function DSS_Executive_Get_Option(i: Integer):PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(DSS_Executive_Get_Option_AnsiString(i));
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_CommandHelp_AnsiString(i: Integer):AnsiString;inline;
begin
     Result := CommandHelp[i];
end;

function DSS_Executive_Get_CommandHelp(i: Integer):PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(DSS_Executive_Get_CommandHelp_AnsiString(i));
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_OptionHelp_AnsiString(i: Integer):AnsiString;inline;
begin
     Result := OptionHelp[i];
end;

function DSS_Executive_Get_OptionHelp(i: Integer):PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(DSS_Executive_Get_OptionHelp_AnsiString(i));
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_OptionValue_AnsiString(i: Integer):AnsiString;inline;
begin
     DSSExecutive.Command := 'get ' + ExecOption[i];
     Result := GlobalResult;
end;

function DSS_Executive_Get_OptionValue(i: Integer):PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(DSS_Executive_Get_OptionValue_AnsiString(i));
end;
//------------------------------------------------------------------------------
END.
