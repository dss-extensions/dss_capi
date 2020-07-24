unit CAPI_DSS_Executive;

{$inline on}

interface

uses
    CAPI_Utils;

function DSS_Executive_Get_Command(i: Integer): PAnsiChar; CDECL;
function DSS_Executive_Get_NumCommands(): Integer; CDECL;
function DSS_Executive_Get_NumOptions(): Integer; CDECL;
function DSS_Executive_Get_Option(i: Integer): PAnsiChar; CDECL;
function DSS_Executive_Get_CommandHelp(i: Integer): PAnsiChar; CDECL;
function DSS_Executive_Get_OptionHelp(i: Integer): PAnsiChar; CDECL;
function DSS_Executive_Get_OptionValue(i: Integer): PAnsiChar; CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    ExecCommands,
    ExecOptions,
    Executive;

//------------------------------------------------------------------------------
function DSS_Executive_Get_Command(i: Integer): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(ExecCommand[i]);
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_NumCommands(): Integer; CDECL;
begin
    Result := NumExecCommands;
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_NumOptions(): Integer; CDECL;
begin
    Result := NumExecOptions;
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_Option(i: Integer): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(ExecOption[i]);
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_CommandHelp(i: Integer): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(CommandHelp[i]);
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_OptionHelp(i: Integer): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(OptionHelp[i]);
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_OptionValue(i: Integer): PAnsiChar; CDECL;
begin
    DSSExecutive.Command := 'get ' + ExecOption[i];
    Result := DSS_GetAsPAnsiChar(GlobalResult);
end;
//------------------------------------------------------------------------------
end.
