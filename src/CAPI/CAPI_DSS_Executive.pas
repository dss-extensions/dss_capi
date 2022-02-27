unit CAPI_DSS_Executive;

interface

uses
    CAPI_Utils,
    CAPI_Types;

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
    Executive,
    DSSClass,
    DSSHelper;

//------------------------------------------------------------------------------
function DSS_Executive_Get_Command(i: Integer): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSPrime, ExecCommand[i]);
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
    Result := DSS_GetAsPAnsiChar(DSSPrime, ExecOption[i]);
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_CommandHelp(i: Integer): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSPrime, DSSHelp('Command.' + ExecCommand[i]));
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_OptionHelp(i: Integer): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSPrime, DSSHelp('Executive.' + ExecOption[i]));
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_OptionValue(i: Integer): PAnsiChar; CDECL;
begin
    DSSPrime.DSSExecutive.Command := 'get ' + ExecOption[i];
    Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.GlobalResult);
end;
//------------------------------------------------------------------------------
end.
