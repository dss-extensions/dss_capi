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
    Executive,
    DSSClass,
    DSSHelper;

function DSS_Executive_Get_Command_AnsiString(i: Integer): Ansistring; inline;
begin
    Result := ExecCommand[i];
end;

function DSS_Executive_Get_Command(i: Integer): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSS_Executive_Get_Command_AnsiString(i));
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
function DSS_Executive_Get_Option_AnsiString(i: Integer): Ansistring; inline;
begin
    Result := ExecOption[i];
end;

function DSS_Executive_Get_Option(i: Integer): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSS_Executive_Get_Option_AnsiString(i));
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_CommandHelp_AnsiString(i: Integer): Ansistring; inline;
begin
    Result := CommandHelp[i];
end;

function DSS_Executive_Get_CommandHelp(i: Integer): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSS_Executive_Get_CommandHelp_AnsiString(i));
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_OptionHelp_AnsiString(i: Integer): Ansistring; inline;
begin
    Result := OptionHelp[i];
end;

function DSS_Executive_Get_OptionHelp(i: Integer): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSS_Executive_Get_OptionHelp_AnsiString(i));
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_OptionValue_AnsiString(i: Integer): Ansistring; inline;
begin
    DSSExecutive.Command := 'get ' + ExecOption[i];
    Result := DSSPrime.GlobalResult;
end;

function DSS_Executive_Get_OptionValue(i: Integer): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSS_Executive_Get_OptionValue_AnsiString(i));
end;
//------------------------------------------------------------------------------
end.
