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
    if (i >= 1) and (i <= NumExecCommands) then
        Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.DSSExecutive.ExecCommand[i - 1])
    else
        Result := NIL;
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
    if (i >= 1) and (i <= NumExecOptions) then
        Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.DSSExecutive.ExecOption[i - 1])
    else
        Result := NIL;
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_CommandHelp(i: Integer): PAnsiChar; CDECL;
begin
    if (i >= 1) and (i <= NumExecCommands) then
        Result := DSS_GetAsPAnsiChar(DSSPrime, DSSHelp('Command.' + DSSPrime.DSSExecutive.ExecCommand[i - 1]))
    else
        Result := NIL;
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_OptionHelp(i: Integer): PAnsiChar; CDECL;
begin
    if (i >= 1) and (i <= NumExecOptions) then
        Result := DSS_GetAsPAnsiChar(DSSPrime, DSSHelp('Executive.' + DSSPrime.DSSExecutive.ExecOption[i - 1]))
    else
        Result := NIL;
end;
//------------------------------------------------------------------------------
function DSS_Executive_Get_OptionValue(i: Integer): PAnsiChar; CDECL;
begin
    if (i >= 1) and (i <= NumExecOptions) then
    begin
        DSSPrime.DSSExecutive.Command := 'get ' + DSSPrime.DSSExecutive.ExecOption[i - 1];
        Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.GlobalResult);
    end
    else
        Result := NIL;
end;
//------------------------------------------------------------------------------
end.
