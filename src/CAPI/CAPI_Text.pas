unit CAPI_Text;

{$inline on}

interface

uses
    CAPI_Utils;

function Text_Get_Command(): PAnsiChar; CDECL;
procedure Text_Set_Command(const Value: PAnsiChar); CDECL;
function Text_Get_Result(): PAnsiChar; CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    Executive,
    SysUtils,
    DSSClass,
    DSSHelper;

function Text_Get_Command(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSPrime.DSSExecutive.Command);
end;
//------------------------------------------------------------------------------
procedure Text_Set_Command(const Value: PAnsiChar); CDECL;
begin
    DSSPrime.SolutionAbort := FALSE;  // Reset for commands entered from outside
    DSSPrime.DSSExecutive.Command := Value;  {Convert to String}
end;
//------------------------------------------------------------------------------
function Text_Get_Result(): PAnsiChar; CDECL;
begin
    if Length(DSSPrime.GlobalResult) < 1 then
        Result := nil
    else
        Result := DSS_GetAsPAnsiChar(DSSPrime.GlobalResult);
    {****}
    {
      Need to implement a protocol for determining whether to go get the
      result from a file or to issue another DSS command to get the value
      from operations where the result is voluminous.
    }
end;
//------------------------------------------------------------------------------
end.
