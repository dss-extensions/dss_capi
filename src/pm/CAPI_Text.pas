UNIT CAPI_Text;
{$inline on}

INTERFACE

USES CAPI_Utils;

function Text_Get_Command():PAnsiChar;cdecl;
procedure Text_Set_Command(const Value: PAnsiChar);cdecl;
function Text_Get_Result():PAnsiChar;cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, Executive, SysUtils;

const
  nothing: AnsiString = #0#0;

function Text_Get_Command_AnsiString():AnsiString;inline;
begin
   Result := DSSExecutive.Command;
end;

function Text_Get_Command():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Text_Get_Command_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Text_Set_Command(const Value: PAnsiChar);cdecl;
begin
   SolutionAbort := FALSE;  // Reset for commands entered from outside
   DSSExecutive.Command := Value;  {Convert to String}
end;
//------------------------------------------------------------------------------
function Text_Get_Result_AnsiString():AnsiString;inline;
begin
   if Length(GlobalResult) < 1 then
      Result := nothing
   else
      Result := GlobalResult;
    {****}
    {
      Need to implement a protocol for determining whether to go get the
      result from a file or to issue another DSS command to get the value
      from operations where the result is voluminous.
    }

end;

function Text_Get_Result():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Text_Get_Result_AnsiString());
end;
//------------------------------------------------------------------------------
END.
