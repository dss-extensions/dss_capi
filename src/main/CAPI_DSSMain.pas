UNIT CAPI_DSSMain;
{$inline on}

INTERFACE

USES CAPI_Utils, DSS_TLB;

function DSSMain_Get_ActiveCircuit():PAnsiChar;cdecl;
function DSSMain_Get_NumCircuits():Integer;cdecl;
procedure DSSMain_Set_ActiveCircuit(const Value: PAnsiChar);cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, Executive, sysUtils;

function DSSMain_Get_ActiveCircuit_AnsiString():AnsiString;inline;
begin
   Result := ActiveCircuit.Name;
end;

function DSSMain_Get_ActiveCircuit():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(DSSMain_Get_ActiveCircuit_AnsiString());
end;
//------------------------------------------------------------------------------
function DSSMain_Get_NumCircuits():Integer;cdecl;
begin
    Result := NumCircuits;
end;
//------------------------------------------------------------------------------
procedure DSSMain_Set_ActiveCircuit(const Value: PAnsiChar);cdecl;
begin
   DSSExecutive.SetActiveCircuit(String(Value));
end;
//------------------------------------------------------------------------------
END.
