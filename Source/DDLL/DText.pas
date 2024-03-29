unit DText;

interface

function DSSPut_Command(a: Pansichar): Pansichar; CDECL;

implementation

uses
    DSSGlobals,
    Executive,
{$IFNDEF FPC_DLL}
    Dialogs,
{$ENDIF}
    SysUtils;

function DSSPut_Command(a: Pansichar): Pansichar; CDECL;
begin
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    DSSExecutive[ActiveActor].Command := String(a);  {Convert to String}
    Result := Pansichar(Ansistring(GlobalResult));
end;

end.
