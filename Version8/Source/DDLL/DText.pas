unit DText;

interface

function DSSPut_Command(a: PAnsiChar): PAnsiChar; CDECL;

implementation

uses
    DSSGlobals,
    Executive,
    Dialogs,
    SysUtils;

function DSSPut_Command(a: PAnsiChar): PAnsiChar; CDECL;
begin
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    DSSExecutive.Command := Widestring(a);  {Convert to String}
    Result := PAnsiChar(Ansistring(GlobalResult));
end;

end.
