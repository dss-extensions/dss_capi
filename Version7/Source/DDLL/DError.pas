unit DError;

interface

function ErrorDesc(): pAnsiChar; CDECL;
function ErrorCode(): Longint; CDECL;

implementation

uses
    DSSGlobals;

function ErrorCode(): Longint; CDECL;
begin
    Result := ErrorNumber;
    ErrorNumber := 0;  // Reset after retrieving ErrorNumber
end;

function ErrorDesc(): pAnsiChar; CDECL;
begin
    Result := pAnsiChar(Ansistring(LastErrorMessage));
    LastErrorMessage := ''; // Reset after retrieving message
end;

end.
