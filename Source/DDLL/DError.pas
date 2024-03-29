unit DError;

interface

function ErrorDesc(): Pansichar; CDECL;
function ErrorCode(): Longint; CDECL;

implementation

uses
    DSSGlobals;

function ErrorCode(): Longint; CDECL;
begin
    Result := ErrorNumber;
    ErrorNumber := 0;  // Reset after retrieving ErrorNumber
end;

function ErrorDesc(): Pansichar; CDECL;
begin
    Result := Pansichar(Ansistring(LastErrorMessage));
    LastErrorMessage := ''; // Reset after retrieving message
end;

end.
