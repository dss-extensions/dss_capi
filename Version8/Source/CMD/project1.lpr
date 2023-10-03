library project1;

{$mode delphi}{$H+}

{$IFDEF Darwin}
{$linkframework CoreFoundation}
{$linkframework Carbon}
{$ENDIF}

uses
  Classes, SysUtils;

// library subroutine
function DSSI(strIn : string) : PChar; cdecl;
  begin
    DSSI := PChar(UpperCase(strIn));
  end;

// exported subroutine(s)
exports
  DSSI;

end.

