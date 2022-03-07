unit CAPI_ZIP;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure ZIP_Open(const FileName: PAnsiChar); CDECL;
procedure ZIP_Close(); CDECL;
procedure ZIP_Redirect(const FileName: PAnsiChar); CDECL;

implementation

uses
    CAPI_Constants,
    Classes,
    DSSGlobals,
    Executive,
    SysUtils,
    ExecHelper,
    DSSClass,
    DSSHelper;

//------------------------------------------------------------------------------
procedure ZIP_Open(const FileName: PAnsiChar); CDECL;
begin
    DSSPrime.DSSExecutive.ZipOpen(FileName);
end;
//------------------------------------------------------------------------------
procedure ZIP_Close(); CDECL;
begin
    DSSPrime.DSSExecutive.ZipClose();
end;
//------------------------------------------------------------------------------
procedure ZIP_Redirect(const FileName: PAnsiChar); CDECL;
begin
    DSSPrime.DSSExecutive.ZipRedirect(FileName);
end;
//------------------------------------------------------------------------------
end.
