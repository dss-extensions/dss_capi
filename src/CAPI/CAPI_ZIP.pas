unit CAPI_ZIP;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure ZIP_Open(const FileName: PAnsiChar); CDECL;
procedure ZIP_Close(); CDECL;
procedure ZIP_Redirect(const FileName: PAnsiChar); CDECL;
procedure ZIP_Extract(var ResultPtr: PByte; ResultCount: PAPISize; const FileName: PAnsiChar); CDECL;
procedure ZIP_Extract_GR(const FileName: PAnsiChar); CDECL;
function ZIP_Contains(const Name: PAnsiChar): TAPIBoolean; CDECL;
procedure ZIP_List(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; const RegExp: PAnsiChar); CDECL;

implementation

uses
    CAPI_Constants,
    Classes,
    DSSGlobals,
    Executive,
    SysUtils,
    ExecHelper,
    DSSClass,
    DSSHelper,
    RegExpr,
    Contnrs,
    Zipper;

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
procedure ZIP_Extract(var ResultPtr: PByte; ResultCount: PAPISize; const FileName: PAnsiChar); CDECL;
begin
    DSSPrime.DSSExecutive.ZipExtract(ResultPtr, ResultCount, FileName);
end;

procedure ZIP_Extract_GR(const FileName: PAnsiChar); CDECL;
begin
    ZIP_Extract(DSSPrime.GR_DataPtr_PByte, @DSSPrime.GR_Counts_PByte[0], FileName);
end;
//------------------------------------------------------------------------------
function ZIP_Contains(const Name: PAnsiChar): TAPIBoolean; CDECL;
var    
    Hashes: TFPHashList = NIL;
begin
    Result := False;
    if not DSSPrime.DSSExecutive.ZipHashes(Hashes) then
    begin
        DoSimpleMsg(DSSPrime, _('No ZIP file is open.'), 89891);
        Exit;
    end;
    Result := Integer(Hashes.Find(Name)) > 0;
end;
//------------------------------------------------------------------------------
procedure ZIP_List(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; const RegExp: PAnsiChar); CDECL;
var    
    Result: PPAnsiCharArray0;
    unzipper: TUnZipper;
    s: String;
    i: Integer;
    rex: TRegExpr = NIL;
begin
    DefaultResult(ResultPtr, ResultCount);
    unzipper := TUnZipper(DSSPrime.unzipper);
    if unzipper = NIL then
    begin
        DoSimpleMsg(DSSPrime, _('No ZIP file is open.'), 89892);
        Exit;
    end;
    if RegExp <> NIL then
    begin
        s := RegExp;
    end
    else
        s := '';

    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, unzipper.Entries.Count);
    if s = '' then
    begin
        for i := 0 to unzipper.Entries.Count - 1 do
            Result[i] := DSS_CopyStringAsPChar(unzipper.Entries[i].ArchiveFileName);

        Exit;
    end;

    try
        rex := TRegExpr.Create();
        rex.ModifierI := False;
        rex.ModifierM := False;
        rex.ModifierS := True;
        rex.Expression := s;
        ResultCount[0] := 0;
        for i := 0 to unzipper.Entries.Count - 1 do
        begin
            if rex.Exec(unzipper.Entries[i].ArchiveFileName) then
            begin
                Result[ResultCount[0]] := DSS_CopyStringAsPChar(unzipper.Entries[i].ArchiveFileName);
                inc(ResultCount[0]);
            end;
        end;
    finally
        FreeAndNil(rex);
    end;
end;
//------------------------------------------------------------------------------
end.
