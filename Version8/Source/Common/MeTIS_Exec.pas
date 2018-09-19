unit MeTIS_Exec;

interface

uses
  ShellAPI,
  SysUtils,
  Classes,
  {$IFDEF MSWINDOWS}
  windows;
  {$ELSE}
  ;
  {$ENDIF}

type
  TFileSearchReplace = class(TObject)
  private
    FSourceFile: TFileStream;
    FtmpFile: TFileStream;
    FEncoding: TEncoding;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    procedure Replace(const AFrom, ATo: string; ReplaceFlags: TReplaceFlags);
  end;

Function RunMeTIS(DosApp: string): String;
Function GetNumEdges(MeTISSrc : string): String;

implementation

uses
  System.IOUtils,
  System.StrUtils;

function Max(const A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

{ TFileSearchReplace }

constructor TFileSearchReplace.Create(const AFileName: string);
begin
  inherited Create;

  FSourceFile := TFileStream.Create(AFileName, fmOpenReadWrite);
  FtmpFile := TFileStream.Create(ChangeFileExt(AFileName, '.tmp'), fmCreate);
end;

destructor TFileSearchReplace.Destroy;
var
  tmpFileName: string;
begin
  if Assigned(FtmpFile) then
    tmpFileName := FtmpFile.FileName;

  FreeAndNil(FtmpFile);
  FreeAndNil(FSourceFile);

  TFile.Delete(tmpFileName);

  inherited;
end;

procedure TFileSearchReplace.Replace(const AFrom, ATo: string;
  ReplaceFlags: TReplaceFlags);
  procedure CopyPreamble;
  var
    PreambleSize: Integer;
    PreambleBuf: TBytes;
  begin
    // Copy Encoding preamble
    SetLength(PreambleBuf, 100);
    FSourceFile.Read(PreambleBuf, Length(PreambleBuf));
    FSourceFile.Seek(0, soBeginning);

    PreambleSize := TEncoding.GetBufferEncoding(PreambleBuf, FEncoding);
    if PreambleSize <> 0 then
      FtmpFile.CopyFrom(FSourceFile, PreambleSize);
  end;

  function GetLastIndex(const Str, SubStr: string): Integer;
  var
    i: Integer;
    tmpSubStr, tmpStr: string;
  begin
    if not(rfIgnoreCase in ReplaceFlags) then
      begin
        i := Pos(SubStr, Str);
        Result := i;
        while i > 0 do
          begin
            i := PosEx(SubStr, Str, i + 1);
            if i > 0 then
              Result := i;
          end;
        if Result > 0 then
          Inc(Result, Length(SubStr) - 1);
      end
    else
      begin
        tmpStr := UpperCase(Str);
        tmpSubStr := UpperCase(SubStr);
        i := Pos(tmpSubStr, tmpStr);
        Result := i;
        while i > 0 do
          begin
            i := PosEx(tmpSubStr, tmpStr, i + 1);
            if i > 0 then
              Result := i;
          end;
        if Result > 0 then
          Inc(Result, Length(tmpSubStr) - 1);
      end;
  end;

var
  SourceSize: int64;

  procedure ParseBuffer(Buf: TBytes; var IsReplaced: Boolean);
  var
    i: Integer;
    ReadedBufLen: Integer;
    BufStr: string;
    DestBytes: TBytes;
    LastIndex: Integer;
  begin
    if IsReplaced and (not(rfReplaceAll in ReplaceFlags)) then
      begin
        FtmpFile.Write(Buf, Length(Buf));
        Exit;
      end;

    // 1. Get chars from buffer
    ReadedBufLen := 0;
    for i := Length(Buf) downto 0 do
      if FEncoding.GetCharCount(Buf, 0, i) <> 0 then
        begin
          ReadedBufLen := i;
          Break;
        end;
    if ReadedBufLen = 0 then
      raise EEncodingError.Create('Cant convert bytes to str');

    FSourceFile.Seek(ReadedBufLen - Length(Buf), soCurrent);

    BufStr := FEncoding.GetString(Buf, 0, ReadedBufLen);
    if rfIgnoreCase in ReplaceFlags then
      IsReplaced := ContainsText(BufStr, AFrom)
    else
      IsReplaced := ContainsStr(BufStr, AFrom);

    if IsReplaced then
      begin
        LastIndex := GetLastIndex(BufStr, AFrom);
        LastIndex := Max(LastIndex, Length(BufStr) - Length(AFrom) + 1);
      end
    else
      LastIndex := Length(BufStr);

    SetLength(BufStr, LastIndex);
    FSourceFile.Seek(FEncoding.GetByteCount(BufStr) - ReadedBufLen, soCurrent);

    BufStr := StringReplace(BufStr, AFrom, ATo, ReplaceFlags);
    DestBytes := FEncoding.GetBytes(BufStr);
    FtmpFile.Write(DestBytes, Length(DestBytes));
  end;

var
  Buf: TBytes;
  BufLen: Integer;
  bReplaced: Boolean;
begin
  FSourceFile.Seek(0, soBeginning);
  FtmpFile.Size := 0;
  CopyPreamble;

  SourceSize := FSourceFile.Size;
  BufLen := Max(FEncoding.GetByteCount(AFrom) * 5, 2048);
  BufLen := Max(FEncoding.GetByteCount(ATo) * 5, BufLen);
  SetLength(Buf, BufLen);

  bReplaced := False;
  while FSourceFile.Position < SourceSize do
    begin
      BufLen := FSourceFile.Read(Buf, Length(Buf));
      SetLength(Buf, BufLen);
      ParseBuffer(Buf, bReplaced);
    end;

  FSourceFile.Size := 0;
  FSourceFile.CopyFrom(FtmpFile, 0);
end;

Function GetNumEdges(MeTISSrc : string): String;
var
  i,
  j       : Integer;

Const
  SOffset = 13;

Begin
      i       :=  Pos('I only found ', MeTISSrc);
      j       :=  Pos(' edges in the file.',MeTISSrc);
      Result  :=  copy(MeTISSrc,(i + SOffset),(j-(i + SOffset)));// Gets the # of edges proposed by MeTIS
End;

Function RunMeTIS(DosApp: string): String;
const
    READ_BUFFER_SIZE = 2400;
var
    Security    : TSecurityAttributes;
    readableEndOfPipe, writeableEndOfPipe: THandle;
    start       : TStartUpInfo;
    ProcessInfo : TProcessInformation;
    Buffer      : PAnsiChar;
    BytesRead   : DWORD;
    AppRunning  : DWORD;
    AppReturn   : String;
begin
    Security.nLength := SizeOf(TSecurityAttributes);
    Security.bInheritHandle := True;
    Security.lpSecurityDescriptor := nil;

    if CreatePipe({var}readableEndOfPipe, {var}writeableEndOfPipe, @Security, 0) then
    begin
        Buffer := AllocMem(READ_BUFFER_SIZE+1);
        FillChar(Start, Sizeof(Start), #0);
        start.cb := SizeOf(start);
        // Set up members of the STARTUPINFO structure.
        // This structure specifies the STDIN and STDOUT handles for redirection.
        // - Redirect the output and error to the writeable end of our pipe.
        // - We must still supply a valid StdInput handle (because we used STARTF_USESTDHANDLES to swear that all three handles will be valid)
        start.dwFlags := start.dwFlags or STARTF_USESTDHANDLES;
        start.hStdInput := GetStdHandle(STD_INPUT_HANDLE); //we're not redirecting stdInput; but we still have to give it a valid handle
        start.hStdOutput := writeableEndOfPipe; //we give the writeable end of the pipe to the child process; we read from the readable end
        start.hStdError := writeableEndOfPipe;

        //We can also choose to say that the wShowWindow member contains a value.
        //In our case we want to force the console window to be hidden.
        start.dwFlags := start.dwFlags + STARTF_USESHOWWINDOW;
        start.wShowWindow := SW_HIDE;

        // Don't forget to set up members of the PROCESS_INFORMATION structure.
        ProcessInfo := Default(TProcessInformation);

        //WARNING: The unicode version of CreateProcess (CreateProcessW) can modify the command-line "DosApp" string.
        //Therefore "DosApp" cannot be a pointer to read-only memory, or an ACCESS_VIOLATION will occur.
        //We can ensure it's not read-only with the RTL function: UniqueString
        UniqueString({var}DosApp);

        if CreateProcess(nil, PChar(DosApp), nil, nil, True, NORMAL_PRIORITY_CLASS, nil, nil, start, {var}ProcessInfo) then
        begin
            //Wait for the application to terminate, as it writes it's output to the pipe.
            //WARNING: If the console app outputs more than 2400 bytes (ReadBuffer),
            //it will block on writing to the pipe and *never* close.
            repeat
                Apprunning := WaitForSingleObject(ProcessInfo.hProcess, 100);
            until (Apprunning <> WAIT_TIMEOUT);
            //Read the contents of the pipe out of the readable end
            //WARNING: if the console app never writes anything to the StdOutput, then ReadFile will block and never return
            repeat
                BytesRead := 0;
                ReadFile(readableEndOfPipe, Buffer[0], READ_BUFFER_SIZE, {var}BytesRead, nil);
                Buffer[BytesRead]:= #0;
                OemToAnsi(Buffer,Buffer);
                Result := Result + String(Buffer);
            until (BytesRead < READ_BUFFER_SIZE);
        end
        else
          Result  :=  '**Error**';
        FreeMem(Buffer);
        CloseHandle(ProcessInfo.hProcess);
        CloseHandle(ProcessInfo.hThread);
        CloseHandle(readableEndOfPipe);
        CloseHandle(writeableEndOfPipe);
    end;
end;

end.
