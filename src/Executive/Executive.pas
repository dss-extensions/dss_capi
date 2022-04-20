unit Executive;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

USES
      Classes,
      DSSPointerList,
      Command,
      Contnrs,
      DSSClass,
      CAPI_Utils,
      CAPI_Types;

TYPE
     TExecutive = class(TObject)
     private
         FRecorderOn: Boolean;
         FRecorderFile:String;
         RecorderFile: TFileStream;

         FUNCTION Get_LastError:String;
         FUNCTION Get_ErrorResult:Integer;

         function Get_Command: String;
         
         procedure Set_RecorderOn(const Value: Boolean);
     public
         DSS: TDSSContext;
         CommandList: TCommandList;
         OptionList: TCommandList;

         constructor Create(dssContext: TDSSContext);
         destructor  Destroy; override;

         PROCEDURE CreateDefaultDSSItems;
         Procedure Write_to_RecorderFile(const s:String);

         Procedure Clear(Resetting: Boolean = True);
{$IFDEF DSS_CAPI_PM}
         Procedure ClearAll;
{$ENDIF}

         procedure Set_Command(const Value: String); overload;
         procedure Set_Command(const Value: String; LineNum: Integer); overload;

         Property Command:String   read Get_Command write Set_Command;
         Property Error:Integer    read Get_ErrorResult;
         Property LastError:String read Get_LastError;
         Property RecorderOn:Boolean Read FRecorderOn write Set_RecorderOn;

         // ZIP functions
         procedure ZipOpen(ZipFileName: String);
         procedure ZipClose();
         procedure ZipRedirect(FileInZip: String);
         procedure ZipExtract(var ResultPtr: PByte; ResultCount: PAPISize; FileInZip: String);
         function ZipHashes(var Hashes: TFPHashList): Boolean;
         function InZip: Boolean;
         function CurrentZipFileName: String;
         procedure SetInZipPath(path: String);
         function GetZipStream(fn: String): TStream;
     end;


implementation

USES BufStream, ExecCommands, ExecOptions,
     ExecHelper, DSSClassDefs, DSSGlobals, ParserDel,  SysUtils,
     Utilities, Solution, DSSHelper,
     CmdForms,
     Zipper,
     StrUtils;

type
    TDSSUnZipper = class(TUnZipper)
    public
        Enabled: Boolean;
        ziphash: TFPHashList;
        dataStream: TStream;

        constructor Create(fn: String);
        destructor Destroy; override;
        procedure DoCreateOutZipStream(sender: TObject; var stream: TStream; it: TFullZipFileEntry);
        procedure DoDoneOutZipStream(sender: TObject; var stream: TStream; it: TFullZipFileEntry);
        function GetFile(fn: String): TStream;
        procedure PrepareHashmap();
    end;

function TExecutive.InZip: Boolean;
begin
    Result := (DSS.unzipper <> NIL) and (TDSSUnZipper(DSS.unzipper).Enabled);
end;

function TExecutive.CurrentZipFileName: String;
begin
    Result := TDSSUnZipper(DSS.unzipper).FileName;
end;

Constructor TExecutive.Create(dssContext: TDSSContext);
Begin
     Inherited Create;

     DSS := dssContext;
      
     // Exec Commands
     CommandList := TCommandList.Create(ExecCommand);

     // Exec options
     OptionList := TCommandList.Create(ExecOption);

     // Instantiate All DSS Classe Definitions, Intrinsic and User-defined
     CreateDSSClasses(DSS);     // in DSSGlobals

     DSS.Circuits := TDSSPointerList.Create(2);   // default buffer for 2 active circuits
     DSS.NumCircuits := 0;
     DSS.ActiveCircuit := nil;

     DSS.LastCmdLine := '';
     DSS.RedirFile := '';

     FRecorderOn := FALSE;
     FrecorderFile := '';

     // Override Locale defaults so that CSV files get written properly
     FormatSettings.DecimalSeparator  := '.';
     FormatSettings.ThousandSeparator := ',';
end;

destructor TExecutive.Destroy;
begin
    if RecorderOn then 
        RecorderOn := FALSE;

    Clear(False);
    
    CommandList.Free;
    OptionList.Free;
    DSS.Circuits.Free;

    inherited Destroy;
end;

FUNCTION TExecutive.Get_LastError:String;
Begin
     Result := DSS.LastErrorMessage;
End;

FUNCTION TExecutive.Get_ErrorResult:Integer;
Begin
     Result := DSS.ErrorNumber;
End;

PROCEDURE TExecutive.CreateDefaultDSSItems;
// Create default loadshapes, growthshapes, and other general DSS objects
// used by all circuits.
begin
    // this load shape used for generator dispatching, etc.   Loads may refer to it, also. 
    Command := 'new loadshape.default npts=24 1.0 mult=(.677 .6256 .6087 .5833 .58028 .6025 .657 .7477 .832 .88 .94 .989 .985 .98 .9898 .999 1 .958 .936 .913 .876 .876 .828 .756)';
    IF DSS.CmdResult <> 0 THEN
        Exit;

    Set_Command('new growthshape.default 2 year="1 20" mult=(1.025 1.025)');  // 20 years at 2.5%
    Set_Command('new spectrum.default 7  Harmonic=(1 3 5 7 9 11 13)  %mag=(100 33 20 14 11 9 7) Angle=(0 0 0 0 0 0 0)');
    Set_Command('new spectrum.defaultload 7  Harmonic=(1 3 5 7 9 11 13)  %mag=(100 1.5 20 14 1 9 7) Angle=(0 180 180 180 180 180 180)');
    Set_Command('new spectrum.defaultgen 7  Harmonic=(1 3 5 7 9 11 13)  %mag=(100 5 3 1.5 1 .7 .5) Angle=(0 0 0 0 0 0 0)');
    Set_Command('new spectrum.defaultvsource 1  Harmonic=(1 )  %mag=(100 ) Angle=(0 ) ');
    Set_Command('new spectrum.linear 1  Harmonic=(1 )  %mag=(100 ) Angle=(0 ) ');
    Set_Command('new spectrum.pwm6 13  Harmonic=(1 3 5 7 9 11 13 15 17 19 21 23 25) %mag=(100 4.4 76.5 62.7 2.9 24.8 12.7 0.5 7.1 8.4 0.9 4.4 3.3) Angle=(-103 -5 28 -180 -33 -59 79 36 -253 -124 3 -30 86)');
    Set_Command('new spectrum.dc6 10  Harmonic=(1 3 5 7 9 11 13 15 17 19)  %mag=(100 1.2 33.6 1.6 0.4 8.7  1.2  0.3  4.5 1.3) Angle=(-75 28 156 29 -91 49 54 148 -57 -46)');
    DSS.SpectrumClass.BindDefaults();
    Set_Command('New TCC_Curve.A 5 c_array=(1, 2.5, 4.5, 8.0, 14.)  t_array=(0.15 0.07 .05 .045 .045) ');
    Set_Command('New TCC_Curve.D 5 c_array=(1, 2.5, 4.5, 8.0, 14.)  t_array=(6 0.7 .2 .06 .02)');
    Set_Command('New TCC_Curve.TLink 7 c_array=(2 2.1 3 4 6 22 50)  t_array=(300 100 10.1 4.0 1.4 0.1  0.02)');
    Set_Command('New TCC_Curve.KLink 6 c_array=(2 2.2 3 4 6 30)    t_array=(300 20 4 1.3 0.41 0.02)');
    Set_Command('New "TCC_Curve.uv1547" npts=2 C_array=(0.5, 0.9, ) T_array=(0.166, 2, )');
    Set_Command('New "TCC_Curve.ov1547" npts=2 C_array=(1.1, 1.2, ) T_array=(2, 0.166, )');
    Set_Command('New "TCC_Curve.mod_inv" npts=15 C_array=(1.1, 1.3, 1.5, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 50, 100, ) T_array=(27.1053, 9.9029, 6.439, 3.8032, 2.4322, 1.9458, 1.6883, 1.5255, 1.4117, 1.3267, 1.2604, 1.2068, 0.9481, 0.7468, 0.6478, )');
    Set_Command('New "TCC_Curve.very_inv" npts=15 C_array=(1.1, 1.3, 1.5, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 50, 100, ) T_array=(93.872, 28.9113, 16.179, 7.0277, 2.9423, 1.7983, 1.3081, 1.0513, 0.8995, 0.8023, 0.7361, 0.6891, 0.5401, 0.4988, 0.493, )');
    Set_Command('New "TCC_Curve.ext_inv" npts=15 C_array=(1.1, 1.3, 1.5, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 50, 100, ) T_array=(134.4074, 40.9913, 22.6817, 9.5217, 3.6467, 2.0017, 1.2967, 0.9274, 0.7092, 0.5693, 0.4742, 0.4065, 0.1924, 0.133, 0.1245, )');
    Set_Command('New "TCC_Curve.definite" npts=3 C_array=(1, 1.001, 100, ) T_array=(300, 1, 1, )');
end;

function TExecutive.Get_Command: String;
begin
    Result := DSS.LastCmdLine;
end;

procedure TExecutive.Set_Command(const Value: String); overload;
begin
    Set_Command(Value, -1)
end;

procedure TExecutive.Set_Command(const Value: String; LineNum: Integer);
{$IFDEF DSS_CAPI_PM}
var
    idx: Integer;
    PMParent, ChDSS: TDSSContext;
begin
    PMParent := DSS.GetPrime();
    ChDSS := DSS.ActiveChild;
    if ChDSS = NIL then
        ChDSS := DSS;
{$ELSE}
begin
{$ENDIF}
{$IFDEF DSS_CAPI_PM}
    if PMParent.AllActors then
    begin
        for idx := 0 to High(PMParent.Children) do
        begin
            //TODO: if in the future the commands are processed in threads, this would need a lock, and
            //      maybe allow certain commands only in the DSSPrime instance to simplify things
            if not PMParent.AllActors then 
            begin
                Exit;
            end;

            ChDSS := PMParent.Children[idx];
            PMParent.ActiveChild := ChDSS;
            PMParent.ActiveChildIndex := idx;
            ProcessCommand(ChDSS, Value);
        end;
        PMParent.ActiveChild := PMParent;
        PMParent.ActiveChildIndex := 0;
    end
    else
{$ENDIF}
        ProcessCommand({$IFDEF DSS_CAPI_PM}ChDSS{$ELSE}DSS{$ENDIF}, Value);
end;

procedure TExecutive.Clear(Resetting: Boolean = True);
begin
    if (DSS.NumCircuits > 0) OR (DSS_CAPI_LEGACY_MODELS <> DSS_CAPI_LEGACY_MODELS_PREV) then
	begin
{$IFDEF DSS_CAPI_PM}
        // In case the actor hasn't been destroyed
        if DSS.ActorThread <> NIL then
        begin
            DSS.ActorThread.Send_Message(TActorMessage.EXIT_ACTOR);
            DSS.ActorThread.WaitFor();
            DSS.ActorThread.Free();
            DSS.ActorThread := NIL;
        end;
{$ENDIF}        
    	if DSS.DIFilesAreOpen then
        	DSS.EnergyMeterClass.CloseAllDIFiles;

        // First get rid of all existing stuff
        ClearAllCircuits_SingleContext(DSS);
        DisposeDSSClasses(DSS);
        if Resetting then
        begin
            // Now, Start over
            CreateDSSClasses(DSS);
            CreateDefaultDSSItems;
        end;
    end
    else if not Resetting then
        DisposeDSSClasses(DSS);
        
    if not Resetting then
        Exit;

    DSS.DefaultEarthModel := DERI;
    DSS.LogQueries := FALSE;
    DSS.MaxAllocationIterations := 2;

    // Prepare for new variables
    DSS.ParserVars.Free;
    DSS.ParserVars := TParserVar.Create(100);  // start with space for 100 variables
    DSS.Parser.SetVars(DSS.ParserVars);
    DSS.AuxParser.SetVars(DSS.ParserVars);
    DSS.PropParser.SetVars(DSS.ParserVars);

    {$IFNDEF FPC}
    if not IsDLL then
        ControlPanel.UpdateElementBox;
    {$ENDIF}
end;

{$IFDEF DSS_CAPI_PM}
procedure TExecutive.ClearAll;
var
    PMParent: TDSSContext;
    i: integer;
begin
    PMParent := DSS.GetPrime();
    
    for i := 1 to high(PMParent.Children) do
        PMParent.Children[i].Free;
    SetLength(PMParent.Children, 1);
    PMParent.ActiveChildIndex := 0;
    PMParent.ActiveChild := PMParent;
    PMParent.DSSExecutive.Clear();
end;
{$ENDIF}

procedure TExecutive.Set_RecorderOn(const Value: Boolean);
begin
    If Value Then 
    Begin
        If Not FRecorderOn Then 
        Begin
            FRecorderFile := DSS.OutputDirectory + 'DSSRecorder.dss' ;
            RecorderFile := TBufferedFileStream.Create(FRecorderFile, fmCreate);
        End
        else
        begin
            RecorderFile.Free();
            RecorderFile := TBufferedFileStream.Create(FRecorderFile, fmCreate);
        end;
    End 
    Else If FRecorderOn Then 
    Begin
        FreeAndNil(RecorderFile);
    End;
    DSS.GlobalResult := FRecorderFile;
    FRecorderOn := Value;
end;

procedure TExecutive.Write_to_RecorderFile(const s: String);
begin
   FSWriteln(Recorderfile, S);
end;

function TExecutive.ZipHashes(var Hashes: TFPHashList): Boolean;
var
    unzipper: TDSSUnZipper = NIL;
begin
    Result := False;
    if DSS.unzipper = NIL then 
        Exit;

    unzipper := TDSSUnZipper(DSS.unzipper);
    Hashes := unzipper.ziphash;
    if Hashes <> NIL then
        Result := True;
end;

constructor TDSSUnZipper.Create(fn: String);
begin
    //TODO: test on PM scenarios (multiple threads processing the same ZIP).
    //      If it doesn't work, we need to use OnOpenInputStream and replace the
    //      input stream with one of our own.
    inherited Create;
    FileName := fn;
    dataStream := NIL;
    OnCreateStream := DoCreateOutZipStream;
    OnDoneStream := DoDoneOutZipStream;
    ziphash := TFPHashList.Create();
end;

procedure TDSSUnZipper.PrepareHashmap();
var
    i: Integer;
begin
    Examine();
    for i := 0 to Entries.Count - 1 do
    begin
        if Length(Entries[i].ArchiveFileName) > 255 then
        begin
            raise Exception.Create('ZIP archive contains file names longer than 255 chars. This is currently unsupported.');
        end;
        ziphash.Add(Entries[i].ArchiveFileName, Pointer(i + 1));
    end;
end;

destructor TDSSUnZipper.Destroy;
begin
    ziphash.Free;
    inherited Destroy;
end;

function TDSSUnZipper.GetFile(fn: String): TStream;
var
    i: Integer;
begin
    i := Integer(ziphash.Find(fn)) - 1;
    Result := NIL;
    if i < 0 then
        Exit;

    try
        OpenInput();
        UnZipOneFile(Entries[i]);
        Result := dataStream;
    
    finally
        CloseInput();
    end;
end;

procedure TDSSUnZipper.DoCreateOutZipStream(sender: TObject; var stream: TStream; it: TFullZipFileEntry);
begin
    stream := TMemoryStream.Create;
    dataStream := stream;
end;

procedure TDSSUnZipper.DoDoneOutZipStream(sender: TObject; var stream: TStream; it: TFullZipFileEntry);
begin
    stream.Position := 0;
end;

function TExecutive.GetZipStream(fn: String): TStream;
var    
    unzipper: TDSSUnZipper;
    cwd: String;
    efn: String = '';
begin
    Result := NIL;
    try
        if Length(fn) > 1 then
        begin
            // If there's a leading slash, remove it an use the rest directly
            while (fn[1] = '\') or (fn[1] = '/') do
                fn := Copy(fn, 2, Length(fn) - 1);

            efn := fn;
        end
        else
        begin
            // Without a slash, assume relative path to current file (in ZIP)
            efn := DSS.inZipPath + fn
        end;

        unzipper := TDSSUnZipper(DSS.unzipper);

        // Try the given path as is
        try
            Result := unzipper.GetFile(efn);
        except
            on E: Exception do
            begin
            end;
        end;

        // Try to get a simpler version (expand dots, etc.)
        if Result = NIL then
        begin
            efn := ExpandFileName(DSS.inZipPath + fn);
            cwd := GetCurrentDir() + PathDelim;
            efn := ExtractRelativePath(cwd, efn);
            Result := unzipper.GetFile(efn);
        end;

        // Try switching backslashes<->slashes
        if Result = NIL then
        begin
            efn := ReplaceStr(efn, '/', '\');
            Result := unzipper.GetFile(efn);
            if Result = NIL then
            begin
                efn := ReplaceStr(efn, '\', '/');
                Result := unzipper.GetFile(efn);
            end;
        end;
            
    except
        on E: Exception do
        begin
        end;
    end;
    if Result = NIL then
    begin
        if fn <> efn then
            raise Exception.Create(Format(_('Could not read file "%s" ("%s") from ZIP "%s".'), [fn, efn, CurrentZipFileName]))
        else
            raise Exception.Create(Format(_('Could not read file "%s" from ZIP "%s".'), [fn, CurrentZipFileName]));
    end;
end;

procedure TExecutive.SetInZipPath(path: String);
begin
    if path = DSS.inZipPath then
        Exit;

    if Length(Path) <> 0 then
    begin
        if (Path[High(Path)] <> '/') and (Path[High(Path)] <> '\') then
        begin
            Path := Path + PathDelim;
        end;

        while (Path[1] = '\') or (Path[1] = '/') do
            Path := Copy(Path, 2, Length(Path) - 1);

    end;
    DSS.inZipPath := path;
end;

procedure TExecutive.ZipOpen(ZipFileName: String);
var
    unzipper: TDSSUnZipper = NIL;
begin
    if (not DSS_CAPI_ALLOW_CHANGE_DIR) then
        ZipFileName := ExpandFileName(AdjustInputFilePath(DSS, ZipFileName));

    try
        unzipper := TDSSUnZipper.Create(ZipFileName);
        unzipper.PrepareHashmap();
    except
        on E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error preparing ZIP "%s": %s', [ZipFileName, E.message], 4016);
            FreeAndNil(unzipper);
        end;
    end;
    if DSS.unzipper <> NIL then
        DSS.unzipper.Free();
    
    DSS.unzipper := unzipper;
end;

procedure TExecutive.ZipClose();
begin
    FreeAndNil(DSS.unzipper);
end;

procedure TExecutive.ZipRedirect(FileInZip: String);
var
    u: TDSSUnZipper = NIL;
begin
    try
        u := TDSSUnZipper(DSS.unzipper);
        u.Enabled := True;
        SetInZipPath('');

        DSS.Redirect_Abort := False;
        DSS.SolutionAbort := False;

        // Do the actual redirect to the file, while wrapping streams
        // and loading inputs from the UnZipper.
        Set_Command(Format('redirect "%s"', [FileInZip]));
    except
        on E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error reading script "%s" from ZIP "%s": %s', [FileInZip, CurrentZipFileName(), E.message], 4016);
            Exit;
        end;
    end;
    
    u.Enabled := False;

    if DSS.ErrorNumber = 0 then
        Exit;
        
    DSS.LastErrorMessage := DSS.LastErrorMessage + CRLF + 
        Format(_('[ZIP file: "%s"]'), [CurrentZipFileName()]);
end;

procedure TExecutive.ZipExtract(var ResultPtr: PByte; ResultCount: PAPISize; FileInZip: String);
var
    Fstream: TStream = NIL;
begin
    try
        Fstream := GetZipStream(FileInZip);
        DSS_RecreateArray_PByte(ResultPtr, ResultCount, Fstream.Size);
        Fstream.ReadBuffer(ResultPtr^, ResultCount[0]);
        FreeAndNil(Fstream);
    except
        on E: Exception do
        begin
            DoSimpleMsg(DSS, 'File "%s" could not be extracted: %s', [FileInZip, E.Message], 2203);
            DSS.LastErrorMessage := DSS.LastErrorMessage + CRLF + 
                Format(_('[ZIP file: "%s"]'), [CurrentZipFileName()]);
            FreeAndNil(Fstream);
            Exit;
        end;
    end;
end;

end.

