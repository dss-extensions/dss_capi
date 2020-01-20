unit DSSGlobals;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2019, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{$WARN UNIT_PLATFORM OFF}

interface

Uses Classes, DSSClassDefs, DSSObject, DSSClass, ParserDel, Hashlist, PointerList,
     UComplex, Arraydef, CktElement, Circuit, IniRegSave, {$IFNDEF FPC}Graphics, System.IOUtils, {$ENDIF}inifiles,

     {$IFDEF UNIX}BaseUnix,{$ENDIF}

     {Some units which have global vars defined here}
     Spectrum,
     LoadShape,
     TempShape,
     PriceShape,
     XYCurve,
     GrowthShape,
     Monitor,
     EnergyMeter,
     Sensor,
     TCC_Curve,
     Feeder,
     WireData,
     CNData,
     TSData,
     LineSpacing,
     Storage,
     PVSystem,
     InvControl,
     ExpControl,

     LineCode,
     LineGeometry,
     Line,
     VSource,
     ISource,
     VCCS,
     Load,
     Transformer,
     RegControl,
     Capacitor,
     Reactor,
     CapControl,
     Fault,
     Generator,
     GenDispatcher,
     StorageController,
     Relay,
     Recloser,
     Fuse,
     SwtControl,
     UPFC,
     UPFCControl,
     ESPVLControl,
     IndMach012,
     GICsource, // GIC source
     AutoTrans, // Auto Transformer
     VSConverter,
     XfmrCode,
     GICLine,
     GICTransformer;


CONST
      CRLF = sLineBreak;

      PI =  3.14159265359;

      TwoPi = 2.0 * PI;

      RadiansToDegrees = 57.29577951;

      EPSILON = 1.0e-12;   // Default tiny floating point
      EPSILON2 = 1.0e-3;   // Default for Real number mismatch testing

//TODO: move the following to scoped enums!
      POWERFLOW  = 1;  // Load model types for solution
      ADMITTANCE = 2;

      // For YPrim matrices
      
      ALL_YPRIM = 0;
      SERIES = 1;
      SHUNT  = 2;

      {Control Modes}
      CONTROLSOFF = -1;
      EVENTDRIVEN =  1;
      TIMEDRIVEN  =  2;
      MULTIRATE   =  3;
      CTRLSTATIC  =  0;

      {Randomization Constants}
      GAUSSIAN  = 1;
      UNIFORM   = 2;
      LOGNORMAL = 3;

      {Autoadd Constants}
      GENADD = 1;
      CAPADD = 2;

      {ERRORS}
      SOLUTION_ABORT = 99;

      {For General Sequential Time Simulations}
      USEDAILY  = 0;
      USEYEARLY = 1;
      USEDUTY   = 2;
      USENONE   =-1;

      {Earth Model}
      SIMPLECARSON  = 1;
      FULLCARSON    = 2;
      DERI          = 3;

      {Profile Plot Constants}
      PROFILE3PH = 9999; // some big number > likely no. of phases
      PROFILEALL = 9998;
      PROFILEALLPRI = 9997;
      PROFILELLALL = 9996;
      PROFILELLPRI = 9995;
      PROFILELL    = 9994;
      PROFILEPUKM = 9993;  // not mutually exclusive to the other choices 9999..9994
      PROFILE120KFT = 9992;  // not mutually exclusive to the other choices 9999..9994

      IsDLL = True;

VAR
    DLLFirstTime   :Boolean=TRUE;
    DLLDebugFile   :TextFile;
    ProgramName    :String;
{$IFNDEF DSS_CAPI} // Disable DSS_Registry completely when building the DSS_CAPI DLL
    DSS_Registry   :TIniRegSave; // Registry   (See Executive)
{$ENDIF}
{$IFDEF DSS_CAPI}
    DSS_CAPI_INFO_SPARSE_COND : Boolean;
    DSS_CAPI_EARLY_ABORT : Boolean;
    DSS_CAPI_ALLOW_EDITOR: Boolean;
    DSS_CAPI_LOADS_TERMINAL_CHECK: Boolean = True;
{$ENDIF}
    // Global variables for the DSS visualization tool
    DSS_Viz_installed   :Boolean=False; // DSS visualization tool (flag of existance)
    DSS_Viz_path: String;
    DSS_Viz_enable: Boolean=False;

    NoFormsAllowed  :Boolean;

    CALPHA             :Complex;  {120-degree shift constant}
    SQRT2              :Double;
    SQRT3              :Double;
    InvSQRT3           :Double;
    InvSQRT3x1000      :Double;

    DefaultEditor    :String;     // normally, Notepad
    DefaultFontSize  :Integer;
    DefaultFontName  :String;
    DefaultFontStyles :{$IFNDEF FPC}TFontStyles{$ELSE}Integer{$ENDIF};
    DSSFileName      :String;     // Name of current exe or DLL
    DSSDirectory     :String;     // where the current exe resides
    MaxCircuits     :Integer;
    StartupDirectory :String;     // Where we started
    VersionString      :String;
    CPU_Freq           : int64;   // Used to store the CPU performance counter frequency (not the actual CPU frequency)
    CPU_Cores          : integer;
    GlobalDefaultBaseFreq: Double = 60.0;
{$IFNDEF DSS_CAPI}    
    UpdateRegistry     :Boolean;  // update on program exit
{$ENDIF}

procedure DoErrorMsg(DSS: TDSSContext; Const S, Emsg, ProbCause :String; ErrNum:Integer);
procedure DoSimpleMsg(DSS: TDSSContext; Const S :String; ErrNum:Integer);

procedure ClearAllCircuits(DSS: TDSSContext);

procedure SetObject(DSS: TDSSContext; const param :string);
function  SetActiveBus(DSS: TDSSContext; const BusName:String):Integer;
procedure SetDataPath(DSS: TDSSContext; const PathName:String);

procedure SetLastResultFile(DSS: TDSSContext; Const Fname:String);

procedure MakeNewCircuit(DSS: TDSSContext; Const Name:String);

procedure AppendGlobalResult(DSS: TDSSContext; Const s:String);
procedure AppendGlobalResultCRLF(DSS: TDSSContext; const S:String);  // Separate by CRLF

procedure ResetQueryLogFile(DSS: TDSSContext);
procedure WriteQueryLogFile(DSS: TDSSContext; Const Prop, S:String);

procedure WriteDLLDebugFile(DSS: TDSSContext; Const S:String);

{$IFNDEF DSS_CAPI} // Disable DSS_Registry completely when building the DSS_CAPI DLL
procedure ReadDSS_Registry(DSS: TDSSContext);
procedure WriteDSS_Registry(DSS: TDSSContext);
{$ENDIF}

{$IFDEF DSS_CAPI_PM}
procedure Wait4Actors(DSS: TDSSContext; ActorOffset: Integer);
procedure DoClone(DSS: TDSSContext);
procedure New_Actor_Slot(DSS: TDSSContext);
procedure New_Actor(DSS: TDSSContext);
{$ENDIF}

implementation



USES  {Forms,   Controls,}
     {$IFDEF MSWINDOWS}
     Windows,
     {$ENDIF}
     SysUtils,
     {$IFDEF DSS_CAPI}
     CAPI_Metadata,
     {$ENDIF}
     {$IFDEF DSS_CAPI_PM}
     syncobjs,
     {$ENDIF}
     {$IFDEF FPC}
     resource, versiontypes, versionresource, dynlibs, CmdForms,
     {$ELSE}
     DSSForms, SHFolder,
     {$ENDIF}
     Solution,
     Executive,
     ExecCommands,
     ExecOptions,
     DSSHelper;

TYPE

   THandle = NativeUint;

   //TDSSRegister = function(var ClassName: pchar):Integer;  // Returns base class 1 or 2 are defined
   // Users can only define circuit elements at present

VAR

   LastUserDLLHandle: THandle;
   //DSSRegisterProc:TDSSRegister;   // of last library loaded

{$IFDEF FPC}
FUNCTION GetDefaultDataDirectory: String;
Begin
{$IFDEF UNIX}
  Result := SysUtils.GetEnvironmentVariable('HOME') + PathDelim + 'Documents';
{$ENDIF}
{$IF (defined(Windows) or defined(MSWindows))}
  Result := SysUtils.GetEnvironmentVariable('HOMEDRIVE') + SysUtils.GetEnvironmentVariable('HOMEPATH') + PathDelim + 'Documents';
{$ENDIF}
end;

FUNCTION GetDefaultScratchDirectory: String;
Begin
  {$IFDEF UNIX}
  Result := '/tmp';
  {$ENDIF}
  {$IF (defined(Windows) or defined(MSWindows))}
  Result := SysUtils.GetEnvironmentVariable('LOCALAPPDATA');
  {$ENDIF}
End;
{$ELSE}
FUNCTION GetDefaultDataDirectory: String;
Var
  ThePath:Array[0..MAX_PATH] of char;
Begin
  FillChar(ThePath, SizeOF(ThePath), #0);
  SHGetFolderPath (0, CSIDL_PERSONAL, 0, 0, ThePath);
  Result := ThePath;
End;

FUNCTION GetDefaultScratchDirectory: String;
Var
  ThePath:Array[0..MAX_PATH] of char;
Begin
  FillChar(ThePath, SizeOF(ThePath), #0);
  SHGetFolderPath (0, CSIDL_LOCAL_APPDATA, 0, 0, ThePath);
  Result := ThePath;
End;
{$ENDIF}

{--------------------------------------------------------------}
// FUNCTION IsDSSDLL(Fname:String):Boolean;
// 
// Begin
//     Result := FALSE;
// 
//     // Ignore if "DSSLIB.DLL"
//     If CompareText(ExtractFileName(Fname),'dsslib.dll')=0 Then Exit;
// 
//    LastUserDLLHandle := LoadLibrary(pchar(Fname));
//    IF LastUserDLLHandle <> 0 then BEGIN
// 
//    // Assign the address of the DSSRegister proc to DSSRegisterProc variable
//     @DSSRegisterProc := GetProcAddress(LastUserDLLHandle, 'DSSRegister');
//     IF @DSSRegisterProc <> nil THEN Result := TRUE
//     ELSE FreeLibrary(LastUserDLLHandle);
// 
//   END;
// 
// End;
// 
//----------------------------------------------------------------------------
PROCEDURE DoErrorMsg(DSS: TDSSContext; Const S, Emsg, ProbCause:String; ErrNum:Integer);

VAR
    Msg:String;
    Retval:Integer;
Begin

     Msg := Format('Error %d Reported From OpenDSS Intrinsic Function: ', [Errnum])+ CRLF  + S
             + CRLF   + CRLF + 'Error Description: ' + CRLF + Emsg
             + CRLF   + CRLF + 'Probable Cause: ' + CRLF+ ProbCause;

     If Not NoFormsAllowed Then Begin

         If DSS.In_Redirect Then
         Begin
           RetVal := DSSMessageDlg(Msg, FALSE);
           If RetVal = -1 Then DSS.Redirect_Abort := True;
         End
         Else
           DSSMessageDlg(Msg, TRUE);

     End
     Else
     Begin
        {$IFDEF DSS_CAPI}
        if DSS_CAPI_EARLY_ABORT then
            DSS.Redirect_Abort := True;
        {$ENDIF}
     End;

     DSS.LastErrorMessage := Msg;
     DSS.ErrorNumber := ErrNum;
     AppendGlobalResultCRLF(DSS, Msg);
     DSS.SolutionAbort  :=  True;
End;

//----------------------------------------------------------------------------
PROCEDURE AppendGlobalResultCRLF(DSS: TDSSContext; const S:String);

Begin
    If Length(DSS.GlobalResult) > 0
    THEN DSS.GlobalResult := DSS.GlobalResult + CRLF + S
    ELSE DSS.GlobalResult := S;

    DSS.ErrorStrings.Add(Format('(%d) %s' ,[DSS.ErrorNumber, S]));  // Add to Error log
End;

//----------------------------------------------------------------------------
PROCEDURE DoSimpleMsg(DSS: TDSSContext; Const S:String; ErrNum:Integer);

VAR
    Retval:Integer;
Begin
    IF Not  NoFormsAllowed Then Begin
        IF DSS.In_Redirect THEN
        Begin
            RetVal := DSSMessageDlg(Format('(%d) OpenDSS %s%s', [Errnum, CRLF, S]), FALSE);
            {$IFDEF DSS_CAPI}
            if DSS_CAPI_EARLY_ABORT then
                DSS.Redirect_Abort := True;
            {$ENDIF}
            IF RetVal = -1 THEN
                DSS.Redirect_Abort := True;
        End
        ELSE
            DSSInfoMessageDlg(Format('(%d) OpenDSS %s%s', [Errnum, CRLF, S]));
    End
    Else
    Begin
        {$IFDEF DSS_CAPI}
        if DSS_CAPI_EARLY_ABORT then
            DSS.Redirect_Abort := True;
        {$ENDIF}
    End;

    DSS.LastErrorMessage := S;
    DSS.ErrorNumber := ErrNum;
    AppendGlobalResultCRLF(DSS, S);
End;


//----------------------------------------------------------------------------
PROCEDURE SetObject(DSS: TDSSContext; const param :string);

{Set object active by name}

VAR
   dotpos :Integer;
   ObjName, ObjClass :String;

Begin
      ObjClass := '';
      // Split off Obj class and name
      dotpos := Pos('.', Param);
      CASE dotpos OF
         0:ObjName := Copy(Param, 1, Length(Param));  // assume it is all name; class defaults
      ELSE Begin
           ObjClass := Copy(Param, 1, dotpos-1);
           ObjName  := Copy(Param, dotpos+1, Length(Param));
           End;
      End;

      IF Length(ObjClass) > 0 THEN SetObjectClass(DSS, ObjClass);

      DSS.ActiveDSSClass := DSS.DSSClassList.Get(DSS.LastClassReferenced);
      IF DSS.ActiveDSSClass <> Nil THEN
      Begin
        IF Not DSS.ActiveDSSClass.SetActive(Objname) THEN
        Begin // scroll through list of objects untill a match
          DoSimpleMsg(DSS, 'Error! Object "' + ObjName + '" not found.'+ CRLF + DSS.Parser.CmdString, 904);
        End
        ELSE
        With DSS.ActiveCircuit Do
        Begin
           CASE DSS.ActiveDSSObject.DSSObjType OF
                DSS_OBJECT: ;  // do nothing for general DSS object

           ELSE Begin   // for circuit types, set ActiveCircuit Element, too
                 ActiveCktElement := DSS.ActiveDSSClass.GetActiveObj;
                End;
           End;
        End;
      End
      ELSE
        DoSimpleMsg(DSS, 'Error! Active object type/class is not set.', 905);

End;

//----------------------------------------------------------------------------
FUNCTION SetActiveBus(DSS: TDSSContext; const BusName:String):Integer;
Begin

   // Now find the bus and set active
   Result := 0;

   WITH DSS.ActiveCircuit Do
     Begin
        If BusList.ListSize=0 Then Exit;   // Buslist not yet built
        ActiveBusIndex := BusList.Find(BusName);
        IF   ActiveBusIndex=0 Then
          Begin
            Result := 1;
            AppendGlobalResult(DSS, 'SetActiveBus: Bus ' + BusName + ' Not Found.');
          End;
     End;

End;

{$IFNDEF DSS_CAPI_PM}
procedure ClearAllCircuits(DSS: TDSSContext);
begin
    DSS.ActiveCircuit := DSS.Circuits.First;
    while DSS.ActiveCircuit <> nil do
    begin
        DSS.ActiveCircuit.Free;
        DSS.ActiveCircuit := DSS.Circuits.Next;
    end;
    DSS.Circuits.Free;
    DSS.Circuits := TPointerList.Create(2);   // Make a new list of circuits
    DSS.NumCircuits := 0;

    // Revert on key global flags to Original States
    DSS.DefaultEarthModel     := DERI;
    DSS.LogQueries            := FALSE;
    DSS.MaxAllocationIterations := 2;
End;
{$ELSE}
procedure ClearAllCircuits(DSS: TDSSContext);
var
    i : integer;
    PMParent: TDSSContext;
begin
    PMParent := DSS.GetPrime();
    
    for i := 0 to High(PMParent.Children) do
        with PMParent.Children[i] do
        begin
            ActiveCircuit := Circuits.First;
            while ActiveCircuit <> nil do
            begin
                ActiveCircuit.Free;
                ActiveCircuit := Circuits.Next;
            end;
            ActiveCircuit.NumCircuits := 0;
            Circuits.Free;
            Circuits := TPointerList.Create(2);   // Make a new list of circuits
            
            //TODO: check why v8 does this:
            //Parser.Free;
            //Parser := nil;
            
            // In case the actor hasn't been destroyed
            if ActorThread <> nil then
            begin
                //TODO: set SolutionAbort?
                ActorThread.Send_Message(EXIT_ACTOR);
                ActorThread.WaitFor;
                ActorThread.Free;
                ActorThread := nil;
            end;
            
            // Revert on key global flags to Original States
            DefaultEarthModel := DERI;
            LogQueries := FALSE;
            MaxAllocationIterations := 2;
        end;
        
    PMParent.ActiveChild := PMParent;
    PMParent.ActiveChildIndex := 0;
end;
{$ENDIF}// DSS_CAPI_PM


PROCEDURE MakeNewCircuit(DSS: TDSSContext; Const Name:String);
Var
    S: String;
Begin
     If DSS.NumCircuits <= MaxCircuits - 1 Then
     Begin
         DSS.ActiveCircuit := TDSSCircuit.Create(DSS, Name);
         DSS.ActiveDSSObject := DSS.ActiveSolutionObj;
         {*Handle := *} DSS.Circuits.Add(DSS.ActiveCircuit);
         Inc(DSS.NumCircuits);
         S := DSS.Parser.Remainder;    // Pass remainder of string on to vsource.
         {Create a default Circuit}
         DSS.SolutionAbort := False;
         {Voltage source named "source" connected to SourceBus}
         DSS.DSSExecutive.Command := 'New object=vsource.source Bus1=SourceBus ' + S;  // Load up the parser as if it were read in
     End
     Else
     Begin
         DoErrorMsg(DSS, 'MakeNewCircuit',
                    'Cannot create new circuit.',
                    'Max. Circuits Exceeded.'+CRLF+
                    '(Max no. of circuits='+inttostr(Maxcircuits)+')', 906);
     End;
End;


PROCEDURE AppendGlobalResult(DSS: TDSSContext; Const S:String);

// Append a string to Global result, separated by commas

Begin
    If Length(DSS.GlobalResult)=0 Then
        DSS.GlobalResult := S
    Else
        DSS.GlobalResult := DSS.GlobalResult + ', ' + S;
End;


{$IFDEF DSS_CAPI}
FUNCTION GetDSSVersion: String;
BEGIN
    Result := 'DSS C-API Library version ' + DSS_CAPI_VERSION +
              ' revision ' + DSS_CAPI_REV +
              ' based on OpenDSS SVN ' + DSS_CAPI_SVN_REV +
              ' (v7/classic variation)'
    {$IFDEF DSS_CAPI_MVMULT}
              + ' MVMULT'
    {$ENDIF}
              ;
END;
{$ELSE}
{$IFDEF FPC}
FUNCTION GetDSSVersion: String;
(* Unlike most of AboutText (below), this takes significant activity at run-    *)
 (* time to extract version/release/build numbers from resource information      *)
 (* appended to the binary.                                                      *)

 VAR     Stream: TResourceStream;
         vr: TVersionResource;
         fi: TVersionFixedInfo;

 BEGIN
   RESULT:= 'Unknown.';
   TRY

 (* This raises an exception if version info has not been incorporated into the  *)
 (* binary (Lazarus Project -> Project Options -> Version Info -> Version        *)
 (* numbering).                                                                  *)

     Stream:= TResourceStream.CreateFromID(HINSTANCE, 1, PChar(RT_VERSION));
     TRY
       vr:= TVersionResource.Create;
       TRY
         vr.SetCustomRawDataStream(Stream);
         fi:= vr.FixedInfo;
         RESULT := 'Version ' + IntToStr(fi.FileVersion[0]) + '.' + IntToStr(fi.FileVersion[1]) +
                ' release ' + IntToStr(fi.FileVersion[2]) + ' build ' + IntToStr(fi.FileVersion[3]) + LineEnding;
         vr.SetCustomRawDataStream(nil)
       FINALLY
         vr.Free
       END
     FINALLY
       Stream.Free
     END
   EXCEPT
   END
 End;
{$ELSE}
FUNCTION GetDSSVersion: String;
var
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
  MajorVer, MinorVer, BuildNo, RelNo :DWORD;
Begin
    Result := 'Unknown.' ;

    InfoSize := GetFileVersionInfoSize(PChar(DSSFileName), Wnd);
    if InfoSize <> 0 then
    begin
      GetMem(VerBuf, InfoSize);
      try
        if GetFileVersionInfo(PChar(DSSFileName), Wnd, InfoSize, VerBuf) then
          if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then  Begin
            MinorVer := FI.dwFileVersionMS and $FFFF;
            MajorVer := (FI.dwFileVersionMS and $FFFF0000) shr 16;
            BuildNo :=  FI.dwFileVersionLS and $FFFF;
            RelNo := (FI.dwFileVersionLS and $FFFF0000) shr 16;
            Result := Format('%d.%d.%d.%d',[MajorVer, MinorVer, RelNo, BuildNo]);
            End;
      finally
        FreeMem(VerBuf);
      end;
    end;
End;
{$ENDIF}
{$ENDIF} //IFDEF DSS_CAPI

PROCEDURE WriteDLLDebugFile(DSS: TDSSContext; Const S:String);

Begin

        AssignFile(DLLDebugFile, DSS.OutputDirectory + 'DSSDLLDebug.TXT');
        If DLLFirstTime then Begin
           Rewrite(DLLDebugFile);
           DLLFirstTime := False;
        end
        Else Append( DLLDebugFile);
        Writeln(DLLDebugFile, S);
        CloseFile(DLLDebugFile);

End;

{$IFNDEF UNIX}
function IsDirectoryWritable(const Dir: String): Boolean;
var
  TempFile: array[0..MAX_PATH] of Char;
begin
  if GetTempFileName(PChar(Dir), 'DA', 0, TempFile) <> 0 then
    {$IFDEF FPC}Result := DeleteFile(TempFile){$ELSE}Result := Windows.DeleteFile(TempFile){$ENDIF}
  else
    Result := False;
end;
{$ELSE}
function IsDirectoryWritable(const Dir: String): Boolean;
begin
  Result := (FpAccess(PChar(Dir), X_OK or W_OK) = 0);
end;
{$ENDIF}

PROCEDURE SetDataPath(DSS: TDSSContext; const PathName:String);
var
  ScratchPath: String;
// Pathname may be null
BEGIN
  if (Length(PathName) > 0) and not DirectoryExists(PathName) then Begin
  // Try to create the directory
    if not CreateDir(PathName) then Begin
      DoSimpleMsg(DSS, 'Cannot create ' + PathName + ' directory.', 907);
      Exit;
    End;
  End;

  DSS.DataDirectory := PathName;

  // Put a \ on the end if not supplied. Allow a null specification.
  If Length(DSS.DataDirectory) > 0 Then Begin
    ChDir(DSS.DataDirectory);   // Change to specified directory
    If DSS.DataDirectory[Length(DSS.DataDirectory)] <> PathDelim Then DSS.DataDirectory := DSS.DataDirectory + PathDelim;
  End;

  // see if DataDirectory is writable. If not, set OutputDirectory to the user's appdata
  if IsDirectoryWritable(DSS.DataDirectory) then begin
    DSS.OutputDirectory := DSS.DataDirectory;
  end else begin
    ScratchPath := GetDefaultScratchDirectory + PathDelim + ProgramName + PathDelim;
    if not DirectoryExists(ScratchPath) then CreateDir(ScratchPath);
    DSS.OutputDirectory := ScratchPath;
  end;
END;

{$IFNDEF DSS_CAPI} // Disable DSS_Registry completely when building the DSS_CAPI DLL
PROCEDURE ReadDSS_Registry(DSS: TDSSContext);
Var  TestDataDirectory:string;
Begin
  DSS_Registry.Section := 'MainSect';
  {$IFDEF Darwin}
     DefaultEditor    := DSS_Registry.ReadString('Editor', 'open -t');
     DefaultFontSize  := StrToInt(DSS_Registry.ReadString('ScriptFontSize', '12'));
     DefaultFontName  := DSS_Registry.ReadString('ScriptFontName', 'Geneva');
  {$ENDIF}
  {$IFDEF Linux}
     DefaultEditor    := DSS_Registry.ReadString('Editor', 'xdg-open');
     DefaultFontSize  := StrToInt(DSS_Registry.ReadString('ScriptFontSize', '10'));
     DefaultFontName  := DSS_Registry.ReadString('ScriptFontName', 'Arial');
  {$ENDIF}
  {$IF (defined(Windows) or defined(MSWindows))}
     DefaultEditor    := DSS_Registry.ReadString('Editor', 'Notepad.exe' );
     DefaultFontSize  := StrToInt(DSS_Registry.ReadString('ScriptFontSize', '8' ));
     DefaultFontName  := DSS_Registry.ReadString('ScriptFontName', 'MS Sans Serif' );
  {$ENDIF}
  {$IFDEF FPC}
     DefaultFontStyles := 1;
  {$ELSE}
     DefaultFontStyles := [];
     If DSS_Registry.ReadBool('ScriptFontBold', TRUE)    Then DefaultFontStyles := DefaultFontStyles + [fsbold];
     If DSS_Registry.ReadBool('ScriptFontItalic', FALSE) Then DefaultFontStyles := DefaultFontStyles + [fsItalic];
  {$ENDIF}
  GlobalDefaultBaseFreq  := StrToInt(DSS_Registry.ReadString('BaseFrequency', '60' ));
  LastFileCompiled := DSS_Registry.ReadString('LastFile', '' );
  TestDataDirectory :=   DSS_Registry.ReadString('DataPath', DSS.DataDirectory);
  If SysUtils.DirectoryExists (TestDataDirectory) Then SetDataPath (TestDataDirectory)
                                        Else SetDataPath (DSS.DataDirectory);
End;


PROCEDURE WriteDSS_Registry(DSS: TDSSContext);
Begin
  If UpdateRegistry Then  Begin
      DSS_Registry.Section := 'MainSect';
      DSS_Registry.WriteString('Editor',        DefaultEditor);
      DSS_Registry.WriteString('ScriptFontSize', Format('%d',[DefaultFontSize]));
      DSS_Registry.WriteString('ScriptFontName', Format('%s',[DefaultFontName]));
      DSS_Registry.WriteBool('ScriptFontBold', {$IFDEF FPC}False{$ELSE}(fsBold in DefaultFontStyles){$ENDIF});
      DSS_Registry.WriteBool('ScriptFontItalic', {$IFDEF FPC}False{$ELSE}(fsItalic in DefaultFontStyles){$ENDIF});
      DSS_Registry.WriteString('BaseFrequency', Format('%d',[Round(GlobalDefaultBaseFreq)]));
      DSS_Registry.WriteString('LastFile',      LastFileCompiled);
      DSS_Registry.WriteString('DataPath', DataDirectory);
  End;
End;
{$ENDIF}

PROCEDURE ResetQueryLogFile(DSS: TDSSContext);
Begin
     DSS.QueryFirstTime := TRUE;
End;


PROCEDURE WriteQueryLogfile(DSS: TDSSContext; Const Prop, S:String);

{Log file is written after a query command if LogQueries is true.}

Begin

  TRY
        DSS.QueryLogFileName :=  DSS.OutputDirectory + 'QueryLog.CSV';
        AssignFile(DSS.QueryLogFile, DSS.QueryLogFileName);
        If DSS.QueryFirstTime then
        Begin
             Rewrite(DSS.QueryLogFile);  // clear the file
             Writeln(DSS.QueryLogFile, 'Time(h), Property, Result');
             DSS.QueryFirstTime := False;
        end
        Else Append(DSS.QueryLogFile);

        Writeln(DSS.QueryLogFile,Format('%.10g, %s, %s',[DSS.ActiveCircuit.Solution.DynaVars.dblHour, Prop, S]));
        CloseFile(DSS.QueryLogFile);
  EXCEPT
        On E:Exception Do DoSimpleMsg(DSS, 'Error writing Query Log file: ' + E.Message, 908);
  END;

End;

PROCEDURE SetLastResultFile(DSS: TDSSContext; Const Fname:String);

Begin
      DSS.LastResultfile := Fname;
      DSS.ParserVars.Add('@lastfile', Fname);
End;

{$IFNDEF FPC}
// Function to validate the installation and path of the OpenDSS Viewer
function GetIni(s,k: string; d: string; f: string=''): string; overload;
var
  ini: TMemIniFile;
begin
  Result := d;
  if f = '' then
  begin
    ini := TMemIniFile.Create(lowercase(ChangeFileExt(ParamStr(0),'.ini')));
  end
  else
  begin
    if not FileExists(f) then Exit;
    ini := TMemIniFile.Create(f);
  end;
  if ini.ReadString(s,k,'') = '' then
  begin
    ini.WriteString(s,k,d);
    ini.UpdateFile;
  end;
  Result := ini.ReadString(s,k,d);
  FreeAndNil(ini);
end;

// Validates the installation and path of the OpenDSS Viewer
function CheckOpenDSSViewer: Boolean;
var FileName: string;
begin
  DSS_Viz_path:=GetIni('Application','path','', TPath.GetHomePath+'\OpenDSS_Viewer\settings.ini');
  // to make it compatible with the function
  FileName  :=  stringreplace(DSS_Viz_path, '\\' ,'\',[rfReplaceAll, rfIgnoreCase]);
  FileName  :=  stringreplace(FileName, '"' ,'',[rfReplaceAll, rfIgnoreCase]);
  // returns true only if the executable exists
  Result:=fileexists(FileName);
end;
{$ENDIF}


{$IFDEF DSS_CAPI_PM}
// Waits for all the actors running tasks
procedure Wait4Actors(DSS: TDSSContext; ActorOffset: Integer);
var
    i: Integer;
    Flag: Boolean;
    PMParent: TDSSContext;
    Child: TDSSContext;
begin
    PMParent := DSS.GetPrime();
    // ActorOffset defines the starting point in which the actors will be evaluated,
    // modification introduced in 01-10-2019 to facilitate the coordination
    // between actors when a simulation is performed using A-Diakoptics
    for i := ActorOffset to High(PMParent.Children) do
    begin
        try
            Child := PMParent.Children[i];
            if Child.ActorStatus = TActorStatus.Busy then
            begin
                Flag := True;
                while Flag do
                    Flag := (Child.ActorMA_Msg.WaitFor(10) = TWaitResult.wrTimeout);
            end;
        except
        on EOutOfMemory do
            Dosimplemsg(DSS, 'Exception Waiting for the parallel thread to finish a job"', 7006);
        end;
    end;
end;

// Clones the active Circuit as many times as requested if possible
procedure DoClone(DSS: TDSSContext);
var
    PMParent: TDSSContext;
    i,
    NumClones: Integer;
    Ref_Ckt: String;
Begin
    //TODO: DSS must DSSPrime here?
    PMParent := DSS.GetPrime();
    Ref_Ckt := DSS.LastFileCompiled;
    DSS.Parser.NextParam;
    NumClones := DSS.Parser.IntValue;
    PMParent.Parallel_enabled := False;
    if ((PMParent.NumOfActors + NumClones) <= CPU_Cores) and (NumClones > 0) then
    begin
        for i := 1 to NumClones do
        begin
            New_Actor_Slot(PMParent);
            PMParent.ActiveChild.DSSExecutive.Command := 'compile "' + Ref_Ckt + '"';
            // sets the previous maxiterations and controliterations
            PMParent.ActiveChild.ActiveCircuit.Solution.MaxIterations := DSS.ActiveCircuit.Solution.MaxIterations;
            PMParent.ActiveChild.ActiveCircuit.Solution.MaxControlIterations := DSS.ActiveCircuit.Solution.MaxControlIterations;
            // Solves the circuit
            DSS.CmdResult := ExecOptions.DoSetCmd(PMParent.ActiveChild, 1);
        end;
    end
    else
    begin
        if NumClones > 0 then
            DoSimpleMsg(DSS, 'There are no more CPUs available', 7001)
        else
            DoSimpleMsg(DSS, 'The number of clones requested is invalid', 7004)
    end;
end;

// Prepares memory to host a new actor
procedure New_Actor_Slot(DSS: TDSSContext);
var
    PMParent: TDSSContext;
begin
    PMParent := DSS.GetPrime();

    if (High(PMParent.Children) + 1) < CPU_Cores then
    begin
        SetLength(PMParent.Children, High(PMParent.Children) + 2);
        PMParent.ActiveChildIndex := High(PMParent.Children);
        PMParent.ActiveChild := TDSSContext.Create(PMParent);
        PMParent.Children[PMParent.ActiveChildIndex] := PMParent.ActiveChild;
        PMParent.ActiveChild._Name := '_' + inttostr(PMParent.ActiveChildIndex + 1);
        PMParent.ActiveChild.CPU := PMParent.ActiveChildIndex;
        DSS.GlobalResult := inttostr(PMParent.ActiveChildIndex + 1);
    end
    else 
        DoSimpleMsg(DSS, 'There are no more CPUs available', 7001)
End;

// Creates a new actor
procedure New_Actor(DSS: TDSSContext);
begin
    DSS.ActorThread := TSolver.Create(DSS, True, DSS.CPU, nil, DSS.ActorMA_Msg);
//    Child.ActorThread.Priority :=  tpTimeCritical;
    DSS.ActorThread.Start();
    DSS.ActorStatus := TActorStatus.Idle;
end;

{$ENDIF}


initialization
   {Various Constants and Switches}
   {$IFDEF FPC}NoFormsAllowed  := TRUE;{$ENDIF}

   CALPHA                := Cmplx(-0.5, -0.866025); // -120 degrees phase shift
   SQRT2                 := Sqrt(2.0);
   SQRT3                 := Sqrt(3.0);
   InvSQRT3              := 1.0/SQRT3;
   InvSQRT3x1000         := InvSQRT3 * 1000.0;


   {Initialize filenames and directories}

{$IFDEF DSS_CAPI}
   ProgramName      := 'DSS_CAPI';
    {$IFDEF FPC}
   ProgramName      := 'OpenDSSCmd';
    {$ELSE}
   ProgramName      := 'OpenDSS';
    {$ENDIF}
{$ENDIF}

   DSSFileName      := GetDSSExeFile;
   DSSDirectory     := ExtractFilePath(DSSFileName);
   // want to know if this was built for 64-bit, not whether running on 64 bits
   // (i.e. we could have a 32-bit build running on 64 bits; not interested in that
{$IFDEF DSS_CAPI}
    {$IFDEF CPUX64}
   VersionString    := GetDSSVersion + ' (64-bit build)';
    {$ELSE ! CPUX86}
   VersionString    := GetDSSVersion + ' (32-bit build)';
    {$ENDIF}
{$ELSE}
    {$IFDEF CPUX64}
   VersionString    := 'Version ' + GetDSSVersion + ' (64-bit build)';
    {$ELSE ! CPUX86}
   VersionString    := 'Version ' + GetDSSVersion + ' (32-bit build)';
    {$ENDIF}
{$ENDIF}

   StartupDirectory := GetCurrentDir + PathDelim;

{$IFNDEF DSS_CAPI}
   SetDataPath (DSS, GetDefaultDataDirectory + PathDelim + ProgramName + PathDelim); //TODO
   {$IFNDEF FPC}
   DSS_Registry     := TIniRegSave.Create('\Software\' + ProgramName);
   {$ELSE}
   DSS_Registry     := TIniRegSave.Create(DataDirectory + 'opendsscmd.ini');
   {$ENDIF}
   UpdateRegistry   := TRUE;
{$ELSE}
   IF SysUtils.GetEnvironmentVariable('DSS_BASE_FREQUENCY') <> '' THEN
   BEGIN
      GlobalDefaultBaseFreq  := StrToInt(SysUtils.GetEnvironmentVariable('DSS_BASE_FREQUENCY'));
   END;
{$ENDIF}

   {$IFDEF Darwin}
      DefaultEditor := SysUtils.GetEnvironmentVariable('EDITOR');
      // If there is no EDITOR environment variable, keep the old behavior
      if (DefaultEditor = '') then
          DefaultEditor   := 'open -t';
      DefaultFontSize := 12;
      DefaultFontName := 'Geneva';
   {$ENDIF}
   {$IFDEF Linux}
      DefaultEditor := SysUtils.GetEnvironmentVariable('EDITOR');
      // If there is no EDITOR environment variable, keep the old behavior
      if (DefaultEditor = '') then
          DefaultEditor := 'xdg-open';
      DefaultFontSize := 10;
      DefaultFontName := 'Arial';
   {$ENDIF}
   {$IF (defined(Windows) or defined(MSWindows))}
      DefaultEditor   := 'NotePad.exe';
      DefaultFontSize := 8;
      DefaultFontName := 'MS Sans Serif';
   {$ENDIF}

   {$IFNDEF FPC}NoFormsAllowed   := FALSE;{$ENDIF}

   {$IFNDEF MSWINDOWS}
   CPU_Freq := 1000; // until we can query it
   {$ELSE}
   QueryPerformanceFrequency(CPU_Freq);
   {$ENDIF}
   CPU_Cores        :=  CPUCount;
   MaxCircuits           := 1;  //  Not required anymore. planning to remove it

   //WriteDLLDebugFile('DSSGlobals');

   {$IFNDEF FPC}
   DSS_Viz_installed := CheckDSSVisualizationTool; // DSS visualization tool (flag of existance)
   {$ENDIF}
{$IFDEF DSS_CAPI}
   DSS_CAPI_INFO_SPARSE_COND := (SysUtils.GetEnvironmentVariable('DSS_CAPI_INFO_SPARSE_COND') = '1');

   // Default is True, disable at initialization only when DSS_CAPI_EARLY_ABORT = 0
   DSS_CAPI_EARLY_ABORT := (SysUtils.GetEnvironmentVariable('DSS_CAPI_EARLY_ABORT') <> '0');

   // Default is True, enable at initialization when DSS_CAPI_ALLOW_EDITOR = 0
   DSS_CAPI_ALLOW_EDITOR := (SysUtils.GetEnvironmentVariable('DSS_CAPI_ALLOW_EDITOR') <> '0');
{$ENDIF}

    ExecCommands.DefineCommands;

try
   DSSPrime := TDSSContext.Create(nil, True);
except 
    on E: Exception do
    begin
        DSSPrime := nil;
    end;
end;

Finalization

    if DSSPrime <> nil then
    begin
        DSSPrime.Free;  {Writes to Registry}
        DSSPrime := nil;
    end;
        
{$IFNDEF DSS_CAPI}
    DSS_Registry.Free;  {Close Registry}
{$ENDIF}

    ExecCommands.DisposeStrings;
End.
