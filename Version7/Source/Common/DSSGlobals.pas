unit DSSGlobals;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2019, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}


{ Change Log
 8-14-99  SolutionAbort Added

 10-12-99 AutoAdd constants added;
 4-17-00  Added IsShuntCapacitor routine, Updated constants
 10-08-02 Moved Control Panel Instantiation and show to here
 11-6-02  Removed load user DLL because it was causing a conflict
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
    DSS_CAPI_ALLOW_INCREMENTAL_Y: Boolean = False;
{$ENDIF}
    // Global variables for the DSS visualization tool
    DSS_Viz_installed   :Boolean=False; // DSS visualization tool (flag of existance)
    DSS_Viz_path: String;
    DSS_Viz_enable: Boolean=False;

    IsDLL,
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

PROCEDURE DoErrorMsg(DSS: TDSS; Const S, Emsg, ProbCause :String; ErrNum:Integer);
PROCEDURE DoSimpleMsg(DSS: TDSS; Const S :String; ErrNum:Integer);

PROCEDURE ClearAllCircuits(DSS: TDSS);

PROCEDURE SetObject(DSS: TDSS; const param :string);
FUNCTION  SetActiveBus(DSS: TDSS; const BusName:String):Integer;
PROCEDURE SetDataPath(DSS: TDSS; const PathName:String);

PROCEDURE SetLastResultFile(DSS: TDSS; Const Fname:String);

PROCEDURE MakeNewCircuit(DSS: TDSS; Const Name:String);

PROCEDURE AppendGlobalResult(DSS: TDSS; Const s:String);
PROCEDURE AppendGlobalResultCRLF(DSS: TDSS; const S:String);  // Separate by CRLF

PROCEDURE ResetQueryLogFile(DSS: TDSS);
PROCEDURE WriteQueryLogFile(DSS: TDSS; Const Prop, S:String);

PROCEDURE WriteDLLDebugFile(DSS: TDSS; Const S:String);

{$IFNDEF DSS_CAPI} // Disable DSS_Registry completely when building the DSS_CAPI DLL
PROCEDURE ReadDSS_Registry(DSS: TDSS);
PROCEDURE WriteDSS_Registry(DSS: TDSS);
{$ENDIF}

// FUNCTION IsDSSDLL(Fname:String):Boolean;

// Procedure MyReallocMem(Var p:Pointer; newsize:integer);
// Function MyAllocMem(nbytes:Cardinal):Pointer;



implementation



USES  {Forms,   Controls,}
     {$IFDEF MSWINDOWS}
     Windows,
     {$ENDIF}
     SysUtils,
     {$IFDEF DSS_CAPI}
     CAPI_Metadata,
     {$ENDIF}
     {$IFDEF FPC}
     resource, versiontypes, versionresource, dynlibs, CmdForms,
     {$ELSE}
     DSSForms, SHFolder,
     {$ENDIF}
     Solution,
     Executive,
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
  Result := GetEnvironmentVariable('HOME') + PathDelim + 'Documents';
{$ENDIF}
{$IF (defined(Windows) or defined(MSWindows))}
  Result := GetEnvironmentVariable('HOMEDRIVE') + GetEnvironmentVariable('HOMEPATH') + PathDelim + 'Documents';
{$ENDIF}
end;

FUNCTION GetDefaultScratchDirectory: String;
Begin
  {$IFDEF UNIX}
  Result := '/tmp';
  {$ENDIF}
  {$IF (defined(Windows) or defined(MSWindows))}
  Result := GetEnvironmentVariable('LOCALAPPDATA');
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
PROCEDURE DoErrorMsg(DSS: TDSS; Const S, Emsg, ProbCause:String; ErrNum:Integer);

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
PROCEDURE AppendGlobalResultCRLF(DSS: TDSS; const S:String);

Begin
    If Length(DSS.GlobalResult) > 0
    THEN DSS.GlobalResult := DSS.GlobalResult + CRLF + S
    ELSE DSS.GlobalResult := S;

    DSS.ErrorStrings.Add(Format('(%d) %s' ,[DSS.ErrorNumber, S]));  // Add to Error log
End;

//----------------------------------------------------------------------------
PROCEDURE DoSimpleMsg(DSS: TDSS; Const S:String; ErrNum:Integer);

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
PROCEDURE SetObject(DSS: TDSS; const param :string);

{Set object active by name}

VAR
   dotpos :Integer;
   ObjName, ObjClass :String;

Begin
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
FUNCTION SetActiveBus(DSS: TDSS; const BusName:String):Integer;


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

PROCEDURE ClearAllCircuits(DSS: TDSS);
Begin
     DSS.ActiveCircuit := DSS.Circuits.First;
     WHILE DSS.ActiveCircuit<>nil DO
     Begin
        DSS.ActiveCircuit.Free;
        DSS.ActiveCircuit := DSS.Circuits.Next;
     End;
    DSS.Circuits.Free;
    DSS.Circuits := TPointerList.Create(2);   // Make a new list of circuits
    DSS.NumCircuits := 0;

    // Revert on key global flags to Original States
    DSS.DefaultEarthModel     := DERI;
    DSS.LogQueries            := FALSE;
    DSS.MaxAllocationIterations := 2;
End;



PROCEDURE MakeNewCircuit(DSS: TDSS; Const Name:String);
Var
    S:String;
Begin
     If DSS.NumCircuits <= MaxCircuits - 1 Then
     Begin
         DSS.ActiveCircuit := TDSSCircuit.Create(DSS, Name);
         DSS.ActiveDSSObject := DSS.ActiveSolutionObj;
         {*Handle := *} DSS.Circuits.Add(DSS.ActiveCircuit);
         Inc(DSS.NumCircuits);
         S := DSS.Parser.Remainder;    // Pass remainder of string on to vsource.
         {Create a default Circuit}
         DSS.SolutionABort := FALSE;
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


PROCEDURE AppendGlobalResult(DSS: TDSS; Const S:String);

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

PROCEDURE WriteDLLDebugFile(DSS: TDSS; Const S:String);

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

PROCEDURE SetDataPath(DSS: TDSS; const PathName:String);
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
PROCEDURE ReadDSS_Registry(DSS: TDSS);
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


PROCEDURE WriteDSS_Registry(DSS: TDSS);
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

PROCEDURE ResetQueryLogFile(DSS: TDSS);
Begin
     DSS.QueryFirstTime := TRUE;
End;


PROCEDURE WriteQueryLogfile(DSS: TDSS; Const Prop, S:String);

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

PROCEDURE SetLastResultFile(DSS: TDSS; Const Fname:String);

Begin
      DSS.LastResultfile := Fname;
      DSS.ParserVars.Add('@lastfile', Fname);
End;

// Function MyAllocMem(nbytes:Cardinal):Pointer;
// Begin
//     Result := AllocMem(Nbytes);
//     WriteDLLDebugFile(Format('Allocating %d bytes @ %p',[nbytes, Result]));
// End;
// 
// Procedure MyReallocMem(Var p:Pointer; newsize:Integer);
// Begin
//      WriteDLLDebugFile(Format('Reallocating @ %p, new size= %d', [p, newsize]));
//      ReallocMem(p, newsize);
// End;

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
   IF GetEnvironmentVariable('DSS_BASE_FREQUENCY') <> '' THEN
   BEGIN
      GlobalDefaultBaseFreq  := StrToInt(GetEnvironmentVariable('DSS_BASE_FREQUENCY'));
   END;
{$ENDIF}

   {$IFDEF Darwin}
      DefaultEditor := GetEnvironmentVariable('EDITOR');
      // If there is no EDITOR environment variable, keep the old behavior
      if (DefaultEditor = '') then
          DefaultEditor   := 'open -t';
      DefaultFontSize := 12;
      DefaultFontName := 'Geneva';
   {$ENDIF}
   {$IFDEF Linux}
      DefaultEditor := GetEnvironmentVariable('EDITOR');
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
   IsDLL                 := FALSE;
   MaxCircuits           := 1;  //  Not required anymore. planning to remove it

   //WriteDLLDebugFile('DSSGlobals');

   {$IFNDEF FPC}
   DSS_Viz_installed := CheckDSSVisualizationTool; // DSS visualization tool (flag of existance)
   {$ENDIF}
{$IFDEF DSS_CAPI}
   IsDLL := True;
   DSS_CAPI_INFO_SPARSE_COND := (GetEnvironmentVariable('DSS_CAPI_INFO_SPARSE_COND') = '1');

   // Default is True, disable at initialization only when DSS_CAPI_EARLY_ABORT = 0
   DSS_CAPI_EARLY_ABORT := (GetEnvironmentVariable('DSS_CAPI_EARLY_ABORT') <> '0');

   // Default is True, enable at initialization when DSS_CAPI_ALLOW_EDITOR = 0
   DSS_CAPI_ALLOW_EDITOR := (GetEnvironmentVariable('DSS_CAPI_ALLOW_EDITOR') <> '0');
   
   // Default is False, enable at initialization when DSS_CAPI_ALLOW_INCREMENTAL_Y = 1
   DSS_CAPI_ALLOW_INCREMENTAL_Y := (GetEnvironmentVariable('DSS_CAPI_ALLOW_INCREMENTAL_Y') = '1');
   
   if (DSS_CAPI_ALLOW_INCREMENTAL_Y) then WriteLn('DSS_CAPI_ALLOW_INCREMENTAL_Y');
{$ENDIF}

  DSSPrime := TDSS.Create(True);


Finalization

  DSSPrime.Free;  {Writes to Registry}
{$IFNDEF DSS_CAPI}
  DSS_Registry.Free;  {Close Registry}
{$ENDIF}

End.
