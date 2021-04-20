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
     UComplex, Arraydef, CktElement, Circuit, IniRegSave,

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
     WireData,
     CNData,
     TSData,
     LineSpacing,
     Storage,
     Storage2,
     PVSystem,
     PVSystem2,
     InvControl,
     InvControl2,
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
     StorageController2,
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
     VSConverter;


CONST
      CRLF = sLineBreak;

      PI =  3.14159265359;

      TwoPi = 2.0 * PI;

      RadiansToDegrees = 57.29577951;

      EPSILON = 1.0e-12;   // Default tiny floating point
      EPSILON2 = 1.0e-3;   // Default for Real number mismatch testing

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

   ProgramName    :String;
   DSS_CAPI_INFO_SPARSE_COND : Boolean;
   DSS_CAPI_EARLY_ABORT : Boolean;
   DSS_CAPI_ALLOW_EDITOR: Boolean;
   DSS_CAPI_LOADS_TERMINAL_CHECK: Boolean = True;
   DSS_CAPI_ITERATE_DISABLED: Integer = 0; // default to 0 for compatibility
   DSS_CAPI_EXT_ERRORS: Boolean = True;
   DSS_CAPI_LEGACY_MODELS: Boolean = False;
   DSS_CAPI_LEGACY_MODELS_PREV: Boolean = False;
   DSS_CAPI_ALLOW_CHANGE_DIR : Boolean = True;

   // Global variables for the DSS visualization tool
   DSS_Viz_installed   :Boolean=False; // DSS visualization tool (flag of existance)
   DSS_Viz_path: String;
   DSS_Viz_enable: Boolean=False;

   IsDLL,
   NoFormsAllowed  :Boolean;

   ActiveCircuit   :TDSSCircuit;
   ActiveDSSClass  :TDSSClass;
   LastClassReferenced:Integer;  // index of class of last thing edited
   ActiveDSSObject :TDSSObject;
   NumCircuits     :Integer;
   MaxCircuits     :Integer;
   MaxBusLimit     :Integer; // Set in Validation
   MaxAllocationIterations :Integer;
   Circuits        :TPointerList;
   DSSObjs         :TPointerList;

   AuxParser       :TParser;  // Auxiliary parser for use by anybody for reparsing values

   ErrorPending       :Boolean;
   CmdResult,
   ErrorNumber        :Integer;
   LastErrorMessage   :String;

   DefaultEarthModel  :Integer;
   ActiveEarthModel   :Integer;

   LastFileCompiled   :String;
   LastCommandWasCompile :Boolean;

   CALPHA             :Complex;  {120-degree shift constant}
   SQRT2              :Double;
   SQRT3              :Double;
   InvSQRT3           :Double;
   InvSQRT3x1000      :Double;
   SolutionAbort      :Boolean;
   InShowResults      :Boolean;
   Redirect_Abort     :Boolean;
   In_Redirect        :Boolean;
   DIFilesAreOpen     :Boolean;
   AutoShowExport     :Boolean;
   SolutionWasAttempted :Boolean;

   GlobalHelpString   :String;
   GlobalPropertyValue:String;
   GlobalResult       :String;
   LastResultFile     :String;
   VersionString      :String;

   LogQueries         :Boolean;
   QueryFirstTime     :Boolean;
   QueryLogFileName   :String;
   QueryLogFile       :TFileStream = nil;

   DefaultEditor    :String;     // normally, Notepad
   DefaultFontSize  :Integer;
   DefaultFontName  :String;
   DefaultFontStyles :Integer;
   DSSFileName      :String;     // Name of current exe or DLL
   DSSDirectory     :String;     // where the current exe resides
   StartupDirectory :String;     // Where we started
   DataDirectory    :String;     // used to be DSSDataDirectory
   OutputDirectory  :String;     // output files go here, same as DataDirectory if writable
   CircuitName_     :String;     // Name of Circuit with a "_" appended

   DefaultBaseFreq  :Double;
   DaisySize        :Double;

   // Some commonly used classes   so we can find them easily
   LoadShapeClass     :TLoadShape;
   TShapeClass        :TTshape;
   PriceShapeClass    :TPriceShape;
   XYCurveClass       :TXYCurve;
   GrowthShapeClass   :TGrowthShape;
   SpectrumClass      :TSpectrum;
   SolutionClass      :TDSSClass;
   EnergyMeterClass   :TEnergyMeter;
   MonitorClass       :TDSSMonitor;
   SensorClass        :TSensor;
   TCC_CurveClass     :TTCC_Curve;
   WireDataClass      :TWireData;
   CNDataClass        :TCNData;
   TSDataClass        :TTSData;
   LineGeometryClass  :TLineGeometry;
   LineSpacingClass   :TLineSpacing;
   LineCodeClass      :TLineCode;
   StorageClass       :TStorage;
   Storage2Class      :TStorage2;
   PVSystemClass      :TPVSystem;
   PVSystem2Class     :TPVSystem2;
   InvControlClass    :TInvControl;
   InvControl2Class   :TInvControl2;
   ExpControlClass    :TExpControl;

   LineClass          :TLine;
   VSourceClass       :TVSource;
   ISourceClass       :TISource;
   VCSSClass          :TVCCS;
   LoadClass          :TLoad;
   TransformerClass   :TTransf;
   RegControlClass    :TRegControl;
   CapacitorClass     :TCapacitor;
   ReactorClass       :TReactor;
   CapControlClass    :TCapControl;
   FaultClass         :TFault;
   GeneratorClass     :TGenerator;
   GenDispatcherClass :TGenDispatcher;
   StorageControllerClass: TStorageController;
   StorageController2Class: TStorageController2;
   RelayClass         :TRelay;
   RecloserClass      :TRecloser;
   FuseClass          :TFuse;
   SwtControlClass    :TSwtControl;
   UPFCClass          :TUPFC;
   UPFCControlClass   :TUPFCControl;
   ESPVLControlClass  :TESPVLControl;
   IndMach012Class    :TIndMach012;
   GICsourceClass     :TGICsource; // GIC source
   AutoTransClass     :TAutoTrans; // Auto Transformer
   VSConverterClass   :TVSConverter;

   EventStrings: TStringList;
   SavedFileList:TStringList;
   ErrorStrings: TStringList;

   DSSClassList       :TPointerList; // pointers to the base class types
   ClassNames         :THashList;

   UpdateRegistry     :Boolean;  // update on program exit
   CPU_Freq           : int64;          // Used to store the CPU frequency
   CPU_Cores          : integer;

   IncMat_Ordered     : Boolean;
//***********************A-Diakoptics Variables*********************************
  ADiakoptics             : Boolean;

//***********************Seasonal QSTS variables********************************
   SeasonalRating         : Boolean;    // Tells the energy meter if the seasonal rating feature is active
   SeasonSignal           : String;     // Stores the name of the signal for selecting the rating dynamically


PROCEDURE DoErrorMsg(Const S, Emsg, ProbCause :String; ErrNum:Integer);
PROCEDURE DoSimpleMsg(Const S :String; ErrNum:Integer);

PROCEDURE ClearAllCircuits;

PROCEDURE SetObject(const param :string);
FUNCTION  SetActiveBus(const BusName:String):Integer;
PROCEDURE SetDataPath(const PathName:String);

PROCEDURE SetLastResultFile(Const Fname:String);

PROCEDURE MakeNewCircuit(Const Name:String);

PROCEDURE AppendGlobalResult(Const s:String);
PROCEDURE AppendGlobalResultCRLF(const S:String);  // Separate by CRLF

PROCEDURE ResetQueryLogFile;
PROCEDURE WriteQueryLogFile(Const Prop, S:String);

FUNCTION IsDSSDLL(Fname:String):Boolean;

Function GetOutputDirectory:String;

function CurrentDSSDir(): String;
procedure SetCurrentDSSDir(dir: String);

implementation



USES  {Forms,   Controls,}
     {$IFDEF MSWINDOWS}
     Windows,
     // SHFolder,
     {$ENDIF}
     SysUtils,
     CAPI_Metadata,
     CmdForms,
     Solution,
     Executive,
     Utilities;
     {Intrinsic Ckt Elements}

TYPE

   THandle = NativeUint;

   TDSSRegister = function(var ClassName: pchar):Integer;  // Returns base class 1 or 2 are defined
   // Users can only define circuit elements at present

VAR

   LastUserDLLHandle: THandle;
   DSSRegisterProc:TDSSRegister;   // of last library loaded
   CurrentDSSDir_internal: String = '';

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

function GetOutputDirectory:String;
begin
  Result := OutputDirectory;
end;

{--------------------------------------------------------------}
FUNCTION IsDSSDLL(Fname:String):Boolean;

Begin
    Result := FALSE;

    // Ignore if "DSSLIB.DLL"
    If CompareText(ExtractFileName(Fname),'dsslib.dll')=0 Then Exit;

   LastUserDLLHandle := LoadLibrary(pchar(Fname));
   IF LastUserDLLHandle <> 0 then BEGIN

   // Assign the address of the DSSRegister proc to DSSRegisterProc variable
    @DSSRegisterProc := GetProcAddress(LastUserDLLHandle, 'DSSRegister');
    IF @DSSRegisterProc <> nil THEN Result := TRUE
    ELSE FreeLibrary(LastUserDLLHandle);

  END;

End;

//----------------------------------------------------------------------------
PROCEDURE DoErrorMsg(Const S, Emsg, ProbCause:String; ErrNum:Integer);

VAR
    Msg:String;
    Retval:Integer;
Begin

     Msg := Format('Error %d Reported From OpenDSS Intrinsic Function: ', [Errnum])+ CRLF  + S
             + CRLF   + CRLF + 'Error Description: ' + CRLF + Emsg
             + CRLF   + CRLF + 'Probable Cause: ' + CRLF+ ProbCause;

     If Not NoFormsAllowed Then Begin

         If In_Redirect Then
         Begin
           RetVal := DSSMessageDlg(Msg, FALSE);
           If RetVal = -1 Then Redirect_Abort := True;
         End
         Else
           DSSMessageDlg(Msg, TRUE);

     End
     Else
     Begin
        {$IFDEF DSS_CAPI}
        if DSS_CAPI_EARLY_ABORT then
            Redirect_Abort := True;
        {$ENDIF}
     End;

     LastErrorMessage := Msg;
     ErrorNumber := ErrNum;
     AppendGlobalResultCRLF(Msg);
     SolutionAbort  :=  True;
End;

//----------------------------------------------------------------------------
PROCEDURE AppendGlobalResultCRLF(const S:String);

Begin
    If Length(GlobalResult) > 0
    THEN GlobalResult := GlobalResult + CRLF + S
    ELSE GlobalResult := S;

    ErrorStrings.Add(Format('(%d) %s' ,[ErrorNumber, S]));  // Add to Error log
End;

//----------------------------------------------------------------------------
PROCEDURE DoSimpleMsg(Const S:String; ErrNum:Integer);

VAR
    Retval:Integer;
Begin
    IF Not NoFormsAllowed Then Begin
        IF In_Redirect THEN
        Begin
            RetVal := DSSMessageDlg(Format('(%d) OpenDSS %s%s', [Errnum, CRLF, S]), FALSE);
            {$IFDEF DSS_CAPI}
            if DSS_CAPI_EARLY_ABORT then
                Redirect_Abort := True;
            {$ENDIF}
            IF RetVal = -1 THEN
                Redirect_Abort := True;
        End
        ELSE
            DSSInfoMessageDlg(Format('(%d) OpenDSS %s%s', [Errnum, CRLF, S]));
    End
    Else
    Begin
        {$IFDEF DSS_CAPI}
        if DSS_CAPI_EARLY_ABORT then
            Redirect_Abort := True;
        {$ENDIF}
    End;

    LastErrorMessage := S;
    ErrorNumber := ErrNum;
    AppendGlobalResultCRLF(S);
End;


//----------------------------------------------------------------------------
PROCEDURE SetObject(const param :string);

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

      IF Length(ObjClass) > 0 THEN SetObjectClass(ObjClass);

      ActiveDSSClass := DSSClassList.Get(LastClassReferenced);
      IF ActiveDSSClass <> Nil THEN
      Begin
        IF Not ActiveDSSClass.SetActive(Objname) THEN
        Begin // scroll through list of objects untill a match
          DoSimpleMsg('Error! Object "' + ObjName + '" not found.'+ CRLF + parser.CmdString, 904);
        End
        ELSE
        With ActiveCircuit Do
        Begin
           CASE ActiveDSSObject.DSSObjType OF
                DSS_OBJECT: ;  // do nothing for general DSS object

           ELSE Begin   // for circuit types, set ActiveCircuit Element, too
                 ActiveCktElement := ActiveDSSClass.GetActiveObj;
                End;
           End;
        End;
      End
      ELSE
        DoSimpleMsg('Error! Active object type/class is not set.', 905);

End;

//----------------------------------------------------------------------------
FUNCTION SetActiveBus(const BusName:String):Integer;


Begin

   // Now find the bus and set active
   Result := 0;

   WITH ActiveCircuit Do
     Begin
        If BusList.ListSize=0 Then Exit;   // Buslist not yet built
        ActiveBusIndex := BusList.Find(BusName);
        IF   ActiveBusIndex=0 Then
          Begin
            Result := 1;
            AppendGlobalResult('SetActiveBus: Bus ' + BusName + ' Not Found.');
          End;
     End;

End;

PROCEDURE ClearAllCircuits;

Begin

    ActiveCircuit := Circuits.First;
     WHILE ActiveCircuit<>nil DO
     Begin
        ActiveCircuit.Free;
        ActiveCircuit := Circuits.Next;
     End;
    Circuits.Free;
    Circuits := TPointerList.Create(2);   // Make a new list of circuits
    NumCircuits := 0;

    // Revert on key global flags to Original States
    DefaultEarthModel     := DERI;
    LogQueries            := FALSE;
    MaxAllocationIterations := 2;

End;



PROCEDURE MakeNewCircuit(Const Name:String);

//Var
//   handle :Integer;
Var
    S:String;

Begin


     If NumCircuits <= MaxCircuits - 1 Then
     Begin
         ActiveCircuit := TDSSCircuit.Create(Name);
         ActiveDSSObject := ActiveSolutionObj;
         {*Handle := *} Circuits.Add(ActiveCircuit);
         Inc(NumCircuits);
         S := Parser.Remainder;    // Pass remainder of string on to vsource.
         {Create a default Circuit}
         SolutionABort := FALSE;
         {Voltage source named "source" connected to SourceBus}
         DSSExecutive.Command := 'New object=vsource.source Bus1=SourceBus ' + S;  // Load up the parser as if it were read in
     End
     Else
     Begin
         DoErrorMsg('MakeNewCircuit',
                    'Cannot create new circuit.',
                    'Max. Circuits Exceeded.'+CRLF+
                    '(Max no. of circuits='+inttostr(Maxcircuits)+')', 906);
     End;
End;


PROCEDURE AppendGlobalResult(Const S:String);

// Append a string to Global result, separated by commas

Begin
    If Length(GlobalResult)=0 Then
        GlobalResult := S
    Else
        GlobalResult := GlobalResult + ', ' + S;
End;

FUNCTION GetDSSVersion: String;
BEGIN
    Result := 'DSS C-API Library version ' + DSS_CAPI_VERSION +
              ' revision ' + DSS_CAPI_REV +
              ' based on OpenDSS SVN ' + DSS_CAPI_SVN_REV +
              ' (v7/classic variation)'
    {$IFDEF DSS_CAPI_MVMULT}
              + ' MVMULT'
    {$ENDIF}
              + ' [FPC ' + {$include %FPCVersion%} + ']'
    {$IFDEF DSS_CAPI_DEBUG_BUILD}
              + ' DEBUG'
    {$ENDIF}
              ;
END;


{$IFNDEF UNIX}
function IsDirectoryWritable(const Dir: String): Boolean;
var
  TempFile: array[0..MAX_PATH] of Char;
begin
  if GetTempFileName(PChar(Dir), 'DA', 0, TempFile) <> 0 then
    Result := DeleteFile(TempFile)
  else
    Result := False;
end;
{$ELSE}
function IsDirectoryWritable(const Dir: String): Boolean;
begin
  Result := (FpAccess(PChar(Dir), X_OK or W_OK) = 0);
end;
{$ENDIF}

PROCEDURE SetDataPath(const PathName:String);
var
  ScratchPath: String;
// Pathname may be null
BEGIN
  if (Length(PathName) > 0) and not DirectoryExists(PathName) then Begin
  // Try to create the directory
    if not CreateDir(PathName) then Begin
      DosimpleMsg('Cannot create ' + PathName + ' directory.', 907);
      Exit;
    End;
  End;

  DataDirectory := PathName;

  // Put a \ on the end if not supplied. Allow a null specification.
  If Length(DataDirectory) > 0 Then Begin
    SetCurrentDSSDir(DataDirectory);   // Change to specified directory
    If DataDirectory[Length(DataDirectory)] <> PathDelim Then DataDirectory := DataDirectory + PathDelim;
  End;

  // see if DataDirectory is writable. If not, set OutputDirectory to the user's appdata
  if IsDirectoryWritable(DataDirectory) then begin
    OutputDirectory := DataDirectory;
  end else begin
    ScratchPath := GetDefaultScratchDirectory + PathDelim + ProgramName + PathDelim;
    if not DirectoryExists(ScratchPath) then CreateDir(ScratchPath);
    OutputDirectory := ScratchPath;
  end;
END;

PROCEDURE ResetQueryLogFile;
Begin
     QueryFirstTime := TRUE;
End;


PROCEDURE WriteQueryLogfile(Const Prop, S:String);
{Log file is written after a query command if LogQueries is true.}
Begin
    TRY
        QueryLogFileName :=  OutputDirectory + 'QueryLog.CSV';
        If QueryFirstTime then
        Begin
            QueryLogFile := TFileStream.Create(QueryLogFileName, fmCreate);
            FSWriteln(QueryLogFile, 'Time(h), Property, Result');
            QueryFirstTime := False;
        end
        Else 
        begin
            QueryLogFile := TFileStream.Create(QueryLogFileName, fmOpenReadWrite);
            QueryLogFile.Seek(0, soEnd);
        end;

        FSWriteln(QueryLogFile,Format('%.10g, %s, %s',[ActiveCircuit.Solution.DynaVars.dblHour, Prop, S]));
        FreeAndNil(QueryLogFile);
    EXCEPT
        On E:Exception Do DoSimpleMsg('Error writing Query Log file: ' + E.Message, 908);
    END;
End;

PROCEDURE SetLastResultFile(Const Fname:String);

Begin
      LastResultfile := Fname;
      ParserVars.Add('@lastfile', Fname);
End;

function CurrentDSSDir(): String;
begin
    if DSS_CAPI_ALLOW_CHANGE_DIR then
    begin
        Result := GetCurrentDir();
        If Result[Length(Result)] <> PathDelim Then 
            Result := Result + PathDelim;
    end
    else
    begin
        Result := CurrentDSSDir_internal
    end;
end;

procedure SetCurrentDSSDir(dir: String);
begin
    if DSS_CAPI_ALLOW_CHANGE_DIR then
    begin
        SetCurrentDir(dir);
        Exit;
    end;

    If dir[Length(dir)] <> PathDelim Then 
        CurrentDSSDir_internal := dir + PathDelim
    else
        CurrentDSSDir_internal := dir;
end;



initialization

   ADiakoptics      :=    False;  // Disabled by default

   SeasonalRating         :=  False;
   SeasonSignal           :=  '';

   {Various Constants and Switches}
   NoFormsAllowed  := TRUE;

   CALPHA                := Cmplx(-0.5, -0.866025); // -120 degrees phase shift
   SQRT2                 := Sqrt(2.0);
   SQRT3                 := Sqrt(3.0);
   InvSQRT3              := 1.0/SQRT3;
   InvSQRT3x1000         := InvSQRT3 * 1000.0;
   CmdResult             := 0;
   DIFilesAreOpen        := FALSE;
   ErrorNumber           := 0;
   ErrorPending          := FALSE;
   GlobalHelpString      := '';
   GlobalPropertyValue   := '';
   LastResultFile        := '';
   In_Redirect           := FALSE;
   InShowResults         := FALSE;
   IsDLL                 := FALSE;
   LastCommandWasCompile := FALSE;
   LastErrorMessage      := '';
   MaxCircuits           := 1;  //  Not required anymore. planning to remove it
   MaxAllocationIterations := 2;
   SolutionAbort         := FALSE;
   AutoShowExport        := FALSE;
   SolutionWasAttempted  := FALSE;

   DefaultBaseFreq       := 60.0;
   DaisySize             := 1.0;
   DefaultEarthModel     := DERI;
   ActiveEarthModel      := DefaultEarthModel;

   ErrorStrings     := TStringList.Create;
   ErrorStrings.Clear;

   {Initialize filenames and directories}

   ProgramName      := 'dss-extensions';
   DSSFileName      := GetDSSExeFile;
   DSSDirectory     := ExtractFilePath(DSSFileName);
   // want to know if this was built for 64-bit, not whether running on 64 bits
   // (i.e. we could have a 32-bit build running on 64 bits; not interested in that

{$IFDEF CPUX64}
   VersionString    := GetDSSVersion + ' (64-bit build)';
{$ELSE ! CPUX86}
   VersionString    := GetDSSVersion + ' (32-bit build)';
{$ENDIF}


   StartupDirectory := GetCurrentDir + PathDelim;
   SetDataPath (StartupDirectory);

   IF GetEnvironmentVariable('DSS_BASE_FREQUENCY') <> '' THEN
   BEGIN
      DefaultBaseFreq  := StrToInt(GetEnvironmentVariable('DSS_BASE_FREQUENCY'));
   END;

   AuxParser        := TParser.Create;

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

   EventStrings     := TStringList.Create;
   SavedFileList    := TStringList.Create;


   LogQueries       := FALSE;
   QueryLogFileName := '';
   UpdateRegistry   := TRUE;
   {$IFNDEF MSWINDOWS}
   CPU_Freq := 1000; // until we can query it
   {$ELSE}
   QueryPerformanceFrequency(CPU_Freq);
   {$ENDIF}
   CPU_Cores        :=  CPUCount;

   DSS_CAPI_INFO_SPARSE_COND := (GetEnvironmentVariable('DSS_CAPI_INFO_SPARSE_COND') = '1');

   // Default is True, disable at initialization only when DSS_CAPI_EARLY_ABORT = 0
   DSS_CAPI_EARLY_ABORT := (GetEnvironmentVariable('DSS_CAPI_EARLY_ABORT') <> '0');

   // Default is True, disable at initialization when DSS_CAPI_ALLOW_EDITOR = 0
   DSS_CAPI_ALLOW_EDITOR := (GetEnvironmentVariable('DSS_CAPI_ALLOW_EDITOR') <> '0');
   DSS_CAPI_EXT_ERRORS := (GetEnvironmentVariable('DSS_CAPI_EXT_ERRORS') <> '0');
   
   // Default is False, enable at initialization when DSS_CAPI_LEGACY_MODELS = 1
   DSS_CAPI_LEGACY_MODELS := (GetEnvironmentVariable('DSS_CAPI_LEGACY_MODELS') = '1');
   DSS_CAPI_LEGACY_MODELS := DSS_CAPI_LEGACY_MODELS_PREV;
   
   // For the 0.10.x branch, default is True, disable at initialization when DSS_CAPI_ALLOW_CHANGE_DIR = 0
   DSS_CAPI_ALLOW_CHANGE_DIR := (SysUtils.GetEnvironmentVariable('DSS_CAPI_ALLOW_CHANGE_DIR') <> '0');


Finalization

  // Dosimplemsg('Enter DSSGlobals Unit Finalization.');
  Auxparser.Free;

  EventStrings.Free;
  SavedFileList.Free;
  ErrorStrings.Free;

  With DSSExecutive Do If RecorderOn Then Recorderon := FALSE;

  DSSExecutive.Free;

End.


