unit DSSGlobals;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2019, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

Uses Classes, DSSClassDefs, DSSObject, DSSClass, ParserDel, Hashlist, DSSPointerList,
     UComplex, DSSUcomplex, Arraydef, CktElement, Circuit,

     {$IFDEF UNIX}BaseUnix, {$ENDIF}

     gettext,
     CpuCount,

     // Some units which have global vars defined here
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
     VSConverter,
     XfmrCode,
     GICLine,
     GICTransformer;


CONST
    CRLF = sLineBreak;
    IsDLL = True;

    // TODO: CALPHA has exceptionally bad precision here... change for v0.13
    CALPHA: Complex = (re:-0.5; im: -0.866025); // -120 degrees phase shift

    // TODO: toggle for v0.13
    // SQRT2 = 1.4142135623730950488;
    // SQRT3 = 1.7320508075688772935;
    // InvSQRT3 = 0.57735026918962576451;
    // InvSQRT3x1000 = 577.35026918962576450914;
    // PI =  3.1415926535897932385;
    // TwoPi = 2.0 * PI;
    // RadiansToDegrees = 57.2957795130823208768;
    TwoPi = 2.0 * PI;
    RadiansToDegrees = 180.0 / PI;
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
    
    ProgramName = 'dss-extensions';
    MaxCircuits = 2; //TODO: remove limit? or completely remove the concept of a separate circuit, i.e., make it so a DSSContext always contains one circuit

     
VAR
    DSSMessages: TMOFile = NIL;
    DSSPropertyHelp: TMOFile = NIL;
    DSS_CAPI_INFO_SPARSE_COND: Boolean;
    DSS_CAPI_EARLY_ABORT: Boolean;
    DSS_CAPI_ITERATE_DISABLED: Integer = 0; // default to 0 for compatibility
    DSS_CAPI_EXT_ERRORS: Boolean = True;
    DSS_CAPI_LEGACY_MODELS_PREV: Boolean = False;
    DSS_CAPI_ALLOW_CHANGE_DIR: Boolean = True;
    DSS_CAPI_COM_DEFAULTS: Boolean = True;
    GlobalDefaultBaseFreq: Double = 60.0;
    CPU_Freq           : int64;   // Used to store the CPU performance counter frequency (not the actual CPU frequency)
    CPU_Cores          : integer;


    DSS_CAPI_ALLOW_EDITOR: Boolean; //TODO: one per context?
    DSS_CAPI_LOADS_TERMINAL_CHECK: Boolean = True; //TODO: one per context?
    DSS_CAPI_LEGACY_MODELS: Boolean = False; //TODO: one per context?
    NoFormsAllowed: Boolean = True; //TODO: one per context?
    
    SQRT2: Double;
    SQRT3: Double;
    InvSQRT3: Double;
    InvSQRT3x1000: Double;    
    DefaultEditor: String;
    DefaultFontSize: Integer;
    DefaultFontName: String;
    DefaultFontStyles: Integer;
    DSSDirectory: String; // where the current exe resides
    StartupDirectory :String; // Where we started

function VersionString: String;
procedure DoErrorMsg(DSS: TDSSContext; Const S, Emsg, ProbCause :String; ErrNum:Integer);
procedure DoSimpleMsg(DSS: TDSSContext; Const S :String; ErrNum:Integer);overload;
procedure DoSimpleMsg(DSS: TDSSContext; Const S :String; fmtArgs: Array of Const; ErrNum:Integer);overload;

procedure ClearAllCircuits_SingleContext(DSS: TDSSContext);
{$IFDEF DSS_CAPI_PM}
procedure ClearAllCircuits_AllContexts(DSS: TDSSContext);
{$ENDIF}

procedure SetObject(DSS: TDSSContext; const param :string);
function  SetActiveBus(DSS: TDSSContext; const BusName:String):Integer;
procedure SetDataPath(DSS: TDSSContext; const PathName:String);

procedure SetLastResultFile(DSS: TDSSContext; Const Fname:String);

procedure MakeNewCircuit(DSS: TDSSContext; Const Name:String);

PROCEDURE AppendGlobalResult(DSS: TDSSContext; Const s: String); overload;
PROCEDURE AppendGlobalResult(DSS: TDSSContext; Const b: Boolean); overload;
PROCEDURE AppendGlobalResult(DSS: TDSSContext; Const d: Double); overload;
PROCEDURE AppendGlobalResult(DSS: TDSSContext; Const i: Integer); overload;
procedure AppendGlobalResultCRLF(DSS: TDSSContext; const S:String);  // Separate by CRLF

procedure ResetQueryLogFile(DSS: TDSSContext);
procedure WriteQueryLogFile(DSS: TDSSContext; Const Prop, S:String);

{$IFDEF DSS_CAPI_PM}
procedure Wait4Actors(MainDSS: TDSSContext; ActorOffset: Integer);
procedure DoClone(MainDSS: TDSSContext);
procedure New_Actor_Slot(MainDSS: TDSSContext);
{$ENDIF}

function DSSTranslate(const s: String): String;
function DSSHelp(const s: String): String;

implementation

USES
     BufStream,
     {$IFDEF MSWINDOWS}
     Windows,
     // SHFolder,
     {$ENDIF}
     SysUtils,
     CAPI_Metadata,
     CmdForms,
     {$IFDEF DSS_CAPI_PM}
     syncobjs,
     {$ENDIF}
     Solution,
     Executive,
     Utilities,
     ExecCommands,
     ExecOptions,
     DSSHelper;

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

PROCEDURE DoErrorMsg(DSS: TDSSContext; Const S, Emsg, ProbCause:String; ErrNum:Integer);

VAR
    Msg:String;
    Retval:Integer;
begin
    Msg := Format(_('Error %d Reported From OpenDSS Intrinsic Function: '), [Errnum])+ CRLF  + S
        + CRLF + CRLF + _('Error Description: ') + CRLF + Emsg
        + CRLF + CRLF + _('Probable Cause: ') + CRLF+ ProbCause;

    if not NoFormsAllowed then
    begin
        if DSS.In_Redirect then
        begin
            RetVal := DSSMessageDlg(Msg, FALSE);
        end
        else
            DSSMessageDlg(Msg, TRUE);
    end;
    if DSS_CAPI_EARLY_ABORT then
        DSS.Redirect_Abort := True;

     DSS.LastErrorMessage := Msg;
     DSS.ErrorNumber := ErrNum;
     AppendGlobalResultCRLF(DSS, Msg);
     DSS.SolutionAbort  :=  True;
End;

PROCEDURE AppendGlobalResultCRLF(DSS: TDSSContext; const S: String);
begin
    if Length(DSS.GlobalResult) > 0 then
        DSS.GlobalResult := DSS.GlobalResult + CRLF + S
    ELSE 
        DSS.GlobalResult := S;

    DSS.ErrorStrings.Add(Format('(%d) %s' ,[DSS.ErrorNumber, S]));  // Add to Error log
end;

PROCEDURE DoSimpleMsg(DSS: TDSSContext; Const S:String; ErrNum:Integer);
var
    Retval:Integer;
Begin
    if not NoFormsAllowed then 
    begin
        if DSS.In_Redirect then
        begin
            RetVal := DSSMessageDlg(Format('(%d) OpenDSS %s%s', [Errnum, CRLF, S]), FALSE);
        end
        else
            DSSInfoMessageDlg(Format('(%d) OpenDSS %s%s', [Errnum, CRLF, S]));
    end;
    if DSS_CAPI_EARLY_ABORT then
        DSS.Redirect_Abort := True;

    DSS.LastErrorMessage := S;
    DSS.ErrorNumber := ErrNum;
    AppendGlobalResultCRLF(DSS, S);
End;

procedure DoSimpleMsg(DSS: TDSSContext; Const S: String; fmtArgs: Array of Const; ErrNum:Integer);
begin
    DoSimpleMsg(DSS, Format(_(S), fmtArgs), ErrNum)
end;

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
          DoSimpleMsg(DSS, Format(_('Error! Object "%s" not found.'), [ObjName]) + CRLF + DSS.Parser.CmdString, 904);
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
        DoSimpleMsg(DSS, _('Error! Active object type/class is not set.'), 905);
end;

FUNCTION SetActiveBus(DSS: TDSSContext; const BusName:String):Integer;
begin
   // Now find the bus and set active
   Result := 0;

   WITH DSS.ActiveCircuit Do
     Begin
        If BusList.Count = 0 Then Exit;   // Buslist not yet built
        ActiveBusIndex := BusList.Find(BusName);
        IF   ActiveBusIndex=0 Then
          Begin
            Result := 1;
            AppendGlobalResult(DSS, Format(_('SetActiveBus: Bus "%s" notfound'), [BusName]));
          End;
     End;
end;

procedure ClearAllCircuits_SingleContext(DSS: TDSSContext);
Begin
    DSS.ActiveCircuit := DSS.Circuits.First;
    while DSS.ActiveCircuit <> nil do
    begin
        DSS.ActiveCircuit.Free;
        DSS.ActiveCircuit := DSS.Circuits.Next;
    end;
    DSS.Circuits.Free;
    DSS.Circuits := TDSSPointerList.Create(2);   // Make a new list of circuits
    DSS.NumCircuits := 0;

    // Revert on key global flags to Original States
    DSS.DefaultEarthModel     := DERI;
    DSS.LogQueries            := FALSE;
    DSS.MaxAllocationIterations := 2;
End;
{$IFDEF DSS_CAPI_PM}
procedure ClearAllCircuits_AllContexts(DSS: TDSSContext);
var
    i : integer;
    PMParent: TDSSContext;
begin
    PMParent := DSS.GetPrime();
    
    for i := 0 to High(PMParent.Children) do
        with PMParent.Children[i] do
        begin
            // In case the actor hasn't been destroyed
            if ActorThread <> nil then
            begin
                SolutionAbort := True;
                ActorThread.Send_Message(TActorMessage.EXIT_ACTOR);
                ActorThread.WaitFor();
                ActorThread.Free;
                ActorThread := nil;
            end;

            ActiveCircuit := Circuits.First;
            while ActiveCircuit <> nil do
            begin
                ActiveCircuit.Free;
                ActiveCircuit := Circuits.Next;
            end;
            ActiveCircuit := Circuits.First;
            NumCircuits := 0;
            Circuits.Free;
            Circuits := TDSSPointerList.Create(2);   // Make a new list of circuits
            
            //TODO: check why v8 does this:
            // FreeAndNil(Parser);
            
            // Revert on key global flags to Original States
            DefaultEarthModel := DERI;
            LogQueries := FALSE;
            MaxAllocationIterations := 2;
        end;
        
    PMParent.ActiveChild := PMParent;
    PMParent.ActiveChildIndex := 0;
End;
{$ENDIF}// DSS_CAPI_PM

PROCEDURE MakeNewCircuit(DSS: TDSSContext; Const Name:String);
Var
    S: String;
Begin
    If DSS.NumCircuits <= MaxCircuits - 1 Then
    Begin
        DSS.ActiveCircuit := TDSSCircuit.Create(DSS, Name);
        // DSS.ActiveDSSObject := DSS.ActiveCircuit.Solution;
        DSS.Circuits.Add(DSS.ActiveCircuit);
        Inc(DSS.NumCircuits);
        S := DSS.Parser.Remainder;    // Pass remainder of string on to vsource.
        // Create a default Circuit
        DSS.SolutionAbort := False;
        // Voltage source named "source" connected to SourceBus
        DSS.DSSExecutive.Command := 'New object=vsource.source Bus1=SourceBus ' + S;  // Load up the parser as if it were read in
    End
    Else
    Begin
        DoErrorMsg(DSS, 'MakeNewCircuit',
           _('Cannot create new circuit.'),
           Format(_('Max. Circuits Exceeded. (Max no. of circuits=%d)'), 
           [Maxcircuits]), 906);
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

PROCEDURE AppendGlobalResult(DSS: TDSSContext; Const b: Boolean); overload;
Begin
    if b then
        AppendGlobalResult(DSS, 'Yes')
    Else
        AppendGlobalResult(DSS, 'No')
End;

PROCEDURE AppendGlobalResult(DSS: TDSSContext; Const d: Double); overload;
Begin
    AppendGlobalResult(DSS, Format('%-g', [d]));
End;

PROCEDURE AppendGlobalResult(DSS: TDSSContext; Const i: Integer); overload;
Begin
    AppendGlobalResult(DSS, IntToStr(i));
End;


function VersionString: String;
var
    timestamp: String;
BEGIN
    timestamp := Format('%.4d%.2d%.2d%.2d%.2d%.2d', [
        {$include %DATEYEAR%}, 
        {$include %DATEMONTH%},
        {$include %DATEDAY%},
        {$include %TIMEHOUR%},
        {$include %TIMEMINUTE%},
        {$include %TIMESECOND%}
    ]);

    Result := 'DSS C-API Library version ' + DSS_CAPI_VERSION +
              ' revision ' + DSS_CAPI_REV +
              ' based on OpenDSS SVN ' + DSS_CAPI_SVN_REV
              + ' [FPC ' + {$include %FPCVersion%} + ']'
    {$IFDEF CPU64}
              + ' (64-bit build)'
    {$ENDIF}
    {$IFDEF CPU32}
              + ' (32-bit build)'
    {$ENDIF}
    {$IFDEF DSS_CAPI_MVMULT}
              + ' MVMULT'
    {$ENDIF}
    {$IFDEF DSS_CAPI_INCREMENTAL_Y}
              + ' INCREMENTAL_Y'
    {$ENDIF}
    {$IFDEF DSS_CAPI_CONTEXT}
              + ' CONTEXT_API'
    {$ENDIF}
    {$IFDEF DSS_CAPI_PM}
              + ' PM'
    {$ENDIF}
    {$IFDEF DSS_CAPI_DEBUG_BUILD}
              + ' DEBUG'
    {$ENDIF}
              + ' ' + timestamp
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

PROCEDURE SetDataPath(DSS: TDSSContext; const PathName:String);
var
  ScratchPath: String;
// Pathname may be null
BEGIN
  if (Length(PathName) > 0) and not DirectoryExists(PathName) then Begin
  // Try to create the directory
    if not CreateDir(PathName) then Begin
      DoSimpleMsg(DSS, 'Cannot create directory: "%s"', [PathName], 907);
      Exit;
    End;
  End;

  DSS.DataDirectory := PathName;

  // Put a \ on the end if not supplied. Allow a null specification.
  If Length(DSS.DataDirectory) > 0 Then Begin
    DSS.SetCurrentDSSDir(DSS.DataDirectory);   // Change to specified directory
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

PROCEDURE ResetQueryLogFile(DSS: TDSSContext);
Begin
     DSS.QueryFirstTime := TRUE;
End;


PROCEDURE WriteQueryLogfile(DSS: TDSSContext; Const Prop, S:String);
{Log file is written after a query command if LogQueries is true.}
Begin
    TRY
        DSS.QueryLogFileName :=  DSS.OutputDirectory + 'QueryLog.csv';
        If DSS.QueryFirstTime then
        Begin
            DSS.QueryLogFile := TBufferedFileStream.Create(DSS.QueryLogFileName, fmCreate);
            FSWriteln(DSS.QueryLogFile, 'Time(h), Property, Result');
            DSS.QueryFirstTime := False;
        end
        Else 
        begin
            DSS.QueryLogFile := TBufferedFileStream.Create(DSS.QueryLogFileName, fmOpenReadWrite);
            DSS.QueryLogFile.Seek(0, soEnd);
        end;

        FSWriteln(DSS.QueryLogFile,Format('%.10g, %s, %s',[DSS.ActiveCircuit.Solution.DynaVars.dblHour, Prop, S]));
        FreeAndNil(DSS.QueryLogFile);
    EXCEPT
        On E:Exception Do DoSimpleMsg(DSS, 'Error writing Query Log file: %s', [E.Message], 908);
    END;
End;

PROCEDURE SetLastResultFile(DSS: TDSSContext; Const Fname:String);
Begin
      DSS.LastResultfile := Fname;
      DSS.ParserVars.Add('@lastfile', Fname);
End;

{$IFDEF DSS_CAPI_PM}
// Waits for all the actors running tasks
procedure Wait4Actors(MainDSS: TDSSContext; ActorOffset: Integer);
var
    i: Integer;
    PMParent, Child, DSS: TDSSContext;
begin
    PMParent := MainDSS.GetPrime();
    DSS := MainDSS.ActiveChild;
    for i := ActorOffset to High(PMParent.Children) do
    begin
        try
            Child := PMParent.Children[i];
            if Child.ActorStatus = TActorStatus.Idle then
                continue;

            Child.ThreadStatusEvent.ResetEvent();
            while (Child.ActorStatus <> TActorStatus.Idle) do
            begin
                if Child.ThreadStatusEvent.WaitFor(10) = TWaitResult.wrTimeout then
                    continue;
            end;
        except
        on EOutOfMemory do
            Dosimplemsg(DSS, _('Exception Waiting for the parallel thread to finish a job'), 7006);
        end;
    end;
end;

// Clones the active Circuit as many times as requested if possible
procedure DoClone(MainDSS: TDSSContext);
var
    i,
    NumClones: Integer;
    Ref_Ckt: String;
    PMParent, DSS, ChDSS: TDSSContext;
begin
    PMParent := MainDSS.GetPrime();
    DSS := MainDSS.ActiveChild;

    Ref_Ckt := MainDSS.LastFileCompiled;
    DSS.Parser.NextParam;
    NumClones := DSS.Parser.IntValue;
    PMParent.Parallel_enabled := False;
    if ((PMParent.NumOfActors + NumClones) <= CPU_Cores) and (NumClones > 0) then
    begin
        for i := 1 to NumClones do
        begin
            New_Actor_Slot(PMParent);
            ChDSS := PMParent.ActiveChild;
            ChDSS.DSSExecutive.Command := 'compile "' + Ref_Ckt + '"';
            if ChDSS.ActiveCircuit = NIL then
            begin
                DoSimpleMsg(DSS, 'Could not compile the script "%s"', [Ref_Ckt], 7008);
                Exit;
            end;

            // sets the previous maxiterations and controliterations
            ChDSS.ActiveCircuit.Solution.MaxIterations := DSS.ActiveCircuit.Solution.MaxIterations;
            ChDSS.ActiveCircuit.Solution.MaxControlIterations := DSS.ActiveCircuit.Solution.MaxControlIterations;

            // Solves the circuit
            DSS.CmdResult := ExecOptions.DoSetCmd(ChDSS, 1);
        end;
    end
    else
    begin
        if NumClones > 0 then
            DoSimpleMsg(DSS, _('There are no more CPUs available'), 7001)
        else
            DoSimpleMsg(DSS, _('The number of clones requested is invalid'), 7004)
    end;
end;

// Prepares memory to host a new actor
procedure New_Actor_Slot(MainDSS: TDSSContext);
var
    PMParent, DSS: TDSSContext;
begin
    PMParent := MainDSS.GetPrime();
    DSS := MainDSS.ActiveChild;

    if (High(PMParent.Children) + 1) < CPU_Cores then
    begin
        SetLength(PMParent.Children, High(PMParent.Children) + 2);
        PMParent.ActiveChildIndex := High(PMParent.Children);
        PMParent.ActiveChild := TDSSContext.Create(PMParent);
        PMParent.Children[PMParent.ActiveChildIndex] := PMParent.ActiveChild;
        PMParent.ActiveChild._Name := '_' + inttostr(PMParent.ActiveChildIndex + 1);
        // PMParent.ActiveChild.CPU := PMParent.ActiveChildIndex;
        DSS.GlobalResult := inttostr(PMParent.ActiveChildIndex + 1);
    end
    else 
        DoSimpleMsg(DSS, _('There are no more CPUs available'), 7001)
End;
{$ENDIF}


function DSSTranslate(const s: String): String;
begin
    if DSSMessages = NIL then
    begin
        Result := s;
        Exit;
    end;
    Result := DSSMessages.Translate(s);
    if Length(Result) = 0 then
        Result := s;
end;

function DSSHelp(const s: String): String;
begin
    if DSSPropertyHelp = NIL then
    begin
        Result := 'NO HELP OR DESCRIPTION AVAILABLE.';
        Exit;
    end;
    Result := DSSPropertyHelp.Translate(s);
    if Length(Result) = 0 then
        Result := s;
end;



initialization
    SQRT2 := Sqrt(2.0);
    SQRT3 := Sqrt(3.0);
    InvSQRT3 := 1.0/SQRT3;
    InvSQRT3x1000 := InvSQRT3 * 1000.0;

    // Initialize filenames and directories

    DSSDirectory := ExpandFileName('');
    // want to know if this was built for 64-bit, not whether running on 64 bits
    // (i.e. we could have a 32-bit build running on 64 bits; not interested in that

    StartupDirectory := GetCurrentDir + PathDelim;
    if SysUtils.GetEnvironmentVariable('DSS_BASE_FREQUENCY') <> '' then
    begin
        GlobalDefaultBaseFreq  := StrToInt(SysUtils.GetEnvironmentVariable('DSS_BASE_FREQUENCY'));
    end;

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

{$IFNDEF MSWINDOWS}
    CPU_Freq := 1000; // until we can query it
{$ELSE}
    QueryPerformanceFrequency(CPU_Freq);
{$ENDIF}
    CPU_Cores := GetLogicalCpuCount;

    DSS_CAPI_INFO_SPARSE_COND := (SysUtils.GetEnvironmentVariable('DSS_CAPI_INFO_SPARSE_COND') = '1');

    // Default is True, disable at initialization only when DSS_CAPI_EARLY_ABORT = 0
    DSS_CAPI_EARLY_ABORT := (SysUtils.GetEnvironmentVariable('DSS_CAPI_EARLY_ABORT') <> '0');

    // Default is True, disable at initialization when DSS_CAPI_ALLOW_EDITOR = 0
    DSS_CAPI_ALLOW_EDITOR := (SysUtils.GetEnvironmentVariable('DSS_CAPI_ALLOW_EDITOR') <> '0');
    DSS_CAPI_EXT_ERRORS := (SysUtils.GetEnvironmentVariable('DSS_CAPI_EXT_ERRORS') <> '0');

    // Default is False, enable at initialization when DSS_CAPI_LEGACY_MODELS = 1
    DSS_CAPI_LEGACY_MODELS := (SysUtils.GetEnvironmentVariable('DSS_CAPI_LEGACY_MODELS') = '1');
    DSS_CAPI_LEGACY_MODELS := DSS_CAPI_LEGACY_MODELS_PREV;

    // For the 0.12.x branch, default is True, disable at initialization when DSS_CAPI_COM_DEFAULTS = 0
    DSS_CAPI_COM_DEFAULTS := (GetEnvironmentVariable('DSS_CAPI_COM_DEFAULTS') <> '0');;
    // For the 0.12.x branch, default is True, disable at initialization when DSS_CAPI_ALLOW_CHANGE_DIR = 0
    DSS_CAPI_ALLOW_CHANGE_DIR := (SysUtils.GetEnvironmentVariable('DSS_CAPI_ALLOW_CHANGE_DIR') <> '0');

    ExecCommands.DefineCommands;

try
   DSSPrime := TDSSContext.Create(nil, True);
except 
    on E: Exception do
    begin
        DSSPrime := nil;
    end;
end;

finalization
    if DSSPrime <> nil then
    begin
        DSSPrime.Free;
        DSSPrime := nil;
    end;

    ExecCommands.DisposeStrings;
end.
