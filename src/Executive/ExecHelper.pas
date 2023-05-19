unit ExecHelper;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

// Functions for performing DSS Exec Commands and Options

{$MODE DELPHI}

interface

uses
    Executive,
    Classes;

type
    TExecHelper = class helper for TExecutive
    PUBLIC
        function DoNewCmd: Integer;
        function DoEditCmd: Integer;
        function DoBatchEditCmd: Integer;
        function DoSelectCmd: Integer;
        function DoMoreCmd: Integer;
        function DoRedirect(IsCompile: Boolean; inputStrings: TStringList = NIL): Integer;
        function DoSaveCmd: Integer;
        function DoSampleCmd: Integer;


        function DoSolveCmd: Integer;
        function DoEnableCmd: Integer;
        function DoDisableCmd: Integer;

        function DoOpenCmd: Integer;
        function DoResetCmd: Integer;
        function DoNextCmd: Integer;
        function DoFormEditCmd: Integer;
        function DoClassesCmd: Integer;
        function DoUserClassesCmd: Integer;
        function DoHelpCmd: Integer;
        function DoClearCmd: Integer;
{$IFDEF DSS_CAPI_PM}
        function DoClearAllCmd: Integer;
{$ENDIF}
        function DoReduceCmd: Integer;
        function DoInterpolateCmd: Integer;

        function DoCloseCmd: Integer;
        function DoResetMonitors: Integer;

        function DoFileEditCmd: Integer;
        function DoQueryCmd: Integer;
        function DoResetMeters: Integer;
        procedure DoAboutBox;
        function DoSetVoltageBases: Integer;
        function DoSetkVBase: Integer;

        procedure DoLegalVoltageBases;
        procedure DoAutoAddBusList(const S: String);
        procedure DoKeeperBusList(const S: String);
        procedure DoSetReduceStrategy(const S: String);
        procedure DoSetAllocationFactors(const X: Double);
        procedure DoSetCFactors(const X: Double);

        function DovoltagesCmd(const PerUnit: Boolean): Integer;
        function DocurrentsCmd: Integer;
        function DopowersCmd(Total: Integer): Integer;
        function DoseqvoltagesCmd: Integer;
        function DoseqcurrentsCmd: Integer;
        function DoseqpowersCmd: Integer;
        function DolossesCmd: Integer;
        function DophaselossesCmd: Integer;
        function DocktlossesCmd: Integer;
        function DoAllocateLoadsCmd: Integer;
        function DoHarmonicsList(const S: String): Integer;
        function DoMeterTotals: Integer;
        function DoCapacityCmd: Integer;
        function DoZscCmd(Zmatrix: Boolean): Integer;
        function DoZsc10Cmd: Integer;
        function DoZscRefresh: Integer;
        function DoZsc012Cmd: Integer;

        function DoBusCoordsCmd(SwapXY: Boolean): Integer;
        function DoUuidsCmd: Integer;
        function DoSetLoadAndGenKVCmd: Integer;
        function DoVarValuesCmd: Integer;
        function DoVarNamesCmd: Integer;

        function DoMakePosSeq: Integer;
        function DoAlignFileCmd: Integer;
        function DoRotateCmd: Integer;
        function DoVDiffCmd: Integer;
        function DoSummaryCmd: Integer;
        function DoDistributeCmd: Integer;
        function DoDI_PlotCmd: Integer;
        function DoCompareCasesCmd: Integer;
        function DoYearlyCurvesCmd: Integer;
        function DoVisualizeCmd: Integer;
        function DoCloseDICmd: Integer;
        function DoADOScmd: Integer;
        function DoEstimateCmd: Integer;
        function DoReconductorCmd: Integer;
        function DoAddMarkerCmd: Integer;
        function DoCvrtLoadshapesCmd: Integer;
        function DoNodeDiffCmd: Integer;
        function DoRephaseCmd: Integer;
        function DoSetBusXYCmd: Integer;
        function DoUpdateStorageCmd: Integer;
        function DoPstCalc: Integer;
        function DoValVarCmd: Integer;
        function DoLambdaCalcs: Integer;
        function DoVarCmd: Integer;
        function DoNodeListCmd: Integer;
        function DoRemoveCmd: Integer;

        procedure DoSetNormal(pctNormal: Double);

        procedure Set_Time;

        procedure ParseObjName(const fullname: String; var objname, propname: String);

        procedure GetObjClassAndName(var ObjClass, ObjName: String);

        function AddObject(const ObjType, name: String): Integer;
        function EditObject(const ObjType, name: String): Integer;

        procedure SetActiveCircuit(const cktname: String);

        function SetActiveCktElement: Integer;

        function DoPropertyDump: Integer;

    PRIVATE

        procedure MarkCapandReactorBuses;
    end;

implementation

uses
    Command,
    ArrayDef,
    ParserDel,
    SysUtils,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    Monitor, {ShowResults, ExportResults,}
    DSSClass,
    DSSObject,
    Utilities,
    Solution,
    EnergyMeter,
    Generator,
    LoadShape,
    Load,
    PCElement,
    CktElement,
    UComplex,
    DSSUcomplex,
    mathutil,
    Bus,
    SolutionAlgs,
    CmdForms,
    ExecCommands,
    Dynamics,
    Capacitor,
    Reactor,
    Line,
    Lineunits,
    Math,
    CktElementClass,
    Sensor,
    ExportCIMXML,
    NamedObject,
    RegExpr,
    PstCalc,
    PDELement,
    ReduceAlgs,
    Ucmatrix,
    BufStream,
    fpjson,
    DSSHelper;

var
    SaveCommands, DistributeCommands, DI_PlotCommands,
    ReconductorCommands, RephaseCommands, AddMarkerCommands,
    SetBusXYCommands, PstCalcCommands, RemoveCommands: TCommandList;

procedure TExecHelper.GetObjClassAndName(var ObjClass, ObjName: String);
var
    ParamName: String;
    Param: String;

begin
    //   We're looking for Object Definition:
    //
    //    ParamName = 'object' IF given
    //     and the name of the object
    //
    //     Object=Capacitor.C1
    //    or just Capacitor.C1
    //
    //   If no dot, last class is assumed

    ObjClass := '';
    ObjName := '';
    ParamName := AnsiLowerCase(DSS.Parser.NextParam);
    Param := DSS.Parser.StrValue;
    if Length(ParamName) > 0 then
    begin   // IF specified, must be object or an abbreviation
        if ComparetextShortest(ParamName, 'object') <> 0 then
        begin
            DoSimpleMsg(DSS, 'object=Class.Name expected as first parameter in command. %s', [CRLF + DSS.Parser.CmdString], 240);
            Exit;
        end;
    end;
    ParseObjectClassandName(DSS, Param, ObjClass, ObjName);     // see DSSGlobals
end;


function TExecHelper.DoNewCmd: Integer;

// Process the New Command
// new type=xxxx name=xxxx  editstring

// IF the device being added already exists, the default behavior is to
// treat the New command as an Edit command.  This may be overridden
// by setting the DuplicatesAllowed VARiable to true, in which CASE,
// the New command always results in a new device being added.

var
    ObjClass, ObjName: String;
    handle: Integer;

begin
    Result := 0;
    Handle := 0;

    GetObjClassAndName(ObjClass, ObjName);

    if CompareText(ObjClass, 'solution') = 0 then
    begin
        DoSimpleMsg(DSS, _('You cannot create new Solution objects through the command interface.'), 241);
        Exit;
    end;

    if CompareText(ObjClass, 'circuit') = 0 then
    begin
        MakeNewCircuit(DSS, ObjName);  // Make a new circuit
        ClearEventLog(DSS);      // Start the event log in the current directory
        ClearErrorLog(DSS);
    end
    else    // Everything else must be a circuit element or DSS Object
    begin
        Handle := AddObject(ObjClass, ObjName);
    end;

    if Handle = 0 then
        Result := 1;

end;

function TExecHelper.DoEditCmd: Integer;

// edit type=xxxx name=xxxx  editstring
var
    ObjType, ObjName: String;

begin
    Result := 0;

    GetObjClassAndName(ObjType, ObjName);

    if CompareText(ObjType, 'circuit') = 0 then
    begin
                 // Do nothing
    end
    else
    begin
        // Everything ELSE must be a circuit element
        Result := EditObject(ObjType, ObjName);

    end;
end;

function TExecHelper.DoBatchEditCmd: Integer;
// batchedit type=xxxx name=pattern  editstring
var
    ObjType, Pattern: String;
    RegEx1: TRegExpr;
    pObj: TDSSObject;
    Params: Integer;
begin
    Result := 0;
    GetObjClassAndName(ObjType, Pattern);
    if CompareText(ObjType, 'circuit') = 0 then
    begin
    // Do nothing
    end
    else
    begin
        DSS.LastClassReferenced := DSS.ClassNames.Find(ObjType);

        case DSS.LastClassReferenced of
            0:
            begin
                DoSimpleMsg(DSS, 'BatchEdit Command: Object Type "%s" not found. %s', [ObjType, CRLF + DSS.Parser.CmdString], 267);
                Exit;
            end;{Error}
        else
            Params := DSS.Parser.Position;
            DSS.ActiveDSSClass := DSS.DSSClassList.Get(DSS.LastClassReferenced);
            RegEx1 := TRegExpr.Create;
            RegEx1.ModifierI := TRUE; // equivalent to RegEx1.Options:=[preCaseLess]
            RegEx1.Expression := Utf8string(Pattern);
            if DSS.ActiveDSSClass.First > 0 then
                pObj := DSS.ActiveDSSObject
            else
                pObj := NIL;
            while pObj <> NIL do
            begin
                if RegEx1.Exec(Utf8string(pObj.Name)) then
                begin
                    DSS.Parser.Position := Params;
                    DSS.ActiveDSSClass.Edit(DSS.Parser);
                end;
                if DSS.ActiveDSSClass.Next > 0 then
                    pObj := DSS.ActiveDSSObject
                else
                    pObj := NIL;
            end;
            RegEx1.Free;
        end;
    end;
end;

function TExecHelper.DoRedirect(IsCompile: Boolean; inputStrings: TStringList): Integer;
//  This routine should be recursive
//  So you can redirect input an arbitrary number of times

// If Compile, makes directory of the file the new home directory
// If not Compile (is simple redirect), return to where we started

var
    Fin: TextFile;
    InputLine, CurrDir, SaveDir, ReDirFileExp: String;
    LocalCompFileName: String;
    InBlockComment: Boolean;
    strings: TStringList;
    gotTheFile: Boolean;
    stringIdx: Integer;
    LineNum: Integer = 0;
    Fstream: TStream;
    wasProvidedStrings: Boolean = false;
begin
    gotTheFile := FALSE;
    strings := inputStrings;
    Result := 0;
    InBlockComment := FALSE;  // Discarded off stack upon return
    // Therefore extent of block comment does not extend beyond a file
    // Going back up the redirect stack

    if (strings <> NIL) then
    begin
        gotTheFile := true;
        ReDirFileExp := '<user-provided string>';
        wasProvidedStrings := true;
    end;

    if (not gotTheFile) and InZip then
    begin
        // Get next parm and try to interpret as a file name
        DSS.Parser.NextParam;

        if DSS.Parser.StrValue = '' then
            exit;  // ignore altogether IF null filename

        try
            Fstream := GetZipStream(DSS.Parser.StrValue);
        except
            on E: Exception do
            begin
                DoSimpleMsg(DSS, 'Redirect File "%s" could not be read: %s', [DSS.Parser.StrValue, E.Message], 2202);
                DSS.SolutionAbort := TRUE;
                Exit;
            end;
        end;


        strings := TStringList.Create;
        strings.LoadFromStream(Fstream);
        Fstream.Free;
        ReDirFileExp := DSS.inZipPath + DSS.Parser.StrValue;
        gotTheFile := TRUE;
        SaveDir := DSS.inZipPath;
    end
    else 
    if (not gotTheFile) then
    begin
        // Get next parm and try to interpret as a file name
        DSS.Parser.NextParam;

        // Expanded path is required later as other Free Pascal functions 
        // may fail with relative paths
        ReDirFileExp := ExpandFileName(DSS.Parser.StrValue);

        // First check if we need to workaround the SetCurrentDir issues
        if (not DSS_CAPI_ALLOW_CHANGE_DIR) then
        begin
            ReDirFileExp := ExpandFileName(AdjustInputFilePath(DSS, DSS.Parser.StrValue));
        end;

        DSS.ReDirFile := ReDirFileExp;// DSS.Parser.StrValue;
        if DSS.ReDirFile = '' then
            exit;  // ignore altogether IF null filename

        SaveDir := DSS.CurrentDSSDir;
    end;


    if (not gotTheFile) and (FileExists(DSS.ReDirFile)) then
    begin
        // If the usual Pascal text file is broken, 
        // try a stream via a TStringList object
        try
            strings := TStringList.Create;
            strings.LoadFromFile(DSS.ReDirFile);
            if IsCompile then
            begin
                DSS.LastFileCompiled := DSS.ReDirFile;
                LocalCompFileName := DSS.ReDirFile;
            end;
            gotTheFile := TRUE;
        except
            FreeAndNil(strings);
        end;
    end;

    if not gotTheFile then
    begin
        try
            // First try, using the provided name directly
            AssignFile(Fin, DSS.ReDirFile);
            Reset(Fin);
            if IsCompile then
            begin
                DSS.LastFileCompiled := DSS.ReDirFile;
                LocalCompFileName := DSS.ReDirFile;
            end;
            gotTheFile := TRUE;
        except
            // intentionally blank
        end;
    end;

    // For full backwards compatibility
    DSS.ReDirFile := ReDirFileExp;

    if not gotTheFile then
    begin
        // Try the expanded name
        if DSS.ReDirFile = '' then
            exit;

        try
            AssignFile(Fin, DSS.ReDirFile);
            Reset(Fin);
            if IsCompile then
            begin
                DSS.LastFileCompiled := DSS.ReDirFile;
                LocalCompFileName := DSS.ReDirFile;
            end;
            gotTheFile := TRUE;
        except
            // intentionally blank
        end;
    end;

    if not gotTheFile and FileExists(DSS.ReDirFile) then
    begin
        // If the usual Pascal text file is broken, 
        // try a stream via a TStringList object
        try
            strings := TStringList.Create;
            strings.LoadFromFile(DSS.ReDirFile);
            if IsCompile then
            begin
                DSS.LastFileCompiled := DSS.ReDirFile;
                LocalCompFileName := DSS.ReDirFile;
            end;
            gotTheFile := TRUE;
        except
            FreeAndNil(strings);
        end;
    end;

    if not gotTheFile then
    begin
        // Couldn't find file
        // Try appending a '.dss' to the file name
        // If it doesn't already have an extension
        if Pos('.', DSS.ReDirFile) = 0 then
        begin
            DSS.ReDirFile := DSS.ReDirFile + '.dss';
            LocalCompFileName := DSS.ReDirFile;
            try
                AssignFile(Fin, DSS.ReDirFile);
                Reset(Fin);
            except
                DoSimpleMsg(DSS, 'Redirect file not found: "%s"', [DSS.Parser.StrValue], 242);
                DSS.SolutionAbort := TRUE;
                Exit;
            end;
            gotTheFile := TRUE;
        end;
    end;

    if not gotTheFile then
    begin
        DoSimpleMsg(DSS, 'Redirect file not found: "%s"', [DSS.Parser.StrValue], 243);
        DSS.SolutionAbort := TRUE;
        exit;  // Already had an extension, so just bail
    end;

    // For full backwards compatibility
    DSS.ReDirFile := ReDirFileExp;

    // OK, we finally got one open, so we're going to continue
    try
        try
            if not wasProvidedStrings then // skip directory shenanigans when provided the file contents directly
            begin
                // Change Directory to path specified by file in CASE that
                // loads in more files
                CurrDir := ExtractFileDir(DSS.ReDirFile);
                if not InZip then
                begin
                    DSS.SetCurrentDSSDir(CurrDir);
                    if IsCompile then
                        SetDataPath(DSS, CurrDir);  // change datadirectory
                end
                else
                begin
                    SetInZipPath(CurrDir);
                end;
            end;

            DSS.Redirect_Abort := FALSE;
            DSS.In_Redirect := TRUE;

            if strings = NIL then
            begin
                // Traditional TextFile is used
                while not ((EOF(Fin)) or (DSS.Redirect_Abort)) do
                begin
                    Readln(Fin, InputLine);
                    Inc(LineNum);
                    if Length(InputLine) > 0 then
                    begin
                        if not InBlockComment then     // look for '/*'  at baginning of line
                            case InputLine[1] of
                                '/':
                                    if (Length(InputLine) > 1) and (InputLine[2] = '*') then
                                        InBlockComment := TRUE;
                            end;

                        if not InBlockComment then   // process the command line
                            if not DSS.SolutionAbort then
                                Set_Command(InputLine, LineNum)
                            else
                                DSS.Redirect_Abort := TRUE;  // Abort file if solution was aborted

                        // in block comment ... look for */   and cancel block comment (whole line)
                        if InBlockComment then
                            if Pos('*/', Inputline) > 0 then
                                InBlockComment := FALSE;

                    end;

                end // WHILE Not ( (EOF(Fin)) or (Redirect_Abort) ) DO
            end
            else
            begin
                // The string list is used
                for stringIdx := 0 to (strings.Count - 1) do
                begin
                    if DSS.Redirect_Abort then
                        break;

                    LineNum := stringIdx + 1;
                    InputLine := strings[stringIdx];
                    if Length(InputLine) > 0 then
                    begin
                        if not InBlockComment then     // look for '/*'  at baginning of line
                            case InputLine[1] of
                                '/':
                                    if (Length(InputLine) > 1) and (InputLine[2] = '*') then
                                        InBlockComment := TRUE;
                            end;

                        if not InBlockComment then   // process the command line
                            if not DSS.SolutionAbort then
                                Set_Command(InputLine, LineNum)
                            else
                                DSS.Redirect_Abort := TRUE;  // Abort file if solution was aborted

                        // in block comment ... look for */   and cancel block comment (whole line)
                        if InBlockComment then
                            if Pos('*/', Inputline) > 0 then
                                InBlockComment := FALSE;
                    end;
                end; // for stringIdx := 1 to strings.Count do
            end;

            if (not wasProvidedStrings) and (DSS.ActiveCircuit <> NIL) then
                DSS.ActiveCircuit.CurrentDirectory := CurrDir + PathDelim;

        except
            On E: Exception do
                DoErrorMsg(DSS, _('DoRedirect: Error Processing Input Stream in Compile/Redirect.'),
                    E.Message,
                    Format(_('Error in File: "%s" or Filename itself.'), [DSS.ReDirFile]), 244);
        end;
    finally
        if gotTheFile and (DSS.Redirect_Abort or (DSS.ErrorNumber <> 0)) then
        begin
            DSS.LastErrorMessage := DSS.LastErrorMessage + CRLF +
                Format(_('[file: "%s", line: %d]'), [ReDirFileExp, LineNum]);
        end;

        if strings <> NIL then
            FreeAndNil(strings)
        else
            CloseFile(Fin);

        DSS.In_Redirect := FALSE;
        DSS.ParserVars.Add('@lastfile', DSS.ReDirFile);

        if not wasProvidedStrings then // skip directory shenanigans when provided the file contents directly
        begin
            if not InZip then
            begin
                if IsCompile then
                begin
                    SetDataPath(DSS, CurrDir); // change datadirectory
                    DSS.LastCommandWasCompile := TRUE;
                    DSS.ParserVars.Add('@lastcompilefile', LocalCompFileName); // will be last one off the stack
                end
                else
                begin
                    DSS.SetCurrentDSSDir(SaveDir);    // set back to where we were for redirect, but not compile
                    DSS.ParserVars.Add('@lastredirectfile', DSS.ReDirFile);
                end;
            end
            else
            begin
                if not IsCompile then
                    SetInZipPath(SaveDir);
            end;
        end;
    end;
end;

function TExecHelper.DoSelectCmd: Integer;

// select active object
// select element=elementname terminal=terminalnumber
var
    ObjClass, ObjName,
    Param: String;

begin
    Result := 1;

    GetObjClassAndName(ObjClass, ObjName);  // Parse Object class and name

    if (Length(ObjClass) = 0) and (Length(ObjName) = 0) then
        Exit;  // select active obj if any

    if CompareText(ObjClass, 'circuit') = 0 then
    begin
        SetActiveCircuit(ObjName);
    end
    else
    begin
        // Everything else must be a circuit element
        if Length(ObjClass) > 0 then
            SetObjectClass(DSS, ObjClass);

        DSS.ActiveDSSClass := DSS.DSSClassList.Get(DSS.LastClassReferenced);
        if DSS.ActiveDSSClass <> NIL then
        begin
            if not DSS.ActiveDSSClass.SetActive(Objname) then
            begin // scroll through list of objects untill a match
                DoSimpleMsg(DSS, 'Error! Object "%s" not found. %s', [ObjName, CRLF + DSS.Parser.CmdString], 245);
                Result := 0;
            end
            else
                with DSS.ActiveCircuit do
                begin
                    case DSS.ActiveDSSObject.DSSObjType of
                        DSS_OBJECT: ;  // do nothing for general DSS object

                    else
                    begin   // for circuit types, set DSS.ActiveCircuit Element, too
                        ActiveCktElement := DSS.ActiveDSSClass.GetActiveObj;
                   // Now check for active terminal designation
                        DSS.Parser.NextParam;
                        Param := DSS.Parser.StrValue;
                        if Length(Param) > 0 then
                            ActiveCktElement.ActiveTerminalIdx := DSS.Parser.Intvalue
                        else
                            ActiveCktElement.ActiveTerminalIdx := 1;  {default to 1}
                        with ActiveCktElement do
                            SetActiveBus(DSS, StripExtension(Getbus(ActiveTerminalIdx)));
                    end;
                    end;
                end;
        end
        else
        begin
            DoSimpleMsg(DSS, _('Error! Active object type/class is not set.'), 246);
            Result := 0;
        end;

    end;
end;

function TExecHelper.DoMoreCmd: Integer;
// more editstring  (assumes active circuit element)
begin
    if DSS.ActiveDSSClass <> NIL then
        Result := DSS.ActiveDSSClass.Edit(DSS.Parser)
    else
        Result := 0;
end;

function TExecHelper.DoSaveCmd: Integer;
// Save current values in both monitors and Meters
var
    pMon: TMonitorObj;
    pMtr: TEnergyMeterObj;
    i: Integer;

    ParamPointer: Integer;
    ParamName,
    Param: String;
    ObjClass: String;
    SaveDir: String;
    saveFile: String;
    DSSClass: TDSSClass;
begin
    Result := 0;
    ObjClass := '';
    SaveDir := DSS.OutputDirectory; {CurrentDSSDir;}
    SaveFile := '';
    ParamPointer := 0;
    ParamName := DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;
    while Length(Param) > 0 do
    begin
        if (Length(ParamName) = 0) then
            Inc(ParamPointer)
        else
            ParamPointer := SaveCommands.GetCommand(ParamName);

        case ParamPointer of
            1:
                ObjClass := DSS.Parser.StrValue;
            2:
                Savefile := DSS.Parser.StrValue;   // File name for saving  a class
            3:
                SaveDir := DSS.Parser.StrValue;
        else

        end;

        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
    end;

    DSS.InShowResults := TRUE;
    if (Length(ObjClass) = 0) or (CompareTextShortest(ObjClass, 'meters') = 0) then
    begin
   // Save monitors and Meters

        with DSS.ActiveCircuit.Monitors do
            for i := 1 to Count do
            begin
                pMon := Get(i);
                pMon.Save;
            end;

        with DSS.ActiveCircuit.EnergyMeters do
            for i := 1 to Count do
            begin
                pMtr := Get(i);
                pMtr.SaveRegisters;
            end;

        Exit;
    end;
    if CompareTextShortest(ObjClass, 'circuit') = 0 then
    begin
        if not DSS.ActiveCircuit.Save(SaveDir) then
            Result := 1;
        Exit;
    end;
    if CompareTextShortest(ObjClass, 'voltages') = 0 then
    begin
        DSS.ActiveCircuit.Solution.SaveVoltages;
        Exit;
    end;

   {Assume that we have a class name for a DSS Class}
    DSSClass := GetDSSClassPtr(DSS, ObjClass);
    if DSSClass <> NIL then
    begin
        if Length(SaveFile) = 0 then
            SaveFile := objClass;
        if Length(SaveDir) > 0 then
        begin
            if not DirectoryExists(SaveDir) then
                try
                    mkDir(SaveDir);
                except
                    On E: Exception do
                        DoSimpleMsg(DSS, 'Error making Directory: "%s". %s', [SaveDir, E.Message], 247);
                end;
            SaveFile := SaveDir + PathDelim + SaveFile;
        end;
        WriteClassFile(DSS, DSSClass, SaveFile, FALSE); // just write the class with no checks
    end;

    SetLastResultFile(DSS, SaveFile);
    DSS.GlobalResult := SaveFile;
end;

function TExecHelper.DoClearCmd: Integer;
begin
    DSS.DSSExecutive.Clear;
    Result := 0;
end;

{$IFDEF DSS_CAPI_PM}
function TExecHelper.DoClearAllCmd: Integer;
begin
    DSS.DSSExecutive.ClearAll;
    Result := 0;
end;

{$ENDIF}
function TExecHelper.DoHelpCmd: Integer;
begin
    ShowHelpForm(DSS); // DSSForms Unit
    Result := 0;
end;

function TExecHelper.DoSampleCmd: Integer;
// Force all monitors and meters in active circuit to take a sample
begin
    DSS.MonitorClass.SampleAll;
    DSS.EnergyMeterClass.SampleAll;  // gets generators too
    Result := 0;
end;

function TExecHelper.DoSolveCmd: Integer;
begin
    Result := 0;
    DSS.ActiveCircuit.Solution.Solve();
end;

function TExecHelper.SetActiveCktElement: Integer;
// Parses the object off the line and sets it active as a circuitelement.
var
    ObjType, ObjName: String;

begin
    Result := 0;

    GetObjClassAndName(ObjType, ObjName);

    if CompareText(ObjType, 'circuit') = 0 then
    begin
                 // Do nothing
    end
    else
    begin
        if CompareText(ObjType, DSS.ActiveDSSClass.Name) <> 0 then
            DSS.LastClassReferenced := DSS.ClassNames.Find(ObjType);

        case DSS.LastClassReferenced of
            0:
            begin
                DoSimpleMsg(DSS, 'Object Type "%s" not found. %s', [ObjType, CRLF + DSS.Parser.CmdString], 253);
                Result := 0;
                Exit;
            end;{Error}
        else

        // intrinsic and user Defined models
            DSS.ActiveDSSClass := DSS.DSSClassList.Get(DSS.LastClassReferenced);
            if DSS.ActiveDSSClass.SetActive(ObjName) then
                with DSS.ActiveCircuit do
                begin // scroll through list of objects until a match
                    case DSS.ActiveDSSObject.DSSObjType of
                        DSS_OBJECT:
                            DoSimpleMsg(DSS, 'Error in SetActiveCktElement: Object not a circuit Element. %s', [CRLF + DSS.Parser.CmdString], 254);
                    else
                    begin
                        ActiveCktElement := DSS.ActiveDSSClass.GetActiveObj;
                        Result := 1;
                    end;
                    end;
                end;
        end;
    end;
end;


function TExecHelper.DoEnableCmd: Integer;
var
    Objtype, ObjName: String;
    ClassPtr: TDSSClass;
    CktElem: TDSSCktElement;
    i: Integer;
begin
  //   Result := SetActiveCktElement;
  //  IF Result>0 THEN DSS.ActiveCircuit.ActiveCktElement.Enabled := True;

    Result := 0;

    GetObjClassAndName(ObjType, ObjName);

    if CompareText(ObjType, 'circuit') = 0 then
    begin
                 // Do nothing
    end
    else
    if Length(ObjType) > 0 then
    begin
      // only applies to CktElementClass objects
        ClassPtr := GetDSSClassPtr(DSS, ObjType);
        if ClassPtr <> NIL then
        begin
            if (ClassPtr.DSSClassType and BASECLASSMASK) > 0 then
            begin
              // Everything else must be a circuit element
                if CompareText(ObjName, '*') = 0 then
                begin
               // Enable all elements of this class
                    for i := 1 to ClassPtr.ElementCount do
                    begin
                        CktElem := ClassPtr.ElementList.Get(i);
                        CktElem.Enabled := TRUE;
                    end;

                end
                else
                begin
              // just load up the parser and call the edit routine for the object in question

                    DSS.Parser.CmdString := 'Enabled=true';  // Will only work for CktElements
                    Result := EditObject(ObjType, ObjName);
                end;
            end;
        end;
    end;
end;

function TExecHelper.DoDisableCmd: Integer;
var
    Objtype, ObjName: String;
    ClassPtr: TDSSClass;
    CktElem: TDSSCktElement;
    i: Integer;
begin
    Result := 0;

    GetObjClassAndName(ObjType, ObjName);

    if CompareText(ObjType, 'circuit') = 0 then
    begin
                 // Do nothing
    end
    else
    if Length(ObjType) > 0 then
    begin
      // only applies to CktElementClass objects
        ClassPtr := GetDSSClassPtr(DSS, ObjType);
        if ClassPtr <> NIL then
        begin
            if (ClassPtr.DSSClassType and BASECLASSMASK) > 0 then
            begin
              // Everything else must be a circuit element
                if CompareText(ObjName, '*') = 0 then
                begin
               // Disable all elements of this class
                    for i := 1 to ClassPtr.ElementCount do
                    begin
                        CktElem := ClassPtr.ElementList.Get(i);
                        CktElem.Enabled := FALSE;
                    end;

                end
                else
                begin
              // just load up the parser and call the edit routine for the object in question

                    DSS.Parser.CmdString := 'Enabled=false';  // Will only work for CktElements
                    Result := EditObject(ObjType, ObjName);
                end;
            end;
        end;
    end;
//     Result := SetActiveCktElement;
//     IF Result>0 THEN DSS.ActiveCircuit.ActiveCktElement.Enabled := False;
end;

function TExecHelper.DoPropertyDump: Integer;

var
    pObject: TDSSObject;
    F: TFileStream = NIL;
    SingleObject, Debugdump, IsSolution: Boolean;
    i: Integer;
    FileName: String;
    Param, Param2, ObjClass, ObjName: String;
begin
    Result := 0;
    SingleObject := FALSE;
    IsSolution := FALSE;
    DebugDump := FALSE;
    ObjClass := ' ';  // make sure these have at least one character
    ObjName := ' ';

 // Continue parsing command line - check for object name
    DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;
    if Length(Param) > 0 then
    begin
        if CompareText(Param, 'commands') = 0 then
        begin
            DumpAllDSSCommands(DSS, FileName);
{$IFDEF DSS_CAPI}
            DSS.GlobalResult := FileName;
{$ENDIF}
            FireOffEditor(DSS, FileName);
            Exit;
        end;

    // dump bus names hash list
        if CompareText(Param, 'buslist') = 0 then
        begin
            FileName := DSS.OutputDirectory + 'Bus_Hash_List.txt';
            DSS.ActiveCircuit.BusList.DumpToFile(FileName);
{$IFDEF DSS_CAPI}
            DSS.GlobalResult := FileName;
{$ENDIF}
            FireOffEditor(DSS, FileName);
            Exit;
        end;

    // dump device names hash list
        if CompareText(Param, 'devicelist') = 0 then
        begin
            FileName := DSS.OutputDirectory + 'Device_Hash_List.txt';
            DSS.ActiveCircuit.DeviceList.DumpToFile(FileName);
{$IFDEF DSS_CAPI}
            DSS.GlobalResult := FileName;
{$ENDIF}
            FireOffEditor(DSS, FileName);
            Exit;
        end;

        if CompareText(Copy(AnsiLowerCase(Param), 1, 5), 'alloc') = 0 then
        begin
            FileName := DSS.OutputDirectory + 'AllocationFactors.txt';
            DumpAllocationFactors(DSS, FileName);
{$IFDEF DSS_CAPI}
            DSS.GlobalResult := FileName;
{$ENDIF}
            FireOffEditor(DSS, FileName);
            Exit;
        end;

        if CompareText(Param, 'debug') = 0 then
            DebugDump := TRUE
        else
        begin
            if CompareText(Param, 'solution') = 0 then
            begin
          // Assume active circuit solution IF not qualified
          // DSS.ActiveDSSClass := DSS.SolutionClass;
          // DSS.ActiveDSSObject := DSS.ActiveCircuit.Solution;
                IsSolution := TRUE;
            end
            else
            begin
                SingleObject := TRUE;
             // Check to see IF we want a debugdump on this object
                DSS.Parser.NextParam;
                Param2 := DSS.Parser.StrValue;
                if CompareText(Param2, 'debug') = 0 then
                    DebugDump := TRUE;
            // Set active Element to be value in Param
                DSS.Parser.CmdString := '"' + Param + '"';  // put param back into DSS.Parser
                GetObjClassAndName(ObjClass, ObjName);
            // IF DoSelectCmd=0 THEN Exit;  8-17-00
                if SetObjectClass(DSS, ObjClass) then
                begin
                    DSS.ActiveDSSClass := DSS.DSSClassList.Get(DSS.LastClassReferenced);
                    if DSS.ActiveDSSClass = NIL then
                        Exit;
                end
                else
                    Exit;
            end;
        end;
    end;

    try
        F := TBufferedFileStream.Create(DSS.OutputDirectory + DSS.CircuitName_ + 'PropertyDump.txt', fmCreate);
    except
        On E: Exception do
        begin
            DoErrorMsg(DSS,
                Format(_('DoPropertyDump - opening %s DSS_PropertyDump.txt for writing in %s'), [DSS.OutputDirectory, Getcurrentdir]),
                E.Message, _('Disk protected or other file error'), 255);
            Exit;
        end;
    end;

    try
        if SingleObject then
        begin
        // IF ObjName='*' then we dump all objects of this class
            case ObjName[1] of
                '*':
                begin
                    for i := 1 to DSS.ActiveDSSClass.ElementCount do
                    begin
                        DSS.ActiveDSSClass.Active := i;
                        DSS.ActiveDSSObject.DumpProperties(F, DebugDump, TRUE);
                    end;
                end;
            else
                if not DSS.ActiveDSSClass.SetActive(Objname) then
                begin
                    DoSimpleMsg(DSS, 'Error! Object "%s" not found.', [ObjName], 256);
                    Exit;
                end
                else
                    DSS.ActiveDSSObject.DumpProperties(F, DebugDump, TRUE);  // Dump only properties of active circuit element
            end;

        end
        else
        if IsSolution then
        begin
            DSS.ActiveCircuit.Solution.DumpProperties(F, DebugDump, TRUE);
        end
        else
        begin
            // Dump general Circuit stuff

            if DebugDump then
                DSS.ActiveCircuit.DebugDump(F);
            // Dump circuit objects
            try
                for pObject in DSS.ActiveCircuit.CktElements do
                begin
                    pObject.DumpProperties(F, DebugDump, TRUE);
                end;
                for pObject in DSS.DSSObjs do
                begin
                    pObject.DumpProperties(F, DebugDump, TRUE);
                end;
            except
                On E: Exception do
                    DoErrorMsg(DSS, _('DoPropertyDump - Problem writing file.'), E.Message,
                        _('File may be read only, in use, or disk full?'), 257);
            end;

            DSS.ActiveCircuit.Solution.DumpProperties(F, DebugDump, TRUE);
        end;

    finally

        FreeAndNil(F);
    end;  {TRY}

    FileName := DSS.OutputDirectory + DSS.CircuitName_ + 'PropertyDump.txt';
{$IFDEF DSS_CAPI}
    DSS.GlobalResult := FileName;
{$ENDIF}
    FireOffEditor(DSS, FileName);
end;

procedure TExecHelper.Set_Time;
// for interpreting time specified as an array "hour, sec"
var
    TimeArray: array[1..2] of Double;
begin
    DSS.Parser.ParseAsVector(2, pDoubleArray(@TimeArray));
    with DSS.ActiveCircuit.Solution do
    begin
        DynaVars.intHour := Round(TimeArray[1]);
        DynaVars.t := TimeArray[2];
        Update_dblHour;
    end;
end;

procedure TExecHelper.SetActiveCircuit(const cktname: String);
var
    pCkt: TDSSCircuit;
begin
    for pCkt in DSS.Circuits do
    begin
        if AnsiCompareText(pCkt.Name, cktname) = 0 then
        begin
            DSS.ActiveCircuit := pCkt;
            Exit;
        end;
    end;

   // IF none is found, just leave as is after giving error

    DoSimpleMsg(DSS, 'Error! No circuit named "%s" found. Active circuit not changed.', [cktname], 258);
end;


procedure TExecHelper.DoLegalVoltageBases;
var
    Dummy: pDoubleArray;
    i,
    Num: Integer;
begin
    Dummy := AllocMem(Sizeof(Double) * 1000); // Big Buffer
    Num := DSS.Parser.ParseAsVector(1000, Dummy);
     // Parsing zero-fills the array

    with DSS.ActiveCircuit do
    begin
        SetLength(LegalVoltageBases, Num);
        for i := 1 to Num do
            LegalVoltageBases[i - 1] := Dummy^[i];
    end;

    Reallocmem(Dummy, 0);
end;

function TExecHelper.DoOpenCmd: Integer;
// Opens a terminal and conductor of a ckt Element
var
    retval: Integer;
    Terminal: Integer;
    Conductor: Integer;

// syntax:  "Open class.name term=xx cond=xx"
//  IF cond is omitted, all conductors are opened.
begin
    retval := SetActiveCktElement;
    if retval > 0 then
    begin
        DSS.Parser.NextParam;
        Terminal := DSS.Parser.IntValue;
        DSS.Parser.NextParam;
        Conductor := DSS.Parser.IntValue;

        with DSS.ActiveCircuit do
        begin
            ActiveCktElement.ActiveTerminalIdx := Terminal;
            ActiveCktElement.Closed[Conductor] := FALSE;
            with ActiveCktElement do
                SetActiveBus(DSS, StripExtension(Getbus(ActiveTerminalIdx)));
        end;
    end
    else
    begin
        DoSimpleMsg(DSS, 'Error in Open Command: Circuit Element not found. %s', [CRLF + DSS.Parser.CmdString], 259);
    end;
    Result := 0;
end;

function TExecHelper.DoCloseCmd: Integer;
// Closes a terminal and conductor of a ckt Element
var
    retval: Integer;
    Terminal: Integer;
    Conductor: Integer;

// syntax:  "Close class.name term=xx cond=xx"
//  IF cond is omitted, all conductors are opened
begin
    retval := SetActiveCktElement;
    if retval > 0 then
    begin
        DSS.Parser.NextParam;
        Terminal := DSS.Parser.IntValue;
        DSS.Parser.NextParam;
        Conductor := DSS.Parser.IntValue;

        with DSS.ActiveCircuit do
        begin
            ActiveCktElement.ActiveTerminalIdx := Terminal;
            ActiveCktElement.Closed[Conductor] := TRUE;
            with ActiveCktElement do
                SetActiveBus(DSS, StripExtension(Getbus(ActiveTerminalIdx)));
        end;

    end
    else
    begin
        DoSimpleMsg(DSS, 'Error in Close Command: Circuit Element not found. %s', [CRLF + DSS.Parser.CmdString], 260);
    end;
    Result := 0;
end;

function TExecHelper.DoResetCmd: Integer;
var
    Param: String;
begin
    Result := 0;
    // Get next parm and try to interpret as a file name
    DSS.Parser.NextParam;
    Param := AnsiUpperCase(DSS.Parser.StrValue);
    if Length(Param) = 0 then
    begin
        DoResetMonitors;
        DoResetMeters;
        DoResetFaults(DSS);
        DoResetControls(DSS);
        ClearEventLog(DSS);
        ClearErrorLog(DSS);
        DoResetKeepList(DSS);
    end
    else
        case Param[1] of
            'M':
                case Param[2] of
                    'O'{MOnitor}:
                        DoResetMonitors;
                    'E'{MEter}:
                        DoResetMeters;
                end;
            'F'{Faults}:
                DoResetFaults(DSS);
            'C'{Controls}:
                DoResetControls(DSS);
            'E'{EventLog and ErrorLog}:
            begin
                ClearEventLog(DSS);
                ClearErrorLog(DSS);
            end;
            'K':
                DoResetKeepList(DSS);

        else

            DoSimpleMsg(DSS, 'Unknown argument to Reset Command: "%s"', [Param], 261);

        end;
end;

procedure TExecHelper.MarkCapandReactorBuses;
var
    pClass: TDSSClass;
    pCapElement: TCapacitorObj;
    pReacElement: TReactorObj;
begin
    // Mark all buses as keepers if there are capacitors or reactors on them
    pClass := GetDSSClassPtr(DSS, 'capacitor');
    if pClass <> NIL then
    begin
        for pCapElement in pClass do
        begin
            if pCapElement.IsShunt then
            begin
                if pCapElement.Enabled then
                    DSS.ActiveCircuit.Buses[pCapElement.Terminals[0].Busref].Keep := TRUE;
            end;
        end;
    end;

    // Now Get the Reactors
    pClass := GetDSSClassPtr(DSS, 'reactor');
    if pClass <> NIL then
    begin
        for pReacElement in pClass do
        begin
            if pReacElement.IsShunt then
                try
                    if pReacElement.Enabled then
                        DSS.ActiveCircuit.Buses[pReacElement.Terminals[0].Busref].Keep := TRUE;
                except
                    On E: Exception do
                    begin
                        DoSimpleMsg(DSS, '%s %s Reactor=%s Bus No.=%d ', [E.Message, CRLF, pReacElement.Name, pReacElement.NodeRef[1]], 9999);
                        Break;
                    end;
                end;
        end;
    end;
end;

function TExecHelper.DoReduceCmd: Integer;
var
    MetObj: TEnergyMeterObj;
    MeterClass: TEnergyMeter;
    Param: String;
    DevClassIndex: Integer;

begin
    Result := 0;
    // Get next parm and try to interpret as a file name
    DSS.Parser.NextParam;
    Param := AnsiUpperCase(DSS.Parser.StrValue);

    // Mark Capacitor and Reactor buses as Keep so we don't lose them
    MarkCapandReactorBuses;

    if (DSS.ActiveCircuit.EnergyMeters.Count = 0) then
    begin
        Result := 1890;
        DoSimpleMsg(DSS, _('An energy meter is required to use this feature. Please check https://sourceforge.net/p/electricdss/code/HEAD/tree/trunk/Version8/Doc/Circuit%20Reduction%20for%20Version8.docx for examples.'), 1890);
        Exit;
    end;

    if Length(Param) = 0 then
        Param := 'A';
    case Param[1] of
        'A':
        begin
            for MetObj in DSS.ActiveCircuit.EnergyMeters do
            begin
                MetObj.ReduceZone;
            end;
        end;

    else
       // Reduce a specific meter
        DevClassIndex := DSS.ClassNames.Find('energymeter');
        if DevClassIndex > 0 then
        begin
            MeterClass := DSS.DSSClassList.Get(DevClassIndex);
            if MeterClass.SetActive(Param) then   // Try to set it active
            begin
                MetObj := MeterClass.GetActiveObj;
                MetObj.ReduceZone;
            end
            else
                DoSimpleMsg(DSS, 'EnergyMeter "%s" not found.', [Param], 262);
        end;
    end;
end;

function TExecHelper.DoResetMonitors: Integer;
var
    pMon: TMonitorObj;

begin
    with DSS.ActiveCircuit do
    begin
        for pMon in Monitors do
        begin
            pMon.ResetIt;
        end;
        Result := 0;

    end;
end;

function TExecHelper.DoFileEditCmd: Integer;
var
    Param: String;
begin
    Result := 0;
    // Get next parm and try to interpret as a file name
    DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;

    if FileExists(Param) then
        FireOffEditor(DSS, Param)
    else
    begin
        DSS.GlobalResult := Format(_('File "%s" does not exist.'), [param]);
        Result := 1;
    end;
end;

procedure TExecHelper.ParseObjName(const fullname: String; var objname, propname: String);
// Parse strings such as
//
//    1. Classname.Objectname,Property    (full name)
//    2. Objectname.Property   (classname omitted)
//    3. Property           (classname and objectname omitted
//
var
    DotPos1, DotPos2: Integer;
begin
    DotPos1 := Pos('.', fullname);
    case Dotpos1 of

        0:
        begin
            Objname := '';
            PropName := FullName;
        end;

    else
    begin
        PropName := Copy(FullName, Dotpos1 + 1, (Length(FullName) - DotPos1));
        DotPos2 := Pos('.', PropName);
        case DotPos2 of

            0:
            begin
                ObjName := Copy(FullName, 1, DotPos1 - 1);
            end;
        else
        begin
            ObjName := Copy(FullName, 1, Dotpos1 + DotPos2 - 1);
            PropName := Copy(PropName, Dotpos2 + 1, (Length(PropName) - DotPos2));
        end;

        end;

    end;
    end;
end;

function TExecHelper.DoQueryCmd: Integer;
// ? Command
// Syntax:  ? Line.Line1.R1
var
    Param, ObjName, PropName: String;
    PropIndex: Integer;
begin
    Result := 0;
    DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;

    ParseObjName(Param, ObjName, PropName);

    if CompareText(ObjName, 'solution') = 0 then
    begin  // special for solution
         // DSS.ActiveDSSClass  := DSS.SolutionClass;
         //DSS.ActiveDSSObject := DSS.ActiveCircuit.Solution;
        DSS.GlobalPropertyValue := 'Property Unknown';
    end
    else
    begin
         // Set Object Active
        DSS.Parser.cmdstring := '"' + Objname + '"';
        DoSelectCmd;
          // Put property value in global VARiable
        PropIndex := DSS.ActiveDSSClass.Propertyindex(PropName);
        if PropIndex > 0 then
            DSS.GlobalPropertyValue := DSS.ActiveDSSObject.GetPropertyValue(PropIndex)
        else
            DSS.GlobalPropertyValue := 'Property Unknown';
    end;

    DSS.GlobalResult := DSS.GlobalPropertyValue;

    if DSS.LogQueries then
        WriteQueryLogFile(DSS, param, DSS.GlobalResult); // write time-stamped query

end;

function TExecHelper.DoResetMeters: Integer;
begin
    Result := 0;
    DSS.EnergyMeterClass.ResetAll
end;


function TExecHelper.DoNextCmd: Integer;
var
    Param: String;
begin
    Result := 0;

    // Get next parm and try to interpret as a file name
    DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;

    with DSS.ActiveCircuit.Solution do
        case UpCase(Param[1]) of

            'Y'{Year}:
                Year := Year + 1;
            'H'{Hour}:
                Inc(DynaVars.intHour);
            'T'{Time}:
                Increment_time;
        else

        end;
end;

procedure TExecHelper.DoAboutBox;
begin
    if NoFormsAllowed then
        Exit;
    ShowAboutBox;
end;

function TExecHelper.DoSetVoltageBases: Integer;
begin
    Result := 0;
    DSS.ActiveCircuit.Solution.SetVoltageBases;
end;

function TExecHelper.AddObject(const ObjType, Name: String): Integer;
var
    Obj: TDSSObject = NIL;
begin
    Result := 0;

    // Search for class IF not already active
    // IF nothing specified, LastClassReferenced remains
    if CompareText(Objtype, DSS.ActiveDSSClass.Name) <> 0 then
        DSS.LastClassReferenced := DSS.ClassNames.Find(ObjType);

    if DSS.LastClassReferenced = 0 then
    begin
        DoSimpleMsg(DSS, 'New Command: Object Type "%s" not found. %s', [ObjType, CRLF + DSS.Parser.CmdString], 263);
        Result := 0;
        Exit;
    end;


    // intrinsic and user Defined models
    // Make a new circuit element
    DSS.ActiveDSSClass := DSS.DSSClassList.Get(DSS.LastClassReferenced);

    // Name must be supplied
    if Length(Name) = 0 then
    begin
        DoSimpleMsg(DSS, 'Object Name Missing %s', [CRLF + DSS.Parser.CmdString], 264);
        Exit;
    end;

    // now let's make a new object or set an existing one active, whatever the case
    if DSS.ActiveDSSClass.DSSClassType = DSS_OBJECT then
    begin
        if (DSS.ActiveCircuit = NIL) and ((DSS.ActiveDSSClass = DSS.LineCodeClass) or (DSS.ActiveDSSClass = DSS.LineGeometryClass)) then
        begin
            DoSimpleMsg(DSS, _('You Must Create a circuit first: "new circuit.yourcktname"'), 279);
            Exit;
        end;

        // These can be added WITHout having an active circuit
        // Duplicates not allowed in general DSS objects;
        if not DSS.ActiveDSSClass.SetActive(Name) then
        begin
            Obj := DSS.ActiveDSSClass.NewObject(Name, TRUE, Result);
            DSS.DSSObjs.Add(Obj);  // Stick in pointer list to keep track of it
        end;
    end
    else
    begin
        // These are circuit elements
        if DSS.ActiveCircuit = NIL then
        begin
            DoSimpleMsg(DSS, _('You Must Create a circuit first: "new circuit.yourcktname"'), 265);
            Exit;
        end;

        // IF Object already exists.  Treat as an Edit IF dulicates not allowed
        if DSS.ActiveCircuit.DuplicatesAllowed then
        begin
            Obj := DSS.ActiveDSSClass.NewObject(Name, TRUE, Result); // Returns index into this class
            DSS.ActiveCircuit.AddCktElement(TDSSCktElement(Obj));   // Adds active object to active circuit
        end
        else
        begin // Check to see if we can set it active first
            if not DSS.ActiveDSSClass.SetActive(Name) then
            begin
                Obj := DSS.ActiveDSSClass.NewObject(Name, TRUE, Result);   // Returns index into this class
                DSS.ActiveCircuit.AddCktElement(TDSSCktElement(Obj));   // Adds active object to active circuit
            end
            else
            begin
                DoSimpleMsg(DSS, 'Warning: Duplicate new element definition: "%s.%s". Element being redefined.', [DSS.ActiveDSSClass.Name, Name], 266);
                Exit;
            end;
        end;

    end;
    DSS.ActiveDSSClass.Edit(DSS.Parser);    // Process remaining instructions on the command line
end;


function TExecHelper.EditObject(const ObjType, Name: String): Integer;
begin
    Result := 0;
    DSS.LastClassReferenced := DSS.ClassNames.Find(ObjType);

    if DSS.LastClassReferenced = 0 then
    begin
        DoSimpleMsg(DSS, 'Edit Command: Object Type "%s" not found. %s', [ObjType, CRLF + DSS.Parser.CmdString], 267);
        Result := 0;
        Exit;
    end;

    // intrinsic and user Defined models
    // Edit the DSS object
    DSS.ActiveDSSClass := DSS.DSSClassList.Get(DSS.LastClassReferenced);
    if DSS.ActiveDSSClass.SetActive(Name) then
    begin
        Result := DSS.ActiveDSSClass.Edit(DSS.Parser);   // Edit the active object
    end;
end;

function TExecHelper.DoSetkVBase: Integer;
var
    ParamName, BusName: String;
    kVValue: Double;
begin
   // Parse off next two items on line
    ParamName := DSS.Parser.NextParam;
    BusName := AnsiLowerCase(DSS.Parser.StrValue);

    ParamName := DSS.Parser.NextParam;
    kVValue := DSS.Parser.DblValue;

   // Now find the bus and set the value

    with DSS.ActiveCircuit do
    begin
        ActiveBusIndex := BusList.Find(BusName);

        if ActiveBusIndex > 0 then
        begin
            if Comparetext(ParamName, 'kvln') = 0 then
                Buses[ActiveBusIndex].kVBase := kVValue
            else
                Buses[ActiveBusIndex].kVBase := kVValue / SQRT3;
            Result := 0;
            Solution.VoltageBaseChanged := TRUE;
           // Solution.SolutionInitialized := FALSE;  // Force reinitialization
        end
        else
        begin
            Result := 1;
            AppendGlobalResult(DSS, 'Bus ' + BusName + ' not found.');
        end;
    end;
end;


procedure TExecHelper.DoAutoAddBusList(const S: String);
var
    ParmName,
    Param, S2: String;
    F: TStream = NIL;
begin
    DSS.ActiveCircuit.AutoAddBusList.Clear;

     // Load up auxiliary parser to reparse the array list or file name
    DSS.AuxParser.CmdString := S;
    ParmName := DSS.AuxParser.NextParam;
    Param := DSS.AuxParser.StrValue;

     // Syntax can be either a list of bus names or a file specification:  File= ...

    if CompareText(Parmname, 'file') = 0 then
    begin
         // load the list from a file
        try
            F := DSS.GetROFileStream(Param);
            while (F.Position + 1) < F.Size do
            begin
                FSReadln(F, S2);
                DSS.AuxParser.CmdString := S2;
                ParmName := DSS.AuxParser.NextParam;
                Param := DSS.AuxParser.StrValue;
                if Length(Param) > 0 then
                    DSS.ActiveCircuit.AutoAddBusList.Add(Param);
            end;
            FreeAndNil(F);

        except
            On E: Exception do
                DoSimpleMsg(DSS, 'Error trying to read bus list file: %s', [E.message], 268);
        end;
    end
    else
    begin
       // Parse bus names off of array list
        while Length(Param) > 0 do
        begin
            DSS.ActiveCircuit.AutoAddBusList.Add(Param);
            DSS.AuxParser.NextParam;
            Param := DSS.AuxParser.StrValue;
        end;

    end;
end;

procedure TExecHelper.DoKeeperBusList(const S: String);
// Set Keep flag on buses found in list so they aren't eliminated by some reduction
// algorithm.  This command is cumulative. To clear flag, use Reset Keeplist
var
    ParmName,
    Param, S2: String;
    F: TStream = NIL;
    iBus: Integer;
begin
     // Load up auxiliary parser to reparse the array list or file name
    DSS.AuxParser.CmdString := S;
    ParmName := DSS.AuxParser.NextParam;
    Param := DSS.AuxParser.StrValue;

     // Syntax can be either a list of bus names or a file specification:  File= ...
    if CompareText(Parmname, 'file') = 0 then
    begin
         // load the list from a file
        try
            F := DSS.GetROFileStream(Param);
            while (F.Position + 1) < F.Size do
            begin         // Fixed 7/8/01 to handle all sorts of bus names
                FSReadln(F, S2);
                DSS.AuxParser.CmdString := S2;
                ParmName := DSS.AuxParser.NextParam;
                Param := DSS.AuxParser.StrValue;
                if Length(Param) > 0 then
                    with DSS.ActiveCircuit do
                    begin
                        iBus := BusList.Find(Param);
                        if iBus > 0 then
                            Buses[iBus].Keep := TRUE;
                    end;
            end;
            FreeAndNil(F);

        except
            On E: Exception do
                DoSimpleMsg(DSS, 'Error trying to read bus list file "%s": %s', [param, E.message], 269);
        end;


    end
    else
    begin
       // Parse bus names off of array list
        while Length(Param) > 0 do
        begin
            with DSS.ActiveCircuit do
            begin
                iBus := BusList.Find(Param);
                if iBus > 0 then
                    Buses[iBus].Keep := TRUE;
            end;

            DSS.AuxParser.NextParam;
            Param := DSS.AuxParser.StrValue;
        end;

    end;
end;

function TExecHelper.DocktlossesCmd: Integer;
var
    LossValue: complex;
begin
    Result := 0;
    if DSS.ActiveCircuit <> NIL then
    begin
        DSS.GlobalResult := '';
        LossValue := DSS.ActiveCircuit.Losses;
        DSS.GlobalResult := Format('%10.5g, %10.5g', [LossValue.re * 0.001, LossValue.im * 0.001]);
    end
    else
        DSS.GlobalResult := 'No Active Circuit.';
end;

function TExecHelper.DocurrentsCmd: Integer;
var
    cBuffer: pComplexArray;
    NValues, i: Integer;

begin
    Result := 0;

    if DSS.ActiveCircuit <> NIL then
        with DSS.ActiveCircuit.ActiveCktElement do
        begin
            NValues := NConds * Nterms;
            DSS.GlobalResult := '';
            cBuffer := Allocmem(sizeof(Complex) * NValues);
            GetCurrents(cBuffer);
            for i := 1 to NValues do
            begin
                DSS.GlobalResult := DSS.GlobalResult + Format('%10.5g, %6.1f,', [cabs(cBuffer[i]), Cdang(cBuffer[i])]);
            end;
            Reallocmem(cBuffer, 0);
        end
    else
        DSS.GlobalResult := 'No Active Circuit.';
end;

function TExecHelper.DoNodeListCmd: Integer;
var
    NValues, i: Integer;
    CktElementName: String;
begin
    Result := 0;

    if DSS.ActiveCircuit <> NIL then
    begin
        DSS.Parser.NextParam;
        CktElementName := DSS.Parser.StrValue;

        if Length(CktElementName) > 0 then
            SetObject(DSS, CktElementName);

        if Assigned(DSS.ActiveCircuit.ActiveCktElement) then
            with DSS.ActiveCircuit.ActiveCktElement do
            begin
                NValues := NConds * Nterms;
                DSS.GlobalResult := '';
                for i := 1 to NValues do
                begin
                    DSS.GlobalResult := DSS.GlobalResult + Format('%d, ', [GetNodeNum(DSS, NodeRef[i])]);
                end;
            end
        else
            DSS.GlobalResult := 'No Active Circuit.';
    end;
end;


function TExecHelper.DolossesCmd: Integer;
var
    LossValue: complex;
begin
    Result := 0;
    if DSS.ActiveCircuit <> NIL then
        with DSS.ActiveCircuit do
        begin
            if ActiveCktElement <> NIL then
            begin
                DSS.GlobalResult := '';
                LossValue := ActiveCktElement.Losses;
                DSS.GlobalResult := Format('%10.5g, %10.5g', [LossValue.re * 0.001, LossValue.im * 0.001]);
            end;
        end
    else
        DSS.GlobalResult := 'No Active Circuit.';
end;

function TExecHelper.DophaselossesCmd: Integer;

// Returns Phase losses in kW, kVar

var
    cBuffer: pComplexArray;
    NValues, i: Integer;

begin
    Result := 0;

    if DSS.ActiveCircuit <> NIL then

        with DSS.ActiveCircuit.ActiveCktElement do
        begin
            NValues := NPhases;
            cBuffer := Allocmem(sizeof(Complex) * NValues);
            DSS.GlobalResult := '';
            GetPhaseLosses(NValues, cBuffer);
            for i := 1 to NValues do
            begin
                DSS.GlobalResult := DSS.GlobalResult + Format('%10.5g, %10.5g,', [cBuffer[i].re * 0.001, cBuffer[i].im * 0.001]);
            end;
            Reallocmem(cBuffer, 0);
        end
    else
        DSS.GlobalResult := 'No Active Circuit.'


end;

function TExecHelper.DopowersCmd(Total: Integer): Integer;
var
    cBuffer: pComplexArray;
    NValues,
    myInit,
    myEnd,
    j,
    i: Integer;
    myBuffer: array of Complex;

begin
  // If Total = 0, returns the powers per phase
  // If Total = 1, returns the power sum at each terminal

    Result := 0;
    if DSS.ActiveCircuit <> NIL then
        with DSS.ActiveCircuit.ActiveCktElement do
        begin
            NValues := NConds * Nterms;
            DSS.GlobalResult := '';
            cBuffer := Allocmem(sizeof(Complex) * NValues);
            GetPhasePower(cBuffer);
            if Total = 0 then
            begin
                for i := 1 to NValues do
                begin
                    DSS.GlobalResult := DSS.GlobalResult + Format('%10.5g, %10.5g,', [cBuffer[i].re * 0.001, cBuffer[i].im * 0.001]);
                end;
            end
            else
            begin
                setlength(myBuffer, Nterms);
                for j := 1 to Nterms do
                begin
                    myBuffer[j - 1] := 0;
                    myInit := (j - 1) * NConds + 1;
                    myEnd := NConds * j;
                    for i := myInit to myEnd do
                    begin
                        myBuffer[j - 1] := myBuffer[j - 1] + cBuffer[i];
                    end;
                    DSS.GlobalResult := DSS.GlobalResult + Format('%10.5g, %10.5g,', [myBuffer[j - 1].re * 0.001, myBuffer[j - 1].im * 0.001]);
                end;
            end;
            Reallocmem(cBuffer, 0);
        end
    else
        DSS.GlobalResult := 'No Active Circuit';
end;

function TExecHelper.DoseqcurrentsCmd: Integer;
// All sequence currents of active ciruit element
// returns magnitude only.

var
    Nvalues, i, j, k: Integer;
    IPh, I012: array[1..3] of Complex;
    cBuffer: pComplexArray;

begin
    Result := 0;
    if DSS.ActiveCircuit <> NIL then
        with DSS.ActiveCircuit do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    DSS.GlobalResult := '';
                    if Nphases < 3 then
                        for i := 0 to 3 * Nterms - 1 do
                            DSS.GlobalResult := DSS.GlobalResult + ' -1.0,'  // Signify n/A
                    else
                    begin
                        NValues := NConds * Nterms;
                        cBuffer := Allocmem(sizeof(Complex) * NValues);
                        GetCurrents(cBuffer);
                        for j := 1 to Nterms do
                        begin
                            k := (j - 1) * NConds;
                            for i := 1 to 3 do
                            begin
                                Iph[i] := cBuffer[k + i];
                            end;
                            Phase2SymComp(pComplexArray(@Iph), pComplexArray(@I012));
                            for i := 1 to 3 do
                            begin
                                DSS.GlobalResult := DSS.GlobalResult + Format('%10.5g, ', [Cabs(I012[i])]);
                            end;
                        end;
                        Reallocmem(cBuffer, 0);
                    end; // ELSE
                end; // WITH ActiveCktElement
        end // IF/WITH DSS.ActiveCircuit
    else
        DSS.GlobalResult := 'No Active Circuit';
end;

function TExecHelper.DoSeqpowersCmd: Integer;
// All seq Powers of active 3-phase ciruit element
// returns kW + j kvar

var
    Nvalues, i, j, k: Integer;
    S: Complex;
    VPh, V012: array[1..3] of Complex;
    IPh, I012: array[1..3] of Complex;
    cBuffer: pComplexArray;

begin
    Result := 0;
    if DSS.ActiveCircuit <> NIL then
        with DSS.ActiveCircuit do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    DSS.GlobalResult := '';
                    if NPhases < 3 then
                        for i := 0 to 2 * 3 * Nterms - 1 do
                            DSS.GlobalResult := DSS.GlobalResult + '-1.0, '  // Signify n/A
                    else
                    begin
                        NValues := NConds * Nterms;
                        cBuffer := Allocmem(sizeof(Complex) * NValues);
                        GetCurrents(cBuffer);
                        for j := 1 to Nterms do
                        begin
                            k := (j - 1) * NConds;
                            for i := 1 to 3 do
                            begin
                                Vph[i] := Solution.NodeV[Terminals[j - 1].TermNodeRef[i - 1]];
                            end;
                            for i := 1 to 3 do
                            begin
                                Iph[i] := cBuffer[k + i];
                            end;
                            Phase2SymComp(pComplexArray(@Iph), pComplexArray(@I012));
                            Phase2SymComp(pComplexArray(@Vph), pComplexArray(@V012));
                            for i := 1 to 3 do
                            begin
                                S := V012[i] * cong(I012[i]);
                                DSS.GlobalResult := DSS.GlobalResult + Format('%10.5g, %10.5g,', [S.re * 0.003, S.im * 0.003]); // 3-phase kW conversion
                            end;
                        end;
                    end;
                    Reallocmem(cBuffer, 0);
                end;
        end
    else
        DSS.GlobalResult := 'No Active Circuit';
end;

function TExecHelper.DoseqvoltagesCmd: Integer;

// All voltages of active ciruit element
// magnitude only
// returns a set of seq voltages (3) for each terminal

var
    Nvalues, i, j, k, n: Integer;
    VPh, V012: array[1..3] of Complex;
    S: String;

begin
    Result := 0;
    Nvalues := -1; // unassigned, for exception message
    n := -1; // unassigned, for exception message
    if DSS.ActiveCircuit <> NIL then
        with DSS.ActiveCircuit do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                    if Enabled then
                    begin
                        try
                            Nvalues := NPhases;
                            DSS.GlobalResult := '';
                            if Nvalues < 3 then
                                for i := 1 to 3 * Nterms do
                                    DSS.GlobalResult := DSS.GlobalResult + '-1.0, '  // Signify n/A
                            else
                            begin
                                for j := 1 to Nterms do
                                begin
                                    k := (j - 1) * NConds;
                                    for i := 1 to 3 do
                                    begin
                                        Vph[i] := Solution.NodeV[NodeRef[i + k]];
                                    end;
                                    Phase2SymComp(pComplexArray(@Vph), pComplexArray(@V012));   // Compute Symmetrical components

                                    for i := 1 to 3 do  // Stuff it in the result
                                    begin
                                        DSS.GlobalResult := DSS.GlobalResult + Format('%10.5g, ', [Cabs(V012[i])]);
                                    end;

                                end;
                            end;

                        except
                            On E: Exception do
                            begin
                                S := E.message + CRLF +
                                    'Element=' + ActiveCktElement.Name + CRLF +
                                    'Nvalues=' + IntToStr(NValues) + CRLF +
                                    'Nterms=' + IntToStr(Nterms) + CRLF +
                                    'NConds =' + IntToStr(NConds) + CRLF +
                                    'noderef=' + IntToStr(N);
                                DoSimpleMsg(S, 270);
                            end;
                        end;
                    end
                    else
                        DSS.GlobalResult := _('Element Disabled');  // Disabled

        end
    else
        DSS.GlobalResult := _('No Active Circuit');


end;

function TExecHelper.DovoltagesCmd(const PerUnit: Boolean): Integer;
// Bus Voltages at active terminal

var
    i: Integer;
    Volts: Complex;
    ActiveBus: TDSSBus;
    VMag: Double;

begin
    Result := 0;
    if DSS.ActiveCircuit <> NIL then
        with DSS.ActiveCircuit do
        begin
            if ActiveBusIndex <> 0 then
            begin
                ActiveBus := Buses[ActiveBusIndex];
                DSS.GlobalResult := '';
                for i := 1 to ActiveBus.NumNodesThisBus do
                begin
                    Volts := Solution.NodeV[ActiveBus.GetRef(i)];
                    Vmag := Cabs(Volts);
                    if PerUnit and (ActiveBus.kvbase > 0.0) then
                    begin
                        Vmag := Vmag * 0.001 / ActiveBus.kVBase;
                        DSS.GlobalResult := DSS.GlobalResult + Format('%10.5g, %6.1f, ', [Vmag, CDang(Volts)]);
                    end
                    else
                        DSS.GlobalResult := DSS.GlobalResult + Format('%10.5g, %6.1f, ', [Vmag, CDang(Volts)]);
                end;
            end
            else
                DSS.GlobalResult := 'No Active Bus.';
        end
    else
        DSS.GlobalResult := 'No Active Circuit.';
end;

function TExecHelper.DoZscCmd(Zmatrix: Boolean): Integer;
// Bus Short Circuit matrix

var
    i, j: Integer;
    ActiveBus: TDSSBus;
    Z: Complex;

begin
    Result := 0;
    if DSS.ActiveCircuit <> NIL then
        with DSS.ActiveCircuit do
        begin
            if ActiveBusIndex <> 0 then
            begin
                ActiveBus := Buses[ActiveBusIndex];
                DSS.GlobalResult := '';
                if not assigned(ActiveBus.Zsc) then
                    Exit;
                with ActiveBus do
                    for i := 1 to NumNodesThisBus do
                    begin
                        for j := 1 to NumNodesThisBus do
                        begin
                            if ZMatrix then
                                Z := Zsc[i, j]
                            else
                                Z := Ysc[i, j];
                            DSS.GlobalResult := DSS.GlobalResult + Format('%-.5g, %-.5g,   ', [Z.re, Z.im]);

                        end;

                    end;
            end
            else
                DSS.GlobalResult := 'No Active Bus.';
        end
    else
        DSS.GlobalResult := 'No Active Circuit.';
end;

function TExecHelper.DoZsc012Cmd: Integer;
// Bus Short Circuit matrix
var
    // i: Integer;
    ActiveBus: TDSSBus;
    Z0, Z1, Z2: Complex;
    // Temp1, Temp2: pComplexArray;
    Zsc012Temp: TcMatrix;
begin
    Result := 0;
    if DSS.ActiveCircuit = NIL then
    begin
        DSS.GlobalResult := 'No Active Circuit.';
        Exit;
    end;
    if (DSS.ActiveCircuit.ActiveBusIndex <= 0) or (DSS.ActiveCircuit.ActiveBusIndex > DSS.ActiveCircuit.NumBuses) then
    begin
        DSS.GlobalResult := 'No Active Bus.';
        Exit;
    end;

    ActiveBus := DSS.ActiveCircuit.Buses[DSS.ActiveCircuit.ActiveBusIndex];
    DSS.GlobalResult := '';
    if not assigned(ActiveBus.Zsc) then
        Exit;

    if ActiveBus.NumNodesThisBus <> 3 then
    begin
        DSS.GlobalResult := 'Not a 3-phase bus. Cannot compute Symmetrical Component matrix.';
        Exit;
    end;

    with ActiveBus do
    begin
        // Compute ZSC012 for 3-phase buses else leave it zeros
        // ZSC012 = Ap2s Zsc As2p
        Zsc012Temp := Zsc.MtrxMult(As2p);  // temp for intermediate result
        if Assigned(ZSC012) then
            ZSC012.Free;
        ZSC012 := Ap2s.MtrxMult(Zsc012Temp);
        // Cleanup
        Zsc012Temp.Free;
        // Just return diagonal elements only
        Z0 := Zsc012[1, 1];
        Z1 := Zsc012[2, 2];
        Z2 := Zsc012[3, 3];
        DSS.GlobalResult := DSS.GlobalResult + Format('Z0, (%-.5g, +j %-.5g), ', [Z0.re, Z0.im]) + CRLF;
        DSS.GlobalResult := DSS.GlobalResult + Format('Z1, (%-.5g, +j %-.5g), ', [Z1.re, Z1.im]) + CRLF;
        DSS.GlobalResult := DSS.GlobalResult + Format('Z2, (%-.5g, +j %-.5g), ', [Z2.re, Z2.im]);
    end;
end;

function TExecHelper.DoZsc10Cmd: Integer;
// Bus Short Circuit matrix
var
    ActiveBus: TDSSBus;
    Z: Complex;
begin
    Result := 0;
    if DSS.ActiveCircuit <> NIL then
        with DSS.ActiveCircuit do
        begin
            if ActiveBusIndex <> 0 then
            begin
                ActiveBus := Buses[ActiveBusIndex];
                DSS.GlobalResult := '';
                if not assigned(ActiveBus.Zsc) then
                    Exit;
                with ActiveBus do
                begin
                    Z := Zsc1;
                    DSS.GlobalResult := DSS.GlobalResult + Format('Z1, %-.5g, %-.5g, ', [Z.re, Z.im]) + CRLF;

                    Z := Zsc0;
                    DSS.GlobalResult := DSS.GlobalResult + Format('Z0, %-.5g, %-.5g, ', [Z.re, Z.im]);
                end;

            end
            else
                DSS.GlobalResult := 'No Active Bus.';
        end
    else
        DSS.GlobalResult := 'No Active Circuit.';
end;


function TExecHelper.DoAllocateLoadsCmd: Integer;
// Requires an EnergyMeter Object at the head of the feeder
// Adjusts loads defined by connected kVA or kWh billing
var
    pMeter: TEnergyMeterObj;
    pSensor: TSensorObj;
    iterCount: Integer;

begin
    Result := 0;
    with DSS.ActiveCircuit do
    begin
        LoadMultiplier := 1.0;   // Property .. has side effects
        with Solution do
        begin
            if Mode <> TSolveMode.SNAPSHOT then
                Mode := TSolveMode.SNAPSHOT;   // Resets meters, etc. if not in snapshot mode
            Solve;  // Make guess based on present allocationfactors
        end;

         // Allocation loop -- make MaxAllocationIterations iterations
        for iterCount := 1 to DSS.MaxAllocationIterations do
        begin
           // Do EnergyMeters
            for pMeter in EnergyMeters do
            begin
                pMeter.CalcAllocationFactors;
            end;

           // Now do other Sensors
            for pSensor in Sensors do
            begin
                pSensor.CalcAllocationFactors;
            end;

            // Now let the EnergyMeters run down the circuit setting the loads
            for pMeter in EnergyMeters do
            begin
                pMeter.AllocateLoad;
            end;
            Solution.Solve;  {Update the solution}

        end;
    end;
end;

procedure TExecHelper.DoSetAllocationFactors(const X: Double);

var
    pLoad: TLoadObj;

begin
    if X <= 0.0 then
        DoSimpleMsg(DSS, _('Allocation Factor must be greater than zero.'), 271)
    else
        with DSS.ActiveCircuit do
        begin
            for pLoad in Loads do
            begin
                pLoad.Set_kVAAllocationFactor(X);
            end;
        end;
end;

procedure TExecHelper.DoSetCFactors(const X: Double);

var
    pLoad: TLoadObj;

begin
    if X <= 0.0 then
        DoSimpleMsg(DSS, _('CFactor must be greater than zero.'), 271)
    else
        with DSS.ActiveCircuit do
        begin
            for pLoad in Loads do
            begin
                pLoad.Set_CFactor(X);
            end;
        end;
end;

function TExecHelper.DoHarmonicsList(const S: String): Integer;

var
    Dummy: pDoubleArray;
    i,
    Num: Integer;

begin
    Result := 0;

    with DSS.ActiveCircuit.Solution do
        if CompareText(S, 'ALL') = 0 then
            DoAllHarmonics := TRUE
        else
        begin
            DoAllHarmonics := FALSE;

            Dummy := AllocMem(Sizeof(Double) * 100); // Big Buffer
            Num := DSS.Parser.ParseAsVector(100, Dummy);
       {Parsing zero-fills the array}

            HarmonicListSize := Num;
            Reallocmem(HarmonicList, SizeOf(HarmonicList^[1]) * HarmonicListSize);
            for i := 1 to HarmonicListSize do
                HarmonicList^[i] := Dummy^[i];

            Reallocmem(Dummy, 0);
        end;
end;


function TExecHelper.DoFormEditCmd: Integer;

begin
    Result := 0;
    if NoFormsAllowed then
        Exit;
    DoSelectCmd;  // Select ActiveObject
    if DSS.ActiveDSSObject <> NIL then
    begin
        ShowPropEditForm;

    end
    else
    begin
        DoSimpleMsg(DSS, _('Element not found.'), 272);
        Result := 1;
    end;
end;


function TExecHelper.DoMeterTotals: Integer;
var
    i: Integer;
begin
    Result := 0;
    if DSS.ActiveCircuit <> NIL then
    begin
        DSS.ActiveCircuit.TotalizeMeters;
        // Now export to global result
        for i := 1 to NumEMregisters do
        begin
            AppendGlobalResult(DSS, Format('%-.6g', [DSS.ActiveCircuit.RegisterTotals[i]]));
        end;
    end;
end;

function TExecHelper.DoCapacityCmd: Integer;

var
    ParamPointer: Integer;
    Param, ParamName: String;

begin
    Result := 0;

    ParamPointer := 0;
    ParamName := DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;
    while Length(Param) > 0 do
    begin
        if Length(ParamName) = 0 then
            Inc(ParamPointer)
        else
            case ParamName[1] of
                's':
                    ParamPointer := 1;
                'i':
                    ParamPointer := 2;
            else
                ParamPointer := 0;
            end;

        case ParamPointer of
            0:
                DoSimpleMsg(DSS, 'Unknown parameter "%s" for Capacity Command', [ParamName], 273);
            1:
                DSS.ActiveCircuit.CapacityStart := DSS.Parser.DblValue;
            2:
                DSS.ActiveCircuit.CapacityIncrement := DSS.Parser.DblValue;

        else

        end;

        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
    end;

    with DSS.ActiveCircuit do
        if ComputeCapacity then
        begin   // Totalizes EnergyMeters at End

            DSS.GlobalResult := Format('%-.6g', [(DSS.ActiveCircuit.RegisterTotals[3] + DSS.ActiveCircuit.RegisterTotals[19])]);  // Peak KW in Meters
            AppendGlobalResult(DSS, Format('%-.6g', [LoadMultiplier]));
        end;
end;

function TExecHelper.DoClassesCmd: Integer;

var
    i: Integer;
begin
    for i := 1 to DSS.NumIntrinsicClasses do
    begin
        AppendGlobalResult(DSS, TDSSClass(DSS.DSSClassList.Get(i)).Name);
    end;
    Result := 0;
end;

function TExecHelper.DoUserClassesCmd: Integer;
begin
    Result := 0;
    AppendGlobalResult(DSS, 'No User Classes Defined.');
end;

function TExecHelper.DoZscRefresh: Integer;

var
    j: Integer;

begin
    Result := 1;

    try

        with DSS.ActiveCircuit, DSS.ActiveCircuit.Solution do
        begin
            for j := 1 to NumNodes do
                Currents[j] := 0;  // Clear Currents array

            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                if not assigned(Buses[ActiveBusIndex].Zsc) then
                    Buses[ActiveBusIndex].AllocateBusQuantities;
                Solution.ComputeYsc(ActiveBusIndex);      // Compute YSC for active Bus
                Result := 0;
            end;
        end;

    except
        On E: Exception do
            DoSimpleMsg(DSS, 'ZscRefresh Error: %s', [E.message], 274);
    end;
end;

function TExecHelper.DoVarValuesCmd: Integer;
var
    i: Integer;
  // PcElem:TPCElement;
begin
    Result := 0;
    if DSS.ActiveCircuit <> NIL then
        with DSS.ActiveCircuit do
        begin
         // Check if PCElement
            case (ActiveCktElement.DSSObjType and BASECLASSMASK) of
                PC_ELEMENT:
                    with ActiveCktElement as TPCElement do
                    begin
                        for i := 1 to NumVariables do
                            AppendGlobalResult(DSS, Format('%-.6g', [Variable[i]]));
                    end;
            else
                AppendGlobalResult(DSS, 'Null');
            end;
        end;
end;

function TExecHelper.DoValVarCmd: Integer;
// Get value of specified variable by name of index
var
    ParamName, Param: String;
    VarIndex: Integer;
    PropIndex: Integer;
    PCElem: TPCElement;

begin
    Result := 0;

    // Check to make sure this is a PC Element. If not, return null string in global result

    if (DSS.ActiveCircuit.ActiveCktElement.DSSObjType and BASECLASSMASK) <> PC_ELEMENT then

        DSS.GlobalResult := ''

    else
    begin
        PCElem := DSS.ActiveCircuit.ActiveCktElement as TPCElement;

        // Get next parameter on command line

        ParamName := AnsiUpperCase(DSS.Parser.NextParam);
        Param := DSS.Parser.StrValue;

        PropIndex := 1;
        if Length(ParamName) > 0 then
            case ParamName[1] of
                'N':
                    PropIndex := 1;
                'I':
                    PropIndex := 2;
            end;

        VarIndex := 0;

        case PropIndex of
            1:
                VarIndex := PCElem.LookupVariable(Param);  // Look up property index
            2:
                VarIndex := DSS.Parser.IntValue;
        end;

        if (VarIndex > 0) and (VarIndex <= PCElem.NumVariables) then

            DSS.GlobalResult := Format('%.8g', [PCElem.Variable[VarIndex]])

        else
            DSS.GlobalResult := '';   {Invalid var name or index}

    end;
end;

function TExecHelper.DoVarNamesCmd: Integer;

var
    i: Integer;
begin
    Result := 0;
    if DSS.ActiveCircuit <> NIL then
        with DSS.ActiveCircuit do
        begin
         {Check if PCElement}
            case (ActiveCktElement.DSSObjType and BASECLASSMASK) of
                PC_ELEMENT:
                    with (ActiveCktElement as TPCElement) do
                    begin
                        for i := 1 to NumVariables do
                            AppendGlobalResult(DSS, VariableName(i));
                    end;
            else
                AppendGlobalResult(DSS, 'Null');
            end;
        end;
end;

function TExecHelper.DoBusCoordsCmd(SwapXY: Boolean): Integer;
// Format of File should be
//
//   Busname, x, y
//
//   (x, y are real values)
//
//   If SwapXY is true, x and y values are swapped
var
    strings: TStringList = NIL;
    FStream: TStream = NIL;
    Param,
    BusName: String;
    iB: Integer;
    iLine: Integer;
    stringIdx: Integer;
begin
    Result := 0;

    // Get next parameter on command line

    DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;

    try
        iLine := -1;
        try
            strings := TStringList.Create;
            iLine := 0;

            Fstream := DSS.GetROFileStream(Param);
            strings.LoadFromStream(Fstream);
            for stringIdx := 0 to (strings.Count - 1) do
            begin
                Inc(iLine);
                with DSS.AuxParser do
                begin      // User Auxparser to parse line
                    CmdString := strings[stringIdx];
                    NextParam;
                    BusName := StrValue;
                    iB := DSS.ActiveCircuit.Buslist.Find(BusName);
                    if iB > 0 then
                    begin
                        with DSS.ActiveCircuit.Buses[iB] do
                        begin     // Returns TBus object
                            NextParam;
                            if SwapXY then
                                y := DblValue
                            else
                                x := DblValue;
                            NextParam;
                            if SwapXY then
                                x := DblValue
                            else
                                y := DblValue;
                            CoordDefined := TRUE;
                        end;
                    end;
                end;
              // Else just ignore a bus that's not in the circuit
            end;


        except
          // **CHANGE THIS ERROR MESSAGE**
            ON E: Exception do
            begin
                if iLine = -1 then
                    DoSimpleMsg(DSS, 'Bus Coordinate file "%s" could not be read: %s', [Param, E.Message], 275)
                else
                    DoSimpleMsg(DSS, 'Bus Coordinate file: Error Reading Line %d; %s', [Iline, E.Message], 275);
            end;
        end;

    finally
        FreeAndNil(strings);
        FreeAndNil(FStream);
    end;
end;

function TExecHelper.DoMakePosSeq: Integer;
var
    CktElem: TDSSCktElement;
begin
    Result := 0;

    DSS.ActiveCircuit.PositiveSequence := TRUE;

    for CktElem in DSS.ActiveCircuit.CktElements do
    begin
        CktElem.MakePosSequence();
    end;
end;

procedure TExecHelper.DoSetReduceStrategy(const S: String);

    function AtLeast(i, j: Integer): Integer;
    begin
        if j < i then
            Result := i
        else
            Result := j;
    end;

begin
    DSS.ActiveCircuit.ReductionStrategyString := S;

    DSS.ActiveCircuit.ReductionStrategy := rsDefault;
    if Length(S) = 0 then
        Exit;  {No option given}

    DSS.AuxParser.CmdString := DSS.Parser.Remainder;  // so we don't mess up Set Command

    case AnsiUpperCase(S)[1] of

        'B':
            DSS.ActiveCircuit.ReductionStrategy := rsBreakLoop;
        'D':
            DSS.ActiveCircuit.ReductionStrategy := rsDefault;  {Default}
        'E':
            DSS.ActiveCircuit.ReductionStrategy := rsDangling;  {Ends}
        'L':
        begin {Laterals}
            DSS.ActiveCircuit.ReductionStrategy := rsLaterals;
        end;
        'M':
            DSS.ActiveCircuit.ReductionStrategy := rsMergeParallel;
       (*
       'T': Begin          removed 2-28-2018
              DSS.ActiveCircuit.ReductionStrategy := rsTapEnds;
              DSS.ActiveCircuit.ReductionMaxAngle := 15.0;  {default}
              If Length(param2) > 0 Then  DSS.ActiveCircuit.ReductionMaxAngle := DSS.AuxParser.DblValue;
            End;
            *)
        'S':
        begin  {Shortlines or Switch}
            if CompareTextShortest(S, 'SWITCH') = 0 then
            begin
                DSS.ActiveCircuit.ReductionStrategy := rsSwitches;
            end
            else
            begin
                DSS.ActiveCircuit.ReductionStrategy := rsShortlines;
                  { DSS.ActiveCircuit.ReductionZmag is now set in main ExecOptions     }
            end;
        end;
    else
        DoSimpleMsg(DSS, 'Unknown Reduction Strategy: "%s".', [S], 276);
    end;
end;

function TExecHelper.DoInterpolateCmd: Integer;

{Interpolate bus coordinates in meter zones}

var
    MetObj: TEnergyMeterObj;
    MeterClass: TEnergyMeter;
    Param: String;
    DevClassIndex: Integer;
    CktElem: TDSSCktElement;

begin
    Result := 0;

    DSS.Parser.NextParam;
    Param := AnsiUpperCase(DSS.Parser.StrValue);

    // initialize the Checked Flag FOR all circuit Elements
    with DSS.ActiveCircuit do
    begin
        for CktElem in CktElements do
        begin
            Exclude(CktElem.Flags, Flg.Checked);
        end;
    end;


    if Length(Param) = 0 then
        Param := 'A';
    case Param[1] of
        'A':
        begin
            for metobj in DSS.ActiveCircuit.EnergyMeters do
            begin
                MetObj.InterpolateCoordinates;
            end;
        end;

    else
       {Interpolate a specific meter}
        DevClassIndex := DSS.ClassNames.Find('energymeter');
        if DevClassIndex > 0 then
        begin
            MeterClass := DSS.DSSClassList.Get(DevClassIndex);
            if MeterClass.SetActive(Param) then   // Try to set it active
            begin
                MetObj := MeterClass.GetActiveObj;
                MetObj.InterpolateCoordinates;
            end
            else
                DoSimpleMsg(DSS, 'EnergyMeter "%s" not found.', [Param], 277);
        end;
    end;
end;

function TExecHelper.DoAlignFileCmd: Integer;
{Rewrites designated file, aligning the fields into columns}
var
    Param: String;

begin
    Result := 0;
    DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;


    if FileExists(Param) then
    begin
        if not RewriteAlignedFile(DSS, Param) then
            Result := 1;
    end
    else
    begin
        DoSimpleMsg(DSS, 'File "%s" does not exist.', [Param], 278);
        Result := 1;
    end;

    if Result = 0 then
        FireOffEditor(DSS, DSS.GlobalResult);
end; {DoAlignfileCmd}

procedure TExecHelper.DoSetNormal(pctNormal: Double);

var
    i: Integer;
    pLine: TLineObj;

begin
    if DSS.ActiveCircuit <> NIL then
    begin
        pctNormal := pctNormal * 0.01;  // local copy only
        for i := 1 to DSS.ActiveCircuit.Lines.Count do
        begin
            pLine := DSS.ActiveCircuit.Lines.Get(i);
            pLine.Normamps := pctNormal * pLine.EmergAmps;
        end;
    end;
end;

function TExecHelper.DoRotateCmd: Integer;
// rotate about the center of the coordinates
var
    i: Integer;
    Angle, xmin, xmax, ymin, ymax, xc, yc: Double;
    a, vector: Complex;
begin
    Result := 0;
    if DSS.ActiveCircuit <> NIL then
    begin
        DSS.Parser.NextParam;
        Angle := DSS.Parser.DblValue * PI / 180.0;   // Deg to rad

        a := cmplx(cos(Angle), Sin(Angle));
        with DSS.ActiveCircuit do
        begin
            Xmin := 1.0e50;
            Xmax := -1.0e50;
            Ymin := 1.0e50;
            Ymax := -1.0e50;
            for i := 1 to Numbuses do
            begin
                if Buses[i].CoordDefined then
                begin
                    with Buses[i] do
                    begin
                        Xmax := Max(Xmax, x);
                        XMin := Min(Xmin, x);
                        ymax := Max(ymax, y);
                        yMin := Min(ymin, y);
                    end;
                end;
            end;

            Xc := (Xmax + Xmin) / 2.0;
            Yc := (Ymax + Ymin) / 2.0;

            for i := 1 to Numbuses do
            begin
                if Buses[i].CoordDefined then
                begin
                    with Buses[i] do
                    begin
                        vector := cmplx(x - xc, y - yc);
                        Vector := Vector * a;
                        x := xc + vector.re;
                        y := yc + vector.im;
                    end;
                end;
            end;
        end;
    end;
end;


function TExecHelper.DoVDiffCmd: Integer;
var
    Fin: TBufferedFileStream = NIL;
    Fout: TFileStream = NIL;
    sout: String;
    BusName, Line: String;
    i, node, busIndex: Integer;
    Vmag, Diff: Double;
begin
    Result := 0;
    if FileExists(DSS.OutputDirectory {CurrentDSSDir} + DSS.CircuitName_ + 'SavedVoltages.txt') then
    begin
        try
            try
                Fin := TBufferedFileStream.Create(DSS.OutputDirectory {CurrentDSSDir} + DSS.CircuitName_ + 'SavedVoltages.txt', fmOpenRead or fmShareDenyWrite);
                Fout := TBufferedFileStream.Create(DSS.OutputDirectory {CurrentDSSDir} + DSS.CircuitName_ + 'VDIFF.txt', fmCreate);

                while (Fin.Position + 1) < Fin.Size do
                begin
                    FSReadln(Fin, Line);
                    DSS.AuxParser.CmdString := Line;
                    DSS.AuxParser.NextParam;
                    BusName := DSS.AuxParser.StrValue;
                    if Length(BusName) > 0 then
                    begin
                        BusIndex := DSS.ActiveCircuit.BusList.Find(BusName);
                        if BusIndex > 0 then
                        begin
                            DSS.AuxParser.Nextparam;
                            node := DSS.AuxParser.Intvalue;
                            with DSS.ActiveCircuit.Buses[BusIndex] do
                                for i := 1 to NumNodesThisBus do
                                begin
                                    if GetNum(i) = node then
                                    begin
                                        DSS.AuxParser.Nextparam;
                                        Vmag := DSS.AuxParser.Dblvalue;
                                        Diff := Cabs(DSS.ActiveCircuit.Solution.NodeV[GetRef(i)]) - Vmag;
                                        if Vmag <> 0.0 then
                                        begin
                                            WriteStr(sout, BusName, '.', node, ', ', (Diff / Vmag * 100.0): 7: 2, ', %');
                                            FSWriteln(Fout, sout);
                                        end
                                        else
                                        begin
                                            WriteStr(sout, BusName, '.', node, ', ', format('%-.5g', [Diff]), ', Volts');
                                            FSWriteln(Fout, sout);
                                        end;
                                    end;
                                end;
                        end;
                    end;
                end;

            except
                On E: Exception do
                begin
                    DoSimpleMsg(DSS, 'Error opening Saved Voltages or VDIFF File: %s', [E.message], 280);
                    Exit;
                end;
            end;

        finally
            FreeAndNil(Fin);
            FreeAndNil(Fout);
            FireOffEditor(DSS, DSS.OutputDirectory {CurrentDSSDir} + DSS.CircuitName_ + 'VDIFF.txt');
        end;
    end
    else
        DoSimpleMsg(DSS, _('Error: No Saved Voltages.'), 281);
end;

function TExecHelper.DoSummaryCmd: Integer;

// Returns summary in global result String

var
    S: String;
    cLosses,
    cPower: Complex;

begin
    Result := 0;
    S := '';
    if DSS.ActiveCircuit.Issolved then
        S := S + 'Status = SOLVED' + CRLF
    else
    begin
        S := S + 'Status = NOT Solved' + CRLF;
    end;
    S := S + 'Solution Mode = ' + DSS.SolveModeEnum.OrdinalToString(ord(DSS.ActiveCircuit.Solution.mode)) + CRLF;
    S := S + 'Number = ' + IntToStr(DSS.ActiveCircuit.Solution.NumberofTimes) + CRLF;
    S := S + 'Load Mult = ' + Format('%5.3f', [DSS.ActiveCircuit.LoadMultiplier]) + CRLF;
    S := S + 'Devices = ' + Format('%d', [DSS.ActiveCircuit.NumDevices]) + CRLF;
    S := S + 'Buses = ' + Format('%d', [DSS.ActiveCircuit.NumBuses]) + CRLF;
    S := S + 'Nodes = ' + Format('%d', [DSS.ActiveCircuit.NumNodes]) + CRLF;
    S := S + 'Control Mode =' + DSS.ControlModeEnum.OrdinalToString(DSS.ActiveCircuit.Solution.Controlmode) + CRLF;
    S := S + 'Total Iterations = ' + IntToStr(DSS.ActiveCircuit.Solution.Iteration) + CRLF;
    S := S + 'Control Iterations = ' + IntToStr(DSS.ActiveCircuit.Solution.ControlIteration) + CRLF;
    S := S + 'Max Sol Iter = ' + IntToStr(DSS.ActiveCircuit.Solution.MostIterationsDone) + CRLF;
    S := S + ' ' + CRLF;
    S := S + ' - Circuit Summary -' + CRLF;
    S := S + ' ' + CRLF;
    if DSS.ActiveCircuit <> NIL then
    begin
        S := S + Format('Year = %d ', [DSS.ActiveCircuit.Solution.Year]) + CRLF;
        S := S + Format('Hour = %d ', [DSS.ActiveCircuit.Solution.DynaVars.intHour]) + CRLF;
        S := S + 'Max pu. voltage = ' + Format('%-.5g ', [GetMaxPUVoltage(DSS)]) + CRLF;
        S := S + 'Min pu. voltage = ' + Format('%-.5g ', [GetMinPUVoltage(DSS, TRUE)]) + CRLF;
        cPower := GetTotalPowerFromSources(DSS) * 0.000001;  // MVA
        S := S + Format('Total Active Power:   %-.6g MW', [cpower.re]) + CRLF;
        S := S + Format('Total Reactive Power: %-.6g Mvar', [cpower.im]) + CRLF;
        cLosses := DSS.ActiveCircuit.Losses * 0.000001;
        if cPower.re <> 0.0 then
            S := S + Format('Total Active Losses:   %-.6g MW, (%-.4g %%)', [cLosses.re, (Closses.re / cPower.re * 100.0)]) + CRLF
        else
            S := S + 'Total Active Losses:   ****** MW, (**** %%)' + CRLF;
        S := S + Format('Total Reactive Losses: %-.6g Mvar', [cLosses.im]) + CRLF;
        S := S + Format('Frequency = %-g Hz', [DSS.ActiveCircuit.Solution.Frequency]) + CRLF;
        S := S + 'Mode = ' + DSS.SolveModeEnum.OrdinalToString(ord(DSS.ActiveCircuit.Solution.mode)) + CRLF;
        S := S + 'Control Mode = ' + DSS.ControlModeEnum.OrdinalToString(DSS.ActiveCircuit.Solution.Controlmode) + CRLF;
        S := S + 'Load Model = ' + DSS.DefaultLoadModelEnum.OrdinalToString(DSS.ActiveCircuit.Solution.LoadModel) + CRLF;
    end;

    DSS.GlobalResult := S;
end;

function TExecHelper.DoDistributeCmd: Integer;
var
    ParamPointer: Integer;
    ParamName,
    Param: String;

    DoGenerators: Boolean;

    kW, PF: Double;
    Skip: Integer;
    How,
    FilName: String;

begin
    Result := 0;
    ParamPointer := 0;
     {Defaults}
    kW := 1000.0;
    How := 'Proportional';
    Skip := 1;
    PF := 1.0;
    FilName := 'DistGenerators.dss';
    DoGenerators := TRUE;

    ParamName := DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;
    while Length(Param) > 0 do
    begin
        if (Length(ParamName) = 0) then
            Inc(ParamPointer)
        else
            ParamPointer := DistributeCommands.GetCommand(ParamName);

        case ParamPointer of
            1:
                kW := DSS.Parser.DblValue;
            2:
                How := DSS.Parser.StrValue;
            3:
                Skip := DSS.Parser.IntValue;
            4:
                PF := DSS.Parser.DblValue;
            5:
                FilName := DSS.Parser.StrValue;
            6:
                kW := DSS.Parser.DblValue * 1000.0;
            7:
                if (AnsiUpperCase(Param)[1] = 'L') then
                    DoGenerators := FALSE
                else
                    DoGenerators := TRUE;  // Load or Generator

        else
             // ignore unnamed and extra parms
        end;

        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
    end;

    if not DoGenerators then
        FilName := 'DistLoads.dss';

    MakeDistributedGenerators(DSS, kW, PF, How, Skip, FilName, DoGenerators);  // in Utilities

end;

function TExecHelper.DoDI_PlotCmd: Integer;
var
    ParamName, Param: String;
    ParamPointer, i: Integer;
    CaseName: String;
    MeterName: String;
    CaseYear: Integer;
    dRegisters: array[1..NumEMRegisters] of Double;
    iRegisters: array of Integer;
    NumRegs: Integer;
    PeakDay: Boolean;
    plotParams: TJSONObject = NIL;
    jsonRegisters: TJSONArray = NIL;
    plotParamsStr: String = '';
    gotError: Boolean = TRUE;
begin
    if DSS.DIFilesAreOpen then
        DSS.EnergyMeterClass.CloseAllDIFiles;

     // Defaults
    NumRegs := 1;
    SetLength(IRegisters, NumRegs);
    iRegisters[0] := 9;
    PeakDay := FALSE;
    CaseYear := 1;
    CaseName := '';
    MeterName := 'DI_Totals';

    ParamPointer := 0;
    ParamName := DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;
    while Length(Param) > 0 do
    begin
        if (Length(ParamName) = 0) then
            Inc(ParamPointer)
        else
            ParamPointer := DI_PlotCommands.GetCommand(ParamName);

        case ParamPointer of
            1:
                CaseName := Param;
            2:
                CaseYear := DSS.Parser.Intvalue;
            3:
            begin
                NumRegs := DSS.Parser.ParseAsVector(NumEMREgisters, pDoubleArray(@dRegisters));
                SetLength(iRegisters, NumRegs);
                for i := 1 to NumRegs do
                    iRegisters[i - 1] := Round(dRegisters[i]);
            end;
            4:
                PeakDay := InterpretYesNo(Param);
            5:
                MeterName := Param;

        else
             // ignore unnamed and extra parms
        end;

        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
    end;

    try
        jsonRegisters := TJSONArray.Create();
        for i := 0 to High(iRegisters) do
            jsonRegisters.Add(iRegisters[i]);

        plotParams := TJSONObject.Create([
            'PlotType', 'DI',
            'CaseYear', CaseYear,
            'CaseName', CaseName,
            'MeterName', MeterName,
            'Registers', jsonRegisters,
            'PeakDay', PeakDay
            ]);

        plotParamsStr := plotParams.FormatJSON();
        DSS.DSSPlotCallback(DSS, Pchar(plotParamsStr));
        gotError := FALSE;
    finally
        FreeAndNil(plotParams);
    end;

    if gotError then
        DoSimpleMsg(DSS, _('Could not setup DI_Plot data'), 778);

    iRegisters := NIL;
    Result := 0;
end;

function TExecHelper.DoCompareCasesCmd: Integer;
var
    ParamName, Param: String;
    ParamPointer: Integer;
    UnKnown: Boolean;
    Reg: Integer;
    CaseName1, CaseName2, WhichFile: String;
    plotParams: TJSONObject = NIL;
    plotParamsStr: String = '';
    gotError: Boolean = TRUE;
begin
    if DSS.DIFilesAreOpen then
        DSS.EnergyMeterClass.CloseAllDIFiles;
    CaseName1 := 'base';
    CaseName2 := '';
    Reg := 9;    // Overload EEN
    WhichFile := 'Totals';

    ParamPointer := 0;
    ParamName := AnsiUpperCase(DSS.Parser.NextParam);
    Param := DSS.Parser.StrValue;
    while Length(Param) > 0 do
    begin
        Unknown := FALSE;
        if (Length(ParamName) = 0) then
            Inc(ParamPointer)

        else
        begin
            if CompareTextShortest(ParamName, 'CASE1') = 0 then
                ParamPointer := 1
            else
            if CompareTextShortest(ParamName, 'CASE2') = 0 then
                ParamPointer := 2
            else
            if CompareTextShortest(ParamName, 'REGISTER') = 0 then
                ParamPointer := 3
            else
            if CompareTextShortest(ParamName, 'METER') = 0 then
                ParamPointer := 4
            else
                Unknown := TRUE;
        end;


        if not Unknown then
            case ParamPointer of
                1:
                    CaseName1 := Param;
                2:
                    CaseName2 := Param;
                3:
                    Reg := DSS.Parser.IntValue;
                4:
                    WhichFile := Param;
            else
             // ignore unnamed and extra parms
            end;

        ParamName := AnsiUpperCase(DSS.Parser.NextParam);
        Param := DSS.Parser.StrValue;
    end;

    try
        plotParams := TJSONObject.Create([
            'PlotType', 'CompareCases',
            'CaseName1', CaseName1,
            'CaseName2', CaseName2,
            'MeterName', WhichFile,
            'Register', Reg
            ]);
        plotParamsStr := plotParams.FormatJSON();
        DSS.DSSPlotCallback(DSS, Pchar(plotParamsStr));
        gotError := FALSE;
    finally
        FreeAndNil(plotParams);
    end;

    if gotError then
        DoSimpleMsg(DSS, _('Could not setup DI_Plot data'), 778);

    Result := 0;
end;

function TExecHelper.DoYearlyCurvesCmd: Integer;
var
    ParamName, Param: String;
    ParamPointer, i: Integer;
    UnKnown: Boolean;
    CaseNames: TStringList;
    dRegisters: array[1..NumEMRegisters] of Double;
    iRegisters: array of Integer;
    Nregs: Integer;
    WhichFile: String;
    plotParams: TJSONObject = NIL;
    jsonCaseNames: TJSONArray = NIL;
    jsonRegisters: TJSONArray = NIL;
    plotParamsStr: String = '';
    gotError: Boolean = TRUE;
begin
    if DSS.DIFilesAreOpen then
        DSS.EnergyMeterClass.CloseAllDIFiles;

    Nregs := 1;
    SetLength(iRegisters, Nregs);
    jsonCaseNames := TJSONArray.Create();
    WhichFile := 'Totals';

    ParamPointer := 0;
    ParamName := DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;
    while Length(Param) > 0 do
    begin
        Unknown := FALSE;
        if (Length(ParamName) = 0) then
            Inc(ParamPointer)

        else
            case AnsiUpperCase(ParamName)[1] of
                'C':
                    ParamPointer := 1;
                'R':
                    ParamPointer := 2;
                'M':
                    ParamPointer := 3; {meter=}
            else
                Unknown := TRUE;
            end;

        if not Unknown then
            case ParamPointer of
                1:
                begin  // List of case names
                    DSS.AuxParser.CmdString := Param;
                    DSS.AuxParser.NextParam;
                    Param := DSS.AuxParser.StrValue;
                    while Length(Param) > 0 do
                    begin
                        jsonCaseNames.Add(Param);
                        DSS.AuxParser.NextParam;
                        Param := DSS.AuxParser.StrValue;
                    end;
                end;
                2:
                begin
                    NRegs := DSS.Parser.ParseAsVector(NumEMRegisters, pDoubleArray(@dRegisters));
                    SetLength(iRegisters, Nregs);
                    for i := 1 to NRegs do
                        iRegisters[i - 1] := Round(dRegisters[i]);
                end;
                3:
                    WhichFile := Param;
            else
             // ignore unnamed and extra parms
            end;

        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
    end;

    try
        jsonRegisters := TJSONArray.Create();
        for i := 0 to High(iRegisters) do
            jsonRegisters.Add(iRegisters[i]);

        plotParams := TJSONObject.Create([
            'PlotType', 'YearlyCurve',
            'CaseNames', jsonCaseNames,
            'MeterName', WhichFile,
            'Registers', jsonRegisters
            ]);
        jsonCaseNames := NIL;
        plotParamsStr := plotParams.FormatJSON();
        DSS.DSSPlotCallback(DSS, Pchar(plotParamsStr));
        gotError := FALSE;
    finally
        FreeAndNil(plotParams);
        FreeAndNil(jsonCaseNames);
    end;

    if gotError then
        DoSimpleMsg(DSS, _('Could not setup YearlyCurves data'), 779);

    iRegisters := NIL;
    Result := 0;
end;

function TExecHelper.DoVisualizeCmd: Integer;
var
    DevIndex: Integer;
    Param: String;
    ParamName: String;
    ParamPointer: Integer;
    Unknown: Boolean;
    Quantity: String;
    ElemName: String;
    plotParamsStr: String;
    pElem: TDSSObject;
    plotParams: TJSONObject = NIL;
begin
    Result := 0;
     // Abort if no circuit or solution
    if not assigned(DSS.ActiveCircuit) then
    begin
        DoSimpleMsg(DSS, _('No circuit created.'), 24721);
        Exit;
    end;
    if not assigned(DSS.ActiveCircuit.Solution) or not assigned(DSS.ActiveCircuit.Solution.NodeV) then
    begin
        DoSimpleMsg(DSS, _('The circuit must be solved before you can do this.'), 24722);
        Exit;
    end;
    Quantity := 'Current';
    ElemName := '';
    // Parse rest of command line
    ParamPointer := 0;
    ParamName := AnsiUpperCase(DSS.Parser.NextParam);
    Param := DSS.Parser.StrValue;
    while Length(Param) > 0 do
    begin
        Unknown := FALSE;
        if (Length(ParamName) = 0) then
            Inc(ParamPointer)
        else
        begin
            if CompareTextShortest(ParamName, 'WHAT') = 0 then
                ParamPointer := 1
            else
            if CompareTextShortest(ParamName, 'ELEMENT') = 0 then
                ParamPointer := 2
            else
                Unknown := TRUE;
        end;
        if not Unknown then
            case ParamPointer of
                1:
                    case AnsiLowerCase(Param)[1] of
                        'c':
                            Quantity := 'Current';
                        'v':
                            Quantity := 'Voltage';
                        'p':
                            Quantity := 'Power';
                    end;
                2:
                    ElemName := Param;
            else
            // ignore unnamed and extra parms
            end;
        ParamName := AnsiUpperCase(DSS.Parser.NextParam);
        Param := DSS.Parser.StrValue;
    end;
    // --------------------------------------------------------------
    Devindex := GetCktElementIndex(DSS, ElemName); // Global function
    if DevIndex > 0 then
    begin  //  element must already exist
        pElem := DSS.ActiveCircuit.CktElements.Get(DevIndex);
        if not (pElem is TDSSCktElement) then
        begin
            DoSimpleMsg(DSS, '"%s" must be a circuit element type!', [pElem.Name], 282);   // Wrong type
            Exit;
        end;
    end
    else
    begin
        DoSimpleMsg(DSS, 'Requested Circuit Element: "%s" not found.', [ElemName], 282); // Did not find it ..
        Exit;
    end;

    try
        // pElem.ComputeIterminal(); 
        // pElem.ComputeVTerminal();
        plotParams := TJSONObject.Create([
            'PlotType', 'Visualize',
            'ElementName', pElem.Name,
            'ElementType', pElem.DSSClassName,
            'Quantity', Quantity
            ]);
        // plotParams.CompressedJSON := True;
        plotParamsStr := plotParams.FormatJSON();
        if (@DSS.DSSPlotCallback) <> NIL then
            DSS.DSSPlotCallback(DSS, Pchar(plotParamsStr));
    finally
        FreeAndNil(plotParams);
    end;
end;

function TExecHelper.DoCloseDICmd: Integer;

begin
    Result := 0;
    DSS.EnergyMeterClass.CloseAllDIFiles;
end;

function TExecHelper.DoADOScmd: Integer;

begin
    Result := 0;
    DoDOScmd(DSS, DSS.Parser.Remainder);
end;

function TExecHelper.DoEstimateCmd: Integer;


begin
    Result := 0;

    // Load current Estimation is driven by Energy Meters at head of feeders.
    DoAllocateLoadsCmd;

     // Let's look to see how well we did
    if not DSS.AutoShowExport then
        DSS.DSSExecutive.Command := 'Set showexport=yes';
    DSS.DSSExecutive.Command := 'Export Estimation';
end;


function TExecHelper.DoReconductorCmd: Integer;

var
    Param: String;
    ParamName: String;
    ParamPointer: Integer;
    Line1, Line2,
    Linecode,
    Geometry,
    EditString,
    MyEditString: String;
    LineCodeSpecified,
    GeometrySpecified: Boolean;
    pLine1, pLine2: TLineObj;
    LineClass: TLine;
    TraceDirection: Integer;
    NPhases: Integer;


begin
    Result := 0;
    ParamPointer := 0;
    LineCodeSpecified := FALSE;
    GeometrySpecified := FALSE;
    Line1 := '';
    Line2 := '';
    MyEditString := '';
    NPhases := 0; // no filtering by number of phases
    ParamName := DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;
    while Length(Param) > 0 do
    begin
        if Length(ParamName) = 0 then
            Inc(ParamPointer)
        else
            ParamPointer := ReconductorCommands.GetCommand(ParamName);

        case ParamPointer of
            1:
                Line1 := Param;
            2:
                Line2 := Param;
            3:
            begin
                Linecode := Param;
                LineCodeSpecified := TRUE;
                GeometrySpecified := FALSE;
            end;
            4:
            begin
                Geometry := Param;
                LineCodeSpecified := FALSE;
                GeometrySpecified := TRUE;
            end;
            5:
                MyEditString := Param;
            6:
                Nphases := DSS.Parser.IntValue;
        else
            DoSimpleMsg(DSS, 'Error: Unknown Parameter on command line: %s', [Param], 28701);
        end;

        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
    end;

     {Check for Errors}

     {If user specified full line name, get rid of "line."}
    Line1 := StripClassName(Line1);
    Line2 := StripClassName(Line2);

    if (Length(Line1) = 0) or (Length(Line2) = 0) then
    begin
        DoSimpleMsg(DSS, _('Both Line1 and Line2 must be specified!'), 28702);
        Exit;
    end;

    if (not LineCodeSpecified) and (not GeometrySpecified) then
    begin
        DoSimpleMsg(DSS, _('Either a new LineCode or a Geometry must be specified!'), 28703);
        Exit;
    end;

    LineClass := DSS.DSSClassList.Get(DSS.ClassNames.Find('Line'));
    pLine1 := LineClass.Find(Line1);
    pLine2 := LineCLass.Find(Line2);

    if (pLine1 = NIL) or (pLine2 = NIL) then
    begin
        if pLine1 = NIL then
            DoSimpleMsg(DSS, 'Line.%s not found.', [Line1], 28704)
        else
        if pLine2 = NIL then
            DoSimpleMsg(DSS, 'Line.%s not found.', [Line2], 28704);
        Exit;
    end;

     {Now check to make sure they are in the same meter's zone}
    if (pLine1.MeterObj = NIL) or (pLine2.MeterObj = NIL) then
    begin
        DoSimpleMsg(DSS, _('Error: Both Lines must be in the same EnergyMeter zone. One or both are not in any meter zone.'), 28705);
        Exit;
    end;

    if pLine1.MeterObj <> pline2.MeterObj then
    begin
        DoSimpleMsg(DSS, 'Error: Line1 is in %s zone while Line2 is in %s zone. Both must be in the same Zone.',
            [pLine1.MeterObj.FullName, pLine2.MeterObj.FullName], 28706);
        Exit;
    end;

     {Since the lines can be given in either order, Have to check to see which direction they are specified and find the path between them}
    TraceDirection := 0;
    if IsPathBetween(pLine1, pLine2) then
        TraceDirection := 1;
    if IsPathBetween(pLine2, pLine1) then
        TraceDirection := 2;

    if LineCodeSpecified then
        EditString := 'Linecode=' + LineCode
    else
        EditString := 'Geometry=' + Geometry;

     // Append MyEditString onto the end of the edit string to change the linecode  or geometry
    EditString := Format('%s  %s', [EditString, MyEditString]);

    case TraceDirection of
        1:
            TraceAndEdit(DSS, pLine1, pLine2, NPhases, Editstring);
        2:
            TraceAndEdit(DSS, pLine2, pLine1, NPhases, Editstring);
    else
        DoSimpleMsg(DSS, _('Traceback path not found between Line1 and Line2.'), 28707);
        Exit;
    end;
end;

function TExecHelper.DoAddMarkerCmd: Integer;
var
    ParamPointer: Integer;
    ParamName,
    Param: String;
    BusMarker: TBusMarker;

begin
    Result := 0;
    ParamPointer := 0;

    BusMarker := TBusMarker.Create;
    DSS.ActiveCircuit.BusMarkerList.Add(BusMarker);

    ParamName := DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;
    while Length(Param) > 0 do
    begin
        if (Length(ParamName) = 0) then
            Inc(ParamPointer)
        else
            ParamPointer := AddmarkerCommands.GetCommand(ParamName);

        with BusMarker do
            case ParamPointer of
                1:
                    BusName := Param;
                2:
                    AddMarkerCode := DSS.Parser.IntValue;
                3:
                    AddMarkerColor := InterpretColorName(DSS, Param);
                4:
                    AddMarkerSize := DSS.Parser.IntValue;

            else
             // ignore unnamed and extra parms
            end;

        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
    end;
end;

function TExecHelper.DoSetLoadAndGenKVCmd: Integer;
var
    pLoad: TLoadObj;
    pGen: TGeneratorObj;
    pBus: TDSSBus;
    sBus: String;
    iBus, i: Integer;
    kvln: Double;
begin
    Result := 0;
    for pLoad in DSS.ActiveCircuit.Loads do
    begin
        sBus := StripExtension(pLoad.GetBus(1));
        iBus := DSS.ActiveCircuit.BusList.Find(sBus);
        pBus := DSS.ActiveCircuit.Buses[iBus];
        kvln := pBus.kVBase;
        if (pLoad.Connection = TLoadConnection.Delta) or (pLoad.NPhases = 3) then
            pLoad.kVLoadBase := kvln * sqrt(3.0)
        else
            pLoad.kVLoadBase := kvln;

        pLoad.PropertySideEffects(ord(TLoadProp.kV));
        pLoad.RecalcElementData;
    end;

    for i := 1 to DSS.ActiveCircuit.Generators.Count do
    begin
        pGen := DSS.ActiveCircuit.Generators.Get(i);
        sBus := StripExtension(pGen.GetBus(1));
        iBus := DSS.ActiveCircuit.BusList.Find(sBus);
        pBus := DSS.ActiveCircuit.Buses[iBus];
        kvln := pBus.kVBase;
        if (pGen.Connection = 1) or (pGen.NPhases > 1) then
            pGen.PresentKV := kvln * sqrt(3.0)
        else
            pGen.PresentKV := kvln;
        pGen.RecalcElementData;
    end;
end;

function TExecHelper.DoUuidsCmd: Integer;
var
    F: TStream = NIL;
    Param, S, NameVal, UuidVal, DevClass, DevName: String;
    pName: TNamedObject;
    idx: Integer;
begin
    DSS.CIMExporter.StartUuidList(DSS.ActiveCircuit.NumBuses + 2 * DSS.ActiveCircuit.NumDevices);
    Result := 0;
    DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;
    if not FileExists(Param) then
    begin
        DoSimpleMsg(DSS, 'UUIDs file: %s does not exist', [Param], 242);
        exit;
    end;
    try
        F := DSS.GetROFileStream(Param);
        DSS.AuxParser.Delimiters := ',';
        while (F.Position + 1) < F.Size do
        begin
            FSReadln(F, S);
            with DSS.AuxParser do
            begin
                pName := NIL;
                CmdString := S;
                NextParam;
                NameVal := StrValue;
                NextParam;
                UuidVal := StrValue;
        // format the UUID properly
                if Pos('{', UuidVal) < 1 then
                    UuidVal := '{' + UuidVal + '}';
                if Pos('=', NameVal) > 0 then
                begin  // it's a non-identified object in OpenDSS
                    DSS.CIMExporter.AddHashedUuid(NameVal, UuidVal);
                end
                else
                begin  // find this as a descendant of TNamedObject
                    pName := NIL;
                    ParseObjectClassAndName(DSS, NameVal, DevClass, DevName);
                    if CompareText(DevClass, 'circuit') = 0 then
                    begin
                        pName := DSS.ActiveCircuit
                    end
                    else
                    if CompareText(DevClass, 'Bus') = 0 then
                    begin
                        idx := DSS.ActiveCircuit.BusList.Find(DevName);
                        if idx > 0 then
                            pName := DSS.ActiveCircuit.Buses[idx];
                    end
                    else
                    begin
                        DSS.LastClassReferenced := DSS.ClassNames.Find(DevClass);
                        DSS.ActiveDSSClass := DSS.DSSClassList.Get(DSS.LastClassReferenced);
                        if DSS.ActiveDSSClass <> NIL then
                            if DSS.ActiveDSSClass.SetActive(DevName) then
                                pName := DSS.ActiveDSSClass.GetActiveObj;
                    end;
          // re-assign its UUID
                    if pName <> NIL then
                        pName.UUID := StringToUuid(UuidVal);
                end;
            end;
        end;
    finally
        DSS.AuxParser.ResetDelims;
        FreeAndNil(F);
    end;
end;

function TExecHelper.DoCvrtLoadshapesCmd: Integer;
var
    pLoadshape: TLoadShapeObj;
    LoadShapeClass: TLoadShape;
   //ParamName      :String;
    Param: String;
    Action: String;
    F: TFileStream = NIL;
    Fname: String;

begin
    DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;

    if length(param) = 0 then
        Param := 's';

    {Double file or Single file?}
    case AnsiLowerCase(param)[1] of
        'd':
            Action := 'action=dblsave';
    else
        Action := 'action=sngsave';   // default
    end;

    LoadShapeClass := GetDSSClassPtr(DSS, 'loadshape') as TLoadShape;

    Fname := DSS.OutputDirectory {CurrentDSSDir} + 'ReloadLoadshapes.dss';
    F := TBufferedFileStream.Create(Fname, fmCreate);

    for pLoadShape in LoadShapeClass do
    begin
        DSS.Parser.CmdString := Action;
        pLoadShape.Edit(DSS.Parser);
        FSWriteln(F, Format('New %s Npts=%d Interval=%.8g %s', [pLoadShape.FullName, pLoadShape.NumPoints, pLoadShape.Interval, DSS.GlobalResult]));
    end;

    FreeAndNil(F);
    FireOffEditor(DSS, Fname);
    Result := 0;
end;

function TExecHelper.DoNodeDiffCmd: Integer;

var
    ParamName: String;
    Param: String;
    sNode1, sNode2: String;
    SBusName: String;
    V1, V2,
    VNodeDiff: Complex;
    iBusidx: Integer;
    B1ref: Integer;
    B2ref: Integer;
    NumNodes: Integer;
    NodeBuffer: array[1..50] of Integer;


begin
    Result := 0;
    ParamName := DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;
    sNode1 := Param;
    if Pos('2', ParamName) > 0 then
        sNode2 := Param;

    ParamName := DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;
    sNode2 := Param;
    if Pos('1', ParamName) > 0 then
        sNode1 := Param;

    // Get first node voltage
    NodeBuffer[1] := 1;
    sBusName := DSS.AuxParser.ParseAsBusName(sNode1, numNodes, pIntegerArray(@NodeBuffer));
    iBusidx := DSS.ActiveCircuit.Buslist.Find(sBusName);
    if iBusidx > 0 then
    begin
        B1Ref := DSS.ActiveCircuit.Buses[iBusidx].Find(NodeBuffer[1])
    end
    else
    begin
        DoSimpleMsg(DSS, 'Bus %s not found.', [sBusName], 28709);
        Exit;
    end;

    V1 := DSS.ActiveCircuit.Solution.NodeV[B1Ref];

    // Get 2nd node voltage
    NodeBuffer[1] := 1;
    sBusName := DSS.AuxParser.ParseAsBusName(sNode2, numNodes, pIntegerArray(@NodeBuffer));
    iBusidx := DSS.ActiveCircuit.Buslist.Find(sBusName);
    if iBusidx > 0 then
    begin
        B2Ref := DSS.ActiveCircuit.Buses[iBusidx].Find(NodeBuffer[1])
    end
    else
    begin
        DoSimpleMsg(DSS, 'Bus %s not found.', [sBusName], 28710);
        Exit;
    end;

    V2 := DSS.ActiveCircuit.Solution.NodeV[B2Ref];

    VNodeDiff := V1 - V2;
    DSS.GlobalResult := Format('%.7g, V,    %.7g, deg  ', [Cabs(VNodeDiff), CDang(VNodeDiff)]);
end;

function TExecHelper.DoRephaseCmd: Integer;
var
    Param: String;
    ParamName: String;
    ParamPointer: Integer;
    StartLine: String;
    NewPhases: String;
    MyEditString: String;
    ScriptfileName: String;
    pStartLine: TLineObj;
    LineClass: TLine;
    TransfStop: Boolean;

begin
    Result := 0;
    ParamPointer := 0;
    MyEditString := '';
    ScriptfileName := 'RephaseEditScript.dss';
    TransfStop := TRUE;  // Stop at Transformers

    ParamName := DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;
    while Length(Param) > 0 do
    begin
        if Length(ParamName) = 0 then
            Inc(ParamPointer)
        else
            ParamPointer := RephaseCommands.GetCommand(ParamName);

        case ParamPointer of
            1:
                StartLine := Param;
            2:
                NewPhases := Param;
            3:
                MyEditString := Param;
            4:
                ScriptFileName := Param;
            5:
                TransfStop := InterpretYesNo(Param);
        else
            DoSimpleMsg(DSS, 'Error: Unknown Parameter on command line: %s', [Param], 28711);
        end;

        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
    end;

    LineClass := DSS.DSSClassList.Get(DSS.ClassNames.Find('Line'));
    pStartLine := LineClass.Find(StripClassName(StartLine));
    if pStartLine = NIL then
    begin
        DoSimpleMsg(DSS, 'Starting Line (%s) not found.', [StartLine], 28712);
        Exit;
    end;
     {Check for some error conditions and abort if necessary}
    if pStartLine.MeterObj = NIL then
    begin
        DoSimpleMsg(DSS, _('Starting Line must be in an EnergyMeter zone.'), 28713);
        Exit;
    end;

    if not (pStartLine.MeterObj is TEnergyMeterObj) then
    begin
        DoSimpleMsg(DSS, _('Starting Line must be in an EnergyMeter zone.'), 28714);
        Exit;
    end;

    GoForwardandRephase(DSS, pStartLine, NewPhases, MyEditString, ScriptfileName, TransfStop);
end;

function TExecHelper.DoSetBusXYCmd: Integer;

var
    Param: String;
    ParamName: String;
    ParamPointer: Integer;
    BusName: String;
    Xval: Double;
    Yval: Double;
    iB: Integer;

begin
    Result := 0;
    ParamName := DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;
    ParamPointer := 0;
    Xval := 0.0;
    Yval := 0.0;
    while Length(Param) > 0 do
    begin
        if Length(ParamName) = 0 then
            Inc(ParamPointer)
        else
            ParamPointer := SetBusXYCommands.GetCommand(ParamName);

        case ParamPointer of
            1:
                BusName := Param;
            2:
                Xval := DSS.Parser.DblValue;
            3:
                Yval := DSS.Parser.DblValue;
        else
            DoSimpleMsg(DSS, 'Error: Unknown Parameter on command line: %s', [Param], 28721);
        end;

        iB := DSS.ActiveCircuit.Buslist.Find(BusName);
        if iB > 0 then
        begin
            with DSS.ActiveCircuit.Buses[iB] do
            begin     // Returns TBus object
                x := Xval;
                y := Yval;
                CoordDefined := TRUE;
            end;
        end
        else
        begin
            DoSimpleMsg(DSS, 'Error: Bus "%s" not found.', [BusName], 28722);
        end;

        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
    end;
end;

function TExecHelper.DoUpdateStorageCmd: Integer;
begin
    DSS.StorageClass.UpdateAll();
    Result := 0;
end;

function TExecHelper.DoPstCalc;

var
    Param: String;
    ParamName: String;
    ParamPointer: Integer;
    Npts: Integer;
    Varray: pDoubleArray;
    CyclesPerSample: Integer;
    Lamp: Integer;
    PstArray: pDoubleArray;
    nPst: Integer;
    i: Integer;
    S: String;
    Freq: Double;

begin
    Result := 0;
    Varray := NIL;
    PstArray := NIL;
    Npts := 0;
    Lamp := 120;  // 120 or 230
    CyclesPerSample := 60;
    Freq := DSS.DefaultBaseFreq;

    ParamName := DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;
    ParamPointer := 0;
    while Length(Param) > 0 do
    begin
        if Length(ParamName) = 0 then
            Inc(ParamPointer)
        else
            ParamPointer := PstCalcCommands.GetCommand(ParamName);
         // 'Npts', 'Voltages', 'cycles', 'lamp'
        case ParamPointer of
            1:
            begin
                Npts := DSS.Parser.IntValue;
                Reallocmem(Varray, SizeOf(Varray^[1]) * Npts);
            end;
            2:
                Npts := InterpretDblArray(DSS, Param, Npts, Varray);
            3:
                CyclesPerSample := Round(DSS.ActiveCircuit.Solution.Frequency * DSS.Parser.dblvalue);
            4:
                Freq := DSS.Parser.DblValue;
            5:
                Lamp := DSS.Parser.IntValue;
        else
            DoSimpleMsg(DSS, 'Error: Unknown Parameter on command line: %s', [Param], 28722);
        end;

        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
    end;

    if Npts > 10 then
    begin
        nPst := PstRMS(PstArray, Varray, Freq, CyclesPerSample, Npts, Lamp);
         // put resulting pst array in the result string
        S := '';
        for i := 1 to nPst do
            S := S + Format('%.8g, ', [PstArray^[i]]);
        DSS.GlobalResult := S;
    end
    else
        DoSimpleMsg(DSS, _('Insuffient number of points for Pst Calculation.'), 28723);


    Reallocmem(Varray, 0);   // discard temp arrays
    Reallocmem(PstArray, 0);
end;

function TExecHelper.DoLambdaCalcs: Integer;
// Execute fault rate and bus number of interruptions calc
var
    pMeter: TEnergyMeterObj;
    i: Integer;
    //ParamName,
    Param: String;
    AssumeRestoration: Boolean;
begin
    Result := 0;

    // Do for each Energymeter object in active circuit
    if DSS.ActiveCircuit.EnergyMeters.Count = 0 then
    begin
        DoSimpleMsg(DSS, _('No EnergyMeter Objects Defined. EnergyMeter objects required for this function.'), 28724);
        Exit;
    end;

    DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;

    if Length(Param) > 0 then
        Assumerestoration := InterpretYesNo(param)
    else
        Assumerestoration := FALSE;

       // initialize bus quantities
    with DSS.ActiveCircuit do
        for i := 1 to NumBuses do
            with Buses[i] do
            begin
                BusFltRate := 0.0;
                Bus_Num_Interrupt := 0.0;
            end;

    for pMeter in DSS.ActiveCircuit.EnergyMeters do
    begin
        pMeter.AssumeRestoration := AssumeRestoration;
        pMeter.CalcReliabilityIndices();
    end;
end;

function TExecHelper.DoVarCmd: Integer;
// Process Script variables
var
    ParamName: String;
    Param: String;
    Str: String;
    iVar: Integer;
begin
    Result := 0;

    ParamName := DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;

    if Length(Param) = 0 then  // show all vars
    begin
          // MsgStrings := TStringList.Create;
          // MsgStrings.Add('Variable, Value');
          // for iVar := 1 to DSS.ParserVars.NumVariables  do
          //     MsgStrings.Add(DSS.ParserVars.VarString[iVar] );
          // ShowMessageForm(MsgStrings);
          // MsgStrings.Free;
        Str := _('Variable, Value') + CRLF;
        for iVar := 1 to DSS.ParserVars.NumVariables do
            Str := Str + DSS.ParserVars.VarString[iVar] + CRLF;

        DSS.GlobalResult := Str;
    end
    else
    if Length(ParamName) = 0 then   // show value of this var
    begin
        DSS.GlobalResult := Param;  // DSS.Parser substitutes @var with value
    end
    else
    begin
        while Length(ParamName) > 0 do
        begin
            case ParamName[1] of
                '@':
                    DSS.ParserVars.Add(ParamName, Param);
            else
                DoSimpleMsg(DSS, 'Illegal Variable Name: %s; Must begin with "@"', [ParamName], 28725);
                Exit;
            end;
            ParamName := DSS.Parser.NextParam;
            Param := DSS.Parser.StrValue;
        end;

    end;
end;

function TExecHelper.DoRemoveCmd: Integer;
var
    ParamName: String;
    Param: String;
    ParamPointer: Integer;
    DeviceIndex: Integer;

    FElementName: String;
    FKeepLoad: Boolean;
    FEditString: String;

    elem: TDSSObject;
    pPDElem: TPDelement;
    pMeter: TEnergyMeterObj;
    FMeterName: String;
begin
    Result := 0;
    if DSS.ActiveCircuit = NIL then
    begin
        DoSimpleMsg(DSS, _('Error: There is no active circuit!'), 28998);
        Exit;
    end;

    FElementName := '';
    FEditString := '';
    FKeepLoad := TRUE;
    ParamPointer := 0;

    ParamName := DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;

    while Length(Param) > 0 do
    begin
        if Length(ParamName) = 0 then
            Inc(ParamPointer)
        else
            ParamPointer := RemoveCommands.GetCommand(ParamName);

        case ParamPointer of
            1:
                FElementName := Param; {ElementName}
            2:
                FkeepLoad := InterpretYesNo(Param); {KeepLoad}
            3:
                FEditString := Param; {EditString}
        end;

        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
    end;

    // Check for existence of FelementName
    DeviceIndex := GetCktElementIndex(DSS, FElementName);
    if DeviceIndex = 0 then
    begin
        DoSimpleMsg(DSS,
            Format('Error: Element %s does not exist in this circuit.', [FelementName]),
            28726
            );
        Exit;
    end;

    // first, checks if the element is not linked to an energy meter, if it does, abort (added 01/06/2021 -DM)
    elem := DSS.ActiveCircuit.CktElements.Get(DeviceIndex);
    with DSS.ActiveCircuit do
    begin
        for pMeter in EnergyMeters do
        begin
            if pMeter.MeteredElement = elem then
            begin
                DoSimpleMsg(DSS,
                    Format('Error: Element %s is tied to an Energy Meter.', [FelementName]),
                    28800
                    );
                Exit;
            end;
        end;
    end;

    // Set CktElement active
    SetObject(DSS, FelementName);
    if not (DSS.ActiveCircuit.ActiveCktElement is TPDElement) then
    begin
        DoSimpleMsg(DSS,
            Format('Error: Element "%s" is not a power delivery element (PDElement)', [FelementName]),
            28728
            );
        Exit;
    end;

    // Get Energymeter associated with this element.
    pPDElem := DSS.ActiveCircuit.ActiveCktElement as TPDElement;
    if pPDElem.SensorObj = NIL then
    begin
        DoSimpleMsg(DSS,
            'Element "%s" is not in a meter zone! Add an Energymeter. ',
            [pPDelem.FullName],
            287261);
        Exit;
    end;

    FMeterName := pPDElem.SensorObj.FullName;
    SetObject(DSS, FMeterName);

    if not (DSS.ActiveCircuit.ActiveCktElement is TEnergyMeterObj) then
    begin
        DoSimpleMsg(DSS,
            'Error: The Sensor Object for "%s" is not an EnergyMeter object', [FelementName],
            28727
            );
        Exit;
    end;

    pMeter := DSS.ActiveCircuit.ActiveCktElement as TEnergyMeterObj;
    // in ReduceAlgs
    DoRemoveBranches(DSS, pMeter.BranchList, pPDelem, FKeepLoad, FEditString);
end;

initialization
    // Initialize Command lists

    SaveCommands := TCommandList.Create(['class', 'file', 'dir', 'keepdisabled'], TRUE);
    DI_PlotCommands := TCommandList.Create(['case', 'year', 'registers', 'peak', 'meter']);
    DistributeCommands := TCommandList.Create(['kW', 'how', 'skip', 'pf', 'file', 'MW', 'what'], TRUE);
    ReconductorCommands := TCommandList.Create(['Line1', 'Line2', 'LineCode', 'Geometry', 'EditString', 'Nphases'], TRUE);
    RephaseCommands := TCommandList.Create(['StartLine', 'PhaseDesignation', 'EditString', 'ScriptFileName', 'StopAtTransformers'], TRUE);
    AddMarkerCommands := TCommandList.Create(['Bus', 'code', 'color', 'size'], TRUE);
    SetBusXYCommands := TCommandList.Create(['Bus', 'x', 'y'], TRUE);
    PstCalcCommands := TCommandList.Create(['Npts', 'Voltages', 'dt', 'Frequency', 'lamp'], TRUE);
    RemoveCommands := TCommandList.Create(['ElementName', 'KeepLoad', 'Editstring'], TRUE);

finalization

    DistributeCommands.Free;
    DI_PlotCommands.Free;
    SaveCommands.Free;
    AddMarkerCommands.Free;
    ReconductorCommands.Free;
    RephaseCommands.Free;
    SetBusXYCommands.Free;
    PstCalcCommands.Free;
    RemoveCommands.Free;

end.