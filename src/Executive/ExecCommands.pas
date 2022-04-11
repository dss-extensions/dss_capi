unit ExecCommands;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Command, DSSClass;

type
    //Main executive commands
    {$SCOPEDENUMS ON}
    TExecCommand = (
        INVALID = 0,
        New = 1,
        Edit = 2,
        More = 3,
        M = 4,
        tilde = 5, // "~"
        Select = 6,
        Save = 7,
        Show = 8,
        Solve = 9,
        Enable = 10,
        Disable = 11,
        Plot = 12,
        Reset = 13,
        Compile = 14,
        SetOpt = 15,   // "Set" = Set DSS Options
        Dump = 16,   // Debug dump
        Open = 17,   // Open a device terminal conductor
        Close = 18,   // Close a device terminal conductor
        DoubleSlash = 19,       // "//" Comment
        Redirect = 20,
        Help = 21,
        Quit = 22,
        questionmark = 23,   // "?" = Property Value inquiry
        Next = 24,
        Panel = 25,
        Sample = 26,
        Clear = 27,
        About = 28,
        Calcvoltagebases = 29,  //  Computes voltage bases
        SetkVBase = 30,  //  Set kV Base at a Bus
        BuildY = 31,  //  forces Rebuild of Y matrix right now
        Get = 32,  //  returns values set WITH Set command
        Init = 33,
        Export = 34,
        Fileedit = 35,
        Voltages = 36,
        Currents = 37,
        Powers = 38,
        Seqvoltages = 39,
        Seqcurrents = 40,
        Seqpowers = 41,
        Losses = 42,
        Phaselosses = 43,
        Cktlosses = 44,
        Allocateloads = 45,
        Formedit = 46,
        Totals = 47,  // Total all energymeters
        Capacity = 48,  // Find upper kW limit of system for present year
        Classes = 49,  // List of intrinsic classes
        Userclasses = 50,  // List of user-defined classes
        Zsc = 51,
        Zsc10 = 52,
        ZscRefresh = 53,
        Ysc = 54,
        puvoltages = 55,
        VarValues = 56,
        Varnames = 57,
        Buscoords = 58,
        MakeBusList = 59,
        MakePosSeq = 60,
        Reduce = 61,
        Interpolate = 62,
        AlignFile = 63,
        TOP = 64, //TODO: remove
        Rotate = 65,
        Vdiff = 66,
        Summary = 67,
        Distribute = 68,
        DI_plot = 69,
        Comparecases = 70,
        YearlyCurves = 71,
        CD = 72,
        Visualize = 73,
        CloseDI = 74,
        DOScmd = 75,
        Estimate = 76,
        Reconductor = 77,
        _InitSnap = 78,
        _SolveNoControl = 79,
        _SampleControls = 80,
        _DoControlActions = 81,
        _ShowControlQueue = 82,
        _SolveDirect = 83,
        _SolvePFlow = 84,
        AddBusMarker = 85,
        Uuids = 86,
        SetLoadAndGenKV = 87,
        CvrtLoadshapes = 88,
        NodeDiff = 89,
        Rephase = 90,
        SetBusXY = 91,
        UpdateStorage = 92,
        Obfuscate = 93,
        LatLongCoords = 94,
        BatchEdit = 95,
        Pstcalc = 96,
        Variable = 97,
        ReprocessBuses = 98,
        ClearBusMarkers = 99,
        RelCalc = 100,
        vr = 101, // var
        Cleanup = 102,
        FinishTimeStep = 103,
        NodeList = 104,
        Connect = 105, //TODO: remove
        Disconnect = 106, //TODO: remove
        Remove = 107,
        CalcIncMatrix = 108,
        CalcIncMatrix_O = 109,
        Refine_BusLevels = 110,
        CalcLaplacian = 111,
        ExportOverloads = 112,
        ExportVViolations = 113,
        Zsc012 = 114,
        AllPCEatBus,
        AllPDEatBus,
        TotalPowers,
        GISCoords,
        ClearAll,
        COMHelp
{$IFDEF DSS_CAPI_PM}
        ,
        NewActor,
        Wait,
        SolveAll
    {$IFDEF DSS_CAPI_ADIAKOPTICS}
        ,
        AggregateProfiles,
        Tear_Circuit
    {$ENDIF}
        ,
        Abort,
        Clone
{$ENDIF}
    );

const
    NumExecCommands = ord(High(TExecCommand));
var
    ExecCommand: array[1..NumExecCommands] of String;

procedure ProcessCommand({$IFDEF DSS_CAPI_PM}MainDSS{$ELSE}DSS{$ENDIF}: TDSSContext; const CmdLine: String; LineNum: Integer = -1);

procedure DefineCommands;
procedure DisposeStrings;

implementation

uses
    DSSGlobals,
    ExecHelper,
    Executive,
    ExecOptions,
    PlotOptions,
    ShowOptions,
    ExportOptions,
    ParserDel,
    LoadShape,
    CmdForms,
    sysutils,
    Utilities,
    SolutionAlgs,
    DSSHelper,
    DSSClassDefs,
    EnergyMeter,
    MemoryMap_lib,
    TypInfo,
    KLUSolve,
    Solution
{$IFDEF DSS_CAPI_ADIAKOPTICS}
    , Diakoptics
    , sparse_math
{$ENDIF}
    ;
    
type
    Cmd = TExecCommand;

procedure DefineCommands;
var
    info: Pointer;
    i: Integer;
begin
    info := TypeInfo(Cmd);
    for i := 1 to NumExecCommands do
        ExecCommand[i] := GetEnumName(info, i);
    // Replace the ones that don't fit as Pascal identifiers
    ExecCommand[ord(Cmd.vr)] := 'var';
    ExecCommand[ord(Cmd.tilde)] := '~';
    ExecCommand[ord(Cmd.doubleslash)] := '//';
    ExecCommand[ord(Cmd.questionmark)] := '?';
    ExecCommand[ord(Cmd.SetOpt)] := 'Set';
end;

procedure ProcessCommand({$IFDEF DSS_CAPI_PM}MainDSS{$ELSE}DSS{$ENDIF}: TDSSContext; const CmdLine: String; LineNum: Integer);
var
    CommandList: TCommandList;
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    ObjName, PropName: String;
{$IFDEF DSS_CAPI_PM}
    i: Integer;
    PMParent, DSS: TDSSContext;
begin
    PMParent := MainDSS.GetPrime();
    DSS := MainDSS.ActiveChild;
{$ELSE}
begin
{$ENDIF}
    try
        CommandList := DSS.DSSExecutive.CommandList;
        DSS.CmdResult := 0;
        DSS.ErrorNumber := 0;  // Reset Error number
        DSS.GlobalResult := '';

        // Load up the parser and process the first parameter only
        DSS.LastCmdLine := CmdLine;
        DSS.Parser.CmdString := DSS.LastCmdLine;  // Load up command parser
        DSS.LastCommandWasCompile := FALSE;

        ParamPointer := 0;
        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
        if Length(Param) = 0 then
            Exit;  // Skip blank line

        // Check for Command verb or Property Value
        // Commands do not have equal signs so ParamName must be zero
        if Length(ParamName) = 0 then
            ParamPointer := CommandList.GetCommand(Param);

        // Check first for Compile or Redirect and get outta here
        case ParamPointer of
            ord(Cmd.Compile), ord(Cmd.Redirect):
            begin
                with DSS.DSSExecutive do
                    if RecorderOn then
                        Write_to_RecorderFile(CRLF + '!*********' + CmdLine);
                DSS.CmdResult := DSS.DSSExecutive.DoRedirect(ParamPointer = ord(Cmd.Compile));
                Exit;
            end;
        else   // Write everything direct to recorder, if ON
            with DSS.DSSExecutive do
                if RecorderOn then
                    Write_to_RecorderFile(CmdLine);
        end;

        // Things that are OK to do before a circuit is defined
        case ParamPointer of
            ord(Cmd.New):
                DSS.CmdResult := DSS.DSSExecutive.DoNewCmd; // new

            ord(Cmd.SetOpt):
                if not Assigned(DSS.ActiveCircuit) then
                begin
                    DoSetCmd_NoCircuit(DSS); // can only call this if no circuit active
                    Exit;    // We exit with either a good outcome or bad
                end;
            ord(Cmd.DoubleSlash):
                ; // Do Nothing - comment
            ord(Cmd.Help):
                DSS.CmdResult := DSS.DSSExecutive.DoHelpCmd;
            ord(Cmd.Quit):
                ;
                // if not IsDLL then ExitControlPanel;  // Quit in Stand alone version
            ord(Cmd.Panel):
                DoSimpleMsg(DSS, _('Command "panel" supported in DSS Extensions.'), 999);
            ord(Cmd.Clear):
                DSS.DSSExecutive.DoClearCmd;
            ord(Cmd.About):
                DSS.DSSExecutive.DoAboutBox;
            ord(Cmd.Get):
                if not Assigned(DSS.ActiveCircuit) then
                begin
                    DoGetCmd_NoCircuit(DSS); // can only call this if no circuit active
                    Exit;    // We exit with either a good outcome or bad
                end;
            ord(Cmd.Fileedit):
                DSS.CmdResult := DSS.DSSExecutive.DoFileEditCmd;
            ord(Cmd.Classes):
                DSS.CmdResult := DSS.DSSExecutive.DoClassesCmd;
            ord(Cmd.Userclasses):
                DSS.CmdResult := DSS.DSSExecutive.DoUserClassesCmd;
            ord(Cmd.AlignFile):
                DSS.CmdResult := DSS.DSSExecutive.DoAlignFileCmd;
            ord(Cmd.DI_plot):
                DSS.CmdResult := DSS.DSSExecutive.DoDI_PlotCmd;
            ord(Cmd.Comparecases):
                DSS.CmdResult := DSS.DSSExecutive.DoCompareCasesCmd;
            ord(Cmd.YearlyCurves):
                DSS.CmdResult := DSS.DSSExecutive.DoYearlyCurvesCmd;
            ord(Cmd.CD):
            begin
                ParamName := DSS.Parser.NextParam;
                Param := DSS.Parser.StrValue;
                if DirectoryExists(Param) then
                begin
                    DSS.CmdResult := 0;
                    SetDataPath(DSS, Param);  // change datadirectory
                end
                else
                    DoSimpleMsg(DSS, 'Directory "%s" not found.', [Param], 282);
            end;
            ord(Cmd.DOScmd):
                if DSS_CAPI_ALLOW_DOSCMD then
                    DSS.DSSExecutive.DoADosCmd()
                else
                    DoSimpleMsg(DSS, _('DOScmd is disabled. Enable it via API or set the environment variable DSS_CAPI_ALLOW_DOSCMD=1 before starting the process.'), 283);
            ord(Cmd.CvrtLoadshapes):
                DSS.DSSExecutive.DoCvrtLoadshapesCmd;
            ord(Cmd.vr):
                DSS.DSSExecutive.DoVarCmd;
{$IFDEF DSS_CAPI_PM}
            ord(Cmd.NewActor):
            begin
                New_Actor_Slot(DSS);
            end;
            ord(Cmd.ClearAll):
                DSS.DSSExecutive.DoClearAllCmd;
            ord(Cmd.Wait):
                if PMParent.Parallel_enabled then
                    Wait4Actors(DSS, ALL_ACTORS);
            ord(Cmd.SolveAll):
            begin
                PMParent.IsSolveAll := TRUE;
                for i := 0 to PMParent.NumOfActors - 1 do
                begin
                    PMParent.ActiveChild := PMParent.Children[i];
                    PMParent.ActiveChild.CmdResult := DoSetCmd(PMParent.ActiveChild, 1);
                end;
            end;
{$ELSE}
            ord(Cmd.ClearAll):
                DSS.DSSExecutive.DoClearCmd;
{$ENDIF}
            ord(Cmd.COMHelp):
            begin
                DoSimpleMsg(DSS, _('COMHelp is not available on DSS Extensions. You can download "OpenDSS_COM.chm" at https://sourceforge.net/p/electricdss/code/HEAD/tree/trunk/Version8/Distrib/x64/OpenDSS_COM.chm?format=raw as well as other example and documentation files from the official OpenDSS distribution at https://sourceforge.net/p/electricdss/code/HEAD/tree/trunk/Version8/Distrib/ and subfolders. Please see https://dss-extensions.org/ for further links.'), 999);
                DSS.CmdResult := 0;
            end;
        else
            if DSS.ActiveCircuit = NIL then
            begin
                DoSimpleMsg(DSS, _('You must create a new circuit object first: "new circuit.mycktname" to execute this command.'), 301);
                Exit;
            end;
        end;

        // Now check to see if this is a command or a property reference

        if ParamPointer = 0 then
        begin
            // If not a command or the command is unknown, THEN it could be a property of a circuit element

            // If a command or no text beFORe the = sign, THEN error
            if (Length(ParamName) = 0) or (Comparetext(paramName, 'command') = 0) then
            begin
                DoSimpleMsg(DSS, 'Unknown Command: "%s" %s', [Param, CRLF + DSS.Parser.CmdString], 302);
                DSS.CmdResult := 1;
            end
            else
            begin
                DSS.DSSExecutive.ParseObjName(ParamName, ObjName, PropName);
                if Length(ObjName) > 0 then
                    SetObject(DSS, ObjName);  // Set active element
                if DSS.ActiveDSSObject <> NIL then
                begin
                    // rebuild command line and pass to editor
                    // use quotes to ensure first parameter is interpreted OK after rebuild
                    DSS.Parser.CmdString := PropName + '="' + Param + '" ' + DSS.Parser.Remainder;
                    DSS.ActiveDSSClass.Edit(DSS.Parser);
                end;
            end;
            Exit;  // Done - don't need to do anything ELSE
        end;

        // Process the rest of the commands

        case ParamPointer of
            ord(Cmd.CalcIncMatrix):
            begin
                DSS.ActiveCircuit.Solution.Calc_Inc_Matrix();
            end;
            ord(Cmd.CalcIncMatrix_O):
            begin
                DSS.ActiveCircuit.Solution.Calc_Inc_Matrix_Org();
            end;
{$IFDEF DSS_CAPI_ADIAKOPTICS}
            ord(Cmd.Refine_BusLevels):
            begin
                DSS.ActiveCircuit.Get_paths_4_Coverage();
                DSS.GlobalResult := Format(_('%d new paths detected'), [length(DSS.ActiveCircuit.Path_Idx) - 1]);
            end;
{$ENDIF}
            ord(Cmd.CalcLaplacian):
            begin
                with DSS.ActiveCircuit.Solution do
                begin
                    Laplacian := IncMat.Transpose();          // Transposes the Incidence Matrix
                    Laplacian := Laplacian.multiply(IncMat);  // IncMatT*IncMat
                end;
            end;
            ord(Cmd.Edit):
                DSS.CmdResult := DSS.DSSExecutive.DoEditCmd;
            ord(Cmd.More), ord(Cmd.M), ord(Cmd.tilde):
                DSS.CmdResult := DSS.DSSExecutive.DoMoreCmd; // more , m, ~
            ord(Cmd.Select):
                DSS.CmdResult := DSS.DSSExecutive.DoSelectCmd;
            ord(Cmd.Save):
                DSS.CmdResult := DSS.DSSExecutive.DoSaveCmd; //'save';
            ord(Cmd.Show):
                DSS.CmdResult := DoShowCmd(DSS); //'show';
            ord(Cmd.Solve):
            begin
{$IFDEF DSS_CAPI_PM}
                PMParent.IsSolveAll := FALSE;
{$IFDEF DSS_CAPI_ADIAKOPTICS}
                DSS.ActiveCircuit.AD_Init := FALSE;
{$ENDIF}
{$ENDIF}
                DSS.CmdResult := DoSetCmd(DSS, 1);  // changed from DoSolveCmd; //'solve';
            end;
            ord(Cmd.Enable):
                DSS.CmdResult := DSS.DSSExecutive.DoEnableCmd;
            ord(Cmd.Disable):
                DSS.CmdResult := DSS.DSSExecutive.DoDisableCmd;
            ord(Cmd.Plot):
            begin
                DoPlotCmd(DSS);
                DSS.CmdResult := 0;
            end;
            ord(Cmd.Reset):
                DSS.CmdResult := DSS.DSSExecutive.DoResetCmd; //'resetmonitors';
            ord(Cmd.SetOpt):
                DSS.CmdResult := DoSetCmd(DSS, 0);  //'set WITH no solve'
            ord(Cmd.Dump):
                DSS.CmdResult := DSS.DSSExecutive.DoPropertyDump;
            ord(Cmd.Open):
                DSS.CmdResult := DSS.DSSExecutive.DoOpenCmd;
            ord(Cmd.Close):
                DSS.CmdResult := DSS.DSSExecutive.DoCloseCmd;

            ord(Cmd.questionmark):
                DSS.CmdResult := DSS.DSSExecutive.DoQueryCmd;
            ord(Cmd.Next):
                DSS.CmdResult := DSS.DSSExecutive.DoNextCmd;  // Advances time
            ord(Cmd.Panel):
                DoSimpleMsg(DSS, _('Command supported in DSS Extensions.'), 999);
            ord(Cmd.Sample):
                DSS.CmdResult := DSS.DSSExecutive.DoSampleCmd;
            // ord(Cmd.Clear): Begin ClearAllCircuits; DisposeDSSClasses; CreateDSSClasses; End;
            // ord(Cmd.About): DoAboutBox; 
            ord(Cmd.Calcvoltagebases):
                DSS.CmdResult := DSS.DSSExecutive.DoSetVoltageBases;
            ord(Cmd.SetkVBase):
                DSS.CmdResult := DSS.DSSExecutive.DoSetkVBase;
            ord(Cmd.BuildY):
                DSS.ActiveCircuit.InvalidateAllPCElements;  // FORce rebuilding of Y
            ord(Cmd.Get):
                DSS.CmdResult := DoGetCmd(DSS);
            ord(Cmd.Init):
                DSS.ActiveCircuit.Solution.SolutionInitialized := FALSE;
            ord(Cmd.Export):
                DSS.CmdResult := DoExportCmd(DSS);
            // ord(Cmd.Fileedit): DSS.CmdResult := DoFileEditCmd;
            ord(Cmd.Voltages):
                DSS.CmdResult := DSS.DSSExecutive.DovoltagesCmd(FALSE);
            ord(Cmd.Currents):
                DSS.CmdResult := DSS.DSSExecutive.DocurrentsCmd;
            ord(Cmd.Powers):
                DSS.CmdResult := DSS.DSSExecutive.DopowersCmd(0);
            ord(Cmd.Seqvoltages):
                DSS.CmdResult := DSS.DSSExecutive.DoseqvoltagesCmd;
            ord(Cmd.Seqcurrents):
                DSS.CmdResult := DSS.DSSExecutive.DoseqcurrentsCmd;
            ord(Cmd.Seqpowers):
                DSS.CmdResult := DSS.DSSExecutive.DoseqpowersCmd;
            ord(Cmd.Losses):
                DSS.CmdResult := DSS.DSSExecutive.DolossesCmd;
            ord(Cmd.Phaselosses):
                DSS.CmdResult := DSS.DSSExecutive.DophaselossesCmd;
            ord(Cmd.Cktlosses):
                DSS.CmdResult := DSS.DSSExecutive.DocktlossesCmd;
            ord(Cmd.Allocateloads):
                DSS.CmdResult := DSS.DSSExecutive.DoAllocateLoadsCmd;
            ord(Cmd.Formedit):
                DSS.CmdResult := DSS.DSSExecutive.DoFormEditCmd;
            ord(Cmd.Totals):
                DSS.CmdResult := DSS.DSSExecutive.DoMeterTotals;
            ord(Cmd.Capacity):
                DSS.CmdResult := DSS.DSSExecutive.DoCapacityCmd;
            // ord(Cmd.Classes): DSS.CmdResult := DoClassesCmd;
            // ord(Cmd.Userclasses): DSS.CmdResult := DoUserClassesCmd;
            ord(Cmd.Zsc):
                DSS.CmdResult := DSS.DSSExecutive.DoZscCmd(TRUE);
            ord(Cmd.Zsc10):
                DSS.CmdResult := DSS.DSSExecutive.DoZsc10cmd;
            ord(Cmd.ZscRefresh):
                DSS.CmdResult := DSS.DSSExecutive.DoZscRefresh;
            ord(Cmd.Ysc):
                DSS.CmdResult := DSS.DSSExecutive.DoZscCmd(FALSE);
            ord(Cmd.puvoltages):
                DSS.CmdResult := DSS.DSSExecutive.DovoltagesCmd(TRUE);
            ord(Cmd.VarValues):
                DSS.CmdResult := DSS.DSSExecutive.DoVarValuesCmd;
            ord(Cmd.Varnames):
                DSS.CmdResult := DSS.DSSExecutive.DoVarNamesCmd;
            ord(Cmd.Buscoords):
                DSS.CmdResult := DSS.DSSExecutive.DoBusCoordsCmd(FALSE);
            ord(Cmd.MakeBusList):
                with DSS.ActiveCircuit do
                    if BusNameRedefined then
                        ReprocessBusDefs;
            ord(Cmd.MakePosSeq):
                DSS.CmdResult := DSS.DSSExecutive.DoMakePosSeq;
            ord(Cmd.Reduce):
                DSS.CmdResult := DSS.DSSExecutive.DoReduceCmd;
            ord(Cmd.Interpolate):
                DSS.CmdResult := DSS.DSSExecutive.DoInterpolateCmd;
            ord(Cmd.TOP):
            begin
                DoSimpleMsg(DSS, _('TOP is not supported in DSS Extensions.'), 999);
                DSS.CmdResult := 0;
            end;
            ord(Cmd.Rotate):
                DSS.CmdResult := DSS.DSSExecutive.DoRotateCmd;
            ord(Cmd.Vdiff):
                DSS.CmdResult := DSS.DSSExecutive.DoVdiffCmd;
            ord(Cmd.Summary):
                DSS.CmdResult := DSS.DSSExecutive.DoSummaryCmd;
            ord(Cmd.Distribute):
                DSS.CmdResult := DSS.DSSExecutive.DoDistributeCmd;
            // ord(Cmd.DI_plot);
            // ord(Cmd.Comparecases);
            // ord(Cmd.YearlyCurves);
            // ord(Cmd.CD);
            ord(Cmd.Visualize):
                DSS.CmdResult := DSS.DSSExecutive.DoVisualizeCmd;
            ord(Cmd.CloseDI):
                DSS.CmdResult := DSS.DSSExecutive.DoCloseDICmd;
            ord(Cmd.Estimate):
                DSS.CmdResult := DSS.DSSExecutive.DoEstimateCmd;
            ord(Cmd.Reconductor):
                DSS.CmdResult := DSS.DSSExecutive.DoReconductorCmd;
            
            // Step solution commands
            ord(Cmd._InitSnap):
                DSS.ActiveCircuit.Solution.SnapShotInit;
            ord(Cmd._SolveNoControl):
                DSS.ActiveCircuit.Solution.SolveCircuit;
            ord(Cmd._SampleControls):
                DSS.ActiveCircuit.Solution.SampleControlDevices;
            ord(Cmd._DoControlActions):
                DSS.ActiveCircuit.Solution.DoControlActions;
            ord(Cmd._ShowControlQueue):
                DSS.ActiveCircuit.ControlQueue.ShowQueue(DSS.OutputDirectory + DSS.CircuitName_ + 'ControlQueue.csv');
            ord(Cmd._SolveDirect):
                DSS.ActiveCircuit.Solution.SolveDirect;
            ord(Cmd._SolvePFlow):
                DSS.ActiveCircuit.Solution.DoPFLOWsolution;
            ord(Cmd.AddBusMarker):
                DSS.CmdResult := DSS.DSSExecutive.DoAddMarkerCmd;
            ord(Cmd.Uuids):
                DSS.CmdResult := DSS.DSSExecutive.DoUuidsCmd;
            ord(Cmd.SetLoadAndGenKV):
                DSS.CmdResult := DSS.DSSExecutive.DoSetLoadAndGenKVCmd;
            // ord(Cmd.CvrtLoadshapes):;
            ord(Cmd.NodeDiff):
                DSS.CmdResult := DSS.DSSExecutive.DoNodeDiffCmd;
            ord(Cmd.Rephase):
                DSS.CmdResult := DSS.DSSExecutive.DoRephaseCmd;
            ord(Cmd.SetBusXY):
                DSS.CmdResult := DSS.DSSExecutive.DoSetBusXYCmd;
            ord(Cmd.UpdateStorage):
                DSS.CmdResult := DSS.DSSExecutive.DoUpDateStorageCmd;
            ord(Cmd.Obfuscate):
                Obfuscate(DSS);
            ord(Cmd.LatLongCoords):
                DSS.CmdResult := DSS.DSSExecutive.DoBusCoordsCmd(TRUE);   // swaps X and Y
            ord(Cmd.BatchEdit):
                DSS.CmdResult := DSS.DSSExecutive.DoBatchEditCmd;
            ord(Cmd.Pstcalc):
                DSS.CmdResult := DSS.DSSExecutive.DoPstCalc;
            ord(Cmd.Variable):
                DSS.CmdResult := DSS.DSSExecutive.DoValVarCmd;
            ord(Cmd.ReprocessBuses):
                DSS.ActiveCircuit.ReprocessBusDefs;
            ord(Cmd.ClearBusMarkers):
                DSS.ActiveCircuit.ClearBusMarkers;
            ord(Cmd.RelCalc):
                DSS.CmdResult := DSS.DSSExecutive.DoLambdaCalcs;   // Option: Assume Restoration
            ord(Cmd.Cleanup):
                DSS.ActiveCircuit.Solution.EndofTimeStepCleanup;
            ord(Cmd.FinishTimeStep):
                DSS.ActiveCircuit.Solution.FinishTimeStep;
            ord(Cmd.NodeList):
                DSS.CmdResult := DSS.DSSExecutive.DoNodeListCmd;
            ord(Cmd.GISCoords):
                ; // Do nothing here on DSS C-API. Just ignore it silently so files 
                  // saved with EPRI's version can be loaded more easily.
                  // OpenDSS-GIS is out of the scope proposed by DSS Extensions,
                  // but we could provide compatibility when/if OpenDSS-GIS becomes
                  // at least available to users outside of EPRI.
            ord(Cmd.Connect):
            begin
                DoSimpleMsg(DSS, _('Winsock TCP/IP connection is not supported in DSS Extensions'), 999);
                DSS.CmdResult := 0;
            end;
            ord(Cmd.Disconnect):
            begin
                DoSimpleMsg(DSS, _('Winsock TCP/IP connection is not supported in DSS Extensions'), 999);
                DSS.CmdResult := 0;
            end;
            ord(Cmd.Remove):
                DSS.DSSExecutive.DoRemoveCmd;
            ord(Cmd.ExportOverloads):
                if DSS.EnergyMeterClass.OV_MHandle <> nil then
                    CloseMHandler(DSS, DSS.EnergyMeterClass.OV_MHandle, DSS.EnergyMeterClass.DI_Dir + PathDelim + 'DI_Overloads' + '.csv', DSS.EnergyMeterClass.OV_Append);
{$IFDEF DSS_CAPI_PM}
            ord(Cmd.Abort):
                for i := 0 to High(PMParent.Children) do
                    PMParent.Children[i].SolutionAbort := TRUE;
            ord(Cmd.Clone):
                DoClone(DSS);
{$ENDIF}
{$IFDEF DSS_CAPI_ADIAKOPTICS}
            ord(Cmd.Tear_Circuit):
                ADiakoptics_Tearing(DSS, False);
            ord(Cmd.AggregateProfiles):
            begin
                DSS.Parser.NextParam;
                DSS.ActiveCircuit.AggregateProfiles(DSS.Parser.StrValue);
            end;
{$ENDIF}
            ord(Cmd.ExportVViolations):
                if DSS.EnergyMeterClass.VR_MHandle <> nil then
                    CloseMHandler(DSS, DSS.EnergyMeterClass.VR_MHandle, DSS.EnergyMeterClass.DI_Dir + PathDelim + 'DI_VoltExceptions' + '.csv', DSS.EnergyMeterClass.VR_Append);
            ord(Cmd.Zsc012):
                DSS.CmdResult := DSS.DSSExecutive.DoZsc012Cmd; // Get full symmetrical component transformation of Zsc
            ord(Cmd.AllPCEatBus):
            begin
                DSS.Parser.NextParam;
                DSS.GlobalResult  :=  DSS.ActiveCircuit.ReportPCEatBus(DSS.Parser.StrValue);
            end;
            ord(Cmd.AllPDEatBus):
            begin
                DSS.Parser.NextParam;
                DSS.GlobalResult  :=  DSS.ActiveCircuit.ReportPDEatBus(DSS.Parser.StrValue);
            end;
            ord(Cmd.TotalPowers): 
                DSS.CmdResult := DSS.DSSExecutive.DopowersCmd(1);
        else
       // Ignore excess parameters
        end;

    except
        On E: Exception do
            DoErrorMsg(DSS, 
                Format(_('ProcessCommand: Exception Raised While Processing DSS Command: %s'), [CRLF + DSS.Parser.CmdString]),
                E.Message, _('Error in command string or circuit data.'), 303);
    end;
{$IFNDEF DSS_CAPI_PM}
    DSS.ParserVars.Add('@result', DSS.GlobalResult)
{$ENDIF}
end;

procedure DisposeStrings;
var
    i: Integer;
begin
    for i := 1 to NumExecCommands do
    begin
        ExecCommand[i] := '';
    end;
end;

end.
