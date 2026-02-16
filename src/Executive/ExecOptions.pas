unit ExecOptions;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Command,
    DSSClass,
    ArrayDef;

type
{$SCOPEDENUMS ON}
    TExecOption = (
        INVALID = 0,
        typ = 1,
        element = 2,
        hour = 3,
        sec = 4,
        year = 5,
        frequency = 6,
        stepsize = 7,
        mode = 8,
        random = 9,
        number = 10,
        time = 11,
        cls = 12,
        obj = 13,
        circuit = 14,
        editor = 15,
        tolerance = 16,
        maxiterations = 17,
        h = 18,
        Loadmodel = 19,
        Loadmult = 20,
        normvminpu = 21,
        normvmaxpu = 22,
        emergvminpu = 23,
        emergvmaxpu = 24,
        pctmean = 25, // %mean
        pctstddev = 26, // %stddev
        LDCurve = 27,  // Load Duration Curve
        pctgrowth = 28,  // %growth -- default growth rate
        Genkw = 29,
        Genpf = 30,
        CapkVAR = 31,
        Addtype = 32,
        Allowduplicates = 33,
        Zonelock = 34,
        UEweight = 35,
        Lossweight = 36,
        UEregs = 37,
        Lossregs = 38,
        Voltagebases = 39,  //  changes the default voltage base rules
        Algorithm = 40,  //  changes the default voltage base rules
        Trapezoidal = 41,
        Autobuslist = 42,  // array of bus names to include in auto add solutions
        Controlmode = 43,
        Tracecontrol = 44,
        Genmult = 45,
        Defaultdaily = 46,
        Defaultyearly = 47,
        Allocationfactors = 48,
        Cktmodel = 49,
        Pricesignal = 50,
        Pricecurve = 51,
        Terminal = 52,
        Basefrequency = 53,
        Harmonics = 54,
        Maxcontroliter = 55,
        Bus = 56,
        Datapath = 57,
        KeepList = 58,
        ReduceOption = 59,
        DemandInterval = 60,
        pctNormal = 61, // %Normal
        DIVerbose = 62,
        Casename = 63,
        Markercode = 64,
        Nodewidth = 65,
        Log = 66,
        Recorder = 67,
        Overloadreport = 68,
        Voltexceptionreport = 69,
        Cfactors = 70,
        ShowExport = 71,
        Numallociterations = 72,
        DefaultBaseFrequency = 73,
        Markswitches = 74,
        Switchmarkercode = 75,
        Daisysize = 76,
        Marktransformers = 77,
        TransMarkerCode = 78,
        TransMarkerSize = 79,
        LoadShapeClass = 80,
        EarthModel = 81,
        QueryLog = 82,
        MarkCapacitors = 83,
        MarkRegulators = 84,
        MarkPVSystems = 85,
        MarkStorage = 86,
        CapMarkerCode = 87,
        RegMarkerCode = 88,
        PVMarkerCode = 89,
        StoreMarkerCode = 90,
        CapMarkerSize = 91,
        RegMarkerSize = 92,
        PVMarkerSize = 93,
        StoreMarkerSize = 94,
        NeglectLoadY = 95,
        MarkFuses = 96,
        FuseMarkerCode = 97,
        FuseMarkerSize = 98,
        MarkReclosers = 99,
        RecloserMarkerCode = 100,
        RecloserMarkerSize = 101,
        RegistryUpdate = 102,
        MarkRelays = 103,
        RelayMarkerCode = 104,
        RelayMarkerSize = 105,
        ProcessTime = 106,
        TotalTime = 107,
        StepTime = 108,
        SampleEnergyMeters = 109,
        MinIterations = 110, // default is 2
        OpenDSSViewer = 111,
        KeepLoad = 112,
        Zmag = 113,
        SeasonRating = 114,
        SeasonSignal = 115,
        LineTypes,
        EventLogDefault,
        LongLineCorrection,
        ShowReports
        ,
        NumCPUs,
        NumCores,
        NumActors,
        ActiveActor,
        CPU,
        ActorProgress,
        Parallel,
        ConcatenateReports,
        NUMANodes,
{$IFDEF DSS_CAPI_ADIAKOPTICS}
        Coverage,
        Num_SubCircuits,
        ADiakoptics,
        LinkBranches,
{$ENDIF}
        IgnoreGenQLimits,
        NCIMQGain,
        StateVar,
        PyPath,
        IterNumber,
        CtrlIterNumber,
        InjCurrent,
        ITerminal,
        YPrim,
        IntegrationFlag,
        AllowForms,
        AllowProgressBar
    );
{$SCOPEDENUMS OFF}

const
    NumExecOptions = ord(High(TExecOption));

function DoGetCmd(MainDSS: TDSSContext): Integer;
function DoSetCmd(MainDSS: TDSSContext; SolveOption: Integer): Integer;
function DoSetCmd_NoCircuit(MainDSS: TDSSContext): Boolean;  // Set Commands that do not require a circuit
function DoGetCmd_NoCircuit(MainDSS: TDSSContext): Boolean;  // Get Commands that do not require a circuit
procedure DefineOptions(var ExecOption: ArrayOfString);

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    ParserDel,
    Math,
    Executive,
    ExecHelper,
    LoadShape,
    Line,
    Utilities,
    Sysutils,
    Solution,
    Energymeter,
    Dynamics,
    DSSHelper,
    StrUtils,
    Circuit,
    CktElement,
    PCElement,
    DSSUcomplex,
    TypInfo

{$IFDEF DSS_CAPI_ADIAKOPTICS}
    , Diakoptics
{$ENDIF}
    ;

type
    Opt = TExecOption;

procedure DefineOptions(var ExecOption: ArrayOfString);
var
    info: Pointer;
    i: Integer;
    name: String;
begin
    info := TypeInfo(TExecOption);
    SetLength(ExecOption, NumExecOptions);
    for i := 1 to NumExecOptions do
    begin
        name := ReplaceStr(GetEnumName(info, i), 'pct', '%');
        if name = 'cls' then
            name := 'class'
        else if name = 'typ' then
            name := name + 'e'
        else if name = 'obj' then
            name := 'object';

        ExecOption[i - 1] := name;
    end;
end;

function DoSetCmd_NoCircuit(MainDSS: TDSSContext): Boolean;  // Set Commands that do not require a circuit
// This is for setting global options that do not require an active circuit
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    PMParent, DSS: TDSSContext;
begin
    PMParent := MainDSS.GetPrime();
    DSS := MainDSS.ActiveChild;

    Result := TRUE;
     // Continue parsing command line
    ParamPointer := 0;
    ParamName := DSS.Parser.NextParam;
    Param := DSS.Parser.MakeString();
    while Length(Param) > 0 do
    begin
        if Length(ParamName) = 0 then
            Inc(ParamPointer)
        else
            ParamPointer := DSS.DSSExecutive.OptionList.GetCommand(ParamName);

        case ParamPointer of
            0:
                DoSimpleMsg(DSS, 'Unknown parameter "%s" for Set Command', [ParamName], 130);
            15:
                DefaultEditor := Param;     // 'Editor='
            57:
                SetDataPath(DSS, Param);  // Set a legal data path
            67:
                DSS.DSSExecutive.SetRecorderOn(InterpretYesNo(Param));
            73:
                DSS.DefaultBaseFreq := DSS.Parser.MakeDouble();
            102:
                DoSimpleMsg(DSS, _('This is not supported in the AltDSS engine.'), 25040101);
            111:
                DoSimpleMsg(DSS, _('This is not supported in the AltDSS engine.'), 25040101);
            ord(Opt.ActiveActor):
                if DSS.Parser.MakeString() = '*' then
                begin
                    PMParent.AllActors := TRUE;
                    PMParent.ActiveChildIndex := 0;
                    PMParent.ActiveChild := PMParent;
                end
                else
                begin
                    if (DSS.Parser.MakeInteger() > 0) and (DSS.Parser.MakeInteger() <= PMParent.NumOfActors()) then
                    begin
                        PMParent.ActiveChildIndex := DSS.Parser.MakeInteger() - 1;
                        PMParent.ActiveChild := PMParent.Children[PMParent.ActiveChildIndex];
                        PMParent.AllActors := FALSE;
                    end
                    else
                    begin
                        DoSimpleMsg(DSS, _('The actor does not exist'), 7002);
                    end;
                end;
            ord(Opt.CPU):
                if DSS.Parser.MakeInteger() < CPU_Cores then
                begin
                    DSS.CPU := DSS.Parser.MakeInteger();
                    if DSS.ActorThread() <> NIL then
                        DSS.ActorThread().SetCPU(DSS.CPU);
                end
                else
                begin
                    DoSimpleMsg(DSS, _('The CPU does not exist'), 7003);
                end;
            ord(Opt.Parallel):
                PMParent.Parallel_enabled := InterpretYesNo(Param);
            ord(Opt.ConcatenateReports):
                PMParent.ConcatenateReports := InterpretYesNo(Param);
            ord(Opt.EventLogDefault):
                DSS.EventLogDefault := InterpretYesNo(Param);
        else
            begin
                DoSimpleMsg(DSS, _('You must create a new circuit object first: "new circuit.mycktname" to execute this Set command.'), 301);
                Result := FALSE;  // Indicate that we could not process all set command
                Exit;
            end;
        end;

        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.MakeString();
    end; {WHILE}

end;

function interpretTimeStepSize(DSS: TDSSContext; const s: String): Double;
// Return stepsize in seconds
var
    Code: Integer;
    ch: Char;
    s2: String;

begin
    // Try to convert and see if we get an error
    val(s, Result, Code);
    if Code = 0 then
        Exit;  // Only a number was specified, so must be seconds

    // Error occurred so must have a units specifier
    ch := s[Length(s)];  // get last character
    s2 := copy(s, 1, Length(s) - 1);
    Val(S2, Result, Code);
    if Code > 0 then
    begin   // check for error
        Result := DSS.ActiveCircuit.Solution.DynaVars.h; // Don't change it
        DoSimpleMsg(DSS, 'Error in specification of StepSize: %s', [s], 99933);
        Exit;
    end;
    case ch of
        'h':
            Result := Result * 3600.0;
        'm':
            Result := Result * 60.0;
        's': ; // Do nothing
    else
        Result := DSS.ActiveCircuit.Solution.DynaVars.h; // Don't change it
        DoSimpleMsg(DSS, 'Error in specification of StepSize: "%s". Units can only be h, m, or s (single char only)', [s], 99934);
    end;
end;

procedure parseIntArray(DSS: TDSSContext; var iarray: ArrayOfInteger; const s: String);
var
    param: String;
    i, count: Integer;
begin
    // Parse the line once to get the count of tokens on string, S
    DSS.AuxParser.SetCmdString(S);
    count := 0;
    repeat
        DSS.AuxParser.NextParam();
        Param := DSS.AuxParser.MakeString();
        if Length(Param) > 0 then
            Inc(count);
    until Length(Param) = 0;

    // reallocate iarray  to new size
    SetLength(iarray, count);

    // Parse again for real
    DSS.AuxParser.SetCmdString(S);
    for i := 0 to count - 1 do
    begin
        DSS.AuxParser.NextParam();
        iarray[i] := DSS.AuxParser.MakeInteger();
    end;
end;

function DoSetCmd(MainDSS: TDSSContext; SolveOption: Integer): Integer;
// Set DSS Options
// Solve Command is re-routed here first to set options beFORe solving
var
    i: Integer;
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    TestLoadShapeObj: TLoadShapeObj;
    LineObj: TLineObj;
    TmpStr: String;
    cktElem: TDSSCktElement;
    pce: TPCElement;
    norder: Integer;
    cvalues: pComplexArray;
    PMParent, DSS: TDSSContext;
begin
    PMParent := MainDSS.GetPrime();
    DSS := MainDSS.ActiveChild;
    Result := 0;

    if DSS.ActiveCircuit = NIL then
    begin
        if not DoSetCmd_NoCircuit(DSS) then
            Result := 1;
            
        Exit;
    end;
    
     // Continue parsing command line
    ParamPointer := 0;
    ParamName := DSS.Parser.NextParam;
    Param := DSS.Parser.MakeString();
    while Length(Param) > 0 do
    begin
        if Length(ParamName) = 0 then
            Inc(ParamPointer)
        else
            ParamPointer := DSS.DSSExecutive.OptionList.GetCommand(ParamName);

        case ParamPointer of
            0:
                DoSimpleMsg(DSS, 'Unknown parameter "%s" for Set Command', [ParamName], 130);
            1, 12:
                SetObjectClass(DSS, Param);
            2, 13:
                SetObject(DSS, Param);
            3:
            begin
                DSS.ActiveCircuit.Solution.DynaVars.intHour := DSS.Parser.MakeInteger();
                DSS.SyncSeasonalRatingIdx();
            end;
            4:
                DSS.ActiveCircuit.Solution.DynaVars.t := DSS.Parser.MakeDouble();
            5:
                with DSS.ActiveCircuit do
                begin
                    Solution.SetYear(DSS.Parser.MakeInteger());
                    DefaultGrowthFactor := IntPower(DefaultGrowthRate, (Solution.Year() - 1));
                end;
            6:
                DSS.ActiveCircuit.Solution.SetFrequency(DSS.Parser.MakeDouble());
            7, 18:
                with DSS.ActiveCircuit do
                begin
                    Solution.DynaVars.h := interpretTimeStepSize(DSS, Param);
                    Solution.IntervalHrs := Solution.DynaVars.h/3600.0;
                end;
            ord(Opt.Mode):
                DSS.ActiveCircuit.Solution.SetMode(TSolveMode(DSS.SolveModeEnum.StringToOrdinal(Param)));  // see DSSGlobals
            9:
                DSS.ActiveCircuit.Solution.RandomType := DSS.RandomModeEnum.StringToOrdinal(Param);
            10:
                DSS.ActiveCircuit.Solution.NumberOfTimes := DSS.Parser.MakeInteger();
            11:
                DSS.DSSExecutive.Set_Time;
            14:
                DSS.DSSExecutive.SetActiveCircuit(Param);
            15:
                DefaultEditor := Param;     // 'Editor='
            16:
                DSS.ActiveCircuit.Solution.ConvergenceTolerance := DSS.Parser.MakeDouble();
            17:
                DSS.ActiveCircuit.Solution.MaxIterations := DSS.Parser.MakeInteger();
            19:
                with DSS.ActiveCircuit.solution do
                begin
                    DefaultLoadModel := DSS.DefaultLoadModelEnum.StringToOrdinal(Param); // for reverting to last on specified
                    LoadModel := DefaultLoadModel;
                end;
            ord(TExecOption.Loadmult):
            begin
                DSS.ActiveCircuit.SetLoadMultiplier(DSS.Parser.MakeDouble());  // Set using LoadMultiplier property
                DSS.ActiveCircuit.Solution.InvalidateSystemY();
            end;
            21:
                DSS.ActiveCircuit.NormalMinVolts := DSS.Parser.MakeDouble();
            22:
                DSS.ActiveCircuit.NormalMaxVolts := DSS.Parser.MakeDouble();
            23:
                DSS.ActiveCircuit.EmergMinVolts := DSS.Parser.MakeDouble();
            24:
                DSS.ActiveCircuit.EmergMaxVolts := DSS.Parser.MakeDouble();
            25:
                DSS.ActiveCircuit.DefaultDailyShapeObj.SetMean(DSS.Parser.MakeDouble() / 100.0);
            26:
                DSS.ActiveCircuit.DefaultDailyShapeObj.SetStdDev(DSS.Parser.MakeDouble() / 100.0);
            27:
                with DSS.ActiveCircuit do
                begin
                    LoadDurCurveObj := DSS.LoadShapeClass.Find(Param);
                    if LoadDurCurveObj = NIL then
                        DoSimpleMsg(DSS, _('Load-Duration Curve not found.'), 131);
                end;
            28:
                with DSS.ActiveCircuit do
                begin
                    DefaultGrowthRate := 1.0 + DSS.Parser.MakeDouble() / 100.0;
                    DefaultGrowthFactor := IntPower(DefaultGrowthRate, (Solution.Year() - 1));
                end;
            29:
                DSS.ActiveCircuit.AutoAddObj.GenkW := DSS.Parser.MakeDouble();
            30:
                DSS.ActiveCircuit.AutoAddObj.GenPF := DSS.Parser.MakeDouble();
            31:
                DSS.ActiveCircuit.AutoAddObj.CapkVAR := DSS.Parser.MakeDouble();
            32:
                DSS.ActiveCircuit.AutoAddObj.AddType := DSS.AddTypeEnum.StringToOrdinal(Param);
            33:
                DSS.ActiveCircuit.DuplicatesAllowed := InterpretYesNo(Param);
            34:
                DSS.ActiveCircuit.ZonesLocked := InterpretYesNo(Param);
            35:
                DSS.ActiveCircuit.UEWeight := DSS.Parser.MakeDouble();
            36:
                DSS.ActiveCircuit.LossWeight := DSS.Parser.MakeDouble();
            37:
                parseIntArray(DSS, DSS.ActiveCircuit.UERegs, Param);
            38:
                parseIntArray(DSS, DSS.ActiveCircuit.LossRegs, Param);
            39:
                DSS.DSSExecutive.DoLegalVoltageBases;
            40:
            begin
                DSS.ActiveCircuit.Solution.Algorithm := DSS.SolveAlgEnum.StringToOrdinal(Param);
                if DSS.ActiveCircuit.Solution.Algorithm = NCIMSOLVE then
                    DSS.ActiveCircuit.Solution.NCIM_Ready := false;
            end;
            41:
                DSS.ActiveCircuit.TrapezoidalIntegration := InterpretYesNo(Param);
            42:
                DSS.DSSExecutive.DoAutoAddBusList(Param);
            43:
                with DSS.ActiveCircuit.Solution do
                begin
                    ControlMode := DSS.ControlModeEnum.StringToOrdinal(Param);
                    DefaultControlMode := ControlMode;  // always revert to last one specified in a script
{$IFDEF DSS_CAPI_ADIAKOPTICS}
                    if PMParent.ADiakoptics and (PMParent.ActiveChildIndex = 0) then
                        SendADCommandToActors(PMParent, GETCTRLMODE);
{$ENDIF}
                end;
            44:
                DSS.ActiveCircuit.ControlQueue.SetTraceLog(InterpretYesNo(Param));
            45:
                DSS.ActiveCircuit.GenMultiplier := DSS.Parser.MakeDouble();
            46:
            begin
                TestLoadShapeObj := DSS.LoadShapeClass.Find(Param);
                if TestLoadShapeObj <> NIL then
                    DSS.ActiveCircuit.DefaultDailyShapeObj := TestLoadShapeObj;
            end;
            47:
            begin
                TestLoadShapeObj := DSS.LoadShapeClass.Find(Param);
                if TestLoadShapeObj <> NIL then
                    DSS.ActiveCircuit.DefaultYearlyShapeObj := TestLoadShapeObj;
            end;
            48:
                DSS.DSSExecutive.DoSetAllocationFactors(DSS.Parser.MakeDouble());
            49:
                DSS.ActiveCircuit.PositiveSequence := Boolean(DSS.CktModelEnum.StringToOrdinal(Param));
            50:
                DSS.ActiveCircuit.PriceSignal := DSS.Parser.MakeDouble();
            51:
                with DSS.ActiveCircuit do
                begin
                    PriceCurveObj := DSS.PriceShapeClass.Find(Param);
                    if PriceCurveObj = NIL then
                        DoSimpleMsg(DSS, 'Priceshape.%s not found.', [param], 132);
                end;
            52:
                with DSS.ActiveCircuit do
                    if ActiveCktElement() <> NIL then
                        with ActiveCktElement() do
                        begin
                            SetActiveTerminalIdx(DSS.Parser.MakeInteger());
                            SetActiveBus(DSS, StripExtension(Getbus(ActiveTerminalIdx())));   // bus connected to terminal
                        end;
            53:
            begin
                DSS.ActiveCircuit.Fundamental := DSS.Parser.MakeDouble();     // Set Base Frequency for system (used henceforth)
                DSS.ActiveCircuit.Solution.SetFrequency(DSS.Parser.MakeDouble());
            end;
            54:
                DSS.DSSExecutive.DoHarmonicsList(Param);
            55:
            begin
                DSS.ActiveCircuit.Solution.MaxControlIterations := DSS.Parser.MakeInteger();
{$IFDEF DSS_CAPI_ADIAKOPTICS}
                if PMParent.ADiakoptics and (PMParent.ActiveChildIndex = 0) then
                    SendADCommandToActors(PMParent, GETCTRLMODE);
{$ENDIF}
            end;
            56:
                Result := SetActiveBus(DSS, Param);   // See DSSGlobals
            57:
                SetDataPath(DSS, Param);  // Set a legal data path
            58:
                DSS.DSSExecutive.DoKeeperBusList(Param);
            59:
                DSS.DSSExecutive.DoSetReduceStrategy(param);
            60:
                DSS.EnergyMeterClass.SetSaveDemandInterval(InterpretYesNo(Param));
            61:
            begin
                DSS.ActiveCircuit.PctNormalFactor := DSS.Parser.MakeDouble();
                DSS.DSSExecutive.DoSetNormal(DSS.ActiveCircuit.PctNormalFactor);
            end;
            62:
                DSS.EnergyMeterClass.SetDIVerbose(InterpretYesNo(Param));
            63:
                DSS.ActiveCircuit.SetCaseName(DSS.Parser.MakeString());
            64:
                DSS.ActiveCircuit.NodeMarkerCode := DSS.Parser.MakeInteger();
            65:
                DSS.ActiveCircuit.NodeMarkerWidth := DSS.Parser.MakeInteger();
            66:
                DSS.ActiveCircuit.LogEvents := InterpretYesNo(Param);
            67:
                DSS.DSSExecutive.SetRecorderOn(InterpretYesNo(Param));
            68:
                DSS.EnergyMeterClass.Do_OverloadReport := InterpretYesNo(Param);
            ord(Opt.Voltexceptionreport):
                DSS.EnergyMeterClass.Do_VoltageExceptionReport := InterpretYesNo(Param);
            70:
                DSS.DSSExecutive.DoSetCFactors(DSS.Parser.MakeDouble());
            71:
                DSS.AutoShowExport := InterpretYesNo(Param);
            72:
                DSS.MaxAllocationIterations := DSS.Parser.MakeInteger();
            73:
            begin
                DSS.DefaultBaseFreq := DSS.Parser.MakeDouble();
                DSS.ActiveCircuit.Fundamental := DSS.Parser.MakeDouble();     // Set Base Frequency for system (used henceforth)
                DSS.ActiveCircuit.Solution.SetFrequency(DSS.Parser.MakeDouble());
            end;
            74:
                DSS.ActiveCircuit.MarkSwitches := InterpretYesNo(Param);
            75:
                DSS.ActiveCircuit.SwitchMarkerCode := DSS.Parser.MakeInteger();
            76:
                DSS.DaisySize := DSS.Parser.MakeDouble();
            77:
                DSS.ActiveCircuit.MarkTransformers := InterpretYesNo(Param);
            78:
                DSS.ActiveCircuit.TransMarkerCode := DSS.Parser.MakeInteger();
            79:
                DSS.ActiveCircuit.TransMarkerSize := DSS.Parser.MakeInteger();
            80:
                DSS.ActiveCircuit.ActiveLoadShapeClass := DSS.LoadShapeClassEnum.StringToOrdinal(Param);
            81:
                DSS.DefaultEarthModel := DSS.EarthModelEnum.StringToOrdinal(Param);
            82:
            begin
                DSS.LogQueries := InterpretYesNo(Param);
                if DSS.LogQueries then
                    ResetQueryLogFile(DSS);
            end;
            83:
                DSS.ActiveCircuit.MarkCapacitors := InterpretYesNo(Param);
            84:
                DSS.ActiveCircuit.MarkRegulators := InterpretYesNo(Param);
            85:
                DSS.ActiveCircuit.MarkPVSystems := InterpretYesNo(Param);
            86:
                DSS.ActiveCircuit.MarkStorage := InterpretYesNo(Param);
            87:
                DSS.ActiveCircuit.CapMarkerCode := DSS.Parser.MakeInteger();
            88:
                DSS.ActiveCircuit.RegMarkerCode := DSS.Parser.MakeInteger();
            89:
                DSS.ActiveCircuit.PVMarkerCode := DSS.Parser.MakeInteger();
            90:
                DSS.ActiveCircuit.StoreMarkerCode := DSS.Parser.MakeInteger();
            91:
                DSS.ActiveCircuit.CapMarkerSize := DSS.Parser.MakeInteger();
            92:
                DSS.ActiveCircuit.RegMarkerSize := DSS.Parser.MakeInteger();
            93:
                DSS.ActiveCircuit.PVMarkerSize := DSS.Parser.MakeInteger();
            94:
                DSS.ActiveCircuit.StoreMarkerSize := DSS.Parser.MakeInteger();
            95:
                DSS.ActiveCircuit.NeglectLoadY := InterpretYesNo(Param);
            96:
                DSS.ActiveCircuit.MarkFuses := InterpretYesNo(Param);
            97:
                DSS.ActiveCircuit.FuseMarkerCode := DSS.Parser.MakeInteger();
            98:
                DSS.ActiveCircuit.FuseMarkerSize := DSS.Parser.MakeInteger();
            99:
                DSS.ActiveCircuit.MarkReclosers := InterpretYesNo(Param);
            100:
                DSS.ActiveCircuit.RecloserMarkerCode := DSS.Parser.MakeInteger();
            101:
                DSS.ActiveCircuit.RecloserMarkerSize := DSS.Parser.MakeInteger();
            102:
                DoSimpleMsg(DSS, _('This is not supported in the AltDSS engine.'), 25040101);
            103:
                DSS.ActiveCircuit.MarkRelays := InterpretYesNo(Param);
            104:
                DSS.ActiveCircuit.RelayMarkerCode := DSS.Parser.MakeInteger();
            105:
                DSS.ActiveCircuit.RelayMarkerSize := DSS.Parser.MakeInteger();
            107:
                DSS.ActiveCircuit.Solution.Total_Time_Elapsed := DSS.Parser.MakeDouble();
            109:
                DSS.ActiveCircuit.Solution.SampleTheMeters := InterpretYesNo(Param);
            110:
                DSS.ActiveCircuit.Solution.MinIterations := DSS.Parser.MakeInteger();
            111:
                DoSimpleMsg(DSS, _('This is not supported in the AltDSS engine.'), 25040101);
            112:
                DSS.ActiveCircuit.ReduceLateralsKeepLoad := InterpretYesNo(Param);
            113:
                DSS.ActiveCircuit.ReductionZmag := DSS.Parser.MakeDouble();
            114:
                begin
                    DSS.SeasonalRating := InterpretYesNo(Param);
                    DSS.SyncSeasonalRatingIdx();
                end;
            115:
                begin
                    DSS.SeasonSignalObj := DSS.XYCurveClass.Find(Param);
                    DSS.SyncSeasonalRatingIdx();
                    if DSS.SeasonSignalObj = NIL then
                    begin
                        DoSimpleMsg(DSS, '"XYCurve.%s" not found. Please create it before setting it as SeasonSignal.', [param], 132);
                    end;
                end;
            ord(Opt.ActiveActor):
                if DSS.Parser.MakeString() = '*' then
                begin
                    PMParent.AllActors := TRUE;
                    PMParent.ActiveChildIndex := 0;
                    PMParent.ActiveChild := PMParent;
                end
                else
                begin
                    if (DSS.Parser.MakeInteger() > 0) and (DSS.Parser.MakeInteger() <= PMParent.NumOfActors()) then
                    begin
                        PMParent.ActiveChildIndex := DSS.Parser.MakeInteger() - 1;
                        PMParent.ActiveChild := PMParent.Children[PMParent.ActiveChildIndex];
                        PMParent.AllActors := FALSE;
                    end
                    else
                    begin
                        DoSimpleMsg(DSS, _('The actor does not exist'), 7002);
                    end;
                end;
            ord(Opt.CPU):
                if DSS.Parser.MakeInteger() < CPU_Cores then
                begin
                    DSS.CPU := DSS.Parser.MakeInteger();
                    if DSS.ActorThread() <> NIL then
                        DSS.ActorThread().SetCPU(DSS.CPU);
                end
                else
                begin
                    DoSimpleMsg(DSS, _('The CPU does not exist'), 7003);
                end;
            ord(Opt.Parallel):
                PMParent.Parallel_enabled := InterpretYesNo(Param);
            ord(Opt.ConcatenateReports):
                PMParent.ConcatenateReports := InterpretYesNo(Param);
            ord(Opt.EventLogDefault):
                DSS.EventLogDefault := InterpretYesNo(Param);
            ord(Opt.LongLineCorrection):
                DSS.ActiveCircuit.LongLineCorrection := InterpretYesNo(Param);
            ord(Opt.ShowReports):
                DSS.AutoDisplayShowReport := InterpretYesNo(Param);
            ord(Opt.AllowForms):
                NoFormsAllowed := not InterpretYesNo(Param);
            ord(Opt.AllowProgressBar):
                NoProgressBarFormAllowed := not InterpretYesNo(Param);
{$IFDEF DSS_CAPI_ADIAKOPTICS}
            ord(Opt.Coverage):
                DSS.ActiveCircuit.Coverage := DSS.Parser.MakeDouble();
            ord(Opt.Num_SubCircuits):
                DSS.ActiveCircuit.Num_SubCkts := DSS.Parser.MakeInteger();
            ord(Opt.ADiakoptics):
            begin
                if InterpretYesNo(Param) then
                    ADiakopticsInit(DSS)  // Initalizes the parallel environment if enabled
                else
                    DSS.ADiakoptics := FALSE;
            end;
{$ENDIF}
            ord(Opt.IgnoreGenQLimits):
                DSS.ActiveCircuit.Solution.NCIM_IgnoreQLimit := InterpretYesNo(Param);
            ord(Opt.NCIMQGain):
                DSS.ActiveCircuit.Solution.NCIM_GenGain := DSS.Parser.MakeDouble();
            ord(Opt.StateVar):
            begin
                DSS.Parser.NextParam;
                TmpStr := DSS.Parser.MakeString();
                if DSS.ActiveCircuit.SetElementActive(TmpStr) = 0 then
                begin
                    DoSimpleMsg(DSS, 'Object "%s" not found', [TmpStr], 7100);
                    Exit;
                end;

                if not (DSS.ActiveCircuit.ActiveCktElement() is TPCElement) then
                begin
                    DoSimpleMsg(DSS, 'Object "%s" is not a valid PC element.', [DSS.ActiveCircuit.ActiveCktElement.FullName()], 7103);
                    Exit;
                end;
                pce := DSS.ActiveCircuit.ActiveCktElement() as TPCElement;

                if pce.NumVariables() = 0 then
                begin
                    DoSimpleMsg(DSS, 'Object "%s" is not a valid element for this command. Only a selection of PC elements have state variables.', [TmpStr], 7101);
                    Exit;
                end;

                DSS.Parser.NextParam;
                TmpStr := LowerCase(DSS.Parser.MakeString());

                // Search for the variable within the object
                i := pce.LookupVariable(TmpStr, true);
                if i < 0 then
                begin
                    DoSimpleMsg(DSS, 'State variable "%s" not found in "%s".', [TmpStr, pce.FullName()], 7102);
                    Exit;
                end;

                // Once found, modifies the value
                DSS.Parser.NextParam;
                pce.SetVariable(i, DSS.Parser.MakeDouble());
            end;
            ord(Opt.PyPath):
            begin
                DoSimpleMsg(DSS, _('This is not supported in the AltDSS engine.'), 25040101);
                Exit;
            end;
            ord(Opt.IterNumber):
            begin
                DoSimpleMsg(DSS, _('This value is read-only.'), 25040103);
                Exit;
            end;
            ord(Opt.CtrlIterNumber):
            begin
                DoSimpleMsg(DSS, _('This value is read-only.'), 25040103);
                Exit;
            end;
            ord(Opt.InjCurrent):
            begin
                cktElem := DSS.ActiveCircuit.ActiveCktElement;
                if (cktElem = NIL) or ((cktElem.DSSObjType and BASECLASSMASK) <> PC_ELEMENT) then
                begin
                    DoSimpleMsg(DSS, 'Active element (%s) is not a PCElement.', [FullNameIfNotNil(cktElem)], 3002);
                    Exit;
                end;
                pce := TPCElement(cktElem);
                //TODO: error out if different number of elements provided?
                DSS.Parser.ParseAsComplexVector(pce.NPhases, pce.InjCurrent);
                Include(pce.Flags, Flg.ForceInjCurrents); // Force use of the currents provided by the user
            end;
            ord(Opt.ITerminal):
            begin
                cktElem := DSS.ActiveCircuit.ActiveCktElement;
                if (cktElem = NIL) or ((cktElem.DSSObjType and BASECLASSMASK) <> PC_ELEMENT) then
                begin
                    DoSimpleMsg(DSS, 'Active element (%s) is not a PCElement.', [FullNameIfNotNil(cktElem)], 3002);
                    Exit;
                end;
                pce := TPCElement(cktElem);
                //TODO: error out if different number of elements provided?
                DSS.Parser.ParseAsComplexVector(pce.NPhases, pce.Iterminal);
                pce.SetITerminalUpdated(true);
                Include(pce.Flags, Flg.ForceInjCurrents); // Force use of the currents provided by the user
            end;
            ord(Opt.YPrim):
            begin
                cktElem := DSS.ActiveCircuit.ActiveCktElement;
                if (cktElem = NIL) or ((cktElem.DSSObjType and BASECLASSMASK) <> PC_ELEMENT) then
                begin
                    DoSimpleMsg(DSS, 'Active element (%s) is not a PCElement.', [FullNameIfNotNil(cktElem)], 3003);
                    Exit;
                end;
                pce := TPCElement(cktElem);
                cvalues := pce.YPrim.GetValuesArrayPtr(norder);
                if (DSS.Parser.ParseAsComplexMatrix(norder, cvalues)) <> norder then
                begin
                    // Note: we'd need to keep a copy of the old matrix here to emulate the behavior on EPRI's impl.,
                    // but since this is an error state...
                    DoSimpleMsg(DSS, _('The size of the matrix provided does not match with the number of conductors of the active PCE.'), 3004);
                    Exit;
                end;
                pce.SetYprimInvalid(false);
                Include(pce.Flags, Flg.ForceYPrim); // Force use of the YPrim provided by the user
            end;
            ord(Opt.IntegrationFlag):
            begin
                DoSimpleMsg(DSS, _('This value is read-only.'), 25040103);
                Exit;
            end;
        else
           // Ignore excess parameters
           //TODO: warn about excess parameters
        end;

        case ParamPointer of
            3, 4:
                DSS.ActiveCircuit.Solution.Update_dblHour();
            ord(Opt.LongLineCorrection):
                with DSS.ActiveCircuit Do
                begin
                    for LineObj in Lines do
                    begin
                        if LineObj.Enabled() and LineObj.SymComponentsModel then
                            LineObj.SetYPrimInvalid(true);
                    end;
                end;            
        end;

        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.MakeString();
    end; {WHILE}

    if SolveOption = 1 then
        DSS.DSSExecutive.DoSolveCmd;
end;

function DoGetCmd(MainDSS: TDSSContext): Integer;
// Get DSS Options Reguest and put it in Global Result string
// may be retrieved by Result property of the DSSText interface
var
    ParamPointer, i: Integer;
    ParamName: String;
    Param: String;
    TmpStr: String;
    cktElem: TDSSCktElement;
    pce: TPCElement;
    PMParent, DSS: TDSSContext;
    // ckt: TDSSCircuit;
begin
    PMParent := MainDSS.GetPrime();
    DSS := MainDSS.ActiveChild;

    Result := 0;
    try
        DSS.GlobalResult := '';  //initialize for appending

        // Continue parsing command line
        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.MakeString();
        // there will be no named paramters in this command and the params
        // themselves will be the parameter name to return
        while Length(Param) > 0 do
        begin
            ParamPointer := DSS.DSSExecutive.OptionList.GetCommand(Param);

            case ParamPointer of
                0:
                    DoSimpleMsg(DSS, 'Unknown parameter "%s" for Get Command', [ParamName], 133);
                1, 12:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.ActiveCktElement.DSSClassName);
                2, 13:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.ActiveCktElement.Name());
                3:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.DynaVars.intHour);
                4:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.DynaVars.t);
                5:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.Year());
                6:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.Frequency());
                7, 18:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.DynaVars.h);
                ord(Opt.Mode):
                    AppendGlobalResult(DSS, DSS.SolveModeEnum.OrdinalToString(ord(DSS.ActiveCircuit.Solution.Mode())));
                9:
                    AppendGlobalResult(DSS, DSS.RandomModeEnum.OrdinalToString(DSS.ActiveCircuit.Solution.RandomType));
                10:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.NumberOfTimes);
                11:
                    AppendGlobalResult(DSS, Format('[ %d, %-g ] !... %-g (hours)', [DSS.ActiveCircuit.Solution.DynaVars.intHour, DSS.ActiveCircuit.Solution.DynaVars.t, DSS.ActiveCircuit.Solution.DynaVars.dblHour]));
                14:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Name());
                15:
                    AppendGlobalResult(DSS, DefaultEditor);
                16:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.ConvergenceTolerance);
                17:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.MaxIterations);
                19:
                    AppendGlobalResult(DSS, DSS.DefaultLoadModelEnum.OrdinalToString(DSS.ActiveCircuit.Solution.LoadModel));
                20:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.LoadMultiplier());
                21:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.NormalMinVolts);
                22:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.NormalMaxVolts);
                23:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.EmergMinVolts);
                24:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.EmergMaxVolts);
                25:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.DefaultDailyShapeObj.GetMean() * 100.0);
                26:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.DefaultDailyShapeObj.GetStdDev() * 100.0);
                27:
                    AppendGlobalResult(DSS, NameIfNotNil(DSS.ActiveCircuit.LoadDurCurveObj));
                28:
                    AppendGlobalResult(DSS, (DSS.ActiveCircuit.DefaultGrowthRate - 1.0) * 100.0);
                29:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.AutoAddObj.GenkW);
                30:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.AutoAddObj.GenPF);
                31:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.AutoAddObj.CapkVAR);
                32:
                    case DSS.ActiveCircuit.AutoAddObj.Addtype of
                        GENADD:
                            AppendGlobalResult(DSS, 'generator');
                        CAPADD:
                            AppendGlobalResult(DSS, 'capacitor');
                    end;
                33:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.DuplicatesAllowed);
                34:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.ZonesLocked);
                35:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.UEWeight);
                36:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.LossWeight);
                37:
                    AppendGlobalResult(DSS, IntArrayToString(DSS.ActiveCircuit.UERegs));
                38:
                    AppendGlobalResult(DSS, IntArrayToString(DSS.ActiveCircuit.LossRegs));
                39:
                    with DSS.ActiveCircuit do
                    begin
                        i := 1;
                        DSS.GlobalResult := '(';
                        for i := 0 to High(LegalVoltageBases) do
                            DSS.GlobalResult := DSS.GlobalResult + Format('%-g, ', [LegalVoltageBases[i]]);
                        DSS.GlobalResult := DSS.GlobalResult + ')';
                    end;
                40:
                    AppendGlobalResult(DSS, DSS.SolveAlgEnum.OrdinalToString(DSS.ActiveCircuit.Solution.Algorithm));
                41:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.TrapezoidalIntegration);
                42:
                    with DSS.ActiveCircuit.AutoAddBusList do
                        for i := 1 to Count do
                            AppendGlobalResult(DSS, NameOfIndex(i));
                43:
                    AppendGlobalResult(DSS, DSS.ControlModeEnum.OrdinalToString(DSS.ActiveCircuit.Solution.Controlmode));
                44:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.ControlQueue.traceLog);
                45:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.GenMultiplier);
                46:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.DefaultDailyShapeObj.Name());
                47:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.DefaultYearlyShapeObj.Name());
                48:
                    AppendGlobalResult(DSS, 'Get function not applicable.');
                49:
                    AppendGlobalResult(DSS, DSS.CktModelEnum.OrdinalToString(Integer(DSS.ActiveCircuit.positiveSequence)));
                50:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.PriceSignal);
                51:
                    AppendGlobalResult(DSS, NameIfNotNil(DSS.ActiveCircuit.PriceCurveObj));
                52:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.ActiveCktElement.ActiveTerminalIdx());
                53:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Fundamental);
                54:
                    with DSS.ActiveCircuit.Solution do
                        if DoALLHarmonics then
                            AppendGlobalResult(DSS, 'ALL')
                        else
                        begin
                            for i := 0 to High(HarmonicList) do
                                AppendGlobalResult(DSS, HarmonicList[i]);
                        end;
                55:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.MaxControlIterations);
                56:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.BusList.NameOfIndex(DSS.ActiveCircuit.ActiveBusIndex));
                57:
                    AppendGlobalResult(DSS, DSS.DataDirectory); // NOTE - not necessarily output directory
                58:
                    with DSS.ActiveCircuit do
                        for i := 1 to NumBuses do
                            if Buses[i].Keep then
                                AppendGlobalResult(DSS, BusList.NameOfIndex(i));
                59:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.ReductionStrategyString);
                60:
                    AppendGlobalResult(DSS, DSS.EnergyMeterClass.SaveDemandInterval());
                61:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.PctNormalFactor);
                62:
                    AppendGlobalResult(DSS, DSS.EnergyMeterClass.DIVerbose());
                63:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.CaseName());
                64:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.NodeMarkerCode);
                65:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.NodeMarkerWidth);
                66:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.LogEvents);
                67:
                    AppendGlobalResult(DSS, DSS.DSSExecutive.RecorderOn());
                68:
                    AppendGlobalResult(DSS, DSS.EnergyMeterClass.Do_OverloadReport);
                ord(Opt.Voltexceptionreport):
                    AppendGlobalResult(DSS, DSS.EnergyMeterClass.Do_VoltageExceptionReport);
                70:
                    AppendGlobalResult(DSS, 'Get function not applicable.');
                71:
                    AppendGlobalResult(DSS, DSS.AutoShowExport);
                72:
                    AppendGlobalResult(DSS, DSS.MaxAllocationIterations);
                73:
                    AppendGlobalResult(DSS, Round(DSS.DefaultBaseFreq));
                74:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.MarkSwitches);
                75:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.SwitchMarkerCode);
                76:
                    AppendGlobalResult(DSS, Format('%-.6g', [DSS.DaisySize]));
                77:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.MarkTransformers);
                78:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.TransMarkerCode);
                79:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.TransMarkerSize);
                80:
                    AppendGlobalResult(DSS, DSS.LoadShapeClassEnum.OrdinalToString(DSS.ActiveCircuit.ActiveLoadShapeClass));
                81:
                    AppendGlobalResult(DSS, DSS.EarthModelEnum.OrdinalToString(DSS.DefaultEarthModel));
                82:
                    AppendGlobalResult(DSS, DSS.LogQueries);
                83:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.MarkCapacitors);
                84:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.MarkRegulators);
                85:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.MarkPVSystems);
                86:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.MarkStorage);
                87:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.CapMarkerCode);
                88:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.RegMarkerCode);
                89:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.PVMarkerCode);
                90:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.StoreMarkerCode);
                91:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.CapMarkerSize);
                92:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.RegMarkerSize);
                93:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.PVMarkerSize);
                94:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.StoreMarkerSize);
                95:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.NeglectLoadY);
                96:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.MarkFuses);
                97:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.FuseMarkerCode);
                98:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.FuseMarkerSize);
                99:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.MarkReclosers);
                100:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.RecloserMarkerCode);
                101:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.RecloserMarkerSize);
                102:
                    AppendGlobalResult(DSS, 'No');
                103:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.MarkRelays);
                104:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.RelayMarkerCode);
                105:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.RelayMarkerSize);
                106:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.Solve_Time_Elapsed);
                107:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.Total_Time_Elapsed);
                108:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.Step_Time_Elapsed);
                109:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.SampleTheMeters);
                110:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.MinIterations);
                111:
                    AppendGlobalResult(DSS, 'No');
                112:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.ReduceLateralsKeepLoad);
                113:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.ReductionZmag);
                114:
                    AppendGlobalResult(DSS, DSS.SeasonalRating);
                115:
                    AppendGlobalResult(DSS, NameIfNotNil(DSS.SeasonSignalObj));

                ord(Opt.NumCPUs):
                    AppendGlobalResult(DSS, Format('%d', [CPU_Cores]));
                ord(Opt.NumCores):
                    AppendGlobalResult(DSS, Format('%-g', [CPU_Cores / 2])); //TODO: fix -- some people do disable hyperthreading on Intel CPUs nowadays, some CPUs don't have it
                ord(Opt.NumActors):
                    AppendGlobalResult(DSS, Format('%d', [high(PMParent.Children) + 1]));
                ord(Opt.ActiveActor):
                begin
                    if PMParent.AllActors then
                        AppendGlobalResult(DSS, 'All')
                    else
                        AppendGlobalResult(DSS, Format('%d', [PMParent.ActiveChildIndex + 1]));
                end;
                ord(Opt.CPU):
                    AppendGlobalResult(DSS, Format('%d', [PMParent.ActiveChild.CPU]));
                ord(Opt.ActorProgress):
                begin
                    if (@DSS.DSSMessageCallback) <> NIL then
                        DSS.DSSMessageCallback(DSS, NIL, ord(DSSMessageType.ProgressSummary), 0, 0);
                    // ScriptEd.UpdateProgressSummary
                end;
                ord(Opt.Parallel):
                    AppendGlobalResult(DSS, PMParent.parallel_enabled);
                ord(Opt.ConcatenateReports):
                    AppendGlobalResult(DSS, PMParent.ConcatenateReports);
                ord(Opt.NUMANodes):
                    DoSimpleMsg(DSS, _('This is not supported in the AltDSS engine.'), 25040101); //TODO: looks like EPRI's version has this hardcoded
                ord(Opt.LineTypes):
                    DSS.GlobalResult := DSS.LineTypeEnum.Joined();
                ord(Opt.EventLogDefault):
                    AppendGlobalResult(DSS, DSS.EventLogDefault);
                ord(Opt.LongLineCorrection):
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.LongLineCorrection);
                ord(Opt.ShowReports):
                    AppendGlobalResult(DSS, DSS.AutoDisplayShowReport);
                ord(Opt.AllowForms):
                    AppendGlobalResult(DSS, not NoFormsAllowed);
                ord(Opt.AllowProgressBar):
                    AppendGlobalResult(DSS, not NoProgressBarFormAllowed);
{$IFDEF DSS_CAPI_ADIAKOPTICS}
                ord(Opt.Coverage):
                    AppendGlobalResult(DSS, Format('%-g', [DSS.ActiveCircuit.Actual_Coverage]));
                ord(Opt.Num_SubCircuits):
                    AppendGlobalResult(DSS, Format('%d', [DSS.ActiveCircuit.Num_SubCkts]));
                ord(Opt.ADiakoptics):
                    AppendGlobalResult(DSS, PMParent.ADiakoptics);
                ord(Opt.LinkBranches):
                begin
                    if PMParent.ADiakoptics then
                    begin
                        for i := 1 to High(PMParent.ActiveCircuit.Link_Branches) do
                            AppendGlobalResult(DSS, PMParent.ActiveCircuit.Link_Branches[i]);
                    end
                    else
                        AppendGlobalResult(DSS, 'Initialize A-Diakoptics first!');
                end;
{$ENDIF}
                ord(Opt.IgnoreGenQLimits):
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.NCIM_IgnoreQLimit);
                ord(Opt.NCIMQGain):
                    AppendGlobalResult(DSS, Format('%g', [DSS.ActiveCircuit.Solution.NCIM_GenGain]));
                ord(Opt.StateVar):
                begin
                    // ckt := DSS.ActiveCircuit;

                    DSS.Parser.NextParam;
                    TmpStr := DSS.Parser.MakeString();
                    if DSS.ActiveCircuit.SetElementActive(TmpStr) = 0 then
                    begin
                        DoSimpleMsg(DSS, 'Object "%s" not found', [TmpStr], 7100);
                        Exit;
                    end;

                    if not (DSS.ActiveCircuit.ActiveCktElement() is TPCElement) then
                    begin
                        DoSimpleMsg(DSS, 'Object "%s" is not a valid PC element.', [DSS.ActiveCircuit.ActiveCktElement.FullName()], 7103);
                        Exit;
                    end;
                    pce := DSS.ActiveCircuit.ActiveCktElement() as TPCElement;

                    if pce.NumVariables() = 0 then
                    begin
                        DoSimpleMsg(DSS, 'Object "%s" is not a valid element for this command. Only a selection of PC elements have state variables.', [TmpStr], 7101);
                        Exit;
                    end;

                    DSS.Parser.NextParam;
                    TmpStr := LowerCase(DSS.Parser.MakeString());

                    // Search for the variable within the object
                    i := pce.LookupVariable(TmpStr, true);
                    if i < 0 then
                    begin
                        DoSimpleMsg(DSS, 'State variable "%s" not found in "%s".', [TmpStr, pce.FullName()], 7102);
                        Exit;
                    end;
                    AppendGlobalResult(DSS, Format('%g', [pce.GetVariable(i)]));
                end;
                ord(Opt.PyPath):
                begin
                    DoSimpleMsg(DSS, _('This is not supported in the AltDSS engine.'), 25040101);
                    Exit;
                end;
                ord(Opt.IterNumber):
                begin
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.Iteration);
                end;
                ord(Opt.CtrlIterNumber):
                begin
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.ControlIteration);
                end;
                ord(Opt.InjCurrent):
                begin
                    cktElem := DSS.ActiveCircuit.ActiveCktElement;
                    if (cktElem = NIL) or ((cktElem.DSSObjType and BASECLASSMASK) <> PC_ELEMENT) then
                    begin
                        // Use the same message as EPRI's OpenDSS for compatibility
                        AppendGlobalResult(DSS, 'Error, the active element is not PCE');
                        Exit;
                    end;
                    pce := TPCElement(cktElem);
                    if (pce.NodeRef = NIL) or (pce.InjCurrent = NIL) then
                    begin
                        AppendGlobalResult(DSS, 'Error, the active element is not initialized yet');
                        Exit;
                    end;
                    AppendGlobalResult(DSS, ComplexArrayToString(pce.InjCurrent, pce.NConds));
                end;
                ord(Opt.ITerminal):
                begin
                    cktElem := DSS.ActiveCircuit.ActiveCktElement;
                    if (cktElem = NIL) or ((cktElem.DSSObjType and BASECLASSMASK) <> PC_ELEMENT) then
                    begin
                        // Use the same message as EPRI's OpenDSS for compatibility
                        AppendGlobalResult(DSS, 'Error, the active element is not PCE');
                        Exit;
                    end;
                    pce := TPCElement(cktElem);
                    if (pce.NodeRef = NIL) or (pce.ITerminal = NIL) then
                    begin
                        AppendGlobalResult(DSS, 'Error, the active element is not initialized yet');
                        Exit;
                    end;
                    AppendGlobalResult(DSS, ComplexArrayToString(pce.ITerminal, pce.NConds));
                end;
                ord(Opt.YPrim):
                begin
                    cktElem := DSS.ActiveCircuit.ActiveCktElement;
                    if (cktElem = NIL) or ((cktElem.DSSObjType and BASECLASSMASK) <> PC_ELEMENT) then
                    begin
                        // Use the same message as EPRI's OpenDSS for compatibility
                        AppendGlobalResult(DSS, 'Error, the active element is not PCE');
                        Exit;
                    end;
                    pce := TPCElement(cktElem);
                    if (pce.NodeRef = NIL) or (pce.YPrim = NIL) then
                    begin
                        AppendGlobalResult(DSS, 'Error, the active element is not initialized yet');
                        Exit;
                    end;
                    AppendGlobalResult(DSS, pce.YPrim.ToString());
                end;
                ord(Opt.IntegrationFlag):
                begin
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.DynaVars.IterationFlag);
                end;
            else
           // Ignore excess parameters
            end;

            ParamName := DSS.Parser.NextParam;
            Param := DSS.Parser.MakeString();
        end; // WHILE
    except
        AppendGlobalResult(DSS, '***Error***');
    end;
end;

function DoGetCmd_NoCircuit(MainDSS: TDSSContext): Boolean;
// Get DSS Options Reguest and put it in Global Result string
// may be retrieved by Result property of the DSSText interface
var
    // ParamName: String;
    Param: String;
    ParamPointer: Integer;
    PMParent, DSS: TDSSContext;
begin
    PMParent := MainDSS.GetPrime();
    DSS := MainDSS.ActiveChild;

    Result := FALSE;
    try
        DSS.GlobalResult := '';  //initialize for appending

        // Continue parsing command line
        {ParamName :=} DSS.Parser.NextParam;
        Param := DSS.Parser.MakeString();
        // there will be no named paramters in this command and the params
        // themselves will be the parameter name to return
        while Length(Param) > 0 do
        begin
            ParamPointer := DSS.DSSExecutive.OptionList.GetCommand(Param);
            case ParamPointer of
                ord(Opt.NumCPUs):
                    AppendGlobalResult(DSS, Format('%d', [CPU_Cores]));
                ord(Opt.NumCores):
                    AppendGlobalResult(DSS, Format('%-g', [CPU_Cores / 2])); //TODO: fix -- some people do disable hyperthreading on Intel CPUs nowadays
                ord(Opt.NumActors):
                    AppendGlobalResult(DSS, Format('%d', [high(PMParent.Children) + 1]));
                ord(Opt.NUMANodes):
                    ;
                ord(Opt.ActiveActor):
                    if PMParent.AllActors then
                        AppendGlobalResult(DSS, 'All')
                    else
                        AppendGlobalResult(DSS, Format('%d', [DSS.Parent.ActiveChildIndex + 1]));
                ord(Opt.CPU):
                    AppendGlobalResult(DSS, Format('%d', [PMParent.ActiveChild.CPU]));
                ord(Opt.Parallel):
                    AppendGlobalResult(DSS, PMParent.parallel_enabled);
                ord(Opt.ConcatenateReports):
                    AppendGlobalResult(DSS, PMParent.ConcatenateReports);
                ord(Opt.LineTypes):
                    DSS.GlobalResult := DSS.LineTypeEnum.Joined();
                ord(Opt.EventLogDefault):
                    AppendGlobalResult(DSS, DSS.EventLogDefault);
                ord(Opt.PyPath):
                begin
                    DoSimpleMsg(DSS, _('This is not supported in the AltDSS engine.'), 25040101);
                    Exit;
                end;
                else
                begin
                    DoSimpleMsg(DSS, _('You must create a new circuit object first: "new circuit.mycktname" to execute this Set command.'), 301);
                    Exit;
                end;
            end;
            {ParamName :=} DSS.Parser.NextParam;
            Param := DSS.Parser.MakeString();
        end; {WHILE}

    except
        AppendGlobalResult(DSS, _('***Error***'));
    end;
end;

end.
