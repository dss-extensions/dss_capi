unit ExecOptions;

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
        Showexport = 71,
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
        DSSVisualizationTool = 111,
        KeepLoad = 112,
        Zmag = 113,
        SeasonRating = 114,
        SeasonSignal = 115
{$IFDEF DSS_CAPI_PM}
        ,
        NumCPUs = 116,
        NumCores = 117,
        NumActors = 118,
        ActiveActor = 119,
        CPU = 120,
        ActorProgress = 121,
        Parallel = 122,
        ConcatenateReports = 123,
        NUMANodes
{$ENDIF}
{$IFDEF DSS_CAPI_ADIAKOPTICS}
        ,
        Coverage,
        Num_SubCircuits,
        ADiakoptics,
        LinkBranches
{$ENDIF}
    );
{$SCOPEDENUMS OFF}

const
    NumExecOptions = ord(High(TExecOption));

function DoGetCmd({$IFDEF DSS_CAPI_PM}MainDSS{$ELSE}DSS{$ENDIF}: TDSSContext): Integer;
function DoSetCmd({$IFDEF DSS_CAPI_PM}MainDSS{$ELSE}DSS{$ENDIF}: TDSSContext; SolveOption: Integer): Integer;
function DoSetCmd_NoCircuit({$IFDEF DSS_CAPI_PM}MainDSS{$ELSE}DSS{$ENDIF}: TDSSContext): Boolean;  // Set Commands that do not require a circuit
function DoGetCmd_NoCircuit({$IFDEF DSS_CAPI_PM}MainDSS{$ELSE}DSS{$ENDIF}: TDSSContext): Boolean;  // Get Commands that do not require a circuit
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
    Utilities,
    Sysutils,
    Solution,
    Energymeter,
    Dynamics,
    DSSHelper,
    StrUtils,
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

function DoSetCmd_NoCircuit({$IFDEF DSS_CAPI_PM}MainDSS{$ELSE}DSS{$ENDIF}: TDSSContext): Boolean;  // Set Commands that do not require a circuit
// This is for setting global options that do not require an active circuit
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
{$IFDEF DSS_CAPI_PM}
    PMParent, DSS: TDSSContext;
begin
    PMParent := MainDSS.GetPrime();
    DSS := MainDSS.ActiveChild;
{$ELSE}
begin
{$ENDIF}

    Result := TRUE;
     // Continue parsing command line
    ParamPointer := 0;
    ParamName := DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;
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
                DSS.DSSExecutive.RecorderOn := InterpretYesNo(Param);
            73:
                DSS.DefaultBaseFreq := DSS.Parser.DblValue;
            102:
                DoSimpleMsg(DSS, _('This is not supported in DSS Extensions.'), 302);
            111:
                DoSimpleMsg(DSS, _('This is not supported in DSS Extensions.'), 302);
{$IFDEF DSS_CAPI_PM}
            ord(Opt.ActiveActor):
                if DSS.Parser.StrValue = '*' then
                begin
                    PMParent.AllActors := TRUE;
                    PMParent.ActiveChildIndex := 0;
                    PMParent.ActiveChild := PMParent;
                end
                else
                begin
                    if (DSS.Parser.IntValue > 0) and (DSS.Parser.IntValue <= PMParent.NumOfActors) then
                    begin
                        PMParent.ActiveChildIndex := DSS.Parser.IntValue - 1;
                        PMParent.ActiveChild := PMParent.Children[PMParent.ActiveChildIndex];
                        PMParent.AllActors := FALSE;
                    end
                    else
                    begin
                        DoSimpleMsg(DSS, _('The actor does not exist'), 7002);
                    end;
                end;
            ord(Opt.CPU):
                if DSS.Parser.IntValue < CPU_Cores then
                begin
                    DSS.CPU := DSS.Parser.IntValue;
                    if DSS.ActorThread <> NIL then
                        DSS.ActorThread.CPU := DSS.CPU;
                end
                else
                begin
                    DoSimpleMsg(DSS, _('The CPU does not exist'), 7003);
                end;
            ord(Opt.Parallel):
                PMParent.Parallel_enabled := InterpretYesNo(Param);
            ord(Opt.ConcatenateReports):
                PMParent.ConcatenateReports := InterpretYesNo(Param);
{$ENDIF} //DSS_CAPI_PM            
        else
            begin
                DoSimpleMsg(DSS, _('You must create a new circuit object first: "new circuit.mycktname" to execute this Set command.'), 301);
                Result := FALSE;  // Indicate that we could not process all set command
                Exit;
            end;
        end;

        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
    end; {WHILE}

end;

function DoSetCmd({$IFDEF DSS_CAPI_PM}MainDSS{$ELSE}DSS{$ENDIF}: TDSSContext; SolveOption: Integer): Integer;
// Set DSS Options
// Solve Command is re-routed here first to set options beFORe solving
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    TestLoadShapeObj: TLoadShapeObj;
{$IFDEF DSS_CAPI_PM}
    PMParent, DSS: TDSSContext;
begin
    PMParent := MainDSS.GetPrime();
    DSS := MainDSS.ActiveChild;
{$ELSE}
begin
{$ENDIF}
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
    Param := DSS.Parser.StrValue;
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
                DSS.ActiveCircuit.solution.DynaVars.intHour := DSS.Parser.IntValue;
            4:
                DSS.ActiveCircuit.solution.DynaVars.t := DSS.Parser.DblValue;
            5:
                with DSS.ActiveCircuit do
                begin
                    Solution.Year := DSS.Parser.IntValue;
                    DefaultGrowthFactor := IntPower(DefaultGrowthRate, (Solution.Year - 1));
                end;
            6:
                DSS.ActiveCircuit.solution.Frequency := DSS.Parser.DblValue;
            7, 18:
                with DSS.ActiveCircuit do
                begin
                    Solution.DynaVars.h := InterpretTimeStepSize(DSS, Param);
                    Solution.IntervalHrs := Solution.DynaVars.h/3600.0;
                end;
            ord(Opt.Mode):
                DSS.ActiveCircuit.solution.Mode := TSolveMode(DSS.SolveModeEnum.StringToOrdinal(Param));  // see DSSGlobals
            9:
                DSS.ActiveCircuit.solution.RandomType := DSS.RandomModeEnum.StringToOrdinal(Param);
            10:
                DSS.ActiveCircuit.solution.NumberOfTimes := DSS.Parser.IntValue;
            11:
                DSS.DSSExecutive.Set_Time;
            14:
                DSS.DSSExecutive.SetActiveCircuit(Param);
            15:
                DefaultEditor := Param;     // 'Editor='
            16:
                DSS.ActiveCircuit.solution.ConvergenceTolerance := DSS.Parser.DblValue;
            17:
                DSS.ActiveCircuit.solution.MaxIterations := DSS.Parser.IntValue;
            19:
                with DSS.ActiveCircuit.solution do
                begin
                    DefaultLoadModel := DSS.DefaultLoadModelEnum.StringToOrdinal(Param); // for reverting to last on specified
                    LoadModel := DefaultLoadModel;
                end;
            20:
                DSS.ActiveCircuit.LoadMultiplier := DSS.Parser.DblValue;  // Set using LoadMultiplier property
            21:
                DSS.ActiveCircuit.NormalMinVolts := DSS.Parser.DblValue;
            22:
                DSS.ActiveCircuit.NormalMaxVolts := DSS.Parser.DblValue;
            23:
                DSS.ActiveCircuit.EmergMinVolts := DSS.Parser.DblValue;
            24:
                DSS.ActiveCircuit.EmergMaxVolts := DSS.Parser.DblValue;
            25:
                DSS.ActiveCircuit.DefaultDailyShapeObj.Mean := DSS.Parser.DblValue / 100.0;
            26:
                DSS.ActiveCircuit.DefaultDailyShapeObj.StdDev := DSS.Parser.DblValue / 100.0;
            27:
                with DSS.ActiveCircuit do
                begin
                    LoadDurCurve := Param;
                    LoadDurCurveObj := DSS.LoadShapeClass.Find(Param);
                    if LoadDurCurveObj = NIL then
                        DoSimpleMsg(DSS, _('Load-Duration Curve not found.'), 131);
                end;
            28:
                with DSS.ActiveCircuit do
                begin
                    DefaultGrowthRate := 1.0 + DSS.Parser.DblValue / 100.0;
                    DefaultGrowthFactor := IntPower(DefaultGrowthRate, (Solution.Year - 1));
                end;
            29:
                DSS.ActiveCircuit.AutoAddObj.GenkW := DSS.Parser.DblValue;
            30:
                DSS.ActiveCircuit.AutoAddObj.GenPF := DSS.Parser.DblValue;
            31:
                DSS.ActiveCircuit.AutoAddObj.CapkVAR := DSS.Parser.DblValue;
            32:
                DSS.ActiveCircuit.AutoAddObj.AddType := DSS.AddTypeEnum.StringToOrdinal(Param);
            33:
                DSS.ActiveCircuit.DuplicatesAllowed := InterpretYesNo(Param);
            34:
                DSS.ActiveCircuit.ZonesLocked := InterpretYesNo(Param);
            35:
                DSS.ActiveCircuit.UEWeight := DSS.Parser.DblValue;
            36:
                DSS.ActiveCircuit.LossWeight := DSS.Parser.DblValue;
            37:
                ParseIntArray(DSS, DSS.ActiveCircuit.UERegs, DSS.ActiveCircuit.NumUEregs, Param);
            38:
                ParseIntArray(DSS, DSS.ActiveCircuit.LossRegs, DSS.ActiveCircuit.NumLossregs, Param);
            39:
                DSS.DSSExecutive.DoLegalVoltageBases;
            40:
                DSS.ActiveCircuit.Solution.Algorithm := DSS.SolveAlgEnum.StringToOrdinal(Param);
            41:
                DSS.ActiveCircuit.TrapezoidalIntegration := InterpretYesNo(Param);
            42:
                DSS.DSSExecutive.DoAutoAddBusList(Param);
            43:
                with DSS.ActiveCircuit.Solution do
                begin
                    ControlMode := DSS.ControlModeEnum.StringToOrdinal(Param);
                    DefaultControlMode := ControlMode;  // always revert to last one specified in a script
                end;
            44:
                DSS.ActiveCircuit.ControlQueue.TraceLog := InterpretYesNo(Param);
            45:
                DSS.ActiveCircuit.GenMultiplier := DSS.Parser.DblValue;
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
                DSS.DSSExecutive.DoSetAllocationFactors(DSS.Parser.DblValue);
            49:
                DSS.ActiveCircuit.PositiveSequence := Boolean(DSS.CktModelEnum.StringToOrdinal(Param));
            50:
                DSS.ActiveCircuit.PriceSignal := DSS.Parser.DblValue;
            51:
                with DSS.ActiveCircuit do
                begin
                    PriceCurve := Param;
                    PriceCurveObj := DSS.PriceShapeClass.Find(Param);
                    if PriceCurveObj = NIL then
                        DoSimpleMsg(DSS, 'Priceshape.%s not found.', [param], 132);
                end;
            52:
                with DSS.ActiveCircuit do
                    if ActiveCktElement <> NIL then
                        with ActiveCktElement do
                        begin
                            ActiveTerminalIdx := DSS.Parser.IntValue;
                            SetActiveBus(DSS, StripExtension(Getbus(ActiveTerminalIdx)));   // bus connected to terminal
                        end;
            53:
            begin
                DSS.ActiveCircuit.Fundamental := DSS.Parser.DblValue;     // Set Base Frequency for system (used henceforth)
                DSS.ActiveCircuit.Solution.Frequency := DSS.Parser.DblValue;
            end;
            54:
                DSS.DSSExecutive.DoHarmonicsList(Param);
            55:
                DSS.ActiveCircuit.Solution.MaxControlIterations := DSS.Parser.IntValue;
            56:
                Result := SetActiveBus(DSS, Param);   // See DSSGlobals
            57:
                SetDataPath(DSS, Param);  // Set a legal data path
            58:
                DSS.DSSExecutive.DoKeeperBusList(Param);
            59:
                DSS.DSSExecutive.DoSetReduceStrategy(param);
            60:
                DSS.EnergyMeterClass.SaveDemandInterval := InterpretYesNo(Param);
            61:
            begin
                DSS.ActiveCircuit.PctNormalFactor := DSS.Parser.DblValue;
                DSS.DSSExecutive.DoSetNormal(DSS.ActiveCircuit.PctNormalFactor);
            end;
            62:
                DSS.EnergyMeterClass.DI_Verbose := InterpretYesNo(Param);
            63:
                DSS.ActiveCircuit.CaseName := DSS.Parser.StrValue;
            64:
                DSS.ActiveCircuit.NodeMarkerCode := DSS.Parser.IntValue;
            65:
                DSS.ActiveCircuit.NodeMarkerWidth := DSS.Parser.IntValue;
            66:
                DSS.ActiveCircuit.LogEvents := InterpretYesNo(Param);
            67:
                DSS.DSSExecutive.RecorderOn := InterpretYesNo(Param);
            68:
                DSS.EnergyMeterClass.Do_OverloadReport := InterpretYesNo(Param);
            69:
                DSS.EnergyMeterClass.Do_VoltageExceptionReport := InterpretYesNo(Param);
            70:
                DSS.DSSExecutive.DoSetCFactors(DSS.Parser.DblValue);
            71:
                DSS.AutoShowExport := InterpretYesNo(Param);
            72:
                DSS.MaxAllocationIterations := DSS.Parser.IntValue;
            73:
            begin
                DSS.DefaultBaseFreq := DSS.Parser.DblValue;
                DSS.ActiveCircuit.Fundamental := DSS.Parser.DblValue;     // Set Base Frequency for system (used henceforth)
                DSS.ActiveCircuit.Solution.Frequency := DSS.Parser.DblValue;
            end;
            74:
                DSS.ActiveCircuit.MarkSwitches := InterpretYesNo(Param);
            75:
                DSS.ActiveCircuit.SwitchMarkerCode := DSS.Parser.IntValue;
            76:
                DSS.DaisySize := DSS.Parser.DblValue;
            77:
                DSS.ActiveCircuit.MarkTransformers := InterpretYesNo(Param);
            78:
                DSS.ActiveCircuit.TransMarkerCode := DSS.Parser.IntValue;
            79:
                DSS.ActiveCircuit.TransMarkerSize := DSS.Parser.IntValue;
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
                DSS.ActiveCircuit.CapMarkerCode := DSS.Parser.IntValue;
            88:
                DSS.ActiveCircuit.RegMarkerCode := DSS.Parser.IntValue;
            89:
                DSS.ActiveCircuit.PVMarkerCode := DSS.Parser.IntValue;
            90:
                DSS.ActiveCircuit.StoreMarkerCode := DSS.Parser.IntValue;
            91:
                DSS.ActiveCircuit.CapMarkerSize := DSS.Parser.IntValue;
            92:
                DSS.ActiveCircuit.RegMarkerSize := DSS.Parser.IntValue;
            93:
                DSS.ActiveCircuit.PVMarkerSize := DSS.Parser.IntValue;
            94:
                DSS.ActiveCircuit.StoreMarkerSize := DSS.Parser.IntValue;
            95:
                DSS.ActiveCircuit.NeglectLoadY := InterpretYesNo(Param);
            96:
                DSS.ActiveCircuit.MarkFuses := InterpretYesNo(Param);
            97:
                DSS.ActiveCircuit.FuseMarkerCode := DSS.Parser.IntValue;
            98:
                DSS.ActiveCircuit.FuseMarkerSize := DSS.Parser.IntValue;
            99:
                DSS.ActiveCircuit.MarkReclosers := InterpretYesNo(Param);
            100:
                DSS.ActiveCircuit.RecloserMarkerCode := DSS.Parser.IntValue;
            101:
                DSS.ActiveCircuit.RecloserMarkerSize := DSS.Parser.IntValue;
            102:
                DoSimpleMsg(DSS, _('This is not supported in DSS Extensions.'), 309);
            103:
                DSS.ActiveCircuit.MarkRelays := InterpretYesNo(Param);
            104:
                DSS.ActiveCircuit.RelayMarkerCode := DSS.Parser.IntValue;
            105:
                DSS.ActiveCircuit.RelayMarkerSize := DSS.Parser.IntValue;
            107:
                DSS.ActiveCircuit.Solution.Total_Time := DSS.Parser.DblValue;
            109:
                DSS.ActiveCircuit.Solution.SampleTheMeters := InterpretYesNo(Param);
            110:
                DSS.ActiveCircuit.solution.MinIterations := DSS.Parser.IntValue;
            111:
                DoSimpleMsg(DSS, _('This is not supported in DSS Extensions.'), 303);
            112:
                DSS.ActiveCircuit.ReduceLateralsKeepLoad := InterpretYesNo(Param);
            113:
                DSS.ActiveCircuit.ReductionZmag := DSS.Parser.DblValue;
            114:
                DSS.SeasonalRating := InterpretYesNo(Param);
            115:
                DSS.SeasonSignal := Param;
{$IFDEF DSS_CAPI_PM}                
            ord(Opt.ActiveActor):
                if DSS.Parser.StrValue = '*' then
                begin
                    PMParent.AllActors := TRUE;
                    PMParent.ActiveChildIndex := 0;
                    PMParent.ActiveChild := PMParent;
                end
                else
                begin
                    if (DSS.Parser.IntValue > 0) and (DSS.Parser.IntValue <= PMParent.NumOfActors) then
                    begin
                        PMParent.ActiveChildIndex := DSS.Parser.IntValue - 1;
                        PMParent.ActiveChild := PMParent.Children[PMParent.ActiveChildIndex];
                        PMParent.AllActors := FALSE;
                    end
                    else
                    begin
                        DoSimpleMsg(DSS, _('The actor does not exist'), 7002);
                    end;
                end;
            ord(Opt.CPU):
                if DSS.Parser.IntValue < CPU_Cores then
                begin
                    DSS.CPU := DSS.Parser.IntValue;
                    if DSS.ActorThread <> NIL then
                        DSS.ActorThread.CPU := DSS.CPU;
                end
                else
                begin
                    DoSimpleMsg(DSS, _('The CPU does not exist'), 7003);
                end;
            ord(Opt.Parallel):
                PMParent.Parallel_enabled := InterpretYesNo(Param);
            ord(Opt.ConcatenateReports):
                PMParent.ConcatenateReports := InterpretYesNo(Param);
{$ENDIF}
{$IFDEF DSS_CAPI_ADIAKOPTICS}
            ord(Opt.Coverage):
                DSS.ActiveCircuit.Coverage := DSS.Parser.DblValue;
            ord(Opt.Num_SubCircuits):
                DSS.ActiveCircuit.Num_SubCkts := DSS.Parser.IntValue;
            ord(Opt.ADiakoptics):
            begin
                if InterpretYesNo(Param) then
                    ADiakopticsInit(DSS)  // Initalizes the parallel environment if enabled
                else
                    DSS.ActiveCircuit.Solution.ADiakoptics := FALSE;
            end;
{$ENDIF}
        else
           // Ignore excess parameters
           //TODO: warn about excess parameters
        end;

        case ParamPointer of
            3, 4:
                DSS.ActiveCircuit.Solution.Update_dblHour;
        end;

        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
    end; {WHILE}

    if SolveOption = 1 then
        DSS.DSSExecutive.DoSolveCmd;
end;

function DoGetCmd({$IFDEF DSS_CAPI_PM}MainDSS{$ELSE}DSS{$ENDIF}: TDSSContext): Integer;
// Get DSS Options Reguest and put it in Global Result string
// may be retrieved by Result property of the DSSText interface
var
    ParamPointer, i: Integer;
    ParamName: String;
    Param: String;
{$IFDEF DSS_CAPI_PM}
    PMParent, DSS: TDSSContext;
begin
    PMParent := MainDSS.GetPrime();
    DSS := MainDSS.ActiveChild;
{$ELSE}
begin
{$ENDIF}

    Result := 0;
    try
        DSS.GlobalResult := '';  //initialize for appending

        // Continue parsing command line
        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
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
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.ActiveCktElement.Name);
                3:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.solution.DynaVars.intHour);
                4:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.solution.DynaVars.t);
                5:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.solution.Year);
                6:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.solution.Frequency);
                7, 18:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.solution.DynaVars.h);
                ord(Opt.Mode):
                    AppendGlobalResult(DSS, DSS.SolveModeEnum.OrdinalToString(ord(DSS.ActiveCircuit.Solution.mode)));
                9:
                    AppendGlobalResult(DSS, DSS.RandomModeEnum.OrdinalToString(DSS.ActiveCircuit.Solution.RandomType));
                10:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.solution.NumberOfTimes);
                11:
                    AppendGlobalResult(DSS, Format('[ %d, %-g ] !... %-g (hours)', [DSS.ActiveCircuit.solution.DynaVars.intHour, DSS.ActiveCircuit.solution.DynaVars.t, DSS.ActiveCircuit.solution.DynaVars.dblHour]));
                14:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.name);
                15:
                    AppendGlobalResult(DSS, DefaultEditor);
                16:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.solution.ConvergenceTolerance);
                17:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.solution.MaxIterations);
                19:
                    AppendGlobalResult(DSS, DSS.DefaultLoadModelEnum.OrdinalToString(DSS.ActiveCircuit.Solution.LoadModel));
                20:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.LoadMultiplier);
                21:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.NormalMinVolts);
                22:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.NormalMaxVolts);
                23:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.EmergMinVolts);
                24:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.EmergMaxVolts);
                25:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.DefaultDailyShapeObj.Mean * 100.0);
                26:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.DefaultDailyShapeObj.StdDev * 100.0);
                27:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.LoadDurCurve);
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
                    AppendGlobalResult(DSS, IntArrayToString(DSS.ActiveCircuit.UERegs, DSS.ActiveCircuit.NumUEregs));
                38:
                    AppendGlobalResult(DSS, IntArrayToString(DSS.ActiveCircuit.LossRegs, DSS.ActiveCircuit.NumLossRegs));
                39:
                    with DSS.ActiveCircuit do
                    begin
                        i := 1;
                        DSS.GlobalResult := '(';
                        while LegalVoltageBases^[i] > 0.0 do
                        begin
                            DSS.GlobalResult := DSS.GlobalResult + Format('%-g, ', [LegalVoltageBases^[i]]);
                            inc(i);
                        end;
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
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.DefaultDailyShapeObj.Name);
                47:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.DefaultYearlyShapeObj.Name);
                48:
                    AppendGlobalResult(DSS, 'Get function not applicable.');
                49:
                    AppendGlobalResult(DSS, DSS.CktModelEnum.OrdinalToString(Integer(DSS.ActiveCircuit.positiveSequence)));
                50:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.PriceSignal);
                51:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.PriceCurve);
                52:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.ActiveCktElement.ActiveTerminalIdx);
                53:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Fundamental);
                54:
                    with DSS.ActiveCircuit.Solution do
                        if DoALLHarmonics then
                            AppendGlobalResult(DSS, 'ALL')
                        else
                        begin
                            for i := 1 to HarmonicListSize do
                                AppendGlobalResult(DSS, HarmonicList^[i]);
                        end;
                55:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.solution.MaxControlIterations);
                56:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.BusList.NameOfIndex(DSS.ActiveCircuit.ActiveBusIndex));
                57:
                    AppendGlobalResult(DSS, DSS.DataDirectory); // NOTE - not necessarily output directory
                58:
                    with DSS.ActiveCircuit do
                        for i := 1 to NumBuses do
                            if Buses^[i].Keep then
                                AppendGlobalResult(DSS, BusList.NameOfIndex(i));
                59:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.ReductionStrategyString);
                60:
                    AppendGlobalResult(DSS, DSS.EnergyMeterClass.SaveDemandInterval);
                61:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.PctNormalFactor);
                62:
                    AppendGlobalResult(DSS, DSS.EnergyMeterClass.DI_Verbose);
                63:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.CaseName);
                64:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.NodeMarkerCode);
                65:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.NodeMarkerWidth);
                66:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.LogEvents);
                67:
                    AppendGlobalResult(DSS, DSS.DSSExecutive.RecorderON);
                68:
                    AppendGlobalResult(DSS, DSS.EnergyMeterClass.Do_OverloadReport);
                69:
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
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.Time_Solve);
                107:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.Total_Time);
                108:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.Time_Step);
                109:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.Solution.SampleTheMeters);
                110:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.solution.MinIterations);
                111:
                    AppendGlobalResult(DSS, 'No');
                112:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.ReduceLateralsKeepLoad);
                113:
                    AppendGlobalResult(DSS, DSS.ActiveCircuit.ReductionZmag);
                114:
                    AppendGlobalResult(DSS, DSS.SeasonalRating);
                115:
                    AppendGlobalResult(DSS, DSS.SeasonSignal);

{$IFDEF DSS_CAPI_PM}
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
{$IFNDEF FPC}
                    ScriptEd.UpdateProgressSummary
{$ENDIF}
                    ;
                ord(Opt.Parallel):
                    AppendGlobalResult(DSS, PMParent.parallel_enabled);
                ord(Opt.ConcatenateReports):
                    AppendGlobalResult(DSS, PMParent.ConcatenateReports);
                ord(Opt.NUMANodes):
                    DoSimpleMsg(DSS, _('This is not supported in DSS Extensions.'), 303); //TODO: looks like the official version has this hardcoded
{$ENDIF} //DSS_CAPI_PM
{$IFDEF DSS_CAPI_ADIAKOPTICS}
                ord(Opt.Coverage):
                    AppendGlobalResult(DSS, Format('%-g', [DSS.ActiveCircuit.Actual_Coverage]));
                ord(Opt.Num_SubCircuits):
                    AppendGlobalResult(DSS, Format('%d', [DSS.ActiveCircuit.Num_SubCkts]));
                ord(Opt.ADiakoptics):
                    AppendGlobalResult(DSS, PMParent.ActiveCircuit.Solution.ADiakoptics);
                ord(Opt.LinkBranches):
                begin
                    if PMParent.ActiveCircuit.Solution.ADiakoptics then
                    begin
                        for i := 1 to High(PMParent.ActiveCircuit.Link_Branches) do
                            AppendGlobalResult(DSS, PMParent.ActiveCircuit.Link_Branches[i]);
                    end
                    else
                        AppendGlobalResult(DSS, 'Initialize A-Diakoptics first!');
                end;
{$ENDIF}
            else
           // Ignore excess parameters
            end;

            ParamName := DSS.Parser.NextParam;
            Param := DSS.Parser.StrValue;
        end; // WHILE
    except
        AppendGlobalResult(DSS, '***Error***');
    end;
end;

function DoGetCmd_NoCircuit({$IFDEF DSS_CAPI_PM}MainDSS{$ELSE}DSS{$ENDIF}: TDSSContext): Boolean;
// Get DSS Options Reguest and put it in Global Result string
// may be retrieved by Result property of the DSSText interface
var
    // ParamName: String;
    Param: String;
{$IFDEF DSS_CAPI_PM}
    ParamPointer: Integer;
    PMParent, DSS: TDSSContext;
begin
    PMParent := MainDSS.GetPrime();
    DSS := MainDSS.ActiveChild;
{$ELSE}
begin
{$ENDIF}

    Result := FALSE;
    try
        DSS.GlobalResult := '';  //initialize for appending

        // Continue parsing command line
        {ParamName :=} DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
        // there will be no named paramters in this command and the params
        // themselves will be the parameter name to return
        while Length(Param) > 0 do
        begin
{$IFDEF DSS_CAPI_PM}
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
                    AppendGlobalResult(DSS, PMParent.ConcatenateReports)
                else
                begin
                    DoSimpleMsg(DSS, _('You must create a new circuit object first: "new circuit.mycktname" to execute this Set command.'), 301);
                    Result := FALSE;  // Indicate that we could not process all set command
                    Exit;
                end;
            end;
            {ParamName :=} DSS.Parser.NextParam;
            Param := DSS.Parser.StrValue;
{$ELSE} 
            DoSimpleMsg(DSS, _('You must create a new circuit object first: "new circuit.mycktname" to execute this Set command.'), 301);
            Result := FALSE;  // Indicate that we could not process all set command
            Exit;
{$ENDIF} // DSS_CAPI_PM
        end; {WHILE}

    except
        AppendGlobalResult(DSS, _('***Error***'));
    end;
end;

end.
