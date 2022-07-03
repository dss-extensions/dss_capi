unit ExportOptions;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2019, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Command,
    DSSClass;

type
{$SCOPEDENUMS ON}
    TExportOption = (
        INVALID = 0,
        Voltages = 1,
        SeqVoltages = 2,
        Currents = 3,
        SeqCurrents = 4,
        Estimation = 5,
        Capacity = 6,
        Overloads = 7,
        Unserved = 8,
        Powers = 9,
        SeqPowers = 10,
        Faultstudy = 11,
        Generators = 12,
        Loads = 13,
        Meters = 14,
        Monitors = 15,
        Yprims = 16,
        Y = 17,
        seqz = 18,
        P_byphase = 19,
        CIM100Fragments = 20,
        CIM100 = 21,
        CDPSMAsset = 22,
        Buscoords = 23,
        Losses = 24,
        Uuids = 25,
        Counts = 26,
        Summary = 27,
        CDPSMElec = 28,
        CDPSMGeo = 29,
        CDPSMTopo = 30,
        CDPSMStateVar = 31,
        Profile = 32,
        EventLog = 33,
        AllocationFactors = 34,
        VoltagesElements = 35,
        GICMvars = 36,
        BusReliability = 37,
        BranchReliability = 38,
        NodeNames = 39,
        Taps = 40,
        NodeOrder = 41,
        ElemCurrents = 42,
        ElemVoltages = 43,
        ElemPowers = 44,
        Result = 45,
        YNodeList = 46,
        YVoltages = 47,
        YCurrents = 48,
        PVSystem_Meters = 49,
        Storage_Meters = 50,
        Sections = 51,
        ErrorLog = 52,
        IncMatrix = 53,
        IncMatrixRows = 54,
        IncMatrixCols = 55,
        BusLevels = 56,
        Laplacian = 57
{$IFDEF DSS_CAPI_ADIAKOPTICS}
        ,
        ZLL = 58,
        ZCC = 59,
        Contours = 60,
        Y4 = 61
{$ENDIF}        
    );
{$SCOPEDENUMS OFF}

const
    NumExportOptions = ord(High(TExportOption));

function DoExportCmd({$IFDEF DSS_CAPI_PM}MainDSS{$ELSE}DSS{$ENDIF}: TDSSContext): Integer;
procedure DefineOptions(var ExportOption: ArrayOfString);

implementation

uses
    ExportResults,
    Monitor,
    EnergyMeter,
    ParserDel,
    sysutils,
    DSSGlobals,
    ExportCIMXML,
    Utilities,
    NamedObject,
    DSSHelper,
    TypInfo;

function AssignNewUUID(val: String): TUuid;
begin
    if Pos('{', val) < 1 then
        val := '{' + val + '}';
    result := StringToUuid(val);
end;

procedure DefineOptions(var ExportOption: ArrayOfString);
var
    info: Pointer;
    i: Integer;
begin
    info := TypeInfo(TExportOption);
    SetLength(ExportOption, NumExportOptions);
    for i := 1 to NumExportOptions do
        ExportOption[i - 1] := GetEnumName(info, i);
end;

function DoExportCmd({$IFDEF DSS_CAPI_PM}MainDSS{$ELSE}DSS{$ENDIF}: TDSSContext): Integer;
var
    ParamName,
    Parm1,
    Parm2,
    FileName: String;
    MVAopt: Integer;
    UEonlyOpt: Boolean;
    TripletOpt: Boolean;
    pMon: TMonitorObj;
    pMeter: TEnergyMeterObj;
    ParamPointer: Integer;
    PhasesToPlot: Integer;
    AbortExport: Boolean;
    Substation, GeographicRegion, SubGeographicRegion: String; // for CIM export
    FdrUuid, SubUuid, SubGeoUuid, RgnUuid: TUuid;              // for CIM export
{$IFDEF DSS_CAPI_PM}
    InitP, FinalP, idxP: Integer;
    PMParent, DSS: TDSSContext;
begin
    PMParent := MainDSS.GetPrime();
    DSS := MainDSS.ActiveChild;
{$ELSE}
begin
{$ENDIF}

    Result := 0;
    AbortExport := FALSE;
    FileName := '';

    Parm2 := '';
    ParamName := DSS.Parser.NextParam;
    Parm1 := AnsiLowerCase(DSS.Parser.StrValue);
    ParamPointer := DSS.DSSExecutive.ExportCommands.Getcommand(Parm1);

   {Check commands requiring a solution and abort if no solution or circuit}
    case ParamPointer of
        1..24, 28..32, 35, 46..51:
        begin
            if not assigned(DSS.ActiveCircuit) then
            begin
                DoSimpleMsg(DSS, 'No circuit created.', 24711);
                Exit;
            end;
            if not assigned(DSS.ActiveCircuit.Solution) or not assigned(DSS.ActiveCircuit.Solution.NodeV) then
            begin
                DoSimpleMsg(DSS, 'The circuit must be solved before you can do this.', 24712);
                Exit;
            end;
        end;
    end;


    MVAOpt := 0;
    UEonlyOpt := FALSE;
    TripletOpt := FALSE;
    PhasesToPlot := PROFILE3PH;  // init this to get rid of compiler warning
    pMeter := NIL;
    Substation := DSS.ActiveCircuit.Name + '_Substation';
    SubGeographicRegion := DSS.ActiveCircuit.Name + '_SubRegion';
    GeographicRegion := DSS.ActiveCircuit.Name + '_Region';
    DSS.CIMExporter.DefaultCircuitUUIDs(FdrUuid, SubUuid, RgnUuid, SubGeoUuid);

    case ParamPointer of
        9, 19:
        begin { Trap export powers command and look for MVA/kVA option }
            ParamName := DSS.Parser.nextParam;
            Parm2 := AnsiLowerCase(DSS.Parser.strvalue);
            MVAOpt := 0;
            if Length(Parm2) > 0 then
                if Parm2[1] = 'm' then
                    MVAOpt := 1;
        end;

        8:
        begin { Trap UE only flag  }
            ParamName := DSS.Parser.nextParam;
            Parm2 := AnsiLowerCase(DSS.Parser.strvalue);
            UEonlyOpt := FALSE;
            if Length(Parm2) > 0 then
                if Parm2[1] = 'u' then
                    UEonlyOpt := TRUE;
        end;

        15:
        begin {Get monitor name for export monitors command}
            ParamName := DSS.Parser.NextParam;
            Parm2 := DSS.Parser.StrValue;
        end;

        17:
        begin { Trap Sparse Triplet flag  }
            ParamName := DSS.Parser.nextParam;
            Parm2 := AnsiLowerCase(DSS.Parser.strvalue);
            TripletOpt := FALSE;
            if Length(Parm2) > 0 then
                if Parm2[1] = 't' then
                    TripletOpt := TRUE;
        end;

        20, 21:
        begin {user-supplied substation and regions}
            ParamName := AnsiLowerCase(DSS.Parser.nextParam);
            Parm2 := DSS.Parser.strValue;
            while Length(ParamName) > 0 do
            begin
                if CompareTextShortest(ParamName, 'subs') = 0 then
                    Substation := Parm2
                else
                if CompareTextShortest(ParamName, 'subg') = 0 then
                    SubGeographicRegion := Parm2
                else
                if CompareTextShortest(ParamName, 'g') = 0 then
                    GeographicRegion := Parm2
                else
                if CompareTextShortest(ParamName, 'fil') = 0 then
                    FileName := Parm2
                else
                if CompareTextShortest(ParamName, 'fid') = 0 then
                    FdrUuid := AssignNewUUID(Parm2)
                else
                if CompareTextShortest(ParamName, 'sid') = 0 then
                    SubUuid := AssignNewUUID(Parm2)
                else
                if CompareTextShortest(ParamName, 'sg') = 0 then
                    SubGeoUuid := AssignNewUUID(Parm2)
                else
                if CompareTextShortest(ParamName, 'rg') = 0 then
                    RgnUuid := AssignNewUUID(Parm2);
                ParamName := AnsiLowerCase(DSS.Parser.nextParam);
                Parm2 := DSS.Parser.strValue;
            end;
        end;

        32:
        begin {Get phases to plot}
            ParamName := DSS.Parser.NextParam;
            Parm2 := DSS.Parser.StrValue;
            PhasesToPlot := PROFILE3PH; // the default
            if CompareTextShortest(Parm2, 'default') = 0 then
                PhasesToPlot := PROFILE3PH
            else
            if CompareTextShortest(Parm2, 'all') = 0 then
                PhasesToPlot := PROFILEALL
            else
            if CompareTextShortest(Parm2, 'primary') = 0 then
                PhasesToPlot := PROFILEALLPRI
            else
            if CompareTextShortest(Parm2, 'll3ph') = 0 then
                PhasesToPlot := PROFILELL
            else
            if CompareTextShortest(Parm2, 'llall') = 0 then
                PhasesToPlot := PROFILELLALL
            else
            if CompareTextShortest(Parm2, 'llprimary') = 0 then
                PhasesToPlot := PROFILELLPRI
            else
            if Length(Parm2) = 1 then
                PhasesToPlot := DSS.Parser.IntValue;

        end;

        51:
        begin {Sections}
            ParamName := DSS.Parser.NextParam;
            Parm2 := DSS.Parser.StrValue;

            if CompareTextShortest(ParamName, 'meter') = 0 then
                pMeter := DSS.EnergyMeterClass.Find(Parm2);
        end;

    end;

   {Pick up next parameter on line, alternate file name, if any}
    if Length(FileName) = 0 then
    begin
        ParamName := DSS.Parser.NextParam;
        FileName := AnsiLowerCase(DSS.Parser.StrValue);    // should be full path name to work universally
    end;

    DSS.InShowResults := TRUE;

   {Assign default file name if alternate not specified}
    if Length(FileName) = 0 then
    begin
        case ParamPointer of
            1:
                FileName := 'EXP_VOLTAGES.csv';
            2:
                FileName := 'EXP_SEQVOLTAGES.csv';
            3:
                FileName := 'EXP_CURRENTS.csv';
            4:
                FileName := 'EXP_SEQCURRENTS.csv';
            5:
                FileName := 'EXP_ESTIMATION.csv';   // Estimation error
            6:
                FileName := 'EXP_CAPACITY.csv';
            7:
                FileName := 'EXP_OVERLOADS.csv';
            8:
                FileName := 'EXP_UNSERVED.csv';
            9:
                FileName := 'EXP_POWERS.csv';
            10:
                FileName := 'EXP_SEQPOWERS.csv';
            11:
                FileName := 'EXP_FAULTS.csv';
            12:
                FileName := 'EXP_GENMETERS.csv';
            13:
                FileName := 'EXP_LOADS.csv';
            14:
                FileName := 'EXP_METERS.csv';
         {15: Filename is assigned}
            16:
                Filename := 'EXP_YPRIM.csv';
            17:
                Filename := 'EXP_Y.csv';
            18:
                Filename := 'EXP_SEQZ.csv';
            19:
                Filename := 'EXP_P_BYPHASE.csv';
            20:
                FileName := 'CIM100.XML';
            21:
                FileName := 'CIM100x.XML';
            22:
                FileName := '';
            23:
                FileName := 'EXP_BUSCOORDS.csv';
            24:
                FileName := 'EXP_LOSSES.csv';
            25:
                FileName := 'EXP_UUIDS.csv';
            26:
                FileName := 'EXP_Counts.csv';
            27:
                FileName := 'EXP_Summary.csv';
            28:
                FileName := '';
            29:
                FileName := '';
            30:
                FileName := '';
            31:
                FileName := '';
            32:
                FileName := 'EXP_Profile.csv';
            33:
                FileName := 'EXP_EventLog.csv';
            34:
                FileName := 'AllocationFactors.txt';
            35:
                FileName := 'EXP_VOLTAGES_ELEM.csv';
            36:
                FileName := 'EXP_GIC_Mvar.csv';
            37:
                FileName := 'EXP_BusReliability.csv';
            38:
                FileName := 'EXP_BranchReliability.csv';
            39:
                FileName := 'EXP_NodeNames.csv';
            40:
                FileName := 'EXP_Taps.csv';
            41:
                FileName := 'EXP_NodeOrder.csv';
            42:
                FileName := 'EXP_ElemCurrents.csv';
            43:
                FileName := 'EXP_ElemVoltages.csv';
            44:
                FileName := 'EXP_ElemPowers.csv';
            45:
                FileName := 'EXP_Result.csv';
            46:
                FileName := 'EXP_YNodeList.csv';
            47:
                FileName := 'EXP_YVoltages.csv';
            48:
                FileName := 'EXP_YCurrents.csv';
            49:
                FileName := 'EXP_PVMeters.csv';
            50:
                FileName := 'EXP_STORAGEMeters.csv';
            51:
                FileName := 'EXP_SECTIONS.csv';
            52:
                FileName := 'EXP_ErrorLog.txt';
            53:
                FileName := 'Inc_Matrix.csv';
            54:
                FileName := 'Inc_Matrix_Rows.csv';
            55:
                FileName := 'Inc_Matrix_Cols.csv';
            56:
                FileName := 'Bus_Levels.csv';
            57:
                FileName := 'Laplacian.csv';
{$IFDEF DSS_CAPI_PM}                
            58:
                FileName := 'ZLL.csv';
            59:
                FileName := 'ZCC.csv';
            60:
                FileName := 'C.csv';
            61:
                FileName := 'Y4.csv';
{$ENDIF}
        else
            FileName := 'EXP_VOLTAGES.csv';    // default
        end;
        FileName := DSS.OutputDirectory + DSS.CircuitName_ + FileName;  // Explicitly define directory
    end;

    case ParamPointer of
        1:
            ExportVoltages(DSS, Filename);
        2:
            ExportSeqVoltages(DSS, Filename);
        3:
            ExportCurrents(DSS, Filename);
        4:
            ExportSeqCurrents(DSS, Filename);
        5:
            ExportEstimation(DSS, Filename);   // Estimation error
        6:
            ExportCapacity(DSS, Filename);
        7:
            ExportOverLoads(DSS, Filename);
        8:
            ExportUnserved(DSS, FileName, UEOnlyOpt);
        9:
            ExportPowers(DSS, FileName, MVAOpt);
        10:
            ExportSeqPowers(DSS, FileName, MVAopt);
        11:
            ExportFaultStudy(DSS, Filename);
        12:
            ExportGenMeters(DSS, Filename);
        13:
            ExportLoads(DSS, Filename);
        14:
            ExportMeters(DSS, FileName);
        15:
            if Length(Parm2) = 0 then
                DoSimpleMsg(DSS, 'Monitor name not specified. %s', [CRLF + DSS.Parser.CmdString], 251)
            else
            begin
{$IFDEF DSS_CAPI_PM}
                if not PMParent.ConcatenateReports then
                begin
{$ENDIF}
                    if Parm2 = 'all' then
                    begin
                        pMon := DSS.ActiveCircuit.Monitors.First;
                        while pMon <> NIL do
                        begin
                            if pMon <> NIL then
                            begin
                                pMon.TranslateToCSV(FALSE);
                                FileName := DSS.GlobalResult;
                            end;
                            pMon := DSS.ActiveCircuit.Monitors.Next;
                        end;
                    end
                    else
                    begin
                        pMon := DSS.MonitorClass.Find(Parm2);
                        if pMon <> NIL then
                        begin
                            pMon.TranslateToCSV(FALSE);
                            FileName := DSS.GlobalResult;
                        end
                        else
                            DoSimpleMsg(DSS, 'Monitor "%s" not found. %s', [Parm2, CRLF + DSS.Parser.CmdString], 250);
                    end;
{$IFDEF DSS_CAPI_PM}
                end
                else
                begin
                    InitP := 0;
                    FinalP := High(PMParent.Children);
                    for idxP := InitP to FinalP do
                    begin
                        if Parm2 = 'all' then
                        begin
                            pMon := PMParent.Children[idxP].ActiveCircuit.Monitors.First;
                            while pMon <> NIL do
                            begin
                                if pMon <> NIL then
                                begin
                                    pMon.TranslateToCSV(FALSE);
                                    FileName := DSS.GlobalResult;
                                end;
                                pMon := PMParent.Children[idxP].ActiveCircuit.Monitors.Next;
                            end;
                        end
                        else
                        begin
                            pMon := PMParent.Children[idxP].MonitorClass.Find(Parm2);
                            if pMon <> NIL then
                            begin
                                pMon.TranslateToCSV(FALSE);
                                FileName := DSS.GlobalResult;
                            end
                            else
                                DoSimpleMsg(DSS, 'Monitor "%s" not found. %s', [Parm2, CRLF + DSS.Parser.CmdString], 250);
                        end;
                    end;
                end;
{$ENDIF}
            end;
        16:
            ExportYprim(DSS, Filename);
        17:
            ExportY(DSS, Filename, TripletOpt);
        18:
            ExportSeqZ(DSS, Filename);
        19:
            ExportPbyphase(DSS, Filename, MVAOpt);
        20:
            DSS.CIMExporter.ExportCDPSM(Filename, Substation, SubGeographicRegion, GeographicRegion, FdrUuid, SubUuid, SubGeoUuid, RgnUuid, FALSE);
        21:
            DSS.CIMExporter.ExportCDPSM(Filename, Substation, SubGeographicRegion, GeographicRegion, FdrUuid, SubUuid, SubGeoUuid, RgnUuid, TRUE);
        22:
            DoSimpleMsg(DSS, _('Asset export no longer supported; use Export CIM100'), 252);
        23:
            ExportBusCoords(DSS, Filename);
        24:
            ExportLosses(DSS, Filename);
        25:
            ExportUuids(DSS, Filename);
        26:
            ExportCounts(DSS, Filename);
        27:
            ExportSummary(DSS, Filename);
        28:
            DoSimpleMsg(DSS, _('ElectricalProperties export no longer supported; use Export CIM100'), 252);
        29:
            DoSimpleMsg(DSS, _('Geographical export no longer supported; use Export CIM100'), 252);
        30:
            DoSimpleMsg(DSS, _('Topology export no longer supported; use Export CIM100'), 252);
        31:
            DoSimpleMsg(DSS, _('StateVariables export no longer supported; use Export CIM100'), 252);
        32:
            ExportProfile(DSS, FileName, PhasesToPlot);
        33:
            ExportEventLog(DSS, Filename);
        34:
            DumpAllocationFactors(DSS, FileName);
        35:
            ExportVoltagesElements(DSS, Filename);
        36:
            ExportGICMvar(DSS, Filename);
        37:
            ExportBusReliability(DSS, Filename);
        38:
            ExportBranchReliability(DSS, Filename);
        39:
            ExportNodeNames(DSS, Filename);
        40:
            ExportTaps(DSS, Filename);
        41:
            ExportNodeOrder(DSS, Filename);
        42:
            ExportElemCurrents(DSS, Filename);
        43:
            ExportElemVoltages(DSS, Filename);
        44:
            ExportElemPowers(DSS, Filename);
        45:
            ExportResult(DSS, Filename);
        46:
            ExportYNodeList(DSS, Filename);
        47:
            ExportYVoltages(DSS, Filename);
        48:
            ExportYCurrents(DSS, Filename);
        49:
            if DSS_CAPI_LEGACY_MODELS then
                ExportPVSystemMeters(DSS, FileName)
            else
                ExportPVSystem2Meters(DSS, FileName);
        50:
            ExportStorageMeters(DSS, Filename);
        51:
            ExportSections(DSS, FileName, pMeter);
        52:
            ExportErrorLog(DSS, Filename);
        53:
            ExportIncMatrix(DSS, Filename);
        54:
            ExportIncMatrixRows(DSS, Filename);
        55:
            ExportIncMatrixCols(DSS, Filename);
        56:
            ExportBusLevels(DSS, Filename);
        57:
            ExportLaplacian(DSS, FileName);
{$IFDEF DSS_CAPI_ADIAKOPTICS}
        58:
            ExportZLL(DSS, Filename);
        59:
            ExportZCC(DSS, Filename);
        60:
            ExportC(DSS, Filename);
        61:
            ExportY4(DSS, FileName);
{$ENDIF}
    else
        // ExportVoltages(DSS, Filename);    // default
        DoSimpleMsg(DSS, 'Error: Unknown Export command: "%s"', [parm1], 24713);
        AbortExport := TRUE;
    end;

    Result := 0;
    DSS.InShowResults := FALSE;

    if not AbortExport then
    begin
        SetLastResultFile(DSS, FileName);
        DSS.ParserVars.Add('@lastexportfile', FileName);
        if DSS.AutoShowExport then
            FireOffEditor(DSS, FileName);
    end;
end;

end.
