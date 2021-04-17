unit ExportOptions;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Command;

const
    NumExportOptions = 52;

function DoExportCmd: Integer;


var
    ExportOption,
    ExportHelp: array[1..NumExportOptions] of String;
    ExportCommands: TCommandList;

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
    NamedObject;

function AssignNewUUID(val: String): TUuid;
begin
    if Pos('{', val) < 1 then
        val := '{' + val + '}';
    result := StringToUuid(val);
end;

procedure DefineOptions;

begin

    ExportOption[1] := 'Voltages';
    ExportOption[2] := 'SeqVoltages';
    ExportOption[3] := 'Currents';
    ExportOption[4] := 'SeqCurrents';
    ExportOption[5] := 'Estimation';
    ExportOption[6] := 'Capacity';
    ExportOption[7] := 'Overloads';
    ExportOption[8] := 'Unserved';
    ExportOption[9] := 'Powers';
    ExportOption[10] := 'SeqPowers';
    ExportOption[11] := 'Faultstudy';
    ExportOption[12] := 'Generators';
    ExportOption[13] := 'Loads';
    ExportOption[14] := 'Meters';
    ExportOption[15] := 'Monitors';
    ExportOption[16] := 'Yprims';
    ExportOption[17] := 'Y';
    ExportOption[18] := 'seqz';
    ExportOption[19] := 'P_byphase';
    ExportOption[20] := 'CIM100Fragments';
    ExportOption[21] := 'CIM100';
    ExportOption[22] := 'CDPSMAsset';
    ExportOption[23] := 'Buscoords';
    ExportOption[24] := 'Losses';
    ExportOption[25] := 'Uuids';
    ExportOption[26] := 'Counts';
    ExportOption[27] := 'Summary';
    ExportOption[28] := 'CDPSMElec';
    ExportOption[29] := 'CDPSMGeo';
    ExportOption[30] := 'CDPSMTopo';
    ExportOption[31] := 'CDPSMStateVar';
    ExportOption[32] := 'Profile';
    ExportOption[33] := 'EventLog';
    ExportOption[34] := 'AllocationFactors';
    ExportOption[35] := 'VoltagesElements';
    ExportOption[36] := 'GICMvars';
    ExportOption[37] := 'BusReliability';
    ExportOption[38] := 'BranchReliability';
    ExportOption[39] := 'NodeNames';
    ExportOption[40] := 'Taps';
    ExportOption[41] := 'NodeOrder';
    ExportOption[42] := 'ElemCurrents';
    ExportOption[43] := 'ElemVoltages';
    ExportOption[44] := 'ElemPowers';
    ExportOption[45] := 'Result';
    ExportOption[46] := 'YNodeList';
    ExportOption[47] := 'YVoltages';
    ExportOption[48] := 'YCurrents';
    ExportOption[49] := 'PVSystem_Meters';
    ExportOption[50] := 'Storage_Meters';
    ExportOption[51] := 'Sections';
    ExportOption[52] := 'ErrorLog';

    ExportHelp[1] := '(Default file = EXP_VOLTAGES.CSV) Voltages to ground by bus/node.';
    ExportHelp[2] := '(Default file = EXP_SEQVOLTAGES.CSV) Sequence voltages.';
    ExportHelp[3] := '(Default file = EXP_CURRENTS.CSV) Currents in each conductor of each element.';
    ExportHelp[4] := '(Default file = EXP_SEQCURRENTS.CSV) Sequence currents in each terminal of 3-phase elements.';
    ExportHelp[5] := '(Default file = EXP_ESTIMATION.CSV) Results of last estimation.';
    ExportHelp[6] := '(Default file = EXP_CAPACITY.CSV) Capacity report.';
    ExportHelp[7] := '(Default file = EXP_OVERLOADS.CSV) Overloaded elements report.';
    ExportHelp[8] := '(Default file = EXP_UNSERVED.CSV) [UEonly] [Filename] Report on elements that are unserved due to violation of ratings.';
    ExportHelp[9] := '(Default file = EXP_POWERS.CSV) [MVA] [Filename] Powers (kVA by default) into each terminal of each element.';
    ExportHelp[10] := '(Default file = EXP_SEQPOWERS.CSV) Sequence powers into each terminal of 3-phase elements.';
    ExportHelp[11] := '(Default file = EXP_FAULTS.CSV) results of a fault study.';
    ExportHelp[12] := '(Default file = EXP_GENMETERS.CSV) Present values of generator meters. Adding the switch "/multiple" or "/m" will ' +
        ' cause a separate file to be written for each generator.';
    ExportHelp[13] := '(Default file = EXP_LOADS.CSV) Report on loads from most recent solution.';
    ExportHelp[14] := '(Default file = EXP_METERS.CSV) Energy meter exports. Adding the switch "/multiple" or "/m" will ' +
        ' cause a separate file to be written for each meter.';
    ExportHelp[15] := '(file name is assigned by Monitor export) Monitor values.';
    ExportHelp[16] := '(Default file = EXP_YPRIMS.CSV) All primitive Y matrices.';
    ExportHelp[17] := '(Default file = EXP_Y.CSV) [triplets] [Filename] System Y matrix, defaults to non-sparse format.';
    ExportHelp[18] := '(Default file = EXP_SEQZ.CSV) Equivalent sequence Z1, Z0 to each bus.';
    ExportHelp[19] := '(Default file = EXP_P_BYPHASE.CSV) [MVA] [Filename] Power by phase. Default is kVA.';
    ExportHelp[20] := '** Deprecated ** (IEC 61968-13, CIM16-17 version of the CDPSM Combined profile)';
    ExportHelp[20] := '(Default file ROOT = CIM100) (IEC 61968-13, CIM100 for unbalanced load flow profile)' + CRLF + ' produces 6 separate files ROOT_FUN.XML for Functional profile,' + CRLF + ' ROOT_EP.XML for Electrical Properties profile,' + CRLF + ' ROOT_TOPO.XML for Topology profile,' + CRLF + ' ROOT_CAT.XML for Asset Catalog profile,' + CRLF + ' ROOT_GEO.XML for Geographical profile and' + CRLF + ' ROOT_SSH.XML for Steady State Hypothesis profile' + CRLF + ' [File=fileroot fid=_uuidstring Substation=subname sid=_uuidstring' + CRLF + ' SubGeographicRegion=subgeoname sgrid=_uuidstring GeographicRegion=geoname rgnid=_uuidstring]';
    ExportHelp[21] := '(Default file = CIM100x.XML) (IEC 61968-13, combined CIM100 for unbalanced load flow profile)' + CRLF + ' [File=filename fid=_uuidstring Substation=subname sid=_uuidstring' + CRLF + ' SubGeographicRegion=subgeoname sgrid=_uuidstring GeographicRegion=geoname rgnid=_uuidstring]';
    ExportHelp[22] := '** Deprecated ** (IEC 61968-13, CDPSM Asset profile)';
    ExportHelp[23] := '[Default file = EXP_BUSCOORDS.CSV] Bus coordinates in csv form.';
    ExportHelp[24] := '[Default file = EXP_LOSSES.CSV] Losses for each element.';
    ExportHelp[25] := '[Default file = EXP_UUIDS.CSV] Uuids for each element. This frees the UUID list after export.';
    ExportHelp[26] := '[Default file = EXP_Counts.CSV] (instance counts for each class)';
    ExportHelp[27] := '[Default file = EXP_Summary.CSV] Solution summary.';
    ExportHelp[28] := '** Deprecated ** (IEC 61968-13, CDPSM Electrical Properties profile)';
    ExportHelp[29] := '** Deprecated ** (IEC 61968-13, CDPSM Geographical profile)';
    ExportHelp[30] := '** Deprecated ** (IEC 61968-13, CDPSM Topology profile)';
    ExportHelp[31] := '** Deprecated ** (IEC 61968-13, CDPSM State Variables profile)';
    ExportHelp[32] := '[Default file = EXP_Profile.CSV] Coordinates, color of each line section in Profile plot. Same options as Plot Profile Phases property.' + CRLF + CRLF +
        'Example:  Export Profile Phases=All [optional file name]';
    ExportHelp[33] := '(Default file = EXP_EventLog.CSV) All entries in the present event log.';
    ExportHelp[34] := 'Exports load allocation factors. File name is assigned.';
    ExportHelp[35] := '(Default file = EXP_VOLTAGES_ELEM.CSV) Voltages to ground by circuit element.';
    ExportHelp[36] := '(Default file = EXP_GIC_Mvar.CSV) Mvar for each GICtransformer object by bus for export to power flow programs ';
    ExportHelp[37] := '(Default file = EXP_BusReliability.CSV) Failure rate, number of interruptions and other reliability data at each bus.';
    ExportHelp[38] := '(Default file = EXP_BranchReliability.CSV) Failure rate, number of interruptions and other reliability data for each PD element.';
    ExportHelp[39] := '(Default file = EXP_NodeNames.CSV) Exports Single-column file of all node names in the active circuit. Useful for making scripts.';
    ExportHelp[40] := '(Default file = EXP_Taps.CSV)  Exports the regulator tap report similar to Show Taps.';
    ExportHelp[41] := '(Default file = EXP_NodeOrder.CSV)  Exports the present node order for all conductors of all circuit elements';
    ExportHelp[42] := '(Default file = EXP_ElemCurrents.CSV)  Exports the current into all conductors of all circuit elements';
    ExportHelp[43] := '(Default file = EXP_ElemVoltages.CSV)  Exports the voltages to ground at all conductors of all circuit elements';
    ExportHelp[44] := '(Default file = EXP_elemPowers.CSV)  Exports the powers into all conductors of all circuit elements';
    ExportHelp[45] := '(Default file = EXP_Result.CSV)  Exports the result of the most recent command.';
    ExportHelp[46] := '(Default file = EXP_YNodeList.CSV)  Exports a list of nodes in the same order as the System Y matrix.';
    ExportHelp[47] := '(Default file = EXP_YVoltages.CSV)  Exports the present solution complex Voltage array in same order as YNodeList.';
    ExportHelp[48] := '(Default file = EXP_YCurrents.CSV)  Exports the present solution complex Current array in same order as YNodeList. This is generally the injection current array';
    ExportHelp[49] := '(Default file = EXP_PVMETERS.CSV) Present values of PVSystem meters. Adding the switch "/multiple" or "/m" will ' +
        ' cause a separate file to be written for each PVSystem.';
    ExportHelp[50] := '(Default file = EXP_STORAGEMETERS.CSV) Present values of Storage meters. Adding the switch "/multiple" or "/m" will ' +
        ' cause a separate file to be written for each Storage device.';
    ExportHelp[51] := '(Default file = EXP_SECTIONS.CSV) Data for each section between overcurrent protection devices. ' + CRLF + CRLF +
        'Examples: ' + CRLF + '  Export Sections [optional filename]' + CRLF + 'Export Sections meter=M1 [optional filename]';
    ExportHelp[52] := '(Default file = EXP_ErrorLog.TXT) All entries in the present Error log.';

end;

//----------------------------------------------------------------------------
function DoExportCmd: Integer;

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

begin
    Result := 0;
    AbortExport := FALSE;
    FileName := '';

    ParamName := Parser.NextParam;
    Parm1 := LowerCase(Parser.StrValue);
    ParamPointer := ExportCommands.Getcommand(Parm1);

   {Check commands requiring a solution and abort if no solution or circuit}
    case ParamPointer of
        1..24, 28..32, 35, 46..51:
        begin
            if not assigned(ActiveCircuit) then
            begin
                DoSimpleMsg('No circuit created.', 24711);
                Exit;
            end;
            if not assigned(ActiveCircuit.Solution) or not assigned(ActiveCircuit.Solution.NodeV) then
            begin
                DoSimpleMsg('The circuit must be solved before you can do this.', 24712);
                Exit;
            end;
        end;
    end;


    MVAOpt := 0;
    UEonlyOpt := FALSE;
    TripletOpt := FALSE;
    PhasesToPlot := PROFILE3PH;  // init this to get rid of compiler warning
    pMeter := NIL;
    Substation := ActiveCircuit.Name + '_Substation';
    SubGeographicRegion := ActiveCircuit.Name + '_SubRegion';
    GeographicRegion := ActiveCircuit.Name + '_Region';
    DefaultCircuitUUIDs(FdrUuid, SubUuid, RgnUuid, SubGeoUuid);

    case ParamPointer of
        9, 19:
        begin { Trap export powers command and look for MVA/kVA option }
            ParamName := parser.nextParam;
            Parm2 := LowerCase(Parser.strvalue);
            MVAOpt := 0;
            if Length(Parm2) > 0 then
                if Parm2[1] = 'm' then
                    MVAOpt := 1;
        end;

        8:
        begin { Trap UE only flag  }
            ParamName := parser.nextParam;
            Parm2 := LowerCase(Parser.strvalue);
            UEonlyOpt := FALSE;
            if Length(Parm2) > 0 then
                if Parm2[1] = 'u' then
                    UEonlyOpt := TRUE;
        end;

        15:
        begin {Get monitor name for export monitors command}
            ParamName := Parser.NextParam;
            Parm2 := Parser.StrValue;
        end;

        17:
        begin { Trap Sparse Triplet flag  }
            ParamName := parser.nextParam;
            Parm2 := LowerCase(Parser.strvalue);
            TripletOpt := FALSE;
            if Length(Parm2) > 0 then
                if Parm2[1] = 't' then
                    TripletOpt := TRUE;
        end;

        20, 21:
        begin {user-supplied substation and regions}
            ParamName := LowerCase(parser.nextParam);
            Parm2 := Parser.strValue;
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
                ParamName := LowerCase(parser.nextParam);
                Parm2 := Parser.strValue;
            end;
        end;

        32:
        begin {Get phases to plot}
            ParamName := Parser.NextParam;
            Parm2 := Parser.StrValue;
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
                PhasesToPlot := Parser.IntValue;

        end;

        51:
        begin {Sections}
            ParamName := Parser.NextParam;
            Parm2 := Parser.StrValue;

            if CompareTextShortest(ParamName, 'meter') = 0 then
                pMeter := EnergyMeterClass.Find(Parm2);
        end;

    end;

   {Pick up next parameter on line, alternate file name, if any}
    if Length(FileName) = 0 then
    begin
        ParamName := Parser.NextParam;
        FileName := LowerCase(Parser.StrValue);    // should be full path name to work universally
    end;

    InShowResults := TRUE;

   {Assign default file name if alternate not specified}
    if Length(FileName) = 0 then
    begin
        case ParamPointer of
            1:
                FileName := 'EXP_VOLTAGES.CSV';
            2:
                FileName := 'EXP_SEQVOLTAGES.CSV';
            3:
                FileName := 'EXP_CURRENTS.CSV';
            4:
                FileName := 'EXP_SEQCURRENTS.CSV';
            5:
                FileName := 'EXP_ESTIMATION.CSV';   // Estimation error
            6:
                FileName := 'EXP_CAPACITY.CSV';
            7:
                FileName := 'EXP_OVERLOADS.CSV';
            8:
                FileName := 'EXP_UNSERVED.CSV';
            9:
                FileName := 'EXP_POWERS.CSV';
            10:
                FileName := 'EXP_SEQPOWERS.CSV';
            11:
                FileName := 'EXP_FAULTS.CSV';
            12:
                FileName := 'EXP_GENMETERS.CSV';
            13:
                FileName := 'EXP_LOADS.CSV';
            14:
                FileName := 'EXP_METERS.CSV';
         {15: Filename is assigned}
            16:
                Filename := 'EXP_YPRIM.CSV';
            17:
                Filename := 'EXP_Y.CSV';
            18:
                Filename := 'EXP_SEQZ.CSV';
            19:
                Filename := 'EXP_P_BYPHASE.CSV';
            20:
                FileName := 'CIM100';
            21:
                FileName := 'CIM100x.XML';
            22:
                FileName := '';
            23:
                FileName := 'EXP_BUSCOORDS.CSV';
            24:
                FileName := 'EXP_LOSSES.CSV';
            25:
                FileName := 'EXP_UUIDS.CSV';
            26:
                FileName := 'EXP_Counts.CSV';
            27:
                FileName := 'EXP_Summary.CSV';
            28:
                FileName := '';
            29:
                FileName := '';
            30:
                FileName := '';
            31:
                FileName := '';
            32:
                FileName := 'EXP_Profile.CSV';
            33:
                FileName := 'EXP_EventLog.CSV';
            34:
                FileName := 'AllocationFactors.Txt';
            35:
                FileName := 'EXP_VOLTAGES_ELEM.CSV';
            36:
                FileName := 'EXP_GIC_Mvar.CSV';
            37:
                FileName := 'EXP_BusReliability.CSV';
            38:
                FileName := 'EXP_BranchReliability.CSV';
            39:
                FileName := 'EXP_NodeNames.CSV';
            40:
                FileName := 'EXP_Taps.CSV';
            41:
                FileName := 'EXP_NodeOrder.CSV';
            42:
                FileName := 'EXP_ElemCurrents.CSV';
            43:
                FileName := 'EXP_ElemVoltages.CSV';
            44:
                FileName := 'EXP_ElemPowers.CSV';
            45:
                FileName := 'EXP_Result.CSV';
            46:
                FileName := 'EXP_YNodeList.CSV';
            47:
                FileName := 'EXP_YVoltages.CSV';
            48:
                FileName := 'EXP_YCurrents.CSV';
            49:
                FileName := 'EXP_PVMeters.CSV';
            50:
                FileName := 'EXP_STORAGEMeters.CSV';
            51:
                FileName := 'EXP_SECTIONS.CSV';
            52:
                FileName := 'EXP_ErrorLog.txt';

        else
            FileName := 'EXP_VOLTAGES.CSV';    // default
        end;
        FileName := GetOutputDirectory + CircuitName_ + FileName;  // Explicitly define directory
    end;

    case ParamPointer of
        1:
            ExportVoltages(FileName);
        2:
            ExportSeqVoltages(FileName);
        3:
            ExportCurrents(FileName);
        4:
            ExportSeqCurrents(FileName);
        5:
            ExportEstimation(FileName);   // Estimation error
        6:
            ExportCapacity(FileName);
        7:
            ExportOverLoads(FileName);
        8:
            ExportUnserved(FileName, UEOnlyOpt);
        9:
            ExportPowers(FileName, MVAOpt);
        10:
            ExportSeqPowers(FileName, MVAopt);
        11:
            ExportFaultStudy(FileName);
        12:
            ExportGenMeters(FileName);
        13:
            ExportLoads(FileName);
        14:
            ExportMeters(FileName);
        15:
            if Length(Parm2) > 0 then
            begin
                pMon := MonitorClass.Find(Parm2);
                if pMon <> NIL then
                begin
                    pMon.TranslateToCSV(FALSE);
                    FileName := GlobalResult;
                end
                else
                    DoSimpleMsg('Monitor "' + Parm2 + '" not found.' + CRLF + parser.CmdString, 250);
            end
            else
                DoSimpleMsg('Monitor Name Not Specified.' + CRLF + parser.CmdString, 251);
        16:
            ExportYprim(Filename);
        17:
            ExportY(Filename, TripletOpt);
        18:
            ExportSeqZ(Filename);
        19:
            ExportPbyphase(Filename, MVAOpt);
        20:
            ExportCDPSM(Filename, Substation, SubGeographicRegion, GeographicRegion, FdrUuid, SubUuid, SubGeoUuid, RgnUuid, FALSE);
        21:
            ExportCDPSM(Filename, Substation, SubGeographicRegion, GeographicRegion, FdrUuid, SubUuid, SubGeoUuid, RgnUuid, TRUE);
        22:
            DoSimpleMsg('Asset export no longer supported; use Export CIM100', 252);
        23:
            ExportBusCoords(Filename);
        24:
            ExportLosses(Filename);
        25:
            ExportUuids(Filename);
        26:
            ExportCounts(Filename);
        27:
            ExportSummary(Filename);
        28:
            DoSimpleMsg('ElectricalProperties export no longer supported; use Export CIM100', 252);
        29:
            DoSimpleMsg('Geographical export no longer supported; use Export CIM100', 252);
        30:
            DoSimpleMsg('Topology export no longer supported; use Export CIM100', 252);
        31:
            DoSimpleMsg('StateVariables export no longer supported; use Export CIM100', 252);
        32:
            ExportProfile(FileName, PhasesToPlot);
        33:
            ExportEventLog(FileName);
        34:
            DumpAllocationFactors(FileName);
        35:
            ExportVoltagesElements(FileName);
        36:
            ExportGICMvar(FileName);
        37:
            ExportBusReliability(FileName);
        38:
            ExportBranchReliability(FileName);
        39:
            ExportNodeNames(FileName);
        40:
            ExportTaps(FileName);
        41:
            ExportNodeOrder(FileName);
        42:
            ExportElemCurrents(FileName);
        43:
            ExportElemVoltages(FileName);
        44:
            ExportElemPowers(FileName);
        45:
            ExportResult(FileName);
        46:
            ExportYNodeList(FileName);
        47:
            ExportYVoltages(FileName);
        48:
            ExportYCurrents(FileName);
        49:
            ExportPVSystemMeters(FileName);
        50:
            ExportStorageMeters(FileName);
        51:
            ExportSections(FileName, pMeter);
        52:
            ExportErrorLog(FileName)

    else
        // ExportVoltages(FileName);    // default
        DoSimpleMsg('Error: Unknown Export command: "' + parm1 + '"', 24713);
        AbortExport := TRUE;
    end;

    Result := 0;
    InShowResults := FALSE;

    if not AbortExport then
    begin
        SetLastResultFile(FileName);
        ParserVars.Add('@lastexportfile', FileName);
        if AutoShowExport then
            FireOffEditor(FileName);
    end;

end;


procedure DisposeStrings;
var
    i: Integer;

begin
    for i := 1 to NumExportOptions do
    begin
        ExportOption[i] := '';
        ExportHelp[i] := '';
    end;

end;

initialization

    DefineOptions;

    ExportCommands := TCommandList.Create(ExportOption);
    ExportCommands.Abbrev := TRUE;

finalization

    DisposeStrings;
    ExportCommands.Free;
end.
