unit ShowOptions;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Command,
    DSSClass;

type
{$SCOPEDENUMS ON}
    TShowOption = (
        INVALID = 0,
        autoadded = 1,
        buses = 2,
        currents = 3,
        convergence = 4,
        elements = 5,
        faults = 6,
        isolated = 7,
        generators = 8,
        meters = 9,
        monitor = 10,
        panel = 11,
        powers = 12,
        voltages = 13,
        zone = 14,
        taps = 15,
        overloads = 16,
        unserved = 17,
        eventlog = 18,
        variables = 19,
        ratings = 20,
        loops = 21,
        losses = 22,
        busflow = 23,
        lineconstants = 24,
        yprim = 25,
        y = 26,
        controlqueue = 27,
        topology = 28,
        mismatch = 29,
        kvbasemismatch = 30,
        deltaV = 31,
        QueryLog = 32,
        Controlled = 33,
        Result = 34
    );
{$SCOPEDENUMS OFF}

const
    NumShowOptions = Ord(High(TShowOption));

function DoShowCmd({$IFDEF DSS_CAPI_PM}MainDSS{$ELSE}DSS{$ENDIF}: TDSSContext): Integer;
procedure DefineOptions(var ShowOption: ArrayOfString);

implementation

uses
    ShowResults,
    ParserDel,
    Monitor,
    Utilities,
    DSSGlobals,
    sysutils,
    CmdForms,
    LineUnits,
    DSSHelper,
    TypInfo;

procedure DefineOptions(var ShowOption: ArrayOfString);
var
    info: Pointer;
    i: Integer;
begin
    info := TypeInfo(TShowOption);
    SetLength(ShowOption, NumShowOptions);
    for i := 1 to NumShowOptions do
        ShowOption[i - 1] := GetEnumName(info, i);
end;

function DoShowCmd({$IFDEF DSS_CAPI_PM}MainDSS{$ELSE}DSS{$ENDIF}: TDSSContext): Integer;
var
    Param, Filname: String;
    ParamPointer: Integer;
    pMon: TMonitorObj;

    MVAopt: Integer;
    LLopt: Boolean;
    ShowResid: Boolean;
    ShowOptionCode: Integer;
    BusName: String;
    Freq: Double;
    Units: Integer;
    Rho_line: Double;
{$IFDEF DSS_CAPI_PM}
    InitP, FinalP, idxP: Integer;  // Variables added to concatenate the results in OpenDSS-PM
    PMParent, DSS: TDSSContext;
begin
    PMParent := MainDSS.GetPrime();
    DSS := MainDSS.ActiveChild;
{$ELSE}
begin
{$ENDIF}
    Result := 0;

    DSS.Parser.NextParam;
    Param := AnsiLowerCase(DSS.Parser.StrValue);
    ParamPointer := DSS.DSSExecutive.ShowCommands.Getcommand(Param);

    if ParamPointer = 0 then
    begin
        DoSimpleMsg(DSS, 'Error: Unknown Show Command:"%s"', [Param], 24700);
        Exit;
//        ParamPointer := 13;  {voltages}
    end;

    // Check commands requiring a solution and abort if no solution or circuit
    case ParamPointer of
        4, 6, 8..10, 12, 13..17, 19..23, 29..31:
        begin
            if not assigned(DSS.ActiveCircuit) then
            begin
                DoSimpleMsg(DSS, _('No circuit created.'), 24701);
                Exit;
            end;
            if not assigned(DSS.ActiveCircuit.Solution) or not assigned(DSS.ActiveCircuit.Solution.NodeV) then
            begin
                DoSimpleMsg(DSS, _('The circuit must be solved before you can do this.'), 24702);
                Exit;
            end;
        end;
    end;

    DSS.InShowResults := TRUE;

    case ParamPointer of
        1:
        begin {Autoadded}
            FireOffEditor(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'AutoAddedGenerators.txt');
            FireOffEditor(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'AutoAddedCapacitors.txt');
        end;
        2:
            ShowBuses(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'Buses.txt');
        3:
        begin
            ShowOptionCode := 0;
            ShowResid := FALSE;
            DSS.Parser.NextParam;   // Look for residual
            Param := AnsiUpperCase(DSS.Parser.StrValue);
           // logic handles show curr y|n|T elements or show curr elements
            if (Length(Param) > 0) then
                case Param[1] of
                    'Y', 'T':
                        ShowResid := TRUE;
                    'N':
                        ShowResid := FALSE;
                    'E':
                        ShowOptionCode := 1;
                end;
            DSS.Parser.NextParam;   // Look for another param
            Param := AnsiUpperCase(DSS.Parser.StrValue);
            if (Length(Param) > 0) then
                case Param[1] of
                    'E':
                        ShowOptionCode := 1;
                end;
            case ShowOptionCode of
                0:
                    Filname := 'Curr_Seq';
                1:
                    Filname := 'Curr_Elem';
            else
                FilName := 'ERROR';
            end;
            ShowCurrents(DSS, DSS.OutputDirectory + DSS.CircuitName_ + FilName + '.txt', ShowResid, ShowOptionCode);
        end;
        4:
            DSS.ActiveCircuit.Solution.WriteConvergenceReport(DSS.OutputDirectory + DSS.CircuitName_ + 'Convergence.txt');
        5:
        begin
            DSS.Parser.NextParam;   // Look for another param
            Param := AnsiLowerCase(DSS.Parser.StrValue);
            ShowElements(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'Elements.txt', Param);
        end;
        6:
            ShowFaultStudy(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'FaultStudy.txt');
        7:
            ShowIsolated(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'Isolated.txt');
        8:
            ShowGenMeters(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'GenMeterOut.txt');
        9:
            ShowMeters(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'EMout.txt');
        10:
        begin     // Show Monitor
            DSS.Parser.NextParam;
            Param := DSS.Parser.StrValue;
            if Length(Param) = 0 then
                DoSimpleMsg(DSS, 'Monitor Name Not Specified. %s', [CRLF + DSS.Parser.CmdString], 249)
            else
            begin
{$IFDEF DSS_CAPI_PM}
                if not PMParent.ConcatenateReports then
                begin
{$ENDIF}
                    pMon := DSS.MonitorClass.Find(Param);
                    if pMon <> NIL then
                        pMon.TranslateToCSV(TRUE)
                    else
                        DoSimpleMsg(DSS, 'Monitor "%s" not found. %s', [param, CRLF + DSS.Parser.CmdString], 248);
{$IFDEF DSS_CAPI_PM}
                end
                else
                begin
                    InitP := 0;
                    FinalP := High(PMParent.Children);
                    for idxP := InitP to FinalP do
                    begin
                        pMon := PMParent.Children[idxP].MonitorClass.Find(Param);
                        if pMon <> NIL then
                            pMon.TranslateToCSV((idxP = FinalP))
                        else
                            DoSimpleMsg(DSS, 'Monitor "%s" not found. %s', [param, CRLF + DSS.Parser.CmdString], 248);
                    end;
                end;
{$ENDIF}
            end;
        end;
        11:
            DoSimpleMsg(DSS, _('Command "show panel" is not supported in DSS Extensions.'), 999);
        12:
        begin
            ShowOptionCode := 0;
            MVAOpt := 0;
            FilName := 'Power';
            DSS.Parser.nextParam;
            Param := AnsiLowerCase(DSS.Parser.strvalue);
            if Length(Param) > 0 then
                case Param[1] of
                    'm':
                        MVAOpt := 1;
                    'e':
                        ShowOptionCode := 1;
                end;
            DSS.Parser.nextParam;
            Param := AnsiLowerCase(DSS.Parser.strvalue);
            if Length(Param) > 0 then
                if Param[1] = 'e' then
                    ShowOptionCode := 1;
            if ShowOptionCode = 1 then
                FilName := FilName + '_elem'
            else
                FilName := FilName + '_seq';
            if MVAOpt = 1 then
                FilName := FilName + '_MVA'
            else
                FilName := FilName + '_kVA';

            ShowPowers(DSS, DSS.OutputDirectory + DSS.CircuitName_ + filname + '.txt', MVAOpt, ShowOptionCode);
        end;
        13:
        begin
            LLOpt := FALSE;      // Line-Line voltage option
            ShowOptionCode := 0;
            {Check for LL or LN option}
            DSS.Parser.nextParam;
            Param := DSS.Parser.strvalue;

            FilName := 'VLN';
            if Length(Param) > 0 then
                if CompareText(Param, 'LL') = 0 then
                begin
                    LLopt := TRUE;
                    FilName := 'VLL';
                end;
            {Check for Seq | nodes | elements}
            DSS.Parser.nextParam;
            Param := AnsiUpperCase(DSS.Parser.strvalue);
            if Length(Param) > 0 then
                case Param[1] of
                    'N':
                    begin
                        ShowOptionCode := 1;
                        FilName := FilName + '_Node';
                    end;
                    'E':
                    begin
                        ShowOptionCode := 2;
                        FilName := FilName + '_elem';
                    end;
                else
                    FilName := FilName + '_seq';
                end;
            ShowVoltages(DSS, DSS.OutputDirectory + DSS.CircuitName_ + FilName + '.txt', LLopt, ShowOptionCode);
        end;
        14:
            ShowMeterZone(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'ZoneOut.txt');
        15:
            ShowRegulatorTaps(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'RegTaps.txt');
        16:
            ShowOverloads(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'Overload.txt');
        17:
        begin
            DSS.Parser.NextParam;
            Param := DSS.Parser.StrValue;
            if Length(Param) > 0 then
                ShowUnserved(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'Unserved.txt', TRUE)
            else
                ShowUnserved(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'Unserved.txt', FALSE);
        end;
        18:
            ShowEventLog(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'EventLog.txt');// ShowMessageForm(EventStrings);
        19:
            ShowVariables(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'Variables.txt');
        20:
            ShowRatings(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'RatingsOut.txt');
        21:
            ShowLoops(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'Loops.txt');
        22:
            ShowLosses(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'Losses.txt');
        23:
        begin  // Show Bus Power Report
            ShowOptionCode := 0;
            MVAOpt := 0;
            DSS.Parser.nextParam; // Get busname
            Busname := DSS.Parser.strvalue;
            if Length(BusName) > 0 then
                FilName := BusName
            else
                FilName := 'BusPower';
            DSS.Parser.nextParam;
            Param := AnsiLowerCase(DSS.Parser.strvalue);
            if Length(Param) > 0 then
                case Param[1] of
                    'm':
                        MVAOpt := 1;
                    'e':
                        ShowOptionCode := 1;
                end;
            DSS.Parser.nextParam;
            Param := AnsiLowerCase(DSS.Parser.strvalue);
            if Length(Param) > 0 then
                if Param[1] = 'e' then
                    ShowOptionCode := 1;
            if ShowOptionCode = 1 then
                FilName := FilName + '_elem'
            else
                FilName := FilName + '_seq';
            if MVAOpt = 1 then
                FilName := FilName + '_MVA'
            else
                FilName := FilName + '_kVA';

            ShowBusPowers(DSS, DSS.OutputDirectory + DSS.CircuitName_ + FilName + '.txt', BusName, MVAOpt, ShowOptionCode);
        end;
        24:
        begin {ShowLineConstants  Show Lineconstants 60 mi}
            Freq := DSS.DefaultBaseFreq;  // Default
            Units := UNITS_KFT; // 'kft'; // default
            Rho_line := 100.0;
            DSS.Parser.nextparam;
            if Length(DSS.Parser.strvalue) > 0 then
                Freq := DSS.Parser.dblvalue;
            DSS.Parser.nextparam;
            if Length(DSS.Parser.strvalue) > 0 then
                Units := GetUnitsCode(DSS.Parser.strvalue);
            DSS.Parser.nextparam;
            if Length(DSS.Parser.strvalue) > 0 then
                Rho_line := DSS.Parser.dblValue;
            ShowLineConstants(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'LineConstants.txt', freq, units, Rho_line);
        end;

        25:
            if DSS.ActiveCircuit <> NIL then
            begin  {Yprim}
                with DSS.ActiveCircuit.ActiveCktElement do
                    ShowYprim(DSS, DSS.OutputDirectory + ParentClass.name + '_' + name + '_Yprim.txt');
            end;

        26:
        begin   {Y}
            ShowY(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'SystemY.txt');
        end;
        27:
            if DSS.ActiveCircuit <> NIL then
                DSS.ActiveCircuit.ControlQueue.ShowQueue(DSS.OutputDirectory + DSS.CircuitName_ + 'ControlQueue.csv');
        28:
            ShowTopology(DSS, DSS.OutputDirectory + DSS.CircuitName_);
        29:
            ShowNodeCurrentSum(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'NodeMismatch.txt');
        30:
            ShowkVBaseMismatch(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'kVBaseMismatch.txt');
        31:
            ShowDeltaV(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'DeltaV.txt');
        32:
            FireOffEditor(DSS, DSS.QueryLogFileName);
        33:
            ShowControlledElements(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'ControlledElements.csv');
        34:
            ShowResult(DSS, DSS.OutputDirectory + DSS.CircuitName_ + 'Result.csv');
    end;

    DSS.InShowResults := FALSE;
end;

end.
