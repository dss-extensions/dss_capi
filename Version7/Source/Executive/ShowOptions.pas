unit ShowOptions;

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
    NumShowOptions = 34;

function DoShowCmd: Integer;


var
    ShowOption,
    ShowHelp: array[1..NumShowOptions] of String;
    ShowCommands: TCommandList;

implementation

uses
    ShowResults,
    ParserDel,
    Monitor,
    Utilities,
    DSSGlobals,
    sysutils,
{$IFDEF FPC}
    CmdForms,
{$ELSE}
    DSSForms,
{$ENDIF}
    LineUnits;

procedure DefineOptions;

begin

    ShowOption[1] := 'autoadded';
    ShowOption[2] := 'buses';
    ShowOption[3] := 'currents';
    ShowOption[4] := 'convergence';
    ShowOption[5] := 'elements';
    ShowOption[6] := 'faults';
    ShowOption[7] := 'isolated';
    ShowOption[8] := 'generators';
    ShowOption[9] := 'meters';
    ShowOption[10] := 'monitor';
    ShowOption[11] := 'panel';
    ShowOption[12] := 'powers';
    ShowOption[13] := 'voltages';
    ShowOption[14] := 'zone';
    ShowOption[15] := 'taps';
    ShowOption[16] := 'overloads';
    ShowOption[17] := 'unserved';
    ShowOption[18] := 'eventlog';
    ShowOption[19] := 'variables';
    ShowOption[20] := 'ratings';
    ShowOption[21] := 'loops';
    ShowOption[22] := 'losses';
    ShowOption[23] := 'busflow';
    ShowOption[24] := 'lineconstants';
    ShowOption[25] := 'yprim';
    ShowOption[26] := 'y';
    ShowOption[27] := 'controlqueue';
    ShowOption[28] := 'topology';
    ShowOption[29] := 'mismatch';
    ShowOption[30] := 'kvbasemismatch';
    ShowOption[31] := 'deltaV';
    ShowOption[32] := 'QueryLog';
    ShowOption[33] := 'Controlled';
    ShowOption[34] := 'Result';


    ShowHelp[1] := 'Shows auto added capacitors or generators. See AutoAdd solution mode.';
    ShowHelp[2] := 'Report showing all buses and nodes currently defined.';
    ShowHelp[3] := 'Report showing currents from most recent solution. syntax: ' + CRLF + CRLF +
        'Show Currents  [[residual=]yes|no*] [Seq* | Elements]' + CRLF + CRLF +
        'If "residual" flag is yes, the sum of currents in all conductors is reported. ' +
        'Default is to report Sequence currents; otherwise currents in all conductors are reported.';
    ShowHelp[4] := 'Report on the convergence of each node voltage.';
    ShowHelp[5] := 'Shows names of all elements in circuit or all elements of a specified class. Syntax: ' + CRLF + CRLF +
        'Show ELements [Classname] ' + CRLF + CRLF +
        'Useful for creating scripts that act on selected classes of elements. ';
    ShowHelp[6] := 'After fault study solution, shows fault currents.';
    ShowHelp[7] := 'Report showing buses and elements that are isolated from the main source.';
    ShowHelp[8] := 'Report showing generator elements currently defined and the values of the energy meters ' + CRLF +
        'associated with each generator.';
    ShowHelp[9] := 'Shows the present values of the registers in the EnergyMeter elements.';
    ShowHelp[10] := 'Shows the contents of a selected monitor. Syntax: ' + CRLF + CRLF +
        ' Show Monitor  monitorname';
    ShowHelp[11] := 'Shows control panel. (not necessary for standalone version)';
    ShowHelp[12] := 'Report on powers flowing in circuit from most recent solution. ' + CRLF +
        'Powers may be reported in kVA or MVA and in sequence quantities or in every ' +
        'conductor of each element. Syntax:' + CRLF + CRLF +
        'Show Powers [MVA|kVA*] [Seq* | Elements]' + CRLF + CRLF +
        'Sequence powers in kVA is the default. Examples:' + CRLF + CRLF +
        'Show powers' + CRLF +
        'Show power kva element' + CRLF +
        'Show power mva elem';
    ShowHelp[13] := 'Reports voltages from most recent solution. Voltages are reported with respect to ' + CRLF +
        'system reference (Node 0) by default (LN option), but may also be reported Line-Line (LL option).' + CRLF +
        'The voltages are normally reported by bus/node, but may also be reported by circuit element. Syntax:' + CRLF + CRLF +
        'Show Voltages [LL |LN*]  [Seq* | Nodes | Elements]' + CRLF + CRLF +
        'Show Voltages' + crlf +
        'Show Voltage LN Nodes' + CRLF +
        'Show Voltages LL Nodes' + CRLF +
        'Show Voltage LN Elem';
    ShowHelp[14] := 'Shows the zone for a selected EnergyMeter element. Shows zone either in ' +
        'a text file or in a graphical tree view.' + CRLF + CRLF +
        'Show Zone  energymetername [Treeview]';
    ShowHelp[15] := 'Shows the regulator/LTC taps from the most recent solution.';
    ShowHelp[16] := 'Shows overloaded power delivery elements.';
    ShowHelp[17] := 'Shows loads that are "unserved". That is, loads for which the voltage is too low, ' +
        'or a branch on the source side is overloaded. If UEonly is specified, shows only those loads ' +
        'in which the emergency rating has been exceeded. Syntax:' + CRLF + CRLF +
        'Show Unserved [UEonly] (unserved loads)';
    ShowHelp[18] := 'Shows the present event log. (Regulator tap changes, capacitor switching, etc.)';
    ShowHelp[19] := 'Shows internal state variables of devices (Power conversion elements) that report them.';
    ShowHelp[20] := 'Shows ratings of power delivery elements.';
    ShowHelp[21] := 'Shows closed loops detected by EnergyMeter elements that are possibly unwanted. Otherwise, loops are OK.';
    ShowHelp[22] := 'Reports losses in each element and in the entire circuit.';
    ShowHelp[23] := 'Creates a report showing power and current flows as well as voltages around a selected bus. Syntax:' + CRLF + CRLF +
        'Show BUSFlow busname [MVA|kVA*] [Seq* | Elements]' + CRLF + CRLF +
        'Show busflow busxxx kVA elem' + CRLF +
        'Show busflow busxxx MVA seq' + CRLF + CRLF +
        'NOTE: The Show menu will prompt you for these values.';
    ShowHelp[24] := 'Creates two report files for the line constants (impedances) of every LINEGEOMETRY element currently defined. ' +
        'One file shows the main report with the matrices. The other file contains corresponding LINECODE ' +
        'definitions that you may use in subsequent simulations.  Syntax:' + CRLF + CRLF +
        'Show LIneConstants [frequency] [none|mi|km|kft|m|me|ft|in|cm] [rho]' + CRLF + CRLF +
        'Specify the frequency, length units and earth resistivity (meter-ohms). Examples:' + CRLF + CRLF +
        'Show Lineconstants 60 kft 100' + CRLF +
        'Show Linecon 50 km 1000';
    ShowHelp[25] := 'Show the primitive admittance (y) matrix for the active element.';
    ShowHelp[26] := 'Show the system Y matrix. Could be a large file!';
    ShowHelp[27] := 'Shows the present contents of the control queue.';
    ShowHelp[28] := 'Shows the topology as seen by the SwtControl elements.';
    ShowHelp[29] := 'Shows the current mismatches at each node in amperes and percent of max currents at node.';
    ShowHelp[30] := 'Creates a report of Load and Generator elements for which the base voltage does not match the Bus base voltage. ' +
        'Scripts for correcting the voltage base are suggested.';
    ShowHelp[31] := 'Show voltages ACROSS each 2-terminal element, phase-by-phase. ';
    ShowHelp[32] := 'Show Query Log file. ';
    ShowHelp[33] := 'Show Controlled elements and the names of the controls connected to them in CSV format.';
    ShowHelp[34] := 'Show last result (in @result variable).';

end;


//----------------------------------------------------------------------------
function DoShowCmd: Integer;

var
    ParamName, Param, Filname: String;
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


begin
    Result := 0;

    ParamName := Parser.NextParam;
    Param := LowerCase(Parser.StrValue);
    ParamPointer := ShowCommands.Getcommand(Param);

    if ParamPointer = 0 then
    begin
        DoSimpleMsg('Error: Unknown Show Command:"' + Param + '"', 24700);
        Exit;
//        ParamPointer := 13;  {voltages}
    end;

   {Check commands requiring a solution and abort if no solution or circuit}
    case ParamPointer of
        4, 6, 8..10, 12, 13..17, 19..23, 29..31:
        begin
            if not assigned(ActiveCircuit) then
            begin
                DoSimpleMsg('No circuit created.', 24701);
                Exit;
            end;
            if not assigned(ActiveCircuit.Solution) or not assigned(ActiveCircuit.Solution.NodeV) then
            begin
                DoSimpleMsg('The circuit must be solved before you can do this.', 24702);
                Exit;
            end;
        end;
    end;

    InShowResults := TRUE;

    case ParamPointer of
        1:
        begin {Autoadded}
            FireOffEditor(GetOutputDirectory + CircuitName_ + 'AutoAddedGenerators.Txt');
            FireOffEditor(GetOutputDirectory + CircuitName_ + 'AutoAddedCapacitors.Txt');
        end;
        2:
            ShowBuses(GetOutputDirectory + CircuitName_ + 'Buses.Txt');
        3:
        begin
            ShowOptionCode := 0;
            ShowResid := FALSE;
            ParamName := Parser.NextParam;   // Look for residual
            Param := Uppercase(Parser.StrValue);
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
            ParamName := Parser.NextParam;   // Look for another param
            Param := Uppercase(Parser.StrValue);
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
            end;
            ShowCurrents(GetOutputDirectory + CircuitName_ + FilName + '.Txt', ShowResid, ShowOptionCode);
        end;
        4:
            ActiveCircuit.Solution.WriteConvergenceReport(GetOutputDirectory + CircuitName_ + 'Convergence.TXT');
        5:
        begin
            ParamName := Parser.NextParam;   // Look for another param
            Param := LowerCase(Parser.StrValue);
            ShowElements(GetOutputDirectory + CircuitName_ + 'Elements.Txt', Param);
        end;
        6:
            ShowFaultStudy(GetOutputDirectory + CircuitName_ + 'FaultStudy.Txt');
        7:
            ShowIsolated(GetOutputDirectory + CircuitName_ + 'Isolated.Txt');
        8:
            ShowGenMeters(GetOutputDirectory + CircuitName_ + 'GenMeterOut.Txt');
        9:
            ShowMeters(GetOutputDirectory + CircuitName_ + 'EMout.Txt');
        10:
        begin     // Show Monitor
            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
            if Length(Param) > 0 then
            begin
                pMon := MonitorClass.Find(Param);
                if pMon <> NIL then
                    pMon.TranslateToCSV(TRUE)
                else
                    DoSimpleMsg('Monitor "' + param + '" not found.' + CRLF + parser.CmdString, 248);
            end
            else
                DoSimpleMsg('Monitor Name Not Specified.' + CRLF + parser.CmdString, 249);
        end;
        11:
            ShowControlPanel;
        12:
        begin
            ShowOptionCode := 0;
            MVAOpt := 0;
            FilName := 'Power';
            Paramname := parser.nextParam;
            Param := LowerCase(Parser.strvalue);
            if Length(Param) > 0 then
                case Param[1] of
                    'm':
                        MVAOpt := 1;
                    'e':
                        ShowOptionCode := 1;
                end;
            Paramname := parser.nextParam;
            Param := LowerCase(Parser.strvalue);
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

            ShowPowers(GetOutputDirectory + CircuitName_ + filname + '.txt', MVAOpt, ShowOptionCode);
        end;
        13:
        begin
            LLOpt := FALSE;      // Line-Line voltage option
            ShowOptionCode := 0;
            {Check for LL or LN option}
            Paramname := parser.nextParam;
            Param := Parser.strvalue;

            FilName := 'VLN';
            if Length(Param) > 0 then
                if CompareText(Param, 'LL') = 0 then
                begin
                    LLopt := TRUE;
                    FilName := 'VLL';
                end;
            {Check for Seq | nodes | elements}
            Paramname := parser.nextParam;
            Param := UpperCase(Parser.strvalue);
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
            ShowVoltages(GetOutputDirectory + CircuitName_ + FilName + '.Txt', LLopt, ShowOptionCode);
        end;
        14:
            ShowMeterZone(GetOutputDirectory + CircuitName_ + 'ZoneOut.Txt');
        15:
            ShowRegulatorTaps(GetOutputDirectory + CircuitName_ + 'RegTaps.Txt');
        16:
            ShowOverloads(GetOutputDirectory + CircuitName_ + 'Overload.Txt');
        17:
        begin
            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
            if Length(Param) > 0 then
                ShowUnserved(GetOutputDirectory + CircuitName_ + 'Unserved.Txt', TRUE)
            else
                ShowUnserved(GetOutputDirectory + CircuitName_ + 'Unserved.Txt', FALSE);
        end;
        18:
            ShowEventLog(GetOutputDirectory + CircuitName_ + 'EventLog.Txt');// ShowMessageForm(EventStrings);
        19:
            ShowVariables(GetOutputDirectory + CircuitName_ + 'Variables.Txt');
        20:
            ShowRatings(GetOutputDirectory + CircuitName_ + 'RatingsOut.Txt');
        21:
            ShowLoops(GetOutputDirectory + CircuitName_ + 'Loops.Txt');
        22:
            ShowLosses(GetOutputDirectory + CircuitName_ + 'Losses.Txt');
        23:
        begin  // Show Bus Power Report
            ShowOptionCode := 0;
            MVAOpt := 0;
            Paramname := parser.nextParam; // Get busname
            Busname := Parser.strvalue;
            if Length(BusName) > 0 then
                FilName := BusName
            else
                FilName := 'BusPower';
            Paramname := parser.nextParam;
            Param := LowerCase(Parser.strvalue);
            if Length(Param) > 0 then
                case Param[1] of
                    'm':
                        MVAOpt := 1;
                    'e':
                        ShowOptionCode := 1;
                end;
            Paramname := parser.nextParam;
            Param := LowerCase(Parser.strvalue);
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

            ShowBusPowers(GetOutputDirectory + CircuitName_ + FilName + '.txt', BusName, MVAOpt, ShowOptionCode);
        end;
        24:
        begin {ShowLineConstants  Show Lineconstants 60 mi}
            Freq := DefaultBaseFreq;  // Default
            Units := UNITS_KFT; // 'kft'; // default
            Rho_line := 100.0;
            ParamName := parser.nextparam;
            if Length(Parser.strvalue) > 0 then
                Freq := Parser.dblvalue;
            ParamName := parser.nextparam;
            if Length(Parser.strvalue) > 0 then
                Units := GetUnitsCode(Parser.strvalue);
            ParamName := parser.nextparam;
            if Length(Parser.strvalue) > 0 then
                Rho_line := Parser.dblValue;
            ShowLineConstants(GetOutputDirectory + CircuitName_ + 'LineConstants.txt', freq, units, Rho_line);
        end;

        25:
            if ActiveCircuit <> NIL then
            begin  {Yprim}
                with ActiveCircuit.ActiveCktElement do
                    ShowYprim(GetOutputDirectory + ParentClass.name + '_' + name + '_Yprim.txt');
            end;

        26:
        begin   {Y}
            ShowY(GetOutputDirectory + CircuitName_ + 'SystemY.txt');
        end;
        27:
            if ActiveCircuit <> NIL then
                ActiveCircuit.ControlQueue.ShowQueue(GetOutputDirectory + CircuitName_ + 'ControlQueue.csv');
        28:
            ShowTopology(GetOutputDirectory + CircuitName_);
        29:
            ShowNodeCurrentSum(GetOutputDirectory + CircuitName_ + 'NodeMismatch.Txt');
        30:
            ShowkVBaseMismatch(GetOutputDirectory + CircuitName_ + 'kVBaseMismatch.Txt');
        31:
            ShowDeltaV(GetOutputDirectory + CircuitName_ + 'DeltaV.Txt');
        32:
            FireOffEditor(QueryLogFileName);
        33:
            ShowControlledElements(GetOutputDirectory + CircuitName_ + 'ControlledElements.CSV');
        34:
            ShowResult(GetOutputDirectory + CircuitName_ + 'Result.CSV');
    else
    end;


    InShowResults := FALSE;

end;

procedure DisposeStrings;
var
    i: Integer;

begin
    for i := 1 to NumShowOptions do
    begin
        ShowOption[i] := '';
        ShowHelp[i] := '';
    end;

end;

initialization

    DefineOptions;

    ShowCommands := TCommandList.Create(ShowOption);
    ShowCommands.Abbrev := TRUE;

finalization

    DisposeStrings;
    ShowCommands.Free;

end.
