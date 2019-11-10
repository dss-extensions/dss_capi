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

const
    NumExecCommands = 111;

var

    ExecCommand,
    CommandHelp: array[1..NumExecCommands] of String;

    CommandList: TCommandList;

procedure ProcessCommand(DSS: TDSS; const CmdLine: String);

implementation

uses
    DSSGlobals,
    ExecHelper,
    Executive,
    ExecOptions,
    ShowOptions,
    ExportOptions,
    ParserDel,
    LoadShape,
     {$IFDEF FPC}
    CmdForms,
{$ELSE}
    PlotOptions,
    DSSForms,
    ConnectOptions,
{$ENDIF}
    sysutils,
    Utilities,
    SolutionAlgs,
    DSSHelper;

procedure DefineCommands;

begin
    {Main executive commands}
    ExecCommand[1] := 'New';
    ExecCommand[2] := 'Edit';
    ExecCommand[3] := 'More';
    ExecCommand[4] := 'M';
    ExecCommand[5] := '~';
    ExecCommand[6] := 'Select';
    ExecCommand[7] := 'Save';
    ExecCommand[8] := 'Show';
    ExecCommand[9] := 'Solve';
    ExecCommand[10] := 'Enable';
    ExecCommand[11] := 'Disable';
    ExecCommand[12] := 'Plot';
    ExecCommand[13] := 'Reset';
    ExecCommand[14] := 'Compile';
    ExecCommand[15] := 'Set';   // Set DSS Options
    ExecCommand[16] := 'Dump';   // Debug dump
    ExecCommand[17] := 'Open';   // Open a device terminal conductor
    ExecCommand[18] := 'Close';   // Close a device terminal conductor
    ExecCommand[19] := '//';       // Comment
    ExecCommand[20] := 'Redirect';
    ExecCommand[21] := 'Help';
    ExecCommand[22] := 'Quit';
    ExecCommand[23] := '?';   // Property Value inquiry
    ExecCommand[24] := 'Next';
    ExecCommand[25] := 'Panel';
    ExecCommand[26] := 'Sample';
    ExecCommand[27] := 'Clear';
    ExecCommand[28] := 'About';
    ExecCommand[29] := 'Calcvoltagebases';  //  Computes voltage bases
    ExecCommand[30] := 'SetkVBase';  //  Set kV Base at a Bus
    ExecCommand[31] := 'BuildY';  //  forces Rebuild of Y matrix right now
    ExecCommand[32] := 'Get';  //  returns values set WITH Set command
    ExecCommand[33] := 'Init';
    ExecCommand[34] := 'Export';
    ExecCommand[35] := 'Fileedit';
    ExecCommand[36] := 'Voltages';
    ExecCommand[37] := 'Currents';
    ExecCommand[38] := 'Powers';
    ExecCommand[39] := 'Seqvoltages';
    ExecCommand[40] := 'Seqcurrents';
    ExecCommand[41] := 'Seqpowers';
    ExecCommand[42] := 'Losses';
    ExecCommand[43] := 'Phaselosses';
    ExecCommand[44] := 'Cktlosses';
    ExecCommand[45] := 'Allocateloads';
    ExecCommand[46] := 'Formedit';
    ExecCommand[47] := 'Totals';  // Total all energymeters
    ExecCommand[48] := 'Capacity';  // Find upper kW limit of system for present year
    ExecCommand[49] := 'Classes';  // List of intrinsic classes
    ExecCommand[50] := 'Userclasses';  // List of user-defined classes
    ExecCommand[51] := 'Zsc';
    ExecCommand[52] := 'Zsc10';
    ExecCommand[53] := 'ZscRefresh';
    ExecCommand[54] := 'Ysc';
    ExecCommand[55] := 'puvoltages';
    ExecCommand[56] := 'VarValues';
    ExecCommand[57] := 'Varnames';
    ExecCommand[58] := 'Buscoords';
    ExecCommand[59] := 'MakeBusList';
    ExecCommand[60] := 'MakePosSeq';
    ExecCommand[61] := 'Reduce';
    ExecCommand[62] := 'Interpolate';
    ExecCommand[63] := 'AlignFile';
    ExecCommand[64] := 'TOP';
    ExecCommand[65] := 'Rotate';
    ExecCommand[66] := 'Vdiff';
    ExecCommand[67] := 'Summary';
    ExecCommand[68] := 'Distribute';
    ExecCommand[69] := 'DI_plot';
    ExecCommand[70] := 'Comparecases';
    ExecCommand[71] := 'YearlyCurves';
    ExecCommand[72] := 'CD';
    ExecCommand[73] := 'Visualize';
    ExecCommand[74] := 'CloseDI';
    ExecCommand[75] := 'DOScmd';
    ExecCommand[76] := 'Estimate';
    ExecCommand[77] := 'Reconductor';
    ExecCommand[78] := '_InitSnap';
    ExecCommand[79] := '_SolveNoControl';
    ExecCommand[80] := '_SampleControls';
    ExecCommand[81] := '_DoControlActions';
    ExecCommand[82] := '_ShowControlQueue';
    ExecCommand[83] := '_SolveDirect';
    ExecCommand[84] := '_SolvePFlow';
    ExecCommand[85] := 'AddBusMarker';

    ExecCommand[86] := 'Uuids';
    ExecCommand[87] := 'SetLoadAndGenKV';
    ExecCommand[88] := 'CvrtLoadshapes';
    ExecCommand[89] := 'NodeDiff';
    ExecCommand[90] := 'Rephase';
    ExecCommand[91] := 'SetBusXY';
    ExecCommand[92] := 'UpdateStorage';
    ExecCommand[93] := 'Obfuscate';
    ExecCommand[94] := 'LatLongCoords';
    ExecCommand[95] := 'BatchEdit';
    ExecCommand[96] := 'Pstcalc';
    ExecCommand[97] := 'Variable';
    ExecCommand[98] := 'ReprocessBuses';
    ExecCommand[99] := 'ClearBusMarkers';
    ExecCommand[100] := 'RelCalc';
    ExecCommand[101] := 'var';
    ExecCommand[102] := 'Cleanup';
    ExecCommand[103] := 'FinishTimeStep';
    ExecCommand[104] := 'NodeList';
    ExecCommand[105] := 'Connect';
    ExecCommand[106] := 'Disconnect';
    ExecCommand[107] := 'Remove';
    ExecCommand[108] := 'CalcIncMatrix';
    ExecCommand[109] := 'CalcIncMatrix_O';
    ExecCommand[110] := 'Refine_BusLevels';
    ExecCommand[111] := 'CalcLaplacian';

    CommandHelp[1] := 'Create a new object within the DSS. Object becomes the ' +
        'active object' + CRLF +
        'Example: New Line.line1 ...';
    CommandHelp[2] := 'Edit an object. The object is selected and it then becomes the active object.' + CRLF + CRLF +
        'Note that Edit is the default command.  You many change a property value simply by ' +
        'giving the full property name and the new value, for example:' + CRLF + CRLF +
        'line.line1.r1=.04' + CRLF +
        'vsource.source.kvll=230';
    CommandHelp[3] := 'Continuation of editing on the active object.';
    CommandHelp[4] := 'Continuation of editing on the active object. An abbreviation for More';
    CommandHelp[5] := 'Continuation of editing on the active object. An abbreviation.' + CRLF +
        CRLF +
        'Example:' + CRLF +
        'New Line.Line1 Bus1=aaa  bus2=bbb' + CRLF +
        '~ R1=.058' + CRLF +
        '~ X1=.1121';
    CommandHelp[6] := 'Selects an element and makes it the active element.  You can also specify the ' +
        'active terminal (default = 1).' + CRLF + CRLF +
        'Syntax:' + CRLF +
        'Select [element=]elementname  [terminal=]terminalnumber ' + CRLF + CRLF +
        'Example:' + CRLF +
        'Select Line.Line1 ' + CRLF +
        '~ R1=.1' + CRLF + '(continue editing)' + CRLF + CRLF +
        'Select Line.Line1 2 ' + CRLF +
        'Voltages  (returns voltages at terminal 2 in Result)';
    CommandHelp[7] := '{Save [class=]{Meters | Circuit | Voltages | (classname)} [file=]filename [dir=]directory ' + CRLF + CRLF +
        'Default class = Meters, which saves the present values in both monitors and energy meters in the active circuit. ' + CRLF + CRLF +
        '"Save Circuit" saves the present enabled circuit elements to the specified subdirectory in standard DSS form ' +
        'with a Master.txt file and separate files for each class of data. ' + CRLF + CRLF +
        'If Dir= not specified a unique name based on the circuit name is created automatically. ' + CRLF + CRLF +
        'If Dir= is specified, any existing files are overwritten. ' + CRLF + CRLF +
        '"Save Voltages" saves the present solution in a simple CSV format in a file called DSS_SavedVoltages. ' +
        'Used for VDIFF command.' + CRLF + CRLF +
        'Any class can be saved to a file.  If no filename specified, the classname is used.';
    CommandHelp[8] := 'Writes selected results to a text file and brings ' +
        'up the default text editor (see Set Editor=....) with the file for you to browse.' + CRLF + CRLF +
        'See separate help on Show command. ' + CRLF + CRLF +
        'Default is "show voltages LN Seq".  ';
    CommandHelp[9] := 'Perform the solution of the present solution mode. You can set any option ' +
        'that you can set with the Set command (see Set). ' +
        'The Solve command is virtually synonymous with the Set command except that ' +
        'a solution is performed after the options are processed.';
    CommandHelp[10] := 'Enables a circuit element or entire class.  Example:' + CRLF +
        'Enable load.loadxxx' + CRLF +
        'Enable generator.*  (enables all generators)';
    CommandHelp[11] := 'Disables a circuit element or entire class.  Example:' + CRLF +
        'Disable load.loadxxx' + CRLF +
        'Disable generator.*  (Disables all generators)' + CRLF + CRLF +
        'The item remains defined, but is not included in the solution.';
    CommandHelp[12] := 'Plots circuits and results in a variety of manners.  See separate Plot command help.';
    CommandHelp[13] := '{MOnitors | MEters | Faults | Controls | Eventlog | Keeplist |(no argument) } Resets all Monitors, Energymeters, etc. ' +
        'If no argument specified, resets all options listed.';
    CommandHelp[14] := 'Reads the designated file name containing DSS commands ' +
        'and processes them as if they were entered directly into the command line. ' +
        'The file is said to be "compiled." ' +
        'Similar to "redirect" except changes the default directory to the path of the specified file.' + CRLF + CRLF +
        'Syntax:' + CRLF +
        'Compile filename';
    CommandHelp[15] := 'Used to set various DSS solution modes and options.  You may also set the options with the Solve command. ' +
        'See "Options" for help.';
    CommandHelp[16] := 'Display the properties of either a specific DSS object or a complete dump ' +
        'on all variables in the problem (Warning! Could be very large!).' +
        ' Brings up the default text editor with the text file written by this command.' + CRLF +
        ' Syntax: dump [class.obj] [debug]' + CRLF +
        ' Examples:' + CRLF + CRLF +
        ' Dump line.line1 ' + CRLF +
        ' Dump solution  (dumps all solution vars) ' + CRLF +
        ' Dump commands  (dumps all commands to a text file) ' + CRLF +
        ' Dump transformer.*  (dumps all transformers)' + CRLF +
        ' Dump ALLOCationfactors  (load allocation factors)' + CRLF +
        ' Dump Buslist    (bus name hash list)' + CRLF +
        ' Dump Devicelist    (Device name hash list)' + CRLF +
        ' Dump      (dumps all objects in circuit) ';
                         //' Dump debug';   // Debug dump
    CommandHelp[17] := 'Opens the specified terminal and conductor of the specified circuit element. ' +
        'If the conductor is not specified, all phase conductors of the terminal are opened.' + CRLF + CRLF +
        'Examples:' + CRLF +
        'Open line.line1 2 ' + CRLF +
        '(opens all phases of terminal 2)' + CRLF + CRLF +
        'Open line.line1 2 3' + CRLF +
        '(opens the 3rd conductor of terminal 2)';
    CommandHelp[18] := 'Opposite of the Open command.';   // Close a device terminal conductor
    CommandHelp[19] := 'Comment.  Command line is ignored.';       // Comment
    CommandHelp[20] := 'Reads the designated file name containing DSS commands ' +
        'and processes them as if they were entered directly into the command line. ' +
        'Similar to "Compile", but leaves current directory where it was when Redirect command is invoked.' +
        'Can temporarily change to subdirectories if nested Redirect commands require.' + Crlf + crlf +
        'ex:  redirect filename';
    CommandHelp[21] := 'Gives this display.';
    CommandHelp[22] := 'Shuts down DSS unless this is the DLL version.  Then it does nothing;  DLL parent is responsible for shutting down the DLL.';
    CommandHelp[23] := 'Inquiry for property value.  Result is put into GlobalReault and can be seen in the Result Window. ' +
        'Specify the full property name.' + CRLF + CRLF +
        'Example: ? Line.Line1.R1' + CRLF + CRLF +
        'Note you can set this property merely by saying:' + CRLF +
        'Line.line1.r1=.058';   // Property Value inquiry
    CommandHelp[24] := '{Year | Hour | t}  Increments year, hour, or time as specified.  If "t" is ' +
        'specified, then increments time by current step size.';
    CommandHelp[25] := 'Displays main control panel window.';
    CommandHelp[26] := 'Force all monitors and meters to take a sample for the most recent solution. Keep in mind that meters will perform integration.';
    CommandHelp[27] := 'Clear all circuits currently in memory.';
    CommandHelp[28] := 'Display "About Box".  (Result string set to Version string.)';
    CommandHelp[29] := 'Calculates voltagebase for buses based on voltage bases defined ' +
        'with Set voltagebases=... command.';
    CommandHelp[30] := 'Command to explicitly set the base voltage for a bus. ' +
        'Bus must be previously defined. Parameters in order are:' + crlf +
        'Bus = {bus name}' + Crlf +
        'kVLL = (line-to-line base kV)' + crlf +
        'kVLN = (line-to-neutral base kV)' + Crlf + Crlf +
        'kV base is normally given in line-to-line kV (phase-phase). ' +
        'However, it may also be specified by line-to-neutral kV.' + crlf +
        'The following exampes are equivalent:' + crlf + crlf +
        'setkvbase Bus=B9654 kVLL=13.2' + crlf +
        'setkvbase B9654 13.2' + crlf +
        'setkvbase B9654 kvln=7.62';
    CommandHelp[31] := 'Forces rebuild of Y matrix upon next Solve command regardless of need. ' +
        'The usual reason for doing this would be to reset the matrix for another ' +
        'load level when using LoadModel=PowerFlow (the default) when the system is difficult to ' +
        'solve when the load is far from its base value.  Works by invalidating the Y primitive ' +
        'matrices for all the Power Conversion elements.';
    CommandHelp[32] := 'Returns DSS property values set using the Set command. ' +
        'Result is returne in Result property of the Text interface. ' + CRLF + CRLF +
        'VBA Example:' + CRLF + CRLF +
        'DSSText.Command = "Get mode"' + CRLF +
        'Answer = DSSText.Result' + CRLF + CRLF +
        'Multiple properties may be requested on one get.  The results are appended ' +
        'and the individual values separated by commas.' + CRLF + CRLF +
        'See help on Set command for property names.';
    CommandHelp[33] := 'This command forces reinitialization of the solution for the next Solve command. ' +
        'To minimize iterations, most solutions start with the previous solution unless there ' +
        'has been a circuit change.  However, if the previous solution is bad, it may be necessary ' +
        'to re-initialize.  In most cases, a re-initiallization results in a zero-load power flow ' +
        'solution with only the series power delivery elements considered.';
    CommandHelp[34] := 'Export various solution values to CSV (or XML) files for import into other programs. ' +
        'Creates a new file except for Energymeter and Generator objects, for which ' +
        'the results for each device of this class are APPENDED to the CSV File. You may export to ' +
        'a specific file by specifying the file name as the LAST parameter on the line. For example:' + CRLF + CRLF +
        '  Export Voltage Myvoltagefile.CSV' + CRLF + CRLF +
        'Otherwise, the default file names shown in the Export help are used. ' +
        'For Energymeter and Generator, specifying the switch "/multiple" (or /m) for the file name will cause ' +
        'a separate file to be written for each meter or generator. ' +
        'The default is for a single file containing all elements.' + CRLF + CRLF +
        'May be abreviated Export V, Export C, etc.  Default is "V" for voltages.' +
        ' If Set ShowExport=Yes, the output file will be automatically displayed in the default editor. ' +
        'Otherwise, you must open the file separately. The name appears in the Result window.';
    CommandHelp[35] := 'Edit specified file in default text file editor (see Set Editor= option).' + CRLF + CRLF +
        'Fileedit EXP_METERS.CSV (brings up the meters export file)' + CRLF + CRLF +
        '"FileEdit" may be abbreviated to a unique character string.';
    CommandHelp[36] := 'Returns the voltages for the ACTIVE BUS in the Result string. ' +
        'For setting the active Bus, use the Select command or the Set Bus= option. ' +
        'Returned as magnitude and angle quantities, comma separated, one set per conductor of the terminal.';
    CommandHelp[37] := 'Returns the currents for each conductor of ALL terminals of the active circuit element in the Result string. ' +
        '(See Select command.)' +
        'Returned as comma-separated magnitude and angle.';
    CommandHelp[38] := 'Returns the powers (complex) going into each conductors of ALL terminals of the active circuit element in the Result string. ' +
        '(See Select command.)' +
        'Returned as comma-separated kW and kvar.';
    CommandHelp[39] := 'Returns the sequence voltages at all terminals of the active circuit element (see Select command) in Result string.  Returned as comma-separated magnitude only values.' +
        'Order of returned values: 0, 1, 2  (for each terminal).';
    CommandHelp[40] := 'Returns the sequence currents into all terminals of the active circuit element (see Select command) in Result string.  Returned as comma-separated magnitude only values.' +
        'Order of returned values: 0, 1, 2  (for each terminal).';
    CommandHelp[41] := 'Returns the sequence powers into all terminals of the active circuit element (see Select command) in Result string.  Returned as comma-separated kw, kvar pairs.' +
        'Order of returned values: 0, 1, 2  (for each terminal).';
    CommandHelp[42] := 'Returns the total losses for the active circuit element (see Select command) ' +
        'in the Result string in kW, kvar.';
    CommandHelp[43] := 'Returns the losses for the active circuit element (see Select command) ' +
        'for each PHASE in the Result string in comma-separated kW, kvar pairs.';
    CommandHelp[44] := 'Returns the total losses for the active circuit in the Result string in kW, kvar.';
    CommandHelp[45] := 'Estimates the allocation factors for loads that are defined using the XFKVA property. ' +
        'Requires that energymeter objects be defined with the PEAKCURRENT property set. ' +
        'Loads that are not in the zone of an energymeter cannot be allocated.';
    CommandHelp[46] := 'FormEdit [class.object].  Brings up form editor on active DSS object.';
    CommandHelp[47] := 'Totals all EnergyMeter objects in the circuit and reports register totals in the result string.';
    CommandHelp[48] := 'Find the maximum load the active circuit can serve in the PRESENT YEAR. Uses the EnergyMeter objects with the registers ' +
        'set with the SET UEREGS= (..) command for the AutoAdd functions.  Syntax (defaults shown):' + CRLF + CRLF +
        'capacity [start=]0.9 [increment=]0.005' + CRLF + CRLF +
        'Returns the metered kW (load + losses - generation) and per unit load multiplier for the loading level at which something in the system reports an overload or undervoltage. ' +
        'If no violations, then it returns the metered kW for peak load for the year (1.0 multiplier). ' +
        'Aborts and returns 0 if no energymeters.';
    CommandHelp[49] := 'List of intrinsic DSS Classes. Returns comma-separated list in Result variable.';
    CommandHelp[50] := 'List of user-defined DSS Classes. Returns comma-separated list in Result variable.';
    CommandHelp[51] := 'Returns full Zsc matrix for the ACTIVE BUS in comma-separated complex number form.';
    CommandHelp[52] := 'Returns symmetrical component impedances, Z1, Z0 for the ACTIVE BUS in comma-separated R+jX form.';
    CommandHelp[53] := 'Refreshes Zsc matrix for the ACTIVE BUS.';
    CommandHelp[54] := 'Returns full Ysc matrix for the ACTIVE BUS in comma-separated complex number form G + jB.';
    CommandHelp[55] := 'Just like the Voltages command, except the voltages are in per unit if the kVbase at the bus is defined.';
    CommandHelp[56] := 'Returns variable values for active element if PC element. Otherwise, returns null.';
    CommandHelp[57] := 'Returns variable names for active element if PC element. Otherwise, returns null.';
    CommandHelp[58] := 'Define x,y coordinates for buses.  Execute after Solve or MakeBusList command is executed so that bus lists are defined.' +
        'Reads coordinates from a CSV file with records of the form: busname, x, y.' + CRLF + CRLF +
        'Example: BusCoords [file=]xxxx.csv';
    CommandHelp[59] := 'Updates the buslist, if needed, using the currently enabled circuit elements.  (This happens automatically for Solve command.)' +
        ' See ReprocessBuses';
    CommandHelp[60] := 'Attempts to convert present circuit model to a positive sequence equivalent. ' +
        'It is recommended to Save the circuit after this and edit the saved version to correct possible misinterpretations.';
    CommandHelp[61] := '{All | MeterName}  Default is "All".  Reduce the circuit according to reduction options. ' +
        'See "Set ReduceOptions" and "Set Keeplist" options.' +
        'Energymeter objects actually perform the reduction.  "All" causes all meters to reduce their zones.';
    CommandHelp[62] := '{All | MeterName}  Default is "All". Interpolates coordinates for missing bus coordinates in meter zone';
    CommandHelp[63] := 'Alignfile [file=]filename.  Aligns DSS script files in columns for easier reading.';
    CommandHelp[64] := '[class=]{Loadshape | Tshape | Monitor  } [object=]{ALL (Loadshapes only) | objectname}. ' +
        'Send specified object to TOP.  Loadshapes and TShapes must be hourly fixed interval. ';
    CommandHelp[65] := 'Usage: Rotate [angle=]nnn.  Rotate circuit plotting coordinates by specified angle (degrees). ';
    CommandHelp[66] := 'Displays the difference between the present solution and the last on saved using the SAVE VOLTAGES command.';
    CommandHelp[67] := 'Returns a power flow summary of the most recent solution in the global result string.';
    CommandHelp[68] := 'kw=nn how={Proportional* | Uniform |Random | Skip} skip=nn PF=nn file=filename MW=nn What=[Generator*|Load]' + CRLF + CRLF +
        'Creates a DSS script file to distribute Generator or Load objects on the system in the manner specified by "how".' + CRLF +
        'kW = total generation to be distributed (default=1000) ' + CRLF +
        'how= process name as indicated (default=proportional to load)' + CRLF +
        'skip = no. of buses to skip for "How=Skip" (default=1)' + CRLF +
        'PF = power factor for new generators (default=1.0)' + CRLF +
        'file = name of file to save (default=distgenerators.dss or distloads.dss)' + CRLF +
        'MW = alternate way to specify kW (default = 1)' + CRLF +
        'What = what type of device to add, Generator (default) or Load';
    CommandHelp[69] := '[case=]casename [year=]yr [registers=](reg1, reg2,...)  [peak=]y/n  [meter=]metername' + CRLF +
        'Plots demand interval (DI) results from yearly simulation cases.  ' +
        'Plots selected registers from selected meter file (default = DI_Totals.CSV).  ' +
        'Peak defaults to NO.  If YES, only daily peak of specified registers ' +
        'is plotted. Example:' + CRLF + CRLF +
        ' DI_Plot basecase year=5 registers=(9,11) no';
    CommandHelp[70] := '[Case1=]casename [case2=]casename [register=](register number) [meter=]{Totals* | SystemMeter | metername}. ' + CRLF +
        'Compares yearly simulations of two specified cases with respect to the quantity in the designated register ' +
        'from the designated meter file. ' +
        'Defaults: Register=9 meter=Totals.  Example:' + CRLF + CRLF +
        'Comparecases base pvgens 10';
    CommandHelp[71] := '[cases=](case1, case2, ...) [registers=](reg1, reg2, ...)  [meter=]{Totals* | SystemMeter | metername}' +
        'Plots yearly curves for specified cases and registers. ' + CRLF +
        'Default: meter=Totals. Example: ' + CRLF + CRLF +
        'yearlycurves cases=(basecase, pvgens) registers=9';
    CommandHelp[72] := 'Change default directory to specified directory' + CRLF + CRLF +
        'CD dirname';
    CommandHelp[73] := '[What=] one of {Currents* | Voltages | Powers} [element=]full_element_name  (class.name). ' +
        'Shows the selected quantity for selected element on a multiphase line drawing in phasor values.';
    CommandHelp[74] := 'Close all DI files ... useful at end of yearly solution where DI files are left open. ' +
        '(Reset and Set Year=nnn will also close the DI files)';
    CommandHelp[75] := 'Do a DOS command. Sends the command "cmd ... " to Windows. Execute the "cmd /?" command ' +
        'in a DOS window to see the options. To do a DOS command and automatically exit, do ' + CRLF + CRLF +
        'DOScmd /c ...command string ...' + CRLF + CRLF +
        'To keep the DOS window open, use /k switch.';
    CommandHelp[76] := 'Execute state estimator on present circuit given present sensor values.';
    CommandHelp[77] := 'Reconductor a line section. Must be in an EnergyMeter zone. ' + CRLF +
        'Syntax: Reconductor Line1=... Line2=... {LineCode= | Geometry = } EditString="..." NPhases=#' + CRLF +
        'Line1 and Line2 may be given in any order. All lines in the path between the two are redefined ' +
        'with either the LineCode or Geometry (not both). You may also add an optional string the alter any other line properties. ' +
        'The edit string should be enclosed in quotes or parens or brackets.' + CRLF +
        'Nphases is an optional filter on the number of phases in line segments to change.';
    CommandHelp[78] := 'For step control of solution process: Intialize iteration counters, etc. that normally occurs at the ' +
        'start of a snapshot solution process.';
    CommandHelp[79] := 'For step control of solution process: Solves the circuit in present state but does not check for control actions.';
    CommandHelp[80] := 'For step control of solution process: Sample the control elements, which push control action requests onto the control queue.';
    CommandHelp[81] := 'For step control of solution process: Pops control actions off the control queue according to the present control mode rules. ' +
        'Dispatches contol actions to proper control element "DoPendingAction" handlers.';
    CommandHelp[82] := 'For step control of solution process: Show the present control queue contents.';
    CommandHelp[83] := 'For step control of solution process: Invoke direct solution function in DSS. Non-iterative solution of Y matrix and active sources only.';
    CommandHelp[84] := 'For step control of solution process: Invoke iterative power flow solution function of DSS directly.';
    CommandHelp[85] := 'Add a marker to a bus in a circuit plot. Markers must be added before issuing the Plot command. Effect is persistent until circuit is cleared. ' +
        'See also ClearBusMarkers command. Example: ' + CRLF + CRLF +
        'ClearBusMarkers    !...Clears any previous bus markers' + CRLF +
        'AddBusMarker Bus=Mybusname code=5 color=Red size=3' + CRLF + CRLF +
        'You can use any of the standard color names  or RGB numbers. See Help on C1 property in Plot command.';
    CommandHelp[86] := 'Read UUIDs (v4) for class names. Tab or comma-delimited file with full object name and UUID';
    CommandHelp[87] := 'Set load and generator object kv to agree with the bus they are connected to using the bus voltage base and connection type.';
    CommandHelp[88] := 'Convert all Loadshapes presently loaded into either files of single or files of double. ' +
        'Usually files of singles are adequate precision for loadshapes.  Syntax:' + CRLF + CRLF +
        'cvrtloadshapes type=sng  (this is the default)' + crlf +
        'cvrtloadshapes type=dbl' + CRLF + CRLF +
        'A DSS script for loading the loadshapes from the created files is produced and displayed in the default editor. ';
    CommandHelp[89] := 'Global result is set to voltage difference, volts and degrees, (Node1 - Node2) between any two nodes. Syntax:' + CRLF + CRLF +
        '   NodeDiff Node1=MyBus.1 Node2=MyOtherBus.1';
    CommandHelp[90] := 'Generates a script to change the phase designation of all lines downstream from a start in line. Useful for such things as moving a single-phase ' +
        'lateral from one phase to another and keep the phase designation consistent for reporting functions that need it to be ' +
        '(not required for simply solving). ' + CRLF + CRLF +
        'StartLine=... PhaseDesignation="..."  EditString="..." ScriptFileName=... StopAtTransformers=Y/N/T/F' + CRLF + CRLF +
        'Enclose the PhaseDesignation in quotes since it contains periods (dots).' + CRLF +
        'You may add and optional EditString to edit any other line properties.' + CRLF + CRLF +
        'Rephase StartLine=Line.L100  PhaseDesignation=".2"  EditString="phases=1" ScriptFile=Myphasechangefile.DSS  Stop=No';
    CommandHelp[91] := 'Bus=...  X=...  Y=... Set the X, Y coordinates for a single bus. Prerequisite: Bus must exist as a result of a Solve, CalcVoltageBases, or MakeBusList command.';
    CommandHelp[92] := 'Update Storage elements based on present solution and time interval. ';
    CommandHelp[93] := 'Change Bus and circuit element names to generic values to remove identifying names. Generally, ' +
        'you will follow this command immediately by a "Save Circuit Dir=MyDirName" command.';
    CommandHelp[94] := 'Define x,y coordinates for buses using Latitude and Longitude values (decimal numbers).  Similar to BusCoords command. ' +
        'Execute after Solve command or MakeBusList command is executed so that bus lists are defined.' +
        'Reads coordinates from a CSV file with records of the form: busname, Latitude, Longitude.' + CRLF + CRLF +
        'Example: LatLongCoords [file=]xxxx.csv' + CRLF + CRLF +
        'Note: Longitude is mapped to x coordinate and Latitude is mapped to y coordinate.';
    CommandHelp[95] := 'Batch edit objects in the same class. Example: BatchEdit Load..* duty=duty_shape' + CRLF +
        'In place of the object name, supply a PERL regular expression. .* matches all names.' + CRLF +
        'The subsequent parameter string is applied to each object selected.';
    CommandHelp[96] := 'Pst calculation. PstCalc Npts=nnn Voltages=[array] dt=nnn freq=nn lamp=120 or 230.' + CRLF +
        'Set Npts to a big enough value to hold the incoming voltage array. ' + CRLF +
        'dt = time increment in seconds. default is 1' + CRLF +
        'freq = base frequency in Hz 50 or 60. Default is default base frequency' + CRLF +
        'Lamp= 120 for North America; 230 for Europe. Default is 120' + CRLF + CRLF +
        'PSTCalc Npts=1900 V=[file=MyCSVFile.CSV, Col=3, Header=y] dt=1 freq=60 lamp=120';
    CommandHelp[97] := '[name=] MyVariableName  [Index=] IndexofMyVariable ' + CRLF + CRLF +
        'Returns the value of the specified state variable of the active circuit element, if a PCelement. ' +
        'Returns the value as a string in the Result window or the Text.Result interface if using the COM server. ' + CRLF + CRLF +
        'You may specify the variable by name or by its index. You can determine the index using the VarNames command. ' +
        'If any part of the request is invalid, the Result is null.';
    CommandHelp[98] := 'Forces reprocessing of bus definitions whether there has been a change or not. Use for rebuilding meter zone lists ' +
        'when a line length changes, for example or some other event that would not normally trigger an update to the bus list.';
    CommandHelp[99] := 'Clear all bus markers created with the AddBusMarker command.';
    CommandHelp[100] := '[restore=Y/N]Perform reliability calcs: Failure rates and number of interruptions. ' + CRLF + CRLF +
        'Optional parameter:' + CRLF + CRLF + 'If restore=y automatic restoration of unfaulted section is assumed.';
    CommandHelp[101] := 'Define and view script variables.  Variable names begin with "@"' + CRLF + CRLF +
        'Usage:' + CRLF + CRLF +
        'var @varname1=values  @varname2=value2    ...' + CRLF +
        'var @varname1  (shows the value of @varname1)' + CRLF +
        'var            (displays all variabiles and values)' + CRLF + CRLF +
        'Example of using a variable:' + CRLF + CRLF +
        'FileEdit @LastFile';
    CommandHelp[102] := 'Force execution of the end-of-time-step cleanup functions that samples/saves meters and updates selected state variables such as storage level';
    CommandHelp[103] := 'Do Cleanup, sample monitors, and increment time.';
    CommandHelp[104] := '[Circuit element name] (Optional) Returns a list of node numbers for all conductors of all terminals of the active circuit element in the Result window or interface.' +
        'If the optional circuit element name is supplied, the program makes it the active element. Usage:' + CRLF + CRLF +
        'NodeList' + CRLF +
        'NodeList Line.Myline';
    CommandHelp[105] := 'Request to create a TCP/IP socket to communicate data with external modules. This function requires the host address and TCP port to connect.';
    CommandHelp[106] := 'Request to terminate a TCP/IP socket. This function requires the host address and TCP port to disconnect.';
    CommandHelp[107] := '{ElementName=} [KeepLoad=Y*/N] [EditString="..."] ' +
        'Remove (disable) all branches downline from the PDelement named by "ElementName" property. Circuit must have an Energymeter on this branch. ' +
        'If KeepLoad=Y (default) a new Load element is defined and kW, kvar set to ' +
        'present power flow solution for the first element eliminated. ' +
        'The EditString is applied to each new Load element defined. ' + CRLF +
        'If KeepLoad=N, all downline elements are disabled. Examples: ' + CRLF + CRLF +
        'Remove Line.Lin3021' + CRLF +
        'Remove Line.L22 Editstring="Daily=Dailycurve Duty=SolarShape' + CRLF +
        'Remove Line.L333 KeepLoad=No';
    CommandHelp[108] := 'Calculates the incidence matrix of the Active Circuit';
    CommandHelp[109] := 'Calculates the incidence matrix of the Active Circuit. However, in this case the matrix will be calculated by considering its hierarchical order,' +
        'which means that the buses order will be generated considering their distribution from the substation to the last load in a radial hierarchy';
    CommandHelp[110] := 'This function takes the bus levels array and traces all the possible paths considering the longest paths from the substation to the longest branches' +
        ' within the circuit. Then, the new paths are filled with 0 to complement the oroginal levels proposed by the calcincmatrix_o command.';
    CommandHelp[111] := 'Calculate the laplacian matrix using the incidence matrix' + CRLF +
        'previously calculated, this means that before calling this command' + CRLF +
        'the incidence matrix needs to be calculated using calcincmatrix/calcincmatrix_o';

end;

//----------------------------------------------------------------------------
procedure ProcessCommand(DSS: TDSS; const CmdLine: String);
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    ObjName, PropName: String;

begin
    try

        DSS.CmdResult := 0;
        DSS.ErrorNumber := 0;  // Reset Error number
        DSS.GlobalResult := '';

{Load up the parser and process the first parameter only}
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
            14:
            begin
                with DSS.DSSExecutive do
                    if RecorderOn then
                        Write_to_RecorderFile(CRLF + '!*********' + CmdLine);
                DSS.CmdResult := DSS.DSSExecutive.DoRedirect(TRUE);
                Exit;
            end;//'Compile';
            20:
            begin
                with DSS.DSSExecutive do
                    if RecorderOn then
                        Write_to_RecorderFile(CRLF + '!*********' + CmdLine);
                DSS.CmdResult := DSS.DSSExecutive.DoRedirect(FALSE);
                Exit;
            end; //'Redirect';
        else   // Write everything direct to recorder, if ON
            with DSS.DSSExecutive do
                if RecorderOn then
                    Write_to_RecorderFile(CmdLine);
        end;

   // Things that are OK to do before a circuit is defined
        case ParamPointer of

            1:
                DSS.CmdResult := DSS.DSSExecutive.DoNewCmd; // new

            15:
                if not Assigned(DSS.ActiveCircuit) then
                begin
                    DoSetCmd_NoCircuit(DSS); // can only call this if no circuit active
                    Exit;    // We exit with either a good outcome or bad
                end;
            19:
{Do Nothing - comment};

            21:
                DSS.CmdResult := DSS.DSSExecutive.DoHelpCmd;
            22:
                if not IsDLL then
                    ExitControlPanel;  // Quit in Stand alone version
            25:
                ShowControlPanel; // DSSForms
            27:
                DSS.DSSExecutive.DoClearCmd;
            28:
                DSS.DSSExecutive.DoAboutBox;
            35:
                DSS.CmdResult := DSS.DSSExecutive.DoFileEditCmd;
            49:
                DSS.CmdResult := DSS.DSSExecutive.DoClassesCmd;
            50:
                DSS.CmdResult := DSS.DSSExecutive.DoUserClassesCmd;
            63:
                DSS.CmdResult := DSS.DSSExecutive.DoAlignFileCmd;
            69:
                DSS.CmdResult := DSS.DSSExecutive.DoDI_PlotCmd;
            70:
                DSS.CmdResult := DSS.DSSExecutive.DoCompareCasesCmd;
            71:
                DSS.CmdResult := DSS.DSSExecutive.DoYearlyCurvesCmd;
            72:
            begin
                ParamName := DSS.Parser.NextParam;
                Param := DSS.Parser.StrValue;
                if SetCurrentDir(Param) then
                begin
                    DSS.CmdResult := 0;
                    SetDataPath(DSS, Param);  // change datadirectory
                end
                else
                    DoSimpleMsg(DSS, 'Directory "' + Param + '" not found.', 282);
            end;
            75:
                DSS.DSSExecutive.DoADosCmd;
            88:
                DSS.DSSExecutive.DoCvrtLoadshapesCmd;

            101:
                DSS.DSSExecutive.DoVarCmd;

            108: // CalcIncMatrix
            begin
                DSS.ActiveCircuit.Solution.Calc_Inc_Matrix();
            end;
            109: // CalcIncMatrix_O
            begin
                DSS.ActiveCircuit.Solution.Calc_Inc_Matrix_Org();
            end;
            110: // Refine_BusLevels
            begin
                DSS.ActiveCircuit.Get_paths_4_Coverage();
                DSS.GlobalResult := inttostr(length(DSS.ActiveCircuit.Path_Idx) - 1) + ' new paths detected';
            end;
            111: // CalcLaplacian
            begin
                with DSS.ActiveCircuit.Solution do
                begin
                    Laplacian := IncMat.Transpose();          // Transposes the Incidence Matrix
                    Laplacian := Laplacian.multiply(IncMat);  // IncMatT*IncMat
                end;
            end
        else
            if DSS.ActiveCircuit = NIL then
            begin
                DoSimpleMsg(DSS, 'You must create a new circuit object first: "new circuit.mycktname" to execute this command.', 301);
                Exit;
            end;
        end;

   // Now check to see if this is a command or a property reference

        if ParamPointer = 0 then
        begin
     {If not a command or the command is unknown, THEN it could be a property of a circuit element}

       {If a command or no text beFORe the = sign, THEN error}
            if (Length(ParamName) = 0) or (Comparetext(paramName, 'command') = 0) then
            begin
                DoSimpleMsg(DSS, 'Unknown Command: "' + Param + '" ' + CRLF + DSS.Parser.CmdString, 302);
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
                    DSS.ActiveDSSClass.Edit;
                end;
            end;
            Exit;  // Done - don't need to do anything ELSE
        end;

   // Process the rest of the commands

        case ParamPointer of

            2:
                DSS.CmdResult := DSS.DSSExecutive.DoEditCmd; // edit
            3..5:
                DSS.CmdResult := DSS.DSSExecutive.DoMoreCmd; // more , m, ~
            6:
                DSS.CmdResult := DSS.DSSExecutive.DoSelectCmd;
            7:
                DSS.CmdResult := DSS.DSSExecutive.DoSaveCmd; //'save';
            8:
                DSS.CmdResult := DoShowCmd(DSS); //'show';
            9:
                DSS.CmdResult := DoSetCmd(DSS, 1);  // changed from DoSolveCmd; //'solve';
            10:
                DSS.CmdResult := DSS.DSSExecutive.DoEnableCmd;
            11:
                DSS.CmdResult := DSS.DSSExecutive.DoDisableCmd;
       {$IFNDEF FPC}
            12:
                DSS.CmdResult := DoPlotCmd; //'plot';
       {$ELSE}
            12:
            begin
                DSSInfoMessageDlg('Plotting not supported in FPC version');
                DSS.CmdResult := 0;
            end;
       {$ENDIF}
            13:
                DSS.CmdResult := DSS.DSSExecutive.DoResetCmd; //'resetmonitors';
            15:
                DSS.CmdResult := DoSetCmd(DSS, 0);  //'set WITH no solve'
            16:
                DSS.CmdResult := DSS.DSSExecutive.DoPropertyDump;
            17:
                DSS.CmdResult := DSS.DSSExecutive.DoOpenCmd;
            18:
                DSS.CmdResult := DSS.DSSExecutive.DoCloseCmd;


            23:
                DSS.CmdResult := DSS.DSSExecutive.DoQueryCmd;
            24:
                DSS.CmdResult := DSS.DSSExecutive.DoNextCmd;  // Advances time
       {25: ControlPanel.Show -- see above }
            26:
                DSS.CmdResult := DSS.DSSExecutive.DoSampleCmd;
       {27: Begin ClearAllCircuits; DisposeDSSClasses; CreateDSSClasses; End;}
       {28: DoAboutBox; }
            29:
                DSS.CmdResult := DSS.DSSExecutive.DoSetVoltageBases;
            30:
                DSS.CmdResult := DSS.DSSExecutive.DoSetkVBase;
            31:
                DSS.ActiveCircuit.InvalidateAllPCElements;  // FORce rebuilding of Y
            32:
                DSS.CmdResult := DoGetCmd(DSS);
            33:
                DSS.ActiveCircuit.Solution.SolutionInitialized := FALSE;
            34:
                DSS.CmdResult := DoExportCmd(DSS);
       {35: DSS.CmdResult := DoFileEditCmd;}
            36:
                DSS.CmdResult := DSS.DSSExecutive.DovoltagesCmd(FALSE);
            37:
                DSS.CmdResult := DSS.DSSExecutive.DocurrentsCmd;
            38:
                DSS.CmdResult := DSS.DSSExecutive.DopowersCmd;
            39:
                DSS.CmdResult := DSS.DSSExecutive.DoseqvoltagesCmd;
            40:
                DSS.CmdResult := DSS.DSSExecutive.DoseqcurrentsCmd;
            41:
                DSS.CmdResult := DSS.DSSExecutive.DoseqpowersCmd;
            42:
                DSS.CmdResult := DSS.DSSExecutive.DolossesCmd;
            43:
                DSS.CmdResult := DSS.DSSExecutive.DophaselossesCmd;
            44:
                DSS.CmdResult := DSS.DSSExecutive.DocktlossesCmd;
            45:
                DSS.CmdResult := DSS.DSSExecutive.DoAllocateLoadsCmd;
            46:
                DSS.CmdResult := DSS.DSSExecutive.DoFormEditCmd;
            47:
                DSS.CmdResult := DSS.DSSExecutive.DoMeterTotals;
            48:
                DSS.CmdResult := DSS.DSSExecutive.DoCapacityCmd;
//       49: DSS.CmdResult := DoClassesCmd;
//       50: DSS.CmdResult := DoUserClassesCmd;
            51:
                DSS.CmdResult := DSS.DSSExecutive.DoZscCmd(TRUE);
            52:
                DSS.CmdResult := DSS.DSSExecutive.DoZsc10cmd;
            53:
                DSS.CmdResult := DSS.DSSExecutive.DoZscRefresh;
            54:
                DSS.CmdResult := DSS.DSSExecutive.DoZscCmd(FALSE);
            55:
                DSS.CmdResult := DSS.DSSExecutive.DovoltagesCmd(TRUE);
            56:
                DSS.CmdResult := DSS.DSSExecutive.DoVarValuesCmd;
            57:
                DSS.CmdResult := DSS.DSSExecutive.DoVarNamesCmd;
            58:
                DSS.CmdResult := DSS.DSSExecutive.DoBusCoordsCmd(FALSE);
            59:
                with DSS.ActiveCircuit do
                    if BusNameRedefined then
                        ReprocessBusDefs;
            60:
                DSS.CmdResult := DSS.DSSExecutive.DoMakePosSeq;
            61:
                DSS.CmdResult := DSS.DSSExecutive.DoReduceCmd;
            62:
                DSS.CmdResult := DSS.DSSExecutive.DoInterpolateCmd;
            64:
                DSS.CmdResult := DSS.DSSExecutive.DoTOPCmd;
            65:
                DSS.CmdResult := DSS.DSSExecutive.DoRotateCmd;
            66:
                DSS.CmdResult := DSS.DSSExecutive.DoVdiffCmd;
            67:
                DSS.CmdResult := DSS.DSSExecutive.DoSummaryCmd;
            68:
                DSS.CmdResult := DSS.DSSExecutive.DoDistributeCmd;
//      69;
//      70;
//      71;
//      72;
            73:
                DSS.CmdResult := DSS.DSSExecutive.DoVisualizeCmd;
            74:
                DSS.CmdResult := DSS.DSSExecutive.DoCloseDICmd;
            76:
                DSS.CmdResult := DSS.DSSExecutive.DoEstimateCmd;
            77:
                DSS.CmdResult := DSS.DSSExecutive.DoReconductorCmd;
       {Step solution commands}
            78:
                DSS.ActiveCircuit.Solution.SnapShotInit;
            79:
                DSS.ActiveCircuit.Solution.SolveCircuit;
            80:
                DSS.ActiveCircuit.Solution.SampleControlDevices;
            81:
                DSS.ActiveCircuit.Solution.DoControlActions;
            82:
                DSS.ActiveCircuit.ControlQueue.ShowQueue(DSS.OutputDirectory + DSS.CircuitName_ + 'ControlQueue.csv');
            83:
                DSS.ActiveCircuit.Solution.SolveDirect;
            84:
                DSS.ActiveCircuit.Solution.DoPFLOWsolution;
            85:
                DSS.CmdResult := DSS.DSSExecutive.DoAddMarkerCmd;
            86:
                DSS.CmdResult := DSS.DSSExecutive.DoUuidsCmd;
            87:
                DSS.CmdResult := DSS.DSSExecutive.DoSetLoadAndGenKVCmd;
//       88:;
            89:
                DSS.CmdResult := DSS.DSSExecutive.DoNodeDiffCmd;
            90:
                DSS.CmdResult := DSS.DSSExecutive.DoRephaseCmd;
            91:
                DSS.CmdResult := DSS.DSSExecutive.DoSetBusXYCmd;
            92:
                DSS.CmdResult := DSS.DSSExecutive.DoUpDateStorageCmd;
            93:
                Obfuscate(DSS);
            94:
                DSS.CmdResult := DSS.DSSExecutive.DoBusCoordsCmd(TRUE);   // swaps X and Y
            95:
                DSS.CmdResult := DSS.DSSExecutive.DoBatchEditCmd;
            96:
                DSS.CmdResult := DSS.DSSExecutive.DoPstCalc;
            97:
                DSS.CmdResult := DSS.DSSExecutive.DoValVarCmd;
            98:
                DSS.ActiveCircuit.ReprocessBusDefs;
            99:
                DSS.ActiveCircuit.ClearBusMarkers;
            100:
                DSS.CmdResult := DSS.DSSExecutive.DoLambdaCalcs;   // Option: Assume Restoration
            102:
                DSS.ActiveCircuit.Solution.EndofTimeStepCleanup;
            103:
                DSS.ActiveCircuit.Solution.FinishTimeStep;
            104:
                DSS.CmdResult := DSS.DSSExecutive.DoNodeListCmd;
      {$IFNDEF FPC}
            105:
                DSS.CmdResult := DoConnectCmd; //'TCP/IP connect';
            106:
                DSS.CmdResult := DoDisConnectCmd; //'TCP/IP disconnect';
      {$ELSE}
            105:
            begin
                DSSInfoMessageDlg('Winsock TCP/IP connection is not supported in FPC version');
                DSS.CmdResult := 0;
            end;
            106:
            begin
                DSSInfoMessageDlg('Winsock TCP/IP disconnection is not supported in FPC version');
                DSS.CmdResult := 0;
            end;
      {$ENDIF}
            107:
                DSS.DSSExecutive.DoRemoveCmd;
        else
       // Ignore excess parameters
        end;

    except
        On E: Exception do
            DoErrorMsg(DSS, ('ProcessCommand' + CRLF + 'Exception Raised While Processing DSS Command:' + CRLF + DSS.Parser.CmdString),
                E.Message,
                'Error in command string or circuit data.', 303);
    end;

    DSS.ParserVars.Add('@result', DSS.GlobalResult)

end;

procedure DisposeStrings;
var
    i: Integer;

begin
    for i := 1 to NumExecCommands do
    begin
        ExecCommand[i] := '';
        CommandHelp[i] := '';
    end;
end;

initialization

    DefineCommands;

finalization

    DisposeStrings;

end.
