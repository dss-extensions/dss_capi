unit ExecOptions;

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
    NumExecOptions = 115;

var
    ExecOption,
    OptionHelp: array[1..NumExecOptions] of String;
    OptionList: TCommandList;

function DoGetCmd: Integer;
function DoSetCmd(SolveOption: Integer): Integer;
function DoSetCmd_NoCircuit: Boolean;  // Set Commands that do not require a circuit


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
    DSSClass,
    DSSHelper;

procedure DefineOptions;

begin

    ExecOption[1] := 'type';
    ExecOption[2] := 'element';
    ExecOption[3] := 'hour';
    ExecOption[4] := 'sec';
    ExecOption[5] := 'year';
    ExecOption[6] := 'frequency';
    ExecOption[7] := 'stepsize';
    ExecOption[8] := 'mode';
    ExecOption[9] := 'random';
    ExecOption[10] := 'number';
    ExecOption[11] := 'time';
    ExecOption[12] := 'class';
    ExecOption[13] := 'object';
    ExecOption[14] := 'circuit';
    ExecOption[15] := 'editor';
    ExecOption[16] := 'tolerance';
    ExecOption[17] := 'maxiterations';
    ExecOption[18] := 'h';
    ExecOption[19] := 'Loadmodel';
    ExecOption[20] := 'Loadmult';
    ExecOption[21] := 'normvminpu';
    ExecOption[22] := 'normvmaxpu';
    ExecOption[23] := 'emergvminpu';
    ExecOption[24] := 'emergvmaxpu';
    ExecOption[25] := '%mean';
    ExecOption[26] := '%stddev';
    ExecOption[27] := 'LDCurve';  // Load Duration Curve
    ExecOption[28] := '%growth';  // default growth rate
    ExecOption[29] := 'Genkw';
    ExecOption[30] := 'Genpf';
    ExecOption[31] := 'CapkVAR';
    ExecOption[32] := 'Addtype';
    ExecOption[33] := 'Allowduplicates';
    ExecOption[34] := 'Zonelock';
    ExecOption[35] := 'UEweight';
    ExecOption[36] := 'Lossweight';
    ExecOption[37] := 'UEregs';
    ExecOption[38] := 'Lossregs';
    ExecOption[39] := 'Voltagebases';  //  changes the default voltage base rules
    ExecOption[40] := 'Algorithm';  //  changes the default voltage base rules
    ExecOption[41] := 'Trapezoidal';
    ExecOption[42] := 'Autobuslist';  // array of bus names to include in auto add solutions
    ExecOption[43] := 'Controlmode';
    ExecOption[44] := 'Tracecontrol';
    ExecOption[45] := 'Genmult';
    ExecOption[46] := 'Defaultdaily';
    ExecOption[47] := 'Defaultyearly';
    ExecOption[48] := 'Allocationfactors';
    ExecOption[49] := 'Cktmodel';
    ExecOption[50] := 'Pricesignal';
    ExecOption[51] := 'Pricecurve';
    ExecOption[52] := 'Terminal';
    ExecOption[53] := 'Basefrequency';
    ExecOption[54] := 'Harmonics';
    ExecOption[55] := 'Maxcontroliter';
    ExecOption[56] := 'Bus';
    ExecOption[57] := 'Datapath';
    ExecOption[58] := 'KeepList';
    ExecOption[59] := 'ReduceOption';
    ExecOption[60] := 'DemandInterval';
    ExecOption[61] := '%Normal';
    ExecOption[62] := 'DIVerbose';
    ExecOption[63] := 'Casename';
    ExecOption[64] := 'Markercode';
    ExecOption[65] := 'Nodewidth';
    ExecOption[66] := 'Log';
    ExecOption[67] := 'Recorder';
    ExecOption[68] := 'Overloadreport';
    ExecOption[69] := 'Voltexceptionreport';
    ExecOption[70] := 'Cfactors';
    ExecOption[71] := 'Showexport';
    ExecOption[72] := 'Numallociterations';
    ExecOption[73] := 'DefaultBaseFrequency';
    ExecOption[74] := 'Markswitches';
    ExecOption[75] := 'Switchmarkercode';
    ExecOption[76] := 'Daisysize';
    ExecOption[77] := 'Marktransformers';
    ExecOption[78] := 'TransMarkerCode';
    ExecOption[79] := 'TransMarkerSize';
    ExecOption[80] := 'LoadShapeClass';
    ExecOption[81] := 'EarthModel';
    ExecOption[82] := 'QueryLog';
    ExecOption[83] := 'MarkCapacitors';
    ExecOption[84] := 'MarkRegulators';
    ExecOption[85] := 'MarkPVSystems';
    ExecOption[86] := 'MarkStorage';
    ExecOption[87] := 'CapMarkerCode';
    ExecOption[88] := 'RegMarkerCode';
    ExecOption[89] := 'PVMarkerCode';
    ExecOption[90] := 'StoreMarkerCode';
    ExecOption[91] := 'CapMarkerSize';
    ExecOption[92] := 'RegMarkerSize';
    ExecOption[93] := 'PVMarkerSize';
    ExecOption[94] := 'StoreMarkerSize';
    ExecOption[95] := 'NeglectLoadY';
    ExecOption[96] := 'MarkFuses';
    ExecOption[97] := 'FuseMarkerCode';
    ExecOption[98] := 'FuseMarkerSize';
    ExecOption[99] := 'MarkReclosers';
    ExecOption[100] := 'RecloserMarkerCode';
    ExecOption[101] := 'RecloserMarkerSize';
    ExecOption[102] := 'RegistryUpdate';
    ExecOption[103] := 'MarkRelays';
    ExecOption[104] := 'RelayMarkerCode';
    ExecOption[105] := 'RelayMarkerSize';
    ExecOption[106] := 'ProcessTime';
    ExecOption[107] := 'TotalTime';
    ExecOption[108] := 'StepTime';
    ExecOption[109] := 'SampleEnergyMeters';
    ExecOption[110] := 'MinIterations'; // default is 2
    ExecOption[111] := 'DSSVisualizationTool';
    ExecOption[112] := 'KeepLoad';
    ExecOption[113] := 'Zmag';
    ExecOption[114] := 'SeasonRating';
    ExecOption[115] := 'SeasonSignal';


    OptionHelp[1] := 'Sets the active DSS class type.  Same as Class=...';
    OptionHelp[2] := 'Sets the active DSS element by name. You can use ' +
        'the complete object spec (class.name) or just the ' +
        'name.  if full name is specifed, class becomes the active ' +
        'class, also.';
    OptionHelp[3] := 'Sets the hour used for the start time of the solution.';
    OptionHelp[4] := 'Sets the seconds from the hour for the start time of the solution.';
    OptionHelp[5] := 'Sets the Year (integer number) to be used for the solution. ' +
        'for certain solution types, this determines the growth multiplier.';
    OptionHelp[6] := 'Sets the frequency for the solution of the active circuit.';
    OptionHelp[7] := 'Sets the time step size for the active circuit.  Default units are s. ' +
        'May also be specified in minutes or hours by appending "m" or "h" to the value. For example:' + CRLF + CRLF +
        '   stepsize=.25h ' + CRLF + '  stepsize=15m' + CRLF + '  stepsize=900s';
    OptionHelp[8] := 'Set the solution Mode: One of' +
        CRLF + '  Snapshot,' +
        CRLF + '  Daily,' +
        CRLF + '  Yearly (follow Yearly curve),' +
        CRLF + '  DIrect,' +
        CRLF + '  DUtycycle,' +
        CRLF + '  Time, ( see LoadShapeClass, SampleEnergymeters options)' +
        CRLF + '  DYnamic,  ( see LoadShapeClass option)' +
        CRLF + '  Harmonic,' +
        CRLF + '  HarmonicT,  (sequential Harmonic Mode)' +
        CRLF + '  M1 (Monte Carlo 1),' +
        CRLF + '  M2 (Monte Carlo 2),' +
        CRLF + '  M3 (Monte Carlo 3),' +
        CRLF + '  Faultstudy,' +
        CRLF + '  MF (monte carlo fault study)' +
        CRLF + '  Peakday,' +
        CRLF + '  LD1 (load-duration 1)' +
        CRLF + '  LD2 (load-duration 2)' +
        CRLF + '  AutoAdd (see AddType)' + CRLF + CRLF +
        'Side effect: setting the Mode propergy resets all monitors and energy meters. It also ' +
        'resets the time step, etc. to defaults for each mode.  After the initial reset, the user ' +
        'must explicitly reset the monitors and/or meters until another Set Mode= command.';
    OptionHelp[9] := 'One of [Uniform | Gaussian | Lognormal | None ] for Monte Carlo Variables.';
    OptionHelp[10] := 'Number of solutions or time steps to perform for each Solve command. Defaults for selected modes: ' + CRLF + CRLF +
        'Daily = 24' + CRLF + 'Yearly = 8760' + CRLF + 'Duty = 100';
    OptionHelp[11] := 'Specify the solution start time as an array:' + CRLF +
        'time=(hour, secs)';
    OptionHelp[12] := 'Synonym for Type=. (See above)';
    OptionHelp[13] := 'Synonym for Element=. (See above)';
    OptionHelp[14] := 'Set the active circuit by name.';
    OptionHelp[15] := 'Set the command string required to start up the editor preferred by the user. Does not require a circuit defined.';
    OptionHelp[16] := 'Sets the solution tolerance.  Default is 0.0001.';
    OptionHelp[17] := 'Sets the maximum allowable iterations for power flow solutions. Default is 15.';
    OptionHelp[18] := 'Alternate name for time step size.';
    OptionHelp[19] := '{Powerflow | Admittance} depending on the type of solution you wish to perform. ' +
        'If admittance, a non-iterative, direct solution is done with all loads and generators modeled by their ' +
        'equivalent admittance.';
    OptionHelp[20] := 'Global load multiplier for this circuit.  Does not affect loads ' +
        'designated to be "fixed".  All other base kW values are multiplied by this number. ' +
        'Defaults to 1.0 when the circuit is created. As with other values, it always stays ' +
        'at the last value to which it was set until changed again.';
    OptionHelp[21] := 'Minimum permissible per unit voltage for normal conditions. Default is 0.95.';
    OptionHelp[22] := 'Maximum permissible per unit voltage for normal conditions. Default is 1.05.';
    OptionHelp[23] := 'Minimum permissible per unit voltage for emergency (contingency) conditions. Default is 0.90.';
    OptionHelp[24] := 'Maximum permissible per unit voltage for emergency (contingency) conditions. Default is 1.08.';
    OptionHelp[25] := 'Percent mean to use for global load multiplier. Default is 65%.';
    OptionHelp[26] := 'Percent Standard deviation to use for global load multiplier. Default is 9%.';
    OptionHelp[27] := 'Set Load-Duration Curve. Global load multiplier is defined by this curve for LD1 and LD2 solution modes. Default is Nil.';
    OptionHelp[28] := 'Set default annual growth rate, percent, for loads with no growth curve specified. Default is 2.5.';
    OptionHelp[29] := 'Size of generator, kW, to automatically add to system. Default is 1000.0';
    OptionHelp[30] := 'Power factor of generator to assume for automatic addition. Default is 1.0.';
    OptionHelp[31] := 'Size of capacitor, kVAR, to automatically add to system.  Default is 600.0.';
    OptionHelp[32] := '{Generator | Capacitor} Default is Generator. Type of device for AutoAdd Mode.';
    OptionHelp[33] := '{YES/TRUE | NO/FALSE}   Default is No. Flag to indicate if it is OK to have devices of same name in the same class. ' +
        'If No, then a New command is treated as an Edit command. ' +
        'If Yes, then a New command will always result in a device being added.';
    OptionHelp[34] := '{YES/TRUE | NO/FALSE}  Default is No. if No, then meter zones are recomputed each time there is a change in the circuit. ' +
        'If Yes, then meter zones are not recomputed unless they have not yet been computed. ' +
        'Meter zones are normally recomputed on Solve command following a circuit change.';
    OptionHelp[35] := 'Weighting factor for UE/EEN in AutoAdd functions.  Defaults to 1.0.' + CRLF + CRLF +
        'Autoadd mode minimizes' + CRLF + CRLF +
        '(Lossweight * Losses + UEweight * UE). ' + CRLF + CRLF +
        'If you wish to ignore UE, set to 0. ' +
        'This applies only when there are EnergyMeter objects. ' +
        'Otherwise, AutoAdd mode minimizes total system losses.';
    OptionHelp[36] := 'Weighting factor for Losses in AutoAdd functions.  Defaults to 1.0.' + CRLF + CRLF +
        'Autoadd mode minimizes' + CRLF + CRLF +
        '(Lossweight * Losses + UEweight * UE). ' + CRLF + CRLF +
        'If you wish to ignore Losses, set to 0. ' +
        'This applies only when there are EnergyMeter objects. ' +
        'Otherwise, AutoAdd mode minimizes total system losses.';
    OptionHelp[37] := 'Which EnergyMeter register(s) to use for UE in AutoAdd Mode. ' +
        'May be one or more registers.  if more than one, register values are summed together. ' +
        'Array of integer values > 0.  Defaults to 11 (for Load EEN). ' + CRLF + CRLF +
        'for a list of EnergyMeter register numbers, do the "Show Meters" command after defining a circuit.';
    OptionHelp[38] := 'Which EnergyMeter register(s) to use for Losses in AutoAdd Mode. ' +
        'May be one or more registers.  if more than one, register values are summed together. ' +
        'Array of integer values > 0.  Defaults to 13 (for Zone kWh Losses). ' + CRLF + CRLF +
        'for a list of EnergyMeter register numbers, do the "Show Meters" command after defining a circuit.';
    OptionHelp[39] := 'Define legal bus voltage bases for this circuit.  Enter an array ' +
        'of the legal voltage bases, in phase-to-phase voltages, for example:' + CRLF + CRLF +
        'set voltagebases=".208, .480, 12.47, 24.9, 34.5, 115.0, 230.0" ' + CRLF + CRLF +
        'When the CalcVoltageBases command is issued, a snapshot solution is performed ' +
        'with no load injections and the bus base voltage is set to the nearest legal voltage base. ' +
        'The defaults are as shown in the example above.';
    OptionHelp[40] := '{Normal | Newton}  Solution algorithm type.  Normal is a fixed point iteration ' +
        'that is a little quicker than the Newton iteration.  Normal is adequate for most radial ' +
        'distribution circuits.  Newton is more robust for circuits that are difficult to solve.';
    OptionHelp[41] := '{YES/TRUE | NO/FALSE}  Default is "No/False". Specifies whether to use trapezoidal integration for accumulating energy meter registers. ' +
        'Applies to EnergyMeter and Generator objects.  Default method simply multiplies the ' +
        'present value of the registers times the width of the interval (Euler). ' +
        'Trapezoidal is more accurate when there are sharp changes in a load shape or unequal intervals. ' +
        'Trapezoidal is automatically used for ' +
        'some load-duration curve simulations where the interval size varies considerably. ' +
        'Keep in mind that for Trapezoidal, you have to solve one more point than the number of intervals. ' +
        'That is, to do a Daily simulation on a 24-hr load shape, you would set Number=25 to force a solution ' +
        'at the first point again to establish the last (24th) interval.' + CRLF + CRLF +
        'Note: Set Mode= resets Trapezoidal to No/False. Set this to Yes/True AFTER setting the Mode option.';
    OptionHelp[42] := 'Array of bus names to include in AutoAdd searches. Or, you can specify a text file holding the names, one to a line, ' +
        'by using the syntax (file=filename) instead of the actual array elements. ' +
        'Default is null, which results in the program ' +
        'using either the buses in the EnergyMeter object zones or, if no EnergyMeters, all the buses, which can ' +
        'make for lengthy solution times. ' + Crlf + Crlf +
        'Examples:' + Crlf + CRlf +
        'Set autobuslist=(bus1, bus2, bus3, ... )' + CRLF +
        'Set autobuslist=(file=buslist.txt)';
    OptionHelp[43] := '{OFF | STATIC |EVENT | TIME | MULTIRATE}  Default is "STATIC".  Control mode for the solution. ' +
        'Set to OFF to prevent controls from changing.' + CRLF +
        'STATIC = Time does not advance.  Control actions are executed in order of shortest time to act ' +
        'until all actions are cleared from the control queue.  Use this mode for power flow solutions which may require several ' +
        'regulator tap changes per solution.' + CRLF + CRLF +
        'EVENT = solution is event driven.  Only the control actions nearest in time ' +
        'are executed and the time is advanced automatically to the time of the event. ' + crlf + crlf +
        'TIME = solution is time driven.  Control actions are executed when the time for the pending ' +
        'action is reached or surpassed.' + CRLF + CRLF +
        'MULTIRATE = solution is time driven.  Control actions are executed when the time for the pending ' +
        'action is reached or surpassed. In this control mode a solution is performed after each control action' +
        'is performed to reduce the error accumulated when the time step is to long' + CRLF + CRLF +
        'Controls may reset and may choose not to act when it comes their time. ' + CRLF +
        'Use TIME mode when modeling a control externally to the DSS and a solution mode such as ' +
        'DAILY or DUTYCYCLE that advances time, or set the time (hour and sec) explicitly from the external program. ';
    OptionHelp[44] := '{YES/TRUE | NO/FALSE}  Set to YES to trace the actions taken in the control queue.  ' +
        'Creates a file named TRACE_CONTROLQUEUE.CSV in the default directory. ' +
        'The names of all circuit elements taking an action are logged.';
    OptionHelp[45] := 'Global multiplier for the kW output of every generator in the circuit. Default is 1.0. ' +
        'Applies to all but Autoadd solution modes. ' +
        'Ignored for generators designated as Status=Fixed.';
    OptionHelp[46] := 'Default daily load shape name. Default value is "default", which is a 24-hour curve defined when the DSS is started.';
    OptionHelp[47] := 'Default yearly load shape name. Default value is "default", which is a 24-hour curve defined when the DSS is started.';
    OptionHelp[48] := 'Sets the connected kVA allocation factors for all loads in the active circuit to the value given.';
    OptionHelp[49] := '{Multiphase | Positive}  Default = Multiphase.  Designates whether circuit model is to interpreted as a normal multi-phase ' +
        'model or a positive-sequence only model';
    OptionHelp[50] := 'Sets the present price signal ($/MWh) for the circuit.  Default value is 25.';
    OptionHelp[51] := 'Sets the PRICESHAPE object to use to obtain for price signal. Default is none (null string). If none, ' +
        'price signal either remains constant or is set by an external process using Set Price= option. ' +
        'Curve is defined as a PRICESHAPE  in actual values (not normalized) and should be defined to correspond to ' +
        'the type of analysis being performed (daily, yearly, etc.).';
    OptionHelp[52] := 'Set the active terminal of the active circuit element. May also be done with Select command.';
    OptionHelp[53] := 'Default = 60. Set the fundamental frequency for harmonic solution and the default base frequency for all impedance quantities. ' +
        'Side effect: also changes the value of the solution frequency. Saved as default for next circuit.';
    OptionHelp[54] := '{ALL | (list of harmonics) }  Default = ALL. Array of harmonics for which to perform a solution in Harmonics mode. ' +
        'If ALL, then solution is performed for all harmonics defined in spectra currently being used. ' +
        'Otherwise, specify a more limited list such as: ' + CRLF + CRLF +
        '   Set Harmonics=(1 5 7 11 13)';
    OptionHelp[55] := 'Max control iterations per solution.  Default is 10.';
    OptionHelp[56] := 'Set Active Bus by name.  Can also be done with Select and SetkVBase commands and the "Set Terminal="  option. ' +
        'The bus connected to the active terminal becomes the active bus. See Zsc and Zsc012 commands.';
    OptionHelp[57] := 'Set the data path for files written or read by the DSS.' + CRLF +
        'Defaults to the user documents folder.' + CRLF +
        'If the DataPath is not writable, output files will be written to the user application data folder.' + CRLF +
        'May be Null.  Executes a CHDIR to this path if non-null.' + CRLF +
        'Does not require a circuit defined.';
    OptionHelp[58] := 'Array of bus names to keep when performing circuit reductions. You can specify a text file holding the names, one to a line, ' +
        'by using the syntax (file=filename) instead of the actual array elements. ' +
        'Command is cumulative (reset keeplist first). ' +
        'Reduction algorithm may keep other buses automatically. ' + Crlf + Crlf +
        'Examples:' + Crlf + CRlf +
        'Reset Keeplist (sets all buses to FALSE (no keep))' + CRLF +
        'Set KeepList=(bus1, bus2, bus3, ... )' + CRLF +
        'Set KeepList=(file=buslist.txt)';
    OptionHelp[59] := '{ Default or [null] | Shortlines [Zmag=nnn] | MergeParallel | BreakLoops | Switches | Ends | Laterals}  Strategy for reducing feeders. ' +
        'Default is to eliminate all dangling end buses and buses without load, caps, or taps. ' + CRLF +
        '"Shortlines [Zmag=0.02]" merges short branches with impedance less than Zmag (default = 0.02 ohms) ' + CRLF +
        '"MergeParallel" merges lines that have been found to be in parallel ' + CRLF +
        '"Breakloops" disables one of the lines at the head of a loop. ' + CRLF +
        '"Ends" eliminates dangling ends only.' + CRLF +
        '"Switches" merges switches with downline lines and eliminates dangling switches.' + CRLF +
        '"Laterals [Keepload=Yes*/No]" uses the Remove command to eliminate 1-phase laterals and optionally lump the load back to the 2- or 3-phase feeder (default behavior). ' + CRLF + CRLF +
        'Marking buses with "Keeplist" will prevent their elimination.';
    OptionHelp[60] := '{YES/TRUE | NO/FALSE} Default = no. Set for keeping demand interval data for daily, yearly, etc, simulations. ' +
        'Side Effect:  Resets all meters!!!';
    OptionHelp[61] := 'Sets the Normal rating of all lines to a specified percent of the emergency rating.  Note: This action takes place immediately. ' +
        'Only the in-memory value is changed for the duration of the run.';
    OptionHelp[62] := '{YES/TRUE | NO/FALSE} Default = FALSE.  Set to Yes/True if you wish a separate demand interval (DI) file written ' +
        'for each meter.  Otherwise, only the totalizing meters are written.';
    OptionHelp[63] := 'Name of case for yearly simulations with demand interval data. ' +
        'Becomes the name of the subdirectory under which all the year data are stored. ' +
        'Default = circuit name ' + CRLF + CRLF +
        'Side Effect: Sets the prefix for output files';
    OptionHelp[64] := 'Number code for node marker on circuit plots. Number from 0 to 47. Default is 16 (open circle). 24 is solid circle. Try other values for other symbols. See also Nodewidth';
    OptionHelp[65] := 'Width of node marker. Default=1. See MarkerCode';
    OptionHelp[66] := '{YES/TRUE | NO/FALSE} Default = FALSE.  Significant solution events are added to the Event Log, primarily for debugging.';
    OptionHelp[67] := '{YES/TRUE | NO/FALSE} Default = FALSE. Opens DSSRecorder.DSS in DSS install folder and enables recording of all commands that come through ' +
        'the text command interface. Closed by either setting to NO/FALSE or exiting the program. ' +
        'When closed by this command, the file name can be found in the Result. Does not require a circuit defined.';
    OptionHelp[68] := '{YES/TRUE | NO/FALSE} Default = FALSE. For yearly solution mode, sets overload reporting on/off. DemandInterval must be set to true for this to have effect.';
    OptionHelp[69] := '{YES/TRUE | NO/FALSE} Default = FALSE. For yearly solution mode, sets voltage exception reporting on/off. DemandInterval must be set to true for this to have effect.';
    OptionHelp[70] := 'Sets the CFactors for for all loads in the active circuit to the value given.';
    OptionHelp[71] := '{YES/TRUE | NO/FALSE} Default = FALSE. If YES/TRUE will automatically show the results of an Export Command after it is written.';
    OptionHelp[72] := 'Default is 2. Maximum number of iterations for load allocations for each time the AllocateLoads or Estimate command is given.';
    OptionHelp[73] := 'Set Default Base Frequency, Hz. Side effect: Sets solution Frequency and default Circuit Base Frequency. This value is saved when the DSS closes down.';
    OptionHelp[74] := '{YES/TRUE | NO/FALSE}  Default is NO. Mark lines that are switches or are isolated with a symbol. See SwitchMarkerCode.';
    OptionHelp[75] := 'Numeric marker code for lines with switches or are isolated from the circuit. Default is 4. See markswitches option.';
    OptionHelp[76] := 'Default is 1.0. Relative size (a multiplier applied to default size) of daisy circles on daisy plot.';
    OptionHelp[77] := '{YES/TRUE | NO/FALSE}  Default is NO. Mark transformer locations with a symbol. See TransMarkerCode. ' +
        'The coordinate of one of the buses for winding 1 or 2 must be defined for the symbol to show';
    OptionHelp[78] := 'Numeric marker code (0..47 see Users Manual) for transformers. Default is 35. See markstransformers option.';
    OptionHelp[79] := 'Size of transformer marker. Default is 1.';
    OptionHelp[80] := '={Daily | Yearly | Duty | None*} Default loadshape class to use for mode=time and mode=dynamic simulations. Loads and generators, etc., will follow ' +
        'this shape as time is advanced. Default value is None. That is, Load will not vary with time.';
    OptionHelp[81] := 'One of {Carson | FullCarson | Deri*}.  Default is Deri, which is' +
        'a  fit to the Full Carson that works well into high frequencies. ' +
        '"Carson" is the simplified Carson method that is typically used for 50/60 Hz power flow programs. ' +
        'Applies only to Line objects that use LineGeometry objects to compute impedances.';
    OptionHelp[82] := '{YES/TRUE | NO/FALSE} Default = FALSE. When set to TRUE/YES, clears the query log file and thereafter appends ' +
        'the time-stamped Result string contents to the log file after a query command, ?. ';
    OptionHelp[83] := '{YES/TRUE | NO/FALSE}  Default is NO. Mark Capacitor locations with a symbol. See CapMarkerCode. ';
    OptionHelp[84] := '{YES/TRUE | NO/FALSE}  Default is NO. Mark Regulator locations with a symbol. See RegMarkerCode. ';
    OptionHelp[85] := '{YES/TRUE | NO/FALSE}  Default is NO. Mark PVSystem locations with a symbol. See PVMarkerCode and PVMarkerSize. ';
    OptionHelp[86] := '{YES/TRUE | NO/FALSE}  Default is NO. Mark Storage locations with a symbol. See StoreMarkerCode and StoreMarkerSize. ';
    OptionHelp[87] := 'Numeric marker code (0..47 -- see Users Manual) for Capacitors. Default is 38.';
    OptionHelp[88] := 'Numeric marker code (0..47 see Users Manual) for Regulators. Default is 17. (red)';
    OptionHelp[89] := 'Numeric marker code (0..47 see Users Manual) for PVSystems. Default is 15.';
    OptionHelp[90] := 'Numeric marker code (0..47 see Users Manual) for Storage elements. Default is 9.';
    OptionHelp[91] := 'Size of Capacitor marker. Default is 3.';
    OptionHelp[92] := 'Size of Regulator marker. Default is 5.';
    OptionHelp[93] := 'Size of PVsystem marker. Default is 1.';
    OptionHelp[94] := 'Size of Storage marker. Default is 1.';
    OptionHelp[95] := '{YES/TRUE | NO/FALSE}  Default is NO. For Harmonic solution, neglect the Load shunt admittance branch that can siphon off some of the Load injection current. ' + CRLF + CRLF +
        'If YES, the current injected from the LOAD at harmonic frequencies will be nearly ideal.';
    OptionHelp[96] := '{YES/TRUE | NO/FALSE}  Default is NO. Mark Fuse locations with a symbol. See FuseMarkerCode and FuseMarkerSize. ';
    OptionHelp[97] := 'Numeric marker code (0..47 see Users Manual) for Fuse elements. Default is 25.';
    OptionHelp[98] := 'Size of Fuse marker. Default is 1.';
    OptionHelp[99] := '{YES/TRUE | NO/FALSE}  Default is NO. Mark Recloser locations with a symbol. See RecloserMarkerCode and RecloserMarkerSize. ';
    OptionHelp[100] := 'Numeric marker code (0..47 see Users Manual) for Recloser elements. Default is 17. (color=Lime)';
    OptionHelp[101] := 'Size of Recloser marker. Default is 5.';
    OptionHelp[102] := '{YES/TRUE | NO/FALSE}  Default is Yes. Update Windows Registry values upon exiting.  You might want to turn this off if you temporarily ' +
        'change fonts or DefaultBaseFrequency, for example. ';
    OptionHelp[103] := '{YES/TRUE | NO/FALSE}  Default is NO. Mark Relay locations with a symbol. See RelayMarkerCode and RelayMarkerSize. ';
    OptionHelp[104] := 'Numeric marker code (0..47 see Users Manual) for Relay elements. Default is 17. (Color=Lime)';
    OptionHelp[105] := 'Size of Relay marker. Default is 5.';
    OptionHelp[106] := 'The time in microseconds to execute the solve process in the most recent time step or solution (read only)';
    OptionHelp[107] := 'The accumulated time in microseconds to solve the circuit since the last reset. Set this value to reset the accumulator.';
    OptionHelp[108] := 'Process time + meter sampling time in microseconds for most recent time step - (read only)';
    OptionHelp[109] := '{YES/TRUE | NO/FALSE} Overrides default value for sampling EnergyMeter objects at the end of the solution loop. ' +
        'Normally Time and Duty modes do not automatically sample EnergyMeters whereas Daily, Yearly, M1, M2, M3, LD1 and LD2 modes do. ' +
        'Use this Option to turn sampling on or off';
    OptionHelp[110] := 'Minimum number of iterations required for a solution. Default is 2.';
    OptionHelp[111] := 'Activates/Deactivates the extended version of the plot command for figures with the DSS Visualization Tool.';
    OptionHelp[112] := 'Keeploads = Y/N option for ReduceOption Laterals option';
    OptionHelp[113] := 'Sets the Zmag option (in Ohms) for ReduceOption Shortlines option. Lines have less line mode impedance are reduced.';
    OptionHelp[114] := 'Enables/disables the seasonal selection of the rating for determining if an element is overloaded. When enabled, the energy meter will' + CRLF +
        'look for the rating (NormAmps) using the SeasonSignal to eavluate if the element is overloaded';
    OptionHelp[115] := 'Is the name of the XY curve defining the seasonal change when performing QSTS simulations.';
end;
//----------------------------------------------------------------------------
function DoSetCmd_NoCircuit: Boolean;  // Set Commands that do not require a circuit
//----------------------------------------------------------------------------

// This is for setting global options that do not require an active circuit

var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

begin

    Result := TRUE;
     // Continue parsing command line
    ParamPointer := 0;
    ParamName := DSSPrime.Parser.NextParam;
    Param := DSSPrime.Parser.StrValue;
    while Length(Param) > 0 do
    begin
        if Length(ParamName) = 0 then
            Inc(ParamPointer)
        else
            ParamPointer := OptionList.GetCommand(ParamName);

        case ParamPointer of
            0:
                DoSimpleMsg(DSSPrime, 'Unknown parameter "' + ParamName + '" for Set Command ', 130);
            15:
                DefaultEditor := Param;     // 'Editor='
            57:
                SetDataPath(DSSPrime, Param);  // Set a legal data path
            67:
                DSSPrime.DSSExecutive.RecorderOn := InterpretYesNo(Param);
            73:
                DSSPrime.DefaultBaseFreq := DSSPrime.Parser.DblValue;
            102:
                UpdateRegistry := InterpretYesNo(Param);
            111:
            begin
                DSS_Viz_enable := InterpretYesNo(Param);
            end;
        else
        begin
            DoSimpleMsg(DSSPrime, 'You must create a new circuit object first: "new circuit.mycktname" to execute this Set command.', 301);
            Result := FALSE;  // Indicate that we could not process all set command
            Exit;
        end;
        end;

        ParamName := DSSPrime.Parser.NextParam;
        Param := DSSPrime.Parser.StrValue;
    end; {WHILE}

end;

//----------------------------------------------------------------------------
function DoSetCmd(SolveOption: Integer): Integer;
//----------------------------------------------------------------------------

// Set DSS Options
// Solve Command is re-routed here first to set options beFORe solving

var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    TestLoadShapeObj: TLoadShapeObj;


begin

    Result := 0;
     // Continue parsing command line
    ParamPointer := 0;
    ParamName := DSSPrime.Parser.NextParam;
    Param := DSSPrime.Parser.StrValue;
    while Length(Param) > 0 do
    begin
        if Length(ParamName) = 0 then
            Inc(ParamPointer)
        else
            ParamPointer := OptionList.GetCommand(ParamName);

        case ParamPointer of
            0:
                DoSimpleMsg(DSSPrime, 'Unknown parameter "' + ParamName + '" for Set Command ', 130);
            1, 12:
                SetObjectClass(DSSPrime, Param);
            2, 13:
                SetObject(DSSPrime, Param);
            3:
                DSSPrime.ActiveCircuit.solution.DynaVars.intHour := DSSPrime.Parser.IntValue;
            4:
                DSSPrime.ActiveCircuit.solution.DynaVars.t := DSSPrime.Parser.DblValue;
            5:
                with DSSPrime.ActiveCircuit do
                begin
                    Solution.Year := DSSPrime.Parser.IntValue;
                    DefaultGrowthFactor := IntPower(DefaultGrowthRate, (Solution.Year - 1));
                end;
            6:
                DSSPrime.ActiveCircuit.solution.Frequency := DSSPrime.Parser.DblValue;
            7, 18:
                DSSPrime.ActiveCircuit.solution.DynaVars.h := InterpretTimeStepSize(Param);
            8:
                DSSPrime.ActiveCircuit.solution.Mode := InterpretSolveMode(Param);  // see DSSGlobals
            9:
                DSSPrime.ActiveCircuit.solution.RandomType := InterpretRandom(Param);
            10:
                DSSPrime.ActiveCircuit.solution.NumberOfTimes := DSSPrime.Parser.IntValue;
            11:
                DSSPrime.DSSExecutive.Set_Time;
            14:
                DSSPrime.DSSExecutive.SetActiveCircuit(Param);
            15:
                DefaultEditor := Param;     // 'Editor='
            16:
                DSSPrime.ActiveCircuit.solution.ConvergenceTolerance := DSSPrime.Parser.DblValue;
            17:
                DSSPrime.ActiveCircuit.solution.MaxIterations := DSSPrime.Parser.IntValue;
            19:
                with DSSPrime.ActiveCircuit.solution do
                begin
                    DefaultLoadModel := InterpretLoadModel(Param); // for reverting to last on specified
                    LoadModel := DefaultLoadModel;
                end;
            20:
                DSSPrime.ActiveCircuit.LoadMultiplier := DSSPrime.Parser.DblValue;  // Set using LoadMultiplier property
            21:
                DSSPrime.ActiveCircuit.NormalMinVolts := DSSPrime.Parser.DblValue;
            22:
                DSSPrime.ActiveCircuit.NormalMaxVolts := DSSPrime.Parser.DblValue;
            23:
                DSSPrime.ActiveCircuit.EmergMinVolts := DSSPrime.Parser.DblValue;
            24:
                DSSPrime.ActiveCircuit.EmergMaxVolts := DSSPrime.Parser.DblValue;
            25:
                DSSPrime.ActiveCircuit.DefaultDailyShapeObj.Mean := DSSPrime.Parser.DblValue / 100.0;
            26:
                DSSPrime.ActiveCircuit.DefaultDailyShapeObj.StdDev := DSSPrime.Parser.DblValue / 100.0;
            27:
                with DSSPrime.ActiveCircuit do
                begin
                    LoadDurCurve := Param;
                    LoadDurCurveObj := DSSPrime.LoadShapeClass.Find(Param);
                    if LoadDurCurveObj = NIL then
                        DoSimpleMsg(DSSPrime, 'Load-Duration Curve not found.', 131);
                end;
            28:
                with DSSPrime.ActiveCircuit do
                begin
                    DefaultGrowthRate := 1.0 + DSSPrime.Parser.DblValue / 100.0;
                    DefaultGrowthFactor := IntPower(DefaultGrowthRate, (Solution.Year - 1));
                end;
            29:
                DSSPrime.ActiveCircuit.AutoAddObj.GenkW := DSSPrime.Parser.DblValue;
            30:
                DSSPrime.ActiveCircuit.AutoAddObj.GenPF := DSSPrime.Parser.DblValue;
            31:
                DSSPrime.ActiveCircuit.AutoAddObj.CapkVAR := DSSPrime.Parser.DblValue;
            32:
                DSSPrime.ActiveCircuit.AutoAddObj.AddType := InterpretAddType(Param);
            33:
                DSSPrime.ActiveCircuit.DuplicatesAllowed := InterpretYesNo(Param);
            34:
                DSSPrime.ActiveCircuit.ZonesLocked := InterpretYesNo(Param);
            35:
                DSSPrime.ActiveCircuit.UEWeight := DSSPrime.Parser.DblValue;
            36:
                DSSPrime.ActiveCircuit.LossWeight := DSSPrime.Parser.DblValue;
            37:
                ParseIntArray(DSSPrime.ActiveCircuit.UERegs, DSSPrime.ActiveCircuit.NumUEregs, Param);
            38:
                ParseIntArray(DSSPrime.ActiveCircuit.LossRegs, DSSPrime.ActiveCircuit.NumLossregs, Param);
            39:
                DSSPrime.DSSExecutive.DoLegalVoltageBases;
            40:
                DSSPrime.ActiveCircuit.Solution.Algorithm := InterpretSolveAlg(Param);
            41:
                DSSPrime.ActiveCircuit.TrapezoidalIntegration := InterpretYesNo(Param);
            42:
                DSSPrime.DSSExecutive.DoAutoAddBusList(Param);
            43:
                with DSSPrime.ActiveCircuit.Solution do
                begin
                    ControlMode := InterpretControlMode(Param);
                    DefaultControlMode := ControlMode;  // always revert to last one specified in a script
                end;
            44:
                DSSPrime.ActiveCircuit.ControlQueue.TraceLog := InterpretYesNo(Param);
            45:
                DSSPrime.ActiveCircuit.GenMultiplier := DSSPrime.Parser.DblValue;
            46:
            begin
                TestLoadShapeObj := DSSPrime.LoadShapeClass.Find(Param);
                if TestLoadShapeObj <> NIL then
                    DSSPrime.ActiveCircuit.DefaultDailyShapeObj := TestLoadShapeObj;
            end;
            47:
            begin
                TestLoadShapeObj := DSSPrime.LoadShapeClass.Find(Param);
                if TestLoadShapeObj <> NIL then
                    DSSPrime.ActiveCircuit.DefaultYearlyShapeObj := TestLoadShapeObj;
            end;
            48:
                DSSPrime.DSSExecutive.DoSetAllocationFactors(DSSPrime.Parser.DblValue);
            49:
                DSSPrime.ActiveCircuit.PositiveSequence := InterpretCktModel(Param);
            50:
                DSSPrime.ActiveCircuit.PriceSignal := DSSPrime.Parser.DblValue;
            51:
                with DSSPrime.ActiveCircuit do
                begin
                    PriceCurve := Param;
                    PriceCurveObj := DSSPrime.PriceShapeClass.Find(Param);
                    if PriceCurveObj = NIL then
                        DoSimpleMsg(DSSPrime, 'Priceshape.' + param + ' not found.', 132);
                end;
            52:
                with DSSPrime.ActiveCircuit do
                    if ActiveCktElement <> NIL then
                        with ActiveCktElement do
                        begin
                            ActiveTerminalIdx := DSSPrime.Parser.IntValue;
                            SetActiveBus(DSSPrime, StripExtension(Getbus(ActiveTerminalIdx)));   // bus connected to terminal
                        end;
            53:
            begin
                DSSPrime.ActiveCircuit.Fundamental := DSSPrime.Parser.DblValue;     // Set Base Frequency for system (used henceforth)
                DSSPrime.ActiveCircuit.Solution.Frequency := DSSPrime.Parser.DblValue;
            end;
            54:
                DSSPrime.DSSExecutive.DoHarmonicsList(Param);
            55:
                DSSPrime.ActiveCircuit.Solution.MaxControlIterations := DSSPrime.Parser.IntValue;
            56:
                Result := SetActiveBus(DSSPrime, Param);   // See DSSGlobals
            57:
                SetDataPath(DSSPrime, Param);  // Set a legal data path
            58:
                DSSPrime.DSSExecutive.DoKeeperBusList(Param);
            59:
                DSSPrime.DSSExecutive.DoSetReduceStrategy(param);
            60:
                DSSPrime.EnergyMeterClass.SaveDemandInterval := InterpretYesNo(Param);
            61:
            begin
                DSSPrime.ActiveCircuit.PctNormalFactor := DSSPrime.Parser.DblValue;
                DSSPrime.DSSExecutive.DoSetNormal(DSSPrime.ActiveCircuit.PctNormalFactor);
            end;
            62:
                DSSPrime.EnergyMeterClass.DI_Verbose := InterpretYesNo(Param);
            63:
                DSSPrime.ActiveCircuit.CaseName := DSSPrime.Parser.StrValue;
            64:
                DSSPrime.ActiveCircuit.NodeMarkerCode := DSSPrime.Parser.IntValue;
            65:
                DSSPrime.ActiveCircuit.NodeMarkerWidth := DSSPrime.Parser.IntValue;
            66:
                DSSPrime.ActiveCircuit.LogEvents := InterpretYesNo(Param);
            67:
                DSSPrime.DSSExecutive.RecorderOn := InterpretYesNo(Param);
            68:
                DSSPrime.EnergyMeterClass.Do_OverloadReport := InterpretYesNo(Param);
            69:
                DSSPrime.EnergyMeterClass.Do_VoltageExceptionReport := InterpretYesNo(Param);
            70:
                DSSPrime.DSSExecutive.DoSetCFactors(DSSPrime.Parser.DblValue);
            71:
                DSSPrime.AutoShowExport := InterpretYesNo(Param);
            72:
                DSSPrime.MaxAllocationIterations := DSSPrime.Parser.IntValue;
            73:
            begin
                DSSPrime.DefaultBaseFreq := DSSPrime.Parser.DblValue;
                DSSPrime.ActiveCircuit.Fundamental := DSSPrime.Parser.DblValue;     // Set Base Frequency for system (used henceforth)
                DSSPrime.ActiveCircuit.Solution.Frequency := DSSPrime.Parser.DblValue;
            end;
            74:
                DSSPrime.ActiveCircuit.MarkSwitches := InterpretYesNo(Param);
            75:
                DSSPrime.ActiveCircuit.SwitchMarkerCode := DSSPrime.Parser.IntValue;
            76:
                DSSPrime.DaisySize := DSSPrime.Parser.DblValue;
            77:
                DSSPrime.ActiveCircuit.MarkTransformers := InterpretYesNo(Param);
            78:
                DSSPrime.ActiveCircuit.TransMarkerCode := DSSPrime.Parser.IntValue;
            79:
                DSSPrime.ActiveCircuit.TransMarkerSize := DSSPrime.Parser.IntValue;
            80:
                DSSPrime.ActiveCircuit.ActiveLoadShapeClass := InterpretLoadShapeClass(Param);
            81:
                DSSPrime.DefaultEarthModel := InterpretEarthModel(Param);
            82:
            begin
                DSSPrime.LogQueries := InterpretYesNo(Param);
                if DSSPrime.LogQueries then
                    ResetQueryLogFile(DSSPrime);
            end;
            83:
                DSSPrime.ActiveCircuit.MarkCapacitors := InterpretYesNo(Param);
            84:
                DSSPrime.ActiveCircuit.MarkRegulators := InterpretYesNo(Param);
            85:
                DSSPrime.ActiveCircuit.MarkPVSystems := InterpretYesNo(Param);
            86:
                DSSPrime.ActiveCircuit.MarkStorage := InterpretYesNo(Param);
            87:
                DSSPrime.ActiveCircuit.CapMarkerCode := DSSPrime.Parser.IntValue;
            88:
                DSSPrime.ActiveCircuit.RegMarkerCode := DSSPrime.Parser.IntValue;
            89:
                DSSPrime.ActiveCircuit.PVMarkerCode := DSSPrime.Parser.IntValue;
            90:
                DSSPrime.ActiveCircuit.StoreMarkerCode := DSSPrime.Parser.IntValue;
            91:
                DSSPrime.ActiveCircuit.CapMarkerSize := DSSPrime.Parser.IntValue;
            92:
                DSSPrime.ActiveCircuit.RegMarkerSize := DSSPrime.Parser.IntValue;
            93:
                DSSPrime.ActiveCircuit.PVMarkerSize := DSSPrime.Parser.IntValue;
            94:
                DSSPrime.ActiveCircuit.StoreMarkerSize := DSSPrime.Parser.IntValue;
            95:
                DSSPrime.ActiveCircuit.NeglectLoadY := InterpretYesNo(Param);
            96:
                DSSPrime.ActiveCircuit.MarkFuses := InterpretYesNo(Param);
            97:
                DSSPrime.ActiveCircuit.FuseMarkerCode := DSSPrime.Parser.IntValue;
            98:
                DSSPrime.ActiveCircuit.FuseMarkerSize := DSSPrime.Parser.IntValue;
            99:
                DSSPrime.ActiveCircuit.MarkReclosers := InterpretYesNo(Param);
            100:
                DSSPrime.ActiveCircuit.RecloserMarkerCode := DSSPrime.Parser.IntValue;
            101:
                DSSPrime.ActiveCircuit.RecloserMarkerSize := DSSPrime.Parser.IntValue;
            102:
                UpdateRegistry := InterpretYesNo(Param);
            103:
                DSSPrime.ActiveCircuit.MarkRelays := InterpretYesNo(Param);
            104:
                DSSPrime.ActiveCircuit.RelayMarkerCode := DSSPrime.Parser.IntValue;
            105:
                DSSPrime.ActiveCircuit.RelayMarkerSize := DSSPrime.Parser.IntValue;
            107:
                DSSPrime.ActiveCircuit.Solution.Total_Time := DSSPrime.Parser.DblValue;
            109:
                DSSPrime.ActiveCircuit.Solution.SampleTheMeters := InterpretYesNo(Param);
            110:
                DSSPrime.ActiveCircuit.solution.MinIterations := DSSPrime.Parser.IntValue;
            111:
                DSS_Viz_enable := InterpretYesNo(Param);
            112:
                DSSPrime.ActiveCircuit.ReduceLateralsKeepLoad := InterpretYesNo(Param);
            113:
                DSSPrime.ActiveCircuit.ReductionZmag := DSSPrime.Parser.DblValue;
            114:
                DSSPrime.SeasonalRating := InterpretYesNo(Param);
            115:
                DSSPrime.SeasonSignal := Param;

        else
           // Ignore excess parameters
        end;

        case ParamPointer of
            3, 4:
                DSSPrime.ActiveCircuit.Solution.Update_dblHour;
              // Update IntervalHrs for devices that integrate
            7, 18:
                DSSPrime.ActiveCircuit.Solution.IntervalHrs := DSSPrime.ActiveCircuit.Solution.DynaVars.h / 3600.0;
        end;

        ParamName := DSSPrime.Parser.NextParam;
        Param := DSSPrime.Parser.StrValue;
    end; {WHILE}

    if SolveOption = 1 then
        DSSPrime.DSSExecutive.DoSolveCmd;

end;


//----------------------------------------------------------------------------
function DoGetCmd: Integer;

// Get DSS Options Reguest and put it in Global Result string
// may be retrieved by Result property of the DSSText interface

var
    ParamPointer, i: Integer;
    ParamName: String;
    Param: String;

begin

    Result := 0;
    try

        DSSPrime.GlobalResult := '';  //initialize for appending

     // Continue parsing command line
        ParamName := DSSPrime.Parser.NextParam;
        Param := DSSPrime.Parser.StrValue;
     // there will be no named paramters in this command and the params
     // themselves will be the parameter name to return
        while Length(Param) > 0 do
        begin
            ParamPointer := OptionList.GetCommand(Param);

            case ParamPointer of
                0:
                    DoSimpleMsg(DSSPrime, 'Unknown parameter "' + ParamName + '" for Get Command ', 133);
                1, 12:
                    AppendGlobalResult(DSSPrime, DSSPrime.ActiveCircuit.ActiveCktElement.DSSClassName);
                2, 13:
                    AppendGlobalResult(DSSPrime, DSSPrime.ActiveCircuit.ActiveCktElement.Name);
                3:
                    AppendGlobalResult(DSSPrime, IntToStr(DSSPrime.ActiveCircuit.solution.DynaVars.intHour));
                4:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.solution.DynaVars.t]));
                5:
                    AppendGlobalResult(DSSPrime, IntToStr(DSSPrime.ActiveCircuit.solution.Year));
                6:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.solution.Frequency]));
                7, 18:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.solution.DynaVars.h]));
                8:
                    AppendGlobalResult(DSSPrime, GetSolutionModeID);
                9:
                    AppendGlobalResult(DSSPrime, GetRandomModeID);
                10:
                    AppendGlobalResult(DSSPrime, IntToStr(DSSPrime.ActiveCircuit.solution.NumberOfTimes));
                11:
                    AppendGlobalResult(DSSPrime, Format('[ %d, %-g ] !... %-g (hours)', [DSSPrime.ActiveCircuit.solution.DynaVars.intHour, DSSPrime.ActiveCircuit.solution.DynaVars.t, DSSPrime.ActiveCircuit.solution.DynaVars.dblHour]));
                14:
                    AppendGlobalResult(DSSPrime, DSSPrime.ActiveCircuit.name);
                15:
                    AppendGlobalResult(DSSPrime, DefaultEditor);
                16:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.solution.ConvergenceTolerance]));
                17:
                    AppendGlobalResult(DSSPrime, IntToStr(DSSPrime.ActiveCircuit.solution.MaxIterations));
                19:
                    AppendGlobalResult(DSSPrime, GetLoadModel);
                20:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.LoadMultiplier]));
                21:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.NormalMinVolts]));
                22:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.NormalMaxVolts]));
                23:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.EmergMinVolts]));
                24:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.EmergMaxVolts]));
                25:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.DefaultDailyShapeObj.Mean * 100.0]));
                26:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.DefaultDailyShapeObj.StdDev * 100.0]));
                27:
                    AppendGlobalResult(DSSPrime, DSSPrime.ActiveCircuit.LoadDurCurve);
                28:
                    AppendGlobalResult(DSSPrime, Format('%-g', [(DSSPrime.ActiveCircuit.DefaultGrowthRate - 1.0) * 100.0]));
                29:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.AutoAddObj.GenkW]));
                30:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.AutoAddObj.GenPF]));
                31:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.AutoAddObj.CapkVAR]));
                32:
                    case DSSPrime.ActiveCircuit.AutoAddObj.Addtype of
                        GENADD:
                            AppendGlobalResult(DSSPrime, 'generator');
                        CAPADD:
                            AppendGlobalResult(DSSPrime, 'capacitor');
                    end;
                33:
                    if DSSPrime.ActiveCircuit.DuplicatesAllowed then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                34:
                    if DSSPrime.ActiveCircuit.ZonesLocked then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                35:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.UEWeight]));
                36:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.LossWeight]));
                37:
                    AppendGlobalResult(DSSPrime, IntArrayToString(DSSPrime.ActiveCircuit.UERegs, DSSPrime.ActiveCircuit.NumUEregs));
                38:
                    AppendGlobalResult(DSSPrime, IntArrayToString(DSSPrime.ActiveCircuit.LossRegs, DSSPrime.ActiveCircuit.NumLossRegs));
                39:
                    with DSSPrime.ActiveCircuit do
                    begin
                        i := 1;
                        DSSPrime.GlobalResult := '(';
                        while LegalVoltageBases^[i] > 0.0 do
                        begin
                            DSSPrime.GlobalResult := DSSPrime.GlobalResult + Format('%-g, ', [LegalVoltageBases^[i]]);
                            inc(i);
                        end;
                        DSSPrime.GlobalResult := DSSPrime.GlobalResult + ')';
                    end;
                40:
                    case DSSPrime.ActiveCircuit.Solution.Algorithm of
                        NORMALSOLVE:
                            AppendGlobalResult(DSSPrime, 'normal');
                        NEWTONSOLVE:
                            AppendGlobalResult(DSSPrime, 'newton');
                    end;
                41:
                    if DSSPrime.ActiveCircuit.TrapezoidalIntegration then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                42:
                    with DSSPrime.ActiveCircuit.AutoAddBusList do
                        for i := 1 to ListSize do
                            AppendGlobalResult(DSSPrime, Get(i));
                43:
                    AppendGlobalResult(DSSPrime, GetControlModeID);
                44:
                    if DSSPrime.ActiveCircuit.ControlQueue.traceLog then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                45:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.GenMultiplier]));
                46:
                    AppendGlobalResult(DSSPrime, DSSPrime.ActiveCircuit.DefaultDailyShapeObj.Name);
                47:
                    AppendGlobalResult(DSSPrime, DSSPrime.ActiveCircuit.DefaultYearlyShapeObj.Name);
                48:
                    AppendGlobalResult(DSSPrime, 'Get function not applicable.');
                49:
                    if DSSPrime.ActiveCircuit.positiveSequence then
                        AppendGlobalResult(DSSPrime, 'positive')
                    else
                        AppendGlobalResult(DSSPrime, 'multiphase');
                50:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.PriceSignal]));
                51:
                    AppendGlobalResult(DSSPrime, DSSPrime.ActiveCircuit.PriceCurve);
                52:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.ActiveCircuit.ActiveCktElement.ActiveTerminalIdx]));
                53:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.Fundamental]));
                54:
                    with DSSPrime.ActiveCircuit.Solution do
                        if DoALLHarmonics then
                            AppendGlobalResult(DSSPrime, 'ALL')
                        else
                        begin
                            for i := 1 to HarmonicListSize do
                                AppendGlobalResult(DSSPrime, Format('%-g', [HarmonicList^[i]]));
                        end;
                55:
                    AppendGlobalResult(DSSPrime, IntToStr(DSSPrime.ActiveCircuit.solution.MaxControlIterations));
                56:
                    AppendGlobalResult(DSSPrime, DSSPrime.ActiveCircuit.BusList.Get(DSSPrime.ActiveCircuit.ActiveBusIndex));
                57:
                    AppendGlobalResult(DSSPrime, DSSPrime.DataDirectory); // NOTE - not necessarily output directory
                58:
                    with DSSPrime.ActiveCircuit do
                        for i := 1 to NumBuses do
                            if Buses^[i].Keep then
                                AppendGlobalResult(DSSPrime, BusList.Get(i));
                59:
                    AppendGlobalResult(DSSPrime, DSSPrime.ActiveCircuit.ReductionStrategyString);
                60:
                    if DSSPrime.EnergyMeterClass.SaveDemandInterval then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                61:
                    AppendGlobalResult(DSSPrime, Format('%-.g', [DSSPrime.ActiveCircuit.PctNormalFactor]));
                62:
                    if DSSPrime.EnergyMeterClass.DI_Verbose then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                63:
                    AppendGlobalResult(DSSPrime, DSSPrime.ActiveCircuit.CaseName);
                64:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.ActiveCircuit.NodeMarkerCode]));
                65:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.ActiveCircuit.NodeMarkerWidth]));
                66:
                    if DSSPrime.ActiveCircuit.LogEvents then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                67:
                    if DSSPrime.DSSExecutive.RecorderON then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                68:
                    if DSSPrime.EnergyMeterClass.Do_OverloadReport then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                69:
                    if DSSPrime.EnergyMeterClass.Do_VoltageExceptionReport then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                70:
                    AppendGlobalResult(DSSPrime, 'Get function not applicable.');
                71:
                    if DSSPrime.AutoShowExport then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                72:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.MaxAllocationIterations]));
                73:
                    AppendGlobalResult(DSSPrime, Format('%d', [Round(DSSPrime.DefaultBaseFreq)]));
                74:
                    if DSSPrime.ActiveCircuit.MarkSwitches then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                75:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.ActiveCircuit.SwitchMarkerCode]));
                76:
                    AppendGlobalResult(DSSPrime, Format('%-.6g', [DSSPrime.DaisySize]));
                77:
                    if DSSPrime.ActiveCircuit.MarkTransformers then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                78:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.ActiveCircuit.TransMarkerCode]));
                79:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.ActiveCircuit.TransMarkerSize]));
                80:
                    AppendGlobalResult(DSSPrime, GetActiveLoadShapeClass);
                81:
                    AppendGlobalResult(DSSPrime, GetEarthModel(DSSPrime.DefaultEarthModel));
                82:
                    if DSSPrime.LogQueries then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                83:
                    if DSSPrime.ActiveCircuit.MarkCapacitors then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                84:
                    if DSSPrime.ActiveCircuit.MarkRegulators then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                85:
                    if DSSPrime.ActiveCircuit.MarkPVSystems then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                86:
                    if DSSPrime.ActiveCircuit.MarkStorage then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                87:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.ActiveCircuit.CapMarkerCode]));
                88:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.ActiveCircuit.RegMarkerCode]));
                89:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.ActiveCircuit.PVMarkerCode]));
                90:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.ActiveCircuit.StoreMarkerCode]));
                91:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.ActiveCircuit.CapMarkerSize]));
                92:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.ActiveCircuit.RegMarkerSize]));
                93:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.ActiveCircuit.PVMarkerSize]));
                94:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.ActiveCircuit.StoreMarkerSize]));
                95:
                    if DSSPrime.ActiveCircuit.NeglectLoadY then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                96:
                    if DSSPrime.ActiveCircuit.MarkFuses then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                97:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.ActiveCircuit.FuseMarkerCode]));
                98:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.ActiveCircuit.FuseMarkerSize]));
                99:
                    if DSSPrime.ActiveCircuit.MarkReclosers then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                100:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.ActiveCircuit.RecloserMarkerCode]));
                101:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.ActiveCircuit.RecloserMarkerSize]));
                102:
                    if UpdateRegistry then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                103:
                    if DSSPrime.ActiveCircuit.MarkRelays then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                104:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.ActiveCircuit.RelayMarkerCode]));
                105:
                    AppendGlobalResult(DSSPrime, Format('%d', [DSSPrime.ActiveCircuit.RelayMarkerSize]));
                106:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.Solution.Time_Solve]));
                107:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.Solution.Total_Time]));
                108:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.Solution.Time_Step]));
                109:
                    if DSSPrime.ActiveCircuit.Solution.SampleTheMeters then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                110:
                    AppendGlobalResult(DSSPrime, IntToStr(DSSPrime.ActiveCircuit.solution.MinIterations));
                111:
                    if DSS_Viz_enable then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                112:
                    if DSSPrime.ActiveCircuit.ReduceLateralsKeepLoad then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                113:
                    AppendGlobalResult(DSSPrime, Format('%-g', [DSSPrime.ActiveCircuit.ReductionZmag]));
                114:
                    if DSSPrime.SeasonalRating then
                        AppendGlobalResult(DSSPrime, 'Yes')
                    else
                        AppendGlobalResult(DSSPrime, 'No');
                115:
                    AppendGlobalResult(DSSPrime, DSSPrime.SeasonSignal);
            else
           // Ignore excess parameters
            end;

            ParamName := DSSPrime.Parser.NextParam;
            Param := DSSPrime.Parser.StrValue;
        end; {WHILE}

    except
        AppendGlobalResult(DSSPrime, '***Error***');
    end;

end;

procedure DisposeStrings;
var
    i: Integer;

begin
    for i := 1 to NumExecOptions do
    begin
        ExecOption[i] := '';
        OptionHelp[i] := '';
    end;

end;


initialization

    DefineOptions;

finalization

    DisposeStrings;


end.
