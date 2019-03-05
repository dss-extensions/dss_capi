unit PlotOptions;

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
    NumPlotOptions = 23;

function DoPlotCmd: Integer;

var

    PlotOption,
    PlotHelp: array[1..NumPlotOptions] of String;
    PlotCommands: TCommandList;


implementation

uses
    DSSPlot,
    DSSGlobals,
    SysUtils,
    ParserDel,
    Utilities;

procedure DefineOptions;

begin


    PlotOption[1] := 'type';
    PlotOption[2] := 'quantity';
    PlotOption[3] := 'max';
    PlotOption[4] := 'dots';
    PlotOption[5] := 'labels';
    PlotOption[6] := 'object';
    PlotOption[7] := 'showloops';
    PlotOption[8] := 'r3';
    PlotOption[9] := 'r2';
    PlotOption[10] := 'c1';
    PlotOption[11] := 'c2';
    PlotOption[12] := 'c3';
    PlotOption[13] := 'channels';
    PlotOption[14] := 'bases';
    PlotOption[15] := 'subs';
    PlotOption[16] := 'thickness';
    PlotOption[17] := 'buslist';
    PlotOption[18] := 'min';
    PlotOption[19] := '3phLinestyle';
    PlotOption[20] := '1phLinestyle';
    PlotOption[21] := 'phases';
    PlotOption[22] := 'profilescale';
    PlotOption[23] := 'PlotID';


    PlotHelp[1] := 'One of {Circuit | Monitor | Daisy | Zones | AutoAdd | ' +
        'General (bus data) | Loadshape | Tshape | Priceshape |Profile} ' + CRLF +
        'A "Daisy" plot is a special circuit plot that places a marker at each Generator location ' +
        'or at buses in the BusList property, if defined. ' +
        'A Zones plot shows the meter zones (see help on Object). ' +
        'Autoadd shows the autoadded generators. General plot shows quantities associated with buses ' +
        'using gradient colors between C1 and C2. Values are read from a file (see Object). ' +
        'Loadshape plots the specified loadshape. Examples:' + CRLF + CRLF +
        'Plot type=circuit quantity=power' + CRLF +
        'Plot Circuit Losses 1phlinestyle=3' + CRLF +
        'Plot Circuit quantity=3 object=mybranchdata.csv' + CRLF +
        'Plot daisy power max=5000 dots=N Buslist=[file=MyBusList.txt]' + CRLF +
        'Plot General quantity=1 object=mybusdata.csv' + CRLF +
        'Plot Loadshape object=myloadshape' + CRLF +
        'Plot Tshape object=mytemperatureshape' + CRLF +
        'Plot Priceshape object=mypriceshape' + CRLF +
        'Plot Profile' + CRLF +
        'Plot Profile Phases=Primary' + CRLF + CRLF +
        'Additional plots with the OpenDSS Viewer (These plots are enabled with the OpenDSSViewer option):' + CRLF +
        '- Plot evolution  ! Probabilistic density evolution plot with the line-to-ground magnitude of all load voltages in per unit base.' + CRLF +
        '- Plot loadshape object=myLoadshape  ! Loadshapes with the OpenDSS Viewer functionalities.' + CRLF +
        '- Plot matrix incidence  ! Incidence matrix plot (Requires: CalcIncMatrix or CalcIncMatrix_O).' + CRLF +
        '- Plot matrix laplacian  ! Laplacian matrix plot (Requires: CalcLaplacian).' + CRLF +
        '- Plot monitor object=myMonitor  ! Monitors with the OpenDSS Viewer functionalities. All channels are included in this plot.' + CRLF +
        '- Plot profile  ! 3D and 2D versions of the voltage profile.' + CRLF +
        '- Plot scatter  ! Scatter plot with geovisualization of line-to-ground bus voltage magnitudes in per unit.';
    PlotHelp[2] := 'One of {Voltage | Current | Power | Losses | Capacity | (Value Index for General, AutoAdd, or Circuit[w/ file]) }';
    PlotHelp[3] := 'Enter 0 (the default value) or the value corresponding to max scale or line thickness in the circuit plots. ' +
        'Power and Losses in kW. Also, use this to specify the max value corresponding to color C2 in General plots.';
    PlotHelp[4] := 'Yes or No*. Places a marker on the circuit plot at the bus location. See Set Markercode under options.';
    PlotHelp[5] := 'Yes or No*. If yes, bus labels (abbreviated) are printed on the circuit plot.';
    PlotHelp[6] := 'Object to be plotted. One of [Meter Name (zones plot) | Monitor Name | LoadShape Name | File Name for General bus data | File Name Circuit branch data]';
    PlotHelp[7] := '{Yes | No*} Shows loops on Circuit plot. Requires an EnergyMeter to be defined.';
    PlotHelp[8] := 'pu value for tri-color plot max range [default=.85 of max scale]. Corresponds to color C3.';
    PlotHelp[9] := 'pu value for tri-color plot mid range [default=.50 of max scale]. Corresponds to color C2.';
    PlotHelp[10] := 'RGB color number or standard color name for color C1. This is the default color for circuit plots. Default is blue. See options in the Plot menu.' + CRLF + CRLF +
        'Standard color names are: ' + CRLF + CRLF +
        ' Black  ' + CRLF +
        ' Maroon ' + CRLF +
        ' Green  ' + CRLF +
        ' Olive  ' + CRLF +
        ' Navy   ' + CRLF +
        ' Purple ' + CRLF +
        ' Teal   ' + CRLF +
        ' Gray   ' + CRLF +
        ' Silver ' + CRLF +
        ' Red    ' + CRLF +
        ' Lime   ' + CRLF +
        ' Yellow ' + CRLF +
        ' Blue   ' + CRLF +
        ' Fuchsia' + CRLF +
        ' Aqua   ' + CRLF +
        ' LtGray ' + CRLF +
        ' DkGray ' + CRLF +
        ' White  ';
    PlotHelp[11] := 'RGB color number or standard color name for color C2. Used for gradients and tricolor plots such as circuit voltage.' + CRLF + CRLF +
        'See Help on C1 for list of standard color names.';
    PlotHelp[12] := 'RGB color number or standard color name for color C3. Used for gradients and tricolor plots such a circuit voltage.' + CRLF + CRLF +
        'See Help on C1 for list of standard color names.';
    PlotHelp[13] := 'Array of channel numbers for monitor plot. Example' + CRLF + CRLF +
        'Plot Type=Monitor Object=MyMonitor Channels=[1, 3, 5]' + CRLF + CRLF +
        'Do "Show Monitor MyMonitor" to see channel definitions.';
    PlotHelp[14] := 'Array of base values for each channel for monitor plot. Useful for creating per unit plots. Default is 1.0 for each channel.  Set Base= property after defining channels.' + CRLF + CRLF +
        'Plot Type=Monitor Object=MyMonitor Channels=[1, 3, 5] Bases=[2400 2400 2400]' + CRLF + CRLF +
        'Do "Show Monitor MyMonitor" to see channel range and definitions.';
    ;
    PlotHelp[15] := '{Yes | No*} Displays a marker at each transformer declared to be a substation. ' +
        'At least one bus coordinate must be defined for the transformer. ' +
        'See MarkTransformer and TransMarkerCode options.';
    PlotHelp[16] := 'Max thickness allowed for lines in circuit plots (default=7).';
    PlotHelp[17] := '{Array of Bus Names | File=filename } This is for the Daisy plot. ' + CRLF + CRLF +
        'Plot daisy power max=5000 dots=N Buslist=[file=MyBusList.txt]' + CRLF + CRLF +
        'A "daisy" marker is plotted for ' +
        'each bus in the list. Bus name may be repeated, which results in multiple markers distributed around the bus location. ' +
        'This gives the appearance of a daisy if there are several symbols at a bus. Not needed for plotting active generators.';
    PlotHelp[18] := 'Enter 0 (the default value) or the value corresponding to min value corresponding to color C1 in General bus data plots.';
    PlotHelp[19] := 'Line style for drawing 3-phase lines. A number in the range of [1..7].Default is 1 (solid). Use 3 for dotted; 2 for dashed.';
    PlotHelp[20] := 'Line style for drawing 1-phase lines. A number in the range of [1..7].Default is 1 (solid). Use 3 for dotted; 2 for dashed.';
    PlotHelp[21] := '{default* | ALL | PRIMARY | LL3ph | LLALL | LLPRIMARY | (phase number)} For Profile plot. Specify which phases you want plotted.' + CRLF + CRLF +
        'default = plot only nodes 1-3 at 3-phase buses (default)' + CRLF +
        'ALL = plot all nodes' + CRLF +
        'PRIMARY = plot all nodes -- primary only (voltage > 1kV)' + CRLF +
        'LL3ph = 3-ph buses only -- L-L voltages)' + CRLF +
        'LLALL = plot all nodes -- L-L voltages)' + CRLF +
        'LLPRIMARY = plot all nodes -- L-L voltages primary only)' + CRLF +
        '(phase number) = plot all nodes on selected phase' + CRLF + CRLF +
        'Note: Only nodes downline from an energy meter are plotted.';
    PlotHelp[22] := 'PUKM | 120KFT, default is PUKM' + CRLF +
        'PUKM = per-unit voltage vs. distance in km' + CRLF +
        '120KFT = voltage on 120-V base vs. distance in kft.';
    PlotHelp[23] := 'Plot identifier for dynamic updates of "profile" and "scatter" plots in the OpenDSS Viewer (See "plot type" for more details).' +
        'When multiple "plot" commands are executed with the same PlotID, the same figure will be updated with the most recent simulation results.' + CRLF + CRLF +
        'This identifier could be declared as an integer number or a string without spaces.' + CRLF + CRLF +
        'Example:' + CRLF + CRLF +
        'set OpenDSSViewer=true ! OpenDSS Viewer enabled' + CRLF +
        'solve' + CRLF +
        'plot scatter PlotID=plotA  !Generates a new scatter plot' + CRLF +
        'solve' + CRLF +
        'plot scatter PlotID=plotB  !Generates a new scatter plot' + CRLF +
        'solve' + CRLF +
        'plot scatter PlotID=plotA  !Updates the data in plotA' + CRLF +
        'solve' + CRLF +
        'plot scatter PlotID=plotA  !Updates the data in plotA' + CRLF;

end;


//----------------------------------------------------------------------------
function DoPlotCmd: Integer;

{
  Produce a plot with the DSSGraphX object
}

var

    ParamName, Param: String;
    ParamPointer, i: Integer;
    DblBuffer: array[0..50] of Double;
    NumChannels: Integer;

begin
    Result := 0;

    if NoFormsAllowed then
    begin
        Result := 1;
        Exit;
    end;

    if not Assigned(DSSPlotObj) then
        DSSPlotObj := TDSSPlot.Create;

    DSSPlotObj.SetDefaults;

    {Get next parameter on command line}
    ParamPointer := 0;
    ParamName := Uppercase(Parser[ActiveActor].NextParam);
    Param := Uppercase(Parser[ActiveActor].StrValue);
    while Length(Param) > 0 do
    begin
      {Interpret Parameter}
        if (Length(ParamName) = 0) then
            Inc(ParamPointer)
        else
            ParamPointer := PlotCommands.Getcommand(ParamName);

      {Check options requiring a solution and abort if no solution or circuit}
        case ParamPointer of
            1:
                case Param[1] of
                    'A', 'C', 'D', 'G', 'M', 'P', 'Z':
                        if not (CompareTextShortest('pri', Param) = 0) then   // allow Price shape
                        begin
                            if not assigned(ActiveCircuit[ActiveActor]) then
                            begin
                                DoSimpleMsg('No circuit created.', 24731);
                                Exit;
                            end;
                            if not assigned(ActiveCircuit[ActiveActor].Solution) or not assigned(ActiveCircuit[ActiveActor].Solution.NodeV) then
                            begin
                                DoSimpleMsg('The circuit must be solved before you can do this.', 24732);
                                Exit;
                            end;
                        end;
                end;
        end;


        with DSSPlotObj do
            case ParamPointer of

                1:
                    case Param[1] of
                        'A':
                        begin
                            PlotType := ptAutoAddLogPlot;
                            ObjectName := CircuitName_[ActiveActor] + 'AutoAddLog.CSV';
                            ValueIndex := 2;
                        end;
                        'C':
                            PlotType := ptCircuitplot;
                        'E':
{$IFDEF MSWINDOWS}
                            PlotType := ptEvolutionPlot
{$ENDIF}
                            ;
                        'G':
                            PlotType := ptGeneralDataPlot;
                        'L':
                            PlotType := ptLoadshape;
                        'M':
                            if CompareTextShortest('mon', Param) = 0 then
                                PlotType := ptMonitorplot
                    {$IFDEF MSWINDOWS}
                            else
                                PlotType := ptMatrixplot
{$ENDIF}
                            ;
                        'P':
                            if CompareTextShortest('pro', Param) = 0 then
                                PlotType := ptProfile
                            else
                                PlotType := ptPriceShape;
                        'S':
{$IFDEF MSWINDOWS}
                            PlotType := ptScatterPlot
{$ENDIF}
                            ;
                        'T':
{$IFDEF MSWINDOWS}
                            PlotType := ptTshape
{$ENDIF}
                            ;
                        'D':
                        begin
                            PlotType := ptDaisyplot;
                            DaisyBusList.Clear;
                        end;
                        'Z':
                            PlotType := ptMeterZones;
                    else
                    end;
                2:
                    case Param[1] of
                        'V':
                            Quantity := pqVoltage;
                        'C':
                            case Param[2] of
                                'A':
                                    Quantity := pqcapacity;
                                'U':
                                    Quantity := pqcurrent;
                            end;
                        'P':
                            Quantity := pqpower;
                        'L':
{$IFDEF MSWINDOWS}
                            if CompareTextShortest('los', Param) = 0 then
                                Quantity := pqlosses
                            else
                                MatrixType := pLaplacian
{$ENDIF}
                            ;
                        'I':
{$IFDEF MSWINDOWS}
                            MatrixType := pIncMatrix
{$ENDIF}
                            ;
                    else
                        Quantity := pqNone;
                        Valueindex := Parser[ActiveActor].IntValue;
                    end;
                3:
                begin
                    MaxScale := Parser[ActiveActor].DblValue;
                    if MaxScale > 0.0 then
                        MaxScaleIsSpecified := TRUE    // Indicate the user wants a particular value
                    else
                        MaxScaleIsSpecified := FALSE;
                end;
                4:
                    Dots := InterpretYesNo(Param);
                5:
                    Labels := InterpretYesNo(Param);
                6:
                    ObjectName := Parser[ActiveActor].StrValue;
                7:
                begin
                    ShowLoops := InterpretYesNo(Param);
                    if ShowLoops then
                        PlotType := ptMeterzones;
                end;
                8:
                    TriColorMax := Parser[ActiveActor].DblValue;
                9:
                    TriColorMid := Parser[ActiveActor].DblValue;
                10:
                    Color1 := InterpretColorName(Param);
                11:
                    Color2 := InterpretColorName(Param);
                12:
                    Color3 := InterpretColorName(Param);
                13:
                begin    {Channel definitions for Plot Monitor}
                    NumChannels := Parser[ActiveActor].ParseAsVector(51, @DblBuffer);  // allow up to 50 channels
                    if NumChannels > 0 then
                    begin   // Else take the defaults
                        SetLength(Channels, NumChannels);
                        for i := 0 to NumChannels - 1 do
                            Channels[i] := Round(DblBuffer[i]);
                        SetLength(Bases, NumChannels);
                        for i := 0 to NumChannels - 1 do
                            Bases[i] := 1.0;
                    end;
                end;
                14:
                begin
                    NumChannels := Parser[ActiveActor].ParseAsVector(51, @DblBuffer);  // allow up to 50 channels
                    if NumChannels > 0 then
                    begin
                        SetLength(Bases, NumChannels);
                        for i := 0 to NumChannels - 1 do
                            Bases[i] := DblBuffer[i];
                    end;
                end;
                15:
                    ShowSubs := InterpretYesNo(Param);
                16:
                    MaxLineThickness := Parser[ActiveActor].IntValue;
                17:
                    InterpretTStringListArray(Param, DaisyBusList);  {read in Bus list}
                18:
                begin
                {$IFDEF MSWINDOWS}
                    MinScale := Parser[ActiveActor].DblValue;
                    MinScaleIsSpecified := TRUE;    // Indicate the user wants a particular value
                {$ENDIF}
                end;
                19:
{$IFDEF MSWINDOWS}
                    ThreePhLineStyle := Parser[ActiveActor].IntValue
{$ENDIF}
                    ;
                20:
{$IFDEF MSWINDOWS}
                    SinglePhLineStyle := Parser[ActiveActor].IntValue
{$ENDIF}
                    ;
                21:
                begin  // Parse off phase(s) to plot
             {$IFDEF MSWINDOWS}
                    PhasesToPlot := PROFILE3PH; // the default
                    if CompareTextShortest(Param, 'default') = 0 then
                        PhasesToPlot := PROFILE3PH
                    else
                    if CompareTextShortest(Param, 'all') = 0 then
                        PhasesToPlot := PROFILEALL
                    else
                    if CompareTextShortest(Param, 'primary') = 0 then
                        PhasesToPlot := PROFILEALLPRI
                    else
                    if CompareTextShortest(Param, 'll3ph') = 0 then
                        PhasesToPlot := PROFILELL
                    else
                    if CompareTextShortest(Param, 'llall') = 0 then
                        PhasesToPlot := PROFILELLALL
                    else
                    if CompareTextShortest(Param, 'llprimary') = 0 then
                        PhasesToPlot := PROFILELLPRI
                    else
                    if Length(Param) = 1 then
                        PhasesToPlot := Parser[ActiveActor].IntValue;
             {$ENDIF}
                end;
                22:
                begin
                    ProfileScale := PROFILEPUKM;
                    if CompareTextShortest(Param, '120KFT') = 0 then
                        ProfileScale := PROFILE120KFT;
                end;
                23:
                    PlotID := Parser[ActiveActor].StrValue;
            else
            end;


        ParamName := Uppercase(Parser[ActiveActor].NextParam);
        Param := Uppercase(Parser[ActiveActor].StrValue);
    end;

    if not ActiveCircuit[ActiveActor].Issolved then
        DSSPlotObj.Quantity := pqNone;

    with DSSPlotObj do
    begin
        if DSS_Viz_enable then
        begin
      {$IFDEF MSWINDOWS}
            if (DSS_Viz_installed and ((
                PlotType = ptMonitorplot) or (
                PlotType = ptLoadshape) or (
                PlotType = ptProfile) or (
                PlotType = ptScatterPlot) or (
                PlotType = ptEvolutionPlot) or (
                PlotType = ptMatrixplot))) then
                DSSVizPlot; // OpenDSS Viewer
      {$ENDIF}
        end
        else
        begin
      {$IFDEF MSWINDOWS}
            if (PlotType = ptScatterPlot) or (
                PlotType = ptEvolutionPlot) or (
                PlotType = ptMatrixplot) then
                DoSimpleMsg('The OpenDSS Viewer is disabled (Check the OpenDSSViewer option in the help).', 0)
            else
                Execute;   // makes a new plot based on these options
      {$ENDIF}
        end;
    end;

end;


procedure DisposeStrings;
var
    i: Integer;

begin
    for i := 1 to NumPlotOptions do
    begin
        PlotOption[i] := '';
        PlotHelp[i] := '';
    end;

end;


initialization

    DefineOptions;

    PlotCommands := TCommandList.Create(PlotOption);
    PlotCommands.Abbrev := TRUE;

finalization

    DisposeStrings;
    PlotCommands.Free;
end.
