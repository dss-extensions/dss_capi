# DSS Extensions: OpenDSS Commands and Properties

---

**This document was generated from:** `DSS C-API Library version 0.10.7 revision 47422ccbb833c89de64d8941c520d106c9e7d0d4 based on OpenDSS SVN 2963 (v7/classic variation) MVMULT [FPC 3.2.0] (64-bit build)`

*Generated with the legacy models disabled (i.e. OpenDSS v9+ compatibility mode).*

---

## About this

This is a document automatically generated from the commands, options and properties for the DSS language (script level) exposed in the DSS Extensions version of the OpenDSS engine. A separate document will be developed in the future to detail **API** functions and general usage recommendations for the projects under DSS Extensions.

Since the extensive majority of properties and elements are compatible, this document can be useful when using either the official OpenDSS implementation or the DSS Extensions version (DSS C-API engine), consumed through the projects DSS Python (`dss_python`), OpenDSSDirect.py, OpenDSSDirect.jl, DSS Sharp (`dss_sharp`), and DSS MATLAB (`dss_matlab`).  If you are using the official OpenDSS, when in doubt check the official documentation and/or source code.

As a final note, keep in mind that not all commands are implemented in the DSS Extensions engine, interactive commands like plots are missing (on purpose).

---
## Commands

| Command | Description |
| - | - |
| // | Comment.  Command line is ignored. |
| ? | Inquiry for property value.  Result is put into GlobalReault and can be seen in the Result Window. Specify the full property name.<br><br>Example: ? Line.Line1.R1<br><br>Note you can set this property merely by saying:<br>Line.line1.r1=.058 |
| _DoControlActions | For step control of solution process: Pops control actions off the control queue according to the present control mode rules. Dispatches contol actions to proper control element "DoPendingAction" handlers. |
| _InitSnap | For step control of solution process: Intialize iteration counters, etc. that normally occurs at the start of a snapshot solution process. |
| _SampleControls | For step control of solution process: Sample the control elements, which push control action requests onto the control queue. |
| _ShowControlQueue | For step control of solution process: Show the present control queue contents. |
| _SolveDirect | For step control of solution process: Invoke direct solution function in DSS. Non-iterative solution of Y matrix and active sources only. |
| _SolveNoControl | For step control of solution process: Solves the circuit in present state but does not check for control actions. |
| _SolvePFlow | For step control of solution process: Invoke iterative power flow solution function of DSS directly. |
| ~ | Continuation of editing on the active object. An abbreviation.<br><br>Example:<br>New Line.Line1 Bus1=aaa  bus2=bbb<br>\~ R1=.058<br>\~ X1=.1121 |
| About | Display "About Box".  (Result string set to Version string.) |
| AddBusMarker | Add a marker to a bus in a circuit plot. Markers must be added before issuing the Plot command. Effect is persistent until circuit is cleared. See also ClearBusMarkers command. Example: <br><br>ClearBusMarkers    !...Clears any previous bus markers<br>AddBusMarker Bus=Mybusname code=5 color=Red size=3<br><br>You can use any of the standard color names  or RGB numbers. See Help on C1 property in Plot command. |
| AggregateProfiles | Aggregates the load shapes in the model using the number of zones given in the argument.<br>Use this command when the number of load shapes is considerably big, this algorithm will simplify<br>the amount of load shapes in order to make the memory consumption lower for the model.<br>The output of this algorithm is a script describing the new load shapes and their application into loads across the model.<br>The argument on this command can be: Actual/pu to define the units in which the load profiles are.<br>Check the OpenDSS user manual for details |
| AlignFile | Alignfile [file=]filename.  Aligns DSS script files in columns for easier reading. |
| Allocateloads | Estimates the allocation factors for loads that are defined using the XFKVA property. Requires that energymeter objects be defined with the PEAKCURRENT property set. Loads that are not in the zone of an energymeter cannot be allocated. |
| AllPCEatBus | Brings back the names of all PCE connected to the bus specified in the argument.<br>The command goes as follows:<br><br>AllPCEatBus myBus<br><br>Where "myBus" is the name of the bus of interest |
| AllPDEatBus | Brings back the names of all PDE connected to the bus specified in the argument.<br>The command goes as follows:<br><br>AllPDEatBus myBus<br><br>Where "myBus" is the name of the bus of interest |
| BatchEdit | Batch edit objects in the same class. Example: BatchEdit Load..\* duty=duty_shape<br>In place of the object name, supply a PERL regular expression. .\* matches all names.<br>The subsequent parameter string is applied to each object selected. |
| BuildY | Forces rebuild of Y matrix upon next Solve command regardless of need. The usual reason for doing this would be to reset the matrix for another load level when using LoadModel=PowerFlow (the default) when the system is difficult to solve when the load is far from its base value.  Works by invalidating the Y primitive matrices for all the Power Conversion elements. |
| Buscoords | Define x,y coordinates for buses.  Execute after Solve or MakeBusList command is executed so that bus lists are defined.Reads coordinates from a CSV file with records of the form: busname, x, y.<br><br>Example: BusCoords [file=]xxxx.csv |
| CalcIncMatrix | Calculates the incidence matrix of the Active Circuit |
| CalcIncMatrix_O | Calculates the incidence matrix of the Active Circuit. However, in this case the matrix will be calculated by considering its hierarchical order,which means that the buses order will be generated considering their distribution from the substation to the last load in a radial hierarchy |
| CalcLaplacian | Calculate the laplacian matrix using the incidence matrix<br>previously calculated, this means that before calling this command<br>the incidence matrix needs to be calculated using calcincmatrix/calcincmatrix_o |
| Calcvoltagebases | Calculates voltagebase for buses based on voltage bases defined with Set voltagebases=... command. |
| Capacity | Find the maximum load the active circuit can serve in the PRESENT YEAR. Uses the EnergyMeter objects with the registers set with the SET UEREGS= (..) command for the AutoAdd functions.  Syntax (defaults shown):<br><br>capacity [start=]0.9 [increment=]0.005<br><br>Returns the metered kW (load + losses - generation) and per unit load multiplier for the loading level at which something in the system reports an overload or undervoltage. If no violations, then it returns the metered kW for peak load for the year (1.0 multiplier). Aborts and returns 0 if no energymeters. |
| CD | Change default directory to specified directory<br><br>CD dirname |
| Cktlosses | Returns the total losses for the active circuit in the Result string in kW, kvar. |
| Classes | List of intrinsic DSS Classes. Returns comma-separated list in Result variable. |
| Cleanup | Force execution of the end-of-time-step cleanup functions that samples/saves meters and updates selected state variables such as storage level |
| Clear | Clear all circuits currently in memory. |
| ClearBusMarkers | Clear all bus markers created with the AddBusMarker command. |
| Close | Opposite of the Open command. |
| CloseDI | Close all DI files ... useful at end of yearly solution where DI files are left open. (Reset and Set Year=nnn will also close the DI files) |
| Comparecases | [Case1=]casename [case2=]casename [register=](register number) [meter=]{Totals\* \| SystemMeter \| metername}. <br>Compares yearly simulations of two specified cases with respect to the quantity in the designated register from the designated meter file. Defaults: Register=9 meter=Totals.  Example:<br><br>Comparecases base pvgens 10 |
| Compile | Reads the designated file name containing DSS commands and processes them as if they were entered directly into the command line. The file is said to be "compiled." Similar to "redirect" except changes the default directory to the path of the specified file.<br><br>Syntax:<br>Compile filename |
| Connect | Request to create a TCP/IP socket to communicate data with external modules. This function requires the host address and TCP port to connect. |
| Currents | Returns the currents for each conductor of ALL terminals of the active circuit element in the Result string. (See Select command.)Returned as comma-separated magnitude and angle. |
| CvrtLoadshapes | Convert all Loadshapes presently loaded into either files of single or files of double. Usually files of singles are adequate precision for loadshapes.  Syntax:<br><br>cvrtloadshapes type=sng  (this is the default)<br>cvrtloadshapes type=dbl<br><br>A DSS script for loading the loadshapes from the created files is produced and displayed in the default editor.  |
| DI_plot | [case=]casename [year=]yr [registers=](reg1, reg2,...)  [peak=]y/n  [meter=]metername<br>Plots demand interval (DI) results from yearly simulation cases.  Plots selected registers from selected meter file (default = DI_Totals.CSV).  Peak defaults to NO.  If YES, only daily peak of specified registers is plotted. Example:<br><br> DI_Plot basecase year=5 registers=(9,11) no |
| Disable | Disables a circuit element or entire class.  Example:<br>Disable load.loadxxx<br>Disable generator.\*  (Disables all generators)<br><br>The item remains defined, but is not included in the solution. |
| Disconnect | Request to terminate a TCP/IP socket. This function requires the host address and TCP port to disconnect. |
| Distribute | kw=nn how={Proportional\* \| Uniform \|Random \| Skip} skip=nn PF=nn file=filename MW=nn What=[Generator\*\|Load]<br><br>Creates a DSS script file to distribute Generator or Load objects on the system in the manner specified by "how".<br>kW = total generation to be distributed (default=1000) <br>how= process name as indicated (default=proportional to load)<br>skip = no. of buses to skip for "How=Skip" (default=1)<br>PF = power factor for new generators (default=1.0)<br>file = name of file to save (default=distgenerators.dss or distloads.dss)<br>MW = alternate way to specify kW (default = 1)<br>What = what type of device to add, Generator (default) or Load |
| DOScmd | Do a DOS command. Sends the command "cmd ... " to Windows. Execute the "cmd /?" command in a DOS window to see the options. To do a DOS command and automatically exit, do <br><br>DOScmd /c ...command string ...<br><br>To keep the DOS window open, use /k switch. |
| Dump | Display the properties of either a specific DSS object or a complete dump on all variables in the problem (Warning! Could be very large!). Brings up the default text editor with the text file written by this command.<br> Syntax: dump [class.obj] [debug]<br> Examples:<br><br> Dump line.line1 <br> Dump solution  (dumps all solution vars) <br> Dump commands  (dumps all commands to a text file) <br> Dump transformer.\*  (dumps all transformers)<br> Dump ALLOCationfactors  (load allocation factors)<br> Dump Buslist    (bus name hash list)<br> Dump Devicelist    (Device name hash list)<br> Dump      (dumps all objects in circuit)  |
| Edit | Edit an object. The object is selected and it then becomes the active object.<br><br>Note that Edit is the default command.  You many change a property value simply by giving the full property name and the new value, for example:<br><br>line.line1.r1=.04<br>vsource.source.kvll=230 |
| Enable | Enables a circuit element or entire class.  Example:<br>Enable load.loadxxx<br>Enable generator.\*  (enables all generators) |
| Estimate | Execute state estimator on present circuit given present sensor values. |
| Export | Export various solution values to CSV (or XML) files for import into other programs. Creates a new file except for Energymeter and Generator objects, for which the results for each device of this class are APPENDED to the CSV File. You may export to a specific file by specifying the file name as the LAST parameter on the line. For example:<br><br>  Export Voltage Myvoltagefile.CSV<br><br>Otherwise, the default file names shown in the Export help are used. For Energymeter and Generator, specifying the switch "/multiple" (or /m) for the file name will cause a separate file to be written for each meter or generator. The default is for a single file containing all elements.<br><br>May be abreviated Export V, Export C, etc.  Default is "V" for voltages. If Set ShowExport=Yes, the output file will be automatically displayed in the default editor. Otherwise, you must open the file separately. The name appears in the Result window. |
| ExportOverloads | Exports the overloads report with the content available at the moment of the call. It only affects the overloads report for the active actor. |
| ExportVViolations | Exports the voltage violations report with the content available at the moment of the call. It only affects the voltage violations report for the active actor. |
| Fileedit | Edit specified file in default text file editor (see Set Editor= option).<br><br>Fileedit EXP_METERS.CSV (brings up the meters export file)<br><br>"FileEdit" may be abbreviated to a unique character string. |
| FinishTimeStep | Do Cleanup, sample monitors, and increment time. |
| Formedit | FormEdit [class.object].  Brings up form editor on active DSS object. |
| Get | Returns DSS property values set using the Set command. Result is returne in Result property of the Text interface. <br><br>VBA Example:<br><br>DSSText.Command = "Get mode"<br>Answer = DSSText.Result<br><br>Multiple properties may be requested on one get.  The results are appended and the individual values separated by commas.<br><br>See help on Set command for property names. |
| Help | Gives this display. |
| Init | This command forces reinitialization of the solution for the next Solve command. To minimize iterations, most solutions start with the previous solution unless there has been a circuit change.  However, if the previous solution is bad, it may be necessary to re-initialize.  In most cases, a re-initiallization results in a zero-load power flow solution with only the series power delivery elements considered. |
| Interpolate | {All \| MeterName}  Default is "All". Interpolates coordinates for missing bus coordinates in meter zone |
| LatLongCoords | Define x,y coordinates for buses using Latitude and Longitude values (decimal numbers).  Similar to BusCoords command. Execute after Solve command or MakeBusList command is executed so that bus lists are defined.Reads coordinates from a CSV file with records of the form: busname, Latitude, Longitude.<br><br>Example: LatLongCoords [file=]xxxx.csv<br><br>Note: Longitude is mapped to x coordinate and Latitude is mapped to y coordinate. |
| Losses | Returns the total losses for the active circuit element (see Select command) in the Result string in kW, kvar. |
| M | Continuation of editing on the active object. An abbreviation for More |
| MakeBusList | Updates the buslist, if needed, using the currently enabled circuit elements.  (This happens automatically for Solve command.) See ReprocessBuses |
| MakePosSeq | Attempts to convert present circuit model to a positive sequence equivalent. It is recommended to Save the circuit after this and edit the saved version to correct possible misinterpretations. |
| More | Continuation of editing on the active object. |
| New | Create a new object within the DSS. Object becomes the active object<br>Example: New Line.line1 ... |
| Next | {Year \| Hour \| t}  Increments year, hour, or time as specified.  If "t" is specified, then increments time by current step size. |
| NodeDiff | Global result is set to voltage difference, volts and degrees, (Node1 - Node2) between any two nodes. Syntax:<br><br>   NodeDiff Node1=MyBus.1 Node2=MyOtherBus.1 |
| NodeList | [Circuit element name] (Optional) Returns a list of node numbers for all conductors of all terminals of the active circuit element in the Result window or interface.If the optional circuit element name is supplied, the program makes it the active element. Usage:<br><br>NodeList<br>NodeList Line.Myline |
| Obfuscate | Change Bus and circuit element names to generic values to remove identifying names. Generally, you will follow this command immediately by a "Save Circuit Dir=MyDirName" command. |
| Open | Opens the specified terminal and conductor of the specified circuit element. If the conductor is not specified, all phase conductors of the terminal are opened.<br><br>Examples:<br>Open line.line1 2 <br>(opens all phases of terminal 2)<br><br>Open line.line1 2 3<br>(opens the 3rd conductor of terminal 2) |
| Panel | Displays main control panel window. |
| Phaselosses | Returns the losses for the active circuit element (see Select command) for each PHASE in the Result string in comma-separated kW, kvar pairs. |
| Plot | Plots circuits and results in a variety of manners.  See separate Plot command help. |
| Powers | Returns the powers (complex) going into each conductors of ALL terminals of the active circuit element in the Result string. (See Select command.)Returned as comma-separated kW and kvar. |
| Pstcalc | Pst calculation. PstCalc Npts=nnn Voltages=[array] dt=nnn freq=nn lamp=120 or 230.<br>Set Npts to a big enough value to hold the incoming voltage array. <br>dt = time increment in seconds. default is 1<br>freq = base frequency in Hz 50 or 60. Default is default base frequency<br>Lamp= 120 for North America; 230 for Europe. Default is 120<br><br>PSTCalc Npts=1900 V=[file=MyCSVFile.CSV, Col=3, Header=y] dt=1 freq=60 lamp=120 |
| puvoltages | Just like the Voltages command, except the voltages are in per unit if the kVbase at the bus is defined. |
| Quit | Shuts down DSS unless this is the DLL version.  Then it does nothing;  DLL parent is responsible for shutting down the DLL. |
| Reconductor | Reconductor a line section. Must be in an EnergyMeter zone. <br>Syntax: Reconductor Line1=... Line2=... {LineCode= \| Geometry = } EditString="..." NPhases=#<br>Line1 and Line2 may be given in any order. All lines in the path between the two are redefined with either the LineCode or Geometry (not both). You may also add an optional string the alter any other line properties. The edit string should be enclosed in quotes or parens or brackets.<br>Nphases is an optional filter on the number of phases in line segments to change. |
| Redirect | Reads the designated file name containing DSS commands and processes them as if they were entered directly into the command line. Similar to "Compile", but leaves current directory where it was when Redirect command is invoked.Can temporarily change to subdirectories if nested Redirect commands require.<br><br>ex:  redirect filename |
| Reduce | {All \| MeterName}  Default is "All".  Reduce the circuit according to reduction options. See "Set ReduceOptions" and "Set Keeplist" options.Energymeter objects actually perform the reduction.  "All" causes all meters to reduce their zones. |
| Refine_BusLevels | This function takes the bus levels array and traces all the possible paths considering the longest paths from the substation to the longest branches within the circuit. Then, the new paths are filled with 0 to complement the oroginal levels proposed by the calcincmatrix_o command. |
| RelCalc | [restore=Y/N]Perform reliability calcs: Failure rates and number of interruptions. <br><br>Optional parameter:<br><br>If restore=y automatic restoration of unfaulted section is assumed. |
| Remove | {ElementName=} [KeepLoad=Y\*/N] [EditString="..."] Remove (disable) all branches downline from the PDelement named by "ElementName" property. Circuit must have an Energymeter on this branch. If KeepLoad=Y (default) a new Load element is defined and kW, kvar set to present power flow solution for the first element eliminated. The EditString is applied to each new Load element defined. <br>If KeepLoad=N, all downline elements are disabled. Examples: <br><br>Remove Line.Lin3021<br>Remove Line.L22 Editstring="Daily=Dailycurve Duty=SolarShape<br>Remove Line.L333 KeepLoad=No |
| Rephase | Generates a script to change the phase designation of all lines downstream from a start in line. Useful for such things as moving a single-phase lateral from one phase to another and keep the phase designation consistent for reporting functions that need it to be (not required for simply solving). <br><br>StartLine=... PhaseDesignation="..."  EditString="..." ScriptFileName=... StopAtTransformers=Y/N/T/F<br><br>Enclose the PhaseDesignation in quotes since it contains periods (dots).<br>You may add and optional EditString to edit any other line properties.<br><br>Rephase StartLine=Line.L100  PhaseDesignation=".2"  EditString="phases=1" ScriptFile=Myphasechangefile.DSS  Stop=No |
| ReprocessBuses | Forces reprocessing of bus definitions whether there has been a change or not. Use for rebuilding meter zone lists when a line length changes, for example or some other event that would not normally trigger an update to the bus list. |
| Reset | {MOnitors \| MEters \| Faults \| Controls \| Eventlog \| Keeplist \|(no argument) } Resets all Monitors, Energymeters, etc. If no argument specified, resets all options listed. |
| Rotate | Usage: Rotate [angle=]nnn.  Rotate circuit plotting coordinates by specified angle (degrees).  |
| Sample | Force all monitors and meters to take a sample for the most recent solution. Keep in mind that meters will perform integration. |
| Save | {Save [class=]{Meters \| Circuit \| Voltages \| (classname)} [file=]filename [dir=]directory <br><br>Default class = Meters, which saves the present values in both monitors and energy meters in the active circuit. <br><br>"Save Circuit" saves the present enabled circuit elements to the specified subdirectory in standard DSS form with a Master.txt file and separate files for each class of data. <br><br>If Dir= not specified a unique name based on the circuit name is created automatically. <br><br>If Dir= is specified, any existing files are overwritten. <br><br>"Save Voltages" saves the present solution in a simple CSV format in a file called DSS_SavedVoltages. Used for VDIFF command.<br><br>Any class can be saved to a file.  If no filename specified, the classname is used. |
| Select | Selects an element and makes it the active element.  You can also specify the active terminal (default = 1).<br><br>Syntax:<br>Select [element=]elementname  [terminal=]terminalnumber <br><br>Example:<br>Select Line.Line1 <br>\~ R1=.1<br>(continue editing)<br><br>Select Line.Line1 2 <br>Voltages  (returns voltages at terminal 2 in Result) |
| Seqcurrents | Returns the sequence currents into all terminals of the active circuit element (see Select command) in Result string.  Returned as comma-separated magnitude only values.Order of returned values: 0, 1, 2  (for each terminal). |
| Seqpowers | Returns the sequence powers into all terminals of the active circuit element (see Select command) in Result string.  Returned as comma-separated kw, kvar pairs.Order of returned values: 0, 1, 2  (for each terminal). |
| Seqvoltages | Returns the sequence voltages at all terminals of the active circuit element (see Select command) in Result string.  Returned as comma-separated magnitude only values.Order of returned values: 0, 1, 2  (for each terminal). |
| Set | Used to set various DSS solution modes and options.  You may also set the options with the Solve command. See "Options" for help. |
| SetBusXY | Bus=...  X=...  Y=... Set the X, Y coordinates for a single bus. Prerequisite: Bus must exist as a result of a Solve, CalcVoltageBases, or MakeBusList command. |
| SetkVBase | Command to explicitly set the base voltage for a bus. Bus must be previously defined. Parameters in order are:<br>Bus = {bus name}<br>kVLL = (line-to-line base kV)<br>kVLN = (line-to-neutral base kV)<br><br>kV base is normally given in line-to-line kV (phase-phase). However, it may also be specified by line-to-neutral kV.<br>The following exampes are equivalent:<br><br>setkvbase Bus=B9654 kVLL=13.2<br>setkvbase B9654 13.2<br>setkvbase B9654 kvln=7.62 |
| SetLoadAndGenKV | Set load and generator object kv to agree with the bus they are connected to using the bus voltage base and connection type. |
| Show | Writes selected results to a text file and brings up the default text editor (see Set Editor=....) with the file for you to browse.<br><br>See separate help on Show command. <br><br>Default is "show voltages LN Seq".   |
| Solve | Perform the solution of the present solution mode. You can set any option that you can set with the Set command (see Set). The Solve command is virtually synonymous with the Set command except that a solution is performed after the options are processed. |
| Summary | Returns a power flow summary of the most recent solution in the global result string. |
| TOP | [class=]{Loadshape \| Tshape \| Monitor  } [object=]{ALL (Loadshapes only) \| objectname}. Send specified object to TOP.  Loadshapes and TShapes must be hourly fixed interval.  |
| TotalPowers | Returns the total powers (complex) at ALL terminals of the active circuit element in the Result string. (See Select command.)Returned as comma-separated kW and kvar. |
| Totals | Totals all EnergyMeter objects in the circuit and reports register totals in the result string. |
| UpdateStorage | Update Storage elements based on present solution and time interval.  |
| Userclasses | List of user-defined DSS Classes. Returns comma-separated list in Result variable. |
| Uuids | Read UUIDs (v4) for class names. Tab or comma-delimited file with full object name and UUID |
| var | Define and view script variables.  Variable names begin with "@"<br><br>Usage:<br><br>var @varname1=values  @varname2=value2    ...<br>var @varname1  (shows the value of @varname1)<br>var            (displays all variabiles and values)<br><br>Example of using a variable:<br><br>FileEdit @LastFile |
| Variable | [name=] MyVariableName  [Index=] IndexofMyVariable <br><br>Returns the value of the specified state variable of the active circuit element, if a PCelement. Returns the value as a string in the Result window or the Text.Result interface if using the COM server. <br><br>You may specify the variable by name or by its index. You can determine the index using the VarNames command. If any part of the request is invalid, the Result is null. |
| Varnames | Returns variable names for active element if PC element. Otherwise, returns null. |
| VarValues | Returns variable values for active element if PC element. Otherwise, returns null. |
| Vdiff | Displays the difference between the present solution and the last on saved using the SAVE VOLTAGES command. |
| Visualize | [What=] one of {Currents\* \| Voltages \| Powers} [element=]full_element_name  (class.name). Shows the selected quantity for selected element on a multiphase line drawing in phasor values. |
| Voltages | Returns the voltages for the ACTIVE BUS in the Result string. For setting the active Bus, use the Select command or the Set Bus= option. Returned as magnitude and angle quantities, comma separated, one set per conductor of the terminal. |
| YearlyCurves | [cases=](case1, case2, ...) [registers=](reg1, reg2, ...)  [meter=]{Totals\* \| SystemMeter \| metername}Plots yearly curves for specified cases and registers. <br>Default: meter=Totals. Example: <br><br>yearlycurves cases=(basecase, pvgens) registers=9 |
| Ysc | Returns full Ysc matrix for the ACTIVE BUS in comma-separated complex number form G + jB. |
| Zsc | Returns full Zsc matrix for the ACTIVE BUS in comma-separated complex number form. |
| Zsc012 | Returns symmetrical component short circuit impedances Z0, Z1, and Z2 for the ACTIVE 3-PHASE BUS. Determined from Zsc matrix. |
| Zsc10 | Returns symmetrical component impedances, Z1, Z0 for the ACTIVE BUS in comma-separated R+jX form. |
| ZscRefresh | Refreshes Zsc matrix for the ACTIVE BUS. |

---
## Execution Options

| Option | Description |
| - | - |
| %growth | Set default annual growth rate, percent, for loads with no growth curve specified. Default is 2.5. |
| %mean | Percent mean to use for global load multiplier. Default is 65%. |
| %Normal | Sets the Normal rating of all lines to a specified percent of the emergency rating.  Note: This action takes place immediately. Only the in-memory value is changed for the duration of the run. |
| %stddev | Percent Standard deviation to use for global load multiplier. Default is 9%. |
| Addtype | {Generator \| Capacitor} Default is Generator. Type of device for AutoAdd Mode. |
| Algorithm | {Normal \| Newton}  Solution algorithm type.  Normal is a fixed point iteration that is a little quicker than the Newton iteration.  Normal is adequate for most radial distribution circuits.  Newton is more robust for circuits that are difficult to solve. |
| Allocationfactors | Sets the connected kVA allocation factors for all loads in the active circuit to the value given. |
| Allowduplicates | {YES/TRUE \| NO/FALSE}   Default is No. Flag to indicate if it is OK to have devices of same name in the same class. If No, then a New command is treated as an Edit command. If Yes, then a New command will always result in a device being added. |
| Autobuslist | Array of bus names to include in AutoAdd searches. Or, you can specify a text file holding the names, one to a line, by using the syntax (file=filename) instead of the actual array elements. Default is null, which results in the program using either the buses in the EnergyMeter object zones or, if no EnergyMeters, all the buses, which can make for lengthy solution times. <br><br>Examples:<br><br>Set autobuslist=(bus1, bus2, bus3, ... )<br>Set autobuslist=(file=buslist.txt) |
| Basefrequency | Default = 60. Set the fundamental frequency for harmonic solution and the default base frequency for all impedance quantities. Side effect: also changes the value of the solution frequency. Saved as default for next circuit. |
| Bus | Set Active Bus by name.  Can also be done with Select and SetkVBase commands and the "Set Terminal="  option. The bus connected to the active terminal becomes the active bus. See Zsc and Zsc012 commands. |
| CapkVAR | Size of capacitor, kVAR, to automatically add to system.  Default is 600.0. |
| CapMarkerCode | Numeric marker code (0..47 -- see Users Manual) for Capacitors. Default is 38. |
| CapMarkerSize | Size of Capacitor marker. Default is 3. |
| Casename | Name of case for yearly simulations with demand interval data. Becomes the name of the subdirectory under which all the year data are stored. Default = circuit name <br><br>Side Effect: Sets the prefix for output files |
| Cfactors | Sets the CFactors for for all loads in the active circuit to the value given. |
| circuit | Set the active circuit by name. |
| Cktmodel | {Multiphase \| Positive}  Default = Multiphase.  Designates whether circuit model is to interpreted as a normal multi-phase model or a positive-sequence only model |
| class | Synonym for Type=. (See above) |
| Controlmode | {OFF \| STATIC \|EVENT \| TIME \| MULTIRATE}  Default is "STATIC".  Control mode for the solution. Set to OFF to prevent controls from changing.<br>STATIC = Time does not advance.  Control actions are executed in order of shortest time to act until all actions are cleared from the control queue.  Use this mode for power flow solutions which may require several regulator tap changes per solution.<br><br>EVENT = solution is event driven.  Only the control actions nearest in time are executed and the time is advanced automatically to the time of the event. <br><br>TIME = solution is time driven.  Control actions are executed when the time for the pending action is reached or surpassed.<br><br>MULTIRATE = solution is time driven.  Control actions are executed when the time for the pending action is reached or surpassed. In this control mode a solution is performed after each control actionis performed to reduce the error accumulated when the time step is to long<br><br>Controls may reset and may choose not to act when it comes their time. <br>Use TIME mode when modeling a control externally to the DSS and a solution mode such as DAILY or DUTYCYCLE that advances time, or set the time (hour and sec) explicitly from the external program.  |
| Daisysize | Default is 1.0. Relative size (a multiplier applied to default size) of daisy circles on daisy plot. |
| Datapath | Set the data path for files written or read by the DSS.<br>Defaults to the user documents folder.<br>If the DataPath is not writable, output files will be written to the user application data folder.<br>May be Null.  Executes a CHDIR to this path if non-null.<br>Does not require a circuit defined. |
| DefaultBaseFrequency | Set Default Base Frequency, Hz. Side effect: Sets solution Frequency and default Circuit Base Frequency. This value is saved when the DSS closes down. |
| Defaultdaily | Default daily load shape name. Default value is "default", which is a 24-hour curve defined when the DSS is started. |
| Defaultyearly | Default yearly load shape name. Default value is "default", which is a 24-hour curve defined when the DSS is started. |
| DemandInterval | {YES/TRUE \| NO/FALSE} Default = no. Set for keeping demand interval data for daily, yearly, etc, simulations. Side Effect:  Resets all meters!!! |
| DIVerbose | {YES/TRUE \| NO/FALSE} Default = FALSE.  Set to Yes/True if you wish a separate demand interval (DI) file written for each meter.  Otherwise, only the totalizing meters are written. |
| DSSVisualizationTool | Activates/Deactivates the extended version of the plot command for figures with the DSS Visualization Tool. |
| EarthModel | One of {Carson \| FullCarson \| Deri\*}.  Default is Deri, which isa  fit to the Full Carson that works well into high frequencies. "Carson" is the simplified Carson method that is typically used for 50/60 Hz power flow programs. Applies only to Line objects that use LineGeometry objects to compute impedances. |
| editor | Set the command string required to start up the editor preferred by the user. Does not require a circuit defined. |
| element | Sets the active DSS element by name. You can use the complete object spec (class.name) or just the name.  if full name is specifed, class becomes the active class, also. |
| emergvmaxpu | Maximum permissible per unit voltage for emergency (contingency) conditions. Default is 1.08. |
| emergvminpu | Minimum permissible per unit voltage for emergency (contingency) conditions. Default is 0.90. |
| frequency | Sets the frequency for the solution of the active circuit. |
| FuseMarkerCode | Numeric marker code (0..47 see Users Manual) for Fuse elements. Default is 25. |
| FuseMarkerSize | Size of Fuse marker. Default is 1. |
| Genkw | Size of generator, kW, to automatically add to system. Default is 1000.0 |
| Genmult | Global multiplier for the kW output of every generator in the circuit. Default is 1.0. Applies to all but Autoadd solution modes. Ignored for generators designated as Status=Fixed. |
| Genpf | Power factor of generator to assume for automatic addition. Default is 1.0. |
| h | Alternate name for time step size. |
| Harmonics | {ALL \| (list of harmonics) }  Default = ALL. Array of harmonics for which to perform a solution in Harmonics mode. If ALL, then solution is performed for all harmonics defined in spectra currently being used. Otherwise, specify a more limited list such as: <br><br>   Set Harmonics=(1 5 7 11 13) |
| hour | Sets the hour used for the start time of the solution. |
| KeepList | Array of bus names to keep when performing circuit reductions. You can specify a text file holding the names, one to a line, by using the syntax (file=filename) instead of the actual array elements. Command is cumulative (reset keeplist first). Reduction algorithm may keep other buses automatically. <br><br>Examples:<br><br>Reset Keeplist (sets all buses to FALSE (no keep))<br>Set KeepList=(bus1, bus2, bus3, ... )<br>Set KeepList=(file=buslist.txt) |
| KeepLoad | Keeploads = Y/N option for ReduceOption Laterals option |
| LDCurve | Set Load-Duration Curve. Global load multiplier is defined by this curve for LD1 and LD2 solution modes. Default is Nil. |
| Loadmodel | {Powerflow \| Admittance} depending on the type of solution you wish to perform. If admittance, a non-iterative, direct solution is done with all loads and generators modeled by their equivalent admittance. |
| Loadmult | Global load multiplier for this circuit.  Does not affect loads designated to be "fixed".  All other base kW values are multiplied by this number. Defaults to 1.0 when the circuit is created. As with other values, it always stays at the last value to which it was set until changed again. |
| LoadShapeClass | ={Daily \| Yearly \| Duty \| None\*} Default loadshape class to use for mode=time and mode=dynamic simulations. Loads and generators, etc., will follow this shape as time is advanced. Default value is None. That is, Load will not vary with time. |
| Log | {YES/TRUE \| NO/FALSE} Default = FALSE.  Significant solution events are added to the Event Log, primarily for debugging. |
| Lossregs | Which EnergyMeter register(s) to use for Losses in AutoAdd Mode. May be one or more registers.  if more than one, register values are summed together. Array of integer values > 0.  Defaults to 13 (for Zone kWh Losses). <br><br>for a list of EnergyMeter register numbers, do the "Show Meters" command after defining a circuit. |
| Lossweight | Weighting factor for Losses in AutoAdd functions.  Defaults to 1.0.<br><br>Autoadd mode minimizes<br><br>(Lossweight \* Losses + UEweight \* UE). <br><br>If you wish to ignore Losses, set to 0. This applies only when there are EnergyMeter objects. Otherwise, AutoAdd mode minimizes total system losses. |
| MarkCapacitors | {YES/TRUE \| NO/FALSE}  Default is NO. Mark Capacitor locations with a symbol. See CapMarkerCode.  |
| Markercode | Number code for node marker on circuit plots. Number from 0 to 47. Default is 16 (open circle). 24 is solid circle. Try other values for other symbols. See also Nodewidth |
| MarkFuses | {YES/TRUE \| NO/FALSE}  Default is NO. Mark Fuse locations with a symbol. See FuseMarkerCode and FuseMarkerSize.  |
| MarkPVSystems | {YES/TRUE \| NO/FALSE}  Default is NO. Mark PVSystem locations with a symbol. See PVMarkerCode and PVMarkerSize.  |
| MarkReclosers | {YES/TRUE \| NO/FALSE}  Default is NO. Mark Recloser locations with a symbol. See RecloserMarkerCode and RecloserMarkerSize.  |
| MarkRegulators | {YES/TRUE \| NO/FALSE}  Default is NO. Mark Regulator locations with a symbol. See RegMarkerCode.  |
| MarkRelays | {YES/TRUE \| NO/FALSE}  Default is NO. Mark Relay locations with a symbol. See RelayMarkerCode and RelayMarkerSize.  |
| MarkStorage | {YES/TRUE \| NO/FALSE}  Default is NO. Mark Storage locations with a symbol. See StoreMarkerCode and StoreMarkerSize.  |
| Markswitches | {YES/TRUE \| NO/FALSE}  Default is NO. Mark lines that are switches or are isolated with a symbol. See SwitchMarkerCode. |
| Marktransformers | {YES/TRUE \| NO/FALSE}  Default is NO. Mark transformer locations with a symbol. See TransMarkerCode. The coordinate of one of the buses for winding 1 or 2 must be defined for the symbol to show |
| Maxcontroliter | Max control iterations per solution.  Default is 10. |
| maxiterations | Sets the maximum allowable iterations for power flow solutions. Default is 15. |
| MinIterations | Minimum number of iterations required for a solution. Default is 2. |
| mode | Set the solution Mode: One of<br>  Snapshot,<br>  Daily,<br>  Yearly (follow Yearly curve),<br>  DIrect,<br>  DUtycycle,<br>  Time, ( see LoadShapeClass, SampleEnergymeters options)<br>  DYnamic,  ( see LoadShapeClass option)<br>  Harmonic,<br>  HarmonicT,  (sequential Harmonic Mode)<br>  M1 (Monte Carlo 1),<br>  M2 (Monte Carlo 2),<br>  M3 (Monte Carlo 3),<br>  Faultstudy,<br>  MF (monte carlo fault study)<br>  Peakday,<br>  LD1 (load-duration 1)<br>  LD2 (load-duration 2)<br>  AutoAdd (see AddType)<br><br>Side effect: setting the Mode propergy resets all monitors and energy meters. It also resets the time step, etc. to defaults for each mode.  After the initial reset, the user must explicitly reset the monitors and/or meters until another Set Mode= command. |
| NeglectLoadY | {YES/TRUE \| NO/FALSE}  Default is NO. For Harmonic solution, neglect the Load shunt admittance branch that can siphon off some of the Load injection current. <br><br>If YES, the current injected from the LOAD at harmonic frequencies will be nearly ideal. |
| Nodewidth | Width of node marker. Default=1. See MarkerCode |
| normvmaxpu | Maximum permissible per unit voltage for normal conditions. Default is 1.05. |
| normvminpu | Minimum permissible per unit voltage for normal conditions. Default is 0.95. |
| Numallociterations | Default is 2. Maximum number of iterations for load allocations for each time the AllocateLoads or Estimate command is given. |
| number | Number of solutions or time steps to perform for each Solve command. Defaults for selected modes: <br><br>Daily = 24<br>Yearly = 8760<br>Duty = 100 |
| object | Synonym for Element=. (See above) |
| Overloadreport | {YES/TRUE \| NO/FALSE} Default = FALSE. For yearly solution mode, sets overload reporting on/off. DemandInterval must be set to true for this to have effect. |
| Pricecurve | Sets the PRICESHAPE object to use to obtain for price signal. Default is none (null string). If none, price signal either remains constant or is set by an external process using Set Price= option. Curve is defined as a PRICESHAPE  in actual values (not normalized) and should be defined to correspond to the type of analysis being performed (daily, yearly, etc.). |
| Pricesignal | Sets the present price signal ($/MWh) for the circuit.  Default value is 25. |
| ProcessTime | The time in microseconds to execute the solve process in the most recent time step or solution (read only) |
| PVMarkerCode | Numeric marker code (0..47 see Users Manual) for PVSystems. Default is 15. |
| PVMarkerSize | Size of PVsystem marker. Default is 1. |
| QueryLog | {YES/TRUE \| NO/FALSE} Default = FALSE. When set to TRUE/YES, clears the query log file and thereafter appends the time-stamped Result string contents to the log file after a query command, ?.  |
| random | One of [Uniform \| Gaussian \| Lognormal \| None ] for Monte Carlo Variables. |
| RecloserMarkerCode | Numeric marker code (0..47 see Users Manual) for Recloser elements. Default is 17. (color=Lime) |
| RecloserMarkerSize | Size of Recloser marker. Default is 5. |
| Recorder | {YES/TRUE \| NO/FALSE} Default = FALSE. Opens DSSRecorder.DSS in DSS install folder and enables recording of all commands that come through the text command interface. Closed by either setting to NO/FALSE or exiting the program. When closed by this command, the file name can be found in the Result. Does not require a circuit defined. |
| ReduceOption | { Default or [null] \| Shortlines [Zmag=nnn] \| MergeParallel \| BreakLoops \| Switches \| Ends \| Laterals}  Strategy for reducing feeders. Default is to eliminate all dangling end buses and buses without load, caps, or taps. <br>"Shortlines [Zmag=0.02]" merges short branches with impedance less than Zmag (default = 0.02 ohms) <br>"MergeParallel" merges lines that have been found to be in parallel <br>"Breakloops" disables one of the lines at the head of a loop. <br>"Ends" eliminates dangling ends only.<br>"Switches" merges switches with downline lines and eliminates dangling switches.<br>"Laterals [Keepload=Yes\*/No]" uses the Remove command to eliminate 1-phase laterals and optionally lump the load back to the 2- or 3-phase feeder (default behavior). <br><br>Marking buses with "Keeplist" will prevent their elimination. |
| RegistryUpdate | {YES/TRUE \| NO/FALSE}  Default is Yes. Update Windows Registry values upon exiting.  You might want to turn this off if you temporarily change fonts or DefaultBaseFrequency, for example.  |
| RegMarkerCode | Numeric marker code (0..47 see Users Manual) for Regulators. Default is 17. (red) |
| RegMarkerSize | Size of Regulator marker. Default is 5. |
| RelayMarkerCode | Numeric marker code (0..47 see Users Manual) for Relay elements. Default is 17. (Color=Lime) |
| RelayMarkerSize | Size of Relay marker. Default is 5. |
| SampleEnergyMeters | {YES/TRUE \| NO/FALSE} Overrides default value for sampling EnergyMeter objects at the end of the solution loop. Normally Time and Duty modes do not automatically sample EnergyMeters whereas Daily, Yearly, M1, M2, M3, LD1 and LD2 modes do. Use this Option to turn sampling on or off |
| SeasonRating | Enables/disables the seasonal selection of the rating for determining if an element is overloaded. When enabled, the energy meter will<br>look for the rating (NormAmps) using the SeasonSignal to eavluate if the element is overloaded |
| SeasonSignal | Is the name of the XY curve defining the seasonal change when performing QSTS simulations. |
| sec | Sets the seconds from the hour for the start time of the solution. |
| Showexport | {YES/TRUE \| NO/FALSE} Default = FALSE. If YES/TRUE will automatically show the results of an Export Command after it is written. |
| stepsize | Sets the time step size for the active circuit.  Default units are s. May also be specified in minutes or hours by appending "m" or "h" to the value. For example:<br><br>   stepsize=.25h <br>  stepsize=15m<br>  stepsize=900s |
| StepTime | Process time + meter sampling time in microseconds for most recent time step - (read only) |
| StoreMarkerCode | Numeric marker code (0..47 see Users Manual) for Storage elements. Default is 9. |
| StoreMarkerSize | Size of Storage marker. Default is 1. |
| Switchmarkercode | Numeric marker code for lines with switches or are isolated from the circuit. Default is 4. See markswitches option. |
| Terminal | Set the active terminal of the active circuit element. May also be done with Select command. |
| time | Specify the solution start time as an array:<br>time=(hour, secs) |
| tolerance | Sets the solution tolerance.  Default is 0.0001. |
| TotalTime | The accumulated time in microseconds to solve the circuit since the last reset. Set this value to reset the accumulator. |
| Tracecontrol | {YES/TRUE \| NO/FALSE}  Set to YES to trace the actions taken in the control queue.  Creates a file named TRACE_CONTROLQUEUE.CSV in the default directory. The names of all circuit elements taking an action are logged. |
| TransMarkerCode | Numeric marker code (0..47 see Users Manual) for transformers. Default is 35. See markstransformers option. |
| TransMarkerSize | Size of transformer marker. Default is 1. |
| Trapezoidal | {YES/TRUE \| NO/FALSE}  Default is "No/False". Specifies whether to use trapezoidal integration for accumulating energy meter registers. Applies to EnergyMeter and Generator objects.  Default method simply multiplies the present value of the registers times the width of the interval (Euler). Trapezoidal is more accurate when there are sharp changes in a load shape or unequal intervals. Trapezoidal is automatically used for some load-duration curve simulations where the interval size varies considerably. Keep in mind that for Trapezoidal, you have to solve one more point than the number of intervals. That is, to do a Daily simulation on a 24-hr load shape, you would set Number=25 to force a solution at the first point again to establish the last (24th) interval.<br><br>Note: Set Mode= resets Trapezoidal to No/False. Set this to Yes/True AFTER setting the Mode option. |
| type | Sets the active DSS class type.  Same as Class=... |
| UEregs | Which EnergyMeter register(s) to use for UE in AutoAdd Mode. May be one or more registers.  if more than one, register values are summed together. Array of integer values > 0.  Defaults to 11 (for Load EEN). <br><br>for a list of EnergyMeter register numbers, do the "Show Meters" command after defining a circuit. |
| UEweight | Weighting factor for UE/EEN in AutoAdd functions.  Defaults to 1.0.<br><br>Autoadd mode minimizes<br><br>(Lossweight \* Losses + UEweight \* UE). <br><br>If you wish to ignore UE, set to 0. This applies only when there are EnergyMeter objects. Otherwise, AutoAdd mode minimizes total system losses. |
| Voltagebases | Define legal bus voltage bases for this circuit.  Enter an array of the legal voltage bases, in phase-to-phase voltages, for example:<br><br>set voltagebases=".208, .480, 12.47, 24.9, 34.5, 115.0, 230.0" <br><br>When the CalcVoltageBases command is issued, a snapshot solution is performed with no load injections and the bus base voltage is set to the nearest legal voltage base. The defaults are as shown in the example above. |
| Voltexceptionreport | {YES/TRUE \| NO/FALSE} Default = FALSE. For yearly solution mode, sets voltage exception reporting on/off. DemandInterval must be set to true for this to have effect. |
| year | Sets the Year (integer number) to be used for the solution. for certain solution types, this determines the growth multiplier. |
| Zmag | Sets the Zmag option (in Ohms) for ReduceOption Shortlines option. Lines have less line mode impedance are reduced. |
| Zonelock | {YES/TRUE \| NO/FALSE}  Default is No. if No, then meter zones are recomputed each time there is a change in the circuit. If Yes, then meter zones are not recomputed unless they have not yet been computed. Meter zones are normally recomputed on Solve command following a circuit change. |

---
## `Show` options

| Option | Description |
| - | - |
| autoadded | Shows auto added capacitors or generators. See AutoAdd solution mode. |
| buses | Report showing all buses and nodes currently defined. |
| busflow | Creates a report showing power and current flows as well as voltages around a selected bus. Syntax:<br><br>Show BUSFlow busname [MVA\|kVA\*] [Seq\* \| Elements]<br><br>Show busflow busxxx kVA elem<br>Show busflow busxxx MVA seq<br><br>NOTE: The Show menu will prompt you for these values. |
| Controlled | Show Controlled elements and the names of the controls connected to them in CSV format. |
| controlqueue | Shows the present contents of the control queue. |
| convergence | Report on the convergence of each node voltage. |
| currents | Report showing currents from most recent solution. syntax: <br><br>Show Currents  [[residual=]yes\|no\*] [Seq\* \| Elements]<br><br>If "residual" flag is yes, the sum of currents in all conductors is reported. Default is to report Sequence currents; otherwise currents in all conductors are reported. |
| deltaV | Show voltages ACROSS each 2-terminal element, phase-by-phase.  |
| elements | Shows names of all elements in circuit or all elements of a specified class. Syntax: <br><br>Show ELements [Classname] <br><br>Useful for creating scripts that act on selected classes of elements.  |
| eventlog | Shows the present event log. (Regulator tap changes, capacitor switching, etc.) |
| faults | After fault study solution, shows fault currents. |
| generators | Report showing generator elements currently defined and the values of the energy meters <br>associated with each generator. |
| isolated | Report showing buses and elements that are isolated from the main source. |
| kvbasemismatch | Creates a report of Load and Generator elements for which the base voltage does not match the Bus base voltage. Scripts for correcting the voltage base are suggested. |
| lineconstants | Creates two report files for the line constants (impedances) of every LINEGEOMETRY element currently defined. One file shows the main report with the matrices. The other file contains corresponding LINECODE definitions that you may use in subsequent simulations.  Syntax:<br><br>Show LIneConstants [frequency] [none\|mi\|km\|kft\|m\|me\|ft\|in\|cm] [rho]<br><br>Specify the frequency, length units and earth resistivity (meter-ohms). Examples:<br><br>Show Lineconstants 60 kft 100<br>Show Linecon 50 km 1000 |
| loops | Shows closed loops detected by EnergyMeter elements that are possibly unwanted. Otherwise, loops are OK. |
| losses | Reports losses in each element and in the entire circuit. |
| meters | Shows the present values of the registers in the EnergyMeter elements. |
| mismatch | Shows the current mismatches at each node in amperes and percent of max currents at node. |
| monitor | Shows the contents of a selected monitor. Syntax: <br><br> Show Monitor  monitorname |
| overloads | Shows overloaded power delivery elements. |
| panel | Shows control panel. (not necessary for standalone version) |
| powers | Report on powers flowing in circuit from most recent solution. <br>Powers may be reported in kVA or MVA and in sequence quantities or in every conductor of each element. Syntax:<br><br>Show Powers [MVA\|kVA\*] [Seq\* \| Elements]<br><br>Sequence powers in kVA is the default. Examples:<br><br>Show powers<br>Show power kva element<br>Show power mva elem |
| QueryLog | Show Query Log file.  |
| ratings | Shows ratings of power delivery elements. |
| Result | Show last result (in @result variable). |
| taps | Shows the regulator/LTC taps from the most recent solution. |
| topology | Shows the topology as seen by the SwtControl elements. |
| unserved | Shows loads that are "unserved". That is, loads for which the voltage is too low, or a branch on the source side is overloaded. If UEonly is specified, shows only those loads in which the emergency rating has been exceeded. Syntax:<br><br>Show Unserved [UEonly] (unserved loads) |
| variables | Shows internal state variables of devices (Power conversion elements) that report them. |
| voltages | Reports voltages from most recent solution. Voltages are reported with respect to <br>system reference (Node 0) by default (LN option), but may also be reported Line-Line (LL option).<br>The voltages are normally reported by bus/node, but may also be reported by circuit element. Syntax:<br><br>Show Voltages [LL \|LN\*]  [Seq\* \| Nodes \| Elements]<br><br>Show Voltages<br>Show Voltage LN Nodes<br>Show Voltages LL Nodes<br>Show Voltage LN Elem |
| y | Show the system Y matrix. Could be a large file! |
| yprim | Show the primitive admittance (y) matrix for the active element. |
| zone | Shows the zone for a selected EnergyMeter element. Shows zone either in a text file or in a graphical tree view.<br><br>Show Zone  energymetername [Treeview] |

---
## `Export` options

| Option | Description |
| - | - |
| AllocationFactors | Exports load allocation factors. File name is assigned. |
| BranchReliability | (Default file = EXP_BranchReliability.CSV) Failure rate, number of interruptions and other reliability data for each PD element. |
| Buscoords | [Default file = EXP_BUSCOORDS.CSV] Bus coordinates in csv form. |
| BusLevels | Exports the names and the level of each Bus inside the Circuit based on its topology information. The level value defineshow far or close is the bus from the circuits backbone (0 means that the bus is at the backbone) |
| BusReliability | (Default file = EXP_BusReliability.CSV) Failure rate, number of interruptions and other reliability data at each bus. |
| Capacity | (Default file = EXP_CAPACITY.CSV) Capacity report. |
| CDPSMAsset | \*\* Deprecated \*\* (IEC 61968-13, CDPSM Asset profile) |
| CDPSMElec | \*\* Deprecated \*\* (IEC 61968-13, CDPSM Electrical Properties profile) |
| CDPSMGeo | \*\* Deprecated \*\* (IEC 61968-13, CDPSM Geographical profile) |
| CDPSMStateVar | \*\* Deprecated \*\* (IEC 61968-13, CDPSM State Variables profile) |
| CDPSMTopo | \*\* Deprecated \*\* (IEC 61968-13, CDPSM Topology profile) |
| CIM100 | (Default file = CIM100x.XML) (IEC 61968-13, combined CIM100 for unbalanced load flow profile)<br> [File=filename fid=_uuidstring Substation=subname sid=_uuidstring<br> SubGeographicRegion=subgeoname sgrid=_uuidstring GeographicRegion=geoname rgnid=_uuidstring] |
| CIM100Fragments | (Default file ROOT = CIM100) (IEC 61968-13, CIM100 for unbalanced load flow profile)<br> produces 6 separate files ROOT_FUN.XML for Functional profile,<br> ROOT_EP.XML for Electrical Properties profile,<br> ROOT_TOPO.XML for Topology profile,<br> ROOT_CAT.XML for Asset Catalog profile,<br> ROOT_GEO.XML for Geographical profile and<br> ROOT_SSH.XML for Steady State Hypothesis profile<br> [File=fileroot fid=_uuidstring Substation=subname sid=_uuidstring<br> SubGeographicRegion=subgeoname sgrid=_uuidstring GeographicRegion=geoname rgnid=_uuidstring] |
| Contours | Exports the Contours matrix (C) calculated after initilizing A-Diakoptics. The output format is compressed coordianted and the values are integers.  If A-Diakoptics is not initialized this command does nothing |
| Counts | [Default file = EXP_Counts.CSV] (instance counts for each class) |
| Currents | (Default file = EXP_CURRENTS.CSV) Currents in each conductor of each element. |
| ElemCurrents | (Default file = EXP_ElemCurrents.CSV)  Exports the current into all conductors of all circuit elements |
| ElemPowers | (Default file = EXP_elemPowers.CSV)  Exports the powers into all conductors of all circuit elements |
| ElemVoltages | (Default file = EXP_ElemVoltages.CSV)  Exports the voltages to ground at all conductors of all circuit elements |
| ErrorLog | (Default file = EXP_ErrorLog.TXT) All entries in the present Error log. |
| Estimation | (Default file = EXP_ESTIMATION.CSV) Results of last estimation. |
| EventLog | (Default file = EXP_EventLog.CSV) All entries in the present event log. |
| Faultstudy | (Default file = EXP_FAULTS.CSV) results of a fault study. |
| Generators | (Default file = EXP_GENMETERS.CSV) Present values of generator meters. Adding the switch "/multiple" or "/m" will  cause a separate file to be written for each generator. |
| GICMvars | (Default file = EXP_GIC_Mvar.CSV) Mvar for each GICtransformer object by bus for export to power flow programs  |
| IncMatrix | Exports the Branch-to-Node Incidence matrix calculated for the circuit in compressed coordianted format (Row,Col,Value) |
| IncMatrixCols | Exports the names of the Cols (Buses) used for calculating the Branch-to-Node Incidence matrix for the active circuit |
| IncMatrixRows | Exports the names of the rows (PDElements) used for calculating the Branch-to-Node Incidence matrix for the active circuit |
| Laplacian | Exports the Laplacian matrix calculated using the branch-to-node Incidence matrix in compressed coordinated format (Row,Col,Value) |
| Loads | (Default file = EXP_LOADS.CSV) Report on loads from most recent solution. |
| Losses | [Default file = EXP_LOSSES.CSV] Losses for each element. |
| Meters | (Default file = EXP_METERS.CSV) Energy meter exports. Adding the switch "/multiple" or "/m" will  cause a separate file to be written for each meter. |
| Monitors | (file name is assigned by Monitor export) Monitor values. The argument is the name of the monitor (e.g. Export Monitor XYZ, XYZ is the name of the monitor).<br>The argument can be ALL, which means that all the monitors will be exported |
| NodeNames | (Default file = EXP_NodeNames.CSV) Exports Single-column file of all node names in the active circuit. Useful for making scripts. |
| NodeOrder | (Default file = EXP_NodeOrder.CSV)  Exports the present node order for all conductors of all circuit elements |
| Overloads | (Default file = EXP_OVERLOADS.CSV) Overloaded elements report. |
| P_byphase | (Default file = EXP_P_BYPHASE.CSV) [MVA] [Filename] Power by phase. Default is kVA. |
| Powers | (Default file = EXP_POWERS.CSV) [MVA] [Filename] Powers (kVA by default) into each terminal of each element. |
| Profile | [Default file = EXP_Profile.CSV] Coordinates, color of each line section in Profile plot. Same options as Plot Profile Phases property.<br><br>Example:  Export Profile Phases=All [optional file name] |
| PVSystem_Meters | (Default file = EXP_PVMETERS.CSV) Present values of PVSystem meters. Adding the switch "/multiple" or "/m" will  cause a separate file to be written for each PVSystem. |
| Result | (Default file = EXP_Result.CSV)  Exports the result of the most recent command. |
| Sections | (Default file = EXP_SECTIONS.CSV) Data for each section between overcurrent protection devices. <br><br>Examples: <br>  Export Sections [optional filename]<br>Export Sections meter=M1 [optional filename] |
| SeqCurrents | (Default file = EXP_SEQCURRENTS.CSV) Sequence currents in each terminal of 3-phase elements. |
| SeqPowers | (Default file = EXP_SEQPOWERS.CSV) Sequence powers into each terminal of 3-phase elements. |
| SeqVoltages | (Default file = EXP_SEQVOLTAGES.CSV) Sequence voltages. |
| seqz | (Default file = EXP_SEQZ.CSV) Equivalent sequence Z1, Z0 to each bus. |
| Storage_Meters | (Default file = EXP_STORAGEMETERS.CSV) Present values of Storage meters. Adding the switch "/multiple" or "/m" will  cause a separate file to be written for each Storage device. |
| Summary | [Default file = EXP_Summary.CSV] Solution summary. |
| Taps | (Default file = EXP_Taps.CSV)  Exports the regulator tap report similar to Show Taps. |
| Unserved | (Default file = EXP_UNSERVED.CSV) [UEonly] [Filename] Report on elements that are unserved due to violation of ratings. |
| Uuids | [Default file = EXP_UUIDS.CSV] Uuids for each element. This frees the UUID list after export. |
| Voltages | (Default file = EXP_VOLTAGES.CSV) Voltages to ground by bus/node. |
| VoltagesElements | (Default file = EXP_VOLTAGES_ELEM.CSV) Voltages to ground by circuit element. |
| Y | (Default file = EXP_Y.CSV) [triplets] [Filename] System Y matrix, defaults to non-sparse format. |
| Y4 | Exports the inverse of Z4 (ZCC) calculated after initilizing A-Diakoptics. The output format is compressed coordianted and the values are complex conjugates.  If A-Diakoptics is not initialized this command does nothing |
| YCurrents | (Default file = EXP_YCurrents.CSV)  Exports the present solution complex Current array in same order as YNodeList. This is generally the injection current array |
| YNodeList | (Default file = EXP_YNodeList.CSV)  Exports a list of nodes in the same order as the System Y matrix. |
| Yprims | (Default file = EXP_YPRIMS.CSV) All primitive Y matrices. |
| YVoltages | (Default file = EXP_YVoltages.CSV)  Exports the present solution complex Voltage array in same order as YNodeList. |
| ZCC | Exports the connectivity matrix (ZCC) calculated after initilizing A-Diakoptics. The output format is compressed coordianted and the values are complex conjugates.  If A-Diakoptics is not initialized this command does nothing |
| ZLL | Exports the Link branches matrix (ZLL) calculated after initilizing A-Diakoptics. The output format is compressed coordianted and the values are complex conjugates. If A-Diakoptics is not initialized this command does nothing |

---
## Elements

---
### Power Delivery Elements

#### `AutoTrans` properties

| Number | Name | Description |
| - | - | - |
| 1 | phases | Number of phases this AutoTransformer. Default is 3. |
| 2 | windings | Number of windings, this AutoTransformer. (Also is the number of terminals) Default is 2. This property triggers memory allocation for the AutoTrans and will cause other properties to revert to default values. |
| 3 | wdg | Set this = to the number of the winding you wish to define.  Then set the values for this winding.  Winding 1 is always the Series winding. Winding 2 is always Common winding (wye connected). Repeat for each winding.  Alternatively, use the array collections (buses, kVAs, etc.) to define the windings.  Note: reactances are BETWEEN pairs of windings; they are not the property of a single winding. |
| 4 | bus | Bus connection spec for this winding. |
| 5 | conn | Connection of this winding {Series, wye\*, Delta, LN, LL }. Default is "wye" with the neutral solidly grounded. <br>For AutoTrans, Winding 1 is always Series and Winding 2 (the Common winding) is always Wye. <br>If only 2 windings, no need to specify connections. |
| 6 | kV | For 2-or 3-phase, enter phase-phase kV rating.  Otherwise, kV rating of the actual winding. Specify H terminal kV rating for Series winding. |
| 7 | kVA | Base kVA rating of the winding. Side effect: forces change of max normal and emerg kVA ratings.If 2-winding AutoTrans, forces other winding to same value. When winding 1 is defined, all other windings are defaulted to the same rating and the first two winding resistances are defaulted to the %loadloss value. |
| 8 | tap | Per unit tap that this winding is on. |
| 9 | %R | Percent ac resistance this winding.  This value is for the power flow model.Is derived from the full load losses in the transformer test report. |
| 10 | Rdcohms | Winding dc resistance in OHMS. Specify this for GIC analysis. From transformer test report (divide by number of phases). Defaults to 85% of %R property (the ac value that includes stray losses). |
| 11 | Core | {Shell\*\|5-leg\|3-Leg\|1-phase} Core Type. Used for GIC analysis in auxiliary programs. Not used inside OpenDSS. |
| 12 | buses | Use this to specify all the bus connections at once using an array. Example:<br><br>New AutoTrans.T1 buses=[Hbus, Xbus] |
| 13 | conns | Use this to specify all the Winding connections at once using an array. Example:<br><br>New AutoTrans.T1 buses=[Hbus, Xbus] \~ conns=(series, wye) |
| 14 | kVs | Use this to specify the kV ratings of all windings at once using an array. Example:<br><br>New AutoTrans.T1 buses=[Hbus, Xbus] <br>\~ conns=(series, wye)<br>\~ kvs=(115, 12.47)<br><br>See kV= property for voltage rules. |
| 15 | kVAs | Use this to specify the kVA ratings of all windings at once using an array. |
| 16 | taps | Use this to specify the p.u. tap of all windings at once using an array. |
| 17 | XHX | Use this to specify the percent reactance, H-L (winding 1 to winding 2).  Use for 2- or 3-winding AutoTranss. On the kVA base of winding 1(H-X). See also X12. |
| 18 | XHT | Use this to specify the percent reactance, H-T (winding 1 to winding 3).  Use for 3-winding AutoTranss only. On the kVA base of winding 1(H-X). See also X13. |
| 19 | XXT | Use this to specify the percent reactance, L-T (winding 2 to winding 3).  Use for 3-winding AutoTranss only. On the kVA base of winding 1(H-X).  See also X23. |
| 20 | XSCarray | Use this to specify the percent reactance between all pairs of windings as an array. All values are on the kVA base of winding 1.  The order of the values is as follows:<br><br>(x12 13 14... 23 24.. 34 ..)  <br><br>There will be n(n-1)/2 values, where n=number of windings. |
| 21 | thermal | Thermal time constant of the AutoTrans in hours.  Typically about 2. |
| 22 | n | n Exponent for thermal properties in IEEE C57.  Typically 0.8. |
| 23 | m | m Exponent for thermal properties in IEEE C57.  Typically 0.9 - 1.0 |
| 24 | flrise | Temperature rise, deg C, for full load.  Default is 65. |
| 25 | hsrise | Hot spot temperature rise, deg C.  Default is 15. |
| 26 | %loadloss | Percent load loss at full load. The %R of the High and Low windings (1 and 2) are adjusted to agree at rated kVA loading. |
| 27 | %noloadloss | Percent no load losses at rated excitatation voltage. Default is 0. Converts to a resistance in parallel with the magnetizing impedance in each winding. |
| 28 | normhkVA | Normal maximum kVA rating of H winding (winding 1+2).  Usually 100% - 110% ofmaximum nameplate rating, depending on load shape. Defaults to 110% of kVA rating of Winding 1. |
| 29 | emerghkVA | Emergency (contingency)  kVA rating of H winding (winding 1+2).  Usually 140% - 150% ofmaximum nameplate rating, depending on load shape. Defaults to 150% of kVA rating of Winding 1. |
| 30 | sub | ={Yes\|No}  Designates whether this AutoTrans is to be considered a substation.Default is No. |
| 31 | MaxTap | Max per unit tap for the active winding.  Default is 1.10 |
| 32 | MinTap | Min per unit tap for the active winding.  Default is 0.90 |
| 33 | NumTaps | Total number of taps between min and max tap.  Default is 32 (16 raise and 16 lower taps about the neutral position). The neutral position is not counted. |
| 34 | subname | Substation Name. Optional. Default is null. If specified, printed on plots |
| 35 | %imag | Percent magnetizing current. Default=0.0. Magnetizing branch is in parallel with windings in each phase. Also, see "ppm_antifloat". |
| 36 | ppm_antifloat | Default=1 ppm.  Parts per million of AutoTrans winding VA rating connected to ground to protect against accidentally floating a winding without a reference. If positive then the effect is adding a very large reactance to ground.  If negative, then a capacitor. |
| 37 | %Rs | Use this property to specify all the winding ac %resistances using an array. Example:<br><br>New AutoTrans.T1 buses=[Hibus, lowbus] \~ %Rs=(0.2  0.3) |
| 38 | bank | Name of the bank this transformer is part of, for CIM, MultiSpeak, and other interfaces. |
| 39 | XfmrCode | Name of a library entry for transformer properties. The named XfmrCode must already be defined. |
| 40 | XRConst | ={Yes\|No} Default is NO. Signifies whether or not the X/R is assumed contant for harmonic studies. |
| 41 | LeadLag | {Lead \| Lag (default) \| ANSI (default) \| Euro } Designation in mixed Delta-wye connections the relationship between HV to LV winding. Default is ANSI 30 deg lag, e.g., Dy1 of Yd1 vector group. To get typical European Dy11 connection, specify either "lead" or "Euro" |
| 42 | WdgCurrents | (Read only) Makes winding currents available via return on query (? AutoTrans.TX.WdgCurrents). Order: Phase 1, Wdg 1, Wdg 2, ..., Phase 2 ... |
| 43 | normamps | Normal rated current. |
| 44 | emergamps | Maximum or emerg current. |
| 45 | faultrate | Failure rate per year. |
| 46 | pctperm | Percent of failures that become permanent. |
| 47 | repair | Hours to repair. |
| 48 | basefreq | Base Frequency for ratings. |
| 49 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 50 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `Capacitor` properties

| Number | Name | Description |
| - | - | - |
| 1 | bus1 | Name of first bus of 2-terminal capacitor. Examples:<br>bus1=busname<br>bus1=busname.1.2.3<br><br>If only one bus specified, Bus2 will default to this bus, Node 0, and the capacitor will be a Yg shunt bank. |
| 2 | bus2 | Name of 2nd bus. Defaults to all phases connected to first bus, node 0, (Shunt Wye Connection) except when Bus2 explicitly specified. <br><br>Not necessary to specify for delta (LL) connection. |
| 3 | phases | Number of phases. |
| 4 | kvar | Total kvar, if one step, or ARRAY of kvar ratings for each step.  Evenly divided among phases. See rules for NUMSTEPS. |
| 5 | kv | For 2, 3-phase, kV phase-phase. Otherwise specify actual can rating. |
| 6 | conn | ={wye \| delta \|LN \|LL}  Default is wye, which is equivalent to LN |
| 7 | cmatrix | Nodal cap. matrix, lower triangle, microfarads, of the following form:<br><br>cmatrix="c11 \| -c21 c22 \| -c31 -c32 c33"<br><br>All steps are assumed the same if this property is used. |
| 8 | cuf | ARRAY of Capacitance, each phase, for each step, microfarads.<br>See Rules for NumSteps. |
| 9 | R | ARRAY of series resistance in each phase (line), ohms. Default is 0.0 |
| 10 | XL | ARRAY of series inductive reactance(s) in each phase (line) for filter, ohms at base frequency. Use this OR "h" property to define filter. Default is 0.0. |
| 11 | Harm | ARRAY of harmonics to which each step is tuned. Zero is interpreted as meaning zero reactance (no filter). Default is zero. |
| 12 | Numsteps | Number of steps in this capacitor bank. Default = 1. Forces reallocation of the capacitance, reactor, and states array.  Rules: If this property was previously =1, the value in the kvar property is divided equally among the steps. The kvar property does not need to be reset if that is accurate.  If the Cuf or Cmatrix property was used previously, all steps are set to the value of the first step. The states property is set to all steps on. All filter steps are set to the same harmonic. If this property was previously >1, the arrays are reallocated, but no values are altered. You must SUBSEQUENTLY assign all array properties. |
| 13 | states | ARRAY of integers {1\|0} states representing the state of each step (on\|off). Defaults to 1 when reallocated (on). Capcontrol will modify this array as it turns steps on or off. |
| 14 | normamps | Normal rated current. |
| 15 | emergamps | Maximum or emerg current. |
| 16 | faultrate | Failure rate per year. |
| 17 | pctperm | Percent of failures that become permanent. |
| 18 | repair | Hours to repair. |
| 19 | basefreq | Base Frequency for ratings. |
| 20 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 21 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `GICTransformer` properties

| Number | Name | Description |
| - | - | - |
| 1 | BusH | Name of High-side(H) bus. Examples:<br>BusH=busname<br>BusH=busname.1.2.3 |
| 2 | BusNH | Name of Neutral bus for H, or first, winding. Defaults to all phases connected to H-side bus, node 0, if not specified and transformer type is either GSU or YY. (Shunt Wye Connection to ground reference)For Auto, this is automatically set to the X bus. |
| 3 | BusX | Name of Low-side(X) bus, if type=Auto or YY.  |
| 4 | BusNX | Name of Neutral bus for X, or Second, winding. Defaults to all phases connected to X-side bus, node 0, if not specified. (Shunt Wye Connection to ground reference) |
| 5 | phases | Number of Phases. Default is 3. |
| 6 | Type | Type of transformer: {GSU\* \| Auto \| YY}. Default is GSU. |
| 7 | R1 | Resistance, each phase, ohms for H winding, (Series winding, if Auto). Default is 0.0001. If  |
| 8 | R2 | Resistance, each phase, ohms for X winding, (Common winding, if Auto). Default is 0.0001.  |
| 9 | KVLL1 | Optional. kV LL rating for H winding (winding 1). Default is 500. Required if you are going to export vars for power flow analysis or enter winding resistances in percent. |
| 10 | KVLL2 | Optional. kV LL rating for X winding (winding 2). Default is 138. Required if you are going to export vars for power flow analysis or enter winding resistances in percent.. |
| 11 | MVA | Optional. MVA Rating assumed Transformer. Default is 100. Used for computing vars due to GIC and winding resistances if kV and MVA ratings are specified. |
| 12 | VarCurve | Optional. XYCurve object name. Curve is expected as TOTAL pu vars vs pu GIC amps/phase. Vars are in pu of the MVA property. No Default value. Required only if you are going to export vars for power flow analysis. See K property. |
| 13 | %R1 | Optional. Percent Resistance, each phase, for H winding (1), (Series winding, if Auto). Default is 0.2. <br><br>Alternative way to enter R1 value. It is the actual resistances in ohmns that matter. MVA and kV should be specified. |
| 14 | %R2 | Optional. Percent Resistance, each phase, for X winding (2), (Common winding, if Auto). Default is 0.2. <br><br>Alternative way to enter R2 value. It is the actual resistances in ohms that matter. MVA and kV should be specified. |
| 15 | K | Mvar K factor. Default way to convert GIC Amps in H winding (winding 1) to Mvar. Default is 2.2. Commonly-used simple multiplier for estimating Mvar losses for power flow analysis. <br><br>Mvar = K \* kvLL \* GIC per phase / 1000 <br><br>Mutually exclusive with using the VarCurve property and pu curves.If you specify this (default), VarCurve is ignored. |
| 16 | normamps | Normal rated current. |
| 17 | emergamps | Maximum or emerg current. |
| 18 | faultrate | Failure rate per year. |
| 19 | pctperm | Percent of failures that become permanent. |
| 20 | repair | Hours to repair. |
| 21 | basefreq | Base Frequency for ratings. |
| 22 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 23 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `Line` properties

| Number | Name | Description |
| - | - | - |
| 1 | bus1 | Name of bus to which first terminal is connected.<br>Example:<br>bus1=busname   (assumes all terminals connected in normal phase order)<br>bus1=busname.3.1.2.0 (specify terminal to node connections explicitly) |
| 2 | bus2 | Name of bus to which 2nd terminal is connected. |
| 3 | linecode | Name of linecode object describing line impedances.<br>If you use a line code, you do not need to specify the impedances here. The line code must have been PREVIOUSLY defined. The values specified last will prevail over those specified earlier (left-to-right sequence of properties).  You can subsequently change the number of phases if symmetrical component quantities are specified.If no line code or impedance data are specified, the line object defaults to 336 MCM ACSR on 4 ft spacing. |
| 4 | length | Length of line. Default is 1.0. If units do not match the impedance data, specify "units" property.  |
| 5 | phases | Number of phases, this line. |
| 6 | r1 | Positive-sequence Resistance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces the program to use the symmetrical component line definition. See also Rmatrix. |
| 7 | x1 | Positive-sequence Reactance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces the program to use the symmetrical component line definition.  See also Xmatrix |
| 8 | r0 | Zero-sequence Resistance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces the program to use the symmetrical component line definition. |
| 9 | x0 | Zero-sequence Reactance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces the program to use the symmetrical component line definition. |
| 10 | C1 | Positive-sequence capacitance, nf per unit length.  Setting any of R1, R0, X1, X0, C1, C0 forces the program to use the symmetrical component line definition. See also Cmatrix and B1. |
| 11 | C0 | Zero-sequence capacitance, nf per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces the program to use the symmetrical component line definition.See also B0. |
| 12 | rmatrix | Resistance matrix, lower triangle, ohms per unit length. Order of the matrix is the number of phases. May be used to specify the impedance of any line configuration. Using any of Rmatrix, Xmatrix, Cmatrix forces program to use the matrix values for line impedance definition. For balanced line models, you may use the standard symmetrical component data definition instead. |
| 13 | xmatrix | Reactance matrix, lower triangle, ohms per unit length. Order of the matrix is the number of phases. May be used to specify the impedance of any line configuration. Using any of Rmatrix, Xmatrix, Cmatrix forces program to use the matrix values for line impedance definition.  For balanced line models, you may use the standard symmetrical component data definition instead. |
| 14 | cmatrix | Nodal Capacitance matrix, lower triangle, nf per unit length.Order of the matrix is the number of phases. May be used to specify the shunt capacitance of any line configuration. Using any of Rmatrix, Xmatrix, Cmatrix forces program to use the matrix values for line impedance definition.  For balanced line models, you may use the standard symmetrical component data definition instead. |
| 15 | Switch | {y/n \| T/F}  Default= no/false.  Designates this line as a switch for graphics and algorithmic purposes. <br>SIDE EFFECT: Sets r1 = 1.0; x1 = 1.0; r0 = 1.0; x0 = 1.0; c1 = 1.1 ; c0 = 1.0;  length = 0.001; You must reset if you want something different. |
| 16 | Rg | Carson earth return resistance per unit length used to compute impedance values at base frequency. Default is 0.01805 = 60 Hz value in ohms per kft (matches default line impedances). This value is required for harmonic solutions if you wish to adjust the earth return impedances for frequency. If not, set both Rg and Xg = 0. |
| 17 | Xg | Carson earth return reactance per unit length used to compute impedance values at base frequency.  For making better frequency adjustments. Default is 0.155081 = 60 Hz value in ohms per kft (matches default line impedances). This value is required for harmonic solutions if you wish to adjust the earth return impedances for frequency. If not, set both Rg and Xg = 0. |
| 18 | rho | Default=100 meter ohms.  Earth resitivity used to compute earth correction factor. Overrides Line geometry definition if specified. |
| 19 | geometry | Geometry code for LineGeometry Object. Supercedes any previous definition of line impedance. Line constants are computed for each frequency change or rho change. CAUTION: may alter number of phases. You cannot subsequently change the number of phases unless you change how the line impedance is defined. |
| 20 | units | Length Units = {none \| mi\|kft\|km\|m\|Ft\|in\|cm } Default is None - assumes length units match impedance units. |
| 21 | spacing | Reference to a LineSpacing for use in a line constants calculation.<br>Must be used in conjunction with the Wires property.<br>Specify this before the wires property. |
| 22 | wires | Array of WireData names for use in an overhead line constants calculation.<br>Must be used in conjunction with the Spacing property.<br>Specify the Spacing first, and "ncond" wires.<br>May also be used to specify bare neutrals with cables, using "ncond-nphase" wires. |
| 23 | EarthModel | One of {Carson \| FullCarson \| Deri}. Default is the global value established with the Set EarthModel command. See the Options Help on EarthModel option. This is used to override the global value for this line. This option applies only when the "geometry" property is used. |
| 24 | cncables | Array of CNData names for use in a cable constants calculation.<br>Must be used in conjunction with the Spacing property.<br>Specify the Spacing first, using "nphases" cncables.<br>You may later specify "nconds-nphases" wires for separate neutrals |
| 25 | tscables | Array of TSData names for use in a cable constants calculation.<br>Must be used in conjunction with the Spacing property.<br>Specify the Spacing first, using "nphases" tscables.<br>You may later specify "nconds-nphases" wires for separate neutrals |
| 26 | B1 | Alternate way to specify C1. MicroS per unit length |
| 27 | B0 | Alternate way to specify C0. MicroS per unit length |
| 28 | Seasons | Defines the number of ratings to be defined for the wire, to be used only when defining seasonal ratings using the "Ratings" property. |
| 29 | Ratings | An array of ratings to be used when the seasonal ratings flag is True. It can be used to insert<br>multiple ratings to change during a QSTS simulation to evaluate different ratings in lines. |
| 30 | LineType | Code designating the type of line. <br>One of: OH, UG, UG_TS, UG_CN, SWT_LDBRK, SWT_FUSE, SWT_SECT, SWT_REC, SWT_DISC, SWT_BRK, SWT_ELBOW<br><br>OpenDSS currently does not use this internally. For whatever purpose the user defines. Default is OH. |
| 31 | normamps | Normal rated current. |
| 32 | emergamps | Maximum or emerg current. |
| 33 | faultrate | Failure rate PER UNIT LENGTH per year. Length must be same units as LENGTH property. Default is 0.1 fault per unit length per year. |
| 34 | pctperm | Percent of failures that become permanent. Default is 20. |
| 35 | repair | Hours to repair. Default is 3 hr. |
| 36 | basefreq | Base Frequency for ratings. |
| 37 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 38 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `Reactor` properties

| Number | Name | Description |
| - | - | - |
| 1 | bus1 | Name of first bus. Examples:<br>bus1=busname<br>bus1=busname.1.2.3<br><br>Bus2 property will default to this bus, node 0, unless previously specified. Only Bus1 need be specified for a Yg shunt reactor. |
| 2 | bus2 | Name of 2nd bus. Defaults to all phases connected to first bus, node 0, (Shunt Wye Connection) except when Bus2 is specifically defined.<br><br>Not necessary to specify for delta (LL) connection |
| 3 | phases | Number of phases. |
| 4 | kvar | Total kvar, all phases.  Evenly divided among phases. Only determines X. Specify R separately |
| 5 | kv | For 2, 3-phase, kV phase-phase. Otherwise specify actual coil rating. |
| 6 | conn | ={wye \| delta \|LN \|LL}  Default is wye, which is equivalent to LN. If Delta, then only one terminal. |
| 7 | Rmatrix | Resistance matrix, lower triangle, ohms at base frequency. Order of the matrix is the number of phases. Mutually exclusive to specifying parameters by kvar or X. |
| 8 | Xmatrix | Reactance matrix, lower triangle, ohms at base frequency. Order of the matrix is the number of phases. Mutually exclusive to specifying parameters by kvar or X. |
| 9 | Parallel | {Yes \| No}  Default=No. Indicates whether Rmatrix and Xmatrix are to be considered in parallel. Default is series. For other models, specify R and Rp. |
| 10 | R | Resistance (in series with reactance), each phase, ohms. This property applies to REACTOR specified by either kvar or X. See also help on Z. |
| 11 | X | Reactance, each phase, ohms at base frequency. See also help on Z and LmH properties. |
| 12 | Rp | Resistance in parallel with R and X (the entire branch). Assumed infinite if not specified. |
| 13 | Z1 | Positive-sequence impedance, ohms, as a 2-element array representing a complex number. Example: <br><br>Z1=[1, 2]  ! represents 1 + j2 <br><br>If defined, Z1, Z2, and Z0 are used to define the impedance matrix of the REACTOR. Z1 MUST BE DEFINED TO USE THIS OPTION FOR DEFINING THE MATRIX.<br><br>Side Effect: Sets Z2 and Z0 to same values unless they were previously defined. |
| 14 | Z2 | Negative-sequence impedance, ohms, as a 2-element array representing a complex number. Example: <br><br>Z2=[1, 2]  ! represents 1 + j2 <br><br>Used to define the impedance matrix of the REACTOR if Z1 is also specified. <br><br>Note: Z2 defaults to Z1 if it is not specifically defined. If Z2 is not equal to Z1, the impedance matrix is asymmetrical. |
| 15 | Z0 | Zer0-sequence impedance, ohms, as a 2-element array representing a complex number. Example: <br><br>Z0=[3, 4]  ! represents 3 + j4 <br><br>Used to define the impedance matrix of the REACTOR if Z1 is also specified. <br><br>Note: Z0 defaults to Z1 if it is not specifically defined.  |
| 16 | Z | Alternative way of defining R and X properties. Enter a 2-element array representing R +jX in ohms. Example:<br><br>Z=[5  10]   ! equivalent to R=5  X=10  |
| 17 | RCurve | Name of XYCurve object, previously defined, describing per-unit variation of phase resistance, R, vs. frequency. Applies to resistance specified by R or Z property. If actual values are not known, R often increases by approximately the square root of frequency. |
| 18 | LCurve | Name of XYCurve object, previously defined, describing per-unit variation of phase inductance, L=X/w, vs. frequency. Applies to reactance specified by X, LmH, Z, or kvar property.L generally decreases somewhat with frequency above the base frequency, approaching a limit at a few kHz. |
| 19 | LmH | Inductance, mH. Alternate way to define the reactance, X, property. |
| 20 | normamps | Normal rated current. |
| 21 | emergamps | Maximum or emerg current. |
| 22 | faultrate | Failure rate per year. |
| 23 | pctperm | Percent of failures that become permanent. |
| 24 | repair | Hours to repair. |
| 25 | basefreq | Base Frequency for ratings. |
| 26 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 27 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `Transformer` properties

| Number | Name | Description |
| - | - | - |
| 1 | phases | Number of phases this transformer. Default is 3. |
| 2 | windings | Number of windings, this transformers. (Also is the number of terminals) Default is 2. This property triggers memory allocation for the Transformer and will cause other properties to revert to default values. |
| 3 | wdg | Set this = to the number of the winding you wish to define.  Then set the values for this winding.  Repeat for each winding.  Alternatively, use the array collections (buses, kVAs, etc.) to define the windings.  Note: reactances are BETWEEN pairs of windings; they are not the property of a single winding. |
| 4 | bus | Bus connection spec for this winding. |
| 5 | conn | Connection of this winding {wye\*, Delta, LN, LL}. Default is "wye" with the neutral solidly grounded.  |
| 6 | kV | For 2-or 3-phase, enter phase-phase kV rating.  Otherwise, kV rating of the actual winding |
| 7 | kVA | Base kVA rating of the winding. Side effect: forces change of max normal and emerg kVA ratings.If 2-winding transformer, forces other winding to same value. When winding 1 is defined, all other windings are defaulted to the same rating and the first two winding resistances are defaulted to the %loadloss value. |
| 8 | tap | Per unit tap that this winding is on. |
| 9 | %R | Percent resistance this winding.  (half of total for a 2-winding). |
| 10 | Rneut | Default = -1. Neutral resistance of wye (star)-connected winding in actual ohms. If entered as a negative value, the neutral is assumed to be open, or floating. To solidly ground the neutral, connect the neutral conductor to Node 0 in the Bus property spec for this winding. For example: Bus=MyBusName.1.2.3.0, which is generally the default connection. |
| 11 | Xneut | Neutral reactance of wye(star)-connected winding in actual ohms.  May be + or -. |
| 12 | buses | Use this to specify all the bus connections at once using an array. Example:<br><br>New Transformer.T1 buses="Hibus, lowbus" |
| 13 | conns | Use this to specify all the Winding connections at once using an array. Example:<br><br>New Transformer.T1 buses="Hibus, lowbus" \~ conns=(delta, wye) |
| 14 | kVs | Use this to specify the kV ratings of all windings at once using an array. Example:<br><br>New Transformer.T1 buses="Hibus, lowbus" <br>\~ conns=(delta, wye)<br>\~ kvs=(115, 12.47)<br><br>See kV= property for voltage rules. |
| 15 | kVAs | Use this to specify the kVA ratings of all windings at once using an array. |
| 16 | taps | Use this to specify the p.u. tap of all windings at once using an array. |
| 17 | XHL | Use this to specify the percent reactance, H-L (winding 1 to winding 2).  Use for 2- or 3-winding transformers. On the kVA base of winding 1. See also X12. |
| 18 | XHT | Use this to specify the percent reactance, H-T (winding 1 to winding 3).  Use for 3-winding transformers only. On the kVA base of winding 1. See also X13. |
| 19 | XLT | Use this to specify the percent reactance, L-T (winding 2 to winding 3).  Use for 3-winding transformers only. On the kVA base of winding 1.  See also X23. |
| 20 | Xscarray | Use this to specify the percent reactance between all pairs of windings as an array. All values are on the kVA base of winding 1.  The order of the values is as follows:<br><br>(x12 13 14... 23 24.. 34 ..)  <br><br>There will be n(n-1)/2 values, where n=number of windings. |
| 21 | thermal | Thermal time constant of the transformer in hours.  Typically about 2. |
| 22 | n | n Exponent for thermal properties in IEEE C57.  Typically 0.8. |
| 23 | m | m Exponent for thermal properties in IEEE C57.  Typically 0.9 - 1.0 |
| 24 | flrise | Temperature rise, deg C, for full load.  Default is 65. |
| 25 | hsrise | Hot spot temperature rise, deg C.  Default is 15. |
| 26 | %loadloss | Percent load loss at full load. The %R of the High and Low windings (1 and 2) are adjusted to agree at rated kVA loading. |
| 27 | %noloadloss | Percent no load losses at rated excitatation voltage. Default is 0. Converts to a resistance in parallel with the magnetizing impedance in each winding. |
| 28 | normhkVA | Normal maximum kVA rating of H winding (winding 1).  Usually 100% - 110% ofmaximum nameplate rating, depending on load shape. Defaults to 110% of kVA rating of Winding 1. |
| 29 | emerghkVA | Emergency (contingency)  kVA rating of H winding (winding 1).  Usually 140% - 150% ofmaximum nameplate rating, depending on load shape. Defaults to 150% of kVA rating of Winding 1. |
| 30 | sub | ={Yes\|No}  Designates whether this transformer is to be considered a substation.Default is No. |
| 31 | MaxTap | Max per unit tap for the active winding.  Default is 1.10 |
| 32 | MinTap | Min per unit tap for the active winding.  Default is 0.90 |
| 33 | NumTaps | Total number of taps between min and max tap.  Default is 32 (16 raise and 16 lower taps about the neutral position). The neutral position is not counted. |
| 34 | subname | Substation Name. Optional. Default is null. If specified, printed on plots |
| 35 | %imag | Percent magnetizing current. Default=0.0. Magnetizing branch is in parallel with windings in each phase. Also, see "ppm_antifloat". |
| 36 | ppm_antifloat | Default=1 ppm.  Parts per million of transformer winding VA rating connected to ground to protect against accidentally floating a winding without a reference. If positive then the effect is adding a very large reactance to ground.  If negative, then a capacitor. |
| 37 | %Rs | Use this property to specify all the winding %resistances using an array. Example:<br><br>New Transformer.T1 buses="Hibus, lowbus" \~ %Rs=(0.2  0.3) |
| 38 | bank | Name of the bank this transformer is part of, for CIM, MultiSpeak, and other interfaces. |
| 39 | XfmrCode | Name of a library entry for transformer properties. The named XfmrCode must already be defined. |
| 40 | XRConst | ={Yes\|No} Default is NO. Signifies whether or not the X/R is assumed contant for harmonic studies. |
| 41 | X12 | Alternative to XHL for specifying the percent reactance from winding 1 to winding 2.  Use for 2- or 3-winding transformers. Percent on the kVA base of winding 1.  |
| 42 | X13 | Alternative to XHT for specifying the percent reactance from winding 1 to winding 3.  Use for 3-winding transformers only. Percent on the kVA base of winding 1.  |
| 43 | X23 | Alternative to XLT for specifying the percent reactance from winding 2 to winding 3.Use for 3-winding transformers only. Percent on the kVA base of winding 1.   |
| 44 | LeadLag | {Lead \| Lag (default) \| ANSI (default) \| Euro } Designation in mixed Delta-wye connections the relationship between HV to LV winding. Default is ANSI 30 deg lag, e.g., Dy1 of Yd1 vector group. To get typical European Dy11 connection, specify either "lead" or "Euro" |
| 45 | WdgCurrents | (Read only) Makes winding currents available via return on query (? Transformer.TX.WdgCurrents). Order: Phase 1, Wdg 1, Wdg 2, ..., Phase 2 ... |
| 46 | Core | {Shell\*\|5-leg\|3-Leg\|1-phase} Core Type. Used for GIC analysis |
| 47 | RdcOhms | Winding dc resistance in OHMS. Useful for GIC analysis. From transformer test report. Defaults to 85% of %R property |
| 48 | Seasons | Defines the number of ratings to be defined for the transfomer, to be used only when defining seasonal ratings using the "Ratings" property. |
| 49 | Ratings | An array of ratings to be used when the seasonal ratings flag is True. It can be used to insert<br>multiple ratings to change during a QSTS simulation to evaluate different ratings in transformers. Is given in kVA |
| 50 | normamps | Normal rated current. |
| 51 | emergamps | Maximum or emerg current. |
| 52 | faultrate | Failure rate per year. |
| 53 | pctperm | Percent of failures that become permanent. |
| 54 | repair | Hours to repair. |
| 55 | basefreq | Base Frequency for ratings. |
| 56 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 57 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


---
### Power Conversion Elements

#### `Generator` properties

| Number | Name | Description |
| - | - | - |
| 1 | phases | Number of Phases, this Generator.  Power is evenly divided among phases. |
| 2 | bus1 | Bus to which the Generator is connected.  May include specific node specification. |
| 3 | kv | Nominal rated (1.0 per unit) voltage, kV, for Generator. For 2- and 3-phase Generators, specify phase-phase kV. Otherwise, for phases=1 or phases>3, specify actual kV across each branch of the Generator. If wye (star), specify phase-neutral kV. If delta or phase-phase connected, specify phase-phase kV. |
| 4 | kW | Total base kW for the Generator.  A positive value denotes power coming OUT of the element, <br>which is the opposite of a load. This value is modified depending on the dispatch mode. Unaffected by the global load multiplier and growth curves. If you want there to be more generation, you must add more generators or change this value. |
| 5 | pf | Generator power factor. Default is 0.80. Enter negative for leading powerfactor (when kW and kvar have opposite signs.)<br>A positive power factor for a generator signifies that the generator produces vars <br>as is typical for a synchronous generator.  Induction machines would be <br>specified with a negative power factor. |
| 6 | kvar | Specify the base kvar.  Alternative to specifying the power factor.  Side effect:  the power factor value is altered to agree based on present value of kW. |
| 7 | model | Integer code for the model to use for generation variation with voltage. Valid values are:<br><br>1:Generator injects a constant kW at specified power factor.<br>2:Generator is modeled as a constant admittance.<br>3:Const kW, constant kV.  Somewhat like a conventional transmission power flow P-V generator.<br>4:Const kW, Fixed Q (Q never varies)<br>5:Const kW, Fixed Q(as a constant reactance)<br>6:Compute load injection from User-written Model.(see usage of Xd, Xdp)<br>7:Constant kW, kvar, but current-limited below Vminpu. Approximates a simple inverter. See also Balanced. |
| 8 | Vminpu | Default = 0.90.  Minimum per unit voltage for which the Model is assumed to apply. Below this value, the load model reverts to a constant impedance model. For model 7, the current is limited to the value computed for constant power at Vminpu. |
| 9 | Vmaxpu | Default = 1.10.  Maximum per unit voltage for which the Model is assumed to apply. Above this value, the load model reverts to a constant impedance model. |
| 10 | yearly | Dispatch shape to use for yearly simulations.  Must be previously defined as a Loadshape object. If this is not specified, a constant value is assumed (no variation). If the generator is assumed to be ON continuously, specify Status=FIXED, or designate a curve that is 1.0 per unit at all times. Set to NONE to reset to no loadahape. Nominally for 8760 simulations.  If there are fewer points in the designated shape than the number of points in the solution, the curve is repeated. |
| 11 | daily | Dispatch shape to use for daily simulations.  Must be previously defined as a Loadshape object of 24 hrs, typically.  If generator is assumed to be ON continuously, specify Status=FIXED, or designate a Loadshape objectthat is 1.0 perunit for all hours. Set to NONE to reset to no loadahape.  |
| 12 | duty | Load shape to use for duty cycle dispatch simulations such as for wind generation. Must be previously defined as a Loadshape object. Typically would have time intervals less than 1 hr -- perhaps, in seconds. Set Status=Fixed to ignore Loadshape designation. Set to NONE to reset to no loadahape. Designate the number of points to solve using the Set Number=xxxx command. If there are fewer points in the actual shape, the shape is assumed to repeat. |
| 13 | dispmode | {Default\* \| Loadlevel \| Price } Default = Default. Dispatch mode. In default mode, gen is either always on or follows dispatch curve as specified. Otherwise, the gen comes on when either the global default load level (Loadshape "default") or the price level exceeds the dispatch value. |
| 14 | dispvalue | Dispatch value. <br>If = 0.0 (default) then Generator follow dispatch curves, if any. <br>If > 0  then Generator is ON only when either the price signal (in Price dispatch mode) exceeds this value or the active circuit load multiplier \* "default" loadshape value \* the default yearly growth factor exceeds this value.  Then the generator follows dispatch curves (duty, daily, or yearly), if any (see also Status). |
| 15 | conn | ={wye\|LN\|delta\|LL}.  Default is wye. |
| 16 | Rneut | Removed due to causing confusion - Add neutral impedance externally. |
| 17 | Xneut | Removed due to causing confusion - Add neutral impedance externally. |
| 18 | status | ={Fixed \| Variable\*}.  If Fixed, then dispatch multipliers do not apply. The generator is alway at full power when it is ON.  Default is Variable  (follows curves). |
| 19 | class | An arbitrary integer number representing the class of Generator so that Generator values may be segregated by class. |
| 20 | Vpu | Per Unit voltage set point for Model = 3  (typical power flow model).  Default is 1.0.  |
| 21 | maxkvar | Maximum kvar limit for Model = 3.  Defaults to twice the specified load kvar.  Always reset this if you change PF or kvar properties. |
| 22 | minkvar | Minimum kvar limit for Model = 3. Enter a negative number if generator can absorb vars. Defaults to negative of Maxkvar.  Always reset this if you change PF or kvar properties. |
| 23 | pvfactor | Deceleration factor for P-V generator model (Model=3).  Default is 0.1. If the circuit converges easily, you may want to use a higher number such as 1.0. Use a lower number if solution diverges. Use Debugtrace=yes to create a file that will trace the convergence of a generator model. |
| 24 | forceon | {Yes \| No}  Forces generator ON despite requirements of other dispatch modes. Stays ON until this property is set to NO, or an internal algorithm cancels the forced ON state. |
| 25 | kVA | kVA rating of electrical machine. Defaults to 1.2\* kW if not specified. Applied to machine or inverter definition for Dynamics mode solutions.  |
| 26 | MVA | MVA rating of electrical machine.  Alternative to using kVA=. |
| 27 | Xd | Per unit synchronous reactance of machine. Presently used only for Thevinen impedance for power flow calcs of user models (model=6). Typically use a value 0.4 to 1.0. Default is 1.0 |
| 28 | Xdp | Per unit transient reactance of the machine.  Used for Dynamics mode and Fault studies.  Default is 0.27.For user models, this value is used for the Thevinen/Norton impedance for Dynamics Mode. |
| 29 | Xdpp | Per unit subtransient reactance of the machine.  Used for Harmonics. Default is 0.20. |
| 30 | H | Per unit mass constant of the machine.  MW-sec/MVA.  Default is 1.0. |
| 31 | D | Damping constant.  Usual range is 0 to 4. Default is 1.0.  Adjust to get damping |
| 32 | UserModel | Name of DLL containing user-written model, which computes the terminal currents for Dynamics studies, overriding the default model.  Set to "none" to negate previous setting. |
| 33 | UserData | String (in quotes or parentheses) that gets passed to user-written model for defining the data required for that model. |
| 34 | ShaftModel | Name of user-written DLL containing a Shaft model, which models the prime mover and determines the power on the shaft for Dynamics studies. Models additional mass elements other than the single-mass model in the DSS default model. Set to "none" to negate previous setting. |
| 35 | ShaftData | String (in quotes or parentheses) that gets passed to user-written shaft dynamic model for defining the data for that model. |
| 36 | DutyStart | Starting time offset [hours] into the duty cycle shape for this generator, defaults to 0 |
| 37 | debugtrace | {Yes \| No }  Default is no.  Turn this on to capture the progress of the generator model for each iteration.  Creates a separate file for each generator named "GEN_name.CSV". |
| 38 | Balanced | {Yes \| No\*} Default is No.  For Model=7, force balanced current only for 3-phase generators. Force zero- and negative-sequence to zero. |
| 39 | XRdp | Default is 20. X/R ratio for Xdp property for FaultStudy and Dynamic modes. |
| 40 | UseFuel | {Yes \| \*No}. Activates the use of fuel for the operation of the generator. When the fuel level reaches the reserve level, the generator stops until it gets refueled. By default, the generator is connected to a continuous fuel supply, Use this mode to mimic dependency on fuel level for different generation technologies. |
| 41 | FuelkWh | {\*0}Is the nominal level of fuel for the generator (kWh). It only applies if UseFuel = Yes/True |
| 42 | %Fuel | It is a number between 0 and 100 representing the current amount of fuel avaiable in percentage of FuelkWh. It only applies if UseFuel = Yes/True |
| 43 | %Reserve | It is a number between 0 and 100 representing the reserve level in percentage of FuelkWh. It only applies if UseFuel = Yes/True |
| 44 | Refuel | It is a boolean value (Yes/True, No/False) that can be used to manually refuel the generator when needed. It only applies if UseFuel = Yes/True |
| 45 | spectrum | Name of harmonic voltage or current spectrum for this generator. Voltage behind Xd" for machine - default. Current injection for inverter. Default value is "default", which is defined when the DSS starts. |
| 46 | basefreq | Base Frequency for ratings. |
| 47 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 48 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `GICLine` properties

| Number | Name | Description |
| - | - | - |
| 1 | bus1 | Name of bus to which the main terminal (1) is connected.<br>bus1=busname<br>bus1=busname.1.2.3 |
| 2 | bus2 | Name of bus to which 2nd terminal is connected.<br>bus2=busname<br>bus2=busname.1.2.3<br><br>No Default; must be specified. |
| 3 | Volts | Voltage magnitude, in volts, of the GIC voltage induced across this line. When spedified, voltage source is assumed defined by Voltage and Angle properties. <br><br>Specify this value<br><br>OR<br><br>EN, EE, lat1, lon1, lat2, lon2. <br><br>Not both!!  Last one entered will take precedence. Assumed identical in each phase of the Line object. |
| 4 | Angle | Phase angle in degrees of first phase. Default=0.0.  See Voltage property |
| 5 | frequency | Source frequency.  Defaults to 0.1 Hz. |
| 6 | phases | Number of phases.  Defaults to 3. |
| 7 | R | Resistance of line, ohms of impedance in series with GIC voltage source.  |
| 8 | X | Reactance at base frequency, ohms. Default = 0.0. This value is generally not important for GIC studies but may be used if desired. |
| 9 | C | Value of line blocking capacitance in microfarads. Default = 0.0, implying that there is no line blocking capacitor. |
| 10 | EN | Northward Electric field (V/km). If specified, Voltage and Angle are computed from EN, EE, lat and lon values. |
| 11 | EE | Eastward Electric field (V/km).  If specified, Voltage and Angle are computed from EN, EE, lat and lon values. |
| 12 | Lat1 | Latitude of Bus1 (degrees) |
| 13 | Lon1 | Longitude of Bus1 (degrees) |
| 14 | Lat2 | Latitude of Bus2 (degrees) |
| 15 | Lon2 | Longitude of Bus2 (degrees) |
| 16 | spectrum | Inherited Property for all PCElements. Name of harmonic spectrum for this source.  Default is "defaultvsource", which is defined when the DSS starts. |
| 17 | basefreq | Inherited Property for all PCElements. Base frequency for specification of reactance value. |
| 18 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 19 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `IndMach012` properties

| Number | Name | Description |
| - | - | - |
| 1 | phases | Number of Phases, this Induction Machine.   |
| 2 | bus1 | Bus to which the Induction Machine is connected.  May include specific node specification. |
| 3 | kv | Nominal rated (1.0 per unit) voltage, kV. For 2- and 3-phase machines, specify phase-phase kV. Otherwise, specify actual kV across each branch of the machine. If wye (star), specify phase-neutral kV. If delta or phase-phase connected, specify phase-phase kV. |
| 4 | kW | Shaft Power, kW, for the Induction Machine.  A positive value denotes power for a load. <br>Negative value denotes an induction generator.  |
| 5 | pf | [Read Only] Present power factor for the machine.  |
| 6 | conn | Connection of stator: Delta or Wye. Default is Delta. |
| 7 | kVA | Rated kVA for the machine. |
| 8 | H | Per unit mass constant of the machine.  MW-sec/MVA.  Default is 1.0. |
| 9 | D | Damping constant.  Usual range is 0 to 4. Default is 1.0.  Adjust to get damping in Dynamics mode, |
| 10 | puRs | Per unit stator resistance. Default is 0.0053. |
| 11 | puXs | Per unit stator leakage reactance. Default is 0.106. |
| 12 | puRr | Per unit rotor  resistance. Default is 0.007. |
| 13 | puXr | Per unit rotor leakage reactance. Default is 0.12. |
| 14 | puXm | Per unit magnetizing reactance.Default is 4.0. |
| 15 | Slip | Initial slip value. Default is 0.007 |
| 16 | MaxSlip | Max slip value to allow. Default is 0.1. Set this before setting slip. |
| 17 | SlipOption | Option for slip model. One of {fixedslip \| variableslip\*  } |
| 18 | Yearly | LOADSHAPE object to use for yearly simulations.  Must be previously defined as a Loadshape object. Is set to the Daily load shape  when Daily is defined.  The daily load shape is repeated in this case. Set Status=Fixed to ignore Loadshape designation. Set to NONE to reset to no loadahape. The default is no variation. |
| 19 | Daily | LOADSHAPE object to use for daily simulations.  Must be previously defined as a Loadshape object of 24 hrs, typically. Set Status=Fixed to ignore Loadshape designation. Set to NONE to reset to no loadahape. Default is no variation (constant) if not defined. Side effect: Sets Yearly load shape if not already defined. |
| 20 | Duty | LOADSHAPE object to use for duty cycle simulations.  Must be previously defined as a Loadshape object.  Typically would have time intervals less than 1 hr. Designate the number of points to solve using the Set Number=xxxx command. If there are fewer points in the actual shape, the shape is assumed to repeat.Set to NONE to reset to no loadahape. Set Status=Fixed to ignore Loadshape designation.  Defaults to Daily curve If not specified. |
| 21 | Debugtrace | [Yes \| No\*] Write DebugTrace file. |
| 22 | spectrum | Name of harmonic voltage or current spectrum for this IndMach012. Voltage behind Xd" for machine - default. Current injection for inverter. Default value is "default", which is defined when the DSS starts. |
| 23 | basefreq | Base Frequency for ratings. |
| 24 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 25 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `Load` properties

| Number | Name | Description |
| - | - | - |
| 1 | phases | Number of Phases, this load.  Load is evenly divided among phases. |
| 2 | bus1 | Bus to which the load is connected.  May include specific node specification. |
| 3 | kV | Nominal rated (1.0 per unit) voltage, kV, for load. For 2- and 3-phase loads, specify phase-phase kV. Otherwise, specify actual kV across each branch of the load. If wye (star), specify phase-neutral kV. If delta or phase-phase connected, specify phase-phase kV. |
| 4 | kW | Total base kW for the load.  Normally, you would enter the maximum kW for the load for the first year and allow it to be adjusted by the load shapes, growth shapes, and global load multiplier.<br><br>Legal ways to define base load:<br>kW, PF<br>kW, kvar<br>kVA, PF<br>XFKVA \* Allocationfactor, PF<br>kWh/(kWhdays\*24) \* Cfactor, PF |
| 5 | pf | Load power factor.  Enter negative for leading powerfactor (when kW and kvar have opposite signs.) |
| 6 | model | Integer code for the model to use for load variation with voltage. Valid values are:<br><br>1:Standard constant P+jQ load. (Default)<br>2:Constant impedance load. <br>3:Const P, Quadratic Q (like a motor).<br>4:Nominal Linear P, Quadratic Q (feeder mix). Use this with CVRfactor.<br>5:Constant Current Magnitude<br>6:Const P, Fixed Q<br>7:Const P, Fixed Impedance Q<br>8:ZIPV (7 values)<br><br>For Types 6 and 7, only the P is modified by load multipliers. |
| 7 | yearly | LOADSHAPE object to use for yearly simulations.  Must be previously defined as a Loadshape object. Is set to the Daily load shape  when Daily is defined.  The daily load shape is repeated in this case. Set Status=Fixed to ignore Loadshape designation. Set to NONE to reset to no loadahape. The default is no variation. |
| 8 | daily | LOADSHAPE object to use for daily simulations.  Must be previously defined as a Loadshape object of 24 hrs, typically. Set Status=Fixed to ignore Loadshape designation. Set to NONE to reset to no loadahape. Default is no variation (constant) if not defined. Side effect: Sets Yearly load shape if not already defined. |
| 9 | duty | LOADSHAPE object to use for duty cycle simulations.  Must be previously defined as a Loadshape object.  Typically would have time intervals less than 1 hr. Designate the number of points to solve using the Set Number=xxxx command. If there are fewer points in the actual shape, the shape is assumed to repeat.Set to NONE to reset to no loadahape. Set Status=Fixed to ignore Loadshape designation.  Defaults to Daily curve If not specified. |
| 10 | growth | Characteristic  to use for growth factors by years.  Must be previously defined as a Growthshape object. Defaults to circuit default growth factor (see Set Growth command). |
| 11 | conn | ={wye or LN \| delta or LL}.  Default is wye. |
| 12 | kvar | Specify the base kvar for specifying load as kW & kvar.  Assumes kW has been already defined.  Alternative to specifying the power factor.  Side effect:  the power factor and kVA is altered to agree. |
| 13 | Rneut | Default is -1. Neutral resistance of wye (star)-connected load in actual ohms. If entered as a negative value, the neutral can be open, or floating, or it can be connected to node 0 (ground), which is the usual default. If >=0 be sure to explicitly specify the node connection for the neutral, or last, conductor. Otherwise, the neutral impedance will be shorted to ground. |
| 14 | Xneut | Neutral reactance of wye(star)-connected load in actual ohms.  May be + or -. |
| 15 | status | ={Variable \| Fixed \| Exempt}.  Default is variable. If Fixed, no load multipliers apply;  however, growth multipliers do apply.  All multipliers apply to Variable loads.  Exempt loads are not modified by the global load multiplier, such as in load duration curves, etc.  Daily multipliers do apply, so setting this property to Exempt is a good way to represent industrial load that stays the same day-after-day for the period study. |
| 16 | class | An arbitrary integer number representing the class of load so that load values may be segregated by load value. Default is 1; not used internally. |
| 17 | Vminpu | Default = 0.95.  Minimum per unit voltage for which the MODEL is assumed to apply. Lower end of normal voltage range.Below this value, the load model reverts to a constant impedance model that matches the model at the transition voltage. See also "Vlowpu" which causes the model to match Model=2 below the transition voltage. |
| 18 | Vmaxpu | Default = 1.05.  Maximum per unit voltage for which the MODEL is assumed to apply. Above this value, the load model reverts to a constant impedance model. |
| 19 | Vminnorm | Minimum per unit voltage for load EEN evaluations, Normal limit.  Default = 0, which defaults to system "vminnorm" property (see Set Command under Executive).  If this property is specified, it ALWAYS overrides the system specification. This allows you to have different criteria for different loads. Set to zero to revert to the default system value. |
| 20 | Vminemerg | Minimum per unit voltage for load UE evaluations, Emergency limit.  Default = 0, which defaults to system "vminemerg" property (see Set Command under Executive).  If this property is specified, it ALWAYS overrides the system specification. This allows you to have different criteria for different loads. Set to zero to revert to the default system value. |
| 21 | xfkVA | Default = 0.0.  Rated kVA of service transformer for allocating loads based on connected kVA at a bus. Side effect:  kW, PF, and kvar are modified. See help on kVA. |
| 22 | allocationfactor | Default = 0.5.  Allocation factor for allocating loads based on connected kVA at a bus. Side effect:  kW, PF, and kvar are modified by multiplying this factor times the XFKVA (if > 0). |
| 23 | kVA | Specify base Load in kVA (and power factor)<br><br>Legal ways to define base load:<br>kW, PF<br>kW, kvar<br>kVA, PF<br>XFKVA \* Allocationfactor, PF<br>kWh/(kWhdays\*24) \* Cfactor, PF |
| 24 | %mean | Percent mean value for load to use for monte carlo studies if no loadshape is assigned to this load. Default is 50. |
| 25 | %stddev | Percent Std deviation value for load to use for monte carlo studies if no loadshape is assigned to this load. Default is 10. |
| 26 | CVRwatts | Percent reduction in active power (watts) per 1% reduction in voltage from 100% rated. Default=1. <br> Typical values range from 0.4 to 0.8. Applies to Model=4 only.<br> Intended to represent conservation voltage reduction or voltage optimization measures. |
| 27 | CVRvars | Percent reduction in reactive power (vars) per 1% reduction in voltage from 100% rated. Default=2. <br> Typical values range from 2 to 3. Applies to Model=4 only.<br> Intended to represent conservation voltage reduction or voltage optimization measures. |
| 28 | kwh | kWh billed for this period. Default is 0. See help on kVA and Cfactor and kWhDays. |
| 29 | kwhdays | Length of kWh billing period in days (24 hr days). Default is 30. Average demand is computed using this value. |
| 30 | Cfactor | Factor relating average kW to peak kW. Default is 4.0. See kWh and kWhdays. See kVA. |
| 31 | CVRcurve | Default is NONE. Curve describing both watt and var factors as a function of time. Refers to a LoadShape object with both Mult and Qmult defined. Define a Loadshape to agree with yearly or daily curve according to the type of analysis being done. If NONE, the CVRwatts and CVRvars factors are used and assumed constant. |
| 32 | NumCust | Number of customers, this load. Default is 1. |
| 33 | ZIPV | Array of 7 coefficients:<br><br> First 3 are ZIP weighting factors for real power (should sum to 1)<br> Next 3 are ZIP weighting factors for reactive power (should sum to 1)<br> Last 1 is cut-off voltage in p.u. of base kV; load is 0 below this cut-off<br> No defaults; all coefficients must be specified if using model=8. |
| 34 | %SeriesRL | Percent of load that is series R-L for Harmonic studies. Default is 50. Remainder is assumed to be parallel R and L. This can have a significant impact on the amount of damping observed in Harmonics solutions. |
| 35 | RelWeight | Relative weighting factor for reliability calcs. Default = 1. Used to designate high priority loads such as hospitals, etc. <br><br>Is multiplied by number of customers and load kW during reliability calcs. |
| 36 | Vlowpu | Default = 0.50.  Per unit voltage at which the model switches to same as constant Z model (model=2). This allows more consistent convergence at very low voltaes due to opening switches or solving for fault situations. |
| 37 | puXharm | Special reactance, pu (based on kVA, kV properties), for the series impedance branch in the load model for HARMONICS analysis. Generally used to represent motor load blocked rotor reactance. If not specified (that is, set =0, the default value), the series branch is computed from the percentage of the nominal load at fundamental frequency specified by the %SERIESRL property. <br><br>Applies to load model in HARMONICS mode only.<br><br>A typical value would be approximately 0.20 pu based on kVA \* %SeriesRL / 100.0. |
| 38 | XRharm | X/R ratio of the special harmonics mode reactance specified by the puXHARM property at fundamental frequency. Default is 6.  |
| 39 | spectrum | Name of harmonic current spectrum for this load.  Default is "defaultload", which is defined when the DSS starts. |
| 40 | basefreq | Base Frequency for ratings. |
| 41 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 42 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `PVSystem` properties

| Number | Name | Description |
| - | - | - |
| 1 | phases | Number of Phases, this PVSystem element.  Power is evenly divided among phases. |
| 2 | bus1 | Bus to which the PVSystem element is connected.  May include specific node specification. |
| 3 | kv | Nominal rated (1.0 per unit) voltage, kV, for PVSystem element. For 2- and 3-phase PVSystem elements, specify phase-phase kV. Otherwise, specify actual kV across each branch of the PVSystem element. If 1-phase wye (star or LN), specify phase-neutral kV. If 1-phase delta or phase-phase connected, specify phase-phase kV. |
| 4 | irradiance | Get/set the present irradiance value in kW/sq-m. Used as base value for shape multipliers. Generally entered as peak value for the time period of interest and the yearly, daily, and duty load shape objects are defined as per unit multipliers (just like Loads/Generators). |
| 5 | Pmpp | Get/set the rated max power of the PV array for 1.0 kW/sq-m irradiance and a user-selected array temperature. The P-TCurve should be defined relative to the selected array temperature. |
| 6 | %Pmpp | Upper limit on active power as a percentage of Pmpp. |
| 7 | Temperature | Get/set the present Temperature. Used as fixed value corresponding to PTCurve property. A multiplier is obtained from the Pmpp-Temp curve and applied to the nominal Pmpp from the irradiance to determine the net array output. |
| 8 | pf | Nominally, the power factor for the output power. Default is 1.0. Setting this property will cause the inverter to operate in constant power factor mode.Enter negative when kW and kvar have opposite signs.<br>A positive power factor signifies that the PVSystem element produces vars <br>as is typical for a generator.   |
| 9 | conn | ={wye\|LN\|delta\|LL}.  Default is wye. |
| 10 | kvar | Get/set the present kvar value.  Setting this property forces the inverter to operate in constant kvar mode. |
| 11 | kVA | kVA rating of inverter. Used as the base for Dynamics mode and Harmonics mode values. |
| 12 | %Cutin | % cut-in power -- % of kVA rating of inverter. When the inverter is OFF, the power from the array must be greater than this for the inverter to turn on. |
| 13 | %Cutout | % cut-out power -- % of kVA rating of inverter. When the inverter is ON, the inverter turns OFF when the power from the array drops below this value. |
| 14 | EffCurve | An XYCurve object, previously defined, that describes the PER UNIT efficiency vs PER UNIT of rated kVA for the inverter. Inverter output power is discounted by the multiplier obtained from this curve. |
| 15 | P-TCurve | An XYCurve object, previously defined, that describes the PV array PER UNIT Pmpp vs Temperature curve. Temperature units must agree with the Temperature property and the Temperature shapes used for simulations. The Pmpp values are specified in per unit of the Pmpp value for 1 kW/sq-m irradiance. The value for the temperature at which Pmpp is defined should be 1.0. The net array power is determined by the irradiance \* Pmpp \* f(Temperature) |
| 16 | %R | Equivalent percent internal resistance, ohms. Default is 50%. Placed in series with internal voltage source for harmonics and dynamics modes. (Limits fault current to about 2 pu if not current limited -- see LimitCurrent)  |
| 17 | %X | Equivalent percent internal reactance, ohms. Default is 0%. Placed in series with internal voltage source for harmonics and dynamics modes.  |
| 18 | model | Integer code (default=1) for the model to use for power output variation with voltage. Valid values are:<br><br>1:PVSystem element injects a CONSTANT kW at specified power factor.<br>2:PVSystem element is modeled as a CONSTANT ADMITTANCE.<br>3:Compute load injection from User-written Model. |
| 19 | Vminpu | Default = 0.90.  Minimum per unit voltage for which the Model is assumed to apply. Below this value, the load model reverts to a constant impedance model except for Dynamics model. In Dynamics mode, the current magnitude is limited to the value the power flow would compute for this voltage. |
| 20 | Vmaxpu | Default = 1.10.  Maximum per unit voltage for which the Model is assumed to apply. Above this value, the load model reverts to a constant impedance model. |
| 21 | Balanced | {Yes \| No\*} Default is No.  Force balanced current only for 3-phase PVSystems2. Forces zero- and negative-sequence to zero.  |
| 22 | LimitCurrent | Limits current magnitude to Vminpu value for both 1-phase and 3-phase PVSystems2 similar to Generator Model 7. For 3-phase, limits the positive-sequence current but not the negative-sequence. |
| 23 | yearly | Dispatch shape to use for yearly simulations.  Must be previously defined as a Loadshape object. If this is not specified, the Daily dispatch shape, if any, is repeated during Yearly solution modes. In the default dispatch mode, the PVSystem element uses this loadshape to trigger State changes. |
| 24 | daily | Dispatch shape to use for daily simulations.  Must be previously defined as a Loadshape object of 24 hrs, typically.  In the default dispatch mode, the PVSystem element uses this loadshape to trigger State changes. |
| 25 | duty | Load shape to use for duty cycle dispatch simulations such as for solar ramp rate studies. Must be previously defined as a Loadshape object. Typically would have time intervals of 1-5 seconds. Designate the number of points to solve using the Set Number=xxxx command. If there are fewer points in the actual shape, the shape is assumed to repeat. |
| 26 | Tyearly | Temperature shape to use for yearly simulations.  Must be previously defined as a TShape object. If this is not specified, the Daily dispatch shape, if any, is repeated during Yearly solution modes. The PVSystem element uses this TShape to determine the Pmpp from the Pmpp vs T curve. Units must agree with the Pmpp vs T curve. |
| 27 | Tdaily | Temperature shape to use for daily simulations.  Must be previously defined as a TShape object of 24 hrs, typically.  The PVSystem element uses this TShape to determine the Pmpp from the Pmpp vs T curve. Units must agree with the Pmpp vs T curve. |
| 28 | Tduty | Temperature shape to use for duty cycle dispatch simulations such as for solar ramp rate studies. Must be previously defined as a TShape object. Typically would have time intervals of 1-5 seconds. Designate the number of points to solve using the Set Number=xxxx command. If there are fewer points in the actual shape, the shape is assumed to repeat. The PVSystem model uses this TShape to determine the Pmpp from the Pmpp vs T curve. Units must agree with the Pmpp vs T curve. |
| 29 | class | An arbitrary integer number representing the class of PVSystem element so that PVSystem values may be segregated by class. |
| 30 | UserModel | Name of DLL containing user-written model, which computes the terminal currents for Dynamics studies, overriding the default model.  Set to "none" to negate previous setting. |
| 31 | UserData | String (in quotes or parentheses) that gets passed to user-written model for defining the data required for that model. |
| 32 | debugtrace | {Yes \| No }  Default is no.  Turn this on to capture the progress of the PVSystem model for each iteration.  Creates a separate file for each PVSystem element named "PVSystem_name.CSV". |
| 33 | VarFollowInverter | Boolean variable (Yes\|No) or (True\|False). Defaults to False which indicates that the reactive power generation/absorption does not respect the inverter status.When set to True, the PVSystem reactive power generation/absorption will cease when the inverter status is off, due to panel kW dropping below %Cutout.  The reactive power generation/absorption will begin again when the panel kW is above %Cutin.  When set to False, the PVSystem will generate/absorb reactive power regardless of the status of the inverter. |
| 34 | DutyStart | Starting time offset [hours] into the duty cycle shape for this PVSystem, defaults to 0 |
| 35 | WattPriority | {Yes/No\*/True/False} Set inverter to watt priority instead of the default var priority |
| 36 | PFPriority | {Yes/No\*/True/False} Set inverter to operate with PF priority when in constant PF mode. If "Yes", value assigned to "WattPriority" is neglected. If controlled by an InvControl with either Volt-Var or DRC or both functions activated, PF priority is neglected and "WattPriority" is considered. Default = No. |
| 37 | %PminNoVars | Minimum active power as percentage of Pmpp under which there is no vars production/absorption. |
| 38 | %PminkvarMax | Minimum active power as percentage of Pmpp that allows the inverter to produce/absorb reactive power up to its kvarMax or kvarMaxAbs. |
| 39 | kvarMax | Indicates the maximum reactive power GENERATION (un-signed numerical variable in kvar) for the inverter (as an un-signed value). Defaults to kVA rating of the inverter. |
| 40 | kvarMaxAbs | Indicates the maximum reactive power ABSORPTION (un-signed numerical variable in kvar) for the inverter (as an un-signed value). Defaults to kVA rating of the inverter. |
| 41 | spectrum | Name of harmonic voltage or current spectrum for this PVSystem element. A harmonic voltage source is assumed for the inverter. Default value is "default", which is defined when the DSS starts. |
| 42 | basefreq | Base Frequency for ratings. |
| 43 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 44 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `Storage` properties

| Number | Name | Description |
| - | - | - |
| 1 | phases | Number of Phases, this Storage element.  Power is evenly divided among phases. |
| 2 | bus1 | Bus to which the Storage element is connected.  May include specific node specification. |
| 3 | kv | Nominal rated (1.0 per unit) voltage, kV, for Storage element. For 2- and 3-phase Storage elements, specify phase-phase kV. Otherwise, specify actual kV across each branch of the Storage element. <br><br>If wye (star), specify phase-neutral kV. <br><br>If delta or phase-phase connected, specify phase-phase kV. |
| 4 | conn | ={wye\|LN\|delta\|LL}.  Default is wye. |
| 5 | kW | Get/set the requested kW value. Final kW is subjected to the inverter ratings. A positive value denotes power coming OUT of the element, which is the opposite of a Load element. A negative value indicates the Storage element is in Charging state. This value is modified internally depending on the dispatch mode. |
| 6 | kvar | Get/set the requested kvar value. Final kvar is subjected to the inverter ratings. Sets inverter to operate in constant kvar mode. |
| 7 | pf | Get/set the requested PF value. Final PF is subjected to the inverter ratings. Sets inverter to operate in constant PF mode. Nominally, the power factor for discharging (acting as a generator). Default is 1.0. <br><br>Enter negative for leading power factor (when kW and kvar have opposite signs.)<br><br>A positive power factor signifies kw and kvar at the same direction. |
| 8 | kVA | Indicates the inverter nameplate capability (in kVA). Used as the base for Dynamics mode and Harmonics mode values. |
| 9 | %Cutin | Cut-in power as a percentage of inverter kVA rating. It is the minimum DC power necessary to turn the inverter ON when it is OFF. Must be greater than or equal to %CutOut. Defaults to 2 for PVSystems and 0 for Storage elements which means that the inverter state will be always ON for this element. |
| 10 | %Cutout | Cut-out power as a percentage of inverter kVA rating. It is the minimum DC power necessary to keep the inverter ON. Must be less than or equal to %CutIn. Defaults to 0, which means that, once ON, the inverter state will be always ON for this element. |
| 11 | EffCurve | An XYCurve object, previously defined, that describes the PER UNIT efficiency vs PER UNIT of rated kVA for the inverter. Power at the AC side of the inverter is discounted by the multiplier obtained from this curve. |
| 12 | VarFollowInverter | Boolean variable (Yes\|No) or (True\|False). Defaults to False, which indicates that the reactive power generation/absorption does not respect the inverter status.When set to True, the reactive power generation/absorption will cease when the inverter status is off, due to DC kW dropping below %CutOut.  The reactive power generation/absorption will begin again when the DC kW is above %CutIn.  When set to False, the Storage will generate/absorb reactive power regardless of the status of the inverter. |
| 13 | kvarMax | Indicates the maximum reactive power GENERATION (un-signed numerical variable in kvar) for the inverter. Defaults to kVA rating of the inverter. |
| 14 | kvarMaxAbs | Indicates the maximum reactive power ABSORPTION (un-signed numerical variable in kvar) for the inverter. Defaults to kvarMax. |
| 15 | WattPriority | {Yes/No\*/True/False} Set inverter to watt priority instead of the default var priority. |
| 16 | PFPriority | If set to true, priority is given to power factor and WattPriority is neglected. It works only if operating in either constant PF or constant kvar modes. Defaults to False. |
| 17 | %PminNoVars | Minimum active power as percentage of kWrated under which there is no vars production/absorption. Defaults to 0 (disabled). |
| 18 | %PminkvarMax | Minimum active power as percentage of kWrated that allows the inverter to produce/absorb reactive power up to its maximum reactive power, which can be either kvarMax or kvarMaxAbs, depending on the current operation quadrant. Defaults to 0 (disabled). |
| 19 | kWrated | kW rating of power output. Base for Loadshapes when DispMode=Follow. Sets kVA property if it has not been specified yet. Defaults to 25. |
| 20 | %kWrated | Upper limit on active power as a percentage of kWrated. Defaults to 100 (disabled). |
| 21 | kWhrated | Rated Storage capacity in kWh. Default is 50. |
| 22 | kWhstored | Present amount of energy stored, kWh. Default is same as kWhrated. |
| 23 | %stored | Present amount of energy stored, % of rated kWh. Default is 100. |
| 24 | %reserve | Percentage of rated kWh Storage capacity to be held in reserve for normal operation. Default = 20. <br>This is treated as the minimum energy discharge level unless there is an emergency. For emergency operation set this property lower. Cannot be less than zero. |
| 25 | State | {IDLING \| CHARGING \| DISCHARGING}  Get/Set present operational state. In DISCHARGING mode, the Storage element acts as a generator and the kW property is positive. The element continues discharging at the scheduled output power level until the Storage reaches the reserve value. Then the state reverts to IDLING. In the CHARGING state, the Storage element behaves like a Load and the kW property is negative. The element continues to charge until the max Storage kWh is reached and then switches to IDLING state. In IDLING state, the element draws the idling losses plus the associated inverter losses. |
| 26 | %Discharge | Discharge rate (output power) in percentage of rated kW. Default = 100. |
| 27 | %Charge | Charging rate (input power) in percentage of rated kW. Default = 100. |
| 28 | %EffCharge | Percentage efficiency for CHARGING the Storage element. Default = 90. |
| 29 | %EffDischarge | Percentage efficiency for DISCHARGING the Storage element. Default = 90. |
| 30 | %IdlingkW | Percentage of rated kW consumed by idling losses. Default = 1. |
| 31 | %Idlingkvar | Deprecated. |
| 32 | %R | Equivalent percentage internal resistance, ohms. Default is 0. Placed in series with internal voltage source for harmonics and dynamics modes. Use a combination of %IdlingkW, %EffCharge and %EffDischarge to account for losses in power flow modes. |
| 33 | %X | Equivalent percentage internal reactance, ohms. Default is 50%. Placed in series with internal voltage source for harmonics and dynamics modes. (Limits fault current to 2 pu. |
| 34 | model | Integer code (default=1) for the model to be used for power output variation with voltage. Valid values are:<br><br>1:Storage element injects/absorbs a CONSTANT power.<br>2:Storage element is modeled as a CONSTANT IMPEDANCE.<br>3:Compute load injection from User-written Model. |
| 35 | Vminpu | Default = 0.90.  Minimum per unit voltage for which the Model is assumed to apply. Below this value, the load model reverts to a constant impedance model. |
| 36 | Vmaxpu | Default = 1.10.  Maximum per unit voltage for which the Model is assumed to apply. Above this value, the load model reverts to a constant impedance model. |
| 37 | Balanced | {Yes \| No\*} Default is No. Force balanced current only for 3-phase Storage. Forces zero- and negative-sequence to zero.  |
| 38 | LimitCurrent | Limits current magnitude to Vminpu value for both 1-phase and 3-phase Storage similar to Generator Model 7. For 3-phase, limits the positive-sequence current but not the negative-sequence. |
| 39 | yearly | Dispatch shape to use for yearly simulations.  Must be previously defined as a Loadshape object. If this is not specified, the Daily dispatch shape, if any, is repeated during Yearly solution modes. In the default dispatch mode, the Storage element uses this loadshape to trigger State changes. |
| 40 | daily | Dispatch shape to use for daily simulations.  Must be previously defined as a Loadshape object of 24 hrs, typically.  In the default dispatch mode, the Storage element uses this loadshape to trigger State changes. |
| 41 | duty | Load shape to use for duty cycle dispatch simulations such as for solar ramp rate studies. Must be previously defined as a Loadshape object. <br><br>Typically would have time intervals of 1-5 seconds. <br><br>Designate the number of points to solve using the Set Number=xxxx command. If there are fewer points in the actual shape, the shape is assumed to repeat. |
| 42 | DispMode | {DEFAULT \| FOLLOW \| EXTERNAL \| LOADLEVEL \| PRICE } Default = "DEFAULT". Dispatch mode. <br><br>In DEFAULT mode, Storage element state is triggered to discharge or charge at the specified rate by the loadshape curve corresponding to the solution mode. <br><br>In FOLLOW mode the kW output of the Storage element follows the active loadshape multiplier until Storage is either exhausted or full. The element discharges for positive values and charges for negative values.  The loadshape is based on rated kW. <br><br>In EXTERNAL mode, Storage element state is controlled by an external Storagecontroller. This mode is automatically set if this Storage element is included in the element list of a StorageController element. <br><br>For the other two dispatch modes, the Storage element state is controlled by either the global default Loadlevel value or the price level.  |
| 43 | DischargeTrigger | Dispatch trigger value for discharging the Storage. <br>If = 0.0 the Storage element state is changed by the State command or by a StorageController object. <br>If <> 0  the Storage element state is set to DISCHARGING when this trigger level is EXCEEDED by either the specified Loadshape curve value or the price signal or global Loadlevel value, depending on dispatch mode. See State property. |
| 44 | ChargeTrigger | Dispatch trigger value for charging the Storage. <br><br>If = 0.0 the Storage element state is changed by the State command or StorageController object.  <br><br>If <> 0  the Storage element state is set to CHARGING when this trigger level is GREATER than either the specified Loadshape curve value or the price signal or global Loadlevel value, depending on dispatch mode. See State property. |
| 45 | TimeChargeTrig | Time of day in fractional hours (0230 = 2.5) at which Storage element will automatically go into charge state. Default is 2.0.  Enter a negative time value to disable this feature. |
| 46 | class | An arbitrary integer number representing the class of Storage element so that Storage values may be segregated by class. |
| 47 | DynaDLL | Name of DLL containing user-written dynamics model, which computes the terminal currents for Dynamics-mode simulations, overriding the default model.  Set to "none" to negate previous setting. This DLL has a simpler interface than the UserModel DLL and is only used for Dynamics mode. |
| 48 | DynaData | String (in quotes or parentheses if necessary) that gets passed to the user-written dynamics model Edit function for defining the data required for that model. |
| 49 | UserModel | Name of DLL containing user-written model, which computes the terminal currents for both power flow and dynamics, overriding the default model.  Set to "none" to negate previous setting. |
| 50 | UserData | String (in quotes or parentheses) that gets passed to user-written model for defining the data required for that model. |
| 51 | debugtrace | {Yes \| No }  Default is no.  Turn this on to capture the progress of the Storage model for each iteration.  Creates a separate file for each Storage element named "Storage_name.CSV". |
| 52 | spectrum | Name of harmonic voltage or current spectrum for this Storage element. Current injection is assumed for inverter. Default value is "default", which is defined when the DSS starts. |
| 53 | basefreq | Base Frequency for ratings. |
| 54 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 55 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `UPFC` properties

| Number | Name | Description |
| - | - | - |
| 1 | bus1 | Name of bus to which the input terminal (1) is connected.<br>bus1=busname.1.3<br>bus1=busname.1.2.3 |
| 2 | bus2 | Name of bus to which the output terminal (2) is connected.<br>bus2=busname.1.2<br>bus2=busname.1.2.3 |
| 3 | refkv | Base Voltage expected at the output of the UPFC<br><br>"refkv=0.24" |
| 4 | pf | Power factor target at the input terminal. |
| 5 | frequency | UPFC working frequency.  Defaults to system default base frequency. |
| 6 | phases | Number of phases.  Defaults to 1 phase (2 terminals, 1 conductor per terminal). |
| 7 | Xs | Reactance of the series transformer of the UPFC, ohms (default=0.7540 ... 2 mH) |
| 8 | Tol1 | Tolerance in pu for the series PI controller<br>Tol1=0.02 is the format used to define 2% tolerance (Default=2%) |
| 9 | Mode | Integer used to define the control mode of the UPFC: <br><br>0 = Off, <br>1 = Voltage regulator, <br>2 = Phase angle regulator, <br>3 = Dual mode |
| 10 | VpqMax | Maximum voltage (in volts) delivered by the series voltage source (Default = 24 V) |
| 11 | LossCurve | Name of the XYCurve for describing the losses behavior as a function of the voltage at the input of the UPFC |
| 12 | VHLimit | High limit for the voltage at the input of the UPFC, if the voltage is above this value the UPFC turns off. This value is specified in Volts (default 300 V) |
| 13 | VLLimit | low limit for the voltage at the input of the UPFC, if voltage is below this value the UPFC turns off. This value is specified in Volts (default 125 V) |
| 14 | CLimit | Current Limit for the UPFC, if the current passing through the UPFC is higher than this value the UPFC turns off. This value is specified in Amps (Default 265 A) |
| 15 | refkv2 | Base Voltage expected at the output of the UPFC for control modes 4 and 5.<br><br>This reference must be lower than refkv, see control modes 4 and 5 for details |
| 16 | kvarLimit | Maximum amount of reactive power (kvar) that can be absorved by the UPFC (Default = 5) |
| 17 | spectrum | Name of harmonic spectrum for this source.  Default is "defaultUPFC", which is defined when the DSS starts. |
| 18 | basefreq | Base Frequency for ratings. |
| 19 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 20 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `VCCS` properties

| Number | Name | Description |
| - | - | - |
| 1 | bus1 | Name of bus to which source is connected.<br>bus1=busname<br>bus1=busname.1.2.3 |
| 2 | phases | Number of phases.  Defaults to 1. |
| 3 | prated | Total rated power, in Watts. |
| 4 | vrated | Rated line-to-line voltage, in Volts |
| 5 | ppct | Steady-state operating output, in percent of rated. |
| 6 | bp1 | XYCurve defining the input piece-wise linear block. |
| 7 | bp2 | XYCurve defining the output piece-wise linear block. |
| 8 | filter | XYCurve defining the digital filter coefficients (x numerator, y denominator). |
| 9 | fsample | Sample frequency [Hz} for the digital filter. |
| 10 | rmsmode | True if only Hz is used to represent a phase-locked loop (PLL), ignoring the BP1, BP2 and time-domain transformations. Default is no. |
| 11 | imaxpu | Maximum output current in per-unit of rated; defaults to 1.1 |
| 12 | vrmstau | Time constant in sensing Vrms for the PLL; defaults to 0.0015 |
| 13 | irmstau | Time constant in producing Irms from the PLL; defaults to 0.0015 |
| 14 | spectrum | Harmonic spectrum assumed for this source.  Default is "default". |
| 15 | basefreq | Base Frequency for ratings. |
| 16 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 17 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `VSConverter` properties

| Number | Name | Description |
| - | - | - |
| 1 | phases | Number of AC plus DC conductors. Default is 4. AC phases numbered before DC conductors. |
| 2 | Bus1 | Name of converter bus, containing both AC and DC conductors. Bus2 is always ground. |
| 3 | kVac | Nominal AC line-neutral voltage in kV. Must be specified > 0. |
| 4 | kVdc | Nominal DC voltage in kV. Must be specified > 0. |
| 5 | kW | Nominal converter power in kW. Must be specified > 0. |
| 6 | Ndc | Number of DC conductors. Default is 1. DC conductors numbered after AC phases. |
| 7 | Rac | AC resistance (ohms) for the converter transformer, plus any series reactors. Default is 0.<br>Must be 0 for Vac control mode. |
| 8 | Xac | AC reactance (ohms) for the converter transformer, plus any series reactors. Default is 0.<br>Must be 0 for Vac control mode. Must be >0 for PacVac, PacQac or VacVdc control mode. |
| 9 | m0 | Fixed or initial value of the modulation index. Default is 0.5. |
| 10 | d0 | Fixed or initial value of the power angle in degrees. Default is 0. |
| 11 | Mmin | Minimum value of modulation index. Default is 0.1. |
| 12 | Mmax | Maximum value of modulation index. Default is 0.9. |
| 13 | Iacmax | Maximum value of AC line current, per-unit of nominal. Default is 2. |
| 14 | Idcmax | Maximum value of DC current, per-unit of nominal. Default is 2. |
| 15 | Vacref | Reference AC line-to-neutral voltage, RMS Volts. Default is 0.<br>Applies to PacVac and VdcVac control modes, influencing m. |
| 16 | Pacref | Reference total AC real power, Watts. Default is 0.<br>Applies to PacVac and PacQac control modes, influencing d. |
| 17 | Qacref | Reference total AC reactive power, Vars. Default is 0.<br>Applies to PacQac and VdcQac control modes, influencing m. |
| 18 | Vdcref | Reference DC voltage, Volts. Default is 0.<br>Applies to VdcVac control mode, influencing d. |
| 19 | VscMode | Control Mode (Fixed\|PacVac\|PacQac\|VdcVac\|VdcQac). Default is Fixed. |
| 20 | spectrum | Name of harmonic spectrum for this device. |
| 21 | basefreq | Base Frequency for ratings. |
| 22 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 23 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


---
###  Control Elements

#### `CapControl` properties

| Number | Name | Description |
| - | - | - |
| 1 | element | Full object name of the circuit element, typically a line or transformer, to which the capacitor control's PT and/or CT are connected.There is no default; must be specified. |
| 2 | terminal | Number of the terminal of the circuit element to which the CapControl is connected. 1 or 2, typically.  Default is 1. |
| 3 | capacitor | Name of Capacitor element which the CapControl controls. No Default; Must be specified.Do not specify the full object name; "Capacitor" is assumed for the object class.  Example:<br><br>Capacitor=cap1 |
| 4 | type | {Current \| voltage \| kvar \| PF \| time } Control type.  Specify the ONsetting and OFFsetting appropriately with the type of control. (See help for ONsetting) |
| 5 | PTratio | Ratio of the PT that converts the monitored voltage to the control voltage. Default is 60.  If the capacitor is Wye, the 1st phase line-to-neutral voltage is monitored.  Else, the line-to-line voltage (1st - 2nd phase) is monitored. |
| 6 | CTratio | Ratio of the CT from line amps to control ampere setting for current and kvar control types.  |
| 7 | ONsetting | Value at which the control arms to switch the capacitor ON (or ratchet up a step).  <br><br>Type of Control:<br><br>Current: Line Amps / CTratio<br>Voltage: Line-Neutral (or Line-Line for delta) Volts / PTratio<br>kvar:    Total kvar, all phases (3-phase for pos seq model). This is directional. <br>PF:      Power Factor, Total power in monitored terminal. Negative for Leading. <br>Time:    Hrs from Midnight as a floating point number (decimal). 7:30am would be entered as 7.5. |
| 8 | OFFsetting | Value at which the control arms to switch the capacitor OFF. (See help for ONsetting)For Time control, is OK to have Off time the next day ( < On time) |
| 9 | Delay | Time delay, in seconds, from when the control is armed before it sends out the switching command to turn ON.  The control may reset before the action actually occurs. This is used to determine which capacity control will act first. Default is 15.  You may specify any floating point number to achieve a model of whatever condition is necessary. |
| 10 | VoltOverride | {Yes \| No}  Default is No.  Switch to indicate whether VOLTAGE OVERRIDE is to be considered. Vmax and Vmin must be set to reasonable values if this property is Yes. |
| 11 | Vmax | Maximum voltage, in volts.  If the voltage across the capacitor divided by the PTRATIO is greater than this voltage, the capacitor will switch OFF regardless of other control settings. Default is 126 (goes with a PT ratio of 60 for 12.47 kV system). |
| 12 | Vmin | Minimum voltage, in volts.  If the voltage across the capacitor divided by the PTRATIO is less than this voltage, the capacitor will switch ON regardless of other control settings. Default is 115 (goes with a PT ratio of 60 for 12.47 kV system). |
| 13 | DelayOFF | Time delay, in seconds, for control to turn OFF when present state is ON. Default is 15. |
| 14 | DeadTime | Dead time after capacitor is turned OFF before it can be turned back ON. Default is 300 sec. |
| 15 | CTPhase | Number of the phase being monitored for CURRENT control or one of {AVG \| MAX \| MIN} for all phases. Default=1. If delta or L-L connection, enter the first or the two phases being monitored [1-2, 2-3, 3-1]. Must be less than the number of phases. Does not apply to kvar control which uses all phases by default. |
| 16 | PTPhase | Number of the phase being monitored for VOLTAGE control or one of {AVG \| MAX \| MIN} for all phases. Default=1. If delta or L-L connection, enter the first or the two phases being monitored [1-2, 2-3, 3-1]. Must be less than the number of phases. Does not apply to kvar control which uses all phases by default. |
| 17 | VBus | Name of bus to use for voltage override function. Default is bus at monitored terminal. Sometimes it is useful to monitor a bus in another location to emulate various DMS control algorithms. |
| 18 | EventLog | {Yes/True\* \| No/False} Default is YES for CapControl. Log control actions to Eventlog. |
| 19 | UserModel | Name of DLL containing user-written CapControl model, overriding the default model.  Set to "none" to negate previous setting.  |
| 20 | UserData | String (in quotes or parentheses if necessary) that gets passed to the user-written CapControl model Edit function for defining the data required for that model.  |
| 21 | pctMinkvar | For PF control option, min percent of total bank kvar at which control will close capacitor switch. Default = 50. |
| 22 | Reset | {Yes \| No} If Yes, forces Reset of this CapControl. |
| 23 | basefreq | Base Frequency for ratings. |
| 24 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 25 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `ESPVLControl` properties

| Number | Name | Description |
| - | - | - |
| 1 | Element | Full object name of the circuit element, typically a line or transformer, which the control is monitoring. There is no default; must be specified. |
| 2 | Terminal | Number of the terminal of the circuit element to which the ESPVLControl control is connected. 1 or 2, typically.  Default is 1. Make sure you have the direction on the power matching the sign of kWLimit. |
| 3 | Type | Type of controller.  1= System Controller; 2= Local controller.  |
| 4 | kWBand | Bandwidth (kW) of the dead band around the target limit.No dispatch changes are attempted if the power in the monitored terminal stays within this band. |
| 5 | kvarlimit | Max kvar to be delivered through the element.  Uses same dead band as kW. |
| 6 | LocalControlList | Array list of ESPVLControl local controller objects to be dispatched by System Controller. If not specified, all ESPVLControl devices with type=local in the circuit not attached to another controller are assumed to be part of this controller's fleet. |
| 7 | LocalControlWeights | Array of proportional weights corresponding to each ESPVLControl local controller in the LocalControlList. |
| 8 | PVSystemList | Array list of PVSystem objects to be dispatched by a Local Controller.  |
| 9 | PVSystemWeights | Array of proportional weights corresponding to each PVSystem in the PVSystemList. |
| 10 | StorageList | Array list of Storage objects to be dispatched by Local Controller.  |
| 11 | StorageWeights | Array of proportional weights corresponding to each Storage object in the StorageControlList. |
| 12 | Forecast | Loadshape object containing daily forecast. |
| 13 | basefreq | Base Frequency for ratings. |
| 14 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 15 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `ExpControl` properties

| Number | Name | Description |
| - | - | - |
| 1 | PVSystemList | Array list of PVSystems to be controlled.<br><br>If not specified, all PVSystems in the circuit are assumed to be controlled by this ExpControl. |
| 2 | Vreg | Per-unit voltage at which reactive power is zero; defaults to 1.0.<br><br>This may dynamically self-adjust when VregTau > 0, limited by VregMin and VregMax.If imput as 0, Vreg will be initialized from a snapshot solution with no inverter Q.The equilibrium point of reactive power is also affected by Qbias |
| 3 | Slope | Per-unit reactive power injection / per-unit voltage deviation from Vreg; defaults to 50.<br><br>Unlike InvControl, base reactive power is constant at the inverter kva rating. |
| 4 | VregTau | Time constant for adaptive Vreg. Defaults to 1200 seconds.<br><br>When the control injects or absorbs reactive power due to a voltage deviation from the Q=0 crossing of the volt-var curve, the Q=0 crossing will move toward the actual terminal voltage with this time constant. Over time, the effect is to gradually bring inverter reactive power to zero as the grid voltage changes due to non-solar effects. If zero, then Vreg stays fixed. IEEE1547-2018 requires adjustability from 300s to 5000s |
| 5 | Qbias | Equilibrium per-unit reactive power when V=Vreg; defaults to 0.<br><br>Enter > 0 for lagging (capacitive) bias, < 0 for leading (inductive) bias. |
| 6 | VregMin | Lower limit on adaptive Vreg; defaults to 0.95 per-unit |
| 7 | VregMax | Upper limit on adaptive Vreg; defaults to 1.05 per-unit |
| 8 | QmaxLead | Limit on leading (inductive) reactive power injection, in per-unit of base kva; defaults to 0.44.For Category A inverters per P1547/D7, set this value to 0.25.<br><br>Regardless of QmaxLead, the reactive power injection is still limited by dynamic headroom when actual real power output exceeds 0% |
| 9 | QmaxLag | Limit on lagging (capacitive) reactive power injection, in per-unit of base kva; defaults to 0.44.<br><br>For Category A inverters per P1547/D7, set this value to 0.25.Regardless of QmaxLag, the reactive power injection is still limited by dynamic headroom when actual real power output exceeds 0% |
| 10 | EventLog | {Yes/True \| No/False\*} Default is No for ExpControl. Log control actions to Eventlog. |
| 11 | DeltaQ_factor | Convergence parameter; Defaults to 0.7. <br><br>Sets the maximum change (in per unit) from the prior var output level to the desired var output level during each control iteration. If numerical instability is noticed in solutions such as var sign changing from one control iteration to the next and voltages oscillating between two values with some separation, this is an indication of numerical instability (use the EventLog to diagnose). If the maximum control iterations are exceeded, and no numerical instability is seen in the EventLog of via monitors, then try increasing the value of this parameter to reduce the number of control iterations needed to achieve the control criteria, and move to the power flow solution. |
| 12 | PreferQ | {Yes/True \| No/False\*} Default is No for ExpControl.<br><br>Curtails real power output as needed to meet the reactive power requirement. IEEE1547-2018 requires Yes, but the default is No for backward compatibility of OpenDSS models. |
| 13 | Tresponse | Open-loop response time for changes in Q.<br><br>The value of Q reaches 90% of the target change within Tresponse, which corresponds to a low-pass filter having tau = Tresponse / 2.3026. The behavior is similar to LPFTAU in InvControl, but here the response time is input instead of the time constant. IEEE1547-2018 default is 10s for Catagory A and 5s for Category B, adjustable from 1s to 90s for both categories. However, the default is 0 for backward compatibility of OpenDSS models. |
| 14 | basefreq | Base Frequency for ratings. |
| 15 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 16 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `Fuse` properties

| Number | Name | Description |
| - | - | - |
| 1 | MonitoredObj | Full object name of the circuit element, typically a line, transformer, load, or generator, to which the Fuse is connected. This is the "monitored" element. There is no default; must be specified. |
| 2 | MonitoredTerm | Number of the terminal of the circuit element to which the Fuse is connected. 1 or 2, typically.  Default is 1. |
| 3 | SwitchedObj | Name of circuit element switch that the Fuse controls. Specify the full object name.Defaults to the same as the Monitored element. This is the "controlled" element. |
| 4 | SwitchedTerm | Number of the terminal of the controlled element in which the switch is controlled by the Fuse. 1 or 2, typically.  Default is 1.  Assumes all phases of the element have a fuse of this type. |
| 5 | FuseCurve | Name of the TCC Curve object that determines the fuse blowing.  Must have been previously defined as a TCC_Curve object. Default is "Tlink". Multiplying the current values in the curve by the "RatedCurrent" value gives the actual current. |
| 6 | RatedCurrent | Multiplier or actual phase amps for the phase TCC curve.  Defaults to 1.0. |
| 7 | Delay | Fixed delay time (sec) added to Fuse blowing time determined from the TCC curve. Default is 0.0. Used to represent fuse clearing time or any other delay. |
| 8 | Action | {Trip/Open \| Close}  Action that overrides the Fuse control. Simulates manual control on Fuse "Trip" or "Open" causes the controlled element to open and lock out. "Close" causes the controlled element to close and the Fuse to reset. |
| 9 | basefreq | Base Frequency for ratings. |
| 10 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 11 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `GenDispatcher` properties

| Number | Name | Description |
| - | - | - |
| 1 | Element | Full object name of the circuit element, typically a line or transformer, which the control is monitoring. There is no default; must be specified. |
| 2 | Terminal | Number of the terminal of the circuit element to which the GenDispatcher control is connected. 1 or 2, typically.  Default is 1. Make sure you have the direction on the power matching the sign of kWLimit. |
| 3 | kWLimit | kW Limit for the monitored element. The generators are dispatched to hold the power in band. |
| 4 | kWBand | Bandwidth (kW) of the dead band around the target limit.No dispatch changes are attempted if the power in the monitored terminal stays within this band. |
| 5 | kvarlimit | Max kvar to be delivered through the element.  Uses same dead band as kW. |
| 6 | GenList | Array list of generators to be dispatched.  If not specified, all generators in the circuit are assumed dispatchable. |
| 7 | basefreq | Base Frequency for ratings. |
| 8 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 9 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `InvControl` properties

| Number | Name | Description |
| - | - | - |
| 1 | DERList | Array list of PVSystem and/or Storage elements to be controlled. If not specified, all PVSystem and Storage in the circuit are assumed to be controlled by this control. <br><br>No capability of hierarchical control between two controls for a single element is implemented at this time. |
| 2 | Mode | Smart inverter function in which the InvControl2 will control the PC elements specified in DERList, according to the options below:<br><br>Must be one of: {VOLTVAR\* \| VOLTWATT \| DYNAMICREACCURR \| WATTPF \| WATTVAR} <br>if the user desires to use modes simultaneously, then set the CombiMode property. Setting the Mode to any valid value disables combination mode.<br><br>In volt-var mode (Default). This mode attempts to CONTROL the vars, according to one or two volt-var curves, depending on the monitored voltages, present active power output, and the capabilities of the PVSystem/Storage. <br><br>In volt-watt mode. This mode attempts to LIMIT the watts, according to one defined volt-watt curve, depending on the monitored voltages and the capabilities of the PVSystem/Storage. <br><br>In dynamic reactive current mode. This mode attempts to increasingly counter deviations by CONTROLLING vars, depending on the monitored voltages, present active power output, and the capabilities of the of the PVSystem/Storage.<br><br>In watt-pf mode. This mode attempts to CONTROL the vars, according to a watt-pf curve, depending on the present active power output, and the capabilities of the PVSystem/Storage. <br><br>In watt-var mode. This mode attempts to CONTROL the vars, according to a watt-var curve, depending on the present active power output, and the capabilities of the PVSystem/Storage.  |
| 3 | CombiMode | Combination of smart inverter functions in which the InvControl2 will control the PC elements in DERList, according to the options below: <br><br>Must be a combination of the following: {VV_VW \| VV_DRC}. Default is to not set this property, in which case the single control mode in Mode is active.  <br><br>In combined VV_VW mode, both volt-var and volt-watt control modes are active simultaneously.  See help individually for volt-var mode and volt-watt mode in Mode property.<br>Note that the PVSystem/Storage will attempt to achieve both the volt-watt and volt-var set-points based on the capabilities of the inverter in the PVSystem/Storage (kVA rating, etc), any limits set on maximum active power,<br><br>In combined VV_DRC, both the volt-var and the dynamic reactive current modes are simultaneously active. |
| 4 | vvc_curve1 | Required for VOLTVAR mode. <br><br>Name of the XYCurve object containing the volt-var curve. The positive values of the y-axis of the volt-var curve represent values in pu of the provided base reactive power. The negative values of the y-axis are values in pu of the absorbed base reactive power. <br>Provided and absorbed base reactive power values are defined in the RefReactivePower property<br><br>Units for the x-axis are per-unit voltage, which may be in per unit of the rated voltage for the PVSystem/Storage, or may be in per unit of the average voltage at the terminals over a user-defined number of prior solutions.  |
| 5 | hysteresis_offset | Required for VOLTVAR mode, and defaults to 0. <br><br>for the times when the terminal voltage is decreasing, this is the off-set in per-unit voltage of a curve whose shape is the same as vvc_curve. It is offset by a certain negative value of per-unit voltage, which is defined by the base quantity for the x-axis of the volt-var curve (see help for voltage_curvex_ref)<br><br>if the PVSystem/Storage terminal voltage has been increasing, and has not changed directions, utilize vvc_curve1 for the volt-var response. <br><br>if the PVSystem/Storage terminal voltage has been increasing and changes directions and begins to decrease, then move from utilizing vvc_curve1 to a volt-var curve of the same shape, but offset by a certain per-unit voltage value. <br><br>Maintain the same per-unit available var output level (unless head-room has changed due to change in active power or kva rating of PVSystem/Storage).  Per-unit var values remain the same for this internally constructed second curve (hysteresis curve). <br><br>if the terminal voltage has been decreasing and changes directions and begins to increase , then move from utilizing the offset curve, back to the vvc_curve1 for volt-var response, but stay at the same per-unit available vars output level. |
| 6 | voltage_curvex_ref | Required for VOLTVAR and VOLTWATT modes, and defaults to rated.  Possible values are: {rated\|avg\|ravg}.  <br><br>Defines whether the x-axis values (voltage in per unit) for vvc_curve1 and the volt-watt curve corresponds to:<br><br>rated. The rated voltage for the PVSystem/Storage object (1.0 in the volt-var curve equals rated voltage).<br><br>avg. The average terminal voltage recorded over a certain number of prior power-flow solutions.<br>with the avg setting, 1.0 per unit on the x-axis of the volt-var curve(s) corresponds to the average voltage.<br>from a certain number of prior intervals.  See avgwindowlen parameter.<br><br>ravg. Same as avg, with the exception that the avgerage terminal voltage is divided by the rated voltage. |
| 7 | avgwindowlen | Required for VOLTVAR mode and VOLTWATT mode, and defaults to 0 seconds (0s). <br><br>Sets the length of the averaging window over which the average PVSystem/Storage terminal voltage is calculated. <br><br>Units are indicated by appending s, m, or h to the integer value. <br><br>The averaging window will calculate the average PVSystem/Storage terminal voltage over the specified period of time, up to and including the last power flow solution. <br><br>Note, if the solution stepsize is larger than the window length, then the voltage will be assumed to have been constant over the time-frame specified by the window length. |
| 8 | voltwatt_curve | Required for VOLTWATT mode. <br><br>Name of the XYCurve object containing the volt-watt curve. <br><br>Units for the x-axis are per-unit voltage, which may be in per unit of the rated voltage for the PVSystem/Storage, or may be in per unit of the average voltage at the terminals over a user-defined number of prior solutions. <br><br>Units for the y-axis are either in one of the options described in the VoltwattYAxis property.  |
| 9 | DbVMin | Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.95 per-unit voltage (referenced to the PVSystem/Storage object rated voltage or a windowed average value). <br><br>This parameter is the minimum voltage that defines the voltage dead-band within which no reactive power is allowed to be generated.  |
| 10 | DbVMax | Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 1.05 per-unit voltage (referenced to the PVSystem object rated voltage or a windowed average value). <br><br>This parameter is the maximum voltage that defines the voltage dead-band within which no reactive power is allowed to be generated.  |
| 11 | ArGraLowV | Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.1  <br><br>This is a gradient, expressed in unit-less terms of %/%, to establish the ratio by which percentage capacitive reactive power production is increased as the  percent delta-voltage decreases below DbVMin. <br><br>Percent delta-voltage is defined as the present PVSystem/Storage terminal voltage minus the moving average voltage, expressed as a percentage of the rated voltage for the PVSystem/Storage object. <br><br>Note, the moving average voltage for the dynamic reactive current mode is different than the moving average voltage for the volt-watt and volt-var modes. |
| 12 | ArGraHiV | Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.1  <br><br>This is a gradient, expressed in unit-less terms of %/%, to establish the ratio by which percentage inductive reactive power production is increased as the  percent delta-voltage decreases above DbVMax. <br><br>Percent delta-voltage is defined as the present PVSystem/Storage terminal voltage minus the moving average voltage, expressed as a percentage of the rated voltage for the PVSystem/Storage object. <br><br>Note, the moving average voltage for the dynamic reactive current mode is different than the mmoving average voltage for the volt-watt and volt-var modes. |
| 13 | DynReacavgwindowlen | Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 1 seconds (1s). do not use a value smaller than 1.0 <br><br>Sets the length of the averaging window over which the average PVSystem/Storage terminal voltage is calculated for the dynamic reactive current mode. <br><br>Units are indicated by appending s, m, or h to the integer value. <br><br>Typically this will be a shorter averaging window than the volt-var and volt-watt averaging window.<br><br>The averaging window will calculate the average PVSystem/Storage terminal voltage over the specified period of time, up to and including the last power flow solution.  Note, if the solution stepsize is larger than the window length, then the voltage will be assumed to have been constant over the time-frame specified by the window length. |
| 14 | deltaQ_Factor | Required for the VOLTVAR and DYNAMICREACCURR modes.  Defaults to -1.0. <br><br>Defining -1.0, OpenDSS takes care internally of delta_Q itself. It tries to improve convergence as well as speed up process<br><br>Sets the maximum change (in per unit) from the prior var output level to the desired var output level during each control iteration. <br><br><br>if numerical instability is noticed in solutions such as var sign changing from one control iteration to the next and voltages oscillating between two values with some separation, this is an indication of numerical instability (use the EventLog to diagnose). <br><br>if the maximum control iterations are exceeded, and no numerical instability is seen in the EventLog of via monitors, then try increasing the value of this parameter to reduce the number of control iterations needed to achieve the control criteria, and move to the power flow solution. |
| 15 | VoltageChangeTolerance | Defaults to 0.0001 per-unit voltage.  This parameter should only be modified by advanced users of the InvControl.  <br><br>Tolerance in pu of the control loop convergence associated to the monitored voltage in pu. This value is compared with the difference of the monitored voltage in pu of the current and previous control iterations of the control loop<br><br>This voltage tolerance value plus the var/watt tolerance value (VarChangeTolerance/ActivePChangeTolerance) determine, together, when to stop control iterations by the InvControl. <br><br>If an InvControl2 is controlling more than one PVSystem/Storage, each PVSystem/Storage has this quantity calculated independently, and so an individual PVSystem/Storage may reach the tolerance within different numbers of control iterations. |
| 16 | VarChangeTolerance | Required for VOLTVAR and DYNAMICREACCURR modes.  Defaults to 0.025 per unit of the base provided or absorbed reactive power described in the RefReactivePower property This parameter should only be modified by advanced users of the InvControl. <br><br>Tolerance in pu of the convergence of the control loop associated with reactive power. For the same control iteration, this value is compared to the difference, as an absolute value (without sign), between the desired reactive power value in pu and the output reactive power in pu of the controlled element.<br><br>This reactive power tolerance value plus the voltage tolerance value (VoltageChangeTolerance) determine, together, when to stop control iterations by the InvControl.  <br><br>If an InvControl2 is controlling more than one PVSystem/Storage, each PVSystem/Storage has this quantity calculated independently, and so an individual PVSystem/Storage may reach the tolerance within different numbers of control iterations. |
| 17 | VoltwattYAxis | Required for VOLTWATT mode.  Must be one of: {PMPPPU\* \| PAVAILABLEPU\| PCTPMPPPU \| KVARATINGPU}.  The default is PMPPPU.  <br><br>Units for the y-axis of the volt-watt curve while in volt-watt mode. <br><br>When set to PMPPPU. The y-axis corresponds to the value in pu of Pmpp property of the PVSystem. <br><br>When set to PAVAILABLEPU. The y-axis corresponds to the value in pu of the available active power of the PVSystem. <br><br>When set to PCTPMPPPU. The y-axis corresponds to the value in pu of the power Pmpp multiplied by 1/100 of the %Pmpp property of the PVSystem.<br><br>When set to KVARATINGPU. The y-axis corresponds to the value in pu of the kVA property of the PVSystem. |
| 18 | RateofChangeMode | Required for VOLTWATT and VOLTVAR mode.  Must be one of: {INACTIVE\* \| LPF \| RISEFALL }.  The default is INACTIVE.  <br><br>Auxiliary option that aims to limit the changes of the desired reactive power and the active power limit between time steps, the alternatives are listed below: <br><br>INACTIVE. It indicates there is no limit on rate of change imposed for either active or reactive power output. <br><br>LPF. A low-pass RC filter is applied to the desired reactive power and/or the active power limit to determine the output power as a function of a time constant defined in the LPFTau property. <br><br>RISEFALL. A rise and fall limit in the change of active and/or reactive power expressed in terms of pu power per second, defined in the RiseFallLimit, is applied to the desired reactive power and/or the active power limit.  |
| 19 | LPFTau | Not required. Defaults to 0 seconds. <br><br>Filter time constant of the LPF option of the RateofChangeMode property. The time constant will cause the low-pass filter to achieve 95% of the target value in 3 time constants. |
| 20 | RiseFallLimit | Not required.  Defaults to no limit (-1). Must be -1 (no limit) or a positive value.  <br><br>Limit in power in pu per second used by the RISEFALL option of the RateofChangeMode property.The base value for this ramp is defined in the RefReactivePower property and/or in VoltwattYAxis. |
| 21 | deltaP_Factor | Required for the VOLTWATT modes.  Defaults to -1.0. <br><br>Defining -1.0, OpenDSS takes care internally of delta_P itself. It tries to improve convergence as well as speed up process<br><br>Defining between 0.05 and 1.0, it sets the maximum change (in unit of the y-axis) from the prior active power output level to the desired active power output level during each control iteration. <br><br><br>If numerical instability is noticed in solutions such as active power changing substantially from one control iteration to the next and/or voltages oscillating between two values with some separation, this is an indication of numerical instability (use the EventLog to diagnose). <br><br>If the maximum control iterations are exceeded, and no numerical instability is seen in the EventLog of via monitors, then try increasing the value of this parameter to reduce the number of control iterations needed to achieve the control criteria, and move to the power flow solution. |
| 22 | EventLog | {Yes/True\* \| No/False} Default is YES for InvControl. Log control actions to Eventlog. |
| 23 | RefReactivePower | Required for any mode that has VOLTVAR, DYNAMICREACCURR and WATTVAR. Defaults to VARAVAL.<br><br>Defines the base reactive power for both the provided and absorbed reactive power, according to one of the following options: <br><br>VARAVAL. The base values for the provided and absorbed reactive power are equal to the available reactive power.<br><br>VARMAX: The base values of the provided and absorbed reactive power are equal to the value defined in the kvarMax and kvarMaxAbs properties, respectively. |
| 24 | ActivePChangeTolerance | Required for VOLTWATT. Default is 0.01<br><br>Tolerance in pu of the convergence of the control loop associated with active power. For the same control iteration, this value is compared to the difference between the active power limit in pu resulted from the convergence process and the one resulted from the volt-watt function.<br><br>This reactive power tolerance value plus the voltage tolerance value (VoltageChangeTolerance) determine, together, when to stop control iterations by the InvControl.  <br><br>If an InvControl2 is controlling more than one PVSystem/Storage, each PVSystem/Storage has this quantity calculated independently, and so an individual PVSystem/Storage may reach the tolerance within different numbers of control iterations. |
| 25 | monVoltageCalc | Number of the phase being monitored or one of {AVG \| MAX \| MIN} for all phases. Default=AVG.  |
| 26 | monBus | Name of monitored bus used by the voltage-dependente control modes. Default is bus of the controlled PVSystem/Storage or Storage. |
| 27 | MonBusesVbase | Array list of rated voltages of the buses and their nodes presented in the monBus property. This list may have different line-to-line and/or line-to-ground voltages. |
| 28 | voltwattCH_curve | Required for VOLTWATT mode for Storage element in CHARGING state. <br><br>The name of an XYCurve object that describes the variation in active power output (in per unit of maximum active power outut for the Storage). <br><br>Units for the x-axis are per-unit voltage, which may be in per unit of the rated voltage for the Storage, or may be in per unit of the average voltage at the terminals over a user-defined number of prior solutions. <br><br>Units for the y-axis are either in: (1) per unit of maximum active power output capability of the Storage, or (2) maximum available active power output capability (defined by the parameter: VoltwattYAxis), corresponding to the terminal voltage (x-axis value in per unit). <br><br>No default -- must be specified for VOLTWATT mode for Storage element in CHARGING state. |
| 29 | wattpf_curve | Required for WATTPF mode.<br><br>Name of the XYCurve object containing the watt-pf curve.<br>The positive values of the y-axis are positive power factor values. The negative values of the the y-axis are negative power factor values. When positive, the output reactive power has the same direction of the output active power, and when negative, it has the opposite direction.<br>Units for the x-axis are per-unit output active power, and the base active power is the Pmpp for PVSystem and kWrated for Storage.<br><br>The y-axis represents the power factor and the reference is power factor equal to 0. <br><br>For example, if the user wants to define the following XY coordinates: (0, 0.9); (0.2, 0.9); (0.5, -0.9); (1, -0.9).<br>Try to plot them considering the y-axis reference equal to unity power factor.<br><br>The user needs to translate this curve into a plot in which the y-axis reference is equal to 0 power factor.It means that two new XY coordinates need to be included, in this case they are: (0.35, 1); (0.35, -1).<br>Try to plot them considering the y-axis reference equal to 0 power factor.<br>The discontinity in 0.35pu is not a problem since var is zero for either power factor equal to 1 or -1. |
| 30 | wattvar_curve | Required for WATTVAR mode. <br><br>Name of the XYCurve object containing the watt-var curve. The positive values of the y-axis of the watt-var curve represent values in pu of the provided base reactive power. The negative values of the y-axis are values in pu of the absorbed base reactive power. <br>Provided and absorbed base reactive power values are defined in the RefReactivePower property.<br><br>Units for the x-axis are per-unit output active power, and the base active power is the Pmpp for PVSystem and kWrated for Storage. |
| 31 | VV_RefReactivePower | Deprecated, use RefReactivePower instead. |
| 32 | PVSystemList | Deprecated, use DERList instead. |
| 33 | basefreq | Base Frequency for ratings. |
| 34 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 35 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `Recloser` properties

| Number | Name | Description |
| - | - | - |
| 1 | MonitoredObj | Full object name of the circuit element, typically a line, transformer, load, or generator, to which the Recloser's PT and/or CT are connected. This is the "monitored" element. There is no default; must be specified. |
| 2 | MonitoredTerm | Number of the terminal of the circuit element to which the Recloser is connected. 1 or 2, typically.  Default is 1. |
| 3 | SwitchedObj | Name of circuit element switch that the Recloser controls. Specify the full object name.Defaults to the same as the Monitored element. This is the "controlled" element. |
| 4 | SwitchedTerm | Number of the terminal of the controlled element in which the switch is controlled by the Recloser. 1 or 2, typically.  Default is 1. |
| 5 | NumFast | Number of Fast (fuse saving) operations.  Default is 1. (See "Shots") |
| 6 | PhaseFast | Name of the TCC Curve object that determines the Phase Fast trip.  Must have been previously defined as a TCC_Curve object. Default is "A". Multiplying the current values in the curve by the "phasetrip" value gives the actual current. |
| 7 | PhaseDelayed | Name of the TCC Curve object that determines the Phase Delayed trip.  Must have been previously defined as a TCC_Curve object. Default is "D".Multiplying the current values in the curve by the "phasetrip" value gives the actual current. |
| 8 | GroundFast | Name of the TCC Curve object that determines the Ground Fast trip.  Must have been previously defined as a TCC_Curve object. Default is none (ignored). Multiplying the current values in the curve by the "groundtrip" value gives the actual current. |
| 9 | GroundDelayed | Name of the TCC Curve object that determines the Ground Delayed trip.  Must have been previously defined as a TCC_Curve object. Default is none (ignored).Multiplying the current values in the curve by the "groundtrip" value gives the actual current. |
| 10 | PhaseTrip | Multiplier or actual phase amps for the phase TCC curve.  Defaults to 1.0. |
| 11 | GroundTrip | Multiplier or actual ground amps (3I0) for the ground TCC curve.  Defaults to 1.0. |
| 12 | PhaseInst | Actual amps for instantaneous phase trip which is assumed to happen in 0.01 sec + Delay Time. Default is 0.0, which signifies no inst trip.  |
| 13 | GroundInst | Actual amps for instantaneous ground trip which is assumed to happen in 0.01 sec + Delay Time.Default is 0.0, which signifies no inst trip. |
| 14 | Reset | Reset time in sec for Recloser.  Default is 15.  |
| 15 | Shots | Total Number of fast and delayed shots to lockout.  Default is 4. This is one more than the number of reclose intervals. |
| 16 | RecloseIntervals | Array of reclose intervals.  Default for Recloser is (0.5, 2.0, 2.0) seconds. A locked out Recloser must be closed manually (action=close). |
| 17 | Delay | Fixed delay time (sec) added to Recloser trip time. Default is 0.0. Used to represent breaker time or any other delay. |
| 18 | Action | {Trip/Open \| Close}  Action that overrides the Recloser control. Simulates manual control on recloser "Trip" or "Open" causes the controlled element to open and lock out. "Close" causes the controlled element to close and the Recloser to reset to its first operation. |
| 19 | TDPhFast | Time dial for Phase Fast trip curve. Multiplier on time axis of specified curve. Default=1.0. |
| 20 | TDGrFast | Time dial for Ground Fast trip curve. Multiplier on time axis of specified curve. Default=1.0. |
| 21 | TDPhDelayed | Time dial for Phase Delayed trip curve. Multiplier on time axis of specified curve. Default=1.0. |
| 22 | TDGrDelayed | Time dial for Ground Delayed trip curve. Multiplier on time axis of specified curve. Default=1.0. |
| 23 | basefreq | Base Frequency for ratings. |
| 24 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 25 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `RegControl` properties

| Number | Name | Description |
| - | - | - |
| 1 | transformer | Name of Transformer or AutoTrans element to which the RegControl is connected. Do not specify the full object name; "Transformer" or "AutoTrans" is assumed for the object class.  Example:<br><br>Transformer=Xfmr1 |
| 2 | winding | Number of the winding of the transformer element that the RegControl is monitoring. 1 or 2, typically.  Side Effect: Sets TAPWINDING property to the same winding. |
| 3 | vreg | Voltage regulator setting, in VOLTS, for the winding being controlled.  Multiplying this value times the ptratio should yield the voltage across the WINDING of the controlled transformer. Default is 120.0 |
| 4 | band | Bandwidth in VOLTS for the controlled bus (see help for ptratio property).  Default is 3.0 |
| 5 | ptratio | Ratio of the PT that converts the controlled winding voltage to the regulator control voltage. Default is 60.  If the winding is Wye, the line-to-neutral voltage is used.  Else, the line-to-line voltage is used. SIDE EFFECT: Also sets RemotePTRatio property. |
| 6 | CTprim | Rating, in Amperes, of the primary CT rating for which the line amps convert to control rated amps.The typical default secondary ampere rating is 0.2 Amps (check with manufacturer specs). Current at which the LDC voltages match the R and X settings. |
| 7 | R | R setting on the line drop compensator in the regulator, expressed in VOLTS. |
| 8 | X | X setting on the line drop compensator in the regulator, expressed in VOLTS. |
| 9 | bus | Name of a bus (busname.nodename) in the system to use as the controlled bus instead of the bus to which the transformer winding is connected or the R and X line drop compensator settings.  Do not specify this value if you wish to use the line drop compensator settings.  Default is null string. Assumes the base voltage for this bus is the same as the transformer winding base specified above. Note: This bus (1-phase) WILL BE CREATED by the regulator control upon SOLVE if not defined by some other device. You can specify the node of the bus you wish to sample (defaults to 1). If specified, the RegControl is redefined as a 1-phase device since only one voltage is used. |
| 10 | delay | Time delay, in seconds, from when the voltage goes out of band to when the tap changing begins. This is used to determine which regulator control will act first. Default is 15.  You may specify any floating point number to achieve a model of whatever condition is necessary. |
| 11 | reversible | {Yes \|No\*} Indicates whether or not the regulator can be switched to regulate in the reverse direction. Default is No.Typically applies only to line regulators and not to LTC on a substation transformer. |
| 12 | revvreg | Voltage setting in volts for operation in the reverse direction. |
| 13 | revband | Bandwidth for operating in the reverse direction. |
| 14 | revR | R line drop compensator setting for reverse direction. |
| 15 | revX | X line drop compensator setting for reverse direction. |
| 16 | tapdelay | Delay in sec between tap changes. Default is 2. This is how long it takes between changes after the first change. |
| 17 | debugtrace | {Yes \| No\* }  Default is no.  Turn this on to capture the progress of the regulator model for each control iteration.  Creates a separate file for each RegControl named "REG_name.CSV". |
| 18 | maxtapchange | Maximum allowable tap change per control iteration in STATIC control mode.  Default is 16. <br><br>Set this to 1 to better approximate actual control action. <br><br>Set this to 0 to fix the tap in the current position. |
| 19 | inversetime | {Yes \| No\* } Default is no.  The time delay is adjusted inversely proportional to the amount the voltage is outside the band down to 10%. |
| 20 | tapwinding | Winding containing the actual taps, if different than the WINDING property. Defaults to the same winding as specified by the WINDING property. |
| 21 | vlimit | Voltage Limit for bus to which regulated winding is connected (e.g. first customer). Default is 0.0. Set to a value greater then zero to activate this function. |
| 22 | PTphase | For multi-phase transformers, the number of the phase being monitored or one of { MAX \| MIN} for all phases. Default=1. Must be less than or equal to the number of phases. Ignored for regulated bus. |
| 23 | revThreshold | kW reverse power threshold for reversing the direction of the regulator. Default is 100.0 kw. |
| 24 | revDelay | Time Delay in seconds (s) for executing the reversing action once the threshold for reversing has been exceeded. Default is 60 s. |
| 25 | revNeutral | {Yes \| No\*} Default is no. Set this to Yes if you want the regulator to go to neutral in the reverse direction or in cogen operation. |
| 26 | EventLog | {Yes/True\* \| No/False} Default is YES for regulator control. Log control actions to Eventlog. |
| 27 | RemotePTRatio | When regulating a bus (the Bus= property is set), the PT ratio required to convert actual voltage at the remote bus to control voltage. Is initialized to PTratio property. Set this property after setting PTratio. |
| 28 | TapNum | An integer number indicating the tap position that the controlled transformer winding tap position is currently at, or is being set to.  If being set, and the value is outside the range of the transformer min or max tap, then set to the min or max tap position as appropriate. Default is 0 |
| 29 | Reset | {Yes \| No} If Yes, forces Reset of this RegControl. |
| 30 | LDC_Z | Z value for Beckwith LDC_Z control option. Volts adjustment at rated control current. |
| 31 | rev_Z | Reverse Z value for Beckwith LDC_Z control option. |
| 32 | Cogen | {Yes\|No\*} Default is No. The Cogen feature is activated. Continues looking forward if power reverses, but switches to reverse-mode LDC values. |
| 33 | basefreq | Base Frequency for ratings. |
| 34 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 35 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `Relay` properties

| Number | Name | Description |
| - | - | - |
| 1 | MonitoredObj | Full object name of the circuit element, typically a line, transformer, load, or generator, to which the relay's PT and/or CT are connected. This is the "monitored" element. There is no default; must be specified. |
| 2 | MonitoredTerm | Number of the terminal of the circuit element to which the Relay is connected. 1 or 2, typically.  Default is 1. |
| 3 | SwitchedObj | Name of circuit element switch that the Relay controls. Specify the full object name.Defaults to the same as the Monitored element. This is the "controlled" element. |
| 4 | SwitchedTerm | Number of the terminal of the controlled element in which the switch is controlled by the Relay. 1 or 2, typically.  Default is 1. |
| 5 | type | One of a legal relay type:<br>Current<br>Voltage<br>Reversepower<br>46 (neg seq current)<br>47 (neg seq voltage)<br>Generic (generic over/under relay)<br><br>Default is overcurrent relay (Current) Specify the curve and pickup settings appropriate for each type. Generic relays monitor PC Element Control variables and trip on out of over/under range in definite time. |
| 6 | Phasecurve | Name of the TCC Curve object that determines the phase trip.  Must have been previously defined as a TCC_Curve object. Default is none (ignored). For overcurrent relay, multiplying the current values in the curve by the "phasetrip" value gives the actual current. |
| 7 | Groundcurve | Name of the TCC Curve object that determines the ground trip.  Must have been previously defined as a TCC_Curve object. Default is none (ignored).For overcurrent relay, multiplying the current values in the curve by the "groundtrip" valuw gives the actual current. |
| 8 | PhaseTrip | Multiplier or actual phase amps for the phase TCC curve.  Defaults to 1.0. |
| 9 | GroundTrip | Multiplier or actual ground amps (3I0) for the ground TCC curve.  Defaults to 1.0. |
| 10 | TDPhase | Time dial for Phase trip curve. Multiplier on time axis of specified curve. Default=1.0. |
| 11 | TDGround | Time dial for Ground trip curve. Multiplier on time axis of specified curve. Default=1.0. |
| 12 | PhaseInst | Actual  amps (Current relay) or kW (reverse power relay) for instantaneous phase trip which is assumed to happen in 0.01 sec + Delay Time. Default is 0.0, which signifies no inst trip. Use this value for specifying the Reverse Power threshold (kW) for reverse power relays. |
| 13 | GroundInst | Actual  amps for instantaneous ground trip which is assumed to happen in 0.01 sec + Delay Time.Default is 0.0, which signifies no inst trip. |
| 14 | Reset | Reset time in sec for relay.  Default is 15. If  |
| 15 | Shots | Number of shots to lockout.  Default is 4. This is one more than the number of reclose intervals. |
| 16 | RecloseIntervals | Array of reclose intervals. If none, specify "NONE". Default for overcurrent relay is (0.5, 2.0, 2.0) seconds. Default for a voltage relay is (5.0). In a voltage relay, this is  seconds after restoration of voltage that the reclose occurs. Reverse power relay is one shot to lockout, so this is ignored.  A locked out relay must be closed manually (set action=close). |
| 17 | Delay | Trip time delay (sec) for DEFINITE TIME relays. Default is 0.0 for current and voltage relays.  If >0 then this value is used instead of curves.  Used by Generic, RevPower, 46 and 47 relays. Defaults to 0.1 s for these relays. |
| 18 | Overvoltcurve | TCC Curve object to use for overvoltage relay.  Curve is assumed to be defined with per unit voltage values. Voltage base should be defined for the relay. Default is none (ignored). |
| 19 | Undervoltcurve | TCC Curve object to use for undervoltage relay.  Curve is assumed to be defined with per unit voltage values. Voltage base should be defined for the relay. Default is none (ignored). |
| 20 | kvbase | Voltage base (kV) for the relay. Specify line-line for 3 phase devices); line-neutral for 1-phase devices.  Relay assumes the number of phases of the monitored element.  Default is 0.0, which results in assuming the voltage values in the "TCC" curve are specified in actual line-to-neutral volts. |
| 21 | 47%Pickup | Percent voltage pickup for 47 relay (Neg seq voltage). Default is 2. Specify also base voltage (kvbase) and delay time value.    |
| 22 | 46BaseAmps | Base current, Amps, for 46 relay (neg seq current).  Used for establishing pickup and per unit I-squared-t. |
| 23 | 46%Pickup | Percent pickup current for 46 relay (neg seq current).  Default is 20.0.   When current exceeds this value \* BaseAmps, I-squared-t calc starts. |
| 24 | 46isqt | Negative Sequence I-squared-t trip value for 46 relay (neg seq current).  Default is 1 (trips in 1 sec for 1 per unit neg seq current).  Should be 1 to 99. |
| 25 | Variable | Name of variable in PC Elements being monitored.  Only applies to Generic relay. |
| 26 | overtrip | Trip setting (high value) for Generic relay variable.  Relay trips in definite time if value of variable exceeds this value. |
| 27 | undertrip | Trip setting (low value) for Generic relay variable.  Relay trips in definite time if value of variable is less than this value. |
| 28 | Breakertime | Fixed delay time (sec) added to relay time. Default is 0.0. Designed to represent breaker time or some other delay after a trip decision is made.Use Delay property for setting a fixed trip time delay.Added to trip time of current and voltage relays. Could use in combination with inst trip value to obtain a definite time overcurrent relay. |
| 29 | action | {Trip/Open \| Close}  Action that overrides the relay control. Simulates manual control on breaker. "Trip" or "Open" causes the controlled element to open and lock out. "Close" causes the controlled element to close and the relay to reset to its first operation. |
| 30 | basefreq | Base Frequency for ratings. |
| 31 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 32 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `StorageController` properties

| Number | Name | Description |
| - | - | - |
| 1 | Element | Full object name of the circuit element, typically a line or transformer, which the control is monitoring. There is no default; Must be specified. |
| 2 | Terminal | Number of the terminal of the circuit element to which the StorageController2 control is connected. 1 or 2, typically.  Default is 1. Make sure to select the proper direction on the power for the respective dispatch mode. |
| 3 | MonPhase | Number of the phase being monitored or one of {AVG \| MAX \| MIN} for all phases. Default=MAX. Must be less than the number of phases. Used in PeakShave, Follow, Support and I-PeakShave discharging modes and in PeakShaveLow, I-PeakShaveLow charging modes. For modes based on active power measurements, the value used by the control is the monitored one multiplied by the number of phases of the monitored element. |
| 4 | kWTarget | kW/kamps target for Discharging. The Storage element fleet is dispatched to try to hold the power/current in band at least until the Storage is depleted. The selection of power or current depends on the Discharge mode (PeakShave->kW, I-PeakShave->kamps). |
| 5 | kWTargetLow | kW/kamps target for Charging. The Storage element fleet is dispatched to try to hold the power/current in band at least until the Storage is fully charged. The selection of power or current depends on the charge mode (PeakShavelow->kW, I-PeakShavelow->kamps). |
| 6 | %kWBand | Bandwidth (% of Target kW/kamps) of the dead band around the kW/kamps target value. Default is 2% (+/-1%).No dispatch changes are attempted if the power in the monitored terminal stays within this band. |
| 7 | kWBand | Alternative way of specifying the bandwidth. (kW/kamps) of the dead band around the kW/kamps target value. Default is 2% of kWTarget (+/-1%).No dispatch changes are attempted if the power in the monitored terminal stays within this band. |
| 8 | %kWBandLow | Bandwidth (% of kWTargetLow) of the dead band around the kW/kamps low target value. Default is 2% (+/-1%).No charging is attempted if the power in the monitored terminal stays within this band. |
| 9 | kWBandLow | Alternative way of specifying the bandwidth. (kW/kamps) of the dead band around the kW/kamps low target value. Default is 2% of kWTargetLow (+/-1%).No charging is attempted if the power in the monitored terminal stays within this band. |
| 10 | ElementList | Array list of Storage elements to be controlled.  If not specified, all Storage elements in the circuit not presently dispatched by another controller are assumed dispatched by this controller. |
| 11 | Weights | Array of proportional weights corresponding to each Storage element in the ElementList. The needed kW or kvar to get back to center band is dispatched to each Storage element according to these weights. Default is to set all weights to 1.0. |
| 12 | ModeDischarge | {PeakShave\* \| Follow \| Support \| Loadshape \| Time \| Schedule \| I-PeakShave} Mode of operation for the DISCHARGE FUNCTION of this controller. <br><br>In PeakShave mode (Default), the control attempts to discharge Storage to keep power in the monitored element below the kWTarget. <br><br>In Follow mode, the control is triggered by time and resets the kWTarget value to the present monitored element power. It then attempts to discharge Storage to keep power in the monitored element below the new kWTarget. See TimeDischargeTrigger.<br><br>In Support mode, the control operates oppositely of PeakShave mode: Storage is discharged to keep kW power output up near the target. <br><br>In Loadshape mode, both charging and discharging precisely follows the per unit loadshape. Storage is discharged when the loadshape value is positive. <br><br>In Time mode, the Storage discharge is turned on at the specified %RatekW at the specified discharge trigger time in fractional hours.<br><br>In Schedule mode, the Tup, TFlat, and Tdn properties specify the up ramp duration, flat duration, and down ramp duration for the schedule. The schedule start time is set by TimeDischargeTrigger and the rate of discharge for the flat part is determined by %RatekW.<br><br>In I-PeakShave mode, the control attempts to discharge Storage to keep current in the monitored element below the target given in k-amps (thousands of amps), when this control mode is active, the property kWTarget will be expressed in k-amps.  |
| 13 | ModeCharge | {Loadshape \| Time\* \| PeakShaveLow \| I-PeakShaveLow} Mode of operation for the CHARGE FUNCTION of this controller. <br><br>In Loadshape mode, both charging and discharging precisely follows the per unit loadshape. Storage is charged when the loadshape value is negative. <br><br>In Time mode, the Storage charging FUNCTION is triggered at the specified %RateCharge at the specified charge trigger time in fractional hours.<br><br>In PeakShaveLow mode, the charging operation will charge the Storage fleet when the power at amonitored element is below a specified KW target (kWTarget_low). The Storage will charge as much power as necessary to keep the power within the deadband around kWTarget_low.<br><br>In I-PeakShaveLow mode, the charging operation will charge the Storage fleet when the current (Amps) at amonitored element is below a specified amps target (kWTarget_low). The Storage will charge as much power as necessary to keep the amps within the deadband around kWTarget_low. When this control mode is active, the property kWTarget_low will be expressed in k-amps and all the other parameters will be adjusted to match the amps (current) control criteria. |
| 14 | TimeDischargeTrigger | Default time of day (hr) for initiating Discharging of the fleet. During Follow or Time mode discharging is triggered at a fixed time each day at this hour. If Follow mode, Storage will be discharged to attempt to hold the load at or below the power level at the time of triggering. In Time mode, the discharge is based on the %RatekW property value. Set this to a negative value to ignore. Default is 12.0 for Follow mode; otherwise it is -1 (ignored).  |
| 15 | TimeChargeTrigger | Default time of day (hr) for initiating charging in Time control mode. Set this to a negative value to ignore. Default is 2.0.  (0200).When this value is >0 the Storage fleet is set to charging at this time regardless of other control criteria to make sure Storage is topped off for the next discharge cycle. |
| 16 | %RatekW | Sets the kW discharge rate in % of rated capacity for each element of the fleet. Applies to TIME control mode, SCHEDULE mode, or anytime discharging is triggered by time. |
| 17 | %RateCharge | Sets the kW charging rate in % of rated capacity for each element of the fleet. Applies to TIME control mode and anytime charging mode is entered due to a time trigger. |
| 18 | %Reserve | Use this property to change the % reserve for each Storage element under control of this controller. This might be used, for example, to allow deeper discharges of Storage or in case of emergency operation to use the remainder of the Storage element. |
| 19 | kWhTotal | (Read only). Total rated kWh energy Storage capacity of Storage elements controlled by this controller. |
| 20 | kWTotal | (Read only). Total rated kW power capacity of Storage elements controlled by this controller. |
| 21 | kWhActual | (Read only). Actual kWh stored of all controlled Storage elements.  |
| 22 | kWActual | (Read only). Actual kW output of all controlled Storage elements.  |
| 23 | kWneed | (Read only). KW needed to meet target. |
| 24 | Yearly | Dispatch loadshape object, If any, for Yearly solution Mode. |
| 25 | Daily | Dispatch loadshape object, If any, for Daily solution mode. |
| 26 | Duty | Dispatch loadshape object, If any, for Dutycycle solution mode. |
| 27 | EventLog | {Yes/True \| No/False} Default is No. Log control actions to Eventlog. |
| 28 | InhibitTime | Hours (integer) to inhibit Discharging after going into Charge mode. Default is 5. |
| 29 | Tup | Duration, hrs, of upramp part for SCHEDULE mode. Default is 0.25. |
| 30 | TFlat | Duration, hrs, of flat part for SCHEDULE mode. Default is 2.0. |
| 31 | Tdn | Duration, hrs, of downramp part for SCHEDULE mode. Default is 0.25. |
| 32 | kWThreshold | Threshold, kW, for Follow mode. kW has to be above this value for the Storage element to be dispatched on. Defaults to 75% of the kWTarget value. Must reset this property after setting kWTarget if you want a different value. |
| 33 | DispFactor | Defaults to 1 (disabled). Set to any value between 0 and 1 to enable this parameter.<br><br>Use this parameter to reduce the amount of power requested by the controller in each control iteration. It can be useful when maximum control iterations are exceeded due to numerical instability such as fleet being set to charging and idling in subsequent control iterations (check the Eventlog).  |
| 34 | ResetLevel | The level of charge required for allowing the storage to discharge again after reaching the reserve storage level. After reaching this level, the storage control  will not allow the storage device to discharge, forcing the storage to charge. Once the storage reaches thislevel, the storage will be able to discharge again. This value is a number between 0.2 and 1 |
| 35 | Seasons | With this property the user can specify the number of targets to be used by the controller using the list given at "SeasonTargets"/"SeasonTargetsLow", which can be used to dynamically adjust the storage controller during a QSTS simulation. The default value is 1. This property needs to be defined before defining SeasonTargets/SeasonTargetsLow. |
| 36 | SeasonTargets | An array of doubles specifying the targets to be used during a QSTS simulation. These targets will take effect only if SeasonRating=true. The number of targets cannot exceed the number of seasons defined at the SeasonSignal.The difference between the targets defined at SeasonTargets and SeasonTargetsLow is that SeasonTargets applies to discharging modes, while SeasonTargetsLow applies to charging modes. |
| 37 | SeasonTargetsLow | An array of doubles specifying the targets to be used during a QSTS simulation. These targets will take effect only if SeasonRating=true. The number of targets cannot exceed the number of seasons defined at the SeasonSignal.The difference between the targets defined at SeasonTargets and SeasonTargetsLow is that SeasonTargets applies to discharging modes, while SeasonTargetsLow applies to charging modes. |
| 38 | basefreq | Base Frequency for ratings. |
| 39 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 40 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `SwtControl` properties

| Number | Name | Description |
| - | - | - |
| 1 | SwitchedObj | Name of circuit element switch that the SwtControl operates. Specify the full object class and name. |
| 2 | SwitchedTerm | Terminal number of the controlled element switch. 1 or 2, typically.  Default is 1. |
| 3 | Action | {Open \| Close}  After specified delay time, and if not locked, causes the controlled switch to open or close.  |
| 4 | Lock | {Yes \| No} Delayed action. Sends CTRL_LOCK or CTRL_UNLOCK message to control queue. After delay time, controlled switch is locked in its present open / close state or unlocked. Switch will not respond to either manual (Action) or automatic (COM interface) control or internal OpenDSS Reset when locked. |
| 5 | Delay | Operating time delay (sec) of the switch. Defaults to 120. |
| 6 | Normal | {Open \| Closed] Normal state of the switch. If not Locked, the switch reverts to this state for reset, change of mode, etc. Defaults to first Action or State specified if not specifically declared. |
| 7 | State | {Open \| Closed] Present state of the switch. Upon setting, immediately forces state of switch. |
| 8 | Reset | {Yes \| No} If Yes, forces Reset of switch to Normal state and removes Lock independently of any internal reset command for mode change, etc. |
| 9 | basefreq | Base Frequency for ratings. |
| 10 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 11 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `UPFCControl` properties

| Number | Name | Description |
| - | - | - |
| 1 | Element | Full object name of the circuit element, typically a line or transformer, which the control is monitoring. There is no default; must be specified. |
| 2 | Terminal | Number of the terminal of the circuit element to which the UPFCControl control is connected. 1 or 2, typically.  Default is 1. Make sure you have the direction on the power matching the sign of kWLimit. |
| 3 | kWLimit | kW Limit for the monitored element. The generators are dispatched to hold the power in band. |
| 4 | kWBand | Bandwidth (kW) of the dead band around the target limit.No dispatch changes are attempted if the power in the monitored terminal stays within this band. |
| 5 | kvarlimit | Max kvar to be delivered through the element.  Uses same dead band as kW. |
| 6 | GenList | Array list of generators to be dispatched.  If not specified, all generators in the circuit are assumed dispatchable. |
| 7 | basefreq | Base Frequency for ratings. |
| 8 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 9 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


---
### Metering Elements

#### `EnergyMeter` properties

| Number | Name | Description |
| - | - | - |
| 1 | element | Name (Full Object name) of element to which the monitor is connected. |
| 2 | terminal | Number of the terminal of the circuit element to which the monitor is connected. 1 or 2, typically. |
| 3 | action | {Clear (reset) \| Save \| Take \| Zonedump \| Allocate \| Reduce} <br><br>(A)llocate = Allocate loads on the meter zone to match PeakCurrent.<br>(C)lear = reset all registers to zero<br>(R)educe = reduces zone by merging lines (see Set Keeplist & ReduceOption)<br>(S)ave = saves the current register values to a file.<br>   File name is "MTR_metername.CSV".<br>(T)ake = Takes a sample at present solution<br>(Z)onedump = Dump names of elements in meter zone to a file<br>   File name is "Zone_metername.CSV". |
| 4 | option | Enter a string ARRAY of any combination of the following. Options processed left-to-right:<br><br>(E)xcess : (default) UE/EEN is estimate of energy over capacity <br>(T)otal : UE/EEN is total energy after capacity exceeded<br>(R)adial : (default) Treats zone as a radial circuit<br>(M)esh : Treats zone as meshed network (not radial).<br>(C)ombined : (default) Load UE/EEN computed from combination of overload and undervoltage.<br>(V)oltage : Load UE/EEN computed based on voltage only.<br><br>Example: option=(E, R) |
| 5 | kVAnormal | Upper limit on kVA load in the zone, Normal configuration. Default is 0.0 (ignored). Overrides limits on individual lines for overload EEN. With "LocalOnly=Yes" option, uses only load in metered branch. |
| 6 | kVAemerg | Upper limit on kVA load in the zone, Emergency configuration. Default is 0.0 (ignored). Overrides limits on individual lines for overload UE. With "LocalOnly=Yes" option, uses only load in metered branch. |
| 7 | peakcurrent | ARRAY of current magnitudes representing the peak currents measured at this location for the load allocation function.  Default is (400, 400, 400). Enter one current for each phase |
| 8 | Zonelist | ARRAY of full element names for this meter's zone.  Default is for meter to find it's own zone. If specified, DSS uses this list instead.  Can access the names in a single-column text file.  Examples: <br><br>zonelist=[line.L1, transformer.T1, Line.L3] <br>zonelist=(file=branchlist.txt) |
| 9 | LocalOnly | {Yes \| No}  Default is NO.  If Yes, meter considers only the monitored element for EEN and UE calcs.  Uses whole zone for losses. |
| 10 | Mask | Mask for adding registers whenever all meters are totalized.  Array of floating point numbers representing the multiplier to be used for summing each register from this meter. Default = (1, 1, 1, 1, ... ).  You only have to enter as many as are changed (positional). Useful when two meters monitor same energy, etc. |
| 11 | Losses | {Yes \| No}  Default is YES. Compute Zone losses. If NO, then no losses at all are computed. |
| 12 | LineLosses | {Yes \| No}  Default is YES. Compute Line losses. If NO, then none of the losses are computed. |
| 13 | XfmrLosses | {Yes \| No}  Default is YES. Compute Transformer losses. If NO, transformers are ignored in loss calculations. |
| 14 | SeqLosses | {Yes \| No}  Default is YES. Compute Sequence losses in lines and segregate by line mode losses and zero mode losses. |
| 15 | 3phaseLosses | {Yes \| No}  Default is YES. Compute Line losses and segregate by 3-phase and other (1- and 2-phase) line losses.  |
| 16 | VbaseLosses | {Yes \| No}  Default is YES. Compute losses and segregate by voltage base. If NO, then voltage-based tabulation is not reported. |
| 17 | PhaseVoltageReport | {Yes \| No}  Default is NO.  Report min, max, and average phase voltages for the zone and tabulate by voltage base. Demand Intervals must be turned on (Set Demand=true) and voltage bases must be defined for this property to take effect. Result is in a separate report file. |
| 18 | Int_Rate | Average number of annual interruptions for head of the meter zone (source side of zone or feeder). |
| 19 | Int_Duration | Average annual duration, in hr, of interruptions for head of the meter zone (source side of zone or feeder). |
| 20 | SAIFI | (Read only) Makes SAIFI result available via return on query (? energymeter.myMeter.SAIFI. |
| 21 | SAIFIkW | (Read only) Makes SAIFIkW result available via return on query (? energymeter.myMeter.SAIFIkW. |
| 22 | SAIDI | (Read only) Makes SAIDI result available via return on query (? energymeter.myMeter.SAIDI. |
| 23 | CAIDI | (Read only) Makes CAIDI result available via return on query (? energymeter.myMeter.CAIDI. |
| 24 | CustInterrupts | (Read only) Makes Total Customer Interrupts value result available via return on query (? energymeter.myMeter.CustInterrupts. |
| 25 | basefreq | Base Frequency for ratings. |
| 26 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 27 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `Monitor` properties

| Number | Name | Description |
| - | - | - |
| 1 | element | Name (Full Object name) of element to which the monitor is connected. |
| 2 | terminal | Number of the terminal of the circuit element to which the monitor is connected. 1 or 2, typically. For monitoring states, attach monitor to terminal 1. |
| 3 | mode | Bitmask integer designating the values the monitor is to capture: <br>0 = Voltages and currents<br>1 = Powers<br>2 = Tap Position (Transformer Device only)<br>3 = State Variables (PCElements only)<br>4 = Flicker level and severity index (Pst) for voltages. No adders apply.<br>    Flicker level at simulation time step, Pst at 10-minute time step.<br>5 = Solution variables (Iterations, etc).<br>Normally, these would be actual phasor quantities from solution.<br>6 = Capacitor Switching (Capacitors only)<br>7 = Storage state vars (Storage device only)<br>8 = All winding currents (Transformer device only)<br>9 = Losses, watts and var (of monitored device)<br><br>10 = All Winding voltages (Transformer device only)<br><br>Normally, these would be actual phasor quantities from solution.<br>Combine mode with adders below to achieve other results for terminal quantities:<br>+16 = Sequence quantities<br>+32 = Magnitude only<br>+64 = Positive sequence only or avg of all phases<br><br>Mix adder to obtain desired results. For example:<br>Mode=112 will save positive sequence voltage and current magnitudes only<br>Mode=48 will save all sequence voltages and currents, but magnitude only. |
| 4 | action | {Clear \| Save \| Take \| Process}<br>(C)lears or (S)aves current buffer.<br>(T)ake action takes a sample.<br>(P)rocesses the data taken so far (e.g. Pst for mode 4).<br><br>Note that monitors are automatically reset (cleared) when the Set Mode= command is issued. Otherwise, the user must explicitly reset all monitors (reset monitors command) or individual monitors with the Clear action. |
| 5 | residual | {Yes/True \| No/False} Default = No.  Include Residual cbannel (sum of all phases) for voltage and current. Does not apply to sequence quantity modes or power modes. |
| 6 | VIPolar | {Yes/True \| No/False} Default = YES. Report voltage and current in polar form (Mag/Angle). (default)  Otherwise, it will be real and imaginary. |
| 7 | PPolar | {Yes/True \| No/False} Default = YES. Report power in Apparent power, S, in polar form (Mag/Angle).(default)  Otherwise, is P and Q |
| 8 | basefreq | Base Frequency for ratings. |
| 9 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 10 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `Sensor` properties

| Number | Name | Description |
| - | - | - |
| 1 | element | Name (Full Object name) of element to which the Sensor is connected. |
| 2 | terminal | Number of the terminal of the circuit element to which the Sensor is connected. 1 or 2, typically. Default is 1. |
| 3 | kvbase | Voltage base for the sensor, in kV. If connected to a 2- or 3-phase terminal, <br>specify L-L voltage. For 1-phase devices specify L-N or actual 1-phase voltage. Like many other DSS devices, default is 12.47kV. |
| 4 | clear | { Yes \| No }. Clear=Yes clears sensor values. Should be issued before putting in a new set of measurements. |
| 5 | kVs | Array of Voltages (kV) measured by the voltage sensor. For Delta-connected sensors, Line-Line voltages are expected. For Wye, Line-Neutral are expected. |
| 6 | currents | Array of Currents (amps) measured by the current sensor. Specify this or power quantities; not both. |
| 7 | kWs | Array of Active power (kW) measurements at the sensor. Is converted into Currents along with q=[...]<br>Will override any currents=[...] specification. |
| 8 | kvars | Array of Reactive power (kvar) measurements at the sensor. Is converted into Currents along with p=[...] |
| 9 | conn | Voltage sensor Connection: { wye \| delta \| LN \| LL }.  Default is wye. Applies to voltage measurement only. <br>Currents are always assumed to be line currents.<br>If wye or LN, voltage is assumed measured line-neutral; otherwise, line-line. |
| 10 | Deltadirection | {1 or -1}  Default is 1:  1-2, 2-3, 3-1.  For reverse rotation, enter -1. Any positive or negative entry will suffice. |
| 11 | %Error | Assumed percent error in the measurement. Default is 1. |
| 12 | Weight | Weighting factor: Default is 1. |
| 13 | action | NOT IMPLEMENTED.Action options: <br>SQERROR: Show square error of the present value of the monitored terminal  <br>quantity vs the sensor value. Actual values - convert to per unit in calling program.  <br>Value reported in result window/result variable. |
| 14 | basefreq | Base Frequency for ratings. |
| 15 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 16 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


---
### Supporting Elements

#### `CNData` properties

| Number | Name | Description |
| - | - | - |
| 1 | k | Number of concentric neutral strands; default is 2 |
| 2 | DiaStrand | Diameter of a concentric neutral strand; same units as core conductor radius; no default. |
| 3 | GmrStrand | Geometric mean radius of a concentric neutral strand; same units as core conductor GMR; defaults to 0.7788 \* CN strand radius. |
| 4 | Rstrand | AC resistance of a concentric neutral strand; same units as core conductor resistance; no default. |
| 5 | EpsR | Insulation layer relative permittivity; default is 2.3. |
| 6 | InsLayer | Insulation layer thickness; same units as radius; no default. With DiaIns, establishes inner radius for capacitance calculation. |
| 7 | DiaIns | Diameter over insulation layer; same units as radius; no default. Establishes outer radius for capacitance calculation. |
| 8 | DiaCable | Diameter over cable; same units as radius; no default. |
| 9 | Rdc | dc Resistance, ohms per unit length (see Runits). Defaults to Rac/1.02 if not specified. |
| 10 | Rac | Resistance at 60 Hz per unit length. Defaults to 1.02\*Rdc if not specified. |
| 11 | Runits | Length units for resistance: ohms per {mi\|kft\|km\|m\|Ft\|in\|cm\|mm} Default=none. |
| 12 | GMRac | GMR at 60 Hz. Defaults to .7788\*radius if not specified. |
| 13 | GMRunits | Units for GMR: {mi\|kft\|km\|m\|Ft\|in\|cm\|mm} Default=none. |
| 14 | radius | Outside radius of conductor. Defaults to GMR/0.7788 if not specified. |
| 15 | radunits | Units for outside radius: {mi\|kft\|km\|m\|Ft\|in\|cm\|mm} Default=none. |
| 16 | normamps | Normal ampacity, amperes. Defaults to Emergency amps/1.5 if not specified. |
| 17 | emergamps | Emergency ampacity, amperes. Defaults to 1.5 \* Normal Amps if not specified. |
| 18 | diam | Diameter; Alternative method for entering radius. |
| 19 | Seasons | Defines the number of ratings to be defined for the wire, to be used only when defining seasonal ratings using the "Ratings" property. |
| 20 | Ratings | An array of ratings to be used when the seasonal ratings flag is True. It can be used to insert<br>multiple ratings to change during a QSTS simulation to evaluate different ratings in lines. |
| 21 | Capradius | Equivalent conductor radius for capacitance calcs. Specify this for bundled conductors. Defaults to same value as radius. |
| 22 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `GrowthShape` properties

| Number | Name | Description |
| - | - | - |
| 1 | npts | Number of points to expect in subsequent vector. |
| 2 | year | Array of year values, or a text file spec, corresponding to the multipliers. Enter only those years where the growth changes. May be any integer sequence -- just so it is consistent. See help on Mult. |
| 3 | mult | Array of growth multiplier values, or a text file spec, corresponding to the year values. Enter the multiplier by which you would multiply the previous year's load to get the present year's.<br><br>Examples:<br><br>  Year = [1, 2, 5]   Mult=[1.05, 1.025, 1.02].<br>  Year= (File=years.txt) Mult= (file=mults.txt).<br><br>Text files contain one value per line. |
| 4 | csvfile | Switch input of growth curve data to a csv file containing (year, mult) points, one per line. |
| 5 | sngfile | Switch input of growth curve data to a binary file of singles containing (year, mult) points, packed one after another. |
| 6 | dblfile | Switch input of growth curve data to a binary file of doubles containing (year, mult) points, packed one after another. |
| 7 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `LineCode` properties

| Number | Name | Description |
| - | - | - |
| 1 | nphases | Number of phases in the line this line code data represents.  Setting this property reinitializes the line code.  Impedance matrix is reset for default symmetrical component. |
| 2 | r1 | Positive-sequence Resistance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces the program to use the symmetrical component line definition. See also Rmatrix. |
| 3 | x1 | Positive-sequence Reactance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces the program to use the symmetrical component line definition. See also Xmatrix |
| 4 | r0 | Zero-sequence Resistance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces the program to use the symmetrical component line definition. |
| 5 | x0 | Zero-sequence Reactance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces the program to use the symmetrical component line definition. |
| 6 | C1 | Positive-sequence capacitance, nf per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces the program to use the symmetrical component line definition. See also Cmatrix and B1. |
| 7 | C0 | Zero-sequence capacitance, nf per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces the program to use the symmetrical component line definition. See also B0. |
| 8 | units | One of (ohms per ...) {none\|mi\|km\|kft\|m\|me\|ft\|in\|cm}.  Default is none; assumes units agree with length unitsgiven in Line object |
| 9 | rmatrix | Resistance matrix, lower triangle, ohms per unit length. Order of the matrix is the number of phases. May be used to specify the impedance of any line configuration.  For balanced line models, you may use the standard symmetrical component data definition instead. |
| 10 | xmatrix | Reactance matrix, lower triangle, ohms per unit length. Order of the matrix is the number of phases. May be used to specify the impedance of any line configuration.  For balanced line models, you may use the standard symmetrical component data definition instead. |
| 11 | cmatrix | Nodal Capacitance matrix, lower triangle, nf per unit length.Order of the matrix is the number of phases. May be used to specify the shunt capacitance of any line configuration.  For balanced line models, you may use the standard symmetrical component data definition instead. |
| 12 | baseFreq | Frequency at which impedances are specified. |
| 13 | normamps | Normal ampere limit on line.  This is the so-called Planning Limit. It may also be the value above which load will have to be dropped in a contingency.  Usually about 75% - 80% of the emergency (one-hour) rating. |
| 14 | emergamps | Emergency ampere limit on line (usually one-hour rating). |
| 15 | faultrate | Number of faults per unit length per year. |
| 16 | pctperm | Percentage of the faults that become permanent. |
| 17 | repair | Hours to repair. |
| 18 | Kron | Kron = Y/N. Default=N.  Perform Kron reduction on the impedance matrix after it is formed, reducing order by 1. Eliminates the conductor designated by the "Neutral=" property. Do this after the R, X, and C matrices are defined. Ignored for symmetrical components. May be issued more than once to eliminate more than one conductor by resetting the Neutral property after the previous invoking of this property. Generally, you do not want to do a Kron reduction on the matrix if you intend to solve at a frequency other than the base frequency and exploit the Rg and Xg values. |
| 19 | Rg | Carson earth return resistance per unit length used to compute impedance values at base frequency.  For making better frequency adjustments. Default is 0.01805 = 60 Hz value in ohms per kft (matches default line impedances). This value is required for harmonic solutions if you wish to adjust the earth return impedances for frequency. If not, set both Rg and Xg = 0. |
| 20 | Xg | Carson earth return reactance per unit length used to compute impedance values at base frequency.  For making better frequency adjustments. Default value is 0.155081 = 60 Hz value in ohms per kft (matches default line impedances). This value is required for harmonic solutions if you wish to adjust the earth return impedances for frequency. If not, set both Rg and Xg = 0. |
| 21 | rho | Default=100 meter ohms.  Earth resitivity used to compute earth correction factor. |
| 22 | neutral | Designates which conductor is the "neutral" conductor that will be eliminated by Kron reduction. Default is the last conductor (nphases value). After Kron reduction is set to 0. Subsequent issuing of Kron=Yes will not do anything until this property is set to a legal value. Applies only to LineCodes defined by R, X, and C matrix. |
| 23 | B1 | Alternate way to specify C1. MicroS per unit length |
| 24 | B0 | Alternate way to specify C0. MicroS per unit length |
| 25 | Seasons | Defines the number of ratings to be defined for the wire, to be used only when defining seasonal ratings using the "Ratings" property. |
| 26 | Ratings | An array of ratings to be used when the seasonal ratings flag is True. It can be used to insert<br>multiple ratings to change during a QSTS simulation to evaluate different ratings in lines. |
| 27 | LineType | Code designating the type of line. <br>One of: OH, UG, UG_TS, UG_CN, SWT_LDBRK, SWT_FUSE, SWT_SECT, SWT_REC, SWT_DISC, SWT_BRK, SWT_ELBOW<br><br>OpenDSS currently does not use this internally. For whatever purpose the user defines. Default is OH. |
| 28 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `LineGeometry` properties

| Number | Name | Description |
| - | - | - |
| 1 | nconds | Number of conductors in this geometry. Default is 3. Triggers memory allocations. Define first! |
| 2 | nphases | Number of phases. Default =3; All other conductors are considered neutrals and might be reduced out. |
| 3 | cond | Set this = number of the conductor you wish to define. Default is 1. |
| 4 | wire | Code from WireData. MUST BE PREVIOUSLY DEFINED. no default.<br>Specifies use of Overhead Line parameter calculation,<br>Unless Tape Shield cable previously assigned to phases, and this wire is a neutral. |
| 5 | x | x coordinate. |
| 6 | h | Height of conductor. |
| 7 | units | Units for x and h: {mi\|kft\|km\|m\|Ft\|in\|cm } Initial default is "ft", but defaults to last unit defined |
| 8 | normamps | Normal ampacity, amperes for the line. Defaults to first conductor if not specified. |
| 9 | emergamps | Emergency ampacity, amperes. Defaults to first conductor if not specified. |
| 10 | reduce | {Yes \| No} Default = no. Reduce to Nphases (Kron Reduction). Reduce out neutrals. |
| 11 | spacing | Reference to a LineSpacing for use in a line constants calculation.<br>Alternative to x, h, and units. MUST BE PREVIOUSLY DEFINED.<br>Must match "nconds" as previously defined for this geometry.<br>Must be used in conjunction with the Wires property. |
| 12 | wires | Array of WireData names for use in a line constants calculation.<br>Alternative to individual wire inputs. ALL MUST BE PREVIOUSLY DEFINED.<br>Must match "nconds" as previously defined for this geometry,<br>unless TSData or CNData were previously assigned to phases, and these wires are neutrals.<br>Must be used in conjunction with the Spacing property. |
| 13 | cncable | Code from CNData. MUST BE PREVIOUSLY DEFINED. no default.<br>Specifies use of Concentric Neutral cable parameter calculation. |
| 14 | tscable | Code from TSData. MUST BE PREVIOUSLY DEFINED. no default.<br>Specifies use of Tape Shield cable parameter calculation. |
| 15 | cncables | Array of CNData names for cable parameter calculation.<br>All must be previously defined, and match "nphases" for this geometry.<br>You can later define "nconds-nphases" wires for bare neutral conductors. |
| 16 | tscables | Array of TSData names for cable parameter calculation.<br>All must be previously defined, and match "nphases" for this geometry.<br>You can later define "nconds-nphases" wires for bare neutral conductors. |
| 17 | Seasons | Defines the number of ratings to be defined for the wire, to be used only when defining seasonal ratings using the "Ratings" property. |
| 18 | Ratings | An array of ratings to be used when the seasonal ratings flag is True. It can be used to insert<br>multiple ratings to change during a QSTS simulation to evaluate different ratings in lines. |
| 19 | LineType | Code designating the type of line. <br>One of: OH, UG, UG_TS, UG_CN, SWT_LDBRK, SWT_FUSE, SWT_SECT, SWT_REC, SWT_DISC, SWT_BRK, SWT_ELBOW<br><br>OpenDSS currently does not use this internally. For whatever purpose the user defines. Default is OH. |
| 20 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `LineSpacing` properties

| Number | Name | Description |
| - | - | - |
| 1 | nconds | Number of wires in this geometry. Default is 3. Triggers memory allocations. Define first! |
| 2 | nphases | Number of retained phase conductors. If less than the number of wires, list the retained phase coordinates first. |
| 3 | x | Array of wire X coordinates. |
| 4 | h | Array of wire Heights. |
| 5 | units | Units for x and h: {mi\|kft\|km\|m\|Ft\|in\|cm } Initial default is "ft", but defaults to last unit defined |
| 6 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `LoadShape` properties

| Number | Name | Description |
| - | - | - |
| 1 | npts | Max number of points to expect in load shape vectors. This gets reset to the number of multiplier values found (in files only) if less than specified. |
| 2 | interval | Time interval for fixed interval data, hrs. Default = 1. If Interval = 0 then time data (in hours) may be at either regular or  irregular intervals and time value must be specified using either the Hour property or input files. Then values are interpolated when Interval=0, but not for fixed interval data.  <br><br>See also "sinterval" and "minterval". |
| 3 | mult | Array of multiplier values for active power (P) or other key value (such as pu V for Vsource). <br><br>You can also use the syntax: <br><br>mult = (file=filename)     !for text file one value per line<br>mult = (dblfile=filename)  !for packed file of doubles<br>mult = (sngfile=filename)  !for packed file of singles <br>mult = (file=MyCSVFile.CSV, col=3, header=yes)  !for multicolumn CSV files <br><br>Note: this property will reset Npts if the  number of values in the files are fewer.<br><br>Same as Pmult |
| 4 | hour | Array of hour values. Only necessary to define for variable interval data (Interval=0). If you set Interval>0 to denote fixed interval data, DO NOT USE THIS PROPERTY. You can also use the syntax: <br>hour = (file=filename)     !for text file one value per line<br>hour = (dblfile=filename)  !for packed file of doubles<br>hour = (sngfile=filename)  !for packed file of singles  |
| 5 | mean | Mean of the active power multipliers.  This is computed on demand the first time a value is needed.  However, you may set it to another value independently. Used for Monte Carlo load simulations. |
| 6 | stddev | Standard deviation of active power multipliers.  This is computed on demand the first time a value is needed.  However, you may set it to another value independently.Is overwritten if you subsequently read in a curve<br><br>Used for Monte Carlo load simulations. |
| 7 | csvfile | Switch input of active power load curve data to a CSV text file containing (hour, mult) points, or simply (mult) values for fixed time interval data, one per line. NOTE: This action may reset the number of points to a lower value. |
| 8 | sngfile | Switch input of active power load curve data to a binary file of singles containing (hour, mult) points, or simply (mult) values for fixed time interval data, packed one after another. NOTE: This action may reset the number of points to a lower value. |
| 9 | dblfile | Switch input of active power load curve data to a binary file of doubles containing (hour, mult) points, or simply (mult) values for fixed time interval data, packed one after another. NOTE: This action may reset the number of points to a lower value. |
| 10 | action | {NORMALIZE \| DblSave \| SngSave} After defining load curve data, setting action=normalize will modify the multipliers so that the peak is 1.0. The mean and std deviation are recomputed.<br><br>Setting action=DblSave or SngSave will cause the present mult and qmult values to be written to either a packed file of double or single. The filename is the loadshape name. The mult array will have a "_P" appended on the file name and the qmult array, if it exists, will have "_Q" appended. |
| 11 | qmult | Array of multiplier values for reactive power (Q).  You can also use the syntax: <br>qmult = (file=filename)     !for text file one value per line<br>qmult = (dblfile=filename)  !for packed file of doubles<br>qmult = (sngfile=filename)  !for packed file of singles <br>qmult = (file=MyCSVFile.CSV, col=4, header=yes)  !for multicolumn CSV files  |
| 12 | UseActual | {Yes \| No\* \| True \| False\*} If true, signifies to Load, Generator, Vsource, or other objects to use the return value as the actual kW, kvar, kV, or other value rather than a multiplier. Nominally for AMI Load data but may be used for other functions. |
| 13 | Pmax | kW value at the time of max power. Is automatically set upon reading in a loadshape. Use this property to override the value automatically computed or to retrieve the value computed. |
| 14 | Qmax | kvar value at the time of max kW power. Is automatically set upon reading in a loadshape. Use this property to override the value automatically computed or to retrieve the value computed. |
| 15 | sinterval | Specify fixed interval in SECONDS. Alternate way to specify Interval property. |
| 16 | minterval | Specify fixed interval in MINUTES. Alternate way to specify Interval property. |
| 17 | Pbase | Base P value for normalization. Default is zero, meaning the peak will be used. |
| 18 | Qbase | Base Q value for normalization. Default is zero, meaning the peak will be used. |
| 19 | Pmult | Synonym for "mult". |
| 20 | PQCSVFile | Switch input to a CSV text file containing (active, reactive) power (P, Q) multiplier pairs, one per row. <br>If the interval=0, there should be 3 items on each line: (hour, Pmult, Qmult) |
| 21 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `PriceShape` properties

| Number | Name | Description |
| - | - | - |
| 1 | npts | Max number of points to expect in price shape vectors. This gets reset to the number of Price values found if less than specified. |
| 2 | interval | Time interval for fixed interval data, hrs. Default = 1. If Interval = 0 then time data (in hours) may be at irregular intervals and time value must be specified using either the Hour property or input files. Then values are interpolated when Interval=0, but not for fixed interval data.  <br><br>See also "sinterval" and "minterval". |
| 3 | price | Array of Price values.  Units should be compatible with the object using the data. You can also use the syntax: <br>Price = (file=filename)     !for text file one value per line<br>Price = (dblfile=filename)  !for packed file of doubles<br>Price = (sngfile=filename)  !for packed file of singles <br><br>Note: this property will reset Npts if the  number of values in the files are fewer. |
| 4 | hour | Array of hour values. Only necessary to define this property for variable interval data. If the data are fixed interval, do not use this property. You can also use the syntax: <br>hour = (file=filename)     !for text file one value per line<br>hour = (dblfile=filename)  !for packed file of doubles<br>hour = (sngfile=filename)  !for packed file of singles  |
| 5 | mean | Mean of the Price curve values.  This is computed on demand the first time a value is needed.  However, you may set it to another value independently. Used for Monte Carlo load simulations. |
| 6 | stddev | Standard deviation of the Prices.  This is computed on demand the first time a value is needed.  However, you may set it to another value independently.Is overwritten if you subsequently read in a curve<br><br>Used for Monte Carlo load simulations. |
| 7 | csvfile | Switch input of  Price curve data to a csv file containing (hour, Price) points, or simply (Price) values for fixed time interval data, one per line. NOTE: This action may reset the number of points to a lower value. |
| 8 | sngfile | Switch input of  Price curve data to a binary file of singles containing (hour, Price) points, or simply (Price) values for fixed time interval data, packed one after another. NOTE: This action may reset the number of points to a lower value. |
| 9 | dblfile | Switch input of  Price curve data to a binary file of doubles containing (hour, Price) points, or simply (Price) values for fixed time interval data, packed one after another. NOTE: This action may reset the number of points to a lower value. |
| 10 | sinterval | Specify fixed interval in SECONDS. Alternate way to specify Interval property. |
| 11 | minterval | Specify fixed interval in MINUTES. Alternate way to specify Interval property. |
| 12 | action | {DblSave \| SngSave} After defining Price curve data... Setting action=DblSave or SngSave will cause the present "Price" values to be written to either a packed file of double or single. The filename is the PriceShape name.  |
| 13 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `Spectrum` properties

| Number | Name | Description |
| - | - | - |
| 1 | NumHarm | Number of frequencies in this spectrum. (See CSVFile) |
| 2 | harmonic | Array of harmonic values. You can also use the syntax<br>harmonic = (file=filename)     !for text file one value per line<br>harmonic = (dblfile=filename)  !for packed file of doubles<br>harmonic = (sngfile=filename)  !for packed file of singles  |
| 3 | %mag | Array of magnitude values, assumed to be in PERCENT. You can also use the syntax<br>%mag = (file=filename)     !for text file one value per line<br>%mag = (dblfile=filename)  !for packed file of doubles<br>%mag = (sngfile=filename)  !for packed file of singles  |
| 4 | angle | Array of phase angle values, degrees.You can also use the syntax<br>angle = (file=filename)     !for text file one value per line<br>angle = (dblfile=filename)  !for packed file of doubles<br>angle = (sngfile=filename)  !for packed file of singles  |
| 5 | CSVFile | File of spectrum points with (harmonic, magnitude-percent, angle-degrees) values, one set of 3 per line, in CSV format. If fewer than NUMHARM frequencies found in the file, NUMHARM is set to the smaller value. |
| 6 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `TCC_Curve` properties

| Number | Name | Description |
| - | - | - |
| 1 | npts | Number of points to expect in time-current arrays. |
| 2 | C_array | Array of current (or voltage) values corresponding to time values (see help on T_Array). |
| 3 | T_array | Array of time values in sec. Typical array syntax: <br>t_array = (1, 2, 3, 4, ...)<br><br>Can also substitute a file designation: <br>t_array =  (file=filename)<br><br>The specified file has one value per line. |
| 4 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `TSData` properties

| Number | Name | Description |
| - | - | - |
| 1 | DiaShield | Diameter over tape shield; same units as radius; no default. |
| 2 | TapeLayer | Tape shield thickness; same units as radius; no default. |
| 3 | TapeLap | Tape Lap in percent; default 20.0 |
| 4 | EpsR | Insulation layer relative permittivity; default is 2.3. |
| 5 | InsLayer | Insulation layer thickness; same units as radius; no default. With DiaIns, establishes inner radius for capacitance calculation. |
| 6 | DiaIns | Diameter over insulation layer; same units as radius; no default. Establishes outer radius for capacitance calculation. |
| 7 | DiaCable | Diameter over cable; same units as radius; no default. |
| 8 | Rdc | dc Resistance, ohms per unit length (see Runits). Defaults to Rac/1.02 if not specified. |
| 9 | Rac | Resistance at 60 Hz per unit length. Defaults to 1.02\*Rdc if not specified. |
| 10 | Runits | Length units for resistance: ohms per {mi\|kft\|km\|m\|Ft\|in\|cm\|mm} Default=none. |
| 11 | GMRac | GMR at 60 Hz. Defaults to .7788\*radius if not specified. |
| 12 | GMRunits | Units for GMR: {mi\|kft\|km\|m\|Ft\|in\|cm\|mm} Default=none. |
| 13 | radius | Outside radius of conductor. Defaults to GMR/0.7788 if not specified. |
| 14 | radunits | Units for outside radius: {mi\|kft\|km\|m\|Ft\|in\|cm\|mm} Default=none. |
| 15 | normamps | Normal ampacity, amperes. Defaults to Emergency amps/1.5 if not specified. |
| 16 | emergamps | Emergency ampacity, amperes. Defaults to 1.5 \* Normal Amps if not specified. |
| 17 | diam | Diameter; Alternative method for entering radius. |
| 18 | Seasons | Defines the number of ratings to be defined for the wire, to be used only when defining seasonal ratings using the "Ratings" property. |
| 19 | Ratings | An array of ratings to be used when the seasonal ratings flag is True. It can be used to insert<br>multiple ratings to change during a QSTS simulation to evaluate different ratings in lines. |
| 20 | Capradius | Equivalent conductor radius for capacitance calcs. Specify this for bundled conductors. Defaults to same value as radius. |
| 21 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `TShape` properties

| Number | Name | Description |
| - | - | - |
| 1 | npts | Max number of points to expect in temperature shape vectors. This gets reset to the number of Temperature values found if less than specified. |
| 2 | interval | Time interval for fixed interval data, hrs. Default = 1. If Interval = 0 then time data (in hours) may be at irregular intervals and time value must be specified using either the Hour property or input files. Then values are interpolated when Interval=0, but not for fixed interval data.  <br><br>See also "sinterval" and "minterval". |
| 3 | temp | Array of temperature values.  Units should be compatible with the object using the data. You can also use the syntax: <br>Temp = (file=filename)     !for text file one value per line<br>Temp = (dblfile=filename)  !for packed file of doubles<br>Temp = (sngfile=filename)  !for packed file of singles <br><br>Note: this property will reset Npts if the  number of values in the files are fewer. |
| 4 | hour | Array of hour values. Only necessary to define this property for variable interval data. If the data are fixed interval, do not use this property. You can also use the syntax: <br>hour = (file=filename)     !for text file one value per line<br>hour = (dblfile=filename)  !for packed file of doubles<br>hour = (sngfile=filename)  !for packed file of singles  |
| 5 | mean | Mean of the temperature curve values.  This is computed on demand the first time a value is needed.  However, you may set it to another value independently. Used for Monte Carlo load simulations. |
| 6 | stddev | Standard deviation of the temperatures.  This is computed on demand the first time a value is needed.  However, you may set it to another value independently.Is overwritten if you subsequently read in a curve<br><br>Used for Monte Carlo load simulations. |
| 7 | csvfile | Switch input of  temperature curve data to a csv file containing (hour, Temp) points, or simply (Temp) values for fixed time interval data, one per line. NOTE: This action may reset the number of points to a lower value. |
| 8 | sngfile | Switch input of  temperature curve data to a binary file of singles containing (hour, Temp) points, or simply (Temp) values for fixed time interval data, packed one after another. NOTE: This action may reset the number of points to a lower value. |
| 9 | dblfile | Switch input of  temperature curve data to a binary file of doubles containing (hour, Temp) points, or simply (Temp) values for fixed time interval data, packed one after another. NOTE: This action may reset the number of points to a lower value. |
| 10 | sinterval | Specify fixed interval in SECONDS. Alternate way to specify Interval property. |
| 11 | minterval | Specify fixed interval in MINUTES. Alternate way to specify Interval property. |
| 12 | action | {DblSave \| SngSave} After defining temperature curve data... Setting action=DblSave or SngSave will cause the present "Temp" values to be written to either a packed file of double or single. The filename is the Tshape name.  |
| 13 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `WireData` properties

| Number | Name | Description |
| - | - | - |
| 1 | Rdc | dc Resistance, ohms per unit length (see Runits). Defaults to Rac/1.02 if not specified. |
| 2 | Rac | Resistance at 60 Hz per unit length. Defaults to 1.02\*Rdc if not specified. |
| 3 | Runits | Length units for resistance: ohms per {mi\|kft\|km\|m\|Ft\|in\|cm\|mm} Default=none. |
| 4 | GMRac | GMR at 60 Hz. Defaults to .7788\*radius if not specified. |
| 5 | GMRunits | Units for GMR: {mi\|kft\|km\|m\|Ft\|in\|cm\|mm} Default=none. |
| 6 | radius | Outside radius of conductor. Defaults to GMR/0.7788 if not specified. |
| 7 | radunits | Units for outside radius: {mi\|kft\|km\|m\|Ft\|in\|cm\|mm} Default=none. |
| 8 | normamps | Normal ampacity, amperes. Defaults to Emergency amps/1.5 if not specified. |
| 9 | emergamps | Emergency ampacity, amperes. Defaults to 1.5 \* Normal Amps if not specified. |
| 10 | diam | Diameter; Alternative method for entering radius. |
| 11 | Seasons | Defines the number of ratings to be defined for the wire, to be used only when defining seasonal ratings using the "Ratings" property. |
| 12 | Ratings | An array of ratings to be used when the seasonal ratings flag is True. It can be used to insert<br>multiple ratings to change during a QSTS simulation to evaluate different ratings in lines. |
| 13 | Capradius | Equivalent conductor radius for capacitance calcs. Specify this for bundled conductors. Defaults to same value as radius. |
| 14 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `XfmrCode` properties

| Number | Name | Description |
| - | - | - |
| 1 | phases | Number of phases this transformer. Default is 3. |
| 2 | windings | Number of windings, this transformers. (Also is the number of terminals) Default is 2. This property triggers memory allocation for the Transformer and will cause other properties to revert to default values. |
| 3 | wdg | Set this = to the number of the winding you wish to define.  Then set the values for this winding.  Repeat for each winding.  Alternatively, use the array collections (buses, kvas, etc.) to define the windings.  Note: reactances are BETWEEN pairs of windings; they are not the property of a single winding. |
| 4 | conn | Connection of this winding. Default is "wye" with the neutral solidly grounded. |
| 5 | kV | For 2-or 3-phase, enter phase-phase kV rating.  Otherwise, kV rating of the actual winding |
| 6 | kVA | Base kVA rating of the winding. Side effect: forces change of max normal and emerg kva ratings.If 2-winding transformer, forces other winding to same value. When winding 1 is defined, all other windings are defaulted to the same rating and the first two winding resistances are defaulted to the %loadloss value. |
| 7 | tap | Per unit tap that this winding is normally on. |
| 8 | %R | Percent resistance this winding.  (half of total for a 2-winding). |
| 9 | Rneut | Default = -1. Neutral resistance of wye (star)-connected winding in actual ohms.If entered as a negative value, the neutral is assumed to be open, or floating. |
| 10 | Xneut | Neutral reactance of wye(star)-connected winding in actual ohms.  May be + or -. |
| 11 | conns | Use this to specify all the Winding connections at once using an array. Example:<br><br>New Transformer.T1 buses="Hibus, lowbus" \~ conns=(delta, wye) |
| 12 | kVs | Use this to specify the kV ratings of all windings at once using an array. Example:<br><br>New Transformer.T1 buses="Hibus, lowbus" <br>\~ conns=(delta, wye)<br>\~ kvs=(115, 12.47)<br><br>See kV= property for voltage rules. |
| 13 | kVAs | Use this to specify the kVA ratings of all windings at once using an array. |
| 14 | taps | Use this to specify the normal p.u. tap of all windings at once using an array. |
| 15 | Xhl | Use this to specify the percent reactance, H-L (winding 1 to winding 2).  Use for 2- or 3-winding transformers. On the kva base of winding 1. |
| 16 | Xht | Use this to specify the percent reactance, H-T (winding 1 to winding 3).  Use for 3-winding transformers only. On the kVA base of winding 1. |
| 17 | Xlt | Use this to specify the percent reactance, L-T (winding 2 to winding 3).  Use for 3-winding transformers only. On the kVA base of winding 1. |
| 18 | Xscarray | Use this to specify the percent reactance between all pairs of windings as an array. All values are on the kVA base of winding 1.  The order of the values is as follows:<br><br>(x12 13 14... 23 24.. 34 ..)  <br><br>There will be n(n-1)/2 values, where n=number of windings. |
| 19 | thermal | Thermal time constant of the transformer in hours.  Typically about 2. |
| 20 | n | n Exponent for thermal properties in IEEE C57.  Typically 0.8. |
| 21 | m | m Exponent for thermal properties in IEEE C57.  Typically 0.9 - 1.0 |
| 22 | flrise | Temperature rise, deg C, for full load.  Default is 65. |
| 23 | hsrise | Hot spot temperature rise, deg C.  Default is 15. |
| 24 | %loadloss | Percent load loss at full load. The %R of the High and Low windings (1 and 2) are adjusted to agree at rated kVA loading. |
| 25 | %noloadloss | Percent no load losses at rated excitatation voltage. Default is 0. Converts to a resistance in parallel with the magnetizing impedance in each winding. |
| 26 | normhkVA | Normal maximum kVA rating of H winding (winding 1).  Usually 100% - 110% ofmaximum nameplate rating, depending on load shape. Defaults to 110% of kVA rating of Winding 1. |
| 27 | emerghkVA | Emergency (contingency)  kVA rating of H winding (winding 1).  Usually 140% - 150% ofmaximum nameplate rating, depending on load shape. Defaults to 150% of kVA rating of Winding 1. |
| 28 | MaxTap | Max per unit tap for the active winding.  Default is 1.10 |
| 29 | MinTap | Min per unit tap for the active winding.  Default is 0.90 |
| 30 | NumTaps | Total number of taps between min and max tap.  Default is 32. |
| 31 | %imag | Percent magnetizing current. Default=0.0. Magnetizing branch is in parallel with windings in each phase. Also, see "ppm_antifloat". |
| 32 | ppm_antifloat | Default=1 ppm.  Parts per million of transformer winding VA rating connected to ground to protect against accidentally floating a winding without a reference. If positive then the effect is adding a very large reactance to ground.  If negative, then a capacitor. |
| 33 | %Rs | Use this property to specify all the winding %resistances using an array. Example:<br><br>New Transformer.T1 buses="Hibus, lowbus" \~ %Rs=(0.2  0.3) |
| 34 | X12 | Alternative to XHL for specifying the percent reactance from winding 1 to winding 2.  Use for 2- or 3-winding transformers. Percent on the kVA base of winding 1.  |
| 35 | X13 | Alternative to XHT for specifying the percent reactance from winding 1 to winding 3.  Use for 3-winding transformers only. Percent on the kVA base of winding 1.  |
| 36 | X23 | Alternative to XLT for specifying the percent reactance from winding 2 to winding 3.Use for 3-winding transformers only. Percent on the kVA base of winding 1.   |
| 37 | RdcOhms | Winding dc resistance in OHMS. Useful for GIC analysis. From transformer test report. Defaults to 85% of %R property |
| 38 | Seasons | Defines the number of ratings to be defined for the transfomer, to be used only when defining seasonal ratings using the "Ratings" property. |
| 39 | Ratings | An array of ratings to be used when the seasonal ratings flag is True. It can be used to insert<br>multiple ratings to change during a QSTS simulation to evaluate different ratings in transformers. |
| 40 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `XYcurve` properties

| Number | Name | Description |
| - | - | - |
| 1 | npts | Max number of points to expect in curve. This could get reset to the actual number of points defined if less than specified. |
| 2 | Points | One way to enter the points in a curve. Enter x and y values as one array in the order [x1, y1, x2, y2, ...]. For example:<br><br>Points=[1,100 2,200 3, 300] <br><br>Values separated by commas or white space. Zero fills arrays if insufficient number of values. |
| 3 | Yarray | Alternate way to enter Y values. Enter an array of Y values corresponding to the X values.  You can also use the syntax: <br>Yarray = (file=filename)     !for text file one value per line<br>Yarray = (dblfile=filename)  !for packed file of doubles<br>Yarray = (sngfile=filename)  !for packed file of singles <br><br>Note: this property will reset Npts to a smaller value if the  number of values in the files are fewer. |
| 4 | Xarray | Alternate way to enter X values. Enter an array of X values corresponding to the Y values.  You can also use the syntax: <br>Xarray = (file=filename)     !for text file one value per line<br>Xarray = (dblfile=filename)  !for packed file of doubles<br>Xarray = (sngfile=filename)  !for packed file of singles <br><br>Note: this property will reset Npts to a smaller value if the  number of values in the files are fewer. |
| 5 | csvfile | Switch input of  X-Y curve data to a CSV file containing X, Y points one per line. NOTE: This action may reset the number of points to a lower value. |
| 6 | sngfile | Switch input of  X-Y curve data to a binary file of SINGLES containing X, Y points packed one after another. NOTE: This action may reset the number of points to a lower value. |
| 7 | dblfile | Switch input of  X-Y  curve data to a binary file of DOUBLES containing X, Y points packed one after another. NOTE: This action may reset the number of points to a lower value. |
| 8 | x | Enter a value and then retrieve the interpolated Y value from the Y property. On input shifted then scaled to original curve. Scaled then shifted on output. |
| 9 | y | Enter a value and then retrieve the interpolated X value from the X property. On input shifted then scaled to original curve. Scaled then shifted on output. |
| 10 | Xshift | Shift X property values (in/out) by this amount of offset. Default = 0. Does not change original definition of arrays. |
| 11 | Yshift | Shift Y property values (in/out) by this amount of offset. Default = 0. Does not change original definition of arrays. |
| 12 | Xscale | Scale X property values (in/out) by this factor. Default = 1.0. Does not change original definition of arrays. |
| 13 | Yscale | Scale Y property values (in/out) by this factor. Default = 1.0. Does not change original definition of arrays. |
| 14 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


---
### Other Elements

#### `Fault` properties

| Number | Name | Description |
| - | - | - |
| 1 | bus1 | Name of first bus. Examples:<br><br>bus1=busname<br>bus1=busname.1.2.3<br><br>Bus2 automatically defaults to busname.0,0,0 unless it was previously defined.  |
| 2 | bus2 | Name of 2nd bus of the 2-terminal Fault object. Defaults to all phases connected to first bus, node 0, if not specified. (Shunt Wye Connection to ground reference)<br><br>That is, the Fault defaults to a ground fault unless otherwise specified. |
| 3 | phases | Number of Phases. Default is 1. |
| 4 | r | Resistance, each phase, ohms. Default is 0.0001. Assumed to be Mean value if gaussian random mode.Max value if uniform mode.  A Fault is actually a series resistance that defaults to a wye connection to ground on the second terminal.  You may reconnect the 2nd terminal to achieve whatever connection.  Use the Gmatrix property to specify an arbitrary conductance matrix. |
| 5 | %stddev | Percent standard deviation in resistance to assume for Monte Carlo fault (MF) solution mode for GAUSSIAN distribution. Default is 0 (no variation from mean). |
| 6 | Gmatrix | Use this to specify a nodal conductance (G) matrix to represent some arbitrary resistance network. Specify in lower triangle form as usual for DSS matrices. |
| 7 | ONtime | Time (sec) at which the fault is established for time varying simulations. Default is 0.0 (on at the beginning of the simulation) |
| 8 | temporary | {Yes \| No} Default is No.  Designate whether the fault is temporary.  For Time-varying simulations, the fault will be removed if the current through the fault drops below the MINAMPS criteria. |
| 9 | MinAmps | Minimum amps that can sustain a temporary fault. Default is 5. |
| 10 | normamps | Normal rated current. |
| 11 | emergamps | Maximum or emerg current. |
| 12 | faultrate | Failure rate per year. |
| 13 | pctperm | Percent of failures that become permanent. |
| 14 | repair | Hours to repair. |
| 15 | basefreq | Base Frequency for ratings. |
| 16 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 17 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `GICsource` properties

| Number | Name | Description |
| - | - | - |
| 1 | Volts | Voltage magnitude, in volts, of the GIC voltage induced across the associated line. When specified, induced voltage is assumed defined by Voltage and Angle properties. <br><br>Specify this value<br><br>OR<br><br>EN, EE, lat1, lon1, lat2, lon2. <br><br>Not both!!  Last one entered will take precedence. Assumed identical in each phase of the Line object. |
| 2 | angle | Phase angle in degrees of first phase. Default=0.0.  See Voltage property |
| 3 | frequency | Source frequency.  Defaults to  0.1 Hz. So GICSource=0 at power frequency. |
| 4 | phases | Number of phases.  Defaults to 3. All three phases are assumed in phase (zero sequence) |
| 5 | EN | Northward Electric field (V/km). If specified, Voltage and Angle are computed from EN, EE, lat and lon values. |
| 6 | EE | Eastward Electric field (V/km).  If specified, Voltage and Angle are computed from EN, EE, lat and lon values. |
| 7 | Lat1 | Latitude of Bus1 of the line(degrees) |
| 8 | Lon1 | Longitude of Bus1 of the line (degrees) |
| 9 | Lat2 | Latitude of Bus2 of the line (degrees) |
| 10 | Lon2 | Longitude of Bus2 of the line (degrees) |
| 11 | spectrum | Not used. |
| 12 | basefreq | Not used. |
| 13 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 14 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `Isource` properties

| Number | Name | Description |
| - | - | - |
| 1 | bus1 | Name of bus to which source is connected.<br>bus1=busname<br>bus1=busname.1.2.3 |
| 2 | amps | Magnitude of current source, each phase, in Amps. |
| 3 | angle | Phase angle in degrees of first phase: e.g.,Angle=10.3.<br>Phase shift between phases is assumed 120 degrees when number of phases <= 3 |
| 4 | frequency | Source frequency.  Defaults to  circuit fundamental frequency. |
| 5 | phases | Number of phases.  Defaults to 3. For 3 or less, phase shift is 120 degrees. |
| 6 | scantype | {pos\*\| zero \| none} Maintain specified sequence for harmonic solution. Default is positive sequence. Otherwise, angle between phases rotates with harmonic. |
| 7 | sequence | {pos\*\| neg \| zero} Set the phase angles for the specified symmetrical component sequence for non-harmonic solution modes. Default is positive sequence.  |
| 8 | Yearly | LOADSHAPE object to use for the per-unit current for YEARLY-mode simulations. Set the Mult property of the LOADSHAPE to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual Amp.<br><br>Must be previously defined as a LOADSHAPE object. <br><br>Is set to the Daily load shape when Daily is defined.  The daily load shape is repeated in this case. Set to NONE to reset to no loadahape for Yearly mode. The default is no variation. |
| 9 | Daily | LOADSHAPE object to use for the per-unit current for DAILY-mode simulations. Set the Mult property of the LOADSHAPE to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual A.<br><br>Must be previously defined as a LOADSHAPE object. <br><br>Sets Yearly curve if it is not already defined.   Set to NONE to reset to no loadahape for Yearly mode. The default is no variation. |
| 10 | Duty | LOADSHAPE object to use for the per-unit current for DUTYCYCLE-mode simulations. Set the Mult property of the LOADSHAPE to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual A.<br><br>Must be previously defined as a LOADSHAPE object. <br><br>Defaults to Daily load shape when Daily is defined.   Set to NONE to reset to no loadahape for Yearly mode. The default is no variation. |
| 11 | Bus2 | Name of bus to which 2nd terminal is connected.<br>bus2=busname<br>bus2=busname.1.2.3<br><br>Default is Bus1.0.0.0 (grounded-wye connection) |
| 12 | spectrum | Harmonic spectrum assumed for this source.  Default is "default". |
| 13 | basefreq | Base Frequency for ratings. |
| 14 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 15 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


#### `Vsource` properties

| Number | Name | Description |
| - | - | - |
| 1 | bus1 | Name of bus to which the main terminal (1) is connected.<br>bus1=busname<br>bus1=busname.1.2.3<br><br>The VSOURCE object is a two-terminal voltage source (thevenin equivalent). Bus2 defaults to Bus1 with all phases connected to ground (node 0) unless previously specified. This is a Yg connection. If you want something different, define the Bus2 property ezplicitly. |
| 2 | basekv | Base Source kV, usually phase-phase (L-L) unless you are making a positive-sequence model or 1-phase modelin which case, it will be phase-neutral (L-N) kV. |
| 3 | pu | Per unit of the base voltage that the source is actually operating at.<br>"pu=1.05" |
| 4 | angle | Phase angle in degrees of first phase: e.g.,Angle=10.3 |
| 5 | frequency | Source frequency.  Defaults to system default base frequency. |
| 6 | phases | Number of phases.  Defaults to 3. |
| 7 | MVAsc3 | MVA Short circuit, 3-phase fault. Default = 2000. Z1 is determined by squaring the base kv and dividing by this value. For single-phase source, this value is not used. |
| 8 | MVAsc1 | MVA Short Circuit, 1-phase fault. Default = 2100. The "single-phase impedance", Zs, is determined by squaring the base kV and dividing by this value. Then Z0 is determined by Z0 = 3Zs - 2Z1.  For 1-phase sources, Zs is used directly. Use X0R0 to define X/R ratio for 1-phase source. |
| 9 | x1r1 | Positive-sequence  X/R ratio. Default = 4. |
| 10 | x0r0 | Zero-sequence X/R ratio.Default = 3. |
| 11 | Isc3 | Alternate method of defining the source impedance. <br>3-phase short circuit current, amps.  Default is 10000. |
| 12 | Isc1 | Alternate method of defining the source impedance. <br>single-phase short circuit current, amps.  Default is 10500. |
| 13 | R1 | Alternate method of defining the source impedance. <br>Positive-sequence resistance, ohms.  Default is 1.65. |
| 14 | X1 | Alternate method of defining the source impedance. <br>Positive-sequence reactance, ohms.  Default is 6.6. |
| 15 | R0 | Alternate method of defining the source impedance. <br>Zero-sequence resistance, ohms.  Default is 1.9. |
| 16 | X0 | Alternate method of defining the source impedance. <br>Zero-sequence reactance, ohms.  Default is 5.7. |
| 17 | ScanType | {pos\*\| zero \| none} Maintain specified sequence for harmonic solution. Default is positive sequence. Otherwise, angle between phases rotates with harmonic. |
| 18 | Sequence | {pos\*\| neg \| zero} Set the phase angles for the specified symmetrical component sequence for non-harmonic solution modes. Default is positive sequence.  |
| 19 | bus2 | Name of bus to which 2nd terminal is connected.<br>bus2=busname<br>bus2=busname.1.2.3<br><br>Default is Bus1.0.0.0 (grounded wye connection) |
| 20 | Z1 | Positive-sequence equivalent source impedance, ohms, as a 2-element array representing a complex number. Example: <br><br>Z1=[1, 2]  ! represents 1 + j2 <br><br>If defined, Z1, Z2, and Z0 are used to define the impedance matrix of the VSOURCE. Z1 MUST BE DEFINED TO USE THIS OPTION FOR DEFINING THE MATRIX.<br><br>Side Effect: Sets Z2 and Z0 to same values unless they were previously defined. |
| 21 | Z0 | Zero-sequence equivalent source impedance, ohms, as a 2-element array representing a complex number. Example: <br><br>Z0=[3, 4]  ! represents 3 + j4 <br><br>Used to define the impedance matrix of the VSOURCE if Z1 is also specified. <br><br>Note: Z0 defaults to Z1 if it is not specifically defined.  |
| 22 | Z2 | Negative-sequence equivalent source impedance, ohms, as a 2-element array representing a complex number. Example: <br><br>Z2=[1, 2]  ! represents 1 + j2 <br><br>Used to define the impedance matrix of the VSOURCE if Z1 is also specified. <br><br>Note: Z2 defaults to Z1 if it is not specifically defined. If Z2 is not equal to Z1, the impedance matrix is asymmetrical. |
| 23 | puZ1 | 2-element array: e.g., [1  2]. An alternate way to specify Z1. See Z1 property. Per-unit positive-sequence impedance on base of Vsource BasekV and BaseMVA. |
| 24 | puZ0 | 2-element array: e.g., [1  2]. An alternate way to specify Z0. See Z0 property. Per-unit zero-sequence impedance on base of Vsource BasekV and BaseMVA. |
| 25 | puZ2 | 2-element array: e.g., [1  2]. An alternate way to specify Z2. See Z2 property. Per-unit negative-sequence impedance on base of Vsource BasekV and BaseMVA. |
| 26 | baseMVA | Default value is 100. Base used to convert values specifiied with puZ1, puZ0, and puZ2 properties to ohms on kV base specified by BasekV property. |
| 27 | Yearly | LOADSHAPE object to use for the per-unit voltage for YEARLY-mode simulations. Set the Mult property of the LOADSHAPE to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual L-N kV.<br><br>Must be previously defined as a LOADSHAPE object. <br><br>Is set to the Daily load shape when Daily is defined.  The daily load shape is repeated in this case. Set to NONE to reset to no loadahape for Yearly mode. The default is no variation. |
| 28 | Daily | LOADSHAPE object to use for the per-unit voltage for DAILY-mode simulations. Set the Mult property of the LOADSHAPE to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual L-N kV.<br><br>Must be previously defined as a LOADSHAPE object. <br><br>Sets Yearly curve if it is not already defined.   Set to NONE to reset to no loadahape for Yearly mode. The default is no variation. |
| 29 | Duty | LOADSHAPE object to use for the per-unit voltage for DUTYCYCLE-mode simulations. Set the Mult property of the LOADSHAPE to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual L-N kV.<br><br>Must be previously defined as a LOADSHAPE object. <br><br>Defaults to Daily load shape when Daily is defined.   Set to NONE to reset to no loadahape for Yearly mode. The default is no variation. |
| 30 | Model | {Thevenin\* \| Ideal}  Specifies whether the Vsource is to be considered a Thevenin short circuit model or a quasi-ideal voltage source. If Thevenin, the Vsource uses the impedances defined for all calculations. If "Ideal", the model uses a small impedance on the diagonal of the impedance matrix for the fundamental base frequency power flow only. Then switches to actual Thevenin model for other frequencies.  |
| 31 | puZideal | 2-element array: e.g., [1  2]. The pu impedance to use for the quasi-ideal voltage source model. Should be a very small impedances. Default is [1e-6, 0.001]. Per-unit impedance on base of Vsource BasekV and BaseMVA. If too small, solution may not work. Be sure to check the voltage values and powers. |
| 32 | spectrum | Name of harmonic spectrum for this source.  Default is "defaultvsource", which is defined when the DSS starts. |
| 33 | basefreq | Base Frequency for ratings. |
| 34 | enabled | {Yes\|No or True\|False} Indicates whether this element is enabled. |
| 35 | like | Make like another object, e.g.:<br><br>New Capacitor.C2 like=c1  ... |


