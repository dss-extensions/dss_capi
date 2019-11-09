unit Monitor;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2018, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   Change Log
   12-7-99 Modified Getcurrents override
   1-22-00 Derived from MeterElement Class
   5-30-00 Added test for positive sequence ckt model
           Fixed resetting of Nphases to match metered element
   10-27-00 Changed default to magnitude and angle instead of real and imag
   12-18-01 Added Transformer Tap Monitor Code
   12-18-02 Added Monitor Stream
   2-19-08 Added SampleCount
   01-19-13 Added flicker meter mode
   08-18-15 Added Solution monitor mode
   08-10-16 Added mode 6 for storing capacitor switching
   06-04-18 Added modes 7-9
   11-29-18 Added mode 10; revised mode 8
   12-4-18  Added link to AutoTransformer

}

{
  A monitor is a circuit element that is connected to a terminal of another
  circuit element.  It records the voltages and currents at that terminal as
  a function of time and can report those values upon demand.

  A Monitor is defined by a New commands:

  New Type=Monitor Name=myname Element=elemname Terminal=[1,2,...] Buffer=clear|save

  Upon creation, the monitor buffer is established.  There is a file associated
  with the buffer.  It is named "Mon_elemnameN.mon"  where N is the terminal no.
  The file is truncated to zero at creation or buffer clearing.

  The Monitor keeps results in the in-memory buffer until it is filled.  Then it
  appends the buffer to the associated file and resets the in-memory buffer.

  For buffer=save, the present in-memory buffer is appended to the disk file so
  that it is saved for later reference.

  The Monitor is a passive device that takes a sample whenever its "TakeSample"
  method is invoked.  The SampleAll method of the Monitor ckt element class will
  force all monitors elements to take a sample.  If the present time (for the most
  recent solution is greater than the last time entered in to the monitor buffer,
  the sample is appended to the buffer.  Otherwise, it replaces the last entry.

  Monitor Files are simple binary files of doubles.  The first record
  contains the number of conductors per terminal (NCond). (always use 'round' function
  when converting this to an integer). Then subsequent records consist of time and
  voltage and current samples for each terminal (all complex doubles) in the order
  shown below:

  <NCond>
           <--- All voltages first ---------------->|<--- All currents ----->|
  <hour 1> <sec 1> <V1.re>  <V1.im>  <V2.re>  <V2.im>  .... <I1.re>  <I1.im> ...
  <hour 2> <sec 1> <V1.re>  <V1.im>  <V2.re>  <V2.im>  .... <I1.re>  <I1.im> ...
  <hour 3> <sec 1> <V1.re>  <V1.im>  <V2.re>  <V2.im>  .... <I1.re>  <I1.im> ...

  The time values will not necessarily be in a uniform time step;  they will
  be at times samples or solutions were taken.  This could vary from several
  hours down to a few milliseconds.

  The monitor ID can be determined from the file name.  Thus, these values can
  be post-processed at any later time, provided that the monitors are not reset.

  Modes are:
   0: Standard mode - V and I,each phase, Mag and Angle
   1: Power each phase, complex (kw and kvars)
   2: Transformer Tap
   3: State Variables
   4: Flicker level and severity index by phase (no modifiers apply)
   5: Solution Variables (Iteration count, etc.)
   6: Capacitor Switching (Capacitors only)
   7: Storage Variables
   8: Transformer Winding Currents
   9: Losses (watts and vars)
  10: Transformer Winding Voltages (across winding)

   +16: Sequence components: V012, I012
   +32: Magnitude Only
   +64: Pos Seq only or Average of phases

}

interface

uses
    Command,
    MeterClass,
    Meterelement,
    DSSClass,
    Arraydef,
    ucomplex,
    utilities,
    Classes;

type
    TMonitorStrBuffer = array[1..256] of AnsiChar;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   {This has to be named TDSSMonitor because Delphi has a TMonitor Class and the compiler will get confused}
    TDSSMonitor = class(TMeterClass)
    PRIVATE

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const MonitorName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create(dss: TDSS);
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function Init(Handle: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

        procedure ResetAll; OVERRIDE;
        procedure SampleAll; OVERRIDE;  // Force all monitors to take a sample
        procedure SampleAllMode5;  // Sample just Mode 5 monitors
        procedure SaveAll; OVERRIDE;   // Force all monitors to save their buffers to disk
        procedure PostProcessAll;
        procedure TOPExport(Objname: String);

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TMonitorObj = class(TMeterElement)
    PRIVATE
        BufferSize: Integer;
        Hour: Integer;
        Sec: Double;    // last time entered in the buffer
        MonBuffer: pSingleArray;
        Bufptr: Integer;  // point to present (last) element in buffer must be incremented to add

        CurrentBuffer: pComplexArray;
        VoltageBuffer: pComplexArray;
        WdgCurrentsBuffer: pComplexArray;
        WdgVoltagesBuffer: pComplexArray;
        PhsVoltagesBuffer: pComplexArray;
        NumTransformerCurrents: Integer;
        NumWindingVoltages: Integer;

        NumStateVars: Integer;
        StateBuffer: pDoubleArray;

        FlickerBuffer: pComplexArray; // store phase voltages in polar form
                                       // then convert to re=flicker level, update every time step
                                       //             and im=Pst, update every 10 minutes
        SolutionBuffer: pDoubleArray;


        IncludeResidual: Boolean;
        VIpolar: Boolean;
        Ppolar: Boolean;

        FileSignature: Integer;
        FileVersion: Integer;

        BaseFrequency: Double;

        BufferFile: String;  // Name of file for catching buffer overflow

        IsFileOpen: Boolean;
        ValidMonitor: Boolean;
        IsProcessed: Boolean;

        procedure AddDblsToBuffer(Dbl: pDoubleArray; Ndoubles: Integer);
        procedure AddDblToBuffer(const Dbl: Double);

        procedure DoFlickerCalculations;  // call from CloseMonitorStream
        function Get_FileName: String;


    PUBLIC
        Mode: Integer;
        MonitorStream: TMemoryStream;
        SampleCount: Integer;  // This is the number of samples taken

        constructor Create(ParClass: TDSSClass; const MonitorName: String);
        destructor Destroy; OVERRIDE;

        procedure MakePosSequence; OVERRIDE;  // Make a positive Sequence Model, reset nphases
        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;    // Always Zero for a monitor
        procedure TakeSample; OVERRIDE; // Go add a sample to the buffer
        procedure ResetIt;
        procedure Save;     // Saves present buffer to file
        procedure PostProcess; // calculates Pst or other post-processing

        procedure OpenMonitorStream;
        procedure ClearMonitorStream;
        procedure CloseMonitorStream;

        procedure TranslateToCSV(Show: Boolean);

        procedure GetCurrents(Curr: pComplexArray); OVERRIDE; // Get present value of terminal Curr
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
       //Property  MonitorFileName:String read BufferFile;

        property CSVFileName: String READ Get_FileName;
    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

var
    ActiveMonitorObj: TMonitorObj;

{--------------------------------------------------------------------------}
implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    CktElement,
    Transformer,
    AutoTrans,
    PCElement,
    {$IFNDEF FPC}
    AnsiStrings,
{$ENDIF}
    Sysutils,
    ucmatrix,
    showresults,
    mathUtil,
    PointerList,
    TOPExport,
    Dynamics,
    PstCalc,
    Capacitor,
    Storage;

const
    SEQUENCEMASK = 16;
    MAGNITUDEMASK = 32;
    POSSEQONLYMASK = 64;
    MODEMASK = 15;

    NumPropsThisClass = 7;
    NumSolutionVars = 12;

var
    StrBuffer: TMonitorStrBuffer;

{$IFNDEF FPC}
function StrLCat(Dest: PAnsiChar; const Source: PAnsiChar; MaxLen: Cardinal): PAnsiChar; inline;
begin
    Result := System.AnsiStrings.StrLCat(Dest, Source, MaxLen);
end;

{$ENDIF}

{--------------------------------------------------------------------------}
constructor TDSSMonitor.Create(dss: TDSS);  // Creates superstructure for all Monitor objects
begin
    inherited Create(dss);

    Class_name := 'Monitor';
    DSSClassType := DSSClassType + MON_ELEMENT;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

{--------------------------------------------------------------------------}
destructor TDSSMonitor.Destroy;

begin
    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSMonitor.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;

     // Define Property names

    PropertyName[1] := 'element';
    PropertyName[2] := 'terminal';
    PropertyName[3] := 'mode';
    PropertyName[4] := 'action';  // buffer=clear|save
    PropertyName[5] := 'residual';  // buffer=clear|save
    PropertyName[6] := 'VIPolar';  // V I in mag and angle rather then re and im
    PropertyName[7] := 'PPolar';  // Power in power PF rather then power and vars

    PropertyHelp[1] := 'Name (Full Object name) of element to which the monitor is connected.';
    PropertyHelp[2] := 'Number of the terminal of the circuit element to which the monitor is connected. ' +
        '1 or 2, typically. For monitoring states, attach monitor to terminal 1.';
    PropertyHelp[3] := 'Bitmask integer designating the values the monitor is to capture: ' + CRLF +
        '0 = Voltages and currents' + CRLF +
        '1 = Powers' + CRLF +
        '2 = Tap Position (Transformer Device only)' + CRLF +
        '3 = State Variables (PCElements only)' + CRLF +
        '4 = Flicker level and severity index (Pst) for voltages. No adders apply.' + CRLF +
        '    Flicker level at simulation time step, Pst at 10-minute time step.' + CRLF +
        '5 = Solution variables (Iterations, etc).' + CRLF +
        'Normally, these would be actual phasor quantities from solution.' + CRLF +
        '6 = Capacitor Switching (Capacitors only)' + CRLF +
        '7 = Storage state vars (Storage device only)' + CRLF +
        '8 = All winding currents (Transformer device only)' + CRLF +
        '9 = Losses, watts and var (of monitored device)' + CRLF + CRLF +
        '10 = All Winding voltages (Transformer device only)' + CRLF + CRLF +
        'Normally, these would be actual phasor quantities from solution.' + CRLF +
        'Combine mode with adders below to achieve other results for terminal quantities:' + CRLF +
        '+16 = Sequence quantities' + CRLF +
        '+32 = Magnitude only' + CRLF +
        '+64 = Positive sequence only or avg of all phases' + CRLF +
        CRLF +
        'Mix adder to obtain desired results. For example:' + CRLF +
        'Mode=112 will save positive sequence voltage and current magnitudes only' + CRLF +
        'Mode=48 will save all sequence voltages and currents, but magnitude only.';
    PropertyHelp[4] := '{Clear | Save | Take | Process}' + CRLF +
        '(C)lears or (S)aves current buffer.' + CRLF +
        '(T)ake action takes a sample.' + CRLF +
        '(P)rocesses the data taken so far (e.g. Pst for mode 4).' + CRLF + CRLF +
        'Note that monitors are automatically reset (cleared) when the Set Mode= command is issued. ' +
        'Otherwise, the user must explicitly reset all monitors (reset monitors command) or individual ' +
        'monitors with the Clear action.';
    PropertyHelp[5] := '{Yes/True | No/False} Default = No.  Include Residual cbannel (sum of all phases) for voltage and current. ' +
        'Does not apply to sequence quantity modes or power modes.';
    PropertyHelp[6] := '{Yes/True | No/False} Default = YES. Report voltage and current in polar form (Mag/Angle). (default)  Otherwise, it will be real and imaginary.';
    PropertyHelp[7] := '{Yes/True | No/False} Default = YES. Report power in Apparent power, S, in polar form (Mag/Angle).(default)  Otherwise, is P and Q';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

{--------------------------------------------------------------------------}
function TDSSMonitor.NewObject(const ObjName: String): Integer;
begin
    // Make a new Monitor and add it to Monitor class list
    with ActiveCircuit do
    begin
        ActiveCktElement := TMonitorObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;

{--------------------------------------------------------------------------}
function TDSSMonitor.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    recalc: Integer;

begin

  // continue parsing with contents of Parser
  // continue parsing with contents of Parser
    ActiveMonitorObj := ElementList.Active;
    ActiveCircuit.ActiveCktElement := ActiveMonitorObj;

    Result := 0;
    recalc := 0;

    with ActiveMonitorObj do
    begin

        ParamPointer := 0;
        ParamName := Parser.NextParam;
        Param := Parser.StrValue;
        while Length(Param) > 0 do
        begin
            if Length(ParamName) = 0 then
                Inc(ParamPointer)
            else
                ParamPointer := CommandList.GetCommand(ParamName);

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[ParamPointer] := Param;
            inc(recalc);

            case ParamPointer of
                0:
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 661);
                1:
                begin
                    ElementName := ConstructElemName(lowercase(param));   // subtitute @var values if any
                    PropertyValue[1] := ElementName;
                end;
                2:
                    MeteredTerminal := Parser.IntValue;
                3:
                    Mode := Parser.IntValue;
                4:
                begin
                    param := lowercase(param);
                    case param[1] of
                        's':
                            Save;
                        'c', 'r':
                            ResetIt;
                        't':
                            TakeSample;
                        'p':
                        begin
                            PostProcess;
                            dec(recalc)
                        end
                    end;
                end;  // buffer
                5:
                    IncludeResidual := InterpretYesNo(Param);
                6:
                    VIpolar := InterpretYesNo(Param);
                7:
                    Ppolar := InterpretYesNo(Param);
            else
           // Inherited parameters
                ClassEdit(ActiveMonitorObj, ParamPointer - NumPropsthisClass)
            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

        if recalc > 0 then
            RecalcElementData;
    end;

end;

{--------------------------------------------------------------------------}
procedure TDSSMonitor.ResetAll;  // Force all monitors in the circuit to reset

var
    Mon: TMonitorObj;

begin
    Mon := ActiveCircuit.Monitors.First;
    while Mon <> NIL do
    begin
        if Mon.enabled then
            Mon.ResetIt;
        Mon := ActiveCircuit.Monitors.Next;
    end;

end;

{--------------------------------------------------------------------------}
procedure TDSSMonitor.SampleAll;  // Force all monitors in the circuit to take a sample

var
    Mon: TMonitorObj;
// sample all monitors except mode 5 monitors
begin
    Mon := ActiveCircuit.Monitors.First;
    while Mon <> NIL do
    begin
        if Mon.enabled then
            if Mon.Mode <> 5 then
                Mon.TakeSample;
        Mon := ActiveCircuit.Monitors.Next;
    end;
end;

{--------------------------------------------------------------------------}
procedure TDSSMonitor.SampleAllMode5;  // Force all mode=5 monitors in the circuit to take a sample

var
    Mon: TMonitorObj;
// sample all Mode 5 monitors except monitors
begin
    Mon := ActiveCircuit.Monitors.First;
    while Mon <> NIL do
    begin
        if Mon.enabled then
            if Mon.Mode = 5 then
                Mon.TakeSample;
        Mon := ActiveCircuit.Monitors.Next;
    end;
end;

{--------------------------------------------------------------------------}
procedure TDSSMonitor.PostProcessAll;
var
    Mon: TMonitorObj;
begin
    Mon := ActiveCircuit.Monitors.First;
    while Mon <> NIL do
    begin
        if Mon.Enabled then
            Mon.PostProcess;
        Mon := ActiveCircuit.Monitors.Next;
    end;
end;

{--------------------------------------------------------------------------}
procedure TDSSMonitor.SaveAll;     // Force all monitors in the circuit to save their buffers to disk

var
    Mon: TMonitorObj;

begin
    Mon := ActiveCircuit.Monitors.First;
    while Mon <> NIL do
    begin
        if Mon.Enabled then
            Mon.Save;
        Mon := ActiveCircuit.Monitors.Next;
    end;
end;

{--------------------------------------------------------------------------}
function TDSSMonitor.MakeLike(const MonitorName: String): Integer;
var
    OtherMonitor: TMonitorObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this Monitor name in the present collection}
    OtherMonitor := Find(MonitorName);
    if OtherMonitor <> NIL then
        with ActiveMonitorObj do
        begin

            NPhases := OtherMonitor.Fnphases;
            NConds := OtherMonitor.Fnconds; // Force Reallocation of terminal stuff

            Buffersize := OtherMonitor.Buffersize;
            ElementName := OtherMonitor.ElementName;
            MeteredElement := OtherMonitor.MeteredElement;  // Pointer to target circuit element
            MeteredTerminal := OtherMonitor.MeteredTerminal;
            Mode := OtherMonitor.Mode;
            IncludeResidual := OtherMonitor.IncludeResidual;

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherMonitor.PropertyValue[i];

            BaseFrequency := OtherMonitor.BaseFrequency;

        end
    else
        DoSimpleMsg('Error in Monitor MakeLike: "' + MonitorName + '" Not Found.', 662);

end;

{--------------------------------------------------------------------------}
function TDSSMonitor.Init(Handle: Integer): Integer;
var
    Mon: TMonitorObj;

begin
    Result := 0;

    if Handle > 0 then
    begin
        Mon := ElementList.Get(Handle);
        Mon.ResetIt;
    end
    else
    begin  // Do 'em all
        Mon := ElementList.First;
        while Mon <> NIL do
        begin
            Mon.ResetIt;
            Mon := ElementList.Next;
        end;
    end;

end;


{==========================================================================}
{                    TMonitorObj                                           }
{==========================================================================}


{--------------------------------------------------------------------------}
constructor TMonitorObj.Create(ParClass: TDSSClass; const MonitorName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(MonitorName);

    Nphases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors
                         // in base class

     {Current Buffer has to be big enough to hold all terminals}
    CurrentBuffer := NIL;
    VoltageBuffer := NIL;
    StateBuffer := NIL;
    FlickerBuffer := NIL;
    SolutionBuffer := NIL;
    WdgCurrentsBuffer := NIL;
    WdgVoltagesBuffer := NIL;
    PhsVoltagesBuffer := NIL;

    NumTransformerCurrents := 0;

    Basefrequency := 60.0;
    Hour := 0;
    Sec := 0.0;

    Mode := 0;  // Standard Mode: V & I, complex values

    BufferSize := 1024;       // Makes a 4K buffer
    MonBuffer := AllocMem(Sizeof(MonBuffer^[1]) * BufferSize);
    BufPtr := 0;

    ElementName := TDSSCktElement(ActiveCircuit.CktElements.Get(1)).Name; // Default to first circuit element (source)
    MeteredElement := NIL;
    Bufferfile := '';

    MonitorStream := TMemoryStream.Create; // Create memory stream

    IsFileOpen := FALSE;
    MeteredTerminal := 1;
    IncludeResidual := FALSE;
    VIPolar := TRUE;
    Ppolar := TRUE;
    FileSignature := 43756;
    FileVersion := 1;
    SampleCount := 0;
    IsProcessed := FALSE;

    DSSObjType := ParClass.DSSClassType; //MON_ELEMENT;

    InitPropertyValues(0);

end;

destructor TMonitorObj.Destroy;
begin
    MonitorStream.Free;
    ElementName := '';
    Bufferfile := '';
    ReAllocMem(MonBuffer, 0);
    ReAllocMem(StateBuffer, 0);
    ReAllocMem(CurrentBuffer, 0);
    ReAllocMem(VoltageBuffer, 0);
    ReAllocMem(FlickerBuffer, 0);
    ReAllocMem(SolutionBuffer, 0);
    ReAllocMem(WdgVoltagesBuffer, 0);
    ReAllocMem(WdgCurrentsBuffer, 0);
    ReAllocMem(PhsVoltagesBuffer, 0);

    inherited Destroy;
end;


{--------------------------------------------------------------------------}
procedure ConvertBlanks(var s: String);
var
    BlankPos: Integer;

begin
     { Convert spaces to Underscores }
    BlankPos := Pos(' ', S);
    while BlankPos > 0 do
    begin
        S[BlankPos] := '_';
        BlankPos := Pos(' ', S);
    end;
end;

{--------------------------------------------------------------------------}
procedure TMonitorObj.RecalcElementData;

var
    DevIndex: Integer;

begin
    ValidMonitor := FALSE;
    Devindex := GetCktElementIndex(ElementName);                   // Global function
    if DevIndex > 0 then
    begin                                       // Monitored element must already exist
        MeteredElement := ActiveCircuit.CktElements.Get(DevIndex);
        case (Mode and MODEMASK) of
            2, 8, 10:
            begin                                                // Must be transformer
                if (MeteredElement.DSSObjType and CLASSMASK) <> XFMR_ELEMENT then
                    if (MeteredElement.DSSObjType and CLASSMASK) <> AUTOTRANS_ELEMENT then
                    begin
                        DoSimpleMsg(MeteredElement.Name + ' is not a transformer!', 663);
                        Exit;
                    end;
            end;
            3:
            begin                                                // Must be PCElement
                if (MeteredElement.DSSObjType and BASECLASSMASK) <> PC_ELEMENT then
                begin
                    DoSimpleMsg(MeteredElement.Name + ' must be a power conversion element (Load or Generator)!', 664);
                    Exit;
                end;
            end;
            6:
            begin                                                // Checking Caps Tap
                if (MeteredElement.DSSObjType and CLASSMASK) <> CAP_ELEMENT then
                begin
                    DoSimpleMsg(MeteredElement.Name + ' is not a capacitor!', 2016001);
                    Exit;
                end;
            end;

            7:
            begin                                                // Checking if the element is a storage device
                if (MeteredElement.DSSObjType and CLASSMASK) <> STORAGE_ELEMENT then
                begin
                    DoSimpleMsg(MeteredElement.Name + ' is not a storage device!', 2016002);
                    Exit;
                end;
            end;


        end;

        if MeteredTerminal > MeteredElement.Nterms then
        begin
            DoErrorMsg('Monitor: "' + Name + '"',
                'Terminal no. "' + '" does not exist.',
                'Respecify terminal no.', 665);
        end
        else
        begin
            Nphases := MeteredElement.NPhases;
            Nconds := MeteredElement.NConds;

               // Sets name of i-th terminal's connected bus in monitor's buslist
               // This value will be used to set the NodeRef array (see TakeSample)
            Setbus(1, MeteredElement.GetBus(MeteredTerminal));
               // Make a name for the Buffer File
            BufferFile := {ActiveCircuit.CurrentDirectory + }
                CircuitName_ + 'Mon_' + Name + '.mon';
                 // removed 10/19/99 ConvertBlanks(BufferFile); // turn blanks into '_'

                 {Allocate Buffers}

            case (Mode and MODEMASK) of
                3:
                begin
                    NumStateVars := TPCElement(MeteredElement).Numvariables;
                    ReallocMem(StateBuffer, Sizeof(StateBuffer^[1]) * NumStatevars);
                end;
                4:
                begin
                    ReallocMem(FlickerBuffer, Sizeof(FlickerBuffer^[1]) * Nphases);
                end;
                5:
                begin
                    ReallocMem(SolutionBuffer, Sizeof(SolutionBuffer^[1]) * NumSolutionVars);
                end;
                8:
                begin
                    if (MeteredElement.DSSObjType and CLASSMASK) = AUTOTRANS_ELEMENT then
                        with  TAutoTransObj(MeteredElement) do
                            NumTransformerCurrents := 2 * NumberOfWindings * nphases
                    else
                        with  TTransfObj(MeteredElement) do
                            NumTransformerCurrents := 2 * NumberOfWindings * nphases;
                    ReallocMem(WdgCurrentsBuffer, Sizeof(Complex) * NumTransformerCurrents);
                end;
                10:
                begin
                    if (MeteredElement.DSSObjType and CLASSMASK) = AUTOTRANS_ELEMENT then
                        with  TAutoTransObj(MeteredElement) do
                            NumWindingVoltages := NumberOfWindings * nphases
                    else
                        with  TTransfObj(MeteredElement) do
                            NumWindingVoltages := NumberOfWindings * nphases;
                    ReallocMem(WdgVoltagesBuffer, Sizeof(Complex) * NumWindingVoltages);   // total all phases, all windings
                    ReallocMem(PhsVoltagesBuffer, Sizeof(Complex) * nphases);
                end;
            else
                ReallocMem(CurrentBuffer, SizeOf(CurrentBuffer^[1]) * MeteredElement.Yorder);
                ReallocMem(VoltageBuffer, SizeOf(VoltageBuffer^[1]) * MeteredElement.NConds);
            end;

            ClearMonitorStream;

            ValidMonitor := TRUE;

        end;

    end
    else
    begin
        MeteredElement := NIL;   // element not found
        DoErrorMsg('Monitor: "' + Self.Name + '"', 'Circuit Element "' + ElementName + '" Not Found.',
            ' Element must be defined previously.', 666);
    end;
end;

procedure TMonitorObj.MakePosSequence;
begin
    if MeteredElement <> NIL then
    begin
        Setbus(1, MeteredElement.GetBus(MeteredTerminal));
        Nphases := MeteredElement.NPhases;
        Nconds := MeteredElement.Nconds;
        case (Mode and MODEMASK) of
            3:
            begin
                NumStateVars := TPCElement(MeteredElement).Numvariables;
                ReallocMem(StateBuffer, Sizeof(StateBuffer^[1]) * NumStatevars);
            end;
            4:
            begin
                ReallocMem(FlickerBuffer, Sizeof(FlickerBuffer^[1]) * Nphases);
            end;
            5:
            begin
                ReallocMem(SolutionBuffer, Sizeof(SolutionBuffer^[1]) * NumSolutionVars);
            end;
        else
            ReallocMem(CurrentBuffer, SizeOf(CurrentBuffer^[1]) * MeteredElement.Yorder);
            ReallocMem(VoltageBuffer, SizeOf(VoltageBuffer^[1]) * MeteredElement.NConds);
        end;
        ClearMonitorStream;
        ValidMonitor := TRUE;
    end;
    inherited;
end;


{--------------------------------------------------------------------------}
procedure TMonitorObj.CalcYPrim;
begin

  {A Monitor is a zero current source; Yprim is always zero.}
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
end;

{--------------------------------------------------------------------------}
procedure TMonitorObj.ClearMonitorStream;

var
    i, j: Integer;
    iMax: Integer;
    iMin: Integer;
    IsPosSeq: Boolean;
    IsPower: Boolean;
    NameOfState: Ansistring;
    NumVI: Integer;
    RecordSize: Integer;
    strPtr: pANSIchar;
    Str_Temp: Ansistring;

begin
    try

        MonitorStream.Clear;
        IsProcessed := FALSE;
        SampleCount := 0;
        IsPosSeq := FALSE;
        fillchar(StrBuffer, Sizeof(TMonitorStrBuffer), 0);  {clear buffer}
        strPtr := @StrBuffer;
        strPtr^ := chr(0);     // Init string
        if ActiveCircuit.Solution.IsHarmonicModel then
            strLcat(strPtr, pAnsichar('Freq, Harmonic, '), Sizeof(TMonitorStrBuffer))
        else
            strLcat(strPtr, pAnsichar('hour, t(sec), '), Sizeof(TMonitorStrBuffer));

        case (Mode and MODEMASK) of

            2:
            begin
                RecordSize := 1;     // Transformer Taps
                strLcat(strPtr, pAnsichar('Tap (pu)'), Sizeof(TMonitorStrBuffer));
            end;
            3:
            begin
                RecordSize := NumStateVars;   // Statevariabes
                for i := 1 to NumStateVars do
                begin
                    NameofState := Ansistring(TpcElement(MeteredElement).VariableName(i) + ',');
                    strLcat(strPtr, pAnsichar(NameofState), Sizeof(TMonitorStrBuffer));
                end;
            end;
            4:
            begin
                RecordSize := 2 * FnPhases;
                for i := 1 to FnPhases do
                begin
                    strLcat(strPtr, pAnsichar(Ansistring('Flk' + IntToStr(i) + ', Pst' + IntToStr(i))), Sizeof(TMonitorStrBuffer));
                    if i < FnPhases then
                        strLcat(strPtr, pAnsichar(', '), Sizeof(TMonitorStrBuffer));
                end;
            end;
            5:
            begin
                RecordSize := NumSolutionVars;
                strLcat(strPtr, pAnsichar('TotalIterations, '), Sizeof(TMonitorStrBuffer));
                strLcat(strPtr, pAnsichar('ControlIteration, '), Sizeof(TMonitorStrBuffer));
                strLcat(strPtr, pAnsichar('MaxIterations, '), Sizeof(TMonitorStrBuffer));
                strLcat(strPtr, pAnsichar('MaxControlIterations, '), Sizeof(TMonitorStrBuffer));
                strLcat(strPtr, pAnsichar('Converged, '), Sizeof(TMonitorStrBuffer));
                strLcat(strPtr, pAnsichar('IntervalHrs, '), Sizeof(TMonitorStrBuffer));
                strLcat(strPtr, pAnsichar('SolutionCount, '), Sizeof(TMonitorStrBuffer));
                strLcat(strPtr, pAnsichar('Mode, '), Sizeof(TMonitorStrBuffer));
                strLcat(strPtr, pAnsichar('Frequency, '), Sizeof(TMonitorStrBuffer));
                strLcat(strPtr, pAnsichar('Year, '), Sizeof(TMonitorStrBuffer));
                strLcat(strPtr, pAnsichar('SolveSnap_uSecs, '), Sizeof(TMonitorStrBuffer));
                strLcat(strPtr, pAnsichar('TimeStep_uSecs, '), Sizeof(TMonitorStrBuffer));
            end;
            6:
            begin
                RecordSize := TCapacitorObj(MeteredElement).NumSteps;     // Capacitor Taps
                for i := 1 to RecordSize do
                begin
                    Str_Temp := Ansistring('Step_' + inttostr(i) + ' ');
                    strLcat(strPtr, pAnsichar(Str_Temp), Sizeof(TMonitorStrBuffer));
                end;
            end;
            7:
            begin
                RecordSize := 5;     // Storage state vars
                strLcat(strPtr, pAnsichar('kW output, '), Sizeof(TMonitorStrBuffer));
                strLcat(strPtr, pAnsichar('kvar output, '), Sizeof(TMonitorStrBuffer));
                strLcat(strPtr, pAnsichar('kW Stored, '), Sizeof(TMonitorStrBuffer));
                strLcat(strPtr, pAnsichar('%kW Stored, '), Sizeof(TMonitorStrBuffer));
                strLcat(strPtr, pAnsichar('State, '), Sizeof(TMonitorStrBuffer));
            end;
            8:
            begin   // All winding Currents
                if (MeteredElement.DSSObjType and CLASSMASK) = AUTOTRANS_ELEMENT then
                    with TAutoTransObj(MeteredElement) do
                    begin
                        RecordSize := NumTransformerCurrents;     // Transformer Winding Currents
                        for i := 1 to Nphases do
                        begin
                            for j := 1 to NumberOfWindings do
                            begin
                                Str_Temp := Ansistring(Format('P%dW%d,Deg, ', [i, j]));
                                strLcat(strPtr, pAnsichar(Str_Temp), Sizeof(TMonitorStrBuffer));
                            end;
                        end;
                    end
                else
                    with TTransfObj(MeteredElement) do
                    begin
                        RecordSize := NumTransformerCurrents;     // Transformer Winding Currents
                        for i := 1 to Nphases do
                        begin
                            for j := 1 to NumberOfWindings do
                            begin
                                Str_Temp := Ansistring(Format('P%dW%d,Deg, ', [i, j]));
                                strLcat(strPtr, pAnsichar(Str_Temp), Sizeof(TMonitorStrBuffer));
                            end;
                        end;
                    end;
            end;
            9:
            begin // watts vars of meteredElement
                RecordSize := 2;
                strLcat(strPtr, pAnsichar('watts, vars'), Sizeof(TMonitorStrBuffer));
            end;
            10:
            begin // All Winding Voltages
                if (MeteredElement.DSSObjType and CLASSMASK) = AUTOTRANS_ELEMENT then
                    with TAutoTransObj(MeteredElement) do
                    begin
                        RecordSize := 2 * NumberOfWindings * Nphases;     // Transformer Winding woltages
                        for i := 1 to Nphases do
                        begin
                            for j := 1 to NumberOfWindings do
                            begin
                                Str_Temp := Ansistring(Format('P%dW%d,Deg, ', [i, j]));
                                strLcat(strPtr, pAnsichar(Str_Temp), Sizeof(TMonitorStrBuffer));
                            end;
                        end;
                    end
                else
                    with TTransfObj(MeteredElement) do
                    begin
                        RecordSize := 2 * NumberOfWindings * Nphases;     // Transformer Winding woltages
                        for i := 1 to Nphases do
                        begin
                            for j := 1 to NumberOfWindings do
                            begin
                                Str_Temp := Ansistring(Format('P%dW%d,Deg, ', [i, j]));
                                strLcat(strPtr, pAnsichar(Str_Temp), Sizeof(TMonitorStrBuffer));
                            end;
                        end;
                    end;
            end;

        else
        begin
         // Compute RecordSize
         // Use same logic as in TakeSample Method

            if ((Mode and SEQUENCEMASK) > 0) and (Fnphases = 3) then
            begin  // Convert to Symmetrical components
                IsPosSeq := TRUE;
                NumVI := 3;
            end
            else
            begin
                NumVI := Fnconds;
            end;
          // Convert Voltage Buffer to power kW, kvar
            if ((Mode and MODEMASK) = 1) then
                IsPower := TRUE
            else
                IsPower := FALSE;

            case (Mode and (MAGNITUDEMASK + POSSEQONLYMASK)) of
                32:
                begin // Save Magnitudes only
                    RecordSize := 0;
                    for i := 1 to NumVI do
                        Inc(RecordSize, 1);
                    if not IsPower then
                    begin
                        for i := 1 to NumVI do
                            Inc(RecordSize, 1);
                        if IncludeResidual then
                            Inc(RecordSize, 2);
                        for i := 1 to NumVI do
                        begin
                            strLcat(strPtr, pAnsichar(Ansistring(Format('|V|%d (volts)', [i]))), Sizeof(TMonitorStrBuffer));
                            strLcat(strPtr, pAnsichar(', '), Sizeof(TMonitorStrBuffer));
                        end;
                        if IncludeResidual then
                        begin
                            strLcat(strPtr, pAnsichar('|VN| (volts)'), Sizeof(TMonitorStrBuffer));
                            strLcat(strPtr, pAnsichar(', '), Sizeof(TMonitorStrBuffer));
                        end;
                        for i := 1 to NumVI do
                        begin
                            strLcat(strPtr, pAnsichar(Ansistring('|I|' + IntToStr(i) + ' (amps)')), Sizeof(TMonitorStrBuffer));
                            if i < NumVI then
                                strLcat(strPtr, pAnsichar(', '), Sizeof(TMonitorStrBuffer));
                        end;
                        if IncludeResidual then
                        begin
                            strLcat(strPtr, pAnsichar(',|IN| (amps)'), Sizeof(TMonitorStrBuffer));
                        end;
                    end
                    else
                    begin  // Power
                        for i := 1 to NumVI do
                        begin
                            if PPolar then
                                strLcat(strPtr, pAnsichar(Ansistring('S' + IntToStr(i) + ' (kVA)')), Sizeof(TMonitorStrBuffer))
                            else
                                strLcat(strPtr, pAnsichar(Ansistring('P' + IntToStr(i) + ' (kW)')), Sizeof(TMonitorStrBuffer));
                            if i < NumVI then
                                strLcat(strPtr, pAnsichar(', '), Sizeof(TMonitorStrBuffer));
                        end;
                    end;
                end;
                64:
                begin // Save Pos Seq or Total of all Phases or Total power (Complex)
                    RecordSize := 2;
                    if not IsPower then
                    begin
                        RecordSize := RecordSize + 2;
                        if VIPolar then
                            strLcat(strPtr, pAnsichar('V1, V1ang, I1, I1ang'), Sizeof(TMonitorStrBuffer))
                        else
                            strLcat(strPtr, pAnsichar('V1.re, V1.im, I1.re, I1.im'), Sizeof(TMonitorStrBuffer));
                    end
                    else
                    begin
                        if Ppolar then
                            strLcat(strPtr, pAnsichar('S1 (kVA), Ang '), Sizeof(TMonitorStrBuffer))
                        else
                            strLcat(strPtr, pAnsichar('P1 (kW), Q1 (kvar)'), Sizeof(TMonitorStrBuffer));
                    end;
                end;
                96:
                begin  // Save Pos Seq or Aver magnitude of all Phases of total kVA (Magnitude)
                    RecordSize := 1;
                    if not IsPower then
                    begin
                        RecordSize := RecordSize + 1;
                        strLcat(strPtr, pAnsichar('V, I '), Sizeof(TMonitorStrBuffer));
                    end
                    else
                    begin  // Power
                        if Ppolar then
                            strLcat(strPtr, pAnsichar('S1 (kVA)'), Sizeof(TMonitorStrBuffer))
                        else
                            strLcat(strPtr, pAnsichar('P1 (kW)'), Sizeof(TMonitorStrBuffer));
                    end;
                end;

            else // save  V and I in mag and angle or complex kW, kvar
                RecordSize := NumVI * 2;
                if not IsPower then
                begin
                    if isPosSeq then
                    begin
                        iMin := 0;
                        iMax := NumVI - 1;
                    end
                    else
                    begin
                        iMin := 1;
                        iMax := NumVI;
                    end;
                    RecordSize := RecordSize + NumVI * 2;
                    if IncludeResidual then
                        Inc(RecordSize, 4);
                    for i := iMin to iMax do
                    begin
                        if VIPolar then
                            strLcat(strPtr, pAnsichar(Ansistring('V' + IntToStr(i) + ', VAngle' + IntToStr(i))), Sizeof(TMonitorStrBuffer))
                        else
                            strLcat(strPtr, pAnsichar(Ansistring('V' + IntToStr(i) + '.re, V' + IntToStr(i) + '.im')), Sizeof(TMonitorStrBuffer));
                        strLcat(strPtr, pAnsichar(', '), Sizeof(TMonitorStrBuffer));
                    end;
                    if IncludeResidual then
                    begin
                        if VIPolar then
                            strLcat(strPtr, pAnsichar('VN, VNAngle'), Sizeof(TMonitorStrBuffer))
                        else
                            strLcat(strPtr, pAnsichar('VN.re, VN.im'), Sizeof(TMonitorStrBuffer));
                        strLcat(strPtr, pAnsichar(', '), Sizeof(TMonitorStrBuffer));
                    end;
                    for i := iMin to iMax do
                    begin
                        if VIPolar then
                            strLcat(strPtr, pAnsichar(Ansistring('I' + IntToStr(i) + ', IAngle' + IntToStr(i))), Sizeof(TMonitorStrBuffer))
                        else
                            strLcat(strPtr, pAnsichar(Ansistring('I' + IntToStr(i) + '.re, I' + IntToStr(i) + '.im')), Sizeof(TMonitorStrBuffer));
                        if i < NumVI then
                            strLcat(strPtr, pAnsichar(', '), Sizeof(TMonitorStrBuffer));
                    end;
                    if IncludeResidual then
                    begin
                        if VIPolar then
                            strLcat(strPtr, pAnsichar(', IN, INAngle'), Sizeof(TMonitorStrBuffer))
                        else
                            strLcat(strPtr, pAnsichar(', IN.re, IN.im'), Sizeof(TMonitorStrBuffer));
                    end;
                end
                else
                begin
                    if isPosSeq then
                    begin
                        iMin := 0;
                        iMax := NumVI - 1;
                    end
                    else
                    begin
                        iMin := 1;
                        iMax := NumVI;
                    end;
                    for i := iMin to iMax do
                    begin
                        if Ppolar then
                            strLcat(strPtr, pAnsichar(Ansistring('S' + IntToStr(i) + ' (kVA), Ang' + IntToStr(i))), Sizeof(TMonitorStrBuffer))
                        else
                            strLcat(strPtr, pAnsichar(Ansistring('P' + IntToStr(i) + ' (kW), Q' + IntToStr(i) + ' (kvar)')), Sizeof(TMonitorStrBuffer));
                        if i < NumVI then
                            strLcat(strPtr, pAnsichar(', '), Sizeof(TMonitorStrBuffer));
                    end;
                end;
            end;
        end;
        end;  {CASE}


     // RecordSize is the number of singles in the sample (after the hour and sec)

     // Write Header to Monitor Stream
     // Write ID so we know it is a DSS Monitor file and which version in case we
     // change it down the road

        with MonitorStream do
        begin
            Write(FileSignature, Sizeof(FileSignature));
            Write(FileVersion, Sizeof(FileVersion));
            Write(RecordSize, Sizeof(RecordSize));
            Write(Mode, Sizeof(Mode));
            Write(StrBuffer, Sizeof(TMonitorStrBuffer));
        end;

{    So the file now looks like:
       FileSignature (4 bytes)    32-bit Integers
       FileVersion   (4)
       RecordSize    (4)
       Mode          (4)
       String        (256)

       hr   (4)       all singles
       Sec  (4)
       Sample  (4*RecordSize)
       ...

 }

    except
        On E: Exception do
            DoErrorMsg('Cannot open Monitor file.',
                E.Message,
                'Monitor: "' + Name + '"', 670)

    end;
end;


{--------------------------------------------------------------------------}
procedure TMonitorObj.OpenMonitorStream;
begin

    if not IsFileOpen then
    begin
        MonitorStream.Seek(0, soFromEnd);    // Positioned at End of Stream
        IsFileOpen := TRUE;
    end;

end;

{--------------------------------------------------------------------------}
procedure TMonitorObj.CloseMonitorStream;
begin
    try
        if IsFileOpen then
        begin  // only close open files
            PostProcess;
            MonitorStream.Seek(0, soFromBeginning);   // just move stream position to the beginning
            IsFileOpen := FALSE;
        end;
    except
        On E: Exception do
            DoErrorMsg('Cannot close Monitor stream.',
                E.Message,
                'Monitor: "' + Name + '"', 671)
    end;
end;

{--------------------------------------------------------------------------}
procedure TMonitorObj.Save;

// Saves present buffer to monitor file, resets bufferptrs and continues

begin

    if not IsFileOpen then
        OpenMonitorStream; // Position to end of stream

     {Write present monitor buffer to monitorstream}
    MonitorStream.Write(MonBuffer^, SizeOF(MonBuffer^[1]) * BufPtr);

    BufPtr := 0; // reset Buffer for next

end;


{--------------------------------------------------------------------------}
procedure TMonitorObj.ResetIt;
begin
    BufPtr := 0;
    ClearMonitorStream;
end;

{--------------------------------------------------------------------------}
procedure TMonitorObj.PostProcess;
begin
    if IsProcessed = FALSE then
    begin
        if (mode = 4) and (MonitorStream.Position > 0) then
            DoFlickerCalculations;
    end;
    IsProcessed := TRUE;
end;

{--------------------------------------------------------------------------}
procedure TMonitorObj.TakeSample;
var
    dHour: Double;
    dSum: Double;
    i, j, k: Integer;
    IsPower: Boolean;
    IsSequence: Boolean;
    NumVI: Integer;
    Offset: Integer;
    ResidualCurr: Complex;
    ResidualVolt: Complex;
    Sum: Complex;
    CplxLosses: Complex;
    V012, I012: array[1..3] of Complex;


begin

    if not (ValidMonitor and Enabled) then
        Exit;

    inc(SampleCount);

    Hour := ActiveCircuit.Solution.DynaVars.intHour;
    Sec := ActiveCircuit.Solution.Dynavars.t;

    Offset := (MeteredTerminal - 1) * MeteredElement.NConds;

   //Save time unless Harmonics mode and then save Frequency and Harmonic
    with ActiveCircuit.Solution do
        if IsHarmonicModel then
        begin
            AddDblsToBuffer(@Frequency, 1);  // put freq in hour slot as a double
            AddDblsToBuffer(@Harmonic, 1);  // stick harmonic in time slot in buffer
        end
        else
        begin
            dHour := Hour;      // convert to double
            AddDblsToBuffer(@dHour, 1);  // put hours in buffer as a double
            AddDblsToBuffer(@Sec, 1);  // stick time in sec in buffer
        end;

    case (Mode and MODEMASK) of

        0, 1:       // Voltage, current. Powers
        begin

            // MeteredElement.GetCurrents(CurrentBuffer);
            // To save some time, call ComputeITerminal
            MeteredElement.ComputeIterminal;   // only does calc if needed
            for i := 1 to MeteredElement.Yorder do
                CurrentBuffer^[i] := MeteredElement.Iterminal^[i];

            try
                for i := 1 to Fnconds do
                begin
                // NodeRef is set by the main Circuit object
                // It is the index of the terminal into the system node list
                    VoltageBuffer^[i] := ActiveCircuit.Solution.NodeV^[NodeRef^[i]];
                end;
            except
                On E: Exception do
                    DoSimpleMsg(E.Message + CRLF + 'NodeRef is invalid. Try solving a snapshot or direct before solving in a mode that takes a monitor sample.', 672);
            end;
        end;

        2:
        begin     // Monitor Transformer Tap Position
            if (MeteredElement.DSSObjType and CLASSMASK) = AUTOTRANS_ELEMENT then
                with TAutoTransObj(MeteredElement) do
                    AddDblToBuffer(PresentTap[MeteredTerminal])
            else
                with TTransfObj(MeteredElement) do
                    AddDblToBuffer(PresentTap[MeteredTerminal]);

            Exit;  // Done with this mode now.
        end;

        3:
        begin   // Pick up device state variables
            TPCElement(MeteredElement).GetAllVariables(StateBuffer);
            AddDblsToBuffer(StateBuffer, NumStateVars);
            Exit; // Done with this mode now
        end;

        4:
        begin   // RMS phase voltages for flicker evaluation
            try
                for i := 1 to Fnphases do
                begin
                    FlickerBuffer^[i] := ActiveCircuit.Solution.NodeV^[NodeRef^[i]];
                end;
            except
                On E: Exception do
                    DoSimpleMsg(E.Message + CRLF + 'NodeRef is invalid. Try solving a snapshot or direct before solving in a mode that takes a monitor sample.', 672);
            end;
        end;

        5:
        begin
            (* Capture Solution Variables *)
            with ActiveCircuit.Solution do
            begin
                SolutionBuffer^[1] := Iteration;
                SolutionBuffer^[2] := ControlIteration;
                SolutionBuffer^[3] := MaxIterations;
                SolutionBuffer^[4] := MaxControlIterations;
                if ConvergedFlag then
                    SolutionBuffer^[5] := 1
                else
                    SolutionBuffer^[5] := 0;
                SolutionBuffer^[6] := IntervalHrs;
                SolutionBuffer^[7] := SolutionCount;
                SolutionBuffer^[8] := Ord(Mode);
                SolutionBuffer^[9] := Frequency;
                SolutionBuffer^[10] := Year;
                SolutionBuffer^[11] := Time_Solve;
                SolutionBuffer^[12] := Time_Step;
            end;

        end;

        6:
        begin     // Monitor Capacitor State

            with TCapacitorObj(MeteredElement) do
            begin
                for i := 1 to NumSteps do
                begin
                    AddDblToBuffer(States[i]);
                end;
            end;
            Exit;  // Done with this mode now.
        end;
        7:
        begin     // Monitor Storage Device state variables
            with TStorageObj(MeteredElement) do
            begin
                AddDblToBuffer(PresentkW);
                AddDblToBuffer(Presentkvar);
                AddDblToBuffer(StorageVars.kWhStored);
                AddDblToBuffer(((StorageVars.kWhStored) / (StorageVars.kWhRating)) * 100);
                AddDblToBuffer(StorageState);
            end;
            Exit;  // Done with this mode now.
        end;

        8:
        begin   // Winding Currents
              // Get all currents in each end of each winding
            if (MeteredElement.DSSObjType and CLASSMASK) = AUTOTRANS_ELEMENT then
                with TAutoTransObj(MeteredElement) do
                begin
                    GetAllWindingCurrents(WdgCurrentsBuffer);
                    ConvertComplexArrayToPolar(WdgCurrentsBuffer, NumTransformerCurrents);
                  // Put every other Current into buffer
                  // Current magnitude is same in each end
                    k := 1;
                    for i := 1 to Nphases * NumberOfWindings do
                    begin
                        AddDblsToBuffer(@WdgCurrentsBuffer^[k].re, 2);  // Add Mag, Angle
                        k := k + 2;
                    end;
                end
            else
                with TTransfobj(MeteredElement) do
                begin
                    GetAllWindingCurrents(WdgCurrentsBuffer);
                    ConvertComplexArrayToPolar(WdgCurrentsBuffer, NumTransformerCurrents);
                // Put every other Current into buffer
                  // Current magnitude is same in each end
                    k := 1;
                    for i := 1 to Nphases * NumberOfWindings do
                    begin
                        AddDblsToBuffer(@WdgCurrentsBuffer^[k].re, 2);  // Add Mag, Angle
                        k := k + 2;
                    end;
               // AddDblsToBuffer(@WdgCurrentsBuffer^[1].re, NumTransformerCurrents);
                end;
            Exit;
        end;

        9:
        begin  // losses
            CplxLosses := MeteredElement.Losses;
            AddDblToBuffer(CplxLosses.re);
            AddDblToBuffer(CplxLosses.im);
            Exit; // Done with this mode now.
        end;

        10:
        begin   // Winding Voltages
              // Get all Voltages across each winding and put into buffer
            if (MeteredElement.DSSObjType and CLASSMASK) = AUTOTRANS_ELEMENT then
                with TAutoTransObj(MeteredElement) do
                begin
                    for i := 1 to NumberOfWindings do
                    begin
                        GetAutoWindingVoltages(i, PhsVoltagesBuffer);
                        for j := 1 to nphases do
                            WdgVoltagesBuffer^[i + (j - 1) * NumberofWindings] := PhsVoltagesBuffer^[j];
                    end;
                    ConvertComplexArrayToPolar(WdgVoltagesBuffer, NumWindingVoltages);
                  {Put winding Voltages into Monitor}
                    AddDblsToBuffer(@WdgVoltagesBuffer^[1].re, 2 * NumWindingVoltages);  // Add Mag, Angle each winding
                end

            else
                with TTransfobj(MeteredElement) do
                begin
                    for i := 1 to NumberOfWindings do
                    begin
                        GetWindingVoltages(i, PhsVoltagesBuffer);
                        for j := 1 to nphases do
                            WdgVoltagesBuffer^[i + (j - 1) * NumberofWindings] := PhsVoltagesBuffer^[j];
                    end;
                    ConvertComplexArrayToPolar(WdgVoltagesBuffer, NumWindingVoltages);
                  {Put winding Voltages into Monitor}
                    AddDblsToBuffer(@WdgVoltagesBuffer^[1].re, 2 * NumWindingVoltages);  // Add Mag, Angle each winding
                end;
            Exit;
        end;
    else
        Exit  // Ignore invalid mask

    end;


    if ((Mode and SEQUENCEMASK) > 0) and (Fnphases = 3) then
    begin  // Convert to Symmetrical components
        Phase2SymComp(VoltageBuffer, @V012);
        Phase2SymComp(@CurrentBuffer^[Offset + 1], @I012);
        NumVI := 3;
        IsSequence := TRUE;
       // Replace voltage and current buffer with sequence quantities
        for i := 1 to 3 do
            VoltageBuffer^[i] := V012[i];
        for i := 1 to 3 do
            CurrentBuffer[Offset + i] := I012[i];
    end
    else
    begin
        NumVI := Fnconds;
        IsSequence := FALSE;
    end;

    IsPower := FALSE;  // Init so compiler won't complain
    case (Mode and MODEMASK) of
        0:
        begin        // Convert to Mag, Angle   and compute residual if required
            IsPower := FALSE;
            if IncludeResidual then
            begin
                if VIPolar then
                begin
                    ResidualVolt := ResidualPolar(@VoltageBuffer^[1], Fnphases);
                    ResidualCurr := ResidualPolar(@CurrentBuffer^[Offset + 1], Fnphases);
                end
                else
                begin
                    ResidualVolt := Residual(@VoltageBuffer^[1], Fnphases);
                    ResidualCurr := Residual(@CurrentBuffer^[Offset + 1], Fnphases);
                end;
            end;
            if VIPolar then
            begin
                ConvertComplexArrayToPolar(VoltageBuffer, NumVI);
                ConvertComplexArrayToPolar(@CurrentBuffer^[Offset + 1], NumVI);    // Corrected 3-11-13
            end;
        end;
        1:
        begin     // Convert Voltage Buffer to power kW, kvar or Mag/Angle
            CalckPowers(VoltageBuffer, VoltageBuffer, @CurrentBuffer^[Offset + 1], NumVI);
            if (IsSequence or ActiveCircuit.PositiveSequence) then
                CmulArray(VoltageBuffer, 3.0, NumVI); // convert to total power
            if Ppolar then
                ConvertComplexArrayToPolar(VoltageBuffer, NumVI);
            IsPower := TRUE;
        end;
        4:
        begin
            IsPower := FALSE;
            ConvertComplexArrayToPolar(FlickerBuffer, Fnphases);
        end
    else
    end;

   // Now check to see what to write to disk
    case (Mode and (MAGNITUDEMASK + POSSEQONLYMASK)) of
        32:
        begin // Save Magnitudes only
            for i := 1 to NumVI do
                AddDblToBuffer(VoltageBuffer^[i].re {Cabs(VoltageBuffer^[i])});
            if IncludeResidual then
                AddDblToBuffer(ResidualVolt.re);
            if not IsPower then
            begin
                for i := 1 to NumVI do
                    AddDblToBuffer(CurrentBuffer^[Offset + i].re {Cabs(CurrentBuffer^[Offset+i])});
                if IncludeResidual then
                    AddDblToBuffer(ResidualCurr.re);
            end;
        end;
        64:
        begin // Save Pos Seq or Avg of all Phases or Total power (Complex)
            if isSequence then
            begin
                AddDblsToBuffer(@VoltageBuffer^[2].re, 2);
                if not IsPower then
                    AddDblsToBuffer(@CurrentBuffer^[Offset + 2].re, 2);
            end
            else
            begin
                if IsPower then
                begin
                    Sum := cZero;
                    for i := 1 to Fnphases do
                        Caccum(Sum, VoltageBuffer^[i]);
                    AddDblsToBuffer(@Sum.re, 2);
                end
                else
                begin  // Average the phase magnitudes and  sum angles
                    Sum := cZero;
                    for i := 1 to Fnphases do
                        Caccum(Sum, VoltageBuffer^[i]);
                    Sum.re := Sum.re / FnPhases;
                    AddDblsToBuffer(@Sum.re, 2);
                    Sum := cZero;
                    for i := 1 to Fnphases do
                        Caccum(Sum, CurrentBuffer^[Offset + i]);   // Corrected 3-11-13
                    Sum.re := Sum.re / FnPhases;
                    AddDblsToBuffer(@Sum.re, 2);
                end;
            end;
        end;
        96:
        begin  // Save Pos Seq or Aver magnitude of all Phases of total kVA (Magnitude)
            if isSequence then
            begin
                AddDblToBuffer(VoltageBuffer^[2].Re);    // First double is magnitude
                if not IsPower then
                    AddDblToBuffer(CurrentBuffer^[Offset + 2].Re);
            end
            else
            begin
                dSum := 0.0;
                for i := 1 to Fnphases do
                    dSum := dSum + VoltageBuffer^[i].re; //Cabs(VoltageBuffer^[i]);
                if not IsPower then
                    dSum := dSum / Fnphases;
                AddDblToBuffer(dSum);
                if not IsPower then
                begin
                    dSum := 0.0;
                    for i := 1 to Fnphases do
                        dSum := dSum + CurrentBuffer^[Offset + i].re; //Cabs(CurrentBuffer^[Offset+i]);
                    dSum := dSum / Fnphases;
                    AddDblToBuffer(dSum);
                end;
            end;
        end;

    else
        case Mode of
            4:
                AddDblsToBuffer(@FlickerBuffer^[1].re, Fnphases * 2);
            5:
                AddDblsToBuffer(@SolutionBuffer^[1], NumSolutionVars);
        else
        begin
            AddDblsToBuffer(@VoltageBuffer^[1].re, NumVI * 2);
            if not IsPower then
            begin
                if IncludeResidual then
                    AddDblsToBuffer(@ResidualVolt, 2);
                AddDblsToBuffer(@CurrentBuffer^[Offset + 1].re, NumVI * 2);
                if IncludeResidual then
                    AddDblsToBuffer(@ResidualCurr, 2);
            end;
        end;
        end;
    end;
end;

{--------------------------------------------------------------------------}
procedure TMonitorObj.AddDblsToBuffer(Dbl: pDoubleArray; Ndoubles: Integer);

var
    i: Integer;

begin
    for i := 1 to Ndoubles do
        AddDblToBuffer(Dbl^[i]);
end;

{--------------------------------------------------------------------------}
procedure TMonitorObj.AddDblToBuffer(const Dbl: Double);

begin
    // first check to see if there's enough room
    // if not, save to monitorstream first.
    if BufPtr = BufferSize then
        Save;
    Inc(BufPtr);
    MonBuffer^[BufPtr] := Dbl;
end;

procedure TMonitorObj.DoFlickerCalculations;
var
    FSignature: Integer;
    Fversion: Integer;
    RecordSize: Cardinal;
    RecordBytes: Cardinal;
    SngBuffer: array[1..100] of Single;
    hr: Single;
    s: Single;
    N: Integer;
    Npst: Integer;
    i, p: Integer;
    bStart: Integer;
    data: array of pSingleArray; // indexed from zero (time) to FnPhases
    pst: array of pSingleArray; // indexed from zero to FnPhases - 1
    ipst: Integer;
    tpst: Single;
    defaultpst: Single;
    Vbase: Single;
    busref: Integer;
begin
    N := SampleCount;
    with MonitorStream do
    begin
        Seek(0, soFromBeginning);  // Start at the beginning of the Stream
        Read(Fsignature, Sizeof(Fsignature));
        Read(Fversion, Sizeof(Fversion));
        Read(RecordSize, Sizeof(RecordSize));
        Read(Mode, Sizeof(Mode));
        Read(StrBuffer, Sizeof(StrBuffer));
        bStart := Position;
    end;
    RecordBytes := Sizeof(SngBuffer[1]) * RecordSize;
    try
    // read rms voltages out of the monitor stream into arrays
        SetLength(data, Fnphases + 1);
        SetLength(pst, Fnphases);
        for p := 0 to FnPhases do
            data[p] := AllocMem(Sizeof(SngBuffer[1]) * N);
        i := 1;
        while not (MonitorStream.Position >= MonitorStream.Size) do
        begin
            with MonitorStream do
            begin
                Read(hr, SizeOf(hr));
                Read(s, SizeOf(s));
                Read(SngBuffer, RecordBytes);
                data[0][i] := s + 3600.0 * hr;
                for p := 1 to FnPhases do
                    data[p][i] := SngBuffer[2 * p - 1];
                i := i + 1;
            end;
        end;

    // calculate the flicker level and pst
        Npst := 1 + Trunc(data[0][N] / 600.0); // pst updates every 10 minutes or 600 seconds
        for p := 0 to FnPhases - 1 do
        begin
            pst[p] := AllocMem(Sizeof(SngBuffer[1]) * Npst);
            busref := MeteredElement.Terminals[MeteredTerminal].BusRef;
            Vbase := 1000.0 * ActiveCircuit.Buses^[busref].kVBase;
            FlickerMeter(N, BaseFrequency, Vbase, data[0], data[p + 1], pst[p]);
        end;

    // stuff the flicker level and pst back into the monitor stream
        with MonitorStream do
        begin
            Position := bStart;
            tpst := 0.0;
            ipst := 0;
            defaultpst := 0;
            for i := 1 to N do
            begin
                if (data[0][i] - tpst) >= 600.0 then
                begin
                    inc(ipst);
                    tpst := data[0][i];
                end;
                Position := Position + 2 * SizeOf(hr); // don't alter the time
                for p := 1 to FnPhases do
                begin
                    Write(data[p][i], sizeof(data[p][i]));
                    if (ipst > 0) and (ipst <= Npst) then
                        Write(pst[p - 1][ipst], sizeof(pst[p - 1][ipst]))
                    else
                        Write(defaultpst, sizeof(defaultpst))
                end;
            end;
        end;
    finally
        for p := 0 to FnPhases do
            ReAllocMem(data[p], 0);
        for p := 0 to FnPhases - 1 do
            ReAllocMem(pst[p], 0);
    end;
end;

{--------------------------------------------------------------------------}
procedure TMonitorObj.TranslateToCSV(Show: Boolean);


var
    CSVName: String;
    F: TextFile;
    FSignature: Integer;
    Fversion: Integer;
    hr: Single;
    i: Cardinal;
    Mode: Integer;
    Nread: Cardinal;
    pStr: pAnsichar;
    RecordBytes: Cardinal;
    RecordSize: Cardinal;
    s: Single;
    sngBuffer: array[1..100] of Single;

begin

    Save;  // Save present buffer
    CloseMonitorStream;   // Position at beginning

    CSVName := Get_FileName;

    try
        AssignFile(F, CSVName);    // Make CSV file
        Rewrite(F);
    except
        On E: Exception do
        begin
            DoSimpleMsg('Error opening CSVFile "' + CSVName + '" for writing' + CRLF + E.Message, 672);
            Exit
        end;
    end;

    with MonitorStream do
    begin
        Seek(0, soFromBeginning);  // Start at the beginning of the Stream
        Read(Fsignature, Sizeof(Fsignature));
        Read(Fversion, Sizeof(Fversion));
        Read(RecordSize, Sizeof(RecordSize));
        Read(Mode, Sizeof(Mode));
        Read(StrBuffer, Sizeof(StrBuffer));
    end;

    pStr := @StrBuffer;
    Writeln(F, pStr);
    RecordBytes := Sizeof(SngBuffer[1]) * RecordSize;

    try
        try

            while not (MonitorStream.Position >= MonitorStream.Size) do
            begin
                with MonitorStream do
                begin
                    Read(hr, SizeOF(hr));
                    Read(s, SizeOf(s));
                    Nread := Read(sngBuffer, RecordBytes);
                end;
                if Nread < RecordBytes then
                    Break;
                Write(F, hr: 0: 0);          // hours
                Write(F, ', ', s: 0: 5);     // sec
                for i := 1 to RecordSize do
                begin
                    Write(F, ', ', Format('%-.6g', [sngBuffer[i]]))
                end;
                Writeln(F);
            end;

        except

            On E: Exception do
            begin
                DoSimpleMsg('Error Writing CSVFile "' + CSVName + '" ' + CRLF + E.Message, 673);
            end;

        end;

    finally

        CloseMonitorStream;
        CloseFile(F);

    end;

    if Show then
        FireOffEditor(CSVName);

    GlobalResult := CSVName;
end;

{--------------------------------------------------------------------------}
procedure TMonitorObj.GetCurrents(Curr: pComplexArray);  //Get present value of terminal Curr for reports
var
    i: Integer;
begin

{
  Revised 12-7-99 to return Zero current instead of Monitored element current because
 it was messing up Newton iteration.
}

    for i := 1 to Fnconds do
        Curr^[i] := CZERO;

end;

{--------------------------------------------------------------------------}
procedure TMonitorObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i, k: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
        end;


    if Complete then
    begin
        Writeln(F);
        Writeln(F, '// BufferSize=', BufferSize: 0);
        Writeln(F, '// Hour=', Hour: 0);
        Writeln(F, '// Sec=', Sec: 0);
        Writeln(F, '// BaseFrequency=', BaseFrequency: 0: 1);
        Writeln(F, '// Bufptr=', BufPtr: 0);
        Writeln(F, '// Buffer=');
        k := 0;
        for i := 1 to BufPtr do
        begin
            Write(F, MonBuffer^[i]: 0: 1, ', ');
            Inc(k);
            if k = (2 + Fnconds * 4) then
            begin
                Writeln(F);
                k := 0;
            end;
        end;
        Writeln(F);
    end;

end;

procedure TMonitorObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := ''; //'element';
    PropertyValue[2] := '1'; //'terminal';
    PropertyValue[3] := '0'; //'mode';
    PropertyValue[4] := ''; // 'action';  // buffer=clear|save|take|process
    PropertyValue[5] := 'NO';
    PropertyValue[6] := 'YES';
    PropertyValue[7] := 'YES';

    inherited  InitPropertyValues(NumPropsThisClass);

end;


{--------------------------------------------------------------------------}

procedure TDSSMonitor.TOPExport(ObjName: String);

var
    NameList, CNames: TStringList;
    Vbuf, CBuf: pDoubleArray;
    Obj: TMonitorObj;
    i: Integer;
    MaxTime: Double;
    ObjList: TPointerList;
    Hours: Boolean;
    StrBuffer: TMonitorStrBuffer;
    pStrBuffer: PAnsichar;
    Fversion, FSignature, iMode: Integer;
    Nread, RecordSize, RecordBytes, PositionSave: Cardinal;
    sngBuffer: array[1..100] of Single;
    time: Double;
    hr, s: Single;
    TrialFileName, FileNumber: String;

begin
     // Create a unique file name
    TrialFileName := GetOutputDirectory + 'TOP_Mon_' + ObjName;
    FileNumber := '';
    i := 0;
    while FileExists(TrialFileName + FileNumber + '.STO') do
    begin
        Inc(i);
        FileNumber := IntToStr(i);
    end;
    TOPTransferFile.FileName := TrialFileName + Filenumber + '.STO';
    try
        TOPTransferFile.Open;
    except
        ON E: Exception do
        begin
            DoSimpleMsg('TOP Transfer File Error: ' + E.message, 674);
            try
                TopTransferFile.Close;
            except
              {OK if Error}
            end;
            Exit;
        end;
    end;

     {Send only fixed interval data}

    ObjList := TPointerList.Create(10);
    NameList := TStringList.Create;
    CNames := TStringList.Create;

     {Make a List of fixed interval data where the interval is greater than 1 minute}
    if CompareText(ObjName, 'ALL') = 0 then
    begin
        DoSimpleMsg('ALL option not yet implemented.', 675);
     {
       Obj := ElementList.First;
       While Obj <>  Nil Do Begin
          If Obj.Interval>(1.0/60.0) Then ObjList.Add(Obj);
          Obj := ElementList.Next;
       End;
     }
    end
    else
    begin
        Obj := Find(ObjName);
        if Obj <> NIL then
            ObjList.Add(Obj)
        else
            DoSimpleMsg('Monitor.' + ObjName + ' not found.', 676);
    end;

     {If none found, exit}
    if ObjList.ListSize > 0 then
    begin

        Obj := ObjList.First;  {And only}
        with Obj do
        begin

            Save;  // Save present buffer
            CloseMonitorStream;

            pStrBuffer := @StrBuffer;
            with MonitorStream do
            begin
                Seek(0, soFromBeginning);  // Start at the beginning of the Stream
                Read(Fsignature, Sizeof(Fsignature));
                Read(Fversion, Sizeof(Fversion));
                Read(RecordSize, Sizeof(RecordSize));
                Read(iMode, Sizeof(iMode));
                Read(StrBuffer, Sizeof(StrBuffer));
            end;

           {Parse off Channel Names}
            AuxParser.Whitespace := '';
            AuxParser.CmdString := String(pStrBuffer);
            AuxParser.NextParam;  // pop off two
            AuxParser.NextParam;
            for i := 1 to RecordSize do
            begin
                AuxParser.NextParam;
                NameList.Add(AuxParser.StrValue);
            end;
            AuxParser.ResetDelims;

           {Write TOP Header}

          {Find Max number of points}
            RecordBytes := Sizeof(SngBuffer[1]) * RecordSize;
            VBuf := AllocMem(Sizeof(Double) * RecordSize);  // Put Everything in here for now
            CBuf := AllocMem(Sizeof(Double) * 1);   // just a dummy -- Cbuf is ignored here

           {Get first time value and set the interval to this value}
            hr := 0.0;
            s := 0.0;
            if not (MonitorStream.Position >= MonitorStream.Size) then
                with MonitorStream do
                begin
                    Read(hr, 4);  // singles
                    Read(s, 4);
                    Read(sngBuffer, RecordBytes);
                end;
           {Set Hours or Seconds for Interval}
            Hours := TRUE;
            if (s > 0.0) and (s < 100.0) then
                Hours := FALSE;

            case ActiveCircuit.Solution.DynaVars.SolutionMode of
                TSolveMode.HARMONICMODE:
                    Time := hr;
            else
                if Hours then
                    Time := hr + s / 3600.0 // in hrs
                else
                    Time := Hr * 3600.0 + s; // in sec
            end;

           {Now find Maxtime in Monitor}
            PositionSave := MonitorStream.Position;
            MonitorStream.Seek(-(Recordbytes + 8), soEnd);
            if not (MonitorStream.Position >= MonitorStream.Size) then
                with MonitorStream do
                begin
                    Read(hr, 4);  // singles
                    Read(s, 4);
                    Read(sngBuffer, RecordBytes);
                end;

            case ActiveCircuit.Solution.DynaVars.SolutionMode of
                TSolveMode.HARMONICMODE:
                    MaxTime := hr;
            else
                if Hours then
                    MaxTime := hr + s / 3600.0 // in hrs
                else
                    MaxTime := Hr * 3600.0 + s; // in sec
            end;

           {Go Back to where we were}
            MonitorStream.Seek(PositionSave, soBeginning);

            TopTransferFile.WriteHeader(Time, MaxTime, Time, RecordSize, 0, 16, 'DSS (TM), EPRI (R)');
            TopTransferFile.WriteNames(NameList, CNames);

           {Now Process rest of monitor file}

            if not (MonitorStream.Position >= MonitorStream.Size) then
                repeat
                    for i := 1 to RecordSize do
                        VBuf^[i] := SngBuffer[i];
                    TopTransferFile.WriteData(Time, Vbuf, Cbuf);
                    with MonitorStream do
                    begin
                        Read(hr, SizeOF(hr));
                        Read(s, SizeOf(s));
                        Nread := Read(sngBuffer, RecordBytes);
                    end;
                    if Nread < RecordBytes then
                        Break;
                    case ActiveCircuit.Solution.DynaVars.SolutionMode of
                        TSolveMode.HARMONICMODE:
                            Time := hr;
                    else
                        if Hours then
                            Time := hr + s / 3600.0 // in hrs
                        else
                            Time := hr * 3600.0 + s; // in sec
                    end;
                until (MonitorStream.Position >= MonitorStream.Size);

            CloseMonitorStream;

            TopTransferFile.Close;
            TopTransferFile.SendToTop;
            Reallocmem(Vbuf, 0);
            Reallocmem(Cbuf, 0);
        end;

    end;

    ObjList.Free;
    NameList.Free;
    CNames.Free;

end;

function TMonitorObj.Get_FileName: String;
begin
    Result := GetOutputDirectory + CircuitName_ + 'Mon_' + Name + '.csv'
end;

initialization
  //WriteDLLDebugFile('Monitor');

end.
