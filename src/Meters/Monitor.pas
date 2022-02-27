unit Monitor;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

// **For DSS Extensions:**
// Note that the header implementation is now different from the 
// official one. Compatibility could be added if required later,
// adding extra spaces before most fields.


//   06-04-18 Added modes 7-9
//   11-29-18 Added mode 10; revised mode 8
//   12-4-18  Added link to AutoTransformer
//   08-21-20 Added mode 11


//  A monitor is a circuit element that is connected to a terminal of another
//  circuit element.  It records the voltages and currents at that terminal as
//  a function of time and can report those values upon demand.
//
//  A Monitor is defined by a New commands:
//
//  New Type=Monitor Name=myname Element=elemname Terminal=[1,2,...] Buffer=clear|save
//
//  Upon creation, the monitor buffer is established.  There is a file associated
//  with the buffer.  It is named "Mon_elemnameN.mon"  where N is the terminal no.
//  The file is truncated to zero at creation or buffer clearing.
//
//  The Monitor keeps results in the in-memory buffer until it is filled.  Then it
//  appends the buffer to the associated file and resets the in-memory buffer.
//
//  For buffer=save, the present in-memory buffer is appended to the disk file so
//  that it is saved for later reference.
//
//  The Monitor is a passive device that takes a sample whenever its "TakeSample"
//  method is invoked.  The SampleAll method of the Monitor ckt element class will
//  force all monitors elements to take a sample.  If the present time (for the most
//  recent solution is greater than the last time entered in to the monitor buffer,
//  the sample is appended to the buffer.  Otherwise, it replaces the last entry.
//
//  Monitor Files are simple binary files of singles.  The first record
//  contains the number of conductors per terminal (NCond). (always use 'round' function
//  when converting this to an integer). Then subsequent records consist of time and
//  voltage and current samples for each terminal (all complex doubles) in the order
//  shown below:
//
//  <NCond>
//           <--- All voltages first ---------------->|<--- All currents ----->|
//  <hour 1> <sec 1> <V1.re>  <V1.im>  <V2.re>  <V2.im>  .... <I1.re>  <I1.im> ...
//  <hour 2> <sec 1> <V1.re>  <V1.im>  <V2.re>  <V2.im>  .... <I1.re>  <I1.im> ...
//  <hour 3> <sec 1> <V1.re>  <V1.im>  <V2.re>  <V2.im>  .... <I1.re>  <I1.im> ...
//
//  The time values will not necessarily be in a uniform time step;  they will
//  be at times samples or solutions were taken.  This could vary from several
//  hours down to a few milliseconds.
//
//  The monitor ID can be determined from the file name.  Thus, these values can
//  be post-processed at any later time, provided that the monitors are not reset.
//
//  Modes are:
//   0: Standard mode - V and I,each phase, Mag and Angle
//   1: Power each phase, complex (kw and kvars)
//   2: Transformer Tap
//   3: State Variables
//   4: Flicker level and severity index by phase (no modifiers apply)
//   5: Solution Variables (Iteration count, etc.)
//   6: Capacitor Switching (Capacitors only)
//   7: Storage Variables
//   8: Transformer Winding Currents
//   9: Losses (watts and vars)
//  10: Transformer Winding Voltages (across winding)
//  11: All terminal V and I, all conductors, mag and angle
//
//   +16: Sequence components: V012, I012
//   +32: Magnitude Only
//   +64: Pos Seq only or Average of phases

interface

uses
    Command,
    MeterClass,
    Meterelement,
    DSSClass,
    Arraydef,
    UComplex, DSSUcomplex,
    utilities,
    Classes;

type
{$SCOPEDENUMS ON}
    TMonitorProp = (
        INVALID = 0,
        element = 1, // TODO: has specific type according to the current mode
        terminal = 2,
        mode = 3,
        action = 4, // buffer=clear|save
        residual = 5, // buffer=clear|save
        VIPolar = 6, // V I in mag and angle rather then re and im
        PPolar = 7 // Power in power PF rather then power and vars
    );
{$SCOPEDENUMS OFF}

    TLegacyMonitorStrBuffer = array[1..256] of AnsiChar;

    TDSSMonitor = class(TMeterClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function BeginEdit(ptr: Pointer; SetActive_: Boolean=True): Pointer; override;
        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;

        procedure ResetAll; OVERRIDE;
        procedure SampleAll; OVERRIDE;  // Force all monitors to take a sample
        procedure SampleAllMode5;  // Sample just Mode 5 monitors
        procedure SaveAll; OVERRIDE;   // Force all monitors to save their buffers to disk
        procedure PostProcessAll;

    end;

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

        // BaseFrequency: Double; -- duplicated

        BufferFile: String;  // Name of file for catching buffer overflow

        IsFileOpen: Boolean;
        ValidMonitor: Boolean;
        IsProcessed: Boolean;

        recalc: Int8; // Used in Edit

        procedure AddDblsToBuffer(Dbl: pDoubleArray; Ndoubles: Integer);
        procedure AddDblToBuffer(const Dbl: Double);

        procedure DoFlickerCalculations;  // call from CloseMonitorStream
        function Get_FileName: String;


    PUBLIC
        Mode: Integer;
        MonitorStream: TMemoryStream;
        SampleCount: Integer;  // This is the number of samples taken
        Header: TStringList;
        RecordSize: Integer;
        FileVersion: Integer;

        constructor Create(ParClass: TDSSClass; const MonitorName: String);
        destructor Destroy; OVERRIDE;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model, reset nphases
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
        procedure DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;
       //Property  MonitorFileName:String read BufferFile;

        property CSVFileName: String READ Get_FileName;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    CktElement,
    Transformer,
    AutoTrans,
    PCElement,
    Sysutils,
    ucmatrix,
    showresults,
    mathUtil,
    Dynamics,
    PstCalc,
    Capacitor,
    Storage,
    Storage2,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TMonitorObj;
    TProp = TMonitorProp;
{$PUSH}
{$Z4} // keep enums as int32 values
    TMonitorAction = (
        Clear = 0,
        Save = 1,
        Take = 2,
        Process = 3
    );
{$POP}
const
    NumPropsThisClass = Ord(High(TProp));

    SEQUENCEMASK = 16;
    MAGNITUDEMASK = 32;
    POSSEQONLYMASK = 64;
    MODEMASK = 15;

    NumSolutionVars = 12;

var
    EMPTY_LEGACY_HEADER: TLegacyMonitorStrBuffer;
    PropInfo: Pointer;    
    ActionEnum: TDSSEnum;

constructor TDSSMonitor.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        ActionEnum := TDSSEnum.Create('Monitor: Action', True, 1, 1, 
            ['Clear', 'Save', 'Take', 'Process', 'Reset'], 
            [ord(TMonitorAction.Clear), ord(TMonitorAction.Save), ord(TMonitorAction.Take), ord(TMonitorAction.Process), ord(TMonitorAction.Clear)]);
    end;

    inherited Create(dssContext, MON_ELEMENT, 'Monitor');
end;

destructor TDSSMonitor.Destroy;
begin
    inherited Destroy;
end;

procedure DoAction(obj: TObj; action: TMonitorAction);
begin
    case action of 
        TMonitorAction.Save:
            Obj.Save;
        TMonitorAction.Clear:
            Obj.ResetIt;
        TMonitorAction.Take:
            Obj.TakeSample;
        TMonitorAction.Process:
        begin
            Obj.PostProcess;
            dec(Obj.recalc)
        end
    end;
end;

procedure TDSSMonitor.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // boolean properties
    PropertyType[ord(TProp.residual)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.VIpolar)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.Ppolar)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.residual)] := ptruint(@obj.IncludeResidual);
    PropertyOffset[ord(TProp.VIpolar)] := ptruint(@obj.VIpolar);
    PropertyOffset[ord(TProp.Ppolar)] := ptruint(@obj.Ppolar);

    // integer properties
    PropertyType[ord(TProp.terminal)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.mode)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.terminal)] := ptruint(@obj.MeteredTerminal);
    PropertyOffset[ord(TProp.mode)] := ptruint(@obj.Mode);

    // object reference
    PropertyType[ord(TProp.element)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.element)] := ptruint(@obj.MeteredElement);
    PropertyOffset2[ord(TProp.element)] := 0;
    //PropertyFlags[ord(TProp.element)] := [TPropertyFlag.CheckForVar]; // not required for general cktelements

    // enum action
    PropertyType[ord(TProp.Action)] := TPropertyType.StringEnumActionProperty;
    PropertyOffset[ord(TProp.Action)] := ptruint(@DoAction); 
    PropertyOffset2[ord(TProp.Action)] := PtrInt(ActionEnum); 

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TDSSMonitor.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

function TDSSMonitor.BeginEdit(ptr: Pointer; SetActive_: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj(inherited BeginEdit(ptr, SetActive_));
    Obj.recalc := 0;
    Result := Obj;
end;

function TDSSMonitor.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
begin
    with TObj(ptr) do
    begin
        if (NumChanges + recalc) > 0 then
            RecalcElementData;
        Exclude(Flags, Flg.EditionActive);
    end;
    Result := True;
end;

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

procedure TMonitorObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    FNPhases := Other.Fnphases;
    NConds := Other.Fnconds; // Force Reallocation of terminal stuff

    Buffersize := Other.Buffersize;
    MeteredElement := Other.MeteredElement;  // Pointer to target circuit element
    MeteredTerminal := Other.MeteredTerminal;
    Mode := Other.Mode;
    IncludeResidual := Other.IncludeResidual;

    BaseFrequency := Other.BaseFrequency;
end;

constructor TMonitorObj.Create(ParClass: TDSSClass; const MonitorName: String);
begin
    inherited Create(ParClass);
    Name := LowerCase(MonitorName);

    FNphases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors
                         // in base class

    // Current Buffer has to be big enough to hold all terminals
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

    MeteredElement := TDSSCktElement(ActiveCircuit.CktElements.Get(1)); // Default to first circuit element (source)
    Bufferfile := '';

    MonitorStream := TMemoryStream.Create; // Create memory stream
    Header := TStringList.Create;
    RecordSize := 0;

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
end;

destructor TMonitorObj.Destroy;
begin
    MonitorStream.Free;
    Header.Free;
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

procedure TMonitorObj.RecalcElementData;
begin
    ValidMonitor := FALSE;
    if MeteredElement <> NIL then
    begin // Monitored element must already exist
        case (Mode and MODEMASK) of
            2, 8, 10:
            begin  // Must be transformer
                if (MeteredElement.DSSObjType and CLASSMASK) <> XFMR_ELEMENT then
                    if (MeteredElement.DSSObjType and CLASSMASK) <> AUTOTRANS_ELEMENT then
                    begin
                        DoSimpleMsg('%s is not a transformer!', [MeteredElement.Name], 663);
                        Exit;
                    end;
            end;
            3:
            begin // Must be PCElement
                if (MeteredElement.DSSObjType and BASECLASSMASK) <> PC_ELEMENT then
                begin
                    DoSimpleMsg('%s must be a power conversion element (Load or Generator)!', [MeteredElement.Name], 664);
                    Exit;
                end;
            end;
            6:
            begin // Checking Caps Tap
                if (MeteredElement.DSSObjType and CLASSMASK) <> CAP_ELEMENT then
                begin
                    DoSimpleMsg('%s is not a capacitor!', [MeteredElement.Name], 2016001);
                    Exit;
                end;
            end;

            7:
            begin // Checking if the element is a storage device
                if ((MeteredElement.DSSObjType and CLASSMASK) <> STORAGE_ELEMENT) then
                begin
                    DoSimpleMsg('%s is not a storage device!', [MeteredElement.Name], 2016002);
                    Exit;
                end;
            end;
        end;

        if MeteredTerminal > MeteredElement.Nterms then
        begin
            DoErrorMsg(
                Format(_('Monitor: "%s"'), [Name]),
                Format(_('Terminal no. "%d" does not exist.'), [MeteredTerminal]),
                _('Respecify terminal no.'), 665);
        end
        else
        begin
            FNphases := MeteredElement.NPhases;
            Nconds := MeteredElement.NConds;

            // Sets name of i-th terminal's connected bus in monitor's buslist
            // This value will be used to set the NodeRef array (see TakeSample)
            Setbus(1, MeteredElement.GetBus(MeteredTerminal));
            // Make a name for the Buffer File
            BufferFile := {ActiveCircuit.CurrentDirectory + }
                DSS.CircuitName_ + 'Mon_' + Name + '.mon';

            // Allocate Buffers
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
                            NumTransformerCurrents := 2 * NumWindings * nphases
                    else
                        with  TTransfObj(MeteredElement) do
                            NumTransformerCurrents := 2 * NumWindings * nphases;
                    ReallocMem(WdgCurrentsBuffer, Sizeof(Complex) * NumTransformerCurrents);
                end;
                10:
                begin
                    if (MeteredElement.DSSObjType and CLASSMASK) = AUTOTRANS_ELEMENT then
                        with  TAutoTransObj(MeteredElement) do
                            NumWindingVoltages := NumWindings * nphases
                    else
                        with  TTransfObj(MeteredElement) do
                            NumWindingVoltages := NumWindings * nphases;
                    ReallocMem(WdgVoltagesBuffer, Sizeof(Complex) * NumWindingVoltages);   // total all phases, all windings
                    ReallocMem(PhsVoltagesBuffer, Sizeof(Complex) * nphases);
                end;
                11:
                begin
                    ReallocMem(CurrentBuffer, SizeOf(CurrentBuffer^[1])*MeteredElement.Yorder);
                    ReallocMem(VoltageBuffer, SizeOf(VoltageBuffer^[1])*MeteredElement.Yorder);
                end;
                12: 
                begin
                    ReallocMem(CurrentBuffer, SizeOf(CurrentBuffer^[1])*MeteredElement.Yorder);
                    ReallocMem(VoltageBuffer, SizeOf(VoltageBuffer^[1])*(MeteredElement.Yorder + 1));
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
        // element not found/set
        DoErrorMsg(Format(_('Monitor: "%s"'), [Self.Name]), 
            _('Circuit Element is not set.'),
            _('Element must be defined previously.'), 666);
    end;
end;

procedure TMonitorObj.MakePosSequence();
begin
    if MeteredElement <> NIL then
    begin
        Setbus(1, MeteredElement.GetBus(MeteredTerminal));
        FNphases := MeteredElement.NPhases;
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

procedure TMonitorObj.CalcYPrim;
begin
  // A Monitor is a zero current source; Yprim is always zero.

  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
end;

procedure TMonitorObj.ClearMonitorStream;
var
    PhaseLoc: Array of Integer;
    i, j: Integer;
    iMax: Integer;
    iMin: Integer;
    IsPosSeq: Boolean;
    IsPower: Boolean;
    NumVI: Integer;
begin
    try
        MonitorStream.Clear;
        Header.Clear;
        IsProcessed := FALSE;
        SampleCount := 0;
        IsPosSeq := FALSE;
        
        if ActiveCircuit.Solution.IsHarmonicModel then
        begin
            Header.Add('Freq');
            Header.Add('Harmonic');
        end
        else
        begin
            Header.Add('hour');
            Header.Add('t(sec)');
        end;

        case (Mode and MODEMASK) of

            2:
            begin
                RecordSize := 1; // Transformer Taps
                Header.Add('Tap (pu)');
            end;
            3:
            begin
                RecordSize := NumStateVars; // Statevariabes
                for i := 1 to NumStateVars do
                    Header.Add(TpcElement(MeteredElement).VariableName(i));
            end;
            4:
            begin
                RecordSize := 2 * FnPhases;
                for i := 1 to FnPhases do
                begin
                    Header.Add('Flk' + IntToStr(i));
                    Header.Add('Pst' + IntToStr(i));
                end;
            end;
            5:
            begin
                RecordSize := NumSolutionVars;
                Header.Add('TotalIterations');
                Header.Add('ControlIteration');
                Header.Add('MaxIterations');
                Header.Add('MaxControlIterations');
                Header.Add('Converged');
                Header.Add('IntervalHrs');
                Header.Add('SolutionCount');
                Header.Add('Mode');
                Header.Add('Frequency');
                Header.Add('Year');
                Header.Add('SolveSnap_uSecs');
                Header.Add('TimeStep_uSecs');
            end;
            6:
            begin
                RecordSize := TCapacitorObj(MeteredElement).NumSteps;     // Capacitor Taps
                for i := 1 to RecordSize do
                    Header.Add('Step_' + inttostr(i));
            end;
            7:
            begin
                RecordSize := 5;     // Storage state vars
                Header.Add('kW output');
                Header.Add('kvar output');
                Header.Add('kW Stored');
                Header.Add('%kW Stored');
                Header.Add('State');
            end;
            8:
            begin   // All winding Currents
                if (MeteredElement.DSSObjType and CLASSMASK) = AUTOTRANS_ELEMENT then
                    with TAutoTransObj(MeteredElement) do
                    begin
                        RecordSize := NumTransformerCurrents;     // Transformer Winding Currents
                        for i := 1 to Nphases do
                            for j := 1 to NumWindings do
                            begin
                                Header.Add(Format('P%dW%d', [i, j]));
                                Header.Add('Deg');
                            end;
                    end
                else
                    with TTransfObj(MeteredElement) do
                    begin
                        RecordSize := NumTransformerCurrents;     // Transformer Winding Currents
                        for i := 1 to Nphases do
                            for j := 1 to NumWindings do
                            begin
                                Header.Add(Format('P%dW%d', [i, j]));
                                Header.Add('Deg');
                            end;
                    end;
            end;
            9:
            begin // watts vars of meteredElement
                RecordSize := 2;
                Header.Add('watts');
                Header.Add('vars');
            end;
            10:
            begin // All Winding Voltages
                if (MeteredElement.DSSObjType and CLASSMASK) = AUTOTRANS_ELEMENT then
                    with TAutoTransObj(MeteredElement) do
                    begin
                        RecordSize := 2 * NumWindings * Nphases;     // Transformer Winding woltages
                        for i := 1 to Nphases do
                            for j := 1 to NumWindings do
                            begin
                                Header.Add(Format('P%dW%d', [i, j]));
                                Header.Add('Deg');
                            end;
                    end
                else
                    with TTransfObj(MeteredElement) do
                    begin
                        RecordSize := 2 * NumWindings * Nphases;     // Transformer Winding woltages
                        for i := 1 to Nphases do
                            for j := 1 to NumWindings do
                            begin
                                Header.Add(Format('P%dW%d', [i, j]));
                                Header.Add('Deg');
                            end;
                    end;
            end;
            11: {All terminal voltages and currents  *****}
            begin 
                Recordsize := 2 * 2 * MeteredElement.Yorder;  // V and I

                // Voltages
                for j := 1 to MeteredElement.NTerms do
                    for i := 1 to MeteredElement.NConds do
                    begin
                        Header.Add(Format('V%dT%d', [i, j]));
                        Header.Add('Deg');
                    end;

                // Currents
                for j := 1 to MeteredElement.NTerms do
                    for i := 1 to MeteredElement.NConds do
                    begin
                        Header.Add(Format('I%dT%d', [i, j]));
                        Header.Add('Deg');
                    end;
            end;
            12: {All terminal voltages LL and currents  *****}
            begin 
                with MeteredElement do
                begin
                    Recordsize := 2 * ((NPhases * NTerms) + Yorder);  // V and I
                    SetLength(PhaseLoc, NPhases + 1);
                end;

                // Creates the map of phase combinations (LL)
                for j := 1 to MeteredElement.NPhases do
                    PhaseLoc[j - 1] := j;

                PhaseLoc[High(PhaseLoc)] := 1;

                // Voltages
                for j := 1 to MeteredElement.NTerms do
                    for i := 1 to MeteredElement.NPhases do
                    begin
                        Header.Add(Format('V%d-%dT%d', [PhaseLoc[i-1], PhaseLoc[i], j]));
                        Header.Add('Deg');
                    end;

                // Currents
                for j := 1 to MeteredElement.NTerms do
                    for i := 1 to MeteredElement.NConds do
                    begin
                        Header.Add(Format('I%dT%d', [i, j]));
                        Header.Add('Deg');
                    end;
            end
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
                            Header.Add(Format('|V|%d (volts)', [i]));

                        if IncludeResidual then
                            Header.Add('|VN| (volts)');

                        for i := 1 to NumVI do
                            Header.Add('|I|' + IntToStr(i) + ' (amps)');

                        if IncludeResidual then
                            Header.Add('|IN| (amps)');
                    end
                    else
                    begin  // Power
                        for i := 1 to NumVI do
                        begin
                            if PPolar then
                                Header.Add('S' + IntToStr(i) + ' (kVA)')
                            else
                                Header.Add('P' + IntToStr(i) + ' (kW)');
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
                        begin
                            Header.Add('V1');
                            Header.Add('V1ang');
                            Header.Add('I1');
                            Header.Add('I1ang');
                        end
                        else
                        begin
                            Header.Add('V1.re');
                            Header.Add('V1.im');
                            Header.Add('I1.re');
                            Header.Add('I1.im');
                        end;
                    end
                    else
                    begin
                        if Ppolar then
                        begin
                            Header.Add('S1 (kVA)');
                            Header.Add('Ang');
                        end
                        else
                        begin
                            Header.Add('P1 (kW)');
                            Header.Add('Q1 (kvar)');
                        end;
                    end;
                end;
                96:
                begin  // Save Pos Seq or Aver magnitude of all Phases of total kVA (Magnitude)
                    RecordSize := 1;
                    if not IsPower then
                    begin
                        RecordSize := RecordSize + 1;
                        Header.Add('V');
                        Header.Add('I');
                    end
                    else
                    begin  // Power
                        if Ppolar then
                            Header.Add('S1 (kVA)')
                        else
                            Header.Add('P1 (kW)');
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
                        begin
                            Header.Add('V' + IntToStr(i));
                            Header.Add('VAngle' + IntToStr(i));
                        end
                        else
                        begin
                            Header.Add('V' + IntToStr(i) + '.re');
                            Header.Add('V' + IntToStr(i) + '.im');
                        end;
                    end;
                    if IncludeResidual then
                    begin
                        if VIPolar then
                        begin
                            Header.Add('VN');
                            Header.Add('VNAngle');
                        end
                        else
                        begin
                            Header.Add('VN.re');
                            Header.Add('VN.im');
                        end;
                    end;
                    for i := iMin to iMax do
                    begin
                        if VIPolar then
                        begin
                            Header.Add('I' + IntToStr(i));
                            Header.Add('IAngle' + IntToStr(i));
                        end
                        else
                        begin
                            Header.Add('I' + IntToStr(i) + '.re');
                            Header.Add('I' + IntToStr(i) + '.im');
                        end;
                    end;
                    if IncludeResidual then
                    begin
                        if VIPolar then
                        begin
                            Header.Add('IN');
                            Header.Add('INAngle');
                        end
                        else
                        begin
                            Header.Add('IN.re');
                            Header.Add('IN.im');
                        end;
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
                        begin
                            Header.Add('S' + IntToStr(i) + ' (kVA)');
                            Header.Add('Ang' + IntToStr(i));
                        end
                        else
                        begin
                            Header.Add('P' + IntToStr(i) + ' (kW)');
                            Header.Add('Q' + IntToStr(i) + ' (kvar)');
                        end;
                    end;
                end;
            end;
        end;
        end; // CASE


        // RecordSize is the number of singles in the sample (after the hour and sec)

        // Write ID so we know it is a DSS Monitor file and which version in case we
        // change it down the road

        with MonitorStream do
        begin
            Write(FileSignature, Sizeof(FileSignature));
            Write(FileVersion, Sizeof(FileVersion));
            Write(RecordSize, Sizeof(RecordSize));
            Write(Mode, Sizeof(Mode));
            // adds the empty dummy record to avoid
            // killing apps relying on this space
            Write(EMPTY_LEGACY_HEADER, Sizeof(TLegacyMonitorStrBuffer)); 
        end;

        // So the file now looks like: (update 05-18-2021)
        //   FileSignature (4 bytes)    32-bit Integers
        //   FileVersion   (4)
        //   RecordSize    (4)
        //   Mode          (4)
        //   String        (256) - > this is empty now
        //  
        //   hr   (4)       all singles
        //   Sec  (4)
        //   Sample  (4*RecordSize)
        //   ...

    except
        On E: Exception do
            DoErrorMsg(_('Cannot open Monitor file.'),
                E.Message,
                Format(_('Monitor: "%s"'), [Name]), 670)

    end;
end;

procedure TMonitorObj.OpenMonitorStream;
begin
    if not IsFileOpen then
    begin
        MonitorStream.Seek(0, soFromEnd);    // Positioned at End of Stream
        IsFileOpen := TRUE;
    end;
end;

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
            DoErrorMsg(_('Cannot close Monitor stream.'),
                E.Message,
                Format(_('Monitor: "%s"'), [Name]), 671)
    end;
end;

procedure TMonitorObj.Save;
// Saves present buffer to monitor file, resets bufferptrs and continues
begin
    if not IsFileOpen then
        OpenMonitorStream; // Position to end of stream

     {Write present monitor buffer to monitorstream}
    MonitorStream.Write(MonBuffer^, SizeOF(MonBuffer^[1]) * BufPtr);

    BufPtr := 0; // reset Buffer for next
end;

procedure TMonitorObj.ResetIt;
begin
    BufPtr := 0;
    ClearMonitorStream;
end;

procedure TMonitorObj.PostProcess;
begin
    if IsProcessed = FALSE then
    begin
        if (mode = 4) and (MonitorStream.Position > 0) then
            DoFlickerCalculations;
    end;
    IsProcessed := TRUE;
end;

procedure TMonitorObj.TakeSample;
var
    dHour: Double;
    dSum: Double;
    IsPower: Boolean;
    IsSequence: Boolean;
    BuffInit, 
    BuffEnd,
    i, j, k,
    myRefIdx,
    NumVI: Integer;
    Offset: Integer;
    ResidualCurr: Complex;
    ResidualVolt: Complex;
    Sum: Complex;
    CplxLosses: Complex;
    V012, I012: Complex3;


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
            AddDblsToBuffer(pDoubleArray(@Frequency), 1);  // put freq in hour slot as a double
            AddDblsToBuffer(pDoubleArray(@Harmonic), 1);  // stick harmonic in time slot in buffer
        end
        else
        begin
            dHour := Hour;      // convert to double
            AddDblsToBuffer(pDoubleArray(@dHour), 1);  // put hours in buffer as a double
            AddDblsToBuffer(pDoubleArray(@Sec), 1);  // stick time in sec in buffer
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
                    DoSimpleMsg(E.Message + CRLF + _('NodeRef is invalid. Try solving a snapshot or direct before solving in a mode that takes a monitor sample.'), 672);
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
                    DoSimpleMsg(E.Message + CRLF + _('NodeRef is invalid. Try solving a snapshot or direct before solving in a mode that takes a monitor sample.'), 672);
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
            if ((MeteredElement.DSSObjType and CLASSMASK) = STORAGE_ELEMENT) and DSS_CAPI_LEGACY_MODELS then
            begin  // Storage Element
                with TStorageObj(MeteredElement) do
                begin
                    AddDblToBuffer(PresentkW);
                    AddDblToBuffer(Presentkvar);
                    AddDblToBuffer(StorageVars.kWhStored);
                    AddDblToBuffer(((StorageVars.kWhStored) / (StorageVars.kWhRating)) * 100);
                    AddDblToBuffer(StorageState);
                end;
            end
            else
            if ((MeteredElement.DSSObjType and CLASSMASK) = STORAGE_ELEMENT) and not DSS_CAPI_LEGACY_MODELS then
            begin   // Storage2 Element
                with TStorage2Obj(MeteredElement) do
                begin
                    AddDblToBuffer(PresentkW);
                    AddDblToBuffer(Presentkvar);
                    AddDblToBuffer(StorageVars.kWhStored);
                    AddDblToBuffer(((StorageVars.kWhStored) / (StorageVars.kWhRating)) * 100);
                    AddDblToBuffer(StorageState);
                end;
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
                    for i := 1 to Nphases * NumWindings do
                    begin
                        AddDblsToBuffer(pDoubleArray(@WdgCurrentsBuffer^[k].re), 2);  // Add Mag, Angle
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
                    for i := 1 to Nphases * NumWindings do
                    begin
                        AddDblsToBuffer(pDoubleArray(@WdgCurrentsBuffer^[k].re), 2);  // Add Mag, Angle
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
                    for i := 1 to NumWindings do
                    begin
                        GetAutoWindingVoltages(i, PhsVoltagesBuffer);
                        for j := 1 to nphases do
                            WdgVoltagesBuffer^[i + (j - 1) * NumWindings] := PhsVoltagesBuffer^[j];
                    end;
                    ConvertComplexArrayToPolar(WdgVoltagesBuffer, NumWindingVoltages);
                  {Put winding Voltages into Monitor}
                    AddDblsToBuffer(pDoubleArray(@WdgVoltagesBuffer^[1].re), 2 * NumWindingVoltages);  // Add Mag, Angle each winding
                end

            else
                with TTransfobj(MeteredElement) do
                begin
                    for i := 1 to NumWindings do
                    begin
                        GetWindingVoltages(i, PhsVoltagesBuffer);
                        for j := 1 to nphases do
                            WdgVoltagesBuffer^[i + (j - 1) * NumWindings] := PhsVoltagesBuffer^[j];
                    end;
                    ConvertComplexArrayToPolar(WdgVoltagesBuffer, NumWindingVoltages);
                  {Put winding Voltages into Monitor}
                    AddDblsToBuffer(pDoubleArray(@WdgVoltagesBuffer^[1].re), 2 * NumWindingVoltages);  // Add Mag, Angle each winding
                end;
            Exit;
        end;
        
        11: 
        begin    {Get all terminal voltages and currents of this device}
            {Get All node voltages at all terminals}
            MeteredElement.ComputeVterminal();
            For i := 1 to MeteredElement.Yorder do 
                VoltageBuffer^[i] := MeteredElement.Vterminal^[i];

            ConvertComplexArrayToPolar( VoltageBuffer, MeteredElement.Yorder);
            {Put Terminal Voltages into Monitor}
            AddDblsToBuffer(pDoubleArray(@VoltageBuffer^[1].re), 2 * MeteredElement.Yorder);

            {Get all terminsl currents}
            MeteredElement.ComputeIterminal();   // only does calc if needed
            for i := 1 to MeteredElement.Yorder do 
                CurrentBuffer^[i] := MeteredElement.Iterminal^[i];

            ConvertComplexArrayToPolar( CurrentBuffer, MeteredElement.Yorder);
            {Put Terminal currents into Monitor}
            AddDblsToBuffer(pDoubleArray(@CurrentBuffer^[1].re), 2 * MeteredElement.Yorder);
            Exit;
        end;
        12: 
        begin // Get all terminal voltages LL and currents of this device - 05192021
            with MeteredElement do
            begin
                // Get All node voltages at all terminals
                ComputeVterminal();

                for k := 1 to NTerms do // Adds each term separately
                begin
                    BuffInit := 1 + NPhases * (k - 1);
                    BuffEnd := NPhases * k;
                    for i := BuffInit to BuffEnd do
                        VoltageBuffer^[i - (BuffInit - 1)] := Vterminal^[i];

                    if NPhases = NConds then
                        myRefIdx := NPhases + 1
                    else
                        myRefIdx := NConds;

                    //Brings the first phase to the last place for calculations
                    VoltageBuffer^[myRefIdx] := VoltageBuffer^[1];
                    
                    // Calculates the LL voltages
                    for i := 1 to NPhases do
                        VoltageBuffer^[i] := VoltageBuffer^[i] - VoltageBuffer^[i + 1];
                    
                    ConvertComplexArrayToPolar(VoltageBuffer, Yorder);
                    
                    // Put Terminal Voltages into Monitor
                    AddDblsToBuffer(pDoubleArray(@VoltageBuffer^[1].re), 2 * NPhases);
                end;

                // Get all terminsl currents
                ComputeIterminal();   // only does calc if needed
                
                for i := 1 to Yorder do 
                    CurrentBuffer^[i] := Iterminal^[i];
                
                ConvertComplexArrayToPolar(CurrentBuffer, Yorder);
                
                // Put Terminal currents into Monitor
                AddDblsToBuffer(pDoubleArray(@CurrentBuffer^[1].re), 2 * Yorder);
                
                Exit;
            end;
        end
    else
        Exit  // Ignore invalid mask
    end;


    if ((Mode and SEQUENCEMASK) > 0) and (Fnphases = 3) then
    begin  // Convert to Symmetrical components
        Phase2SymComp(PComplex3(VoltageBuffer), @V012);
        Phase2SymComp(PComplex3(@CurrentBuffer^[Offset + 1]), @I012);
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
                ConvertComplexArrayToPolar(PComplexArray(@CurrentBuffer^[Offset + 1]), NumVI);    // Corrected 3-11-13
            end;
        end;
        1:
        begin     // Convert Voltage Buffer to power kW, kvar or Mag/Angle
            CalckPowers(VoltageBuffer, VoltageBuffer, PComplexArray(@CurrentBuffer^[Offset + 1]), NumVI);
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
                AddDblsToBuffer(pDoubleArray(@VoltageBuffer^[2].re), 2);
                if not IsPower then
                    AddDblsToBuffer(pDoubleArray(@CurrentBuffer^[Offset + 2].re), 2);
            end
            else
            begin
                if IsPower then
                begin
                    Sum := cZero;
                    for i := 1 to Fnphases do
                        Sum += VoltageBuffer^[i];
                    AddDblsToBuffer(pDoubleArray(@Sum.re), 2);
                end
                else
                begin  // Average the phase magnitudes and  sum angles
                    Sum := cZero;
                    for i := 1 to Fnphases do
                        Sum += VoltageBuffer^[i];
                    Sum.re := Sum.re / FnPhases;
                    AddDblsToBuffer(pDoubleArray(@Sum.re), 2);
                    Sum := cZero;
                    for i := 1 to Fnphases do
                        Sum += CurrentBuffer^[Offset + i];   // Corrected 3-11-13
                    Sum.re := Sum.re / FnPhases;
                    AddDblsToBuffer(pDoubleArray(@Sum.re), 2);
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
                AddDblsToBuffer(pDoubleArray(@FlickerBuffer^[1].re), Fnphases * 2);
            5:
                AddDblsToBuffer(pDoubleArray(@SolutionBuffer^[1]), NumSolutionVars);
        else
        begin
            AddDblsToBuffer(pDoubleArray(@VoltageBuffer^[1].re), NumVI * 2);
            if not IsPower then
            begin
                if IncludeResidual then
                    AddDblsToBuffer(pDoubleArray(@ResidualVolt), 2);
                AddDblsToBuffer(pDoubleArray(@CurrentBuffer^[Offset + 1].re), NumVI * 2);
                if IncludeResidual then
                    AddDblsToBuffer(pDoubleArray(@ResidualCurr), 2);
            end;
        end;
        end;
    end;
end;

procedure TMonitorObj.AddDblsToBuffer(Dbl: pDoubleArray; Ndoubles: Integer);

var
    i: Integer;

begin
    for i := 1 to Ndoubles do
        AddDblToBuffer(Dbl^[i]);
end;

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
        Seek(SizeOf(TLegacyMonitorStrBuffer), soFromCurrent);
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

procedure TMonitorObj.TranslateToCSV(Show: Boolean);
var
    CSVName: String;
    F: TFileStream = nil;
    FSignature: Integer;
    Fversion: Integer;
    hr: Single;
    i: Cardinal;
    Mode: Integer;
    Nread: Cardinal;
    RecordBytes: Cardinal;
    RecordSize: Cardinal;
    s: Single;
    sngBuffer: array[1..100] of Single;
    sout: String;
{$IFDEF DSS_CAPI_PM}
    PMParent: TDSSContext;
begin
    PMParent := DSS.GetPrime();
{$ELSE}
begin
{$ENDIF}

    Save;  // Save present buffer
    CloseMonitorStream;   // Position at beginning

    CSVName := Get_FileName;

    try
{$IFDEF DSS_CAPI_PM}
        if PMParent.ConcatenateReports then
            // We may need to wait other threads before using the file
            PMParent.ConcatenateReportsLock.Acquire();

        if PMParent.ConcatenateReports and (PMParent <> DSS) then
        begin
            F := TFileStream.Create(CSVName, fmOpenReadWrite);
            F.Seek(0, soFromEnd);
        end
        else
{$ENDIF}
            F := TFileStream.Create(CSVName, fmCreate);

    except
        On E: Exception do
        begin
            DoSimpleMsg('Error opening CSVFile "%s" for writing: %s', [CSVName, E.Message], 672);
{$IFDEF DSS_CAPI_PM}
            if PMParent.ConcatenateReports then
                // We may need to wait other threads before using the file
                PMParent.ConcatenateReportsLock.Release();
{$ENDIF}
            Exit;
        end;
    end;

    with MonitorStream do
    begin
        Seek(0, soFromBeginning);  // Start at the beginning of the Stream
        Read(Fsignature, Sizeof(Fsignature));
        Read(Fversion, Sizeof(Fversion));
        Read(RecordSize, Sizeof(RecordSize));
        Read(Mode, Sizeof(Mode));
        Seek(SizeOf(TLegacyMonitorStrBuffer), soFromCurrent);
    end;

{$IFDEF DSS_CAPI_PM}
    if not PMParent.ConcatenateReports or (PMParent = DSS) then
{$ENDIF}
        FSWriteln(F, Header.CommaText);
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
                
                WriteStr(sout, hr: 0: 0, ', ', s: 0: 5);
                FSWrite(F, sout);
                
                for i := 1 to RecordSize do
                begin
                    FSWrite(F, Format(', %-.6g', [sngBuffer[i]]))
                end;
                FSWriteln(F);
            end;
        except
            On E: Exception do
            begin
                DoSimpleMsg('Error Writing CSVFile "%s": %s', [CSVName, E.Message], 673);
            end;
        end;

    finally
        CloseMonitorStream;
        FreeAndNil(F);
{$IFDEF DSS_CAPI_PM}
        if PMParent.ConcatenateReports then
            // We may need to wait other threads before using the file
            PMParent.ConcatenateReportsLock.Release();
{$ENDIF}
    end;

    if Show then
        FireOffEditor(DSS, CSVName);

    DSS.GlobalResult := CSVName;
end;

procedure TMonitorObj.GetCurrents(Curr: pComplexArray);  //Get present value of terminal Curr for reports
var
    i: Integer;
begin
// Revised 12-7-99 to return Zero current instead of Monitored element current because
// it was messing up Newton iteration.
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

procedure TMonitorObj.DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean);

var
    i, k: Integer;
    sout: String;
begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            FSWriteln(F, '~ ' + PropertyName^[i] + '=' + PropertyValue[i]);
        end;


    if Complete then
    begin
        FSWriteln(F);
        FSWriteln(F, '// BufferSize=', IntToStr(BufferSize));
        FSWriteln(F, '// Hour=', IntToStr(Hour));
        WriteStr(sout, '// Sec=', Sec: 0);
        FSWriteln(F, sout);
        FSWriteln(F, Format('// BaseFrequency=%.1g', [BaseFrequency]));
        FSWriteln(F, '// Bufptr=', IntToStr(BufPtr));
        FSWriteln(F, '// Buffer=');
        k := 0;
        for i := 1 to BufPtr do
        begin
            WriteStr(sout, MonBuffer^[i]: 0: 1, ', ');
            FSWrite(F, sout);
            Inc(k);
            if k = (2 + Fnconds * 4) then
            begin
                FSWriteln(F);
                k := 0;
            end;
        end;
        FSWriteln(F);
    end;
end;

function TMonitorObj.Get_FileName: String;
{$IFDEF DSS_CAPI_PM}
var
    PMParent: TDSSContext;
{$ENDIF}
begin
{$IFDEF DSS_CAPI_PM}
    PMParent := DSS.GetPrime();
    if PMParent.ConcatenateReports then
    begin
        Result := PMParent.OutputDirectory + PMParent.CircuitName_ + 'Mon_' + Name + '.csv';
        Exit;
    end;
{$ENDIF}
    Result := DSS.OutputDirectory + DSS.CircuitName_ + 'Mon_' + Name + DSS._Name + '.csv'
end;

initialization
    FillChar(EMPTY_LEGACY_HEADER, SizeOf(EMPTY_LEGACY_HEADER), 0);
    PropInfo := NIL;
finalization
    ActionEnum.Free;        
end.
