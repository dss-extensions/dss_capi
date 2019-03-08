unit RegControl;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   Change Log
   1-28-00 Created
   4-29-00 fixed problem with NumPhases = # phases of controlled element
   12/17/01 Added LDC logic
   12/18/01 Added MaxTapChange property and logic
   6/18/11 Updated Rev Power logic
   12/4/2018  Added autotransformer control
}

{
  A RegControl is a control element that is connected to a terminal of another
  circuit element that must be a transformer.

  A RegControl is defined by a New command:

  New RegControl.Name=myname Transformer = name Terminal=[1,2,...] Controlledbus=name etc...

  Transformer to be controlled must already exist.
}

interface

uses
    Command,
    ControlClass,
    ControlElem,
    DSSClass,
    Arraydef,
    ucomplex,
    Transformer,
    AutoTrans,
    utilities;

type

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TRegControl = class(TControlClass)
    PRIVATE

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const RegControlName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TRegControlObj = class(TControlElem)
    PRIVATE

        Vreg,
        Bandwidth,
        PTRatio,
        RemotePTRatio,
        CTRating,
        R,
        X,
        LDC_Z: Double;

        {Reverse Power Variables}
        revVreg,
        revBandwidth,
        RevPowerThreshold,   // W
        kWRevPowerThreshold,
        revDelay,
        revR,
        revX,
        revLDC_Z: Double;

        IsReversible: Boolean;
        InReverseMode: Boolean;
        ReversePending: Boolean;
        ReverseNeutral: Boolean;
        CogenEnabled: Boolean;
        InCogenMode: Boolean;

        RevHandle: Integer;
        RevBackHandle: Integer;

        LDCActive: Boolean;
        UsingRegulatedBus: Boolean;
        RegulatedBus: String;

        FPendingTapChange,   // amount of tap change pending
        TapDelay: Double;   // delay between taps

        DebugTrace: Boolean;
        Armed: Boolean;
        Tracefile: TextFile;

        TapLimitPerChange: Integer;
        TapWinding: Integer;  // Added 7-19-07
        FInversetime: Boolean;
        Vlimit: Double;
        VLimitActive: Boolean;

        FPTphase: Integer;
        ControlledPhase: Integer;

        ControlActionHandle: Integer;

        VBuffer, CBuffer: pComplexArray;

        function Get_Transformer: TTransfObj;
        function Get_Winding: Integer;
        // CIM accessors
        function Get_MinTap: Double;
        function Get_MaxTap: Double;
        function Get_TapIncrement: Double;
        function Get_NumTaps: Integer;
        function Get_TapNum: Integer;

        procedure RegWriteTraceRecord(TapChangeMade: Double);
        procedure RegWriteDebugRecord(S: String);
        procedure set_PendingTapChange(const Value: Double);
        function AtLeastOneTap(const ProposedChange: Double; Increment: Double): Double;
        function ComputeTimeDelay(Vavg: Double): Double;
        function GetControlVoltage(VBuffer: pComplexArray; Nphs: Integer; PTRatio: Double): Complex;
        procedure Set_TapNum(const Value: Integer);

    PUBLIC

        constructor Create(ParClass: TDSSClass; const RegControlName: String);
        destructor Destroy; OVERRIDE;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;    // Always Zero for a RegControl

        procedure Sample; OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state


        procedure GetCurrents(Curr: pComplexArray); OVERRIDE; // Get present value of terminal Curr
        procedure GetInjCurrents(Curr: pComplexArray); OVERRIDE;   // Returns Injextion currents

        procedure MakePosSequence; OVERRIDE;  // Make a positive Sequence Model
        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
        procedure SaveWrite(var F: TextFile); OVERRIDE;

        property Transformer: TTransfObj READ Get_Transformer;  // Pointer to controlled Transformer
        property TrWinding: Integer READ Get_Winding;  // Report Tapped winding

        property PendingTapChange: Double READ FPendingTapChange WRITE set_PendingTapChange;

       // CIM XML accessors
        property TargetVoltage: Double READ Vreg;
        property BandVoltage: Double READ BandWidth;
        property CT: Double READ CTRating;
        property PT: Double READ PTRatio;
        property LineDropR: Double READ R;
        property LineDropX: Double READ X;
        property RevLineDropR: Double READ revR;
        property RevLineDropX: Double READ revX;
        property RevTargetVoltage: Double READ revVreg;
        property RevBandVoltage: Double READ revBandWidth;
        property UseLineDrop: Boolean READ LDCActive;
        property UseReverseDrop: Boolean READ IsReversible;
        property UseLimit: Boolean READ VLimitActive;
        property VoltageLimit: Double READ VLimit;
        property InitialDelay: Double READ TimeDelay;
        property SubsequentDelay: Double READ TapDelay;
        property MinTap: Double READ Get_MinTap;
        property MaxTap: Double READ Get_MaxTap;
        property TapIncrement: Double READ Get_TapIncrement;
        property NumTaps: Integer READ Get_NumTaps;
        property MaxTapChange: Integer READ TapLimitPerChange;
        property IsInverseTime: Boolean READ FInverseTime;
        property TapNum: Integer READ Get_TapNum WRITE Set_TapNum;
    end;


var
    ActiveRegControlObj: TRegControlObj;

{--------------------------------------------------------------------------}
implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    CktElement,
    Sysutils,
    uCmatrix,
    MathUtil,
    Math;

const
    AVGPHASES = -1;
    MAXPHASE = -2;
    MINPHASE = -3;

    ACTION_TAPCHANGE = 0;
    ACTION_REVERSE = 1;

    NumPropsThisClass = 32;

var
    LastChange: Integer;

{--------------------------------------------------------------------------}
constructor TRegControl.Create;  // Creates superstructure for all RegControl objects
begin
    inherited Create;

    Class_name := 'RegControl';
    DSSClassType := DSSClassType + REG_CONTROL;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

{--------------------------------------------------------------------------}
destructor TRegControl.Destroy;

begin
    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TRegControl.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


     // Define Property names

    PropertyName[1] := 'transformer';
    PropertyName[2] := 'winding';
    PropertyName[3] := 'vreg';
    PropertyName[4] := 'band';
    PropertyName[5] := 'ptratio';
    PropertyName[6] := 'CTprim';
    PropertyName[7] := 'R';
    PropertyName[8] := 'X';
    PropertyName[9] := 'bus';
    PropertyName[10] := 'delay';
    PropertyName[11] := 'reversible';
    PropertyName[12] := 'revvreg';
    PropertyName[13] := 'revband';
    PropertyName[14] := 'revR';
    PropertyName[15] := 'revX';
    PropertyName[16] := 'tapdelay';
    PropertyName[17] := 'debugtrace';
    PropertyName[18] := 'maxtapchange';
    PropertyName[19] := 'inversetime';
    PropertyName[20] := 'tapwinding';
    PropertyName[21] := 'vlimit';
    PropertyName[22] := 'PTphase';
    PropertyName[23] := 'revThreshold';
    PropertyName[24] := 'revDelay';
    PropertyName[25] := 'revNeutral';
    PropertyName[26] := 'EventLog';
    PropertyName[27] := 'RemotePTRatio';
    PropertyName[28] := 'TapNum';
    PropertyName[29] := 'Reset';
    PropertyName[30] := 'LDC_Z';
    PropertyName[31] := 'rev_Z';
    PropertyName[32] := 'Cogen';

    PropertyHelp[1] := 'Name of Transformer or AutoTrans element to which the RegControl is connected. ' +
        'Do not specify the full object name; "Transformer" or "AutoTrans" is assumed for ' +
        'the object class.  Example:' + CRLF + CRLF +
        'Transformer=Xfmr1';
    PropertyHelp[2] := 'Number of the winding of the transformer element that the RegControl is monitoring. ' +
        '1 or 2, typically.  Side Effect: Sets TAPWINDING property to the same winding.';
    PropertyHelp[3] := 'Voltage regulator setting, in VOLTS, for the winding being controlled.  Multiplying this ' +
        'value times the ptratio should yield the voltage across the WINDING of the controlled transformer.' +
        ' Default is 120.0';
    PropertyHelp[4] := 'Bandwidth in VOLTS for the controlled bus (see help for ptratio property).  Default is 3.0';
    PropertyHelp[5] := 'Ratio of the PT that converts the controlled winding voltage to the regulator control voltage. ' +
        'Default is 60.  If the winding is Wye, the line-to-neutral voltage is used.  Else, the line-to-line ' +
        'voltage is used. SIDE EFFECT: Also sets RemotePTRatio property.';
    PropertyHelp[6] := 'Rating, in Amperes, of the primary CT rating for which the line amps convert to control rated amps.' +
        'The typical default secondary ampere rating is 0.2 Amps (check with manufacturer specs). ' +
        'Current at which the LDC voltages match the R and X settings.';
    PropertyHelp[7] := 'R setting on the line drop compensator in the regulator, expressed in VOLTS.';
    PropertyHelp[8] := 'X setting on the line drop compensator in the regulator, expressed in VOLTS.';
    PropertyHelp[9] := 'Name of a bus (busname.nodename) in the system to use as the controlled bus instead of the bus to which the ' +
        'transformer winding is connected or the R and X line drop compensator settings.  Do not specify this ' +
        'value if you wish to use the line drop compensator settings.  Default is null string. Assumes the base voltage for this ' +
        'bus is the same as the transformer winding base specified above. ' +
        'Note: This bus (1-phase) WILL BE CREATED by the regulator control upon SOLVE if not defined by some other device. ' +
        'You can specify the node of the bus you wish to sample (defaults to 1). ' +
        'If specified, the RegControl is redefined as a 1-phase device since only one voltage is used.';
    PropertyHelp[10] := 'Time delay, in seconds, from when the voltage goes out of band to when the tap changing begins. ' +
        'This is used to determine which regulator control will act first. Default is 15.  You may specify any ' +
        'floating point number to achieve a model of whatever condition is necessary.';
    PropertyHelp[11] := '{Yes |No*} Indicates whether or not the regulator can be switched to regulate in the reverse direction. Default is No.' +
        'Typically applies only to line regulators and not to LTC on a substation transformer.';
    PropertyHelp[12] := 'Voltage setting in volts for operation in the reverse direction.';
    PropertyHelp[13] := 'Bandwidth for operating in the reverse direction.';
    PropertyHelp[14] := 'R line drop compensator setting for reverse direction.';
    PropertyHelp[15] := 'X line drop compensator setting for reverse direction.';
    PropertyHelp[16] := 'Delay in sec between tap changes. Default is 2. This is how long it takes between changes ' +
        'after the first change.';
    PropertyHelp[17] := '{Yes | No* }  Default is no.  Turn this on to capture the progress of the regulator model ' +
        'for each control iteration.  Creates a separate file for each RegControl named "REG_name.CSV".';
    PropertyHelp[18] := 'Maximum allowable tap change per control iteration in STATIC control mode.  Default is 16. ' + CRLF + CRLF +
        'Set this to 1 to better approximate actual control action. ' + CRLF + CRLF +
        'Set this to 0 to fix the tap in the current position.';
    PropertyHelp[19] := '{Yes | No* } Default is no.  The time delay is adjusted inversely proportional to the amount the voltage is outside the band down to 10%.';
    PropertyHelp[20] := 'Winding containing the actual taps, if different than the WINDING property. Defaults to the same winding as specified by the WINDING property.';
    PropertyHelp[21] := 'Voltage Limit for bus to which regulated winding is connected (e.g. first customer). Default is 0.0. ' +
        'Set to a value greater then zero to activate this function.';
    PropertyHelp[22] := 'For multi-phase transformers, the number of the phase being monitored or one of { MAX | MIN} for all phases. Default=1. ' +
        'Must be less than or equal to the number of phases. Ignored for regulated bus.';
    PropertyHelp[23] := 'kW reverse power threshold for reversing the direction of the regulator. Default is 100.0 kw.';
    PropertyHelp[24] := 'Time Delay in seconds (s) for executing the reversing action once the threshold for reversing has been exceeded. Default is 60 s.';
    PropertyHelp[25] := '{Yes | No*} Default is no. Set this to Yes if you want the regulator to go to neutral in the reverse direction or in cogen operation.';
    PropertyHelp[26] := '{Yes/True* | No/False} Default is YES for regulator control. Log control actions to Eventlog.';
    PropertyHelp[27] := 'When regulating a bus (the Bus= property is set), the PT ratio required to convert actual voltage at the remote bus to control voltage. ' +
        'Is initialized to PTratio property. Set this property after setting PTratio.';
    PropertyHelp[28] := 'An integer number indicating the tap position that the controlled transformer winding tap position is currently at, or is being set to.  If being set, and the value is outside the range of the transformer min or max tap,' +
        ' then set to the min or max tap position as appropriate. Default is 0';
    PropertyHelp[29] := '{Yes | No} If Yes, forces Reset of this RegControl.';
    PropertyHelp[30] := 'Z value for Beckwith LDC_Z control option. Volts adjustment at rated control current.';
    PropertyHelp[31] := 'Reverse Z value for Beckwith LDC_Z control option.';
    PropertyHelp[32] := '{Yes|No*} Default is No. The Cogen feature is activated. Continues looking forward if power ' +
        'reverses, but switches to reverse-mode LDC values.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

{--------------------------------------------------------------------------}
function TRegControl.NewObject(const ObjName: String): Integer;
begin
    // Make a new RegControl and add it to RegControl class list
    with ActiveCircuit do
    begin
        ActiveCktElement := TRegControlObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;

{--------------------------------------------------------------------------}
function TRegControl.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

    function Max(a, b: Integer): Integer;
    begin
        if a >= b then
            Result := a
        else
            Result := b;
    end;

begin

  // continue parsing WITH contents of Parser
    ActiveRegControlObj := ElementList.Active;
    ActiveCircuit.ActiveCktElement := ActiveRegControlObj;

    Result := 0;

    with ActiveRegControlObj do
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

            case ParamPointer of
                0:
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 120);
                1:
                    ElementName := 'Transformer.' + lowercase(param);  // Initialize to Transformer
                2:
                    ElementTerminal := Parser.IntValue;
                3:
                    Vreg := Parser.DblValue;
                4:
                    Bandwidth := Parser.DblValue;
                5:
                    PTRatio := Parser.DblValue;
                6:
                    CTRating := Parser.DblValue;
                7:
                    R := Parser.DblValue;
                8:
                    X := Parser.DblValue;
                9:
                    RegulatedBus := Param;      // Buaname.node
                10:
                    TimeDelay := Parser.DblValue;
                11:
                    IsReversible := InterpretYesNo(Param);
                12:
                    revVreg := Parser.DblValue;
                13:
                    revBandwidth := Parser.DblValue;
                14:
                    revR := Parser.DblValue;
                15:
                    revX := Parser.DblValue;
                16:
                    TapDelay := Parser.DblValue;
                17:
                    DebugTrace := InterpretYesNo(Param);
                18:
                    TapLimitPerChange := max(0, Parser.IntValue);
                19:
                    FInversetime := InterpretYesNo(Param);
                20:
                    TapWinding := Parser.intValue;
                21:
                begin
                    Vlimit := Parser.DblValue;
                    if VLimit > 0.0 then
                        VLimitActive := TRUE
                    else
                        VLimitActive := FALSE;
                end;
                22:
                    if CompareTextShortest(param, 'max') = 0 then
                        FPTPhase := MAXPHASE
                    else
                    if CompareTextShortest(param, 'min') = 0 then
                        FPTPhase := MINPHASE
                    else
                        FPTPhase := max(1, Parser.IntValue);
                23:
                    kWRevPowerThreshold := Parser.DblValue;
                24:
                    RevDelay := Parser.DblValue;
                25:
                    ReverseNeutral := InterpretYesNo(Param);
                26:
                    ShowEventLog := InterpretYesNo(param);
                27:
                    RemotePTRatio := Parser.DblValue;
                28:
                    TapNum := Parser.IntValue;
                29:
                    if InterpretYesNo(Param) then
                    begin  // force a reset
                        Reset;
                        PropertyValue[29] := 'n'; // so it gets reported properly
                    end;
                30:
                    LDC_Z := Parser.DblValue;
                31:
                    revLDC_Z := Parser.DblValue;
                32:
                    CogenEnabled := InterpretYesNo(Param);

            else
           // Inherited parameters
                ClassEdit(ActiveRegControlObj, ParamPointer - NumPropsthisClass)
            end;

            case ParamPointer of
                2:
                begin
                    Tapwinding := ElementTerminal;  // Resets if property re-assigned
                    PropertyValue[20] := Param;
                end;
                5:
                    RemotePTRatio := PTRatio;  // re-initialise RemotePTRatio whenever PTRatio is set
                17:
                    if DebugTrace then
                    begin
                        AssignFile(TraceFile, GetOutputDirectory + 'REG_' + Name + '.CSV');
                        ReWrite(TraceFile);
                        Writeln(TraceFile, 'Hour, Sec, ControlIteration, Iterations, LoadMultiplier, Present Tap, Pending Change, Actual Change, Increment, Min Tap, Max Tap');
                        CloseFile(Tracefile);
                    end;
                23:
                    RevPowerThreshold := kWRevPowerThreshold * 1000.0;
            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

        RecalcElementData;
    end;  {With}

end;


{--------------------------------------------------------------------------}
function TRegControl.MakeLike(const RegControlName: String): Integer;
var
    OtherRegControl: TRegControlObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this RegControl name in the present collection}
    OtherRegControl := Find(RegControlName);
    if OtherRegControl <> NIL then
        with ActiveRegControlObj do
        begin

            Nphases := OtherRegControl.Fnphases;
            NConds := OtherRegControl.Fnconds; // Force Reallocation of terminal stuff

            ElementName := OtherRegControl.ElementName;
            ControlledElement := OtherRegControl.ControlledElement;  // Pointer to target circuit element
            ElementTerminal := OtherRegControl.ElementTerminal;

            Vreg := OtherRegControl.Vreg;
            Bandwidth := OtherRegControl.Bandwidth;
            PTRatio := OtherRegControl.PTRatio;
            RemotePTRatio := OtherRegControl.RemotePTRatio;
            CTRating := OtherRegControl.CTRating;
            R := OtherRegControl.R;
            X := OtherRegControl.X;
            RegulatedBus := OtherRegControl.RegulatedBus;
            TimeDelay := OtherRegControl.TimeDelay;
            IsReversible := OtherRegControl.IsReversible;
            revVreg := OtherRegControl.revVreg;
            revBandwidth := OtherRegControl.revBandwidth;
            revR := OtherRegControl.revR;
            revX := OtherRegControl.revX;
            TapDelay := OtherRegControl.TapDelay;
            TapWinding := OtherRegControl.TapWinding;
            FInversetime := OtherRegControl.FInversetime;

            TapLimitPerChange := OtherRegControl.TapLimitPerChange;
            kWRevPowerThreshold := OtherRegControl.kWRevPowerThreshold;
            RevPowerThreshold := OtherRegControl.RevPowerThreshold;
            RevDelay := OtherRegControl.RevDelay;
            ReverseNeutral := OtherRegControl.ReverseNeutral;
            ShowEventLog := OtherRegControl.ShowEventLog;
    //    DebugTrace     := OtherRegControl.DebugTrace;  Always default to NO

            FPTphase := OtherRegControl.FPTphase;
            TapNum := OtherRegControl.TapNum;
            CogenEnabled := OtherRegControl.CogenEnabled;
            LDC_Z := OtherRegControl.LDC_Z;
            RevLDC_Z := OtherRegControl.revLDC_Z;

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherRegControl.PropertyValue[i];
        end

    else
        DoSimpleMsg('Error in RegControl MakeLike: "' + RegControlName + '" Not Found.', 121);

end;


{==========================================================================}
{                    TRegControlObj                                           }
{==========================================================================}


{--------------------------------------------------------------------------}
constructor TRegControlObj.Create(ParClass: TDSSClass; const RegControlName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(RegControlName);
    DSSObjType := ParClass.DSSClassType;

    NPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors
                         // in base class


    Vreg := 120.0;
    Bandwidth := 3.0;
    PTRatio := 60.0;
    RemotePTRatio := PTRatio;
    CTRating := 300.0;
    R := 0.0;
    X := 0.0;
    LDC_Z := 0.0;
    TimeDelay := 15.0;

    FPTphase := 1;


    LDCActive := FALSE;
    TapDelay := 2.0;
    TapLimitPerChange := 16;

    DebugTrace := FALSE;
    Armed := FALSE;

    {Reverse mode variables}
    revVreg := 120.0;
    revBandwidth := 3.0;
    revR := 0.0;
    revX := 0.0;
    revLDC_Z := 0.0;
    revDelay := 60.0; // Power must be reversed this long before it will reverse
    RevPowerThreshold := 100000.0; // 100 kW
    kWRevPowerThreshold := 100.0;
    IsReversible := FALSE;
    ReversePending := FALSE;
    InReverseMode := FALSE;
    ReverseNeutral := FALSE;
    InCogenMode := FALSE;
    CogenEnabled := FALSE;

    RevHandle := 0;
    RevBackHandle := 0;

    ElementName := '';
    ControlledElement := NIL;
    ElementTerminal := 1;
    TapWinding := ElementTerminal;

    VBuffer := NIL;
    CBuffer := NIL;

    DSSObjType := ParClass.DSSClassType; //REG_CONTROL;

    InitPropertyValues(0);
    FInversetime := FALSE;
    RegulatedBus := '';
    Vlimit := 0.0;

    ControlActionHandle := 0;

   //  RecalcElementData;

end;

destructor TRegControlObj.Destroy;
begin
    ElementName := '';
    if Assigned(VBuffer) then
        ReallocMem(VBuffer, 0);
    if Assigned(CBuffer) then
        ReallocMem(CBuffer, 0);
    inherited Destroy;
end;

{--------------------------------------------------------------------------}
procedure TRegControlObj.RecalcElementData;

var
    DevIndex: Integer;
    TransName, NewElementName: String;

begin
    if (R <> 0.0) or (X <> 0.0) or (LDC_Z > 0.0) then
        LDCActive := TRUE
    else
        LDCActive := FALSE;
    if Length(RegulatedBus) = 0 then
        UsingRegulatedBus := FALSE
    else
        UsingRegulatedBus := TRUE;

    Devindex := GetCktElementIndex(ElementName); // Global FUNCTION
    if DevIndex = 0 then
    begin // Try 'AutoTrans' instead of Transformer
        TransName := StripClassName(ElementName);
        NewElementName := 'autotrans.' + TransName;
        Devindex := GetCktElementIndex(NewElementName);
        if Devindex > 0 then
            ElementName := NewElementName;
    end;

    if DevIndex > 0 then
    begin  // RegControled element must already exist
        ControlledElement := ActiveCircuit.CktElements.Get(DevIndex);

        if UsingRegulatedBus then
        begin
            Nphases := 1;     // Only need one phase
            Nconds := 2;
        end
        else
        begin
            Nphases := ControlledElement.NPhases;
            Nconds := FNphases;
            if FPTphase > FNphases then
            begin
                FPTphase := 1;
                PropertyValue[22] := '1';
            end;
        end;

        if (Comparetext(ControlledElement.DSSClassName, 'transformer') = 0) or  // either should work
            (Comparetext(ControlledElement.DSSClassName, 'autotrans') = 0) then
        begin
            if ElementTerminal > ControlledElement.Nterms then
            begin
                DoErrorMsg('RegControl: "' + Name + '"', 'Winding no. "' + '" does not exist.',
                    'Respecify Monitored Winding no.', 122);
            end
            else
            begin
                     // Sets name of i-th terminal's connected bus in RegControl's buslist
                     // This value will be used to set the NodeRef array (see Sample function)
                if UsingRegulatedBus then
                    Setbus(1, RegulatedBus)   // hopefully this will actually exist
                else
                    Setbus(1, ControlledElement.GetBus(ElementTerminal));
                ReAllocMem(VBuffer, SizeOF(Vbuffer^[1]) * ControlledElement.NPhases);  // buffer to hold regulator voltages
                ReAllocMem(CBuffer, SizeOF(CBuffer^[1]) * ControlledElement.Yorder);
            end;
        end
        else
        begin
            ControlledElement := NIL;   // we get here if element not found
            DoErrorMsg('RegControl: "' + Self.Name + '"', 'Controlled Regulator Element "' + ElementName + '" Is not a transformer.',
                ' Element must be defined previously.', 123);
        end;
    end
    else
    begin
        ControlledElement := NIL;   // element not found
        DoErrorMsg('RegControl: "' + Self.Name + '"', 'Transformer Element "' + ElementName + '" Not Found.',
            ' Element must be defined previously.', 124);
    end;
end;

{--------------------------------------------------------------------------}
procedure TRegControlObj.CalcYPrim;
begin
  // leave YPrim as nil and it will be ignored ... zero current source
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
end;


{--------------------------------------------------------------------------}
function TRegControlObj.GetControlVoltage(VBuffer: pComplexArray; Nphs: Integer; PTRatio: Double): Complex;

var
    i: Integer;
    V: Double;

begin


    case FPTphase of
{
         AVGPHASES: Begin
                        Result := CZERO;
                        FOR i := 1 to Nphs Do Result := Result + Cabs(VBuffer^[i]);
                        Result := CdivReal(Result, (Nphs*PTRatio));
                    End;

}       MAXPHASE:
        begin
            ControlledPhase := 1;
            V := Cabs(VBuffer^[ControlledPhase]);
            for i := 2 to Nphs do
                if Cabs(VBuffer^[i]) > V then
                begin
                    V := Cabs(VBuffer^[i]);
                    ControlledPhase := i;
                end;
            Result := CDivReal(VBuffer^[ControlledPhase], PTRatio);
        end;
        MINPHASE:
        begin
            ControlledPhase := 1;
            V := Cabs(VBuffer^[ControlledPhase]);
            for i := 2 to Nphs do
                if Cabs(VBuffer^[i]) < V then
                begin
                    V := Cabs(VBuffer^[i]);
                    ControlledPhase := i;
                end;
            Result := CDivReal(VBuffer^[ControlledPhase], PTRatio);
        end;
    else
    {Just use one phase because that's what most controls do.}
        Result := CDivReal(VBuffer^[FPTPhase], PTRatio);
        ControlledPhase := FPTPhase;
    end;


end;

procedure TRegControlObj.GetCurrents(Curr: pComplexArray);
var
    i: Integer;
begin

    for i := 1 to Fnconds do
        Curr^[i] := CZERO;

end;

procedure TRegControlObj.GetInjCurrents(Curr: pComplexArray);
var
    i: Integer;
begin
    for i := 1 to Fnconds do
        Curr^[i] := cZero;
end;

{--------------------------------------------------------------------}

function TRegControlObj.GetPropertyValue(Index: Integer): String;
begin
    case Index of
        28:
            Result := Format('%d', [Tapnum]);
    else
        Result := inherited GetPropertyValue(index);
    end;
end;

{--------------------------------------------------------------------------}
procedure TRegControlObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
        end;

     // Note: The PropertyValue access function calls GetPropertyValue routine.

    if Complete then
    begin
        Writeln(F, '! Bus =', GetBus(1));
        Writeln(F);
    end;

end;

{--------------------------------------------------------------------------}
function TRegControlObj.AtLeastOneTap(const ProposedChange: Double; Increment: Double): Double;

// Called in STATIC mode
// Changes 70% of the way but at least one tap, subject to maximum allowable tap change
var
    NumTaps: Integer;

begin

    NumTaps := Trunc(0.7 * Abs(ProposedChange) / Increment);

    if NumTaps = 0 then
        NumTaps := 1;

    if NumTaps > TapLimitPerChange then
        NumTaps := TapLimitPerChange;

    LastChange := NumTaps;

    if ProposedChange > 0.0    // check sign on change
    then
        Result := NumTaps * Increment
    else
    begin
        Result := -NumTaps * Increment;
        LastChange := -NumTaps;
    end;

end;


{--------------------------------------------------------------------------}
function OneInDirectionOf(var ProposedChange: Double; Increment: Double): Double;

// Computes the amount of one tap change in the direction of the pending tapchange
// Automatically decrements the proposed change by that amount

begin
    LastChange := 0;
    if ProposedChange > 0.0 then
    begin
        Result := Increment;
        LastChange := 1;
        ProposedChange := ProposedChange - Increment;
    end
    else
    begin
        Result := -Increment;
        LastChange := -1;
        ProposedChange := ProposedChange + Increment;
    end;

    if Abs(ProposedChange) < 0.9 * Increment then
        ProposedChange := 0.0;

end;

{--------------------------------------------------------------------------}
procedure TRegControlObj.DoPendingAction(const Code, ProxyHdl: Integer);

// 2-23-00 Modified to change one tap at a time
var
    TapChangeToMake: Double;

begin

    case Code of
        ACTION_TAPCHANGE:
        begin
            if (DebugTrace) then
                with ActiveCircuit do
                    RegWriteDebugRecord(Format('+++ %.6g s: Handling TapChange = %.8g', [Solution.DynaVars.t, PendingTapChange]));

            if PendingTapChange = 0.0 then  {Check to make sure control has not reset}

                Armed := FALSE

            else
                with   TTransfObj(ControlledElement) do
                begin

                 // Transformer PresentTap property automatically limits tap
                    with ActiveCircuit, ActiveCircuit.Solution do
                    begin
                        case ControlMode of
                            CTRLSTATIC:
                            begin
                                TapChangeToMake := AtLeastOneTap(PendingTapChange, TapIncrement[TapWinding]);
                                if (DebugTrace) then
                                    RegWriteTraceRecord(TapChangeToMake);
                                PresentTap[TapWinding] := PresentTap[TapWinding] + TapChangeToMake;
                                if ShowEventLog then
                                    AppendtoEventLog('Regulator.' + ControlledElement.Name, Format(' Changed %d taps to %-.6g.', [Lastchange, PresentTap[TapWinding]]));
                                PendingTapChange := 0.0;  // Reset to no change.  Program will determine if another needed.
                                Armed := FALSE;
                            end;

                            EVENTDRIVEN:
                            begin
                                TapChangeToMake := OneInDirectionOf(FPendingTapChange, TapIncrement[TapWinding]);
                                if (DebugTrace) then
                                    RegWriteTraceRecord(TapChangeToMake);
                                PresentTap[TapWinding] := PresentTap[TapWinding] + TapChangeToMake;
                                if PendingTapChange <> 0.0 then
                                    ControlQueue.Push(DynaVars.intHour, Dynavars.t + TapDelay, 0, 0, Self)
                                else
                                    Armed := FALSE;
                            end;

                            TIMEDRIVEN:
                            begin
                                TapChangeToMake := OneInDirectionOf(FPendingTapChange, TapIncrement[TapWinding]);
                                if (DebugTrace) then
                                    RegWriteTraceRecord(TapChangeToMake);
                                PresentTap[TapWinding] := PresentTap[TapWinding] + TapChangeToMake;
                                if ShowEventLog then
                                    AppendtoEventLog('Regulator.' + ControlledElement.Name, Format(' Changed %d tap to %-.6g.', [Lastchange, PresentTap[TapWinding]]));
                                if (DebugTrace) then
                                    RegWriteDebugRecord(Format('--- Regulator.%s Changed %d tap to %-.6g.', [ControlledElement.Name, Lastchange, PresentTap[TapWinding]]));

                                if PendingTapChange <> 0.0 then
                                    ControlQueue.Push(DynaVars.intHour, DynaVars.t + TapDelay, 0, 0, Self)
                                else
                                    Armed := FALSE;
                            end;

                            MULTIRATE:
                            begin
                                TapChangeToMake := OneInDirectionOf(FPendingTapChange, TapIncrement[TapWinding]);
                                if (DebugTrace) then
                                    RegWriteTraceRecord(TapChangeToMake);
                                PresentTap[TapWinding] := PresentTap[TapWinding] + TapChangeToMake;
                                if ShowEventLog then
                                    AppendtoEventLog('Regulator.' + ControlledElement.Name, Format(' Changed %d tap to %-.6g.', [Lastchange, PresentTap[TapWinding]]));
                                if (DebugTrace) then
                                    RegWriteDebugRecord(Format('--- Regulator.%s Changed %d tap to %-.6g.', [ControlledElement.Name, Lastchange, PresentTap[TapWinding]]));

                                if PendingTapChange <> 0.0 then
                                    ControlQueue.Push(DynaVars.intHour, DynaVars.t + TapDelay, 0, 0, Self)
                                else
                                    Armed := FALSE;
                            end;

                        end;
                    end;
                end;
        end;  {ACTION_TAPCHANGE}

        ACTION_REVERSE:
        begin  // Toggle reverse mode or Cogen mode flag
            if (DebugTrace) then
                RegWriteDebugRecord(Format('Handling Reverse Action, ReversePending=%s, InReverseMode=%s',
                    [BoolToStr(ReversePending, TRUE), BoolToStr(InReverseMode, TRUE)]));
            if ReversePending then        // check to see if action has reset
            begin
                if CogenEnabled then
                begin   // Cogen mode takes precedence if present
                    if InCogenMode then
                        InCogenMode := FALSE
                    else
                        InCogenMode := TRUE;
                end
                else
                if InReverseMode then
                    InReverseMode := FALSE
                else
                    InReverseMode := TRUE;
                ReversePending := FALSE;
            end;
        end;  {ACTION_REVERSE}

    end;
end;

procedure TRegControlObj.Sample;

{This is where it all happens ...}

var

    BoostNeeded,
    Increment,
    Vactual,
    VregTest,
    BandTest,
    Vboost: Double;
    VlocalBus: Double;
    FwdPower: Double;
    Vcontrol,
    VLDC,
    ILDC: Complex;
    TapChangeIsNeeded: Boolean;
    LookingForward: Boolean;
    i, ii: Integer;
    ControlledTransformer: TTransfObj;
    TransformerConnection: Integer;

begin
    ControlledTransformer := TTransfObj(ControlledElement);

    if TapLimitPerChange = 0 then
    begin
        PendingTapChange := 0;
        Exit;
    end;

    LookingForward := (not InReverseMode) or InCogenMode; // Always looking forward in cogen mode

     {First, check the direction of power flow to see if we need to reverse direction}
     {Don't do this if using regulated bus logic}
    if not UsingRegulatedBus then
    begin
        if IsReversible or CogenEnabled then
        begin

            if LookingForward and (not InCogenMode) then   // If looking forward, check to see if we should reverse
            begin
                FwdPower := -ControlledTransformer.Power[ElementTerminal].re;  // watts
                if (not ReversePending) then  // If reverse is already pending, don't send any more messages
                begin
                    if (FwdPower < -RevPowerThreshold) then
                    begin
                        ReversePending := TRUE;
                        with ActiveCircuit do
                            RevHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + RevDelay, ACTION_REVERSE, 0, Self);
                        if (DebugTrace) then
                            RegWriteDebugRecord(Format('Pushed Reverse Action, Handle=%d, FwdPower=%.8g', [RevHandle, FwdPower]));
                    end
                end;
                if ReversePending and (FwdPower >= -RevPowerThreshold) then // Reset  reverse pending
                begin
                    ReversePending := FALSE; // Reset it if power goes back
                    if RevHandle > 0 then
                    begin
                        if (DebugTrace) then
                            RegWriteDebugRecord(Format('Deleting Reverse Action, Handle=%d', [RevHandle]));
                        ActiveCircuit.ControlQueue.Delete(RevHandle);
                        RevHandle := 0;   // reset for next time
                    end;
                end;
            end

            else      // Looking the reverse direction or in cogen mode

            begin   // If reversed look to see if power is back in forward direction
                FwdPower := -ControlledTransformer.Power[ElementTerminal].re;  // watts
                if not ReversePending then
                begin
                    if (FwdPower > RevPowerThreshold) then
                    begin
                        ReversePending := TRUE;
                        with ActiveCircuit do
                            RevBackHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + RevDelay, ACTION_REVERSE, 0, Self);
                        if (DebugTrace) then
                            RegWriteDebugRecord(Format('Pushed ReverseBack Action to switch back, Handle=%d, FwdPower=%.8g', [RevBackHandle, FwdPower]));
                    end
                end;
                if ReversePending and (FwdPower <= RevPowerThreshold) then // Reset  reverse pending                            Else
                begin
                    ReversePending := FALSE; // Reset it if power goes back
                    if RevBackHandle > 0 then
                    begin
                        if (DebugTrace) then
                            RegWriteDebugRecord(Format('Deleting ReverseBack Action, Handle=%d', [RevBackHandle]));
                        ActiveCircuit.ControlQueue.Delete(RevBackHandle);
                        RevBackHandle := 0;   // reset for next time
                    end;
                end;

                  {Check for special case of Reverse Neutral where regulator is to move to neutral position}
                  {Both Cogen Mode and Reverse operaiont}
                with ControlledTransformer do
                    if ReverseNeutral then
                    begin
                        if not Armed then
                        begin
                            PendingTapChange := 0.0;
                            if (abs(PresentTap[TapWinding] - 1.0) > Epsilon) then
                            begin
                                Increment := TapIncrement[TapWinding];
                                PendingTapChange := Round((1.0 - PresentTap[Tapwinding]) / Increment) * Increment;
                                if (PendingTapChange <> 0.0) and not Armed then
                                    with ActiveCircuit do
                                    begin
                                        if (DebugTrace) then
                                            RegWriteDebugRecord(Format('*** %.6g s: Pushing TapChange = %.8g, delay= %.8g', [Solution.DynaVars.t, PendingTapChange, TapDelay]));
                                        ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TapDelay, ACTION_TAPCHANGE, 0, Self);
                                        Armed := TRUE;
                                    end;
                            end;
                        end;
                        Exit;  // We're done here in any case if Reverse neutral specified
                    end;

            end; {Else}
        end;
    end;


    if UsingRegulatedBus then
    begin
        TransformerConnection := ControlledTransformer.Winding^[ElementTerminal].Connection;
        ComputeVTerminal;   // Computes the voltage at the bus being regulated
        for i := 1 to Fnphases do
        begin
            case TransformerConnection of
                0:
                begin      // Wye
                    VBuffer^[i] := Vterminal^[i];
                end;
                1:
                begin   // Delta
                    ii := ControlledTransformer.RotatePhases(i);      // Get next phase in sequence using Transformer Obj rotate
                    VBuffer^[i] := CSub(Vterminal^[i], Vterminal^[ii]);
                end
            end;
        end;
        Vcontrol := GetControlVoltage(VBuffer, Fnphases, RemotePTRatio);
    end
    else
    begin
        ControlledTransformer.GetWindingVoltages(ElementTerminal, VBuffer);
        Vcontrol := GetControlVoltage(VBuffer, Fnphases, PTRatio);
    end;

     // Check Vlimit
    if VlimitActive then
    begin
        if UsingRegulatedBus then
        begin
            ControlledTransformer.GetWindingVoltages(ElementTerminal, VBuffer);
            Vlocalbus := Cabs(CDivReal(VBuffer^[1], PTRatio));
        end
        else
        begin
            Vlocalbus := Cabs(Vcontrol);
        end;
    end
    else
        Vlocalbus := 0.0; // to get rid of warning message;

     // Check for LDC
    if not UsingRegulatedBus and LDCActive then
    begin
        ControlledElement.GetCurrents(Cbuffer);
        // Convert current to control current by CTRating
        ILDC := CDivReal(CBuffer^[ControlledElement.Nconds * (ElementTerminal - 1) + ControlledPhase], CTRating);
        if LDC_Z = 0.0 then  // Standard R, X LDC
        begin
            if InReverseMode or InCogenMode then
                VLDC := Cmul(Cmplx(revR, revX), ILDC)
            else
                VLDC := Cmul(Cmplx(R, X), ILDC);
            Vcontrol := Cadd(Vcontrol, VLDC);   // Direction on ILDC is INTO terminal, so this is equivalent to Vterm - (R+jX)*ILDC
        end
        else // Beckwith LDC_Z control mode
        begin
            if InReverseMode or InCogenMode then
                Vcontrol := Cmplx((Cabs(VControl) - Cabs(ILDC) * revLDC_Z), 0.0)
            else
                Vcontrol := Cmplx((Cabs(VControl) - Cabs(ILDC) * LDC_Z), 0.0);   // Just magnitudes
        end;
    end;

    Vactual := Cabs(Vcontrol);   // Assumes looking forward; see below

    with  ControlledTransformer do
    begin
         // Check for out of band voltage
        if InReverseMode then
        begin
            Vactual := Vactual / PresentTap[TapWinding];
            VregTest := RevVreg;
            BandTest := RevBandwidth;
        end
        else
        begin   // Forward or Cogen Modes
            VregTest := Vreg;
            BandTest := Bandwidth;
        end;
        if (Abs(VregTest - Vactual) > BandTest / 2.0) then
            TapChangeIsNeeded := TRUE
        else
            TapChangeIsNeeded := FALSE;

        if Vlimitactive then
            if (Vlocalbus > Vlimit) then
                TapChangeIsNeeded := TRUE;

        if TapChangeIsNeeded then
        begin
                // Compute tapchange
            Vboost := (VregTest - Vactual);
            if Vlimitactive then
                if (Vlocalbus > Vlimit) then
                    Vboost := (Vlimit - Vlocalbus);
            BoostNeeded := Vboost * PTRatio / BaseVoltage[ElementTerminal];  // per unit Winding boost needed
            Increment := TapIncrement[TapWinding];
            PendingTapChange := Round(BoostNeeded / Increment) * Increment;  // Make sure it is an even increment

                {If Tap is another winding or in REVERSE MODE, it has to move the other way to accomplish the change}
            if (TapWinding <> ElementTerminal) or InReverseMode then
                PendingTapChange := -PendingTapChange;

                // Send Initial Tap Change message to control queue
                // Add Delay time to solution control queue
            if (PendingTapChange <> 0.0) and not Armed then
            begin
                     // Now see if any tap change is possible in desired direction  Else ignore
                if PendingTapChange > 0.0 then
                begin
                    if PresentTap[TapWinding] < MaxTap[TapWinding] then
                        with ActiveCircuit do
                        begin
                            ControlActionHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ComputeTimeDelay(Vactual), ACTION_TAPCHANGE, 0, Self);
                            Armed := TRUE;  // Armed to change taps
                        end;
                end
                else
                begin
                    if PresentTap[TapWinding] > MinTap[TapWinding] then
                        with ActiveCircuit do
                        begin
                            ControlActionHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ComputeTimeDelay(Vactual), ACTION_TAPCHANGE, 0, Self);
                            Armed := TRUE;  // Armed to change taps
                        end;
                end;
            end;
        end {If TapChangeIsNeeded}
        else
        begin {Reset if back in band.}
            PendingTapChange := 0.0;
            if Armed then
            begin
                ActiveCircuit.ControlQueue.Delete(ControlActionHandle);
                Armed := FALSE;
                ControlActionHandle := 0;
            end;
        end;

    end;

end;

function TRegControlObj.Get_Transformer: TTransfObj;
begin

    Result := TTransfObj(ControlledElement);
end;

function TRegControlObj.Get_Winding: Integer;
begin
    Result := TapWinding;
end;

function TRegControlObj.Get_TapNum: Integer;
var
    ctrldTransformer: TTransfObj;
    ictrldWinding: Integer;

begin
    if ControlledElement <> NIL then
    begin

        ctrldTransformer := Get_Transformer;
        ictrldWinding := TRWinding;
        with ctrldTransformer do
            Result := round((PresentTap[ictrldWinding] - (MaxTap[ictrldWinding] + MinTap[ictrldWinding]) / 2.0) / TapIncrement[ictrldWinding]);

    end
    else
        Result := 0;
end;

function TRegControlObj.Get_MinTap: Double;
begin
    Result := Get_Transformer.Mintap[TapWinding];
end;

function TRegControlObj.Get_MaxTap: Double;
begin
    Result := Get_Transformer.Maxtap[TapWinding];
end;

function TRegControlObj.Get_TapIncrement: Double;
begin
    Result := Get_Transformer.TapIncrement[TapWinding];
end;

function TRegControlObj.Get_NumTaps: Integer;
begin
    Result := Get_Transformer.NumTaps[TapWinding];
end;

procedure TRegControlObj.RegWriteDebugRecord(S: String);
// write a general debug string
begin
    try
        if (not InshowResults) then
        begin
            Append(TraceFile);
            Writeln(TraceFile, S);
            CloseFile(TraceFile);
        end;
    except
        On E: Exception do
        begin
        end;

    end;

end;

procedure TRegControlObj.RegWriteTraceRecord(TapChangeMade: Double);
var
    Separator: String;

begin

    try
        if (not InshowResults) then
        begin
            Separator := ', ';
            Append(TraceFile);
            with TTransfObj(ControlledElement) do
                Writeln(TraceFile,
                    ActiveCircuit.Solution.DynaVars.intHour: 0, Separator,
                    ActiveCircuit.Solution.DynaVars.t: 0: 3, Separator,
                    ActiveCircuit.Solution.ControlIteration: 0, Separator,
                    ActiveCircuit.Solution.Iteration: 0, Separator,
                    ActiveCircuit.LoadMultiplier: 6: 2, Separator,
                    PresentTap[ElementTerminal]: 8: 5, Separator,
                    PendingTapChange: 8: 5, Separator,
                    TapChangeMade: 8: 5, Separator,
                    TapIncrement[ElementTerminal]: 8: 5, Separator,
                    MinTap[ElementTerminal]: 8: 5, Separator,
                    MaxTap[ElementTerminal]: 8: 5);

            CloseFile(TraceFile);
        end;
    except
        On E: Exception do
        begin {Do Nothing}
        end;

    end;
end;

procedure TRegControlObj.Reset;
begin
    PendingTapChange := 0.0;
    ARMED := FALSE;
end;

procedure TRegcontrolObj.SaveWrite(var F: TextFile);
{Override standard SaveWrite}
{Regcontrol structure not conducive to standard means of saving}
var
    iprop: Integer;
begin
   {Write only properties that were explicitly set in the
   final order they were actually set}

   // Write Transformer name out first so that it is set for later operations
    iProp := 1;
    if Length(PropertyValue[iProp]) > 0 then
        with ParentClass do
            Write(F, Format(' %s=%s', [PropertyName^[RevPropertyIdxMap[iProp]], CheckForBlanks(PropertyValue[iProp])]));

    iProp := GetNextPropertySet(0); // Works on ActiveDSSObject
    while iProp > 0 do
        with ParentClass do
        begin
            if iProp <> 1 then   // Don't repeat Transformer property
                if Length(PropertyValue[iProp]) > 0 then
                    Write(F, Format(' %s=%s', [PropertyName^[RevPropertyIdxMap[iProp]], CheckForBlanks(PropertyValue[iProp])]));
            iProp := GetNextPropertySet(iProp);
        end;
end;

procedure TRegcontrolObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := ''; //'element';
    PropertyValue[2] := '1'; //'terminal';
    PropertyValue[3] := '120';
    PropertyValue[4] := '3';
    PropertyValue[5] := '60';
    PropertyValue[6] := '300';
    PropertyValue[7] := '0';
    PropertyValue[8] := '0';
    PropertyValue[9] := '';
    PropertyValue[10] := '15';
    PropertyValue[11] := 'no';
    PropertyValue[12] := '120';
    PropertyValue[13] := '3';
    PropertyValue[14] := '0';
    PropertyValue[15] := '0';
    PropertyValue[16] := '2';
    PropertyValue[17] := 'no';
    PropertyValue[18] := '16';
    PropertyValue[19] := 'no';
    PropertyValue[20] := '1';
    PropertyValue[21] := '0.0';
    PropertyValue[22] := '1';
    PropertyValue[23] := '100';
    PropertyValue[24] := '60';
    PropertyValue[25] := 'No';
    PropertyValue[26] := 'YES';
    PropertyValue[27] := '60';
    PropertyValue[28] := '0';
    PropertyValue[29] := 'NO';
    PropertyValue[30] := '0';
    PropertyValue[31] := '0';
    PropertyValue[32] := 'No';

    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TRegControlObj.set_PendingTapChange(const Value: Double);
begin
    FPendingTapChange := Value;
    dblTraceParameter := Value;
end;

procedure TRegControlObj.Set_TapNum(const Value: Integer);
var
    ctrldTransformer: TTransfObj;
    ictrldWinding: Integer;

begin
    if not Assigned(ControlledElement) then
        RecalcElementData;

    if ControlledElement <> NIL then
    begin
        ctrldTransformer := TTransfObj(ControlledElement);
        ictrldWinding := TRWinding;
        with ctrldTransformer do
            PresentTap[ictrldWinding] := Value * TapIncrement[ictrldWinding] + ((MaxTap[ictrldWinding] + MinTap[ictrldWinding]) / 2.0);

// Tap range checking is done in PresentTap
// You can attempt to set the tap at an illegal value but it won't do anything

    end;
end;


procedure TRegControlObj.MakePosSequence;
begin
    if ControlledElement <> NIL then
    begin
        Enabled := ControlledElement.Enabled;
        if UsingRegulatedBus then
            Nphases := 1
        else
            Nphases := ControlledElement.NPhases;
        Nconds := FNphases;
        if (Comparetext(ControlledElement.DSSClassName, 'transformer') = 0) or   // either should work
            (Comparetext(ControlledElement.DSSClassName, 'autotrans') = 0) then
        begin
        // Sets name of i-th terminal's connected bus in RegControl's buslist
        // This value will be used to set the NodeRef array (see Sample function)
            if UsingRegulatedBus then
                Setbus(1, RegulatedBus)   // hopefully this will actually exist
            else
                Setbus(1, ControlledElement.GetBus(ElementTerminal));
            ReAllocMem(VBuffer, SizeOF(Vbuffer^[1]) * ControlledElement.NPhases);  // buffer to hold regulator voltages
            ReAllocMem(CBuffer, SizeOF(CBuffer^[1]) * ControlledElement.Yorder);
        end;
    end;
    inherited;
end;

function TRegControlObj.ComputeTimeDelay(Vavg: Double): Double;
begin

    if Finversetime then
        Result := TimeDelay / Min(10.0, (2.0 * Abs(Vreg - Vavg) / Bandwidth))
    else
        Result := TimeDelay;
end;

initialization


end.
