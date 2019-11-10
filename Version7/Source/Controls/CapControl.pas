unit CapControl;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   Change Log
   2-14-00 Created

   3-1-00  Added Voltage override
   5/21/01  Fixed bug with number of phases
   5/30/01  Eliminated extra event queue reports
}

{
  A CapControl is a control element that is connected to a terminal of another
  circuit element and controls a capacitor.  The control is usually placed in the
  terminal of a line or transformer, although a voltage control device could be placed
  in the terminal of the capacitor it controls

  A CapControl is defined by a New command:

  New CapControl.Name=myname Element=devclass.name terminal=[ 1|2|...] Capacitor = name

  Capacitor to be controlled must already exist.
}

interface

uses
    Command,
    ControlClass,
    ControlElem,
    CktElement,
    Bus,
    DSSClass,
    Arraydef,
    ucomplex,
    Capacitor,
    utilities,
    CapControlVars,
    CapUserControl;

type


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TCapControl = class(TControlClass)
    PRIVATE

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const CapControlName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create(dss: TDSS);
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TCapControlObj = class(TControlElem)
    PRIVATE
        ControlType: ECapControlType;

        ControlVars: TCapControlVars;

        ControlledCapacitor: TCapacitorObj;

        cBuffer: pComplexArray;    // Complexarray buffer

        IsUserModel: Boolean;
        UserModel: TCapUserControl;

        FpctMinkvar: Double;

        function Get_Capacitor: TCapacitorObj;
        function NormalizeToTOD(h: Integer; sec: Double): Double;
        procedure Set_PendingChange(const Value: EControlAction);
        function Get_PendingChange: EControlAction;
        procedure GetControlVoltage(var ControlVoltage: Double);
        procedure GetControlCurrent(var ControlCurrent: Double);
        procedure GetBusVoltages(pBus: TDSSBus; Buff: pComplexArray);


    PUBLIC

        constructor Create(ParClass: TDSSClass; const CapControlName: String);
        destructor Destroy; OVERRIDE;

        procedure MakePosSequence; OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;    // Always Zero for a CapControl

        procedure Sample; OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state


        procedure GetCurrents(Curr: pComplexArray); OVERRIDE; // Get present value of terminal Curr
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

        property This_Capacitor: TCapacitorObj READ Get_Capacitor;  // Pointer to controlled Capacitor
        property PendingChange: EControlAction READ Get_PendingChange WRITE Set_PendingChange;

       // for CIM export, which doesn't yet use the delays, CT, PT, and voltage override
        property CapControlType: ECapControlType READ ControlType WRITE ControlType;
        property OnValue: Double READ ControlVars.ON_Value;
        property OffValue: Double READ ControlVars.OFF_Value;
        property PFOnValue: Double READ ControlVars.PFON_Value;
        property PFOffValue: Double READ ControlVars.PFOFF_Value;
        property PTRatioVal: Double READ ControlVars.PTratio;
        property CTRatioVal: Double READ ControlVars.CTratio;
        property OnDelayVal: Double READ ControlVars.OnDelay;
        property OffDelayVal: Double READ ControlVars.OffDelay;
        property VminVal: Double READ ControlVars.Vmin;
        property VmaxVal: Double READ ControlVars.Vmax;
        property UseVoltageOverride: Boolean READ ControlVars.Voverride;
        property DeadTimeVal: Double READ ControlVars.DeadTime;
        property PTPhase: Integer READ ControlVars.FPTPhase;
    end;


{--------------------------------------------------------------------------}
implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    Sysutils,
    uCmatrix,
    MathUtil,
    Math,
    DSSHelper;

const
    AVGPHASES = -1;
    MAXPHASE = -2;
    MINPHASE = -3;
    NumPropsThisClass = 22;


{--------------------------------------------------------------------------}
constructor TCapControl.Create(dss: TDSS);  // Creates superstructure for all CapControl objects
begin
    inherited Create(dss);

    Class_name := 'CapControl';
    DSSClassType := DSSClassType + CAP_CONTROL;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

{--------------------------------------------------------------------------}
destructor TCapControl.Destroy;

begin
    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TCapControl.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;

     // Define Property names

    PropertyName[1] := 'element';
    PropertyName[2] := 'terminal';
    PropertyName[3] := 'capacitor';
    PropertyName[4] := 'type';
    PropertyName[5] := 'PTratio';
    PropertyName[6] := 'CTratio';
    PropertyName[7] := 'ONsetting';
    PropertyName[8] := 'OFFsetting';
    PropertyName[9] := 'Delay';
    PropertyName[10] := 'VoltOverride';
    PropertyName[11] := 'Vmax';
    PropertyName[12] := 'Vmin';
    PropertyName[13] := 'DelayOFF';
    PropertyName[14] := 'DeadTime';
    PropertyName[15] := 'CTPhase';
    PropertyName[16] := 'PTPhase';
    PropertyName[17] := 'VBus';
    PropertyName[18] := 'EventLog';
    PropertyName[19] := 'UserModel';
    PropertyName[20] := 'UserData';
    PropertyName[21] := 'pctMinkvar';
    PropertyName[22] := 'Reset';


    PropertyHelp[1] := 'Full object name of the circuit element, typically a line or transformer, ' +
        'to which the capacitor control''s PT and/or CT are connected.' +
        'There is no default; must be specified.';
    PropertyHelp[2] := 'Number of the terminal of the circuit element to which the CapControl is connected. ' +
        '1 or 2, typically.  Default is 1.';
    PropertyHelp[3] := 'Name of Capacitor element which the CapControl controls. No Default; Must be specified.' +
        'Do not specify the full object name; "Capacitor" is assumed for ' +
        'the object class.  Example:' + CRLF + CRLF +
        'Capacitor=cap1';
    PropertyHelp[4] := '{Current | voltage | kvar | PF | time } Control type.  Specify the ONsetting and OFFsetting ' +
        'appropriately with the type of control. (See help for ONsetting)';
    PropertyHelp[5] := 'Ratio of the PT that converts the monitored voltage to the control voltage. ' +
        'Default is 60.  If the capacitor is Wye, the 1st phase line-to-neutral voltage is monitored.  Else, the line-to-line ' +
        'voltage (1st - 2nd phase) is monitored.';
    PropertyHelp[6] := 'Ratio of the CT from line amps to control ampere setting for current and kvar control types. ';
    PropertyHelp[7] := 'Value at which the control arms to switch the capacitor ON (or ratchet up a step).  ' + CRLF + CRLF +
        'Type of Control:' + CRLF + CRLF +
        'Current: Line Amps / CTratio' + CRLF +
        'Voltage: Line-Neutral (or Line-Line for delta) Volts / PTratio' + CRLF +
        'kvar:    Total kvar, all phases (3-phase for pos seq model). This is directional. ' + CRLF +
        'PF:      Power Factor, Total power in monitored terminal. Negative for Leading. ' + CRLF +
        'Time:    Hrs from Midnight as a floating point number (decimal). 7:30am would be entered as 7.5.';
    PropertyHelp[8] := 'Value at which the control arms to switch the capacitor OFF. (See help for ONsetting)' +
        'For Time control, is OK to have Off time the next day ( < On time)';
    PropertyHelp[9] := 'Time delay, in seconds, from when the control is armed before it sends out the switching ' +
        'command to turn ON.  The control may reset before the action actually occurs. ' +
        'This is used to determine which capacity control will act first. Default is 15.  You may specify any ' +
        'floating point number to achieve a model of whatever condition is necessary.';
    PropertyHelp[10] := '{Yes | No}  Default is No.  Switch to indicate whether VOLTAGE OVERRIDE is to be considered. ' +
        'Vmax and Vmin must be set to reasonable values if this property is Yes.';
    PropertyHelp[11] := 'Maximum voltage, in volts.  If the voltage across the capacitor divided by the PTRATIO is greater ' +
        'than this voltage, the capacitor will switch OFF regardless of other control settings. ' +
        'Default is 126 (goes with a PT ratio of 60 for 12.47 kV system).';
    PropertyHelp[12] := 'Minimum voltage, in volts.  If the voltage across the capacitor divided by the PTRATIO is less ' +
        'than this voltage, the capacitor will switch ON regardless of other control settings. ' +
        'Default is 115 (goes with a PT ratio of 60 for 12.47 kV system).';
    PropertyHelp[13] := 'Time delay, in seconds, for control to turn OFF when present state is ON. Default is 15.';
    PropertyHelp[14] := 'Dead time after capacitor is turned OFF before it can be turned back ON. Default is 300 sec.';
    PropertyHelp[15] := 'Number of the phase being monitored for CURRENT control or one of {AVG | MAX | MIN} for all phases. Default=1. ' +
        'If delta or L-L connection, enter the first or the two phases being monitored [1-2, 2-3, 3-1]. ' +
        'Must be less than the number of phases. Does not apply to kvar control which uses all phases by default.';
    PropertyHelp[16] := 'Number of the phase being monitored for VOLTAGE control or one of {AVG | MAX | MIN} for all phases. Default=1. ' +
        'If delta or L-L connection, enter the first or the two phases being monitored [1-2, 2-3, 3-1]. ' +
        'Must be less than the number of phases. Does not apply to kvar control which uses all phases by default.';
    PropertyHelp[17] := 'Name of bus to use for voltage override function. Default is bus at monitored terminal. ' +
        'Sometimes it is useful to monitor a bus in another location to emulate various DMS control algorithms.';
    PropertyHelp[18] := '{Yes/True* | No/False} Default is YES for CapControl. Log control actions to Eventlog.';
    PropertyHelp[19] := 'Name of DLL containing user-written CapControl model, overriding the default model.  Set to "none" to negate previous setting. ';
    PropertyHelp[20] := 'String (in quotes or parentheses if necessary) that gets passed to the user-written CapControl model Edit function for defining the data required for that model. ';
    PropertyHelp[21] := 'For PF control option, min percent of total bank kvar at which control will close capacitor switch. Default = 50.';
    PropertyHelp[22] := '{Yes | No} If Yes, forces Reset of this CapControl.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

{--------------------------------------------------------------------------}
function TCapControl.NewObject(const ObjName: String): Integer;
begin
    // Make a new CapControl and add it to CapControl class list
    with DSS.ActiveCircuit do
    begin
        ActiveCktElement := TCapControlObj.Create(Self, ObjName);
        Result := AddObjectToList(DSS.ActiveDSSObject);
    end;
end;

{--------------------------------------------------------------------------}
function TCapControl.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

begin

  // continue parsing WITH contents of Parser
    DSS.ActiveCapControlObj := ElementList.Active;
    DSS.ActiveCircuit.ActiveCktElement := DSS.ActiveCapControlObj;

    Result := 0;

    with DSS.ActiveCapControlObj do
    begin

        ParamPointer := 0;
        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
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
                    DoSimpleMsg(DSS, 'Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 352);
                1:
                    ElementName := ConstructElemName(DSS, lowercase(param));  // substitute @var value if any
                2:
                    ElementTerminal := DSS.Parser.IntValue;
                3:
                    ControlVars.CapacitorName := 'capacitor.' + param;   // will automatically substitute @var value
                4:
                    case lowercase(param)[1] of
                        'c':
                            ControlType := CURRENTCONTROL;
                        'v':
                            ControlType := VOLTAGECONTROL;
                        'k':
                            ControlType := KVARCONTROL;
                        't':
                            ControlType := TIMECONTROL;
                        'p':
                            ControlType := PFCONTROL;
                    else
                        DoSimpleMsg(DSS, Format('Unrecognized CapControl Type: "%s" (Capcontrol.%s)', [param, DSS.ActiveCapControlObj.name]), 352);
                    end;
                5:
                    ControlVars.PTRatio := DSS.Parser.DblValue;
                6:
                    ControlVars.CTRatio := DSS.Parser.DblValue;
                7:
                    ControlVars.ON_Value := DSS.Parser.DblValue;
                8:
                    ControlVars.OFF_Value := DSS.Parser.DblValue;
                9:
                    ControlVars.ONDelay := DSS.Parser.DblValue;
                10:
                    ControlVars.Voverride := InterpretYesNo(param);
                11:
                    ControlVars.Vmax := DSS.Parser.DblValue;
                12:
                    ControlVars.Vmin := DSS.Parser.DblValue;
                13:
                    ControlVars.OFFDelay := DSS.Parser.DblValue;
                14:
                    ControlVars.DeadTime := DSS.Parser.DblValue;
                15:
                    if CompareTextShortest(param, 'avg') = 0 then
                        ControlVars.FCTPhase := AVGPHASES
                    else
                    if CompareTextShortest(param, 'max') = 0 then
                        ControlVars.FCTPhase := MAXPHASE
                    else
                    if CompareTextShortest(param, 'min') = 0 then
                        ControlVars.FCTPhase := MINPHASE
                    else
                        ControlVars.FCTPhase := max(1, DSS.Parser.IntValue);
                16:
                    if CompareTextShortest(param, 'avg') = 0 then
                        ControlVars.FPTPhase := AVGPHASES
                    else
                    if CompareTextShortest(param, 'max') = 0 then
                        ControlVars.FPTPhase := MAXPHASE
                    else
                    if CompareTextShortest(param, 'min') = 0 then
                        ControlVars.FPTPhase := MINPHASE
                    else
                        ControlVars.FPTPhase := max(1, DSS.Parser.IntValue);
                17:
                begin
                    ControlVars.VoverrideBusSpecified := TRUE;
                    ControlVars.VOverrideBusName := Param;
                end;
                18:
                    ShowEventLog := InterpretYesNo(param);
                19:
                    UserModel.Name := DSS.Parser.StrValue;  // Connect to user written model
                20:
                    if UserModel.Exists then
                        UserModel.Edit := DSS.Parser.StrValue;  // Send edit string to user model
                21:
                    FpctMinKvar := DSS.Parser.DblValue;
                22:
                    if InterpretYesNo(Param) then
                    begin  // force a reset
                        Reset;
                        PropertyValue[22] := 'n'; // so it gets reported properly
                    end;
            else
           // Inherited parameters
                ClassEdit(DSS.ActiveCapControlObj, ParamPointer - NumPropsthisClass)
            end;


         {PF Controller changes}
            if ControlType = PFCONTROL then
                with ControlVars do
                    case ParamPointer of
                        1:
                            PropertyValue[1] := ElementName;  // Synch up with change
                        4:
                        begin
                            PFON_Value := 0.95;     // defaults
                            PFOFF_Value := 1.05;
                        end;

                        7:
                        begin
                            if (ON_Value >= -1.0) and (ON_Value <= 1.0) then
                            begin
                                if ON_Value < 0.0 then
                                    PFON_Value := 2.0 + ON_Value
                                else
                                    PFON_Value := ON_Value;
                            end
                            else
                            begin
                                DoSimpleMsg(DSS, 'Invalid PF ON value for CapControl.' + DSS.ActiveCapControlObj.Name, 353);
                            end;
                        end;
                        8:
                        begin
                            if (OFF_Value >= -1.0) and (OFF_Value <= 1.0) then
                            begin
                                if OFF_Value < 0.0 then
                                    PFOFF_Value := 2.0 + OFF_Value
                                else
                                    PFOFF_Value := OFF_Value;
                            end
                            else
                            begin
                                DoSimpleMsg(DSS, 'Invalid PF OFF value for CapControl.' + DSS.ActiveCapControlObj.Name, 35301);
                            end;
                        end;

                        15:
                            if FCTPhase > FNphases then
                            begin
                                DoSimpleMsg(DSS, Format('Error: Monitored phase(%d) must be less than or equal to number of phases(%d). ', [FCTPhase, FNphases]), 35302);
                                FCTPhase := 1;
                            end;

                        16:
                            if FPTPhase > FNphases then
                            begin
                                DoSimpleMsg(DSS, Format('Error: Monitored phase(%d) must be less than or equal to number of phases(%d). ', [FPTPhase, FNphases]), 35303);
                                FPTPhase := 1;
                            end;
                    end;

            case ParamPointer of
                19:
                    IsUserModel := UserModel.Exists;
            end;

            if IsUserModel then
                ControlType := USERCONTROL;


            ParamName := DSS.Parser.NextParam;
            Param := DSS.Parser.StrValue;
        end;

        RecalcElementData;
    end;

end;

{--------------------------------------------------------------------------}
function TCapControl.MakeLike(const CapControlName: String): Integer;
var
    OtherCapControl: TCapControlObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this CapControl name in the present collection}
    OtherCapControl := Find(CapControlName);
    if OtherCapControl <> NIL then
        with DSS.ActiveCapControlObj do
        begin

            NPhases := OtherCapControl.Fnphases;
            NConds := OtherCapControl.Fnconds; // Force Reallocation of terminal stuff

            ElementName := OtherCapControl.ElementName;
            ControlVars.CapacitorName := OtherCapControl.ControlVars.CapacitorName;
            ControlledElement := OtherCapControl.ControlledElement;  // Pointer to target circuit element
            MonitoredElement := OtherCapControl.MonitoredElement;  // Pointer to target circuit element

            ElementTerminal := OtherCapControl.ElementTerminal;
            with ControlVars do
            begin
                PTRatio := OtherCapControl.ControlVars.PTRatio;
                CTRatio := OtherCapControl.ControlVars.CTRatio;
                ControlType := OtherCapControl.ControlType;
                PresentState := OtherCapControl.ControlVars.PresentState;
                ShouldSwitch := OtherCapControl.ControlVars.ShouldSwitch;
                CondOffset := OtherCapControl.ControlVars.CondOffset;

                ON_Value := OtherCapControl.ControlVars.ON_Value;
                OFF_Value := OtherCapControl.ControlVars.OFF_Value;
                PFON_Value := OtherCapControl.ControlVars.PFON_Value;
                PFOFF_Value := OtherCapControl.ControlVars.PFOFF_Value;

                FCTPhase := OtherCapControl.ControlVars.FCTPhase;
                FPTPhase := OtherCapControl.ControlVars.FPTPhase;

                Voverride := OtherCapControl.ControlVars.Voverride;
                VoverrideBusSpecified := OtherCapControl.ControlVars.VoverrideBusSpecified;     // Added 8-11-11
                VOverrideBusName := OtherCapControl.ControlVars.VOverrideBusName;
            end;

            UserModel.Name := OtherCapControl.UserModel.Name;  // Connect to user written models
            IsUserModel := OtherCapControl.IsUserModel;

            FpctMinkvar := OtherCapControl.FpctMinkvar;

            ShowEventLog := OtherCapControl.ShowEventLog;


            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherCapControl.PropertyValue[i];

        end
    else
        DoSimpleMsg(DSS, 'Error in CapControl MakeLike: "' + CapControlName + '" Not Found.', 360);

end;


{==========================================================================}
{                    TCapControlObj                                        }
{==========================================================================}


{--------------------------------------------------------------------------}
constructor TCapControlObj.Create(ParClass: TDSSClass; const CapControlName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(CapControlName);
    DSSObjType := ParClass.DSSClassType;

    NPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors
                         // in base class
    with ControlVars do
    begin
        FCTPhase := 1;
        FPTPhase := 1;

        PTRatio := 60.0;
        CTRatio := 60.0;
        ControlType := CURRENTCONTROL;
        ONDelay := 15.0;
        OFFDelay := 15.0;
        DeadTime := 300.0;
        LastOpenTime := -DeadTime;

        ON_Value := 300.0;
        OFF_Value := 200.0;

        PFON_Value := 0.95;
        PFOFF_Value := 1.05;

        Voverride := FALSE;
        VoverrideEvent := FALSE;
        VoverrideBusSpecified := FALSE;
        VOverrideBusName := '';   // This is not in public data Struct at this time

        Vmax := 126;
        Vmin := 115;
        PresentState := CTRL_CLOSE;

        ShouldSwitch := FALSE;
        Armed := FALSE;
        PendingChange := CTRL_NONE;
    end;

    PublicDataStruct := @ControlVars;   // So User-written models can access
    PublicDataSize := Sizeof(TCapControlVars);

    ElementName := '';
    ControlledElement := NIL;
    ElementTerminal := 1;
    ControlVars.CapacitorName := '';
    MonitoredElement := NIL;

    FpctMinkvar := 50.0;

    IsUserModel := FALSE;
    UserModel := TCapUserControl.Create(DSS);   // Inits handles, FID


    ControlVars.ControlActionHandle := 0;

    cBuffer := NIL; // Complex buffer

    DSSObjType := ParClass.DSSClassType; //cap_CONTROL;

    InitPropertyValues(0);

   //  RecalcElementData;

end;


destructor TCapControlObj.Destroy;
begin
    ElementName := '';
    ControlVars.CapacitorName := '';
    if Assigned(cBuffer) then
        ReallocMem(cBuffer, 0);
    try
        UserModel.Free;
    finally
        UserModel := NIL; // do nothing
    end;
    inherited Destroy;
end;


{--------------------------------------------------------------------------}
procedure TCapControlObj.RecalcElementData;

var
    DevIndex: Integer;

begin

{Check for existence of capacitor}

// 5-21-01 RCD moved this section ahead of monitored element so Nphases gets defined first

    Devindex := GetCktElementIndex(DSS, ControlVars.CapacitorName); // Global function
    if DevIndex > 0 then
    begin  // Both capacitor and monitored element must already exist
        ControlledElement := DSS.ActiveCircuit.CktElements.Get(DevIndex);
        ControlledCapacitor := This_Capacitor;
        Nphases := ControlledElement.NPhases;  // Force number of phases to be same   Added 5/21/01  RCD
        Nconds := FNphases;
        ControlledElement.ActiveTerminalIdx := 1;  // Make the 1 st terminal active
                 // Get control synched up with capacitor
        with ControlledCapacitor do
            if ControlVars.AvailableSteps = Numsteps then
                ControlledElement.Closed[0] := FALSE
            else
                ControlledElement.Closed[0] := TRUE;
        if ControlledElement.Closed[0]      // Check state of phases of active terminal
        then
            ControlVars.PresentState := CTRL_CLOSE
        else
            ControlVars.PresentState := CTRL_OPEN;
    end
    else
    begin
        ControlledElement := NIL;   // element not found
        DoErrorMsg(DSS, 'CapControl: "' + Self.Name + '"', 'Capacitor Element "' + ControlVars.CapacitorName + '" Not Found.',
            ' Element must be defined previously.', 361);
    end;

    ControlVars.InitialState := ControlVars.PresentState;

{Check for existence of monitored element}

    Devindex := GetCktElementIndex(DSS, ElementName); // Global function
    if DevIndex > 0 then
    begin
        MonitoredElement := DSS.ActiveCircuit.CktElements.Get(DevIndex);
        if ElementTerminal > MonitoredElement.Nterms then
        begin
            DoErrorMsg(DSS, 'CapControl.' + Name + ':',
                'Terminal no. "' + '" does not exist.',
                'Re-specify terminal no.', 362);
        end
        else
        begin
               // Sets name of i-th terminal's connected bus in CapControl's buslist
            Setbus(1, MonitoredElement.GetBus(ElementTerminal));
               // Allocate a buffer bigenough to hold everything from the monitored element
            ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder);
            ControlVars.CondOffset := (ElementTerminal - 1) * MonitoredElement.NConds; // for speedy sampling
        end;
    end
    else
        DoSimpleMsg(DSS, 'Monitored Element in CapControl.' + Name + ' does not exist:"' + ElementName + '"', 363);

         {Alternative override bus}
    if ControlVars.VoverrideBusSpecified then
        with ControlVars do
        begin
            VOverrideBusIndex := DSS.ActiveCircuit.BusList.Find(VOverrideBusName);
            if VOverrideBusIndex = 0 then
            begin
                DoSimpleMsg(DSS, Format('CapControl.%s: Voltage override Bus "%s" not found. Did you wait until buses were defined? Reverting to default.', [Name, VOverrideBusName]), 10361);
                VoverrideBusSpecified := FALSE;
            end;

        end;

         // User model property update, if necessary
    if Usermodel.Exists then
        UserModel.UpdateModel;  // Checks for existence and Selects

end;

procedure TCapControlObj.MakePosSequence;
begin
    if ControlledElement <> NIL then
    begin
        Enabled := ControlledElement.Enabled;
        Nphases := ControlledElement.NPhases;
        Nconds := FNphases;
    end;
    if MonitoredElement <> NIL then
    begin
        Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    // Allocate a buffer bigenough to hold everything from the monitored element
        ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder);
        ControlVars.CondOffset := (ElementTerminal - 1) * MonitoredElement.NConds; // for speedy sampling
    end;
    inherited;
end;

{--------------------------------------------------------------------------}
procedure TCapControlObj.CalcYPrim;
begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
end;

{--------------------------------------------------------------------------}
procedure TCapControlObj.GetBusVoltages(pBus: TDSSBus; Buff: pComplexArray);
var
    j: Integer;
begin
    with pBus do
        if Assigned(Vbus) then    // uses nphases from CapControlObj
            for j := 1 to nPhases do
                cBuffer^[j] := DSS.ActiveCircuit.Solution.NodeV^[GetRef(j)];
    ;

end;

procedure TCapControlObj.GetControlCurrent(var ControlCurrent: Double);

// Get current to control on based on type of control specified.

var
    i: Integer;

begin

    with ControlVars do
        case FCTphase of
            AVGPHASES:
            begin
                ControlCurrent := 0.0;     // Get avg of all phases
                for i := (1 + CondOffset) to (Fnphases + CondOffset) do
                    ControlCurrent := ControlCurrent + Cabs(cBuffer^[i]);
                ControlCurrent := ControlCurrent / Fnphases / CTRatio;
            end;
            MAXPHASE:
            begin
                ControlCurrent := 0.0;     // Get max of all phases
                for i := (1 + CondOffset) to (Fnphases + CondOffset) do
                    ControlCurrent := max(ControlCurrent, Cabs(cBuffer^[i]));
                ControlCurrent := ControlCurrent / CTRatio;
            end;
            MINPHASE:
            begin
                ControlCurrent := 1.0e50;     // Get min of all phases
                for i := (1 + CondOffset) to (Fnphases + CondOffset) do
                    ControlCurrent := min(ControlCurrent, Cabs(cBuffer^[i]));
                ControlCurrent := ControlCurrent / CTRatio;
            end;
        else
    {Just use one phase because that's what most controls do.}
            ControlCurrent := Cabs(Cbuffer^[FCTphase]) / CTRatio;  // monitored phase only
        end;


end;

procedure TCapControlObj.GetCurrents(Curr: pComplexArray);
var
    i: Integer;
begin

    for i := 1 to Fnconds do
        Curr^[i] := CZERO;

end;

{--------------------------------------------------------------------------}
procedure TCapControlObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i: Integer;

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
    end;

end;


{--------------------------------------------------------------------------}
procedure TCapControlObj.DoPendingAction(const Code, ProxyHdl: Integer);

begin

    ControlledElement.ActiveTerminalIdx := 1;  // Set active terminal of capacitor to terminal 1

        {Allow user control to do something}
    case ControlType of
        USERCONTROL:
            if UserModel.Exists then
            begin
                UserModel.DoPending(Code, ProxyHdl);
                              // If control action changes last step in service, force update of Yprim and Fstates array
                ControlledCapacitor.LastStepInService := ControlVars.LastStepInService;
                              // Usermodel could override Pending change so the rest of this procedure is ignored.
            end;
    end;


    with ControlVars do
        case PendingChange of
            CTRL_OPEN:
                case ControlledCapacitor.NumSteps of
                    1:
                    begin
                        if PresentState = CTRL_CLOSE then
                        begin

                            ControlledElement.Closed[0] := FALSE;  // Open all phases of active terminal
                            ControlledCapacitor.SubtractStep;

                            if ShowEventLog then
                                AppendtoEventLog(DSS, 'Capacitor.' + ControlledElement.Name, '**Opened**');
                            PresentState := CTRL_OPEN;
                            with DSS.ActiveCircuit.Solution do
                                LastOpenTime := DynaVars.t + 3600.0 * DynaVars.intHour;
                        end;
                    end;
                else
                    if PresentState = CTRL_CLOSE then
                    begin      // Do this only if at least one step is closed
                        if not ControlledCapacitor.SubtractStep then
                        begin
                            PresentState := CTRL_OPEN;
                            ControlledElement.Closed[0] := FALSE;   // Open all phases of active terminal
                            if ShowEventLog then
                                AppendtoEventLog(DSS, 'Capacitor.' + ControlledElement.Name, '**Opened**');
                        end
                        else
                        if ShowEventLog then
                            AppendtoEventLog(DSS, 'Capacitor.' + ControlledElement.Name, '**Step Down**');
                    end;
                end;
            CTRL_CLOSE:
            begin
                if PresentState = CTRL_OPEN then
                begin
                    ControlledElement.Closed[0] := TRUE;    // Close all phases of active terminal
                    if ShowEventLog then
                        AppendtoEventLog(DSS, 'Capacitor.' + ControlledElement.Name, '**Closed**');
                    PresentState := CTRL_CLOSE;
                    ControlledCapacitor.AddStep;
                end
                else
                begin
                    if ControlledCapacitor.AddStep then
                        if ShowEventLog then
                            AppendtoEventLog(DSS, 'Capacitor.' + ControlledElement.Name, '**Step Up**');
                end;
            end;
        else
            {Do Nothing for NONE if the control has reset}
        end;

    with ControlVars do
    begin
        VoverrideEvent := FALSE;
        ShouldSwitch := FALSE;
        Armed := FALSE;   // reset control
    end;
end;

procedure TCapControlObj.GetControlVoltage(var ControlVoltage: Double);

// Get Voltage used for voltage control based on specified options

var
    i: Integer;

    function NextDeltaPhase(iphs: Integer): Integer;
    begin
        Result := iphs + 1;
        if Result > Fnphases then
            Result := 1;
    end;

begin
    with ControlVars do
        case FPTphase of
            AVGPHASES:
            begin
                ControlVoltage := 0.0;
                for i := 1 to MonitoredElement.NPhases do
                    ControlVoltage := ControlVoltage + Cabs(cBuffer^[i]);
                ControlVoltage := ControlVoltage / MonitoredElement.NPhases / PTRatio;
            end;
            MAXPHASE:
            begin
                ControlVoltage := 0.0;
                for i := 1 to MonitoredElement.NPhases do
                    ControlVoltage := Max(ControlVoltage, Cabs(cBuffer^[i]));
                ControlVoltage := ControlVoltage / PTRatio;
            end;
            MINPHASE:
            begin
                ControlVoltage := 1.0E50;
                for i := 1 to MonitoredElement.NPhases do
                    ControlVoltage := Min(ControlVoltage, Cabs(cBuffer^[i]));
                ControlVoltage := ControlVoltage / PTRatio;
            end;
        else
    {Just use one phase because that's what most controls do.}
    // Use L-L aB if capacitor is delta connected!!
            case TCapacitorObj(ControlledElement).Connection of
                1:
                    ControlVoltage := Cabs(Csub(cBuffer^[FPTPhase], cBuffer^[NextDeltaPhase(FPTPhase)])) / PTRatio;   // Delta
            else
                ControlVoltage := Cabs(cBuffer^[FPTPhase]) / PTRatio;     // Wye - Default
            end;
        end;
end;

{--------------------------------------------------------------------------}
procedure TCapControlObj.Sample;

var
    CurrTest,
    Vtest,
    NormalizedTime,
    Q: Double;
    S: Complex;
    PF: Double;
    Sabs: Double;


    function PF1to2(const Spower: Complex): Double;   // return PF in range of 1 to 2
    begin
        Sabs := Cabs(Spower);
        if Sabs <> 0.0 then
            Result := abs(Spower.re) / Sabs
        else
            Result := 1.0;  // default to unity
        if Spower.im < 0.0 then
            Result := 2.0 - Result;
    end;


begin

    ControlledElement.ActiveTerminalIdx := 1;
    if ControlledElement.Closed[0]      // Check state of phases of active terminal
    then
        ControlVars.PresentState := CTRL_CLOSE
    else
        ControlVars.PresentState := CTRL_OPEN;

    with   MonitoredElement, ControlVars do
    begin
        ShouldSwitch := FALSE;

         // First Check voltage override
        if Voverride then
            if ControlType <> VOLTAGECONTROL then
            begin  // Don't bother for voltage control

                if VoverrideBusSpecified then
                begin
                    GetBusVoltages(DSS.ActiveCircuit.Buses^[VOverrideBusIndex], cBuffer);
                end
                else
                    MonitoredElement.GetTermVoltages(ElementTerminal, cBuffer);

                GetControlVoltage(Vtest);

                case PresentState of
                    CTRL_OPEN:
                        if Vtest < VMin then
                        begin
                            PendingChange := CTRL_CLOSE;
                            ShouldSwitch := TRUE;
                            VoverrideEvent := TRUE;
                            if ShowEventLog then
                                AppendtoEventLog(DSS, 'Capacitor.' + ControlledElement.Name, Format('Low Voltage Override: %.8g V', [Vtest]));
                        end;
                    CTRL_CLOSE:
                        if Vtest > Vmax then
                        begin
                            PendingChange := CTRL_OPEN;
                            ShouldSwitch := TRUE;
                            VoverrideEvent := TRUE;
                            if ShowEventLog then
                                AppendtoEventLog(DSS, 'Capacitor.' + ControlledElement.Name, Format('High Voltage Override: %.8g V', [Vtest]));
                        end;
                end;


            end;


        if not ShouldSwitch then   // Else skip other control evaluations
            case ControlType of

                CURRENTCONTROL: {Current}
                begin

                     // Check largest Current of all phases of monitored element
                    MonitoredElement.GetCurrents(cBuffer);

                    GetControlCurrent(CurrTest);


                    case PresentState of
                        CTRL_OPEN:
                            if CurrTest > ON_Value then
                            begin
                                PendingChange := CTRL_CLOSE;
                                ShouldSwitch := TRUE;
                            end
                            else // Reset
                                PendingChange := CTRL_NONE;
                        CTRL_CLOSE:
                            if CurrTest < OFF_Value then
                            begin
                                PendingChange := CTRL_OPEN;
                                ShouldSwitch := TRUE;
                            end
                            else
                            if ControlledCapacitor.AvailableSteps > 0 then
                            begin
                                if CurrTest > ON_Value then
                                begin
                                    PendingChange := CTRL_CLOSE;
                                    ShouldSwitch := TRUE;
                                end;
                            end
                            else // Reset
                                PendingChange := CTRL_NONE;
                    end;

                end;

                VOLTAGECONTROL: {Voltage}
                begin
                    MonitoredElement.GetTermVoltages(ElementTerminal, cBuffer);

                    GetControlVoltage(Vtest);

                    case PresentState of
                        CTRL_OPEN:
                            if Vtest < ON_Value then
                            begin
                                PendingChange := CTRL_CLOSE;
                                ShouldSwitch := TRUE;
                            end
                            else // Reset
                                PendingChange := CTRL_NONE;
                        CTRL_CLOSE:
                        begin
                            PendingChange := CTRL_NONE;
                            if Vtest > OFF_Value then
                            begin
                                PendingChange := CTRL_OPEN;
                                ShouldSwitch := TRUE;
                            end
                            else
                            if ControlledCapacitor.AvailableSteps > 0 then
                            begin
                                if Vtest < ON_Value then
                                begin
                                    PendingChange := CTRL_CLOSE;
                                    ShouldSwitch := TRUE;
                                end
                            end;
                        end;
                    end;

                end;

                KVARCONTROL: {kvar}
                begin
                      //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
                    S := MonitoredElement.Power[ElementTerminal];
                    Q := S.im * 0.001;  // kvar

                    case PresentState of
                        CTRL_OPEN:
                            if Q > ON_Value then
                            begin
                                PendingChange := CTRL_CLOSE;
                                ShouldSwitch := TRUE;
                            end
                            else // Reset
                                PendingChange := CTRL_NONE;
                        CTRL_CLOSE:
                            if Q < OFF_Value then
                            begin
                                PendingChange := CTRL_OPEN;
                                ShouldSwitch := TRUE;
                            end
                            else
                            if ControlledCapacitor.AvailableSteps > 0 then
                            begin
                                if Q > ON_Value then
                                begin
                                    PendingChange := CTRL_CLOSE;  // We can go some more
                                    ShouldSwitch := TRUE;
                                end;
                            end
                            else // Reset
                                PendingChange := CTRL_NONE;
                    end;

                end;

              {User Control}
                USERCONTROL:
                    if UserModel.Exists then   // selects the model associated with this control
                    begin
                     // Load up test data into the public data record
                        SampleP := CmulReal(MonitoredElement.Power[ElementTerminal], 0.001);  // kW kvar

                        MonitoredElement.GetTermVoltages(ElementTerminal, cBuffer);
                        GetControlVoltage(SampleV);

                        MonitoredElement.GetCurrents(cBuffer);
                        GetControlCurrent(SampleCurr);

                        NumCapSteps := ControlledCapacitor.NumSteps;
                        AvailableSteps := ControlledCapacitor.AvailableSteps;
                        LastStepInService := ControlledCapacitor.LastStepInService;

                        UserModel.Sample;   // Sets the switching flags

                    end;


                TIMECONTROL: {time}
              {7-8-10  NormalizeToTOD Algorithm modified to close logic hole between 11 PM and midnight}
                begin
                    with DSS.ActiveCircuit.Solution do
                    begin
                        NormalizedTime := NormalizeToTOD(DynaVars.intHour, DynaVars.t);
                    end;
                    { 1/28/09 Code modified to accommodate OFF_Value < ON_Value }
                    case PresentState of
                        CTRL_OPEN:
                            if OFF_Value > ON_Value then
                            begin
                                if (NormalizedTime >= ON_Value) and (NormalizedTime < OFF_Value) then
                                begin
                                    PendingChange := CTRL_CLOSE;
                                    ShouldSwitch := TRUE;
                                end
                                else // Reset
                                    PendingChange := CTRL_NONE;
                            end
                            else
                            begin    // OFF time is next day
                                if (NormalizedTime >= ON_Value) and (NormalizedTime < 24.0) then
                                begin
                                    PendingChange := CTRL_CLOSE;
                                    ShouldSwitch := TRUE;
                                end
                                else // Reset
                                    PendingChange := CTRL_NONE;
                            end;

                        CTRL_CLOSE:
                            if OFF_Value > ON_Value then
                            begin
                                if (NormalizedTime >= OFF_Value) or (NormalizedTime < ON_Value) then
                                begin
                                    PendingChange := CTRL_OPEN;
                                    ShouldSwitch := TRUE;
                                end
                                else
                                if ControlledCapacitor.AvailableSteps > 0 then
                                begin
                                    if (NormalizedTime >= ON_Value) and (NormalizedTime < OFF_Value) then
                                    begin
                                        PendingChange := CTRL_CLOSE;  // We can go some more
                                        ShouldSwitch := TRUE;
                                    end;
                                end
                                else // Reset
                                    PendingChange := CTRL_NONE;
                            end
                            else
                            begin  // OFF time is next day
                                if (NormalizedTime >= OFF_Value) and (NormalizedTime < ON_Value) then
                                begin
                                    PendingChange := CTRL_OPEN;
                                    ShouldSwitch := TRUE;
                                end
                                else
                                if ControlledCapacitor.AvailableSteps > 0 then
                                begin
                                    if (NormalizedTime >= ON_Value) and (NormalizedTime < 24.0) then
                                    begin
                                        PendingChange := CTRL_CLOSE;  // We can go some more
                                        ShouldSwitch := TRUE;
                                    end;
                                end
                                else // Reset
                                    PendingChange := CTRL_NONE;
                            end;
                    end;
                end;

                PFCONTROL: {PF}
                begin
                      //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
                    S := MonitoredElement.Power[ElementTerminal];
                    PF := PF1to2(S);

                      {PF is in range of 0 .. 2;  Leading is 1..2}
                      {When turning on make sure there is at least half the kvar of the bank}

                    case PresentState of
                        CTRL_OPEN:
                            if (PF < PFON_Value) and (S.im * 0.001 > ControlledCapacitor.Totalkvar * FpctMinkvar * 0.01) // make sure we don't go too far leading
                            then
                            begin
                                PendingChange := CTRL_CLOSE;
                                ShouldSwitch := TRUE;
                            end
                            else // Reset
                                PendingChange := CTRL_NONE;
                        CTRL_CLOSE:
                            if PF > PFOFF_Value then
                            begin
                                PendingChange := CTRL_OPEN;
                                ShouldSwitch := TRUE;
                            end
                            else
                            if ControlledCapacitor.AvailableSteps > 0 then
                            begin
                                if (PF < PFON_Value) and (S.im * 0.001 > ControlledCapacitor.Totalkvar / ControlledCapacitor.Numsteps * 0.5) then
                                begin
                                    PendingChange := CTRL_CLOSE;  // We can go some more
                                    ShouldSwitch := TRUE;
                                end;
                            end
                            else // Reset
                                PendingChange := CTRL_NONE;
                    end;

                end;

            end;
    end;
    with DSS.ActiveCircuit, ControlVars do
    begin
        if ShouldSwitch and not Armed then
        begin
            if PendingChange = CTRL_CLOSE then
            begin
                if (Solution.DynaVars.t + Solution.DynaVars.intHour * 3600.0 - LastOpenTime) < DeadTime then // delay the close operation
                      {2-6-09 Added ONDelay to Deadtime so that all caps do not close back in at same time}
                    TimeDelay := Max(ONDelay, (Deadtime + ONDelay) - (Solution.DynaVars.t + Solution.DynaVars.intHour * 3600.0 - LastOpenTime))
                else
                    TimeDelay := ONDelay;
            end
            else
                TimeDelay := OFFDelay;
            ControlActionHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TimeDelay, PendingChange, 0, Self);
            Armed := TRUE;
            if ShowEventLog then
                AppendtoEventLog(DSS, 'Capacitor.' + ControlledElement.Name, Format('**Armed**, Delay= %.5g sec', [TimeDelay]));
        end;

        if Armed and (PendingChange = CTRL_NONE) then
        begin
            ControlQueue.Delete(ControlActionHandle);
            Armed := FALSE;
            if ShowEventLog then
                AppendtoEventLog(DSS, 'Capacitor.' + ControlledElement.Name, '**Reset**');
        end;
    end;  {With}
end;

function TCapControlObj.Get_Capacitor: TCapacitorObj;
begin

    Result := ControlledElement as TCapacitorObj;

end;


function TCapControlObj.Get_PendingChange: EControlAction;
begin
    Result := ControlVars.FPendingChange;
end;

function TCapControlObj.NormalizeToTOD(h: Integer; sec: Double): Double;
// Normalize time to a floating point number representing time of day if Hour > 24
// Resulting time should be 0:00+ to 24:00 inclusive.
var
    HourOfDay: Integer;

begin

    if h > 24 then
        HourOfDay := (h - ((h - 1) div 24) * 24)  // creates numbers 1..24
    else
        HourOfDay := h;

    Result := HourOfDay + sec / 3600.0;

   // If the TOD is at least slightly greater than 24:00 wrap around to 0:00
    if Result - 24.0 > Epsilon then
        Result := Result - 24.0;   // Wrap around

end;


procedure TCapControlObj.Reset;
begin
    PendingChange := CTRL_NONE;
    ControlledElement.ActiveTerminalIdx := 1;
    with ControlVars do
    begin
        case InitialState of
            CTRL_OPEN:
                ControlledElement.Closed[0] := FALSE;   // Open all phases of active terminal
            CTRL_CLOSE:
                ControlledElement.Closed[0] := TRUE;    // Close all phases of active terminal
        end;
        ShouldSwitch := FALSE;
        LastOpenTime := -DeadTime;
        PresentState := InitialState;
    end;
end;

procedure TCapControlObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := '';   //'element';
    PropertyValue[2] := '1';   //'terminal';
    PropertyValue[3] := '';
    PropertyValue[4] := 'current';
    PropertyValue[5] := '60';
    PropertyValue[6] := '60';
    PropertyValue[7] := '300';
    PropertyValue[8] := '200';
    PropertyValue[9] := '15';
    PropertyValue[10] := 'NO';
    PropertyValue[11] := '126';
    PropertyValue[12] := '115';
    PropertyValue[13] := '15';
    PropertyValue[14] := '300';
    PropertyValue[15] := '1';
    PropertyValue[16] := '1';
    PropertyValue[17] := '';
    PropertyValue[18] := 'YES';
    PropertyValue[19] := '';
    PropertyValue[20] := '';
    PropertyValue[21] := '50';

    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TCapControlObj.Set_PendingChange(const Value: EControlAction);
begin
    ControlVars.FPendingChange := Value;
    DblTraceParameter := Integer(Value);
end;


initialization

end.
